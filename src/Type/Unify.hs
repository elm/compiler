{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.Unify (unify) where

import Control.Monad (liftM2, when, zipWithM_)
import Control.Monad.Except (ExceptT, lift, liftIO, throwError, runExceptT)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import AST.Type (Super(..))
import qualified AST.Variable as Var
import qualified Reporting.Region as R
import qualified Reporting.Error.Type as Error
import qualified Type.Occurs as Occurs
import qualified Type.State as TS
import Type.Type as Type
import qualified Type.UnionFind as UF



-- KICK OFF UNIFICATION


unify :: Error.Hint -> R.Region -> Variable -> Variable -> TS.Solver ()
unify hint region expected actual =
  {-# SCC elm_compiler_type_unify #-}
  do  result <- runExceptT (guardedUnify ExpectedActual expected actual)
      case result of
        Right () ->
            return ()

        Left problem ->
          do  expectedSrcType <- liftIO $ Type.toSrcType expected
              actualSrcType   <- liftIO $ Type.toSrcType actual
              liftIO $ mergeHelp expected actual (Error "?") noRank
              TS.addError region $
                case problem of
                  Typical ->
                    Error.Mismatch $
                      Error.MismatchInfo hint expectedSrcType actualSrcType Nothing

                  Special reason ->
                    Error.Mismatch $
                      Error.MismatchInfo hint expectedSrcType actualSrcType (Just reason)

                  Infinite ->
                    Error.InfiniteType (Left hint) expectedSrcType



-- UNIFICATION HELPERS


type Unify =
  ExceptT Problem TS.Solver


data Context =
  Context
    { _orientation :: Orientation
    , _first :: Variable
    , _firstDesc :: Descriptor
    , _second :: Variable
    , _secondDesc :: Descriptor
    }


data Orientation = ExpectedActual | ActualExpected


reorient :: Context -> Context
reorient (Context orientation var1 desc1 var2 desc2) =
  let
    otherOrientation =
      case orientation of
        ExpectedActual -> ActualExpected
        ActualExpected -> ExpectedActual
  in
    Context otherOrientation var2 desc2 var1 desc1



-- ERROR MESSAGES


data Problem
  = Typical
  | Special Error.Reason
  | Infinite


mismatch :: Context -> Maybe Error.Reason -> Unify a
mismatch context@(Context orientation var1 _ var2 _) maybeReason =
  do  anyInfinite <- liftIO $ liftM2 (||) (Occurs.occurs var1) (Occurs.occurs var2)
      when anyInfinite $ throwError Infinite

      args1 <- liftIO $ collectArgs var1
      args2 <- liftIO $ collectArgs var2

      let numArgs1 = length args1
      let numArgs2 = length args2

      if numArgs1 == numArgs2
        then
          throwError $ reasonToProblem orientation maybeReason
        else
          do  lift $ zipWithM_ (\a1 a2 -> runExceptT $ subUnify context a1 a2) args1 args2
              throwError $ reasonToProblem orientation $ Just $
                Error.MissingArgs $ abs (numArgs2 - numArgs1)


reasonToProblem :: Orientation -> Maybe Error.Reason -> Problem
reasonToProblem orientation maybeReason =
  case maybeReason of
    Nothing ->
      Typical

    Just reason ->
      case orientation of
        ExpectedActual ->
          Special reason

        ActualExpected ->
          Special (Error.flipReason reason)


badRigid :: Maybe Text -> Error.Reason
badRigid maybeName =
  Error.BadVar (Just (Error.Rigid maybeName)) Nothing


badSuper :: Super -> Error.Reason
badSuper super =
  Error.BadVar (Just (errorSuper super)) Nothing


badSupers :: Super -> Super -> Error.Reason
badSupers super1 super2 =
  Error.BadVar (Just (errorSuper super1)) (Just (errorSuper super2))


doubleBad :: Error.VarType -> Error.VarType -> Error.Reason
doubleBad vt1 vt2 =
  Error.BadVar (Just vt1) (Just vt2)


errorSuper :: Super -> Error.VarType
errorSuper super =
  case super of
    Number ->
        Error.Number

    Comparable ->
        Error.Comparable

    Appendable ->
        Error.Appendable

    CompAppend ->
        Error.CompAppend



-- MERGE


merge :: Context -> Content -> Unify ()
merge (Context _ var1 (Descriptor _ rank1 _ _) var2 (Descriptor _ rank2 _ _)) content =
  liftIO $ mergeHelp var1 var2 content (min rank1 rank2)


mergeHelp :: Variable -> Variable -> Content -> Int -> IO ()
mergeHelp var1 var2 newContent newRank =
  UF.union var1 var2 $
    Descriptor newContent newRank noMark Nothing


fresh :: Context -> Content -> Unify Variable
fresh (Context _ _ (Descriptor _ rank1 _ _) _ (Descriptor _ rank2 _ _)) content =
  do  freshVariable <-
          liftIO $ UF.fresh $
            Descriptor content (min rank1 rank2) noMark Nothing

      lift $ TS.register freshVariable

      return freshVariable



-- ACTUALLY UNIFY THINGS


guardedUnify :: Orientation -> Variable -> Variable -> Unify ()
guardedUnify orientation left right =
  do  equivalent <- liftIO $ UF.equivalent left right
      if equivalent
        then return ()
        else
          do  leftDesc <- liftIO $ UF.descriptor left
              rightDesc <- liftIO $ UF.descriptor right
              actuallyUnify (Context orientation left leftDesc right rightDesc)


subUnify :: Context -> Variable -> Variable -> Unify ()
subUnify context var1 var2 =
  guardedUnify (_orientation context) var1 var2


actuallyUnify :: Context -> Unify ()
actuallyUnify context@(Context _ _ (Descriptor firstContent _ _ _) _ (Descriptor secondContent _ _ _)) =
  case firstContent of
    Error _ ->
        -- If there was an error, just pretend it is okay. This lets us avoid
        -- "cascading" errors where one problem manifests as multiple message.
        return ()

    Var Flex Nothing _ ->
        unifyFlex context secondContent

    Var Flex (Just super) _ ->
        unifyFlexSuper context super firstContent secondContent

    Var Rigid maybeSuper maybeName ->
        unifyRigid context maybeSuper maybeName firstContent secondContent

    Alias name args realVar ->
        unifyAlias context name args realVar secondContent

    Structure term ->
        unifyStructure context term firstContent secondContent



-- UNIFY FLEXIBLE VARIABLES


unifyFlex :: Context -> Content -> Unify ()
unifyFlex context otherContent =
  case otherContent of
    Error _ ->
        return ()

    Var Flex _ _ ->
        merge context otherContent

    Var Rigid _ _ ->
        merge context otherContent

    Alias _ _ _ ->
        merge context otherContent

    Structure _ ->
        merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid :: Context -> Maybe Super -> Maybe Text -> Content -> Content -> Unify ()
unifyRigid context maybeSuper maybeName content otherContent =
  case otherContent of
    Error _ ->
        return ()

    Var Flex otherMaybeSuper _ ->
        case (maybeSuper, otherMaybeSuper) of
          (_, Nothing) ->
              merge context content

          (Nothing, Just _) ->
              mismatch context (Just (badRigid maybeName))

          (Just super, Just otherSuper) ->
              if combineRigidSupers super otherSuper then
                  merge context content
              else
                  mismatch context (Just (badRigid maybeName))

    Var Rigid _ otherMaybeName ->
        mismatch context $ Just $
          doubleBad (Error.Rigid maybeName) (Error.Rigid otherMaybeName)

    Alias _ _ _ ->
        mismatch context (Just (badRigid maybeName))

    Structure _ ->
        mismatch context (Just (badRigid maybeName))



-- UNIFY SUPER VARIABLES


unifyFlexSuper :: Context -> Super -> Content -> Content -> Unify ()
unifyFlexSuper context super content otherContent =
  case otherContent of
    Structure term ->
        unifyFlexSuperStructure context super term

    Var Rigid Nothing maybeName ->
        mismatch context (Just (doubleBad (errorSuper super) (Error.Rigid maybeName)))

    Var Rigid (Just otherSuper) maybeName ->
        if combineRigidSupers otherSuper super then
            merge context otherContent
        else
            mismatch context $ Just $ badRigid maybeName

    Var Flex Nothing _ ->
        merge context content

    Var Flex (Just otherSuper) _ ->
      case super of
        Number ->
          case otherSuper of
            Number     -> merge context content
            Comparable -> merge context content
            _          -> mismatch context $ Just $ badSupers super otherSuper

        Comparable ->
          case otherSuper of
            Comparable -> merge context otherContent
            Number     -> merge context otherContent
            Appendable -> merge context (Var Flex (Just CompAppend) Nothing)
            CompAppend -> merge context otherContent

        Appendable ->
          case otherSuper of
            Appendable -> merge context otherContent
            Comparable -> merge context (Var Flex (Just CompAppend) Nothing)
            CompAppend -> merge context otherContent
            Number     -> mismatch context $ Just $ badSupers super otherSuper

        CompAppend ->
          case otherSuper of
            Comparable -> merge context content
            Appendable -> merge context content
            CompAppend -> merge context content
            Number     -> mismatch context $ Just $ badSupers super otherSuper

    Alias _ _ realVar ->
        subUnify context (_first context) realVar

    Error _ ->
        return ()


combineRigidSupers :: Super -> Super -> Bool
combineRigidSupers rigid flex =
  rigid == flex
  || (rigid == Number && flex == Comparable)
  || (rigid == CompAppend && (flex == Comparable || flex == Appendable))


atomMatchesSuper :: Super -> Var.Canonical -> Bool
atomMatchesSuper super name =
  case super of
    Number ->
        elem name [ Var.int, Var.float ]

    Comparable ->
        elem name [ Var.string, Var.int, Var.float, Var.char]

    Appendable ->
        name == Var.string

    CompAppend ->
        name == Var.string


unifyFlexSuperStructure :: Context -> Super -> FlatType -> Unify ()
unifyFlexSuperStructure context super term =
  do  case term of
        App1 name [] ->
          if atomMatchesSuper super name then
            merge context (Structure term)
          else
            mismatch context (Just (badSuper super))

        App1 name [variable] | name == Var.list ->
          case super of
            Number ->
                mismatch context (Just (badSuper super))

            Appendable ->
                merge context (Structure term)

            Comparable ->
                do  comparableOccursCheck context
                    merge context (Structure term)
                    unifyComparableRecursive (_orientation context) variable

            CompAppend ->
                do  comparableOccursCheck context
                    merge context (Structure term)
                    unifyComparableRecursive (_orientation context) variable

        App1 name args | Var.isTuple name ->
          case super of
            Number ->
                mismatch context (Just (badSuper super))

            Appendable ->
                mismatch context (Just (badSuper super))

            Comparable ->
                if length args > 6 then
                    mismatch context (Just (Error.TooLongComparableTuple (length args)))

                else
                    do  comparableOccursCheck context
                        merge context (Structure term)
                        mapM_ (unifyComparableRecursive (_orientation context)) args

            CompAppend ->
                mismatch context (Just (badSuper super))

        _ ->
          mismatch context (Just (badSuper super))


-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?
comparableOccursCheck :: Context -> Unify ()
comparableOccursCheck (Context _ _ _ var _) =
  do  hasOccurred <- liftIO $ Occurs.occurs var
      if hasOccurred
        then throwError Infinite
        else return ()


unifyComparableRecursive :: Orientation -> Variable -> Unify ()
unifyComparableRecursive orientation var =
  do  compVar <- liftIO $
        do  (Descriptor _ rank _ _) <- UF.descriptor var
            UF.fresh $ Descriptor (Var Flex (Just Comparable) Nothing) rank noMark Nothing

      lift $ TS.register compVar

      guardedUnify orientation compVar var


collectArgs :: Variable -> IO [Variable]
collectArgs variable =
  collectArgsHelp [] variable


collectArgsHelp :: [Variable] -> Variable -> IO [Variable]
collectArgsHelp revArgs variable =
  do  (Descriptor content _ _ _) <- UF.descriptor variable
      case content of
        Structure (Fun1 arg returnType) ->
          collectArgsHelp (arg : revArgs) returnType

        _ ->
          return $ variable : revArgs



-- UNIFY ALIASES


unifyAlias :: Context -> Var.Canonical -> [(Text, Variable)] -> Variable -> Content -> Unify ()
unifyAlias context name args realVar otherContent =
  case otherContent of
    Error _ ->
        return ()

    Var Flex Nothing _ ->
        merge context (Alias name args realVar)

    Var _ _ _ ->
        subUnify context realVar (_second context)

    Alias otherName otherArgs otherRealVar ->
        if name == otherName then
            do  zipWithM_ (subUnify context) (map snd args) (map snd otherArgs)
                merge context otherContent

        else
            subUnify context realVar otherRealVar

    Structure _ ->
        subUnify context realVar (_second context)



-- UNIFY STRUCTURES


unifyStructure :: Context -> FlatType -> Content -> Content -> Unify ()
unifyStructure context term content otherContent =
  case otherContent of
    Error _ ->
        return ()

    Var Flex Nothing _ ->
        merge context content

    Var Flex (Just super) _ ->
        unifyFlexSuperStructure (reorient context) super term

    Var Rigid _ maybeName ->
        mismatch context (Just (Error.flipReason (badRigid maybeName)))

    Alias _ _ realVar ->
        subUnify context (_first context) realVar

    Structure otherTerm ->
        case (term, otherTerm) of
          (App1 name args, App1 otherName otherArgs) ->
              if name == otherName then
                do  zipWithM_ (subUnify context) args otherArgs
                    merge context otherContent

              else if isIntFloat name otherName || isIntFloat otherName name then
                mismatch context (Just Error.IntFloat)

              else
                mismatch context Nothing

          (Fun1 arg result, Fun1 otherArg otherResult) ->
              do  subUnify context arg otherArg
                  subUnify context result otherResult
                  merge context otherContent

          (EmptyRecord1, EmptyRecord1) ->
              merge context otherContent

          (Record1 fields ext, EmptyRecord1) | Map.null fields ->
              subUnify context ext (_second context)

          (EmptyRecord1, Record1 fields ext) | Map.null fields ->
              subUnify context (_first context) ext

          (Record1 fields extension, Record1 otherFields otherExtension) ->
              do  firstStructure <- liftIO $ gatherFields context fields extension
                  secondStructure <- liftIO $ gatherFields context otherFields otherExtension
                  unifyRecord context firstStructure secondStructure

          _ ->
              mismatch context Nothing


isIntFloat :: Var.Canonical -> Var.Canonical -> Bool
isIntFloat name otherName =
  Var.isPrim "Int" name && Var.isPrim "Float" otherName



-- UNIFY RECORDS


unifyRecord :: Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord context firstStructure secondStructure =
  do  let (RecordStructure expFields expVar expStruct) = firstStructure
      let (RecordStructure actFields actVar actStruct) = secondStructure

      let sharedFields = Map.intersectionWith (,) expFields actFields

      -- call after unifying extension, make sure record shape matches before
      -- looking into whether the particular field types match.
      let unifySharedFields otherFields ext =
            do  unifySharedFieldsHelp context sharedFields
                let allFields = Map.union (Map.map fst sharedFields) otherFields
                merge context (Structure (Record1 allFields ext))

      let uniqueExpFields = Map.difference expFields actFields
      let uniqueActFields = Map.difference actFields expFields

      case (expStruct, Map.null uniqueExpFields, actStruct, Map.null uniqueActFields) of
        (_, True, _, True) ->
            do  subUnify context expVar actVar
                unifySharedFields Map.empty expVar

        (Empty, _, _, False) ->
            mismatch context $ Just $
              Error.MessyFields (Map.keys sharedFields) (Map.keys uniqueExpFields) (Map.keys uniqueActFields)

        (_, False, Empty, _) ->
            mismatch context $ Just $
              Error.MessyFields (Map.keys sharedFields) (Map.keys uniqueExpFields) (Map.keys uniqueActFields)

        (_, False, _, True) ->
            do  subRecord <- fresh context (Structure (Record1 uniqueExpFields expVar))
                subUnify context subRecord actVar
                unifySharedFields Map.empty subRecord

        (_, True, _, False) ->
            do  subRecord <- fresh context (Structure (Record1 uniqueActFields actVar))
                subUnify context expVar subRecord
                unifySharedFields Map.empty subRecord

        (Extension, False, Extension, False) ->
            do  let subFields = Map.union uniqueExpFields uniqueActFields
                subExt <- fresh context (Var Flex Nothing Nothing)
                expRecord <- fresh context (Structure (Record1 uniqueActFields subExt))
                actRecord <- fresh context (Structure (Record1 uniqueExpFields subExt))
                subUnify context expVar expRecord
                subUnify context actRecord actVar
                unifySharedFields subFields subExt



unifySharedFieldsHelp :: Context -> Map.Map Text (Variable, Variable) -> Unify ()
unifySharedFieldsHelp context sharedFields =
  do  maybeBadFields <- traverse (unifyField context) (Map.toList sharedFields)
      case Maybe.catMaybes maybeBadFields of
        [] ->
          return ()

        badFields ->
          mismatch context (Just (Error.BadFields (reverse badFields)))


unifyField :: Context -> (Text, (Variable, Variable)) -> Unify (Maybe (Text, Maybe Error.Reason))
unifyField context (field, (expected, actual)) =
  do  result <- lift $ runExceptT $ subUnify context expected actual
      case result of
        Right () ->
          return $ Nothing

        Left problem ->
          return $ Just $ (,) field $
            case problem of
              Typical ->
                Nothing

              Special reason ->
                Just reason

              Infinite ->
                Nothing



-- GATHER RECORD STRUCTURE


data RecordStructure = RecordStructure
    { _fields :: Map.Map Text Variable
    , _extVar :: Variable
    , _extStruct :: ExtensionStructure
    }


data ExtensionStructure
    = Empty
    | Extension


gatherFields :: Context -> Map.Map Text Variable -> Variable -> IO RecordStructure
gatherFields context fields variable =
  do  (Descriptor content _ _ _) <- UF.descriptor variable
      case content of
        Structure (Record1 subFields subExt) ->
            gatherFields context (Map.union fields subFields) subExt

        Structure EmptyRecord1 ->
            return (RecordStructure fields variable Empty)

        Alias _ _ var ->
            -- TODO may be dropping useful alias info here
            gatherFields context fields var

        _ ->
            return (RecordStructure fields variable Extension)

