{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Type.Unify (unify) where

import Control.Monad (foldM, liftM2, zipWithM_)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Strict as Map

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Reporting.Region as R
import qualified Reporting.Error.Type as Error
import qualified Type.Occurs as Occurs
import qualified Type.State as TS
import Type.Type as Type
import qualified Type.UnionFind as UF



-- UNIFY


unify :: R.Region -> Variable -> Variable -> TS.Solver ()
unify region actual expected =
  do  answer <- run (guardedUnify ActualExpected actual expected)
      case answer of
        Ok ->
          return ()

        Err problem ->
          do  actualSrcType <- liftIO $ Type.toSrcType actual
              expectedSrcType <- liftIO $ Type.toSrcType expected
              liftIO $ mergeHelp actual expected (Error "?") noRank
              TS.addError region $
                case problem of
                  Reason hasDecoder maybeReason ->
                    Error.MismatchExpr $
                      Error.MismatchInfo actualSrcType expectedSrcType hasDecoder maybeReason

                  Infinite ->
                    Error.InfiniteType actualSrcType


data Answer = Ok | Err Problem


run :: Unify () -> TS.Solver Answer
run (Unify k) =
  do  (vars, answer) <-
        liftIO $ k []
          (\vars () -> return (vars, Ok))
          (\vars p  -> return (vars, Err p))
      TS.introduce vars
      return answer



-- CPS UNIFIER


newtype Unify a =
  Unify (forall r.
    [Variable]
    -> ([Variable] -> a -> IO r)
    -> ([Variable] -> Problem -> IO r)
    -> IO r
  )


instance Functor Unify where
  fmap func (Unify kv) =
    Unify $ \vars ok err ->
      let
        ok1 vars1 value =
          ok vars1 (func value)
      in
      kv vars ok1 err


instance Applicative Unify where
  pure a =
    Unify $ \vars ok _ ->
      ok vars a

  (<*>) (Unify kf) (Unify kv) =
    Unify $ \vars ok err ->
      let
        ok1 vars1 func =
          let
            ok2 vars2 value =
              ok vars2 (func value)
          in
          kv vars1 ok2 err
      in
      kf vars ok1 err


instance Monad Unify where
  return a =
    Unify $ \vars ok _ ->
      ok vars a

  (>>=) (Unify ka) callback =
    Unify $ \vars ok err ->
      let
        ok1 vars1 a =
          case callback a of
            Unify kb -> kb vars1 ok err
      in
      ka vars ok1 err


register :: IO Variable -> Unify Variable
register mkVar =
  Unify $ \vars ok _ ->
    do  var <- mkVar
        ok (var:vars) var



-- UNIFICATION HELPERS


data Context =
  Context
    { _orientation :: Orientation
    , _first :: Variable
    , _firstDesc :: Descriptor
    , _second :: Variable
    , _secondDesc :: Descriptor
    }


data Orientation
  = ExpectedActual
  | ActualExpected


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
  = Reason Bool (Maybe Error.Reason)
  | Infinite


mismatch :: Context -> Maybe Error.Reason -> Unify a
mismatch context@(Context orientation var1 _ var2 _) maybeReason =
  Unify $ \vars _ err ->
    do  anyInfinite <- liftM2 (||) (Occurs.occurs var1) (Occurs.occurs var2)
        if anyInfinite
          then err vars Infinite
          else
            do  args1 <- collectArgs [] var1
                args2 <- collectArgs [] var2

                hasDecoder <-
                  or <$> traverse detectDecoder (args1 ++ args2)

                if length args1 /= length args2
                  then
                    do  newVars <- foldM (subUnifyAnyway context) vars (zip args1 args2)
                        err newVars $ Reason hasDecoder $ Just $
                          Error.MissingArgs (abs (length args2 - length args1))
                  else
                    err vars $ Reason hasDecoder $
                      case orientation of
                        ExpectedActual ->
                          maybeReason

                        ActualExpected ->
                          fmap Error.flipReason maybeReason


subUnifyAnyway :: Context -> [Variable] -> (Variable, Variable) -> IO [Variable]
subUnifyAnyway context vars (arg1, arg2) =
  case subUnify context arg1 arg2 of
    Unify k ->
      k vars (\vs _ -> return vs) (\vs _ -> return vs)


collectArgs :: [Variable] -> Variable -> IO [Variable]
collectArgs revArgs variable =
  do  (Descriptor content _ _ _) <- UF.descriptor variable
      case content of
        Structure (Fun1 arg returnType) ->
          collectArgs (arg : revArgs) returnType

        _ ->
          return $ variable : revArgs


detectDecoder :: Variable -> IO Bool
detectDecoder variable =
  do  (Descriptor content _ _ _) <- UF.descriptor variable
      case content of
        Structure (App1 home name [_]) ->
          return (home == ModuleName.jsonDecode && name == "Decoder")

        _ ->
          return False



-- MERGE


merge :: Context -> Content -> Unify ()
merge (Context _ var1 (Descriptor _ rank1 _ _) var2 (Descriptor _ rank2 _ _)) content =
  Unify $ \vars ok _ ->
    ok vars =<< mergeHelp var1 var2 content (min rank1 rank2)


mergeHelp :: Variable -> Variable -> Content -> Int -> IO ()
mergeHelp var1 var2 newContent newRank =
  UF.union var1 var2 $
    Descriptor newContent newRank noMark Nothing


fresh :: Context -> Content -> Unify Variable
fresh (Context _ _ (Descriptor _ rank1 _ _) _ (Descriptor _ rank2 _ _)) content =
  register $ UF.fresh $
    Descriptor content (min rank1 rank2) noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify :: Orientation -> Variable -> Variable -> Unify ()
guardedUnify orientation left right =
  Unify $ \vars ok err ->
    do  equivalent <- UF.equivalent left right
        if equivalent
          then ok vars ()
          else
            do  leftDesc <- UF.descriptor left
                rightDesc <- UF.descriptor right
                case actuallyUnify (Context orientation left leftDesc right rightDesc) of
                  Unify k ->
                    k vars ok err


subUnify :: Context -> Variable -> Variable -> Unify ()
subUnify context var1 var2 =
  guardedUnify (_orientation context) var1 var2


actuallyUnify :: Context -> Unify ()
actuallyUnify context@(Context _ _ (Descriptor firstContent _ _ _) _ (Descriptor secondContent _ _ _)) =
  case firstContent of
    FlexVar _ ->
        unifyFlex context secondContent

    FlexSuper super _ ->
        unifyFlexSuper context super firstContent secondContent

    RigidVar name ->
        unifyRigid context Nothing name firstContent secondContent

    RigidSuper super name ->
        unifyRigid context (Just super) name firstContent secondContent

    Alias home name args realVar ->
        unifyAlias context home name args realVar secondContent

    Structure flatType ->
        unifyStructure context flatType firstContent secondContent

    Error _ ->
        -- If there was an error, just pretend it is okay. This lets us avoid
        -- "cascading" errors where one problem manifests as multiple message.
        return ()



-- UNIFY FLEXIBLE VARIABLES


unifyFlex :: Context -> Content -> Unify ()
unifyFlex context otherContent =
  case otherContent of
    Error _ ->
        return ()

    -- TODO see if wildcarding makes a noticable perf difference

    FlexVar _ ->
        merge context otherContent

    FlexSuper _ _ ->
        merge context otherContent

    RigidVar _ ->
        merge context otherContent

    RigidSuper _ _ ->
        merge context otherContent

    Alias _ _ _ _ ->
        merge context otherContent

    Structure _ ->
        merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid :: Context -> Maybe SuperType -> N.Name -> Content -> Content -> Unify ()
unifyRigid context maybeSuper name content otherContent =
  case otherContent of
    FlexVar _ ->
        merge context content

    FlexSuper otherSuper _ ->
        case maybeSuper of
          Just super ->
            if combineRigidSupers super otherSuper then
              merge context content
            else
              mismatch context $ Just $
                Error.RigidSuperTooGeneric super name $ Error.SpecificSuper otherSuper

          Nothing ->
            mismatch context $ Just $
              Error.RigidVarTooGeneric name $ Error.SpecificSuper otherSuper

    RigidVar otherName ->
        mismatch context $ Just $
          Error.RigidClash name otherName

    RigidSuper _ otherName ->
        mismatch context $ Just $
          Error.RigidClash name otherName

    Alias _ otherName _ _ ->
        mismatch context $ Just $
          maybe Error.RigidVarTooGeneric Error.RigidSuperTooGeneric maybeSuper name $
            Error.SpecificType otherName

    Structure flatType ->
        mismatch context $ Just $
          maybe Error.RigidVarTooGeneric Error.RigidSuperTooGeneric maybeSuper name $
            flatTypeToSpecificThing flatType

    Error _ ->
        return ()


flatTypeToSpecificThing :: FlatType -> Error.SpecificThing
flatTypeToSpecificThing flatType =
  case flatType of
    App1 _ name _ ->
      Error.SpecificType name

    Fun1 _ _ ->
      Error.SpecificFunction

    EmptyRecord1 ->
      Error.SpecificRecord

    Record1 _ _ ->
      Error.SpecificRecord

    Unit1 ->
      Error.SpecificUnit

    Tuple1 _ _ _ ->
      Error.SpecificTuple



-- UNIFY SUPER VARIABLES


unifyFlexSuper :: Context -> SuperType -> Content -> Content -> Unify ()
unifyFlexSuper context super content otherContent =
  case otherContent of
    Structure flatType ->
        unifyFlexSuperStructure context super flatType

    RigidVar name ->
        mismatch context $ Just $
          Error.RigidVarTooGeneric name (Error.SpecificSuper super)

    RigidSuper otherSuper name ->
        if combineRigidSupers otherSuper super then
            merge context otherContent
        else
            mismatch context $ Just $
              Error.RigidSuperTooGeneric otherSuper name (Error.SpecificSuper super)

    FlexVar _ ->
        merge context content

    FlexSuper otherSuper _ ->
      case super of
        Number ->
          case otherSuper of
            Number     -> merge context content
            Comparable -> merge context content
            _          -> mismatch context Nothing -- Error.NumberAppendableClash

        Comparable ->
          case otherSuper of
            Comparable -> merge context otherContent
            Number     -> merge context otherContent
            Appendable -> merge context (Type.unnamedFlexSuper CompAppend)
            CompAppend -> merge context otherContent

        Appendable ->
          case otherSuper of
            Appendable -> merge context otherContent
            Comparable -> merge context (Type.unnamedFlexSuper CompAppend)
            CompAppend -> merge context otherContent
            Number     -> mismatch context Nothing -- Error.NumberAppendableClash

        CompAppend ->
          case otherSuper of
            Comparable -> merge context content
            Appendable -> merge context content
            CompAppend -> merge context content
            Number     -> mismatch context Nothing -- Error.NumberAppendableClash

    Alias _ _ _ realVar ->
        subUnify context (_first context) realVar

    Error _ ->
        return ()


combineRigidSupers :: SuperType -> SuperType -> Bool
combineRigidSupers rigid flex =
  rigid == flex
  || (rigid == Number && flex == Comparable)
  || (rigid == CompAppend && (flex == Comparable || flex == Appendable))


atomMatchesSuper :: SuperType -> ModuleName.Canonical -> N.Name -> Bool
atomMatchesSuper super home name =
  home == ModuleName.basics
  &&
  case super of
    Number ->
      name == N.int
      || name == N.float

    Comparable ->
      name == N.int
      || name == N.float
      || name == N.string
      || name == N.char

    Appendable ->
      name == N.string

    CompAppend ->
      name == N.string


unifyFlexSuperStructure :: Context -> SuperType -> FlatType -> Unify ()
unifyFlexSuperStructure context super flatType =
  case flatType of
    App1 home name [] ->
      if atomMatchesSuper super home name then
        merge context (Structure flatType)
      else
        mismatch context $ Just $ Error.NotPartOfSuper super

    App1 home name [variable] | home == ModuleName.list && name == N.list ->
      case super of
        Number ->
            mismatch context $ Just $ Error.NotPartOfSuper super

        Appendable ->
            merge context (Structure flatType)

        Comparable ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                unifyComparableRecursive (_orientation context) variable

        CompAppend ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                unifyComparableRecursive (_orientation context) variable

    Tuple1 a b maybeC ->
      case super of
        Number ->
            mismatch context $ Just $ Error.NotPartOfSuper super

        Appendable ->
            mismatch context $ Just $ Error.NotPartOfSuper super

        Comparable ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                let orientation = _orientation context
                unifyComparableRecursive orientation a
                unifyComparableRecursive orientation b
                case maybeC of
                  Nothing ->
                    return ()

                  Just c ->
                    unifyComparableRecursive orientation c

        CompAppend ->
            mismatch context $ Just $ Error.NotPartOfSuper super

    _ ->
      mismatch context $ Just $ Error.NotPartOfSuper super


-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?
comparableOccursCheck :: Context -> Unify ()
comparableOccursCheck (Context _ _ _ var _) =
  Unify $ \vars ok err ->
    do  hasOccurred <- Occurs.occurs var
        if hasOccurred
          then err vars Infinite
          else ok vars ()


unifyComparableRecursive :: Orientation -> Variable -> Unify ()
unifyComparableRecursive orientation var =
  do  compVar <- register $
        do  (Descriptor _ rank _ _) <- UF.descriptor var
            UF.fresh $ Descriptor (Type.unnamedFlexSuper Comparable) rank noMark Nothing
      guardedUnify orientation compVar var



-- UNIFY ALIASES


unifyAlias :: Context -> ModuleName.Canonical -> N.Name -> [(N.Name, Variable)] -> Variable -> Content -> Unify ()
unifyAlias context home name args realVar otherContent =
  case otherContent of
    FlexVar _ ->
        merge context (Alias home name args realVar)

    FlexSuper _ _ ->
        subUnify context realVar (_second context)

    RigidVar _ ->
        subUnify context realVar (_second context)

    RigidSuper _ _ ->
        subUnify context realVar (_second context)

    Alias otherHome otherName otherArgs otherRealVar ->
        if name == otherName && home == otherHome then
            do  zipWithM_ (subUnify context) (map snd args) (map snd otherArgs)
                merge context otherContent

        else
            subUnify context realVar otherRealVar

    Structure _ ->
        subUnify context realVar (_second context)

    Error _ ->
        return ()



-- UNIFY STRUCTURES


unifyStructure :: Context -> FlatType -> Content -> Content -> Unify ()
unifyStructure context flatType content otherContent =
  case otherContent of
    FlexVar _ ->
        merge context content

    FlexSuper super _ ->
        unifyFlexSuperStructure (reorient context) super flatType

    RigidVar name ->
        mismatch context $ Just $ Error.RigidVarTooGeneric name (flatTypeToSpecificThing flatType)

    RigidSuper super name ->
        mismatch context $ Just $ Error.RigidSuperTooGeneric super name (flatTypeToSpecificThing flatType)

    Alias _ _ _ realVar ->
        subUnify context (_first context) realVar

    Structure otherTerm ->
        case (flatType, otherTerm) of
          (App1 home name args, App1 otherHome otherName otherArgs) ->
              if home == otherHome && name == otherName then
                do  zipWithM_ (subUnify context) args otherArgs
                    merge context otherContent

              else if isIntFloat home name otherHome otherName then
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
              Unify $ \vars ok err ->
                do  firstStructure <- gatherFields context fields extension
                    secondStructure <- gatherFields context otherFields otherExtension
                    case unifyRecord context firstStructure secondStructure of
                      Unify k ->
                        k vars ok err

          _ ->
              mismatch context Nothing

    Error _ ->
        return ()



isIntFloat :: ModuleName.Canonical -> N.Name -> ModuleName.Canonical -> N.Name -> Bool
isIntFloat home name otherHome otherName =
  otherHome == ModuleName.basics
  && home == ModuleName.basics
  && (
    (name == N.int && otherName == N.float)
    ||
    (name == N.float && otherName == N.int)
  )



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
                subExt <- fresh context Type.unnamedFlexVar
                expRecord <- fresh context (Structure (Record1 uniqueActFields subExt))
                actRecord <- fresh context (Structure (Record1 uniqueExpFields subExt))
                subUnify context expVar expRecord
                subUnify context actRecord actVar
                unifySharedFields subFields subExt



unifySharedFieldsHelp :: Context -> Map.Map N.Name (Variable, Variable) -> Unify ()
unifySharedFieldsHelp context sharedFields =
  do  badFields <- Map.traverseMaybeWithKey (unifyField context) sharedFields
      case Map.toList badFields of
        [] ->
          return ()

        badFieldPairs ->
          mismatch context (Just (Error.BadFields badFieldPairs))


unifyField :: Context -> N.Name -> (Variable, Variable) -> Unify (Maybe (Maybe Error.Reason))
unifyField context _ (actual, expected) =
  Unify $ \vars ok _ ->
    case subUnify context actual expected of
      Unify k ->
        k vars
          (\vs () -> ok vs Nothing)
          (\vs problem ->
              ok vs $ Just $
                case problem of
                  Reason _ reason ->
                    reason

                  Infinite ->
                    Nothing
          )



-- GATHER RECORD STRUCTURE


data RecordStructure =
  RecordStructure
    { _fields :: Map.Map N.Name Variable
    , _extVar :: Variable
    , _extStruct :: ExtensionStructure
    }


data ExtensionStructure
  = Empty
  | Extension


gatherFields :: Context -> Map.Map N.Name Variable -> Variable -> IO RecordStructure
gatherFields context fields variable =
  do  (Descriptor content _ _ _) <- UF.descriptor variable
      case content of
        Structure (Record1 subFields subExt) ->
            gatherFields context (Map.union fields subFields) subExt

        Structure EmptyRecord1 ->
            return (RecordStructure fields variable Empty)

        Alias _ _ _ var ->
            -- TODO may be dropping useful alias info here
            gatherFields context fields var

        _ ->
            return (RecordStructure fields variable Extension)

