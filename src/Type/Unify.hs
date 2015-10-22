{-# OPTIONS_GHC -Wall #-}
module Type.Unify (unify) where

import Control.Monad (zipWithM_)
import Control.Monad.Except (ExceptT, lift, liftIO, throwError, runExceptT)
import qualified Data.Map as Map
import qualified Data.UnionFind.IO as UF

import qualified AST.Variable as Var
import qualified Reporting.Region as R
import qualified Reporting.Error.Type as Error
import qualified Type.State as TS
import Type.Type as Type



-- KICK OFF UNIFICATION


unify :: Error.Hint -> R.Region -> Variable -> Variable -> TS.Solver ()
unify hint region expected actual =
  do  result <- runExceptT (guardedUnify ExpectedActual expected actual)
      case result of
        Right state ->
            return state

        Left (Mismatch subExpected subActual maybeReason) ->
            let
              mkError =
                do  expectedSrcType <- Type.toSrcType expected
                    actualSrcType <- Type.toSrcType actual
                    mergeHelp subExpected subActual Error
                    let info = Error.MismatchInfo hint expectedSrcType actualSrcType maybeReason
                    return (Error.Mismatch info)
            in
              TS.addError region =<< liftIO mkError



-- UNIFICATION HELPERS


type Unify =
  ExceptT Mismatch TS.Solver


data Context = Context
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


data Mismatch
    = Mismatch Variable Variable (Maybe Error.Reason)


mismatch :: Context -> Maybe Error.Reason -> Unify a
mismatch (Context orientation first _ second _) maybeReason =
  let
    (expected, actual) =
        case orientation of
          ExpectedActual ->
              (first, second)

          ActualExpected ->
              (second, first)
  in
    throwError (Mismatch expected actual maybeReason)


rigidityError :: Context -> Maybe String -> Unify ()
rigidityError context maybeName =
  mismatch context (Just (Error.Rigid maybeName))



-- MERGE


merge :: Context -> Content -> Unify ()
merge (Context _ first _ second _) content =
  liftIO $ mergeHelp first second content


mergeHelp :: Variable -> Variable -> Content -> IO ()
mergeHelp first second content =
  UF.union' first second $ \desc1 desc2 ->
      return $
        Descriptor
          { _content = content
          , _rank = min (_rank desc1) (_rank desc2)
          , _mark = noMark
          , _copy = Nothing
          }


fresh :: Context -> Content -> Unify Variable
fresh (Context _ _ desc1 _ desc2) content =
  do  freshVariable <-
          liftIO $ UF.fresh $
            Descriptor
              { _content = content
              , _rank = min (_rank desc1) (_rank desc2)
              , _mark = noMark
              , _copy = Nothing
              }
      lift (TS.register freshVariable)



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
actuallyUnify context@(Context _ _ firstDesc _ secondDesc) =
  let
    secondContent = _content secondDesc
  in
  case _content firstDesc of
    Error ->
        -- If there was an error, just pretend it is okay. This lets us avoid
        -- "cascading" errors where one problem manifests as multiple message.
        return ()

    Var Flex Nothing _ ->
        unifyFlex context secondContent

    Var Flex (Just super) _ ->
        unifySuper context super secondContent

    Var Rigid maybeSuper maybeName ->
        unifyRigid context maybeSuper maybeName secondContent

    Atom name ->
        unifyAtom context name secondContent

    Alias name args realVar ->
        unifyAlias context name args realVar secondContent

    Structure term ->
        unifyStructure context term secondContent



-- UNIFY FLEXIBLE VARIABLES


unifyFlex :: Context -> Content -> Unify ()
unifyFlex context otherContent =
  case otherContent of
    Error ->
        return ()

    Var Flex _ _ ->
        merge context otherContent

    Var Rigid _ _ ->
        merge context otherContent

    Atom _ ->
        merge context otherContent

    Alias _ _ _ ->
        merge context otherContent

    Structure _ ->
        merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid :: Context -> Maybe Super -> Maybe String -> Content -> Unify ()
unifyRigid context maybeSuper maybeName otherContent =
  case otherContent of
    Error ->
        return ()

    Var Flex otherMaybeSuper _ ->
        if maybeSuper == otherMaybeSuper then
            merge context (Var Rigid maybeSuper maybeName)
        else
            rigidityError context maybeName

    Var Rigid _ _ ->
        mismatch context (Just Error.DoubleRigid)

    Atom _ ->
        rigidityError context maybeName

    Alias _ _ _ ->
        rigidityError context maybeName

    Structure _ ->
        rigidityError context maybeName


-- UNIFY SUPER VARIABLES


unifySuper :: Context -> Super -> Content -> Unify ()
unifySuper context super otherContent =
  case otherContent of
    Structure term ->
        unifySuperStructure context super term

    Atom name ->
        if atomMatchesSuper super name then
            merge context otherContent
        else
            mismatch context (Just (superReason super))

    Var Rigid Nothing maybeName ->
        rigidityError context maybeName

    Var Rigid (Just otherSuper) maybeName ->
        if super == otherSuper then
            merge context otherContent
        else
            rigidityError context maybeName

    Var Flex Nothing _ ->
        merge context (Var Flex (Just super) Nothing)

    Var Flex (Just otherSuper) _ ->
        case combineSupers super otherSuper of
          Nothing ->
              mismatch context Nothing

          Just newSuper ->
              merge context (Var Flex (Just newSuper) Nothing)

    Alias _ _ realVar ->
        subUnify context (_first context) realVar

    Error ->
        return ()


combineSupers :: Super -> Super -> Maybe Super
combineSupers firstSuper secondSuper =
  case (firstSuper, secondSuper) of
    (Number    , Number    ) -> Just Number
    (Comparable, Number    ) -> Just Number
    (Number    , Comparable) -> Just Number

    (Comparable, Comparable) -> Just Comparable
    (Appendable, Appendable) -> Just Appendable

    (Appendable, Comparable) -> Just CompAppend
    (Comparable, Appendable) -> Just CompAppend

    (CompAppend, CompAppend) -> Just CompAppend
    (CompAppend, Comparable) -> Just CompAppend
    (Comparable, CompAppend) -> Just CompAppend
    (CompAppend, Appendable) -> Just CompAppend
    (Appendable, CompAppend) -> Just CompAppend

    (_         , _         ) -> Nothing


superReason :: Super -> Error.Reason
superReason super =
  case super of
    Number ->
        Error.NotNumber

    Comparable ->
        Error.NotComparable

    Appendable ->
        Error.NotAppendable

    CompAppend ->
        Error.NotCompAppend


isPrimitiveFrom :: [String] -> Var.Canonical -> Bool
isPrimitiveFrom prims var =
  any (\p -> Var.isPrim p var) prims


atomMatchesSuper :: Super -> Var.Canonical -> Bool
atomMatchesSuper super name =
  case super of
    Number ->
        isPrimitiveFrom ["Int", "Float"] name

    Comparable ->
        isPrimitiveFrom ["Int", "Float", "Char", "String"] name

    Appendable ->
        Var.isPrim "String" name || Var.isText name

    CompAppend ->
        Var.isPrim "String" name


unifySuperStructure :: Context -> Super -> Term1 Variable -> Unify ()
unifySuperStructure context super term =
  do  appStructure <- liftIO (collectApps (Structure term))
      case appStructure of
        Other ->
            mismatch context (Just (superReason super))

        List variable ->
            case super of
              Number ->
                  mismatch context (Just Error.NotNumber)

              Appendable ->
                  merge context (Structure term)

              Comparable ->
                  do  merge context (Structure term)
                      unifyComparableRecursive (_orientation context) variable

              CompAppend ->
                  do  merge context (Structure term)
                      unifyComparableRecursive (_orientation context) variable

        Tuple entries ->
            case super of
              Number ->
                  mismatch context (Just Error.NotNumber)

              Appendable ->
                  mismatch context (Just Error.NotAppendable)

              Comparable ->
                  if length entries > 6 then
                      mismatch context (Just (Error.TooLongComparableTuple (length entries)))

                  else
                      do  merge context (Structure term)
                          mapM_ (unifyComparableRecursive (_orientation context)) entries

              CompAppend ->
                  mismatch context (Just Error.NotCompAppend)


unifyComparableRecursive :: Orientation -> Variable -> Unify ()
unifyComparableRecursive orientation var =
  do  compVar <-
          liftIO $
            do  desc <- UF.descriptor var
                UF.fresh $
                  Descriptor
                    { _content = Var Flex (Just Comparable) Nothing
                    , _rank = _rank desc
                    , _mark = noMark
                    , _copy = Nothing
                    }

      guardedUnify orientation compVar var


data AppStructure
    = List Variable
    | Tuple [Variable]
    | Other


collectApps :: Content -> IO AppStructure
collectApps content =
    collectAppsHelp [] content


collectAppsHelp :: [Variable] -> Content -> IO AppStructure
collectAppsHelp args content =
  case (content, args) of
    (Structure (App1 func arg), _) ->
        collectAppsHelp (args ++ [arg]) =<< getContent func

    (Atom name, [arg]) | Var.isList name ->
        return (List arg)

    (Atom name, _) | Var.isTuple name ->
        return (Tuple args)

    _ ->
        return Other


getContent :: Variable -> IO Content
getContent variable =
  _content <$> UF.descriptor variable



-- UNIFY ATOMS


unifyAtom :: Context -> Var.Canonical -> Content -> Unify ()
unifyAtom context name otherContent =
  case otherContent of
    Error ->
        return ()

    Var Flex Nothing _ ->
        merge context (Atom name)

    Var Flex (Just super) _ ->
        if atomMatchesSuper super name then
            merge context (Atom name)

        else
            mismatch context (Just (superReason super))

    Var Rigid _ maybeName ->
        rigidityError context maybeName

    Atom otherName ->
        if name == otherName then
            merge context otherContent

        else
            mismatch context $
              if isIntFloat name otherName || isIntFloat otherName name then
                  Just Error.IntFloat
              else
                  Nothing

    Alias _ _ realVar ->
        subUnify context (_first context) realVar

    Structure _ ->
        mismatch context Nothing


isIntFloat :: Var.Canonical -> Var.Canonical -> Bool
isIntFloat name otherName =
  Var.isPrim "Int" name && Var.isPrim "Float" otherName


-- UNIFY ALIASES


unifyAlias :: Context -> Var.Canonical -> [(String, Variable)] -> Variable -> Content -> Unify ()
unifyAlias context name args realVar otherContent =
  case otherContent of
    Error ->
        return ()

    Var Flex Nothing _ ->
        merge context (Alias name args realVar)

    Var _ _ _ ->
        subUnify context realVar (_second context)

    Atom _ ->
        subUnify context realVar (_second context)


    Alias otherName otherArgs otherRealVar ->
        if name == otherName then
            do  merge context otherContent
                zipWithM_ (subUnify context) (map snd args) (map snd otherArgs)

        else
            subUnify context realVar otherRealVar

    Structure _ ->
        subUnify context realVar (_second context)



-- UNIFY STRUCTURES


unifyStructure :: Context -> Term1 Variable -> Content -> Unify ()
unifyStructure context term otherContent =
  case otherContent of
    Error ->
        return ()

    Var Flex Nothing _ ->
        merge context (Structure term)

    Var Flex (Just super) _ ->
        unifySuper (reorient context) super (Structure term)

    Var Rigid _ maybeName ->
        rigidityError context maybeName

    Atom _ ->
        mismatch context Nothing

    Alias _ _ realVar ->
        subUnify context (_first context) realVar

    Structure otherTerm ->
        case (term, otherTerm) of
          (App1 func arg, App1 otherFunc otherArg) ->
              do  merge context otherContent
                  subUnify context func otherFunc
                  subUnify context arg otherArg

          (Fun1 arg result, Fun1 otherArg otherResult) ->
              do  merge context otherContent
                  subUnify context arg otherArg
                  subUnify context result otherResult

          (EmptyRecord1, EmptyRecord1) ->
              merge context otherContent

          (Record1 fields ext, EmptyRecord1) | Map.null fields ->
              subUnify context ext (_second context)

          (EmptyRecord1, Record1 fields ext) | Map.null fields ->
              subUnify context (_first context) ext

          (Record1 fields extension, Record1 otherFields otherExtension) ->
              do  firstStructure <- gatherFields context fields extension
                  secondStructure <- gatherFields context otherFields otherExtension
                  unifyRecord context firstStructure secondStructure

          _ ->
              mismatch context Nothing



-- UNIFY RECORDS


unifyRecord :: Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord context firstStructure secondStructure =
  do  let (RecordStructure expFields expVar expStruct) = firstStructure
      let (RecordStructure actFields actVar actStruct) = secondStructure

      -- call after unifying extension, make sure record shape matches before
      -- looking into whether the particular field types match.
      let unifySharedFields otherFields ext =
            do  let sharedFields = Map.intersectionWith (,) expFields actFields
                _ <- traverse (uncurry (subUnify context)) sharedFields
                let allFields = Map.union (Map.map fst sharedFields) otherFields
                merge context (Structure (Record1 allFields ext))

      let uniqueExpFields = Map.difference expFields actFields
      let uniqueActFields = Map.difference actFields expFields

      case (expStruct, Map.null uniqueExpFields, actStruct, Map.null uniqueActFields) of
        (_, True, _, True) ->
            do  subUnify context expVar actVar
                unifySharedFields Map.empty expVar

        (Empty, _, _, False) ->
            mismatch context (Just (Error.MessyFields (Map.keys uniqueExpFields) (Map.keys uniqueActFields)))

        (_, False, Empty, _) ->
            mismatch context (Just (Error.MessyFields (Map.keys uniqueExpFields) (Map.keys uniqueActFields)))

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



-- GATHER RECORD STRUCTURE


data RecordStructure = RecordStructure
    { _fields :: Map.Map String Variable
    , _extVar :: Variable
    , _extStruct :: ExtensionStructure
    }


data ExtensionStructure
    = Empty
    | Extension


gatherFields :: Context -> Map.Map String Variable -> Variable -> Unify RecordStructure
gatherFields context fields variable =
  do  desc <- liftIO (UF.descriptor variable)
      case _content desc of
        Structure (Record1 subFields subExt) ->
            gatherFields context (Map.union fields subFields) subExt

        Structure EmptyRecord1 ->
            return (RecordStructure fields variable Empty)

        Alias _ _ var ->
            -- TODO may be dropping useful alias info here
            gatherFields context fields var

        _ ->
            return (RecordStructure fields variable Extension)

