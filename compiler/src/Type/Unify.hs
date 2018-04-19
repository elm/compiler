{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Type.Unify
  ( Answer(..)
  , unify
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Module.Name as ModuleName
import qualified Elm.Name as N
import qualified Type.Error as Error
import qualified Type.Occurs as Occurs
import Type.Type as Type
import qualified Type.UnionFind as UF



-- UNIFY


data Answer
  = Ok [Variable]
  | Err [Variable] Error.Type Error.Type


unify :: Variable -> Variable -> IO Answer
unify v1 v2 =
  case guardedUnify v1 v2 of
    Unify k ->
      k [] onSuccess $ \vars () ->
        do  t1 <- Type.toErrorType v1
            t2 <- Type.toErrorType v2
            UF.union v1 v2 errorDescriptor
            return (Err vars t1 t2)


onSuccess :: [Variable] -> () -> IO Answer
onSuccess vars () =
  return (Ok vars)


{-# NOINLINE errorDescriptor #-}
errorDescriptor :: Descriptor
errorDescriptor =
  Descriptor Error noRank noMark Nothing



-- CPS UNIFIER


newtype Unify a =
  Unify (forall r.
    [Variable]
    -> ([Variable] -> a -> IO r)
    -> ([Variable] -> () -> IO r)
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


mismatch :: Unify a
mismatch =
  Unify $ \vars _ err ->
    err vars ()



-- UNIFICATION HELPERS


data Context =
  Context
    { _first :: Variable
    , _firstDesc :: Descriptor
    , _second :: Variable
    , _secondDesc :: Descriptor
    }


reorient :: Context -> Context
reorient (Context var1 desc1 var2 desc2) =
  Context var2 desc2 var1 desc1



-- MERGE


merge :: Context -> Content -> Unify ()
merge (Context var1 (Descriptor _ rank1 _ _) var2 (Descriptor _ rank2 _ _)) content =
  Unify $ \vars ok _ ->
    ok vars =<<
      UF.union var1 var2 (Descriptor content (min rank1 rank2) noMark Nothing)


fresh :: Context -> Content -> Unify Variable
fresh (Context _ (Descriptor _ rank1 _ _) _ (Descriptor _ rank2 _ _)) content =
  register $ UF.fresh $
    Descriptor content (min rank1 rank2) noMark Nothing



-- ACTUALLY UNIFY THINGS


guardedUnify :: Variable -> Variable -> Unify ()
guardedUnify left right =
  Unify $ \vars ok err ->
    do  equivalent <- UF.equivalent left right
        if equivalent
          then ok vars ()
          else
            do  leftDesc <- UF.get left
                rightDesc <- UF.get right
                case actuallyUnify (Context left leftDesc right rightDesc) of
                  Unify k ->
                    k vars ok err


subUnify :: Variable -> Variable -> Unify ()
subUnify var1 var2 =
  guardedUnify var1 var2


actuallyUnify :: Context -> Unify ()
actuallyUnify context@(Context _ (Descriptor firstContent _ _ _) _ (Descriptor secondContent _ _ _)) =
  case firstContent of
    FlexVar _ ->
        unifyFlex context firstContent secondContent

    FlexSuper super _ ->
        unifyFlexSuper context super firstContent secondContent

    RigidVar _ ->
        unifyRigid context Nothing firstContent secondContent

    RigidSuper super _ ->
        unifyRigid context (Just super) firstContent secondContent

    Alias home name args realVar ->
        unifyAlias context home name args realVar secondContent

    Structure flatType ->
        unifyStructure context flatType firstContent secondContent

    Error ->
        -- If there was an error, just pretend it is okay. This lets us avoid
        -- "cascading" errors where one problem manifests as multiple message.
        merge context Error



-- UNIFY FLEXIBLE VARIABLES


unifyFlex :: Context -> Content -> Content -> Unify ()
unifyFlex context content otherContent =
  case otherContent of
    Error ->
        merge context Error

    -- TODO see if wildcarding makes a noticable perf difference

    FlexVar maybeName ->
        merge context $
          case maybeName of
            Nothing ->
              content

            Just _ ->
              otherContent

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


unifyRigid :: Context -> Maybe SuperType -> Content -> Content -> Unify ()
unifyRigid context maybeSuper content otherContent =
  case otherContent of
    FlexVar _ ->
        merge context content

    FlexSuper otherSuper _ ->
        case maybeSuper of
          Just super ->
            if combineRigidSupers super otherSuper then
              merge context content
            else
              mismatch

          Nothing ->
            mismatch

    RigidVar _ ->
        mismatch

    RigidSuper _ _ ->
        mismatch

    Alias _ _ _ _ ->
        mismatch

    Structure _ ->
        mismatch

    Error ->
        merge context Error



-- UNIFY SUPER VARIABLES


unifyFlexSuper :: Context -> SuperType -> Content -> Content -> Unify ()
unifyFlexSuper context super content otherContent =
  case otherContent of
    Structure flatType ->
        unifyFlexSuperStructure context super flatType

    RigidVar _ ->
        mismatch

    RigidSuper otherSuper _ ->
        if combineRigidSupers otherSuper super then
            merge context otherContent
        else
            mismatch

    FlexVar _ ->
        merge context content

    FlexSuper otherSuper _ ->
      case super of
        Number ->
          case otherSuper of
            Number     -> merge context content
            Comparable -> merge context content
            Appendable -> mismatch
            CompAppend -> mismatch

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
            Number     -> mismatch

        CompAppend ->
          case otherSuper of
            Comparable -> merge context content
            Appendable -> merge context content
            CompAppend -> merge context content
            Number     -> mismatch

    Alias _ _ _ realVar ->
        subUnify (_first context) realVar

    Error ->
        merge context Error


combineRigidSupers :: SuperType -> SuperType -> Bool
combineRigidSupers rigid flex =
  rigid == flex
  || (rigid == Number && flex == Comparable)
  || (rigid == CompAppend && (flex == Comparable || flex == Appendable))


atomMatchesSuper :: SuperType -> ModuleName.Canonical -> N.Name -> Bool
atomMatchesSuper super home name =
  case super of
    Number ->
      isNumber home name

    Comparable ->
      isNumber home name
      || Error.isString home name
      || Error.isChar home name

    Appendable ->
      Error.isString home name

    CompAppend ->
      Error.isString home name


isNumber :: ModuleName.Canonical -> N.Name -> Bool
isNumber home name =
  home == ModuleName.basics
  &&
  (name == N.int || name == N.float)


unifyFlexSuperStructure :: Context -> SuperType -> FlatType -> Unify ()
unifyFlexSuperStructure context super flatType =
  case flatType of
    App1 home name [] ->
      if atomMatchesSuper super home name then
        merge context (Structure flatType)
      else
        mismatch

    App1 home name [variable] | home == ModuleName.list && name == N.list ->
      case super of
        Number ->
            mismatch

        Appendable ->
            merge context (Structure flatType)

        Comparable ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                unifyComparableRecursive variable

        CompAppend ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                unifyComparableRecursive variable

    Tuple1 a b maybeC ->
      case super of
        Number ->
            mismatch

        Appendable ->
            mismatch

        Comparable ->
            do  comparableOccursCheck context
                merge context (Structure flatType)
                unifyComparableRecursive a
                unifyComparableRecursive b
                case maybeC of
                  Nothing ->
                    return ()

                  Just c ->
                    unifyComparableRecursive c

        CompAppend ->
            mismatch

    _ ->
      mismatch


-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?
comparableOccursCheck :: Context -> Unify ()
comparableOccursCheck (Context _ _ var _) =
  Unify $ \vars ok err ->
    do  hasOccurred <- Occurs.occurs var
        if hasOccurred
          then err vars ()
          else ok vars ()


unifyComparableRecursive :: Variable -> Unify ()
unifyComparableRecursive var =
  do  compVar <- register $
        do  (Descriptor _ rank _ _) <- UF.get var
            UF.fresh $ Descriptor (Type.unnamedFlexSuper Comparable) rank noMark Nothing
      guardedUnify compVar var



-- UNIFY ALIASES


unifyAlias :: Context -> ModuleName.Canonical -> N.Name -> [(N.Name, Variable)] -> Variable -> Content -> Unify ()
unifyAlias context home name args realVar otherContent =
  case otherContent of
    FlexVar _ ->
      merge context (Alias home name args realVar)

    FlexSuper _ _ ->
      subUnify realVar (_second context)

    RigidVar _ ->
      subUnify realVar (_second context)

    RigidSuper _ _ ->
      subUnify realVar (_second context)

    Alias otherHome otherName otherArgs otherRealVar ->
      if name == otherName && home == otherHome then
        Unify $ \vars ok err ->
          let
            ok1 vars1 () =
              case merge context otherContent of
                Unify k ->
                  k vars1 ok err
          in
          unifyAliasArgs vars context args otherArgs ok1 err

      else
        subUnify realVar otherRealVar

    Structure _ ->
      subUnify realVar (_second context)

    Error ->
      merge context Error


unifyAliasArgs :: [Variable] -> Context -> [(N.Name,Variable)] -> [(N.Name,Variable)] -> ([Variable] -> () -> IO r) -> ([Variable] -> () -> IO r) -> IO r
unifyAliasArgs vars context args1 args2 ok err =
  case args1 of
    (_,arg1):others1 ->
      case args2 of
        (_,arg2):others2 ->
          case subUnify arg1 arg2 of
            Unify k ->
              k vars
                (\vs () -> unifyAliasArgs vs context others1 others2 ok err)
                (\vs () -> unifyAliasArgs vs context others1 others2 err err)

        _ ->
          err vars ()

    [] ->
      case args2 of
        [] ->
          ok vars ()

        _ ->
          err vars ()



-- UNIFY STRUCTURES


unifyStructure :: Context -> FlatType -> Content -> Content -> Unify ()
unifyStructure context flatType content otherContent =
  case otherContent of
    FlexVar _ ->
        merge context content

    FlexSuper super _ ->
        unifyFlexSuperStructure (reorient context) super flatType

    RigidVar _ ->
        mismatch

    RigidSuper _ _ ->
        mismatch

    Alias _ _ _ realVar ->
        subUnify (_first context) realVar

    Structure otherFlatType ->
        case (flatType, otherFlatType) of
          (App1 home name args, App1 otherHome otherName otherArgs) | home == otherHome && name == otherName ->
              Unify $ \vars ok err ->
                let
                  ok1 vars1 () =
                    case merge context otherContent of
                      Unify k ->
                        k vars1 ok err
                in
                unifyArgs vars context args otherArgs ok1 err

          (Fun1 arg1 res1, Fun1 arg2 res2) ->
              do  subUnify arg1 arg2
                  subUnify res1 res2
                  merge context otherContent

          (EmptyRecord1, EmptyRecord1) ->
              merge context otherContent

          (Record1 fields ext, EmptyRecord1) | Map.null fields ->
              subUnify ext (_second context)

          (EmptyRecord1, Record1 fields ext) | Map.null fields ->
              subUnify (_first context) ext

          (Record1 fields1 ext1, Record1 fields2 ext2) ->
              Unify $ \vars ok err ->
                do  structure1 <- gatherFields fields1 ext1
                    structure2 <- gatherFields fields2 ext2
                    case unifyRecord context structure1 structure2 of
                      Unify k ->
                        k vars ok err

          (Tuple1 a b Nothing, Tuple1 x y Nothing) ->
              do  subUnify a x
                  subUnify b y
                  merge context otherContent

          (Tuple1 a b (Just c), Tuple1 x y (Just z)) ->
              do  subUnify a x
                  subUnify b y
                  subUnify c z
                  merge context otherContent

          (Unit1, Unit1) ->
              merge context otherContent

          _ ->
              mismatch

    Error ->
        merge context Error



-- UNIFY ARGS


unifyArgs :: [Variable] -> Context -> [Variable] -> [Variable] -> ([Variable] -> () -> IO r) -> ([Variable] -> () -> IO r) -> IO r
unifyArgs vars context args1 args2 ok err =
  case args1 of
    arg1:others1 ->
      case args2 of
        arg2:others2 ->
          case subUnify arg1 arg2 of
            Unify k ->
              k vars
                (\vs () -> unifyArgs vs context others1 others2 ok err)
                (\vs () -> unifyArgs vs context others1 others2 err err)

        _ ->
          err vars ()

    [] ->
      case args2 of
        [] ->
          ok vars ()

        _ ->
          err vars ()



-- UNIFY RECORDS


unifyRecord :: Context -> RecordStructure -> RecordStructure -> Unify ()
unifyRecord context (RecordStructure fields1 ext1) (RecordStructure fields2 ext2) =
  let
    sharedFields = Map.intersectionWith (,) fields1 fields2
    uniqueFields1 = Map.difference fields1 fields2
    uniqueFields2 = Map.difference fields2 fields1
  in
  if Map.null uniqueFields1 then

    if Map.null uniqueFields2 then
      do  subUnify ext1 ext2
          unifySharedFields context sharedFields Map.empty ext1

    else
      do  subRecord <- fresh context (Structure (Record1 uniqueFields2 ext2))
          subUnify ext1 subRecord
          unifySharedFields context sharedFields Map.empty subRecord

  else

    if Map.null uniqueFields2 then
      do  subRecord <- fresh context (Structure (Record1 uniqueFields1 ext1))
          subUnify subRecord ext2
          unifySharedFields context sharedFields Map.empty subRecord

    else
      do  let otherFields = Map.union uniqueFields1 uniqueFields2
          ext <- fresh context Type.unnamedFlexVar
          sub1 <- fresh context (Structure (Record1 uniqueFields1 ext))
          sub2 <- fresh context (Structure (Record1 uniqueFields2 ext))
          subUnify ext1 sub2
          subUnify sub1 ext2
          unifySharedFields context sharedFields otherFields ext


unifySharedFields :: Context -> Map.Map N.Name (Variable, Variable) -> Map.Map N.Name Variable -> Variable -> Unify ()
unifySharedFields context sharedFields otherFields ext =
  do  matchingFields <- Map.traverseMaybeWithKey unifyField sharedFields
      if Map.size sharedFields == Map.size matchingFields
        then merge context (Structure (Record1 (Map.union matchingFields otherFields) ext))
        else mismatch


unifyField :: N.Name -> (Variable, Variable) -> Unify (Maybe Variable)
unifyField _ (actual, expected) =
  Unify $ \vars ok _ ->
    case subUnify actual expected of
      Unify k ->
        k vars
          (\vs () -> ok vs (Just actual))
          (\vs () -> ok vs Nothing)



-- GATHER RECORD STRUCTURE


data RecordStructure =
  RecordStructure
    { _fields :: Map.Map N.Name Variable
    , _extension :: Variable
    }


gatherFields :: Map.Map N.Name Variable -> Variable -> IO RecordStructure
gatherFields fields variable =
  do  (Descriptor content _ _ _) <- UF.get variable
      case content of
        Structure (Record1 subFields subExt) ->
            gatherFields (Map.union fields subFields) subExt

        Alias _ _ _ var ->
            -- TODO may be dropping useful alias info here
            gatherFields fields var

        _ ->
            return (RecordStructure fields variable)

