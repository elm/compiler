{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, UnboxedTuples #-}
module Type.Unify
  ( Answer(..)
  , unify
  )
  where


import Data.Map.Internal (Map(Tip,Bin))
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyMap as NEM
import GHC.Exts (isTrue#)
import GHC.Prim ((+#), (==#), (<#), indexSmallArray#, sizeofSmallArray#)

import qualified Array as A
import qualified String as S

import qualified AST.Prim.Fields as F
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Render.Type as Err
import qualified Type.Occurs as Occurs
import Type.Type as Type
import qualified Type.UnionFind as UF



-- UNIFY


data Answer
  = Ok [Variable]
  | Err [Variable] Err.Type Err.Type


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



-- GUARDED UNIFY
--
-- Check if the two variables are equivalent. If so, you can skip the more
-- expensive check because they already match!


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



-- ACTUALLY UNIFY
--
-- Everything can unify with an Error! Actually having Unify fail is reserved
-- for legitimate mismatches. This avoids "cascading" errors where one problem
-- manifests as multiple message.


actuallyUnify :: Context -> Unify ()
actuallyUnify context@(Context _ (Descriptor firstContent _ _ _) _ (Descriptor secondContent _ _ _)) =
  case firstContent of
    Flex             v -> unifyFlex   context v firstContent secondContent
    Rigid            v -> unifyRigid  context v firstContent secondContent
    Alias h n _ xs _ v -> unifyAlias  context h n xs v       secondContent
    Structure        t -> unifyStruct context t firstContent secondContent
    Error              -> merge context Error



-- UNIFY VARIABLES


unifyFlex :: Context -> T.Var -> Content -> Content -> Unify ()
unifyFlex context var content otherContent =
  case otherContent of
    Flex  otherVar    -> unifyFlexWithFlex context var content otherVar otherContent
    Rigid otherVar    -> unifyRigidWithFlex (reorient context) otherVar otherContent var
    Alias _ _ _ _ _ v -> subUnify (_first context) v
    Structure       t -> unifyFlexWithStructure context var t otherContent
    Error             -> merge context Error


unifyRigid :: Context -> T.Var -> Content -> Content -> Unify ()
unifyRigid context var content otherContent =
  case otherContent of
    Flex  otherVar    -> unifyRigidWithFlex context var content otherVar
    Rigid _           -> mismatch
    Alias _ _ _ _ _ _ -> mismatch
    Structure _       -> mismatch
    Error             -> merge context Error


unifyFlexWithFlex :: Context -> T.Var -> Content -> T.Var -> Content -> Unify ()
unifyFlexWithFlex context (T.Var name con) content (T.Var otherName otherCon) otherContent =
  T.unifyFlexWithFlex con otherCon
    (
      if S.size name >= S.size otherName
      then merge context content
      else merge context otherContent
    )
    (merge context content)
    (merge context otherContent)
    (merge context flexCompAppend)
    mismatch


unifyRigidWithFlex :: Context -> T.Var -> Content -> T.Var -> Unify ()
unifyRigidWithFlex context (T.Var _ rigidCon) rigidContent (T.Var _ flexCon) =
  T.unifyRigidWithFlex rigidCon flexCon
    (merge context rigidContent)
    mismatch



-- UNIFY FLEX SUPER STRUCTURE


unifyFlexWithStructure :: Context -> T.Var -> FlatType -> Content -> Unify ()
unifyFlexWithStructure context (T.Var _ con) flatType content =
  case flatType of
    App1 h n m args ->
      let
        recurse c =
          do  occursCheck context
              A.traverse_ (unifyFlexRecursive c) args
              merge context content
      in
      case con of
        T.Any        -> merge context content
        T.Comparable -> if M.isComparable m then recurse flexComparable else mismatch
        T.Appendable -> if isAppendable h n then merge context content  else mismatch
        T.CompAppend -> if isAppendable h n then recurse flexComparable else mismatch
        T.Number     -> if M.isNumber     m then merge context content  else mismatch

    Unit1 ->
      case con of
        T.Any        -> merge context content
        T.Comparable -> merge context content
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    Pair1 x y ->
      let
        recurse c =
          do  occursCheck context
              unifyFlexRecursive c x
              unifyFlexRecursive c y
              merge context content
      in
      case con of
        T.Any        -> merge context content
        T.Comparable -> recurse flexComparable
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    Triple1 x y z ->
      let
        recurse c =
          do  occursCheck context
              unifyFlexRecursive c x
              unifyFlexRecursive c y
              unifyFlexRecursive c z
              merge context content
      in
      case con of
        T.Any        -> merge context content
        T.Comparable -> recurse flexComparable
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    Empty1 ->
      case con of
        T.Any        -> merge context content
        T.Comparable -> mismatch
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    Record1 _ _ ->
      case con of
        T.Any        -> merge context content
        T.Comparable -> mismatch
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    ExtRec1 _ _ _ ->
      case con of
        T.Any        -> merge context content
        T.Comparable -> mismatch
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch

    Fun1 _ _ ->
      case con of
        T.Any        -> merge context content
        T.Comparable -> mismatch
        T.Appendable -> mismatch
        T.CompAppend -> mismatch
        T.Number     -> mismatch


-- TODO: is there some way to avoid doing this?
-- Do type classes require occurs checks?
occursCheck :: Context -> Unify ()
occursCheck (Context _ _ var _) =
  Unify $ \vars ok err ->
    do  hasOccurred <- Occurs.occurs var
        if hasOccurred
          then err vars ()
          else ok vars ()


unifyFlexRecursive :: Content -> Variable -> Unify ()
unifyFlexRecursive content subVar =
  do  subDesc@(Descriptor _ rank _ _) <- io $ UF.get subVar
      let desc = Descriptor content rank noMark Nothing
      var <- register desc
      actuallyUnify (Context var desc subVar subDesc)


isAppendable :: ModuleName.Canonical -> T.Name -> Bool
isAppendable home name =
  (home == ModuleName.string && name == T.string)
  ||
  (home == ModuleName.list && name == T.list)



-- UNIFY ALIASES


unifyAlias :: Context -> ModuleName.Canonical -> T.Name -> A.Array Variable -> Variable -> Content -> Unify ()
unifyAlias context home name args realVar otherContent =
  case otherContent of
    Flex _ ->
      subUnify realVar (_second context)

    Rigid _ ->
      subUnify realVar (_second context)

    Alias otherHome otherName _ otherArgs _ otherRealVar ->
      if name == otherName && home == otherHome
      then
        Unify $ \vars ok err ->
          let
            ok1 vars1 () =
              case merge context otherContent of
                Unify k ->
                  k vars1 ok err
          in
          unifyArgs vars args otherArgs ok1 err
      else
        subUnify realVar otherRealVar

    Structure _ ->
      subUnify realVar (_second context)

    Error ->
      merge context Error


unifyArgs :: [Variable] -> A.Array Variable -> A.Array Variable -> ([Variable] -> () -> IO r) -> ([Variable] -> () -> IO r) -> IO r
unifyArgs variables (A.Array sa1) (A.Array sa2) ok err =
  if isTrue# (len1 ==# len2)
  then go variables 0# len1 ok
  else
    if isTrue# (len1 <# len2)
    then go variables 0# len1 err
    else go variables 0# len2 err
  where
    len1 = sizeofSmallArray# sa1
    len2 = sizeofSmallArray# sa2

    go vars i len done =
      if isTrue# (i <# len)
      then
        case indexSmallArray# sa1 i of { (# arg1 #) ->
        case indexSmallArray# sa2 i of { (# arg2 #) ->
        case subUnify arg1 arg2     of {  Unify k   ->
          k vars
            (\vs () -> go vs (i +# 1#) len done)
            (\vs () -> go vs (i +# 1#) len err)
        }}}
      else
        done vars ()



-- UNIFY STRUCTURES


unifyStruct :: Context -> FlatType -> Content -> Content -> Unify ()
unifyStruct context flatType content otherContent =
  case otherContent of
    Flex var ->
      unifyFlexWithStructure (reorient context) var flatType content

    Rigid _ ->
      mismatch

    Alias _ _ _ _ _ realVar ->
      subUnify (_first context) realVar

    Structure otherFlatType ->
      case (flatType, otherFlatType) of
        (App1 h n _ args, App1 h' n' _ otherArgs) | h == h' && n == n' ->
            Unify $ \vars ok err ->
              let
                ok1 vars1 () =
                  case merge context otherContent of
                    Unify k ->
                      k vars1 ok err
              in
              unifyArgs vars args otherArgs ok1 err

        (Fun1 arg1 res1, Fun1 arg2 res2) ->
            do  subUnify arg1 arg2
                subUnify res1 res2
                merge context otherContent

        (Empty1, Empty1) ->
            merge context otherContent

        (Record1 _ fields1, Record1 _ fields2) ->
            unifyRecord context fields1 content fields2

        (Record1 _ fields1, ExtRec1 _ fields2 ext2) ->
            do  flat2 <- flatten fields2 ext2
                case flat2 of
                  Record fs   -> unifyRecord context fields1 content fs
                  ExtRec fs e -> unifyRecExt context fields1 content fs e

        (ExtRec1 _ fields1 ext1, Record1 _ fields2) ->
            do  flat1 <- flatten fields1 ext1
                case flat1 of
                  Record fs   -> unifyRecord (reorient context) fields2 content fs
                  ExtRec fs e -> unifyRecExt (reorient context) fields2 content fs e

        (ExtRec1 _ fields1 ext1, ExtRec1 _ fields2 ext2) ->
            do  flat1 <- flatten fields1 ext1
                flat2 <- flatten fields2 ext2
                case (flat1, flat2) of
                  (Record fs1   , Record fs2   ) -> unifyRecord context fs1 content fs2
                  (Record fs1   , ExtRec fs2 e2) -> unifyRecExt context fs1 content fs2 e2
                  (ExtRec fs1 e1, Record fs2   ) -> unifyRecExt (reorient context) fs2 content fs1 e1
                  (ExtRec fs1 e1, ExtRec fs2 e2) -> unifyExtExt context fs1 e1 content fs2 e2 otherContent

        (Pair1 a b, Pair1 x y) ->
            do  subUnify a x
                subUnify b y
                merge context otherContent

        (Triple1 a b c, Triple1 x y z) ->
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



-- UNIFY RECORDS


unifyRecord :: Context -> NEM.Map N.Name Variable -> Content -> NEM.Map N.Name Variable -> Unify ()
unifyRecord context fields1 content fields2 =
  do  isGood <- loop (NEM.toDict fields1) (NEM.toDict fields2)
      if isGood
        then merge context content
        else mismatch
  where
    loop Tip Tip = return True
    loop _   Tip = return False
    loop Tip _   = return False
    loop (Bin _ k v l r) t2 =
      case Map.splitLookup k t2 of
        (l2, mv2, r2) ->
          case mv2 of
            Nothing ->
              do  _ <- loop l l2
                  _ <- loop r r2
                  return False

            Just v2 ->
              do  a <- loop l l2
                  b <- unifyField v v2
                  c <- loop r r2
                  return (a && b && c)


unifyField :: Variable -> Variable -> Unify Bool
unifyField actual expected =
  Unify $ \vars ok _ ->
    case subUnify actual expected of
      Unify k ->
        k vars
          (\vs () -> ok vs True)
          (\vs () -> ok vs False)



-- UNIFY RECORD WITH EXTENSIBLE RECORD


unifyRecExt :: Context -> NEM.Map N.Name Variable -> Content -> NEM.Map N.Name Variable -> Variable -> Unify ()
unifyRecExt context fields1 content partial2 ext2 =
  do  isGood <- loop (NEM.toDict fields1) (NEM.toDict partial2)
      if isGood
        then
          do  ext1 <- fresh context (Structure (diff fields1 partial2))
              subUnify ext1 ext2
              merge context content
        else
          mismatch
  where
    loop _   Tip = return True
    loop Tip _   = return False
    loop (Bin _ k v l r) t2 =
      case Map.splitLookup k t2 of
        (l2, mv2, r2) ->
          case mv2 of
            Nothing ->
              do  a <- loop l l2
                  b <- loop r r2
                  return (a && b)

            Just v2 ->
              do  a <- loop l l2
                  b <- unifyField v v2
                  c <- loop r r2
                  return (a && b && c)

    diff f1 p2 =
      if NEM.size f1 == NEM.size p2
      then Empty1
      else Record1 order (NEM.unsafeDiff f1 p2)

    !(F.Order order) = F.unordered



-- UNIFY EXTENSIBLE RECORDS


unifyExtExt :: Context -> NEM.Map N.Name Variable -> Variable -> Content -> NEM.Map N.Name Variable -> Variable -> Content -> Unify ()
unifyExtExt context partial1 ext1 content partial2 ext2 otherContent =
  do  numOverlap <- loop (NEM.toDict partial1) (NEM.toDict partial2)
      if numOverlap < 0
        then mismatch
        else
          if NEM.size partial1 == numOverlap
          then
            if NEM.size partial2 == numOverlap
            then
              do  subUnify ext1 ext2
                  merge context content
            else
              do  leftover2 <- fresh context (diff partial2 partial1 ext2)
                  subUnify ext1 leftover2
                  merge context otherContent
          else
            if NEM.size partial2 == numOverlap
            then
              do  leftover1 <- fresh context (diff partial1 partial2 ext1)
                  subUnify leftover1 ext2
                  merge context content
            else
              do  ext       <- fresh context flexAny
                  leftover1 <- fresh context (diff partial2 partial1 ext)
                  leftover2 <- fresh context (diff partial1 partial2 ext)
                  subUnify ext1 leftover2
                  subUnify leftover1 ext2
                  merge context (Structure (ExtRec1 order (NEM.union partial1 partial2) ext))
  where
    loop _   Tip = return 0
    loop Tip _   = return 0
    loop (Bin _ k v l r) t2 =
      case Map.splitLookup k t2 of
        (l2, mv2, r2) ->
          case mv2 of
            Nothing ->
              do  x <- loop l l2
                  y <- loop r r2
                  return $! x + y

            Just v2 ->
              do  x <- loop l l2
                  b <- unifyField v v2
                  y <- loop r r2
                  return $! x + y + (if b then 1 else -0x100000000)

    diff p1 p2 ext =
      Structure (ExtRec1 order (NEM.unsafeDiff p1 p2) ext)

    !(F.Order order) = F.unordered



-- FLATTEN RECORD
--
-- TODO this should do some sort of check to detect when extensible records are
-- combined in a way that overlaps field names.


data FlatRecord
  = Record (NEM.Map N.Name Variable)
  | ExtRec (NEM.Map N.Name Variable) Variable


flatten :: NEM.Map N.Name Variable -> Variable -> Unify FlatRecord
flatten fields variable =
  Unify $ \vars ok _ ->
    ok vars =<< flattenHelp fields variable


flattenHelp :: NEM.Map N.Name Variable -> Variable -> IO FlatRecord
flattenHelp fields variable =
  do  (Descriptor content _ _ _) <- UF.get variable
      case content of
        Structure t ->
          case t of
            Empty1         -> return $ Record fields
            Record1 _ fs   -> return $ Record (NEM.union fields fs)
            ExtRec1 _ fs e -> flattenHelp (NEM.union fields fs) e
            App1 _ _ _ _   -> return $ ExtRec fields variable
            Fun1 _ _       -> return $ ExtRec fields variable
            Unit1          -> return $ ExtRec fields variable
            Pair1 _ _      -> return $ ExtRec fields variable
            Triple1 _ _ _  -> return $ ExtRec fields variable

        Alias _ _ _ _ _ v -> flattenHelp fields v -- TODO may be dropping useful alias info here
        Flex  _           -> return $ ExtRec fields variable
        Rigid _           -> return $ ExtRec fields variable
        Error             -> return $ ExtRec fields variable



-- UNIFIER


newtype Unify a =
  Unify (forall r.
    [Variable] -> ([Variable] -> a -> IO r) -> ([Variable] -> () -> IO r) -> IO r
  )


register :: Descriptor -> Unify Variable
register desc =
  Unify $ \vars ok _ ->
    do  var <- UF.fresh desc
        ok (var:vars) var


mismatch :: Unify a
mismatch =
  Unify $ \vars _ err ->
    err vars ()


io :: IO a -> Unify a
io work =
  Unify $ \vars ok _ ->
    ok vars =<< work



-- CONTEXT


data Context =
  Context
    { _first      :: Variable
    , _firstDesc  :: Descriptor
    , _second     :: Variable
    , _secondDesc :: Descriptor
    }


reorient :: Context -> Context
reorient (Context var1 desc1 var2 desc2) =
  Context var2 desc2 var1 desc1


merge :: Context -> Content -> Unify ()
merge (Context var1 (Descriptor _ rank1 _ _) var2 (Descriptor _ rank2 _ _)) content =
  Unify $ \vars ok _ ->
    ok vars =<<
      UF.union var1 var2 (Descriptor content (min rank1 rank2) noMark Nothing)


fresh :: Context -> Content -> Unify Variable
fresh (Context _ (Descriptor _ rank1 _ _) _ (Descriptor _ rank2 _ _)) content =
  register $ Descriptor content (min rank1 rank2) noMark Nothing



-- UNIFY INSTANCES


instance Functor Unify where
  fmap func (Unify k) =
    Unify $ \vars ok err ->
      let
        ok' vs value = ok vs (func value)
      in
      k vars ok' err


instance Applicative Unify where
  pure a =
    Unify $ \vars ok _ ->
      ok vars a

  (<*>) (Unify kF) (Unify kV) =
    Unify $ \vars ok err ->
      let
        okF vs func =
          let
            okV vs' value = ok vs' (func value)
          in
          kV vs okV err
      in
      kF vars okF err

  (*>) (Unify kA) (Unify kB) =
    Unify $ \vars ok err ->
      let
        ok' vs _ = kB vs ok err
      in
      kA vars ok' err


instance Monad Unify where
  (>>=) (Unify kA) callback =
    Unify $ \vars ok err ->
      let
        ok' vars1 a =
          case callback a of
            Unify kB -> kB vars1 ok err
      in
      kA vars ok' err


