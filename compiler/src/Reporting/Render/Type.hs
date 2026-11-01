{-# LANGUAGE MagicHash, OverloadedStrings, TemplateHaskell #-}
module Reporting.Render.Type
  ( Localizer
  , emptyLocalizer
  , fromModuleNames
  , fromSourceModule
  --
  , Type(..)
  , Ext(..)
  --
  , Context(..)
  , errToDoc
  , srcToDoc
  , iToDoc
  --
  , Problem(..)
  , toDiffDocs
  --
  , isBool
  , isInt
  , isFloat
  , isString
  , isMaybe
  , isList
  --
  , iteratedDealias
  --
  , lambda
  , apply
  , tuple
  , Field(..)
  , record
  , vrecordSnippet
  , vrecord
  , record1
  , extrec1
  )
  where


import qualified Data.Bag as Bag
import qualified Data.Map as Map
import qualified Data.NonEmptyMap as NEM
import qualified Data.Set as Set

import qualified Array as A

import qualified AST.Source as Src
import qualified AST.Prim.Fields as F
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.TypeIndex as T
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified AST.Prim.TypeVar as T
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc (Doc, (<+>))



-- LOCALIZER
--
-- PERF should I use IntMap for the Localizer?


newtype Localizer =
  Localizer (Map.Map Module.Name Import)


data Import =
  Import
    { _alias :: Maybe Module.Prefix
    , _exposing :: Exposing
    }


data Exposing
  = All
  | Only (Set.Set T.Name)


emptyLocalizer :: Localizer
emptyLocalizer =
  Localizer Map.empty


fromModuleNames :: Map.Map Module.Name a -> Localizer
fromModuleNames names =
  Localizer $ Map.map (\_ -> Import Nothing All) names


fromSourceModule :: Src.Module -> Localizer
fromSourceModule (Src.Module header _ imports _ _) =
  Localizer $ Map.fromList $
    (Src.getName header, Import Nothing All) : map toPair imports
  where
    toPair (Src.Import (A.At _ home) alias exposing) =
      ( home, Import alias (toExposing exposing) )

    toExposing exposing =
      case exposing of
        Src.Open        -> All
        Src.Explicit xs -> Only (foldr addType Set.empty xs)

    addType exposed types =
      case exposed of
        Src.Lower _               -> types
        Src.Upper (A.At _ name) _ -> Set.insert name types
        Src.Operator _ _          -> types



-- ERROR TYPES


data Type
  = Lambda Type Type [Type]
  | Infinite
  | Error
  | Flex  T.Var
  | Rigid T.Var
  | Type  ModuleName.Canonical T.Name M.Mask [Type]
  | Alias ModuleName.Canonical T.Name M.Mask [Type] Type
  | Unit
  | Pair   Type Type
  | Triple Type Type Type
  | Empty
  | Record (F.Fields Type)
  | ExtRec (F.Fields Type) Ext


data Ext
  = ExtFlex  T.Var
  | ExtRigid T.Var



--------------------------------------------------------------------------------
-- TO DOC ----------------------------------------------------------------------
--------------------------------------------------------------------------------



-- ERROR TYPE TO DOC


data Context
  = None
  | Func
  | App


errToDoc :: Localizer -> Context -> Type -> Doc
errToDoc localizer =
    go
  where
    go ctx tipe =
      case tipe of
        Lambda a b cs    -> lambda ctx (go Func a) (go Func b) (map (go Func) cs)
        Infinite         -> "∞"
        Error            -> "?"
        Flex  v          -> D.fromVar v
        Rigid v          -> D.fromVar v
        Type  h n _ xs   -> apply ctx (localName localizer h n) (map (go App) xs)
        Alias h n _ xs _ -> apply ctx (localName localizer h n) (map (go App) xs)
        Unit             -> "()"
        Pair   a b       -> pair   (go None a) (go None b)
        Triple a b c     -> triple (go None a) (go None b) (go None c)
        Empty            -> "{}"
        Record fs        -> record goField fs
        ExtRec fs e      -> extrec goField fs (goExt e)

    goField k t =
      Field k (go None t)

    goExt ext =
      case ext of
        ExtFlex  v -> D.fromVar v
        ExtRigid v -> D.fromVar v



-- SOURCE TYPE TO DOC


srcToDoc :: Context -> Src.Type -> Doc
srcToDoc ctx (A.At _ tipe) =
  case tipe of
    Src.TLambda     x y zs -> lambda ctx (srcToDoc Func x) (srcToDoc Func y) (map (srcToDoc Func) zs)
    Src.TVar        x      -> D.fromVar x
    Src.TType     _   n xs -> apply ctx (D.fromType n) (map (srcToDoc App) xs)
    Src.TTypeQual _ h n xs -> apply ctx (goQual h n)   (map (srcToDoc App) xs)
    Src.TUnit              -> "()"
    Src.TTuple      a b cs -> tuple (srcToDoc None a) (srcToDoc None b) (map (srcToDoc None) cs)
    Src.TEmpty             -> "{}"
    Src.TRecord     f fs   -> record1 (goField f) (map goField fs)
    Src.TExtRec     f fs e -> extrec1 (goField f) (map goField fs) (D.fromVar (A.toValue e))
  where
    goField (n,t) =
      Field (A.toValue n) (srcToDoc None t)

    goQual h n =
      D.fromPrefix h <> "." <> D.fromType n



-- CANONICAL TYPE TO DOC


iToDoc :: Localizer -> Context -> I.Annotation -> Doc
iToDoc localizer context (I.Forall vars tipe) =
    go context tipe
  where
    go ctx t =
      case t of
        I.TLambda  x y zs   -> lambda ctx (go Func x) (go Func y) (map (go Func) zs)
        I.TVar     i        -> goVar i
        I.TType  h n _ xs   -> apply ctx (localName localizer h n) (A.toListWith (go App) xs)
        I.TAlias h n _ xs _ -> apply ctx (localName localizer h n) (A.toListWith (go App) xs)
        I.TUnit             -> "()"
        I.TPair   a b       -> pair   (go None a) (go None b)
        I.TTriple a b c     -> triple (go None a) (go None b) (go None c)
        I.TEmpty            -> "{}"
        I.TRecord fs        -> record goField fs
        I.TExtRec fs e      -> extrec goField fs (goVar e)

    goVar i =
      D.fromVar (T.get i vars)

    goField n t =
      Field n (go None t)



-- TO LOCAL NAME


localName :: Localizer -> ModuleName.Canonical -> T.Name -> Doc
localName localizer (ModuleName.Canonical _ home) name =
  case toPrefix localizer home name of
    PNone -> D.fromType name
    PFull -> D.fromModule home <> "." <> D.fromType name
    PAs h -> D.fromPrefix h  <> "." <> D.fromType name


data Prefix
  = PNone
  | PFull
  | PAs Module.Prefix


toPrefix :: Localizer -> Module.Name -> T.Name -> Prefix
toPrefix (Localizer dict) home name =
  case Map.lookup home dict of
    Nothing ->
      PFull

    Just (Import alias exposing) ->
      case exposing of
        All      -> PNone
        Only set ->
          if Set.member name set
          then PNone
          else
            case alias of
              Nothing -> PFull
              Just h  -> PAs h



-- LOCAL NAME EQUALITY


isLocalOverlap :: Localizer -> ModuleName.Canonical -> T.Name -> ModuleName.Canonical -> T.Name -> Bool
isLocalOverlap localizer (ModuleName.Canonical _ home1) name1 (ModuleName.Canonical _ home2) name2 =
  name1 == name2
  &&
  case toPrefix localizer home1 name1 of
    PNone ->
      case toPrefix localizer home2 name2 of
        PNone -> True
        PFull -> False
        PAs _ -> False

    PFull ->
      case toPrefix localizer home2 name2 of
        PNone  -> False
        PFull  -> home1 == home2
        PAs p2 -> Module.toChars home1 == Module.prefixToChars p2

    PAs p1 ->
      case toPrefix localizer home2 name2 of
        PNone  -> False
        PFull  -> Module.prefixToChars p1 == Module.toChars home2
        PAs p2 -> p1 == p2



--------------------------------------------------------------------------------
-- DIFF ------------------------------------------------------------------------
--------------------------------------------------------------------------------



-- DIFF


data Diff a =
  Diff a a Status


data Status
  = Same
  | Similar
  | Different (Bag.Bag Problem)


data Problem
  = IntFloat
  | StringFromInt
  | StringFromFloat
  | StringToInt
  | StringToFloat
  | AnythingToBool
  | AnythingFromMaybe
  --
  | ArityMismatch Int Int
  --
  | HaveFlex  T.Var Type
  | HaveRigid T.Var Type
  --
  | WantFlex  Type T.Var
  | WantRigid Type T.Var
  --
  | HaveExtFlex T.Var
  | WantExtFlex T.Var
  --
  | HaveExtRigid T.Var
  | WantExtRigid T.Var
  | BothExtRigid T.Var T.Var
  --
  | RecordMismatch [N.Name] [N.Name]


instance Functor Diff where
  fmap func (Diff a b status) =
    Diff (func a) (func b) status


instance Applicative Diff where
  pure a =
    Diff a a Same

  (<*>) (Diff aFunc bFunc status1) (Diff aArg bArg status2) =
    Diff (aFunc aArg) (bFunc bArg) (merge status1 status2)


merge :: Status -> Status -> Status
merge status1 status2 =
  case status1 of
    Same ->
      status2

    Similar ->
      case status2 of
        Same        -> Similar
        Similar     -> Similar
        Different _ -> status2

    Different p1 ->
      case status2 of
        Same         -> status1
        Similar      -> status1
        Different p2 -> Different (Bag.append p1 p2)



-- TO DIFF


toDiffDocs :: Localizer -> Type -> Type -> (D.Doc, D.Doc, [Problem])
toDiffDocs localizer tipe1 tipe2 =
  let
    (Diff doc1 doc2 status) = toDiff localizer None tipe1 tipe2
  in
  case status of
    Same         -> (doc1, doc2, [])
    Similar      -> (doc1, doc2, [])
    Different ps -> (doc1, doc2, Bag.toList ps)


toDiff :: Localizer -> Context -> Type -> Type -> Diff D.Doc
toDiff localizer ctx tipe1 tipe2 =
  case (tipe1, tipe2) of
    (Unit    , Unit    ) -> same localizer ctx tipe1
    (Error   , Error   ) -> same localizer ctx tipe1
    (Infinite, Infinite) -> same localizer ctx tipe1

    (Flex  x, Flex  y) | x == y -> same localizer ctx tipe1
    (Rigid x, Rigid y) | x == y -> same localizer ctx tipe1

    (Lambda a b cs, Lambda x y zs) ->
      if length cs == length zs then
        lambda ctx
          <$> toDiff localizer Func a x
          <*> toDiff localizer Func b y
          <*> sequenceA (zipWith (toDiff localizer Func) cs zs)
      else
        let f = errToDoc localizer Func in
        different
          (D.dullyellow (lambda ctx (f a) (f b) (map f cs)))
          (D.dullyellow (lambda ctx (f x) (f y) (map f zs)))
          (Bag.one (ArityMismatch (2 + length cs) (2 + length zs)))

    (Pair a b, Pair x y) ->
      tuple
        <$> toDiff localizer None a x
        <*> toDiff localizer None b y
        <*> pure []

    (Triple a b c, Triple x y z) ->
      tuple
        <$> toDiff localizer None a x
        <*> toDiff localizer None b y
        <*> ((:[]) <$> toDiff localizer None c z)

    (Record f1   , Record f2   ) -> diffRecord localizer f1    f2
    (ExtRec f1 e1, Record f2   ) -> diffExtRec localizer f1 e1 f2
    (Record f1   , ExtRec f2 e2) -> diffRecExt localizer f1    f2 e2
    (ExtRec f1 e1, ExtRec f2 e2) -> diffExtExt localizer f1 e1 f2 e2

    (Type h1 n1 _ xs1, Type h2 n2 _ xs2) | h1 == h2 && n1 == n2 ->
      apply ctx (localName localizer h1 n1)
        <$> sequenceA (zipWith (toDiff localizer App) xs1 xs2)

    (Alias h1 n1 _ xs1 _, Alias h2 n2 _ xs2 _) | h1 == h2 && n1 == n2 ->
      apply ctx (localName localizer h1 n1)
        <$> sequenceA (zipWith (toDiff localizer App) xs1 xs2)

    -- start trying to find specific problems

    (Flex x, t) | flexUnifies x t -> similar localizer ctx tipe1 tipe2
    (t, Flex x) | flexUnifies x t -> similar localizer ctx tipe1 tipe2

    (Type h1 n1 _ xs1, Type h2 n2 _ xs2) | isLocalOverlap localizer h1 n1 h2 n2 ->
      different
        (nameClashToDoc ctx localizer h1 n1 xs1)
        (nameClashToDoc ctx localizer h2 n2 xs2)
        Bag.empty

    (Type h n _ [t1], t2) | isMaybe h n && isSimilar (toDiff localizer ctx t1 t2) ->
      different
        (apply ctx (D.dullyellow (localName localizer h n)) [errToDoc localizer App t1])
        (errToDoc localizer ctx t2)
        (Bag.one AnythingFromMaybe)

    (t1, Type h n _ [t2]) | isList h n && isSimilar (toDiff localizer ctx t1 t2) ->
      different
        (errToDoc localizer ctx t1)
        (apply ctx (D.dullyellow (localName localizer h n)) [errToDoc localizer App t2])
        Bag.empty

    (Alias h1 n1 _ xs1 t1, t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just diff -> diff
        Nothing ->
          case t2 of
            Type h2 n2 _ xs2 | isLocalOverlap localizer h1 n1 h2 n2 ->
              different
                (nameClashToDoc ctx localizer h1 n1 xs1)
                (nameClashToDoc ctx localizer h2 n2 xs2)
                Bag.empty

            _ ->
              different
                (D.dullyellow (errToDoc localizer ctx tipe1))
                (D.dullyellow (errToDoc localizer ctx tipe2))
                Bag.empty

    (t1, Alias h2 n2 _ xs2 t2) ->
      case diffAliasedRecord localizer t1 t2 of
        Just diff -> diff
        Nothing ->
          case t1 of
            Type h1 n1 _ xs1 | isLocalOverlap localizer h1 n1 h2 n2 ->
              different
                (nameClashToDoc ctx localizer h1 n1 xs1)
                (nameClashToDoc ctx localizer h2 n2 xs2)
                Bag.empty

            _ ->
              different
                (D.dullyellow (errToDoc localizer ctx tipe1))
                (D.dullyellow (errToDoc localizer ctx tipe2))
                Bag.empty

    (Rigid x, other) -> mkDiff $ Bag.one $ HaveRigid x other
    (Flex  x, other) -> mkDiff $ Bag.one $ HaveFlex  x other
    (other, Rigid x) -> mkDiff $ Bag.one $ WantRigid other x
    (other, Flex  x) -> mkDiff $ Bag.one $ WantFlex  other x

    (Type h1 n1 _ [], Type h2 n2 _ [])
      | isInt    h1 n1 && isFloat  h2 n2 -> mkDiff $ Bag.one IntFloat
      | isFloat  h1 n1 && isInt    h2 n2 -> mkDiff $ Bag.one IntFloat
      | isInt    h1 n1 && isString h2 n2 -> mkDiff $ Bag.one StringFromInt
      | isFloat  h1 n1 && isString h2 n2 -> mkDiff $ Bag.one StringFromFloat
      | isString h1 n1 && isInt    h2 n2 -> mkDiff $ Bag.one StringToInt
      | isString h1 n1 && isFloat  h2 n2 -> mkDiff $ Bag.one StringToFloat
      |                   isBool   h2 n2 -> mkDiff $ Bag.one AnythingToBool

    (_, _) ->
      mkDiff $ Bag.empty
  where
    mkDiff problems =
      let
        doc1 = D.dullyellow (errToDoc localizer ctx tipe1)
        doc2 = D.dullyellow (errToDoc localizer ctx tipe2)
      in
      different doc1 doc2 problems



-- DIFF HELPERS


same :: Localizer -> Context -> Type -> Diff D.Doc
same localizer ctx tipe =
  let
    doc = errToDoc localizer ctx tipe
  in
  Diff doc doc Same


similar :: Localizer -> Context -> Type -> Type -> Diff D.Doc
similar localizer ctx t1 t2 =
  Diff (errToDoc localizer ctx t1) (errToDoc localizer ctx t2) Similar


different :: a -> a -> Bag.Bag Problem -> Diff a
different a b problems =
  Diff a b (Different problems)


isSimilar :: Diff a -> Bool
isSimilar (Diff _ _ status) =
  case status of
    Same        -> True
    Similar     -> True
    Different _ -> False



-- IS TYPE?


isBool   :: ModuleName.Canonical -> T.Name -> Bool
isInt    :: ModuleName.Canonical -> T.Name -> Bool
isFloat  :: ModuleName.Canonical -> T.Name -> Bool
isString :: ModuleName.Canonical -> T.Name -> Bool
isMaybe  :: ModuleName.Canonical -> T.Name -> Bool
isList   :: ModuleName.Canonical -> T.Name -> Bool

isBool   h n = h == ModuleName.basics && n == T.bool
isInt    h n = h == ModuleName.basics && n == T.int
isFloat  h n = h == ModuleName.basics && n == T.float
isString h n = h == ModuleName.string && n == T.string
isMaybe  h n = h == ModuleName.maybe  && n == T.maybe
isList   h n = h == ModuleName.list   && n == T.list



-- FLEX UNIFIES


flexUnifies :: T.Var -> Type -> Bool
flexUnifies (T.Var _ con) tipe =
  case con of
    T.Any        -> True
    T.Comparable -> crawl M.isComparable tipe
    T.Appendable -> appendable True tipe
    T.CompAppend -> appendable False tipe
    T.Number     -> crawl M.isNumber tipe
  where
    crawl isGoodMask =
        go
      where
        go typ =
          case typ of
            Lambda _ _ _     -> isGoodMask M.lambda
            Infinite         -> False
            Error            -> False
            Flex  v          -> flexWithFlex con v
            Rigid v          -> rigidWithFlex v con
            Type  _ _ m xs   -> isGoodMask m && all go xs
            Alias _ _ m xs _ -> isGoodMask m && all go xs
            Unit             -> isGoodMask M.unit
            Pair   a b       -> isGoodMask M.pair   && go a && go b
            Triple a b c     -> isGoodMask M.triple && go a && go b && go c
            Empty            -> isGoodMask M.empty
            Record fs        -> isGoodMask M.record && F.all go fs
            ExtRec fs e      -> isGoodMask M.record && F.all go fs && goExt e

        goExt ext =
          case ext of
            ExtFlex  v -> flexWithFlex con v
            ExtRigid v -> rigidWithFlex v con

    appendable notComp typ =
      case typ of
        Lambda _ _ _    -> False
        Infinite        -> False
        Error           -> False
        Flex  v         -> flexWithFlex con v
        Rigid v         -> rigidWithFlex v con
        Type  h n _ xs  -> isAppendable h n && (notComp || all (crawl M.isComparable) xs)
        Alias _ _ _ _ t -> appendable notComp t
        Unit            -> False
        Pair   _ _      -> False
        Triple _ _ _    -> False
        Empty           -> False
        Record _        -> False
        ExtRec _ _      -> False


flexWithFlex :: T.Constraint -> T.Var -> Bool
flexWithFlex con1 (T.Var _ con2) =
  T.unifyFlexWithFlex con1 con2 True True True True False


rigidWithFlex :: T.Var -> T.Constraint -> Bool
rigidWithFlex (T.Var _ rigidCon) flexCon =
  T.unifyRigidWithFlex rigidCon flexCon True False


isAppendable :: ModuleName.Canonical -> T.Name -> Bool
isAppendable h n =
  (h == ModuleName.string && n == T.string)
  ||
  (h == ModuleName.list && n == T.list)



-- NAME CLASH


nameClashToDoc :: Context -> Localizer -> ModuleName.Canonical -> T.Name -> [Type] -> D.Doc
nameClashToDoc ctx localizer (ModuleName.Canonical _ home) name args =
  apply ctx
    (D.yellow (D.fromModule home) <> D.dullyellow ("." <> D.fromType name))
    (map (errToDoc localizer App) args)



-- DIFF ALIASED RECORD
--
-- TODO add a hint about the alias name, maybe put it in the words before each type?


diffAliasedRecord :: Localizer -> Type -> Type -> Maybe (Diff D.Doc)
diffAliasedRecord localizer t1 t2 =
  case (iteratedDealias t1, iteratedDealias t2) of
    (Record f1   , Record f2   ) -> Just $ diffRecord localizer f1    f2
    (ExtRec f1 e1, Record f2   ) -> Just $ diffExtRec localizer f1 e1 f2
    (Record f1   , ExtRec f2 e2) -> Just $ diffRecExt localizer f1    f2 e2
    (ExtRec f1 e1, ExtRec f2 e2) -> Just $ diffExtExt localizer f1 e1 f2 e2
    _                            -> Nothing



-- RECORD DIFFS


diffRecord :: Localizer -> F.Fields Type -> F.Fields Type -> Diff Doc
diffRecord localizer (F.Fields order1 dict1) (F.Fields order2 dict2) =
  let
    size1 = NEM.size dict1
    size2 = NEM.size dict2
  in
  if size1 == size2 && size1 == NEM.intersectionSize dict1 dict2
  then
    record' (\_ f -> f) (F.bestOrder order1 order2) <$>
      $(NEM.unsafeOverlapWithKeyA 'diffRecord) (diffField localizer) dict1 dict2
  else
    let
      diffs = NEM.intersectWithKey (diffField localizer) dict1 dict2
      only1 = Map.keys $ NEM.withoutMap dict1 diffs
      only2 = Map.keys $ NEM.withoutMap dict2 diffs
      bag   = Bag.one (RecordMismatch only1 only2)

      overlay order dict overlap =
        record' (\_ f -> f) order $
          NEM.diffWithKey (toFBold localizer) (\_ _ docs -> docs) dict overlap
    in
    Diff (overlay order1 dict1) (overlay order2 dict2) (Different bag)
      <*> sequenceA diffs


diffField :: Localizer -> N.Name -> Type -> Type -> Diff Field
diffField localizer n t1 t2 =
  let
    (Diff d1 d2 status) = toDiff localizer None t1 t2
  in
  case status of
    Same        -> Diff (FFade n d1) (FFade n d2) status
    Similar     -> Diff (Field n d1) (Field n d2) status
    Different _ -> Diff (Field n d1) (Field n d2) status


toFBold :: Localizer -> N.Name -> Type -> Field
toFBold localizer n t =
  FBold n (errToDoc localizer None t)



-- EXT REC DIFFS


diffExtRec :: Localizer -> F.Fields Type -> Ext -> F.Fields Type -> Diff Doc
diffExtRec localizer (F.Fields order1 dict1) ext1 fields2@(F.Fields order2 dict2) =
  case ext1 of
    ExtFlex x ->
      let
        (extDoc, extStatus) =
          if flexUnifies x (Record fields2)
          then (               D.fromVar x , Similar                             )
          else ( D.dullyellow (D.fromVar x), Different $ Bag.one $ HaveExtFlex x )

        mkExt1 overlap =
          extrec' (\_ f -> f) order1
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict1 overlap)
            extDoc

        mkRec2 overlap =
          record' (\_ f -> f) order2
            (NEM.diffWithKey (toFFade localizer) (\_ _ f -> f) dict2 overlap)
      in
      Diff mkExt1 mkRec2 extStatus
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)

    ExtRigid x ->
      let
        extStatus =
          Different $ Bag.one $ HaveExtRigid x

        mkExt1 overlap =
          extrec' (\_ f -> f) order1
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict1 overlap)
            (D.dullyellow (D.fromVar x))

        mkRec2 overlap =
          record' (\_ f -> f) order2
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict2 overlap)
      in
      Diff mkExt1 mkRec2 extStatus
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)



-- REC EXT DIFFS


diffRecExt :: Localizer -> F.Fields Type -> F.Fields Type -> Ext -> Diff Doc
diffRecExt localizer fields1@(F.Fields order1 dict1) (F.Fields order2 dict2) ext2 =
  case ext2 of
    ExtFlex x ->
      let
        (extDoc, extStatus) =
          if flexUnifies x (Record fields1)
          then (               D.fromVar x , Similar                             )
          else ( D.dullyellow (D.fromVar x), Different $ Bag.one $ WantExtFlex x )

        mkRec1 overlap =
          record' (\_ f -> f) order1
            (NEM.diffWithKey (toFFade localizer) (\_ _ f -> f) dict1 overlap)

        mkExt2 overlap =
          extrec' (\_ f -> f) order2
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict2 overlap)
            extDoc
      in
      Diff mkRec1 mkExt2 extStatus
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)

    ExtRigid x ->
      let
        extStatus =
          Different $ Bag.one $ WantExtRigid x

        mkRec1 overlap =
          record' (\_ f -> f) order1
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict1 overlap)

        mkExt2 overlap =
          extrec' (\_ f -> f) order2
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict2 overlap)
            (D.dullyellow (D.fromVar x))
      in
      Diff mkRec1 mkExt2 extStatus
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)



-- EXT EXT DIFFS


diffExtExt :: Localizer -> F.Fields Type -> Ext -> F.Fields Type -> Ext -> Diff Doc
diffExtExt localizer (F.Fields order1 dict1) ext1 (F.Fields order2 dict2) ext2 =
  case (ext1, ext2) of
    (ExtFlex x@(T.Var _ con), ExtFlex y) ->
      let
        overlay order dict overlap =
          extrec' (\_ f -> f) order $
            NEM.diffWithKey (toFFade localizer) (\_ _ f -> f) dict overlap
      in
      Diff (overlay order1 dict1) (overlay order2 dict2) Same
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)
        <*>
          if flexWithFlex con y
          then Diff               (D.fromVar x)                (D.fromVar y)  (if x == y then Same else Similar)
          else Diff (D.dullyellow (D.fromVar x)) (D.dullyellow (D.fromVar y)) (Different Bag.empty)

    (ExtFlex x, ExtRigid y) ->
      let
        mkFlex1 overlap =
          extrec' (\_ f -> f) order1
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict1 overlap)
            (D.fromVar x)

        mkRigid2 overlap =
          extrec' (\_ f -> f) order2
            (NEM.diffWithKey (toFFade localizer) (\_ _ f -> f) dict2 overlap)
            (D.dullyellow (D.fromVar y))
      in
      Diff mkFlex1 mkRigid2 (Different (Bag.one (WantExtRigid y)))
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)

    (ExtRigid x, ExtFlex y) ->
      let
        mkRigid1 overlap =
          extrec' (\_ f -> f) order1
            (NEM.diffWithKey (toFFade localizer) (\_ _ f -> f) dict1 overlap)
            (D.dullyellow (D.fromVar x))

        mkFlex2 overlap =
          extrec' (\_ f -> f) order2
            (NEM.diffWithKey (toFBold localizer) (\_ _ f -> f) dict2 overlap)
            (D.fromVar y)
      in
      Diff mkRigid1 mkFlex2 (Different (Bag.one (HaveExtRigid x)))
        <*> sequenceA (NEM.intersectWithKey (diffField localizer) dict1 dict2)

    (ExtRigid x, ExtRigid y) ->
      let
        size1 = NEM.size dict1
        size2 = NEM.size dict2
      in
      if size1 == size2 && size1 == NEM.intersectionSize dict1 dict2
      then
        extrec' (\_ f -> f) (F.bestOrder order1 order2)
          <$> $(NEM.unsafeOverlapWithKeyA 'diffExtExt) (diffField localizer) dict1 dict2
          <*>
            if x == y
            then Diff (D.fromVar x) (D.fromVar y) Same
            else Diff (D.dullyellow (D.fromVar x)) (D.dullyellow (D.fromVar y)) (Different (Bag.one (BothExtRigid x y)))
      else
        let
          diffs = NEM.intersectWithKey (diffField localizer) dict1 dict2
          only1 = Map.keys $ NEM.withoutMap dict1 diffs
          only2 = Map.keys $ NEM.withoutMap dict2 diffs
          bag   =
            if null only1 && null only2
            then Bag.empty
            else Bag.one (RecordMismatch only1 only2)

          overlay order dict overlap ext =
            extrec' (\_ f -> f) order
              (NEM.diffWithKey (toFBold localizer) (\_ _ docs -> docs) dict overlap)
              ext
        in
        Diff (overlay order1 dict1) (overlay order2 dict2) (Different bag)
          <*> sequenceA diffs
          <*>
            if x == y
            then Diff (D.fromVar x) (D.fromVar y) Same
            else Diff (D.dullyellow (D.fromVar x)) (D.dullyellow (D.fromVar y)) (Different (Bag.one (BothExtRigid x y)))


toFFade :: Localizer -> N.Name -> Type -> Field
toFFade localizer n t =
  FFade n (errToDoc localizer None t)



-- ITERATED DEALIAS


iteratedDealias :: Type -> Type
iteratedDealias tipe =
  case tipe of
    Lambda _ _ _    -> tipe
    Infinite        -> tipe
    Error           -> tipe
    Flex  _         -> tipe
    Rigid _         -> tipe
    Type  _ _ _ _   -> tipe
    Alias _ _ _ _ t -> iteratedDealias t
    Unit            -> tipe
    Pair   _ _      -> tipe
    Triple _ _ _    -> tipe
    Empty           -> tipe
    Record _        -> tipe
    ExtRec _ _      -> tipe



--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------



-- TO DOC


lambda :: Context -> Doc -> Doc -> [Doc] -> Doc
lambda ctx arg1 arg2 args =
  let
    lambdaDoc =
      D.align $ D.sep (arg1 : map ("->" <+>) (arg2:args))
  in
  case ctx of
    None -> lambdaDoc
    Func -> D.cat [ "(", lambdaDoc, ")" ]
    App  -> D.cat [ "(", lambdaDoc, ")" ]


apply :: Context -> Doc -> [Doc] -> Doc
apply ctx name args =
  case args of
    [] ->
      name

    _:_ ->
      let
        applyDoc =
          D.hang 4 (D.sep (name : args))
      in
      case ctx of
        App  -> D.cat [ "(", applyDoc, ")" ]
        Func -> applyDoc
        None -> applyDoc



-- TUPLES


pair :: Doc -> Doc -> Doc
pair a b =
  tuple a b []


triple :: Doc -> Doc -> Doc -> Doc
triple a b c =
  tuple a b [c]


tuple :: Doc -> Doc -> [Doc] -> Doc
tuple a b cs =
  let
    entries =
      zipWith (<+>) ("(" : repeat ",") (a:b:cs)
  in
  D.align $ D.sep [ D.cat entries, ")" ]



-- RECORDS


data Field
  = Field N.Name Doc
  | FFade N.Name Doc
  | FBold N.Name Doc


record :: (N.Name -> a -> Field) -> F.Fields a -> Doc
record func fields =
    record1 f fs
  where
    (f,fs) = F.flattenWith func fields


record' :: (N.Name -> a -> Field) -> F.Order# -> NEM.Map N.Name a -> Doc
record' func order dict =
  record func (F.Fields order dict)


record1 :: Field -> [Field] -> Doc
record1 field fields =
  D.align $ D.sep $
    [ D.cat (fieldToDoc '{' field : map (fieldToDoc ',') fields)
    , "}"
    ]


fieldToDoc :: Char -> Field -> Doc
fieldToDoc sep field =
  D.fromChar sep <+> D.hang 4 (
    case field of
      Field n t -> D.sep [ D.fromName n <+> ":", t ]
      FFade n t -> D.dullwhite (D.sep [ D.fromName n <+> ":", t ])
      FBold n t -> D.sep [ D.dullyellow (D.fromName n) <+> ":", t ]
  )



-- EXT REC


extrec :: (N.Name -> a -> Field) -> F.Fields a -> Doc -> Doc
extrec func fields ext =
    extrec1 f fs ext
  where
    (f,fs) = F.flattenWith func fields


extrec' :: (N.Name -> a -> Field) -> F.Order# -> NEM.Map N.Name a -> Doc -> Doc
extrec' func order dict ext =
  extrec func (F.Fields order dict) ext


extrec1 :: Field -> [Field] -> Doc -> Doc
extrec1 field fields ext =
  D.align $ D.sep $
    [ D.hang 4 $ D.sep $
        [ "{" <+> ext
        , D.cat (fieldToDoc '|' field : map (fieldToDoc ',') fields)
        ]
    , "}"
    ]



-- VERTICAL RECORDS


vrecord :: [(Doc, Doc)] -> Maybe Doc -> Doc
vrecord entries maybeExt =
  case (map vfieldToDoc entries, maybeExt) of
    ([], Nothing) ->
      "{}"

    (fields, Nothing) ->
      D.vcat $
        zipWith (<+>) ("{" : repeat ",") fields ++ ["}"]

    (fields, Just ext) ->
      D.vcat
        [ D.hang 4 $ D.vcat $
            [ "{" <+> ext
            , D.cat (zipWith (<+>) ("|" : repeat ",") fields)
            ]
        , "}"
        ]


vrecordSnippet :: (Doc, Doc) -> [(Doc, Doc)] -> Doc
vrecordSnippet entry entries =
  let
    field  = "{" <+> vfieldToDoc entry
    fields = zipWith (<+>) (repeat ",") (map vfieldToDoc entries ++ ["..."])
  in
  D.vcat (field : fields ++ ["}"])


vfieldToDoc :: (Doc, Doc) -> Doc
vfieldToDoc (fieldName, fieldType) =
  D.hang 4 (D.sep [ fieldName <+> ":", fieldType ])

