module Canonicalize.Environment
  ( Env(..)
  --
  , Value(..)
  , ForeignValue(..)
  --
  , Type(..)
  , UnionInfo(..)
  , AliasInfo(..)
  , Arity(..)
  , UnionCtor(..)
  , AliasCtor(..)
  , Binop(..)
  --
  , mergeValues
  , mergeTypes
  --
  , withType
  , withTypeQual
  , withCtor
  , withCtorQual
  --
  , toPossibleNames
  )
  where


import qualified Data.Map.Strict as Map

import qualified AST.Canonical as Can
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.Tag as Tag
import qualified AST.Prim.TypeMask as M
import qualified AST.Prim.TypeName as T
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Ambiguous as Amb
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E



-- ENVIRONMENT


data Env =
  Env
    { _home    :: ModuleName.Canonical
    , _vars    :: Map.Map N.Name Value
    , _types   :: Map.Map T.Name (Type UnionInfo AliasInfo)
    , _ctors   :: Map.Map N.Name (Type UnionCtor AliasCtor)
    , _binops  :: Map.Map Op.Name Binop
    , _q_vars  :: Map.Map Module.Prefix (Map.Map N.Name ForeignValue)
    , _q_types :: Map.Map Module.Prefix (Map.Map T.Name (Type UnionInfo AliasInfo))
    , _q_ctors :: Map.Map Module.Prefix (Map.Map N.Name (Type UnionCtor AliasCtor))
    }



-- VALUES


data Value
  = Local A.Region
  | TopLevel A.Region
  | Foreign ModuleName.Canonical I.Annotation
  | Foreigns (Amb.Ambiguous Module.Name)


data ForeignValue
  = ForeignValue ModuleName.Canonical I.Annotation
  | ForeignValues (Amb.Ambiguous Module.Name)



-- TYPES


data Type u a
  = Union ModuleName.Canonical u
  | Alias ModuleName.Canonical a
  | Types (Amb.Ambiguous Module.Name)


data UnionInfo = UnionInfo M.Mask Arity
data AliasInfo = AliasInfo M.Mask Arity I.Aliased

newtype Arity = Arity Int



-- CTORS


data UnionCtor
  = Variant I.Annotation Can.PType Tag.Tag Int [I.Type]
  | Enum    I.Annotation Can.PType Tag.Tag
  | Vector  I.Annotation Can.PType Int I.Type I.Type [I.Type]
  | Box     I.Annotation Can.PType I.Type
  | Void    I.Annotation Can.PType


data AliasCtor =
  Record I.Annotation N.Name [N.Name]



-- BINOPS


data Binop =
  Binop
    { _op_home  :: ModuleName.Canonical
    , _op_name  :: N.Name
    , _op_type  :: I.Annotation
    , _op_assoc :: Op.Associativity
    , _op_prec  :: Op.Precedence
    }



-- MERGE


mergeValues :: ForeignValue -> ForeignValue -> ForeignValue
mergeValues value1 value2 =
  case value1 of
    ForeignValue h1 _ ->
      case value2 of
        ForeignValue h2 _  -> if h1 == h2 then value1 else ForeignValues $ Amb.init (go h1) (go h2)
        ForeignValues amb2 -> ForeignValues $ Amb.add (go h1) amb2

    ForeignValues amb1 ->
      case value2 of
        ForeignValue h2 _  -> ForeignValues $ Amb.add (go h2) amb1
        ForeignValues amb2 -> ForeignValues $ Amb.merge amb1 amb2
  where
    go (ModuleName.Canonical _ h) = h


mergeTypes :: Type u a -> Type u a -> Type u a
mergeTypes tipe1 tipe2 =
  case tipe1 of
    Union h1 _ ->
      case tipe2 of
        Union h2 _ -> if h1 == h2 then tipe1 else Types $ Amb.init (go h1) (go h2)
        Alias h2 _ -> Types $ Amb.init (go h1) (go h2)
        Types amb2 -> Types $ Amb.add  (go h1) amb2

    Alias h1 _ ->
      case tipe2 of
        Alias h2 _ -> if h1 == h2 then tipe1 else Types $ Amb.init (go h1) (go h2)
        Union h2 _ -> Types $ Amb.init (go h1) (go h2)
        Types amb2 -> Types $ Amb.add  (go h1) amb2

    Types amb1 ->
      case tipe2 of
        Union h2 _ -> Types $ Amb.add (go h2) amb1
        Alias h2 _ -> Types $ Amb.add (go h2) amb1
        Types amb2 -> Types $ Amb.merge amb1 amb2
  where
    go (ModuleName.Canonical _ h) = h



-- WITH TYPE


type OkUI r = ModuleName.Canonical -> UnionInfo -> r
type OkAI r = ModuleName.Canonical -> AliasInfo -> r


withType :: Env -> A.Region -> T.Name -> OkUI r -> OkAI r -> (E.Problem -> r) -> r
withType (Env _ _ ts _ _ _ qts _) region name okU okA err =
  case Map.lookup name ts of
    Just info ->
      case info of
        Union s i -> okU s i
        Alias s i -> okA s i
        Types amb -> err $ E.AmbiguousType region Nothing name amb

    Nothing ->
      err $ E.NotFoundType region Nothing name (toPossibleNames ts qts)


withTypeQual :: Env -> A.Region -> Module.Prefix -> T.Name -> OkUI r -> OkAI r -> (E.Problem -> r) -> r
withTypeQual (Env _ _ ts _ _ _ qts _) region prefix name okU okA err =
  case Map.lookup name =<< Map.lookup prefix qts of
    Just info ->
      case info of
        Union s i -> okU s i
        Alias s i -> okA s i
        Types amb -> err $ E.AmbiguousType region (Just prefix) name amb

    Nothing ->
      err $ E.NotFoundType region (Just prefix) name (toPossibleNames ts qts)



-- WITH CTOR


type OkUC r = ModuleName.Canonical -> UnionCtor -> r
type OkAC r = ModuleName.Canonical -> AliasCtor -> r


withCtor :: Env -> A.Region -> N.Name -> OkUC r -> OkAC r -> (E.Problem -> r) -> r
withCtor (Env _ _ _ cs _ _ _ qcs) region name okU okA err =
  case Map.lookup name cs of
    Just info ->
      case info of
        Union h c -> okU h c
        Alias h c -> okA h c
        Types amb -> err $ E.AmbiguousCtor region Nothing name amb

    Nothing ->
      err $ E.NotFoundCtor region Nothing name (toPossibleNames cs qcs)


withCtorQual :: Env -> A.Region -> Module.Prefix -> N.Name -> OkUC r -> OkAC r -> (E.Problem -> r) -> r
withCtorQual (Env _ _ _ cs _ _ _ qcs) region prefix name okU okA err =
  case Map.lookup name =<< Map.lookup prefix qcs of
    Just info ->
      case info of
        Union h c -> okU h c
        Alias h c -> okA h c
        Types amb -> err $ E.AmbiguousCtor region (Just prefix) name amb

    Nothing ->
      err $ E.NotFoundCtor region (Just prefix) name (toPossibleNames cs qcs)



-- TO POSSIBLE NAMES


toPossibleNames :: Map.Map name a -> Map.Map Module.Prefix (Map.Map name b) -> E.PossibleNames name
toPossibleNames exposed qualified =
  E.PossibleNames (Map.keysSet exposed) (Map.map Map.keysSet qualified)

