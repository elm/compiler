{-# LANGUAGE QuasiQuotes, Rank2Types, TemplateHaskell #-}
module Canonicalize.Module
  ( Result(..)
  , canonicalize
  )
  where


import Prelude hiding (cycle)
import qualified Data.Dups as Dups
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.OneOrMore as OOM

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Prim.Mode as Mode
import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Canonicalize.Decl as Decl
import qualified Canonicalize.Effects as Effects
import qualified Canonicalize.Environment as Env
import qualified Canonicalize.Environment.Foreign as Foreign
import qualified Canonicalize.TypeDecl as TypeDecl
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Canonicalize as E
import qualified Reporting.Warning as W



-- CANONICALIZE


newtype Result w a =
  Result (forall r. w -> (w -> a -> IO r) -> (w -> OOM.OneOrMore E.Error -> IO r) -> IO r)


canonicalize :: Mode.Mode -> Map.Map Module.Name I.Interface -> Src.Module -> Result [W.Warning] Can.Module
canonicalize mode ifaces (Src.Module header docs imports decls infixes) =
  do  let home = ModuleName.Canonical (Mode.toPackage mode) (Src.getName header)
      (Decls values unions aliases ports) <- splitDecls decls
      (exports, style) <- checkHeader home header
      (env1          ) <- initEnv home ifaces imports
      (env2, types   ) <- addTypes unions aliases env1
      (env3, style1  ) <- addEffects mode style values types ports env2
      (env4          ) <- addValues values env3
      canModule env4 exports docs types values infixes style1


initEnv :: ModuleName.Canonical -> Map.Map Module.Name I.Interface -> [Src.Import] -> Result w Env.Env
initEnv home ifaces imports =
  Result $ \warns ok err ->
    case Foreign.initialize home ifaces imports of
      Foreign.Result k ->
        k (ok warns) (err warns)


addTypes :: [A.Located Src.Union] -> [A.Located Src.Alias] -> Env.Env -> Result w (Env.Env, Map.Map T.Name Can.TypeDecl)
addTypes unions aliases env =
  Result $ \warns ok err ->
    case TypeDecl.addTypes unions aliases env of
      TypeDecl.Result k ->
        k (ok warns) (err warns . OOM.map E.Problem)



-- CANONICALIZE MODULE


canModule :: Env.Env -> Can.Exports -> Src.Docs -> Map.Map T.Name Can.TypeDecl -> [Src.Value] -> [A.Located Src.Infix] -> Can.Style -> Result [W.Warning] Can.Module
canModule env@(Env.Env home _ _ _ _ _ _ _) exports docs types values infixes style =
  Can.Module home exports docs
    <$> canValues env values
    <*> pure types
    <*> canBinops infixes
    <*> pure style


canValues :: Env.Env -> [Src.Value] -> Result [W.Warning] Can.Decls
canValues env values =
  Result $ \warns ok err ->
    case Decl.canonicalize env values of
      Decl.Result k ->
          k warns ok (\ws xs -> err ws (OOM.map E.Problem xs))


canBinops :: [A.Located Src.Infix] -> Result w (Map.Map Op.Name Can.Binop)
canBinops infixes =
  case Dups.detect (\_ _ b -> b) E.DuplicateBinop dups of
    Dups.Detector k ->
      k return throws
  where
    dups = List.foldl' add Dups.none infixes

    add dict (A.At _ (Src.Infix (A.At region op) assoc prec name)) =
      Dups.insert op region (Can.Binop_ name assoc prec) dict



-- SPLIT DECLARATIONS
--
-- Declarations are stored in reverse order so that they can be easily collected
-- in this phase.


data Decls =
  Decls
    { _values  :: [Src.Value]
    , _unions  :: [A.Located Src.Union]
    , _aliases :: [A.Located Src.Alias]
    , _ports   :: [Src.Port]
    }


splitDecls :: Src.Decls -> Result w Decls
splitDecls decls0 =
  case decls0 of
    Src.RevDecls d ds -> step [] [] [] [] d ds
    Src.NoDecls  r    -> throw $ E.NoDecls r
  where
    loop vs us as ps decls =
      case decls of
        []   -> pure $ Decls vs us as ps
        d:ds -> step vs us as ps d ds

    step vs us as ps decl decls =
      case decl of
        Src.Value_ _ (A.At _ v) -> loop (v:vs) us as ps decls
        Src.Union_ _ (       u) -> loop vs (u:us) as ps decls
        Src.Alias_ _ (       a) -> loop vs us (a:as) ps decls
        Src.Port_  _ (A.At _ p) -> loop vs us as (p:ps) decls



-- CHECK HEADER
--
-- TODO an error here blocks the rest, but it is possible to continue with a
-- dummy Header value to collect more errors.


checkHeader :: ModuleName.Canonical -> Src.Header -> Result w (Can.Exports, Src.Style)
checkHeader (ModuleName.Canonical _ expected) header =
  case header of
    Src.NoHeader region ->
      throw $ E.ModuleNameUnspecified expected region

    Src.YesHeader style actual exposing ->
      if expected /= A.toValue actual
      then throw $ E.ModuleNameMismatch expected actual
      else pure (toExports exposing, style)


toExports :: A.Located Src.Exposing -> Can.Exports
toExports (A.At region exposing) =
  case exposing of
    Src.Open        -> Can.ExportEverything region
    Src.Explicit es -> loop es Dups.none Dups.none Dups.none
  where
    loop exposeds uppers lowers ops =
      case exposeds of
        [] ->
          Can.Export uppers lowers ops

        e:es ->
          case e of
            Src.Upper (A.At r n) p -> loop es (Dups.insert n r p uppers) lowers ops
            Src.Lower (A.At r n)   -> loop es uppers (Dups.insert n r () lowers) ops
            Src.Operator r n       -> loop es uppers lowers (Dups.insert n r () ops)



-- ADD EFFECTS
--
-- Local unqualified values take precedence, so use union to overwrite foreign values.


addEffects :: Mode.Mode -> Src.Style -> [Src.Value] -> Map.Map T.Name Can.TypeDecl -> [Src.Port] -> Env.Env -> Result w (Env.Env, Can.Style)
addEffects mode style values types ports env =
  case Effects.canonicalize env mode values (Map.mapMaybe toUnion types) ports style of
    Effects.Result k ->
      k (\style1 -> pure (env1, style1)) throws_
  where
    env1 =
      case style of
        Src.Normal _   -> env
        Src.Ports  _   -> add env $ Map.fromList $ List.map toValue ports
        Src.Effect _ m ->
          case m of
            Src.Cmd c   -> add env $ Map.singleton [N.ascii|command|] (Env.TopLevel (A.toRegion c))
            Src.Sub   s -> add env $ Map.singleton [N.ascii|subscription|] (Env.TopLevel (A.toRegion s))
            Src.Fx  c s -> add env $ Map.insert [N.ascii|command|] (Env.TopLevel (A.toRegion c)) $ Map.singleton [N.ascii|subscription|] (Env.TopLevel (A.toRegion s))

    add (Env.Env h vs ts cs bs qvs qts qcs) newValues =
      Env.Env h (Map.union newValues vs) ts cs bs qvs qts qcs

    toValue (Src.Port (A.At r n) _) =
      (n, Env.TopLevel r)

    toUnion decl =
      case decl of
        Can.Union u -> Just u
        Can.Alias _ -> Nothing



-- ADD VALUES
--
-- Local unqualified values take precedence, so use union to overwrite foreign values.


addValues :: [Src.Value] -> Env.Env -> Result w Env.Env
addValues values (Env.Env h vs ts cs bs qvs qts qcs) =
  do  newValues <- gatherValues values
      pure $ Env.Env h (Map.union newValues vs) ts cs bs qvs qts qcs



-- GATHER VALUES


gatherValues :: [Src.Value] -> Result w (Map.Map N.Name Env.Value)
gatherValues values =
  case Dups.detect (\_ r _ -> Env.TopLevel r) E.DuplicateDecl dups of
    Dups.Detector k ->
      k return throws
  where
    dups = List.foldl' add Dups.none values

    add d v@(Src.Value (A.At r n) _ _ _) =
      Dups.insert n r v d



-- RESULT HELPERS


throw :: E.Error -> Result w a
throw x =
  Result $ \warns _ err ->
    err warns (OOM.one x)


throws :: OOM.OneOrMore E.Problem -> Result w a
throws xs =
  Result $ \warns _ err ->
    err warns (OOM.map E.Problem xs)


throws_ :: OOM.OneOrMore E.Error -> Result w a
throws_ xs =
  Result $ \warns _ err -> err warns xs


instance Functor (Result w) where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \warns ok err ->
      let
        ok' w arg = ok w (func arg)
      in
      k warns ok' err


instance Applicative (Result w) where
  {-# INLINE pure #-}
  pure a =
    Result $ \w ok _ -> ok w a

  {-# INLINE (<*>) #-}
  (<*>) (Result kF) (Result kA) =
    Result $ \warns ok err ->
      let
        okF ws func = kA ws (\w arg -> ok w (func arg)) err
        errF ws xs  = kA ws (\w _  -> err w xs) (\w ys -> err w (OOM.more xs ys))
      in
      kF warns okF errF


instance Monad (Result w) where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \warns ok err ->
      let
        okA w a = case callback a of { Result kB -> kB w ok err }
      in
      kA warns okA err
