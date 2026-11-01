{-# LANGUAGE BangPatterns, MagicHash, Rank2Types, TemplateHaskell, UnboxedTuples #-}
module Type.Check
  ( Outcome(..)
  , check
  )
  where


import Data.Foldable (foldrM)
import qualified Data.Dups as Dups
import qualified Data.Map as Map
import qualified Data.Map.Utils as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OOM

import qualified Graph

import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified AST.Typed as Tpd
import qualified AST.Prim.Name as N
import qualified AST.Prim.Operator as Op
import qualified AST.Prim.TypeName as T
import qualified Elm.Interface as I
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Type as Error
import qualified Reporting.Error.Nitpick as E
import qualified Type.Constrain.Module as TC
import qualified Type.Solve as TS
import qualified Type.Type as Type



-- CHECK
--
-- This module does a few checks and transformations:
--
--   1. Run type inference
--   2. Validate exports and create module interface
--
-- So it is a bit more than just type checking, but it seems like the sensible
-- time to do all of these steps nonetheless!


type InferredTypes =
  Map.Map N.Name Type.Variable


data Outcome
  = Ok (I.Interface, Tpd.Module)
  | BadType (NE.List Error.Error)
  | BadNitpick (NE.List E.Error)


check :: Can.Module -> IO Outcome
check modul@(Can.Module home exports0 docs decls types binops style) =
  do  result <- TS.run =<< TC.constrain modul
      case result of
        Left xs ->
          return $ BadType xs

        Right inferred ->
          run Ok BadNitpick $
            do  exports <- checkExports exports0
                iface   <- toInterface inferred home exports decls types binops
                return (iface, Tpd.Module home exports docs decls types binops style)



-- CHECK EXPORTS
--
-- Detect duplicates in the explicitly exposed values and typess.


checkExports :: Can.Exports -> Result Tpd.Exports
checkExports exports =
  case exports of
    Can.ExportEverything r ->
      return $ Tpd.ExportEverything r

    Can.Export ts vs bs ->
      case
        Tpd.Export
          <$> Dups.detect (\_ r p  -> (r,p)) E.ExportDuplicate_Type ts
          <*> Dups.detect (\_ r () ->  r   ) E.ExportDuplicate_Var  vs
          <*> Dups.detect (\_ r () ->  r   ) E.ExportDuplicate_Op   bs
      of
        Dups.Detector k ->
          k return throws



--------------------------------------------------------------------------------
-- TO INTERFACE ----------------------------------------------------------------
--------------------------------------------------------------------------------


toInterface :: InferredTypes -> ModuleName.Canonical -> Tpd.Exports -> Can.Decls -> Map.Map T.Name Can.TypeDecl -> Map.Map Op.Name Can.Binop -> Result I.Interface
toInterface inferred (ModuleName.Canonical pkg _) exports (Can.Decls (Graph.Sorted decls)) types binops =
  do  values <- io $ Map.unions <$> traverse (toIValue inferred) decls
      case exports of
        Tpd.ExportEverything _ ->
          do  let ts = Map.map  toITypeDecl      types
              let bs = Map.map (toIBinop values) binops
              return $ I.Interface pkg values ts bs

        Tpd.Export ts vs bs ->
          I.Interface pkg
            <$> exportValues vs values
            <*> exportTypes  ts types
            <*> exportBinops bs binops values



-- TO INTERFACE CONTENT


toIValue :: InferredTypes -> Can.Decl -> IO (Map.Map N.Name I.Annotation)
toIValue inferred decl =
  case decl of
    Can.Decl    d    -> add d Map.empty
    Can.DeclRec d ds -> add d =<< foldrM add Map.empty ds
  where
    add def dict =
      case def of
        Can.DValue_ (A.At _ n)     _     -> get n >>= \a -> pure $ Map.insert n a dict
        Can.DValue  (A.At _ n)     _ _ a ->                 pure $ Map.insert n a dict
        Can.DFunc_  (A.At _ n) _ _ _     -> get n >>= \a -> pure $ Map.insert n a dict
        Can.DFunc   (A.At _ n) _ _ _ _ a ->                 pure $ Map.insert n a dict

    get n =
      Type.toAnnotation $ $(Map.require 'toIValue) n inferred N.toChars


toITypeDecl :: Can.TypeDecl -> I.TypeDecl
toITypeDecl tipe =
  case tipe of
    Can.Alias a -> I.AliasPublic a
    Can.Union u -> I.UnionOpen u


toIBinop :: Map.Map N.Name I.Annotation -> Can.Binop -> I.Binop
toIBinop values (Can.Binop_ name assoc prec) =
  let
    !anno = $(Map.require 'toIBinop) name values N.toChars
  in
  I.Binop name anno assoc prec



-- EXPORT FOR INTERFACE


exportValues :: Map.Map N.Name A.Region -> Map.Map N.Name I.Annotation -> Result (Map.Map N.Name I.Annotation)
exportValues lowers values =
    Map.mergeA (Map.traverseMissing goLeft) Map.dropMissing (Map.zipWithMatched goBoth) lowers values
  where
    goLeft n r   = throw $ E.ExportNotFound_Var r n (Map.keys values)
    goBoth _ _ v = v


exportTypes :: Map.Map T.Name (A.Region, Src.Privacy) -> Map.Map T.Name Can.TypeDecl -> Result (Map.Map T.Name I.TypeDecl)
exportTypes uppers types =
    Map.mergeA (Map.traverseMissing goLeft) (Map.mapMissing goRight) (Map.zipWithAMatched goBoth) uppers types
  where
    goLeft name (region,_) =
      throw $ E.ExportNotFound_Type region name (Map.keys types)

    goRight _ decl =
      case decl of
        Can.Union u -> I.UnionPrivate u
        Can.Alias a -> I.AliasPrivate a

    goBoth name (_,privacy) decl =
      case privacy of
        Src.Public region ->
          case decl of
            Can.Union u -> return $ I.UnionOpen u
            Can.Alias _ -> throw  $ E.ExportOpenAlias region name

        Src.Private ->
          case decl of
            Can.Union u -> return $ I.UnionClosed u
            Can.Alias a -> return $ I.AliasPublic a


exportBinops :: Map.Map Op.Name A.Region -> Map.Map Op.Name Can.Binop -> Map.Map N.Name I.Annotation -> Result (Map.Map Op.Name I.Binop)
exportBinops lowers binops values =
    Map.mergeA (Map.traverseMissing goLeft) Map.dropMissing (Map.zipWithMatched goBoth) lowers binops
  where
    goLeft n r =
      throw $ E.ExportNotFound_Op r n (Map.keys binops)

    goBoth _ _ (Can.Binop_ name assoc prec) =
      let
        !anno = $(Map.require 'exportBinops) name values N.toChars
      in
      I.Binop name anno assoc prec



-- RESULT


newtype Result a =
  Result (forall r. (a -> IO r) -> (OOM.OneOrMore E.Error -> IO r) -> IO r)


run :: (a -> r) -> (NE.List E.Error -> r) -> Result a -> IO r
run ok err (Result k) =
  k
    (return . ok)
    (return . err . OOM.destruct NE.List)


io :: IO a -> Result a
io work =
  Result $ \ok _ ->
    ok =<< work


throw :: E.Error -> Result a
throw x =
  Result $ \_ err ->
    err (OOM.one x)


throws :: OOM.OneOrMore E.Error -> Result a
throws xs =
  Result $ \_ err ->
    err xs


instance Functor Result where
  {-# INLINE fmap #-}
  fmap func (Result k) =
    Result $ \ok err ->
      k (ok . func) err


instance Applicative Result where
  {-# INLINE pure #-}
  pure a =
    Result $ \ok _ ->
      ok a

  {-# INLINE (<*>) #-}
  (<*>) (Result kFunc) (Result kArg) =
    Result $ \ok err ->
      let
        okF func = kArg (ok . func) err
        errF xs = kArg (\_ -> err xs) (\ys -> err (OOM.more xs ys))
      in
      kFunc okF errF


instance Monad Result where
  {-# INLINE (>>=) #-}
  (>>=) (Result kA) callback =
    Result $ \ok err ->
      let
        okA a =
          case callback a of
            Result kB -> kB ok err
      in
      kA okA err

