{-# LANGUAGE NamedFieldPuns, TupleSections #-}
module CheckMatch
    ( checkBody
    , Pat (..)
    , CanonicalPat
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Except (throwError)
import Control.Monad (liftM)
import Control.Monad (forM)

import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Elm.Utils ((|>))
import qualified AST.Expression.General as General
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Pattern as Pattern
import AST.Pattern (CanonicalPattern)
import qualified Reporting.Annotation as Annotation
import qualified Reporting.Region as Region
import qualified AST.Variable as Var
import qualified Reporting.Error.CheckMatch as Error
import Reporting.Warning.CheckMatch

type M =
    Either (Annotation.Located Error.Error)


fromPattern :: CanonicalPattern -> CanonicalPat
fromPattern (Annotation.A _region p) =
    case p of
      Pattern.Data ctor ps ->
          Data ctor (map fromPattern ps)

      Pattern.Record fs ->
          Record fs

      Pattern.Alias s p ->
          Alias s (fromPattern p)

      Pattern.Var s ->
          Var s

      Pattern.Anything ->
          Anything

      Pattern.Literal l ->
          Literal l


checkBody
  :: Module.Interfaces
  -> Module.CanonicalBody
  -> M [Annotation.Located Warning]
checkBody interfaces =
    checkExpr . Module.program
    where
    checkExpr :: Canonical.Expr -> M [Annotation.Located Warning]
    checkExpr (Annotation.A r e) =
        case e of
          General.ExplicitList es  ->
            concatMapM checkExpr es

          General.Binop _ e e' ->
              (++) <$> checkExpr e <*> checkExpr e'

          General.Lambda p e ->
              maybeCons <$> checkMatch r [p] <*> checkExpr e

          General.App e e' ->
              (++) <$> checkExpr e <*> checkExpr e'

          General.MultiIf branches ->
              concatMapM (\(b,e) -> (++) <$> checkExpr b <*> checkExpr e) branches

          General.Let defs e ->
              concat <$> sequence [ps, rhssResults, checkExpr e]
              where
              ps          = mapMaybeM (\(Canonical.Definition p _ _) -> checkMatch r [p]) defs
              rhssResults = concatMapM (\(Canonical.Definition _ e _) -> checkExpr e) defs

          General.Case e branches ->
              do  eResults      <- checkExpr e
                  result        <- checkMatch r ps
                  branchResults <- concatMapM (checkExpr . snd) branches
                  return (eResults ++ maybeCons result branchResults)
                  where ps = map fst branches

          General.Data _ctor es ->
              concatMapM checkExpr es

          General.Access e _f ->
              checkExpr e

          General.Remove e _f ->
              checkExpr e

          General.Insert e _f e' ->
              (++) <$> checkExpr e <*> checkExpr e'

          General.Modify e _ ->
              checkExpr e

          General.Record fields ->
              concatMapM (checkExpr . snd) fields

          _ ->
              return []


    ctorToAdt :: Var.Canonical -> Maybe (Module.AdtInfo String)
    ctorToAdt =
        \(Var.Canonical {Var.home,Var.name}) -> Map.lookup name =<< Map.lookup home m
        where
        -- Maps home to ctor name to the adt to which that ctor belongs
        m :: Map.Map Var.Home (Map.Map String (Module.AdtInfo String))
        m = builtins `Map.union` fromModules where
            -- TODO
            builtins = Map.empty

            fromModules = Map.mapKeysMonotonic Var.Module (Map.map Module.iAdts interfaces)


    ctorToAdtErr :: Region.Region -> Var.Canonical -> M (Module.AdtInfo String)
    ctorToAdtErr region c =
        case ctorToAdt c of
          Nothing ->
              throwError (Annotation.A region (Error.ConstructorTypeNotFound c))

          Just adt ->
              return adt


    complement :: Region.Region -> CanonicalPat -> M [CanonicalPat]
    complement region p =
        case p of
          Data ctor ps ->
              do  let ctorName = Var.name ctor
                      ctorHome = Var.home ctor
                  (_tyVars, ctors) <- ctorToAdtErr region ctor
                  -- A pattern is in the complement of this one if it uses one of the
                  -- other constructors of this type...
                  let otherCtorPat (ctor', args) =
                          Data (Var.Canonical {Var.name = ctor', Var.home = ctorHome})
                            (replicate (length args) Anything)

                      otherCtorPatterns =
                          filter (\(ctor', _) -> ctor' /= ctorName) ctors
                          |> map otherCtorPat

                      -- or if it uses this constructor and one of its argument patterns is
                      -- in the complement of an argument pattern here.
                      numArgs        = length ps
                      sandwich pat i = replicate i Anything ++ pat : replicate (numArgs - i - 1) Anything

                  argComplement <- fmap concat . forM (zip [0..] ps) $ \(i, argP) ->
                      map (\argP' -> Data ctor (sandwich argP' i)) <$> (complement region argP)

                  return (otherCtorPatterns ++ argComplement)

          Record _fs ->
              return []

          Alias _s p ->
              complement region p

          Var _s ->
              return []

          Anything ->
              return []

          Literal l ->
              return [AnythingBut (Set.singleton l)]

          AnythingBut ls ->
              return (map Literal (Set.toList ls))

    intersection :: CanonicalPat -> CanonicalPat -> Maybe CanonicalPat
    intersection p1 p2 =
        case (p1, p2) of
          (Alias _ p1', _) ->
              intersection p1' p2

          (_, Alias _ p2') ->
              intersection p1 p2'

          (Anything, _) ->
              Just p2

          (Var _, _) ->
              Just p2

          (_, Anything) ->
              Just p1

          (_, Var _) ->
              Just p1

          (Data ctor1 args1, Data ctor2 args2) ->
              if ctor1 /= ctor2
              then Nothing
              else fmap (Data ctor1) (sequence (zipWith intersection args1 args2))

          -- TODO Assuming the fields are identical.
          (Record _fs, Record _) ->
              Just p1

          (Literal l1, Literal l2) ->
              if l1 == l2 then Just p1 else Nothing

          (AnythingBut ls, Literal l) ->
              if l `Set.member` ls then Nothing else Just p2

          (Literal l, AnythingBut ls) ->
              if l `Set.member` ls then Nothing else Just p1

          (AnythingBut ls1, AnythingBut ls2) ->
              Just (AnythingBut (Set.union ls1 ls2))

          _ -> Nothing

    checkMatch :: Region.Region -> [CanonicalPattern] -> M (Maybe (Annotation.Located Warning))
    checkMatch region =
        go [Anything]
        where
        go :: [CanonicalPat] -> [CanonicalPattern] -> M (Maybe (Annotation.Located Warning))
        go unhandled ps =
            case (unhandled, ps) of
              ([], []) ->
                  return Nothing

              (_:_, []) ->
                  return (Just (Annotation.A region (Inexhaustive unhandled)))

              (_, p:ps') ->
                  let pp                    = fromPattern p
                      Annotation.A region _ = p
                  in
                  if all (\q -> intersection q pp == Nothing) unhandled
                  then return (Just (Annotation.A region (Redundant p)))
                  else
                    do  pComp <- complement region pp
                        go (concatMap (\q -> mapMaybe (intersection q) pComp) unhandled) ps'

-- Util
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f =
    liftM concat . mapM f


mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f =
    liftM catMaybes . mapM f


maybeCons :: Maybe a -> [a] -> [a]
maybeCons =
    maybe id (:)

