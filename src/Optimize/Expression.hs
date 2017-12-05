{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize.Expression
  ( Cycle
  , optimize
  , destructArgs
  , optimizePotentialTailCall
  )
  where


import Prelude hiding (cycle)
import Control.Monad (foldM)
import qualified Data.Set as Set

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
import qualified Optimize.Case as Case
import qualified Optimize.Names as Names
import qualified Reporting.Annotation as A



-- OPTIMIZE


type Cycle =
  Set.Set N.Name


optimize :: Cycle -> Can.Expr -> Names.Tracker Opt.Expr
optimize cycle (A.At region expression) =
  case expression of
    Can.VarLocal name ->
      pure (Opt.VarLocal name)

    Can.VarTopLevel home name ->
      if Set.member name cycle then
        pure (Opt.VarCycle home name)
      else
        Names.registerGlobal home name

    Can.VarKernel home name ->
      Names.registerKernel home (Opt.VarKernel home name)

    Can.VarForeign home name _ ->
      Names.registerGlobal home name

    Can.VarDebug home name _ ->
      Names.registerKernel N.debug (Opt.VarDebug name home region Nothing)

    Can.VarOperator _ home name _ ->
      Names.registerGlobal home name

    Can.Chr chr ->
      pure (Opt.Chr chr)

    Can.Str str ->
      pure (Opt.Str str)

    Can.Int int ->
      pure (Opt.Int int)

    Can.Float float ->
      pure (Opt.Float float)

    Can.List entries ->
      Opt.List <$> traverse (optimize cycle) entries

    Can.Negate expr ->
      do  func <- Names.registerGlobal ModuleName.basics N.negate
          arg <- optimize cycle expr
          pure $ Opt.Call func [arg]

    Can.Binop _ home name _ left right ->
      do  optFunc <- Names.registerGlobal home name
          optLeft <- optimize cycle left
          optRight <- optimize cycle right
          return (Opt.Call optFunc [optLeft, optRight])

    Can.Lambda args body ->
      do  (argNames, defss) <- destructArgs args
          obody <- optimize cycle body
          pure $ Opt.Function argNames (foldr Opt.Let obody (concat defss))

    Can.Call func args ->
      Opt.Call
        <$> optimize cycle func
        <*> traverse (optimize cycle) args

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimize cycle condition
            <*> optimize cycle branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimize cycle finally

    Can.Let def body ->
      optimizeDef cycle def =<< optimize cycle body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCall cycle def
            <*> optimize cycle body

        _ ->
          do  obody <- optimize cycle body
              foldM (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (name, defs) <- destruct pattern
          oexpr <- optimize cycle expr
          obody <- optimize cycle body
          pure $
            Opt.Let (Opt.Def name oexpr) (foldr Opt.Let obody defs)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          do  defs <- destructHelp (Opt.VarLocal root) pattern []
              obranch <- optimize cycle branch
              pure (pattern, foldr Opt.Let obranch defs)
      in
      do  temp <- Names.generate
          oexpr <- optimize cycle expr
          case oexpr of
            Opt.VarLocal root ->
              Case.optimize temp root <$> traverse (optimizeBranch root) branches

            _ ->
              Case.optimize temp temp <$> traverse (optimizeBranch temp) branches

    Can.Accessor field ->
      Names.registerField field (Opt.Accessor field)

    Can.Access record field ->
      do  optRecord <- optimize cycle record
          Names.registerField field (Opt.Access optRecord field)

    Can.Update record updates ->
      Names.registerFieldDict updates Opt.Update
        <*> optimize cycle record
        <*> traverse (optimize cycle) updates

    Can.Record fields ->
      Names.registerFieldDict fields Opt.Record
        <*> traverse (optimize cycle) fields

    Can.Unit ->
      Names.registerKernel N.utils Opt.Unit

    Can.Tuple a b maybeC ->
      Names.registerKernel N.utils Opt.Tuple
        <*> optimize cycle a
        <*> optimize cycle b
        <*> traverse (optimize cycle) maybeC

    Can.Shader _ src _ ->
      pure (Opt.Shader src)



-- DEFINITION


optimizeDef :: Cycle -> Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef cycle def body =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizeDefHelp cycle name args expr body

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizeDefHelp cycle name (map fst typedArgs) expr body


optimizeDefHelp :: Cycle -> N.Name -> [Can.Pattern] -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp cycle name args expr body =
  case args of
    [] ->
      do  oexpr <- optimize cycle expr
          pure $ Opt.Let (Opt.Def name oexpr) body

    _ ->
      do  (argNames, defss) <- destructArgs args
          let oexpr = Opt.Function argNames (foldr Opt.Let body (concat defss))
          pure $ Opt.Let (Opt.Def name oexpr) body



-- DESTRUCTURING


destructArgs :: [Can.Pattern] -> Names.Tracker ([N.Name], [[Opt.Def]])
destructArgs args =
  unzip <$> traverse destruct args


destruct :: Can.Pattern -> Names.Tracker (N.Name, [Opt.Def])
destruct pattern@(A.At _ ptrn) =
  case ptrn of
    Can.PVar name ->
      pure (name, [])

    Can.PAlias subPattern name ->
      do  revDefs <- destructHelp (Opt.VarLocal name) subPattern []
          pure (name, reverse revDefs)

    _ ->
      do  name <- Names.generate
          revDefs <- destructHelp (Opt.VarLocal name) pattern []
          pure (name, reverse revDefs)


destructHelp :: Opt.Expr -> Can.Pattern -> [Opt.Def] -> Names.Tracker [Opt.Def]
destructHelp root (A.At region pattern) revDefs =
  case pattern of
    Can.PAnything ->
      pure revDefs

    Can.PVar name ->
      pure (Opt.Def name root : revDefs)

    Can.PRecord fields ->
      let toDef name = Opt.Def name (Opt.Access root name) in
      Names.registerFieldList fields (map toDef fields ++ revDefs)

    Can.PAlias subPattern name ->
      destructHelp (Opt.VarLocal name) subPattern $
        Opt.Def name root : revDefs

    Can.PUnit ->
      pure revDefs

    Can.PTuple a b maybeC ->
      do  name <- Names.generate
          let tuple = Opt.VarLocal name
          let newRevDefs = Opt.Def name root : revDefs
          case maybeC of
            Nothing ->
              destructHelp (Opt.Index tuple 1) b =<<
                destructHelp (Opt.Index tuple 0) a newRevDefs

            Just c ->
              destructHelp (Opt.Index tuple 2) c =<<
                destructHelp (Opt.Index tuple 1) b =<<
                  destructHelp (Opt.Index tuple 0) a newRevDefs

    Can.PList [] ->
      pure revDefs

    Can.PList (hd:tl) ->
      destructCons root hd (A.At region (Can.PList tl)) revDefs

    Can.PCons hd tl ->
      destructCons root hd tl revDefs

    Can.PChr _ ->
      pure revDefs

    Can.PStr _ ->
      pure revDefs

    Can.PInt _ ->
      pure revDefs

    Can.PCtor _ _ _ _ _ _ args ->
      do  name <- Names.generate
          foldM
            (destructCtorArg (Opt.VarLocal name))
            (Opt.Def name root : revDefs)
            args


destructCons :: Opt.Expr -> Can.Pattern -> Can.Pattern -> [Opt.Def] -> Names.Tracker [Opt.Def]
destructCons root hd tl revDefs =
  do  name <- Names.generate
      let list = Opt.VarLocal name
      destructHelp (Opt.Index list 0) hd =<<
        destructHelp (Opt.Index list 1) tl (Opt.Def name root : revDefs)


destructCtorArg :: Opt.Expr -> [Opt.Def] -> Can.PatternCtorArg -> Names.Tracker [Opt.Def]
destructCtorArg root revDefs (Can.PatternCtorArg index _ arg) =
  destructHelp (Opt.Index root (Index.toZeroBased index)) arg revDefs



-- TAIL CALL


optimizePotentialTailCall :: Cycle -> Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCall cycle def =
  case def of
    Can.Def (A.At _ name) args expr ->
      do  (argNames, defss) <- destructArgs args
          toTailDef name argNames defss <$>
            optimizeTail cycle name argNames expr

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      do  (argNames, defss) <- destructArgs (map fst typedArgs)
          toTailDef name argNames defss <$>
            optimizeTail cycle name argNames expr


optimizeTail :: Cycle -> N.Name -> [N.Name] -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTail cycle rootName argNames locExpr@(A.At _ expression) =
  case expression of
    Can.Call func@(A.At _ (Can.VarTopLevel _ name)) args ->
      do  oargs <- traverse (optimize cycle) args
          if name == rootName
            then
              case Index.indexedZipWith (\_ a b -> (a,b)) argNames oargs of
                Index.LengthMatch pairs ->
                  pure $ Opt.TailCall name pairs

                Index.LengthMismatch _ _ ->
                  do  ofunc <- optimize cycle func
                      pure $ Opt.Call ofunc oargs
            else
              do  ofunc <- optimize cycle func
                  pure $ Opt.Call ofunc oargs

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimize cycle condition
            <*> optimizeTail cycle rootName argNames branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimizeTail cycle rootName argNames finally

    Can.Let def body ->
      optimizeDef cycle def =<< optimizeTail cycle rootName argNames body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCall cycle def
            <*> optimizeTail cycle rootName argNames body

        _ ->
          do  obody <- optimizeTail cycle rootName argNames body
              foldM (\bod def -> optimizeDef cycle def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (dname, defs) <- destruct pattern
          oexpr <- optimize cycle expr
          obody <- optimizeTail cycle dname argNames body
          pure $
            Opt.Let (Opt.Def dname oexpr) (foldr Opt.Let obody defs)

    Can.Case expr branches ->
      let
        optimizeBranch root (Can.CaseBranch pattern branch) =
          do  defs <- destructHelp (Opt.VarLocal root) pattern []
              obranch <- optimizeTail cycle rootName argNames branch
              pure (pattern, foldr Opt.Let obranch defs)
      in
      do  temp <- Names.generate
          oexpr <- optimize cycle expr
          case oexpr of
            Opt.VarLocal root ->
              Case.optimize temp root <$> traverse (optimizeBranch root) branches

            _ ->
              Case.optimize temp temp <$> traverse (optimizeBranch temp) branches

    _ ->
      optimize cycle locExpr



-- DETECT TAIL CALLS


toTailDef :: N.Name -> [N.Name] -> [[Opt.Def]] -> Opt.Expr -> Opt.Def
toTailDef name argNames defss body =
  if hasTailCall body then
    Opt.TailDef name argNames (foldr Opt.Let body (concat defss))
  else
    Opt.Def name (Opt.Function argNames (foldr Opt.Let body (concat defss)))


hasTailCall :: Opt.Expr -> Bool
hasTailCall expression =
  case expression of
    Opt.TailCall _ _ ->
      True

    Opt.If branches finally ->
      hasTailCall finally || any (hasTailCall . snd) branches

    Opt.Let _ body ->
      hasTailCall body

    Opt.Case _ _ _ branches ->
      any (hasTailCall . snd) branches

    _ ->
      False
