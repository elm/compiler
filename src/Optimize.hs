{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Optimize
  ( optimizeExpr
  )
  where


import Control.Monad (foldM)

import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Module.Name as ModuleName
import qualified Data.Index as Index
import qualified Elm.Name as N
--import qualified Optimize.Case as Case
import qualified Optimize.DecisionTree as DT
import qualified Optimize.Names as Names
--import qualified Optimize.Inline as Inline
--import qualified Optimize.Port as Port
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R



-- OPTIMIZE



-- EXPRESSIONS


optimizeExpr :: Can.Expr -> Names.Tracker Opt.Expr
optimizeExpr (A.At region expression) =
  case expression of
    Can.VarLocal name ->
      pure (Opt.VarLocal name)

    Can.VarTopLevel home name ->
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
      Opt.List <$> traverse optimizeExpr entries

    Can.Negate expr ->
      do  func <- Names.registerGlobal ModuleName.basics N.negate
          arg <- optimizeExpr expr
          pure $ Opt.Call func [arg]

    Can.Binop _ home name _ left right ->
      do  optFunc <- Names.registerGlobal home name
          optLeft <- optimizeExpr left
          optRight <- optimizeExpr right
          return (Opt.Call optFunc [optLeft, optRight])

    Can.Lambda args body ->
      do  (argNames, defss) <- destructArgs args
          obody <- optimizeExpr body
          pure $
            Opt.Function argNames (foldr Opt.Let obody (concat defss))

    Can.Call func args ->
      Opt.Call
        <$> optimizeExpr func
        <*> traverse optimizeExpr args

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimizeExpr condition
            <*> optimizeExpr branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimizeExpr finally

    Can.Let def body ->
      optimizeDef def =<< optimizeExpr body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCall def
            <*> optimizeExpr body

        _ ->
          do  obody <- optimizeExpr body
              foldM (\bod def -> optimizeDef def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (name, defs) <- destruct pattern
          oexpr <- optimizeExpr expr
          obody <- optimizeExpr body
          pure $
            Opt.Let (Opt.Def name oexpr) (foldr Opt.Let obody defs)

    Can.Case expr branches ->
      error "TODO"

    Can.Accessor field ->
      pure (Opt.Accessor field)

    Can.Access record field ->
      do  optRecord <- optimizeExpr record
          pure (Opt.Access optRecord field)

    Can.Update record updates ->
      Opt.Update
        <$> optimizeExpr record
        <*> traverse optimizeExpr updates

    Can.Record fields ->
      Opt.Record <$> traverse optimizeExpr fields

    Can.Unit ->
      Names.registerKernel N.utils Opt.Unit

    Can.Tuple a b maybeC ->
      Names.registerKernel N.utils Opt.Tuple
        <*> optimizeExpr a
        <*> optimizeExpr b
        <*> traverse optimizeExpr maybeC

    Can.Shader _ src _ ->
      pure (Opt.Shader src)



-- DEFINITION


optimizeDef :: Can.Def -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDef def body =
  case def of
    Can.Def (A.At _ name) args expr ->
      optimizeDefHelp name args expr body

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      optimizeDefHelp name (map fst typedArgs) expr body


optimizeDefHelp :: N.Name -> [Can.Pattern] -> Can.Expr -> Opt.Expr -> Names.Tracker Opt.Expr
optimizeDefHelp name args expr body =
  case args of
    [] ->
      do  oexpr <- optimizeExpr expr
          pure $ Opt.Let (Opt.Def name oexpr) body

    _ ->
      do  (argNames, defss) <- destructArgs args
          let func = Opt.Function argNames (foldr Opt.Let body (concat defss))
          pure $
            Opt.Let (Opt.Def name func) body



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
      let
        toDef field =
          Opt.Def field (Opt.Access root field)
      in
      pure (map toDef fields ++ revDefs)

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
              destructHelp (Opt.CtorAccess tuple Index.second) b =<<
                destructHelp (Opt.CtorAccess tuple Index.first) a newRevDefs

            Just c ->
              destructHelp (Opt.CtorAccess tuple Index.third) c =<<
                destructHelp (Opt.CtorAccess tuple Index.second) b =<<
                  destructHelp (Opt.CtorAccess tuple Index.first) a newRevDefs

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

    Can.PCtor _ _ _ _ _ args ->
      do  name <- Names.generate
          foldM
            (destructCtorArg (Opt.VarLocal name))
            (Opt.Def name root : revDefs)
            args


destructCons :: Opt.Expr -> Can.Pattern -> Can.Pattern -> [Opt.Def] -> Names.Tracker [Opt.Def]
destructCons root hd tl revDefs =
  do  name <- Names.generate
      let list = Opt.VarLocal name
      destructHelp (Opt.CtorAccess list Index.first) hd =<<
        destructHelp (Opt.CtorAccess list Index.second) tl (Opt.Def name root : revDefs)


destructCtorArg :: Opt.Expr -> [Opt.Def] -> Can.PatternCtorArg -> Names.Tracker [Opt.Def]
destructCtorArg root revDefs (Can.PatternCtorArg index _ arg) =
  destructHelp (Opt.CtorAccess root index) arg revDefs



-- TAIL CALL


optimizePotentialTailCall :: Can.Def -> Names.Tracker Opt.Def
optimizePotentialTailCall def =
  case def of
    Can.Def (A.At _ name) args expr ->
      do  (argNames, defss) <- destructArgs args
          toTailDef name argNames defss <$>
            optimizeTailExpr name argNames expr

    Can.TypedDef (A.At _ name) _ typedArgs expr _ ->
      do  (argNames, defss) <- destructArgs (map fst typedArgs)
          toTailDef name argNames defss <$>
            optimizeTailExpr name argNames expr


optimizeTailExpr :: N.Name -> [N.Name] -> Can.Expr -> Names.Tracker Opt.Expr
optimizeTailExpr name argNames locExpr@(A.At _ expression) =
  case expression of
    Can.Call (A.At _ (Can.VarTopLevel _ funcName)) args ->
      if funcName == name && length args == length argNames then
        Opt.TailCall name argNames <$> traverse optimizeExpr args
      else
        optimizeExpr locExpr

    Can.If branches finally ->
      let
        optimizeBranch (condition, branch) =
          (,)
            <$> optimizeExpr condition
            <*> optimizeTailExpr name argNames branch
      in
      Opt.If
        <$> traverse optimizeBranch branches
        <*> optimizeTailExpr name argNames finally

    Can.Let def body ->
      optimizeDef def =<< optimizeTailExpr name argNames body

    Can.LetRec defs body ->
      case defs of
        [def] ->
          Opt.Let
            <$> optimizePotentialTailCall def
            <*> optimizeTailExpr name argNames body

        _ ->
          do  obody <- optimizeTailExpr name argNames body
              foldM (\bod def -> optimizeDef def bod) obody defs

    Can.LetDestruct pattern expr body ->
      do  (dname, defs) <- destruct pattern
          oexpr <- optimizeExpr expr
          obody <- optimizeTailExpr dname argNames body
          pure $
            Opt.Let (Opt.Def dname oexpr) (foldr Opt.Let obody defs)

    Can.Case expr branches ->
      error "TODO"

    _ ->
      optimizeExpr locExpr



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
    Opt.TailCall _ _ _ ->
      True

    Opt.If branches finally ->
      hasTailCall finally || any (hasTailCall . snd) branches

    Opt.Let _ body ->
      hasTailCall body

    Opt.Case _ _ branches ->
      any (hasTailCall . snd) branches

    _ ->
      False

