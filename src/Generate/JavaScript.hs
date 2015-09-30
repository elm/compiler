module Generate.JavaScript (generate) where

import Control.Monad.State as State
import qualified Data.List as List
import Language.ECMAScript3.PrettyPrint as ES
import Language.ECMAScript3.Syntax
import qualified Text.PrettyPrint.Leijen as PP

import qualified AST.Expression.Optimized as Optimized
import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Variable
import qualified Generate.JavaScript.Expression as JS
import Generate.JavaScript.Helpers (call, function, localRuntime, obj, prop, ref, varDecl)
import qualified Generate.JavaScript.Variable as Var


generate :: Module.Optimized -> String
generate module_ =
  let
    (ModuleName.Canonical _ name) =
      Module.name module_

    body =
      concat
        [ useStrict
        , checkForCachedVersion name
        , imports (Module.imports module_)
        , definitions (Module.program (Module.body module_))
        , exports name (Module.exports module_)
        ]

    make =
      "Elm" : name ++ ["make"]

    javascript =
      buildIntermediateObjects make
      ++
      [ assign make (function [localRuntime] body)
      ]
  in
    PP.displayS (PP.renderPretty 0.4 160 (ES.prettyPrint javascript)) ""



-- INTRO


useStrict :: [Statement ()]
useStrict =
  [ ExprStmt () (StringLit () "use strict")
  ]


checkForCachedVersion :: ModuleName.Raw -> [Statement ()]
checkForCachedVersion name =
  let
    values =
      localRuntime : name ++ ["values"]

    valuesObject =
      obj values
  in
    buildIntermediateObjects values
    ++
    [ IfSingleStmt () valuesObject (ReturnStmt () (Just valuesObject))
    ]


buildIntermediateObjects :: [String] -> [Statement ()]
buildIntermediateObjects path =
  let
    create name =
      assign name (InfixExpr () OpLOr (obj name) (ObjectLit () []))

    paths =
      drop 2 (init (List.inits path))
  in
    map create paths


assign :: [String] -> Expression () -> Statement ()
assign path expr =
  case path of
    [x] ->
        VarDeclStmt () [ varDecl x expr ]

    _ ->
        ExprStmt () $
            AssignExpr () OpAssign (LDot () (obj (init path)) (last path)) expr



-- IMPORTS


imports :: [ModuleName.Raw] -> [Statement ()]
imports rawImports =
  case rawImports of
    [] ->
        []

    _ ->
        [ VarDeclStmt () (map jsImport rawImports)
        ]


jsImport :: ModuleName.Raw -> VarDecl ()
jsImport name =
  let
    make =
      obj ("Elm" : name ++ ["make"])
  in
    varDecl (Var.modulePrefix name) (call make [ref localRuntime])



-- DEFINITIONS


definitions :: [Optimized.Def] -> [Statement ()]
definitions defs =
  let
    setup =
      Var.define "_op" (ObjectLit () [])
  in
    setup : State.evalState (mapM JS.generateDef defs) 0



-- EXPORTS


exports :: ModuleName.Raw -> [Variable.Value] -> [Statement ()]
exports name exportedValues =
  let
    exportedFields =
      map toField ("_op" : concatMap extract exportedValues)

    toField x =
      (prop x, ref x)

    extract value =
        case value of
          Variable.Alias _ ->
              []

          Variable.Value x ->
              if Help.isOp x then
                  []

              else
                  [Var.safe x]

          Variable.Union _ (Variable.Listing ctors _) ->
              map Var.safe ctors

    values =
      LDot () (obj (localRuntime : name)) "values"
  in
    [ ReturnStmt () $ Just $
        AssignExpr () OpAssign values (ObjectLit () exportedFields)
    ]
