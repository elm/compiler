{-# OPTIONS_GHC -Wall #-}
module Generate.JavaScript.Variable
    ( fresh
    , canonical
    , qualified
    , native
    , coreNative
    , staticProgram
    , define
    , safe
    )
    where

import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Helpers as Help
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import qualified Elm.Package as Pkg
import qualified Generate.JavaScript.Helpers as JS



-- FRESH NAMES


fresh :: State.State Int String
fresh =
  do  n <- State.get
      State.modify (+1)
      return ("_v" ++ show n)



-- DEF NAMES


define :: Maybe ModuleName.Canonical -> String -> JS.Expression () -> [JS.Statement ()]
define maybeHome name body =
  if not (Help.isOp name) then
    let
      jsName =
        maybe unqualified qualified maybeHome name
    in
      [ JS.VarDeclStmt () [ JS.varDecl jsName body ] ]

  else
    case maybeHome of
      Nothing ->
        error "can only define infix operators at the top level"

      Just home ->
        let
          opsDictName =
            getOpsDictName (Var.TopLevel home)

          lvalue =
            JS.LBracket () (JS.ref opsDictName) (JS.StringLit () name)
        in
          [ JS.VarDeclStmt () [ JS.varDecl opsDictName (JS.refOrObject opsDictName) ]
          , JS.ExprStmt () (JS.AssignExpr () JS.OpAssign lvalue body)
          ]



-- INSTANTIATE VARIABLES


canonical :: Var.Canonical -> JS.Expression ()
canonical (Var.Canonical home name) =
  if Help.isOp name then
    JS.BracketRef () (JS.ref (getOpsDictName home)) (JS.StringLit () name)

  else
    case home of
      Var.Local ->
        JS.ref (unqualified name)

      Var.BuiltIn ->
        JS.ref (unqualified name)

      Var.Module moduleName@(ModuleName.Canonical _ rawName) ->
        if ModuleName.isNative rawName then
          native moduleName name

        else
          JS.ref (qualified moduleName name)

      Var.TopLevel moduleName ->
        JS.ref (qualified moduleName name)


unqualified :: String -> String
unqualified =
  safe


qualified :: ModuleName.Canonical -> String -> String
qualified moduleName name =
  moduleToString moduleName ++ "$" ++ removePrimes name


native :: ModuleName.Canonical -> String -> JS.Expression ()
native moduleName name =
  JS.obj [ moduleToString moduleName, name ]


coreNative :: String -> String -> JS.Expression ()
coreNative moduleName name =
  native (ModuleName.inCore ["Native",moduleName]) name


staticProgram :: JS.Expression ()
staticProgram =
  native (ModuleName.inVirtualDom ["Native","VirtualDom"]) "staticProgram"


getOpsDictName :: Var.Home -> String
getOpsDictName home =
  let
    moduleName =
      case home of
        Var.Local -> error "infix operators should only be defined in top-level declarations"
        Var.BuiltIn -> error "there should be no built-in infix operators"
        Var.Module name -> name
        Var.TopLevel name -> name
  in
    moduleToString moduleName ++ "_ops"


moduleToString :: ModuleName.Canonical -> String
moduleToString (ModuleName.Canonical (Pkg.Name user project) moduleName) =
  let
    safeUser =
      map (swap '-' '_') user

    safeProject =
      map (swap '-' '_') project

    safeModuleName =
      List.intercalate "_" moduleName
  in
    '_' : safeUser ++ "$" ++ safeProject ++ "$" ++ safeModuleName


swap :: Char -> Char -> Char -> Char
swap from to c =
  if c == from then to else c



-- SAFE NAMES


safe :: String -> String
safe name =
  removePrimes $
    if Set.member name jsReserveds then '$' : name else name


removePrimes :: String -> String
removePrimes name =
  map (swap '\'' '$') name


jsReserveds :: Set.Set String
jsReserveds =
  Set.fromList
    -- JS reserved words
    [ "null", "undefined", "Nan", "Infinity", "true", "false", "eval"
    , "arguments", "int", "byte", "char", "goto", "long", "final", "float"
    , "short", "double", "native", "throws", "boolean", "abstract", "volatile"
    , "transient", "synchronized", "function", "break", "case", "catch"
    , "continue", "debugger", "default", "delete", "do", "else", "finally"
    , "for", "function", "if", "in", "instanceof", "new", "return", "switch"
    , "this", "throw", "try", "typeof", "var", "void", "while", "with", "class"
    , "const", "enum", "export", "extends", "import", "super", "implements"
    , "interface", "let", "package", "private", "protected", "public"
    , "static", "yield"
    -- reserved by the Elm runtime system
    , "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    , JS.localRuntime
    ]
