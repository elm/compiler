module Generate.JavaScript.Variable (fresh, canonical, define, safe) where

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

define :: Maybe ModuleName.Canonical -> String -> JS.Expression () -> JS.Statement ()
define maybeHome name body =
  let
    varDecl safeName =
      JS.VarDeclStmt () [ JS.VarDecl () (JS.Id () safeName) (Just body) ]
  in
  case maybeHome of
    Nothing ->
        varDecl (safe name)

    Just home ->
        if Help.isOp name then
          let
            root =
              JS.VarRef () (JS.Id () (canonicalPrefix home "_op"))

            lvalue =
              JS.LBracket () root (JS.StringLit () name)
          in
            JS.ExprStmt () (JS.AssignExpr () JS.OpAssign lvalue body)

        else
          varDecl (canonicalPrefix home (safe name))


-- INSTANTIATE VARIABLES

canonical :: Var.Canonical -> JS.Expression ()
canonical (Var.Canonical home name) =
  if Help.isOp name then
    JS.BracketRef () (JS.ref (addRoot home "_op")) (JS.string name)

  else
    JS.ref (addRoot home (safe name))


addRoot :: Var.Home -> String -> String
addRoot home name =
  case home of
    Var.Local ->
        name

    Var.TopLevel moduleName ->
        canonicalPrefix moduleName name

    Var.BuiltIn ->
        name

    Var.Module moduleName ->
        canonicalPrefix moduleName name


canonicalPrefix :: ModuleName.Canonical -> String -> String
canonicalPrefix (ModuleName.Canonical (Pkg.Name user project) moduleName) name =
    map (swap '-' '_') user
    ++ '$' : map (swap '-' '_') project
    ++ '$' : List.intercalate "$" moduleName
    ++ '$' : safe name


swap :: Char -> Char -> Char -> Char
swap from to c =
  if c == from then to else c


-- SAFE NAMES

safe :: String -> String
safe name =
    let saferName =
          if Set.member name jsReserveds then '$' : name else name
    in
        map (swap '\'' '$') saferName


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
    ]
