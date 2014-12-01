module Generate.JavaScript.Variable where

import qualified AST.Helpers as Help
import qualified AST.Module as Module
import qualified AST.Variable as Var
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Generate.JavaScript.Helpers as JS
import qualified Language.ECMAScript3.Syntax as JS


swap :: Char -> Char -> Char -> Char
swap from to c =
  if c == from then to else c


canonical :: Var.Canonical -> JS.Expression ()
canonical (Var.Canonical home name) =
  case Help.isOp name of
    True  -> JS.BracketRef () (JS.obj (home' ++ ["_op"])) (JS.string name)
    False -> JS.obj (home' ++ [ varName name ])
  where
    home' =
      case home of
        Var.Local       -> []
        Var.BuiltIn     -> []
        Var.Module path -> [ moduleName path ]


moduleName :: Module.Name -> String
moduleName name =
    '$' : List.intercalate "$" name


varName :: String -> String
varName x = map (swap '\'' '$') x'
    where
      x' = if Set.member x jsReserveds then '$' : x else x


value :: Module.Name -> String -> JS.Expression ()
value home name =
    canonical (Var.Canonical (Var.Module home) name)


jsReserveds :: Set.Set String
jsReserveds = Set.fromList
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
    , "Elm", "ElmRuntime"
    , "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9"
    , "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"
    ]
