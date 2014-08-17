{-# OPTIONS_GHC -W #-}
module Generate.JavaScript.Helpers where

import AST.Annotation (Region)
import qualified AST.PrettyPrint as PP
import qualified Language.ECMAScript3.Syntax as JS

-- Creating Variables

var :: String -> JS.Id ()
var name = JS.Id () name

ref :: String -> JS.Expression ()
ref name = JS.VarRef () (var name)

prop :: String -> JS.Prop ()
prop name = JS.PropId () (var name)

string = JS.StringLit ()

obj :: [String] -> JS.Expression ()
obj vars =
    case vars of
      x:xs -> foldl (JS.DotRef ()) (ref x) (map var xs)
      [] -> error "dotSep must be called on a non-empty list of variables"

-- Specific Utilities

localModuleName :: String
localModuleName = "$moduleName"

throw :: String -> Region -> JS.Expression ()
throw kind region =
    obj ["_E",kind] `call` [ ref localModuleName, string (PP.renderPretty region) ]

-- Function Calls

(<|) :: JS.Expression () -> JS.Expression () -> JS.Expression ()
(<|) f x = JS.CallExpr () f [x]

ret :: JS.Expression () -> JS.Statement ()
ret e = JS.ReturnStmt () (Just e)

(==>) :: [String] -> JS.Expression () -> JS.Expression ()
(==>) args e = JS.FuncExpr () Nothing (map var args) [ ret e ]

function :: [String] -> [JS.Statement ()] -> JS.Expression ()
function args stmts = JS.FuncExpr () Nothing (map var args) stmts

call :: JS.Expression () -> [JS.Expression ()] -> JS.Expression ()
call = JS.CallExpr ()

-- Checks

equal :: JS.Expression () -> JS.Expression () -> JS.Expression ()
equal a b = JS.InfixExpr () JS.OpStrictEq a b

instanceof :: String -> JS.Expression () -> JS.Expression ()
instanceof tipe x =
    JS.InfixExpr () JS.OpLAnd (typeof "object" x) (JS.InfixExpr () JS.OpInstanceof x (ref tipe))

typeof :: String -> JS.Expression () -> JS.Expression ()
typeof tipe x = equal (JS.PrefixExpr () JS.PrefixTypeof x) (string tipe)

member :: String -> JS.Expression () -> JS.Expression ()
member field x = JS.InfixExpr () JS.OpIn (string field) x
