module Generate.JavaScript.Helpers where

import Language.ECMAScript3.Syntax


localRuntime :: String
localRuntime =
    "_elm"


varDecl :: String -> Expression () -> VarDecl ()
varDecl x expr =
    VarDecl () (var x) (Just expr)


make :: [String] -> Expression ()
make moduleName =
    obj (moduleName ++ ["make"]) <| ref localRuntime


useLazy :: [String] -> String -> Expression ()
useLazy moduleName functionName =
    DotRef () (make moduleName) (var functionName)


-- Creating Variables

var :: String -> Id ()
var name =
    Id () name


ref :: String -> Expression ()
ref name =
    VarRef () (var name)


prop :: String -> Prop ()
prop name =
    PropId () (var name)


string = StringLit ()


obj :: [String] -> Expression ()
obj vars =
    case vars of
      x:xs -> foldl (DotRef ()) (ref x) (map var xs)
      [] -> error "dotSep must be called on a non-empty list of variables"


-- Function Calls

(<|) :: Expression () -> Expression () -> Expression ()
(<|) f x =
    CallExpr () f [x]


ret :: Expression () -> Statement ()
ret e =
    ReturnStmt () (Just e)


(==>) :: [String] -> Expression () -> Expression ()
(==>) args e =
    FuncExpr () Nothing (map var args) [ ret e ]


function :: [String] -> [Statement ()] -> Expression ()
function args stmts =
    FuncExpr () Nothing (map var args) stmts


call :: Expression () -> [Expression ()] -> Expression ()
call =
    CallExpr ()


-- Checks

equal :: Expression () -> Expression () -> Expression ()
equal a b =
    InfixExpr () OpStrictEq a b


instanceof :: String -> Expression () -> Expression ()
instanceof tipe x =
    InfixExpr () OpLAnd (typeof "object" x) (InfixExpr () OpInstanceof x (ref tipe))


typeof :: String -> Expression () -> Expression ()
typeof tipe x =
    equal (PrefixExpr () PrefixTypeof x) (string tipe)


member :: String -> Expression () -> Expression ()
member field x =
    InfixExpr () OpIn (string field) x
