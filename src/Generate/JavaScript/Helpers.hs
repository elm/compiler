module Generate.JavaScript.Helpers where

import Language.ECMAScript3.Syntax



localRuntime :: String
localRuntime =
    "_elm"


varDecl :: String -> Expression () -> VarDecl ()
varDecl x expr =
    VarDecl () (var x) (Just expr)


refOrObject :: String -> Expression ()
refOrObject name =
  InfixExpr () OpLOr (ref name) (ObjectLit () [])



-- Creating Variables


var :: String -> Id ()
var name =
    Id () name


ref :: String -> Expression ()
ref name =
    VarRef () (var name)


(==>) :: String -> Expression () -> (Prop (), Expression ())
(==>) name expr =
  ( PropId () (var name), expr )


obj :: [String] -> Expression ()
obj vars =
    case vars of
      x:xs ->
          foldl (DotRef ()) (ref x) (map var xs)

      [] ->
          error "dotSep must be called on a non-empty list of variables"



-- Function Calls


(<|) :: Expression () -> Expression () -> Expression ()
(<|) f x =
    CallExpr () f [x]


function :: [String] -> [Statement ()] -> Expression ()
function args stmts =
    FuncExpr () Nothing (map var args) stmts


call :: Expression () -> [Expression ()] -> Expression ()
call =
    CallExpr ()

