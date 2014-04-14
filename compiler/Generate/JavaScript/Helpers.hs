{-# OPTIONS_GHC -W #-}
module Generate.JavaScript.Helpers where

import qualified AST.Helpers as Help
import Language.ECMAScript3.Syntax

dolarize :: String -> String
dolarize x = map (\c -> if c == '.' then '$' else c) x

split :: String -> [String]
split = go []
  where
    go vars str =
        case break (=='.') str of
          (x,_:rest) | Help.isOp x -> vars ++ [x ++ '.' : rest]
                     | otherwise -> go (vars ++ [x]) rest
          (x,[]) -> vars ++ [x]

var :: String -> Id ()
var name = Id () name

ref :: String -> Expression ()
ref name = VarRef () (var name)

prop name = PropId () (var name)
f <| x = CallExpr () f [x]
ret e = ReturnStmt () (Just e)
args ==> e = FuncExpr () Nothing (map var args) [ ret e ]
function args stmts = FuncExpr () Nothing (map var args) stmts
call = CallExpr ()
string = StringLit ()

dotSep :: [String] -> Expression ()
dotSep vars =
    case vars of
      x:xs -> foldl (DotRef ()) (ref x) (map var xs)
      [] -> error "dotSep must be called on a non-empty list of variables"

obj = dotSep . split

equal a b = InfixExpr () OpStrictEq a b
instanceof tipe x =
    InfixExpr () OpLAnd (typeof "object" x) (InfixExpr () OpInstanceof x (ref tipe))
typeof tipe x = equal (PrefixExpr () PrefixTypeof x) (string tipe)
member field x = InfixExpr () OpIn (string field) x
