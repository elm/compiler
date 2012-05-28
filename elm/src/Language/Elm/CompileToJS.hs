
module Language.Elm.CompileToJS (compile, compileToJS) where

import Ast
import Control.Monad (liftM,(<=<),join)
import Data.Char (isAlpha)
import Data.List (intercalate,sortBy)
import Data.Map (toList)

import Language.Elm.Initialize


compile = (return . addMain . toJS) <=< initialize
compileToJS = addMain . either (\err -> "text('"++err++"')") toJS
addMain body = "function main(){return " ++ body ++ ";}"

parens = ("("++) . (++")")
braces = ("{"++) . (++"}")
jsList = ("["++) . (++"]") . intercalate ","
jsFunc args body = "function(" ++ args ++ "){" ++ body ++ "}"
assign x e = "var " ++ x ++ "=" ++ e ++ ";"
ret e = "return "++ e ++";"
iff a b c = a ++ "?" ++ b ++ ":" ++ c

toJS expr =
    case expr of
      Number n -> show n
      Var x -> x
      Chr c -> show c
      Str s -> toJS . list $ map Chr s
      Boolean b -> if b then "true" else "false"
      Range lo hi -> jsRange (toJS lo) (toJS hi)
      Access e lbl -> toJS e ++ "." ++ lbl
      Binop op e1 e2 -> binop op (toJS e1) (toJS e2)
      If eb et ef -> parens $ iff (toJS eb) (toJS et) (toJS ef)
      Lambda v e -> jsFunc v $ ret (toJS e)
      App (Var "toText") (Str s) -> show s
      App (Var "link") (Str s) -> "link(" ++ show s ++ ")"
      App (Var "plainText") (Str s) -> "plainText(" ++ show s ++ ")"
      App e1 e2 -> toJS e1 ++ parens (toJS e2)
      Let defs e -> jsLet defs e
      Case e cases -> jsCase e cases
      Data name es -> jsList $ show name : map toJS es

jsLet defs e' = jsFunc "" (defs' ++ ret (toJS e')) ++ "()"
    where defs' = concatMap toDef $ sortBy f defs
          f a b = compare (isLambda a) (isLambda b)
          isLambda (_, Lambda _ _) = 1
          isLambda _ = 0
          toDef (f, Lambda x e) =
              "function " ++ f ++ parens x ++ braces (ret $ toJS e) ++ ";"
          toDef (x, e) = assign x (toJS e)

jsCase e  [c]  = jsMatch c ++ parens (toJS e)
jsCase e cases = "(function(){" ++
                 assign "v" (toJS e) ++
                 assign "c" jsCases  ++
                 "for(var i=c.length;i--;){" ++
                 assign "r" "c[i](v)" ++
                 "if(r!==undefined){return r;}}}())"
    where jsCases = jsList $ map jsMatch (reverse cases)

jsMatch (p,e) = jsFunc "v" . match p "v" . ret $ toJS e
match p v hole =
    case p of
      PAnything -> hole
      PVar x -> assign x v ++ hole
      PData name ps ->
          "if(" ++ show name ++ "!==" ++ v ++
          "[0]){return undefined;}else{"++body++"}"
              where matches = zipWith match ps vs
                    vs = map (\i -> v++"["++show (i+1)++"]") [0..length ps-1]
                    body = foldr ($) hole matches

jsNil         = "[\"Nil\"]"
jsCons  e1 e2 = jsList [ show "Cons", e1, e2 ]
jsRange e1 e2 = (++"()") . jsFunc "" $
                assign "lo" e1 ++ assign "hi" e2 ++ assign "lst" jsNil ++
                "do{" ++ assign "lst" (jsCons "hi" "lst") ++ "}while(hi-->lo)" ++
                ret "lst"

binop (o:p) e1 e2
    | isAlpha o || '_' == o = (o:p) ++ parens e1 ++ parens e2
    | otherwise = case o:p of
                    ":" -> jsCons e1 e2
                    "++" -> append e1 e2
                    "$" -> e1 ++ parens e2
                    "." -> jsFunc "x" . ret $ e1 ++ parens (e2 ++ parens "x")
                    "/=" -> e1 ++ "!==" ++ e2
                    _ -> e1 ++ (o:p) ++ e2

append e1 e2 = "Value.append" ++ parens (e1 ++ "," ++ e2)