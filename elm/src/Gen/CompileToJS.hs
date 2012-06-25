
module CompileToJS (showErr, jsModule) where

import Ast
import Control.Monad (liftM,(<=<),join)
import Data.Char (isAlpha,isDigit)
import Data.List (intercalate,sortBy,inits)
import Data.Map (toList)
import Data.Maybe (mapMaybe)

import Initialize

showErr :: String -> String
showErr err = mainEquals $ "text(monospace(" ++ msg ++ "))"
    where msg = show . concatMap (++"<br>") . lines $ err

parens = ("("++) . (++")")
braces = ("{"++) . (++"}")
jsList = ("["++) . (++"]") . intercalate ","
jsFunc args body = "function(" ++ args ++ "){" ++ body ++ "}"
assign x e = "\nvar " ++ x ++ "=" ++ e ++ ";"
ret e = "\nreturn "++ e ++";"
iff a b c = a ++ "?" ++ b ++ ":" ++ c

mainEquals s = globalAssign "ElmCode.main" (jsFunc "" (ret s))
globalAssign m s = "\n" ++ m ++ "=" ++ s ++ ";"

tryBlock names e = 
    concat [ "\ntry{\n" ++ e ++ "\n\n} catch (e) {"
           , "ElmCode.main=function() {"
	   , "var msg = ('<br><h2>Your browser may not be supported. " ++
             "Are you using a modern browser?</h2>' +" ++
             " '<br><span style=\"color:grey\">Runtime Error in " ++
             intercalate "." names ++ " module:<br>' + e + '</span>');"
	   , "document.body.innerHTML = Text.monospace(msg);"
           , "throw e;"
           , "};}"
           ]


jsModule (Module names exports imports defs (ims,exs)) =
    tryBlock (tail modNames) $ concat
           [ concatMap (\n -> globalAssign n $ n ++ " || {}") .
             map (intercalate ".") . drop 2 . inits $
             take (length modNames - 1) modNames
           , "\nif (" ++ modName ++ ") throw \"Module name collision, '" ++
             intercalate "." (tail modNames) ++ "' is already defined.\"; "
           , globalAssign modName $ jsFunc "" (includes++body++export)++"()"
           , mainEquals $ modName ++ ".main" ]
        where modNames = if null names then ["ElmCode", "Main"] else "ElmCode" : names
              modName  = intercalate "." modNames
              includes = concatMap jsImport $
                         map (\(x,y) -> ("ElmCode." ++ x,y)) imports
              body = jsDefs defs
              export = ret . braces . intercalate "," $ mapMaybe getNames defs
              exps = if null exports then ["main"] else exports
              getNames (x,_) =
                  let y = reverse . tail . dropWhile isDigit $ reverse x in
                  if y `elem` exps then Just $ y ++ ":" ++ x else Nothing

ffi (ImportValue js elm _) = assign elm js
ffi (ExportValue js elm _) = assign js elm
ffi (ImportEvent js elm base _) =
    concat [ "var " ++ elm ++ " = Elm.Input(" ++ toJS base ++ ");"
           , "Signal.addListener(document, '" ++ js
           , "', function(e) { Dispatcher.notify(" ++ elm
           , ".id, e); });" ]
ffi (ExportEvent js elm base _) =
    concat [ "lift (function(v) { var e = document.createEvent('Event');"
           , "e.initEvent('" ++ js ++ "', true, true);"
           , "e.elmValue = v;"
           , "document.disspatchEvent(e); return v; })(" ++ elm ++ ")"
           ]

jsImport (modul, how) =
  concat [ "\ntry{" ++ modul ++ "} catch(e) {throw \"Module '"
         , drop 1 (dropWhile (/='.') modul)
         , "' is missing. Compile with --make flag or load missing "
         , "module in a separate JavaScript file.\";}" ] ++
     jsImport' (modul, how)
  
jsImport' (modul, As name) = assign name modul
jsImport' (modul, Importing []) = jsImport' (modul, Hiding [])
jsImport' (modul, Importing vs) =
    concatMap (\v -> assign v $ modul ++ "." ++ v) vs
jsImport' (modul, Hiding vs) =
    concat [ "\nfor(var i in " ++ modul ++ "){"
           , assign "hiddenVars" . jsList $ map (\v -> "'" ++ v ++ "'") vs
           , "\nif (hiddenVars.indexOf(i) >= 0) continue;"
           , globalAssign "this[i]" $ modul ++ "[i]"
           , "}" ]


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

jsLet defs e' = jsFunc "" (jsDefs defs ++ ret (toJS e')) ++ "()"

jsDefs defs = concatMap toDef $ sortBy f defs
    where f a b = compare (isLambda a) (isLambda b)
          isLambda (_, Lambda _ _) = 1
          isLambda _ = 0
          toDef (f, Lambda x e) =
              "\nfunction " ++ f ++ parens x ++ braces (ret $ toJS e) ++ ";"
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
                    "==" -> "eq(" ++ e1 ++ "," ++ e2 ++ ")"
                    "/=" -> "not(eq(" ++ e1 ++ "," ++ e2 ++ "))"
                    _ -> e1 ++ (o:p) ++ e2

append e1 e2 = "Value.append" ++ parens (e1 ++ "," ++ e2)