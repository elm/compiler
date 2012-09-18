
module CompileToJS (showErr, jsModule) where

import Ast
import Control.Arrow (first)
import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import Data.List (intercalate,sortBy,inits,foldl')
import Data.Map (toList)
import Data.Maybe (mapMaybe)
import qualified Text.Pandoc as Pan

import Initialize
import Rename (derename)
import Cases
import Guid

showErr :: String -> String
showErr err = mainEquals $ "text(monospace(" ++ msg ++ "))"
    where msg = show . concatMap (++"<br>") . lines $ err

parens s  = "(" ++ s ++ ")"
braces s  = "{" ++ s ++ "}"
jsList ss = "["++ intercalate "," ss ++"]"
jsFunc args body = "function(" ++ args ++ "){" ++ body ++ "}"
assign x e = "\nvar " ++ x ++ "=" ++ e ++ ";"
ret e = "\nreturn "++ e ++";"
iff a b c = a ++ "?" ++ b ++ ":" ++ c

mainEquals s = globalAssign "ElmCode.main" (jsFunc "" (ret s))
globalAssign m s = "\n" ++ m ++ "=" ++ s ++ ";"

tryBlock escapees names e = 
    concat [ "\ntry{\n" ++ e ++ "\n\n} catch (e) {"
           , "ElmCode.main=function() {"
	   , "var msg = ('<br/><h2>Your browser may not be supported. " ++
             "Are you using a modern browser?</h2>' +" ++
             " '<br/><span style=\"color:grey\">Runtime Error in " ++
             intercalate "." names ++ " module:<br/>' + e + '" ++ msg ++ "</span>');"
	   , "document.body.innerHTML = Text.monospace(msg);"
           , "throw e;"
           , "};}"
           ]
    where msg | escapees /= [] = concat [ "<br/><br/>The problem may stem from an improper usage of:<br/>"
                                        ,  intercalate ", " escapees ]
              | otherwise = ""

jsModule (escapees, Module names exports imports stmts) =
    tryBlock escapees (tail modNames) $ concat
           [ concatMap (\n -> globalAssign n $ n ++ " || {}") .
             map (intercalate ".") . drop 2 . inits $
             take (length modNames - 1) modNames
           , "\nif (" ++ modName ++ ") throw \"Module name collision, '" ++
             intercalate "." (tail modNames) ++ "' is already defined.\"; "
           , globalAssign modName $ jsFunc "" (includes ++body++ export) ++ "()"
           , mainEquals $ modName ++ ".main" ]
        where modNames = if null names then ["ElmCode", "Main"]
                                       else  "ElmCode" : names
              modName  = intercalate "." modNames
              includes = concatMap jsImport $ map (first ("ElmCode."++)) imports
              body = stmtsToJS stmts
              export = getExports exps stmts
              exps = if null exports then ["main"] else exports

getExports names stmts = ret . braces $ intercalate "," pairs
    where pairs = mapMaybe pair $ concatMap get stmts
          pair x = if y `elem` names then Just $ y ++ ":" ++ x else Nothing
              where y = derename x
          get s = case s of Def x _ _           -> [x]
                            Datatype _ _ tcs    -> map fst tcs
                            ImportEvent _ _ x _ -> [x]
                            ExportEvent _ _ _   -> []


jsImport (modul, how) =
  concat [ "\ntry{if (!(" ++ modul ++ " instanceof Object)) throw 'module not found'; } catch(e) {throw \"Module '"
         , drop 1 (dropWhile (/='.') modul)
         , "' is missing. Compile with --make flag or load missing "
         , "module in a separate JavaScript file.\";}" ] ++
     jsImport' (modul, how)
  
jsImport' (modul, As name) = assign name modul
jsImport' (modul, Importing []) = jsImport' (modul, Hiding [])
jsImport' (modul, Importing vs) =
    concatMap (\v -> assign v $ modul ++ "." ++ v) vs
jsImport' (modul, Hiding vs) =
    concat [ assign "hiddenVars" . jsList $ map (\v -> "'" ++ v ++ "'") vs
           , "\nfor(var i in " ++ modul ++ "){"
           , "\nif (hiddenVars.indexOf(i) >= 0) continue;"
           , globalAssign "this[i]" $ modul ++ "[i]"
           , "}" ]

stmtsToJS :: [Statement] -> String
stmtsToJS stmts = concatMap stmtToJS (sortBy cmpStmt stmts)
    where cmpStmt s1 s2 = compare (valueOf s1) (valueOf s2)
          valueOf s = case s of Datatype _ _ _      -> 1
                                ImportEvent _ _ _ _ -> 3
                                Def _ [] _          -> 3
                                Def _ _ _           -> 4
                                ExportEvent _ _ _   -> 5

stmtToJS :: Statement -> String
stmtToJS (Def name [] e) = assign name (toJS e)
stmtToJS (Def name (a:as) e) = "\nfunction " ++ name ++ parens a ++
                               braces (ret . toJS $ foldr Lambda e as) ++ ";"
stmtToJS (Datatype _ _ tcs) = concatMap (stmtToJS . toDef) tcs
    where toDef (name,args) = Def name vars $ Data (derename name) (map Var vars)
              where vars = map (('a':) . show) [1..length args]
stmtToJS (ImportEvent js base elm _) =
    concat [ "\nvar " ++ elm ++ " = Elm.Input(" ++ toJS base ++ ");"
           , "\nSignal.addListener(document, '" ++ js
           , "', function(e) { Dispatcher.notify(" ++ elm
           , ".id, e.value); });" ]
stmtToJS (ExportEvent js elm _) =
    concat [ "\nlift(function(v) { var e = document.createEvent('Event');"
           , "e.initEvent('" ++ js ++ "', true, true);"
           , "e.value = v;"
           , "document.dispatchEvent(e); return v; })(" ++ elm ++ ");"
           ]


toJS = run . toJS'

toJS' :: Expr -> GuidCounter String
toJS' expr =
    case expr of
      IntNum n -> return $ show n
      FloatNum n -> return $ show n
      Var x -> return $ x
      Chr c -> return $ show c
      Str s -> return $ "Value.str" ++ parens (show s)
      Boolean b -> return $ if b then "true" else "false"
      Range lo hi -> jsRange `liftM` toJS' lo `ap` toJS' hi
      Access e lbl -> (\s -> s ++ "." ++ lbl) `liftM` toJS' e
      Binop op e1 e2 -> binop op `liftM` toJS' e1 `ap` toJS' e2
      If eb et ef -> parens `liftM` (iff `liftM` toJS' eb `ap` toJS' et `ap` toJS' ef)
      Lambda v e -> liftM (jsFunc v . ret) (toJS' e)
      App (Var "toText") (Str s) -> return $ "toText" ++ parens (show s)
      App (Var "link") (Str s) -> return $ "link(" ++ show s ++ ")"
      App (Var "plainText") (Str s) -> return $ "plainText(" ++ show s ++ ")"
      App e1 e2 -> (++) `liftM` (toJS' e1) `ap` (parens `liftM` toJS' e2)
      Let defs e -> jsLet defs e
      Case e cases -> caseToJS e cases
      Data name es -> (\ss -> jsList $ show name : ss) `liftM` mapM toJS' es
      Markdown doc -> return $ "text('" ++ pad ++ md ++ pad ++ "')"
          where md = formatMarkdown $ Pan.writeHtmlString Pan.defaultWriterOptions doc
                pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"

formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

jsLet defs e' = do
  body <- (++) `liftM` jsDefs defs `ap` (ret `liftM` toJS' e')
  return $ jsFunc "" body ++ "()"

jsDefs defs = concat `liftM` mapM toDef (sortBy f defs)
    where f a b = compare (valueOf a) (valueOf b)
          valueOf (Definition _ args _) = min 1 (length args)
          toDef (Definition x [] e) = assign x `liftM` toJS' e
          toDef (Definition f (a:as) e) = do
            out <- toJS' $ foldr Lambda e as
            return $ "\nfunction " ++ f ++ parens a ++ braces (ret out) ++ ";"

caseToJS e ps = do
  match <- caseToMatch ps
  var <- liftM (\n -> "case" ++ show n) guid
  matches <- matchToJS var match
  e' <- toJS' e
  return $ concat [ "function(", var, "){"
                  , case match of { Match name _ _ -> assign name var ; _ -> "" }
                  , matches
                  , "}", parens e' ]

matchToJS v (Match name clauses def) = do
  cases <- concat `liftM` mapM (clauseToJS name) clauses
  finally <- matchToJS v def
  return $ concat [ "\nswitch(", name, "[0]){", cases, "\n}", finally ]
matchToJS _ Fail  = return "\nthrow \"Non-exhaustive pattern match in case\";"
matchToJS _ Break = return "break;"
matchToJS _ (Other e) = ret `liftM` toJS' e
matchToJS v (Seq ms) = concat `liftM` mapM (matchToJS v) ms

clauseToJS var (Clause name vars e) = do
  s <- matchToJS var e
  return $ concat [ "\ncase ", show name, ":"
                  , defs
                  , s ]
        where toDef v n = v ++ "=" ++ var ++ "[" ++ show n ++ "]"
              defs = case vars of [] -> ""
                                  _ -> ("\nvar "++) . (++";") . intercalate "," $
                                         zipWith toDef vars [ 1 .. length vars ]

jsNil         = "[\"Nil\"]"
jsCons  e1 e2 = jsList [ show "Cons", e1, e2 ]
jsRange e1 e2 = (++"()") . jsFunc "" $
                assign "lo" e1 ++ assign "hi" e2 ++ assign "lst" jsNil ++
                "if(lo<=hi){do{" ++ assign "lst" (jsCons "hi" "lst") ++ "}while(hi-->lo)}" ++
                ret "lst"

binop (o:p) e1 e2
    | isAlpha o || '_' == o = (o:p) ++ parens e1 ++ parens e2
    | otherwise = case o:p of
                    ":"  -> jsCons e1 e2
                    "++" -> append e1 e2
                    "$"  -> e1 ++ parens e2
                    "."  -> jsFunc "x" . ret $ e1 ++ parens (e2 ++ parens "x")
                    "^"  -> "Math.pow(" ++ e1 ++ "," ++ e2 ++ ")"
                    "==" -> "eq(" ++ e1 ++ "," ++ e2 ++ ")"
                    "/=" -> "not(eq(" ++ e1 ++ "," ++ e2 ++ "))"
                    "<"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0] === 'LT')"
                    ">"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0] === 'GT')"
                    "<=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0]; return ord === 'LT' || ord === 'EQ'; }()"
                    ">=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0]; return ord === 'GT' || ord === 'EQ'; }()"
                    _    -> parens (e1 ++ (o:p) ++ e2)

append e1 e2 = "Value.append" ++ parens (e1 ++ "," ++ e2)