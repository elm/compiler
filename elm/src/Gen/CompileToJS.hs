module CompileToJS (showErr, jsModule) where

import Ast
import Control.Arrow (first)
import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import Data.List (intercalate,sortBy,inits,foldl')
import Data.Map (toList)
import Data.Either (partitionEithers)
import qualified Text.Pandoc as Pan

import Initialize
import Rename (derename)
import Cases
import Guid
import Parse.Library (isOp)
import Rename (deprime)

showErr :: String -> String
showErr err = mainEquals $ "Elm.Graphics.text(Elm.Text.monospace(" ++ msg ++ "))"
    where msg = show . concatMap (++"<br>") . lines $ err

indent = concatMap f
    where f '\n' = "\n "
          f c = [c]

parens s  = "(" ++ s ++ ")"
braces s  = "{" ++ s ++ "}"
jsList ss = "["++ intercalate "," ss ++"]"
jsFunc args body = "function(" ++ args ++ "){" ++ indent body ++ "}"
assign x e = "\nvar " ++ x ++ "=" ++ e ++ ";"
ret e = "\nreturn "++ e ++";"
iff a b c = a ++ "?" ++ b ++ ":" ++ c
quoted s  = "'" ++ concatMap f s ++ "'"
    where f '\n' = "\\n"
          f '\'' = "\\'"
          f '\t' = "\\t"
          f '\"' = "\\\""
          f '\\' = "\\\\"
          f c    = [c]

mainEquals s = globalAssign "Elm.main" (jsFunc "" (ret s))
globalAssign m s = "\n" ++ m ++ "=" ++ s ++ ";"

tryBlock escapees names e = 
    concat [ "\ntry{\n" ++ e ++ "\n} catch (e) {"
           , "\nElm.main=function() {"
	   , "\nvar msg = ('<br/><h2>Your browser may not be supported. " ++
             "Are you using a modern browser?</h2>' +" ++
             " '<br/><span style=\"color:grey\">Runtime Error in " ++
             intercalate "." names ++ " module:<br/>' + e + '" ++ msg ++ "</span>');"
	   , "\ndocument.body.innerHTML = Elm.Text.monospace(msg);"
           , "throw e;"
           , "};}"
           ]
    where msg | escapees /= [] = concat [ "<br/><br/>The problem may stem from an improper usage of:<br/>"
                                        ,  intercalate ", " $ map (concatMap escape) escapees ]
              | otherwise = ""
          escape '\'' = "\\'"
          escape '"' = "\\\""
          escape c = [c]


jsModule (escapees, Module names exports imports stmts) =
    tryBlock escapees (tail modNames) $ concat
           [ concatMap (\n -> globalAssign n $ n ++ " || {}") .
             map (intercalate ".") . drop 2 . inits $
             take (length modNames - 1) modNames
           , "\nif (" ++ modName ++ ") throw \"Module name collision, '" ++
             intercalate "." (tail modNames) ++ "' is already defined.\"; "
           , globalAssign modName $ jsFunc "" (defs ++ includes ++ body ++ export) ++ "()"
           , mainEquals $ modName ++ ".main" ]
        where modNames = if null names then ["Elm", "Main"]
                                       else  "Elm" : names
              modName  = intercalate "." modNames
              includes = concatMap jsImport $ map (first ("Elm."++)) imports
              body = stmtsToJS stmts
              export = getExports exports stmts
              exps = if null exports then ["main"] else exports
              defs = concat [ assign "$op" "{}"
                            , "\nfor(Elm['i'] in Elm){eval('var '+Elm['i']+'=Elm[Elm.i];');}" ]

getExports names stmts = ret . braces $ intercalate ",\n" (op : map fnPair fns)
    where exNames n = either derename id n `elem` names
          exports | null names = concatMap get stmts
                  | otherwise  = filter exNames (concatMap get stmts)

          (fns,ops) = partitionEithers exports

          opPair op = "'" ++ op ++ "' : $op['" ++ op ++ "']"
          fnPair fn = let fn' = derename fn in fn' ++ ":" ++ fn

          op = ("$op : "++) . braces . intercalate ", " $ map opPair ops

          get' (FnDef x _ _) = Left x
          get' (OpDef op _ _ _) = Right op
          get s = case s of Definition d        -> [ get' d ]
                            Datatype _ _ tcs    -> map (Left . fst) tcs
                            ImportEvent _ _ x _ -> [ Left x ]
                            ExportEvent _ _ _   -> []


jsImport (modul, how) =
  concat [ "\ntry{\n if (!(" ++ modul ++ " instanceof Object)) throw 'module not found';\n} catch(e) {\n throw (\"Module '"
         , drop 1 (dropWhile (/='.') modul)
         , "' is missing. Compile with --make flag or load missing "
         , "module in a separate JavaScript file.\");\n}" ] ++
     jsImport' (modul, how)
  
jsImport' (modul, As name) = assign name modul
jsImport' (modul, Importing vs) = concatMap def vs
    where def [] = []
          def (o:p) | isOp o    = let v = "$op['" ++ o:p ++ "']" in
                                  "\n" ++ v ++ " = " ++ modul ++ "." ++ v ++ ";"
                    | otherwise = let v = deprime (o:p) in
                                  assign v $ modul ++ "." ++ v

jsImport' (modul, Hiding vs) =
    concat [ assign "hiddenVars" . ("{"++) . (++"}") . intercalate "," $
                    map (\v -> v ++ ":true") (map deprime vs)
           , "\nfor (Elm['i'] in " ++ modul ++ ") "
           , braces . indent . concat $
               [ "\nif (hiddenVars[Elm['i']]) continue;"
               , "\neval('var ' + Elm['i'] + ' = "
               , modul, "[Elm.i];');" ]
           ]

stmtsToJS :: [Statement] -> String
stmtsToJS stmts = run (concat `liftM` mapM toJS (sortBy cmpStmt stmts))
    where cmpStmt s1 s2 = compare (valueOf s1) (valueOf s2)
          valueOf s = case s of
                        Datatype _ _ _             -> 1
                        ImportEvent _ _ _ _        -> 2
                        Definition (FnDef f [] _)  ->
                            if derename f == "main" then 5 else 4
                        Definition _               -> 3
                        ExportEvent _ _ _          -> 6

class ToJS a where
  toJS :: a -> GuidCounter String

instance ToJS Def where
  toJS (FnDef x [] e) = assign x `liftM` toJS e
  toJS (FnDef f (a:as) e) =
      do body <- toJS (foldr Lambda e as)
         return $ concat ["\nfunction ",f,parens a, braces . indent $ ret body]
  toJS (OpDef op a1 a2 e) =
      do body <- toJS (foldr Lambda e [a1,a2])
         return $ concat [ "\n$op['", op, "'] = ", body, ";" ]

instance ToJS Statement where
    toJS (Definition d) = toJS d
    toJS (Datatype _ _ tcs) = concat `liftM` mapM (toJS . toDef) tcs
        where toDef (name,args) =
                  Definition . FnDef name vars $ Data (derename name) (map Var vars)
                      where vars = map (('a':) . show) [1..length args]
    toJS (ImportEvent js base elm _) =
        do v <- toJS base
           return $ concat [ "\nvar " ++ elm ++ "=Elm.Signal.constant(" ++ v ++ ");"
                           , "\nValue.addListener(document, '" ++ js
                           , "', function(e) { Dispatcher.notify(" ++ elm
                           , ".id, e.value); });" ]
    toJS (ExportEvent js elm _) =
        return $ concat [ "\nlift(function(v) { "
                        , "var e = document.createEvent('Event');"
                        , "e.initEvent('", js, "', true, true);"
                        , "e.value = v;"
                        , "document.dispatchEvent(e); return v; })(", elm, ");" ]

instance ToJS Expr where
  toJS expr =
    case expr of
      IntNum n -> return $ show n
      FloatNum n -> return $ show n
      Var x -> return $ x
      Chr c -> return $ quoted [c]
      Str s -> return $ "Value.str" ++ parens (quoted s)
      Boolean b -> return $ if b then "true" else "false"
      Range lo hi -> jsRange `liftM` toJS lo `ap` toJS hi
      Access e lbl -> (\s -> s ++ "." ++ lbl) `liftM` toJS e
      Binop op e1 e2 -> binop op `liftM` toJS e1 `ap` toJS e2
      If eb et ef -> parens `liftM` (iff `liftM` toJS eb `ap` toJS et `ap` toJS ef)
      Guard ps -> guardToJS ps
      Lambda v e -> liftM (jsFunc v . ret) (toJS e)
      App (Var "toText") (Str s) -> return $ "toText" ++ parens (quoted s)
      App (Var "link") (Str s) -> return $ "link(" ++ quoted s ++ ")"
      App (Var "plainText") (Str s) -> return $ "plainText(" ++ quoted s ++ ")"
      App e1 e2 -> (++) `liftM` (toJS e1) `ap` (parens `liftM` toJS e2)
      Let defs e -> jsLet defs e
      Case e cases -> caseToJS e cases
      Data name es -> (\ss -> jsList $ quoted name : ss) `liftM` mapM toJS es
      Markdown doc -> return $ "text('" ++ pad ++ md ++ pad ++ "')"
          where md = formatMarkdown $ Pan.writeHtmlString Pan.defaultWriterOptions doc
                pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"

formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

guardToJS ps = format `liftM` mapM f ps
    where format cs = foldr (\c e -> parens $ c ++ " : " ++ e) err cs
          err = "(function(){throw \"Non-exhaustive guard expression\";}())"
          f (b,e) = do b' <- toJS b
                       e' <- toJS e
                       return (b' ++ " ? " ++ e')

jsLet defs e' = do
  body <- (++) `liftM` jsDefs defs `ap` (ret `liftM` toJS e')
  return $ jsFunc "" body ++ "()"

jsDefs defs = concat `liftM` mapM toJS (sortBy f defs)
    where f a b = compare (valueOf a) (valueOf b)
          valueOf (FnDef _ args _) = min 1 (length args)
          valueOf (OpDef _ _ _ _)  = 1

caseToJS e ps = do
  match <- caseToMatch ps
  e' <- toJS e
  (match',stmt) <- case (match,e) of
                     (Match name _ _, Var x) -> return (matchSubst [(name,x)] match, "")
                     (Match name _ _, _    ) -> return (match, assign name e')
                     _ -> liftM (\n -> (match, e')) guid
  matches <- matchToJS match'
  return $ concat [ "function(){", stmt, matches, "}()" ]

matchToJS (Match name clauses def) = do
  cases <- concat `liftM` mapM (clauseToJS name) clauses
  finally <- matchToJS def
  return $ concat [ "\nswitch(", name, "[0]){", indent cases, "\n}", finally ]
matchToJS Fail  = return "\nthrow \"Non-exhaustive pattern match in case\";"
matchToJS Break = return "break;"
matchToJS (Other e) = ret `liftM` toJS e
matchToJS (Seq ms) = concat `liftM` mapM matchToJS ms

clauseToJS var (Clause name vars e) = do
  let vars' = map (\n -> var ++ "[" ++ show n ++ "]") [ 1 .. length vars ]
  s <- matchToJS $ matchSubst (zip vars vars') e
  return $ concat [ "\ncase ", quoted name, ":", s ]

jsNil         = "[\"Nil\"]"
jsCons  e1 e2 = jsList [ quoted "Cons", e1, e2 ]
jsRange e1 e2 = (++"()") . jsFunc "" $
                assign "lo" e1 ++ assign "hi" e2 ++ assign "lst" jsNil ++
                "if(lo<=hi){do{lst=" ++ (jsCons "hi" "lst") ++ "}while(hi-->lo)}" ++
                ret "lst"

binop (o:p) e1 e2
    | isAlpha o || '_' == o = (o:p) ++ parens e1 ++ parens e2
    | otherwise =
        let ops = ["+","-","*","/","&&","||"] in
        case o:p of
          ":"  -> jsCons e1 e2
          "++" -> append e1 e2
          "$"  -> e1 ++ parens e2
          "."  -> jsFunc "x" . ret $ e1 ++ parens (e2 ++ parens "x")
          "^"  -> "Math.pow(" ++ e1 ++ "," ++ e2 ++ ")"
          "==" -> "eq(" ++ e1 ++ "," ++ e2 ++ ")"
          "/=" -> "not(eq(" ++ e1 ++ "," ++ e2 ++ "))"
          "<"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0] === 'LT')"
          ">"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ")[0] === 'GT')"
          "<=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++
                  e2 ++ ")[0]; return ord==='LT' || ord==='EQ'; }()"
          ">=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++
                  e2 ++ ")[0]; return ord==='GT' || ord==='EQ'; }()"
          "<~" -> "lift" ++ parens e1 ++ parens e2
          "~"  -> "lift2(function(f){return function(x){return f(x);};})" ++
                  parens e1 ++ parens e2
          _  | elem (o:p) ops -> parens (e1 ++ (o:p) ++ e2)
             | otherwise      -> concat [ "$op['", o:p, "']"
                                        , parens e1, parens e2 ]

append e1 e2 = "Value.append" ++ parens (e1 ++ "," ++ e2)
