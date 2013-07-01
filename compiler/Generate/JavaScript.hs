module Generate.JavaScript (showErr, jsModule) where

import Control.Arrow (first,second)
import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import Data.List (intercalate,sortBy,inits,foldl')
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import qualified Text.Pandoc as Pan
import Data.Maybe (maybeToList)
import Data.Data

import Unique
import Generate.Cases
import SourceSyntax.Everything hiding (parens)
import Types.Types ( Type(RecordT) )

showErr :: String -> String
showErr err = globalAssign "Elm.Main" (jsFunc "elm" body)
    where msg = show . concatMap (++"<br>") . lines $ err
          body = "var T = Elm.Text(elm);\n\
                 \return { main : T.text(T.monospace(" ++ msg ++ ")) };"

indent = concatMap f
    where f '\n' = "\n  "
          f c = [c]

internalImports =
    [ ("N" , "Elm.Native"),
      ("_N", "N.Utils(elm)"),
      ("_L", "N.List(elm)"),
      ("_E", "N.Error(elm)"),
      ("_J", "N.JavaScript(elm)"),
      ("_str", "_J.toString")
    ]

parens s  = "(" ++ s ++ ")"
brackets s  = "{" ++ s ++ "}"
jsObj = brackets . intercalate ", "
jsList ss = "["++ intercalate "," ss ++"]"
jsFunc args body = "function(" ++ args ++ "){" ++ indent body ++ "}"
assign x e = "\nvar " ++ x ++ " = " ++ e ++ ";"
ret e = "\nreturn "++ e ++";"
quoted s  = "'" ++ concatMap f s ++ "'"
    where f '\n' = "\\n"
          f '\'' = "\\'"
          f '\t' = "\\t"
          f '\"' = "\\\""
          f '\\' = "\\\\"
          f c    = [c]

globalAssign n e = "\n" ++ assign' n e ++ ";"
assign' n e = n ++ " = " ++ e

jsModule (Module names exports imports stmts) =
 setup ("Elm":modNames) ++ globalAssign ("Elm." ++ modName) (jsFunc "elm" program)
  where
    modNames = if null names then ["Main"] else names
    modName  = intercalate "." modNames
    includes = concatMap jsImport imports
    body = stmtsToJS stmts
    defs = assign "$op" "{}"
    program = "\nvar " ++ usefulFuncs ++ ";" ++ defs ++ includes ++ body ++
              setup ("elm":"Native":modNames) ++
              assign "_" ("elm.Native." ++ modName ++ "||{}") ++
              getExports exports stmts ++ setup ("elm":modNames) ++
              ret (assign' ("elm." ++ modName) "_") ++ "\n"
    setup modNames = concatMap (\n -> globalAssign n $ n ++ "||{}") .
                     map (intercalate ".") . drop 2 . inits $ init modNames
    usefulFuncs = intercalate ", " (map (uncurry assign') internalImports)

getExports names stmts = "\n"++ intercalate ";\n" (op : map fnPair fns)
    where exNames n = either derename id n `elem` names
          exports | null names = concatMap get stmts
                  | otherwise  = filter exNames (concatMap get stmts)

          (fns,ops) = partitionEithers exports

          opPair op = "'" ++ op ++ "' : $op['" ++ op ++ "']"
          fnPair fn = let fn' = derename fn in "_." ++ fn' ++ " = " ++ fn

          op = ("_.$op = "++) . jsObj $ map opPair ops

          get' (FnDef x _ _) = Just (Left x)
          get' (OpDef op _ _ _) = Just (Right op)
          get' (TypeAnnotation _ _) = Nothing
          get s = case s of Definition d        -> maybeToList (get' d)
                            Datatype _ _ tcs    -> map (Left . fst) tcs
                            ImportEvent _ _ x _ -> [ Left x ]
                            ExportEvent _ _ _   -> []
                            TypeAlias _ _ _     -> []


jsImport (modul, how) =
    case how of
      As name -> assign name ("Elm." ++ modul ++ parens "elm")
      Hiding vs -> include ++ " var hiding=" ++ (jsObj $ map (++":1") vs) ++
                   "; for(var k in _){if(k in hiding)continue;" ++
                   "eval('var '+k+'=_[\"'+k+'\"]')}"
      Importing vs -> include ++ named
          where 
            imprt v = assign' v ("_." ++ v)
            def (o:p) = imprt (if isOp o then "$op['" ++ o:p ++ "']" else deprime (o:p))
            named = if null vs then "" else "\nvar " ++ intercalate ", " (map def vs) ++ ";"
  where
    include = "\nvar _ = Elm." ++ modul ++ parens "elm" ++ ";" ++ setup modul
    setup moduleName = " var " ++ concatMap (++";") (defs ++ [assign' moduleName "_"])
        where
          defs = map (\n -> assign' n (n ++ "||{}")) (subnames moduleName)
          subnames = map (intercalate ".") . tail . inits . init . split
          split names = case go [] names of
                          (name, []) -> [name]
                          (name, ns) -> name : split ns
          go name str = case str of
                          '.':rest -> (reverse name, rest)
                          c:rest   -> go (c:name) rest
                          []       -> (reverse name, [])



stmtsToJS :: (Data t, Data v) => [Declaration t v] -> String
stmtsToJS stmts = run $ do program <- mapM toJS (sortBy cmpStmt stmts)
                           return (concat program)
    where
      cmpStmt s1 s2 = compare (valueOf s1) (valueOf s2)
      valueOf s = case s of
                    Datatype _ _ _             -> 1
                    ImportEvent _ _ _ _        -> 2
                    Definition (FnDef f [] _)  ->
                        if derename f == "main" then 5 else 4
                    Definition (TypeAnnotation _ _)  -> 0
                    Definition _               -> 3
                    ExportEvent _ _ _          -> 6
                    TypeAlias _ _ _            -> 0

class ToJS a where
  toJS :: a -> Unique String

instance (Data t, Data v) => ToJS (Def t v) where
  toJS (FnDef x [] e) = assign x `liftM` toJS' e
  toJS (FnDef f as e) = (assign f . wrapper . func) `liftM` toJS' e
      where
        func body = jsFunc (intercalate ", " as) (ret body)
        wrapper e | length as == 1 = e
                  | otherwise = 'F' : show (length as) ++ parens e
  toJS (OpDef op a1 a2 e) =
      do body <- toJS' e                    
         let func = "F2" ++ parens (jsFunc (a1 ++ ", " ++ a2) (ret body))
         return (globalAssign ("$op['" ++ op ++ "']") func)
  toJS (TypeAnnotation _ _) = return ""


instance (Data t, Data v) => ToJS (Declaration t v) where
  toJS stmt =
    case stmt of
      Definition d -> toJS d
      Datatype _ _ tcs -> concat `liftM` mapM (toJS . (`asTypeOf` stmt) . toDef) tcs
          where
            toDef (name,args) =
                    let vars = map (('a':) . show) [1..length args] in
                    Definition . FnDef name vars . none $
                    Data (derename name) (map (none . Var) vars)
      ImportEvent js base elm _ ->
        do v <- toJS' base
           return $ concat [ "\nvar " ++ elm ++ "=Elm.Signal(elm).constant(" ++ v ++ ");"
                           , "\ndocument.addEventListener('", js
                           , "_' + elm.id, function(e) { elm.notify(", elm
                           , ".id, e.value); });" ]
      ExportEvent js elm _ ->
        return $ concat [ "\nlift(function(v) { "
                        , "var e = document.createEvent('Event');"
                        , "e.initEvent('", js, "_' + elm.id, true, true);"
                        , "e.value = v;"
                        , "document.dispatchEvent(e); return v; })(", elm, ");" ]
      TypeAlias n _ t -> return ""

toJS' :: (Data t, Data v) => LExpr t v -> Unique String
toJS' (L txt span expr) =
    case expr of
      MultiIf ps -> multiIfToJS span ps
      Case e cases -> caseToJS span e cases
      _ -> toJS expr

remove x e = "_N.remove('" ++ x ++ "', " ++ e ++ ")"
addField x v e = "_N.insert('" ++ x ++ "', " ++ v ++ ", " ++ e ++ ")"
setField fs e = "_N.replace(" ++ jsList (map f fs) ++ ", " ++ e ++ ")"
    where f (x,v) = "['" ++ x ++ "'," ++ v ++ "]"
access x e = e ++ "." ++ x
makeRecord kvs = record `liftM` collect kvs
  where
    combine r (k,v) = Map.insertWith (++) k v r
    collect = liftM (foldl' combine Map.empty) . mapM prep
    prep (k, as, e@(L t s _)) =
        do v <- toJS' (foldr (\x e -> L t s $ Lambda x e) e as)
           return (k,[v])
    fields fs =
        brackets ("\n  "++intercalate ",\n  " (map (\(k,v) -> k++":"++v) fs))
    hidden = fields . map (second jsList) .
             filter (not . null . snd) . Map.toList . Map.map tail
    record kvs = fields . (("_", hidden kvs) :) . Map.toList . Map.map head $ kvs


instance ToJS Literal where
 toJS lit =
  case lit of
    Chr c -> return $ quoted [c]
    Str s -> return $ "_str" ++ parens (quoted s)
    IntNum   n -> return $ show n
    FloatNum n -> return $ show n
    Boolean  b -> return $ if b then "true" else "false"


instance (Data t, Data v) => ToJS (Expr t v) where
 toJS expr =
  case expr of
    Var x -> return x
    Literal lit -> toJS lit
    Range lo hi -> jsRange `liftM` toJS' lo `ap` toJS' hi
    Access e x -> access x `liftM` toJS' e
    Remove e x -> remove x `liftM` toJS' e
    Insert e x v -> addField x `liftM` toJS' v `ap` toJS' e
    Modify e fs -> do fs' <- (mapM (\(x,v) -> (,) x `liftM` toJS' v) fs)
                      setField fs' `liftM` toJS' e
    Record fs -> makeRecord fs
    Binop op e1 e2 -> binop op `liftM` toJS' e1 `ap` toJS' e2

    Lambda v e -> liftM (jsFunc v . ret) (toJS' e)

    App e1 e2 -> jsApp e1 e2
    Let defs e -> jsLet defs e

    ExplicitList es ->
        do es' <- mapM toJS' es
           return $ "_J.toList" ++ parens (jsList es')

    Data name es ->
        do fs <- mapM toJS' es
           return $ case name of
              "[]" -> jsNil
              "::" -> jsCons (head fs) ((head . tail) fs)
              _ -> jsObj $ ("ctor:" ++ show name) : fields
                   where fields = zipWith (\n e -> "_" ++ show n ++ ":" ++ e) [0..] fs

    Markdown doc -> return $ "text('" ++ pad ++ md ++ pad ++ "')"
        where pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
              md = formatMarkdown $ Pan.writeHtmlString Pan.def doc

jsApp e1 e2 =
    do f  <- toJS' func
       as <- mapM toJS' args
       return $ case as of
                  [a] -> f ++ parens a
                  _   -> "A" ++ show (length as) ++ parens (intercalate ", " (f:as))
  where
    (func, args) = go [e2] e1
    go args e =
       case e of
         (L _ _ (App e1 e2)) -> go (e2 : args) e1
         _ -> (e, args)

formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

multiIfToJS span ps =
    case last ps of
      (L _ _ (Var "otherwise"), e) -> toJS' e >>= \b -> format b (init ps)
      _ -> format ("_E.If" ++ parens (quoted (show span))) ps
  where
    format base ps =
        foldr (\c e -> parens $ c ++ " : " ++ e) base `liftM` mapM f ps
    f (b,e) = do b' <- toJS' b
                 e' <- toJS' e
                 return (b' ++ " ? " ++ e')

jsLet defs e' = do ds <- jsDefs defs
                   e <- toJS' e'
                   return $ jsFunc "" (concat ds ++ ret e) ++ "()"
  where 
    jsDefs defs = mapM toJS (sortBy f defs)
    f a b = compare (valueOf a) (valueOf b)
    valueOf (FnDef _ [] _) = 2
    valueOf (FnDef _ _ _) = 1
    valueOf (OpDef _ _ _ _)  = 1
    valueOf (TypeAnnotation _ _) = 0

caseToJS span e ps = do
  match <- caseToMatch ps
  e' <- toJS' e
  let (match',stmt) = case (match,e) of
        (Match name _ _, L _ _ (Var x)) -> (matchSubst [(name,x)] match, "")
        (Match name _ _, _)             -> (match, assign name e')
        _                               -> (match, "")
  matches <- matchToJS span match'
  return $ "function(){ " ++ stmt ++ matches ++ " }()"

matchToJS span match =
  case match of
    Match name clauses def ->
        do cases <- concat `liftM` mapM (clauseToJS span name) clauses
           finally <- matchToJS span def
           let isLiteral p = case p of
                               Clause (Right _) _ _ -> True
                               _ -> False
               access = if any isLiteral clauses then "" else ".ctor"
           return $ concat [ "\nswitch (", name, access, ") {"
                           , indent cases, "\n}", finally ]
    Fail -> return ("_E.Case" ++ parens (quoted (show span)))
    Break -> return "break;"
    Other e -> ret `liftM` toJS' e
    Seq ms -> concat `liftM` mapM (matchToJS span) (dropEnd [] ms)
        where
          dropEnd acc [] = acc
          dropEnd acc (m:ms) =
              case m of
                Other _ -> acc ++ [m]
                _ -> dropEnd (acc ++ [m]) ms

clauseToJS span var (Clause value vars e) = do
  let vars' = map (\n -> var ++ "._" ++ show n) [0..]
  s <- matchToJS span $ matchSubst (zip vars vars') e
  return $ concat [ "\ncase ", case value of
                                 Left name -> quoted name
                                 Right (Boolean True)  -> "true"
                                 Right (Boolean False) -> "false"
                                 Right lit -> show lit
                  , ":", indent s ]

jsNil         = "_L.Nil"
jsCons  e1 e2 = "_L.Cons(" ++ e1 ++ "," ++ e2 ++ ")"
jsRange e1 e2 = "_L.range" ++ parens (e1 ++ "," ++ e2)
jsCompare e1 e2 op = parens ("_N.cmp(" ++ e1 ++ "," ++ e2 ++ ").ctor" ++ op)

binop (o:p) e1 e2
    | isAlpha o || '_' == o = (o:p) ++ parens e1 ++ parens e2
    | otherwise =
        let ops = ["+","-","*","/","&&","||"] in
        case o:p of
          "::" -> jsCons e1 e2
          "++" -> "_L.append" ++ parens (e1 ++ "," ++ e2)
          "<|" -> e1 ++ parens e2
          "|>" -> e2 ++ parens e1
          "."  -> jsFunc "x" . ret $ e1 ++ parens (e2 ++ parens "x")
          "^"  -> "Math.pow(" ++ e1 ++ "," ++ e2 ++ ")"
          "==" -> "_N.eq(" ++ e1 ++ "," ++ e2 ++ ")"
          "/=" -> "!_N.eq(" ++ e1 ++ "," ++ e2 ++ ")"
          "<"  -> jsCompare e1 e2 "==='LT'"
          ">"  -> jsCompare e1 e2 "==='GT'"
          "<=" -> jsCompare e1 e2 "!=='GT'"
          ">=" -> jsCompare e1 e2 "!=='LT'"
          "<~" -> "A2(lift," ++ e1 ++ "," ++ e2 ++ ")"
          "~"  -> "A3(lift2,F2(function(f,x){return f(x)}),"++e1++","++e2++")"
          _  | elem (o:p) ops -> parens (e1 ++ (o:p) ++ e2)
             | otherwise      -> concat [ "$op['", o:p, "']"
                                        , parens e1, parens e2 ]