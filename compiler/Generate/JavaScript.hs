module Generate.JavaScript (showErr, jsModule) where

import Control.Arrow (first,second)
import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (partitionEithers)
import qualified Text.Pandoc as Pan

import Unique
import Generate.Cases
import SourceSyntax.Everything hiding (parens)
import SourceSyntax.Location as Loc
import qualified Transform.SortDefinitions as SD

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
commaSep = List.intercalate ", "
dotSep = List.intercalate "."
jsObj = brackets . commaSep
jsList ss = "["++ List.intercalate "," ss ++"]"
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

jsModule :: MetadataModule t v -> String
jsModule modul =
    run $ do
      body <- toJS . fst . SD.flattenLets [] $ program modul
      foreignImport <- mapM importEvent (foreignImports modul)
      return $ concat [ setup ("Elm": names modul)
                      , globalAssign ("Elm." ++ modName)
                                     (jsFunc "elm" $ makeProgram body foreignImport) ]
  where
    modName  = dotSep (names modul)
    makeProgram body foreignImport =
        concat [ "\nvar " ++ usefulFuncs ++ ";"
               , assign "_op" "{}"
               , concatMap jsImport (imports modul)
               , concat foreignImport
               , concatMap exportEvent $ foreignExports modul
               , body
               , assign "_" "{}"
               , concatMap jsExport (exports modul)
               , setup ("elm" : names modul)
               , ret (assign' ("elm." ++ modName) "_")
               ]
    setup names = concatMap (\n -> globalAssign n $ n ++ "||{}") .
                  map dotSep . drop 2 . List.inits $ init names
    usefulFuncs = commaSep (map (uncurry assign') internalImports)

    jsExport x = 
        if isOp x then "\n_._op['" ++ x ++ "'] = _op['" ++ x ++ "'];"
                  else "\n_." ++ x ++ " = " ++ x ++ ";"
    
    importEvent (js,base,elm,_) =
        do v <- toJS' base
           return $ concat [ "\nvar " ++ elm ++ "=Elm.Signal(elm).constant(" ++ v ++ ");"
                           , "\ndocument.addEventListener('", js
                           , "_' + elm.id, function(e) { elm.notify(", elm
                           , ".id, e.value); });" ]

    exportEvent (js,elm,_) =
        concat [ "\nlift(function(v) { "
               , "var e = document.createEvent('Event');"
               , "e.initEvent('", js, "_' + elm.id, true, true);"
               , "e.value = v;"
               , "document.dispatchEvent(e); return v; })(", elm, ");" ]

jsImport (modul, how) =
    case how of
      As name -> assign name ("Elm." ++ modul ++ parens "elm")
      Hiding vs -> include ++ " var hiding=" ++ (jsObj $ map (++":1") vs) ++
                   "; for(var k in _){if(k in hiding)continue;" ++
                   "eval('var '+k+'=_[\"'+k+'\"]')}"
      Importing vs -> include ++ named
          where 
            imprt v = assign' v ("_." ++ v)
            def x = imprt $ if isOp x then "_op['" ++ x ++ "']" else deprime x
            named = if null vs then "" else "\nvar " ++ commaSep (map def vs) ++ ";"
  where
    include = "\nvar _ = Elm." ++ modul ++ parens "elm" ++ ";" ++ setup modul
    setup moduleName = " var " ++ concatMap (++";") (defs ++ [assign' moduleName "_"])
        where
          defs = map (\n -> assign' n (n ++ "||{}")) (subnames moduleName)
          subnames = map dotSep . tail . List.inits . init . split
          split names = case go [] names of
                          (name, []) -> [name]
                          (name, ns) -> name : split ns
          go name str = case str of
                          '.':rest -> (reverse name, rest)
                          c:rest   -> go (c:name) rest
                          []       -> (reverse name, [])


class ToJS a where
  toJS :: a -> Unique String


instance ToJS (Def t v) where

  -- TODO: Make this handle patterns besides plain variables
  toJS (Def (PVar x) e)
      | isOp x = globalAssign ("_op['" ++ x ++ "']")  `liftM` toJS' e
      | otherwise = assign x `liftM` toJS' e

  toJS (Def pattern e) =
      do n <- guid
         let x = "_" ++ show n
             var = Loc.none . Var
             toDef y = Def (PVar y) (Loc.none $ Case (var x) [(pattern, var y)])
         stmt <- assign x `liftM` toJS' e
         vars <- toJS . map toDef . Set.toList $ SD.boundVars pattern
         return (stmt ++ vars)

  toJS (TypeAnnotation _ _) = return ""


instance ToJS a => ToJS [a] where
  toJS xs = concat `liftM` mapM toJS xs

toJS' :: LExpr t v -> Unique String
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
    collect = liftM (List.foldl' combine Map.empty) . mapM prep
    prep (k, e) =
        do v <- toJS' e
           return (k,[v])
    fields fs =
        brackets ("\n  "++List.intercalate ",\n  " (map (\(k,v) -> k++":"++v) fs))
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


instance ToJS (Expr t v) where
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

    Lambda p e -> liftM (jsFunc (commaSep args) . ret) (toJS' body)
        where
          (args, body) = foldr depattern ([], innerBody) (zip patterns [1..])

          depattern (pattern,n) (args, body) =
            case pattern of
              PVar x -> (x:args, body)
              _ -> let arg = "arg" ++ show n
                   in  (arg:args, Loc.none (Case (Loc.none (Var arg)) [(pattern, body)]))

          (patterns, innerBody) = collect [p] e

          collect patterns lexpr@(L a b expr) =
            case expr of
              Lambda p e -> collect (p:patterns) e
              _ -> (patterns, lexpr)

    App e1 e2 -> jsApp e1 e2
    Let defs e -> jsLet $ SD.flattenLets defs e 

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
                  _   -> "A" ++ show (length as) ++ parens (commaSep (f:as))
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

jsLet (defs,e') = do ds <- mapM toJS defs
                     e <- toJS' e'
                     return $ jsFunc "" (concat ds ++ ret e) ++ "()"

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

-- todo: this is incorrect. Operators come in prefixed by their module now.
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
             | otherwise      -> concat [ "_op['", o:p, "']"
                                        , parens e1, parens e2 ]