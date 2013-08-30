module Generate.JavaScript (jsModule) where

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
import SourceSyntax.Everything
import SourceSyntax.Location
import qualified Transform.SortDefinitions as SD
import Transform.Variable (makeVarsSafe)

indent = concatMap f
    where f '\n' = "\n  "
          f c = [c]

internalImports name =
    [ ("N" , "Elm.Native"),
      ("_N", "N.Utils(elm)"),
      ("_L", "N.List(elm)"),
      ("_E", "N.Error(elm)"),
      ("_J", "N.JavaScript(elm)"),
      ("_str", "_J.toString"),
      ("$moduleName", quoted name)
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
jsModule unsafeModule =
  run $ do
    
    body <- toJS . fst . SD.flattenLets [] $ program modul
    foreignImport <- mapM importEvent (foreignImports modul)
    return $ concat [ setup ("Elm" : names modul)
                    , globalAssign ("Elm." ++ modName)
                      (jsFunc "elm" $ makeProgram body foreignImport) ]
  where
    modul = makeVarsSafe unsafeModule
    modName  = dotSep (names modul)
    makeProgram body foreignImport =
        concat [ setup ("elm" : names modul)
               , "\nif (elm." ++ modName ++ ") return elm." ++ modName ++ ";"
               , "\nvar " ++ usefulFuncs ++ ";"
               , concatMap jsImport (imports modul)
               , concat foreignImport
               , assign "_op" "{}"
               , body
               , concatMap exportEvent $ foreignExports modul
               , jsExports
               ]
    setup names = concatMap (\n -> globalAssign n $ n ++ " || {}") .
                  map dotSep . drop 2 . List.inits $ init names
    usefulFuncs = commaSep (map (uncurry assign') (internalImports modName))

    jsExports = ret (assign' ("elm." ++ modName) (brackets exs))
        where
          exs = indent . commaSep . concatMap pair $ "_op" : exports modul
          pair x | isOp x = []
                 | otherwise = ["\n" ++ x ++ " : " ++ x]


    importEvent (js,base,elm,_) =
        do v <- toJS' base
           return $ concat [ "\nvar " ++ elm ++ "=Signal.constant(" ++ v ++ ");"
                           , "\ndocument.addEventListener('", js
                           , "_' + elm.id, function(e) { elm.notify(", elm
                           , ".id, e.value); });" ]

    exportEvent (js,elm,_) =
        concat [ "\nA2( Signal.lift, function(v) { "
               , "var e = document.createEvent('Event');"
               , "e.initEvent('", js, "_' + elm.id, true, true);"
               , "e.value = v;"
               , "document.dispatchEvent(e); return v; }, ", elm, ");" ]

jsImport (modul, method) =
    concat $ zipWith3 (\s n v -> s ++ assign' n v ++ ";") starters subnames values
  where
    starters = "\nvar " : repeat "\n"
    values = map (\name -> name ++ " || {}") (init subnames) ++
             ["Elm." ++ modul ++ parens "elm"]
    subnames = map dotSep . tail . List.inits $ split modul

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

  toJS (Def pattern e@(L s _)) =
      do n <- guid
         let x = "_" ++ show n
             var = L s . Var
             toDef y = Def (PVar y) (L s $ Case (var x) [(pattern, var y)])
         stmt <- assign x `liftM` toJS' e
         vars <- toJS . map toDef . Set.toList $ SD.boundVars pattern
         return (stmt ++ vars)

  toJS (TypeAnnotation _ _) = return ""


instance ToJS a => ToJS [a] where
  toJS xs = concat `liftM` mapM toJS xs

toJS' :: LExpr t v -> Unique String
toJS' (L span expr) =
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
           return (k, [v])
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
    Binop op e1 e2 -> binop op e1 e2

    Lambda p e@(L s _) -> liftM fastFunc (toJS' body)
        where
          fastFunc body
              | length args < 2 || length args > 9 =
                  foldr (\arg bod -> jsFunc arg (ret bod)) body args
              | otherwise =
                  "F" ++ show (length args) ++ parens (jsFunc (commaSep args) (ret body))

          (args, body) = first reverse $ foldr depattern ([], innerBody) (zip patterns [1..])

          depattern (pattern,n) (args, body) =
            case pattern of
              PVar x -> (x : args, body)
              _ -> let arg = "arg" ++ show n
                   in  (arg:args, L s (Case (L s (Var arg)) [(pattern, body)]))

          (patterns, innerBody) = collect [p] e

          collect patterns lexpr@(L _ expr) =
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

    Markdown doc -> return $ "Text.text('" ++ pad ++ md ++ pad ++ "')"
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
         (L _ (App e1 e2)) -> go (e2 : args) e1
         _ -> (e, args)

formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

multiIfToJS span ps =
    case last ps of
      (L _ (Var "otherwise"), e) -> toJS' e >>= \b -> format b (init ps)
      _ -> format ("_E.If" ++ parens ("$moduleName," ++ quoted (show span))) ps
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
  (tempVar,match) <- caseToMatch ps
  e' <- toJS' e
  let (match',stmt) = case e of
        L _ (Var x) -> (matchSubst [(tempVar,x)] match, "")
        _           -> (match, assign tempVar e')
  matches <- matchToJS span match'
  return $ jsFunc "" (stmt ++ matches) ++ "()"

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
    Fail -> return ("_E.Case" ++ parens ("$moduleName," ++ quoted (show span)))
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
  pattern <- case value of
               Right (Boolean True)  -> return "true"
               Right (Boolean False) -> return "false"
               Right lit -> toJS lit
               Left name -> return . quoted $ case List.elemIndices '.' name of
                                                [] -> name
                                                is -> drop (last is + 1) name
  return $ concat [ "\ncase ", pattern, ":", indent s ]

jsNil         = "_L.Nil"
jsCons  e1 e2 = "_L.Cons(" ++ e1 ++ "," ++ e2 ++ ")"
jsRange e1 e2 = "_L.range" ++ parens (e1 ++ "," ++ e2)
jsCompare e1 e2 op = parens ("_N.cmp(" ++ e1 ++ "," ++ e2 ++ ")" ++ op)


binop op e1 e2
    | op == "Basics.." =
        do ss <- mapM toJS' (e1 : collect [] e2)
           return . jsFunc "$" . ret $ apply "$" ss
    | op == "Basics.<|" =
        do arg <- toJS' e2
           funcs <- mapM toJS' (collect [] e1)
           return (apply arg funcs)
    | otherwise =
        binopNormal op `liftM` toJS' e1 `ap` toJS' e2
  where
    apply = foldr (\f arg-> f ++ parens arg)

    collect es e =
        case e of
          L _ (Binop op e1 e2) | op == "Basics.." -> collect (es ++ [e1]) e2
          _ -> es ++ [e]

binopNormal op s1 s2 =
  case Map.lookup op opDict of
    Just e -> e
    Nothing -> "A2" ++ parens (commaSep [ prefix ++ func, s1, s2 ])
  where
    func | isOp op'  = "_op['" ++ op' ++ "']"
         | otherwise = op'
    (prefix, op') = case List.elemIndices '.' op of
                      [] -> ("", op)
                      xs -> splitAt (last xs + 1) op

    opDict = Map.fromList $ basics ++
             [ ("::", jsCons s1 s2)
             , ("List.++", "_L.append" ++ parens (s1 ++ "," ++ s2)) ]

    ops = pow : map (\op -> (op, parens (s1 ++ op ++ s2))) ["+","-","*","/","&&","||"]
        where pow = ("^" , "Math.pow(" ++ s1 ++ "," ++ s2 ++ ")")

    basics = ops ++ map (\(op,e) -> ("Basics." ++ op, e))
             (ops ++ [ ("<|", s1 ++ parens s2)
                     , ("|>", s2 ++ parens s1)
                     , ("==", "_N.eq(" ++ s1 ++ "," ++ s2 ++ ")")
                     , ("/=", "!_N.eq(" ++ s1 ++ "," ++ s2 ++ ")")
                     , ("<" , jsCompare s1 s2 "<0")
                     , (">" , jsCompare s1 s2 ">0")
                     , ("<=", jsCompare s1 s2 "<1")
                     , (">=", jsCompare s1 s2 ">-1")
                     , ("div", parens (s1 ++ "/" ++ s2 ++ "|0"))
                     ])
