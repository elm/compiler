module CompileToJS (showErr, jsModule) where

import Control.Arrow (first,second)
import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import Data.List (intercalate,sortBy,inits,foldl')
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import qualified Text.Pandoc as Pan

import Ast
import Context
import Rename (derename)
import Cases
import Guid
import LetBoundVars
import Parse.Library (isOp)
import Rename (deprime)
import Types.Types ( Type(RecordT) )

showErr :: String -> String
showErr err = mainEquals $ "Elm.Graphics.text(Elm.Text.monospace(" ++ msg ++ "))"
    where msg = show . concatMap (++"<br>") . lines $ err

indent = concatMap f
    where f '\n' = "\n "
          f c = [c]

parens s  = "(" ++ s ++ ")"
brackets s  = "{" ++ s ++ "}"
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
globalAssign n e = "\n" ++ assign' n e ++ ";"
assign' n e = n ++ " = " ++ e

jsModule (Module names exports imports stmts) =
  concat [ concatMap (\n -> globalAssign n $ n ++ " || {}") .
           map (intercalate ".") . drop 2 . inits $
           take (length modNames - 1) modNames
         , parens (jsFunc "" (defs ++ includes ++ body ++ globalAssign modName export) ++ "()")
         , mainEquals $ modName ++ ".main" ]
      where modNames = if null names then ["Elm", "Main"]
                                     else  "Elm" : names
            modName  = intercalate "." modNames
            includes = concatMap jsImport imports
            body = stmtsToJS stmts
            export = getExports exports stmts
            exps = if null exports then ["main"] else exports
            defs = assign "$op" "{}"

getExports names stmts = brackets . ("\n "++) $
                         intercalate ",\n " (op : map fnPair fns)
    where exNames n = either derename id n `elem` names
          exports | null names = concatMap get stmts
                  | otherwise  = filter exNames (concatMap get stmts)

          (fns,ops) = partitionEithers exports

          opPair op = "'" ++ op ++ "' : $op['" ++ op ++ "']"
          fnPair fn = let fn' = derename fn in fn' ++ ":" ++ fn

          op = ("$op : "++) . brackets . intercalate ", " $ map opPair ops

          get' (FnDef x _ _) = Left x
          get' (OpDef op _ _ _) = Right op
          get s = case s of Definition d        -> [ get' d ]
                            Datatype _ _ tcs    -> map (Left . fst) tcs
                            ImportEvent _ _ x _ -> [ Left x ]
                            ExportEvent _ _ _   -> []
                            TypeAlias _ _ _     -> []
                            TypeAnnotation _ _  -> []


jsImport (modul, how) =
    case how of
      As name -> assign name ("Elm." ++ modul)
      Importing vs ->
          assign modul ("Elm." ++ modul) ++ concatMap def vs
          where
            imprt asgn v = asgn v ("Elm." ++ modul ++ "." ++ v)
            def (o:p) =
                if isOp o then imprt globalAssign ("$op['" ++ o:p ++ "']")
                          else imprt assign (deprime (o:p))


stmtsToJS :: [Statement] -> String
stmtsToJS stmts = run $ do program <- mapM toJS (sortBy cmpStmt stmts)
                           return (vars ++ concat program)
    where
      vars = "\nvar " ++ intercalate "," (letBoundVars stmts) ++ ";"
      cmpStmt s1 s2 = compare (valueOf s1) (valueOf s2)
      valueOf s = case s of
                    Datatype _ _ _             -> 1
                    ImportEvent _ _ _ _        -> 2
                    Definition (FnDef f [] _)  ->
                        if derename f == "main" then 5 else 4
                    Definition _               -> 3
                    ExportEvent _ _ _          -> 6
                    TypeAlias _ _ _            -> 0
                    TypeAnnotation _ _         -> 0

class ToJS a where
  toJS :: a -> GuidCounter String

instance ToJS Def where
  toJS (FnDef x [] e) = assign' x `liftM` toJS' e
  toJS (FnDef f (a:as) e) =
      do body <- toJS' (foldr (\x e -> noContext (Lambda x e)) e as)
         return $ assign' f (jsFunc a (ret body))
  toJS (OpDef op a1 a2 e) =
      do body <- toJS' (foldr (\x e -> noContext (Lambda x e)) e [a1,a2])
         return $ concat [ "\n$op['", op, "'] = ", body, ";" ]

instance ToJS Statement where
  toJS stmt =
    case stmt of
      Definition d -> (\asgn -> "\n" ++ asgn ++ ";") `liftM` toJS d
      Datatype _ _ tcs -> concat `liftM` mapM (toJS . toDef) tcs
          where toDef (name,args) =
                    let vars = map (('a':) . show) [1..length args] in
                    Definition . FnDef name vars . noContext $
                    Data (derename name) (map (noContext . Var) vars)
      ImportEvent js base elm _ ->
        do v <- toJS' base
           return $ concat [ "\nvar " ++ elm ++ "=Elm.Signal.constant(" ++ v ++ ");"
                           , "\nValue.addListener(document, '" ++ js
                           , "', function(e) { Dispatcher.notify(" ++ elm
                           , ".id, e.value); });" ]
      ExportEvent js elm _ ->
        return $ concat [ "\nlift(function(v) { "
                        , "var e = document.createEvent('Event');"
                        , "e.initEvent('", js, "', true, true);"
                        , "e.value = v;"
                        , "document.dispatchEvent(e); return v; })(", elm, ");" ]
      TypeAnnotation _ _ -> return ""
      TypeAlias n _ t ->
          case t of
            RecordT kvs _ -> return $ "\nfunction" ++ args ++ brackets body
                where fs = Map.keys kvs
                      args = parens (intercalate "," fs)
                      body = ret . brackets . intercalate "," $
                             map (\k -> k ++ ":" ++ k) fs
            _ -> return ""

toJS' :: CExpr -> GuidCounter String
toJS' (C txt span expr) =
    case expr of
      MultiIf ps -> multiIfToJS span ps
      Case e cases -> caseToJS span e cases
      _ -> toJS expr

remove x e = "elmRecordRemove('" ++ x ++ "', " ++ e ++ ")"
addField x v e = "elmRecordInsert('" ++ x ++ "', " ++ v ++ ", " ++ e ++ ")"
setField fs e = "elmRecordReplace(" ++ jsList (map f fs) ++ ", " ++ e ++ ")"
    where f (x,v) = "['" ++ x ++ "'," ++ v ++ "]"
access x e = parens e ++ "." ++ x
makeRecord kvs = record `liftM` collect kvs
  where
    combine r (k,v) = Map.insertWith (++) k v r
    collect = liftM (foldl' combine Map.empty) . mapM prep
    prep (k, as, e@(C t s _)) =
        do v <- toJS' (foldr (\x e -> C t s $ Lambda x e) e as)
           return (k,[v])
    fields fs =
        brackets ("\n  "++intercalate ",\n  " (map (\(k,v) -> k++":"++v) fs))
    hidden = fields . map (second jsList) .
             filter (not . null . snd) . Map.toList . Map.map tail
    record kvs = fields . (("_", hidden kvs) :) . Map.toList . Map.map head $ kvs


instance ToJS Expr where
 toJS expr =
  case expr of
    IntNum n -> return $ show n ++ "|0"
    FloatNum n -> return $ show n
    Var x -> return $ x
    Chr c -> return $ quoted [c]
    Str s -> return $ "Value.str" ++ parens (quoted s)
    Boolean b -> return $ if b then "true" else "false"
    Range lo hi -> jsRange `liftM` toJS' lo `ap` toJS' hi
    Access e x -> access x `liftM` toJS' e
    Remove e x -> remove x `liftM` toJS' e
    Insert e x v -> addField x `liftM` toJS' v `ap` toJS' e
    Modify e fs -> do fs' <- (mapM (\(x,v) -> (,) x `liftM` toJS' v) fs)
                      setField fs' `liftM` toJS' e
    Record fs -> makeRecord fs
    Binop op e1 e2 -> binop op `liftM` toJS' e1 `ap` toJS' e2

    If eb et ef ->
        parens `liftM` (iff `liftM` toJS' eb `ap` toJS' et `ap` toJS' ef)

    Lambda v e -> liftM (jsFunc v . ret) (toJS' e)

    App (C _ _ (Var "toText")) (C _ _ (Str s)) ->
        return $ "toText" ++ parens (quoted s)

    App (C _ _ (Var "link")) (C _ _ (Str s)) ->
        return $ "link(" ++ quoted s ++ ")"

    App (C _ _ (Var "plainText")) (C _ _ (Str s)) ->
        return $ "plainText(" ++ quoted s ++ ")"

    App e1 e2 -> (++) `liftM` (toJS' e1) `ap` (parens `liftM` toJS' e2)
    Let defs e -> jsLet defs e
    Data name es ->
        do fs <- mapM toJS' es
           let fields = zipWith (\n e -> "_" ++ show n ++ ":" ++ e) [0..] fs
           return (brackets ("ctor:" ++ show name ++ concatMap (", "++) fields))

    Markdown doc -> return $ "text('" ++ pad ++ md ++ pad ++ "')"
        where pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
              md = formatMarkdown $ Pan.writeHtmlString Pan.def doc

formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

multiIfToJS span ps =
    case last ps of
      (C _ _ (Var "otherwise"), e) -> toJS' e >>= \b -> format b (init ps)
      _ -> format err ps
  where
    format base ps =
        foldr (\c e -> parens $ c ++ " : " ++ e) base `liftM` mapM f ps
    err = "throw new Error('Non-exhaustive multi-way-if expression (" ++
          show span ++ ")')"
    f (b,e) = do b' <- toJS' b
                 e' <- toJS' e
                 return (b' ++ " ? " ++ e')

jsLet defs e' = do ds <- jsDefs defs
                   e  <- toJS' e'
                   return $ parens (intercalate ", " ds ++ ", " ++ e)
  where 
    jsDefs defs = mapM toJS (sortBy f defs)
    f a b = compare (valueOf a) (valueOf b)
    valueOf (FnDef _ args _) = min 1 (length args)
    valueOf (OpDef _ _ _ _)  = 1

caseToJS span e ps = do
  match <- caseToMatch ps
  e' <- toJS' e
  (match',stmt) <- case (match,e) of
      (Match name _ _, C _ _ (Var x)) -> return (matchSubst [(name,x)] match, "")
      (Match name _ _, _)             -> return (match, assign name e')
      _                               -> liftM (\n -> (match, e')) guid
  matches <- matchToJS span match'
  return $ concat [ "function(){", stmt, matches, "}()" ]

matchToJS span (Match name clauses def) = do
  cases <- concat `liftM` mapM (clauseToJS span name) clauses
  finally <- matchToJS span def
  return $ concat [ "\nswitch(", name, ".ctor){", indent cases, "\n}", finally ]
matchToJS span Fail  =
    return ("\nthrow new Error(\"Non-exhaustive pattern match " ++
            "in case expression (" ++ show span ++ ")\");")
matchToJS span Break = return "break;"
matchToJS span (Other e) = ret `liftM` toJS' e
matchToJS span (Seq ms) = concat `liftM` mapM (matchToJS span) ms

clauseToJS span var (Clause name vars e) = do
  let vars' = map (\n -> var ++ "._" ++ show n) [ 1 .. length vars ]
  s <- matchToJS span $ matchSubst (zip vars vars') e
  return $ concat [ "\ncase ", quoted name, ":", s ]

jsNil         = "{ctor:'Nil'}"
jsCons  e1 e2 = "{ctor:'Cons',_0:" ++ e1 ++ ",_1:" ++ e2 ++ "}"
jsRange e1 e2 = "Elm.Native.List.range" ++ parens (e1 ++ "," ++ e2)

binop (o:p) e1 e2
    | isAlpha o || '_' == o = (o:p) ++ parens e1 ++ parens e2
    | otherwise =
        let ops = ["+","-","*","/","&&","||"] in
        case o:p of
          "::" -> jsCons e1 e2
          "++" -> append e1 e2
          "$"  -> e1 ++ parens e2
          "."  -> jsFunc "x" . ret $ e1 ++ parens (e2 ++ parens "x")
          "^"  -> "Math.pow(" ++ e1 ++ "," ++ e2 ++ ")"
          "==" -> "eq(" ++ e1 ++ "," ++ e2 ++ ")"
          "/=" -> "not(eq(" ++ e1 ++ "," ++ e2 ++ "))"
          "<"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ").ctor === 'LT')"
          ">"  -> "(compare(" ++ e1 ++ ")(" ++ e2 ++ ").ctor === 'GT')"
          "<=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++
                  e2 ++ ").ctor; return ord==='LT' || ord==='EQ'; }()"
          ">=" -> "function() { var ord = compare(" ++ e1 ++ ")(" ++
                  e2 ++ ").ctor; return ord==='GT' || ord==='EQ'; }()"
          "<~" -> "lift" ++ parens e1 ++ parens e2
          "~"  -> "lift2(function(f){return function(x){return f(x);};})" ++
                  parens e1 ++ parens e2
          _  | elem (o:p) ops -> parens (e1 ++ (o:p) ++ e2)
             | otherwise      -> concat [ "$op['", o:p, "']"
                                        , parens e1, parens e2 ]

append e1 e2 = "Value.append" ++ parens (e1 ++ "," ++ e2)
