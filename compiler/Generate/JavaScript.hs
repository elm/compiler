module Generate.JavaScript where

import Control.Monad (liftM,(<=<),join,ap)
import Data.Char (isAlpha,isDigit)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Either (partitionEithers)


import Unique
import Generate.Cases
import SourceSyntax.Everything
import SourceSyntax.Location
import qualified Transform.SortDefinitions as SD
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Parse.Helpers (makeSafe, iParse)

import Parse.Expression

testExpr str = (prettyPrint . expression) `fmap` iParse expr "" str

var name = Id () (makeSafe name)
ref name = VarRef () (var name)
prop name = PropId () (var name)
f <| x = CallExpr () f [x]
args ==> e = FuncExpr () Nothing (map var args) [ ReturnStmt () (Just e) ]
call = CallExpr ()
string = StringLit ()

dotSep (x:xs) = foldl (DotRef ()) (ref x) (map var xs)

varDecl x expr =
    VarDecl () (var x) (Just expr)

include alias moduleName =
    varDecl alias (dotSep moduleName <| ref "elm")

internalImports name =
    VarDeclStmt () 
    [ varDecl "N" (dotSep ["Elm","Native"])
    , include "_N" ["N","Utils"]
    , include "_L" ["N","List"]
    , include "_E" ["N","Error"]
    , include "_J" ["N","JavaScript"]
    , varDecl "_str" (dotSep ["_J","toString"])
    , varDecl "$moduleName" (string name)
    ]

literal lit =
  case lit of
    Chr c -> string [c]
    Str s -> ref "_str" <| string s
    IntNum   n -> IntLit () n
--    FloatNum n -> NumLit () (float2Double n)  -- wrong!!!
    Boolean  b -> BoolLit () b

expression (L span expr) =
    case expr of
      Var x -> ref x
      Literal lit -> literal lit
      Range lo hi -> dotSep ["_L","range"] `call` [expression lo, expression hi]

      Access e x -> DotRef () (expression e) (var x)
      Remove e x -> dotSep ["_N","remove"] `call` [ref x, expression e]
      Insert e x v -> dotSep ["_N","insert"] `call` [ref x, expression v, expression e]
{--
      Modify e fs -> do fs' <- (mapM (\(x,v) -> (,) x `liftM` toJS' v) fs)
                        setField fs' `liftM` toJS' e
    Record fs -> makeRecord fs

addField x v e = "_N.insert('" ++ x ++ "', " ++ v ++ ", " ++ e ++ ")"
setField fs e = "_N.replace(" ++ jsList (map f fs) ++ ", " ++ e ++ ")"
    where f (x,v) = "['" ++ x ++ "'," ++ v ++ "]"
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
--}
      Binop op e1 e2 -> binop span op e1 e2

      Lambda p e@(L s _) -> fastFunc
          where
            fastFunc
                | length args < 2 || length args > 9 =
                    foldr (==>) (expression body) (map (:[]) args)
                | otherwise =
                    ref ("F" ++ show (length args)) <| (args ==> expression body)

            (args, body) = foldr depattern ([], innerBody) (zip patterns [1..])

            depattern (pattern,n) (args, body) =
                case pattern of
                  PVar x -> (args ++ [x], body)
                  _ -> let arg = "arg" ++ show n
                       in  (args ++ [arg], L s (Case (L s (Var arg)) [(pattern, body)]))

            (patterns, innerBody) = collect [p] e

            collect patterns lexpr@(L _ expr) =
                case expr of
                  Lambda p e -> collect (p:patterns) e
                  _ -> (patterns, lexpr)
--}
      App e1 e2 ->
          case args of
            [arg] -> func <| arg
            _   -> ref aN `call` (func:args)
          where
            aN = "A" ++ show (length args)
            (func, args) = getArgs e1 [e2]
            getArgs func args =
                case func of
                  (L _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (expression func, map expression args)
{--
      Let defs e -> jsLet $ SD.flattenLets defs e 
--}

      MultiIf branches ->
          case last branches of
            (L _ (Var "otherwise"), e) -> ifs (init branches) (expression e)
            _ -> ifs branches (dotSep ["_E","If"] `call` [ ref "$moduleName", string (show span) ])
          where
            ifs branches finally = foldr iff finally branches
            iff (if', then') else' = CondExpr () (expression if') (expression then') else'

      ExplicitList es ->
          dotSep ["_J","toList"] <| ArrayLit () (map expression es)

      Data name es ->
          ObjectLit () (ctor : fields)
        where
          ctor = (prop "ctor", string (makeSafe name))
          fields = zipWith (\n e -> (prop ("_" ++ show n), expression e)) [0..] es
{--
    Markdown doc -> return $ "Text.text('" ++ pad ++ md ++ pad ++ "')"
        where pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
              md = formatMarkdown $ Pan.writeHtmlString Pan.def doc
--}

{--

globalAssign n e = "\n" ++ assign' n e ++ ";"
assign' n e = n ++ " = " ++ e

jsModule :: MetadataModule t v -> String
jsModule modul =
    run $ do
      body <- toJS . fst . SD.flattenLets [] $ program modul
      foreignImport <- mapM importEvent (foreignImports modul)
      return $ concat [ setup ("Elm" : names modul)
                      , globalAssign ("Elm." ++ modName)
                                     (jsFunc "elm" $ makeProgram body foreignImport) ]
  where
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
      Case e cases -> caseToJS span e cases
      _ -> toJS expr



formatMarkdown = concatMap f
    where f '\'' = "\\'"
          f '\n' = "\\n"
          f '"'  = "\""
          f c = [c]

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

jsCons  e1 e2 = "_L.Cons(" ++ e1 ++ "," ++ e2 ++ ")"
jsCompare e1 e2 op = parens ("_N.cmp(" ++ e1 ++ "," ++ e2 ++ ")" ++ op)

--}
binop span op e1 e2 =
    case op of
      "Basics.."  -> ["$"] ==> foldr (<|) (ref "$") (map expression (e1 : collect [] e2))
      "Basics.<|" -> foldr (<|) (expression e2) (map expression (collect [] e1))
      "::"        -> expression (L span (Data "::" [e1,e2]))
      "List.++"   -> dotSep ["_L","append"] `call` [js1, js2]
      _           -> case Map.lookup op opDict of
                       Just f -> f js1 js2
                       Nothing -> ref "A2" `call` [ func [] op, js1, js2 ]
  where
    collect es e =
        case e of
          L _ (Binop op e1 e2) | op == "Basics.." -> collect (es ++ [e1]) e2
          _ -> es ++ [e]

    js1 = expression e1
    js2 = expression e2

    func vs str =
        case break (=='.') str of
          (x,'.':rest) -> func (vs ++ [x]) rest
          (op,[])
              | isOp op   -> BracketRef () (dotSep (vs ++ ["_op"])) (string op)
              | otherwise -> dotSep (vs ++ [op])

    opDict = Map.fromList (infixOps ++ specialOps)

    specialOp str func = ("Basic." ++ str, func)
    infixOp str op = specialOp str (InfixExpr () op)

    infixOps =
        [ infixOp "+"  OpAdd
        , infixOp "-"  OpSub
        , infixOp "*"  OpMul
        , infixOp "/"  OpDiv
        , infixOp "&&" OpLAnd
        , infixOp "||" OpLOr
        ]

    specialOps = 
        [ specialOp "^"   $ \a b -> dotSep ["Math","pow"] `call` [a,b]
        , specialOp "|>"  $ flip (<|)
        , specialOp "=="  $ \a b -> dotSep ["_N","eq"] `call` [a,b]
        , specialOp "/="  $ \a b -> PrefixExpr () PrefixLNot (dotSep ["_N","eq"] `call` [a,b])
        , specialOp "<"   $ cmp OpLT 0
        , specialOp ">"   $ cmp OpGT 0
        , specialOp "<="  $ cmp OpLT 1
        , specialOp ">="  $ cmp OpGT (-1)
        , specialOp "div" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b = InfixExpr () op (dotSep ["_N","cmp"] `call` [a,b]) (IntLit () n)
--}