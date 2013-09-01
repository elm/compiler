module Generate.JavaScript where

import Control.Arrow ((***))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Pandoc as Pan

import Generate.Cases
import SourceSyntax.Everything
import SourceSyntax.Location
import qualified Transform.SortDefinitions as SD
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Parse.Helpers (jsReserveds)

makeSafe :: String -> String
makeSafe = dereserve . deprime
  where
    deprime = map (\c -> if c == '\'' then '$' else c)
    dereserve x = case Set.member x jsReserveds of
                    False -> x
                    True  -> "$" ++ x

split = go []
  where
    go vars str =
        case break (=='.') str of
          (x,'.':rest) -> go (vars ++ [x]) rest
          (x,[]) -> vars ++ [x]

var name = Id () (makeSafe name)
ref name = VarRef () (var name)
prop name = PropId () (var name)
f <| x = CallExpr () f [x]
args ==> e = FuncExpr () Nothing (map var args) [ ReturnStmt () (Just e) ]
function args stmts = FuncExpr () Nothing (map var args) stmts
call = CallExpr ()
string = StringLit ()

dotSep (x:xs) = foldl (DotRef ()) (ref x) (map var xs)
get = dotSep . split

varDecl x expr =
    VarDecl () (var x) (Just expr)

include alias moduleName =
    varDecl alias (get moduleName <| ref "elm")

internalImports name =
    VarDeclStmt () 
    [ varDecl "N" (get "Elm.Native")
    , include "_N" "N.Utils"
    , include "_L" "N.List"
    , include "_E" "N.Error"
    , include "_J" "N.JavaScript"
    , varDecl "_str" (get "_J.toString")
    , varDecl "$moduleName" (string name)
    ]

literal lit =
  case lit of
    Chr c -> string [c]
    Str s -> ref "_str" <| string s
    IntNum   n -> IntLit () n
    FloatNum n -> NumLit () n
    Boolean  b -> BoolLit () b

expression :: LExpr () () -> Expression ()
expression (L span expr) =
    case expr of
      Var x -> ref x
      Literal lit -> literal lit
      Range lo hi -> get "_L.range" `call` [expression lo, expression hi]

      Access e x -> DotRef () (expression e) (var x)
      Remove e x -> get "_N.remove" `call` [string x, expression e]
      Insert e x v -> get "_N.insert" `call` [string x, expression v, expression e]
      Modify e fs ->
          let modify (f,v) = ArrayLit () [string f, expression v]
          in  get "_N.replace" `call` [ArrayLit () (map modify fs), expression e]

      Record fields -> ObjectLit () $ (PropId () (var "_"), hidden) : visible
        where
          combine r (k,v) = Map.insertWith (++) k [v] r
          fieldMap = List.foldl' combine Map.empty fields
          hidden = ObjectLit () . map ((PropId () . var) *** (ArrayLit () . map expression)) .
                   Map.toList . Map.filter (not . null) $ Map.map tail fieldMap
          visible = map ((PropId () . var) *** expression) . Map.toList $ Map.map head fieldMap

      Binop op e1 e2 -> binop span op e1 e2

      Lambda p e@(L s _) -> fastFunc
          where
            fastFunc
                | length args < 2 || length args > 9 =
                    foldr (==>) (expression body) (map (:[]) args)
                | otherwise =
                    ref ("F" ++ show (length args)) <| (args ==> expression body)

            (args, body) = foldr depattern ([], innerBody) (zip patterns [0..])

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

      Let defs e -> function [] stmts `call` []
          where
            (defs',e') = SD.flattenLets defs e 
            stmts = concatMap definition defs' ++ [ ReturnStmt () (Just (expression e')) ]

      MultiIf branches ->
          case last branches of
            (L _ (Var "otherwise"), e) -> ifs (init branches) (expression e)
            _ -> ifs branches (get "_E.If" `call` [ ref "$moduleName", string (show span) ])
          where
            ifs branches finally = foldr iff finally branches
            iff (if', then') else' = CondExpr () (expression if') (expression then') else'

      Case e cases -> function [] (stmt ++ match span revisedMatch) `call` []
          where
            (tempVar,initialMatch) = caseToMatch cases
            (revisedMatch, stmt) =
                case e of
                  L _ (Var x) -> (matchSubst [(tempVar,x)] initialMatch, [])
                  _ -> (initialMatch, [VarDeclStmt () [varDecl tempVar (expression e)]])

      ExplicitList es ->
          get "_J.toList" <| ArrayLit () (map expression es)

      Data name es ->
          ObjectLit () (ctor : fields)
        where
          ctor = (prop "ctor", string (makeSafe name))
          fields = zipWith (\n e -> (prop ("_" ++ show n), expression e)) [0..] es

      Markdown doc -> get "Text.text" <| string (pad ++ md ++ pad)
          where pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
                md = Pan.writeHtmlString Pan.def doc


definition def =
  case def of
    TypeAnnotation _ _ -> []
    Def pattern expr@(L span _) ->
      let assign x = varDecl x (expression expr) in
      case pattern of
        PVar x
            | isOp x ->
                let op = LBracket () (ref "_op") (string x) in
                [ ExprStmt () (AssignExpr () OpAssign op (expression expr)) ]
            | otherwise ->
                [ VarDeclStmt () [ assign x ] ]

        PRecord fields -> [ VarDeclStmt () (assign "$" : map setField fields) ]
            where
              setField f = varDecl f (dotSep ["$",f])

        PData name patterns | vars /= Nothing ->
            case vars of
              Just vs -> [ VarDeclStmt () (setup (zipWith decl vs [0..])) ]
            where
              vars = getVars patterns
              getVars patterns =
                  case patterns of
                    PVar x : rest -> (x:) `fmap` getVars rest
                    [] -> Just []
                    _ -> Nothing

              decl x n = varDecl x (dotSep ["$","_" ++ show n])
              setup vars
                  | isTuple name = assign "$" : vars
                  | otherwise = safeAssign : vars

              safeAssign = varDecl "$" (CondExpr () if' (expression expr) exception)
              if' = InfixExpr () OpStrictEq (get "$.ctor") (string name)
              exception = get "_E.Case" `call` [ref "$moduleName", string (show span)]

        _ -> VarDeclStmt () [assign "$"] : concatMap toDef vars
            where
              vars = Set.toList $ SD.boundVars pattern
              mkVar = L span . Var
              toDef y = definition $
                        Def (PVar y) (L span $ Case (mkVar "$") [(pattern, mkVar y)])

match span mtch =
  case mtch of
    Match name clauses mtch' -> SwitchStmt () (access name) clauses' : match span mtch'
        where
          clauses' = map (clause span name) clauses
          isLiteral p = case p of
                          Clause (Right _) _ _ -> True
                          _ -> False
          access name = if any isLiteral clauses then ref name else dotSep [name,"ctor"]

    Fail -> [ ExprStmt () (get "_E.Case" `call` [ref "$moduleName", string (show span)]) ]
    Break -> []
    Other e -> [ ReturnStmt () (Just $ expression e) ]
    Seq ms -> concatMap (match span) (dropEnd [] ms)
        where
          dropEnd acc [] = acc
          dropEnd acc (m:ms) =
              case m of
                Other _ -> acc ++ [m]
                _ -> dropEnd (acc ++ [m]) ms

clause span variable (Clause value vars mtch) =
    CaseClause () pattern stmt
  where
    vars' = map (\n -> variable ++ "._" ++ show n) [0..]
    stmt = match span $ matchSubst (zip vars vars') mtch
    pattern = case value of
                Right (Boolean b)  -> BoolLit () b
                Right lit -> literal lit
                Left name -> string $ case List.elemIndices '.' name of
                                        [] -> name
                                        is -> drop (last is + 1) name

jsModule :: MetadataModule () () -> String -- [Statement ()]
jsModule modul = show . prettyPrint $ setup (Just "Elm") (names modul) ++
                               [ assign ("Elm" : names modul) (function ["elm"] programStmts) ]
  where
    thisModule = dotSep ("elm" : names modul)
    programStmts =
        concat [ setup (Just "elm") (names modul)
               , [ IfSingleStmt () thisModule (ReturnStmt () (Just thisModule)) ]
               , [ internalImports (List.intercalate "." (names modul)) ]
               , concatMap jsImport (imports modul)
               , concatMap importEvent (foreignImports modul)
               , [ assign ["_op"] (ObjectLit () []) ]
               , concatMap definition . fst . SD.flattenLets [] $ program modul
               , map exportEvent $ foreignExports modul
               , [ jsExports ]
               , [ ReturnStmt () (Just thisModule) ]
               ]

    jsExports = assign ("elm" : names modul) (ObjectLit () exs)
        where
          exs = map entry . filter (not . isOp) $ "_op" : exports modul
          entry x = (PropId () (var x), ref x)
          
    assign path expr =
             case path of
               [x] -> VarDeclStmt () [ varDecl x expr ]
               _   -> ExprStmt () $
                      AssignExpr () OpAssign (LDot () (dotSep (init path)) (last path)) expr

    jsImport (modul,_) = setup Nothing path ++ [ assign path (dotSep ("Elm" : path) <| ref "elm") ]
        where
          path = split modul

    setup namespace path = map create paths
        where
          create name = assign name (InfixExpr () OpLOr (dotSep name) (ObjectLit () []))
          paths = case namespace of
                    Nothing -> tail . init $ List.inits path
                    Just nmspc -> drop 2 . init . List.inits $ nmspc : path

    addId js = InfixExpr () OpAdd (string (js++"_")) (get "elm.id")

    importEvent (js,base,elm,_) =
        [ VarDeclStmt () [ varDecl elm $ get "Signal.constant" <| expression base ]
        , ExprStmt () $
            get "document.addEventListener" `call`
                  [ addId js
                  , function ["e"]
                        [ ExprStmt () $ get "elm.notify" `call` [dotSep [elm,"id"], get "e.value"] ]
                  ]
        ]

    exportEvent (js,elm,_) =
        ExprStmt () $
        ref "A2" `call`
                [ get "Signal.lift"
                , function ["v"]
                      [ VarDeclStmt () [varDecl "e" $ get "document.createEvent" <| string "Event"]
                      , ExprStmt () $
                            get "e.initEvent" `call` [ addId js, BoolLit () True, BoolLit () True ]
                      , ExprStmt () $ AssignExpr () OpAssign (LDot () (ref "e") "value") (ref "v")
                      , ExprStmt () $ get "document.dispatchEvent" <| ref "e"
                      , ReturnStmt () (Just $ ref "v")
                      ]
                , ref elm ]


binop span op e1 e2 =
    case op of
      "Basics.."  -> ["$"] ==> foldr (<|) (ref "$") (map expression (e1 : collect [] e2))
      "Basics.<|" -> foldr (<|) (expression e2) (map expression (collect [] e1))
      "::"        -> expression (L span (Data "::" [e1,e2]))
      "List.++"   -> get "_L.append" `call` [js1, js2]
      _           -> case Map.lookup op opDict of
                       Just f -> f js1 js2
                       Nothing -> ref "A2" `call` [ func, js1, js2 ]
  where
    collect es e =
        case e of
          L _ (Binop op e1 e2) | op == "Basics.." -> collect (es ++ [e1]) e2
          _ -> es ++ [e]

    js1 = expression e1
    js2 = expression e2

    func | isOp operator = BracketRef () (dotSep (init parts ++ ["_op"])) (string op)
         | otherwise     = dotSep parts
        where
          parts = split op
          operator = last parts

    opDict = Map.fromList (infixOps ++ specialOps)

    specialOp str func = ("Basics." ++ str, func)
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
        [ specialOp "^"   $ \a b -> get "Math.pow" `call` [a,b]
        , specialOp "|>"  $ flip (<|)
        , specialOp "=="  $ \a b -> get "_N.eq" `call` [a,b]
        , specialOp "/="  $ \a b -> PrefixExpr () PrefixLNot (get "_N.eq" `call` [a,b])
        , specialOp "<"   $ cmp OpLT 0
        , specialOp ">"   $ cmp OpGT 0
        , specialOp "<="  $ cmp OpLT 1
        , specialOp ">="  $ cmp OpGT (-1)
        , specialOp "div" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b = InfixExpr () op (get "_N.cmp" `call` [a,b]) (IntLit () n)