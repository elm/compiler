module Generate.JavaScript where

import Control.Arrow (first,(***))
import Control.Applicative ((<$>),(<*>))
import Control.Monad.State
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
namedFunction name args e = FuncExpr () name (map var args) [ ReturnStmt () (Just e) ]
function args stmts = FuncExpr () Nothing (map var args) stmts
call = CallExpr ()
string = StringLit ()

dotSep (x:xs) = foldl (DotRef ()) (ref x) (map var xs)
obj = dotSep . split

varDecl x expr =
    VarDecl () (var x) (Just expr)

include alias moduleName =
    varDecl alias (obj moduleName <| ref "elm")

internalImports name =
    VarDeclStmt () 
    [ varDecl "N" (obj "Elm.Native")
    , include "_N" "N.Utils"
    , include "_L" "N.List"
    , include "_E" "N.Error"
    , include "_J" "N.JavaScript"
    , varDecl "_str" (obj "_J.toString")
    , varDecl "$moduleName" (string name)
    ]

literal lit =
  case lit of
    Chr c -> string [c]
    Str s -> ref "_str" <| string s
    IntNum   n -> IntLit () n
    FloatNum n -> NumLit () n
    Boolean  b -> BoolLit () b

expression :: LExpr () () -> State Int (Expression ())
expression (L span expr) =
    case expr of
      Var x -> return $ ref x
      Literal lit -> return $ literal lit

      Range lo hi ->
          do lo' <- expression lo
             hi' <- expression hi
             return $ obj "_L.range" `call` [lo',hi']

      Access e x ->
          do e' <- expression e
             return $ DotRef () e' (var x)

      Remove e x ->
          do e' <- expression e
             return $ obj "_N.remove" `call` [string x, e']

      Insert e x v ->
          do v' <- expression v
             e' <- expression e
             return $ obj "_N.insert" `call` [string x, v', e']

      Modify e fs ->
          do e' <- expression e
             fs' <- forM fs $ \(f,v) -> do
                      v' <- expression v
                      return $ ArrayLit () [string f, v']
             return $ obj "_N.replace" `call` [ArrayLit () fs', e']

      Record fields ->
          do fields' <- forM fields $ \(f,e) -> do
                          (,) f <$> expression e
             let fieldMap = List.foldl' combine Map.empty fields'
             return $ ObjectLit () $ (PropId () (var "_"), hidden fieldMap) : visible fieldMap
          where
            combine r (k,v) = Map.insertWith (++) k [v] r
            prop = PropId () . var
            hidden fs = ObjectLit () . map (prop *** ArrayLit ()) .
                        Map.toList . Map.filter (not . null) $ Map.map tail fs
            visible fs = map (first prop) . Map.toList $ Map.map head fs

      Binop op e1 e2 -> binop span op e1 e2

      Lambda pattern body -> lambda Nothing span pattern body

      App e1 e2 ->
          do func' <- expression func
             args' <- mapM expression args
             return $ case args' of
                        [arg] -> func' <| arg
                        _   -> ref aN `call` (func':args')
          where
            aN = "A" ++ show (length args)
            (func, args) = getArgs e1 [e2]
            getArgs func args =
                case func of
                  (L _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)

      Let defs e ->
          do let (defs',e') = SD.flattenLets defs e 
             stmts <- concat <$> mapM definition defs'
             exp <- expression e'
             return $ function [] (stmts ++ [ ReturnStmt () (Just exp) ]) `call` []

      MultiIf branches ->
          do branches' <- forM branches $ \(b,e) -> (,) <$> expression b <*> expression e
             return $ case last branches of
                        (L _ (Var "otherwise"), e) -> ifs (init branches') (snd (last branches'))
                        _ -> ifs branches'
                                 (obj "_E.If" `call` [ ref "$moduleName", string (show span) ])
          where
            ifs branches finally = foldr iff finally branches
            iff (if', then') else' = CondExpr () if' then' else'

      Case e cases ->
          do (tempVar,initialMatch) <- caseToMatch cases
             (revisedMatch, stmt) <-
                 case e of
                   L _ (Var x) -> return (matchSubst [(tempVar,x)] initialMatch, [])
                   _ -> do e' <- expression e
                           return (initialMatch, [VarDeclStmt () [varDecl tempVar e']])
             match' <- match span revisedMatch
             return (function [] (stmt ++ match') `call` [])

      ExplicitList es ->
          do es' <- mapM expression es
             return $ obj "_J.toList" <| ArrayLit () es'

      Data name es ->
          do es' <- mapM expression es
             return $ ObjectLit () (ctor : fields es')
          where
            ctor = (prop "ctor", string (makeSafe name))
            fields = zipWith (\n e -> (prop ("_" ++ show n), e)) [0..]

      Markdown doc _ -> return $ obj "Text.text" <| string (pad ++ md ++ pad)
          where pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
                md = Pan.writeHtmlString Pan.def doc

definition :: Def () () -> State Int [Statement ()]
definition def =
  case def of
    TypeAnnotation _ _ -> return []

    Def (PVar f) (L span (Lambda pattern body))
        | not (isOp f) ->
            do expr <- lambda (Just (var f)) span pattern body
               return [ VarDeclStmt () [ varDecl f expr ] ]

    Def pattern expr@(L span _) -> do
      expr' <- expression expr
      let assign x = varDecl x expr'
      case pattern of
        PVar x
          | isOp x ->
              let op = LBracket () (ref "_op") (string x) in
              return [ ExprStmt () $ AssignExpr () OpAssign op expr' ]
          | otherwise ->
              return [ VarDeclStmt () [ assign x ] ]

        PRecord fields ->
            let setField f = varDecl f (dotSep ["$",f]) in
            return [ VarDeclStmt () (assign "$" : map setField fields) ]

        PData name patterns | vars /= Nothing ->
            case vars of
              Just vs -> return [ VarDeclStmt () (setup (zipWith decl vs [0..])) ]
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

              safeAssign = varDecl "$" (CondExpr () if' expr' exception)
              if' = InfixExpr () OpStrictEq (obj "$.ctor") (string name)
              exception = obj "_E.Case" `call` [ref "$moduleName", string (show span)]

        _ ->
            do defs' <- concat <$> mapM toDef vars
               return (VarDeclStmt () [assign "$"] : defs')
            where
              vars = Set.toList $ SD.boundVars pattern
              mkVar = L span . Var
              toDef y = definition $
                        Def (PVar y) (L span $ Case (mkVar "$") [(pattern, mkVar y)])


lambda name span outerPattern outerBody =
    do body' <- expression body
       return $ case args of
                  [arg] -> namedFunction name args body'
                  arg:rest ->
                      case length args < 10 of
                        True  -> ref ("F" ++ show (length args)) <| (namedFunction name args body')
                        False -> namedFunction name [arg] (foldr (==>) body' (map (:[]) rest))
    where
      (args, body) = foldr depattern ([], innerBody) (zip patterns [0..])

      depattern (pattern,n) (args, body) =
          case pattern of
            PVar x -> (args ++ [x], body)
            _ -> let arg = "arg" ++ show n
                 in  (args ++ [arg], L span (Case (L span (Var arg)) [(pattern, body)]))

      (patterns, innerBody) = collect [outerPattern] outerBody

      collect patterns lexpr@(L _ expr) =
          case expr of
            Lambda p e -> collect (p:patterns) e
            _ -> (patterns, lexpr)

match span mtch =
  case mtch of
    Match name clauses mtch' ->
        do clauses' <- mapM (clause span name) clauses
           mtch'' <- match span mtch'
           return (SwitchStmt () (access name) clauses' : mtch'')
        where
          isLiteral p = case p of
                          Clause (Right _) _ _ -> True
                          _ -> False
          access name = if any isLiteral clauses then ref name else dotSep [name,"ctor"]

    Fail -> return [ ExprStmt () (obj "_E.Case" `call` [ref "$moduleName", string (show span)]) ]
    Break -> return []
    Other e ->
        do e' <- expression e
           return [ ReturnStmt () (Just e') ]
    Seq ms -> concat <$> mapM (match span) (dropEnd [] ms)
        where
          dropEnd acc [] = acc
          dropEnd acc (m:ms) =
              case m of
                Other _ -> acc ++ [m]
                _ -> dropEnd (acc ++ [m]) ms

clause span variable (Clause value vars mtch) =
    CaseClause () pattern <$> match span (matchSubst (zip vars vars') mtch)
  where
    vars' = map (\n -> variable ++ "._" ++ show n) [0..]
    pattern = case value of
                Right (Boolean b)  -> BoolLit () b
                Right lit -> literal lit
                Left name -> string $ case List.elemIndices '.' name of
                                        [] -> name
                                        is -> drop (last is + 1) name


jsModule :: MetadataModule () () -> String 
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
               , concat $ evalState (mapM definition . fst . SD.flattenLets [] $ program modul) 0
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

    addId js = InfixExpr () OpAdd (string (js++"_")) (obj "elm.id")

    importEvent (js,base,elm,_) =
        [ VarDeclStmt () [ varDecl elm $ obj "Signal.constant" <| evalState (expression base) 0 ]
        , ExprStmt () $
            obj "document.addEventListener" `call`
                  [ addId js
                  , function ["e"]
                        [ ExprStmt () $ obj "elm.notify" `call` [dotSep [elm,"id"], obj "e.value"] ]
                  ]
        ]

    exportEvent (js,elm,_) =
        ExprStmt () $
        ref "A2" `call`
                [ obj "Signal.lift"
                , function ["v"]
                      [ VarDeclStmt () [varDecl "e" $ obj "document.createEvent" <| string "Event"]
                      , ExprStmt () $
                            obj "e.initEvent" `call` [ addId js, BoolLit () True, BoolLit () True ]
                      , ExprStmt () $ AssignExpr () OpAssign (LDot () (ref "e") "value") (ref "v")
                      , ExprStmt () $ obj "document.dispatchEvent" <| ref "e"
                      , ReturnStmt () (Just $ ref "v")
                      ]
                , ref elm ]

binop span op e1 e2 =
    case op of
      "Basics.." ->
          do es <- mapM expression (e1 : collect [] e2)
             return $ ["$"] ==> foldr (<|) (ref "$") es
      "Basics.<|" ->
          do e2' <- expression e2
             es <- mapM expression (collect [] e1)
             return $ foldr (<|) e2' es
      "List.++" ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ obj "_L.append" `call` [e1', e2']
      "::" -> expression (L span (Data "::" [e1,e2]))
      _ ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ case Map.lookup op opDict of
                        Just f -> f e1' e2'
                        Nothing -> ref "A2" `call` [ func, e1', e2' ]
  where
    collect es e =
        case e of
          L _ (Binop op e1 e2) | op == "Basics.." -> collect (es ++ [e1]) e2
          _ -> es ++ [e]

    js1 = expression e1
    js2 = expression e2

    func | isOp operator = BracketRef () (dotSep (init parts ++ ["_op"])) (string operator)
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
        [ specialOp "^"   $ \a b -> obj "Math.pow" `call` [a,b]
        , specialOp "|>"  $ flip (<|)
        , specialOp "=="  $ \a b -> obj "_N.eq" `call` [a,b]
        , specialOp "/="  $ \a b -> PrefixExpr () PrefixLNot (obj "_N.eq" `call` [a,b])
        , specialOp "<"   $ cmp OpLT 0
        , specialOp ">"   $ cmp OpGT 0
        , specialOp "<="  $ cmp OpLT 1
        , specialOp ">="  $ cmp OpGT (-1)
        , specialOp "div" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b = InfixExpr () op (obj "_N.cmp" `call` [a,b]) (IntLit () n)
