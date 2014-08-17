{-# OPTIONS_GHC -W #-}
module Generate.JavaScript (generate) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,(***))
import Control.Monad.State (State, evalState, foldM, forM)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.ECMAScript3.PrettyPrint (prettyPrint)
import qualified Language.ECMAScript3.Syntax as JS

import Generate.JavaScript.Helpers as Help
import qualified Generate.Cases as Case
import qualified Generate.JavaScript.Ports as Port
import qualified Generate.JavaScript.Variable as Var
import qualified Generate.Markdown as MD

import AST.Annotation (Annotated(A), Region)
import qualified AST.Expression.General as Expr
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Helpers as Help
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var

varDecl :: String -> JS.Expression () -> JS.VarDecl ()
varDecl x expr =
    JS.VarDecl () (var x) (Just expr)

internalImports :: String -> [JS.VarDecl ()]
internalImports name =
    [ varDecl "_N" (obj ["Elm","Native"])
    , include "_U" "Utils"
    , include "_L" "List" 
    , include "_A" "Array"
    , include "_E" "Error"
    , varDecl Help.localModuleName (string name)
    ]
    where
      include :: String -> String -> JS.VarDecl ()
      include alias modul =
          varDecl alias (obj ["_N", modul, "make"] <| ref "_elm")

_Utils :: String -> JS.Expression ()
_Utils x = obj ["_U", x]

_List :: String -> JS.Expression ()
_List x = obj ["_L", x]

literal :: L.Literal -> JS.Expression ()
literal lit =
  case lit of
    L.Chr c -> _Utils "chr" <| string [c]
    L.Str s -> string s
    L.IntNum   n -> JS.IntLit () n
    L.FloatNum n -> JS.NumLit () n
    L.Boolean  b -> JS.BoolLit () b

expression :: Canonical.Expr -> State Int (JS.Expression ())
expression (A region expr) =
    case expr of
      Expr.Var var -> return $ Var.canonical var

      Expr.Literal lit -> return $ literal lit

      Expr.Range lo hi ->
          do lo' <- expression lo
             hi' <- expression hi
             return $ _List "range" `call` [lo',hi']

      Expr.Access e x ->
          do e' <- expression e
             return $ JS.DotRef () e' (var x)

      Expr.Remove e x ->
          do e' <- expression e
             return $ _Utils "remove" `call` [string x, e']

      Expr.Insert e x v ->
          do v' <- expression v
             e' <- expression e
             return $ _Utils "insert" `call` [string x, v', e']

      Expr.Modify e fs ->
          do e' <- expression e
             fs' <- forM fs $ \(f,v) -> do
                      v' <- expression v
                      return $ JS.ArrayLit () [string f, v']
             return $ _Utils "replace" `call` [JS.ArrayLit () fs', e']

      Expr.Record fields ->
          do fields' <- forM fields $ \(f,e) -> do
                          (,) f <$> expression e
             let fieldMap = List.foldl' combine Map.empty fields'
             return $ JS.ObjectLit () $ (prop "_", hidden fieldMap) : visible fieldMap
          where
            combine r (k,v) = Map.insertWith (++) k [v] r
            hidden fs = JS.ObjectLit () . map (prop *** JS.ArrayLit ()) .
                        Map.toList . Map.filter (not . null) $ Map.map tail fs
            visible fs = map (first prop) . Map.toList $ Map.map head fs

      Expr.Binop op e1 e2 -> binop region op e1 e2

      Expr.Lambda p e@(A ann _) ->
          do (args, body) <- foldM depattern ([], innerBody) (reverse patterns)
             body' <- expression body
             return $ case length args < 2 || length args > 9 of
                        True  -> foldr (==>) body' (map (:[]) args)
                        False -> ref ("F" ++ show (length args)) <| (args ==> body')
          where
            depattern (args, body) pattern =
                case pattern of
                  P.Var x -> return (args ++ [ Var.varName x ], body)
                  _ -> do arg <- Case.newVar
                          return ( args ++ [arg]
                                 , A ann (Expr.Case (A ann (Expr.localVar arg)) [(pattern, body)]))

            (patterns, innerBody) = collect [p] e

            collect patterns lexpr@(A _ expr) =
                case expr of
                  Expr.Lambda p e -> collect (p:patterns) e
                  _ -> (patterns, lexpr)

      Expr.App e1 e2 ->
          do func' <- expression func
             args' <- mapM expression args
             return $ case args' of
                        [arg] -> func' <| arg
                        _ | length args' <= 9 -> ref aN `call` (func':args')
                          | otherwise         -> foldl1 (<|) (func':args')
          where
            aN = "A" ++ show (length args)
            (func, args) = getArgs e1 [e2]
            getArgs func args =
                case func of
                  (A _ (Expr.App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)

      Expr.Let defs e ->
          do let (defs',e') = flattenLets defs e 
             stmts <- concat <$> mapM definition defs'
             exp <- expression e'
             return $ function [] (stmts ++ [ ret exp ]) `call` []

      Expr.MultiIf branches ->
        do branches' <- forM branches $ \(b,e) -> (,) <$> expression b <*> expression e
           return $ case last branches of
             (A _ (Expr.Var (Var.Canonical (Var.Module "Basics") "otherwise")), _) ->
                 safeIfs branches'
             (A _ (Expr.Literal (L.Boolean True)), _) -> safeIfs branches'
             _ -> ifs branches' (throw "If" region)
        where
          safeIfs branches = ifs (init branches) (snd (last branches))
          ifs branches finally = foldr iff finally branches
          iff (if', then') else' = JS.CondExpr () if' then' else'

      Expr.Case e cases ->
          do (tempVar,initialMatch) <- Case.toMatch cases
             (revisedMatch, stmt) <-
                 case e of
                   A _ (Expr.Var (Var.Canonical Var.Local x)) ->
                       return (Case.matchSubst [(tempVar,x)] initialMatch, [])
                   _ ->
                       do e' <- expression e
                          return (initialMatch, [JS.VarDeclStmt () [varDecl tempVar e']])
             match' <- match region revisedMatch
             return (function [] (stmt ++ match') `call` [])

      Expr.ExplicitList es ->
          do es' <- mapM expression es
             return $ _List "fromArray" <| JS.ArrayLit () es'

      Expr.Data name es ->
          do es' <- mapM expression es
             return $ JS.ObjectLit () (ctor : fields es')
          where
            ctor = (prop "ctor", string name)
            fields = zipWith (\n e -> (prop ("_" ++ show n), e)) [0..]

      Expr.Markdown uid doc es ->
          do es' <- mapM expression es
             return $ Var.value "Text" "markdown" `call` (string md : string uid : es')
          where
            pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
            md = pad ++ MD.toHtml doc ++ pad

      Expr.GLShader _uid src _tipe ->
        return $ JS.ObjectLit () [(JS.PropString () "src", literal (L.Str src))]
          
      Expr.PortIn name tipe ->
          return $ Var.value "Native.Ports" "portIn" `call`
                     [ string name, Port.incoming tipe ]

      Expr.PortOut name tipe value ->
          do value' <- expression value
             return $ Var.value "Native.Ports" "portOut" `call`
                        [ string name, Port.outgoing tipe, value' ]

definition :: Canonical.Def -> State Int [JS.Statement ()]
definition (Canonical.Definition pattern expr@(A region _) _) = do
  expr' <- expression expr
  let assign x = varDecl x expr'
  case pattern of
    P.Var x
        | Help.isOp x ->
            let op = JS.LBracket () (ref "_op") (string x) in
            return [ JS.ExprStmt () $ JS.AssignExpr () JS.OpAssign op expr' ]
        | otherwise ->
            return [ JS.VarDeclStmt () [ assign (Var.varName x) ] ]

    P.Record fields ->
        let setField f = varDecl f (obj ["$",f]) in
        return [ JS.VarDeclStmt () (assign "$" : map setField fields) ]

    P.Data (Var.Canonical _ name) patterns | vars /= Nothing ->
        return [ JS.VarDeclStmt () (setup (zipWith decl (maybe [] id vars) [0..])) ]
        where
          vars = getVars patterns
          getVars patterns =
              case patterns of
                P.Var x : rest -> (Var.varName x :) `fmap` getVars rest
                [] -> Just []
                _ -> Nothing

          decl x n = varDecl x (obj ["$","_" ++ show n])
          setup vars
              | Help.isTuple name = assign "$" : vars
              | otherwise = assign "_raw" : safeAssign : vars

          safeAssign = varDecl "$" (JS.CondExpr () if' (ref "_raw") exception)
          if' = JS.InfixExpr () JS.OpStrictEq (obj ["_raw","ctor"]) (string name)
          exception = Help.throw "Case" region

    _ ->
        do defs' <- concat <$> mapM toDef vars
           return (JS.VarDeclStmt () [assign "_"] : defs')
        where
          vars = P.boundVarList pattern
          mkVar = A region . Expr.localVar
          toDef y = let expr =  A region $ Expr.Case (mkVar "_") [(pattern, mkVar y)]
                    in  definition $ Canonical.Definition (P.Var y) expr Nothing

match :: Region -> Case.Match -> State Int [JS.Statement ()]
match region mtch =
  case mtch of
    Case.Match name clauses mtch' ->
        do (isChars, clauses') <- unzip <$> mapM (clause region name) clauses
           mtch'' <- match region mtch'
           return (JS.SwitchStmt () (format isChars (access name)) clauses' : mtch'')
        where
          isLiteral p = case p of
                          Case.Clause (Right _) _ _ -> True
                          _ -> False

          access name
              | any isLiteral clauses = obj [name]
              | otherwise = obj (Help.splitDots name ++ ["ctor"])

          format isChars e
              | or isChars = JS.InfixExpr () JS.OpAdd e (string "")
              | otherwise = e

    Case.Fail ->
        return [ JS.ExprStmt () (Help.throw "Case" region) ]

    Case.Break -> return [JS.BreakStmt () Nothing]

    Case.Other e ->
        do e' <- expression e
           return [ ret e' ]

    Case.Seq ms -> concat <$> mapM (match region) (dropEnd [] ms)
        where
          dropEnd acc [] = acc
          dropEnd acc (m:ms) =
              case m of
                Case.Other _ -> acc ++ [m]
                _ -> dropEnd (acc ++ [m]) ms

clause :: Region -> String -> Case.Clause -> State Int (Bool, JS.CaseClause ())
clause region variable (Case.Clause value vars mtch) =
    (,) isChar . JS.CaseClause () pattern <$> match region (Case.matchSubst (zip vars vars') mtch)
  where
    vars' = map (\n -> variable ++ "._" ++ show n) [0..]
    (isChar, pattern) =
        case value of
          Right (L.Chr c) -> (True, string [c])
          _ -> (,) False $ case value of
                             Right (L.Boolean b) -> JS.BoolLit () b
                             Right lit -> literal lit
                             Left (Var.Canonical _ name) ->
                                 string name

flattenLets :: [Canonical.Def] -> Canonical.Expr -> ([Canonical.Def], Canonical.Expr)
flattenLets defs lexpr@(A _ expr) =
    case expr of
      Expr.Let ds body -> flattenLets (defs ++ ds) body
      _ -> (defs, lexpr)

generate :: Module.CanonicalModule -> String 
generate modul =
    show . prettyPrint $ setup "Elm" (names ++ ["make"]) ++
             [ assign ("Elm" : names ++ ["make"]) (function ["_elm"] programStmts) ]
  where
    names :: [String]
    names = Module.names modul

    thisModule :: JS.Expression ()
    thisModule = obj ("_elm" : names ++ ["values"])

    programStmts :: [JS.Statement ()]
    programStmts =
        concat
        [ [ JS.ExprStmt () (string "use strict") ]
        , setup "_elm" (names ++ ["values"])
        , [ JS.IfSingleStmt () thisModule (ret thisModule) ]
        , [ JS.VarDeclStmt () localVars ]
        , body
        , [ jsExports ]
        , [ ret thisModule ]
        ]

    localVars :: [JS.VarDecl ()]
    localVars = varDecl "_op" (JS.ObjectLit () []) :
                internalImports (Module.getName modul) ++
                explicitImports
      where
        explicitImports :: [JS.VarDecl ()]
        explicitImports =
            map jsImport . Set.toList . Set.fromList . map fst $ Module.imports modul

        jsImport :: String -> JS.VarDecl ()
        jsImport name =
            varDecl (Var.moduleName name) $
                obj ("Elm" : Help.splitDots name ++ ["make"]) <| ref "_elm"

    body :: [JS.Statement ()]
    body = concat $ evalState defs 0
      where
        defs = mapM definition . fst . flattenLets [] $ Module.program (Module.body modul)

    setup namespace path = map create paths
      where
        create name = assign name (JS.InfixExpr () JS.OpLOr (obj name) (JS.ObjectLit () []))
        paths = drop 2 . init . List.inits $ namespace : path

    jsExports = assign ("_elm" : names ++ ["values"]) (JS.ObjectLit () exs)
        where
          exs = map entry $ "_op" : concatMap extract (Module.exports modul)
          entry x = (prop x, ref x)
          extract value =
              case value of
                Var.Alias _ -> []
                Var.Value x | Help.isOp x -> []
                            | otherwise   -> [Var.varName x]
                Var.ADT _ (Var.Listing ctors _) -> map Var.varName ctors
          
    assign path expr =
             case path of
               [x] -> JS.VarDeclStmt () [ varDecl x expr ]
               _   -> JS.ExprStmt () $
                      JS.AssignExpr () JS.OpAssign (JS.LDot () (obj (init path)) (last path)) expr

binop :: Region -> Var.Canonical -> Canonical.Expr -> Canonical.Expr
      -> State Int (JS.Expression ())
binop region func@(Var.Canonical home op) e1 e2 =
    case (home, op) of
      (Var.Module "Basics", ".") ->
          do es <- mapM expression (e1 : collect [] e2)
             return $ ["$"] ==> foldr (<|) (ref "$") es

      (Var.Module "Basics", "<|") ->
          do e2' <- expression e2
             es <- mapM expression (collect [] e1)
             return $ foldr (<|) e2' es

      (Var.Module "List", "++") ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ _List "append" `call` [e1', e2']

      (Var.BuiltIn, "::") ->
          expression (A region (Expr.Data "::" [e1,e2]))

      (Var.Module "Basics", _) ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ case Map.lookup op basicOps of
                        Just f -> f e1' e2'
                        Nothing -> ref "A2" `call` [ Var.canonical func, e1', e2' ]

      _ ->
          do e1' <- expression e1
             e2' <- expression e2
             return (ref "A2" `call` [ Var.canonical func, e1', e2' ])

  where
    collect es e =
        case e of
          A _ (Expr.Binop (Var.Canonical (Var.Module "Basics") ".") e1 e2) ->
              collect (es ++ [e1]) e2
          _ -> es ++ [e]

    basicOps = Map.fromList (infixOps ++ specialOps)

    infixOps =
        let infixOp str op = (str, JS.InfixExpr () op) in
        [ infixOp "+"  JS.OpAdd
        , infixOp "-"  JS.OpSub
        , infixOp "*"  JS.OpMul
        , infixOp "/"  JS.OpDiv
        , infixOp "&&" JS.OpLAnd
        , infixOp "||" JS.OpLOr
        ]

    specialOps =
        [ (,) "^"   $ \a b -> obj ["Math","pow"] `call` [a,b]
        , (,) "|>"  $ flip (<|)
        , (,) "=="  $ \a b -> _Utils "eq" `call` [a,b]
        , (,) "/="  $ \a b -> JS.PrefixExpr () JS.PrefixLNot (_Utils "eq" `call` [a,b])
        , (,) "<"   $ cmp JS.OpLT 0
        , (,) ">"   $ cmp JS.OpGT 0
        , (,) "<="  $ cmp JS.OpLT 1
        , (,) ">="  $ cmp JS.OpGT (-1)
        , (,) "div" $ \a b -> JS.InfixExpr () JS.OpBOr (JS.InfixExpr () JS.OpDiv a b) (JS.IntLit () 0)
        ]

    cmp op n a b = JS.InfixExpr () op (_Utils "cmp" `call` [a,b]) (JS.IntLit () n)
