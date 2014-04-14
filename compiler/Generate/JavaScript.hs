{-# OPTIONS_GHC -W #-}
module Generate.JavaScript (generate) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first,(***))
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.ECMAScript3.PrettyPrint
import Language.ECMAScript3.Syntax

import Generate.JavaScript.Helpers
import qualified Generate.Cases as Case
import qualified Generate.JavaScript.Ports as Port
import qualified Generate.Markdown as MD

import AST.Annotation
import AST.Module
import AST.Expression.General
import qualified AST.Expression.Canonical as Canonical
import qualified AST.Module as Module
import qualified AST.Helpers as Help
import AST.Literal
import qualified AST.Pattern as P
import AST.PrettyPrint (renderPretty)
import qualified AST.Variable as Var

varDecl :: String -> Expression () -> VarDecl ()
varDecl x expr =
    VarDecl () (var x) (Just expr)

include :: String -> String -> VarDecl ()
include alias moduleName =
    varDecl alias (obj (moduleName ++ ".make") <| ref "_elm")

internalImports :: String -> Statement ()
internalImports name =
    VarDeclStmt () 
    [ varDecl "_N" (obj "Elm.Native")
    , include "_U" "_N.Utils"
    , include "_L" "_N.List"
    , include "_E" "_N.Error"
    , varDecl "$moduleName" (string name)
    ]

literal :: Literal -> Expression ()
literal lit =
  case lit of
    Chr c -> obj "_U.chr" <| string [c]
    Str s -> string s
    IntNum   n -> IntLit () n
    FloatNum n -> NumLit () n
    Boolean  b -> BoolLit () b

canonical :: Var.Canonical -> Expression ()
canonical (Var.Canonical home name) =
  case Help.isOp name of
    True  -> BracketRef () (dotSep (home' ++ ["_op"])) (string name)
    False -> dotSep (home' ++ [map (swap '\'' '$') name])
  where
    swap from to c = if c == from then to else c

    home' =
      case home of
        Var.Local       -> []
        Var.BuiltIn     -> []
        Var.Module path -> [ map (swap '.' '$') path ]

expression :: Canonical.Expr -> State Int (Expression ())
expression (A region expr) =
    case expr of
      Var var -> return $ canonical var

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
             return $ obj "_U.remove" `call` [string x, e']

      Insert e x v ->
          do v' <- expression v
             e' <- expression e
             return $ obj "_U.insert" `call` [string x, v', e']

      Modify e fs ->
          do e' <- expression e
             fs' <- forM fs $ \(f,v) -> do
                      v' <- expression v
                      return $ ArrayLit () [string f, v']
             return $ obj "_U.replace" `call` [ArrayLit () fs', e']

      Record fields ->
          do fields' <- forM fields $ \(f,e) -> do
                          (,) f <$> expression e
             let fieldMap = List.foldl' combine Map.empty fields'
             return $ ObjectLit () $ (prop "_", hidden fieldMap) : visible fieldMap
          where
            combine r (k,v) = Map.insertWith (++) k [v] r
            hidden fs = ObjectLit () . map (prop *** ArrayLit ()) .
                        Map.toList . Map.filter (not . null) $ Map.map tail fs
            visible fs = map (first prop) . Map.toList $ Map.map head fs

      Binop op e1 e2 -> binop region op e1 e2

      Lambda p e@(A ann _) ->
          do (args, body) <- foldM depattern ([], innerBody) (reverse patterns)
             body' <- expression body
             return $ case length args < 2 || length args > 9 of
                        True  -> foldr (==>) body' (map (:[]) args)
                        False -> ref ("F" ++ show (length args)) <| (args ==> body')
          where
            depattern (args, body) pattern =
                case pattern of
                  P.Var x -> return (args ++ [x], body)
                  _ -> do arg <- Case.newVar
                          return ( args ++ [arg]
                                 , A ann (Case (A ann (localVar arg)) [(pattern, body)]))

            (patterns, innerBody) = collect [p] e

            collect patterns lexpr@(A _ expr) =
                case expr of
                  Lambda p e -> collect (p:patterns) e
                  _ -> (patterns, lexpr)

      App e1 e2 ->
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
                  (A _ (App f arg)) -> getArgs f (arg : args)
                  _ -> (func, args)

      Let defs e ->
          do let (defs',e') = flattenLets defs e 
             stmts <- concat <$> mapM definition defs'
             exp <- expression e'
             return $ function [] (stmts ++ [ ret exp ]) `call` []

      MultiIf branches ->
        do branches' <- forM branches $ \(b,e) -> (,) <$> expression b <*> expression e
           return $ case last branches of
             (A _ (Var (Var.Canonical (Var.Module "Basics") "otherwise")), _) ->
                 safeIfs branches'
             (A _ (Literal (Boolean True)), _) -> safeIfs branches'
             _ -> ifs branches' (obj "_E.If" `call` [ ref "$moduleName", string (renderPretty region) ])
        where
          safeIfs branches = ifs (init branches) (snd (last branches))
          ifs branches finally = foldr iff finally branches
          iff (if', then') else' = CondExpr () if' then' else'

      Case e cases ->
          do (tempVar,initialMatch) <- Case.toMatch cases
             (revisedMatch, stmt) <-
                 case e of
                   A _ (Var (Var.Canonical Var.Local x)) ->
                       return (Case.matchSubst [(tempVar,x)] initialMatch, [])
                   _ ->
                       do e' <- expression e
                          return (initialMatch, [VarDeclStmt () [varDecl tempVar e']])
             match' <- match region revisedMatch
             return (function [] (stmt ++ match') `call` [])

      ExplicitList es ->
          do es' <- mapM expression es
             return $ obj "_L.fromArray" <| ArrayLit () es'

      Data name es ->
          do es' <- mapM expression es
             return $ ObjectLit () (ctor : fields es')
          where
            ctor = (prop "ctor", string name)
            fields = zipWith (\n e -> (prop ("_" ++ show n), e)) [0..]

      Markdown uid doc es ->
          do es' <- mapM expression es
             return $ obj "Text.markdown" `call` (string md : string uid : es')
          where
            pad = "<div style=\"height:0;width:0;\">&nbsp;</div>"
            md = pad ++ MD.toHtml doc ++ pad

      PortIn name tipe ->
          return $ obj "Native.Ports.portIn" `call` [ string name, Port.incoming tipe ]

      PortOut name tipe value ->
          do value' <- expression value
             return $ obj "Native.Ports.portOut" `call`
                        [ string name, Port.outgoing tipe, value' ]

definition :: Canonical.Def -> State Int [Statement ()]
definition (Canonical.Definition pattern expr@(A region _) _) = do
  expr' <- expression expr
  let assign x = varDecl x expr'
  case pattern of
    P.Var x
        | Help.isOp x ->
            let op = LBracket () (ref "_op") (string x) in
            return [ ExprStmt () $ AssignExpr () OpAssign op expr' ]
        | otherwise ->
            return [ VarDeclStmt () [ assign x ] ]

    P.Record fields ->
        let setField f = varDecl f (dotSep ["$",f]) in
        return [ VarDeclStmt () (assign "$" : map setField fields) ]

    P.Data (Var.Canonical _ name) patterns | vars /= Nothing ->
        return [ VarDeclStmt () (setup (zipWith decl (maybe [] id vars) [0..])) ]
        where
          vars = getVars patterns
          getVars patterns =
              case patterns of
                P.Var x : rest -> (x:) `fmap` getVars rest
                [] -> Just []
                _ -> Nothing

          decl x n = varDecl x (dotSep ["$","_" ++ show n])
          setup vars
              | Help.isTuple name = assign "$" : vars
              | otherwise = assign "_raw" : safeAssign : vars

          safeAssign = varDecl "$" (CondExpr () if' (obj "_raw") exception)
          if' = InfixExpr () OpStrictEq (obj "_raw.ctor") (string name)
          exception = obj "_E.Case" `call` [ref "$moduleName", string (renderPretty region)]

    _ ->
        do defs' <- concat <$> mapM toDef vars
           return (VarDeclStmt () [assign "_"] : defs')
        where
          vars = P.boundVarList pattern
          mkVar = A region . localVar
          toDef y = let expr =  A region $ Case (mkVar "_") [(pattern, mkVar y)]
                    in  definition $ Canonical.Definition (P.Var y) expr Nothing

match :: Region -> Case.Match -> State Int [Statement ()]
match region mtch =
  case mtch of
    Case.Match name clauses mtch' ->
        do (isChars, clauses') <- unzip <$> mapM (clause region name) clauses
           mtch'' <- match region mtch'
           return (SwitchStmt () (format isChars (access name)) clauses' : mtch'')
        where
          isLiteral p = case p of
                          Case.Clause (Right _) _ _ -> True
                          _ -> False

          access name
              | any isLiteral clauses = obj name
              | otherwise = dotSep (split name ++ ["ctor"])

          format isChars e
              | or isChars = InfixExpr () OpAdd e (string "")
              | otherwise = e

    Case.Fail ->
        return [ ExprStmt () (obj "_E.Case" `call` [ref "$moduleName", string (renderPretty region)]) ]

    Case.Break -> return [BreakStmt () Nothing]

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

clause :: Region -> String -> Case.Clause -> State Int (Bool, CaseClause ())
clause region variable (Case.Clause value vars mtch) =
    (,) isChar . CaseClause () pattern <$> match region (Case.matchSubst (zip vars vars') mtch)
  where
    vars' = map (\n -> variable ++ "._" ++ show n) [0..]
    (isChar, pattern) =
        case value of
          Right (Chr c) -> (True, string [c])
          _ -> (,) False $ case value of
                             Right (Boolean b) -> BoolLit () b
                             Right lit -> literal lit
                             Left (Var.Canonical _ name) ->
                                 string name

flattenLets :: [Canonical.Def] -> Canonical.Expr -> ([Canonical.Def], Canonical.Expr)
flattenLets defs lexpr@(A _ expr) =
    case expr of
      Let ds body -> flattenLets (defs ++ ds) body
      _ -> (defs, lexpr)

generate :: CanonicalModule -> String 
generate modul =
    show . prettyPrint $ setup "Elm" (names ++ ["make"]) ++
             [ assign ("Elm" : names ++ ["make"]) (function ["_elm"] programStmts) ]
  where
    names = Module.names modul

    thisModule = dotSep ("_elm" : names ++ ["values"])
    programStmts =
        concat
        [ [ ExprStmt () $ string "use strict" ]
        , setup "_elm" (names ++ ["values"])
        , [ IfSingleStmt () thisModule (ret thisModule) ]
        , [ internalImports (List.intercalate "." names) ]
        , map jsImport . Set.toList . Set.fromList . map fst $ Module.imports modul
        , [ assign ["_op"] (ObjectLit () []) ]
        , concat $ evalState (mapM definition . fst . flattenLets [] $ Module.program (Module.body modul)) 0
        , [ jsExports ]
        , [ ret thisModule ]
        ]

    jsExports = assign ("_elm" : names ++ ["values"]) (ObjectLit () exs)
        where
          exs = map entry $ "_op" : concatMap extract (exports modul)
          entry x = (prop x, ref x)
          extract value =
              case value of
                Var.Alias _ -> []
                Var.Value x | Help.isOp x -> []
                            | otherwise   -> [x]
                Var.ADT _ (Var.Listing ctors _) -> ctors
          
    assign path expr =
             case path of
               [x] -> VarDeclStmt () [ varDecl x expr ]
               _   -> ExprStmt () $
                      AssignExpr () OpAssign (LDot () (dotSep (init path)) (last path)) expr

    jsImport name =
        assign [dolarize name] $ dotSep ("Elm" : Help.splitDots name ++ ["make"]) <| ref "_elm"

    setup namespace path = map create paths
        where
          create name = assign name (InfixExpr () OpLOr (dotSep name) (ObjectLit () []))
          paths = drop 2 . init . List.inits $ namespace : path

binop :: Region -> Var.Canonical -> Canonical.Expr -> Canonical.Expr
      -> State Int (Expression ())
binop region (Var.Canonical home op) e1 e2 =
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
             return $ obj "_L.append" `call` [e1', e2']

      (Var.BuiltIn, "::") ->
          expression (A region (Data "::" [e1,e2]))

      (Var.Module "Basics", _) ->
          do e1' <- expression e1
             e2' <- expression e2
             return $ case Map.lookup op basicOps of
                        Just f -> f e1' e2'
                        Nothing -> ref "A2" `call` [ func, e1', e2' ]

      _ ->
          do e1' <- expression e1
             e2' <- expression e2
             return (ref "A2" `call` [ func, e1', e2' ])

  where
    collect es e =
        case e of
          A _ (Binop (Var.Canonical (Var.Module "Basics") ".") e1 e2) ->
              collect (es ++ [e1]) e2
          _ -> es ++ [e]

    func | Help.isOp op = BracketRef () (dotSep (path ++ ["_op"])) (string op)
         | otherwise    = dotSep (path ++ [op])
         where
           path = case home of
                    Var.Local -> []
                    Var.BuiltIn -> []
                    Var.Module m -> [dolarize m]

    basicOps = Map.fromList (infixOps ++ specialOps)

    infixOps =
        let infixOp str op = (str, InfixExpr () op) in
        [ infixOp "+"  OpAdd
        , infixOp "-"  OpSub
        , infixOp "*"  OpMul
        , infixOp "/"  OpDiv
        , infixOp "&&" OpLAnd
        , infixOp "||" OpLOr
        ]

    specialOps =
        [ (,) "^"   $ \a b -> obj "Math.pow" `call` [a,b]
        , (,) "|>"  $ flip (<|)
        , (,) "=="  $ \a b -> obj "_U.eq" `call` [a,b]
        , (,) "/="  $ \a b -> PrefixExpr () PrefixLNot (obj "_U.eq" `call` [a,b])
        , (,) "<"   $ cmp OpLT 0
        , (,) ">"   $ cmp OpGT 0
        , (,) "<="  $ cmp OpLT 1
        , (,) ">="  $ cmp OpGT (-1)
        , (,) "div" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
        ]

    cmp op n a b = InfixExpr () op (obj "_U.cmp" `call` [a,b]) (IntLit () n)
