module Generate.JavaScript (generate) where

import Control.Applicative ((<$>),(<*>))
import Control.Arrow (first, second, (***))
import Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import Language.ECMAScript3.PrettyPrint as ES
import Language.ECMAScript3.Syntax
import qualified Text.PrettyPrint.Leijen as PP

import qualified AST.Expression.General as Expr
import AST.Expression.Optimized as Opt
import AST.Literal
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Generate.JavaScript.Helpers as Help
import qualified Generate.JavaScript.BuiltIn as BuiltIn
import qualified Generate.JavaScript.Literal as Literal
import qualified Generate.JavaScript.Port as Port
import qualified Generate.JavaScript.Variable as Var
import qualified Optimize.Cases as PM


generate :: Module.Optimized -> String
generate definitions =
  let
    stmts =
        State.evalState (mapM generateDef definitions) 0
  in
    PP.displayS (PP.renderPretty 0.4 120 (ES.prettyPrint stmts)) ""


-- CODE CHUNKS

data Code
    = JsExpr (Expression ())
    | JsBlock [Statement ()]


jsExpr :: Expression () -> State Int Code
jsExpr exp =
  return (JsExpr exp)


jsBlock :: [Statement ()] -> State Int Code
jsBlock exp =
  return (JsBlock exp)


isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr _ -> False


toStatementList :: Code -> [Statement ()]
toStatementList code =
  case code of
    JsExpr expr ->
        [ ret expr ]
    JsBlock stmts ->
        stmts


toStatement :: Code -> Statement ()
toStatement code =
  case code of
    JsExpr expr ->
        ret expr
    JsBlock [stmt] ->
        stmt
    JsBlock stmts ->
        BlockStmt () stmts


toExpr :: Code -> Expression ()
toExpr code =
  case code of
    JsExpr expr ->
        expr
    JsBlock stmts ->
        function [] stmts `call` []


-- TAIL CALL CONTEXT

type TailCallContext =
  Maybe (String, [String], Label)


type Label =
  Id ()


-- EXPRESSIONS

toJsExpr :: Opt.Expr -> State Int (Expression ())
toJsExpr =
  exprToJsExpr Nothing


toCode :: Opt.Expr -> State Int Code
toCode =
  exprToCode Nothing


exprToJsExpr :: TailCallContext -> Opt.Expr -> State Int (Expression ())
exprToJsExpr tcc canonicalExpr =
  toExpr <$> exprToCode tcc canonicalExpr


exprToCode :: TailCallContext -> Opt.Expr -> State Int Code
exprToCode tcc expr =
    case expr of
      Var var ->
          jsExpr $ Var.canonical var

      Literal lit ->
          jsExpr (Literal.literal lit)

      Range lo hi ->
          do  lo' <- toJsExpr lo
              hi' <- toJsExpr hi
              jsExpr $ BuiltIn.range lo' hi'

      Access record field ->
          do  record' <- toJsExpr record
              jsExpr $ DotRef () record' (var (Var.safe field))

      Update record fields ->
          let
            fieldToJs (field, value) =
              do  jsValue <- toJsExpr value
                  return (Var.safe field, jsValue)
          in
            do  jsRecord <- toJsExpr record
                jsFields <- mapM fieldToJs fields
                jsExpr $ BuiltIn.recordUpdate jsRecord jsFields

      Record fields ->
          do  fields' <-
                forM fields $ \(field, e) ->
                    (,) (Var.safe field) <$> toJsExpr e

              let fieldMap =
                    List.foldl' combine Map.empty fields'

              jsExpr $ ObjectLit () $ (prop "_", hidden fieldMap) : visible fieldMap
          where
            combine record (field, value) =
                Map.insertWith (++) field [value] record

            hidden fs =
                ObjectLit () . map (prop *** ArrayLit ()) $
                  Map.toList (Map.filter (not . null) (Map.map tail fs))

            visible fs =
                map (first prop) (Map.toList (Map.map head fs))

      Binop op leftExpr rightExpr ->
          binop op leftExpr rightExpr

      Function args body ->
          generateFunction args body

      Call func args ->
          let
            arity = length args
            aN = "A" ++ show arity
          in
            do  jsFunc <- toJsExpr func
                jsArgs <- mapM toJsExpr args
                jsExpr $
                  if 2 <= arity && arity <= 9 then
                    ref aN `call` (jsFunc:jsArgs)
                  else
                    foldl1 (<|) (jsFunc:jsArgs)

      TailCall name argNames args ->
          let
            reassign name tempName =
              ExprStmt () (AssignExpr () OpAssign (LVar () name) (ref tempName))
          in
            do  args' <- mapM toJsExpr args
                tempNames <- mapM (\_ -> Var.fresh) args
                jsBlock $
                  VarDeclStmt () (zipWith varDecl tempNames args')
                  : zipWith reassign argNames tempNames
                  ++ [ContinueStmt () (Just (Id () name))]

      Let defs body ->
          generateLet tcc defs body

      If branches finally ->
          generateIf tcc branches finally

      Case expr decisionTree branches ->
          generateCase tcc expr decisionTree branches

      ExplicitList elements ->
          do  jsElements <- mapM toJsExpr elements
              jsExpr $ BuiltIn.list jsElements

      Data tag members ->
          do  jsMembers <- mapM toJsExpr members
              jsExpr $ ObjectLit () (ctor : fields jsMembers)
          where
            ctor = (prop "ctor", string tag)
            fields =
                zipWith (\n e -> (prop ("_" ++ show n), e)) [ 0 :: Int .. ]

      DataAccess dataExpr index ->
          do  jsDataExpr <- toJsExpr dataExpr
              jsExpr $ DotRef () jsDataExpr (var ("_" ++ show index))

      GLShader _uid src _tipe ->
          jsExpr $ ObjectLit () [(PropString () "src", Literal.literal (Str src))]

      Port impl ->
          case impl of
            Expr.In name portType ->
                jsExpr (Port.inbound name portType)

            Expr.Out name expr portType ->
                do  expr' <- toJsExpr expr
                    jsExpr (Port.outbound name expr' portType)

            Expr.Task name expr portType ->
                do  expr' <- toJsExpr expr
                    jsExpr (Port.task name expr' portType)

      Crash ->
          error "TODO"


-- FUNCTIONS

generateFunction :: [String] -> Opt.Expr -> State Int Code
generateFunction args body =
  do  code <- exprToCode Nothing body
      jsExpr (generateFunctionWithArity args code)


generateTailFunction :: String -> [String] -> Opt.Expr -> State Int (Expression ())
generateTailFunction name args body =
  do  code <- exprToCode Nothing body
      return $ generateFunctionWithArity args $ JsBlock $ (:[]) $
          LabelledStmt ()
              (Id () name)
              (WhileStmt () (BoolLit () True) (toStatement code))


generateFunctionWithArity :: [String] -> Code -> Expression ()
generateFunctionWithArity args code =
    let
        arity = length args
    in
      if 2 <= arity && arity <= 9 then
          let
              fN = "F" ++ show arity
          in
              ref fN <| function args (toStatementList code)
      else
          let
              (lastArg:otherArgs) = reverse args
              innerBody = function [lastArg] (toStatementList code)
          in
              foldl (\body arg -> function [arg] [ret body]) innerBody otherArgs


-- LET EXPRESSIONS

generateLet
    :: TailCallContext
    -> [Opt.Def]
    -> Opt.Expr
    -> State Int Code
generateLet tcc defs body =
  do  stmts <- mapM generateDef defs
      code <- exprToCode tcc body
      jsBlock (stmts ++ toStatementList code)


-- GENERATE IFS

generateIf :: TailCallContext -> [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> State Int Code
generateIf tcc givenBranches givenFinally =
  let
    (branches, finally) =
        crushIfs givenBranches givenFinally

    convertBranch (condition, expr) =
        (,) <$> toJsExpr condition <*> exprToCode tcc expr

    ifExpression (condition, branch) otherwise =
        CondExpr () condition branch otherwise

    ifStatement (condition, branch) otherwise =
        IfStmt () condition branch otherwise
  in
    do  jsBranches <- mapM convertBranch branches
        jsFinally <- exprToCode tcc finally

        if any isBlock (jsFinally : map snd jsBranches)
          then
            jsBlock [ foldr ifStatement (toStatement jsFinally) (map (second toStatement) jsBranches) ]
          else
            jsExpr (foldr ifExpression (toExpr jsFinally) (map (second toExpr) jsBranches))


crushIfs
    :: [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfs branches finally =
  crushIfsHelp [] branches finally


crushIfsHelp
    :: [(Opt.Expr, Opt.Expr)]
    -> [(Opt.Expr, Opt.Expr)]
    -> Opt.Expr
    -> ([(Opt.Expr, Opt.Expr)], Opt.Expr)
crushIfsHelp visitedBranches unvisitedBranches finally =
  case unvisitedBranches of
    [] ->
        case finally of
          If subBranches subFinally ->
              crushIfsHelp visitedBranches subBranches subFinally

          _ ->
              (reverse visitedBranches, finally)

    (Literal (Boolean True), branch) : _ ->
        crushIfsHelp visitedBranches [] branch

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited finally



-- DEFINITIONS

generateDef :: Opt.Def -> State Int (Statement ())
generateDef def =
  do  (home, name, jsBody) <-
          case def of
            Opt.TailDef (Opt.Facts home _) name argNames body ->
                (,,) home name <$> generateTailFunction name argNames body

            Opt.Def (Opt.Facts home _) name body ->
                (,,) home name <$> toJsExpr body

      return (VarDeclStmt () [ varDecl (Var.defName home name) jsBody ])


-- CASE EXPRESSIONS

generateCase
    :: TailCallContext
    -> Opt.Expr
    -> PM.DecisionTree Int
    -> Map.Map Int Opt.Expr
    -> State Int Code
generateCase tcc expr decisionTree branches =
    error "TODO" tcc expr decisionTree branches


-- BINARY OPERATORS

binop
    :: Var.Canonical
    -> Opt.Expr
    -> Opt.Expr
    -> State Int Code
binop func left right
  | func == Var.Canonical Var.BuiltIn "::" =
      toCode (Data "::" [left,right])

  | func == forwardCompose =
      compose (collectLeftAssoc forwardCompose left right)

  | func == backwardCompose =
      compose (collectRightAssoc backwardCompose left right)

  | func == forwardApply =
      pipe (collectLeftAssoc forwardApply left right)

  | func == backwardApply =
      pipe (collectRightAssoc backwardApply left right)

  | otherwise =
      do  jsLeft <- toJsExpr left
          jsRight <- toJsExpr right
          jsExpr (makeExpr func jsLeft jsRight)


-- CONTROL FLOW OPERATOR HELPERS

apply :: Expression () -> Expression () -> Expression ()
apply arg func =
  func <| arg


compose :: [Opt.Expr] -> State Int Code
compose functions =
  do  jsFunctions <- mapM toJsExpr functions
      jsExpr $ ["$"] ==> List.foldl' apply (ref "$") jsFunctions


pipe :: [Opt.Expr] -> State Int Code
pipe expressions =
  do  (value:functions) <- mapM toJsExpr expressions
      jsExpr $ List.foldl' apply value functions


forwardCompose :: Var.Canonical
forwardCompose =
  inBasics ">>"


backwardCompose :: Var.Canonical
backwardCompose =
  inBasics "<<"


forwardApply :: Var.Canonical
forwardApply =
  inBasics "|>"


backwardApply :: Var.Canonical
backwardApply =
  inBasics "<|"


inBasics :: String -> Var.Canonical
inBasics name =
  Var.inCore ["Basics"] name


-- BINARY OPERATOR HELPERS

makeExpr :: Var.Canonical -> (Expression () -> Expression () -> Expression ())
makeExpr qualifiedOp@(Var.Canonical home op) =
    let
        simpleMake left right =
            ref "A2" `call` [ Var.canonical qualifiedOp, left, right ]
    in
        if home == Var.Module (ModuleName.inCore ["Basics"]) then
            Map.findWithDefault simpleMake op basicOps
        else
            simpleMake


basicOps :: Map.Map String (Expression () -> Expression () -> Expression ())
basicOps =
    Map.fromList (infixOps ++ specialOps)


infixOps :: [(String, Expression () -> Expression () -> Expression ())]
infixOps =
    let
        infixOp str op =
            (str, InfixExpr () op)
    in
        [ infixOp "+"  OpAdd
        , infixOp "-"  OpSub
        , infixOp "*"  OpMul
        , infixOp "/"  OpDiv
        , infixOp "&&" OpLAnd
        , infixOp "||" OpLOr
        ]


specialOps :: [(String, Expression () -> Expression () -> Expression ())]
specialOps =
    [ (,) "^"  $ \a b -> obj ["Math","pow"] `call` [a,b]
    , (,) "|>" $ flip (<|)
    , (,) "==" $ \a b -> BuiltIn.eq a b
    , (,) "/=" $ \a b -> PrefixExpr () PrefixLNot (BuiltIn.eq a b)
    , (,) "<"  $ cmp OpLT 0
    , (,) ">"  $ cmp OpGT 0
    , (,) "<=" $ cmp OpLT 1
    , (,) ">=" $ cmp OpGT (-1)
    , (,) "//" $ \a b -> InfixExpr () OpBOr (InfixExpr () OpDiv a b) (IntLit () 0)
    ]


cmp :: InfixOp -> Int -> Expression () -> Expression () -> Expression ()
cmp op n a b =
    InfixExpr () op (BuiltIn.compare a b) (IntLit () n)


-- BINARY OPERATOR COLLECTORS

-- (h << g << f) becomes [f, g, h] which is the order you want for doing stuff
collectRightAssoc :: Var.Canonical -> Opt.Expr -> Opt.Expr -> [Opt.Expr]
collectRightAssoc desiredOp left right =
  collectRightAssocHelp desiredOp [left] right


collectRightAssocHelp :: Var.Canonical -> [Opt.Expr] -> Opt.Expr -> [Opt.Expr]
collectRightAssocHelp desiredOp exprList expr =
    case expr of
      Binop op left right | op == desiredOp ->
          collectRightAssocHelp desiredOp (left : exprList) right

      _ ->
          expr : exprList


-- (f >> g >> h) becomes [f, g, h] which is the order you want for doing stuff
collectLeftAssoc :: Var.Canonical -> Opt.Expr -> Opt.Expr -> [Opt.Expr]
collectLeftAssoc desiredOp left right =
  collectLeftAssocHelp desiredOp [right] left


collectLeftAssocHelp :: Var.Canonical -> [Opt.Expr] -> Opt.Expr -> [Opt.Expr]
collectLeftAssocHelp desiredOp exprList expr =
    case expr of
      Binop op left right | op == desiredOp ->
          collectLeftAssocHelp desiredOp (right : exprList) left

      _ ->
          expr : exprList

