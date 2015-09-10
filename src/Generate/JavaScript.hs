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
import qualified AST.Literal as L
import qualified AST.Module as Module
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as Var
import Generate.JavaScript.Helpers as Help
import qualified Generate.JavaScript.BuiltIn as BuiltIn
import qualified Generate.JavaScript.Literal as Literal
import qualified Generate.JavaScript.Port as Port
import qualified Generate.JavaScript.Variable as Var
import qualified Optimize.Patterns.DecisionTree as DT


generate :: Module.Optimized -> String
generate module_ =
  let
    definitions =
        Module.program (Module.body module_)

    setup =
        Var.define (Just (Module.name module_)) "_op" (ObjectLit () [])

    stmts =
        State.evalState (mapM generateDef definitions) 0
  in
    PP.displayS (PP.renderPretty 0.4 120 (ES.prettyPrint (setup:stmts))) ""


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


-- DEFINITIONS

generateDef :: Opt.Def -> State Int (Statement ())
generateDef def =
  do  (home, name, jsBody) <-
          case def of
            Opt.TailDef (Opt.Facts home _) name argNames body ->
                (,,) home name <$> generateTailFunction name argNames body

            Opt.Def (Opt.Facts home _) name body ->
                (,,) home name <$> generateJsExpr body

      return (Var.define home name jsBody)


-- EXPRESSIONS

generateJsExpr :: Opt.Expr -> State Int (Expression ())
generateJsExpr optExpr =
  toExpr <$> generateCode optExpr


generateCode :: Opt.Expr -> State Int Code
generateCode expr =
    case expr of
      Var var ->
          jsExpr $ Var.canonical var

      Literal lit ->
          jsExpr (Literal.literal lit)

      Range lo hi ->
          do  lo' <- generateJsExpr lo
              hi' <- generateJsExpr hi
              jsExpr $ BuiltIn.range lo' hi'

      Access record field ->
          do  record' <- generateJsExpr record
              jsExpr $ DotRef () record' (var (Var.safe field))

      Update record fields ->
          let
            fieldToJs (field, value) =
              do  jsValue <- generateJsExpr value
                  return (Var.safe field, jsValue)
          in
            do  jsRecord <- generateJsExpr record
                jsFields <- mapM fieldToJs fields
                jsExpr $ BuiltIn.recordUpdate jsRecord jsFields

      Record fields ->
          do  fields' <-
                forM fields $ \(field, e) ->
                    (,) (Var.safe field) <$> generateJsExpr e

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
            do  jsFunc <- generateJsExpr func
                jsArgs <- mapM generateJsExpr args
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
            do  args' <- mapM generateJsExpr args
                tempNames <- mapM (\_ -> Var.fresh) args
                jsBlock $
                  VarDeclStmt () (zipWith varDecl tempNames args')
                  : zipWith reassign argNames tempNames
                  ++ [ContinueStmt () (Just (Id () name))]

      Let defs body ->
          do  stmts <- mapM generateDef defs
              code <- generateCode body
              jsBlock (stmts ++ toStatementList code)

      If branches finally ->
          generateIf branches finally

      Case root decisionTree branches ->
          JsBlock <$> generateCase root decisionTree branches

      ExplicitList elements ->
          do  jsElements <- mapM generateJsExpr elements
              jsExpr $ BuiltIn.list jsElements

      Data tag members ->
          do  jsMembers <- mapM generateJsExpr members
              jsExpr $ ObjectLit () (ctor : fields jsMembers)
          where
            ctor = (prop "ctor", string tag)
            fields =
                zipWith (\n e -> (prop ("_" ++ show n), e)) [ 0 :: Int .. ]

      DataAccess dataExpr index ->
          do  jsDataExpr <- generateJsExpr dataExpr
              jsExpr $ DotRef () jsDataExpr (var ("_" ++ show index))

      GLShader _uid src _tipe ->
          jsExpr $ ObjectLit () [(PropString () "src", Literal.literal (L.Str src))]

      Port impl ->
          case impl of
            Expr.In name portType ->
                jsExpr (Port.inbound name portType)

            Expr.Out name expr portType ->
                do  expr' <- generateJsExpr expr
                    jsExpr (Port.outbound name expr' portType)

            Expr.Task name expr portType ->
                do  expr' <- generateJsExpr expr
                    jsExpr (Port.task name expr' portType)

      Crash region maybeCaseCrashValue ->
          jsExpr $ BuiltIn.crash region maybeCaseCrashValue


-- FUNCTIONS

generateFunction :: [String] -> Opt.Expr -> State Int Code
generateFunction args body =
  do  code <- generateCode body
      jsExpr (generateFunctionWithArity args code)


generateTailFunction :: String -> [String] -> Opt.Expr -> State Int (Expression ())
generateTailFunction name args body =
  do  code <- generateCode body
      return $ generateFunctionWithArity args $ JsBlock $ (:[]) $
          LabelledStmt ()
              (Id () name)
              (WhileStmt () (BoolLit () True) (toStatement code))


generateFunctionWithArity :: [String] -> Code -> Expression ()
generateFunctionWithArity rawArgs code =
    let
        args = map Var.safe rawArgs
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


-- GENERATE IFS

generateIf :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> State Int Code
generateIf givenBranches givenFinally =
  let
    (branches, finally) =
        crushIfs givenBranches givenFinally

    convertBranch (condition, expr) =
        (,) <$> generateJsExpr condition <*> generateCode expr

    ifExpression (condition, branch) otherwise =
        CondExpr () condition branch otherwise

    ifStatement (condition, branch) otherwise =
        IfStmt () condition branch otherwise
  in
    do  jsBranches <- mapM convertBranch branches
        jsFinally <- generateCode finally

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

    (Literal (L.Boolean True), branch) : _ ->
        crushIfsHelp visitedBranches [] branch

    visiting : unvisited ->
        crushIfsHelp (visiting : visitedBranches) unvisited finally


-- CASE EXPRESSIONS

generateCase
    :: String
    -> DT.DecisionTree Opt.Jump
    -> [(Int, Opt.Branch)]
    -> State Int [Statement ()]
generateCase root decisionTree jumpBranches =
  do  decider <- generateDecisionTree root decisionTree
      foldM (goto root) decider jumpBranches


goto :: String -> [Statement ()] -> (Int, Opt.Branch) -> State Int [Statement ()]
goto root decider (target, branch) =
  let
    deciderStmt =
      LabelledStmt ()
        (targetLabel root target)
        (DoWhileStmt () (BlockStmt () decider) (BoolLit () False))
  in
    do  stmts <- generateBranch root branch
        return (deciderStmt : stmts)


targetLabel :: String -> Int -> Id ()
targetLabel root target =
  Id () (root ++ "_" ++ show target)


generateDecisionTree :: String -> DT.DecisionTree Opt.Jump -> State Int [Statement ()]
generateDecisionTree root decisionTree =
  case decisionTree of
    DT.Match (Opt.Inline branch) ->
        generateBranch root branch

    DT.Match (Opt.Jump target) ->
        return [ BreakStmt () (Just (targetLabel root target)) ]

    DT.Decision testPath edges fallback ->
        do  accessExpr <- generateJsExpr (pathToExpr root testPath)

            let testExpr =
                  case fst (head edges) of
                    DT.Constructor _ ->
                        DotRef () accessExpr (Id () "ctor")

                    DT.Literal _ ->
                        accessExpr

            caseClauses <- mapM (edgeToCaseClause root) edges
            caseDefault <- fallbackToDefault root fallback

            return [ SwitchStmt () testExpr (caseClauses ++ caseDefault) ]


edgeToCaseClause :: String -> (DT.Test, DT.DecisionTree Opt.Jump) -> State Int (CaseClause ())
edgeToCaseClause root (test, subTree) =
  CaseClause () (testToExpr test) <$> generateDecisionTree root subTree


testToExpr :: DT.Test -> Expression ()
testToExpr test =
  case test of
    DT.Constructor (Var.Canonical _ tag) ->
        StringLit () tag

    DT.Literal (L.Chr char) ->
        StringLit () [char]

    DT.Literal lit ->
        Literal.literal lit


fallbackToDefault :: String -> Maybe (DT.DecisionTree Opt.Jump) -> State Int [CaseClause ()]
fallbackToDefault root fallback =
  case fallback of
    Nothing ->
        return []

    Just subTree ->
        do  stmts <- generateDecisionTree root subTree
            return [ CaseDefault () stmts ]


generateBranch :: String -> Opt.Branch -> State Int [Statement ()]
generateBranch root (Opt.Branch substitutions expr) =
  do  subts <- mapM (loadPath root) substitutions
      code <- generateCode expr
      return (subts ++ toStatementList code)


loadPath :: String -> (String, DT.Path) -> State Int (Statement ())
loadPath root (name, path) =
  do  jsAccessExpr <- generateJsExpr (pathToExpr root path)
      return $ VarDeclStmt () [ varDecl name jsAccessExpr ]


pathToExpr :: String -> DT.Path -> Opt.Expr
pathToExpr root fullPath =
    go (Opt.Var (Var.local root)) fullPath
  where
    go expr path =
        case path of
          DT.Position index subpath ->
              go (Opt.DataAccess expr index) subpath

          DT.Field field subpath ->
              go (Opt.Access expr field) subpath

          DT.Empty ->
              expr

          DT.Alias ->
              expr


-- BINARY OPERATORS

binop
    :: Var.Canonical
    -> Opt.Expr
    -> Opt.Expr
    -> State Int Code
binop func left right =
    do  jsLeft <- generateJsExpr left
        jsRight <- generateJsExpr right
        jsExpr (makeExpr func jsLeft jsRight)


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
