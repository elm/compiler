{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Expression
  ( generateDecl
  )
  where

import Prelude hiding (exp)
import Control.Arrow (second)
import Control.Monad (foldM)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)

import AST.Expression.Optimized as Opt
import qualified AST.Literal as L
import qualified AST.Module.Name as ModuleName
import qualified AST.Variable as V
import Generate.JavaScript.Helpers as Help
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.BuiltIn as BuiltIn
import qualified Generate.JavaScript.Literal as Literal
import qualified Generate.JavaScript.Variable as Var
import Generate.JavaScript.Variable (Generator)
import qualified Optimize.DecisionTree as DT



-- GENERATE DECLARATION


generateDecl :: ModuleName.Canonical -> Text -> Opt.Def -> Generator JS.Stmt
generateDecl home name def =
  do  jsName <- Var.globalName (V.Global home name)
      jsBody <- generateDef name def
      return $ JS.VarDeclStmt [ Help.varDecl jsName jsBody ]



-- CODE CHUNKS


data Code
    = JsExpr JS.Expr
    | JsBlock [JS.Stmt]


jsExpr :: JS.Expr -> Generator Code
jsExpr exp =
  return (JsExpr exp)


jsBlock :: [JS.Stmt] -> Generator Code
jsBlock stmts =
  return (JsBlock stmts)


isBlock :: Code -> Bool
isBlock code =
  case code of
    JsBlock _ -> True
    JsExpr _ -> False


toStatementList :: Code -> [JS.Stmt]
toStatementList code =
  case code of
    JsExpr expr ->
        [ JS.Return (Just expr) ]

    JsBlock stmts ->
        stmts


toStatement :: Code -> JS.Stmt
toStatement code =
  case code of
    JsExpr expr ->
        JS.Return (Just expr)

    JsBlock [stmt] ->
        stmt

    JsBlock stmts ->
        JS.Block stmts


toExpr :: Code -> JS.Expr
toExpr code =
  case code of
    JsExpr expr ->
      expr

    JsBlock stmts ->
      JS.Call (function [] stmts) []



-- EXPRESSIONS


generateJsExpr :: Opt.Expr -> Generator JS.Expr
generateJsExpr optExpr =
  toExpr <$> generateCode optExpr


generateCode :: Opt.Expr -> Generator Code
generateCode expr =
    case expr of
      VarLocal name ->
          jsExpr $ Var.local name

      VarGlobal name ->
          JsExpr <$> Var.global name

      Literal lit ->
          JsExpr <$> Literal.literal lit

      Access record field ->
          do  jsRecord <- generateJsExpr record
              jsExpr $ JS.DotRef jsRecord (JS.Id (Var.safe field))

      Update record fields ->
          let
            fieldToJs (field, value) =
              do  jsValue <- generateJsExpr value
                  return (Var.safe field, jsValue)
          in
            do  jsRecord <- generateJsExpr record
                jsFields <- mapM fieldToJs fields
                JsExpr <$> BuiltIn.recordUpdate jsRecord jsFields

      Record fields ->
          let
            toField (field, value) =
              do  jsValue <- generateJsExpr value
                  return (Var.safe field ==> jsValue)
          in
            do  jsFields <- mapM toField fields
                jsExpr $ JS.Object jsFields

      Binop op leftExpr rightExpr ->
          generateBinop op leftExpr rightExpr

      Function args body ->
          generateFunction args body

      Call func args ->
          generateCall func args

      TailCall name argNames args ->
          let
            reassign argName tempName =
              JS.ExprStmt $
                JS.Assign (JS.LVar (Var.safe argName)) (ref tempName)
          in
            do  args' <- mapM generateJsExpr args
                tempNames <- mapM (\_ -> Var.fresh) args
                jsBlock $
                  JS.VarDeclStmt (zipWith varDecl tempNames args')
                  : zipWith reassign argNames tempNames
                  ++ [JS.Continue (Just (JS.Id (Var.safe name)))]

      Let defs body ->
          do  stmts <- mapM generateLetDef defs
              code <- generateCode body
              jsBlock (stmts ++ toStatementList code)

      If branches finally ->
          generateIf branches finally

      Case exprName decider jumps ->
          JsBlock <$> generateCase exprName decider jumps

      List elements ->
          do  jsElements <- mapM generateJsExpr elements
              JsExpr <$> BuiltIn.list jsElements

      Ctor tag members ->
        let
          tagField =
            "$" ==> JS.String tag

          toEntry entry n =
            (Help.toFieldName n) ==> entry
        in
          do  jsMembers <- mapM generateJsExpr members
              jsExpr $ JS.Object (tagField : zipWith toEntry jsMembers [ 0 :: Int .. ])

      CtorAccess dataExpr index ->
          do  jsDataExpr <- generateJsExpr dataExpr
              jsExpr $ JS.DotRef jsDataExpr (JS.Id (Help.toFieldName index))

      Cmd moduleName _ ->
          JsExpr <$> BuiltIn.effect moduleName

      Sub moduleName _ ->
          JsExpr <$> BuiltIn.effect moduleName

      OutgoingPort name encoder ->
          do  jsEncoder <- generateJsExpr encoder
              JsExpr <$> BuiltIn.outgoingPort name jsEncoder

      IncomingPort name decoder ->
          do  jsDecoder <- generateJsExpr decoder
              JsExpr <$> BuiltIn.incomingPort name jsDecoder

      Program kind body ->
          generateProgram kind body

      GLShader src ->
          do  string <- Literal.literal (L.Str src)
              jsExpr $ JS.Object [(JS.StringProp "src", string)]

      Crash home region maybeBranchProblem ->
          do  maybeOptBranchProblem <- traverse generateJsExpr maybeBranchProblem
              JsExpr <$> BuiltIn.crash home region maybeOptBranchProblem



-- PROGRAMS


generateProgram :: Opt.Main -> Opt.Expr -> Generator Code
generateProgram kind body =
  case kind of
    Opt.Static ->
      do  html <- generateJsExpr body
          func <- BuiltIn.staticProgram
          jsExpr (func <| html)

    Opt.Dynamic decoder ->
      do  almostProgram <- generateJsExpr body
          flagDecoder <- generateJsExpr decoder
          jsExpr (almostProgram <| flagDecoder)



-- DEFINITIONS


generateLetDef :: (Text, Opt.Def) -> Generator JS.Stmt
generateLetDef (name, def) =
  do  jsBody <- generateDef name def
      return $ JS.VarDeclStmt [ Help.varDecl (Var.safe name) jsBody ]


generateDef :: Text -> Opt.Def -> Generator JS.Expr
generateDef name def =
  case def of
    Opt.TailDef argNames body ->
      generateTailFunction name argNames body

    Opt.Def body ->
      generateJsExpr body


generateTailFunction :: Text -> [Text] -> Opt.Expr -> Generator JS.Expr
generateTailFunction name args body =
  do  code <- generateCode body
      return $ generateFunctionWithArity args $ JsBlock $ (:[]) $
          JS.Labelled
              (JS.Id (Var.safe name))
              (JS.While (JS.Bool True) (toStatement code))



-- FUNCTIONS


generateFunction :: [Text] -> Opt.Expr -> Generator Code
generateFunction args body =
  do  code <- generateCode body
      jsExpr (generateFunctionWithArity args code)


generateFunctionWithArity :: [Text] -> Code -> JS.Expr
generateFunctionWithArity rawArgs code =
    let
        args = map Var.safe rawArgs
        arity = length args
    in
      if 2 <= arity && arity <= 9 then
          let
              fN = "F" <> Text.pack (show arity)
          in
              ref fN <| function args (toStatementList code)
      else
          let
              (lastArg:otherArgs) = reverse args
              innerBody = function [lastArg] (toStatementList code)
          in
              foldl (\body arg -> function [arg] [JS.Return (Just body)]) innerBody otherArgs



-- GENERATE CALL


generateCall :: Opt.Expr -> [Opt.Expr] -> Generator Code
generateCall func args =
  case (func, args) of
    (Opt.VarGlobal (V.Global home name), [arg]) ->
      case name of
        "complement" | home == bitwise -> genPrefix JS.PrefixBNot arg
        "not"        | home == basics  -> genPrefix JS.PrefixLNot arg
        _                              -> generateCallHelp func args

    (Opt.VarGlobal (V.Global home name), [ left, right ]) | home == bitwise ->
      case name of
        "and"            -> genInfix JS.OpBAnd     left right
        "or"             -> genInfix JS.OpBOr      left right
        "xor"            -> genInfix JS.OpBXor     left right
        "shiftLeftBy"    -> genInfix JS.OpLShift   right left
        "shiftRightBy"   -> genInfix JS.OpSpRShift right left
        "shiftRightZfBy" -> genInfix JS.OpZfRShift right left
        _                -> generateCallHelp func args

    _ ->
      generateCallHelp func args


genPrefix :: JS.PrefixOp -> Opt.Expr -> Generator Code
genPrefix op arg =
  do  jsArg <- generateJsExpr arg
      jsExpr (JS.Prefix op jsArg)


genInfix :: JS.InfixOp -> Opt.Expr -> Opt.Expr -> Generator Code
genInfix op left right =
  do  jsLeft <- generateJsExpr left
      jsRight <- generateJsExpr right
      jsExpr $ JS.Infix op jsLeft jsRight


basics :: ModuleName.Canonical
basics =
  ModuleName.inCore "Basics"


bitwise :: ModuleName.Canonical
bitwise =
  ModuleName.inCore "Bitwise"


generateCallHelp :: Opt.Expr -> [Opt.Expr] -> Generator Code
generateCallHelp func args =
  let
    arity = length args
    aN = "A" <> Text.pack (show arity)
  in
    do  jsFunc <- generateJsExpr func
        jsArgs <- mapM generateJsExpr args
        jsExpr $
          if 2 <= arity && arity <= 9 then
            JS.Call (ref aN) (jsFunc:jsArgs)
          else
            foldl1 (<|) (jsFunc:jsArgs)



-- GENERATE IFS


generateIf :: [(Opt.Expr, Opt.Expr)] -> Opt.Expr -> Generator Code
generateIf givenBranches givenFinally =
  let
    (branches, finally) =
        crushIfs givenBranches givenFinally

    convertBranch (condition, expr) =
        (,) <$> generateJsExpr condition <*> generateCode expr

    ifExpression (condition, branch) final =
        JS.If condition branch final

    ifStatement (condition, branch) final =
        JS.IfStmt condition branch final
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


generateCase :: Text -> Opt.Decider Opt.Choice -> [(Int, Opt.Expr)] -> Generator [JS.Stmt]
generateCase exprName decider jumps =
  do  labelRoot <- Var.fresh
      jsDecider <- generateDecider exprName labelRoot decider
      foldM (goto labelRoot) jsDecider jumps



-- handle any jumps


goto :: Text -> [JS.Stmt] -> (Int, Opt.Expr) -> Generator [JS.Stmt]
goto labelRoot deciderStmts (target, branch) =
  let
    labeledDeciderStmt =
      JS.Labelled
        (toLabel labelRoot target)
        (JS.DoWhile (JS.Block deciderStmts) (JS.Bool False))
  in
    do  code <- generateCode branch
        return (labeledDeciderStmt : toStatementList code)


toLabel :: Text -> Int -> JS.Id
toLabel root target =
  JS.Id (root <> "_" <> Text.pack (show target))



-- turn deciders into ifs and switches


generateDecider :: Text -> Text -> Opt.Decider Opt.Choice -> Generator [JS.Stmt]
generateDecider exprName labelRoot decisionTree =
  case decisionTree of
    Opt.Leaf (Opt.Inline branch) ->
        toStatementList <$> generateCode branch

    Opt.Leaf (Opt.Jump target) ->
        return [ JS.Break (Just (toLabel labelRoot target)) ]

    Opt.Chain testChain success failure ->
        let
          makeTest (path, test) =
            do  testExpr <- pathToTestableExpr exprName path test
                JS.Infix JS.OpStrictEq testExpr <$> testToExpr test
        in
          do  testExprs <- mapM makeTest testChain
              let cond = List.foldl1' (JS.Infix JS.OpLAnd) testExprs
              thenBranch <- generateDecider exprName labelRoot success
              elseBranch <- generateDecider exprName labelRoot failure
              return [ JS.IfStmt cond (JS.Block thenBranch) (JS.Block elseBranch) ]

    Opt.FanOut path edges fallback ->
        do  testExpr <- pathToTestableExpr exprName path (fst (head edges))
            caseClauses <- mapM (edgeToCase exprName labelRoot) edges
            caseDefault <- JS.Default <$> generateDecider exprName labelRoot fallback
            return [ JS.Switch testExpr (caseClauses ++ [caseDefault]) ]


edgeToCase :: Text -> Text -> (DT.Test, Opt.Decider Opt.Choice) -> Generator JS.Case
edgeToCase exprName labelRoot (test, subTree) =
  JS.Case
    <$> testToExpr test
    <*> generateDecider exprName labelRoot subTree


testToExpr :: DT.Test -> Generator JS.Expr
testToExpr test =
  case test of
    DT.Constructor (V.Canonical _ tag) ->
      return $ JS.String tag

    DT.Literal (L.Chr char) ->
      return $ JS.String char

    DT.Literal lit ->
      Literal.literal lit



-- work with paths


pathToTestableExpr :: Text -> DT.Path -> DT.Test -> Generator JS.Expr
pathToTestableExpr root path exampleTest =
  do  accessExpr <- generateJsExpr (pathToExpr root path)
      case exampleTest of
        DT.Constructor _ ->
            return $ JS.DotRef accessExpr (JS.Id "$")

        DT.Literal (L.Chr _) ->
            return $ JS.Call (JS.DotRef accessExpr (JS.Id "valueOf")) []

        DT.Literal _ ->
            return accessExpr


pathToExpr :: Text -> DT.Path -> Opt.Expr
pathToExpr root fullPath =
    go (Opt.VarLocal root) fullPath
  where
    go expr path =
        case path of
          DT.Position index subpath ->
              go (Opt.CtorAccess expr index) subpath

          DT.Field field subpath ->
              go (Opt.Access expr field) subpath

          DT.Empty ->
              expr

          DT.Alias ->
              expr



-- BINARY OPERATORS


generateBinop :: V.Global -> Opt.Expr -> Opt.Expr -> Generator Code
generateBinop op left right =
  do  jsLeft <- generateJsExpr left
      jsRight <- generateJsExpr right
      JsExpr <$> generateBinopHelp op jsLeft jsRight



-- BINARY OPERATOR HELPERS


generateBinopHelp :: V.Global -> JS.Expr -> JS.Expr -> Generator JS.Expr
generateBinopHelp var@(V.Global home op) jsLeft jsRight =
  let
    simpleMake left right =
      do  func <- Var.global var
          return $ JS.Call (ref "A2") [ func, left, right ]
  in
    if home == basics then
      (Map.findWithDefault simpleMake op basicsOps) jsLeft jsRight

    else if op == "::" && home == list then
      BuiltIn.cons jsLeft jsRight

    else
      simpleMake jsLeft jsRight


list :: ModuleName.Canonical
list =
  ModuleName.inCore "List"


basicsOps :: Map.Map Text (JS.Expr -> JS.Expr -> Generator JS.Expr)
basicsOps =
  Map.fromList (basicsOpsNormal ++ basicsOpsSpecial)


basicsOpsNormal :: [(Text, JS.Expr -> JS.Expr -> Generator JS.Expr)]
basicsOpsNormal =
  let
    (=*>) str op =
      ( str, \left right -> return (JS.Infix op left right) )
  in
    [ "+"  =*> JS.OpAdd
    , "-"  =*> JS.OpSub
    , "*"  =*> JS.OpMul
    , "/"  =*> JS.OpDiv
    , "&&" =*> JS.OpLAnd
    , "||" =*> JS.OpLOr
    ]


basicsOpsSpecial :: [(Text, JS.Expr -> JS.Expr -> Generator JS.Expr)]
basicsOpsSpecial =
  let
    (=*>) = (,)
  in
    [ "^"  =*> \a b -> return $ JS.Call (obj ["Math","pow"]) [a,b]
    , "==" =*> BuiltIn.eq
    , "/=" =*> \a b -> JS.Prefix JS.PrefixLNot <$> BuiltIn.eq a b
    , "<"  =*> cmp JS.OpLT 0
    , ">"  =*> cmp JS.OpGT 0
    , "<=" =*> cmp JS.OpLT 1
    , ">=" =*> cmp JS.OpGT (-1)
    , "//" =*> \a b -> return $ JS.Infix JS.OpBOr (JS.Infix JS.OpDiv a b) (JS.Int 0)
    ]


cmp :: JS.InfixOp -> Int -> JS.Expr -> JS.Expr -> Generator JS.Expr
cmp op n a b =
  do  ordering <- BuiltIn.cmp a b
      return $ JS.Infix op ordering (JS.Int n)
