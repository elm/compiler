{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Builder
  ( stmtToBuilder
  , exprToBuilder
  , Expr(..), LValue(..)
  , Stmt(..), Case(..)
  , InfixOp(..), PrefixOp(..)
  )
  where

-- Based on the language-ecmascript package.
-- https://hackage.haskell.org/package/language-ecmascript
-- They did the hard work of reading the spec to figure out
-- how all the types should fit together.

import Prelude hiding (lines)
import qualified Data.List as List
import qualified Data.ByteString as BS
import Data.ByteString.Builder as B
import qualified Generate.JavaScript.Name as Name
import Generate.JavaScript.Name (Name)
import qualified Json.Encode as Json



-- EXPRESSIONS


-- NOTE: I tried making this create a B.Builder directly.
--
-- The hope was that it'd allocate less and speed things up, but it seemed
-- to be neutral for perf.
--
-- The downside is that Generate.JavaScript.Expression inspects the
-- structure of Expr and Stmt on some occassions to try to strip out
-- unnecessary closures. I think these closures are already avoided
-- by other logic in code gen these days, but I am not 100% certain.
--
-- For this to be worth it, I think it would be necessary to avoid
-- returning tuples when generating expressions.
--
data Expr
  = String Builder
  | Float Builder
  | Int Int
  | Bool Bool
  | Null
  | Json Json.Value
  | Array [Expr]
  | Object [(Name, Expr)]
  | Ref Name
  | Access Expr Name -- foo.bar
  | Index  Expr Expr -- foo[bar]
  | Prefix PrefixOp Expr
  | Infix InfixOp Expr Expr
  | If Expr Expr Expr
  | Assign LValue Expr
  | Call Expr [Expr]
  | Function (Maybe Name) [Name] [Stmt]


data LValue
  = LRef Name
  | LDot Expr Name
  | LBracket Expr Expr



-- STATEMENTS


data Stmt
  = Block [Stmt]
  | EmptyStmt
  | ExprStmt Expr
  | IfStmt Expr Stmt Stmt
  | Switch Expr [Case]
  | While Expr Stmt
  | Break (Maybe Name)
  | Continue (Maybe Name)
  | Labelled Name Stmt
  | Try Stmt Name Stmt
  | Throw Expr
  | Return Expr
  | Var Name Expr
  | Vars [(Name, Expr)]
  | FunctionStmt Name [Name] [Stmt]


data Case
  = Case Expr [Stmt]
  | Default [Stmt]



-- OPERATORS


data InfixOp
  = OpAdd -- +
  | OpSub -- -
  | OpMul -- *
  | OpDiv -- /
  | OpMod -- %
  | OpEq -- ===
  | OpNe -- !==
  | OpLt -- <
  | OpLe -- <=
  | OpGt -- >
  | OpGe -- >=
  | OpAnd -- &&
  | OpOr  -- ||
  | OpBitwiseAnd -- &
  | OpBitwiseXor -- ^
  | OpBitwiseOr  -- |
  | OpLShift     -- <<
  | OpSpRShift   -- >>
  | OpZfRShift   -- >>>


data PrefixOp
  = PrefixNot        -- !
  | PrefixNegate     -- -
  | PrefixComplement -- ~



-- ENCODE


stmtToBuilder :: Stmt -> Builder
stmtToBuilder stmts =
  fromStmt levelZero stmts


exprToBuilder :: Expr -> Builder
exprToBuilder expr =
  snd $ fromExpr levelZero Whatever expr



-- INDENT LEVEL


data Level =
  Level Builder Level


levelZero :: Level
levelZero =
  Level mempty (makeLevel 1 (BS.replicate 16 0x09 {-\t-}))


makeLevel :: Int -> BS.ByteString -> Level
makeLevel level oldTabs =
  let
    tabs =
      if level <= BS.length oldTabs
      then oldTabs
      else BS.replicate (BS.length oldTabs * 2) 0x09 {-\t-}
  in
  Level (B.byteString (BS.take level tabs)) (makeLevel (level + 1) tabs)



-- HELPERS


commaSep :: [Builder] -> Builder
commaSep builders =
  mconcat (List.intersperse ", " builders)


commaNewlineSep :: Level -> [Builder] -> Builder
commaNewlineSep (Level _ (Level deeperIndent _)) builders =
  mconcat (List.intersperse (",\n" <> deeperIndent) builders)



-- STATEMENTS


fromStmtBlock :: Level -> [Stmt] -> Builder
fromStmtBlock level stmts =
  mconcat (map (fromStmt level) stmts)


fromStmt :: Level -> Stmt -> Builder
fromStmt level@(Level indent nextLevel) statement =
  case statement of
    Block stmts ->
      fromStmtBlock level stmts

    EmptyStmt ->
      mempty

    ExprStmt expr ->
      indent <> snd (fromExpr level Whatever expr) <> ";\n"

    IfStmt condition thenStmt elseStmt ->
      mconcat
        [ indent, "if (", snd (fromExpr level Whatever condition), ") {\n"
        , fromStmt nextLevel thenStmt
        , indent, "} else {\n"
        , fromStmt nextLevel elseStmt
        , indent, "}\n"
        ]

    Switch expr clauses ->
      mconcat
        [ indent, "switch (", snd (fromExpr level Whatever expr), ") {\n"
        , mconcat (map (fromClause nextLevel) clauses)
        , indent, "}\n"
        ]

    While expr stmt ->
      mconcat
        [ indent, "while (", snd (fromExpr level Whatever expr), ") {\n"
        , fromStmt nextLevel stmt
        , indent, "}\n"
        ]

    Break Nothing ->
      indent <> "break;\n"

    Break (Just label) ->
      indent <> "break " <> Name.toBuilder label <> ";\n"

    Continue Nothing ->
      indent <> "continue;\n"

    Continue (Just label) ->
      indent <> "continue " <> Name.toBuilder label <> ";\n"

    Labelled label stmt ->
      mconcat
        [ indent, Name.toBuilder label, ":\n"
        , fromStmt level stmt
        ]

    Try tryStmt errorName catchStmt ->
      mconcat
        [ indent, "try {\n"
        , fromStmt nextLevel tryStmt
        , indent, "} catch (", Name.toBuilder errorName, ") {\n"
        , fromStmt nextLevel catchStmt
        , indent, "}\n"
        ]

    Throw expr ->
      indent <> "throw " <> snd (fromExpr level Whatever expr) <> ";"

    Return expr ->
      indent <> "return " <> snd (fromExpr level Whatever expr) <> ";\n"

    Var name expr ->
      indent <> "var " <> Name.toBuilder name <> " = " <> snd (fromExpr level Whatever expr) <> ";\n"

    Vars [] ->
      mempty

    Vars vars ->
      indent <> "var " <> commaNewlineSep level (map (varToBuilder level) vars) <> ";\n"

    FunctionStmt name args stmts ->
      indent <> "function " <> Name.toBuilder name <> "(" <> commaSep (map Name.toBuilder args) <> ") {\n"
      <>
          fromStmtBlock nextLevel stmts
      <>
      indent <> "}\n"



-- SWITCH CLAUSES


fromClause :: Level -> Case -> Builder
fromClause level@(Level indent nextLevel) clause =
  case clause of
    Case expr stmts ->
      indent <> "case " <> snd (fromExpr level Whatever expr) <> ":\n"
      <> fromStmtBlock nextLevel stmts

    Default stmts ->
      indent <> "default:\n"
      <> fromStmtBlock nextLevel stmts



-- VAR DECLS


varToBuilder :: Level -> (Name, Expr) -> Builder
varToBuilder level (name, expr) =
  Name.toBuilder name <> " = " <> snd (fromExpr level Whatever expr)



-- EXPRESSIONS


data Lines = One | Many deriving (Eq)


merge :: Lines -> Lines -> Lines
merge a b =
  if a == Many || b == Many then Many else One


linesMap :: (a -> (Lines, b)) -> [a] -> (Bool, [b])
linesMap func xs =
  let
    pairs = map func xs
  in
  ( any ((==) Many . fst) pairs
  , map snd pairs
  )


data Grouping = Atomic | Whatever


parensFor :: Grouping -> Builder -> Builder
parensFor grouping builder =
  case grouping of
    Atomic ->
      "(" <> builder <> ")"

    Whatever ->
      builder


fromExpr :: Level -> Grouping -> Expr -> (Lines, Builder)
fromExpr level@(Level indent nextLevel@(Level deeperIndent _)) grouping expression =
  case expression of
    String string ->
      ( One, "'" <> string <> "'" )

    Float float ->
      ( One, float )

    Int n ->
      ( One, B.intDec n )

    Bool bool ->
      ( One, if bool then "true" else "false" )

    Null ->
      ( One, "null" )

    Json json ->
      ( One, Json.encodeUgly json )

    Array exprs ->
      (,) Many $
        let
          (anyMany, builders) = linesMap (fromExpr level Whatever) exprs
        in
        if anyMany then
          "[\n"
          <> deeperIndent
          <> commaNewlineSep level builders
          <> "\n" <> indent <> "]"
        else
          "[" <> commaSep builders <> "]"

    Object fields ->
      (,) Many $
        let
          (anyMany, builders) = linesMap (fromField nextLevel) fields
        in
        if anyMany then
          "{\n"
          <> deeperIndent
          <> commaNewlineSep level builders
          <> "\n" <> indent <> "}"
        else
          "{" <> commaSep builders <> "}"

    Ref name ->
      ( One, Name.toBuilder name )

    Access expr field ->
      makeDot level expr field

    Index expr bracketedExpr ->
      makeBracketed level expr bracketedExpr

    Prefix op expr ->
      let
        (lines, builder) = fromExpr level Atomic expr
      in
      ( lines
      , parensFor grouping (fromPrefix op <> builder)
      )

    Infix op leftExpr rightExpr ->
      let
        (leftLines , left ) = fromExpr level Atomic leftExpr
        (rightLines, right) = fromExpr level Atomic rightExpr
      in
      ( merge leftLines rightLines
      , parensFor grouping (left <> fromInfix op <> right)
      )

    If condExpr thenExpr elseExpr ->
      let
        condB = snd (fromExpr level Atomic condExpr)
        thenB = snd (fromExpr level Atomic thenExpr)
        elseB = snd (fromExpr level Atomic elseExpr)
      in
      ( Many
      , parensFor grouping (condB <> " ? " <> thenB <> " : " <> elseB)
      )

    Assign lValue expr ->
      let
        (leftLines , left ) = fromLValue level lValue
        (rightLines, right) = fromExpr level Whatever expr
      in
      ( merge leftLines rightLines
      , parensFor grouping (left <> " = " <> right)
      )

    Call function args ->
      (,) Many $
        let
          (_      , funcB) = fromExpr level Atomic function
          (anyMany, argsB) = linesMap (fromExpr nextLevel Whatever) args
        in
        if anyMany then
          funcB <> "(\n" <> deeperIndent <> commaNewlineSep level argsB <> ")"
        else
          funcB <> "(" <> commaSep argsB <> ")"

    Function maybeName args stmts ->
      (,) Many $
        "function " <> maybe mempty Name.toBuilder maybeName <> "(" <> commaSep (map Name.toBuilder args) <> ") {\n"
        <>
            fromStmtBlock nextLevel stmts
        <>
        indent <> "}"



-- FIELDS


fromField :: Level -> (Name, Expr) -> (Lines, Builder)
fromField level (field, expr) =
  let
    (lines, builder) = fromExpr level Whatever expr
  in
  ( lines
  , Name.toBuilder field <> ": " <> builder
  )



-- VALUES


fromLValue :: Level -> LValue -> (Lines, Builder)
fromLValue level lValue =
  case lValue of
    LRef name ->
      (One, Name.toBuilder name)

    LDot expr field ->
      makeDot level expr field

    LBracket expr bracketedExpr ->
      makeBracketed level expr bracketedExpr


makeDot :: Level -> Expr -> Name -> (Lines, Builder)
makeDot level expr field =
  let
    (lines, builder) = fromExpr level Atomic expr
  in
  (lines, builder <> "." <> Name.toBuilder field)


makeBracketed :: Level -> Expr -> Expr -> (Lines, Builder)
makeBracketed level expr bracketedExpr =
  let
    (lines         , builder         ) = fromExpr level Atomic expr
    (bracketedLines, bracketedBuilder) = fromExpr level Whatever bracketedExpr
  in
  ( merge lines bracketedLines
  , builder <> "[" <> bracketedBuilder <> "]"
  )



-- OPERATORS


fromPrefix :: PrefixOp -> Builder
fromPrefix op =
  case op of
    PrefixNot        -> "!"
    PrefixNegate     -> "-"
    PrefixComplement -> "~"


fromInfix :: InfixOp -> Builder
fromInfix op =
  case op of
    OpAdd        -> " + "
    OpSub        -> " - "
    OpMul        -> " * "
    OpDiv        -> " / "
    OpMod        -> " % "
    OpEq         -> " === "
    OpNe         -> " !== "
    OpLt         -> " < "
    OpLe         -> " <= "
    OpGt         -> " > "
    OpGe         -> " >= "
    OpAnd        -> " && "
    OpOr         -> " || "
    OpBitwiseAnd -> " & "
    OpBitwiseXor -> " ^ "
    OpBitwiseOr  -> " | "
    OpLShift     -> " << "
    OpSpRShift   -> " >> "
    OpZfRShift   -> " >>> "
