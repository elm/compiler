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
import Data.ByteString.Builder as B
import Data.Monoid ((<>))
import qualified Generate.JavaScript.Name as Name
import Generate.JavaScript.Name (Name)
import qualified Json.Encode as Json



-- EXPRESSIONS


data Expr
  = String Builder
  | Float Double
  | Int Int
  | Bool Bool
  | Null
  | Json Json.Value
  | Array [Expr]
  | Object [(Name, Expr)]
  | Ref Name
  | Access Expr Name -- ^ @foo.bar@, spec 11.2.1
  | Index Expr Expr -- ^ @foo[bar]@, spec 11.2.1
  | Prefix PrefixOp Expr
  | Infix InfixOp Expr Expr
  | If Expr Expr Expr
  | Assign LValue Expr
  | Call Expr [Expr] -- ^ @f(x,y,z)@, spec 11.2.3
  | Function (Maybe Name) [Name] [Stmt]


data LValue
  = LRef Name
  | LDot Expr Name
  | LBracket Expr Expr



-- STATEMENTS


data Stmt
  = Block [Stmt] -- {stmts}
  | EmptyStmt -- ;
  | ExprStmt Expr -- expr;
  | IfStmt Expr Stmt Stmt -- if (e) stmt1 else stmt2
  | Switch Expr [Case] -- switch (e) clauses
  | While Expr Stmt -- while (e) do stmt
  | Break (Maybe Name) -- break lab;
  | Continue (Maybe Name) -- continue lab;
  | Labelled Name Stmt -- lab: stmt
  | Try Stmt Name Stmt -- try stmt catch (x) stmt
  | Throw Expr -- throw expr;
  | Return (Maybe Expr) -- return expr;
  | Var [(Name, Maybe Expr)] -- var x, y=42;
  | FunctionStmt Name [Name] [Stmt] -- function f(x, y, z) {...}


data Case
  = Case Expr [Stmt]
  | Default [Stmt]



-- OPERATORS


data InfixOp
  = OpLT -- <
  | OpLEq -- <=
  | OpGT -- >
  | OpGEq -- >=
  | OpIn -- in
  | OpInstanceof -- instanceof
  | OpEq -- ==
  | OpNEq -- !=
  | OpStrictEq -- ===
  | OpStrictNEq -- !==
  | OpLAnd -- &&
  | OpLOr -- ||
  | OpMul -- *
  | OpDiv -- /
  | OpMod -- %
  | OpSub -- -
  | OpLShift -- <<
  | OpSpRShift -- >>
  | OpZfRShift -- >>>
  | OpBAnd -- &
  | OpBXor -- ^
  | OpBOr -- |
  | OpAdd -- +


data PrefixOp
  = PrefixLNot -- !
  | PrefixBNot -- ~
  | PrefixPlus -- +
  | PrefixMinus -- -
  | PrefixTypeof -- typeof



-- ENCODE


stmtToBuilder :: Stmt -> Builder
stmtToBuilder stmts =
  fromStmt "" stmts


exprToBuilder :: Expr -> Builder
exprToBuilder expr =
  snd $ fromExpr "" Whatever expr



-- HELPERS


deeper :: Builder -> Builder
deeper indent =
  "\t" <> indent


commaSep :: [Builder] -> Builder
commaSep builders =
  mconcat (List.intersperse ", " builders)


commaNewlineSep :: Builder -> [Builder] -> Builder
commaNewlineSep indent builders =
  mconcat (List.intersperse (",\n" <> deeper indent) builders)



-- STATEMENTS


fromStmtBlock :: Builder -> [Stmt] -> Builder
fromStmtBlock indent stmts =
  mconcat (map (fromStmt indent) stmts)


fromStmt :: Builder -> Stmt -> Builder
fromStmt indent statement =
  case statement of
    Block stmts ->
      fromStmtBlock indent stmts

    EmptyStmt ->
      mempty

    ExprStmt expr ->
      indent <> snd (fromExpr indent Whatever expr) <> ";\n"

    IfStmt condition thenStmt elseStmt ->
      mconcat
        [ indent, "if (", snd (fromExpr indent Whatever condition), ") {\n"
        , fromStmt (deeper indent) thenStmt
        , indent, "} else {\n"
        , fromStmt (deeper indent) elseStmt
        , indent, "}\n"
        ]

    Switch expr clauses ->
      mconcat
        [ indent, "switch (", snd (fromExpr indent Whatever expr), ") {\n"
        , mconcat (map (fromClause (deeper indent)) clauses)
        , indent, "}\n"
        ]

    While expr stmt ->
      mconcat
        [ indent, "while (", snd (fromExpr indent Whatever expr), ") {\n"
        , fromStmt (deeper indent) stmt
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
        , fromStmt indent stmt
        ]

    Try tryStmt errorName catchStmt ->
      mconcat
        [ indent, "try {\n"
        , fromStmt (deeper indent) tryStmt
        , indent, "} catch (", Name.toBuilder errorName, ") {\n"
        , fromStmt (deeper indent) catchStmt
        , indent, "}\n"
        ]

    Throw expr ->
      "throw " <> snd (fromExpr indent Whatever expr) <> ";"

    Return Nothing ->
      indent <> "return;\n"

    Return (Just expr) ->
      indent <> "return " <> snd (fromExpr indent Whatever expr) <> ";\n"

    Var [] ->
      mempty

    Var vars ->
      indent <> "var " <> commaNewlineSep indent (map (varToBuilder indent) vars) <> ";\n"

    FunctionStmt name args stmts ->
      indent <> "function " <> Name.toBuilder name <> "(" <> commaSep (map Name.toBuilder args) <> ") {\n"
      <>
          fromStmtBlock (deeper indent) stmts
      <>
      indent <> "}\n"



-- SWITCH CLAUSES


fromClause :: Builder -> Case -> Builder
fromClause indent clause =
  case clause of
    Case expr stmts ->
      indent <> "case " <> snd (fromExpr indent Whatever expr) <> ":\n"
      <> fromStmtBlock (deeper indent) stmts

    Default stmts ->
      indent <> "default:\n"
      <> fromStmtBlock (deeper indent) stmts



-- VAR DECLS


varToBuilder :: Builder -> (Name, Maybe Expr) -> Builder
varToBuilder indent (name, maybeExpr) =
  case maybeExpr of
    Nothing ->
      Name.toBuilder name

    Just expr ->
      Name.toBuilder name <> " = " <> snd (fromExpr indent Whatever expr)



-- EXPRESSIONS


data Lines = One | Many deriving (Eq)


merge :: Lines -> Lines -> Lines
merge a b =
  if a == Many || b == Many then Many else One


linesMap :: (a -> (Lines, b)) -> [a] -> (Bool, [b])
linesMap func xs =
  let
    pairs =
      map func xs
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


fromExpr :: Builder -> Grouping -> Expr -> (Lines, Builder)
fromExpr indent grouping expression =
  case expression of
    String string ->
      (One, "'" <> string <> "'")

    Float n ->
      (One, B.doubleDec n)

    Int n ->
      (One, B.intDec n)

    Bool bool ->
      (One, if bool then "true" else "false")

    Null ->
      (One, "null")

    Json json ->
      ( One, Json.encodeUgly json )

    Array exprs ->
      (,) Many $
        let
          (anyMany, builders) =
            linesMap (fromExpr indent Whatever) exprs
        in
          if anyMany then
            "[\n"
            <> deeper indent
            <> commaNewlineSep indent builders
            <> "\n" <> indent <> "]"

          else
            "[" <> commaSep builders <> "]"

    Object fields ->
      (,) Many $
        let
          deeperIndent =
            deeper indent

          (anyMany, builders) =
            linesMap (fromField deeperIndent) fields
        in
          if anyMany then
            "{\n"
            <> deeperIndent
            <> commaNewlineSep indent builders
            <> "\n" <> indent <> "}"

          else
            "{" <> commaSep builders <> "}"

    Ref name ->
      (One, Name.toBuilder name)

    Access expr field ->
      makeDot indent expr field

    Index expr bracketedExpr ->
      makeBracketed indent expr bracketedExpr

    Prefix op expr ->
      let
        (lines, builder) =
          fromExpr indent Atomic expr
      in
        ( lines
        , parensFor grouping (fromPrefix op <> builder)
        )

    Infix op leftExpr rightExpr ->
      let
        (leftLines, left) =
          fromExpr indent Atomic leftExpr

        (rightLines, right) =
          fromExpr indent Atomic rightExpr
      in
        ( merge leftLines rightLines
        , parensFor grouping (left <> fromInfix op <> right)
        )

    If condExpr thenExpr elseExpr ->
      let
        condB = snd (fromExpr indent Atomic condExpr)
        thenB = snd (fromExpr indent Atomic thenExpr)
        elseB = snd (fromExpr indent Atomic elseExpr)
      in
        ( Many
        , parensFor grouping (condB <> " ? " <> thenB <> " : " <> elseB)
        )

    Assign lValue expr ->
      let
        (leftLines, left) =
          fromLValue indent lValue

        (rightLines, right) =
          fromExpr indent Whatever expr
      in
        ( merge leftLines rightLines
        , parensFor grouping (left <> " = " <> right)
        )

    Call function args ->
      (,) Many $
        let
          deeperIndent =
            deeper indent

          funcB =
            snd (fromExpr indent Atomic function)

          (anyMany, argsB) =
            linesMap (fromExpr deeperIndent Whatever) args
        in
          if anyMany then
            funcB <> "(\n" <> deeperIndent <> commaNewlineSep indent argsB <> ")"

          else
            funcB <> "(" <> commaSep argsB <> ")"

    Function maybeName args stmts ->
      (,) Many $
        "function " <> maybe mempty Name.toBuilder maybeName <> "(" <> commaSep (map Name.toBuilder args) <> ") {\n"
        <>
            fromStmtBlock (deeper indent) stmts
        <>
        indent <> "}"



-- FIELDS


fromField :: Builder -> (Name, Expr) -> (Lines, Builder)
fromField indent (field, expr) =
  let
    (lines, builder) =
      fromExpr indent Whatever expr
  in
    ( lines
    , Name.toBuilder field <> ": " <> builder
    )



-- VALUES


fromLValue :: Builder -> LValue -> (Lines, Builder)
fromLValue indent lValue =
  case lValue of
    LRef name ->
      (One, Name.toBuilder name)

    LDot expr field ->
      makeDot indent expr field

    LBracket expr bracketedExpr ->
      makeBracketed indent expr bracketedExpr


makeDot :: Builder -> Expr -> Name -> (Lines, Builder)
makeDot indent expr field =
  let
    (lines, builder) =
      fromExpr indent Atomic expr
  in
    (lines, builder <> "." <> Name.toBuilder field)


makeBracketed :: Builder -> Expr -> Expr -> (Lines, Builder)
makeBracketed indent expr bracketedExpr =
  let
    (lines, builder) =
      fromExpr indent Atomic expr

    (bracketedLines, bracketedBuilder) =
      fromExpr indent Whatever bracketedExpr
  in
    ( merge lines bracketedLines
    , builder <> "[" <> bracketedBuilder <> "]"
    )



-- OPERATORS


fromPrefix :: PrefixOp -> Builder
fromPrefix op =
  case op of
    PrefixLNot   -> "!"
    PrefixBNot   -> "~"
    PrefixPlus   -> "+"
    PrefixMinus  -> "-"
    PrefixTypeof -> "typeof "


fromInfix :: InfixOp -> Builder
fromInfix op =
  case op of
    OpLT         -> " < "
    OpLEq        -> " <= "
    OpGT         -> " > "
    OpGEq        -> " >= "
    OpIn         -> " in "
    OpInstanceof -> " instanceof "
    OpEq         -> " == "
    OpNEq        -> " != "
    OpStrictEq   -> " === "
    OpStrictNEq  -> " !== "
    OpLAnd       -> " && "
    OpLOr        -> " || "
    OpMul        -> " * "
    OpDiv        -> " / "
    OpMod        -> " % "
    OpSub        -> " - "
    OpLShift     -> " << "
    OpSpRShift   -> " >> "
    OpZfRShift   -> " >>> "
    OpBAnd       -> " & "
    OpBXor       -> " ^ "
    OpBOr        -> " | "
    OpAdd        -> " + "
