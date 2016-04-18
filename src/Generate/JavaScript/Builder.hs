{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.JavaScript.Builder (stmtsToText) where

import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)
import Language.ECMAScript3.Syntax
import Prelude hiding (lines)



-- CONVERT TO LAZY TEXT


stmtsToText :: [Statement a] -> LazyText.Text
stmtsToText stmts =
  toLazyText (fromStmtBlock "" stmts)



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


fromStmtBlock :: Builder -> [Statement a] -> Builder
fromStmtBlock indent stmts =
  mconcat (map (fromStmt indent) stmts)


fromStmt :: Builder -> Statement a -> Builder
fromStmt indent statement =
  case statement of
    BlockStmt _ stmts ->
      fromStmtBlock indent stmts

    EmptyStmt _ ->
      mempty

    ExprStmt _ expr ->
      indent <> snd (fromExpr indent Whatever expr) <> ";\n"

    IfStmt _ condition thenStmt elseStmt ->
      mconcat
        [ indent, "if (", snd (fromExpr indent Whatever condition), ") {\n"
        , fromStmt (deeper indent) thenStmt
        , indent, "} else {\n"
        , fromStmt (deeper indent) elseStmt
        , indent, "}\n"
        ]

    IfSingleStmt _ condition thenStmt ->
      mconcat
        [ indent, "if (", snd (fromExpr indent Whatever condition), ") {\n"
        , fromStmt (deeper indent) thenStmt
        , indent, "}\n"
        ]

    SwitchStmt _ expr clauses ->
      mconcat
        [ indent, "switch (", snd (fromExpr indent Whatever expr), ") {\n"
        , mconcat (map (fromClause (deeper indent)) clauses)
        , indent, "}\n"
        ]

    WhileStmt _ expr stmt ->
      mconcat
        [ indent, "while (", snd (fromExpr indent Whatever expr), ") {\n"
        , fromStmt (deeper indent) stmt
        , indent, "}\n"
        ]

    DoWhileStmt _ stmt expr ->
      mconcat
        [ indent, "do {\n"
        , fromStmt (deeper indent) stmt
        , indent, "} while(", snd (fromExpr indent Whatever expr), ");\n"
        ]

    BreakStmt _ Nothing ->
      indent <> "break;\n"

    BreakStmt _ (Just label) ->
      indent <> "break " <> fromId label <> ";\n"

    ContinueStmt _ Nothing ->
      indent <> "continue;\n"

    ContinueStmt _ (Just label) ->
      indent <> "continue " <> fromId label <> ";\n"

    LabelledStmt _ label stmt ->
      mconcat
        [ indent, fromId label, ":\n"
        , fromStmt indent stmt
        ]

    ForInStmt _ _ _ _ ->
      error "TODO"

    ForStmt _ _ _ _ _ ->
      error "TODO"

    TryStmt _ _ _ _ ->
      error "TODO"

    ThrowStmt _ expr ->
      indent <> "throw " <> snd (fromExpr indent Whatever expr) <> ";\n"

    ReturnStmt _ Nothing ->
      indent <> "return;\n"

    ReturnStmt _ (Just expr) ->
      indent <> "return " <> snd (fromExpr indent Whatever expr) <> ";\n"

    WithStmt _ _ _ ->
      error "TODO"

    VarDeclStmt _ [] ->
      mempty

    VarDeclStmt _ decls ->
      indent <> "var " <> commaNewlineSep indent (map (fromVarDecl indent) decls) <> ";\n"

    FunctionStmt _ name args stmts ->
      indent <> "function " <> fromId name <> "(" <> commaSep (map fromId args) <> ") {\n"
      <>
          fromStmtBlock (deeper indent) stmts
      <>
      indent <> "}\n"



-- SWITCH CLAUSES


fromClause :: Builder -> CaseClause a -> Builder
fromClause indent clause =
  case clause of
    CaseClause _ expr stmts ->
      indent <> "case " <> snd (fromExpr indent Whatever expr) <> ":\n"
      <> fromStmtBlock (deeper indent) stmts

    CaseDefault _ stmts ->
      indent <> "default:\n"
      <> fromStmtBlock (deeper indent) stmts



-- ID


fromId :: Id a -> Builder
fromId (Id _ name) =
  fromString name



-- VAR DECLS


fromVarDecl :: Builder -> VarDecl a -> Builder
fromVarDecl indent (VarDecl _ (Id _ name) maybeExpr) =
  case maybeExpr of
    Nothing ->
      fromString name

    Just expr ->
      fromString name <> " = " <> snd (fromExpr indent Whatever expr)



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


fromExpr :: Builder -> Grouping -> Expression a -> (Lines, Builder)
fromExpr indent grouping expression =
  case expression of
    StringLit _ string ->
      (One, quoted string)

    RegexpLit _ _ _ _ ->
      error "TODO"

    NumLit _ n ->
      (One, realFloat n)

    IntLit _ n ->
      (One, decimal n)

    BoolLit _ True ->
      (One, "true")

    BoolLit _ False ->
      (One, "false")

    NullLit _ ->
      (One, "null")

    ArrayLit _ exprs ->
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

    ObjectLit _ fields ->
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

    ThisRef _ ->
      (One, "this")

    VarRef _ name ->
      (One, fromId name)

    DotRef _ expr (Id _ name) ->
      makeDot indent expr name

    BracketRef _ expr bracketedExpr ->
      makeBracketed indent expr bracketedExpr

    NewExpr _ _ _ ->
      error "TODO"

    PrefixExpr _ op expr ->
      let
        (lines, builder) =
          fromExpr indent Atomic expr
      in
        ( lines
        , parensFor grouping (fromPrefix op <> builder)
        )

    UnaryAssignExpr _ _ _ ->
      error "TODO"

    InfixExpr _ op leftExpr rightExpr ->
      let
        (leftLines, left) =
          fromExpr indent Atomic leftExpr

        (rightLines, right) =
          fromExpr indent Atomic rightExpr
      in
        ( merge leftLines rightLines
        , parensFor grouping (left <> fromInfix op <> right)
        )

    CondExpr _ condExpr thenExpr elseExpr ->
      let
        condB = snd (fromExpr indent Atomic condExpr)
        thenB = snd (fromExpr indent Atomic thenExpr)
        elseB = snd (fromExpr indent Atomic elseExpr)
      in
        ( Many
        , parensFor grouping (condB <> " ? " <> thenB <> " : " <> elseB)
        )

    AssignExpr _ op lValue expr ->
      let
        (leftLines, left) =
          fromLValue indent lValue

        (rightLines, right) =
          fromExpr indent Whatever expr
      in
        ( merge leftLines rightLines
        , parensFor grouping (left <> fromAssign op <> right)
        )

    ListExpr _ _ ->
      error "TODO"

    CallExpr _ function args ->
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

    FuncExpr _ maybeName args stmts ->
      (,) Many $
        "function " <> maybe mempty fromId maybeName <> "(" <> commaSep (map fromId args) <> ") {\n"
        <>
            fromStmtBlock (deeper indent) stmts
        <>
        indent <> "}"



-- FIELDS


fromField :: Builder -> (Prop a, Expression a) -> (Lines, Builder)
fromField indent (prop, expr) =
  let
    (lines, builder) =
      fromExpr indent Whatever expr
  in
    ( lines
    , fromProp prop <> ": " <> builder
    )


fromProp :: Prop a -> Builder
fromProp prop =
  case prop of
    PropId _ name ->
      fromId name

    PropString _ string ->
      quoted string

    PropNum _ n ->
      decimal n



-- STRINGS


quoted :: String -> Builder
quoted string =
  fromString ('\'' : foldr escapeCons "'" string)


-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Grammar_and_types#String_literals
escapeCons :: Char -> String -> String
escapeCons char rest =
  case char of
    '\b' -> '\\' : 'b' : rest
    '\f' -> '\\' : 'f' : rest
    '\n' -> '\\' : 'n' : rest
    '\r' -> '\\' : 'r' : rest
    '\t' -> '\\' : 't' : rest
    '\v' -> '\\' : 'v' : rest
    '\"' -> '\\' : '"' : rest
    '\'' -> '\\' : '\'' : rest
    '\\' -> '\\' : '\\' : rest
    _    -> char : rest



-- VALUES


fromLValue :: Builder -> LValue a -> (Lines, Builder)
fromLValue indent lValue =
  case lValue of
    LVar _ name ->
      (One, fromString name)

    LDot _ expr field ->
      makeDot indent expr field

    LBracket _ expr bracketedExpr ->
      makeBracketed indent expr bracketedExpr


makeDot :: Builder -> Expression a -> String -> (Lines, Builder)
makeDot indent expr field =
  let
    (lines, builder) =
      fromExpr indent Atomic expr
  in
    (lines, builder <> "." <> fromString field)


makeBracketed :: Builder -> Expression a -> Expression a -> (Lines, Builder)
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
    PrefixVoid   -> "void "
    PrefixDelete -> "delete "


fromAssign :: AssignOp -> Builder
fromAssign op =
  case op of
    OpAssign         -> " = "
    OpAssignAdd      -> " += "
    OpAssignSub      -> " -= "
    OpAssignMul      -> " *= "
    OpAssignDiv      -> " /= "
    OpAssignMod      -> " %= "
    OpAssignLShift   -> " <<= "
    OpAssignSpRShift -> " >>= "
    OpAssignZfRShift -> " >>>= "
    OpAssignBAnd     -> " &= "
    OpAssignBXor     -> " ^= "
    OpAssignBOr      -> " |= "


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
    OpStrictNEq  -> " !=== "
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
