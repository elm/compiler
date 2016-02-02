module Generate.JavaScript.Literal (literal) where

import qualified Language.ECMAScript3.Syntax as JS

import qualified AST.Literal as Lit
import qualified Generate.JavaScript.BuiltIn as BuiltIn


literal :: Lit.Literal -> JS.Expression ()
literal lit =
  case lit of
    Lit.Chr char ->
        BuiltIn.character char

    Lit.Str string ->
        BuiltIn.string string

    Lit.IntNum number ->
        JS.IntLit () number

    Lit.FloatNum number ->
        JS.NumLit () number

    Lit.Boolean boolean ->
        JS.BoolLit () boolean
