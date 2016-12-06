module Generate.JavaScript.Literal (literal) where

import qualified AST.Literal as Lit
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.BuiltIn as BuiltIn


literal :: Lit.Literal -> JS.Expr
literal lit =
  case lit of
    Lit.Chr char ->
        BuiltIn.character char

    Lit.Str string ->
        JS.String string

    Lit.IntNum number ->
        JS.Int number

    Lit.FloatNum number ->
        JS.Float number

    Lit.Boolean boolean ->
        JS.Bool boolean
