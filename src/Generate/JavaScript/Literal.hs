module Generate.JavaScript.Literal (literal) where

import qualified AST.Literal as Lit
import qualified Generate.JavaScript.Builder as JS
import qualified Generate.JavaScript.BuiltIn as BuiltIn
import Generate.JavaScript.Variable (Generator)


literal :: Lit.Literal -> Generator JS.Expr
literal lit =
  case lit of
    Lit.Chr char ->
      BuiltIn.char char

    Lit.Str string ->
      return $ JS.String string

    Lit.IntNum number ->
      return $ JS.Int number

    Lit.FloatNum number ->
      return $ JS.Float number

    Lit.Boolean boolean ->
      return $ JS.Bool boolean
