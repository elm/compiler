
module Type.PrettyPrint where

import Text.PrettyPrint
import qualified AST.PrettyPrint as Src

data ParensWhen = Fn | App | Never

class PrettyType a where
  pretty :: ParensWhen -> a -> Doc

commaSep :: [Doc] -> Doc
commaSep docs = sep (punctuate comma docs)

parensIf :: Bool -> Doc -> Doc
parensIf bool doc = if bool then parens doc else doc
