
module Type.PrettyPrint where

import Text.PrettyPrint

data ParensWhen = Fn | App | Never

class PrettyType a where
  pretty :: ParensWhen -> a -> Doc

commaSep docs = sep (punctuate comma docs)

parensIf bool doc = if bool then parens doc else doc
