
module Type.PrettyPrint where

import Text.PrettyPrint
import qualified SourceSyntax.PrettyPrint as Src

data ParensWhen = Fn | App | Never

class PrettyType a where
  pretty :: ParensWhen -> a -> Doc

commaSep docs = sep (punctuate comma docs)

parensIf bool doc = if bool then parens doc else doc

reprime = Src.reprime