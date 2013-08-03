
module SourceSyntax.PrettyPrint where

import Text.PrettyPrint
import qualified SourceSyntax.Helpers as Help

class Pretty a where
  pretty :: a -> Doc

instance Pretty () where
  pretty () = empty

commaCat docs = cat (punctuate comma docs)
commaSep docs = sep (punctuate comma docs)

parensIf bool doc = if bool then parens doc else doc

variable x = parensIf (Help.isOp x) (text x)