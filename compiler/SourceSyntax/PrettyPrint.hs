
module SourceSyntax.PrettyPrint where

import Text.PrettyPrint
import SourceSyntax.Location
import qualified SourceSyntax.Helpers as Help

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty (Located a) where
  pretty (L _ e) = pretty e

instance Pretty () where
  pretty () = empty

commaCat docs = cat (punctuate comma docs)
commaSep docs = sep (punctuate comma docs)

parensIf bool doc = if bool then parens doc else doc

variable x = parensIf (Help.isOp x) (text x)