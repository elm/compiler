
module SourceSyntax.PrettyPrint where

import Text.PrettyPrint
import SourceSyntax.Location

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty (Located a) where
  pretty (L _ _ e) = pretty e

commaCat docs = cat (punctuate comma docs)

parensIf bool doc = if bool then parens doc else doc