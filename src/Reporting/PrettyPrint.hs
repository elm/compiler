{-# LANGUAGE FlexibleInstances #-}
module Reporting.PrettyPrint where

import Text.PrettyPrint as P
import qualified AST.Helpers as Help


class Pretty a where
  pretty :: Bool -> a -> Doc


instance Pretty String where
  pretty _ str =
      P.text str


commaCat :: [Doc] -> Doc
commaCat docs =
  cat (punctuate comma docs)


commaSep :: [Doc] -> Doc
commaSep docs =
  sep (punctuate comma docs)


parensIf :: Bool -> Doc -> Doc
parensIf bool doc =
  if bool then parens doc else doc


variable :: String -> Doc
variable x =
  if Help.isOp x then parens (text x) else text x
