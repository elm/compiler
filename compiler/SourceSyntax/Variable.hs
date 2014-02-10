
module SourceSyntax.Variable where

import qualified Text.PrettyPrint as P
import SourceSyntax.PrettyPrint

newtype Raw = Raw String
    deriving (Eq,Ord,Show)

instance Pretty Raw where
    pretty (Raw var) = variable var
