{-# LANGUAGE FlexibleInstances #-}
module AST.PrettyPrint where

import Control.Monad.Trans.Error
import Text.PrettyPrint as P
import qualified AST.Helpers as Help

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty doc = doc

instance Pretty String where
  pretty = P.text

renderPretty :: (Pretty a) => a -> String
renderPretty e = render (pretty e)

commaCat docs = cat (punctuate comma docs)
commaSep docs = sep (punctuate comma docs)

parensIf :: Bool -> Doc -> Doc
parensIf bool doc = if bool then parens doc else doc

variable :: String -> Doc
variable x =
    if Help.isOp x then parens (text x) else text x

eightyCharLines :: Int -> String -> String
eightyCharLines indent message = answer
    where
      (answer,_,_) = foldl step (spaces, indent-1, "") chunks

      chunks = map (\w -> (w, length w)) (words message)
      spaces = replicate indent ' '
      step (sentence, slen, space) (word, wlen)
          | slen + wlen > 79 = (sentence ++ "\n" ++ spaces ++ word, indent + wlen, " ")
          | otherwise        = (sentence ++ space ++ word, slen + wlen + length space, " ")

instance ErrorList Doc where
    listMsg str = [ P.text str ]
