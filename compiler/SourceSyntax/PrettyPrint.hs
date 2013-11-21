
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

variable x =
    if Help.isOp x then parens (text x)
                   else text (reprime x)

reprime :: String -> String
reprime = map (\c -> if c == '$' then '\'' else c)

eightyCharLines :: Int -> String -> String
eightyCharLines indent message = answer
    where
      (answer,_,_) = foldl step (spaces, indent-1, "") chunks

      chunks = map (\w -> (w, length w)) (words message)
      spaces = replicate indent ' '
      step (sentence, slen, space) (word, wlen)
          | slen + wlen > 79 = (sentence ++ "\n" ++ spaces ++ word, indent + wlen, " ")
          | otherwise        = (sentence ++ space ++ word, slen + wlen + length space, " ")
