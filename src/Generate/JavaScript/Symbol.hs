import qualified Data.Char as Char
import Data.Word (Word16, Word32)



-- SYMBOLS


data Symbol =
  Symbol
    { _home :: !Word16
    , _name :: !Word32
    }



-- BINARY


instance Binary Symbol where
  put (Symbol home name) =
    put home >> put name

  get =
    liftM2 Symbol get get



-- EXPRESSION


toExpr :: Symbol -> JS.Expr
toExpr (Symbol home name) =
  JS.DotRef (JS.ref (homeToText home)) (JS.Id (nameToText name))


homeToText :: Word16 -> Text
homeToText word =
  Text.pack ('$' : show word)


nameToText :: Word32 -> Text
nameToText word =
  Text.pack (makeName word "")


makeName :: Word32 -> String -> String
makeName word str =
  if word < 26 then
    makeChar 97 word : str

  else if word < 52 then
    makeChar 65 (word - 26) : str

  else
    let
      (next, leftover) =
        quotRem word 52
    in
      makeName next (makeInnerChar leftover : str)


makeInnerChar :: Word32 -> Char
makeInnerChar word =
  if word < 26 then
    makeChar 97 word
  else
    makeChar 65 (word - 26)


{-# INLINE makeChar #-}
makeChar :: Word32 -> Word32 -> Char
makeChar root offset =
  Char.chr (fromIntegral (root + offset))
