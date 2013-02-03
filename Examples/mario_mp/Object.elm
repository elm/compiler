
module Object where

import JavaScript
import JSON

main = plainText . castJSStringToString . (toPrettyJSString "") . fromList $ [ ("answer", JsonNumber 42) ]

