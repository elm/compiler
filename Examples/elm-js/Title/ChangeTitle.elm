
module ChangeTitle where

import Foreign.JavaScript
import Signal.Input


foreign export jsevent "changeTitle"
  title :: Signal JSString

(field, title) = let (f,t) = textField "" in
                 (f, lift castStringToJSString t)

main = plainText "Change this page's title to: " `beside` field