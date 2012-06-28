
module Log where

import Foreign.JavaScript
import Signal.Input


foreign export jsevent "logMessage"
  messages :: Signal JSString

(field, message) = textField ""
(butn , pressed) = button " Log "

messages =
  lift castStringToJSString $ keepWhen pressed "" message

main = field `beside` butn