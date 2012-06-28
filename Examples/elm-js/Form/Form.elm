
module Form where

import Data.Maybe (mapMaybe)
import Foreign.JavaScript
import Signal.Input
import Signal.Window as Win

foreign export jsevent "redirect"
  redirectTo :: Signal JSString


-- Helpers

isEmpty xs = case xs of { [] -> True ; _ -> False }

getErrors first last email remail =
  mapMaybe (\(err,msg) -> if err then Just msg else Nothing)
  [ (isEmpty first  , "First name required.")
  , (isEmpty last   , "Last name required.")
  , (isEmpty email  , "Must enter your email address.")
  , (isEmpty remail , "Must re-enter your email address.")
  , (email /= remail, "Email addresses do not match.")
  ]

url first last email =
  "login?first=" ++ first ++ "&last=" ++ last ++ "&email="++ email


-- Signals

(firstBox , first)  = textField "First Name"
(lastBox  , last)   = textField "Last Name"
(emailBox , email)  = textField "Your Email"
(remailBox, remail) = textField "Re-enter Email"
(butn     , press)  = button "Submit"

pressCount = foldp (\p c -> if p then c+1 else c) 0 press
errors = lift4 getErrors first last email remail
sendable = lift2 (&&) press (lift isEmpty errors)

redirectTo = lift castStringToJSString $
             keepWhen sendable "" (lift3 url first last email)


-- Display

field txt fld =
  width 400 . flow right $
    [ size 120 30 . box 6 $ plainText txt
    , size 200 30 . box 5 $ size 180 20 fld
    ]

showErrors presses errs =
  if presses == 0 || isEmpty errs then rectangle 10 10 else
    flow down $ map (text . Text.color red . toText) errs

form presses errs (w,h) =
  size w h . box 5 . color (rgb 230 230 230) . flow down $
    [ field "First Name:"     firstBox
    , field "Last Name:"      lastBox
    , field "Your Email:"     emailBox
    , field "Re-enter Email:" remailBox
    , showErrors presses errs
    , size 310 40 . box 6 $ size 60 30 butn
    ]

main = lift3 form pressCount errors Win.dimensions
