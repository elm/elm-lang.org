
module Form where

import Data.Maybe (mapMaybe)
import Foreign.JavaScript
import Signal.Input
import Signal.Window as Win


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

foreign export jsevent "elm_redirect"
  redirectTo :: Signal JSString


-- Display

field txt fld =
  flow right
    [ container 120 32 midRight $ plainText txt
    , container 200 32 middle $ size 180 26 fld ]

showErrors presses errs =
  if presses == 0 || isEmpty errs then spacer 10 10 else
    flow down $ map (text . Text.color red . toText) errs

form presses errs =
  let entry = color (rgb 230 230 230) . flow down $
               [ field "First Name:"     firstBox
               , field "Last Name:"      lastBox
               , field "Your Email:"     emailBox
               , field "Re-enter Email:" remailBox
               , showErrors presses errs
               , container 310 40 midRight $ size 60 30 butn
               ]
  in  container (widthOf entry + 60) (heightOf entry + 60) middle entry 

main = lift2 form pressCount errors
