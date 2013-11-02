module Form where

import Graphics.Input as Input
import Http

getErrors : String -> String -> String -> String -> [String]
getErrors first last email remail =
  justs <| map (\(err,msg) -> if err then Just msg else Nothing)
  [ (isEmpty first  , "First name required.")
  , (isEmpty last   , "Last name required.")
  , (isEmpty email  , "Must enter your email address.")
  , (isEmpty remail , "Must re-enter your email address.")
  , (email /= remail, "Email addresses do not match.")
  ]

url : String -> String -> String -> String
url first last email = 
    "/login?first=" ++ first ++ "&last=" ++ last ++ "&email="++ email

-- Signals
(firstBox , first)  = Input.field "First Name"
(lastBox  , last)   = Input.field "Last Name"
(emailBox , email)  = Input.field "Your Email"
(remailBox, remail) = Input.field "Re-enter Email"
(butn     , press)  = Input.button "Submit"

errors : Signal [String]
errors = lift4 getErrors first last email remail

sendable : Signal Bool
sendable = sampleOn press <| lift isEmpty errors

-- Display
fieldWith : String -> Element -> Element
fieldWith txt fld =
  flow right
    [ container 120 32 midRight <| plainText txt
    , container 200 32 middle <| size 180 26 fld ]

showErrors : [String] -> Element
showErrors errs =
  if isEmpty errs then spacer 10 10 else
    flow down <| map (text . Text.color red . toText) errs

entry : Element -> Element -> Element -> Element -> [String] -> Element
entry f l em r e = color (rgb 230 230 230) . flow down <|
               [ fieldWith "First Name:" f
               , fieldWith "Last Name:"  l
               , fieldWith "Your Email:" em
               , fieldWith "Re-enter Email:" r
               , showErrors e
               , container 310 40 midRight <| size 60 30 butn
               ]

-- HTTP control
sendRequest : Signal String
sendRequest = keepWhen sendable "" <| lift3 url first last email

getLogin : Signal String -> Signal (Http.Response String)
getLogin req = Http.send <| lift (\r -> Http.post r "") req

-- HTTP printing  
prettyPrint : Http.Response String -> Element
prettyPrint res = case res of
  Http.Waiting -> plainText ""
  Http.Failure _ _ -> plainText ""
  Http.Success a -> plainText a

inputForm =entry <~ firstBox ~ lastBox ~ emailBox ~ remailBox ~ errors 
inputBox = lift (container 360 360 topLeft) <| inputForm
loginResponse = lift prettyPrint <| getLogin sendRequest

main : Signal Element
main = above <~ inputBox ~ loginResponse
