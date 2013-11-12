module Form where

import Graphics.Input as Input
import Http
import String

getErrors : String -> String -> String -> String -> [String]
getErrors first last email remail =
  justs <| map (\(err,msg) -> if err then Just msg else Nothing)
  [ (String.isEmpty first , "First name required.")
  , (String.isEmpty last  , "Last name required.")
  , (String.isEmpty email , "Must enter your email address.")
  , (String.isEmpty remail, "Must re-enter your email address.")
  , (email /= remail      , "Email addresses do not match.")
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

sendAttempt : Signal Bool
sendAttempt = lift (\c -> c > 0) (count press)

errors : Signal [String]
errors = keepWhen sendAttempt []
                  (lift4 getErrors first last email remail)

sendable : Signal Bool
sendable = sampleOn press (lift2 (&&) sendAttempt (lift isEmpty errors))

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

userEntry : Element -> Element -> Element -> Element -> [String] -> Element
userEntry first last email remail errors =
    color (rgb 230 230 230) . flow down <|
        [ fieldWith "First Name:" first
        , fieldWith "Last Name:"  last
        , fieldWith "Your Email:" email
        , fieldWith "Re-enter Email:" remail
        , showErrors errors
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

inputForm = lift5 userEntry firstBox lastBox emailBox remailBox errors 
inputBox = container 360 360 topLeft <~ inputForm
loginResponse = prettyPrint <~ getLogin sendRequest

main : Signal Element
main = above <~ inputBox ~ loginResponse
