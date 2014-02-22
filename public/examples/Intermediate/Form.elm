import Graphics.Input (input, FieldContent, noContent)
import Graphics.Input as Input
import Http
import String
import Window

getErrors : FieldContent -> FieldContent -> FieldContent -> FieldContent -> [String]
getErrors first last email remail =
  let empty content = String.isEmpty content.string
      checks = [ (empty first , "First name required.")
               , (empty last  , "Last name required.")
               , (empty email , "Must enter your email address.")
               , (empty remail, "Must re-enter your email address.")
               , (email.string /= remail.string, "Email addresses do not match.")
               ]
      activeError (err,msg) = if err then Just msg else Nothing
  in
      justs <| map activeError checks


-- Signals and Inputs
first  = input noContent
last   = input noContent
email  = input noContent
remail = input noContent
submit = input ()

hasAttempted : Signal Bool
hasAttempted =
  let isPositive c = c > 0
  in  isPositive <~ count submit.signal

errors : Signal [String]
errors = keepWhen hasAttempted []
         (lift4 getErrors first.signal last.signal email.signal remail.signal)

sendable : Signal Bool
sendable = sampleOn submit.signal (isEmpty <~ errors)

-- Display
field : String -> Input.Handle FieldContent -> FieldContent -> Element
field label handle content =
  flow right
    [ container 200 32 midRight <| plainText label
    , container 200 32 middle <| size 180 26 <| Input.field handle id "" content
    ]

showErrors : [String] -> Element
showErrors errs =
  if isEmpty errs then spacer 10 10 else
    flow down <| map (text . Text.color red . toText) errs

formTitle : String -> Element
formTitle str = width 400 . centered . Text.height 38 <| toText str

userEntry : FieldContent -> FieldContent -> FieldContent -> FieldContent -> [String] -> Element
userEntry first' last' email' remail' errors =
  color (rgb 230 230 230) . flow down <|
    [ formTitle "Example Form"
    , field "First Name:"     first.handle  first'
    , field "Last Name:"      last.handle   last'
    , field "Your Email:"     email.handle  email'
    , field "Re-enter Email:" remail.handle remail'
    , showErrors errors
    , container 390 50 midRight <| size 60 30 <| Input.button submit.handle () "Submit"
    ]

-- HTTP control
sendRequest : Signal String
sendRequest = keepWhen sendable "" <| lift3 url first.signal last.signal email.signal

url : FieldContent -> FieldContent -> FieldContent -> String
url first last email = 
    "/login?first=" ++ first.string ++ "&last=" ++ last.string ++ "&email="++ email.string

getLogin : Signal String -> Signal (Http.Response String)
getLogin req = Http.send <| lift (\r -> Http.post r "") req

-- HTTP printing  
prettyPrint : Http.Response String -> Element
prettyPrint res =
  case res of
    Http.Success str -> plainText str
    Http.Waiting     -> spacer 0 0
    Http.Failure _ _ -> spacer 0 0

inputForm = lift5 userEntry first.signal last.signal email.signal remail.signal errors

inputBox = 
  let
    cmaker inForm bWidth bHeight = container bWidth bHeight topLeft inForm
    w = lift widthOf inputForm
    h = lift heightOf inputForm
  in 
    lift3 cmaker inputForm w h
    
loginResponse = lift prettyPrint (getLogin sendRequest)

scene (w,h) box result =
  flow down 
    [ spacer w 50
    , container w ((heightOf box)) midTop box
    , container w (h - heightOf box) midTop result
    ]

main : Signal Element
main = lift3 scene Window.dimensions inputBox loginResponse
