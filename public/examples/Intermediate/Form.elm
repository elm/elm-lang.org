import Graphics.Input.Field as Field
import Graphics.Input as Input
import Http
import String
import Text
import Window

main : Signal Element
main = scene <~ Window.dimensions
              ~ lift5 form first.signal last.signal email.signal remail.signal errors

-- Signals and Inputs
first  = Input.input Field.noContent
last   = Input.input Field.noContent
email  = Input.input Field.noContent
remail = Input.input Field.noContent
submit = Input.input ()

hasAttempted : Signal Bool
hasAttempted =
    let isPositive c = c > 0
    in  isPositive <~ count submit.signal

sendable : Signal Bool
sendable = keepWhen hasAttempted False (isEmpty <~ errors)

errors : Signal [String]
errors =
    let rawErrors = lift4 getErrors first.signal last.signal email.signal remail.signal
    in  keepWhen hasAttempted [] <| merge rawErrors (sampleOn submit.signal rawErrors)

getErrors : Field.Content -> Field.Content -> Field.Content -> Field.Content -> [String]
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
        filterMap identity <| map activeError checks

port redirect : Signal String
port redirect =
    keepWhen sendable "" <| sampleOn submit.signal <|
    lift3 url first.signal last.signal email.signal

url : Field.Content -> Field.Content -> Field.Content -> String
url first last email = 
    "/login?first=" ++ first.string ++ "&last=" ++ last.string ++ "&email="++ email.string

getLogin : Signal String -> Signal (Http.Response String)
getLogin req = Http.send <| lift (\r -> Http.post r "") req


-- Display
scene : (Int,Int) -> Element -> Element
scene (w,h) form =
    color charcoal . flow down <|
      [ spacer w 50
      , container w (h-50) midTop form
      ]

form : Field.Content -> Field.Content -> Field.Content -> Field.Content -> [String] -> Element
form first' last' email' remail' errors =
    color lightGrey . flow down <|
      [ container 340 60 middle . leftAligned . Text.height 32 <| toText "Example Sign Up"
      , field "First Name:"     first.handle  first'
      , field "Last Name:"      last.handle   last'
      , field "Your Email:"     email.handle  email'
      , field "Re-enter Email:" remail.handle remail'
      , showErrors errors
      , container 300 50 midRight <| size 60 30 <| Input.button submit.handle () "Submit"
      ]

field : String -> Input.Handle Field.Content -> Field.Content -> Element
field label handle content =
  flow right
    [ container 120 36 midRight <| plainText label
    , container 220 36 middle <| size 180 26 <|
      Field.field Field.defaultStyle handle identity "" content
    ]

showErrors : [String] -> Element
showErrors errs =
  flow down
    [ spacer 10 10
    , if isEmpty errs
        then spacer 0 0
        else flow down <| map (width 340 . centered . Text.color red . toText) errs
    ]
