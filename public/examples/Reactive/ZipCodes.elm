import Char
import String
import Maybe
import Http
import Graphics.Input as Input
import Graphics.Input.Field as Field

main : Signal Element
main =
  let msg = plainText "Enter a valid zip code, such as 12345 or 90210."
      output fieldContent url response =
          flow down [ Field.field Field.defaultStyle content.handle id "Zip Code" fieldContent
                    , Maybe.maybe msg (always (display response)) url
                    ]
  in lift3 output content.signal url responses

content : Input.Input Field.Content
content = Input.input Field.noContent

-- Display a response

display : Http.Response String -> Element
display response = 
  case response of
    Http.Success address -> leftAligned . monospace <| toText address
    Http.Waiting -> image 16 16 "waiting.gif"
    Http.Failure _ _ -> asText response

-- Send requests based on user input

responses : Signal (Http.Response String)
responses = Http.sendGet (Maybe.maybe "" id <~ url)

url : Signal (Maybe String)
url = lift toUrl content.signal

toUrl : Field.Content -> Maybe String
toUrl content =
    let s = content.string in
    if String.length s == 5 && String.all Char.isDigit s
      then Just ("http://zip.elevenbasetwo.com/v2/US/" ++ s)
      else Nothing
