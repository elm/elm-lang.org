import Char
import String
import Maybe
import Http
import Graphics.Input as Input

main : Signal Element
main =
  let msg = plainText "Enter a valid zip code, such as 12345 or 90210."
      output content url response =
          flow down [ Input.field portal id "Zip Code" content
                    , Maybe.maybe msg (always (display response)) url
                    ]
  in lift3 output content url responses

(content, portal) = Input.input Input.noContent

-- Display a response

display : Http.Response String -> Element
display response = 
  case response of
    Http.Success address -> text . monospace <| toText address
    Http.Waiting -> image 16 16 "waiting.gif"
    Http.Failure _ _ -> asText response

-- Send requests based on user input

responses : Signal (Http.Response String)
responses = Http.sendGet (Maybe.maybe "" id <~ url)

url : Signal (Maybe String)
url = lift (\c -> toUrl c.string) content

toUrl : String -> Maybe String
toUrl s =
    if String.length s == 5 && String.all Char.isDigit s
      then Just ("http://zip.elevenbasetwo.com/v2/US/" ++ s)
      else Nothing
