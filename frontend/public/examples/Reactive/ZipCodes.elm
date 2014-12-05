import Char
import Graphics.Element (..)
import Graphics.Input.Field as Field
import Http
import Maybe
import Signal
import String
import Text


main : Signal Element
main =
  let msg = Text.plainText "Enter a valid zip code, such as 12345 or 90210."
      output fieldContent url response =
        flow down
          [ Field.field Field.defaultStyle (Signal.send content) "Zip Code" fieldContent
          , Maybe.withDefault msg (Maybe.map (always (display response)) url)
          ]
  in
      Signal.map3 output (Signal.subscribe content) url responses


content : Signal.Channel Field.Content
content =
  Signal.channel Field.noContent


-- Display a response

display : Http.Response String -> Element
display response = 
  case response of
    Http.Success address -> Text.leftAligned (Text.monospace (Text.fromString address))
    Http.Waiting -> image 16 16 "waiting.gif"
    Http.Failure _ _ -> Text.asText response


-- Send requests based on user input

responses : Signal (Http.Response String)
responses =
  Http.sendGet (Signal.map (Maybe.withDefault "") url)


url : Signal (Maybe String)
url =
  Signal.map toUrl (Signal.subscribe content)


toUrl : Field.Content -> Maybe String
toUrl content =
  let s = content.string in
  if String.length s == 5 && String.all Char.isDigit s
      then Just ("http://zip.elevenbasetwo.com/v2/US/" ++ s)
      else Nothing
