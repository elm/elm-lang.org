import Char
import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import Http
import Maybe
import Mailbox as Mb
import String
import Text


main : Varying Element
main =
  let msg = Text.plainText "Enter a valid zip code, such as 12345 or 90210."
      output fieldContent url response =
        flow down
          [ Field.field Field.defaultStyle (Mb.message content.mailbox) "Zip Code" fieldContent
          , Maybe.withDefault msg (Maybe.map (always (display response)) url)
          ]
  in
      Varying.map3 output content.stream url responses


input content : Stream.WritableStream Field.Content


-- Display a response

display : Result Http.Error String -> Element
display result =
  case result of
    Ok address -> leftAligned (Text.monospace (Text.fromString address))
    Err msg -> show msg


-- Send requests based on user input

loopback responses : Stream (Result Http.Error String)
loopback responses <-
    Stream.filterMap toUrl content.stream


toUrl : Field.Content -> Maybe String
toUrl content =
  let s = content.string in
  if String.length s == 5 && String.all Char.isDigit s
      then Just (Http.get anything ("http://zip.elevenbasetwo.com/v2/US/" ++ s))
      else Nothing
