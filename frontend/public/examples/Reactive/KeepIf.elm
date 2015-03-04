import Char exposing (isDigit)
import Graphics.Element exposing (..)
import Graphics.Input.Field as Field
import String


main : Varying Element
main =
  let allDigits content = String.all isDigit content.string
  in
      Signal.subscribe numbers
        |> Signal.keepIf allDigits Field.noContent 
        |> Varying.map display


numbers : Signal.Channel Field.Content
numbers =
  Signal.channel Field.noContent


display : Field.Content -> Element
display content =
  Field.field Field.defaultStyle (Signal.send numbers) "Only numbers!" content
