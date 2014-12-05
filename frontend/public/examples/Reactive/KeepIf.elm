import Char (isDigit)
import Graphics.Element (..)
import Graphics.Input.Field as Field
import Signal
import String


main : Signal Element
main =
  let allDigits content = String.all isDigit content.string
  in
      Signal.subscribe numbers
        |> Signal.keepIf allDigits Field.noContent 
        |> Signal.map display


numbers : Signal.Channel Field.Content
numbers =
  Signal.channel Field.noContent


display : Field.Content -> Element
display content =
  Field.field Field.defaultStyle (Signal.send numbers) "Only numbers!" content
