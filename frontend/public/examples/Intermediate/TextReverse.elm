import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (Content, noContent, field, defaultStyle, Direction(..))
import String
import Text
import Window

main : Signal Element
main = Signal.map2 view Window.dimensions content.signal

content : Signal.Mailbox Content
content = Signal.mailbox noContent

view : (Int,Int) -> Content -> Element
view (w,h) fieldContent =
  let viewText str =
        Text.fromString str
          |> Text.color lightGrey
          |> centered
          |> width 300
          |> container 300 60 middle
  in
    color lightPurple <|
    container w h middle <|
    flow down
    [ viewText "Type in either field to reverse text:"
    , myField identity "Forward" fieldContent
    , myField reverse "Backward" (reverse fieldContent)
    , viewText "Lookup palindromes and emordnilaps to try to make sentences!"
    ]

myField : (Content -> Content) -> String -> Content -> Element
myField handler placeHolder fieldContent =
  field defaultStyle (Signal.message content.address << handler) placeHolder fieldContent
    |> container 300 50 middle

reverse : Content -> Content
reverse content =
  let len = String.length content.string
  in
    { string = String.reverse content.string
    , selection =
        { start = len - content.selection.end
        , end = len - content.selection.start
        , direction =
            case content.selection.direction of
              Forward -> Backward
              Backward -> Forward
        }
    }
