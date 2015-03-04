import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input.Field exposing (Content, noContent, field, defaultStyle, Direction(..))
import String
import Text
import Window

main : Varying Element
main = Varying.map2 view Window.dimensions (Signal.subscribe content)

content : Signal.Channel Content
content = Signal.channel noContent

view : (Int,Int) -> Content -> Element
view (w,h) fieldContent =
  let viewText str =
        Text.fromString str
          |> Text.color lightGrey
          |> Text.centered
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
  field defaultStyle (Signal.send content << handler) placeHolder fieldContent
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
