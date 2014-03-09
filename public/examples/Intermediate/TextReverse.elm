import Graphics.Input (Input, input)
import Graphics.Input.Field (Content, noContent, field, defaultStyle, Forward, Backward)
import String
import Window

main : Signal Element
main = display <~ Window.dimensions ~ content.signal

content : Input Content
content = input noContent

display : (Int,Int) -> Content -> Element
display (w,h) fieldContent =
    let txt = container 300 60 middle . width 300 . centered . toText in
    color lightOrange <| container w h middle <| flow down
    [ txt "Type in either field to reverse text:"
    , myField id "Forward" fieldContent
    , myField reverse "Backward" (reverse fieldContent)
    , txt "Lookup palindromes and emordnilaps to try to make sentences!"
    ]

myField : (Content -> Content) -> String -> Content -> Element
myField handler placeHolder fieldContent =
    container 300 50 middle <|
    field defaultStyle content.handle handler placeHolder fieldContent

reverse : Content -> Content
reverse content =
    let len = String.length content.string in
    { string = String.reverse content.string
    , selection = { start = len - content.selection.end
                  , end = len - content.selection.start
                  , direction = case content.selection.direction of
                                  Forward -> Backward
                                  Backward -> Forward
                  }
    }
