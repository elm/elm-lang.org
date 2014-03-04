import String
import Graphics.Input (Input, input)
import Graphics.Input.Field (Content, noContent, field, defaultStyle, Forward, Backward)

main : Signal Element
main = lift2 above
         (field content.handle id defaultStyle "Forward" <~ content.signal)
         (field content.handle reverse defaultStyle "Backward" . reverse <~ content.signal)

content : Input Content
content = input noContent

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
