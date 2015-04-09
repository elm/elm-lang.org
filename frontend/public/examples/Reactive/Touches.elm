import Graphics.Element exposing (..)
import Markdown
import Touch


main : Signal Element
main =
  Signal.map (above msg << flow down << List.map show) Touch.touches


msg : Element
msg = Markdown.toElement """

<a href="/examples/Reactive/Touches.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

"""