import Graphics.Element exposing (..)
import Markdown
import Touch


main : Varying Element
main =
  Varying.map (above msg << flow down << List.map show) Touch.touches


msg : Element
msg = Markdown.toElement """

<a href="/examples/Reactive/Touches.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

"""