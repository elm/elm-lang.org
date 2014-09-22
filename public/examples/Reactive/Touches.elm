
import Touch

main : Signal Element
main =
    lift (above msg << flow down << map asText) Touch.touches

msg : Element
msg = [markdown|

<a href="/examples/Reactive/Touches.elm" target="_top">Try it fullscreen</a>
if you are on iOS or Android.

|]
