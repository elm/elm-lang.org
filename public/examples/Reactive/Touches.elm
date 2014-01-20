-- Try this out on an iOS or Android device by removing "edit"
-- from the URL: http://elm-lang.org/examples/Reactive/Touches.elm

import Touch

main = lift (flow down . map asText) Touch.touches