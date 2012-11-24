import Website.Skeleton
import Website.Tiles
import Website.ColorScheme

intro = [markdown|

### The Elm Programming Language

Elm aims to make front-end web development more pleasant.
It introduces a new approach to GUI programming that corrects the
systemic problems of HTML, CSS, and JavaScript.
Elm allows you to quickly and easily [work with visual layout][flow],
[use the canvas][canvas], [manage complicated user input][frp],
and [escape from callback hell][escape].

  [flow]:    /edit/examples/Elements/FlowDown2.elm "Flow down example"
  [canvas]:  /edit/examples/Reactive/Transforms.elm "Canvas Example"
  [escape]:  /learn/Escape-from-Callback-Hell.elm
  [why-elm]: http://www.testblogpleaseignore.com/2012/06/21/why-elm/ "Why Elm?"
  [frp]:     /learn/What-is-FRP.elm "functional reactive programming"
  [http]:    /edit/examples/JavaScript/ZipCodes.elm "HTTP requests"

|]

features = [markdown|

#### Features

[Functional Reactive Programming][frp]

[Strong][strong] / [static][static] types, [type inference][infer]

[Native Markdown support](/edit/examples/Elements/Markdown.elm)

[Libraries](/Documentation.elm) and [Module System][modules]

  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [strong]: http://en.wikipedia.org/wiki/Strong_typing "strongly"
  [static]: http://en.wikipedia.org/wiki/Type_system#Static_typing "statically"
  [infer]:  http://en.wikipedia.org/wiki/Type_inference "type inference"
  [modules]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Module System"

|]

news = [markdown|

#### News

[Escape from Callback Hell][escape]: Never write a callback again!

[Making Pong in Elm][pong]: a comprehensive walkthrough of how to make purely functional games.

  [escape]: /learn/Escape-from-Callback-Hell.elm "Escape from Callback Hell"
  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"

|]

contact = [markdown|

#### Contact / Contribute

Elm's [mailing list][7] is the place to go for questions, announcements, and discussion.
Or see if anyone is on IRC ([`#elm` on freenode][irc]). There are also tons of ways to
[contribute to Elm](/Contribute.elm).

  [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
  [irc]: http://webchat.freenode.net/?channels=elm "#elm"

|]

addFolder folder = map (\(x,y) -> (x, y, folder))

examples = addFolder "Intermediate/"
  [ ("Analog Clock", "Clock")
  , ("Slide Show", "SlideShow")
  , ("Stamps", "Stamps")
  , ("Diagrams", "Physics")
  , ("Fibonacci Tiles", "FibonacciTiles")
  ]

content w =
  let tiles = tile w $ map toTile examples in
  container w (heightOf tiles) midTop tiles

info w =
  let wid = (w-60) `div` 3 in
  flow down
    [ width w intro
    , spacer 10 40
    , content w
    , spacer 10 40
    , flow right . List.intersperse (spacer 30 10) $ map (width wid) [ features, news, contact ]
    ]

main = lift (skeleton info) Window.width

title = constant (JavaScript.castStringToJSString "The Elm Programming Language")
foreign export jsevent "elm_title"
  title :: Signal JSString