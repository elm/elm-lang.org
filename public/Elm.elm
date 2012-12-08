import Website.Skeleton
import Website.Tiles
import Website.ColorScheme

intro = [markdown|

### The Elm Programming Language

**Elm is a [functional reactive programming][frp] language meant to replace HTML/CSS/JavaScript.**
Elm is optimized for creating [web][flow] [GUIs][canvas], [supporting complex user input][frp],
and [avoiding callbacks][escape].

  [flow]:    /edit/examples/Elements/FlowDown2.elm "Flow down example"
  [canvas]:  /edit/examples/Reactive/Transforms.elm "Canvas Example"
  [escape]:  /learn/Escape-from-Callback-Hell.elm
  [why-elm]: http://www.testblogpleaseignore.com/2012/06/21/why-elm/ "Why Elm?"
  [frp]:     /learn/What-is-FRP.elm "functional reactive programming"
  [http]:    /edit/examples/JavaScript/ZipCodes.elm "HTTP requests"

|]

features = [markdown|

#### Features

* [Functional Reactive Programming][frp]

* [Strong][strong] / [static][static] types with [type inference][infer]

* [Native Markdown support](/edit/examples/Elements/Markdown.elm)

* [Core Libraries](/Documentation.elm) and [Module System][modules]

  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [strong]: http://en.wikipedia.org/wiki/Strong_typing "strongly"
  [static]: http://en.wikipedia.org/wiki/Type_system#Static_typing "statically"
  [infer]:  http://en.wikipedia.org/wiki/Type_inference "type inference"
  [modules]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Module System"

|]

news = [markdown|

#### News

[Escape from Callback Hell][escape]: AJAX without callbacks

[Making Pong in Elm][pong]: a comprehensive walkthrough

[Elm 0.6 Released][v6]

Elm featured on [InfoQ][video] and [O&rsquo;Reilly Radar][radar]

  [escape]: /learn/Escape-from-Callback-Hell.elm "Escape from Callback Hell"
  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [v6]: /blog/announce/version-0.6.elm "Elm 0.6 Released"
  [video]: http://www.infoq.com/presentations/Elm "Making the Web Functional"
  [radar]: http://radar.oreilly.com/2012/12/emerging-languages-spotlight-elm.html "Emerging Languages Spotlight"

|]

contact = [markdown|

#### Contact / Contribute

Elm's [mailing list][7] is the place to go for questions, announcements, and discussion.
Or see if anyone is on IRC ([`#elm` on freenode][irc]).

There are also tons of ways to [contribute to Elm](/Contribute.elm).

  [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
  [irc]: http://webchat.freenode.net/?channels=elm "#elm"

|]

exampleText = [markdown|

#### Examples

Read, use, and edit real Elm programs. Think about how
you would implement the same things with HTML, CSS, and JavaScript.
Tons more examples can be found [here](/Examples.elm).

|]

infoqDesc = [markdown|

#### Making the Web Functional

An introduction to Elm. Why you should care. How it works. How to make cool stuff.

|]

downloadText = [markdown|

#### Get Started!

Download Elm and start using it now. Or just start using
Elm&rsquo;s [online compiler](/edit/examples/Reactive/Position.elm)
with no set-up at all.

|]

examples = map (\(x,y) -> (x, y, "Intermediate/"))
  [ ("Analog Clock", "Clock")
  , ("Slide Show", "SlideShow")
  , ("Stamps", "Stamps")
  , ("Diagrams", "Physics")
  , ("Quick Animations", "Slide")
  , ("Pascal's Triangle", "PascalsTriangle")
  ]

content w =
  let tiles = miniTiles 250 examples in
  flow down [ width w exampleText
            , container w (heightOf tiles) middle tiles ]

infoq w =
  let { lnk = "http://www.infoq.com/presentations/Elm"
      ; vid = layers [ image 320 240 "/infoq.jpg"
                   , Graphics.link lnk $ spacer 320 240 ]
      }
  in  width w infoqDesc `above` container w (heightOf vid) middle vid

download w =
  let lnk = "https://github.com/evancz/Elm/blob/master/README.md#elm" in
  flow down [ width w downloadText
            , container w 60 middle $
              layers [ color mediumGrey . container 200 60 middle .
                       color lightGrey  . container 198 58 middle .
                       text . Text.height 1.5 $ toText "Download"
                     , Graphics.link lnk $ spacer 200 60 ]
            ]

info w =
  let wid = (w-60) `div` 2
  in  flow down
       [ width w intro
       , spacer 10 30
       , flow right [ width wid features, spacer 60 10, width wid news ]
       , spacer 10 50
       , flow right [ content wid, spacer 60 10, infoq wid ]
       , spacer 10 50
       , flow right [ width wid contact, spacer 60 10, download wid ]
       ]

main = lift (skeleton info) Window.width

title = constant (JavaScript.castStringToJSString "The Elm Programming Language")
foreign export jsevent "elm_title"
  title :: Signal JSString