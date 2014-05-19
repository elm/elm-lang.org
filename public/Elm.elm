import Website.Skeleton (homeSkeleton, bigLogo, installButtons)
import Website.Tiles as Tile

import Text
import Window

moreInfo = [markdown|

We carefully choose the **practical** and **useful** ideas from the functional
programming world and work hard to make them **accessible**. Elm has features
like [functional reactive programming][frp], immutability, and type inference
because we think these are ideas that prove their usefulness very quickly in
practice.

These features not only made it possible to create Elm&rsquo;s [Time Traveling
Debugger](http://debug.elm-lang.org/), but we think they will make your code
simpler and safer too.

It is easy to make claims like this though, so **check out some
[examples](http://elm-lang.org/Examples.elm) and decide for yourself!**
And definitely take advantage of the [accessible learning resources](/Learn.elm),
[great library documentation][docs], and [friendly community][list] when you
need help.

  [frp]:  /learn/What-is-FRP.elm
  [docs]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/
  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

|]

other w = width (w `div` 2) [markdown|

### Community

* [mailing list][list]
* [`#elm` on IRC](http://webchat.freenode.net/?channels=elm)
* [/r/elm](http://www.reddit.com/r/elm)
* [Elm user group SF](http://www.meetup.com/Elm-user-group-SF/)
* [Share Code](http://www.share-elm.com)

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
|] `beside` width (w `div` 2) [markdown|

### News

* [Time Traveling Debugger](http://debug.elm-lang.org)
* [Easy user input](/blog/announce/0.12.elm) with 0.12
* [Hot-swapping](/blog/Interactive-Programming.elm) in Elm
* [Elm and Prezi](http://elm-lang.org/blog/announce/Elm-and-Prezi.elm),
  [Prezi and Elm](http://engineering.prezi.com/blog/2013/05/21/elm-at-prezi/)

|]

exampleBlock w =
    Tile.examples w
    [ map Tile.intermediate [ "Mario", "TextReverse", "Calculator", "Pong" ]
    , map Tile.intermediate [ "Clock", "PieChart", "Plot", "Walk" ] -- ++ [ Tile.webgl "Thwomp" ]
    ]

language = [markdown|

Elm is a functional language that compiles to HTML, CSS, and JavaScript.
It is great for creating [websites](http://library.elm-lang.org/),
[interactive widgets](https://github.com/evancz/TodoFRP),
[diagrams](https://github.com/seliopou/elm-d3), or [2D](/blog/Pong.elm) or
[3D](https://github.com/johnpmayer/elm-webgl) games.

Elm is also [super simple to embed](https://github.com/evancz/elm-html-and-js)
as a component in an existing project, and [interop with JS](/learn/Ports.elm)
is easy. Just pick a small widget and see if Elm can simplify it!

  [frp]: /learn/What-is-FRP.elm "functional reactive programming"

|]

info w = let content = flow down [ spacer w 20
                                 , link "/" bigLogo
                                 , width w language
                                 , spacer w 4
                                 , exampleBlock w
                                 , spacer w 24
                                 , installButtons w
                                 , width w moreInfo
                                 , spacer w 8
                                 , other w ]
         in  container w (heightOf content) middle content 

main = homeSkeleton info <~ Window.dimensions

port title : String
port title = "Elm - functional web programming"
