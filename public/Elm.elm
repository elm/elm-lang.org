import Website.Skeleton (homeSkeleton, bigLogo, installButtons)
import Website.Tiles (examples)

import Text
import Window

moreInfo = [markdown|

Elm brings the benefits of purity, immutability, and an expressive static type
system to GUI programming. Elm is fully type inferred, so no type annotations
are needed to get the benefits of static checks.

See the [examples](/Examples.elm) and [learning resources](/Learn.elm) to learn
more. Join the
[elm-discuss list](https://groups.google.com/forum/?fromgroups#!forum/elm-discuss)
for questions, announcements, and discussion.

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
* [Improved JS interop](/blog/announce/0.11.elm) in 0.11
* [Hot-swapping](/blog/Interactive-Programming.elm) in Elm
* [Elm and Prezi](http://elm-lang.org/blog/announce/Elm-and-Prezi.elm),
  [Prezi and Elm](http://engineering.prezi.com/blog/2013/05/21/elm-at-prezi/)

|]

exampleBlock w =
    examples w [ [ "Mario", "TextReverse", "Calculator", "Pong" ]
               , [ "Clock", "PieChart", "Plot", "Walk" ]
               ]

language = [markdown|

Elm is a functional language that compiles to HTML, CSS, and JavaScript. 
Designed for [functional reactive programming][frp], Elm makes it easy to create
[highly][mario] [interactive][pong] [applications][flickr].

  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [mario]:  /edit/examples/Intermediate/Mario.elm "mario"
  [pong]:   /edit/examples/Intermediate/Pong.elm "pong"
  [flickr]: /edit/examples/Intermediate/Flickr.elm "flickr"

|]

info w = let content = flow down [ spacer w 20
                                 , link "/" bigLogo
                                 , width w language
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
