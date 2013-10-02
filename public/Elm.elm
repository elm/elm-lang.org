import Website.Skeleton (skeleton)
import open Website.ColorScheme

import Text
import JavaScript as JS
import Window
import Graphics.Input as Input

contentWidth = 526

intro = [markdown|

Elm also brings the benefits of purity and an expressive static type system
to GUI programming. Elm is fully type inferred, so no type annotations are
needed to get the benefits of static checks.

Join the [elm-discuss list][list] for questions, announcements, and discussion!

  [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"

|]

other w = width (w `div` 2) [markdown|

#### Community

* [elm-discuss list][list]
* [`#elm` on IRC](http://webchat.freenode.net/?channels=elm irc)
* [/r/elm](http://www.reddit.com/r/elm subreddit)
* [Elm user group SF](http://www.meetup.com/Elm-user-group-SF/ meetup in SF)

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
|] `beside` width (w `div` 2) [markdown|

#### News

* [Hot-swapping](/blog/Interactive-Programming.elm) in Elm
* Elm at [StrangeLoop 2013][loop]
* Type-checker fixed in [Elm 0.9][v9]
* Elm in [VentureBeat][vb]
* [Elm and Prezi](http://elm-lang.org/blog/announce/Elm-and-Prezi.elm),
  [Prezi and Elm](http://engineering.prezi.com/blog/2013/05/21/elm-at-prezi/)
* Extensible Records in [Elm 0.7][v7]

 [vb]: http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/
 [v7]: /blog/announce/version-0.7.elm "Extensible Records"
 [v9]: /blog/announce/version-0.9.elm
 [loop]: https://thestrangeloop.com/sessions/functional-reactive-programming-in-elm "Elm at StrangeLoop"

|]

exampleBlock =
    let row = flow right . intersperse (spacer 10 124) . map example
    in  container contentWidth 258 middle <| flow down [ row examples1, spacer 10 10, row examples2 ]

examples1 = [ "Mario", "Walk", "Pong", "SlideShow" ]
examples2 = [ "Clock", "Physics", "Slide", "Stamps" ]

navigation = Input.customButtons ()

example name =
    let btn clr = color clr . container 124 124 middle <|
                  image 120 120 ("/screenshot/" ++ name ++ ".jpg")
    in  link ("/edit/examples/Intermediate/" ++ name ++ ".elm") <|
        navigation.customButton () (btn mediumGrey) (btn accent1) (btn accent3)

language = [markdown|

# Elm <span style="font-size: 0.6em">functional web programming</span>

Elm is a functional programming language that compiles to HTML, CSS, and JavaScript. 
It is built around the concept of [functional reactive programming][frp],
making it easy to create [highly][mario] [interactive][pong] [applications][flickr].

  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [mario]:  /edit/examples/Intermediate/Mario.elm "mario"
  [pong]:   /edit/examples/Intermediate/Pong.elm "pong"
  [flickr]: /edit/examples/Intermediate/Flickr.elm "flickr"

|]

info w = let content = flow down [ width contentWidth language, exampleBlock
                                 , spacer contentWidth 10
                                 , width contentWidth intro, other contentWidth ]
         in  container w (heightOf content) middle content 

main = skeleton info <~ Window.dimensions

title = constant (JS.fromString "Elm - functional web programming")
foreign export jsevent "title"
  title : Signal JS.JSString
