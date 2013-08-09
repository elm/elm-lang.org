import Website.Skeleton (skeleton)
import Website.Tiles (miniTiles)
import Website.ColorScheme

import Text
import JavaScript as JS
import Window

intro = [markdown|

### The Elm Programming Language

Elm is a [functional reactive programming][frp] (FRP) language
that compiles to HTML, CSS, and JS. [FRP][frp] is a
concise and elegant way to [create][e1] [highly][e2]
[interactive][e3] [applications][e4] and [avoid callbacks][escape].

  [games]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [escape]:  /learn/Escape-from-Callback-Hell.elm
  [frp]:     /learn/What-is-FRP.elm "functional reactive programming"
  [e1]: /edit/examples/Intermediate/Clock.elm
  [e2]: /edit/examples/Intermediate/Mario.elm
  [e3]: /edit/examples/Intermediate/Pong.elm
  [e4]: /edit/examples/Intermediate/Flickr.elm

|]

exampleText = [markdown|

#### Learn by Example

The best way to learn Elm is [by example](/Examples.elm).
Start with the [basics](/examples/Basic.elm). Work up to
[intermediate examples](/examples/Intermediate.elm) and
[games](/blog/games-in-elm/part-0/Making-Pong.html).

The [About](/About.elm) and [Docs](/Documentation.elm) tabs
give even more info about Elm.

Together these resources illustrate the key features of Elm: [Functional Reactive Programming][frp], 
[strong][strong] [static][static] types with full [type inference][infer],
[Markdown support](/edit/examples/Elements/Markdown.elm), the
[core libraries](/Documentation.elm), and the [module system][modules].


  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [strong]: http://en.wikipedia.org/wiki/Strong_typing "strong typing"
  [static]: http://en.wikipedia.org/wiki/Type_system#Static_typing "static typing"
  [infer]:  http://en.wikipedia.org/wiki/Type_inference "type inference"
  [modules]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Module System"

|]

resources = [markdown|

#### Videos and Resources

There are tons of resources available in the [About](/About.elm) and
[Docs](/Documentation.elm) tabs. The following videos provide a guided
tour through Elm:

* [Making the Web Functional][vid] motivates and introduces the core
  parts of Elm.
* [Introduction to Functional Reactive Programming][frp] explains FRP from
  scratch, building up to a simple Mario game and asynchronous HTTP requests.

  [vid]: http://www.infoq.com/presentations/Elm "Elm at Emerging Languages Camp"
  [exs]: https://github.com/evancz/elm-at-strangeloop#elm-at-strangeloop "Non-grainy Examples"
  [frp]: http://www.ustream.tv/recorded/29330499 "Functional Reactive Programming"

|]

tryOnline = [markdown|

#### Use the online editor

The [online editor](/try) lets you write and compile Elm without downloading
anything. It supports automatic compilation, editor customization, and inline
documentation.
|]

contact = [markdown|

#### Community

[Elm's mailing list][7] is the place to go for questions, announcements,
and discussion. Or check out the IRC channel at [`#elm` on freenode][irc].

 [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
 [irc]: http://webchat.freenode.net/?channels=elm "#elm"

|]

news = [markdown|

#### News

* Upcoming talk on Elm at [StrangeLoop 2013][loop] in St. Louis
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

examples = map (\(x,y) -> (x, y, "Intermediate/"))
  [ ("Analog Clock", "Clock")
  , ("Stamps", "Stamps")
  , ("Diagrams", "Physics")
  , ("Slide Show", "SlideShow")
  , ("Walking", "Walk")
  , ("Mario", "Mario")
  ]

content w =
  let tiles = miniTiles w examples
  in  flow down [ spacer 10 20
                , container w (heightOf tiles) middle tiles
                ]

infoq w =
  let lnk = "http://www.infoq.com/presentations/Elm"
      vid = fittedImage w 200 "/infoq.jpg"
  in  spacer 10 20 `above` layers [ vid, link lnk (spacer w 200 )]

tryElm w =
  flow down [ spacer 10 20
            , link "/try" (fittedImage w (w `div` 2) "editor.jpg") ]

info w =
  let sw = if w >= 800 then 300 else 200
      box txt pics = flow right [ width (w-(30+sw)) txt, spacer 30 10, pics sw ]
  in  flow down <| [ width w intro, spacer w 10 ] ++
      intersperse (spacer w 40)
       [ box exampleText content,
         box resources infoq,
         box tryOnline tryElm,
         box news (flip width contact) ]

main = lift (skeleton info) Window.width

title = constant (JS.fromString "The Elm Programming Language")
foreign export jsevent "title"
  title : Signal JS.JSString
