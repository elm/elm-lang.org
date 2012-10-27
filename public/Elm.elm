import Website.Skeleton
import Website.ColorScheme

intro = [markdown|

### The Elm Programming Language

Elm aims to make front-end web development more pleasant.
It introduces a new approach to GUI programming that corrects the
systemic problems that make HTML, CSS, and JavaScript a headache to use.
Strongly influenced by [Haskell][why-elm] and [Functional Reactive Programming][frp],
Elm allows you to quickly and easily work with visual layout, create scenes
with HTML5&rsquo;s canvas, make [asynchronous requests][http], and handle complicated user input.

  [why-elm]: http://www.testblogpleaseignore.com/2012/06/21/why-elm/ "Why Elm?"
  [frp]:     /learn/What-is-FRP.elm "functional reactive programming"
  [http]:    /edit/examples/JavaScript/ZipCodes.elm "HTTP requests"

|]

features = [markdown|

#### Features

- [Functional Reactive Programming][frp]
- [Strong][strong] / [static][static] typing with [type inference][infer]
- [Native Markdown support](/edit/examples/Elements/Markdown.elm)
- [Growing set of core libraries](/Documentation.elm)

  [frp]:    /learn/What-is-FRP.elm "functional reactive programming"
  [strong]: http://en.wikipedia.org/wiki/Strong_typing "strongly"
  [static]: http://en.wikipedia.org/wiki/Type_system#Static_typing "statically"
  [infer]:  http://en.wikipedia.org/wiki/Type_inference "type inference"

|]

examples = [markdown|

#### Examples

- [Hello, world!](/edit/examples/Elements/HelloWorld.elm)
- [Images](/edit/examples/Elements/Image.elm)
- [Mouse position](/edit/examples/Reactive/Position.elm)
- [The Basics](/examples/Basic.elm)
- [This website](/edit/Elm.elm)
- [Much, much more!](/Examples.elm)

|]


news = [markdown|

#### News

[Making Pong in Elm][pong]: a comprehensive walkthrough of how to make purely functional games.

[Elm 0.5][announcement]: Lots of new and useful libraries!

  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [announcement]: /blog/announce/version-0.5.0.elm "announcement"

|]

contact = [markdown|

#### Contact / Contribute

Elm's [mailing list][7] is the place to go for questions, announcements, and discussion.
Or see if anyone is on IRC ([`#elm` on freenode][irc]). There are also tons of ways to
[contribute to Elm](/Contribute.elm).

  [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"
  [irc]: http://webchat.freenode.net/?channels=elm "#elm"

|]

info w =
  let { more = if w < 800 then w `div` 2 - 20 else 380
      ; less = if w < 800 then w `div` 2 - 20 else 380
      ; twoCol l r = flow right [ width more l, spacer 40 10, width less r ]
      }
  in  flow down
       [ width w intro
       , twoCol features examples
       , spacer 10 20
       , twoCol news contact
       ]

main = lift (skeleton info) Window.width
