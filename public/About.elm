import Website.Skeleton (skeleton)
import Window

---- Text of the page: all written in Markdown ----

intro = [markdown|

## About Elm

The following articles provide an introduction to Elm and [FRP][frp].
Each one focuses on a specific question, and aims to increase your
general understanding. When read in sequence, they should slowly
ramp up in difficulty, taking you from beginner to expert.

 [frp]: /learn/What-is-FRP.elm "What is FRP?"
|]

leftCol = [markdown|

#### Overview of features

* [What is &ldquo;Functional Reactive Programming&rdquo;?][frp]
* [Getting started with Types][types]
* [Pattern matching and ADTs][adt]
* [Extensible records][records]
* [JavaScript Integration][js]
* [Create a purely functional game][pong]
* [Escape from Callback Hell][efch]
* [The semantics of FRP in Elm][thesis]

 [adt]: learn/Pattern-Matching.elm
 [efch]: /learn/Escape-from-Callback-Hell.elm "Escape from Callback Hell"
 [events]: /learn/FRP-vs-Events.elm "FRP vs Events"
 [frp]: /learn/What-is-FRP.elm "What is FRP?"
 [js]: /learn/JavaScript-Integration.elm
 [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
 [records]: /learn/Records.elm "Records in Elm"
 [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
 [types]: /learn/Getting-started-with-Types.elm "Getting started with Types"

|]

rightCol = [markdown|

#### Quick Start

* [Syntax Reference][syntax]
* [Learn by example][learn]

  [learn]: /Examples.elm "Elm by Example"
  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

#### Getting set up

* [Setting up the Elm compiler and server][install]
* [Creating a working website][this]
* [A skeleton for making games][games]

 [install]: https://github.com/evancz/Elm/blob/master/README.md#elm "install"
 [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
 [games]: https://github.com/evancz/Elm/blob/master/Examples/elm-js/GameSkeleton/GameSkeleton.elm#L1 "Game Skeleton"

|]

midtro = [markdown|
If you want a general overview of Elm, check out the conference videos.
If you are interested in particular features, the release notes can be
very helpful. They each explain the once-new features of Elm in depth,
covering nearly all parts of the language.
|]

releaseNotes = [markdown|

#### Release notes

* [`0.8.0`][8]  &nbsp; &nbsp; `05/2013` &nbsp; &nbsp; HTML/JS integration, type annotations/aliases
* [`0.7.1`][71] &nbsp; &nbsp; `02/2013` &nbsp; &nbsp; Touch, Either, and better Keyboard
* [`0.7.0`][7]  &nbsp; &nbsp; `01/2012` &nbsp; &nbsp; Extensible Records
* [`0.6.0`][6]  &nbsp; &nbsp; `12/2012` &nbsp; &nbsp; Time, Dates, and whitespace sensitivity
* [`0.5.0`][5]  &nbsp; &nbsp; `10/2012` &nbsp; &nbsp; Dictionaries, Sets, and Automata 
* [`0.4.0`][4]  &nbsp; &nbsp; `09/2012` &nbsp; &nbsp; Markdown and better graphics
* [`0.3.6`][36] &nbsp; &nbsp; `08/2012` &nbsp; &nbsp; JSON support
* [`0.3.5`][35] &nbsp; &nbsp; `06/2012` &nbsp; &nbsp; JavaScript FFI
* [`0.3.0`][3]  &nbsp; &nbsp; `06/2012` &nbsp; &nbsp; Modules
* `0.1.0`       &nbsp; &nbsp; `04/2012` &nbsp; &nbsp; Initial Release

  [3]:  http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Modules"
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Integration"
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/ "JSON"
  [4]:  /blog/announce/version-0.4.0.elm "Graphics Upgrade"
  [5]:  /blog/announce/version-0.5.0.elm "Libraries"
  [6]:  /blog/announce/version-0.6.elm "Time, Date, and Syntax"
  [7]:  /blog/announce/version-0.7.elm "Extensible Records & More"
  [71]: /blog/announce/version-0.7.1.elm "Touch, Keyboard, Either, etc."
  [8]:  /blog/announce/version-0.8.elm
|]

videos = [markdown|

#### Videos

* [What is Elm? Why do I care?][infoq]
* [What is FRP? How do I make games?][mlocjs]

 [infoq]: http://www.infoq.com/presentations/Elm "Elm at the Emerging Languages conference"
 [mlocjs]: http://www.ustream.tv/recorded/29330499 "Elm and the mloc.js conference"
|]

content w =
  let hwidth = if w < 800 then w `div` 2 - 20 else 380 in
  flow down
    [ width w intro
    , flow right [ width hwidth leftCol, spacer 40 10, width hwidth rightCol ]
    , width w midtro
    , width w releaseNotes
    , width w videos
    ]

main = lift (skeleton content) Window.width
