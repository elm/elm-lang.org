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

 [install]: https://github.com/evancz/Elm/blob/master/README.md#install "install"
 [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
 [games]: https://github.com/evancz/elm-lang.org/blob/master/public/examples/Intermediate/GameSkeleton.elm#L1 "Game Skeleton"

|]

midtro = [markdown|
If you want a general overview of Elm, check out the conference videos.
If you are interested in particular features, the release notes can be
very helpful. They each explain the once-new features of Elm in depth,
covering nearly all parts of the language.
|]

releaseNotes = [markdown|

#### Release notes

* <code>[0.9][9]    &nbsp; &nbsp;&nbsp; 8/2013 &nbsp; &nbsp; </code>Fix the type checker. Fast and reliable static checks.
* <code>[0.8][8]    &nbsp; &nbsp;&nbsp; 5/2013 &nbsp; &nbsp; </code>HTML/JS integration, type annotations/aliases
* <code>[0.7.1][71]        &nbsp;&nbsp; 2/2013 &nbsp; &nbsp; </code>Touch, Either, and better Keyboard
* <code>[0.7][7]    &nbsp; &nbsp;&nbsp; 1/2013 &nbsp; &nbsp; </code>Extensible Records
* <code>[0.6][6]    &nbsp; &nbsp;&nbsp;12/2012 &nbsp; &nbsp; </code>Time, Dates, and whitespace sensitivity
* <code>[0.5][5]    &nbsp; &nbsp;&nbsp;10/2012 &nbsp; &nbsp; </code>Dictionaries, Sets, and Automata 
* <code>[0.4][4]    &nbsp; &nbsp;&nbsp; 9/2012 &nbsp; &nbsp; </code>Markdown and better graphics
* <code>[0.3.6][36]        &nbsp;&nbsp; 8/2012 &nbsp; &nbsp; </code>JSON support
* <code>[0.3.5][35]        &nbsp;&nbsp; 6/2012 &nbsp; &nbsp; </code>JavaScript FFI
* <code>[0.3][3]    &nbsp; &nbsp;&nbsp; 6/2012 &nbsp; &nbsp; </code>Modules
* <code> 0.1        &nbsp; &nbsp;&nbsp; 4/2012 &nbsp; &nbsp; </code>Initial Release

  [3]:  http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Modules"
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Integration"
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/ "JSON"
  [4]:  /blog/announce/version-0.4.0.elm "Graphics Upgrade"
  [5]:  /blog/announce/version-0.5.0.elm "Libraries"
  [6]:  /blog/announce/version-0.6.elm "Time, Date, and Syntax"
  [7]:  /blog/announce/version-0.7.elm "Extensible Records & More"
  [71]: /blog/announce/version-0.7.1.elm "Touch, Keyboard, Either, etc."
  [8]:  /blog/announce/version-0.8.elm
  [9]:  /blog/announce/version-0.9.elm

#### Publications

* [Concurrent FRP for GUIs][thesis] - reasonably accessible overview of Elm and history of FRP
* [Asynchronous FRP for GUIs][pldi] - revised and condensed overview of Elm from PLDI 2013

 [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

#### Conference Talks

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
    ]

main = lift (skeleton content) Window.width
