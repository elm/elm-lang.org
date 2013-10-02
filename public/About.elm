import Website.Skeleton (skeleton')
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

papers = [markdown|

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
    , width w papers
    ]

main = lift (skeleton' 800 content) Window.dimensions
