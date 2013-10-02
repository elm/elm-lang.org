import Website.Skeleton (skeleton)
import Window

---- Text of the page: all written in Markdown ----

intro = [markdown|

# Learn

|]

leftCol = [markdown|

#### Language features

* [FRP][frp]
* [Types][types]
* [Algebraic data types][adt]
* [Records][records]
* [JavaScript integration][js]

 [adt]: learn/Pattern-Matching.elm
 [events]: /learn/FRP-vs-Events.elm "FRP vs Events"
 [frp]: /learn/What-is-FRP.elm "What is FRP?"
 [js]: /learn/JavaScript-Integration.elm
 [records]: /learn/Records.elm "Records in Elm"
 [types]: /learn/Getting-started-with-Types.elm "Getting started with Types"

|]

rightCol = [markdown|

#### Get Started

* [Syntax reference](/learn/Syntax.elm)
* [Website skeleton][this]
* [Game skeleton][games] / [walkthrough](/blog/games-in-elm/part-0/Making-Pong.html)

 [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
 [games]: https://github.com/evancz/elm-lang.org/blob/master/public/examples/Intermediate/GameSkeleton.elm#L1 "Game Skeleton"

#### Beginner Classes

* [Intro to Programming](/learn/courses/beginner/Programming.elm)
* [Intro to Graphics](/learn/courses/beginner/Graphics.elm)
* [Intro to Lists and Records](/learn/courses/beginner/Lists-and-Records.elm)

|]

papers = [markdown|

#### Publications

* [Concurrent FRP for GUIs][thesis] - overview of Elm and history of FRP
* [Asynchronous FRP for GUIs][pldi] - semantics of Elm, PLDI 2013

 [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

#### Conference Videos

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

main = lift (skeleton content) Window.dimensions
