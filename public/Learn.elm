import Website.Skeleton (skeleton)
import Window

intro = [markdown|
<style>
h1 { margin-bottom: 0; }
ul { margin-top: 0; }
h2,h3,h4 { margin-bottom: 0.5em; margin-top: 2em; }
h5 { margin-bottom: 0.5em; }
</style>

# Learn

The [syntax reference](/learn/Syntax.elm) is a good place to start.
The [examples page](/Examples.elm) is intended to help you learn by
example, slowly building up skills. From there, this page provides
much more documentation. See [the roadmap](/learn/Roadmap.elm) for
advice on how to learn Elm based on your background.

|]

leftCol = [markdown|

#### Syntax

* [Syntax reference](/learn/Syntax.elm)

#### Features

* [FRP](/learn/What-is-FRP.elm)
* [Types](/learn/Getting-started-with-Types.elm)
* [Algebraic Data Types](/learn/Pattern-Matching.elm)
* [Records](/learn/Records.elm)
* [Components: Embed in HTML](/learn/Components.elm)
* [Ports: Talk to JS](/learn/Ports.elm)

|]

rightCol = [markdown|

#### Beginner Classes

* [Intro to Programming](/learn/courses/beginner/Programming.elm)
* [Intro to Graphics](/learn/courses/beginner/Graphics.elm)
* [Intro to Lists and Records](/learn/courses/beginner/Lists-and-Records.elm)

#### Making Stuff

* [Website skeleton](https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm)
* [Game skeleton](https://github.com/evancz/elm-lang.org/blob/master/public/examples/Intermediate/GameSkeleton.elm#L1)
* [Making Pong](/blog/games-in-elm/part-0/Making-Pong.html)

|]

papers = [markdown|

#### Conference Videos

* [StrangeLoop 2013][sl2013] &mdash; The best one so far! Teaches the basics of
  graphics and FRP in Elm and builds up to implementing a basic [Mario
  game](/edit/examples/Intermediate/Mario.elm) *and* it is pretty fun to watch.
* [mloc.js 2013][mlocjs] &mdash; a less polished version of the StrangeLoop 2013 talk.
* [StrangeLoop 2012][sl2012] &mdash; First conference talk ever. Covers the basics of
  graphics and FRP before Elm even had its record system!

 [sl2013]: http://www.infoq.com/presentations/elm-reactive-programming
 [mlocjs]: http://www.ustream.tv/recorded/29330499
 [sl2012]: http://www.infoq.com/presentations/Elm

#### Publications

* [Concurrent FRP for GUIs][thesis] &mdash; overview of Elm and history of FRP
* [Asynchronous FRP for GUIs][pldi] &mdash; formal semantics of Elm, PLDI 2013

 [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

#### Articles / Blog

 * [Elm package manager](/blog/announce/PackageManager.elm)
 * [Elm REPL](/blog/announce/Repl.elm)
 * [Hot-swapping in Elm](/blog/Interactive-Programming.elm)
 * [Concepts behind the Elm Logo](https://prezi.com/npjjrmt_badc/tangrams-logo/)
 * [Elm in VentureBeat](http://venturebeat.com/2013/07/26/why-i-designed-a-front-end-programming-language-from-scratch/)
 * [Elm &hearts; Prezi](/blog/announce/Elm-and-Prezi.elm)
 * [Escape from Callback Hell](/learn/Escape-from-Callback-Hell.elm)
 * [Making Pong](/blog/games-in-elm/part-0/Making-Pong.html)


|]

content w =
  let hwidth = if w < 800 then w `div` 2 - 20 else 380 in
  flow down
    [ width w intro
    , flow right [ width hwidth leftCol, spacer 40 10, width hwidth rightCol ]
    , width w papers
    ]

main = lift (skeleton content) Window.dimensions
