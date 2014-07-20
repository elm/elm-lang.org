import Website.Skeleton (skeleton)
import Window

main = skeleton "Learn" content <~ Window.dimensions

content outer =
  let w = 600
      hwidth = if w < 800 then w `div` 2 - 20 else 380
      body = flow down
             [ width w intro
             , flow right [ width hwidth leftCol, spacer 40 10, width hwidth rightCol ]
             , width w papers
             ]
  in  container outer (heightOf body) middle body

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
example, slowly building up skills. This page provides more in-depth
documentation.

|]

leftCol = [markdown|

#### General Resources

* [Syntax reference](/learn/Syntax.elm)
* [Learning roadmap](/learn/Roadmap.elm)
* [FAQ by language](/learn/FAQ.elm)

#### Topic Tutorials

* [FRP](/learn/What-is-FRP.elm)
* [Types](/learn/Getting-started-with-Types.elm)
* [Algebraic Data Types](/learn/Pattern-Matching.elm)
* [Records](/learn/Records.elm)
* [Interactive UI Elements](/learn/Interactive-UI-Elements.elm)
* [Embed in HTML](/learn/Components.elm)
* [Interop with JS](/learn/Ports.elm)

|]

rightCol = [markdown|

#### Beginner Classes

* [Intro to Programming](/learn/courses/beginner/Programming.elm)
* [Intro to Graphics](/learn/courses/beginner/Graphics.elm)
* [Intro to Lists and Records](/learn/courses/beginner/Lists-and-Records.elm)

#### Making Stuff

* [Website skeleton](https://github.com/elm-lang/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm)
* [Game skeleton](https://github.com/elm-lang/elm-lang.org/blob/master/public/examples/Intermediate/GameSkeleton.elm#L1)
* [Making Pong](/blog/Pong.elm)
* [Library Design Guidelines](http://library.elm-lang.org/DesignGuidelines.html)
* [Writing Docs](http://library.elm-lang.org/Documentation.html)

|]

papers = [markdown|

#### Publications

* [Concurrent FRP for GUIs][thesis] &mdash; very accessible overview of Elm and a history of FRP
* [Asynchronous FRP for GUIs][pldi] &mdash; formal semantics of Elm, from PLDI 2013

 [thesis]: /papers/concurrent-frp.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

|]


