import Website.Skeleton (skeleton)
import Window

---- Text of the page: all written in Markdown ----

intro = [markdown|
<style>
h1 { margin-bottom: 0; }
ul { margin-top: 0; }
h2,h3,h4 { margin-bottom: 0.5em; margin-top: 2em; }
h5 { margin-bottom: 0.5em; }
</style>

# Learn

This page focuses on written and video tutorials on Elm. There are
learning materials for every level of programmer:

 * *New to programming* &mdash; focus on the [beginner classes](#beginner-classes)

 * *New to functional programming* &mdash; check out the
   [mini tutorials](#mini-tutorials), [syntax reference](/learn/Syntax.elm),
   and [these examples](/Examples.elm#compute)

 * *New to FRP* &mdash; read [this intro](/learn/What-is-FRP.elm),
   see [these examples](/Examples.elm#react),
   watch [this video](http://www.ustream.tv/recorded/29330499),
   and read [this paper](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)

 * *Using Elm* &mdash; see the [intermediate and large examples](/Examples.elm)
   and resources on [making stuff](#making-stuff), [formal semantics](#publications),
   [release notes](/Install.elm#release-notes),
   and [blog posts](#articles-blog)

|]

leftCol = [markdown|

#### Mini Tutorials

* [FRP](/learn/What-is-FRP.elm)
* [Syntax](/learn/Syntax.elm)
* [Types](/learn/Getting-started-with-Types.elm)
* [Algebraic Data Types](/learn/Pattern-Matching.elm)
* [Records](/learn/Records.elm)
* [Documenting Code](/learn/Documentation.elm)
* [HTML/JS Integration](https://github.com/evancz/elm-html-and-js#htmljs-integration--live-demo)

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

* [Making the Web Functional][infoq] &ndash; Elm and purely functional graphics
* [Functional Reactive Programming][mlocjs] &ndash; intro to FRP, live-coding Mario

 [infoq]: http://www.infoq.com/presentations/Elm "Elm at the Emerging Languages conference"
 [mlocjs]: http://www.ustream.tv/recorded/29330499 "Elm and the mloc.js conference"

#### Publications

* [Concurrent FRP for GUIs][thesis] &ndash; overview of Elm and history of FRP
* [Asynchronous FRP for GUIs][pldi] &ndash; formal semantics of Elm, PLDI 2013

 [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

#### Articles / Blog

 * [Hot-swapping in Elm](/blog/Interactive-Programming.elm)
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
