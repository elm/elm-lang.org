import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Window

main = skeleton "Learn" content <~ Window.dimensions

content outer =
  let w = 600
      hwidth = if w < 800 then w // 2 - 20 else 380
      body =
          width w info
  in
      container outer (heightOf body) middle body

-- * [Learning roadmap](/learn/Roadmap.elm) &mdash; &ldquo;based on my background, what should I learn first?&rdquo;

info = Markdown.toElement """

# Learn

The [syntax reference](/learn/Syntax.elm) is a good place to start.
The [examples page](/Examples.elm) is intended to help you learn by
example, slowly building up skills. This page provides more in-depth
documentation.

### General Resources

* [Syntax reference](/learn/Syntax.elm) &mdash; all of the syntax constructs in Elm
* [FAQ](/learn/FAQ.elm) &mdash; frequently asked questions, organized by who tends to ask them

### Building Blocks

* [Understanding Types](/learn/Understanding-Types.elm) &mdash; primitive values, data structures, and functions
* [Union Types](/learn/Union-Types.elm) &mdash; mixing different kinds of values
* [Records](/learn/Records.elm) &mdash; values with named fields, sort of like objects

### Signals

* [Key Concepts](/learn/What-is-FRP.elm) &mdash; key concepts behind signals
* [Using Signals](/learn/Using-Signals.elm) &mdash; common patterns and pitfalls
* [Inputs](/learn/Interactive-UI-Elements.elm) &mdash; working with text fields, check boxes, etc.
* [Architecting large programs](/learn/Architecture.elm) &mdash; how to use signals as programs get larger

### Interop

* [HTML in Elm][elm-html] &mdash; making HTML faster and simpler
* [Elm in HTML](/learn/Components.elm) &mdash; put any Elm program in any div
* [Ports](/learn/Ports.elm) &mdash; communicate with JavaScript


### Making Stuff

* [Website skeleton](https://github.com/evancz/elm-todomvc/blob/master/Todo.elm) &mdash; template for making websites with [elm-html][]
* [Game skeleton](https://github.com/elm-lang/elm-lang.org/blob/master/frontend/public/examples/Intermediate/GameSkeleton.elm) &mdash; template for making games, though many things fit this pattern
* [Making Pong](/blog/Pong.elm) &mdash; a full walkthrough of how to create Pong
* [Library Design Guidelines](http://library.elm-lang.org/help/design-guidelines) &mdash; creating pleasant and consistent libraries in Elm
* [Writing Docs](http://library.elm-lang.org/help/documentation-format) &mdash; an important thing to do!

[elm-html]: /blog/Blazing-Fast-Html.elm

### Beginner Classes

* [Intro to Programming](/learn/courses/beginner/Programming.elm) &mdash; assumes no knowledge of programming
* [Intro to Graphics](/learn/courses/beginner/Graphics.elm) &mdash; builds upon &ldquo;Intro to Programming&rdquo; to draw things
* [Intro to Lists and Records](/learn/courses/beginner/Lists-and-Records.elm) &mdash; getting deeper into programming concepts in Elm


### Publications

* [Concurrent FRP for GUIs][thesis] &mdash; very accessible history of FRP and overview of Elm 
* [Asynchronous FRP for GUIs][pldi] &mdash; formal semantics of Elm from PLDI 2013

 [thesis]: /papers/concurrent-frp.pdf "thesis"
 [pldi]: http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html "PLDI 2013 paper"

"""


