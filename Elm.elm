
import Website.Skeleton
import Website.Tiles

intro = [markdown|

### The Elm Programming Language

The Elm programming language aims to make web development
more pleasant. Elm is a type-safe, functional reactive language
that compiles to HTML, CSS, and JavaScript. You can start coding
in Elm without any install or setup with Elm's [interactive editor][1],
so start learning Elm by [example][2]:

  [1]: /edit/examples/Reactive/Transforms.elm "interactive editor"
  [2]: /Examples.elm "example"

|]

exampleData =
  [ ("Layout"       , "FlowDown2" , "Elements/")
  , ("Shapes"       , "Shapes"    , "Elements/")
  , ("Analog Clock" , "Clock"     , "Intermediate/")
  , ("Slide Show"   , "SlideShow" , "Intermediate/")
  , ("Form Validation", "Form"    , "Intermediate/")
  ]

examples w = let tiles = tile w $ map toTile exampleData in
             container w (heightOf tiles) middle tiles

rest = [markdown|

Elm's major distinguishing features are support for [reactive programming][1]
and its focus on graphical user interfaces. Elm is also [call-by-value][2] and
[strongly][3] / [statically][4] typed with [type-inference]5], so those of you
already familiar with languages like Haskell or ML should be quite comfortable.

The language features mentioned above help catch errors in your code at
compile time, but there are some errors that are external to your code,
such as browser incompatabilities. By using HTML, CSS, and JavaScript as
an assembly language, Elm can avoid some of these problems.

  [1]: http://en.wikipedia.org/wiki/Reactive_programming "reactive programming"
  [2]: http://en.wikipedia.org/wiki/Evaluation_strategy "call-by-value"
  [3]: http://en.wikipedia.org/wiki/Strong_typing "strongly"
  [4]: http://en.wikipedia.org/wiki/Type_system#Static_typing "statically"
  [5]: http://en.wikipedia.org/wiki/Type_inference "type inference"

<br/>

#### News

Elm 0.4: New features for game making and layout, Markdown support,
optimizations, and more. More information [here][6].

  [6]: /blog/announce/version-0.4.0.elm "announcement"

#### Contact

See the Elm [mailing list][7] for questions, announcements, and discussion. You can contact me directly at info (at) elm-lang (dot) org.

  [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"

|]

info w = flow down $ map (width w) [ intro, examples w, rest ]

main = lift (skeleton info) Window.width
