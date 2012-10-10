import Website.Skeleton
import Website.Tiles

intro = [markdown|

### The Elm Programming Language

The Elm programming language aims to make web development more pleasant.
There are tons of [interactive examples][examples], so it easy to learn
and use Elm for beginners and experts alike.

  [examples]: /Examples.elm "example"

Elm compiles to HTML, CSS and JavaScript. So you can use the [web platform][web]
with all the benefits of a modern programming language.
Elm's features include:

- [Strong][strong] / [static][static] typing with [type inference][infer]
- [Functional Reactive Programming][frp] (FRP)
- [Markdown support][md] ([example](/edit/examples/Elements/Markdown.elm))
- [Module system][module]
- [Growing set of core libraries](/Documentation.elm)
- [Online interactive editor][ide]
- [Purely functional][pure] graphics
- [Pattern matching][match]
- [Call-by-Value evaluation][cbv]


  [strong]: http://en.wikipedia.org/wiki/Strong_typing "strongly"
  [static]: http://en.wikipedia.org/wiki/Type_system#Static_typing "statically"
  [infer]:  http://en.wikipedia.org/wiki/Type_inference "type inference"
  [frp]:    http://en.wikipedia.org/wiki/Functional_reactive_programming "functional reactive programming"
  [module]: http://en.wikipedia.org/wiki/Modular_programming "modules"
  [md]:     http://daringfireball.net/projects/markdown/basics "markdown"
  [ide]:    /edit/examples/Reactive/Transforms.elm "interactive editor"
  [pure]:   http://en.wikipedia.org/wiki/Purely_functional "purely functional"
  [match]:  http://en.wikipedia.org/wiki/Pattern_matching "pattern matching"
  [cbv]:    http://en.wikipedia.org/wiki/Evaluation_strategy "call-by-value"


  [web]: http://www.webplatform.org/ "web platform"

For a quick introduction to Elm, look at the [basic examples](/examples/Basic.elm).

<br/>

#### News

Elm 0.4: New features for game making and layout, Markdown support,
optimizations, and more. More info [here][6].

  [6]: /blog/announce/version-0.4.0.elm "announcement"

#### Contact

See the Elm [mailing list][7] for questions, announcements, and discussion. You can contact me directly at info (at) elm-lang (dot) org.

  [7]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "mailing list"

|]

info w = flow down $ map (width w) [ intro ]

main = lift (skeleton info) Window.width
