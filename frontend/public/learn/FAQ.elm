import Graphics.Element exposing (..)
import Markdown
import Website.Skeleton exposing (skeleton)
import Window


main : Signal Element
main =
  Signal.map (skeleton "Learn" (\w -> width (min 600 w) content)) Window.dimensions


content : Element
content = Markdown.toElement """

# Tips by language

These are answers to frequently asked questions, broken down by the
background of the questioner. It currently covers [Haskell](#haskell)
and [JavaScript](#javascript)

<h2 id="haskell">Haskell</h2>

* [Although Elm looks like Haskell, Elm is not
  Haskell](https://groups.google.com/d/msg/elm-discuss/rI_IAf4TiAA/KTvQv1LQ6uAJ).
  In the pursuit of making purely functional GUIs easy and practical, Elm draws
  inspiration from many languages (e.g. Haskell, OCaml, Agda, F#, SML, Python,
  Clojure) and aims to borrow and improve upon the best of existing languages
  and research.

* Instead of using `($)` for function application, Elm uses the `(<|)` and `(|>)`
  operators for forward and backward application: `(f <| x) == f x == (x |> f)`.
  Borrowed from F#, the goal is to make the symbol reflect its meaning.

* Instead of using `(.)` for function composition, Elm uses the `(<<)` and
  `(>>)`. Borrowed from F#, the goal is to make the symbol reflect its meaning.

* The order of arguments for
  [foldl](http://package.elm-lang.org/packages/elm-lang/core/latest/List#foldl)
  is deliberately different from Haskell's [to improve
  composability](http://library.elm-lang.org/DesignGuidelines.html#the-data-structure-is-always-the-last-argument).

* The meanings of `(:)` and `(::)` are swapped. This matches ML, Standard ML,
  OCaml, F#, Agda, Rust, etc. and is based on the fact that type annotations
  are much more common than consing.

* [Graphics.Collage](http://package.elm-lang.org/packages/elm-lang/core/latest/Graphics-Collage)
  uses Cartesian coordinates, so positive y is up and the origin at the center of the canvas.

* [Elm is not lazy](http://www.testblogpleaseignore.com/2012/06/22/the-trouble-with-frp-and-laziness/).

* Signals are not monads in Elm. There is no `join : Signal (Signal a) -> Signal a`.
  [This conference talk](https://www.youtube.com/watch?v=Agu6jipKfYw) explains
  it best. You can read more about these concerns in [this
  thesis](/papers/concurrent-frp.pdf). You can also use [the automaton
  library](http://package.elm-lang.org/packages/evancz/automaton/latest/) which
  provides much of the flexibility of higher-order FRP without the performance
  issues.


<h2 id="javascript">JavaScript</h2>

* [Graphics.Collage](http://package.elm-lang.org/packages/elm-lang/core/latest/Graphics-Collage)
  uses Cartesian coordinates, so positive y is up and the origin at the center of the canvas

* The generated HTML is not intended for external styling. Learn about
  [Elm Graphics](/learn/courses/beginner/Graphics.elm) and try
  [these examples](/Examples.elm#Display)

* The Elm compiler enforces type-correctness and does not do automatic casting.

* Lists use square brackets like arrays, but all items must be of the same type.
  They are implemented as singly-linked linked lists.

* Tuples such as `(3, True)` may mix types but must be of a known, fixed length.

* Anonymous functions are written `(\\a b c -> expression)`

* Function application happens before any infix operation.

"""
