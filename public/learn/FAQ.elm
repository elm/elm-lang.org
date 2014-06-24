import Website.Skeleton (skeleton)
import Window

main = skeleton "Learn" content <~ Window.dimensions

content w = width (min 600 w) [markdown|
<style>
h1 { margin-bottom: 0; }
ul { margin-top: 0; }
h2,h3,h4 { margin-bottom: 0.5em; margin-top: 2em; }
h5 { margin-bottom: 0.5em; }
</style>

# Tips by language

These are answers to frequently asked questions, broken down by the
background of the questioner. It currently covers [Haskell](#haskell)
and [JavaScript](#javascript)

## Haskell

* [Although Elm looks like Haskell, Elm is not
  Haskell](http://www.testblogpleaseignore.com/2012/06/21/why-elm/). In the pursuit
  of making purely functional GUIs easy and practical, Elm draws inspiration from
  many languages (e.g. Haskell, OCaml, Agda, F#, SML) and aims to borrow and improve
  upon the best of existing languages and research.

* Instead of using `($)` for function application, Elm uses the `(<|)` and `(|>)`
  operators for forward and backward application: `(f <| x) == f x == (x |> f)`.
  Borrowed from F#, the goal is to make the symbol reflect its meaning.

* The order of arguments for
  [foldl](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/List#foldl)
  is deliberately different from Haskell's [to improve
  composability](http://library.elm-lang.org/DesignGuidelines.html#the-data-structure-is-always-the-last-argument).

* The meanings of `(:)` and `(::)` are swapped. This matches ML, Standard ML,
  OCaml, F#, Agda, Rust, etc. and is based on the fact that type annotations
  are much more common than consing.

* [Graphics.Collage](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Collage)
  uses Cartesian coordinates, so positive y is up and the origin at the center of the canvas.

* [Elm is not lazy](http://www.testblogpleaseignore.com/2012/06/22/the-trouble-with-frp-and-laziness/).

* Signals are not monads. There is no function `join : Signal (Signal a) -> Signal a`.
  Pure monadic FRP has theoretical problems regarding performance, requiring
  memory usage to grow linearly with time to maintain referential transparency.
  You can read more about these concerns in [my
  thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)
  or the closely related [PLDI
  paper](http://people.seas.harvard.edu/~chong/abstracts/CzaplickiC13.html).
  [Arrowized FRP](http://library.elm-lang.org/catalog/evancz-automaton/0.1/)
  provides much of the flexibility of Monadic FRP without the performance issues.


## JavaScript

* [Graphics.Collage](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Collage)
  uses Cartesian coordinates, so positive y is up and the origin at the center of the canvas

* The generated HTML is not intended for external styling. Learn about
  [Elm Graphics](/learn/courses/beginner/Graphics.elm) and try
  [these examples](/Examples.elm#Display)

* The Elm compiler enforces type-correctness and does not do automatic casting.

* Lists use square brackets like arrays, but all items must be of the same type.
  They are implemented as singly-linked linked lists.

* Tuples such as `(3, True)` may mix types but must be of a known, fixed length.

* Anonymous functions are written `(\a b c -> expression)`

* Function application happens before any infix operation.

|]
