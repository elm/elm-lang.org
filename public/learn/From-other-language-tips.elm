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

# Coming to Elm from another programming language?

Here is a couple of points to be aware of when starting to learn about Elm.

|]

fromHaskell = [markdown|

#### From Haskell

* Elm is not Haskell. Although the Elm compiler is written in Haskell, and  Haskell, along with other functional langages, is a source of insipiration for the Elm language and syntax.
* Elm's `(<|)` operator is equivalent to Haskell's `($)`. There is also `(|>)` which has the arguments reversed. The arrow points towards the function.
* The order of foldl's arguments is deliberately different from Haskell's to improve composability in Elm.
* The meanings of `(:)` and `(::)` are swapped. `(:)` is “has type” and `(::)` is cons. This is the same as ML.
* Graphics.Collage uses Cartesian coordinates with the origin at the center of the canvas, instead of having (0,0) at top-left corner. Positive y is up.
* Elm is not lazy.
* Signals are not monads. There is no function `join : Signal (Signal a) -> Signal a`. This is done for performance reasons.

|]

fromJS = [markdown|

#### From JS, HTML and CSS

* Graphics.Collage uses Cartesian coordinates with the origin at the center of the canvas, instead of having (0,0) at top-left corner. Positive y is up.
* The generated HTML is not intended for external styling. Learn about [Elm Graphics](/learn/courses/beginner/Graphics.elm) and try [these examples](/Examples.elm#Display)
* The Elm compiler enforces type-correctness and does not do automatic casting.
* Lists use square brackets like arrays, but all items must be of the same type. They are implemented as singly-linked linked lists.
* Tuples such as `(3, True)` may mix types but must be of a known, fixed length.
* Anonymous functions are written `(\var1 var2 -> expression)`.
* Function application happens before any infix operation.

|]

content w =
  flow down <| map (width w) [intro, fromHaskell, fromJS]

main = lift (skeleton content) Window.dimensions
