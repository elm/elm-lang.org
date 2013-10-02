
import Website.Skeleton (skeleton)
import Window

words = [markdown|

# Examples

Elm's [interactive editor](/try) allows you to learn Elm by seeing and modifying
actual code. These examples are designed to build skills as you move down the page.

Check the [Elm syntax reference][syntax] when you see new syntax.

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

### Basics

Examples here

### Intermediate

Prettier examples here

|]

content w = width w words

main = lift (skeleton content) Window.dimensions
