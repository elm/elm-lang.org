
import Website.Skeleton
import Website.Tiles

words = [markdown|

### Learn by Example

Elm's interactive editor allows you to learn Elm by seeing and modifying
actual code. There are a couple categories of examples for designed to build
certain skills:

[Basic][1] &#8212; the basic building blocks of Elm

[Intermediate][2] &#8212; building components with Elm

[Elm + JavaScript][3] &#8212; use existing JavaScript libraries, features, etc.

[Elm + Haskell][4] &#8212; serve Elm code without leaving Haskell

[This Website][5] &#8212; the Elm source code for this site

[Elm for Games][6] &#8212; how to make games in Elm

<br/>

  [1]: /examples/Basic.elm "Basic"
  [2]: /examples/Intermediate.elm "Intermediate"
  [3]: /examples/ElmJS.elm "Elm + JavaScript"
  [4]: /examples/ElmHaskell.elm "Elm + Haskell"
  [5]: /examples/ThisWebsite.elm "This Website"
  [6]: /blog/games-in-elm/part-0/Making-Pong.html "Elm for Games"

|]

content w = width w words

main = lift (skeleton content) Window.width
