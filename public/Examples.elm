
import Website.Skeleton (skeleton)
import Window

words = [markdown|

### Learn by Example

Elm's interactive editor allows you to learn Elm by seeing and modifying
actual code. There are a couple categories of examples for designed to build
certain skills.

You may also want to take a look at the [overview of Elm&rsquo;s syntax][syntax].

* [Basic][1]

* [Intermediate][2]

* [Elm + JavaScript][4]

* [Elm + Haskell][5]

* [This Website][6]

* [Elm for Games][7]

<br/>

Grzegorz has created a great project called [Preselm][preselm] that uses
Elm as a framework for online presentations. The [source is available][src]
too! He has also created some cool [smaller examples][3] too.


  [1]: /examples/Basic.elm "Basic"
  [2]: /examples/Intermediate.elm "Intermediate"
  [3]: http://www.grzegorzbalcerek.net/elm/index.html "Syntax and More"
  [4]: /examples/ElmJS.elm "Elm + JavaScript"
  [5]: /examples/ElmHaskell.elm "Elm + Haskell"
  [6]: /examples/ThisWebsite.elm "This Website"
  [7]: /blog/games-in-elm/part-0/Making-Pong.html "Elm for Games"

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"
  [preselm]: http://www.grzegorzbalcerek.net/preselm/Tutorial.html "Preselm"
  [src]: https://github.com/grzegorzbalcerek/Preselm "Preselm Source"
|]

content w = width w words

main = lift (skeleton content) Window.width
