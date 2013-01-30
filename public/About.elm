import Website.Skeleton

---- Text of the page: all written in Markdown ----

content = [markdown|

## About Elm

The following articles provide an introduction to Elm and [FRP][frp].
Each one focuses on a specific question, and aims to increase your
general understanding. When read in sequence, they should slowly
ramp up in difficulty, taking you from beginner to expert.

#### About the language

* [What is &ldquo;Functional Reactive Programming&rdquo;?][frp]
* [Syntax Reference][syntax]
* [Learn by example][learn]
* [Pattern matching and algebraic data types][adt]
* [Extensible records][records]
* [Escape from Callback Hell][efch]
* [Create a purely functional game][pong] and [a skeleton for making games][games]
* [The semantics of FRP in Elm][thesis]

#### Getting set up

* [Setting up the Elm compiler and server][install]
* [Creating a working website][this]

#### Release notes

* [`0.7.1`][71] &ndash; `Feb '13` &ndash; Touch, Either, and better Keyboard
* [`0.7.0`][7] &ndash; `Jan '13` &ndash; Extensible Records
* [`0.6.0`][6] &ndash; `Dec '12` &ndash; Time, Dates, and whitespace sensitivity
* [`0.5.0`][5] &ndash; `Oct '12` &ndash; Dictionaries, Sets, and Automata 
* [`0.4.0`][4] &ndash; `Sep '12` &ndash; Markdown and better graphics
* [`0.3.6`][36] &ndash; `Aug '12` &ndash; JSON support
* [`0.3.5`][35] &ndash; `Jun '12` &ndash; JavaScript FFI
* [`0.3.0`][3] &ndash; `Jun '12` &ndash; Modules
* `0.1.0` &ndash; `Apr '12` &ndash; Initial Release


  [syntax]: /learn/Syntax.elm "The Syntax of Elm"
  [adt]: learn/Pattern-Matching.elm
  [records]: /learn/Records.elm "Records in Elm"
  [learn]: /Examples.elm "Elm by Example"
  [frp]: /learn/What-is-FRP.elm "What is FRP?"
  [efch]: /learn/Escape-from-Callback-Hell.elm "Escape from Callback Hell"
  [events]: /learn/FRP-vs-Events.elm "FRP vs Events"
  [install]: https://github.com/evancz/Elm/blob/master/README.md#elm "install"
  [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"
  [games]: https://github.com/evancz/Elm/blob/master/Examples/elm-js/GameSkeleton/GameSkeleton.elm#L1 "Game Skeleton"

  [3]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Modules"
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Integration"
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/ "JSON"
  [4]: /blog/announce/version-0.4.0.elm "Graphics Upgrade"
  [5]: /blog/announce/version-0.5.0.elm "Libraries"
  [6]: /blog/announce/version-0.6.elm "Time, Date, and Syntax"
  [7]: /blog/announce/version-0.7.elm "Extensible Records & More"
  [71]: /blog/announce/version-0.7.1.elm "Touch, Keyboard, Either, etc."

#### How to Contribute

Lots of details and ideas live [here](/Contribute.elm).

|]

main = lift (skeleton (\w -> width w content)) Window.width
