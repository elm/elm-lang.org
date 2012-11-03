import Website.Skeleton

---- Text of the page: all written in Markdown ----

content = [markdown|

### About Elm

The following articles provide an introduction to Elm and [FRP][frp].
Each one focuses on a specific question, and aims to increase your
general understanding. When read in sequence, they should slowly
ramp up in difficulty, taking you from beginner to expert.

* [What is &ldquo;Functional Reactive Programming&rdquo;?][frp]

* [Escape from Callback Hell][efch]

* [What do I need to learn to master Elm?][wiki]

* [How do I set up Elm on my machine?][install]

* [How can I create a working website?][this]

* [How can I create a larger interactive project with Elm?][pong]

* What new features came out in each version of the compiler?
  [0.3][3], [0.3.5][35], [0.3.6][36], [0.4][4], [0.5][5].

* [What is the theory behind FRP in Elm?][thesis] (PDF)

  [wiki]: https://github.com/evancz/Elm/wiki "wiki"
  [frp]: /learn/What-is-FRP.elm "What is FRP?"
  [efch]: /learn/Escape-from-Callback-Hell.elm "Escape from Callback Hell"
  [events]: /learn/FRP-vs-Events.elm "FRP vs Events"
  [install]: https://github.com/evancz/Elm/blob/master/README.md#elm "install"
  [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"

  [3]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "Modules"
  [35]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript Integration"
  [36]: http://www.testblogpleaseignore.com/2012/08/16/elm-0-3-6json-support-and-better-error-messages/ "JSON"
  [4]: /blog/announce/version-0.4.0.elm "Graphics Upgrade"
  [5]: /blog/announce/version-0.5.0.elm "Libraries"

<br/>

### How to Contribute

Lots of details and ideas live [here](/Contribute.elm).

|]

main = lift (skeleton (\w -> width w content)) Window.width
