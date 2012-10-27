import Website.Skeleton

---- Text of the page: all written in Markdown ----

content = [markdown|

### About Elm

The following articles provide an introduction to Elm and [FRP][frp].
Each one focuses on a specific question, and aims to increase your
general understanding. When read in sequence, they should slowly
ramp up in difficulty, taking you from beginner to expert.

* [How do I set up Elm on my machine?][install]

* [What is &ldquo;Functional Reactive Programming&rdquo;?][frp]

* [How is FRP different/better than events?][events]

* [What do I need to learn to master Elm?][wiki]

* [How can I create a working website?][this]

* [How can I create a larger interactive project with Elm?][pong]

* [What is the theory behind FRP in Elm?][thesis] (PDF)

  [wiki]: https://github.com/evancz/Elm/wiki "wiki"
  [frp]: /learn/What-is-FRP.elm "What is FRP?"
  [events]: /learn/FRP-vs-Events.elm "FRP vs Events"
  [install]: https://github.com/evancz/Elm/blob/master/README.md#elm "install"
  [this]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "this site"
  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong"
  [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis"

<br/>

### How to Contribute

Lots of details and ideas live [here](/Contribute.elm).

|]

main = lift (skeleton (\w -> width w content)) Window.width
