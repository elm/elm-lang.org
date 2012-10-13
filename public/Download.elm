
import Website.Skeleton
import Signal.Window as Window

install = [markdown|

### Installation

What you are getting:

1. A **self-contained server** that serves Elm files, images, videos, HTML, JavaScript,
   and anything else you can think of with no set up.
   No need to mess around with Apache or PHP or Ruby or whatever else.
   The server just works, so you can focus on things that matter.

2. A **compiler** that turns Elm code into HTML, CSS, and JavaScript. You can
   then serve those files however you see fit.

See these [install instructions][1] to get Elm running on your machine.
If you run into problems, you should email the [mailing list][2]
or report an issue to Elm's [source repository][3]

  [1]: https://github.com/evancz/Elm/blob/master/README.md#elm "install instructions"
  [2]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "email list"
  [3]: https://github.com/evancz/Elm "source repository"

|]

other = [markdown|

### Source Code

The [source code of the Elm compiler and server](https://github.com/evancz/Elm)
is available, so please browse, fork, and contribute to the project.

The [source code of this website](https://github.com/evancz/elm-lang.org) is also
available. Follow [these instructions][instruct] and you can easily get this entire website
running on your own machine and use Elm's interactive editor locally. From there
you can use it as a starting point for creating your own website!

 [instruct]: https://github.com/evancz/elm-lang.org#elm-langorg-a-template-for-creating-websites-in-elm "install"

<br/>

### Thesis on Elm

My [thesis on Elm][4] is available too. It provides a more formal
definition of Elm and a discription of Concurrent FRP, a new approach
to efficient [Functional Reactive Programming][frp].

  [4]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis on Elm"
  [frp]: /learn/What-is-FRP.elm "functional reactive programming"

|]

info w =
  let hwidth = if w < 800 then w `div` 2 - 20 else 380 in
  flow right [ width hwidth install, spacer 40 10, width hwidth other ]

main = lift (skeleton info) Window.width
