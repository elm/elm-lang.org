
import Website.Skeleton
import Signal.Window as Window

info w = width w [markdown|

### Installation

See these [install instructions][1] to get Elm running on your machine.
If you run into problems, you should email the [mailing list][2]
or report an issue to Elm's [source repository][3]

<br/>

### Thesis on Elm

My [thesis on Elm][4] is available too. It provides a more formal
definition of Elm and a discription of Concurrent FRP, a new approach
to efficient Functional Reactive Programming.

  [1]: https://github.com/evancz/Elm/blob/master/README.md "install instructions"
  [2]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "email list"
  [3]: https://github.com/evancz/Elm "source repository"
  [4]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "thesis on Elm"

|]
 
main = lift (skeleton info) Window.width
