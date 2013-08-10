import Website.Skeleton (skeleton)
import JavaScript as JS
import Window


---- Text of the page: all written in Markdown ----

content = [markdown|

### What is the difference between FRP and Events?

Event handlers *do* things. They are fundamentally imperative. You get a value and then you
go change something in the world.

With FRP, you never have to *do* things.

Say you want to do a canvas drawing where a pentagon follows the mouse.

* With HTML/CSS/JS, you start by creating a `\<canvas\>` node and an event listener
  on mouse movements. Whenever an event happens you find the `\<canvas\>`, you
  `getContext('2D')`, you erase everything, and you draw a brand new pentagon.
  This description does not do service to how much code would go into doing this!

* In FRP, you create a pure function that takes a position and returns a scene with
  a pentagon at that position. You then `lift` that function onto the `Mouse.position`
  signal and you are done. You have an animation that updates automatically.
  You never had to go manually erase things and redraw. You did not *do* anything.
  Those details were left to the compiler.

[This example][transform] shows how easy this task is in FRP.
The important thing is that graphics are way easier in Elm, so in the end it becomes a
lot simpler to describe a complex animation or interaction.

  [transform]: http://elm-lang.org/edit/examples/Reactive/Transforms.elm "transform"


### Ok, but why is that better?

The event loop is fundamentally sequential. It processes one event at a time, no exceptions.
There is no notion of concurrency, no way to easily run many functions at the same time. To
make this more concrete:

* If you have a long computation, it will block other events. This causes the GUI to freeze!
  To avoid this, you must manually break the computation into small pieces.
  The logic of the computation is broken into phases that make the code more confusing and disorganized.
* If you make asynchronous calls you end up creating callbacks within callbacks within
  callbacks to handle the data when it arrives.

With these problems in mind, Elm was designed to work great with concurrency and
asynchrony ([details][thesis]).

FRP makes it easy to work with asynchronous events, no callback hell!
[This example][ajax] makes asynchronous HTTP requests and has a very clear, linear
control flow.

Elm is able to handle concurrency and asynchrony in a natural way *specifically*
because it is a pure language, as described above. There are no risks of [deadlock][deadlock]
and no need to use fancy synchronization techniques because Elm makes it impossible
for these problems to crop up.

  [deadlock]: http://en.wikipedia.org/wiki/Deadlock "Deadlock"

I am working on more examples to help clarify the benefits of having a built-in
notion of concurrency and asynchrony.

*Note:* Although Elm can support concurrency in theory, this feature is not yet available
in the current implementation. Asynchrony is already there though!

  [thesis]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "Concurrent FRP"
  [ajax]: http://elm-lang.org/edit/examples/Reactive/ZipCodes.elm "AJAX"

|]


main = lift (skeleton (\w -> width w content)) Window.width


---- Setting the title of the page to be prettier ----

titles = lift JS.fromString (constant "FRP vs Events")
foreign export jsevent "title"
  titles : Signal JS.JSString