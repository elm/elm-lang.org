
import Website.Blog (skeleton)
import Window
import JavaScript as JS
import Graphics.Input (Input, input, checkbox)

port title : String
port title = "Mysteriously Well-Architected Code"

main = skeleton <~ lift everything check.signal ~ Window.width

check : Input Bool
check = input False

section : Int -> Int -> Element -> Element
section outer inner txt =
    let content = width inner txt
    in  container outer (heightOf content) middle content

everything : Bool -> Int -> Element
everything checked w =
  let outer = truncate (toFloat w * 0.8)
      inner = min 600 outer
      box = checkbox check.handle id checked
  in  flow down
      [ width outer pageTitle
      , section outer inner intro
      , container outer 50 middle <| flow right [ box, box, box ]
      , section outer inner rest
      ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Better than MVC</div>
<div style="font-size: 1.5em;">How FRP in Elm improves upon MVC</div>
</div>
|]

intro = [markdown|

<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
pre {
  background-color: white;
  padding: 10px;
  border: 1px solid rgb(216, 221, 225);
  border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

<br/>
During my week as a resident at [Hacker School](https://www.hackerschool.com/)
I saw a shocking amount of well-architected Elm code.
Students who had never programmed in *any* functional language before were picking
up Elm and writing things like [Vessel](http://slawrence.github.io/vessel/).
I was very impressed that they were able to accomplish so much in just one
week, but the craziest part was that [the code](https://github.com/slawrence/vessel)
always looked great! I mean, these students were very talented, but it did not quite make
sense. These are students learning new syntax, how to understand
[types](/learn/Getting-started-with-Types.elm) and type
inference, figuring out a bunch of libraries, thinking of how their program should
work, and somehow their code *always* came out well-architected.

It turns out that Elm's style of [Functional Reactive Programming](/learn/What-is-FRP.elm)
(FRP) strongly influences the structure of Elm programs. In fact, it *forces*
you to write well-architected code. The most interesting thing is that this
architecture is a fairly substantial improvement over the traditional
[Model-View-Controller][mvc] (MVC) approach. FRP in Elm blah blah blah.

  [mvc]: http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller

## A better way than MVC

At a very high level, all Elm program have the following architecture:

<img src="/imud.png" style="display:block; margin:auto;" width="600" height="240">

An **Input** is a stream of events coming into your program. Inputs are things
like mouse position, key presses, touches, button presses, etc. Any events you
need to describe how your program works. Notice that inputs are completely
separate from the Display! In Elm, you must make all inputs explicit.

The **Model** is a full representation of the current state of the program.
It is an immutable data structure&mdash;often a [record](/learn/Records.elm)&mdash;that
holds all information relevant to your application. There is no cheating with
your model in Elm. If there is information you need, you cannot sneak it into
your display!

The **Update** is a way to update your Model as new Input comes in. In Elm
it is just a collection of [pure functions](http://en.wikipedia.org/wiki/Pure_function)
that transform your model. As is typical in functional languages, this tends to
break up into [small, orthogonal functions](/edit/examples/Intermediate/Mario.elm).
This means the Update code is very easy to unit test or load up into a REPL and
play with.

The **Display** is a set of pure functions that convert a Model into something
you can show on screen. The display code *only* cares about the model:
&ldquo;how do I show this information on screen?&rdquo;

These four parts give you a very clear separation of concerns. Perhaps the most
interesting thing about this architecture is that it *emerges* from Elm's
formulation of FRP. This was not something I set out to design, but rather the
inevitable result of purity, immutability, and Elm's style of FRP.

<span style="color:grey;">
Technical Note: A key property of FRP in Elm is that
all signal graphs are static. This property is not very common in FRP systems,
but it is the core reason that Elm code comes out well-architected every time.
This property also makes it [really simple to implement
hot-swapping](/blog/Interactive-Programming.elm) in Elm. [My
thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)
is a pretty accessible way to dig into this in more detail.
</span>



## What's this got to do with UI?

I bring this up specifically because UI elements like text fields and checkboxes
seem to inherently ruin everything I just said. The traditional concept of a
text field holds state and can trigger events. It *requires* that we have
inputs and model in our display! This is why the Controller in
[Model-View-Controler][mvc] (MVC) can easily turn into a crazy tangled mess.

  [mvc]: http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller

The primary goal of [Elm 0.12](/blog/announce/0.12.elm) was to figure out how
to solve this problem. An Elm program must cleanly separate input, model,
update, and view, so **how can we create a library for UI elements that respects
this structure *and* is pleasant to use?** The answer in previous versions of
Elm respected the structure of Elm programs, but no one could figure out how
the hell it worked.

The new approach is based on the idea that **UI elements and inputs are
separate things and should have separate representations**. A slightly more
precise way to say this is that:

  * All inputs must be defined explicitly. If you have a bunch of checkboxes,
    they will report to a separately defined input.

  * All functions to create UI elements are [pure functions][pure]. They display
    part of the model on screen and nothing more. If you want to change the
    content of a text field, that needs to happen in your model.

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Separating UI into these two distinct concepts lets us keep the nice structure
of Elm programs. The rest of this post will be going into how the API works and
showing some examples of its use. At the end of the post I will link to a ton of
examples.

## Separating inputs and UI elements

Elm forces you to separate your program into input, model, update, and display.
The trick to making text fields and buttons fit this model is figuring out a
way to for the display to communicate with the input:

<img src="/imud-computer.png" style="display:block; margin:auto;" width="460" height="450">

The arrow from Display to the monitor indicates that Elm's runtime is
taking an `Element` from your program and showing it on screen. This happens in
every Elm program. The arrow from the monitor to the Input is the special part.
When you click on a button or type into a text field, the Elm runtime generates
events that flow to a particular Input. The key trick is to explicitly
define the input, the UI element, and crucially *a way for UI elements to refer
to specific inputs*. The next two subsections describe how these three parts
fit together.

### Inputs

The first thing we do when making an interactive UI element is to create an input:

```haskell
type Input a = { signal : Signal a, handle : Handle a }
```

An `Input` has two distinct parts. One is a `signal` of events coming from UI
elements. This signal will be used to update our model. The more subtle part
of an `Input` is the `handle`. This is simply a way for UI elements to refer to
a specific input. This is the arrow from the monitor to the Input. It tells the
Elm runtime where to send events when the user clicks on a check box or a drop
down menu.

You create an `Input` with the `input` function, like this:

```haskell
input : a -> Input a

numbers : Input Int
numbers = input 42
```

The argument to `input` serves as the default value of the input&rsquo;s
`signal`. So when we create the `numbers` input, we get a signal called
`numbers.signal` with the initial value 42 and a handle called `numers.handle`
that any UI element can refer to to send values to `numbers.signal`.

### UI Elements

There are a bunch of UI elements available in [`Graphics.Input`][gi] and
[`Graphics.Input.Field`][gif] but we will focus on checkboxes which do a
great job illustrating the general pattern of UI elements in Elm. [The
`checkbox` function][box] creates checkboxes for everyone to click on, and it
has this type:

  [gi]: http://library.elm-lang.org/catalog/evancz-Elm/0.12/Graphics-Input
  [gif]: http://library.elm-lang.org/catalog/evancz-Elm/0.12/Graphics-Input-Field
  [box]: http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/Graphics-Input#checkbox

```haskell
checkbox : Handle a -> (Bool -> a) -> Bool -> Element
checkbox handle processingFunction checked = ...
```

The three arguments work like this:

  1. Clicking on a checkbox generates an event, and the handle specifies which
     `Input` these events should be sent to. The generated event is a boolean
     value that represents what the user *wants* the checkbox to be. Clicking an
     unchecked box generates a `True` event, and clicking a checked box generates
     a `False` event.

  2. A way to process each event before sending it along to the `Input`
     specified by the handle. This processing function lets us add
     extra information to the event. A common thing to add is an ID to indicate
     which checkbox has been clicked, so when there are four check boxes
     reporting to the same `Input` we can send events like `(True, 1)` to
     indicate that the first box wants to be checked. Adding extra information
     is not always necessary, so a common processing function is `id` which
     passes the boolean value to the specified `Input` unmodified.

  3. The third argument is the actual state of the checkbox: whether it is
     checked or not. This means `checkbox` is a pure function! Whether it is
     checked or not is an argument, so that information must live in your
     model. The display is no more than a data structure that we show on screen.

Breaking the concept of a checkbox into an `Input`, a `Handle`, and a `checkbox`
makes the flow of events very explicit. I think these functions become much
clearer when you actually see them used!

## Two Small Examples

Our first example is a set of three synced checkboxes. Changing one of them
will change the two others:

|]

rest = [markdown|

<style type="text/css">
p { text-align: justify }
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

Here is the code we need to make that happen! It is also available on
[share-elm](http://share-elm.com/sprout/53210898e4b0f7cc0dd4e6bd) so you
can play with it yourself.

```haskell
import Input (Input, input, checkbox)

check : Input Bool
check = input False

displayBoxes : Bool -> Element
displayBoxes isChecked =
    let box = checkbox check.handle id isChecked
    in  flow right [ box, box, box ]

main : Signal Element
main = displayBoxes <~ check.signal
```

In this example, we first create the `check` input for checkboxes to report to.
We have given the `input` function `False`, so the default value of `check.signal`
is `False` and the checkboxes will be unchecked at first.

Next we set up the `displayBoxes` function which shows three check boxes that
are checked or unchecked depending on whether the `isChecked` argument is true
or false. Notice that when we create the `checkbox` we tell it to report to the
`check` input. We give `id` as the processing function, so we pass the events
directly to `check.signal` unchanged.

Finally, in `main` we display the current value of `check.signal` with
`displayBoxes`.

Synced checkboxes are nice for an introductory example, but it is simple because
it really only has an input and a display section. There is not a lot to model
or update. In the next example we will have three checkboxes that change
independently. This means we will need to model the state of these boxes and
write code to update the model when new events come in.

```haskell
import Input (Input, input, checkbox)

check : Input (Int, Bool)
check = input False

displayBoxes : Bool -> Element
displayBoxes isChecked =
    let box = checkbox check.handle id isChecked
    in  flow right [ box, box, box ]

main : Signal Element
main = displayBoxes <~ check.signal
```

## More Examples

Start with the simple examples:

  * [text fields](/edit/examples/Reactive/TextField.elm)
  * [passwords](/edit/examples/Reactive/Password.elm)
  * [checkboxes](/edit/examples/Reactive/CheckBox.elm)
  * [drop downs](/edit/examples/Reactive/DropDown.elm)
  * [fields + HTTP](/edit/examples/Reactive/ZipCodes.elm)
  * [filtered fields](/edit/examples/Reactive/KeepIf.elm)

And then take a look at some more advanced examples:

  * [field reversal](/edit/examples/Intermediate/TextReverse.elm)
  * [calculator](/edit/examples/Intermediate/Calculator.elm)
  * [sign-up form](/edit/examples/Intermediate/Form.elm)
  * [todo list](https://github.com/evancz/TodoFRP)

These API is brand new, so I hope to see some libraries to spring up in the
[Elm Public Library](library.elm-lang.org) to factor out common patterns like
form-validation, password verification, credit-card entry, etc.

<br>

## Thank you!

Huge thank you to [Spiros Eliopoulos](https://github.com/seliopou) whose work
on [elm-d3](https://github.com/seliopou/elm-d3) led inspired the `signal` field
of an `Input`. Another huge thank you to Jeff Smitts for talking through
Spiros's idea with me and inspiring the `handle` field of an `Input`. Finally
thanks to everyone on [the mailing list][list] for discussing and reviewing
APIs, docs, and examples as the new UI elements library came together!

  [list]: https://groups.google.com/forum/#!forum/elm-discuss

|]
