import Graphics.Input (Input, input, checkbox)
import Website.Skeleton (skeleton)
import Window
import JavaScript as JS

port title : String
port title = "Interactive UI Elements"

main = skeleton <~ (everything <~ check.signal)
                 ~ Window.dimensions

check : Input Bool
check = input False

everything : Bool -> Int -> Element
everything isChecked wid =
    let w = min 600 wid
        box = checkbox check.handle id isChecked
    in  flow down
        [ width w intro
        , container w 30 middle <| flow right [ box, box, box ]
        , width w rest
        ]

intro = [markdown|

<style type="text/css">
p { text-align: justify }
</style>

<h1><div style="text-align:center">Interactive UI Elements
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Using text fields, drop downs, buttons, etc.*</div></div>
</h1>

Many UI elements are interactive. You can click on them, hover above them, type
into them. In Elm you handle all of these interactions with the
[`Graphics.Input`][gi] library.

At the core of this library is the concept of an `Input`. An `Input` is like a
[port](/learn/Ports.elm) that receives all of its events from the UI. When the
user clicks a button, the event goes to an `Input`. When the user types into a
field, the event goes to an `Input`. I think the best way to really understand
this approach is to see it in action! In this post we will start with an example
and then dive into how it works.

 [gi]: http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/Graphics-Input

## A Minimal Example

We are going to make three synced checkboxes. Changing one of them will change
the two others:

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

Here is the code we need to make that happen. You can play with it in [the
online editor](/try). (Try adding more boxes!)

```haskell
import Graphics.Input (Input, input, checkbox)

check : Input Bool
check = input False

displayBoxes : Bool -> Element
displayBoxes isChecked =
    let box = checkbox check.handle id isChecked
    in  flow right [ box, box, box ]

main : Signal Element
main = displayBoxes <~ check.signal
```

The key things to notice before we go into the API itself is the `Input` named
`check`. It is used twice in the rest of the code:

  1. In `displayBoxes` we create a single `box` that we display three times.
     When we define `box` we refer to `check.handle` which means that all
     `checkbox` events are going to be sent to the `check` input.

  2. In `main` we pipe all of the events from the `check` input to our display.
     As the user toggles between true and false, these events will guide what
     we actually display on screen.

This is a very high level view of how things work, but it illustrates the
typical structure of interactive UI elements in Elm. Let's look at the API
in more detail.

## Inputs

The first thing we do when making an interactive UI element is to create an input:

```haskell
type Input a = { signal : Signal a, handle : Handle a }
```

An `Input` has two distinct parts. One is a `signal` of events coming from UI
elements. In our synced checkbox example, these are boolean values indicating
whether the box should be checked or not. The more subtle part of an `Input` is
the `handle`. All interactive UI elements will latch on to a handle and report
their events to the corresponding `Input`. The handle lets us get events from
the UI back into our program without any messy callbacks or event-listeners.

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

## UI Elements

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
     checked or not. This means `checkbox` is a [pure function][pure]! Whether
     it is checked or not is an argument, so that information must live in the
     model, not in the view!

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Breaking the concept of a checkbox into an `Input`, a `Handle`, and a `checkbox`
makes the flow of events very explicit. I think these functions become much
clearer when you actually see them used!

## Tons of Examples

Synced checkboxes are nice for an introductory example, but it is simple because
it really only has an input and a display section. There is not a lot to model
or update. I have created a bunch of examples to illustrate some more complex
situations. Start with the simple examples:

  * [Text Fields](/edit/examples/Reactive/TextField.elm)
  * [Passwords](/edit/examples/Reactive/Password.elm)
  * [Checkboxes](/edit/examples/Reactive/CheckBox.elm)
  * [Drop Downs](/edit/examples/Reactive/DropDown.elm)
  * [Fields + HTTP](/edit/examples/Reactive/ZipCodes.elm)
  * [Filtered Fields](/edit/examples/Reactive/KeepIf.elm)

And then take a look at some more complex examples:

  * [Field Reversal](/edit/examples/Intermediate/TextReverse.elm)
  * [Calculator](/edit/examples/Intermediate/Calculator.elm)
  * [Sign-up Form](/edit/examples/Intermediate/Form.elm)
  * [Todo List](https://github.com/evancz/TodoFRP)

The APIs for UI elements are brand new, so I hope to see some libraries to
spring up in the [Elm Public Library](library.elm-lang.org) to factor out
common patterns like form-validation, password verification, credit-card
entry, etc.

## Thank you!

Huge thank you to [Spiros Eliopoulos](https://github.com/seliopou) whose work
on [elm-d3](https://github.com/seliopou/elm-d3) led inspired the `signal` field
of an `Input`. Another huge thank you to [Jeff Smitts](https://github.com/Apanatshka) for talking through
Spiros's idea with me and inspiring the `handle` field of an `Input`. Finally
thanks to everyone on [the mailing list][list] for discussing and reviewing
APIs, docs, and examples as the new UI elements library came together!

  [list]: https://groups.google.com/forum/#!forum/elm-discuss

## Misc

The traditional concept of a text field holds state and can trigger events.
Combining so many aspects of a program into this single abstraction tends to
lead to messy callback-riden code.


Separating UI into these two distinct concepts lets us keep the nice structure
of Elm programs. The rest of this post will be going into how the API works and
showing some examples of its use. At the end of the post I will link to a ton of
examples.

## Structure of UI code

Elm forces you to separate your program into input, model, update, and display.
The trick to making text fields and buttons fit this model is figuring out a
way to for the display to communicate with the input:

<img src="/imud-computer.png" style="display:block; margin:auto;" width="460" height="450">

The arrow from Display to the monitor indicates that Elm's runtime is
taking an `Element` from your program and showing it on screen.

The arrow from the monitor to the Input is the special part. When you click on
a button or type into a text field, the Elm runtime generates events that flow
to a particular Input.

|]
