import Graphics.Input exposing (Input, input, checkbox)
import Website.Skeleton exposing (skeleton)
import Window

port title : String
port title = "Interactive UI Elements"

main = lift2 (skeleton "Learn")
             (everything <~ check.signal)
             Window.dimensions

check : Input Bool
check = input False

everything : Bool -> Int -> Element
everything isChecked wid =
    let w = min 600 wid
        box = checkbox check.handle identity isChecked
    in  flow down
        [ width w intro
        , container w 30 middle <| flow right [ box, box, box ]
        , width w rest
        ]

intro = [markdown|

<h1><div style="text-align:center">Interactive UI Elements
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">Using text fields, drop downs, buttons, etc.</div></div>
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

 [gi]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Input

## A Minimal Example

We are going to make three synced checkboxes. Changing one of them will change
the two others:

|]

rest = [markdown|

Here is the code we need to make that happen. Don't worry about the details too
much yet. This is more to get a feel for the API so we know what we are working
towards when we dive into the details:

```haskell
import Graphics.Input exposing (Input, input, checkbox)

check : Input Bool
check = input False

displayBoxes : Bool -> Element
displayBoxes isChecked =
    let box = checkbox check.handle identity isChecked
    in  flow right [ box, box, box ]

main : Signal Element
main = displayBoxes <~ check.signal
```

The key things to notice before we go into the API itself is the `Input` named
`check`. All UI events are going to flow through `check`. It is used twice in
the rest of the code:

  1. In `displayBoxes` we create a `box` that we display three times.
     The definition of `box` refers to `check.handle` which means that all
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

check : Input Bool
check = input False
```

The argument to `input` serves as the default value of the input&rsquo;s
`signal`. So when we create the `check` input we get:

  1. A signal called `check.signal` with the initial value `False`.
  2. A handle called `check.handle`. Any UI element can refer to `check.handle`
     which would send events to `check.signal`.

Now that we have a way to manage UI events, we need to actually create the
interactive UI elements.

## Interactive UI Elements

There are a bunch of interactive UI elements available in [`Graphics.Input`][gi]
and [`Graphics.Input.Field`][gif] from traditional buttons and drop downs to
functions that can make any `Element` clickable or hoverable. We will focus on
[the `checkbox` function][box] which does a great job illustrating the general
pattern used by interactive UI elements in Elm:

  [gi]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Input
  [gif]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Input-Field
  [box]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Input#checkbox

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
     is not always necessary, as in our synced checkbox example, so a common
     processing function is `id` which passes the boolean value to the
     specified `Input` unmodified.

  3. The third argument is the actual state of the checkbox: whether it is
     checked or not. This means `checkbox` is a [pure function][pure]! Whether
     it is checked or not is an argument, so that information must live in the
     model, not in the view!

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Breaking the concept of a checkbox into an `Input`, a `Handle`, and a `checkbox`
makes the flow of events very explicit. This same pattern is used by all
interactive UI elements, so you will see this again and again as you look at
more examples.

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

I suspect a lot of this code could be shortened, and I hope to see some
libraries spring up in the [Elm Public Library](http://library.elm-lang.org)
to factor out common patterns like form-validation, password verification,
credit-card entry, etc.

## Thank you!

Huge thank you to [Spiros Eliopoulos](https://github.com/seliopou) whose work
on [elm-d3](https://github.com/seliopou/elm-d3) led inspired the `signal` field
of an `Input`. Another huge thank you to [Jeff Smitts](https://github.com/Apanatshka)
for talking through Spiros's idea with me and inspiring the `handle` field of
an `Input`. Finally thanks to everyone on [the mailing list][list] for
discussing and reviewing APIs, docs, and examples as the new UI elements library
came together!

  [list]: https://groups.google.com/forum/#!forum/elm-discuss

|]
