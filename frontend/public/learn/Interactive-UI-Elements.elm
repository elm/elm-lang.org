import Graphics.Input (checkbox)
import Graphics.Element (..)
import Markdown
import Website.Skeleton (skeleton)
import Signal
import Window

port title : String
port title = "Interactive UI Elements"

main : Signal.Signal Element
main = Signal.map2 display checkSignal Window.dimensions

display: Bool -> (Int, Int) -> Element
display checked (w,h) =
  skeleton "Learn" (content checked << min 600) (w,h)

check : Signal.Channel Bool
check = Signal.channel False

checkSignal = Signal.subscribe check

content isChecked w =
    let box = checkbox (Signal.send check) isChecked
    in  flow down
        [ width w intro
        , container w 30 middle <| flow right [ box, box, box ]
        , width w rest
        ]


intro : Element
intro = Markdown.toElement """

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

"""

rest = Markdown.toElement """

Here is the code we need to make that happen. Don't worry about the details too
much yet. This is more to get a feel for the API so we know what we are working
towards when we dive into the details:

```haskell
import Graphics.Input (checkbox)

check : Signal.Channel Bool
check = Signal.channel True

displayBoxes : Bool -> Element
displayBoxes isChecked =
    let box = checkbox (Signal.send check) isChecked
    in  flow right [ box, box, box ]

main : Signal Element
main = displayBoxes <~ Signal.subscribe check
```

The key things to notice before we go into the API itself is the `Channel` named
`check`. All UI events are going to flow through `check`. It is used twice in
the rest of the code:

  1. In `displayBoxes` we create a `box` that we display three times.
     The definition of `box` refers to `Signal.send check` which means that all
     `checkbox` events are going to be sent to the `check` input.

  2. In `main` we pipe all of the events from the `check` input to our display.
     As the user toggles between true and false, these events will guide what
     we actually display on screen.

This is a very high level view of how things work, but it illustrates the
typical structure of interactive UI elements in Elm. Let's look at the API
in more detail.

## Inputs

The first thing we do when making an interactive UI element is to create a channel:

```haskell
channel : a -> Channel a
subscribe : Channel a -> Signal a
send : Channel a -> a -> Message
```

A `Channel a` can be easily 'transformed' to `Signal a` via `subscribe`.
The `send` function returns a `Message` function that the input will call
foreach UI event. You can look at a `Channel` as a mechanism to transform
messy callbacks to a signal.

You create a `Channel` with the `channel` function, like this:

```haskell
channel : a -> Channel a

check : Channel Bool
check = channel False
```

The argument to `Channel` serves as the default value of the channel&rsquo;s
`signal`. So when we create the `check` channel we get:

  1. A signal we can `subscribe` to with the initial value `False`.
  2. A mechanism to `send` messages to this channel. Any UI element can call
     the message function `(send check)` which would send events to the subsribed signal.

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
checkbox : (Bool -> Signal.Message) -> Bool -> Element
checkbox message checked = ...
```

The two arguments work like this:

  1. Clicking on a checkbox generates an event, and the message function
     is the mechanism through which these events should be sent to.
     The generated event is a boolean value that represents what the user *wants*
     the checkbox to be. Clicking an unchecked box generates a `True` event,
     and clicking a checked box generates a `False` event.

  3. The checked argument is the actual state of the checkbox: whether it is
     checked or not. This means `checkbox` is a [pure function][pure]! Whether
     it is checked or not is an argument, so that information must live in the
     model, not in the view!

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Breaking the concept of a checkbox into a `Channel` and a `checkbox`
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

"""
