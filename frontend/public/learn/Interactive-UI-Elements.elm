import Graphics.Element (..)
import Graphics.Input (checkbox)
import Markdown
import Signal ((<~), map2)
import Signal 
import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Interactive UI Elements"

main = map2 (skeleton "Learn")
            (everything <~ Signal.subscribe check)
            Window.dimensions

check : Signal.Channel Bool
check = Signal.channel False

everything : Bool -> Int -> Element
everything isChecked wid =
    let w = min 600 wid
        box = checkbox (Signal.send check) isChecked
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

At the core of this library is the concept of a `Channel`. A `Channel` is like a
[port](/learn/Ports.elm) that receives all of its events from the UI. When the
user clicks a button, the event goes to a `Channel`. When the user types into a
field, the event goes to an `Channel`. I think the best way to really understand
this approach is to see it in action! In this post we will start with an example
and then dive into how it works.

 [gi]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Graphics-Input

## A Minimal Example

We are going to make three synced checkboxes. Changing one of them will change
the two others:

"""

rest : Element
rest = Markdown.toElement """

Here is the code we need to make that happen. Don't worry about the details too
much yet. This is more to get a feel for the API so we know what we are working
towards when we dive into the details:

```haskell
import Graphics.Input (checkbox)
import Signal

check : Signal.Channel Bool
check = Signal.channel False

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
     The definition of `box` refers to `check` which means that all
     `checkbox` events are going to be sent to the `check` channel.

  2. In `main` we pipe all of the events from the `check` channel to our display.
     As the user toggles between true and false, these events will guide what
     we actually display on screen.

This is a very high level view of how things work, but it illustrates the
typical structure of interactive UI elements in Elm. Let's look at the API
in more detail.

## Channels

The first thing we do when making an interactive UI element is to create a channel:

```haskell
type Channel a = Channel
```

A `Channel` has two distinct uses. One is to provide a `Signal` of events coming from UI
elements. In our synced checkbox example, these are boolean values indicating
whether the box should be checked or not. This `Signal` can be subscribed to. The more subtle part of a `Channel` is
to provide a `Signal.Message` handler. All interactive UI elements will latch on to a message handler and so report
their events to the corresponding `Channel`. The message handler lets us get events from
the UI back into our program without any messy callbacks or event-listeners.

You create a `Channel` with the `channel` function, like this:

```haskell
channel : a -> Channel a

check : Signal.Channel Bool
check = Signal.channel False
```

The argument to `channel` serves as the default value of the channel&rsquo;s
`Signal`. So when we create the `check` channel we can:

  1. Subscribe to the channel using `Signal.subscribe check`, to receive all its
     messages as a `Signal`, the initial value being `False` in the example.
  2. Send a `Signal.Message` to the `check` channel, by calling `Signal.send check value`.
     Any UI element can send a message to the `check` channel, which would pass it
     along through a `Signal` to its subscribers.

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
checkbox handler checked = ...
```

The two arguments work like this:

  1. Clicking on a checkbox generates an event, and the handler specifies which
     `Channel` these events should be sent to. The generated event is a boolean
     value that represents what the user *wants* the checkbox to be. Clicking an
     unchecked box generates a `True` event, and clicking a checked box generates
     a `False` event.

  2. The second argument is the actual state of the checkbox: whether it is
     checked or not. This means `checkbox` is a [pure function][pure]! Whether
     it is checked or not is an argument, so that information must live in the
     model, not in the view!

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Breaking the concept of a checkbox into a `Channel`, a `Signal.Message` handler, and a `checkbox`
makes the flow of events very explicit. This same pattern is used by all
interactive UI elements, so you will see this again and again as you look at
more examples.

## Tons of Examples

Synced checkboxes are nice for an introductory example, but it is simple because
it really only has a channel and a display section. There is not a lot to model
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
