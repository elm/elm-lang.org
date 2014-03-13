
import Website.Skeleton (skeleton)
import Window
import JavaScript as JS
import Graphics.Input (Input, input, checkbox)

port title : String
port title = "UI in Elm 0.12"

main = skeleton <~ lift everything check.signal ~ Window.dimensions

check : Input Bool
check = input False

everything : Bool -> Int -> Element
everything checked wid =
    let w = min 600 wid
        box = checkbox check.handle id checked
    in  flow down
        [ width w intro
        , container w 50 middle <| flow right [ box, box, box ]
        , width w rest
        ]

intro = [markdown|

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

<h1><div style="text-align:center">UI in Elm 0.12
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Accidentally writing well-architected code*</div></div>
</h1>

[The 0.12 release of Elm](/blog/announce/0.12.elm) introduced an entirely new API
for user input. This post explains the thinking and motivation behind its design
and how to actually use the API to do real things!

## Accidentally well-architected code

Due to the design of FRP in Elm, Elm programs *always* break into four distinct
parts:

  1. Input &mdash; events from “the world” (keyboard, mouse, touch, time, etc.)

  2. Model &mdash; a full representation of our Elm component.

  3. Update &mdash; functions for updating our model based on inputs.

  4. Display &mdash; functions describing the user’s view of the model.

This diagram shows how values would flow through each of these four parts:

<img src="/imud.png" style="display:block; margin:auto;" width="510" height="200">

The key take away here is not that this is a good architecture for designing a
GUI or that you should always try to split your projects into these parts. I
mean, those are true things, but the important take away is that **the design of
FRP in Elm *forces* you to to write code with this architecture**. I often find
myself at the end of a messy hacking / prototyping session with Elm code that
is mysteriously well-architected. I know I did not plan ahead or have a deep
understanding of how the different parts of my program would eventually fit
together, yet the input, model, update, and display are all neatly separated in
my rough draft. This ultimately comes down to the specific design choices made
in Elm's formulation of FRP. The fact that programs tend to be easy and pleasant
to write makes the &ldquo;accidentally well-architected&rdquo; property one of
the most important parts of Elm. You are not paying more in pain or frustration
to get a better result.

I bring this up specifically because I wanted to keep &ldquo;accidentally
well-architected&rdquo; property when introducing typical user input elements
like buttons, checkboxes, and text fields. As people who have used MVC
frameworks will know, UI elements seem to inherently ruin our nice separation
of concerns: by introducing a text field, we suddenly have inputs and model in
our view! Until now, UI elements in Elm have used a rather convoluted and
confusing API to keep the clean division between inputs, model, update, and
display. This release finally introduces UI elements that are easy to use *and* 
keep the &ldquo;accidentally well-architected&rdquo; property of Elm!

## Making inputs explicit

With a traditional text field, it is both a UI element *and* an input to your
program. It is defined to be both a visual and interactive. These text fields
also hold state in the view. The fact that text fields exist in this form in
JS guarantees that your input, update, and view code will become intermingled.
The API itself strongly encourages you to write messy code.

This release introduces the concept of an `Input`. When I type in a text field
or click a button, that event will always be sent to an explicitly defined
`Input`. By making inputs explicit, we decouple our input and view. We'll look
at the API itself and then see it used in an example:

```haskell
type Input a = { handle : Handle a, signal : Signal a }
```

An `Input` has two distinct parts. The first is a `handle` that UI elements can
refer to. The second is a `signal` of events coming from those UI elements. We
create an `Input` with the `input` function, like this:

```haskell
input : a -> Input a

numbers : Input Int
numbers = input 42
```

The argument to `input` serves as the default value of the `Input`&rsquo;s
`signal`, so the initial value of `numbers.signal` is 42. To make this more
concrete, let's see an example of synced checkboxes:

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

main : Signal Element
main = displayBoxes <~ check.signal

displayBoxes : Bool -> Element
displayBoxes checked =
    let box = checkbox check.handle id checked
    in  flow right [ box, box, box ]
```

In this example, we first create the `check` input for checkboxes to report to.
We have given the `input` function `False`, so the default value of `check.signal`
is `False`. In `main` we display the current value of `check.signal` with
`displayBoxes`. This function displays three synced checkboxes using
[`checkbox`][checkbox] from the overhauled [`Graphics.Input`][input] library.
This is where we see the new API in action! The type of `checkbox` looks like
this:

  [checkbox]: http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/Graphics-Input#checkbox
  [input]: http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/Graphics-Input

```haskell
checkbox : Handle a -> (Bool -> a) -> Bool -> Element
```

The first argument is a handle, specifying which `Input` to report to. In our
example that is `check.handle`. Next we have a function that processes input before
it is reported to `check.signal`. In our example, we use the `id` function to pass
the new value along to the `check` input. Finally, we give the actual value of the
checkbox, whether it is checked or not. So the result of `(displayBoxes True)`
would show three checked boxes and `(displayBoxes False)` would show three
unchecked ones.

Notice that `checkbox` is a pure function! As a result, *no state is held in
the display*. The display is simply a data structure that we show on screen,
so we can keep the clean separation of input, model, update, and display:

<img src="/imud-computer.png" style="display:block; margin:auto;" width="460" height="510">

The arrow coming from Display is the static image to show on screen. When you click on
a button or type into a text field, Elm generates events that flow to the Input.

## Fancier Checkboxes

Synced checkboxes are nice for an introductory example, but it is simple specifically
because the model and update are very trivial. This section will show another example
that is a bit more complicated.

|]
