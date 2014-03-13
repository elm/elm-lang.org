
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
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Mysteriously well-architected code*</div></div>
</h1>

[The 0.12 release of Elm](/blog/announce/0.12.elm) introduced an entirely new
API for user input. Creating traditional forms with buttons, text fields, and
check boxes should finally be a pleasant experience in Elm. This post explains:

  1. The thinking and motivation behind the API.
  2. How to actually use the API in real life!

We will walk through some smaller examples, 

## Mysteriously well-architected code

I often find myself at the end of a messy hacking / prototyping session with
Elm code that is mysteriously well-architected. I know I did not plan ahead or
have a deep understanding of how the different parts of my program would
eventually fit together, yet the input, model, update, and display are all
neatly separated in my rough draft. Okay, but I designed the language so surely
it is no surprise if my Elm code turns out nice, right?

I really took note of &ldquo;mysteriously well-architected code&rdquo; when I
observed it during my week at [Hacker School](https://www.hackerschool.com/).
Students who had never programmed in *any* functional language before were picking
up Elm and writing things like [Vessel](http://slawrence.github.io/vessel/),
and the craziest part was that [the code](https://github.com/slawrence/vessel)
always looked great! These are students learning new syntax, how to understand
types and type inference, figuring out a bunch of libraries, thinking of how
their program should work, and somehow their code always came out well-architected.
They all modelled the problem, created a way to update the model based on inputs,
and had a section of code for display. All neatly separated!

I knew that Elm's version of FRP strongly influenced the structure of Elm
programs, but at this point I began to suspect that it actually *forces* people
to write well-architected code. Regardless of skill level, experience, or
knowledge of FRP, the code just comes out well-architected.

## How can this be?

FRP in Elm is somewhat unique in that all signal graphs are static. This
is the same property that made it really simple to implement [hot-swapping in
Elm](/blog/Interactive-Programming.elm). [My
thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf)
is a pretty accessible way to figure out what the hell &ldquo;all signal graphs
are static&rdquo; means *exactly*, but I am about to explain the &ldquo;mysteriously
well-architected code&rdquo; in a way that does not require that kind of background.

Due to the design of FRP in Elm, time-dependencies are always explicit *in your
source code*. As a rough example, consider that position and velocity of
[Mario](/edit/examples/Intermediate/Mario.elm) depends on time passing and
arrow keys. In Elm, you say &ldquo;Mario is what you get when you combine time
and key presses in this exact way&rdquo;. Describing Mario in Elm *requires*
you to make the dependencies explicit! As a result, Elm programs always break
into four distinct parts:

  1. Input &mdash; events from “the world”: mouse, touch, time, etc.

  2. Model &mdash; a full representation of the state in the program

  3. Update &mdash; functions for updating our model based on inputs

  4. Display &mdash; functions describing the user’s view of the model

So at a very high level you can think of an Elm program as having this structure:

<img src="/imud.png" style="display:block; margin:auto;" width="510" height="200">

The Elm runtime sends events from the world to your inputs, you use those events
to update your model, you describe how that should get displayed, and the Elm runtime
takes care of showing it on screen. This is just the structure of Elm programs, so
anything you write will come out with this architecture!

## What's this got to do with UI?

I bring this up specifically because UI elements like text fields and checkboxes
seem to inherently ruin our nice separation of concerns. The traditional concept
of a text field holds state and can trigger events. It *requires* that we have
inputs and model in our display! This is why the Controller in
[Model-View-Controler][mvc] (MVC) can easily turn into a crazy tangled mess.

  [mvc]: http://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller

The primary goal of Elm 0.12 was to figure out how to solve this problem.
An Elm program must cleanly separate input, model, update, and view, so **how
can we create a library for UI elements that respects this structure *and* is
pleasant to use?** The earlier version respected the structure of Elm programs,
but no one could figure out how the hell it worked.

The new approach is based on the idea that **UI elements and inputs are
separate things and should have separate representations**. A slightly more
precise way to say this is that:

  * All inputs must be defined explicitly. If you have a bunch of checkboxes,
    they will report to an input.

  * All functions to create UI elements are [pure functions][pure]. They display
    part of the model on screen and nothing more. If you want to change the
    content of a text field, that needs to happen in your model.

  [pure]: http://en.wikipedia.org/wiki/Pure_function

Separating UI into these two distinct concepts lets us keep the nice structure
of Elm programs. The rest of this post will be going into the particulars of
how the API works and showing some examples of its use, but if you prefer to
just read code you can jump right into it with some simple examples
([text fields](/edit/examples/Reactive/TextField.elm),
[passwords](/edit/examples/Reactive/Password.elm),
[checkboxes](/edit/examples/Reactive/CheckBox.elm),
[drop downs](/edit/examples/Reactive/DropDown.elm),
[fields + HTTP](/edit/examples/Reactive/ZipCodes.elm),
[filtered fields](/edit/examples/Reactive/KeepIf.elm))
or some more advanced examples
([field reversal](/edit/examples/Intermediate/TextReverse.elm),
[calculator](/edit/examples/Intermediate/Calculator.elm),
[sign-up form](/edit/examples/Intermediate/Form.elm),
[todo list](https://github.com/evancz/TodoFRP)).

## Inputs

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
`signal`, so the initial value of `numbers.signal` is 42.




To make this more
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
