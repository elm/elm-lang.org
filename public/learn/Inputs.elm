
import Website.Skeleton (skeleton)
import Window
import JavaScript as JS

port title : String
port title = "Elm 0.11 - Ports"

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  width w intro

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

<h1><div style="text-align:center">Elm 0.12
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Purely functional user input*</div></div>
</h1>

For the past few months I have been slowly but surely pushing Elm to be an
industry-ready language. I released [the REPL](/blog/announce/Repl.elm), the
[package manager](/blog/announce/PackageManager.elm), and made [huge
improvements to JS interop](/blog/announce/0.11.elm). This release makes the
next step by making it easy to work with input elements like text fields and
checkboxes.

Working with input elements has long been pretty tricky in Elm. After
conferences I always get questions along the lines of, &ldquo;that
[Mario](/edit/examples/Intermediate/Mario.elm) example is really cool, but can
I use this approach for the web forms and dashboards I write every day at
work?&rdquo; As of today, the answer is yes! Elm 0.12 completely overhauls the
`Graphics.Input` library and introduces the `Graphics.Input.Field` library to
make it easy to create and style inputs.

This release also expands and improves Elm's core libraries quite dramatically.

  * Overhaul `Graphics.Input` library (inspired by @seliopou and Jeff Smits)
  * Overhaul `Text` library to accomodate new `Graphics.Input.Field`
    library and make the API more consistent overall
  * Add `Graphics.Input.Field` for customizable text fields

# Practical Libraries

This release introduces the
[`Trampoline`](http://library.elm-lang.org/catalog/evancz-Elm/0.12/Trampoline) and
[`Debug`](http://library.elm-lang.org/catalog/evancz-Elm/0.12/Debug) libraries.
Both are very practical libraries to make development a bit easier. The `Trampoline`
library helps you get around JavaScript's lack of tail call elimination in a fully
general way. The `Debug` library allows you to log to the console. It is intended
solely for debugging!


  * Add `Trampoline` library () 
  * Add `Debug` library (inspired by @timthelion)

This release also 
  * Overhaul `Regex` library (inspired by Attila Gazso)

Thanks to [Max New](http://github.com/maxsnew) and [Tim Hobbs](timthelion)
for designing and advocating the `Trampoline` library. Thanks again to Tim
who motivated the `Debug` library, initially providing a more comprehensive
approach.

# IDE-oriented changes

All libraries uploaded to [library.elm-lang.org](http://library.elm-lang.org/)
generate a JSON file filled with types, documentation, and precedence/associativity
for all exported values ([like
this](http://library.elm-lang.org/catalog/evancz-Elm/0.12/docs.json)). The goals
is to make it really easy to work with library metadata to create tools like
Elmoogle and auto-complete in IDEs. This release improves the format for types,
making them much easier to work with.

Another change that considered IDE support was to get rid of the `open` keyword
in module imports. Now if you want to import everything from the `List` module
into local scope you use this syntax:

```haskell
import List (..)
```

This is the least prefered method of importing values of [the four
possiblities](/learn/Syntax.elm#modules). It is convenient for quickly
prototyping or hacking something together, but it does not scale well. Imagine
you do [26 imports like this][imports], bringing tons of functions into local
scope. When I want to find the definition of [`isFunPtrTy`][function] I have no
easy way to know which of those 26 modules it came from!

 [imports]: https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcForeign.lhs#L33-L60
 [function]: https://github.com/ghc/ghc/blob/master/compiler/typecheck/TcForeign.lhs#L326

So use this new syntax with care. I hope the ellipsis in `import List (..)` will
entice you to fill in the particular values you are using. As IDE support for
Elm improves, it will become possible to automate this dependency finding, so
my eventual hope is that `import List (..)` can be removed entirely.


The traditional concept of a text field holds state and can trigger events.
Combining so many aspects of a program into this single abstraction tends to
lead to messy callback-riden code.

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
taking an `Element` from your program and showing it on screen. With MVC you
would be manually mutating the view in your controller.

The arrow from the monitor to the Input is the special part. When you click on
a button or type into a text field, the Elm runtime generates events that flow
to a particular Input. Again, with MVC you would be doing this manually in the
controller. The key trick to make it possible for the Elm runtime to fully
manage this is to explicitly define Inputs. The next two subsections
describe how this works in detail.

### Inputs

The first thing we do when making an interactive UI element is to create an input:

```haskell
type Input a = { signal : Signal a, handle : Handle a }
```

An `Input` has two distinct parts. One is a `signal` of events coming from UI
elements. This signal will be used to update our model. The more subtle part
of an `Input` is the `handle`. This is how UI elements refer to a specific input.
This is the arrow from the monitor to the Input. It tells the Elm runtime where
to send events when the user clicks on a check box or a drop down menu.

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

## A Simple Example

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

|]
