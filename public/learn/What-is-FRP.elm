import Website.Skeleton (skeleton)
import open Website.ColorScheme

import JavaScript as JS
import Mouse
import Window


---- Text of the page: all written in Markdown ----

what1 = [markdown|

Functional Reactive Programming (FRP) is a high-level way to work with
interactions. It provides control flow structures for *time*.

FRP is built around the idea of time-varying values, called *signals* in Elm.

You can see some signals in action in the colorful box to the right.
Take a second to play around with each of the examples. Try to make them change
and guess what they do.

The first example is the position of the mouse. In Elm, the mouse position
is represented by a signal named `Mouse.position`. When the mouse moves, the value of
`Mouse.position` changes automatically [[3]][mouse].

  [mouse]: /edit/examples/Reactive/Position.elm "mouse"

The `Window.dimensions` signal works exactly the same way, automatically changing
whenever the window is resized [[4]][dimensions].

  [dimensions]: /edit/examples/Reactive/ResizePaint.elm "dimensions"

The third example uses the `count` function which counts the number of times a
signal is updated. In this case we are counting mouse clicks.

|]

what2 = [markdown|

In the fourth example we square the number of clicks. The `lift` function is used
to &ldquo;lift&rdquo; a normal function onto a signal. Once lifted, a function is applied
automatically whenever the signal updates. So if the function produces static graphics,
[lifting it onto a signal produces an animation][example]!

  [example]: /edit/examples/Reactive/Clock.elm "animation"

These examples are just the basics of FRP. There are tons of other
[interactive examples](/examples/Basic.elm) that allow you to play around
with FRP in [Elm](/). More information on how to use signals can be found
[here](/docs/Signal.elm).

|]

complex1 = [markdown|

## Combining Signals

FRP is also great for working with multiple signals. This means you can combine multiple event sources
really easily.

The following examples start to get fancy with the `Mouse.position` signal. Take a look
and try to figure out what they do.

|]

complex2 = [markdown|<br/>

The first example shows how you combine two different signals. In this case we finding out
the x-coordinate of the mouse as a percentage of screen width. So we can know that the mouse
is 75% of the way accross the screen.

The second example produces a signal of click locations. The `sampleOn` function allows you
to &ldquo;sample&rdquo; a signal whenever a particular signal updates.

In the third example, we keep the mouse position when the mouse is down. The resulting
signal updates only when the mouse is dragged.

The `keepWhen` function takes three args: a signal of booleans, a default value,
and a signal of anything. It returns a signal that only updates when the first
signal is true. The default value is needed in case the first signal is *never* true.
This ensures that the resulting signal always has a value.

Again, this stuff becomes even cooler when you mix it with a good graphics library!

|]

why = [markdown|

## Why is FRP a good idea?

Functional reactive programming (FRP) is a *declarative* approach to GUI design. The
term *declarative* makes a distinction between the &ldquo;what&rdquo; and the
&ldquo;how&rdquo; of programming. A declarative language allows you to say *what* is
displayed, without having to specify exactly *how* the computer should do it.

The term declarative is important only because most current frameworks for graphical
user interfaces are *not* declarative. They mire programmers in the many small,
nonessential details of handling user input and manually modifying the display.

So with FRP, many of the irrelevant details are left
to the compiler, freeing the programmer to think about things that matter.
That means no event handlers, no callbacks, no DOM manipulations. These things simply
are not necessary with FRP in Elm.

Coding these examples in a traditional GUI framework &ndash; such as HTML/CSS/JavaScript
&ndash; would require significantly more work and headache. Imagine manually extracting
the mouse position from an event, adjusting the value to deal with cross-browser
incompatabilities, finding a node in the DOM based on its ID, and finally describing exactly
how to destructively modify that node. Not only is that painful to code, but it
also requires broad and deep knowledge of inconsequential things.

FRP makes tasks considerably easier by taking care of the messy &ldquo;how&rdquo;
of events, display, and updates.

|]


---- Putting together the examples and making them pretty ----

entry w1 w2 v1 v2 = container w1 40 middle v1 `beside` container w2 40 middle v2
example w1 w2 code =
    lift (\info -> entry w1 w2 (text . monospace . toText <| code) (asText info))

clickCount = count Mouse.clicks

examples1 =
  let title = text . bold . toText
      example' = example 250 110
  in [ constant (entry 250 110 (title "Source Code") (title "Value"))
     , example' "Mouse.position" Mouse.position
     , example' "Window.dimensions" Window.dimensions
     , example' "clks = count Mouse.clicks" clickCount
     , example' "lift (\\n -> n^2) clks" (lift (\n -> n^2) clickCount)
     ]

examples2 =
  let title = text . bold . toText
      example' = example 420 200
  in [ constant (entry 420 200 (title "Source Code") (title "Value"))
     , example' "lift2 (/) Mouse.x Window.width" (lift2 (\a b -> toFloat a / toFloat b) Mouse.x Window.width)
     , example' "sampleOn Mouse.clicks Mouse.position" (sampleOn Mouse.clicks Mouse.position)
     , example' "keepWhen Mouse.isDown (0,0) Mouse.position" (keepWhen Mouse.isDown (0,0) Mouse.position)
     ]

box exs =
  let putInBox exs = 
        let eBox  = color white <| flow down exs
            eBox' = flow right [ color accent2 <| spacer 2 (heightOf eBox)
                               , eBox
                               , color accent3 <| spacer 2 (heightOf eBox) ]
        in  flow down [ color accent1 <| spacer (widthOf eBox') 2
                      , eBox'
                      , color accent4 <| spacer (widthOf eBox') 2 ]
  in  lift putInBox <| combine exs



---- Putting it all together into a single page ----

whatIsFRP exs w =
  let hw = w `div` 2 - 15 in
  flow down
    [ [markdown|## What is Functional Reactive Programming? |]
    , flow right [ width hw what1
                 , spacer 30 10
                 , flow down [ spacer hw 20
                             , container hw (heightOf exs) middle exs
                             , spacer hw 20
                             , width hw what2
                             ]
                 ]
    ]

moreOnFRP exs w =
  let hw = w `div` 2 - 15 in
  flow down
    [ width w complex1
    , container w (heightOf exs) middle exs
    , width w complex2
    ]

display exs1 exs2 w = flow down [ whatIsFRP exs1 w, moreOnFRP exs2 w, width w why ]

main = lift2 skeleton (lift2 display (box examples1) (box examples2)) Window.width


---- Setting the title of the page to be prettier ----

titles = constant (JS.fromString "What is FRP?")
foreign export jsevent "title"
  titles : Signal JS.JSString
