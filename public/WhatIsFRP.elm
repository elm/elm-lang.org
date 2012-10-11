import Website.Skeleton
import Website.ColorScheme


what1 = [markdown|

Functional Reactive Programming (FRP) comes down to one simple idea:
*some values change over time*.

In FRP, these time-varying values are called *signals* and they update automatically.

You can see some signals in action in the colorful box to the right.
Take a second to play around with each of the examples. Try to make them change
and guess what they do.

The first example is the position of the mouse. In Elm, the mouse position
is represented by a signal named `Mouse.position`. When the mouse moves, the value of
`Mouse.position` changes automatically.

The `Window.dimensions` signal works exactly the same way, automatically changing
whenever the window is resized.

The third example uses the `count` function which counts the number of times a
signal is updated. In this case we are counting mouse clicks.

In the fourth example we square the number of clicks. The `lift` function is used
to &ldquo;lift&rdquo; a normal function onto a signal. Once lifted, a function is applied
automatically whenever the signal updates.

These examples are just the basics of FRP. There are tons of other
[interactive examples](/examples/Basic.elm) that allow you to play around
with FRP in [Elm](/). More information on signals can be found [here](/docs/Signal/Signal.elm)
and in the *Related Work* section of
[my thesis](http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf).

FRP becomes truly powerful when paired with a good [graphics library](/docs/Graphics/Element.elm),
making it possible to create pages such as this one ([source](/edit/WhatIsFRP.elm)).

|]

what2 = [markdown|

*Note*: There are no fancy tricks going on here. There are no event handlers or
callbacks or DOM manipulations that I am hiding from you. These things simply
are not necessary with FRP.

Coding these four examples in a traditional GUI framework &ndash; such as JavaScript
&ndash; would require significantly more work and headache. Imagine manually extracting
the mouse position from an event, adjusting the value to deal with cross-browser
incompatabilities, finding a node in the DOM based on its ID, and finally describing exactly
how to destructively modify that node. Not only is that painful to code, but it
also requires broad and deep knowledge of inconsequential things.

FRP makes tasks considerably easier by taking care of the messy &ldquo;how&rdquo;
of events, display, and updates.

|]

why = [markdown|

* * *

## Why FRP is a good idea?

Functional reactive programming (FRP) is a *declarative* approach to GUI design. The
term *declarative* makes a distinction between the &ldquo;what&rdquo; and the
&ldquo;how&rdquo; of programming. A declarative language allows you to say *what* is
displayed, without having to specify exactly *how* the computer should do it.

The term declarative is important only because most current frameworks for graphical
user interfaces are *not* declarative. They mire programmers in the many small,
nonessential details of handling user input and manually modifying the display.

So with FRP, many of the irrelevant details are left
to the compiler, freeing the programmer to think about things that matter.


|]

entry l r = container 250 40 middle l `beside` container 110 40 middle r

example code =
    lift (\info -> entry (text . monospace . toText $ code) (asText info))

clickCount = count Mouse.clicks

examples =
  let title = text . bold . toText in
    [ constant (entry (title "Source Code") (title "Value"))
    , example "Mouse.position" Mouse.position
    , example "Window.dimensions" Window.dimensions
    , example "clks = count Mouse.clicks" clickCount
    , example "lift (\\n -> n^2) clks" (lift (\n -> n^2) clickCount)
    ]

box exs =
  let eBox  = color white $ flow down exs in
  let eBox' = flow right [ color accent2 $ spacer 2 (heightOf eBox)
                         , eBox
                         , color accent3 $ spacer 2 (heightOf eBox) ]
  in  flow down [ color accent1 $ spacer (widthOf eBox') 2
                , eBox'
                , color accent4 $ spacer (widthOf eBox') 2 ]

exampleColumn = lift box $ foldr (lift2 (:)) (constant []) examples

display exs w =
  let hw = w `div` 2 - 15 in
  flow down
    [ [markdown|## What is &ldquo;Functional Reactive Programming&rdquo;? |]
    , flow right [ width hw what1
                 , spacer 30 10
                 , flow down [ spacer hw 50
                             , container hw (heightOf exs) middle exs
                             , spacer hw 50
                             , width hw what2
                             ]
                 ]
    , width w why
    ]

main = lift2 skeleton (lift display exampleColumn) Window.width
