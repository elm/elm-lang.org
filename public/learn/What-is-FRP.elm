import Website.Skeleton (skeleton)
import Website.ColorScheme as C

import Mouse
import Window

port title : String
port title = "What is FRP?"

main = lift2 (skeleton "Learn")
             (lift2 display (box examples1) (box examples2))
             Window.dimensions

display exs1 exs2 outer =
    let w = 800
        body = flow down [ whatIsFRP exs1 w, moreOnFRP exs2 w, width w why ]
    in
        container outer (heightOf body) middle body


---- Text of the page: all written in Markdown ----

what1 = [markdown|

Functional Reactive Programming (FRP) provides control flow structures for
events. It gives you a high-level way to describe interactions with a mouse,
keyboard, server, etc.

*Signals* are the key concept in FRP. A signal is a value that changes over time.

You can see some signals in action in the blue box to the right.
Take a second to play around with each of the examples. Try to make them change
and guess what they do.

The first example is the position of the mouse. In Elm, the mouse position
is represented by a signal named `Mouse.position`. When the mouse moves, the value of
`Mouse.position` changes automatically ([play][mouse]).

  [mouse]: /edit/examples/Reactive/Position.elm "mouse"

The `Window.dimensions` signal works exactly the same way, automatically changing
whenever the window is resized ([play][dimensions]).

  [dimensions]: /edit/examples/Reactive/ResizePaint.elm "dimensions"

The third example uses the `count` function which counts the number of times a
signal is updated. In this case we are counting mouse clicks.

|]

what2 = [markdown|

In the fourth example we square the number of clicks. The `lift` function is used
to &ldquo;lift&rdquo; a normal function onto a signal. Once lifted, a function is applied
automatically whenever the signal updates. So if the function produces static graphics,
[lifting it onto a signal produces an animation][example]!

  [example]: /edit/examples/Intermediate/Clock.elm "animation"

These examples are just the basics of FRP. There are tons of other
[interactive examples](/Examples.elm) that allow you to play around
with FRP in [Elm](/). More information on how to use signals can be found
[here](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Signal).

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
is 75% of the way across the screen.

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

Functional reactive programming (FRP) is a *declarative* approach to GUI design.
The term *declarative* makes a distinction between the &ldquo;what&rdquo; and
the &ldquo;how&rdquo; of programming. A declarative language allows you to say
*what* is displayed, without having to specify exactly *how* the computer should
do it.

Because FRP gives us a high-level way to react to events, there is no need for
messy event handlers, callbacks, or manual DOM manipulations. This cleans up
code dramatically. The operational stuff largely disappears, often leaving you
exclusively with code that is essential to your task. No non-essential details!

The best evidence is code though, so take a look at [more
examples](/Examples.elm)!

|]


---- Putting together the examples and making them pretty ----

entry w1 w2 v1 v2 = container w1 40 middle v1 `beside` container w2 40 middle v2
example w1 w2 code =
    lift (\info -> entry w1 w2 (leftAligned . monospace <| toText code) (asText info))

clickCount = count Mouse.clicks

examples1 =
  let title = leftAligned . bold . toText
      example' = example 250 110
  in [ constant (entry 250 110 (title "Source Code") (title "Value"))
     , example' "Mouse.position" Mouse.position
     , example' "Window.dimensions" Window.dimensions
     , example' "clks = count Mouse.clicks" clickCount
     , example' "lift (\\n -> n^2) clks" (lift (\n -> n^2) clickCount)
     ]

examples2 =
  let title = leftAligned . bold . toText
      example' = example 420 140
  in [ constant (entry 420 140 (title "Source Code") (title "Value"))
     , example' "lift2 (/) Mouse.x Window.width" (lift2 (\a b -> toFloat (round (1000 * toFloat a / toFloat b)) / 1000) Mouse.x Window.width)
     , example' "sampleOn Mouse.clicks Mouse.position" (sampleOn Mouse.clicks Mouse.position)
     , example' "keepWhen Mouse.isDown (0,0) Mouse.position" (keepWhen Mouse.isDown (0,0) Mouse.position)
     ]

box exs =
  let putInBox exs =
        let eBox  = color white <| flow down exs
        in  color C.accent1 <|
            container (widthOf eBox + 4) (heightOf eBox + 4) middle eBox
  in  lift putInBox <| combine exs



---- Putting it all together into a single page ----

whatIsFRP exs w =
  let hw = w `div` 2 - 15 in
  flow down
    [ [markdown|# What is Functional Reactive Programming? |]
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
