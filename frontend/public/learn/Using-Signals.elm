import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Using Signals"


main : Signal Element
main =
  skeleton "Learn" (\w -> width (min 600 w) content) <~ Window.dimensions


content : Element
content = Markdown.toElement """

# Using Signals

Signals are values that change over time. You can learn more about the basics
of signals in [this post][frp] and in [the examples](/Examples.elm). This post
first goes through the most important functions for using signals, then on
useful patterns you will use in every Elm program, and finally discuss some
common pitfalls and how to get out of them.

[frp]: /learn/What-is-FRP.elm


## Inputs

Every Elm program starts with an input. Something like [`Mouse.position`][pos]
or [`Window.dimensions`][dim] that gives you information about the world and
what your users might be up to.

[pos]: http://package.elm-lang.org/packages/elm-lang/core/latest/Mouse#position
[dim]: http://package.elm-lang.org/packages/elm-lang/core/latest/Window#dimensions

```haskell
Mouse.position : Signal (Int,Int)
Window.dimensions : Signal (Int,Int)
```

These values will change as the mouse moves or as the browser window resizes.
Inputs like these will be the starting point for updates in every Elm program.


## Transforming Signals

One of the most important things you can do with a signal is transform it into
something else. We use the `map` function for this.

```haskell
map : (a -> b) -> Signal a -> Signal b
```

As an example, say we have the `Window.dimensions` signal and want to get the
aspect ratio of the users window.

```haskell
toAspectRatio : (Int,Int) -> Float
toAspectRatio (w,h) =
    toFloat w / toFloat h

aspectRatio : Signal Float
aspectRatio =
    map toAspectRatio Window.dimensions
```

Now every pair of dimensions is turned into an aspect ratio. As the window
resizes, `aspectRatio` is updated automatically.


## Merging Signals

It is often useful to put multiple signals together. You can use `merge` or
`map2`, each with slightly different results. Lets look at `merge` first.

```haskell
merge : Signal a -> Signal a -> Signal a
```

This function takes two signals and merges them into one. Whenever an incoming
signal updates, the outgoing signal updates to that value. If both incoming
signals update, the left one wins the race. It is common to want to merge
signals that do not have the same type though. In that case you want to use
a [union type](/learn/Union-Types.elm) like this:

```haskell
type Update = Move (Int,Int) | TimeDelta Float

updates : Signal Update
updates =
    merge (map Move Mouse.position) (map TimeDelta (fps 30))
```

Now we have a signal of mouse movements and time deltas. Whenever one of those
incoming signals updates, the outgoing signal does too.

The other common way to merge signals is with `map2` which works a little bit
differently.

```haskell
map2 : (a -> b -> c) -> Signal a -> Signal b -> Signal c
```

This method of merging uses a function to put the two incoming signals together.
The outgoing signal is the result of this function. Whenever one of the
incoming signals updates, we grab the latest values from both and compute the
new value for the outgoing signal. This means you cannot tell which signal is
responsible for the updating. A common mistake is to use `map2` instead of
`merge`, so keep this distinction in mind!


## State

One of the most important uses of signals is to hold state. We do this with a
function called `foldp` which is short for &ldquo;fold from the past&rdquo;.

```haskell
foldp : (a -> state -> state) -> state -> Signal a -> Signal state
```

It takes an update function, a starting state, and a signal that will drive the
state updates. The result is a signal representing the latest state. Here is an
example usage that lets us count mouse clicks.

```haskell
clickCount : Signal Int
clickCount =
    foldp (\\click count -> count + 1) 0 Mouse.clicks
```

So we gave three arguments: a way to increment the counter, an initial count of
zero, and the `Mouse.clicks` signal. Whenever a mouse click happens, we update
our count with the function we provided.

You will see `foldp` in pretty much all non-trivial Elm programs.


## Filtering Signals

Sometimes it is useful to filter certain updates, though it does not come up
super frequently. For example, lets say I want to have a signal that represents
dragging the mouse. I could use [`keepWhen`][keepWhen] for this:

[keepWhen]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal#keepWhen

```haskell
keepWhen : Signal a -> b -> Signal b -> Signal b

drags : Signal (Int,Int)
drags =
    keepWhen Mouse.isDown (0,0) Mouse.position
```

Essentially we are saying, only use updates from the `Mouse.position` signal
when `Mouse.isDown` is true. If it is false, just drop all the updates. Besides
these two signals, we also give `keepWhen` an initial value. A core aspect of
signals is that they are always defined, but what if `Mouse.isDown` never
becomes true? We need to give `drags` some value, but we may not be allowed to
take it from `Mouse.position` so we use a default value until `Mouse.isDown`
becomes true.

There are a bunch of other filtering functions like `dropRepeats` or `keepIf`
that work in similar ways. Again, these signal functions are quite a bit more
rare than the others, but can still come in handy sometimes.


## The Typical Pattern

When writing Elm code, it is usually best to use signals as little as possible.
They help you handle inputs from the world and manage state, but when it comes
to writing nice modular code, you should primarily use normal functions and
values.

Okay, but what about the part that *does* use signals?

The state of your application will live primarily in a single [`foldp`][foldp].
This `foldp` will take in some signal of &ldquo;inputs&rdquo; that indicate
how your application state should be updated.

[foldp]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal#foldp

Say those inputs include `Mouse.clicks` and time deltas. We need to put these
signals together in a way that will update our `foldp` correctly. It may seem
tempting to write something like this:

```haskell
inputs : Signal ((), Float)
inputs =
    map2 (,) Mouse.clicks (fps 40)
```

The `inputs` signal will now update whenever `Mouse.clicks` or `(fps 40)`
update. That means we cannot know who triggered the update. Should I react to a
click or a time delta?!

Instead you want to model the kinds of updates that can be done and
[`merge`][merge] them all together.

[merge]: http://package.elm-lang.org/packages/elm-lang/core/latest/Signal#merge

```haskell
type Update = Click | TimeDelta Float

inputs : Signal Update
inputs =
    merge
        (map (always Click) Mouse.clicks)
        (map TimeDelta (fps 40))
```

When writing large applications, use the techniques described
[here](/learn/Architecture.elm) to make this basic approach modular as your
codebase grows.


## Common Pitfalls

A common way to get stuck using signals is to try to use them too much. It is
tempting to try to do everything with signals, but it is usually best to write
as much code as possible without them.

In practice this means looking at your code and figuring out how to
&ldquo;move signals up a level&rdquo;. If you find yourself in a situation
where you think you want a list of signals, how can you change your code such
that you end up with a signal of lists?

```haskell
List (Signal a) -> Signal (List a)
```

You will have an easier time with a signal of lists because all of the `Signal`
functions focus on working with exactly this kind of value.


"""


