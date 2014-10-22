import Website.Skeleton (skeleton)
import Window

port title : String
port title = "Using Signals"

main = skeleton "Learn" (content << min 600) <~ Window.dimensions


content w = width w [markdown|

# Using Signals

Signals are values that change over time. You can learn more about the basics
of signals in [this post][frp] and in [the examples](/Examples.elm). This post
focuses on common design patterns that you will definitely see when programming
with signals in Elm.

[frp]: /learn/What-is-FRP.elm

## The Typical Pattern

When writing Elm code, it is usually best to use signals as little as possible.
They help you handle inputs from the world and manage state, but when it comes
to writing nice modular code, you should primarily use normal functions and
values.

Okay, but what about the part that *does* use signals?

The state of your application will live primarily in a single [`foldp`][foldp].
This `foldp` will take in some signal of &ldquo;inputs&rdquo; that indicate
how your application state should be updated.

[foldp]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Signal#foldp

Say those inputs include `Mouse.clicks` and time deltas. We need to put these
signals together in a way that will update our `foldp` correctly. It may seem
tempting to write something like this:

```haskell
inputs : Signal ((), Float)
inputs =
    lift2 (,) Mouse.clicks (fps 40)
```

The `inputs` signal will now update whenever `Mouse.clicks` or `(fps 40)`
update. That means we cannot know who triggered the update. Should I react to a
click or a time delta?!

Instead you want to model the kinds of updates that can be done and
[`merge`][merge] them all together.

[merge]: http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Signal#merge

```haskell
type Update = Click | TimeDelta Float

inputs : Signal Update
inputs =
    merge
        (lift (always Click) Mouse.clicks)
        (lift TimeDelta (fps 40))
```

When writing large applications, use the techniques described
[here](/learn/Architecture.elm) to make this basic approach modular as your
codebase grows.

|]


