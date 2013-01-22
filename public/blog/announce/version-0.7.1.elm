
import Website.Skeleton
import Website.ColorScheme

title = constant (JavaScript.castStringToJSString "Elm 0.7.1 - Keyboard and Touch")
foreign export jsevent "elm_title"
  title :: Signal JSString

intro = [markdown|

<style type="text/css">
p { text-align: justify }
h2,h3,h4 { padding-top: 0.5em; }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
</style>

# Keyboard and Touch Signals &ndash; Elm 0.7.1

This is an incremental release that:

- introduces the `Keyboard`, `Touch`, and `Either` libraries
- fixes a couple bizarre bugs
- cleans up some libraries to get names more consistent and easier to remember

## Keyboard

## Touch

The `Touch` library has only two signals at the moment: `touches` and `taps`.
These provide the low-level building blocks for detecting gestures. We will
discuss some ideas for a higher-level API for defining gestures in a moment!

The `touches` signal is a list of all of the ongoing touches. A touch is:

* an `id` that differentiates one touch from another
* the current coordinates, `x` and `y`
* the starting coordinates, `x0` and `y0`
* the starting time `t0`

So the type looks like this:

    touches :: Signal [{ x  :: Int, y  :: Int, id :: Int
                       , x0 :: Int, y0 :: Int, t0 :: Int }]

This information should be sufficient for building up any kind of gesture.
The question now is: how do we best represent a gesture in a functional way?
Some thoughts so far are (1) using a state machine or (2) defining gestures
as a function of position and time. Option 1 seems fairly straight forward
conceptually, but I am not sure exactly what a general API would look like
yet. Option 2 could potentially be a very beautiful solution in some cases,
but it may also just force people to translate their state machines into
mathematics. There may be some other option out there, so keep these ideas
in mind when using `touches` and let [the list] know what you learn!

The `Touch` library also includes a `taps` signal which just gives the
coordinates of the latest tap. The default value is the origin.

    taps    :: Signal { x :: Int, y :: Int }

## Either

    fromMaybe :: a -> Maybe a -> a
    fromMaybe d m = maybe d id m

    map :: (a -> b) -> Maybe a -> Maybe b
    map f m = maybe Nothing (Just . f) m

|]

scene w = width w intro

main = lift (skeleton (scene . min 600)) Window.width

