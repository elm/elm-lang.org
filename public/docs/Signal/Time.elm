
import Website.Docs (createDocs2)

times =
  [ ("hour, minute, second, ms", "Time", [markdown|
Units of time, making it easier to specify things like a half-second `(second / 2)`.|])
  ]

ticker =
  [ ("fps", "Number -> Signal Time", [markdown|
Takes desired number of frames per second (fps). The resulting signal
gives a sequence of time deltas as quickly as possible until it reaches
the desired FPS. A time delta is the time between the last frame and
the current frame.|])
  , ("fpsWhen", "Number -> Signal Bool -> Signal Time", [markdown|
Same as the fps function, but you can turn it on and off. Allows
you to do brief animations based on user input without major ineffeciencies.
The first time delta after a pause is always zero, no matter how long the pause was.
This way summing the deltas will actually give the amount of time that the output
signal has been running.|])
  , ("every", "Time -> Signal Time", [markdown|
Takes a time interval `t`. The resulting signal is the current time, updated every `t`.|])
  ]

other =
  [ ("delay", "Time -> Signal a -> Signal a", [markdown|
Delay a signal by a certain amount of time. So `(delay second Mouse.clicks)`
will update one second later than any mouse click.|])
  , ("since", "Time -> Signal a -> Signal Bool", [markdown|
Takes a time `t` and any signal. The resulting boolean signal
is true for time `t` after every event on the input signal.
So ``(second `since` Mouse.clicks)`` would result in a signal
that is true for one second after each mouse click and false
otherwise.|])
  , ("timestamp", "Signal a -> Signal (Time,a)", [markdown|
Add a timestamp to any signal. Timestamps increase monotonically. Each timestamp is
related to a specfic event, so `Mouse.x` and `Mouse.y` will always have the same
timestamp because they both rely on the same underlying event.|])
  , ("timeOf", "Signal a -> Signal Time", [markdown|
Same as `timestamp` but it throws out the incoming value. So `(timeOf == lift fst . timestamp)`.|])
  ]

conversions =
  [ ("inHours, inMinutes, inSeconds, inMss", "Time -> Float", [markdown|
Unit conversions for times. `(inHours (30 * minute) == 0.5)`|])
  , ("toDate", "Time -> Date", [markdown|
Convert a Time to a Date, as specified in the [Date](/docs/Date.elm) library.|])
  , ("read", "String -> Maybe Time", [markdown|
Read a time in from a string. Follows the same rules as JavaScript&rsquo;s `Date.parse` function.
|])
  ]


categories = [ ("Times", times)
             , ("Tickers", ticker)
             , ("Time + Signals", other)
             , ("Conversions", conversions) ]

intro = [markdown|
Library for working with time. The `Time` data type represents some number of
milliseconds.
|]

main = createDocs2 "Time" intro categories