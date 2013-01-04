import Website.Docs (createDocs2)

ops =
  [ ("(<~)", "(a -> b) -> Signal a -> Signal b", [markdown|
An alias for `lift`. A prettier way to apply a
function to the current value of a signal.|])
  , ("(~)", "Signal (a -> b) -> Signal a -> Signal b", [markdown|
Signal application. This takes two signals, holding a function and
a value. It applies the current function to the current value.

So the following expressions are equivalent:

    scene <~ Mouse.x ~ Mouse.y
    lift2 scene Mouse.x Mouse.y|])
  ]

lifts =
  [ ("constant", "a -> Signal a", [markdown|
Create a constant signal that never changes.|])
  , ("lift", "(a -> b) -> Signal a -> Signal b", [markdown|
Transform a signal with a given function.|])
  , ("lift2", "(a -> b -> c) -> Signal a -> Signal b -> Signal c", [markdown|
Combine two signals with a given function.|])
  , ("lift3", "(a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d", [markdown|
Combine three signals with a given function.|])
  , ("merge", "Signal a -> Signal a -> Signal a", [markdown|
Merge two signals into one, biased towards the first signal if both signals
update at the same time.|])
  , ("merges", "[Signal a] -> Signal a", [markdown|
Merge many signals into one, biased towards the left-most signal if multiple
signals update simultaneously.|])
  ]

folds =
  [ ("foldp", "(a -> b -> b) -> b -> Signal a -> Signal b", [markdown|
Create a past-dependent signal. Each value given on the input signal will
be accumulated, producing a new output value.

For instance,
`(foldp (\\t acc -> acc + 1) 0 (Time.every second))` increments every second.|])
  , ("count", "Signal a -> Signal Int", [markdown|
Count the number of events that have occured.|])
  , ("countIf", "(a -> Bool) -> Signal a -> Signal Int", [markdown|
Count the number of events that have occured that satisfy a given predicate.|])
  , ("average", "Int -> Signal Number -> Signal Float", [markdown|
Takes an integer `n` and a signal of numbers. Computes the running
average of the signal over the last `n` events.

So `(average 20 (fps 40))` would be the average time between the frames for
the last 20 frames.|])
  , ("foldp1", "(a -> a -> a) -> Signal a -> Signal a", [markdown|
Create a past-dependent signal. The first value on the signal is used
as the base case.|])
  , ("foldp'", "(a -> b -> b) -> (a -> b) -> Signal a -> Signal b", [markdown|
Just like foldp, but instead of a base case, you provide a function to be
applied to the first value, creating the base case.|])
  ]

filters =
  [ ("keepIf", "(a -> Bool) -> a -> Signal a -> Signal a", [markdown|
Keep only events that satisfy the given predicate. Elm does not allow
undefined signals, so a base case must be provided in case the predicate is
never satisfied.|])
  , ("dropIf", "(a -> Bool) -> a -> Signal a -> Signal a", [markdown|
Drop events that satisfy the given predicate. Elm does not allow undefined
signals, so a base case must be provided in case the predicate is never
satisfied.|])
  , ("keepWhen", "Signal Bool -> a -> Signal a -> Signal a", [markdown|
Keep events only when the first signal is true. When the first signal becomes
true, the most recent value of the second signal will be propagated. Until
the first signal becomes false again, all events will be propagated. Elm does
not allow undefined signals, so a base case must be provided in case the first
signal is never true.|])
  , ("dropWhen", "Signal Bool -> a -> Signal a -> Signal a", [markdown|
Drop events when the first signal is true. When the first signal becomes false,
the most recent value of the second signal will be propagated. Until the first
signal becomes true again, all events will be propagated. Elm does not allow
undefined signals, so a base case must be provided in case the first signal is
always true.|])
  , ("dropRepeats", "Signal a -> Signal a", [markdown|
Drop sequential repeated values. For example, if a signal produces the
sequence `[1,1,2,2,1]`, it becomes `[1,2,1]` by dropping the values that
are the same as the previous value.|])
  , ("sampleOn", "Signal a -> Signal b -> Signal b", [markdown|
Sample from the second input every time an event occurs on the first input.
For example, `(sampleOn clicks (every second))` will give the approximate
time of the latest click.|])
  ]

moreLifts =
  [ ("lift4"
    ,"(a -> b -> c -> d -> e)\n      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e"
    , spacer 0 0)
  , ("lift5"
    ,"(a -> b -> c -> d -> e -> f)\n      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f"
    , spacer 0 0)
  , ("lift6"
    ,"(a -> b -> c -> d -> e -> f -> g)\n      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g"
    , spacer 0 0)
  , ("lift7"
    ,"(a -> b -> c -> d -> e -> f -> g -> h)\n      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h"
    , spacer 0 0)
  , ("lift8"
    ,"(a -> b -> c -> d -> e -> f -> g -> h -> i)\n      -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f -> Signal g -> Signal h -> Signal i"
    , spacer 0 0)
  ]

categories = 
  [ ("Combine", lifts)
  , ("Pretty Combine", ops)
  , ("Past-Dependence", folds)
  , ("Filters", filters)
  , ("Do you even lift?", moreLifts)
  ]

intro = [markdown|
The library for general signal manipulation. Some useful functions for
working with time (e.g. setting FPS) and combining signals and time (e.g.
delaying updates, getting timestamps) can be found in the
[`Time`](/docs/Signal/Time.elm) library.

Note: There are lift functions up to `lift8`.
|]

main = createDocs2 "Signal" intro categories
