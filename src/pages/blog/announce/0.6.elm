import Blog
import Center


main =
  Blog.blog
    "Elm 0.6"
    "Time, Dates, and Syntax"
    Blog.evan
    (Blog.Date 2012 12 8)
    [ Center.markdown "600px" content ]


content = """

This release makes Elm much prettier and much better for working with time.
With these additions, I think its easier than ever to create complex
interactions and animations.

The most obvious changes in [Elm](/) 0.6 are whitespace sensitivity and the
addition of many useful time signals such as `(fps : Number -> Signal Time)`
which make it much easier to make rich animations that work on many devices.

This release also includes a [`Date` library][date], supports
[HSV colors][hsv] in the [`Color` library][color], and much more.
I highly encourage you to at least read on to learn about the new time
library because it brings a number of fundamentally new capabilities to Elm.
There is also some cool news at the end!

  [date]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Date "Date library"
  [hsv]: http://en.wikipedia.org/wiki/HSL_and_HSV "HSV Colors"
  [color]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Color "Color library"

These changes allowed a big rewrite of the [Pong in Elm][pong]
[source code][code], making things generally much nicer. I also wrote a
examples to illustrate the benefits of Elm 0.6 in this post and in two
larger examples ([sliding circle][slide], [color wheel][wheel]).

  [pong]:  /blog/making-pong
  [code]:  /examples/pong
  [slide]: /edit/examples/Intermediate/Slide.elm "Sliding Circle"
  [wheel]: /edit/examples/Intermediate/ColorWheel.elm "Color Wheel"

**The rest of this post is out of order for now. Sorry for the inconvenience!**

"""

{--

## Syntax

Elm&rsquo;s use of curly braces and semi-colons added quite a bit of visual
noise to let- and case-expressions. Elm now supports whitespace sensitivity,
so you can leave out the `{;;;}` as long as you indent each term uniformly.
"""

guards = Markdown.toElement """
The new guarded definitions are a great way to avoid ugly nested if-expressions.
Say you want to use the up (40) and down (38) arrow keys to control a value.
"""

infixes = Markdown.toElement """
You can also define custom infix operators. Say you want to work with vectors:
"""

time = Markdown.toElement """
For now, all user defined infix
operators are left-associative and have the highest precedence, meaning that
they bind tighter than any predefined operators. Furthermore, you cannot
override predefined infix operators right now.

## Time

The time library has been completely rewritten with a much stronger focus on
creating highly interactive animations. Before we get to the cool stuff, there
are some basic changes to discuss.

The new Time library is based on milliseconds. This is a departure from past
versions of Elm, and will be a breaking change for any uses of `every`. A quick
fix is to define:

        every' t = lift inSeconds << every (t * seconds)

And replace any use of `every` with the new `every'`.
To help make the change to milliseconds more natural in new code, the library
provides the values:

        ms, second, minute, hour : Time

so you can express half a second as `(second / 2)` and 45 minutes
as `(45 * minute)` or `(0.75 * hour)`.

Now for the cool stuff: mixing time and signals. These additions fall into
three major groups: timestamping signals, frames-per-second (FPS), and
delaying signals.

#### Timestamps

The following functions simply provide a way to add timestamps to any signal.

        timestamp : Signal a -> Signal (Time,a)
        timeOf    : Signal a -> Signal Time

Function `timestamp` adds a timestamp whereas `timeOf` just gives the time of
the update, dropping the value entirely. This is a low-level way to explore
the time relationships between updates.

For example, we can now discover the time between clicks:
"""

time1 = Markdown.toElement """
#### Frames per Second

The following FPS functions provide a way to request an upper bound on frame
rates that will gracefully degrade on less powerful devices.

        fps     : Number -> Signal Time
        fpsWhen : Number -> Signal Bool -> Signal Time

So the expression `(fps 40)` will create a signal that attempts to update 40
times per second. The value of the signal is always the time delta between
the current and previous update. If you'd prefer to have the precise time of
each update instead, its as easy as saying `(timeOf (fps 40))`.

The function `fpsWhen` is just like `fps` but you can turn it on and off!
This allows you to do updates only when absolutely necessary, saving cycles
and battery life. Function `fpsWhen` works best when you want to react to
user input with animation, such as reacting to sustained mouse presses:
"""

time2 = Markdown.toElement """
If you have ever read through the [Pong in Elm walkthrough][pong], you
probably noticed that getting flexible frame rates was kind of a pain,
requiring 10 lines of Elm and a helper JS file. That can all be accomplished
in one line now. If you take a look at [the updated code for Pong][code],
you'll see that the syntax upgrades and `fps` function have made it
significantly clearer and more concise.

  [pong]: /blog/games-in-elm/part-0/Making-Pong.html "Pong in Elm"
  [code]: https://github.com/elm-lang/Elm/tree/master/Examples/elm-js/Pong "Source for Pong"

#### Delays

Finally, we have a function for delaying signals by a given amount of time:

        delay : Time -> Signal a -> Signal a

The `delay` function makes it possible to ask questions like, &ldquo;has this
signal changed in the last second?&rdquo;

        isStable signal = let tstamp = timeOf signal in
                          lift2 (==) tstamp (delay second tstamp)

This could be useful if you want to inhibit a signal if it is likely to change
very quickly. For instance, you could only send HTTP requests if the user input
has been stable for half a second. Or you could only use mouse values if the
mouse has been stable for 10 milliseconds.

The Time library also includes some helpers based on the `delay` function:

        since : Time -> Signal a -> Signal Bool
        after : Time -> Signal a -> Signal Bool

These let you ask &ldquo;is it one second after the last mouse click?&rdquo;

        (second `after` Mouse.clicks)

These functions are powerful when paired with `fpsWhen`. They allow you to do
brief bursts of animation based on user input. Say you want to animate something
for one second following each mouse click. You can say:

        deltas = 40 `fpsWhen` (second `since` Mouse.clicks)

Function `since` takes a time `t` and a signal, producing a signal that is
true if it has been less than `t` since the latest update. Therefore `deltas`
will only cause updates when it is necessary.
"""

rest = Markdown.toElement """
For a more exciting example of brief animations, see [here](/edit/examples/Intermediate/Slide.elm).
This example can provide the basis for many slick animations.

You can see the entire `Time` library [here].

  [here]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Time "Time Library"

## HSV Colors

After doing some graphics programming you are probably familiar with [RGB]
colors, but there are many interesting ways to specify color. Elm now supports
[Hue-Saturation-Value (HSV) colors][hsv] which make it easier to compute
color complements, triads, analogous colors, etc. [This example][cmp] uses
`hsv` to illustrate complementary colors.

  [cmp]: /edit/examples/Intermediate/Complements.elm "Complementary Colors"

The color is actually given by a degree between 0&deg; and 360&deg;, starting
at red and cycling back around to red. This also makes it easy to do subtle
color variations which can help draw attention to something.

For more information on using HSV colors see the
[Color library documentation][lib] and the [Wikipedia page on HSV colors][hsv].

  [rgb]: http://en.wikipedia.org/wiki/RGB_color_model "RGB colors"
  [lib]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Color "Color Library"
  [hsv]: http://en.wikipedia.org/wiki/HSL_and_HSV "HSV colors"

## Even more Signals

In addition to the new `Time` signals, there are a couple more new and useful signal functions. Again, providing fundamentally new capabilities is the `merge` function which combines two different signals into one, always taking the latest value.

        merge  : Signal a -> Signal a -> Signal a
        merges : [Signal a] -> Signal a

Both `merge` and `merges` are left-biased. Expressions such as `(merge Mouse.x Mouse.y)` result in simultaneous updates for both input signals, so the left-most signal takes precedence. Therefore, `(merge Mouse.x Mouse.y)` is equivalent to `Mouse.x`.

This release also includes the `average` function, which makes it easier to assess the average frame rates you are getting for an animation.

        average : Int -> Signal Number -> Signal Float

Function `average` takes a sample size `n` and a signal `s`. It uses the `n` most recent updates to `s` to get a snapshot of the average value of `s`. If you wanted to get the frame rate of a signal called `deltas` over the last 40 frames, it would look like this:

        frameRate = lift (\\t -> second / t) (average 40 deltas)

## Dates

The Date library provides a basic way to work with locale specific dates. I am new to internationalization, so I expect that someone somewhere will have a problem. Just let me know if you do!

The coolest function here is probably `Date.read` which attempts to read an arbitrary string as a date:
"""

(dateInput, dateString) = Input.textfield "Date"

maybeDate w str =
  let msg = Graphics.height 50 (case Date.read str of
                                  Just d  -> text << Text.color accent4 << toText <| show d
                                  Nothing -> text << Text.color accent3 << toText <| "Invalid Date Input")
  in  container w 50 middle dateInput `above` container w 50 middle msg

date2 = Markdown.toElement """
It is also quite easy to look up the `dayOfWeek` for a particular date. The `Day` data type then makes it fairly easy to provide day-of-week related logic:

        data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

I suggest using this to verify the claims of potential savants. See [these docs][date] for information on the rest of the Date library.

  [date]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Date "Date Library"

## Miscellaneous Improvements

- Ensure that `fittedImage` works with animated GIFs.

- Fix rendering bug on IE9.

- End namespace pollution by the Elm RTS.
  Previously, some Elm functions would leak out into the global JavaScript
  namespace.

- Type checking is a bit faster.

- Make it possible to compare tuples if they contain comparable things.

- Fix imports of variables that have a prime, such as `(init')`.

- Fix `middleAt` rendering.

- Make Elm compatable with current version of Yesod.

## Other News

My talk from the Emerging languages camp at StrangeLoop was recently posted
[online at InfoQ](http://www.infoq.com/presentations/Elm).

Elm was featured on O&lsquo;Reilly&lsquo;s [emerging languages spotlight][spot].

  [spot]: http://radar.oreilly.com/2012/12/emerging-languages-spotlight-elm.html "Spotlight"

Grzegorz has posted a number of examples on the syntax of Elm and some
larger examples of Elm in action in [his site][blog]. Thank you Grzegorz!

 [blog]: http://www.grzegorzbalcerek.net/elm/index.html "Grzegorz's Examples"

Elm just got it&rsquo;s 200<sup>th</sup> star on [GitHub](https://github.com/evancz)!
"""

spiralCode = Markdown.toElement """
    deltas = 30 `fpsWhen` Mouse.isDown
    main = lift spiral (foldp (+) 0 deltas)
"""

scene speed t1 t3 str w' =
  let sprl = spiral t1
      sprl3 = spiral t3
      w = min 600 w'
  in  flow down
       [ width w intro
       , sideBySide w beforeLet afterLet
       , sideBySide w beforeCase afterCase
       , width w guards
       , sideBySide w beforeIf afterIf
       , width w infixes
       , sideBySide w beforeInfix afterInfix
       , width w time
       , container w (heightOf speed) middle speed
       , width w time1
       , flow right
          [ container (w - widthOf sprl - 100) (heightOf sprl) middle spiralCode
          , container (widthOf sprl + 100) (heightOf sprl) middle sprl ]
       , width w time2
       , container w (heightOf sprl3) middle sprl3
       , width w rest
       , maybeDate w str
       , width w date2
       ]

beforeIf = Markdown.toElement """
    toDirection k y =
        if k == 40 then y+1 else
        if k == 38 then y-1 else y
"""

afterIf = Markdown.toElement """
    toDirection k y
        | k == 40   = y + 1
        | k == 38   = y - 1
        | otherwise = y
"""

beforeLet = Markdown.toElement """
    let { xs = ...
        ; ys = ...
        }
    in  zipWith max xs ys
"""

afterLet = Markdown.toElement """
    let xs = ...
        ys = ...
    in  zipWith max xs ys
"""
beforeCase = Markdown.toElement """
    case httpResponse of
      { Success s   -> ...
      ; Waiting     -> ...
      ; Failure _ _ -> ...
      }
"""
afterCase = Markdown.toElement """
    case httpResponse of
        Success s   -> ...
        Waiting     -> ...
        Failure _ _ -> ...
"""
beforeInfix = Markdown.toElement """
    vplus (x,y) (w,z) =
        (w + x, y + z)

    v = a `vplus` b `vplus` c
"""
afterInfix = Markdown.toElement """
    (x,y) -+- (w,z) =
        (w + x, y + z)

    v = a -+- b -+- c
"""

spiral time =
  let a = inSeconds time / 2
      f n = ( n/2 * cos (n/3)
            , n/2 * sin (n/3) )
      spiral = line <| map f [ 3 .. 100 ]
      clr = hsv (round (inSeconds time * 30) `mod` 360) 1 1
  in  collage 100 100 [ move (50,50) << rotate a <| traced (solid clr) spiral ]

times1 = foldp (+) 0 <| 30 `fpsWhen` Mouse.isDown

clickSpeed minDelta =
 flow down << map (width 300) <|
      [ centeredText << Text.color accent1 << toText <| "Speed Record"
      , centeredText << Text.color accent4 << Text.height 3 << bold << toText <| show minDelta
      , centeredText << Text.color accent1 << toText <| "milliseconds"
      ]

diffs s = lift snd <| foldp (\t (t0,d) -> (t,t-t0)) (0,0) s
speed = lift clickSpeed << foldp min 5000 << diffs <| timeOf Mouse.clicks


times3 = foldp (+) 0 (30 `fpsWhen` (second `since` Mouse.clicks))

sideBySide wid e1 e2 =
  let w = wid `div` 2
      h = max (heightOf e1) (heightOf e2)
      arrow = text << Text.height 3 << Text.color accent1 << toText <| "&rarr;"
  in  layers [ container wid h middle arrow
             , flow right [ container w h middle e1
                          , container w h middle e2
                          ]
             ]

main = lift2 skeleton (lift4 scene speed times1 times3 dateString) Window.dimensions

--}
