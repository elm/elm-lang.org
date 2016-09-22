import Blog
import Center


main =
  Blog.blog
    "Elm 0.7.1"
    "Library Cultivation"
    Blog.evan
    (Blog.Date 2013 1 25)
    [ Center.markdown "600px" content ]


content = """

This release is partly the result of exploring what is possible with
the recently added [extensible records][recs] and partly an effort to
make Elm a better choice for mobile devices. This version:

- introduces the [`Keyboard`][keys], [`Touch`][touch], and [`Either`][either] libraries
- make names more consistent and easier to remember accross libraries
- fixes a couple bizarre bugs

Touch is a brand new addition to Elm, and as far as I know, brand
new to FRP. I am excited to see all the creative (and hopefully elegant)
things will come from it. My favorite use so far is [a simple drawing app][draw]
that takes only 5 lines! (obvious warning: requires a touch screen!)

Before we get started, there is some non-compiler news:

* Grzegorz recently released [Preselm](https://github.com/grzegorzbalcerek/Preselm),
  an open source project for creating presentations.
  In addition to being really cool, it is a great example of how to effectively use
  [the new record system](/blog/announce/0.7).

* Mads is currently working on getting inline documentation working in the online editor.
  I think it is a big step towards making Elm really easy to learn for people from
  all backgrounds, and you can [try it out](https://groups.google.com/forum/?fromgroups=#!topic/elm-discuss/_xmbeVfjYbI)
  fairly easily or [watch the demo](https://www.youtube.com/watch?v=vfWDvIbt4YY&hd=1).
  Your constructive feedback would be greatly appreciated!

Okay, now let&rsquo;s dive into Elm 0.7.1!

 [recs]: /blog/announce/0.7
 [draw]: /edit/examples/Reactive/Draw.elm

## Keyboard

[The new `Keyboard` library][keys] introduces a couple simple and useful signals.
First there are the directional signals:

```elm
arrows, wasd : Signal { x : Int, y : Int }
```

These allow you to easily handle input from the arrow keys or the [wasd keys][wasd].
It turns out there is a [long and crazy history of arrow configurations][arrows],
so please let me know if you think there should be more choices.

There are also a few signals for common modifier keys:

```elm
ctrl, shift, space : Signal Bool
```

The existing `Keyboard.Raw` library provides some lower-level signals that
allow you to work with arbitrary keyboard input, but that is often too more
powerful than you really need.

These signals came in handy when creating a turtle that swims around and surfaces,
based on the `arrows` and `space` signals.

<div class="intrinsic-container">
  <iframe
    src="https://www.youtube.com/embed/Xshzzzbw3KY?rel=0;showinfo=0"
    allowfullscreen></iframe>
</div>

This takes only 20 physical lines of code, which you can see and modify
in the interactive editor [here][src].

 [keys]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Keyboard "Keyboard Library"
 [arrows]: http://en.wikipedia.org/wiki/Arrow_keys "alternative cursor movement keys"
 [wasd]: http://en.wikipedia.org/wiki/Arrow_keys#WASD_keys "wasd"
 [src]: /edit/examples/Intermediate/Turtle.elm "Turtle Source"

## Touch

[The `Touch` library][touch] has only two signals at the moment: `touches` and `taps`.
These provide the low-level building blocks for detecting gestures. We will
discuss some ideas for a higher-level API for defining gestures in a moment.

 [touch]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Touch "Touch Library"

Warning: I do not have any Microsoft touch devices to test on, so I am not
sure how well this will work there. Please let me know if you have any problems.

The `touches` signal is a list of all of the ongoing touches. A touch is:

* an `id` that differentiates one touch from another
* the current coordinates, `x` and `y`
* the starting coordinates, `x0` and `y0`
* the starting time `t0`

The reason for including so much information is to make it easier to define
more complicated gestures. The type looks like this:

```elm
touches : Signal [{ x:Int, y:Int, id:Int, x0:Int, y0:Int, t0:Int }]
```

The `Touch` library also includes a `taps` signal which just gives the
coordinates of the latest tap. The default value is the origin.

```elm
taps : Signal { x : Int, y : Int }
```

You can find examples for these signals [here][t1], [here][t2], [here][t3],
and [here][t4]. Warning: you need an Android or iOS touch device for these!
The second of those examples shows how to force a mobile device to correctly
report its dimensions.

 [t1]: /edit/examples/Reactive/Touches.elm "Touches"
 [t2]: /edit/examples/Reactive/Touch.elm "Touch"
 [t3]: /edit/examples/Reactive/Taps.elm "Taps"
 [t4]: /edit/examples/Reactive/Draw.elm "Draw"

&ldquo;Why no complex gestures?!&rdquo; you may be wondering. If you take close
look at an iOS or Android device, you will notice that nearly every swipe behaves
slightly differently. For example, the unlock swipe *must* go all the way across
the screen (making it hard to accidentally unlock) while swiping between screens
happens if you are half way or if you swipe fast enough (making it easy to flip around).
Pulling down the notifications tray and going from lock screen to camera also have
unique behavior.

The question now is: how do we provide a higher-level API that permits the care and
specificity that is required to make each gesture feel right?

And because touch input is new to FRP, this is an area in need of
some experimentation and design work. When using the touch API to
build gestures, I encourage you to continually ask: how do we best represent
a gesture in a general and purely functional way?

I have come up with two basic ideas that need to be tested out:

1. Represent gestures with a general state machine. Perhaps the programmer
   provides a set of step conditions, and the output is a signal of states
   (such as `TwoSwipe {x:Int,y:Int}`) that carry any relevant information
   for interactive animations.

2. Represent gestures as a function of position and time. For instance,
   say function *f* defines a time indexed area in 2D space, and if a touch
   escapes that area, it registers as a gesture. This may be a very
   beautiful solution in some cases, but it may also just force people to
   translate their state machines into mathematics.

There may be some other option out there, so keep these ideas
in mind when using `touches` and let [the list][discuss] know what you learn!

 [discuss]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "Elm discuss"

## Either

[The `Either` library][either] can be seen in action
[here](/examples/either). The most important
addition to come with this library is actually in the `Signal` library:

```elm
mergeEither : Signal a -> Signal b -> Signal (Either a b)
```

This lets you combine two signals without losing information about
the original source of the signal.

 [either]: http://package.elm-lang.org/packages/imeckler/either/1.0.0/ "Either"


## Consistency

I have simplified [the `Maybe` library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Maybe)
a little bit. Names are now consistent with the new `Either` library. For example,
when working with lists of Maybes or Eithers, you just ask to extract the
values you want:

```elm
justs  : [Maybe a] -> [a]
lefts  : [Either a b] -> [a]
rights : [Either a b] -> [b]
```

Or if you are curious about what kind of value you have:

```elm
isNothing, isJust : Maybe a -> Bool
isLeft, isRight   : Either a b -> Bool
```

Or if you want to extract a value from a Maybe or Either, you can use:

```elm
maybe  : b -> (a -> b) -> Maybe a -> b
either : (a -> c) -> (b -> c) -> Either a b -> c
```

With both extraction functions, you provide two ways to extract a value.

Some functions have also been taken out of the `Maybe` library: `fromMaybe`
and `mapMaybe`. I removed them because I thought they had silly names and
could be defined easily with more general functions. They can be re-defined as follows:

```elm
fromMaybe : a -> Maybe a -> a
fromMaybe default option = maybe default id option
fromMaybe' d = maybe d id

mapMaybe : (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = justs (map f xs)
mapMaybe' f = justs << map f
```

In general, it is probably just easier to not define the function and use `maybe` and
`justs` in these situations. In any case, sorry for any inconvenience!

## Thank you!

Thank you to Dobes and Luke for brainstorming the Touch API with me, and extra
thanks to Dobes for planting the thought in my mind.

Thank you to Grzegorz and John for finding two weird bugs!

Thanks to Grzegorz and Mads for working on cool projects! Again, I encourage you, the reader, to
[set up Preselm](https://github.com/grzegorzbalcerek/Preselm#preselm) or
[try out the inline docs](https://groups.google.com/forum/?fromgroups=#!topic/elm-discuss/_xmbeVfjYbI)!
"""
