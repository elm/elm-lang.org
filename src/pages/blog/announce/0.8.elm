import Blog
import Center
import Color exposing (rgb)
import Html


main =
  Blog.blog
    "Elm 0.8"
    "Too many improvements to fit in a pithy title"
    Blog.evan
    (Blog.Date 2013 5 29)
    [ Center.markdown "600px" content1

    , Html.div [] [ Html.text "TODO" ]
--    , Html.div
--        [Center.style (toString (widthOf dots) ++ "px")]
--        [Html.fromElement dots]

    , Center.markdown "600px" content2

    , Html.div [] [ Html.text "TODO" ]
--    , Html.div
--        [Center.style (toString (widthOf crosses) ++ "px")]
--        [Html.fromElement crosses]

    , Center.markdown "600px" content3
    ]


accent1 = rgb 96 181 204      -- #60B5CC  blue
accent2 = rgb 240 173 0       -- #F0AD00  yellow
accent3 = rgb 234 21 122      -- #EA157A  pink
accent4 = rgb 127 209 59      -- #7FD13B  green


content1 = """

This release has been in the works for quite a while now, and it is introducing
a ton of new features. The most important improvements and additions are:

* [Embedding Elm in HTML and JS](#embedding-elm-in-html-and-js) ([video demo](http://www.youtube.com/watch?v=xt07tLqa_m8))
* [Type annotations and type aliases](#type-annotations-and-type-aliases)
* [Input widgets can be created dynamically and updated programmatically](#dynamic-inputs)
* [Better 2D graphics API](#better-2d-graphics)
* [Performance and infrastructure improvements](#faster-currying-and-data-structures)
* [Inline documentation in the online editor](#inline-documentation) (thanks to Mads!)
* [Module tweaks, records to objects, WebSocket library, and new operators](#miscellaneous-experimental)

So this release finally answers the questions &ldquo;How do I dynamically create
buttons?&rdquo; and &ldquo;How do I embed Elm in HTML and JS?&rdquo; I think
this is a big step towards writing traditional web apps in the Functional
Reactive style. In the case of dynamic creation of buttons, it actually
required an approach that appears to be new to FRP.

There are a some breaking changes, so it is highly likely that your existing
Elm code will need to be modified to work with 0.8. If you are having trouble,
please let us know on the [mailing
list](https://groups.google.com/forum/?fromgroups#!forum/elm-discuss)
so we can help you out and get better documentation out for any confusing parts.

## Embedding Elm in HTML and JS

Elm can now be embedded directly in HTML or JS. This means Elm will
integrate with your existing workflow, whether you make web apps
or work with server-side JS. Using Elm is not an all-or-nothing choice anymore.

This lowers the barrier if you want to experiment with Elm and makes it
easier to convince your boss that it is okay to use Elm in an existing project.

The description of [how to embed Elm code](/guide/interop)
explains all of the details of the API.

The following video is a short demo of how to embed Elm in a `<div>`.

<div class="intrinsic-container">
  <iframe src="http://www.youtube.com/embed/xt07tLqa_m8?rel=0" allowfullscreen></iframe>
</div>

So it is no longer an all-or-nothing choice. You can use Elm where it
makes sense and HTML everywhere else.

## Type Annotations and Type Aliases

If you are new to types, I recommend reading
[Getting started with Types](/guide/model-the-problem)
which explains how types work in Elm.

You can now add type information to your programs if you want. It is
not required, but it is recommended.

```elm
reverse : [a] -> [a]
reverse = foldl (::) []
```

Notice that the meanings of `(:)` and `(::)` have swapped. `(:)` is &ldquo;has
type&rdquo; and `(::)` is cons. This is how it is in SML, OCaml, Coq, and Agda.
Given the relative frequency of type annotations, it makes sense to give types
a lighter syntax.

You can also add type aliases. This lets you give nice consise names
for larger types. This is most useful for records:

```elm
type Point = { x:Float, y:Float }

add : Point -> Point -> Point
add a b = { x = a.x + b.x, y = a.y + b.y }
```

You can also have type variables in your aliases which opens the door for
lots of cool stuff.

```elm
type Positioned a = { a | x:Float, y:Float }
type Movable a = { a | velocity:Float, angle:Float }

ball : Positioned (Movable {})
ball = { x=0, y=0, velocity=42, angle=0 }
```

For those of you who really know your types, I should note that higher-kinded
polymorphism is not possible right now. That would permit an explicit form
of type-classes, but that is for another day.


## Dynamic Inputs

The new [`Graphics.Input` library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Graphics-Input)
introduces text boxes, buttons, and checkboxes that can be created dynamically and
updated programmatically.

I am working on writing a walkthrough to explain how these new features work
in detail. For the now the best showcase is [this TodoFRP
demo](https://www.youtube.com/watch?v=cI__rjCiH_k). The plan is to release
this code and an overview of how it works as soon as possible!

These capabilities is brand new to Elm and was made possible by an idea
from [@gozala](https://twitter.com/gozala). As this API took shape,
it was very encouraging that it ended up confirming the principles
of [bidirectional data flow](http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html)
as described by apfelmus.


## Better 2D Graphics

The `collage` API has been streamlined and now permits grouping
and 2D matrix transforms. It also uses a proper [cartesian coordinate
system](http://en.wikipedia.org/wiki/Cartesian_coordinate_system).


#### Cartesian Coordinates

First we should discuss how things used to work in Elm. The Old Way.

<img src="/assets/blog/0.8/flipped.jpg"
     alt="upside down coordinates"
     style="border: 1px solid rgb(216,221,225); margin-left: 30px; float:right; width:126px; height:120px;">

The JavaScript `<canvas>` uses a coordinate system that is upside down.
Increasing the y-coordinate moves an object *lower* on
the screen. Elm did this too up until now.

With the inverted y-axis, it is easy to find yourself randomly adding
minus signs to try to
get your code to work in this coordinate system, especially when you
are trying to work with rotations and [polar
coordinates](http://en.wikipedia.org/wiki/Polar_coordinate_system).

<img src="/assets/blog/0.8/cartesian.jpg"
     alt="cartesian coordinates"
     style="border: 1px solid rgb(216,221,225); margin-left:30px; float:right; width:126px; height:91px;">

Elm now uses the cartesian plane for rendering. The origin is in the middle
of the `collage` and the y-axis points up.

This means your mental model maps directly onto the graphics API.


#### Forms at the origin

Now when you create a form, it is positioned at the origin. You no longer
need to provide a position. Here are a few functions from the new API:

```elm
circle : Float -> Shape
toForm : Element -> Form
```
You can see the full API in the
[`Graphics.Collage` library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Graphics-Collage).
For now we will look at some smaller examples and uses.

The following example creates four colorful dots. It shows a basic
case in which having the position default to the origin is quite convenient.
It uses the standard accent colors for the Elm website:

"""


content2 = """

```elm
dot colr = collage 50 50 [ filled colr (circle 15) ]

dots = flow right (map dot [accent1,accent2,accent3,accent4])
```
That seems okay, but this is a breaking change that will effect all
`collage` code. Are we really any better off than before?
Starting from the origin really comes in handy when it is
paired with grouping.

#### Grouping and matrix transforms

Version 0.8 introduces two grouping functions which let you flatten a list
of forms into a single form. This makes things much more composable:

```elm
group : [Form] -> Form
groupTransform : Matrix2D -> [Form] -> Form
```
These functions let you create small self-contained components.
You can position forms without thinking about how they will be used
later on.

This next example creates four colorful, rotated crosses.
It uses `group` to flattens a visual component into a single `Form`,
making them easy to move and rotate.
"""


content3 = """

```elm
-- Create two rectangles that cross at the origin. We use
-- the group function to flatten them into a signle Form.
twoRects : Color -> Form
twoRects colr =
    group (List.map (filled colr) [ rect 8 30, rect 30 8 ])

-- We now create the cross by giving our rectangles a color
-- and then rotating them by the given angle in degrees.
cross : (Float,Color) -> Element
cross (angle, colr) =
    collage 50 50 [ rotate (degrees angle) (twoRects colr) ]

-- Now we put together four crosses, each with a
-- different angle and color.
crosses : Element
crosses =
    flow right <|
      List.map cross
        [ ( 0, accent1)
        , (10, accent2)
        , (20, accent3)
        , (30, accent4)
        ]
```

This example also shows that rotations are based on the
[unit circle](http://en.wikipedia.org/wiki/Unit_circle).
Angles start on the positive x-axis and go counterclockwise,
just like with `sin`, `cos`, etc.

This example also shows the `degrees` function, which is part of
a family of functions:

```elm
degrees 90 == radians (pi/2) == turns 0.25
```

These functions convert angles into whatever system Elm uses for angles.
That system happens to be radians, but now you do not *need*
to remember that.

The `groupTransform` function works just like `group` except that
it applies a matrix transformation to the flattened `Form`.
This allows you to build up a traditional
[scene graph](http://en.wikipedia.org/wiki/Scene_graph)
and do fancier transformations like reflections and skews with
the new [`Matrix2D` library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Transform2D).


## Faster currying and data structures

Currying is now quite a bit faster and all ADTs have a much faster
representation. This should not change how you write code. Your code
is just faster now!

Function calls in JS can be somewhat expensive, so when you have curried
functions, you end up paying for a lot of function calls even though you may
know most or all of the arguments. So for `(clamp 0 100 n)` the basic
implementation would make three function calls, but we already know all
three arguments so it would be faster just to make one function call for
all three. This release introduces this optimization.

If you want to read more about a bunch of different currying optimizations, see
[Making a fast Curry](http://community.haskell.org/~simonmar/papers/evalapplyjfp06.pdf)
which describes how currying works in Haskell.

## Inline Documentation

[The online editor](/edit/examples/Intermediate/Circles.elm) now shows type
information and documentation when your cursor moves over a function. This
even works for hard-to-Google operators like `(<~)` and built-in syntax
like `if`.
This makes it way easier for beginners to get oriented in a program.

You can use `ctrl-k` to peek at the type and description of a value in the
editor. With `ctrl-shift-k`, you can jump directly to the docs for that function.

It is simple to disable this feature if you want, but I find it is usually
quite handy.

This feature was conceived, designed, and implemented by Mads.
I think this is one of the coolest things in the editor, and it is
another step towards getting extremely fast feedback for Elm development.

## Miscellaneous / Experimental

There is a bunch of small changes and additions that aim to make Elm better
long-term. Many of these are experimental or setting the groundwork for bigger
features in future releases.

#### Importing Modules

There are some minor changes here. This is how module imports work now:

```elm
-- Import the Dict module. You can only access its values
-- with field access: Dict.empty, Dict.insert, etc.
import Dict

-- Import Dict and load its values into local scope.
-- You can just use empty, insert, etc. without any prefix.
import open Dict

-- Import Dict and load certain values into local scope.
-- This lets you say empty and Dict.empty.
import Dict (empty, insert)

-- Import Graphics.Input under the name Input
import Graphics.Input as Input
```

First-class modules are in the pipeline for Elm, and
the `open` keyword is a very early step in this direction.
For more info on first-class modules, check out the module system
of OCaml or Agda.

This release also fixes a bug in detecting cyclic module dependencies.

#### Objects and WebSockets

You can convert between JS objects and Elm records with the
[`JavaScript.Experimental` library](https://github.com/elm-lang/elm-compiler/blob/0.8.0.3/libraries/JavaScript/Experimental.elm).
As the name suggests, the approach used by this library is experimental!
Please let me know what you think of it on the [mailing
list](groups.google.com/forum/?fromgroups#!forum/elm-discuss).

You can also work with websockets via the
[`WebSocket` library](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/WebSocket).
This API may change to accomadate more usage scenarios. Please let me
know how you want to use it!

#### Application Operators

Elm now has operators for forward application `(|>)` and
backward application `(<|)`. These work much like the old `($)` operator
and are mainly useful for saving yourself from writing too many parentheses.

```elm
-- (sqrt 4) == (sqrt <| 4) == (4 |> sqrt) == 2

-- The (<|) operator works just like ($).
formA = scale 2 << rotate (degrees 30) << filled blue <| ngon 3 40

-- The (|>) operator can be quite nice when creating forms.
formB = ngon 3 40 |> filled blue
                  |> rotate (degrees 30)
                  |> scale 2
```

The `($)` operator is being deprecated, so try to get rid of it in your code.
It will probably get taken out for good in the next release.

#### Pattern Matching

You can now do record pattern matching in case expressions. We have talked about
how to make record pattern matching more flexible in general, so that will
come out at some point in the future.

## Thank you!

Thank you to Mads for his really nice improvement to the editor. Having
the documentation for every function makes it way easier for a beginner
to navigate the examples.

Thank you to John for devising a clever representation for modules that
ended up making it easy to embed Elm modules in HTML and JS.

Thank you to [@gozala](https://twitter.com/gozala) for providing the
key insight that makes the new `Graphics.Input` library work.

Thank you to Luite and Hamish for showing me the
[Making a fast Curry](http://community.haskell.org/~simonmar/papers/evalapplyjfp06.pdf)
paper and getting me thinking about JS performance. For more info about making
all of Haskell work in browsers, see [their talk on ghcjs](http://www.ustream.tv/recorded/29327620)
from the mloc.js conference.

Thank you to everyone on [the mailing list](https://groups.google.com/forum/?fromgroups#!forum/elm-discuss)
who helped me test and think through all of these new ideas!

I have never had so many people to thank for a release, so please forgive me
if I have forgotten anyone!

"""
