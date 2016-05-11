import Blog
import Center


main =
  Blog.blog
    "Elm 0.4"
    "Graphics Upgrade"
    Blog.evan
    (Blog.Date 2012 9 28)
    [ Center.markdown "600px" content ]


content = """

This release makes [Elm](/) better for:

* **Making games.** Elements, sprites, and textures can now be used in a `collage`
  This includes text, gifs, videos, and any other complex `Element`, making Elm much
  more flexible for game creation. Check out examples of the new features
  ([Elements][a], [sprites][b], [textures][c]) and learn how to make games with
  the [Pong walkthrough][pong].

* **Creating text.** Elm now has native [Markdown][1] support ([example][2]).
  You can also set typefaces (a.k.a. "fonts") programatically ([example][typeface]).

* **Graphics in general.** Screen updates are now much more efficient, reducing
  memory usage, CPU usage, and screen flickering. You can also get some information
  about elements with `widthOf`, `heightOf`, and `sizeOf`.

* **Everything.** The compiler now performs slightly more aggressive optimizations,
  and it produces pattern matching code that is much faster. And minification of generated
  JavaScript is now possible with the `--minify` compiler flag.

I am really excited about these new features, and I think they substantially
increase the practicality and pleasantness of Elm. And with the `collage` additions,
I think Elm is currently the best language for creating purely functional
online games.

To install see the [instructions](https://github.com/elm-lang/Elm/blob/master/README.md)
on [github](https://github.com/elm-lang/Elm), and to upgrade use the command:

```bash
cabal update ; cabal install elm
```

The rest of this post is devoted to explaining the new features and providing
examples of their usage. These improvements also come with some minor breaking
changes, which are detailed below with upgrade advice.

I hope you enjoy Elm 0.4!

 [pong]: /blog/making-pong
 [a]: /edit/examples/Elements/ToForm.elm "toForm"
 [b]: /edit/examples/Elements/Sprite.elm "sprites"
 [c]: /examples/texture "texture"

## Markdown Support

Creating text in a language designed for GUIs *should* be really easy. Well
now it is! [Markdown][1] can now be embedded directly in Elm!
Markdown lets you write fairly complicated formatted text
"using an easy-to-read, easy-to-write plain text format".

The syntax is as follows:

```elm
[markdown| ... |] : Element
```

Where `...` is some Markdown (and can span multiple lines). For example:

```elm
main = [markdown|

# The Title

This is a paragraph.
This sentence is part of the same paragraph.

|]
```

See [this example][2] if you want to play around with this feature, and be sure
to check out [this site][1] to learn the full capabilities of Markdown.

This blog post ([source][blog]) is actually written using this feature!
I had no idea such a simple addition could have such a positive impact!

 [1]: http://daringfireball.net/projects/markdown/dingus "Markdown"
 [2]: /examples/markdown "Elm+Markdown example"
 [blog]: https://github.com/elm-lang/elm-lang.org/blob/master/src/pages/blog/announce/0.4.elm

## Games: Elements, Sprites, and Textures

The `collage` interface just became *way* more flexible. Collages can now
include pretty much anything. More specifically:

* Display *any* `Element` with:
      `toForm : (Int,Int) -> Element -> Form`<br/>
  This allows you to use text, gifs, videos, and other complex components
  just like any other `Form`.

* Display sprites with:
      `sprite : String -> Int -> Int -> (Int,Int) -> Form`

* Fill shapes with arbitrary textures with:
      `texture : String -> Shape -> Form`

All of these new forms can be moved, scaled, and rotated.

I think these additions, especially `toForm`, make Elm the nicest and easiest
way to make games in a functional language. Of course, I am totally biased, so
you can decide for yourself!

These additions required a small change to the API for transforms. The new
functions have the following types:

```elm
move : Int -> Int -> Form -> Form
rotate : Float -> Form -> Form
scale : Float -> Form -> Form
```

The only difference is that a transform now acts upon a `Form` instead of
directly upon a form-precursor (a `Shape` or `Line`). Before, form-precursors
could *only* appear in a collage. Now that the form-precursors also include
Elements, this approach is not possible. What would it mean to rotate an Element?!

## Graphics in General

Along with the more obvious API changes, Elm has also undergone some major internal
rewrites. The part of the runtime-system (RTS) responsible for screen updates has been
completely rewritten to make things more efficient.

The RTS now manipulates the DOM way less frequently, so creation of new DOM nodes has
decreased dramatically. This means fewer cycles are spent messing with the DOM, less
memory is wasted on unused DOM nodes, and the JavaScript garbage-collector
spends less time cleaning up the mess afterwards.

This is great for Elm in general, and it is particularly important for game making.

## New Functions

#### Typeface

Set the typeface (often incorrectly called the "font") used to display text.
The string argument contains the names of the typefaces you want to use.

```elm
typeface : String -> Text -> Text
```

See the [`typeface` example][typeface] for details.

 [typeface]: /edit/examples/Elements/Typeface.elm "typeface example"

#### Dimensions

You can look up the dimensions of an `Element` with:

```elm
widthOf  : Element -> Int
heightOf : Element -> Int
sizeOf   : Element -> (Int,Int)
```

This should make it easier to create higher-order elements (functions that take
take elements as arguments and produce a more complicated layout).

## Optimizations, Pattern Matching, Minification

The compiler now performs beta-reduction when it reduces the size of the output.

Pattern matching is implemented much more efficiently (as described
[here](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/start.htm)).

You can also reduce the size of the compiler output by using the `--minify` flag.

## Breaking Changes

The following changes were made after much deliberation, but I think they are best for
Elm. If they cause you trouble, please email the [list][elm-discuss] and tell
me what is up. I will do everything I can to make the transition to 0.4 as smooth
as possible.

 [elm-discuss]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss "elm-discuss"

#### Dimensions for Images and Videos

This one is sad, but `image` and `video` both require a width and a
height. The new API is:

```elm
image, video : Int -> Int -> String -> Element
```

This unfortunate requirement has a very good rational though.

This version of Elm allows some inspection of Elements with `widthOf`,
`heightOf`, and `sizeOf`. A very useful feature to have! To implement
these functions, the size of every Element must be known at runtime.
"No problem," you might think, "images and videos have a default size!"
Yes, but image and video content is loaded asynchronously. If we want to
know the default size of an image, we have to wait until it loads. This
would mean that an Elm program would block until every single image is
loaded. But what if one of the images does not load? The entire page
does not load!

It is actually still possible to load an image without specifying dimensions,
but the API is a little different:

```elm
images : Signal String -> Signal Element
```

The old `image src` is almost the same as `images (constant src)`, but
instead of yeilding an `Element`, the new version produces `Signal Element`.
This is actually the correct API because it captures the fact that
the image loads asynchronously.

#### Containers and Spacers

Two existing functions (`box` and `rectangle`) have been renamed and reworked
(now `container` and `spacer`).

```elm
container : Int -> Int -> Position -> Element -> Element
spacer : Int -> Int -> Element
```

For more info on the new functions see the [`container` example][container] and the
[`spacer` example][spacer].

 [container]: /edit/examples/Elements/Position.elm "container example"
 [spacer]: /edit/examples/Elements/Spacer.elm "spacer example"

If you have code that uses the old versions, you can just add these two
definitions to your code:

```elm
box n e =
  let pos = head << drop ((n-1) `mod` 9) <|
        [ topLeft, midTop, topRight
        , midLeft, middle, midRight
        , bottomLeft, midBottom, bottomRight ]
  in  container (widthOf e) (heightOf e) pos e

rectangle = spacer
```

I would really recommend updating the code (it was a pretty easy change in
my experience), but it is your call!

"""
