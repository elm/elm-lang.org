import Website.Skeleton (skeleton)
import Website.Tiles as Tile
import Window

port title : String
port title = "Elm 0.12.2 - WebGL"

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  flow down
        [ width w intro
        , exampleBlock w
        , width w rest
        ]

exampleBlock w =
    Tile.examples w [ map Tile.webgl [ "Cube", "Crate", "Thwomp", "FirstPerson" ] ]

intro = [markdown|
<style type="text/css">
p { text-align: justify }
</style>

<h1><div style="text-align:center">Elm 0.12.2 - WebGL
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Functional 3D Rendering*</div></div>
</h1>

Elm now supports 3D rendering with WebGL! Huge thank you to
[John P. Mayer](https://github.com/johnpmayer/) for designing and implementing
such a simple API for this. It has been really fun to work with so far and we
are excited to see what people can do with it!

This release also comes with some changes to the `Color` library, making it
easier to create colors programmatically. The initial motivation was to make
`Color` play nice with WebGL, but the library came out a lot friendlier to use
in general.

## Functional 3D Rendering

We of course need to start with an example. As you move your mouse above the
grey box, [Thwomp](http://www.mariowiki.com/Thwomp#Super_Mario_64) will stare
at you. Too far away to crush you, but waiting...

<iframe src="/examples/WebGL/Thwomp.elm"
        frameborder="0"
        width="526"
        height="300"
        style="background-color: #D8DDE1;">
</iframe>

Typically, working with WebGL in JS means wrestling with a huge 90s era C++ API
with a great deal of [incidental
complexity](http://en.wikipedia.org/wiki/Accidental_complexity).
[John](https://github.com/johnpmayer/) has done a great job simplifying and
modernizing this API for Elm, only exposing details that are truly essential
to 3D rendering and efficient use of the GPU.

The best way to get started with this API is to read about [the architecture of
WebGL in Elm](https://github.com/johnpmayer/elm-webgl/blob/master/README.md) and
then play around with some examples to get a feel for actually using this API:

|]

rest = [markdown|

<style type="text/css">
p { text-align: justify }
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

The immediate next steps for WebGL are to begin building on this foundation.
We have the basics, but we can start to create libraries for common shapes
(branching out from cubes!) or loading meshes or defining shaders with an EDSL.
There is a lot to explore here, and John's simplified API significantly lowers
the barrier to entry and makes it really fun to work with 3D!

## Colors

[The `Color` library](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Color)
underwent a breaking change by switching from HSV to HSL. If those letters do
not mean anything to you, your code will be fine and you are about to learn
about a very useful color space! HSV and HSL are very closely related, so
upgrading should be easy. The library also got some new functions that make it
possible to generate colors and color schemes programmatically!

#### Switching to HSL

<img src="http://upload.wikimedia.org/wikipedia/commons/thumb/c/cb/HSL_color_solid_cylinder_alpha_lowgamma.png/320px-HSL_color_solid_cylinder_alpha_lowgamma.png"
     style="float:right; padding-left:10px;"
     width="160"
     height="120">

HSL stands for Hue-Saturation-Lightness. Hue determines a particular color on
[the color wheel](http://colorschemedesigner.com/). The saturation level is how
vibrant the color is, like a dial between grey and bright colors. The lightness
level is a dial between white and black. Once you internalize how this system
works, possibly by reading [the wikipedia
article](http://en.wikipedia.org/wiki/HSL_and_HSV) a few times, it is a really
easy way to turn the *idea* of a color into some concrete numbers in code. It
is totally worth the effort to learn to think in HSL!

We switched away from Hue-Saturation-Value (HSV) because Value is a bit more
confusing than Lightness. For example, it is fairly easy to deduce that pastel
colors are light and desaturated. In HSL, you turn down saturation and turn up
lightness. Pretty easy. In HSV, there is not really a dial for lightening
things up, so it is trickier to deduce how to create a pastel color.

#### Algorithmic Colors

The new color library includes two new functions that make it possible to work
with colors programmatically:

```haskell
toRgb : Color -> { red:Int, green:Int, blue:Int, alpha:Float }

toHsl : Color -> { hue        : Float
                 , saturation : Float
                 , lightness  : Float
                 , alpha      : Float
                 }
```

This makes it easy to use Elm's built-in colors in [the cube
example](/edit/examples/WebGL/Cube.elm), but these new functions are much more
general than that!

[The color wheel](http://colorschemedesigner.com/) can be really helpful for
creating color schemes. The relationships between colors could be the foundation
for a library that lets you easily create triads or [analogous
colors](http://en.wikipedia.org/wiki/Analogous_colors). You could also generate
lighter or darker or greener versions of a color, so your programs could be easy
to update when the color scheme changes. You could also create a
[color conversion site](http://rem.im/rgb2hex.html) that actually lets you
convert back-and-forth between color spaces. Easily discoverable sites for this
tend to work only in one direction (hex to RGB) and often are not very pleasant
to look at or use. Please someone fix this!

## Thank you!

Thank you again to [John P. Mayer](https://github.com/johnpmayer/) who designed
and implemented the WebGL libraries! The Elm community has been wondering about
this from very early on, and it is great to finally see it in practice!

Thanks also to [Max New](https://github.com/maxsnew) who has been making a
bunch of contributions, particularly for handling the recent outbreak of cabal
issues with Travis CI. Cabal hell is a very dark place, so thank you!

|]
