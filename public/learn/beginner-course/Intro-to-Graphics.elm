import Website.Blog (skeleton)
import Window
import JavaScript as JS

titles = constant (JS.fromString "Intro to Graphics")
foreign export jsevent "title"
  titles : Signal JS.JSString

main = lift (skeleton everything) Window.width

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w title
  , section preface
  , width w video
  , section intro
  ]

title = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Introduction to Graphics</div>
</div>
|]

preface = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>
You are about to learn the basics of graphics in Elm.
The following video, [written explanation](#words), and [practice problems](#practice-problems)
are designed to help you dive into working with images, text, and shapes.

The video is followed by a written explanation that covers exactly the
same material. You can use the [online editor](http://elm-lang.org/try) to
follow along and start experimenting on your own.
|]

video = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/7mMBWfBpyYg?rel=0&html5=1"
        frameborder="0"
        allowfullscreen
        style="position: absolute; top: 0; left: 0; width: 100%; height: 100%;"
        width="853" height="480"></iframe>
</div>
|]

intro = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
h1, h2, h3, h4 {
  font-family: futura,'century gothic','twentieth century',calibri,verdana,helvetica,arial;
}
pre {
  margin: 0 30px;
  padding: 4px 10px;
  border: solid 1px rgb(235,235,235);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: white; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: white; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
</style>

<span id="words"></span><br/>
Okay, now we are going to cover the same material, but in text form.

This covers the basic graphical elements in Elm.
We will first cover images and text. From there we will learn how to
put many graphical elements together. Once we are good at putting
rectangular shapes together, we will branch out to the wild west
of triangles, pentagons, and circles.

# Elements

Basic graphical elements are called *elements* in Elm. An element is a rectangle
with a known width and height. Unlike triangles and pentagons, rectangles are
no-nonsense shapes that can be put together very easily.

There are many built-in functions that will help us create all of these
graphical elements. First we are going to cover images and text.

### Images

We will start with a classic cartoon bear.

```haskell
main = image 200 200 "/yogi.jpg"
```

Pictures have a width, height, and file name.
That file can be anything on the internet. We chose Yogi Bear, but
we could have chosen
[Hermann Hesse](http://upload.wikimedia.org/wikipedia/commons/thumb/d/da/Hermann_Hesse_2.jpg/501px-Hermann_Hesse_2.jpg)
or [Saturn](http://upload.wikimedia.org/wikipedia/commons/2/25/Saturn_PIA06077.jpg)

Maybe we decide to look at some shells from South Africa:

```haskell
main = image 400 200 "/shells.jpg"
```

That image looks all weird. The `image` function just stretches the
image to fit the dimensions. Let’s fix that!

```haskell
main = fittedImage 400 200 "/shells.jpg"
```

Lookin&rsquo; good.

### Text

In the first class, we used `asText` to show
very simple values, but it always used a monospace font.
It is not great for normal blocks of text. For that we use
[Markdown](http://daringfireball.net/projects/markdown/), a
nice format for making fancy text:

```haskell
main = [markdown|

# Making Fancy Text

You need to have an empty line to start a new
paragraph. Let’s try it.

Yep, this is definitely a new paragraph.
You can also make a list of things. Let’s make a
list of kinds of fancy text:

  1. *italic*
  2. **bold**
  3. `computery`
  4. [links to things](http://xkcd.com/323/)

|\]
```

Markdown is supposed to look a lot like the styled text it produces.
So bullet points look a lot like bullet points, and paragraphs are actually
separated like paragraphs. It tends to do the right thing, but if not,
you can look up the details [here](http://daringfireball.net/projects/markdown/syntax).

### Stacking Things

Now that we have some basic elements, the next step is to start putting them
together. We do this with the `flow` function.

```haskell
tongueTwister = [markdown|
She sells sea shells by the sea shore.
|\]

main = flow down [ tongueTwister, fittedImage 300 200 "/shells.jpg" ]
```

You can change down to be lots of different things. Your options are:
`up`, `down`, `left`, `right`, `inward`, and `outward`. They do what
they say they do. Try some of them!

# Forms

Rectangles are cool and all, but sometimes you just need a pentagon.
In Elm, irregular shapes that cannot be stacked easily are called *forms*.
These visual forms have shape, color, and many more properties and can be
displayed any which way on a [collage](ahttp://en.wikipedia.org/wiki/Collage).
We will start with a red pentagon:

```haskell
main = collage 400 400 [ filled red (ngon 5 100) ]
```

A `collage` is an element with a width and a height. It is just like images and text;
it is an easily stackable rectangle. The cool part is that a collage takes
a list of forms. In this case, we gave it a pentagon (an N-gon with five sides)
that has a radius of 100 pixels. We then filled the pentagon with red.

We could just have easily outlined that pentagon with blue.

```haskell
main = collage 400 400
         [ filled red (ngon 5 100)
         , outlined (dashed green) (ngon 5 100) ]
```

It does not need to be a dashed line though, it could also be `solid` or `dotted`.

### Moving, Rotating, and Scaling

The best part of forms is that we can move, rotate, and scale them.

```haskell
box color = filled color (square 40)

main =
  collage 400 400
    [ move (100,100) (box red)
    , scale 2 (box green)
    , rotate (degrees 45) (box blue)
    ]
```

Okay, so that was a lot of stuff. The trouble is that there is a lot more stuff!
This is why documentation is so important for programming languages. There are
tons of cool functions that other people have written that you can just start using.


# Practice Problems


### Problem 1


|]