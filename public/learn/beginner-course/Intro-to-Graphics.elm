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
same material. As always, **you can use the [online editor](http://elm-lang.org/try) to
follow along** and start experimenting on your own.
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
Okay, now we are going to cover the same material, but in text form. This is nice
for skimming or review or whatever.

# Elements

The basic building blocks of graphics are literally blocks, rectangles
filled with pretty things.

### Images

Some of those pretty rectangles are pictures:

```haskell
main = image 400 200 "shells.jpg"
```

Pictures have a width, height, and file name.
That file can be anything on the internet, even this one.

But that image looks weird. It is all stretched. Let’s fix that!

```haskell
main = fittedImage 400 200 "shells.jpg"
```

Lookin&rsquo; good.

### Fancy Text

You can also do text that is all fancy, like with bullet
points and italics and big bold things.

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

Markdown is supposed to look a lot like the styled version it produces.
So bullet points look a lot like bullet points, and paragraphs are actually
separated like paragraphs. It tends to do the right thing, but if not,
you can look up the details [here](http://daringfireball.net/projects/markdown/syntax).

### Stacking Things

It’s not enough to have pretty pictures and fancy text. We need both.

At the same time.

We do this with the flow function.

```haskell
fancyText = [markdown|
I’m a *pretty* pony.
|\]

main = flow down [ fancyText, fittedImage 300 200 "car.jpg" ]
```

You can change down to be lots of different things. Your options are:
`up`, `down`, `left`, `right`, `inward`, and `outward`. They do what
they say they do. Try some of them!

`flow up` kind of sounds like throw up. I don’t like this.

# Forms

Rectangles are cool and all, but sometimes you just need a pentagon.

```haskell
main = collage 400 400 [ filled green (ngon 5 100) ]
```

Okay, so this one is more complicated. A `collage` is an Element with
a width and a height (boring). The cool part is that it takes
a list of forms that can be rotated, moved, and scaled in any way you want.
In this case, we gave it a pentagon (an n-gon with five sides) that has
a radius of 100 pixels. We then filled the pentagon with green.

We could just have easily outlined that pentagon with blue.

```haskell
main = collage 400 400
         [ filled yellow (ngon 5 100) 
         , outlined (dashed blue) (ngon 5 100) ]
```

It does not need to be a dashed line though, it could also be `solid` or `dotted`.

### Moving, Rotating, and Scaling

Now that we can make shapes, let’s mess around with them.

```haskell
lightGrey = filled (rgba 99 99 99 0.3)

main =
  collage 400 400
    [ move (100,100) (lightGrey (circle 50))
    , scale 2 (lightGrey (oval 50 30))
    , rotate (degrees 45) (lightGrey (rect 150 15))
    ]
```

It can get kind of hard to read all of these functions, so Elm lets you use
the `|>` operator to change the order of functions and get rid of a bunch of parentheses.

As a basic example, let’s use `|>` on some simple functions:

```haskell
main = asText [ sqrt 4, 4 |> sqrt ]
```

They are the same thing, so you can think of `|>` as saying
“give this value to that function over there, where I am pointing.”

Let’s use it on our example of moving things around:

```haskell
main =
  collage 400 400
    [ circle 50
          |> outlined (solid green)
          |> move (100,100)
    , oval 50 30
          |> filled red
          |> scale 2
    , rect 150 15
          |> filled (rgba 12 0 212 0.3)
          |> rotate (degrees 45)
    ]
```

Wowzers, that is way easier to read now!

Okay, so that was a lot of stuff. The trouble is that there is a lot more stuff!
This is why documentation is so important for programming languages. There are
tons of cool functions that other people have written that you can just start using.


# Practice Problems


### Problem 1


|]