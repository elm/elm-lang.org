import Website.Blog (skeleton)
import Window
import JavaScript as JS

titles = constant (JS.fromString "Intro to Lists and Records")
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
<div style="font-size: 4em;">Intro to Lists and Records</div>
</div>
|]

preface = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>
Now that you have been [introduced to programming](/learn/beginner-course/Intro-to-Programming.elm),
you are about to learn how to work with lists and records.

  * **Lists** are basically what they say, a big list of values.
    Not too crazy, but the values can be *anything*! Lists can
    represent anything from todo lists to armories full of
    axes and magic potions.
  * **Records** are a way for structuring and naming information.
    This sounds pretty dull, but records are amazing!
    They make it easy to represent complex ideas in code, like
    the values that will go in our todo list or armory.

The following video, [written explanation](#words), and [practice problems](#practice-problems)
are designed to help you dive into lists and records.

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

This covers basic graphics in Elm.
We will first cover lists. From there we will learn how to
use records. From there we will bring lists and records together
to start writing more complex programs.

# Lists

# Records

# Practice Problems

|]