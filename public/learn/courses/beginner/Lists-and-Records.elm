import Website.Blog (skeleton)
import Window
import JavaScript as JS

port title : String
port title = "Intro to Lists and Records"

main = lift (skeleton everything) Window.width

everything wid =
  let w  = truncate (toFloat wid * 0.8)
      w' = min 600 w
      section txt =
          let words = width w' txt in
          container w (heightOf words) middle words
  in
  flow down
  [ width w pageTitle
  , section preface
  , width w video
  , section intro
  ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, calibri, verdana, helvetica, arial; text-align: center;">
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
    Lists can represent anything from todo lists to armories full of
    axes and magic potions.

  * **Records** are a way for structuring and naming information.
    Records make it easy to represent complex ideas in code, so
    we can begin to talk about tasks in our todo list or weapons
    in our armory.

The following video and [practice problems](#practice-problems)
are designed to help you dive into lists and records.

The video is followed by a written explanation that covers exactly the
same material. You can use the [online editor](http://elm-lang.org/try) to
follow along and start experimenting on your own.
|]

video = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/MLNvn7fml_Q?rel=0&html5=1"
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
  font-family: futura,calibri,verdana,helvetica,arial;
}
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

# Practice Problems

These problems will challenge you to use and expand upon your knowledge of
lists and records. Remember that case expressions and pattern matching are
your friends!

  1. Write `product` which gets the product of a list:

        product [1,2,3] == 6
        product [4,4] == 16
        product [] == 1

  2. Write `squareAll` which squares every member of a list:

        squareAll [1,2,3] == [1,4,9]

  3. Write `dropTom` which takes Tom out of a list:

        dropTom ["Sue","Tom","Bill"] == ["Sue","Bill"]
        dropTom ["Sam","Peter"] == ["Sam","Peter"]

  4. Write `everyOther` which gets every other element of a list:

        everyOther [1,2,3,4] == [1,3]
        everyOther ["Tom","Sue","Sam"] == ["Tom","Sam"]
        everyOther [] == []

  5. Write `factorial` without using `if`

        factorial 0 == 1
        factorial 1 == 1
        factorial 2 == 2
        factorial 3 == 6
        factorial 4 == 24

  6. Create record representations of some animals.
     Include information on species, age, and coolness level.

  7. Put all of your animals in a list. Write a function `coolest`
     which finds the coolest animal.

        coolest [cow, chicken, trex] == trex
        coolest [whale, mouse, tiger] == whale

     These examples use my personal opinions of coolness. Your
     `coolest` function should decide what to do in the event of
     a coolness tie.

|]