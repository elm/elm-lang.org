import Website.Blog (skeleton)
import Window
import JavaScript as JS

port title : String
port title = "Intro to Programming"

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
  , section (pics w')
  , section problems
  ]

pageTitle = [markdown|
<br/>
<div style="font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; text-align: center;">
<div style="font-size: 4em;">Introduction to Programming</div>
</div>
|]

preface = [markdown|
<style type="text/css">
p, li {
  text-align: justify;
  line-height: 1.5em;
}
</style>
You are about to learn the basics of programming.
The following video, [written explanation](#words), and [practice problems](#practice-problems)
are designed to take you from thinking that programming is
weird and mysterious to being able to read and write basic programs.

The video is followed by a written explanation that covers exactly the
same material. Neither assume any prior knowledge of programming.
I recommend starting with the video, but either way,
**you can use the [online editor](http://elm-lang.org/try) to follow along**
and start experimenting on your own.
|]

video = [markdown|
<div style="position: relative; padding-bottom: 56.25%; padding-top: 30px; height: 0; overflow: hidden;">
<iframe src="//www.youtube.com/embed/alySKbsDZ9E"
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

<span id="words"></span><br/>
Okay, now we are going to cover the same material, but in text form. This is nice
for skimming or review or whatever.

### Hello World
Every coder’s first program is “Hello World!”. Welcome to the club!

```haskell
main = asText "Hello World!"
```

`main` is what gets shown on screen. `asText` will turn anything
into text that can be shown on screen.


### Numbers

You can also do simple mathematics.

```haskell
main = asText 42
```

Addition uses the plus sign.

```haskell
main = asText (2 + 2)
```

This is similar to using a graphing calculator or doing math on paper.
Note that `*` means multiply, `/` means divide, and `^` means “to the power of”

This is 3 times 4 over 2

```haskell
main = asText (3 * 4 / 2)
```

Here we compute (3<sup>2</sup> + 4<sup>2</sup>)

```haskell
main = asText (3^2 + 4^2)
```

### Strings

Strings are sequences of letters. We can put them together with the `++` operator.

```haskell
main = asText ("Hello " ++ "Steve!")
```

### Lists
Lists are a sequence of things. All the things must be the same type
of thing though! For instance, the list can be all numbers:

```haskell
main = asText [1,2,3]
```

Or a list can be all strings:

```haskell
main = asText ["Sally", "Steve", "Tom"]
```

Note: square braces, curly braces, and parentheses are all mean
*different* things! Be careful!


### Boolean Logic
This is a fancy way to say that we can talk about things being `True` and `False`.

```haskell
main = asText True
```

If something can be `True` or `False`, it can also be `not True` or `not False`!

```haskell
main = asText (not False)
```

Boolean logic also lets us compare things. Is 2 less than 4?

```haskell
main = asText (2 < 4)
```

Is 2 + 2 equal to 5?

```haskell
main = asText (2 + 2 == 5)
```

Is it true that 3<sup>2</sup> + 4<sup>2</sup> = 5<sup>2</sup>?

```haskell
main = asText (3^2 + 4^2 == 5^2)
```

Does capitalization matter? (Hint: YES!!!)

```haskell
main = asText ("Hello" == "hello")
```

### Conditionals
Now we are going to use boolean logic for something more useful.

```haskell
main = asText (if True then 42 else 11)
```

Let’s recreate [this scene](http://www.youtube.com/watch?v=SiMHTK15Pik)
from Dragon Ball Z.

```haskell
main = asText (if 9500 < 9000 then "meh"
                              else "It's over 9000!!!")
```

### Variables
This lets us “save” values to use later. Say we define a number that we’d
like to use in two places.

```haskell
number = 2 + 2
main = asText (number + number)
```

We just made up the name `number`. We could have called it anything we want!
A more descriptive name would be `four`, but the program works exactly the same.

```haskell
four = 2 + 2
main = asText (four + four)
```

We can even choose names like `fish` and `tree` if we want, but it is best
to give relevant and descriptive names.

Maybe we want to save someones name so it is easier to change:

```haskell
name = "Steve"
main = asText ("Hello " ++ name ++ "!")
```

Maybe we want to give custom greetings depending on who we are talking to:

```haskell
name = "Steve"
main = asText (if name == "Steve" then "Hey friend!"
                                  else "Hello " ++ name ++ "!")
```

Let’s rewrite our scene from Dragon Ball Z with a variable:

```haskell
powerLevel = 9001
main = asText (if powerLevel < 9000 then "meh"
                                    else "It's over 9000!!!")
```

### Functions

A function is a way of reusing variables. Right now it is hard to say hello to
two people; we’d have to write the same code twice! To avoid this, we can
write a function called `hello` to help us reuse code.

```haskell
hello name =
    if name == "Steve" then "Hey friend!"
                       else "Hello " ++ name ++ "!"

main = asText [ hello "Sally", hello "Tom", hello "Steve" ]
```

We can also make it easier to react to someone’s power level by writing a
function called `react`.

```haskell
react powerLevel =
    if powerLevel < 9000 then "meh" else "It's over 9000!!!"

main = asText [ react 10, react 1000, react 10000 ]
```

There are a lot of built-in functions like `sqrt`, which takes the square root
of a number.

```haskell
main = asText (sqrt 25)
```

Here is a function that increments numbers by one.

```haskell
increment n = n + 1
main = asText [ increment 3, increment 4, increment 41 ]
```

Maybe you want to average two numbers.

```haskell
average n m = (n + m) / 2
main = asText [ average 3 4, average 5 12 ]
```

### Functions that depend on themselves!

Remember the definition of [factorial][] from school?
It is the product of all numbers between 1 and *n*, and it
is useful for thinking about [permutations][]. If you want to
know how many ways you can order a deck of cards, you want factorial.

In math talk, it is defined like this:

 [factorial]: http://en.wikipedia.org/wiki/Factorial
 [permutations]: http://en.wikipedia.org/wiki/Permutation

<img src="/imgs/factorial.png"
     style="height: 100px; display: block; margin-left: auto; margin-right: auto"></img>

And when you turn this into code, it looks very very similar!

```haskell
factorial n =
    if n == 0 then 1 else n * factorial (n-1)

main = asText (factorial 3)
```

That was a quick overview of the basics of programming. To practice these
ideas, read on and try the practice problems!

# Practice Problems

These problems are a way for you to get more familiar writing code and *thinking*
like a programmer.

### Problem 1

There is a built-in function called `sqrt`. It lets you compute the
square root of a number. So the following is a list of
`True` things: `[ sqrt 4 == 2, sqrt 9 == 3 ]`

Do you remember [the Pythagorean Theorem](http://en.wikipedia.org/wiki/Pythagorean_theorem)?
It is a way to find the length of the diagonal edge of a right triangle. You can think of it
a couple different ways:
|]

pics w =
  flow right . map (container (w `div` 2) 160 middle) <|
    [ image 200 140 "/imgs/right-triangle.jpg"
    , image 150 150 "http://upload.wikimedia.org/wikipedia/commons/6/65/Pythag_anim.gif"
    ]

problems = [markdown|
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

<div style="height:50px">
The standard equation is
<img src="http://upload.wikimedia.org/math/3/a/e/3ae71ab3eb71d3d182a3b9e437fba6ee.png"
     style="width:100px; height:20px;"></img>
but to make it easier to *find x*, we can write it as 
<img src="http://upload.wikimedia.org/math/3/b/8/3b84a4234e90bf69db1029281d06e174.png"
     style="width:114px; height:21px;"></img>
</div>

Let’s write a function that takes an `a` and `b` and computes `c`.

```haskell
hypotenuse a b = ???

main = asText [hypotenuse 3 4, hypotenuse 5 12, hypotenuse 8 15]
-- the answer should be [5,13,17]
```

### Problem 2

Boolean values let us talk about things that are true and false. We can also
talk about truth when there are many conditions. Say we want to extend our `hello`
function above to deal with more people.

```haskell
-- isEnemy just checks for Sally. She is the enemy.
isEnemy name = name == "Sally"

-- isFriend is True if the given name is Steve OR Sally.
--   || means OR
isFriend name = name == "Steve" || name == "Sally"

-- isFrenemy is True if the person is a friend AND an enemy.
--   && means AND
isFrenemy name = isFriend name && isEnemy name
```

Using `||` and `&&` to mean *or* and *and* is kind of weird, but it is one
of the many historical things that is now so common that it is pretty much
impossible to change.

Your challenge is to write a function that tells you if the three
arguments are in increasing order.

```haskell
isIncreasing a b c = ???

main = asText [ isIncreasing 1 2 3
              , isIncreasing 1 4 9
              , isIncreasing 1 4 0 ]
```

The answer should be `[True,True,False]`.

### Problem 3

Remember the factorial problem? Okay, keep that in your mind...

Now we are going to mess around with [Fibonacci
Numbers](http://en.wikipedia.org/wiki/Fibonacci_number).
The Fibonacci Numbers are the following sequence of numbers:

<img src="http://upload.wikimedia.org/math/c/a/b/cabe91689f6a1af616ace02827c6e89c.png"
     style="width:402px; height:18px; display:block; margin-left:auto; margin-right:auto;"></img>

We arbitrarily say that the first two numbers are 0 and 1. From there, each number
is the sum of the previous two. We will give each of these numbers a name *F<sub>n</sub>*
where *n* indicates how many numbers in we are.

The mathier way to say this is like this:

<img src="http://upload.wikimedia.org/math/0/c/e/0cebc512d9a3ac497eda6f10203f792e.png"
     style="width:156px; height:18px; display:block; margin-left:auto; margin-right:auto; padding:10px;"></img>
<img src="http://upload.wikimedia.org/math/a/9/2/a92c5f0981136ba333124cdfe6d3c3ce.png"
     style="width:132px; height:18px; display:block; margin-left:auto; margin-right:auto; padding:10px;"></img>

Okay, so your challenge is to compute the *n<sup>th</sup>* Fibonacci number.

```haskell
fibonacci n = ???

main = asText [ fibonacci 0
              , fibonacci 1
              , fibonacci 2
              , fibonacci 3
              , fibonacci 4
              , fibonacci 5
              , fibonacci 6 ]
-- The result should be [0,1,1,2,3,5,8]
```

Once you are done, you can check out a [visualization of
the Fibonacci numbers](/edit/examples/Intermediate/FibonacciTiles.elm).

|]