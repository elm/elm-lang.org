import Website.Skeleton (skeleton)
import Window

main = lift (skeleton content) Window.width

content w = width (min 600 w) words

words = [markdown|

<style type="text/css">
h3 { padding-top: 1em; }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: #f8f8f8; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: #f8f8f8; }
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

# Programming!
This introduction assumes no prior knowledge of programming.
It will make you more badass.

All of the following programs can be typed into
[this editor](http://elm-lang.org/try), and you can
play with them online.

Note: capitalization matters! Be careful!

### Hello World
Every coder’s first program is “Hello World!”. Welcome to the club!

```haskell
main = asText "Hello World!"
```

`main` is what gets shown on screen. Accept this. `asText` will turn anything
into text that can be shown on screen.


### Numbers

You can also do simple mathematics.

```haskell
main = asText 42
```

Addition just uses the plus sign.

```haskell
main = asText (2 + 2)
```

This is just like using a graphing calculator or doing math on paper.
Note that `*` means multiply, `/` means divide, and `^` means “to the power of”

This is 3 times 4 over 2

```haskell
main = asText (3 * 4 / 2)
```

Here we compute (32 + 42)

```haskell
main = asText (3^2 + 4^2)
```

### Strings

Strings are sequences of letters. We can put them together with the `++` operator.

```haskell
main = asText ("Hello " ++ "Steve!")
```

### Lists
Lists are just a sequence of things. All the things must be the same type
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
main = asText (if 9500 < 9000 then "meh" else "It's over 9000!!!")
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
main = asText (if name == "Steve" then "Anyad!"
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
    if name == "Steve" then "Anyad!"
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

Here is a very simple function that just increment numbers by one.

```haskell
increment n = n + 1
main = asText [ increment 3, increment 4, increment 41 ]
```

Maybe you are into the Pythagorean theorem?

```haskell
hypotenuse a b = sqrt (a^2 + b^2)
main = asText [ hypotenuse 3 4, hypotenuse 5 12 ]
```

Think about how much you’d have to repeat yourself to say these things
without functions!

### Functions that depend on themselves!

Remember the definition of n factorial (*n!*) from school?
It is the product of all numbers between 1 and *n*. In math talk:

n! = n * (n-1)!
0! = 1

When you turn this into code, it looks very very similar!

```haskell
factorial n =
    if n == 0 then 1 else n * factorial (n-1)

main = asText (factorial 3)
```

### Map

You can use map to use a function many times. Remember the functions we
defined above? Let’s use them many times.

```haskell
main = asText (map hello ["Sally", "Steve", "Tom"])

main = asText (map react [100, 1000, 10000])

main = asText (map increment [1,2,3,4])

main = asText (map factorial [1,2,3,4])
```
|]