
# Core Language

This section will walk you through Elm's simple core language. The aim is to build a strong understanding of the basics so you have a good foundation when you start working with graphics and interactivity.

To follow along [get everything installed](/install) and start up `elm repl` in the terminal. It should look like this:

```haskell
Elm REPL 0.4.1 (Elm Platform 0.15)
  See usage examples at <https://github.com/elm-lang/elm-repl>
  Type :help for help, :exit to exit
>
```

We will cover [values](#values), [functions](#functions), [lists](#lists), [tuples](#tuples), and [records](#records) which all correspond pretty closely with structures in languages like JavaScript, Python, and Java.


## Values

Lets get started with some math. Here is some addition, subtraction, and multiplication.

```haskell
> 2 + 2
4 : number

> 99 - 1
98 : number

> 2 * 3
6 : number
```

It looks just like what you would type into a calculator.

> **Note:** The REPL is printing out the result of our input along with a *type* that tells us what type of value we are looking at. In this case, our results are all numbers. You can read the colon as &ldquo;has type&rdquo; so the result `(4 : number)` is pronounced &ldquo;four has type number&rdquo;. We will get into types more at a later time, so don't worry about this too much right now!

When you do more complex math, it follows the normal order of operations. You can also add parentheses to clarify the order.

```haskell
> 2 + 3 * 4
14 : number

> (2 + 3) * 4
20 : number
```

Elm makes a distinction between integers and floating point numbers. You can think of it as a distinction between whole numbers and fractions. This is particularly important for division. Elm has floating point division which works on fractions and produces fractions.

```haskell
> 9 / 2
4.5 : Float
```

Seems pretty normal! Elm also has integer division which always results in an integer. If there is a remainder, it gets thrown away.

```haskell
> 9 // 2
4 : Int
```

Okay, so working with numbers is pretty natural. Strings look pretty similar:

```haskell
> "hello"
"hello" : String

> "hello" ++ "world"
"helloworld" : String

> "hello" ++ " world"
"hello world" : String
```

Elm uses the `(++)` operator to put strings together. Notice that both strings are preserved exactly as is when they are put together so when we combine `"hello"` and `"world"` the result has no spaces!


## Functions

Functions are a way to make reusable chunks of code. One of the coolest thing about functions in Elm is that you can think of them as simple find-and-replace operations. Just substitute in the body of the function and everything will work out! Lets start with a function that doubles numbers.

```haskell
> double n = n + n
<function> : number -> number

> double 4
8 : number

> double (5 + 1)
12 : number
```

We defined a function `double` and used it a few times. Whenever we see `double` we can think of it as a find-and-replace operation, so when we evaluate `(double 4)` it happens in the following three steps.

  * `double 4`
  * `4 + 4`
  * `8`

We start with `(double 4)` and then swap in the meaning of `(double n)` by replacing all occurances of `n` with `4`. This gives us our second step `(4 + 4)`. From here it is just some addition to get to `8`, the third and final step of evaluation.

The same process happens in more complex expressions like `(double (5 + 1))`. For that expression, evaluation takes the following steps:

  * `double (5 + 1)`
  * `double 6`
  * `6 + 6`
  * `12`

Notice that we turn `(5 + 1)` into `6` *before* doing the find-and-replace with `double`. This is really important and can save us a lot of work if `n` is used multiple times. Lets see how evaluation would proceed if we did the find-and-replace sooner:

  * `double (5 + 1)`
  * `(5 + 1) + (5 + 1)`
  * `6 + (5 + 1)`
  * `6 + 6`
  * `12`

Notice that we have to evaluate `(5 + 1)` twice. It is always going to be `6` so we are wasting our time figuring it out again! This is especially important as we start doing more complex stuff. `(5 + 1)` only takes one step, but imagine something that takes a thousand steps, or a million! At that scale, doing things twice is pretty wasteful.

> **Note:** For people with a background in languages like Java or JavaScript or Python, you will notice that function application looks different in Elm. Instead of wrapping all arguments in parentheses and separating them with commas, we use spaces to apply the function. So `(add(3,4))` becomes `(add 3 4)` which ends up avoiding a bunch of parens and commas as things get bigger. Ultimately, this looks much cleaner once you get used to it!

Okay, now that we have seen a basic function and learned how it gets evaluated, lets define a few more functions for practice. This next function figures out if a number is negative or not.

```haskell
> isNegative n = n < 0
<function> : number -> Bool

> isNegative 4
False : Bool

> isNegative -7
True : Bool

> isNegative (double -3)
True : Bool
```

The `isNegative` function takes in some number and checks if it is less than zero. This will result in `True` or `False` which are called boolean values, or `Bool` for short.

Again, it is just like defining a find-and-replace rule, so the evaluation of the fanciest case goes through the following steps:

  * `isNegative (double -3)`
  * `isNegative (-3 + -3)`
  * `isNegative -6`
  * `-6 < 0`
  * `True`

Lets try a slightly fancier function that finds the average of two numbers.

```haskell
> average a b = (a + b) / 2
<function> : Float -> Float -> Float

> average 10 20
15 : Float

> average (1 + 1) (10 - 7)
2.5 : Float

> isNegative (average (double -2) 2)
True : Bool
```

The `average` function takes two arguments, otherwise it is exactly the same as the functions we have seen before. Lets walk through the evaluation steps for the trickier use of `average`.

  * `average (1 + 1) (10 - 7)`
  * `average 2 (10 - 7)`
  * `average 2 3`
  * `(2 + 3) / 2`
  * `5 / 2`
  * `2.5`

Again, we fully evaluate the arguments before doing the find-and-replace. The evaluation process follows the same rules when we are using lots of functions together, as in the fanciest use of `average`.

  * `isNegative (average (double -2) 2)`
  * `isNegative (average (-2 + -2) 2)`
  * `isNegative (average -4 2)`
  * `isNegative ((-4 + 2) / 2)`
  * `isNegative (-2 / 2)`
  * `isNegative -1`
  * `-1 < 0`
  * `True`

Hopefully showing the evaluation order has helped clarify exactly how to think about a snippet of Elm code! As we start looking at more and more complex code, it can be helpful to go through these steps to make sure you know exactly what is going on.

So the key takeaways from this section are that:

  1. Elm functions are like find-and-replace operations.
  2. All expressions have a simple evaluation order that minimizes work.

Now that we have defined a few functions and learned how they work, lets introduce some more language constructs.


## If Expressions

When you want to do have conditional behavior in Elm, you use an if-expression.

```haskell
> if True then "hello" else "world"
"hello" : String

> if False then "hello" else "world"
"world" : String
```

The keywords `if` `then` `else` are used to separate the conditional and the two branches so we do not need any parentheses or curly braces. Now lets make a function that tells us if a number is over 9000.

```haskell
> over9000 powerLevel = \\
|   if powerLevel > 9000 then "It's over 9000!!!" else "meh"
<function> : number -> String

> over9000 42
"meh" : String

> over9000 100000
"It's over 9000!!!" : String
```

> **Note:** Using a backslash in the REPL lets us split things on to multiple lines. We use this in the definition of `over9000` above. Furthermore, it is best practice to always bring the body of a function down a line. It makes things a lot more uniform and easy to read, so you want to do this with all the functions and values you define.


## Lists

Lists are one of the most common data structures in Elm. Lists generally fill the same role as arrays in languages like JavaScript or Java, for holding a sequence of related things.

Lists can hold many values, and those values must all have the same type. Here are a few examples that use functions from [the `List` library][list]:

[list]: http://package.elm-lang.org/packages/elm-lang/core/latest/List

```haskell
> names = [ "Alice", "Bob", "Chuck" ]
["Alice","Bob","Chuck"] : List String

> List.isEmpty names
False : Bool

> List.length names
3 : Int

> List.reverse names
["Chuck","Bob","Alice"] : List String

> numbers = [1,4,3,2]
[1,4,3,2] : List number

> List.sort numbers
[1,2,3,4] : List number

> List.map double numbers
[2,8,6,4] : List number
```

Again, the key thing is that all elements of the list have exactly the same type.


## Tuples

Tuples are another useful data structure. A tuple can hold a fixed number of values, and each value can have any type. The most common use is for representing a point:

```haskell
> ( 3.0, 4.0 )
( 3.0, 4.0 ) : (Float, Float)
```

This pair of integers is the most basic tuple. Tuples are mainly for grouping information, so you could use them to represent a book, holding the title, author, and number of pages.

```haskell
book : (String,String,Int)
book =
  ("Demian","Hesse",176)
```

This illustrates that you can hold many different values, each with a different type. When the data structure becomes more complicated or specific, it is often best to use records instead tuples.


## Records

Elm also has [records][records] which let you have more structured values. Say you want to make a list of high quality books. We can put them in a record that has a title, author, and number of pages:

```haskell
book1 : { title : String, author : String, pages : Int }
book1 =
  { title = "Demian"
  , author = "Hesse"
  , pages = 176
  }
```

We can use type aliases to make things a bit clearer.

```haskell
type alias Book =
    { title : String
    , author : String
    , pages : Int
    }


book2 : Book
book2 =
  { title = "Magister Ludi"
  , author = "Hesse"
  , pages = 558
  }


books : List Book
books =
  [ book1, book2 ]
```

In the tuple version of the book, it was unclear from the type which `String` was the title and which was the author. You would have to read some code or do some experiment to figure it out. With records, it is totally clear and extracting a title is as simple as saying `book2.title`.


