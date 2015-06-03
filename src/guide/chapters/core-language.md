
# Core Language

This section will walk you through Elm's simple core language. The aim is to build a strong understanding of the basics so you have a good foundation when you start working with graphics and interactivity.

To follow along [get everything installed](/install) and start up `elm repl` in the terminal. It should look like this:

```haskell
Elm REPL 0.4.1 (Elm Platform 0.15)
  See usage examples at <https://github.com/elm-lang/elm-repl>
  Type :help for help, :exit to exit
>
```

> **Note:** The REPL will print out the type of the result, but for the sake of introducing concepts gradually, we will elide the types in the REPL examples in this section.

We will cover [values](#values), [functions](#functions), [lists](#lists), [tuples](#tuples), and [records](#records) which all correspond pretty closely with structures in languages like JavaScript, Python, and Java.


## Values

Lets get started with some math. Here is some addition, subtraction, and multiplication.

```haskell
> 2 + 2
4

> 99 - 1
98

> 2 * 3
6
```

It looks just like what you would type into a calculator.

When you do more complex math, it follows the normal order of operations. You can also add parentheses to clarify the order.

```haskell
> 2 + 3 * 4
14

> (2 + 3) * 4
20
```

Elm makes a distinction between integers and floating point numbers. You can think of it as a distinction between whole numbers and fractions. This is particularly important for division. Elm has floating point division which works on fractions and produces fractions.

```haskell
> 9 / 2
4.5
```

Seems pretty normal! Elm also has integer division which always results in an integer. If there is a remainder, it gets thrown away.

```haskell
> 9 // 2
4
```

Okay, so working with numbers is pretty natural. Strings look pretty similar:

```haskell
> "hello"
"hello"

> "hello" ++ "world"
"helloworld"

> "hello" ++ " world"
"hello world"
```

Elm uses the `(++)` operator to put strings together. Notice that both strings are preserved exactly as is when they are put together so when we combine `"hello"` and `"world"` the result has no spaces!


## Functions

Functions are a way to make reusable chunks of code. One of the coolest thing about functions in Elm is that you can think of them as simple find-and-replace operations. Just substitute in the body of the function and everything will work out! Lets start with a function that doubles numbers.

```haskell
> double n = n + n
<function>

> double 4
8

> double (5 + 1)
12
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
<function>

> isNegative 4
False

> isNegative -7
True

> isNegative (double -3)
True
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
<function>

> average 10 20
15

> average (1 + 1) (10 - 7)
2.5

> isNegative (average (double -2) 2)
True
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
"hello"

> if False then "hello" else "world"
"world"
```

The keywords `if` `then` `else` are used to separate the conditional and the two branches so we do not need any parentheses or curly braces. Now lets make a function that tells us if a number is over 9000.

```haskell
> over9000 powerLevel = \\
|   if powerLevel > 9000 then "It's over 9000!!!" else "meh"
<function>

> over9000 42
"meh"

> over9000 100000
"It's over 9000!!!"
```

> **Note:** Using a backslash in the REPL lets us split things on to multiple lines. We use this in the definition of `over9000` above. Furthermore, it is best practice to always bring the body of a function down a line. It makes things a lot more uniform and easy to read, so you want to do this with all the functions and values you define.


## Lists

Lists are one of the most common data structures in Elm. Lists generally fill the same role as arrays in languages like JavaScript or Java, for holding a sequence of related things.

Lists can hold many values, and those values must all have the same type. Here are a few examples that use functions from [the `List` library][list]:

[list]: http://package.elm-lang.org/packages/elm-lang/core/latest/List

```haskell
> names = [ "Alice", "Bob", "Chuck" ]
["Alice","Bob","Chuck"]

> List.isEmpty names
False

> List.length names
3

> List.reverse names
["Chuck","Bob","Alice"]

> numbers = [1,4,3,2]
[1,4,3,2]

> List.sort numbers
[1,2,3,4]

> List.map double numbers
[2,8,6,4]
```

Again, the key thing is that all elements of the list have exactly the same type.


## Tuples

Tuples are another useful data structure. A tuple can hold a fixed number of values, and each value can have any type. The most common use is for representing a 2D point:

```haskell
> (3,4)
( 3, 4 )

> distance (a,b) (x,y) = \\
|   sqrt ( (a-x)^2 + (b-y)^2 )
<function>

> distance (0,0) (0,3)
3

> distance (0,0) (3,4)
5
```

Working with pairs of numbers is the most common case, but tuples are generally useful for grouping information. For example, you can use them to represent a book, holding the title, author, and number of pages.

```haskell
> ( "Demian", "Hesse", 176 )
("Demian","Hesse",176)

> getTitle (title, author, pages) = title
<function>
```

This illustrates that you can hold many different values, each with a different type. But when the data structure starts becoming more complicated, it is often best to use records instead tuples.


## Records

A records is a set of key-value pairs, similar to objects in JavaScript or Python. You will find that they are extremely common and useful in Elm! Lets see some basic examples.


```haskell
> point = { x = 3, y = 4 }
{ x = 3, y = 4 }

> point.x
3

> bill = { name = "Gates", age = 57 }
{ age = 57, name = "Gates" }

> bill.name
"Gates"
```

So we can create records using curly braces and access fields using a dot. Elm also has a version of record access that works like a function. By starting the variable with a dot, you are saying please access the field with the following name, so `.name` accesses the `name` field of the record.

```haskell
> .name bill
"Gates"

> List.map .name [bill,bill,bill]
["Gates","Gates","Gates"]
```

When it comes to making functions with records, you can do some pattern matching to make things a bit lighter.

```haskell
> under70 {age} = age < 70
<function> 

> under70 bill
True

> under70 { species = "Triceratops", age = 68000000 }
False
```

So we can pass any record in as long is it has an `age` field that holds a number.

It is often useful to update the values in a record.

```haskell
> { point | x <- 1 }
{ x = 1, y = 0 }

> { point | y <- 4 }
{ x = 0, y = 4 }

> { bill | name <- "Nye" }
{ age = 56, name = "Nye" }
```

You can update as many fields as you want, separating each update by a comma. It is important to notice that we do not make *destructive* updates. In other words, when we update some fields of `point` we actually create a new record rather than overwriting the existing one. Elm makes this efficient by sharing as much content as possible. If you update one of ten fields, the new record will share all of the nine unchanged values.


### Comparing Records and Objects

Records in Elm are *similar* to objects in JavaScript, but there are some crucial differences. The major differences are that with records:

- You cannot ask for a field that does not exist.
- No field will ever be undefined or null.
- You cannot create recursive records with a `this` or `self` keyword.

Elm encourages a strict separation of data and logic, and the ability to say `this` is primarily used to break this separation. This is a systematic problem in Object Oriented languages that Elm is purposely avoiding.

Records also support [structural typing][st] which means records in Elm can be used in any situation as long as the necessary fields exist. This gives us flexibility without compromising reliability.

 [st]: http://en.wikipedia.org/wiki/Structural_type_system "Structural Types"

