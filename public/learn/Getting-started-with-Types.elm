
import Website.Skeleton (skeleton)
import Window

intro = [markdown|

<h1><div style="text-align:center">Getting started with Types
<div style="font-size:0.5em;font-weight:normal">*What are types? Why are they useful?*</div></div>
</h1>

<style type="text/css">
pre {
  background-color: white;
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

Types help you write correct programs. They come with tools that will warn you
about problems while you are writing the program, as opposed to when people
are already using it. They also force you to think a bit harder about how
you design your program, which will improve a code-base of any size.

We will cover [basic types](#what-is-a-type), [types for data
structures](#types-for-data-structures), and [types for
functions](#types-for-functions).

**To people new to types:** If you start feeling lost, do not fear!
It can take a while for types to really sink into your brain, and the
more you use them, the more natural they will become.

**To people with an imperative background:**
Types in Elm are not like types in C and Java. Elm has full type-inference,
meaning that all the types can be figured out and checked by the compiler
without *any* type annotations, making it much less onerous.

So give it a chance and do not get discouraged! Learning how types work is as an investment.
The reward is being able to write programs that &ldquo;just work&rdquo;.

Let&rsquo;s get started!

## What is a Type?

All values in Elm have a type. A number is one *type* of value, and a list is
a totally separate *type* of value.

For example, the number 42 is an integer. So to write down the type we say:

```haskell
42 : Int
```

This is read, &ldquo;42 has type `Int`&rdquo;. The term `Int` is the abbreviation
for integers. The same works for all types of primitive values:

```haskell
True    : Bool      -- True is a boolean value
3.1415  : Float     -- pi is a floating point number
'x'     : Char      -- 'x' is a character
"Alice" : String    -- "Alice" is a string of characters
```

You would read each entry as &ldquo;True has type Bool&rdquo;,
&ldquo;3.1415 has type float&rdquo;, etc. The basic types are
all slightly abbreviated, and this convention has become so widespread in
programming languages that there is no going back. It may be confusing
at first, but it turns out to be quite nice.

One thing to note, a floating point number is any number with a decimal point.
Its name reflects some of its technical details, but they are not super
important. The main thing to know is that a `Float` is not 100% accurate, at
some point it stops keeping track of very, very small fractions. It is also
important to know that an `Int` and a `Float` are not the same thing! They
are both numbers, but they are represented differently and have different
properties.

Types become more interesting and useful when you start working with more
complicated values. So let&rsquo;s take a look at types for data structures.

## Types for Data Structures

This section will cover lists, tuples, and [records][records]. It may help
to find some examples or read some documentation on these data structures
before continuing.

 [records]: /learn/Records.elm "Records in Elm"

### Lists

One of the most common data structures in Elm is the list. Lists can hold
many values, and those values must all have the same type. For example,

```haskell
numbers : [Int]
numbers = [1,2,3]
```

This could be read &ldquo;`numbers` has type list of ints.&rdquo; If you
wanted to represent a list of names:

```haskell
names : [String]
names = ["Alice", "Bob", "Chuck"]
```

Again, the key thing is that all elements of the list have exactly the
same type.

### Tuples

Tuples are another useful data structure. A tuple can hold a fixed number of
values, and each value can have any type. The most common use is for
representing a point:

```haskell
point : (Int,Int)
point = (3,4)
```

This pair of integers is the most basic tuple. Tuples are mainly for grouping
information, so you could use them to represent a book, holding the title,
author, and number of pages.

```haskell
book : (String,String,Int)
book = ("Demian","Hesse",176)
```

This illustrates that you can hold many different values, each with a different
type. When the data structure becomes more complicated or specific, 
it is often best to use records instead tuples.

### Records

Elm also has [records][records] which let you have more structured
values. Say you want to make a list of high quality books. We can put
them in a record that has a title, author, and number of pages:

```haskell
book1 : { title:String, author:String, pages:Int }
book1 = { title="Demian", author="Hesse", pages=176 }
```

We can use type aliases to make things a bit clearer.

```haskell
type Book = { title:String, author:String, pages:Int }

book2 : Book
book2 = { title="Magister Ludi", author="Hesse", pages=558 }

books : [Book]
books = [ book1, book2 ]
```

In the tuple version of the book, it was unclear from the type
which `String` was the title and which was the author. You would have to
read some code or do some experiment to figure it out. With records, it is
totally clear and extracting a title is as simple as saying `book2.title`.

## Types for Functions

So far we have only looked at unchanging values, but this is *functional*
programming! What about functions?!

One of the simplest functions is the boolean `not`. Here is how you would
implement it from scratch:

```haskell
not : Bool -> Bool
not b = if b then False else True
```

So `not` is a function that takes in a boolean and gives back a boolean.
This would be read as &ldquo;`not` has type `Bool` to `Bool`&rdquo;.

Another example is the `length` function which figures out the length of a list.

```haskell
length : [a] -> Int
length xs = case xs of
              hd::tl -> 1 + length tl
              []     -> 0
```

A lower case type is called a *type variable*. A type variable means
&ldquo;any type can go here&rdquo;. Since `length` never looks at any
of the values in the list, it can work on any kind of list!

People will say that `length` is a *polymorphic function*. If you pretend you
know greek, you can think of it as a function with &ldquo;many forms&rdquo;.
This is all just code words for a type that has *type variables* in it
(the lower case ones).

Functions can have multiple arguments. For example, the `drop` function
takes in a number of elements to drop and a list. It then returns the
shortened list. For example, it is true that
`(drop 2 [1,2,3,4] == [3,4])`. The type of `drop` is:

```haskell
drop : Int -> [a] -> [a]
```

When you see multiple arrows, you can think of a bunch of argument types
ending with the return type. So you can think of `drop` as a function that
takes two arguments and returns a list. Notice that we use the same type variable
for the input list `[a]` and the output list `[a]`. That means if you give
an integer list `[Int]`, you must get back an integer list `[Int]`!

### Higher-order Functions

There are some functions that take functions as arguments. One of the most
common examples of this is the `map` function. It applies a function to every
element of a list.

```haskell
map not [True,False]   -- returns [False,True]

map round [4.2,7.9]    -- returns [4,8]
```

In the first example, all of the boolean values in the list become *not* that
value. In the second example, all of the floating point numbers are rounded to
the nearest integer.

The `map` function has the following type:

```haskell
map : (a -> b) -> [a] -> [b]
```

The first argument is a function from `a`&rsquo;s to `b`&rsquo;s, the second
argument is a list of `a`&rsquo;s, and the return value is a transformed list
of `b`&rsquo;s.

The types of `not` and `round` are:

```haskell
not : Bool -> Bool
round : Float -> Int
```

The `map` function can be specialized for each case. When `map` is used
with `not`, it is specialized to have the type:

```haskell
-- map : (Bool -> Bool) -> [Bool] -> [Bool]

nots : [Bool] -> [Bool]
nots bools = map not bools
```

And when it is used with `round` it is specialized to have the type:

```haskell
-- map : (Float -> Int) -> [Float] -> [Int]

rounds : [Float] -> [Int]
rounds floats = map length floats
```

These are some of the &ldquo;many forms&rdquo; of the `map` function. The type
variables make `map` very flexible, but they also ensure that you cannot use
it in an undefined way. So the compiler would yell at you if you tried to do

```haskell
map not ["tree","france"]    -- Type Error!!!
```

Applying `not` to a string does not make any sense! So when you try to
specialize `map` to make this work, the type variable `a` ends up not matching!

```haskell
map : ( a   ->  b  ) -> [  a   ] -> [ b  ]
map : (Bool -> Bool) -> [String] -> [Bool]   -- Type Error!!!
```

Type variable `a` cannot be a `Bool` and a `String`! This is when types
begin to become extremely useful! For every expression you write, the type
checker is making sure that your code makes sense. In practice, that means
that the primary errors you will make will be logical errors: writing code
that is valid but it does not do what you intended. Many programmers find
that this kind of error is quite rare, so it is common that if it
compiles, it works!

## Wrap up

We have taken an extremely quick tour through types in Elm. Do not worry if
you got lost at any point, it usually takes a while for your brain to really
figure out what is going on here, especially with functions and polymorphism.
The best way to improve is to program more and to see more examples of types
and type errors.
|]

content w = width (min 600 w) intro

main = lift (skeleton content) Window.dimensions