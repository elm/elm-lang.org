# Core Language

This page will go through some of the basic values in Elm, including primitives,
data structures, and functions. It focuses on understanding their types, which
can be hard to learn just by example and has big benefits once you get into the
swing of things.


## Values

The basic building blocks of Elm are a set of primitive values that include
strings, booleans, and numbers. For example, the number 42 is an integer. So
to write down the type we say:

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


## Functions

So far we have only looked at unchanging values, but this is *functional*
programming! What about functions?!

One of the simplest functions is the boolean `not`. Here is how you would
implement it from scratch:

```haskell
not : Bool -> Bool
not boolean =
    if boolean then False else True
```

So `not` is a function that takes in a boolean and gives back a boolean.
This would be read as &ldquo;`not` has type `Bool` to `Bool`&rdquo;.

Another example is the `length` function which figures out the length of a list.

```haskell
length : List a -> Int
length list =
    case list of
      [] -> 0
      first :: rest -> 1 + length rest
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
drop : Int -> List a -> List a
```

When you see multiple arrows, you can think of a bunch of argument types
ending with the return type. So you can think of `drop` as a function that
takes two arguments and returns a list. Notice that we use the same type variable
for the input list `List a` and the output list `List a`. That means if you give
an integer list `List Int`, you must get back an integer list `List Int`!

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
map : (a -> b) -> List a -> List b
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
-- map : (Bool -> Bool) -> List Bool -> List Bool

nots : List Bool -> List Bool
nots bools =
    map not bools
```

And when it is used with `round` it is specialized to have the type:

```haskell
-- map : (Float -> Int) -> List Float -> List Int

rounds : List Float -> List Int
rounds floats =
    map round floats
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
map : ( a   ->  b  ) -> List a      -> List b
map : (Bool -> Bool) -> List String -> List Bool   -- Error!!!
```

Type variable `a` cannot be a `Bool` and a `String`! This is when types
begin to become extremely useful! For every expression you write, the type
checker is making sure that your code makes sense. In practice, that means
that the primary errors you will make will be logical errors: writing code
that is valid but it does not do what you intended. Many programmers find
that this kind of error is quite rare, so it is common that if it
compiles, it works!


## If

## Let
