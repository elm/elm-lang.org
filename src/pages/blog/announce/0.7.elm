import Blog
import Center


main =
  Blog.blog
    "Elm 0.7"
    "Extensible Records"
    Blog.evan
    (Blog.Date 2013 1 4)
    [ Center.markdown "600px" content ]


content = """

Major changes in this release include:

 - Polymorphic Extensible Records!
 - Nicer syntax for `lift`, `lift2`, etc.
 - Guarded definitions have been replaced by multi-way-if expressions
 - Line and column numbers for all errors!

I am very excited about these changes! [Records](#records) are
a very important addition, so they will be the major focus
of this post. Nonetheless, the new syntax for `lift` is quite nice too,
so be sure to take a look at that too!

Multi-way-ifs are a nice way to deal with very conditional code in any
situation. They replace guarded definitions because they are just as
flexible and can be used in strictly more situations. This is a breaking
change, so read that section to see the simple fix!

This website has undergone some changes too. You can now compile with Ctrl-Enter
and you can turn on automatic compilation which will recompile by itself. Check
out the [Try Elm](/try) page to see these features in action. Big
thanks to [madscoaducom](https://github.com/madscoaducom) for adding this!

<h2 id="records">Records</h2>

Elm now supports records. Records are a labeled data structures
that permit lightweight representations of complex data.

Elm also supports [structural typing][st] meaning that if a function
requires a record with an `x` and `y` field, it will work with *any*
record that has those fields (2D points, 3D points, spaceships, etc.).

Elm&rsquo;s records are also extensible, meaning that you can add and remove
fields. This makes it easy to rename fields and update their values.

Finally, Elm&rsquo;s records permit polymorphic functions. This gives
them expressivity similar to first-class modules and typeclasses. More
on this later!

This overall approach is called [polymorphic extensible records with scoped
labels][records], and the academic paper I just linked is a relatively accessible
and enjoyable read. I highly recommend reading it!

The following is a brief overview and some notes on the expressiveness of
Elm&rsquo;s records. If you just want an overview of the syntax,
[see here][syntax], and for a comprehensive introduction to records in
Elm, [see here][overview].

 [syntax]: /docs/syntax#records "Record Syntax"
 [overview]: /docs/records "Overview of Records"
 [st]: http://en.wikipedia.org/wiki/Structural_type_system "Structural Types"
 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"

#### A Brief Overview

The following shows creation, access, restriction, and extension in that order.
The comments show the result of the expression:

```elm
point2 = { x = 0, y = 0 }
point3 = { x = 3, y = 4, z = 12 }
book   = { title = "Steppenwolf", author = "Hesse" }
group  = { add a b = a + b, zero = 0 }

book.title              -- "Steppenwolf"
.title book             -- "Steppenwolf"

{ book - title }        -- { author = "Hesse" }
{  point3 - z  }        -- {  x = 3,  y = 4   }

{ point2 | z = 0 }      -- { x = 0, y = 0, z = 0 }
{ book | pages = 237 }  -- { title  = "Steppenwolf"
                        -- , author = "Hesse"
                        -- , pages  = 237 }
```

With restriction and extension it is possible to rename fields and update values.
There is also a special short-hand for updating fields that lets you do
updates in bulk:

```elm
{ book - title | name = book.title }    -- rename
{ book - title | title = "Demian" }     -- update

{ point2 | x <- 1 }            -- { x = 1, y = 0 }
{ point3 | x <- 7, y <- 9 }    -- { x = 7, y = 9, z = 12 }
{ book | title <- "Demian" }   -- { title  = "Demian"
                               -- , author = "Hesse" }
```

You can pattern match on records in function definitions and in let-expressions.
You can alse use special field accessors. A field must be present if you use it,
and it is fine if there are additional fields.

```elm
dist {x,y} = sqrt (x^2 + y^2)

dist point3 == 5

demian  = { book | title <- "Demian" }
gertrud = { book | title <- "Gertrud" }
titles  = map .title [ book, demain, gertrud ]
```

It is fine to call `dist` on `point2`, `point3`, and anything else that has
an `x` and `y` field. And the value of `titles` is
`["Steppenwolf","Demian","Gertrud"]`. You can also pattern match in let expressions
and lambdas.

```elm
\\{x,y} -> (x,y)
let {author} = book in ...
```

As a result, it is no longer possible to use the `{;;}` syntax to separate definitions
in let expressions. This is a breaking change that may require some minor refactoring,
but I think it will make code nicer overall.

Again, I highly recommend reading [this paper][records] and [this overview][over]
to get a more complete understanding of records in Elm.

 [over]: /docs/records "Extensible Records"
 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"

#### Notes on Expressiveness

Polymorphic extensible records give some of the power of first-class
modules in OCaml and SML. In fact, this should be nearly equivalent
to first-class modules, but without the ability to `open` or `import`
a record into the global namespace. There may be other differences, but
I am not sure about them.

These records also make it possible to create a more labor intensive
version of typeclasses. One way to implement typeclasses is to create
a record for each instance holding all of the necessary values. The difference
is that with compiler support, these records can be created and passed around
automatically by the compiler, making them very lightweight and convenient.
I think typeclasses can also make stronger guarantees about never using
conflicting records for a particular instance.

Part of why I am dragging my feet on adding typeclasses to Elm is because
records, first-class modules, and typeclasses do a lot of the same things
([records and modules][records] and [modules and typeclasses][same]).
There have been one or two proposals to [unify first-class modules and
typeclasses][unify] as well. I want to make sure Elm is getting the best
of all of these features, so I have been doing a lot of research to
make sure I do not make the wrong choices here.

Extensible records seem to be a fairly straightforward win regardless of how the
typeclasses vs. first-class modules decision turns out. They are more flexible
than the record systems in either Haskell or ML, and having a record system
seems fairly non-controversial, even though there is some overlap.

As a side note, extensible records are nearly equivalent to Object Oriented
programming. Objects can be represented as a recursive record system! I
specifically left out a keyword like `this` or `self` that would permit this
sort of recursion though. I consider it extremely bad practice to mix your data
and your logic, and as far as I can tell, that is the sole purpose of `this`
and `self`. For more on this connection, check out [this][a].

 [a]: /docs/records#comparison-of-records-and-objects
 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"
 [same]: http://www.cse.unsw.edu.au/~chak/papers/modules-classes.pdf "Constructive Comparison"
 [unify]: http://www.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf "Modular Type Classes"

## Do you even lift?

Up until now the sole way of applying a pure function to time-varying values was
with the `lift` function and its numbered relatives `lift2`, `lift3`, etc. This
release introduces two new infix operators `(<~)` and `(~)` which make this
a bit lighter visually.

The `(<~)` operator is exactly equivalent to `lift`.

```elm
(<~) : (a -> b) -> Signal a -> Signal b

f <~ s = lift f s
```

The `(~)` operator allows you to apply a signal of functions to a signal of values.

```elm
(~) : Signal (a -> b) -> Signal a -> Signal b

sf ~ s = lift2 (\\f x -> f x) sf s
```

This allows you to put together many signals quite easily. The following
pairs of expressions are equivalent:

```elm
lift asText Mouse.position
asText <~ Mouse.position

lift2 scene signal1 signal2
scene <~ signal1 ~ signal2


values =
  (,,)
      <~ count (every second)
      ~ sampleOn Mouse.clicks Mouse.position
      ~ delay second (count Mouse.clicks)

values' =
  lift3 (,,)
      (count (every second))
      (sampleOn Mouse.clicks Mouse.position)
      (delay second (count Mouse.clicks))
```

I do not really advocate switching over to `(<~)` and `(~)` entirely.
They are nice when used in the appropriate situation, but can be more
confusing in some cases, especially when dealing with many signals.
So use them carefully and judiciously! Nifty syntax should not trump
readability.

For Haskell people, this is the same as `(<$>)` and `(<*>)` for applicative
functors with the added benefit of not being extremely hideous.

## Multi-way If

The multi-way if is simply a nicer way to write nested if expressions. So the
following expression can be used anywhere, not just in definitions.

```elm
if | key == 40 -> n + 1
   | key == 38 -> n - 1
   | otherwise -> n
```

This means that any existing guarded definitions will cease to work in their
current form. Definitions will have to be converted as follows:

```elm
step key n
    | key == 40 = n + 1
    | key == 38 = n - 1
    | otherwise = n

step' key n =
 if | key == 40 -> n + 1
    | key == 38 -> n - 1
    | otherwise -> n
```

You just have to add an equals sign (just like in every other definition),
the `if` keyword, and convert the equals signs into arrows.

The motivation for this change was that I have always been frustrated by this
kind of syntax in Haskell. There are now three different ways to do multi-way ifs
in Haskell (guarded definitions, guarded patterns, and multi-way ifs) and the
syntax between them is non-uniform, some using `=` and others using `->` to
separate conditionals from their body. There is some logic to the different
syntax choices, but I generally find it frustrating to have to think about this.

I also found it frustrating that some definitions could end with an equals sign
and others with a bunch of guarded definitions. For some reason this always felt
wrong to me.

These are more subjective things, but after seeing that all of these cases can
be reduced to the multi-way if, I decided to go for that. It gets rid of the
things I found non-uniform and frustrating without losing expressiveness.

## Better Error Messages

Finally, Elm provides a line and column number for type errors and runtime errors
having to do with non-exhaustive pattern matches and non-exhaustive multi-way
if-expressions. Yay!

## Wrap-up

Thanks to Dobes for taking a look at the record system early on and finding
a bunch of bugs! Thanks to John for pushing for a nicer way to `lift`!

We also have been discussing a nice way to construct records. See
[this discussion][github] if you would like to see what we have been
thinking or if you want to make suggestions.

Also, I am scheduled to speak at the [mloc.js](http://mloc-js.com/)
conference in Budapest in mid February, so maybe check it out
if you are in the neighborhood!

 [github]: https://github.com/elm-lang/Elm/issues/73 "Record Constructors"

"""
