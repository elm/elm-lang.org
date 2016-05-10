import Blog
import Center


main =
  Blog.blog
    "Elm 0.9"
    "Fast and reliable static checks"
    Blog.evan
    (Blog.Date 2013 8 13)
    [ Center.markdown "600px" content ]


content = """

Before this release, my primary priority was: prove that
FRP is pleasant and practical. If FRP is not the right way, it does
not matter how good or bad the type checker is.

I recently started to feel that poor error messages had become
the primary barrier for Elm. Questions started to shift from
&ldquo;is this possible with FRP?&rdquo; to &ldquo;I am
doing this with FRP, how can the tools be better?&rdquo;
This is a very positive sign!

To begin improving tools for FRP and Elm, the type checker has been
completely rewritten. The key improvements are:

  * Undefined values are errors
  * All type errors are caught and reported
  * Error messages are more specific and easier to read
  * It&rsquo;s fast

This is a huge step forward. It also creates a solid foundation
for further improvement.

This release also introduces many frequently requested syntax improvements. The
most notable are as follows:

  * [Unary negation](#unary-negation)
  * [Pattern matching on literals and `as` patterns](#pattern-matching)
  * [Multi-line strings](#multi-line-strings)
  * [Record constructors](#record-constructors)

Finally, there are a bunch of miscellaneous improvements:

  * `elm-server` can serve multi-module projects
  * [Detect mouse hover][hover]
  * [Transparency in collages][alpha] and many bug fixes
  * [`Text.height`][height] use pixels instead of [ems][], after [much debate][]

  [hover]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Graphics-Input#hoverable
  [alpha]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Graphics-Collage#alpha
  [height]: http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Text#height
  [much debate]: https://groups.google.com/forum/?fromgroups#!searchin/elm-discuss/specifying$20size$20of$20text/elm-discuss/3Iz-HpV1QRg/oHPoqWDgrmEJ
  [ems]: http://en.wikipedia.org/wiki/Em_(typography)

The rest of this post will try to cover [all of the changes][changes] in more detail.
Maybe start the [download process][download] while you are reading!

  [download]: https://github.com/elm-lang/Elm/blob/master/README.md#install
  [changes]: https://github.com/elm-lang/Elm/blob/master/changelog.txt

<br/>

## Type checker and Build Improvements

This was a really big project, so I want to first thank Prezi
for making all of this work possible! Also, thank you to Spiros and
Laszlo for talking through issues with me as they came up; this was
a huge help!

A big part of improving the type checker was making it possible for information
to flow between modules. This meant improving the build system.

Elm now creates two directories when compiling:

  * The `cache/` directory holds intermediate files. These hold metadata
    about types, type aliases, fixity of infix ops, etc. This information
    is used to pass information between modules and to build the final result.
    It also caches information from previous compilation passes, so if a file
    is unchanged, it does not need to be recompiled. This speeds up the build.

  * The `build/` directory holds only the generated `.html` and `.js` files,
    things you want to run and distribute. This directory should match the
    directory structure of your project, making it easy to export these
    files and start serving them.

You can use the `--cache-dir` and `--build-dir` flags to change the name
or location of these two directories. We [discussed a bunch of naming options][dirs]
for them, but ultimately, maybe you still know better :P

  [dirs]: https://groups.google.com/forum/?fromgroups#!topic/elm-discuss/bkEEN1P5f9U

These improvements make it possible to allow user-defined fixity and associativity
for infix operators, so that should be arriving in a future release.

#### Notes on the Type Checker

With this build infrastructure in place, cross-module type checking became
possible. The next task was understand and implement an efficienct type inference
algorithm.

To summarize, it is very tricky and there are few resources available that even
*discuss* efficient practical implementations, let alone give details on specific
techniques and strategies. I am planning to do a &ldquo;field guide to efficient
type inference&rdquo; to make it easier for others to create a type inferencer
that is correct and *fast*.

For now, take a look at [this chapter][] of Advanced Topics in Types and Programming
Languages. It is one of the best resources I have found.

  [this chapter]: http://www.cs.cmu.edu/~rwh/courses/refinements/papers/PottierRemy04/hmx.pdf

## Syntax Improvements

<h4 id="pattern-matching">Pattern Matching</h4>

You can now pattern match on literals like numbers, strings, and booleans.

```elm
removeZeros numbers =
  case numbers of
    []        -> numbers
    0 :: rest -> removeZeros rest
    n :: rest -> n :: removeZeros rest

isOrigin pos =
  case pos of
    (0,0) -> True
    _     -> False
```

You can also use `as` patterns now, thanks to Andrew!

```elm
move time ({x,y,vx,vy} as object) =
  { object | x <- x + vx * time
           , y <- y + vy * time }
```

This lets you name a structure and match on its sub-structure.
The `as` keyword binds very loosely, so it often needs parentheses.
One example might be:

```elm
data World = World Mario [Goomba] [Brick]

step input (World mario goombas bricks as world) = ...
```

<h4 id="unary-negation">Unary negation</h4>

I was very hesitant to add this feature because I had not seen a
statically-typed functional language that I felt got this right.

You get questions like: is `(x -1)` subtraction? Is `(f -1)` function
application with a negative argument? From the perspective of the parser
they are exactly the same, even though we can figure it out based on context.

Writing `(x -1)` to mean subtraction is not recommended and is considered
sloppy, whereas `(f -1)` is definitely going to come up quite quickly
([it does in OCaml][ocaml]). After [discussing many options][negate],
we decided to optimize for function application.

 [ocaml]: http://stackoverflow.com/questions/8984661/unary-minus-and-floating-point-number-in-ocaml
 [negate]: https://groups.google.com/forum/?fromgroups#!searchin/elm-discuss/negation/elm-discuss/DcvoUKPzM_M/KIogCVoL9G0J

In Elm, any unary negation operator must meet **both** of these requirements:

  1. It is preceded by whitespace or `(` or `[` or `,`
  2. It is *not* followed by whitespace

Where whitespace means spaces, newlines, and comments.
The following expressions show many cases you might encounter
in the wild:

```elm
n - 1         -- subtraction
n-1           -- subtraction, breaks requirement 1
-10           -- negative ten
- 10          -- parse error, breaks requirement 2
( -1, -1 )    -- point in quadrant III
[-1,-1,-1]    -- a list of negative ones
abs -1        -- abs (-1)
max -2 -4     -- max (-2) (-4)
h - 3 - 5     -- subtraction twice
n - -1        -- n + 1
```

Another way to describe these rules is &ldquo;unary negation binds tighter than function
application and infix operations.&rdquo;

In practice, I have found that this is how my brain parses code. I definitely read
`(max -2 -4)` as a function, my brain blocks each syntactic unit into a semantic unit.

This is similar to how `(.)` can mean many different things depending on spacing. It is
unfortunate to overload, but I think this is the best solution given the constraints.

<h4 id="multi-line-strings">Multi-line Strings</h4>

Just like Python, you can use multi-line strings if you use the triple-quote.
This will make it easier to embed plain-text or JSON if the text uses `"`.

```elm
json = \"\"\"
{
  "title" : "Narcissus and Goldmund",
  "author": "Hermann Hesse",
  "pages" : 320
}
\"\"\"
```

<h4 id="record-constructors">Record Constructors</h4>

When you create a type alias for a record, you also create a &ldquo;record constructor&rdquo;.

```elm
type Book = { title:String, author:String, pages:Int }

-- This creates the following record constructor:
-- Book : String -> String -> Int -> Book

book : Book
book = Book "Foundation" "Asimov" 255
```

The arguments to `Book` must be given in the order they appear in the type alias.

Record constructors also work for extensible records:

```elm
type Positioned a = { a | x:Float, y:Float }

-- This creates the following record constructor:
-- Positioned : Float -> Float -> a -> Positioned a

myBook : Positioned Book
myBook = Positioned 3 4 book
```

Notice that the record we are extending is the *last* argument. This convention
makes it much easier to compose a chain of record extensions.

```elm
type Moving a = { a | velocity:Float, angle:Float }

projectile : Moving (Positioned Book)
projectile = Moving 100 (degrees 30) myBook

projectile' : Moving (Positioned Book)
projectile' = book |> Positioned 0 0
                   |> Moving 100 (degrees 90)
```

## Final Notes

Huge thank you to Prezi and the community on the [elm-discuss lists][list].
The diversity of opinions and experiences on the list is extremely helpful
for Elm. I find that bringing an idea up on the lists always results in a
thoughtful discussion and ultimately leads to more refined design choices,
so thank you!

 [list]: https://groups.google.com/forum/?fromgroups#!forum/elm-discuss

Now for some 0.9 specifics. Thank you to Andrew who added `as` patterns and
type annotations in let expressions. And thank you to Max New who significantly
sped up this website.

Thank you to the beginners who came to my programming class in Budapest.
Not only was it super fun, but you found a bug in the compiler!
This experience also convinced me that unary negation was a good idea.

"""
