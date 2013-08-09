
import Website.Skeleton (skeleton)
import open Website.ColorScheme
import Window
import JavaScript as JS

title = constant (JS.fromString "Elm 0.9 - Fix the type-checker")
foreign export jsevent "title"
  title : Signal JS.JSString

main = lift (skeleton everything) Window.width

everything wid =
    let w = min 600 wid
    in  width w intro

intro = [markdown|

<style type="text/css">
p { text-align: justify }
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

<h1><div style="text-align:center">Elm 0.9
<div style="font-size:0.5em;font-weight:normal">*Fast and reliable static checks*</div></div>
</h1>

[Elm](/)&rsquo;s type checker has been completely rewritten.
At a high level, the key difference is simple: it works now.

  * Undefined values are errors. Finally.
  * All type errors are caught and reported.
  * Error messages are more specific and easier to read.
  * It&rsquo;s fast.

This is a huge step forward, creating a solid foundation for further improvement.

This release also introduces many frequently requested syntax improvements. The
most notable are as follows:

  * Unary negation
  * Pattern matching on literals and `as` patterns
  * Multi-line strings
  * Record constructors

Finally, there are a bunch of miscellaneous improvements:

  * API for detecting when the mouse hovers over an `Element`
  * Set `alpha` of any `Form` for transparency in a `collage`
  * Many bug fixes for `collage`, especially when rendering an `Element`
  * `Text.height` use pixels instead of [ems][], after [much debate][]

  [much debate]: https://groups.google.com/forum/?fromgroups#!searchin/elm-discuss/specifying$20size$20of$20text/elm-discuss/3Iz-HpV1QRg/oHPoqWDgrmEJ
  [ems]: http://en.wikipedia.org/wiki/Em_(typography)

The rest of this post will try to cover [all of the changes][changes] in more detail.
Maybe start the [download process][download] while you are reading!

  [download]: https://github.com/evancz/Elm/blob/master/README.md#install
  [changes]: https://github.com/evancz/Elm/blob/master/changelog.txt

<br/>

## Type checker and Build Improvements

This was a really big project for me, so I want to first thank Prezi
for making all of this work possible! Also, thank you to Spiros and
Laszlo for talking through issues with me as they came up; this was
a huge help!

#### Why was the type cheker bad before?

Before this release, my primary priority was: prove that FRP is viable and good.
If FRP is not the right way, it does not matter how good or bad the type checker
is.

I recently started to feel that the poor error messages were
becoming the primary barrier for Elm. The questions have started to
move from &ldquo;is this possible with FRP?&rdquo; to &ldquo;I am
doing this with FRP, how can the tools be better?&rdquo; This is a
very positive sign for FRP!

#### How to make it better?

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

With the build infrastructure was in place, cross-module type checking became
possible. The next task was understand and implement an efficienct type inference
algorithm.

To summarize, it is very tricky and there are few resources available that even
discuss efficient practical implementations, let alone give details on specific
techniques and strategies. I am planning to do a &ldquo;field guide to efficient
type inference&rdquo; to make it easier for others to create a type inferencer
that is correct and *fast*.

For now, take a look at [this chapter][] of Advanced Topics in Types and Programming
Languages. It is one of the best resources I have found.

  [this chapter]: http://www.cs.cmu.edu/~rwh/courses/refinements/papers/PottierRemy04/hmx.pdf

## Syntax Improvements

#### Pattern Matching

You can now pattern match on literals like numbers, strings, and booleans.

```haskell
commasToSpaces str =
  case str of
    ',':rest -> ' ' : simpleEscape rest
    c  :rest ->  c  : simpleEscape rest

isOrigin pos =
  case pos of
    (0,0) -> True
    _     -> False
```

You can also use `as` patterns now, thanks to Andrew!

```haskell
move time ({x,y,vx,vy} as object) =
  { object | x <- x + vx * time
           , y <- y + vy * time }
```

This lets you name a structure and match on its sub-structure.
The `as` keyword binds very loosely, so it often needs parentheses.
One example might be:

```haskell
data World = World Mario [Goomba] [Brick]

step input (World mario goombas bricks as world) = ...
```

#### Unary negation

I was very hesitant to add this feature because I had not seen a
statically-typed functional language that I felt got this right.
The issue is that the `(-)` operator overlaps with subtraction,
so mathematical expressions can become ambiguous for the parser
and reader.

You get questions like: is `(x -1)` subtraction or is `(f -1)` function
application with a negative argument?

After [discussing many options][negate], we decided on the following requirements
to classify a minus sign as unary negation:

  * it is preceded by whitespace or `(` or `[` or `,`
  * no whitespace on the right, the negated value follows without spaces

The following examples attempt to cover all of the cases you might see in the wild:

```haskell
n - 1        -- subtraction
n-1          -- subtraction
-10          -- negative ten
- 10         -- parse error, no spaces allowed on right
(-100,-100)  -- point in quadrant III
abs -1       -- abs (-1)
max -2 -4    -- max (-2) (-4)
h - 3 - 5    -- subtraction twice
n - -1       -- n + 1
```

Another way to describe these rules is &ldquo;unary negation binds tighter than function
application and infix operations.&rdquo;

  [negate]: https://groups.google.com/forum/?fromgroups#!searchin/elm-discuss/negation/elm-discuss/DcvoUKPzM_M/KIogCVoL9G0J

In practice, I have found that this is how my brain parses code. I definitely read
`(max -2 -4)` as a function, my brain blocks each syntactic unit into a semantic unit.

This is similar to how `(.)` can mean many different things depending on spacing. It is
unfortunate to overload, but I think this is the best solution given the constraints.

#### Triple-quoted multi-line strings

Just like Python, you can have big multi-line strings:
This will make it easier to embed plain-text or JSON if
necessary.

```haskell
json = """
{
  "title" : "Narcissus and Goldmund",
  "author": "Hermann Hesse",
  "pages" : 320
}
"""
```

#### Record Constructors

When you create a type alias for a record, you also create a &ldquo;record constructor&rdquo;.

```haskell
type Book = { title:String, author:String, pages:Int }

-- This creates the following record constructor:
-- Book : String -> String -> Int -> Book

book : Book
book = Book "Foundation" "Asimov" 255
```

The arguments to `Book` must be given in the order they appear in the type alias.

Record constructors also work for extensible records:

```haskell
type Positioned a = { a | x:Float, y:Float }

-- This creates the following record constructor:
-- Positioned : Float -> Float -> a -> Positioned a

myBook : Positioned Book
myBook = Positioned 3 4 book
```

Notice that the record we are extending is the *last* argument. This convention
makes it much easier to compose a chain of record extensions.

```haskell
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

|]
