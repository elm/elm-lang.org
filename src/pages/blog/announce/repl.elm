import Blog
import Center


main =
  Blog.blog
    "Elm REPL"
    "Elm in the terminal"
    Blog.evan
    (Blog.Date 2013 9 3)
    [ Center.markdown "600px" content ]


content = """

The first release of [`elm-repl`](https://github.com/elm-lang/elm-repl#elm-repl)
is now available. Like any traditional
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop),
it lets you interact with functions and expressions buried deep inside a
large project with many modules.

After you install [Node.js](http://nodejs.org/download/),
you can install the REPL with:

```bash
cabal update ; cabal install elm-repl
```

From there, just run `elm-repl` and start writing Elm expressions, definitions,
ADTs, and module imports.

It is just for the command line now, but I&rsquo;d *love* to see `elm-repl`
integrated into editors. Modes for [emacs](https://github.com/jcollard/elm-mode),
[vim](https://github.com/lambdatoast/elm.vim), and
[Sublime Text](https://github.com/deadfoxygrandpa/Elm.tmLanguage) are still
maturing and improving, so if you are interested in working on any of these,
please get in contact with the authors. This is a great way to contribute to Elm!

## Usage

The first thing to know about a REPL is how to exit:
press `Ctrl-d`.

When you enter an expression, you get the result and its type:

```elm
> 1 + 1
2 : number

> "hello" ++ "world"
"helloworld" : String
```

The same can be done with definitions of values and functions:

```elm
> fortyOne = 41
41 : number

> increment n = n + 1
<function> : number -> number

> increment fortyOne
42 : number

> factorial n = \\
|   if n < 1 then 1 else n * factorial (n-1)
<function> : number -> number

> factorial 5
120 : number
```

You can also define union types:

```elm
> type List a = Nil | Cons a (List a)

> isNil xs = \\
|   case xs of \\
|     Nil -> True \\
|     Cons _ _ -> False
<function> : List a -> Bool

> isNil Nil
True : Bool
```

You can also import standard libraries and any library reachable
from the directory in which `elm-repl` is running. Let's say you
are working on a module called `Graph`:

```elm
> import String

> String.length "hello"
5 : Int

> String.reverse "flow"
"wolf" : String

> import Graph

> Graph.edges
<function> : Graph -> [Edge]
```

This means you can dig into large projects you are working on
and see how a specific function behaves.

## What happened to &ldquo;A New Kind of REPL&rdquo;?

When I announced [hot-swapping in Elm](/blog/interactive-programming), I
called it a new kind of REPL. Riffing on Stephen Wolfram's
[New Kind of Science](http://en.wikipedia.org/wiki/A_New_Kind_of_Science) definitely
makes for a provocative title, but perhaps unsurprisingly, the old kind
of REPL is still very important.

It is clear that [hot-swapping](/blog/interactive-programming)
changes how we tweak and perfect our programs. It changes how we debug.
It changes how beginners learn to program. It changes how developers dig
into existing codebases. That is all great, but for some reason we still had
folks on [the list][repl-request] asking for a good old fashioned
[Read-eval-print-loop](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).
My initial feeling was &ldquo;Don't you see!
[REPLs are so 2000 and late](https://youtu.be/4m48GqaOz90?t=54)&rdquo;
but I was missing the bigger picture.

When it comes to exploring functions deep inside a large codebase, a REPL is
a great tool. Looking at the results of an entire program makes it hard to
pin down specific functions, especially when they are more abstract or do not
have direct impact on the things displayed by the program. In other words,
a REPL is the unit test of interactive programming.

<p style="text-align:center;">
[REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) : [unit testing](http://en.wikipedia.org/wiki/Unit_testing) :: [hot-swapping](http://en.wikipedia.org/wiki/Hot_swapping) : [system testing](http://en.wikipedia.org/wiki/System_testing)
</p>

Once I put together the basics of `elm-repl` it was obvious that REPLs and
hot-swapping are great complements, both helping make developing and debugging
easier in their own way.

## Thank you

Thank you to Joe Collard for [explaining to me why he needed a
REPL][repl-request].
Once I fully understood the problem, I had to do it. It was like a happier version
of [The Tell-Tale Heart](http://en.wikipedia.org/wiki/The_Tell-Tale_Heart).
Thank you to [Thomas Bereknyei](https://github.com/tomberek) for figuring out
how to catch `Ctrl-c` presses in a platform independent way. Thanks to
the [haskeline](http://hackage.haskell.org/package/haskeline) project
which provided lots of great infrastructure for this project.

 [repl-request]: https://groups.google.com/forum/#!searchin/elm-discuss/repl/elm-discuss/OqT-HjGCkyY/sGvDAcb8Y84J

"""