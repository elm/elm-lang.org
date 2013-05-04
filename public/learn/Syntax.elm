import Website.Skeleton
import Website.ColorScheme
import Window as Window

intro = [markdown|

<h1><div style="text-align:center">The Syntax of Elm
<div style="font-size:0.5em;font-weight:normal">*A Quick Tour*</div></div>
</h1>

<style type="text/css">
h3 { padding-top: 1em; }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
</style>

This document lists all possible Elm syntax.

- [Comments](#comments)
- [Literals](#literals)
- [Lists](#lists)
- [Conditionals](#conditionals)
- [Algebraic Data Types](#algebraic-data-types)
- [Records](#records)
- [Functions](#functions)
- [Let Expressions](#let-expressions)
- [Applying Functions](#applying-functions)
- [Lifting with (<~) and (~)](#lifting)
- [Modules](#modules)
- [JavaScript FFI](#javascript-ffi)
- [Things *not* in Elm](#things-not-in-elm)

To learn about the libraries and semantics of Elm, check out the [Documentation][docs].

  [docs]: /Documentation.elm "Documentation"

### Comments

    -- a single line comment

    {- a multiline comment
       {- can be nested -}
    -}

Here's a handy trick that every Elm programmer should know:

    {--}
    add x y = x + y
    --}

Just add or remove the `}` on the first line and you'll toggle between commented and uncommented!

### Literals

The following shows all the different boolean literals and operators.

    True && not (True || False)

Character and string literals are defined as follows:

    'a'       -- Char
    "Hello!"  -- String

A `Number` can be interpreted as an `Int` or a `Float` depending on how it is used.

    42        -- Number
    3.14      -- Float

### Lists

Here are four things that are equivalent:

    [1,2,3,4]
    1 :: [2,3,4]
    1 :: 2 :: 3 :: 4 :: []
    [1..4]

Here are two things that are equivalent:

    ['a','b','c']
    "abc"

### Conditionals

    if conditional then trueBranch else falseBranch
    if powerLevel > 9000 then "OVER 9000!!!" else "meh"

Multi-way if-expressions make it easier
to have a bunch of different branches.
You can read the `|` as *where*.

    if | key == 40 -> n+1
       | key == 38 -> n-1
       | otherwise -> n

You can alse have conditional behavior based on the structure of algebraic
data types.

    case exp of
      Pattern1 -> exp1
      Pattern2 -> exp2
      _        -> exp3

    case xs of
      []    -> Nothing
      x::xs -> Just x

Each pattern is indentation sensitive, meaning that you have to align
all of your patterns for this to parse right.

If you think you can do a one liner, it might be useful to use the `{;;}`
separators for your case statement.

    case xs of { [] -> True ; _ -> False }

This kind of case-expression is not indentation sensitive.

### Algebraic Data Types

    data List = Nil | Cons Int List

Not sure what this means? [Read this.](/learn/Pattern-Matching.elm)

### Records

For more explanation of Elm&rsquo;s record system, see [this overview][exp],
the [initial announcement][v7], or [this academic paper][records].

  [exp]: /learn/Records.elm "Records in Elm"
  [v7]:  /blog/announce/version-0.7.elm "Elm version 0.7"
  [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible records with scoped labels"

    point = { x = 3, y = 4 }       -- create a record

    point.x                        -- access field
    map .x [point,{x=0,y=0}]       -- field access function

    { point - x }                  -- remove field
    { point | z = 12 }             -- add field
    { point - x | z = point.x }    -- rename field
    { point - x | x = 6 }          -- update field

    { point | x <- 6 }             -- nicer way to update a field
    { point | x <- point.x + 1
            , y <- point.x + 1 }   -- batch update fields

    dist {x,y} = sqrt (x^2 + y^2)  -- pattern matching on fields
    \\{x,y} -> (x,y)

    lib = { id x = x }             -- polymorphic fields
    (lib.id 42 == 42)
    (lib.id [] == [])

    type Location = { line:Int, column:Int }

### Functions

    f x = exp

You can pattern match in the declaration of any function.

    hypotenuse a b = sqrt (a^2 + b^2)

You can also create custom infix operators. They have the highest precedence
and are left associative. You cannot override built-in operators.

    (a,b) +++ (x,y) = (a + x, b + y)

In the case of recursion and mutual recursion, the order of the 
function definitions does not matter, Elm figures it out.
In all other cases, assume that definitions
need to appear before their use.

### Let Expressions

This lets you reuse code, avoid repeating
computations, and improve code readability.

    let c = hypotenuse 3 4 in
      c*c

    let c1 = hypotenuse 7 12
        c2 = hypotenuse 3 4
    in  hypotenuse c1 c2

Let-expressions are also indentation sensitive, so each definition
should align with the one above it.

### Applying Functions

If we deine some function called `append` that puts lists together

    append xs ys = xs ++ ys

The following expressions are equivalent:

    append [3] [4]
    [3] `append` [4]
    (append [3]) [4]

In fact, infix operators can be used as functions, so the next few expressions are
*also* equivalent:

    (++) [3] [4]
    [3] ++ [4]
    ((++) [3]) [4]

The basic arithmetic infix operators all figure out what type they should have automatically.

    23 + 19    -- Number
    2.0 + 1    -- Float

    6 * 7      -- Number
    10 * 4.2   -- Float

    1 `div` 2  -- Int
    1 / 2      -- Float

There is a special function for creating tuples:

    (,) 1 2              -- (1,2)
    (,,,) 1 True 'a' []  -- (1,True,'a',[])

You can use as many commas as you want.

You can also use anonymous functions:

    func = (\\a b -> toFloat a / toFloat b)
    xsMod7 = map (\\n -> n `mod` 7) [1..100]

### Lifting

The `lift` functions are used to apply a normal function like `sqrt` to a signal
of values such as `Mouse.x`. So the expression `(lift sqrt Mouse.x)` evaluates
to a signal in which the current value is equal to the square root of the current
x-coordinate of the mouse.

You can also use the functions `(<~)` and `(~)` to lift signals. The squigly
arrow is exactly the same as the lift function, so the following expressions
are the same:

    lift sqrt Mouse.x
    sqrt <~ Mouse.x

You can think of it as saying &ldquo;send this signal through this
function.&rdquo;

The `(~)` operator allows you to apply a signal of functions to a signal of
values `(Signal (a -> b) -> Signal a -> Signal b)`. It can be used to put
together many signals, just like `lift2`, `lift3`, etc. So the following
expressions are equivalent:

    lift2 (,) Mouse.x Mouse.y
    (,) <~ Mouse.x ~ Mouse.y

    lift2 scene (fps 50) (sampleOn Mouse.clicks Mouse.position)
    scene <~ fps 50 ~ sampleOn Mouse.clicks Mouse.position

More info can be found [here](/blog/announce/version-0.7.elm#do-you-even-lift)
and [here](/docs/Signal/Signal.elm).

### Modules

    module MyModule where

    import List
    import List (intercalate, intersperse)
    import List hiding (map,foldl,foldr)
    import List as L

### JavaScript FFI

    foreign import jsevent "eventName"
        (expr)
        signalName : Signal jsType

The `expr` can be any Elm expression. It is the initial value of the
signal `signalName`. As events with name `eventName` occur, signal
`signalName` will get updated. The type `jsType` must be a JavaScript
type such as `JSNumber` or `JSString`.

    foreign export jsevent "eventName"
        signalName : Signal jsType

The rules are the same for `export` except you do not need an initial value.

Elm has four built-in event handlers that take a `JSString` and automatically
take some imperative action:

* `"elm_title"` which sets the page title, ignoring empty strings.
* `"elm_log"` which logs messages in the developer console.
* `"elm_redirect"` which redirects to a different page, ignoring empty strings.
* `"elm_viewport"` which is necessary if you want mobile browsers to accurately
  report their dimensions, allowing you to design a &ldquo;mobile
  optimized&rdquo; program that looks great on any device. The given string
  sets the `content` field of a meta tag like this:<br/>
  `<meta name="viewport" content="width=device-width, initial-scale=1"\>`

### Things *not* in Elm

Elm currently does not support:

- setting the precedence or associativity of infix operators (also will be added)
- operator sections such as `(1+)`
- guarded definitions or guarded cases. Use the multi-way if for this.
- arbitrary ordering for non-function definitions (i.e. `x = y` must come after `y = 3` for now)
- `where` clauses
- any sort of `do` or `proc` notation
- a unary negation operator. Negative 3 is the same as `(0-3)`.
- multi-line function declarations where the function being defined actually
  appears multiple times as a definition. In Elm, that actually means to
  define a value multiple times.

|]

content w = width (min 600 w) intro

main = lift (skeleton content) Window.width
