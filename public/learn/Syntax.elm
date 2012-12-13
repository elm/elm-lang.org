
import Website.Skeleton
import Website.ColorScheme

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
- [Functions](#functions)
- [Let Expressions](#let-expressions)
- [Applying Functions](#applying-functions)
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
    1:[2,3,4]
    1:2:3:4:[]
    [1..4]

Here are two things that are equivalent:

    ['a','b','c']
    "abc"

### Conditionals

    if conditional then trueBranch else falseBranch
    if powerLevel > 9000 then "OVER 9000!!!" else "meh"

    case exp of
        Pattern1 -> exp1
        Pattern2 -> exp2
        _        -> exp3

    case xs of
        []   -> Nothing
        x:xs -> Just x

Each pattern is indentation sensitive, meaning that you have to align
all of your patterns for this to parse right.

If you think you can do a one liner, it might be useful to use the `{;;}`
separators for your case statement.

    case xs of { [] -> True ; _ -> False }

This kind of case-expression is not indentation sensitive.

### Algebraic Data Types

    data List = Nil | Cons Int List

### Functions

    f x = exp

You can pattern match in the declaration of any function.

    hypotenuse a b = sqrt (a^2 + b^2)

You can also create guarded definitions for named functions.
So `moveBy` is defined differently
given different conditions. You can read the `|` as *where*.

    moveBy key n | key == 40 = n+1
                 | key == 38 = n-1
                 | otherwise = n

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

    let c = hypotenuse 3 4
    in  c*c

    let c1 = hypotenuse 7 12
        c2 = hypotenuse 3 4
    in  hypotenuse c1 c2

Let-expressions are also indentation sensitive, so each definition
should align with the one above it. You can also use the `{;;}` separators to make things unaware of indentation, but it is generally
not as pretty.

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

### Modules

    module MyModule where

    import List
    import List (intercalate, intersperse)
    import List hiding (map,foldl,foldr)
    import List as L

### JavaScript FFI

    foreign import jsevent "eventName"
        (expr)
        signalName :: Signal jsType

The `expr` can be any Elm expression. It is the initial value of the signal `signalName`.
As events with name `eventName` occur, signal `signalName` will get updated.
The type `jsType` must be a JavaScript type such as `JSNumber` or `JSString`.

    foreign export jsevent "eventName"
        signalName :: Signal jsType

The rules are the same for `export` except you do not need an initial value.

### Things *not* in Elm

Elm currently does not support:

- type annotations (high-priority to add)
- setting the precedence or associativity of infix operators
- operator sections such as `(1+)`
- `where` clauses
- any sort of `do` or `proc` notation
- a unary negation operator. Negative 3 is the same as `(0-3)`.
- multi-line function declarations where the function being defined actually
  appears multiple times as a definition. In Elm, that actually means to
  define a value multiple times.

|]

content w = width (min 600 w) intro

main = lift (skeleton content) Window.width