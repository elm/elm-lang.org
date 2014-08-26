import Website.Skeleton (skeleton)
import Window

main = skeleton "Learn" content <~ Window.dimensions

content w = width (min 600 w) intro

intro = [markdown|

# The Syntax of Elm

This syntax reference is a minimal introduction to:

- [Comments](#comments)
- [Literals](#literals)
- [Lists](#lists)
- [Conditionals](#conditionals)
- [Algebraic Data Types](#adts)
- [Records](#records)
- [Functions](#functions)
- [Infix Operators](#infix-operators)
- [Let Expressions](#let-expressions)
- [Applying Functions](#applying-functions)
- [Lifting with `(<~)` and `(~)`](#lifting)
- [Modules](#modules)
- [Type Annotations](#type-annotations)
- [Type Aliases](#type-aliases)
- [JavaScript FFI](#javascript-ffi)
- [Things *not* in Elm](#things-not-in-elm)

Check out the [learning resources](/Learn.elm) for
tutorials and examples on actually *using* this syntax.

<h3 id="comments">Comments</h3>

```haskell
-- a single line comment

{- a multiline comment
   {- can be nested -}
-}
```

Here's a handy trick that every Elm programmer should know:

```haskell
{--}
add x y = x + y
--}
```

Just add or remove the `}` on the first line and you'll toggle between commented and uncommented!

<h3 id="literals">Literals</h3>

```haskell
-- Boolean
True  : Bool
False : Bool

42    : number  -- Int or Float depending on usage
3.14  : Float

'a'   : Char
"abc" : String

-- multi-line String
"""
This is useful for holding JSON or other
content that has "quotation marks".
"""
```

Typical manipulation of literals:

```haskell
True && not (True || False)
(2 + 4) * (4^2 - 9)
"abc" ++ "def"
```

<h3 id="lists">Lists</h3>

Here are four things that are equivalent:

```haskell
[1..4]
[1,2,3,4]
1 :: [2,3,4]
1 :: 2 :: 3 :: 4 :: []
```

<h3 id="conditionals">Conditionals</h3>

```haskell
if powerLevel > 9000 then "OVER 9000!!!" else "meh"
```

Multi-way if-expressions make it easier
to have a bunch of different branches.
You can read the `|` as *where*.

```haskell
if | key == 40 -> n+1
   | key == 38 -> n-1
   | otherwise -> n
```

You can also have conditional behavior based on the structure of algebraic
data types and literals

```haskell
case maybe of
  Just xs -> xs
  Nothing -> []

case xs of
  hd::tl -> Just (hd,tl)
  []     -> Nothing

case n of
  0 -> 1
  1 -> 1
  _ -> fib (n-1) + fib (n-2)
```

Each pattern is indentation sensitive, meaning that you have to align
all of your patterns.

<h3 id="adts">Algebraic Data Types</h3>

```haskell
data List = Nil | Cons Int List
```

Not sure what this means? [Read this](/learn/Pattern-Matching.elm).

<h3 id="records">Records</h3>

For more explanation of Elm&rsquo;s record system, see [this overview][exp],
the [initial announcement][v7], or [this academic paper][records].

  [exp]: /learn/Records.elm "Records in Elm"
  [v7]:  /blog/announce/0.7.elm "Elm version 0.7"
  [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible records with scoped labels"

```haskell
point = { x = 3, y = 4 }       -- create a record

point.x                        -- access field
map .x [point,{x=0,y=0}]       -- field access function

{ point - x }                  -- remove field
{ point | z = 12 }             -- add field
{ point - x | z = point.x }    -- rename field
{ point - x | x = 6 }          -- update field

{ point | x <- 6 }             -- nicer way to update a field
{ point | x <- point.x + 1
        , y <- point.y + 1 }   -- batch update fields

dist {x,y} = sqrt (x^2 + y^2)  -- pattern matching on fields
\{x,y} -> (x,y)

lib = { id x = x }             -- polymorphic fields
(lib.id 42 == 42)
(lib.id [] == [])

type Location = { line:Int, column:Int }
```

<h3 id="functions">Functions</h3>

```haskell
square n = n^2

hypotenuse a b = sqrt (square a + square b)

distance (a,b) (x,y) = hypotenuse (a-x) (b-y)
```

Anonymous functions:

```haskell
square = \n -> n^2
squares = map (\n -> n^2) [1..100]
```

<h3 id="infix-operators">Infix Operators</h3>

You can create custom infix operators. The default
[precedence](http://en.wikipedia.org/wiki/Order_of_operations)
is 9 and the default
[associativity](http://en.wikipedia.org/wiki/Operator_associativity)
is left, but you can set your own.
You cannot override the built-in operators though.

```haskell
f <| x = f x
(<~) = lift

infixr 0 <|
infixl 4 <~
```

Use [`(<|)`](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Basics#<|)
and [`(|>)`](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Basics#|>)
to reduce parentheses usage. They are aliases for function
application.

```haskell
f <| x = f x
x |> f = f x

dot  = scale 2 (move (20,20) (filled blue (circle 10)))
dot' = circle 10 |> filled blue
                 |> move (20,20)
                 |> scale 2
```

Historical note: this is borrowed from F#, inspired by Unix pipes,
improving upon Haskell&rsquo;s `($)`.

<h3 id="let-expressions">Let Expressions</h3>

```haskell
let n = 42
    (a,b) = (3,4)
    {x,y} = { x=3, y=4 }
    square n = n * n
in
    square a + square b
```

Let-expressions are indentation sensitive.
Each definition should align with the one above it.

<h3 id="applying-functions">Applying Functions</h3>

```haskell
-- alias for appending lists and two lists
append xs ys = xs ++ ys
xs = [1,2,3]
ys = [4,5,6]

-- All of the following expressions are equivalent:
a1 = append xs ys
a2 = (++) xs ys

b1 = xs `append` ys
b2 = xs ++ ys

c1 = (append xs) ys
c2 = ((++) xs) ys
```

The basic arithmetic infix operators all figure out what type they should have automatically.

```haskell
23 + 19    : number
2.0 + 1    : Float

6 * 7      : number
10 * 4.2   : Float

1 `div` 2  : Int
1 / 2      : Float
```

There is a special function for creating tuples:

```haskell
(,) 1 2              == (1,2)
(,,,) 1 True 'a' []  == (1,True,'a',[])
```

You can use as many commas as you want.

<h3 id="lifting">Lifting</h3>

The `lift` functions are used to apply a normal function like `sqrt` to a signal
of values such as `Mouse.x`. So the expression `(lift sqrt Mouse.x)` evaluates
to a signal in which the current value is equal to the square root of the current
x-coordinate of the mouse.

You can also use the functions `(<~)` and `(~)` to lift signals. The squigly
arrow is exactly the same as the lift function, so the following expressions
are the same:

```haskell
lift sqrt Mouse.x
sqrt <~ Mouse.x
```

You can think of it as saying &ldquo;send this signal through this
function.&rdquo;

The `(~)` operator allows you to apply a signal of functions to a signal of
values `(Signal (a -> b) -> Signal a -> Signal b)`. It can be used to put
together many signals, just like `lift2`, `lift3`, etc. So the following
expressions are equivalent:

```haskell
lift2 (,) Mouse.x Mouse.y
(,) <~ Mouse.x ~ Mouse.y

lift2 scene (fps 50) (sampleOn Mouse.clicks Mouse.position)
scene <~ fps 50 ~ sampleOn Mouse.clicks Mouse.position
```

More info can be found [here](/blog/announce/0.7.elm#do-you-even-lift)
and [here](http://library.elm-lang.org/catalog/elm-lang-Elm/latest/Signal).

<h3 id="modules">Modules</h3>

```haskell
module MyModule where

-- qualified imports
import List               -- List.map, List.foldl
import List as L          -- L.map, L.foldl

-- open imports
import List (..)          -- map, foldl, concat, ...
import List (map,foldl)   -- map, foldl
```

Qualified imports are preferred. Module names must match their file name,
so module `Parser.Utils` needs to be in file `Parser/Utils.elm`.

<h3 id="type-annotations">Type Annotations</h3>

```haskell
answer : Int
answer = 42

factorial : Int -> Int
factorial n = product [1..n]

addName : String -> a -> { a | name:String }
addName name record = { record | name = name }
```

<h3 id="type-aliases">Type Aliases</h3>

```haskell
type Name = String
type Age = Int

info : (Name,Age)
info = ("Steve", 28)

type Point = { x:Float, y:Float }

origin : Point
origin = { x=0, y=0 }
```

<h3 id="javascript-ffi">JavaScript FFI</h3>

```haskell
-- incoming values
port userID : String
port prices : Signal Float

-- outgoing values
port time : Signal Float
port time = every second

port increment : Int -> Int
port increment = \n -> n + 1
```

From JS, you talk to these ports like this:

```javascript
var example = Elm.worker(Elm.Example, {
  userID:"abc123",
  prices:11
});

example.ports.prices.send(42);
example.ports.prices.send(13);

example.ports.time.subscribe(callback);
example.ports.time.unsubscribe(callback);

example.ports.increment(41) === 42;
```

More example uses can be found
[here](https://github.com/evancz/elm-html-and-js)
and [here](https://gist.github.com/evancz/8521339).

Elm has some built-in port handlers that automatically take some
imperative action:

 * [`title`](/edit/examples/Reactive/Title.elm) sets the page title, ignoring empty strings
 * [`log`](/edit/examples/Reactive/Log.elm) logs messages to the developer console
 * [`redirect`](/edit/examples/Reactive/Redirect.elm) redirects to a different page, ignoring empty strings

Experimental port handlers:

 * `favicon` sets the pages favicon
 * `stdout` logs to stdout in node.js and to console in browser
 * `stderr` logs to stderr in node.js and to console in browser

<h3 id="things-not-in-elm">Things *not* in Elm</h3>

Elm currently does not support:

- operator sections such as `(+1)`
- guarded definitions or guarded cases. Use the multi-way if for this.
- `where` clauses
- any sort of `do` or `proc` notation

|]
