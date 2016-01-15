import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


port title : String
port title = "Elm Syntax"


(=>) = (,)


main =
  Blog.docs
    "Elm Syntax"
    [ Center.markdown "600px" content ]

content = """

This syntax reference is a minimal introduction to:

- [Comments](#comments)
- [Literals](#literals)
- [Lists](#lists)
- [Conditionals](#conditionals)
- [Union Types](#union-types)
- [Records](#records)
- [Functions](#functions)
- [Infix Operators](#infix-operators)
- [Let Expressions](#let-expressions)
- [Applying Functions](#applying-functions)
- [Modules](#modules)
- [Type Annotations](#type-annotations)
- [Type Aliases](#type-aliases)
- [JavaScript FFI](#javascript-ffi)

Check out the [learning resources](/Learn.elm) for
tutorials and examples on actually *using* this syntax.

### Comments

```elm
-- a single line comment

{- a multiline comment
   {- can be nested -}
-}
```

Here's a handy trick that every Elm programmer should know:

```elm
{--}
add x y = x + y
--}
```

Just add or remove the `}` on the first line and you'll toggle between commented and uncommented!

### Literals

```elm
-- Boolean
True  : Bool
False : Bool

42    : number  -- Int or Float depending on usage
3.14  : Float

'a'   : Char
"abc" : String

-- multi-line String
\"\"\"
This is useful for holding JSON or other
content that has "quotation marks".
\"\"\"
```

Typical manipulation of literals:

```elm
True && not (True || False)
(2 + 4) * (4^2 - 9)
"abc" ++ "def"
```

### Lists

Here are four things that are equivalent:

```elm
[1..4]
[1,2,3,4]
1 :: [2,3,4]
1 :: 2 :: 3 :: 4 :: []
```

### Conditionals

```elm
if powerLevel > 9000 then "OVER 9000!!!" else "meh"
```

If you need to branch on many different conditions, you just chain this
construct together.

```elm
if key == 40 then
    n + 1

else if key == 38 then
    n - 1

else
    n
```

You can also have conditional behavior based on the structure of algebraic
data types and literals

```elm
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

### Union Types

```elm
type List = Empty | Node Int List
```

Not sure what this means? [Read this](/guide/model-the-problem).

### Records

For more explanation of Elm&rsquo;s record system, see [this overview][exp],
the [initial announcement][v7], or [this academic paper][records].

  [exp]: /docs/records "Records in Elm"
  [v7]:  /blog/announce/0.7 "Elm version 0.7"
  [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible records with scoped labels"

```elm
point =                    -- create a record
  { x = 3, y = 4 }

point.x                    -- access field

map .x [point,{x=0,y=0}]   -- field access function

{ point | x = 6 }          -- update a field

{ point |                  -- update many fields
    x = point.x + 1,
    y = point.y + 1
}

dist {x,y} =               -- pattern matching on fields
  sqrt (x^2 + y^2)

type alias Location =      -- type aliases for records
  { line : Int
  , column : Int
  }
```

### Functions

```elm
square n =
  n^2

hypotenuse a b =
  sqrt (square a + square b)

distance (a,b) (x,y) =
  hypotenuse (a-x) (b-y)
```

Anonymous functions:

```elm
square =
  \\n -> n^2

squares =
  List.map (\\n -> n^2) [1..100]
```

### Infix Operators

You can create custom infix operators.
[Precedence](http://en.wikipedia.org/wiki/Order_of_operations) goes from 0 to
9, where 9 is the tightest. The default precedence is 9 and the default
[associativity](http://en.wikipedia.org/wiki/Operator_associativity) is left.
You can set this yourself, but you cannot override built-in operators.

```elm
(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?
```

Use [`(<|)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#<|)
and [`(|>)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#|>)
to reduce parentheses usage. They are aliases for function
application.

```elm
f <| x = f x
x |> f = f x

dot =
  scale 2 (move (20,20) (filled blue (circle 10)))

dot' =
  circle 10
    |> filled blue
    |> move (20,20)
    |> scale 2
```

Historical note: this is borrowed from F#, inspired by Unix pipes.

Relatedly, [`(<<)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#<<)
and [`(>>)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#>>)
are function composition operators.


### Let Expressions

Let expressions are for assigning variables, kind of like a `var` in
JavaScript.

```elm
let
  x = 3 * 8
  y = 4 ^ 2
in
  x + y
```

You can define functions and use &ldquo;destructuring assignment&rdquo; in let
expressions too.

```elm
let
  (x,y) = (3,4)

  hypotenuse a b =
    sqrt (a^2 + b^2)
in
  hypotenuse x y
```

Let-expressions are indentation sensitive, so each definition must align with
the one above it.


### Applying Functions

```elm
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

```elm
23 + 19    : number
2.0 + 1    : Float

6 * 7      : number
10 * 4.2   : Float

100 // 2  : Int
1 / 2     : Float
```

There is a special function for creating tuples:

```elm
(,) 1 2              == (1,2)
(,,,) 1 True 'a' []  == (1,True,'a',[])
```

You can use as many commas as you want.


### Modules

```elm
module MyModule where

-- module decleration exporting only foo and bar
module MyModule (foo, bar) where

-- qualified imports
import List                    -- List.map, List.foldl
import List as L               -- L.map, L.foldl

-- open imports
import List exposing (..)               -- map, foldl, concat, ...
import List exposing ( map, foldl )     -- map, foldl

import Maybe exposing ( Maybe )         -- Maybe
import Maybe exposing ( Maybe(..) )     -- Maybe, Just, Nothing
import Maybe exposing ( Maybe(Just) )   -- Maybe, Just
```

Qualified imports are preferred. Module names must match their file name,
so module `Parser.Utils` needs to be in file `Parser/Utils.elm`.


### Type Annotations

```elm
answer : Int
answer =
  42

factorial : Int -> Int
factorial n =
  List.product [1..n]

distance : { x : Float, y : Float } -> Float
distance {x,y} =
  sqrt (x^2 + y^2)
```


### Type Aliases

```elm
type alias Name = String
type alias Age = Int

info : (Name,Age)
info =
  ("Steve", 28)

type alias Point = { x:Float, y:Float }

origin : Point
origin =
  { x = 0, y = 0 }
```


### JavaScript FFI

```elm
-- incoming values
port userID : String
port prices : Signal Float

-- outgoing values
port time : Signal Float
port time =
  every second
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

 * `title` sets the page title, ignoring empty strings
 * `log` logs messages to the developer console
 * `redirect` redirects to a different page, ignoring empty strings

Experimental port handlers:

 * `favicon` sets the pages favicon
 * `stdout` logs to stdout in node.js and to console in browser
 * `stderr` logs to stderr in node.js and to console in browser

"""
