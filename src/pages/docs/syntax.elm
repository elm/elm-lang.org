import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center

port title : String
port title = "Elm Syntax"

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
- [Defining Functions](#defining-functions)
- [Applying Functions](#applying-functions)
- [Union Types](#union-types)
- [Let Expressions](#let-expressions)
- [Tuples](#tuples)
- [Records](#records)
- [Type Aliases](#type-aliases)
- [Type Annotations](#type-annotations)
- [Working with Functions](#working-with-functions)
- [Infix Operators](#infix-operators)
- [Modules and Imports](#modules-and-imports)
- [Connecting to JavaScript](#connecting-to-javascript)

Most of these topics are covered more thoroughly in the [Complete Guide to Elm](/docs#complete-guide).

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
True  : Bool -- read: "true has type bool"
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
"abc" ++ "def" == "abcdef"
abs -1 == 1
```

You can also compare Elm's literals to [those in JavaScript](/docs/from-javascript).

### Lists

The list is Elm's main data structure. Every element in a list must be of the same
type. Here are four equal lists:

```elm
[1..4]
[1,2,3,4]
1 :: [2,3,4]
1 :: 2 :: 3 :: 4 :: []
```

The `(::)` operator is pronounced "cons" (the parantheses mean it is infix).

### Conditionals

```elm
if powerLevel > 9000 then "OVER 9000!!!" else "meh"
```

You can chain if-expressions into a bunch of different branches.

```elm
if key == 40 then n+1
else if key == 38 then n-1
else n
```

You can also have conditional behavior based on the structure of literals
and [union types](#union-types).

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

### Defining Functions
Functions are defined by writing their name, arguments separated by spaces, an equals sign,
and then the function body.

```elm
square n = n^2

hypotenuse a b = sqrt (square a + square b)

distance (a,b) (x,y) = hypotenuse (a-x) (b-y)
```

An anonymous function is introduced with a backslash. They are usually enclosed
in parantheses.

```elm
-- the style above is preferred, but this is equivalent
square = \\n -> n^2
-- typical use of an anonymous function
squares = List.map (\\n -> n^2) [1..100]
```

A definition is like a function with no arguments:

```elm
duration = 1.5*second
```

Elm uses `camelCase` for names of functions and values.

### Applying Functions

Functions and arguments are separated only by whitespace.

```elm
-- alias for appending lists, and two lists
append xs ys = xs ++ ys
xs = [1,2,3]
ys = [4,5,6]

-- All six of the following expressions are equivalent:
a1 = append xs ys
a2 = (++) xs ys

b1 = xs `append` ys
b2 = xs ++ ys

c1 = (append xs) ys
c2 = ((++) xs) ys
```

### Let Expressions

Define local variables with a let expression. Only the final result will be
visible to the outside world.

```elm
let a = 42
    b = 256
    square n = n * n
in
    square a + square b
```

Let-expressions are indentation sensitive.
Each definition should align with the one above it.

### Union Types

A union type consists of one or more tags. Each tag can have one or more values
of a known type carried with it.

```elm
-- a simple enumeration
type ConnectionStatus = Connecting | Connected | Disconnected | CouldNotConnect

-- Any Node will have two other values, one of which is recursive
type ListOfInts = Empty | Node Int ListOfInts

-- a "tree of a", where "a" can be any type
type Tree a = Leaf | Node a (Tree a) (Tree a)
```

Union types are explained in more detail [here](/guide/model-the-problem).

### Tuples

Tuples are lightweight groups of values. You always know how many values
there are, and their types. Both tuples and their types are written with
parentheses.

```elm
(1.41, 2.72) : (Float, Float)
(True, "Love") : (Bool, String)
```

Usually you access a tuple's values with pattern matching.

```elm
area (width, height) = width * height
```

There is a special function for creating tuples:

```elm
(,) 1 2              == (1,2)
(,,,) 1 True 'a' []  == (1,True,'a',[])
```

You can use as many commas as you want.

The empty tuple or *unit* is sometimes used as a placeholder value. It is
the only value of its type.

```elm
() : ()
```

### Records

A tuple holds values in order; a record holds values by key.

```elm
point = { x = 3, y = 4 }       -- create a record

point.x                        -- access field
map .x [point, {x=0,y=0}]      -- field access function

{ point | x = 6 }              -- update a field
{ point | x = point.x + 1
        , y = point.y + 1 }    -- batch update fields based on old values

dist {x,y} = sqrt (x^2 + y^2)  -- pattern matching on fields
\\{x,y} -> (x,y)

lib = { double x = x*2 }       -- fields can hold functions
lib.double 42 == 84
```

### Type Aliases

Unlike union types, which create new types, type alias introduce new names for
existing types. This is very handy when you have large tuples or records.

```elm
type alias Name = String
type alias Age = Int

info : (Name, Age)
info = ("Steve", 28)

type alias Point = { x : Float, y : Float }

origin : Point
origin = { x = 0, y = 0 }
```

When you alias a record, you get a record constructor that takes arguments in
the same order as the record.

```elm
Point 0 0 == origin
```

### Type Annotations

Types always begin with a capital letter. Type variables begin with a lowercase letter.

```elm
answer : Int
answer = 42

factorial : Int -> Int
factorial n = product [1..n]

listLength : List a -> Int
listLength aList =
    case aList of
        [] -> 0
        x::xs -> 1 + listLength xs
```

### Working with Functions

Every function with more than one argument can be partially applied with only
some of its arguments.

```elm
log2 = logBase 2
log2 64 == logBase 2 64
log2 256 == 8
```

Use [`(<|)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#<|)
and [`(|>)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#|>)
to reduce parentheses usage. They are aliases for function application. If it
helps, you can think of them like Unix pipes.

```elm
f <| x = f x
x |> f = f x

dot =
  scale 2 (move (20,20) (filled blue (circle 10)))

otherDot =
  circle 10
    |> filled blue
    |> move (20,20)
    |> scale 2
```

Relatedly, use [`(<<)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#<<)
and [`(>>)`](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#>>)
for function composition.

```elm
type alias Person = {position : {x : Float, y : Float}}
xValues = List.map (.position >> .x) people

List.map (logBase 10 >> ceiling) [42, 256, 9001] == [2, 3, 4]
```

Be aware that function equality is not supported.

```elm
-- DON'T DO THIS!
myFunction == anotherFunction
```

### Infix Operators

Function application has higher precedence than (happens before) any infix
operator.

```elm
square 6 + 6 == 42
square (6 + 6) == 144
```

The basic arithmetic infix operators follow the order of operations.

```elm
2 + 5 * 2^3 == 2 + (5 * (2^3))
cos (degrees 30) ^ 2 + sin (degrees 30) ^ 2 == 1
```

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

### Modules and Imports

```elm
module MyModule where

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

### Connecting to JavaScript

```elm
-- incoming values are declared only as type annotations
port userID : String
port prices : Signal Float

-- outgoing values must have a definition
port time : Signal Float
port time = every second
```

In JS, you talk to these ports like this:

```javascript
var example = Elm.worker(Elm.Example, {
  userID: "abc123",
  prices: 11
});

example.ports.prices.send(42);
example.ports.prices.send(13);

example.ports.time.subscribe(callback);
example.ports.time.unsubscribe(callback);
```

Elm has a built-in port handler to set the page title (ignoring empty strings).

```elm
port title : String
port title = "My Cool Page"
```

Ports are also used to run Tasks. Instead of handing off a value to a callback,
you hand off a description of work to be done, and Elm does it for you.

For more information, see the [interop guide](/guide/interop).

"""