
import Website.Skeleton (skeleton)
import Window as Window

intro = [markdown|

<h1><div style="text-align:center">Types in Elm
<div style="font-size:0.5em;font-weight:normal">*What are they? Why are they useful?*</div></div>
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

What are types? Why are they useful?

Elm is a strongly and statically typed language. All types can be inferred, so
it is totally fine to leave them out entirely. The compiler will tell you if
you have made a mistake.

For people coming from languages like JavaScript, Java, and C, this may seem


### Type annotations

First we have type annotations, which are a form of self-documentation that is
useful for debugging code and reading other peoples' code. It looks like this:

    factorial : Int -> Int

This means that factorial is a function that takes integers and gives back
integers. Without seeing the function, we have a pretty good idea that the
actual definition will be:

    factorial n = product [1..n]

This is increasingly valuable as your functions become more complex and more
abstract. For example, say I am writing mergesort and I want to split a list
into two equal length sub-lists, I'd want something like this:

   split : [a] -> ([a],[a])

It takes in a list, produces a pair of lists. Type annotations are valuable
because they encourage you to write functions like a contract or interface.
You must think, "what does this function actually need to do?", and other
people can read that without knowing any implementation details.

### Type Aliases

Type aliases are a way of giving types a clearer or nicer name. In the simplest
case, you can use them to make your API more instructive:

    type Height = Float
    type Weight = Float

    -- calculate body mass index (BMI)
    bmi : Height -> Weight -> Float

That is a somewhat dubious usage, but it should be illustrative. You can tell
from just the type of `bmi` exactly what it's arguments should be.

Type aliases are increasingly useful as your types become more complex.

They are particularly useful for records. Say you'd like to represent something
with a position, velocity, and direction:

    type Thing = { x:Float, y:Float, v:Float, a:Float }

This says, you can now write types like this:

    car : Thing
    car = { x=0, y=0, v=100, a = degrees 30 }

    positionOf : Thing -> (Float,Float)

Without type aliases, we'd be typing out a potentially very large record every
time a `Thing` was used. And if the record changed, we'd have to go through and
update every occurrence. Not good!

You can also have type aliases with type variables. Perhaps you want to make
use of extensible records, you can say:

    type Named a = { a | name:String }
    type Colorful a = { a | color:Color }
    
    myCar : Colorful (Named Thing)
    myCar = { car | color=red, name="Flying Brick" }

We can now write generic functions to handle any object that can move through
the world.

    type Movable a = { a | x:Float, y:Float, v:Float, a:Float }

    move : Time -> Movable a -> Movable a

This is saying, `move` can operate on *anything* that has a position, velocity,
and direction. Well, a `Thing` fits exactly that description, so you can call
`move` on `car` and `myCar` with no problem!

|]

content w = width (min 600 w) intro

main = lift (skeleton content) Window.width