import Website.Skeleton
import Website.ColorScheme


---- Text of the page: all written in Markdown ----

what = [markdown|

## What is &ldquo;Pattern Matching&rdquo;?

The term "pattern matching" is used in a different way by the functional
programming community. In the non-functional world, it often refers to
finding patterns within strings (we call that &ldquo;using regular
expressions&rdquo; or &ldquo;parsing&rdquo;). For us it is a way to break
apart an algebraic data type (ADT). Here is an example of an algebraic data
type:

        data Color = Blue | Red

So we have defined the type "Color" and it has two possible values: "Blue"
and "Red". These values are called type constructors because the allow you
to construct a value of type `Color`.

Now we need a way to interact with these values. The operators we already know
like `(+)` and `(||)` are no use, so we introduce pattern matching with
case-expressions.

        toString color = case color of
                           Blue -> "Blue"
                           Red  -> "Red"

The case-expression is saying, &ldquo;look at the structure of `color`. If it
is `Blue`, do this. If it is `Red`, do that.&rdquo;
So `(toString Blue)` evaluates to `"Blue"` and `(toString Red)`
evaluates to `"Red"`.

Now let's try a more complicated example. If you have ever implemented a
linked list in C or Java you will appreciate how easy this is in Elm.
The following algebraic data type represents a list. The front of a list
can only be one of two things: empty or something followed by a list.
We can turn this informal definition into an ADT:

        data List a = Empty | Cons a (List a)

So this creates a type called `List`. A list can either be empty or it can
have one element (called the *head* of the list) and &ldquo;the rest of the
list&rdquo; (called the *tail* of the list).
The second type constructor is called `Cons` because that&rsquo;s
just the way it has always been. I find [the real explaination][cons] fairly
unsatisfying, but at some point it stops being weird.

  [cons]: http://en.wikipedia.org/wiki/Cons "Cons"

List also takes a type as an argument, so we can create `(List Int)` or
`(List String)` or whatever. The values that have the type `(List Int)`
would look like this: 

- `Empty`
- `Cons 1 Empty`
- `Cons 3 (Cons 2 (Cons 1 Empty))`

All of these have the same type, so they can be used in all the same places.
So when we pattern match we can define what we want to do in each case.
Say we want to compute the product of all of the numbers in a list. The
following function defines the logic for each possible scenario.

        product xs = case xs of
                       Cons hd tl -> hd * sum tl
                       Empty -> 1

This use of pattern matching is more complicated. In the `Cons` case we are
matching with variables. This means the head of the list will be named `hd`
and the tail will be called `tl`. This way we can refer to those
internal values.

The `product` function works as follows.
If list `xs` is a `Cons` then we take the head of the list and multiply it
with the product of the tail of the list. If list `xs` is empty, the product
is one. So an expression like `(product (Cons 1 (Cons 2 (Cons 3 Empty))))` is
evaluated like this:

        product (Cons 1 (Cons 2 (Cons 3 Empty)))
        1 * product (Cons 2 (Cons 3 Empty))
        1 * (2 * product (Cons 3 Empty))
        1 * (2 * (3 * product Empty))
        1 * (2 * (3 * 1))
        1 * (2 * 3)
        1 * 6
        6

In fact, this is exactly how lists work in Elm! It's just a little more
concise, so `(Cons 1 (Cons 2 (Cons 3 Empty)))` becomes
`(1 : (2 : (3 : [])))`. This can also be written as `(1 : 2 : 3 : [])` or
preferably as `[1,2,3]`. So the above example could be written as
`(product [1,2,3])`.

You can also use expressions like `[1..9]` to automatically construct
lists over a given range. This is called *list interpolation*, and it
can be surprisingly useful. For instance, it allows us to write an
extremely consices factorial function:

        factorial n = product [1..n]

Great! Let's try some more difficult stuff.

We can create all sorts of data structures, like trees.

        data Tree a = Empty | Node a (Tree a) (Tree a)

A tree is either empty or it is a node with a value and two children.

We can model interactions. For instance, when we get an HTTP response
we know it will be one of three things:

        data Response a = Success a | Waiting | Failure Int String

We are always in one of these states, and with pattern matching it is
easy to define what should happen in each case.

We can even model a programming language! In this case, it is one that only
deals with Boolean values:

        data BoolExpr = Tru
                      | Fls
                      | Not BoolExpr
                      | Or BoolExpr BoolExpr
                      | And BoolExpr BoolExpr

Once we have modeled the possible values we can define functions like `eval`
which reduces any `BoolExpr` to `True` or `False`.

|]


---- Putting it all together into a single page ----


main = lift (skeleton (\w -> width w what)) Window.width


---- Setting the title of the page to be prettier ----

titles = lift JavaScript.castStringToJSString (constant "What is Pattern Matching?")
foreign export jsevent "elm_title"
  titles :: Signal JSString