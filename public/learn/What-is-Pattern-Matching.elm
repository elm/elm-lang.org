import Website.Skeleton
import Website.ColorScheme


---- Text of the page: all written in Markdown ----

what = [markdown|

## What is &ldquo;Pattern Matching&rdquo;?

The term "pattern matching" is used in a different way by the functional
programming community. In the non-functional world, it often refers to
finding patterns within strings (we call that &ldquo;using regular expressions&rdquo;
or &ldquo;parsing&rdquo;). For us it is a way to break apart an algebraic data type (ADT).
Here is an example:

        data Color = Blue | Red

So we have defined the type "Color" and it has two possible values: "Blue" and "Red".
So when we pattern match like this:

        toString clr =
            case clr of
              { Blue -> "Blue"
              ; Red  -> "Red" }

The case-expression is saying, &ldquo;look at the structure of `clr`. If it is `Blue`, do this.
If it is `Red`, do that.&rdquo; So `(toString Blue)` evaluates to `"Blue"` and `(toString Red)`
evaluates to `"Red"`.

Now let's try a more complicated example. If you have ever implemented a linked list in
C or Java you will appreciate how easy this is in Elm.

        data List a = Empty | Cons a (List a)

So this creates a type called `List`. A list can either be empty or it can have one element
(called the *head* of the list) and &ldquo;the rest of the list&rdquo; (called the *tail* of the list).
The second case is called &ldquo;cons&rdquo; for historical reasons.

List also takes a type as an argument, so we can create
`(List Int)` or `(List String)` or whatever. The values that have the type `(List Int)`
would look like this: 

- `Empty`
- `Cons 1 Empty`
- `Cons 3 (Cons 2 (Cons 1 Empty))`

All of these have the same type, so they can be used in all the same places.
So when we pattern match we can define what we want to do in each case.

        length myList =
            case myList of
              { Cons head tail -> 1 + (length tail)
              ; Empty -> 0 }

So when we write something like `(length (Cons 1 (Cons 2 (Cons 3 Empty))))` which is
evaluated like this:

        length (Cons 1 (Cons 2 (Cons 3 Empty)))
        1 + (length (Cons 2 (Cons 3 Empty)))
        1 + (1 + length (Cons 3 Empty))
        1 + (1 + (1 + length Empty))
        1 + (1 + (1 + 0))
        1 + (1 + 1)
        1 + 2
        3

This is exactly how lists work in Elm! It's just a little more concise, so
`(Cons 1 (Cons 2 (Cons 3 Empty)))` becomes `(1 : (2 : (3 : [])))`. This
can also be written as `(1 : 2 : 3 : [])` or preferably as `[1,2,3]`.

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