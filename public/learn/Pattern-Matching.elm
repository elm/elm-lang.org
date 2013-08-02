import Website.Skeleton (skeleton)
import Window
import JavaScript as JS

---- Text of the page: all written in Markdown ----

what w = width w [markdown|

<style type="text/css">
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

## What is &ldquo;Pattern Matching&rdquo;?

The term "pattern matching" is used in a different way by the functional
programming community. In the non-functional world, it often refers to
finding patterns within strings (we call that &ldquo;using [regular
expressions][re]&rdquo; or &ldquo;parsing&rdquo;). For us it is a way to break
apart an [algebraic data type][adt] (ADT). Here is an example of an algebraic data
type:

 [re]: http://en.wikipedia.org/wiki/Regular_expression#Formal_language_theory "Regular Expressions"
 [adt]: http://en.wikipedia.org/wiki/Algebraic_data_type "Algebraic Data Type"

```haskell
data Color = Blue | Red
```

So we have defined the type "Color" and it has two possible values: "Blue"
and "Red". These values are called type constructors because the allow you
to construct a value of type `Color`.

Now we need a way to interact with these values. The operators we already know
like `(+)` and `(||)` are no use, so we introduce **pattern matching**,
using patterns in the structure of a `Color` to break it apart. This happens with
case-expressions.

```haskell
toString color = case color of
                   Blue -> "Blue"
                   Red  -> "Red"
```

The case-expression is saying, &ldquo;look at the structure of `color`. If it
is `Blue`, do this. If it is `Red`, do that.&rdquo;
So `(toString Blue)` evaluates to `"Blue"` and `(toString Red)`
evaluates to `"Red"`.

Algebraic data types (ADTs) are much more flexible than that though! When we
start using the `Color` ADT we just defined, we will quickly find it a bit
constraining. What is exclusively blue and red? Besides underwater volcanos,
not much. I mean, that *is* pretty rad, but it probably will not come up too often.

Let&rsquo;s try to make a better ADT for representing colors. Maybe we want to define
colors as transparent or an arbitrary RGB color.

```haskell
data BetterColor = Transparent | RGB Int Int Int
```

Each case actually holds additional data. The first case just represents transparency.
The second case `RGB` holds three integer values representing the red, blue,
and green that make up a color.

We can now define any color we want!

```haskell
orange = RGB 255 127 0
purple = RGB 128 0 128
```

Okay, now that we know how to put additional data in our ADTs,
let&rsquo;s try to make something more practical.

### Practical Examples

If you have ever implemented a [linked list](https://en.wikipedia.org/wiki/Linked_list)
in C or Java you will appreciate how easy this is in Elm.
The following algebraic data type represents a list. The front of a list
can only be one of two things: empty or something followed by a list.
We can turn this informal definition into an ADT:

```haskell
data List a = Empty | Cons a (List a)
```

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

```haskell
product xs = case xs of
               Cons head tail -> head * product tail
               Empty -> 1
```

This use of pattern matching is more complicated than with a `Color` because
we are using variables *in* the pattern. In the case of a `Cons` value, the
head of the list will be named `head` and the tail will be called `tail`.
Now we can refer to those internal values.

The `product` function works as follows.
If list `xs` is a `Cons` then we take the head of the list and multiply it
with the product of the tail of the list. If list `xs` is empty, the product
is one. So an expression like `(product (Cons 1 (Cons 2 (Cons 3 Empty))))` is
evaluated like this:


```haskell
product (Cons 1 (Cons 2 (Cons 3 Empty)))
1 * product (Cons 2 (Cons 3 Empty))
1 * (2 * product (Cons 3 Empty))
1 * (2 * (3 * product Empty))
1 * (2 * (3 * 1))
1 * (2 * 3)
1 * 6
6
```

In fact, this is exactly how lists work in Elm! It's just a little more
concise, so `(Cons 1 (Cons 2 (Cons 3 Empty)))` becomes
`(1 :: (2 :: (3 :: [])))`. This can also be written as `(1 :: 2 :: 3 :: [])` or
preferably as `[1,2,3]`. So the above example could be written as
`(product [1,2,3])`.

You can also use expressions like `[1..9]` to automatically construct
lists over a given range. This is called *list interpolation*, and it
can be surprisingly useful. For instance, it allows us to write an
extremely concise factorial function:

```haskell
factorial n = product [1..n]
```

Great! Let's try some more difficult stuff.

### Additional Resources

We can create all sorts of data structures, like [binary trees][binary].

 [binary]: http://en.wikipedia.org/wiki/Binary_tree "Binary Trees"

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```

A tree is either empty or it is a node with a value and two children.
This is actually a generalization of lists. You could represent lists
by always having the left child be empty. There is way more discussion
of trees in [this example][trees]. If you can do all of the exercises
at the end of the example, consider yourself a capable user of ADTs.

 [trees]: /edit/examples/Functional/Tree.elm "Tree Example"

We can also model interactions. For instance, when we get an HTTP response
we know it will be one of three things:

```haskell
data Response a = Success a | Waiting | Failure Int String
```

We are always in one of these states, and with pattern matching it is
easy to define what should happen in each case. This data type is actually
used in the [HTTP library][http].

 [http]: /docs/Signal/HTTP.elm "HTTP library"

We can even model a programming language! In this case, it is one that only
deals with [Boolean algebra][algebra]:

 [algebra]: http://en.wikipedia.org/wiki/Boolean_algebra#Operations "Boolean Algebra"

```haskell
data Boolean
    = Tru
    | Fls
    | Not Boolean
    | Or  Boolean Boolean
    | And Boolean Boolean

tru = Or Tru Fls
fls = And Tru (Not Tru)
```

Once we have modeled the possible values we can define functions like `eval`
which reduces any `Boolean` to `True` or `False`. See [this example][bool] for
more about representing boolean expressions.

 [bool]: /edit/examples/Functional/BooleanExpressions.elm

|]


---- Putting it all together into a single page ----


main = lift (skeleton (what . min 600)) Window.width


---- Setting the title of the page to be prettier ----

titles = constant (JS.fromString "What is Pattern Matching?")
foreign export jsevent "title"
  titles : Signal JS.JSString