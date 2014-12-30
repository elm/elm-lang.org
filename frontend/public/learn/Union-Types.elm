import Graphics.Element (..)
import Markdown
import Signal (Signal, (<~))
import Website.Skeleton (skeleton)
import Window


port title : String
port title = "Union Types"


main : Signal Element
main = skeleton "Learn" (\w -> width (min 600 w) content) <~ Window.dimensions


content : Element
content = Markdown.toElement """

# Union Types

A union type is a way to put together many different types. If you have Java
background, think of them as enums on steroids.

## Enumerating Possibilities

One common use of union types is to enumerate a couple possible states. Imagine
we are creating a [todo list](http://evancz.github.io/elm-todomvc/) and want to
crate a filter on which tasks are visible. We can show all tasks, all the
active tasks, or all the completed tasks. We can represent these three states
like this:

```haskell
type Visibility
    = All
    | Active
    | Completed
```

This defines a new type `Visibility` with exactly three possible values: `All`,
`Active`, and `Completed`. We use **case-expressions** to do different things
depending which value we are working with:

```haskell
toString : Visibility -> String
toString visibility =
    case visibility of
      All -> "All"
      Active -> "Active"
      Completed -> "Completed"

-- toString All       == "All"
-- toString Active    == "Active"
-- toString Completed == "Completed"
```

The case-expression is saying, &ldquo;look at the structure of `visibility`.
If it is `All`, do this. If it is `Active`, do that. If it is `Completed` do
this other thing.&rdquo;

This fills the same role as &ldquo;enumerations&rdquo; in other languages, but
union types are much more flexible than that!

## Enumeration + Data

Okay, what if we want to represent if someone is logged in or not. With union
types we can say:

```haskell
type User
    = Anonymous
    | LoggedIn String
```

Notice that the `LoggedIn` value is associated with extra information! This is
saying that a user is either `Anonymous` or they are `LoggedIn` and we know
their user name. We can use that extra information with *case-expressions*. The
following code turns user info into image resources for their picture.

```haskell
userPhoto : User -> String
userPhoto user =
    case user of
      Anonymous ->
          "anon.png"

      LoggedIn name ->
          "users/" ++ name ++ "/photo.png"
```

If they are not logged in we show a dummy photo, but if they *are* logged in
we show the photo we have saved. Now imagine we have a bunch of users all
collaborating on a document and we want to show all their pictures.

```haskell
activeUsers : List User
activeUsers =
    [ Anonymous
    , LoggedIn "Tom"
    , LoggedIn "Steve"
    , Anonymous
    ]
```

We can mix data with very different shapes in the same list. If we combine
the `userPhoto` function with our `activeUsers` list, we can get all the images
we need:

```haskell
map userPhoto activeUsers
-- [ "anon.png"
-- , "users/Tom/photo.png"
-- , "users/Steve/photo.png"
-- , "anon.png"
-- ]
```

All the users are turned into image resources. Okay, but union types can still
do more!

## Putting Types Together

Union types are all about putting together different types. We have seen some
special cases so far, but union types are much more flexible and are an
extremely important part of programming in Elm.

Say you are creating a dashboard with three different kinds of widgets. One
shows scatter plots, one shows recent log data, and one shows time plots. Type
unions make it really easy to put together the data we need:

```haskell
type Widget
    = ScatterPlot (List (Int, Int))
    | LogData (List String)
    | TimePlot (List (Time, Int))
```

You can think of this as putting together three different types. Each type is
&ldquo;tagged&rdquo; with a name like `ScatterPlot` or `LogData`. This lets us
tell them apart when your program is running. Now we can write something to
render a widget like this:

```haskell
view : Widget -> Element
view widget =
    case widget of
      ScatterPlot points ->
          viewScatterPlot points

      LogData logs ->
          flow down (map viewLog logs)

      TimePlot occurances ->
          viewTimePlot occurances
```

Depending on what kind of widget we are looking at, we will render it
differently. Perhaps we want to get a bit trickier and have some time plots
that that are showed on a logarithmic scale. We can augment our `Widget` type
a bit.

```haskell
type Scale = Normal | Logarithmic

type Widget
    = ScatterPlot (List (Int, Int))
    | LogData (List String)
    | TimePlot Scale (List (Time, Int))
```

Notice that the `TimePlot` tag now has two pieces of data. Each tag can
actually hold a bunch of different types.

All of these strategies can be used if you are making a game and have a bunch
of different bad guys. Goombas should update one way, but Koopa Troopas do
something totally different. Use a union type to put them all together!

## No more NULL

Tons of languages have a concept of `null`. Any time you think you have a
`String` you just might have a `null` instead. Should you check? Did the person
giving you the value check? Maybe it will be fine? Maybe it will crash your
servers? I guess we will find out later!

Union types let us sidestep this problem entirely with a type called `Maybe`.

```haskell
type Maybe a
    = Just a
    | Nothing
```

Notice that this union type takes an argument `a` that we can fill in with any
type we want. We can have types like `(Maybe Int)` which is either `Just` an
integer or it is `Nothing`. For example, say we want to parse months from
strings.

```haskell
String.toInt : String -> Maybe Int

toMonth : String -> Maybe Int
toMonth rawString =
    case String.toInt rawString of
      Nothing -> Nothing
      Just n ->
          if n > 0 && n <= 12 then Just n else Nothing
```

Now our types explicitly tell everyone that you may end up with something
besides and integer. You never have to wonder if there is a `null` value
sneaking around. This may seem like a subtle improvement, but think about what
your life will be like when you never have to hunt for a null pointer
exception again!

## Recursive Data Structures

If you have ever implemented a [linked list](https://en.wikipedia.org/wiki/Linked_list)
in C or Java you will appreciate how easy this is in Elm.
The following algebraic data type represents a list. The front of a list
can only be one of two things: empty or something followed by a list.
We can turn this informal definition into a union type:

```haskell
type List a
    = Empty
    | Node a (List a)
```

So this creates a type called `List`. A list can either be empty or it can
have one element (called the *head* of the list) and &ldquo;the rest of the
list&rdquo; (called the *tail* of the list).

List also takes a type as an argument, so we can create `(List Int)` or
`(List String)` or whatever. The values that have the type `(List Int)`
would look like this:

- `Empty`
- `Node 1 Empty`
- `Node 3 (Node 2 (Node 1 Empty))`

All of these have the same type, so they can be used in all the same places.
So when we pattern match we can define what we want to do in each case.
Say we want to compute the sum of all of the numbers in a list. The
following function defines the logic for each possible scenario.

```haskell
sum : List Int -> Int
sum xs =
    case xs of
      Empty -> 0

      Node first rest ->
          first + sum rest
```

If we get an `Empty` value, the sum is 0. If we have a `Node` we add the first
element to the sum of all the remaining ones. So an expression like
`(sum (Node 1 (Node 2 (Node 3 Empty))))` is evaluated like this:


```haskell
sum (Node 1 (Node 2 (Node 3 Empty)))
1 + sum (Node 2 (Node 3 Empty))
1 + (2 + sum (Node 3 Empty))
1 + (2 + (3 + sum Empty))
1 + (2 + (3 + 0))
1 + (2 + 3)
1 + 5
6
```

On each line, we see one evaluation step. When we call `sum` it transforms the
list based on whether it is looking at a `Node` or an `Empty` value.

## Additional Resources

We can create all sorts of data structures, like [binary trees][binary].

 [binary]: http://en.wikipedia.org/wiki/Binary_tree "Binary Trees"

```haskell
type Tree a
    = Empty
    | Node a (Tree a) (Tree a)
```

A tree is either empty or it is a node with a value and two children.
This is actually a generalization of lists. You could represent lists
by always having the left child be empty. There is way more discussion
of trees in [this example][trees]. If you can do all of the exercises
at the end of the example, consider yourself a capable user of union types.

 [trees]: /edit/examples/Functional/Tree.elm "Tree Example"

We can even model a programming language! In this case, it is one that only
deals with [Boolean algebra][algebra]:

 [algebra]: http://en.wikipedia.org/wiki/Boolean_algebra#Operations "Boolean Algebra"

```haskell
type Boolean
    = T
    | F
    | Not Boolean
    | Or  Boolean Boolean
    | And Boolean Boolean

true  = Or T F
false = And T (Not T)
```

Once we have modeled the possible values we can define functions like `eval`
which reduces any `Boolean` to `True` or `False`. See [this example][bool] for
more about representing boolean expressions.

 [bool]: /edit/examples/Functional/BooleanExpressions.elm

"""


