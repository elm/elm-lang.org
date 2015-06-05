
# Model The Problem

Many languages have trouble expressing data with weird shapes in a reliable way. You often find yourself doing weird tricks with boolean flags or strings, or giving in and letting the language force you into a model that is slightly off. Either way you end up with code that is hard to maintain and refactor!

This section goes through the parts of Elm that let you work with crazy data in a way is reliable and easy to refactor. We will start with a foundation of &ldquo;contracts&rdquo; and then use them with progressively crazier and crazier data.


## Contracts

Types are an important tool for modeling. Think of them like a contract that can be checked by the compiler that says something like &ldquo;I only accept string arguments&rdquo; so you can make sure that bad data *never* gets in. This is a huge part of how we can rule out runtime errors in Elm.

> **Note:** The term &ldquo;types&rdquo; will be used to mean &ldquo;types as they appear in Elm&rdquo;. This is an important distinction because *types in Elm are very different than types in Java!* Many programmers have only seen types in Java, so their experience is roughly &ldquo;using types is verbose and annoying, and at the end of the day, I still get the same runtime errors and null pointer exceptions as in JavaScript or Python or Ruby. What's the point?!&rdquo; Most Elm programmers share all of these complaints about Java!

The way we write down these contracts is with &ldquo;type annotations&rdquo; where we define the exact shape of the data we are working with.


```haskell
fortyTwo : Int
fortyTwo =
  42


names : List String
names =
  [ "Alice", "Bob", "Chuck" ]


book : { title: String, author: String, pages: Int }
book =
  { title = "Demian", author = "Hesse", pages = 176 }
```

Here we are just describing the general shape of the data we are working with. `fortyTwo` is an integer, `names` is a list of strings, and `book` is a record with certain fields. Nothing crazy, just describing the shape of our data. This becomes much more valuable when you start using it with functions, where in many languages, getting the wrong kind of data can lead to a crash!

```haskell
import List exposing (sum, map, length)


averageNameLength : List String -> Float
averageNameLength names =
  sum (map String.length names) / length names


isLong : { record | pages : Int } -> Bool
isLong book =
  book.pages > 400
```

In the `longestName` example, we are requiring that our input is a list of strings. If someone tries to pass in a list of integers or books, the `String.length` function would break, so this contract rules that out. We also say the `longestName` function is defenitely going to return a `Float` so if we use its result somewhere else, we have a 100% guarantee that its a floating point number.

The `isLong` example is doing exactly the same thing. It requires a record with a field name `pages` that holds integers. Any record will do, with however many other fields you want, but we definitely need the `pages` field!

So in both cases we are writing contracts that say &ldquo;I require input with this shape, and I will give you output with that shape.&rdquo; This is the essense of ruling out runtime errors in Elm. We always know what kind of values a function needs and what kind it produces, so we can just check that we always follow these rules.

> **Note:** All of these types can be inferred, so you can leave off the type annotations and Elm can still check that data is flowing around in a way that works. This means you can just *not* write these contracts and still get all the benefits!

So far we have seen some simple cases where we make sure our data is the right shape, but these contracts become extremely powerful when you start making your own types.


## Enumerations

It is quite common to create a type that enumerates a couple possible states. Imagine we are creating a [todo list](http://evancz.github.io/elm-todomvc/) and want to create a filter on which tasks are visible. We can show all tasks, all the active tasks, or all the completed tasks. We can represent these three states like this:

```haskell
type Visibility = All | Active | Completed
```

This defines a new type `Visibility` with exactly three possible values: `All`, `Active`, or `Completed`. This means that if you pass in something with type `Visibility` it must be one of these three things!

We use **case-expressions** to do different things depending which value we are working with. It is pretty similar to the switch-statements in JavaScript, but a case-expression does not have fall through, so you don't need to say `break` everywhere to make things sane.

```haskell
toString : Visibility -> String
toString visibility =
    case visibility of
      All ->
          "All"

      Active ->
          "Active"

      Completed ->
          "Completed"


-- toString All == "All"
-- toString Active == "Active"
-- toString Completed == "Completed"
```

The case-expression is saying, &ldquo;look at the structure of `visibility`. If it is `All`, do this. If it is `Active`, do that. If it is `Completed` do this other thing.&rdquo;

This fills the same role as &ldquo;enumerations&rdquo; in languages like Java or C++, but we can do much more than that!


## State Machines

Okay, what if we want to represent whether someone is logged in or not? We can make a little state machine that lets a user toggled between anonymous and logged in with a user name:

```haskell
type User = Anonymous | LoggedIn String
```

Notice that the `LoggedIn` value is associated with extra information! This is saying that a user is either `Anonymous` or they are `LoggedIn` and we know their user name. We can use that extra information with case-expressions. The following code turns user info into image resources for their picture.

```haskell
userPhoto : User -> String
userPhoto user =
    case user of
      Anonymous ->
          "anon.png"

      LoggedIn name ->
          "users/" ++ name ++ "/photo.png"
```

If they are not logged in we show a dummy photo, but if they *are* logged in we show the photo we have saved. Now imagine we have a bunch of users all collaborating on a document and we want to show all their pictures.

```haskell
activeUsers : List User
activeUsers =
    [ Anonymous
    , LoggedIn "Tom"
    , LoggedIn "Steve"
    , Anonymous
    ]
```

We can mix data with very different shapes in the same list. If we combine the `userPhoto` function with our `activeUsers` list, we can get all the images we need:

```haskell
photos =
    List.map userPhoto activeUsers

-- photos =
--     [ "anon.png"
--     , "users/Tom/photo.png"
--     , "users/Steve/photo.png"
--     , "anon.png"
--     ]
```

All the users are turned into image resources. So we saw a relatively simple state machine here, but you could imagine users having 5 different possible states, and we can model that in a really precise way that makes it really hard for errors to sneak in. Making little state machines like this is at the heart of making the most of types!


## Tagged Unions

Now lets try to put together a bunch of *different* types of data in a coherent way.

> **Note:** These are sometimes called [tagged unions](http://en.wikipedia.org/wiki/Tagged_union) (or [ADTs](http://en.wikipedia.org/wiki/Algebraic_data_type) in certain communities).

Say you are creating a dashboard with three different kinds of widgets. One shows scatter plots, one shows recent log data, and one shows time plots. Type unions make it really easy to put together the data we need:

```haskell
type Widget
    = ScatterPlot (List (Int, Int))
    | LogData (List String)
    | TimePlot (List (Time, Int))
```

You can think of this as putting together three different types. Each type is &ldquo;tagged&rdquo; with a name like `ScatterPlot` or `LogData`. This lets us tell them apart when your program is running. Now we can write something to render a widget like this:

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

Depending on what kind of widget we are looking at, we will render it differently. Perhaps we want to get a bit trickier and have some time plots that are showed on a logarithmic scale. We can augment our `Widget` type a bit.

```haskell
type Scale = Normal | Logarithmic

type Widget
    = ScatterPlot (List (Int, Int))
    | LogData (List String)
    | TimePlot Scale (List (Time, Int))
```

Notice that the `TimePlot` tag now has two pieces of data. Each tag can actually hold a bunch of different types.

All of these strategies can be used if you are making a game and have a bunch of different bad guys. Goombas should update one way, but Koopa Troopas do something totally different. Use a tagged union to put them all together!


## Banishing NULL

Tons of languages have a concept of `null`. Any time you think you have a `String` you just might have a `null` instead. Should you check? Did the person giving you the value check? Maybe it will be fine? Maybe it will crash your servers? I guess we will find out later!

The inventor, Tony Hoare, has this to say about it:

> I call it my billion-dollar mistake. It was the invention of the null reference in 1965. At that time, I was designing the first comprehensive type system for references in an object oriented language (ALGOL W). My goal was to ensure that all use of references should be absolutely safe, with checking performed automatically by the compiler. But I couldn't resist the temptation to put in a null reference, simply because it was so easy to implement. This has led to innumerable errors, vulnerabilities, and system crashes, which have probably caused a billion dollars of pain and damage in the last forty years.

Elm sidesteps this problem entirely with a type called `Maybe`. You can think of it as making `null` explicit, so we *know* if we have to handle it.

```haskell
type Maybe a = Just a | Nothing
```

Notice that this type takes an argument `a` that we can fill in with any type we want. We can have types like `(Maybe Int)` which is either `Just` an integer or it is `Nothing`. For example, say we want to parse months from strings.

```haskell
String.toInt : String -> Maybe Int


toMonth : String -> Maybe Int
toMonth rawString =
    case String.toInt rawString of
      Nothing ->
          Nothing

      Just n ->
          if n > 0 && n <= 12 then Just n else Nothing
```

The contract for `toMonth` explicitly tells everyone that it will give back an integer *or* it won't! You never have to wonder if there is a `null` value sneaking around.

This may seem like a subtle improvement, but imagine all the code you have where you defensively added a `null` check just in case someone else behaves badly. Having contracts means you have a guarantee that a caller won't send you bad data! This is a world where you never again have to spend 4 hours debugging a null pointer exception!


## Recursive Data Structures

If you have ever implemented a [linked list](https://en.wikipedia.org/wiki/Linked_list) in C or Java you will appreciate how easy this is in Elm. The following type represents a list. The front of a list can only be one of two things: empty or something followed by a list. We can turn this informal definition into a type:

```haskell
type List a = Empty | Node a (List a)
```

So this creates a type called `List`. A list can either be empty or it can have one element (called the *head* of the list) and &ldquo;the rest of the list&rdquo; (called the *tail* of the list).

List also takes a type as an argument, so we can create `(List Int)` or `(List String)` or whatever. The values that have the type `(List Int)` would look like this:

  * `Empty`
  * `Node 1 Empty`
  * `Node 3 (Node 2 (Node 1 Empty))`

All of these have the same type, so they can be used in all the same places. So when we pattern match we can define what we want to do in each case. Say we want to compute the sum of all of the numbers in a list. The following function defines the logic for each possible scenario.

```haskell
sum : List Int -> Int
sum xs =
    case xs of
      Empty ->
          0

      Node first rest ->
          first + sum rest
```

If we get an `Empty` value, the sum is 0. If we have a `Node` we add the first element to the sum of all the remaining ones. So an expression like `(sum (Node 1 (Node 2 (Node 3 Empty))))` is evaluated like this:

  * `sum (Node 1 (Node 2 (Node 3 Empty)))`
  * `1 + sum (Node 2 (Node 3 Empty))`
  * `1 + (2 + sum (Node 3 Empty))`
  * `1 + (2 + (3 + sum Empty))`
  * `1 + (2 + (3 + 0))`
  * `1 + (2 + 3)`
  * `1 + 5`
  * `6`

On each line, we see one evaluation step. When we call `sum` it transforms the list based on whether it is looking at a `Node` or an `Empty` value.

Making lists is just the start, we can easily create all sorts of data structures, like [binary trees][binary].

 [binary]: http://en.wikipedia.org/wiki/Binary_tree "Binary Trees"

```haskell
type Tree a = Empty | Node a (Tree a) (Tree a)
```

A tree is either empty or it is a node with a value and two children. Check out [this example][trees] to see some more examples of union types for data structures. If you can do all of the exercises at the end of the example, consider yourself a capable user of this feature!

[trees]: /examples/binary-tree

> **Note:** Imagine doing this binary tree exercise in Java. We would probably be working with one super class and two sub classes just to define a tree in the first place! Imagine doing it in JavaScript. It is not quite as bad at first, but imagine trying to refactor the resulting code later if you need to change the core representation. Sneaky breakages everywhere!

We can even model a programming language as data if we want to go really crazy! In this case, it is one that only deals with [Boolean algebra][algebra]:

[algebra]: http://en.wikipedia.org/wiki/Boolean_algebra#Operations "Boolean Algebra"

```haskell
type Boolean
    = T
    | F
    | Not Boolean
    | Or  Boolean Boolean
    | And Boolean Boolean

true = Or T F
false = And T (Not T)
```

Once we have modeled the possible values we can define functions like `eval` which evaluates any `Boolean` to `True` or `False`. See [this example][bool] for more about representing boolean expressions.

[bool]: /examples/boolean-expressions.elm
