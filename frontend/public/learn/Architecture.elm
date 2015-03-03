import Graphics.Element exposing (..)
import Markdown
import Signal exposing (Signal, (<~))
import Website.Skeleton exposing (skeleton)
import Window

port title : String
port title = "Architecture"


main : Signal Element
main =
  skeleton "Learn" content <~ Window.dimensions


content : Int -> Element
content w =
  width (min 600 w) article


article : Element
article = Markdown.toElement """

# Architecture in Larger Apps

This document is a collection of concepts and strategies to make large Elm
projects modular and extensible.

We will start by thinking about the structure of signals in our program.
Broadly speaking, your application state should live in one big `foldp`. You
will probably `merge` a bunch of input signals into a single stream of
updates. This sounds a bit crazy at first, but it is in the same ballpark as
Om or Facebook's Flux. There are a couple major benefits to having a
centralized home for your application state:

 1. **There is a single source of truth.** Traditional approaches force you
    to write a decent amount of custom and error prone code to synchronize
    state between many different stateful components. (The state of this
    widget needs to be synced with the application state, which needs to be
    synced with some other widget, etc.) By placing all of your state in one
    location, you eliminate an entire class of bugs in which two components get
    into inconsistent states. We also think you will end up writing much less
    code. That has been our observation in Elm so far.

 2. **Save and Undo become quite easy.** Many applications would benefit from
    the ability to save all application state and send it off to the server so
    it can be reloaded at some later date. This is extremely difficult when
    your application state is spread all over the place and potentially tied
    to objects that cannot be serialized. With a central store, this becomes
    very simple. Many applications would also benefit from the ability to
    easily undo user's actions. For example, a painting app is better with
    Undo. Since everything is immutable in Elm, this is also very easy. Saving
    past states is trivial, and you will automatically get pretty good sharing
    guarantees to keep the size of the snapshots down.

I think these two strengths will be extremely worthwhile in large applications,
though I feel that strength 1 is a huge deal for speeding up development and
avoiding silly bugs that waste your time.

So most of your code will be pure functions that make your big `foldp` actually
do the right thing. The rest of this document focuses on how to make *that*
code modular and extensible.

## Modularity

To make things modular, the major strategy is to hide implementation details,
as shown in [this pseudocode](https://gist.github.com/evancz/ee696a87a644a3d1fa02).
When you create a widget, this makes it possible to expose exactly the API you
want. Essentially just this kind of info:

```haskell
type Model.State
type Model.Action

Model.initialize : String -> ... -> State

Update.step : Action -> State -> State
Update.resetField : State -> State

View.full : Input Action -> State -> Element
View.mini : Input Action -> State -> Element
```

As a user, you don't know anything about what `State` really is, but you have
carefully selected functions for creating it, stepping it, and doing custom
modifications without an Action (e.g. `resetField`) in case other components
need to act on the state. The designer has full control over the API they
expose and can hide any details they want.

In Elm you have the added benefit that these abstraction boundaries are quite
strong. Unlike in JS, you cannot just inspect the structure of arbitrary values
and do what you want with that. That means best practices must be enforced with
culture or dogma. In Elm you can actually ensure that people write code in a
good way.

## Extensibility

When you have a ton of widgets, all with different sets of actions, how do you
use them all together? [The TodoMVC code][todo] uses a big ADT called `Action`,
but that is not easy to extend. There are a few useful techniques here.

[todo]: https://github.com/evancz/elm-todomvc/blob/master/Todo.elm

### Nesting

So let's say we have three different widgets, each with their code living in
modules called `SearchBar`, `Filters`, and `Results`. This means we have three
sets of actions `SearchBar.Action`, `Filters.Action`, and `Results.Action`
which we do not know anything about. To put them together, we would create some
nested actions and state:

```haskell
data Action
    = SearchBar SearchBar.Action
    | Filters Filters.Action
    | Results Results.Action

type State =
    { searchBar : SearchBar.State
    , filters   : Filters.State
    , results   : Results.State
    }

step : Action -> State -> State
step action state =
  case action of
    SearchBar a ->
        { state |
            searchBar <- SearchBar.step a state.searchBar
        }

    Filters a ->
        { state |
            filters <- Filters.step a state.filters
        }

    Results a ->
        { state |
            results <- Results.step a state.results
        }
```

You can just keep nesting and nesting like this. For example, the `Results`
module may be made up of 4 smaller widgets with their own actions and state
and we don't need to know anything about those details.

### Generalizing Actions

In some cases where you want an ADT to be more extensible. Here is the fully
general approach:

```haskell
type Action = State -> State

removeTask : Int -> Action
removeTask id state =
    { state |
        tasks <- filter (\\task -> task.id /= id) state.tasks
    }
```

We can then expose a set of functions like `removeTask` that let people create
a certain set of `Actions`. So rather than having a step function that handles
the many cases of an ADT, we just apply the `Action` to the state:

```haskell
step : Action -> State -> State
step action state =
    action state
```

It is just function application! This means people can write their own
functions like `(resize : Int -> Int -> State -> State)` just by building up
from the publicly available ways to transition state. This means we can act
on `State` in composable ways without revealing any facts about `State`.

This can also be mixed with the ADT approach like so:

```haskell
data Action
    = RemoveTask Int
    | ...
    | Anything (State -> State)
```

This way you still have the structure of an ADT that helps organize code and
forces you to think hard about what functionality you actually want, but you
can escape that structure if needed using the `Anything` case which will just
step the state in an arbitrary way.

I am not sure how this approach will work out in practice as I personally
favor ADTs, but I think we should explore it more and see how it goes. I can
see it going either way.

### Reusable Actions

So assuming we go with something like `type Action = State -> State`, how can
we reuse an action on different kinds of state? Elm's [extensible
records][records] can help a lot with this. Rather than defining a totally
opaque type `State`, we can build it from smaller pieces:

[records]: http://elm-lang.org/learn/Records.elm

```haskell
type Position r = { r |  x:Float,  y:Float }
type Velocity r = { r | vx:Float, vy:Float }
type Lives    r = { r | lives:Int }
type Coins    r = { r | coins:Int }

type Mario  = Position (Velocity (Lives {}))
type Goomba = Position (Velocity {})
type Brick  = Position (Coins {})

oneUp : Lives r -> Lives r

gravity : Time -> Position (Velocity r) -> Position (Velocity r)

takeCoin : Coins r -> Coins r
```

Now we can use the `oneUp` and `gravity` functions on anything that have the
required fields. So stepping Mario's state forward could look like
`oneUp << gravity dt`. This gives us an interesting way to reuse functions that
may be more clever than is necessary in practice, but at least now you know
it's available to you!

## Specializing Inputs

This section is about conveniences that may need to be added to Elm to fully
realize the vision outlined here.

I think it may be pleasant/necessary to introduce a function something like
this:

```haskell
specialize : (particular -> general) -> Input general -> Input particular
```

Notice that the order of arguments seems a bit wonky here. You are creating a
new input of `particulars` and you need a way to convert all of those to the
more `general` type to integrate with the rest of the system. The idea is that
you can create one input, but have all 3 of your widgets report to it with
things like this:

```haskell
-- using the general Action ADT defined earlier
searchBarInput = specialize SearchBar appInput
filtersInput   = specialize Filters   appInput
resultsInput   = specialize Results   appInput
```

It may also be a good idea to make input creation syntactic as with ports and
to demand that they all be created in the Main module to ensure that people
structure their code in a reusable way.

## Focus

It is possible to give widgets access to a small part of the overall state. Say
you want the `Filters` widget to set some state relevant to the `SearchBox`
widget. One way to do this is to have a `Filters.Action` that knows about that
particular piece of state. You could handle that particular action one level
above in the code that manages the many subcomponents. I think this is fairly
intuitive and will work well enough.

A `Focus` generalizes this idea of giving access to a subpart of a big data
structure. I have tried to [outline the
idea](https://gist.github.com/evancz/78293dc6a4ac2547676c) as I think it should
look in Elm, but I'm not sure it is actually a big win for architecting
applications. It sounds promising, but it has a conceptual complexity cost that
I'm not sure will pay off. My main concern is that, by making it easy to look
deep inside of data structures, it encourages you to stop thinking about how to
make these substructures modular, perhaps leading to an architecture that is
not as nice *and* has extra conceptual complexity. 

The reason for my skepticism is that I'd like to keep the number of new
concepts as low as possible. If we can get by without this, I think Elm will be
more attractive.

"""
