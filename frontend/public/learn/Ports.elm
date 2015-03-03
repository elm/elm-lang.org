import Graphics.Element exposing (..)
import Markdown
import Signal exposing (Signal, (<~))
import Website.Skeleton exposing (skeleton)
import Website.ColorScheme
import Window

port title : String
port title = "Ports"


main : Signal Element
main =
  skeleton "Learn" (\w -> width (min 600 w) intro) <~ Window.dimensions


intro : Element
intro = Markdown.toElement """

# Ports

Ports are a general purpose way to communicate with JavaScript. They let you
send messages in and out of Elm so you can use JavaScript whenever you need to.

[This example](https://github.com/evancz/elm-html-and-js) and
[this example](https://gist.github.com/evancz/8521339) show
code you can take a look at, but this document will explain
the specifics of what you can and cannot do with ports.

## From JavaScript to Elm

To send messages from JavaScript to Elm, you use an incoming port like this:

```haskell
port addUser : Signal (String, UserRecord)
```

This means we now have a signal in Elm called `addUser` that is updated
by some code in JavaScript. To actually send messages to this port, we would
write something like this in JavaScript:

```javascript
myapp.ports.addUser.send([ "Tom", { age: 32, job: "lumberjack" } ]);
myapp.ports.addUser.send([ "Sue", { age: 37, job: "accountant" } ]);
```

This sends two updates to Elm, automatically converting to values that work
well in Elm.

## From Elm to JavaScript

To send messages from Elm to JavaScript, you define an outgoing port like this:

```haskell
port requestUser : Signal String
port requestUser =
    signalOfUsersWeWantMoreInfoOn
```

In this case we are taking a signal that exists in Elm and sending each of its
values to JavaScript. On the JavaScript side, we handle these messages by
subscribing to that port:

```javascript
myapp.ports.requestUser.subscribe(databaseLookup);

function databaseLookup(user) {
    var userInfo = database.lookup(user);
    myapp.ports.addUser.send(user, userInfo);
}
```

We have subscribed to the `requestUser` port and will actually go and do the
database lookup. When we get the results, we send them back into Elm using
another port. Perhaps at some point you will need to unsubscribe from a port,
in which case you would do this:

```javascript
myapp.ports.requestUser.unsubscribe(databaseLookup);
```

Check out [this example](https://github.com/evancz/elm-html-and-js) or
[this example](https://gist.github.com/evancz/8521339) to see these ideas in
working code.

## Customs and Border Protection

Ports must be careful about what values are allowed through.
Elm is statically typed, so each port is fitted with some
border protection code that ensures that type errors are kept
out. Ports also do some conversions so that you get nice
colloquial data structures in both Elm and JS.

The particular types that can be sent in and out of ports is quite flexible,
covering [all valid JSON values](http://www.json.org/). Specifically, incoming
ports can handle all the following Elm types:

  * **Booleans and Strings** &ndash; both exist in Elm and JS!
  * **Numbers** &ndash; Elm ints and floats correspond to JS numbers
  * **Lists**   &ndash; correspond to JS arrays
  * **Arrays**  &ndash; correspond to JS arrays
  * **Tuples**  &ndash; correspond to fixed-length, mixed-type JS arrays
  * **Records** &ndash; correspond to JavaScript objects
  * **Signals** &ndash; correspond to event streams in JS
  * **Maybes**  &ndash; `Nothing` and `Just 42` correspond to `null` and `42` in JS
  * **Json**    &ndash; [`Json.Value`](http://package.elm-lang.org/packages/elm-lang/core/latest/Json) corresponds to arbitrary JSON

All conversions are symmetric and type safe. If someone tries to give a
badly typed value to Elm it will throw an error in JS immediately. By having
a border check like this, Elm code can continue to guarantee that you will
never have type errors at runtime.

Outgoing ports let you export all of the values listed above with
one important addition: first-order functions!
If you wrote a nice parser or library in Elm, you can use those functions
directly in JS. The mapping between Elm and JS function looks like this:

```haskell
add x y = x + y
```
```javascript
function add(x,y) { return x + y; }
```

You lose currying on the JS side, but the goal of this whole feature is to
produce *colloquial* values in both Elm and JS. One important restriction on
exporting functions is that they must be *first-order* functions. Things
like `map` and `foldl` cannot be exported because the Elm compiler may
eventually perform optimizations that assume purity, and higher-order
functions allow you to introduce impure functions which *could* be executed
in an unexpected order.

"""
