import Graphics.Element exposing (..)
import Markdown
import Website.Skeleton exposing (skeleton)
import Website.Tiles as Tile
import Window

port title : String
port title = "Elm 0.15"


main =
  Signal.map (skeleton "Blog" everything) Window.dimensions


everything wid =
  let w = min 600 wid
  in
    flow down
      [ width w content
      ]

content = Markdown.toElement """

# Tasks

Tasks make it easy to describe asynchronous operations that may fail, like
HTTP requests or writing to a database. Tons of browser APIs are described as
tasks in Elm:

  * [elm-http][] &mdash; talk to servers
  * [elm-history][] &mdash; navigate browser history
  * [elm-storage][] &mdash; save info in the users browser

[elm-http]: http://package.elm-lang.org/packages/evancz/elm-http/latest/
[elm-history]: https://github.com/TheSeamau5/elm-history/
[elm-storage]: https://github.com/TheSeamau5/elm-storage/

Tasks also work like light-weight threads in Elm, so you can have a bunch of
tasks running at the same time and the [runtime][rts] will hop between them if
they are blocked.

[rts]: http://en.wikipedia.org/wiki/Runtime_system


## Basic Example

As a simple example, let’s get the README for Elm&rsquo;s core libraries from
the [Elm Package Catalog](http://package.elm-lang.org/).

```haskell
import Http

pkgUrl =
  "http://package.elm-lang.org/packages/elm-lang/core/latest/README.md"

getReadme : Task Http.Error String
getReadme =
  Http.getString pkgUrl
```

So `getReadme` is a `Task` that can be performed by Elm&rsquo;s runtime. When
we run the task, it will either fail with an [`Http.Error`][error] or succeed
with a string of markdown.

[error]: http://package.elm-lang.org/packages/evancz/elm-http/latest/Http#Error


## Basic Example

```elm
sleep : Time -> Task x ()
```

```elm
Http.getString : String -> Task Http.Error String
```


## Chain Tasks

```elm
andThen : Task x a -> (a -> Task x b) -> Task x b
```


## Perform Tasks



"""