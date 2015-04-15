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

A task is an asynchronous operation.


## Basic Example

```elm
sleep : Time -> Task x ()
```

```elm
Http.getString : String -> Task Http.Error String
```


## Chaining Tasks

```elm
andThen : Task x a -> (a -> Task x b) -> Task x b
```


"""