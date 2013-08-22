
import Website.Skeleton (skeleton)
import Website.Tiles (toTile, tile)
import Window

addFolder folder = map (\(x,y) -> (x, y, folder))

intermediate = addFolder "Showcase/"
  [ 
  ]

intro =  [markdown|

### Elm Showcase

|]

content w =
  let tiles = tile w <| map toTile intermediate
  in  width w intro `above` container w (heightOf tiles) midTop tiles

main = lift (skeleton content) Window.width
