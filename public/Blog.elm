import Website.Skeleton (skeleton)
import Window

blog = [markdown|

# Blog

 * [Interactive Programming / Hot-swapping in Elm](/blog/Interactive-Programming.elm)
 * [Elm &hearts; Prezi](/blog/announce/Elm-and-Prezi.elm)
 * [Escape from Callback Hell](/learn/Escape-from-Callback-Hell.elm)

|]

info w = width w blog

main = lift (skeleton info) Window.dimensions
