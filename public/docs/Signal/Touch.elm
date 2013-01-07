
import Website.Docs (createDocs2)

touches =
  [ ("touches", "Signal [{ x :: Int, y :: Int, id :: Int }]", [markdown|
A list of touches. Each ongoing touch is represented by a set of `x`
and `y` coordinates and an identifier `id` that allows you to distinguish
between different touches.
|])
  ]

categories = [ ("Touches", touches) ]

intro = [markdown|
This is an early version of the touch library. It will likely grow
to include gestures that would be useful for both games and web-pages.
|]

main = createDocs2 "Touch" intro categories