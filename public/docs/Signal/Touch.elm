
import Website.Docs (createDocs2)

touches =
  [ ("touches", "Signal [{ x :: Int, y :: Int, id :: Int }]", [markdown|
A list of touches. Each ongoing touch is represented by a set of `x`
and `y` coordinates and an identifier `id` that allows you to distinguish
between different touches.
|])
  ]

gestures =
  [ ("taps", "Signal [{ x :: Int, y :: Int }]", [markdown|
The last position that was tapped. Default value is `{x=0,y=0}`. Updates
whenever the user taps the screen.
|])
  ]

categories = [ ("Touches", touches)
             , ("Gestures", gestures) ]

intro = [markdown|
This is an early version of the touch library. It will likely grow
to include gestures that would be useful for both games and web-pages.
|]

main = createDocs2 "Touch" intro categories