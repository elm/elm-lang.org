
import Website.Skeleton (addSpaces, skeleton)
import Website.Tiles (tile)
import Data.List
import Graphics.Text as Text

standard = ("General Purpose",
  [ ("Data.Char", "docs/Data/Char.elm")
  , ("Data.Maybe", "docs/Data/Maybe.elm")
  , ("Data.List",  "docs/Data/List.elm")
  , ("Data.Map",  "docs/Data/Map.elm")
  , ("Data.Set",  "docs/Data/Set.elm")
  , ("Prelude", "docs/Prelude.elm")
  ])

elements = ("Graphics",
  [ ("Graphics.Element", "docs/Graphics/Element.elm")
  , ("Graphics.Color"  , "docs/Graphics/Color.elm")
  , ("Graphics.Text",  "docs/Graphics/Text.elm")
  ])

reaction = ("Signals",
  [ ("Signal" , "docs/Signal/Signal.elm")
  , ("Signal.Mouse"  , "docs/Signal/Mouse.elm")
  , ("Signal.Keyboard.Raw", "docs/Signal/KeyboardRaw.elm")
  , ("Signal.Window" , "docs/Signal/Window.elm")
  , ("Signal.Time"   , "docs/Signal/Time.elm")
  , ("Signal.HTTP"   , "docs/Signal/HTTP.elm")
  , ("Signal.Input"  , "docs/Signal/Input.elm")
  , ("Signal.Random" , "docs/Signal/Random.elm")
  ])

ffi = ("JavaScript Interface",
  [ ("Foreign.JavaScript", "docs/Foreign/JavaScript.elm") 
  , ("Foreign.JavaScript.Experimental", "docs/Foreign/JavaScript/Experimental.elm") 
  , ("Foreign.JavaScript.JSON", "docs/Foreign/JavaScript/JSON.elm") 
  ])

intro = [markdown|

### Library Documentation

This section provides type-signatures and explanations of Elm's current
standard libraries.

[This blog][1] is the source of some discussion and
announcements. It also has more detailed information on the [module system][2]
and [JavaScript integration][3].

My [thesis][4] gives a more formal specification of Elm.

  [1]: http://www.testblogpleaseignore.com "Elm blog"
  [2]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "module system"
  [3]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript integration"
  [4]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "Elm Thesis"

|]

linkify (name, src) = toText "    " ++ Text.link src (toText name)
linkList (name, pairs) = 
  flow down . map text $ bold (toText name) : map linkify pairs
links = map linkList [ standard, elements, reaction, ffi ]

categories w =
  flow down . map (width w) . intersperse (plainText "&nbsp;") $ intro : links

main = lift (skeleton categories) Window.width