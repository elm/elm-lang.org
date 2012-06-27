
import Website.Skeleton (addSpaces, skeleton)
import Website.Tiles (tile)
import Data.List

section = text . bold . Text.height (5/4) . toText

standard = ("General Purpose",
  [ ("Data.Char", "docs/Data/Char.elm")
  , ("Data.Maybe", "docs/Data/Maybe.elm")
  , ("Data.List",  "docs/Data/List.elm")
  , ("Prelude", "docs/Prelude.elm")
  ])

elements = ("Display",
  [ ("Element", "docs/Element.elm")
  , ("Text",  "docs/Text.elm")
  , ("Color", "docs/Color.elm")
  , ("Shape", "docs/Shape.elm")
  , ("Line",  "docs/Line.elm")
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

foreign = ("JavaScript Interface", [ ("Foreign.JavaScript", "docs/Foreign/JavaScript.elm") ])

intro = [ section "Library Documentation"
        , text $ toText "This section provides type-signatures and explanations of Elm's current " ++
                 toText "standard libraries. The standard libraries are broken up into general " ++
                 toText "catagories below. Additional information about Elm can be found in " ++
                 link "http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf" (toText "this thesis") ++
                 toText " and at " ++
                 link "http://www.testblogpleaseignore.com" (toText "this blog") ++ toText "."
        ]

linkify (name, src) = toText "&nbsp;&nbsp;&nbsp;&nbsp;" ++ link src (toText name)
linkList (name, pairs) = 
  flow down . map text $ bold (toText name) : map linkify pairs
links = map linkList [ standard, elements, reaction, foreign ]

categories w =
  flow down . map (width w) . addSpaces $ intro ++ links

main = lift (skeleton categories) Window.width