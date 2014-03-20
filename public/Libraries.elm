import Website.Skeleton (skeleton)
import String
import Window

general = ("General",
  [ "Basics"
  , "String"
  , "Regex"
  , "Char"
  , "Date"
  , "Bitwise"
  , "Trampoline"
  , "Debug"
  ])
containers = ("Containers",
  [ "List"
  , "Dict"
  , "Set"
  , "Maybe"
  , "Either"
  ])

graphics = ("Graphics",
  [ "Graphics.Element"
  , "Graphics.Collage"
  , "Graphics.Input"
  , "Graphics.Input.Field"
  , "Color"
  , "Text"
  , "Transform2D"
  ])

signals = ("Interaction",
  [ "Signal"
  ])
userInput = ("User Input",
  [ "Mouse"
  , "Keyboard"
  , "Touch"
  ])
systemInput = ("System Input",
  [ "Window"
  , "Time"
  , "Random"
  , "Http"
  , "WebSocket"
  ])
ffi = ("JavaScript",
  [ "JavaScript"
  , "Json"
  , "JavaScript.Experimental"
  ])

intro = [markdown|

# Libraries

The Standard Libraries, listed below, come with the latest release of the Elm
compiler and make it easy to get productive with Elm. Search for
functions like `map` or `fold` in the standard library
[by filtering](http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/).

See the [syntax reference](/learn/Syntax.elm) and [other learning
resources](/Learn.elm) to learn more about the language itself.
Check out [library.elm-lang.org](http://library.elm-lang.org) to discover
3rd party libraries and browse their documentation.

|]

deslash c = if c == '.' then '-' else c

linkify name =
    let path = "http://library.elm-lang.org/catalog/evancz-Elm/0.11.2/" ++ String.map deslash name
    in  Text.toText "  " ++ Text.link path (Text.toText name)

linkList (name, pairs) = 
  flow down . intersperse (spacer 2 4) . map Text.leftAligned <|
  Text.height 18 (Text.toText name) :: map linkify pairs

makeCol w = width w . flow down . intersperse (spacer 10 20) . map linkList
threeCol w l m r =
    flow right <| map (makeCol (min 220 <| w `div` 3)) [l,m,r]

col1 = [ general, containers ]
col2 = [ signals, userInput, systemInput ]
col3 = [ graphics, ffi ]

content w =
  flow down
  [ width w intro
  , spacer w 20
  , threeCol w col1 col2 col3
  ]

main = lift (skeleton content) Window.dimensions
