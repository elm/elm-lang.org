
import Website.Skeleton (skeleton)
import Window

general = ("General",
  [ ("Char", "Char.elm")
  , ("Date", "Date.elm")
  , ("Prelude", "Prelude.elm")
  ])
containers = ("Containers",
 [("List",  "List.elm"),
  ("Dict",  "Dict.elm"),
  ("Set",  "Set.elm"),
  ("Maybe", "Maybe.elm"),
  ("Either", "Either.elm")])

graphics = ("Graphics",
  [ ("Element" , "Graphics/Element.elm")
  , ("Collage" , "Graphics/Collage.elm")
  , ("Input"   , "Graphics/Input.elm")
  , ("Color"   , "Color.elm")
  , ("Text"    , "Text.elm")
  , ("Matrix2D", "Matrix2D.elm")
  ])

signals = ("Interaction",
  [("Signal" , "Signal.elm"),
   ("Automaton", "Automaton.elm")])
userInput = ("User Input",
  [("Mouse"  , "Mouse.elm"),
   ("Keyboard","Keyboard.elm"),
   ("Touch"  , "Touch.elm")])
systemInput = ("System Input",
  [("Window" , "Window.elm"),
   ("Time"   , "Time.elm"),
   ("Random" , "Random.elm"),
   ("Http"   , "Http.elm"),
   ("WebSocket", "WebSocket.elm")])
ffi = ("JavaScript",
  [ ("JavaScript", "JavaScript.elm") 
  , ("Json", "Json.elm") 
  , ("JavaScript.Experimental", "JavaScript/Experimental.elm") 
  ])

intro = [markdown|

### Library Documentation

This section provides type-signatures and explanations of Elm's current
standard libraries.

<br/>

|]

outro = [markdown|

<br/>

### Syntax, Semantics, and More

There are more in depth introductions to many of the concepts in Elm in [the
About section](/About.elm).

[The Syntax of Elm][syntax] is a brief overview of all of Elm&rsquo;s syntax.

  [syntax]: /learn/Syntax.elm "The Syntax of Elm"

My [thesis][4] discusses the semantics of FRP in
Elm and how to make FRP efficient. It also provides a
detailed history of FRP in the *Related Works* section.

Look [here](/learn/What-is-FRP.elm) to answer the question
&ldquo;What is Functional Reactive Programming?&rdquo;

[This blog][1] is the source of some discussion and
announcements. It also has more detailed information on the [module system][2]
and [JavaScript integration][3].

  [1]: http://www.testblogpleaseignore.com "Elm blog"
  [2]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "module system"
  [3]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript integration"
  [4]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "Elm Thesis"

<br/>

|]

linkify (name, src) =
    Text.toText "    " ++ Text.link ("docs/" ++ src) (Text.toText name)
linkList (name, pairs) = 
  flow down . intersperse (spacer 2 2) . map Text.text $
  Text.bold (Text.toText name) :: map linkify pairs

makeCol w = width w . flow down . intersperse (spacer 10 20) . map linkList
threeCol w l m r =
    flow right $ map (makeCol (w `div` 3)) [l,m,r]

col1 = [ general, containers ]
col2 = [ signals, userInput, systemInput ]
col3 = [ graphics, ffi ]

categories w =
  flow down [ width w intro,
              threeCol w col1 col2 col3,
              width w outro ]

main = lift (skeleton categories) Window.width
