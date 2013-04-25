
import Website.Skeleton
import Window as Window
import Graphics.Text (text)
import Graphics.Text as Text

general = ("General",
  [ ("Char", "docs/Char.elm")
  , ("Date", "docs/Date.elm")
  , ("Prelude", "docs/Prelude.elm")
  ])
containers = ("Containers",
 [("List",  "docs/List.elm"),
  ("Dict",  "docs/Dict.elm"),
  ("Set",  "docs/Set.elm"),
  ("Maybe", "docs/Maybe.elm"),
  ("Either", "docs/Either.elm")])

graphics = ("Graphics",
  [ ("Graphics", "docs/Graphics/Element.elm")
  , ("Color"  , "docs/Graphics/Color.elm")
  , ("Text",  "docs/Graphics/Text.elm")
  , ("Input"  , "docs/Signal/Input.elm")
  ])

signals = ("Interaction",
  [("Signal" , "docs/Signal.elm"),
   ("Automaton", "docs/Automaton.elm")])
userInput = ("User Input",
  [("Mouse"  , "docs/Mouse.elm"),
   ("Keyboard","docs/Keyboard.elm"),
   ("Touch"  , "docs/Touch.elm")])
systemInput = ("System Input",
  [("Window" , "docs/Window.elm"),
   ("Time"   , "docs/Time.elm"),
   ("Random" , "docs/Random.elm"),
   ("Http"   , "docs/Http.elm"),
   ("WebSocket", "docs/WebSocket.elm")])
ffi = ("JavaScript",
  [ ("JavaScript", "docs/JavaScript.elm") 
  , ("Json", "docs/Json.elm") 
  , ("Json.Experimental", "docs/Json/Experimental.elm") 
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

linkify (name, src) = Text.toText "    " ++ Text.link src (Text.toText name)
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
