
import Website.Skeleton (addSpaces, skeleton)
import Website.Tiles (tile)
import List
import Graphics.Text as Text

standard = ("General Purpose",
  [ ("List",  "docs/Data/List.elm")
  , ("Dict",  "docs/Data/Dict.elm")
  , ("Set",  "docs/Data/Set.elm")
  , ("Char", "docs/Data/Char.elm")
  , ("Maybe", "docs/Data/Maybe.elm")
  , ("Prelude", "docs/Prelude.elm")
  ])

graphics = ("Graphics",
  [ ("Graphics.Element", "docs/Graphics/Element.elm")
  , ("Graphics.Color"  , "docs/Graphics/Color.elm")
  , ("Graphics.Text",  "docs/Graphics/Text.elm")
  ])

input = ("Interaction",
  [ ("Signal" , "docs/Signal/Signal.elm")
  , ("Mouse"  , "docs/Signal/Mouse.elm")
  , ("Keyboard.Raw", "docs/Signal/KeyboardRaw.elm")
  , ("Window" , "docs/Signal/Window.elm")
  , ("Input"  , "docs/Signal/Input.elm")
  , ("Time"   , "docs/Signal/Time.elm")
  , ("HTTP"   , "docs/Signal/HTTP.elm")
  , ("Random" , "docs/Signal/Random.elm")
  ])


ffi = ("JavaScript",
  [ ("JavaScript", "docs/Foreign/JavaScript.elm") 
  , ("JavaScript.Experimental", "docs/Foreign/JavaScript/Experimental.elm") 
  , ("JSON", "docs/Foreign/JavaScript/JSON.elm") 
  ])

intro = [markdown|

### Library Documentation

This section provides type-signatures and explanations of Elm's current
standard libraries.

<br/>

|]

outro = [markdown|

<br/>

### More Info

Look [here](/learn/What-is-FRP.elm) to answer the question
&ldquo;What is Functional Reactive Programming?&rdquo;

My [thesis][4] discusses the theory behind FRP in Elm and how to make FRP efficient. It also provides a
detailed history of FRP in the *Related Works* section.

[This blog][1] is the source of some discussion and
announcements. It also has more detailed information on the [module system][2]
and [JavaScript integration][3].

  [1]: http://www.testblogpleaseignore.com "Elm blog"
  [2]: http://www.testblogpleaseignore.com/2012/06/19/announcing-elm-0-3-modules/ "module system"
  [3]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5-javascript-integration-signal-filters-and-more/ "JavaScript integration"
  [4]: http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf "Elm Thesis"

<br/>

|]

linkify (name, src) = toText "    " ++ Text.link src (toText name)
linkList (name, pairs) = 
  flow down . map text $ bold (toText name) : map linkify pairs

threeCol w l m r =
  let w' = w `div` 3 in
  flow right [ width w' l, width w' m, width w' r ]

categories w =
  flow down
  [ width w intro
  , threeCol w (linkList standard) (linkList input) (flow down [ linkList graphics, spacer 10 40, linkList ffi ])
  , width w outro
  ]

main = lift (skeleton categories) Window.width