
button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (240/255) (241/255) (244/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title (inner-400) `beside` buttons

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , width outer . box 2 $ plainText "&nbsp;" `above` body inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

addSpaces = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

linkify (name, src) = link src (fromString name)
linkList (name, pairs) =
  bold (fromString $ name ++ ": ") ++ List.intercalate (fromString ", ") (List.map linkify pairs)

standard = ("General Purpose",
  [ ("List",  "docs/List.elm")
  , ("Prelude", "docs/Prelude.elm")
  ])

elements = ("Basic Display",
  [ ("Element", "docs/Element.elm")
  , ("Text",  "docs/Text.elm")
  ])

forms = ("Collage",
  [ ("Color", "docs/Color.elm")
  , ("Shape", "docs/Shape.elm")
  , ("Line",  "docs/Line.elm")
  ])

reaction = ("Reactive Values",
  [ ("Signal" , "docs/Signal.elm")
  , ("Mouse"  , "docs/Mouse.elm")
  , ("Time"   , "docs/Time.elm")
  , ("HTTP"   , "docs/HTTP.elm")
  , ("Window" , "docs/Window.elm")
  , ("Input"  , "docs/Input.elm")
  , ("Random" , "docs/Random.elm")
  ])

intro = [ section "Library Documentation"
        , text $ toText "This section provides type-signatures and explanations of Elm's current " ++
                 toText "standard libraries. The standard libraries are broken up into general " ++
                 toText "catagories below. Additional information about Elm can be found in " ++
                 link "http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf" (toText "this thesis") ++
                 toText "."
        ]
links = List.map (text . linkList) [ standard, elements, forms, reaction ]

categories w =
  flow down . List.map (width w) . addSpaces $ intro ++ links

main = lift (skeleton categories) Window.width