
import List (intercalate,intersperse)
import Website.Skeleton
import Website.ColorScheme

addFolder folder lst =
  let add (x,y) = (x, folder ++ y ++ ".elm") in
  let f (n,xs) = (n, map add xs) in
  map f lst

elements = addFolder "Elements/"
  [ ("Primitives",
        [ ("Text"  , "HelloWorld")
        , ("Images", "Image")
        , ("Fitted Images", "FittedImage")
        , ("Videos", "Video")
        , ("Markdown", "Markdown")
        ])
  , ("Formatting",
        [ ("Size"    , "Size")
        , ("Opacity" , "Opacity")
        , ("Text"    , "Text")
        , ("Typeface", "Typeface")
        ])
  , ("Layout",
        [ ("Simple Flow", "FlowDown1a")
        , ("Flow Down"  , "FlowDown2")
        , ("Layers"     , "Layers")
        , ("Positioning", "Position")
        , ("Spacers"    , "Spacer")
        ])
  , ("Collage", [ ("Lines"     , "Lines")
                , ("Shapes"    , "Shapes")
                , ("Sprites"   , "Sprite")
                , ("Elements"  , "ToForm")
                , ("Colors"    , "Color")
                , ("Textures"  , "Texture")
                , ("Transforms", "Transforms")
                ])
  ]


functional = addFolder "Functional/"
  [ ("Recursion",
        [ ("Factorial"  , "Factorial")
        , ("List Length", "Length")
        , ("Zip"        , "Zip")
        , ("Quick Sort" , "QuickSort")
        ])
  , ("Functions",
        [ ("Anonymous Functions", "Anonymous")
        , ("Application"        , "Application")
        , ("Composition"        , "Composition")
        , ("Infix Operators"    , "Infix")
        ])
  , ("Higher-Order",
        [ ("Map"    , "Map")
        , ("Fold"   , "Sum")
        , ("Filter" , "Filter")
        , ("ZipWith", "ZipWith")
        ])
  , ("Data Types",
        [ ("Maybe", "Maybe")
        , ("Boolean Expressions", "BooleanExpressions")
        , ("Tree", "Tree")
        ])
  ]

reactive = addFolder "Reactive/"
  [ ("Mouse",  [ ("Position", "Position")
               , ("Presses"    , "IsDown")
               , ("Clicks"    , "CountClicks")
               , ("Position+Image", "ResizeYogi")
               , ("Position+Collage"    , "Transforms")
               -- , ("Hover"     , "IsAbove")
               ])
  ,("Keyboard",[ ("Keys Down"  , "KeysDown")
               , ("Key Presses", "CharPressed")
               ])
  , ("Window", [ ("Size", "ResizePaint")
               , ("Centering", "Centering")
               ])
  , ("Time",   [ ("Before and After", "Between")
               , ("Every"           , "Every")
               , ("Clock"           , "Clock")
               ])
  , ("Input",  [ ("Text Fields", "TextField")
               , ("Passwords"  , "Password")
               , ("Check Boxes", "CheckBox")
               , ("String Drop Down", "StringDropDown")
               , ("Drop Down", "DropDown")
               ])
  , ("Random", [ ("Randomize", "Randomize") ])
  , ("HTTP",   [ ("Zip Codes", "ZipCodes") ])
  , ("Filters",[ ("Sample", "SampleOn")
               , ("Keep If", "KeepIf")
               , ("Drop Repeats", "DropRepeats")
               ])
  ]

js = [markdown|

<img src="/yogi.jpg"
     style="visibility:hidden;width:0;"
     onload="body.onload=function(){var p=top.location.pathname;if (p.slice(0,15) == '/edit/examples/') { var e=document.createEvent('Event');e.initEvent('location', true, true);e.value = p.slice(15).split('/')[0];document.dispatchEvent(e);}}" />

|]

example (name, loc) =
    let href = "javascript:top.location.href='/edit/examples/" ++ loc ++ "';" in
    text $ Text.height 0.8 (toText "   " ++ Text.link href (toText name))
toLinks (title, links) =
  flow down $ (text . bold $ toText title) :: map example links

subsection w info =
  flow right $ map (width (w `div` length info) . toLinks) info

foreign import jsevent "location"
  (JavaScript.castStringToJSString "Elements")
  locs : Signal JSString

location = lift JavaScript.castJSStringToString locs

withLoc loc a b c = 
    if loc == "Elements"   then a else
    if loc == "Reactive"   then b else
    if loc == "Functional" then c else a

navTo loc = concat [ "javascript:var e=document.createEvent('Event');"
                   , "e.initEvent('location', true, true);"
                   , "e.value = '", loc, "';"
                   , "document.dispatchEvent(e);"
                   , "void(0);" ]

navLink w h loc name =
    Graphics.link (navTo loc) . container w (h `div` 3) middle $
            text (Text.height 1.5 (toText name))

act = color lightGrey
pas = color mediumGrey
links w h loc =
  flow down [ withLoc loc act pas pas $ navLink w h "Elements" "Display"
            , withLoc loc pas act pas $ navLink w h "Reactive" "React"
            , withLoc loc pas pas act $ navLink w h "Functional" "Compute" ]

content (w,h) loc =
  let exs = withLoc loc elements reactive functional in
  flow right [ layers [ js, links 130 h loc ]
             , color lightGrey $ spacer 20 h
             , color lightGrey $ Graphics.height h $ subsection (w-150) exs ]

main = lift2 content Window.dimensions location
