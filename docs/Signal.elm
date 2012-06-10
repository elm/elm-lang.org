
button (name, href) = size 100 60 . Element.link href . size 100 60 . box 5 $ plainText name
buttons = size 400 60 . flow right . List.map button $
  [ ("Home","/"), ("Examples","/Examples.elm"), ("Docs","/Documentation.elm"), ("Download","/Download.elm") ]

title w = size w 60 . box 4 . text . header . toText $ "Elm"

lightGrey = rgb (245/255) (245/255) (245/255)
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

addSpaces px = List.map (\f -> f ()) . List.intersperse (\x -> height px $ plainText "") . List.map (\e x -> e)

----------------------

section = text . bold . Text.height (5/4) . toText

darkerGrey = rgb (114/255) (124/255) (129/255)
entry w (name, type, desc) =
  flow down . List.map (width w) $
    [ text . monospace . toText $ name ++ " :: " ++ type
    , text . Text.color darkerGrey . toText $ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" ++ desc ]

group w (name, fs) = flow down . addSpaces 5 . List.map (width w) $ (text . bold $ toText name) : List.map (entry w) fs

createDocs name cats =
  let f w = flow down . addSpaces 30 $ section name : List.map (group w) cats in
  lift (skeleton f) Window.width

----------------------


lifts =
  [ ("constant", "a -> Signal a", "Create a constant signal that never changes.")
  , ("lift", "(a -> b) -> Signal a -> Signal b", "Transform a signal with a given function.")
  , ("lift2", "(a -> b -> c) -> Signal a -> Signal b -> Signal c", "Combine two signals with a given function.")
  , ("lift3", "(a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d", "Combine three signals with a given function.")
  , ("lift4", "(a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e", "Combine four signals with a given function.")
  ]

folds =
  [ ("foldp", "(a -> b -> b) -> b -> Signal a -> Signal b", "Create a past-dependent signal. Each value given on the input signal will be accumulated, producing a new output value. For instance, (foldp (\t count -> count + 1) 0 (Time.every 3) counts up every time the timer ticks.") ]

categories = 
  [ ("Lifts (Transforming and Combining Signals)", lifts)
  , ("Folds (Past-Dependent Transformations)", folds)
  ]

main = createDocs "Signal" categories