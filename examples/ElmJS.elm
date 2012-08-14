
import Data.List (intercalate)
import Website.Skeleton

section = text . bold . Text.height (5/4) . toText

basics =
  [ ("Change Page Title",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-js/Title")
        , ("Result", "/code/examples/JavaScript/ChangeTitle.elm")
        ])
  , ("Redirect",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-js/Redirect")
        , ("Result", "/edit/examples/JavaScript/Redirect.elm")
        ])
  , ("Logging",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-js/Logging")
        , ("Result", "/edit/examples/JavaScript/Logging.elm")
        ])
  ]

biggers =
  [ ("Managed Frame Rate / Game Basics",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-js/FrameRate/")
        , ("Result", "/misc/FrameRate.html")
        ])
  , ("Open Street Map",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-js/Maps/")
        , ("Result", "/misc/Map.html")
        ])
  ]

json =
  [ ("Creating Objects",
        [ ("Person" , "/edit/examples/JavaScript/Person.elm")
        , ("People" , "/edit/examples/JavaScript/People.elm")
        ])
  , ("Extracting Info",
        [ ("LookupWithDefault" , "/edit/examples/JavaScript/JsonLookup.elm")
        ])
  ]

example (name, loc) = link loc (fromString name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         intercalate (toText ", ") (map example links)

insertSpace lst = case lst of { x:xs -> x : rectangle 1 5 : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) : map toLinks info

content w =
  [ section "Elm + JavaScript"
  , text $ toText "Elm integrates with JavaScript, so if it cannot be done in Elm you can fall back to JavaScript (" ++
           link "http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5javascript-integration-signal-filters-and-more/" (toText "details") ++
           toText "). There are also a number of libraries that help with JavaScript integration, listed " ++
           link "/Documentation.elm" (toText "here") ++
           toText ". Now for some examples."
  ] ++ map (subsection w) [ ("JSON", json), ("Basic I/O", basics), ("Larger Examples", biggers) ]

exampleSets w = flow down . map (width w) . addSpaces $ content w

main = lift (skeleton exampleSets) Window.width
