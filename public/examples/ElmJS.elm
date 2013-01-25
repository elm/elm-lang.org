
import List (intercalate,intersperse)
import Website.Skeleton

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
  [ ("Open Street Map",
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
        [ ("Lookup" , "/edit/examples/JavaScript/Lookup.elm")
        , ("Find with Default" , "/edit/examples/JavaScript/FindWithDefault.elm")
        ])
  , ("Modifying Fields",
        [ ("Insert and Delete" , "/edit/examples/JavaScript/Modify.elm")
        ])
  , ("Live Example",
        [ ("Zip Code Lookup" , "/edit/examples/JavaScript/ZipCodes.elm")
        ])
  ]

example (name, loc) = Text.link loc (fromString name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         intercalate (toText ", ") (map example links)

insertSpace lst = case lst of { x:xs -> x : spacer 1 5 : xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) : map toLinks info

intro =   [markdown|

### Elm + JavaScript

Elm integrates with JavaScript, so if it cannot be done in Elm you can fall
back to JavaScript ([details][1]). There are also a number of libraries that
help with JavaScript integration, listed [here](/Documentation.elm "docs").
Now for some examples.

  [1]: http://www.testblogpleaseignore.com/2012/06/29/announcing-elm-0-3-5javascript-integration-signal-filters-and-more/ "details"
|]

content w =
  width w intro : map (subsection w) [ ("JSON", json), ("Basic I/O", basics), ("Larger Examples", biggers) ]

exampleSets w = flow down . intersperse (plainText "&nbsp;") $ content w

main = skeleton exampleSets <~ Window.width
