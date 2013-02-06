
import List (intercalate,intersperse)
import Website.Skeleton

servers =
  [ ("HAppStack",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-happstack") ])
  , ("Yesod",
        [ ("Source", "https://github.com/evancz/Elm/tree/master/Examples/elm-yesod") ])
  ]

libraries =
  [ ("General API",
        [ ("Docs", "http://hackage.haskell.org/package/Elm") ])
  , ("Yesod-Specific API",
        [ ("Docs", "http://hackage.haskell.org/package/elm-yesod")
        , ("Getting Started", "https://github.com/evancz/Elm/wiki/Elm-with-Yesod:-Getting-Started")
        ])
  ]

example (name, loc) = Text.link loc (fromString name)
toLinks (title, links) =
  text $ toText "&nbsp;&nbsp;&nbsp;" ++ italic (toText title) ++ toText " &#8212; " ++
         intercalate (toText ", ") (map example links)

insertSpace lst = case lst of { x::xs -> x :: spacer 1 5 :: xs ; [] -> [] }

subsection w (name,info) =
  flow down . insertSpace . map (width w) $
    (text . bold $ toText name) :: map toLinks info

intro = [markdown|

### Elm + Haskell

There is a Haskell library that compiles Elm code, so you can serve .elm files directly.
This library is not platform specific, so you can use it with Yesod, Snap, HAppStack, or
anything else you can think of.

The actual Elm compiler is more robust than the Haskell library (more flags, more flexibility),
so it may be better to just serve the compiled .html depending on your purpose. Maybe take a look
at the compiler flags before you commit to a method.

|]

content w = intersperse (plainText "&nbsp;") $
  map (subsection w) [ ("Haskell Libraries", libraries), ("Working Examples", servers) ] ++
  [ plainText "The HAppStack example uses the general API, so it should be fairly similar to using the Elm API with Snap." ]

exampleSets w = flow down . map (width w) $ intro :: content w

main = skeleton exampleSets <~ Window.width
