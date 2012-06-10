
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
            , body outer inner
            , size outer 50 . box 8 . text . Text.color mediumGrey $
                toText "&copy; 2011-2012 Evan Czaplicki" 
            ]

----------------------

section = text . bold . Text.height (7/6) . toText
sub s = flow down [ plainText "&nbsp;", text . bold $ toText s ]

info w = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map ((\e x -> e) . width w) $
  [ section "Downloads"
  , sub "Installation"
  , text $ toText "See these " ++ link "https://github.com/evancz/Elm/blob/master/README.md" (toText "install instructions") ++
           toText " to get Elm running on your machine. If you run into problems, you can email the " ++
           link "https://groups.google.com/forum/?fromgroups#!forum/elm-discuss" (toText "mailing list") ++
           toText " or report an issue to Elm's " ++
           link "https://github.com/evancz/Elm" (toText "source repository") ++ toText "."
  , sub "A Large Example"
  , text $ toText "This site is written almost entirely in Elm, and the source code is available on " ++
           link "https://github.com/evancz/elm-lang.org" (toText "github") ++
           toText ". The download links are " ++
           link "https://github.com/evancz/elm-lang.org/downloads" (toText "here") ++
           toText " and include all of the examples on this site. This will make it easier to work through examples on your own machine."
  , sub "Haskell Integration"
  , text $ toText "There are a couple Haskell libraries which make it easier to serve Elm code with Yesod, Snap, HAppStack, etc. "++
           toText "The general Haskell integration libraries are available " ++
           link "http://hackage.haskell.org/package/Elm" (toText "here") ++
           toText " on Hackage. For the Yesod specific package see " ++
           link "https://github.com/evancz/Elm/wiki/Elm-with-Yesod:-Getting-Started" (toText "these directions") ++
           toText ". No matter which web-framework you prefer, be sure to check out these " ++
           link "https://github.com/evancz/Elm/tree/master/Examples" (toText "examples servers") ++
           toText " to get started serving Elm with Haskell."
  , sub "Thesis on Elm"
  , text $ toText "My " ++
           link "http://www.testblogpleaseignore.com/wp-content/uploads/2012/04/thesis.pdf" (toText "thesis on Elm") ++
           toText " is available though. It provides a more formal definition of Elm and a discription of Concurrent FRP, a new " ++
           toText "approach to efficient Functional Reactive Programming."
  ]
 
body outer inner = width outer . box 2 . flow down . (:) (plainText "&nbsp;") $ info inner

main = lift (skeleton body) Window.width
