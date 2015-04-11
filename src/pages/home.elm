import Graphics.Element as E exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Text
import Window

import TopBar
import Center


main =
  Signal.map view Window.width


view w =
  div []
    [ TopBar.topBar "home"
    , splash
    , debuggerSection w
    , bulletSection w
    , exampleSection w
    ]


(=>) = (,)


-- SPLASH

splash =
  div [ id "splash" ]
    [ div [ size 100 16 ] [ text "elm" ]
    , div [ size 26 8 ] [ text "the best of functional programming in your browser" ]
    , div [ size 16 4 ] [ text "writing great code should be easy ... now it is" ]
    , div [ size 26 30 ]
        [ a [ href "/try" ] [ text "try" ]
        , span [ style [ "font-size" => "16px" ] ] [ text " \x00A0 or \x00A0 " ]
        , a [ href "/install" ] [ text "install" ]
        ]
    ]


size height padding =
  style
    [ "font-size" => (toString height ++ "px")
    , "padding" => (toString padding ++ "px 0")
    ]


-- CODE SNIPPET / DEBUGGER

debuggerSection w =
  section []
    [ h2 [ style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"] ] [ text "Hello HTML" ]
    , p [ style [ "text-align" => "center" ] ]
        [ code
            [ class "lang-haskell hljs"
            , style [ "display" => "inline-block", "border-radius" => "16px", "padding" => "24px 48px" ]
            ]
            [ span [class "hljs-title"] [text "main"]
            , text " = span [class "
            , span [class "hljs-string"] [text "\"welcome-message\""]
            , text "] [text "
            , span [class "hljs-string"] [text "\"Hello, World!\""]
            , text "]"
            ]
        ]
    , div [ style [ "display" => "block", "margin" => "2em auto 0", "width" => "600px" ] ]
        [ p [ style [ "text-align" => "center" ] ]
            [ text "Writing HTML is super easy with "
            , a [href "http://package.elm-lang.org/packages/evancz/elm-html/latest/"] [ code [] [text "elm-html"] ]
            , text ". Not only does it render "
            , a [href "/blog/Blazing-Fast-Html.elm"] [text "extremely fast"]
            , text ", it also guides you towards "
            , a [href "https://github.com/evancz/elm-architecture-tutorial/"] [text "well-architected code"]
            , text "."
            ]
        ]
    ]


codeBlock = """

```haskell
main = span [class "welcome-message"] [text "Hello, World!"]
```

"""


-- FEATURES

bulletSection w =
  section []
    [ h1 [ style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"] ] [text "Features"]
    , bulletRow w bulletsRowOne
    , bulletRow w bulletsRowTwo
    ]


bulletRow w (one, two, three) =
  let sidePad = toString ((w-1020) // 2) ++ "px"
  in
  section [ style [ "height" => "240px", "padding" => ("0 " ++ sidePad) ] ]
    [ section [ style [ "float" => "left", "width" => "300px" ] ] one
    , section [ style [ "float" => "left", "width" => "300px", "padding" => "0 60px" ] ] two
    , section [ style [ "float" => "left", "width" => "300px" ] ] three
    ]


bulletsRowOne =
  ( [ h2 [] [ text "No runtime exceptions"]
    , p []
      [ text "Elm’s compiler is amazing at finding errors before they can impact your users. The only way to get Elm code to throw a runtime exception is by explicitly invoking "
      , a [href "http://package.elm-lang.org/packages/elm-lang/core/1.1.0/Debug#crash"] [code [] [text "crash"]]
      , text "."
      ]
    , p [] [text "Seriously. You read that right."]
    ]
  , [ h2 [] [text "Blazing fast rendering"]
    , p [] [text "The elm-html View library outperforms even React.js in TodoMVC benchmarks. Optimizing your View is easy with lazy. Elm is designed to make building high-performance UIs a straightforward experience."]
    ]
  , [ h2 [] [text "Libraries with guarantees"]
    , p [] [text "The Elm Package Manager enforces semantic versioning. Any breaking API changes are detected automatically and cannot reach the repository without a major version bump. You can upgrade with confidence."]
    ]
  )


bulletsRowTwo =
  ( [ h2 [] [text "Clean syntax"]
    , p [] [text "No semicolons. No mandatory parentheses for function calls. Everything is an expression. For even more concise code there’s also destructuring assignment, pattern matching, automatic currying, and more."]
    ]
  , [ h2 [] [text "Smooth JavaScript interop"]
    , p [] [text "No need to reinvent the wheel when there’s a JavaScript library that already does what you need. Thanks to Elm’s simple Ports system, your Elm code can communicate with JavaScript without sacrificing guarantees."]
    ]
  , [ h2 [] [text "Time-traveling debugger"]
    , p [] [text "What if you could pause time and replay all recent user inputs? What if you could make a code change and watch the results replay without a page refresh? Try it out and see for yourself!"]
    ]
  )


-- EXAMPLES

exampleSection w =
  let sidePad = toString ((w-960) // 2) ++ "px"
  in
  section []
    [ h1 [ style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"] ] [text "Examples"]
    , section [ style [ "height" => "300px", "padding" => ("0 " ++ sidePad) ] ] <|
        List.map example
        [ ("Home/Mario", "/edit/examples/Intermediate/Mario.elm", "evancz", "")
        , ("Home/Elmtris", "http://people.cs.umass.edu/~jcollard/elmtris/", "jcollard", "https://github.com/jcollard/elmtris")
        , ("Home/Vessel", "https://slawrence.github.io/vessel", "slawrence", "https://github.com/slawrence/vessel")
        , ("Home/FirstPerson", "https://evancz.github.io/first-person-elm", "evancz", "https://github.com/evancz/first-person-elm")
        ]
    , section [ style [ "height" => "300px", "padding" => ("0 " ++ sidePad) ] ] <|
        List.map example
        [ ("Home/Todo", "https://evancz.github.io/elm-todomvc", "evancz", "https://github.com/evancz/elm-todomvc")
        , ("Home/DreamWriter", "http://dreamwriter.io", "rtfeldman", "https://github.com/rtfeldman/dreamwriter")
        , ("Home/Catalog", "http://package.elm-lang.org/packages/elm-lang/core/latest", "evancz", "https://github.com/elm-lang/package.elm-lang.org")
        , ("Home/Fractal", "http://gideon.smdng.nl/2014/04/fractals-for-fun-and-profit/", "stygianguest", "https://github.com/stygianguest/Sierpinski")
        ]
    ]


example (imgSrc, demo, author, code) =
  section [ style [ "display" => "block", "float" => "left", "width" => "200px", "padding" => "0 20px" ] ]
    [ a [href demo] [img [style ["padding-bottom" => "0.5em"], src ("/screenshot/" ++ imgSrc ++ ".png")] []]
    , p [style [ "display" => "block", "float" => "left", "margin" => "0" ]]
        [text "by ", a [href ("http://github.com/" ++ author)] [text author]]
    , a [href code, style ["text-transform" => "lowercase", "display" => "block", "float" => "right"]] [text "source"]
    ]