import Graphics.Element as E exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Text
import Window

import TopBar
import Center


port title : String
port title =
  "Elm"


main =
  div []
    [ TopBar.topBar "home"
    , splash
    , debuggerSection
    , bulletSection
    , exampleSection
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

debuggerSection =
  section []
    [ h2 [ style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"] ] [ text "Hello HTML" ]
    , p [ style [ "text-align" => "center" ] ]
        [ a [href "/examples/hello-world", style ["display" => "inline-block"]]
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
        ]
    , div [ style [ "display" => "block", "margin" => "2em auto 0", "max-width" => "600px" ] ]
        [ p [ style [ "text-align" => "center" ] ]
            [ text "Writing HTML apps is super easy with "
            , a [href "https://github.com/evancz/start-app/blob/master/README.md"] [text "start-app"]
            , text ". Not only does it render "
            , a [href "/blog/blazing-fast-html"] [text "extremely fast"]
            , text ", it also quietly guides you towards "
            , a [href "https://github.com/evancz/elm-architecture-tutorial/"] [text "well-architected code"]
            , text "."
            ]
        ]
    ]


-- FEATURES

bulletSection : Html
bulletSection =
  section []
    [ h1
        [style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"]]
        [text "Features"]
    , fluidList 300 3 bullets
    ]


bullets : List (List Html)
bullets =
  [ [ h2 [] [ text "No runtime exceptions"]
    , p [] [text "Yes, you read that right, no runtime exceptions. Elm’s compiler is amazing at finding errors before they can impact your users. The only way to get Elm code to throw a runtime exception is by explicitly invoking "
      , a [href "http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash"] [code [] [text "crash"]]
      , text "."
      ]
    ]
  , [ h2 [] [text "Blazing fast rendering"]
    , p []
        [ text "The "
        , a [href "/blog/blazing-fast-html"] [text "elm-html"]
        , text " library outperforms even React.js in "
        , a [href "http://evancz.github.io/todomvc-perf-comparison/"] [text "TodoMVC benchmarks"]
        , text ", and it is super simple to optimize your code by sprinkling in some "
        , a [href "http://package.elm-lang.org/packages/evancz/elm-html/latest/Html-Lazy"] [code [] [text "lazy"]]
        , text " rendering."
        ]
    ]
  , [ h2 [] [text "Libraries with guarantees"]
    , p []
        [ text "Semantic versioning is automatically enforced for all "
        , a [href "http://package.elm-lang.org/"] [text "community libraries"]
        , text ". Elm's package manager "
        , a [href "https://twitter.com/czaplic/status/601826927838650369"] [text "detects any API changes"]
        , text ", so breaking API changes never sneak into patches. You can upgrade with confidence."
        ]
    ]
  , [ h2 [] [text "Clean syntax"]
    , p [] [text "No semicolons. No mandatory parentheses for function calls. Everything is an expression. For even more concise code there’s also destructuring assignment, pattern matching, automatic currying, and more."]
    ]
  , [ h2 [] [text "Smooth JavaScript interop"]
    , p []
        [ text "No need to reinvent the wheel when there’s a JavaScript library that already does what you need. Thanks to Elm’s simple "
        , a [href "/guide/interop"] [text "ports"]
        , text " system, your Elm code can communicate with JavaScript without sacrificing guarantees."
        ]
    ]
  , [ h2 [] [text "Time-traveling debugger"]
    , p []
        [ text "What if you could pause time and replay all recent user inputs? What if you could make a code change and watch the results replay without a page refresh? "
        , a [href "/blog/time-travel-made-easy"] [text "Try it out"]
        , text " and see for yourself!"
        ]
    ]
  ]


-- EXAMPLES

examples : List (List Html)
examples =
  [ example
      "Home/Mario"
      "/examples/short-mario"
      "evancz"
      ""
  , example
      "Home/Elmtris"
      "http://people.cs.umass.edu/~jcollard/elmtris/"
      "jcollard"
      "https://github.com/jcollard/elmtris"
  , example
      "Home/Vessel"
      "https://slawrence.github.io/vessel"
      "slawrence"
      "https://github.com/slawrence/vessel"
  , example
      "Home/FirstPerson"
      "https://evancz.github.io/first-person-elm"
      "evancz"
      "https://github.com/evancz/first-person-elm"
  , example
      "Home/Todo"
      "https://evancz.github.io/elm-todomvc"
      "evancz"
      "https://github.com/evancz/elm-todomvc"
  , example
      "Home/DreamWriter"
      "http://dreamwriter.io"
      "rtfeldman"
      "https://github.com/rtfeldman/dreamwriter"
  , example
      "Home/Catalog"
      "http://package.elm-lang.org/packages/elm-lang/core/latest"
      "evancz"
      "https://github.com/elm-lang/package.elm-lang.org"
  , example
      "Home/Fractal"
      "http://gideon.smdng.nl/2014/04/fractals-for-fun-and-profit/"
      "stygianguest"
      "https://github.com/stygianguest/Sierpinski"
  ]


exampleSection : Html
exampleSection =
  section []
    [ h1
        [style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"]]
        [text "Examples"]
    , fluidList 200 4 examples
    ]


example : String -> String -> String -> String -> List Html
example imgSrc demo author code =
  [ a [ href demo, style ["display" => "block"] ]
      [ img
          [style [], src ("/screenshot/" ++ imgSrc ++ ".png")]
          []
      ]
  , p [style ["display" => "block", "float" => "left", "margin" => "0", "height" => "60px"]]
      [ text "by "
      , a [href ("http://github.com/" ++ author)] [text author]
      ]
  , p [style ["display" => "block", "float" => "right", "margin" => "0", "height" => "60px"]]
      [ a [href code] [text "source"]
      ]
  ]


-- FLUID LIST

fluidList : Int -> Int -> List (List Html) -> Html
fluidList itemWidth maxColumns itemList =
  let
    toPx : Int -> String
    toPx num =
      toString num ++ "px"

    bulletStyle =
        [ "display" => "inline-block"
        , "width" => toPx itemWidth
        , "vertical-align" => "top"
        , "text-align" => "left"
        , "margin" => ("0 " ++ toPx gutter)
        ]

    gutter = 30
  in
    section
      [style ["max-width" => toPx (itemWidth*maxColumns + 2*gutter*maxColumns), "margin" => "auto", "text-align" => "center", "margin-top" => "30px"]]
      (List.map (section [style bulletStyle]) itemList)

