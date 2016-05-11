
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import String

import Center
import Skeleton



main =
  Skeleton.skeleton "home"
    [ splash
    , htmlSection
    , bulletSection
    , exampleSection
    , userSection
    , br [] []
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



-- CODE SNIPPET


htmlSection =
  section []
    [ h2 [ style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"] ] [ text "Hello HTML" ]
    , p [ style [ "text-align" => "center" ] ]
        [ a [href "/examples/hello-world", style ["display" => "inline-block"]]
          [ code
            [ class "lang-elm hljs"
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
            , a [href "http://package.elm-lang.org/packages/elm-lang/html/1.0.0/Html-App"] [text "Html.App"]
            , text ". Not only does it render "
            , a [href "/blog/blazing-fast-html"] [text "extremely fast"]
            , text ", it also quietly guides you towards "
            , a [href "http://guide.elm-lang.org/"] [text "well-architected code"]
            , text "."
            ]
        ]
    ]



-- FEATURES


bulletSection : Html msg
bulletSection =
  section []
    [ h1
        [style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"]]
        [text "Features"]
    , fluidList 300 3 bullets
    ]


bullets : List (List (Html msg))
bullets =
  [ [ h2 [] [ text "No runtime exceptions"]
    , p [] [text "Yes, you read that right, no runtime exceptions. Elm’s compiler is amazing at finding errors before they can impact your users. The only way to get Elm code to throw a runtime exception is by explicitly invoking "
      , a [href "http://package.elm-lang.org/packages/elm-lang/core/latest/Debug#crash"] [code [] [text "crash"]]
      , text "."
      ]
    ]
  , [ h2 [] [text "Friendly Error Messages"]
    , p []
        [ text "The Elm compiler is able to find tricky issues in your code very quickly. When it finds an issue, "
        , a [href "/blog/compilers-as-assistants"] [text "it provides friendly and helpful messages"]
        , text ". This makes it easy to find and fix problems before your users see them."
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
        , a [href "http://guide.elm-lang.org/interop/javascript.html"] [text "ports"]
        , text " system, your Elm code can communicate with JavaScript without sacrificing guarantees."
        ]
    ]
  ]



-- EXAMPLES


exampleSection : Html msg
exampleSection =
  section []
    [ h1
        [style ["text-align" => "center", "font-size" => "3em", "padding-top" => "80px"]]
        [text "Examples"]
    , fluidList 400 3 examples
    , p [ style [ "text-align" => "center", "font-size" => "20px" ] ]
        [ text "More large projects at "
        , a [href "http://builtwithelm.co/"] [text "builtwithelm.co"]
        , text " and more small examples in "
        , a [href "/examples"] [text "the examples tab"]
        , text "."
        ]
    ]


examples : List (List (Html msg))
examples =
  [ example
      "todomvc"
      "https://evancz.github.io/elm-todomvc"
      "https://github.com/evancz/elm-todomvc"
  , example
      "hedley"
      "https://gizra.github.io/elm-hedley"
      "https://github.com/Gizra/elm-hedley"
  , example
      "mantl-ui"
      "https://mantl.io/"
      "https://github.com/CiscoCloud/mantl-ui-frontend"
  , example
      "package"
      "http://package.elm-lang.org"
      "https://github.com/elm-lang/package.elm-lang.org"
  , example
      "flatris"
      "http://unsoundscapes.com/elm-flatris.html"
      "https://github.com/w0rm/elm-flatris"
  , example
      "sketch-n-sketch"
      "http://ravichugh.github.io/sketch-n-sketch/releases/latest/"
      "https://github.com/ravichugh/sketch-n-sketch"
  ]


example : String -> String -> String -> List (Html msg)
example imgSrc demo code =
  [ a [ href demo, style ["display" => "block"] ]
      [ img [src ("/assets/examples/" ++ imgSrc ++ ".png")] []
      ]
  , p [style ["display" => "block", "text-align" => "center", "margin" => "0", "height" => "60px"]]
      [ a [href code] [text "source"]
      ]
  ]



-- FLUID LIST


fluidList : Int -> Int -> List (List (Html msg)) -> Html msg
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
      [style ["max-width" => toPx (itemWidth*maxColumns + 2*gutter*maxColumns), "margin" => "auto", "text-align" => "center"]]
      (List.map (section [style bulletStyle]) itemList)



-- USERS


userSection : Html msg
userSection =
  section []
    [ h1
        [style ["text-align" => "center", "font-size" => "2em", "padding-top" => "80px"]]
        [text "Featured Commercial Users"]
    , fluidList 200 3
        [ company
            "NoRedInk"
            "http://tech.noredink.com/post/129641182738/building-a-live-validated-signup-form-in-elm"
            "png"
        , company
            "CircuitHub"
            "https://circuithub.com/"
            "png"
        , company
            "Beautiful Destinations"
            "http://www.beautifuldestinations.com/"
            "svg"
        , company
            "Prezi"
            "https://prezi.com/"
            "png"
        , company
            "Gizra"
            "http://www.gizra.com/content/thinking-choosing-elm/"
            "png"
        , company
            "TruQu"
            "https://truqu.com/"
            "png"
        , company
            "Futurice"
            "http://futurice.com/blog/elm-in-the-real-world"
            "svg"
        ]
    , p [ style [ "text-align" => "center", "color" => "#bbbbbb" ] ]
        [ text "Want to get featured? Let us know how your company uses Elm on "
        , a [class "grey-link", href "https://groups.google.com/forum/#!forum/elm-discuss"] [text "elm-discuss"]
        , text " or "
        , a [class "grey-link", href "https://twitter.com/elmlang"] [text "twitter"]
        , text "!"
        ]
    ]


company name website extension =
  let
    lowerName =
      String.toLower name

    imgSrc =
      "/assets/logos/"
      ++ String.map (\c -> if c == ' ' then '-' else c) lowerName
      ++ "." ++ extension
  in
    [ a [ href website ]
        [ div
            [ style
                [ "width" => "100%"
                , "height" => "100px"
                , "background-image" => ("url('" ++ imgSrc ++ "')")
                , "background-repeat" => "no-repeat"
                , "background-position" => "center"
                ]
            ]
            []
        ]
    ]



