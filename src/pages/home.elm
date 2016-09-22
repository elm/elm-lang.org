
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import String

import Center
import Skeleton



main =
  Skeleton.skeleton "home"
    [ splash
    , featureSection
    , exampleSection
    , userSection
    , getStartedSection
    ]


(=>) = (,)



-- SPLASH


splash =
  div [ class "splash" ]
    [ div [ size 120 16 ] [ text "elm" ]
    , div [ size 26 8 ] [ text "A delightful language for reliable webapps." ]
    , div [ size 16 8 ] [ text "Generate JavaScript with great performance and no runtime exceptions." ]
    , br [] []
    , getStarted
    ]


size height padding =
  style
    [ "font-size" => (toString height ++ "px")
    , "padding" => (toString padding ++ "px 0")
    ]



-- GET STARTED


getStarted : Html msg
getStarted =
  a [ class "get-started-button"
    , href "http://guide.elm-lang.org/get_started.html"
    ]
    [ text "Get Started"
    ]


getStartedSection : Html msg
getStartedSection =
  div [ class "splash", style [("margin", "100px 0")] ] [ getStarted ]



-- FEATURES


featureSection : Html msg
featureSection =
  section [class "home-section"]
    [ h1 [] [text "Features"]
    , ul [class "features"] (List.map viewFeature features)
    ]


type alias Feature msg =
  { title : String
  , height : Int
  , image : String
  , link : String
  , description : List (Html msg)
  }


viewFeature : Feature msg -> Html msg
viewFeature feature =
  li
    [ class "feature"
    , style [ ( "min-height", toString feature.height ++ "px" ) ]
    ]
    [ div [class "feature-description"]
        [ h2 [] [text feature.title]
        , p [] feature.description
        ]
    , a [ class "feature-image"
        , href feature.link
        ]
        [ img [src feature.image
              , style [("width", "100%")]
              ] []
        ]
    ]


features : List (Feature msg)
features =
  [ Feature "JavaScript Interop" 100 "/assets/home/embed.png" "/blog/how-to-use-elm-at-work" <|
      [ text "Elm compiles to JavaScript, so trying out Elm is easy. Convert a small part of your app to Elm and "
      , a [href "/blog/how-to-use-elm-at-work"] [text "embed it in JS"]
      , text ". No full rewrites, no huge time investment. More about that "
      , a [href "http://guide.elm-lang.org/interop/"] [text "here"]
      , text "."
      ]
  , Feature "No Runtime Exceptions" 200 "/assets/home/errors.png" "/blog/compilers-as-assistants" <|
      [ text "Unlike hand-written JavaScript, Elm code does not produce runtime exceptions in practice. Instead, Elm uses type inference to detect problems during compilation and give "
      , a [href "/blog/compilers-as-assistants"] [text "friendly hints"]
      , text ". This way problems never make it to your users. NoRedInk has 36k lines of Elm, and after more than a year in production, it still has not produced a single runtime exception."
      ]
  , Feature "Great Performance" 320 "/assets/home/benchmark.png" "/blog/blazing-fast-html-round-two" <|
      [ text "Elm has its own virtual DOM implementation, designed for simplicity and speed. All values are immutable in Elm, and "
      , a [href "/blog/blazing-fast-html-round-two"] [text "the benchmarks"]
      , text " show that this helps us generate particularly fast JavaScript code."
      ]
  , Feature "Enforced Semantic Versioning" 280 "/assets/home/semver.png" "http://package.elm-lang.org" <|
      [ text "Elm can detect all API changes automatically thanks to its type system. We use that information to force everything in "
      , a [href "http://package.elm-lang.org"] [text "our package catalog"]
      , text " to follow "
      , a [href "https://github.com/elm-lang/elm-package/#version-rules"] [text "semantic versioning"]
      , text " precisely. No more surprises in PATCH releases!"
      ]
  ]



-- EXAMPLES


exampleSection : Html msg
exampleSection =
  section [class "home-section"]
    [ h1 [] [text "Examples"]
    , p [class "home-paragraph", style [("margin-bottom","40px")] ]
        [ text "Learning by example is important, so we have some "
        , a [href "/examples"] [text "simple"]
        , text " and "
        , a [href "http://builtwithelm.co/"] [text "elaborate"]
        , text " examples to help you as you move through "
        , a [href "http://guide.elm-lang.org/"] [text "An Introduction to Elm"]
        , text ". Here are some nice ones!"
        ]
    , fluidList 400 2 examples
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
      "http://ravichugh.github.io/sketch-n-sketch/releases/v0.5/"
      "https://github.com/ravichugh/sketch-n-sketch"
  ]


example : String -> String -> String -> List (Html msg)
example imgSrc demo code =
  [ a [ href demo, style ["display" => "block"] ]
      [ img
          [ src ("/assets/examples/" ++ imgSrc ++ ".png")
          , alt imgSrc
          , style [("width", "100%")]
          ]
          []
      ]
  , p [style ["display" => "block", "text-align" => "center", "margin" => "0", "height" => "60px" ]]
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
        , "max-width" => toPx itemWidth
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
  section [class "home-section"]
    [ h1 [] [text "Featured Users"]
    , p [class "home-paragraph"]
        [ text "Definitely check out the links for "
        , a [href "http://tech.noredink.com/post/129641182738/building-a-live-validated-signup-form-in-elm"] [text "NoRedInk"]
        , text ", "
        , a [href "http://www.gizra.com/content/thinking-choosing-elm/"] [text "Gizra"]
        , text ", and "
        , a [href "http://futurice.com/blog/elm-in-the-real-world"] [text "Futurice"]
        , text " to learn more about how and why they are using Elm. If you want to join them and use Elm at work, definitely follow "
        , a [href "/blog/how-to-use-elm-at-work"] [text "this advice"]
        , text " and do it gradually. Elm is all about reducing risk, even in adoption!"
        ]
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
            "Carfax"
            "https://www.carfax.com/"
            "png"
        , company
            "Futurice"
            "http://futurice.com/blog/elm-in-the-real-world"
            "svg"
        , company
            "Gizra"
            "http://www.gizra.com/content/thinking-choosing-elm/"
            "png"
        , company
            "TruQu"
            "https://truqu.com/"
            "png"
        , company
            "Prezi"
            "https://prezi.com/"
            "png"
        , company
            "Beautiful Destinations"
            "http://www.beautifuldestinations.com/"
            "svg"
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
                [ "width" => "200px"
                , "height" => "100px"
                , "background-image" => ("url('" ++ imgSrc ++ "')")
                , "background-repeat" => "no-repeat"
                , "background-position" => "center"
                ]
            ]
            []
        ]
    ]
