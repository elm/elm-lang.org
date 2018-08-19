
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import String

import Center
import Skeleton



main =
  Skeleton.skeleton
    "Elm - A delightful language for reliable webapps"
    "home"
    [ splash
    , featureSection
    , exampleSection
    , userSection
    ]



-- SPLASH


splash : Html msg
splash =
  div [ class "splash" ]
    [ div (size 120 16) [ text "elm" ]
    , div (size 26 8) [ text "A delightful language for reliable webapps." ]
    , div (size 16 8) [ text "Generate JavaScript with great performance and no runtime exceptions." ]
    , br [] []
    , getStarted
    ]


size : Int -> Int -> List (Attribute msg)
size height padding =
  [ style "font-size" (String.fromInt height ++ "px")
  , style "padding" (String.fromInt padding ++ "px 0")
  ]



-- GET STARTED


getStarted : Html msg
getStarted =
  div [ class "buttons" ]
    [ a [ href "https://ellie-app.com/new" ] [ text "Try Online" ]
    , a [ href "https://guide.elm-lang.org/install.html" ] [ text "Install" ]
    ]



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
  , description : List (Html msg)
  , dramatization : List (Html msg)
  }


viewFeature : Feature msg -> Html msg
viewFeature feature =
  li
    [ class "feature"
    , style "min-height" (String.fromInt feature.height ++ "px")
    ]
    [ div [class "feature-description"]
        [ h2 [] [text feature.title]
        , p [] feature.description
        ]
    , div [ class "feature-image" ] feature.dramatization
    ]


features : List (Feature msg)
features =
  [ Feature "No Runtime Exceptions" 240
      [ text "Elm uses type inference to detect corner cases and give friendly hints. For example, what if someone provides invalid inputs?! NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production. ("
      , a [href "/blog/compilers-as-assistants"] [text "details"]
      , text ")"
      ]
      [ div [ class "terminal" ]
          [ color cyan "-- TYPE MISMATCH ---------------------------- Main.elm"
          , text "\n\nThe 1st argument to `drop` is not what I expect:\n\n8|   List.drop (String.toInt userInput) [1,2,3,4,5,6]\n                "
          , color dullRed "^^^^^^^^^^^^^^^^^^^^^^"
          , text "\nThis `toInt` call produces:\n\n    "
          , color dullYellow "Maybe"
          , text " Int\n\nBut `drop` needs the 1st argument to be:\n\n    Int\n\n"
          , span [ style "text-decoration" "underline" ] [ text "Hint" ]
          , text ": Use "
          , color green "Maybe.withDefault"
          , text " to handle possible errors."
          ]
      ]
  , Feature "Great Performance" 320
      [ text "Elm has its own virtual DOM implementation, designed for simplicity and speed. All values are immutable in Elm, and the benchmarks show that this helps us generate particularly fast JavaScript code. ("
      , a [href "/blog/blazing-fast-html-round-two"] [text "details"]
      , text ")"
      ]
      [ img
            [ src "/assets/home/benchmark.png"
            , style "width" "100%"
            ]
            []
      ]
  , Feature "Enforced Semantic Versioning" 200
      [ text "Elm can detect all API changes automatically thanks to its type system. We use that information to guarantee that every single Elm package follows semantic versioning precisely. No more surprises in PATCH releases! ("
      , a [href "https://package.elm-lang.org"] [text "details"]
      , text ")"
      ]
      [ div [ class "terminal" ]
          [ color "plum" "$"
          , text " elm diff Microsoft/elm-json-tree-view 1.0.0 2.0.0\nThis is a "
          , color green "MAJOR"
          , text " change.\n\n"
          , color cyan "---- JsonTree - MAJOR ----"
          , text "\n\n    Changed:\n      - parseString : String -> Result String Node\n      + parseString : String -> Result Error Node\n\n      - parseValue : Value -> Result String Node\n      + parseValue : Value -> Result Error Node\n\n"
          ]
      ]
  , Feature "JavaScript Interop" 100
      [ text "Elm can take over a single node, so you can try it out on a small part of an existing project. No major risk in trying it for something small! ("
      , a [href "http://guide.elm-lang.org/interop/"] [text "details"]
      , text ")"
      ]
      [ div [ class "terminal" ]
          [ var
          , text " Elm "
          , equals
          , text " require("
          , string "'./dist/elm/main.js'"
          , text ");\n\n"
          , var
          , text " app "
          , equals
          , text " Elm.Main.init({\n  node: document.getElementById("
          , string "'elm-app'"
          , text ")\n});\n\n"
          , color grey "// set up ports here"
          ]
      ]
  ]


var : Html msg
var =
  color cyan "var"


equals : Html msg
equals =
  color dullRed "="


string : String -> Html msg
string str =
  color dullYellow str


color : String -> String -> Html msg
color clr str =
  span [ style "color" clr ] [ text str ]


cyan : String
cyan =
  "rgb(51,187,200)"


dullRed : String
dullRed =
  "rgb(194,54,33)"


dullYellow : String
dullYellow =
  "rgb(173,173,39)"


green : String
green =
  "rgb(49,231,34)"


grey : String
grey =
  "rgb(143,144,145)"



-- EXAMPLES


exampleSection : Html msg
exampleSection =
  section [class "home-section"]
    [ h1 [] [text "Examples"]
    , p [ class "home-paragraph"
        , style "margin-bottom" "40px"
        ]
        [ text "We have some "
        , a [href "/examples"] [text "simple"]
        , text " and some "
        , a [href "http://builtwithelm.co/"] [text "elaborate"]
        , text " examples to help you as you move through "
        , a [href "http://guide.elm-lang.org/"] [text "the official guide"]
        , text ". Here are some particularly neat ones!"
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
      "elm-spa-example"
      "http://rtfeldman.github.io/elm-spa-example/"
      "https://github.com/rtfeldman/elm-spa-example"
  , example
      "package"
      "http://package.elm-lang.org"
      "https://github.com/elm-lang/package.elm-lang.org"
  , example
      "flatris"
      "https://unsoundscapes.itch.io/flatris"
      "https://github.com/w0rm/elm-flatris"
  ]


example : String -> String -> String -> List (Html msg)
example imgSrc demo code =
  [ a [ href demo, style "display" "block" ]
      [ img
          [ src ("/assets/examples/" ++ imgSrc ++ ".png")
          , alt imgSrc
          , style "width" "100%"
          ]
          []
      ]
  , p [ style "display" "block"
      , style "text-align" "center"
      , style "margin" "0"
      , style "height" "60px"
      ]
      [ a [href code] [text "source"]
      ]
  ]



-- FLUID LIST


fluidList : Int -> Int -> List (List (Html msg)) -> Html msg
fluidList itemWidth maxColumns itemList =
  let
    toPx : Int -> String
    toPx num =
      String.fromInt num ++ "px"

    bulletStyles =
        [ style "display" "inline-block"
        , style "max-width" (toPx itemWidth)
        , style "vertical-align" "top"
        , style "text-align" "left"
        , style "margin" ("0 " ++ toPx gutter)
        ]

    gutter = 30
  in
    section
      [ style "max-width" (toPx (itemWidth*maxColumns + 2*gutter*maxColumns))
      , style "margin" "auto"
      , style "text-align" "center"
      ]
      (List.map (section bulletStyles) itemList)



-- USERS


userSection : Html msg
userSection =
  section [class "home-section"]
    [ h1 [] [text "Featured Users"]
    , div [ class "featured-user" ]
        [ div [ class "quote" ]
            [ p [] [ text "We’ve had zero run-time failures, the filesize is ridiculously small, and it runs faster than anything else in our code base. We’ve also had fewer bugs... " ]
            , p [] [ text "To sum it up, our manager has mandated that all new code be written in Elm." ]
            ]
        , div [ class "attribution" ]
            [ div [ class "attribution-author" ]
                [ p [] [ text "Jeff Schomay" ]
                , p [] [ a [ href "https://www.pivotaltracker.com/blog/Elm-pivotal-tracker/" ] [ text "Pivotal Tracker Blog" ] ]
                ]
            , a [ class "attribution-logo"
                , href "https://www.pivotaltracker.com"
                ]
                [ div
                    [ style "width" "200px"
                    , style "height" "100px"
                    , style "background-image" ("url('" ++ toLogoSrc "PivotalTracker" "svg" ++ "')")
                    , style "background-repeat" "no-repeat"
                    , style "background-position" "center"
                    , style "display" "block"
                    ]
                    []
                ]
            ]
        ]
    , fluidList 200 3
        [ company
            "NoRedInk"
            "http://tech.noredink.com/post/129641182738/building-a-live-validated-signup-form-in-elm"
            "png"
        , company
            "Futurice"
            "http://futurice.com/blog/elm-in-the-real-world"
            "svg"
        , company
            "Gizra"
            "http://www.gizra.com/content/thinking-choosing-elm/"
            "png"
        ]
    ]


company : String -> String -> String -> List (Html msg)
company name website extension =
  [ toLogo name website extension ]


toLogo : String -> String -> String -> Html msg
toLogo name website extension =
  let
    imgSrc =
      toLogoSrc name extension
  in
    a [ href website ]
      [ div
          [ style "width" "200px"
          , style "height" "100px"
          , style "background-image" ("url('" ++ imgSrc ++ "')")
          , style "background-repeat" "no-repeat"
          , style "background-position" "center"
          ]
          []
      ]


toLogoSrc : String -> String -> String
toLogoSrc name extension =
  let
    lowerName =
      String.toLower name
  in
    "/assets/logos/"
    ++ String.map (\c -> if c == ' ' then '-' else c) lowerName
    ++ "." ++ extension
