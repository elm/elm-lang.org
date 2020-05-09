
import Browser
import Browser.Events as E
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as D
import Markdown
import String
import Time
import Element as E
import Element.Font as F
import Element.Lazy as L
import Element.Region as R
import Element.Input as I
import Element.Background as B
import Element.Border as Bo

import Center
import Grid
import Cycle
import Logo
import Skeleton
import Tabs
import TextAnimation



-- MAIN


main =
  Browser.document
    { init = \() -> ( init, Cmd.none )
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = subscriptions
    , view = \model ->
        { title = "Elm -  delightful language for reliable webapps"
        , body =
            [ -- Grid.view ,
              E.layout
                [ E.width E.fill
                , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
                ] <|
                E.column
                  [ E.width (E.fill |> E.maximum 1000)
                  , E.paddingXY 40 20
                  , E.centerX
                  ]
                  [ E.row
                      [ E.width E.fill, E.spacing 40 ]
                      [ navitem "documentation"
                      , navitem "packages"
                      , navitem "community"
                      , navitem "download"
                      ]
                  , E.row
                      [ E.width E.fill
                      , E.height E.fill
                      , E.paddingXY 0 20
                      ]
                      [ E.el [ F.size 134, E.paddingXY 0 20, E.moveUp 2, E.moveLeft 5, E.alignTop, E.width (E.fillPortion 1) ] (E.text "elm")
                      , viewSplash model
                      ]
                  , E.row
                      [ E.width E.fill ]
                      [ E.html <| Tabs.view [ "Features", "Projects", "Examples" ] ]
                  , E.row
                      [ E.width E.fill
                      , E.spacing 10
                      , E.paddingXY 20 0
                      , Bo.color (E.rgb255 18 147 216)
                      , Bo.solid
                      , Bo.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
                      ]
                      [ navitem2 True "FEATURES"
                      , navitem2 False "PROJECTS"
                      , navitem2 False "EXAMPLES"
                      ]
                  , E.row
                      [ E.width E.fill
                      , E.spacing 40
                      , E.paddingXY 0 20
                      ]
                      [ runtime
                      , runtime
                      ]
                  ]
            ]
        }
    }


navitem : String -> E.Element msg
navitem name =
  E.el
    [ F.size 18
    , F.bold
    , Bo.color (E.rgb 0 0 0)
    , Bo.solid
    , Bo.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
    , E.paddingEach { bottom = 5, left = 0, right = 0, top = 0 }
    ]
    (E.text name)


navitem2 : Bool -> String -> E.Element msg
navitem2 chosen name =
  E.el
    [ F.size 14
    , F.bold
    , F.letterSpacing 2
    , if chosen then E.moveDown 2 else E.moveDown 0
    , if chosen then F.color (E.rgb255 18 147 216) else F.color (E.rgb255 0 0 0)
    , B.color (E.rgb255 255 255 255)
    , Bo.color (E.rgb255 18 147 216)
    , Bo.solid
    , Bo.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
    , Bo.widthEach { bottom = 0, left = 2, right = 2, top = 2 }
    , E.paddingXY 20 10
    ]
    (E.text name)


runtime : E.Element msg
runtime =
  E.column
    [ E.width E.fill
    , E.width E.fill
    , E.spacing 40
    ]
    [ E.textColumn
        [ E.width (E.fillPortion 1)
        , E.alignLeft
        , E.alignTop
        ]
        [ E.el
            [ F.size 25
            , E.paddingXY 0 15
            ]
            (E.text "No Runtime Exceptions")
        , E.paragraph
            [ F.size 15 ]
            [ E.text "Elm uses type inference to detect corner cases and give friendly hints. NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production. (details)" ]
        ]
    , E.el
        [ E.width (E.fillPortion 1)
        , E.alignRight
        ] <|
          E.html <|
            div [ class "terminal" ]
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


-- "overview" "featured" "documentation" "community" "news" "limitations"

-- MODEL


type alias Model =
  { time : Float
  , logo : Logo.Model
  , patterns : Cycle.Cycle Logo.Pattern
  , taglines : TextAnimation.State
  , visibility : E.Visibility
  }


init : Model
init =
  { time = 0
  , logo = Logo.start
  , visibility = E.Visible
  , taglines =
      TextAnimation.init
        "for reliable webapps."
        [ "with no runtime exceptions."
        , "for data visualization."
        , "with friendly error messages."
        , "for 3D graphics."
        ]
  , patterns =
      Cycle.init
        Logo.heart
        [ Logo.logo ]
  }



-- UPDATE


type Msg
  = MouseMoved Float Float Float Float Float
  | MouseClicked
  | TimeDelta Float
  | VisibilityChanged E.Visibility
  | TimePassed


update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseMoved t x y dx dy ->
      { model
          | time = t
          , logo = Logo.perturb (t - model.time) x y dx dy model.logo
      }

    MouseClicked ->
      { model
          | patterns = Cycle.step model.patterns
          , logo = Logo.setPattern (Cycle.next model.patterns) model.logo
      }

    TimeDelta timeDelta ->
      { model
          | logo =
              if Logo.isMoving model.logo
              then Logo.step timeDelta model.logo
              else model.logo
          , taglines =
              if TextAnimation.isMoving model.taglines
              then TextAnimation.step model.taglines
              else model.taglines
      }

    VisibilityChanged visibility ->
      { model | visibility = visibility }

    TimePassed ->
      { model | taglines = TextAnimation.step model.taglines }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onVisibilityChange VisibilityChanged
    , case model.visibility of
        E.Hidden ->
          Sub.none

        E.Visible ->
          if Logo.isMoving model.logo || TextAnimation.isMoving model.taglines
          then E.onAnimationFrameDelta TimeDelta
          else Time.every 4000 (\_ -> TimePassed)
    ]



-- VIEW SPLASH


viewSplash : Model -> E.Element Msg
viewSplash model =
  E.column
    [ E.width (E.fillPortion 2)
    , E.paddingXY 0 30
    , E.alignTop
    , E.spacing 20
    ]
    [ E.row
       [ E.width E.fill ]
       [ E.textColumn
            [ E.alignTop
            , E.width (E.fillPortion 3)
            ]
            [ E.paragraph
               [ F.size 30
               , Bo.color (E.rgb255 211 211 211)
               , Bo.dotted
               , Bo.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
               ]
               [ E.text "A delightful language "
               , E.html (Html.br [] [])
               , case TextAnimation.view model.taglines of
                   "" ->  E.html <| Html.br [] []
                   s -> E.text s
               ]
            ]
        , E.el [ E.width (E.fillPortion 1) ] E.none
        ]
    , E.row
        [ E.width E.fill, E.spacing 15, E.paddingXY 0 5 ]
        [ coolButton "/try"  "Try"
        , coolButton "https://guide.elm-lang.org" "Tutorial"
        ]
    ]


coolButton : String -> String -> E.Element msg
coolButton link label =
  E.link
    [ E.padding 10
    , E.width (E.fillPortion 2)
    , F.center
    , B.color (E.rgb255 255 255 255)
    , Bo.color (E.rgb255 18 147 216)
    , Bo.width 2
    , Bo.solid
    , Bo.shadow
        { offset = ( 5, 5 )
        , size = 1
        , blur = 0
        , color = E.rgb255 18 147 216
        }
    ]
    { url = link
    , label = E.text label
    }


onMouseMove : Attribute Msg
onMouseMove =
  on "mousemove" <|
    D.map7 (\t x y dx dy w h -> MouseMoved t (x / w - 0.5) (0.5 - y / h) (dx / w) (-dy / h))
      (D.field "timeStamp" D.float)
      (D.field "offsetX" D.float)
      (D.field "offsetY" D.float)
      (D.field "movementX" D.float)
      (D.field "movementY" D.float)
      (D.field "currentTarget" (D.field "clientWidth" D.float))
      (D.field "currentTarget" (D.field "clientHeight" D.float))



-- FEATURES


viewFeatures : Html msg
viewFeatures =
  section [class "home-section"]
    [ h1 [] [ text "Features" ]
    , ul [ class "features" ] (List.map viewFeature features)
    ]


type alias Feature msg =
  { title : String
  , height : Int
  , description : List (Html msg)
  , image : List (Html msg)
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
    , div [ class "feature-image" ] feature.image
    ]


features : List (Feature msg)
features =
  [ Feature "No Runtime Exceptions" 240
      [ text "Elm uses type inference to detect corner cases and give friendly hints. NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production. ("
      , a [href "/news/compilers-as-assistants"] [text "details"]
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
      , a [href "/news/blazing-fast-html-round-two"] [text "details"]
      , text ")"
      ]
      [ img
            [ src "/assets/home/benchmark.png"
            , style "width" "100%"
            ]
            []
      ]
  , Feature "Enforced Semantic Versioning" 200
      [ text "Elm can detect all API changes automatically thanks to its type system. We use that information to guarantee that every single Elm package follows semantic versioning precisely. No surprises in PATCH releases. ("
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
  , Feature "Small Assets" 280
      [ text "Smaller assets means faster downloads and faster page loads, so Elm does a bunch of optimizations to make small assets the default. Just compile with the "
      , code [] [ text "--optimize" ]
      , text " flag and let the compiler do the rest. No complicated set up. ("
      , a [href "/news/small-assets-without-the-headache"] [text "details"]
      , text ")"
      ]
      [ img
            [ src "/assets/home/asset-sizes.png"
            , style "width" "100%"
            ]
            []
      ]
  , Feature "JavaScript Interop" 120
      [ text "Elm can take over a single node, so you can try it out on a small part of an existing project. Try it for something small. See if you like it. ("
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

