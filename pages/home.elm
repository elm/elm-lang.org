
import Browser
import Browser.Events as E
import Html exposing (Html, Attribute, div, span, text)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick, on)
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
import Element.Events as Ev

import Svg
import Svg.Attributes
import Svg.Coordinates
import Svg.Plot

import Ui exposing (Link)
import Center
import Cycle
import Logo
import Skeleton
import TextAnimation
import Chart
import Colors as C
import Highlight exposing (..)



-- MAIN


main =
  Browser.document
    { init = \flags -> ( init flags, Cmd.none )
    , update = \msg model -> ( update msg model, Cmd.none )
    , subscriptions = subscriptions
    , view = \model ->
        { title = "Elm - delightful language for reliable web applications"
        , body = view model
        }
    }



-- MODEL


type alias Model =
  { window : { width : Int, height : Int }
  , patterns : Cycle.Cycle Logo.Pattern
  , time : Float
  , logo : Logo.Model
  , taglines : TextAnimation.State
  , visibility : E.Visibility
  }


init : { width : Int, height : Int } -> Model
init window =
  { window = window
  , time = 0
  , logo = Logo.start
  , visibility = E.Visible
  , taglines =
      TextAnimation.init
        "for reliable web applications."
        [ "with no runtime exceptions."
        , "for data visualization."
        , "with friendly error messages."
        , "for 3D graphics."
        , "with great performance."
        ]
  , patterns = Cycle.init Logo.heart [ Logo.logo ]
  }



-- UPDATE


type Msg
  = OnResize Int Int
  | MouseMoved Float Float Float Float Float
  | MouseClicked
  | TimeDelta Float
  | VisibilityChanged E.Visibility
  | TimePassed
  | HoveringTry
  | HoveringGuide
  | HoveringInstaller
  | UnhoveringButton


update : Msg -> Model -> Model
update msg model =
  case msg of
    OnResize width height ->
      { model | window = { width = width, height = height } }

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

    HoveringTry ->
      { model | logo = Logo.setPattern Logo.child model.logo }

    HoveringGuide ->
      { model | logo = Logo.setPattern Logo.house model.logo }

    HoveringInstaller ->
      { model | logo = Logo.setPattern Logo.heart model.logo }

    UnhoveringButton ->
      { model | logo = Logo.setPattern Logo.logo model.logo }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ E.onResize OnResize
    , E.onVisibilityChange VisibilityChanged
    , case model.visibility of
        E.Hidden ->
          Sub.none

        E.Visible ->
          if Logo.isMoving model.logo || TextAnimation.isMoving model.taglines
          then E.onAnimationFrameDelta TimeDelta
          else Time.every 4000 (\_ -> TimePassed)
    ]



-- VIEW


view : Model -> List (Html Msg)
view model =
  if model.window.width > 950 then
    viewLarge model
  else if model.window.width > 750 then
    viewMedium model
  else
    viewSmall model



-- VIEW LARGE


viewLarge : Model -> List (Html Msg)
viewLarge model =
  let viewFeature feature =
        E.row
          [ E.width E.fill
          , E.width E.fill
          , E.spacing 100
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container True <|
      E.column
        [ E.width E.fill
        , E.paddingXY 20 0
        ]
        [ E.row
            [ E.htmlAttribute (style "min-height" "calc(100vh - 60px)")
            , E.width E.fill
            ]
            [ E.el
                [ E.width E.fill
                , E.moveLeft 50
                ]
                (tangram model)
            , E.column
                [ E.width E.fill
                , E.centerX
                , E.centerY
                , E.spacing 20
                ]
                [ movingText model
                , E.row
                    [ E.width E.fill
                    , E.spacing 20
                    , E.paddingXY 0 5
                    ]
                    [ tryButton
                    , tutorialButton
                    ]
                , downloadLink
                ]
            ]
        , E.column
            [ E.width E.fill
            , E.spacing 140
            , E.paddingEach { top = 40, bottom = 200, left = 0, right = 0 }
            ] <|
            List.map viewFeature features
        ]
  , fixedPointer
  , fixedMenu
  ]


fixedPointer : Html msg
fixedPointer =
  Html.div
    [ class "fixed-pointer" ]
    [ text "↓" ]


fixedMenu : Html Msg
fixedMenu =
  Html.div
    [ class "fixed-menu" ]
    [ container False <|
        E.column
          [ E.width pageColumn
          , E.centerX
          ]
          [ E.row
              [ E.width E.fill
              , E.centerX
              , E.spacing 40
              , E.paddingEach { top = 10, bottom = 10, left = 0, right = 0 }
              ]
              (elmTitle 30 :: List.map navColumn grouped)
          , E.row
              [ E.centerX
              , E.spacing 20
              , E.paddingEach { top = 20, bottom = 20, left = 0, right = 0 }
              , F.size 14
              , F.color C.gray
              ] <|
              (List.map Ui.grayLink sources) ++ [ copyRight ]
          ]
    ]


navColumn : { title : String, links : List Link } -> E.Element msg
navColumn {title, links} =
  E.column
    [ E.width E.fill
    , E.alignTop
    ]
    (navTitle title :: List.map navitem links)


navTitle : String -> E.Element msg
navTitle title =
  E.el
    [ E.padding 5
    , E.width E.fill
    , E.paddingXY 0 10
    , F.size 16
    , F.color C.gray
    , F.bold
    ]
    (E.text title)


navitem : Link -> E.Element msg
navitem link =
  E.link
    [ E.padding 5
    , E.width E.fill
    , E.paddingXY 0 5
    , F.size 13
    , F.color C.black
    , F.regular
    ]
    { url = link.url
    , label = E.text link.title
    }



-- VIEW MEDIUM


viewMedium : Model -> List (Html Msg)
viewMedium model =
  let viewFeature feature =
        E.column
          [ E.width E.fill
          , E.width E.fill
          , E.spacing 40
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container True <|
      E.column
        [ E.width (E.maximum 600 E.fill)
        , E.centerX
        , E.paddingXY 20 0
        , E.spacing 50
        ]
        [ E.row
            [ E.width E.fill
            , E.spaceEvenly
            , E.centerX
            , F.size 14
            , E.paddingXY 0 20
            ]
            [ elmTitle 30
            , E.row
                [ E.width E.fill
                , E.alignRight
                , E.spacing 20
                ]
                (List.map (Ui.link [ E.alignRight ]) toplevel)
            ]
        , E.column
            [ E.width E.fill
            , E.centerX
            , E.centerY
            , E.spacing 20
            ]
            [ movingText model
            , tryButton
            , tutorialButton
            ]
        , E.column
            [ E.width E.fill
            , E.spacing 50
            , E.paddingEach { top = 40, bottom = 20, left = 0, right = 0 }
            ] <|
            List.map viewFeature features
        , E.row
              [ E.centerX
              , E.spacing 20
              , E.paddingEach { top = 20, bottom = 20, left = 0, right = 0 }
              , F.size 14
              , F.color C.gray
              ] <|
              (List.map Ui.grayLink sources) ++ [copyRight]
        ]
  ]



-- VIEW SMALL


viewSmall : Model -> List (Html Msg)
viewSmall model =
  let viewFeature feature =
        E.column
          [ E.width E.fill
          , E.width E.fill
          , E.spacing 40
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container True <|
      E.column
        [ E.width (E.maximum 500 E.fill)
        , E.centerX
        , E.paddingXY 20 0
        , E.spacing 50
        ]
        [ E.row
            [ E.width E.fill
            , E.spaceEvenly
            , E.centerX
            , F.size 14
            , E.paddingXY 0 20
            ]
            (elmTitle 20 :: List.map (Ui.link [ E.paddingXY 5 0, F.size 13 ]) toplevel)
        , E.column
            [ E.width E.fill
            , E.centerX
            , E.centerY
            , E.spacing 20
            ]
            [ movingText model
            , tryButton
            , tutorialButton
            ]
        , E.column
            [ E.width E.fill
            , E.spacing 50
            , E.paddingEach { top = 40, bottom = 20, left = 0, right = 0 }
            ] <|
            List.map viewFeature features
        , E.column
              [ E.centerX
              , E.spacing 20
              , E.paddingEach { top = 20, bottom = 20, left = 0, right = 0 }
              , F.size 14
              , F.color C.gray
              , F.center
              ]
              [ E.row
                  [ E.width E.fill
                  , E.centerX
                  , F.center
                  , E.spacing 20
                  ]
                  (List.map Ui.grayLink sources)
              , copyRight
              ]
        ]
  ]



-- VIEW HELPERS


container : Bool -> E.Element Msg -> Html Msg
container isPrimary content =
  let attributes =
        [ E.width E.fill
        , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
        ]

      wrap =
        if isPrimary then E.layout attributes
        else E.layoutWith { options = [ E.noStaticStyleSheet ] } attributes
  in
  wrap <|
    E.el
      [ E.width pageColumn
      , E.centerX
      ]
      content


pageColumn : E.Length
pageColumn =
  E.fill |> E.maximum 920


elmTitle : Int -> E.Element msg
elmTitle size =
  E.el
    [ F.size size
    , E.alignTop
    , E.width E.fill
    ]
    (E.text "elm")


featureText : Feature Msg -> E.Element Msg
featureText feature =
  E.textColumn
    [ E.width E.fill
    , E.alignLeft
    , E.alignTop
    ]
    [ E.paragraph
        [ F.size 25
        , E.paddingXY 0 15
        , E.width E.fill
        ]
        [ E.text feature.title ]
    , E.paragraph
        [ F.size 16
        , E.width E.fill
        ]
        feature.description
    ]


featureImage : Feature Msg -> E.Element Msg
featureImage feature =
  E.el
    [ E.width E.fill
    , E.alignRight
    ] <| E.el [ E.width E.fill ] <| E.html <| feature.image



-- CONTENT / INTRODUCTION


movingText : Model -> E.Element Msg
movingText model =
  E.textColumn
    [ E.alignTop
    , E.width E.fill
    ]
    [ E.paragraph
        [ F.size 30
        , F.center
        ]
        [ E.text "A delightful language "
        , E.html (Html.br [] [])
        , case TextAnimation.view model.taglines of
           "" ->  E.html <| Html.br [] []
           s -> E.text s
        ]
    ]


tryButton : E.Element Msg
tryButton =
  Ui.linkButton "/try" "Try"
    [ Ev.onMouseEnter HoveringTry
    , Ev.onMouseLeave UnhoveringButton
    ]


tutorialButton : E.Element Msg
tutorialButton =
  Ui.linkButton "https://guide.elm-lang.org" "Tutorial"
    [ Ev.onMouseEnter HoveringGuide
    , Ev.onMouseLeave UnhoveringButton
    ]


downloadLink : E.Element Msg
downloadLink =
  E.paragraph
    [ E.width E.fill
    , F.size 18
    , F.center
    ]
    [ E.text "or "
    , Ui.link
        [ Ev.onMouseEnter HoveringInstaller
        , Ev.onMouseLeave UnhoveringButton
        ]
        (Link "download the installer." "https://guide.elm-lang.org/install/elm.html")
    ]



-- CONTENT / TANGRAM


tangram : Model -> E.Element Msg
tangram model =
  E.html <|
    Logo.view
      [ style "max-height" "500px"
      , style "max-width" "500px"
      , onMouseMove
      , onClick MouseClicked
      ]
      model.logo


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



-- CONTENT / NAVIGATION


toplevel : List Link
toplevel =
  [ Link "Examples" "https://elm-lang.org/examples"
  , Link "Docs" "https://elm-lang.org/docs"
  , Link "Community" "https://elm-lang.org/community"
  , Link "News" "https://elm-lang.org/news"
  ]


grouped : List { title : String, links : List Link }
grouped =
  [ { title = "Quick links"
    , links =
        [ Link "Install" "https://guide.elm-lang.org/install/elm.html"
        , Link "Packages" "https://package.elm-lang.org/"
        , Link "Guide" "https://guide.elm-lang.org/"
        , Link "News" "https://elm-lang.org/news"
        ]
    }
  , { title = "Beginner"
    , links =
        [ Link "Tutorial" "https://guide.elm-lang.org/"
        , Link "Examples" "https://elm-lang.org/examples"
        , Link "Try online" "https://elm-lang.org/try"
        , Link "Talks" "https://elm-lang.org/news#talks"
        , Link "Syntax" "https://elm-lang.org/docs/syntax"
        , Link "Syntax vs JS" "https://elm-lang.org/docs/from-javascript"
        , Link "FAQ" "http://faq.elm-community.org/"
        , Link "Advanced Topics" "https://elm-lang.org/docs/advanced-topics"
        -- , Link "Limitations" TODO
        ]
    }
  , { title = "Community"
    , links =
        [ Link "News" "https://elm-lang.org/news"
        , Link "Slack" "https://elmlang.herokuapp.com/"
        , Link "Discourse" "https://discourse.elm-lang.org/"
        , Link "Twitter" "https://twitter.com/elmlang"
        , Link "Meetup" "https://www.meetup.com/topics/elm-programming/all/"
        , Link "Code of Conduct" "https://elm-lang.org/community#code-of-conduct"
        , Link "Sharing code" "https://elm-lang.org/community#sharing-code"
        ]
    }
  , { title = "Contributing"
    , links =
        [ Link "How to" "https://elm-lang.org/community#sharing-code"
        , Link "Package Design" "https://package.elm-lang.org/help/design-guidelines"
        , Link "Style Guide" "https://elm-lang.org/docs/style-guide"
        , Link "Writing Documentation" "https://package.elm-lang.org/help/documentation-format"
        ]
    }
  ]


sources : List Link
sources =
  [ Link "Compiler Source" "https://github.com/elm/compiler"
  , Link "Site Source" "https://github.com/elm/elm-lang.org"
  ]


copyRight : E.Element Msg
copyRight =
  E.text "© 2012-2021 Evan Czaplicki"



 -- CONTENT / FEATURES


type alias Feature msg =
  { title : String
  , description : List (E.Element msg)
  , image : Html msg
  }


features : List (Feature msg)
features =
  let readMore url =
        Ui.link [] (Link "Read more" url)
  in
  [ { title = "No Runtime Exceptions"
    , description =
      [ E.text "Elm uses type inference to detect corner cases and give friendly hints. NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production. "
      , readMore "/news/compilers-as-assistants"
      ]
    , image =
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
    }
  , { title = "Great Performance"
    , description =
        [ E.text "Elm has its own virtual DOM implementation, designed for simplicity and speed. All values are immutable in Elm, and the benchmarks show that this helps us generate particularly fast JavaScript code. "
        , readMore "/news/blazing-fast-html-round-two"
        ]
    , image = performanceChart
    }
  , { title = "Enforced Semantic Versioning"
    , description =
        [ E.text "Elm can detect all API changes automatically thanks to its type system. We use that information to guarantee that every single Elm package follows semantic versioning precisely. No surprises in PATCH releases. "
        , readMore "https://package.elm-lang.org"
        ]
    , image =
        div [ class "terminal" ]
          [ color "plum" "$"
          , text " elm diff Microsoft/elm-json-tree-view 1.0.0 2.0.0\nThis is a "
          , color green "MAJOR"
          , text " change.\n\n"
          , color cyan "---- JsonTree - MAJOR ----"
          , text "\n\n    Changed:\n      - parseString : String -> Result String Node\n      + parseString : String -> Result Error Node\n\n      - parseValue : Value -> Result String Node\n      + parseValue : Value -> Result Error Node\n\n"
          ]
    }
  , { title = "Small Assets"
    , description =
        [ E.text "Smaller assets means faster downloads and faster page loads, so Elm does a bunch of optimizations to make small assets the default. Just compile with the "
        , E.html (Html.code [ style "display" "inline-block" ] [ Html.text "--optimize" ])
        , E.text " flag and let the compiler do the rest. No complicated set up. "
        , readMore "/news/small-assets-without-the-headache"
        ]
    , image = assetsChart
    }
  , { title = "JavaScript Interop"
    , description =
        [ E.text "Elm can take over a single node, so you can try it out on a small part of an existing project. Try it for something small. See if you like it. "
        , readMore "http://guide.elm-lang.org/interop/"
        ]
    , image =
        div [ class "terminal" ]
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
    }
  ]


performanceChart : Html msg
performanceChart =
  Chart.view
    { marginTop = 40
    , marginLeft = 40
    , yTickValues = [ 1000, 2000, 3000, 4000, 5000 ]
    , values = [ Chart.Value "Ember" 4326, Chart.Value "React" 4612, Chart.Value "Angular 1" 3838, Chart.Value "Angular 2" 3494, Chart.Value "Elm" 2480 ]
    , overlays =
        [ Chart.Overlay 30 25 "text-anchor: end; font-size: 12;" "ms"
        , Chart.Overlay 203 20 "text-anchor: middle; font-size: 16;" "Benchmark Times on Chrome 52"
        , Chart.Overlay 280 40 "text-anchor: middle; font-size: 12; fill: grey;" "Lower is better."
        ]
    }


assetsChart : Html msg
assetsChart =
  Chart.view
    { marginTop = 40
    , marginLeft = 30
    , yTickValues = [ 25, 50, 75, 100 ]
    , values = [ Chart.Value "Vue" 100, Chart.Value "Angular 6" 93, Chart.Value "React 16.4" 77, Chart.Value "Elm 19.0" 29 ]
    , overlays =
        [ Chart.Overlay 20 25 "text-anchor: end; font-size: 12;" "kb"
        , Chart.Overlay 200 20 "text-anchor: middle; font-size: 16;" "RealWorld Asset Size"
        , Chart.Overlay 320 18 "text-anchor: middle; font-size: 12; fill: grey;" "(uglify + gzip)"
        ]
    }
