
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

import Svg
import Svg.Attributes
import Svg.Coordinates
import Svg.Plot

import Center
import Grid
import Cycle
import Logo
import Skeleton
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
            [ Grid.view
            , E.layout
                [ E.width E.fill
                , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
                ] <|
                E.column
                  [ E.width E.fill ]
                  [ E.row
                      [ E.width E.fill
                      , E.paddingXY 40 34
                      ]
                      [ E.el
                          [ E.alignRight
                          ] <|
                          E.paragraph
                            [ F.size 16
                            , F.bold
                            , E.moveDown 3
                            ]
                            [ E.text "Looking for the package website? "
                            , E.html <| Html.span [ class "effect" ] [ Html.text " →" ]
                            ]
                      ]
                  , E.column
                      [ E.width (E.fill |> E.maximum 920)
                      , E.paddingXY 0 0
                      , E.centerX
                      ]
                      [ E.row
                          [ E.width E.fill
                          , E.height E.fill
                          , E.paddingEach { bottom = 100, left = 0, right = 0, top = 60 }
                          ]
                          [ E.el
                              [ E.width (E.fillPortion 2)
                              , E.paddingXY 0 0
                              ]
                              (E.html <|
                               div
                                  [ class "tangram"
                                  , onMouseMove
                                  ]
                                  [ Logo.view
                                      [ style "color" "white"
                                      , onClick MouseClicked
                                      ]
                                      model.logo
                                  ])
                          , viewSplash model
                          ]
                      , E.column
                          [ E.width E.fill
                          , E.spacing 80
                          , E.paddingXY 60 40
                          ]
                          (List.indexedMap viewFeature features)

                      ]
                  ]
            , Html.div [ class "fixed-menu" ]
                [ E.layoutWith { options = [ E.noStaticStyleSheet ] }
                    [ E.width E.fill
                    , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
                    ]
                    <| E.row
                        [ E.width (E.fill |> E.maximum 860)
                        , E.spacing 40
                        , E.paddingXY 0 10
                        , E.centerX
                        ]
                        [ E.el
                          [ F.size 30
                          , E.alignTop
                          , E.width E.fill
                          ]
                          (E.text "elm")
                        , navColumn "Quick links" [ "Install", "Packages", "Guide", "News" ]
                        , navColumn "Beginner" [ "Tutorial", "Examples", "Try online", "Talks", "Syntax", "FAQ", "Limitations" ]
                        , navColumn "Community" [ "News", "Slack", "Discourse", "Twitter", "Meetup", "Code of Conduct" ]
                        , navColumn "Contributing" [ "How to", "Package Design", "Style Guide", "Writing Documentation", "Advanced Topics" ]
                        ]
                ]
            ]
        }
    }

navColumn : String -> List String -> E.Element msg
navColumn title items =
  E.column
    [ E.width E.fill
    , E.alignTop
    ]
    (navitem True title :: List.map (navitem False) items)


navitem : Bool -> String -> E.Element msg
navitem isTitle name =
  E.el
    [ E.padding 5
    , E.width E.fill
    , if isTitle then E.paddingXY 0 10 else E.paddingXY 0 5
    , if isTitle then F.size 16 else F.size 13
    , if isTitle then F.color (E.rgb255 128 128 128) else F.color (E.rgb255 0 0 0)
    , if isTitle then F.bold else F.regular
    --, E.paddingEach { bottom = 5, left = 0, right = 0, top = 0 }
    ]
    (E.text name)


navitemTop : Bool -> String -> E.Element msg
navitemTop selected name =
  E.el
    [ F.size 18
    , E.padding 5
    , E.paddingEach { bottom = 5, left = 0, right = 0, top = 0 }
    --, Bo.color (E.rgb 0 0 0)
    --, Bo.solid
    --, Bo.widthEach { bottom = 2, left = 0, right = 0, top = 0 }
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
        "for reliable web applications."
        [ "with no runtime exceptions."
        , "for data visualization."
        , "with friendly error messages."
        , "for 3D graphics."
        , "with great performance."
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
    , E.paddingXY 20 30
    , E.centerX
    , E.centerY
    , E.spacing 20
    ]
    [ E.textColumn
        [ E.alignTop
        , E.width (E.fillPortion 3)
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
    , E.row
        [ E.width E.fill, E.spacing 15, E.paddingXY 0 5 ]
        [ coolButton "/try"  "Try"
        , coolButton "https://guide.elm-lang.org" "Tutorial"
        ]
    , E.paragraph
        [ E.width E.fill
        , F.size 18
        , F.center
        ]
        [ E.text "or "
        , E.link
            [ F.color (E.rgb255 18 147 216) ]
            { url = "/"
            , label = E.text "download the installer."
            }
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


type alias Feature msg =
  { title : String
  , height : Int
  , description : List (E.Element msg)
  , image : List (Html msg)
  }


viewFeature : Int -> Feature msg -> E.Element msg
viewFeature index feature =
  E.row
    [ E.width E.fill
    , E.width E.fill
    , E.spacing 60
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
            (E.html <| Html.span [ class "highlight"] [Html.text feature.title])
        , E.paragraph
            [ F.size 15 ]
            feature.description
        ]
    , E.el
        [ E.width (E.fillPortion 1)
        , E.alignRight
        ] <| E.html <| div [] feature.image
    ]


features : List (Feature msg)
features =
  [ Feature "No Runtime Exceptions" 240
      [ E.text "Elm uses type inference to detect corner cases and give friendly hints. NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production. ("
      , E.link [] { url = "/news/compilers-as-assistants", label = E.text "details" }
      , E.text ")"
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
      [ E.text "Elm has its own virtual DOM implementation, designed for simplicity and speed. All values are immutable in Elm, and the benchmarks show that this helps us generate particularly fast JavaScript code. ("
      , E.link [] { url = "/news/blazing-fast-html-round-two", label = E.text "details" }
      , E.text ")"
      ]
      [ performanceChart
      ]
  , Feature "Enforced Semantic Versioning" 200
      [ E.text "Elm can detect all API changes automatically thanks to its type system. We use that information to guarantee that every single Elm package follows semantic versioning precisely. No surprises in PATCH releases. ("
      , E.link [] { url = "https://package.elm-lang.org", label = E.text "details" }
      , E.text ")"
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
      [ E.text "Smaller assets means faster downloads and faster page loads, so Elm does a bunch of optimizations to make small assets the default. Just compile with the "
      , E.html (Html.code [] [ Html.text "--optimize" ])
      , E.text " flag and let the compiler do the rest. No complicated set up. ("
      , E.link [] { url = "/news/small-assets-without-the-headache", label = E.text "details" }
      , E.text ")"
      ]
      [ assetsChart
      ]
  , Feature "JavaScript Interop" 120
      [ E.text "Elm can take over a single node, so you can try it out on a small part of an existing project. Try it for something small. See if you like it. ("
      , E.link [] { url = "http://guide.elm-lang.org/interop/", label = E.text "details" }
      , E.text ")"
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


performanceChart : Html msg
performanceChart =
  let plane =
        { x = Svg.Coordinates.Axis 40 0 400 0.5 5.5
        , y = Svg.Coordinates.Axis 40 23 300 0 (Svg.Coordinates.maximum identity yLabelValues)
        }

      group : Int -> List Float -> List (Svg.Plot.Bar msg)
      group index =
          List.map <| Svg.Plot.Bar <|
            if index == 4 then
              [ Svg.Attributes.stroke "#1293D8", Svg.Attributes.fill "#1293D8" ]
            else
              [ Svg.Attributes.stroke "rgba(18, 147, 216, 0.5)", Svg.Attributes.fill "rgba(18, 147, 216, 0.5)" ]

      place : Svg.Coordinates.Point -> Float -> Float -> String -> String -> Svg.Svg msg
      place point xOff yOff style label =
        Svg.g
          [ Svg.Coordinates.placeWithOffset plane point xOff yOff
          , Svg.Attributes.style (style ++ "font-size: 12px;")
          ]
          [ Svg.text_ [] [ Svg.tspan [] [ Svg.text label ] ] ]

      xLabels =
        Svg.g [] <|
          List.indexedMap
            (\i l -> place { x = toFloat (i + 1), y = 0 } 0 20 "text-anchor: middle;" l)
            [ "Ember", "React", "Angular 1", "Angular 2", "Elm" ]

      yLabels =
        Svg.g [] <|
          List.map
          (\y -> place { x = 0, y = y } -10 5 "text-anchor: end;" (String.fromFloat y))
          yLabelValues

      markers : Svg.Svg msg
      markers =
        Svg.g [] <|
          List.indexedMap
          (\i y -> place { x = toFloat (i + 1), y = y } 0 -7 "text-anchor: middle;" (String.fromFloat y))
          yValues

      yLabelValues =
        [ 1000, 2000, 3000, 4000, 5000 ]

      yValues =
        [ 4326, 4612, 3838, 3494, 2480 ]

      viewText : Float -> Float -> Int -> String -> String -> Svg.Svg msg
      viewText x y size style label =
        Svg.g
          [ Svg.Attributes.transform <| "translate( " ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
          , Svg.Attributes.style <| "text-anchor: middle; font-size: " ++ String.fromInt size ++ "px; " ++ style
          ]
          [ Svg.text_ [] [ Svg.tspan [] [ Svg.text label ] ] ]
  in
  Svg.svg
    [ Svg.Attributes.width (String.fromFloat plane.x.length)
    , Svg.Attributes.height (String.fromFloat plane.y.length)
    ]
    [ Svg.Plot.grouped plane
        { groups = List.indexedMap group (List.map List.singleton yValues)
        , width = 0.75
        }
    , Svg.Plot.fullHorizontal plane [] 0
    , Svg.Plot.xTicks plane 5 [] 0 [ 1, 2, 3, 4, 5 ]
    , xLabels
    , Svg.Plot.fullVertical plane [] 0.5
    , Svg.Plot.yTicks plane 5 [] 0.5 yLabelValues
    , yLabels
    , viewText 30 25 12 "text-anchor: end;" "ms"
    , markers
    , viewText 200 20 16 "" "Benchmarks Times on Chrome 52"
    , viewText 280 40 12 "fill: grey;" "Lower is better."
    ]


assetsChart : Html msg
assetsChart =
  let plane =
        { x = Svg.Coordinates.Axis 30 0 400 0.5 4.5
        , y = Svg.Coordinates.Axis 50 23 300 0 (Svg.Coordinates.maximum identity yLabelValues)
        }

      group : Int -> List Float -> List (Svg.Plot.Bar msg)
      group index =
          List.map <| Svg.Plot.Bar <|
            if index == 3 then
              [ Svg.Attributes.stroke "#1293D8", Svg.Attributes.fill "#1293D8" ]
            else
              [ Svg.Attributes.stroke "rgba(18, 147, 216, 0.5)", Svg.Attributes.fill "rgba(18, 147, 216, 0.5)" ]

      place : Svg.Coordinates.Point -> Float -> Float -> String -> String -> Svg.Svg msg
      place point xOff yOff style label =
        Svg.g
          [ Svg.Coordinates.placeWithOffset plane point xOff yOff
          , Svg.Attributes.style (style ++ "font-size: 12px;")
          ]
          [ Svg.text_ [] [ Svg.tspan [] [ Svg.text label ] ] ]

      xLabels =
        Svg.g [] <|
          List.indexedMap
            (\i l -> place { x = toFloat (i + 1), y = 0 } 0 20 "text-anchor: middle;" l)
            [ "Vue", "Angular 6", "React 16.4", "Elm 19.0" ]

      yLabels =
        Svg.g [] <|
          List.map
          (\y -> place { x = 0, y = y } -10 5 "text-anchor: end;" (String.fromFloat y))
          yLabelValues

      markers : Svg.Svg msg
      markers =
        Svg.g [] <|
          List.indexedMap
          (\i y -> place { x = toFloat (i + 1), y = y } 0 -7 "text-anchor: middle;" (String.fromFloat y))
          yValues

      yLabelValues =
        [ 25, 50, 75, 100 ]

      yValues =
        [ 100, 93, 77, 29 ]

      viewText : Float -> Float -> Int -> String -> String -> Svg.Svg msg
      viewText x y size style label =
        Svg.g
          [ Svg.Attributes.transform <| "translate( " ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
          , Svg.Attributes.style <| "text-anchor: middle; font-size: " ++ String.fromInt size ++ "px; " ++ style
          ]
          [ Svg.text_ [] [ Svg.tspan [] [ Svg.text label ] ] ]
  in
  Svg.svg
    [ Svg.Attributes.width (String.fromFloat plane.x.length)
    , Svg.Attributes.height (String.fromFloat plane.y.length)
    ]
    [ Svg.Plot.grouped plane
        { groups = List.indexedMap group (List.map List.singleton yValues)
        , width = 0.75
        }
    , Svg.Plot.fullHorizontal plane [] 0
    , Svg.Plot.xTicks plane 5 [] 0 [ 1, 2, 3, 4, 5 ]
    , xLabels
    , Svg.Plot.fullVertical plane [] 0.5
    , Svg.Plot.yTicks plane 5 [] 0.5 yLabelValues
    , yLabels
    , viewText 30 30 12 "text-anchor: end;" "kb"
    , markers
    , viewText 200 20 16 "" "RealWorld Asset Size"
    , viewText 320 20 12 "fill: grey;" "(uglify + gzip)"
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

