
import Browser
import Browser.Events as E
import Html exposing (Html, Attribute, div, span, text)
import Html.Attributes exposing (style, class, attribute, href)
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

import Svg as S
import Svg.Attributes as SA

import Ui exposing (Link)
import Center
import Cycle
import Logo
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
  , quotes : Cycle.Cycle SmallQuote
  , justSelected : Bool
  , taglines : TextAnimation.State
  , visibility : E.Visibility
  }


type SmallQuote
  = QuoteByAuthor String String (Maybe String)
  | QuoteByEmployee String String (Maybe String) String String (Maybe String)



init : { width : Int, height : Int } -> Model
init window =
  { window = window
  , time = 0
  , logo = Logo.start
  , visibility = E.Visible
  , quotes =
      Cycle.init
        (QuoteByEmployee "It is the most productive programming language I have used." "Rupert Smith" (Just "https://github.com/the-sett") "Software Engineer" "The Sett Ltd" Nothing)
        [ QuoteByEmployee "Using Elm, I can deploy and go to sleep!" "Mario Uher" (Just "https://github.com/ream88") "CTO" "yodel.io" (Just "https://www.yodel.io/")
        , QuoteByAuthor "You just follow the compiler errors and come out the other end with a working app." "James Carlson" (Just "https://jxxcarlson.io/")
        , QuoteByEmployee "[To me it's] the feeling of joy and relaxation when writing Elm code." "Luca Mugnaini" (Just "https://github.com/lucamug") "Software Engineer" "Rakuten" (Just "https://www.rakuten.com/")
        , QuoteByAuthor "You can learn it in a day and keep it in your head even if you don’t use it for weeks." "Jeremy Brown" (Just "https://github.com/jhbrown94")
        , QuoteByAuthor "The language helps steer you towards writing api’s that are simple and clear." "Eric Henry" Nothing
        , QuoteByAuthor "My favorite thing about Elm is that I don’t have to worry when coding" "David Andrews" (Just "https://github.com/DavidDTA")
        , QuoteByAuthor "I love how fast Elm is. I make a change and I get an immediate response." "Wolfgang Schuster" (Just "https://github.com/wolfadex")
        , QuoteByEmployee "Everything in core fits together like Lego." "Atle Wee Førre" (Just "https://github.com/atlewee") "Software Engineer" "Equinor" (Just "https://www.equinor.com")
        , QuoteByEmployee "Thanks to Elm, now I just go and build things in peace. It’s wonderful." "Agus Zubiaga" (Just "https://github.com/agu-z") "Head of Engineering" "PINATA" (Just "https://www.gopinata.com/")
        ]
  , justSelected = False
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
  | JumpToQuote Int


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
      { model
      | taglines = TextAnimation.step model.taglines
      , quotes =
          if model.justSelected
          then model.quotes
          else Cycle.step model.quotes
      , justSelected = False
      }

    HoveringTry ->
      { model | logo = Logo.setPattern Logo.child model.logo }

    HoveringGuide ->
      { model | logo = Logo.setPattern Logo.house model.logo }

    HoveringInstaller ->
      { model | logo = Logo.setPattern Logo.heart model.logo }

    UnhoveringButton ->
      { model | logo = Logo.setPattern Logo.logo model.logo }

    JumpToQuote index ->
      { model | quotes = Cycle.select index model.quotes, justSelected = True }



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
          else Time.every 3000 (\_ -> TimePassed)
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
        E.el [ E.width E.fill ] <| E.row
          [ E.width pageColumn
          , E.centerX
          , E.spacing 100
          , Ui.role "article"
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container <|
      E.column
        [ E.width E.fill
        ]
        [ E.el
            [ E.width E.fill
            ] <|
            E.row
              [ E.htmlAttribute (style "min-height" "calc(100vh - 135px)")
              , E.width pageColumn
              , E.centerX
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
        , smallQuotes 0 100 model
        , E.column
            [ E.width E.fill
            , E.paddingEach { top = 60, bottom = 200, left = 0, right = 0 }
            , E.spacing 120
            , R.mainContent
            ] <|
            List.map viewFeature features
        ]
  --, fixedPointer
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
    [ subContainer <|
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
              , R.footer
              ] <|
              (List.map Ui.grayLink sources) ++ [ copyRight ]
          ]
    ]


navColumn : { title : String, links : List Link } -> E.Element msg
navColumn {title, links} =
  E.column
    [ E.width E.fill
    , E.alignTop
    , R.navigation
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
          , E.spacing 40
          , Ui.role "article"
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container <|
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
                , R.navigation
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
        , smallQuotes 40 20 model
        , E.column
            [ E.width E.fill
            , E.spacing 50
            , E.paddingEach { top = 0, bottom = 20, left = 0, right = 0 }
            , R.mainContent
            ] <|
            List.map viewFeature features
        , E.row
              [ E.centerX
              , E.spacing 20
              , E.paddingEach { top = 20, bottom = 20, left = 0, right = 0 }
              , F.size 14
              , F.color C.gray
              , R.footer
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
          , Ui.role "article"
          ]
          [ featureText feature
          , featureImage feature
          ]
  in
  [ container <|
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
            , R.navigation
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
        , smallQuotes 40 20 model
        , E.column
            [ E.width E.fill
            , E.spacing 50
            , E.paddingEach { top = 0, bottom = 20, left = 0, right = 0 }
            , R.mainContent
            ] <|
            List.map viewFeature features
        , E.column
              [ E.centerX
              , E.spacing 20
              , E.paddingEach { top = 20, bottom = 20, left = 0, right = 0 }
              , F.size 14
              , F.color C.gray
              , F.center
              , R.footer
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


container : E.Element Msg -> Html Msg
container =
  E.layout
    [ E.width E.fill
    , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
    ]


subContainer : E.Element Msg -> Html Msg
subContainer =
  E.layoutWith { options = [ E.noStaticStyleSheet ] }
    [ E.width E.fill
    , F.family [ F.typeface "IBM Plex Sans", F.sansSerif ]
    ]


pageColumn : E.Length
pageColumn =
  E.maximum 920 E.fill


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
    , E.spacing 15
    ]
    [ E.paragraph
        [ F.size 25
        , E.width E.fill
        ]
        [ Ui.h2 feature.title
        ]
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
    ] <| E.el [ E.width E.fill ] <| feature.image


featureQuote : Quote -> E.Element msg
featureQuote quote =
  E.textColumn
    [ E.width E.fill
    , E.alignTop
    , Ui.role "figure"
    ]
    [ E.paragraph
        [ F.size 22
        , F.italic
        , E.htmlAttribute (Html.Attributes.style "background-image" "url(\"/assets/home/quotes.svg\")")
        , E.htmlAttribute (Html.Attributes.style "background-repeat" "no-repeat")
        , E.htmlAttribute (Html.Attributes.style "background-size" "150px")
        , E.paddingEach { top = 30, bottom = 20, left = 0, right = 0 }
        ]
        [ Ui.blockquote quote.quote
        ]
    , E.paragraph
      [ F.size 16
      , E.alignRight
      ]
      [ Ui.figcaption quote.author
      ]
    ]


smallQuotes : Int -> Int -> Model -> E.Element Msg
smallQuotes top bottom model =
  let viewOne quote =
        E.paragraph
          [ F.size 18
          , F.color C.gray
          , F.italic
          , F.center
          , E.width (E.maximum 700 E.fill)
          ] <|
          case quote of
            QuoteByAuthor text author gitlink ->
              [ Ui.quote text
              , E.html (Html.br [] [])
              , E.el [ F.size 16 ] (Ui.cite gitlink author)
              , E.el [ F.size 16 ] (E.text ", Software engineer")
              ]

            QuoteByEmployee text author gitlink title company link ->
              [ Ui.quote text
              , E.html (Html.br [] [])
              , E.el [ F.size 16 ] (Ui.cite gitlink author)
              , E.el [ F.size 16 ] (E.text ", ")
              , E.el [ F.size 16 ] (E.text title)
              , E.el [ F.size 16 ] (E.text ", ")
              , case link of
                  Just url -> E.link [ F.size 16 ] { url = url, label = E.text company }
                  Nothing -> E.el [ F.size 16 ] (E.text company)

              ]


      viewDot index quote =
        I.button
          [ E.width (E.px 7)
          , E.height (E.px 7)
          , Bo.rounded 100
          , if Cycle.next model.quotes == quote then
              B.color C.blue
            else
              B.color (E.rgb255 230 230 230)
          , R.description "Next quote"
          ]
          { onPress = Just (JumpToQuote index)
          , label = E.none
          }
  in
  E.column
    [ E.width E.fill
    , E.spacing 15
    , E.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }
    ]
    [ E.el [ E.centerX ] (viewOne (Cycle.next model.quotes))
    , E.row
        [ E.spacing 10, E.centerX ]
        (List.indexedMap viewDot (Cycle.toList model.quotes))
    ]




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
        [ Ui.h1
            [ "A delightful language "
            , "for reliable web applications."
            ]
        ]
    ]


tryButton : E.Element Msg
tryButton =
  Ui.linkButton "/try" "Playground"
    [ Ev.onMouseEnter HoveringTry
    , Ev.onMouseLeave UnhoveringButton
    , R.description "Go to playground"
    ]


tutorialButton : E.Element Msg
tutorialButton =
  Ui.linkButton "https://guide.elm-lang.org" "Tutorial"
    [ Ev.onMouseEnter HoveringGuide
    , Ev.onMouseLeave UnhoveringButton
    , R.description "Go to Tutorial"
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
  [ Link "Examples" "/examples"
  , Link "Docs" "/docs"
  , Link "Community" "/community"
  , Link "News" "/news"
  ]


grouped : List { title : String, links : List Link }
grouped =
  [ { title = "Quick links"
    , links =
        [ Link "Install" "https://guide.elm-lang.org/install/elm.html"
        , Link "Packages" "https://package.elm-lang.org/"
        , Link "Guide" "https://guide.elm-lang.org/"
        , Link "News" "/news"
        ]
    }
  , { title = "Beginner"
    , links =
        [ Link "Tutorial" "https://guide.elm-lang.org/"
        , Link "Examples" "/examples"
        , Link "Try online" "/try"
        , Link "Syntax" "/docs/syntax"
        , Link "Syntax vs JS" "/docs/from-javascript"
        , Link "FAQ" "http://faq.elm-community.org/"
        , Link "Advanced Topics" "/docs/advanced-topics"
        -- , Link "Limitations" TODO
        ]
    }
  , { title = "Community"
    , links =
        [ Link "News" "/news"
        , Link "Slack" "https://elmlang.herokuapp.com/"
        , Link "Discourse" "https://discourse.elm-lang.org/"
        , Link "Twitter" "https://twitter.com/elmlang"
        , Link "Meetup" "https://www.meetup.com/topics/elm-programming/all/"
        , Link "Code of Conduct" "/community#code-of-conduct"
        , Link "Sharing code" "/community#share-code"
        ]
    }
  , { title = "Contributing"
    , links =
        [ Link "How to" "/community#share-code"
        , Link "Package Design" "https://package.elm-lang.org/help/design-guidelines"
        , Link "Style Guide" "/docs/style-guide"
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
  , image : E.Element msg
  }


type alias Quote =
  { quote : String
  , author : String
  , link : Maybe String
  }


features : List (Feature msg)
features =
  let paragraphLinked url content =
        [ E.html <| Html.p [] (content ++ [ text " ", Html.a [ href url ] [ Html.text "Learn more." ] ]) ]

      paragraph content =
        [ E.html <| Html.p [] content ]
  in
  [ { title = "No Runtime Exceptions"
    , description =
        paragraphLinked "/news/compilers-as-assistants"
          [ text "Elm uses type inference to detect corner cases and give friendly hints. NoRedInk switched to Elm about two years ago, and 250k+ lines later, they still have not had to scramble to fix a confusing runtime exception in production." ]
    , image = E.html <|
        div [ class "terminal", attribute "role" "figure" ]
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
  , { title = "Fearless refactoring"
    , description =
        paragraph
          [ text "The compiler guides you safely through your changes, ensuring confidence even through the most widereaching refactorings in unfamiliar codebases." ]
    , image =
        featureQuote
          { quote = "Whether it's renaming a function or a type, or making a drastic change in a core data type, you just follow the compiler errors and come out the other end with a working app."
          , author = "James Carlson, Elm developer"
          , link = Nothing
          }
    }
  , { title = "One way to do anything"
    , description =
        paragraphLinked "https://guide.elm-lang.org/architecture/"
          [ text "All Elm programs are written in the same pattern, eliminating doubt when building new projects and making it easy to navigate old or foreign codebases. This has been proven especially valuable for companies with many engineers and large codebases!" ]
    , image = E.html <|
        div [ class "terminal", attribute "role" "figure" ]
          [ color grey "-- THE ELM ARCHITECTURE"
          , text "\n"
          , text "\n"
          , color cyan "init"
          , text " : ( Model, Cmd Msg )\n"
          , text "\n"
          , color cyan "update"
          , text " : Msg -> Model -> ( Model, Cmd Msg )\n"
          , text "\n"
          , color cyan "subscriptions"
          , text " : Model -> Sub Msg\n"
          , text "\n"
          , color cyan "view"
          , text " : Model -> Html Msg\n"
          ]
    }
  , { title = "Fast and useful feedback"
    , description =
        paragraph
          [ text "Even on large codebases, compilation is blazing fast." ]
    , image =
        featureQuote
          { quote = "I love how fast Elm is. I make a change and I get an immediate response. It’s like I’m having a conversation with the compiler about how best to build things."
          , author = "Wolfgang Schuster, Elm developer"
          , link = Nothing
          }
    }
  , { title = "Great Performance"
    , description =
        paragraphLinked "/news/blazing-fast-html-round-two"
          [ text "Elm has its own virtual DOM implementation, designed for simplicity and speed. All values are immutable in Elm, and the benchmarks show that this helps us generate particularly fast JavaScript code." ]
    , image = E.html performanceChart
    }
  , { title = "Enforced Semantic Versioning"
    , description =
        paragraphLinked "https://package.elm-lang.org"
          [ text "Elm can detect all API changes automatically thanks to its type system. We use that information to guarantee that every single Elm package follows semantic versioning precisely. No surprises in PATCH releases." ]
    , image = E.html <|
        div [ class "terminal", attribute "role" "figure" ]
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
        paragraphLinked "/news/small-assets-without-the-headache"
          [ text "Smaller assets means faster downloads and faster page loads, so Elm does a bunch of optimizations to make small assets the default. Just compile with the "
          , Html.code [ style "display" "inline-block" ] [ Html.text "--optimize" ]
          , text " flag and let the compiler do the rest. No complicated set up."
          ]
    , image = E.html assetsChart
    }
  , { title = "JavaScript Interop"
    , description =
        paragraphLinked "http://guide.elm-lang.org/interop/"
        [ text "Elm can take over a single node, so you can try it out on a small part of an existing project. Try it for something small. See if you like it. "
        ]
    , image = E.html <|
        div [ class "terminal", attribute "role" "figure" ]
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
        [ Chart.Overlay 30 25 "text-anchor: end; font-size: 12px;" "ms"
        , Chart.Overlay 203 20 "text-anchor: middle; font-size: 16px;" "Benchmark Times on Chrome 52"
        , Chart.Overlay 280 40 "text-anchor: middle; font-size: 12px; fill: grey;" "Lower is better."
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
        [ Chart.Overlay 20 25 "text-anchor: end; font-size: 12px;" "kb"
        , Chart.Overlay 200 20 "text-anchor: middle; font-size: 16px;" "RealWorld Asset Size"
        , Chart.Overlay 320 18 "text-anchor: middle; font-size: 12px; fill: grey;" "(uglify + gzip)"
        ]
    }

