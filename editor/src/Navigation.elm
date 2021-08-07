module Navigation exposing
  ( Navigation, view
  , elmLogo, toggleOpen, lights, Status(..), compilation, share, deploy, toggleSplit
  , IconButton, iconButton
  )

{-| The navigation bar.

-}

import FeatherIcons as I
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)
import Svg exposing (svg, use)
import Svg.Attributes as SA


{-| -}
type alias Navigation msg =
  { isLight : Bool
  , isOpen : Bool
  , left : List (Html msg)
  , right : List (Html msg)
  }


{-| -}
view : Navigation msg -> Html msg
view config =
  nav
    [ id "navigation"
    , classList
        [ ( "open", config.isOpen )
        , ( "closed", not config.isOpen )
        ]
    ]
    [ section
        [ id "topbar" ]
        [ aside [] config.left
        , aside [] config.right
        ]
    ]



-- BUTTON / PREMADE


{-| -}
elmLogo : Html msg
elmLogo =
  a [ href "/", class "icon-link" ]
    [ svg
        [ SA.height "14"
        , SA.width "14"
        ]
        [ use [ SA.xlinkHref "#logo" ] []
        ]
    --, span [] [ text "Elm" ]
    ]


{-| -}
toggleOpen : msg -> Bool -> Html msg
toggleOpen onClick_ isMenuOpen =
  iconButton []
    { icon = if isMenuOpen then I.chevronDown else I.chevronUp
    , iconColor = Nothing
    , label = Nothing
    , alt = if isMenuOpen then "Close menu" else "Open menu"
    , onClick = Just onClick_
    }


{-| -}
toggleSplit : msg -> Html msg
toggleSplit onClick_ =
  iconButton [ style "padding" "0 5px" ]
    { icon = I.code
    , iconColor = Nothing
    , label = Nothing
    , alt = "Open or close result"
    , onClick = Just onClick_
    }

{-| -}
lights : msg -> Bool -> Html msg
lights onClick_ isLight =
  iconButton []
    { icon = if isLight then I.moon else I.sun
    , iconColor = Nothing
    , label = Just "Lights"
    , alt = "Switch the color scheme"
    , onClick = Just onClick_
    }


{-| -}
type Status
  = Changed
  | Compiling
  | Success
  | ProblemsFound
  | CouldNotCompile


{-| -}
compilation : msg -> Status -> Html msg
compilation onClick_ status =
  let ( icon, iconColor, label ) =
        case status of
          Changed ->
            ( I.refreshCcw
            , Just "blue"
            , "Check changes"
            )

          Compiling ->
            ( I.loader
            , Nothing
            , "Compiling..."
            )

          Success ->
            ( I.check
            , Just "green"
            , "Success"
            )

          ProblemsFound ->
            ( I.x
            , Just "red"
            , "Problems found"
            )

          CouldNotCompile ->
            ( I.x
            , Just "red"
            , "Try again later."
            )
  in
  iconButton []
    { icon = icon
    , iconColor = iconColor
    , label = Just label
    , alt = "Compile your code (Ctrl-Enter)"
    , onClick = Just onClick_
    }


{-| -}
share : msg -> Html msg
share onClick_ =
  iconButton []
    { icon = I.link
    , iconColor = Nothing
    , label = Just "Share"
    , alt = "Copy link to this code"
    , onClick = Just onClick_
    }


{-| -}
deploy : msg -> Html msg
deploy onClick_ =
  iconButton []
    { icon = I.send
    , iconColor = Nothing
    , label = Just "Publish"
    , alt = "Publish this code"
    , onClick = Just onClick_
    }



-- BUTTON / GENERAL


type alias IconButton msg =
  { icon : I.Icon
  , iconColor : Maybe String
  , label : Maybe String
  , alt : String
  , onClick : Maybe msg
  }


iconButton : List (Attribute msg) -> IconButton msg -> Html msg
iconButton attrs config =
  let viewIcon =
        config.icon
          |> I.withSize 14
          |> I.withClass ("icon " ++ Maybe.withDefault "" config.iconColor)
          |> I.toHtml []
  in
  button
    (attrs ++
      [ attribute "aria-label" config.alt
      , class "navigation-button"
      , case config.onClick of
          Just msg -> onClick msg
          Nothing -> disabled True
      ])
    [ viewIcon
    , case config.label of
        Just label -> span [] [ text label ]
        Nothing -> text ""
    ]


type alias LabelButton msg =
  { label : String
  , alt : String
  , onClick : msg
  }


labelButton : List (Attribute msg) -> LabelButton msg -> Html msg
labelButton attrs config =
  button
    (attrs ++
      [ attribute "aria-label" config.alt
      , onClick config.onClick
      ])
    [ text config.label ]
