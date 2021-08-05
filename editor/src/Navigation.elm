module Navigation exposing
  ( Navigation, navigation
  , toggleOpen, lights, Status(..), compilation, share, deploy
  , Button, menuButton
  )

import FeatherIcons as I
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on)


{-| -}
type alias Navigation msg =
  { isLight : Bool
  , isOpen : Bool
  , left : List (Html msg)
  , right : List (Html msg)
  }


{-| -}
navigation : Navigation msg -> Html msg
navigation config =
  nav
    [ id "navigation"
    , classList
        [ ( "theme-light", config.isLight )
        , ( "theme-dark", not config.isLight )
        , ( "open", config.isOpen )
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
toggleOpen : msg -> Bool -> Html msg
toggleOpen onClick_ isMenuOpen =
  menuButton
    { icon = if isMenuOpen then I.chevronDown else I.chevronUp
    , iconColor = Nothing
    , label = Nothing
    , alt = if isMenuOpen then "Close menu" else "Open menu"
    , onClick = onClick_
    }


{-| -}
lights : msg -> Bool -> Html msg
lights onClick_ isLight =
  menuButton
    { icon = if isLight then I.moon else I.sun
    , iconColor = Nothing
    , label = Just (if isLight then "Lights off" else "Lights on")
    , alt = "Switch the color scheme"
    , onClick = onClick_
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
  menuButton
    { icon = icon
    , iconColor = iconColor
    , label = Just label
    , alt = "Compile your code (Ctrl-Enter)"
    , onClick = onClick_
    }


{-| -}
share : msg -> Html msg
share onClick_ =
  menuButton
    { icon = I.link
    , iconColor = Nothing
    , label = Just "Share"
    , alt = "Copy link to this code"
    , onClick = onClick_
    }


{-| -}
deploy : msg -> Html msg
deploy onClick_ =
  menuButton
    { icon = I.send
    , iconColor = Nothing
    , label = Just "Deploy"
    , alt = "Deploy this code"
    , onClick = onClick_
    }



-- BUTTON / GENERAL


type alias Button msg =
  { icon : I.Icon
  , iconColor : Maybe String
  , label : Maybe String
  , alt : String
  , onClick : msg
  }


menuButton : Button msg -> Html msg
menuButton config =
  let viewIcon =
        config.icon
          |> I.withSize 14
          |> I.withClass ("icon " ++ Maybe.withDefault "" config.iconColor)
          |> I.toHtml []
  in
  button
    [ attribute "aria-label" config.alt
    , onClick config.onClick
    ]
    [ viewIcon
    , case config.label of
        Just label -> span [] [ text label ]
        Nothing -> text ""
    ]


