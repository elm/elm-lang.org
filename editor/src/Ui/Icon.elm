module Ui.Icon exposing
  ( simpleIcon
  , Button, button
  , Link, link
  )


import FeatherIcons as I
import Html as H
import Html.Attributes as HA
import Html.Events as HE



-- SIMPLE ICON


simpleIcon : Maybe String -> I.Icon -> H.Html msg
simpleIcon color icon =
  icon
    |> I.withSize 14
    |> I.withClass ("icon " ++ Maybe.withDefault "" color)
    |> I.toHtml []



-- BUTTON / GENERAL


type alias Button msg =
  { background : Maybe String
  , icon : I.Icon
  , iconColor : Maybe String
  , labelColor : Maybe String
  , label : Maybe String
  , alt : String
  , onClick : Maybe msg
  }


button : List (H.Attribute msg) -> Button msg -> H.Html msg
button attrs config =
  let defaultAttrs =
        [ HA.attribute "aria-label" config.alt
        , HA.classList classes
        , case config.onClick of
            Just msg -> HE.onClick msg
            Nothing -> HA.disabled True
        ]

      classes =
        case config.background of
          Just background ->
            [ ( "icon-button", True )
            , ( "background", True )
            , ( background, True )
            ]

          Nothing ->
            [ ( "icon-button", True ) ]
  in
  H.button
    (attrs ++ defaultAttrs)
    [ simpleIcon config.iconColor config.icon
    , case config.label of
        Just label -> H.span [ HA.class (Maybe.withDefault "" config.labelColor) ] [ H.text label ]
        Nothing -> H.text ""
    ]



-- LINK / GENERAL


type alias Link =
  { icon : I.Icon
  , iconColor : Maybe String
  , label : Maybe String
  , alt : String
  , link : String
  }


link : List (H.Attribute msg) -> Link -> H.Html msg
link attrs config =
  let defaultAttrs =
        [ HA.attribute "aria-label" config.alt
        , HA.class "icon-button"
        , HA.target "_blank"
        , HA.href config.link
        ]
  in
  H.a (attrs ++ defaultAttrs)
    [ simpleIcon config.iconColor config.icon
    , case config.label of
        Just label -> H.span [] [ H.text label ]
        Nothing -> H.text ""
    ]