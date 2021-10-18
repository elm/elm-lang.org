module Ui.Elements exposing (..)


import Element as E
import Element.Background as BG
import Element.Border as B
import Element.Font as F
import Element.Input as I
import FeatherIcons as I
import Ui.Color exposing (..)


iconTitle : I.Icon -> String -> E.Element msg
iconTitle icon str =
  E.row
    [ F.size 18
    , F.color black
    , E.spacing 12
    ]
    [ simpleIcon 18 icon
    , E.text str
    ]


iconNote : I.Icon -> String -> E.Element msg
iconNote icon str =
  E.row
    [ F.size 14
    , F.bold
    , F.color gray
    , E.spacing 12
    ]
    [ simpleIcon 14 icon
    , E.text str
    ]


iconButton : msg -> E.Color -> I.Icon -> String -> List (E.Attribute msg) -> E.Element msg
iconButton msg color icon label attrs =
  I.button attrs
    { onPress = Just msg
    , label =
        E.row
          [ E.spacing 8, F.color color, F.size 14 ]
          [ simpleIcon 14 icon, E.text label ]
    }


simpleIcon : Int -> I.Icon -> E.Element msg
simpleIcon size icon =
  icon
    |> I.withSize (toFloat size)
    |> I.toHtml []
    |> E.html
    |> E.el []


simpleCircle : Int -> E.Color -> E.Element msg
simpleCircle size color =
  E.el
    [ B.width 0
    , B.rounded 50
    , E.height (E.px size)
    , E.width (E.px size)
    , E.centerY
    , BG.color color
    ]
    E.none


wall : E.Element msg
wall =
  E.el
    [ E.height E.fill
    , B.color border
    , B.widthEach { top = 0, bottom = 0, left = 1, right = 0 }
    ] E.none