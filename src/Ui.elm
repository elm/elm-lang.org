module Ui exposing (..)

import Element as E exposing (Element, Attribute)
import Element.Font as F
import Element.Lazy as L
import Element.Region as R
import Element.Input as I
import Element.Background as B
import Element.Border as Bo
import Element.Events as Ev
import Colors as C


link : String -> String -> List (Attribute msg) -> Element msg
link url label attrs =
  E.link
    ([ F.color C.blue
     ] ++ attrs)
    { url = url
    , label = E.text label
    }


grayLink : String -> String -> Element msg
grayLink url label =
  E.link []
    { url = url
    , label = E.text label
    }


linkButton : String -> String -> List (Attribute msg) -> E.Element msg
linkButton url label events =
  let styles =
        [ E.padding 10
        , E.width (E.maximum 400 E.fill)
        , E.centerX
        , F.center
        , B.color C.white
        , Bo.color C.blue
        , Bo.width 2
        , Bo.solid
        , Bo.shadow
            { offset = ( 5, 5 )
            , size = 1
            , blur = 0
            , color = C.blue
            }
        , E.mouseOver pressed
        , E.mouseDown pressed
        , E.focused pressed
        ]

      pressed =
        [ E.moveDown 3
        , E.moveRight 3
        , Bo.shadow
            { offset = ( 2, 2 )
            , size = 1
            , blur = 0
            , color = C.blue
            }
        ]
  in
  E.link (styles ++ events)
    { url = url
    , label = E.text label
    }



-- RESPONSIVENESS


isSmall : E.Device -> Bool
isSmall device =
  case device.class of
    E.Phone -> True
    E.Tablet -> True
    E.Desktop -> False
    E.BigDesktop -> False


type alias Stacking msg =
  { attrs : List (Attribute msg)
  , left : List (Attribute msg) -> E.Element msg
  , right : List (Attribute msg) -> E.Element msg
  }


stacking : E.Device -> Stacking msg -> E.Element msg
stacking device =
  let horizontal {attrs, left, right} =
        E.row attrs
          [ left
              [ E.width E.fill
              , E.alignLeft
              , E.alignTop
              ]
          , right
              [ E.width E.fill
              , E.alignRight
              ]
          ]

      vertical {attrs, left, right} =
        E.column (E.paddingXY 30 0 :: attrs)
          [ left
              [ E.width E.fill
              , E.centerX
              ]
          , right
              [ E.width E.fill
              , E.centerX ]
          ]

  in
  case device.class of
    E.Phone -> vertical
    E.Tablet -> horizontal
    E.Desktop -> horizontal
    E.BigDesktop -> horizontal