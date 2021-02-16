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


linkButton : String -> String -> List (Attribute msg) -> E.Element msg
linkButton url label events =
  let styles =
        [ E.padding 10
        , E.width (E.fillPortion 2)
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