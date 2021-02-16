module Chart exposing (view, Value(..), Overlay(..))

import Svg exposing (..)
import Svg.Attributes as Attributes exposing (..)
import Svg.Coordinates exposing (..)
import Svg.Plot exposing (..)


{-| -}
type alias Config =
  { marginTop : Float
  , marginLeft : Float
  , yTickValues : List Float
  , values : List Value
  , overlays : List Overlay
  }


{-| -}
type Value
  = Value String Float


{-| -}
type Overlay
  = Overlay Float Float String String


{-| -}
view : Config -> Svg msg
view config =
  let plane : Plane
      plane =
        { x = Axis config.marginLeft 0 400 0.5 (List.length config.values |> add 0.5)
        , y = Axis config.marginTop 23 300 0 (maximum identity config.yTickValues)
        }

      isElm : Int -> Bool
      isElm index =
        index + 1 == List.length config.values

      toBar : Int -> Value -> Bar msg
      toBar index (Value label y) =
        Bar (if isElm index then blue else lightblue) y

      position : Point -> Float -> Float -> String -> String -> Svg msg
      position point xOff yOff styles label =
        g
          [ placeWithOffset plane point xOff yOff
          , Attributes.style (styles ++ "font-size: 12px;")
          ]
          [ text_ [] [ tspan [] [ text label ] ] ]

      viewLabelX : Int -> Value -> Svg msg
      viewLabelX index (Value label y) =
        position { x = toFloat (index + 1), y = 0 } 0 20 "text-anchor: middle;" label

      viewLabelY : Float -> Svg msg
      viewLabelY y =
        position { x = 0.5, y = y } -10 5 "text-anchor: end;" (String.fromFloat y)

      viewBarValues : Int -> Value -> Svg msg
      viewBarValues index (Value label y) =
        position { x = toFloat (index + 1), y = y } 0 -7 "text-anchor: middle;" (String.fromFloat y)

      xTickValues : List Float
      xTickValues =
        List.range 1 (List.length config.values) |> List.map toFloat

      viewOverlay : Overlay -> Svg msg
      viewOverlay (Overlay x y styles label) =
        Svg.g
          [ translate x y, Attributes.style styles ]
          [ Svg.text_ [] [ Svg.tspan [] [ Svg.text label ] ] ]
  in
  svg
    [ Attributes.style "box-sizing: border-box; width: 100%; max-width: 500px; margin: 0 auto;"
    , viewBox ("0 0 " ++ String.fromFloat plane.x.length ++ " " ++ String.fromFloat plane.y.length)
    ]
    [ grouped plane
        { groups = List.map List.singleton (List.indexedMap toBar config.values)
        , width = 0.5
        }
    , fullHorizontal plane [] 0
    , xTicks plane 5 [] 0 xTickValues
    , g [] (List.indexedMap viewLabelX config.values)
    , fullVertical plane [] 0.5
    , yTicks plane 5 [] 0.5 config.yTickValues
    , g [] (List.map viewLabelY config.yTickValues)
    , g [] (List.indexedMap viewBarValues config.values)
    , g [] (List.map viewOverlay config.overlays)
    ]


-- HELPERS


blue : List (Attribute msg)
blue =
  [ stroke "#1293D8", fill "#1293D8" ]


lightblue : List (Attribute msg)
lightblue =
  [ stroke "#95c9ec", fill "#95c9ec" ]


translate : Float -> Float -> Attribute msg
translate x y =
  transform ("translate( " ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")


add : Float -> Int -> Float
add subv v =
  toFloat v + 0.5


