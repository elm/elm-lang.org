module Highlight exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)


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

