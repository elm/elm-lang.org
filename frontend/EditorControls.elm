module EditorControls where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Window


main =
  Varying.map2 topBar
    Window.width
    (Varying.constant (Just ("map", " : (a -> b) -> List a -> List b")))


-- VIEW

topBar : Int -> Maybe (String, String) -> Html
topBar outerWidth maybeHint =
  let px = toString (outerWidth - 340) ++ "px"
  in
  div [ class "options" ]
    [ div [ class "hint", style [("width", px)] ] (hintBody maybeHint)
    , div
        [ class "button blue"
        , title "Compile and run the fresh code (Ctrl-Enter)"
        , onClick (Stream.message compileClicks.address ())
        ]
        [ text "Compile" ]
    , div
        [ class "button green"
        , title "Keep the state, change the behavior (Ctrl-Shift-Enter)"
        , onClick (Stream.message hotSwapClicks.address ())
        ]
        [ text "Hot Swap" ]
    , div
        [ class "button purple"
        , title "Switch editor color scheme"
        , onClick (Stream.message lightsClicks.address ())
        ]
        [ text "Lights" ]
    ]


hintBody : Maybe (String,String) -> List Html
hintBody maybeHint =
  case maybeHint of
    Nothing -> []
    Just (name, facts) ->
        [ text "Hint: "
        , a [ href "http://package.elm-lang.org/packages/elm-lang/core/latest/List#map" ]
            [ text name ]
        ]


-- INPUT / OUTPUT

input compileClicks : Stream.Input ()

foreign output compile : Stream ()
foreign output compile =
    compileClicks.stream


input hotSwapClicks : Stream.Input ()

foreign output hotSwap : Stream ()
foreign output hotSwap =
    hotSwapClicks.stream


input lightsClicks : Stream.Input ()

foreign output lights : Stream ()
foreign output lights =
    lightsClicks.stream


