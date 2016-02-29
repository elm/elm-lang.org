import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

import Random exposing (generate, initialSeed, int)

main =
  StartApp.start { model = model, view = view, update = update }

{- 
  Every time you make a call to the random generator, you must pass a seed.
  There's two ways of getting the seed - either from a call to the generator, 
  or by calling initialSeed with some value.
  
  Here we store a seed in our model, along with the current number.
-}
model = { number = 0,  seed = Random.initialSeed 5 }


updateNumber n model = { model | number <- n } 
updateSeed s model = { model | seed <- s }

{- 
  The random number generator takes Generator a, 
  where a is the type of the value to be produced.
  
  The generators provided such as int take a min and max value to generate
  between
-}
newNumber model = 
  let
    (number, seed) = generate (int 0 10) model.seed
  in
     updateNumber number <| updateSeed seed model 



view address model =
  div []
    [ button [ onClick address Click ] [ text "New random number!" ]
    , div [] [ text <| "Current number: " ++ (toString model.number) ]
    ]

type Action = Click

update action model =
  case action of
    Click -> newNumber model
