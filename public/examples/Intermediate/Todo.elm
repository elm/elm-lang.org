module Todo where

import Graphics.Input as Input
import Keyboard
import Window

-- Model
type Task = { done : Bool, description : String, id : Int }

taskInput = Input.fields Input.emptyFieldState

entered = keepIf id True Keyboard.enter

taskFieldState =
    let resetOnEnter = sampleOn entered (constant Input.emptyFieldState)
    in  merge taskInput.events resetOnEnter

taskField = taskInput.field id "What needs to be done?" <~ taskFieldState

-- Update
data Action = Add String | Remove Int

taskDelete = Input.customButtons 0
input = merges [ Remove <~ taskDelete.events,
                 (Add . .string) <~ sampleOn entered taskInput.events ]

step action (tasks,count) =
 case action of
   Add [] -> (tasks, count)
   Add dsc -> (Task False dsc count :: tasks, count + 1)
   Remove n -> (filter (\t -> t.id /= n) tasks, count)

descriptions = lift fst $ foldp step ([],0) input

-- Display
btn str = container 30 30 middle . text . Text.height 1.5 . bold <| toText str
grey = rgb 200 200 200
displayTask task =
    color (rgba 255 255 255 0.9) . container 500 30 midRight <|
    flow right [ container 410 30 midLeft . text <| toText task.description
               , taskDelete.customButton task.id
                   (btn "") (btn "&times;") (btn "&times;") ]

display (w,h) taskField tasks =
  let pos = midTopAt (relative 0.5) (absolute 40)
  in  layers
        [ tiledImage w h "/texture.png"
        , [markdown|<style>input:focus { outline: none; }</style>|]
        , container w h pos <|
          flow down
          [ toText "todos" |> Text.height 3
                           |> Text.color (rgb 179 179 179)
                           |> centered
                           |> width 500
          , spacer 500 15 |> color (rgb 141 125 119)
          , spacer 500 1  |> color (rgb 108 125 119)
          , color (rgb 247 247 247) . container 500 45 midRight <|
                  (taskField |> width 440 |> height 45)
          , flow down <| map displayTask tasks ]
        ]

main = display <~ Window.dimensions ~ taskField ~ descriptions
