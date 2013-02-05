import Website.Docs (createDocs2)

directions =
  [ ("arrows", "Signal { x:Int, y:Int }", [markdown|
A signal of records indicating which arrow keys are pressed.

`{ x = 0, y = 0 }` when pressing no arrows.<br>
`{ x =-1, y = 0 }` when pressing the left arrow.<br>
`{ x = 1, y = 1 }` when pressing the up and right arrows.<br>
`{ x = 0, y =-1 }` when pressing the down,left, and right arrows.
|])
  , ("wasd", "Signal { x:Int, y:Int }", [markdown|
Just like the `arrows` signal, but this uses keys `w`, `a`,
`s`, and `d`, which are common controls for many computer games.
|])
  ]

modifiers =
  [ ("shift", "Signal Bool", [markdown|Whether the shift key is pressed.|])
  , ("ctrl" , "Signal Bool", [markdown|Whether the control key is pressed.|])
  , ("space", "Signal Bool", [markdown|Whether the space key is pressed.|])
  ]

categories = [ ("Directions", directions)
             , ("Modifiers", modifiers)
             ]

intro = [markdown|
These are nicely curated inputs from the keyboard.
See the [`Keyboard.Raw` library](/docs/Signal/KeyboardRaw.elm)
for a lower-level interface that will let you define more
complicated behavior.
|]

main = createDocs2 "Keyboard" intro categories
