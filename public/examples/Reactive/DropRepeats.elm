
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

latestKey = let step curr prev = maybe prev Char.fromCode curr in
            foldp step '_' Keyboard.Raw.charPressed

display chr =
  plainText $ "Number of non-repeated key presses: " ++ show chr

main = display <~ count (dropRepeats latestKey)