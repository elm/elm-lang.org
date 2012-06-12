
-- Focus on the display screen (i.e. click the right half of this window)
-- and start pressing keys!

update chr oldChr = case chr of { Just c -> c ; Nothing -> oldChr }
latest sig = foldp update '_' sig

main = lift asText $ latest Keyboard.Raw.charPressed