
-- The fps function gives deltas between each update.
-- So summing all of the deltas should give the time
-- that the signal has been running:

main = lift asText (foldp (+) 0 (fps 30))