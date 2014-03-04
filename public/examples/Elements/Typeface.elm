
msg : Text
msg = toText "\nThe quick brown fox jumps over the lazy dog.\n"

main : Element
main = flow down <| map (\tf -> leftAligned (typeface tf msg))
    [ ["times new roman"]
    , ["helvetica", "sans-serif"]
    , ["georgia", "serif"]
    , ["trebuchet ms", "sans-serif"]
    , ["inconsolata, courier new, monospace"]
    ]


{-- More Information on Typefaces -----------------------------------

The typeface provided is a list of possible options. Each typeface is
tried (left-to-right) until one is found that is available on the
client machine. You do not *need* to specify multiple typefaces, but
it is a good idea just in case someone is missing the particular
typeface you want.

Look into the CSS font-family property to learn more.

--------------------------------------------------------------------}
