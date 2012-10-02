
msg = toText "\nThe quick brown fox jumps over the lazy dog.\n"

main = flow down $ map (\tf -> text $ typeface tf msg)
    [ "times new roman"
    , "helvetica, sans-serif"
    , "georgia, serif"
    , "trebuchet ms, sans-serif"
    , "courier new, monospace"
    ]


{-- More Information on Typefaces -----------------------------------

The typeface provided is a comma separated list of possible options.
Each typeface is tried (left-to-right) until one is found that is
available on the client machine. So you do not need to specify
multiple typefaces, but it is a good policy, just in case someone
is missing the typeface you want.

Look into the CSS font-family property to learn more.

--------------------------------------------------------------------}
