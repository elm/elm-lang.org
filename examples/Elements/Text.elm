
main = flowDown $ map (padding 5 . text)
    [ header "Make headers"
    , bold "Bold"
    , italic "Italicize"
    , overline "Overline"
    , strikeThrough "Strike Through"
    , underline "Underline"
    , link "/docs/docs.elm" "Create links"
    , textColor red "Set color"
    ]