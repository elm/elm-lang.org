
import Website.ColorScheme

accents = [accent0,accent1,accent2,accent3,accent4]

topBar k n =
    let n' = toFloat n
        k' = toFloat k
        segs = map (\i -> round (n' * toFloat i / k')) [1..k]
        ws = zipWith (-) segs (0:segs)
        accentCycle = concatMap (\_ -> accents) [ 0 .. k `div` 5 ]
    in  flow right $ zipWith (\c w -> color c $ spacer w 5) accentCycle ws

msg = [markdown|
<style type="text/css">p { text-align:justify; }</style>

# Try Elm

To the left is an online editor for writing and compiling [Elm](/) code.
If you are unsure how to get started, take a look at some [examples][exs].

  [exs]: /Examples.elm "Elm Examples"
|]

scene (w,h) = container w h middle (box $ width 320 msg)

box e =
  let { w = widthOf e ; h = heightOf e } in
  flow down [ topBar 5 (w+40)
            , color mediumGrey . container (w+40) (h+11) midTop .
              color lightGrey  . container (w+38) (h+10) midTop $ e
            ]

main = lift scene Window.dimensions