
add t d = let vs = Dict.findWithDefault [] t.id d
          in  Dict.insert t.id ((t.x,t.y):vs) d

scene (w,h) = collage w h . map (solid red . line)

main = let ts = foldp (\ts d -> foldl add d ts) Dict.empty Touch.touches
       in  lift2 scene Window.dimensions (lift Dict.values ts)
