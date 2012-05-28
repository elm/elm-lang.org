

main = let img = image "elm.jpg" in
       lift1 (\status -> flowDown [ img, asText status ]) $ Mouse.isAbove img