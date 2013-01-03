

main = let img = image "elm.jpg" in
       lift (\status -> flowDown [ img, asText status ]) $ Mouse.isAbove img