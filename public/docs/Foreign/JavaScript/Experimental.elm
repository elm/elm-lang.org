
import Website.Docs (createDocs)

casts =
  [ ("castJSElementToElement" , "Int -> Int -> JSElement -> Element", "All Elm Elements must have a concrete size, so the desired width and height of the JSElement must be provided.")
  , ("castElementToJSElement" , "Element -> JSElement", "This produces DOM nodes. Do not depend on the internal structure of the resulting node. It can and will change. I repeat, DO NOT depend on the internal structure of the resulting node! When your code breaks because you have ignored this warning, do not be surprised!")
  ]

categories = [ ("Experimental Conversions", casts) ]

main = createDocs "JavaScript.Experimental" categories