module Search (box, results) where

import Char
import Http
import Json
import JavaScript.Experimental as JSE
import Graphics.Input as Input

(box, searchTerm) = Input.field "  filter libraries"

docs = Http.sendGet (constant "/docs.json")

moduleList docs =
    case docs of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) -> map (.name . JSE.toRecord . Json.toJSObject) xs
            _ -> []
      _ -> []

isInfix sub string =
    let len = length sub
        sub' = map Char.toLower sub
        go str = 
            case str of
              [] -> False
              hd::tl -> take len str == sub' || go tl
    in  go (map Char.toLower string)

search docs term =
    filter (isInfix term) docs

results = search <~ (moduleList <~ docs)
                  ~ searchTerm