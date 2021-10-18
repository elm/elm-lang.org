module Data.Registry.Package exposing
  ( Package, Name, Key
  , toKey, toName
  , keyFromName, nameFromKey
  , toDocsLink
  )

import Data.Version as V


type alias Package =
  { author : String
  , project : String
  , version : V.Version
  }


type alias Name =
  String


type alias Key =
  ( String, String )


toKey : Package -> Key
toKey pkg =
  ( pkg.author, pkg.project )


toName : Package -> Name
toName pkg =
  pkg.author ++ "/" ++ pkg.project



-- CONVERSION


keyFromName : Name -> Maybe Key
keyFromName str =
  case String.split "/" str of
    author :: project :: [] -> Just ( author, project )
    _ -> Nothing


nameFromKey : Key -> Name
nameFromKey ( author, project ) =
  author ++ "/" ++ project



-- DOCS


toDocsLink : Package -> String
toDocsLink package =
  "https://package.elm-lang.org/packages/" ++ toName package ++ "/" ++ V.toString package.version