{-# LANGUAGE OverloadedStrings #-}
module Init.FileTree (file, init) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Prelude hiding (init)
import qualified System.Directory as D
import System.FilePath ((</>), (<.>))


-- FILE TREE

data FileTree
    = Dir String [FileTree]
    | File String


fileTree :: [FileTree]
fileTree =
  [ Dir "log"
      [ File "access.log"
      , File "error.log"
      ]
  , Dir "gen"
      [ Dir "examples"
          [ Dir "code" []
          , Dir "result" []
          , Dir "editor" []
          ]
      , Dir "editor" []
      , Dir "pages" []
      ]
  ]


-- PATH LOOKUP

file :: [String] -> String -> String -> FilePath
file dirs fileName extension =
  dir dirs </> fileName <.> extension


dir :: [String] -> FilePath
dir dirs =
  case dirHelp fileTree ("gen" : dirs) of
    Nothing ->
        error ("could not find directory " ++ List.intercalate "/" dirs)

    Just path ->
        path


dirHelp :: [FileTree] -> [String] -> Maybe FilePath
dirHelp trees dirs =
  case dirs of
    [] -> Just ""

    name : otherNames ->
        case trees of
          [] ->
              Nothing

          Dir dirName subTrees : _
              | name == dirName ->
                  (name </>) `fmap` dirHelp subTrees otherNames

          _ : otherTrees ->
              dirHelp otherTrees dirs


-- INITIALIZE

init :: IO ()
init =
  mapM_ (initFileTree ".") fileTree


initFileTree :: FilePath -> FileTree -> IO ()
initFileTree dir tree =
  case tree of
    Dir name subTrees ->
        do  let newDir = dir </> name
            D.createDirectoryIfMissing True newDir
            mapM_ (initFileTree newDir) subTrees

    File name ->
        do  let path = dir </> name
            exists <- D.doesFileExist path
            when (not exists) $ BS.writeFile path ""
