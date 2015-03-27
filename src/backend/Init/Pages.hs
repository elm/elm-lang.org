module Init.Pages (init) where

import Control.Monad (forM)
import Elm.Utils ((|>))
import Prelude hiding (init)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), (<.>), dropExtension, hasExtension, joinPath, takeExtension)
import System.IO (hFlush, stdout)

import qualified Init.FileTree as FT
import Init.Helpers (make)


init :: IO [(FilePath, FilePath)]
init =
  do  files <- getFiles ("src" </> "pages") ".elm"

      let numFiles = length files
      result <-
        forM (zip [1..] files) $ \(index, name) ->
          do  putStr $ "\rPrepping file " ++ show index ++ " of " ++ show numFiles
              hFlush stdout
              let input = "src" </> "pages" </> name <.> "elm"
              let output = FT.file ["pages"] name "js"
              make input output
              return (name, output)

      putStrLn ""
      return result


getFiles :: FilePath -> String -> IO [FilePath]
getFiles root ext =
  getFilesHelp root ext []


getFilesHelp :: FilePath -> String -> [FilePath] -> IO [FilePath]
getFilesHelp root ext dirs =
  do  let directory = root </> joinPath dirs
      contents <- getDirectoryContents directory

      let files =
            contents
              |> filter (\name -> ext == takeExtension name)
              |> map (joinPath . rightCons dirs . dropExtension)

      let subDirs =
            filter (not . hasExtension) contents

      subFiles <- mapM (getFilesHelp root ext . rightCons dirs) subDirs

      return (files ++ concat subFiles)
  where
    rightCons xs x =
        xs ++ [x]