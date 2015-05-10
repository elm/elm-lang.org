module Init.Pages (init) where

import Elm.Utils ((|>))
import Prelude hiding (init)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), (<.>), dropExtension, hasExtension, joinPath, takeExtension)

import qualified Init.FileTree as FT
import Init.Helpers (makeWithStyle, write)


init :: IO [(FilePath, FilePath)]
init =
  do  files <- getFiles ("src" </> "pages") ".elm"

      let numFiles = length files
      result <- mapM (initFile numFiles) (zip [1..] files)

      putStrLn "done\n"
      return result


initFile :: Int -> (Int, String) -> IO (String, FilePath)
initFile numFiles (index, name) =
  do  write $ "\rSetting up pages (" ++ show index ++ " of " ++ show numFiles ++ ") "

      let input = "src" </> "pages" </> name <.> "elm"
      let output = FT.file ["pages"] name "html"

      makeWithStyle input output

      return (name, output)


-- COLLECT FILES

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