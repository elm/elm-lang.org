module Init.Helpers (isOutdated, make, makeWithStyle, write) where

import Control.Monad.Except (runExceptT, when)
import qualified Data.List as List
import qualified Data.List.Split as List
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.FilePath (splitExtension, takeBaseName)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import qualified Text.Blaze.Html.Renderer.String as Blaze

import qualified Elm.Utils as Utils
import qualified Generate


write :: String -> IO ()
write str =
  hPutStr stdout str >> hFlush stdout


make :: FilePath -> FilePath -> IO Bool
make input output =
  do  outdated <- isOutdated input output
      when outdated (makeForReal input output)
      return outdated


makeWithStyle :: FilePath -> FilePath -> IO Bool
makeWithStyle input output =
  do  outdated <- isOutdated input output

      when outdated $
        do  let (name, ext) = splitExtension output
            let jsOutput = name ++ ".js"

            makeForReal input jsOutput
            case ext of
              ".js" ->
                return ()

              ".html" ->
                do  jsSource <- readFile jsOutput
                    writeFile output
                        (Blaze.renderHtml (Generate.serverHtml (fileToTitle output) jsSource))
                    removeFile jsOutput

              _ ->
                error ("not sure what to do with file extension: " ++ ext)

      return outdated


fileToTitle :: FilePath -> String
fileToTitle filePath =
  List.intercalate " " (List.splitOn "-" (takeBaseName filePath))


makeForReal :: FilePath -> FilePath -> IO ()
makeForReal input output =
  do  compilerResult <-
        runExceptT $
          Utils.run "elm-make" [ "--yes", input, "--output=" ++ output ]

      case compilerResult of
        Left msg ->
          do  putStrLn (" problem compiling " ++ input ++ "\n")
              hPutStrLn stderr msg
              exitFailure

        Right _ ->
          do  return ()


isOutdated :: FilePath -> FilePath -> IO Bool
isOutdated input output =
  do  exists <- doesFileExist output

      if not exists
        then return True
        else
          do  inputTime <- getModificationTime input
              outputTime <- getModificationTime output
              return (inputTime > outputTime)