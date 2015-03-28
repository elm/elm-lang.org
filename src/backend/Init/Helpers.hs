module Init.Helpers (isOutdated, make, write) where

import Control.Monad.Error (runErrorT, when)
import System.Directory (doesFileExist, getModificationTime)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import qualified Elm.Utils as Utils


write :: String -> IO ()
write str =
  hPutStr stdout str >> hFlush stdout


make :: FilePath -> FilePath -> IO Bool
make input output =
  do  outdated <- isOutdated input output
      when outdated (makeForReal input output)
      return outdated


makeForReal :: FilePath -> FilePath -> IO ()
makeForReal input output =
  do  compilerResult <-
        runErrorT $
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