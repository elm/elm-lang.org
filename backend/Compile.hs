module Compile (toJS, removeArtifacts) where

import Control.Exception (catch, SomeException)
import Control.Monad.Error (runErrorT)
import System.Directory (removeFile)
import System.FilePath ((</>), (<.>), dropExtension, replaceExtension, takeFileName)
import System.IO (hClose, hPutStr, openTempFile)

import qualified Elm.Utils as Utils


toJS :: String -> IO (Either String (String, String))
toJS source =
  catchCrashes (compileSource source)


compileSource :: String -> IO (Either String (String, String))
compileSource source =
  do  (elmFilePath, handle) <- openTempFile "." "Temp.elm"
      let moduleName = dropExtension (takeFileName elmFilePath)
      hPutStr handle ("module " ++ moduleName ++ " where\n" ++ source)
      hClose handle

      let jsFilePath = replaceExtension elmFilePath "js"

      compilerResult <-
        runErrorT $
            Utils.run "elm-make" [ elmFilePath, "--output=" ++ jsFilePath ]

      case compilerResult of
        Right _ ->
          do  jsSource <- readFile jsFilePath
              removeFile elmFilePath
              removeFile jsFilePath
              removeArtifacts moduleName
              return (Right (moduleName, jsSource))

        Left msg ->
          do  removeFile elmFilePath
              return (Left $ unlines (drop 3 (lines msg)))


removeArtifacts :: String -> IO ()
removeArtifacts moduleName =
  do  let dir = "elm-stuff" </> "build-artifacts" </> "elm-lang" </> "elm-lang.org" </> "1.0.0"
      removeFile (dir </> moduleName <.> "elmi")
      removeFile (dir </> moduleName <.> "elmo")



catchCrashes :: IO (Either String a) -> IO (Either String a)
catchCrashes io =
    io `catch` recover
  where
    recover :: SomeException -> IO (Either String b)
    recover exception =
        return (Left (show exception))