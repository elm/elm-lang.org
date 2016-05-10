module Init.Compiler (init, compile) where

import Control.Exception (SomeException, bracket, try)
import qualified Data.Time.Clock.POSIX as Time
import qualified Elm.Compiler as Elm
import qualified Elm.Package as Pkg
import Prelude hiding (init)
import System.Directory
  ( createDirectoryIfMissing, getCurrentDirectory, setCurrentDirectory
  , copyFile, removeFile
  , doesFileExist
  )
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>), (<.>))
import System.Process (rawSystem, readProcessWithExitCode)
import qualified Text.Blaze.Html5 as H

import qualified Generate



-- INIT


init :: IO ()
init =
  do  createDirectoryIfMissing True tempDirectory
      copyFile "elm-package.json" (tempDirectory </> "elm-package.json")
      code <- rawSystem "elm-package" ["install", "--yes"]
      case code of
        ExitSuccess ->
          return ()

        ExitFailure _ ->
          exitWith code


tempDirectory :: FilePath
tempDirectory =
  "tmp"



-- COMPILE


compile :: String -> IO H.Html
compile elmSource =
  withCurrentDirectory tempDirectory $
    do  name <- getTempName

        let elmFile = name <.> "elm"
        let jsFile = name <.> "js"

        writeFile elmFile (addHeader name elmSource)

        let args = ["--yes", "--output=" ++ jsFile, elmFile]
        result <- try $ readProcessWithExitCode "elm-make" args ""

        html <-
          case result of
            Left exception ->
              return $ Generate.compilerError (show (exception :: SomeException))

            Right (ExitFailure _, out, err) ->
              return $ Generate.compilerError (out ++ err)

            Right (ExitSuccess, _, _) ->
              do  js <- readFile jsFile
                  length js `seq` return ()
                  removeFile (pathToArtifact name "elmi")
                  removeFile (pathToArtifact name "elmo")
                  removeFile jsFile
                  return $ Generate.compilerSuccess name js


        removeFile elmFile

        return html



addHeader :: String -> String -> String
addHeader name elmSource =
  "module " ++ name ++ " exposing (..)\n" ++ elmSource


pathToArtifact :: String -> String -> FilePath
pathToArtifact name ext =
  "elm-stuff" </> "build-artifacts" </> Pkg.versionToString Elm.version
  </> "user" </> "project" </> "1.0.0"
  </> name <.> ext


withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir doSomeStuff =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ ->
    do  setCurrentDirectory dir
        doSomeStuff



-- TEMPORARY NAMES


getTempName :: IO String
getTempName =
  iterateOnName =<< Time.getPOSIXTime


iterateOnName :: Time.POSIXTime -> IO String
iterateOnName time =
  do  let name = timeToName time
      exists <- doesFileExist (name <.> "elm")
      case exists of
        True ->
          iterateOnName (time - 60)

        False ->
          return name


timeToName :: Time.POSIXTime -> String
timeToName time =
  "Temp" ++ show (round (time * 1000000))
