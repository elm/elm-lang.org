module Init.Compiler (init, compile) where

import Control.Exception (SomeException, bracket, try)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Text as Text
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

        let args = ["--yes", "--report=json", "--output=" ++ jsFile, elmFile]
        result <- try $ readProcessWithExitCode "elm-make" args ""

        html <-
          case result of
            Left exception ->
              return $ Generate.compilerError (toCrashJson exception)

            Right (ExitFailure _, stdout, stderr) ->
              let
                json =
                  if take 2 stdout == "[{" then
                    stdout
                  else
                    show stderr
              in
                return $ Generate.compilerError json

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



-- RECOVER FROM CRASHES


toCrashJson :: SomeException -> String
toCrashJson exception =
  BS.toString $ Json.encode $ Json.toJSON $ (:[]) $ Json.object $
    [ "region" ==>
        Json.object [ "start" ==> zero, "end" ==> zero ]
    , "subregion" ==>
        Json.Null
    , "tag" ==>
        "COMPILER ERROR"
    , "overview" ==>
        ( "Looks like you ran into an issue with the compiler!\n"
          ++ "It crashed with the following message:\n\n"
          ++ show exception
        )
    , "details" ==>
        "Maybe it was <https://github.com/elm-lang/elm-compiler/issues/832>\n\n\
        \If not, try to find it at <https://github.com/elm-lang/elm-compiler/issues>\n\
        \and open a new issue if it seems like you found an unknown bug."
    ]


zero :: Json.Value
zero =
  Json.object
    [ "line" ==> (0 :: Int)
    , "column" ==> (0 :: Int)
    ]


(==>) :: Json.ToJSON a => String -> a -> Json.Pair
(==>) field value =
  (Text.pack field, Json.toJSON value)
