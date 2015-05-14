module Init.Compiler (init) where

-- make something that depends on literally everything
-- read all the resulting interfaces
-- return a "compile" function and a .js file of all the stuff

import Control.Monad.Except (ExceptT, liftIO, runExceptT, throwError)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Name as N
import qualified Elm.Package.Solution as Solution
import qualified Elm.Package.Version as V
import qualified Elm.Utils as Utils
import Prelude hiding (init)
import System.Exit (exitFailure)
import System.Directory (getDirectoryContents, removeFile)
import System.FilePath ((</>), splitExtension)

import Init.Helpers (make, write)
import qualified Init.FileTree as FT


-- INITIALIZE THE COMPILER

init :: IO (String -> Either String (String, String))
init =
  do  write "Setting up compiler ..."
      result <- runExceptT getInterfaces
      case result of
        Left msg ->
            do  putStrLn " something went wrong!"
                putStrLn msg
                exitFailure

        Right interfaces ->
            do  putStrLn " done\n"
                return (compile interfaces)


compile
    :: Map.Map Module.Name Module.Interface
    -> String
    -> Either String (String, String)
compile interfaces elmSource =
  case rawCompile interfaces elmSource of
    Right v ->
        Right v
    Left msgs ->
        Left (concatMap (Compiler.errorToString "" elmSource) msgs)


rawCompile
    :: Map.Map Module.Name Module.Interface
    -> String
    -> Either [Compiler.Error] (String, String)
rawCompile interfaces elmSource =
  do  (name, _) <- Compiler.parseDependencies elmSource

      let (_warnings, either) =
            Compiler.compile "evancz" "elm-lang" True elmSource interfaces

      (_, jsSource) <- either

      return (Module.nameToString name, jsSource)


-- GET ALL RELEVANT INTERFACES

getInterfaces
    :: ExceptT String IO (Map.Map Module.Name Module.Interface)
getInterfaces =
  do  Utils.run "elm-package" ["install", "--yes"]

      desc <- Desc.read "elm-package.json"
      solution <- Solution.read "elm-stuff/exact-dependencies.json"
      let usedDeps = Map.intersection solution (Map.fromList (Desc.dependencies desc))

      dependencies <- mapM readDependencies (Map.toList usedDeps)

      liftIO $ do
          writeFile "all-modules.elm" (toElmSource dependencies)
          make "all-modules.elm" (FT.file ["editor"] "everything" "js")
          removeFile "all-modules.elm"

      rawInterfaces <- mapM readInterfaces (Map.toList solution)
      return (Map.fromList (concat rawInterfaces))


readDependencies
    :: (N.Name, V.Version)
    -> ExceptT String IO (N.Name, V.Version, [Module.Name])
readDependencies (name, version) =
  do  desc <- Desc.read path
      return (name, version, Desc.exposed desc)
  where
    path =
        "elm-stuff"
          </> "packages"
          </> N.toFilePath name
          </> V.toString version
          </> "elm-package.json"


toElmSource :: [(N.Name, V.Version, [Module.Name])] -> String
toElmSource deps =
  let toImportList (_, _, exposedModules) =
          concatMap toImport exposedModules

      toImport name =
          "import " ++ Module.nameToString name ++ "\n"
  in
      concatMap toImportList deps ++ "\nfortyTwo = 40 + 2"


readInterfaces
    :: (N.Name, V.Version)
    -> ExceptT String IO [(Module.Name, Module.Interface)]
readInterfaces package =
  do  let directory = directoryFor package
      contents <- liftIO (getDirectoryContents directory)
      let elmis = Maybe.mapMaybe isElmi contents
      mapM (readInterface directory) elmis
  where
    directoryFor (name, version) =
        "elm-stuff"
          </> "build-artifacts"
          </> N.toFilePath name
          </> V.toString version

    isElmi file =
      case splitExtension file of
        (name, ".elmi") -> (,) file `fmap` Module.dehyphenate name
        _ -> Nothing

    readInterface directory (file, name) =
      do  bits <- liftIO (LBS.readFile (directory </> file))
          case Binary.decodeOrFail bits of
            Right (_, _, value) ->
                return (name, value)
            Left _ ->
                throwError $
                    " messed up elmi file for " ++ Module.nameToString name

