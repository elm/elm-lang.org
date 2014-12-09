{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Lazy.IO as Text
import Control.Applicative
import Control.Monad.Error
import GHC.Conc
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.CmdArgs
import System.Directory
import System.FilePath as FP
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Text as BlazeText
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeBS

import qualified Compile
import qualified Editor
import qualified Elm.Utils as Utils
import qualified Generate


data Flags = Flags
  { port :: Int
  }
  deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }


main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors
      putStrLn "Initializing Server"
      setupLogging
      args <- cmdArgs flags
      precompile
      httpServe (setPort (port args) defaultConfig) $
          ifTop (serveElm "artifacts/Elm.elm")
          <|> route [ ("try", serveHtml Editor.empty)
                    , ("edit", edit)
                    , ("code", code)
                    , ("compile", compile)
                    , ("hotswap", hotswap)
                    , ("login", login)
                    ]
          <|> serveDirectoryWith directoryConfig "artifacts"
          <|> serveDirectoryWith simpleDirectoryConfig "resources"
          <|> error404


error404 :: Snap ()
error404 =
  do  modifyResponse $ setResponseStatus 404 "Not found"
      serveElm "artifacts/Error404.elm"


serveElm :: FilePath -> Snap ()
serveElm =
  serveFileAs "text/html; charset=UTF-8"


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse $ setContentType "text/html"
      writeBuilder (BlazeBS.renderHtmlBuilder html)


hotswap :: Snap ()
hotswap =
    maybe error404 serve =<< getParam "input"
  where
    serve src =
      do  modifyResponse $ setContentType "application/javascript"
          result <- liftIO . Generate.js $ BSC.unpack src
          writeBS (BSC.pack result)


compile :: Snap ()
compile =
    maybe error404 serve =<< getParam "input"
  where
    serve src =
      do  result <- liftIO . Generate.html "Compiled Elm" $ BSC.unpack src
          serveHtml result


edit :: Snap ()
edit =
  do  cols <- BSC.unpack . maybe "50%,50%" id <$> getQueryParam "cols"
      path <- BSC.unpack . rqPathInfo <$> getRequest
      let maybePath =
            case path of
              "empty" -> Nothing
              _ -> Just path

      serveHtml (Editor.ide cols maybePath)


code :: Snap ()
code =
  do  path <- BSC.unpack . rqPathInfo <$> getRequest
      case path of
        "empty" ->
          serveHtml (Editor.editor "Empty.elm" "")

        _ ->
          do  let file = "frontend/public/" ++ path
              exists <- liftIO (doesFileExist file)
              if not exists
                then error404
                else
                  do  content <- liftIO $ readFile file
                      serveHtml (Editor.editor path content)


-- | Simple response for form-validation demo.
login :: Snap ()
login =
  do  first <- maybe "John" id <$> getQueryParam "first"
      last' <- maybe "Doe" id <$> getQueryParam "last"
      email <- maybe "john.doe@example.com" id <$> getQueryParam "email"
      writeBS $
        BS.concat
        [ "Hello, ", first, " ", last'
        , "! Welcome to the fake login-confirmation page.\n\n"
        , "We will not attempt to contact you at ", email
        , ".\nIn fact, your (fake?) email has not even been recorded."
        ]


directoryConfig :: MonadSnap m => DirectoryConfig m
directoryConfig =
    fancyDirectoryConfig
    { indexGenerator = defaultIndexGenerator defaultMimeTypes indexStyle
    , mimeTypes = Map.insert ".elm" "text/html" defaultMimeTypes
    }


indexStyle :: BS.ByteString
indexStyle =
    "body { margin:0; font-family:sans-serif; background:rgb(245,245,245);\
    \       font-family: calibri, verdana, helvetica, arial; }\
    \div.header { padding: 40px 50px; font-size: 24px; }\
    \div.content { padding: 0 40px }\
    \div.footer { display:none; }\
    \table { width:100%; border-collapse:collapse; }\
    \td { padding: 6px 10px; }\
    \tr:nth-child(odd) { background:rgb(216,221,225); }\
    \td { font-family:monospace }\
    \th { background:rgb(90,99,120); color:white; text-align:left;\
    \     padding:10px; font-weight:normal; }"


setupLogging :: IO ()
setupLogging =
  do  createDirectoryIfMissing True "log"
      createIfMissing "log/access.log"
      createIfMissing "log/error.log"
  where
    createIfMissing path =
      do  exists <- doesFileExist path
          when (not exists) $ BS.writeFile path ""


-- PRECOMPILE ELM TO HTML

{-| frontend/public/*.elm => artifacts/*.elm as HTML
-}
precompile :: IO ()
precompile =
  do  setCurrentDirectory ("frontend" </> "public")
      files <- getFiles ".elm" "."
      setCurrentDirectory (".." </> "..")

      let numFiles = length files
      forM_ (zip [1..] files) $ \(index, filePath) ->
          do  putStr $ "\rPrepping file " ++ show index ++ " of " ++ show numFiles
              hFlush stdout

              let source = "frontend" </> "public" </> filePath
              let result = "artifacts" </> filePath

              exists <- doesFileExist result

              shouldCompile <-
                if not exists
                  then return True
                  else
                    do  sourceTime <- getModificationTime source
                        resultTime <- getModificationTime result
                        return (sourceTime > resultTime)

              when shouldCompile (compileFile filePath)

      putStrLn ""
      exists <- doesFileExist "elm.js"
      when exists (removeFile "elm.js")


getFiles :: String -> FilePath -> IO [FilePath]
getFiles ext directory =
  do  contents <- map (directory </>) `fmap` getDirectoryContents directory

      let files =
            filter ((ext==) . FP.takeExtension) contents

      let directories =
            filter (not . FP.hasExtension) contents

      filess <- mapM (getFiles ext) directories

      return (files ++ concat filess)


compileFile :: FilePath -> IO ()
compileFile filePath =
  do  compilerResult <-
        runErrorT $
          Utils.run "elm-make" [ "--yes", "frontend" </> "public" </> filePath ]
  
      case compilerResult of
        Left msg ->
          do  putStrLn ""
              hPutStrLn stderr msg

        Right _ ->
          do  jsSource <- Text.readFile "elm.js"
              Compile.removeArtifacts "Main"
              let fileName = FP.dropExtension (FP.takeFileName filePath)
              let html = Generate.htmlSkeleton False fileName (Generate.scripts "Main" jsSource)
              createDirectoryIfMissing True (dropFileName ("artifacts" </> filePath))
              Text.writeFile ("artifacts" </> filePath) (BlazeText.renderHtml html)
