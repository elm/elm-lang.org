{-# OPTIONS_GHC -W #-}
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as Map
import Control.Applicative
import Control.Monad.Error

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeBS
import qualified Text.Blaze.Html.Renderer.String as BlazeS

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Console.CmdArgs
import System.Exit (ExitCode(..))
import System.FilePath as FP
import System.IO (hFlush, stdout)
import System.Process
import System.Directory
import GHC.Conc

import qualified Elm.Internal.Paths as Elm
import qualified Generate
import qualified Editor

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags :: Flags
flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  putStrLn "Initializing Server"
  getRuntimeAndDocs
  setupLogging
  precompile
  cargs <- cmdArgs flags
  httpServe (setPort (port cargs) defaultConfig) $
      ifTop (serveElm "build/public/Elm.elm")
      <|> route [ ("try", serveHtml Editor.empty)
                , ("edit", edit)
                , ("code", code)
                , ("compile", compile)
                , ("hotswap", hotswap)
                , ("login", login)
                ]
      <|> serveDirectoryWith directoryConfig "build/public/"
      <|> serveDirectoryWith simpleDirectoryConfig "resources"
      <|> error404

error404 :: Snap ()
error404 =
    do modifyResponse $ setResponseStatus 404 "Not found"
       serveElm "build/public/Error404.elm"

serveElm :: FilePath -> Snap ()
serveElm = serveFileAs "text/html; charset=UTF-8"

serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
    do setContentType "text/html" <$> getResponse
       writeLBS (BlazeBS.renderHtml html)

hotswap :: Snap ()
hotswap = maybe error404 serve =<< getParam "input"
    where
      serve src = do
        setContentType "application/javascript" <$> getResponse
        result <- liftIO . Generate.js $ BSC.unpack src
        writeBS (BSC.pack result)

compile :: Snap ()
compile = maybe error404 serve =<< getParam "input"
    where
      serve src = do
        result <- liftIO . Generate.html "Compiled Elm" $ BSC.unpack src
        serveHtml result

edit :: Snap ()
edit = do
  cols <- BSC.unpack . maybe "50%,50%" id <$> getQueryParam "cols"
  withFile (Editor.ide cols)

code :: Snap ()
code = withFile Editor.editor

withFile :: (FilePath -> String -> H.Html) -> Snap ()
withFile handler = do
  path <- BSC.unpack . rqPathInfo <$> getRequest
  let file = "public/" ++ path         
  exists <- liftIO (doesFileExist file)
  if not exists then error404 else
      do content <- liftIO $ readFile file
         serveHtml $ handler path content

-- | Simple response for form-validation demo.
login :: Snap ()
login = do
  first <- maybe "John" id <$> getQueryParam "first"
  last' <- maybe "Doe" id <$> getQueryParam "last"
  email <- maybe "john.doe@example.com" id <$> getQueryParam "email"
  writeBS $ BS.concat [ "Hello, ", first, " ", last'
                      , "! Welcome to the fake login-confirmation page.\n\n"
                      , "We will not attempt to contact you at ", email
                      , ".\nIn fact, your (fake?) email has not even been recorded." ]

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
    do createDirectoryIfMissing True "log"
       createIfMissing "log/access.log"
       createIfMissing "log/error.log"
    where
      createIfMissing path = do
        exists <- doesFileExist path
        when (not exists) $ BS.writeFile path ""

-- | Compile all of the Elm files in public/, placing results in build/public/
precompile :: IO ()
precompile =
  do files <- getFiles ".elm" "public"
     compilationHack files
     htmls <- getFiles ".html" "build"
     mapM_ adjustHtmlFile htmls
  where
    getFiles :: String -> FilePath -> IO [FilePath]
    getFiles ext directory = do
      contents <- map (directory </>) `fmap` getDirectoryContents directory
      let files = filter ((ext==) . FP.takeExtension) contents
          directories  = filter (not . FP.hasExtension) contents
      filess <- mapM (getFiles ext) directories
      return (files ++ concat filess)

    -- There's a problem with the builder that native files will be included
    -- even if they are not needed. This avoids that.
    compilationHack files =
        do hide
           forM_ files $ \file -> do
             exitCode <- compileTry1 file
             case exitCode of
               ExitSuccess -> return ()
               ExitFailure _ -> compileTry2 file
           unhide
           putStrLn ""
        where
          unhide = renameFile "resources/elm_dependencies.json" "elm_dependencies.json"
          hide   = renameFile "elm_dependencies.json" "resources/elm_dependencies.json"

          compileTry1 file =
              do putStr "."
                 hFlush stdout
                 (exitCode, _, _) <- readProcessWithExitCode "elm" (flags file) ""
                 return exitCode

          compileTry2 file =
              do unhide
                 (exitCode, out, err) <- readProcessWithExitCode "elm" (flags file) ""
                 case exitCode of
                   ExitSuccess -> putStr "." >> hFlush stdout
                   ExitFailure _ -> putStrLn ("\n" ++ out ++ err)
                 hide

          flags file =
              [ "--make"
              , "--set-runtime=/elm-runtime.js"
              , "--src-dir=public"
              , file
              ]


getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  writeFile "resources/elm-runtime.js" =<< readFile Elm.runtime
  writeFile "resources/docs.json" =<< readFile Elm.docs

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- BSC.readFile file
     let (before, after) = BSC.breakSubstring "<title>" src
     BSC.writeFile (FP.replaceExtension file "elm") $
        BSC.concat [before, style, after, analytics]
     removeFile file

style :: BSC.ByteString
style = 
    "<style type=\"text/css\"></style>"

-- | Add analytics to a page.
analytics :: BSC.ByteString
analytics = BSC.pack . BlazeS.renderHtml $
    H.script ! A.type_ "text/javascript" $
         "var _gaq = _gaq || [];\n\
         \_gaq.push(['_setAccount', 'UA-25827182-1']);\n\
         \_gaq.push(['_setDomainName', 'elm-lang.org']);\n\
         \_gaq.push(['_trackPageview']);\n\
         \(function() {\n\
         \  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
         \  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
         \  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
         \})();"
