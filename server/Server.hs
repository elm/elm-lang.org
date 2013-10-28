{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Prelude hiding (head, span, id, catch)
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Monad
import Happstack.Server hiding (body,port)
import Happstack.Server.Compression
import Happstack.Server.FileServe.BuildingBlocks (serveDirectory')
import qualified Happstack.Server as Happs

import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception
import System.Console.CmdArgs
import System.FilePath as FP
import System.Process
import System.Directory
import GHC.Conc

import qualified Language.Elm as Elm
import ElmToHtml
import Editor
import Utils
import Gist
import Data.Acid
import Data.Acid.Local
import Data.Acid.Advanced
import Control.Monad (replicateM)
import Control.Applicative
import Network.HTTP.Base (urlEncode)
import qualified Happstack.Server.SURI as HSURI
import qualified System.FilePath as FP
import Data.Hashable
import Control.Concurrent (killThread, forkIO)

data Flags = Flags
  { port :: Int
  } deriving (Data,Typeable,Show,Eq)

flags = Flags
  { port = 8000 &= help "set the port of the server"
  }

-- | Set up the server.
main :: IO ()
main = do
  setNumCapabilities =<< getNumProcessors
  args <- cmdArgs flags
  putStrLn "Initializing Server"
  precompile
  getRuntimeAndDocs
  
  putStrLn "Initializing Database"
  gistDB <- openLocalState initialGistDB
  wordDB <- initialWordDB >>= openLocalState w
  
  putStrLn $ "Serving at localhost:" ++ show (port args)
  httpThreadId <- forkIO $ simpleHTTP nullConf { Happs.port = port args } $ do
      compressedResponseFilter
      let mime = asContentType "text/html; charset=UTF-8"
      route gistDB wordDB (serveFile mime "public/build/Elm.elm")
            (serveDirectory' EnableBrowsing [] mime "public/build")
  
  waitForTermination
  putStrLn "Killing server"
  killThread httpThreadId
  putStrLn "Creating Checkpoints"
  createCheckpointAndClose gistDB >> createArchive gistDB
  createCheckpointAndClose wordDB >> createArchive wordDB
  
route :: AcidState GistDB -> AcidState WordDB
      -> ServerPartT IO Response -> ServerPartT IO Response -> ServerPartT IO Response
route acidGist acidWord empty rest = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  msum
    [ nullDir >> empty
    , serveDirectory DisableBrowsing [] "resources"
    , dir "try" $ ok $ toResponse $ emptyIDE
    , dir "xkcd" $ method GET >> (path $ \a -> path $ \b -> path $ \c -> path $ \d -> getXKCD (XKCD (a,b,c,d)) acidGist)
    , dir "xkcd" $ method POST >> (path $ \a -> path $ \b -> path $ \c -> path $ \d -> putXKCD (XKCD (a,b,c,d)) acidGist acidWord)
    , dir "xkcd" $ (path $ \a -> getGistByID (read a) acidGist)
    , dir "editXKCD" $ method GET >> (path $ \a -> path $ \b -> path $ \c -> path $ \d -> editXKCD a b c d)
    , dir "compile" $ compilePart (elmToHtml "Compiled Elm")
    , dir "hotswap" $ compilePart elmToJS
    , dir "jsondocs" $ serveFile (asContentType "text/json") "resources/docs.json?0.10"
    , dir "edit" serveEditor
    , dir "code" $ do
                    x <- makeWordsUp acidWord
                    uriRest $ withFile (editor x)
    , dir "login" sayHi
    , rest
    , return404
    ]

-- | Compile an Elm program that has been sent via "input" to the server.
compilePart :: ToMessage a => (String -> a) -> ServerPartT IO Response
compilePart compile = look "input" >>= ok . toResponse . compile

open :: String -> ServerPart (Maybe String)
open fp =
  do exists <- liftIO (doesFileExist file)
     if exists then openFile else return Nothing
  where
    file = "public/" ++ takeWhile (/='?') fp
    openFile = liftIO $ catch (fmap Just $ readFile file) handleError

    handleError :: SomeException -> IO (Maybe String)
    handleError _ = return Nothing

serveEditor :: ServerPart Response
serveEditor = do
  cols <- getDataFn (look "cols")
  uriRest . withFile . ide $ case cols of
                               Left _    -> "50%,50%"
                               Right str -> str

-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  eitherContent <- open fp
  case eitherContent of
    Just content -> ok . toResponse $ handler (FP.takeBaseName fp) content
    Nothing -> return404

ideXKCD :: XKCD -> String -> ServerPart Response
ideXKCD (XKCD (a,b,c,d)) code= ok $ toResponse $ ideBuilder "50%,50%" ("Elm Editor: " ++ (unwords [a,b,c,d]))
                  ("/editXKCD/" ++ a ++ "/" ++ b ++ "/" ++ c ++ "/" ++ d ++ "?input="++ urlEncode code)
                  ("/compile?input=" ++ urlEncode code)

editXKCD :: String -> String -> String -> String -> ServerPart Response
editXKCD a b c d = do
  code <- look "input"
  ok $ toResponse $ editor (XKCD (a,b,c,d)) (unwords ["XKCD: ",a,b,c,d]) code

getXKCD :: XKCD -> AcidState GistDB -> ServerPartT IO Response
getXKCD x acid = do
      Gist code <- query' acid (GetGist $ hash x)
      ideXKCD x code

getGistByID :: Int -> AcidState GistDB -> ServerPartT IO Response
getGistByID a acid = do
  Gist code <- query' acid (GetGist a)
  ok $ toResponse $ elmToHtml "Compiled Elm" code

putXKCD :: XKCD -> AcidState GistDB -> AcidState WordDB -> ServerPartT IO Response
putXKCD x@(XKCD (a,b,c,d)) acidG acidW = do
  code <- look "input"
  res <- query' acidW (MemberWords [a,b,c,d])
  if res
  then do
    saveGist x code
    ideXKCD x code
  else do
    x@(XKCD (a,b,c,d)) <- makeWordsUp acidW
    saveGist x code
    seeOther  ("/xkcd/" ++ a ++ "/" ++ b ++ "/" ++ c ++ "/" ++ d)
            (toResponse ("Redirected" ::String))
  where
    saveGist x c = update' acidG (UpdateGist x $ Gist $ c)

return404 =
  notFound =<< serveFile (asContentType "text/html; charset=UTF-8") "public/build/Error404.elm"

-- | Simple response for form-validation demo.
sayHi :: ServerPart Response
sayHi = do
  first <- look "first"
  last  <- look "last"
  email <- look "email"
  ok . toResponse $
     concat [ "Hello, ", first, " ", last
            , "! Welcome to the fake login-confirmation page.\n\n"
            , "We will not attempt to contact you at ", email
            , ".\nIn fact, your (fake?) email has not even been recorded." ]

-- | Compile all of the Elm files in public/, placing results in public/build/
precompile :: IO ()
precompile =
  do setCurrentDirectory "public"
     files <- getFiles True ".elm" "."
     forM_ files $ \file -> do
       rawSystem "elm" ["--make","--runtime=/elm-runtime.js?v0.10",file]
     htmls <- getFiles False ".html" "build"
     mapM_ adjustHtmlFile htmls
     setCurrentDirectory ".."
  where
    getFiles :: Bool -> String -> FilePath -> IO [FilePath]
    getFiles skip ext dir = do
        case skip && "build" `elem` map dropTrailingPathSeparator (splitPath dir) of
          True -> return []
          False -> do
            contents <- map (dir </>) `fmap` getDirectoryContents dir
            let files = filter ((ext==) . takeExtension) contents
                dirs  = filter (not . hasExtension) contents
            filess <- mapM (getFiles skip ext) dirs
            return (files ++ concat filess)

getRuntimeAndDocs :: IO ()
getRuntimeAndDocs = do
  writeFile "resources/elm-runtime.js" =<< readFile =<< Elm.runtime
  writeFile "resources/docs.json" =<< readFile =<< Elm.docs

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- readFile file
     let (before,after) =
             length src `seq`
             List.break (List.isInfixOf "<title>") (lines src)
     removeFile file
     writeFile (replaceExtension file "elm") (unlines (before ++ [style] ++ after ++ [renderHtml googleAnalytics]))
  where
    style =
        unlines . map ("    "++) $
        [ "<style type=\"text/css\">"
        , "  a:link {text-decoration: none; color: rgb(15,102,230);}"
        , "  a:visited {text-decoration: none}"
        , "  a:active {text-decoration: none}"
        , "  a:hover {text-decoration: underline; color: rgb(234,21,122);}"
        , "  body { font-family: \"Lucida Grande\",\"Trebuchet MS\",\"Bitstream Vera Sans\",Verdana,Helvetica,sans-serif !important; }"
        , "  p, li { font-size: 14px !important;"
        , "          line-height: 1.5em !important; }"
        , "</style>" ]
