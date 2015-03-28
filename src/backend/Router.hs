{-# LANGUAGE OverloadedStrings #-}
module Router (router) where

import Control.Applicative ((<|>))
import Control.Monad.Error (liftIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.IO as Text
import Snap.Core
    ( Snap, MonadSnap, dir, getParam, getQueryParam, getRequest, ifTop
    , modifyResponse, pass, route, rqPathInfo, setContentType
    , setResponseStatus, writeBS, writeBuilder
    )
import Snap.Util.FileServe ( serveDirectoryWith, serveFile, simpleDirectoryConfig )
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Generate
import qualified Init.FileTree as FT
import qualified Init.Guide as Guide


router
    :: (String -> Either String (String,String))
    -> [(FilePath,FilePath)]
    -> Snap ()
router compiler pages =
  ifTop root
    <|> servePages pages
    <|> route routes
    <|> dir "editor" (serveDirectoryWith simpleDirectoryConfig ("gen" </> "editor"))
    <|> serveDirectoryWith simpleDirectoryConfig "resources"
    <|> error404
  where
    routes =
        -- top-bar routes
        [ ("guide", guide)
        , ("examples/:name", examples)
        , ("packages", packages)

        -- discoverable routes
        , ("try", try)
        , ("debug", debug)
        , ("install", install)

        -- called by other routes
        , ("code", emptyCode)
        , ("code/:name", code)
        , ("compile", compile compiler)
        , ("hotswap", hotswap compiler)
        ]


servePages :: [(FilePath, FilePath)] -> Snap ()
servePages pairs =
    route (map servePage pairs)
  where
    servePage (path, jsFile) =
        ( BS.pack path
        , ifTop $ do
              jsSource <- liftIO (Text.readFile jsFile)
              serveHtml (Generate.serverHtml path jsSource)
        )


-- top-bar routes

root :: Snap ()
root =
  try


guide :: Snap ()
guide =
  ifTop pass
    <|> route (map chapterToRoute Guide.chapters)
  where
    chapterToRoute name =
        ( BS.pack name
        , ifTop (serveFile (FT.file ["guide","html"] name "html"))
        )


examples :: Snap ()
examples =
  do  name <- demandParam "name"
      directory <-
        ifTop (return "editor")
          <|> dir "code" (return "code")
          <|> dir "result" (return "result")

      serveIfExists (FT.file ["examples",directory] name "html")


packages :: Snap ()
packages =
  undefined


-- discoverable routes

try :: Snap ()
try =
  serveFile (FT.file ["examples","editor"] "try" "html")


debug :: Snap ()
debug =
  undefined


install :: Snap ()
install =
  undefined


-- called by other routes

emptyCode :: Snap ()
emptyCode =
  serveFile (FT.file ["examples","code"] "try" "html")


code :: Snap ()
code =
  do  name <- demandParam "name"
      serveIfExists (FT.file ["examples","code"] name "html")


compile :: (String -> Either String (String,String)) -> Snap ()
compile compiler =
  do  elmSource <- demandParam "input"
      serveHtml (Generate.userHtml (compiler elmSource))


hotswap :: (String -> Either String (String,String)) -> Snap ()
hotswap compiler =
  do  elmSource <- demandParam "input"
      modifyResponse $ setContentType "application/javascript"
      let json = Generate.js (compiler elmSource)
      writeBS (BS.pack json)


error404 :: Snap ()
error404 =
  do  modifyResponse (setResponseStatus 404 "Not found")
      jsSource <- liftIO (Text.readFile (FT.file ["pages"] "404" "js"))
      serveHtml (Generate.serverHtml "Oops!" jsSource)


-- HELPERS

demandParam :: BS.ByteString -> Snap String
demandParam param =
  do  maybeBS <- getParam param
      maybe pass (return . BS.unpack) maybeBS


serveIfExists :: FilePath -> Snap ()
serveIfExists path =
  do  exists <- liftIO (doesFileExist path)
      if exists
          then serveFile path
          else pass


serveHtml :: MonadSnap m => H.Html -> m ()
serveHtml html =
  do  modifyResponse $ setContentType "text/html"
      writeBuilder (Blaze.renderHtmlBuilder html)
