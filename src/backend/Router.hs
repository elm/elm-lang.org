{-# LANGUAGE OverloadedStrings #-}
module Router (router) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Char8 as BS
import Snap.Core
    ( Snap, MonadSnap, dir, getParam, ifTop, modifyResponse, pass, route
    , setContentType, setResponseStatus, writeBuilder, writeLBS
    )
import Snap.Util.FileServe ( serveDirectoryWith, serveFile, simpleDirectoryConfig )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Generate
import qualified Init.FileTree as FT
import qualified Init.Guide as Guide


router
    :: (String -> Either Json.Value (String,String))
    -> [(FilePath,FilePath)]
    -> Snap ()
router compiler pages =
  ifTop (serveFile (FT.file ["pages"] "home" "html"))
    <|> servePages pages
    <|> route routes
    <|> dir "editor" (serveDirectoryWith simpleDirectoryConfig ("gen" </> "editor"))
    <|> dir "assets" (serveDirectoryWith simpleDirectoryConfig "assets")
    <|> serveDirectoryWith simpleDirectoryConfig "resources"
    <|> error404
  where
    routes =
        -- top-bar routes
        [ ("guide", guide)
        , ("examples/:name", examples)

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
    servePage (path, html) =
        ( BS.pack path
        , ifTop (serveFile html)
        )


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


compile :: (String -> Either Json.Value (String,String)) -> Snap ()
compile compiler =
  do  elmSource <- demandParam "input"
      serveHtml (Generate.userHtml (compiler elmSource))


hotswap :: (String -> Either Json.Value (String,String)) -> Snap ()
hotswap compiler =
  do  elmSource <- demandParam "input"
      modifyResponse $ setContentType "application/javascript"
      let json = Generate.js (compiler elmSource)
      writeLBS (Json.encode json)


error404 :: Snap ()
error404 =
  do  modifyResponse (setResponseStatus 404 "Not found")
      serveFile (FT.file ["pages"] "404" "html")


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
