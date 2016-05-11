{-# LANGUAGE OverloadedStrings #-}
module Router (router) where

import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.UTF8 as Utf8
import Snap.Core
    ( Snap, MonadSnap, dir, getParam, ifTop, modifyResponse, pass, redirect'
    , route, setContentType, setResponseStatus, writeBuilder
    )
import Snap.Util.FileServe ( serveDirectoryWith, serveFile, simpleDirectoryConfig )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html.Renderer.Utf8 as Blaze

import qualified Init.Compiler as Compiler
import qualified Init.FileTree as FT


router :: [(FilePath,FilePath)] -> Snap ()
router pages =
  ifTop (serveFile (FT.file ["pages"] "home" "html"))
    <|> servePages pages
    <|> route routes
    <|> dir "editor" (serveDirectoryWith simpleDirectoryConfig ("gen" </> "editor"))
    <|> dir "assets" (serveDirectoryWith simpleDirectoryConfig "assets")
    <|> serveDirectoryWith simpleDirectoryConfig "resources"
    <|> redirects
    <|> error404
  where
    routes =
        -- top-bar routes
        [ ("examples/:name", examples)

        -- discoverable routes
        , ("try", try)

        -- called by other routes
        , ("code", emptyCode)
        , ("code/:name", code)
        , ("compile", compile)
        ]


servePages :: [(FilePath, FilePath)] -> Snap ()
servePages pairs =
    route (map servePage pairs)
  where
    servePage (path, html) =
        ( Utf8.fromString path
        , ifTop (serveFile html)
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



-- called by other routes


emptyCode :: Snap ()
emptyCode =
  serveFile (FT.file ["examples","code"] "try" "html")


code :: Snap ()
code =
  do  name <- demandParam "name"
      serveIfExists (FT.file ["examples","code"] name "html")


compile :: Snap ()
compile =
  do  elmSource <- demandParam "input"
      serveHtml =<< liftIO (Compiler.compile elmSource)


error404 :: Snap ()
error404 =
  do  modifyResponse (setResponseStatus 404 "Not found")
      serveFile (FT.file ["pages"] "404" "html")



-- HELPERS


demandParam :: Utf8.ByteString -> Snap String
demandParam param =
  do  maybeBS <- getParam param
      maybe pass (return . Utf8.toString) maybeBS


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



-- REDIRECTS


(==>) old new =
  (old, redirect' new 301)


redirects :: MonadSnap m => m ()
redirects =
  route $
    map versionRedirect versions
    ++
    [ "Blog.elm" ==> "/blog"
    , "Community.elm" ==> "/community"
    , "Elm.elm" ==> "/"
    , "Examples.elm" ==> "/examples"
    , "Get-Started.elm" ==> "/get-started"
    , "Install.elm" ==> "/install"
    , "Learn.elm" ==> "/docs"
    , "Libraries.elm" ==> "http://package.elm-lang.org"
    , "blog/Introducing-Elm-Reactor.elm" ==> "/blog/time-travel-made-easy"
    , "blog/Blazing-Fast-Html.elm" ==> "/blog/blazing-fast-html"
    , "blog/announce/PackageManager.elm" ==> "/blog/announce/package-manager"
    , "blog/announce/Repl.elm" ==> "/blog/announce/repl"
    , "blog/Interactive-Programming.elm" ==> "/blog/interactive-programming"
    , "blog/announce/Elm-and-Prezi.elm" ==> "/blog/announce/elm-and-prezi"
    , "learn/Escape-from-Callback-Hell.elm" ==> "/learn/escape-from-callback-hell"
    , "blog/Pong.elm" ==> "/blog/making-pong"
    , "learn/Syntax.elm" ==> "/docs/syntax"
    , "learn/FAQ.elm" ==> "/docs/from-javascript"
    , "learn/Understanding-Types.elm" ==> "/guide/model-the-problem"
    , "learn/Pattern-Matching.elm" ==> "/guide/model-the-problem"
    , "learn/Union-Types.elm" ==> "/guide/model-the-problem"
    , "learn/Records.elm" ==> "/docs/records"
    , "learn/What-is-FRP.elm" ==> "/guide/reactivity" -- TODO
    , "learn/Using-Signals.elm" ==> "/guide/reactivity" -- TODO
    , "learn/Tasks.elm" ==> "/guide/reactivity#tasks"
    , "learn/Components.elm" ==> "/guide/interop"
    , "learn/Ports.elm" ==> "/guide/interop"
    -- TODO /guide/* ==> new home
    , "guide/architecture" ==> "https://github.com/evancz/elm-architecture-tutorial/"
    ]


versions :: [String]
versions =
  [ "0.4.0"
  , "0.5.0"
  , "0.6"
  , "0.7"
  , "0.7.1"
  , "0.8"
  , "0.9"
  , "0.10"
  , "0.10.1"
  , "0.11"
  , "0.12"
  , "0.12.1"
  , "0.12.3"
  , "0.13"
  , "0.14"
  , "0.15"
  ]


versionRedirect :: MonadSnap m => String -> (Utf8.ByteString, m ())
versionRedirect version =
  let
    old =
      "blog/announce/" ++ version ++ ".elm"

    new =
      "/blog/announce/" ++ version
  in
    Utf8.fromString old ==> Utf8.fromString new
