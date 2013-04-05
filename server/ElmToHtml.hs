{-# LANGUAGE OverloadedStrings #-}
module ElmToHtml (elmToHtml) where

import Text.Blaze (preEscapedToMarkup)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Network.HTTP.Base (urlEncode)

import qualified Language.Elm as Elm
import Utils

-- | Using a page title and the full source of an Elm program, compile down to
--   a valid HTML document.
elmToHtml :: String -> String -> H.Html
elmToHtml name src =
  H.docTypeHtml $ do
      H.head $ do
        H.meta ! A.charset "UTF-8"
        H.title . H.toHtml $ name
        H.style ! A.type_ "text/css" $ preEscapedToMarkup
         ("a:link {text-decoration: none; color: rgb(15,102,230);}\
          \a:visited {text-decoration: none}\
          \a:active {text-decoration: none}\
          \a:hover {text-decoration: underline; color: rgb(234,21,122);}" :: String)
      H.body $ do
        H.script ! A.type_ "text/javascript" ! A.src (H.toValue ("/elm-mini.js" :: String)) $ ""
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup (Elm.compile src)
        H.script ! A.type_ "text/javascript" $ preEscapedToMarkup ("Elm.init(Elm." ++ Elm.moduleName src ++ ")")
        googleAnalytics
