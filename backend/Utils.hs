{-# LANGUAGE OverloadedStrings #-}
module Utils where

import qualified Data.Text as Text
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


-- | Add analytics to a page.
googleAnalytics :: Html
googleAnalytics =
    H.script ! A.type_ "text/javascript" $
        "!function(e,l,m,L,a,n,g){e.GoogleAnalyticsObject=m;e[m]||(e[m]=function(){\n\
        \(e[m].q=e[m].q||[]).push(arguments)});e[m].l=+new Date;n=l.createElement(L);\n\
        \g=l.getElementsByTagName(L)[0];n.src=a;g.parentNode.insertBefore(n,g)}\n\
        \(window,document,'ga','script','//www.google-analytics.com/analytics.js');\n\
        \\n\
        \ga('create', 'UA-25827182-1', 'auto');\n\
        \ga('send', 'pageview');\n"


standardStyle :: Text.Text
standardStyle = 
    "html,head,body { padding:0; margin:0; }\n\
    \body { font-family: 'Lucida Grande','Trebuchet MS','Bitstream Vera Sans',Verdana,Helvetica,sans-serif; }\n\
    \a {\n\
    \  color: #1184CE;\n\
    \  text-decoration: none;\n\
    \}\n\
    \a:hover {\n\
    \  text-decoration: underline;\n\
    \  color: rgb(234,21,122);\n\
    \}\n\
    \h1,h2,h3,h4 { font-weight:normal; font-family: futura, 'century gothic', 'twentieth century', calibri, verdana, helvetica, arial; }\n\
    \p, li {\n\
    \  font-size: 14px !important;\n\
    \  line-height: 1.5em !important;\n\
    \}\n\
    \pre {\n\
    \  margin: 0;\n\
    \  padding: 10px;\n\
    \  background-color: rgb(254,254,254);\n\
    \  border-style: solid;\n\
    \  border-width: 1px;\n\
    \  border-color: rgb(245,245,245);\n\
    \  border-radius: 6px;\n\
    \}\n"