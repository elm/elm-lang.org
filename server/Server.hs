{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head,span,id)
import Control.Monad (msum,when)
import Happstack.Server hiding (body)
import Happstack.Server.Compression

import Text.Blaze.Html (Html)
import Control.Monad.Trans (MonadIO(liftIO))

import ElmEditor


compilePart :: ServerPart Response
compilePart = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  code <- look "input"
  ok $ toResponse $ compile "Compiled Elm" code

main :: IO ()
main = simpleHTTP nullConf $ do
         compressedResponseFilter
         msum [ nullDir >> compileFile "Elm.elm"
              , serveDirectory DisableBrowsing [] "resources"
              , dir "compile" $ compilePart
              , dir "edit" . uriRest $ withFile editor
              , dir "code" . uriRest $ withFile codeFrame
              , dir "login" sayHi
              , uriRest compileFile
              ]

withFile f fp = do
  content <- liftIO $ readFile ("public/" ++ fp)
  ok . toResponse $ f fp content

compileFile fp =
    let pageTitle = reverse . takeWhile (/='/') . drop 4 $ reverse fp in
    do content <- liftIO $ readFile ("public/" ++ fp)
       ok $ toResponse $ compile pageTitle content

sayHi = do
  first <- look "first"
  last  <- look "last"
  email <- look "email"
  ok . toResponse $
     concat [ "Hello, ", first, " ", last
            , "! Welcome to the fake login-confirmation page.\n\n"
            , "We will not attempt to contact you at ", email
            , ".\nIn fact, your (fake?) email has not even been recorded." ]