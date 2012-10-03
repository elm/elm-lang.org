{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head,span,id)
import Control.Monad (msum,when)
import Happstack.Server hiding (body)
import Happstack.Server.Compression

import Text.Blaze.Html (Html)
import Control.Monad.Trans (MonadIO(liftIO))

import ElmToHtml
import Editor
import Utils

-- | Set up the server.
main :: IO ()
main = simpleHTTP nullConf $ do
         compressedResponseFilter
         msum [ nullDir >> compileFile "Elm.elm"
              , serveDirectory DisableBrowsing [] "resources"
              , dir "compile" $ compilePart
              , dir "edit" . uriRest $ withFile ide
              , dir "code" . uriRest $ withFile editor
              , dir "login" sayHi
              , uriRest compileFile
              ]


-- | Compile an Elm program that has been POST'd to the server.
compilePart :: ServerPart Response
compilePart = do
  decodeBody $ defaultBodyPolicy "/tmp/" 0 10000 1000
  code <- look "input"
  ok $ toResponse $ elmToHtml "Compiled Elm" code


-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  content <- liftIO $ readFile ("public/" ++ fp)
  ok . toResponse $ handler fp content


-- | Compile an arbitrary Elm program from the public/ directory.
compileFile :: FilePath -> ServerPart Response
compileFile fp =
    do content <- liftIO $ readFile ("public/" ++ fp)
       ok $ toResponse $ elmToHtml (pageTitle fp) content


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