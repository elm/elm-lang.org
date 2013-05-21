{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (head,span,id,catch)
import Control.Monad (msum,when)
import Happstack.Server hiding (body)
import Happstack.Server.Compression

import Text.Blaze.Html (Html)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception

import qualified Language.Elm as Elm (docs)
import ElmToHtml
import Editor
import Utils

-- | Set up the server.
main :: IO ()
main = simpleHTTP nullConf $ do
         docsPath <- liftIO $ Elm.docs
         compressedResponseFilter
         msum [ nullDir >> compileFile "Elm.elm"
              , serveDirectory DisableBrowsing [] "resources"
              , dir "try" (ok $ toResponse $ emptyIDE)
              , dir "compile" $ compilePart
              , dir "jsondocs" $ serveFile (asContentType "text/json") docsPath
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

open :: String -> ServerPart String
open fp = liftIO $ catch (readFile ("public/" ++ fp))
                         ((\_ -> return "") :: SomeException -> IO String)

-- | Do something with the contents of a File.
withFile :: (FilePath -> String -> Html) -> FilePath -> ServerPart Response
withFile handler fp = do
  content <- open fp
  ok . toResponse $ handler fp (content :: String)


-- | Compile an arbitrary Elm program from the public/ directory.
compileFile :: FilePath -> ServerPart Response
compileFile fp =
    do content <- open fp
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
