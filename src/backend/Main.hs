{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import GHC.Conc
import Snap.Http.Server (defaultConfig, httpServe, setPort)
import System.Console.CmdArgs

import qualified Init.Compiler as Compiler
import qualified Init.Examples as Examples
import qualified Init.FileTree as FileTree
import qualified Init.Pages as Pages
import qualified Router


data Flags = Flags
    { port :: Int
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { port = 8000 &= help "set the port of the server"
    }


main :: IO ()
main =
  do  setNumCapabilities =<< getNumProcessors

      args <- cmdArgs flags

      FileTree.init
      Examples.init
      Compiler.init
      pages <- Pages.init

      httpServe
          (setPort (port args) defaultConfig)
          (Router.router pages)

