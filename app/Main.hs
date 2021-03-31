{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (join)
import System.Exit
import Data.Maybe
import qualified Data.Text as T hiding(map, concatMap)
import qualified Options.Declarative as D

import Control.Monad.IO.Class

import Language.ProtocolBuffers.Parser
import Language.ProtocolBuffers.Types

parse :: D.Arg "filename" FilePath -> D.Cmd "parse proto file" ()
parse file = do
  let protoFile = D.get file
  p <- liftIO $ parseProtoBufFile protoFile
  liftIO $ putStr $ protoFile ++ ":"
  liftIO $ print p
  case p of
    Right ok -> liftIO $ exitWith ExitSuccess
    Left ng -> liftIO $ exitWith (ExitFailure 1)

main :: IO()
main =   D.run_ $ D.Group "Group of actions"
  [
    D.subCmd "parse" parse
  ]

