{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (join)
import System.Exit
import Data.Maybe
import qualified Data.Text as T hiding(map, concatMap)
import qualified Options.Declarative as D
import Text.Megaparsec.Error

import Control.Monad.IO.Class

import Language.ProtocolBuffers

instance ShowErrorComponent Char where
  showErrorComponent c = show c

parse :: D.Arg "filename" FilePath -> D.Cmd "parse proto file" ()
parse file = do
  let protoFile = D.get file
  p <- liftIO $ parseProtocolBufferFile protoFile
  case p of
    Right ok -> do
      liftIO $ print p
      liftIO $ exitWith ExitSuccess
    Left bndl@(ParseErrorBundle a b) -> do
      liftIO $ print p
      liftIO $ putStrLn $ errorBundlePretty bndl
      liftIO $ exitWith (ExitFailure 1)

main :: IO()
main =   D.run_ $ D.Group "Group of actions"
  [
    D.subCmd "parse" parse
  ]

