{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Monad (join)
import System.Exit
import Data.Maybe
import Data.Binary.Get
import qualified Data.Text as T hiding(map, concatMap)
import qualified Options.Declarative as D
import Text.Megaparsec.Error

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception (bracket)
import System.IO

import Language.ProtocolBuffers
import Language.ProtocolBuffers.Wire

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

runAsDummyPlugin :: D.Arg "file to be saved" FilePath -> D.Cmd "Dump plugin input" ()
runAsDummyPlugin file = do
  let dumpFile = D.get file
  wfs <- liftIO $ copyStdinToFile dumpFile
  return ()

scan :: D.Arg "file to be scan" FilePath -> D.Cmd "Scan file and show summary" ()
scan file = do
  let file' = D.get file
  wfs <- liftIO $ scanFile file'
  liftIO $ print wfs
  return ()

copyStdinToFile :: FilePath -> IO([WireField a])
copyStdinToFile path = bracket (openFile path WriteMode) hClose
  $ \h -> do
            bs <- BL.getContents
            let !wfs = runGet (many getWireField) bs
            return wfs

scanFile :: FilePath -> IO([WireField a])
scanFile path = bracket (openFile path ReadMode) hClose
  $ \h -> do
            bs <- BL.hGetContents h
            let !wfs = runGet (many getWireField) bs
            return wfs

main :: IO()
main =   D.run_ $ D.Group "Group of actions"
  [
    D.subCmd "parse" parse
  , D.subCmd "plugin"  runAsDummyPlugin
  , D.subCmd "scan"    scan
  ]
