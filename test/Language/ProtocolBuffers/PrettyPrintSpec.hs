{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ProtocolBuffers.PrettyPrintSpec where


import NeatInterpolation
import Data.Text as T hiding(reverse) 
import Text.Megaparsec hiding(parse, parseMaybe)
import qualified Text.Megaparsec as P (parse, parseMaybe)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser
import Language.ProtocolBuffers.PrettyPrint
import Test.Hspec
import Test.QuickCheck
import Text.PrettyPrint (render)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)
import Data.Either (isRight)
import Debug.Trace (trace)

parsePrettyParse :: T.Text -> Either (ParseErrorBundle T.Text Char) (ProtocolBuffer IntLit FloatLit StringLit)
parsePrettyParse txt = trace (show pp ) ppp
  where
    pp  = fmap (T.pack . render . pretty) $ parseProtocolBuffer txt
    ppp = join $ fmap parseProtocolBuffer pp


showEither (Right a) = show a
showEither (Left e)  = show e

rightShow :: Either (ParseErrorBundle T.Text Char) T.Text -> T.Text
rightShow (Right a) = a
rightShow _ = ""



spec :: Spec
spec = do
  describe "PrettyPrint" $ do
    it "misc" $ do
      let input :: T.Text
          input = [text|
syntax = 'proto3';
import public "test.a";
import weak "test.b";
import "test.c";
option java_package = "com.example";
option allow_signal = true;
message A {
  int32 month = -1;
  message AA {
    int32 days = 2;
  }
  enum BB {
    BB1 = 1;
  }
}
|]
          p1 = parseProtocolBuffer input
          p2 = parsePrettyParse input
      liftIO $ putStr $ T.unpack input
      p2 `shouldBe` p1
{-package test;
import public "test.a";
import weak "test.b";
import "test.c";
option java_package = "com.example";
option allow_signal = true;  
message A {
  int32 month = -1;
  message AA {
    int32 days = 2;
  }
  enum BB {
    BB1 = 1;
  }
  int32 days = 1;
  required int32 days = 1;  
  int32 days = 1 [test = 1];
  int32 days = 1 [test = 1, test = 2];
  option allow_signal = true;  
  enum B {
    B2 = 2 [custom_option = "hello!"];
    B1 = 1;
  };
  oneof C {
    string a = 1;
  };
  optional bool client_streaming = 5;
  bool client_streaming = 5 [default = true];
  optional bool client_streaming = 5 [default = false];
}
-}
