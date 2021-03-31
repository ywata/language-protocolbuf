{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ProtocolBuffers.PrettyPrintSpec where


import NeatInterpolation
import Data.Text as T
import Text.Megaparsec hiding(parse, parseMaybe)
import qualified Text.Megaparsec as P (parse, parseMaybe)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser
import Language.ProtocolBuffers.PrettyPrint
import Test.Hspec
import Text.PrettyPrint (render)
import Control.Monad.IO.Class (liftIO)

prettyParse :: T.Text -> Either (ParseErrorBundle T.Text Char) T.Text
prettyParse = fmap (T.pack . render . pretty) . parseProtocolBuffer

rightShow :: Either (ParseErrorBundle T.Text Char) T.Text -> T.Text
rightShow (Right a) = a
rightShow _ = ""

spec :: Spec
spec = do
  describe "PrettyPrint" $ do
    it "EmptyDecl" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
package test;
import public "test.a";
import weak "test.b";
import "test.c";
option java_package = "com.example";
message A{
  int32 days = 1;
  option allow_signal = true;
  enum B {
    B1 = 1;
    B2 = 2 [custom_option = "hello!"];
  };
  opeof C {
    string a = 1;
  };
}
|]
          p = prettyParse input
      liftIO . print $ p
      liftIO . putStrLn . T.unpack $ rightShow p
      p `shouldBe` (Right "")

