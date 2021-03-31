{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ProtocolBuffersSpec where


import NeatInterpolation
import Data.Text as T
import Text.Megaparsec hiding(parse, parseMaybe)
import qualified Text.Megaparsec as P (parse, parseMaybe)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser
import Test.Hspec


spec :: Spec
spec = do
  describe "syntax" $ do
    it "Syntax" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
;
|]
      parseProtocolBuffer input `shouldBe` (Right (ProtocolBuffer []))
  
  
  describe "Full" $ do
    it "Full" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
package org.github.proto;
import "org.github";
import weak "org.github";
import public "org.github";
option java_package = "org.github";
message A{
  int int_field1 = 1;
  repeated int int_field2 = 2;
  required int int_field3 = 3;
  optional int int_field4 = 4;
  int int_field5 = 5 [default = 1];
  A msg_filed = 6;
  oneof foo {
    string name = 7;
    string nickname = 8;
  }
  reserved 10;
  reserved "f1";
  reserved "f2", "f3";  
}
|]
      parseProtocolBuffer input `shouldBe` (Right (ProtocolBuffer []))
    it "Full" $ do
      let input :: T.Text    
          input = [text|
syntax = "proto3";

message A{
  reserved 1;
  reserved 5,6;
  reserved 3 to 4;
  reserved 7,8 to 9;
  reserved "af1", "f2";
}
|]
      parseProtocolBuffer input `shouldBe` Right (ProtocolBuffer  [])


  describe "Types" $ do
    it "primitive types" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
/*
type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
      | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
      | "bool" | "string" | "bytes" | messageType | enumType
*/      
message Primitive {
  double d = 1;
  float f = 2;
  int32 i32 = 3;
  int64 i64 = 4;
  uint32 ui32 = 5;
  uint64 ui64 = 6;
  sint32 si32 = 7;
  sint64 si64 = 8;
  fixed32 f32 = 9;
  fixed64 f64 = 10;
  sfixed32 sf32 = 11;
  sfixed64 sf64 = 12;
  bool b = 13;
  string s = 14;
  bytes bs = 15;
}
|]
      parseProtocolBuffer input `shouldBe` (Right (ProtocolBuffer []))

    it "Enum" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
enum One {ZERO1 = 0; }
enum Two {ZERO2 = 0; ONE2 = 1;} 
enum Three {ZERO3 = 0; ONE3= 1; TWO2 = 2;} [(custom_option) = ""]

message Primitive {
  One one = 1;
  Two two = 2;
  Three three = 3;
}
|]
      parseProtocolBuffer input `shouldBe` (Right (ProtocolBuffer []))
    it "EmptyDecl" $ do
      let input :: T.Text    
          input = [text|
syntax = 'proto3';
;
message A{
  ;
}
|]
      parseProtocolBuffer input `shouldBe` (Right (ProtocolBuffer[]))



