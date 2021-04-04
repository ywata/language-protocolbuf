{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ProtocolBuffers.ParserSpec where

import NeatInterpolation
import Data.Text as T
import qualified Text.Megaparsec as P (parse, parseMaybe)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.ParserHelper
import Language.ProtocolBuffers.Parser
import Test.Hspec



spec :: Spec
spec = do
  describe "identifier" $ do
    it "succeeds" $ do
      let input :: T.Text    
          input = [text|
abcdefefghijklmnopqrstuvwxyzABCDEFEFGHIJKLMNOPQRSTUVWXYZ_0123456789]
|]
      P.parse identifier "" input `shouldBe` Right "abcdefefghijklmnopqrstuvwxyzABCDEFEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

    it "fail" $ do
      let input :: T.Text    
          input = [text|
ab.cdefefghijklmnopqrstuvwxyzABCDEFEFGHIJKLMNOPQRSTUVWXYZ_012345678
|]
      P.parse identifier "" input `shouldBe` Right "ab"
  describe "fullIdentifier" $ do      
    it "succeeds" $ do
      let input :: T.Text    
          input = [text|
ab.cdefefghijklmnopqrstuvwxyzABCDEFEFGHIJKLMNOPQRSTUVWXYZ_012345678
|]
      P.parse fullIdentifier "" input `shouldBe` Right ["ab", "cdefefghijklmnopqrstuvwxyzABCDEFEFGHIJKLMNOPQRSTUVWXYZ_012345678"]
      

  describe "constant" $ do
    it "true" $ do
      let input :: T.Text    
          input = [text|
true
|]
      P.parse constant "" input `shouldBe` Right (KBool True)
    it "false" $ do
      let input :: T.Text    
          input = [text|
false
|]
      P.parse constant "" input `shouldBe` Right (KBool False)
    it "decimal" $ do
      let input :: T.Text    
          input = [text|
1
|]
      P.parse constant "" input `shouldBe` Right (KInt (Dec U "1"))
    it "decimal" $ do
      let input :: T.Text    
          input = [text|
-1
|]
      P.parse constant "" input `shouldBe` Right (KInt (Dec U "1"))
    it "octal" $ do
      let input :: T.Text    
          input = [text|
01
|]
      P.parse constant "" input `shouldBe` Right (KInt (Oct U "1"))
    it "hex" $ do
      let input :: T.Text    
          input = [text|
0x1
|]
      P.parse constant "" input `shouldBe` Right (KInt (Hex U "1"))
    it "string" $ do
      let input :: T.Text    
          input = [text|
"a"
|]
      P.parse constant "" input `shouldBe` Right (KString (StringLit [CHAR 'a']))
    it "identifier" $ do
      let input :: T.Text    
          input = [text|
identifier
|]
      P.parse constant "" input `shouldBe` Right (KIdentifier ["identifier"])

    it "hex" $ do
      let input :: T.Text    
          input = [text|
0x1
|]
      P.parse constant "" input `shouldBe` Right (KInt (Hex U "1"))
      
    it "integer" $ do
      let input :: T.Text    
          input = [text|
abc = 1
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KInt (Dec U "1")))
    it "positive integer" $ do
      let input :: T.Text    
          input = [text|
abc = +1
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KInt (Dec P "1")))
    it "negative integer" $ do
      let input :: T.Text    
          input = [text|
abc = -1
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KInt (Dec N "1")))
    it "negative float" $ do
      let input :: T.Text    
          input = [text|
abc = -1.0
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KFloat (Fl1 P "1" (Just "0") Nothing)))
    it "positive float" $ do
      let input :: T.Text    
          input = [text|
abc = +1.0
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KFloat (Fl1 P "1" (Just "0") Nothing)))
      
    it "true" $ do
      let input :: T.Text    
          input = [text|
abc = true
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KBool True))
    it "false" $ do
      let input :: T.Text    
          input = [text|
abc = false
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KBool False))
    it "string" $ do
      let input :: T.Text    
          input = [text|
abc = "b"
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KString (StringLit [CHAR 'b'])))
    it "identifier" $ do
      let input :: T.Text    
          input = [text|
abc = ident.fier
|]
      P.parse option "" input `shouldBe` Right (Option (Regular ["abc"]) (KIdentifier ["ident", "fier"]))




  describe "options" $ do
    it "one" $ do
      let input :: T.Text    
          input = [text|
[abc = 1]
|]
      P.parse (betweenSquares options) "" input `shouldBe` Right [Option (Regular ["abc"]) (KInt (Dec U "1"))]
    it "two" $ do
      let input :: T.Text    
          input = [text|
[abc = 1, abc = 2]
|]
      P.parse (betweenSquares options) "" input `shouldBe` Right [Option (Regular ["abc"]) (KInt (Dec U "1")), Option (Regular ["abc"]) (KInt (Dec U "2"))]

  describe "messageField" $ do
    it "empty" $ do
      let input :: T.Text    
          input = [text|
;
|]
      P.parse messageField "" input `shouldBe` Right MEmpty
      
    it "bool" $ do
      let input :: T.Text    
          input = [text|
bool d = 1;
|]
      P.parse messageField "" input `shouldBe` Right (MField Single TBool "d" (Dec U "1") [])
    it "repeated" $ do
      let input :: T.Text    
          input = [text|
repeated bool d = 3333;
|]
      P.parse messageField "" input `shouldBe` Right (MField Repeated TBool "d" (Dec U "3333") [])

    it "required bool" $ do
      let input :: T.Text    
          input = [text|
required bool d = 1;
|]
      P.parse messageField "" input `shouldBe` Right (MField Required TBool "d" (Dec U "1") [])
    it "optional bool" $ do
      let input :: T.Text    
          input = [text|
optional bool d = 2;
|]
      P.parse messageField "" input `shouldBe` Right (MField Optional TBool "d" (Dec U "2") [])



    it "bool with default" $ do
      let input :: T.Text    
          input = [text|
bool d = 1 [integer = 1, text = "string", boolean = true, boolean = false];
|]
      P.parse messageField "" input `shouldBe` Right (MField Single TBool "d" (Dec U "1") [Option (Regular ["default"]) (KBool True)])


