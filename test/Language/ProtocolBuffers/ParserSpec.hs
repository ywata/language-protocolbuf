{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ProtocolBuffers.ParserSpec where

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
  describe "option" $ do
    it "File level empty string" $ do
      let input :: T.Text    
          input = [text|
option java_package = "";
|]
      P.parse topOption "" input `shouldBe` Right (Option (Regular ["java_package"]) (KString (StringLit [])))
  describe "option" $ do
    it "File level empty string" $ do
      let input :: T.Text    
          input = [text|
option value.float_opt = 0.1e+10;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Regular ["value", "float_opt"]) (KFloat (Fl1 "0" (Just "1") (Just "+10"))))
    it "File level option 0.1" $ do
      let input :: T.Text    
          input = [text|
option float_opt = 0.1;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Regular ["float_opt"]) (KFloat (Fl1 "0" (Just "1")Nothing)))
    it "File level option 0.1" $ do
      let input :: T.Text    
          input = [text|
option float_opt = 0.1;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Regular ["float_opt"]) (KFloat (Fl1 "0" (Just "1") Nothing)))
    it "File level option 0E-10" $ do
      let input :: T.Text    
          input = [text|
option float_opt = 0E-10;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Regular ["float_opt"]) (KFloat (Fl2 "0" "-10")))

    it "Custom option single" $ do
      let input :: T.Text    
          input = [text|
option (custom) = 1;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Custom ["custom"] []) (KInt (Dec "1")))
    it "Custom option multiple" $ do
      let input :: T.Text    
          input = [text|
option (custom1.custom2) = 1;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Custom ["custom1", "custom2"] []) (KInt (Dec "1")))
    it "Custom ref" $ do
      let input :: T.Text    
          input = [text|
option (custom1.custom2).custom3 = 1;
|]
      P.parse topOption "" input `shouldBe` Right (Option (Custom ["custom1", "custom2"] ["custom3"]) (KInt (Dec "1")))




  describe "innerOtion" $ do
    it "File level empty string" $ do
      let input :: T.Text    
          input = [text|
value.float_opt = "";
|]
      P.parse innerOption "" input `shouldBe` Right (Option (Regular ["value", "float_opt"]) (KString (StringLit [])))

  describe "enumField" $ do
    it "option" $ do
      let input :: T.Text    
          input = [text|
option test = "s";
|]
      P.parse enumThing "" input `shouldBe` Right (EnumThingOption (Option (Regular ["test"]) (KString (StringLit [CHAR 's']))))
    it "enumFiled without option" $ do
      let input :: T.Text    
          input = [text|
E1 = 2;
|]
      P.parse enumThing "" input `shouldBe` Right (EnumThingField (EnumField "E1" (Dec "2") []))
    it "enumFiled with option" $ do
      let input :: T.Text    
          input = [text|
E1 = 2[test = 1];
|]
      P.parse enumThing "" input `shouldBe` Right (EnumThingField (EnumField "E1" (Dec "2") [Option (Regular ["test"]) (KInt (Dec "1"))]))
