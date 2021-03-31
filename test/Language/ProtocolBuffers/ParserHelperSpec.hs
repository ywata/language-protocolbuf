{-# Language OverloadedStrings #-}
module Language.ProtocolBuffers.ParserHelperSpec where

import NeatInterpolation
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.ParserHelper
import Test.Hspec

spec :: Spec
spec = do
  describe "reserved" $ do
    it "float" $ do
      parse (reserved "float") "" "float" `shouldBe` (Right "float")
    it "floatt" $ do
      parseMaybe (reserved "float")  "floatt" `shouldBe` Nothing
    
  describe "decimalLit" $ do
    it "10" $ do
      parse decimalLit "" "10" `shouldBe` (Right "10")
    it "0" $ do
      parse decimalLit "" "0" `shouldBe` (Right "0")
    it "1234" $ do
      parse decimalLit "" "1234" `shouldBe` (Right "1234")

  describe "Parse Float" $ do
    it "inf" $ do
      parse floatLit "" "inf" `shouldBe` Right Inf
    it "nan" $ do
      parse floatLit "" "nan" `shouldBe` Right NaN

    it "1." $ do
      parse floatLit "" "1." `shouldBe` Right (Fl1 "1" Nothing Nothing)
    it "1.1" $ do
      parse floatLit "" "1.1" `shouldBe` Right (Fl1 "1" (Just "1") Nothing)
    it "1.e1" $ do
      parse floatLit "" "1.e1" `shouldBe` Right (Fl1 "1" Nothing (Just "1"))
    it "1.1e1" $ do
      parse floatLit "" "1.1e1" `shouldBe` Right (Fl1 "1" (Just "1") (Just "1"))
    it "0123456789.1234567890e1234567890" $ do
      parse floatLit "" "0123456789.1234567890e1234567890" `shouldBe` Right (Fl1 "0123456789" (Just "1234567890") (Just "1234567890"))
    it ".1" $ do
      parse floatLit "" ".1" `shouldBe` Right (Fl3 "1" Nothing)
    it ".1" $ do
      parse floatLit "" ".1e1" `shouldBe` Right (Fl3 "1" (Just "1"))


    it "10e10" $ do
      parse floatLit "" "10e10" `shouldBe` Right (Fl2 "10" "10")
    it "10e-10" $ do
      parse floatLit "" "10e-10" `shouldBe` Right (Fl2 "10" "-10")
    it "10e+10" $ do
      parse floatLit "" "10e+10" `shouldBe` Right (Fl2 "10" "+10")
    it "012e0" $ do
      parse floatLit "" "012e+0" `shouldBe` Right (Fl2 "012" "+0")

  describe "charValue" $ do
    it "\\x01" $ do
      parse charValue "" "\\x01" `shouldBe` Right (EscHex "01")
    it "\\x23" $ do
      parse charValue "" "\\x23" `shouldBe` Right (EscHex "23")
    it "\\x45" $ do
      parse charValue "" "\\x45" `shouldBe` Right (EscHex "45")
    it "\\x67" $ do
      parse charValue "" "\\x67" `shouldBe` Right (EscHex "67")
    it "\\x89" $ do
      parse charValue "" "\\x89" `shouldBe` Right (EscHex "89")
    it "\\xAB" $ do
      parse charValue "" "\\xAB" `shouldBe` Right (EscHex "AB")
    it "\\xCD" $ do
      parse charValue "" "\\xCD" `shouldBe` Right (EscHex "CD")
    it "\\xEF" $ do
      parse charValue "" "\\xEF" `shouldBe` Right (EscHex "EF")
    it "\\xab" $ do
      parse charValue "" "\\xab" `shouldBe` Right (EscHex "ab")
    it "\\xcd" $ do
      parse charValue "" "\\xcd" `shouldBe` Right (EscHex "cd")
    it "\\xef" $ do
      parse charValue "" "\\xef" `shouldBe` Right (EscHex "ef")
      
    it "\\012" $ do
      parse charValue "" "\\012" `shouldBe` Right (EscOct "012")

    it "\\a" $ do
      parse charValue "" "\\a" `shouldBe` Right (EscChar 'a')
    it "\\b" $ do
      parse charValue "" "\\b" `shouldBe` Right (EscChar 'b')
    it "\\f" $ do
      parse charValue "" "\\f" `shouldBe` Right (EscChar 'f')
    it "\\n" $ do
      parse charValue "" "\\n" `shouldBe` Right (EscChar 'n')
    it "\\r" $ do
      parse charValue "" "\\r" `shouldBe` Right (EscChar 'r')
    it "\\t" $ do
      parse charValue "" "\\t" `shouldBe` Right (EscChar 't')
    it "\\v" $ do
      parse charValue "" "\\v" `shouldBe` Right (EscChar 'v')
    it "\\\\" $ do
      parse charValue "" "\\\\" `shouldBe` Right (EscChar '\\')
    it "\\'" $ do
      parse charValue "" "\\\'" `shouldBe` Right (EscChar '\'')
    it "\\\"" $ do
      parse charValue "" "\\\"" `shouldBe` Right (EscChar '\"')
    it "あ" $ do
      parse charValue "" "あ" `shouldBe` Right (CHAR 'あ')
    it "x" $ do
      parse charValue "" "x" `shouldBe` Right (CHAR 'x')



  describe "string literal" $ do
    let input :: T.Text
        input = "\"\""
    it (T.unpack input) $ do
      parse strLit "" input `shouldBe` Right (StringLit [])
    let input :: T.Text
        input = "\'\'"
    it (T.unpack input) $ do
      parse strLit "" input `shouldBe` Right (StringLit [])
    let input :: T.Text
        input = "\'a\\n\\x01\\0123\\\'\\\"あ\'"
    it (T.unpack input) $ do
      parse strLit "" input `shouldBe` Right (StringLit [CHAR 'a',EscChar 'n',EscHex "01",EscOct "012",CHAR '3',EscChar '\'',EscChar '"',CHAR '\12354'])


        
