{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.ProtocolBuffers.WireSpec where

import Language.ProtocolBuffers.Wire

import Data.Fixed hiding (Fixed(..))
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Int

import Test.QuickCheck hiding (Fixed(..))
import Test.Hspec
import Control.Monad (forM)

import Debug.Trace

spec :: Spec
spec = do
  describe "unzig . zig = id" $ do
    it "Int8" $ property $
      (\(i :: Int8) -> (unzig . zig) i `shouldBe` i)
    it "Int32" $ property $
      (\(i :: Int32) -> (unzig . zig) i `shouldBe` i)
    it "Int64" $ property $
      (\(i :: Int64) -> (unzig . zig) i `shouldBe` i)

  describe "decode . encode = id" $ do
    it "Word32" $ property
      $ (\(i :: Word32) -> decode (encode i) `shouldBe` i)
    it "Word64" $ property
      $ (\(i :: Word64) -> decode (encode i) `shouldBe` i)
    it "Int32" $ property
      $ (\(i :: Int32) -> decode (encode i) `shouldBe` i)
    it "Int64" $ property
      $ (\(i :: Int64) -> decode (encode i) `shouldBe` i)
    it "Fixed Int16" $ property
      $ (\(i :: Fixed Int16) -> decode (encode i) `shouldBe` i)
    it "Fixed Int32" $ property
      $ (\(i :: Fixed Int32) -> decode (encode i) `shouldBe` i)
    it "Fixed Int64" $ property
      $ (\(i :: Fixed Int64) -> decode (encode i) `shouldBe` i)
    it "Signed Int32" $ property
      $ (\(i :: Signed Int32) -> decode (encode i) `shouldBe` i)
    it "Signed Int64" $ property
      $ (\(i :: Signed Int64) -> decode (encode i) `shouldBe` i)
    it "Float" $ property
      $ (\(i :: Float) -> decode (encode i) `shouldBe` i)
    it "Double" $ property
      $ (\(i :: Double) -> decode (encode i) `shouldBe` i)
    it "Bool" $ property
      $ (\(i :: Bool) -> decode (encode i) `shouldBe` i)

  describe "WireField fieldNumber" $ do
    it "fieldNumber 1" $ do
      let v = 1 :: Int32
          WireField f' wt' v' = getAfterPut_wireField 1 v
      f' `shouldBe` v
    it "fieldNumber 256" $ do
      let v = 256 :: Int32
          WireField f' wt' v' = getAfterPut_wireField 256 v
      f' `shouldBe` v
  describe "WireField Value" $ do
    it "Int32" $ do
      let v = -912 :: Int32
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Int64" $ do
      let v = -983 :: Int64
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Fixed Int32" $ do
      let v = -912 :: Fixed Int32
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Fixed Int64" $ do
      let v = -983 :: Fixed Int64
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Signed Int32" $ do
      let v = -912 :: Signed Int32
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Signed Int64" $ do
      let v = -983 :: Signed Int64
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Word32" $ do
      let v = 9912 :: Word32
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v
    it "Word64" $ do
      let v = 9983 :: Word64
          WireField f' wt' v' = getAfterPut_wireField 1 v
      (decode v') `shouldBe` v

    it "String with ?????????????????????" $ do
      let v = "test?????????????????????" :: String
          WireField f' wt' v' = getAfterPut_wireField 256 v
      (decode v') `shouldBe` v

  describe "WireField WiredType" $ do
    it "VarInt Int32" $ do
      let v = 1 :: Int32
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` (wireType v)
      wt' `shouldBe` VarInt
    it "VarInt Int64" $ do
      let v = 1 :: Int64
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` (wireType v)
      wt' `shouldBe` VarInt

    it "Fixed32 fixed32" $ do
      let v = 0 :: Fixed Int32
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` Fixed32
    it "Fixed32 float" $ do
      let v = 98.0 :: Float
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` Fixed32
    it "Fixed64" $ do
      let v = 0 :: Fixed Int64
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` Fixed64
    it "Signed Int32" $ do
      let v = 999 :: Signed Int32
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` VarInt
    it "Signed Int64" $ do
      let v = 9 :: Signed Int64
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` VarInt

    it "Double" $ do
      let v = 100.9 :: Double
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` Fixed64
    it "String with ?????????????????????" $ do
      let v = "test?????????????????????" :: String
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` LengthDelim

  describe "0" $ do
    it "Int32" $ do
      let v = 0 :: Int32
          WireField f' wt' v' = getAfterPut_wireField 256 v
      wt' `shouldBe` wireType v
      wt' `shouldBe` VarInt

getAfterPut_wireField :: Wire a => FieldNumber -> a -> WireField a
getAfterPut_wireField f a = runGet getWireField (trace (show $ bs) bs)
  where
     bs = runPut (putWireField f a)


prop_zig_unzig8 :: Int8 -> Bool
prop_zig_unzig8 i = unzig (zig i) == i

prop_zig_unzig32 :: Int32 -> Bool
prop_zig_unzig32 i = unzig (zig i) == i

prop_zig_unzig64 :: Int64 -> Bool
prop_zig_unzig64 i = unzig (zig i) == i

prop_Word32 :: Word32 -> Bool
prop_Word32 i = decode (encode i) == i

prop_Word64 :: Word64 -> Bool
prop_Word64 i = decode (encode i) == i

prop_Int32 :: Int32 -> Bool
prop_Int32 i = decode (encode i) == i

prop_Int64 :: Int64 -> Bool
prop_Int64 i = decode (encode i) == i

prop_SignedInt8 :: Signed Int8 -> Bool
prop_SignedInt8 i = decode (encode i) == i

prop_SignedInt32 :: Signed Int32 -> Bool
prop_SignedInt32 i = decode (encode i) == i

prop_SignedInt64 :: Signed Int64 -> Bool
prop_SignedInt64 i = decode (encode i) == i

prop_FixedInt16 :: Fixed Int16 -> Bool
prop_FixedInt16 i = decode (encode i) == i

prop_FixedInt32 :: Fixed Int32 -> Bool
prop_FixedInt32 i = decode (encode i) == i

prop_FixedInt64 :: Fixed Int64 -> Bool
prop_FixedInt64 i = decode (encode i) == i

prop_Float :: Float -> Bool
prop_Float i = decode (encode i) == i

prop_Double :: Double -> Bool
prop_Double i = decode (encode i) == i

prop_Bool :: Bool -> Bool
prop_Bool b = decode (encode b) == b

instance Arbitrary (Signed Int8) where
  arbitrary  = Signed <$> arbitrary

instance Arbitrary (Signed Int32) where
  arbitrary  = Signed <$> arbitrary

instance Arbitrary (Signed Int64) where
  arbitrary  = Signed <$> arbitrary

instance Arbitrary (Fixed Int16) where
  arbitrary  = Fixed <$> arbitrary

instance Arbitrary (Fixed Int32) where
  arbitrary  = Fixed <$> arbitrary

instance Arbitrary (Fixed Int64) where
  arbitrary  = Fixed <$> arbitrary
