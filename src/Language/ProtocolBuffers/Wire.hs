{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.ProtocolBuffers.Wire where
{-
Google briefly explains how wire format is constructed.
https://developers.google.com/protocol-buffers/docs/encoding
-}

import Unsafe.Coerce
--import Data.Fixed hiding (Fixed(..))
import Data.Int
import Data.Word

import Data.Bits

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS (pack, unpack, ByteString(..))
import qualified Data.ByteString.Lazy as BL (pack, hGetContents, unpack, ByteString(..))
import qualified Data.ByteString.UTF8 as UBS
import qualified Data.Text as T
import Control.Monad.IO.Class

import Control.Applicative (many)
import Control.Exception (bracket)
import System.IO (openFile, hClose, hFlush, IOMode(..))

import Debug.Trace


newtype Signed a = Signed a
  deriving (Show, Ord, Real, Eq, Enum, Bits, Integral, Num) via a
newtype Fixed a = Fixed a
  deriving (Show, Ord, Real, Eq, Enum, Bits, Integral, Num) via a

type FieldNumber = Int32

-- The MSB of a byte is a continuation mark if the data is var int (variable length encoded), i.e,
-- integers, enum or bool, next byte is also a part of the var int.
contBit :: Word8
contBit = 0x80
mask7 :: Word8
mask7 = complement contBit

mask8 :: Word8
mask8 = complement 0


-- shift7 is for var ints.
shift7 :: (Integral a, Bits a) => a -> Int -> Word8
shift7 a n = fromIntegral ((shift a (-7 * n)) .&. 0x7F)
-- shift8 is for fixed length data.
shift8 :: (Integral a, Bits a) => a -> Int -> Word8
shift8 a n = fromIntegral ((shift a (-8 * n)) .&. 0xFF)

-- mark contBit set if a byte has continuation data.
setContFlags :: [Word8] -> [Word8]
setContFlags [] = []
setContFlags [x] = [x]
setContFlags (x : xs) = (x .|. contBit) : setContFlags xs

-- | type class for converting data and [Word8].
class Wire a where
  wireType :: a -> WireType
  encode :: a -> [Word8]
  decode :: [Word8] -> a

-- Wire format
-- type 0 : int32, int64, uint32, uint64, sint32, sint64, bool, enum
instance Wire Int32 where
  wireType = const VarInt
  encode i  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 i) (reverse [0..4])
  decode xs = fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)
instance Wire Int64 where
  wireType = const VarInt
  encode i  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 i) (reverse [0..9])
  decode xs = fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)

-- Word corresponds to Unsigned.
instance Wire Word32 where
  wireType = const VarInt
  encode i  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 i) (reverse [0..4])
  decode xs = fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)

instance Wire Word64 where
  wireType = const VarInt
  encode i  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 i) (reverse [0..9])
  decode xs = fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)

-- Signed integers
instance Wire (Signed Int8) where
  wireType = const VarInt
  encode i  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 i) (reverse [0..4])
  decode xs = fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)

instance Wire (Signed Int32) where
  wireType = const VarInt
  encode (Signed i)  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 (zig32 i)) (reverse [0..4])
  decode xs = Signed . unzig32 . fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)
instance Wire (Signed Int64) where
  wireType = const VarInt
  encode (Signed i)  = setContFlags $ reverse $ dropWhile (== 0) $ map (shift7 (zig64 i)) (reverse [0..9])
  decode xs = Signed . unzig64 . fromIntegral $ foldl f 0 (reverse xs)
    where f c v = c * 128 + toInteger (v .&. mask7)



-- Fixed Ints.
instance Wire (Fixed Int16) where
  wireType = const Fixed32  
  encode i  = map (shift8 i) [0..1]
  decode xs = fromIntegral $ foldl f 0 (reverse xs) - 2^16
    where f c v = c * 256 + toInteger (v .&. mask8)
instance Wire (Fixed Int32) where
  wireType = const Fixed32
  encode i  = map (shift8 i) [0..3]
  decode xs = fromIntegral $ foldl f 0 (reverse xs) - 2^32
    where f c v = c * 256 + toInteger (v .&. mask8)
instance Wire (Fixed Int64) where
  wireType = const Fixed64
  encode i  = map (shift8 i) [0..7]
  decode xs = fromIntegral $ foldl f 0 (reverse xs) - 2^64
    where f c v = c * 256 + toInteger (v .&. mask8)


-- Converting Float and Double into a Word32(or 64) seems to require unsafeCoerce.
instance Wire Float where
  wireType = const Fixed32
  encode i = encode w32
    where w32 :: Word32
          w32 = unsafeCoerce i
  decode xs = f
    where w32 :: Word32
          w32 = decode xs
          f :: Float
          f = unsafeCoerce w32
      
instance Wire Double where
  wireType = const Fixed64
  encode i = encode w64
    where w64 :: Word64
          w64 = unsafeCoerce i
  decode xs = d
    where w64 :: Word64
          w64 = decode xs
          d :: Double
          d = unsafeCoerce w64


instance Wire BS.ByteString where
  wireType = const LengthDelim
  encode = BS.unpack
  decode = BS.pack

instance Wire BL.ByteString where
  wireType = const LengthDelim
  encode = BL.unpack
  decode = BL.pack

-- This implmentation can be very inefficient.
instance Wire String where
  wireType = const LengthDelim
  encode = BS.unpack . UBS.fromString
  decode = UBS.toString . BS.pack
-- This implmentation can be very inefficient too.
instance Wire T.Text where
  wireType = const  LengthDelim
  encode = BS.unpack . UBS.fromString . T.unpack
  decode = T.pack . UBS.toString . BS.pack

instance Wire Bool where
  wireType = const LengthDelim
  encode False = encode (0 :: Int32)
  encode True  = encode (1 :: Int32)
  decode [] = False
  decode _  = True -- not sure


-- zig zag encoding
-- zig8 is implemented to understand the conversion or haskell libraries.
zig8 :: Int8 -> Word8
zig8 n = l `xor` r
  where 
        l, r :: Word8
        l = fromIntegral (shiftL n 1) -- 1 bit left shift = (* 2)
        r = fromIntegral (shiftR n 7) -- arithmetic 7 bit right shift = fill byte with MSB (sign)
unzig8 :: Word8 -> Int8
unzig8 n = l `xor` msk
  where
        msk :: Int8
        msk = if n .&. 1 == 1 then -1 else 0 -- 0xFF if original data is negative otherwise 0x00
        l :: Int8
        l = fromIntegral (shiftR n 1) -- inverse of l in zig8.

zig32 :: Int32 -> Word32
zig32 n = l `xor` r
  where 
        l, r :: Word32
        l = fromIntegral (shiftL n 1)
        r = fromIntegral (shiftR n 31)
unzig32 :: Word32 -> Int32
unzig32 n = l `xor` msk
  where
        msk :: Int32
        msk = if n .&. 1 == 1 then -1 else 0
        l :: Int32
        l = fromIntegral (shiftR n 1)

zig64 :: Int64 -> Word64
zig64 n = l `xor` r
  where 
        l, r :: Word64
        l = fromIntegral (shiftL n 1)
        r = fromIntegral (shiftR n 63)

unzig64 :: Word64 -> Int64
unzig64 n = l `xor` msk
  where
        msk :: Int64
        msk = if n .&. 1 == 1 then -1 else 0
        l :: Int64
        l = fromIntegral (shiftR n 1)


data WireType = VarInt | Fixed64 | LengthDelim | StartGroup | EndGroup | Fixed32
  deriving (Show, Read, Eq, Bounded, Enum)

data WireField a where
  WireField :: Int32 {- ? -} -> WireType -> [Word8] -> WireField a
  WireGroup :: [WireField a] -> WireField a
  deriving (Show, Read, Eq)


readWire :: FilePath -> IO [WireField a]
readWire path = bracket (openFile path ReadMode) hClose $ \h -> do
  bs <- BL.hGetContents h
  let r = runGet (many getWireField) bs
  print r
  return r


getWireField :: Get (WireField a)
getWireField = getWireField' []
getWireField' :: [WireField a] -> Get (WireField a)
getWireField' wfs = do
  v <- getVarInt
  let l = getFieldNum v
      wt = getWireType v
  case wfs of
    [] -> case (l, wt) of
        (Just len, Just w@VarInt)      -> do
          v <- getVarInt
          return $ WireField len w v
        (Just len, Just w@Fixed32)     -> do
          v <- getSizedBytes 4
          return $ WireField len w v
        (Just len, Just w@Fixed64)     -> do
          v <- getSizedBytes 8
          return $ WireField len w v
        (Just len, Just w@LengthDelim) -> do
          v <- getVarInt
          let strLen = decode v :: Int32
          vs <- getSizedBytes (fromIntegral strLen)
          return $ WireField len w vs
        (Just len, Just w@StartGroup)  -> getWireField' []
--        (Just len, Just w@EndGroup)    -> error "should not happen" -- getWireField' [] -- this is error
        _ -> fail "illegal format"        
    _ -> case (l, wt) of
        (Just len, Just w@VarInt)      -> do
          v <- getVarInt
          getWireField' (WireField len w v : wfs)
        (Just len, Just w@Fixed32)     -> do
          v <- getSizedBytes 4
          getWireField' (WireField len w v : wfs)
        (Just len, Just w@Fixed64)     -> do
          v <- getSizedBytes 8
          getWireField' (WireField len w v : wfs)
        (Just len, Just w@LengthDelim) -> do
          v <- getVarInt
          let strLen = decode v :: Int32
          vs <- getSizedBytes (fromIntegral strLen)
          getWireField' (WireField len w vs: wfs)
        (Just len, Just w@StartGroup)  -> getWireField' []
        (Just len, Just w@EndGroup)  -> return $ WireGroup (reverse wfs)
        _ -> fail "illegal format"


putWireField ::Wire a =>  FieldNumber -> a -> Put
putWireField i v = do
  let hdr = combineWireHeader i wt
      wt = wireType v
  putByteString $ BS.pack hdr
  case wt of
    VarInt  -> putByteString $ BS.pack $ encode v
    Fixed32 -> putByteString $ BS.pack $ encode v
    Fixed64 -> putByteString $ BS.pack $ encode v
    LengthDelim -> do
      let len :: Int32
          len = fromIntegral $ length $ encode v
      putByteString (BS.pack $ encode len)
      putByteString (BS.pack $ encode v)
--  StartGroup ->
--  EndGroup -> 
--  _ -> fail "Unknown WireType value"


mask :: (Bits a, Integral a) => Int -> a
mask n = foldr (.|.) 0 (map bit [0..n-1])
wtShift :: Int
wtShift = 3


getWireType :: [Word8] -> Maybe WireType
getWireType [] = Nothing
getWireType ws | length ws > 10 = Nothing
getWireType ws | otherwise = Just . toEnum $ ix
  where
    i32 :: Int32
    i32 = decode ws
    ix :: Int
    ix = fromIntegral (i32 .&. (mask wtShift))

getFieldNum :: [Word8] -> Maybe FieldNumber {- ? -}
getFieldNum [] = Nothing
getFieldNum ws | length ws > 10 = Nothing
getFieldNum ws | otherwise = Just $ shiftR (decode ws) wtShift

combineWireHeader :: FieldNumber -> WireType -> [Word8]
combineWireHeader i wt = encode hdr
  where
    hdr ::FieldNumber
    hdr = shiftL i wtShift .|. (fromIntegral$  fromEnum wt)

getSizedBytes :: Int -> Get [Word8]
getSizedBytes i = BS.unpack <$> getByteString i


getVarInt :: Get [Word8]
getVarInt = getVarInt' []
  where
    getVarInt' xs = do
      w8 <- getWord8
      if w8 .&. contBit == contBit then
        getVarInt' (w8 : xs)
      else
        return $ reverse (w8 : xs)

putVarInt :: [Word8] -> Put
putVarInt = putByteString . BS.pack
