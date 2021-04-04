{-# LANGUAGE OverloadedStrings #-}
{-# Language GADTs #-}
module Language.ProtocolBuffers.PrimTypes (
  strVal, intVal, floatVal, IntLit(..),
  FloatLit(..), CHAR(..), Sign(..), Signable(..), StringLit(..)) where

import Data.Text as T (pack, unpack, Text)

data Sign = P | N | U
  deriving (Show, Eq)

data IntLit where
  Dec :: Sign -> T.Text -> IntLit
  Hex :: Sign -> T.Text -> IntLit
  Oct :: Sign -> T.Text -> IntLit
  deriving (Show, Eq)


data FloatLit where
  Fl1 :: Sign -> T.Text -> Maybe T.Text -> Maybe T.Text -> FloatLit
  Fl2 :: Sign -> T.Text -> T.Text -> FloatLit
  Fl3 :: Sign -> T.Text -> Maybe T.Text-> FloatLit
  Inf :: Sign -> FloatLit
  NaN :: Sign -> FloatLit
  deriving (Show, Eq)

class Signable a where
  mkP :: a -> a
  mkN :: a -> a
  
instance Signable IntLit where
  mkP (Dec s v) = Dec P v
  mkP (Hex s v) = Hex P v
  mkP (Oct s v) = Oct P v
  mkN (Dec s v) = Dec N v
  mkN (Hex s v) = Hex N v
  mkN (Oct s v) = Oct N v    
instance Signable FloatLit where
  mkP (Fl1 s a b c) = Fl1 P a b c
  mkP (Fl2 s a b) = Fl2 P a b
  mkP (Fl3 s a b) = Fl3 P a b
  mkP (Inf s) = Inf P
  mkP (NaN s) = NaN P
  mkN (Fl1 s a b c) = Fl1 P a b c
  mkN (Fl2 s a b) = Fl2 P a b
  mkN (Fl3 s a b) = Fl3 P a b
  mkN (Inf s) = Inf P
  mkN (NaN s) = NaN P

intPart i = (read i :: Float)
decimalPart d = (read ("0." ++ d) :: Float)
exponentPart e = (read ("10e" ++ e) :: Float)



data CHAR = EscOct String | EscHex String | EscChar Char | CHAR Char | Z
  deriving(Show, Eq)

newtype StringLit = StringLit [CHAR]
  deriving(Show, Eq)
intVal :: IntLit -> Int
intVal (Dec s t) = read (T.unpack t)
intVal (Hex s t) = read ("0x" ++ (T.unpack t))
intVal (Oct s t) = read ("\\" ++ (T.unpack t))

floatVal :: FloatLit -> Float
floatVal (Fl1 s i d e) = (intPart (T.unpack i)
                          + maybe 0.0 (decimalPart . T.unpack) d) * (maybe 1.0 (exponentPart . T.unpack) e)
floatVal (Fl2 s i d) = intPart (T.unpack i) + (decimalPart . T.unpack) d
floatVal (Fl3 s d e) = (decimalPart . T.unpack) d * (maybe 1.0 (exponentPart . T.unpack) e)
floatVal (NaN s) = undefined
floatVal (Inf s) = undefined

strVal :: StringLit -> T.Text
strVal (StringLit cs) = T.pack $ concatMap charVal cs
  where
    charVal :: CHAR -> [Char]
    charVal (EscOct o) = "\\" ++ o
    charVal (EscHex x) = "0x" ++ x
    charVal (EscChar c) = "\\" ++ [c]
    charVal (CHAR c) = [c]
    charVal Z = "\\0"
