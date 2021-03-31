{-# LANGUAGE OverloadedStrings #-}
{-# Language GADTs #-}
module Language.ProtocolBuffers.PrimTypes (
  strVal, intVal, floatVal, IntLit(..),
  FloatLit(..), CHAR(..), StringLit(..)) where

import Data.Text as T (pack, unpack, Text)

data IntLit where
  Dec :: T.Text -> IntLit
  Hex :: T.Text -> IntLit
  Oct :: T.Text -> IntLit
  deriving (Show, Eq)



data FloatLit where
  Fl1 :: T.Text -> Maybe T.Text -> Maybe T.Text -> FloatLit
  Fl2 :: T.Text -> T.Text -> FloatLit
  Fl3 :: T.Text -> Maybe T.Text-> FloatLit
  Inf :: FloatLit
  NaN :: FloatLit
  deriving (Show, Eq)


intPart i = (read i :: Float)
decimalPart d = (read ("0." ++ d) :: Float)
exponentPart e = (read ("10e" ++ e) :: Float)



data CHAR = EscOct String | EscHex String | EscChar Char | CHAR Char
  deriving(Show, Eq)

newtype StringLit = StringLit [CHAR]
  deriving(Show, Eq)
intVal :: IntLit -> Int
intVal (Dec t) = read (T.unpack t)
intVal (Hex t) = read ("0x" ++ (T.unpack t))
intVal (Oct t) = read ("\\" ++ (T.unpack t))

floatVal :: FloatLit -> Float
floatVal (Fl1 i d e) = (intPart (T.unpack i)
                          + maybe 0.0 (decimalPart . T.unpack) d) * (maybe 1.0 (exponentPart . T.unpack) e)
floatVal (Fl2 i d) = intPart (T.unpack i) + (decimalPart . T.unpack) d
floatVal (Fl3 d e) = (decimalPart . T.unpack) d * (maybe 1.0 (exponentPart . T.unpack) e)
floatVal NaN = undefined
floatVal Inf = undefined

strVal :: StringLit -> T.Text
strVal (StringLit cs) = T.pack $ concatMap charVal cs
  where
    charVal :: CHAR -> [Char]
    charVal (EscOct o) = "\\" ++ o
    charVal (EscHex x) = "0x" ++ x
    charVal (EscChar c) = "\\" ++ [c]
    charVal (CHAR c) = [c]
