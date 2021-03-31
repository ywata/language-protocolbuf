{-# Language MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.ProtocolBuffers(
  parseProtocolBufferFile
  , parseProtocolBuffer
  , parseProtoBufDeclarations

    --
  , FloatLit(..)
  , IntLit(..)
  , StringLit(..)
  -- defined in Types.hs
  , Identifier
  , FullIdentifier
  , ProtoBuf(..)
  , Declaration(..)
  , Option(..)
  , TypeDeclaration(..)
  , ServiceDeclaration(..)
  , Method(..)
  , ImportType(..)
  , Constant(..)
  , EnumField(..)
  , TypeName
  , FieldType(..)
  , FieldName
  , FieldNumber
  , MessageField(..)
  , Label(..)
  , Reserved(..)
  , MaxOrInt(..)
  , ReservedValue(..)
  )

where

import qualified Data.Text as T

import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types
import Language.ProtocolBuffers.Parser

class Normalizable a b where
  normalize :: a -> b

instance Normalizable IntLit Int where
  normalize = intVal
instance Normalizable FloatLit Float where
  normalize = floatVal
instance Normalizable StringLit T.Text where
  normalize = strVal
  
