{-# language GADTSyntax, PatternSynonyms #-}
-- | Elements in the Protocol Buffers syntax,
--   as defined in <https://developers.google.com/protocol-buffers/docs/reference/proto3-spec>
-- I modified Types defined in language-protobuf for my purpose.
--   - It supports proto2 and proto3 Ex. optional, required. Marked -- proto2.
--   - It also tries to model more accurate syntax.
--   - It generalizes types over integer, floating and strings so that precise definitions
--     can be captured.
module Language.ProtocolBuffers.Types where

{-
Some options are file-level options, meaning they should be written at the top-level scope,
not inside any message, enum, or service definition. (Defined in ProtoBuf)

Some options are message-level options, meaning they should be written inside message definitions.
(Defined in DMessage and DEnum in TypeDeclaration)

Some options are field-level options, meaning they should be written inside field definitions.
(Defined EnumField and MessageField)

Options can also be written
  on enum types, -- Defined in DEnum
  enum values,   --  ???
  oneof fields,  -- OneOfField in MessageField
  service types, -- Service in ServiceDeclaration
  and service methods; however, no useful options currently exist for any of these.
-}

import qualified Data.Text as T
import Language.ProtocolBuffers.PrimTypes

type Identifier = T.Text
type FullIdentifier = [Identifier]


-- | Whole definition, in which declarations are
--   sorted out by their form.
data ProtoBuf i f s
  = ProtoBuf { syntax   :: Maybe T.Text
             , package  :: Maybe FullIdentifier
             , imports  :: [(ImportType, T.Text)]
             , options  :: [Option i f s]
             , types    :: [TypeDeclaration i f s]
             , services :: [ServiceDeclaration i f s]
             }
  deriving (Eq, Show)

data ProtocolBuffer i f s = ProtocolBuffer [Declaration i f s]
  deriving (Eq, Show)

declsToProtoBuf :: [Declaration i f s] -> ProtoBuf i f s
declsToProtoBuf things
  = ProtoBuf (safeHead [s | DSyntax s <- things])
             (safeHead [s | DPackage s <- things])
             [(i,t) | DImport i t <- things]
             [o | DOption o <- things]
             [t | DType t <- things]
             [s | DService s <- things]

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | Declarations, that is, anything which may
--   appear in the top-level.
data Declaration i f s where
  DSyntax  :: T.Text                    -> Declaration i f s
  DImport  :: ImportType -> T.Text      -> Declaration i f s
  DPackage :: FullIdentifier            -> Declaration i f s
  DOption  :: Option i f s              -> Declaration i f s
  DType    :: TypeDeclaration i f s     -> Declaration i f s
  DService :: ServiceDeclaration i f s  -> Declaration i f s
  deriving (Eq, Show)


data OptionName where
  Regular :: FullIdentifier -> OptionName
  Custom :: FullIdentifier -> FullIdentifier -> OptionName
  deriving (Eq, Show)

data Option i f s where
  Option :: OptionName -> Constant i f s -> Option i f s
  deriving (Eq, Show)

data TypeDeclaration  i f s where
  DEnum    :: Identifier -> [Option i f s] -> [EnumField i f s]
           -> TypeDeclaration i f s
  DMessage :: Identifier -> [Option i f s] -> [Reserved i f s]
           -> [MessageField i f s] -> [TypeDeclaration i f s]
           -> TypeDeclaration i f s
  DEmptyTyDecl   :: TypeDeclaration i f s
  deriving (Eq, Show)

data ServiceDeclaration i f s where
  Service :: Identifier -> [Option i f s] -> [Method i f s] -> ServiceDeclaration i f s
  deriving (Eq, Show)

data Method i f s where
  Method :: Identifier
         -> Label -> FieldType
         -> Label -> FieldType
         -> [Option i f s] -> Method i f s
  deriving (Eq, Show)

data ImportType
  = Normal | Weak | Public
  deriving (Eq, Show)

data Constant i f s where
  KIdentifier :: FullIdentifier       -> Constant i f s
  KInt        :: i                    -> Constant i f s
  KFloat      :: f                    -> Constant i f s
  KString     :: s                    -> Constant i f s
  KBool       :: Bool                 -> Constant i f s
  KObject     :: [(T.Text, Constant i f s)] -> Constant i f s
  deriving (Eq, Show)

data EnumField i f s where
  EnumField :: FieldName -> i  -> [Option i f s] -> EnumField i f s
  deriving (Eq, Show)

type TypeName = FullIdentifier
data FieldType
  = TInt32 | TInt64 | TUInt32 | TUInt64 | TSInt32 | TSInt64
  | TFixed32 | TFixed64 | TSFixed32 | TSFixed64
  | TDouble | TBool | TString | TBytes | TOther TypeName
  deriving (Eq, Show)

type FieldName = Identifier
type FieldNumber = Int

data MessageField i f s where
  NormalField :: Label -> FieldType
              -> FieldName -> i
              -> [Option i f s] -> MessageField i f s
  OneOfField  :: FieldName -> [MessageField i f s]
              -> [Option i f s] -> MessageField i f s -- Added Options but not used.
  MapField    :: FieldType -> FieldType
              -> FieldName -> i
              -> [Option i f s] -> MessageField i f s
  EmptyMessageField :: MessageField i f s
  deriving (Eq, Show)

data Label
  = Single
  | Repeated
  | Optional -- proto2
  | Required -- proto2
  deriving (Eq, Show)

pattern Stream = Repeated

type Reserved i f s = [ReservedValue i f s]
data MaxOrInt i = Max | I i
  deriving (Eq, Show)

data ReservedValue i f s where
  RRanges :: [i] -> Maybe (MaxOrInt i) ->  ReservedValue i f s
  RNames  :: [Identifier] -> ReservedValue i f s
  deriving (Eq, Show)
