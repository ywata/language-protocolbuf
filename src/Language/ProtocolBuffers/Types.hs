{-# language GADTSyntax, PatternSynonyms #-}
-- | Elements in the Protocol Buffers syntax,
--   as defined in <https://developers.google.com/protocol-buffers/docs/reference/proto3-spec>
-- I modified Types defined in language-protobuf for my purpose.
--   - It supports proto2 and proto3 Ex. optional, required. Marked -- proto2.
--   - It also tries to model more accurate syntax.
--   - It generalizes types over integer, floating and strings so that precise definitions
--     can be captured.
module Language.ProtocolBuffers.Types where
import Prelude hiding (Enum)
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

data ProtocolBuffer i f s = ProtocolBuffer [Declaration i f s]
  deriving (Eq, Show)

type Version = T.Text


-- | Declarations, that is, anything which may
-- |  appear in the top-level.
data Declaration i f s where
  DSyntax  :: Version                      -> Declaration i f s
  DImport  :: ImportType -> FullIdentifier -> Declaration i f s
  DPackage :: FullIdentifier               -> Declaration i f s
  DOption  :: Option i f s                 -> Declaration i f s
  {- TopLevelDef -}
  DMessage :: Message i f s -> Declaration i f s -- Defined like this as it is defined both in message or toplevel
  DEnum    :: Enum i f s    -> Declaration i f s -- Defined like this as it is defined both in message or toplevel
  DService :: ServiceName -> [ServiceField i f s] -> Declaration i f s
  
  DEmpty   :: Declaration i f s
  deriving (Eq, Show)

data Option i f s where
  Option :: OptionName -> Constant i f s -> Option i f s
  deriving (Eq, Show)
-- | OptionName:
-- |  Regular : Option defined by google.
-- |  Custom  : Custom option.
data OptionName where
  Regular :: FullIdentifier -> OptionName
  Custom  :: FullIdentifier -> FullIdentifier -> OptionName
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

type TypeName = FullIdentifier
data FieldType
  = TInt32 | TInt64 | TUInt32 | TUInt64 | TSInt32 | TSInt64
  | TFixed32 | TFixed64 | TSFixed32 | TSFixed64
  | TDouble | TBool | TString | TBytes | TOther TypeName
  deriving (Eq, Show)

type FieldName = Identifier
type FieldNumber = Int


{-
enum = "enum" enumName enumBody
enumBody = "{" { option | enumField | emptyStatement } "}"
enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
enumValueOption = optionName "=" constant
-}
type EnumName = Identifier

data Enum i f s where
  Enum :: EnumName -> [EnumField i f s] -> Enum i f s
  deriving (Eq, Show)

data EnumField i f s where
  EnumField :: FieldName -> i -> [Option i f s] -> EnumField i f s
  EOption   :: Option i f s                     -> EnumField i f s
  EEmpty    ::                                     EnumField i f s
  deriving (Eq, Show)
{-
message = "message" messageName messageBody
messageBody = "{" { field | enum | message | option | oneof | mapField |
reserved | emptyStatement } "}"
-}

type OneOfName = Identifier
data OneOf i f s where
  OneOf :: OneOfName -> [OneOfField i f s] -> OneOf i f s
  deriving (Eq, Show)
data OneOfField i f s where
  OneOfField :: FieldType -> OneOfName -> i -> [Option i f s] -> OneOfField i f s
  deriving (Eq, Show)
type MessageName = Identifier
data Message i f s where
  Message :: MessageName -> [MessageField i f s] -> Message i f s
  deriving (Eq, Show)
type MapName = Identifier

data MessageField i f s where
  MField      :: Label -> FieldType -> FieldName -> i -> [Option i f s] -> MessageField i f s
  MEnum      :: Enum i f s                                             -> MessageField i f s
  MMessage   :: Message i f s                                          -> MessageField i f s
  MOption    :: Option i f s                                           -> MessageField i f s
  MOneOfDef   :: OneOf i f s                                            -> MessageField i f s
  MapDef     :: FieldType {-sans float-} -> FullIdentifier
             -> MapName -> i -> [Option i f s]                         -> MessageField i f s
  MReserved   :: ReservedValue i f s                                    -> MessageField i f s
  MEmpty ::                                                               MessageField i f s
  deriving (Eq, Show) 
data Label
  = Single
  | Repeated
  | Optional -- proto2
  | Required -- proto2
  deriving (Eq, Show)

--pattern Stream = Repeated

data MaxOrInt i = Max | I i
  deriving (Eq, Show)

data ReservedValue i f s where
  RRanges :: [i] -> Maybe (MaxOrInt i) ->  ReservedValue i f s
  RNames  :: [Identifier] -> ReservedValue i f s
  deriving (Eq, Show)


{-
service = "service" serviceName "{" { option | rpc | stream | emptyStatement } "}"
rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
messageType ")" (( "{" { option | emptyStatement } "}" ) | ";" )
stream = "stream" streamName "(" messageType "," messageType ")" (( "{"
{ option | emptyStatement } "}") | ";" )
-}
data Stream i f s where
  Stream :: Identifier -> [FullIdentifier] -> [Option i f s] -> Stream i f s
type ServiceName = Identifier
data ServiceField i f s where
  SOption :: Option i f s -> ServiceField i f s
  SRPC :: Identifier -> Bool  -> FullIdentifier -> Bool -> FullIdentifier
    -> [Option i f s] -> ServiceField i f s
  SEmpty :: ServiceField i f s
  deriving (Eq, Show)

data Service i f s where
  Service :: ServiceName -> [ServiceField i f s] -> Service i f s 
  deriving (Eq, Show)

data  Method i f s where
  Method :: Identifier
         -> Label -> FieldType
         -> Label -> FieldType
         -> [Option i f s] -> Method i f s
  
  deriving (Eq, Show)
