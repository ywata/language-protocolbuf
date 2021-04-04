{-# language FlexibleContexts, OverloadedStrings #-}
-- | Parser for the proto3 spec,
--   as defined in <https://developers.google.com/protocol-buffers/docs/reference/proto3-spec>.
module Language.ProtocolBuffers.Parser {-(
  -- * Parse and sort out a whole file
  parseProtoBufFile
, parseProtoBuf
  -- * Parse declarations
, parseProtoBufDeclarations
)-} where

import Prelude hiding (Enum)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (label, oneOf, option)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.ProtocolBuffers.ParserHelper
import Language.ProtocolBuffers.PrimTypes
import Language.ProtocolBuffers.Types

-- | Parse a whole file into a 'ProtoBuf' structure.
--   This function sorts together the different declarations.
parseProtocolBufferFile :: FilePath -> IO (Either (ParseErrorBundle T.Text Char) (ProtocolBuffer IntLit FloatLit StringLit))
parseProtocolBufferFile p = parseProtocolBuffer <$> T.readFile p
-- | Parse 'T.Text' into a 'ProtoBuf' structure.
--   This function sorts together the different declarations.
parseProtocolBuffer :: T.Text -> Either (ParseErrorBundle T.Text Char) (ProtocolBuffer IntLit FloatLit StringLit)
parseProtocolBuffer = parse wholeProtocolBuffer ""
-- | Parse all the declarations in a 'T.Text'.
parseProtoBufDeclarations :: T.Text -> Either (ParseErrorBundle T.Text Char) [Declaration IntLit FloatLit StringLit]
parseProtoBufDeclarations = parse (many declaration) ""


------------
identifier :: MonadParsec Char T.Text m => m T.Text
identifier = lexeme ident
fullIdentifier :: MonadParsec Char T.Text m => m FullIdentifier
fullIdentifier = lexeme $ sepBy1 identifier (char '.')

constant :: MonadParsec Char T.Text m => m (Constant IntLit FloatLit StringLit)
constant
  =   KBool True  <$  try (reserved "true")
  <|> KBool False <$  try (reserved "false")
  <|> KString     <$> try strLit
  <|> KObject     <$> try (betweenBraces (many objectField))
  <|> signedNumber  
  <|> KIdentifier <$> try fullIdentifier
  where
        objectField = (,) <$> identifier
                          <*> (   symbol ":" *> constant
                             <|> KObject <$> betweenBraces (sepBy1 objectField (symbol ",")) )

signedNumber :: MonadParsec Char T.Text m => m (Constant IntLit FloatLit StringLit)
signedNumber =     KFloat       <$> try floatLit
               <|> KInt         <$> try intLit               
number :: MonadParsec Char T.Text m => m (Constant IntLit FloatLit StringLit)
number = KFloat <$> try floatLit
     <|> KInt   <$> try intLit

optionName :: MonadParsec Char T.Text m => m OptionName
optionName = Regular <$> fullIdentifier
             <|>
             Custom <$> betweenParens fullIdentifier <*> many (char '.' >> identifier)
innerOption :: MonadParsec Char T.Text m => m (Option IntLit FloatLit StringLit)
innerOption
  = Option <$> optionName
           <*  symbol "="
           <*> constant
option :: MonadParsec Char T.Text m => m (Option IntLit FloatLit StringLit)
option
  = Option <$> optionName
           <*  symbol "="
           <*> constant 

options :: MonadParsec Char T.Text m => m [Option IntLit FloatLit StringLit]
options = sepBy option (symbol ",")

emptyStatement :: MonadParsec Char T.Text m => m ()
emptyStatement = lexeme (char ';') >> return ()

wholeProtocolBuffer :: MonadParsec Char T.Text m => m (ProtocolBuffer IntLit FloatLit StringLit)
wholeProtocolBuffer = ProtocolBuffer <$ spaceConsumer <*> many declaration

declaration :: MonadParsec Char T.Text m => m (Declaration IntLit FloatLit StringLit)
declaration = spaceConsumer *> declaration'
  where 
    declaration'
      =   DSyntax  <$ reserved "syntax" <* symbol "=" <*> (strVal <$> strLit)
      <|> DImport  <$ reserved "import"
                   <*> (Weak <$ reserved "weak" <|> Public <$ reserved "public" <|> pure Normal)
                   <*> (T.splitOn "." . strVal <$> strLit)
      <|> DPackage <$ reserved "package" <*> fullIdentifier
      <|> DOption  <$> (reserved "option" *> option)
      <|> DMessage <$> (reserved "message" *> messageParser)
      <|> DEnum    <$> (reserved "enum" *> enumParser)
      <|> DService <$> (reserved "service" *> identifier) <*> many serviceField
      <|> DEmpty   <$ symbol ";"

messageParser :: MonadParsec Char T.Text m => m (Message IntLit FloatLit StringLit)
messageParser = Message <$> identifier <*> betweenBraces (many messageField)

label :: MonadParsec Char T.Text m => m Label
label = Repeated <$ reserved "repeated"
        <|> Optional <$ reserved "optional"
        <|> Required <$ reserved "required"


groupParser :: MonadParsec Char T.Text m => m (Group IntLit FloatLit StringLit)
groupParser =
  try (Group <$> label       <*> (reserved "group" *> identifier) <*> (symbol "=" *> intLit) <*> betweenBraces (many messageField))
  <|> (Group <$> pure Single <*> (reserved "group" <* identifier) <*> (symbol "=" *> intLit) <*> betweenBraces (many messageField))

    
refType :: MonadParsec Char T.Text m => m RefType
refType = Dot <$> ((symbol ".") *> fullIdentifier)
       <|> RefType <$> fullIdentifier

messageField :: MonadParsec Char T.Text m => m (MessageField IntLit FloatLit StringLit)
messageField =
      try(MEnum        <$> (reserved "enum" *> enumParser))
  <|> try(MMessage     <$> (reserved "message" *> messageParser))
  <|> try(MOption      <$> (reserved "option" *> option))
  <|> try(MOneOf       <$> (reserved "oneof" *> oneOf))
  <|> try(MGroup       <$> groupParser)
  <|> try(MMapField    <$> (reserved "map" *> symbol "<" *> fieldType <* symbol "," )
                       <*> (fullIdentifier <* symbol ">")
                       <*> identifier
                       <*> (symbol "=" *> intLit)
                       <*> (betweenSquares options <|> pure []) <* symbol ";")
  <|> try (MExtend     <$> (reserved "extend" *> extendParser))
  <|> try (MReserved   <$> reservedValue)
  <|> try (MExtensions <$> extensionValue)
  <|> try (MField      <$> label   <*> fieldType <*> identifier <*> (symbol "=" *> intLit))
                       <*> ((betweenSquares options <|> pure[])<*  symbol ";")
  <|> try (MField      <$> pure Single <*> try fieldType <*> identifier <*> (symbol "=" *> intLit)
                       <*> ((betweenSquares options ) <|> pure[]) <*  symbol ";")
  
  <|> MEmpty           <$  symbol ";"

oneOf :: MonadParsec Char T.Text m => m (OneOf IntLit FloatLit StringLit)
oneOf = OneOf <$> identifier <*> betweenBraces (many oneOfField)

oneOfField :: MonadParsec Char T.Text m => m (OneOfField IntLit FloatLit StringLit)
oneOfField =
  OOption <$> (reserved "option" *> option)
  <|> OneOfField  <$> fieldType <*> identifier <*> (symbol "=" *> intLit) <*> (betweenSquares options <|> pure [])
  <|> OEmpty  <$ symbol ";"

enumParser :: MonadParsec Char T.Text m => m (Enum IntLit FloatLit StringLit)
enumParser = Enum <$> identifier <*> (betweenBraces (many enumField))


enumField :: MonadParsec Char T.Text m => m (EnumField IntLit FloatLit StringLit)
enumField = try (EnumField <$> identifier <* symbol "=" <*> intLit <*> betweenSquares options <* symbol ";")
            <|> try (EnumField <$> identifier <* symbol "=" <*> intLit <*> pure [] <* symbol ";")
            <|> EOption <$> (reserved "option" *> option) <* symbol ";"
            <|> EEmpty <$ symbol ";"

extendParser :: MonadParsec Char T.Text m => m (Extend IntLit FloatLit StringLit)
extendParser = Extend <$> refType <*> (betweenBraces (many extendField))
extendField :: MonadParsec Char T.Text m => m (MessageField IntLit FloatLit StringLit)
extendField =  messageField {-<|> group -} 

          
{-
reserved = "reserved" ( ranges | fieldNames ) ";"
ranges = range { "," range }
range =  intLit [ "to" ( intLit | "max" ) ]
fieldNames = fieldName { "," fieldName }

data Range i where
  Solo   :: i -> Range i
  FromTo :: i -> i -> Range i
  ToMax  :: i -> Range i
  deriving(Show, Eq)
-}
fieldNames :: MonadParsec Char T.Text m => m [FullIdentifier]
fieldNames = sepBy1 ids (symbol ",")
  where
    ids = T.splitOn "." . strVal <$> strLit 
ranges :: MonadParsec Char T.Text m => m [Range IntLit]
ranges = sepBy1 (try fromTo <|> try toMax <|> solo) (symbol ",")
  where
    solo = Solo <$> intLit
    fromTo = FromTo <$> intLit <* reserved "to" <*> intLit
    toMax = ToMax <$> intLit <* reserved "to" <* reserved "max"

reservedValue :: MonadParsec Char T.Text m => m (Reserved IntLit)
reservedValue = reserved "reserved" *> (Ranges <$> ranges <|> FieldNames <$> fieldNames)


extensionValue ::  MonadParsec Char T.Text m => m (Extension IntLit)
extensionValue = Extension <$> (reserved "extensions" *> ranges)

{-
service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
-}
serviceField :: MonadParsec Char T.Text m => m (ServiceField IntLit FloatLit StringLit)
serviceField =
  SOption <$> (reserved "option" *> innerOption) <* symbol ";"
   <|> SRpc <$> (reserved "rpc" *> identifier) 
            <*> ((char '(') *> ((reserved "stream") >> pure Stream <|> pure Unary))
            <*> (refType <* char ')' <* reserved "returns")
            <*> ((char '(') *> ((reserved "stream") >> pure Stream <|> pure Unary))
            <*> refType <* char ')'
            <*> (many ((reserved "option" *> innerOption) <* symbol ";")  <|> (symbol ";" >> pure []))
  <|> SEmpty <$ symbol ";"



fieldType :: MonadParsec Char T.Text m => m FieldType
fieldType
  =   TInt32    <$ try (reserved "int32")
  <|> TInt64    <$ try (reserved "int64")
  <|> TUInt32   <$ try (reserved "uint32")
  <|> TUInt64   <$ try (reserved "uint64")
  <|> TSInt32   <$ try (reserved "sint32")
  <|> TSInt64   <$ try (reserved "sint64")
  <|> TFixed32  <$ try (reserved "fixed32")
  <|> TFixed64  <$ try (reserved "fixed64")
  <|> TSFixed32 <$ try (reserved "sfixed32")
  <|> TSFixed64 <$ try (reserved "sfixed64")
  <|> TDouble   <$ try (reserved "double")
  <|> TBool     <$ try (reserved "bool")
  <|> TString   <$ try (reserved "string")
  <|> TBytes    <$ try (reserved "bytes")
  <|> TOther    <$> refType

------------
