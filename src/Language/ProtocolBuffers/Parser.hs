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

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
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
  =   KFloat      <$> try floatLit     -- temporary ignore +- sign
  <|> KInt        <$> try intLit    -- temporary ignore +- sign
  <|> KString     <$> strLit
  <|> KBool True  <$  try (reserved "true")
  <|> KBool False <$  try (reserved "false")
  <|> KObject     <$> betweenBraces (many objectField)  
  <|> KIdentifier <$> fullIdentifier
  where
        objectField = (,) <$> identifier
                          <*> (   symbol ":" *> constant
                              <|> KObject <$> betweenBraces (many objectField) )
optionName :: MonadParsec Char T.Text m => m OptionName
--optionName = (++) <$> ((: []) <$> identifier <|> betweenParens fullIdentifier)
--                  <*> many (char '.' >> identifier)
optionName = Regular <$> fullIdentifier
             <|>
             Custom <$> betweenParens fullIdentifier <*> many (char '.' >> identifier)

topOption :: MonadParsec Char T.Text m => m (Option IntLit FloatLit StringLit)
topOption
  = Option <$  reserved "option"
           <*> optionName
           <*  symbol "="
           <*> constant
           <*  symbol ";"
innerOption :: MonadParsec Char T.Text m => m (Option IntLit FloatLit StringLit)
innerOption
  = Option <$> optionName
           <*  symbol "="
           <*> constant

emptyStatement :: MonadParsec Char T.Text m => m ()
emptyStatement = lexeme (char ';') >> return ()

wholeProtocolBuffer :: MonadParsec Char T.Text m => m (ProtocolBuffer IntLit FloatLit StringLit)
wholeProtocolBuffer = ProtocolBuffer <$ spaceConsumer <*> many declaration

declaration :: MonadParsec Char T.Text m => m (Declaration IntLit FloatLit StringLit)
declaration = spaceConsumer *> declaration'
  where 
    declaration'
      =   DSyntax  <$ reserved "syntax" <* symbol "=" <*> (strVal <$> strLit) <* symbol ";"
      <|> DImport  <$ reserved "import"
                   <*> (Weak <$ reserved "weak" <|> Public <$ reserved "public" <|> pure Normal)
                   <*> (strVal <$> strLit) <* symbol ";"
      <|> DPackage <$ reserved "package" <*> fullIdentifier <* symbol ";"
      <|> DOption  <$> topOption
      <|> DType    <$> typeDeclaration
      <|> DService <$> serviceDeclaration

typeDeclaration :: MonadParsec Char T.Text m => m (TypeDeclaration IntLit FloatLit StringLit)
typeDeclaration
  =   buildEnum <$  reserved "enum"
                <*> lexeme identifier
                <*> betweenBraces (many enumThing)
  <|> buildMessage <$  reserved "message"
                   <*> identifier
                   <*> betweenBraces (many msgThing)
  <|> DEmptyTyDecl <$ emptyStatement
  where
    buildEnum :: T.Text -> [EnumThing i f s] -> TypeDeclaration i f s
    buildEnum name things
      = DEnum name [o | EnumThingOption o <- things]
                   [f | EnumThingField  f <- things]
    buildMessage :: T.Text -> [MsgThing i f s] -> TypeDeclaration i f s
    buildMessage name things
      = DMessage name [o | MsgThingOption   o <- things]
                      [r | MsgThingReserved r <- things]
                      [f | MsgThingField    f <- things]
                      [i | MsgThingInner    i <- things]

data EnumThing i f s
  = EnumThingOption (Option i f s) | EnumThingField (EnumField i f s)
  deriving(Show, Eq)
enumThing :: MonadParsec Char T.Text m => m (EnumThing IntLit FloatLit StringLit)
enumThing
  =   EnumThingOption <$> topOption
  <|> EnumThingField  <$> enumField
  where
    enumField :: MonadParsec Char T.Text m => m (EnumField IntLit FloatLit StringLit)
    enumField = EnumField <$> identifier
                          <*  symbol "="
                          <*> lexeme intLit
                          <*> (betweenSquares (sepBy innerOption (symbol ",")) <|> pure [])
--                          <*> (pure [])
                          <*  symbol ";"

data MsgThing i f s
  = MsgThingOption   (Option i f s)
  | MsgThingReserved (Reserved i f s)
  | MsgThingField    (MessageField i f s)
  | MsgThingInner    (TypeDeclaration i f s)
msgThing :: MonadParsec Char T.Text m => m (MsgThing IntLit FloatLit StringLit)
msgThing
  =   MsgThingOption   <$> topOption
  <|> MsgThingReserved <$> reservedOption
  <|> MsgThingInner    <$> typeDeclaration
  <|> MsgThingField    <$> msgField

reservedOption :: MonadParsec Char T.Text m => m (Reserved IntLit FloatLit StringLit)
reservedOption
  = id <$  reserved "reserved"
       <*> lexeme (sepBy1 reservedValue (symbol ","))
       <*  symbol ";"
  where
    reservedValue = ranges <|> fieldNames
    ranges = RRanges <$> lexeme (sepBy1 intLit (symbol ","))
             <*> optional (reserved "to" >> ((I <$> intLit) <|> (Max <$ reserved "max")))
    fieldNames = RNames <$> quoted (sepBy1 ident (symbol ","))
    
msgField :: MonadParsec Char T.Text m => m (MessageField IntLit FloatLit StringLit)
msgField
  =   OneOfField  <$  reserved "oneof"
                  <*> identifier
                  <*> lexeme (betweenBraces (some msgField))
                  <*> lexeme (betweenSquares (sepBy innerOption (symbol ","))  <|> pure []) -- Added.
  <|> MapField    <$  reserved "map"
                  <*  symbol "<"
                  <*> fieldType
                  <*  symbol ","
                  <*> fieldType
                  <*  symbol ">"
                  <*> identifier
                  <*  symbol "="
                  <*> intLit
                  <*> lexeme (betweenSquares (sepBy innerOption (symbol ",")) <|> pure [])
                  <*  symbol ";"
  <|> NormalField <$> (Repeated <$ reserved "repeated"
                       <|> Optional <$ reserved "optional" -- proto2
                       <|> Required <$ reserved "required" -- proto2
                       <|> pure Single)
                  <*> fieldType
                  <*> identifier
                  <*  symbol "="
                  <*> lexeme intLit
                  <*> lexeme (betweenSquares (sepBy innerOption (symbol ","))  <|> pure []) 
                  <*  symbol ";"


serviceDeclaration :: MonadParsec Char T.Text m => m (ServiceDeclaration IntLit FloatLit StringLit)
serviceDeclaration
  = buildService <$  reserved "service"
                 <*> identifier
                 <*> betweenBraces (many serviceThing)
  where
    buildService :: T.Text -> [ServiceThing i f s] -> ServiceDeclaration i f s
    buildService name things
      = Service name [o | ServiceThingOption o <- things]
                     [m | ServiceThingMethod m <- things]

data ServiceThing i f s
  = ServiceThingOption (Option i f s) | ServiceThingMethod (Method i f s)
serviceThing :: MonadParsec Char T.Text m => m (ServiceThing IntLit FloatLit StringLit)
serviceThing
  =   ServiceThingOption <$> topOption
  <|> ServiceThingMethod <$> method
  where
    method = Method <$  reserved "rpc"
                    <*> identifier
                    <*  symbol "("
                    <*> (Stream <$ reserved "stream" <|> pure Single)
                    <*> fieldType
                    <*  symbol ")"
                    <*  reserved "returns"
                    <*  symbol "("
                    <*> (Stream <$ reserved "stream" <|> pure Single)
                    <*> fieldType
                    <*  symbol ")"
                    <*> (betweenBraces (many topOption) <|> [] <$ symbol ";")

fieldType :: MonadParsec Char T.Text m => m FieldType
fieldType
  =   TInt32 <$ reserved "int32"
  <|> TInt64 <$ reserved "int64"
  <|> TUInt32 <$ reserved "uint32"
  <|> TUInt64 <$ reserved "uint64"
  <|> TSInt32 <$ reserved "sint32"
  <|> TSInt64 <$ reserved "sint64"
  <|> TFixed32 <$ reserved "fixed32"
  <|> TFixed64 <$ reserved "fixed64"
  <|> TSFixed32 <$ reserved "sfixed32"
  <|> TSFixed64 <$ reserved "sfixed64"
  <|> TDouble <$ reserved "double"
  <|> TBool <$ reserved "bool"
  <|> TString <$ reserved "string"
  <|> TBytes <$ reserved "bytes"
  <|> TOther <$> fullIdentifier

------------
