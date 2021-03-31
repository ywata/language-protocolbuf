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
                   <*> (T.splitOn "." . strVal <$> strLit) <* symbol ";"
      <|> DPackage <$ reserved "package" <*> fullIdentifier <* symbol ";"
      <|> DOption  <$> topOption
      <|> DMessage <$> messageParser
      <|> DEnum    <$> enumParser
      <|> DService <$> identifier <*> many serviceField
      <|> DEmpty   <$ symbol ";"

messageParser :: MonadParsec Char T.Text m => m (Message IntLit FloatLit StringLit)
messageParser = Message <$> identifier <*> many messageField
enumParser :: MonadParsec Char T.Text m => m (Enum IntLit FloatLit StringLit)
enumParser = Enum <$> identifier <*> many enumField


messageField :: MonadParsec Char T.Text m => m (MessageField IntLit FloatLit StringLit)
messageField = undefined
enumField :: MonadParsec Char T.Text m => m (EnumField IntLit FloatLit StringLit)
enumField = undefined
serviceField :: MonadParsec Char T.Text m => m (ServiceField IntLit FloatLit StringLit)
serviceField = undefined
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
