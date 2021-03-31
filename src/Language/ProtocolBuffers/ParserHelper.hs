{-# language FlexibleContexts, OverloadedStrings, GADTs #-}
module Language.ProtocolBuffers.ParserHelper where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Language.ProtocolBuffers.PrimTypes

spaceConsumer :: MonadParsec Char T.Text m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: MonadParsec Char T.Text m => m a -> m a
lexeme  = L.lexeme spaceConsumer
symbol :: MonadParsec Char T.Text m => T.Text -> m T.Text
symbol = L.symbol spaceConsumer

reserved' :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved' txt = do
  n <- lookAhead ident'
  if T.toCaseFold n == T.toCaseFold txt then ident' else empty
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved txt = lexeme (reserved' txt)

isAsciiAlphabet :: Char -> Bool
isAsciiAlphabet ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'z')

asciiAlphabet :: MonadParsec Char T.Text m => m Char
asciiAlphabet = satisfy isAsciiAlphabet <?> "ascii alphabet"
asciiAlphaNum :: MonadParsec Char T.Text m => m Char
asciiAlphaNum = (asciiAlphabet <|> digitChar) <?> "ascii alphabet or digit"

ident' :: MonadParsec Char T.Text m => m T.Text
ident' = (\h t -> T.pack (h:t))
        <$> asciiAlphabet
        <*> (many (asciiAlphaNum <|> char '_'))

isStartDigit :: Char -> Bool
isStartDigit ch = ('1' <= ch && ch <= '9')
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch && ch <= '9')


ident :: MonadParsec Char T.Text m => m T.Text
ident = lexeme ident'


many1 :: MonadParsec Char T.Text m => m a -> m [a]
many1 p = (\h t -> h : t) <$> p <*> many p




intLit', intLit :: MonadParsec Char T.Text m => m IntLit
intLit' = try (Oct <$> lexeme octalLit) <|> try (Hex <$> lexeme hexLit) <|> (Dec <$> lexeme decimalLit)
intLit = lexeme intLit'




decimalLit :: MonadParsec Char T.Text m => m T.Text
decimalLit = try ((\h t -> T.pack (h : t)) <$> satisfy isStartDigit <*> many (satisfy isDigit))
             <|> try ((\h -> T.pack [h]) <$> char '0')

octalLit :: MonadParsec Char T.Text m => m T.Text
octalLit = (\h t -> T.pack (h : t)) <$> char '0' <*> many1 octDigitChar

hexLit :: MonadParsec Char T.Text m => m T.Text
hexLit = T.pack <$> ((char '0' >> (char 'x' <|> char 'X')) *> many1 hexDigitChar)

floatLit', floatLit :: MonadParsec Char T.Text m => m FloatLit
floatLit' = 
  (reserved "inf" >> pure Inf)
           <|> (reserved "nan" >> pure NaN)
           <|> try (Fl1 <$> (decimals <* char '.') <*> optional decimals <*> optional exponent)
           <|> try (Fl3 <$> (char '.' *> decimals) <*> optional exponent)    
           <|> try (Fl2 <$> decimals <*> exponent)

  where
    decimals:: MonadParsec Char T.Text m => m T.Text
    decimals = T.pack <$> many1 (satisfy isDigit)
    exponent :: MonadParsec Char T.Text m => m T.Text
    exponent = (\s d -> T.append (maybe "" (T.pack . (:[])) s) d) <$>
               ((optional (char' 'e')) *> (optional (char '+' <|> char '-'))) <*> decimals
floatLit = lexeme floatLit'

boolLit :: MonadParsec Char T.Text m => m T.Text
boolLit = (reserved "true") <|> (reserved "false")


charValue :: MonadParsec Char T.Text m => m CHAR
charValue = try (EscHex <$> hexEscape) <|> try (EscOct <$> octEscape) <|> try (EscChar <$> charEscape)
            <|> (CHAR <$> satisfy isValidChar)
  where
    isValidChar ch = ch /= '\0' && ch /= '\n' && ch /= '\\' && ch /= '\'' && ch /= '\"'
    hexEscape = (\a b -> [a, b]) <$> ((char '\\' >> char' 'x') >> hexDigitChar) <*> hexDigitChar
    octEscape = (\a b c -> [a, b, c]) <$> (char '\\' *> octDigitChar) <*> octDigitChar <*> octDigitChar
    charEscape = (char '\\' *> (oneOf $ ['a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '\'', '\"']))

strLit', strLit :: MonadParsec Char T.Text m => m StringLit
strLit' = StringLit <$> quoted (many charValue)
strLit = lexeme strLit'

quoted :: MonadParsec Char T.Text m => m a -> m a
quoted p =　do
  q <- char '\'' <|> char '\"'
  r <- p
  char q
  return r


betweenBraces :: MonadParsec Char T.Text m => m a -> m a
betweenBraces = between (symbol "{") (symbol "}")
betweenSquares :: MonadParsec Char T.Text m => m a -> m a
betweenSquares = between (symbol "[") (symbol "]")
betweenParens :: MonadParsec Char T.Text m => m a -> m a
betweenParens = between (symbol "(") (symbol ")")
