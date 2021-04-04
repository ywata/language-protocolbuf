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
  if n == txt then ident' else empty  
--  if T.toCaseFold n == T.toCaseFold txt then ident' else empty
reserved :: MonadParsec Char T.Text m => T.Text -> m T.Text
reserved txt = lexeme (reserved' txt)

isAsciiAlphabet :: Char -> Bool
isAsciiAlphabet ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')

asciiAlphabet :: MonadParsec Char T.Text m => m Char
asciiAlphabet = satisfy isAsciiAlphabet <?> "ascii alphabet"
asciiAlphaNum :: MonadParsec Char T.Text m => m Char
asciiAlphaNum = (asciiAlphabet <|> digitChar) <?> "ascii alphabet or digit"

ident' :: MonadParsec Char T.Text m => m T.Text
ident' = (\h t -> T.pack (h:t)) <$> (asciiAlphabet <|> char '_') <*> (many (asciiAlphaNum <|> char '_'))
        
        
ident :: MonadParsec Char T.Text m => m T.Text
ident = lexeme ident'

isStartDigit :: Char -> Bool
isStartDigit ch = ('1' <= ch && ch <= '9')
isDigit :: Char -> Bool
isDigit ch = ('0' <= ch && ch <= '9')

many1 :: MonadParsec Char T.Text m => m a -> m [a]
many1 p = (\h t -> h : t) <$> p <*> many p


intLit'', intLit', intLit :: MonadParsec Char T.Text m => m IntLit
intLit'' = try (Oct U <$> lexeme octalLit) 
      <|> try (Hex U <$> lexeme hexLit)
      <|>     (Dec U <$> lexeme decimalLit)
intLit' = mkP <$> (symbol "+" *> intLit'')
      <|> mkN <$> (symbol "-" *> intLit'')
      <|> intLit''
intLit = lexeme intLit'


decimalLit, decimalLit' :: MonadParsec Char T.Text m => m T.Text
decimalLit = try ((\h t -> T.pack (h : t)) <$> satisfy isStartDigit <*> many (satisfy isDigit))
             <|> try ((\h -> T.pack [h]) <$> char '0')
decimalLit' = decimalLit

octalLit, octalLit' :: MonadParsec Char T.Text m => m T.Text
octalLit' = T.pack  <$> (char '0' *> many1 octDigitChar)
octalLit = octalLit'


hexLit, hexLit' :: MonadParsec Char T.Text m => m T.Text
hexLit' = T.pack <$> ((char '0' >> (char 'x' <|> char 'X')) *> many1 hexDigitChar)
hexLit = hexLit'

floatLit'', floatLit', floatLit :: MonadParsec Char T.Text m => m FloatLit
floatLit'' = (reserved "inf" >> (pure $ Inf U))
           <|> (reserved "nan" >> (pure $ NaN U))
           <|> try (Fl1 U <$> (decimals <* char '.') <*> optional decimals <*> optional exponent)
           <|> try (Fl3 U <$> (char '.' *> decimals) <*> optional exponent)    
           <|> try (Fl2 U <$> decimals <*> exponent)

  where
    decimals:: MonadParsec Char T.Text m => m T.Text
    decimals = T.pack <$> many1 (satisfy isDigit)
    exponent :: MonadParsec Char T.Text m => m T.Text
    exponent = (\s d -> T.append (maybe "" (T.pack . (:[])) s) d) <$>
               ((optional (char' 'e')) *> (optional (char '+' <|> char '-'))) <*> decimals
floatLit' = mkP <$> (symbol "+" *> floatLit'')
      <|> mkN <$> (symbol "-" *> floatLit'')
      <|> floatLit''

floatLit = lexeme floatLit'


boolLit :: MonadParsec Char T.Text m => m T.Text
boolLit = (reserved "true") <|> (reserved "false")


charValue :: MonadParsec Char T.Text m => m CHAR
charValue = try (EscHex <$> hexEscape) <|> try (EscOct <$> octEscape) <|> try (EscChar <$> charEscape)
            <|> (CHAR <$> satisfy isValidChar) <|> (Z <$ zeroEscape)
  where
    isValidChar ch = ch /= '\0' && ch /= '\n' && ch /= '\\' && ch /= '\'' && ch /= '\"'
    hexEscape = (\a b -> [a, b]) <$> ((char '\\' >> char' 'x') >> hexDigitChar) <*> hexDigitChar
    octEscape = (\a b c -> [a, b, c]) <$> (char '\\' *> octDigitChar) <*> octDigitChar <*> octDigitChar
    zeroEscape = (char '\\' >> char '0') 
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
