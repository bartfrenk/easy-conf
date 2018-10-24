module Data.Config.Expr where

import           Control.Monad         (void)
import           Control.Monad.Catch   (Exception, MonadThrow, throwM)
import           Data.Char             (isAlphaNum)
import           Data.Functor.Identity
import           Data.Text             (Text)
import           Data.Typeable
import           Text.Parsec

type CharStream s = Stream s Identity Char

type Parser s = Parsec s ()

whitespace :: CharStream s => Parser s ()
whitespace = void $ many $ oneOf spaceChars

spaceChars :: String
spaceChars = " \n\t"

lexeme :: CharStream s => Parser s a -> Parser s a
lexeme p = p <* whitespace

symbol :: CharStream s => String -> Parser s String
symbol name = lexeme $ string name

quoted :: CharStream s => Parser s String
quoted = lexeme $ between (symbol "'") (symbol "'") $ many nonQuote
  where
    nonQuote = satisfy (/= '\'')

nonQuoted :: CharStream s => Parser s String
nonQuoted = lexeme $ many1 nonSpace
  where
    nonSpace = satisfy (not . flip elem spaceChars)

parenthesized :: CharStream s => Parser s a -> Parser s a
parenthesized = lexeme . between (symbol "(") (symbol ")")

tmFunc :: CharStream s => Parser s Expr
tmFunc = do
  void $ symbol "$"
  TmFunc <$> funcName <*> parenthesized funArg

funcName :: CharStream s => Parser s String
funcName = lexeme $ many1 letter

nat :: CharStream s => Parser s String
nat = lexeme $ many1 digit

tmLit :: CharStream s => Parser s Expr
tmLit = TmLit <$> (quoted <|> nat <|> nonQuoted)

funArg :: CharStream s => Parser s String
funArg = lexeme $ many1 argChar
  where
    argChar = satisfy (\a -> isAlphaNum a || a == '.' || a == '_')

expr :: CharStream s => Parser s Expr
expr = chainr1 term op
  where
    op = TmOr <$ lexeme (symbol "or")
    term = tmFunc <|> tmLit

data Expr
  = TmFunc String String
  | TmLit String
  | TmOr Expr Expr
  deriving (Eq, Show)

newtype ParseException = ParseException ParseError
  deriving (Typeable)

instance Show ParseException where
  show (ParseException err) = show err

instance Exception ParseException

parseExpr :: MonadThrow m => Text -> m Expr
parseExpr s =
  case parse (lexeme expr <* eof) "" s of
    Left err -> throwM $ ParseException err
    Right e -> return e
