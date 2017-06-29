{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import           Control.Monad         (void)
import           Control.Monad.Except
import           Data.Char             (isAlphaNum)
import           Data.Functor.Identity
import qualified Data.Text             as T
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
  where nonQuote = satisfy (/= '\'')

nonQuoted :: CharStream s => Parser s String
nonQuoted = lexeme $ many1 nonSpace
  where nonSpace = satisfy (not . flip elem spaceChars)

parenthesized :: CharStream s => Parser s a -> Parser s a
parenthesized = lexeme . between (symbol "(") (symbol ")")

-- TODO: specify allowed function names and arguments
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
  where argChar = satisfy (\a -> isAlphaNum a || a == '_')

expr :: CharStream s => Parser s Expr
expr = chainr1 term op
  where op = TmOr <$ lexeme (symbol "or")
        term = tmFunc <|> tmLit

data Expr
  = TmFunc String String
  | TmLit String
  | TmOr Expr Expr
  deriving (Eq, Show)

example :: T.Text
example = "$env(X) or www.bla.com"

parseExpr :: Monad m => T.Text -> ExceptT String m Expr
parseExpr s = case parse (lexeme expr <* eof) "" s of
  Left err -> throwError $ show err
  Right e  -> return e

