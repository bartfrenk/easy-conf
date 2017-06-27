{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import           Control.Monad         (void)
import           Control.Monad.Except
import           Data.Char             (isDigit, isUpper)
import qualified Data.Text            as T
import           Data.Functor.Identity
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

tmGetEnv :: CharStream s => Parser s Expr
tmGetEnv = do
  void $ symbol "getEnv"
  TmGetEnv <$> parenthesized envVar

nat :: CharStream s => Parser s String
nat = lexeme $ many1 digit

tmLit :: CharStream s => Parser s Expr
tmLit = TmLit <$> (quoted <|> nat <|> nonQuoted)

envVar :: CharStream s => Parser s String
envVar = lexeme $ (:) <$> firstChar <*> many nonFirstChar
  where firstChar = satisfy (\a -> isUpper a || a == '_')
        nonFirstChar = satisfy (\a -> isUpper a || isDigit a || a == '_')

expr :: CharStream s => Parser s Expr
expr = chainr1 term op
  where op = TmOr <$ lexeme (symbol "or")
        term = tmGetEnv <|> tmLit

data Expr
  = TmGetEnv String
  | TmLit String
  | TmOr Expr Expr
  deriving (Eq, Show)

example :: String
example = "getEnv(X) or www.bla.com"

parseExpr :: Monad m => T.Text -> ExceptT String m Expr
parseExpr s = case parse (lexeme expr <* eof) "" s of
  Left err -> throwError $ show err
  Right e -> return e