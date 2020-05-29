module WildcardParser () where

import Data.List
import Data.Either

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

keychars = "[]+?*,"

stringP :: Parser (Parser String)
stringP = do
  ret <- many1$ noneOf keychars
  return$ string ret

starP :: Parser (Parser String)
starP = do
  char '*'
  return$ many anyChar

plusP :: Parser (Parser String)
plusP = do
  char '+'
  return$ many1 anyChar

oneP :: Parser (Parser String)
oneP = do
  char '?'
  return$ count 1$ anyChar

wildcardP :: Parser (Parser String)
wildcardP = do
  char '['
  ret <- negatedWildcard <|> normalWildcard
  char ']'
  return$ ret

  where
    negatedWildcard :: Parser (Parser String)
    negatedWildcard = do
      char '!'
      notAllowed <- many1$ molecule
      return$ count 1$ noneOf$ concat notAllowed

    normalWildcard :: Parser (Parser String)
    normalWildcard = do
      allowed <- many1$ molecule
      return$ count 1$ oneOf$ concat allowed

    range :: Parser String
    range = do
      a <- noneOf keychars
      char '-'
      b <- noneOf keychars
      return [a..b]

    atom :: Parser String
    atom = do
      ret <- count 1$ noneOf keychars
      return ret

    molecule :: Parser String
    molecule = do
      ret <- (try range) <|> atom
      return ret
      
combinatedP :: Parser (Parser String)
combinatedP = do
  dis_ret <- many$ plusP <|> starP <|> oneP <|> (try wildcardP) <|> stringP
  eof
  let parser = foldl1 (>>) (dis_ret)
  return$ add_eof parser

  where 
    add_eof :: Parser String -> Parser String
    add_eof p = do
      ret <- p
      eof
      return ret


--wildcardParser :: String -> Parser String
--wildcardParser s = 
