module Lsql.Csv.Lang.BlockSeparator (splitBlocks) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import Data.List
import qualified Data.Text as T

quote1 :: Parser String
quote1 = do
  char '"'
  ret <- many$ noneOf "\""
  char '"'
  return$ "\"" ++ ret ++ "\""

quote2 :: Parser String
quote2 = do
  char '\''
  ret <- many$ noneOf "'"
  char '\''
  return$ "'" ++ ret ++ "'"

quote3 :: Parser String
quote3 = do
  char '`'
  ret <- many$ noneOf "`"
  char '`'
  return$ "`" ++ ret ++ "`"

normal :: Parser String
normal = many1$ noneOf "\"`',"

block :: Parser String
block = do
  cret <- many1$ (quote1 <|> quote2 <|> quote3 <|> normal)
  return$ concat cret

nonTerminalBlock :: Parser String
nonTerminalBlock = do
  ret <- block
  char ','
  return ret

blocks :: Parser [String]
blocks = do
  rets <- many1$ try nonTerminalBlock
  last <- block
  return$ rets ++ [last]
  
splitBlocks :: String -> [String]
splitBlocks input =
  case parse blocks "block parser"$ (T.pack input) of
    Left err -> error $ show err
    Right val -> val

