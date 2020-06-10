module Lsql.Csv.Lang.BlockChain () where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import qualified Data.Text as T

import Lsql.Csv.Core.Symbols
import Lsql.Csv.Lang.Selector

import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.BlockOps

selectBP :: [String] -> Parser Block
selectBP symbol_list = do
  ret <- selectorP symbol_list
  return$ Select ret

ifBP :: [String] -> Parser Block
ifBP symbol_list = do
  skipMany spaces
  string "if"
  skipMany spaces
  ret <- aritmeticExprP symbol_list
  return$ If ret

sortBP :: [String] -> Parser Block
sortBP symbol_list = do
  skipMany spaces
  string "sort"
  skipMany spaces
  ret <- selectorP symbol_list
  return$ Sort ret

byBP :: [String] -> Parser Block
byBP symbol_list = do
  skipMany spaces
  string "by"
  skipMany spaces
  ret <- selectorP symbol_list
  return$ By ret

blockP :: [String] -> Parser Block
blockP symbol_list = 
  (try$ sortBP symbol_list) <|> 
  (try$ ifBP symbol_list) <|> 
  (try$ byBP symbol_list) <|> 
  (selectBP symbol_list)

parseBlocks :: [String] -> [String] -> [Block]
parseBlocks [] _ = []
parseBlocks (input : rest) symbol_list =
  case parse (blockP symbol_list) ("'" ++ input ++ "'" ++ " block")$ 
      T.pack input of
    Left err -> error$ show err
    Right parsed -> parsed : parseBlocks rest symbol_list

