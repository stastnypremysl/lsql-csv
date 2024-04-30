{-|
This module contains the main parser of blocks other than the from block.
-}
module Lsql.Csv.Lang.BlockChain (parseBlocks) where

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
  skipMany space
  string "if"
  skipMany1 space
  ret <- arithmeticExprP symbol_list
  skipMany space
  return$ If ret

sortBP :: [String] -> Parser Block
sortBP symbol_list = do
  skipMany space
  string "sort"
  skipMany1 space
  ret <- selectorP symbol_list
  skipMany space
  return$ Sort ret

byBP :: [String] -> Parser Block
byBP symbol_list = do
  skipMany space
  string "by"
  skipMany1 space
  ret <- selectorP symbol_list
  skipMany space
  return$ By ret

blockP :: [String] -> Parser Block
blockP symbol_list = 
  (try$ sortBP symbol_list) <|> 
  (try$ byBP symbol_list) <|> 
  (try$ ifBP symbol_list) <|> 
  (selectBP symbol_list)

-- | A function for parsing blocks other than the from block.
parseBlocks :: [String] -- ^ The blocks to parse
            -> [String] -- ^ The list of all symbol names
            -> [Block] -- ^ The parsed blocks

parseBlocks [] _ = []
parseBlocks (input : rest) symbol_list =
  case parse (blockP symbol_list) ("'" ++ input ++ "'" ++ " block")$ 
      T.pack input of
    Left err -> error$ show err
    Right parsed -> parsed : parseBlocks rest symbol_list

