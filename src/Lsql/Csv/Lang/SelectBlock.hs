module Lsql.Csv.Lang.SelectBlock(parseSelectBlock) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import qualified Data.Text as T

import Lsql.Csv.Lang.Selector
import Lsql.Csv.Core.Functions

parseSelectBlock :: String -> [String] -> [Arg]
parseSelectBlock input symbol_list =
  case parse (selectorP symbol_list) "select block"$ T.pack input of
    Left err -> error$ show err
    Right parsed -> parsed

