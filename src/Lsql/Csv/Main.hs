{-|
This module contains the starting point for an lsql-csv evaluation.
-}
module Lsql.Csv.Main (run) where

import Lsql.Csv.Lang.Args
import Lsql.Csv.Lang.BlockSeparator
import Lsql.Csv.Lang.From.Block
import Lsql.Csv.Lang.BlockChain

import Lsql.Csv.Core.BlockOps
import Lsql.Csv.Core.Symbols
import Lsql.Csv.Core.Evaluator

import Lsql.Csv.Utils.CsvGenerator


-- | The starting point for a lsql-csv evaluation. Returns a `String` with output CSV.
run :: Program -> IO String
run prog = do
  symbol_map <- getFromSymbols prog from_block

  let blocks = parseBlocks rest_blocks$ symbolList symbol_map
  let evaluated = evaluate symbol_map blocks
  
  return$ csvGenerate sep sec_sep evaluated

  where
    Program command sep sec_sep _ = prog
    
    blocks_split :: [String]
    blocks_split = splitBlocks command

    from_block : rest_blocks = blocks_split


