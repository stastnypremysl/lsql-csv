module Lsql.Csv.Core.Evaluator (evaluate) where

import Lsql.Csv.Core.BlockOps
import Lsql.Csv.Core.Symbols
import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import Data.List

evaluate :: SymbolMap -> [Block] -> [Printable]
evaluate symbol_map blocks =
  map (eval symbol_map) selects

  where 
    selects :: [Arg]
    selects = concat $ getSelects blocks
  

