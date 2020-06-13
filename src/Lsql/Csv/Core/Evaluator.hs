module Lsql.Csv.Core.Evaluator (evaluate) where

import Lsql.Csv.Core.BlockOps
import Lsql.Csv.Core.Symbols
import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import Data.List

evaluate :: SymbolMap -> [Block] -> [Printable]
evaluate symbol_map blocks =

  map (eval cross_symbol_map) selects

  where 
    selects :: [Arg]
    selects = concat $ getSelects blocks
  
    cross_table :: Table
    cross_table = foldl1 crossJoinTable$ getTables symbol_map

    cross_symbol_map :: SymbolMap
    cross_symbol_map = getSymbolMap [cross_table]
