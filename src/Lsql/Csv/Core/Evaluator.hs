module Lsql.Csv.Core.Evaluator (evaluate) where

import Lsql.Csv.Core.BlockOps
import Lsql.Csv.Core.Symbols
import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import Data.List


filterCols :: [Printable] -> [Column]
filterCols [] = []
filterCols (ValueP val : rest) = filterCols rest
filterCols (ColumnP col : rest) = col : filterCols rest


evaluate :: SymbolMap -> [Block] -> [Printable]
evaluate symbol_map blocks =

  map (eval sorted_symbol_map) aggregated_selects

  where 
    selects :: [Arg]
    selects = concat $ getSelects blocks

    cond :: Arg
    cond = getIf blocks
  
    cross_table :: Table
    cross_table = foldl1 crossJoinTable$ getTables symbol_map

    cross_symbol_map :: SymbolMap
    cross_symbol_map = getSymbolMap [cross_table]

    filtered_table :: Table
    filtered_table = doFilter$ eval cross_symbol_map cond
      where
        
        doFilter :: Printable -> Table
        doFilter (ValueP val) 
          | getBool val = cross_table 
          | otherwise = emptyTable cross_table

        doFilter (ColumnP col) = filterTable col cross_table


    filtered_symbol_map = getSymbolMap [filtered_table]

    by_stmt :: [Arg]
    by_stmt = getBy blocks

    by_tables :: [Table]
    by_tables = byTable 
      (filterCols$ map (eval filtered_symbol_map) by_stmt) filtered_table

    by_evaled_table :: Table
    by_evaled_table 
      | null by_stmt = filtered_table
      | otherwise = unionTables filtered_table$ map oneValTable$ by_tables

    aggregated_selects
      | null by_stmt = selects
      | otherwise = map (evalAggregateFunctions filtered_symbol_map) selects

    by_symbol_table :: SymbolMap
    by_symbol_table = getSymbolMap [by_evaled_table]
      

    sort_stmt :: [Arg]
    sort_stmt = getSort blocks

    sorted_table :: Table
    sorted_table 
      | null sort_stmt = by_evaled_table
      | otherwise = sortTable 
        (filterCols$ map (eval by_symbol_table) sort_stmt) by_evaled_table

    sorted_symbol_map = getSymbolMap [sorted_table]

