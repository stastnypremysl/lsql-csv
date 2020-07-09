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
  printTable sorted

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

    
    aggregated_tables :: [Table]
    aggregated_tables
      | null by_stmt = [filtered_table]
      | otherwise = byTable (by_col$ map (eval filtered_symbol_map) by_stmt) 
          filtered_table
    
      where
        by_stmt :: [Arg]
        by_stmt = getBy blocks

        by_printable :: [Printable]
        by_printable = map (eval filtered_symbol_map) by_stmt

        by_col :: [Printable] -> [Column]
        by_col [] = []
        by_col ((ColumnP col) : rest) = col : (by_col rest)
        by_col (_ : rest) = by_col rest

     
    aggregated :: Bool
    aggregated = foldl1 (||)$ map containsAggregateF selects

    sort_by :: [Arg]
    sort_by = getSort blocks

    post_aggregated :: ([Column], Table)
    post_aggregated
     | not aggregated = 
         let evaluator = map (eval filtered_symbol_map) in
         ((getCols$ evaluator sort_by), (getTable []$ evaluator selects))

     | otherwise = 
        let evaluator x = (unionAggCols$ map (aggEval x) aggregated_tables) in
        ((getCols$ evaluator sort_by), (getTable []$ evaluator selects))

     where
       aggEval :: [Arg] -> Table -> [Printable]
       aggEval to_eval table =
         let symbols = getSymbolMap [table] in
         map (eval symbols)$ map (evalAggregateFunctions symbols) to_eval

    sorted :: Table
    sorted = sortTable (fst post_aggregated) (snd post_aggregated)




    
    
