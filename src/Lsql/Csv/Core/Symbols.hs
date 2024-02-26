{-|
This module contains definition of `Symbol`, `SymbolMap` and helper functions.
-}
module Lsql.Csv.Core.Symbols
  (
    Symbol, SymbolMap, (-->), (==>),
    getSymbolMap, symbolList,
    getTables
  )
where

import Lsql.Csv.Core.Tables
import qualified Data.Map as M
import Data.List

-- | Data type for single named column (Symbol)
data Symbol = NamedColumn 
  String -- ^ identifier of column 
  Column -- ^ column itself
  String -- ^ name alias of column

-- | Datatype containing `M.Map` for aliases of symbols and list of all tables
data SymbolMap = SymbolMap (M.Map String Symbol) [Table]

emptySymbolMap :: [Table] -> SymbolMap
emptySymbolMap tables = SymbolMap M.empty tables

getSymbolName :: Symbol -> String
getSymbolName (NamedColumn _ _ name) = name

-- | Returns list of name aliases of all columns
symbolList :: SymbolMap -> [String]
symbolList (SymbolMap t_map _) = map (fst) (M.toList t_map)

(+++) :: SymbolMap -> [Symbol] -> SymbolMap
(SymbolMap s_map tables) +++ [] = SymbolMap s_map tables
(SymbolMap s_map tables) +++ (a : s_array) =  
  let name = getSymbolName a in
  (SymbolMap (M.insert name a s_map) tables) +++ s_array

-- | Lookup for `Symbol` (NamedColumn) in `SymbolMap`
(-->) :: SymbolMap -> String -> Symbol
(SymbolMap s_map _) --> name = 
  case M.lookup name s_map of
    Nothing -> error$ "Symbol " ++ name ++ " not found. " 
    Just s -> s

-- | Lookup for `Symbol` (NamedColumn) in `SymbolMap`
-- returning `Column`
(==>) :: SymbolMap -> String -> Column
s_map ==> name =
  let (NamedColumn _ ret _) = s_map --> name in
  ret

getSymbolsFromTable :: Table -> [Symbol]
getSymbolsFromTable table = 
  let c_names = columnNames table in
  [NamedColumn (head names) col name | (names, col) <- c_names, name <- names]

-- | Generates `SymbolMap` out of list of `Table`
getSymbolMap :: [Table] -> SymbolMap
getSymbolMap tables =
  foldl (+++) (emptySymbolMap tables)$ 
    map getSymbolsFromTable tables


-- | Returns list of `Table`s out of SymbolMap
getTables :: SymbolMap -> [Table]
getTables (SymbolMap _ tables) = tables
