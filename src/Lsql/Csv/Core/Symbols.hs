{-|
This module contains the definition of `Symbol`, `SymbolMap`, and helper functions for working with them. 
`SymbolMap` is one of the representations of input data.
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

-- | The datatype for a single named column (Symbol)
data Symbol = NamedColumn 
  String -- ^ identifier of column 
  Column -- ^ column itself
  String -- ^ name alias of column

-- | The datatype containing a `M.Map` for aliases of `Symbol`s and a list of all `Table`s
data SymbolMap = SymbolMap (M.Map String Symbol) [Table]

emptySymbolMap :: [Table] -> SymbolMap
emptySymbolMap tables = SymbolMap M.empty tables

getSymbolName :: Symbol -> String
getSymbolName (NamedColumn _ _ name) = name

-- | Returns the list of name aliases of all columns.
symbolList :: SymbolMap -> [String]
symbolList (SymbolMap t_map _) = map (fst) (M.toList t_map)

(+++) :: SymbolMap -> [Symbol] -> SymbolMap
(SymbolMap s_map tables) +++ [] = SymbolMap s_map tables
(SymbolMap s_map tables) +++ (a : s_array) =  
  let name = getSymbolName a in
  (SymbolMap (M.insert name a s_map) tables) +++ s_array

-- | Lookup for a `Symbol` (NamedColumn) in a `SymbolMap`
(-->) :: SymbolMap -> String -> Symbol
(SymbolMap s_map _) --> name = 
  case M.lookup name s_map of
    Nothing -> error$ "Symbol " ++ name ++ " not found. " 
    Just s -> s

-- | Lookup for a `Symbol` (NamedColumn) in a `SymbolMap`
-- returning a `Column`
(==>) :: SymbolMap -> String -> Column
s_map ==> name =
  let (NamedColumn _ ret _) = s_map --> name in
  ret

getSymbolsFromTable :: Table -> [Symbol]
getSymbolsFromTable table = 
  let c_names = columnNames table in
  [NamedColumn (head names) col name | (names, col) <- c_names, name <- names]

-- | Generates a `SymbolMap` out of a list of `Table`.
getSymbolMap :: [Table] -> SymbolMap
getSymbolMap tables =
  foldl (+++) (emptySymbolMap tables)$ 
    map getSymbolsFromTable tables


-- | Returns the list of `Table`s out of `SymbolMap`.
getTables :: SymbolMap -> [Table]
getTables (SymbolMap _ tables) = tables
