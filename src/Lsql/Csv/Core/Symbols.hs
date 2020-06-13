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

--id column alias
data Symbol = NamedColumn String Column String

data SymbolMap = SymbolMap (M.Map String Symbol) [Table]

emptySymbolMap :: [Table] -> SymbolMap
emptySymbolMap tables = SymbolMap M.empty tables

getSymbolName :: Symbol -> String
getSymbolName (NamedColumn _ _ name) = name

symbolList :: SymbolMap -> [String]
symbolList (SymbolMap t_map _) = map (fst) (M.toList t_map)

(+++) :: SymbolMap -> [Symbol] -> SymbolMap
(SymbolMap s_map tables) +++ [] = SymbolMap s_map tables
(SymbolMap s_map tables) +++ (a : s_array) =  
  let name = getSymbolName a in

  case M.lookup name s_map of
    Nothing -> (SymbolMap (M.insert name a s_map) tables) +++ s_array
    _ -> error$ "Symbol " ++ name ++ "is duplicite."

(-->) :: SymbolMap -> String -> Symbol
(SymbolMap s_map _) --> name = 
  case M.lookup name s_map of
    Nothing -> error$ "Symbol " ++ name ++ " not found" 
    Just s -> s

(==>) :: SymbolMap -> String -> Column
s_map ==> name =
  let (NamedColumn _ ret _) = s_map --> name in
  ret

getSymbolsFromTable :: Table -> [Symbol]
getSymbolsFromTable table = 
  let c_names = columnNames table in
  [NamedColumn (head names) col name | (names, col) <- c_names, name <- names]

getSymbolMap :: [Table] -> SymbolMap
getSymbolMap tables =
  foldl (+++) (emptySymbolMap tables)$ 
    map getSymbolsFromTable tables


getTables :: SymbolMap -> [Table]
getTables (SymbolMap _ tables) = tables
