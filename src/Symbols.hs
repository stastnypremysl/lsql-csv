module Symbols(Symbol, getSymbolsFromInputTable) where

import Tables
import qualified Data.Map as M
import Data.List

data Symbol = NamedTable Table String | NamedColumn Column String 

data SymbolMap = SymbolMap (M.Map String Symbol)

getSymbolName :: Symbol -> String
getSymbolName (NamedTable _ name) = name
getSymbolName (NamedColumn _ name) = name

(+++) :: SymbolMap -> [Symbol] -> SymbolMap
(SymbolMap s_map) +++ [] = SymbolMap s_map
(SymbolMap s_map) +++ (a : s_array) =  
  let name = getSymbolName a in

  case M.lookup name s_map of
    Nothing -> SymbolMap$ M.insert name a s_map
    _ -> error$ "Symbol " ++ name ++ "is duplicite."

(-->) :: SymbolMap -> String -> Symbol
(SymbolMap s_map) --> name = 
  case M.lookup name s_map of
    Nothing -> error$ "Symbol " ++ name ++ " not found" 
    Just s -> s

getSymbolsFromInputTable :: [String] -> Table -> [Symbol]
getSymbolsFromInputTable table_names table = 
  (map (NamedTable table) table_names) ++ 

  [NamedColumn col name | (col_names, col) <- columnNames table, col_name <- col_names,
    table_name <- table_names, let name = table_name ++ "." ++ col_name]


