module Lsql.Csv.Core.Symbols
  (
    Symbol, SymbolMap (SymbolMap), (+++), (-->), (==>),
    emptySymbolMap, getSymbolsFromTable,
    symbolList
  )
where

import Lsql.Csv.Core.Tables
import qualified Data.Map as M
import Data.List

--id column alias
data Symbol = NamedColumn String Column String

data SymbolMap = SymbolMap (M.Map String Symbol)

emptySymbolMap :: SymbolMap
emptySymbolMap = SymbolMap M.empty

getSymbolName :: Symbol -> String
getSymbolName (NamedColumn _ _ name) = name

symbolList :: SymbolMap -> [String]
symbolList (SymbolMap t_map) = map (fst) (M.toList t_map)

(+++) :: SymbolMap -> [Symbol] -> SymbolMap
(SymbolMap s_map) +++ [] = SymbolMap s_map
(SymbolMap s_map) +++ (a : s_array) =  
  let name = getSymbolName a in

  case M.lookup name s_map of
    Nothing -> (SymbolMap$ M.insert name a s_map) +++ s_array
    _ -> error$ "Symbol " ++ name ++ "is duplicite."

(-->) :: SymbolMap -> String -> Symbol
(SymbolMap s_map) --> name = 
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


