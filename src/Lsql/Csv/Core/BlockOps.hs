module Lsql.Csv.Core.BlockOps 
  (
    Block(Select, If, Sort, By),
    getSelects, getIf
  ) 
where

import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import Data.List

data Block = Select [Arg] | If Arg | Sort [Arg] | By [Arg]

getSelects :: [Block] -> [[Arg]]
getSelects [] = []
getSelects ((Select a) : rest) = a : getSelects rest
getSelects (_ : rest) = getSelects rest

getIf :: [Block] -> Arg
getIf blocks = 
  foldl (\x y -> Function$ LogicF$ And x y) 
    (Value$ BoolValue$ True) $ getIfBlocks blocks

  where
    getIfBlocks :: [Block] -> [Arg]
    getIfBlocks [] =[]
    getIfBlocks ((If a) : rest) = a : getIfBlocks rest
    getIfBlocks (_ : rest) = getIfBlocks rest
