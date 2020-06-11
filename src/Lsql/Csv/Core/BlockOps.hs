module Lsql.Csv.Core.BlockOps 
  (
    Block(Select, If, Sort, By),
    getSelects
  ) 
where

import Lsql.Csv.Core.Functions

data Block = Select [Arg] | If Arg | Sort [Arg] | By [Arg]

getSelects :: [Block] -> [[Arg]]
getSelects [] = []
getSelects ((Select a) : rest) = a : getSelects rest
getSelects (_ : rest) = getSelects rest

