module Lsql.Csv.Core.BlockOps 
  (
    Block(Select, If, Sort, By)
  ) 
where

import Lsql.Csv.Core.Functions

data Block = Select [Arg] | If Arg | Sort [Arg] | By [Arg]

getSelects :: [Block] -> [Block]
getSelects [] = []
getSelects ((Select a) : rest) = Select a : getSelects rest
getSelects (_ : rest) = getSelects rest

