{-|
This module contains `Block` definition and functions for getting specific types of blocks from list of `Block`.
-}

module Lsql.Csv.Core.BlockOps 
  (
    Block(Select, If, Sort, By),
    getSelects, getIf, getSort, getBy
  ) 
where

import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import Data.List

-- | This data structure represents parsed blocks
data Block = Select [Arg] | If Arg | Sort [Arg] | By [Arg]

-- | Returns all selects blocks
getSelects :: [Block] -> [[Arg]]
getSelects [] = []
getSelects ((Select a) : rest) = a : getSelects rest
getSelects (_ : rest) = getSelects rest

-- | Makes conjunction of all if blocks (if none, simple True is returned) and returns it
getIf :: [Block] -> Arg
getIf blocks = 
  foldl (\x y -> Function$ LogicF$ And x y) 
    (Value$ BoolValue$ True) $ getIfBlocks blocks

  where
    getIfBlocks :: [Block] -> [Arg]
    getIfBlocks [] = []
    getIfBlocks ((If a) : rest) = a : getIfBlocks rest
    getIfBlocks (_ : rest) = getIfBlocks rest

-- | Finds sort block, if exists, and returns it. If there is non, [] is returned.
-- If there is more than one sort block, fails.
getSort :: [Block] -> [Arg]
getSort [] = []
getSort ((Sort a) : rest) 
  | null$ getSort rest = a
  | otherwise = error "There can be only one sort statement."

getSort (_ : rest) = getSort rest

-- | Finds by block, if exists, and returns it. If there is non, [] is returned.
-- If there is more than one by block, fails.
getBy :: [Block] -> [Arg]
getBy [] = []
getBy ((By a) : rest)
  | null$ getBy rest = a
  | otherwise = error "There can be only one by statement."

getBy (_ : rest) = getBy rest

