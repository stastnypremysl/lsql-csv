module Lsql.Csv.Utils.CsvGenerator(csvGenerate) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Functions

import Data.List


csvGenerate :: Char -> Char -> [Printable] -> String
csvGenerate sep sec_sep cols =
  concat$ map genLine rows

  where
    
    rows :: [[String]]
    rows = transpose$ genStrCols cols

    genLine :: [String] -> String
    genLine (a : b : rest) = genCell a ++ [sep] ++ genLine (b : rest)
    genLine (a : _) = genCell a ++ ['\n']

    genCell :: String -> String
    genCell input 
      | any (==sep) input = [sec_sep] ++ (doubleSecSep input) ++ [sec_sep]
      | otherwise = input

      where 
        doubleSecSep :: String -> String
        doubleSecSep [] = []
        doubleSecSep (a : rest)
          | sec_sep == a = sec_sep : sec_sep : doubleSecSep rest
          | otherwise = a : doubleSecSep rest
