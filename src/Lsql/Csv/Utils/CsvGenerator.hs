module Lsql.Csv.Utils.CsvGenerator(csvGenerate) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Functions

import Data.List


csvGenerate :: Char -> Char -> [Printable] -> String
csvGenerate sep sec_sep cols =
  concat$ map genLine rows

  where
    n = getPrintableLength cols
    
    str_cols :: [[String]]
    str_cols = map pShow cols
      where
        pShow :: Printable -> [String]
        pShow (ValueP v) = take n$ repeat$ show v 
        pShow (ColumnP c) = showColumn c

    rows :: [[String]]
    rows = transpose str_cols

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
        doubleSecSep (sec_sep : rest) = sec_sep : sec_sep : doubleSecSep rest
        double (a : rest) = a : doubleSecSep rest