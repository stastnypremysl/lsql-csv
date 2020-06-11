module Lsql.Csv.Utils.CsvGenerator(csvGenerate) where

import Lsql.Csv.Core.Tables

import Data.List

csvGenerate :: Char -> Char -> [Column] -> String
csvGenerate sep sec_sep cols =
  concat$ map genLine rows

  where
    str_cols :: [[String]]
    str_cols = map showColumn cols

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
