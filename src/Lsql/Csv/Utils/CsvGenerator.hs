{-|
This module contains the CSV generator for the output.

-}

module Lsql.Csv.Utils.CsvGenerator(csvGenerate) where



import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Functions

import Data.List

-- | This function generates the CSV output.
csvGenerate :: Char -- ^ The primary delimiter 
            -> Char -- ^ The secondary delimiter (quote char)
            -> [Printable] -- ^ The list of columns
            -> String  

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
      | any (==sep) input = quoted_input
      | any (==sec_sep) input = quoted_input
      | any (=='\n') input = quoted_input

      | otherwise = input

      where 
        quoted_input :: String
        quoted_input = [sec_sep] ++ (doubleSecSep input) ++ [sec_sep]

        doubleSecSep :: String -> String
        doubleSecSep [] = []
        doubleSecSep (a : rest)
          | sec_sep == a = sec_sep : sec_sep : doubleSecSep rest
          | otherwise = a : doubleSecSep rest
