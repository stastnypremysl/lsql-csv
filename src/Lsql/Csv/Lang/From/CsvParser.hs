{-|
This module contains the CsvParser called by parseFile, which loads input CSV files.
-}

module Lsql.Csv.Lang.From.CsvParser
  (Assignment (CoreCsv, NamedCsv), parseFile) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Symbols

import Lsql.Csv.Lang.Args
import Lsql.Csv.Lang.Options

import Lsql.Csv.Utils.BracketExpansion

import Data.List
import Data.Char
import qualified Data.Text as T

import System.IO

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

-- Csv Index Path Program [Option]
-- | Data structure representing one input file
data Assignment = 
  -- | Input file without any assign names
  CoreCsv 
    Int -- ^ Index of a file (indexing from 1)
    String -- ^ Path to a file
    Program -- ^ Potentially altered `Program` for given CSV file
    [Option] -- ^ Parsed additional `Option` for given CSV file
    |

  -- | Input file with assign name
  NamedCsv 
    String -- ^ Assign name
    Assignment -- ^ The rest of assignment


quoteP :: Char -> Parser String
quoteP second_delimiter = do
  char second_delimiter
  ret <- many$ ((try doubleQuoteP) <|> noneOf [second_delimiter])
  char second_delimiter
  return ret

  where
    doubleQuoteP :: Parser Char
    doubleQuoteP = do
      char second_delimiter
      char second_delimiter
      return second_delimiter

normalCellP :: Char -> Parser String
normalCellP delimiter = many$ noneOf ['\n', delimiter]

cellP :: Char -> Char -> Parser String
cellP delimiter sec_delimiter = (try$ quoteP sec_delimiter) <|> (normalCellP delimiter)

rowP :: Char -> Char -> Parser [String]
rowP delimiter sec_delimiter = do
  ret_non_term <- many$ try not_term_p 
  ret_term <- term_p
  _ <- char '\n'

  return$ ret_non_term ++ [ret_term]

  where 
    cell_p = cellP delimiter sec_delimiter

    not_term_p :: Parser String
    not_term_p = do
      ret <- cell_p
      char delimiter
      return ret
    
    term_p :: Parser String
    term_p = do 
      ret <- cell_p
      return ret


tableP :: Char -> Char -> Parser [[String]]
tableP delimiter sec_delimiter = many1$ rowP delimiter sec_delimiter

readValue :: String -> Value
readValue val
  | isTrue val = BoolValue True
  | isFalse val = BoolValue False
  | isInteger val = IntValue (read val)
  | isDouble val = DoubleValue (read val)
  | otherwise = StringValue val
  
  where
    isTrue s = s == "true"
    isFalse s = s == "false"

    isInteger s = case reads s :: [(Integer, String)] of
      [(_, "")] -> True
      _         -> False
 
    isDouble s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

check_square_data :: [[a]] -> [[a]]
check_square_data [] = []
check_square_data (x : rest) = x : check_square_data_n 2 (length x) rest
  
check_square_data_n :: Int -> Int -> [[a]] -> [[a]]
check_square_data_n _ _ [] = []
check_square_data_n line_number n (x : rest) = 
  if length x == n then 
    x : check_square_data_n (line_number+1) n rest
  else
    error$ "Invalid CSV file. Bad number of columns at line " ++ show line_number ++ 
      ". Expected " ++ show n ++ ". Got " ++ show (length x) ++ "."


buildTableFromIn :: [String] -> Bool -> [[String]] -> Table
buildTableFromIn table_names named in_str =
  buildTable table_names expanded_names in_data  

  where
    expanded_names :: [[String]]
    expanded_names = [[ table_name ++ "." ++ col_name
      | col_name <- col_names, table_name <- table_names] 
      | col_names <- names]

    names :: [[String]]
    names
      |named = map (\(x,y) -> [x,y]) (zip (head in_str)$ map show [1..])
      |otherwise = map (map show) $ group [1..]
    
    c_in_str :: [[String]]
    c_in_str = check_square_data in_str

    in_data :: [[Value]]
    in_data
      |named = map (map readValue)$ tail c_in_str
      |otherwise = map (map readValue)$ c_in_str

-- | Parses CSV file described in given `Assignment`
parseFile :: Assignment -> IO Table
parseFile assignment = do
  file_content <- load_input
  return$ parseTable (file_content)

  where    
    load_input :: IO String
    load_input 
      | file_name == "-" = getContents
      | otherwise = readFile file_name

    (Program _ delimiter second_delimiter first_line_names) = finalProgram
      where
        finalProgram :: Program
        finalProgram = reloadOpts in_program opts

    (CoreCsv index file_name in_program opts) = coreAssignment assignment
      where
        coreAssignment :: Assignment -> Assignment
        coreAssignment (NamedCsv _ core) = core
        coreAssignment core = core
    
    tableNames :: [String]
    tableNames = tableNamesGet assignment
      where 
        tableNamesGet :: Assignment -> [String]
        tableNamesGet (CoreCsv index file_name _ _ ) = ['&' : show index, file_name]
        tableNamesGet (NamedCsv next_name rest) = next_name : tableNamesGet rest

    parseTable :: String -> Table
    parseTable content =
      case parse (tableP delimiter second_delimiter) file_name$ T.pack content of
        Left err -> error$ show err
        Right parsed -> buildTableFromIn tableNames first_line_names parsed
    





