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
data Assignment = CoreCsv Int String Program [Option] | NamedCsv String Assignment


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
  skipMany$ oneOf " \t\n"

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
      skipMany$ oneOf " \t"
      char '\n'
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
    
    in_data :: [[Value]]
    in_data
      |named = map (map readValue)$ tail in_str
      |otherwise = map (map readValue)$ in_str


parseFile :: Assignment -> IO Table
parseFile assignment = do
  file_content <- load_input
  return$ parseTable (file_content ++ ['\n'])

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
    





