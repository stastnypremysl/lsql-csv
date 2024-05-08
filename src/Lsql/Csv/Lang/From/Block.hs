{-|
This module contains the from block parser. It loads the initial `SymbolMap` with input data.
-}
module Lsql.Csv.Lang.From.Block(getFromSymbols) where 

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Symbols

import Lsql.Csv.Lang.From.CsvParser
import Lsql.Csv.Lang.Options
import Lsql.Csv.Lang.Args

import Lsql.Csv.Utils.BracketExpansion

import System.IO
import System.Environment

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import System.FilePath.Glob

import qualified Data.Text as T

assignP :: Parser String
assignP = do
  ret <- many1$ noneOf "= \n\t"
  char '='
  return ret

data FileName = ExoticFileName String | WildCards [String]

exoticFileName :: Parser FileName
exoticFileName = do
  char '`'
  ret <- many1$ noneOf "`"
  char '`'
  return$ ExoticFileName ret

wildCards :: Parser FileName
wildCards = do
  file_n <- many1$ noneOf " \n\t"
  return$ WildCards$ bracketExpand file_n

data FileAssignment = 
  FileAssignment FileName [Option] | NamedFileAssignment String FileAssignment

stdinFileP :: Parser FileName
stdinFileP = do
  char '-'
  return$ ExoticFileName "-"


optionSpaceParser :: Parser Option
optionSpaceParser = do
  ret <- optionParser
  many space
  return$ ret

unnamedFileP :: Parser FileAssignment
unnamedFileP = do
  file_name <- stdinFileP <|> exoticFileName <|> wildCards
  many space
  options <- many$ (try optionSpaceParser)
  return$ FileAssignment file_name options


namedFileP :: Parser FileAssignment
namedFileP = do
  name <- assignP
  rest <- unnamedFileP
  return$ NamedFileAssignment name rest

fileP :: Parser FileAssignment
fileP = do
  skipMany space
  ret <- (try namedFileP) <|> unnamedFileP
  skipMany space
  return ret

filesP :: Parser [FileAssignment]
filesP = do
  ret <- many fileP
  return ret

parseFromBlock :: String -> [FileAssignment]
parseFromBlock content =
  case parse filesP "from block"$ T.pack content of
    Left err -> error$ show err
    Right parsed -> parsed

wildcardExpand :: FileAssignment -> IO [String]
wildcardExpand (NamedFileAssignment _ a) = wildcardExpand a

wildcardExpand (FileAssignment (ExoticFileName s) _) = do
  return [s]

wildcardExpand (FileAssignment (WildCards wcs) _) = do
  resolved <- mapM custGlob wcs
  return$ concat resolved

  where
    custGlob :: String -> IO [String]
    custGlob x = do
      ret <- glob x

      if ret /= [] then
        return ret
      else 
        return [x]

getNames :: FileAssignment -> [String]
getNames (NamedFileAssignment name rest) = name : getNames rest
getNames (_) = []

getOptions :: FileAssignment -> [Option]
getOptions (NamedFileAssignment _ rest) = getOptions rest
getOptions (FileAssignment _ options) = options

-- | Loads `SymbolMap` according to a `Program` and a from block `String` in the second argument.
getFromSymbols :: Program -> String -> IO SymbolMap
getFromSymbols prog from_block = do

  let file_assignments = parseFromBlock from_block
  expanded <- mapM wildcardExpand file_assignments
  let no_expanded = zip3 expanded (map getNames file_assignments) (map getOptions file_assignments)

  let assignments = map getAssignment$ distribute 1 no_expanded
  tables <- mapM parseFile assignments

  return$ getSymbolMap tables

  where
    distribute :: Int -> [([String], [String], [Option])] -> [(Int, [String], String, [Option])]
    distribute _ [] = []
    distribute idx ((paths, names, options) : rest) = 
      let pth_length = length$ paths in
      [(i, names, path, options)| (i, path) <- zip [idx..] paths] ++

      distribute (idx + pth_length) rest

    getAssignment :: (Int, [String], String, [Option]) -> Assignment
    getAssignment (idx, name : rest, path, options) = 
      NamedCsv name $ getAssignment (idx, rest, path, options)

    getAssignment (idx, [], path, options) = CoreCsv idx path prog options


  
