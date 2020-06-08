module FromBlock(getFromSymbols) where 

import Tables
import FileParser
import Options
import Args
import BracketExpansion
import Symbols

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
  ret <- many$ noneOf "= \n"
  char '='
  return ret

data FileName = ExoticFileName String | WildCards [String]

exoticFileName :: Parser FileName
exoticFileName = do
  char '`'
  ret <- many$ noneOf "`"
  char '`'
  return$ ExoticFileName ret

wildCards :: Parser FileName
wildCards = do
  file_n <- many$ noneOf " \n"
  return$ WildCards$ bracketExpand file_n

data FileAssignment = FileAssignment FileName [Option] | NamedFileAssignment String FileAssignment

unnamedFileP :: Parser FileAssignment
unnamedFileP = do
  file_name <- (try exoticFileName) <|> wildCards
  options <- many$ (try optionParser)
  return$ FileAssignment file_name options

namedFileP :: Parser FileAssignment
namedFileP = do
  name <- assignP
  rest <- unnamedFileP
  return$ NamedFileAssignment name rest

fileP :: Parser FileAssignment
fileP = do
  skipMany spaces
  ret <- (try namedFileP) <|> unnamedFileP
  return ret

filesP :: Parser [FileAssignment]
filesP = do
  ret <- many fileP
  skipMany spaces
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
  resolved <- mapM glob wcs
  return$ concat resolved

getNames :: FileAssignment -> [String]
getNames (NamedFileAssignment name rest) = name : getNames rest
getNames (_) = []

getOptions :: FileAssignment -> [Option]
getOptions (NamedFileAssignment _ rest) = getOptions rest
getOptions (FileAssignment _ options) = options

getFromSymbols :: Program -> String -> IO SymbolMap
getFromSymbols prog from_block = do

  let file_assignments = parseFromBlock from_block
  expanded <- mapM wildcardExpand file_assignments
  let no_expanded = zip3 expanded (map getNames file_assignments) (map getOptions file_assignments)

  let assignments = map getAssignment$ distribute 1 no_expanded
  symbols <- mapM parseFile assignments

  return$ foldl (+++) emptySymbolMap symbols

  where
    distribute :: Int -> [([String], [String], [Option])] -> [(Int, [String], String, [Option])]
    distribute _ [] = []
    distribute idx ((paths, names, options) : rest) = 
      let pth_length = length$ paths in
      [(i, names, path, options)| (i, path) <- zip [idx..] paths] ++

      distribute (idx + pth_length) rest

    getAssignment :: (Int, [String], String, [Option]) -> Assignment
    getAssignment (idx, name : rest, path, options) = NamedCsv name $ getAssignment (idx, rest, path, options)

    getAssignment (idx, [], path, options) = CoreCsv idx path prog options


  
