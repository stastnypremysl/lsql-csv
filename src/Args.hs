module Args (Program, parseArgs) where

import Text.ParserCombinators.Parsec
import Data.List
import Options

-- command, first, second, name_line
data Program = Program String Char Char Bool deriving (Show)

data Arg = Opt Option| Cmd String

load_args :: [Arg] -> Program
load_args args = 
  load_ag (Program "" ';' ',' False) args

  where
    load_ag :: Program -> [Arg] -> Program

    load_ag p [] = p
    load_ag (Program a1 a2 a3 a4) (Cmd c : rest) = load_ag (Program c a2 a3 a4) rest
    load_ag (Program a1 a2 a3 a4) (Opt (Delimiter c) : rest) = load_ag (Program a1 c a3 a4) rest
    load_ag (Program a1 a2 a3 a4) (Opt (SecondaryDelimiter c) : rest) = load_ag (Program a1 a2 c a4) rest
    load_ag (Program a1 a2 a3 a4) (Opt (Named c) : rest) = load_ag (Program a1 a2 a3 c) rest

argOptionP :: Parser Arg
argOptionP = do
  op <- optionParser
  return$ Opt op

argCmdP :: Parser Arg
argCmdP = do
  op <- many1 anyChar
  return$ Cmd op

argP :: Parser Arg
argP = do
  skipMany space
  ret <- (try argOptionP) <|> argCmdP
  return ret

parseArgs :: [String] -> Program
parseArgs args = 
  let input = unwords args in
  case parse (many argP) "arguments" input of
    Left err -> error $ show err
    Right val -> load_args val
