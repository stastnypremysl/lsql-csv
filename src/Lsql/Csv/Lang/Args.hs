module Lsql.Csv.Lang.Args (Program(Program), parseArgs, reloadOpts) where

import Lsql.Csv.Lang.Options

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import Data.List
import qualified Data.Text as T

-- command, first, second, name_line
data Program = Program String Char Char Bool deriving (Show)

data Arg = Opt Option| Cmd String

load_ag :: Program -> [Arg] -> Program

load_ag p [] = p
load_ag (Program a1 a2 a3 a4) (Cmd c : rest) = load_ag (Program c a2 a3 a4) rest
load_ag (Program a1 a2 a3 a4) (Opt (Delimiter c) : rest) = load_ag (Program a1 c a3 a4) rest
load_ag (Program a1 a2 a3 a4) (Opt (SecondaryDelimiter c) : rest) = load_ag (Program a1 a2 c a4) rest
load_ag (Program a1 a2 a3 a4) (Opt (Named c) : rest) = load_ag (Program a1 a2 a3 c) rest


load_args :: [Arg] -> Program
load_args args = 
  load_ag (Program "" ';' '"' False) args

reloadOpts :: Program -> [Option] -> Program
reloadOpts pg ops = load_ag pg$ map (Opt) ops

   
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
  ret <- (try argOptionP)
  skipMany space
  return ret

fullP :: Parser [Arg]
fullP = do
  ags <- many argP
  cmd <- argCmdP
  return$ ags ++ [cmd]

parseArgs :: [String] -> Program
parseArgs args = 
  let input = unwords args in
  case parse fullP "arguments" (T.pack input) of
    Left err -> error $ show err
    Right val -> load_args val
