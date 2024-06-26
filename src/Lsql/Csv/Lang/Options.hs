{-|
This module implements the common `Option` type for the from block custom attributes representation and for the command-line optional arguments representation, and its parsers.
-}
module Lsql.Csv.Lang.Options (optionParser, 
  Option(Delimiter, SecondaryDelimiter, Quote, Named)) where

import System.Environment

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

char_option_p :: Parser Char
char_option_p = do
  ret <- anyChar
  return ret


delOp :: String -> Parser Char
delOp opt = do
  string opt
  c <- char_option_p
  return c

-- | The `Option` datatype for representing from block and command-line optional arguments.
data Option = Delimiter Char | SecondaryDelimiter Char | Quote Char | Named Bool

primaryDelOp = do
  r <- (try$ delOp "-d") <|> delOp "--delimiter="
  return $ Delimiter r

secondaryDelOp = do
  r <- (try$ delOp "-s") <|> delOp "--secondary-delimiter="
  return $ SecondaryDelimiter r


notNamedOp = do
  (try$ string "-N") <|> string "--not-named"
  return False

namedOp = do
  (try$ string "-n") <|> string "--named"
  return True

nameOp = do
  r <- (try$ namedOp) <|> notNamedOp
  return $ Named r

-- | The `Option` parser monad for parsing from block and command-line optional arguments.
optionParser :: Parser Option
optionParser = (try primaryDelOp) <|> (try secondaryDelOp) <|> nameOp
