module Options (optionParser, 
  Option(Delimiter, SecondaryDelimiter, Quote, Named)) where

import System.Environment

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

char_option_p :: Parser Char
char_option_p = do
  ret <- q1 <|> q2 <|> q3
  return ret

  where
    q1 = do
      char '"'
      r <- anyChar
      char '"'
      return r
    
    q2 = do
      char '\''
      r <- anyChar
      char '\''
      return r

    q3 = do
      r <- anyChar
      return r


delOp :: String -> Parser Char
delOp opt = do
  string opt
  skipMany space
  c <- char_option_p
  return c

data Option = Delimiter Char | SecondaryDelimiter Char | Quote Char | Named Bool

primaryDelOp = do
  r <- (try$ delOp "-d") <|> delOp "--delimiter"
  return $ Delimiter r

secondaryDelOp = do
  r <- (try$ delOp "-s") <|> delOp "--secondary-delimiter"
  return $ SecondaryDelimiter r

quoteOp = do
  r <- (try$ delOp "-q") <|> delOp "--quote"
  return $ Quote r

notNamedOp = do
  (try$ string "-N") <|> string "--not-named"
  return False

namedOp = do
  (try$ string "-n") <|> string "--named"
  return True

nameOp = do
  r <- namedOp <|> notNamedOp
  return $ Named r

optionParser :: Parser Option
optionParser = (try primaryDelOp) <|> (try secondaryDelOp) <|> (try quoteOp) <|> nameOp
