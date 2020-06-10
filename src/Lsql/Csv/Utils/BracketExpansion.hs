module Lsql.Csv.Utils.BracketExpansion (bracketExpand) where

import Data.List

import Text.ParserCombinators.Parsec

cumulatorComma :: Parser [String]
cumulatorComma = do
  atoms <- (try p_range) <|> p_atoms
  return atoms 
  
  where
    p_range :: Parser [String]
    p_range = do
      nb1 <- many1 digit
      optional$ spaces
      string ".."
      optional$ spaces
      nb2 <- many1 digit

      return$ map (show) $ get_range (read nb1) (read nb2)
      where
        get_range :: Int -> Int -> [Int]
        get_range n1 n2
          | n1 > n2 = reverse$ get_range n2 n1
          | otherwise = [n1..n2]

    p_atoms = many p_atom

    p_atom :: Parser String
    p_atom = do
      atom <- many1$ noneOf ",}"
      optional$ char ','
      return atom


bracketP :: Parser [String]
bracketP = do
  char '{'
  ret <- cumulatorComma
  char '}'
  return ret

afterBracketP :: Parser String
afterBracketP = do
  char '{'
  many$ noneOf "}"
  char '}'
  ret <- many$ noneOf ""
  return ret

bracketExpand :: String -> [String]
bracketExpand input =
  let first_expand = parseOne input in
  concat$ map (recOne) first_expand

  where
    parseOne :: String -> [String]
    parseOne inp = case parse bracketP "bracket expanstion" inp of
      Left _ -> [inp]
      Right ret -> map (++after_bracket) ret

      where
        after_bracket = case parse afterBracketP "bracket expansion" inp of
          Left err -> error$ show err
          Right ret -> ret

    recOne :: String -> [String]
    recOne [x] = [[x]]
    recOne (i:inp) = map (\x -> i:x) (bracketExpand inp)
