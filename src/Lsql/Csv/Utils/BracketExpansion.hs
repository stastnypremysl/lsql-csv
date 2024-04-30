{-|
This module contains the curly bracket (braces) expansion implementation.
-}
module Lsql.Csv.Utils.BracketExpansion (bracketExpand) where

import Data.List
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

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


-- | The curly brackets (braces) expand function
--
-- The argument is a `String`, which you want to expand. Returns a list of expanded `String`s.
--
-- There are given a few usage examples:
--
-- >>> bracketExpand "car{A,B}"
-- ["carA","carB"]
--
-- >>> bracketExpand "car{1..5}"
-- ["car1","car2","car3","car4","car5"]
--
-- >>> bracketExpand "{car,bus}{0..2}"
-- ["car0","car1","car2","bus0","bus1","bus2"]

bracketExpand :: String -> [String]
bracketExpand input =
  let first_expand = parseOne input in
  concat$ map (recOne) first_expand

  where
    parseOne :: String -> [String]
    parseOne inp = case parse bracketP "bracket expansion"$ T.pack inp of
      Left _ -> [inp]
      Right ret -> map (++after_bracket) ret

      where
        after_bracket = case parse afterBracketP "bracket expansion"$ T.pack inp of
          Left err -> error$ show err
          Right ret -> ret

    recOne :: String -> [String]
    recOne [x] = [[x]]
    recOne (i:inp) = map (\x -> i:x) (bracketExpand inp)
