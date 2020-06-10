module Lsql.Csv.Lang.Selector (selectorP, aritmeticExprP) where

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Text
import Text.Parsec.Char

import Data.List

import Lsql.Csv.Utils.BracketExpansion
import Lsql.Csv.Utils.Errors

import Lsql.Csv.Core.Functions
import Lsql.Csv.Core.Tables

import System.FilePath.Glob

aggregateFGen :: ([Arg] -> AggregateF) -> [Arg] -> Arg
aggregateFGen fun arg = Function$ AggregateF$ fun arg

aggregateFP :: ([Arg] -> Arg) -> String -> [String] -> Parser Arg
aggregateFP constructor name symbol_list = do
  string name
  char '('
  ret <- selectorP symbol_list
  char ')'
  return$ constructor$ ret

catP symbol_list = try$ aggregateFP (aggregateFGen Cat) "cat" symbol_list
sumP symbol_list = try$ aggregateFP (aggregateFGen Sum) "sum" symbol_list

aggregateFunctionsP :: [String] -> Parser Arg
aggregateFunctionsP symbol_list =
  (catP symbol_list) <|> (sumP symbol_list)

oneArgFP :: (Arg -> Arg) -> String -> [String] -> Parser Arg
oneArgFP constructor name symbol_list = do
  string name
  char '('
  ret <- aritmeticExprP symbol_list
  char ')'

  return$ constructor$ ret

aritmeticFGen :: (Arg -> AritmeticF) -> Arg -> Arg
aritmeticFGen fun arg = Function$ AritmeticF$ fun arg
 
sinP symbol_list = try$ oneArgFP (aritmeticFGen Sin) "sin" symbol_list
cosP symbol_list = try$ oneArgFP (aritmeticFGen Cos) "cos" symbol_list
tanP symbol_list = try$ oneArgFP (aritmeticFGen Tan) "tan" symbol_list
arcsinP symbol_list = try$ oneArgFP (aritmeticFGen Arcsin) "arcsin" symbol_list
arccosP symbol_list = try$ oneArgFP (aritmeticFGen Arccos) "arccos" symbol_list
arctanP symbol_list = try$ oneArgFP (aritmeticFGen Arctan) "arctan" symbol_list
lengthP symbol_list = try$ oneArgFP (aritmeticFGen Count) "count" symbol_list

oneArgFunctionsP :: [String] -> Parser Arg
oneArgFunctionsP symbol_list = 
  (sinP symbol_list) <|> (cosP symbol_list) <|> (tanP symbol_list) <|>
  (arcsinP symbol_list) <|> (arccosP symbol_list) <|> (arctanP symbol_list) <|>
  (lengthP symbol_list)

notP :: [String] -> Parser Arg
notP symbol_list = do
  char '!'
  skipMany spaces
  ret <- aritmeticExprP symbol_list
  return$ Function$ LogicF$ Not ret

minusSP :: [String] -> Parser Arg
minusSP symbol_list = do
  char '-'
  skipMany spaces
  ret <- aritmeticExprP symbol_list
  return$ Function$ AritmeticF$ MinusS ret


aritmeticF2Gen :: (Arg -> Arg -> AritmeticF) -> Arg -> Arg -> Arg
aritmeticF2Gen fun arg1 arg2 = Function$ AritmeticF$ fun arg1 arg2

logicF2Gen :: (Arg -> Arg -> LogicF) -> Arg -> Arg -> Arg
logicF2Gen fun arg1 arg2 = Function$ LogicF$ fun arg1 arg2

twoArgInFP :: (Arg -> Arg -> Arg) -> String -> [String] -> Parser Arg
twoArgInFP constructor op_name symbol_list = do
  arg1 <- aritmeticExprP symbol_list
  skipMany spaces
  string op_name
  arg2 <- aritmeticExprP symbol_list

  return$ constructor arg1 arg2

orP symbol_list = try$ twoArgInFP (logicF2Gen Or) "or" symbol_list
andP symbol_list = try$ twoArgInFP (logicF2Gen And) "and" symbol_list

leftOuterJoinP symbol_list = try$ twoArgInFP (aritmeticF2Gen LeftOuterJoin) "=>=" symbol_list

lessOrEqualP symbol_list = try$ twoArgInFP (aritmeticF2Gen LessOrEqual) "<=" symbol_list
moreOrEqualP symbol_list = try$ twoArgInFP (aritmeticF2Gen MoreOrEqual) ">=" symbol_list

lessP symbol_list = try$ twoArgInFP (aritmeticF2Gen Less) "<" symbol_list
moreP symbol_list = try$ twoArgInFP (aritmeticF2Gen More) ">" symbol_list

inP symbol_list = try$ twoArgInFP (aritmeticF2Gen In) "in" symbol_list

powerP symbol_list = try$ twoArgInFP (aritmeticF2Gen Power) "^" symbol_list

multiplyP symbol_list = try$ twoArgInFP (aritmeticF2Gen Multiply) "*" symbol_list
divideP symbol_list = try$ twoArgInFP (aritmeticF2Gen Divide) "/" symbol_list

plusP symbol_list = try$ twoArgInFP (aritmeticF2Gen Plus) "+" symbol_list
minusP symbol_list = try$ twoArgInFP (aritmeticF2Gen Minus) "-" symbol_list

twoArgInFunctionsP :: [String] -> Parser Arg
twoArgInFunctionsP symbol_list = 
  (orP symbol_list) <|> (andP symbol_list) <|> (leftOuterJoinP symbol_list) <|>
  (lessOrEqualP symbol_list) <|> (moreOrEqualP symbol_list) <|>
  (lessP symbol_list) <|> (moreP symbol_list) <|>
  (inP symbol_list) <|> (powerP symbol_list) <|>
  (multiplyP symbol_list) <|> (divideP symbol_list) <|>
  (plusP symbol_list) <|> (minusP symbol_list)

bracketAritmeticExprP :: [String] -> Parser Arg
bracketAritmeticExprP symbol_list = do
  char '('
  ret <- aritmeticExprP symbol_list
  char ')'
  return$ ret

dolarAritmeticExprP :: [String] -> Parser Arg
dolarAritmeticExprP symbol_list = do
  char '$'
  ret <- bracketAritmeticExprP symbol_list
  return ret

aritmeticExprP :: [String] -> Parser Arg
aritmeticExprP symbol_list = do 
  skipMany space

  ret <- (bracketAritmeticExprP symbol_list) <|>
    (try$ dolarAritmeticExprP symbol_list) <|> 
    (try$ oneArgFunctionsP symbol_list) <|>
    (try$ aggregateFunctionsP symbol_list) <|>
    (try$ notP symbol_list) <|>
    (try$ minusSP symbol_list) <|>
    (try$ twoArgInFunctionsP symbol_list) <|>
    oneAtomP

  return ret



globMatching :: [String] -> String -> [String]
globMatching symbols expr = 
  let p = compile expr in
  filter (match p) symbols

exoticAtomP :: Parser Arg
exoticAtomP = do
  char '`'
  ret <- many1$ noneOf "`"
  char '`'
  return$ Symbol ret

nonAtomChars = "\n `\",'$()"

oneRegularAtomP :: Parser Arg
oneRegularAtomP = do
  skipMany space
  atom <- many1$ noneOf nonAtomChars
  return$ Symbol atom

oneAtomP :: Parser Arg
oneAtomP = do
  skipMany spaces
  ret <- exoticAtomP <|>stringConstantP <|>
    (try$ trueConstantP) <|> (try$ falseConstantP) <|>
    (try$ intConstantP) <|> (try$ doubleConstantP) <|>
     oneRegularAtomP
  return ret

selectAtomP :: [String] -> Parser [Arg]
selectAtomP symbol_list = do 
  expr <- many1$ noneOf nonAtomChars
  let symbols = concat$ map (globMatching symbol_list)$ bracketExpand expr

  return$ map Symbol symbols

intConstantP :: Parser Arg
intConstantP = do
  ret <- many1$ digit
  return$ Value$ IntValue$ read ret

doubleConstantP :: Parser Arg
doubleConstantP = do
  ret1 <- many$ digit
  ret2 <- string "."
  ret3 <- many$ digit

  return$ Value$ DoubleValue$ read$ ret1 ++ ret2 ++ ret3

stringConstantP :: Parser Arg
stringConstantP = do
  char '"'
  ret <- many$ noneOf "\""
  char '"'
  
  return$ Value$ StringValue ret

trueConstantP :: Parser Arg
trueConstantP = do
  string "true"
  return$ Value$ BoolValue$ True

falseConstantP :: Parser Arg
falseConstantP = do
  string "false"
  return$ Value$ BoolValue$ False
 
atomP :: [String] -> Parser [Arg]
atomP symbol_list = do
  skipMany spaces
  ret <- (many1$ stringConstantP) <|> (many1$ exoticAtomP) <|>
    (try$ many1$ dolarAritmeticExprP symbol_list) <|>
    (try$ many1$ trueConstantP) <|> (try$ many1$ falseConstantP) <|>
    (try$ many1$ intConstantP) <|> (try$ many1$ doubleConstantP) <|>
    (selectAtomP symbol_list)
  return ret
  

selectorP :: [String] -> Parser [Arg]
selectorP symbol_list = do
  ret <- many$ atomP symbol_list
  return$ concat ret


  
