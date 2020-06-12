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
asinP symbol_list = try$ oneArgFP (aritmeticFGen Asin) "asin" symbol_list
acosP symbol_list = try$ oneArgFP (aritmeticFGen Acos) "acos" symbol_list
atanP symbol_list = try$ oneArgFP (aritmeticFGen Atan) "atan" symbol_list

sinhP symbol_list = try$ oneArgFP (aritmeticFGen Sinh) "sinh" symbol_list
coshP symbol_list = try$ oneArgFP (aritmeticFGen Cosh) "cosh" symbol_list
tanhP symbol_list = try$ oneArgFP (aritmeticFGen Tanh) "tanh" symbol_list
asinhP symbol_list = try$ oneArgFP (aritmeticFGen Asinh) "asinh" symbol_list
acoshP symbol_list = try$ oneArgFP (aritmeticFGen Acosh) "acosh" symbol_list
atanhP symbol_list = try$ oneArgFP (aritmeticFGen Atanh) "atanh" symbol_list

lengthP symbol_list = try$ oneArgFP (aritmeticFGen Atanh) "length" symbol_list

oneArgFunctionsP :: [String] -> Parser Arg
oneArgFunctionsP symbol_list = 
  (sinP symbol_list) <|> (cosP symbol_list) <|> (tanP symbol_list) <|>
  (asinP symbol_list) <|> (acosP symbol_list) <|> (atanP symbol_list) <|>

  (sinhP symbol_list) <|> (coshP symbol_list) <|> (tanhP symbol_list) <|>
  (asinhP symbol_list) <|> (acoshP symbol_list) <|> (atanhP symbol_list) <|>

  (lengthP symbol_list)

notP :: [String] -> Parser Arg
notP symbol_list = do
  char '!'
  skipMany space
  ret <- aritmeticExprP symbol_list
  return$ Function$ LogicF$ Not ret

minusSP :: [String] -> Parser Arg
minusSP symbol_list = do
  char '-'
  skipMany space
  ret <- aritmeticExprP symbol_list
  return$ Function$ AritmeticF$ MinusS ret


aritmeticF2Gen :: (Arg -> Arg -> AritmeticF) -> Arg -> Arg -> Arg
aritmeticF2Gen fun arg1 arg2 = Function$ AritmeticF$ fun arg1 arg2

logicF2Gen :: (Arg -> Arg -> LogicF) -> Arg -> Arg -> Arg
logicF2Gen fun arg1 arg2 = Function$ LogicF$ fun arg1 arg2

twoArgInFP :: Int -> (Arg -> Arg -> Arg) -> String -> [String] -> Arg -> Parser Arg
twoArgInFP level constructor op_name symbol_list arg1 = do
  skipMany space
  string op_name
  arg2 <- aritmeticExprGenP level symbol_list

  return$ constructor arg1 arg2

orP symbol_list arg = try$ twoArgInFP 1 (logicF2Gen Or) "or" symbol_list arg
andP symbol_list arg = try$ twoArgInFP 1 (logicF2Gen And) "and" symbol_list arg

leftOuterJoinP symbol_list arg = 
  try$ twoArgInFP 2 (aritmeticF2Gen LeftOuterJoin) "=>=" symbol_list arg

lessOrEqualP symbol_list arg = 
  try$ twoArgInFP 2 (aritmeticF2Gen LessOrEqual) "<=" symbol_list arg

moreOrEqualP symbol_list arg = 
  try$ twoArgInFP 2 (aritmeticF2Gen MoreOrEqual) ">=" symbol_list arg

lessP symbol_list arg = 
  try$ twoArgInFP 2 (aritmeticF2Gen Less) "<" symbol_list arg
moreP symbol_list arg = 
  try$ twoArgInFP 2 (aritmeticF2Gen More) ">" symbol_list arg

inP symbol_list arg = 
  try$ twoArgInFP 3 (aritmeticF2Gen In) "in" symbol_list arg

powerP symbol_list arg = 
  try$ twoArgInFP 3 (aritmeticF2Gen Power) "^" symbol_list arg

multiplyP symbol_list arg = 
  try$ twoArgInFP 4 (aritmeticF2Gen Multiply) "*" symbol_list arg
divideP symbol_list arg = 
  try$ twoArgInFP 4 (aritmeticF2Gen Divide) "/" symbol_list arg

plusP symbol_list arg = 
  try$ twoArgInFP 5 (aritmeticF2Gen Plus) "+" symbol_list arg
minusP symbol_list arg = 
  try$ twoArgInFP 5 (aritmeticF2Gen Minus) "-" symbol_list arg

twoArgInFunctions1P :: [String] -> Arg -> Parser Arg
twoArgInFunctions1P symbol_list arg = 
  (orP symbol_list arg) <|> (andP symbol_list arg) 

twoArgInFunctions2P :: [String] -> Arg -> Parser Arg
twoArgInFunctions2P symbol_list arg = 
  (leftOuterJoinP symbol_list arg) <|>
  (lessOrEqualP symbol_list arg) <|> (moreOrEqualP symbol_list arg) <|>
  (lessP symbol_list arg) <|> (moreP symbol_list arg) 

twoArgInFunctions3P :: [String] -> Arg -> Parser Arg
twoArgInFunctions3P symbol_list arg = 
  (inP symbol_list arg) <|> (powerP symbol_list arg)

twoArgInFunctions4P :: [String] -> Arg -> Parser Arg
twoArgInFunctions4P symbol_list arg = 
  (multiplyP symbol_list arg) <|> (divideP symbol_list arg)

twoArgInFunctions5P :: [String] -> Arg -> Parser Arg
twoArgInFunctions5P symbol_list arg = 
  (plusP symbol_list arg) <|> (minusP symbol_list arg)

bracketAritmeticExprP :: [String] -> Parser Arg
bracketAritmeticExprP symbol_list = do
  char '('
  ret <- aritmeticExprP symbol_list
  char ')'
  skipMany space
  return$ ret

dolarAritmeticExprP :: [String] -> Parser Arg
dolarAritmeticExprP symbol_list = do
  char '$'
  ret <- bracketAritmeticExprP symbol_list
  return ret

nonRecAritmeticExpr :: [String] -> Parser Arg
nonRecAritmeticExpr symbol_list = do 
  skipMany space
  ret <-(bracketAritmeticExprP symbol_list) <|>
    (try$ dolarAritmeticExprP symbol_list) <|> 
    (try$ oneArgFunctionsP symbol_list) <|>
    (try$ aggregateFunctionsP symbol_list) <|>
    (try$ notP symbol_list) <|>
    (try$ minusSP symbol_list) <|>
    oneAtomP
  
  skipMany space
  return ret

aritmeticExprNP :: [[String] -> Arg -> Parser Arg] -> [String] -> Parser Arg
aritmeticExprNP [] symbol_list = nonRecAritmeticExpr symbol_list

aritmeticExprNP (funGen : rest) symbol_list = do 
  pre <- aritmeticExprNP rest symbol_list
  ret <- recP pre
  return$ ret

  where
    recP :: Arg -> Parser Arg
    recP accu = do
      next <- optionMaybe (try$ funGen symbol_list accu)

      case next of
        Just jnext -> do
          rec_ret <- recP jnext
          return$ rec_ret
        Nothing -> return accu

aritmeticExprGenFP =  [twoArgInFunctions5P, twoArgInFunctions4P,
    twoArgInFunctions3P, twoArgInFunctions2P, twoArgInFunctions1P]

aritmeticExprGenP :: Int -> [String] -> Parser Arg
aritmeticExprGenP n symbol_list =
  aritmeticExprNP (drop (length aritmeticExprGenFP - n) aritmeticExprGenFP)
    symbol_list


aritmeticExprP :: [String] -> Parser Arg
aritmeticExprP symbol_list = aritmeticExprGenP 5 symbol_list

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
  skipMany space
  ret <- exoticAtomP <|>stringConstantP <|>
    (try$ trueConstantP) <|> (try$ falseConstantP) <|>
    (try$ intConstantP) <|> (try$ doubleConstantP) <|>
    (try$ minusIntConstantP) <|> (try$ minusDoubleConstantP) <|>
     oneRegularAtomP
  return ret

selectAtomP :: [String] -> Parser [Arg]
selectAtomP symbol_list = do 
  skipMany space

  expr <- many1$ noneOf nonAtomChars
  let symbols = concat$ map (globMatching symbol_list)$ bracketExpand expr

  skipMany space
  return$ map Symbol symbols

minusIntConstantP :: Parser Arg
minusIntConstantP = do
  char '-'
  ret <- many1$ digit
  return$ Value$ IntValue$ negate$ read ret

intConstantP :: Parser Arg
intConstantP = do
  ret <- many1$ digit
  return$ Value$ IntValue$ read ret

minusDoubleConstantP :: Parser Arg
minusDoubleConstantP = do
  char '-'
  ret1 <- many$ digit
  ret2 <- string "."
  ret3 <- many$ digit

  return$ Value$ DoubleValue$ negate$ read$ ret1 ++ ret2 ++ ret3

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
  skipMany space
  ret <- (many1$ stringConstantP) <|> (many1$ exoticAtomP) <|>
    (many1$ dolarAritmeticExprP symbol_list) <|>
    (try$ many1$ oneArgFunctionsP symbol_list) <|>
    (try$ many1$ trueConstantP) <|> (try$ many1$ falseConstantP) <|>
    (try$ many1$ intConstantP) <|> (try$ many1$ doubleConstantP) <|>
    (try$ many1$ minusIntConstantP) <|> (try$ many1$ minusDoubleConstantP) <|>
    (selectAtomP symbol_list)
  return ret
  

selectorP :: [String] -> Parser [Arg]
selectorP symbol_list = do
  skipMany space
  ret <- many$ atomP symbol_list
  skipMany space
  return$ concat ret


  
