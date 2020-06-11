module Lsql.Csv.Core.Functions 
  (
    Arg(Symbol, Function, Value), 
    Function (AritmeticF, AggregateF, LogicF),

    AritmeticF(
      Sin, Cos, Tan, Asin, Acos, Atan,
      Sinh, Cosh, Tanh, Asinh, Acosh, Atanh,

      MinusS, Plus, Minus, Multiply, Divide, Power, Append,
      Count, Less, LessOrEqual, More, MoreOrEqual,
      Equal, LeftOuterJoin, In),

    LogicF(And, Or, Not),
    AggregateF(Cat, Sum),

    Printable (ColumnP, ValueP),
    getPrintableLength,

    eval

  ) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Symbols

import Data.List

data Function = AritmeticF AritmeticF | AggregateF AggregateF | LogicF LogicF
data Arg = Function Function | Symbol String | Value Value 

data Printable = ColumnP Column | ValueP Value

applyInOpP :: (Value -> Value -> Value) -> Printable -> Printable -> Printable
applyInOpP op (ColumnP c1) (ColumnP c2) = 
  ColumnP$ applyInOp op c1 c2

applyInOpP op (ColumnP c1) (ValueP c2) = 
  ColumnP$ applyInOp op c1 (Column ["gen"]$ repeat c2)

applyInOpP op (ValueP c1) (ColumnP c2) =
  ColumnP$ applyInOp op (Column ["gen"]$ repeat c1) c2

applyInOpP op (ValueP c1) (ValueP c2) = ValueP$ op c1 c2

applyOpP :: (Value -> Value) -> Printable -> Printable
applyOpP op (ColumnP c) = ColumnP$ applyOp op c
applyOpP op (ValueP c) = ValueP$ op c


data Grouping = By [Arg]

data AritmeticF = 
  Sin Arg | Cos Arg | Tan Arg | Asin Arg | Acos Arg | Atan Arg|
  Sinh Arg | Cosh Arg | Tanh Arg | Asinh Arg | Acosh Arg | Atanh Arg|
  Exp Arg | Sqrt Arg |
  MinusS Arg | Abs Arg | Signum Arg | Negate Arg |
  Plus Arg Arg | Minus Arg Arg | Multiply Arg Arg | Divide Arg Arg | Power Arg Arg |
  Append Arg Arg | Count Arg |
  Less Arg Arg | LessOrEqual Arg Arg | More Arg Arg | MoreOrEqual Arg Arg |
  Equal Arg Arg| LeftOuterJoin Arg Arg |
  In Arg Arg

data LogicF = And Arg Arg | Or Arg Arg | Not Arg
  

data AggregateF = Cat [Arg] | Sum [Arg]

getPrintableLength :: [Printable] -> Int
getPrintableLength [] = 1
getPrintableLength ((ValueP p) : rest) = getPrintableLength rest
getPrintableLength ((ColumnP c) : _ ) = length$ showColumn c

eval :: SymbolMap -> Arg -> Printable
eval symbol_map (Symbol name) = ColumnP$ symbol_map ==> name
eval symbol_map (Value val) = ValueP$ val
eval symbol_map (Function f) = evalFunction symbol_map f

evalFunction :: SymbolMap -> Function -> Printable

evalFunction sm (AritmeticF (Sin arg)) = applyOpP sin$ eval sm arg
evalFunction sm (AritmeticF (Cos arg)) = applyOpP cos$ eval sm arg
evalFunction sm (AritmeticF (Tan arg)) = applyOpP tan$ eval sm arg
evalFunction sm (AritmeticF (Asin arg)) = applyOpP asin$ eval sm arg
evalFunction sm (AritmeticF (Acos arg)) = applyOpP acos$ eval sm arg
evalFunction sm (AritmeticF (Atan arg)) = applyOpP atan$ eval sm arg

evalFunction sm (AritmeticF (Sinh arg)) = applyOpP sinh$ eval sm arg
evalFunction sm (AritmeticF (Cosh arg)) = applyOpP cosh$ eval sm arg
evalFunction sm (AritmeticF (Tanh arg)) = applyOpP tanh$ eval sm arg
evalFunction sm (AritmeticF (Asinh arg)) = applyOpP asinh$ eval sm arg
evalFunction sm (AritmeticF (Acosh arg)) = applyOpP acosh$ eval sm arg
evalFunction sm (AritmeticF (Atanh arg)) = applyOpP atanh$ eval sm arg

evalFunction sm (AritmeticF (MinusS arg)) = applyOpP negate$ eval sm arg

evalFunction sm (AritmeticF (Plus arg1 arg2)) = 
  applyInOpP (+) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Minus arg1 arg2)) = 
  applyInOpP (-) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Multiply arg1 arg2)) = 
  applyInOpP (*) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Divide arg1 arg2)) = 
  applyInOpP (/) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Power arg1 arg2)) = 
  applyInOpP (**) (eval sm arg1) (eval sm arg2)



--getBool :: SymbolMap -> Function -> Column



--groupEval :: SymbolMap -> Grouping -> [AggregateF] -> (SymbolMap, [Column])
