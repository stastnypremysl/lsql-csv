module Lsql.Csv.Core.Functions 
  (
    Arg(Symbol, Function, Value), 
    Function (AritmeticF, AggregateF, LogicF),

    AritmeticF(Sin, Cos, Tan, Arcsin, Arccos, Arctan,
      MinusS, Plus, Minus, Multiply, Divide, Power, Append,
      Count, Less, LessOrEqual, More, MoreOrEqual,
      Equal, LeftOuterJoin, In),

    LogicF(And, Or, Not),
    AggregateF(Cat, Sum),

    eval

  ) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Symbols

data Function = AritmeticF AritmeticF | AggregateF AggregateF | LogicF LogicF
data Arg = Function Function | Symbol String | Value Value 

data Grouping = By [Arg]

data AritmeticF = 
  Sin Arg | Cos Arg | Tan Arg | Arcsin Arg | Arccos Arg | Arctan Arg|
  MinusS Arg |
  Plus Arg Arg | Minus Arg Arg | Multiply Arg Arg | Divide Arg Arg | Power Arg Arg |
  Append Arg Arg | Count Arg |
  Less Arg Arg | LessOrEqual Arg Arg | More Arg Arg | MoreOrEqual Arg Arg |
  Equal Arg Arg| LeftOuterJoin Arg Arg |
  In Arg Arg

data LogicF = And Arg Arg | Or Arg Arg | Not Arg
  

data AggregateF = Cat [Arg] | Sum [Arg]

eval :: SymbolMap -> Arg -> Column
eval symbol_map (Symbol name) = symbol_map ==> name


--groupEval :: SymbolMap -> Grouping -> [AggregateF] -> (SymbolMap, [Column])
