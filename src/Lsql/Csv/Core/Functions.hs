module Lsql.Csv.Core.Functions 
  (
    Arg(Symbol, Function, Value), 
    Function (AritmeticF, AggregateF, LogicF),

    AritmeticF(
      Sin, Cos, Tan, Asin, Acos, Atan,
      Sinh, Cosh, Tanh, Asinh, Acosh, Atanh,
      Exp, Sqrt,

      Size, ToString, Append,

      Round, Truncate, Ceiling, Floor,

      MinusS, Abs, Signum, Negate,

      Plus, Minus, Multiply, Divide, Power,

      Even, Odd,

      NaturalPower, Div, Quot, Rem, Mod, Gcd, Lcm,

      Less, LessOrEqual, More, MoreOrEqual,
      Equal, NotEqual, LeftOuterJoin, In
      ),

    LogicF(And, Or, Not),
    AggregateF(Cat, Sum),

    Printable (ColumnP, ValueP),
    getPrintableLength,

    appendArg, catterate,

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

  Size Arg | ToString Arg | Append Arg Arg | 

  Round Arg | Truncate Arg | Ceiling Arg | Floor Arg |

  MinusS Arg | Abs Arg | Signum Arg | Negate Arg |

  Plus Arg Arg | Minus Arg Arg | Multiply Arg Arg | Divide Arg Arg | 
  Power Arg Arg |
  
  Even Arg | Odd Arg | 

  NaturalPower Arg Arg | Div Arg Arg | Quot Arg Arg | Rem Arg Arg |
  Mod Arg Arg | Gcd Arg Arg | Lcm Arg Arg |

  Less Arg Arg | LessOrEqual Arg Arg | More Arg Arg | MoreOrEqual Arg Arg |
  Equal Arg Arg | NotEqual Arg Arg | LeftOuterJoin Arg Arg |
  In Arg Arg

data LogicF = And Arg Arg | Or Arg Arg | Not Arg
  

data AggregateF = Cat [Arg] | Sum [Arg] | Avg [Arg] | Count [Arg]

catterate :: [Arg] -> Arg
catterate args = foldl1 appendArg args

appendArg :: Arg -> Arg -> Arg
appendArg a b = Function$ AritmeticF$ Append a b

getPrintableLength :: [Printable] -> Int
getPrintableLength [] = 1
getPrintableLength ((ValueP p) : rest) = getPrintableLength rest
getPrintableLength ((ColumnP c) : _ ) = length$ showColumn c

eval :: SymbolMap -> Arg -> Printable
eval symbol_map (Symbol name) = ColumnP$ symbol_map ==> name
eval _ (Value val) = ValueP$ val
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

evalFunction sm (AritmeticF (Exp arg)) = applyOpP exp$ eval sm arg
evalFunction sm (AritmeticF (Sqrt arg)) = applyOpP sqrt$ eval sm arg

evalFunction sm (AritmeticF (Size arg)) = 
  applyOpP (IntValue. length. show)$ eval sm arg

evalFunction sm (AritmeticF (ToString arg)) = 
  applyOpP (StringValue. show)$ eval sm arg


evalFunction sm (AritmeticF (Round arg)) = applyOpP round$ eval sm arg
evalFunction sm (AritmeticF (Truncate arg)) = applyOpP truncate$ eval sm arg
evalFunction sm (AritmeticF (Ceiling arg)) = applyOpP ceiling$ eval sm arg
evalFunction sm (AritmeticF (Floor arg)) = applyOpP floor$ eval sm arg

evalFunction sm (AritmeticF (MinusS arg)) = applyOpP negate$ eval sm arg
evalFunction sm (AritmeticF (Negate arg)) = applyOpP negate$ eval sm arg
evalFunction sm (AritmeticF (Abs arg)) = applyOpP abs$ eval sm arg
evalFunction sm (AritmeticF (Signum arg)) = applyOpP signum$ eval sm arg

evalFunction sm (AritmeticF (Even arg)) = 
  applyOpP (BoolValue. even)$ eval sm arg
evalFunction sm (AritmeticF (Odd arg)) = 
  applyOpP (BoolValue. odd)$ eval sm arg


evalFunction sm (LogicF (Not arg)) = 
  applyOpP (BoolValue. not. getBool) $ eval sm arg

evalFunction sm (LogicF (And arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ getBool x && getBool y) 
    (eval sm arg1) (eval sm arg2)

evalFunction sm (LogicF (Or arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ getBool x || getBool y) 
    (eval sm arg1) (eval sm arg2)


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


evalFunction sm (AritmeticF (NaturalPower arg1 arg2)) = 
  applyInOpP (^^) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Div arg1 arg2)) = 
  applyInOpP (div) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Quot arg1 arg2)) = 
  applyInOpP (quot) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Rem arg1 arg2)) = 
  applyInOpP (rem) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Mod arg1 arg2)) = 
  applyInOpP (mod) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Gcd arg1 arg2)) = 
  applyInOpP (gcd) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Lcm arg1 arg2)) = 
  applyInOpP (lcm) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Append arg1 arg2)) = 
  applyInOpP (\x y -> StringValue$ show x ++ show y)
    (eval sm arg1) (eval sm arg2)


evalFunction sm (AritmeticF (Less arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x < y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (LessOrEqual arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x <= y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (More arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x > y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (MoreOrEqual arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x >= y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (NotEqual arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x == y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Equal arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x /= y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (In arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ (show x) `is_in` (show y)) 
    (eval sm arg1) (eval sm arg2)
  
  where
    cq_in :: String -> String -> Bool
    cq_in [] [] = True
    cq_in _ [] = False
    cq_in [] _ = False

    cq_in (a : rest_a) (b : rest_b)
      | a == b = cq_in rest_a rest_b
      | otherwise = False

    is_in :: String -> String -> Bool
    is_in a b 
      | cq_in a b == True = True
      | otherwise = 
          let (_ : rest_a) = a in 
          let (_ : rest_b) = b in 
          is_in rest_a rest_b


--getBool :: SymbolMap -> Function -> Column



--groupEval :: SymbolMap -> Grouping -> [AggregateF] -> (SymbolMap, [Column])
