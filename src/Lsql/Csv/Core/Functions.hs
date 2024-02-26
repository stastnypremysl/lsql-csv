{-|
This module contains syntactic tree definition and helper functions for its evaluation.
-}
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
    AggregateF(Cat, Sum, Avg, Count, Min, Max),

    Printable (ColumnP, ValueP),
    genStrCols, getCols, getTable, printTable, unionAggCols,

    appendArg, catterate,

    eval, evalAggregateFunctions, containsAggregateF

  ) where

import Lsql.Csv.Core.Tables
import Lsql.Csv.Core.Symbols

import Data.List


-- | Syntax tree element
data Arg = 
    Function Function -- ^ Call of function
  | Symbol String -- ^ Reference to a column
  | Value Value -- ^ Constant

-- | Syntax tree element
data Function = 
  AritmeticF AritmeticF | -- ^ Arithmetic function
  AggregateF AggregateF | -- ^ Aggregate function
  LogicF LogicF -- ^ Logical function

-- | Data type for single `Column` or single `Value`
data Printable = ColumnP Column | ValueP Value
  deriving (Eq, Ord, Show)

-- | Syntax tree element 
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

-- | Syntax tree element 
data LogicF = And Arg Arg | Or Arg Arg | Not Arg
  
-- | Syntax tree element 
data AggregateF = Cat [Arg] | Sum [Arg] | Avg [Arg] | Count [Arg] | 
  Min [Arg] | Max [Arg]

pShow :: Int -> Printable -> [String]
pShow n (ValueP v) = take n$ repeat$ show v 
pShow n (ColumnP c) = take n$ showColumn c

-- | Converts list of `Printable` to list of `String` columns
-- Useful for generating CSV output
genStrCols :: [Printable] -> [[String]]
genStrCols cols = map (pShow n) cols
  where
    n = getPrintableLength cols


-- | Converts list of `Printable` to list of `Column`
getCols :: [Printable] -> [Column]
getCols printables =
  toCols printables

  where
    n = getPrintableLength printables
    
    toCols :: [Printable] -> [Column]
    toCols [] = []
    toCols ((ColumnP c) : rest) = c : (toCols rest)
    toCols ((ValueP v) : rest) = (Column [] (take n$ repeat v)) : (toCols rest)

-- | Converts list of `Printable` to a `Table`
getTable :: [String] -- ^ Names of table
         -> [Printable] -- ^ Columns of the table
         -> Table
getTable names printables = Table names (getCols printables)

-- | Converts table to list of ColumnP `Printable`s
printTable :: Table -> [Printable]
printTable (Table _ cols) = map ColumnP cols

genOnelineCols :: [Printable] -> [Printable]
genOnelineCols [] = []

genOnelineCols (ColumnP (Column _ (val : _)) : rest) = 
  (ValueP val) : (genOnelineCols rest)

genOnelineCols ((ValueP val) : rest) = (ValueP val) : (genOnelineCols rest)

printableColumnnide :: Printable -> Printable
printableColumnnide (ValueP p) = ColumnP$ Column [] [p]
printableColumnnide p = p

appendPrintable :: Printable -> Printable -> Printable
appendPrintable a0 b0 =
  let a = printableColumnnide a0 in
  let b = printableColumnnide b0 in

  doAppend a b

  where
    doAppend :: Printable -> Printable -> Printable
    doAppend (ColumnP (Column ns a)) (ColumnP (Column _ b)) =
      ColumnP (Column ns (a ++ b))


unionCols :: [[Printable]] -> [Printable]
unionCols cols 
  | null cols = []
  | otherwise = foldl1 col2union cols

  where 
    col2union :: [Printable] -> [Printable] -> [Printable]
    col2union [] [] = []
    col2union (a : rest_a) (b : rest_b) = 
      (a `appendPrintable` b) : (col2union rest_a rest_b)

-- | Unions multiple first lines of lists of [`Printable`]
-- into one `Printable`
unionAggCols :: [[Printable]] -> [Printable]
unionAggCols cols =
  unionCols$ map genOnelineCols cols


-- | Function for applying two argument function to two `Printable`s
applyInOpP :: (Value -> Value -> Value) -> Printable -> Printable -> Printable
applyInOpP op (ColumnP c1) (ColumnP c2) = 
  ColumnP$ applyInOp op c1 c2

applyInOpP op (ColumnP c1) (ValueP c2) = 
  ColumnP$ applyInOp op c1 (Column ["gen"]$ repeat c2)

applyInOpP op (ValueP c1) (ColumnP c2) =
  ColumnP$ applyInOp op (Column ["gen"]$ repeat c1) c2

applyInOpP op (ValueP c1) (ValueP c2) = ValueP$ op c1 c2

-- | Function for applying single argument function to `Printable`
applyOpP :: (Value -> Value) -> Printable -> Printable
applyOpP op (ColumnP c) = ColumnP$ applyOp op c
applyOpP op (ValueP c) = ValueP$ op c


-- | Appends list of arguments together
catterate :: [Arg] -> Arg
catterate args = foldl1 appendArg args

-- | Appends two arguments together
appendArg :: Arg -> Arg -> Arg
appendArg a b = Function$ AritmeticF$ Append a b


getPrintableLength :: [Printable] -> Int
getPrintableLength [] = 1
getPrintableLength ((ValueP p) : rest) = getPrintableLength rest
getPrintableLength ((ColumnP c) : _ ) = length$ showColumn c

-- | Evaluates all nonagregate functions to `Printable`. Fails on aggregate function.
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
  applyInOpP (\x y -> BoolValue$ x /= y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (Equal arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ x == y) (eval sm arg1) (eval sm arg2)

evalFunction sm (AritmeticF (In arg1 arg2)) = 
  applyInOpP (\x y -> BoolValue$ (show x) `is_in` (show y)) 
    (eval sm arg1) (eval sm arg2)

  where
    cq_in :: String -> String -> Bool
    cq_in [] [] = True
    cq_in _ [] = False
    cq_in [] _ = True

    cq_in (a : rest_a) (b : rest_b)
      | a == b = cq_in rest_a rest_b
      | otherwise = False

    is_in :: String -> String -> Bool
    is_in _ [] = False
    is_in [] _ = False
    is_in a b 
      | cq_in a b == True = True
      | otherwise = 
          let (_ : rest_b) = b in 
          is_in a rest_b


evalFunction sm (AggregateF _) =
  error$ "Aggregate functions can't be evaluated before grouping. " ++
    "This usually happens, when you call aggregate function from condition."
  
-- | Evaluates all aggregate functions. Normal functions are not evaluated
-- if not called under other aggregate function.
evalAggregateFunctions :: SymbolMap -> Arg -> Arg

evalAggregateFunctions symbol_map (Value val) =
  Value val

evalAggregateFunctions symbol_map (Symbol val) =
  Symbol val


evalAggregateFunctions symbol_map (Function (AritmeticF (Sin arg))) =
  Function (AritmeticF (Sin$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Cos arg))) =
  Function (AritmeticF (Cos$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Tan arg))) =
  Function (AritmeticF (Tan$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Asin arg))) =
  Function (AritmeticF (Asin$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Acos arg))) =
  Function (AritmeticF (Acos$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Atan arg))) =
  Function (AritmeticF (Atan$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Sinh arg))) =
  Function (AritmeticF (Sinh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Cosh arg))) =
  Function (AritmeticF (Cosh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Tanh arg))) =
  Function (AritmeticF (Tanh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Asinh arg))) =
  Function (AritmeticF (Asinh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Acosh arg))) =
  Function (AritmeticF (Acosh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Atanh arg))) =
  Function (AritmeticF (Atanh$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Exp arg))) =
  Function (AritmeticF (Exp$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Sqrt arg))) =
  Function (AritmeticF (Sqrt$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Size arg))) =
  Function (AritmeticF (Size$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (ToString arg))) =
  Function (AritmeticF (ToString$ evalAggregateFunctions symbol_map arg))


evalAggregateFunctions symbol_map (Function (AritmeticF (Append arg1 arg2))) =
  Function (AritmeticF (Append 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))


evalAggregateFunctions symbol_map (Function (AritmeticF (Round arg))) =
  Function (AritmeticF (Round$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Truncate arg))) =
  Function (AritmeticF (Truncate$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Ceiling arg))) =
  Function (AritmeticF (Ceiling$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Floor arg))) =
  Function (AritmeticF (Floor$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (MinusS arg))) =
  Function (AritmeticF (MinusS$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Abs arg))) =
  Function (AritmeticF (Abs$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Signum arg))) =
  Function (AritmeticF (Signum$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Negate arg))) =
  Function (AritmeticF (Negate$ evalAggregateFunctions symbol_map arg))


evalAggregateFunctions symbol_map (Function (AritmeticF (Plus arg1 arg2))) =
  Function (AritmeticF (Plus 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Minus arg1 arg2))) =
  Function (AritmeticF (Minus 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Multiply arg1 arg2))) =
  Function (AritmeticF (Multiply 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Divide arg1 arg2))) =
  Function (AritmeticF (Divide 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Power arg1 arg2))) =
  Function (AritmeticF (Power 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))


evalAggregateFunctions symbol_map (Function (AritmeticF (Even arg))) =
  Function (AritmeticF (Even$ evalAggregateFunctions symbol_map arg))

evalAggregateFunctions symbol_map (Function (AritmeticF (Odd arg))) =
  Function (AritmeticF (Odd$ evalAggregateFunctions symbol_map arg))


evalAggregateFunctions symbol_map (Function (AritmeticF (NaturalPower arg1 arg2))) =
  Function (AritmeticF (NaturalPower 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Div arg1 arg2))) =
  Function (AritmeticF (Div 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Quot arg1 arg2))) =
  Function (AritmeticF (Quot 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Rem arg1 arg2))) =
  Function (AritmeticF (Rem 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Mod arg1 arg2))) =
  Function (AritmeticF (Mod 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Gcd arg1 arg2))) =
  Function (AritmeticF (Gcd 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Lcm arg1 arg2))) =
  Function (AritmeticF (Lcm 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Less arg1 arg2))) =
  Function (AritmeticF (Less 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (LessOrEqual arg1 arg2))) =
  Function (AritmeticF (LessOrEqual 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (More arg1 arg2))) =
  Function (AritmeticF (More 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (MoreOrEqual arg1 arg2))) =
  Function (AritmeticF (MoreOrEqual 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (Equal arg1 arg2))) =
  Function (AritmeticF (Equal 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (NotEqual arg1 arg2))) =
  Function (AritmeticF (NotEqual 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (LeftOuterJoin arg1 arg2))) =
  Function (AritmeticF (LeftOuterJoin 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (AritmeticF (In arg1 arg2))) =
  Function (AritmeticF (In 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))



evalAggregateFunctions symbol_map (Function (LogicF (And arg1 arg2))) =
  Function (LogicF (And 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (LogicF (Or arg1 arg2))) =
  Function (LogicF (Or 
    (evalAggregateFunctions symbol_map arg1) (evalAggregateFunctions symbol_map arg2)))

evalAggregateFunctions symbol_map (Function (LogicF (Not arg))) =
  Function (LogicF (Not$ evalAggregateFunctions symbol_map arg))


evalAggregateFunctions symbol_map (Function (AggregateF (Cat args))) =
  Value$ doCat evaled

  where 
    evaled :: Printable
    evaled = eval symbol_map (catterate args)

    doCat :: Printable -> Value
    doCat (ValueP value) = value
    doCat (ColumnP (Column _ vals)) = StringValue$ concat$ map show vals

evalAggregateFunctions symbol_map (Function (AggregateF (Sum args))) =
  Value$ doSum evaled

  where 
    evaled :: Printable
    evaled = eval symbol_map$ foldl1 
      (\x y -> Function$ AritmeticF$ Plus x y)$ args

    doSum :: Printable -> Value
    doSum (ValueP value) = error "You cannot sum constant."
    doSum (ColumnP (Column _ vals)) 
      | null vals = 0
      | otherwise = foldl1 (+) vals

evalAggregateFunctions symbol_map (Function (AggregateF (Count args))) 
  | null evaled = Value$ IntValue 0
  | otherwise = Value$ foldl1 (+)$ map doCount evaled

  where 
    evaled :: [Printable]
    evaled = map (eval symbol_map) args

    doCount :: Printable -> Value
    doCount (ValueP value) = error "You cannot count constant"
    doCount (ColumnP (Column _ vals)) = IntValue$ length vals

evalAggregateFunctions symbol_map (Function (AggregateF (Avg args))) =
  Value$ doAvg evaled

  where 
    evaled :: Printable
    evaled = eval symbol_map$
      evalAggregateFunctions symbol_map$ Function$ AritmeticF$ 
      Divide (Function$ AggregateF$ Sum$ args) (Function$ AggregateF$ Count$ args)

    doAvg :: Printable -> Value
    doAvg (ValueP value) = value

evalAggregateFunctions symbol_map (Function (AggregateF (Min args))) =
  Value$ minimum$ map doMin evaled

  where 
    evaled :: [Printable]
    evaled = map (eval symbol_map) args

    doMin :: Printable -> Value
    doMin (ValueP value) = value
    doMin (ColumnP (Column _ vals)) = minimum vals


evalAggregateFunctions symbol_map (Function (AggregateF (Max args))) =
  Value$ maximum$ map doMax evaled

  where 
    evaled :: [Printable]
    evaled = map (eval symbol_map) args

    doMax :: Printable -> Value
    doMax (ValueP value) = value
    doMax (ColumnP (Column _ vals)) = maximum vals
   

--evalAggregateFunctions _ x = x

-- | Runs through the syntactic tree and check, whether it contains 
-- aggregate function.
containsAggregateF :: Arg -> Bool
containsAggregateF (Function (AritmeticF (Sin arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Cos arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Tan arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Asin arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Acos arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Atan arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (Sinh arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Cosh arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Tanh arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Asinh arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Acosh arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Atanh arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (Exp arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Sqrt arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (Size arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (ToString arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Append arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2

containsAggregateF (Function (AritmeticF (Round arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Truncate arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Ceiling arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Floor arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (MinusS arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Abs arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Signum arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Negate arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (Plus arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Minus arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Multiply arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Divide arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Power arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2

containsAggregateF (Function (AritmeticF (Even arg))) = containsAggregateF arg
containsAggregateF (Function (AritmeticF (Odd arg))) = containsAggregateF arg

containsAggregateF (Function (AritmeticF (NaturalPower arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Div arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Quot arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Rem arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Mod arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Gcd arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Lcm arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2

containsAggregateF (Function (AritmeticF (Less arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (LessOrEqual arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (More arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (MoreOrEqual arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (Equal arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (NotEqual arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (LeftOuterJoin arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (AritmeticF (In arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2

containsAggregateF (Function (LogicF (And arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (LogicF (Or arg1 arg2))) = containsAggregateF arg1 || containsAggregateF arg2
containsAggregateF (Function (LogicF (Not arg))) = containsAggregateF arg

containsAggregateF (Function (AggregateF _)) = True

containsAggregateF _ = False
