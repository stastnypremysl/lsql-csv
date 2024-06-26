{-|
This module contains the definition of `Value`, `Table`, and `Column`, class instancies over them, functions for manipulation of them, and `Boolable` class definition.
-}
module Lsql.Csv.Core.Tables
  (
    Table(Table), Column(Column),
    Value(IntValue, StringValue, DoubleValue, BoolValue), 

    buildTable, crossJoinTable, filterTable, sortTable, byTable,
    emptyTable, 

    columnNames, columnValue, showColumn,
    applyOp, applyInOp,

    Boolable(getBool)
  )
where

import Data.List

-- | Class for converting a value to a `Bool`
class Boolable a where
  getBool :: a -> Bool

-- | The representation of data in `Table`s
data Value = IntValue Int | StringValue String | DoubleValue Double | BoolValue Bool

instance Boolable Value where
  getBool (IntValue v) = v /= 0
  getBool (DoubleValue d) = error$ "Double " ++ show d ++" can't be converted to bool."
  getBool (BoolValue b) = b
  getBool (StringValue s)
   | s == "true" = True
   | s == "false" = False
   | otherwise = error$ "String " ++ s ++ " can't be converted to bool."

instance Ord Value where
  (IntValue a) <= (IntValue b) = a <= b
  (DoubleValue a) <= (IntValue b) = a <= fromIntegral b
  (IntValue a) <= (DoubleValue b) = fromIntegral a <= b
  (DoubleValue a) <= (DoubleValue b) = a <= b
  a <= b = (show a) <= (show b)

instance Eq Value where
  (IntValue a) == (BoolValue b) = b == (a > 0)
  (StringValue a) == (BoolValue b) = a == show b

  (IntValue a) == (IntValue b) = a == b
  (StringValue a) == (StringValue b) = a == b
  (BoolValue a) == (BoolValue b) = a == b
  (DoubleValue a) == (DoubleValue b) = a == b

  _ == _ = False

instance Real Value where
  toRational (IntValue a) = toRational a
  toRational (DoubleValue a) = toRational a
  toRational x = error$ "Aritmetic operations with non-number " ++ show x ++ " is not supported."

instance RealFrac Value where
  properFraction (IntValue a) = (fromIntegral a, IntValue$ 0)
  properFraction (DoubleValue a) = 
    let (n,f) = properFraction a in
    (n, DoubleValue$ f)

  properFraction x = error$ "Aritmetic operations with non-number " ++ show x ++ " is not supported."

instance Enum Value where
  toEnum a = IntValue$ toEnum a

  fromEnum (IntValue a) = fromEnum a
  fromEnum x = error$ "Integer operations with non-integer " ++ show x ++ " is not supported."


instance Integral Value where
  toInteger (IntValue a) = toInteger a
  toInteger x = error$ "Integer operations on non-integer " ++ show x ++ " is not supported."

  quotRem (IntValue a) (IntValue b) =
    let (x,y) = quotRem a b in
    (IntValue x, IntValue y)

  quotRem x y = error$ "Integer operations on non-integer " ++ show x ++ " or " ++ show y ++" is not supported."

instance Num Value where
  (IntValue a) + (IntValue b) = IntValue$ a + b
  (DoubleValue a) + (DoubleValue b) = DoubleValue$ a + b
  (IntValue a) + (DoubleValue b) = DoubleValue$ fromIntegral a + b
  (DoubleValue a) + (IntValue b) = DoubleValue$ a + fromIntegral b
  x + y = error$ "+ operation on non-number " ++ show x ++ " or " ++ show y ++ " is not supported."

  (IntValue a) * (IntValue b) = IntValue$ a * b
  (DoubleValue a) * (DoubleValue b) = DoubleValue$ a * b
  (IntValue a) * (DoubleValue b) = DoubleValue$ fromIntegral a * b
  (DoubleValue a) * (IntValue b) = DoubleValue$ a * fromIntegral b
  x * y = error$ "* operation on non-number " ++ show x ++ " or " ++ show y ++ " is not supported."

  abs (IntValue a) = IntValue$ abs a
  abs (DoubleValue a) = DoubleValue$ abs a
  abs x = error$ "abs operation on non-number " ++ show x ++ " is not supported."

  signum (IntValue a) = IntValue$ signum a
  signum (DoubleValue a) = DoubleValue$ signum a
  signum x = error$ "signum operation on non-number " ++ show x ++ " is not supported."

  fromInteger a = IntValue$ fromInteger a

  negate (IntValue a) = IntValue$ -a
  negate (DoubleValue a) = DoubleValue$ -a
  negate x = error$ "negate operation on non-number " ++ show x ++ " is not supported."

instance Fractional Value where
  (IntValue a) / (IntValue b) = DoubleValue$
    (fromIntegral a :: Double) / (fromIntegral b :: Double)

  (DoubleValue a) / (IntValue b) = DoubleValue$ a / (fromIntegral b :: Double)
  (IntValue a) / (DoubleValue b) = DoubleValue$ (fromIntegral a :: Double) / b
  (DoubleValue a) / (DoubleValue b) = DoubleValue$ a/b

  fromRational a = DoubleValue$ fromRational a

instance Floating Value where
  pi = DoubleValue$ pi

  exp (DoubleValue a) = DoubleValue$ exp a
  exp (IntValue a) = DoubleValue$ exp$ fromIntegral a
  exp x = error$ "exp operation on non-number " ++ show x ++ " is not supported."

  log (DoubleValue a) = DoubleValue$ log a
  log (IntValue a) = DoubleValue$ log$ fromIntegral a
  log x = error$ "log operation on non-number " ++ show x ++ " is not supported."

  sin (DoubleValue a) = DoubleValue$ sin a
  sin (IntValue a) = DoubleValue$ sin$ fromIntegral a
  sin x = error$ "sin operation on non-number " ++ show x ++ " is not supported."

  cos (DoubleValue a) = DoubleValue$ cos a
  cos (IntValue a) = DoubleValue$ cos$ fromIntegral a
  cos x = error$ "cos operation on non-number " ++ show x ++ " is not supported."

  asin (DoubleValue a) = DoubleValue$ asin a
  asin (IntValue a) = DoubleValue$ asin$ fromIntegral a
  asin x = error$ "asin operation on non-number " ++ show x ++ " is not supported."

  acos (DoubleValue a) = DoubleValue$ acos a
  acos (IntValue a) = DoubleValue$ acos$ fromIntegral a
  acos x = error$ "acos operation on non-number " ++ show x ++ " is not supported."

  atan (DoubleValue a) = DoubleValue$ atan a
  atan (IntValue a) = DoubleValue$ atan$ fromIntegral a
  atan x = error$ "atan operation on non-number " ++ show x ++ " is not supported."

  sinh (DoubleValue a) = DoubleValue$ sinh a
  sinh (IntValue a) = DoubleValue$ sinh$ fromIntegral a
  sinh x = error$ "sinh operation on non-number " ++ show x ++ " is not supported."

  asinh (DoubleValue a) = DoubleValue$ asinh a
  asinh (IntValue a) = DoubleValue$ asinh$ fromIntegral a
  asinh x = error$ "asinh operation on non-number " ++ show x ++ " is not supported."

  cosh (DoubleValue a) = DoubleValue$ cosh a
  cosh (IntValue a) = DoubleValue$ cosh$ fromIntegral a
  cosh x = error$ "cosh operation on non-number " ++ show x ++ " is not supported."

  acosh (DoubleValue a) = DoubleValue$ acosh a
  acosh (IntValue a) = DoubleValue$ acosh$ fromIntegral a
  acosh x = error$ "acosh operation on non-number " ++ show x ++ " is not supported."
  
  atanh (DoubleValue a) = DoubleValue$ atanh a
  atanh (IntValue a) = DoubleValue$ atanh$ fromIntegral a
  atanh x = error$ "atanh operation on non-number " ++ show x ++ " is not supported."

instance Show Value where
  show (IntValue v) = show v
  show (StringValue v) = v
  show (DoubleValue v) = show v
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"

-- | A single column of a table
data Column = Column 
  [String] -- ^ The names of the column
  [Value] -- ^ The values of the column

instance Eq Column where
  (Column _ a) == (Column _ b) = a == b

instance Ord Column where
  (Column _ a) <= (Column _ b) = a <= b

instance Show Column where
  show (Column _ a) = show a


-- | A function for applying a two-argument function to two `Column`s
applyInOp:: (Value -> Value -> Value) -> Column -> Column -> Column
applyInOp op (Column _ a) (Column _ b) = (Column ["comp"] (map (\(x,y) -> op x y)$ zip a b))


-- | A function for applying a single argument function to a `Column`
applyOp:: (Value -> Value) -> Column -> Column
applyOp op (Column _ a) = (Column ["comp"] (map op a))


-- | A single table of data
data Table = 
  Table 
  [String] -- ^ The table names 
  [Column] -- ^ The columns of the table

instance Show Table where
  show (Table _ a) = show a

-- | Converts `Column` to the list of `String` from its data.
showColumn :: Column -> [String]
showColumn (Column _ col) =
  map show col

-- | Returns all names of a `Column`.
columnName :: Column -> [String]
columnName (Column names _ ) = names

-- | Returns all values of a `Column`.
columnValue :: Column -> [Value]
columnValue (Column _ values ) = values

-- | Returns pairs of names of `Column` and `Column` itself of the table.
columnNames :: Table -> [([String], Column)]
columnNames (Table _ cols) =
  let names = map columnName cols in
  zip names cols


-- | Makes a table out of rows of `Value`.
buildTable :: 
       [String] -- ^ The names of the table
    -> [[String]] -- ^ The names of the columns
    -> [[Value]] -- ^ The rows of the table
    -> Table -- ^ The result table

buildTable table_names names in_data =
  if in_data /= [] then
    Table table_names columns
  else
    Table table_names$ map (\c_names -> Column c_names []) names

  where
    trans_data = transpose $ in_data 

    n = length in_data
    m = length trans_data

    columns :: [Column]
    columns = 
      let named_col_data = zip names trans_data in
      map tieColumn named_col_data

      where
        tieColumn :: ([String], [Value]) -> Column
        tieColumn (c_names, vals) = Column c_names vals

getRows :: [Column] -> [[Value]]
getRows cols =
  transpose$ map columnValue cols

-- | Cross joins two `Table`s into one.
crossJoinTable :: Table -> Table -> Table
crossJoinTable (Table names1 cols1) (Table names2 cols2) =
  buildTable tableName colsNames$
    [row1 ++ row2 | row1 <- rows1, row2 <- rows2]

  where
    tableName :: [String]
    tableName = names1 ++ names2
   
    colsNames :: [[String]]
    colsNames = map columnName cols1 ++ map columnName cols2

    rows1 :: [[Value]]
    rows1 = getRows cols1

    rows2 :: [[Value]]
    rows2 = getRows cols2


-- | Filters out rows, where the `Column` is `False`. 
-- The rows, where the `Column` is `True`, are kept.
filterTable :: Column -> Table -> Table
filterTable (Column _ if_cols) (Table t_name cols) =
  buildTable t_name cols_name$
    filterRows (map getBool if_cols) rows

  where
    rows :: [[Value]]
    rows = getRows cols

    cols_name = map columnName cols

    filterRows :: [Bool] -> [[Value]] -> [[Value]]
    filterRows [] [] = []
    filterRows (False : r_bool) (_ : r_rows) = filterRows r_bool r_rows
    filterRows (True : r_bool) (row : r_rows) = row : (filterRows r_bool r_rows)

-- | Returns `Table` with same metadata as the original `Table`, but no data (no rows).
emptyTable :: Table -> Table
emptyTable (Table t_name cols) = Table t_name 
  [Column (columnName col) [] | col <- cols]


-- | Sorts a `Table` according to given `Column`s.
sortTable :: [Column] -> Table -> Table
sortTable [] table = table

sortTable s_cols (Table name cols) =
  buildTable name (map columnName cols)$
    sorted_rows
  
  where
    s_rows :: [[Value]]
    s_rows = getRows s_cols

    rows :: [[Value]]
    rows = getRows cols

    sorted_p :: [([Value], [Value])]
    sorted_p = sort$ zip s_rows rows

    sorted_rows :: [[Value]]
    sorted_rows = map snd sorted_p


-- | Splits a `Table` into multiple `Table`s so that
-- rows of `Column`s at first argument are at each `Table` the same
-- and the number of `Table`s is minimal. (factorization)
byTable :: [Column] -> Table -> [Table]
byTable s_cols orig_table =
  map (buildTable name (map columnName orig_cols))
    new_rows

  where
    Table name orig_cols = orig_table

    s_rows :: [[Value]]
    s_rows = getRows s_cols

    rows :: [[Value]]
    rows = getRows orig_cols

    sorted_p :: [([Value], [Value])]
    sorted_p = sort$ zip s_rows rows

    groupF :: ([Value], [Value]) -> ([Value], [Value]) -> Bool
    groupF (a, _) (b, _) = a == b

    grouped_p :: [[([Value], [Value])]]
    grouped_p = groupBy groupF sorted_p

    new_rows :: [[[Value]]]
    new_rows = map (map snd) grouped_p


 
