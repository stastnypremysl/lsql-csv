module Lsql.Csv.Core.Tables
  (
    Table, Row, Column, Cell, 
    Value(IntValue, StringValue, DoubleValue, BoolValue), 
    buildTable, columnNames, showColumn
  )
where

import Data.List
import Data.Array

data Value = IntValue Int | StringValue String | DoubleValue Double | BoolValue Bool
  deriving (Eq, Ord)

instance Show Value where
  show (IntValue v) = show v
  show (StringValue v) = v
  show (DoubleValue v) = show v
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"

data Cell = Cell Row Column Value 
instance Eq Cell where
  (Cell _ _ a) == (Cell _ _ b) = a == b

instance Ord Cell where
  (Cell _ _ a) <= (Cell _ _ b) = a <= b

instance Show Cell where
  show (Cell _ _ a) = show a

data Column = Column [String] [Cell] 

data Row = Row [Cell] 

data Table = Table [String] [Row] [Column]

showColumn :: Column -> [String]
showColumn (Column _ col) =
  map show col

columnNames :: Table -> [([String], Column)]
columnNames (Table _ _ cols) =
  let names = map columnName cols in
  zip names cols

  where
    columnName :: Column -> [String]
    columnName (Column names _ ) = names


buildTable :: [String] -> [[String]] -> [[Value]] -> Table
buildTable table_names names in_data =
  Table table_names rows columns

  where
    trans_data = transpose $ in_data 

    n = length in_data
    m = length trans_data

    array_columns = array (1, m)$ zip [1..] columns
    
    columns :: [Column]
    columns = 
      let named_col_data = zip3 [1..] names trans_data in
      map tieColumn named_col_data

      where
        tieColumn :: (Int, [String], [Value]) -> Column
        tieColumn (index, c_names, vals) = Column c_names 
          [Cell (array_rows ! j ) (array_columns ! index) val | 
            (j, val) <- zip [1..] vals ]

    array_rows = array (1, n)$ zip [1..] rows

    rows :: [Row]
    rows = map tieRow$ zip [1..] in_data
      where
        tieRow :: (Int, [Value]) -> Row
        tieRow (index, vals) = Row
          [Cell (array_rows ! index) (array_columns ! j) val |
            (j, val) <- zip [1..] vals ]
      


--joinTable :: Column -> Column -> Table
--leftJoinTable :: Column -> Column -> Table
--multiplyTable :: Table -> Table -> Table

--filterTable :: (Value -> Bool) -> Column -> Table
--crossFilterTable :: (Value -> Value -> Bool) -> Column -> Column -> Table
--sortTable :: Column -> Table

--columnToTable :: Column -> Table
