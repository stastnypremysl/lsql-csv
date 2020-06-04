module Tables
  (Table, Row, Column, Cell, 
    Value(IntValue, StringValue, DoubleValue), 
    buildTable, columnNames) 

where

import Data.List
import Data.Array

data Value = IntValue Int | StringValue String | DoubleValue Double 
  deriving (Show, Eq, Ord)

data Cell = Cell Row Column Value 
instance Eq Cell where
  (Cell _ _ a) == (Cell _ _ b) = a == b

instance Ord Cell where
  (Cell _ _ a) <= (Cell _ _ b) = a <= b

data Column = Column [String] [Cell] 

data Row = Row [Cell] 

data Table = Table [Row] [Column]

columnNames :: Table -> [([String], Column)]
columnNames (Table _ cols) =
  let names = map columnName cols in
  zip names cols

  where
    columnName :: Column -> [String]
    columnName (Column names _ ) = names


buildTable :: [[String]] -> [[Value]] -> Table
buildTable names in_data =
  Table rows columns

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
        tieColumn (index, names, vals) = Column names 
          [Cell (array_rows ! j ) (array_columns ! index) val | (j, val) <- zip [1..] vals ]

    array_rows = array (1, n)$ zip [1..] rows

    rows :: [Row]
    rows = map tieRow$ zip [1..] in_data
      where
        tieRow :: (Int, [Value]) -> Row
        tieRow (index, vals) = Row
          [Cell (array_rows ! index) (array_columns ! j) val | (j, val) <- zip [1..] vals ]
      


--joinTable :: Column -> Column -> Table
--leftJoinTable :: Column -> Column -> Table
--multiplyTable :: Table -> Table -> Table

--filterTable :: (Value -> Bool) -> Column -> Table
--crossFilterTable :: (Value -> Value -> Bool) -> Column -> Column -> Table
--sortTable :: Column -> Table

--columnToTable :: Column -> Table
