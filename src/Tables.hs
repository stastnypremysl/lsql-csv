module Tables() where

data Value = IntValue Int | StringValue String | DoubleValue Double 
  deriving (Show, Eq, Ord)
type ColumnNames = [String]


data Cell = Cell Row Column Value 
  deriving (Show, Eq, Ord)

data Column = Column [String] [Cell] 
  deriving (Show, Eq, Ord)

data Row = Row [Cell] 
  deriving (Show, Eq, Ord)

data Table = Table [Row] [Column] deriving (Show)

--buildTable :: [[String]] -> Table

--joinTable :: Column -> Column -> Table
--leftJoinTable :: Column -> Column -> Table
--multiplyTable :: Table -> Table -> Table

--filterTable :: (Value -> Bool) -> Column -> Table
--crossFilterTable :: (Value -> Value -> Bool) -> Column -> Column -> Table
--sortTable :: Column -> Table

--columnToTable :: Column -> Table
