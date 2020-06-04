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


readValue :: String -> Value
readValue val
  | isInteger val = IntValue (read val)
  | isDouble val = DoubleValue (read val)
  | otherwise = StringValue val
  
  where
    isInteger s = case reads s :: [(Integer, String)] of
      [(_, "")] -> True
      _         -> False
 
    isDouble s = case reads s :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

 

--buildTable :: [[String]] -> Table

--joinTable :: Column -> Column -> Table
--leftJoinTable :: Column -> Column -> Table
--multiplyTable :: Table -> Table -> Table

--filterTable :: (Value -> Bool) -> Column -> Table
--crossFilterTable :: (Value -> Value -> Bool) -> Column -> Column -> Table
--sortTable :: Column -> Table

--columnToTable :: Column -> Table
