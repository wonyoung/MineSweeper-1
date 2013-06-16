module MineSweeper where

newtype CleanCell = CleanCell { adjMine :: Int }  deriving (Show, Eq)
data MineCell = MineCell deriving (Show, Eq)
type Cell = Either MineCell CleanCell

-- disables 'Checked MineCell' at type level which corresponds to 'Boomed' type
data CellState = Unchecked (Marked Cell) | Checked CleanCell | Boomed deriving (Show, Eq)
data Marked c = Nomark c | Flag c | Question c deriving (Show, Eq)
data Event = Mark | Check deriving (Show, Eq)

getCell :: Marked c -> c
getCell (Nomark c) = c
getCell (Flag c) = c
getCell (Question c) = c

nextState :: CellState -> Event -> CellState
nextState cs Check = case cs of
  -- at first, I've missed this case: Flagged Cell should not receive Check event.
  Unchecked (Flag c) -> cs
  Unchecked mc -> case getCell mc of
                  Left MineCell -> Boomed
                  Right cc -> Checked cc
  otherwise -> cs

nextState cs Mark = case cs of
  Unchecked mc -> Unchecked (nextMark mc)
  otherwise -> cs

nextMark :: Marked c -> Marked c
nextMark mc = case mc of
  Nomark c -> Flag c
  Flag c -> Question c
  Question c -> Nomark c

