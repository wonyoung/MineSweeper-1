module MineSweeper where

import Data.Char 

newtype CleanCell = CleanCell { adjMine :: Int }  deriving (Eq)
data MineCell = MineCell deriving (Eq)
type Cell = Either MineCell CleanCell

-- disables 'Checked MineCell' at type level which corresponds to 'Boomed' type
data CellState = Unchecked (Marked Cell) | Checked CleanCell | Boomed deriving (Eq)
data Marked c = Nomark c | Flag c | Question c deriving (Eq)
data Event = Mark | Check deriving (Show, Eq)

type Board = [[CellState]]

instance Show CleanCell where
  show c = show $ adjMine c

instance Show MineCell where
  show m = "*"

instance Show c => Show (Marked c) where
  show (Nomark c) = "_"
  show (Flag c) = "F"
  show (Question c) = "?"

instance Show CellState where
  show (Unchecked mc) = show mc
  show (Checked c) = show c
  show Boomed = "X"

getCell :: Marked c -> c
getCell (Nomark c) = c
getCell (Flag c) = c
getCell (Question c) = c

nextState :: CellState -> Event -> CellState
nextState cs Check = case cs of
  -- Previously, I've missed this case: Flagged Cell should not receive Check event.
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

