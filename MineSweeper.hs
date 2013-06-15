module MineSweeper where

data Cell = Adj Int | Mine | Clear deriving Show 
data CellState c = Unchecked (Marked c) | Checked c | Boomed deriving Show
data Marked c = Nomark c | Flag c | Question c deriving Show
data Event = Mark | Check deriving Show

getCell :: Marked c -> c
getCell (Nomark c) = c
getCell (Flag c) = c
getCell (Question c) = c

nextMark :: Marked c -> Marked c
nextMark mc = case mc of
  Nomark c -> Flag c
  Flag c -> Question c
  Question c -> Nomark c

nextState :: CellState Cell -> Event -> CellState Cell
nextState cs Mark = case cs of
  Unchecked mc -> Unchecked (nextMark mc)
  otherwise -> cs

nextState cs Check
  | hasMine cs = Boomed
  | otherwise = check cs 

hasMine (Unchecked mc) = case getCell mc of
  Mine -> True
  otherwise -> False

check :: CellState c -> CellState c
check cs = case cs of
  (Unchecked mc) -> Checked (getCell mc)
  otherwise -> cs
 
