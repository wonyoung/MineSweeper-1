module MineSweeper where

import Data.Char 

newtype CleanCell = CleanCell { adjMine :: Int }  deriving (Eq)
data MineCell = MineCell deriving (Eq)
type Cell = Either MineCell CleanCell

-- disables 'Checked MineCell' at type level which corresponds to 'Boomed' type
data CellState = Unchecked (Marked Cell) | Checked CleanCell | Boomed deriving (Eq)
data Marked c = Nomark c | Flag c | Question c deriving (Eq)
data Event = Mark | Check deriving (Show, Eq)
data GameState = Lose | Win | Playing deriving (Eq)

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

applyEvent:: Board -> Event -> (Int, Int) -> Board
applyEvent board evt (px, py)
  | not (inRange board px py) = board
  | otherwise = 
    let (x, xRest) = splitAt (px - 1) board in
    let (y, yRest) = splitAt (py - 1) (head xRest) in
    let newState = nextState (head yRest) evt in
    x ++ [y ++ [newState] ++ tail yRest] ++ tail xRest
  where inRange b x y = x > 0 && x <= length b && 
                        y > 0 && y <= length b
      
isLose :: Board -> Bool
isLose b = not . null $ filter (== Boomed) $ concat b

-- if there's no Unchecked (Marked CleanCell) and no Boomed state, then game win
isWin :: Board -> Bool
isWin b = (not $ isLose b) && (null $ filter isUncheckedCleanCell $ concat b)
  where isUncheckedCleanCell (Unchecked mc) 
          | isCleanCell mc = True
          | otherwise = False
        isUncheckedCleanCell (Checked c) = False
        isUncheckedCleanCell Boomed = False
        isCleanCell mc = case getCell mc of
          Left MineCell -> False
          otherwise -> True

getGameState:: Board -> GameState
getGameState b 
 | isLose b  = Lose
 | isWin b = Win
 | otherwise = Playing


