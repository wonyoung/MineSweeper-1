module ConsoleDriver where

import MineSweeper
import Data.List (intercalate)

data GameState = Lose | Win | Playing deriving (Eq)

board = genBoard [
  " 1 2 * 1 0 1 1 1",
  " * 2 1 1 0 2 * 2",
  " 2 3 2 1 0 2 * 2",
  " 1 * * 1 1 2 2 1",
  " 2 3 2 1 1 * 1 0",
  " * 1 0 0 1 1 1 0",
  " 2 3 1 1 0 0 0 0",
  " * 2 * 1 0 0 0 0"]

run = do
  putStrLn "+++ Welcome to MineSweeper! +++\n"
  putStrLn "Here are two commands."
  putStrLn "    C x y: Check (x, y)"
  putStrLn "    M x y: Mark (x, y)\n"
  repl board

repl b = do
  printBoard b
  let gamestate = getGameState b 
  case gamestate of
    Lose -> putStrLn "Lose!"
    Win -> putStrLn "Win!"
    Playing -> putStrLn "Type command"
  cmd <- getLine
  case words cmd of
    "C":x:[y] -> repl $ applyEvent b Check (read x, read y)
    "M":x:[y] -> repl $ applyEvent b Mark (read x, read y)
    otherwise -> do putStrLn "Unknown command"
                    repl b

applyEvent:: Board -> Event -> (Int, Int) -> Board
applyEvent board evt (px, py) =
  let (x, xRest) = splitAt (px - 1) board in
  let (y, yRest) = splitAt (py - 1) (head xRest) in
  let newState = nextState (head yRest) evt in
  x ++ [y ++ [newState] ++ tail yRest] ++ tail xRest
      
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


printBoard :: Board -> IO()
printBoard b = putStrLn $ intercalate "\n" (map show b)

genBoard :: [String] -> [[CellState]]
genBoard b = map ((map str2cellstate) . words) b

str2cellstate :: String -> CellState
str2cellstate "*" = Unchecked (Nomark (Left MineCell))
str2cellstate numstr = Unchecked (Nomark (Right (CleanCell (read numstr))))




