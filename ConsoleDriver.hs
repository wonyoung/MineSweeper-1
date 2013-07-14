module ConsoleDriver where

import MineSweeper
import Data.List (intercalate)


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
    Lose -> do putStrLn "Lose!"
               return Lose
    Win -> do putStrLn "Win!"
              return Win
    Playing -> do  
      putStrLn "Type command"
      cmd <- getLine
      case words cmd of
       "C":x:[y] -> repl $ applyEvent b Check (read x, read y)
       "M":x:[y] -> repl $ applyEvent b Mark (read x, read y)
       otherwise -> do putStrLn "Unknown command"
                       repl b

printBoard :: Board -> IO()
printBoard b = putStrLn $ intercalate "\n" (map show b)

genBoard :: [String] -> [[CellState]]
genBoard b = map ((map str2cellstate) . words) b

str2cellstate :: String -> CellState
str2cellstate "*" = Unchecked (Nomark (Left MineCell))
str2cellstate numstr = Unchecked (Nomark (Right (CleanCell (read numstr))))




