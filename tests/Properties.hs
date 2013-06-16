module Properties (properties) where

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import MineSweeper

instance Arbitrary Event where
  arbitrary = elements [Mark, Check]

instance Arbitrary CleanCell where
  arbitrary = do
    numAdjacentMine <- elements [0..8]
    return $ CleanCell numAdjacentMine

instance Arbitrary MineCell where
  arbitrary = elements [MineCell]

instance Arbitrary CellState where
  arbitrary = undefined

isUncheckedCell :: CellState -> Bool
isUncheckedCell cs = case cs of
  Unchecked mc -> True
  otherwise -> False

prop_flagged_cell_does_not_go_to_checked_state :: Cell -> Event -> Bool
prop_flagged_cell_does_not_go_to_checked_state cell evt = 
  let flagged = Unchecked (Flag cell)
  in isUncheckedCell $ nextState flagged evt

--properties :: [Test]
properties =
  [ testProperty "flag_guards_events" prop_flagged_cell_does_not_go_to_checked_state
  ]
