module Properties (properties) where

import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Char

prop_nothing :: String -> Bool
prop_nothing = \_ -> True

--properties :: [Test]
properties =
  [ testProperty "dumb" prop_nothing
  ]
