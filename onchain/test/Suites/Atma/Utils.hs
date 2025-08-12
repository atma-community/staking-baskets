module Suites.Atma.Utils (tests) where

import Suites.Atma.Utils.ExRate qualified as ExRate
import Test.Tasty qualified as Tasty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Utilities"
    [ ExRate.tests
    ]
