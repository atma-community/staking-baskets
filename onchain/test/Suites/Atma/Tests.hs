module Suites.Atma.Tests (tests) where

import Test.Tasty qualified as Tasty

import Suites.Atma.Common.Utils (Scripts)
import Suites.Atma.Deposit.Tests qualified as Deposit
import Suites.Atma.Donate.Tests qualified as Donate
import Suites.Atma.Initialize.Tests qualified as Initialize
import Suites.Atma.OneShotMintingPolicy.Tests qualified as OneShotMintingPolicy
import Suites.Atma.Rebalancing.Tests qualified as Rebalance
import Suites.Atma.SetAdmin.Tests qualified as SetAdmin
import Suites.Atma.SwitchLock.Tests qualified as SwitchLock
import Suites.Atma.UpdateExRate.Tests qualified as UpdateExRate
import Suites.Atma.Withdraw.Tests qualified as Withdraw
import System.Random qualified as Random

tests :: forall (g :: Type). Random.RandomGen g => g -> Scripts -> Tasty.TestTree
tests gen s =
  Tasty.testGroup
    "Scripts"
    [ Tasty.testGroup
        "Basket Validator"
        [ SetAdmin.tests s
        , Rebalance.tests gen s
        , Initialize.tests s
        , SwitchLock.tests s
        , Deposit.tests s
        , Withdraw.tests gen s
        , Donate.tests s
        , UpdateExRate.tests s
        ]
    , Tasty.testGroup "OneShotMintingPolicy" [OneShotMintingPolicy.tests]
    ]
