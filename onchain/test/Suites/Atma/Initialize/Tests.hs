{-# LANGUAGE BlockArguments #-}

module Suites.Atma.Initialize.Tests (tests) where

import Test.Tasty qualified as Tasty

import Plutus.Model (
  Run,
  testNoErrors,
  waitNSlots,
 )
import Plutus.Model.V2 (
  adaValue,
  newUser,
 )

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import PlutusLedgerApi.V2 (
  Value,
 )

import Atma.Types (LockingParams (MkLockingParams))
import Data.Coerce (coerce)
import Suites.Atma.Common (initializeBasketContract)
import Suites.Atma.Common.BasketContext (Ctx)
import Suites.Atma.Common.Utils (
  Admin (Admin),
  InitParams (MkInitParams),
  Scripts,
  User (User),
 )
import Suites.Atma.Config (config)

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 35000)
    (DiffMilliSeconds 35000)
    (DiffMilliSeconds 5000)

initFunds :: Value
initFunds = adaValue 50_000_000

setupUsers :: Run (Admin, User)
setupUsers =
  fmap coerce $
    (,) <$> newUser initFunds <*> newUser initFunds

initializeBasket :: Scripts -> Run (Ctx, User)
initializeBasket scripts = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 2
  ctx <- initializeBasketContract initParams scripts
  pure (ctx, user)

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Initialize Scenarios"
    [ testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful complete initialize"
        $ initializeBasket scripts
    ]
