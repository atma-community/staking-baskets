{-# LANGUAGE BlockArguments #-}

module Suites.Atma.UpdateExRate.Tests (tests) where

import Atma.Types (
  LockingParams (MkLockingParams),
 )
import Control.Monad.Reader (
  ReaderT (runReaderT),
 )
import Data.Bifunctor (bimap)
import Data.List.NonEmpty qualified as NE
import Plutus.Model (
  Run,
  testNoErrors,
  waitNSlots,
 )
import Plutus.Model.V2 (
  adaValue,
  newUser,
 )
import PlutusLedgerApi.V1.Time (
  DiffMilliSeconds (DiffMilliSeconds),
 )
import PlutusLedgerApi.V2 (
  Value,
 )
import Suites.Atma.Common (
  Utxo (MkUtxo),
  initializeBasketContract,
 )
import Suites.Atma.Common.Utils (
  Admin (Admin),
  InitParams (MkInitParams),
  Scripts,
  User (User),
  findStakePoolUtxos,
 )
import Suites.Atma.Config (config)
import Suites.Atma.Deposit.Transactions (
  depositContract'SuccessfulDeposit,
 )
import Suites.Atma.Donate.Transactions (
  donate,
 )
import Suites.Atma.UpdateExRate.Transactions (
  updateExRate,
 )
import Test.Tasty qualified as Tasty

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 5000)
    (DiffMilliSeconds 5000)
    (DiffMilliSeconds 5000)

initFunds :: Value
initFunds = adaValue 50_000_000

setupUsers :: Run (Admin, User)
setupUsers = bimap Admin User <$> ((,) <$> newUser initFunds <*> newUser initFunds)

successfulUpdateExRateTest :: Scripts -> Run ()
successfulUpdateExRateTest scripts = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams scripts

  flip runReaderT ctx do
    depositContract'SuccessfulDeposit user 50
    sp <- NE.head <$> findStakePoolUtxos
    donate user 100 (MkUtxo sp)
    updateExRate

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Update ExRate Scenarios"
    [ testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful Ex Rate update"
        $ successfulUpdateExRateTest scripts
    ]
