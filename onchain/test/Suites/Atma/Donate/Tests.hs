{-# LANGUAGE BlockArguments #-}

module Suites.Atma.Donate.Tests (tests) where

import Atma.Types (
  LockingParams (MkLockingParams),
 )
import Control.Monad.Reader (
  ReaderT (runReaderT),
  when,
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
  stakePoolUtxosSummary,
 )
import Suites.Atma.Common.Utils (
  Admin (Admin),
  InitParams (MkInitParams),
  Scripts,
  User (User),
  findStakePoolUtxos,
 )
import Suites.Atma.Config (config)
import Suites.Atma.Donate.Transactions (
  donate,
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

successfulDonateTest :: Scripts -> Run ()
successfulDonateTest scripts = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams scripts

  let amt = 100

  r@(before, after) <- flip runReaderT ctx do
    spUtxos <- findStakePoolUtxos
    let (totalAda, _) = stakePoolUtxosSummary $ MkUtxo <$> spUtxos

    donate user amt (MkUtxo $ NE.head spUtxos)

    spUtxos' <- findStakePoolUtxos
    let (totalAda', _) = stakePoolUtxosSummary $ MkUtxo <$> spUtxos'

    pure (totalAda, totalAda')

  when (after /= before <> amt) $ fail $ "Failed donate test: " <> show r

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Donation Scenarios"
    [ testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful donation"
        $ successfulDonateTest scripts
    ]
