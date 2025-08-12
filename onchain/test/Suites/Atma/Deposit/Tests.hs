{-# LANGUAGE BlockArguments #-}

module Suites.Atma.Deposit.Tests (tests) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Bifunctor (bimap)

import Suites.Atma.Deposit.Transactions (
  depositContract'ConsumeMultipleStakePool,
  depositContract'MintMore,
  depositContract'StealAda,
  depositContract'StealStakePoolCS,
  depositContract'SuccessfulDeposit,
 )
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

import Atma.Types (
  LockingParams (MkLockingParams),
 )
import Suites.Atma.Common (initializeBasketContract)
import Suites.Atma.Common.Utils (
  Admin (Admin),
  ErrorMsgContains (ErrorMsgContains),
  InitParams (MkInitParams),
  Scripts,
  User (User),
  checkFailWithMsg,
 )
import Suites.Atma.Config (config)
import Suites.Atma.SwitchLock.Transactions (
  LockSwitch (Lock, Unlock),
  LockType (BasketLock, PledgeLock),
  switchLockBlocking,
 )

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

successfulDepositTest :: Scripts -> Run ()
successfulDepositTest s = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams s
  flip runReaderT ctx $ depositContract'SuccessfulDeposit user 1

mintMoreTest :: Scripts -> Run ()
mintMoreTest s = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams s
  flip runReaderT ctx $ depositContract'MintMore user 1

consumeMoreStakePoolUtxoTest :: Scripts -> Run ()
consumeMoreStakePoolUtxoTest s = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 4
  ctx <- initializeBasketContract initParams s
  flip runReaderT ctx $ depositContract'ConsumeMultipleStakePool user 1

stealAdaFromStakePoolUtxoTest :: Scripts -> Run ()
stealAdaFromStakePoolUtxoTest s = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 1
  ctx <- initializeBasketContract initParams s
  flip runReaderT ctx do
    depositContract'SuccessfulDeposit user 10
    depositContract'StealAda user 1

stealStakePoolTokenFromStakePoolUtxoTest :: Scripts -> Run ()
stealStakePoolTokenFromStakePoolUtxoTest s = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 1
  ctx <- initializeBasketContract initParams s
  flip runReaderT ctx $ depositContract'StealStakePoolCS user 10

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Deposit Scenarios"
    [ testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful deposit"
        $ successfulDepositTest scripts
    , testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful deposit after basket lock"
        do
          (admin, user) <- setupUsers
          waitNSlots 1
          let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
          ctx <- initializeBasketContract initParams scripts
          flip runReaderT ctx do
            switchLockBlocking BasketLock Lock
            switchLockBlocking BasketLock $ Unlock $ Just user
            depositContract'SuccessfulDeposit user 1
    , testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful deposit during and after pledge lock"
        do
          (admin, user) <- setupUsers
          waitNSlots 1
          let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
          ctx <- initializeBasketContract initParams scripts
          flip runReaderT ctx do
            switchLockBlocking PledgeLock Lock
            depositContract'SuccessfulDeposit user 1
            switchLockBlocking PledgeLock $ Unlock $ Just user
            depositContract'SuccessfulDeposit user 1
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to mint more basket tokens than allowed"
        (ErrorMsgContains "Exchange assertion failed")
        $ mintMoreTest scripts
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to consume more than one stake pool utxo while depositing"
        (ErrorMsgContains "Spending more than one UTxO from basket validator address")
        $ consumeMoreStakePoolUtxoTest scripts
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to steal ada from stake pool utxo"
        (ErrorMsgContains "Minting assertion failed")
        $ stealAdaFromStakePoolUtxoTest scripts
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to steal StakePool token from stake pool utxo"
        (ErrorMsgContains "No token at output staking pool utxo")
        $ stealStakePoolTokenFromStakePoolUtxoTest scripts
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to deposit during Basket lock"
        (ErrorMsgContains "Basket must be unlocked for deposit/withdrawal")
        do
          (admin, user) <- setupUsers
          waitNSlots 1
          let initParams = MkInitParams [] admin bogusLockingParams Nothing 3
          ctx <- initializeBasketContract initParams scripts
          flip runReaderT ctx do
            switchLockBlocking BasketLock Lock
            depositContract'SuccessfulDeposit user 1
    ]
