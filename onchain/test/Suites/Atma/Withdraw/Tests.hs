{-# LANGUAGE BlockArguments #-}

module Suites.Atma.Withdraw.Tests (tests) where

import Control.Monad.Reader (ReaderT (runReaderT))

import Suites.Atma.Deposit.Transactions (
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
  ExRate (MkExRate),
  LockingParams (MkLockingParams),
 )
import Data.Coerce (coerce)
import Data.Ratio qualified as R
import Suites.Atma.Common (initializeBasketContract)
import Suites.Atma.Common.BasketContext (Ctx)
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
import Suites.Atma.UpdateExRate.Transactions (
  increaseExRateToAtLeast,
 )
import Suites.Atma.Withdraw.Transactions (
  withdrawContract'ConsumeExtraBasketValidatorUtxo,
  withdrawContract'ConsumeMultipleStakePoolUtxo,
  withdrawContract'StealBasketStateToken,
  withdrawContract'StealBasketToken,
  withdrawContract'StealStakePoolToken,
  withdrawContract'SuccesfulWithdraw,
  withdrawContract'UnderdecrementCounter,
  withdrawContract'WithdrawMore,
 )
import System.Random qualified as Random

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 5000)
    (DiffMilliSeconds 5000)
    (DiffMilliSeconds 5000)

initFunds :: Value
initFunds = adaValue 50_000_000

setupUsers :: Run (Admin, User)
setupUsers =
  fmap coerce $
    (,) <$> newUser initFunds <*> newUser initFunds

initializeBasketForWithdraw ::
  forall g.
  Random.RandomGen g =>
  g ->
  Scripts ->
  Run (Ctx, User, g)
initializeBasketForWithdraw gen scripts = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams scripts
  flip runReaderT ctx do
    depositContract'SuccessfulDeposit user 1000
    switchLockBlocking BasketLock $ Unlock $ Just user
    let (denom, gen') = Random.randomR (1, 10) gen
        (numer, gen'') = Random.randomR (denom, denom + 5) gen'
    increaseExRateToAtLeast user $ MkExRate $ numer R.% denom
    pure (ctx, user, gen'')

successfulWithdrawTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
successfulWithdrawTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  waitNSlots 1
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'SuccesfulWithdraw user 1000

successfulPartialWithdrawTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
successfulPartialWithdrawTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'SuccesfulWithdraw user 500

withdrawMoreAdaThanAllowedTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
withdrawMoreAdaThanAllowedTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'WithdrawMore user 500

stealStakePoolTokenTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
stealStakePoolTokenTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'StealStakePoolToken user 500

stealBasketStateTokenTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
stealBasketStateTokenTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'StealBasketStateToken user 500

stealBasketTokenTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
stealBasketTokenTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'StealBasketToken user 500

consumeExtraUtxoTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
consumeExtraUtxoTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'ConsumeExtraBasketValidatorUtxo user 500

underdecrementBasketCounter :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
underdecrementBasketCounter gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'UnderdecrementCounter user 500

consumeMultipleStakePoolUtxoTest :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
consumeMultipleStakePoolUtxoTest gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    withdrawContract'ConsumeMultipleStakePoolUtxo user 500

withdrawWhen3To1Test :: forall g. Random.RandomGen g => g -> Scripts -> Run ()
withdrawWhen3To1Test gen s = do
  (ctx, user, _) <- initializeBasketForWithdraw gen s
  flip runReaderT ctx do
    switchLockBlocking BasketLock $ Unlock $ Just user
    increaseExRateToAtLeast user $ MkExRate $ 3 R.% 1
    withdrawContract'SuccesfulWithdraw user 500

tests :: forall g. Random.RandomGen g => g -> Scripts -> Tasty.TestTree
tests gen s =
  Tasty.testGroup
    "Withdraw Scenarios"
    [ testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful complete withdraw"
        $ successfulWithdrawTest gen s
    , testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful complete withdraw after pledge lock"
        do
          (ctx, user, _) <- initializeBasketForWithdraw gen s
          waitNSlots 1
          flip runReaderT ctx do
            switchLockBlocking PledgeLock Lock
            switchLockBlocking PledgeLock $ Unlock $ Just user
            switchLockBlocking BasketLock $ Unlock $ Just user
            withdrawContract'SuccesfulWithdraw user 1000
    , testNoErrors
        (adaValue 100_000_000)
        config
        "Make successful partial withdraw"
        $ successfulPartialWithdrawTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to withdraw more ada than allowed"
        (ErrorMsgContains "Exchange assertion failed")
        $ withdrawMoreAdaThanAllowedTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Must decrement BasketToken counter correctly"
        (ErrorMsgContains "Basket token counter has been updated incorrectly")
        $ underdecrementBasketCounter gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Can not steal BasketToken instead of burning"
        (ErrorMsgContains "Exchange assertion failed")
        $ stealBasketTokenTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to steal stake pool token while withdrawing"
        (ErrorMsgContains "No token at output staking pool utxo")
        $ stealStakePoolTokenTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to steal basket state"
        ( ErrorMsgContains
            "Spending more than one UTxO from basket validator address"
        )
        $ stealBasketStateTokenTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to spend even user UTxO from validator"
        ( ErrorMsgContains
            "Spending more than one UTxO from basket validator address"
        )
        $ consumeExtraUtxoTest gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to consume multiple stake pool UTxOs"
        ( ErrorMsgContains
            "Spending more than one UTxO from basket validator address"
        )
        $ consumeMultipleStakePoolUtxoTest gen s
    , testNoErrors
        (adaValue 100_000_000)
        config
        "Can withdraw when ratio is 3:1"
        $ withdrawWhen3To1Test gen s
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to withdraw during pledge lock"
        ( ErrorMsgContains
            "Basket must be unlocked for withdrawal due to pledge locking"
        )
        do
          (ctx, user, _) <- initializeBasketForWithdraw gen s
          flip runReaderT ctx do
            switchLockBlocking BasketLock $ Unlock Nothing
            switchLockBlocking PledgeLock Lock
            withdrawContract'SuccesfulWithdraw user 500
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to withdraw during basket lock"
        ( ErrorMsgContains
            "Basket must be unlocked for deposit/withdrawal"
        )
        do
          (ctx, user, _) <- initializeBasketForWithdraw gen s
          flip runReaderT ctx do
            switchLockBlocking PledgeLock $ Unlock Nothing
            switchLockBlocking BasketLock Lock
            withdrawContract'SuccesfulWithdraw user 500
    , checkFailWithMsg
        config
        (adaValue 100_000_000)
        "Not able to withdraw during both locks"
        ( ErrorMsgContains
            "Basket must be unlocked for deposit/withdrawal"
        )
        do
          (ctx, user, _) <- initializeBasketForWithdraw gen s
          flip runReaderT ctx do
            switchLockBlocking PledgeLock Lock
            switchLockBlocking BasketLock Lock
            withdrawContract'SuccesfulWithdraw user 500
    ]
