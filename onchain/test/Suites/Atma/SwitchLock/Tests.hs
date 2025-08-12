{-# LANGUAGE BlockArguments #-}

module Suites.Atma.SwitchLock.Tests (tests) where

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

import PlutusLedgerApi.V1.Time (
  DiffMilliSeconds (DiffMilliSeconds),
 )
import PlutusLedgerApi.V2 (
  Value,
 )

import Atma.PTypes (plocked, punlocked)
import Atma.Types (
  BasketLock (Locked, Unlocked),
  LockingParams (MkLockingParams),
 )
import Control.Monad.Reader (ReaderT (runReaderT), lift)
import Data.Coerce (coerce)
import Suites.Atma.Common (
  initializeBasketContract,
 )
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
  UpdateBasketStateFields (
    update'exRate',
    update'numOfStakePoolUTxOs'
  ),
  defaultBasketStateUpdate,
  lockAsUser,
  switchLockAsPkhContract,
  switchLockAsPkhWith,
  switchLockBlocking,
  update'lock',
  update'pledgeLock',
  waitUntilCanSwitch,
 )

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 3000)

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
  let initParams = MkInitParams [] admin bogusLockingParams (Just bogusLockingParams) 3
  ctx <- initializeBasketContract initParams scripts
  pure (ctx, user)

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Locking Scenarios"
    [ Tasty.testGroup
        "Basket lock"
        [ testNoErrors
            (adaValue 100_000_000)
            config
            "Can unlock, lock and unlock again"
            ( do
                (ctx, user) <- initializeBasket scripts
                flip runReaderT ctx do
                  switchLockBlocking BasketLock $ Unlock $ Just user
                  switchLockBlocking BasketLock Lock
                  lift $ waitNSlots 10
                  switchLockBlocking BasketLock $ Unlock $ Just user
            )
        , Tasty.testGroup
            "Can't update anything in BasketState apart from 'lock' field"
            let
              checkCase fieldName basketStateUpdate =
                checkFailWithMsg
                  config
                  (adaValue 100_000_000)
                  ("Can't update " <> fieldName <> " field")
                  (ErrorMsgContains "BasketState has been incorrectly updated")
                  do
                    (ctx, _) <- initializeBasket scripts
                    flip runReaderT ctx do
                      waitUntilCanSwitch BasketLock Lock
                      switchLockAsPkhWith BasketLock Lock basketStateUpdate
             in
              [ checkCase
                  "numOfStakePoolUTxOs"
                  defaultBasketStateUpdate
                    { update'numOfStakePoolUTxOs' = const 22
                    }
              , checkCase
                  "exRate"
                  defaultBasketStateUpdate
                    { update'exRate' = const 4
                    }
              , checkCase
                  "pledgeLock"
                  defaultBasketStateUpdate
                    { update'pledgeLock' = \lock -> case plift lock of
                        Locked at -> punlocked # pconstant at
                        Unlocked at -> plocked # pconstant at
                    }
              ]
        , checkFailWithMsg
            config
            (adaValue 100_000_000)
            "Locking require administrator signature"
            (ErrorMsgContains "Not signed by an administrator")
            do
              (ctx, user) <- initializeBasket scripts
              flip runReaderT ctx do
                waitUntilCanSwitch BasketLock Lock
                lockAsUser BasketLock user
        , checkFailWithMsg
            config
            (adaValue 100_000_000)
            "Can't lock during 'minLockInterval'"
            (ErrorMsgContains "Can't lock again that soon")
            do
              (ctx, user) <- initializeBasket scripts
              flip runReaderT ctx do
                switchLockBlocking BasketLock Lock
                switchLockBlocking BasketLock $ Unlock $ Just user
                switchLockAsPkhContract BasketLock Lock
        ]
    , Tasty.testGroup
        "Pledge lock"
        [ testNoErrors
            (adaValue 100_000_000)
            config
            "Can unlock, lock and unlock again"
            ( do
                (ctx, user) <- initializeBasket scripts
                flip runReaderT ctx do
                  switchLockBlocking PledgeLock $ Unlock $ Just user
                  switchLockBlocking PledgeLock Lock
                  lift $ waitNSlots 10
                  switchLockBlocking PledgeLock $ Unlock $ Just user
            )
        , Tasty.testGroup
            "Can't update anything in BasketState apart from 'pledgeLock' field"
            let
              checkCase fieldName basketStateUpdate =
                checkFailWithMsg
                  config
                  (adaValue 100_000_000)
                  ("Can't update " <> fieldName <> " field")
                  (ErrorMsgContains "BasketState has been incorrectly updated")
                  do
                    (ctx, _) <- initializeBasket scripts
                    flip runReaderT ctx do
                      waitUntilCanSwitch PledgeLock Lock
                      switchLockAsPkhWith PledgeLock Lock basketStateUpdate
             in
              [ checkCase
                  "numOfStakePoolUTxOs"
                  defaultBasketStateUpdate
                    { update'numOfStakePoolUTxOs' = const 22
                    }
              , checkCase
                  "exRate"
                  defaultBasketStateUpdate
                    { update'exRate' = const 4
                    }
              , checkCase
                  "lock"
                  defaultBasketStateUpdate
                    { update'lock' = \lock -> case plift lock of
                        Locked at -> punlocked # pconstant at
                        Unlocked at -> plocked # pconstant at
                    }
              ]
        , checkFailWithMsg
            config
            (adaValue 100_000_000)
            "Locking require administrator signature"
            (ErrorMsgContains "Not signed by an administrator")
            do
              (ctx, user) <- initializeBasket scripts
              flip runReaderT ctx do
                waitUntilCanSwitch PledgeLock Lock
                lockAsUser PledgeLock user
        , checkFailWithMsg
            config
            (adaValue 100_000_000)
            "Can't lock during 'minLockInterval'"
            (ErrorMsgContains "Can't lock again that soon")
            do
              (ctx, user) <- initializeBasket scripts
              flip runReaderT ctx do
                switchLockBlocking PledgeLock Lock
                switchLockBlocking PledgeLock $ Unlock $ Just user
                switchLockAsPkhContract PledgeLock Lock
        ]
    ]
