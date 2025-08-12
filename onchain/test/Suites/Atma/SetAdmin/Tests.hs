{-# LANGUAGE BlockArguments #-}

module Suites.Atma.SetAdmin.Tests (tests) where

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
  PubKeyHash,
  Value,
 )

import Atma.PTypes (PBasketState)
import Atma.Types (
  AdminPubKeyHash (getAdminPubKeyHash),
  LockingParams (MkLockingParams),
 )
import Control.Applicative ((<**>))
import Control.Monad (replicateM, void, when)
import Control.Monad.Reader (MonadTrans (lift), ReaderT (runReaderT))
import Data.Coerce (coerce)
import Data.List (sort)
import Suites.Atma.Common (
  initializeBasketContract,
 )
import Suites.Atma.Common.BasketContext (Ctx)
import Suites.Atma.Common.PSM (PlutarchAdapter (PlutarchAdapter))
import Suites.Atma.Common.Utils (
  Admin (Admin),
  ErrorMsgContains (ErrorMsgContains),
  InitParams (MkInitParams),
  Scripts,
  checkFailWithMsg,
  findBasketStateUtxo,
 )
import Suites.Atma.Config (config)
import Suites.Atma.SetAdmin.Transactions (setAdmin)
import Test.Tasty qualified as Tasty

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 3000)

initFunds :: Value
initFunds = adaValue 50_000_000

setupAdmin :: Run Admin
setupAdmin =
  coerce <$> newUser initFunds

setupVoters :: Int -> Run [PubKeyHash]
setupVoters = flip replicateM $ newUser initFunds

initializeBasket :: Int -> Scripts -> Run (Ctx, Admin, [PubKeyHash])
initializeBasket numOfVoters scripts = do
  admin <- setupAdmin
  voters <- setupVoters numOfVoters
  waitNSlots 1
  let initParams =
        MkInitParams
          (sort voters)
          admin
          bogusLockingParams
          (Just bogusLockingParams)
          1
  ctx <- initializeBasketContract initParams scripts
  pure (ctx, admin, voters)

extractAdmin :: ClosedTerm PBasketState -> Admin
extractAdmin t = Admin $ getAdminPubKeyHash $ plift $ pfield @"adminPkh" # t

adminNotChangedMessage :: String
adminNotChangedMessage = "Admin not changed"

test :: ([PubKeyHash] -> [PubKeyHash]) -> Int -> Scripts -> Run (Admin, Admin)
test selectVoters numOfVoters scripts = do
  (ctx, _, voters) <- initializeBasket numOfVoters scripts
  newAdmin <- setupAdmin
  expectedAdmin <- flip runReaderT ctx do
    setAdmin newAdmin $ selectVoters voters
    lift $ waitNSlots 10
    (_, PlutarchAdapter basketState) <- findBasketStateUtxo
    pure $ extractAdmin basketState
  pure (newAdmin, expectedAdmin)

test1 :: Int -> Scripts -> Run ()
test1 numOfVoters scripts = do
  (newAdmin, expectedAdmin) <- test id numOfVoters scripts
  when (newAdmin /= expectedAdmin) $ fail adminNotChangedMessage

test3 :: Int -> Scripts -> Run ()
test3 numOfVoters scripts = do
  (newAdmin, expectedAdmin) <-
    test
      (\voters -> take (1 + length voters `div` 2) voters)
      numOfVoters
      scripts
  when (newAdmin /= expectedAdmin) $ fail adminNotChangedMessage

test2 :: Int -> Scripts -> Run (Admin, Admin)
test2 = test $ \voters -> take (length voters `div` 2) voters

test4 :: Int -> Scripts -> Run (Admin, Admin)
test4 = test $ const []

tests :: Scripts -> Tasty.TestTree
tests scripts =
  Tasty.testGroup
    "Admin settings tests"
    [ Tasty.testGroup
        "Sunny day scenarios"
        $ [1 .. 20]
          <**> [ \numOfVoters ->
                  testNoErrors
                    (adaValue 2_000_000_000)
                    config
                    ("Can change the admin with all voters; num of voters: " <> show numOfVoters)
                    $ test1 numOfVoters scripts
               , \numOfVoters ->
                  testNoErrors
                    (adaValue 2_000_000_000)
                    config
                    ("Can change the admin with just enough voters; num of voters: " <> show numOfVoters)
                    $ test3 numOfVoters scripts
               ]
    , Tasty.testGroup
        "When too few voters"
        $ [1 .. 20]
          <**> [ \numOfVoters ->
                  checkFailWithMsg
                    config
                    (adaValue 2_000_000_000)
                    ("Can't change the admin when no majority; with num of voters: " <> show numOfVoters)
                    (ErrorMsgContains "Majority of voters have not signed the transaction")
                    $ void
                    $ test2 numOfVoters scripts
               , \numOfVoters ->
                  checkFailWithMsg
                    config
                    (adaValue 2_000_000_000)
                    ("Can't change the admin when no one signed; with num of voters: " <> show numOfVoters)
                    (ErrorMsgContains "Majority of voters have not signed the transaction")
                    $ void
                    $ test4 numOfVoters scripts
               ]
    ]
