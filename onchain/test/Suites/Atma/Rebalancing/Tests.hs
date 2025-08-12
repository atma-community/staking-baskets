{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NegativeLiterals #-}

module Suites.Atma.Rebalancing.Tests (tests) where

import Atma.Scripts.Common (stakePoolTN)
import Atma.Types (LockingParams (MkLockingParams))
import Control.Lens (
  each,
  filtered,
  view,
  (%~),
  (&),
  (+~),
  (<>~),
  (?~),
  (^.),
  _1,
  _2,
 )
import Control.Monad (zipWithM_)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Coerce (coerce)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Plutarch.Test.QuickCheck.Instances ()
import Plutus.Model (
  Run,
  adaOf,
  mintValue,
  payToKey,
  scriptCurrencySymbol,
  testNoErrors,
  waitNSlots,
 )
import Plutus.Model.V2 (
  Ada (Lovelace),
  adaValue,
  newUser,
 )
import PlutusLedgerApi.V1.Time (
  DiffMilliSeconds (DiffMilliSeconds),
 )
import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (
  Value,
  singleton,
  txOutValue,
 )
import Suites.Atma.Common (
  initializeBasketContract,
 )
import Suites.Atma.Common.BasketContext (BasketContext, Ctx (Ctx, stakingPoolMP))
import Suites.Atma.Common.Utils (
  Admin (Admin),
  ErrorMsgContains (ErrorMsgContains),
  InitParams (MkInitParams),
  NumOfStakePoolUtxos,
  Scripts,
  User (User),
  checkFailWithMsg,
  findStakePoolUtxos,
  randomDistribution,
  _basketTokenCounter,
  _poolPkh,
 )
import Suites.Atma.Config (config)
import Suites.Atma.Deposit.Transactions (
  depositContract'SuccessfulDeposit,
  depositToStakePoolUtxo,
 )
import Suites.Atma.Rebalancing.Transactions (
  RebalanceRecipe (MkRebalanceRecipe, outputDistribution),
  TxRecipe (
    extraConstraints,
    submitTxAs
  ),
  completeIdentityRebalance,
  completeRebalance,
  consumeBasketStateUtxo,
  correctTxRecipe,
  partialIdentityRebalance,
  partialRebalance,
  rebalanceTx,
  rebalanceTxWith,
  returnBasketStateUtxo,
 )
import Suites.Atma.SwitchLock.Transactions (
  LockSwitch (Lock),
  LockType (BasketLock, PledgeLock),
  switchLockBlocking,
 )
import System.Random qualified as Random
import Test.Tasty qualified as Tasty

bogusLockingParams :: LockingParams
bogusLockingParams =
  MkLockingParams
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 2000)
    (DiffMilliSeconds 3000)

initFunds :: Value
initFunds = adaValue 500_000_000

setupUsers :: Run (Admin, User)
setupUsers =
  fmap coerce $
    (,) <$> newUser initFunds <*> newUser initFunds

initializeBasket :: Scripts -> NumOfStakePoolUtxos -> Run (Ctx, User)
initializeBasket scripts stakePoolsAmount = do
  (admin, user) <- setupUsers
  waitNSlots 1
  let initParams =
        MkInitParams
          []
          admin
          bogusLockingParams
          (Just bogusLockingParams)
          stakePoolsAmount
  ctx <- initializeBasketContract initParams scripts
  pure (ctx, user)

distributeFunds :: User -> [Integer] -> BasketContext ()
distributeFunds user distribution = do
  spUtxos <- NE.toList <$> findStakePoolUtxos
  zipWithM_
    (depositToStakePoolUtxo user)
    distribution
    spUtxos

distributeSpAdaRandomly ::
  forall (g :: Type). Random.RandomGen g => g -> User -> BasketContext g
distributeSpAdaRandomly gen user = do
  distributionSize <- length <$> findStakePoolUtxos
  let
    (distrib, gen') = randomDistribution gen (100, 300) distributionSize
  distributeFunds user $ NE.toList distrib
  pure gen'

-- | Above this value Tx is likely to exceed limit by ~10%
stakePoolsPerTxThreshold :: NumOfStakePoolUtxos
stakePoolsPerTxThreshold = 28

initialAda :: Value
initialAda = adaValue 1_000_000_000

tests :: forall (g :: Type). Random.RandomGen g => g -> Scripts -> Tasty.TestTree
tests gen scripts =
  Tasty.testGroup
    "Rebalancing Scenarios"
    [ testNoErrors
        initialAda
        config
        "Can perform complete identity rebalance in a completely unlocked basket"
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            rebalanceTx =<< completeIdentityRebalance
    , testNoErrors
        initialAda
        config
        "Can perform partial identity rebalance in a completely unlocked basket"
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            rebalanceTx =<< partialIdentityRebalance gen
    , testNoErrors
        initialAda
        config
        "Can perform complete rebalance correctly in completely unlocked basket"
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            rebalanceTx =<< completeRebalance gen
    , testNoErrors
        initialAda
        config
        "Can perform partial rebalance correctly in completely unlocked basket"
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            rebalanceTx =<< partialRebalance gen
    , testNoErrors
        initialAda
        config
        "Can rebalance correctly with all locks set up"
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            switchLockBlocking BasketLock Lock
            switchLockBlocking PledgeLock Lock
            rebalanceTx =<< partialRebalance gen
    , checkFailWithMsg
        config
        initialAda
        "Can't rebalance with no administrator signature"
        (ErrorMsgContains "Not signed by an administrator")
        do
          (ctx, user@(User userPkh)) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            correctTxRecipe <- correctTxRecipe
            rebalanceTxWith (correctTxRecipe {submitTxAs = userPkh})
              =<< partialRebalance gen
    , checkFailWithMsg
        config
        initialAda
        "Can't consume BasketState UTxO"
        (ErrorMsgContains "Should not consume Basket State UTxO")
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            consumeBsUtxo <- consumeBasketStateUtxo
            returnBsUtxo <- returnBasketStateUtxo
            correctTxRecipe <- correctTxRecipe
            rebalanceTxWith
              correctTxRecipe {extraConstraints = consumeBsUtxo <> returnBsUtxo}
              =<< partialRebalance gen
    , testNoErrors
        initialAda
        config
        "Can change stake pool Pkh using MkPRebalance redeemer"
        do
          (ctx, user@(User userPkh)) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            plan@MkRebalanceRecipe {outputDistribution} <-
              partialRebalance gen
            let
              changePoolPkh = _2 . _1 . _poolPkh ?~ userPkh
              mapOdds f = zipWith ($) $ cycle [f, id]
            rebalanceTx $
              plan
                { outputDistribution =
                    mapOdds changePoolPkh outputDistribution
                }
    , checkFailWithMsg
        config
        initialAda
        "Can not mint"
        -- we will try to mint StakePool token
        -- and add it to the very first StakePool UTxO
        (ErrorMsgContains "Should not mint or burn")
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            plan@MkRebalanceRecipe {outputDistribution} <-
              completeIdentityRebalance
            firstSp : rest <- pure outputDistribution
            txRecipe <- correctTxRecipe
            Ctx {stakingPoolMP} <- ask
            let
              spCs = scriptCurrencySymbol stakingPoolMP
              toMint = singleton spCs (tokenName "StakePool") 1
              firstSp' = firstSp & _2 . _2 <>~ toMint
            rebalanceTxWith
              txRecipe
                { extraConstraints = mintValue stakingPoolMP () toMint
                }
              plan {outputDistribution = firstSp' : rest}
    , checkFailWithMsg
        config
        initialAda
        "Can not change amount of StakePool UTxOs"
        -- We will try to smash all StakePool UTxOs into one
        -- preserving Ada and BasketTokens balance
        -- to not trigger these checks
        (ErrorMsgContains "Should not mint or burn")
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            plan@MkRebalanceRecipe {outputDistribution} <-
              completeIdentityRebalance
            firstSp : rest <- pure outputDistribution
            let
              remainingAda =
                foldMap (adaValue . coerce . adaOf . snd . snd) rest
              remainingBasketToken =
                sum
                  . fmap (view $ _2 . _1 . _basketTokenCounter)
                  $ rest
            Ctx {stakingPoolMP} <- ask
            let
              spCs = scriptCurrencySymbol stakingPoolMP
              stakePoolTokensToBurn =
                singleton spCs stakePoolTN
                  . toInteger
                  . negate
                  $ length rest
            txRecipe <- correctTxRecipe
            rebalanceTxWith
              txRecipe
                { extraConstraints =
                    mintValue stakingPoolMP () stakePoolTokensToBurn
                }
              plan
                { outputDistribution =
                    [ firstSp
                        & _2 . _1 . _basketTokenCounter +~ remainingBasketToken
                        & _2 . _2 <>~ remainingAda
                    ]
                }
    , checkFailWithMsg
        config
        initialAda
        "Can not steal StakePool token"
        -- We will try to remove one stake pool UTxO
        -- and steal the token, but preserve Ada and BasketToken balances
        (ErrorMsgContains "Stealing SP token")
        do
          (ctx, user@(User userPkh)) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            plan@MkRebalanceRecipe {outputDistribution} <-
              completeIdentityRebalance
            spToSteal : nextSp : rest <- pure outputDistribution
            let
              adaFromRemovedSp =
                adaValue . coerce . adaOf . snd . snd $ spToSteal
              btFromRemovedSp = spToSteal ^. _2 . _1 . _basketTokenCounter
            Ctx {stakingPoolMP} <- ask
            let
              spCs = scriptCurrencySymbol stakingPoolMP
              stakePoolTokens = singleton spCs stakePoolTN
              nextSp' =
                nextSp
                  & _2 . _1 . _basketTokenCounter +~ btFromRemovedSp
                  & _2 . _2 <>~ adaFromRemovedSp
            txRecipe <- correctTxRecipe
            rebalanceTxWith
              txRecipe
                { extraConstraints = payToKey userPkh $ stakePoolTokens 1
                }
              plan
                { outputDistribution = nextSp' : rest
                }
    , checkFailWithMsg
        config
        initialAda
        "Can't change BasketToken counter using MkPRebalance redeemer"
        (ErrorMsgContains "(total basket tokens) must be preserved")
        do
          (ctx, user) <- initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            correctRecipe@MkRebalanceRecipe {outputDistribution} <-
              partialRebalance gen
            let
              incrementBtCounter = _2 . _1 . _basketTokenCounter %~ succ
              mapOdds f = zipWith ($) $ cycle [f, id]
            rebalanceTx $
              correctRecipe
                { outputDistribution =
                    mapOdds incrementBtCounter outputDistribution
                }
    , checkFailWithMsg
        config
        initialAda
        "Can't steal ADA"
        (ErrorMsgContains "(Total basket ADA) and (total basket tokens) must be preserved")
        do
          (ctx, user@(User userPkh)) <-
            initializeBasket scripts stakePoolsPerTxThreshold
          flip runReaderT ctx do
            distributeSpAdaRandomly gen user
            -- Sent funds to one of SP utxos, so at least one have enough to
            -- what we are going to steal
            depositContract'SuccessfulDeposit user 1_000
            let
              utxoAda = adaOf . txOutValue . snd . fst
            -- Find SP Utxo that has the highest amount of Ada, to steal from
            ((targetRef, _), _) :| _ <-
              NE.reverse . NE.sortBy (compare `on` utxoAda)
                <$> findStakePoolUtxos

            correctRecipe@MkRebalanceRecipe {outputDistribution} <-
              completeIdentityRebalance

            correctTx <- correctTxRecipe
            rebalanceTxWith
              correctTx
                { extraConstraints = payToKey userPkh $ adaValue 1_000
                }
              correctRecipe
                { outputDistribution =
                    outputDistribution
                      & each
                        . filtered ((==) targetRef . fst)
                        . _2
                        . _2
                        <>~ adaValue -1_000
                }
    ]
