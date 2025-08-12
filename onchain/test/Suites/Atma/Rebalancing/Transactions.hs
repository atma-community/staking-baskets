{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.Rebalancing.Transactions (
  completeIdentityRebalance,
  completeRebalance,
  partialIdentityRebalance,
  partialRebalance,
  rebalanceTx,
  rebalanceTxWith,
  RebalanceRecipe (
    MkRebalanceRecipe,
    inputDistribution,
    outputDistribution
  ),
  TxRecipe (
    MkTxRecipe,
    submitTxAs,
    extraConstraints
  ),
  correctTxRecipe,
  consumeBasketStateUtxo,
  returnBasketStateUtxo,
) where

import Control.Monad.Cont (MonadTrans (lift))

import Plutus.Model (
  DatumMode (InlineDatum),
  refInputHash,
  submitTx,
 )
import Plutus.Model.V2 (
  Tx,
  payToRef,
  spendScriptRef,
 )

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  PubKeyHash,
  TxOut (txOutValue),
  TxOutRef,
  Value,
 )

import Atma.PTypes (
  PBasketValidatorRedeemer (
    MkPRebalance,
    MkPRebalanceDelegate
  ),
  PStakePoolUtxoDatum,
 )
import Atma.Types (
  AdminPubKeyHash (getAdminPubKeyHash),
 )

import Atma.Utils (pextractStakePoolUtxoDatum)
import Control.Arrow (Arrow (second))
import Control.Monad.Reader.Class (ask)
import Data.Foldable (Foldable (toList))
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    basketValidatorBasketStateUtxo,
    basketValidatorStakePoolUtxo,
    basketValidatorUtxo
  ),
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
 )
import Suites.Atma.Common.Utils (
  findBasketStateUtxo,
  findStakePoolUtxos,
  shuffle,
 )
import System.Random qualified as Random

extractSpInfo :: TxOut -> (PlutarchAdapter PStakePoolUtxoDatum, Value)
extractSpInfo out =
  ( PlutarchAdapter $ pextractStakePoolUtxoDatum # pconstant out
  , txOutValue out
  )

identityRebalance :: [(TxOutRef, TxOut)] -> RebalanceRecipe
identityRebalance inputs =
  MkRebalanceRecipe
    { inputDistribution = second fst <$> infos
    , outputDistribution = infos
    }
  where
    infos = second extractSpInfo <$> inputs

shuffleFunds ::
  forall (g :: Type).
  Random.RandomGen g =>
  g ->
  [(TxOutRef, TxOut)] ->
  RebalanceRecipe
shuffleFunds gen inputs =
  MkRebalanceRecipe
    { inputDistribution = second fst <$> infos
    , outputDistribution = infos''
    }
  where
    infos :: [(TxOutRef, (_, Value))]
    infos = second extractSpInfo <$> inputs
    infos' = shuffle gen infos
    infos'' =
      zipWith
        (\(ref, (datum, _)) (_, (_, value')) -> (ref, (datum, value')))
        infos
        infos'

completeRebalance ::
  forall (g :: Type).
  Random.RandomGen g =>
  g ->
  BasketContext RebalanceRecipe
completeRebalance gen =
  shuffleFunds gen . fmap fst . toList <$> findStakePoolUtxos

partialRebalance ::
  forall (g :: Type).
  Random.RandomGen g =>
  g ->
  BasketContext RebalanceRecipe
partialRebalance gen = do
  stakePools <- findStakePoolUtxos
  let
    (sampleSize, gen') = Random.randomR (2, length stakePools) gen
    recipe = shuffleFunds gen' $ fmap fst $ take sampleSize $ toList stakePools
  pure recipe

data RebalanceRecipe = MkRebalanceRecipe
  { inputDistribution :: [(TxOutRef, PlutarchAdapter PStakePoolUtxoDatum)]
  , outputDistribution :: [(TxOutRef, (PlutarchAdapter PStakePoolUtxoDatum, Value))]
  }

completeIdentityRebalance :: BasketContext RebalanceRecipe
completeIdentityRebalance =
  identityRebalance . fmap fst . toList <$> findStakePoolUtxos

partialIdentityRebalance ::
  forall (g :: Type).
  Random.RandomGen g =>
  g ->
  BasketContext RebalanceRecipe
partialIdentityRebalance gen = do
  stakePools <- findStakePoolUtxos
  let
    (sampleSize, _) = Random.randomR (1, length stakePools) gen
    recipe = identityRebalance $ fmap fst $ take sampleSize $ toList stakePools
  pure recipe

data TxRecipe = MkTxRecipe
  { submitTxAs :: PubKeyHash
  , extraConstraints :: Tx
  }

correctTxRecipe :: BasketContext TxRecipe
correctTxRecipe = do
  (_, PlutarchAdapter basketState) <- findBasketStateUtxo
  let
    admin =
      plift $ pfromData $ pfield @"adminPkh" # pto basketState
  pure $
    MkTxRecipe
      { submitTxAs = getAdminPubKeyHash admin
      , extraConstraints = mempty
      }

rebalanceTx :: RebalanceRecipe -> BasketContext ()
rebalanceTx recipe = do
  txRecipe <- correctTxRecipe
  rebalanceTxWith txRecipe recipe

rebalanceTxWith :: TxRecipe -> RebalanceRecipe -> BasketContext ()
rebalanceTxWith MkTxRecipe {..} MkRebalanceRecipe {..} = do
  ctx <- ask
  let
    delegateRedeemer =
      PlutarchAdapter $ pcon $ MkPRebalanceDelegate pdnil
    checkingRedeemer =
      PlutarchAdapter $ pcon $ MkPRebalance pdnil
    inputs =
      zip inputDistribution $
        checkingRedeemer : repeat delegateRedeemer
    scriptRef = basketValidatorUtxo ctx
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo
  lift $
    submitTx submitTxAs $
      mconcat
        [ flip foldMap inputs \((ref, datum), redeemer) ->
            spendScriptRef
              scriptRef
              (basketValidatorStakePoolUtxo ctx)
              ref
              redeemer
              datum
        , foldMap
            ( uncurry (payToRef (basketValidatorStakePoolUtxo ctx) . InlineDatum)
                . snd
            )
            outputDistribution
        , extraConstraints
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]

consumeBasketStateUtxo :: BasketContext Tx
consumeBasketStateUtxo = do
  ctx <- ask
  ((ref, _), datum) <- findBasketStateUtxo
  let scriptRef = basketValidatorUtxo ctx
  pure $
    spendScriptRef
      scriptRef
      (basketValidatorBasketStateUtxo ctx)
      ref
      -- FIXME: should other redeemers be checked as well?
      (PlutarchAdapter $ pcon $ MkPRebalance pdnil)
      datum

returnBasketStateUtxo :: BasketContext Tx
returnBasketStateUtxo = do
  ctx <- ask
  ((_, out), datum) <- findBasketStateUtxo
  pure $
    payToRef (basketValidatorBasketStateUtxo ctx) (InlineDatum datum) $
      txOutValue out
