{-# LANGUAGE UndecidableInstances #-}

module Suites.Atma.Common.PSM (
  BasketValidatorStakePoolUtxo,
  BasketValidatorBasketStateUtxo,
  OneShotMP,
  StakePoolTokenMP,
  BasketTokenMP,
  getBasketValidatorStakePoolUtxo,
  getBasketValidatorBasketStateUtxo,
  getOneShotMP,
  getStakePoolTokenMP,
  getBasketTokenMP,
  getUnspendableValidator,
  PlutarchAdapter (PlutarchAdapter),
  tracingConfig,
) where

import Data.Either (fromRight)

import Plutus.Model (Run)
import Plutus.Model.V2 (TypedPolicy, TypedValidator, mkTypedValidatorPlutarch)
import Plutus.Model.Validator.V2 (
  mkTypedPolicyPlutarch,
  mkTypedPolicyPlutarchTypedScript,
  mkTypedValidatorPlutarchTypedScript,
 )

import Plutarch (Config (Config), TracingMode (DoTracing), tracingMode)
import Plutarch.Api.V2 (PMintingPolicy, PTokenName, PValidator)
import Plutarch.Api.V2.Tx (PTxOutRef)
import Plutarch.Builtin (pforgetData)
import Plutarch.Positive (PPositive)

import PlutusLedgerApi.V2 (BuiltinData (BuiltinData))
import PlutusTx.IsData (FromData (fromBuiltinData), ToData (toBuiltinData))

import Atma.PTypes (
  PBasketParams,
  PBasketState,
  PBasketStateCS,
  PBasketValidatorRedeemer,
  PStakePoolUtxoDatum,
  PStakingPoolCS,
 )
import Atma.Scripts.BasketTokenMP (pMkBasketTokenMP)
import Atma.Scripts.OneShotMintingPolicy (pMkOneShotMintingPolicy)
import Atma.Scripts.UnspendableValidator (pMkUnspendableValidator)
import Atma.Types (BasketStateCS)
import Atma.Utils (ptryFromUndata)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (PlutarchTypedScript, applyPlutarchTypedScript)
import Data.Void (Void)
import Unsafe.Coerce (unsafeCoerce)

newtype PlutarchAdapter p = PlutarchAdapter (ClosedTerm p)

instance PIsData p => ToData (PlutarchAdapter p) where
  toBuiltinData (PlutarchAdapter t) =
    BuiltinData $ plift $ pforgetData $ pdata t

instance (PTryFrom PData (PAsData p), PIsData p) => FromData (PlutarchAdapter p) where
  fromBuiltinData (BuiltinData d) =
    Just $ PlutarchAdapter $ unTermCont $ ptryFromUndata $ pconstant d

tracingConfig :: Config
tracingConfig = Config {tracingMode = DoTracing}

scriptExtractionError :: String
scriptExtractionError = " "

extract :: MonadFail m => Either a b -> m b
extract s = fromRight (fail scriptExtractionError) (pure <$> s)

type BasketValidatorBasketStateUtxo =
  TypedValidator (PlutarchAdapter PBasketState) (PlutarchAdapter PBasketValidatorRedeemer)

getBasketValidatorBasketStateUtxo ::
  PlutarchTypedScript (PBasketParams :--> PValidator) ->
  ClosedTerm PBasketParams ->
  Run BasketValidatorBasketStateUtxo
getBasketValidatorBasketStateUtxo basketValidatorScript basketParams =
  mkTypedValidatorPlutarchTypedScript <$> extract (basketValidatorScript `apply` basketParams)
  where
    apply = applyPlutarchTypedScript tracingConfig

type BasketValidatorStakePoolUtxo =
  TypedValidator (PlutarchAdapter PStakePoolUtxoDatum) (PlutarchAdapter PBasketValidatorRedeemer)

getBasketValidatorStakePoolUtxo ::
  BasketValidatorBasketStateUtxo ->
  BasketValidatorStakePoolUtxo
getBasketValidatorStakePoolUtxo = unsafeCoerce

getUnspendableValidator ::
  forall m.
  MonadFail m =>
  BasketStateCS ->
  m (TypedValidator BasketValidatorBasketStateUtxo Void)
getUnspendableValidator basketStateCs = do
  extract $
    mkTypedValidatorPlutarch tracingConfig $
      pMkUnspendableValidator # pconstant basketStateCs

type OneShotMP = TypedPolicy ()

getOneShotMP ::
  ClosedTerm PTokenName ->
  ClosedTerm PPositive ->
  ClosedTerm PTxOutRef ->
  Run OneShotMP
getOneShotMP tn amt txOutRef =
  extract $
    mkTypedPolicyPlutarch tracingConfig $
      pMkOneShotMintingPolicy # tn # amt # txOutRef

type StakePoolTokenMP = TypedPolicy ()

getStakePoolTokenMP ::
  PlutarchTypedScript (PBasketStateCS :--> PMintingPolicy) ->
  ClosedTerm PBasketStateCS ->
  Run StakePoolTokenMP
getStakePoolTokenMP stakePoolMP basketStateCS =
  mkTypedPolicyPlutarchTypedScript
    <$> extract
      (stakePoolMP `apply` basketStateCS)
  where
    apply = applyPlutarchTypedScript tracingConfig

type BasketTokenMP = TypedPolicy ()

getBasketTokenMP ::
  ClosedTerm PStakingPoolCS ->
  Run BasketTokenMP
getBasketTokenMP stakingPoolCS =
  extract $
    mkTypedPolicyPlutarch tracingConfig $
      pMkBasketTokenMP # stakingPoolCS
