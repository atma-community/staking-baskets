{-# LANGUAGE BlockArguments #-}

module Atma.Scripts.BasketValidator (
  pMkBasketValidator,
  pMkBasketValidatorUntyped,
  basketValidatorScript,
) where

import Data.Default (def)
import Flat.Types (Text)

import Plutarch (
  Config (tracingMode),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V2.Contexts (PScriptContext)

import Atma.PTypes (
  PBasketParams,
  PBasketValidatorRedeemer (
    MkPDeposit,
    MkPDonate,
    MkPRebalance,
    MkPRebalanceDelegate,
    MkPSetAdmin,
    MkPStakePoolMPTriggerWitness,
    MkPSwitchBasketLock,
    MkPSwitchPledgeLock,
    MkPUpdateExRate,
    MkPWithdraw
  ),
 )
import Atma.Utils (
  ptryFromUndata,
 )

import Atma.Scripts.BasketValidator.DepositWithdraw (
  depositExchangeAssert,
  depositMintingAssert,
  depositPledgeLockAssert,
  pvalidateWith,
  withdrawExchangeAssert,
  withdrawMintingAssert,
  withdrawPledgeLockAssert,
 )
import Atma.Scripts.BasketValidator.Donate (pvalidateDonate)
import Atma.Scripts.BasketValidator.Rebalance (
  pvalidateRebalance,
  pvalidateRebalanceDelegate,
 )
import Atma.Scripts.BasketValidator.SetAdmin (psetAdmin)
import Atma.Scripts.BasketValidator.SwitchLock (pswitchBasketLock, pswitchPledgeLock)
import Atma.Scripts.BasketValidator.UpdateExRate (
  pvalidateUpdateExRate,
 )
import Atma.Scripts.StakePoolTokenMP (
  pvalidateStakePoolMPTriggerWitness,
 )

pMkBasketValidator ::
  ClosedTerm
    ( PBasketParams
        :--> PData
        :--> PBasketValidatorRedeemer
        :--> PScriptContext
        :--> POpaque
    )
pMkBasketValidator = plam $ \basketParams _ redeemer ctx -> P.do
  pmatch redeemer $ \case
    MkPDeposit _ ->
      pvalidateWith
        depositMintingAssert
        depositExchangeAssert
        depositPledgeLockAssert
        # basketParams
        # ctx
    MkPWithdraw _ ->
      pvalidateWith
        withdrawMintingAssert
        withdrawExchangeAssert
        withdrawPledgeLockAssert
        # basketParams
        # ctx
    MkPUpdateExRate _ ->
      pvalidateUpdateExRate
        # basketParams
        # ctx
    MkPRebalance _ ->
      pvalidateRebalance
        # basketParams
        # ctx
    MkPDonate _ ->
      pvalidateDonate
        # basketParams
        # ctx
    MkPRebalanceDelegate _ ->
      pvalidateRebalanceDelegate
        # ctx
    MkPStakePoolMPTriggerWitness _ ->
      pvalidateStakePoolMPTriggerWitness
        # basketParams
        # ctx
    MkPSwitchBasketLock switch ->
      pswitchBasketLock
        # basketParams
        # (pfield @"atTime" # switch)
        # ctx
    MkPSwitchPledgeLock switch ->
      pswitchPledgeLock
        # basketParams
        # (pfield @"atTime" # switch)
        # ctx
    MkPSetAdmin _ ->
      psetAdmin
        # basketParams
        # ctx

pMkBasketValidatorUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkBasketValidatorUntyped = plam $ \basketParams datum redeemer ->
  pMkBasketValidator
    # unTermCont (ptryFromUndata basketParams)
    # unTermCont (ptryFromUndata datum)
    # unTermCont (ptryFromUndata redeemer)

basketValidatorScript :: Either Text Script
basketValidatorScript =
  compile (def {tracingMode = DoTracing}) pMkBasketValidatorUntyped
