{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Suites.Atma.Deposit.Transactions (
  depositContract'ConsumeMultipleStakePool,
  depositContract'MintMore,
  depositContract'StealAda,
  depositContract'StealStakePoolCS,
  depositContract'SuccessfulDeposit,
  depositToStakePoolUtxo,
) where

import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Reader.Class (ask)
import Data.List.NonEmpty qualified as NE
import GHC.Base (coerce)

import Plutus.Model (
  DatumMode (InlineDatum),
  mintValue,
  payToKey,
  submitTx,
 )
import Plutus.Model.V2 (
  adaValue,
  payToRef,
  refInputHash,
  scriptCurrencySymbol,
  spendScriptRef,
  userSpend,
 )

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  PubKeyHash (PubKeyHash),
  TxOut (txOutValue),
  TxOutRef,
  singleton,
 )

import Atma.PTypes (
  PBasketValidatorRedeemer (MkPDeposit),
  PStakePoolUtxoDatum,
 )
import Atma.Types (
  BasketToken (MkBasketToken),
  BasketTokenCS (MkBasketTokenCS),
  BasketTokenTN (getBasketTokenTN),
  basketTokenCS,
 )

import Atma.Utils.ExRate (
  PCoin (MkPCoin),
  plovelaceToBasketTokens,
 )

import Atma.Scripts.Common (stakePoolTN)
import Suites.Atma.Common (
  incrementBasketTokenCounter,
  mkPStakePoolUtxoDatum,
 )
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    Ctx,
    basketParams,
    basketTokenMP,
    basketValidatorStakePoolUtxo,
    basketValidatorUtxo,
    stakingPoolMP
  ),
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
 )
import Suites.Atma.Common.Utils (
  User (User),
  basketTokenTN,
  findBasketStateUtxo,
  findStakePoolUtxos,
  spend,
  tryFromMaybe,
 )

depositToStakePoolUtxo ::
  User ->
  Integer ->
  ((TxOutRef, TxOut), PlutarchAdapter PStakePoolUtxoDatum) ->
  BasketContext ()
depositToStakePoolUtxo
  user@(User userPkh)
  amt
  (stakePoolUtxo, PlutarchAdapter stakePoolDatum) = do
    (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

    ctx <- ask

    let scriptRef = basketValidatorUtxo ctx
        MkBasketToken tokensAmount =
          plift $
            plovelaceToBasketTokens
              # pfromData (pfield @"exRate" # pto basketState)
              # pcon (MkPCoin $ pconstant amt)
        mintedValue =
          singleton
            (coerce $ basketTokenCS $ basketParams ctx)
            (getBasketTokenTN basketTokenTN)
            tokensAmount

    userSpend' <-
      lift $
        spend (Just "user deposit") (coerce user) $
          adaValue amt

    lift $
      submitTx (coerce user) $
        mconcat
          [ mintValue (basketTokenMP ctx) () mintedValue
          , userSpend userSpend'
          , payToRef
              (basketValidatorStakePoolUtxo ctx)
              (InlineDatum $ PlutarchAdapter $ incrementBasketTokenCounter stakePoolDatum tokensAmount)
              (txOutValue (snd stakePoolUtxo) <> adaValue amt)
          , payToKey userPkh mintedValue
          , spendScriptRef
              scriptRef
              (basketValidatorStakePoolUtxo ctx)
              (fst stakePoolUtxo)
              (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
              (PlutarchAdapter stakePoolDatum)
          , refInputHash
              (fst basketStateUtxo)
              (PlutarchAdapter basketState)
          ]

depositContract'SuccessfulDeposit :: User -> Integer -> BasketContext ()
depositContract'SuccessfulDeposit user amt =
  depositToStakePoolUtxo user amt . NE.head =<< findStakePoolUtxos

depositContract'StealAda :: User -> Integer -> BasketContext ()
depositContract'StealAda user@(User userPkh) amt = do
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

  ctx <- ask

  stakePoolUtxos <- findStakePoolUtxos

  let (stakePoolUtxo, stakePoolDatum) = NE.head stakePoolUtxos
      scriptRef = basketValidatorUtxo ctx

  lift $
    submitTx (coerce user) $
      mconcat
        [ payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum $ PlutarchAdapter $ mkPStakePoolUtxoDatum 0)
            (txOutValue (snd stakePoolUtxo) <> adaValue (negate amt))
        , payToKey userPkh (adaValue amt)
        , spendScriptRef
            scriptRef
            (basketValidatorStakePoolUtxo ctx)
            (fst stakePoolUtxo)
            (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
            stakePoolDatum
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]

depositContract'MintMore :: User -> Integer -> BasketContext ()
depositContract'MintMore user@(User userPkh) amt = do
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

  ctx <- ask

  stakePoolUtxos <- findStakePoolUtxos

  let (stakePoolUtxo, stakePoolDatum) = NE.head stakePoolUtxos
      scriptRef = basketValidatorUtxo ctx
      MkBasketToken tokensAmount =
        plift $
          plovelaceToBasketTokens
            # pfromData (pfield @"exRate" # pto basketState)
            # pcon (MkPCoin $ pconstant amt)
      mintedValue =
        singleton
          (coerce $ basketTokenCS $ basketParams ctx)
          (getBasketTokenTN basketTokenTN)
          (tokensAmount + 100)

  userSpend' <-
    lift $
      spend (Just "user deposit") (coerce user) $
        adaValue amt

  lift $
    submitTx (coerce user) $
      mconcat
        [ mintValue (basketTokenMP ctx) () mintedValue
        , userSpend userSpend'
        , payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum stakePoolDatum)
            (txOutValue (snd stakePoolUtxo) <> adaValue amt)
        , payToKey userPkh mintedValue
        , spendScriptRef
            scriptRef
            (basketValidatorStakePoolUtxo ctx)
            (fst stakePoolUtxo)
            (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
            stakePoolDatum
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]

depositContract'ConsumeMultipleStakePool :: User -> Integer -> BasketContext ()
depositContract'ConsumeMultipleStakePool user@(User userPkh) amt = do
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

  ctx <- ask

  stakePoolUtxos <- findStakePoolUtxos

  let (stakePoolUtxo, stakePoolDatum) = NE.head stakePoolUtxos
      MkBasketToken tokensAmount =
        plift $
          plovelaceToBasketTokens
            # pfromData (pfield @"exRate" # pto basketState)
            # pcon (MkPCoin $ pconstant amt)
      mintedValue =
        singleton
          (coerce $ basketTokenCS $ basketParams ctx)
          (getBasketTokenTN basketTokenTN)
          tokensAmount

  stakePoolUtxosAfter <-
    lift
      . tryFromMaybe
        "There's only one stake pool utxo present at the basket validator."
      . NE.nonEmpty
      . NE.filter ((/= stakePoolUtxo) . fst)
      =<< findStakePoolUtxos

  let (stakePoolUtxo2, stakePoolDatum2) = NE.head stakePoolUtxosAfter
      scriptRef = basketValidatorUtxo ctx

  userSpend' <-
    lift $
      spend (Just "user deposit") (coerce user) $
        adaValue amt

  lift $
    submitTx (coerce user) $
      mconcat
        [ mintValue (basketTokenMP ctx) () mintedValue
        , userSpend userSpend'
        , payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum stakePoolDatum)
            (txOutValue (snd stakePoolUtxo) <> adaValue amt)
        , payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum stakePoolDatum2)
            (txOutValue (snd stakePoolUtxo2))
        , payToKey userPkh mintedValue
        , spendScriptRef
            scriptRef
            (basketValidatorStakePoolUtxo ctx)
            (fst stakePoolUtxo)
            (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
            stakePoolDatum
        , spendScriptRef
            scriptRef
            (basketValidatorStakePoolUtxo ctx)
            (fst stakePoolUtxo2)
            (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
            stakePoolDatum2
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]

depositContract'StealStakePoolCS :: User -> Integer -> BasketContext ()
depositContract'StealStakePoolCS user@(User userPkh) amt = do
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

  ctx@Ctx {stakingPoolMP} <- ask

  stakePoolUtxos <- findStakePoolUtxos

  let (stakePoolUtxo, stakePoolDatum) = NE.head stakePoolUtxos
      scriptRef = basketValidatorUtxo ctx
      MkBasketToken tokensAmount =
        plift $
          plovelaceToBasketTokens
            # pfromData (pfield @"exRate" # pto basketState)
            # pcon (MkPCoin $ pconstant amt)
      mintedValue =
        singleton
          (coerce $ basketTokenCS $ basketParams ctx)
          (getBasketTokenTN basketTokenTN)
          tokensAmount

      stakePoolTkn = singleton (scriptCurrencySymbol stakingPoolMP) stakePoolTN

  userSpend' <-
    lift $
      spend (Just "user deposit") (coerce user) $
        adaValue amt

  lift $
    submitTx (coerce user) $
      mconcat
        [ mintValue (basketTokenMP ctx) () mintedValue
        , userSpend userSpend'
        , payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum stakePoolDatum)
            (txOutValue (snd stakePoolUtxo) <> adaValue amt <> stakePoolTkn (-1))
        , payToKey userPkh (mintedValue <> stakePoolTkn 1)
        , spendScriptRef
            scriptRef
            (basketValidatorStakePoolUtxo ctx)
            (fst stakePoolUtxo)
            (PlutarchAdapter $ pcon $ MkPDeposit pdnil)
            stakePoolDatum
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]
