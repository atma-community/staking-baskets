{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.Withdraw.Transactions (
  withdrawContract'SuccesfulWithdraw,
  withdrawContract'ConsumeMultipleStakePoolUtxo,
  withdrawContract'WithdrawMore,
  withdrawContract'StealStakePoolToken,
  withdrawContract'StealBasketStateToken,
  withdrawContract'ConsumeExtraBasketValidatorUtxo,
  withdrawContract'UnderdecrementCounter,
  withdrawContract'StealBasketToken,
) where

import Atma.PTypes (
  PBasketValidatorRedeemer (MkPWithdraw),
  PStakePoolUtxoDatum,
 )
import Atma.Scripts.Common (stakePoolTN)
import Atma.Types (
  BasketToken (MkBasketToken),
  BasketTokenCS (MkBasketTokenCS),
  BasketTokenTN (getBasketTokenTN),
  Coin (MkCoin),
  basketTokenCS,
 )
import Atma.Types qualified as Atma
import Atma.Utils.ExRate (
  PCoin (MkPCoin),
  pbasketTokensToLovelace,
  plovelaceToBasketTokens,
 )
import Control.Lens (
  (+~),
  (^.),
 )
import Control.Monad (when)
import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Reader.Class (ask)
import Data.Foldable (
  find,
  maximumBy,
 )
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import GHC.Base (coerce)
import Plutarch.Test.QuickCheck.Instances ()
import Plutus.Model (
  DatumMode (InlineDatum),
  adaOf,
  mintValue,
  payToKey,
  submitTx,
 )
import Plutus.Model.V2 (
  Ada (Lovelace),
  Tx,
  TypedValidator,
  ada,
  adaValue,
  payToRef,
  refInputHash,
  spend,
  spendScriptRef,
  userSpend,
  utxoAt,
 )
import PlutusLedgerApi.V2 (
  PubKeyHash (PubKeyHash),
  TxOut (txOutValue),
  TxOutRef,
  Value,
  singleton,
 )
import Suites.Atma.Common (
  incrementBasketTokenCounter,
  lovelaceValueOf,
  spendableCoin,
 )
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    basketParams,
    basketTokenMP,
    basketValidatorBasketStateUtxo,
    basketValidatorStakePoolUtxo,
    basketValidatorUtxo
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
  _basketTokenCounter,
  _exRate,
 )
import Unsafe.Coerce (unsafeCoerce)

data TxRecipe = MkTxRecipe
  { extraConstraints :: Tx
  , -- modify stake pool datum after it has been built
    withOutStakePoolDatum ::
      PlutarchAdapter PStakePoolUtxoDatum ->
      PlutarchAdapter PStakePoolUtxoDatum
  , selectStakePool ::
      BasketContext ((TxOutRef, TxOut), PlutarchAdapter PStakePoolUtxoDatum)
  , extraOutStakePoolValue :: Value
  , extraValueToUser :: Value
  , extraMint :: Value
  }

correctTxRecipe :: TxRecipe
correctTxRecipe =
  MkTxRecipe
    { extraConstraints = mempty
    , selectStakePool =
        maximumBy (comparing $ lovelaceValueOf . txOutValue . snd . fst)
          <$> findStakePoolUtxos
    , extraOutStakePoolValue = mempty
    , extraValueToUser = mempty
    , withOutStakePoolDatum = id
    , extraMint = mempty
    }

withdrawWith :: TxRecipe -> User -> Ada -> BasketContext ()
withdrawWith
  MkTxRecipe {..}
  user@(User userPkh)
  amt = do
    ctx <- ask
    (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

    (stakePoolUtxo, PlutarchAdapter stakePoolDatum) <- selectStakePool

    let basketTokenValue =
          singleton
            (coerce $ basketTokenCS $ basketParams ctx)
            (getBasketTokenTN basketTokenTN)
        scriptRef = basketValidatorUtxo ctx
    let maxToSpend = spendableCoin $ txOutValue $ snd stakePoolUtxo
    (adaToWithdraw, bt@(MkBasketToken burnBasketToken)) <-
      calculateEquivCurrencies $
        min amt maxToSpend

    userSpend' <-
      lift $
        spend (coerce user) $
          basketTokenValue burnBasketToken
    let toMint =
          extraMint
            <> basketTokenValue (negate burnBasketToken)
    when (negate burnBasketToken >= 0) do
      let exRate = PlutarchAdapter basketState ^. _exRate
      error $ show (adaToWithdraw, bt, exRate, (amt, maxToSpend))
    lift $
      submitTx (coerce user) $
        mconcat
          [ if toMint == mempty
              then mempty
              else mintValue (basketTokenMP ctx) () toMint
          , userSpend userSpend'
          , payToRef
              (basketValidatorStakePoolUtxo ctx)
              ( InlineDatum $
                  withOutStakePoolDatum $
                    PlutarchAdapter $
                      incrementBasketTokenCounter stakePoolDatum (-burnBasketToken)
              )
              $ mconcat
                [ txOutValue (snd stakePoolUtxo)
                , ada (-adaToWithdraw)
                , extraOutStakePoolValue
                ]
          , payToKey userPkh $
              mconcat
                [ ada adaToWithdraw
                , extraValueToUser
                ]
          , spendScriptRef
              scriptRef
              (basketValidatorStakePoolUtxo ctx)
              (fst stakePoolUtxo)
              (PlutarchAdapter $ pcon $ MkPWithdraw pdnil)
              (PlutarchAdapter stakePoolDatum)
          , refInputHash
              (fst basketStateUtxo)
              (PlutarchAdapter basketState)
          , extraConstraints
          ]

-- FIXME: this is just the trick to bypass the bug
calculateEquivCurrencies :: Ada -> BasketContext (Ada, BasketToken)
calculateEquivCurrencies rawAda = do
  basketTokens <- currentBasketTokenExRate rawAda
  ada <- currentAdaExRate basketTokens
  pure (ada, basketTokens)

currentBasketTokenExRate :: Ada -> BasketContext BasketToken
currentBasketTokenExRate amt = do
  (_, PlutarchAdapter basketState) <- findBasketStateUtxo

  let basketTokens =
        plift $
          plovelaceToBasketTokens
            # (pfield @"exRate" # basketState)
            # pcon (MkPCoin $ pconstant $ coerce amt)

  pure basketTokens

currentAdaExRate :: BasketToken -> BasketContext Ada
currentAdaExRate amt = do
  (_, PlutarchAdapter basketState) <- findBasketStateUtxo

  let MkCoin lovelace =
        plift $
          pbasketTokensToLovelace
            # (pfield @"exRate" # basketState)
            # pconstant amt

  pure $ Lovelace lovelace

withdrawContract'SuccesfulWithdraw :: User -> Ada -> BasketContext ()
withdrawContract'SuccesfulWithdraw = withdrawWith correctTxRecipe

withdrawContract'UnderdecrementCounter :: User -> Ada -> BasketContext ()
withdrawContract'UnderdecrementCounter =
  withdrawWith
    correctTxRecipe
      { withOutStakePoolDatum = _basketTokenCounter +~ 20
      }

withdrawContract'StealBasketToken :: User -> Ada -> BasketContext ()
withdrawContract'StealBasketToken user amt = do
  ctx <- ask
  let basketTokenValue =
        singleton
          (coerce $ basketTokenCS $ basketParams ctx)
          (getBasketTokenTN basketTokenTN)
  withdrawWith
    correctTxRecipe
      { extraMint = basketTokenValue 1
      , extraValueToUser = basketTokenValue 1
      }
    user
    amt

withdrawContract'WithdrawMore :: User -> Ada -> BasketContext ()
withdrawContract'WithdrawMore =
  let withdrawMoreAmt = 100
   in withdrawWith
        correctTxRecipe
          { extraOutStakePoolValue = adaValue (-withdrawMoreAmt)
          , extraValueToUser = adaValue withdrawMoreAmt
          }

untypedValidator ::
  forall c d a b.
  TypedValidator a b ->
  TypedValidator c d
untypedValidator = unsafeCoerce

withdrawContract'ConsumeExtraBasketValidatorUtxo ::
  User ->
  Ada ->
  BasketContext ()
withdrawContract'ConsumeExtraBasketValidatorUtxo user@(User userPkh) amt = do
  userSpend' <- lift $ spend userPkh $ adaValue 200
  ctx <- ask
  let
    scriptRef = basketValidatorUtxo ctx
    untypedBasketValidator =
      untypedValidator @() @(PlutarchAdapter PBasketValidatorRedeemer) $
        basketValidatorStakePoolUtxo ctx
  lift $
    submitTx (coerce user) $
      mconcat
        [ userSpend userSpend'
        , payToRef
            untypedBasketValidator
            (InlineDatum ())
            (adaValue 200)
        ]
  validatorUtxos <- lift $ utxoAt (basketValidatorStakePoolUtxo ctx)
  Just extraUtxo <- pure $ flip find validatorUtxos \utxo ->
    ada (adaOf $ txOutValue $ snd utxo) == txOutValue (snd utxo)
  withdrawWith
    correctTxRecipe
      { extraConstraints =
          mconcat
            [ spendScriptRef
                scriptRef
                untypedBasketValidator
                (fst extraUtxo)
                (PlutarchAdapter $ pcon $ MkPWithdraw pdnil)
                ()
            , payToKey userPkh $ txOutValue $ snd extraUtxo
            ]
      }
    user
    amt

withdrawContract'StealStakePoolToken :: User -> Ada -> BasketContext ()
withdrawContract'StealStakePoolToken user amt = do
  ctx <- ask
  let stakePoolTokenValue =
        singleton
          (coerce $ Atma.stakingPoolCS $ basketParams ctx)
          stakePoolTN
  withdrawWith
    correctTxRecipe
      { extraOutStakePoolValue = stakePoolTokenValue (-1)
      , extraValueToUser = stakePoolTokenValue 1
      }
    user
    amt

withdrawContract'StealBasketStateToken :: User -> Ada -> BasketContext ()
withdrawContract'StealBasketStateToken user amt = do
  ctx <- ask
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo
  let
    scriptRef = basketValidatorUtxo ctx
    basketUtxoValue = txOutValue $ snd basketStateUtxo

  withdrawWith
    correctTxRecipe
      { extraConstraints =
          spendScriptRef
            scriptRef
            (basketValidatorBasketStateUtxo ctx)
            (fst basketStateUtxo)
            (PlutarchAdapter $ pcon $ MkPWithdraw pdnil)
            (PlutarchAdapter basketState)
      , extraValueToUser = basketUtxoValue
      }
    user
    amt

withdrawContract'ConsumeMultipleStakePoolUtxo :: User -> Ada -> BasketContext ()
withdrawContract'ConsumeMultipleStakePoolUtxo user amt = do
  ctx <- ask

  mainStakePool NE.:| otherStakePool : _ <-
    NE.sortBy (flip $ comparing $ lovelaceValueOf . txOutValue . snd . fst)
      <$> findStakePoolUtxos

  let
    (stakePoolUtxoOther, stakePoolUtxoDatumOther) = otherStakePool
    scriptRef = basketValidatorUtxo ctx
  withdrawWith
    correctTxRecipe
      { selectStakePool = pure mainStakePool
      , extraConstraints =
          mconcat
            [ spendScriptRef
                scriptRef
                (basketValidatorStakePoolUtxo ctx)
                (fst stakePoolUtxoOther)
                (PlutarchAdapter $ pcon $ MkPWithdraw pdnil)
                stakePoolUtxoDatumOther
            , payToRef
                (basketValidatorStakePoolUtxo ctx)
                (InlineDatum stakePoolUtxoDatumOther)
                (txOutValue $ snd stakePoolUtxoOther)
            ]
      }
    user
    amt
