module Suites.Atma.UpdateExRate.Transactions (
  updateExRate,
  increaseExRateToAtLeast,
) where

import Atma.PTypes (
  PBasketValidatorRedeemer (
    MkPUpdateExRate
  ),
 )
import Atma.Types (
  AdminPubKeyHash (MkAdminPubKeyHash),
  BasketToken (MkBasketToken),
  ExRate (MkExRate),
  getExRate,
 )
import Control.Lens (
  (&),
  (.~),
 )
import Control.Monad (when)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (lift)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Data.Ratio qualified as R
import Plutus.Model.V2 (
  Ada (Lovelace),
  DatumMode (HashDatum),
  getLovelace,
  payToRef,
  refInputInline,
  spendScriptRef,
  submitTx,
 )
import PlutusLedgerApi.V2.Tx (
  TxOut (txOutValue),
 )
import Suites.Atma.Common (Utxo (MkUtxo), stakePoolUtxosSummary)
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    basketValidatorBasketStateUtxo,
    basketValidatorUtxo
  ),
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (
    PlutarchAdapter
  ),
 )
import Suites.Atma.Common.Utils (
  User,
  findBasketStateUtxo,
  findStakePoolUtxos,
  _exRate,
 )
import Suites.Atma.Donate.Transactions (donate)

updateExRate :: BasketContext ()
updateExRate = do
  ctx <- ask
  let
    bsScript = basketValidatorBasketStateUtxo ctx
    scriptRef = basketValidatorUtxo ctx
  ((ref, out), state@(PlutarchAdapter basketState)) <- findBasketStateUtxo
  let MkAdminPubKeyHash admin =
        plift $ pfromData $ pfield @"adminPkh" # pto basketState
  spUtxos <- findStakePoolUtxos
  let (totalSpendableCoin, totalBasketToken) =
        stakePoolUtxosSummary $ MkUtxo <$> spUtxos
      exRate =
        MkExRate $
          coerce totalSpendableCoin R.% coerce totalBasketToken
  lift $
    submitTx admin $
      mconcat
        [ spendScriptRef
            scriptRef
            bsScript
            ref
            (PlutarchAdapter $ pcon $ MkPUpdateExRate pdnil)
            state
        , foldMap (refInputInline . fst . fst) spUtxos
        , payToRef
            bsScript
            (HashDatum $ state & _exRate .~ exRate)
            (txOutValue out)
        ]

errorIfExRateIsSmallerThan :: ExRate -> BasketContext ()
errorIfExRateIsSmallerThan desiredExRate = do
  spUtxos <- fmap MkUtxo <$> findStakePoolUtxos
  let (totalAda, totalBt) = stakePoolUtxosSummary spUtxos
      effectiveExRate = MkExRate $ coerce totalAda R.% coerce totalBt

  when (effectiveExRate < desiredExRate) $
    error $
      "Wrong resulting ex rate: "
        <> show (effectiveExRate, desiredExRate)

increaseExRateToAtLeast :: User -> ExRate -> BasketContext ()
increaseExRateToAtLeast user desiredExRate = do
  spUtxos <- fmap MkUtxo <$> findStakePoolUtxos
  let
    (totalAda, totalBt) = stakePoolUtxosSummary spUtxos
    donation =
      computeDonation
        totalAda
        totalBt

  when (getLovelace donation > 0) $ do
    donate user donation $ NE.head spUtxos
    updateExRate

  errorIfExRateIsSmallerThan desiredExRate
  where
    computeDonation :: Ada -> BasketToken -> Ada
    computeDonation
      (Lovelace totalAda)
      (MkBasketToken totalBt) =
        ceiling $ getExRate desiredExRate * (totalBt R.% 1) - (totalAda R.% 1)
