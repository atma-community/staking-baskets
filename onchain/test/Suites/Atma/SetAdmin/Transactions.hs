module Suites.Atma.SetAdmin.Transactions (setAdmin) where

import PlutusLedgerApi.V1 (PubKeyHash)

import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Reader.Class (ask)

import Plutus.Model (
  DatumMode (HashDatum),
  signTx,
  submitTx,
 )
import Plutus.Model.V2 (
  payToRef,
  spendScriptRef,
 )

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  TxOut (txOutValue),
 )

import Atma.PTypes (
  PAdminPubKeyHash (MkPAdminPubKeyHash),
  PBasketState,
  PBasketValidatorRedeemer (
    MkPSetAdmin
  ),
 )

import Atma.Utils (pupdateAdminPubKeyHash)
import Control.Monad (foldM)
import Data.Coerce (coerce)
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    basketValidatorUtxo
  ),
  basketValidatorBasketStateUtxo,
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
 )
import Suites.Atma.Common.Utils (
  Admin (Admin),
  findBasketStateUtxo,
 )

psetAdminRedeemer :: Term s PBasketValidatorRedeemer
psetAdminRedeemer = pcon $ MkPSetAdmin pdnil

updateAdmin :: Admin -> ClosedTerm PBasketState -> ClosedTerm PBasketState
updateAdmin (Admin admin) basketState =
  pupdateAdminPubKeyHash
    # basketState
      #$ pcon
    $ MkPAdminPubKeyHash
    $ pconstant admin

setAdmin :: Admin -> [PubKeyHash] -> BasketContext ()
setAdmin newAdmin voters = do
  ctx <- ask
  let scriptRef = basketValidatorUtxo ctx
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo
  let tx =
        mconcat
          [ spendScriptRef
              scriptRef
              (basketValidatorBasketStateUtxo ctx)
              (fst basketStateUtxo)
              (PlutarchAdapter psetAdminRedeemer)
              (PlutarchAdapter basketState)
          , payToRef
              (basketValidatorBasketStateUtxo ctx)
              (HashDatum $ PlutarchAdapter $ updateAdmin newAdmin basketState)
              (txOutValue $ snd basketStateUtxo)
          ]
  signedTx <- lift $ foldM (flip signTx) tx voters

  lift $ submitTx (coerce newAdmin) signedTx
