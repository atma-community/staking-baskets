module Suites.Atma.Donate.Transactions (
  donate,
) where

import Atma.Types (
  AdminPubKeyHash (MkAdminPubKeyHash),
 )

import Atma.PTypes (
  PBasketValidatorRedeemer (MkPDonate),
  PStakePoolUtxoDatum,
 )
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans (lift)
import Plutus.Model.V2 (
  Ada,
  DatumMode (InlineDatum),
  ada,
  payToRef,
  refInputHash,
  spendScriptRef,
  submitTx,
  userSpend,
 )
import PlutusLedgerApi.V2.Tx (TxOut (txOutValue))
import Suites.Atma.Common (
  Utxo (MkUtxo),
 )
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (basketValidatorStakePoolUtxo, basketValidatorUtxo),
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
 )
import Suites.Atma.Common.Utils (
  User (User),
  findBasketStateUtxo,
  spend,
 )

donate :: User -> Ada -> Utxo (PlutarchAdapter PStakePoolUtxoDatum) -> BasketContext ()
donate (User _) amount (MkUtxo ((ref, out), datum)) = do
  ctx <- ask

  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo

  let MkAdminPubKeyHash admin =
        plift $ pfromData $ pfield @"adminPkh" # pto basketState

  userSpend' <- lift $ spend (Just "Donation") admin $ ada amount

  lift $
    submitTx admin $
      mconcat
        [ spendScriptRef
            (basketValidatorUtxo ctx)
            (basketValidatorStakePoolUtxo ctx)
            ref
            (PlutarchAdapter $ pcon $ MkPDonate pdnil)
            datum
        , userSpend userSpend'
        , payToRef
            (basketValidatorStakePoolUtxo ctx)
            (InlineDatum datum)
            (txOutValue out <> ada amount)
        , refInputHash
            (fst basketStateUtxo)
            (PlutarchAdapter basketState)
        ]
