module Atma.Scripts.BasketValidator.Donate (pvalidateDonate) where

import Atma.PTypes (PBasketParams)
import Atma.Scripts.Common (pstakePoolTN)
import Atma.Utils (
  datumDoesNotChange,
  findInputAtPaymentCredential,
  findOutputAtPaymentCredential,
  perrorIfFalse,
  pextractBasketState,
  pgetOwnTxOut,
  psignedBy,
  ptokenInUtxo,
 )
import Atma.Utils.ExRate (PCoin (MkPCoin))

import Plutarch.Api.V1.Value (
  padaSymbol,
  padaToken,
  plovelaceValueOf,
  pnormalize,
 )
import Plutarch.Api.V1.Value qualified as V
import Plutarch.Api.V2 (PScriptContext)
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#-))

{- | 'pvalidateDonate' is used to validate donation to the basket.
     This corresponds to `MkPDonate` redeemer.

     When donating a user puts ADA into a basket but doesn't get
     any basket tokens in return.

     Donation is used by the admin when collecting rewards.
     Staking rewards accrued by Cardano are donated to
     stake pool utxos in Reward period of each cycle.
     Donation is allowed throughout the whole cycle
     and by any user.

     Following checks are performed:
     - No tokens are minted nor burnt
     - Only one utxo from basket address is consumed
     - And that utxo carries stake pool token
     - Stake pool utxo is propagated
     - And its address is preserved
     - And its datum is preserved
     - Amount of ada on stake pool utxo increases
-}
pvalidateDonate :: ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
pvalidateDonate = phoistAcyclic $ plam $ \basketParams' ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txInfo <- pletFields @'["inputs", "mint", "outputs", "referenceInputs", "signatories"] $ ctx.txInfo
  stakingPoolCS <- plet $ pfromData $ pfield @"stakingPoolCS" # basketParams'

  let mint = pnormalize # txInfo.mint
      signatories = txInfo.signatories

  perrorIfFalse
    "Minting not allowed"
    (mint #== mempty)

  ownResolved <- plet $ pgetOwnTxOut # ctx.purpose # txInfo.inputs
  ownAddress <- plet $ pfromData $ pfield @"address" # ownResolved
  ownCredential <- plet $ pfield @"credential" # ownAddress

  let fromBasketValidator =
        findInputAtPaymentCredential
          # ownCredential
          # txInfo.inputs

      referencesFromBasketValidator =
        findInputAtPaymentCredential
          # ownCredential
          # txInfo.referenceInputs

      basketStateUtxoResolved = P.do
        PCons basketStateUtxo _ <- pmatch referencesFromBasketValidator
        pfield @"resolved" # basketStateUtxo

      basketState =
        pextractBasketState # (pfield @"datums" # ctx.txInfo) # basketStateUtxoResolved

  let adminPkh = pfromData $ pfield @"adminPkh" # pto basketState

  perrorIfFalse
    "Not signed by an administrator"
    (psignedBy # pto adminPkh # signatories)

  perrorIfFalse
    "Spending more than one UTxO from basket validator address"
    ((plength # fromBasketValidator) #== 1)

  perrorIfFalse
    "No token at stake pool utxo"
    (ptokenInUtxo # pto stakingPoolCS # pstakePoolTN # ownResolved)

  toBasketValidator <-
    plet $
      findOutputAtPaymentCredential
        # ownCredential
        # txInfo.outputs

  -- just for simplicity of validation
  perrorIfFalse
    "More than one output utxo at own address"
    ((plength # toBasketValidator) #== 1)

  PCons outputUtxo _ <- pmatch toBasketValidator

  perrorIfFalse
    "No token at output staking pool utxo"
    (ptokenInUtxo # pto stakingPoolCS # pstakePoolTN # outputUtxo)

  perrorIfFalse
    "Staking credential changed at output staking pool utxo"
    (pfield @"address" # outputUtxo #== ownAddress)

  ownValue <- plet $ pnormalize #$ pfield @"value" # ownResolved
  outputValue <- plet $ pnormalize #$ pfield @"value" # outputUtxo

  let inputCoin = plovelaceValueOf # ownValue
      outputCoin = plovelaceValueOf # outputValue
  donation <- plet $ pcon $ MkPCoin (outputCoin #- inputCoin)

  let expectedOutputValue =
        ownValue <> (V.psingleton # padaSymbol # padaToken # pto donation)

  perrorIfFalse
    "Propagated stake pool utxo value can differ only by lovelace"
    (outputValue #== expectedOutputValue)

  perrorIfFalse
    "The datum of stake pool utxo should not change"
    (datumDoesNotChange # ownResolved # outputUtxo)

  perrorIfFalse
    "Amount of ada should increase after donation"
    (0 #< donation)

  popaque $ pconstant ()
