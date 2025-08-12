module Atma.Scripts.BasketValidator.UpdateExRate (pvalidateUpdateExRate) where

import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Monadic qualified as P

import Atma.PTypes (
  PBasketParams,
 )
import Atma.Utils (
  PBasketStateUtxoValidation (MkPBasketStateUtxoValidation),
  ensureOnlyBasketStateUtxoConsumed,
  perrorIfFalse,
  psignedBy,
  pupdateExRate,
 )

import Atma.Scripts.BasketValidator.Rebalance (pvisitInputSPUtxo)
import Atma.Scripts.Common (pbasketStateTN)
import Atma.Utils.ExRate (PBasketToken, PCoin, PExRate (MkPExRate))
import GHC.Generics qualified as GHC (Generic)
import Plutarch.Api.V2 (PTxInInfo)
import Plutarch.Rational (pfromInteger, (#/))

{- |  `pvalidateUpdateExRate` is used to validate `MkPUpdateExRate` redeemer

      Following checks are performed:
      -
      - Transaction is signed by the administrator
      - No tokens are minted nor burnt
      - Only one utxo from basket address is consumed
      - And that utxo carries basket state token
      - All existing stake pool utxos are provided as reference inputs
      - Basket state utxo is propagated
      - Basket state datum is updated correctly and invariants are satisfied:
        - echange rate is monotonically growing
        - the update happens at most once per cycle
        - new exchange rate matches the exchange rate computed onchain
        - other fields of Basket state datum do not change
-}
pvalidateUpdateExRate :: ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
pvalidateUpdateExRate = plam $ \basketParams' ctx' -> P.do
  ctx <- pletFields @'["purpose", "txInfo"] ctx'
  txInfo <-
    pletFields
      @'[ "inputs"
        , "mint"
        , "outputs"
        , "signatories"
        , "validRange"
        , "referenceInputs"
        ]
      ctx.txInfo

  let mint = pnormalize # txInfo.mint
      signatories = txInfo.signatories

  basketParam <- pletFields @'["adminPkh", "basketStateCS", "lockingParams", "stakingPoolCS"] basketParams'

  MkPBasketStateUtxoValidation inputBasketState outputBasketState <-
    pmatch $
      ensureOnlyBasketStateUtxoConsumed
        # basketParam.basketStateCS
        # pbasketStateTN
        # ctx.purpose
        # txInfo.inputs
        # txInfo.outputs
        #$ pfield @"datums"
        # ctx.txInfo

  let prevExRate = pfield @"exRate" # inputBasketState
      currentExRate = pfield @"exRate" # outputBasketState

  perrorIfFalse
    "Ex rate is not monotonically growing"
    (pfromData prevExRate #< pfromData currentExRate)

  perrorIfFalse
    "Minting tokens"
    (mint #== mempty)

  let adminPkh = pfromData $ pfield @"adminPkh" # pto inputBasketState

  perrorIfFalse
    "Not signed by an administrator"
    (psignedBy # pto adminPkh # signatories)

  pvisit <- plet $ pvisitInputSPUtxo # basketParam.stakingPoolCS

  let paccumSPUtxos = plam $ \(input :: Term s PTxInInfo) acc -> P.do
        pmatch (pvisit #$ pfield @"resolved" # input) $ \case
          PNothing -> acc
          PJust actual -> P.do
            PPair coin basketTokens <- pmatch actual
            MkPFoldResult accumSPUtxos accumCoin accumBasketTokens <- pmatch acc
            pcon $
              MkPFoldResult
                (accumSPUtxos + 1)
                (accumCoin + coin)
                (accumBasketTokens + basketTokens)

  {- Here we fold over all stake pool utxo and compute:
      - number of stake pool utxos
      - total amount basket token that exist at the moment
      - total amount of Lovelace deposited to a basket at the moment
  -}
  MkPFoldResult spUtxos totalDepositedLovelace totalBasketTokens <-
    pmatch $
      pfoldr
        # paccumSPUtxos
        # pcon (MkPFoldResult 0 0 0)
        # txInfo.referenceInputs

  perrorIfFalse
    "Not all stake pool utxos provided as reference inputs"
    (spUtxos #== pfield @"numOfStakePoolUTxOs" # inputBasketState)

  expectedExRate <-
    plet $
      pcon $
        MkPExRate $
          (pfromInteger # pto totalDepositedLovelace) #/ (pfromInteger # pto totalBasketTokens)

  perrorIfFalse
    "Unexpected exchange rate"
    (expectedExRate #== (pfield @"exRate" # outputBasketState))

  let expectedOutputBasketState = pupdateExRate # inputBasketState # expectedExRate

  perrorIfFalse
    "BasketState has been incorrectly updated"
    (expectedOutputBasketState #== outputBasketState)

  popaque $ pconstant ()

data PFoldResult (s :: S)
  = MkPFoldResult (Term s PInteger) (Term s PCoin) (Term s PBasketToken)
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PFoldResult where type DPTStrat _ = PlutusTypeScott
