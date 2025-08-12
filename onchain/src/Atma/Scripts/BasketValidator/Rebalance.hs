module Atma.Scripts.BasketValidator.Rebalance (
  pvalidateRebalance,
  pvalidateRebalanceDelegate,
  pvisitOutputSPUtxo,
  pvisitInputSPUtxo,
  passertIsBalanced,
  paddPair,
) where

import Plutarch.Api.V1.Value (plovelaceValueOf, pnormalize)
import Plutarch.Api.V2 (
  PScriptContext,
  PScriptPurpose (PSpending),
  PTxInInfo,
  PTxInfo,
  PTxOut,
 )
import Plutarch.Monadic qualified as P

import Atma.PTypes (
  PBasketParams,
  PBasketState,
  PBasketStateCS,
  PBasketValidatorRedeemer (MkPRebalance),
  PStakingPoolCS,
 )
import Atma.Scripts.Common (findBasketStateInput, pbasketStateTN, pstakePoolTN)
import Atma.Utils (
  perrorIfFalse,
  pextractBasketState,
  pextractStakePoolUtxoDatum,
  pgetOwnTxOut,
  psignedBy,
  ptokenInUtxo,
  ptokenInUtxoAssert,
 )
import Atma.Utils.ExRate (PBasketToken (MkPBasketToken), PCoin (MkPCoin))
import Plutarch.Api.V1 (PCredential)
import Plutarch.Bool (pand')
import Plutarch.Builtin (pforgetData)
import Plutarch.Extra.Maybe (pfromMaybe, pjust, pnothing)

{- | Rebalancing is a process where funds within stake pool utxos set
     are redistributed to achieve the desired distribution.

     Two redeemers are involved in the process:
     - `MkPRebalance`
     - `MkPRebalanceDelegate`

     Is is mandatory that at least one stake pool utxo is consumed with
     `MkPRebalance` redeemer. Remaining stake pool utxos can be consumed with
     `MkPRebalanceDelegate` reedeemer.

     `MkPRebalance` causes the main rebalancing validation to be performed.
     `MkPRebalanceDelegate` is used to check that validation associateed
      with `MkPRebalance` will be triggered.

      Details are in the comments of
      `pvalidateRebalance` and `pvalidateRebalanceDelegate` functions.
-}

{- | `pvisitOutputSPUtxo` is a helper function used in `pvalidateRebalance`

      It should be used to validate Stake Pool UTxOs produced in a transaciton.

      Arguments are:
      - `PAddress` - basket address
      - `PStakingPoolCS` - currency symbol of stake pool token
      - `PTxOut` - TxOut that is handled

      Stake pool utxo is said to be well-formed when:
      - carries exactly one stake pool token
      - utxo's payment credential is at basket address

      `pvisitOutputSPUtxo` inspects TxOut argument:
      If utxo is a well-formed stake pool utxo returns
        PJust (ADA payload, basket token counter carried by datum)
      If utxo is not well-formed stake pool utxo
        ('isOnAddress xor carriesSPToken' case) it errors out
    | If utxo is not a stake pool utxo (and not at basket address) it returns PNothing

      Please note that this function is effectful.
      It calls `perror` if utxo is not well-formed stake pool utxo.
-}
pvisitOutputSPUtxo :: ClosedTerm (PCredential :--> PStakingPoolCS :--> PTxOut :--> PMaybe (PPair PCoin PBasketToken))
pvisitOutputSPUtxo =
  phoistAcyclic $ plam $ \ownPaymentCredential stakingPoolCS txOut -> P.do
    isOnAddress <-
      plet $
        (pfield @"credential" #$ pfield @"address" # txOut)
          #== ownPaymentCredential

    carriesSPToken <-
      plet $
        ptokenInUtxoAssert
          # pto stakingPoolCS
          # pstakePoolTN
          # txOut

    pif
      (pand' # isOnAddress # carriesSPToken)
      ( P.do
          let lovelace =
                plovelaceValueOf
                  #$ pfield @"value"
                  # txOut

              basketTokenCounter =
                pfield @"basketTokenCounter"
                  #$ pto
                  $ pextractStakePoolUtxoDatum
                    # txOut

          pjust
            #$ pcon
            $ PPair
              (pcon $ MkPCoin lovelace)
              (pcon $ MkPBasketToken basketTokenCounter)
      )
      ( pif
          (pand' # (pnot # isOnAddress) # carriesSPToken)
          (ptraceError "Stealing SP token")
          pnothing
      )

{- | `pvisitInputSPUtxo` is a helper function used in `pvalidateRebalance`

      It should be used to validate Stake Pool UTxOs cousumed in a transaciton.

      Arguments are:
      - `PStakingPoolCS` - currency symbol of stake pool token
      - `PTxOut` - TxOut that is handled

      This function is a weaker (and cheaper) version of `pvisitOutputSPUtxo`.
      For inputs UTxOs it's not necessary to check if Stake Pool UTxO
      is well-formed. If a UTxO carries Stake Pool Token then no doubt
      it's well-formed.
-}
pvisitInputSPUtxo :: ClosedTerm (PStakingPoolCS :--> PTxOut :--> PMaybe (PPair PCoin PBasketToken))
pvisitInputSPUtxo =
  phoistAcyclic $ plam $ \stakingPoolCS txOut -> P.do
    let carriesSPToken =
          ptokenInUtxoAssert
            # pto stakingPoolCS
            # pstakePoolTN
            # txOut

    pif
      carriesSPToken
      ( P.do
          let lovelace =
                plovelaceValueOf
                  #$ pfield @"value"
                  # txOut

              basketTokenCounter =
                pfield @"basketTokenCounter"
                  #$ pto
                  $ pextractStakePoolUtxoDatum
                    # txOut

          pjust
            #$ pcon
            $ PPair
              (pcon $ MkPCoin lovelace)
              (pcon $ MkPBasketToken basketTokenCounter)
      )
      pnothing

pgetBasketStateAsRefInput ::
  ClosedTerm (PBasketStateCS :--> PTxInfo :--> PBuiltinList PTxInInfo :--> PBasketState)
pgetBasketStateAsRefInput = plam $ \basketStateCS txInfo refereceInputs -> P.do
  let predicate =
        plam $ \txInInfo -> P.do
          let txOut = pfield @"resolved" # pto txInInfo
          ptokenInUtxo # pto basketStateCS # pbasketStateTN # txOut

  PJust txInInfo <- pmatch $ pfind # predicate # refereceInputs

  pextractBasketState
    # (pfield @"datums" # txInfo)
    #$ pfield @"resolved"
    # pto txInInfo

{- | `pvalidateRebalance` is used to validate `MkPRebalance` redeemer.

      Following checks are performed:
      -
      - Transaction is signed by the administrator
      - No tokens are minted nor burnt
      - Transaction is balanced

      Transaction is balanced iff:
        - total ADA on input stake pool utxos is equal to
          total ada on output stake pool utxos.
        - sum of `basketTokenCounter`s from input stake pool utxos
          is equal to sum of `basketTokenCounter`s from output
          stake pool utxos.

      Since minting or burning is not allowed the number of input and output
      stake pool utxos must be the same in rebalancing transaction.

      Total ADA on input utxos is computed by folding over `txInfo.inputs`
      (e.g. each and every utxo that is consumed by the transaction)
      with a function that accumulates ADA found on stake pool utxos.
      More specifically `pVisitUtxo` function is used to extract ADA from
      stake pool utxos. The aforementioned fold is effectful in a sense that
      it errors out if any 'anomaly' is detected. See `pVisitUtxo` for more
      information.

      The analogue fold is applied to `txInfo.outputs`
      (e.g. each and every utxo produced in the transaction).

      It's worth mentioning that these folds have twofold role:
      - computing total ADA on stake pool utxos
      - validating that stake pool utxos are well formed
-}
pvalidateRebalance ::
  ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
pvalidateRebalance = plam $ \basketParams' ctx' -> P.do
  ctx <- pletFields @'["purpose", "txInfo"] ctx'
  basketParams <- pletFields @'["stakingPoolCS", "basketStateCS"] basketParams'
  txInfo <-
    pletFields
      @'[ "inputs"
        , "referenceInputs"
        , "mint"
        , "outputs"
        , "signatories"
        ]
      ctx.txInfo

  let signatories = txInfo.signatories
      basketState = pgetBasketStateAsRefInput # basketParams.basketStateCS # ctx.txInfo # txInfo.referenceInputs
      adminPkh = pfromData $ pfield @"adminPkh" # pto basketState

  perrorIfFalse
    "Not signed by an administrator"
    (psignedBy # pto adminPkh # signatories)

  let mint = pnormalize # txInfo.mint

  perrorIfFalse
    "Should not mint or burn"
    (mint #== mempty)

  ownResolved <- plet $ pgetOwnTxOut # ctx.purpose # txInfo.inputs
  ownPaymentCredential <- plet $ pfield @"credential" #$ pfield @"address" # ownResolved

  let maybeBasketStateUtxo = findBasketStateInput # basketParams.basketStateCS # txInfo.inputs

  perrorIfFalse
    "Should not consume Basket State UTxO"
    (maybeBasketStateUtxo #== pnothing)

  passertIsBalanced
    ownPaymentCredential
    basketParams.stakingPoolCS
    txInfo.inputs
    txInfo.outputs

passertIsBalanced ::
  Term s PCredential ->
  Term s PStakingPoolCS ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s POpaque
passertIsBalanced ownPaymentCredential stakingPoolCS inputs outputs = P.do
  let paccumOutputs = plam $ \a b ->
        let r =
              pfromMaybe
                # pcon (PPair 0 0)
                #$ pvisitOutputSPUtxo
                # ownPaymentCredential
                # stakingPoolCS
                # a
         in paddPair # r # b

  let paccumInputs = plam $ \(a :: Term s PTxInInfo) b ->
        let r =
              pfromMaybe
                # pcon (PPair 0 0)
                #$ pvisitInputSPUtxo
                # stakingPoolCS
                #$ pfield @"resolved"
                # a
         in paddPair # r # b

  init <- plet $ pcon $ PPair 0 0

  lovelaceAndCounterOutput <-
    plet $ pfoldr # paccumOutputs # init # outputs
  lovelaceAndCounterInput <-
    plet $ pfoldr # paccumInputs # init # inputs

  perrorIfFalse
    "(Total basket ADA) and (total basket tokens) must be preserved"
    (lovelaceAndCounterInput #== lovelaceAndCounterOutput)

  popaque $ pconstant ()

prebalance :: ClosedTerm PBasketValidatorRedeemer
prebalance = pcon $ MkPRebalance pdnil

{- | `pvalidateRebalanceDelegate` is used
     to validate `MkPRebalanceDelegate` redeemer.

     The operation:
     - infer basket validator address
       by checking address associated with utxo being validated
     - check that in the transaction there exists a utxo that is
       at the inferred basket validator address and that is
       consumed with `MkPRebalance` redeemer.

     The latter is achieved by inspecting `txInfo.redeemers` map
     and is to ensure that the main rebalancing validator will
     be triggered.
-}
pvalidateRebalanceDelegate ::
  ClosedTerm (PScriptContext :--> POpaque)
pvalidateRebalanceDelegate = plam $ \ctx' -> P.do
  ctx <- pletFields @'["purpose", "txInfo"] ctx'
  txInfo <-
    pletFields
      @'[ "inputs"
        , "redeemers"
        ]
      ctx.txInfo

  extractCredential <- plet $ plam $ \txOut ->
    pfield @"credential"
      #$ pfield @"address"
      # txOut

  ownCredential <-
    plet $
      extractCredential
        #$ pgetOwnTxOut
        # ctx.purpose
        # txInfo.inputs

  redeemers <- plet $ pto $ pfromData $ txInfo.redeemers

  doesDelegate <- plet $ plam $ \(txInInfo :: Term s PTxInInfo) -> P.do
    let matchesAddress =
          ownCredential
            #== (extractCredential #$ pfield @"resolved" # txInInfo)

    pif
      matchesAddress
      ( P.do
          txOutRef <- plet $ pfromData $ pfield @"outRef" # txInInfo

          let hasOutRef = plam $ \pair -> P.do
                pmatch (pfromData $ pfstBuiltin # pair) $ \case
                  PSpending ref -> txOutRef #== pfield @"_0" # ref
                  _ -> pcon PFalse

          PJust pair <- pmatch $ pfind # hasOutRef # redeemers

          let redeemer = pfromData $ psndBuiltin # pair

          pto redeemer #== pforgetData (pdata prebalance)
      )
      (pcon PFalse)

  PJust _ <- pmatch $ pfind # doesDelegate # pfromData txInfo.inputs

  popaque $ pconstant ()

paddPair ::
  Term
    s
    ( PPair PCoin PBasketToken
        :--> PPair PCoin PBasketToken
        :--> PPair PCoin PBasketToken
    )
paddPair = phoistAcyclic $ plam $ \l r -> P.do
  PPair lx ly <- pmatch l
  PPair rx ry <- pmatch r
  pcon $ PPair (lx + rx) (ly + ry)
