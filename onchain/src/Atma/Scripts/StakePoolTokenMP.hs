module Atma.Scripts.StakePoolTokenMP (
  pMkStakePoolTokenMP,
  pMkStakePoolTokenMPUntyped,
  stakePoolTokenMPScript,
  pvalidateStakePoolMPTriggerWitness,
) where

import Atma.PTypes (PBasketParams, PBasketState, PBasketStateCS, PStakingPoolCS (MkPStakingPoolCS))
import Atma.Scripts.BasketValidator.Rebalance (paddPair, passertIsBalanced, pvisitOutputSPUtxo)
import Atma.Scripts.Common (
  findBasketStateInput,
  findBasketStateOutput,
  pstakePoolTN,
  stakePoolValue,
 )
import Atma.Utils (perrorIfFalse, pextractBasketState, pownCurrencySymbol, ptryFromUndata, pupdateNumOfStakePoolUTxOs)
import Atma.Utils.ExRate (PExRate, plovelaceToBasketTokens)
import Atma.Utils.Value (phasCS, ptxOutHasCS)
import Data.Default (def)
import Flat.Types (Text)
import Plutarch (
  Config (tracingMode),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V1 (PCredential)
import Plutarch.Api.V1.Value (
  pvalueOf,
 )
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (KeyGuarantees (Unsorted), PAddress, PDatum, PDatumHash, PMap, PMintingPolicy, PTxOut)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Api.V2.Tx (PTxInInfo)
import Plutarch.Extra.Maybe (pfromMaybe, ptraceIfNothing)
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#+))

pvalidateStakePoolMPTriggerWitness ::
  ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
pvalidateStakePoolMPTriggerWitness = plam $ \basketParams sc -> P.do
  let mint = pfield @"mint" #$ pfield @"txInfo" # sc
      stakePoolCS = pfromData $ pfield @"stakingPoolCS" # basketParams

  perrorIfFalse
    "stake pool minting policy won't trigger"
    (phasCS # pto stakePoolCS # mint)

  popaque $ pconstant ()

{- | `pvalidateBasketStateUtxoHandling` performes Basket State utxo checks

     On successful validation the function returns the inferred address
     of utxos that belong to a basket.

     The same checks are performed on minting on burning:
     - utxo A carrying basket state token is consumed
     - utxo B carrying basket state token is produced
     - utxo B is at the same address as utxo A
     - Basket state datum changes in the way that
       - `numOfStakePoolUTxOs` changes by the amount given as a parameter to this function
       - other fields do not change
-}
pvalidateBasketStateUtxoHandling ::
  Term s PBasketStateCS ->
  Term s (PMap 'Unsorted PDatumHash PDatum) ->
  Term s PInteger ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s (PPair PAddress PBasketState)
pvalidateBasketStateUtxoHandling
  basketStateCS
  datumMap
  amountOfStakePools
  inputs
  outputs = P.do
    inputBasketStateUtxo <-
      plet $
        ptraceIfNothing "Must consume BasketState UTxO" $
          findBasketStateInput # basketStateCS # inputs

    outputBasketStateUtxo <-
      plet $
        ptraceIfNothing "Must propagate BasketState UTxO" $
          findBasketStateOutput # basketStateCS # outputs

    inputBasketState <-
      plet $
        pextractBasketState
          # datumMap
          #$ pfield @"resolved"
          # inputBasketStateUtxo

    let outputBasketState =
          pextractBasketState # datumMap # outputBasketStateUtxo

    -- Derive BasketValidator address from referenced BasketState UTxO
    inferredAddress <-
      plet $
        pfield @"address"
          #$ pfield @"resolved"
          # inputBasketStateUtxo

    perrorIfFalse "Basket State token at wrong address" $
      inferredAddress #== (pfield @"address" # outputBasketStateUtxo)

    let expectedNumofStakePoolUtxos =
          (pfield @"numOfStakePoolUTxOs" # inputBasketState) #+ amountOfStakePools

    let expectedOutputBasketState =
          pupdateNumOfStakePoolUTxOs
            # inputBasketState
            # expectedNumofStakePoolUtxos

    perrorIfFalse
      "BasketState has been incorrectly updated"
      (expectedOutputBasketState #== outputBasketState)

    pcon $ PPair inferredAddress outputBasketState

{- | `pMkStakePoolTokenMP` is a stake pool token minting policy

     Following common checks are performed:
     - Transaction is signed by the administrator
     - Only stake pool tokens are minted or burnt
     - checks performed by `pvalidateBasketStateUtxoHandling` function
       - in particular it is checks that `numOfStakePoolUTxOs` from Basket State datum
         increases by the correct amount when stake pool utxos are created and
         decreases by the correct amount when stake pool utxos are destroyed

     When minting:
     - Stake Pool Tokens are minted (>0)
     - No stake pool utxo is consumed
     - Valid stake pool utxos are produced:
       - utxo is at inferred basket address
       - carry exactly one stake pool token
       - `basketTokenCounter` is initialized with a number corresponding to
         ADA that is locked in the newly created Stake Pool Utxo

     When burning:
     - Stake Pool Tokens are burnt (<0)
     - The transaction is balanced in a sense of
       balancing `basketTokenCounter` field and balancing ADA
       (see rebalancing codition when handling `MkPRebalance` redeemer)
-}
pMkStakePoolTokenMP ::
  ClosedTerm (PBasketStateCS :--> PMintingPolicy)
pMkStakePoolTokenMP = plam $ \basketStateCS _ ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txInfo <-
    pletFields
      @'["signatories", "inputs", "outputs", "mint"]
      ctx.txInfo
  stakePoolCS <-
    plet . pcon . MkPStakingPoolCS $
      pownCurrencySymbol # pfromData ctx.purpose

  let toMint = Value.pnormalize # txInfo.mint
      amountOfStakePools =
        pvalueOf # toMint # pto stakePoolCS # pstakePoolTN

  perrorIfFalse "Must not mint or burn anything but StakePool tokens" $
    let mintedStakePoolTokens =
          stakePoolValue # stakePoolCS # amountOfStakePools
     in mintedStakePoolTokens #== toMint

  PPair basketAddress outputBasketState <-
    pmatch $
      pvalidateBasketStateUtxoHandling
        basketStateCS
        (pfield @"datums" # ctx.txInfo)
        amountOfStakePools
        txInfo.inputs
        txInfo.outputs

  bs <- pletFields @'["adminPkh", "exRate"] outputBasketState

  perrorIfFalse "Not signed by admin" $
    pelem # pdata (pto $ pfromData $ bs.adminPkh) # pfromData txInfo.signatories

  ownPaymentCredential <- plet $ pfield @"credential" # basketAddress

  pif
    (0 #< amountOfStakePools)
    ( validateMinting
        ownPaymentCredential
        stakePoolCS
        (pfromData txInfo.inputs)
        (pfromData txInfo.outputs)
        bs.exRate
    )
    ( validateBurning
        ownPaymentCredential
        stakePoolCS
        (pfromData txInfo.inputs)
        (pfromData txInfo.outputs)
    )

validateBurning ::
  Term s PCredential ->
  Term s PStakingPoolCS ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s POpaque
validateBurning = passertIsBalanced

validateMinting ::
  Term s PCredential ->
  Term s PStakingPoolCS ->
  Term s (PBuiltinList PTxInInfo) ->
  Term s (PBuiltinList PTxOut) ->
  Term s PExRate ->
  Term s POpaque
validateMinting
  ownPaymentCredential
  stakePoolCS
  inputs
  outputs
  exRate = P.do
    perrorIfFalse "Must not consume StakePoolUTxO" $
      let isStakePoolUtxo input =
            -- naming assumes protocol is not vulnerable
            ptxOutHasCS # pto stakePoolCS #$ pfield @"resolved" # input
       in pnot #$ pany # plam isStakePoolUtxo # inputs

    {- Together, checking that
      - StakePool tokens are not consumed as inputs
      - output StakePool UTxOs are
      guarantee that Tx produce as much SP UTxOs as tokens has been minted.
    -}

    let paccumOutputs = plam $ \a b ->
          let r =
                pfromMaybe
                  # pcon (PPair 0 0)
                  #$ pvisitOutputSPUtxo
                  # ownPaymentCredential
                  # stakePoolCS
                  # a
           in paddPair # r # b

    PPair lovelace basketTokens <-
      pmatch $ pfoldr # paccumOutputs # pcon (PPair 0 0) # outputs

    let expectedBasketTokens =
          plovelaceToBasketTokens
            # exRate
            # lovelace

    perrorIfFalse
      "Must not consume StakePoolUTxO"
      (expectedBasketTokens #== basketTokens)

    popaque $ pconstant ()

pMkStakePoolTokenMPUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkStakePoolTokenMPUntyped = plam $ \cs ->
  pMkStakePoolTokenMP
    # unTermCont (ptryFromUndata cs)

stakePoolTokenMPScript :: Either Text Script
stakePoolTokenMPScript =
  compile (def {tracingMode = DoTracing}) pMkStakePoolTokenMPUntyped
