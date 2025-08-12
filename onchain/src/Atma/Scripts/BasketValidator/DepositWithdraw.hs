{-# LANGUAGE BlockArguments #-}

module Atma.Scripts.BasketValidator.DepositWithdraw (
  pvalidateWith,
  depositMintingAssert,
  depositExchangeAssert,
  withdrawMintingAssert,
  withdrawExchangeAssert,
  depositPledgeLockAssert,
  withdrawPledgeLockAssert,
) where

import Plutarch.Api.V1.Value (
  padaSymbol,
  padaToken,
  plovelaceValueOf,
  pnormalize,
  pvalueOf,
 )
import Plutarch.Api.V1.Value qualified as V

import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Monadic qualified as P
import Plutarch.Num ((#-))

import Atma.PTypes (
  PBasketLock (MkPLocked, MkPUnlocked),
  PBasketParams,
  PBasketState,
 )
import Atma.Scripts.Common (
  pbasketStateTN,
  pisBasketLocked,
  pstakePoolTN,
 )
import Atma.Utils (
  findInputAtPaymentCredential,
  findOutputAtPaymentCredential,
  perrorIfFalse,
  pextractBasketState,
  pextractStakePoolUtxoDatum,
  pgetOwnTxOut,
  ptokenInUtxo,
  pupdateBasketTokenCounter,
 )
import Atma.Utils.ExRate (
  PBasketToken (MkPBasketToken),
  PCoin (MkPCoin),
  pbasketTokensToLovelace,
  plovelaceToBasketTokens,
 )

type MintingAssert = ClosedTerm (PBasketToken :--> PBool)
type ExchangeAssert =
  ClosedTerm
    ( PBasketState
        :--> PBasketToken
        :--> PCoin
        :--> PBool
    )
type PledgeLockAssert = ClosedTerm (PBasketLock :--> PBool)

depositPledgeLockAssert :: PledgeLockAssert
depositPledgeLockAssert = plam $ const $ pcon PTrue

withdrawPledgeLockAssert :: PledgeLockAssert
withdrawPledgeLockAssert = plam $ \pledgeLock ->
  pmatch pledgeLock $ \case
    MkPLocked _ -> pcon PFalse
    MkPUnlocked _ -> pcon PTrue

depositMintingAssert :: MintingAssert
depositMintingAssert = plam $ \amt -> 0 #< amt

depositExchangeAssert :: ExchangeAssert
depositExchangeAssert = plam $ \basketState' actualBasketTokens lovelace -> P.do
  let maxExpectedBasketTokens =
        plovelaceToBasketTokens
          # (pfield @"exRate" # basketState')
          # lovelace
  actualBasketTokens #== maxExpectedBasketTokens

withdrawMintingAssert :: MintingAssert
withdrawMintingAssert = plam $ \amt -> amt #< 0

withdrawExchangeAssert :: ExchangeAssert
withdrawExchangeAssert =
  plam $
    \basketState'
     basketTokens
     actualCoin ->
        let exRate =
              pfield @"exRate" # basketState'

            maxExpectedCoin =
              pbasketTokensToLovelace # exRate # basketTokens
         in maxExpectedCoin #== actualCoin

{- | 'pvalidateWith' is used to validate user deposit and withdrawals.
     These correspond to `MkPDeposit` and `MkPWithdraw` redeemers.

     Desposit and withdrawal validation differs in `MintingAssert` and
     `ExchangeAssert` provided to the function.

     Following common checks are performed:
     -
     - Only basket tokens are minted or burnt
     - Only one utxo from basket address is consumed
     - And that utxo carries stake pool token
     - Only one utxo from basket address is provided as reference input
     - And that utxos is basket state utxo
     - Stake pool utxo is propagated
     - And its address is preserved
     - And its datum is preserved
     - The amount of ada at propagated stake pool utxo
       satisfies minimum ada requirement
     - `basketTokenCounter` gets updated by a correct amount -
       when basket tokens are minted the counter gets incremented;
       when basket tokens are burnt the counter gets decremented;

    For deposit:
     - basket tokens are minted
     - correct amount of basket tokens is minted as per
       deposited ada and the `depositExRate`

    For withdrawal:
     - basket tokens are burnt
     - correct amount of ada is released from stake pool utxo as per
       burnt basket tokens and the `withdrawExRate`
-}
pvalidateWith ::
  MintingAssert ->
  ExchangeAssert ->
  PledgeLockAssert ->
  ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
pvalidateWith mintingAssert exchangeAssert pledgeAssert =
  plam $ \basketParams' ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    txInfo <-
      pletFields
        @'[ "inputs"
          , "mint"
          , "referenceInputs"
          , "outputs"
          ]
        ctx.txInfo
    basketParams <-
      pletFields
        @'[ "stakingPoolCS"
          , "basketStateCS"
          , "basketTokenCS"
          , "basketTokenTN"
          ]
        basketParams'

    basketTokenCS <- plet $ pfromData basketParams.basketTokenCS
    stakingPoolCS <- plet $ pfromData basketParams.stakingPoolCS
    basketStateCS <- plet $ pfromData basketParams.basketStateCS
    basketTokenTN <- plet $ pfromData basketParams.basketTokenTN

    ownResolved <- plet $ pgetOwnTxOut # ctx.purpose # txInfo.inputs
    ownAddress <- plet $ pfromData $ pfield @"address" # ownResolved
    ownCredential <- plet $ pfield @"credential" # ownAddress

    mint <- plet $ pnormalize # txInfo.mint

    basketTokensMinted <-
      plet $
        pcon $
          MkPBasketToken $
            pvalueOf # mint # pto basketTokenCS # pto basketTokenTN

    perrorIfFalse
      "Minting assertion failed"
      (mintingAssert # basketTokensMinted)

    perrorIfFalse "Minting other tokens" $
      let expectToMint =
            V.psingleton
              # pto basketTokenCS
              # pto basketTokenTN
              # pto basketTokensMinted
       in mint #== expectToMint

    let fromBasketValidator =
          findInputAtPaymentCredential # ownCredential # txInfo.inputs

    perrorIfFalse
      "Spending more than one UTxO from basket validator address"
      ((plength # fromBasketValidator) #== 1)

    perrorIfFalse
      "No token at stake pool utxo"
      (ptokenInUtxo # pto stakingPoolCS # pstakePoolTN # ownResolved)

    referencesFromBasketValidator <-
      plet $
        findInputAtPaymentCredential
          # ownCredential
          # txInfo.referenceInputs

    -- Just for simplicity, to not perform search
    perrorIfFalse
      "More than one reference inputs from basket validator address"
      ((plength # referencesFromBasketValidator) #== 1)

    basketStateUtxoResolved <- plet $ P.do
      PCons basketStateUtxo _ <- pmatch referencesFromBasketValidator
      pfield @"resolved" # basketStateUtxo

    perrorIfFalse "No token at basket state utxo" $
      ptokenInUtxo
        # pto basketStateCS
        # pbasketStateTN
        # basketStateUtxoResolved

    toBasketValidator <-
      plet $
        findOutputAtPaymentCredential # ownCredential # txInfo.outputs

    -- Just for simplicity, to not do searching
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

    basketState <-
      plet $
        pextractBasketState # (pfield @"datums" # ctx.txInfo) # basketStateUtxoResolved

    perrorIfFalse "Basket must be unlocked for deposit/withdrawal" $
      pnot #$ pisBasketLocked # basketState

    let pledgeLock = pfromData $ pfield @"pledgeLock" # pto basketState

    perrorIfFalse "Basket must be unlocked for withdrawal due to pledge locking" $
      pledgeAssert # pledgeLock

    ownValue <- plet $ pnormalize #$ pfield @"value" # ownResolved
    outputValue <- plet $ pnormalize #$ pfield @"value" # outputUtxo

    let inputCoin = plovelaceValueOf # ownValue
    outputCoin <- plet $ plovelaceValueOf # outputValue
    diff <- plet $ pcon $ MkPCoin $ outputCoin #- inputCoin

    perrorIfFalse "Exchange assertion failed" $
      exchangeAssert
        # basketState
        # basketTokensMinted
        # diff

    let expectedOutputValue =
          ownValue <> (V.psingleton # padaSymbol # padaToken # pto diff)

    perrorIfFalse
      "Propagated stake pool utxo value can differ only by lovelace"
      (outputValue #== expectedOutputValue)

    inputDatum <- plet $ pextractStakePoolUtxoDatum # ownResolved
    let outputDatum = pextractStakePoolUtxoDatum # outputUtxo

    let expectedDatum =
          pupdateBasketTokenCounter
            # inputDatum
            #$ (pfield @"basketTokenCounter" # inputDatum)
            + pto basketTokensMinted

    perrorIfFalse
      "Basket token counter has been updated incorrectly"
      (outputDatum #== expectedDatum)

    popaque $ pconstant ()
