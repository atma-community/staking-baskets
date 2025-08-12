module Atma.Scripts.BasketValidator.SetAdmin (psetAdmin) where

import Atma.PTypes (PBasketParams)
import Plutarch.Api.V2 (PScriptContext)

import Atma.Scripts.Common (pbasketStateTN)
import Atma.Utils (
  PBasketStateUtxoValidation (MkPBasketStateUtxoValidation),
  ensureOnlyBasketStateUtxoConsumed,
  perrorIfFalse,
  pupdateAdminPubKeyHash,
 )
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Extra.Ord (psort)
import Plutarch.Monadic qualified as P

{-

The function returns number of elements
of the first list that are also present in the second list

The assumptions is that both argument lists:
- are sorted
- do not contain duplicates

Thus the function computes the number of elements
of an intersection of lists viewed as sets.

Functionally it's equivalent to haskell function:

```
countIntersection' :: forall a. Ord a => [a] -> [a] -> Int
countIntersection' = fix fun
  where
    fun _ [] _ = 0
    fun _ _ [] = 0
    fun rec xss@(x:xs) yss@(y:ys) =
      case x `compare` y of
        EQ -> 1 + rec xs ys
        LT -> rec xs yss
        GT -> rec xss ys
```

-}
countIntersection ::
  (PEq a, POrd a, PListLike list, PElemConstraint list a) =>
  ClosedTerm
    ( list a
        :--> list a
        :--> PInteger
    )
countIntersection =
  pfix #$ plam $ \rec xss yss ->
    elim 0 xss $ \x xs ->
      elim 0 yss $ \y ys ->
        pif
          (x #== y)
          (1 + rec # xs # ys)
          $ pif
            (x #< y)
            (rec # xs # yss)
            (rec # xss # ys)
  where
    elim onEmpty list f = pelimList f onEmpty list

{- |  `psetAdmin` is used to validate `MkPSetAdmin` redeemer

      Following checks are performed:
      - Transaction is signed by majority of eligible pub key hashes,
          i.e. named in basket parameters 'voters' list
      - No tokens are minted nor burnt
      - Only one utxo from basket address is consumed
      - And that utxo carries basket state token
      - Basket state utxo is propagated
      - Basket state datum is updated correctly:
        - only 'adminPkh' field can change

      WARNING!

      `voters` list from basket parameters must be sorted
      and should not contain any duplicates. If `voters`
      are not sorted or `voters` contain duplicates then
      the validator may fail even if required number of
      signatures is present.
-}
psetAdmin ::
  ClosedTerm (PBasketParams :--> PScriptContext :--> POpaque)
psetAdmin = plam $ \basketParams' ctx' -> P.do
  ctx <- pletFields @'["purpose", "txInfo"] ctx'
  txInfo <-
    pletFields
      @'[ "inputs"
        , "mint"
        , "outputs"
        , "signatories"
        ]
      ctx.txInfo

  basketParam <-
    pletFields
      @'[ "voters"
        , "basketStateCS"
        ]
      basketParams'
  let mint = pnormalize # txInfo.mint
      signatories = pmap # plam pfromData #$ pfromData $ txInfo.signatories
      -- one does not rely on the ordering of `signatories` in script context
      sortedSignatories = psort # signatories
      -- voters are assumed to be sorted in basket parameters
      voters = pmap # plam pfromData #$ pfromData $ basketParam.voters

  perrorIfFalse
    "Minting tokens"
    (mint #== mempty)

  let numOfVoters = plength # voters
      reqMajority = 1 + pquot # numOfVoters # 2
      numSigned = countIntersection # sortedSignatories # voters

  perrorIfFalse
    "Majority of voters have not signed the transaction"
    (reqMajority #<= numSigned)

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

  let expectedOutputBasketState =
        pupdateAdminPubKeyHash
          # inputBasketState
          #$ pfield @"adminPkh"
          # outputBasketState

  perrorIfFalse
    "BasketState has been incorrectly updated"
    (expectedOutputBasketState #== outputBasketState)

  popaque $ pconstant ()
