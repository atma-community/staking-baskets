{-
Copyright 2022 Shueki LLC

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

{-
    This module implements a one-shot policy for minting NFTs.
    The policy is parametrized by a UTXO and a token name
-}

module Atma.Scripts.OneShotMintingPolicy (
  pMkOneShotMintingPolicy,
  pMkOneShotMintingPolicyUntyped,
  oneShotMintingPolicyScript,
  compileOneShotMintingPolicy,
) where

import Data.Default (def)
import Flat.Types (Text)

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTokenName,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Positive (PPositive, ptryPositive)
import PlutusLedgerApi.V2 (
  ScriptContext,
  TokenName,
  TxOutRef,
 )

import Atma.Utils (
  pconsumesRef,
  perrorIfFalse,
  phasTokens,
  pownCurrencySymbol,
  ptryFromUndata,
 )
import Plutarch (
  Config (tracingMode),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Builtin (pforgetData)

{-
    Minting policy for OneShot tokens.

    Ensures a given `TxOutRef` is consumed to enforce uniqueness of the token.
    Only a single token can be minted at a time.
-}
pMkOneShotMintingPolicy ::
  ClosedTerm
    ( PTokenName
        :--> PPositive
        :--> PTxOutRef
        :--> PMintingPolicy
    )
pMkOneShotMintingPolicy = phoistAcyclic $
  plam $ \tn amount txOutRef _ ctx' -> P.do
    ctx <- pletFields @'["txInfo", "purpose"] ctx'
    cs <- plet $ pownCurrencySymbol # ctx.purpose
    txInfo <- pletFields @'["inputs", "mint"] $ ctx.txInfo

    perrorIfFalse "Doesn't consume utxo" $
      pconsumesRef # txOutRef # pfromData txInfo.inputs

    perrorIfFalse "Incorrect minted value" $
      phasTokens # cs # tn # pto amount #$ pnormalize # txInfo.mint

    popaque $ pconstant ()

-- | Untyped version of `mkOneShotMintingPolicy` for CTL
pMkOneShotMintingPolicyUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkOneShotMintingPolicyUntyped = plam $ \tn amount oref ->
  pMkOneShotMintingPolicy
    # unTermCont (ptryFromUndata tn)
    # unTermCont (ptryFromUndata amount)
    # unTermCont (ptryFromUndata oref)

oneShotMintingPolicyScript :: Either Text Script
oneShotMintingPolicyScript =
  compile (def {tracingMode = DoTracing}) pMkOneShotMintingPolicyUntyped

compileOneShotMintingPolicy ::
  TokenName ->
  Integer ->
  TxOutRef ->
  ScriptContext ->
  Either Text Script
compileOneShotMintingPolicy tokenName amount inputUtxoRef ctx =
  compile (def {tracingMode = DoTracing}) $
    pMkOneShotMintingPolicy
      # pconstant tokenName
      # (ptryPositive # pconstant amount)
      # pconstant inputUtxoRef
      # pforgetData (pconstantData ())
      # pconstant ctx
