module Atma.Scripts.BasketTokenMP (
  pMkBasketTokenMP,
  pMkBasketTokenMPUntyped,
  basketTokenMPScript,
) where

import Data.Default (def)
import Flat.Types (Text)

import Plutarch (
  Config (tracingMode),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V2 (PMintingPolicy)

import Atma.PTypes (PStakingPoolCS)
import Atma.Scripts.Common (pstakePoolTN)
import Atma.Utils (pconsumesToken, ptryFromUndata)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Monadic qualified as P

{- | `pMkBasketTokenMP` is a basket token minting policy

     Basket token minting or burning can happen only
     when depositing or withdrawing, e.g. consuming stake pool utxo
     with `MkPDeposit` or `MkPWithdraw` redeemer.

     This minting policy delegates to the main basket validator
     by ensuring that in the transaction a stake pool utxo
     is consumed. It is achived by checking that such a utxo
     carries token with `PStakingPoolCS` currency symbol.
-}
pMkBasketTokenMP :: ClosedTerm (PStakingPoolCS :--> PMintingPolicy)
pMkBasketTokenMP = plam $ \stakingPoolCS _ ctx -> P.do
  let inputs = pfield @"inputs" #$ pfield @"txInfo" # ctx

  consumesStakingPoolUtxo <-
    plet $
      pconsumesToken
        # pto stakingPoolCS
        # pstakePoolTN
        # pfromData inputs

  pif
    consumesStakingPoolUtxo
    (popaque $ pconstant ())
    (ptraceError "Staking pool utxo not consumed")

pMkBasketTokenMPUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkBasketTokenMPUntyped = plam $ \cs ->
  pMkBasketTokenMP
    # unTermCont (ptryFromUndata cs)

basketTokenMPScript :: Either Text Script
basketTokenMPScript =
  compile (def {tracingMode = DoTracing}) pMkBasketTokenMPUntyped
