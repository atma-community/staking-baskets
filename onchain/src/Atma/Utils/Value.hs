module Atma.Utils.Value (phasCS, ptxOutHasCS, ptxInHasCS) where

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (
  AmountGuarantees,
  KeyGuarantees,
  PCurrencySymbol,
  PTxOut,
  PValue,
 )
import Plutarch.Api.V2.Tx (PTxInInfo)
import Plutarch.Extra.Maybe (pisJust)

ptxInHasCS :: ClosedTerm (PCurrencySymbol :--> PTxInInfo :--> PBool)
ptxInHasCS = phoistAcyclic $ plam $ \cs input ->
  ptxOutHasCS # cs #$ pfield @"resolved" # input

ptxOutHasCS ::
  ClosedTerm (PCurrencySymbol :--> PTxOut :--> PBool)
ptxOutHasCS = phoistAcyclic $ plam $ \cs txOut ->
  let inputValue = pfromData $ pfield @"value" # txOut
   in phasCS # cs # inputValue

phasCS ::
  forall {w1 :: KeyGuarantees} {w2 :: AmountGuarantees}.
  ClosedTerm
    ( PCurrencySymbol
        :--> PValue w1 w2
        :--> PBool
    )
phasCS = phoistAcyclic $
  plam $
    \cs value -> pisJust #$ AssocMap.plookup # cs # pto value
