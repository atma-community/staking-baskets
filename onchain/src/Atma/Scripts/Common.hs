{-# LANGUAGE AllowAmbiguousTypes #-}

module Atma.Scripts.Common (
  pstakePoolTN,
  stakePoolValue,
  findBasketStateInput,
  findBasketStateOutput,
  paddressScriptHash,
  basketStateTN,
  stakePoolTN,
  pbasketStateTN,
  basketTokenValue,
  pisBasketLocked,
) where

import Atma.PTypes (PBasketLock (MkPLocked), PBasketState, PBasketStateCS, PBasketTokenCS, PBasketTokenTN, PStakingPoolCS)
import Atma.Utils.Value (ptxInHasCS, ptxOutHasCS)
import Plutarch.Api.V1 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PCredential (PScriptCredential), PTokenName)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2 (PAddress, PScriptHash, PTxOut, PValue)
import Plutarch.Api.V2.Tx (PTxInInfo)
import PlutusLedgerApi.V1.Value (tokenName)
import PlutusLedgerApi.V2 (TokenName)

pisBasketLocked :: ClosedTerm (PBasketState :--> PBool)
pisBasketLocked = phoistAcyclic $ plam $ \basketState ->
  pmatch (pfield @"lock" # basketState) $ \case
    MkPLocked _ -> pcon PTrue
    _ -> pcon PFalse

findBasketStateInput ::
  forall (list :: PType -> PType).
  PIsListLike list PTxInInfo =>
  ClosedTerm (PBasketStateCS :--> list PTxInInfo :--> PMaybe PTxInInfo)
findBasketStateInput = phoistAcyclic $ plam $ \basketStateCS inputs ->
  pfind # (ptxInHasCS # pto basketStateCS) # inputs

findBasketStateOutput ::
  forall (list :: PType -> PType).
  PIsListLike list PTxOut =>
  ClosedTerm (PBasketStateCS :--> list PTxOut :--> PMaybe PTxOut)
findBasketStateOutput = phoistAcyclic $ plam $ \basketStateCS inputs ->
  pfind # (ptxOutHasCS # pto basketStateCS) # inputs

paddressScriptHash :: ClosedTerm (PAddress :--> PMaybe PScriptHash)
paddressScriptHash = phoistAcyclic $ plam $ \addr -> P.do
  pmatch (pfromData $ pfield @"credential" # addr) $ \case
    PScriptCredential cred -> pcon $ PJust $ pfield @"_0" # cred
    _ -> pcon PNothing

stakePoolValue ::
  ClosedTerm
    ( PStakingPoolCS
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
    )
stakePoolValue = phoistAcyclic $ plam $ \stakePoolCS tokens ->
  Value.psingleton # pto stakePoolCS # pstakePoolTN # tokens

basketTokenValue ::
  ClosedTerm
    ( PBasketTokenCS
        :--> PBasketTokenTN
        :--> PInteger
        :--> PValue 'Sorted 'NonZero
    )
basketTokenValue = phoistAcyclic $ plam $ \basketTokenCS basketTokenTN amount ->
  Value.psingleton # pto basketTokenCS # pto basketTokenTN # amount

basketStateTN :: TokenName
basketStateTN = tokenName "BasketState"

stakePoolTN :: TokenName
stakePoolTN = tokenName "StakePool"

pstakePoolTN :: ClosedTerm PTokenName
pstakePoolTN = phoistAcyclic $ pconstant stakePoolTN

pbasketStateTN :: ClosedTerm PTokenName
pbasketStateTN = phoistAcyclic $ pconstant basketStateTN
