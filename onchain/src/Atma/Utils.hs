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

module Atma.Utils (
  pconsumesRef,
  perrorIfFalse,
  phasTokens,
  pownCurrencySymbol,
  ptryFromUndata,
  pconsumesToken,
  pparseDatum,
  getDataByHash,
  pfindOwnInput,
  findInputAtPaymentCredential,
  findOutputAtPaymentCredential,
  ptokenInUtxo,
  padaValue,
  equalPaymentCredentials,
  psignedBy,
  pgetOwnTxOut,
  pextractBasketState,
  pextractStakePoolUtxoDatum,
  datumDoesNotChange,
  ptokenInUtxoAssert,
  pintervalDuration,
  pupdateBasketLock,
  pupdatePledgeLock,
  pupdateExRate,
  pupdateNumOfStakePoolUTxOs,
  pupdateAdminPubKeyHash,
  pupdateBasketTokenCounter,
  ensureOnlyBasketStateUtxoConsumed,
  PBasketStateUtxoValidation (MkPBasketStateUtxoValidation),
) where

import Atma.PTypes (PAdminPubKeyHash, PBasketLock, PBasketState, PBasketStateCS, PDiffMilliSeconds (MkPDiffMilliSeconds), PStakePoolUtxoDatum, pbasketState, pstakePoolUtxoDatum)
import Atma.Utils.ExRate (PExRate)
import GHC.Generics qualified as GHC
import Plutarch.Api.V1 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PCredential, PInterval (PInterval), PPOSIXTimeRange, PPubKeyHash)
import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.AssocMap (PMap)
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Scripts (PDatumHash)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V1.Value qualified as V1
import Plutarch.Api.V2 (KeyGuarantees (Unsorted), PCurrencySymbol, PDatum (PDatum), PExtended (PFinite), PMap (PMap), POutputDatum (POutputDatum), PScriptPurpose (PMinting, PSpending), PTokenName, PTxInInfo, PTxOutRef, PValue)
import Plutarch.Api.V2.Tx (POutputDatum (POutputDatumHash), PTxOut)
import Plutarch.Monadic qualified as P

pupdateNumOfStakePoolUTxOs :: ClosedTerm (PBasketState :--> PInteger :--> PBasketState)
pupdateNumOfStakePoolUTxOs = phoistAcyclic $ plam $ \state numOfStakePoolUTxOs' ->
  pbasketState
    # pfromData (pfield @"exRate" # state)
    # pfromData (pdata numOfStakePoolUTxOs')
    # pfromData (pfield @"lock" # state)
    # pfromData (pfield @"pledgeLock" # state)
    # pfromData (pfield @"adminPkh" # state)

pupdateExRate :: ClosedTerm (PBasketState :--> PExRate :--> PBasketState)
pupdateExRate = phoistAcyclic $ plam $ \state exRate' ->
  pbasketState
    # pfromData (pdata exRate')
    # pfromData (pfield @"numOfStakePoolUTxOs" # state)
    # pfromData (pfield @"lock" # state)
    # pfromData (pfield @"pledgeLock" # state)
    # pfromData (pfield @"adminPkh" # state)

pupdateBasketLock :: ClosedTerm (PBasketState :--> PBasketLock :--> PBasketState)
pupdateBasketLock = phoistAcyclic $ plam $ \state lock' ->
  pbasketState
    # pfromData (pfield @"exRate" # state)
    # pfromData (pfield @"numOfStakePoolUTxOs" # state)
    # pfromData (pdata lock')
    # pfromData (pfield @"pledgeLock" # state)
    # pfromData (pfield @"adminPkh" # state)

pupdatePledgeLock :: ClosedTerm (PBasketState :--> PBasketLock :--> PBasketState)
pupdatePledgeLock = phoistAcyclic $ plam $ \state lock' ->
  pbasketState
    # pfromData (pfield @"exRate" # state)
    # pfromData (pfield @"numOfStakePoolUTxOs" # state)
    # pfromData (pfield @"lock" # state)
    # pfromData (pdata lock')
    # pfromData (pfield @"adminPkh" # state)

pupdateAdminPubKeyHash :: ClosedTerm (PBasketState :--> PAdminPubKeyHash :--> PBasketState)
pupdateAdminPubKeyHash = phoistAcyclic $ plam $ \state adminPkh' ->
  pbasketState
    # pfromData (pfield @"exRate" # state)
    # pfromData (pfield @"numOfStakePoolUTxOs" # state)
    # pfromData (pfield @"lock" # state)
    # pfromData (pfield @"pledgeLock" # state)
    # pfromData (pdata adminPkh')

pupdateBasketTokenCounter :: ClosedTerm (PStakePoolUtxoDatum :--> PInteger :--> PStakePoolUtxoDatum)
pupdateBasketTokenCounter = phoistAcyclic $ plam $ \datum basketTokenCounter' ->
  pstakePoolUtxoDatum
    # pfromData (pfield @"poolPkh" # datum)
    # pfromData (pdata basketTokenCounter')

datumDoesNotChange :: ClosedTerm (PTxOut :--> PTxOut :--> PBool)
datumDoesNotChange = phoistAcyclic $ plam $ \txOut1 txOut2 -> P.do
  pfield @"datum" # txOut1 #== pfield @"datum" # txOut2

pparseDatum ::
  forall (a :: PType).
  ( PIsData a
  , PTryFrom PData (PAsData a)
  ) =>
  ClosedTerm (PDatumHash :--> PMap 'Unsorted PDatumHash PDatum :--> PMaybe a)
pparseDatum = plam $ \dh datums' -> P.do
  PMap datums <- pmatch datums'
  pmatch (pfind # (matches # dh) # datums) $ \case
    PNothing -> pcon PNothing
    PJust pair -> P.do
      let datum = pto $ pfromData $ psndBuiltin # pair
          a = unTermCont $ ptryFromUndata @a datum
      pcon $ PJust a
  where
    matches ::
      Term
        s
        ( PDatumHash
            :--> ( PBuiltinPair (PAsData PDatumHash) (PAsData PDatum)
                    :--> PBool
                 )
        )
    matches = plam $ \dh pair -> P.do
      let dh' = pfromData $ pfstBuiltin # pair
      dh' #== dh

getDataByHash ::
  forall (a :: PType).
  ( PIsData a
  , PTryFrom PData (PAsData a)
  ) =>
  ClosedTerm
    ( PTxOut
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PMaybe a
    )
getDataByHash = phoistAcyclic $ plam $ \txOut datumMap -> P.do
  let datum' = pfield @"datum" # txOut
  POutputDatumHash datumHash <- pmatch datum'
  let datumHash' = pfromData $ pfield @"datumHash" # datumHash
  pparseDatum # datumHash' # datumMap

txInInfoMatches :: ClosedTerm (PTxOutRef :--> PTxInInfo :--> PBool)
txInInfoMatches = phoistAcyclic $
  plam $ \outref txininfo ->
    outref #== pfield @"outRef" # txininfo

pfindOwnInput :: ClosedTerm (PBuiltinList PTxInInfo :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (txInInfoMatches # outRef) # inputs

txOutHasPaymentCredential :: ClosedTerm (PCredential :--> PTxOut :--> PBool)
txOutHasPaymentCredential = phoistAcyclic $ plam $ \credential input -> P.do
  let a = pfield @"credential" #$ pfield @"address" # input
  a #== credential

findInputAtPaymentCredential ::
  ClosedTerm (PCredential :--> PBuiltinList PTxInInfo :--> PBuiltinList PTxInInfo)
findInputAtPaymentCredential = phoistAcyclic $ plam $ \credential inputs -> P.do
  let hasCredential = plam $ \input -> P.do
        let txOut = pfield @"resolved" # input
        txOutHasPaymentCredential # credential # txOut
  pfilter # hasCredential # inputs

findOutputAtPaymentCredential ::
  ClosedTerm (PCredential :--> PBuiltinList PTxOut :--> PBuiltinList PTxOut)
findOutputAtPaymentCredential = phoistAcyclic $ plam $ \credential l -> P.do
  pfilter # (txOutHasPaymentCredential # credential) # l

ptokenInUtxo ::
  ClosedTerm (PCurrencySymbol :--> PTokenName :--> PTxOut :--> PBool)
ptokenInUtxo = phoistAcyclic $
  plam $ \cs tn txOut ->
    pvalueOf # (pfield @"value" # txOut) # cs # tn #== 1

ptokenInUtxoAssert ::
  ClosedTerm (PCurrencySymbol :--> PTokenName :--> PTxOut :--> PBool)
ptokenInUtxoAssert = phoistAcyclic $
  plam $ \cs tn txOut -> P.do
    let amt = pvalueOf # (pfield @"value" # txOut) # cs # tn
    pif
      (amt #== 0)
      (pcon PFalse)
      ( pif
          (amt #== 1)
          (pcon PTrue)
          (ptraceError "# of tokens > 1")
      )

-- | Check if a particular token is being used as an input to the transaction
pconsumesToken ::
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PBuiltinList PTxInInfo
        :--> PBool
    )
pconsumesToken = phoistAcyclic $
  plam $ \cs tn -> P.do
    pany #$ plam $ \input -> P.do
      let inputValue = pfield @"value" #$ pfield @"resolved" # input
      pvalueOf # inputValue # cs # tn #== 1

-- | Error immediately with a trace message if the argument is false.
perrorIfFalse ::
  forall {s :: S} (r :: PType).
  Term s PString ->
  Term s PBool ->
  Term s r ->
  Term s r
perrorIfFalse msg b cont = pif b cont (ptraceError msg)

{- | Check if a value has exactly the given amount of the given token.  Enforces
 value contains only the specified token name of a given currency symbol, but
 other currency symbols are allowed.
-}
phasTokens ::
  forall {w1 :: KeyGuarantees} {w2 :: AmountGuarantees}.
  ClosedTerm
    ( PCurrencySymbol
        :--> PTokenName
        :--> PInteger
        :--> PValue w1 w2
        :--> PBool
    )
phasTokens = phoistAcyclic $
  plam $ \cs tn amt v -> P.do
    let expected = AssocMap.psingleton # tn # amt
    pmatch (AssocMap.plookup # cs # pto v) $ \case
      PNothing -> pconstant False
      PJust actual ->
        plistEquals
          # pto actual
          # pto expected

-- | Check if utxo is consumed
pconsumesRef ::
  ClosedTerm (PTxOutRef :--> PBuiltinList PTxInInfo :--> PBool)
pconsumesRef = phoistAcyclic $
  plam $ \txOutRef ->
    pany #$ plam $ \input -> P.do
      let txOutRef' = pfield @"outRef" # input
      pdata txOutRef #== pdata txOutRef'

-- | Gets the currency symbol of the script
pownCurrencySymbol ::
  ClosedTerm (PScriptPurpose :--> PCurrencySymbol)
pownCurrencySymbol = plam $ \purpose ->
  pmatch purpose $ \case
    PMinting cs -> pfield @"_0" # cs
    _ -> ptraceError "pownCurrencySymbol: not a minting transaction"

-- * Functions for working with the `PTryFrom` class

-- | Copied from plutarch-extra
ptryFromData ::
  forall {s :: S} (a :: PType).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  TermCont s (Term s (PAsData a))
ptryFromData = fmap fst . tcont . ptryFrom @(PAsData a)

ptryFromUndata ::
  forall {s :: S} (a :: PType).
  (PIsData a, PTryFrom PData (PAsData a)) =>
  Term s PData ->
  TermCont s (Term s a)
ptryFromUndata = fmap pfromData . ptryFromData @a

psignedBy ::
  ClosedTerm
    ( PPubKeyHash
        :--> PBuiltinList (PAsData PPubKeyHash)
        :--> PBool
    )
psignedBy = phoistAcyclic $
  plam $
    \pkh signatories -> pelem # pdata pkh # signatories

{- HLINT ignore "Use join" -}
pgetOwnTxOut :: ClosedTerm (PScriptPurpose :--> PBuiltinList PTxInInfo :--> PTxOut)
pgetOwnTxOut = phoistAcyclic $ plam $ \scriptPurpose inputs -> P.do
  let ownOutRef = P.do
        PSpending ownOutRef' <- pmatch scriptPurpose
        pfield @"_0" # ownOutRef'
  P.do
    PJust ownInput <- pmatch $ pfindOwnInput # inputs # ownOutRef
    pfield @"resolved" # ownInput

pextractBasketState :: ClosedTerm (PMap 'Unsorted PDatumHash PDatum :--> PTxOut :--> PBasketState)
pextractBasketState = phoistAcyclic $ plam $ \datumMap txOut -> P.do
  PJust basketState <-
    pmatch $ getDataByHash @PBasketState # txOut # datumMap
  basketState

pextractStakePoolUtxoDatum :: ClosedTerm (PTxOut :--> PStakePoolUtxoDatum)
pextractStakePoolUtxoDatum = phoistAcyclic $ plam $ \txOut -> P.do
  POutputDatum inputDatum <- pmatch $ pfromData $ pfield @"datum" # txOut
  PDatum inputDatum' <- pmatch $ pfromData $ pfield @"outputDatum" # inputDatum
  unTermCont $ ptryFromUndata inputDatum'

-- * Platform configuration

equalPaymentCredentials :: ClosedTerm (PAddress :--> PAddress :--> PBool)
equalPaymentCredentials = phoistAcyclic $ plam $ \addr1 addr2 ->
  let add1PaymentCred = pfield @"credential" # addr1
      add2PaymentCred = pfield @"credential" # addr2
   in add1PaymentCred #== add2PaymentCred

padaValue :: ClosedTerm (PInteger :--> PValue 'Sorted 'NonZero)
padaValue = phoistAcyclic $ plam $ \amount ->
  V1.psingleton # V1.padaSymbol # V1.padaToken # amount

pintervalDuration :: ClosedTerm (PPOSIXTimeRange :--> PDiffMilliSeconds)
pintervalDuration = phoistAcyclic $ plam $ \range -> P.do
  PInterval interval' <- pmatch range
  interval <- pletFields @'["from", "to"] interval'
  lowerBound <- pletFields @'["_0", "_1"] interval.from
  upperBound <- pletFields @'["_0", "_1"] interval.to
  PFinite from' <- pmatch lowerBound._0
  PFinite to' <- pmatch upperBound._0
  let from = pfromData $ pfield @"_0" # from'
      to = pfromData $ pfield @"_0" # to'
  pcon $ MkPDiffMilliSeconds $ pto $ to - from

data PBasketStateUtxoValidation (s :: S)
  = MkPBasketStateUtxoValidation
      (Term s PBasketState)
      (Term s PBasketState)
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PBasketStateUtxoValidation where type DPTStrat _ = PlutusTypeScott

ensureOnlyBasketStateUtxoConsumed ::
  ClosedTerm
    ( PBasketStateCS
        :--> PTokenName
        :--> PScriptPurpose
        :--> PBuiltinList PTxInInfo
        :--> PBuiltinList PTxOut
        :--> PMap 'Unsorted PDatumHash PDatum
        :--> PBasketStateUtxoValidation
    )
ensureOnlyBasketStateUtxoConsumed =
  phoistAcyclic $
    plam $
      \basketStateCS basketStateTN purpose inputs outputs datumMap -> P.do
        ownResolved <-
          plet $
            pgetOwnTxOut # purpose # inputs
        ownCredential <-
          plet $
            pfield @"credential" #$ pfield @"address" # ownResolved

        let fromBasketValidator =
              findInputAtPaymentCredential
                # ownCredential
                # inputs

        perrorIfFalse
          "Spending more than one UTxO from basket validator address"
          ((plength # fromBasketValidator) #== 1)

        perrorIfFalse
          "No basket token at basket state utxo"
          (ptokenInUtxo # pto basketStateCS # basketStateTN # ownResolved)

        toBasketValidator <-
          plet $
            findOutputAtPaymentCredential
              # ownCredential
              # outputs

        perrorIfFalse
          "More than one output utxo at own address"
          ((plength # toBasketValidator) #== 1)

        PCons outputUtxo _ <- pmatch toBasketValidator

        perrorIfFalse
          "BasketState UTxO address must not change"
          (pfield @"address" # ownResolved #== pfield @"address" # outputUtxo)

        perrorIfFalse
          "Incorrect value at propagated basket state utxo"
          ((pfield @"value" # outputUtxo) #== (pfield @"value" # ownResolved))

        let inputBasketState = pextractBasketState # datumMap # ownResolved
        let outputBasketState = pextractBasketState # datumMap # outputUtxo

        pcon $
          MkPBasketStateUtxoValidation
            inputBasketState
            outputBasketState
