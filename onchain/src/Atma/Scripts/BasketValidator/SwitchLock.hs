{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PackageImports #-}

module Atma.Scripts.BasketValidator.SwitchLock (pswitchPledgeLock, pswitchBasketLock) where

import Plutarch.Api.V1.Value (pnormalize)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Monadic qualified as P

import Atma.PTypes (
  PBasketLock (MkPLocked, MkPUnlocked),
  PBasketParams,
  PBasketState,
  PLockingParams,
  plocked,
  punlocked,
 )
import Atma.Utils (
  PBasketStateUtxoValidation (MkPBasketStateUtxoValidation),
  ensureOnlyBasketStateUtxoConsumed,
  perrorIfFalse,
  pintervalDuration,
  psignedBy,
  pupdateBasketLock,
  pupdatePledgeLock,
 )

import Atma.Scripts.Common (pbasketStateTN)
import Plutarch.Api.V2 (PPOSIXTime (PPOSIXTime))
import Plutarch.Extra.Interval (pmember)
import Plutarch.Extra.Maybe (passertPDJust)
import "plutarch-extra" Plutarch.Extra.TermCont (pguardC)

-- TODO: update this description

{- |  `pswitchBasketLock` is used to validate `MkSwitchLock` redeemer

      Following checks are performed:
      - No tokens are minted nor burnt
      - Basket state UTxO is the only output to BasketValidator
        - Must contain BasketState token

      - Basket state utxo is propagated
        - Address is preserved
        - Value is preserved
        - Datum is only differs by 'lock' field
      - Basket state datum 'lock' field correctly updated
        - If input BasketState is locked, output checked to be unlocked
        - and vice versa
      - Previous lock switch timestamp is less than current
      - Current lock switch contains within Tx validity range
      - Tx validity range is no greater than 'maxTxValidityDuration'
      - If tx unlocks basket, either:
        - Tx is singed by admin
        - 'maxLockDuration' expired
      - If Tx locks basket:
        - Tx is signed by admin
        - 'minLockInterval' expired
-}
pswitchLock ::
  ClosedTerm
    ( (PBasketParams :--> PLockingParams)
        :--> (PBasketState :--> PBasketLock)
        :--> (PBasketState :--> PBasketLock :--> PBasketState)
        :--> (PBasketParams :--> PPOSIXTime :--> PScriptContext :--> POpaque)
    )
pswitchLock = phoistAcyclic $ plam $ \extractLockParams extractLock updateLock basketParams' switchAt ctx' -> P.do
  ctx <- pletFields @'["purpose", "txInfo"] ctx'
  txInfo <-
    pletFields
      @'[ "mint"
        , "signatories"
        , "validRange"
        , "inputs"
        , -- inputs amount are unchecked since it's not possible to
          -- spend more than one
          -- because:
          --  1. Other redeemers are incompatible with PSwitchLock
          --  2. No endpoints allow to bypass redeemer check
          --  3. Only 1 BasketState UTxO exists and it's spend by this redeemer
          "outputs"
        ]
      ctx.txInfo

  let mint = pnormalize # txInfo.mint
      signatories = txInfo.signatories

  basketParam <-
    pletFields
      @'[ "adminPkh"
        , "basketStateCS"
        ]
      basketParams'

  lockingParams <-
    pletFields
      @'[ "minLockInterval"
        , "maxTxValidityDuration"
        , "maxLockDuration"
        ]
      (extractLockParams # basketParams')

  perrorIfFalse
    "Minting tokens"
    (mint #== mempty)

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

  -- Locking checks

  perrorIfFalse
    "Tx validity range is too large"
    (pintervalDuration # txInfo.validRange #<= lockingParams.maxTxValidityDuration)

  perrorIfFalse
    "Lock switch time is outside of tx validity range"
    (pmember # pdata switchAt # txInfo.validRange)

  let adminPkh = pfromData $ pfield @"adminPkh" # pto inputBasketState
  isSignedByAdmin <- plet $ psignedBy # pto adminPkh # signatories

  (prevSwitchAt, expectedOutBasketState) <- runTermCont do
    lock <- tcont $ pmatch $ extractLock # inputBasketState
    case lock of
      MkPUnlocked unlocked -> do
        unlockedAt <- tcont $ plet $ pfield @"unlockedAt" # unlocked
        pguardC
          "Not signed by an administrator"
          isSignedByAdmin
        let minLockIntervalTime =
              pcon $
                PPOSIXTime $
                  pto $
                    pfromData lockingParams.minLockInterval

            canLockFrom = unlockedAt + minLockIntervalTime

            basketState' :: Term _ PBasketState
            basketState' =
              updateLock
                # inputBasketState
                # (plocked # switchAt)
        pguardC
          "Can't lock again that soon"
          (canLockFrom #< switchAt)

        pure (unlockedAt, basketState')
      MkPLocked locked -> do
        lockedAt <- tcont $ plet $ pfield @"lockedAt" # locked
        let maxLockDurationTime =
              pcon $
                PPOSIXTime $
                  pto $
                    pfromData lockingParams.maxLockDuration

            canUnlockFrom = lockedAt + maxLockDurationTime

            basketState' :: Term _ PBasketState
            basketState' =
              updateLock
                # inputBasketState
                # (punlocked # switchAt)
        pguardC
          "User can't unlock that soon, so admin signature required"
          (isSignedByAdmin #|| (canUnlockFrom #< switchAt))
        pure (lockedAt, basketState')

  perrorIfFalse
    "BasketState has been incorrectly updated"
    ( outputBasketState #== expectedOutBasketState
    )

  perrorIfFalse
    "Lock switch time must be after previous one"
    (prevSwitchAt #< switchAt)

  popaque $ pconstant ()

pswitchBasketLock :: ClosedTerm (PBasketParams :--> PPOSIXTime :--> PScriptContext :--> POpaque)
pswitchBasketLock =
  pswitchLock
    # plam (\basketParams -> pfield @"lockingParams" # pto basketParams)
    # plam (\basketState -> pfield @"lock" # pto basketState)
    # pupdateBasketLock

pswitchPledgeLock :: ClosedTerm (PBasketParams :--> PPOSIXTime :--> PScriptContext :--> POpaque)
pswitchPledgeLock =
  pswitchLock
    # plam
      ( \basketParams ->
          passertPDJust
            # "Error trying to extract pledge locking params"
            #$ pfield @"pledgeLockingParams"
            # pto basketParams
      )
    # plam (\basketState -> pfield @"pledgeLock" # pto basketState)
    # pupdatePledgeLock
