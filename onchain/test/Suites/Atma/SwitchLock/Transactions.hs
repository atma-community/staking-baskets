{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.SwitchLock.Transactions (
  switchLockAsPkhContract,
  switchLockBlocking,
  switchLockAsPkhWith,
  defaultBasketStateUpdate,
  canSwitchLocksAfter,
  UpdateBasketStateFields (
    update'exRate',
    update'numOfStakePoolUTxOs',
    update'lock',
    update'pledgeLock'
  ),
  LockSwitch (Lock, Unlock),
  LockType (BasketLock, PledgeLock),
  lockAsUser,
  waitUntilCanSwitch,
) where

import Control.Monad.Cont (MonadTrans (lift))
import Control.Monad.Reader.Class (ask, asks)

import Plutus.Model (
  DatumMode (HashDatum),
  submitTx,
 )
import Plutus.Model.V2 (
  currentTime,
  payToRef,
  spendScriptRef,
  validateIn,
 )

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  POSIXTime (POSIXTime),
  POSIXTimeRange,
  TxOut (txOutValue),
 )

import Atma.PTypes (
  PBasketLock,
  PBasketState,
  PBasketValidatorRedeemer (
    MkPSwitchBasketLock,
    MkPSwitchPledgeLock
  ),
  pbasketState,
  plocked,
  punlocked,
 )
import Atma.Types (
  AdminPubKeyHash (getAdminPubKeyHash),
  BasketLock (Locked, Unlocked),
  BasketParams (MkBasketParams),
  LockingParams (MkLockingParams),
  lockingParams,
  maxLockDuration,
  maxTxValidityDuration,
  minLockInterval,
  pledgeLockingParams,
 )

import Atma.Utils (pupdateBasketLock, pupdatePledgeLock)
import Atma.Utils.ExRate (PExRate)
import Control.Monad (void, when, (<=<))
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import Suites.Atma.Common (
  floorPosixTime,
  waitFor,
 )
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    basketParams,
    basketValidatorUtxo
  ),
  basketValidatorBasketStateUtxo,
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
 )
import Suites.Atma.Common.Utils (
  User (User),
  findBasketStateUtxo,
 )

data LockType
  = BasketLock
  | PledgeLock

data LockSwitch
  = Lock
  | Unlock (Maybe User)

getLockingParams :: LockType -> BasketContext (Maybe LockingParams)
getLockingParams lockType = asks $ getLock . basketParams
  where
    getLock = case lockType of
      BasketLock -> Just . lockingParams
      PledgeLock -> pledgeLockingParams

withLock ::
  forall r a s.
  LockType ->
  (Term s PBasketLock -> TermCont @r s (Term s PBasketLock, a)) ->
  Term s PBasketState ->
  TermCont @r s (Term s PBasketState, a)
withLock lockType f state = do
  (lock', result) <- f case lockType of
    BasketLock -> pfield @"lock" # state
    PledgeLock -> pfield @"pledgeLock" # state
  let
    state' = case lockType of
      BasketLock -> pupdateBasketLock # state # lock'
      PledgeLock -> pupdatePledgeLock # state # lock'
  pure (state', result)

extractLock :: ClosedTerm PBasketState -> LockType -> BasketLock
extractLock pstate = \case
  BasketLock -> plift $ pfield @"lock" # pstate
  PledgeLock -> plift $ pfield @"pledgeLock" # pstate

waitUntilCanSwitch :: LockType -> LockSwitch -> BasketContext Bool
waitUntilCanSwitch lockType switch = do
  params <- asks basketParams
  (_, PlutarchAdapter basketState) <- findBasketStateUtxo
  case extractLock basketState lockType of
    Locked _ | Lock <- switch -> pure False
    Unlocked _ | Unlock _ <- switch -> pure False
    _ -> do
      let
        (basketLockAt, pledgeLockAt) = canSwitchLocksAfter params basketState
        canSwitchAt = case lockType of
          BasketLock -> basketLockAt
          PledgeLock -> case pledgeLockAt of
            Just at -> at
            Nothing ->
              -- due to laziness it means to trigger at the very last moment
              -- when we already expect basket to be Pledge basket
              error "Can't wait until can switch pledge lock of a non-pledge basket"
      void $ waitFor canSwitchAt
      pure True

switchLockBlocking :: LockType -> LockSwitch -> BasketContext ()
switchLockBlocking lockType switch = do
  waited <- waitUntilCanSwitch lockType switch
  when waited do
    switchLockAsPkhContract lockType switch

plockRedeemer ::
  POSIXTime ->
  LockType ->
  ClosedTerm PBasketValidatorRedeemer
plockRedeemer at = \case
  BasketLock -> pcon $ MkPSwitchBasketLock $ pdcons # pdata (pconstant at) # pdnil
  PledgeLock -> pcon $ MkPSwitchPledgeLock $ pdcons # pdata (pconstant at) # pdnil

switchLockAsPkhContract :: LockType -> LockSwitch -> BasketContext ()
switchLockAsPkhContract lockType switch =
  switchLockAsPkhWith lockType switch defaultBasketStateUpdate

switchLockAsPkhWith :: LockType -> LockSwitch -> UpdateBasketStateFields -> BasketContext ()
switchLockAsPkhWith lockType switch MkUpdateBasketStateFields {..} = do
  ctx <- ask
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo
  (now, validFor) <- safeValidityRange lockType
  let
    pbasketLock :: ClosedTerm PBasketLock
    pbasketLock = pfield @"lock" # basketState
    ppledgeLock :: ClosedTerm PBasketLock
    ppledgeLock = pfield @"pledgeLock" # basketState
    pupdatedLock :: ClosedTerm PBasketLock
    pupdatedLock = case switch of
      Lock -> plocked # pconstant now
      Unlock _ -> punlocked # pconstant now
    (PlutarchAdapter pbasketLock', PlutarchAdapter ppledgeLock') = case lockType of
      BasketLock -> (PlutarchAdapter pupdatedLock, PlutarchAdapter ppledgeLock)
      PledgeLock -> (PlutarchAdapter pbasketLock, PlutarchAdapter pupdatedLock)

    adminPkh =
      plift $ pfromData $ pfield @"adminPkh" # pto basketState

    updatedDatum :: ClosedTerm PBasketState
    updatedDatum =
      pbasketState
        # update'exRate' (pfield @"exRate" # basketState)
        # update'numOfStakePoolUTxOs' (pfield @"numOfStakePoolUTxOs" # basketState)
        # update'lock' pbasketLock'
        # update'pledgeLock' ppledgeLock'
        # pconstant adminPkh
    admin = getAdminPubKeyHash adminPkh

  let scriptRef = basketValidatorUtxo ctx
      submitAs = case switch of
        Lock -> admin
        Unlock as -> maybe admin coerce as
  tx <-
    lift $
      validateIn validFor $
        mconcat
          [ spendScriptRef
              scriptRef
              (basketValidatorBasketStateUtxo ctx)
              (fst basketStateUtxo)
              (PlutarchAdapter $ plockRedeemer now lockType)
              (PlutarchAdapter basketState)
          , payToRef
              (basketValidatorBasketStateUtxo ctx)
              (HashDatum $ PlutarchAdapter updatedDatum)
              (txOutValue $ snd basketStateUtxo)
          ]
  lift $ submitTx submitAs tx

data UpdateBasketStateFields = MkUpdateBasketStateFields
  { update'exRate' :: ClosedTerm PExRate -> ClosedTerm PExRate
  , update'numOfStakePoolUTxOs' :: ClosedTerm PInteger -> ClosedTerm PInteger
  , update'lock' :: ClosedTerm PBasketLock -> ClosedTerm PBasketLock
  , update'pledgeLock' :: ClosedTerm PBasketLock -> ClosedTerm PBasketLock
  }

defaultBasketStateUpdate :: UpdateBasketStateFields
defaultBasketStateUpdate =
  MkUpdateBasketStateFields
    { update'exRate' = id
    , update'numOfStakePoolUTxOs' = id
    , update'lock' = id
    , update'pledgeLock' = id
    }

lockAsUser :: LockType -> User -> BasketContext ()
lockAsUser lockType user = do
  ctx <- ask
  (basketStateUtxo, PlutarchAdapter basketState) <- findBasketStateUtxo
  (now, validFor) <- safeValidityRange lockType
  let
    scriptRef = basketValidatorUtxo ctx
    basketState' :: ClosedTerm PBasketState
    basketState' =
      unTermCont $
        fst
          <$> withLock
            lockType
            (const $ pure (plocked # pconstant now, ()))
            basketState

  lift $
    submitTx (coerce user) <=< validateIn validFor $
      mconcat
        [ spendScriptRef
            scriptRef
            (basketValidatorBasketStateUtxo ctx)
            (fst basketStateUtxo)
            (PlutarchAdapter $ plockRedeemer now lockType)
            (PlutarchAdapter basketState)
        , payToRef
            (basketValidatorBasketStateUtxo ctx)
            (HashDatum $ PlutarchAdapter basketState')
            (txOutValue $ snd basketStateUtxo)
        ]

canSwitchLocksAfter ::
  BasketParams ->
  ClosedTerm PBasketState ->
  (POSIXTime, Maybe POSIXTime)
canSwitchLocksAfter MkBasketParams {..} pbasketState =
  ( canSwitchAt lockingParams basketLock
  , flip canSwitchAt pledgeLock <$> pledgeLockingParams
  )
  where
    basketLock :: BasketLock
    basketLock = plift $ pfield @"lock" # pbasketState
    pledgeLock :: BasketLock
    pledgeLock = plift $ pfield @"pledgeLock" # pbasketState
    canSwitchAt :: LockingParams -> BasketLock -> POSIXTime
    canSwitchAt params = \case
      Locked at -> coerce (maxLockDuration params) + at
      Unlocked at -> coerce (minLockInterval params) + at

safeValidityRange :: LockType -> BasketContext (POSIXTime, POSIXTimeRange)
safeValidityRange lockType = do
  lockingParams <- asks $ lockingParams . basketParams
  MkLockingParams {maxTxValidityDuration} <-
    fromMaybe lockingParams <$> getLockingParams lockType
  now <- lift currentTime -- this time is already rounded with slot length
  let to = floorPosixTime $ coerce maxTxValidityDuration
  pure (now, interval now (to + now))
