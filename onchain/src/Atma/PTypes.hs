{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Atma.PTypes (
  PDiffMilliSeconds (MkPDiffMilliSeconds),
  PBasketParams (MkPBasketParams),
  PAdminPubKeyHash (MkPAdminPubKeyHash),
  PBasketStateCS (MkPBasketStateCS),
  PStakingPoolCS (MkPStakingPoolCS),
  PBasketValidatorRedeemer (
    MkPDeposit,
    MkPWithdraw,
    MkPUpdateExRate,
    MkPRebalance,
    MkPDonate,
    MkPRebalanceDelegate,
    MkPStakePoolMPTriggerWitness,
    MkPSwitchBasketLock,
    MkPSwitchPledgeLock,
    MkPSetAdmin
  ),
  PBasketState (MkPBasketState),
  PStakePoolUtxoDatum (MkPStakePoolUtxoDatum),
  PLockingParams (MkPLockingParams),
  PBasketLock (
    MkPLocked,
    MkPUnlocked
  ),
  PBasketTokenCS (MkPBasketTokenCS),
  PBasketTokenTN (MkPBasketTokenTN),
  PStrictDiffMilliSeconds,
  -- PCycle (MkPCycle),
  PPoolPubKeyHash (..),
  pdiffMilliSeconds,
  pstrictDiffMilliSeconds,
  pbasketState,
  pstakePoolUtxoDatum,
  plocked,
  punlocked,
) where

import Atma.Types (
  AdminPubKeyHash (MkAdminPubKeyHash),
  BasketLock,
  BasketParams,
  BasketStateCS (MkBasketStateCS),
  BasketTokenCS (MkBasketTokenCS),
  BasketTokenTN (MkBasketTokenTN),
  LockingParams,
  PoolPubKeyHash (MkPoolPubKeyHash),
  StakePoolUtxoDatum,
  StakingPoolCS (MkStakingPoolCS),
 )
import Atma.Utils.ExRate (PExRate)
import GHC.Generics qualified as GHC (Generic)
import Plutarch.Api.V1 (
  PCurrencySymbol,
  PMaybeData,
  PPOSIXTime,
  PPubKeyHash,
  PTokenName,
 )

import Plutarch.Api.V1.Scripts (PScriptHash)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import Plutarch.Positive (PPositive)
import Plutarch.Test.QuickCheck (
  PArbitrary (parbitrary, pshrink),
  TestableTerm (TestableTerm),
 )
import Plutarch.Test.QuickCheck.Instances ()
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (DiffMilliSeconds))
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  NonNegative (getNonNegative),
 )

-- * Script parameters

type PDiffMilliSeconds :: PType
newtype PDiffMilliSeconds (s :: S)
  = MkPDiffMilliSeconds (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass
    ( PlutusType
    , PNum
    , PIntegral
    , PIsData
    , PEq
    , PPartialOrd
    , POrd
    , PShow
    )

instance DerivePlutusType PDiffMilliSeconds where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PDiffMilliSeconds)

instance PUnsafeLiftDecl PDiffMilliSeconds where
  type PLifted PDiffMilliSeconds = DiffMilliSeconds

deriving via
  (DerivePConstantViaNewtype DiffMilliSeconds PDiffMilliSeconds PInteger)
  instance
    PConstantDecl DiffMilliSeconds

instance PArbitrary PDiffMilliSeconds where
  parbitrary :: Gen (TestableTerm PDiffMilliSeconds)
  parbitrary = do
    diff <- getNonNegative <$> arbitrary
    pure $ TestableTerm $ pcon $ MkPDiffMilliSeconds $ pconstant diff

  pshrink ::
    TestableTerm PDiffMilliSeconds ->
    [TestableTerm PDiffMilliSeconds]
  pshrink = \case
    TestableTerm cycle ->
      [ TestableTerm (pcon $ MkPDiffMilliSeconds x)
      | TestableTerm x <- pshrink $ TestableTerm (pto cycle)
      , plift (0 #<= x)
      ]

type PLockingParams :: PType
newtype PLockingParams (s :: S)
  = MkPLockingParams
      ( Term
          s
          ( PDataRecord
              '[ "minLockInterval" ':= PDiffMilliSeconds
               , -- Minimal time that must pass from unlocking before next lock
                 "maxLockDuration" ':= PDiffMilliSeconds
               , -- After this amount of time from lock non-admin users can unlock basket
                 "maxTxValidityDuration" ':= PDiffMilliSeconds
                 -- Maximum duration of validity range for lock switching Tx
                 -- This parameter must be significantly smaller
                 -- than 'maxLockDuration' and 'minLockInterval'
                 -- Too large parameter allows locking far in the future
                 -- or unlocking far before 'maxLockDuration'
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    , PEq
    , PPartialOrd
    , POrd
    , PShow
    )

instance DerivePlutusType PLockingParams where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData PLockingParams
deriving anyclass instance PTryFrom PData (PAsData PLockingParams)

instance PUnsafeLiftDecl PLockingParams where
  type PLifted PLockingParams = LockingParams

deriving via
  (DerivePConstantViaData LockingParams PLockingParams)
  instance
    PConstantDecl LockingParams

type PStrictDiffMilliSeconds :: PType
newtype PStrictDiffMilliSeconds (s :: S)
  = MkPStrictDiffMilliSeconds (Term s PDiffMilliSeconds)
  deriving stock (GHC.Generic)
  deriving anyclass
    ( PlutusType
    , PNum
    , PIntegral
    , PIsData
    , PEq
    , PPartialOrd
    , POrd
    , PShow
    )

instance DerivePlutusType PStrictDiffMilliSeconds where
  type DPTStrat _ = PlutusTypeNewtype

instance PArbitrary PStrictDiffMilliSeconds where
  parbitrary :: Gen (TestableTerm PStrictDiffMilliSeconds)
  parbitrary = do
    TestableTerm diff <- parbitrary @PPositive
    pure $
      TestableTerm $
        pcon $
          MkPStrictDiffMilliSeconds $
            pcon $
              MkPDiffMilliSeconds (pto diff)

  pshrink ::
    TestableTerm PStrictDiffMilliSeconds ->
    [TestableTerm PStrictDiffMilliSeconds]
  pshrink = \case
    TestableTerm cycle ->
      [ TestableTerm (pcon $ MkPStrictDiffMilliSeconds x)
      | TestableTerm x <- pshrink $ TestableTerm (pto cycle)
      , plift (0 #< x)
      ]

pstrictDiffMilliSeconds ::
  forall {s :: S}.
  Term s (PDiffMilliSeconds :--> PStrictDiffMilliSeconds)
pstrictDiffMilliSeconds =
  phoistAcyclic $
    plam $
      pcon . MkPStrictDiffMilliSeconds

pdiffMilliSeconds ::
  ClosedTerm (PInteger :--> PDiffMilliSeconds)
pdiffMilliSeconds =
  phoistAcyclic $
    plam $
      pcon . MkPDiffMilliSeconds

plockingParams ::
  ClosedTerm
    ( PAsData PDiffMilliSeconds
        :--> PAsData PDiffMilliSeconds
        :--> PAsData PDiffMilliSeconds
        :--> PLockingParams
    )
plockingParams =
  phoistAcyclic $
    plam $
      \minLockInterval maxLockDuration maxTxValidityDuration ->
        pcon $
          MkPLockingParams $
            pdcons
              # minLockInterval
              #$ pdcons
              # maxLockDuration
              #$ pdcons
              # maxTxValidityDuration
              #$ pdnil

instance PArbitrary PLockingParams where
  parbitrary :: Gen (TestableTerm PLockingParams)
  parbitrary = do
    (TestableTerm minLockInterval) <-
      parbitrary @PStrictDiffMilliSeconds

    (TestableTerm maxLockDuration) <-
      parbitrary @PStrictDiffMilliSeconds

    (TestableTerm maxTxValidityDuration) <-
      parbitrary @PStrictDiffMilliSeconds

    pure $
      TestableTerm $
        plockingParams
          # pdata (pto minLockInterval)
          # pdata (pto maxLockDuration)
          # pdata (pto maxTxValidityDuration)

  pshrink ::
    TestableTerm PLockingParams ->
    [TestableTerm PLockingParams]
  pshrink = \case
    TestableTerm params -> do
      let minLockInterval' =
            TestableTerm $ pfield @"minLockInterval" # params
          maxLockDuration' =
            TestableTerm $ pfield @"maxLockDuration" # params
          maxTxValidityDuration' =
            TestableTerm $ pfield @"maxTxValidityDuration" # params

      TestableTerm minLockInterval <- pshrink minLockInterval'
      TestableTerm maxLockDuration <- pshrink maxLockDuration'
      TestableTerm maxTxValidityDuration <- pshrink maxTxValidityDuration'

      pure $
        TestableTerm $
          plockingParams
            # minLockInterval
            # maxLockDuration
            # maxTxValidityDuration

type PBasketParams :: PType
newtype PBasketParams (s :: S)
  = MkPBasketParams
      ( Term
          s
          ( PDataRecord
              '[ "voters" ':= PBuiltinList (PAsData PPubKeyHash)
               , "lockingParams" ':= PLockingParams
               , "pledgeLockingParams" ':= PMaybeData PLockingParams
               , "basketStateCS" ':= PBasketStateCS
               , "basketTokenCS" ':= PBasketTokenCS
               , "stakingPoolCS" ':= PStakingPoolCS
               , "basketTokenTN" ':= PBasketTokenTN
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow, PDataFields)

instance DerivePlutusType PBasketParams where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData PBasketParams
deriving anyclass instance PTryFrom PData (PAsData PBasketParams)

instance PUnsafeLiftDecl PBasketParams where
  type PLifted PBasketParams = BasketParams

deriving via
  (DerivePConstantViaData BasketParams PBasketParams)
  instance
    PConstantDecl BasketParams

instance PArbitrary PBasketParams where
  parbitrary :: Gen (TestableTerm PBasketParams)
  parbitrary = do
    TestableTerm voters <- parbitrary
    TestableTerm lockingParams <- parbitrary
    TestableTerm pledgeLockingParams <- parbitrary
    TestableTerm basketStateCS <- parbitrary
    TestableTerm basketTokenCS <- parbitrary
    TestableTerm stakingPoolCS <- parbitrary
    TestableTerm basketTokenTN <- parbitrary
    pure $
      TestableTerm $
        pcon $
          MkPBasketParams $
            pdcons @"voters"
              # voters
              #$ pdcons @"lockingParams"
              # lockingParams
              #$ pdcons @"pledgeLockingParams"
              # pledgeLockingParams
              #$ pdcons @"basketStateCS"
              # basketStateCS
              #$ pdcons @"basketTokenCS"
              # basketTokenCS
              #$ pdcons @"stakingPoolCS"
              # stakingPoolCS
              #$ pdcons @"basketTokenTN"
              # basketTokenTN
              #$ pdnil

type PPoolPubKeyHash :: PType
newtype PPoolPubKeyHash (s :: S)
  = MkPPoolPubKeyHash (Term s PPubKeyHash)
  deriving stock (GHC.Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PEq
    , PPartialOrd
    , POrd
    , PShow
    )

instance DerivePlutusType PPoolPubKeyHash where
  type DPTStrat _ = PlutusTypeNewtype

-- deriving anyclass instance PTryFrom PData PPoolPubKeyHash
deriving anyclass instance PTryFrom PData (PAsData PPoolPubKeyHash)

instance PUnsafeLiftDecl PPoolPubKeyHash where
  type PLifted PPoolPubKeyHash = PoolPubKeyHash

deriving via
  (DerivePConstantViaNewtype PoolPubKeyHash PPoolPubKeyHash PPubKeyHash)
  instance
    PConstantDecl PoolPubKeyHash

-- -- * StakePoolUtxoDatum

type PStakePoolUtxoDatum :: PType
newtype PStakePoolUtxoDatum (s :: S)
  = MkPStakePoolUtxoDatum
      ( Term
          s
          ( PDataRecord
              '[ "poolPkh" ':= PMaybeData (PAsData PPoolPubKeyHash)
               , -- if utxo delegates then PoolPubKeyHash of stake pool utxo delegates to; else Nothing
                 "basketTokenCounter" ':= PInteger
                 -- In order to find out what is the total number of basket tokens that exist at the moment
                 -- one has to sum up `basketTokenCounter`s of all stake pool utxos that belong to a Basket.
                 -- `basketTokenCounter` can go negative.
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PDataFields)

instance DerivePlutusType PStakePoolUtxoDatum where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData (PAsData PStakePoolUtxoDatum)

instance PUnsafeLiftDecl PStakePoolUtxoDatum where
  type PLifted PStakePoolUtxoDatum = StakePoolUtxoDatum

deriving via
  (DerivePConstantViaData StakePoolUtxoDatum PStakePoolUtxoDatum)
  instance
    PConstantDecl StakePoolUtxoDatum

-- -- * State

-- -- TODO: Make pconstant/plift if possible
type PBasketState :: PType
newtype PBasketState (s :: S)
  = MkPBasketState
      ( Term
          s
          ( PDataRecord
              '[ "exRate" ':= PExRate
               , -- Exchange rate of basket tokens for Coin. Common for deposits and withdrawals.
                 "numOfStakePoolUTxOs" ':= PInteger
               , -- Number of stake pool utxos existing at the moment.
                 "lock" ':= PBasketLock
               , -- Administrator lock, that temporarily prevents users from interacting with the basket
                 -- Locks have limited duration and interval, this is configured in 'BasketParams'
                 "pledgeLock" ':= PBasketLock
               , -- key hash of the administrator of the basket, only the admin can:
                 -- create and destroy stake pool utxos
                 -- rebalance and update the exchange rate
                 -- set basket and pledge lock
                 "adminPkh" ':= PAdminPubKeyHash
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow, PDataFields)

instance DerivePlutusType PBasketState where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData (PAsData PBasketState)

pbasketState ::
  ClosedTerm
    (PExRate :--> PInteger :--> PBasketLock :--> PBasketLock :--> PAdminPubKeyHash :--> PBasketState)
pbasketState = phoistAcyclic $ plam $ \exRate numOfStakePoolUTxOs lock pledgeLock adminPkh ->
  pcon $
    MkPBasketState $
      pdcons
        # pdata exRate
        #$ pdcons
        # pdata numOfStakePoolUTxOs
        #$ pdcons
        # pdata lock
        #$ pdcons
        # pdata pledgeLock
        #$ pdcons
        # pdata adminPkh
        # pdnil

pstakePoolUtxoDatum ::
  ClosedTerm
    (PMaybeData (PAsData PPoolPubKeyHash) :--> PInteger :--> PStakePoolUtxoDatum)
pstakePoolUtxoDatum = phoistAcyclic $ plam $ \poolPkh basketTokenCounter ->
  pcon $
    MkPStakePoolUtxoDatum $
      pdcons
        # pdata poolPkh
        #$ pdcons
        # pdata basketTokenCounter
        # pdnil

-- -- * Lock
data PBasketLock (s :: S)
  = MkPLocked
      ( Term
          s
          ( PDataRecord
              '[ "lockedAt" ':= PPOSIXTime
               ]
          )
      )
  | MkPUnlocked
      ( Term
          s
          ( PDataRecord
              '[ "unlockedAt" ':= PPOSIXTime
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketLock where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData PBasketLock
deriving anyclass instance PTryFrom PData (PAsData PBasketLock)

instance PUnsafeLiftDecl PBasketLock where
  type PLifted PBasketLock = BasketLock

deriving via
  (DerivePConstantViaData BasketLock PBasketLock)
  instance
    (PConstantDecl BasketLock)

plocked :: ClosedTerm (PPOSIXTime :--> PBasketLock)
plocked = phoistAcyclic $ plam $ \at ->
  pcon $ MkPLocked $ pdcons # pdata at # pdnil

punlocked :: ClosedTerm (PPOSIXTime :--> PBasketLock)
punlocked = phoistAcyclic $ plam $ \at ->
  pcon $ MkPUnlocked $ pdcons # pdata at # pdnil

-- -- * Basket validator redeemer

data PBasketValidatorRedeemer (s :: S)
  = MkPDeposit
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPWithdraw
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPUpdateExRate
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPRebalance
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPDonate
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPStakePoolMPTriggerWitness
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPRebalanceDelegate
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  | MkPSwitchBasketLock
      ( Term
          s
          ( PDataRecord
              '["atTime" ':= PPOSIXTime]
          )
      )
  | MkPSwitchPledgeLock
      ( Term
          s
          ( PDataRecord
              '["atTime" ':= PPOSIXTime]
          )
      )
  | MkPSetAdmin
      ( Term
          s
          ( PDataRecord
              '[]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketValidatorRedeemer where
  type DPTStrat _ = PlutusTypeData

deriving anyclass instance PTryFrom PData PBasketValidatorRedeemer
deriving anyclass instance PTryFrom PData (PAsData PBasketValidatorRedeemer)

-- -- * Basket scripts

newtype PBasketStateCS s = MkPBasketStateCS
  {getPBasketStateCS :: Term s PCurrencySymbol}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketStateCS where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PBasketStateCS)

instance PUnsafeLiftDecl PBasketStateCS where
  type PLifted PBasketStateCS = BasketStateCS

deriving via
  (DerivePConstantViaNewtype BasketStateCS PBasketStateCS PCurrencySymbol)
  instance
    PConstantDecl BasketStateCS

instance PArbitrary PBasketStateCS where
  parbitrary :: Gen (TestableTerm PBasketStateCS)
  parbitrary = do
    TestableTerm cs <- parbitrary
    pure $ TestableTerm $ pcon $ MkPBasketStateCS cs

newtype PBasketTokenTN s = MkPBasketTokenTN
  {getPBasketTokenTN :: Term s PTokenName}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketTokenTN where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PBasketTokenTN)

instance PUnsafeLiftDecl PBasketTokenTN where
  type PLifted PBasketTokenTN = BasketTokenTN

deriving via
  (DerivePConstantViaNewtype BasketTokenTN PBasketTokenTN PTokenName)
  instance
    PConstantDecl BasketTokenTN

instance PArbitrary PBasketTokenTN where
  parbitrary :: Gen (TestableTerm PBasketTokenTN)
  parbitrary = do
    TestableTerm cs <- parbitrary
    pure $ TestableTerm $ pcon $ MkPBasketTokenTN cs

newtype PBasketTokenCS s = MkPBasketTokenCS
  {getPBasketTokenCS :: Term s PCurrencySymbol}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketTokenCS where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PBasketTokenCS)

instance PUnsafeLiftDecl PBasketTokenCS where
  type PLifted PBasketTokenCS = BasketTokenCS

deriving via
  (DerivePConstantViaNewtype BasketTokenCS PBasketTokenCS PCurrencySymbol)
  instance
    PConstantDecl BasketTokenCS

instance PArbitrary PBasketTokenCS where
  parbitrary :: Gen (TestableTerm PBasketTokenCS)
  parbitrary = do
    TestableTerm cs <- parbitrary
    pure $ TestableTerm $ pcon $ MkPBasketTokenCS cs

newtype PStakingPoolCS s = MkPStakingPoolCS
  {getPStakingPoolCS :: Term s PCurrencySymbol}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PStakingPoolCS where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PStakingPoolCS)

instance PUnsafeLiftDecl PStakingPoolCS where
  type PLifted PStakingPoolCS = StakingPoolCS

deriving via
  (DerivePConstantViaNewtype StakingPoolCS PStakingPoolCS PCurrencySymbol)
  instance
    PConstantDecl StakingPoolCS

instance PArbitrary PStakingPoolCS where
  parbitrary :: Gen (TestableTerm PStakingPoolCS)
  parbitrary = do
    TestableTerm cs <- parbitrary
    pure $ TestableTerm $ pcon $ MkPStakingPoolCS cs

newtype PAdminPubKeyHash s = MkPAdminPubKeyHash
  {getPAdminPubKeyHash :: Term s PPubKeyHash}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PAdminPubKeyHash where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PAdminPubKeyHash)

instance PUnsafeLiftDecl PAdminPubKeyHash where
  type PLifted PAdminPubKeyHash = AdminPubKeyHash

deriving via
  (DerivePConstantViaNewtype AdminPubKeyHash PAdminPubKeyHash PPubKeyHash)
  instance
    PConstantDecl AdminPubKeyHash

instance PArbitrary PAdminPubKeyHash where
  parbitrary :: Gen (TestableTerm PAdminPubKeyHash)
  parbitrary = do
    TestableTerm hash <- parbitrary
    pure $ TestableTerm $ pcon $ MkPAdminPubKeyHash hash

newtype PBasketValidatorHash s = MkPBasketValidatorHash
  {getPBasketValidatorHash :: Term s PScriptHash}
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PShow)

instance DerivePlutusType PBasketValidatorHash where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PBasketValidatorHash)
