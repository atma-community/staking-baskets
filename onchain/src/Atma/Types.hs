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

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Atma.Types (
  BasketParams (
    MkBasketParams,
    voters,
    lockingParams,
    pledgeLockingParams,
    basketStateCS,
    basketTokenCS,
    stakingPoolCS
  ),
  BasketLock (
    Locked,
    Unlocked
  ),
  LockingParams (
    MkLockingParams,
    minLockInterval,
    maxLockDuration,
    maxTxValidityDuration
  ),
  AdminPubKeyHash (MkAdminPubKeyHash, getAdminPubKeyHash),
  BasketStateCS (MkBasketStateCS, getBasketStateCS),
  BasketTokenCS (MkBasketTokenCS, getBasketTokenCS),
  BasketTokenTN (MkBasketTokenTN, getBasketTokenTN),
  StakingPoolCS (MkStakingPoolCS, getStakingPoolCS),
  StakePoolUtxoDatum (
    MkStakePoolUtxoDatum,
    poolPkh,
    basketTokenCounter
  ),
  BasketValidatorHash (MkBasketValidatorHash, getBasketValidatorHash),
  ExRate (MkExRate, getExRate),
  BasketToken (MkBasketToken, getBasketToken),
  Coin (..),
  PoolPubKeyHash (..),
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import PlutusLedgerApi.V1.Time (DiffMilliSeconds)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash,
  ScriptHash,
  TokenName,
 )

import PlutusTx qualified

newtype AdminPubKeyHash = MkAdminPubKeyHash
  { getAdminPubKeyHash :: PubKeyHash
  }
  deriving stock (Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype BasketStateCS = MkBasketStateCS
  {getBasketStateCS :: CurrencySymbol}
  deriving stock (Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype StakingPoolCS = MkStakingPoolCS
  {getStakingPoolCS :: CurrencySymbol}
  deriving stock (Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype BasketTokenCS = MkBasketTokenCS
  {getBasketTokenCS :: CurrencySymbol}
  deriving stock (Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

newtype BasketTokenTN = MkBasketTokenTN
  {getBasketTokenTN :: TokenName}
  deriving stock (Eq, Show)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

-- * Other types
newtype ExRate = MkExRate {getExRate :: Rational}
  deriving stock (GHC.Generic)
  deriving newtype (Num, Fractional, Show, Eq, Ord)

-- * Script parameters
data LockingParams = MkLockingParams
  { minLockInterval :: DiffMilliSeconds
  , maxLockDuration :: DiffMilliSeconds
  , maxTxValidityDuration :: DiffMilliSeconds
  }
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

-- FIXME: it is competely outdated
data BasketLock
  = Locked POSIXTime
  | Unlocked POSIXTime
  deriving stock (Show)

PlutusTx.makeLift ''BasketLock
PlutusTx.makeIsDataIndexed
  ''BasketLock
  [ ('Locked, 0)
  , ('Unlocked, 1)
  ]

PlutusTx.makeIsDataIndexed
  ''LockingParams
  [('MkLockingParams, 0)]

data BasketParams = MkBasketParams
  { voters :: [PubKeyHash]
  , lockingParams :: LockingParams
  , pledgeLockingParams :: Maybe LockingParams
  , basketStateCS :: BasketStateCS
  , basketTokenCS :: BasketTokenCS
  , stakingPoolCS :: StakingPoolCS
  , basketTokenTN :: BasketTokenTN
  }
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed
  ''BasketParams
  [('MkBasketParams, 0)]

data StakePoolUtxoDatum = MkStakePoolUtxoDatum
  { poolPkh :: Maybe PubKeyHash
  , basketTokenCounter :: Integer
  }
  deriving stock (GHC.Generic, Eq, Show)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed
  ''StakePoolUtxoDatum
  [('MkStakePoolUtxoDatum, 0)]

newtype PoolPubKeyHash = MkPoolPubKeyHash PubKeyHash
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (SOP.Generic)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
-- * Basket scripts

newtype BasketValidatorHash = MkBasketValidatorHash
  {getBasketValidatorHash :: ScriptHash}

newtype BasketToken = MkBasketToken {getBasketToken :: Integer}
  deriving stock (GHC.Generic, Eq, Ord, Show)
  deriving newtype (Num)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed
  ''BasketToken
  [('MkBasketToken, 0)]

newtype Coin = MkCoin Integer
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic)

PlutusTx.makeIsDataIndexed
  ''Coin
  [('MkCoin, 0)]
