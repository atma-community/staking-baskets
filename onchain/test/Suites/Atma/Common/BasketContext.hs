{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.Common.BasketContext (
  Ctx (
    Ctx,
    basketParams,
    basketValidatorBasketStateUtxo,
    basketValidatorUtxo,
    basketValidatorStakePoolUtxo,
    basketTokenMP,
    stakingPoolMP
  ),
  BasketContext,
) where

import PlutusLedgerApi.V1 (
  CurrencySymbol,
  ScriptHash,
  TxOutRef,
 )

import Atma.Types (BasketParams)
import Control.Monad.Reader (ReaderT)
import Plutus.Model (Run, scriptCurrencySymbol, scriptHash)
import Suites.Atma.Common.PSM (
  BasketTokenMP,
  BasketValidatorBasketStateUtxo,
  BasketValidatorStakePoolUtxo,
  StakePoolTokenMP,
 )

data Ctx = Ctx
  { basketParams :: BasketParams
  , basketValidatorBasketStateUtxo :: BasketValidatorBasketStateUtxo
  , basketValidatorUtxo :: TxOutRef
  , basketValidatorStakePoolUtxo :: BasketValidatorStakePoolUtxo
  , basketTokenMP :: BasketTokenMP
  , stakingPoolMP :: StakePoolTokenMP
  }

type BasketContext a = ReaderT Ctx Run a

-- | The internal datatype for debug-printing of 'Ctx'
data ShowCtx = MkShowCtx
  { sbasketParams :: BasketParams
  , sbasketValidatorBasketStateUtxo :: ScriptHash
  , sbasketValidatorUtxo :: TxOutRef
  , sbasketValidatorStakePoolUtxo :: ScriptHash
  , sbasketTokenMP :: CurrencySymbol
  , sstakingPoolMP :: CurrencySymbol
  }
  deriving stock (Show)

instance Show Ctx where
  show Ctx {..} =
    show
      MkShowCtx
        { sbasketParams = basketParams
        , sbasketValidatorBasketStateUtxo =
            scriptHash basketValidatorBasketStateUtxo
        , sbasketValidatorUtxo =
            basketValidatorUtxo
        , sbasketValidatorStakePoolUtxo =
            scriptHash basketValidatorStakePoolUtxo
        , sbasketTokenMP =
            scriptCurrencySymbol basketTokenMP
        , sstakingPoolMP =
            scriptCurrencySymbol stakingPoolMP
        }
