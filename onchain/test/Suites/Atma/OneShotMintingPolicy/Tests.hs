module Suites.Atma.OneShotMintingPolicy.Tests (tests) where

import Test.Tasty qualified as Tasty

import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  TokenName,
 )

import Atma.Utils.QuickCheck (DistinctPair (DistinctPair))
import PlutusLedgerApi.V2.Tx (TxOutRef)
import Suites.Atma.OneShotMintingPolicy.Transactions (
  Params (amount, tokenName, txOutRef),
  mkContext,
  oneShotMintingPolicy,
  validates,
 )
import Test.QuickCheck (Positive (Positive, getPositive))
import Test.Tasty.QuickCheck (testProperty)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "One-shot minting policy"
    [ testProperty "Succeeds to mint the exact amount of new tokens" $
        \(params :: Params) ->
          let ctx = mkContext params
           in validates $ oneShotMintingPolicy params ctx
    , testProperty "Fails to mint if the referenced UTxO is not spend" $
        \(params :: Params, bogusTxOutRef :: TxOutRef) ->
          let ctx = mkContext params {txOutRef = bogusTxOutRef}
           in not $ validates $ oneShotMintingPolicy params ctx
    , testProperty "Fails to mint excess tokens of referenced name" $
        \(params :: Params, excess :: Positive Integer) ->
          let ctx = mkContext params
              -- NOTE: there's no Num instance for Positive Integer
              amount' =
                Positive $
                  getPositive params.amount + getPositive excess
           in not $
                validates $
                  oneShotMintingPolicy (params {amount = amount'}) ctx
    , testProperty "Fails to mint fewer tokens of referenced name" $
        \(params :: Params, excess :: Positive Integer) ->
          let ctx = mkContext params {amount = amount'}
              -- NOTE: there's no Num instance for Positive Integer
              amount' =
                Positive $
                  getPositive params.amount + getPositive excess
           in not $ validates $ oneShotMintingPolicy params ctx
    , testProperty "Fails to mint different tokens with unreferenced names" $
        \( params :: Params
          , DistinctPair (tokenName', bogusTokenName) :: DistinctPair TokenName
          ) ->
            let ctx = mkContext params {tokenName = bogusTokenName}
             in not $
                  validates $
                    oneShotMintingPolicy params {tokenName = tokenName'} ctx
    ]
