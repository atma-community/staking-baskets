module Suites.Atma.OneShotMintingPolicy.Transactions (
  oneShotMintingPolicy,
  validates,
  mkMintingBuilder,
  mkContext,
  Params (MkParams, amount, tokenName, txOutRef),
) where

import Test.Tasty.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  Positive,
  getPositive,
 )

import Data.Default (def)
import Plutarch (
  Config (tracingMode),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V2 (PTokenName, PTxOutRef)
import Plutarch.Builtin (pforgetData)
import Plutarch.Context (
  MintingBuilder,
  buildMinting,
  input,
  mint,
  withMinting,
  withRef,
 )
import Plutarch.Evaluate (evalScript)
import Plutarch.Positive (PPositive, ptryPositive)
import PlutusLedgerApi.V2 (
  ScriptContext,
  TokenName,
  TxOutRef,
  singleton,
 )

import Atma.Utils.QuickCheck (
  NonAdaCurrencySymbol (getNonAdaCurrencySymbol),
 )

import Atma.Scripts.OneShotMintingPolicy (
  pMkOneShotMintingPolicy,
 )
import Plutarch.Test.QuickCheck.Instances ()

validates :: ClosedTerm s -> Bool
validates term =
  case compile (def {tracingMode = DoTracing}) term of
    Right script
      | (Right _, _, _) <- evalScript script -> True
    _ -> False

data Params = MkParams
  { txOutRef :: TxOutRef
  , amount :: Positive Integer
  , currencySymbol :: NonAdaCurrencySymbol
  , tokenName :: TokenName
  }
  deriving stock (Show)

instance Arbitrary Params where
  arbitrary :: Gen Params
  arbitrary =
    MkParams
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

mkMintingBuilder :: Params -> MintingBuilder
mkMintingBuilder params =
  mint (singleton cs params.tokenName n)
    <> input (withRef params.txOutRef)
    <> withMinting cs
  where
    cs = getNonAdaCurrencySymbol params.currencySymbol
    n = getPositive params.amount

mkContext :: Params -> ScriptContext
mkContext = buildMinting mempty . mkMintingBuilder

data PParams s = MkPParams
  { pTokenName :: Term s PTokenName
  , pAmount :: Term s PPositive
  , ptxOutRef :: Term s PTxOutRef
  }

mkPParams :: Params -> PParams s
mkPParams params =
  MkPParams
    { pTokenName = pconstant params.tokenName
    , pAmount = ptryPositive #$ pconstant $ getPositive params.amount
    , ptxOutRef = pconstant params.txOutRef
    }

oneShotMintingPolicy :: Params -> ScriptContext -> Term s POpaque
oneShotMintingPolicy params ctx =
  pMkOneShotMintingPolicy
    # pparams.pTokenName
    # pparams.pAmount
    # pparams.ptxOutRef
    # pforgetData (pdata $ pcon PUnit)
    # pconstant ctx
  where
    pparams = mkPParams params
