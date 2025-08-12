module Suites.Atma.Serialization.PConstantDecl (tests) where

import Atma.PTypes (PBasketParams)
import Plutarch.Builtin (pforgetData)
import Plutarch.Lift (PUnsafeLiftDecl (PLifted))
import Plutarch.Test.QuickCheck (PArbitrary (parbitrary), TestableTerm (TestableTerm))
import PlutusLedgerApi.V2 (BuiltinData (BuiltinData), UnsafeFromData (unsafeFromBuiltinData))
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck (testProperty)

roundTrip ::
  forall {a :: PType}.
  (PIsData a, PUnsafeLiftDecl a, UnsafeFromData (PLifted a)) =>
  ClosedTerm a ->
  ClosedTerm a
roundTrip t =
  pconstant $
    unsafeFromBuiltinData @(PLifted a) $
      BuiltinData $
        plift $
          pforgetData $
            pdata t

checkRoundTrip ::
  ( PIsData a
  , PUnsafeLiftDecl a
  , UnsafeFromData (PLifted a)
  , PEq a
  ) =>
  ClosedTerm a ->
  Bool
checkRoundTrip a = plift $ a #== roundTrip a

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "PConstantDecl haskell-plutarch roundtrip"
    [ testProperty "PBasketParams" $ do
        TestableTerm x <- parbitrary @PBasketParams
        pure $ checkRoundTrip x
    ]
