module Atma.Utils.QuickCheck (
  NonAdaCurrencySymbol (..),
  DistinctPair (..),
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  toBuiltin,
 )

import Test.QuickCheck (
  Arbitrary (arbitrary),
  Gen,
  choose,
  vectorOf,
 )
import Test.QuickCheck.Gen (suchThat)
import Test.QuickCheck.Instances ()

{- | This will never generate the CurrencySymbol for Ada
 Ada has an `emptybytestring` as it's currency symbol
-}
newtype NonAdaCurrencySymbol = NonAdaCurrencySymbol
  { getNonAdaCurrencySymbol :: CurrencySymbol
  }
  deriving newtype (Eq, Show, Ord)

instance Arbitrary NonAdaCurrencySymbol where
  arbitrary =
    NonAdaCurrencySymbol . CurrencySymbol . toBuiltin <$> genByteString 28

newtype DistinctPair a = DistinctPair (a, a)
  deriving newtype (Eq, Show)

instance (Arbitrary a, Eq a) => Arbitrary (DistinctPair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    pure $ DistinctPair (a, b)

genByteString :: Int -> Gen ByteString
genByteString n = BS.pack <$> vectorOf n (choose (0, 255))
