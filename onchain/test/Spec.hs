module Main (main) where

import Atma.Scripts.BasketValidator (pMkBasketValidatorUntyped)
import Atma.Scripts.StakePoolTokenMP (pMkStakePoolTokenMP)
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (mkPlutarchTypedScript)
import Data.Either (either)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (plam, (#))
import Plutarch.Builtin (pdata, pforgetData)
import Suites.Atma.Common.PSM (tracingConfig)
import Suites.Atma.Common.Utils (Scripts (MkScripts))
import Suites.Atma.Serialization.PConstantDecl qualified as PConstantDecl
import Suites.Atma.Tests qualified as Tests
import Suites.Atma.Utils qualified as Utils
import System.Random qualified as Random
import Test.Tasty (TestTree, defaultMain, testGroup)
import Prelude (Either, IO, error, id, print, ($), (<$>), (<*>))

getScripts :: Either Text Scripts
getScripts =
  MkScripts
    <$> mkPlutarchTypedScript
      tracingConfig
      ( plam $
          \basketParams datum redeemer ctx ->
            pMkBasketValidatorUntyped
              # pforgetData (pdata basketParams)
              # datum
              # pforgetData (pdata redeemer)
              # ctx
      )
    <*> mkPlutarchTypedScript tracingConfig pMkStakePoolTokenMP

-- | @since 0.1
main :: IO ()
main = do
  gen <- Random.newStdGen
  print gen
  let scripts =
        either
          (error "Cannot generate scripts")
          id
          getScripts

  setLocaleEncoding utf8
  defaultMain $ tests gen scripts

{- | Project wide tests
 @since 0.1
-}
tests :: forall (g :: Type). Random.RandomGen g => g -> Scripts -> TestTree
tests gen s =
  testGroup
    "Atma"
    [ Tests.tests gen s
    , Utils.tests
    , PConstantDecl.tests
    ]
