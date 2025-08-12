module Suites.Atma.Utils.ExRate (tests) where

import Plutarch.Rational (pfromInteger, precip, ptruncate)
import Plutarch.Test.QuickCheck (fromPFun)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck (testProperty)

import Atma.Utils.ExRate (
  PBasketToken,
  PCoin,
  PExRate (MkPExRate),
  pbasketTokensToLovelace,
  plovelaceToBasketTokens,
 )
import Plutarch.Positive (PPositive)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Exchange rate conversions"
    [ Tasty.testGroup
        "Round exchange rates"
        [ testProperty
            "Coin conversion roundtrip composes to identity"
            $ fromPFun pNatExRateCoinRoundtripProp
        , testProperty
            "Basket token conversion roundtrip composes to identity"
            $ fromPFun pNatExRateBasketTokenRoundtripProp
        ]
    , Tasty.testGroup
        "Arbitrary exchange rates"
        [ testProperty "plovelaceToBasketTokens is monotone" $
            fromPFun plovelaceToBasketTokensMonotoneProp
        , testProperty "pbasketTokensToLovelace is monotone" $
            fromPFun pbasketTokensToLovelaceMonotoneProp
        ]
    ]

ptoExRate :: ClosedTerm (PPositive :--> PExRate)
ptoExRate = plam $ \p -> pcon $ MkPExRate $ pfromInteger #$ pto p

ptoRecipExRate :: ClosedTerm (PPositive :--> PExRate)
ptoRecipExRate = plam $ \p ->
  pcon $ MkPExRate $ precip #$ pfromInteger #$ pto p

pBasketTokenRoundtrip ::
  ClosedTerm (PExRate :--> PBasketToken :--> PBasketToken)
pBasketTokenRoundtrip = plam $ \exRate basketTokens ->
  plovelaceToBasketTokens
    # exRate
    # (pbasketTokensToLovelace # exRate # basketTokens)

plovelaceRoundtrip ::
  ClosedTerm (PExRate :--> PCoin :--> PCoin)
plovelaceRoundtrip = plam $ \exRate lovelace ->
  pbasketTokensToLovelace
    # exRate
    # (plovelaceToBasketTokens # exRate # lovelace)

pNatExRateBasketTokenRoundtripProp ::
  ClosedTerm (PPositive :--> PBasketToken :--> PBool)
pNatExRateBasketTokenRoundtripProp = plam $ \exRateP basketTokens ->
  pBasketTokenRoundtrip
    # (ptoExRate # exRateP)
    # basketTokens
    #== basketTokens

pNatExRateCoinRoundtripProp ::
  ClosedTerm (PPositive :--> PCoin :--> PBool)
pNatExRateCoinRoundtripProp = plam $ \exRateP lovelace ->
  plovelaceRoundtrip
    # (ptoRecipExRate # exRateP)
    # lovelace
    #== lovelace

pfloorExRate :: ClosedTerm (PExRate :--> PExRate)
pfloorExRate = plam $ \exRate ->
  pcon $ MkPExRate $ pfromInteger #$ ptruncate #$ pto exRate

pceilExRate :: ClosedTerm (PExRate :--> PExRate)
pceilExRate = plam $ \exRate ->
  pcon $ MkPExRate $ pfromInteger #$ 1 + (ptruncate #$ pto exRate)

plovelaceToBasketTokensMonotoneProp ::
  ClosedTerm (PExRate :--> PCoin :--> PBool)
plovelaceToBasketTokensMonotoneProp = plam $ \exRate lovelace ->
  let getValue rate = plovelaceToBasketTokens # rate # lovelace
   in getValue (pceilExRate # exRate)
        #<= getValue exRate
        #&& getValue exRate
        #<= getValue (pfloorExRate # exRate)

pbasketTokensToLovelaceMonotoneProp ::
  ClosedTerm (PExRate :--> PBasketToken :--> PBool)
pbasketTokensToLovelaceMonotoneProp = plam $ \exRate basketToken ->
  let getValue rate = pbasketTokensToLovelace # rate # basketToken
   in getValue (pfloorExRate # exRate)
        #<= getValue exRate
        #&& getValue exRate
        #<= getValue (pceilExRate # exRate)
