{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Atma.Utils.ExRate (
  PBasketToken (MkPBasketToken),
  PCoin (MkPCoin),
  PExRate (MkPExRate),
  pbasketTokensToLovelace,
  plovelaceToBasketTokens,
) where

import Atma.Types (BasketToken (MkBasketToken), Coin (MkCoin))
import GHC.Generics qualified as GHC
import Plutarch.Lift (
  DerivePConstantViaNewtype (DerivePConstantViaNewtype),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Num (PNum)
import Plutarch.Positive (PPositive)
import Plutarch.Rational (pfromInteger, precip, ptruncate)
import Plutarch.Test.QuickCheck (
  PArbitrary (parbitrary, pshrink),
  TestableTerm (TestableTerm),
 )
import Test.QuickCheck (Gen)

type PExRate :: PType
newtype PExRate (s :: S)
  = MkPExRate (Term s PRational)
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, PNum, POrd, PShow)

instance DerivePlutusType PExRate where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PExRate)

instance PArbitrary PExRate where
  parbitrary :: Gen (TestableTerm PExRate)
  parbitrary = do
    (TestableTerm x) <- parbitrary @PPositive
    (TestableTerm y) <- parbitrary @PPositive
    pure $
      TestableTerm $
        pcon $
          MkPExRate $
            pcon (PRational (pto $ x + y) y)

  pshrink :: TestableTerm PExRate -> [TestableTerm PExRate]
  pshrink = const []

-- | Plutarch representation of Basket tokens.
type PBasketToken :: PType
newtype PBasketToken (s :: S)
  = MkPBasketToken (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PNum, PShow)

instance DerivePlutusType PBasketToken where
  type DPTStrat _ = PlutusTypeNewtype

deriving anyclass instance PTryFrom PData (PAsData PBasketToken)

instance PUnsafeLiftDecl PBasketToken where
  type PLifted PBasketToken = BasketToken

deriving via
  (DerivePConstantViaNewtype BasketToken PBasketToken PInteger)
  instance
    PConstantDecl BasketToken

instance PArbitrary PBasketToken where
  parbitrary :: Gen (TestableTerm PBasketToken)
  parbitrary = do
    (TestableTerm tn) <- parbitrary @PPositive
    pure $ TestableTerm $ pcon $ MkPBasketToken $ pto tn

  pshrink :: TestableTerm PBasketToken -> [TestableTerm PBasketToken]
  pshrink = const []

-- | Plutarch representation of Coin tokens.
type PCoin :: PType
newtype PCoin (s :: S)
  = MkPCoin (Term s PInteger)
  deriving stock (GHC.Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PNum, PShow)

instance PUnsafeLiftDecl PCoin where
  type PLifted PCoin = Coin

deriving via
  (DerivePConstantViaNewtype Coin PCoin PInteger)
  instance
    PConstantDecl Coin

instance DerivePlutusType PCoin where type DPTStrat _ = PlutusTypeNewtype

instance PArbitrary PCoin where
  parbitrary :: Gen (TestableTerm PCoin)
  parbitrary = do
    (TestableTerm tn) <- parbitrary @PPositive
    pure $ TestableTerm $ pcon $ MkPCoin $ pto tn

  pshrink :: TestableTerm PCoin -> [TestableTerm PCoin]
  pshrink = const []

pbasketTokensToLovelace :: ClosedTerm (PExRate :--> PBasketToken :--> PCoin)
pbasketTokensToLovelace = phoistAcyclic $
  plam $ \exRate btokens ->
    let btRat = pfromInteger #$ pto btokens
     in pcon $ MkPCoin $ ptruncate #$ btRat * pto exRate

plovelaceToBasketTokens :: ClosedTerm (PExRate :--> PCoin :--> PBasketToken)
plovelaceToBasketTokens = phoistAcyclic $
  plam $ \exRate lovelace ->
    let lRat = pfromInteger #$ pto lovelace
     in pcon $ MkPBasketToken $ ptruncate #$ lRat * (precip #$ pto exRate)
