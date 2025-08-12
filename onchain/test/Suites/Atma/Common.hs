{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.Common (
  initializeBasketContract,
  waitFor,
  lovelaceValueOf,
  spendableCoin,
  mkPStakePoolUtxoDatum,
  incrementBasketTokenCounter,
  incrementNumberOfStakePoolUtxos,
  getLockState,
  floorPosixTime,
  Utxo (MkUtxo, unUtxo),
  stakePoolUtxosSummary,
) where

import Control.Monad (replicateM_, when)
import Control.Monad.Trans (lift)
import GHC.Base (coerce)
import Plutus.Model.Contract (getHeadRef)
import Plutus.Model.V2 (
  Ada,
  HasAddress (toAddress),
  adaValue,
  currentTime,
  loadRefScript,
  payToRef,
  refScriptAt,
  spendScriptRef,
  userSpend,
  waitNSlots,
 )

import Plutarch.Positive (ptryPositive)
import Plutarch.Test.QuickCheck.Instances ()

import PlutusLedgerApi.V2 (
  CurrencySymbol,
  POSIXTime,
  PubKeyHash (PubKeyHash),
  TokenName,
  TxOut,
  TxOutRef,
  Value,
  txOutValue,
 )

import PlutusLedgerApi.V2 qualified as V2

import Atma.PTypes (
  PAdminPubKeyHash,
  PBasketLock (MkPUnlocked),
  PBasketState (MkPBasketState),
  PBasketValidatorRedeemer (MkPStakePoolMPTriggerWitness),
  PStakePoolUtxoDatum (MkPStakePoolUtxoDatum),
 )
import Atma.Scripts.Common (
  basketStateTN,
  stakePoolTN,
 )
import Atma.Types (
  AdminPubKeyHash (MkAdminPubKeyHash),
  BasketLock,
  BasketParams (MkBasketParams, stakingPoolCS),
  BasketStateCS (MkBasketStateCS),
  BasketToken (MkBasketToken),
  basketStateCS,
  lockingParams,
 )
import Atma.Utils.ExRate (
  PExRate (MkPExRate),
 )

import Config (stakePoolCreationThreshold)
import Control.Arrow (Arrow ((&&&)))
import Control.Lens (view, _2)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Monoid (Sum (Sum))
import Plutarch.Extra.Maybe (pdnothing)
import Plutarch.Num ((#+))
import Plutus.Model (
  Ada (Lovelace),
  DatumMode (HashDatum, InlineDatum),
  Run,
  mintValue,
  payToKey,
  scriptCurrencySymbol,
  submitTx,
 )
import PlutusLedgerApi.V1.Value (valueOf)
import Suites.Atma.Common.BasketContext (
  BasketContext,
  Ctx (
    Ctx,
    basketParams,
    basketTokenMP,
    basketValidatorBasketStateUtxo,
    basketValidatorStakePoolUtxo,
    basketValidatorUtxo,
    stakingPoolMP
  ),
 )
import Suites.Atma.Common.PSM (
  PlutarchAdapter (PlutarchAdapter),
  getBasketValidatorBasketStateUtxo,
  getBasketValidatorStakePoolUtxo,
  getOneShotMP,
  getUnspendableValidator,
 )
import Suites.Atma.Common.Utils (
  Admin (Admin),
  InitParams (MkInitParams),
  NumOfStakePoolUtxos (NumOfStakePoolUtxos),
  Scripts (
    MkScripts,
    basketValidatorScript,
    stakingPoolMPScript
  ),
  basketParamsDeps,
  findBasketStateUtxo,
  getBasketParams,
  getCtx,
  minAdaTxOut,
  minAdaValue,
  minCoin,
  spend,
  _basketTokenCounter,
 )

newtype Utxo a = MkUtxo {unUtxo :: ((TxOutRef, TxOut), a)}

oneShotMPContract :: Admin -> TokenName -> Integer -> Run CurrencySymbol
oneShotMPContract (Admin adminPkh) tn amt = do
  let min = adaValue 1
  adminSpend <- spend (Just "OneShotMP admin min ADA") adminPkh min
  let headRef = getHeadRef adminSpend
  oneShotMP <-
    getOneShotMP
      (pconstant tn)
      (ptryPositive # pconstant amt)
      (pconstant headRef)

  let cs = scriptCurrencySymbol oneShotMP
      value = V2.singleton cs tn amt
      tx =
        mconcat
          [ userSpend adminSpend
          , mintValue oneShotMP () value
          , payToKey adminPkh $ value <> min
          ]

  submitTx adminPkh tx

  pure cs

mkPStakePoolUtxoDatum :: Integer -> ClosedTerm PStakePoolUtxoDatum
mkPStakePoolUtxoDatum basketTokenCounter =
  pcon $
    MkPStakePoolUtxoDatum $
      pdcons @"poolPkh"
        # pdata pdnothing
          #$ pdcons @"basketTokenCounter"
        # pdata (pconstant basketTokenCounter)
        # pdnil

incrementBasketTokenCounter ::
  ClosedTerm PStakePoolUtxoDatum ->
  Integer ->
  ClosedTerm PStakePoolUtxoDatum
incrementBasketTokenCounter stakePoolDatum inc =
  mkPStakePoolUtxoDatum $
    plift (pfield @"basketTokenCounter" # stakePoolDatum) + inc

mkPBasketState ::
  ClosedTerm PExRate ->
  ClosedTerm PInteger ->
  ClosedTerm PBasketLock ->
  ClosedTerm PBasketLock ->
  ClosedTerm PAdminPubKeyHash ->
  ClosedTerm PBasketState
mkPBasketState exRate numOfStakePoolUTxOs lock pledgeLock adminPkh =
  pcon $
    MkPBasketState $
      pdcons @"exRate"
        # pdata exRate
          #$ pdcons @"numOfStakePoolUTxOs"
        # pdata numOfStakePoolUTxOs
          #$ pdcons @"lock"
        # pdata lock
          #$ pdcons @"pledgeLock"
        # pdata pledgeLock
          #$ pdcons @"adminPkh"
        # pdata adminPkh
        # pdnil

incrementNumberOfStakePoolUtxos ::
  ClosedTerm PBasketState ->
  Integer ->
  ClosedTerm PBasketState
incrementNumberOfStakePoolUtxos basketState inc =
  mkPBasketState
    (pfield @"exRate" # basketState)
    updated
    (pfield @"lock" # basketState)
    (pfield @"pledgeLock" # basketState)
    (pfield @"adminPkh" # basketState)
  where
    updated :: Term s PInteger
    updated = pfromData (pfield @"numOfStakePoolUTxOs" # basketState) #+ pconstant inc

lockStakePools :: Int -> Ctx -> Run ()
lockStakePools numOfStakePools ctx@Ctx {..} = do
  (basketStateUtxo, PlutarchAdapter basketState) <- runReaderT findBasketStateUtxo ctx

  let adminPkh = plift $ pfromData $ pfield @"adminPkh" # pto basketState

  spendA <-
    spend
      (Just "SP user min ADA")
      (coerce adminPkh)
      (mconcat $ replicate numOfStakePools minAdaValue)

  let stakePoolCS = scriptCurrencySymbol stakingPoolMP
      stakePoolToken = V2.singleton stakePoolCS stakePoolTN 1

      basketStakePoolUtxoValidator = getBasketValidatorStakePoolUtxo basketValidatorBasketStateUtxo

      stakePoolsUTxOs =
        replicate numOfStakePools $
          payToRef
            basketStakePoolUtxoValidator
            (InlineDatum $ PlutarchAdapter $ mkPStakePoolUtxoDatum minAdaTxOut)
            (stakePoolToken <> minAdaValue)

      tx =
        mconcat
          [ mintValue
              stakingPoolMP
              ()
              (V2.singleton stakePoolCS stakePoolTN $ toInteger numOfStakePools)
          , spendScriptRef
              basketValidatorUtxo
              basketValidatorBasketStateUtxo
              (fst basketStateUtxo)
              (PlutarchAdapter $ pcon $ MkPStakePoolMPTriggerWitness pdnil)
              (PlutarchAdapter basketState)
          , payToRef
              basketValidatorBasketStateUtxo
              (HashDatum $ PlutarchAdapter $ incrementNumberOfStakePoolUtxos basketState (toInteger numOfStakePools))
              (txOutValue $ snd basketStateUtxo)
          , userSpend spendA
          ]
          <> mconcat stakePoolsUTxOs

  submitTx (coerce adminPkh) tx

lockBasketState :: Ctx -> AdminPubKeyHash -> Run ()
lockBasketState Ctx {basketParams = MkBasketParams {..}, basketValidatorBasketStateUtxo} adminPkh = do
  let initialExRate :: ClosedTerm PExRate
      initialExRate =
        pcon . MkPExRate . pcon $
          PRational (pconstant 1) (ptryPositive # pconstant 1)

      initialBaksetState :: ClosedTerm PBasketState
      initialBaksetState =
        mkPBasketState
          initialExRate
          0
          (pcon $ MkPUnlocked $ pdcons # pdata 0 # pdnil)
          (pcon $ MkPUnlocked $ pdcons # pdata 0 # pdnil)
          (pconstant adminPkh)

  let value =
        V2.singleton (coerce basketStateCS) basketStateTN 1
          <> adaValue 10
  adminSpend <-
    spend
      (Just "admin basketState when locking")
      (coerce adminPkh)
      value

  let tx =
        mconcat
          [ userSpend adminSpend
          , payToRef
              basketValidatorBasketStateUtxo
              (HashDatum $ PlutarchAdapter initialBaksetState)
              value
          ]

  submitTx (coerce adminPkh) tx

findBasketValidatorUtxo :: BasketStateCS -> Run TxOutRef
findBasketValidatorUtxo basketStateCs = do
  unspendableValidator <- getUnspendableValidator basketStateCs
  (basketUtxo, _) : _ <- refScriptAt unspendableValidator
  pure basketUtxo

lockBasketValidatorUtxo :: Admin -> Scripts -> BasketParams -> Run ()
lockBasketValidatorUtxo admin MkScripts {basketValidatorScript} params = do
  let basketStateCs = basketStateCS params
  unspendableValidator <- getUnspendableValidator basketStateCs
  basketValidator <-
    getBasketValidatorBasketStateUtxo basketValidatorScript (pconstant params)
  let minAda = adaValue minAdaTxOut
  sp <- spend Nothing (coerce admin) minAda
  submitTx (coerce admin) $
    mconcat
      [ loadRefScript
          basketValidator
          (toAddress unspendableValidator)
          minAda
      , userSpend sp
      ]

initializeBasketContract :: InitParams -> Scripts -> Run Ctx
initializeBasketContract initParams@(MkInitParams _ admin _ _ numStakePoolUtxos) scripts = do
  basketStateCS <-
    MkBasketStateCS <$> oneShotMPContract (coerce admin) basketStateTN 1

  let paramsDeps = basketParamsDeps initParams basketStateCS

  basketParams <- getBasketParams paramsDeps (stakingPoolMPScript scripts)

  lockBasketValidatorUtxo (coerce admin) scripts basketParams
  basketValidatorUtxo <- findBasketValidatorUtxo basketStateCS

  ctx <- getCtx paramsDeps basketValidatorUtxo scripts

  lockBasketState ctx (coerce admin)

  lockStakePools (coerce numStakePoolUtxos `mod` stakePoolCreationThreshold) ctx

  replicateM_
    (coerce numStakePoolUtxos `div` stakePoolCreationThreshold)
    (lockStakePools stakePoolCreationThreshold ctx)

  pure ctx

slotLen :: POSIXTime
slotLen = 1_000

floorPosixTime :: POSIXTime -> POSIXTime
floorPosixTime t =
  let slots = t `div` slotLen
   in slots * slotLen

getLockState :: BasketContext BasketLock
getLockState = do
  (_, PlutarchAdapter pbasketState) <- findBasketStateUtxo
  pure $ plift $ pfromData $ pfield @"lock" # pbasketState

waitFor :: POSIXTime -> BasketContext POSIXTime
waitFor time = do
  nowTime <- lift currentTime
  let duration = time - nowTime
  when (duration > 0) $
    lift $
      waitNSlots $
        fromInteger (coerce $ duration `div` slotLen) + 1

  lift currentTime

lovelaceValueOf :: Value -> Integer
lovelaceValueOf v = valueOf v V2.adaSymbol V2.adaToken

stakePoolUtxosSummary ::
  forall (f :: Type -> Type).
  Foldable f =>
  f (Utxo (PlutarchAdapter PStakePoolUtxoDatum)) ->
  (Ada, BasketToken)
stakePoolUtxosSummary =
  coerce . foldMap (Sum . cointAmount &&& Sum . btAmount)
  where
    btAmount = MkBasketToken . view (_2 . _basketTokenCounter) . unUtxo
    cointAmount =
      Lovelace . lovelaceValueOf . txOutValue . snd . fst . unUtxo

-- | A value without min ADA
spendableCoin :: Value -> Ada
spendableCoin v = Lovelace $ lovelaceValueOf v - plift (pto minCoin)
