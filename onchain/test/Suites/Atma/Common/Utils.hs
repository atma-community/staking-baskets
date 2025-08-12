{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Suites.Atma.Common.Utils (
  tryFromMaybe,
  Admin (Admin),
  User (User),
  InitParams (
    MkInitParams,
    initParamsVoters,
    initParamsAdminPkh,
    initParamsBasketLocking,
    initParamsPledgeLocking,
    initParamsNStakePools
  ),
  Scripts (MkScripts, basketValidatorScript, stakingPoolMPScript),
  BasketParamsDeps (BasketParamsDeps),
  NumOfStakePoolUtxos (NumOfStakePoolUtxos),
  getBasketParams,
  getCtx,
  findBasketStateUtxo,
  findStakePoolUtxos,
  hasSingleToken,
  hasBasketStateToken,
  hasStakingPoolToken,
  basketParamsDeps,
  debug,
  minAdaValue,
  checkFailWithMsg,
  ErrorMsgContains (ErrorMsgContains),
  shuffle,
  randomDistribution,
  spend,
  basketTokenTN,
  _basketTokenCounter,
  _poolPkh,
  _exRate,
  _numOfStakePoolUTxOs,
  pexRateL,
  minAdaTxOut,
  minCoin,
) where

import Atma.PTypes (PBasketParams, PBasketState, PBasketStateCS, PStakePoolUtxoDatum, pbasketState)
import Atma.Scripts.Common (basketStateTN, stakePoolTN)
import Atma.Types (
  BasketParams (MkBasketParams, basketStateCS, stakingPoolCS),
  BasketStateCS (MkBasketStateCS),
  BasketTokenCS (MkBasketTokenCS),
  BasketTokenTN (MkBasketTokenTN),
  ExRate (MkExRate),
  LockingParams,
  StakePoolUtxoDatum (basketTokenCounter),
  StakingPoolCS (MkStakingPoolCS),
  poolPkh,
 )
import Atma.Utils.ExRate (PCoin (MkPCoin), PExRate (MkPExRate))
import Cardano.Simple.PlutusLedgerApi.V1.Scripts (PlutarchTypedScript)
import Control.Lens (Lens', lens)
import Control.Monad (unless, (>=>))
import Control.Monad.Cont (lift)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.State.Strict (MonadState (get, put))
import Data.Coerce (coerce)
import Data.Foldable (Foldable (fold))
import Data.Foldable qualified as L
import Data.Function (on)
import Data.List (isInfixOf, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Ratio qualified as R
import Data.Traversable (for)
import Plutarch.Api.V2 (PMintingPolicy, PValidator)
import Plutarch.Positive (PPositive)
import Plutarch.Rational (PFractional ((#/)), pfromInteger)
import Plutus.Model (
  FailReason (GenericFail),
  Log (Log),
  MockConfig,
  Run,
  datumAt,
  getFails,
  logError,
  mustFailLog,
  spend',
  testNoErrors,
 )
import Plutus.Model.Contract (UserSpend, utxoAt)
import Plutus.Model.V2 (scriptCurrencySymbol)
import PlutusLedgerApi.V1 (TokenName)
import PlutusLedgerApi.V1.Value (Value, tokenName, valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  PubKeyHash,
  txOutValue,
 )
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V2.Tx (TxOut, TxOutRef)
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
  getBasketTokenMP,
  getBasketValidatorBasketStateUtxo,
  getBasketValidatorStakePoolUtxo,
  getStakePoolTokenMP,
 )
import System.Random qualified as Random
import Test.Tasty (TestTree)

newtype Admin = Admin PubKeyHash
  deriving newtype (Eq)
newtype User = User PubKeyHash

newtype NumOfStakePoolUtxos = NumOfStakePoolUtxos Int
  deriving newtype (Num, Integral, Real, Eq, Ord, Enum)

data InitParams = MkInitParams
  { initParamsVoters :: [PubKeyHash]
  , initParamsAdminPkh :: Admin
  , initParamsBasketLocking :: LockingParams
  , initParamsPledgeLocking :: Maybe LockingParams
  , initParamsNStakePools :: NumOfStakePoolUtxos
  }

data Scripts = MkScripts
  { basketValidatorScript :: PlutarchTypedScript (PBasketParams :--> PValidator)
  , stakingPoolMPScript :: PlutarchTypedScript (PBasketStateCS :--> PMintingPolicy)
  }

data BasketParamsDeps
  = BasketParamsDeps
      BasketStateCS
      [PubKeyHash]
      LockingParams
      (Maybe LockingParams)

_exRate :: Lens' (PlutarchAdapter PBasketState) ExRate
_exRate = pbasketExRateL . pexRateL

pbasketExRateL :: Lens' (PlutarchAdapter PBasketState) (PlutarchAdapter PExRate)
pbasketExRateL =
  lens
    (\(PlutarchAdapter pstate) -> PlutarchAdapter $ pfield @"exRate" # pstate)
    ( \(PlutarchAdapter pstate) (PlutarchAdapter pexRate) ->
        PlutarchAdapter $
          pbasketState
            # pexRate
            # (pfield @"numOfStakePoolUTxOs" # pstate)
            # (pfield @"lock" # pstate)
            # (pfield @"pledgeLock" # pstate)
            # (pfield @"adminPkh" # pstate)
    )

_numOfStakePoolUTxOs :: Lens' (PlutarchAdapter PBasketState) (PlutarchAdapter PInteger)
_numOfStakePoolUTxOs =
  lens
    ( \(PlutarchAdapter pstate) ->
        PlutarchAdapter $ pfield @"numOfStakePoolUTxOs" # pstate
    )
    ( \(PlutarchAdapter pstate) (PlutarchAdapter n) ->
        PlutarchAdapter $
          pbasketState
            # (pfield @"exRate" # pstate)
            # n
            # (pfield @"lock" # pstate)
            # (pfield @"pledgeLock" # pstate)
            # (pfield @"adminPkh" # pstate)
    )

pexRateL :: Lens' (PlutarchAdapter PExRate) ExRate
pexRateL = lens get set
  where
    get (PlutarchAdapter pexRate) =
      let numerator :: ClosedTerm PInteger
          numerator = pnumerator # pto pexRate
          denominator :: ClosedTerm PPositive
          denominator = pdenominator # pto pexRate
       in MkExRate $ plift numerator R.% plift (pto denominator)
    set _ (MkExRate exRate) =
      PlutarchAdapter $
        pcon $
          MkPExRate $
            (pfromInteger # pconstant (R.numerator exRate))
              #/ (pfromInteger # pconstant (R.denominator exRate))

_poolPkh :: Lens' (PlutarchAdapter PStakePoolUtxoDatum) (Maybe PubKeyHash)
_poolPkh =
  lens
    (\(PlutarchAdapter datum) -> poolPkh $ plift datum)
    ( \(PlutarchAdapter datum) newPkh ->
        PlutarchAdapter $ pconstant $ (plift datum) {poolPkh = newPkh}
    )

_basketTokenCounter :: Lens' (PlutarchAdapter PStakePoolUtxoDatum) Integer
_basketTokenCounter =
  lens
    (\(PlutarchAdapter datum) -> basketTokenCounter $ plift datum)
    ( \(PlutarchAdapter datum) newCounter ->
        PlutarchAdapter $ pconstant $ (plift datum) {basketTokenCounter = newCounter}
    )

basketTokenTN :: BasketTokenTN
basketTokenTN = MkBasketTokenTN $ tokenName "BasketToken"

shuffle ::
  forall (a :: Type) (g :: Type).
  Random.RandomGen g =>
  g ->
  [a] ->
  [a]
shuffle gen xs =
  fmap fst
    . sortBy (compare `on` snd)
    $ zip xs (Random.randoms @Int gen)

getBasketParams ::
  BasketParamsDeps ->
  PlutarchTypedScript (PBasketStateCS :--> PMintingPolicy) ->
  Run BasketParams
getBasketParams
  ( BasketParamsDeps
      basketStateCS
      voters
      basketLockingParams
      pledgeLockingParams
    )
  scripts = do
    stakePoolTokenCS <-
      MkStakingPoolCS . scriptCurrencySymbol
        <$> getStakePoolTokenMP
          scripts
          (pconstant basketStateCS)

    basketTokenCS <-
      MkBasketTokenCS . scriptCurrencySymbol
        <$> getBasketTokenMP (pconstant stakePoolTokenCS)

    pure $
      MkBasketParams
        voters
        basketLockingParams
        pledgeLockingParams
        basketStateCS
        basketTokenCS
        stakePoolTokenCS
        basketTokenTN

basketParamsDeps :: InitParams -> BasketStateCS -> BasketParamsDeps
basketParamsDeps
  (MkInitParams voters _ basketLockingParams pledgeLockingParams _)
  basketStateCS =
    BasketParamsDeps basketStateCS voters basketLockingParams pledgeLockingParams

getCtx :: BasketParamsDeps -> TxOutRef -> Scripts -> Run Ctx
getCtx
  deps@(BasketParamsDeps basketStateCS _ _ _)
  basketValidatorUtxo
  scripts = do
    basketParams <- getBasketParams deps (stakingPoolMPScript scripts)

    basketValidatorBasketStateUtxo <-
      getBasketValidatorBasketStateUtxo
        (basketValidatorScript scripts)
        $ pconstant basketParams

    let basketValidatorStakePoolUtxo =
          getBasketValidatorStakePoolUtxo basketValidatorBasketStateUtxo

    stakingPoolMP <-
      getStakePoolTokenMP
        (stakingPoolMPScript scripts)
        (pconstant basketStateCS)

    basketTokenMP <-
      getBasketTokenMP $
        pconstant $
          MkStakingPoolCS $
            scriptCurrencySymbol stakingPoolMP

    pure $ Ctx {..}

tryFromMaybe :: forall (a :: Type). String -> Maybe a -> Run a
tryFromMaybe msg = maybe (fail msg) pure

findBasketStateUtxo :: BasketContext ((TxOutRef, TxOut), PlutarchAdapter PBasketState)
findBasketStateUtxo = do
  ctx <- ask
  let cs = basketStateCS $ basketParams ctx
      validator = basketValidatorBasketStateUtxo ctx

  utxo <-
    lift $
      tryFromMaybe "Script has not received the BasketState UTxO"
        . L.find (hasBasketStateToken cs)
        =<< utxoAt validator

  basketState <-
    lift $
      tryFromMaybe "Can't parse BasketState"
        =<< datumAt @(PlutarchAdapter PBasketState) (fst utxo)

  pure (utxo, basketState)

debug :: BasketContext String
debug = do
  ctx <- ask
  basketBalance <- lift $ utxoAt $ basketValidatorStakePoolUtxo ctx
  pure $
    "BasketValidator balance: "
      <> show basketBalance
      <> ";\n"
      <> show ctx

findStakePoolUtxos :: BasketContext (NE.NonEmpty ((TxOutRef, TxOut), PlutarchAdapter PStakePoolUtxoDatum))
findStakePoolUtxos = do
  ctx <- ask
  let cs = stakingPoolCS $ basketParams ctx
      validator = basketValidatorStakePoolUtxo ctx

  utxos <-
    lift $
      tryFromMaybe "Script has not received the StakePool UTxO"
        . NE.nonEmpty
        . filter (hasStakingPoolToken cs)
        =<< utxoAt validator

  datums <-
    lift $
      for
        utxos
        (datumAt @(PlutarchAdapter PStakePoolUtxoDatum) . fst >=> tryFromMaybe "Can't parse StakePoolDatum")

  pure $ NE.zip utxos datums

minAdaTxOut :: Integer
minAdaTxOut = 2_000_000

minCoin :: ClosedTerm PCoin
minCoin = pcon $ MkPCoin $ pconstant minAdaTxOut

minAdaValue :: Value
minAdaValue =
  V2.singleton V2.adaSymbol V2.adaToken $ plift $ pto minCoin

hasSingleToken :: CurrencySymbol -> TokenName -> TxOut -> Bool
hasSingleToken cs tn txOut = valueOf (txOutValue txOut) cs tn == 1

hasBasketStateToken ::
  forall (a :: Type). BasketStateCS -> (a, TxOut) -> Bool
hasBasketStateToken cs (_, txOut) =
  hasSingleToken (coerce cs) basketStateTN txOut

hasStakingPoolToken ::
  forall (a :: Type). StakingPoolCS -> (a, TxOut) -> Bool
hasStakingPoolToken cs (_, txOut) =
  hasSingleToken (coerce cs) stakePoolTN txOut

{- | Temporary replacement for broken PSM's `spend`.
  See https://github.com/mlabs-haskell/plutus-simple-model/issues/105
-}
spend :: Maybe String -> V2.PubKeyHash -> V2.Value -> Run UserSpend
spend msg pkh val = do
  mSp <- spend' pkh val
  pure $ fromMaybe (error $ "Failed to spend. " <> fold msg) mSp

randomDistribution ::
  forall (a :: Type) (g :: Type).
  Random.RandomGen g =>
  Random.Random a =>
  g ->
  (a, a) ->
  Int ->
  (NonEmpty a, g)
randomDistribution gen bounds ofSize =
  let (distr, gens) = unzip $ take (pred ofSize) rest
      (x0, gen0) :| rest =
        NE.iterate
          (Random.randomR bounds . snd)
          (Random.randomR bounds gen)
   in (x0 :| distr, NE.last $ gen0 :| gens)

newtype ErrorMsgContains = ErrorMsgContains String

checkFailWithMsg ::
  MockConfig -> Value -> String -> ErrorMsgContains -> Run () -> TestTree
checkFailWithMsg cfg funds msg (ErrorMsgContains errorMsg) act =
  testNoErrors funds cfg msg run
  where
    run :: Run ()
    run = do
      st <- get
      act
      Log errs <- getFails
      put st {mustFailLog = Log mempty}
      unless
        ( any
            ( \case
                (_, GenericFail err) -> errorMsg `isInfixOf` err
                _ -> False
            )
            errs
        )
        ( logError $
            "Test case errors didn't contain any error containing: \""
              <> errorMsg
              <> "\" All errors: "
              <> show errs
        )
