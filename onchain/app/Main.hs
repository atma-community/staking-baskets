module Main (main) where

import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS

import Data.Default (def)
import Data.Text qualified as Txt

import Plutarch (Config (tracingMode), TracingMode (DoTracing))
import PlutusLedgerApi.Common (serialiseUPLC)
import Ply (
  TypedScriptEnvelope (tsScript),
 )
import Ply.Core.Serialize.Script (serializeScriptCborHex)
import Ply.Plutarch (mkEnvelope)

import UntypedPlutusCore qualified as UPLC

import Atma.Scripts.BasketTokenMP (pMkBasketTokenMPUntyped)
import Atma.Scripts.BasketValidator (pMkBasketValidatorUntyped)
import Atma.Scripts.OneShotMintingPolicy (pMkOneShotMintingPolicyUntyped)
import Atma.Scripts.StakePoolTokenMP (pMkStakePoolTokenMPUntyped)

import Atma.Scripts.StakeValidator (pMkStakeValidatorUntyped)
import Atma.Scripts.UnspendableValidator (pMkUnspendableValidatorUntyped)
import Control.Applicative ((<|>))
import Data.ByteString (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative (
  Parser,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  progDesc,
  (<**>),
 )
import Ply.Plutarch.TypedWriter (TypedWriter)

writeScript :: ByteString -> TypedScriptEnvelope -> IO ()
writeScript scriptName envelope = do
  let program = tsScript envelope
      plutarchJson =
        object
          [ "cborHex" .= serialiseProgram program
          , "description" .= scriptDescription
          , "type" .= scriptType
          ]

      content = encodePretty plutarchJson

      scriptType :: Txt.Text
      scriptType = Txt.pack "PlutusScriptV2"

      scriptDescription :: Txt.Text
      scriptDescription = Txt.pack ""

  putStr "// Script size "
  print (UPLC.programSize program)

  LBS.putStr $
    "exports._" <> scriptName <> " = " <> content <> ";"
  where
    serialiseProgram =
      serializeScriptCborHex . serialiseUPLC

parseEnvelope :: forall {a}. Either Txt.Text a -> IO a
parseEnvelope = \case
  Left err -> fail $ Txt.unpack err
  Right envelope' -> pure envelope'

writeTermAsScript :: TypedWriter p => Txt.Text -> ClosedTerm p -> IO ()
writeTermAsScript text term = do
  envelope <-
    parseEnvelope $
      mkEnvelope (def {tracingMode = DoTracing}) text term

  writeScript
    (fromStrict $ encodeUtf8 text)
    envelope

writeOneShotMP :: IO ()
writeOneShotMP =
  writeTermAsScript
    "oneShotMintingPolicy"
    pMkOneShotMintingPolicyUntyped

writeBasketValidator :: IO ()
writeBasketValidator =
  writeTermAsScript
    "basketValidator"
    pMkBasketValidatorUntyped

writeStakePoolTokenMP :: IO ()
writeStakePoolTokenMP =
  writeTermAsScript
    "stakePoolTokenMP"
    pMkStakePoolTokenMPUntyped

writeBasketTokenMP :: IO ()
writeBasketTokenMP =
  writeTermAsScript
    "basketTokenMP"
    pMkBasketTokenMPUntyped

writeStakeValidator :: IO ()
writeStakeValidator =
  writeTermAsScript
    "stakeValidator"
    pMkStakeValidatorUntyped

writeUnspendableValidator :: IO ()
writeUnspendableValidator =
  writeTermAsScript
    "unspendableValidator"
    pMkUnspendableValidatorUntyped

data SerialiseArgument
  = BasketValidator
  | OneShotMP
  | StakePoolTokenMP
  | BasketTokenMP
  | StakeValidator
  | UnspendableValidator

serialiseScript :: SerialiseArgument -> IO ()
serialiseScript = \case
  BasketValidator -> writeBasketValidator
  OneShotMP -> writeOneShotMP
  StakePoolTokenMP -> writeStakePoolTokenMP
  BasketTokenMP -> writeBasketTokenMP
  StakeValidator -> writeStakeValidator
  UnspendableValidator -> writeUnspendableValidator

scriptArgument :: Parser SerialiseArgument
scriptArgument =
  flag'
    BasketValidator
    ( long "basketValidator"
        <> help "Atma basket validator"
    )
    <|> flag'
      OneShotMP
      ( long "oneShotMintingPolicy"
          <> help "One shot minting policy for platform initialisation"
      )
    <|> flag'
      StakePoolTokenMP
      ( long "stakePoolTokenMP"
          <> help "Stake pool token minting policy"
      )
    <|> flag'
      BasketTokenMP
      ( long "basketTokenMP"
          <> help "Basket token minting policy"
      )
    <|> flag'
      StakeValidator
      ( long "stakeValidator"
          <> help "Stake validator"
      )
    <|> flag'
      UnspendableValidator
      ( long "unspendableValidator"
          <> help "unspendableValidator"
      )

main :: IO ()
main = serialiseScript =<< execParser opts
  where
    opts =
      info
        (scriptArgument <**> helper)
        ( fullDesc
            <> progDesc "Serialises on-chain scripts to CTL compatible format."
            <> header "serialise - on-chain script serialiser"
        )
