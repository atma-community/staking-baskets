module Suites.Atma.Config (config) where

import Cardano.Ledger.Babbage.PParams (BabbagePParamsHKD (_protocolVersion))
import Cardano.Ledger.BaseTypes (ProtVer (ProtVer, pvMajor, pvMinor))
import Plutus.Model (
  MockConfig,
  defaultMockConfig,
 )
import Plutus.Model.Mock.ProtocolParameters (customBabbageParams)

config :: MockConfig
config =
  defaultMockConfig $
    customBabbageParams
      ( \ps ->
          ps
            { _protocolVersion =
                ProtVer {pvMajor = 7, pvMinor = 0}
            }
      )
