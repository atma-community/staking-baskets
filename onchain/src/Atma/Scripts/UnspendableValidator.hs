{-# LANGUAGE BlockArguments #-}

module Atma.Scripts.UnspendableValidator (
  pMkUnspendableValidator,
  pMkUnspendableValidatorUntyped,
  unspendableValidatorScript,
) where

import Data.Default (def)
import Flat.Types (Text)

import Plutarch (
  Config (tracingMode),
  Script,
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Api.V2.Contexts (PScriptContext)

import Atma.PTypes (
  PBasketStateCS,
 )
import Atma.Utils (
  ptryFromUndata,
 )

{- | `pMkUnspendableValidator` is a validator that always fails.

      It means that a user will be able to create a utxo
      at the corresponding address but this utxos will be
      unspendable. Nobody will be able to spend it.
      However such utxo can still be used as a
      reference input for a transaction and provide data
      carried by it (datum or a script).

      Unspendable validator is used in Atma
      for uploading main basket validator script.
      Then basket validator script is provided to transactions
      that need it for validation by the means of
      reference script mechanism (CIP-33).

      Unspendable validator is parametrized by `PBasketStateCS`.
      By doing this we get per basket permanent storage that
      is used to store basket validator script.
-}
pMkUnspendableValidator ::
  ClosedTerm
    ( PBasketStateCS
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkUnspendableValidator = plam $ \_ _ _ _ -> perror

pMkUnspendableValidatorUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PData
        :--> PScriptContext
        :--> POpaque
    )
pMkUnspendableValidatorUntyped = plam $ \basketStateCS datum redeemer ->
  pMkUnspendableValidator
    # unTermCont (ptryFromUndata basketStateCS)
    # unTermCont (ptryFromUndata datum)
    # unTermCont (ptryFromUndata redeemer)

unspendableValidatorScript :: Either Text Script
unspendableValidatorScript =
  compile (def {tracingMode = DoTracing}) pMkUnspendableValidatorUntyped
