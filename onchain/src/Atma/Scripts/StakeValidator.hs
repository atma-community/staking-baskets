module Atma.Scripts.StakeValidator (
  pMkStakeValidator,
  pMkStakeValidatorUntyped,
  stakeValidatorScript,
) where

import Data.Default (def)
import Flat.Types (Text)

import Atma.PTypes (PAdminPubKeyHash, PBasketStateCS)
import Atma.Utils (
  perrorIfFalse,
  psignedBy,
  ptryFromUndata,
 )

import Plutarch (
  Script,
  TracingMode (DoTracing),
  compile,
  tracingMode,
 )
import Plutarch.Api.V1.DCert (
  PDCert (
    PDCertDelegDeRegKey,
    PDCertDelegDelegate,
    PDCertDelegRegKey
  ),
 )
import Plutarch.Api.V2 (PPubKeyHash, PStakeValidator)
import Plutarch.Api.V2.Contexts (
  PScriptPurpose (
    PCertifying,
    PRewarding
  ),
 )
import Plutarch.Monadic qualified as P

{- | `pMkStakeValidator` is a stake validator

      Parameters:
      - `PBasketStateCS` - although it is not used in the body of the validator
        it's necessary to parametrize stake validator over it. It's to ensure that
        stake validators are not shared across different baskets. Just to remind:
        `PBasketStateCS` uniquely identifies a basket.
      - `PPubKeyHash` - pub key hash of a stake pool
        a stake validator will be delegated to.
      - `PAdminPubKeyHash` - pub key hash of the basket administrator

      Parametrization over (PBasketStateCS, PPubKeyHash) is necessary to
      make stake validator unique for a basket and for a stake pool.

      Stake validator is used when assigning staking credential to stake pool utxos
      in Rebalancing period by the administrator.

      The common check is that the transaction must be signed by
      the administrator.

      There are no additional checks when validating reward extraction.
      It means that the administrator can do whatever he wants with the rewards.
      It's a matter of trust and reputation of the basket administrator to
      put the rewards (with `MkPDonate`) back into some stake pool utxo.

      There are not additional checks for registration and deregistration.

      When validating delegation it is verified that stake validator is delegated
      to a stake pool that the stake validator is parametrized by. It's a kind of
      a consistency check.
-}
pMkStakeValidator ::
  ClosedTerm
    ( PBasketStateCS
        :--> PPubKeyHash
        :--> PAdminPubKeyHash
        :--> PStakeValidator
    )
pMkStakeValidator = phoistAcyclic $
  plam $
    \_ poolPkh adminPkh _ ctx' -> P.do
      ctx <- pletFields @'["purpose", "txInfo"] ctx'
      let signatories = pfield @"signatories" # ctx.txInfo

      perrorIfFalse
        "Not signed by an administrator"
        (psignedBy # pto adminPkh # signatories)

      let pSuccess = popaque $ pconstant ()

      pmatch ctx.purpose $ \case
        PRewarding _ -> pSuccess
        PCertifying dCert' -> pmatch (pfield @"_0" # dCert') $ \case
          PDCertDelegRegKey _ -> pSuccess
          PDCertDelegDeRegKey _ -> pSuccess
          PDCertDelegDelegate delegate -> P.do
            perrorIfFalse
              "Not delagating to an expected stake pool"
              ((pfield @"_1" # delegate) #== poolPkh)
            pSuccess
          _ -> ptraceError "Not a valid DCert"
        _ -> ptraceError "Not a valid script purpose"

pMkStakeValidatorUntyped ::
  ClosedTerm
    ( PData
        :--> PData
        :--> PData
        :--> PStakeValidator
    )
pMkStakeValidatorUntyped = plam $ \basketStateCS poolPkh adminPkh ->
  pMkStakeValidator
    # unTermCont (ptryFromUndata basketStateCS)
    # unTermCont (ptryFromUndata poolPkh)
    # unTermCont (ptryFromUndata adminPkh)

stakeValidatorScript :: Either Text Script
stakeValidatorScript =
  compile (def {tracingMode = DoTracing}) pMkStakeValidatorUntyped
