# Basket Bot

### Bot Configuration

```json
{
  // Path to a file containing parameters related to the staking basket.
  "basketParamsDeps": "./config/test-basket-params.json",

  // Configuration for the main functionality of the bot.
  "botConfig": {
    // Configurations for rebalancing the staking basket.
    "rebalancingCfg": {
      // Path to a file specifying the desired distribution.
      "desiredDistribution": "./config/distribution.json",
      // A boolean indicating whether to preprocess the distribution.
      "preprocessDistribution": true
    },

    // Configurations for withdrawing staking rewards.
    "rewardWithdrawalCfg": {
      // Path to a file containing the admin address where fees will be deposited.
      "rewardeeAddress": "./config/bot-payment.addr",
      // Percentage of fees deducted before depositing rewards.
      "rewardeeFees": 10.0,
      // The threshold for rewards; rewards are withdrawn if they exceed this value.
      "rewardsThreshold": 240000
    },

    // Retry policy configurations.
    "retryPolicy": {
      // A boolean indicating whether to use a Plutus retry policy.
      "usePlutusRetryPolicy": false
    }
  },

  // Specifications for the wallet used by the bot.
  "botWallet": {
    // Specifies the source and type of wallet (private keys or mnemonic phrase).
    "source-type": "private-keys",
    
    // Depending on the source type:
    // For private keys:
    "source": {
      // Path to the file containing the stake key.
      "stakeKeyFile": "./config/bot-stake.skey",
      // Path to the file containing the payment key.
      "paymentKeyFile": "./config/bot-payment.skey"
    }
    
    // NOTE: The bot supports mnemonic phrases as well.
    // For mnemonic:
    // "source-type": "mnemonic",
    // "source": {
    //   // Index of the account.
    //   "accountIndex": 0,
    //   // Index of the address.
    //   "addressIndex": 0,
    //   // Path to the mnemonic file.
    //   "path": "./config/bot-mnemonic",
    //   // A boolean indicating whether to include the stake key.
    //   "withStakeKey": true
    // }
  }
}
```

### Basket Params configuration:

```json
{
  "userAddr": "addr_test1qq9g5wk4td5d8l7r4tq7gcxt2fgywyu4xv3ecaw7hueqwava6ss9j6pt4dhp25aq9358q890jddzku6jt72f33e30lkqc6mn8p",
  "lockingParams": {
    "maxLockDuration": "0",
    "minLockInterval": "0",
    "maxTxValidityDuration": "0"
  },
  // Pledge locking params can be null as well, indicating that the pledge is not a pledge basket.
  "pledgeLockingParams": null,
  "basketTokenTN": "TestBasketToken",
  "basketStateCS": "ba9f4c0f3cefacfa4a893feae7b0a688b9d7f1e986561556c6050f7f",
  "adminPkh": "0a8a3ad55b68d3ffc3aac1e460cb525047139533239c75debf320775"
}
```

### Rebalance Distribution configuration:

```json
[
    // An array of pairs where each pair consists of a stake pool identifier and a percentage.
    // The sum of percentages should be 100, representing the desired distribution of ada in the staking basket.

    ["pool18pn6p9ef58u4ga3wagp44qhzm8f6zncl57g6qgh0pk3yytwz54h", 50.0],
    ["pool1ynfnjspgckgxjf2zeye8s33jz3e3ndk9pcwp0qzaupzvvd8ukwt", 50.0]
]
```
