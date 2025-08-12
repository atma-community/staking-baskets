
---

<p align="center">
  <img src="https://github.com/user-attachments/assets/d93aef5a-69c5-4dac-bd82-8f51480c97d5" alt="logo" width="150"/>
</p>



# Staking Baskets

Staking Baskets offer a unique solution for ADA holders to diversify their staking portfolios and maximize participation in the Cardano network. Unlike traditional staking methods, users can distribute their ADA across multiple stake pools within a single basket, reducing risk and potentially enhancing rewards.

---

## Key Features
* **Diversified Staking:** Spread ADA holdings across various stake pools within a single basket to mitigate risk and optimize rewards.  
* **Basket Tokens:** Fungible tokens represent a user's stake in a basket. Their exchange rate evolves with accumulated staking rewards.  
* **Deposits Protection:** Users can withdraw their deposited ADA in full at any time (unless the basket is locked). The protocol ensures sufficient liquidity for all withdrawals.  
* **Monotonically Growing Exchange Rate:** Once the basket token's exchange rate increases, it will never decrease, guaranteeing consistent and growing value for users' basket tokens.  
* **Administrator Controls:** Administrators manage reward withdrawals and rebalancing of ADA distribution among stake pools. Users should select trustworthy administrators.  

---

## How it Works (High-Level)
1. **Deposit ADA:** Users deposit ADA into a basket and receive basket tokens based on the current exchange rate.  
2. **Staking Rewards:** The basket earns Cardano staking rewards, which increase the total ADA within the basket and thus the exchange rate of the basket tokens.  
3. **Withdraw ADA:** Users burn their basket tokens to redeem ADA at the current exchange rate, reflecting earned rewards.  
4. **Basket Lock Mechanism:** A locking mechanism prevents concurrent user transactions during critical administrator operations (e.g., exchange rate updates, rebalancing), ensuring atomicity. This lock has a maximum duration, after which any user can unlock the basket.  

---

## About This Repository
- To provide transparency into how the Staking Baskets protocol was designed and implemented  
- To serve as a reference for developers building similar concepts on Cardano  
- To act as an educational resource for learning Plutarch-based smart contract development  

If you wish to use this code, please fork it and adapt it for your own purposes. We strongly encourage thorough review and testing before using any portion of this code in production.  

---
## Audit Report

- Audited by [Anastasia Labs](https://www.anastasialabs.com/).

---

## License

This project is released under the [Apache 2.0 License](./LICENSE). You are free to use and adapt this code with proper attribution. No warranties are provided.  

---

## Community and Credits

- Funded in part by [Project Catalyst](https://projectcatalyst.io/).

---


