# Secure Trading Agent Example

This example demonstrates how to create a secure trading agent using Pilot Buzz's security decorators. The agent implements various security measures to ensure safe operation on the Solana blockchain.

## Basic Implementation

```buzz
@verify_ownership
@require_signer
@check_balance(min_sol=2.0)
@prevent_reentrancy
object SecureTrader is agent\AgentState {
    // Security fields
    owner: str = "BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq",
    signer: str = "Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm",
    min_balance: double = 2.0,  // Minimum SOL balance required
    lock: bool = false,         // Reentrancy guard

    // Trading strategy
    strategy = {
        entry: price < ma(200) and balance >= min_balance and !lock,
        exit: price > ma(200) or balance < min_balance,
        size: min(1000, available_liquidity * 0.1)
    }
}
```

## Security Features Explained

### Ownership Verification
The `@verify_ownership` decorator ensures that only the specified owner can control the agent:
```buzz
@verify_ownership
object OwnershipExample is agent\AgentState {
    owner: str = "BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq"
    // Implementation
}
```

### Transaction Signing
The `@require_signer` decorator enforces transaction signing requirements:
```buzz
@require_signer
object SignerExample is agent\AgentState {
    signer: str = "Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm"
    // Implementation
}
```

### Balance Checks
The `@check_balance` decorator prevents operations when balance is too low:
```buzz
@check_balance(min_sol=1.5)
object BalanceExample is agent\AgentState {
    min_balance: double = 1.5
    // Implementation
}
```

### Reentrancy Protection
The `@prevent_reentrancy` decorator guards against reentrancy attacks:
```buzz
@prevent_reentrancy
object ReentrancyExample is agent\AgentState {
    lock: bool = false
    // Implementation
}
```

## Arbitrage Example
Here's a practical example of a secure arbitrage bot:

```buzz
@check_balance
@prevent_reentrancy
object SecureArbitrage is agent\AgentState {
    owner: str = "DxCpxz3uZG5ZvZVqHqyWKmxmqAKvp3YzGzHKVvQJQkwN",
    min_balance: double = 50.0,
    lock: bool = false,

    strategy = {
        // Only enter when not locked and sufficient balance
        entry: !lock and arb_opportunity > 0.02 and balance >= min_balance,
        // Exit on small spread or low balance
        exit: arb_opportunity < 0.01 or balance < min_balance,
        // Conservative position sizing
        size: min(500, balance * 0.05)
    }
}
```

## Best Practices

1. Always combine multiple security decorators for comprehensive protection
2. Set conservative minimum balance requirements
3. Use the lock mechanism for complex DeFi interactions
4. Implement proper error handling for security violations
5. Regularly verify owner and signer addresses

## Error Handling

Security decorators will raise errors in these cases:

- `@verify_ownership`: Invalid owner public key or missing owner field
- `@require_signer`: Missing signer field or invalid signer
- `@check_balance`: Invalid min_balance value or insufficient funds
- `@prevent_reentrancy`: Missing lock field or invalid lock type

Example error handling:
```buzz
try {
    // Agent operations
} catch (SecurityError e) {
    if (e.code == 4001) {
        // Handle ownership verification error
    } else if (e.code == 4003) {
        // Handle signer requirement error
    }
    // Handle other security errors
}
```
