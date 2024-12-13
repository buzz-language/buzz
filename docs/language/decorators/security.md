# Security Decorators

## Overview
Security decorators in Pilot Buzz provide compile-time and runtime security checks for Solana agents. These decorators ensure that agents operate within defined security constraints, protecting against common vulnerabilities in Solana smart contract development.

## Available Decorators

### @verify_ownership
Ensures proper ownership validation for Solana accounts. This decorator verifies that the agent can only operate on accounts it owns or has been explicitly granted permission to access.

```buzz
@verify_ownership
object SecureAgent is agent\AgentState {
    owner: str = "BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq"
    // Agent implementation
}
```

### @require_signer
Validates transaction signers to ensure that critical operations are authorized by the appropriate keypair. This is essential for maintaining transaction security.

```buzz
@require_signer
object SignedAgent is agent\AgentState {
    signer: str = "Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm"
    // Agent implementation
}
```

### @check_balance
Enforces minimum balance requirements for agent operations. This decorator helps prevent insufficient funds errors and ensures agents maintain required liquidity.

```buzz
@check_balance(min_sol=1.5)
object LiquidAgent is agent\AgentState {
    min_balance: float = 1.5  // SOL
    // Agent implementation
}
```

### @prevent_reentrancy
Prevents reentrancy attacks by implementing guards around critical sections of agent code. This is crucial for maintaining atomic operations and preventing exploitation.

```buzz
@prevent_reentrancy
object SafeAgent is agent\AgentState {
    lock: bool = false
    // Agent implementation
}
```

## Integration Example
Here's a comprehensive example combining multiple security decorators:

```buzz
@verify_ownership
@require_signer
@check_balance(min_sol=2.0)
@prevent_reentrancy
object SecureTrader is agent\AgentState {
    owner: str = "BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq"
    signer: str = "Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm"
    min_balance: float = 2.0
    lock: bool = false

    fn should_enter(self) -> bool {
        // Trading logic with security checks
        return true
    }

    fn should_exit(self) -> bool {
        // Exit logic with security checks
        return false
    }
}
```

## Best Practices
1. Always use @verify_ownership when dealing with account ownership
2. Combine @require_signer with critical operations
3. Set appropriate min_balance values based on expected operations
4. Use @prevent_reentrancy for complex DeFi interactions

## Error Handling
Security decorators will raise compile-time errors when:
- Required fields are missing (owner, signer, min_balance, lock)
- Field types don't match expected types
- Decorator parameters are invalid

Runtime checks will throw exceptions when:
- Ownership verification fails
- Required signer is missing
- Balance falls below minimum
- Reentrancy is detected
