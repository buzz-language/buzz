
# 👨‍🚀 Pilot Buzz

## Features

- **Agent-First Design**: Built specifically for creating autonomous on-chain agents
- **DeFi-Native Syntax**: Specialized syntax sugar for common DeFi operations
- **AI/ML Integration**: First-class support for machine learning models and strategies
- **Agent Communication**: Built-in protocols for agent-to-agent communication
- **Safety First**: Static typing and compile-time checks for on-chain safety
- **Rapid Development**: Create complex DeFi bots in just a few lines of code

## Example: Simple Arbitrage Bot in 20 Lines

```buzz
import "solana/defi" as defi;

// Define pools with minimal syntax
pool raydium_sol_usdc SOL USDC {
    fee = 0.3%
    price = SOL/USDC
}

pool orca_sol_usdc SOL USDC {
    fee = 0.3%
    price = SOL/USDC
}

// Define arbitrage strategy
strategy arb_strategy {
    entry {
        (raydium_sol_usdc.price / orca_sol_usdc.price - 1) > 0.005
    }
    size {
        min(1000, available_liquidity * 0.1)
    }
}

// Create and run the bot
bot arb_bot {
    pools = [raydium_sol_usdc, orca_sol_usdc]
    strategy = arb_strategy
    risk = 1%
}
```

## Key Features for On-Chain Agents

### 1. Agent Communication
```buzz
// Define agent-to-agent message protocol
message OrderIntent {
    token: str,
    side: str,
    amount: double,
    price: double
}

// Agent communication channel
channel dex_orders {
    broadcast: [OrderIntent],
    subscribe: fun(handler: fun(OrderIntent) > void)
}
```

### 2. Built-in Strategy Patterns
```buzz
strategy mean_reversion {
    entry {
        price < ma(200) && rsi < 30
    }
    exit {
        price > ma(200) || rsi > 70
    }
    size {
        portfolio.value * 0.1
    }
}
```

### 3. AI Model Integration
```buzz
model price_predictor {
    inputs {
        price_history: [double],
        volume_history: [double]
    }
    outputs {
        predicted_price: double,
        confidence: double
    }
}
```

### 4. Risk Management
```buzz
risk_manager {
    max_position_size = 5%
    stop_loss = 2%
    take_profit = 5%
    max_drawdown = 10%
}
```

## Installation

```bash
# install locally at ~/.local
zig build -Doptimize=ReleaseSafe install -p ~/.local

# install globally at /usr/local
sudo zig build -Doptimize=ReleaseSafe install -p /usr/local
```

## Documentation

- [Getting Started](https://buzz-lang.dev/docs/getting-started)
- [Agent Development Guide](https://buzz-lang.dev/docs/agents)
- [DeFi Integration](https://buzz-lang.dev/docs/defi)
- [Strategy Development](https://buzz-lang.dev/docs/strategies)
- [Risk Management](https://buzz-lang.dev/docs/risk)

## Examples

- [Simple Arbitrage Bot](examples/simple_arb_bot.buzz)
- [Yield Farming Bot](examples/yield_farmer_bot.buzz)
- [Market Making Agent](examples/market_maker.buzz)
- [Multi-Agent System](examples/multi_agent.buzz)

## Contributing

Contributions are welcome! Please read our [Contributing Guide](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## License

MIT License - see the [LICENSE](LICENSE) file for details.
