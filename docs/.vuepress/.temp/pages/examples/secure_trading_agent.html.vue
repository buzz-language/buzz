<template><div><h1 id="secure-trading-agent-example" tabindex="-1"><a class="header-anchor" href="#secure-trading-agent-example" aria-hidden="true">#</a> Secure Trading Agent Example</h1>
<p>This example demonstrates how to create a secure trading agent using Pilot Buzz's security decorators. The agent implements various security measures to ensure safe operation on the Solana blockchain.</p>
<h2 id="basic-implementation" tabindex="-1"><a class="header-anchor" href="#basic-implementation" aria-hidden="true">#</a> Basic Implementation</h2>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@verify_ownership
@require_signer
@check_balance(min_sol=2.0)
@prevent_reentrancy
object SecureTrader is agent\AgentState {
    // Security fields
    owner: str = &quot;BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq&quot;,
    signer: str = &quot;Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm&quot;,
    min_balance: double = 2.0,  // Minimum SOL balance required
    lock: bool = false,         // Reentrancy guard

    // Trading strategy
    strategy = {
        entry: price &lt; ma(200) and balance &gt;= min_balance and !lock,
        exit: price &gt; ma(200) or balance &lt; min_balance,
        size: min(1000, available_liquidity * 0.1)
    }
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h2 id="security-features-explained" tabindex="-1"><a class="header-anchor" href="#security-features-explained" aria-hidden="true">#</a> Security Features Explained</h2>
<h3 id="ownership-verification" tabindex="-1"><a class="header-anchor" href="#ownership-verification" aria-hidden="true">#</a> Ownership Verification</h3>
<p>The <code v-pre>@verify_ownership</code> decorator ensures that only the specified owner can control the agent:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@verify_ownership
object OwnershipExample is agent\AgentState {
    owner: str = &quot;BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq&quot;
    // Implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="transaction-signing" tabindex="-1"><a class="header-anchor" href="#transaction-signing" aria-hidden="true">#</a> Transaction Signing</h3>
<p>The <code v-pre>@require_signer</code> decorator enforces transaction signing requirements:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@require_signer
object SignerExample is agent\AgentState {
    signer: str = &quot;Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm&quot;
    // Implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="balance-checks" tabindex="-1"><a class="header-anchor" href="#balance-checks" aria-hidden="true">#</a> Balance Checks</h3>
<p>The <code v-pre>@check_balance</code> decorator prevents operations when balance is too low:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@check_balance(min_sol=1.5)
object BalanceExample is agent\AgentState {
    min_balance: double = 1.5
    // Implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="reentrancy-protection" tabindex="-1"><a class="header-anchor" href="#reentrancy-protection" aria-hidden="true">#</a> Reentrancy Protection</h3>
<p>The <code v-pre>@prevent_reentrancy</code> decorator guards against reentrancy attacks:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@prevent_reentrancy
object ReentrancyExample is agent\AgentState {
    lock: bool = false
    // Implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h2 id="arbitrage-example" tabindex="-1"><a class="header-anchor" href="#arbitrage-example" aria-hidden="true">#</a> Arbitrage Example</h2>
<p>Here's a practical example of a secure arbitrage bot:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@check_balance
@prevent_reentrancy
object SecureArbitrage is agent\AgentState {
    owner: str = &quot;DxCpxz3uZG5ZvZVqHqyWKmxmqAKvp3YzGzHKVvQJQkwN&quot;,
    min_balance: double = 50.0,
    lock: bool = false,

    strategy = {
        // Only enter when not locked and sufficient balance
        entry: !lock and arb_opportunity &gt; 0.02 and balance &gt;= min_balance,
        // Exit on small spread or low balance
        exit: arb_opportunity &lt; 0.01 or balance &lt; min_balance,
        // Conservative position sizing
        size: min(500, balance * 0.05)
    }
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h2 id="best-practices" tabindex="-1"><a class="header-anchor" href="#best-practices" aria-hidden="true">#</a> Best Practices</h2>
<ol>
<li>Always combine multiple security decorators for comprehensive protection</li>
<li>Set conservative minimum balance requirements</li>
<li>Use the lock mechanism for complex DeFi interactions</li>
<li>Implement proper error handling for security violations</li>
<li>Regularly verify owner and signer addresses</li>
</ol>
<h2 id="error-handling" tabindex="-1"><a class="header-anchor" href="#error-handling" aria-hidden="true">#</a> Error Handling</h2>
<p>Security decorators will raise errors in these cases:</p>
<ul>
<li><code v-pre>@verify_ownership</code>: Invalid owner public key or missing owner field</li>
<li><code v-pre>@require_signer</code>: Missing signer field or invalid signer</li>
<li><code v-pre>@check_balance</code>: Invalid min_balance value or insufficient funds</li>
<li><code v-pre>@prevent_reentrancy</code>: Missing lock field or invalid lock type</li>
</ul>
<p>Example error handling:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>try {
    // Agent operations
} catch (SecurityError e) {
    if (e.code == 4001) {
        // Handle ownership verification error
    } else if (e.code == 4003) {
        // Handle signer requirement error
    }
    // Handle other security errors
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div></div></template>


