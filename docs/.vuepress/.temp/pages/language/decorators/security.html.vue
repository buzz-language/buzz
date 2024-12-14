<template><div><h1 id="security-decorators" tabindex="-1"><a class="header-anchor" href="#security-decorators" aria-hidden="true">#</a> Security Decorators</h1>
<h2 id="overview" tabindex="-1"><a class="header-anchor" href="#overview" aria-hidden="true">#</a> Overview</h2>
<p>Security decorators in Pilot Buzz provide compile-time and runtime security checks for Solana agents. These decorators ensure that agents operate within defined security constraints, protecting against common vulnerabilities in Solana smart contract development.</p>
<h2 id="available-decorators" tabindex="-1"><a class="header-anchor" href="#available-decorators" aria-hidden="true">#</a> Available Decorators</h2>
<h3 id="verify-ownership" tabindex="-1"><a class="header-anchor" href="#verify-ownership" aria-hidden="true">#</a> @verify_ownership</h3>
<p>Ensures proper ownership validation for Solana accounts. This decorator verifies that the agent can only operate on accounts it owns or has been explicitly granted permission to access.</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@verify_ownership
object SecureAgent is agent\AgentState {
    owner: str = &quot;BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq&quot;
    // Agent implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="require-signer" tabindex="-1"><a class="header-anchor" href="#require-signer" aria-hidden="true">#</a> @require_signer</h3>
<p>Validates transaction signers to ensure that critical operations are authorized by the appropriate keypair. This is essential for maintaining transaction security.</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@require_signer
object SignedAgent is agent\AgentState {
    signer: str = &quot;Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm&quot;
    // Agent implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="check-balance" tabindex="-1"><a class="header-anchor" href="#check-balance" aria-hidden="true">#</a> @check_balance</h3>
<p>Enforces minimum balance requirements for agent operations. This decorator helps prevent insufficient funds errors and ensures agents maintain required liquidity.</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@check_balance(min_sol=1.5)
object LiquidAgent is agent\AgentState {
    min_balance: float = 1.5  // SOL
    // Agent implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h3 id="prevent-reentrancy" tabindex="-1"><a class="header-anchor" href="#prevent-reentrancy" aria-hidden="true">#</a> @prevent_reentrancy</h3>
<p>Prevents reentrancy attacks by implementing guards around critical sections of agent code. This is crucial for maintaining atomic operations and preventing exploitation.</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@prevent_reentrancy
object SafeAgent is agent\AgentState {
    lock: bool = false
    // Agent implementation
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h2 id="integration-example" tabindex="-1"><a class="header-anchor" href="#integration-example" aria-hidden="true">#</a> Integration Example</h2>
<p>Here's a comprehensive example combining multiple security decorators:</p>
<div class="language-buzz line-numbers-mode" data-ext="buzz"><pre v-pre class="language-buzz"><code>@verify_ownership
@require_signer
@check_balance(min_sol=2.0)
@prevent_reentrancy
object SecureTrader is agent\AgentState {
    owner: str = &quot;BzwZDtHxXkHnHMg3MFrH8dmxWX6NswHQpqGVkYqpGCEq&quot;
    signer: str = &quot;Hx6LbHY3QwvbJ8YazpSB3AHh4wJHyj9pWEe7qHYKQNUm&quot;
    min_balance: float = 2.0
    lock: bool = false

    fn should_enter(self) -&gt; bool {
        // Trading logic with security checks
        return true
    }

    fn should_exit(self) -&gt; bool {
        // Exit logic with security checks
        return false
    }
}
</code></pre><div class="line-numbers" aria-hidden="true"><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div><div class="line-number"></div></div></div><h2 id="best-practices" tabindex="-1"><a class="header-anchor" href="#best-practices" aria-hidden="true">#</a> Best Practices</h2>
<ol>
<li>Always use @verify_ownership when dealing with account ownership</li>
<li>Combine @require_signer with critical operations</li>
<li>Set appropriate min_balance values based on expected operations</li>
<li>Use @prevent_reentrancy for complex DeFi interactions</li>
</ol>
<h2 id="error-handling" tabindex="-1"><a class="header-anchor" href="#error-handling" aria-hidden="true">#</a> Error Handling</h2>
<p>Security decorators will raise compile-time errors when:</p>
<ul>
<li>Required fields are missing (owner, signer, min_balance, lock)</li>
<li>Field types don't match expected types</li>
<li>Decorator parameters are invalid</li>
</ul>
<p>Runtime checks will throw exceptions when:</p>
<ul>
<li>Ownership verification fails</li>
<li>Required signer is missing</li>
<li>Balance falls below minimum</li>
<li>Reentrancy is detected</li>
</ul>
</div></template>


