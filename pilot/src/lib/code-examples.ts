export const securityExample = `# Secure Token Transfer

@verify_ownership("token")
@prevent_reentrancy
contract SecureTransfer:
    balance: u64
    owner: PublicKey

    def transfer(amount: u64, recipient: PublicKey):
        if self.balance >= amount:
            self.send_tokens(recipient, amount)
`

export const aiExample = `# AI Trading Strategy

@agent
@auto_validate
contract TradingBot:
    risk_level: u8
    portfolio: TokenAccount

    def execute_trade(pair: str, amount: u64):
        if self.analyze_market(pair):
            self.place_order(amount)
`

export const syntaxExample = `# Clean Python-like Syntax

contract Calculator:
    result: u64
    precision: u8

    def compute(a: u64, b: u64):
        if a > b:
            self.result = a - b
        else:
            self.result = a + b
`

export const solanaExample = `# Native Solana Integration

@program_id("ABC123...")
contract TokenSwap:
    pool_token: TokenAccount
    fee_account: TokenAccount

    def swap(amount_in: u64, min_out: u64):
        self.check_slippage(min_out)
        self.execute_swap(amount_in)
`
