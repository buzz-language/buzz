export const securityExample = `# AI Trading Agent with Security
@agent
@verify_ownership("token")
@prevent_reentrancy
contract TradingBot:
    balance: u64
    risk_level: u8

    @auto_validate
    def swap_tokens(amount: u64):
        if self.check_market_conditions():
            self.execute_swap(amount)`

export const daoExample = `# DAO Governance
@dao_contract
contract TokenDAO:
    token: TokenAccount
    proposals: List[Proposal]

    @quadratic_voting
    def propose_upgrade(
        program_id: PublicKey,
        description: str
    ):
        self.proposals.push(
            Proposal(program_id, description)
        )`

export const defiExample = `# Liquidity Pool Contract
@pool_contract
contract LiquidityPool:
    token_a: TokenAccount
    token_b: TokenAccount

    @check_balance
    def provide_liquidity(
        amount_a: u64,
        amount_b: u64
    ):
        self.validate_ratio(amount_a, amount_b)
        self.mint_lp_tokens()`

export const yieldExample = `# Yield Farming
@farm_contract
@auto_compound
contract YieldFarm:
    stake_token: TokenAccount
    reward_token: TokenAccount

    @check_staking_period
    def harvest_rewards(user: Account):
        rewards = self.calculate_rewards(user)
        self.distribute_rewards(user, rewards)`
