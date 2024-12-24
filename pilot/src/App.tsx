import { Button } from "@/components/ui/button"
import { Card } from "@/components/ui/card"
import { Code2Icon, ShieldCheckIcon, RocketIcon, BrainCircuitIcon, CoinsIcon } from 'lucide-react'
import { Link } from 'react-router-dom'
import { useEffect } from 'react'
import Prism from 'prismjs'
import 'prismjs/components/prism-python'
import 'prismjs/components/prism-rust'
import 'prismjs/themes/prism.css'

// Theme configuration
const codeTheme = {
  'code[class*="language-"]': {
    color: '#FFFFFF',
    background: '#000000',
    textShadow: 'none',
  },
  '.token.comment': { color: '#6A9955' },
  '.token.keyword': { color: '#569CD6' },
  '.token.string': { color: '#CE9178' },
  '.token.function': { color: '#DCDCAA' },
  '.token.class-name': { color: '#4EC9B0' },
  '.token.decorator': { color: '#C586C0' },
  '.token.variable': { color: '#9CDCFE' },
}

// Apply theme to Prism
;(Prism as any).styles = { ...((Prism as any).styles || {}), ...codeTheme }

const App: React.FC = () => {
  useEffect(() => {
    Prism.highlightAll()
  }, [])

  return (
    <div className="min-h-screen bg-black">
      <nav className="p-4 flex justify-between items-center backdrop-blur-sm bg-black/30">
        <div className="flex items-center space-x-2 text-white">
          <RocketIcon size={24} className="text-pink-400" />
          <span className="text-xl font-bold">Buzz</span>
        </div>
        <div className="flex items-center space-x-4">
          <Link to="/docs" className="text-white hover:text-pink-400">Docs</Link>
          <Link to="/create-dao">
            <Button className="bg-pink-500 hover:bg-pink-600">
              Create DAO
            </Button>
          </Link>
        </div>
      </nav>

      <main className="container mx-auto px-4 py-12">
        <div className="text-center mb-16">
          <h1 className="text-6xl font-bold text-white mb-6">
            The Language for<br />
            <span className="text-pink-400">Solana Smart Contracts</span>
          </h1>
          <p className="text-xl text-gray-200 max-w-2xl mx-auto">
            Write secure, efficient, and maintainable Solana programs with Python-like syntax and built-in security features.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8 mb-16">
          <Card className="p-6 bg-black/30 backdrop-blur-sm border-0">
            <ShieldCheckIcon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">Security First</h3>
            <p className="text-gray-200">Built-in decorators for ownership verification and reentrancy protection.</p>
          </Card>
          <Card className="p-6 bg-black/30 backdrop-blur-sm border-0">
            <BrainCircuitIcon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">AI Ready</h3>
            <p className="text-gray-200">First-class support for on-chain AI agents and autonomous programs.</p>
          </Card>
          <Card className="p-6 bg-black/30 backdrop-blur-sm border-0">
            <Code2Icon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">Clean Syntax</h3>
            <p className="text-gray-200">Familiar Python-like syntax with modern language features and type safety.</p>
          </Card>
          <Card className="p-6 bg-black/30 backdrop-blur-sm border-0">
            <CoinsIcon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">Solana Native</h3>
            <p className="text-gray-200">Deep integration with Solana's programming model and account system.</p>
          </Card>
        </div>

        <Card className="max-w-4xl mx-auto p-8 bg-black/30 backdrop-blur-sm border-0">
          <div className="space-y-12">
            <h2 className="text-2xl font-semibold text-white mb-6">Write Secure Code by Default</h2>
            <div className="grid md:grid-cols-2 gap-8">
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Traditional Solana program
if ctx.accounts.owner.key() != ctx.accounts.payer.key():
    return Err(ProgramError::InvalidArgument)

# Check for reentrancy
let counter = ctx.accounts.state.counter
if counter != 0:
    return Err(ProgramError::AccountInUse)`}
                </code>
              </pre>
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Buzz program
@verify_ownership
@prevent_reentrancy
def transfer(sender: Account, amount: u64):
    if sender.balance >= amount:
        perform_transfer(sender, amount)`}
                </code>
              </pre>
            </div>

            <h2 className="text-2xl font-semibold text-white mb-6">Create AI Agents Easily</h2>
            <div className="grid md:grid-cols-2 gap-8">
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# AI Trading Agent with Security
@agent
@verify_ownership("token")
@prevent_reentrancy
contract TradingBot:
    balance: u64
    risk_level: u8

    @auto_validate
    def swap_tokens(amount: u64):
        if self.check_market_conditions():
            self.execute_swap(amount)
`}
                </code>
              </pre>
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# DAO Governance
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
        )
`}
                </code>
              </pre>
            </div>

            <h2 className="text-2xl font-semibold text-white mb-6 mt-12">DeFi Integration</h2>
            <div className="grid md:grid-cols-2 gap-8">
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Liquidity Pool Contract
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
        self.mint_lp_tokens()
`}
                </code>
              </pre>
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Yield Farming
@farm_contract
@auto_compound
contract YieldFarm:
    stake_token: TokenAccount
    reward_token: TokenAccount

    @check_staking_period
    def harvest_rewards(user: Account):
        rewards = self.calculate_rewards(user)
        self.distribute_rewards(user, rewards)
`}
                </code>
              </pre>
            </div>
          </div>
        </Card>
      </main>
    </div>
  )
}

export default App
