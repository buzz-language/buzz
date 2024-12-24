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
                <code className="language-rust">
{`// Traditional Solana program
pub struct TradingAgent {
    pub strategy: String,
    pub risk_level: u8,
    pub max_slippage: u64,
}

impl TradingAgent {
    pub fn execute_trade(&self, ctx: Context) -> Result<()> {
        // Complex setup and validation
        self.validate_parameters(ctx)?;
        self.check_market_conditions()?;
        self.execute_strategy()?;
        Ok(())
    }
}`}
                </code>
              </pre>
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Buzz program
@ai_agent
class TradingAgent:
    strategy: str
    risk_level: u8
    max_slippage: u64

    @auto_validate
    def execute_trade(self):
        market_data = self.analyze_market()
        if self.should_trade(market_data):
            self.perform_trade()`}
                </code>
              </pre>
            </div>

            <h2 className="text-2xl font-semibold text-white mb-6">Secure Token Management</h2>
            <div className="grid md:grid-cols-2 gap-8">
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-rust">
{`// Traditional Solana program
pub fn process_token_transfer(
    program_id: &Pubkey,
    accounts: &[AccountInfo],
    amount: u64,
) -> ProgramResult {
    let account_iter = &mut accounts.iter();
    let source = next_account_info(account_iter)?;
    let dest = next_account_info(account_iter)?;

    if !source.is_signer {
        return Err(ProgramError::MissingRequiredSignature);
    }

    // Manual token checks
    if source.owner != program_id {
        return Err(ProgramError::InvalidAccountData);
    }
}`}
                </code>
              </pre>
              <pre className="bg-black p-4 rounded-lg overflow-x-auto border border-gray-800">
                <code className="language-python">
{`# Buzz program
@verify_token_account
@require_signature
def transfer_tokens(
    source: TokenAccount,
    dest: TokenAccount,
    amount: u64
):
    # Automatic validation
    source.transfer(dest, amount)`}
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
