import React from 'react'
import { Card } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { CodeBlock } from "@/components/CodeBlock"
import { Link } from 'react-router-dom'
import { RocketIcon } from 'lucide-react'

const daoExample = `@dao_contract
contract MemeDAO:
    token: TokenAccount  # SPL or SPL22 token
    agents: List[Agent]

    @quadratic_voting
    def create_agent(
        code: str,
        description: str
    ):
        # Verify token staking
        assert self.token.staked_amount > 0

        # Deploy new AI agent
        agent = self.compile_and_deploy(code)
        self.agents.push(agent)`

const CreateDao: React.FC = () => {
  return (
    <div className="min-h-screen bg-black text-white font-['Geist-Mono-Regular']">
      <nav className="px-8 py-6 flex justify-between items-center border-b border-[#404040]">
        <Link to="/" className="flex items-center space-x-4">
          <RocketIcon size={24} className="text-white" />
          <span className="text-lg">Buzz</span>
        </Link>
        <Button className="bg-black hover:bg-[#1a1a1a] border border-[#404040] rounded-none px-6">
          Select Wallet
        </Button>
      </nav>

      <main className="px-8 py-16 max-w-4xl mx-auto">
        <div className="mb-16">
          <h1 className="text-4xl mb-4">Create Your AI DAO</h1>
          <p className="text-lg text-gray-300">
            Launch a memecoin-powered DAO with autonomous AI agents. Build, govern, and deploy on-chain programs together.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-8 mb-16">
          <Card className="p-6 bg-black border-[#404040] rounded-none">
            <h3 className="text-xl mb-2">Token Integration</h3>
            <p className="text-gray-300">Support for SPL and SPL22 tokens with secure staking verification.</p>
          </Card>
          <Card className="p-6 bg-black border-[#404040] rounded-none">
            <h3 className="text-xl mb-2">Quadratic Voting</h3>
            <p className="text-gray-300">Fair and efficient decision-making for your DAO's governance.</p>
          </Card>
        </div>

        <Card className="p-8 bg-black border-[#404040] rounded-none">
          <div className="mb-8">
            <CodeBlock code={daoExample} />
          </div>

          <form className="space-y-8">
            <div>
              <label className="block mb-2">Token Address (SPL/SPL22)</label>
              <input
                type="text"
                className="w-full p-3 bg-black border border-[#404040] rounded-none"
                placeholder="Enter token address..."
              />
            </div>

            <div>
              <label className="block mb-2">DAO Name</label>
              <input
                type="text"
                className="w-full p-3 bg-black border border-[#404040] rounded-none"
                placeholder="Enter DAO name..."
              />
            </div>

            <Button className="w-full bg-black hover:bg-[#1a1a1a] border border-[#404040] rounded-none py-6">
              Connect Wallet to Continue
            </Button>
          </form>
        </Card>
      </main>
    </div>
  )
}

export default CreateDao
