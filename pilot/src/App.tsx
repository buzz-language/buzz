import { WalletMultiButton } from '@solana/wallet-adapter-react-ui'
import { useWallet } from '@solana/wallet-adapter-react'
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Card } from "@/components/ui/card"
import { Textarea } from "@/components/ui/textarea"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { RocketIcon, Code2Icon, CoinsIcon, UsersIcon } from 'lucide-react'
import { useState } from 'react'

function App() {
  const { connected } = useWallet()
  const [isSubmitting, setIsSubmitting] = useState(false)

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setIsSubmitting(true)
    // Simulate submission delay
    await new Promise(resolve => setTimeout(resolve, 1000))
    setIsSubmitting(false)
  }

  return (
    <div className="min-h-screen bg-gradient-to-r from-purple-900 via-purple-800 to-pink-900">
      <nav className="p-4 flex justify-between items-center backdrop-blur-sm bg-black/10">
        <div className="flex items-center space-x-2 text-white">
          <RocketIcon size={24} className="text-pink-400" />
          <span className="text-xl font-bold">Pilot.buzz</span>
        </div>
        <WalletMultiButton />
      </nav>

      <main className="container mx-auto px-4 py-12">
        <div className="text-center mb-12">
          <h1 className="text-5xl font-bold text-white mb-4">Create Your AI DAO</h1>
          <p className="text-xl text-gray-200 max-w-2xl mx-auto">
            Launch a memecoin-powered DAO with autonomous AI agents. Build, govern, and deploy on-chain programs together.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-12">
          <Card className="p-6 bg-white/10 backdrop-blur-sm border-0">
            <CoinsIcon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">Token Integration</h3>
            <p className="text-gray-200">Support for SPL and SPL22 tokens with secure staking verification.</p>
          </Card>
          <Card className="p-6 bg-white/10 backdrop-blur-sm border-0">
            <UsersIcon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">Quadratic Voting</h3>
            <p className="text-gray-200">Fair and efficient decision-making for your DAO's governance.</p>
          </Card>
          <Card className="p-6 bg-white/10 backdrop-blur-sm border-0">
            <Code2Icon size={32} className="text-pink-400 mb-4" />
            <h3 className="text-xl font-semibold text-white mb-2">AI Agents</h3>
            <p className="text-gray-200">Deploy custom Buzz language agents for automated operations.</p>
          </Card>
        </div>

        <Card className="max-w-2xl mx-auto p-8 bg-white/10 backdrop-blur-sm border-0">
          <form className="space-y-8" onSubmit={handleSubmit}>
            {!connected ? (
              <div className="text-center py-8">
                <p className="text-white mb-4">Connect your wallet to create a DAO</p>
                <WalletMultiButton />
              </div>
            ) : (
              <>
                <div className="space-y-4">
                  <h2 className="text-2xl font-semibold text-white">Token Configuration</h2>
                  <div className="space-y-2">
                    <Label htmlFor="token-address" className="text-gray-200">Token Address (SPL/SPL22)</Label>
                    <Input id="token-address" placeholder="Enter token address" className="bg-white/5 border-white/10 text-white" />
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="staking-amount" className="text-gray-200">Staking Amount</Label>
                    <Input id="staking-amount" type="number" placeholder="Enter amount to stake" className="bg-white/5 border-white/10 text-white" />
                  </div>
                </div>

                <div className="space-y-4">
                  <h2 className="text-2xl font-semibold text-white">DAO Settings</h2>
                  <div className="space-y-2">
                    <Label htmlFor="dao-name" className="text-gray-200">DAO Name</Label>
                    <Input id="dao-name" placeholder="Enter DAO name" className="bg-white/5 border-white/10 text-white" />
                  </div>
                  <div className="space-y-2">
                    <Label htmlFor="voting-system" className="text-gray-200">Voting System</Label>
                    <Select defaultValue="quadratic">
                      <SelectTrigger className="bg-white/5 border-white/10 text-white">
                        <SelectValue placeholder="Select voting system" />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="quadratic">Quadratic Voting</SelectItem>
                      </SelectContent>
                    </Select>
                  </div>
                </div>

                <div className="space-y-4">
                  <h2 className="text-2xl font-semibold text-white">AI Agent Settings</h2>
                  <div className="space-y-2">
                    <Label htmlFor="agent-code" className="text-gray-200">Buzz Language Agent Code</Label>
                    <Textarea
                      id="agent-code"
                      placeholder="Enter your Buzz language agent code here..."
                      className="h-48 font-mono bg-white/5 border-white/10 text-white"
                    />
                  </div>
                </div>

                <Button
                  type="submit"
                  className="w-full bg-pink-500 hover:bg-pink-600"
                  disabled={isSubmitting}
                >
                  {isSubmitting ? 'Creating DAO...' : 'Create DAO'}
                </Button>
              </>
            )}
          </form>
        </Card>
      </main>
    </div>
  )
}

export default App
