import { WalletMultiButton } from '@solana/wallet-adapter-react-ui'
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Card } from "@/components/ui/card"
import { Textarea } from "@/components/ui/textarea"
import { Label } from "@/components/ui/label"
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { RocketIcon } from 'lucide-react'

function App() {
  return (
    <div className="min-h-screen bg-gradient-to-r from-purple-500 to-pink-500">
      <nav className="p-4 flex justify-between items-center">
        <div className="flex items-center space-x-2 text-white">
          <RocketIcon size={24} />
          <span className="text-xl font-bold">Pilot.buzz</span>
        </div>
        <WalletMultiButton />
      </nav>

      <main className="container mx-auto px-4 py-8">
        <Card className="max-w-2xl mx-auto p-6">
          <h1 className="text-3xl font-bold mb-8 text-center">Create Your AI DAO</h1>

          <form className="space-y-6">
            <div className="space-y-4">
              <h2 className="text-xl font-semibold">Token Configuration</h2>
              <div className="space-y-2">
                <Label htmlFor="token-address">Token Address (SPL/SPL22)</Label>
                <Input id="token-address" placeholder="Enter token address" />
              </div>
              <div className="space-y-2">
                <Label htmlFor="staking-amount">Staking Amount</Label>
                <Input id="staking-amount" type="number" placeholder="Enter amount to stake" />
              </div>
            </div>

            <div className="space-y-4">
              <h2 className="text-xl font-semibold">DAO Settings</h2>
              <div className="space-y-2">
                <Label htmlFor="dao-name">DAO Name</Label>
                <Input id="dao-name" placeholder="Enter DAO name" />
              </div>
              <div className="space-y-2">
                <Label htmlFor="voting-system">Voting System</Label>
                <Select defaultValue="quadratic">
                  <SelectTrigger>
                    <SelectValue placeholder="Select voting system" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="quadratic">Quadratic Voting</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>

            <div className="space-y-4">
              <h2 className="text-xl font-semibold">AI Agent Settings</h2>
              <div className="space-y-2">
                <Label htmlFor="agent-code">Buzz Language Agent Code</Label>
                <Textarea
                  id="agent-code"
                  placeholder="Enter your Buzz language agent code here..."
                  className="h-48"
                />
              </div>
            </div>

            <Button type="submit" className="w-full">
              Create DAO
            </Button>
          </form>
        </Card>
      </main>
    </div>
  )
}

export default App
