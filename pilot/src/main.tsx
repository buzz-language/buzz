import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import {
  ConnectionProvider,
  WalletProvider
} from '@solana/wallet-adapter-react'
import { WalletModalProvider } from '@solana/wallet-adapter-react-ui'
import { PhantomWalletAdapter } from '@solana/wallet-adapter-wallets'
import { BrowserRouter, Routes, Route } from 'react-router-dom'
import '@solana/wallet-adapter-react-ui/styles.css'
import './index.css'
import './styles/vs-code-theme.css'
import App from './App'
import CreateDao from './pages/create-dao'

const wallets = [new PhantomWalletAdapter()]

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <BrowserRouter>
      <ConnectionProvider endpoint="https://api.mainnet-beta.solana.com">
        <WalletProvider wallets={wallets} autoConnect>
          <WalletModalProvider>
            <Routes>
              <Route path="/" element={<App />} />
              <Route path="/create-dao" element={<CreateDao />} />
            </Routes>
          </WalletModalProvider>
        </WalletProvider>
      </ConnectionProvider>
    </BrowserRouter>
  </StrictMode>,
)
