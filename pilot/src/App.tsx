import { ActionBar, Grid, Text, Container } from './components/sacred'
import { CodeBlock } from "./components/CodeBlock"
import { securityExample, aiExample, syntaxExample, solanaExample } from "./lib/code-examples"
import { Link } from 'react-router-dom'

const App: React.FC = () => {
  const actionBarItems = [
    {
      id: 'docs',
      hotkey: 'd',
      body: <Link to="/docs"><Text>DOCS</Text></Link>,
      onClick: () => {}
    },
    {
      id: 'create-dao',
      hotkey: 'c',
      body: <Link to="/create-dao"><Text>CREATE DAO</Text></Link>,
      onClick: () => {}
    }
  ];

  return (
    <div className="grid-background">
      <ActionBar items={actionBarItems}>
        <Text style={{ fontWeight: 500 }}>BUZZ LANG</Text>
      </ActionBar>

      <Grid>
        <div style={{ display: 'flex', flexDirection: 'column', gap: '2ch', maxWidth: '80ch' }}>
          <Text style={{ fontSize: '24px', fontWeight: 500, color: 'var(--theme-text)' }}>The Language for Solana Smart Contracts</Text>
          <Text style={{ color: 'rgba(255, 255, 255, 0.7)' }}>Write secure, efficient, and maintainable Solana programs with Python-like syntax.</Text>
        </div>

        <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: '4ch', width: '100%' }}>
          <Container>
            <Text style={{ marginBottom: '2ch', fontWeight: 500 }}>Security First</Text>
            <Text style={{ color: 'var(--vscode-gray)' }}>Built-in decorators for ownership verification and reentrancy protection.</Text>
          </Container>
          <Container>
            <Text style={{ marginBottom: '2ch', fontWeight: 500 }}>AI Ready</Text>
            <Text style={{ color: 'var(--vscode-gray)' }}>First-class support for on-chain AI agents and autonomous programs.</Text>
          </Container>
        </div>

        <div style={{ display: 'flex', flexDirection: 'column', gap: '4ch' }}>
          <Text style={{ fontSize: '18px', fontWeight: 500 }}>Code Examples</Text>
          <div style={{
            display: 'grid',
            gridTemplateColumns: '1fr 1fr',
            gap: '4ch',
            width: '100%',
            gridAutoRows: '1fr'
          }}>
            <CodeBlock title="Security First" code={securityExample} />
            <CodeBlock title="AI Ready" code={aiExample} />
            <CodeBlock title="Clean Syntax" code={syntaxExample} />
            <CodeBlock title="Solana Native" code={solanaExample} />
          </div>
        </div>
      </Grid>
    </div>
  )
}

export default App
