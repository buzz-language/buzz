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
    <div className="app">
      <ActionBar items={actionBarItems}>
        <Text style={{ fontWeight: 500 }}>BUZZ LANG</Text>
      </ActionBar>

      <Container isMain>
        <Grid style={{ gap: '6rem' }}>
          <div style={{
            display: 'flex',
            flexDirection: 'column',
            gap: '2ch',
            maxWidth: '80ch',
            margin: '0 auto',
            textAlign: 'center'
          }}>
            <Text style={{
              fontSize: '48px',
              fontWeight: 600,
              color: 'var(--theme-headline-main)',
              lineHeight: 1.2,
              marginBottom: '1.5rem'
            }}>
              The Language for Solana Smart Contracts
            </Text>
            <Text style={{
              fontSize: '20px',
              color: 'var(--theme-text-secondary)',
              maxWidth: '60ch',
              margin: '0 auto'
            }}>
              Write secure, efficient, and maintainable Solana programs with Python-like syntax.
            </Text>
          </div>

          <div style={{
            display: 'grid',
            gridTemplateColumns: 'repeat(auto-fit, minmax(min(100%, 500px), 1fr))',
            gap: '4rem',
            width: '100%',
            padding: '0 2rem'
          }}>
            <Text style={{
              fontSize: '32px',
              fontWeight: 600,
              color: 'var(--theme-headline)',
              marginBottom: '2rem',
              gridColumn: '1 / -1',
              textAlign: 'center'
            }}>
              Features
            </Text>

            <Container style={{ height: 'auto' }}>
              <Text style={{ fontSize: '24px', fontWeight: 500, color: 'var(--theme-headline-secondary)', marginBottom: '1rem' }}>
                Security First
              </Text>
              <Text style={{ color: 'var(--theme-text-secondary)', marginBottom: '2rem' }}>
                Built-in decorators for ownership verification and reentrancy protection.
              </Text>
              <CodeBlock code={securityExample} language="python" />
            </Container>

            <Container style={{ height: 'auto' }}>
              <Text style={{ fontSize: '24px', fontWeight: 500, color: 'var(--theme-headline-secondary)', marginBottom: '1rem' }}>
                AI Ready
              </Text>
              <Text style={{ color: 'var(--theme-text-secondary)', marginBottom: '2rem' }}>
                First-class support for on-chain AI agents and autonomous programs.
              </Text>
              <CodeBlock code={aiExample} language="python" />
            </Container>

            <Container style={{ height: 'auto' }}>
              <Text style={{ fontSize: '24px', fontWeight: 500, color: 'var(--theme-headline-secondary)', marginBottom: '1rem' }}>
                Clean Syntax
              </Text>
              <Text style={{ color: 'var(--theme-text-secondary)', marginBottom: '2rem' }}>
                Python-like syntax for writing smart contracts.
              </Text>
              <CodeBlock code={syntaxExample} language="python" />
            </Container>

            <Container style={{ height: 'auto' }}>
              <Text style={{ fontSize: '24px', fontWeight: 500, color: 'var(--theme-headline-secondary)', marginBottom: '1rem' }}>
                Solana Native
              </Text>
              <Text style={{ color: 'var(--theme-text-secondary)', marginBottom: '2rem' }}>
                Native Solana integration with built-in account management.
              </Text>
              <CodeBlock code={solanaExample} language="python" />
            </Container>
          </div>
        </Grid>
      </Container>
    </div>
  )
}

export default App
