import React, { useEffect, useRef } from 'react'
import { Text } from './sacred'
import Prism from 'prismjs'
import 'prismjs/plugins/line-numbers/prism-line-numbers'
import 'prismjs/plugins/line-numbers/prism-line-numbers.css'
import '../styles/vs-code-theme.css'
import '../lib/prism-buzz'

interface CodeBlockProps {
  title?: string
  code: string
  language?: string
}

export const CodeBlock: React.FC<CodeBlockProps> = ({
  title,
  code,
  language = "buzz"
}) => {
  const preRef = useRef<HTMLPreElement>(null)
  const codeRef = useRef<HTMLElement>(null)

  useEffect(() => {
    if (typeof window !== 'undefined') {
      Prism.manual = true
    }

    if (codeRef.current && preRef.current) {
      const formattedCode = code
        .split('\n')
        .map((line) => {
          if (line.trim() === '') return ''
          const indent = line.search(/\S/)
          if (indent === -1) return line
          const spaces = ' '.repeat(Math.floor(indent / 4) * 4)
          return spaces + line.trim()
        })
        .join('\n')

      codeRef.current.innerHTML = formattedCode
      preRef.current.className = `language-${language} line-numbers`
      codeRef.current.className = `language-${language}`

      requestAnimationFrame(() => {
        if (codeRef.current) {
          Prism.highlightElement(codeRef.current as HTMLElement)
        }
      })
    }
  }, [code, language])

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '2ch' }}>
      {title && (
        <Text style={{ fontWeight: 500 }}>{title}</Text>
      )}
      <div style={{
        border: '1px solid var(--theme-border)',
        overflow: 'hidden',
        background: 'var(--vscode-white)',
        height: '100%'
      }}>
        <pre
          ref={preRef}
          className={`language-${language} line-numbers`}
          style={{ margin: 0, background: 'transparent' }}
          data-start="1"
        >
          <code
            ref={codeRef}
            className={`language-${language}`}
            style={{
              whiteSpace: 'pre',
              tabSize: 4,
              background: 'transparent',
              fontFamily: 'var(--font-family)',
              fontSize: 'var(--font-size)',
              lineHeight: 'var(--theme-line-height-base)',
              padding: '2ch'
            }}
          >
            {code}
          </code>
        </pre>
      </div>
    </div>
  )
}
