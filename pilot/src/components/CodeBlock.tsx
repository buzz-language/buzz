import React from 'react'
import { Card } from "@/components/ui/card"
import { useEffect, useRef } from "react"
import Prism from "prismjs"

interface CodeBlockProps {
  title?: string
  code: string
  language?: string
  showLineNumbers?: boolean
}

export const CodeBlock: React.FC<CodeBlockProps> = ({
  title,
  code,
  language = "python",
  showLineNumbers = true
}) => {
  const codeRef = useRef<HTMLElement>(null)

  useEffect(() => {
    if (codeRef.current) {
      Prism.highlightElement(codeRef.current)
    }
  }, [code])

  return (
    <div className="space-y-2">
      {title && (
        <h3 className="text-lg font-semibold text-white">{title}</h3>
      )}
      <Card className="bg-black border-[#404040] overflow-hidden">
        <pre className={`p-4 overflow-x-auto ${showLineNumbers ? 'line-numbers' : ''}`}>
          <code ref={codeRef} className={`language-${language} min-w-full block`}>
            {code.trim()}
          </code>
        </pre>
      </Card>
    </div>
  )
}
