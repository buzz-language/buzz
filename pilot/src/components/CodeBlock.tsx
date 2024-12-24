import React from 'react'
import { Card } from "@/components/ui/card"
import { useEffect } from "react"
import Prism from "prismjs"

interface CodeBlockProps {
  title?: string
  code: string
  language?: string
}

export const CodeBlock: React.FC<CodeBlockProps> = ({
  title,
  code,
  language = "python"
}) => {
  useEffect(() => {
    Prism.highlightAll()
  }, [code])

  return (
    <div className="space-y-2">
      {title && (
        <h3 className="text-lg font-semibold text-white">{title}</h3>
      )}
      <Card className="bg-black border-[#404040] overflow-hidden">
        <pre className="p-4 overflow-x-auto">
          <code className={`language-${language}`}>
            {code}
          </code>
        </pre>
      </Card>
    </div>
  )
}
