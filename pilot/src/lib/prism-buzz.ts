import Prism from 'prismjs'

// Define Buzz language syntax for Prism.js
Prism.languages.buzz = {
    'comment': {
        pattern: /#.*/,
        greedy: true
    },
    'decorator': {
        pattern: /@[\w\d_]+/,
        alias: 'keyword'
    },
    'string': {
        pattern: /("|')(?:\\.|(?!\1)[^\\\r\n])*\1/,
        greedy: true
    },
    'class-name': {
        pattern: /\b[A-Z]\w*\b/,
        greedy: true
    },
    'keyword': {
        pattern: /\b(?:contract|def|if|else|while|for|in|return|self|import|from|as|True|False|None)\b/,
        greedy: true
    },
    'builtin': {
        pattern: /\b(?:u8|u16|u32|u64|i8|i16|i32|i64|f32|f64|bool|str|List|Dict|Set|TokenAccount|PublicKey|Account)\b/,
        greedy: true
    },
    'function': {
        pattern: /\b\w+(?=\s*\()/,
        greedy: true
    },
    'number': {
        pattern: /(?:\b\d+\.?\d*|\B\.\d+)(?:e[+-]?\d+)?/i,
        greedy: true
    },
    'operator': /[-+*/%=]=?|!=|<=?|>=?|and|or|not|\bis\b|\bin\b/,
    'punctuation': /[.,;:()[\]{}]/
}
