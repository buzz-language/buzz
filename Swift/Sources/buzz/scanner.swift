enum LexicalError {
    case unterminatedString(line: Int, column: Int)
    case unexpectedCharacter(line: Int, column: Int)
}

final class Scanner {
    let source: String
    
    var errors: [LexicalError] = []
    private var tokens: [Token] = []
    private var current: (start: Int, line: Int, column: Int, offset: Int) = (0, 0, 0, 0)
    
    init(source: String) {
        self.source = source
    }
    
    func scan() -> [Token] {
        while (!isEOF()) {
            current.start = current.offset
            scanToken()
        }
        
        tokens.append(Token(token: .eof, lexeme: "\0", literal: "\0" as Any, line: current.line, column: current.column))
        return tokens
    }
    
    private func scanToken() {
        let char = advance()
        switch (char) {
        case Character("|"):
            // Ignore comment
            while (peek() != Character("\n") && !isEOF()) {
                advance();
            }
        case Character("["):
            addToken(tokenType: .leftBracket)
        case Character("]"):
            addToken(tokenType: .rightBracket)
        case Character("("):
            addToken(tokenType: .leftParen)
        case Character(")"):
            addToken(tokenType: .rightParen)
        case Character("{"):
            addToken(tokenType: .leftBrace)
        case Character("}"):
            addToken(tokenType: .rightBrace)
        case Character(","):
            addToken(tokenType: .comma)
        case Character(";"):
            addToken(tokenType: .semicolon)
        case Character(">"):
            addToken(tokenType: match(Character("=")) ? .greaterEqual : .greater)
        case Character("<"):
            addToken(tokenType: match(Character("=")) ? .lessEqual : .less)
        case Character("+"):
            if match(Character("+")) {
                addToken(tokenType: .increment)
            } else if match(Character("=")) {
                addToken(tokenType: .plusEqual)
            } else {
                addToken(tokenType: .plus)
            }
        case Character("-"):
            if match(Character(">")) {
                addToken(tokenType: .arrow)
            } else if match(Character("-")) {
                addToken(tokenType: .decrement)
            } else if match(Character("=")) {
                addToken(tokenType: .minusEqual)
            } else {
                addToken(tokenType: .minus)
            }
        case Character("*"):
            if match(Character("=")) {
                addToken(tokenType: .starEqual)
            } else {
                addToken(tokenType: .star)
            }
        case Character("/"):
            addToken(tokenType: match(Character("=")) ? .slashEqual : .slash)
        case Character("%"):
            addToken(tokenType: .percent)
        case Character("?"):
            addToken(tokenType: match(Character("?")) ? .questionQuestion : .question)
        case Character("!"):
            addToken(tokenType: match(Character("=")) ? .bangEqual : .bang)
        case Character(":"):
            addToken(tokenType: .colon)
        case Character("="):
            addToken(tokenType: match(Character("=")) ? .equal : .equalEqual)
        case Character("\n"):
            current.line += 1
            current.column = 0
        case Character("\""):
            string()
        default:
            if char.isLetter {
                identifier()
            } else if char.isNumber {
                number()
            } else {
                errors.append(LexicalError.unexpectedCharacter(line: current.line, column: current.column))
            }
        }
    }
    
    private func identifier() {
        while (peek().isLetter) {
            advance()
        }
        
        let start = source.index(source.startIndex, offsetBy: current.start)
        let offset = source.index(source.startIndex, offsetBy: current.offset)
        let literal = String(source[start..<offset])
        
        for tokenType in TokenType.keywords {
            if tokenType.rawValue == literal {
                addToken(tokenType: tokenType)
                
                return
            }
        }
        
        addToken(tokenType: .identifier)
        return
    }
    
    private func number() {
        while (peek().isNumber) {
            advance()
        }
        
        if (peek() == Character(".") && peekNext().isNumber) {
            advance() // Consume .
            
            while (peek().isNumber) {
                advance()
            }
        }
        
        let start = source.index(source.startIndex, offsetBy: current.start)
        let offset = source.index(source.startIndex, offsetBy: current.offset)
        let literal = source[start..<offset]
        
        addToken(tokenType: .number, literal: Double(literal)! as Any)
    }
    
    private func string() {
        while (peek() != Character("\"") && !isEOF()) {
            if peek() == Character("\n") {
                errors.append(.unterminatedString(line: current.line, column: current.column))
            }
            
            advance()
        }
        
        if isEOF() {
            errors.append(.unterminatedString(line: current.line, column: current.column))
        } else {
            // Closing "
            advance()
        }
        
        // Trim quotes
        let start = source.index(source.startIndex, offsetBy: current.start + 1)
        let offset = source.index(source.startIndex, offsetBy: current.offset - 1)
        addToken(tokenType: .string, literal: String(source[start..<offset]) as Any)
    }
    
    private func isEOF() -> Bool {
        return current.offset >= source.count
    }
    
    private func peek() -> Character {
        if isEOF() {
            return Character("\0");
        }
        
        return charAt(current.offset)
    }
    
    private func peekNext() -> Character {
        if (current.offset + 1 > source.count) {
            return Character("\0")
        }
        
        return charAt(current.offset + 1)
    }
    
    @discardableResult
    private func advance() -> Character {
        let char = charAt(current.offset)
        current.offset += 1
        current.column += 1
        
        return char
    }
    
    private func match(_ expected: Character) -> Bool {
        if isEOF() {
            return false
        }
        
        if charAt(current.offset) != expected {
            return false
        }
        
        current.offset += 1
        return true
    }
    
    private func addToken(tokenType: TokenType, literal: Any? = nil) {
        let start = source.index(source.startIndex, offsetBy: current.start)
        let offset = source.index(source.startIndex, offsetBy: current.offset)
        
        let token = Token(
            token: tokenType,
            lexeme: String(source[start..<offset]),
            literal: literal,
            line: current.line,
            column: current.column
        )
        
        tokens.append(
            token
        )
        
        print("\(String(format: "%03d", token.line + 1)):\(String(format: "%03d", token.column + 1)) |\t\(token.token.rawValue.uppercased())\t\"\(token.lexeme)\"\(token.literal != nil ? "\t`\(token.literal!)`" : "")")
    }
    
    fileprivate func charAt(_ index: Int) -> Character {
        return source[source.index(source.startIndex, offsetBy: current.offset)]
    }
}
