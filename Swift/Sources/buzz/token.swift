enum TokenType: String {
    case pipe             // |
    case leftBracket      // [
    case rightBracket     // ]
    case leftParen        // (
    case rightParen       // )
    case leftBrace        // {
    case rightBrace       // }
    case comma            // ,
    case semicolon        // ;
    case greater          // >
    case less             // <
    case plus             // +
    case minus            // -
    case star             // *
    case slash            // /
    case percent          // %
    case question         // ?
    case bang             // !
    case colon            // :
    case equal            // =
    case equalEqual       // ==
    case bangEqual        // !=
    case greaterEqual     // >=
    case lessEqual        // <=
    case questionQuestion // ??
    case plusEqual        // +=
    case minusEqual       // -=
    case starEqual        // *=
    case slashEqual       // /=
    case increment        // ++
    case decrement        // --
    case arrow            // ->
    case `true`           // true
    case `false`          // false
    case null             // null
    case or               // or
    case and              // and
    case `return`         // return
    case `if`             // if
    case `else`           // else
    case `while`          // while
    case `for`            // for
    case `switch`         // switch
    case `break`          // break
    case `continue`       // continue
    case `default`        // default
    case fun              // fun
    case `in`             // in
    case number           // 123
    case string           // "hello"
    case identifier       // anIdentifier
    case eof              // EOF
    
    
    static let keywords: [TokenType] = [
        .true,
        .false,
        .null,
        .or,
        .and,
        .return,
        .if,
        .else,
        .while,
        .for,
        .switch,
        .break,
        .continue,
        .default,
        .fun,
        .in,
    ]
}

struct Token {
    let token: TokenType
    let lexeme: String
    let literal: Any?
    let line: Int
    let column: Int
}
