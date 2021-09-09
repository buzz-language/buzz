```bnf
program        -> <declaration>+ EOF

# Declarations
declaration    -> <statement> | <variable> | <function> | <arrow_function> | <anon_function> | <class> | <object> | <enumeration>

variable       -> <type> <identifier> (= <expression>)? ;
function       -> fun <identifier>\(<parameters>?\) (> <type>)? <block>
arrow_function -> \(<paramaters>?\) (> <type>)? -> <expression>
anon_function  -> \(<paramaters>?\) (> <type>)? <block>
class          -> class <identifier> (< <identifier>)? <body>
object         -> object <identifier> <body>
enumeration    -> enum <type>? <identifier> { (<identifier>,)+ }

# Statements
statement      -> <expression>; | <foreach> | <for> | <while> | <until> | <if> | <switch> | <return> | break;

foreach        -> for \(<variable>(,<variable)?) in (<expression>\) <block>
for            -> for \((<variable|<expression>)?; <expression>?; <expression>?\) <block>
while          -> while \(<expression>\) <block>
until          -> do <block> until \(<expression>\)
if             -> if \(<expression>\) <block> (else <if>)* (else <block>)?
switch         -> switch \(<expression>\) { (<expression>: <statement>*)+ (default: <statement>+)? }
return         -> return <expression>?;

# Expressions (the fuzzy part tbh)
15  expression     -> <assignment> | <is>
14  assignment     -> ((<call> | <reference> | <primary>)(\?|!)?\.)?<identifier> (+|-|/|*)= (<assignment> | <is>)
13  is             -> <as> (is <as>)*
12  as             -> <null_or> (as <null_or>)*
11  null_or        -> <logic_or> (\?\? <logic_or>)*
10  logic_or       -> <logic_and> (or <logic_and>)*
9   logic_and      -> <logic_xor> (and <logic_xor>)*
8   logic_xor      -> <comparison> (xor <comparison>)*
7   equality       -> <comparison> ((==|!=) <comparison>)*
6   comparison     -> <term> ((>|<|=>|<=) <term>)*
5   term           -> <shift> ((+|-) <shift>)*
4   shift          -> <factor> ((>>|<<) <factor>)*
3   factor         -> <operand> ((/|*|%) <operand>)*
2   operand        -> <unary> | <call> | <reference> | <subscript> | <primary>
    unary          -> (!|-|--|++)(<call> | <reference> | <subscript> | <primary>) | (<call> | <reference> | <subscript> | <primary>)(--|++)
1   call           -> <primary>\(<arguments>?\)
    reference      -> <primary>(\?|!)?\.<identifier>
    subscript      -> <primary>(\?|!)?\[<expression>\]
0   primary        -> <literal> | \(<expression>\) | super\.<identifier> | <identifier>

literal        -> \b(true|false|null|this)\b | <number> | <string> | <type> | <structure>
structure      -> (<list_type>)?\[<arguments>?\] | (<map_type>)?\{ <literal>: <expression> \}
string         -> "([^"]+ | {<expression>})*"
number         -> ([1-9][0-9]*(\.[0-9]+)? | 0b[0-1]{8} | 0x[0-9A-F]{2})
identifier     -> \b[a-zA-Z_][a-zA-Z0-9_]+\b

# Utility
type           -> \b(str|num|bool|byte|type|<fun_type>|<identifier>|<list_type>|<map_type>)\?\b
fun_type       -> Function(<parameters>?) (> <type>)?
list_type      -> \[<type>\]
map_type       -> \{<type>, <type>\}
block          -> { <statement>* }
body           -> { (<variable>|<function>)* }
parameters     -> <type> <identifier>(, <type> <identifier>)*
arguments      -> <expression>(, <expression>)*
```