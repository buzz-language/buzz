```bnf
program        -> (<statement> | <declaration>)+ EOF

# Declarations
declaration    -> <variable> | <function> | <arrow_function> | <anon_function> | <class> | <object> | <enumeration>

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
expression     -> <assignment> | <is>
assignment     -> ((<call> | <reference> | <primary>)(\?|!)?\.)?<identifier> (+|-|/|*)= (<assignment> | <is>)
is             -> <null_or> (is <null_or>)*
null_or        -> <logic_or> (\?\? <logic_or>)*
logic_or       -> <logic_and> (or <logic_and>)*
logic_and      -> <logic_xor> (and <logic_xor>)*
logic_xor      -> <comparison> (and <comparison>)*
equality       -> <comparison> ((==|!=) <comparison>)*
comparison     -> <term> ((>|<|=>|<=) <term>)*
term           -> <shift> ((+|-) <shift>)*
shift          -> <factor> ((>>|<<) <factor>)*
factor         -> <operand> ((/|*|%) <operand>)*
operand        -> <unary> | <call> | <reference> | <subscript> | <primary>
unary          -> (!|-|--|++)(<call> | <reference> | <subscript> | <primary>) | (<call> | <reference> | <subscript> | <primary>)(--|++)
call           -> <primary>\(<arguments>?\)
reference      -> <primary>(\?|!)?\.<identifier>
subscript      -> <primary>(\?|!)?\[<expression>\]
primary        -> <literal> | \(<expression>\) | super\.<identifier> | <identifier>

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