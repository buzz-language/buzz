# buzz
(temporary name) attempt to make a small/lightweight typed scripting language

# Goal
- A small scripting language but with strict typing and with the primary goal to avoid any ambiguity both for the programmer and the compiler.
- Close integration with the system (maybe having it be a candidate for shell scripting?)
- Easy string manipulations
- Could run in a VM at first, and then target LLVM (but how to do GC then?)

# Ideas
- Use http://c9x.me/compile/doc/il.html ?

# Resources
- http://www.craftinginterpreters.com/contents.html
- utf8: https://github.com/jecolon/ziglyph
- treesitter: https://github.com/tree-sitter/tree-sitter

# How to do type checking
- At comptime: we maintain a map of actual ObjTypeDefs and refer to it to do type checking. We populate it with all possible builtin types (str, num, {str, num}, {num, str}, etc), then, once we have a full class/object/enum declaration, we should have a ObjTypeDef to put there
- At runtime: types (builtin and userland) are stored in the global storage but prefixed with a non-user-authorized character ($str, $num, $MyClass), that way we can do a variable declaration like so

OP_DEFINE_GLOBAL/LOCAL
<index to constant table for type name>
<index to constant table for global/local name>

So all type resolution at runtime is just a global name resolution.

Do we do some type checking at runtime? Or de we trust the compiler did all the necessary type checking at comptime? Since i put first-class types aside for now, we should not be able to refer to types by something else than a constant name. So no all type checking should be static.
Some instructions will end up doing some type checking but only to figure out what to do: OP_ADD will check if operands are numbers or strings etc.