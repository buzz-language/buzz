<p align="center">
    <img src="https://github.com/giann/buzz/raw/main/logo.png" alt="buzz" width="204" height="204">
</p>

# 👨‍🚀 buzz
Attempt to make a small/lightweight typed scripting language written in Zig

# Goal
- Small in size and complexity (just a bit more than Lua though)
- Strict typing
- Unambiguous
- No nonsense coercion
- TBD: coroutines?

# TODO
- [ ] 16 bytes instructions (at least to not be limited to 255 constants, globals, locals, etc.)
- [ ] Register based op codes like lua?

## _Declarative space_
Top-level of script and class body are _declaractive space_: free order declarations.
To solve this:
1. Do a first _shallow_ pass of the body, making note of variables, functions, class, objects and enum BUT whithout parsing their body (we skip them by counting `{}` pairs). And in the case of funtion, we skip their argument list too (because they can refer to things declared later).
3. Then reset the scanner to the start of the body and do the usual parsing. Now, any type we can't resolve by looking at already defined types, we can find in the declaraction skeletons we kept.

Top level allows only declarations of any type.
Class and Objects bodies allow only function and variable declarations.

# FIX
- [ ] users types are interned but since there's no global, the first class named A will shadow any new class with the same name

# Resources
- http://www.craftinginterpreters.com/contents.html
