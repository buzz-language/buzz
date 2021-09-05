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

# FIX
- [ ] users types are interned but since there's no global, the first class named A will shadow any new class with the same name

# Resources
- http://www.craftinginterpreters.com/contents.html
