<p align="center">
    <img src="https://github.com/giann/buzz/raw/main/logo.png" alt="buzz" width="204" height="204">
</p>

# 👨‍🚀 buzz
Attempt to make a small/lightweight typed scripting language

# Goal
- A small scripting language but with strict typing and with the primary goal to avoid any ambiguity both for the programmer and the compiler.
- Close integration with the system (maybe having it be a candidate for shell scripting?)
- Easy string manipulations
- Could run in a VM at first, and then target LLVM (but how to do GC then?)

# TODO
- [ ] 16 bytes instructions (at least to not be limited to 255 constants, globals, locals, etc.)
- [ ] Register based op codes like lua?

# Resources
- http://www.craftinginterpreters.com/contents.html
- treesitter: https://github.com/tree-sitter/tree-sitter
