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

## Objects
- [ ] Add special array of palceholders in ObjectCompiler
- [ ] If inside object declaration, create property/method placeholders (should only occur when using `this`) in that array
- [ ] We know at resolving if they should be a method or a property

Placeholder in globals = slot in globals array
Placeholder in object = named slot in properties hash map

# Resources
- http://www.craftinginterpreters.com/contents.html
