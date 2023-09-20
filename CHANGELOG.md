# Unreleased

## Added

- FFI (https://github.com/buzz-language/buzz/issues/109)
    - Uses MIR to generate wrappers around imported functions and to generate getters and setters to struct fields
    - New `zdef` statement to declare and bind foreign functions and struct using zig code
    - New `ffi` std lib
    - When you need a pointer to something you can use the `Buffer` std lib object. Added:
        - `Buffer.writeZ`, `Buffer.readZ`, `Buffer.writeStruct`, `Buffer.readStruct`
        - `Buffer.ptr`, `Buffer.len`
- New (fancy) error reporter
- Errors have now an associated code
- `os.sleep`
- First class types (https://github.com/buzz-language/buzz/issues/21)
    - Type can be passed around as values like so: `<str>`
    - New `typeof` operator returns type of any value: `typeof "hello"` -> `<str>`
- Delimiters for non-standard identifiers (https://github.com/buzz-language/buzz/issues/138)
- Collectors (https://github.com/buzz-language/buzz/issues/2): if an `object` has a `fun collect() > void` method, it will be called before an instance of this object is collected by the garbage collector
- Helpers around `ud`
    - `std.toUd`, returns userdata from an int or float
    - `bz_valueToObjUserData`
    - `bz_getUserDataPtr`
    - `Buffer.readUserData`, `Buffer.writeUserData`
- `std.serialize` takes any buzz value and return a serializable version of it (objects become maps, etc.) provided the data is has no circular reference and does not contain not serializable values (functions, fibers, etc.)
- UTF8 helpers: `str.utf8Len`, `str.utf8Codepoints`, `str.utf8Valid`
- New integer literal for single chars: `'A' == 65`
- Compiler will warn you when a local or global is never used or when an expression value is discarded. To silence those warnings you can use the `_ = <expression>` or name the local/global `_`.
- `std.currentFiber`, `fiber.isMain`

## Changed

- `json` lib is renamed `serialize`
- `Json` now returns a `Boxed` object (which can be reused in other contexts than JSON)
- Identifiers can now have `_` since pattern delimiters have changed
- Changed pattern delimiters (https://github.com/buzz-language/buzz/issues/165)
- `list.append` does not return the appended value anymore

## Fixed

- Some bugs `any`
- Runtime error stack trace was wrong
- Local name checking failed in some instances

# 0.2.0 (07-26-2023)

## Added

- buzz has a homepage: [https://buzz-lang.dev](https://buzz-lang.dev), a [Discord](https://discord.gg/VnMdNSdpNV) and an actual [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz)
- JIT compiler powered by [MIR](https://github.com/vnmakarov/mir)
- [mimalloc](https://github.com/microsoft/mimalloc) as the main allocator
- Inline if
- Ranges
- `any` type
- `as?` operator to safely cast something
- `http` standard lib (with an HTTP client)
- `crypto` standard lib (in progress)
- `list.pop`, `list.insert`, `list.forEach`, `list.reduce`, `list.filter`, `list.map`, `list.reduce`, `list.sort`
- `pattern.replace`, `pattern.replaceAll`
- `std.random`
- `bz_call` allows a native function to call a buzz function
- `--ast` will dump a script AST in JSON
- `--check` will check that a script compiles whitout running it
- Shebang comment is allowed (`#!/usr/bin/env buzz`) at the start of a script

## Changed
- Main function signature must either be `fun main([str] args) > void` or `fun main([str] args) > int`, plus any required errors
- Numbers are splitted in two types: `int` (32 bits integers) and `float` (64 bits floating points)
- Some performance related changes
    - VM uses tail calls to dispatch opcode instead of a big switch
    - More specialized opcodes to avoid checking types at runtime
    - Some minor things...
- Key can be omitted in `foreach` statements
- buzz wil now search a library in a list of common directories
- `export` can prefix declarations

## Fixed

Too many to count...

# 0.1.0 (11-18-2022)

- static type system
- null safety
- "error safety": much like zig, function signatures must specify which errors can be raised and error must be handled
- objects (struct-like, no inheritance)
- anonymous objects
- protocols
- enums
- lists and maps
- fibers (coroutines)
- userdata (pointer to foreign data wrapped in a buzz value)
- first-class citizen functions
- arrow functions
- generics
- pcre regex
- zic/c interop
- syntax highlighting via vs code extension or by using the TextMate grammar file
- minimal std lib: buffer, fs, io, os, math, gc, debug