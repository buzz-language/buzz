# Unreleased

## Added
- REPL (https://github.com/buzz-language/buzz/issues/17) available by running buzz without any argument
- WASM build (https://github.com/buzz-language/buzz/issues/142) and [web REPL](https://buzz-lang.dev/repl.html)
- Function argument names and object property names can be ommitted if the provided value is a named variable with the same name (https://github.com/buzz-language/buzz/issues/204)
```buzz
object Person {
    str name,
    str lastName,
}

const name = "Joe";
const lastName = "Doe";
```
before:
```buzz
const person = Person {
    name: name,
    lastName: lastName,
};
```
after:
```buzz
const person = Person {
    name,
    lastName,
};
```
- Sandboxing build options `memory_limit` and `cycle_limit` (https://github.com/buzz-language/buzz/issues/182)
- Block expression (https://github.com/buzz-language/buzz/issues/105):
```buzz
var value = from {
    | ... 

    out result;
}
```
- `recursive_call_limit` build option limit recursive calls (default to 200)

## Changed
- Map type notation has changed from `{K, V}` to `{K: V}`. Similarly map expression with specified typed went from `{<K, V>, ...}` to `{<K: V>, ...}` (https://github.com/buzz-language/buzz/issues/253)
- `File.readLine`, `File.readAll`, `Socket.readLine`, `Socket.readAll` have now an optional `maxSize` argument
- Tail call optimization (https://github.com/buzz-language/buzz/issues/9). The effect should be limited for recursive calls since the JIT should kick in pretty quickly in those use cases.
- Empty list and map without a specified type resolve to `[any]`/`{any: any}` unless the variable declaration context provides the type (https://github.com/buzz-language/buzz/issues/86)
- Function yield type is now prefixed with `*>`: `fun willYield() > T > Y?` becomes `fun willYield() > T *> Y?` (https://github.com/buzz-language/buzz/issues/257)

## Fixed

# 0.3.0 (10-14-2023)
## Added

- FFI (https://github.com/buzz-language/buzz/issues/109)
    - Uses MIR to generate wrappers around imported functions and to generate getters and setters to struct fields
    - New `zdef` statement to declare and bind foreign functions and struct using zig code
    - New `ffi` std lib
    - When you need a pointer to something you can use the `Buffer` std lib object. Added:
        - `Buffer.writeZ`, `Buffer.readZ`, `Buffer.writeStruct`, `Buffer.readStruct`
        - `Buffer.ptr`, `Buffer.len`
- New (fancy) error reporter (https://github.com/buzz-language/buzz/issues/153)
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
- UTF8 helpers: `str.utf8Len`, `str.utf8Codepoints`, `str.utf8Valid` (https://github.com/buzz-language/buzz/issues/39)
- New integer literal for single chars: `'A' == 65` (https://github.com/buzz-language/buzz/issues/172)
- Compiler will warn you when a local or global is never used or when an expression value is discarded. To silence those warnings you can use the `_ = <expression>` or name the local/global `_`.
- `std.currentFiber`, `fiber.isMain` (https://github.com/buzz-language/buzz/issues/162)
- `map.sort`, `map.forEach`, `map.map`, `map.filter`, `map.reduce`, `map.diff`, `map.intersect`, `map.clone` (https://github.com/buzz-language/buzz/issues/110)
- `list.clone` (https://github.com/buzz-language/buzz/issues/110)
- Number literals can embed `_`: `1_000_000.300_245` (https://github.com/buzz-language/buzz/issues/163)
- Type can be inferred when declaring a variable/constant with the `var` or `const` keyword: `var something = "hello"` (https://github.com/buzz-language/buzz/issues/194)
- Objects can have generic types  (https://github.com/buzz-language/buzz/issues/82)
- Draft of the testing std lib (https://github.com/buzz-language/buzz/issues/129)
- `File.isTTY`
- `fs.exists`
- Functions annotated with a comment of the form `|| @hot` will always be JIT compiled

## Changed

- `json` lib is renamed `serialize`
- `Json` now returns a `Boxed` object (which can be reused in other contexts than JSON)
- Identifiers can now have `_` since pattern delimiters have changed
- Changed pattern delimiters (https://github.com/buzz-language/buzz/issues/165)
- `list.append` does not return the appended value anymore
- Generic types syntax changed from `myFunction(<K,V>, ...)` to `myFunction::<K,V>(...)`
- Nullable object fields and nullable variables have a `null` initial value if none is provided
- Migrated to pcre2 which is now a submodule built by build.zig
- Mimalloc is now a submodule built by build.zig

## Fixed

- Some bugs `any`
- Runtime error stack trace was wrong
- Local name checking failed in some instances
- Compiler would not force you to give variables an initial value
- Compiler would crash after raising some errors
- Float operation were sometimes wrong
- Catch clause were sometimes not reached in a JIT compiled function
- Stacktraces of errors reported from within a fiber were wrong
- `catch (any error)` was not considered as catching all possible errors by the compiler
- Full GC sweep were never triggered

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