<p align="center">
    <img src="https://github.com/buzz-language/buzz/raw/main/logo.png" alt="buzz" width="204" height="204">
</p>

# 👨‍🚀 buzz

A small/lightweight statically typed scripting language written in Zig

<p align="center">
    <img src="https://github.com/buzz-language/buzz/raw/main/example.png" alt="buzz code example">
</p>

<p align="center">
    <a href="https://buzz-lang.dev">Homepage</a> — <a href="https://discord.gg/VnMdNSdpNV">Discord</a>
</p>

> [!WARNING]
> buzz is in alpha and is **not** ready for any professional or production use

## Features

- Small in size and complexity (just a bit more than Lua though)
- Statically typed
- Unambiguous
- No nonsense coercion
- [Fibers](#fibers)
- JIT compilation with [MIR](https://github.com/vnmakarov/mir)
- Tooling
    - [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz)
    - Helix support thanks to [tree-sitter grammar](https://github.com/buzz-language/tree-sitter-buzz)
    - LSP
    - DAP
    - Formatter
    - Package manager

## How to build and install

### Requirements
- Zig 0.16.0
- Since this is built with Zig, you should be able to build buzz on a wide variety of architectures even though this has only been tested on x86/M1.
- Linux or macOS (Windows support [is coming](https://github.com/buzz-language/buzz/issues/74))
- libc

### Build
1. Clone the project: `git clone https://github.com/buzz-language/buzz <buzz_dir>`
2. Checkout submodules: `git submodule update --init`
3. Build it `zig build -Doptimize=ReleaseSafe`
4. Have fun (with `BUZZ_PATH=./zig-out`)
    - `buzz` launches the REPL
    - `buzz run-script` to run a lone script
    - `buzz init` to start a buzz package
 
### Install

```bash
# install locally at ~/.local
zig build -Doptimize=ReleaseSafe install -p ~/.local

# install globally at /usr/local
sudo zig build -Doptimize=ReleaseSafe install -p /usr/local
```

If you're usage if performance critical (game dev for example), you can build using `-Doptimize=ReleaseFast`.

Remember to modify PATH to include the `bin` directory where it is installed. For example, `export PATH=PATH:/home/xxx/.local/bin`. You can then run buzz with `buzz <myscript.buzz>`. Or you can simply run `buzz` to start the REPL.
