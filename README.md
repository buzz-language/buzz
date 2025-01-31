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

## Features

- Small in size and complexity (just a bit more than Lua though)
- Statically typed
- Unambiguous
- No nonsense coercion
- [Fibers](#fibers)
- JIT compilation with [MIR](https://github.com/vnmakarov/mir)
- Tooling
    - Generate doc from docblocks (planned)
    - [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz)
        - Syntax highlighting
        - LSP (in progress)
        - Debugger and DAP (planned)

## How to build and install

_Latest zig version supported: 0.14.0-dev.2989+bf6ee7cb3_

### Requirements
- Since this is built with Zig, you should be able to build buzz on a wide variety of architectures even though this has only been tested on x86/M1.
- Linux or macOS (Windows support [is coming](https://github.com/buzz-language/buzz/issues/74))
- libc
- zig master

### Build
1. Clone the project: `git clone https://github.com/buzz-language/buzz <buzz_dir>`
2. Checkout submodules: `git submodule update --init`
3. Have fun: `zig build run -- <myscript.buzz>` to run a script  or `zig build run` to start the REPL

### Install

```bash
# install locally at ~/.local
zig build -Doptimize=ReleaseSafe install -p ~/.local

# install globally at /usr/local
sudo zig build -Doptimize=ReleaseSafe install -p /usr/local
```

If you're usage if performance critical (game dev for example), you can build using `-Doptimize=ReleaseFast`.

Remember to modify PATH to include the `bin` directory where it is installed. For example, `export PATH=PATH:/home/xxx/.local/bin`. You can then run buzz with `buzz <myscript.buzz>`. Or you can simply run `buzz` to start the REPL.

Additionally, install the [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz) to get syntax highlighting. If you don't use VS Code but your editor supports [TextMate grammar files](https://github.com/buzz-language/code/blob/main/syntaxes/buzz.tmLanguage.json), you can use that.
