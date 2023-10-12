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
    - [Generate doc from docblocks (in progress)](https://github.com/buzz-language/buzz/blob/main/doc/index.md)
    - [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz)
        - Syntax highlighting
        - LSP (in progress)
        - Debugger and DAP (planned)

## How to install

```bash
# install locally at ~/.local
zig build -Doptimize=ReleaseSafe install -p ~/.local

# install globally at /usr/local
sudo zig build -Doptimize=ReleaseSafe install -p /usr/local
```

Then, you can run buzz with `buzz <myscript.buzz>`. Remember to modify PATH to include the `bin` directory where it is installed. For example, `export PATH=PATH:/home/xxx/.local/bin`.

Additionally, install the [VS Code extension](https://marketplace.visualstudio.com/items?itemName=giann.buzz) to get syntax highlighting. If you don't use VS Code but your editor supports [TextMate grammar files](https://github.com/buzz-language/code/blob/main/syntaxes/buzz.tmLanguage.json), you can use that.

## How to build/develop

### Requirements
- Since this is built with Zig, you should be able to build buzz on a wide variety of architectures even though this has only been tested on x86/M1.
- Linux or macOS (not much work is needed to make it work on [Windows](https://github.com/buzz-language/buzz/issues/74))
- libc
- [mimalloc](https://github.com/microsoft/mimalloc) (can be turned off by building buzz with `-Dmimalloc=false`)
- zig master

### Steps
1. Clone the project: `git clone https://github.com/buzz-language/buzz <buzz_dir>`
2. Checkout submodules: `git submodule update --init`
3. Build MIR:
```bash
cd mir
make
```
4. Have fun: `zig build run -- <myscript.buzz>`