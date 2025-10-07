CRUSH.md

Build, lint, test
- Build: zig build
- Run REPL: zig build run
- Run file: zig build run -- path/to/file.buzz
- Install (user): zig build -Doptimize=ReleaseSafe install -p ~/.local
- Install (system): sudo zig build -Doptimize=ReleaseSafe install -p /usr/local
- Clean: rm -rf .zig-cache zig-out
- Typecheck/lsp: zls (use your editor’s Zig LSP); no separate typecheck script
- Format Zig: zig fmt .
- Web WASM tooling: npm install; node src/wasm.ts (uses esbuild+ts)
- Run single test: zig build run -- tests/068-testing.buzz --name "<test_name>" (tests are Buzz scripts; prefer targeted script runs)
- Example single test: zig build run -- tests/023-std.buzz

Project conventions
- Language: Zig 0.15.x; entry in src/main.zig; build via build.zig; vendor deps under vendors/
- Buzz tests: each .buzz file in tests/ acts as an executable scenario; keep them self-contained
- Imports: Zig uses relative imports (const x = @import("file.zig")); keep import blocks at top, grouped std, vendors, local
- Formatting: run zig fmt before commits; keep line width conventional (~100) and avoid trailing spaces
- Types: prefer explicit types in public APIs; use var/const deliberately; avoid anyopaque unless necessary
- Naming: snake_case for Zig functions/vars, TitleCase for types, SCREAMING_SNAKE for compile-time consts
- Errors: use Zig error sets; return !T and propagate with try/errdefer; avoid panic except unrecoverable
- Memory: use provided allocators; pair alloc/free; errdefer for cleanup; avoid leaks in hot paths
- Concurrency: fibers exist at Buzz level; in Zig keep APIs non-blocking where possible
- Logging/Reporting: use Reporter.zig utilities; avoid printing in libraries
- Performance: prefer slices over arrays; minimize copies; use inline and comptime judiciously

Notes
- No .cursor or Copilot rules found
- VS Code: install Buzz extension and Zig LSP for best experience
- Security: never commit secrets; do not log keys or tokens
