# AGENTS.md

Guidance for coding agents working in this repository.

## Project

Buzz is a small/lightweight statically typed scripting language written in Zig.

Main Buzz implementation subsystems:

- Scanner: `src/Scanner.zig`
- Parser and AST: `src/Parser.zig`, `src/Ast.zig`
- Code generation and bytecode: `src/Codegen.zig`, `src/Chunk.zig`
- Runtime values and objects: `src/value.zig`, `src/obj.zig`
- VM: `src/vm.zig`
- GC: `src/GC.zig`
- JIT: `src/Jit.zig`, `src/mir.zig`
- FFI: `src/FFI.zig`, `src/lib/buzz_ffi.zig`, `src/lib/ffi.buzz`

Tooling subsystems:

- Debugger: `src/Debugger.zig`
- LSP: `src/lsp.zig`
- Formatter: `src/renderer.zig`, `src/tests/fmt.zig`
- REPL: `src/repl.zig`, `src/repl.html`, `src/wasm_repl.zig`

FFI, Debugger, and LSP are immature. Treat changes there as higher risk.

## Working Tree Policy

- Never commit changes.
- Before editing, check the working tree.
- If the working tree is clean, make the requested changes normally.
- If the working tree only has untracked files, edits are allowed. Do not overwrite or delete unrelated untracked files.
- If the working tree has tracked modifications or staged changes that were not initiated by the current agent/session, continue investigation read-only, then show the diff you would have applied and say that no files were modified because the working copy was not clean.
- If tracked modifications were initiated by the current agent/session for the active task, the agent may continue editing those files and related files needed to finish the same task.
- Do not overwrite or revert user changes.

Agents may delete files they generated during their own work. Do not delete files that were already tracked by git.

## Do Not Touch

- Never edit `vendors/`.
- Do not edit generated or local build artifacts, including `zig-cache/`, `zig-out/`, `dist/`, `build/`, `node_modules/`, object files, dylibs, tarballs, and platform package directories.
- Only edit `package.json`, `package-lock.json`, or WASM/frontend files when the task is specifically web/WASM-related.
- Do not download or install dependencies.
- If submodules are missing, `git submodule update --init` is acceptable.
- Do not run `scripts/perf_compare_commits.py`.

## Common Commands

Quick compile check:

```sh
zig build check
```

`zig build check` is useful after small changes, but it does not cover every executable in the project.

Exhaustive compile check:

```sh
zig build
```

Run all important behavior tests:

```sh
zig build test-behavior
```

Run formatter tests:

```sh
zig build test
```

`zig build test` currently tests the Buzz formatter. `zig build test-behavior` is the most important test suite for language behavior.

Run a single Buzz file containing `test` blocks:

```sh
zig build run -- -t tests/behavior/descriptive-name.buzz
```

Run a regular Buzz script with a `main` function:

```sh
zig build run -- path/to/file.buzz
```

Prefer `zig build run` because it builds Buzz and then runs the script. Do not bother setting `BUZZ_PATH` manually.

Check Zig formatting:

```sh
zig fmt --check src/*.zig src/**/*.zig
```

Only Zig files need formatting checks for now.

## Validation Expectations

- If Zig code was touched, run `zig build` before finishing.
- For language behavior changes, run `zig build test-behavior`.
- For formatter changes, run `zig build test`.
- For focused behavior work, first run the relevant file with `zig build run -- -t <testfile.buzz>`, then run the broader suite when appropriate.
- Tests are not expected to be flaky or platform-specific.
- Supported platforms are macOS and Linux. Do not spend effort on Windows compatibility unless explicitly asked.
- Do not test WASM unless the task is specifically WASM-related.

## Tests

- New language behavior tests go in `tests/behavior/` with a descriptive `.buzz` filename.
- Existing numbered behavior test filenames are historical; new tests do not need a sequence number.
- Every language feature or bug fix should include a behavior test unless there is a clear reason not to.
- Parser, typechecker, compiler, and crash fixes should include a reduced regression test when possible.
- Do not add fuzzed crashes yourself.
- Tests that intentionally trigger a Buzz compile error belong in `tests/compile_errors/`.
- Compile-error tests must have a first-line comment containing the expected Buzz error message.

## Style

- Keep patches minimal and localized.
- Preserve existing naming and conventions, even if they look inconsistent, unless the task is specifically cleanup.
- Do not break logic into small functions unless those small functions are used more than once.
- Zig variables use snake_case.
- Zig types and functions use camelCase.
- Comments are encouraged for compiler/runtime logic, but keep them concise and useful.
- Any non-trivial code added must be properly commented in the code. Comments should explain intent, invariants, or tricky control flow, not restate obvious assignments.
- Any new Zig file under `src/` must start with a file docblock (`//! ...`) describing the general role of the file.
- Any new functions, structs, objects, properties, and enums introduced in Zig or Buzz code must have a docblock.

## Runtime And GC Rules

- Strings and types are interned.
- Strings generally come from `gc.copyString`.
- Types generally come from `gc.type_registry.getTypeDef`.
- When doing work while the VM is running, temporary Buzz objects must be pushed on the stack so they are not collected by the GC.

## JIT And Debug Flags

Leave JIT enabled by default. Disable it only to pinpoint an issue.

Useful build flags include:

- `-Ddebug=true` for AST, bytecode, and other debug output.
- `-Ddebug_stack=true` and `-Ddebug_current_instruction=true` for VM instruction/stack tracing.
- `-Ddebug_placeholders=true` for placeholder resolution.
- `-Ddebug_type_registry=true` for type registry debugging.
- `-Dgc_debug=true`, `-Dgc_debug_light=true`, and `-Dgc_debug_access=true` for GC debugging.
- `-Djit_debug=true` for JIT debugging.
- `-Djit=false` to disable JIT while isolating runtime issues.
- `-Djit_asynchronous=<bool>` controls whether JIT jobs run on the worker thread. Keep it enabled by default unless isolating an async publication issue.
- `-Djit_call_threshold=<int>` is the function call count before a function is considered for JIT compilation.
- `-Djit_score_threshold=<int>` is the function score gate. Function score is call count multiplied by chunk complexity.
- `-Djit_hotspot_threshold=<int>` is the loop/hotspot execution count before a hotspot is considered for JIT compilation.
- `-Djit_hotspot_score_threshold=<int>` is the hotspot score gate. Hotspot score is execution count multiplied by AST hotspot complexity.
- `-Dcycle_limit=<int>` to limit bytecode execution, noting that it disables JIT compilation.
- `-Dmemory_limit=<int>` to reproduce or bound memory behavior.

Current default JIT thresholds are intentionally conservative: call threshold `1024`, function score threshold `65535`, hotspot threshold `256`, hotspot score threshold `65535`, async enabled. When tuning, compare against the full `tests/bench` matrix instead of optimizing a single benchmark:

```sh
scripts/jit_bench_matrix.sh quick
scripts/jit_bench_matrix.sh final nojit current sync-current hotspot-only
```

The matrix writes timings and output-hash comparisons under `zig-cache/jit-bench/`.

## Debugging Guidance

- For parser/typechecker/codegen issues, prefer the smallest `.buzz` regression test that reproduces the behavior.
- For VM or GC issues, check object lifetime and stack rooting before changing collection behavior.
- For JIT issues, compare behavior with JIT enabled and disabled before changing JIT code.
- For FFI issues, be careful with pointer lifetimes and ownership across the Zig/Buzz boundary.
- If a change may affect performance, mention that in the final response when relevant, but do not run benchmarks unless explicitly asked.
