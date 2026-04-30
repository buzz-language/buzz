# Buzz Compilation Performance Notes

The main speedup opportunity appears to be in **Parser**, not Codegen. On synthetic no-import compile checks:

`-f` parse/render times were basically the same as `-c` for the top-level cases, so Codegen is not the first bottleneck.

Highest-confidence fixes:

- [x] Root block construction is O(n^2): `src/Parser.zig:1040` rebuilds `body_node.Block` on every top-level declaration. Make it mirror `block()` at `src/Parser.zig:1456`: keep one `ArrayList(Ast.Node.Index)` during the root parse loop, then `toOwnedSlice` once.
- [x] Global lookup/declaration is O(n): `addGlobal`, declaration duplicate checks, and `resolveGlobal` all scan `self.globals` linearly: `src/Parser.zig:1774`, `src/Parser.zig:1878`, `src/Parser.zig:2572`. Add a `StringHashMapUnmanaged` from qualified global name to slot, while keeping the array for ordering. That should take global-heavy compilation from quadratic toward linear.
- [x] `qualifiedName()` allocates even for plain identifiers: `src/Parser.zig:6102`. A fast path for single-token names would reduce allocator pressure in normal expressions.

Secondary Codegen target after Parser: `generateCall()` builds temporary maps/lists and interns names for every call, even common positional calls: `src/Codegen.zig:860`. Add a fast path for "all positional, no defaults, no reorder" calls.

One unrelated note: `buzz -c` currently panics on std-importing bench files because `Check` skips import/dynlib resolution but Codegen can still hit extern/native emission.

## VM Performance Notes

Build used for these numbers:

```text
zig build -Doptimize=ReleaseFast -Djit=false
```

Existing `tests/bench` runs with stdout redirected:

```text
001-btree.buzz 13            real 0.32s
001-btree.buzz 14            real 0.63s
002-merkle.buzz 14           real 0.94s
007-fib.buzz                 real 0.20s
008-for.buzz                 real 0.13s
010-ackermann.buzz 3 9       real 0.81s
011-bubble-sort.buzz 3000    real 0.95s
011-bubble-sort.buzz 5000    real 2.62s
```

Some current bench scripts are not usable as VM timing inputs without fixing their programs first:

```text
003-nbody.buzz               out-of-bound list access in energy()
004-spectral.buzz            division by zero in A(i=0, j=0)
006-fasta.buzz               out-of-bound list access in makeRandomFasta()
```

Temporary runtime-dominated microbenchmarks under `/tmp`:

```text
int while loop, 20M iterations           real 1.29s
function call loop, 5M calls             real 0.46s
object field update loop, 5M updates     real 0.39s
list append/read loop, 3M + 3M ops       real 0.46s
map set/get loop, 500k + 500k ops        real 0.15s
compile checks for these scripts         ~0.00s
```

Highest-confidence VM fixes:

- [x] Avoid repeated `currentFrame()` lookup in the hot dispatch path. `readInstruction()` calls `currentFrame()` (`src/vm.zig:660`), most opcode handlers call `self.currentFrame().?` again when tail-dispatching (for example `src/vm.zig:957`, `src/vm.zig:1166`, `src/vm.zig:3758`), and constants/globals do the same via helpers (`src/vm.zig:685`, `src/vm.zig:593`). Thread the `current_frame` pointer through `readInstruction(frame)`, `readConstant(frame, arg)`, and the next dispatch call for opcodes that do not change frames. Frame-changing opcodes can refresh the pointer explicitly.
- [ ] Split normal dispatch from debug/interrupt dispatch. Every opcode currently passes through `dispatch()` checks for try-catch boundary handling, `cycle_limit`, debugger state, and paused debugger state (`src/vm.zig:840`). Build options remove some checks, but non-debug, non-debugger execution still pays runtime branches around `self.debugger`. A no-debugger dispatch path selected once in `run()` should reduce branch pressure in tight loops.
- [ ] When `BuildOptions.jit == false`, remove JIT accounting from Buzz calls. `call()` still increments `closure.function.call_count` and calls `compileAndCall()` on every call path before discovering that `self.jit` is null (`src/vm.zig:4998`, `src/vm.zig:5020`). Guard that whole block with `if (BuildOptions.jit)` so non-JIT builds do not pay profiling/JIT checks.
- [ ] Add dedicated bytecodes for very common native collection methods, starting with `list.append`, `list.len`, `map.size`, and maybe `list.pop`. A call like `list.append(i)` currently goes through `OP_LIST_INVOKE`, method/native lookup, `callValue()`, and `callNative()` (`src/vm.zig:2059`, `src/vm.zig:5238`, `src/vm.zig:5119`), even though `ObjList.rawAppend()` is the real operation (`src/obj.zig:2072`). Codegen can recognize these builtins and emit direct opcodes.
- [ ] Optimize list/string subscript happy paths by separating checked and unchecked opcodes. `OP_GET_LIST_SUBSCRIPT` has checked, optional-checked, and "leave operands" modes in one opcode (`src/vm.zig:2579`), so the normal valid access path still branches through mode and bounds/error handling. Splitting into smaller opcodes should help list-heavy loops like bubble sort.
- [ ] Consider direct stack manipulation in arithmetic opcodes. Integer arithmetic currently uses `pop()`, `Value.integer()`, `Value.fromInteger()`, and `push()` per operation (`src/vm.zig:3758` and nearby). A helper that rewrites `stack_top[-2]` and decrements once would reduce stack pointer traffic for arithmetic-heavy loops.

Likely order of impact:

```text
1. currentFrame/readInstruction threading
2. no-JIT call path cleanup
3. direct bytecodes for common native collection methods
4. dispatch split for no-debugger runs
5. specialized subscript opcodes
6. stack-top arithmetic micro-ops
```

## JIT Performance Notes

Builds used for these numbers:

```text
zig build -Doptimize=ReleaseFast -Djit=true
zig build -Doptimize=ReleaseFast -Djit=true -Djit_always_on=true -Djit_hotspot_always_on=true
```

Default JIT results, stdout redirected:

```text
001-btree.buzz 14            real 1.11s   non-JIT was 0.63s
002-merkle.buzz 14           real 0.24s   non-JIT was 0.94s
007-fib.buzz                 real 0.04s   non-JIT was 0.20s
008-for.buzz                 real 0.02s   non-JIT was 0.13s
010-ackermann.buzz 3 9       real 0.23s   non-JIT was 0.81s
011-bubble-sort.buzz 5000    real 0.22s   non-JIT was 2.62s
```

Temporary microbenchmarks under `/tmp` with default JIT:

```text
int while loop, 20M iterations           real 0.08s   non-JIT was 1.29s
function call loop, 5M calls             real 0.05s   non-JIT was 0.46s
object field update loop, 5M updates     real 0.02s   non-JIT was 0.39s
list append/read loop, 3M + 3M ops       real 0.05s   non-JIT was 0.46s
map set/get loop, 500k + 500k ops        real 0.06s   non-JIT was 0.15s
```

Always-on JIT did not materially improve the fast cases:

```text
001-btree.buzz 14            real 1.35s
002-merkle.buzz 14           real 0.23s
007-fib.buzz                 real 0.03s
008-for.buzz                 real 0.02s
010-ackermann.buzz 3 9       real 0.22s
011-bubble-sort.buzz 5000    real 0.22s
int while loop, 20M          real 0.08s
function call loop, 5M       real 0.05s
```

That suggests the JIT is already effective for arithmetic loops, recursive calls, list loops, and map loops. The obvious miss is workload sensitivity: `001-btree` regresses, and always-on makes it worse, so compile latency and generic object/call lowering are probably dominating that case.

### JIT improvement targets
- [ ] Replace ratio-only hotness with a cheap pre-compile scoring function. `shouldCompileFunction()` and `shouldCompileHotspot()` use `f128` conversions/division on runtime paths (`src/vm.zig:5475`, `src/vm.zig:5500`) and compare each node against global ratios (`jit.call_count`, `hotspots_count`). That global ratio is both arbitrary and context-sensitive: a node can be worth compiling even if it is below 5% of all calls/hotspots, and another node can pass the ratio while still being too expensive or too generic to profit. Use fixed-point/integer accounting, but more importantly score each candidate by estimated payback before calling into MIR generation.
- [ ] Reduce synchronous compile latency. `compileFunction()` and `compileHotSpot()` both rebuild/load MIR modules, load externs, link, and run `MIR_gen_init`/`MIR_gen_finish` per compilation (`src/Jit.zig:200`, `src/Jit.zig:220`, `src/Jit.zig:222`, `src/Jit.zig:288`, `src/Jit.zig:308`, `src/Jit.zig:310`). Keep MIR generation initialized longer, batch queued functions/hotspots, or move compilation to a background worker and patch bytecode/functions when ready.
- [ ] Cache generated stack/base state inside each JIT function. `buildPush()`, `buildPop()`, `buildPeek()`, `buildGetLocal()`, and `buildSetLocal()` repeatedly reload `NativeCtx.stack_top` or `NativeCtx.base`, allocate an `index` register, and rebuild memory operands (`src/Jit.zig:739`, `src/Jit.zig:811`, `src/Jit.zig:882`, `src/Jit.zig:935`, `src/Jit.zig:983`). Keep `stack_top`, `base`, and `globals` in MIR state registers and flush them only at exits/native calls.
- [ ] Preserve unboxed locals across basic blocks. Integer/double arithmetic gets unwrapped and wrapped (`src/Jit.zig:2918`, `src/Jit.zig:1141`, `src/Jit.zig:1191`), but locals and stack values still move mostly as boxed `Value`s. A small local-value cache or trace-local SSA layer for int/double/bool locals would avoid repeated `Value` packing in tight loops.
- [ ] Specialize common object and collection operations in JIT lowering. Method/property calls currently route through external VM helpers for object fields and list/map/string/range properties (`src/Jit.zig:1992`, `src/Jit.zig:2007`, `src/Jit.zig:2039`). Add direct MIR lowering for the same common cases proposed for VM bytecodes: `list.append`, `list.len`, direct object property load/store by known field index, list subscript get/set, and map get/set where the key type is known.

### JIT candidate scoring policy

The scoring function should answer whether compiling a candidate is likely to pay for itself before `compileFunction()` or `compileHotSpot()` starts MIR generation. It should not be a blind absolute threshold, and it should not depend only on a global activity ratio.

```text
score =
  estimated_future_executions
  * estimated_interpreter_cost_per_exec
  * expected_speedup_factor
  - estimated_compile_cost
```

Compile only when `score > 0`, or preferably `score > min_profit_margin` so borderline candidates stay interpreted. This is still heuristic, but it is better than one rule for all AST shapes because it weighs count against expected compilation cost and expected generated-code quality.

Cheap inputs available before compiling:

```text
estimated_future_executions:
  current call/hotspot count
  recent count growth, if tracked
  recursive call count
  loop backedge count

estimated_interpreter_cost_per_exec:
  bytecode span
  AST node count
  weighted opcode/node count

expected_speedup_factor:
  high for int/double arithmetic, bool logic, locals, tight loops
  medium for simple function calls
  low for object fields, protocol dispatch, native calls, maps, strings
  negative or very low for shapes known to regress

estimated_compile_cost:
  AST/MIR node count
  number of calls
  number of extern/native helper calls
  control-flow complexity
  nested/collateral functions
```

A minimal first version could be:

```text
gain = count * runtime_weight(node)
cost = compile_weight(node)
score = gain - cost
```

The useful distinction is that `count = 500` should not mean the same thing for a tiny integer loop and for object/protocol/native-heavy code. The scoring function should let cheap/high-gain arithmetic loops compile early, while delaying or skipping shapes like `btree` until they are much hotter or until the JIT has better direct lowering for those operations.

The scoring function should be deterministic, cheap, and inspectable. A lightweight JIT stats/debug mode should print the terms used in the decision:

```text
JIT candidate while#123:
  count=1200
  runtime_weight=8
  compile_weight=140
  expected_speedup=0.80
  score=7540
  decision=compile
```

If a compiled candidate turns out to be expensive or regresses, that does not help the current node because the cost is already sunk. Feedback is still useful for later candidates: raise or lower scoring weights for similar AST shapes.

### Likely JIT order of impact

```text
1. cache stack_top/base/globals in generated MIR
2. direct JIT lowering for common object/list/map operations
3. reduce per-compilation MIR link/gen overhead
4. replace ratio-only hotness with a candidate scoring function
5. unboxed local/trace SSA for int/double/bool values
```

## Memory Usage Notes

Builds used for these memory probes:

```text
zig build -Doptimize=ReleaseFast -Djit=true
zig build -Doptimize=ReleaseFast -Djit=false
```

Memory probes used `/usr/bin/time -l`; `peak memory footprint` is the more useful macOS figure here:

```text
compile 12k globals, no imports       peak  31.1 MB
compile 12k functions, no imports     peak 217.3 MB
btree 14, JIT on                      peak 179.4 MB
btree 14, JIT off                     peak 177.5 MB
list append/read microbench, JIT on   peak  77.7 MB
map set/get microbench, JIT on        peak  43.3 MB
object field microbench, JIT on       peak   3.2 MB
```

The `btree` memory footprint is essentially the same with and without JIT, so the main memory pressure there is not generated native code. It is more likely runtime objects, object/list/map support structures, retained AST/type metadata, and GC accounting blind spots.

### Scanner and String Parser
- [ ] `Scanner.docblock()` builds a fresh `ArrayList(u8)` one byte at a time and returns `std.mem.trim(u8, block.items, " ")` as the token literal (`src/Scanner.zig:215`, `src/Scanner.zig:226`, `src/Scanner.zig:249`). The buffer is intentionally retained because the token points into it, but capacity is not trimmed and ownership is not visible from `Token`. Convert it to an owned slice before returning, or store docblocks in the same owned-string path as other parser-owned strings so lifetime and freeing are explicit.
- [ ] `StringParser.parse()` appears to leak the temporary `current_chunk` buffers used for string interpolation chunks. `push()` copies `chars` into an interned `ObjString` via `gc.copyString()` (`src/StringParser.zig:97`, `src/StringParser.zig:109`), then `parse()` resets `current_chunk = .empty` without deinit after each chunk (`src/StringParser.zig:60`, `src/StringParser.zig:62`, `src/StringParser.zig:78`, `src/StringParser.zig:81`). If `copyString()` copied the bytes, the original chunk buffer should be freed or reused with `clearRetainingCapacity()`.
- [ ] Most scanner tokens correctly point into source slices and do not allocate (`src/Scanner.zig:570`). Keep that property. The allocation work should stay limited to docblocks and escaped/interpolated string chunks.

### Parser
- [ ] The root block construction issue is both a CPU and memory churn problem. `parse()` repeatedly wraps the existing root block slice with `ArrayList.fromOwnedSlice()`, appends one declaration, and `toOwnedSlice()`s it again (`src/Parser.zig:1040`, `src/Parser.zig:1044`, `src/Parser.zig:1046`). Keep one root `ArrayList(Ast.Node.Index)` for the whole parse and publish one owned slice at the end.
- [ ] `qualifiedName()` allocates a slice for every named variable, even the common single-identifier case (`src/Parser.zig:6107`, `src/Parser.zig:6109`, `src/Parser.zig:6116`). Add a single-token representation or a small inline buffer path so `variable()` does not allocate for every ordinary local/global read (`src/Parser.zig:6119`).
- [ ] Import path generation does multiple heap allocations per candidate path with repeated `replaceOwned()` calls (`src/Parser.zig:8583`, `src/Parser.zig:8591`, `src/Parser.zig:8599`, and the similar lib/zdef paths). This is cold for simple scripts but noisy for import-heavy programs or missing imports. Generate candidate paths into a reusable `Writer.Allocating`, or use stack/fixed buffers for the common `$`, `?`, `!` substitutions.
- [ ] The parser duplicates imported globals and often allocates renamed `global.name` slices when applying prefixes (`src/Parser.zig:9091`, `src/Parser.zig:9102`, `src/Parser.zig:9113`, `src/Parser.zig:9120`). The comment notes that all globals are imported, then hidden, because runtime globals are indexed (`src/Parser.zig:9118`). This costs memory for selective imports. Long term, named/runtime import indirection would let selective imports avoid cloning hidden globals.
- [ ] Function-heavy compilation is memory-heavy: 12k empty functions peaked around 217 MB versus 31 MB for 12k globals. Likely contributors are per-function AST nodes, unique function typedefs, `ObjFunction` chunks, one empty-string constant per chunk (`src/Codegen.zig:1775`), and parallel code/location arrays. This is worth measuring after root-block and call-generation churn are fixed.

### Codegen and Chunk
- [ ] `generateCall()` allocates temporary maps/lists for every call: `missing_arguments`, `arguments_order_ref`, and sometimes a cloned missing-arguments map (`src/Codegen.zig:860`, `src/Codegen.zig:896`, `src/Codegen.zig:910`). Add a zero-allocation fast path for positional calls with no defaults/reordering, and use a stack/small-buffer path for low arity.
- [ ] Every function generation heap-allocates a `Frame`, then destroys it after codegen (`src/Codegen.zig:1710`, `src/Codegen.zig:1901`, `src/Codegen.zig:1902`). For many small functions this is pure allocator churn. A stack of `Frame` values in `Codegen` or an arena-style frame pool would reduce allocator traffic.
- [ ] `Chunk.write()` appends one `u32` opcode and one `Ast.TokenIndex` location per instruction (`src/Chunk.zig:11`, `src/Chunk.zig:13`, `src/Chunk.zig:30`, `src/Chunk.zig:32`). That doubles instruction stream storage before constants. If full per-instruction locations are not needed in normal runs, use a compressed line/location table or keep dense locations only for debug builds.
- [ ] Each function gets an empty-string constant, even though the empty string is interned globally (`src/Codegen.zig:1775`). The constant entry itself still costs one slot per chunk. Consider a dedicated `OP_EMPTY_STRING`/well-known constant or lazy insertion only for functions that actually emit empty strings.

### VM and Runtime Objects
- [ ] `OP_IMPORT` allocates a child `VM` and runs the imported closure in it, then keeps only copied globals/import cache, but the child VM is never deinitialized or destroyed (`src/vm.zig:2252`, `src/vm.zig:2257`, `src/vm.zig:2267`). After copying exported globals into the parent and import cache (`src/vm.zig:2305`, `src/vm.zig:2320`, `src/vm.zig:2327`), deinit/destroy the child VM unless some field is intentionally retained.
- [ ] Every fiber allocates a full fixed stack of `BuildOptions.stack_size`, default 100,000 `Value`s (`build.zig:772`, `build.zig:806`, `src/vm.zig:151`). Since `Value` is one `u64` (`src/value.zig:41`), that is about 800 KB per fiber before frames/locals. Use a smaller initial stack with growth, segmented stacks, or a per-fiber stack pool.
- [ ] Every `ObjList` and `ObjMap` instance allocates its own `methods` cache array (`src/obj.zig:1957`, `src/obj.zig:1961`, `src/obj.zig:3213`, `src/obj.zig:3216`). String/range/fiber/pattern native-member caches already live on `GC` and are shared (`src/GC.zig:53`, `src/GC.zig:60`, `src/obj.zig:1108`, `src/obj.zig:3159`). If list/map native methods are not instance-specific, move these caches to the type/GC level. This removes one heap allocation and 15-18 optional pointers from every list/map.
- [ ] List/map concatenation clones whole containers (`OP_ADD_LIST` copies left then right, `OP_ADD_MAP` clones the left map before adding right; `src/vm.zig:3674`, `src/vm.zig:3675`, `src/vm.zig:3715`). That is semantically required for immutable `+`, but there is room for capacity pre-sizing and direct `ensureTotalCapacity(left.len + right.len)` to avoid growth churn.
- [ ] Runtime error reporting allocates a stack `ArrayList`, allocates a stringified payload, and allocates one note message per frame (`src/vm.zig:4723`, `src/vm.zig:4752`, `src/vm.zig:4793`, `src/vm.zig:4846`, `src/vm.zig:4856`, `src/vm.zig:4912`). This is cold-path, so it is not a top performance target unless exceptions are used for control flow.

### GC
- [ ] GC accounting does not cover most memory allocated through `gc.allocator` directly. `bytes_allocated` is updated by `GC.allocate()` and `GC.allocateMany()` (`src/GC.zig:135`, `src/GC.zig:168`), but many large buffers use `gc.allocator` directly: chunks, parser slices, hash maps, list/map item buffers, list/map method arrays, VM stacks, frames, and import caches. This means GC thresholds and `memory_limit` can undercount real memory substantially. Consider a tracking allocator for all Buzz-owned allocations, or route runtime container buffers through GC accounting.
- [ ] `allocateMany()` does not enforce `BuildOptions.memory_limit`, while `allocate()` does (`src/GC.zig:146`, `src/GC.zig:168`). Large slices can bypass the configured memory limit. Apply the same limit check after `bytes_allocated` changes in `allocateMany()`.
- [ ] `markFiber()` loops through parent fibers as `ufiber`, but marks `fiber.stack`, `fiber.stack_top`, `fiber.frames`, and `fiber.open_upvalues` inside the loop instead of `ufiber.*` (`src/GC.zig:569`, `src/GC.zig:577`, `src/GC.zig:589`, `src/GC.zig:606`). This is a correctness issue with memory consequences: parent-fiber roots can be missed, and the same initial fiber can be rescanned.
- [ ] `markRoots()` marks every `value` stored on every AST node during each collection (`src/GC.zig:688`, `src/GC.zig:690`). The comment says this is for values only referenced by JIT. This keeps AST constant values alive for the whole run and scans a MultiArrayList every GC. A narrower root set for JIT-required constants would reduce retention and mark time.
- [ ] `gray_stack` uses `ArrayList` and keeps capacity after high-water marks (`src/GC.zig:44`, `src/GC.zig:344`, `src/GC.zig:701`). That is usually good for throughput, but if a one-time spike causes persistent memory retention, shrink after full collections or cap retained capacity.

### JIT
- [ ] `buildExternApiCall()` builds and deinitializes a `full_args` `ArrayList`, but then uses `self.args_buffer` for the actual MIR call (`src/Jit.zig:1711`, `src/Jit.zig:1712`, `src/Jit.zig:1722`, `src/Jit.zig:1734`). The `full_args` list is dead allocation work and should be removed.
- [ ] `REG()` allocates a temporary formatted C string for every MIR register name (`src/Jit.zig:7273`, `src/Jit.zig:7274`, `src/Jit.zig:7279`, `src/Jit.zig:7284`). For generated code with many stack/local helpers, this creates substantial compile-time allocation churn. Use a fixed buffer for short names, or keep a reusable name buffer in `GenState`.
- [ ] JIT call lowering allocates an `AutoArrayHashMapUnmanaged` for every call to map argument names to MIR operands (`src/Jit.zig:2215`, `src/Jit.zig:2225`). Add the same no-allocation positional/no-default fast path as Codegen.
- [ ] `compileFunction()` and `compileHotSpot()` clear JIT queues/modules with `clearAndFree()` after every compilation (`src/Jit.zig:143`, `src/Jit.zig:144`, `src/Jit.zig:147`). If functions/hotspots are compiled repeatedly during one run, this throws away capacity and reallocates on the next compilation. Use `clearRetainingCapacity()` for small queues, or keep a bounded retained capacity.
- [ ] Qualified MIR names are allocated multiple times per function/hotspot (`src/Jit.zig:387`, `src/Jit.zig:392`, `src/Jit.zig:4922`, `src/Jit.zig:4927`, `src/Jit.zig:5044`, `src/Jit.zig:5117`, `src/Jit.zig:5284`). Some are necessary for MIR ownership, but repeated raw/non-raw recomputation can be cached in the JIT state for the current node.

### Memory impact order

```text
1. Track all Buzz-owned allocations in GC accounting, not only GC.allocate/allocateMany.
2. Fix StringParser current_chunk ownership/reuse.
3. Deinit/destroy child VMs created by OP_IMPORT.
4. Move list/map native method caches off each instance.
5. Replace fixed 100k-value fiber stacks with growable/pooled stacks.
6. Remove dead JIT full_args allocation and add call fast paths in JIT/Codegen.
7. Keep one root block ArrayList and avoid single-token qualifiedName allocations.
8. Compress or make optional Chunk.locations for non-debug runs.
9. Fix markFiber parent-fiber marking.
10. Reduce JIT register/name allocation churn.
```
