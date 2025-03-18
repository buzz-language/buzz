const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const obj = @import("obj.zig");
const Token = @import("Token.zig");
const Chunk = @import("Chunk.zig");
const v = @import("value.zig");
const Value = v.Value;
const Integer = v.Integer;
const FFI = @import("FFI.zig");
const Ast = @import("Ast.zig");
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Scanner = @import("Scanner.zig");
const RunFlavor = @import("vm.zig").RunFlavor;
const Reporter = @import("Reporter.zig");
const StringParser = @import("StringParser.zig");
const pcre = if (!is_wasm) @import("pcre.zig") else void;
const buzz_api = @import("lib/buzz_api.zig");
const io = @import("io.zig");

// In the wasm build, libraries are statically linked
const std_lib = if (is_wasm) @import("lib/buzz_std.zig") else void;
const std_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "assert", &std_lib.assert },
        .{ "buzzPanic", &std_lib.buzzPanic },
        .{ "char", &std_lib.char },
        .{ "currentFiber", &std_lib.currentFiber },
        .{ "parseDouble", &std_lib.parseDouble },
        .{ "parseInt", &std_lib.parseInt },
        .{ "parseUd", &std_lib.parseUd },
        .{ "print", &std_lib.print },
        .{ "random", &std_lib.random },
        .{ "toDouble", &std_lib.toDouble },
        .{ "toInt", &std_lib.toInt },
        .{ "toUd", &std_lib.toUd },
        .{ "args", &std_lib.args },
    },
) else void;

const gc_lib = if (is_wasm) @import("lib/buzz_gc.zig") else void;
const gc_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "allocated", &gc_lib.allocated },
        .{ "collect", &gc_lib.collect },
    },
) else void;

const math_lib = if (is_wasm) @import("lib/buzz_math.zig") else void;
const math_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "abs", &math_lib.abs },
        .{ "acos", &math_lib.acos },
        .{ "asin", &math_lib.asin },
        .{ "atan", &math_lib.atan },
        .{ "bzsqrt", &math_lib.bzsqrt },
        .{ "bzceil", &math_lib.bzceil },
        .{ "bzcos", &math_lib.bzcos },
        .{ "bzexp", &math_lib.bzexp },
        .{ "bzfloor", &math_lib.bzfloor },
        .{ "bzlog", &math_lib.bzlog },
        .{ "minDouble", &math_lib.minDouble },
        .{ "maxDouble", &math_lib.maxDouble },
        .{ "minInt", &math_lib.minInt },
        .{ "maxInt", &math_lib.maxInt },
        .{ "bzsin", &math_lib.bzsin },
        .{ "bztan", &math_lib.bztan },
        .{ "pow", &math_lib.pow },
    },
) else void;

const buffer_lib = if (is_wasm) @import("lib/buzz_buffer.zig") else void;
const buffer_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "BufferNew", &buffer_lib.BufferNew },
        .{ "BufferDeinit", &buffer_lib.BufferDeinit },
        .{ "BufferRead", &buffer_lib.BufferRead },
        .{ "BufferWrite", &buffer_lib.BufferWrite },
        .{ "BufferReadBoolean", &buffer_lib.BufferReadBoolean },
        .{ "BufferWriteBoolean", &buffer_lib.BufferWriteBoolean },
        .{ "BufferWriteInt", &buffer_lib.BufferWriteInt },
        .{ "BufferReadInt", &buffer_lib.BufferReadInt },
        .{ "BufferWriteUserData", &buffer_lib.BufferWriteUserData },
        .{ "BufferReadUserData", &buffer_lib.BufferReadUserData },
        .{ "BufferWriteDouble", &buffer_lib.BufferWriteDouble },
        .{ "BufferReadDouble", &buffer_lib.BufferReadDouble },
        .{ "BufferLen", &buffer_lib.BufferLen },
        .{ "BufferCursor", &buffer_lib.BufferCursor },
        .{ "BufferBuffer", &buffer_lib.BufferBuffer },
        .{ "BufferPtr", &buffer_lib.BufferPtr },
        .{ "BufferEmpty", &buffer_lib.BufferEmpty },
        .{ "BufferAt", &buffer_lib.BufferAt },
        .{ "BufferSetAt", &buffer_lib.BufferSetAt },
        .{ "BufferWriteZ", &buffer_lib.BufferWriteZ },
        .{ "BufferWriteZAt", &buffer_lib.BufferWriteZAt },
        .{ "BufferReadZ", &buffer_lib.BufferReadZ },
        .{ "BufferReadZAt", &buffer_lib.BufferReadZAt },
        .{ "BufferWriteStruct", &buffer_lib.BufferWriteStruct },
        .{ "BufferWriteStructAt", &buffer_lib.BufferWriteStructAt },
        .{ "BufferReadStruct", &buffer_lib.BufferReadStruct },
        .{ "BufferReadStructAt", &buffer_lib.BufferReadStructAt },
    },
) else void;

const debug_lib = if (is_wasm) @import("lib/buzz_debug.zig") else void;
const debug_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "dump", &debug_lib.dump },
    },
) else void;

const serialize_lib = if (is_wasm) @import("lib/buzz_serialize.zig") else void;
const serialize_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "serialize", &serialize_lib.serializeValue },
    },
) else void;

const crypto_lib = if (is_wasm) @import("lib/buzz_crypto.zig") else void;
const crypto_api = if (is_wasm) std.StaticStringMap(buzz_api.NativeFn).initComptime(
    .{
        .{ "hash", &crypto_lib.hash },
    },
) else void;

const libs = if (is_wasm)
    std.StaticStringMap(std.StaticStringMap(buzz_api.NativeFn)).initComptime(
        .{
            .{ "std", std_api },
            .{ "gc", gc_api },
            .{ "math", math_api },
            .{ "buffer", buffer_api },
            .{ "debug", debug_api },
            .{ "serialize", serialize_api },
            .{ "crypto", crypto_api },
        },
    )
else
    void;

const Self = @This();

ast: Ast,
gc: *GarbageCollector,
scanner: ?Scanner = null,
current_token: ?Ast.TokenIndex = null,
script_name: []const u8 = undefined,
/// If true the script is being imported
imported: bool = false,
/// True when parsing a declaration inside an export statement
exporting: bool = false,
/// Cached imported functions (shared across instances of Parser)
imports: *std.StringHashMapUnmanaged(ScriptImport),
/// Keep track of things imported by the current script
script_imports: std.StringHashMapUnmanaged(LocalScriptImport),
test_count: u64 = 0,
// FIXME: use SinglyLinkedList instead of heap allocated ptrs
current: ?*Frame = null,
current_object: ?ObjectFrame = null,
globals: std.ArrayListUnmanaged(Global),
global_names: std.StringHashMapUnmanaged(u24),
namespace: ?[]const Ast.TokenIndex = null,
flavor: RunFlavor,
ffi: FFI,
reporter: Reporter,

/// Jump to patch at end of current expression with a optional unwrapping in the middle of it
opt_jumps: ?std.ArrayList(Precedence) = null,

pub fn init(
    gc: *GarbageCollector,
    imports: *std.StringHashMapUnmanaged(ScriptImport),
    imported: bool,
    flavor: RunFlavor,
) Self {
    return .{
        .gc = gc,
        .imports = imports,
        .script_imports = .{},
        .imported = imported,
        .globals = .{},
        .global_names = .empty,
        .flavor = flavor,
        .reporter = Reporter{
            .allocator = gc.allocator,
            .error_prefix = "Syntax",
            .collect = flavor == .Ast,
        },
        .ffi = FFI.init(gc),
        .ast = Ast.init(gc.allocator),
    };
}

pub fn deinit(self: *Self) void {
    // for (self.globals.items) |global| {
    //     self.gc.allocator.free(global.name);
    // }
    self.globals.deinit(self.gc.allocator);
    self.global_names.deinit(self.gc.allocator);
    self.script_imports.deinit(self.gc.allocator);
    if (self.opt_jumps) |jumps| {
        jumps.deinit();
    }
    self.ffi.deinit();
    self.reporter.deinit();
}

extern fn dlerror() callconv(.c) [*:0]u8;

pub fn defaultBuzzPrefix() []const u8 {
    return ".";
}

var _buzz_path_buffer: [4096]u8 = undefined;
pub fn buzzPrefix(allocator: std.mem.Allocator) error{OutOfMemory}![]const u8 {
    // FIXME: don't use std.posix directly
    if (std.process.getEnvVarOwned(allocator, "BUZZ_PATH") catch |err| env: {
        switch (err) {
            error.EnvironmentVariableNotFound, error.InvalidWtf8 => break :env null,
            else => return error.OutOfMemory,
        }
    }) |buzz_path| {
        return buzz_path;
    }

    const path = if (!is_wasm)
        std.fs.selfExePath(&_buzz_path_buffer) catch return defaultBuzzPrefix()
    else
        defaultBuzzPrefix();

    const path1 = std.fs.path.dirname(path) orelse defaultBuzzPrefix();
    const path2 = std.fs.path.dirname(path1) orelse defaultBuzzPrefix();
    return path2;
}

var _buzz_path_buffer2: [4096]u8 = undefined;
/// the returned string can be used only until next call to this function
pub fn buzzLibPath(allocator: std.mem.Allocator) ![]const u8 {
    const path2 = try buzzPrefix(allocator);
    const sep = std.fs.path.sep_str;
    return std.fmt.bufPrint(
        &_buzz_path_buffer2,
        "{s}" ++ sep ++ "lib" ++ sep ++ "buzz",
        .{
            path2,
        },
    ) catch unreachable;
}

pub const CompileError = error{
    Unrecoverable,
    Recoverable,
};

pub const Local = struct {
    name: Ast.TokenIndex,
    node: Ast.Node.Index,
    type_def: *obj.ObjTypeDef,
    depth: i32,
    captured: bool,
    final: bool,
    mutable: bool,
    referenced: bool = false,
    assigned: bool = false,

    pub fn isReferenced(self: Local, ast: Ast) bool {
        const lexemes = ast.tokens.items(.lexeme);
        return self.referenced or
            self.type_def.def_type == .Void or
            self.type_def.def_type == .Placeholder or
            lexemes[self.name][0] == '$' or
            (lexemes[self.name][0] == '_' and lexemes[self.name].len == 1);
    }

    pub fn isAssigned(self: Local, ast: Ast) bool {
        const name = ast.tokens.items(.lexeme)[self.name];
        return self.final or
            self.assigned or
            std.mem.eql(u8, name, "_") or
            std.mem.startsWith(u8, name, "$") or
            std.mem.eql(u8, name, "this");
    }
};

pub const Global = struct {
    name: []const Ast.TokenIndex,
    node: Ast.Node.Index,
    type_def: *obj.ObjTypeDef,
    export_alias: ?Ast.TokenIndex = null,
    imported_from: ?[]const u8 = null,
    placeholder_referrers: std.ArrayListUnmanaged(Ast.Node.Index) = .{},

    initialized: bool = false,
    exported: bool = false,
    hidden: bool = false,
    final: bool,
    mutable: bool,
    referenced: bool = false,

    pub fn isReferenced(self: Global, ast: Ast) bool {
        const lexemes = ast.tokens.items(.lexeme);
        const function_type = if (self.type_def.def_type == .Function)
            self.type_def.resolved_type.?.Function.function_type
        else
            null;

        return self.referenced or
            self.type_def.def_type == .Void or
            self.type_def.def_type == .Placeholder or
            (function_type == .Extern or function_type == .Abstract or function_type == .EntryPoint or function_type == .ScriptEntryPoint or function_type != .Repl) or
            lexemes[self.name[self.name.len - 1]][0] == '$' or
            (lexemes[self.name[self.name.len - 1]][0] == '_' and lexemes[self.name[self.name.len - 1]].len == 1) or
            self.exported;
    }

    fn match(self: Global, ast: Ast, qualified: []const Ast.TokenIndex, name: ?Ast.TokenIndex) bool {
        if (self.name.len != qualified.len + 1) {
            return false;
        }

        const lexemes = ast.tokens.items(.lexeme);

        // TODO could this also use the global_names hashmap instead?
        if (qualified.len > 0) {
            for (qualified[0 .. qualified.len - 1], 0..) |name_token, i| {
                if (!std.mem.eql(u8, lexemes[name_token], lexemes[self.name[i]])) {
                    return false;
                }
            }
        }

        return name == null or std.mem.eql(u8, lexemes[name.?], lexemes[self.name[self.name.len - 1]]);
    }

    pub fn matchName(self: Global, ast: Ast, namespace: []const Ast.TokenIndex, name: Ast.TokenIndex) bool {
        return self.match(ast, namespace, name);
    }

    pub fn matchNamespace(self: Global, ast: Ast, namespace: []const Ast.TokenIndex) bool {
        return self.match(ast, namespace, null);
    }
};

pub const UpValue = struct {
    index: u8,
    is_local: bool,
    node: Ast.Node.Index,
};

pub const Frame = struct {
    enclosing: ?*Frame = null,
    locals: [255]Local,
    local_count: u8 = 0,
    upvalues: [255]UpValue,
    upvalue_count: u8 = 0,
    scope_depth: u32 = 0,
    /// Keep track of the node that introduced the scope (useful for labeled break/continue statements)
    scopes: std.ArrayListUnmanaged(?Ast.Node.Index) = .{},
    /// If false, `return` was omitted or within a conditionned block (if, loop, etc.)
    /// We only count `return` emitted within the scope_depth 0 of the current function or unconditionned else statement
    function_node: Ast.Node.Index,
    function: ?*obj.ObjFunction = null,
    generics: ?*std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef) = null,

    in_try: bool = false,
    in_block_expression: ?u32 = null,

    pub fn deinit(self: *Frame, allocator: std.mem.Allocator) void {
        self.scopes.deinit(allocator);
        // self.generics ends up in AST node so we don't deinit it
    }

    pub fn resolveGeneric(self: Frame, name: *obj.ObjString) ?*obj.ObjTypeDef {
        if (self.generics) |generics| {
            if (generics.get(name)) |type_def| {
                return type_def;
            }
        }

        return if (self.enclosing) |enclosing|
            enclosing.resolveGeneric(name)
        else
            null;
    }
};

pub const ObjectFrame = struct {
    name: Token,
    type_def: *obj.ObjTypeDef,
    generics: ?*std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef) = null,
};

pub const ScriptImport = struct {
    function: Ast.Node.Index,
    globals: std.ArrayListUnmanaged(Global) = .{},
    global_names: std.StringArrayHashMapUnmanaged(u24) = .empty,
    absolute_path: *obj.ObjString,
    imported_by: std.AutoHashMapUnmanaged(*Frame, void) = .{},
};

const LocalScriptImport = struct {
    referenced: bool = false,
    location: Ast.TokenIndex,
    end_location: Ast.TokenIndex,
};

pub var user_library_paths: ?[][]const u8 = null;

pub const DeclarationTerminator = enum {
    Comma,
    OptComma,
    Semicolon,
    Nothing,
};

pub const LoopType = enum {
    While,
    Do,
    For,
    ForEach,
};

pub const LoopScope = struct {
    loop_type: LoopType,
    loop_body_scope: usize,
};

pub const Precedence = enum(u8) {
    None,
    Assignment, // =, -=, +=, *=, /=
    IsAs, // is, as?
    Or, // or
    And, // and
    Equality, // ==, !=
    Comparison, // >=, <=, >, <
    Term, // +, -
    NullCoalescing, // ??, typeof
    Bitwise, // \, &, ^
    Shift, // >>, <<
    Factor, // /, *, %
    Unary, // +, ++, -, --, !
    Call, // call(), dot.ref, sub[script], optUnwrap?
    Primary, // literal, (grouped expression), identifier, [<type>, alist], {<a, map>, ...}
};

pub const Error = error{
    BuzzNoDll,
    ImportError,
    CantCompile,
    UnwrappedNull,
    OutOfBound,
    ReachedMaximumMemoryUsage,
} || std.mem.Allocator.Error || std.fmt.BufPrintError || CompileError;

const ParseFn = *const fn (*Self, bool) Error!Ast.Node.Index;
const InfixParseFn = *const fn (*Self, bool, Ast.Node.Index) Error!Ast.Node.Index;

const ParseRule = struct {
    prefix: ?ParseFn = null,
    infix: ?InfixParseFn = null,
    precedence: Precedence = .None,
};

const search_paths = if (builtin.os.tag == .windows)
    [_][]const u8{
        "$/?.!",
        "$/?/main.!",
        "$/?/src/?.!",
        "$/?/src/main.!",
        "./?.!",
        "./?/main.!",
        "./?/src/main.!",
        "./?/src/?.!",
        "./src/?.!",
        // TODO: what would be common windows paths for this?
    }
else
    [_][]const u8{
        "$/?.!",
        "$/?/main.!",
        "$/?/src/?.!",
        "$/?/src/main.!",
        "./?.!",
        "./?/main.!",
        "./?/src/main.!",
        "./?/src/?.!",
        "./src/?.!",
        "/usr/share/buzz/?.!",
        "/usr/share/buzz/?/main.!",
        "/usr/share/buzz/?/src/main.!",
        "/usr/local/share/buzz/?/src/?.!",
        "/usr/local/share/buzz/?.!",
        "/usr/local/share/buzz/?/main.!",
        "/usr/local/share/buzz/?/src/main.!",
        "/usr/local/share/buzz/?/src/?.!",
    };

const lib_search_paths = if (builtin.os.tag == .windows)
    [_][]const u8{
        "$/?.!",
        "$/?/src/?.!",
        "./?.!",
        "./?/src/?.!",
        "./src/?.!",
    }
else
    [_][]const u8{
        "$/lib?.!",
        "$/?/src/lib?.!",
        "./lib?.!",
        "./?/src/lib?.!",
        "./src/lib?.!",
        "/usr/share/buzz/lib?.!",
        "/usr/share/buzz/?/src/lib?.!",
        "/usr/share/local/buzz/lib?.!",
        "/usr/share/local/buzz/?/src/lib?.!",
    };

const zdef_search_paths = if (builtin.os.tag == .windows)
    [_][]const u8{
        "./?.!",
    }
else
    [_][]const u8{
        "./?.!",
        "/usr/lib/?.!",
        "/usr/local/lib/?.!",
        "./lib?.!",
        "/usr/lib/lib?.!",
        "/usr/local/lib/lib?.!",
    };

const rules = [_]ParseRule{
    .{ .prefix = list, .infix = subscript, .precedence = .Call }, // LeftBracket
    .{}, // RightBracket
    .{ .prefix = grouping, .infix = call, .precedence = .Call }, // LeftParen
    .{}, // RightParen
    .{ .prefix = map, .infix = objectInit, .precedence = .Primary }, // LeftBrace
    .{}, // RightBrace
    .{ .prefix = anonymousObjectInit, .infix = dot, .precedence = .Call }, // Dot
    .{}, // Comma
    .{}, // Semicolon
    .{ .infix = binary, .precedence = .Comparison }, // Greater
    .{ .prefix = typeExpression, .infix = binary, .precedence = .Comparison }, // Less
    .{ .infix = binary, .precedence = .Term }, // Plus
    .{ .prefix = unary, .infix = binary, .precedence = .Term }, // Minus
    .{ .infix = binary, .precedence = .Factor }, // Star
    .{ .infix = binary, .precedence = .Factor }, // Slash
    .{}, // AntiSlash
    .{ .infix = binary, .precedence = .Factor }, // Percent
    .{ .infix = gracefulUnwrap, .precedence = .Call }, // Question
    .{ .prefix = unary, .infix = forceUnwrap, .precedence = .Call }, // Bang
    .{}, // Colon
    .{ .infix = genericResolve, .precedence = .Call }, // DoubleColon
    .{}, // Equal
    .{ .infix = binary, .precedence = .Equality }, // EqualEqual
    .{ .infix = binary, .precedence = .Equality }, // BangEqual
    .{}, // BangGreater
    .{ .infix = binary, .precedence = .Comparison }, // GreaterEqual
    .{ .infix = binary, .precedence = .Comparison }, // LessEqual
    .{ .infix = binary, .precedence = .NullCoalescing }, // QuestionQuestion
    .{}, // Arrow
    .{}, // DoubleArrow
    .{ .prefix = literal }, // True
    .{ .prefix = literal }, // False
    .{ .prefix = literal }, // Null
    .{}, // Str
    .{}, // Ud
    .{}, // Int
    .{}, // Double
    .{}, // Type
    .{}, // Bool
    .{}, // Function
    .{ .infix = binary, .precedence = .Shift }, // ShiftRight
    .{ .infix = binary, .precedence = .Shift }, // ShiftLeft
    .{ .infix = binary, .precedence = .Bitwise }, // Xor
    .{ .infix = binary, .precedence = .Bitwise }, // Bor
    .{ .prefix = unary, .precedence = .Term }, // Bnot
    .{ .infix = @"or", .precedence = .Or }, // Or
    .{ .infix = @"and", .precedence = .And }, // And
    .{}, // Return
    .{ .prefix = inlineIf }, // If
    .{}, // Else
    .{}, // Do
    .{}, // Until
    .{}, // While
    .{}, // For
    .{}, // ForEach
    .{}, // Break
    .{}, // Continue
    .{}, // In
    .{ .infix = is, .precedence = .IsAs }, // Is
    .{ .prefix = literal }, // Integer
    .{ .prefix = literal }, // FloatValue
    .{ .prefix = string }, // String
    .{ .prefix = variable }, // Identifier
    .{ .prefix = fun }, // Fun
    .{}, // Object
    .{}, // Obj
    .{}, // Protocol
    .{}, // Enum
    .{}, // Throw
    .{}, // Try
    .{}, // Catch
    .{}, // Test
    .{}, // Import
    .{}, // Export
    .{}, // Final
    .{}, // Static
    .{ .prefix = blockExpression }, // From
    .{}, // As
    .{ .infix = as, .precedence = .IsAs }, // AsQuestion
    .{}, // Extern
    .{}, // Eof
    .{}, // Error
    .{ .prefix = literal }, // Void
    .{}, // Docblock
    .{ .prefix = pattern }, // Pattern
    .{}, // pat
    .{}, // fib
    .{ .prefix = asyncCall, .infix = binary, .precedence = .Term }, // &
    .{ .prefix = resumeFiber, .precedence = .Primary }, // resume
    .{ .prefix = resolveFiber, .precedence = .Primary }, // resolve
    .{ .prefix = yield, .precedence = .Primary }, // yield
    .{ .infix = range, .precedence = .Primary }, // ..
    .{}, // any
    .{}, // zdef
    .{ .prefix = typeOfExpression, .precedence = .Unary }, // typeof
    .{}, // var
    .{}, // out
    .{}, // namespace
    .{}, // rg
    .{ .prefix = mutableExpression }, // mut
    .{}, // PlusEqual
    .{}, // MinusEqual
    .{}, // StarEqual
    .{}, // SlashEqual
    .{}, // ShiftRightEqual
    .{}, // ShiftLeftEqual
    .{}, // XorEqual
    .{}, // BorEqual
    .{}, // BnotEqual
    .{}, // AmpersandEqual
    .{}, // PercentEqual
};

pub fn reportErrorAtNode(self: *Self, error_type: Reporter.Error, node: Ast.Node.Index, comptime fmt: []const u8, args: anytype) void {
    self.reporter.reportErrorFmt(
        error_type,
        self.ast.tokens.get(self.ast.nodes.items(.location)[node]),
        self.ast.tokens.get(self.ast.nodes.items(.end_location)[node]),
        fmt,
        args,
    );
}

pub fn advance(self: *Self) !void {
    if (self.current_token != null and self.ast.tokens.items(.tag)[self.current_token.?] == .Eof) {
        return;
    }

    self.current_token = if (self.current_token) |ct| ct + 1 else 0;

    if (self.current_token.? >= self.ast.tokens.len) {
        while (true) {
            const new_token = try self.scanner.?.scanToken();

            if (new_token.tag == .Error) {
                self.current_token = if (self.current_token) |ct| ct - 1 else 0;
                self.reporter.reportErrorFmt(
                    .unknown,
                    self.ast.tokens.get(self.current_token.? - 1),
                    self.ast.tokens.get(self.current_token.? - 1),
                    "{s}",
                    .{
                        new_token.literal_string orelse "Unknown Error",
                    },
                );
            }

            _ = try self.ast.appendToken(new_token);

            if (new_token.tag != .Error or new_token.tag == .Eof) {
                break;
            }
        }
    }
}

// Used when parsing several script one after the other so that Eof doesn't stop parsing
fn advancePastEof(self: *Self) !void {
    self.current_token = if (self.current_token) |ct| ct + 1 else 0;

    if (self.current_token.? >= self.ast.tokens.len) {
        while (true) {
            const new_token = try self.scanner.?.scanToken();

            if (new_token.tag == .Error) {
                self.current_token = if (self.current_token) |ct| ct - 1 else 0;
                self.reporter.reportErrorFmt(
                    .unknown,
                    self.ast.tokens.get(self.current_token.? - 1),
                    self.ast.tokens.get(self.current_token.? - 1),
                    "{s}",
                    .{
                        new_token.literal_string orelse "Unknown error.",
                    },
                );
            }

            _ = try self.ast.appendToken(new_token);

            if (new_token.tag != .Error) {
                break;
            }
        }
    }
}

pub fn consume(self: *Self, tag: Token.Type, comptime message: []const u8) !void {
    if (self.ast.tokens.items(.tag)[self.current_token.?] == tag) {
        try self.advance();
        return;
    }

    // Tolerate missing semicolon at end of REPL input
    if (self.flavor == .Repl and
        tag == .Semicolon and
        self.scanner != null and
        self.scanner.?.current.offset >= self.scanner.?.source.len)
    {
        return;
    }

    // If in repl mode and unclosed brace, we dont print out the error
    switch (tag) {
        .RightBracket,
        .RightParen,
        .RightBrace,
        => {
            if (self.flavor == .Repl) {
                self.reporter.panic_mode = true;
                self.reporter.last_error = .unclosed;
            } else {
                self.reporter.reportErrorFmt(
                    .unclosed,
                    self.ast.tokens.get(self.current_token.? - 1),
                    self.ast.tokens.get(self.current_token.? - 1),
                    "{s}",
                    .{
                        message,
                    },
                );
            }
        },
        else => self.reporter.reportErrorFmt(
            .syntax,
            self.ast.tokens.get(self.current_token.? - 1),
            self.ast.tokens.get(self.current_token.? - 1),
            "{s}",
            .{
                message,
            },
        ),
    }
}

// Check next token
fn check(self: *Self, tag: Token.Type) bool {
    return self.ast.tokens.items(.tag)[self.current_token.?] == tag;
}

// Check `n` tokens ahead
fn checkAhead(self: *Self, tag: Token.Type, n: usize) !bool {
    // Parse tokens if we didn't already look that far ahead
    while (n + 1 > self.ast.tokens.len - self.current_token.? - 1) {
        while (true) {
            const token = try self.scanner.?.scanToken();
            _ = try self.ast.appendToken(token);

            if (token.tag == .Eof) {
                return false;
            }

            if (token.tag != .Error) {
                break;
            }

            // If error, report it and keep checking ahead
            self.reporter.reportErrorFmt(
                .syntax,
                self.ast.tokens.get(self.current_token.? - 1),
                self.ast.tokens.get(self.current_token.? - 1),
                "{s}",
                .{
                    token.literal_string orelse "Unknown error.",
                },
            );
        }
    }

    return self.ast.tokens.items(.tag)[self.current_token.? + n + 1] == tag;
}

fn match(self: *Self, tag: Token.Type) !bool {
    if (!self.check(tag)) {
        return false;
    }

    try self.advance();

    return true;
}

fn matchOpEqual(self: *Self) !bool {
    return try self.match(.Equal) or
        try self.match(.PlusEqual) or
        try self.match(.MinusEqual) or
        try self.match(.StarEqual) or
        try self.match(.SlashEqual) or
        try self.match(.ShiftRightEqual) or
        try self.match(.ShiftLeftEqual) or
        try self.match(.XorEqual) or
        try self.match(.BorEqual) or
        try self.match(.BnotEqual) or
        try self.match(.AmpersandEqual) or
        try self.match(.PercentEqual);
}

/// Insert token in ast and advance over it to avoid confusing the parser
fn insertUtilityToken(self: *Self, token: Token) !Ast.TokenIndex {
    const current_token = self.ast.tokens.get(self.current_token.?);

    try self.ast.tokens.insert(
        self.gc.allocator,
        self.current_token.?,
        token,
    );

    const utility_token = self.current_token.?;

    self.current_token = try self.ast.appendToken(current_token);

    return utility_token;
}

// Skip tokens until we reach something that resembles a new statement
fn synchronize(self: *Self) !void {
    self.reporter.panic_mode = false;

    while (self.ast.tokens.items(.tag)[self.current_token.?] != .Eof) : (try self.advance()) {
        if (self.ast.tokens.items(.tag)[self.current_token.? - 1] == .Semicolon) {
            return;
        }

        switch (self.ast.tokens.items(.tag)[self.current_token.?]) {
            .Object,
            .Enum,
            .Test,
            .Fun,
            .Final,
            .If,
            .While,
            .Do,
            .For,
            .ForEach,
            .Return,
            .Throw,
            .Break,
            .Continue,
            .Export,
            .Import,
            .Zdef,
            .From,
            .Var,
            .Yield,
            .Resume,
            .Resolve,
            .Protocol,
            .Try,
            .Static,
            .Extern,
            .Namespace,
            => return,
            else => {},
        }
    }
}

pub fn parse(self: *Self, source: []const u8, file_name: ?[]const u8, name: []const u8) !?Ast {
    if (self.scanner != null) {
        self.scanner = null;
    }

    self.scanner = Scanner.init(self.gc.allocator, file_name orelse name, source);

    const function_type: obj.ObjFunction.FunctionType = if (!self.imported and self.flavor == .Repl)
        .Repl
    else if (self.imported)
        .Script
    else
        .ScriptEntryPoint;

    const function_name: []const u8 = switch (function_type) {
        .EntryPoint => "main",
        .ScriptEntryPoint, .Script => name,
        .Repl => "REPL",
        else => "???",
    };

    const body_node = try self.ast.appendNode(
        .{
            .tag = .Block,
            .location = 0,
            .end_location = 0,
            .components = .{
                .Block = try self.gc.allocator.alloc(Ast.Node.Index, 0),
            },
        },
    );

    const function_node = try self.ast.appendNode(
        .{
            .tag = .Function,
            .location = 0,
            .end_location = undefined,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Function,
                    .resolved_type = .{
                        .Function = .{
                            .id = obj.ObjFunction.FunctionDef.nextId(),
                            .name = try self.gc.copyString(function_name),
                            .script_name = try self.gc.copyString(name),
                            .return_type = self.gc.type_registry.void_type,
                            .yield_type = self.gc.type_registry.void_type,
                            .function_type = function_type,
                        },
                    },
                },
            ),
            .components = .{
                .Function = .{
                    .function_signature = null,
                    .id = Ast.Function.nextId(),
                    .upvalue_binding = .{},
                    .body = body_node,
                    .identifier = 0,
                },
            },
        },
    );

    var entry = Ast.Function.Entry{
        .test_slots = undefined,
        .test_locations = undefined,
    };

    self.script_name = name;

    try self.beginFrame(function_type, function_node, null);

    self.reporter.last_error = null;
    self.reporter.panic_mode = false;

    try self.advancePastEof();

    while (!(try self.match(.Eof))) {
        if (self.declarationOrStatement(null) catch |err| {
            if (function_type != .Repl and err == error.ReachedMaximumMemoryUsage) {
                return err;
            }

            if (BuildOptions.debug) {
                io.print("Parsing of `{s}` failed with error {} (collected {} errors)\n", .{ function_name, err, self.reporter.reports.items.len });
            }
            return null;
        }) |decl| {
            var statements = std.ArrayList(Ast.Node.Index).fromOwnedSlice(
                self.gc.allocator,
                @constCast(self.ast.nodes.items(.components)[body_node].Block),
            );

            try statements.append(decl);

            self.ast.nodes.items(.components)[body_node].Block = try statements.toOwnedSlice();
        } else {
            self.reporter.reportErrorAt(
                .syntax,
                self.ast.tokens.get(self.current_token.? - 1),
                self.ast.tokens.get(self.current_token.? - 1),
                "Expected statement",
            );
            break;
        }
    }

    // If top level, search `main` or `test` function(s) and call them
    // Then put any exported globals on the stack
    if (function_type == .ScriptEntryPoint) {
        for (self.globals.items, 0..) |global, index| {
            if (std.mem.eql(u8, self.ast.tokens.items(.lexeme)[global.name[global.name.len - 1]], "main") and
                !global.hidden and
                (self.namespace == null or
                    global.name.len == 1 or
                    global.matchNamespace(self.ast, self.namespace.?)))
            {
                entry.main_slot = index;
                entry.push_cli_args = global.type_def.resolved_type.?.Function.parameters.count() > 0;
                entry.main_location = global.name[global.name.len - 1];
                break;
            }
        }
    }

    var test_slots = std.ArrayList(usize).init(self.gc.allocator);
    var test_locations = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
    // Create an entry point wich runs all `test`
    for (self.globals.items, 0..) |global, index| {
        if (global.type_def.def_type == .Function and global.type_def.resolved_type.?.Function.function_type == .Test) {
            try test_slots.append(index);
            try test_locations.append(global.name[0]);
        }
    }

    entry.test_slots = try test_slots.toOwnedSlice();
    entry.test_locations = try test_locations.toOwnedSlice();

    // If we're being imported, put all globals on the stack
    if (self.imported) {
        entry.exported_count = self.globals.items.len;
    }

    for (self.globals.items) |global| {
        // Check there's no more root placeholders
        if (global.type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast.slice(), global.type_def.resolved_type.?.Placeholder);
        }
    }

    // Check there's no unreferenced imports
    if (self.flavor != .Repl) {
        var it = self.script_imports.iterator();
        while (it.next()) |kv| {
            if (!kv.value_ptr.*.referenced) {
                self.reporter.warnFmt(
                    .unused_import,
                    self.ast.tokens.get(kv.value_ptr.*.location),
                    self.ast.tokens.get(kv.value_ptr.*.end_location),
                    "Unused import",
                    .{},
                );
            }
        }
    }

    self.ast.nodes.items(.components)[function_node].Function.entry = entry;

    self.ast.root = if (self.reporter.last_error != null) null else self.endFrame();

    return if (self.reporter.last_error != null) null else self.ast;
}

fn beginFrame(self: *Self, function_type: obj.ObjFunction.FunctionType, function_node: Ast.Node.Index, this: ?*obj.ObjTypeDef) !void {
    const enclosing = self.current;
    // FIXME: is this ever deallocated?
    self.current = try self.gc.allocator.create(Frame);
    self.current.?.* = Frame{
        .locals = [_]Local{undefined} ** 255,
        .upvalues = [_]UpValue{undefined} ** 255,
        .enclosing = enclosing,
        .function_node = function_node,
    };

    if (function_type == .Extern) {
        return;
    }

    // First local is reserved for an eventual `this` or cli arguments
    var local = &self.current.?.locals[self.current.?.local_count];
    self.current.?.local_count += 1;
    local.depth = 0;
    local.captured = false;
    local.node = function_node;

    switch (function_type) {
        .Method => {
            std.debug.assert(this != null);

            local.type_def = this.?;
        },
        .EntryPoint, .ScriptEntryPoint => {
            // `args` is [str]
            local.type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .List,
                    .resolved_type = .{
                        .List = obj.ObjList.ListDef.init(
                            self.gc.type_registry.str_type,
                            false,
                        ),
                    },
                },
            );
        },
        else => {
            local.type_def = self.gc.type_registry.void_type;
        },
    }

    var location = Token.identifier(
        switch (function_type) {
            .Method => "this",
            .EntryPoint => "$args",
            .ScriptEntryPoint => "$args",
            else => "_",
        },
    );
    location.source = self.scanner.?.source;
    location.script_name = self.script_name;

    if (function_type == .Method) {
        const function_location = self.ast.tokens.get(
            self.ast.nodes.items(.location)[function_node],
        );
        location.line = function_location.line;
        location.column = function_location.column;
        location.offset = function_location.offset;
    }

    if (self.current_token != null) {
        local.name = try self.insertUtilityToken(location);
    } else {
        local.name = try self.ast.appendToken(location);
        _ = try self.advance();
    }
}

fn endFrame(self: *Self) Ast.Node.Index {
    var i: usize = 0;
    while (i < self.current.?.local_count) : (i += 1) {
        const local = self.current.?.locals[i];

        if (self.flavor != .Repl) {
            // Check discarded locals
            if (!local.isReferenced(self.ast)) {
                const location = self.ast.tokens.get(local.name);
                self.reporter.warnFmt(
                    .unused_argument,
                    location,
                    location,
                    "Local `{s}` is never referenced",
                    .{
                        self.ast.tokens.items(.lexeme)[local.name],
                    },
                );
            }

            // Check var local never assigned
            if (!local.isAssigned(self.ast)) {
                const location = self.ast.tokens.get(local.name);
                self.reporter.warnFmt(
                    .unassigned_final_local,
                    location,
                    location,
                    "Local `{s}` is declared `var` but is never assigned",
                    .{
                        self.ast.tokens.items(.lexeme)[local.name],
                    },
                );
            }
        }
    }

    // If global scope, check unused globals
    const function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;
    if (function_type == .Script or function_type == .ScriptEntryPoint) {
        for (self.globals.items) |global| {
            if (!global.isReferenced(self.ast)) {
                const type_def_str = global.type_def.toStringAlloc(self.gc.allocator) catch unreachable;
                defer self.gc.allocator.free(type_def_str);

                const location = self.ast.tokens.get(global.name[0]);

                self.reporter.warnFmt(
                    .unused_argument,
                    location,
                    location,
                    "Unused global of type `{s}`",
                    .{
                        type_def_str,
                    },
                );
            }
        }
    }

    self.current.?.deinit(self.gc.allocator);

    const current_node = self.current.?.function_node;
    self.current = self.current.?.enclosing;

    return current_node;
}

fn beginScope(self: *Self, at: ?Ast.Node.Index) !void {
    try self.current.?.scopes.append(self.gc.allocator, at);
    self.current.?.scope_depth += 1;
}

fn endScope(self: *Self) ![]Chunk.OpCode {
    const current = self.current.?;
    _ = current.scopes.pop();
    var closing = std.ArrayListUnmanaged(Chunk.OpCode){};
    current.scope_depth -= 1;

    while (current.local_count > 0 and current.locals[current.local_count - 1].depth > current.scope_depth) {
        const local = current.locals[current.local_count - 1];

        if (local.captured) {
            try closing.append(self.gc.allocator, .OP_CLOSE_UPVALUE);
        } else {
            try closing.append(self.gc.allocator, .OP_POP);
        }

        current.local_count -= 1;
    }

    return try closing.toOwnedSlice(self.gc.allocator);
}

fn closeScope(self: *Self, upto_depth: usize) ![]Chunk.OpCode {
    const current = self.current.?;
    var closing = std.ArrayList(Chunk.OpCode).init(self.gc.allocator);

    var local_count = current.local_count;
    while (local_count > 0 and current.locals[local_count - 1].depth > upto_depth - 1) {
        if (current.locals[local_count - 1].captured) {
            try closing.append(.OP_CLOSE_UPVALUE);
        } else {
            try closing.append(.OP_POP);
        }

        local_count -= 1;
    }

    return try closing.toOwnedSlice();
}

inline fn getRule(token: Token.Type) ParseRule {
    return rules[@intFromEnum(token)];
}

fn parsePrecedence(self: *Self, precedence: Precedence, hanging: bool) Error!Ast.Node.Index {
    // In case we are already parsing an expression, the current unwrap chain should not impact deeper expressions
    // Exemple: canBeNull?.aMap[expression] <- here `expression` should not be transformed into an optional
    const previous_opt_jumps = self.opt_jumps;
    self.opt_jumps = null;

    // If hanging is true, that means we already read the start of the expression
    if (!hanging) {
        _ = try self.advance();
    }

    const prefixRule: ?ParseFn = getRule(self.ast.tokens.items(.tag)[self.current_token.? - 1]).prefix;
    if (prefixRule == null) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Expected expression.",
        );

        // TODO: find a way to continue or catch that error
        return CompileError.Unrecoverable;
    }

    const canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
    var node = try prefixRule.?(self, canAssign);

    while (@intFromEnum(getRule(self.ast.tokens.items(.tag)[self.current_token.?]).precedence) >= @intFromEnum(precedence)) {
        // Patch optional jumps
        if (self.opt_jumps) |jumps| {
            std.debug.assert(jumps.items.len > 0);
            // If precedence is less than the precedence that started the nullable chain, stop the chain there
            if (@intFromEnum(getRule(self.ast.tokens.items(.tag)[self.current_token.?]).precedence) < @intFromEnum(jumps.items[0])) {
                jumps.deinit();
                self.opt_jumps = null;

                self.ast.nodes.items(.patch_opt_jumps)[node] = true;

                const node_type_def_ptr = &self.ast.nodes.items(.type_def)[node];
                if (node_type_def_ptr.* != null) {
                    node_type_def_ptr.* = try node_type_def_ptr.*.?.cloneOptional(&self.gc.type_registry);
                }
            }
        }

        _ = try self.advance();

        if (getRule(self.ast.tokens.items(.tag)[self.current_token.? - 1]).infix) |infixRule| {
            node = try infixRule(self, canAssign, node);
        } else {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorFmt(
                .syntax,
                location,
                location,
                "Unexpected token {s}",
                .{
                    @tagName(self.ast.tokens.items(.tag)[self.current_token.? - 1]),
                },
            );
        }
    }

    // If nullable chain still there, stop it now at the end of the expression
    if (self.opt_jumps) |jumps| {
        jumps.deinit();
        self.opt_jumps = null;

        self.ast.nodes.items(.patch_opt_jumps)[node] = true;

        const node_type_def = self.ast.nodes.items(.type_def)[node];
        if (node_type_def != null) {
            self.ast.nodes.items(.type_def)[node] = try node_type_def.?.cloneOptional(&self.gc.type_registry);
        }
    }

    if (canAssign and (try self.match(.Equal))) {
        self.reporter.reportErrorAt(
            .assignable,
            self.ast.tokens.get(self.ast.nodes.items(.location)[node]),
            self.ast.tokens.get(self.ast.nodes.items(.end_location)[node]),
            "Invalid assignment target.",
        );
    }

    self.opt_jumps = previous_opt_jumps;

    return node;
}

fn block(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var statements = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        if (try self.declarationOrStatement(loop_scope)) |declOrStmt| {
            try statements.append(declOrStmt);
        }
    }

    try self.consume(.RightBrace, "Expected `}` after block.");

    return try self.ast.appendNode(
        .{
            .tag = .Block,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Block = try statements.toOwnedSlice(),
            },
        },
    );
}

fn simpleType(self: *Self, def_type: obj.ObjTypeDef.Type) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const optional = try self.match(.Question);

    return try self.ast.appendNode(
        .{
            .tag = .SimpleType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = def_type,
                    .optional = optional,
                },
            ),
            .components = .{
                .SimpleType = {},
            },
        },
    );
}

fn simpleTypeFromToken(token: Token.Type) ?obj.ObjTypeDef.Type {
    return switch (token) {
        .Pat => .Pattern,
        .Ud => .UserData,
        .Str => .String,
        .Int => .Integer,
        .Double => .Double,
        .Bool => .Bool,
        .Range => .Range,
        .Type => .Type,
        .Any => .Any,
        else => null,
    };
}

fn declaration(self: *Self, docblock: ?Ast.TokenIndex) Error!?Ast.Node.Index {
    const global_scope = self.current.?.scope_depth == 0;

    const node = if (try self.match(.Object))
        try self.objectDeclaration()
    else if (try self.match(.Protocol))
        try self.protocolDeclaration()
    else if (try self.match(.Enum))
        try self.enumDeclaration()
    else if ((try self.match(.Fun)) or (global_scope and try self.match(.Extern)))
        try self.funDeclaration()
    else if ((try self.match(.Final)) or
        (try self.match(.Var) or
            (self.check(.Identifier) and std.mem.eql(u8, "_", self.ast.tokens.items(.lexeme)[self.current_token.?]))))
    variable: {
        const final = self.current_token.? > 0 and self.ast.tokens.items(.tag)[self.current_token.? - 1] == .Final;
        try self.consume(.Identifier, "Expected identifier");
        const identifier = self.current_token.? - 1;

        // Type omitted?
        if (!(try self.match(.Colon))) {
            break :variable try self.varDeclaration(
                identifier,
                null,
                .Semicolon,
                final,
                true,
                false,
            );
        }

        // Mutable?
        const mutable = try self.match(.Mut);

        // Simple types
        for ([_]Token.Type{ .Pat, .Ud, .Str, .Int, .Double, .Bool, .Range, .Type, .Any }) |token| {
            if (try self.match(token)) {
                break :variable try self.varDeclaration(
                    identifier,
                    try self.simpleType(simpleTypeFromToken(token).?),
                    .Semicolon,
                    final,
                    true,
                    false,
                );
            }
        }

        // Complex types
        if (try self.match(.Fib))
            break :variable try self.varDeclaration(
                identifier,
                try self.parseFiberType(null),
                .Semicolon,
                final,
                true,
                false,
            )
        else if (try self.match(.Obj))
            break :variable try self.varDeclaration(
                identifier,
                try self.parseObjType(null),
                .Semicolon,
                final,
                true,
                false,
            )
        else if (try self.match(.LeftBracket))
            break :variable try self.varDeclaration(
                identifier,
                try self.parseListType(null, mutable),
                .Semicolon,
                final,
                true,
                false,
            )
        else if (try self.match(.LeftBrace))
            break :variable try self.varDeclaration(
                identifier,
                try self.parseMapType(null, mutable),
                .Semicolon,
                final,
                true,
                false,
            )
        else if (try self.match(.Fun))
            break :variable try self.varDeclaration(
                identifier,
                try self.parseFunctionType(null),
                .Semicolon,
                final,
                true,
                false,
            )
        else if (try self.match(.Identifier)) {
            break :variable try self.userVarDeclaration(
                identifier,
                final,
                mutable,
            );
        }

        break :variable null;
    } else null;

    if (node != null and docblock != null) {
        if (self.ast.nodes.items(.tag)[node.?] == .FunDeclaration) {
            const components = self.ast.nodes.items(.components);
            components[components[node.?].FunDeclaration.function].Function.docblock = docblock;
        }
        self.ast.nodes.items(.docblock)[node.?] = docblock;
    }

    if (self.reporter.panic_mode) {
        try self.synchronize();
    }

    return node;
}

fn declarationOrStatement(self: *Self, loop_scope: ?LoopScope) !?Ast.Node.Index {
    const global_scope = self.current.?.scope_depth == 0;
    const docblock = if (global_scope and try self.match(.Docblock))
        self.current_token.? - 1
    else
        null;

    return try self.declaration(docblock) orelse
        try self.statement(
            docblock,
            false,
            loop_scope,
        );
}

// When a break statement, will return index of jump to patch
fn statement(self: *Self, docblock: ?Ast.TokenIndex, hanging: bool, loop_scope: ?LoopScope) !?Ast.Node.Index {
    const global_scope = self.current.?.scope_depth == 0;
    const statement_allowed = self.flavor == .Repl or !global_scope;

    if (statement_allowed) {
        if (try self.match(.If)) {
            std.debug.assert(!hanging);
            return try self.ifStatement(loop_scope);
        } else if (try self.match(.For)) {
            std.debug.assert(!hanging);
            return try self.forStatement();
        } else if (try self.match(.ForEach)) {
            std.debug.assert(!hanging);
            return try self.forEachStatement();
        } else if (try self.match(.While)) {
            std.debug.assert(!hanging);
            return try self.whileStatement();
        } else if (try self.match(.Do)) {
            std.debug.assert(!hanging);
            return try self.doUntilStatement();
        } else if (!global_scope and try self.match(.Return)) {
            std.debug.assert(!hanging);
            return try self.returnStatement();
        } else if (try self.match(.Try)) {
            std.debug.assert(!hanging);
            return try self.tryStatement();
        } else if (!global_scope and try self.match(.Break)) {
            std.debug.assert(!hanging);
            return try self.breakStatement(loop_scope);
        } else if (!global_scope and try self.match(.Continue)) {
            std.debug.assert(!hanging);
            return try self.continueStatement(loop_scope);
        } else if (try self.match(.Out)) {
            std.debug.assert(!hanging);
            return try self.outStatement();
        } else if (try self.match(.Throw)) {
            return try self.throw();
        }
    }

    if (global_scope) {
        if (try self.match(.Import)) {
            std.debug.assert(!hanging);
            return try self.importStatement();
        } else if (try self.match(.Namespace)) {
            std.debug.assert(!hanging);
            return try self.namespaceStatement();
        } else if (try self.match(.Test)) {
            std.debug.assert(!hanging);
            return try self.testStatement();
        } else if (try self.match(.Zdef)) {
            std.debug.assert(!hanging);
            return try self.zdefStatement();
        } else if (try self.match(.Export)) {
            std.debug.assert(!hanging);
            return try self.exportStatement(docblock);
        }
    }

    return try self.expressionStatement(hanging);
}

fn addLocal(self: *Self, node: Ast.Node.Index, name: Ast.TokenIndex, local_type: *obj.ObjTypeDef, final: bool, mutable: bool) Error!usize {
    if (self.current.?.local_count == std.math.maxInt(u8)) {
        const location = self.ast.tokens.get(name);
        self.reporter.reportErrorAt(
            .locals_count,
            location,
            location,
            "Too many local variables in scope.",
        );
        return 0;
    }

    const function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;
    self.current.?.locals[self.current.?.local_count] = Local{
        .name = name,
        .node = node,
        .depth = -1,
        .captured = false,
        .mutable = mutable,
        .type_def = local_type,
        .final = final,
        // Extern and abstract function arguments are considered referenced
        .referenced = function_type == .Extern or function_type == .Abstract,
    };

    self.current.?.local_count += 1;

    return self.current.?.local_count - 1;
}

fn resolveReferrer(self: *Self, referrer: Ast.Node.Index, definition: Ast.Node.Index) Error!void {
    std.debug.assert(definition != 0);
    switch (self.ast.nodes.items(.tag)[referrer]) {
        .NamedVariable => {
            self.ast.nodes.items(.components)[referrer].NamedVariable.definition = definition;
        },
        else => {},
    }
}

fn addGlobal(self: *Self, node: Ast.Node.Index, name: Ast.TokenIndex, global_type: *obj.ObjTypeDef, final: bool, mutable: bool) Error!usize {
    const lexemes = self.ast.tokens.items(.lexeme);
    // Search for an existing placeholder global with the same name
    for (self.globals.items, 0..) |*global, index| {
        if (global.type_def.def_type == .Placeholder and
            (self.namespace == null or global.matchNamespace(self.ast, self.namespace.?)) and
            std.mem.eql(u8, lexemes[global.name[global.name.len - 1]], lexemes[name]))
        {
            global.exported = self.exporting;

            if (global_type.def_type != .Placeholder) {
                try self.resolvePlaceholder(global.type_def, global_type, final);

                if (self.flavor == .Ast) {
                    for (global.placeholder_referrers.items) |referrer| {
                        try self.resolveReferrer(referrer, node);
                    }

                    global.placeholder_referrers.deinit(self.gc.allocator);
                }
            }

            return index;
        }
    }

    if (self.globals.items.len == std.math.maxInt(u24)) {
        const location = self.ast.tokens.get(name);

        self.reporter.reportErrorAt(
            .globals_count,
            location,
            location,
            "Too many global variables.",
        );
        return 0;
    }

    var qualified_name = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
    if (self.namespace) |namespace| {
        try qualified_name.appendSlice(namespace);
    }
    try qualified_name.append(name);

    try self.globals.append(
        self.gc.allocator,
        .{
            .name = try qualified_name.toOwnedSlice(),
            .node = node,
            .type_def = global_type,
            .final = final,
            .mutable = mutable,
            .exported = self.exporting,
        },
    );

    return self.globals.items.len - 1;
}

fn resolveGeneric(self: *Self, name: *obj.ObjString) ?*obj.ObjTypeDef {
    return if (self.current_object != null and self.current_object.?.generics != null)
        self.current_object.?.generics.?.get(name) orelse self.current.?.resolveGeneric(name)
    else
        self.current.?.resolveGeneric(name);
}

fn resolveLocal(self: *Self, frame: *Frame, name: Ast.TokenIndex) !?usize {
    if (frame.local_count == 0) {
        return null;
    }

    const lexemes = self.ast.tokens.items(.lexeme);
    const lexeme = lexemes[name];

    if (std.mem.eql(u8, lexeme, "_")) {
        return null;
    }

    var i: usize = frame.local_count - 1;
    while (i >= 0) : (i -= 1) {
        var local = &frame.locals[i];
        if (std.mem.eql(u8, lexeme, lexemes[local.name])) {
            if (local.depth == -1) {
                const location = self.ast.tokens.get(local.name);

                self.reporter.reportErrorFmt(
                    .local_initializer,
                    location,
                    location,
                    "Can't read local variable `{s}` in its own initializer.",
                    .{
                        lexeme,
                    },
                );
            }

            local.referenced = true;
            return i;
        }

        if (i == 0) break;
    }

    return null;
}

// Will consume tokens if find a prefixed identifier
pub fn resolveGlobal(self: *Self, referrer: Ast.Node.Index, name: []const Ast.TokenIndex) Error!?usize {
    if (self.globals.items.len == 0) {
        return null;
    }

    const lexemes = self.ast.tokens.items(.lexeme);
    if (name.len == 1 and std.mem.eql(u8, lexemes[name[name.len - 1]], "_")) {
        return null;
    }

    var i: usize = self.globals.items.len - 1;
    while (i >= 0) : (i -= 1) {
        const global: *Global = &self.globals.items[i];

        if (global.matchName(
            self.ast,
            if (name.len > 1)
                name[0 .. name.len - 1]
            else
                self.namespace orelse &[_]Ast.TokenIndex{},
            name[name.len - 1],
        ) and
            !global.hidden)
        {
            if (!global.initialized) {
                const location = self.ast.tokens.get(global.name[0]);

                self.reporter.reportErrorFmt(
                    .global_initializer,
                    location,
                    if (global.name.len > 1)
                        self.ast.tokens.get(global.name[global.name.len - 1])
                    else
                        location,
                    "Can't read global `{s}` variable in its own initializer.",
                    .{
                        lexemes[global.name[global.name.len - 1]],
                    },
                );
            }

            global.referenced = true;

            if (self.flavor == .Ast and global.type_def.def_type == .Placeholder) {
                try global.placeholder_referrers.append(self.gc.allocator, referrer);
            }

            return i;
            // Is it an import prefix?
        }

        if (i == 0) break;
    }

    return null;
}

fn resolvePlaceholderWithRelation(
    self: *Self,
    child: *obj.ObjTypeDef,
    resolved_type: *obj.ObjTypeDef,
    final: bool,
    relation: obj.PlaceholderDef.PlaceholderRelation,
) Error!void {
    const child_placeholder = child.resolved_type.?.Placeholder;
    const child_placeholder_name = self.ast.tokens.items(.lexeme)[child_placeholder.where];

    if (BuildOptions.debug_placeholders) {
        io.print(
            "Attempts to resolve @{} child placeholder @{} ({s}) with relation {s}\n",
            .{
                @intFromPtr(resolved_type),
                @intFromPtr(child),
                self.ast.tokens.items(.lexeme)[child_placeholder.where],
                @tagName(child_placeholder.parent_relation.?),
            },
        );
    }

    switch (relation) {
        .GenericResolve => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.populateGenerics(
                    self.current_token.? - 1,
                    switch (resolved_type.def_type) {
                        .Function => resolved_type.resolved_type.?.Function.id,
                        .Object => resolved_type.resolved_type.?.Object.id,
                        else => null,
                    },
                    child_placeholder.resolved_generics.?,
                    &self.gc.type_registry,
                    null,
                ),
                true,
            );
        },
        .Optional => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.cloneOptional(&self.gc.type_registry),
                false,
            );
        },
        .Unwrap => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.cloneNonOptional(&self.gc.type_registry),
                false,
            );
        },
        .Instance => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.toInstance(
                    &self.gc.type_registry,
                    child.isMutable(),
                ),
                false,
            );
        },
        .Parent => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.toParentType(&self.gc.type_registry),
                false,
            );
        },
        .Call => {
            // Can we call the parent?
            if (resolved_type.def_type != .Function) {
                self.reporter.reportErrorFmt(
                    .callable,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "`{s}` can't be called",
                    .{
                        try resolved_type.toStringAlloc(self.gc.allocator),
                    },
                );
                return;
            }

            try self.resolvePlaceholder(
                child,
                resolved_type.resolved_type.?.Function.return_type,
                false,
            );
        },
        .Yield => {
            // Can we call the parent?
            if (resolved_type.def_type != .Function) {
                self.reporter.reportErrorFmt(
                    .callable,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "`{s}` can't be called",
                    .{
                        try resolved_type.toStringAlloc(self.gc.allocator),
                    },
                );
                return;
            }

            try self.resolvePlaceholder(
                child,
                resolved_type.resolved_type.?.Function.yield_type,
                false,
            );
        },
        .Subscript => {
            if (resolved_type.def_type == .List) {
                try self.resolvePlaceholder(
                    child,
                    resolved_type.resolved_type.?.List.item_type,
                    false,
                );
            } else if (resolved_type.def_type == .Map) {
                try self.resolvePlaceholder(
                    child,
                    try resolved_type.resolved_type.?.Map.value_type.cloneOptional(&self.gc.type_registry),
                    false,
                );
            } else if (resolved_type.def_type == .String) {
                try self.resolvePlaceholder(
                    child,
                    self.gc.type_registry.str_type,
                    false,
                );
            } else {
                self.reporter.reportErrorFmt(
                    .map_key_type,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "`{s}` can't be subscripted",
                    .{
                        try resolved_type.toStringAlloc(self.gc.allocator),
                    },
                );
                return;
            }
        },
        .UnwrappedSubscript => {
            if (resolved_type.def_type == .List) {
                try self.resolvePlaceholder(
                    child,
                    resolved_type.resolved_type.?.List.item_type,
                    false,
                );
            } else if (resolved_type.def_type == .Map) {
                try self.resolvePlaceholder(
                    child,
                    resolved_type.resolved_type.?.Map.value_type,
                    false,
                );
            } else if (resolved_type.def_type == .String) {
                try self.resolvePlaceholder(
                    child,
                    self.gc.type_registry.str_type,
                    false,
                );
            } else {
                self.reporter.reportErrorFmt(
                    .map_key_type,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "`{s}` can't be subscripted",
                    .{
                        try resolved_type.toStringAlloc(self.gc.allocator),
                    },
                );
                return;
            }
        },
        .Key => {
            if (resolved_type.def_type == .Map) {
                try self.resolvePlaceholder(
                    child,
                    resolved_type.resolved_type.?.Map.key_type,
                    false,
                );
            } else if (resolved_type.def_type == .List or resolved_type.def_type == .String) {
                try self.resolvePlaceholder(
                    child,
                    self.gc.type_registry.int_type,
                    false,
                );
            } else {
                self.reporter.reportErrorFmt(
                    .map_key_type,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "Bad key type for `{s}`",
                    .{
                        try resolved_type.toStringAlloc(self.gc.allocator),
                    },
                );
                return;
            }
        },
        .FieldAccess => {
            switch (resolved_type.def_type) {
                .List => {
                    if (try obj.ObjList.ListDef.member(
                        resolved_type,
                        self,
                        child_placeholder_name,
                    )) |member| {
                        try self.resolvePlaceholder(child, member.type_def, false);
                    }
                },
                .Map => {
                    if (try obj.ObjMap.MapDef.member(
                        resolved_type,
                        self,
                        child_placeholder_name,
                    )) |member| {
                        try self.resolvePlaceholder(child, member.type_def, false);
                    }
                },
                .String => {
                    if (try obj.ObjString.memberDefByName(
                        self,
                        child_placeholder_name,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Pattern => {
                    if (try obj.ObjPattern.memberDefByName(
                        self,
                        child_placeholder_name,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Fiber => {
                    if (try obj.ObjFiber.memberDefByName(
                        self,
                        child_placeholder_name,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Object => {
                    // We can't create a field access placeholder without a name
                    const object_def = resolved_type.resolved_type.?.Object;

                    // Search for a field matching the placeholder
                    if (object_def.fields.get(child_placeholder_name)) |field| {
                        // TODO: remove? should only resolve with a field if field accessing an object instance?
                        try self.resolvePlaceholder(
                            child,
                            field.type_def,
                            field.final,
                        );
                    } else {
                        self.reporter.reportErrorFmt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where),
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where_end),
                            "`{s}` has no static field `{s}`",
                            .{
                                object_def.name.string,
                                child_placeholder_name,
                            },
                        );
                    }
                },
                .ObjectInstance => {
                    // We can't create a field access placeholder without a name
                    const object_def = resolved_type.resolved_type.?.ObjectInstance.of.resolved_type.?.Object;

                    // Search for a field matching the placeholder
                    if (object_def.fields.get(child_placeholder_name)) |field| {
                        try self.resolvePlaceholder(
                            child,
                            field.type_def,
                            field.final,
                        );
                    } else {
                        self.reporter.reportErrorFmt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where),
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where_end),
                            "`{s}` has no field `{s}`",
                            .{
                                object_def.name.string,
                                child_placeholder_name,
                            },
                        );
                    }
                },
                .ForeignContainer => {
                    // We can't create a field access placeholder without a name
                    const f_def = resolved_type.resolved_type.?.ForeignContainer;

                    // Search for a field matching the placeholder
                    if (f_def.buzz_type.get(child_placeholder_name)) |field| {
                        try self.resolvePlaceholder(child, field, false);
                    } else {
                        self.reporter.reportErrorFmt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where),
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where_end),
                            "`{s}` has no field `{s}`",
                            .{
                                f_def.name.string,
                                child_placeholder_name,
                            },
                        );
                    }
                },
                .ProtocolInstance => {
                    // We can't create a field access placeholder without a name
                    const protocol_def = resolved_type.resolved_type.?.ProtocolInstance.of.resolved_type.?.Protocol;

                    // Search for a field matching the placeholder
                    if (protocol_def.methods.get(child_placeholder_name)) |method_def| {
                        try self.resolvePlaceholder(child, method_def.type_def, true);
                    } else {
                        self.reporter.reportErrorFmt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where),
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where_end),
                            "`{s}` has no method `{s}`",
                            .{
                                protocol_def.name.string,
                                child_placeholder_name,
                            },
                        );
                    }
                },
                .Enum => {
                    // We can't create a field access placeholder without a name
                    const enum_def = resolved_type.resolved_type.?.Enum;

                    // Search for a case matching the placeholder
                    for (enum_def.cases) |case| {
                        if (std.mem.eql(u8, case, child_placeholder_name)) {
                            try self.resolvePlaceholder(
                                child,
                                try self.gc.type_registry.getTypeDef(
                                    .{
                                        .def_type = .EnumInstance,
                                        .resolved_type = .{
                                            .EnumInstance = .{
                                                .of = resolved_type,
                                                .mutable = false,
                                            },
                                        },
                                    },
                                ),
                                true,
                            );
                            break;
                        }
                    }
                },
                .EnumInstance => {
                    if (std.mem.eql(u8, "value", child_placeholder_name)) {
                        try self.resolvePlaceholder(
                            child,
                            resolved_type.resolved_type.?.EnumInstance.of.resolved_type.?.Enum.enum_type,
                            false,
                        );
                    } else {
                        self.reporter.reportErrorAt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where),
                            self.ast.tokens.get(child.resolved_type.?.Placeholder.where_end),
                            "Enum instance only has field `value`",
                        );
                        return;
                    }
                },
                else => {
                    self.reporter.reportErrorFmt(
                        .field_access,
                        self.ast.tokens.get(child_placeholder.where),
                        self.ast.tokens.get(child_placeholder.where_end),
                        "`{s}` can't be field accessed",
                        .{
                            try resolved_type.toStringAlloc(self.gc.allocator),
                        },
                    );
                    return;
                },
            }
        },
        .Assignment => {
            if (final) {
                self.reporter.reportErrorAt(
                    .final,
                    self.ast.tokens.get(child_placeholder.where),
                    self.ast.tokens.get(child_placeholder.where_end),
                    "Is final.",
                );
                return;
            }

            // Is child type matching the parent?
            try self.resolvePlaceholder(
                child,
                // Assignment relation from a once Placeholder and now Object/Enum is creating an instance
                try resolved_type.toInstance(
                    &self.gc.type_registry,
                    child.resolved_type.?.Placeholder.mutable.?,
                ),
                false,
            );
        },
    }
}

// When we encounter the missing declaration we replace it with the resolved type.
// We then follow the chain of placeholders to see if their assumptions were correct.
// If not we raise a compile error.
pub fn resolvePlaceholder(self: *Self, placeholder: *obj.ObjTypeDef, resolved_type: *obj.ObjTypeDef, final: bool) Error!void {
    std.debug.assert(placeholder.def_type == .Placeholder);

    if (BuildOptions.debug_placeholders) {
        io.print("Attempts to resolve @{} ({s}) with @{} a {s}({})\n", .{
            @intFromPtr(placeholder),
            self.ast.tokens.items(.lexeme)[placeholder.resolved_type.?.Placeholder.where],
            @intFromPtr(resolved_type),
            @tagName(resolved_type.def_type),
            resolved_type.optional,
        });
    }

    // Both placeholders, we have to connect the child placeholder to a root placeholder so its not orphan
    if (resolved_type.def_type == .Placeholder) {
        if (BuildOptions.debug_placeholders) {
            io.print(
                "Replaced linked placeholder @{} ({s}) with rooted placeholder @{} ({s})\n",
                .{
                    @intFromPtr(placeholder),
                    self.ast.tokens.items(.lexeme)[placeholder.resolved_type.?.Placeholder.where],
                    @intFromPtr(resolved_type),
                    self.ast.tokens.items(.lexeme)[resolved_type.resolved_type.?.Placeholder.where],
                },
            );
        }

        if (resolved_type.resolved_type.?.Placeholder.parent) |parent| {
            if (parent.def_type == .Placeholder) {
                try parent.resolved_type.?.Placeholder.children.append(self.gc.allocator, placeholder);
            } else {
                // Parent already resolved, resolve this now orphan placeholder
                try self.resolvePlaceholderWithRelation(
                    resolved_type,
                    parent,
                    final,
                    resolved_type.resolved_type.?.Placeholder.parent_relation.?,
                );
            }
        }

        // Merge both placeholder children list
        // TODO: do we need this?
        // try resolved_type.resolved_type.?.Placeholder.children.appendSlice(placeholder.resolved_type.?.Placeholder.children.items);

        // Don't copy obj header or it will break the linked list of objects
        const o = placeholder.obj;
        placeholder.* = resolved_type.*;
        placeholder.obj = o;
        return;
    }

    var placeholder_def = placeholder.resolved_type.?.Placeholder;

    if (BuildOptions.debug_placeholders) {
        io.print(
            "Resolved placeholder @{} {s}({}) with @{} {s}({})\n",
            .{
                @intFromPtr(placeholder),
                self.ast.tokens.items(.lexeme)[placeholder.resolved_type.?.Placeholder.where],
                placeholder.optional,
                @intFromPtr(resolved_type),
                @tagName(resolved_type.def_type),
                resolved_type.optional,
            },
        );
    }

    // Overwrite placeholder with resolved_type
    // Don't copy obj header or it will break the linked list of objects
    const o = placeholder.obj;
    placeholder.* = resolved_type.*;
    placeholder.obj = o;
    // Put it in the registry so any cloneOptional/cloneNonOptional don't create new types
    // FIXME: if the type was already registered this makes an orphan copy of that type
    try self.gc.type_registry.setTypeDef(placeholder);

    // Now walk the chain of placeholders and see if they hold up
    for (placeholder_def.children.items) |child| {
        if (child.def_type == .Placeholder) {
            try self.resolvePlaceholderWithRelation(
                child,
                placeholder,
                final,
                child.resolved_type.?.Placeholder.parent_relation.?,
            );
        }
    }

    placeholder_def.deinit(self.gc.allocator);

    // TODO: should resolved_type be freed?
    // TODO: does this work with vm.type_defs? (i guess not)
}

fn addUpvalue(self: *Self, node: Ast.Node.Index, frame: *Frame, index: usize, is_local: bool) Error!usize {
    const upvalue_count: u8 = frame.upvalue_count;

    var i: usize = 0;
    while (i < upvalue_count) : (i += 1) {
        const upvalue: *UpValue = &frame.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return i;
        }
    }

    if (upvalue_count == std.math.maxInt(u8)) {
        const location = self.ast.tokens.get(self.current_token.? - 1);

        self.reporter.reportErrorAt(
            .closures_count,
            location,
            location,
            "Too many closure variables in function.",
        );
        return 0;
    }

    frame.upvalues[upvalue_count].is_local = is_local;
    frame.upvalues[upvalue_count].index = @as(u8, @intCast(index));
    frame.upvalues[upvalue_count].node = node;

    frame.upvalue_count += 1;

    return frame.upvalue_count - 1;
}

fn resolveUpvalue(self: *Self, frame: *Frame, name: Ast.TokenIndex) Error!?usize {
    if (frame.enclosing == null) {
        return null;
    }

    const local: ?usize = try self.resolveLocal(frame.enclosing.?, name);
    if (local) |resolved| {
        frame.enclosing.?.locals[resolved].captured = true;
        return try self.addUpvalue(
            frame.enclosing.?.locals[resolved].node,
            frame,
            resolved,
            true,
        );
    }

    const upvalue: ?usize = try self.resolveUpvalue(frame.enclosing.?, name);
    if (upvalue) |resolved| {
        return try self.addUpvalue(
            frame.upvalues[resolved].node,
            frame,
            resolved,
            false,
        );
    }

    return null;
}

fn declareVariable(
    self: *Self,
    node: Ast.Node.Index,
    variable_type: *obj.ObjTypeDef,
    name: Ast.TokenIndex,
    final: bool,
    mutable: bool,
    check_name: bool,
) Error!usize {
    const lexemes = self.ast.tokens.items(.lexeme);
    const name_lexeme = lexemes[name];

    if (self.current.?.scope_depth > 0) {
        // Check a local with the same name doesn't exists
        if (self.current.?.local_count > 0) {
            var i: usize = self.current.?.local_count - 1;
            while (check_name and i >= 0) : (i -= 1) {
                const local: *Local = &self.current.?.locals[i];

                if (local.depth != -1 and local.depth < self.current.?.scope_depth) {
                    break;
                }

                if (!std.mem.eql(u8, name_lexeme, "_") and
                    !std.mem.startsWith(u8, name_lexeme, "$") and
                    std.mem.eql(u8, name_lexeme, lexemes[local.name]))
                {
                    const location = self.ast.tokens.get(name);
                    const decl_location = self.ast.tokens.get(local.name);

                    self.reporter.reportWithOrigin(
                        .variable_already_exists,
                        location,
                        location,
                        decl_location,
                        decl_location,
                        "A variable named `{s}` already exists",
                        .{name_lexeme},
                        null,
                    );
                }

                if (i == 0) break;
            }
        }

        return try self.addLocal(
            node,
            name,
            variable_type,
            final,
            mutable,
        );
    } else {
        if (check_name) {
            // Check a global with the same name doesn't exists
            for (self.globals.items, 0..) |*global, index| {
                if (!std.mem.eql(u8, name_lexeme, "_") and
                    global.matchName(
                        self.ast,
                        self.namespace orelse &[_]Ast.TokenIndex{},
                        name,
                    ) and
                    !global.hidden)
                {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder) {
                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @intFromEnum(variable_type.def_type) < @intFromEnum(obj.ObjTypeDef.Type.ObjectInstance)) {
                            if (BuildOptions.debug_placeholders) {
                                io.print(
                                    "Global placeholder @{} resolve with @{} {s} (opt {})\n",
                                    .{
                                        @intFromPtr(global.type_def),
                                        @intFromPtr(variable_type),
                                        try variable_type.toStringAlloc(self.gc.allocator),
                                        variable_type.optional,
                                    },
                                );
                            }

                            try self.resolvePlaceholder(global.type_def, variable_type, final);

                            if (self.flavor == .Ast) {
                                for (global.placeholder_referrers.items) |referrer| {
                                    try self.resolveReferrer(referrer, node);
                                }

                                global.placeholder_referrers.deinit(self.gc.allocator);
                            }
                        }

                        global.referenced = true;

                        return index;
                    } else {
                        const location = self.ast.tokens.get(name);
                        const decl_location = self.ast.tokens.get(global.name[0]);

                        self.reporter.reportWithOrigin(
                            .variable_already_exists,
                            location,
                            location,
                            decl_location,
                            if (global.name.len > 1)
                                self.ast.tokens.get(global.name[global.name.len - 1])
                            else
                                decl_location,
                            "A global named `{s}` already exists",
                            .{
                                location.lexeme,
                            },
                            null,
                        );
                    }
                }
            }
        }

        return try self.addGlobal(
            node,
            name,
            variable_type,
            final,
            mutable,
        );
    }
}

fn parseVariable(
    self: *Self,
    node: Ast.Node.Index,
    identifier: ?Ast.TokenIndex,
    variable_type: *obj.ObjTypeDef,
    final: bool,
    mutable: bool,
    comptime error_message: []const u8,
) !usize {
    if (identifier == null) {
        try self.consume(.Identifier, error_message);
    }

    return try self.declareVariable(
        node,
        variable_type,
        identifier orelse self.current_token.? - 1,
        final,
        mutable,
        true,
    );
}

fn markInitialized(self: *Self) void {
    if (self.current.?.scope_depth == 0) {
        self.globals.items[self.globals.items.len - 1].initialized = true;
    } else {
        self.current.?.locals[self.current.?.local_count - 1].depth = @intCast(self.current.?.scope_depth);
    }
}

fn declarePlaceholder(self: *Self, name: Ast.TokenIndex, placeholder: ?*obj.ObjTypeDef) Error!usize {
    var placeholder_type: *obj.ObjTypeDef = undefined;

    if (placeholder) |uplaceholder| {
        placeholder_type = uplaceholder;
    } else {
        placeholder_type = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        name,
                        name,
                        null,
                    ),
                },
            },
        );
    }

    std.debug.assert(!placeholder_type.optional);

    const global = try self.addGlobal(
        0, // Will be populated once the placeholder is resolved
        name,
        placeholder_type,
        false,
        true,
    );
    // markInitialized but we don't care what depth we are in
    self.globals.items[global].initialized = true;

    if (BuildOptions.debug_placeholders) {
        io.print(
            "global placeholder @{} for `{s}` at {}\n",
            .{
                @intFromPtr(placeholder_type),
                self.ast.tokens.items(.lexeme)[name],
                global,
            },
        );
    }

    return global;
}

pub fn parseTypeDefFrom(self: *Self, source: []const u8) Error!*obj.ObjTypeDef {
    const type_scanner = Scanner.init(self.gc.allocator, self.script_name, source);
    // Replace parser scanner with one that only looks at that substring
    const scanner = self.scanner;
    self.scanner = type_scanner;
    const current_token = self.current_token;
    const tokens_count = self.ast.tokens.len;

    // If Eof, manually add new token
    if (self.ast.tokens.items(.tag)[self.current_token.?] == .Eof) {
        _ = try self.ast.appendToken(try self.scanner.?.scanToken());
        self.current_token = self.current_token.? + 1;
    } else {
        _ = try self.advance();
    }

    const parsed_type = try self.parseTypeDef(null, true);

    // Restore normal scanner and parser state
    self.scanner = scanner;
    self.current_token = current_token;
    // Remove the added tokens
    self.ast.tokens.shrinkRetainingCapacity(tokens_count);

    return self.ast.nodes.items(.type_def)[parsed_type].?;
}

fn parseTypeDef(
    self: *Self,
    generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef),
    instance: bool,
) Error!Ast.Node.Index {
    const mutable = try self.match(.Mut);
    const mutable_token = self.current_token.?;

    if (try self.match(.Str)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`str` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .String,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Pat)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`pat` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Pattern,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Ud)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`ud` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .UserData,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Type)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`type` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Type,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Void)) {
        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`void` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = self.gc.type_registry.void_type,
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Int)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`int` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Integer,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Double)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`double` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Double,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Bool)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`bool` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Bool,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Range)) {
        const optional = try self.match(.Question);

        if (mutable) {
            self.reporter.report(
                .mutable_forbidden,
                self.ast.tokens.get(mutable_token),
                self.ast.tokens.get(self.current_token.? - 1),
                "`rg` can't be mutable",
            );
        }

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Range,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Any)) {
        const optional = try self.match(.Question);

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Any,
                        .resolved_type = .{
                            .Any = mutable,
                        },
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Type)) {
        const optional = try self.match(.Question);

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Type,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.LeftBracket)) {
        return self.parseListType(generic_types, mutable);
    } else if (try self.match(.LeftBrace)) {
        return self.parseMapType(generic_types, mutable);
    } else if (try self.match(.Fun) or try self.match(.Extern)) {
        return try self.parseFunctionType(generic_types);
    } else if (try self.match(.Fib)) {
        return try self.parseFiberType(generic_types);
    } else if (try self.match(.Obj)) {
        const type_def_node = try self.parseObjType(generic_types);
        if (instance) {
            self.ast.nodes.items(.type_def)[type_def_node] = try self.ast.nodes.items(.type_def)[type_def_node].?.toInstance(
                &self.gc.type_registry,
                mutable,
            );
        }

        return type_def_node;
    } else if ((try self.match(.Identifier))) {
        const identifier = self.current_token.? - 1;
        const identifier_lexeme = self.ast.tokens.items(.lexeme)[identifier];

        var user_type_node: ?Ast.Node.Index = null;
        var user_type: ?*obj.ObjTypeDef = null;
        // Is it a generic type defined in enclosing functions or object?
        if (self.resolveGeneric(try self.gc.copyString(identifier_lexeme))) |generic_type| {
            user_type = generic_type;
        } else if (generic_types != null) {
            // Is it generic type defined in a function signature being parsed?
            if (generic_types.?.get(try self.gc.copyString(identifier_lexeme))) |generic_type| {
                user_type = generic_type;
            }
        }

        // Is it a user defined type (object, enum, etc.) defined in global scope?
        if (user_type == null) {
            user_type_node = try self.parseUserType(instance, mutable);
            user_type = self.ast.nodes.items(.type_def)[user_type_node.?];
        }

        if (try self.match(.Question)) {
            user_type = try user_type.?.cloneOptional(&self.gc.type_registry);

            if (user_type_node) |un| {
                self.ast.nodes.items(.type_def)[un] = user_type;
            }
        }

        return user_type_node orelse try self.ast.appendNode(
            .{
                .tag = .GenericType,
                .location = identifier,
                .end_location = self.current_token.? - 1,
                .type_def = user_type.?,
                .components = .{
                    .GenericType = {},
                },
            },
        );
    } else {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Expected type definition.",
        );

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Void,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    }
}

fn parseFiberType(self: *Self, generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.Less, "Expected `<` after `fib`");
    const return_type = try self.parseTypeDef(generic_types, true);
    try self.consume(.Comma, "Expected `,` after fiber return type");
    const yield_type = try self.parseTypeDef(generic_types, true);

    const yield_type_def = self.ast.nodes.items(.type_def)[yield_type].?;
    if (!yield_type_def.optional and yield_type_def.def_type != .Void) {
        self.reportErrorAtNode(
            .yield_type,
            yield_type,
            "Expected optional type or void",
            .{},
        );
    }

    try self.consume(.Greater, "Expected `>` after fiber yield type");

    return self.ast.appendNode(
        .{
            .tag = .FiberType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                obj.ObjTypeDef{
                    .optional = try self.match(.Question),
                    .def_type = .Fiber,
                    .resolved_type = .{
                        .Fiber = .{
                            .return_type = self.ast.nodes.items(.type_def)[return_type].?,
                            .yield_type = self.ast.nodes.items(.type_def)[yield_type].?,
                        },
                    },
                },
            ),
            .components = .{
                .FiberType = .{
                    .return_type = return_type,
                    .yield_type = yield_type,
                },
            },
        },
    );
}

fn parseListType(self: *Self, generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef), mutable: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const item_type = try self.parseTypeDef(generic_types, true);

    try self.consume(.RightBracket, "Expected `]` after list type.");

    const list_type_def = try self.gc.type_registry.getTypeDef(
        .{
            .optional = try self.match(.Question),
            .def_type = .List,
            .resolved_type = .{
                .List = obj.ObjList.ListDef.init(
                    self.ast.nodes.items(.type_def)[item_type].?,
                    mutable,
                ),
            },
        },
    );

    return try self.ast.appendNode(
        .{
            .tag = .ListType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = list_type_def,
            .components = .{
                .ListType = item_type,
            },
        },
    );
}

fn parseMapType(self: *Self, generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef), mutable: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const key_type = try self.parseTypeDef(generic_types, true);

    try self.consume(.Colon, "Expected `:` after key type.");

    const value_type = try self.parseTypeDef(generic_types, true);

    try self.consume(.RightBrace, "Expected `}` after value type.");

    const type_defs = self.ast.nodes.items(.type_def);

    return try self.ast.appendNode(
        .{
            .tag = .MapType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .optional = try self.match(.Question),
                    .def_type = .Map,
                    .resolved_type = .{
                        .Map = obj.ObjMap.MapDef.init(
                            type_defs[key_type].?,
                            type_defs[value_type].?,
                            mutable,
                        ),
                    },
                },
            ),
            .components = .{
                .MapType = .{
                    .key_type = key_type,
                    .value_type = value_type,
                },
            },
        },
    );
}

fn parseFunctionType(self: *Self, parent_generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const tag = self.ast.tokens.items(.tag)[start_location];

    std.debug.assert(tag == .Fun or tag == .Extern);

    const is_extern = tag == .Extern;

    if (is_extern) {
        try self.consume(.Fun, "Expected `fun` after `extern`.");
    }

    var name_token: ?Ast.TokenIndex = null;
    var name: ?*obj.ObjString = null;
    if (try self.match(.Identifier)) {
        name_token = self.current_token.? - 1;
        name = try self.gc.copyString(self.ast.tokens.items(.lexeme)[self.current_token.? - 1]);
    }

    var merged_generic_types = std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef){};
    defer merged_generic_types.deinit(self.gc.allocator);
    if (parent_generic_types != null) {
        var it = parent_generic_types.?.iterator();
        while (it.next()) |kv| {
            try merged_generic_types.put(
                self.gc.allocator,
                kv.key_ptr.*,
                kv.value_ptr.*,
            );
        }
    }

    var generic_types_list = std.ArrayListUnmanaged(Ast.Node.Index){};
    // To avoid duplicates
    var generic_types = std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef){};
    if (try self.match(.DoubleColon)) {
        try self.consume(.Less, "Expected `<` at start of generic types list.");

        var i: usize = 0;
        while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
            try self.consume(.Identifier, "Expected generic type identifier");

            const generic_identifier = self.current_token.? - 1;
            const generic_identifier_lexeme = self.ast.tokens.items(.lexeme)[generic_identifier];
            if (generic_types.get(try self.gc.copyString(generic_identifier_lexeme)) == null) {
                const generic = obj.ObjTypeDef.GenericDef{
                    .origin = undefined,
                    .index = i,
                };
                const resolved = obj.ObjTypeDef.TypeUnion{ .Generic = generic };
                const type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Generic,
                        .resolved_type = resolved,
                    },
                );

                try generic_types.put(
                    self.gc.allocator,
                    try self.gc.copyString(generic_identifier_lexeme),
                    type_def,
                );

                try merged_generic_types.put(
                    self.gc.allocator,
                    try self.gc.copyString(generic_identifier_lexeme),
                    type_def,
                );

                try generic_types_list.append(
                    self.gc.allocator,
                    try self.ast.appendNode(
                        .{
                            .tag = .GenericType,
                            .location = generic_identifier,
                            .end_location = generic_identifier,
                            .type_def = type_def,
                            .components = .{
                                .GenericType = {},
                            },
                        },
                    ),
                );
            } else {
                const location = self.ast.tokens.get(generic_identifier);

                self.reporter.reportErrorFmt(
                    .generic_type,
                    location,
                    location,
                    "Generic type `{s}` already defined",
                    .{
                        self.ast.tokens.items(.lexeme)[self.current_token.? - 1],
                    },
                );
            }

            if (!self.check(.Greater)) {
                try self.consume(.Comma, "Expected `,` between generic types");
            }
        }

        if (generic_types.count() == 0) {
            self.reporter.reportErrorAt(
                .generic_type,
                self.ast.tokens.get(start_location),
                self.ast.tokens.get(self.current_token.? - 1),
                "Expected at least one generic type",
            );
        }

        try self.consume(.Greater, "Expected `>` after generic types list");
    }

    try self.consume(.LeftParen, "Expected `(` after function name.");

    var arguments = std.ArrayList(Ast.FunctionType.Argument).init(self.gc.allocator);
    var parameters = std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef){};
    var defaults = std.AutoArrayHashMapUnmanaged(*obj.ObjString, Value){};
    var arity: usize = 0;
    if (!self.check(.RightParen)) {
        while (true) {
            arity += 1;
            if (arity > std.math.maxInt(u8)) {
                self.reporter.reportErrorAt(
                    .arguments_count,
                    self.ast.tokens.get(start_location),
                    self.ast.tokens.get(self.current_token.? - 1),
                    "Can't have more than 255 arguments.",
                );
            }

            try self.consume(.Identifier, "Expected argument name");

            const arg_name_token = self.current_token.? - 1;
            const arg_name = self.ast.tokens.items(.lexeme)[self.current_token.? - 1];

            try self.consume(.Colon, "Expected `:`");

            const arg_type = try self.parseTypeDef(
                merged_generic_types,
                true,
            );
            const arg_type_def = self.ast.nodes.items(.type_def)[arg_type];

            var default: ?Ast.Node.Index = null;
            if (try self.match(.Equal)) {
                const expr = try self.expression(false);
                const expr_type_def = self.ast.nodes.items(.type_def)[expr];

                if (expr_type_def != null and expr_type_def.?.def_type == .Placeholder and arg_type_def.?.def_type == .Placeholder) {
                    try obj.PlaceholderDef.link(
                        self.gc.allocator,
                        arg_type_def.?,
                        expr_type_def.?,
                        .Assignment,
                    );
                }

                if (!try self.ast.slice().isConstant(self.gc.allocator, expr)) {
                    self.reportErrorAtNode(
                        .constant_default,
                        expr,
                        "Default parameters must be constant values.",
                        .{},
                    );
                }

                default = expr;
            }

            try arguments.append(
                .{
                    .name = arg_name_token,
                    .default = default,
                    .type = arg_type,
                },
            );
            if (if (default) |dflt|
                try self.ast.slice().toValue(dflt, self.gc)
            else if (arg_type_def.?.optional)
                Value.Null
            else
                null) |dflt|
            {
                try defaults.put(
                    self.gc.allocator,
                    try self.gc.copyString(arg_name),
                    dflt,
                );
            }
            try parameters.put(
                self.gc.allocator,
                try self.gc.copyString(arg_name),
                arg_type_def.?,
            );

            if (!try self.match(.Comma)) break;
        }
    }

    try self.consume(.RightParen, "Expected `)` after function parameters.");

    const return_type = if (try self.match(.Greater))
        try self.parseTypeDef(null, true)
    else
        null;

    var yield_type: ?Ast.Node.Index = null;
    if (try self.match(.Star)) {
        try self.consume(.Greater, "Expected `>` before yield type");
        yield_type = try self.parseTypeDef(null, true);
    }

    var error_types_list = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    var error_types = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
    if (try self.match(.BangGreater)) {
        const expects_multiple_error_types = try self.match(.LeftParen);

        while (!self.check(.Eof)) {
            const error_type = try self.parseTypeDef(generic_types, true);
            const error_type_def = self.ast.nodes.items(.type_def)[error_type].?;
            try error_types_list.append(error_type);
            try error_types.append(error_type_def);

            if (error_type_def.optional) {
                self.reportErrorAtNode(
                    .error_type,
                    error_type,
                    "Error type can't be optional",
                    .{},
                );
            }

            if (!expects_multiple_error_types or !(try self.match(.Comma))) {
                break;
            }
        }

        if (expects_multiple_error_types) {
            try self.consume(.RightParen, "Expected `)`");
        }
    }

    const function_typedef: obj.ObjTypeDef = .{
        .def_type = .Function,
        .optional = try self.match(.Question),
        .resolved_type = .{
            .Function = .{
                .id = obj.ObjFunction.FunctionDef.nextId(),
                .script_name = try self.gc.copyString(self.script_name),
                .name = name orelse try self.gc.copyString("anonymous"),
                .return_type = if (return_type) |rt|
                    self.ast.nodes.items(.type_def)[rt].?
                else
                    self.gc.type_registry.void_type,
                .yield_type = if (yield_type) |yt|
                    self.ast.nodes.items(.type_def)[yt].?
                else
                    self.gc.type_registry.void_type,
                .parameters = parameters,
                .defaults = defaults,
                .function_type = if (is_extern) .Extern else .Anonymous,
                .generic_types = generic_types,
                .error_types = if (error_types.items.len > 0) error_types.items else null,
            },
        },
    };

    return self.ast.appendNode(
        .{
            .tag = .FunctionType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(function_typedef),
            .components = .{
                .FunctionType = .{
                    .lambda = false,
                    .name = name_token,
                    .arguments = try arguments.toOwnedSlice(),
                    .error_types = try error_types_list.toOwnedSlice(),
                    .generic_types = try generic_types_list.toOwnedSlice(self.gc.allocator),
                    .return_type = return_type,
                    .yield_type = yield_type,
                },
            },
        },
    );
}

// Only used to parse anonymouse object type
fn parseObjType(self: *Self, generic_types: ?std.AutoArrayHashMapUnmanaged(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftBrace, "Expected `{` after `obj`");

    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
    defer self.gc.allocator.free(qualifier);
    var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_name.deinit();
    try qualified_name.writer().print("{s}.anonymous", .{qualifier});

    const object_def = obj.ObjObject.ObjectDef.init(
        self.ast.tokens.get(start_location),
        try self.gc.copyString("anonymous"),
        try self.gc.copyString(qualified_name.items),
        true,
    );

    var object_type = obj.ObjTypeDef{
        .def_type = .Object,
        .resolved_type = .{
            .Object = object_def,
        },
    };

    // Anonymous object can only have properties without default values (no methods, no static fields)
    // They can't self reference since their anonymous
    var field_names = std.StringHashMap(void).init(self.gc.allocator);
    defer field_names.deinit();
    var fields = std.ArrayList(Ast.AnonymousObjectType.Field).init(self.gc.allocator);
    var tuple_index: u8 = 0;
    var obj_is_tuple = false;
    var obj_is_not_tuple = false;
    var property_idx: usize = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) : (property_idx += 1) {
        const final = try self.match(.Final);

        const is_tuple = !(try self.match(.Identifier));
        const property_name = if (!is_tuple)
            self.current_token.? - 1
        else
            try self.insertUtilityToken(
                Token.identifier(
                    switch (tuple_index) {
                        0 => "0",
                        1 => "1",
                        2 => "2",
                        3 => "3",
                        else => "invalid",
                    },
                ),
            );
        const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];

        try self.consume(.Colon, "Expected `:`");
        const property_type = try self.parseTypeDef(generic_types, true);

        if (is_tuple) {
            obj_is_tuple = true;

            if (obj_is_not_tuple) {
                self.reportErrorAtNode(
                    .mix_tuple,
                    property_type,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                    .{},
                );
            }

            if (tuple_index >= 4) {
                self.reportErrorAtNode(
                    .tuple_limit,
                    property_type,
                    "Tuples can't have more than 4 elements",
                    .{},
                );
            }

            tuple_index += 1;
        } else {
            obj_is_not_tuple = true;

            if (obj_is_tuple) {
                self.reportErrorAtNode(
                    .mix_tuple,
                    property_type,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                    .{},
                );
            }
        }

        if (!is_tuple and field_names.get(property_name_lexeme) != null) {
            const location = self.ast.tokens.get(property_name);

            self.reporter.reportErrorAt(
                .property_already_exists,
                location,
                location,
                "A property with that name already exists.",
            );
        }

        if (!self.check(.RightBrace) or self.check(.Comma)) {
            try self.consume(.Comma, "Expected `,` after property definition.");
        }
        try object_type.resolved_type.?.Object.fields.put(
            self.gc.allocator,
            property_name_lexeme,
            .{
                .name = property_name_lexeme,
                .type_def = self.ast.nodes.items(.type_def)[property_type].?,
                .location = self.ast.tokens.get(property_name),
                .final = final,
                .static = false,
                .method = false,
                .has_default = false,
                .mutable = false, // makes only sense for methods
                .index = property_idx,
            },
        );
        try field_names.put(property_name_lexeme, {});
        try fields.append(
            .{
                .name = property_name,
                .type = property_type,
            },
        );
    }

    try self.consume(.RightBrace, "Expected `}` after object body.");

    object_type.optional = try self.match(.Question);

    try object_type.resolved_type.?.Object.sortFieldIndexes(self.gc.allocator);

    return try self.ast.appendNode(
        .{
            .tag = .AnonymousObjectType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(object_type),
            .components = .{
                .AnonymousObjectType = .{
                    .fields = try fields.toOwnedSlice(),
                },
            },
        },
    );
}

fn parseUserType(self: *Self, instance: bool, mutable: bool) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.ast.allocator);

    const user_type_name = try self.qualifiedName();
    var var_type: ?*obj.ObjTypeDef = null;
    var global_slot: ?usize = null;

    // Search for a global with that name
    if (try self.resolveGlobal(@intCast(node_slot), user_type_name)) |slot| {
        const global = self.globals.items[slot];

        var_type = global.type_def;
        global_slot = @intCast(slot);

        if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
            const imported_from = global.imported_from.?;

            try self.script_imports.put(
                self.gc.allocator,
                imported_from,
                .{
                    .location = self.script_imports.get(imported_from).?.location,
                    .end_location = self.script_imports.get(imported_from).?.end_location,
                    .referenced = true,
                },
            );
        }
    }

    // If none found, create a placeholder
    if (var_type == null) {
        var_type = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        user_type_name[user_type_name.len - 1],
                        user_type_name[user_type_name.len - 1],
                        null,
                    ),
                },
            },
        );

        std.debug.assert(user_type_name.len > 0);
        global_slot = try self.declarePlaceholder(
            user_type_name[user_type_name.len - 1],
            var_type.?,
        );
    }

    // Concrete generic types list
    var resolved_generics = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
    var generic_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    const generic_resolve = if (try self.match(.DoubleColon)) gn: {
        const generic_start = self.current_token.? - 1;

        try self.consume(.Less, "Expected generic types list after `::`");

        var i: usize = 0;
        while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
            try generic_nodes.append(
                try self.parseTypeDef(
                    if (self.current.?.generics) |generics|
                        generics.*
                    else
                        null,
                    true,
                ),
            );

            try resolved_generics.append(
                self.ast.nodes.items(.type_def)[generic_nodes.items[generic_nodes.items.len - 1]].?,
            );

            if (!self.check(.Greater)) {
                try self.consume(.Comma, "Expected `,` between generic types");
            }
        }

        try self.consume(.Greater, "Expected `>` after generic types list");

        if (resolved_generics.items.len == 0) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .generic_type,
                location,
                location,
                "Expected at least one type",
            );
        }

        var_type = try var_type.?.populateGenerics(
            self.current_token.? - 1,
            var_type.?.resolved_type.?.Object.id,
            try resolved_generics.toOwnedSlice(),
            &self.gc.type_registry,
            null,
        );
        break :gn try self.ast.appendNode(
            .{
                .tag = .GenericResolveType,
                .location = generic_start,
                .end_location = self.current_token.? - 1,
                .type_def = var_type,
                .components = .{
                    .GenericResolveType = try generic_nodes.toOwnedSlice(),
                },
            },
        );
    } else null;

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .UserType,
            .location = user_type_name[0],
            .end_location = self.current_token.? - 1,
            .type_def = if (instance)
                try var_type.?.toInstance(
                    &self.gc.type_registry,
                    mutable,
                )
            else
                var_type.?,
            .components = .{
                .UserType = .{
                    .generic_resolve = generic_resolve,
                    .name = user_type_name,
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn parseGenericResolve(self: *Self, callee_type_def: *obj.ObjTypeDef, expr: ?Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var resolved_generics = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    var resolved_generics_types = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);

    try self.consume(.Less, "Expected `<` at start of generic types list");

    while (!self.check(.Greater) and !self.check(.Eof)) {
        const resolved_generic = try self.parseTypeDef(null, true);

        if (callee_type_def.def_type == .Any) {
            self.reportErrorAtNode(
                .any_generic,
                resolved_generic,
                "`any` not allowed as generic type",
                .{},
            );
        }

        try resolved_generics.append(resolved_generic);
        try resolved_generics_types.append(self.ast.nodes.items(.type_def)[resolved_generic].?);

        if (!self.check(.Greater)) {
            try self.consume(.Comma, "Expected `,` between generic types");
        }
    }

    try self.consume(.Greater, "Expected `>` after generic types list");

    return self.ast.appendNode(
        .{
            .tag = if (expr != null)
                .GenericResolve
            else
                .GenericResolveType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try callee_type_def.populateGenerics(
                self.current_token.? - 1,
                if (callee_type_def.def_type == .Function)
                    callee_type_def.resolved_type.?.Function.id
                else if (callee_type_def.def_type == .Object)
                    callee_type_def.resolved_type.?.Object.id
                else
                    null,
                try resolved_generics_types.toOwnedSlice(),
                &self.gc.type_registry,
                null,
            ),
            .components = if (expr) |e| .{
                .GenericResolve = e,
            } else .{
                .GenericResolveType = try resolved_generics.toOwnedSlice(),
            },
        },
    );
}

fn subscript(self: *Self, can_assign: bool, subscripted: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const subscript_type_def = self.ast.nodes.items(.type_def)[subscripted];
    const checked = try self.match(.Question);
    const index = try self.expression(false);
    const index_type_def = self.ast.nodes.items(.type_def)[index];

    const type_defs = self.ast.nodes.items(.type_def);
    if (subscript_type_def.?.def_type == .Placeholder and index_type_def.?.def_type == .Placeholder) {
        try obj.PlaceholderDef.link(
            self.gc.allocator,
            type_defs[subscripted].?,
            type_defs[index].?,
            .Key,
        );
    }

    var subscripted_type_def: ?*obj.ObjTypeDef = null;

    if (type_defs[subscripted]) |type_def| {
        if (!type_def.optional) {
            switch (type_def.def_type) {
                .Placeholder => {
                    const placeholder = try self.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Placeholder,
                            .resolved_type = .{
                                .Placeholder = obj.PlaceholderDef.init(
                                    start_location,
                                    self.current_token.? - 1,
                                    null,
                                ),
                            },
                        },
                    );

                    try obj.PlaceholderDef.link(
                        self.gc.allocator,
                        type_def,
                        placeholder,
                        .Subscript,
                    );

                    subscripted_type_def = placeholder;
                },
                .String => subscripted_type_def = type_def,
                .List => subscripted_type_def = type_def.resolved_type.?.List.item_type,
                .Map => subscripted_type_def = try type_def.resolved_type.?.Map.value_type.cloneOptional(&self.gc.type_registry),
                else => self.reportErrorAtNode(
                    .subscriptable,
                    subscripted,
                    "Type `{s}` is not subscriptable",
                    .{
                        try type_def.toStringAlloc(self.gc.allocator),
                    },
                ),
            }
        } else {
            self.reportErrorAtNode(
                .subscriptable,
                subscripted,
                "Optional type is not subscriptable",
                .{},
            );
        }
    }

    try self.consume(.RightBracket, "Expected `]`.");

    var value: ?Ast.Node.Index = null;
    if (can_assign and (subscript_type_def == null or subscript_type_def.?.def_type != .String) and try self.match(.Equal)) {
        value = try self.expression(false);
        const value_type_def = self.ast.nodes.items(.type_def)[value.?];

        if (subscript_type_def.?.def_type == .Placeholder and value_type_def.?.def_type == .Placeholder) {
            try obj.PlaceholderDef.link(
                self.gc.allocator,
                subscript_type_def.?,
                value_type_def.?,
                .Subscript,
            );
        }
    }

    return try self.ast.appendNode(
        .{
            .tag = .Subscript,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = if (subscripted_type_def != null and checked)
                try subscripted_type_def.?.cloneOptional(&self.gc.type_registry)
            else
                subscripted_type_def,
            .components = .{
                .Subscript = .{
                    .index = index,
                    .value = value,
                    .subscripted = subscripted,
                    .checked = checked,
                },
            },
        },
    );
}

pub fn expression(self: *Self, hanging: bool) Error!Ast.Node.Index {
    return try self.parsePrecedence(.Assignment, hanging);
}

fn expressionStatement(self: *Self, hanging: bool) Error!Ast.Node.Index {
    const expr = try self.expression(hanging);

    try self.consume(.Semicolon, "Expected `;` after expression.");

    return try self.ast.appendNode(
        .{
            .tag = .Expression,
            .type_def = self.ast.nodes.items(.type_def)[expr],
            .location = self.ast.nodes.items(.location)[expr],
            .end_location = self.ast.nodes.items(.end_location)[expr],
            .components = .{
                .Expression = expr,
            },
        },
    );
}

fn list(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var items = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    const explicit_item_type: ?Ast.Node.Index = null;
    var item_type: ?*obj.ObjTypeDef = null;

    // A list expression can specify its type `[<int>, ...]`
    if (try self.match(.Less)) {
        const item_type_node = try self.parseTypeDef(null, true);
        item_type = self.ast.nodes.items(.type_def)[item_type_node];

        try self.consume(.Greater, "Expected `>` after list type.");
    }

    if (item_type == null or try self.match(.Comma)) {
        var common_type: ?*obj.ObjTypeDef = null;
        var mutable_item = true;
        while (!(try self.match(.RightBracket)) and !(try self.match(.Eof))) {
            const actual_item = try self.expression(false);

            try items.append(actual_item);

            if (item_type == null) {
                if (common_type == null) {
                    common_type = self.ast.nodes.items(.type_def)[actual_item];
                } else if (self.ast.nodes.items(.type_def)[actual_item]) |actual_type_def| {
                    if (!common_type.?.eql(actual_type_def)) {
                        if (common_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_type = if (common_type.?.resolved_type.?
                                .ObjectInstance.of.resolved_type.?
                                .Object.bothConforms(actual_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object)) |protocol_type_def|
                                try protocol_type_def.toInstance(
                                    &self.gc.type_registry,
                                    common_type.?.resolved_type.?.ObjectInstance.mutable,
                                )
                            else
                                common_type;
                        } else {
                            common_type = self.gc.type_registry.any_type;
                        }
                    }
                }
            }

            if (!self.check(.RightBracket)) {
                try self.consume(.Comma, "Expected `,` after list item.");
            }

            if (self.ast.nodes.items(.type_def)[actual_item]) |actual_type_def| {
                if (!actual_type_def.isMutable()) {
                    mutable_item = false;
                }
            }
        }

        // When inferring type, if at least one item is not mutable, the list item type is immutable
        if (common_type != null and common_type.?.isMutable() and !mutable_item) {
            common_type = try common_type.?.cloneMutable(
                &self.gc.type_registry,
                false,
            );
        }

        if (items.items.len > std.math.maxInt(u24)) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Too many elements in list initialization",
            );
        }

        if (self.ast.tokens.items(.tag)[self.current_token.? - 1] != .RightBracket) {
            if (self.flavor == .Repl) {
                self.reporter.panic_mode = true;
                self.reporter.last_error = .unclosed;
            } else {
                const location = self.ast.tokens.get(self.current_token.? - 1);
                self.reporter.reportErrorAt(
                    .unclosed,
                    location,
                    location,
                    "Expected `]`",
                );
            }
        }

        item_type = item_type orelse common_type;
    } else {
        try self.consume(.RightBracket, "Expected `]`");
    }

    return self.ast.appendNode(
        .{
            .tag = .List,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .List,
                    .resolved_type = .{
                        .List = obj.ObjList.ListDef.init(
                            item_type orelse self.gc.type_registry.any_type,
                            false, // If mutable, will be modified by `mutableExpression`
                        ),
                    },
                },
            ),
            .components = .{
                .List = .{
                    .explicit_item_type = explicit_item_type,
                    .items = items.items,
                },
            },
        },
    );
}

fn literal(self: *Self, _: bool) Error!Ast.Node.Index {
    var node = Ast.Node{
        .tag = undefined,
        .location = self.current_token.? - 1,
        .end_location = self.current_token.? - 1,
        .components = undefined,
    };

    switch (self.ast.tokens.items(.tag)[node.location]) {
        .True, .False => {
            node.tag = .Boolean;
            node.components = .{
                .Boolean = self.ast.tokens.items(.tag)[node.location] == .True,
            };
            node.type_def = self.gc.type_registry.bool_type;
        },
        .Null => {
            node.tag = .Null;
            node.components = .{
                .Null = {},
            };
            node.type_def = self.gc.type_registry.void_type;
        },
        .Void => {
            node.tag = .Void;
            node.components = .{
                .Void = {},
            };
            node.type_def = self.gc.type_registry.void_type;
        },
        .IntegerValue => {
            node.tag = .Integer;
            node.components = .{
                .Integer = self.ast.tokens.items(.literal_integer)[node.location].?,
            };
            node.type_def = self.gc.type_registry.int_type;
        },
        .FloatValue => {
            node.tag = .Double;
            node.components = .{
                .Double = self.ast.tokens.items(.literal_float)[node.location].?,
            };
            node.type_def = self.gc.type_registry.float_type;
        },
        else => unreachable,
    }

    return try self.ast.appendNode(node);
}

fn grouping(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const expr = try self.expression(false);

    try self.consume(.RightParen, "Expected ')' after expression.");

    return try self.ast.appendNode(
        .{
            .tag = .Grouping,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.ast.nodes.items(.type_def)[expr],
            .components = .{
                .Grouping = expr,
            },
        },
    );
}

fn argumentList(self: *Self) ![]Ast.Call.Argument {
    var arguments = std.ArrayList(Ast.Call.Argument).init(self.gc.allocator);

    const start_location = self.current_token.? - 1;

    var arg_count: u8 = 0;
    while (!(try self.match(.RightParen)) and !(try self.match(.Eof))) {
        var hanging = false;
        const arg_name = if (try self.match(.Identifier))
            self.current_token.? - 1
        else
            null;

        if (arg_name != null) {
            if (arg_count == 0 or self.check(.Comma) or self.check(.RightParen)) {
                // The identifier we just parsed might not be the argument name but the start of an expression or the expression itself
                hanging = !(try self.match(.Colon));
            } else {
                try self.consume(.Colon, "Expected `:` after argument name.");
            }
        } else if (arg_count != 0 and arg_name == null) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Expected argument name.",
            );
            break;
        }

        const is_named_expr = arg_count != 0 and arg_name != null and hanging and (self.check(.Comma) or self.check(.RightParen));

        try arguments.append(
            .{
                .name = if ((!hanging and arg_name != null) or is_named_expr)
                    arg_name.?
                else
                    null,
                .value = try self.expression(hanging),
            },
        );

        if (arg_count == std.math.maxInt(u8)) {
            self.reporter.reportErrorAt(
                .arguments_count,
                self.ast.tokens.get(start_location),
                self.ast.tokens.get(self.current_token.? - 1),
                "Can't have more than 255 arguments.",
            );

            return try arguments.toOwnedSlice();
        }

        arg_count += 1;

        if (!self.check(.RightParen)) {
            try self.consume(.Comma, "Expected `,` after call argument");
        }
    }

    return arguments.items;
}

fn call(self: *Self, _: bool, callee: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.ast.nodes.items(.location)[callee];
    const callee_type_def = self.ast.nodes.items(.type_def)[callee];

    const arguments = try self.argumentList();
    const catch_default = if (try self.match(.Catch))
        try self.expression(false)
    else
        null;

    // Node type is Function or Native return type or nothing/placeholder
    var type_def = if (callee_type_def != null and callee_type_def.?.def_type == .Function)
        callee_type_def.?.resolved_type.?.Function.return_type
    else if (callee_type_def != null and callee_type_def.?.def_type == .Enum)
        try (try callee_type_def.?.toInstance(&self.gc.type_registry, false))
            .cloneOptional(&self.gc.type_registry)
    else
        null;

    // If null, create placeholder
    if (type_def == null) {
        if (callee_type_def == null or callee_type_def.?.def_type != .Placeholder) {
            self.reportErrorAtNode(
                .callable,
                callee,
                "Can't be called",
                .{},
            );

            type_def = self.gc.type_registry.void_type;
        } else {
            type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = .{
                        .Placeholder = obj.PlaceholderDef.init(
                            start_location,
                            self.current_token.? - 1,
                            null,
                        ),
                    },
                },
            );

            try obj.PlaceholderDef.link(
                self.gc.allocator,
                callee_type_def.?,
                type_def.?,
                .Call,
            );
        }
    }

    return self.ast.appendNode(
        .{
            .tag = .Call,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = type_def.?,
            .components = .{
                .Call = .{
                    .is_async = false,
                    .callee = callee,
                    // We do this because the callee type will change in the dot.call usecase
                    .callee_type_def = self.ast.nodes.items(.type_def)[callee] orelse callee_td: {
                        std.debug.assert(self.reporter.last_error != null);

                        break :callee_td self.gc.type_registry.void_type;
                    },
                    .arguments = arguments,
                    .catch_default = catch_default,
                },
            },
        },
    );
}

fn map(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var value_type_node: ?Ast.Node.Index = null;
    var value_type_def: ?*obj.ObjTypeDef = null;
    var key_type_node: ?Ast.Node.Index = null;
    var key_type_def: ?*obj.ObjTypeDef = null;

    // A map expression can specify its type `{<str: str>, ...}`
    if (try self.match(.Less)) {
        key_type_node = try self.parseTypeDef(null, true);
        key_type_def = self.ast.nodes.items(.type_def)[key_type_node.?];

        try self.consume(.Colon, "Expected `:` after key type");

        value_type_node = try self.parseTypeDef(null, true);
        value_type_def = self.ast.nodes.items(.type_def)[value_type_node.?];

        try self.consume(.Greater, "Expected `>` after map type.");
    }

    var entries = std.ArrayList(Ast.Map.Entry).init(self.gc.allocator);
    if (key_type_node == null or try self.match(.Comma)) {
        var common_key_type: ?*obj.ObjTypeDef = null;
        var common_value_type: ?*obj.ObjTypeDef = null;
        var mutable_key = true;
        var mutable_value = true;
        while (!(try self.match(.RightBrace)) and !(try self.match(.Eof))) {
            const key = try self.expression(false);
            try self.consume(.Colon, "Expected `:` after key.");
            const value = try self.expression(false);

            try entries.append(
                .{
                    .key = key,
                    .value = value,
                },
            );

            if (key_type_node == null) {
                if (common_key_type == null) {
                    common_key_type = self.ast.nodes.items(.type_def)[key];
                } else if (self.ast.nodes.items(.type_def)[key]) |actual_type_def| {
                    if (!common_key_type.?.eql(actual_type_def)) {
                        if (common_key_type.?.def_type == .ObjectInstance and
                            actual_type_def.def_type == .ObjectInstance)
                        {
                            common_key_type = if (common_key_type.?.resolved_type.?.ObjectInstance
                                .of.resolved_type.?.Object
                                .bothConforms(
                                actual_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object,
                            )) |protocol_type_def|
                                try protocol_type_def.toInstance(
                                    &self.gc.type_registry,
                                    common_key_type.?.isMutable(),
                                )
                            else
                                common_key_type;
                        } else {
                            common_key_type = self.gc.type_registry.any_type;
                        }
                    }
                }
            }

            if (value_type_node == null) {
                if (common_value_type == null) {
                    common_value_type = self.ast.nodes.items(.type_def)[value];
                } else if (self.ast.nodes.items(.type_def)[value]) |actual_type_def| {
                    if (!common_value_type.?.eql(actual_type_def)) {
                        if (common_value_type.?.def_type == .ObjectInstance and
                            actual_type_def.def_type == .ObjectInstance)
                        {
                            common_value_type = if (common_value_type.?.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.bothConforms(
                                actual_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object,
                            )) |protocol_type_def|
                                try protocol_type_def.toInstance(
                                    &self.gc.type_registry,
                                    // All values must be mutable for the map value type to be mutable
                                    common_value_type.?.isMutable() and actual_type_def.isMutable(),
                                )
                            else
                                common_value_type;
                        } else {
                            common_value_type = self.gc.type_registry.any_type;
                        }
                    } else {
                        // If one value is not mutable the map value type can't be mutable
                        if (common_value_type.?.isMutable() and !actual_type_def.isMutable()) {
                            common_value_type = try common_value_type.?.cloneMutable(&self.gc.type_registry, false);
                        }
                    }
                }
            }

            if (!self.check(.RightBrace)) {
                try self.consume(.Comma, "Expected `,` after map entry.");
            }

            if (self.ast.nodes.items(.type_def)[key]) |actual_type_def| {
                if (!actual_type_def.isMutable()) {
                    mutable_key = false;
                }
            }

            if (self.ast.nodes.items(.type_def)[value]) |actual_type_def| {
                if (!actual_type_def.isMutable()) {
                    mutable_value = false;
                }
            }
        }

        // When inferring type, if at least one item is not mutable, the list item type is immutable
        if (common_key_type != null and common_key_type.?.isMutable() and !mutable_key) {
            common_key_type = try common_key_type.?.cloneMutable(
                &self.gc.type_registry,
                false,
            );
        }
        if (common_value_type != null and common_value_type.?.isMutable() and !mutable_value) {
            common_value_type = try common_value_type.?.cloneMutable(
                &self.gc.type_registry,
                false,
            );
        }

        if (entries.items.len > std.math.maxInt(u24)) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Too many entries in map initialization",
            );
        }

        key_type_def = key_type_def orelse common_key_type;
        value_type_def = value_type_def orelse common_value_type;
    } else {
        try self.consume(.RightBrace, "Expected `}`");
    }

    return self.ast.appendNode(
        .{
            .tag = .Map,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .optional = try self.match(.Question),
                    .def_type = .Map,
                    .resolved_type = .{
                        .Map = obj.ObjMap.MapDef.init(
                            key_type_def orelse self.gc.type_registry.any_type,
                            value_type_def orelse self.gc.type_registry.any_type,
                            false, // Will be set to true by `mutableExpression`
                        ),
                    },
                },
            ),
            .components = .{
                .Map = .{
                    .explicit_key_type = key_type_node,
                    .explicit_value_type = value_type_node,
                    .entries = try entries.toOwnedSlice(),
                },
            },
        },
    );
}

fn objectInit(self: *Self, _: bool, object: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const obj_type_def = self.ast.nodes.items(.type_def)[object];
    var properties = std.ArrayList(Ast.ObjectInit.Property).init(self.gc.allocator);
    var property_names = std.StringHashMap(Ast.Node.Index).init(self.gc.allocator);
    defer property_names.deinit();

    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        try self.consume(.Identifier, "Expected property name");

        const property_name = self.current_token.? - 1;
        const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];
        if (property_names.get(property_name_lexeme)) |previous_decl| {
            const location = self.ast.tokens.get(property_name);
            const decl_location = self.ast.tokens.get(previous_decl);
            self.reporter.reportWithOrigin(
                .property_already_exists,
                location,
                location,
                decl_location,
                decl_location,
                "Property `{s}` was already defined",
                .{property_name_lexeme},
                "Defined here",
            );
        }
        try property_names.put(property_name_lexeme, property_name);

        var property_placeholder: ?*obj.ObjTypeDef = null;

        // Object is placeholder, create placeholder for the property and link it
        const object_type_def = self.ast.nodes.items(.type_def)[object];
        if (object_type_def != null and object_type_def.?.def_type == .Placeholder) {
            property_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = .{
                        .Placeholder = obj.PlaceholderDef.init(
                            property_name,
                            property_name,
                            null,
                        ),
                    },
                },
            );

            try obj.PlaceholderDef.link(
                self.gc.allocator,
                obj_type_def.?,
                property_placeholder.?,
                .FieldAccess,
            );
        }

        // Named variable with the same name as property
        if (self.check(.Comma) or self.check(.RightBrace)) {
            try properties.append(
                .{
                    .name = property_name,
                    .value = try self.expression(true),
                },
            );
        } else {
            try self.consume(.Equal, "Expected `=` after property name.");

            try properties.append(
                .{
                    .name = property_name,
                    .value = try self.expression(false),
                },
            );
        }

        if (!self.check(.RightBrace) or self.check(.Comma)) {
            try self.consume(.Comma, "Expected `,` after field initialization.");
        }
    }

    try self.consume(.RightBrace, "Expected `}` after object initialization.");

    return self.ast.appendNode(
        .{
            .tag = .ObjectInit,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = if (self.ast.nodes.items(.type_def)[object]) |type_def|
                try type_def.toInstance(
                    &self.gc.type_registry,
                    false, // Will be modified by `mutableExpression`
                )
            else
                null,
            .components = .{
                .ObjectInit = .{
                    .object = object,
                    .properties = try properties.toOwnedSlice(),
                },
            },
        },
    );
}

fn anonymousObjectInit(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    try self.consume(.LeftBrace, "Expected `{` after `.`");

    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
    defer self.gc.allocator.free(qualifier);
    var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_name.deinit();
    try qualified_name.writer().print("{s}.anonymous", .{qualifier});

    const object_def = obj.ObjObject.ObjectDef.init(
        self.ast.tokens.get(start_location),
        try self.gc.copyString("anonymous"),
        try self.gc.copyString(qualified_name.items),
        true,
    );

    const resolved_type = obj.ObjTypeDef.TypeUnion{ .Object = object_def };

    // We build the object type has we parse its instanciation
    var object_type = obj.ObjTypeDef{
        .def_type = .Object,
        .resolved_type = resolved_type,
    };

    // Anonymous object can only have properties without default values (no methods, no static fields)
    // They can't self reference since their anonymous
    var properties = std.ArrayList(Ast.ObjectInit.Property).init(self.gc.allocator);
    var property_names = std.StringHashMap(Ast.Node.Index).init(self.gc.allocator);
    defer property_names.deinit();

    var tuple_index: u8 = 0;
    var obj_is_tuple = false;
    var obj_is_not_tuple = false;
    var property_idx: usize = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) : (property_idx += 1) {
        // Unnamed: this expression is a little bit tricky:
        // - either an identifier followed by something other than =
        // - or not an identifier
        if ((self.check(.Identifier) and !(try self.checkAhead(.Equal, 0))) or
            !self.check(.Identifier))
        {
            const expr = try self.expression(false);
            const is_tuple = self.ast.nodes.items(.tag)[expr] != .NamedVariable;

            if (is_tuple) {
                obj_is_tuple = true;

                if (obj_is_not_tuple) {
                    self.reportErrorAtNode(
                        .mix_tuple,
                        expr,
                        "Can't mix tuple notation and regular properties in anonymous object initialization",
                        .{},
                    );
                }
            }

            if (is_tuple and tuple_index >= 4) {
                self.reportErrorAtNode(
                    .tuple_limit,
                    expr,
                    "Tuples can't have more than 4 elements",
                    .{},
                );
            }

            // Consume identifier if it exists
            const property_name = if (!is_tuple)
                self.current_token.? - 1
            else
                try self.insertUtilityToken(
                    Token.identifier(
                        switch (tuple_index) {
                            0 => "0",
                            1 => "1",
                            2 => "2",
                            3 => "3",
                            else => "invalid",
                        },
                    ),
                );

            if (is_tuple) {
                tuple_index += 1;
            }

            const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];

            try property_names.put(
                property_name_lexeme,
                property_name,
            );

            try properties.append(
                .{
                    .name = property_name,
                    .value = expr,
                },
            );

            try object_type.resolved_type.?.Object.fields.put(
                self.gc.allocator,
                property_name_lexeme,
                .{
                    .name = property_name_lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[expr].?,
                    .location = self.ast.tokens.get(property_name),
                    .static = false,
                    .method = false,
                    .final = false,
                    .has_default = false,
                    .mutable = false, // makes only sense for methods
                    .index = property_idx,
                },
            );
        } else {
            try self.consume(.Identifier, "Expected property name");

            obj_is_not_tuple = true;

            const property_name = self.current_token.? - 1;

            if (obj_is_tuple) {
                const location = self.ast.tokens.get(property_name);
                self.reporter.reportErrorAt(
                    .mix_tuple,
                    location,
                    location,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                );
            }

            const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];
            if (property_names.get(property_name_lexeme)) |previous_decl| {
                const location = self.ast.tokens.get(property_name);
                const decl_location = self.ast.tokens.get(previous_decl);

                self.reporter.reportWithOrigin(
                    .property_already_exists,
                    location,
                    location,
                    decl_location,
                    decl_location,
                    "Property `{s}` was already defined",
                    .{property_name_lexeme},
                    "Defined here",
                );
            }
            try property_names.put(property_name_lexeme, property_name);

            // Named variable with the same name as property or tuple notation
            try self.consume(.Equal, "Expected `=` after property name.");

            const expr = try self.expression(false);

            try properties.append(
                .{
                    .name = property_name,
                    .value = expr,
                },
            );

            try object_type.resolved_type.?.Object.fields.put(
                self.gc.allocator,
                property_name_lexeme,
                .{
                    .name = property_name_lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[expr].?,
                    .location = self.ast.tokens.get(property_name),
                    .static = false,
                    .method = false,
                    .final = false,
                    .mutable = false, // makes only sense for methods
                    .has_default = false,
                    .index = property_idx,
                },
            );
        }

        if (!self.check(.RightBrace) or self.check(.Comma)) {
            try self.consume(.Comma, "Expected `,` after field initialization.");
        }
    }

    try object_type.resolved_type.?.Object.sortFieldIndexes(self.gc.allocator);

    try self.consume(.RightBrace, "Expected `}` after object initialization.");

    return try self.ast.appendNode(
        .{
            .tag = .ObjectInit,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try (try self.gc.type_registry.getTypeDef(object_type)).toInstance(
                &self.gc.type_registry,
                false, // Will be modified by `mutableExpression`
            ),
            .components = .{
                .ObjectInit = .{
                    .object = null,
                    .properties = try properties.toOwnedSlice(),
                },
            },
        },
    );
}

fn dot(self: *Self, can_assign: bool, callee: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.ast.nodes.items(.location)[callee];

    try self.consume(.Identifier, "Expected property name after `.`");
    const member_name_token = self.current_token.? - 1;
    const member_name = self.ast.tokens.items(.lexeme)[member_name_token];

    const dot_node = try self.ast.appendNode(
        .{
            .tag = .Dot,
            .location = start_location,
            .end_location = undefined,
            .components = .{
                .Dot = .{
                    .callee = callee,
                    .identifier = member_name_token,
                    .value_or_call_or_enum = undefined,
                    .generic_resolve = null,
                    .member_type_def = undefined,
                    .member_kind = undefined,
                },
            },
        },
    );

    // Check that name is a property
    const callee_type_def = self.ast.nodes.items(.type_def)[callee];
    if (callee_type_def != null) {
        const callee_def_type = callee_type_def.?.def_type;
        switch (callee_def_type) {
            .String => {
                if (try obj.ObjString.memberDefByName(self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        var components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_type_def = member.?;
                        components[dot_node].Dot.member_kind = .Call;
                        const dot_call = try self.call(
                            can_assign,
                            dot_node,
                        );
                        components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = dot_call,
                        };

                        // Node type is the return type of the call
                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[components[dot_node].Dot.value_or_call_or_enum.Call];
                    } else {
                        // String has only native functions
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "String property doesn't exist.",
                    );
                }
            },
            .Range => {
                if (try obj.ObjRange.memberDefByName(self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        var components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_type_def = member.?;
                        components[dot_node].Dot.member_kind = .Call;
                        const dot_call = try self.call(
                            can_assign,
                            dot_node,
                        );
                        components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = dot_call,
                        };

                        // Node type is the return type of the call
                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[components[dot_node].Dot.value_or_call_or_enum.Call];
                    } else {
                        // Range has only native functions
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Range property doesn't exist.",
                    );
                }
            },
            .Pattern => {
                if (try obj.ObjPattern.memberDefByName(self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        var components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_type_def = member.?;
                        components[dot_node].Dot.member_kind = .Call;
                        const dot_call = try self.call(
                            can_assign,
                            dot_node,
                        );
                        components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = dot_call,
                        };

                        // Node type is the return type of the call
                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[components[dot_node].Dot.value_or_call_or_enum.Call];
                    } else {
                        // Pattern has only native functions members
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Pattern property doesn't exist.",
                    );
                }
            },
            .Fiber => {
                if (try obj.ObjFiber.memberDefByName(self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        var components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_kind = .Call;
                        components[dot_node].Dot.member_type_def = member.?;
                        const dot_call = try self.call(
                            can_assign,
                            dot_node,
                        );
                        components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = dot_call,
                        };

                        // Node type is the return type of the call
                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[components[dot_node].Dot.value_or_call_or_enum.Call];
                    } else {
                        // Fiber has only native functions members
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Fiber property doesn't exist.",
                    );
                }
            },
            .Object => {
                const obj_def = callee_type_def.?.resolved_type.?.Object;
                const property_field = obj_def.fields.get(member_name);
                var property_type = if (property_field) |field| field.type_def else null;

                if (property_field != null and !property_field.?.static) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .undefined,
                        location,
                        location,
                        "Static property `{s}` is not defined",
                        .{
                            self.ast.tokens.items(.lexeme)[member_name_token],
                        },
                    );
                } else
                // Not found, create a placeholder, this is a root placeholder not linked to anything
                // TODO: test with something else than a name
                if (property_type == null and self.current_object != null and std.mem.eql(
                    u8,
                    self.current_object.?.name.lexeme,
                    obj_def.name.string,
                )) {
                    const placeholder = try self.gc.type_registry.getTypeDef(
                        .{
                            .optional = false,
                            .def_type = .Placeholder,
                            .resolved_type = .{
                                .Placeholder = obj.PlaceholderDef.init(
                                    member_name_token,
                                    member_name_token,
                                    null,
                                ),
                            },
                        },
                    );

                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "static placeholder @{} for `{s}`\n",
                            .{
                                @intFromPtr(placeholder),
                                self.ast.tokens.items(.lexeme)[member_name_token],
                            },
                        );
                    }

                    const kv = try callee_type_def.?.resolved_type.?.Object.static_placeholders
                        .getOrPut(self.gc.allocator, member_name);

                    if (!kv.found_existing) {
                        kv.value_ptr.* = .{
                            .placeholder = placeholder,
                        };
                    }

                    try kv.value_ptr.referrers.append(self.gc.allocator, dot_node);

                    property_type = placeholder;
                } else if (property_type == null) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Static property `{s}` does not exists in {s}",
                        .{
                            member_name,
                            obj_def.name.string,
                        },
                    );
                }

                const generic_resolve = if (try self.match(.DoubleColon))
                    try self.parseGenericResolve(property_type.?, null)
                else
                    null;

                self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                property_type = if (generic_resolve) |gr|
                    self.ast.nodes.items(.type_def)[gr]
                else
                    property_type;

                // Do we assign it ?
                var components = self.ast.nodes.items(.components);
                if (can_assign and try self.matchOpEqual()) {
                    const assign_token = self.current_token.? - 1;
                    components[dot_node].Dot.member_kind = .Value;
                    const value = try self.expression(false);
                    // For some reason we blow the comptime quota here
                    @setEvalBranchQuota(1000000);
                    components = self.ast.nodes.items(.components);
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = .{
                            .value = value,
                            .assign_token = assign_token,
                        },
                    };
                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                } else if (try self.match(.LeftParen)) { // Do we call it
                    // `call` will look to the parent node for the function definition
                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                    components[dot_node].Dot.member_type_def = property_type.?;
                    const call_node = try self.call(can_assign, dot_node);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.member_kind = .Call;
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Call = call_node,
                    };

                    // Node type is the return type of the call
                    self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                } else { // access only
                    components[dot_node].Dot.member_kind = .Ref;
                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                }
            },
            .ForeignContainer => {
                const f_def = callee_type_def.?.resolved_type.?.ForeignContainer;

                if (f_def.buzz_type.get(member_name)) |field| {
                    if (can_assign and try self.matchOpEqual()) {
                        const assign_token = self.current_token.? - 1;
                        const components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_kind = .Value;
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Value = .{
                                .value = try self.expression(false),
                                .assign_token = assign_token,
                            },
                        };
                    } else {
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                    }

                    self.ast.nodes.items(.type_def)[dot_node] = field;
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Property `{s}` does not exists in object `{s}`",
                        .{
                            member_name,
                            f_def.name.string,
                        },
                    );
                }
            },
            .ObjectInstance => {
                const object = callee_type_def.?.resolved_type.?.ObjectInstance.of;
                const obj_def = object.resolved_type.?.Object;

                const property_field = obj_def.fields.get(member_name);
                var property_type = (if (property_field) |field|
                    field.type_def
                else
                    null) orelse if (obj_def.placeholders.get(member_name)) |plc| plc.placeholder else null;

                // Else create placeholder
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    const placeholder = try self.gc.type_registry.getTypeDef(
                        .{
                            .optional = false,
                            .def_type = .Placeholder,
                            .resolved_type = .{
                                .Placeholder = obj.PlaceholderDef.init(
                                    member_name_token,
                                    member_name_token,
                                    null,
                                ),
                            },
                        },
                    );

                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "property placeholder @{} for `{s}.{s}`\n",
                            .{
                                @intFromPtr(placeholder),
                                object.resolved_type.?.Object.name.string,
                                self.ast.tokens.items(.lexeme)[member_name_token],
                            },
                        );
                    }
                    const kv = try object.resolved_type.?.Object.placeholders.getOrPut(self.gc.allocator, member_name);

                    if (!kv.found_existing) {
                        kv.value_ptr.* = .{
                            .placeholder = placeholder,
                        };
                    }

                    try kv.value_ptr.referrers.append(self.gc.allocator, dot_node);

                    property_type = placeholder;
                } else if (property_type == null) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Property `{s}` does not exists in object `{s}`",
                        .{ member_name, obj_def.name.string },
                    );
                }

                const generic_resolve = if (try self.match(.DoubleColon))
                    try self.parseGenericResolve(property_type.?, null)
                else
                    null;

                self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                property_type = if (generic_resolve) |gr|
                    self.ast.nodes.items(.type_def)[gr]
                else
                    property_type;

                // If its a field or placeholder, we can assign to it
                // TODO: here get info that field is final or not
                var components = self.ast.nodes.items(.components);
                if (can_assign and try self.matchOpEqual()) {
                    const assign_token = self.current_token.? - 1;
                    components[dot_node].Dot.member_kind = .Value;
                    const expr = try self.expression(false);
                    components = self.ast.nodes.items(.components);
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = .{
                            .value = expr,
                            .assign_token = assign_token,
                        },
                    };

                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                } else if (try self.match(.LeftParen)) { // If it's a method or placeholder we can call it
                    // `call` will look to the parent node for the function definition
                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                    components[dot_node].Dot.member_kind = .Call;
                    components[dot_node].Dot.member_type_def = property_type.?;
                    const call_node = try self.call(can_assign, dot_node);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Call = call_node,
                    };

                    // Node type is the return type of the call
                    self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                } else {
                    components[dot_node].Dot.member_kind = .Ref;
                    self.ast.nodes.items(.type_def)[dot_node] = property_type;
                }
            },
            .ProtocolInstance => {
                const protocol = callee_type_def.?.resolved_type.?.ProtocolInstance.of;
                const protocol_def = protocol.resolved_type.?.Protocol;

                var method_type = if (protocol_def.methods.get(member_name)) |field| field.type_def else null;

                // Else create placeholder
                if (method_type == null) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Method `{s}` does not exists in protocol `{s}`",
                        .{
                            member_name,
                            protocol_def.name.string,
                        },
                    );
                }

                const generic_resolve = if (try self.match(.DoubleColon))
                    try self.parseGenericResolve(method_type.?, null)
                else
                    null;

                self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                method_type = if (generic_resolve) |gr|
                    self.ast.nodes.items(.type_def)[gr]
                else
                    method_type;

                // Only call is allowed
                var components = self.ast.nodes.items(.components);
                if (try self.match(.LeftParen)) {
                    // `call` will look to the parent node for the function definition
                    self.ast.nodes.items(.type_def)[dot_node] = method_type;
                    components[dot_node].Dot.member_kind = .Call;
                    components[dot_node].Dot.member_type_def = method_type.?;
                    const call_node = try self.call(can_assign, dot_node);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Call = call_node,
                    };

                    // Node type is the return type of the call
                    self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                } else {
                    components[dot_node].Dot.member_kind = .Ref;
                    self.ast.nodes.items(.type_def)[dot_node] = method_type;
                }
            },
            .Enum => {
                const enum_def = callee_type_def.?.resolved_type.?.Enum;

                for (enum_def.cases, 0..) |case, index| {
                    if (std.mem.eql(u8, case, member_name)) {
                        self.ast.nodes.items(.type_def)[dot_node] = try self.gc.type_registry.getTypeDef(
                            .{
                                .optional = false,
                                .def_type = .EnumInstance,
                                .resolved_type = .{
                                    .EnumInstance = .{
                                        .of = callee_type_def.?,
                                        .mutable = false,
                                    },
                                },
                            },
                        );
                        const components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_kind = .EnumCase;
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .EnumCase = @intCast(index),
                        };
                        break;
                    }
                }

                if (self.ast.nodes.items(.type_def)[dot_node] == null) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorFmt(
                        .enum_case,
                        location,
                        location,
                        "Enum case `{s}` does not exists.",
                        .{
                            member_name,
                        },
                    );
                }
            },
            .EnumInstance => {
                // Only available field is `.value` to get associated value
                if (!std.mem.eql(u8, member_name, "value")) {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Enum provides only field `value`.",
                    );
                }

                self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                self.ast.nodes.items(.type_def)[dot_node] = callee_type_def.?.resolved_type.?.EnumInstance.of
                    .resolved_type.?.Enum.enum_type;
            },
            .List => {
                if (try obj.ObjList.ListDef.member(callee_type_def.?, self, member_name)) |member_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_def.type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_def.type_def;

                    var components = self.ast.nodes.items(.components);
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        components[dot_node].Dot.member_kind = .Call;
                        components[dot_node].Dot.member_type_def = member.?;
                        const call_node = try self.call(can_assign, dot_node);
                        components = self.ast.nodes.items(.components); // ptr might have been invalidated
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = call_node,
                        };

                        // Node type is the return type of the call

                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                    } else {
                        components[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "List property doesn't exist.",
                    );
                }
            },
            .Map => {
                if (try obj.ObjMap.MapDef.member(callee_type_def.?, self, member_name)) |member_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_def.type_def, null)
                    else
                        null;

                    var components = self.ast.nodes.items(.components);
                    components[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_def.type_def;

                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                        components[dot_node].Dot.member_kind = .Call;
                        components[dot_node].Dot.member_type_def = member.?;
                        const call_node = try self.call(can_assign, dot_node);
                        components = self.ast.nodes.items(.components); // ptr might have been invalidated
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Call = call_node,
                        };

                        // Node type is the return type of the call
                        self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                    } else {
                        components[dot_node].Dot.member_kind = .Ref;
                        self.ast.nodes.items(.type_def)[dot_node] = member;
                    }
                } else {
                    const location = self.ast.tokens.get(member_name_token);
                    self.reporter.reportErrorAt(
                        .property_does_not_exists,
                        location,
                        location,
                        "Map property doesn't exist.",
                    );
                }
            },
            .Placeholder => {
                // We know nothing of the field
                var placeholder = try self.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = .{
                            .Placeholder = obj.PlaceholderDef.init(
                                member_name_token,
                                member_name_token,
                                null,
                            ),
                        },
                    },
                );

                try obj.PlaceholderDef.link(
                    self.gc.allocator,
                    callee_type_def.?,
                    placeholder,
                    .FieldAccess,
                );

                const generic_resolve = if (try self.match(.DoubleColon))
                    try self.parseGenericResolve(placeholder, null)
                else
                    null;

                var components = self.ast.nodes.items(.components);
                components[dot_node].Dot.generic_resolve = generic_resolve;

                placeholder = if (generic_resolve) |gr|
                    self.ast.nodes.items(.type_def)[gr].?
                else
                    placeholder;

                if (can_assign and try self.matchOpEqual()) {
                    const assign_token = self.current_token.? - 1;
                    components[dot_node].Dot.member_kind = .Value;
                    const expr = try self.expression(false);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = .{
                            .value = expr,
                            .assign_token = assign_token,
                        },
                    };
                } else if (try self.match(.LeftParen)) {
                    // `call` will look to the parent node for the function definition
                    self.ast.nodes.items(.type_def)[dot_node] = placeholder;
                    components[dot_node].Dot.member_kind = .Call;
                    components[dot_node].Dot.member_type_def = placeholder;
                    const call_node = try self.call(can_assign, dot_node);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Call = call_node,
                    };
                    // TODO: here maybe we invoke instead of call??

                    // Node type is the return type of the call
                    self.ast.nodes.items(.type_def)[dot_node] = self.ast.nodes.items(.type_def)[call_node];
                } else {
                    components[dot_node].Dot.member_kind = .Ref;
                    self.ast.nodes.items(.type_def)[dot_node] = placeholder;
                }
            },
            else => {
                self.reportErrorAtNode(
                    .field_access,
                    callee,
                    "`{s}` is not field accessible",
                    .{
                        try callee_type_def.?.toStringAlloc(self.gc.allocator),
                    },
                );
            },
        }
    }

    self.ast.nodes.items(.end_location)[dot_node] = self.current_token.? - 1;

    return dot_node;
}

fn gracefulUnwrap(self: *Self, _: bool, unwrapped: Ast.Node.Index) Error!Ast.Node.Index {
    return self.unwrap(false, unwrapped);
}

fn forceUnwrap(self: *Self, _: bool, unwrapped: Ast.Node.Index) Error!Ast.Node.Index {
    return self.unwrap(true, unwrapped);
}

fn unwrap(self: *Self, force: bool, unwrapped: Ast.Node.Index) Error!Ast.Node.Index {
    const unwrapped_type_def = self.ast.nodes.items(.type_def)[unwrapped].?;

    const node = self.ast.appendNode(
        .{
            .tag = if (force) .ForceUnwrap else .Unwrap,
            .location = self.current_token.? - 1,
            .end_location = self.current_token.? - 1,
            .type_def = try unwrapped_type_def.cloneNonOptional(&self.gc.type_registry),
            .components = if (force)
                .{
                    .ForceUnwrap = .{
                        .start_opt_jumps = false,
                        .unwrapped = unwrapped,
                        .original_type = unwrapped_type_def,
                    },
                }
            else
                .{
                    .Unwrap = .{
                        .start_opt_jumps = self.opt_jumps == null,
                        .unwrapped = unwrapped,
                        .original_type = unwrapped_type_def,
                    },
                },
        },
    );

    if (!force) {
        self.opt_jumps = self.opt_jumps orelse .init(self.gc.allocator);

        try self.opt_jumps.?.append(
            getRule(
                self.ast.tokens.items(.tag)[self.current_token.?],
            ).precedence,
        );
    }

    return node;
}

fn unary(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const operator = self.ast.tokens.items(.tag)[start_location];
    const left = try self.parsePrecedence(.Unary, false);

    return self.ast.appendNode(
        .{
            .tag = .Unary,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.ast.nodes.items(.type_def)[left],
            .components = .{
                .Unary = .{
                    .operator = operator,
                    .expression = left,
                },
            },
        },
    );
}

fn genericResolve(self: *Self, _: bool, expr: Ast.Node.Index) Error!Ast.Node.Index {
    return try self.parseGenericResolve(self.ast.nodes.items(.type_def)[expr].?, expr);
}

fn @"or"(self: *Self, _: bool, left: Ast.Node.Index) Error!Ast.Node.Index {
    const operator = self.current_token.? - 1;
    const start_location = self.ast.nodes.items(.location)[left];
    const right = try self.parsePrecedence(.And, false);

    return self.ast.appendNode(
        .{
            .tag = .Binary,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.gc.type_registry.bool_type,
            .components = .{
                .Binary = .{
                    .left = left,
                    .right = right,
                    .operator = self.ast.tokens.items(.tag)[operator],
                },
            },
        },
    );
}

fn @"and"(self: *Self, _: bool, left: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.ast.nodes.items(.location)[left];
    const operator = self.current_token.? - 1;
    const right = try self.parsePrecedence(.And, false);

    return self.ast.appendNode(
        .{
            .tag = .Binary,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.gc.type_registry.bool_type,
            .components = .{
                .Binary = .{
                    .left = left,
                    .right = right,
                    .operator = self.ast.tokens.items(.tag)[operator],
                },
            },
        },
    );
}

fn @"if"(self: *Self, is_statement: bool, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.ast.allocator);

    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `if`.");

    try self.beginScope(null);
    const condition = try self.expression(false);
    const condition_type_def = self.ast.nodes.items(.type_def)[condition];

    var unwrapped_identifier: ?Ast.TokenIndex = null;
    var casted_type: ?Ast.Node.Index = null;
    if (try self.match(.Arrow)) { // if (opt -> unwrapped)
        try self.consume(.Identifier, "Expected identifier");
        _ = try self.parseVariable(
            @intCast(node_slot),
            self.current_token.? - 1,
            try condition_type_def.?.cloneNonOptional(&self.gc.type_registry),
            true,
            condition_type_def.?.isMutable(),
            "Expected optional unwrap identifier",
        );
        self.markInitialized();

        unwrapped_identifier = self.current_token.? - 1;
    } else if (try self.match(.As)) { // if (expr as casted)
        try self.consume(.Identifier, "Expected identifier");
        const identifier = self.current_token.? - 1;
        try self.consume(.Colon, "Expected `:`");
        casted_type = try self.parseTypeDef(null, true);
        _ = try self.parseVariable(
            @intCast(node_slot),
            identifier,
            try self.ast.nodes.items(.type_def)[casted_type.?].?.toInstance(
                &self.gc.type_registry,
                condition_type_def.?.isMutable(),
            ),
            true,
            condition_type_def.?.isMutable(),
            "Expected casted identifier",
        );
        self.markInitialized();
    }

    try self.consume(.RightParen, "Expected `)` after `if` condition.");

    const body = if (is_statement) stmt: {
        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");
        break :stmt try self.block(loop_scope);
    } else try self.expression(false);

    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    var else_branch: ?Ast.Node.Index = null;
    if (!is_statement or self.check(.Else)) {
        try self.consume(.Else, "Expected `else` after inline `if` body.");

        if (try self.match(.If)) {
            else_branch = try self.@"if"(is_statement, loop_scope);
        } else if (is_statement) {
            try self.consume(.LeftBrace, "Expected `{` after `else`.");

            try self.beginScope(null);
            else_branch = try self.block(loop_scope);
            self.ast.nodes.items(.ends_scope)[else_branch.?] = try self.endScope();
        } else {
            else_branch = try self.expression(false);
        }
    }

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .If,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = if (!is_statement) type_def: {
                const type_defs = self.ast.nodes.items(.type_def);
                const body_type_def = type_defs[body];
                const else_branch_type_def = if (else_branch) |eb| type_defs[eb] else null;
                var if_type_def = if (body_type_def == null or body_type_def.?.def_type == .Void)
                    else_branch_type_def
                else
                    body_type_def;

                const is_optional = if_type_def.?.optional or
                    body_type_def.?.optional or
                    else_branch_type_def.?.optional or
                    body_type_def.?.def_type == .Void or
                    else_branch_type_def.?.def_type == .Void;
                if (is_optional and !if_type_def.?.optional) {
                    break :type_def try if_type_def.?.cloneOptional(&self.gc.type_registry);
                }

                break :type_def if_type_def.?;
            } else null,
            .components = .{
                .If = .{
                    .condition = condition,
                    .unwrapped_identifier = unwrapped_identifier,
                    .casted_type = casted_type,
                    .body = body,
                    .else_branch = else_branch,
                    .is_statement = is_statement,
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn ifStatement(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    return try self.@"if"(true, loop_scope);
}

fn inlineIf(self: *Self, _: bool) Error!Ast.Node.Index {
    return try self.@"if"(false, null);
}

fn isAs(self: *Self, left: Ast.Node.Index, is_expr: bool) Error!Ast.Node.Index {
    const start_location = self.ast.nodes.items(.location)[left];
    const constant = try self.parseTypeDef(null, true);
    const type_def = self.ast.nodes.items(.type_def)[constant].?;

    return try self.ast.appendNode(
        .{
            .tag = if (is_expr) .Is else .As,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = if (is_expr)
                self.gc.type_registry.bool_type
            else
                (try type_def.cloneOptional(&self.gc.type_registry)),
            .components = if (is_expr)
                .{
                    .Is = .{
                        .left = left,
                        .constant = constant,
                    },
                }
            else
                .{
                    .As = .{
                        .left = left,
                        .constant = constant,
                    },
                },
        },
    );
}

fn is(self: *Self, _: bool, left: Ast.Node.Index) Error!Ast.Node.Index {
    return self.isAs(left, true);
}

fn as(self: *Self, _: bool, left: Ast.Node.Index) Error!Ast.Node.Index {
    return self.isAs(left, false);
}

fn string(self: *Self, _: bool) Error!Ast.Node.Index {
    const string_token_index = self.current_token.? - 1;
    const string_token = self.ast.tokens.get(string_token_index);
    const current_token = self.ast.tokens.get(self.current_token.?);

    var string_parser = StringParser.init(
        self,
        string_token.literal_string.?,
        self.script_name,
        string_token.line,
        string_token.column,
    );

    const string_node = if (string_token.literal_string.?.len > 0)
        try string_parser.parse()
    else
        try self.ast.appendNode(
            .{
                .tag = .String,
                .location = string_token_index,
                .end_location = string_token_index,
                .type_def = self.gc.type_registry.str_type,
                .components = .{
                    .String = &[_]Ast.Node.Index{},
                },
            },
        );
    self.ast.nodes.items(.location)[string_node] = string_token_index;
    self.ast.nodes.items(.end_location)[string_node] = self.current_token.? - 1;

    // Append again token just after the string so we don't confuse the parser
    self.current_token = try self.ast.appendToken(current_token);

    return string_node;
}

fn namedVariable(self: *Self, name: []const Ast.TokenIndex, can_assign: bool) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.ast.allocator);
    const start_location = self.current_token.? - 1;

    var var_def: ?*obj.ObjTypeDef = null;
    var slot: usize = undefined;
    var slot_type: Ast.SlotType = undefined;
    var slot_final = false;
    var def_node: Ast.Node.Index = 0;
    if (name.len == 1) {
        if (try self.resolveLocal(self.current.?, name[0])) |uslot| {
            var_def = self.current.?.locals[uslot].type_def;
            slot = uslot;
            slot_type = .Local;
            slot_final = self.current.?.locals[uslot].final;
            def_node = self.current.?.locals[uslot].node;
        } else if (try self.resolveUpvalue(self.current.?, name[0])) |uslot| {
            var_def = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].type_def;
            slot = uslot;
            slot_type = .UpValue;
            slot_final = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].final;
            def_node = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].node;
        }
    }

    if (var_def == null) {
        if (try self.resolveGlobal(@intCast(node_slot), name)) |uslot| {
            const global = self.globals.items[uslot];

            var_def = global.type_def;
            slot = uslot;
            slot_type = .Global;
            slot_final = global.final;
            def_node = global.node;

            if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
                const imported_from = global.imported_from.?;

                try self.script_imports.put(
                    self.gc.allocator,
                    imported_from,
                    .{
                        .location = self.script_imports.get(imported_from).?.location,
                        .end_location = self.script_imports.get(imported_from).?.end_location,
                        .referenced = true,
                    },
                );
            }
        } else {
            slot = try self.declarePlaceholder(name[0], null);
            var_def = self.globals.items[slot].type_def;
            slot_type = .Global;

            if (name.len > 1) {
                self.reporter.reportErrorFmt(
                    .syntax,
                    self.ast.tokens.get(name[0]),
                    self.ast.tokens.get(name[name.len - 1]),
                    "`{s}` does not exists in that namespace or namespace does not exists",
                    .{
                        self.ast.tokens.items(.lexeme)[name[0]],
                    },
                );
            }
        }
    }

    const assign_token = if (can_assign and try self.matchOpEqual())
        self.current_token.? - 1
    else
        null;

    const value = if (assign_token != null)
        try self.expression(false)
    else
        null;

    if (value != null) {
        if (slot_final) {
            self.reporter.reportErrorAt(
                .final,
                self.ast.tokens.get(start_location),
                self.ast.tokens.get(self.ast.nodes.items(.end_location)[value.?]),
                "Can't assign to final variable",
            );
        } else if (slot_type == .Local) {
            self.current.?.locals[slot].assigned = true;
        } else if (slot_type == .UpValue) {
            self.current.?.enclosing.?.locals[self.current.?.upvalues[slot].index].assigned = true;
        }
    }

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .NamedVariable,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = var_def,
            .components = .{
                .NamedVariable = .{
                    .name = name,
                    .definition = def_node,
                    .value = value,
                    .assign_token = assign_token,
                    .slot = @intCast(slot),
                    .slot_type = slot_type,
                    .slot_final = slot_final,
                },
            },
        },
    );

    return @intCast(node_slot);
}

/// Will parse qualified name too: identifier or A\B\C\identifier
fn qualifiedName(self: *Self) Error![]const Ast.TokenIndex {
    // Assumes one identifier has already been consumed
    std.debug.assert(self.ast.tokens.items(.tag)[self.current_token.? - 1] == .Identifier);

    var name = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);

    try name.append(self.current_token.? - 1);
    while ((try self.match(.AntiSlash)) and !self.check(.Eof)) {
        try self.consume(.Identifier, "Expected identifier");

        try name.append(self.current_token.? - 1);
    }

    return try name.toOwnedSlice();
}

fn variable(self: *Self, can_assign: bool) Error!Ast.Node.Index {
    return try self.namedVariable(
        try self.qualifiedName(),
        can_assign,
    );
}

fn fun(self: *Self, _: bool) Error!Ast.Node.Index {
    return try self.function(null, .Anonymous, null);
}

fn function(
    self: *Self,
    name: ?Ast.TokenIndex,
    function_type: obj.ObjFunction.FunctionType,
    this: ?*obj.ObjTypeDef,
) Error!Ast.Node.Index {
    var error_types = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    var arguments = std.ArrayList(Ast.FunctionType.Argument).init(self.gc.allocator);
    var generic_types = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);

    const function_signature = try self.ast.appendNode(
        .{
            .tag = .FunctionType,
            .location = self.current_token.? - 1,
            .end_location = undefined,
            .type_def = null,
            .components = .{
                .FunctionType = .{
                    .name = name,
                    .return_type = null,
                    .yield_type = null,
                    .error_types = undefined,
                    .arguments = undefined,
                    .generic_types = undefined,
                    .lambda = false,
                },
            },
        },
    );

    const function_node = try self.ast.appendNode(
        .{
            .tag = .Function,
            .location = self.current_token.? - 1,
            .end_location = undefined,
            .components = .{
                .Function = .{
                    .id = Ast.Function.nextId(),
                    .identifier = self.current_token.? - 1,
                    .upvalue_binding = .{},
                    .function_signature = function_signature,
                },
            },
        },
    );

    try self.beginFrame(function_type, function_node, this);
    try self.beginScope(null);

    // The functiont tyepdef is created in several steps, some need already parsed information like return type
    // We create the incomplete type now and enrich it.
    const function_def = obj.ObjFunction.FunctionDef{
        .id = obj.ObjFunction.FunctionDef.nextId(),
        .script_name = try self.gc.copyString(self.script_name),
        .name = if (name) |uname|
            try self.gc.copyString(self.ast.tokens.items(.lexeme)[uname])
        else
            try self.gc.copyString("anonymous"),
        .return_type = undefined,
        .yield_type = self.gc.type_registry.void_type,
        .function_type = function_type,
        .resolved_generics = null,
    };

    const function_resolved_type = obj.ObjTypeDef.TypeUnion{
        .Function = function_def,
    };

    var function_typedef = obj.ObjTypeDef{
        .def_type = .Function,
        .resolved_type = function_resolved_type,
    };

    // We replace it with a self.gc.type_registry.getTypeDef pointer at the end
    const type_defs = self.ast.nodes.items(.type_def);
    type_defs[function_node] = @constCast(&function_typedef);
    type_defs[function_signature] = @constCast(&function_typedef);

    // So any reference to a generic in the function's body can be resolved
    self.current.?.generics = @constCast(&function_typedef.resolved_type.?.Function.generic_types);

    // Parse generic & argument list
    if (function_type == .Test) {
        try self.consume(.String, "Expected a string after `test`.");
        self.ast.nodes.items(.components)[function_node].Function.test_message = self.current_token.? - 1;
    } else {
        // Generics
        if (try self.match(.DoubleColon)) {
            try self.consume(.Less, "Expected `<` at start of generic types list.");

            var i: usize = 0;
            while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
                try self.consume(.Identifier, "Expected generic type identifier");

                const generic_identifier_token = self.current_token.? - 1;
                const generic_identifier = try self.gc.copyString(self.ast.tokens.items(.lexeme)[generic_identifier_token]);
                if ((self.current.?.generics == null or self.current.?.generics.?.get(generic_identifier) == null) and (self.current_object == null or self.current_object.?.generics == null or self.current_object.?.generics.?.get(generic_identifier) == null)) {
                    try function_typedef.resolved_type.?.Function.generic_types.put(
                        self.gc.allocator,
                        generic_identifier,
                        try self.gc.type_registry.getTypeDef(
                            obj.ObjTypeDef{
                                .def_type = .Generic,
                                .resolved_type = .{
                                    .Generic = .{
                                        .origin = self.ast.nodes.items(.type_def)[function_node].?.resolved_type.?.Function.id,
                                        .index = i,
                                    },
                                },
                            },
                        ),
                    );

                    try generic_types.append(generic_identifier_token);
                } else {
                    const location = self.ast.tokens.get(self.current_token.? - 1);
                    self.reporter.reportErrorFmt(
                        .generic_type,
                        location,
                        location,
                        "Generic type `{s}` already defined",
                        .{
                            location.lexeme,
                        },
                    );
                }

                if (!self.check(.Greater)) {
                    try self.consume(.Comma, "Expected `,` between generic types");
                }
            }

            if (function_typedef.resolved_type.?.Function.generic_types.count() == 0) {
                const location = self.ast.tokens.get(self.current_token.? - 1);
                self.reporter.reportErrorAt(
                    .generic_type,
                    location,
                    location,
                    "Expected at least one generic type",
                );
            }

            try self.consume(.Greater, "Expected `>` after generic types list");
        }

        self.ast.nodes.items(.components)[function_signature].FunctionType.generic_types = try generic_types.toOwnedSlice();

        try self.consume(.LeftParen, "Expected `(` after function name.");

        // Arguments
        var arity: usize = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                arity += 1;
                if (arity > std.math.maxInt(u8)) {
                    const location = self.ast.tokens.get(self.current_token.? - 1);
                    self.reporter.reportErrorAt(
                        .arguments_count,
                        location,
                        location,
                        "Can't have more than 255 arguments.",
                    );
                }

                try self.consume(.Identifier, "Expected argument name");
                const identifier = self.current_token.? - 1;
                try self.consume(.Colon, "Expected `:`");

                const argument_type_node = try self.parseTypeDef(
                    function_typedef.resolved_type.?.Function.generic_types,
                    true,
                );

                self.ast.nodes.items(.type_def)[argument_type_node] = self.ast.nodes.items(.type_def)[
                    argument_type_node
                ];

                const argument_type = self.ast.nodes.items(.type_def)[argument_type_node].?;

                const slot = try self.parseVariable(
                    function_node,
                    identifier,
                    argument_type,
                    true, // function arguments are final
                    false,
                    "Expected argument name",
                );

                std.debug.assert(self.current.?.scope_depth > 0);
                const local = self.current.?.locals[slot];

                try function_typedef.resolved_type.?.Function.parameters.put(
                    self.gc.allocator,
                    try self.gc.copyString(self.ast.tokens.items(.lexeme)[local.name]),
                    local.type_def,
                );

                self.markInitialized();

                // Default arguments
                const default = switch (function_type) {
                    .Function, .Method, .Anonymous, .Extern => value: {
                        if (try self.match(.Equal)) {
                            const expr = try self.expression(false);
                            const expr_type_def = self.ast.nodes.items(.type_def)[expr];

                            if (expr_type_def != null and expr_type_def.?.def_type == .Placeholder and argument_type.def_type == .Placeholder) {
                                try obj.PlaceholderDef.link(
                                    self.gc.allocator,
                                    argument_type,
                                    expr_type_def.?,
                                    .Assignment,
                                );
                            }

                            break :value expr;
                        } else if (argument_type.optional) {
                            try function_typedef.resolved_type.?.Function.defaults.put(
                                self.gc.allocator,
                                try self.gc.copyString(self.ast.tokens.items(.lexeme)[local.name]),
                                Value.Null,
                            );
                        }

                        break :value null;
                    },
                    else => null,
                };

                try arguments.append(
                    .{
                        .name = local.name,
                        .type = argument_type_node,
                        .default = default,
                    },
                );

                if (default) |dft| {
                    try function_typedef.resolved_type.?.Function.defaults.put(
                        self.gc.allocator,
                        try self.gc.copyString(self.ast.tokens.items(.lexeme)[local.name]),
                        try self.ast.slice().toValue(dft, self.gc),
                    );
                }

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after function parameters.");
    }

    self.ast.nodes.items(.components)[function_signature].FunctionType.arguments = try arguments.toOwnedSlice();

    // Parse return type
    const return_type_node = if (function_type != .Test and try self.match(.Greater))
        try self.parseTypeDef(
            function_typedef.resolved_type.?.Function.generic_types,
            true,
        )
    else
        null;

    if (return_type_node) |rtn| {
        function_typedef.resolved_type.?.Function.return_type = self.ast.nodes.items(.type_def)[rtn] orelse self.gc.type_registry.void_type;
        self.ast.nodes.items(.components)[function_signature].FunctionType.return_type = rtn;
    }

    // Parse yield type
    const yield_type_node = if (return_type_node != null and function_type.canYield() and (try self.match(.Star))) yield: {
        try self.consume(.Greater, "Expected `>` before yield type");

        const yield_type_node = try self.parseTypeDef(
            function_typedef.resolved_type.?.Function.generic_types,
            true,
        );
        const yield_type = self.ast.nodes.items(.type_def)[yield_type_node].?;

        if (!yield_type.optional and yield_type.def_type != .Void) {
            self.reportErrorAtNode(
                .yield_type,
                yield_type_node,
                "Expected optional type or void",
                .{},
            );
        }

        function_typedef.resolved_type.?.Function.yield_type = yield_type;

        break :yield yield_type_node;
    } else null;

    self.ast.nodes.items(.components)[function_signature].FunctionType.yield_type = yield_type_node;

    if (yield_type_node == null) {
        function_typedef.resolved_type.?.Function.yield_type = self.gc.type_registry.void_type;
    }

    if (return_type_node == null) {
        function_typedef.resolved_type.?.Function.return_type = self.gc.type_registry.void_type;
    }

    // Error set
    if (function_type.canHaveErrorSet() and (try self.match(.BangGreater))) {
        var error_typedefs = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);

        const end_token: Token.Type = if (function_type.canOmitBody()) .Semicolon else .LeftBrace;

        while (!self.check(end_token) and !self.check(.DoubleArrow) and !self.check(.Eof)) {
            const error_type_node = try self.parseTypeDef(
                self.ast.nodes.items(.type_def)[function_node].?.resolved_type.?.Function.generic_types,
                true,
            );
            const error_type = self.ast.nodes.items(.type_def)[error_type_node].?;

            try error_types.append(error_type_node);
            try error_typedefs.append(error_type);

            if (error_type.optional) {
                self.reportErrorAtNode(
                    .error_type,
                    error_type_node,
                    "Error type can't be optional",
                    .{},
                );
            }

            if (!self.check(end_token) and !self.check(.DoubleArrow)) {
                try self.consume(.Comma, "Expected `,` after error type");
            }
        }

        if (error_types.items.len > 0) {
            function_typedef.resolved_type.?.Function.error_types = try error_typedefs.toOwnedSlice();
        }
    }

    self.ast.nodes.items(.components)[function_signature].FunctionType.error_types = try error_types.toOwnedSlice();
    self.ast.nodes.items(.end_location)[function_signature] = self.current_token.? - 1;

    // Parse body
    if (try self.match(.DoubleArrow)) {
        function_typedef.resolved_type.?.Function.lambda = true;
        self.ast.nodes.items(.components)[function_signature].FunctionType.lambda = true;
        const expr = try self.expression(false);
        const expr_type_def = self.ast.nodes.items(.type_def)[expr];
        self.ast.nodes.items(.components)[function_node].Function.body = expr;

        if (return_type_node == null and expr_type_def != null) {
            function_typedef.resolved_type.?.Function.return_type = expr_type_def.?;
        }
    } else if (!function_type.canOmitBody()) {
        if (return_type_node != null and !function_type.canOmitReturn()) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Expected `>` after function argument list.",
            );
        }

        try self.consume(.LeftBrace, "Expected `{` before function body.");
        const body = try self.block(null);
        self.ast.nodes.items(.components)[function_node].Function.body = body;
    }

    if (return_type_node == null and !function_typedef.resolved_type.?.Function.lambda) {
        function_typedef.resolved_type.?.Function.return_type = self.gc.type_registry.void_type;
    }

    if (function_type == .Extern) {
        if (self.flavor.resolveImports()) {
            const native_opt = if (!is_wasm)
                try self.importLibSymbol(
                    self.ast.nodes.items(.location)[function_node],
                    self.current_token.? - 1,
                    self.script_name,
                    function_typedef.resolved_type.?.Function.name.string,
                )
            else
                try self.importStaticLibSymbol(
                    self.script_name,
                    function_typedef.resolved_type.?.Function.name.string,
                );

            // Search for a dylib/so/dll with the same name as the current script
            if (native_opt) |native| {
                self.ast.nodes.items(.components)[function_node].Function.native = native;
            } else {
                return error.BuzzNoDll;
            }
        }
    } else {
        // Bind upvalues
        var i: usize = 0;
        while (i < self.current.?.upvalue_count) : (i += 1) {
            try self.ast.nodes.items(.components)[function_node].Function.upvalue_binding.put(
                self.gc.allocator,
                self.current.?.upvalues[i].index,
                if (self.current.?.upvalues[i].is_local) true else false,
            );
        }
    }

    self.ast.nodes.items(.type_def)[function_node] = try self.gc.type_registry.getTypeDef(function_typedef);
    self.ast.nodes.items(.end_location)[function_node] = self.current_token.? - 1;

    return self.endFrame();
}

fn pattern(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const source_slice = self.ast.tokens.items(.literal_string)[start_location].?;
    // Replace escaped pattern delimiter with delimiter
    const source = try std.mem.replaceOwned(
        u8,
        self.gc.allocator,
        source_slice,
        "\\\"",
        "\"",
    );

    var err_code: c_int = undefined;
    var err_offset: usize = undefined;
    const reg = if (!is_wasm)
        pcre.compile(
            source.ptr,
            source.len,
            // TODO: provide options to user
            0,
            &err_code,
            &err_offset,
            null,
        )
    else {};

    if (!is_wasm and reg == null) {
        var location = self.ast.tokens.get(self.current_token.? - 1).clone();
        location.column += @intCast(err_offset + 2);
        location.lexeme = location.lexeme[@intCast(2 + err_offset)..@intCast(2 + err_offset + 1)];

        var err_buf: [256]u8 = undefined;
        const err_len = pcre.getErrorMessage(
            err_code,
            &err_buf,
            err_buf.len,
        );

        self.reporter.reportErrorFmt(
            .pattern,
            location,
            location,
            "Could not compile pattern, error at {}: {s}",
            .{
                err_offset,
                err_buf[0..@intCast(err_len)],
            },
        );
        return CompileError.Unrecoverable;
    }

    const constant = try self.gc.allocateObject(
        obj.ObjPattern,
        .{
            .source = source,
            .pattern = if (!is_wasm) reg.? else {},
        },
    );

    return try self.ast.appendNode(
        .{
            .tag = .Pattern,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.gc.type_registry.pat_type,
            .components = .{
                .Pattern = constant,
            },
        },
    );
}

fn asyncCall(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const callable_node = try self.parsePrecedence(.Call, false);
    const callable = self.ast.nodes.get(callable_node);
    const node = try self.ast.appendNode(
        .{
            .tag = .AsyncCall,
            .location = start_location,
            .end_location = undefined,
            .type_def = null,
            .components = .{
                .AsyncCall = callable_node,
            },
        },
    );

    // Expression after `&` must either be a call or a dot call
    if (callable.tag != .Call and (callable.tag != .Dot or callable.components.Dot.member_kind != .Call)) {
        self.reportErrorAtNode(
            .syntax,
            callable_node,
            "Expected function call after `async`",
            .{},
        );

        return node;
    }

    const call_node = switch (callable.tag) {
        .Call => callable_node,
        .Dot => callable.components.Dot.value_or_call_or_enum.Call,
        else => unreachable,
    };

    const call_components = &self.ast.nodes.items(.components)[call_node];

    call_components.Call.is_async = true;

    const function_type = call_components.Call.callee_type_def;

    if (function_type.def_type == .Placeholder) {
        // create placeholders for return and yield types and link them with .Call and .Yield
        const return_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        self.current_token.? - 1,
                        self.current_token.? - 1,
                        null,
                    ),
                },
            },
        );

        try obj.PlaceholderDef.link(
            self.gc.allocator,
            function_type,
            return_placeholder,
            .Call,
        );

        const yield_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        self.current_token.? - 1,
                        self.current_token.? - 1,
                        null,
                    ),
                },
            },
        );

        try obj.PlaceholderDef.link(
            self.gc.allocator,
            function_type,
            yield_placeholder,
            .Yield,
        );

        self.ast.nodes.items(.type_def)[node] = try self.gc.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .Fiber,
                .resolved_type = .{
                    .Fiber = .{
                        .return_type = return_placeholder,
                        .yield_type = yield_placeholder,
                    },
                },
            },
        );
    } else {
        if (function_type.def_type != .Function) {
            io.print(
                "function_type.def_type {}\ncall_components.Call.callee {}\ncallable.tag {}\ncall_node tag {}\n",
                .{
                    function_type.def_type,
                    self.ast.nodes.items(.tag)[call_components.Call.callee],
                    callable.tag,
                    self.ast.nodes.items(.tag)[call_node],
                },
            );
            self.reportErrorAtNode(
                .callable,
                self.ast.nodes.items(.components)[call_node].Call.callee,
                "Can't be called",
                .{},
            );
        } else {
            const return_type = function_type.resolved_type.?.Function.return_type;
            const yield_type = function_type.resolved_type.?.Function.yield_type;

            const fiber_def = obj.ObjFiber.FiberDef{
                .return_type = return_type,
                .yield_type = yield_type,
            };

            const resolved_type = obj.ObjTypeDef.TypeUnion{
                .Fiber = fiber_def,
            };

            self.ast.nodes.items(.type_def)[node] = try self.gc.type_registry.getTypeDef(
                .{
                    .optional = try self.match(.Question),
                    .def_type = .Fiber,
                    .resolved_type = resolved_type,
                },
            );
        }
    }

    self.ast.nodes.items(.end_location)[node] = self.current_token.? - 1;

    return node;
}

fn resumeFiber(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth == 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "`resume` not allowed in global scope",
        );
    }

    const fiber_node = try self.parsePrecedence(.Primary, false);
    const node = try self.ast.appendNode(
        .{
            .tag = .Resume,
            .location = start_location,
            .end_location = undefined,
            .type_def = null,
            .components = .{
                .Resume = fiber_node,
            },
        },
    );

    const fiber_type = self.ast.nodes.items(.type_def)[fiber_node];

    if (fiber_type == null) {
        unreachable;
    } else if (fiber_type.?.def_type == .Placeholder) {
        const yield_placeholder = try (try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        self.current_token.? - 1,
                        self.current_token.? - 1,
                        null,
                    ),
                },
            },
        )).cloneOptional(&self.gc.type_registry);

        try obj.PlaceholderDef.link(
            self.gc.allocator,
            fiber_type.?,
            yield_placeholder,
            .Yield,
        );

        self.ast.nodes.items(.type_def)[node] = yield_placeholder;
    } else {
        if (fiber_type.?.def_type != .Fiber) {
            self.reportErrorAtNode(
                .resumable,
                fiber_node,
                "Can't be resumed",
                .{},
            );
        } else {
            const fiber = fiber_type.?.resolved_type.?.Fiber;

            // Resume returns null if nothing was yielded and/or fiber reached its return statement
            self.ast.nodes.items(.type_def)[node] = fiber.yield_type;
        }
    }

    self.ast.nodes.items(.end_location)[node] = self.current_token.? - 1;

    return node;
}

fn resolveFiber(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth == 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "`resolve` not allowed in global scope",
        );
    }

    const fiber = try self.parsePrecedence(.Primary, false);
    const node = try self.ast.appendNode(
        .{
            .tag = .Resolve,
            .location = start_location,
            .end_location = undefined,
            .type_def = null,
            .components = .{
                .Resolve = fiber,
            },
        },
    );
    const fiber_type = self.ast.nodes.items(.type_def)[fiber];

    if (fiber_type == null) {
        unreachable;
    } else if (fiber_type.?.def_type == .Placeholder) {
        const return_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    .Placeholder = obj.PlaceholderDef.init(
                        self.current_token.? - 1,
                        self.current_token.? - 1,
                        null,
                    ),
                },
            },
        );

        try obj.PlaceholderDef.link(
            self.gc.allocator,
            fiber_type.?,
            return_placeholder,
            .Yield,
        );

        self.ast.nodes.items(.type_def)[node] = return_placeholder;
    } else {
        if (fiber_type.?.def_type != .Fiber) {
            self.reportErrorAtNode(
                .resolvable,
                fiber,
                "Can't be resolveed",
                .{},
            );
        } else {
            const fiber_def = fiber_type.?.resolved_type.?.Fiber;

            self.ast.nodes.items(.type_def)[node] = fiber_def.return_type;
        }
    }

    self.ast.nodes.items(.end_location)[node] = self.current_token.? - 1;

    return node;
}

fn yield(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth == 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "`yield` not allowed in global scope",
        );
    }

    const expr = try self.parsePrecedence(.Primary, false);

    return try self.ast.appendNode(
        .{
            .tag = .Yield,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.ast.nodes.items(.type_def)[expr],
            .components = .{
                .Yield = expr,
            },
        },
    );
}

fn range(self: *Self, _: bool, low: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const high = try self.expression(false);

    self.markInitialized();

    return try self.ast.appendNode(
        .{
            .tag = .Range,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Range,
                    .resolved_type = .{
                        .Range = {},
                    },
                },
            ),
            .components = .{
                .Range = .{
                    .high = high,
                    .low = low,
                },
            },
        },
    );
}

fn typeOfExpression(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const expr = try self.parsePrecedence(.Unary, false);

    return try self.ast.appendNode(
        .{
            .tag = .TypeOfExpression,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(.{ .def_type = .Type }),
            .components = .{
                .TypeOfExpression = expr,
            },
        },
    );
}

fn mutableExpression(self: *Self, _: bool) Error!Ast.Node.Index {
    const expr = try self.parsePrecedence(.Unary, false);
    self.ast.nodes.items(.type_def)[expr] = try self.ast.nodes.items(.type_def)[expr].?.cloneMutable(&self.gc.type_registry, true);

    switch (self.ast.nodes.items(.tag)[expr]) {
        .List, .Map, .ObjectInit => {},
        else => self.reportErrorAtNode(
            .not_mutable,
            expr,
            "Can't be mutable",
            .{},
        ),
    }

    return expr;
}

fn blockExpression(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftBrace, "Expected `{` at start of block expression");

    try self.beginScope(null);
    self.current.?.in_block_expression = self.current.?.scope_depth;

    var statements = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);

    var out: ?Ast.Node.Index = null;
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        if (try self.declarationOrStatement(null)) |stmt| {
            try statements.append(stmt);

            if (self.ast.nodes.items(.tag)[stmt] == .Out) {
                if (out != null) {
                    self.reportErrorAtNode(
                        .syntax,
                        stmt,
                        "Only one `out` statement is allowed in block expression",
                        .{},
                    );
                }

                out = stmt;
            }
        }
    }

    if (out != null and statements.getLastOrNull() != out) {
        if (statements.getLastOrNull()) |stmt| {
            self.reportErrorAtNode(
                .syntax,
                stmt,
                "Last block expression statement must be `out`",
                .{},
            );
        } else {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Last block expression statement must be `out`",
            );
        }
    }

    try self.consume(.RightBrace, "Expected `}` at end of block expression");

    self.current.?.in_block_expression = null;

    return try self.ast.appendNode(
        .{
            .tag = .BlockExpression,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = if (out) |o|
                self.ast.nodes.items(.type_def)[o]
            else
                self.gc.type_registry.void_type,
            .components = .{
                .BlockExpression = try statements.toOwnedSlice(),
            },
            .ends_scope = try self.endScope(),
        },
    );
}

fn binary(self: *Self, _: bool, left: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.ast.nodes.items(.location)[left];

    const operator_token = self.current_token.? - 1;
    const operator = self.ast.tokens.items(.tag)[operator_token];
    const rule = getRule(operator);

    const right = try self.parsePrecedence(
        @enumFromInt(@intFromEnum(rule.precedence) + 1),
        false,
    );

    const type_defs = self.ast.nodes.items(.type_def);
    const right_type_def = type_defs[right];
    const left_type_def = type_defs[left];

    return try self.ast.appendNode(
        .{
            .tag = .Binary,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = switch (operator) {
                .QuestionQuestion => right_type_def,

                .Greater,
                .Less,
                .GreaterEqual,
                .LessEqual,
                .BangEqual,
                .EqualEqual,
                => self.gc.type_registry.bool_type,

                .Plus => left_type_def orelse right_type_def,

                .ShiftLeft,
                .ShiftRight,
                .Ampersand,
                .Bor,
                .Xor,
                => self.gc.type_registry.int_type,

                .Minus,
                .Star,
                .Percent,
                .Slash,
                => if ((left_type_def != null and left_type_def.?.def_type == .Double) or (right_type_def != null and right_type_def.?.def_type == .Double))
                    self.gc.type_registry.float_type
                else
                    self.gc.type_registry.int_type,

                else => unreachable,
            },
            .components = .{
                .Binary = .{
                    .operator = operator,
                    .left = left,
                    .right = right,
                },
            },
        },
    );
}

fn typeExpression(self: *Self, _: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const type_def = try self.parseTypeDef(null, true);

    try self.consume(.Greater, "Expected `>` after type expression.");

    return try self.ast.appendNode(
        .{
            .tag = .TypeExpression,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(.{ .def_type = .Type }),
            .components = .{
                .TypeExpression = type_def,
            },
        },
    );
}

fn funDeclaration(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var function_type: obj.ObjFunction.FunctionType = .Function;

    if (self.ast.tokens.items(.tag)[self.current_token.? - 1] == .Extern) {
        try self.consume(.Fun, "Expected `fun` after `extern`.");

        function_type = .Extern;
    }

    try self.consume(.Identifier, "Expected function name.");
    const name_token = self.current_token.? - 1;

    const current_function_type_def = self.ast.nodes.items(.type_def)[self.current.?.function_node];
    const is_main = std.mem.eql(u8, self.ast.tokens.items(.lexeme)[name_token], "main") and
        current_function_type_def != null and
        current_function_type_def.?.resolved_type.?.Function.function_type == .ScriptEntryPoint;

    if (is_main) {
        if (function_type == .Extern) {
            self.reporter.reportErrorAt(
                .extern_main,
                self.ast.tokens.get(start_location),
                self.ast.tokens.get(name_token),
                "`main` can't be `extern`.",
            );
        }

        function_type = .EntryPoint;
    }

    const function_node = try self.function(name_token, function_type, null);
    const fun_typedef = self.ast.nodes.items(.type_def)[function_node].?;

    if (fun_typedef.resolved_type.?.Function.lambda) {
        try self.consume(.Semicolon, "Expected `;` after statement");
    }

    if (function_type == .Extern) {
        try self.consume(.Semicolon, "Expected `;` after statement.");
    }

    // Enforce main signature
    const fun_def = fun_typedef.resolved_type.?.Function;
    if (is_main) {
        var signature_valid = true;
        if (fun_def.parameters.count() > 1 or (fun_def.return_type.def_type != .Integer and fun_def.return_type.def_type != .Void)) {
            signature_valid = false;
        } else if (fun_def.parameters.count() > 0) {
            const first_param = fun_def.parameters.get(fun_def.parameters.keys()[0]);
            if (first_param == null or
                !(try self.parseTypeDefFrom("[str]")).eql(first_param.?))
            {
                signature_valid = false;
            }
        }

        if (!signature_valid) {
            const main_def_str = fun_typedef.toStringAlloc(self.gc.allocator) catch @panic("Out of memory");
            defer self.gc.allocator.free(main_def_str);
            self.reporter.reportErrorFmt(
                .main_signature,
                self.ast.tokens.get(self.ast.nodes.items(.location)[function_node]),
                self.ast.tokens.get(self.ast.nodes.items(.end_location)[function_node]),
                "Expected `main` signature to be `fun main([ args: [str] ]) > void|int` got {s}",
                .{
                    main_def_str,
                },
            );
        }
    }

    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);

    const slot: usize = try self.declareVariable(
        @intCast(node_slot),
        fun_typedef,
        name_token,
        true,
        false,
        true,
    );

    self.markInitialized();

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .FunDeclaration,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = fun_typedef,
            .components = .{
                .FunDeclaration = .{
                    .function = function_node,
                    .slot = @intCast(slot),
                    .slot_type = if (self.current.?.scope_depth == 0) .Global else .Local,
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn exportStatement(self: *Self, docblock: ?Ast.TokenIndex) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;

    if (self.namespace == null) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "A exporting script must provide a namespace",
        );
    }

    if (self.check(.Identifier) and
        (try self.checkAhead(.Semicolon, 0) or try self.checkAhead(.As, 0) or try self.checkAhead(.AntiSlash, 0)))
    {
        try self.consume(.Identifier, "Expected identifier");
        const qualified_name = try self.qualifiedName();

        // Search for a global with that name
        if (try self.resolveGlobal(@intCast(node_slot), qualified_name)) |slot| {
            const global = &self.globals.items[slot];

            global.referenced = true;
            global.exported = true;
            if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
                const imported_from = global.imported_from.?;

                try self.script_imports.put(
                    self.gc.allocator,
                    imported_from,
                    .{
                        .location = self.script_imports.get(imported_from).?.location,
                        .end_location = self.script_imports.get(imported_from).?.end_location,
                        .referenced = true,
                    },
                );
            }

            var alias: ?Ast.TokenIndex = null;
            if (try self.match(.As)) {
                try self.consume(.Identifier, "Expected identifier after `as`.");

                global.export_alias = self.current_token.? - 1;
                alias = self.current_token.? - 1;
            }

            // If we're exporting an imported global, overwrite its namespace
            const name = global.name[global.name.len - 1];
            // self.gc.allocator.free(global.name);
            var new_global_name = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
            try new_global_name.appendSlice(self.namespace orelse &[_]Ast.TokenIndex{});
            try new_global_name.append(name);
            global.name = try new_global_name.toOwnedSlice();

            try self.consume(.Semicolon, "Expected `;` after statement.");

            self.ast.nodes.set(
                node_slot,
                .{
                    .tag = .Export,
                    .location = start_location,
                    .end_location = self.current_token.? - 1,
                    .type_def = global.type_def,
                    .components = .{
                        .Export = .{
                            .name = qualified_name,
                            .alias = alias,
                            .declaration = null,
                        },
                    },
                },
            );

            return @intCast(node_slot);
        }
    } else {
        self.exporting = true;
        if (try self.declaration(docblock)) |decl| {
            const global = &self.globals.items[self.globals.items.len - 1];
            global.referenced = true;
            self.exporting = false;

            self.ast.nodes.set(
                node_slot,
                .{
                    .tag = .Export,
                    .location = start_location,
                    .end_location = self.current_token.? - 1,
                    .docblock = docblock,
                    .type_def = self.ast.nodes.items(.type_def)[decl],
                    .components = .{
                        .Export = .{
                            .name = null,
                            .alias = null,
                            .declaration = decl,
                        },
                    },
                },
            );

            return @intCast(node_slot);
        }
        self.exporting = false;
    }

    self.reporter.reportErrorAt(
        .syntax,
        self.ast.tokens.get(start_location),
        self.ast.tokens.get(self.current_token.? - 1),
        "Expected identifier or declaration.",
    );

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .Export,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Export = .{
                    .name = &[_]Ast.TokenIndex{self.current_token.? - 1},
                    .alias = null,
                    .declaration = null,
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn objectDeclaration(self: *Self) Error!Ast.Node.Index {
    if (self.current.?.scope_depth > 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Object must be defined at top-level.",
        );
    }

    const start_location = self.current_token.? - 1;

    // Conforms to protocols?
    var protocols = std.AutoHashMapUnmanaged(*obj.ObjTypeDef, void){};
    var protocol_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    var protocol_count: usize = 0;
    if (try self.match(.Less)) {
        while (!self.check(.Greater) and !self.check(.Eof)) : (protocol_count += 1) {
            if (protocol_count > std.math.maxInt(u8)) {
                const location = self.ast.tokens.get(self.current_token.? - 1);
                self.reporter.reportErrorAt(
                    .protocols_count,
                    location,
                    location,
                    "Can't conform to more than 255 protocols",
                );
            }

            try self.consume(.Identifier, "Expected protocol identifier");

            const protocol_node = try self.parseUserType(false, false);
            const protocol = self.ast.nodes.items(.type_def)[protocol_node].?;

            if (protocols.get(protocol) != null) {
                const locations = self.ast.nodes.items(.location);
                const end_locations = self.ast.nodes.items(.end_location);
                self.reporter.reportWithOrigin(
                    .already_conforming_protocol,
                    self.ast.tokens.get(locations[protocol_node]),
                    self.ast.tokens.get(end_locations[protocol_node]),
                    protocol.resolved_type.?.Protocol.location,
                    protocol.resolved_type.?.Protocol.location,
                    "Already conforming to `{s}`.",
                    .{
                        protocol.resolved_type.?.Protocol.name.string,
                    },
                    null,
                );
            }

            try protocols.put(
                self.gc.allocator,
                protocol,
                {},
            );
            try protocol_nodes.append(protocol_node);

            if (!(try self.match(.Comma))) {
                break;
            }
        }

        try self.consume(.Greater, "Expected `>` after protocols list");
    }

    // Get object name
    try self.consume(.Identifier, "Expected object name.");
    const object_name_token = self.current_token.? - 1;
    const object_name = self.ast.tokens.get(object_name_token).clone();

    // Qualified name to avoid cross script collision
    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", "\\");
    defer self.gc.allocator.free(qualifier);
    var qualified_object_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_object_name.deinit();
    try qualified_object_name.writer().print("{s}.{s}", .{ qualifier, object_name.lexeme });

    // Create a placeholder for self-reference which will be resolved at the end when declaring the object
    const placeholder_index = try self.declarePlaceholder(object_name_token, null);
    const object_placeholder = self.globals.items[placeholder_index].type_def;

    var object_def = obj.ObjObject.ObjectDef.init(
        object_name,
        try self.gc.copyString(object_name.lexeme),
        try self.gc.copyString(qualified_object_name.items),
        false,
    );

    object_def.conforms_to.deinit(self.gc.allocator);
    object_def.conforms_to = protocols;

    const resolved_type = obj.ObjTypeDef.TypeUnion{ .Object = object_def };

    // Create type
    var object_type = obj.ObjTypeDef{
        .def_type = .Object,
        .resolved_type = resolved_type,
    };

    const object_frame = ObjectFrame{
        .name = object_name,
        .type_def = object_placeholder,
        .generics = &object_type.resolved_type.?.Object.generic_types,
    };

    self.current_object = object_frame;

    // Parse generic types
    var generics = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
    if (try self.match(.DoubleColon)) {
        try self.consume(.Less, "Expected generic types list after `::`");
        var i: usize = 0;
        while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
            try self.consume(.Identifier, "Expected generic type identifier");

            const generic_identifier_token = self.current_token.? - 1;
            const generic_identifier = try self.gc.copyString(self.ast.tokens.items(.lexeme)[generic_identifier_token]);
            if (object_type.resolved_type.?.Object.generic_types.get(generic_identifier) == null) {
                const generic = obj.ObjTypeDef.GenericDef{
                    .origin = object_def.id,
                    .index = i,
                };
                const resolved_generic_type = obj.ObjTypeDef.TypeUnion{ .Generic = generic };
                try object_type.resolved_type.?.Object.generic_types.put(
                    self.gc.allocator,
                    generic_identifier,
                    try self.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Generic,
                            .resolved_type = resolved_generic_type,
                        },
                    ),
                );

                try generics.append(generic_identifier_token);
            } else {
                const location = self.ast.tokens.get(self.current_token.? - 1);
                self.reporter.reportErrorFmt(
                    .generic_type,
                    location,
                    location,
                    "Generic type `{s}` already defined",
                    .{
                        location.lexeme,
                    },
                );
            }

            if (!self.check(.Greater)) {
                try self.consume(.Comma, "Expected `,` between generic types");
            }
        }

        if (object_type.resolved_type.?.Object.generic_types.count() == 0) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .generic_type,
                location,
                location,
                "Expected at least one generic type",
            );
        }

        try self.consume(.Greater, "Expected `>` after generic types list");
    }

    try self.beginScope(null);

    // Body
    try self.consume(.LeftBrace, "Expected `{` before object body.");

    var members = std.ArrayList(Ast.ObjectDeclaration.Member).init(self.gc.allocator);

    // To avoid using the same name twice
    var fields = std.StringArrayHashMap(void).init(self.gc.allocator);
    defer fields.deinit();

    // Members types
    var properties_type = std.StringHashMap(*obj.ObjTypeDef).init(self.gc.allocator);

    // Docblocks
    var property_idx: usize = 0;
    var static_or_method_property_idx: usize = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        const docblock = if (try self.match(.Docblock))
            self.current_token.? - 1
        else
            null;

        const static = try self.match(.Static);
        const mutable = !static and try self.match(.Mut);

        // Method or static method
        if (try self.match(.Fun)) {
            const method_token = self.current_token.?;
            const method_node = try self.method(
                false,
                static,
                if (static)
                    object_placeholder
                else
                    try object_placeholder.toInstance(
                        &self.gc.type_registry,
                        mutable,
                    ),
            );

            const method_type_def = self.ast.nodes.items(.type_def)[method_node];
            const method_name = method_type_def.?.resolved_type.?.Function.name.string;

            if (fields.get(method_name) != null) {
                const location = self.ast.tokens.get(method_token);
                self.reporter.reportErrorAt(
                    .property_already_exists,
                    location,
                    location,
                    "A member with that name already exists.",
                );
            }

            // Does a placeholder exists for this name ?
            if (static) {
                if (object_type.resolved_type.?.Object.static_placeholders.get(method_name)) |placeholder| {
                    try self.resolvePlaceholder(placeholder.placeholder, method_type_def.?, true);

                    if (self.flavor == .Ast) {
                        for (placeholder.referrers.items) |referrer| {
                            try self.resolveReferrer(referrer, method_node);
                        }

                        var referrers = placeholder.referrers;
                        referrers.deinit(self.gc.allocator);
                    }

                    // Now we know the placeholder was a method
                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "resolved static method for `{s}`\n",
                            .{
                                method_name,
                            },
                        );
                    }
                    _ = object_type.resolved_type.?.Object.static_placeholders.remove(method_name);
                }
            } else {
                if (object_type.resolved_type.?.Object.placeholders.get(method_name)) |placeholder| {
                    try self.resolvePlaceholder(placeholder.placeholder, method_type_def.?, true);

                    if (self.flavor == .Ast) {
                        for (placeholder.referrers.items) |referrer| {
                            try self.resolveReferrer(referrer, method_node);
                        }

                        var referrers = placeholder.referrers;
                        referrers.deinit(self.gc.allocator);
                    }

                    // Now we know the placeholder was a method
                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "resolved method placeholder for `{s}`\n",
                            .{
                                method_name,
                            },
                        );
                    }
                    _ = object_type.resolved_type.?.Object.placeholders.remove(method_name);
                }
            }

            try object_type.resolved_type.?.Object.fields.put(
                self.gc.allocator,
                method_name,
                .{
                    .name = method_name,
                    .type_def = method_type_def.?,
                    .final = true,
                    .static = static,
                    .location = self.ast.tokens.get(method_token),
                    .method = true,
                    .has_default = false,
                    .mutable = mutable,
                    .index = static_or_method_property_idx,
                },
            );

            static_or_method_property_idx += 1;

            try members.append(
                .{
                    .name = method_token,
                    .docblock = docblock,
                    .method = true,
                    .method_or_default_value = method_node,
                    .property_type = null,
                },
            );
            try fields.put(method_name, {});
            try properties_type.put(method_name, self.ast.nodes.items(.type_def)[method_node].?);
        } else { // Property
            const final = try self.match(.Final);

            try self.consume(.Identifier, "Expected property name.");
            const property_token = self.current_token.? - 1;
            const property_name = self.ast.tokens.get(property_token);

            try self.consume(.Colon, "Expected `:`");
            const property_type = try self.parseTypeDef(null, true);
            const property_type_def = self.ast.nodes.items(.type_def)[property_type];

            if (fields.get(property_name.lexeme) != null) {
                self.reporter.reportErrorAt(
                    .property_already_exists,
                    property_name,
                    property_name,
                    "A member with that name already exists.",
                );
            }

            // Does a placeholder exists for this name ?
            if (static) {
                if (object_type.resolved_type.?.Object.static_placeholders.get(property_name.lexeme)) |placeholder| {
                    try self.resolvePlaceholder(placeholder.placeholder, property_type_def.?, false);

                    if (self.flavor == .Ast) {
                        for (placeholder.referrers.items) |referrer| {
                            try self.resolveReferrer(referrer, property_type);
                        }

                        var referrers = placeholder.referrers;
                        referrers.deinit(self.gc.allocator);
                    }

                    // Now we know the placeholder was a field
                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "resolved static property placeholder for `{s}`\n",
                            .{
                                property_name.lexeme,
                            },
                        );
                    }
                    _ = object_type.resolved_type.?.Object.static_placeholders.remove(property_name.lexeme);
                }
            } else {
                if (object_type.resolved_type.?.Object.placeholders.get(property_name.lexeme)) |placeholder| {
                    try self.resolvePlaceholder(placeholder.placeholder, property_type_def.?, false);

                    if (self.flavor == .Ast) {
                        for (placeholder.referrers.items) |referrer| {
                            try self.resolveReferrer(referrer, property_type);
                        }

                        var referrers = placeholder.referrers;
                        referrers.deinit(self.gc.allocator);
                    }

                    // Now we know the placeholder was a field
                    if (BuildOptions.debug_placeholders) {
                        io.print(
                            "resolved property placeholder for `{s}`\n",
                            .{
                                property_name.lexeme,
                            },
                        );
                    }
                    _ = object_type.resolved_type.?.Object.placeholders.remove(property_name.lexeme);
                }
            }

            const default = if (try self.match(.Equal))
                try self.expression(false)
            else if (property_type_def.?.optional)
                try self.ast.appendNode(
                    .{
                        .tag = .Null,
                        .location = self.current_token.? - 1,
                        .end_location = self.current_token.? - 1,
                        .components = .{
                            .Null = {},
                        },
                        .type_def = self.gc.type_registry.void_type,
                    },
                )
            else
                null;

            if (default != null and !(try self.ast.slice().isConstant(self.gc.allocator, default.?))) {
                self.reportErrorAtNode(
                    .constant_default,
                    default.?,
                    "Default value must be constant",
                    .{},
                );
            }

            if (static) {
                if (!self.check(.RightBrace) or self.check(.Semicolon)) {
                    try self.consume(.Semicolon, "Expected `;` after static property definition.");
                }
            } else {
                if (!self.check(.RightBrace) or self.check(.Comma)) {
                    try self.consume(.Comma, "Expected `,` after property definition.");
                }
            }

            try object_type.resolved_type.?.Object.fields.put(
                self.gc.allocator,
                property_name.lexeme,
                .{
                    .name = property_name.lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[property_type].?,
                    .final = final,
                    .static = static,
                    .location = property_name,
                    .method = false,
                    .mutable = false, // makes only sense for methods
                    .has_default = default != null,
                    .index = if (static)
                        static_or_method_property_idx
                    else
                        property_idx,
                },
            );

            if (static) {
                static_or_method_property_idx += 1;
            } else {
                property_idx += 1;
            }

            try members.append(
                .{
                    .name = property_token,
                    .docblock = docblock,
                    .method = false,
                    .method_or_default_value = default,
                    .property_type = property_type,
                },
            );
            try fields.put(property_name.lexeme, {});
            try properties_type.put(
                property_name.lexeme,
                self.ast.nodes.items(.type_def)[property_type].?,
            );
        }
    }

    try object_type.resolved_type.?.Object.sortFieldIndexes(self.gc.allocator);

    try self.consume(.RightBrace, "Expected `}` after object body.");

    const scope_end = try self.endScope();

    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);

    const slot = try self.declareVariable(
        @intCast(node_slot),
        &object_type, // Should resolve object_name_tokenect_placeholder and be discarded
        object_name_token,
        true, // Object is always final
        false,
        true,
    );

    std.debug.assert(!object_type.optional);

    self.markInitialized();

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .ObjectDeclaration,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = object_placeholder,
            .ends_scope = scope_end,
            .components = .{
                .ObjectDeclaration = .{
                    .name = object_name_token,
                    .slot = @intCast(slot),
                    .protocols = try protocol_nodes.toOwnedSlice(),
                    .generics = try generics.toOwnedSlice(),
                    .members = try members.toOwnedSlice(),
                },
            },
        },
    );

    std.debug.assert(object_placeholder.resolved_type.?.Object.placeholders.count() == 0 or object_placeholder.resolved_type.?.Object.static_placeholders.count() == 0);

    self.current_object = null;

    return @intCast(node_slot);
}

fn method(self: *Self, abstract: bool, static: bool, this: *obj.ObjTypeDef) Error!Ast.Node.Index {
    try self.consume(.Identifier, "Expected method name.");

    return try self.function(
        self.current_token.? - 1,
        if (abstract)
            .Abstract
        else if (static)
            .Function
        else
            .Method,
        this,
    );
}

fn protocolDeclaration(self: *Self) Error!Ast.Node.Index {
    if (self.current.?.scope_depth > 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Protocol must be defined at top-level.",
        );
    }

    const start_location = self.current_token.? - 1;

    // Get protocol name
    try self.consume(.Identifier, "Expected protocol name.");
    const protocol_name = self.current_token.? - 1;

    // Qualified name to avoid cross script collision
    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", "\\");
    defer self.gc.allocator.free(qualifier);
    var qualified_protocol_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_protocol_name.deinit();
    try qualified_protocol_name.writer().print(
        "{s}.{s}",
        .{
            qualifier,
            self.ast.tokens.items(.lexeme)[protocol_name],
        },
    );

    // Create a placeholder for self-reference which will be resolved at the end when declaring the object
    const placeholder_index = try self.declarePlaceholder(protocol_name, null);
    const protocol_placeholder = self.globals.items[placeholder_index].type_def;

    // Create type
    var protocol_type: obj.ObjTypeDef = .{
        .def_type = .Protocol,
        .resolved_type = .{
            .Protocol = obj.ObjObject.ProtocolDef.init(
                self.ast.tokens.get(protocol_name),
                try self.gc.copyString(self.ast.tokens.items(.lexeme)[protocol_name]),
                try self.gc.copyString(qualified_protocol_name.items),
            ),
        },
    };

    try self.beginScope(null);

    // Body
    try self.consume(.LeftBrace, "Expected `{` before protocol body.");

    var fields = std.StringHashMap(void).init(self.gc.allocator);
    defer fields.deinit();
    var methods = std.ArrayList(Ast.ProtocolDeclaration.Method).init(self.gc.allocator);
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        const docblock = if (try self.match(.Docblock))
            self.current_token.? - 1
        else
            null;

        const mutable = try self.match(.Mut);
        try self.consume(.Fun, "Expected method definition");

        const method_name_token = self.current_token.?;
        const method_node = try self.method(
            true,
            false,
            try protocol_placeholder.toInstance(
                &self.gc.type_registry,
                mutable,
            ),
        );
        const method_type_def = self.ast.nodes.items(.type_def)[method_node];

        try self.consume(.Semicolon, "Expected `;` after method definition");

        const method_name = method_type_def.?.resolved_type.?.Function.name.string;

        if (fields.get(method_name) != null) {
            const location = self.ast.tokens.get(method_name_token);
            self.reporter.reportErrorAt(
                .property_already_exists,
                location,
                location,
                "A method with that name already exists.",
            );
        }

        try protocol_type.resolved_type.?.Protocol.methods.put(
            self.gc.allocator,
            method_name,
            .{
                .type_def = method_type_def.?,
                .mutable = mutable,
            },
        );

        // FIXME: we should not need this, the VM has a reference to the AST
        try protocol_type.resolved_type.?.Protocol.methods_locations.put(
            self.gc.allocator,
            method_name,
            self.ast.tokens.get(method_name_token),
        );

        try fields.put(method_name, {});
        try methods.append(
            .{
                .docblock = docblock,
                .method = method_node,
            },
        );
    }

    try self.consume(.RightBrace, "Expected `}` after protocol body.");

    const scope_end = try self.endScope();

    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);

    const slot = try self.declareVariable(
        @intCast(node_slot),
        &protocol_type, // Should resolve protocol_placeholder and be discarded
        protocol_name,
        true, // Protocol is always final
        false,
        true,
    );

    std.debug.assert(!protocol_type.optional);

    self.markInitialized();

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .ProtocolDeclaration,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = protocol_placeholder,
            .ends_scope = scope_end,
            .components = .{
                .ProtocolDeclaration = .{
                    .name = protocol_name,
                    .slot = @intCast(slot),
                    .methods = try methods.toOwnedSlice(),
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn enumDeclaration(self: *Self) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth > 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Enum must be defined at top-level.",
        );
    }

    const enum_case_type_node =
        if (try self.match(.Less))
            try self.parseTypeDef(null, true)
        else
            null;

    if (enum_case_type_node != null) {
        try self.consume(.Greater, "Expected `>` after enum type.");
    }

    const enum_case_type = if (enum_case_type_node) |enum_type|
        try self.ast.nodes.items(.type_def)[enum_type].?.toInstance(
            &self.gc.type_registry,
            false,
        )
    else
        self.gc.type_registry.int_type;

    try self.consume(.Identifier, "Expected enum name.");
    const enum_name = self.current_token.? - 1;
    const enum_name_lexeme = self.ast.tokens.items(.lexeme)[enum_name];

    // Qualified name to avoid cross script collision
    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
    defer self.gc.allocator.free(qualifier);
    var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_name.deinit();
    try qualified_name.writer().print(
        "{s}.{s}",
        .{ qualifier, enum_name_lexeme },
    );

    const enum_def = obj.ObjEnum.EnumDef.init(
        try self.gc.copyString(enum_name_lexeme),
        self.ast.tokens.get(enum_name),
        try self.gc.copyString(qualified_name.items),
        enum_case_type,
        undefined,
    );

    const enum_resolved = obj.ObjTypeDef.TypeUnion{ .Enum = enum_def };

    const enum_type = try self.gc.type_registry.getTypeDef(
        .{
            .def_type = .Enum,
            .resolved_type = enum_resolved,
        },
    );

    const slot: usize = try self.declareVariable(
        @intCast(node_slot),
        enum_type,
        enum_name,
        true,
        false,
        true,
    );
    self.markInitialized();

    try self.consume(.LeftBrace, "Expected `{` before enum body.");

    var cases = std.ArrayList(Ast.Enum.Case).init(self.gc.allocator);
    var def_cases = std.ArrayList([]const u8).init(self.gc.allocator);
    var picked = std.ArrayList(bool).init(self.gc.allocator);
    var case_index: v.Integer = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) : (case_index += 1) {
        if (case_index > std.math.maxInt(u24)) {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .enum_cases_count,
                location,
                location,
                "Too many enum cases.",
            );
        }

        const docblock = if (try self.match(.Docblock))
            self.current_token.? - 1
        else
            null;

        try self.consume(.Identifier, "Expected case name.");
        const case_name_token = self.current_token.? - 1;
        const case_name = self.ast.tokens.items(.lexeme)[case_name_token];

        if (enum_case_type_node != null and (enum_case_type.def_type != .String or self.check(.Equal))) {
            try self.consume(.Equal, "Expected `=` after case name.");

            try cases.append(
                .{
                    .name = case_name_token,
                    .docblock = docblock,
                    .value = try self.expression(false),
                },
            );
            try picked.append(true);
        } else {
            if (enum_case_type.def_type == .Integer) {
                try cases.append(
                    .{
                        .name = case_name_token,
                        .docblock = docblock,
                        .value = try self.ast.appendNode(
                            .{
                                .tag = .Integer,
                                .location = self.current_token.? - 1,
                                .end_location = self.current_token.? - 1,
                                .type_def = self.gc.type_registry.int_type,
                                .components = .{
                                    .Integer = case_index,
                                },
                            },
                        ),
                    },
                );
            } else {
                try cases.append(
                    .{
                        .name = case_name_token,
                        .docblock = docblock,
                        .value = try self.ast.appendNode(
                            .{
                                .tag = .StringLiteral,
                                .location = self.current_token.? - 1,
                                .end_location = self.current_token.? - 1,
                                .type_def = self.gc.type_registry.str_type,
                                .components = .{
                                    .StringLiteral = try self.gc.copyString(case_name),
                                },
                            },
                        ),
                    },
                );
            }

            try picked.append(false);
        }

        try def_cases.append(case_name);

        if (!self.check(.RightBrace)) {
            try self.consume(.Comma, "Expected `,` after case definition.");
        }
    }

    enum_type.resolved_type.?.Enum.cases = try def_cases.toOwnedSlice();

    try self.consume(.RightBrace, "Expected `}` after enum body.");

    if (case_index == 0) {
        self.reporter.reportErrorAt(
            .enum_cases_count,
            self.ast.tokens.get(start_location),
            self.ast.tokens.get(self.current_token.? - 1),
            "Enum must have at least one case.",
        );
    }

    const cases_slice = try cases.toOwnedSlice();
    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .Enum,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = enum_type,
            .components = .{
                .Enum = .{
                    .name = enum_name,
                    .case_type = enum_case_type_node,
                    .slot = @intCast(slot),
                    .cases = cases_slice,
                },
            },
        },
    );

    // Generate the enum constant
    var @"enum" = try self.gc.allocateObject(
        obj.ObjEnum,
        obj.ObjEnum.init(enum_type),
    );

    var obj_cases = std.ArrayList(Value).init(self.gc.allocator);
    for (cases_slice, 0..) |case, idx| {
        if (case.value != null and !(try self.ast.slice().isConstant(self.gc.allocator, case.value.?))) {
            self.reportErrorAtNode(
                .enum_case,
                case.value.?,
                "Case value must be constant",
                .{},
            );

            continue;
        }

        try obj_cases.append(
            if (case.value) |case_value|
                try self.ast.slice().toValue(case_value, self.gc)
            else if (enum_type.def_type == .Integer)
                Value.fromInteger(@intCast(idx))
            else if (enum_type.def_type == .String)
                (try self.gc.copyString(self.ast.tokens.items(.lexeme)[case.name])).toValue()
            else
                unreachable,
        );
    }

    @"enum".cases = try obj_cases.toOwnedSlice();

    enum_type.resolved_type.?.Enum.value = @"enum";
    self.ast.nodes.items(.value)[node_slot] = @"enum".toValue();

    return @intCast(node_slot);
}

fn varDeclaration(
    self: *Self,
    identifier: ?Ast.TokenIndex,
    parsed_type: ?Ast.Node.Index,
    terminator: DeclarationTerminator,
    final: bool,
    should_assign: bool,
    type_provided_later: bool,
) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    var var_type = if (parsed_type) |ptype|
        try self.ast.nodes.items(.type_def)[ptype].?.toInstance(
            &self.gc.type_registry,
            self.ast.nodes.items(.type_def)[ptype].?.isMutable(), // ???
        )
    else
        self.gc.type_registry.any_type; // When var type omitted, will be replaced by the value type bellow

    const start_location = self.current_token.? - 1;

    const slot: usize = try self.parseVariable(
        @intCast(node_slot),
        identifier,
        var_type,
        final,
        var_type.isMutable(),
        "Expected variable name.",
    );

    const name = identifier orelse start_location;

    const value = if (should_assign and try self.match(.Equal))
        try self.expression(false)
    else
        null;
    const value_type_def = if (value) |val|
        self.ast.nodes.items(.type_def)[val]
    else
        null;

    if (should_assign and value == null and (parsed_type == null or !self.ast.nodes.items(.type_def)[parsed_type.?].?.optional)) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Expected variable initial value",
        );
    }

    if (var_type.def_type == .Placeholder and value != null and value_type_def != null and value_type_def.?.def_type == .Placeholder) {
        try obj.PlaceholderDef.link(
            self.gc.allocator,
            var_type,
            value_type_def.?,
            .Assignment,
        );
    }

    if (parsed_type == null and value != null and value_type_def != null) {
        var_type = value_type_def.?;

        if (self.current.?.scope_depth == 0) {
            self.globals.items[slot].type_def = var_type;
        } else {
            self.current.?.locals[slot].type_def = var_type;
        }
    } else if (parsed_type == null and !type_provided_later) {
        self.reporter.reportErrorAt(
            .inferred_type,
            self.ast.tokens.get(start_location),
            self.ast.tokens.get(self.current_token.? - 1),
            "Could not infer variable type.",
        );
    }

    if (value) |uvalue| {
        const tags = self.ast.nodes.items(.tag);
        const components = self.ast.nodes.items(.components);
        const parsed_type_def = if (parsed_type) |pt| self.ast.nodes.items(.type_def)[pt] else null;

        // var variable: [T] = [<any>] -> var variable: [T] = [<T>];
        if (parsed_type_def != null and
            parsed_type_def.?.def_type == .List and
            tags[uvalue] == .List and
            components[uvalue].List.explicit_item_type == null and
            components[uvalue].List.items.len == 0)
        {
            self.ast.nodes.items(.type_def)[uvalue] = parsed_type_def.?;
        }

        // var variable: {K: V} = {<any: any>} -> var variable: {K: V} = [<K: V>];
        if (parsed_type_def != null and
            parsed_type_def.?.def_type == .Map and
            tags[uvalue] == .Map and
            components[uvalue].Map.explicit_key_type == null and
            components[uvalue].Map.explicit_value_type == null and
            components[uvalue].Map.entries.len == 0)
        {
            self.ast.nodes.items(.type_def)[uvalue] = parsed_type_def.?;
        }
    }

    switch (terminator) {
        .OptComma => _ = try self.match(.Comma),
        .Comma => try self.consume(.Comma, "Expected `,` after variable declaration."),
        .Semicolon => try self.consume(.Semicolon, "Expected `;` after statement."),
        .Nothing => {},
    }

    self.markInitialized();

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .VarDeclaration,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = var_type,
            .components = .{
                .VarDeclaration = .{
                    .name = name,
                    .value = value,
                    .type = parsed_type,
                    .final = final,
                    .slot = @intCast(slot),
                    .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
                },
            },
        },
    );

    return @intCast(node_slot);
}

// Same as varDeclaration but does not parse anything (useful when we need to declare a variable that need to exists but is not exposed to the user)
fn implicitVarDeclaration(
    self: *Self,
    name: Ast.TokenIndex,
    parsed_type: *obj.ObjTypeDef,
    final: bool,
    mutable: bool,
) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const var_type = try parsed_type.toInstance(
        &self.gc.type_registry,
        mutable,
    );
    const slot = try self.declareVariable(
        @intCast(node_slot),
        var_type,
        name,
        final,
        mutable,
        true,
    );
    self.markInitialized();

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .VarDeclaration,
            .location = self.current_token.? - 1,
            .end_location = self.current_token.? - 1,
            .type_def = var_type,
            .components = .{
                .VarDeclaration = .{
                    .name = name,
                    .value = null,
                    .type = null,
                    .final = final,
                    .slot = @intCast(slot),
                    .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
                },
            },
        },
    );

    return @intCast(node_slot);
}

// `test` is just like a function but we don't parse arguments and we don't care about its return type
fn testStatement(self: *Self) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;
    // We can't consume the name because declareVariable will do it
    const name_token = self.current_token.?;

    var function_def_placeholder = obj.ObjTypeDef{
        .def_type = .Function,
    };

    self.test_count += 1;

    const slot = try self.declareVariable(
        @intCast(node_slot),
        &function_def_placeholder,
        name_token,
        true,
        false,
        true,
    );

    self.markInitialized();

    const function_node = try self.function(name_token, .Test, null);
    const function_type_def = self.ast.nodes.items(.type_def)[function_node];

    if (function_type_def) |type_def| {
        self.globals.items[slot].type_def = type_def;
    }

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .VarDeclaration,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = function_type_def,
            .components = .{
                .VarDeclaration = .{
                    .name = name_token,
                    .value = function_node,
                    .type = self.ast.nodes.items(.components)[function_node].Function.function_signature,
                    .final = true,
                    .slot = @intCast(slot),
                    .slot_type = .Global,
                },
            },
        },
    );

    return @intCast(node_slot);
}

fn searchPaths(self: *Self, file_name: []const u8) ![][]const u8 {
    var paths = std.ArrayList([]const u8).init(self.gc.allocator);

    for (search_paths) |path| {
        const filled = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            path,
            "?",
            file_name,
        );
        defer self.gc.allocator.free(filled);
        const suffixed = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            filled,
            "!",
            "buzz",
        );
        defer self.gc.allocator.free(suffixed);
        const prefixed = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            suffixed,
            "$",
            try buzzLibPath(self.gc.allocator),
        );

        if (builtin.os.tag == .windows) {
            const windows = try std.mem.replaceOwned(
                u8,
                self.gc.allocator,
                prefixed,
                "/",
                "\\",
            );
            try paths.append(windows);
        } else {
            try paths.append(prefixed);
        }
    }

    return try paths.toOwnedSlice();
}

fn searchLibPaths(self: *Self, file_name: []const u8) ![][]const u8 {
    var paths = std.ArrayList([]const u8).init(self.gc.allocator);

    for (lib_search_paths) |path| {
        const filled = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            path,
            "?",
            file_name,
        );
        defer self.gc.allocator.free(filled);
        const suffixed = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            filled,
            "!",
            switch (builtin.os.tag) {
                .linux, .freebsd, .openbsd => "so",
                .windows => "dll",
                .macos, .tvos, .watchos, .ios => "dylib",
                else => unreachable,
            },
        );
        defer self.gc.allocator.free(suffixed);
        const prefixed = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            suffixed,
            "$",
            try buzzLibPath(self.gc.allocator),
        );

        if (builtin.os.tag == .windows) {
            const windows = try std.mem.replaceOwned(
                u8,
                self.gc.allocator,
                prefixed,
                "/",
                "\\",
            );
            try paths.append(windows);
        } else {
            try paths.append(prefixed);
        }
    }

    for (user_library_paths orelse &[_][]const u8{}) |path| {
        var filled = std.ArrayList(u8).init(self.gc.allocator);

        try filled.writer().print(
            "{s}{s}{s}.{s}",
            .{
                path,
                if (builtin.os.tag == .windows and !std.mem.endsWith(u8, path, "\\"))
                    "\\"
                else if (!std.mem.endsWith(u8, path, "/"))
                    "/"
                else
                    "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        try paths.append(try filled.toOwnedSlice());

        var prefixed_filled = std.ArrayList(u8).init(self.gc.allocator);

        try prefixed_filled.writer().print(
            "{s}{s}lib{s}.{s}",
            .{
                path,
                if (builtin.os.tag == .windows and !std.mem.endsWith(u8, path, "\\"))
                    "\\"
                else if (!std.mem.endsWith(u8, path, "/"))
                    "/"
                else
                    "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        try paths.append(try prefixed_filled.toOwnedSlice());
    }

    return try paths.toOwnedSlice();
}

fn searchZdefLibPaths(self: *Self, file_name: []const u8) ![][]const u8 {
    var paths = std.ArrayList([]const u8).init(self.gc.allocator);

    for (zdef_search_paths) |path| {
        const filled = try std.mem.replaceOwned(u8, self.gc.allocator, path, "?", file_name);
        defer self.gc.allocator.free(filled);
        const suffixed = try std.mem.replaceOwned(
            u8,
            self.gc.allocator,
            filled,
            "!",
            switch (builtin.os.tag) {
                .linux, .freebsd, .openbsd => "so",
                .windows => "dll",
                .macos, .tvos, .watchos, .ios => "dylib",
                else => unreachable,
            },
        );

        if (builtin.os.tag == .windows) {
            const windows = try std.mem.replaceOwned(
                u8,
                self.gc.allocator,
                suffixed,
                "/",
                "\\",
            );
            try paths.append(windows);
        } else {
            try paths.append(suffixed);
        }
    }

    for (Self.user_library_paths orelse &[_][]const u8{}) |path| {
        var filled = std.ArrayList(u8).init(self.gc.allocator);

        try filled.writer().print(
            "{s}{s}{s}.{s}",
            .{
                path,
                if (builtin.os.tag == .windows and !std.mem.endsWith(u8, path, "\\"))
                    "\\"
                else if (!std.mem.endsWith(u8, path, "/"))
                    "/"
                else
                    "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        try paths.append(try filled.toOwnedSlice());

        var prefixed_filled = std.ArrayList(u8).init(self.gc.allocator);

        try prefixed_filled.writer().print(
            "{s}{s}lib{s}.{s}",
            .{
                path,
                if (builtin.os.tag == .windows and !std.mem.endsWith(u8, path, "\\"))
                    "\\"
                else if (!std.mem.endsWith(u8, path, "/"))
                    "/"
                else
                    "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        try paths.append(try prefixed_filled.toOwnedSlice());
    }

    return try paths.toOwnedSlice();
}

fn readStaticScript(self: *Self, file_name: []const u8) ?[2][]const u8 {
    // We can't build the file path dynamically
    return if (std.mem.eql(u8, file_name, "std"))
        [_][]const u8{
            @embedFile("lib/std.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "gc"))
        [_][]const u8{
            @embedFile("lib/gc.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "math"))
        [_][]const u8{
            @embedFile("lib/math.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "debug"))
        [_][]const u8{
            @embedFile("lib/debug.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "buffer"))
        [_][]const u8{
            @embedFile("lib/buffer.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "serialize"))
        [_][]const u8{
            @embedFile("lib/serialize.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "errors"))
        [_][]const u8{
            @embedFile("lib/errors.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "test"))
        [_][]const u8{
            @embedFile("lib/testing.buzz"),
            file_name,
        }
    else if (std.mem.eql(u8, file_name, "crypto"))
        [_][]const u8{
            @embedFile("lib/crypto.buzz"),
            file_name,
        }
    else none: {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorFmt(
            .script_not_found,
            location,
            location,
            "buzz script `{s}` not found",
            .{
                file_name,
            },
        );

        break :none null;
    };
}

fn readScript(self: *Self, file_name: []const u8) !?[2][]const u8 {
    const paths = try self.searchPaths(file_name);
    var selected_absolute_path_index: ?usize = null;
    defer {
        for (paths, 0..) |path, index| {
            if (selected_absolute_path_index != null and selected_absolute_path_index.? == index) {
                continue;
            }

            self.gc.allocator.free(path);
        }
        self.gc.allocator.free(paths);
    }

    // Find and read file
    var file: ?std.fs.File = null;
    var absolute_path: ?[]const u8 = null;
    for (paths, 0..) |path, index| {
        if (std.fs.path.isAbsolute(path)) {
            file = std.fs.openFileAbsolute(path, .{}) catch null;
            if (file != null) {
                selected_absolute_path_index = index;
                absolute_path = path;
                break;
            }
        } else {
            file = std.fs.cwd().openFile(path, .{}) catch null;
            if (file != null) {
                absolute_path = std.fs.cwd().realpathAlloc(self.gc.allocator, path) catch {
                    return Error.ImportError;
                };
                break;
            }
        }
    }

    if (file == null) {
        var search_report = std.ArrayList(u8).init(self.gc.allocator);
        defer search_report.deinit();
        var writer = search_report.writer();

        for (paths) |path| {
            try writer.print("    no file `{s}`\n", .{path});
        }

        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorFmt(
            .script_not_found,
            location,
            location,
            "buzz script `{s}` not found:\n{s}",
            .{
                file_name,
                search_report.items,
            },
        );

        return null;
    }

    defer file.?.close();

    const source = try self.gc.allocator.alloc(
        u8,
        (file.?.stat() catch {
            return Error.ImportError;
        }).size,
    );

    _ = file.?.readAll(source) catch {
        return Error.ImportError;
    };

    return [_][]const u8{
        source,
        absolute_path.?,
    };
}

fn importScript(
    self: *Self,
    path_token: Ast.TokenIndex,
    file_name: []const u8,
    prefix: ?[]const Ast.TokenIndex,
    imported_symbols: *std.StringHashMap(Ast.Node.Index),
) Error!?ScriptImport {
    var import = self.imports.get(file_name);

    if (import) |*uimport| {
        if (uimport.imported_by.get(self.current.?) != null) {
            const location = self.ast.tokens.get(path_token);
            self.reporter.reportErrorFmt(
                .import_already_exists,
                location,
                location,
                "`{s}` already imported",
                .{
                    location.literal_string.?,
                },
            );
        }

        try uimport.imported_by.put(
            self.gc.allocator,
            self.current.?,
            {},
        );
    } else {
        const source_and_path = if (is_wasm)
            self.readStaticScript(file_name)
        else
            try self.readScript(file_name);

        if (source_and_path == null) {
            return null;
        }

        var parser = Self.init(
            self.gc,
            self.imports,
            true,
            self.flavor,
        );
        defer parser.deinit();

        // We need to share the token and node list with the import so TokenIndex and Node.Index are usable
        parser.ast = self.ast;
        parser.current_token = self.current_token;
        const token_before_import = self.ast.tokens.get(self.current_token.?);
        const previous_root = self.ast.root;
        const previous_tokens_len = self.ast.tokens.len;
        const previous_node_len = self.ast.nodes.len;

        if (try parser.parse(source_and_path.?[0], source_and_path.?[1], file_name)) |ast| {
            self.ast = ast;
            self.ast.nodes.items(.components)[self.ast.root.?].Function.import_root = true;

            import = ScriptImport{
                .function = self.ast.root.?,
                .absolute_path = try self.gc.copyString(source_and_path.?[1]),
            };

            try import.?.imported_by.put(
                self.gc.allocator,
                self.current.?,
                {},
            );

            const lexemes = self.ast.tokens.items(.lexeme);

            for (parser.globals.items) |*global| {
                if (global.exported) {
                    global.exported = false;

                    if (global.export_alias) |export_alias| {
                        const previous_name = global.name;
                        var new_name = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
                        try new_name.appendSlice(
                            if (global.name.len > 1)
                                global.name[0 .. global.name.len - 1]
                            else
                                &[_]Ast.TokenIndex{},
                        );
                        try new_name.append(export_alias);
                        self.gc.allocator.free(previous_name); // TODO: will this free be an issue?

                        global.name = try new_name.toOwnedSlice();
                        global.export_alias = null;
                    }
                } else {
                    global.hidden = true;
                }

                try import.?.globals.append(self.gc.allocator, global.*);
                try import.?.global_names.put(
                    self.gc.allocator,
                    lexemes[self.current_token.? - 1],
                    @truncate(import.?.globals.items.len - 1),
                );
            }

            try self.imports.put(
                self.gc.allocator,
                file_name,
                import.?,
            );
            try self.script_imports.put(
                self.gc.allocator,
                file_name,
                .{
                    .location = path_token,
                    .end_location = path_token,
                },
            );

            // Caught up this parser with the import parser status
            self.ast.root = previous_root;
            // Last token of imported script is Eof, we discard it
            // Move scanned token just after import statement to replace the imported script Eof
            self.ast.tokens.set(parser.current_token.?, token_before_import);
            self.current_token = parser.current_token;
        } else {
            self.ast.root = previous_root;
            self.ast.nodes.shrinkRetainingCapacity(previous_node_len);
            self.ast.tokens.shrinkRetainingCapacity(previous_tokens_len);
            self.current_token = @intCast(previous_tokens_len);

            self.reporter.last_error = parser.reporter.last_error;
        }
    }

    if (import) |imported| {
        const selective_import = imported_symbols.count() > 0;
        const lexemes = self.ast.tokens.items(.lexeme);
        for (imported.globals.items) |imported_global| {
            var global = imported_global;

            if (!global.hidden) {
                if (imported_symbols.get(lexemes[global.name[global.name.len - 1]]) != null) {
                    _ = imported_symbols.remove(lexemes[global.name[global.name.len - 1]]);
                } else if (selective_import) {
                    global.hidden = true;
                }

                // If requalified, overwrite namespace
                var imported_name = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
                try imported_name.appendSlice(
                    if (prefix != null and prefix.?.len == 1 and std.mem.eql(u8, lexemes[prefix.?[0]], "_"))
                        &[_]Ast.TokenIndex{} // With a `_` override, we erase the namespace
                    else
                        prefix orelse global.name[0 .. global.name.len - 1], // override or take initial namespace
                );
                try imported_name.append(global.name[global.name.len - 1]);
                // self.gc.allocator.free(global.name);

                global.name = try imported_name.toOwnedSlice();
            }

            global.imported_from = file_name;

            // TODO: we're forced to import all and hide some because globals are indexed and not looked up by name at runtime
            //       Only way to avoid this is to go back to named globals at runtime. Then again, is it worth it?
            try self.globals.append(self.gc.allocator, global);
            try self.global_names.put(
                self.gc.allocator,
                lexemes[self.current_token.? - 1],
                @truncate(self.globals.items.len - 1),
            );
        }
    } else {
        // FIXME: here we must have messed up the token list because this crashes
        // self.reportErrorFmt(
        //     .compile,
        //     "Could not compile import or import external dynamic library `{s}`",
        //     .{
        //         file_name,
        //     },
        // );

        return Error.CantCompile;
    }

    return import;
}

// This is used in the wasm build. There, we only allow the import of std libs by name
fn importStaticLibSymbol(self: *Self, file_name: []const u8, symbol: []const u8) !?*obj.ObjNative {
    const symbol_ptr = if (libs.get(file_name)) |lib|
        lib.get(symbol)
    else
        null;

    if (symbol_ptr == null) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorFmt(
            .symbol_not_found,
            location,
            location,
            "Could not find symbol `{s}` in lib `{s}` (only std libraries can be imported from a WASM build)",
            .{
                symbol,
                file_name,
            },
        );
    }

    return if (symbol_ptr) |ptr|
        try self.gc.allocateObject(
            obj.ObjNative,
            .{
                .native = @ptrFromInt(@intFromPtr(ptr)),
            },
        )
    else
        null;
}

// TODO: when to close the lib?
fn importLibSymbol(self: *Self, location: Ast.TokenIndex, end_location: Ast.TokenIndex, full_file_name: []const u8, symbol: []const u8) !?*obj.ObjNative {
    // Remove .buzz extension, this occurs if this is the script being run or if the script was imported like so `import lib/std.buzz`
    // We consider that any other extension is silly from the part of the user
    const file_name = if (std.mem.endsWith(u8, full_file_name, ".buzz"))
        full_file_name[0..(full_file_name.len - 5)]
    else
        full_file_name;

    const file_basename = std.fs.path.basename(file_name);
    const paths = try self.searchLibPaths(file_basename);
    defer {
        for (paths) |path| {
            self.gc.allocator.free(path);
        }
        self.gc.allocator.free(paths);
    }

    var lib: ?std.DynLib = null;
    for (paths) |path| {
        lib = std.DynLib.open(path) catch null;
        if (lib != null) {
            break;
        }
    }

    if (lib) |*dlib| {
        // Convert symbol names to zig slices
        const ssymbol = try self.gc.allocator.dupeZ(u8, symbol);
        defer self.gc.allocator.free(ssymbol);

        // Lookup symbol NativeFn
        const opaque_symbol_method = dlib.lookup(*anyopaque, ssymbol);

        if (opaque_symbol_method == null) {
            self.reporter.reportErrorFmt(
                .symbol_not_found,
                self.ast.tokens.get(location),
                self.ast.tokens.get(end_location),
                "Could not find symbol `{s}` in lib `{s}`",
                .{
                    symbol,
                    file_name,
                },
            );
            return null;
        }

        // Create a ObjNative with it
        return try self.gc.allocateObject(
            obj.ObjNative,
            .{
                .native = opaque_symbol_method.?,
            },
        );
    }

    var search_report = std.ArrayList(u8).init(self.gc.allocator);
    defer search_report.deinit();
    var writer = search_report.writer();

    for (paths) |path| {
        try writer.print("    no file `{s}`\n", .{path});
    }

    self.reporter.reportErrorFmt(
        .library_not_found,
        self.ast.tokens.get(location),
        self.ast.tokens.get(end_location),
        "External library `{s}` not found: {s}{s}\n",
        .{
            file_basename,
            if (builtin.link_libc and builtin.os.tag != .windows)
                std.mem.sliceTo(dlerror(), 0)
            else
                "",
            search_report.items,
        },
    );

    return null;
}

fn importStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var imported_symbols = std.StringHashMap(Ast.TokenIndex).init(self.gc.allocator);
    var symbols = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);

    while ((try self.match(.Identifier)) and !self.check(.Eof)) {
        const symbol = self.ast.tokens.items(.lexeme)[self.current_token.? - 1];

        if (imported_symbols.get(symbol)) |loc| {
            self.reporter.reportWithOrigin(
                .import_already_exists,
                self.ast.tokens.get(start_location),
                self.ast.tokens.get(self.current_token.? - 1),
                self.ast.tokens.get(loc),
                self.ast.tokens.get(loc),
                "Import symbol `{s}` already imported",
                .{symbol},
                "Imported here",
            );
        }
        try imported_symbols.put(self.ast.tokens.items(.lexeme)[self.current_token.? - 1], self.current_token.? - 1);
        try symbols.append(self.current_token.? - 1);

        if (!self.check(.From) or self.check(.Comma)) { // Allow trailing comma
            try self.consume(.Comma, "Expected `,` after identifier.");
        }
    }

    var prefix: ?[]const Ast.TokenIndex = null;
    if (imported_symbols.count() > 0) {
        try self.consume(.From, "Expected `from` after import identifier list.");
    }

    try self.consume(.String, "Expected import path.");

    const path_token = self.current_token.? - 1;
    const path = self.ast.tokens.get(self.current_token.? - 1);
    if (path.lexeme.len <= 1 or path.literal_string.?.len <= 0) {
        self.reporter.reportErrorAt(
            .empty_import,
            path,
            path,
            "Import path can't be empty",
        );
    }
    const file_name: []const u8 = if (path.lexeme.len <= 1 or path.literal_string.?.len <= 0) invalid: {
        self.reporter.reportErrorAt(
            .empty_import,
            path,
            path,
            "Import path can't be empty",
        );

        break :invalid "invalid";
    } else path.lexeme[1..(path.lexeme.len - 1)];

    if (imported_symbols.count() == 0 and try self.match(.As)) {
        try self.consume(.Identifier, "Expected identifier after `as`.");
        prefix = try self.qualifiedName();
    }

    try self.consume(.Semicolon, "Expected `;` after statement.");

    const import = if (self.reporter.last_error == null)
        try self.importScript(
            path_token,
            file_name,
            prefix,
            &imported_symbols,
        )
    else
        null;

    if (imported_symbols.count() > 0) {
        var it = imported_symbols.iterator();
        while (it.next()) |kv| {
            const location = self.ast.tokens.get(kv.value_ptr.*);
            self.reporter.reportErrorFmt(
                .unknown_import,
                location,
                location,
                "Unknown import `{s}`.",
                .{kv.key_ptr.*},
            );
        }
    }

    return try self.ast.appendNode(
        .{
            .tag = .Import,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Import = .{
                    .prefix = prefix,
                    .imported_symbols = try symbols.toOwnedSlice(),
                    .path = path_token,
                    .import = import,
                },
            },
        },
    );
}

fn zdefStatement(self: *Self) Error!Ast.Node.Index {
    if (!BuildOptions.jit and BuildOptions.cycle_limit == null) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .zdef,
            location,
            location,
            "zdef can't be used, this instance of buzz was built with JIT compiler disabled",
        );
    }

    if (is_wasm) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .zdef,
            location,
            location,
            "zdef is not available in WASM build",
        );
    }

    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `zdef`.");
    try self.consume(.String, "Expected identifier after `zdef(`.");
    const lib_name = self.current_token.? - 1;
    try self.consume(.Comma, "Expected `,` after lib name.");
    try self.consume(.String, "Expected zdef declaration.");
    const source = self.current_token.? - 1;
    try self.consume(.RightParen, "Expected `)` to close zdef");
    try self.consume(.Semicolon, "Expected `;`");

    const zdefs = if (!is_wasm)
        (self.ffi.parse(
            self,
            self.ast.tokens.get(source),
            false,
        ) catch {
            return Error.CantCompile;
        }) orelse &[_]*FFI.Zdef{}
    else
        &[_]*FFI.Zdef{};

    var elements = std.ArrayList(Ast.Zdef.ZdefElement).init(self.gc.allocator);
    if (!is_wasm) {
        for (zdefs) |zdef| {
            var fn_ptr: ?*anyopaque = null;
            var slot: usize = undefined;

            std.debug.assert(self.current.?.scope_depth == 0);
            const zdef_name_token = try self.insertUtilityToken(Token.identifier(zdef.name));
            slot = try self.declareVariable(
                @intCast(node_slot),
                zdef.type_def,
                zdef_name_token,
                true,
                false,
                true,
            );
            // self.current_token.? - 1 = zdef_name_token;
            self.markInitialized();

            const lib_name_str = self.ast.tokens.items(.literal_string)[lib_name].?;

            // If zig_type is struct, we just push the objtypedef itself on the stack
            // Otherwise we try to build a wrapper around the imported function
            if (zdef.zig_type == .Fn) {
                // Load the lib
                const paths = try self.searchZdefLibPaths(lib_name_str);
                defer {
                    for (paths) |path| {
                        self.gc.allocator.free(path);
                    }
                    self.gc.allocator.free(paths);
                }

                var lib: ?std.DynLib = null;
                for (paths) |path| {
                    lib = std.DynLib.open(path) catch null;
                    if (lib != null) {
                        break;
                    }
                }

                if (lib) |*dlib| {
                    // Convert symbol names to zig slices
                    const symbol = try self.gc.allocator.dupeZ(u8, zdef.name);
                    defer self.gc.allocator.free(symbol);

                    // Lookup symbol
                    const opaque_symbol_method = dlib.lookup(*anyopaque, symbol);

                    if (opaque_symbol_method == null) {
                        const location = self.ast.tokens.get(lib_name);
                        self.reporter.reportErrorFmt(
                            .symbol_not_found,
                            location,
                            location,
                            "Could not find symbol `{s}` in lib `{s}`",
                            .{
                                symbol,
                                lib_name_str,
                            },
                        );
                    }

                    fn_ptr = opaque_symbol_method;
                } else {
                    var search_report = std.ArrayList(u8).init(self.gc.allocator);
                    defer search_report.deinit();
                    var writer = search_report.writer();

                    for (paths) |path| {
                        try writer.print("    no file `{s}`\n", .{path});
                    }

                    const location = self.ast.tokens.get(lib_name);
                    self.reporter.reportErrorFmt(
                        .library_not_found,
                        location,
                        location,
                        "External library `{s}` not found: {s}{s}\n",
                        .{
                            lib_name_str,
                            if (builtin.link_libc and builtin.os.tag != .windows)
                                std.mem.sliceTo(dlerror(), 0)
                            else
                                "",
                            search_report.items,
                        },
                    );
                }
            }

            try elements.append(
                .{
                    .fn_ptr = fn_ptr,
                    .slot = @intCast(slot),
                    .zdef = zdef,
                    .identifier = self.current_token.? - 1,
                },
            );
        }
    }

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .Zdef,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Zdef = .{
                    .lib_name = lib_name,
                    .source = source,
                    .elements = try elements.toOwnedSlice(),
                },
            },
        },
    );

    return @intCast(node_slot);
}

// FIXME: this is almost the same as parseUserType!
fn userVarDeclaration(self: *Self, identifier: Ast.TokenIndex, final: bool, mutable: bool) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;
    var var_type: ?*obj.ObjTypeDef = null;

    var generic_resolve: ?Ast.Node.Index = null;

    // If next token is `=`, means the identifier wasn't a user type but the variable name
    // and the type needs to be inferred
    const user_type_name = try self.qualifiedName();

    // Is it a generic type defined in enclosing functions or object?
    if (self.resolveGeneric(try self.gc.copyString(self.ast.tokens.items(.lexeme)[self.current_token.? - 1]))) |generic_type| {
        var_type = generic_type;
    } else if (self.current.?.generics != null) {
        // Is it generic type defined in a function signature being parsed?
        if (self.current.?.generics.?.get(try self.gc.copyString(self.ast.tokens.items(.lexeme)[self.current_token.? - 1]))) |generic_type| {
            var_type = generic_type;
        }
    }

    // Search for a global with that name
    if (var_type == null) {
        if (try self.resolveGlobal(@intCast(node_slot), user_type_name)) |slot| {
            const global = self.globals.items[slot];

            var_type = global.type_def;

            if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
                const imported_from = global.imported_from.?;

                try self.script_imports.put(
                    self.gc.allocator,
                    imported_from,
                    .{
                        .location = self.script_imports.get(imported_from).?.location,
                        .end_location = self.script_imports.get(imported_from).?.end_location,
                        .referenced = true,
                    },
                );
            }
        }
    }

    // If none found, create a placeholder
    if (var_type == null) {
        var_type = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = .{
                    // TODO: token is wrong but what else can we put here?
                    .Placeholder = obj.PlaceholderDef.init(
                        user_type_name[0],
                        user_type_name[user_type_name.len - 1],
                        mutable,
                    ),
                },
            },
        );

        _ = try self.declarePlaceholder(
            user_type_name[user_type_name.len - 1],
            var_type,
        );
    }

    // Concrete generic types list
    generic_resolve = if (try self.match(.DoubleColon)) gn: {
        const generic_start = self.current_token.? - 1;

        try self.consume(.Less, "Expected generic types list after `::`");

        var resolved_generics = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
        var generic_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
        var i: usize = 0;
        while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
            try generic_nodes.append(
                try self.parseTypeDef(
                    if (self.current.?.generics) |generics|
                        generics.*
                    else
                        null,
                    true,
                ),
            );

            try resolved_generics.append(
                self.ast.nodes.items(.type_def)[generic_nodes.items[generic_nodes.items.len - 1]].?,
            );

            if (!self.check(.Greater)) {
                try self.consume(.Comma, "Expected `,` between generic types");
            }
        }

        try self.consume(.Greater, "Expected `>` after generic types list");

        if (resolved_generics.items.len == 0) {
            self.reporter.reportErrorAt(
                .generic_type,
                self.ast.tokens.get(generic_start),
                self.ast.tokens.get(self.current_token.? - 1),
                "Expected at least one type",
            );
        }

        // Shouldn't we populate only in codegen?
        var_type = try var_type.?.populateGenerics(
            self.current_token.? - 1,
            var_type.?.resolved_type.?.Object.id,
            try resolved_generics.toOwnedSlice(),
            &self.gc.type_registry,
            null,
        );

        break :gn try self.ast.appendNode(
            .{
                .tag = .GenericResolveType,
                .location = generic_start,
                .end_location = self.current_token.? - 1,
                .type_def = var_type,
                .components = .{
                    .GenericResolveType = try generic_nodes.toOwnedSlice(),
                },
            },
        );
    } else null;

    var_type = try var_type.?.toInstance(
        &self.gc.type_registry,
        mutable,
    );

    if (try self.match(.Question)) {
        var_type = try var_type.?.cloneOptional(&self.gc.type_registry);
    }

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .UserType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = var_type,
            .components = .{
                .UserType = .{
                    .name = user_type_name,
                    .generic_resolve = generic_resolve,
                },
            },
        },
    );

    return try self.varDeclaration(
        identifier,
        @intCast(node_slot),
        .Semicolon,
        final,
        true,
        false,
    );
}

fn forStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `for`.");

    try self.beginScope(null);

    // Should be either VarDeclaration or expression
    var init_declarations = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    while (!self.check(.Semicolon) and !self.check(.Eof)) {
        try self.consume(.Identifier, "Expected identifier");
        const identifier = self.current_token.? - 1;

        try init_declarations.append(
            try self.varDeclaration(
                identifier,
                if (try self.match(.Colon))
                    try self.parseTypeDef(null, true)
                else
                    null,
                .Nothing,
                false,
                true,
                false,
            ),
        );

        self.markInitialized();

        if (!self.check(.Semicolon)) {
            try self.consume(.Comma, "Expected `,` after for loop variable");
        }
    }

    try self.consume(.Semicolon, "Expected `;` after for loop variables.");

    const condition = try self.expression(false);

    try self.consume(.Semicolon, "Expected `;` after for loop condition.");

    var post_loop = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    while (!self.check(.RightParen) and !self.check(.Eof)) {
        try post_loop.append(try self.expression(false));

        if (!self.check(.RightParen)) {
            try self.consume(.Comma, "Expected `,` after for loop expression");
        }
    }

    try self.consume(.RightParen, "Expected `)` after `for` expressions.");

    const label = if (try self.match(.Colon)) lbl: {
        try self.consume(.Identifier, "Expected label after `:`.");

        break :lbl self.current_token.? - 1;
    } else null;

    try self.consume(.LeftBrace, "Expected `{`.");

    // We add it before parsing the body so that we can find it on a labeled break/continue statement
    const for_node = try self.ast.appendNode(
        .{
            .tag = .For,
            .location = start_location,
            .end_location = undefined,
            .components = .{
                .For = .{
                    .init_declarations = try init_declarations.toOwnedSlice(),
                    .condition = condition,
                    .post_loop = try post_loop.toOwnedSlice(),
                    .body = undefined,
                    .label = label,
                },
            },
        },
    );

    try self.beginScope(for_node);
    const body = try self.block(
        .{
            .loop_type = .For,
            .loop_body_scope = self.current.?.scope_depth,
        },
    );
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    self.ast.nodes.items(.end_location)[for_node] = self.current_token.? - 1;
    self.ast.nodes.items(.components)[for_node].For.body = body;
    self.ast.nodes.items(.ends_scope)[for_node] = try self.endScope();

    return for_node;
}

fn forEachStatement(self: *Self) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `foreach`.");

    try self.beginScope(null);

    try self.consume(.Identifier, "Expected identifier");
    const key_identifier = self.current_token.? - 1;
    var key = try self.varDeclaration(
        key_identifier,
        null,
        .Nothing,
        false,
        false,
        true,
    );

    const key_omitted = !(try self.match(.Comma));
    if (!key_omitted) {
        try self.consume(.Identifier, "Expected identifier");
    }
    const value_identifier = self.current_token.? - 1;
    var value = if (!key_omitted)
        try self.varDeclaration(
            value_identifier,
            null,
            .Nothing,
            false,
            false,
            true,
        )
    else
        null;

    // If key is omitted, prepare the slot anyway
    if (key_omitted) {
        value = key;

        key = try self.implicitVarDeclaration(
            try self.insertUtilityToken(Token.identifier("$key")),
            self.gc.type_registry.void_type,
            false,
            false,
        );

        // Switch slots so that key is before value
        const kv_components = self.ast.nodes.items(.components);
        const value_slot = kv_components[key].VarDeclaration.slot;
        const key_slot = kv_components[value.?].VarDeclaration.slot;
        const value_components = kv_components[key];
        const key_components = kv_components[value.?];
        kv_components[key] = key_components;
        kv_components[value.?] = value_components;

        const value_local = self.current.?.locals[key_slot];
        self.current.?.locals[key_slot] = self.current.?.locals[value_slot];
        self.current.?.locals[value_slot] = value_local;
    }

    try self.consume(.In, "Expected `in` after `foreach` variables.");

    // Local not usable by user but needed so that locals are correct
    const iterable_slot = try self.addLocal(
        @intCast(node_slot),
        try self.insertUtilityToken(Token.identifier("$iterable")),
        undefined,
        true,
        false,
    );

    const iterable = try self.expression(false);
    const iterable_type_def = self.ast.nodes.items(.type_def)[iterable].?;

    self.current.?.locals[iterable_slot].type_def = iterable_type_def;

    // Infer key/value type
    const key_type = switch (iterable_type_def.def_type) {
        .List, .String => self.gc.type_registry.int_type,
        .Map => iterable_type_def.resolved_type.?.Map.key_type,
        .Range => self.gc.type_registry.int_type,
        .Placeholder => placeholder: {
            const placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = .{
                        .Placeholder = obj.PlaceholderDef.init(
                            self.ast.nodes.items(.location)[if (key_omitted) iterable else key],
                            self.ast.nodes.items(.end_location)[if (key_omitted) iterable else key],
                            null,
                        ),
                    },
                },
            );

            try obj.PlaceholderDef.link(
                self.gc.allocator,
                iterable_type_def,
                placeholder,
                .Key,
            );

            break :placeholder placeholder;
        },
        else =>
        // Other type don't have key type
        self.current.?.locals[self.ast.nodes.items(.components)[key].VarDeclaration.slot].type_def,
    };

    self.current.?.locals[self.ast.nodes.items(.components)[key].VarDeclaration.slot].type_def = key_type;
    self.ast.nodes.items(.type_def)[key] = key_type;

    const value_type = switch (iterable_type_def.def_type) {
        .List => iterable_type_def.resolved_type.?.List.item_type,
        .Range => self.gc.type_registry.int_type,
        .String => self.gc.type_registry.str_type,
        .Map => iterable_type_def.resolved_type.?.Map.value_type,
        .Enum => try iterable_type_def.toInstance(&self.gc.type_registry, false),
        .Fiber => iterable_type_def.resolved_type.?.Fiber.yield_type,
        .Placeholder => placeholder: {
            const placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = .{
                        .Placeholder = obj.PlaceholderDef.init(
                            self.ast.nodes.items(.location)[value.?],
                            self.ast.nodes.items(.end_location)[value.?],
                            null,
                        ),
                    },
                },
            );

            try obj.PlaceholderDef.link(
                self.gc.allocator,
                iterable_type_def,
                placeholder,
                .UnwrappedSubscript,
            );

            break :placeholder placeholder;
        },
        else =>
        // Other type are invalid and will be caught at codegen
        self.current.?.locals[self.ast.nodes.items(.components)[value.?].VarDeclaration.slot].type_def,
    };

    self.current.?.locals[self.ast.nodes.items(.components)[value.?].VarDeclaration.slot].type_def = value_type;
    self.ast.nodes.items(.type_def)[value.?] = value_type;

    self.markInitialized();

    try self.consume(.RightParen, "Expected `)` after `foreach`.");

    const label = if (try self.match(.Colon)) lbl: {
        try self.consume(.Identifier, "Expected label after `:`.");

        break :lbl self.current_token.? - 1;
    } else null;

    try self.consume(.LeftBrace, "Expected `{`.");

    // We add it before parsing the body so that we can find it on a labeled break/continue statement
    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .ForEach,
            .location = start_location,
            .end_location = undefined,
            .components = .{
                .ForEach = .{
                    .key = key,
                    .value = value.?,
                    .iterable = iterable,
                    .body = undefined,
                    .key_omitted = key_omitted,
                    .label = label,
                },
            },
        },
    );

    try self.beginScope(@intCast(node_slot));
    const body = try self.block(
        .{
            .loop_type = .ForEach,
            .loop_body_scope = self.current.?.scope_depth,
        },
    );
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    self.ast.nodes.items(.end_location)[node_slot] = self.current_token.? - 1;
    self.ast.nodes.items(.components)[node_slot].ForEach.body = body;
    self.ast.nodes.items(.ends_scope)[node_slot] = try self.endScope();

    return @intCast(node_slot);
}

fn whileStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `while`.");

    const condition = try self.expression(false);

    try self.consume(.RightParen, "Expected `)` after `while` condition.");

    const label = if (try self.match(.Colon)) lbl: {
        try self.consume(.Identifier, "Expected label after `:`.");

        break :lbl self.current_token.? - 1;
    } else null;

    try self.consume(.LeftBrace, "Expected `{`.");

    // We add it before parsing the body so that we can find it on a labeled break/continue statement
    const while_node = try self.ast.appendNode(
        .{
            .tag = .While,
            .location = start_location,
            .end_location = undefined,
            .components = .{
                .While = .{
                    .condition = condition,
                    .body = undefined,
                    .label = label,
                },
            },
        },
    );

    try self.beginScope(while_node);
    const body = try self.block(
        .{
            .loop_type = .While,
            .loop_body_scope = self.current.?.scope_depth,
        },
    );
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    self.ast.nodes.items(.end_location)[while_node] = self.current_token.? - 1;
    self.ast.nodes.items(.components)[while_node].While.body = body;

    return while_node;
}

fn doUntilStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const label = if (try self.match(.Colon)) lbl: {
        try self.consume(.Identifier, "Expected label after `:`.");

        break :lbl self.current_token.? - 1;
    } else null;

    try self.consume(.LeftBrace, "Expected `{`.");

    // We add it before parsing the body so that we can find it on a labeled break/continue statement
    const dountil_node = try self.ast.appendNode(
        .{
            .tag = .DoUntil,
            .location = start_location,
            .end_location = undefined,
            .components = .{
                .DoUntil = .{
                    .condition = undefined,
                    .body = undefined,
                    .label = label,
                },
            },
        },
    );

    try self.beginScope(null);
    const body = try self.block(
        .{
            .loop_type = .Do,
            .loop_body_scope = self.current.?.scope_depth,
        },
    );
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    try self.consume(.Until, "Expected `until` after `do` block.");

    try self.consume(.LeftParen, "Expected `(` after `until`.");

    const condition = try self.expression(false);

    try self.consume(.RightParen, "Expected `)` after `until` condition.");

    self.ast.nodes.items(.end_location)[dountil_node] = self.current_token.? - 1;
    self.ast.nodes.items(.components)[dountil_node].DoUntil.condition = condition;
    self.ast.nodes.items(.components)[dountil_node].DoUntil.body = body;

    return dountil_node;
}

fn returnStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth == 0) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "Can't use `return` at top-level.",
        );
    }

    const value = if (!try self.match(.Semicolon))
        try self.expression(false)
    else
        null;

    if (value) |uvalue| {
        try self.consume(.Semicolon, "Expected `;` after statement.");

        if (self.ast.nodes.items(.tag)[uvalue] == .Call) {
            self.ast.nodes.items(.components)[uvalue].Call.tail_call = true;
        } else if (self.ast.nodes.items(.tag)[uvalue] == .Dot and self.ast.nodes.items(.components)[uvalue].Dot.member_kind == .Call) {
            self.ast.nodes.items(.components)[self.ast.nodes.items(.components)[uvalue].Dot.value_or_call_or_enum.Call].Call.tail_call = true;
        }
    }

    return try self.ast.appendNode(
        .{
            .tag = .Return,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Return = .{
                    .value = value,
                    .unconditional = self.current.?.scope_depth == 1,
                },
            },
        },
    );
}

fn outStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.in_block_expression == null) {
        const location = self.ast.tokens.get(start_location);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "`out` statement is only allowed inside a block expression",
        );
    } else if (self.current.?.scope_depth != self.current.?.in_block_expression.?) {
        const location = self.ast.tokens.get(start_location);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "`out` statement must be the last statement of a block expression",
        );
    }

    const expr = try self.expression(false);

    try self.consume(.Semicolon, "Expected `;` after statement.");

    return try self.ast.appendNode(
        .{
            .tag = .Out,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = self.ast.nodes.items(.type_def)[expr],
            .components = .{
                .Out = expr,
            },
        },
    );
}

fn throw(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    // For now we don't care about the type. Later if we have `Error` type of data, we'll type check this
    const error_value = try self.expression(false);

    try self.consume(.Semicolon, "Expected `;` after statement.");

    return try self.ast.appendNode(
        .{
            .tag = .Throw,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Throw = .{
                    .expression = error_value,
                    .unconditional = self.current.?.scope_depth == 1,
                },
            },
        },
    );
}

fn namespaceStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    // Should be the first statement
    const components = self.ast.nodes.items(.components);
    const current_body = components[self.current.?.function_node].Function.body;
    if (current_body == null or components[current_body.?].Block.len > 0) {
        self.reporter.reportErrorAt(
            .syntax,
            self.ast.tokens.get(if (current_body) |body| self.ast.nodes.items(.location)[body] else start_location),
            self.ast.tokens.get(if (current_body) |body| self.ast.nodes.items(.end_location)[body] else start_location),
            "`namespace` should be the first statement",
        );
    }

    var namespace = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
    while (!self.check(.Semicolon) and !self.check(.Eof)) {
        try self.consume(.Identifier, "Expected namespace identifier");

        try namespace.append(self.current_token.? - 1);

        if (!try self.match(.AntiSlash)) {
            break;
        }
    }

    self.namespace = try namespace.toOwnedSlice();

    try self.consume(.Semicolon, "Expected `;` after statement.");

    return try self.ast.appendNode(
        .{
            .tag = .Namespace,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Namespace = namespace.items,
            },
        },
    );
}

fn tryStatement(self: *Self) Error!Ast.Node.Index {
    const node_slot = try self.ast.nodes.addOne(self.gc.allocator);
    const start_location = self.current_token.? - 1;

    if (self.current.?.in_try) {
        const location = self.ast.tokens.get(self.current_token.? - 1);
        self.reporter.reportErrorAt(
            .nested_try,
            location,
            location,
            "Nested `try` statement are not allowed",
        );
    }

    self.current.?.in_try = true;

    try self.consume(.LeftBrace, "Expected `{` after `try`");

    try self.beginScope(null);
    const body = try self.block(null);
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    var clauses = std.ArrayList(Ast.Try.Clause).init(self.gc.allocator);
    var unconditional_clause: ?Ast.Node.Index = null;
    // either catch with no type of catch any
    while (try self.match(.Catch)) {
        if (try self.match(.LeftParen)) {
            if (unconditional_clause != null) {
                const location = self.ast.tokens.get(self.current_token.? - 1);
                self.reporter.reportErrorAt(
                    .syntax,
                    location,
                    location,
                    "Catch clause not allowed after unconditional catch",
                );
            }

            try self.beginScope(null);

            try self.consume(.Identifier, "Expected identifier");
            const identifier = self.current_token.? - 1;
            try self.consume(.Colon, "Expected `:`");
            const type_def = try self.parseTypeDef(null, true);

            _ = try self.parseVariable(
                @intCast(node_slot),
                identifier,
                self.ast.nodes.items(.type_def)[type_def].?,
                true, // function arguments are final
                false,
                "Expected error identifier",
            );

            self.markInitialized();

            try self.consume(.RightParen, "Expected `)` after error identifier");
            try self.consume(.LeftBrace, "Expected `{`");

            const catch_block = try self.block(null);
            self.ast.nodes.items(.ends_scope)[catch_block] = try self.endScope();

            try clauses.append(
                .{
                    .identifier = identifier,
                    .type_def = type_def,
                    .body = catch_block,
                },
            );
        } else if (unconditional_clause == null) {
            try self.consume(.LeftBrace, "Expected `{` after `catch`");

            try self.beginScope(null);
            unconditional_clause = try self.block(null);
            self.ast.nodes.items(.ends_scope)[unconditional_clause.?] = try self.endScope();
        } else {
            const location = self.ast.tokens.get(self.current_token.? - 1);
            self.reporter.reportErrorAt(
                .syntax,
                location,
                location,
                "Expected `(` after `catch`",
            );
        }
    }

    self.current.?.in_try = false;

    self.ast.nodes.set(
        node_slot,
        .{
            .tag = .Try,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Try = .{
                    .body = body,
                    .clauses = try clauses.toOwnedSlice(),
                    .unconditional_clause = unconditional_clause,
                },
            },
        },
    );

    return @intCast(node_slot);
}

// Go up scopes until it finds a loop node with a matching label
fn findLabel(self: *Self, label: Ast.TokenIndex) ?struct { node: Ast.Node.Index, depth: u32 } {
    const tags = self.ast.nodes.items(.tag);
    const components = self.ast.nodes.items(.components);
    const lexemes = self.ast.tokens.items(.lexeme);

    var depth = self.current.?.scope_depth - 1;
    while (depth >= 0) : (depth -= 1) {
        if (self.current.?.scopes.items[depth]) |scope_node| {
            switch (tags[scope_node]) {
                .For => {
                    if (components[scope_node].For.label) |scope_label| {
                        if (std.mem.eql(u8, lexemes[scope_label], lexemes[label])) {
                            return .{
                                .node = scope_node,
                                .depth = depth,
                            };
                        }
                    }
                },
                .ForEach => {
                    if (components[scope_node].ForEach.label) |scope_label| {
                        if (std.mem.eql(u8, lexemes[scope_label], lexemes[label])) {
                            return .{
                                .node = scope_node,
                                .depth = depth,
                            };
                        }
                    }
                },
                .While => {
                    if (components[scope_node].While.label) |scope_label| {
                        if (std.mem.eql(u8, lexemes[scope_label], lexemes[label])) {
                            return .{
                                .node = scope_node,
                                .depth = depth,
                            };
                        }
                    }
                },
                .DoUntil => {
                    if (components[scope_node].DoUntil.label) |scope_label| {
                        if (std.mem.eql(u8, lexemes[scope_label], lexemes[label])) {
                            return .{
                                .node = scope_node,
                                .depth = depth,
                            };
                        }
                    }
                },
                else => {},
            }
        }

        if (depth == 0) {
            break;
        }
    }

    return null;
}

fn breakContinueStatement(self: *Self, @"break": bool, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const label = if (try self.match(.Identifier))
        self.current_token.? - 1
    else
        null;

    const label_scope = if (label) |lbl|
        self.findLabel(lbl)
    else
        null;

    if (label != null and label_scope == null) {
        const location = self.ast.tokens.get(label.?);
        self.reporter.reportErrorFmt(
            .label_does_not_exists,
            location,
            location,
            "Label `{s}` does not exists.",
            .{
                location.lexeme,
            },
        );
    }

    if (label == null and loop_scope == null) {
        const location = self.ast.tokens.get(start_location);
        self.reporter.reportErrorAt(
            .syntax,
            location,
            location,
            "break is not allowed here.",
        );
    }

    try self.consume(.Semicolon, "Expected `;` after statement.");

    return try self.ast.appendNode(
        .{
            .tag = if (@"break") .Break else .Continue,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .ends_scope = if (loop_scope != null or label_scope != null)
                try self.closeScope(
                    if (label_scope) |scope|
                        scope.depth + 1
                    else
                        loop_scope.?.loop_body_scope,
                )
            else
                null,
            .components = if (@"break")
                .{
                    .Break = if (label_scope) |scope|
                        scope.node
                    else
                        null,
                }
            else
                .{
                    .Continue = if (label_scope) |scope|
                        scope.node
                    else
                        null,
                },
        },
    );
}

fn continueStatement(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    return self.breakContinueStatement(false, loop_scope);
}

fn breakStatement(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    return self.breakContinueStatement(true, loop_scope);
}
