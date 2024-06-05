const std = @import("std");
const builtin = @import("builtin");
const is_wasm = builtin.cpu.arch.isWasm();
const BuildOptions = @import("build_options");
const obj = @import("obj.zig");
const Token = @import("Token.zig");
const Chunk = @import("Chunk.zig");
const Value = @import("value.zig").Value;
const FFI = @import("FFI.zig");
const Ast = @import("Ast.zig");
const GarbageCollector = @import("memory.zig").GarbageCollector;
const Scanner = @import("Scanner.zig");
const RunFlavor = @import("vm.zig").RunFlavor;
const Reporter = @import("Reporter.zig");
const StringParser = @import("string_parser.zig").StringParser;
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
        .{ "parseFloat", &std_lib.parseFloat },
        .{ "parseInt", &std_lib.parseInt },
        .{ "parseUd", &std_lib.parseUd },
        .{ "print", &std_lib.print },
        .{ "random", &std_lib.random },
        .{ "toFloat", &std_lib.toFloat },
        .{ "toInt", &std_lib.toInt },
        .{ "toUd", &std_lib.toUd },
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
        .{ "minFloat", &math_lib.minFloat },
        .{ "maxFloat", &math_lib.maxFloat },
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
        .{ "BufferWriteFloat", &buffer_lib.BufferWriteFloat },
        .{ "BufferReadFloat", &buffer_lib.BufferReadFloat },
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

// TODO: other libs

const Self = @This();

extern fn dlerror() [*:0]u8;

pub fn defaultBuzzPrefix() []const u8 {
    return ".";
}

var _buzz_path_buffer: [4096]u8 = undefined;
pub fn buzzPrefix() []const u8 {
    // FIXME: don't use std.posix directly
    if (std.posix.getenv("BUZZ_PATH")) |buzz_path| {
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
pub fn buzzLibPath() ![]const u8 {
    const path2 = buzzPrefix();
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
    name: *obj.ObjString,
    name_token: Ast.TokenIndex,
    location: Ast.TokenIndex,
    type_def: *obj.ObjTypeDef,
    depth: i32,
    is_captured: bool,
    constant: bool,
    referenced: bool = false,

    pub fn isReferenced(self: Local) bool {
        return self.referenced or
            self.type_def.def_type == .Void or
            self.type_def.def_type == .Placeholder or
            self.name.string[0] == '$' or
            (self.name.string[0] == '_' and self.name.string.len == 1);
    }
};

pub const Global = struct {
    prefix: ?[]const u8,
    name: *obj.ObjString,
    name_token: Ast.TokenIndex,
    location: Ast.TokenIndex,
    type_def: *obj.ObjTypeDef,
    export_alias: ?[]const u8 = null,
    imported_from: ?[]const u8 = null,

    initialized: bool = false,
    exported: bool = false,
    hidden: bool = false,
    constant: bool,
    referenced: bool = false,

    pub fn isReferenced(self: Global) bool {
        const function_type = if (self.type_def.def_type == .Function)
            self.type_def.resolved_type.?.Function.function_type
        else
            null;

        return self.referenced or
            self.type_def.def_type == .Void or
            self.type_def.def_type == .Placeholder or
            (function_type != null and (function_type == .Extern or function_type == .Abstract or function_type == .EntryPoint or function_type == .ScriptEntryPoint or function_type != .Repl)) or
            self.name.string[0] == '$' or
            (self.name.string[0] == '_' and self.name.string.len == 1) or
            self.exported;
    }
};

pub const UpValue = struct {
    index: u8,
    is_local: bool,
};

pub const Frame = struct {
    enclosing: ?*Frame = null,
    locals: [255]Local,
    local_count: u8 = 0,
    upvalues: [255]UpValue,
    upvalue_count: u8 = 0,
    scope_depth: u32 = 0,
    // Keep track of the node that introduced the scope (useful for labeled break/continue statements)
    scopes: std.ArrayList(?Ast.Node.Index),
    // If false, `return` was omitted or within a conditionned block (if, loop, etc.)
    // We only count `return` emitted within the scope_depth 0 of the current function or unconditionned else statement
    function_node: Ast.Node.Index,
    function: ?*obj.ObjFunction = null,
    generics: ?*std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef) = null,
    constants: std.ArrayList(Value),

    in_try: bool = false,
    in_block_expression: ?u32 = null,

    pub fn deinit(self: *Frame) void {
        self.scopes.deinit();
        self.constants.deinit();
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
    generics: ?*std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef) = null,
};

pub const ScriptImport = struct {
    function: Ast.Node.Index,
    globals: std.ArrayList(Global),
    absolute_path: *obj.ObjString,
};

const LocalScriptImport = struct {
    referenced: bool = false,
    location: Ast.TokenIndex,
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
    prefix: ?ParseFn,
    infix: ?InfixParseFn,
    precedence: Precedence,
};

const search_paths = [_][]const u8{
    "$/?.!",
    "$/?/main.!",
    "$/?/src/?.!",
    "$/?/src/main.!",
    "./?.!",
    "./?/main.!",
    "./?/src/main.!",
    "./?/src/?.!",
    "/usr/share/buzz/?.!",
    "/usr/share/buzz/?/main.!",
    "/usr/share/buzz/?/src/main.!",
    "/usr/local/share/buzz/?/src/?.!",
    "/usr/local/share/buzz/?.!",
    "/usr/local/share/buzz/?/main.!",
    "/usr/local/share/buzz/?/src/main.!",
    "/usr/local/share/buzz/?/src/?.!",
};

const lib_search_paths = [_][]const u8{
    "$/lib?.!",
    "$/?/src/lib?.!",
    "./lib?.!",
    "./?/src/lib?.!",
    "/usr/share/buzz/lib?.!",
    "/usr/share/buzz/?/src/lib?.!",
    "/usr/share/local/buzz/lib?.!",
    "/usr/share/local/buzz/?/src/lib?.!",
};

const zdef_search_paths = [_][]const u8{
    "./?.!",
    "/usr/lib/?.!",
    "/usr/local/lib/?.!",
    "./lib?.!",
    "/usr/lib/lib?.!",
    "/usr/local/lib/lib?.!",
};

const rules = [_]ParseRule{
    .{ .prefix = null, .infix = null, .precedence = .None }, // Pipe
    .{ .prefix = list, .infix = subscript, .precedence = .Call }, // LeftBracket
    .{ .prefix = null, .infix = null, .precedence = .None }, // RightBracket
    .{ .prefix = grouping, .infix = call, .precedence = .Call }, // LeftParen
    .{ .prefix = null, .infix = null, .precedence = .None }, // RightParen
    .{ .prefix = map, .infix = objectInit, .precedence = .Primary }, // LeftBrace
    .{ .prefix = null, .infix = null, .precedence = .None }, // RightBrace
    .{ .prefix = anonymousObjectInit, .infix = dot, .precedence = .Call }, // Dot
    .{ .prefix = null, .infix = null, .precedence = .None }, // Comma
    .{ .prefix = null, .infix = null, .precedence = .None }, // Semicolon
    .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // Greater
    .{ .prefix = typeExpression, .infix = binary, .precedence = .Comparison }, // Less
    .{ .prefix = null, .infix = binary, .precedence = .Term }, // Plus
    .{ .prefix = unary, .infix = binary, .precedence = .Term }, // Minus
    .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Star
    .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Slash
    .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Percent
    .{ .prefix = null, .infix = gracefulUnwrap, .precedence = .Call }, // Question
    .{ .prefix = unary, .infix = forceUnwrap, .precedence = .Call }, // Bang
    .{ .prefix = null, .infix = null, .precedence = .None }, // Colon
    .{ .prefix = null, .infix = genericResolve, .precedence = .Call }, // DoubleColon
    .{ .prefix = null, .infix = null, .precedence = .None }, // Equal
    .{ .prefix = null, .infix = binary, .precedence = .Equality }, // EqualEqual
    .{ .prefix = null, .infix = binary, .precedence = .Equality }, // BangEqual
    .{ .prefix = null, .infix = null, .precedence = .None }, // BangGreater
    .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // GreaterEqual
    .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // LessEqual
    .{ .prefix = null, .infix = binary, .precedence = .NullCoalescing }, // QuestionQuestion
    .{ .prefix = null, .infix = null, .precedence = .None }, // Arrow
    .{ .prefix = literal, .infix = null, .precedence = .None }, // True
    .{ .prefix = literal, .infix = null, .precedence = .None }, // False
    .{ .prefix = literal, .infix = null, .precedence = .None }, // Null
    .{ .prefix = null, .infix = null, .precedence = .None }, // Str
    .{ .prefix = null, .infix = null, .precedence = .None }, // Ud
    .{ .prefix = null, .infix = null, .precedence = .None }, // Int
    .{ .prefix = null, .infix = null, .precedence = .None }, // Float
    .{ .prefix = null, .infix = null, .precedence = .None }, // Type
    .{ .prefix = null, .infix = null, .precedence = .None }, // Bool
    .{ .prefix = null, .infix = null, .precedence = .None }, // Function
    .{ .prefix = null, .infix = binary, .precedence = .Shift }, // ShiftRight
    .{ .prefix = null, .infix = binary, .precedence = .Shift }, // ShiftLeft
    .{ .prefix = null, .infix = binary, .precedence = .Bitwise }, // Xor
    .{ .prefix = null, .infix = binary, .precedence = .Bitwise }, // Bor
    .{ .prefix = unary, .infix = null, .precedence = .Term }, // Bnot
    .{ .prefix = null, .infix = @"or", .precedence = .Or }, // Or
    .{ .prefix = null, .infix = @"and", .precedence = .And }, // And
    .{ .prefix = null, .infix = null, .precedence = .None }, // Return
    .{ .prefix = inlineIf, .infix = null, .precedence = .None }, // If
    .{ .prefix = null, .infix = null, .precedence = .None }, // Else
    .{ .prefix = null, .infix = null, .precedence = .None }, // Do
    .{ .prefix = null, .infix = null, .precedence = .None }, // Until
    .{ .prefix = null, .infix = null, .precedence = .None }, // While
    .{ .prefix = null, .infix = null, .precedence = .None }, // For
    .{ .prefix = null, .infix = null, .precedence = .None }, // ForEach
    .{ .prefix = null, .infix = null, .precedence = .None }, // Break
    .{ .prefix = null, .infix = null, .precedence = .None }, // Continue
    .{ .prefix = null, .infix = null, .precedence = .None }, // In
    .{ .prefix = null, .infix = is, .precedence = .IsAs }, // Is
    .{ .prefix = literal, .infix = null, .precedence = .None }, // Integer
    .{ .prefix = literal, .infix = null, .precedence = .None }, // FloatValue
    .{ .prefix = string, .infix = null, .precedence = .None }, // String
    .{ .prefix = variable, .infix = null, .precedence = .None }, // Identifier
    .{ .prefix = fun, .infix = null, .precedence = .None }, // Fun
    .{ .prefix = null, .infix = null, .precedence = .None }, // Object
    .{ .prefix = null, .infix = null, .precedence = .None }, // Obj
    .{ .prefix = null, .infix = null, .precedence = .None }, // Protocol
    .{ .prefix = null, .infix = null, .precedence = .None }, // Enum
    .{ .prefix = null, .infix = null, .precedence = .None }, // Throw
    .{ .prefix = null, .infix = null, .precedence = .None }, // Try
    .{ .prefix = null, .infix = null, .precedence = .None }, // Catch
    .{ .prefix = null, .infix = null, .precedence = .None }, // Test
    .{ .prefix = null, .infix = null, .precedence = .None }, // Import
    .{ .prefix = null, .infix = null, .precedence = .None }, // Export
    .{ .prefix = null, .infix = null, .precedence = .None }, // Const
    .{ .prefix = null, .infix = null, .precedence = .None }, // Static
    .{ .prefix = blockExpression, .infix = null, .precedence = .None }, // From
    .{ .prefix = null, .infix = null, .precedence = .None }, // As
    .{ .prefix = null, .infix = as, .precedence = .IsAs }, // AsQuestion
    .{ .prefix = null, .infix = null, .precedence = .None }, // Extern
    .{ .prefix = null, .infix = null, .precedence = .None }, // Eof
    .{ .prefix = null, .infix = null, .precedence = .None }, // Error
    .{ .prefix = literal, .infix = null, .precedence = .None }, // Void
    .{ .prefix = null, .infix = null, .precedence = .None }, // Docblock
    .{ .prefix = pattern, .infix = null, .precedence = .None }, // Pattern
    .{ .prefix = null, .infix = null, .precedence = .None }, // pat
    .{ .prefix = null, .infix = null, .precedence = .None }, // fib
    .{ .prefix = asyncCall, .infix = binary, .precedence = .Term }, // &
    .{ .prefix = resumeFiber, .infix = null, .precedence = .Primary }, // resume
    .{ .prefix = resolveFiber, .infix = null, .precedence = .Primary }, // resolve
    .{ .prefix = yield, .infix = null, .precedence = .Primary }, // yield
    .{ .prefix = null, .infix = range, .precedence = .Primary }, // ..
    .{ .prefix = null, .infix = null, .precedence = .None }, // any
    .{ .prefix = null, .infix = null, .precedence = .None }, // zdef
    .{ .prefix = typeOfExpression, .infix = null, .precedence = .Unary }, // typeof
    .{ .prefix = null, .infix = null, .precedence = .None }, // var
    .{ .prefix = null, .infix = null, .precedence = .None }, // out
    .{ .prefix = null, .infix = null, .precedence = .None }, // namespace
    .{ .prefix = null, .infix = null, .precedence = .None }, // rg
};

ast: Ast,
gc: *GarbageCollector,
scanner: ?Scanner = null,
current_token: ?Ast.TokenIndex = null,
script_name: []const u8 = undefined,
// If true the script is being imported
imported: bool = false,
// True when parsing a declaration inside an export statement
exporting: bool = false,
// Cached imported functions (shared across instances of Parser)
imports: *std.StringHashMap(ScriptImport),
// Keep track of things imported by the current script
script_imports: std.StringHashMap(LocalScriptImport),
test_count: u64 = 0,
// FIXME: use SinglyLinkedList instead of heap allocated ptrs
current: ?*Frame = null,
current_object: ?ObjectFrame = null,
globals: std.ArrayList(Global),
namespace: ?[]const u8 = null,
flavor: RunFlavor,
ffi: FFI,
reporter: Reporter,

// Jump to patch at end of current expression with a optional unwrapping in the middle of it
opt_jumps: ?std.ArrayList(Precedence) = null,

pub fn init(
    gc: *GarbageCollector,
    imports: *std.StringHashMap(ScriptImport),
    imported: bool,
    flavor: RunFlavor,
) Self {
    return .{
        .gc = gc,
        .imports = imports,
        .script_imports = std.StringHashMap(LocalScriptImport).init(gc.allocator),
        .imported = imported,
        .globals = std.ArrayList(Global).init(gc.allocator),
        .flavor = flavor,
        .reporter = Reporter{
            .allocator = gc.allocator,
            .error_prefix = "Syntax",
        },
        .ffi = FFI.init(gc),
        .ast = Ast.init(gc.allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.script_imports.deinit();
    if (self.opt_jumps) |jumps| {
        jumps.deinit();
    }
    self.ffi.deinit();
}

inline fn reportErrorAtCurrent(self: *Self, error_type: Reporter.Error, message: []const u8) void {
    self.reporter.reportErrorAt(
        error_type,
        self.ast.tokens.get(self.current_token.?),
        message,
    );
}

pub inline fn reportError(self: *Self, error_type: Reporter.Error, message: []const u8) void {
    self.reporter.reportErrorAt(
        error_type,
        self.ast.tokens.get(if (self.current_token.? > 0) self.current_token.? - 1 else 0),
        message,
    );
}

inline fn reportErrorFmt(self: *Self, error_type: Reporter.Error, comptime fmt: []const u8, args: anytype) void {
    self.reporter.reportErrorFmt(
        error_type,
        self.ast.tokens.get(if (self.current_token.? > 0) self.current_token.? - 1 else 0),
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
                self.reportErrorAtCurrent(
                    .unknown,
                    new_token.literal_string orelse "Unknown error.",
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
                self.reportErrorAtCurrent(
                    .unknown,
                    new_token.literal_string orelse "Unknown error.",
                );
            }

            _ = try self.ast.appendToken(new_token);

            if (new_token.tag != .Error) {
                break;
            }
        }
    }
}

pub fn consume(self: *Self, tag: Token.Type, message: []const u8) !void {
    if (self.ast.tokens.items(.tag)[self.current_token.?] == tag) {
        try self.advance();
        return;
    }

    self.reportErrorAtCurrent(.syntax, message);
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
            self.reportErrorAtCurrent(.syntax, token.literal_string orelse "Unknown error.");
        }
    }

    return self.ast.tokens.items(.tag)[self.current_token.? + n + 1] == tag;
}

// Check for a sequence ahead, a null in the sequence means continue until next token in the sequence is found
// Don't use null if you're not sure the following token **must** be found, otherwise it'll check the whole source
// Right now the null is only used to parse ahead a generic object type like `Person::<K, V, ...> identifier`
fn checkSequenceAhead(self: *Self, sequence: []const ?Token.Type, limit: usize) !bool {
    std.debug.assert(sequence.len > 0);

    if (!self.check(sequence[0].?)) {
        return false;
    }

    var i: usize = 0;
    for (sequence[1..], 1..) |tag, n| {
        // Avoid going to far
        if (i > limit) {
            return false;
        }

        if (tag) |tt| {
            if (!try self.checkAhead(tt, i)) {
                return false;
            }

            i += 1;
        } else {
            // Advance until next token
            std.debug.assert(n < sequence.len - 1 and sequence[n + 1] != null); // There must be at least one more token in the sequence
            const next_token = sequence[n + 1].?;

            while (!try self.checkAhead(next_token, i)) : (i += 1) {
                // Avoid looping forever if found EOF or Error
                const last = self.ast.tokens.items(.tag)[self.ast.tokens.len - 1];
                if (last == .Eof or last == .Error) {
                    return false;
                }
            }
        }
    }

    return true;
}

fn match(self: *Self, tag: Token.Type) !bool {
    if (!self.check(tag)) {
        return false;
    }

    try self.advance();

    return true;
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
            .Const,
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

pub fn parse(self: *Self, source: []const u8, file_name: []const u8) !?Ast {
    if (self.scanner != null) {
        self.scanner = null;
    }

    self.scanner = Scanner.init(self.gc.allocator, file_name, source);

    const function_type: obj.ObjFunction.FunctionType = if (!self.imported and self.flavor == .Repl)
        .Repl
    else if (self.imported)
        .Script
    else
        .ScriptEntryPoint;

    const function_name: []const u8 = switch (function_type) {
        .EntryPoint => "main",
        .ScriptEntryPoint, .Script => file_name,
        .Repl => "REPL",
        else => "???",
    };

    const body_node = try self.ast.appendNode(
        .{
            .tag = .Block,
            .location = 0,
            .end_location = undefined,
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
                            .script_name = try self.gc.copyString(file_name),
                            .return_type = self.gc.type_registry.void_type,
                            .yield_type = self.gc.type_registry.void_type,
                            .parameters = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator),
                            .defaults = std.AutoArrayHashMap(*obj.ObjString, Value).init(self.gc.allocator),
                            .function_type = function_type,
                            .generic_types = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator),
                        },
                    },
                },
            ),
            .components = .{
                .Function = .{
                    .function_signature = null,
                    .id = Ast.Function.nextId(),
                    .upvalue_binding = std.AutoArrayHashMap(u8, bool).init(self.gc.allocator),
                    .body = body_node,
                },
            },
        },
    );

    var entry = Ast.Function.Entry{
        .test_slots = undefined,
        .test_locations = undefined,
    };

    self.script_name = file_name;

    try self.beginFrame(function_type, function_node, null);

    self.reporter.had_error = false;
    self.reporter.panic_mode = false;

    try self.advancePastEof();

    while (!(try self.match(.Eof))) {
        if (function_type == .Repl) {
            // When running in REPL, global scope is allowed to run statements since the global scope becomes procedural
            if (self.declarationOrStatement(null) catch |err| {
                if (BuildOptions.debug) {
                    io.print("Parsing failed with error {}\n", .{err});
                }
                return null;
            }) |decl| {
                var statements = std.ArrayList(Ast.Node.Index).fromOwnedSlice(
                    self.gc.allocator,
                    self.ast.nodes.items(.components)[body_node].Block,
                );
                defer statements.shrinkAndFree(statements.items.len);

                try statements.append(decl);

                self.ast.nodes.items(.components)[body_node].Block = statements.items;
            }
        } else {
            if (self.declaration() catch |err| {
                if (err == error.ReachedMaximumMemoryUsage) {
                    return err;
                }

                if (BuildOptions.debug) {
                    io.print("Parsing failed with error {}\n", .{err});
                }
                return null;
            }) |decl| {
                var statements = std.ArrayList(Ast.Node.Index).fromOwnedSlice(
                    self.gc.allocator,
                    self.ast.nodes.items(.components)[body_node].Block,
                );
                defer statements.shrinkAndFree(statements.items.len);

                try statements.append(decl);

                self.ast.nodes.items(.components)[body_node].Block = statements.items;
            } else {
                self.reportError(.syntax, "Expected statement");
                break;
            }
        }
    }

    // If top level, search `main` or `test` function(s) and call them
    // Then put any exported globals on the stack
    if (function_type == .ScriptEntryPoint) {
        for (self.globals.items, 0..) |global, index| {
            if (std.mem.eql(u8, global.name.string, "main") and !global.hidden and (self.namespace == null or global.prefix == null or std.mem.eql(u8, global.prefix.?, self.namespace.?))) {
                entry.main_slot = index;
                entry.main_location = global.location;
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
            try test_locations.append(global.location);
        }
    }

    test_slots.shrinkAndFree(test_slots.items.len);
    test_locations.shrinkAndFree(test_locations.items.len);
    entry.test_slots = test_slots.items;
    entry.test_locations = test_locations.items;

    // If we're being imported, put all globals on the stack
    if (self.imported) {
        entry.exported_count = self.globals.items.len;
    }

    // Check there's no more root placeholders
    for (self.globals.items) |global| {
        if (global.type_def.def_type == .Placeholder) {
            self.reporter.reportPlaceholder(self.ast, global.type_def.resolved_type.?.Placeholder);
        }
    }

    // Check there's no unreferenced imports
    if (self.flavor != .Repl) {
        var it = self.script_imports.iterator();
        while (it.next()) |kv| {
            if (!kv.value_ptr.*.referenced) {
                const location = self.ast.tokens.get(kv.value_ptr.*.location);

                self.reporter.warnFmt(
                    .unused_import,
                    location,
                    "Unused import",
                    .{},
                );
            }
        }
    }

    self.ast.nodes.items(.components)[function_node].Function.entry = entry;

    self.ast.root = if (self.reporter.had_error) null else self.endFrame();

    return if (self.reporter.had_error) null else self.ast;
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
        .constants = std.ArrayList(Value).init(self.gc.allocator),
        .scopes = std.ArrayList(?Ast.Node.Index).init(self.gc.allocator),
    };

    if (function_type == .Extern) {
        return;
    }

    // First local is reserved for an eventual `this` or cli arguments
    var local: *Local = &self.current.?.locals[self.current.?.local_count];
    self.current.?.local_count += 1;
    local.depth = 0;
    local.is_captured = false;

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
                            self.gc.allocator,
                            self.gc.type_registry.str_type,
                        ),
                    },
                },
            );
        },
        else => {
            local.type_def = self.gc.type_registry.void_type;
        },
    }

    const name: []const u8 = switch (function_type) {
        .Method => "this",
        .EntryPoint => "$args",
        .ScriptEntryPoint => "$args",
        else => "_",
    };

    local.name = try self.gc.copyString(name);
}

fn endFrame(self: *Self) Ast.Node.Index {
    var i: usize = 0;
    while (i < self.current.?.local_count) : (i += 1) {
        const local = self.current.?.locals[i];

        // Check discarded locals
        if (self.flavor != .Repl and !local.isReferenced()) {
            const type_def_str = local.type_def.toStringAlloc(self.gc.allocator) catch unreachable;
            defer type_def_str.deinit();

            self.reporter.warnFmt(
                .unused_argument,
                self.ast.tokens.get(local.location),
                "Unused local of type `{s}`",
                .{
                    type_def_str.items,
                },
            );
        }
    }

    // If global scope, check unused globals
    const function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;
    if (function_type == .Script or function_type == .ScriptEntryPoint) {
        for (self.globals.items) |global| {
            if (!global.isReferenced()) {
                const type_def_str = global.type_def.toStringAlloc(self.gc.allocator) catch unreachable;
                defer type_def_str.deinit();

                self.reporter.warnFmt(
                    .unused_argument,
                    self.ast.tokens.get(global.location),
                    "Unused global of type `{s}`",
                    .{
                        type_def_str.items,
                    },
                );
            }
        }
    }

    self.current.?.deinit();

    const current_node = self.current.?.function_node;
    self.current = self.current.?.enclosing;

    return current_node;
}

fn beginScope(self: *Self, at: ?Ast.Node.Index) !void {
    try self.current.?.scopes.append(at);
    self.current.?.scope_depth += 1;
}

fn endScope(self: *Self) ![]Chunk.OpCode {
    const current = self.current.?;
    _ = current.scopes.pop();
    var closing = std.ArrayList(Chunk.OpCode).init(self.gc.allocator);
    current.scope_depth -= 1;

    while (current.local_count > 0 and current.locals[current.local_count - 1].depth > current.scope_depth) {
        const local = current.locals[current.local_count - 1];

        if (local.is_captured) {
            try closing.append(.OP_CLOSE_UPVALUE);
        } else {
            try closing.append(.OP_POP);
        }

        // Check discarded locals
        if (self.flavor != .Repl and !local.isReferenced()) {
            const type_def_str = local.type_def.toStringAlloc(self.gc.allocator) catch unreachable;
            defer type_def_str.deinit();

            self.reporter.warnFmt(
                .unused_argument,
                self.ast.tokens.get(local.location),
                "Unused local of type `{s}`",
                .{
                    type_def_str.items,
                },
            );
        }

        current.local_count -= 1;
    }

    closing.shrinkAndFree(closing.items.len);
    return closing.items;
}

fn closeScope(self: *Self, upto_depth: usize) ![]Chunk.OpCode {
    const current = self.current.?;
    var closing = std.ArrayList(Chunk.OpCode).init(self.gc.allocator);

    var local_count = current.local_count;
    while (local_count > 0 and current.locals[local_count - 1].depth > upto_depth - 1) {
        if (current.locals[local_count - 1].is_captured) {
            try closing.append(.OP_CLOSE_UPVALUE);
        } else {
            try closing.append(.OP_POP);
        }

        local_count -= 1;
    }

    closing.shrinkAndFree(closing.items.len);
    return closing.items;
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
        self.reportError(.syntax, "Expected expression.");

        // TODO: find a way to continue or catch that error
        return CompileError.Unrecoverable;
    }

    const canAssign: bool = @intFromEnum(precedence) <= @intFromEnum(Precedence.Assignment);
    var node = try prefixRule.?(self, canAssign);

    while (@intFromEnum(getRule(self.ast.tokens.items(.tag)[self.current_token.?]).precedence) >= @intFromEnum(precedence)) {
        // Patch optional jumps
        if (self.opt_jumps) |jumps| {
            std.debug.assert(jumps.items.len > 0);
            const first_jump: Precedence = jumps.items[0];

            if (@intFromEnum(getRule(self.ast.tokens.items(.tag)[self.current_token.?]).precedence) < @intFromEnum(first_jump)) {
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

        const infixRule: InfixParseFn = getRule(self.ast.tokens.items(.tag)[self.current_token.? - 1]).infix.?;
        node = try infixRule(self, canAssign, node);
    }

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
        self.reportError(.assignable, "Invalid assignment target.");
    }

    self.opt_jumps = previous_opt_jumps;

    return node;
}

fn block(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var statements = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    defer statements.shrinkAndFree(statements.items.len);
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        if (try self.declarationOrStatement(loop_scope)) |declOrStmt| {
            try statements.append(declOrStmt);
        }
    }

    try self.consume(.RightBrace, "Expected `}}` after block.");

    return try self.ast.appendNode(
        .{
            .tag = .Block,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Block = statements.items,
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

fn declaration(self: *Self) Error!?Ast.Node.Index {
    const global_scope = self.current.?.scope_depth == 0;

    const docblock = if (global_scope and try self.match(.Docblock))
        self.current_token.? - 1
    else
        null;

    if (try self.match(.Extern)) {
        const node = try self.funDeclaration();

        self.ast.nodes.items(.docblock)[node] = docblock;

        return node;
    } else if ((self.current_token == 0 or self.ast.tokens.items(.tag)[self.current_token.? - 1] != .Export) and try self.match(.Export)) {
        return try self.exportStatement();
    } else {
        const constant: bool = try self.match(.Const);

        var node = if (global_scope and !constant and try self.match(.Object))
            try self.objectDeclaration()
        else if (global_scope and !constant and try self.match(.Protocol))
            try self.protocolDeclaration()
        else if (global_scope and !constant and try self.match(.Enum))
            try self.enumDeclaration()
        else if (!constant and try self.match(.Fun))
            try self.funDeclaration()
        else if (!constant and try self.match(.Var))
            try self.varDeclaration(
                false,
                null,
                .Semicolon,
                false,
                true,
                false,
            )
        else if (try self.match(.Pat))
            try self.varDeclaration(
                false,
                try self.simpleType(.Pattern),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Ud))
            try self.varDeclaration(
                false,
                try self.simpleType(.UserData),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Str))
            try self.varDeclaration(
                false,
                try self.simpleType(.String),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Int))
            try self.varDeclaration(
                false,
                try self.simpleType(.Integer),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Float))
            try self.varDeclaration(
                false,
                try self.simpleType(.Float),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Bool))
            try self.varDeclaration(
                false,
                try self.simpleType(.Bool),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Range))
            try self.varDeclaration(
                false,
                try self.simpleType(.Range),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Type))
            try self.varDeclaration(
                false,
                try self.simpleType(.Type),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Any))
            try self.varDeclaration(
                false,
                try self.simpleType(.Any),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (self.current_token.? > 0 and self.current_token.? - 1 < self.ast.tokens.len - 1 and
            self.ast.tokens.items(.tag)[self.current_token.?] == .Identifier and
            self.ast.tokens.items(.lexeme)[self.current_token.?].len == 1 and
            self.ast.tokens.items(.lexeme)[self.current_token.?][0] == '_')
            try self.varDeclaration(
                false,
                try self.simpleType(.Any),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Fib))
            try self.varDeclaration(
                false,
                try self.parseFiberType(null),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.Obj))
            try self.varDeclaration(
                false,
                try self.parseObjType(null),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (try self.match(.LeftBracket))
            try self.listDeclaration(constant)
        else if (try self.match(.LeftBrace))
            try self.mapDeclaration(constant)
        else if (!constant and try self.match(.Test))
            try self.testStatement()
        else if (try self.match(.Function))
            try self.varDeclaration(
                false,
                try self.parseFunctionType(null),
                .Semicolon,
                constant,
                true,
                false,
            )
        else if (global_scope and try self.match(.Import))
            try self.importStatement()
        else if (global_scope and try self.match(.Zdef))
            try self.zdefStatement()
        else if (global_scope and !constant and try self.match(.Export))
            try self.exportStatement()
        else if (self.check(.Identifier)) user_decl: {
            if ( // As of now this is the only place where we need to check more than one token ahead
            // Note that we would not have to do this if type were given **after** the identifier. But changing this is a pretty big left turn.
            // `Type variable`
            try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Identifier }, 2) or
                // `prefix.Type variable`
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Dot, .Identifier, .Identifier }, 4) or
                // `prefix.Type? variable`
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Dot, .Identifier, .Question, .Identifier }, 5) or
                // `Type? variable`
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Question, .Identifier }, 3) or
                // `Type::<...> variable`
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .DoubleColon, .Less, null, .Greater, .Identifier }, 255 * 2) or
                // - Type::<...>? variable
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .DoubleColon, .Less, null, .Greater, .Question, .Identifier }, 255 * 2) or
                // - prefix.Type::<...> variable
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Dot, .Identifier, .DoubleColon, .Less, null, .Greater, .Identifier }, 255 * 2) or
                // - prefix.Type::<...>? variable
                try self.checkSequenceAhead(&[_]?Token.Type{ .Identifier, .Dot, .Identifier, .DoubleColon, .Less, null, .Greater, .Question, .Identifier }, 255 * 2))
            {
                _ = try self.advance(); // consume first identifier
                break :user_decl try self.userVarDeclaration(false, constant);
            }

            break :user_decl null;
        } else if (global_scope and !constant and try self.match(.Export))
            try self.exportStatement()
        else if (global_scope and !constant and try self.match(.Namespace))
            try self.namespaceStatement()
        else
            null;

        if (node == null and constant) {
            node = try self.varDeclaration(
                false,
                null,
                .Semicolon,
                true,
                true,
                false,
            );
        }

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
}
fn declarationOrStatement(self: *Self, loop_scope: ?LoopScope) !?Ast.Node.Index {
    return try self.declaration() orelse try self.statement(false, loop_scope);
}

// When a break statement, will return index of jump to patch
fn statement(self: *Self, hanging: bool, loop_scope: ?LoopScope) !?Ast.Node.Index {
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
    } else if (try self.match(.Return)) {
        std.debug.assert(!hanging);
        return try self.returnStatement();
    } else if (try self.match(.Try)) {
        std.debug.assert(!hanging);
        return try self.tryStatement();
    } else if (try self.match(.Break)) {
        std.debug.assert(!hanging);
        return try self.breakStatement(loop_scope);
    } else if (try self.match(.Continue)) {
        std.debug.assert(!hanging);
        return try self.continueStatement(loop_scope);
    } else if (try self.match(.Import)) {
        std.debug.assert(!hanging);
        return try self.importStatement();
    } else if (try self.match(.Out)) {
        std.debug.assert(!hanging);
        return try self.outStatement();
    } else if (try self.match(.Namespace)) {
        std.debug.assert(!hanging);
        return try self.namespaceStatement();
    } else if (try self.match(.Throw)) {
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
    } else {
        return try self.expressionStatement(hanging);
    }

    return null;
}

fn addLocal(self: *Self, name: Ast.TokenIndex, local_type: *obj.ObjTypeDef, constant: bool) Error!usize {
    if (self.current.?.local_count == 255) {
        self.reportError(.locals_count, "Too many local variables in scope.");
        return 0;
    }

    const name_lexeme = self.ast.tokens.items(.lexeme)[name];
    const function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;
    self.current.?.locals[self.current.?.local_count] = Local{
        .name = try self.gc.copyString(name_lexeme),
        .name_token = name,
        .location = name,
        .depth = -1,
        .is_captured = false,
        .type_def = local_type,
        .constant = constant,
        // Extern and abstract function arguments are considered referenced
        .referenced = function_type == .Extern or function_type == .Abstract,
    };

    self.current.?.local_count += 1;

    return self.current.?.local_count - 1;
}

fn addGlobal(self: *Self, name: Ast.TokenIndex, global_type: *obj.ObjTypeDef, constant: bool) Error!usize {
    const lexemes = self.ast.tokens.items(.lexeme);

    // Search for an existing placeholder global with the same name
    for (self.globals.items, 0..) |*global, index| {
        if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and std.mem.eql(u8, lexemes[name], global.name.string)) {
            global.exported = self.exporting;

            if (global_type.def_type != .Placeholder) {
                try self.resolvePlaceholder(global.type_def, global_type, constant);
            }

            return index;
        }
    }

    if (self.globals.items.len == std.math.maxInt(u24)) {
        self.reportError(.globals_count, "Too many global variables.");
        return 0;
    }

    try self.globals.append(
        .{
            .prefix = self.namespace,
            .name_token = name,
            .name = try self.gc.copyString(lexemes[name]),
            .location = name,
            .type_def = global_type,
            .constant = constant,
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

    const lexeme = self.ast.tokens.items(.lexeme)[name];

    if (std.mem.eql(u8, lexeme, "_")) {
        return null;
    }

    var i: usize = frame.local_count - 1;
    while (i >= 0) : (i -= 1) {
        var local: *Local = &frame.locals[i];
        if (std.mem.eql(u8, lexeme, local.name.string)) {
            if (local.depth == -1) {
                self.reportErrorFmt(
                    .local_initializer,
                    "Can't read local variable `{s}` in its own initializer.",
                    .{lexeme},
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
pub fn resolveGlobal(self: *Self, prefix: ?[]const u8, name: []const u8) Error!?usize {
    if (self.globals.items.len == 0) {
        return null;
    }

    if (std.mem.eql(u8, name, "_")) {
        return null;
    }

    var i: usize = self.globals.items.len - 1;
    while (i >= 0) : (i -= 1) {
        const global: *Global = &self.globals.items[i];

        if ((global.prefix == null or // Not prefixed
            (prefix != null and std.mem.eql(u8, prefix.?, global.prefix.?)) or // Same prefix as provided
            (self.namespace != null and std.mem.eql(u8, global.prefix.?, self.namespace.?)) // Prefix is the current namespace
        ) and
            std.mem.eql(u8, name, global.name.string) and
            !global.hidden)
        {
            if (!global.initialized) {
                self.reportErrorFmt(
                    .global_initializer,
                    "Can't read global `{s}` variable in its own initializer.",
                    .{global.name.string},
                );
            }

            global.referenced = true;

            return i;
            // Is it an import prefix?
        } else if (global.prefix != null and std.mem.eql(u8, name, global.prefix.?)) {
            const had_error = self.reporter.had_error;

            try self.consume(.Dot, "Expected `.` after import prefix.");
            try self.consume(.Identifier, "Expected identifier after import prefix.");

            // Avoid infinite recursion
            if (!had_error and self.reporter.had_error) {
                return null;
            }

            return try self.resolveGlobal(global.prefix.?, self.ast.tokens.items(.lexeme)[self.current_token.? - 1]);
        }

        if (i == 0) break;
    }

    return null;
}

fn resolvePlaceholderWithRelation(
    self: *Self,
    child: *obj.ObjTypeDef,
    resolved_type: *obj.ObjTypeDef,
    constant: bool,
    relation: obj.PlaceholderDef.PlaceholderRelation,
) Error!void {
    const child_placeholder = child.resolved_type.?.Placeholder;

    if (BuildOptions.debug_placeholders) {
        io.print(
            "Attempts to resolve @{} child placeholder @{} ({s}) with relation {}\n",
            .{
                @intFromPtr(resolved_type),
                @intFromPtr(child),
                if (child_placeholder.name) |name| name.string else "unknown",
                child_placeholder.parent_relation.?,
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
                try resolved_type.toInstance(self.gc.allocator, &self.gc.type_registry),
                false,
            );
        },
        .Parent => {
            try self.resolvePlaceholder(
                child,
                try resolved_type.toParentType(self.gc.allocator, &self.gc.type_registry),
                false,
            );
        },
        .Call => {
            // Can we call the parent?
            if (resolved_type.def_type != .Function) {
                self.reporter.reportErrorAt(
                    .callable,
                    self.ast.tokens.get(child_placeholder.where),
                    "Can't be called",
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
                self.reporter.reportErrorAt(
                    .callable,
                    self.ast.tokens.get(child_placeholder.where),
                    "Can't be called",
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
                try self.resolvePlaceholder(child, resolved_type.resolved_type.?.List.item_type, false);
            } else if (resolved_type.def_type == .Map) {
                try self.resolvePlaceholder(child, try resolved_type.resolved_type.?.Map.value_type.cloneOptional(&self.gc.type_registry), false);
            } else if (resolved_type.def_type == .String) {
                try self.resolvePlaceholder(child, self.gc.type_registry.str_type, false);
            } else {
                self.reporter.reportErrorAt(
                    .subscriptable,
                    self.ast.tokens.get(child_placeholder.where),
                    "Can't be subscripted",
                );
                return;
            }
        },
        .Key => {
            if (resolved_type.def_type == .Map) {
                try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Map.key_type, false);
            } else if (resolved_type.def_type == .List or resolved_type.def_type == .String) {
                try self.resolvePlaceholder(
                    child,
                    self.gc.type_registry.int_type,
                    false,
                );
            } else {
                self.reporter.reportErrorAt(
                    .map_key_type,
                    self.ast.tokens.get(child_placeholder.where),
                    "Can't be a key",
                );
                return;
            }
        },
        .FieldAccess => {
            switch (resolved_type.def_type) {
                .List => {
                    std.debug.assert(child_placeholder.name != null);

                    if (try obj.ObjList.ListDef.member(
                        resolved_type,
                        self,
                        child_placeholder.name.?.string,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Map => {
                    std.debug.assert(child_placeholder.name != null);

                    if (try obj.ObjMap.MapDef.member(
                        resolved_type,
                        self,
                        child_placeholder.name.?.string,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .String => {
                    std.debug.assert(child_placeholder.name != null);

                    if (try obj.ObjString.memberDefByName(
                        self,
                        child_placeholder.name.?.string,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Pattern => {
                    std.debug.assert(child_placeholder.name != null);

                    if (try obj.ObjPattern.memberDefByName(
                        self,
                        child_placeholder.name.?.string,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Fiber => {
                    std.debug.assert(child_placeholder.name != null);

                    if (try obj.ObjFiber.memberDefByName(
                        self,
                        child_placeholder.name.?.string,
                    )) |member| {
                        try self.resolvePlaceholder(child, member, false);
                    }
                },
                .Object => {
                    // We can't create a field access placeholder without a name
                    std.debug.assert(child_placeholder.name != null);

                    const object_def = resolved_type.resolved_type.?.Object;

                    // Search for a field matching the placeholder
                    if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                        // TODO: remove? should only resolve with a field if field accessing an object instance?
                        try self.resolvePlaceholder(
                            child,
                            field.type_def,
                            field.constant,
                        );
                    } else {
                        self.reportErrorFmt(
                            .property_does_not_exists,
                            "`{s}` has no static field `{s}`",
                            .{
                                object_def.name.string,
                                child_placeholder.name.?.string,
                            },
                        );
                    }
                },
                .ObjectInstance => {
                    // We can't create a field access placeholder without a name
                    std.debug.assert(child_placeholder.name != null);

                    const object_def = resolved_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                    // Search for a field matching the placeholder
                    if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                        try self.resolvePlaceholder(
                            child,
                            field.type_def,
                            field.constant,
                        );
                    } else {
                        self.reportErrorFmt(
                            .property_does_not_exists,
                            "`{s}` has no field `{s}`",
                            .{
                                object_def.name.string,
                                child_placeholder.name.?.string,
                            },
                        );
                    }
                },
                .ForeignContainer => {
                    // We can't create a field access placeholder without a name
                    std.debug.assert(child_placeholder.name != null);

                    const f_def = resolved_type.resolved_type.?.ForeignContainer;

                    // Search for a field matching the placeholder
                    if (f_def.buzz_type.get(child_placeholder.name.?.string)) |field| {
                        try self.resolvePlaceholder(child, field, false);
                    } else {
                        self.reportErrorFmt(
                            .property_does_not_exists,
                            "`{s}` has no field `{s}`",
                            .{
                                f_def.name.string,
                                child_placeholder.name.?.string,
                            },
                        );
                    }
                },
                .ProtocolInstance => {
                    // We can't create a field access placeholder without a name
                    std.debug.assert(child_placeholder.name != null);

                    const protocol_def = resolved_type.resolved_type.?.ProtocolInstance.resolved_type.?.Protocol;

                    // Search for a field matching the placeholder
                    if (protocol_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                        try self.resolvePlaceholder(child, method_def, true);
                    } else {
                        self.reportErrorFmt(
                            .property_does_not_exists,
                            "`{s}` has no method `{s}`",
                            .{
                                protocol_def.name.string,
                                child_placeholder.name.?.string,
                            },
                        );
                    }
                },
                .Enum => {
                    // We can't create a field access placeholder without a name
                    std.debug.assert(child_placeholder.name != null);

                    const enum_def = resolved_type.resolved_type.?.Enum;

                    // Search for a case matching the placeholder
                    for (enum_def.cases.items) |case| {
                        if (std.mem.eql(u8, case, child_placeholder.name.?.string)) {
                            const enum_instance_def: obj.ObjTypeDef.TypeUnion = .{ .EnumInstance = resolved_type };

                            try self.resolvePlaceholder(child, try self.gc.type_registry.getTypeDef(.{
                                .def_type = .EnumInstance,
                                .resolved_type = enum_instance_def,
                            }), true);
                            break;
                        }
                    }
                },
                .EnumInstance => {
                    std.debug.assert(child_placeholder.name != null);

                    if (std.mem.eql(u8, "value", child_placeholder.name.?.string)) {
                        try self.resolvePlaceholder(
                            child,
                            resolved_type.resolved_type.?.EnumInstance.resolved_type.?.Enum.enum_type,
                            false,
                        );
                    } else {
                        self.reporter.reportErrorAt(
                            .property_does_not_exists,
                            self.ast.tokens.get(child_placeholder.where),
                            "Enum instance only has field `value`",
                        );
                        return;
                    }
                },
                else => {
                    self.reporter.reportErrorAt(
                        .field_access,
                        self.ast.tokens.get(child_placeholder.where),
                        "Doesn't support field access",
                    );
                    return;
                },
            }
        },
        .Assignment => {
            if (constant) {
                self.reporter.reportErrorAt(
                    .constant,
                    self.ast.tokens.get(child_placeholder.where),
                    "Is constant.",
                );
                return;
            }

            // Assignment relation from a once Placeholder and now Object/Enum is creating an instance
            const child_type: *obj.ObjTypeDef = try resolved_type.toInstance(self.gc.allocator, &self.gc.type_registry);

            // Is child type matching the parent?
            try self.resolvePlaceholder(child, child_type, false);
        },
    }
}

// When we encounter the missing declaration we replace it with the resolved type.
// We then follow the chain of placeholders to see if their assumptions were correct.
// If not we raise a compile error.
pub fn resolvePlaceholder(self: *Self, placeholder: *obj.ObjTypeDef, resolved_type: *obj.ObjTypeDef, constant: bool) Error!void {
    std.debug.assert(placeholder.def_type == .Placeholder);

    if (BuildOptions.debug_placeholders) {
        io.print("Attempts to resolve @{} ({s}) with @{} a {}({})\n", .{
            @intFromPtr(placeholder),
            if (placeholder.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
            @intFromPtr(resolved_type),
            resolved_type.def_type,
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
                    if (placeholder.resolved_type.?.Placeholder.name != null) placeholder.resolved_type.?.Placeholder.name.?.string else "unknown",
                    @intFromPtr(resolved_type),
                    if (resolved_type.resolved_type.?.Placeholder.name != null) resolved_type.resolved_type.?.Placeholder.name.?.string else "unknown",
                },
            );
        }

        if (resolved_type.resolved_type.?.Placeholder.parent) |parent| {
            if (parent.def_type == .Placeholder) {
                try parent.resolved_type.?.Placeholder.children.append(placeholder);
            } else {
                // Parent already resolved, resolve this now orphan placeholder
                try self.resolvePlaceholderWithRelation(
                    resolved_type,
                    parent,
                    constant,
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

    const placeholder_def = placeholder.resolved_type.?.Placeholder;

    if (BuildOptions.debug_placeholders) {
        io.print(
            "Resolved placeholder @{} {s}({}) with @{}.{}({})\n",
            .{
                @intFromPtr(placeholder),
                if (placeholder.resolved_type.?.Placeholder.name != null) placeholder.resolved_type.?.Placeholder.name.?.string else "unknown",
                placeholder.optional,
                @intFromPtr(resolved_type),
                resolved_type.def_type,
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
    try self.gc.type_registry.setTypeDef(placeholder);

    // Now walk the chain of placeholders and see if they hold up
    for (placeholder_def.children.items) |child| {
        if (child.def_type == .Placeholder) {
            try self.resolvePlaceholderWithRelation(
                child,
                placeholder,
                constant,
                child.resolved_type.?.Placeholder.parent_relation.?,
            );
        }
    }

    // TODO: should resolved_type be freed?
    // TODO: does this work with vm.type_defs? (i guess not)
}

fn addUpvalue(self: *Self, frame: *Frame, index: usize, is_local: bool) Error!usize {
    const upvalue_count: u8 = frame.upvalue_count;

    var i: usize = 0;
    while (i < upvalue_count) : (i += 1) {
        const upvalue: *UpValue = &frame.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return i;
        }
    }

    if (upvalue_count == 255) {
        self.reportError(.closures_count, "Too many closure variables in function.");
        return 0;
    }

    frame.upvalues[upvalue_count].is_local = is_local;
    frame.upvalues[upvalue_count].index = @as(u8, @intCast(index));
    frame.upvalue_count += 1;

    return frame.upvalue_count - 1;
}

fn resolveUpvalue(self: *Self, frame: *Frame, name: Ast.TokenIndex) Error!?usize {
    if (frame.enclosing == null) {
        return null;
    }

    const local: ?usize = try self.resolveLocal(frame.enclosing.?, name);
    if (local) |resolved| {
        frame.enclosing.?.locals[resolved].is_captured = true;
        return try self.addUpvalue(frame, resolved, true);
    }

    const upvalue: ?usize = try self.resolveUpvalue(frame.enclosing.?, name);
    if (upvalue) |resolved| {
        return try self.addUpvalue(frame, resolved, false);
    }

    return null;
}

fn declareVariable(self: *Self, variable_type: *obj.ObjTypeDef, name_token: ?Ast.TokenIndex, constant: bool, check_name: bool) Error!usize {
    const name = name_token orelse self.current_token.? - 1;
    const name_lexeme = self.ast.tokens.items(.lexeme)[name];

    if (self.current.?.scope_depth > 0) {
        // Check a local with the same name doesn't exists
        if (self.current.?.local_count > 0) {
            var i: usize = self.current.?.local_count - 1;
            while (check_name and i >= 0) : (i -= 1) {
                const local: *Local = &self.current.?.locals[i];

                if (local.depth != -1 and local.depth < self.current.?.scope_depth) {
                    break;
                }

                if (!std.mem.eql(u8, name_lexeme, "_") and !std.mem.startsWith(u8, name_lexeme, "$") and std.mem.eql(u8, name_lexeme, local.name.string)) {
                    self.reporter.reportWithOrigin(
                        .variable_already_exists,
                        self.ast.tokens.get(name),
                        self.ast.tokens.get(local.location),
                        "A variable named `{s}` already exists",
                        .{name_lexeme},
                        null,
                    );
                }

                if (i == 0) break;
            }
        }

        return try self.addLocal(name, variable_type, constant);
    } else {
        if (check_name) {
            // Check a global with the same name doesn't exists
            for (self.globals.items, 0..) |*global, index| {
                if (!std.mem.eql(u8, name_lexeme, "_") and std.mem.eql(u8, name_lexeme, global.name.string) and !global.hidden) {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and std.mem.eql(u8, name_lexeme, global.type_def.resolved_type.?.Placeholder.name.?.string)) {
                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @intFromEnum(variable_type.def_type) < @intFromEnum(obj.ObjTypeDef.Type.ObjectInstance)) {
                            if (BuildOptions.debug_placeholders) {
                                io.print(
                                    "Global placeholder @{} resolve with @{} {s} (opt {})\n",
                                    .{
                                        @intFromPtr(global.type_def),
                                        @intFromPtr(variable_type),
                                        (try variable_type.toStringAlloc(self.gc.allocator)).items,
                                        variable_type.optional,
                                    },
                                );
                            }

                            try self.resolvePlaceholder(global.type_def, variable_type, constant);
                        }

                        global.referenced = true;

                        return index;
                    } else if (global.prefix == null) {
                        self.reportError(.variable_already_exists, "A global with the same name already exists.");
                    }
                }
            }
        }

        return try self.addGlobal(name, variable_type, constant);
    }
}

fn parseVariable(
    self: *Self,
    identifier_consumed: bool,
    variable_type: *obj.ObjTypeDef,
    constant: bool,
    error_message: []const u8,
) !usize {
    if (!identifier_consumed) {
        try self.consume(.Identifier, error_message);
    }

    return try self.declareVariable(
        variable_type,
        null,
        constant,
        true,
    );
}

inline fn markInitialized(self: *Self) void {
    if (self.current.?.scope_depth == 0) {
        // assert(!self.globals.items[self.globals.items.len - 1].initialized);
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
        var placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, name),
        };
        placeholder_resolved_type.Placeholder.name = try self.gc.copyString(self.ast.tokens.items(.lexeme)[name]);

        placeholder_type = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = placeholder_resolved_type,
            },
        );
    }

    std.debug.assert(!placeholder_type.optional);

    const global = try self.addGlobal(
        name,
        placeholder_type,
        false,
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
    generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef),
    instance: bool,
) Error!Ast.Node.Index {
    if (try self.match(.Str)) {
        const optional = try self.match(.Question);

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
    } else if (try self.match(.Float)) {
        const optional = try self.match(.Question);

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = self.current_token.? - 1,
                .end_location = self.current_token.? - 1,
                .type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .optional = optional,
                        .def_type = .Float,
                    },
                ),
                .components = .{
                    .SimpleType = {},
                },
            },
        );
    } else if (try self.match(.Bool)) {
        const optional = try self.match(.Question);

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
        return self.parseListType(generic_types);
    } else if (try self.match(.LeftBrace)) {
        return self.parseMapType(generic_types);
    } else if (try self.match(.Function) or try self.match(.Extern)) {
        return try self.parseFunctionType(generic_types);
    } else if (try self.match(.Fib)) {
        return try self.parseFiberType(generic_types);
    } else if (try self.match(.Obj)) {
        const type_def_node = try self.parseObjType(generic_types);
        if (instance) {
            self.ast.nodes.items(.type_def)[type_def_node] = try self.ast.nodes.items(.type_def)[type_def_node].?.toInstance(
                self.gc.allocator,
                &self.gc.type_registry,
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
            user_type_node = try self.parseUserType(instance);
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
        self.reportErrorAtCurrent(.syntax, "Expected type definition.");

        return self.ast.appendNode(
            .{
                .tag = .SimpleType,
                .location = undefined,
                .end_location = undefined,
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

fn parseFiberType(self: *Self, generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.Less, "Expected `<` after `fib`");
    const return_type = try self.parseTypeDef(generic_types, true);
    try self.consume(.Comma, "Expected `,` after fiber return type");
    const yield_type = try self.parseTypeDef(generic_types, true);

    const yield_type_def = self.ast.nodes.items(.type_def)[yield_type].?;
    if (!yield_type_def.optional and yield_type_def.def_type != .Void) {
        self.reportError(.yield_type, "Expected optional type or void");
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

fn parseListType(self: *Self, generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const item_type = try self.parseTypeDef(generic_types, true);

    try self.consume(.RightBracket, "Expected `]` after list type.");

    const list_type_def = try self.gc.type_registry.getTypeDef(
        .{
            .optional = try self.match(.Question),
            .def_type = .List,
            .resolved_type = .{
                .List = obj.ObjList.ListDef.init(
                    self.gc.allocator,
                    self.ast.nodes.items(.type_def)[item_type].?,
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
                .ListType = .{
                    .item_type = item_type,
                },
            },
        },
    );
}

fn parseMapType(self: *Self, generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
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
                            self.gc.allocator,
                            type_defs[key_type].?,
                            type_defs[value_type].?,
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

fn parseFunctionType(self: *Self, parent_generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const tag = self.ast.tokens.items(.tag)[start_location];

    std.debug.assert(tag == .Function or tag == .Extern);

    const is_extern = tag == .Extern;

    if (is_extern) {
        try self.consume(.Function, "Expected `Function` after `extern`.");
    }

    var name_token: ?Ast.TokenIndex = null;
    var name: ?*obj.ObjString = null;
    if (try self.match(.Identifier)) {
        name_token = self.current_token.? - 1;
        name = try self.gc.copyString(self.ast.tokens.items(.lexeme)[self.current_token.? - 1]);
    }

    var merged_generic_types = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator);
    defer merged_generic_types.deinit();
    if (parent_generic_types != null) {
        var it = parent_generic_types.?.iterator();
        while (it.next()) |kv| {
            try merged_generic_types.put(kv.key_ptr.*, kv.value_ptr.*);
        }
    }

    var generic_types_list = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    defer generic_types_list.shrinkAndFree(generic_types_list.items.len);
    // To avoid duplicates
    var generic_types = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator);
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
                    try self.gc.copyString(generic_identifier_lexeme),
                    type_def,
                );

                try merged_generic_types.put(
                    try self.gc.copyString(generic_identifier_lexeme),
                    type_def,
                );

                try generic_types_list.append(
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
                self.reportErrorFmt(
                    .generic_type,
                    "Generic type `{s}` already defined",
                    .{self.ast.tokens.items(.lexeme)[self.current_token.? - 1]},
                );
            }

            if (!self.check(.Greater)) {
                try self.consume(.Comma, "Expected `,` between generic types");
            }
        }

        if (generic_types.count() == 0) {
            self.reportError(.generic_type, "Expected at least one generic type");
        }

        try self.consume(.Greater, "Expected `>` after generic types list");
    }

    try self.consume(.LeftParen, "Expected `(` after function name.");

    var arguments = std.ArrayList(Ast.FunctionType.Argument).init(self.gc.allocator);
    defer arguments.shrinkAndFree(arguments.items.len);
    var parameters = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator);
    var defaults = std.AutoArrayHashMap(*obj.ObjString, Value).init(self.gc.allocator);
    var arity: usize = 0;
    if (!self.check(.RightParen)) {
        while (true) {
            arity += 1;
            if (arity > 255) {
                self.reportErrorAtCurrent(.arguments_count, "Can't have more than 255 arguments.");
            }

            const arg_type = try self.parseTypeDef(
                merged_generic_types,
                true,
            );
            const arg_type_def = self.ast.nodes.items(.type_def)[arg_type];

            try self.consume(.Identifier, "Expected argument name");

            const arg_name_token = self.current_token.? - 1;
            const arg_name = self.ast.tokens.items(.lexeme)[self.current_token.? - 1];

            var default: ?Ast.Node.Index = null;
            if (try self.match(.Equal)) {
                const expr = try self.expression(false);
                const expr_type_def = self.ast.nodes.items(.type_def)[expr];

                if (expr_type_def != null and expr_type_def.?.def_type == .Placeholder and arg_type_def.?.def_type == .Placeholder) {
                    try obj.PlaceholderDef.link(
                        arg_type_def.?,
                        expr_type_def.?,
                        .Assignment,
                    );
                }

                if (!self.ast.isConstant(expr)) {
                    self.reportError(
                        .constant_default,
                        "Default parameters must be constant values.",
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
                try self.ast.toValue(dflt, self.gc)
            else if (arg_type_def.?.optional)
                Value.Null
            else
                null) |dflt|
            {
                try defaults.put(
                    try self.gc.copyString(arg_name),
                    dflt,
                );
            }
            try parameters.put(try self.gc.copyString(arg_name), arg_type_def.?);

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
    defer error_types_list.shrinkAndFree(error_types_list.items.len);
    var error_types = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
    defer error_types.shrinkAndFree(error_types.items.len);
    if (try self.match(.BangGreater)) {
        while (!self.check(.Eof)) {
            const error_type = try self.parseTypeDef(generic_types, true);
            const error_type_def = self.ast.nodes.items(.type_def)[error_type].?;
            try error_types_list.append(error_type);
            try error_types.append(error_type_def);

            if (error_type_def.optional) {
                self.reportError(.error_type, "Error type can't be optional");
            }

            if (!self.check(.Comma)) {
                break;
            }
        }
    }

    var function_typedef: obj.ObjTypeDef = .{
        .def_type = .Function,
        .optional = try self.match(.Question),
    };
    const function_def: obj.ObjFunction.FunctionDef = .{
        .id = obj.ObjFunction.FunctionDef.nextId(),
        .script_name = try self.gc.copyString(self.script_name),
        .name = name orelse try self.gc.copyString("anonymous"),
        .return_type = if (return_type) |rt|
            try self.ast.nodes.items(.type_def)[rt].?.toInstance(self.gc.allocator, &self.gc.type_registry)
        else
            self.gc.type_registry.void_type,
        .yield_type = if (yield_type) |yt|
            try self.ast.nodes.items(.type_def)[yt].?.toInstance(self.gc.allocator, &self.gc.type_registry)
        else
            self.gc.type_registry.void_type,
        .parameters = parameters,
        .defaults = defaults,
        .function_type = if (is_extern) .Extern else .Anonymous,
        .generic_types = generic_types,
        .error_types = if (error_types.items.len > 0) error_types.items else null,
    };
    const function_resolved_type: obj.ObjTypeDef.TypeUnion = .{ .Function = function_def };

    function_typedef.resolved_type = function_resolved_type;

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
                    .arguments = arguments.items,
                    .error_types = error_types_list.items,
                    .generic_types = generic_types_list.items,
                    .return_type = return_type,
                    .yield_type = yield_type,
                },
            },
        },
    );
}

// Only used to parse anonymouse object type
fn parseObjType(self: *Self, generic_types: ?std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef)) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftBrace, "Expected `{` after `obj`");

    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
    defer self.gc.allocator.free(qualifier);
    var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_name.deinit();
    try qualified_name.writer().print("{s}.anonymous", .{qualifier});

    const object_def = obj.ObjObject.ObjectDef.init(
        self.gc.allocator,
        self.ast.tokens.get(start_location),
        try self.gc.copyString("anonymous"),
        try self.gc.copyString(qualified_name.items),
        true,
    );
    const resolved_type = obj.ObjTypeDef.TypeUnion{ .Object = object_def };
    var object_type = obj.ObjTypeDef{
        .def_type = .Object,
        .resolved_type = resolved_type,
    };

    // Anonymous object can only have properties without default values (no methods, no static fields)
    // They can't self reference since their anonymous
    var field_names = std.StringHashMap(void).init(self.gc.allocator);
    defer field_names.deinit();
    var fields = std.ArrayList(Ast.AnonymousObjectType.Field).init(self.gc.allocator);
    defer fields.shrinkAndFree(fields.items.len);
    var tuple_index: u8 = 0;
    var obj_is_tuple = false;
    var obj_is_not_tuple = false;
    var property_idx: usize = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) : (property_idx += 1) {
        const constant = try self.match(.Const);

        const property_type = try self.parseTypeDef(generic_types, true);

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

        if (is_tuple) {
            obj_is_tuple = true;

            if (obj_is_not_tuple) {
                self.reportErrorAtCurrent(
                    .mix_tuple,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                );
            }

            if (tuple_index >= 4) {
                self.reportErrorAtCurrent(
                    .tuple_limit,
                    "Tuples can't have more than 4 elements",
                );
            }

            tuple_index += 1;
        } else {
            obj_is_not_tuple = true;

            if (obj_is_tuple) {
                self.reportErrorAtCurrent(
                    .mix_tuple,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                );
            }
        }

        if (!is_tuple and field_names.get(property_name_lexeme) != null) {
            self.reportError(.property_already_exists, "A property with that name already exists.");
        }

        if (!self.check(.RightBrace) or self.check(.Comma)) {
            try self.consume(.Comma, "Expected `,` after property definition.");
        }
        try object_type.resolved_type.?.Object.fields.put(
            property_name_lexeme,
            .{
                .name = property_name_lexeme,
                .type_def = self.ast.nodes.items(.type_def)[property_type].?,
                .location = self.ast.tokens.get(property_name),
                .constant = constant,
                .static = false,
                .method = false,
                .has_default = false,
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

    return try self.ast.appendNode(
        .{
            .tag = .AnonymousObjectType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try self.gc.type_registry.getTypeDef(object_type),
            .components = .{
                .AnonymousObjectType = .{
                    .fields = fields.items,
                },
            },
        },
    );
}

fn parseUserType(self: *Self, instance: bool) Error!Ast.Node.Index {
    const user_type_name = self.current_token.? - 1;
    var var_type: ?*obj.ObjTypeDef = null;
    var global_slot: ?usize = null;

    // Search for a global with that name
    if (try self.resolveGlobal(null, self.ast.tokens.items(.lexeme)[user_type_name])) |slot| {
        const global = self.globals.items[slot];

        var_type = global.type_def;
        global_slot = @intCast(slot);

        if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
            const imported_from = global.imported_from.?;

            try self.script_imports.put(
                imported_from,
                .{
                    .location = self.script_imports.get(imported_from).?.location,
                    .referenced = true,
                },
            );
        }
    }

    // If none found, create a placeholder
    if (var_type == null) {
        var placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, user_type_name),
        };

        placeholder_resolved_type.Placeholder.name = try self.gc.copyString(self.ast.tokens.items(.lexeme)[user_type_name]);

        var_type = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = placeholder_resolved_type,
            },
        );

        global_slot = try self.declarePlaceholder(user_type_name, var_type.?);
    }

    // Concrete generic types list
    var resolved_generics = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
    defer resolved_generics.shrinkAndFree(resolved_generics.items.len);
    var generic_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    defer generic_nodes.shrinkAndFree(generic_nodes.items.len);
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
            self.reportErrorAtCurrent(.generic_type, "Expected at least one type");
        }

        var_type = try var_type.?.populateGenerics(
            self.current_token.? - 1,
            var_type.?.resolved_type.?.Object.id,
            resolved_generics.items,
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
                    .GenericResolveType = .{
                        .resolved_types = generic_nodes.items,
                    },
                },
            },
        );
    } else null;

    return try self.ast.appendNode(
        .{
            .tag = .UserType,
            .location = user_type_name,
            .end_location = self.current_token.? - 1,
            .type_def = if (instance)
                try var_type.?.toInstance(self.gc.allocator, &self.gc.type_registry)
            else
                var_type.?,
            .components = .{
                .UserType = .{
                    .generic_resolve = generic_resolve,
                    .identifier = user_type_name,
                },
            },
        },
    );
}

fn parseGenericResolve(self: *Self, callee_type_def: *obj.ObjTypeDef, expr: ?Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var resolved_generics = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    defer resolved_generics.shrinkAndFree(resolved_generics.items.len);
    var resolved_generics_types = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
    defer resolved_generics_types.shrinkAndFree(resolved_generics_types.items.len);

    try self.consume(.Less, "Expected `<` at start of generic types list");

    while (!self.check(.Greater) and !self.check(.Eof)) {
        const resolved_generic = try self.parseTypeDef(null, true);

        if (callee_type_def.def_type == .Any) {
            self.reportError(.any_generic, "`any` not allowed as generic type");
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
                resolved_generics_types.items,
                &self.gc.type_registry,
                null,
            ),
            .components = if (expr) |e| .{
                .GenericResolve = e,
            } else .{
                .GenericResolveType = .{
                    .resolved_types = resolved_generics.items,
                },
            },
        },
    );
}

fn subscript(self: *Self, can_assign: bool, subscripted: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    const subscript_type_def = self.ast.nodes.items(.type_def)[subscripted];
    const index = try self.expression(false);
    const index_type_def = self.ast.nodes.items(.type_def)[index];

    const type_defs = self.ast.nodes.items(.type_def);
    if (subscript_type_def.?.def_type == .Placeholder and index_type_def.?.def_type == .Placeholder) {
        try obj.PlaceholderDef.link(
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
                    const placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
                        .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
                    };
                    const placeholder = try self.gc.type_registry.getTypeDef(
                        .{
                            .def_type = .Placeholder,
                            .resolved_type = placeholder_resolved_type,
                        },
                    );

                    try obj.PlaceholderDef.link(type_def, placeholder, .Subscript);

                    subscripted_type_def = placeholder;
                },
                .String => subscripted_type_def = type_def,
                .List => subscripted_type_def = type_def.resolved_type.?.List.item_type,
                .Map => subscripted_type_def = try type_def.resolved_type.?.Map.value_type.cloneOptional(&self.gc.type_registry),
                else => self.reportErrorFmt(
                    .subscriptable,
                    "Type `{s}` is not subscriptable",
                    .{(try type_def.toStringAlloc(self.gc.allocator)).items},
                ),
            }
        } else {
            self.reportError(.subscriptable, "Optional type is not subscriptable");
        }
    }

    try self.consume(.RightBracket, "Expected `]`.");

    var value: ?Ast.Node.Index = null;
    if (can_assign and (subscript_type_def == null or subscript_type_def.?.def_type != .String) and try self.match(.Equal)) {
        value = try self.expression(false);
        const value_type_def = self.ast.nodes.items(.type_def)[value.?];

        if (subscript_type_def.?.def_type == .Placeholder and value_type_def.?.def_type == .Placeholder) {
            try obj.PlaceholderDef.link(subscript_type_def.?, value_type_def.?, .Subscript);
        }
    }

    return try self.ast.appendNode(
        .{
            .tag = .Subscript,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = subscripted_type_def,
            .components = .{
                .Subscript = .{
                    .index = index,
                    .value = value,
                    .subscripted = subscripted,
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
        while (!(try self.match(.RightBracket)) and !(try self.match(.Eof))) {
            const actual_item = try self.expression(false);

            try items.append(actual_item);

            if (item_type == null) {
                if (common_type == null) {
                    common_type = self.ast.nodes.items(.type_def)[actual_item];
                } else if (self.ast.nodes.items(.type_def)[actual_item]) |actual_type_def| {
                    if (!common_type.?.eql(actual_type_def)) {
                        if (common_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_type = common_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object) orelse common_type;
                            common_type = try common_type.?.toInstance(self.gc.allocator, &self.gc.type_registry);
                        } else {
                            common_type = self.gc.type_registry.any_type;
                        }
                    }
                }
            }

            if (!self.check(.RightBracket)) {
                try self.consume(.Comma, "Expected `,` after list item.");
            }
        }

        if (items.items.len > std.math.maxInt(u24)) {
            self.reportErrorAtCurrent(.syntax, "Too many elements in list initialization");
        }

        if (self.ast.tokens.items(.tag)[self.current_token.? - 1] != .RightBracket) {
            self.reportErrorAtCurrent(.syntax, "Expected `]`");
        }

        item_type = item_type orelse common_type;
    } else {
        try self.consume(.RightBracket, "Expected `]`");
    }

    const list_def = obj.ObjList.ListDef.init(
        self.gc.allocator,
        item_type orelse self.gc.type_registry.any_type,
    );

    const resolved_type = obj.ObjTypeDef.TypeUnion{ .List = list_def };

    const list_type = try self.gc.type_registry.getTypeDef(
        .{
            .def_type = .List,
            .resolved_type = resolved_type,
        },
    );

    return self.ast.appendNode(
        .{
            .tag = .List,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = list_type,
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
            node.tag = .Float;
            node.components = .{
                .Float = self.ast.tokens.items(.literal_float)[node.location].?,
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
    defer arguments.shrinkAndFree(arguments.items.len);

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
            self.reportError(.syntax, "Expected argument name.");
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

        if (arg_count == 255) {
            self.reportError(.arguments_count, "Can't have more than 255 arguments.");

            return arguments.items;
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
        try (try callee_type_def.?.toInstance(self.gc.allocator, &self.gc.type_registry)).cloneOptional(&self.gc.type_registry)
    else
        null;

    // If null, create placeholder
    if (type_def == null) {
        if (callee_type_def == null or callee_type_def.?.def_type != .Placeholder) {
            self.reporter.reportErrorAt(
                .callable,
                self.ast.tokens.get(self.ast.nodes.items(.location)[callee]),
                "Can't be called",
            );

            type_def = self.gc.type_registry.void_type;
        } else {
            const placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
                .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, start_location),
            };

            type_def = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            try obj.PlaceholderDef.link(
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
                        std.debug.assert(self.reporter.had_error);

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
    defer entries.shrinkAndFree(entries.items.len);
    if (key_type_node == null or try self.match(.Comma)) {
        var common_key_type: ?*obj.ObjTypeDef = null;
        var common_value_type: ?*obj.ObjTypeDef = null;
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
                        if (common_key_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_key_type = common_key_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(
                                actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object,
                            ) orelse common_key_type;

                            common_key_type = try common_key_type.?.toInstance(
                                self.gc.allocator,
                                &self.gc.type_registry,
                            );
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
                        if (common_value_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_value_type = common_value_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(
                                actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object,
                            ) orelse common_value_type;

                            common_value_type = try common_value_type.?.toInstance(
                                self.gc.allocator,
                                &self.gc.type_registry,
                            );
                        } else {
                            common_value_type = self.gc.type_registry.any_type;
                        }
                    }
                }
            }

            if (!self.check(.RightBrace)) {
                try self.consume(.Comma, "Expected `,` after map entry.");
            }
        }

        if (entries.items.len > std.math.maxInt(u24)) {
            self.reportErrorAtCurrent(.syntax, "Too many entries in map initialization");
        }

        key_type_def = key_type_def orelse common_key_type;
        value_type_def = value_type_def orelse common_value_type;
    } else {
        try self.consume(.RightBrace, "Expected `}`");
    }

    const map_def = obj.ObjMap.MapDef.init(
        self.gc.allocator,
        key_type_def orelse self.gc.type_registry.any_type,
        value_type_def orelse self.gc.type_registry.any_type,
    );
    const resolved_type = obj.ObjTypeDef.TypeUnion{ .Map = map_def };
    const map_type = try self.gc.type_registry.getTypeDef(
        .{
            .optional = try self.match(.Question),
            .def_type = .Map,
            .resolved_type = resolved_type,
        },
    );

    return self.ast.appendNode(
        .{
            .tag = .Map,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = map_type,
            .components = .{
                .Map = .{
                    .explicit_key_type = key_type_node,
                    .explicit_value_type = value_type_node,
                    .entries = entries.items,
                },
            },
        },
    );
}

fn objectInit(self: *Self, _: bool, object: Ast.Node.Index) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const obj_type_def = self.ast.nodes.items(.type_def)[object];
    var properties = std.ArrayList(Ast.ObjectInit.Property).init(self.gc.allocator);
    defer properties.shrinkAndFree(properties.items.len);
    var property_names = std.StringHashMap(Ast.Node.Index).init(self.gc.allocator);
    defer property_names.deinit();

    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        try self.consume(.Identifier, "Expected property name");

        const property_name = self.current_token.? - 1;
        const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];
        if (property_names.get(property_name_lexeme)) |previous_decl| {
            self.reporter.reportWithOrigin(
                .property_already_exists,
                self.ast.tokens.get(property_name),
                self.ast.tokens.get(previous_decl),
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
            var placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
                .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
            };
            placeholder_resolved_type.Placeholder.name = try self.gc.copyString(
                self.ast.tokens.items(.lexeme)[property_name],
            );

            property_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            try obj.PlaceholderDef.link(
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
                try type_def.toInstance(self.gc.allocator, &self.gc.type_registry)
            else
                null,
            .components = .{
                .ObjectInit = .{
                    .object = object,
                    .properties = properties.items,
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
        self.gc.allocator,
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
    defer properties.shrinkAndFree(properties.items.len);
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
                    self.reportErrorAtCurrent(
                        .mix_tuple,
                        "Can't mix tuple notation and regular properties in anonymous object initialization",
                    );
                }
            }

            if (is_tuple and tuple_index >= 4) {
                self.reportErrorAtCurrent(
                    .tuple_limit,
                    "Tuples can't have more than 4 elements",
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
                property_name_lexeme,
                .{
                    .name = property_name_lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[expr].?,
                    .location = self.ast.tokens.get(property_name),
                    .static = false,
                    .method = false,
                    .constant = false,
                    .has_default = false,
                    .index = property_idx,
                },
            );
        } else {
            try self.consume(.Identifier, "Expected property name");

            obj_is_not_tuple = true;

            if (obj_is_tuple) {
                self.reportErrorAtCurrent(
                    .mix_tuple,
                    "Can't mix tuple notation and regular properties in anonymous object initialization",
                );
            }

            const property_name = self.current_token.? - 1;
            const property_name_lexeme = self.ast.tokens.items(.lexeme)[property_name];
            if (property_names.get(property_name_lexeme)) |previous_decl| {
                self.reporter.reportWithOrigin(
                    .property_already_exists,
                    self.ast.tokens.get(property_name),
                    self.ast.tokens.get(previous_decl),
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
                property_name_lexeme,
                .{
                    .name = property_name_lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[expr].?,
                    .location = self.ast.tokens.get(property_name),
                    .static = false,
                    .method = false,
                    .constant = false,
                    .has_default = false,
                    .index = property_idx,
                },
            );
        }

        if (!self.check(.RightBrace) or self.check(.Comma)) {
            try self.consume(.Comma, "Expected `,` after field initialization.");
        }
    }

    try self.consume(.RightBrace, "Expected `}` after object initialization.");

    return try self.ast.appendNode(
        .{
            .tag = .ObjectInit,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = try (try self.gc.type_registry.getTypeDef(object_type)).toInstance(
                self.gc.allocator,
                &self.gc.type_registry,
            ),
            .components = .{
                .ObjectInit = .{
                    .object = null,
                    .properties = properties.items,
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
                    self.reportError(.property_does_not_exists, "String property doesn't exist.");
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
                    self.reportError(.property_does_not_exists, "Range property doesn't exist.");
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
                    self.reportError(.property_does_not_exists, "Pattern property doesn't exist.");
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
                    self.reportError(.property_does_not_exists, "Fiber property doesn't exist.");
                }
            },
            .Object => {
                const obj_def: obj.ObjObject.ObjectDef = callee_type_def.?.resolved_type.?.Object;
                const property_field = obj_def.fields.get(member_name);
                var property_type = if (property_field) |field| field.type_def else null;

                // Not found, create a placeholder, this is a root placeholder not linked to anything
                // TODO: test with something else than a name
                if (property_type == null and self.current_object != null and std.mem.eql(
                    u8,
                    self.current_object.?.name.lexeme,
                    obj_def.name.string,
                )) {
                    var placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
                        .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, member_name_token),
                    };

                    placeholder_resolved_type.Placeholder.name = try self.gc.copyString(
                        self.ast.tokens.items(.lexeme)[member_name_token],
                    );

                    const placeholder: *obj.ObjTypeDef = try self.gc.type_registry.getTypeDef(
                        .{
                            .optional = false,
                            .def_type = .Placeholder,
                            .resolved_type = placeholder_resolved_type,
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
                    try callee_type_def.?.resolved_type.?.Object.static_placeholders.put(member_name, placeholder);

                    property_type = placeholder;
                } else if (property_type == null) {
                    self.reportErrorFmt(
                        .property_does_not_exists,
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
                if (can_assign and try self.match(.Equal)) {
                    components[dot_node].Dot.member_kind = .Value;
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = try self.expression(false),
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
                    if (can_assign and try self.match(.Equal)) {
                        const components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_kind = .Value;
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .Value = try self.expression(false),
                        };
                    } else {
                        self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                    }

                    self.ast.nodes.items(.type_def)[dot_node] = field;
                } else {
                    self.reportErrorFmt(
                        .property_does_not_exists,
                        "Property `{s}` does not exists in object `{s}`",
                        .{
                            member_name,
                            f_def.name.string,
                        },
                    );
                }
            },
            .ObjectInstance => {
                const object = callee_type_def.?.resolved_type.?.ObjectInstance;
                const obj_def = object.resolved_type.?.Object;

                const property_field = obj_def.fields.get(member_name);
                var property_type = (if (property_field) |field| field.type_def else null) orelse obj_def.placeholders.get(member_name);

                // Else create placeholder
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    var placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
                        .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, member_name_token),
                    };
                    placeholder_resolved_type.Placeholder.name = try self.gc.copyString(member_name);

                    const placeholder = try self.gc.type_registry.getTypeDef(
                        .{
                            .optional = false,
                            .def_type = .Placeholder,
                            .resolved_type = placeholder_resolved_type,
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
                    try object.resolved_type.?.Object.placeholders.put(member_name, placeholder);

                    property_type = placeholder;
                } else if (property_type == null) {
                    self.reportErrorFmt(
                        .property_does_not_exists,
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
                // TODO: here get info that field is constant or not
                var components = self.ast.nodes.items(.components);
                if (can_assign and try self.match(.Equal)) {
                    components[dot_node].Dot.member_kind = .Value;
                    const expr = try self.expression(false);
                    components = self.ast.nodes.items(.components);
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = expr,
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
                const protocol = callee_type_def.?.resolved_type.?.ProtocolInstance;
                const protocol_def = protocol.resolved_type.?.Protocol;

                var method_type = protocol_def.methods.get(member_name);

                // Else create placeholder
                if (method_type == null) {
                    self.reportErrorFmt(
                        .property_does_not_exists,
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

                for (enum_def.cases.items, 0..) |case, index| {
                    if (std.mem.eql(u8, case, member_name)) {
                        const enum_instance_resolved_type = obj.ObjTypeDef.TypeUnion{
                            .EnumInstance = callee_type_def.?,
                        };

                        const enum_instance = try self.gc.type_registry.getTypeDef(
                            .{
                                .optional = false,
                                .def_type = .EnumInstance,
                                .resolved_type = enum_instance_resolved_type,
                            },
                        );

                        self.ast.nodes.items(.type_def)[dot_node] = enum_instance;
                        const components = self.ast.nodes.items(.components);
                        components[dot_node].Dot.member_kind = .EnumCase;
                        components[dot_node].Dot.value_or_call_or_enum = .{
                            .EnumCase = @intCast(index),
                        };
                        break;
                    }
                }

                if (self.ast.nodes.items(.type_def)[dot_node] == null) {
                    // TODO: reportWithOrigin
                    self.reportErrorFmt(
                        .enum_case,
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
                    self.reportError(.property_does_not_exists, "Enum provides only field `value`.");
                }

                self.ast.nodes.items(.components)[dot_node].Dot.member_kind = .Ref;
                self.ast.nodes.items(.type_def)[dot_node] = callee_type_def.?.resolved_type.?.EnumInstance.resolved_type.?.Enum.enum_type;
            },
            .List => {
                if (try obj.ObjList.ListDef.member(callee_type_def.?, self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    self.ast.nodes.items(.components)[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

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
                    self.reportError(.property_does_not_exists, "List property doesn't exist.");
                }
            },
            .Map => {
                if (try obj.ObjMap.MapDef.member(callee_type_def.?, self, member_name)) |member_type_def| {
                    const generic_resolve = if (try self.match(.DoubleColon))
                        try self.parseGenericResolve(member_type_def, null)
                    else
                        null;

                    var components = self.ast.nodes.items(.components);
                    components[dot_node].Dot.generic_resolve = generic_resolve;

                    const member = if (generic_resolve) |gr|
                        self.ast.nodes.items(.type_def)[gr]
                    else
                        member_type_def;

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
                    self.reportError(.property_does_not_exists, "Map property doesn't exist.");
                }
            },
            .Placeholder => {
                // We know nothing of the field
                var placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
                    .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, member_name_token),
                };

                placeholder_resolved_type.Placeholder.name = try self.gc.copyString(member_name);

                var placeholder = try self.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    },
                );

                try obj.PlaceholderDef.link(
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

                if (can_assign and try self.match(.Equal)) {
                    components[dot_node].Dot.member_kind = .Value;
                    const expr = try self.expression(false);
                    components = self.ast.nodes.items(.components); // ptr might have been invalidated
                    components[dot_node].Dot.value_or_call_or_enum = .{
                        .Value = expr,
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
                self.reporter.reportErrorFmt(
                    .field_access,
                    self.ast.tokens.get(self.ast.nodes.items(.location)[callee]),
                    "`{s}` is not field accessible",
                    .{
                        (try callee_type_def.?.toStringAlloc(self.gc.allocator)).items,
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
                        .unwrapped = unwrapped,
                        .original_type = unwrapped_type_def,
                    },
                }
            else
                .{
                    .Unwrap = .{
                        .unwrapped = unwrapped,
                        .original_type = unwrapped_type_def,
                    },
                },
        },
    );

    if (!force) {
        self.opt_jumps = self.opt_jumps orelse std.ArrayList(Precedence).init(self.gc.allocator);

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
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `if`.");

    try self.beginScope(null);
    const condition = try self.expression(false);
    const condition_type_def = self.ast.nodes.items(.type_def)[condition];

    var unwrapped_identifier: ?Ast.TokenIndex = null;
    var casted_type: ?Ast.Node.Index = null;
    if (try self.match(.Arrow)) { // if (opt -> unwrapped)
        _ = try self.parseVariable(
            false,
            try condition_type_def.?.cloneNonOptional(&self.gc.type_registry),
            true,
            "Expected optional unwrap identifier",
        );
        self.markInitialized();

        unwrapped_identifier = self.current_token.? - 1;
    } else if (try self.match(.As)) { // if (expr as casted)
        casted_type = try self.parseTypeDef(null, true);

        _ = try self.parseVariable(
            false,
            try self.ast.nodes.items(.type_def)[casted_type.?].?.toInstance(self.gc.allocator, &self.gc.type_registry),
            true,
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

    return self.ast.appendNode(
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

                const is_optional = if_type_def.?.optional or body_type_def.?.optional or else_branch_type_def.?.optional or body_type_def.?.def_type == .Void or else_branch_type_def.?.def_type == .Void;
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
}

fn ifStatement(self: *Self, loop_scope: ?LoopScope) Error!Ast.Node.Index {
    return try self.@"if"(true, loop_scope);
}

fn inlineIf(self: *Self, _: bool) Error!Ast.Node.Index {
    return try self.@"if"(false, null);
}

inline fn isAs(self: *Self, left: Ast.Node.Index, is_expr: bool) Error!Ast.Node.Index {
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

fn namedVariable(self: *Self, name: Ast.TokenIndex, can_assign: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    var var_def: ?*obj.ObjTypeDef = null;
    var slot: usize = undefined;
    var slot_type: Ast.SlotType = undefined;
    var slot_constant = false;
    if (try self.resolveLocal(self.current.?, name)) |uslot| {
        var_def = self.current.?.locals[uslot].type_def;
        slot = uslot;
        slot_type = .Local;
        slot_constant = self.current.?.locals[uslot].constant;
    } else if (try self.resolveUpvalue(self.current.?, name)) |uslot| {
        var_def = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].type_def;
        slot = uslot;
        slot_type = .UpValue;
        slot_constant = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].constant;
    } else if (try self.resolveGlobal(null, self.ast.tokens.items(.lexeme)[name])) |uslot| {
        const global = self.globals.items[uslot];

        var_def = global.type_def;
        slot = uslot;
        slot_type = .Global;
        slot_constant = global.constant;

        if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
            const imported_from = global.imported_from.?;

            try self.script_imports.put(
                imported_from,
                .{
                    .location = self.script_imports.get(imported_from).?.location,
                    .referenced = true,
                },
            );
        }
    } else {
        slot = try self.declarePlaceholder(name, null);
        var_def = self.globals.items[slot].type_def;
        slot_type = .Global;
    }

    const value = if (can_assign and try self.match(.Equal))
        try self.expression(false)
    else
        null;

    if (value != null and slot_constant) {
        self.reportError(.constant, "Can't assign to constant variable");
    }

    return try self.ast.appendNode(
        .{
            .tag = .NamedVariable,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = var_def,
            .components = .{
                .NamedVariable = .{
                    .identifier = name,
                    .value = value,
                    .slot = @intCast(slot),
                    .slot_type = slot_type,
                    .slot_constant = slot_constant,
                },
            },
        },
    );
}

fn variable(self: *Self, can_assign: bool) Error!Ast.Node.Index {
    return try self.namedVariable(self.current_token.? - 1, can_assign);
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
    defer error_types.shrinkAndFree(error_types.items.len);
    var arguments = std.ArrayList(Ast.FunctionType.Argument).init(self.gc.allocator);
    defer arguments.shrinkAndFree(arguments.items.len);
    var generic_types = std.ArrayList(Ast.TokenIndex).init(self.gc.allocator);
    defer generic_types.shrinkAndFree(generic_types.items.len);

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
                    .upvalue_binding = std.AutoArrayHashMap(u8, bool).init(self.gc.allocator),
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
        .parameters = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator),
        .defaults = std.AutoArrayHashMap(*obj.ObjString, Value).init(self.gc.allocator),
        .function_type = function_type,
        .generic_types = std.AutoArrayHashMap(*obj.ObjString, *obj.ObjTypeDef).init(self.gc.allocator),
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
                    self.reportErrorFmt(
                        .generic_type,
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

            if (function_typedef.resolved_type.?.Function.generic_types.count() == 0) {
                self.reportError(
                    .generic_type,
                    "Expected at least one generic type",
                );
            }

            try self.consume(.Greater, "Expected `>` after generic types list");
        }

        self.ast.nodes.items(.components)[function_signature].FunctionType.generic_types = generic_types.items;

        try self.consume(.LeftParen, "Expected `(` after function name.");

        // Arguments
        var arity: usize = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                arity += 1;
                if (arity > 255) {
                    self.reportErrorAtCurrent(
                        .arguments_count,
                        "Can't have more than 255 arguments.",
                    );
                }

                const argument_type_node = try self.parseTypeDef(
                    function_typedef.resolved_type.?.Function.generic_types,
                    true,
                );

                self.ast.nodes.items(.type_def)[argument_type_node] = try (self.ast.nodes.items(.type_def)[
                    argument_type_node
                ]).?.toInstance(
                    self.gc.allocator,
                    &self.gc.type_registry,
                );

                const argument_type = self.ast.nodes.items(.type_def)[argument_type_node].?;

                const slot = try self.parseVariable(
                    false,
                    argument_type,
                    true, // function arguments are constant
                    "Expected argument name",
                );

                std.debug.assert(self.current.?.scope_depth > 0);
                const local = self.current.?.locals[slot];

                try function_typedef.resolved_type.?.Function.parameters.put(local.name, local.type_def);

                self.markInitialized();

                // Default arguments
                const default = switch (function_type) {
                    .Function, .Method, .Anonymous, .Extern => value: {
                        if (try self.match(.Equal)) {
                            const expr = try self.expression(false);
                            const expr_type_def = self.ast.nodes.items(.type_def)[expr];

                            if (expr_type_def != null and expr_type_def.?.def_type == .Placeholder and argument_type.def_type == .Placeholder) {
                                try obj.PlaceholderDef.link(
                                    argument_type,
                                    expr_type_def.?,
                                    .Assignment,
                                );
                            }

                            break :value expr;
                        } else if (argument_type.optional) {
                            try function_typedef.resolved_type.?.Function.defaults.put(
                                local.name,
                                Value.Null,
                            );
                        }

                        break :value null;
                    },
                    else => null,
                };

                try arguments.append(
                    .{
                        .name = local.name_token,
                        .type = argument_type_node,
                        .default = default,
                    },
                );

                if (default) |dft| {
                    try function_typedef.resolved_type.?.Function.defaults.put(
                        local.name,
                        try self.ast.toValue(dft, self.gc),
                    );
                }

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after function parameters.");
    }

    self.ast.nodes.items(.components)[function_signature].FunctionType.arguments = arguments.items;

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

        const yield_type_node = try self.parseTypeDef(function_typedef.resolved_type.?.Function.generic_types, true);
        const yield_type = self.ast.nodes.items(.type_def)[yield_type_node].?;

        if (!yield_type.optional and yield_type.def_type != .Void) {
            self.reportError(.yield_type, "Expected optional type or void");
        }

        function_typedef.resolved_type.?.Function.yield_type = try yield_type.toInstance(self.gc.allocator, &self.gc.type_registry);

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
        defer error_typedefs.shrinkAndFree(error_typedefs.items.len);

        const end_token: Token.Type = if (function_type.canOmitBody()) .Semicolon else .LeftBrace;

        while (!self.check(end_token) and !self.check(.Arrow) and !self.check(.Eof)) {
            const error_type_node = try self.parseTypeDef(
                self.ast.nodes.items(.type_def)[function_node].?.resolved_type.?.Function.generic_types,
                true,
            );
            const error_type = self.ast.nodes.items(.type_def)[error_type_node].?;

            try error_types.append(error_type_node);
            try error_typedefs.append(error_type);

            if (error_type.optional) {
                self.reportError(.error_type, "Error type can't be optional");
            }

            if (!self.check(end_token) and !self.check(.Arrow)) {
                try self.consume(.Comma, "Expected `,` after error type");
            }
        }

        if (error_types.items.len > 0) {
            function_typedef.resolved_type.?.Function.error_types = error_typedefs.items;
        }
    }

    self.ast.nodes.items(.components)[function_signature].FunctionType.error_types = error_types.items;

    // Parse body
    if (try self.match(.Arrow)) {
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
            self.reportError(.syntax, "Expected `>` after function argument list.");
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
                self.current.?.upvalues[i].index,
                if (self.current.?.upvalues[i].is_local) true else false,
            );
        }
    }

    self.ast.nodes.items(.type_def)[function_node] = try self.gc.type_registry.getTypeDef(function_typedef);

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
        self.reporter.reportErrorAt(
            .syntax,
            self.ast.tokens.get(callable.location),
            "Expected function call after `async`",
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
        const return_placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
        };
        const return_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = return_placeholder_resolved_type,
            },
        );

        try obj.PlaceholderDef.link(function_type, return_placeholder, .Call);

        const yield_placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
        };
        const yield_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = yield_placeholder_resolved_type,
            },
        );

        try obj.PlaceholderDef.link(function_type, yield_placeholder, .Yield);

        const fiber_def = obj.ObjFiber.FiberDef{
            .return_type = return_placeholder,
            .yield_type = yield_placeholder,
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
            self.reporter.reportErrorAt(
                .callable,
                self.ast.tokens.get(
                    self.ast.nodes.items(.location)[self.ast.nodes.items(.components)[call_node].Call.callee],
                ),
                "Can't be called",
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
        self.reportError(.syntax, "`resume` not allowed in global scope");
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
        const yield_placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
        };
        const yield_placeholder = try (try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = yield_placeholder_resolved_type,
            },
        )).cloneOptional(&self.gc.type_registry);

        try obj.PlaceholderDef.link(fiber_type.?, yield_placeholder, .Yield);

        self.ast.nodes.items(.type_def)[node] = yield_placeholder;
    } else {
        if (fiber_type.?.def_type != .Fiber) {
            self.reporter.reportErrorAt(
                .resumable,
                self.ast.tokens.get(self.ast.nodes.items(.location)[fiber_node]),
                "Can't be resumed",
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
        self.reportError(.syntax, "`resolve` not allowed in global scope");
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
        const return_placeholder_resolved_type = obj.ObjTypeDef.TypeUnion{
            .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, self.current_token.? - 1),
        };
        const return_placeholder = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Placeholder,
                .resolved_type = return_placeholder_resolved_type,
            },
        );

        try obj.PlaceholderDef.link(fiber_type.?, return_placeholder, .Yield);

        self.ast.nodes.items(.type_def)[node] = return_placeholder;
    } else {
        if (fiber_type.?.def_type != .Fiber) {
            self.reporter.reportErrorAt(
                .resolvable,
                self.ast.tokens.get(self.ast.nodes.items(.location)[fiber]),
                "Can't be resolveed",
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
        self.reportError(.syntax, "`yield` not allowed in global scope");
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
                        .List = obj.ObjList.ListDef.init(
                            self.gc.allocator,
                            self.gc.type_registry.int_type,
                        ),
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
                    self.reportError(
                        .syntax,
                        "Only one `out` statement is allowed in block expression",
                    );
                }

                out = stmt;
            }
        }
    }

    if (out != null and statements.getLastOrNull() != out) {
        self.reportError(
            .syntax,
            "Last block expression statement must be `out`",
        );
    }

    try self.consume(.RightBrace, "Expected `}` at end of block expression");

    self.current.?.in_block_expression = null;

    statements.shrinkAndFree(statements.items.len);

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
                .BlockExpression = statements.items,
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
                => if ((left_type_def != null and left_type_def.?.def_type == .Float) or (right_type_def != null and right_type_def.?.def_type == .Float))
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
    const is_main = std.mem.eql(u8, self.ast.tokens.items(.lexeme)[name_token], "main") and current_function_type_def != null and current_function_type_def.?.resolved_type.?.Function.function_type == .ScriptEntryPoint;

    if (is_main) {
        if (function_type == .Extern) {
            self.reportError(.extern_main, "`main` can't be `extern`.");
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
        if (fun_def.parameters.count() != 1 or (fun_def.return_type.def_type != .Integer and fun_def.return_type.def_type != .Void)) {
            signature_valid = false;
        } else {
            const first_param = fun_def.parameters.get(fun_def.parameters.keys()[0]);
            if (first_param == null or
                !(try self.parseTypeDefFrom("[str]")).eql(first_param.?))
            {
                signature_valid = false;
            }
        }

        if (!signature_valid) {
            const main_def_str = fun_typedef.toStringAlloc(self.gc.allocator) catch @panic("Out of memory");
            defer main_def_str.deinit();
            self.reporter.reportErrorFmt(
                .main_signature,
                self.ast.tokens.get(self.ast.nodes.items(.location)[function_node]),
                "Expected `main` signature to be `fun main([str] args) > void|int` got {s}",
                .{
                    main_def_str.items,
                },
            );
        }
    }

    const slot: usize = try self.declareVariable(
        fun_typedef,
        name_token,
        true,
        true,
    );

    self.markInitialized();

    return try self.ast.appendNode(
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
}

fn exportStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.namespace == null) {
        self.reportError(.syntax, "A exporting script must provide a namespace");
    }

    if (self.check(.Identifier) and (try self.checkAhead(.Semicolon, 0) or try self.checkAhead(.As, 0) or try self.checkAhead(.Dot, 0))) {
        try self.consume(.Identifier, "Expected identifier after `export`.");
        const identifier = self.current_token.? - 1;

        // Search for a global with that name
        if (try self.resolveGlobal(null, self.ast.tokens.items(.lexeme)[identifier])) |slot| {
            const global = &self.globals.items[slot];

            global.referenced = true;
            global.exported = true;
            if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
                const imported_from = global.imported_from.?;

                try self.script_imports.put(
                    imported_from,
                    .{
                        .location = self.script_imports.get(imported_from).?.location,
                        .referenced = true,
                    },
                );
            }

            var alias: ?Ast.TokenIndex = null;
            if (try self.match(.As)) {
                try self.consume(.Identifier, "Expected identifier after `as`.");

                global.export_alias = self.ast.tokens.items(.lexeme)[self.current_token.? - 1];
                alias = self.current_token.? - 1;
            }

            // If we're exporting an imported global, overwrite is prefix
            global.prefix = self.namespace;

            try self.consume(.Semicolon, "Expected `;` after statement.");

            return try self.ast.appendNode(
                .{
                    .tag = .Export,
                    .location = start_location,
                    .end_location = self.current_token.? - 1,
                    .type_def = global.type_def,
                    .components = .{
                        .Export = .{
                            .identifier = identifier,
                            .alias = alias,
                            .declaration = null,
                        },
                    },
                },
            );
        }
    } else {
        self.exporting = true;
        if (try self.declaration()) |decl| {
            self.globals.items[self.globals.items.len - 1].referenced = true;

            self.exporting = false;

            return try self.ast.appendNode(
                .{
                    .tag = .Export,
                    .location = start_location,
                    .end_location = self.current_token.? - 1,
                    .type_def = self.ast.nodes.items(.type_def)[decl],
                    .components = .{
                        .Export = .{
                            .identifier = null,
                            .alias = null,
                            .declaration = decl,
                        },
                    },
                },
            );
        }
        self.exporting = false;
    }

    self.reportError(.syntax, "Expected identifier or declaration.");

    return try self.ast.appendNode(
        .{
            .tag = .Export,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Export = .{
                    .identifier = self.current_token.? - 1,
                    .alias = null,
                    .declaration = null,
                },
            },
        },
    );
}

fn objectDeclaration(self: *Self) Error!Ast.Node.Index {
    if (self.current.?.scope_depth > 0) {
        self.reportError(.syntax, "Object must be defined at top-level.");
    }

    const start_location = self.current_token.? - 1;

    // Conforms to protocols?
    var protocols = std.AutoHashMap(*obj.ObjTypeDef, void).init(self.gc.allocator);
    var protocol_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
    defer protocol_nodes.shrinkAndFree(protocol_nodes.items.len);
    var protocol_count: usize = 0;
    if (try self.match(.LeftParen)) {
        while (!self.check(.RightParen) and !self.check(.Eof)) : (protocol_count += 1) {
            if (protocol_count > 255) {
                self.reportError(.protocols_count, "Can't conform to more than 255 protocols");
            }

            try self.consume(.Identifier, "Expected protocol identifier");

            const protocol_node = try self.parseUserType(false);
            const protocol = self.ast.nodes.items(.type_def)[protocol_node].?;

            if (protocols.get(protocol) != null) {
                self.reportErrorFmt(
                    .already_conforming_protocol,
                    "Already conforming to `{s}`.",
                    .{
                        protocol.resolved_type.?.Protocol.name.string,
                    },
                );
            }

            try protocols.put(protocol, {});
            try protocol_nodes.append(protocol_node);

            if (!(try self.match(.Comma))) {
                break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after protocols list");
    }

    // Get object name
    try self.consume(.Identifier, "Expected object name.");
    const object_name_token = self.current_token.? - 1;
    const object_name = self.ast.tokens.get(object_name_token).clone();

    // Qualified name to avoid cross script collision
    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
    defer self.gc.allocator.free(qualifier);
    var qualified_object_name = std.ArrayList(u8).init(self.gc.allocator);
    defer qualified_object_name.deinit();
    try qualified_object_name.writer().print("{s}.{s}", .{ qualifier, object_name.lexeme });

    // Create a placeholder for self-reference which will be resolved at the end when declaring the object
    const placeholder_index = try self.declarePlaceholder(object_name_token, null);
    const object_placeholder = self.globals.items[placeholder_index].type_def;

    var object_def = obj.ObjObject.ObjectDef.init(
        self.gc.allocator,
        object_name,
        try self.gc.copyString(object_name.lexeme),
        try self.gc.copyString(qualified_object_name.items),
        false,
    );

    object_def.conforms_to.deinit();
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
    defer generics.shrinkAndFree(generics.items.len);
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
                self.reportErrorFmt(
                    .generic_type,
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

        if (object_type.resolved_type.?.Object.generic_types.count() == 0) {
            self.reportError(
                .generic_type,
                "Expected at least one generic type",
            );
        }

        try self.consume(.Greater, "Expected `>` after generic types list");
    }

    try self.beginScope(null);

    // Body
    try self.consume(.LeftBrace, "Expected `{` before object body.");

    var members = std.ArrayList(Ast.ObjectDeclaration.Member).init(self.gc.allocator);
    defer members.shrinkAndFree(members.items.len);

    // To avoid using the same name twice
    var fields = std.StringArrayHashMap(void).init(self.gc.allocator);
    defer fields.deinit();

    // Members types
    var properties_type = std.StringHashMap(*obj.ObjTypeDef).init(self.gc.allocator);

    // Docblocks
    var property_idx: usize = 0;
    var static_property_idx: usize = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        const docblock = if (try self.match(.Docblock))
            self.current_token.? - 1
        else
            null;

        const static = try self.match(.Static);

        if (try self.match(.Fun)) {
            const method_token = self.current_token.?;
            const method_node = try self.method(
                false,
                if (static)
                    object_placeholder
                else
                    try object_placeholder.toInstance(self.gc.allocator, &self.gc.type_registry),
            );

            const method_type_def = self.ast.nodes.items(.type_def)[method_node];
            const method_name = method_type_def.?.resolved_type.?.Function.name.string;

            if (fields.get(method_name) != null) {
                self.reportError(.property_already_exists, "A member with that name already exists.");
            }

            // Does a placeholder exists for this name ?
            if (static) {
                if (object_type.resolved_type.?.Object.static_placeholders.get(method_name)) |placeholder| {
                    try self.resolvePlaceholder(placeholder, method_type_def.?, true);

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
                    try self.resolvePlaceholder(placeholder, method_type_def.?, true);

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
                method_name,
                .{
                    .name = method_name,
                    .type_def = method_type_def.?,
                    .constant = true,
                    .static = static,
                    .location = self.ast.tokens.get(method_token),
                    .method = true,
                    .has_default = false,
                    .index = if (static)
                        static_property_idx
                    else
                        property_idx,
                },
            );

            if (static) {
                static_property_idx += 1;
            } else {
                property_idx += 1;
            }

            try members.append(
                .{
                    .name = method_token,
                    .docblock = docblock,
                    .method = true,
                    .method_or_default_value = method_node,
                },
            );
            try fields.put(method_name, {});
            try properties_type.put(method_name, self.ast.nodes.items(.type_def)[method_node].?);
        } else {
            const constant = try self.match(.Const);
            const property_type = try self.parseTypeDef(null, true);
            const property_type_def = self.ast.nodes.items(.type_def)[property_type];

            try self.consume(.Identifier, "Expected property name.");
            const property_token = self.current_token.? - 1;
            const property_name = self.ast.tokens.get(property_token);

            if (fields.get(property_name.lexeme) != null) {
                self.reportError(.property_already_exists, "A member with that name already exists.");
            }

            // Does a placeholder exists for this name ?
            if (static) {
                if (object_type.resolved_type.?.Object.static_placeholders.get(property_name.lexeme)) |placeholder| {
                    try self.resolvePlaceholder(placeholder, property_type_def.?, false);

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
                    try self.resolvePlaceholder(placeholder, property_type_def.?, false);

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

            if (default != null and !self.ast.isConstant(default.?)) {
                self.reporter.reportErrorAt(
                    .constant_default,
                    self.ast.tokens.get(self.ast.nodes.items(.location)[default.?]),
                    "Default value must be constant",
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
                property_name.lexeme,
                .{
                    .name = property_name.lexeme,
                    .type_def = self.ast.nodes.items(.type_def)[property_type].?,
                    .constant = constant,
                    .static = static,
                    .location = property_name,
                    .method = false,
                    .has_default = default != null,
                    .index = if (static)
                        static_property_idx
                    else
                        property_idx,
                },
            );

            if (static) {
                static_property_idx += 1;
            } else {
                property_idx += 1;
            }

            try members.append(
                .{
                    .name = property_token,
                    .docblock = docblock,
                    .method = false,
                    .method_or_default_value = default,
                },
            );
            try fields.put(property_name.lexeme, {});
            try properties_type.put(
                property_name.lexeme,
                self.ast.nodes.items(.type_def)[property_type].?,
            );
        }
    }

    try self.consume(.RightBrace, "Expected `}` after object body.");

    const scope_end = try self.endScope();

    const slot = try self.declareVariable(
        &object_type, // Should resolve object_name_tokenect_placeholder and be discarded
        object_name_token,
        true, // Object is always constant
        true,
    );

    std.debug.assert(!object_type.optional);

    self.markInitialized();

    const node = self.ast.appendNode(
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
                    .protocols = protocol_nodes.items,
                    .generics = generics.items,
                    .members = members.items,
                },
            },
        },
    );

    std.debug.assert(object_placeholder.resolved_type.?.Object.placeholders.count() == 0 or object_placeholder.resolved_type.?.Object.static_placeholders.count() == 0);

    self.current_object = null;

    return node;
}

fn method(self: *Self, abstract: bool, this: *obj.ObjTypeDef) Error!Ast.Node.Index {
    try self.consume(.Identifier, "Expected method name.");

    return try self.function(
        self.current_token.? - 1,
        if (abstract) .Abstract else .Method,
        this,
    );
}

fn protocolDeclaration(self: *Self) Error!Ast.Node.Index {
    if (self.current.?.scope_depth > 0) {
        self.reportError(.syntax, "Protocol must be defined at top-level.");
    }

    const start_location = self.current_token.? - 1;

    // Get protocol name
    try self.consume(.Identifier, "Expected protocol name.");
    const protocol_name = self.current_token.? - 1;

    // Qualified name to avoid cross script collision
    const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
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
                self.gc.allocator,
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
    defer methods.shrinkAndFree(methods.items.len);
    while (!self.check(.RightBrace) and !self.check(.Eof)) {
        const docblock = if (try self.match(.Docblock))
            self.current_token.? - 1
        else
            null;

        try self.consume(.Fun, "Expected method definition");

        const method_name_token = self.current_token.?;
        const method_node = try self.method(
            true,
            try protocol_placeholder.toInstance(self.gc.allocator, &self.gc.type_registry),
        );
        const method_type_def = self.ast.nodes.items(.type_def)[method_node];

        try self.consume(.Semicolon, "Expected `;` after method definition");

        const method_name = method_type_def.?.resolved_type.?.Function.name.string;

        if (fields.get(method_name) != null) {
            self.reportError(.property_already_exists, "A method with that name already exists.");
        }

        try protocol_type.resolved_type.?.Protocol.methods.put(
            method_name,
            method_type_def.?,
        );

        // FIXME: we should not need this, the VM has a reference to the AST
        try protocol_type.resolved_type.?.Protocol.methods_locations.put(
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

    const slot = try self.declareVariable(
        &protocol_type, // Should resolve protocol_placeholder and be discarded
        protocol_name,
        true, // Protocol is always constant
        true,
    );

    std.debug.assert(!protocol_type.optional);

    self.markInitialized();

    return try self.ast.appendNode(
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
                    .methods = methods.items,
                },
            },
        },
    );
}

fn enumDeclaration(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.scope_depth > 0) {
        self.reportError(.syntax, "Enum must be defined at top-level.");
    }

    const enum_case_type_node =
        if (try self.match(.LeftParen))
        try self.parseTypeDef(null, true)
    else
        null;

    if (enum_case_type_node != null) {
        try self.consume(.RightParen, "Expected `)` after enum type.");
    }

    const enum_case_type = if (enum_case_type_node) |enum_type|
        try self.ast.nodes.items(.type_def)[enum_type].?.toInstance(self.gc.allocator, &self.gc.type_registry)
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
        self.gc.allocator,
        try self.gc.copyString(enum_name_lexeme),
        try self.gc.copyString(qualified_name.items),
        enum_case_type,
    );

    const enum_resolved = obj.ObjTypeDef.TypeUnion{ .Enum = enum_def };

    const enum_type = try self.gc.type_registry.getTypeDef(
        .{
            .def_type = .Enum,
            .resolved_type = enum_resolved,
        },
    );

    const slot: usize = try self.declareVariable(
        enum_type,
        enum_name,
        true,
        true,
    );
    self.markInitialized();

    try self.consume(.LeftBrace, "Expected `{` before enum body.");

    var cases = std.ArrayList(Ast.Enum.Case).init(self.gc.allocator);
    defer cases.shrinkAndFree(cases.items.len);
    var picked = std.ArrayList(bool).init(self.gc.allocator);
    var case_index: i32 = 0;
    while (!self.check(.RightBrace) and !self.check(.Eof)) : (case_index += 1) {
        if (case_index > 255) {
            self.reportError(.enum_cases_count, "Too many enum cases.");
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

        try enum_type.resolved_type.?.Enum.cases.append(case_name);

        if (!self.check(.RightBrace)) {
            try self.consume(.Comma, "Expected `,` after case definition.");
        }
    }

    try self.consume(.RightBrace, "Expected `}` after enum body.");

    if (case_index == 0) {
        self.reportError(.enum_cases_count, "Enum must have at least one case.");
    }

    const node = try self.ast.appendNode(
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
                    .cases = cases.items,
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
    defer obj_cases.shrinkAndFree(obj_cases.items.len);
    for (cases.items, 0..) |case, idx| {
        if (case.value != null and !self.ast.isConstant(case.value.?)) {
            self.reporter.reportErrorAt(
                .enum_case,
                self.ast.tokens.get(self.ast.nodes.items(.location)[case.value.?]),
                "Case value must be constant",
            );

            continue;
        }

        try obj_cases.append(
            if (case.value) |case_value|
                try self.ast.toValue(case_value, self.gc)
            else if (enum_type.def_type == .Integer)
                Value.fromInteger(@intCast(idx))
            else if (enum_type.def_type == .String)
                (try self.gc.copyString(self.ast.tokens.items(.lexeme)[case.name])).toValue()
            else
                unreachable,
        );
    }

    @"enum".cases = obj_cases.items;

    enum_type.resolved_type.?.Enum.value = @"enum";
    self.ast.nodes.items(.value)[node] = @"enum".toValue();

    return node;
}

fn varDeclaration(
    self: *Self,
    identifier_consumed: bool,
    parsed_type: ?Ast.Node.Index,
    terminator: DeclarationTerminator,
    constant: bool,
    should_assign: bool,
    type_provided_later: bool,
) Error!Ast.Node.Index {
    var var_type = if (parsed_type) |ptype|
        try self.ast.nodes.items(.type_def)[ptype].?.toInstance(
            self.gc.allocator,
            &self.gc.type_registry,
        )
    else
        self.gc.type_registry.any_type;

    const slot: usize = try self.parseVariable(
        identifier_consumed,
        var_type,
        constant,
        "Expected variable name.",
    );

    const name = self.current_token.? - 1;
    const start_location = name;

    const value = if (should_assign and try self.match(.Equal))
        try self.expression(false)
    else
        null;
    const value_type_def = if (value) |val|
        self.ast.nodes.items(.type_def)[val]
    else
        null;

    if (should_assign and value == null and (parsed_type == null or !self.ast.nodes.items(.type_def)[parsed_type.?].?.optional)) {
        self.reporter.reportErrorAt(
            .syntax,
            self.ast.tokens.get(self.current_token.? - 1),
            "Expected variable initial value",
        );
    }

    if (var_type.def_type == .Placeholder and value != null and value_type_def != null and value_type_def.?.def_type == .Placeholder) {
        try obj.PlaceholderDef.link(var_type, value_type_def.?, .Assignment);
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
            "Could not infer variable type.",
        );
    }

    if (value) |uvalue| {
        const tags = self.ast.nodes.items(.tag);
        const components = self.ast.nodes.items(.components);
        const parsed_type_def = if (parsed_type) |pt| self.ast.nodes.items(.type_def)[pt] else null;

        // [T] variable = [] -> [T] variable = [<T>];
        if (parsed_type_def != null and parsed_type_def.?.def_type == .List and tags[uvalue] == .List and components[uvalue].List.explicit_item_type == null and components[uvalue].List.items.len == 0) {
            self.ast.nodes.items(.type_def)[uvalue] = parsed_type_def.?;
        }

        // {K: V} variable = {} -> {K: V} variable = [<K: V>];
        if (parsed_type_def != null and parsed_type_def.?.def_type == .Map and tags[uvalue] == .Map and components[uvalue].Map.explicit_key_type == null and components[uvalue].Map.explicit_value_type == null and components[uvalue].Map.entries.len == 0) {
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

    return try self.ast.appendNode(
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
                    .constant = constant,
                    .slot = @intCast(slot),
                    .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
                },
            },
        },
    );
}

// Same as varDeclaration but does not parse anything (useful when we need to declare a variable that need to exists but is not exposed to the user)
fn implicitVarDeclaration(self: *Self, name: Ast.TokenIndex, parsed_type: *obj.ObjTypeDef, constant: bool) Error!Ast.Node.Index {
    const var_type = try parsed_type.toInstance(self.gc.allocator, &self.gc.type_registry);
    const slot = try self.declareVariable(var_type, name, constant, true);
    self.markInitialized();

    return try self.ast.appendNode(
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
                    .constant = constant,
                    .slot = @intCast(slot),
                    .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
                },
            },
        },
    );
}

fn listDeclaration(self: *Self, constant: bool) Error!Ast.Node.Index {
    const current_function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;

    if (self.check(.Less) and (self.current.?.scope_depth > 0 or current_function_type == .Repl)) {
        // Its a list expression
        return try self.expressionStatement(true);
    }

    return try self.varDeclaration(
        false,
        try self.parseListType(null),
        .Semicolon,
        constant,
        true,
        false,
    );
}

fn mapDeclaration(self: *Self, constant: bool) Error!Ast.Node.Index {
    const current_function_type = self.ast.nodes.items(.type_def)[self.current.?.function_node].?.resolved_type.?.Function.function_type;

    if (self.check(.Less) and (self.current.?.scope_depth > 0 or current_function_type == .Repl)) {
        // Its a map expression
        return try self.expressionStatement(true);
    }

    return try self.varDeclaration(
        false,
        try self.parseMapType(null),
        .Semicolon,
        constant,
        true,
        false,
    );
}

// `test` is just like a function but we don't parse arguments and we don't care about its return type
fn testStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    // We can't consume the name because declareVariable will do it
    const name_token = self.current_token.?;

    var function_def_placeholder = obj.ObjTypeDef{
        .def_type = .Function,
    };

    self.test_count += 1;

    const slot = try self.declareVariable(
        &function_def_placeholder,
        name_token,
        true,
        true,
    );

    self.markInitialized();

    const function_node = try self.function(name_token, .Test, null);
    const function_type_def = self.ast.nodes.items(.type_def)[function_node];

    if (function_type_def) |type_def| {
        self.globals.items[slot].type_def = type_def;
    }

    return try self.ast.appendNode(
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
                    .constant = true,
                    .slot = @intCast(slot),
                    .slot_type = .Global,
                },
            },
        },
    );
}
fn searchPaths(self: *Self, file_name: []const u8) ![][]const u8 {
    var paths = std.ArrayList([]const u8).init(self.gc.allocator);
    defer paths.shrinkAndFree(paths.items.len);

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
            try buzzLibPath(),
        );

        try paths.append(prefixed);
    }

    return paths.items;
}

fn searchLibPaths(self: *Self, file_name: []const u8) !std.ArrayList([]const u8) {
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
            try buzzLibPath(),
        );

        try paths.append(prefixed);
    }

    for (user_library_paths orelse &[_][]const u8{}) |path| {
        var filled = std.ArrayList(u8).init(self.gc.allocator);

        try filled.writer().print(
            "{s}{s}{s}.{s}",
            .{
                path,
                if (!std.mem.endsWith(u8, path, "/")) "/" else "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        filled.shrinkAndFree(filled.items.len);

        try paths.append(filled.items);

        var prefixed_filled = std.ArrayList(u8).init(self.gc.allocator);

        try prefixed_filled.writer().print(
            "{s}{s}lib{s}.{s}",
            .{
                path,
                if (!std.mem.endsWith(u8, path, "/")) "/" else "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        prefixed_filled.shrinkAndFree(prefixed_filled.items.len);

        try paths.append(prefixed_filled.items);
    }

    paths.shrinkAndFree(paths.items.len);
    return paths;
}

fn searchZdefLibPaths(self: *Self, file_name: []const u8) !std.ArrayList([]const u8) {
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
        try paths.append(suffixed);
    }

    for (Self.user_library_paths orelse &[_][]const u8{}) |path| {
        var filled = std.ArrayList(u8).init(self.gc.allocator);

        try filled.writer().print(
            "{s}{s}{s}.{s}",
            .{
                path,
                if (!std.mem.endsWith(u8, path, "/")) "/" else "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        filled.shrinkAndFree(filled.items.len);

        try paths.append(filled.items);

        var prefixed_filled = std.ArrayList(u8).init(self.gc.allocator);

        try prefixed_filled.writer().print(
            "{s}{s}lib{s}.{s}",
            .{
                path,
                if (!std.mem.endsWith(u8, path, "/")) "/" else "",
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        prefixed_filled.shrinkAndFree(prefixed_filled.items.len);

        try paths.append(prefixed_filled.items);
    }

    return paths;
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
        self.reportErrorFmt(
            .script_not_found,
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

        self.reportErrorFmt(
            .script_not_found,
            "buzz script `{s}` not found:\n{s}",
            .{
                file_name,
                search_report.items,
            },
        );

        return null;
    }

    defer file.?.close();

    // TODO: put source strings in a ArenaAllocator that frees everything at the end of everything
    const source = try self.gc.allocator.alloc(
        u8,
        (file.?.stat() catch {
            return Error.ImportError;
        }).size,
    );
    // defer self.gc.allocator.free(source);

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
    prefix: ?[]const u8,
    imported_symbols: *std.StringHashMap(Ast.Node.Index),
) Error!?ScriptImport {
    var import = self.imports.get(file_name);

    if (import == null) {
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
        const previous_root = self.ast.root;

        if (try parser.parse(source_and_path.?[0], file_name)) |ast| {
            self.ast = ast;
            self.ast.nodes.items(.components)[self.ast.root.?].Function.import_root = true;

            import = ScriptImport{
                .function = self.ast.root.?,
                .globals = std.ArrayList(Global).init(self.gc.allocator),
                .absolute_path = try self.gc.copyString(source_and_path.?[1]),
            };

            for (parser.globals.items) |*global| {
                if (global.exported) {
                    global.exported = false;

                    if (global.export_alias) |export_alias| {
                        global.name = try self.gc.copyString(export_alias);
                        global.export_alias = null;
                    }
                } else {
                    global.hidden = true;
                }

                try import.?.globals.append(global.*);
            }

            try self.imports.put(file_name, import.?);
            try self.script_imports.put(
                file_name,
                .{
                    .location = path_token,
                },
            );
        }

        // Caught up this parser with the import parser status
        self.ast.root = previous_root;
        // Last token of imported script is Eof, we discard it
        // Move scanned token just after import statement to replace the imported script Eof
        // FIXME: this is letting in place the token just before the import list of tokens
        self.ast.tokens.set(parser.current_token.?, self.ast.tokens.get(self.current_token.?));
        self.current_token = parser.current_token;
    }

    if (import) |imported| {
        const selective_import = imported_symbols.count() > 0;
        for (imported.globals.items) |imported_global| {
            var global = imported_global;

            if (!global.hidden) {
                if (imported_symbols.get(global.name.string) != null) {
                    _ = imported_symbols.remove(global.name.string);
                } else if (selective_import) {
                    global.hidden = true;
                }

                // Search for name collision
                if ((try self.resolveGlobal(prefix, global.name.string)) != null) {
                    self.reporter.reportWithOrigin(
                        .shadowed_global,
                        self.ast.tokens.get(self.current_token.? - 1),
                        self.ast.tokens.get(global.location),
                        "Shadowed global `{s}`",
                        .{global.name.string},
                        null,
                    );
                }

                global.prefix = if (prefix != null and std.mem.eql(u8, "_", prefix.?))
                    null // import "..." as _; | Erased prefix so the imported global are in the importer script namespace
                else
                    prefix orelse global.prefix;
            }

            global.imported_from = file_name;

            // TODO: we're forced to import all and hide some because globals are indexed and not looked up by name at runtime
            //       Only way to avoid this is to go back to named globals at runtime. Then again, is it worth it?
            try self.globals.append(global);
        }
    } else {
        // TODO: when it cannot load dynamic library, the error is the same
        self.reportErrorFmt(.compile, "Could not compile import or import external dynamic library `{s}`", .{file_name});
    }

    return import;
}

// This is used in the wasm build. There, we only allow the import of std libs by name
fn importStaticLibSymbol(self: *Self, file_name: []const u8, symbol: []const u8) !?*obj.ObjNative {
    const symbol_ptr = if (std.mem.eql(u8, file_name, "std"))
        std_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "gc"))
        gc_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "math"))
        math_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "debug"))
        debug_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "buffer"))
        buffer_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "serialize"))
        serialize_api.get(symbol)
    else if (std.mem.eql(u8, file_name, "crypto"))
        crypto_api.get(symbol)
    else
        null;

    if (symbol_ptr == null) {
        self.reportErrorFmt(
            .symbol_not_found,
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
fn importLibSymbol(self: *Self, full_file_name: []const u8, symbol: []const u8) !?*obj.ObjNative {
    // Remove .buzz extension, this occurs if this is the script being run or if the script was imported like so `import lib/std.buzz`
    // We consider that any other extension is silly from the part of the user
    const file_name = if (std.mem.endsWith(u8, full_file_name, ".buzz"))
        full_file_name[0..(full_file_name.len - 5)]
    else
        full_file_name;

    const file_basename = std.fs.path.basename(file_name);
    const paths = try self.searchLibPaths(file_basename);
    defer {
        for (paths.items) |path| {
            self.gc.allocator.free(path);
        }
        paths.deinit();
    }

    var lib: ?std.DynLib = null;
    for (paths.items) |path| {
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
            self.reportErrorFmt(
                .symbol_not_found,
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

    for (paths.items) |path| {
        try writer.print("    no file `{s}`\n", .{path});
    }

    self.reportErrorFmt(
        .library_not_found,
        "External library `{s}` not found: {s}{s}\n",
        .{
            file_basename,
            if (builtin.link_libc)
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
    defer symbols.shrinkAndFree(symbols.items.len);

    while ((try self.match(.Identifier)) and !self.check(.Eof)) {
        const symbol = self.ast.tokens.items(.lexeme)[self.current_token.? - 1];

        if (imported_symbols.get(symbol)) |loc| {
            self.reporter.reportWithOrigin(
                .import_already_exists,
                self.ast.tokens.get(self.current_token.? - 1),
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

    var prefix: ?Ast.TokenIndex = null;
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
            "Import path can't be empty",
        );
    }
    const file_name: []const u8 = if (path.lexeme.len <= 1 or path.literal_string.?.len <= 0) invalid: {
        self.reporter.reportErrorAt(
            .empty_import,
            path,
            "Import path can't be empty",
        );

        break :invalid "invalid";
    } else path.lexeme[1..(path.lexeme.len - 1)];

    if (imported_symbols.count() == 0 and try self.match(.As)) {
        try self.consume(.Identifier, "Expected identifier after `as`.");
        prefix = self.current_token.? - 1;
    }

    try self.consume(.Semicolon, "Expected `;` after statement.");

    const import = if (!self.reporter.had_error)
        try self.importScript(
            path_token,
            file_name,
            if (prefix) |pr|
                self.ast.tokens.items(.lexeme)[pr]
            else
                null,
            &imported_symbols,
        )
    else
        null;

    if (imported_symbols.count() > 0) {
        var it = imported_symbols.iterator();
        while (it.next()) |kv| {
            self.reportErrorFmt(.unknown_import, "Unknown import `{s}`.", .{kv.key_ptr.*});
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
                    .imported_symbols = symbols.items,
                    .path = path_token,
                    .import = import,
                },
            },
        },
    );
}

fn zdefStatement(self: *Self) Error!Ast.Node.Index {
    if (!BuildOptions.jit and BuildOptions.cycle_limit == null) {
        self.reportError(.zdef, "zdef can't be used, this instance of buzz was built with JIT compiler disabled");
    }

    if (is_wasm) {
        self.reportError(.zdef, "zdef is not available in WASM build");
    }

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
                zdef.type_def,
                zdef_name_token,
                true,
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
                    for (paths.items) |path| {
                        self.gc.allocator.free(path);
                    }
                    paths.deinit();
                }

                var lib: ?std.DynLib = null;
                for (paths.items) |path| {
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
                        self.reportErrorFmt(
                            .symbol_not_found,
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

                    for (paths.items) |path| {
                        try writer.print("    no file `{s}`\n", .{path});
                    }

                    self.reportErrorFmt(
                        .library_not_found,
                        "External library `{s}` not found: {s}{s}\n",
                        .{
                            lib_name_str,
                            if (builtin.link_libc)
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
                },
            );
        }
    }

    elements.shrinkAndFree(elements.items.len);

    return try self.ast.appendNode(
        .{
            .tag = .Zdef,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = null,
            .components = .{
                .Zdef = .{
                    .lib_name = lib_name,
                    .source = source,
                    .elements = elements.items,
                },
            },
        },
    );
}

// FIXME: this is almost the same as parseUserType!
fn userVarDeclaration(self: *Self, _: bool, constant: bool) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;
    const identifier = self.current_token.? - 1;
    var var_type: ?*obj.ObjTypeDef = null;

    const inferred_declaration = self.check(.Equal) and constant;
    var generic_resolve: ?Ast.Node.Index = null;

    // If next token is `=`, means the identifier wasn't a user type but the variable name
    // and the type needs to be inferred
    if (!inferred_declaration) {
        const user_type_name = self.current_token.? - 1;

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
            if (try self.resolveGlobal(null, self.ast.tokens.items(.lexeme)[user_type_name])) |slot| {
                const global = self.globals.items[slot];

                var_type = global.type_def;

                if (global.imported_from != null and self.script_imports.get(global.imported_from.?) != null) {
                    const imported_from = global.imported_from.?;

                    try self.script_imports.put(
                        imported_from,
                        .{
                            .location = self.script_imports.get(imported_from).?.location,
                            .referenced = true,
                        },
                    );
                }
            }
        }

        // If none found, create a placeholder
        if (var_type == null) {
            var placeholder_resolved_type: obj.ObjTypeDef.TypeUnion = .{
                // TODO: token is wrong but what else can we put here?
                .Placeholder = obj.PlaceholderDef.init(self.gc.allocator, user_type_name),
            };

            placeholder_resolved_type.Placeholder.name = try self.gc.copyString(
                self.ast.tokens.items(.lexeme)[user_type_name],
            );

            var_type = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            _ = try self.declarePlaceholder(user_type_name, var_type);
        }

        // Concrete generic types list
        generic_resolve = if (try self.match(.DoubleColon)) gn: {
            const generic_start = self.current_token.? - 1;

            try self.consume(.Less, "Expected generic types list after `::`");

            var resolved_generics = std.ArrayList(*obj.ObjTypeDef).init(self.gc.allocator);
            defer resolved_generics.shrinkAndFree(resolved_generics.items.len);
            var generic_nodes = std.ArrayList(Ast.Node.Index).init(self.gc.allocator);
            defer generic_nodes.shrinkAndFree(generic_nodes.items.len);
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
                self.reportErrorAtCurrent(.generic_type, "Expected at least one type");
            }

            // Shouldn't we populate only in codegen?
            var_type = try var_type.?.populateGenerics(
                self.current_token.? - 1,
                var_type.?.resolved_type.?.Object.id,
                resolved_generics.items,
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
                        .GenericResolveType = .{
                            .resolved_types = generic_nodes.items,
                        },
                    },
                },
            );
        } else null;

        if (try self.match(.Question)) {
            var_type = try var_type.?.cloneOptional(&self.gc.type_registry);
        }
    }

    const user_type_node = try self.ast.appendNode(
        .{
            .tag = .UserType,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .type_def = var_type,
            .components = .{
                .UserType = .{
                    .identifier = identifier,
                    .generic_resolve = generic_resolve,
                },
            },
        },
    );

    return try self.varDeclaration(
        inferred_declaration,
        user_type_node,
        .Semicolon,
        constant,
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
    defer init_declarations.shrinkAndFree(init_declarations.items.len);
    while (!self.check(.Semicolon) and !self.check(.Eof)) {
        try init_declarations.append(
            try self.varDeclaration(
                false,
                if (try self.match(.Var))
                    null
                else
                    try self.parseTypeDef(null, true),
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
    defer post_loop.shrinkAndFree(post_loop.items.len);
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
                    .init_declarations = init_declarations.items,
                    .condition = condition,
                    .post_loop = post_loop.items,
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
    const start_location = self.current_token.? - 1;

    try self.consume(.LeftParen, "Expected `(` after `foreach`.");

    try self.beginScope(null);

    const infer_key_type = try self.match(.Var);
    var key = try self.varDeclaration(
        false,
        if (infer_key_type)
            null
        else
            try self.parseTypeDef(null, true),
        .Nothing,
        false,
        false,
        infer_key_type,
    );

    const key_omitted = !(try self.match(.Comma));
    const infer_value_type = !key_omitted and try self.match(.Var);
    var value = if (!key_omitted)
        try self.varDeclaration(
            false,
            if (infer_value_type)
                null
            else
                try self.parseTypeDef(null, true),
            .Nothing,
            false,
            false,
            infer_value_type,
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
        try self.insertUtilityToken(Token.identifier("$iterable")),
        undefined,
        true,
    );

    const iterable = try self.expression(false);
    const iterable_type_def = self.ast.nodes.items(.type_def)[iterable].?;

    self.current.?.locals[iterable_slot].type_def = iterable_type_def;

    // Infer key/value type
    if (infer_key_type and !key_omitted) {
        const key_type = switch (iterable_type_def.def_type) {
            .List, .String => self.gc.type_registry.int_type,
            .Map => iterable_type_def.resolved_type.?.Map.key_type,
            else =>
            // Other type don't have key type
            self.current.?.locals[self.ast.nodes.items(.components)[key].VarDeclaration.slot].type_def,
        };

        self.current.?.locals[self.ast.nodes.items(.components)[key].VarDeclaration.slot].type_def = key_type;
        self.ast.nodes.items(.type_def)[key] = key_type;
    }

    if (infer_value_type or (infer_key_type and key_omitted)) {
        const value_type = switch (iterable_type_def.def_type) {
            .List => iterable_type_def.resolved_type.?.List.item_type,
            .Range => self.gc.type_registry.int_type,
            // .String => self.gc.type_registry.str_type,
            .Map => iterable_type_def.resolved_type.?.Map.value_type,
            .Enum => try iterable_type_def.toInstance(self.gc.allocator, &self.gc.type_registry),
            .Fiber => iterable_type_def.resolved_type.?.Fiber.yield_type,
            else =>
            // Other type are invalid and will be caught at codegen
            self.current.?.locals[self.ast.nodes.items(.components)[value.?].VarDeclaration.slot].type_def,
        };

        self.current.?.locals[self.ast.nodes.items(.components)[value.?].VarDeclaration.slot].type_def = value_type;
        self.ast.nodes.items(.type_def)[value.?] = value_type;
    }

    self.markInitialized();

    try self.consume(.RightParen, "Expected `)` after `foreach`.");

    const label = if (try self.match(.Colon)) lbl: {
        try self.consume(.Identifier, "Expected label after `:`.");

        break :lbl self.current_token.? - 1;
    } else null;

    try self.consume(.LeftBrace, "Expected `{`.");

    // We add it before parsing the body so that we can find it on a labeled break/continue statement
    const foreach_node = try self.ast.appendNode(
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

    try self.beginScope(foreach_node);
    const body = try self.block(
        .{
            .loop_type = .ForEach,
            .loop_body_scope = self.current.?.scope_depth,
        },
    );
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    self.ast.nodes.items(.end_location)[foreach_node] = self.current_token.? - 1;
    self.ast.nodes.items(.components)[foreach_node].ForEach.body = body;
    self.ast.nodes.items(.ends_scope)[foreach_node] = try self.endScope();

    return foreach_node;
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
        self.reportError(.syntax, "Can't use `return` at top-level.");
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
        self.reportError(
            .syntax,
            "`out` statement is only allowed inside a block expression",
        );
    } else if (self.current.?.scope_depth != self.current.?.in_block_expression.?) {
        self.reportError(
            .syntax,
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

fn namespaceStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    // Should be the first statement
    const components = self.ast.nodes.items(.components);
    const current_body = components[self.current.?.function_node].Function.body;
    if (current_body == null or components[current_body.?].Block.len > 0) {
        self.reportError(.syntax, "`namespace` should be the first statement");
    }

    try self.consume(.Identifier, "Expected namespace identifier");

    const identifier = self.current_token.? - 1;
    self.namespace = self.ast.tokens.items(.lexeme)[identifier];

    try self.consume(.Semicolon, "Expected `;` after statement.");

    return try self.ast.appendNode(
        .{
            .tag = .Namespace,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Namespace = identifier,
            },
        },
    );
}

fn tryStatement(self: *Self) Error!Ast.Node.Index {
    const start_location = self.current_token.? - 1;

    if (self.current.?.in_try) {
        self.reportError(.nested_try, "Nested `try` statement are not allowed");
    }

    self.current.?.in_try = true;

    try self.consume(.LeftBrace, "Expected `{` after `try`");

    try self.beginScope(null);
    const body = try self.block(null);
    self.ast.nodes.items(.ends_scope)[body] = try self.endScope();

    var clauses = std.ArrayList(Ast.Try.Clause).init(self.gc.allocator);
    defer clauses.shrinkAndFree(clauses.items.len);
    var unconditional_clause: ?Ast.Node.Index = null;
    // either catch with no type of catch any
    while (try self.match(.Catch)) {
        if (try self.match(.LeftParen)) {
            if (unconditional_clause != null) {
                self.reportError(.syntax, "Catch clause not allowed after unconditional catch");
            }

            try self.beginScope(null);

            const type_def = try self.parseTypeDef(null, true);

            _ = try self.parseVariable(
                false,
                self.ast.nodes.items(.type_def)[type_def].?,
                true, // function arguments are constant
                "Expected error identifier",
            );

            const identifier = self.current_token.? - 1;
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
            self.reportError(.syntax, "Expected `(` after `catch`");
        }
    }

    self.current.?.in_try = false;

    return try self.ast.appendNode(
        .{
            .tag = .Try,
            .location = start_location,
            .end_location = self.current_token.? - 1,
            .components = .{
                .Try = .{
                    .body = body,
                    .clauses = clauses.items,
                    .unconditional_clause = unconditional_clause,
                },
            },
        },
    );
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
        self.reportErrorFmt(
            .label_does_not_exists,
            "Label `{s}` does not exists.",
            .{
                self.ast.tokens.items(.lexeme)[label.?],
            },
        );
    }

    if (label == null and loop_scope == null) {
        self.reportError(.syntax, "break is not allowed here.");
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
