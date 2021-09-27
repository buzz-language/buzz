// zig fmt: off
const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _chunk = @import("./chunk.zig");
const _obj = @import("./obj.zig");
const _token = @import("./token.zig");
const _vm = @import("./vm.zig");
const _value = @import("./value.zig");
const _scanner = @import("./scanner.zig");
const _disassembler = @import("./disassembler.zig");
const _utils = @import("./utils.zig");

const Value = _value.Value;
const ValueType = _value.ValueType;
const valueToHashable = _value.valueToHashable;
const hashableToValue = _value.hashableToValue;
const valueToString = _value.valueToString;
const valueEql = _value.valueEql;
const ObjType = _obj.ObjType;
const Obj = _obj.Obj;
const ObjNative = _obj.ObjNative;
const ObjError = _obj.ObjError;
const ObjString = _obj.ObjString;
const ObjUpValue = _obj.ObjUpValue;
const ObjClosure = _obj.ObjClosure;
const ObjFunction = _obj.ObjFunction;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjObject = _obj.ObjObject;
const ObjectDef = _obj.ObjectDef;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjTypeDef = _obj.ObjTypeDef;
const PlaceholderDef = _obj.PlaceholderDef;
const allocateObject = _obj.allocateObject;
const allocateString = _obj.allocateString;
const OpCode = _chunk.OpCode;
const Chunk = _chunk.Chunk;
const Token = _token.Token;
const TokenType = _token.TokenType;
const copyStringRaw = _obj.copyStringRaw;
const Scanner = _scanner.Scanner;
const VM = _vm.VM;
const toNullTerminated = _utils.toNullTerminated;
const NativeFn = _obj.NativeFn;

extern fn dlerror() [*:0]u8;

const CompileError = error {
    Unrecoverable
};

pub const FunctionType = enum {
    Function,
    Initializer,
    Method,
    Script,
    TryCatch,
    Test,
    Anonymous,
};

pub const Local = struct {
    name: *ObjString, // TODO: do i need to mark those?
    type_def: *ObjTypeDef,
    depth: i32,
    is_captured: bool
};

pub const Global = struct {
    name: *ObjString, // TODO: do i need to mark those?
    type_def: *ObjTypeDef,
    initialized: bool = false,
    exported: bool = false,
    hidden: bool = false,
};

pub const UpValue = struct {
    index: u8,
    is_local: bool
};

pub const ObjectCompiler = struct {
    name: Token,
    enclosing: ?*ObjectCompiler,
};

pub const ChunkCompiler = struct {
    const Self = @This();

    enclosing: ?*ChunkCompiler = null,
    function: *ObjFunction,
    function_type: FunctionType,
    returned_type: ?*ObjTypeDef = null,

    locals: [255]Local,
    local_count: u8 = 0,
    upvalues: [255]UpValue,
    scope_depth: u32 = 0,

    pub fn init(compiler: *Compiler, function_type: FunctionType, file_name: ?[]const u8, this: ?*ObjTypeDef) !void {
        var file_name_string: ?*ObjString = if (file_name) |name| try copyStringRaw(compiler.strings, compiler.allocator, name) else null;

        var function = try ObjFunction.init(compiler.allocator, if (function_type != .Script)
            try copyStringRaw(compiler.strings, compiler.allocator, compiler.parser.previous_token.?.lexeme)
        else
            file_name_string orelse try copyStringRaw(compiler.strings, compiler.allocator, VM.script_string),
        try compiler.getTypeDef(.{
            .def_type = .Void,
            .optional = false,
        }));

        var self: Self = .{
            .locals = [_]Local{undefined} ** 255,
            .upvalues = [_]UpValue{undefined} ** 255,
            .enclosing = compiler.current,
            .function_type = function_type,
            .function = try compiler.allocator.create(ObjFunction),
        };

        self.function.* = function;

        compiler.current = try compiler.allocator.create(ChunkCompiler);
        compiler.current.?.* = self;

        // First local is reserved for an eventual `this`
        var local: *Local = &compiler.current.?.locals[self.local_count];
        compiler.current.?.local_count += 1;
        local.depth = 0;
        local.is_captured = false;

        if (function_type == .Method or function_type == .Initializer) {
            var type_def: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                .ObjectInstance = this.?
            };

            local.type_def = try compiler.getTypeDef(ObjTypeDef{
                .def_type = .ObjectInstance,
                .optional = false,
                .resolved_type = type_def
            });
        } else {
            local.type_def = try compiler.getTypeDef(ObjTypeDef{
                .def_type = .Void,
                .optional = false,
            });
        }

        local.name = try copyStringRaw(
            compiler.strings,
            compiler.allocator,
            if (function_type != .Function) VM.this_string else VM.empty_string
        );
    }

    pub fn getRootCompiler(self: *Self) *Self {
        return if (self.enclosing) |enclosing| enclosing.getRootCompiler() else self;
    }
};

pub const ParserState = struct {
    const Self = @This();

    current_token: ?Token = null,
    previous_token: ?Token = null,
    next_token: ?Token = null,
    had_error: bool = false,
    panic_mode: bool = false,
};

pub const Compiler = struct {
    const Self = @This();

    const Precedence = enum {
        None,
        Assignment, // =, -=, +=, *=, /=
        Is, // is
        Or, // or
        And, // and
        Xor, // xor
        Equality, // ==, !=
        Comparison, // >=, <=, >, <
        NullCoalescing, // ??
        Term, // +, -
        Shift, // >>, <<
        Factor, // /, *, %
        Unary, // +, ++, -, --, !
        Call, // call(), dot.ref, sub[script], optUnwrap?
        Primary, // literal, (grouped expression), super.ref, identifier, <type>[alist], <a, map>{...}
    };

    const ParseFn = fn (compiler: *Compiler, can_assign: bool) anyerror!*ObjTypeDef;
    const InfixParseFn = fn (compiler: *Compiler, can_assign: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef;

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?InfixParseFn,
        precedence: Precedence,
    };

    const rules = [_]ParseRule{
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Pipe
        .{ .prefix = list,     .infix = subscript, .precedence = .Call }, // LeftBracket
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // RightBracket
        .{ .prefix = grouping, .infix = call,      .precedence = .Call }, // LeftParen
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // RightParen
        .{ .prefix = map,      .infix = null,      .precedence = .None }, // LeftBrace
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // RightBrace
        .{ .prefix = null,     .infix = dot,       .precedence = .Call }, // Dot
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Comma
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Semicolon
        .{ .prefix = null,     .infix = binary,    .precedence = .Comparison }, // Greater
        .{ .prefix = list,     .infix = binary,    .precedence = .Comparison }, // Less
        .{ .prefix = null,     .infix = binary,    .precedence = .Term }, // Plus
        .{ .prefix = unary,    .infix = binary,    .precedence = .Term }, // Minus
        .{ .prefix = null,     .infix = binary,    .precedence = .Factor }, // Star
        .{ .prefix = null,     .infix = binary,    .precedence = .Factor }, // Slash
        .{ .prefix = null,     .infix = binary,    .precedence = .Factor }, // Percent
        .{ .prefix = null,     .infix = unwrap,    .precedence = .Call }, // Question
        .{ .prefix = unary,    .infix = forceUnwrap,.precedence = .Call }, // Bang
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Colon
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Equal
        .{ .prefix = null,     .infix = binary,    .precedence = .Equality }, // EqualEqual
        .{ .prefix = null,     .infix = binary,    .precedence = .Equality }, // BangEqual
        .{ .prefix = null,     .infix = binary,    .precedence = .Comparison }, // GreaterEqual
        .{ .prefix = null,     .infix = binary,    .precedence = .Comparison }, // LessEqual
        .{ .prefix = null,     .infix = binary,    .precedence = .NullCoalescing }, // QuestionQuestion
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // PlusEqual
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // MinusEqual
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // StarEqual
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // SlashEqual
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Increment
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Decrement
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Arrow
        .{ .prefix = literal,  .infix = null,      .precedence = .None }, // True
        .{ .prefix = literal,  .infix = null,      .precedence = .None }, // False
        .{ .prefix = literal,  .infix = null,      .precedence = .None }, // Null
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Str
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Num
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Type
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Bool
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Function
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // ShiftRight
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // ShiftLeft
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Xor
        .{ .prefix = null,     .infix = or_,       .precedence = .Or   }, // Or
        .{ .prefix = null,     .infix = and_,      .precedence = .And }, // And
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Return
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // If
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Else
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Do
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Until
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // While
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // For
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // ForEach
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Switch
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Break
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Default
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // In
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Is
        .{ .prefix = number,   .infix = null,      .precedence = .None }, // Number
        .{ .prefix = string,   .infix = null,      .precedence = .None }, // String
        .{ .prefix = variable, .infix = null,      .precedence = .None }, // Identifier
        .{ .prefix = fun,      .infix = null,      .precedence = .None }, // Fun
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Object
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Class
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Enum
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Throw
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Try
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Catch
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Test
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Import
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Export
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Eof
        .{ .prefix = null,     .infix = null,      .precedence = .None }, // Error
    };

    pub const ScriptImport = struct {
        function: *ObjFunction,
        globals: std.ArrayList(Global),
    };

    pub const OptJump = struct {
        jump: usize,
        precedence: Precedence,
    };

    allocator: *Allocator,
    scanner: ?Scanner = null,
    parser: ParserState = .{},
    current: ?*ChunkCompiler = null,
    current_object: ?ObjectCompiler = null,
    globals: std.ArrayList(Global),
    // Interned typedef, find a better way of hashing a key (won't accept float so we use toString)
    type_defs: std.StringHashMap(*ObjTypeDef),
    // Interned strings, will be copied over to the vm
    strings: *std.StringHashMap(*ObjString),
    // If true the script is being imported
    imported: bool = false,
    // Cached imported functions
    imports: *std.StringHashMap(ScriptImport),

    // Jump to patch at end of current expression with a optional unwrapping in the middle of it
    opt_jumps: ?std.ArrayList(OptJump) = null,

    /// If true, will search for a `test` entry point instead of `main`
    testing: bool = false,
    test_count: u64 = 0,

    pub fn init(allocator: *Allocator, strings: *std.StringHashMap(*ObjString), imports: *std.StringHashMap(ScriptImport), imported: bool) Self {
        return .{
            .allocator = allocator,
            .imports = imports,
            .globals = std.ArrayList(Global).init(allocator),
            .strings = strings,
            .type_defs = std.StringHashMap(*ObjTypeDef).init(allocator),
            .imported = imported
        };
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();

        // TODO: key are strings on the heap so free them, does this work?
        var it = self.type_defs.iterator();
        while (it.next()) |kv| {
            self.allocator.free(kv.key_ptr.*);
        }

        self.type_defs.deinit();
    }

    // TODO: walk the chain of compiler and destroy them in deinit

    pub fn compile(self: *Self, source: []const u8, file_name: ?[]const u8, testing: bool) !?*ObjFunction {
        self.testing = testing;

        if (self.scanner != null) {
            self.scanner = null;
        }

        self.scanner = Scanner.init(source);
        defer self.scanner = null;

        try ChunkCompiler.init(self, .Script, file_name, null);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        try self.advance();

        // Enter AST
        while (!(try self.match(.Eof))) {
            if (!(self.declarationOrReturnStatement() catch return null)) {
                return null;
            }
        }

        // Is there any placeholders left
        for (self.globals.items) |global| {
            if (global.type_def.def_type == .Placeholder) {
                try self.reportErrorAt(global.type_def.resolved_type.?.Placeholder.where, "Unknown variable.");
            }
        }

        var new_function: *ObjFunction = try self.endCompiler();

        return if (self.parser.had_error) null else new_function;
    }

    pub fn getTypeDef(self: *Self, type_def: ObjTypeDef) !*ObjTypeDef {
        // Don't intern placeholders
        if (type_def.def_type == .Placeholder) {
            var type_def_ptr: *ObjTypeDef = try self.allocator.create(ObjTypeDef);
            type_def_ptr.* = type_def;
            return type_def_ptr;
        }

        var type_def_str: []const u8 = try type_def.toString(self.allocator);

        if (self.type_defs.get(type_def_str)) |type_def_ptr| {
            self.allocator.free(type_def_str); // If already in map, we don't need this string anymore
            return type_def_ptr;
        }

        var type_def_ptr: *ObjTypeDef = try self.allocator.create(ObjTypeDef);
        type_def_ptr.* = type_def;

        _ = try self.type_defs.put(type_def_str, type_def_ptr);

        return type_def_ptr;
    }

    pub inline fn getTypeDefByName(self: *Self, name: []const u8) ?*ObjTypeDef {
        return self.type_defs.get(name);
    }

    fn report(self: *Self, token: Token, message: []const u8) !void {
        if (try self.scanner.?.getLine(self.allocator, token.line)) |line| {
            std.debug.warn("\n{} | {s}\n", .{ token.line, line });
            var i: usize = 0;
            // TODO: how to do this better?
            while (i < token.column - 1) : (i += 1) {
                std.debug.warn(" ", .{});
            }
            std.debug.warn("   ^\n", .{});
        }
        std.debug.warn(
            "{s}:{}:{}: \u{001b}[31mError:\u{001b}[0m {s}\n",
            .{
                self.current.?.getRootCompiler().function.name.string,
                token.line + 1,
                token.column + 1,
                message
            }
        );
    }

    fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
        if (self.parser.panic_mode) {
            return;
        }

        self.parser.panic_mode = true;
        self.parser.had_error = true;

        try self.report(token, message);
    }

    fn reportError(self: *Self, message: []const u8) !void {
        try self.reportErrorAt(self.parser.previous_token.?, message);
    }

    fn reportErrorAtCurrent(self: *Self, message: []const u8) !void {
        try self.reportErrorAt(self.parser.current_token.?, message);
    }

    fn reportTypeCheckAt(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8, at: Token) !void {
        var expected_str: []const u8 = try expected_type.toString(self.allocator);
        var actual_str: []const u8 = try actual_type.toString(self.allocator);
        var error_message: []u8 = try self.allocator.alloc(u8, expected_str.len + actual_str.len + 200);
        defer {
            self.allocator.free(error_message);
            self.allocator.free(expected_str);
            self.allocator.free(actual_str);
        }

        error_message = try std.fmt.bufPrint(error_message, "{s}: expected type `{s}`, got `{s}`", .{ message, expected_str, actual_str });

        try self.reportErrorAt(at, error_message);
    }

    fn reportTypeCheck(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8) !void {
        try self.reportTypeCheckAt(expected_type, actual_type, message, self.parser.previous_token.?);
    }

    // When we encounter the missing declaration we replace it with the resolved type.
    // We then follow the chain of placeholders to see if their assumptions were correct.
    // If not we raise a compile error.
    pub fn resolvePlaceholder(self: *Self, placeholder: *ObjTypeDef, resolved_type: *ObjTypeDef) anyerror!void {
        assert(placeholder.def_type == .Placeholder);

        // If two placeholders, see if they can be merged        
        if (resolved_type.def_type == .Placeholder) {
            if (!resolved_type.resolved_type.?.Placeholder.eql(placeholder.resolved_type.?.Placeholder)) {
                try self.reportErrorAt(resolved_type.resolved_type.?.Placeholder.where, "[Incompatible assumptions] Type check error");
            } else {
                // TODO: do we care that two equivalent placeholder exist?
                try resolved_type.resolved_type.?.Placeholder.enrich(&placeholder.resolved_type.?.Placeholder);
            }

            return;
        }
        
        var placeholder_def: PlaceholderDef = placeholder.resolved_type.?.Placeholder;

        if (placeholder_def.resolved_type) |assumed_type| {
            if (!assumed_type.eql(resolved_type)) {
                try self.reportTypeCheckAt(resolved_type, assumed_type, "Type check error", placeholder_def.where);
                return;
            }
        }
        
        if (placeholder_def.resolved_def_type) |assumed_def_type| {
            if (assumed_def_type != resolved_type.def_type) {
                try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: type check error.");
                return;
            }
        }

        if (placeholder_def.callable) |call_assumption| {
            if (call_assumption
                and resolved_type.def_type != .Object
                and resolved_type.def_type != .Function) {
                // TODO: better error messages on placeholder stuff
                try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: can't be called.");
                return;
            }
        }

        if (placeholder_def.subscriptable) |subscript_assumption| {
            if (subscript_assumption
                and resolved_type.def_type != .List
                and resolved_type.def_type != .Map) {
                // TODO: better error messages on placeholder stuff
                try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: can't be subscripted.");
                return;
            }
        }

        if (placeholder_def.field_accessible) |field_accessible_assumption| {
            if (field_accessible_assumption
                and resolved_type.def_type != .ObjectInstance
                and resolved_type.def_type != .Enum
                and resolved_type.def_type != .EnumInstance) {
                // TODO: better error messages on placeholder stuff
                try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: has no fields.");
                return;
            }
        }

        if (placeholder_def.resolved_parameters) |assumed_argument_list| {
            var parameters: std.StringArrayHashMap(*ObjTypeDef) = undefined;
            if (resolved_type.def_type == .Function) {
                parameters = resolved_type.resolved_type.?.Function.parameters;
            } else if (resolved_type.def_type == .Object) {
                // Search for `init` method
                var found_init: bool = false;
                var it = resolved_type.resolved_type.?.Object.methods.iterator();
                while (it.next()) |kv| {
                    if (mem.eql(u8, kv.key_ptr.*, "init")) {
                        assert(kv.value_ptr.*.def_type == .Function);
                        parameters = kv.value_ptr.*.resolved_type.?.Function.parameters;
                        found_init = true;
                        break;
                    }
                }

                // Otherwise argument list should be empty
                if (!found_init) {
                    parameters = std.StringArrayHashMap(*ObjTypeDef).init(self.allocator);
                }
            } else {
                unreachable;
            }

            if (assumed_argument_list.count() != parameters.count()) {
                try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: arity not matching.");

                return;
            }

            var it = assumed_argument_list.iterator();
            var resolved_parameter_keys: [][]const u8 = parameters.keys();
            while (it.next()) |kv| {
                // Is there a matching argument with the same type?
                if (parameters.get(kv.key_ptr.*)) |matching_arg| {
                    if (!matching_arg.eql(kv.value_ptr.*)) {
                        try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: argument not matching.");

                        return;
                    }
                } else if (mem.eql(u8, kv.key_ptr.*, "{{first}}")) {
                    // Is the first argument argument matching
                    if (!parameters.get(resolved_parameter_keys[0]).?.eql(kv.value_ptr.*)) {
                        try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: argument not matching.");

                        return;
                    }
                } else {
                    // That argument doesn't exists
                    try self.reportErrorAt(placeholder_def.where, "[Bad assumption]: argument does not exists.");
                }
            }
        }

        // Now walk the chain of placeholders and see if they hold up
        for (placeholder_def.children.items) |child| {
            var child_placeholder: PlaceholderDef = child.resolved_type.?.Placeholder;
            assert(child_placeholder.parent != null);
            assert(child_placeholder.parent_relation != null);

            switch (child_placeholder.parent_relation.?) {
                .Call => {
                    // Can we call the parent?
                    if (resolved_type.def_type != .Object
                        and resolved_type.def_type != .Function) {
                        try self.reportErrorAt(placeholder_def.where, "Can't be called");
                        return;
                    }

                    // Is the child types resolvable with parent return type
                    if (resolved_type.def_type == .Object) {
                        var instance_union: ObjTypeDef.TypeUnion = .{
                            .ObjectInstance = resolved_type
                        };
                        
                        var instance_type: *ObjTypeDef = try self.getTypeDef(.{
                            .optional = false,
                            .def_type = .ObjectInstance,
                            .resolved_type = instance_union,
                        });

                        try self.resolvePlaceholder(child, instance_type);
                    } else if (resolved_type.def_type != .Function) {
                        try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Function.return_type);
                    }
                },
                .Subscript => {
                    // Is child type matching the list type or map value type?
                    unreachable;
                },
                .FieldAccess => {
                    switch (resolved_type.def_type) {
                        .ObjectInstance => {
                            // We can't create a field access placeholder without a name
                            assert(child_placeholder.name != null);

                            var object_def: ObjObject.ObjectDef = resolved_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                            // Search for a field matching the placeholder
                            var resolved_as_field: bool = false;
                            if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                                try self.resolvePlaceholder(child, field);
                                resolved_as_field = true;
                            }

                            // Search for a method matching the placeholder
                            if (!resolved_as_field) {
                                if (object_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                                    try self.resolvePlaceholder(child, method_def);
                                }
                            }
                        },
                        .Enum => {
                            // We can't create a field access placeholder without a name
                            assert(child_placeholder.name != null);

                            var enum_def: ObjEnum.EnumDef = resolved_type.resolved_type.?.Enum;

                            // Search for a case matching the placeholder
                            for (enum_def.cases.items) |case| {
                                if (mem.eql(u8, case, child_placeholder.name.?.string)) {
                                    var enum_instance_def: ObjTypeDef.TypeUnion = .{
                                        .EnumInstance = resolved_type
                                    };

                                    try self.resolvePlaceholder(
                                        child,
                                        try self.getTypeDef(
                                            .{
                                                .optional = false,
                                                .def_type = .EnumInstance,
                                                .resolved_type = enum_instance_def,
                                            }
                                        )
                                    );
                                    break;
                                }
                            }

                        },
                        else => try self.reportErrorAt(placeholder_def.where, "Doesn't support field access")
                    }
                },
                .Assignment => {
                    // Assignment relation from a once Placeholder and now Class/Object/Enum is creating an instance
                    // TODO: but does this allow `AClass = something;` ?
                    var child_type: *ObjTypeDef = (try self.toInstance(resolved_type)) orelse resolved_type;

                    // Is child type matching the parent?
                    try self.resolvePlaceholder(child, child_type);
                },
            }
        }

        // Overwrite placeholder with resolved_type
        placeholder.* = resolved_type.*;

        // TODO: should resolved_type be freed?
        // TODO: does this work with vm.type_defs? (i guess not)
    }

    fn toInstance(self: *Self, instantiable: *ObjTypeDef) !?*ObjTypeDef {
        return switch (instantiable.def_type) {
            .Object => obj_instance: {
                var instance_type: ObjTypeDef.TypeUnion = .{
                    .ObjectInstance = instantiable
                };

                break :obj_instance try self.getTypeDef(ObjTypeDef {
                    .optional = false,
                    .def_type = .ObjectInstance,
                    .resolved_type = instance_type,
                });
            },
            .Enum => enum_instance: {
                var instance_type: ObjTypeDef.TypeUnion = .{
                    .EnumInstance = instantiable
                };

                break :enum_instance try self.getTypeDef(ObjTypeDef {
                    .optional = false,
                    .def_type = .EnumInstance,
                    .resolved_type = instance_type,
                });
            },
            else => null,
        };
    }

    fn advance(self: *Self) !void {
        self.parser.previous_token = self.parser.current_token;
        self.parser.current_token = self.parser.next_token;

        if (self.parser.current_token == null) {
            while (true) {
                self.parser.current_token = try self.scanner.?.scanToken();
                if (self.parser.current_token.?.token_type != .Error) {
                    break;
                }

                try self.reportErrorAtCurrent(self.parser.current_token.?.literal_string orelse "Unknown error.");
            }
        }

        if (self.parser.current_token.?.token_type == .Eof) {
            self.parser.next_token = null;
            return;
        }

        while (true) {
            self.parser.next_token = try self.scanner.?.scanToken();
            if (self.parser.next_token.?.token_type != .Error) {
                break;
            }

            try self.reportErrorAtCurrent(self.parser.next_token.?.literal_string orelse "Unknown error.");
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) !void {
        if (self.parser.current_token.?.token_type == token_type) {
            try self.advance();
            return;
        }

        try self.reportErrorAtCurrent(message);
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.parser.current_token.?.token_type == token_type;
    }

    fn checkAhead(self: *Self, token_type: TokenType) bool {
        return if (self.parser.next_token) |next| next.token_type == token_type else false;
    }

    fn match(self: *Self, token_type: TokenType) !bool {
        if (!self.check(token_type)) {
            return false;
        }

        try self.advance();

        return true;
    }

    fn endCompiler(self: *Self) !*ObjFunction {
        // If .Script, search for exported globals and return them in a map
        if (self.current.?.function_type == .Script) {
            // If top level, search `main` or `test` function(s) and call them
            // Then put any exported globals on the stack
            if (!self.testing) {
                var found_main: bool = false;
                for (self.globals.items) |global, index| {
                    if (mem.eql(u8, global.name.string, "main") and !global.hidden) {
                        found_main = true;

                        // TODO: Somehow push cli args on the stack
                        try self.emitCodeArg(.OP_GET_GLOBAL, @intCast(u24, index));
                        try self.emitCodeArg(.OP_CALL, 0);
                        break;
                    }
                }
            } else {
                // Create an entry point wich runs all `test`
                for (self.globals.items) |global, index| {
                    if (global.name.string.len > 5 and mem.eql(u8, global.name.string[0..5], "$test") and !global.hidden) {
                        try self.emitCodeArg(.OP_GET_GLOBAL, @intCast(u24, index));
                        try self.emitCodeArg(.OP_CALL, 0);
                    }
                }
            }

            // If we're being imported, put all globals on the stack
            if (self.imported) {
                var exported_count: usize = 0;
                for (self.globals.items) |_, index| {
                    exported_count += 1;

                    if (exported_count > 16777215) {
                        try self.reportError("Can't export more than 16777215 values.");
                        break;
                    }

                    try self.emitCodeArg(.OP_GET_GLOBAL, @intCast(u24, index));
                }

                try self.emitCodeArg(.OP_EXPORT, @intCast(u24, exported_count));
            } else {
                try self.emitOpCode(.OP_NULL);
                try self.emitOpCode(.OP_RETURN);
            }
        // Emit `return void;` if no returnStatement was parsed
        } else if (self.current.?.returned_type == null) {
            try self.emitReturn();
        }

        var current_function: *ObjFunction = self.current.?.function;

        self.current = self.current.?.enclosing;

        // std.debug.print("\n\n==========================", .{});
        // try disassembler.disassembleChunk(
        //     &function.chunk,
        //     function.name.string
        // );
        // std.debug.print("\n\n==========================", .{});

        return current_function;
    }

    inline fn beginScope(self: *Self) void {
        self.current.?.scope_depth += 1;
    }

    fn endScope(self: *Self) !void {
        var current: *ChunkCompiler = self.current.?;

        current.scope_depth -= 1;

        while (current.local_count > 0
            and current.locals[current.local_count - 1].depth > current.scope_depth) {
            if (current.locals[current.local_count - 1].is_captured) {
                try self.emitOpCode(.OP_CLOSE_UPVALUE);
            } else {
                try self.emitOpCode(.OP_POP);
            }

            current.local_count -= 1;
        }
    }

    // BYTE EMITTING

    fn emit(self: *Self, code: u32) !void {
        try self.current.?.function.chunk.write(code, self.parser.previous_token.?.line);
    }

    fn emitCodeArg(self: *Self, code: OpCode, arg: u24) !void {
        try self.emit(
            (@intCast(u32, @enumToInt(code)) << 24)
            | @intCast(u32, arg)
        );
    }

    fn emitOpCode(self: *Self, code: OpCode) !void {
        try self.emit(@intCast(u32, @intCast(u32, @enumToInt(code)) << 24));
    }

    fn emitLoop(self: *Self, loop_start: usize) !void {
        const offset: usize = self.current.?.function.chunk.code.items.len - loop_start + 1;
        if (offset > 16777215) {
            try self.reportError("Loop body to large.");
        }

        try self.emitCodeArg(.OP_LOOP, @intCast(u24, offset));
    }

    fn emitJump(self: *Self, instruction: OpCode) !usize {
        try self.emitCodeArg(instruction, 0xffffff);

        return self.current.?.function.chunk.code.items.len - 1;
    }

    fn patchJump(self: *Self, offset: usize) !void {
        const jump: usize = self.current.?.function.chunk.code.items.len - offset - 1;

        if (jump > 16777215) {
            try self.reportError("Jump to large.");
        }

        const original: u32 = self.current.?.function.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, jump);
    }

    fn emitList(self: *Self) !usize {
        try self.emitCodeArg(.OP_LIST, 0xffffff);

        return self.current.?.function.chunk.code.items.len - 1;
    }

    fn patchList(self: *Self, offset: usize, constant: u24) !void {
        const original: u32 = self.current.?.function.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, constant);
    }

    fn emitMap(self: *Self) !usize {
        try self.emitCodeArg(.OP_MAP, 0xffffff);
        try self.emit(0xffffffff);

        return self.current.?.function.chunk.code.items.len - 2;
    }

    fn patchMap(self: *Self, offset: usize, key_constant: u24, value_constant: u24) !void {
        const original: u32 = self.current.?.function.chunk.code.items[offset];
        const instruction: u8 = @intCast(u8, original >> 24);

        self.current.?.function.chunk.code.items[offset] =
            (@intCast(u32, instruction) << 24) | @intCast(u32, key_constant);
        self.current.?.function.chunk.code.items[offset + 1] = @intCast(u32, value_constant);
    }

    fn emitReturn(self: *Self) !void {
        var return_type: *ObjTypeDef = undefined;

        if (self.current.?.function_type == .Initializer) {
            return_type = self.current.?.function.return_type;
            try self.emitCodeArg(.OP_GET_LOCAL, 0);
        } else if (self.current.?.function_type != .Script) {
            return_type = try self.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Void
                }
            );
            try self.emitOpCode(.OP_NULL);
        }

        if (self.current.?.returned_type) |already_returned| {
            if (!already_returned.eql(return_type)) {
                try self.reportError("Returning multiple incompatible types.");
            }
        }

        self.current.?.returned_type = return_type;

        try self.emitOpCode(.OP_RETURN);
    }

    // AST NODES
    // TODO: minimize code redundancy between declaration and declarationOrStatement
    // TODO: varDeclaration here can be an issue if they produce placeholders because opcode can be out of order
    //       We can only allow constant expressions: `str hello = "hello";` but not `num hello = aglobal + 12;`
    fn declarationOrReturnStatement(self: *Self) !bool {
        if (try self.match(.Object)) {
            try self.objectDeclaration(false);
        } else if (try self.match(.Class)) {
            try self.objectDeclaration(true);
        } else if (try self.match(.Enum)) {
            try self.enumDeclaration();
        } else if (try self.match(.Fun)) {
            try self.funDeclaration();
        } else if (try self.match(.Str)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .String
                    }
                ),
                false
            );
        } else if (try self.match(.Num)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Number
                    }
                ),
                false
            );
        } else if (try self.match(.Bool)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Bool
                    }
                ),
                false
            );
        } else if (try self.match(.Type)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Type
                    }
                ),
                false
            );
        } else if (try self.match(.LeftBracket)) {
            try self.listDeclaration();
        } else if (try self.match(.LeftBrace)) {
            try self.mapDeclaration();
        } else if (try self.match(.Test)) {
            try self.testStatement();
        } else if (try self.match(.Function)) {
            try self.varDeclaration(try self.functionType(), false);
        // In the declaractive space, starting with an identifier is always a varDeclaration with a user type
        } else if (try self.match(.Identifier)) {
            var user_type_name: Token = self.parser.previous_token.?.clone();
            var var_type: ?*ObjTypeDef = null;

            // Search for a global with that name
            for (self.globals.items) |global| {
                if (mem.eql(u8, global.name.string, user_type_name.lexeme)) {
                    var_type = global.type_def;
                    break;
                }
            }

            // No placeholder at top-level
            if (var_type == null) {
                try self.reportError("Unknown identifier");
            }

            try self.varDeclaration(var_type.?, false);
        } else if (try self.match(.Return)) {
            try self.returnStatement();
        } else if (try self.match(.Import)) {
            try self.importStatement();
        } else if (try self.match(.Export)) {
            try self.exportStatement();
        } else {
            try self.reportError("No declaration or statement.");

            return false;
        }

        return true;
    }

    fn declarationOrStatement(self: *Self) !?std.ArrayList(usize) {
        var hanging: bool = false;
        // Things we can match with the first token
        if (try self.match(.Fun)) {
            try self.funDeclaration();

            return null;
        } else if (try self.match(.Str)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .String
                    }
                ),
                false
            );

            return null;
        } else if (try self.match(.Num)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Number
                    }
                ),
                false
            );

            return null;
        } else if (try self.match(.Bool)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Bool
                    }
                ),
                false
            );

            return null;
        } else if (try self.match(.Type)) {
            try self.varDeclaration(
                try self.getTypeDef(
                    .{
                        .optional = try self.match(.Question),
                        .def_type = .Type
                    }
                ),
                false
            );

            return null;
        } else if (try self.match(.LeftBracket)) {
            try self.listDeclaration();
            return null;
        } else if (try self.match(.LeftBrace)) {
            try self.mapDeclaration();
            return null;
        } else if (try self.match(.Function)) {
            try self.varDeclaration(try self.functionType(), false);
            return null;
        } else if (try self.match(.Identifier)) {
            if (self.check(.Identifier) or (self.check(.Question) and self.checkAhead(.Identifier))) {
                var user_type_name: Token = self.parser.previous_token.?.clone();
                var var_type: ?*ObjTypeDef = null;

                // Search for a global with that name
                for (self.globals.items) |global| {
                    if (mem.eql(u8, global.name.string, user_type_name.lexeme)) {
                        var_type = global.type_def;
                        break;
                    }
                }

                // If none found, create a placeholder
                if (var_type == null) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                    };

                    placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, user_type_name.lexeme);

                    var_type = try self.getTypeDef(.{
                        .optional = try self.match(.Question),
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type
                    });
                }

                try self.varDeclaration(var_type.?, false);

                return null;
            } else {
                hanging = true;
            }
        }
        
        return try self.statement(hanging);
    }

    // When a break statement, will return index of jump to patch
    fn statement(self: *Self, hanging: bool) !?std.ArrayList(usize) {
        // TODO: remove
        if (try self.match(.If)) {
            assert(!hanging);
            return try self.ifStatement();
        } else if (try self.match(.For)) {
            assert(!hanging);
            try self.forStatement();
        } else if (try self.match(.ForEach)) {
            assert(!hanging);
            try self.forEachStatement();
        } else if (try self.match(.While)) {
            assert(!hanging);
            try self.whileStatement();
        } else if (try self.match(.Do)) {
            assert(!hanging);
            try self.doUntilStatement();
        } else if (try self.match(.Return)) {
            assert(!hanging);
            try self.returnStatement();
        } else if (try self.match(.Break)) {
            assert(!hanging);
            var breaks: std.ArrayList(usize) = std.ArrayList(usize).init(self.allocator);
            try breaks.append(try self.breakStatement());

            return breaks;
        } else if (try self.match(.Try)) {
            try self.tryCatchStatement();
        } else if (try self.match(.Throw)) {
            if (!self.check(.Semicolon)) {
                // TODO: When we have Error objects hierarchy, parse an instance of Error
                var parsed_type: *ObjTypeDef = try self.expression(false);

                if (parsed_type.def_type != .String) {
                    try self.reportError("Expected string after `throw`.");
                }
            } else {
                try self.emitCodeArg(
                    .OP_CONSTANT,
                    try self.makeConstant(Value{ .Obj = (try copyStringRaw(self.strings, self.allocator, "uncaught error")).toObj() })
                );
            }

            try self.consume(.Semicolon, "Expected `;` after `throw.`");

            try self.emitOpCode(.OP_THROW);
        } else {
            try self.expressionStatement(hanging);
        }

        return null;
    }

    fn returnStatement(self: *Self) !void {
        if (self.current.?.scope_depth == 0) {
            try self.reportError("Can't use `return` at top-level.");
        }

        if (try self.match(.Semicolon)) {
            try self.emitReturn();
        } else {
            if (self.current.?.function_type == .Initializer) {
                try self.reportError("Can't return a value from an initializer.");
            }

            var return_type: *ObjTypeDef= try self.expression(false);
            if (!self.current.?.function.return_type.eql(return_type)) {
                try self.reportTypeCheck(self.current.?.function.return_type, return_type, "Return value");
            }

            self.current.?.returned_type = return_type;

            try self.consume(.Semicolon, "Expected `;` after return value.");
            try self.emitOpCode(.OP_RETURN);
        }
    }

    fn parseTypeDef(self: *Self) anyerror!*ObjTypeDef {
        if (try self.match(.Str)) {
            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .String
            });
        } else if (try self.match(.Num)) {
            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Number
            });
        } else if (try self.match(.Bool)) {
            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Bool
            });
        } else if (try self.match(.Type)) {
            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Type
            });
        } else if (try self.match(.LeftBracket)) {
            var item_type: *ObjTypeDef = try self.parseTypeDef();
            var list_def = ObjList.ListDef.init(self.allocator, item_type);

            try self.consume(.RightBracket, "Expected `]` to end list type.");

            const resolved_type: ObjTypeDef.TypeUnion = .{
                .List = list_def
            };

            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = resolved_type
            });
        } else if (try self.match(.LeftBrace)) {
            var key_type: *ObjTypeDef = try self.parseTypeDef();

            try self.consume(.Comma, "Expected `,` after map key type.");

            var value_type: *ObjTypeDef = try self.parseTypeDef();

            try self.consume(.RightBrace, "Expected `}}` to end map type.");

            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = ObjTypeDef.TypeUnion{
                    .Map = ObjMap.MapDef {
                        .key_type = key_type,
                        .value_type = value_type,
                    }
                }
            });
        } else if (try self.match(.Function)) {
            return try self.functionType();
        } else if ((try self.match(.Identifier))) {
            var user_type_name: Token = self.parser.previous_token.?.clone();
            var var_type: ?*ObjTypeDef = null;

            // Search for a global with that name
            for (self.globals.items) |global| {
                if (mem.eql(u8, global.name.string, user_type_name.lexeme)) {
                    var_type = global.type_def;
                    break;
                }
            }

            // If none found, create a placeholder
            if (var_type == null) {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, user_type_name.lexeme);

                var_type = try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                });
            }

            return var_type.?;
        } else {
            try self.reportErrorAtCurrent("Expected type definition.");

            return try self.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Void
            });
        }
    }

    inline fn getRule(token: TokenType) ParseRule {
        return rules[@enumToInt(token)];
    }

    fn parsePrecedence(self: *Self, precedence: Precedence, hanging: bool) !*ObjTypeDef {
        // If hanging is true, that means we already read the start of the expression
        if (!hanging) {
            _ = try self.advance();
        }

        var prefixRule: ?ParseFn = getRule(self.parser.previous_token.?.token_type).prefix;
        if (prefixRule == null) {
            try self.reportError("Expected expression.");

            // TODO: find a way to continue or catch that error
            return CompileError.Unrecoverable;
        }

        var canAssign: bool = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        var parsed_type: *ObjTypeDef = try prefixRule.?(self, canAssign);

        while (@enumToInt(getRule(self.parser.current_token.?.token_type).precedence) >= @enumToInt(precedence)) {
            // Patch optional jumps
            if (self.opt_jumps) |jumps| {
                assert(jumps.items.len > 0);
                var first_jump: OptJump = jumps.items[0];

                if (@enumToInt(getRule(self.parser.current_token.?.token_type).precedence) < @enumToInt(first_jump.precedence)) {
                    // Hope over pop if actual value
                    const njump: usize = try self.emitJump(.OP_JUMP);

                    for (jumps.items) |jump| {
                        try self.patchJump(jump.jump);
                    }
                    // If aborted by a null optional, will result in null on the stackl
                    try self.emitOpCode(.OP_POP);

                    try self.patchJump(njump);

                    jumps.deinit();
                    self.opt_jumps = null;
                }
            }

            _ = try self.advance();

            var infixRule: InfixParseFn = getRule(self.parser.previous_token.?.token_type).infix.?;
            parsed_type = try infixRule(self, canAssign, parsed_type);
        }

        if (canAssign and (try self.match(.Equal))) {
            try self.reportError("Invalid assignment target.");
        }

        return parsed_type;
    }

    fn expression(self: *Self, hanging: bool) !*ObjTypeDef {
        return try self.parsePrecedence(.Assignment, hanging);
    }

    // Returns a list of break jumps to patch
    fn block(self: *Self) anyerror!std.ArrayList(usize) {
        var breaks = std.ArrayList(usize).init(self.allocator);

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            if (try self.declarationOrStatement()) |jumps| {
                try breaks.appendSlice(jumps.items);
                jumps.deinit();
            }
        }

        try self.consume(.RightBrace, "Expected `}}` after block.");

        return breaks;
    }

    fn functionType(self: *Self) !*ObjTypeDef {
        assert(self.parser.previous_token.?.token_type == .Function);

        try self.consume(.LeftParen, "Expected `(` after function name.");

        var parameters: std.StringArrayHashMap(*ObjTypeDef) = std.StringArrayHashMap(*ObjTypeDef).init(self.allocator);
        var arity: usize = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                arity += 1;
                if (arity > 255) {
                    try self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                }

                var param_type: *ObjTypeDef = try self.parseTypeDef();
                var param_name: []const u8 = if (try self.match(.Identifier)) self.parser.previous_token.?.lexeme else "_";

                try parameters.put(param_name, param_type);

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after function parameters.");
        
        var return_type: *ObjTypeDef = undefined;
        if (try self.match(.Greater)) {
            return_type = try self.parseTypeDef();
        } else {
            return_type = try self.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Void
                }
            );
        }

        var function_typedef: ObjTypeDef = .{
            .optional = false,
            .def_type = .Function,
        };

        var function_def: ObjFunction.FunctionDef = .{
            .name = try copyStringRaw(self.strings, self.allocator, "anonymous"),
            .return_type = return_type,
            .parameters = parameters,
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{
            .Function = function_def
        };

        function_typedef.resolved_type = function_resolved_type;

        return try self.getTypeDef(function_typedef);
    }

    fn fun(self: *Self, _: bool) anyerror!*ObjTypeDef {
        return try self.function(null, .Anonymous, null);
    }

    fn function(self: *Self, name: ?Token, function_type: FunctionType, this: ?*ObjTypeDef) !*ObjTypeDef {
        try ChunkCompiler.init(self, function_type, null, this);
        var compiler: *ChunkCompiler = self.current.?;
        self.beginScope();

        var parameters: ?std.StringArrayHashMap(*ObjTypeDef) = null;

        if (function_type != .TryCatch and function_type != .Test) {
            try self.consume(.LeftParen, "Expected `(` after function name.");

            parameters = std.StringArrayHashMap(*ObjTypeDef).init(self.allocator);
            var arity: usize = 0;
            if (!self.check(.RightParen)) {
                while (true) {
                    arity += 1;
                    if (arity > 255) {
                        try self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                    }

                    var param_type: *ObjTypeDef = try self.parseTypeDef();
                    var slot: usize = try self.parseVariable(param_type, "Expected parameter name");

                    if (self.current.?.scope_depth > 0) {
                        var local: Local = self.current.?.locals[slot];
                        try parameters.?.put(local.name.string, local.type_def);
                    } else {
                        var global: Global = self.globals.items[slot];
                        try parameters.?.put(global.name.string, global.type_def);
                    }

                    try self.defineGlobalVariable(@intCast(u24, slot));

                    if (!try self.match(.Comma)) break;
                }
            }

            try self.consume(.RightParen, "Expected `)` after function parameters.");
        } else if (function_type == .Test) {
            try self.consume(.String, "Expected `str` after `test`.");
            _ = try self.string(false);
            try self.emitOpCode(.OP_PRINT);
        }
        
        var return_type: *ObjTypeDef = undefined;
        if (function_type != .TryCatch and function_type != .Test and try self.match(.Greater)) {
            return_type = try self.parseTypeDef();
        } else {
            return_type = try self.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Void
                }
            );
        }

        self.current.?.function.return_type = return_type;

        if (function_type == .Anonymous and try self.match(.Arrow)) {
            var expr_type: *ObjTypeDef = try self.expression(false);

            if (!return_type.eql(expr_type)) {
                try self.reportTypeCheck(return_type, expr_type, "Bad return type");
            }

            try self.emitOpCode(.OP_RETURN); // Lambda functions returns its single expression
        } else {
            try self.consume(.LeftBrace, "Expected `{{` before function body.");
            _ = try self.block();

            var returned_type: *ObjTypeDef = self.current.?.returned_type orelse try self.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Void
                }
            );

            if (!return_type.eql(returned_type)) {
                try self.reportTypeCheck(return_type, returned_type, "Bad return type");
            }
        }

        var new_function: *ObjFunction = try self.endCompiler();

        try self.emitCodeArg(.OP_CLOSURE, try self.makeConstant(Value { .Obj = new_function.toObj() }));

        var i: usize = 0;
        while (i < new_function.upvalue_count) : (i += 1) {
            try self.emit(if (compiler.upvalues[i].is_local) 1 else 0);
            try self.emit(compiler.upvalues[i].index);
        }

        var function_typedef: ObjTypeDef = .{
            .optional = false,
            .def_type = .Function,
        };

        var function_def: ObjFunction.FunctionDef = .{
            .name = if (name) |uname| try copyStringRaw(self.strings, self.allocator, uname.lexeme) else try copyStringRaw(self.strings, self.allocator, "anonymous"),
            .return_type = return_type,
            .parameters = parameters orelse std.StringArrayHashMap(*ObjTypeDef).init(self.allocator),
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{
            .Function = function_def
        };

        function_typedef.resolved_type = function_resolved_type;

        return try self.getTypeDef(function_typedef);
    }

    fn method(self: *Self, object: *ObjTypeDef) !*ObjTypeDef {
        try self.consume(.Identifier, "Expected method name.");
        var constant: u24 = try self.identifierConstant(self.parser.previous_token.?.lexeme);

        var fun_type: FunctionType = .Method;

        if (self.parser.previous_token.?.lexeme.len == 4
            and mem.eql(u8, self.parser.previous_token.?.lexeme, "init")) {
            fun_type = .Initializer;
        }

        var fun_typedef: *ObjTypeDef = try self.function(self.parser.previous_token.?.clone(), fun_type, object);

        try self.emitCodeArg(.OP_METHOD, constant);

        return fun_typedef;
    }

    const Property = struct {
        name: []const u8,
        type_def: *ObjTypeDef,
    };

    fn property(self: *Self) !?Property {
        var name: ?Token = null;
        var type_def: ?*ObjTypeDef = null;
        var constant: ?u24 = null;

        // Parse type and name
        if (try self.match(.Str)) {
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);

            type_def = try self.getTypeDef(ObjTypeDef{
                .optional = try self.match(.Question),
                .def_type = .String,
            });
        } else if (try self.match(.Num)) {
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);

            type_def = try self.getTypeDef(ObjTypeDef{
                .optional = try self.match(.Question),
                .def_type = .Number,
            });
        } else if (try self.match(.Bool)) {
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);

            type_def = try self.getTypeDef(ObjTypeDef{
                .optional = try self.match(.Question),
                .def_type = .Bool,
            });
        } else if (try self.match(.Type)) {
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);

            type_def = try self.getTypeDef(ObjTypeDef{
                .optional = try self.match(.Question),
                .def_type = .Type,
            });
        } else if (try self.match(.LeftBracket)) {
            type_def = try self.parseListType();
            
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);
        } else if (try self.match(.LeftBrace)) {
            type_def = try self.parseMapType();
            
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);
        } else if (try self.match(.Function)) {
            type_def = try self.functionType();

            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);
        } else if (try self.match(.Identifier)) {
            var user_type_name: Token = self.parser.previous_token.?.clone();
            
            try self.consume(.Identifier, "Expected property name.");
            name = self.parser.previous_token.?.clone();
            constant = try self.identifierConstant(name.?.lexeme);

            var var_type: ?*ObjTypeDef = null;

            // Search for a global with that name
            for (self.globals.items) |global| {
                if (mem.eql(u8, global.name.string, user_type_name.lexeme)) {
                    var_type = global.type_def;
                    break;
                }
            }

            // If none found, create a placeholder
            if (var_type == null) {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, user_type_name.lexeme);

                var_type = try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                });
            }

            type_def = var_type.?;
        }

        if (name != null and constant != null and type_def != null) {
            // Parse default value
            if (try self.match(.Equal)) {
                var expr_type: *ObjTypeDef = try self.expression(false);

                // If only receiver is placeholder, make the assumption that its type if what the expression's return
                if (type_def.?.def_type == .Placeholder and expr_type.def_type != .Placeholder) {
                    type_def.?.resolved_type.?.Placeholder.resolved_def_type = expr_type.def_type;
                    type_def.?.resolved_type.?.Placeholder.resolved_type = expr_type;
                // If only expression is a placeholder, make the inverse assumption
                } else if (expr_type.def_type == .Placeholder and type_def.?.def_type != .Placeholder) {
                    if (self.current.?.scope_depth == 0) {
                        try self.reportError("Unknown expression type.");
                        return null;
                    }

                    expr_type.resolved_type.?.Placeholder.resolved_def_type = type_def.?.def_type;
                    expr_type.resolved_type.?.Placeholder.resolved_type = type_def.?;
                // If both are placeholders, check that they are compatible and enrich them
                } else if (expr_type.def_type == .Placeholder and type_def.?.def_type == .Placeholder
                    and expr_type.resolved_type.?.Placeholder.eql(type_def.?.resolved_type.?.Placeholder)) {
                    if (self.current.?.scope_depth == 0) {
                        try self.reportError("Unknown expression type.");
                        return null;
                    }

                    try PlaceholderDef.link(type_def.?, expr_type, .Assignment);
                // Else do a normal type check
                } else if (!type_def.?.eql(expr_type)) {
                    try self.reportTypeCheck(type_def.?, expr_type, "Wrong variable type");
                }

                // Create property default value
                try self.emitCodeArg(.OP_PROPERTY, constant.?);
            }
            
            try self.consume(.Semicolon, "Expected `;` after property definition.");

            return Property{
                .name = name.?.lexeme,
                .type_def = type_def.?,
            };
        }

        return null;
    }

    // `test` is just like a function but we don't parse arguments and we don't care about its return type
    fn testStatement(self: *Self) !void {
        var function_def_placeholder: ObjTypeDef = .{
            .optional = false,
            .def_type = .Function,
        };

        var test_id: []u8 = try self.allocator.alloc(u8, 11);
        test_id = try std.fmt.bufPrint(test_id, "$test#{}", .{ self.test_count });
        // TODO: this string is never freed

        self.test_count += 1;

        const name_token: Token = Token{
            .token_type = .Test,
            .lexeme = test_id,
            .line = 0,
            .column = 0,
        };

        var slot: usize = try self.declareVariable(&function_def_placeholder, name_token);

        self.markInitialized();

        _ = try self.function(name_token, FunctionType.Test, null);

        try self.defineGlobalVariable(@intCast(u24, slot));
    }

    fn tryCatchStatement(self: *Self) !void {
        // Try block
        _ = try self.function(null, .TryCatch, null);

        try self.consume(.Catch, "Expected `catch` statement after `try` block");

        // Catch block
        _ = try self.function(null, .TryCatch, null);

        try self.emitOpCode(.OP_CATCH);

        // Call try clause immediately
        try self.emitCodeArg(.OP_CALL, 0);
    }

    fn importScript(self: *Self, file_name: []const u8) anyerror!void {
        var import: ?ScriptImport = self.imports.get(file_name);

        if (import == null) {
            // Find and read file
            var file = std.fs.cwd().openFile(file_name, .{}) catch {
                try self.reportError("File not found.");
                return;
            };
            defer file.close();
            
            const source = try self.allocator.alloc(u8, (try file.stat()).size);
            defer self.allocator.free(source);
            
            _ = try file.readAll(source);

            var compiler = Compiler.init(self.allocator, self.strings, self.imports, true);
            defer compiler.deinit();
        
            if (try compiler.compile(source, file_name, self.testing)) |import_function| {
                import = ScriptImport {
                    .function = import_function,
                    .globals = std.ArrayList(Global).init(self.allocator),
                };

                for (compiler.globals.items) |*global| {
                    if (global.exported) {
                        global.*.exported = false;
                     } else {
                        global.*.hidden = true;
                    }

                    try import.?.globals.append(global.*);
                }

                try self.imports.put(file_name, import.?);
            }
        }

        if (import) |imported| {
            try self.emitCodeArg(.OP_CONSTANT, try self.makeConstant(Value { .Obj = imported.function.toObj() }));
            try self.emitOpCode(.OP_IMPORT);

            for (imported.globals.items) |*global| {
                if (!global.hidden) {
                    // Search for name collision
                    // TODO: could be slow if many globals
                    for (self.globals.items) |sglobal| {
                        if (std.mem.eql(u8, sglobal.name.string, global.name.string)) {
                            try self.reportError("Shadowed global");
                            break;
                        }
                    }
                }

                try self.globals.append(global.*);
            }
        } else {
            try self.reportError("Could not compile import.");
        }
    }

    pub const LoadLib = fn () [*:0]const u8;
    pub const LibTypeDef = fn () *ObjTypeDef;

    // TODO: support other platform lib formats
    fn importLib(self: *Self, file_name: []const u8) anyerror!void {
        var lib: ?std.DynLib = std.DynLib.open(file_name) catch null;

        if (lib) |*dlib| {
            // search the openLib function
            var dopenLib = dlib.lookup(LoadLib, "openLib");
            if (dopenLib) |openLib| {
                var exported: [*:0]const u8 = openLib();

                var it = std.mem.split(u8, std.mem.span(exported), ",");
                while (it.next()) |symbol| {
                    // Convert symbol names to zig slices
                    var ssymbol = toNullTerminated(self.allocator, symbol);
                    var symbol_def_name = try self.allocator.alloc(u8, symbol.len + 8);
                    defer self.allocator.free(symbol_def_name);
                    symbol_def_name = try std.fmt.bufPrint(symbol_def_name, "{s}TypeDef", .{ symbol });
                    var symbol_def_name_c = toNullTerminated(self.allocator, symbol_def_name);
                    
                    if (symbol_def_name_c == null) {
                        return std.mem.Allocator.Error.OutOfMemory;
                    }

                    defer self.allocator.free(symbol_def_name_c.?);

                    // Lookup symbol NativeFn
                    var symbol_method = dlib.lookup(NativeFn, ssymbol.?);

                    if (symbol_method == null) {
                        try self.reportError("Could not find lib method symbol");
                        return;
                    }
                    
                    // lookup `<symbol>TypeDef` in the loaded library
                    var symbol_typedef = dlib.lookup(LibTypeDef, symbol_def_name_c.?);
                    
                    if (symbol_typedef == null) {
                        try self.reportError("Could not find lib method typedef");
                        return;
                    }

                    // Create global
                    var global_typedef: ?*ObjTypeDef = (symbol_typedef.?)();

                    if (global_typedef == null) {
                        try self.reportError("Could not find out method typedef");
                    } 

                    var slot: usize = try self.addGlobal(
                        Token{
                            .token_type = .Identifier,
                            .lexeme = symbol,
                            .line = self.parser.current_token.?.line,
                            .column = self.parser.current_token.?.column,
                        },
                        global_typedef.?
                    );

                    self.markInitialized();

                    // Create a ObjNative constant with it
                    var native = try self.allocator.create(ObjNative);
                    native.* = .{
                        .native = symbol_method.?
                    };
                    
                    try self.emitCodeArg(
                        .OP_CONSTANT,
                        try self.makeConstant(Value{ .Obj = native.toObj() })
                    );
                    try self.emitCodeArg(
                        .OP_DEFINE_GLOBAL,
                        @intCast(u24, slot)
                    );
                }
            } else {
                try self.reportError("Could not find `openLib` in library.");
            }
        } else {
            try self.reportError(std.mem.span(dlerror()));
        }
    }

    fn importStatement(self: *Self) anyerror!void {
        try self.consume(.String, "Expected path after `import`.");

        var file_name: []const u8 = self.parser.previous_token.?.lexeme[1..(self.parser.previous_token.?.lexeme.len - 1)];

        try self.consume(.Semicolon, "Expected `;` after import.");

        // TODO: search for a name instead of parsing file extension
        if (mem.eql(u8, file_name[file_name.len - 5..file_name.len], ".buzz")) {
            try self.importScript(file_name);
        } else if (mem.eql(u8, file_name[file_name.len - 6..file_name.len], ".dylib")) {
            try self.importLib(file_name);
        } else {
            try self.reportError("Unknown library extension.");
        }
    }

    fn exportStatement(self: *Self) !void {
        try self.consume(.Identifier, "Expected identifier after `export`.");

        // Search for a global with that name
        for (self.globals.items) |global, index| {
            if (mem.eql(u8, global.name.string, self.parser.previous_token.?.lexeme)) {
                self.globals.items[index].exported = true;

                try self.consume(.Semicolon, "Expected `;` after export.");
                return;
            }
        }

        try self.reportError("Unknown global.");
    }

    fn funDeclaration(self: *Self) !void {
        // Placeholder until `function()` provides all the necessary bits
        var function_def_placeholder: ObjTypeDef = .{
            .optional = false,
            .def_type = .Function,
        };
        
        try self.consume(.Identifier, "Expected function name.");
        var name_token: Token = self.parser.previous_token.?;

        var slot: usize = try self.declareVariable(&function_def_placeholder, name_token);

        self.markInitialized();

        var function_def: *ObjTypeDef = try self.function(name_token, FunctionType.Function, null);
        // Now that we have the full function type, get the local and update its type_def
        if (self.current.?.scope_depth > 0) {
            self.current.?.locals[slot].type_def = function_def;
        } else {
            if (self.globals.items[slot].type_def.def_type == .Placeholder) {
                // Now that the function definition is complete, resolve the eventual placeholder
                try self.resolvePlaceholder(self.globals.items[slot].type_def, function_def);
            } else {
                self.globals.items[slot].type_def = function_def;
            }
        }

        try self.defineGlobalVariable(@intCast(u24, slot));
    }

    // TODO: factorize with varDeclaration + another param
    fn varDeclarationOnly(self: *Self, parsed_type: *ObjTypeDef) !void {
        // If var_type is Class/Object/Enum, we expect instance of them
        var var_type: *ObjTypeDef = switch (parsed_type.def_type) {
            .Object => object: {
                var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                    .ObjectInstance = parsed_type
                };

                break :object try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .ObjectInstance,
                    .resolved_type = resolved_type
                });
            },
            .Enum => enum_instance: {
                var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                    .EnumInstance = parsed_type
                };

                break :enum_instance try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .EnumInstance,
                    .resolved_type = resolved_type
                });
            },
            else => parsed_type
        };

        try self.emitOpCode(.OP_NULL);

        try self.defineGlobalVariable(
            @intCast(u24, try self.parseVariable(var_type, "Expected variable name."))
        );
    }

    fn varDeclaration(self: *Self, parsed_type: *ObjTypeDef, in_list: bool) !void {
        // If var_type is Class/Object/Enum, we expect instance of them
        var var_type: *ObjTypeDef = switch (parsed_type.def_type) {
            .Object => object: {
                var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                    .ObjectInstance = parsed_type
                };

                break :object try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .ObjectInstance,
                    .resolved_type = resolved_type
                });
            },
            .Enum => enum_instance: {
                var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
                    .EnumInstance = parsed_type
                };

                break :enum_instance try self.getTypeDef(.{
                    .optional = try self.match(.Question),
                    .def_type = .EnumInstance,
                    .resolved_type = resolved_type
                });
            },
            else => parsed_type
        };

        const slot: usize = try self.parseVariable(var_type, "Expected variable name.");

        if (try self.match(.Equal)) {
            var expr_type: *ObjTypeDef = try self.expression(false);
            
            // If only receiver is placeholder, make the assumption that its type if what the expression's return
            if (var_type.def_type == .Placeholder and expr_type.def_type != .Placeholder) {
                var_type.resolved_type.?.Placeholder.resolved_def_type = expr_type.def_type;
                var_type.resolved_type.?.Placeholder.resolved_type = expr_type;
            // If only expression is a placeholder, make the inverse assumption
            } else if (expr_type.def_type == .Placeholder and var_type.def_type != .Placeholder) {
                if (self.current.?.scope_depth == 0) {
                    try self.reportError("Unknown expression type.");
                    return;
                }

                expr_type.resolved_type.?.Placeholder.resolved_def_type = var_type.def_type;
                expr_type.resolved_type.?.Placeholder.resolved_type = var_type;
            // If both are placeholders, check that they are compatible and enrich them
            } else if (expr_type.def_type == .Placeholder and var_type.def_type == .Placeholder
                and expr_type.resolved_type.?.Placeholder.eql(var_type.resolved_type.?.Placeholder)) {
                if (self.current.?.scope_depth == 0) {
                    try self.reportError("Unknown expression type.");
                    return;
                }

                try PlaceholderDef.link(var_type, expr_type, .Assignment);
            // Else do a normal type check
            } else if (!var_type.eql(expr_type)) {
                try self.reportTypeCheck(var_type, expr_type, "Wrong variable type");
            }
        } else {
            try self.emitOpCode(.OP_NULL);
        }

        if (!in_list) {
            try self.consume(.Semicolon, "Expected `;` after variable declaration.");
        }

        try self.defineGlobalVariable(@intCast(u24, slot));
    }

    fn parseListType(self: *Self) !*ObjTypeDef {
        var list_item_type: *ObjTypeDef = try self.parseTypeDef();

        try self.consume(.RightBracket, "Expected `]` after list type.");

        var list_def = ObjList.ListDef.init(self.allocator, list_item_type);
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .List = list_def
        };

        return try self.getTypeDef(.{
            .optional = try self.match(.Question),
            .def_type = .List,
            .resolved_type = resolved_type
        });
    }

    fn listDeclaration(self: *Self) !void {
        try self.varDeclaration(try self.parseListType(), false);
    }

    fn parseMapType(self: *Self) !*ObjTypeDef {
        var key_type: *ObjTypeDef = try self.parseTypeDef();

        try self.consume(.Comma, "Expected `,` after key type.");

        var value_type: *ObjTypeDef = try self.parseTypeDef();

        try self.consume(.RightBrace, "Expected `}` after value type.");

        var map_def = ObjMap.MapDef{ .key_type = key_type, .value_type = value_type };
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .Map = map_def
        };

        return try self.getTypeDef(.{
            .optional = try self.match(.Question),
            .def_type = .Map,
            .resolved_type = resolved_type
        });
    }

    fn mapDeclaration(self: *Self) !void {
        try self.varDeclaration(try self.parseMapType(), false);
    }

    fn enumDeclaration(self: *Self) !void {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Enum must be defined at top-level.");
            return;
        }

        var enum_case_type: *ObjTypeDef = undefined;
        var case_type_picked: bool = false;
        if (try self.match(.LeftParen)) {
            enum_case_type = try self.parseTypeDef();
            try self.consume(.RightParen, "Expected `)` after enum type.");

            case_type_picked = true;
        } else {
            enum_case_type = try self.getTypeDef(.{
                .optional = false,
                .def_type = .Number
            });
        }

        enum_case_type = (try self.toInstance(enum_case_type)) orelse enum_case_type;

        try self.consume(.Identifier, "Expected enum name.");
        var enum_name: Token = self.parser.previous_token.?.clone();

        for (self.globals.items) |global| {
            if (mem.eql(u8, global.name.string, enum_name.lexeme)) {
                try self.reportError("Global with that name already exists.");
                break;
            }
        }

        var enum_def: ObjEnum.EnumDef = ObjEnum.EnumDef.init(
            self.allocator,
            try copyStringRaw(self.strings, self.allocator, enum_name.lexeme),
            enum_case_type,
        );

        var enum_resolved: ObjTypeDef.TypeUnion = .{
            .Enum = enum_def
        };

        var enum_type: *ObjTypeDef = try self.allocator.create(ObjTypeDef);
        enum_type.* = ObjTypeDef {
            .optional = false,
            .def_type = .Enum,
            .resolved_type = enum_resolved
        };

        const constant: u24 = try self.makeConstant(Value { .Obj = enum_type.toObj() });

        const slot: usize = try self.declareVariable(
            enum_type,
            enum_name
        );

        try self.emitCodeArg(.OP_ENUM, constant);
        try self.emitCodeArg(.OP_DEFINE_GLOBAL, @intCast(u24, slot));

        self.markInitialized();

        _ = try self.namedVariable(enum_name, false);

        try self.consume(.LeftBrace, "Expected `{` before enum body.");
        
        var case_index: f64 = 0;
        while (!self.check(.RightBrace) and !self.check(.Eof)) : (case_index += 1) {
            if (case_index > 255) {
                try self.reportError("Too many enum cases.");
            }

            try self.consume(.Identifier, "Expected case name.");
            const case_name: []const u8 = self.parser.previous_token.?.lexeme;

            if (case_type_picked) {
                try self.consume(.Equal, "Expected `=` after case name.");

                var parsed_type: *ObjTypeDef = try self.expression(false);

                if (!parsed_type.eql(enum_case_type)) {
                    try self.reportTypeCheck(enum_case_type, parsed_type, "Bad enum case type.");
                }

                if (enum_case_type.def_type == .Placeholder and parsed_type.def_type != .Placeholder) {
                    enum_case_type.resolved_type.?.Placeholder.resolved_def_type = parsed_type.def_type;
                    enum_case_type.resolved_type.?.Placeholder.resolved_type = parsed_type;
                    if (!enum_case_type.resolved_type.?.Placeholder.isCoherent()) {
                        try self.reportError("Bad enum case type.");
                    }
                }

                if (parsed_type.def_type == .Placeholder and enum_case_type.def_type != .Placeholder) {
                    parsed_type.resolved_type.?.Placeholder.resolved_def_type = enum_case_type.def_type;
                    parsed_type.resolved_type.?.Placeholder.resolved_type = enum_case_type;
                    if (!parsed_type.resolved_type.?.Placeholder.isCoherent()) {
                        try self.reportError("Bad enum case expression type.");
                    }
                }

                try self.emitOpCode(.OP_ENUM_CASE);
            } else {
                assert(enum_case_type.def_type == .Number);

                try self.emitConstant(Value{ .Number = case_index });
                try self.emitOpCode(.OP_ENUM_CASE);
            }

            // TODO: how to not force a comma at last case?
            try self.consume(.Comma, "Expected `,` after case definition.");

            try enum_type.resolved_type.?.Enum.cases.append(case_name);
        }

        try self.consume(.RightBrace, "Expected `}` after enum body.");

        if (case_index == 0) {
            try self.reportError("Enum must have at least one case.");
        }

        try self.emitOpCode(.OP_POP);
    }

    fn objectDeclaration(self: *Self, is_class: bool) !void {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Object must be defined at top-level.");
            return;
        }

        try self.consume(.Identifier, "Expected object name.");
        var object_name: Token = self.parser.previous_token.?.clone();

        for (self.globals.items) |global| {
            if (mem.eql(u8, global.name.string, object_name.lexeme)) {
                try self.reportError("Global with that name already exists.");
                break;
            }
        }

        var object_type: *ObjTypeDef = try self.allocator.create(ObjTypeDef);
        object_type.* = .{
            .optional = false,
            .def_type = .Object,
            .resolved_type = .{
                .Object = ObjObject.ObjectDef.init(
                    self.allocator,
                    try copyStringRaw(self.strings, self.allocator, object_name.lexeme)
                ),
            }
        };

        if (is_class) {
            object_type.resolved_type.?.Object.inheritable = true;
            // TODO: parse super class here
        }

        var constant: u24 = try self.makeConstant(Value { .Obj = object_type.resolved_type.?.Object.name.toObj() });

        const slot: usize = try self.declareVariable(
            object_type,
            object_name
        );

        try self.emitCodeArg(.OP_OBJECT, constant);
        try self.emitCodeArg(.OP_DEFINE_GLOBAL, @intCast(u24, slot));

        self.markInitialized();

        var object_compiler: ObjectCompiler = .{
            .name = object_name,
            .enclosing = if (self.current_object != null) self.current_object.?.enclosing else null,
        };
        
        self.current_object = object_compiler;

        _ = try self.namedVariable(object_name, false);

        try self.consume(.LeftBrace, "Expected `{` before object body.");
        self.beginScope();

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            if (try self.match(.Fun)) {
                var method_def: *ObjTypeDef = try self.method(object_type);
                var method_name: []const u8 = method_def.resolved_type.?.Function.name.string;
                
                if (object_type.resolved_type.?.Object.methods.get(method_name) != null) {
                    try self.reportError("A method with that name already exists.");
                }

                // Does a placeholder exists for this name ?
                if (object_type.resolved_type.?.Object.placeholders.get(method_name)) |placeholder| {
                    try self.resolvePlaceholder(placeholder, method_def);

                    // Now we know the placeholder was a method
                    _ = object_type.resolved_type.?.Object.placeholders.remove(method_name);
                }

                try object_type.resolved_type.?.Object.methods.put(
                    method_name,
                    method_def,
                );
            } else if (try self.property()) |prop| {
                if (object_type.resolved_type.?.Object.fields.get(prop.name) != null) {
                    try self.reportError("A property with that name already exists.");
                }

                // Does a placeholder exists for this name ?
                if (object_type.resolved_type.?.Object.placeholders.get(prop.name)) |placeholder| {
                    try self.resolvePlaceholder(placeholder, prop.type_def);

                    // Now we know the placeholder was a field
                    _ = object_type.resolved_type.?.Object.placeholders.remove(prop.name);
                }

                try object_type.resolved_type.?.Object.fields.put(
                    prop.name,
                    prop.type_def,
                );
            } else {
                try self.reportError("Expected either method or property.");
                return;
            }
        }

        // TODO: ERROR IN PLACEHOLDERS LEFT

        try self.endScope();
        try self.consume(.RightBrace, "Expected `}` after object body.");

        try self.emitOpCode(.OP_POP);

        self.current_object =  if (self.current_object != null and self.current_object.?.enclosing != null) self.current_object.?.enclosing.?.* else null;
    }

    fn expressionStatement(self: *Self, hanging: bool) !void {
        _ = try self.expression(hanging);
        try self.consume(.Semicolon, "Expected `;` after expression.");
        try self.emitOpCode(.OP_POP);
    }

    fn breakStatement(self: *Self) !usize {
        try self.consume(.Semicolon, "Expected `;` after `break`.");

        return try self.emitJump(.OP_JUMP);
    }

    fn forEachStatement(self: *Self) !void {
        try self.consume(.LeftParen, "Expected `(` after `foreach`.");

        self.beginScope();

        var key_type: *ObjTypeDef = try self.parseTypeDef();
        try self.varDeclarationOnly(key_type);
        var key_name: Token = self.parser.previous_token.?;

        var value_type: ?*ObjTypeDef = null;
        
        if (try self.match(.Comma)) {
            value_type = try self.parseTypeDef();
            try self.varDeclarationOnly(value_type.?);
        }

        try self.consume(.In, "Expected `in` after `foreach` variables.");

        var iterable_type: *ObjTypeDef = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `foreach`.");

        // TODO: object with `next` method
        if (iterable_type.def_type != .List
            and iterable_type.def_type != .Map
            and iterable_type.def_type != .Enum
            // TODO: better placeholder test + enrich it
            and iterable_type.def_type != .Placeholder) {
            try self.reportError("Not iterable.");
        }

        // Check key and value type
        switch (iterable_type.def_type) {
            .Enum => {
                if (value_type != null) {
                    try self.reportError("Only one variable allowed when iterating over Enum.");
                }

                if ((key_type.def_type != .Enum
                    or !key_type.eql(iterable_type))
                    and key_type.def_type != .Placeholder) {
                    // TODO: better placeholder test + enrich it
                    try self.reportTypeCheck(iterable_type, key_type, "Should be instance of");
                }
            },
            .List => {
                if (value_type == null) {
                    try self.reportError("Missing value variable.");
                }

                if (key_type.def_type != .Number
                    and key_type.def_type != .Placeholder) {
                    // TODO: better placeholder test + enrich it
                    try self.reportError("List key must be `num`.");
                }

                if (!value_type.?.eql(iterable_type.resolved_type.?.List.item_type)) {
                    try self.reportTypeCheck(iterable_type.resolved_type.?.List.item_type, value_type.?, "Wrong value type");
                }
            },
            .Map => unreachable,
            else => {} // TODO: what if placeholder?
        }

        var key_slot: u24 = @intCast(u24, (try self.resolveLocal(self.current.?, key_name)).?);

        const loop_start: usize = self.current.?.function.chunk.code.items.len;

        // Calls `next` and update key and value locals
        try self.emitOpCode(.OP_FOREACH);

        // If next key is null, exit loop
        try self.emitCodeArg(.OP_GET_LOCAL, key_slot);
        try self.emitOpCode(.OP_NULL);
        try self.emitOpCode(.OP_EQUAL);
        try self.emitOpCode(.OP_NOT);
        const exit_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitOpCode(.OP_POP);

        try self.consume(.LeftBrace, "Expected `{` after `foreach` definition.");

        var breaks: std.ArrayList(usize) = try self.block();
        defer breaks.deinit();

        try self.emitLoop(loop_start);

        try self.endScope();

        try self.patchJump(exit_jump);
        try self.emitOpCode(.OP_POP);

        // Patch breaks
        for (breaks.items) |jump| {
            try self.patchJump(jump);
        }
    }

    fn forStatement(self: *Self) !void {
        try self.consume(.LeftParen, "Expected `(` after `for`.");

        self.beginScope();

        while (!self.check(.Semicolon) and !self.check(.Eof)) {
            try self.varDeclaration(try self.parseTypeDef(), true);
            
            if (!self.check(.Semicolon)) {
                try self.consume(.Comma, "Expected `,` after for loop variable");
            }
        }

        try self.consume(.Semicolon, "Expected `;` after for loop variables.");

        const loop_start: usize = self.current.?.function.chunk.code.items.len;

        var expr_type: *ObjTypeDef = try self.expression(false);

        if (expr_type.def_type != .Bool and expr_type.def_type != .Placeholder) {
            // TODO: should use Placeholder.isBasicType but breaks zig
            try self.reportError("Expected `bool` condition.");
        }

        try self.consume(.Semicolon, "Expected `;` after for loop condition.");

        const exit_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitOpCode(.OP_POP);

        // Jump over expressions which will be executed at end of loop
        var body_jump: usize = try self.emitJump(.OP_JUMP);

        const expr_loop: usize = self.current.?.function.chunk.code.items.len;
        while (!self.check(.RightParen) and !self.check(.Eof)) {
            _ = try self.expression(false);

            if (!self.check(.RightParen)) {
                try self.consume(.Comma, "Expected `,` after for loop expression");
            }
        }

        try self.consume(.RightParen, "Expected `)` after `for` expressions.");

        try self.emitLoop(loop_start);

        try self.patchJump(body_jump);

        try self.consume(.LeftBrace, "Expected `{` after `for` definition.");

        var breaks: std.ArrayList(usize) = try self.block();
        defer breaks.deinit();

        try self.emitLoop(expr_loop);

        try self.endScope();

        try self.patchJump(exit_jump);
        try self.emitOpCode(.OP_POP);

        // Patch breaks
        for (breaks.items) |jump| {
            try self.patchJump(jump);
        }
    }

    fn whileStatement(self: *Self) !void {
        const loop_start: usize = self.current.?.function.chunk.code.items.len;

        try self.consume(.LeftParen, "Expected `(` after `while`.");

        var parsed_type: *ObjTypeDef = try self.expression(false);
        if (parsed_type.def_type != .Bool and parsed_type.def_type != .Placeholder) {
            try self.reportTypeCheck(try self.getTypeDef(ObjTypeDef{ .optional = false, .def_type = .Bool }), parsed_type, "Bad `while` condition");
        }

        try self.consume(.RightParen, "Expected `)` after `while` condition.");

        const exit_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitOpCode(.OP_POP);

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");
        self.beginScope();
        
        var breaks: std.ArrayList(usize) = try self.block();
        defer breaks.deinit();

        try self.endScope();

        try self.emitLoop(loop_start);
        try self.patchJump(exit_jump);

        try self.emitOpCode(.OP_POP);

        // Patch breaks
        for (breaks.items) |jump| {
            try self.patchJump(jump);
        }
    }

    fn doUntilStatement(self: *Self) !void {
        const loop_start: usize = self.current.?.function.chunk.code.items.len;
        
        try self.consume(.LeftBrace, "Expected `{` after `do`.");
        self.beginScope();
        
        var breaks: std.ArrayList(usize) = try self.block();
        defer breaks.deinit();

        try self.endScope();

        try self.consume(.Until, "Expected `until` after `do` block.");

        try self.consume(.LeftParen, "Expected `(` after `until`.");

        var parsed_type: *ObjTypeDef = try self.expression(false);
        if (parsed_type.def_type != .Bool and parsed_type.def_type != .Placeholder) {
            try self.reportTypeCheck(try self.getTypeDef(ObjTypeDef{ .optional = false, .def_type = .Bool }), parsed_type, "Bad `while` condition");
        }

        try self.consume(.RightParen, "Expected `)` after `until` condition.");

        try self.emitOpCode(.OP_NOT);
        const exit_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitOpCode(.OP_POP);

        try self.emitLoop(loop_start);
        try self.patchJump(exit_jump);

        // Patch breaks
        for (breaks.items) |jump| {
            try self.patchJump(jump);
        }

        try self.emitOpCode(.OP_POP);
    }

    fn ifStatement(self: *Self) anyerror!std.ArrayList(usize) {
        try self.consume(.LeftParen, "Expected `(` after `if`.");

        var parsed_type: *ObjTypeDef = try self.expression(false);
        if (parsed_type.def_type != .Bool and parsed_type.def_type != .Placeholder) {
            try self.reportTypeCheck(try self.getTypeDef(ObjTypeDef{ .optional = false, .def_type = .Bool }), parsed_type, "Bad `if` condition");
        }
        
        try self.consume(.RightParen, "Expected `)` after `if` condition.");

        const then_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        try self.emitOpCode(.OP_POP);

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");
        self.beginScope();
        var breaks: std.ArrayList(usize) = try self.block();
        try self.endScope();

        const else_jump: usize = try self.emitJump(.OP_JUMP);
        
        try self.patchJump(then_jump);
        try self.emitOpCode(.OP_POP);

        if (try self.match(.Else)) {
            if (try self.match(.If)) {
                var else_if_breaks: std.ArrayList(usize) = try self.ifStatement();
                try breaks.appendSlice(else_if_breaks.items);
                else_if_breaks.deinit();
            } else {
                try self.consume(.LeftBrace, "Expected `{` after `else`.");
                self.beginScope();
                var else_breaks: std.ArrayList(usize) = try self.block();
                try breaks.appendSlice(else_breaks.items);
                else_breaks.deinit();
                try self.endScope();
            }
        }

        try self.patchJump(else_jump);

        return breaks;
    }

    inline fn defineGlobalVariable(self: *Self, slot: u24) !void {
        self.markInitialized();

        if (self.current.?.scope_depth > 0) {
            return;
        }

        try self.emitCodeArg(.OP_DEFINE_GLOBAL, slot);
    }

    fn declarePlaceholder(self: *Self, name: Token) !usize {
        if (self.current.?.scope_depth == 0) {
            try self.reportError("Unknown expression type.");
            return 0;
        }

        var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
            .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
        };
        placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, name.lexeme);

        const placeholder_type = try self.getTypeDef(.{
            .optional = false,
            .def_type = .Placeholder,
            .resolved_type = placeholder_resolved_type
        });

        const global: usize = try self.addGlobal(name, placeholder_type);
        // markInitialized but we don't care what depth we are in
        self.globals.items[global].initialized = true;

        if (builtin.mode == .Debug) {
            std.debug.warn(
                "\u{001b}[33m[{}:{}] Warning: defining global placeholder for `{s}` at {}\u{001b}[0m\n",
                .{
                    self.parser.previous_token.?.line + 1,
                    self.parser.previous_token.?.column + 1,
                    name.lexeme,
                    global
                }
            );
        }

        try self.defineGlobalVariable(@intCast(u24, global));

        return global;
    }

    const ParsedArg = struct {
        name: ?Token,
        arg_type: *ObjTypeDef,
    };

    // Like argument list but we parse all the arguments and populate the placeholder type with it
    // TODO: factorize with `argumentList`
    fn placeholderArgumentList(self: *Self, placeholder_type: *ObjTypeDef) !u8 {
        // If the placeholder guessed an actual full type, use argumentList with it
        if (placeholder_type.resolved_type.?.Placeholder.resolved_type) |resolved_type| {
            if (resolved_type.def_type == .Function) {
                return try self.argumentList(resolved_type.resolved_type.?.Function.parameters);
            } else if (resolved_type.def_type == .Object) {
                if (resolved_type.resolved_type.?.Object.methods.get("init")) |init_method| {
                    return try self.argumentList(init_method.resolved_type.?.Function.parameters);
                }

                // No user-defined init method, no arguments
                // TODO: flesh this out when we define how default constructors work
                return try self.argumentList(null);
            }
        } else if (placeholder_type.resolved_type.?.Placeholder.resolved_parameters) |parameters| {
            return try self.argumentList(parameters);
        }

        // Otherwise parse the argument list as we find it and enrich placeholder assumptions
        var parsed_arguments = std.StringArrayHashMap(*ObjTypeDef).init(self.allocator);
        var arg_count: u8 = 0;
        while (!self.check(.RightParen)) {
            var hanging = false;
            var arg_name: ?Token = null;
            if (try self.match(.Identifier)) {
                arg_name = self.parser.previous_token.?;
            }

            if (arg_count != 0 and arg_name == null) {
                try self.reportError("Expected argument name.");
                break;
            }

            if (arg_name != null) {
                if (arg_count == 0) {
                    if (try self.match(.Colon)) {
                        hanging = false;
                    } else {
                        // The identifier we just parsed is not the argument name but the start of an expression
                        hanging = true;
                    }
                } else {
                    try self.consume(.Colon, "Expected `:` after argument name.");
                }
            }

            var expr_type: *ObjTypeDef = try self.expression(hanging);

            // If hanging, the identifier is NOT the argument name
            try parsed_arguments.put(if (arg_name != null and !hanging) arg_name.?.lexeme else "{{first}}", expr_type);

            if (arg_count == 255) {
                try self.reportError("Can't have more than 255 arguments.");
                
                return 0;
            }

            arg_count += 1;
            
            if (!(try self.match(.Comma))) {
                break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after arguments.");

        placeholder_type.resolved_type.?.Placeholder.resolved_parameters = parsed_arguments;

        return arg_count;
    }

    fn argumentList(self: *Self, function_parameters: ?std.StringArrayHashMap(*ObjTypeDef)) !u8 {
        if (function_parameters) |parameters| {
            var arg_count: u8 = 0;
            var parameter_keys: [][]const u8 = parameters.keys();
            
            if (!self.check(.RightParen)) {
                var parsed_arguments = std.ArrayList(ParsedArg).init(self.allocator);
                defer parsed_arguments.deinit();

                while (arg_count < parameter_keys.len) {
                    var hanging = false;
                    var arg_name: ?Token = null;
                    if (try self.match(.Identifier)) {
                        arg_name = self.parser.previous_token.?;
                    }

                    if (arg_count != 0 and arg_name == null) {
                        try self.reportError("Expected argument name.");
                        break;
                    }

                    if (arg_name != null) {
                        if (arg_count == 0) {
                            if (try self.match(.Colon)) {
                                hanging = false;
                            } else {
                                // The identifier we just parsed is not the argument name but the start of an expression
                                hanging = true;
                            }
                        } else {
                            try self.consume(.Colon, "Expected `:` after argument name.");
                        }
                    }

                    var expr_type: *ObjTypeDef = try self.expression(hanging);

                    try parsed_arguments.append(ParsedArg{
                        .name = if (!hanging) arg_name else null, // If hanging, the identifier is NOT the argument name
                        .arg_type = expr_type,
                    });

                    if (arg_count == 255) {
                        try self.reportError("Can't have more than 255 arguments.");
                        
                        return 0;
                    }

                    arg_count += 1;
                    
                    if (!(try self.match(.Comma))) {
                        break;
                    }
                }

                // Now that we parsed all arguments, check they match function definition
                var order_differs = false;
                for (parsed_arguments.items) |argument, index| {
                    if (argument.name) |name| {
                        if (!order_differs and !mem.eql(u8, parameter_keys[index], name.lexeme)) {
                            order_differs = true;
                        }

                        var param_type = parameters.get(name.lexeme);

                        // Does an argument with that name exists?
                        if (param_type) |ptype| {
                            // Is the argument type correct?
                            if (!ptype.eql(argument.arg_type)) {
                                var wrong_type_str: []u8 = try self.allocator.alloc(u8, 100);
                                var param_type_str: []const u8 = try ptype.toString(self.allocator);
                                var expr_type_str: []const u8 = try argument.arg_type.toString(self.allocator);
                                defer {
                                    self.allocator.free(wrong_type_str);
                                    self.allocator.free(param_type_str);
                                    self.allocator.free(expr_type_str);
                                }

                                try self.reportError(
                                    try std.fmt.bufPrint(
                                        wrong_type_str,
                                        "Expected argument `{s}` to be `{s}`, got `{s}`.",
                                        .{
                                            name, param_type_str, expr_type_str
                                        }
                                    )
                                );

                                return 0;
                            }
                        } else {
                            var wrong_name_str: []u8 = try self.allocator.alloc(u8, 100);
                            defer self.allocator.free(wrong_name_str);

                            try self.reportError(
                                try std.fmt.bufPrint(
                                    wrong_name_str,
                                    "Argument named `{s}`, doesn't exist.",
                                    .{ name.lexeme }
                                )
                            );

                            return 0;
                        }
                    } else {
                        assert(index == 0);

                        // First argument without name, check its type
                        var param_type: *ObjTypeDef = parameters.get(parameter_keys[0]).?;
                        if (!param_type.eql(argument.arg_type)) {
                            var wrong_type_str: []u8 = try self.allocator.alloc(u8, 100);
                            var param_type_str: []const u8 = try param_type.toString(self.allocator);
                            var expr_type_str: []const u8 = try argument.arg_type.toString(self.allocator);
                            defer {
                                self.allocator.free(wrong_type_str);
                                self.allocator.free(param_type_str);
                                self.allocator.free(expr_type_str);
                            }
                            var name: []const u8 = parameter_keys[0];

                            try self.reportError(
                                try std.fmt.bufPrint(
                                    wrong_type_str,
                                    "Expected argument `{s}` to be `{s}`, got `{s}`.",
                                    .{
                                        name, param_type_str, expr_type_str
                                    }
                                )
                            );

                            return 0;
                        }
                    }
                }

                // If order differ we emit OP_SWAP so that OP_CALL know where its arguments are
                if (order_differs) {
                    var already_swapped = std.AutoHashMap(u8, u8).init(self.allocator);
                    defer already_swapped.deinit();

                    for (parsed_arguments.items) |argument, index| {
                        assert(argument.name != null or index == 0);

                        var arg_name: []const u8 = if (argument.name) |uname| uname.lexeme else parameter_keys[0];
                        if (!mem.eql(u8 , arg_name, parameter_keys[index])) {
                            // Search for the correct index for this argument
                            for (parameter_keys) |param_name, pindex| {
                                if (mem.eql(u8, param_name, arg_name)) {
                                    var already: ?u8 = already_swapped.get(@intCast(u8, pindex));
                                    if (already != null and already.? == @intCast(u8, index)) {
                                        break;
                                    }

                                    // TODO: both OP_SWAP args could fit in a 32 bit instruction
                                    try self.emitCodeArg(.OP_SWAP, @intCast(u24, index));
                                    // to where it should be
                                    try self.emit(@intCast(u32, pindex));

                                    try already_swapped.put(@intCast(u8, index), @intCast(u8, pindex));
                                }
                            }
                        }
                    }
                }
            }

            if (parameter_keys.len != arg_count) {
                var arity: []u8 = try self.allocator.alloc(u8, 100);
                defer self.allocator.free(arity);

                try self.reportError(try std.fmt.bufPrint(arity, "Expected {} arguments, got {}", .{ parameter_keys.len, arg_count }));

                return 0;
            }

            try self.consume(.RightParen, "Expected `)` after arguments.");
            return arg_count;
        }

        // Empty argument list
        try self.consume(.RightParen, "Expected `)` after arguments.");
        return 0;
    }

    fn unary(self: *Self, _: bool) anyerror!*ObjTypeDef {
        var operator_type: TokenType = self.parser.previous_token.?.token_type;
        
        var parsed_type: *ObjTypeDef = try self.parsePrecedence(.Unary, false);

        switch (operator_type) {
            .Bang => try self.emitOpCode(.OP_NOT),
            .Minus => try self.emitOpCode(.OP_NEGATE),
            else => {},
        }

        return parsed_type;
    }

    fn parseInterpolation(self: *Self, str: []const u8) !bool {
        var interp_start: ?usize = null;
        var previous_interp: ?usize = null;
        var depth: usize = 0;
        var parsed_interp: bool = false;
        var interp_count: usize = 0;
        for (str) |char, index| {
            // std.debug.warn("'{c}', {}, start {}, depth {}\n", .{char, index, interp_start, depth});
            // We finished parsing an interpolation
            if (interp_start != null and char == '}' and depth == 0) {
                var expr: []const u8 = str[interp_start.? + 1..index];

                var expr_scanner = Scanner.init(expr);

                // Replace compiler scanner with one that only looks at that substring
                var scanner = self.scanner;
                self.scanner = expr_scanner;
                var parser = self.parser;
                self.parser = .{};

                try self.advance();

                // Parse expression
                _ = try self.expression(false);
                try self.emitOpCode(.OP_TO_STRING);

                // Put back compiler's scanner
                self.scanner = scanner;
                self.parser = parser;

                // Reset interp state
                interp_start = null;

                parsed_interp = true;

                try self.emitOpCode(.OP_ADD);

                interp_count += 1;
                previous_interp = index;
            } else if (interp_start != null and char == '}') {
                depth -= 1;
            } else if (interp_start == null and char == '{') {
                // Push regular string that lives before or between the last interpolation
                if (previous_interp) |previous| {
                    try self.emitConstant(Value {
                        .Obj = (try copyStringRaw(
                            self.strings,
                            self.allocator,
                            str[previous + 1..index]
                        )).toObj()
                    });

                    try self.emitOpCode(.OP_ADD);
                } else {
                    try self.emitConstant(Value {
                        .Obj = (try copyStringRaw(
                            self.strings,
                            self.allocator,
                            str[0..index]
                        )).toObj()
                    });
                }

                interp_start = index;
                interp_count += 1;
            } else if (interp_start != null and char == '{') {
                depth += 1;
            }
        }

        if (parsed_interp
            and interp_start == null
            and previous_interp != null
            and previous_interp.? < str.len) {
            try self.emitConstant(Value {
                .Obj = (try copyStringRaw(
                    self.strings,
                    self.allocator,
                    str[previous_interp.? + 1..str.len]
                )).toObj()
            });

            try self.emitOpCode(.OP_ADD);
        }

        return parsed_interp;
    }

    fn string(self: *Self, _: bool) anyerror!*ObjTypeDef {
        if (!try self.parseInterpolation(self.parser.previous_token.?.literal_string.?)) {
            try self.emitConstant(Value {
                .Obj = (try copyStringRaw(self.strings, self.allocator, self.parser.previous_token.?.literal_string.?)).toObj()
            });
        }

        return try self.getTypeDef(.{
            .def_type = .String,
            .optional = false,
        });
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) anyerror!*ObjTypeDef {
        var get_op: OpCode = undefined;
        var set_op: OpCode = undefined;

        var var_def: *ObjTypeDef = undefined;

        var arg: ?usize = try self.resolveLocal(self.current.?, name);
        if (arg) |resolved| {
            // TODO: should resolveLocal return the local itself?
            var_def = self.current.?.locals[resolved].type_def;

            get_op = .OP_GET_LOCAL;
            set_op = .OP_SET_LOCAL;
        } else {
            arg = try self.resolveUpvalue(self.current.?, name);
            if (arg) |resolved| {
                var_def = self.current.?.enclosing.?.locals[self.current.?.upvalues[resolved].index].type_def;

                get_op = .OP_GET_UPVALUE;
                set_op = .OP_SET_UPVALUE;
            } else {
                get_op = .OP_GET_GLOBAL;
                set_op = .OP_SET_GLOBAL;

                arg = (try self.resolveGlobal(name)) orelse (try self.declarePlaceholder(name));

                var_def = self.globals.items[arg.?].type_def;
            }
        }

        if (can_assign and try self.match(.Equal)) {
            var expr_type: *ObjTypeDef = try self.expression(false);

            if (!expr_type.eql(var_def)) {
                try self.reportTypeCheck(var_def, expr_type, "Wrong value type");
            }

            try self.emitCodeArg(set_op, @intCast(u24, arg.?));
        } else {
            try self.emitCodeArg(get_op, @intCast(u24, arg.?));
        }

        return var_def;
    }

    fn variable(self: *Self, can_assign: bool) anyerror!*ObjTypeDef {
        return try self.namedVariable(self.parser.previous_token.?, can_assign);
    }

    fn and_(self: *Self, _: bool, left_operand_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        if (left_operand_type.def_type == .Placeholder) {
            left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Bool;
        } else if (left_operand_type.def_type != .Bool) {
            try self.reportError("`and` expects operands to be `bool`");
        }

        const end_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);

        try self.emitOpCode(.OP_POP);
        var right_operand_type: *ObjTypeDef = try self.parsePrecedence(.And, false);

        if (right_operand_type.def_type == .Placeholder) {
            right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Bool;
        } else if (right_operand_type.def_type != .Bool) {
            try self.reportError("`and` expects operands to be `bool`");
        }
        
        try self.patchJump(end_jump);

        return right_operand_type;
    }

    fn or_(self: *Self, _: bool, left_operand_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        if (left_operand_type.def_type == .Placeholder) {
            left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Bool;
        } else if (left_operand_type.def_type != .Bool) {
            try self.reportError("`or` expects operands to be `bool`");
        }

        const else_jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        const end_jump: usize = try self.emitJump(.OP_JUMP);

        try self.patchJump(else_jump);
        try self.emitOpCode(.OP_POP);

        var right_operand_type: *ObjTypeDef = try self.parsePrecedence(.Or, false);

        if (right_operand_type.def_type == .Placeholder) {
            right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Bool;
        } else if (right_operand_type.def_type != .Bool) {
            try self.reportError("`and` expects operands to be `bool`");
        }

        try self.patchJump(end_jump);

        return right_operand_type;
    }

    fn binary(self: *Self, _: bool, left_operand_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        const operator_type: TokenType = self.parser.previous_token.?.token_type;
        const rule: ParseRule = getRule(operator_type);

        var right_operand_type: *ObjTypeDef = try self.parsePrecedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1), false);

        if (!left_operand_type.eql(right_operand_type)
            and left_operand_type.def_type != .Placeholder
            and right_operand_type.def_type != .Placeholder) {
            try self.reportTypeCheck(left_operand_type, right_operand_type, "Type mismatch.");
        }

        switch (operator_type) {
            .QuestionQuestion => {
                if (!left_operand_type.optional
                    or (left_operand_type.def_type == .Placeholder
                        and left_operand_type.resolved_type.?.Placeholder.resolved_type != null
                        and !left_operand_type.resolved_type.?.Placeholder.resolved_type.?.optional)) {
                    try self.reportError("Not an optinal");
                }

                try self.emitOpCode(.OP_NULL_OR);

                return right_operand_type;
            },
            .Greater => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_GREATER);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },
            .Less => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_LESS);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },
            .GreaterEqual => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_LESS);
                try self.emitOpCode(.OP_NOT);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },
            .LessEqual => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_GREATER);
                try self.emitOpCode(.OP_NOT);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },
            .BangEqual => {
                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                try self.emitOpCode(.OP_EQUAL);
                try self.emitOpCode(.OP_NOT);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },
            .EqualEqual => {
                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                try self.emitOpCode(.OP_EQUAL);

                return self.getTypeDef(ObjTypeDef{
                    .optional = false,
                    .def_type = .Bool,
                });
            },

            .Plus => {
                if (left_operand_type.def_type != .Number
                    and left_operand_type.def_type != .String
                    and left_operand_type.def_type != .Placeholder) {
                    try self.reportError("Expected `num` or `str`.");
                }

                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad type.");
                }

                try self.emitOpCode(.OP_ADD);

                return left_operand_type;
            },
            .Minus => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_SUBTRACT);

                return left_operand_type;
            },
            .Star => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_MULTIPLY);

                return left_operand_type;
            },
            .Slash => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_DIVIDE);

                return left_operand_type;
            },
            .Percent => {
                if (left_operand_type.def_type == .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                } else if (left_operand_type.def_type != .Number) {
                    try self.reportError("Expected `num`.");
                }

                if (right_operand_type.def_type == .Placeholder and left_operand_type.def_type != .Placeholder) {
                    right_operand_type.resolved_type.?.Placeholder.resolved_def_type = left_operand_type.def_type;
                } else if (left_operand_type.def_type == .Placeholder and right_operand_type.def_type != .Placeholder) {
                    left_operand_type.resolved_type.?.Placeholder.resolved_def_type = right_operand_type.def_type;
                }

                if (right_operand_type.def_type == .Placeholder and !right_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                if (left_operand_type.def_type == .Placeholder and !left_operand_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Can't be `num`.");
                }

                try self.emitOpCode(.OP_MOD);

                return left_operand_type;
            },
            else => unreachable,
        }
    }

    fn subscript(self: *Self, can_assign: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        if (callee_type.def_type != .List
            and callee_type.def_type != .Map
            and callee_type.def_type != .Placeholder
            and (callee_type.def_type != .Placeholder or !callee_type.resolved_type.?.Placeholder.isSubscriptable())) {
            try self.reportError("Not subscriptable.");
        }

        var item_type: ?*ObjTypeDef = null;

        if (callee_type.def_type == .List
            or (callee_type.def_type == .Placeholder and callee_type.resolved_type.?.Placeholder.couldBeList())) {

            if (callee_type.def_type == .List) {
                item_type = callee_type.resolved_type.?.List.item_type;
            } else {
                assert(callee_type.def_type == .Placeholder);

                // item_type is a placeholder
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                if (callee_type.resolved_type.?.Placeholder.resolved_type) |resolved| {
                    if (resolved.def_type == .List) {
                        placeholder_resolved_type.Placeholder.resolved_def_type = resolved.resolved_type.?.List.item_type.def_type;
                        placeholder_resolved_type.Placeholder.resolved_type = resolved.resolved_type.?.List.item_type;
                    } else {
                        try self.reportError("Not a list.");
                    }
                }

                item_type = try self.getTypeDef(.{
                    .optional = false,
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                });
            }

            var index_type: *ObjTypeDef = try self.expression(false);

            if (index_type.def_type != .Number) {
                if (index_type.def_type == .Placeholder
                    and (index_type.resolved_type.?.Placeholder.resolved_def_type == null
                        or index_type.resolved_type.?.Placeholder.resolved_def_type.? == .Number)
                    and (index_type.resolved_type.?.Placeholder.resolved_type == null
                        or index_type.resolved_type.?.Placeholder.resolved_type.?.def_type == .Number)) {
                    index_type.resolved_type.?.Placeholder.resolved_def_type = .Number;
                    index_type.resolved_type.?.Placeholder.resolved_type = try self.getTypeDef(.{
                        .optional = false,
                        .def_type = .Number,
                    });
                } else {
                    try self.reportError("Expected `num` index.");
                }
            }
        } else if (callee_type.def_type == .Map
            or (callee_type.def_type == .Placeholder and callee_type.resolved_type.?.Placeholder.couldBeMap())) {
            
            if (callee_type.def_type == .Map) {
                item_type = callee_type.resolved_type.?.Map.value_type;
            } else {
                assert(callee_type.def_type == .Placeholder);

                // item_type is a placeholder
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                if (callee_type.resolved_type.?.Placeholder.resolved_type) |resolved| {
                    placeholder_resolved_type.Placeholder.resolved_def_type = resolved.resolved_type.?.Map.value_type.def_type;
                    placeholder_resolved_type.Placeholder.resolved_type = resolved.resolved_type.?.Map.value_type;
                }

                item_type = try self.getTypeDef(.{
                    .optional = false,
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                });
            }

            var key_type: *ObjTypeDef = try self.expression(false);
    

            if (callee_type.def_type == .Map) {
                if (!callee_type.resolved_type.?.Map.key_type.eql(key_type)) {
                    try self.reportTypeCheck(callee_type.resolved_type.?.Map.key_type, key_type, "Wrong map key type");
                }
            } else {
                assert(callee_type.def_type == .Placeholder);

                // key_type is a placeholder
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                if (callee_type.resolved_type.?.Placeholder.resolved_type) |resolved| {
                    if (resolved.def_type == .Map) {
                        placeholder_resolved_type.Placeholder.resolved_def_type = resolved.resolved_type.?.Map.key_type.def_type;
                        placeholder_resolved_type.Placeholder.resolved_type = resolved.resolved_type.?.Map.key_type;
                    } else {
                        try self.reportError("Not a map");
                    }
                }

                var placeholder = ObjTypeDef{
                    .optional = false,
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                };

                if (!ObjTypeDef.eql(&placeholder, key_type)) {
                    try self.reportError("Wrong map key type.");
                }
            }
        }

        try self.consume(.RightBracket, "Expected `]`.");

        if (can_assign and try self.match(.Equal)) {
            var parsed_type: *ObjTypeDef = try self.expression(false);
            
            if (item_type != null and !item_type.?.eql(parsed_type)) {
                try self.reportTypeCheck(item_type.?, parsed_type, "Bad list assignment.");
            }

            try self.emitOpCode(.OP_SET_SUBSCRIPT);
        } else {
            try self.emitOpCode(.OP_GET_SUBSCRIPT);
        }

        return item_type orelse callee_type;
    }

    fn call(self: *Self, _: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        var arg_count: u8 = 0;
        if (callee_type.def_type == .Function) {
            arg_count = try self.argumentList(callee_type.resolved_type.?.Function.parameters);

            try self.emitCodeArg(.OP_CALL, arg_count);

            return callee_type.resolved_type.?.Function.return_type;
        } else if (callee_type.def_type == .Object) {
            if (callee_type.resolved_type.?.Object.methods.get("init")) |initializer| {
                arg_count = try self.argumentList(initializer.resolved_type.?.Function.parameters);
            } else {
                // try self.reportError("No initializer: this is a bug in buzz compiler which should have provided one.");
                try self.consume(.RightParen, "Expected `)` to close argument list.");

                arg_count = 0;
            }

            try self.emitCodeArg(.OP_CALL, arg_count);

            var instance_type: ObjTypeDef.TypeUnion = .{
                .ObjectInstance = callee_type
            };

            return try self.getTypeDef(ObjTypeDef {
                .optional = false,
                .def_type = .ObjectInstance,
                .resolved_type = instance_type,
            });
        } else if (callee_type.def_type == .Native) {
            arg_count = try self.argumentList(callee_type.resolved_type.?.Native.parameters);

            try self.emitCodeArg(.OP_CALL, arg_count);

            return callee_type.resolved_type.?.Native.return_type;
        } else if (callee_type.def_type == .Placeholder) {
            if (self.current.?.scope_depth == 0) {
                try self.reportError("Unknown expression type.");
                return callee_type;
            }

            callee_type.resolved_type.?.Placeholder.callable = callee_type.resolved_type.?.Placeholder.callable orelse true;
            if (!callee_type.resolved_type.?.Placeholder.isCoherent()) {
                try self.reportErrorAt(callee_type.resolved_type.?.Placeholder.where, "Can't be called");

                return callee_type;
            }

            // Call it
            arg_count = try self.placeholderArgumentList(callee_type);

            try self.emitCodeArg(.OP_CALL, arg_count);
            
            // We know nothing of the return value
            var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
            };

            var placeholder = try self.getTypeDef(.{
                .optional = false,
                .def_type = .Placeholder,
                .resolved_type = placeholder_resolved_type
            });

            try PlaceholderDef.link(callee_type, placeholder, .Call);

            return placeholder;
        }

        try self.reportError("Can't be called");

        return callee_type;
    }

    fn unwrap(self: *Self, _: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        if (!callee_type.optional
            or (callee_type.def_type == .Placeholder
                and callee_type.resolved_type.?.Placeholder.resolved_type != null
                and !callee_type.resolved_type.?.Placeholder.resolved_type.?.optional)) {
            try self.reportError("Not an optional.");
        }

        try self.emitCodeArg(.OP_COPY, 1);
        try self.emitOpCode(.OP_NULL);
        try self.emitOpCode(.OP_EQUAL);
        try self.emitOpCode(.OP_NOT);
        
        const jump: usize = try self.emitJump(.OP_JUMP_IF_FALSE);
        if (self.opt_jumps == null) {
            self.opt_jumps = std.ArrayList(OptJump).init(self.allocator);
        }
        try self.opt_jumps.?.append(.{
            .jump = jump,
            .precedence = getRule(self.parser.current_token.?.token_type).precedence
        });
        
        try self.emitOpCode(.OP_POP); // Pop test result

        var unwrapped: ObjTypeDef = callee_type.*;
        unwrapped.optional = false;

        return self.getTypeDef(unwrapped);
    }

    fn forceUnwrap(self: *Self, _: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        if (!callee_type.optional
            or (callee_type.def_type == .Placeholder
                and callee_type.resolved_type.?.Placeholder.resolved_type != null
                and !callee_type.resolved_type.?.Placeholder.resolved_type.?.optional)) {
            try self.reportError("Not an optional.");
        }

        try self.emitOpCode(.OP_UNWRAP);

        var unwrapped: ObjTypeDef = callee_type.*;
        unwrapped.optional = false;

        return self.getTypeDef(unwrapped);
    }

    fn dot(self: *Self, can_assign: bool, callee_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        // TODO: eventually allow dot on Class/Enum/Object themselves for static stuff
        if (callee_type.def_type != .ObjectInstance
            and callee_type.def_type != .Enum
            and callee_type.def_type != .EnumInstance
            and callee_type.def_type != .List
            and callee_type.def_type != .Placeholder) {
            try self.reportError("Doesn't have field access.");
        }

        try self.consume(.Identifier, "Expected property name after `.`");
        var member_name: []const u8 = self.parser.previous_token.?.lexeme;
        var name: u24 = try self.identifierConstant(self.parser.previous_token.?.lexeme);

        // Check that name is a property
        switch (callee_type.def_type) {
            .ObjectInstance => {
                var obj_def: ObjObject.ObjectDef = callee_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                var property_type: ?*ObjTypeDef = obj_def.methods.get(member_name);
                var is_method: bool = property_type != null;
                
                property_type = property_type
                    orelse obj_def.fields.get(member_name)
                    orelse obj_def.placeholders.get(member_name);

                // Else create placeholder
                // TODO: don't create placeholder if we're not in the process of parsing the object def
                if (property_type == null) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?),
                    };

                    var placeholder: *ObjTypeDef = try self.getTypeDef(.{
                        .optional = try self.match(.Question),
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    });

                    try callee_type.resolved_type.?.ObjectInstance.resolved_type.?.Object.placeholders.put(member_name, placeholder);

                    property_type = placeholder;
                }
                
                // If its a field or placeholder, we can assign to it
                if (can_assign and try self.match(.Equal)) {
                    if (property_type.?.def_type == .Placeholder) {
                        property_type.?.resolved_type.?.Placeholder.assignable = true;
                        if (!property_type.?.resolved_type.?.Placeholder.isCoherent()) {
                            try self.reportErrorAt(property_type.?.resolved_type.?.Placeholder.where, "Can't be assigned to.");
                            return property_type.?;
                        }
                    }

                    if (is_method and property_type.?.def_type != .Placeholder) {
                        try self.reportError("Can't be assigned to.");
                    }

                    var parsed_type: *ObjTypeDef = try self.expression(false);

                    if (!parsed_type.eql(property_type.?)) {
                        try self.reportTypeCheck(property_type.?, parsed_type, "Property value");
                    }

                    try self.emitCodeArg(.OP_SET_PROPERTY, name);

                    return parsed_type;             
                }


                // If it's a method or placeholder we can call it
                if (try self.match(.LeftParen)) {
                    if (property_type.?.def_type == .Placeholder) {
                        property_type.?.resolved_type.?.Placeholder.callable = true;
                        if (!property_type.?.resolved_type.?.Placeholder.isCoherent()) {
                            try self.reportErrorAt(property_type.?.resolved_type.?.Placeholder.where, "Can't be called.");
                            return property_type.?;
                        }
                    }

                    if (!is_method and property_type.?.def_type != .Placeholder) {
                        try self.reportError("Can't be called.");
                    }

                    var arg_count: u8 = try self.argumentList(property_type.?.resolved_type.?.Function.parameters);

                    try self.emitCodeArg(.OP_INVOKE, name);
                    try self.emit(arg_count);

                    return property_type.?.resolved_type.?.Function.return_type;
                }

                // Else just get it
                try self.emitCodeArg(.OP_GET_PROPERTY, name);

                return property_type.?;
            },
            .Enum => {
                var enum_def: ObjEnum.EnumDef = callee_type.resolved_type.?.Enum;

                for (enum_def.cases.items) |case, index| {
                    if (mem.eql(u8, case, member_name)) {
                        try self.emitCodeArg(.OP_GET_ENUM_CASE, @intCast(u24, index));

                        var enum_instance_resolved_type: ObjTypeDef.TypeUnion = .{
                            .EnumInstance = callee_type,
                        };

                        var enum_instance: *ObjTypeDef = try self.getTypeDef(.{
                            .optional = try self.match(.Question),
                            .def_type = .EnumInstance,
                            .resolved_type = enum_instance_resolved_type,
                        });

                        return enum_instance;
                    }
                }

                try self.reportError("Enum case doesn't exists.");

                return callee_type;
            },
            .EnumInstance => {
                // Only available field is `.value` to get associated value
                if (!mem.eql(u8, member_name, "value")) {
                    try self.reportError("Enum provides only field `value`.");
                }

                try self.emitOpCode(.OP_GET_ENUM_CASE_VALUE);

                return callee_type.resolved_type.?.EnumInstance.resolved_type.?.Enum.enum_type;
            },
            .List => {
                if (try ObjList.ListDef.member(callee_type, self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        try self.emitOpCode(.OP_COPY); // List is first argument
                        var arg_count: u8 = try self.argumentList(member.resolved_type.?.Native.parameters);
                        try self.emitCodeArg(.OP_INVOKE, name);
                        try self.emit(arg_count + 1);

                        return member.resolved_type.?.Native.return_type;
                    }

                    // Else just get it
                    try self.emitCodeArg(.OP_GET_PROPERTY, name);

                    return member;
                }

                try self.reportError("List property doesn't exist.");
                return callee_type;
            },
            .Placeholder => {
                callee_type.resolved_type.?.Placeholder.field_accessible = callee_type.resolved_type.?.Placeholder.field_accessible orelse true;
                if (!callee_type.resolved_type.?.Placeholder.isCoherent()) {
                    // TODO: how to have a revelant message here?
                    try self.reportErrorAt(callee_type.resolved_type.?.Placeholder.where, "[Bad assumption] doesn't support field access (and something else)");

                    return callee_type;
                }

                // We know nothing of field
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?)
                };

                placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, member_name);

                var placeholder = try self.getTypeDef(.{
                    .optional = false,
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type
                });

                try PlaceholderDef.link(callee_type, placeholder, .FieldAccess);
                
                if (can_assign and try self.match(.Equal)) {
                    var parsed_type: *ObjTypeDef = try self.expression(false);

                    try self.emitCodeArg(.OP_SET_PROPERTY, name);

                    placeholder.resolved_type.?.Placeholder.resolved_def_type = parsed_type.def_type;
                    placeholder.resolved_type.?.Placeholder.resolved_type = parsed_type;

                    if (!placeholder.resolved_type.?.Placeholder.isCoherent()) {
                        try self.reportErrorAt(placeholder.resolved_type.?.Placeholder.where, "[Bad assumption] can't be set or bad type.");
                    }
                } else if (try self.match(.LeftParen)) {
                    placeholder.resolved_type.?.Placeholder.callable = true;

                    if (!placeholder.resolved_type.?.Placeholder.isCoherent()) {
                        try self.reportErrorAt(placeholder.resolved_type.?.Placeholder.where, "[Bad assumption] can't be called.");
                    }

                    var arg_count: u8 = try self.placeholderArgumentList(placeholder);

                    try self.emitCodeArg(.OP_INVOKE, name);
                    try self.emit(arg_count);
                } else {
                    try self.emitCodeArg(.OP_GET_PROPERTY, name);
                }

                return placeholder;
            },
            else => unreachable,
        }

        unreachable;
    }

    fn list(self: *Self, _: bool) anyerror!*ObjTypeDef {
        var item_type: ?*ObjTypeDef = null;

        // A lone list expression is prefixed by `<type>`
        if (self.parser.previous_token.?.token_type == .Less) {
            item_type = try self.parseTypeDef();

            if (try self.match(.Comma)) {
                return try self.mapFinalise(item_type.?);
            }

            try self.consume(.Greater, "Expected `>` after list type.");
            try self.consume(.LeftBracket, "Expected `[` after list type.");
        }

        const list_offset: usize = try self.emitList();

        while (!(try self.match(.RightBracket)) and !(try self.match(.Eof))) {
            var actual_item_type: *ObjTypeDef = try self.expression(false);

            try self.emitOpCode(.OP_LIST_APPEND);

            if (item_type != null and !item_type.?.eql(actual_item_type)) {
                try self.reportError("List can only hold one type.");
            }

            if (item_type != null and item_type.?.def_type == .Placeholder
                and actual_item_type.def_type != .Placeholder) {
                item_type.?.resolved_type.?.Placeholder.resolved_def_type = actual_item_type.def_type;
                item_type.?.resolved_type.?.Placeholder.resolved_type = actual_item_type;
                if (item_type.?.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad list type.");
                }
            }

            if (actual_item_type.def_type == .Placeholder
                and item_type != null
                and item_type.?.def_type != .Placeholder) {
                actual_item_type.resolved_type.?.Placeholder.resolved_def_type = item_type.?.def_type;
                actual_item_type.resolved_type.?.Placeholder.resolved_type = item_type.?;
                if (actual_item_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad list item type.");
                }
            }

            if (item_type == null) {
                item_type = actual_item_type;
            }
            
            if (!self.check(.RightBracket)) {
                try self.consume(.Comma, "Expected `,` after list item.");
            }
        }

        // Should be fine if placeholder because when resolved, it's always the same pointer
        const constant: u24 = try self.makeConstant(Value { .Obj = item_type.?.toObj() });
        try self.patchList(list_offset, constant);

        var list_def = ObjList.ListDef.init(self.allocator, item_type.?);

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .List = list_def
        };

        var list_type: *ObjTypeDef = try self.getTypeDef(.{
            .optional = try self.match(.Question),
            .def_type = .List,
            .resolved_type = resolved_type
        });

        return list_type;
    }

    fn map(self: *Self, _: bool) anyerror!*ObjTypeDef {
        return try self.mapFinalise(null);
    }

    fn mapFinalise(self: *Self, parsed_key_type: ?*ObjTypeDef) anyerror!*ObjTypeDef {
        var value_type: ?*ObjTypeDef = null;
        var key_type: ?*ObjTypeDef = parsed_key_type;

        // A lone map expression is prefixed by `<type, type>`
        // When key_type != null, we come from list() which just parsed `<type,`
        if (key_type != null) {
            value_type = try self.parseTypeDef();

            try self.consume(.Greater, "Expected `>` after map type.");
            try self.consume(.LeftBrace, "Expected `{` before map entries.");
        }

        const map_offset: usize = try self.emitMap();

        while (!(try self.match(.RightBrace)) and !(try self.match(.Eof))) {
            var actual_key_type: *ObjTypeDef = try self.expression(false);
            try self.consume(.Colon, "Expected `:` after key.");
            var actual_value_type: *ObjTypeDef = try self.expression(false);

            try self.emitOpCode(.OP_SET_MAP);

            // Key type checking
            if (key_type != null and !key_type.?.eql(actual_key_type)) {
                try self.reportError("Map can only hold one key type.");
            }

            if (key_type != null
                and key_type.?.def_type == .Placeholder
                and actual_key_type.def_type != .Placeholder) {
                key_type.?.resolved_type.?.Placeholder.resolved_def_type = actual_key_type.def_type;
                key_type.?.resolved_type.?.Placeholder.resolved_type = actual_key_type;
                if (key_type.?.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad map type.");
                }
            }

            if (actual_key_type.def_type == .Placeholder
                and key_type != null
                and key_type.?.def_type != .Placeholder) {
                actual_key_type.resolved_type.?.Placeholder.resolved_def_type = key_type.?.def_type;
                actual_key_type.resolved_type.?.Placeholder.resolved_type = key_type;
                if (actual_key_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad map item type.");
                }
            }

            if (key_type == null) {
                key_type = actual_key_type;
            }

            // Value type checking
            if (value_type != null and !value_type.?.eql(actual_value_type)) {
                try self.reportError("Map can only hold one value type.");
            }

            if (value_type != null and value_type.?.def_type == .Placeholder
                and actual_value_type.def_type != .Placeholder) {
                value_type.?.resolved_type.?.Placeholder.resolved_def_type = actual_value_type.def_type;
                value_type.?.resolved_type.?.Placeholder.resolved_type = actual_value_type;
                if (value_type.?.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad map type.");
                }
            }

            if (actual_value_type.def_type == .Placeholder
                and value_type != null
                and value_type.?.def_type != .Placeholder) {
                actual_value_type.resolved_type.?.Placeholder.resolved_def_type = value_type.?.def_type;
                actual_value_type.resolved_type.?.Placeholder.resolved_type = value_type.?;
                if (actual_value_type.resolved_type.?.Placeholder.isCoherent()) {
                    try self.reportError("Bad map item type.");
                }
            }

            if (value_type == null) {
                value_type = actual_value_type;
            }
            
            if (!self.check(.RightBrace)) {
                try self.consume(.Comma, "Expected `,` after map entry.");
            }
        }

        // Should be fine if placeholder because when resolved, it's always the same pointer
        const key_constant: u24 = try self.makeConstant(Value { .Obj = key_type.?.toObj() });
        const value_constant: u24 = try self.makeConstant(Value { .Obj = value_type.?.toObj() });
        try self.patchMap(map_offset, key_constant, value_constant);

        var map_def = ObjMap.MapDef{ .key_type = key_type.?, .value_type = value_type.? };

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .Map = map_def
        };

        var map_type: *ObjTypeDef = try self.getTypeDef(.{
            .optional = try self.match(.Question),
            .def_type = .Map,
            .resolved_type = resolved_type
        });

        return map_type;
    }

    fn grouping(self: *Self, _: bool) anyerror!*ObjTypeDef {
        var parsed_type: *ObjTypeDef = try self.expression(false);
        try self.consume(.RightParen, "Expected ')' after expression.");

        return parsed_type;
    }

    fn literal(self: *Self, _: bool) anyerror!*ObjTypeDef {
        switch (self.parser.previous_token.?.token_type) {
            .False => {
                try self.emitOpCode(.OP_FALSE);

                return try self.getTypeDef(.{
                    .def_type = .Bool,
                    .optional = false,
                });
            },
            .True => {
                try self.emitOpCode(.OP_TRUE);

                return try self.getTypeDef(.{
                    .def_type = .Bool,
                    .optional = false,
                });
            },
            .Null => {
                try self.emitOpCode(.OP_NULL);

                return try self.getTypeDef(.{
                    .def_type = .Void,
                    .optional = false,
                });
            },
            else => unreachable,
        }
    }

    fn number(self: *Self, _: bool) anyerror!*ObjTypeDef {
        var value: f64 = self.parser.previous_token.?.literal_number.?;

        try self.emitConstant(Value{ .Number = value });

        return try self.getTypeDef(.{
            .def_type = .Number,
            .optional = false,
        });
    }

    fn emitConstant(self: *Self, value: Value) !void {
        try self.emitCodeArg(.OP_CONSTANT, try self.makeConstant(value));
    }

    // LOCALS

    fn addLocal(self: *Self, name: Token, local_type: *ObjTypeDef) !usize {
        if (self.current.?.local_count == 255) {
            try self.reportError("Too many local variables in scope.");
            return 0;
        }

        self.current.?.locals[self.current.?.local_count] = Local{
            .name = try copyStringRaw(self.strings, self.allocator, name.lexeme),
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
        };

        self.current.?.local_count += 1;

        return self.current.?.local_count - 1;
    }

    fn addGlobal(self: *Self, name: Token, global_type: *ObjTypeDef) !usize {
        // Search for an existing placeholder global with the same name
        for (self.globals.items) |global, index| {
            if (global.type_def.def_type == .Placeholder
                and global.type_def.resolved_type.?.Placeholder.name != null
                and mem.eql(u8, name.lexeme, global.name.string)) {
                try self.resolvePlaceholder(global.type_def, global_type);

                return index;
            }
        }

        if (self.globals.items.len == 255) {
            try self.reportError("Too many global variables.");
            return 0;
        }

        try self.globals.append(Global{
            .name = try copyStringRaw(self.strings, self.allocator, name.lexeme),
            .type_def = global_type,
        });

        return self.globals.items.len - 1;
    }

    fn resolveLocal(self: *Self, compiler: *ChunkCompiler, name: Token) !?usize {
        if (compiler.local_count == 0) {
            return null;
        }

        var i: usize = compiler.local_count - 1;
        while (i >= 0) : (i -= 1) {
            var local: *Local = &compiler.locals[i];
            if (mem.eql(u8, name.lexeme, local.name.string)) {
                if (local.depth == -1) {
                    try self.reportError("Can't read local variable in its own initializer.");
                }

                return i;
            }

            if (i == 0) break;
        }

        return null;
    }

    fn resolveGlobal(self: *Self, name: Token) !?usize {
        if (self.globals.items.len == 0) {
            return null;
        }

        var i: usize = self.globals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            var global: *Global = &self.globals.items[i];
            if (mem.eql(u8, name.lexeme, global.name.string)) {
                if (!global.initialized) {
                    try self.reportError("Can't read global variable in its own initializer.");
                }

                return i;
            }

            if (i == 0) break;
        }

        return null;
    }

    fn addUpvalue(self: *Self, compiler: *ChunkCompiler, index: usize, is_local: bool) !usize {
        var upvalue_count: u8 = compiler.function.upvalue_count;

        var i: usize = 0;
        while (i < upvalue_count) : (i += 1) {
            var upvalue: *UpValue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == 255) {
            try self.reportError("Too many closure variables in function.");
            return 0;
        }

        compiler.upvalues[upvalue_count].is_local = is_local;
        compiler.upvalues[upvalue_count].index = @intCast(u8, index);
        compiler.function.upvalue_count += 1;

        return compiler.function.upvalue_count - 1;
    }

    fn resolveUpvalue(self: *Self, compiler: *ChunkCompiler, name: Token) anyerror!?usize {
        if (compiler.enclosing == null) {
            return null;
        }

        var local: ?usize = try self.resolveLocal(compiler.enclosing.?, name);
        if (local) |resolved| {
            compiler.enclosing.?.locals[resolved].is_captured = true;
            return try self.addUpvalue(compiler, resolved, true);
        }

        var upvalue: ?usize = try self.resolveUpvalue(compiler.enclosing.?, name);
        if (upvalue) |resolved| {
            return try self.addUpvalue(compiler, resolved, false);
        }

        return null;
    }

    fn identifiersEqual(a: Token, b: Token) bool {
        if (a.lexeme.len != b.lexeme.len) {
            return false;
        }

        return mem.eql(u8, a.lexeme, b.lexeme);
    }

    // VARIABLES

    fn parseVariable(self: *Self, variable_type: *ObjTypeDef, error_message: []const u8) !usize {
        try self.consume(.Identifier, error_message);

        return try self.declareVariable(variable_type, null);
    }

    inline fn markInitialized(self: *Self) void {
        if (self.current.?.scope_depth == 0) {
            self.globals.items[self.globals.items.len - 1].initialized = true;
        } else {
            self.current.?.locals[self.current.?.local_count - 1].depth = @intCast(i32, self.current.?.scope_depth);
        }
    }

    fn declareVariable(self: *Self, variable_type: *ObjTypeDef, name_token: ?Token) !usize {
        var name: Token = name_token orelse self.parser.previous_token.?;

        if (self.current.?.scope_depth > 0) {
            // Check a local with the same name doesn't exists
            var i: usize = self.current.?.locals.len - 1;
            while (i >= 0) : (i -= 1) {
                var local: *Local = &self.current.?.locals[i];

                if (local.depth != -1 and local.depth < self.current.?.scope_depth) {
                    break;
                }

                if (mem.eql(u8, name.lexeme, local.name.string)) {
                    try self.reportError("A variable with the same name already exists in this scope.");
                }

                if (i == 0) break;
            }

            return try self.addLocal(name, variable_type);
        } else {
            // Check a global with the same name doesn't exists
            for (self.globals.items) |global, index| {
                if (mem.eql(u8, name.lexeme, global.name.string) and !global.hidden and !global.hidden) {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder
                        and global.type_def.resolved_type.?.Placeholder.name != null
                        and mem.eql(u8, name.lexeme, global.type_def.resolved_type.?.Placeholder.name.?.string)) {

                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @enumToInt(variable_type.def_type) < @enumToInt(ObjTypeDef.Type.ObjectInstance)) {
                            try self.resolvePlaceholder(global.type_def, variable_type);
                        }

                        return index;
                    } else {
                        try self.reportError("A global with the same name already exists.");
                    }
                }
            }

            return try self.addGlobal(name, variable_type);
        }
    }

    fn makeConstant(self: *Self, value: Value) !u24 {
        var constant: u24 = try self.current.?.function.chunk.addConstant(null, value);
        if (constant > Chunk.max_constants) {
            try self.reportError("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn identifierConstant(self: *Self, name: []const u8) !u24 {
        return try self.makeConstant(Value{ .Obj = (try copyStringRaw(self.strings, self.allocator, name)).toObj() });
    }
};