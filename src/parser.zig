const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _obj = @import("./obj.zig");
const _node = @import("./node.zig");
const _token = @import("./token.zig");
const _vm = @import("./vm.zig");
const _value = @import("./value.zig");
const _scanner = @import("./scanner.zig");
const _utils = @import("./utils.zig");
const _chunk = @import("./chunk.zig");
const Config = @import("./config.zig").Config;
const StringParser = @import("./string_parser.zig").StringParser;

const Value = _value.Value;
const ValueType = _value.ValueType;
const valueToHashable = _value.valueToHashable;
const hashableToValue = _value.hashableToValue;
const valueToString = _value.valueToString;
const valueEql = _value.valueEql;
const ObjType = _obj.ObjType;
const Obj = _obj.Obj;
const ObjNative = _obj.ObjNative;
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
const Token = _token.Token;
const TokenType = _token.TokenType;
const copyStringRaw = _obj.copyStringRaw;
const Scanner = _scanner.Scanner;
const toNullTerminated = _utils.toNullTerminated;
const NativeFn = _obj.NativeFn;
const FunctionType = _obj.ObjFunction.FunctionType;
const SlotType = _node.SlotType;
const ParseNodeType = _node.ParseNodeType;
const ParseNode = _node.ParseNode;
const NamedVariableNode = _node.NamedVariableNode;
const NumberNode = _node.NumberNode;
const BooleanNode = _node.BooleanNode;
const StringLiteralNode = _node.StringLiteralNode;
const StringNode = _node.StringNode;
const NullNode = _node.NullNode;
const ListNode = _node.ListNode;
const MapNode = _node.MapNode;
const UnwrapNode = _node.UnwrapNode;
const ForceUnwrapNode = _node.ForceUnwrapNode;
const IsNode = _node.IsNode;
const UnaryNode = _node.UnaryNode;
const BinaryNode = _node.BinaryNode;
const SubscriptNode = _node.SubscriptNode;
const FunctionNode = _node.FunctionNode;
const CallNode = _node.CallNode;
const SuperCallNode = _node.SuperCallNode;
const VarDeclarationNode = _node.VarDeclarationNode;
const ExpressionNode = _node.ExpressionNode;
const EnumNode = _node.EnumNode;
const ThrowNode = _node.ThrowNode;
const BreakNode = _node.BreakNode;
const ContinueNode = _node.ContinueNode;
const IfNode = _node.IfNode;
const ReturnNode = _node.ReturnNode;
const ForNode = _node.ForNode;
const ForEachNode = _node.ForEachNode;
const WhileNode = _node.WhileNode;
const DoUntilNode = _node.DoUntilNode;
const BlockNode = _node.BlockNode;
const SuperNode = _node.SuperNode;
const DotNode = _node.DotNode;
const FunDeclarationNode = _node.FunDeclarationNode;
const ObjectInitNode = _node.ObjectInitNode;
const ObjectDeclarationNode = _node.ObjectDeclarationNode;
const ExportNode = _node.ExportNode;
const ImportNode = _node.ImportNode;
const ParsedArg = _node.ParsedArg;
const OpCode = _chunk.OpCode;
const TypeRegistry = _obj.TypeRegistry;

extern fn dlerror() [*:0]u8;

pub const CompileError = error{
    Unrecoverable,
    Recoverable,
};

pub const ParserState = struct {
    const Self = @This();

    current_token: ?Token = null,
    previous_token: ?Token = null,

    // Most of the time 1 token in advance is enough, but in some special cases we want to be able to look
    // some arbitrary number of token ahead
    ahead: std.ArrayList(Token),

    had_error: bool = false,
    panic_mode: bool = false,

    pub fn init(allocator: Allocator) Self {
        return .{
            .ahead = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.ahead.deinit();
    }
};

pub const Local = struct {
    name: *ObjString,
    type_def: *ObjTypeDef,
    depth: i32,
    is_captured: bool,
    constant: bool,
};

pub const Global = struct {
    prefix: ?[]const u8 = null,
    name: *ObjString, // TODO: do i need to mark those? does it need to be an objstring?
    type_def: *ObjTypeDef,
    initialized: bool = false,
    exported: bool = false,
    export_alias: ?[]const u8 = null,
    hidden: bool = false,
    constant: bool,
    // When resolving a placeholder, the start of the resolution is the global
    // If `constant` is true, we can search for any `.Assignment` link and fail then.
};

pub const UpValue = struct { index: u8, is_local: bool };

pub const Frame = struct {
    enclosing: ?*Frame = null,
    locals: [255]Local,
    local_count: u8 = 0,
    upvalues: [255]UpValue,
    upvalue_count: u8 = 0,
    scope_depth: u32 = 0,
    // If false, `return` was omitted or within a conditionned block (if, loop, etc.)
    // We only count `return` emitted within the scope_depth 0 of the current function or unconditionned else statement
    function_node: *FunctionNode,
    function: ?*ObjFunction = null,
    constants: std.ArrayList(Value),
};

pub const ObjectCompiler = struct {
    name: Token,
    type_def: *ObjTypeDef,
};

pub const Parser = struct {
    const Self = @This();

    pub const DeclarationTerminator = enum {
        Comma,
        OptComma,
        Semicolon,
        Nothing,
    };

    pub const BlockType = enum {
        While,
        Do,
        For,
        ForEach,
        Function,
    };

    pub const Precedence = enum {
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

    const ParseFn = fn (*Parser, bool) anyerror!*ParseNode;
    const InfixParseFn = fn (*Parser, bool, *ParseNode) anyerror!*ParseNode;

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?InfixParseFn,
        precedence: Precedence,
    };

    const rules = [_]ParseRule{
        .{ .prefix = null, .infix = null, .precedence = .None }, // Pipe
        .{ .prefix = list, .infix = subscript, .precedence = .Call }, // LeftBracket
        .{ .prefix = null, .infix = null, .precedence = .None }, // RightBracket
        .{ .prefix = grouping, .infix = call, .precedence = .Call }, // LeftParen
        .{ .prefix = null, .infix = null, .precedence = .None }, // RightParen
        .{ .prefix = map, .infix = objectInit, .precedence = .Primary }, // LeftBrace
        .{ .prefix = null, .infix = null, .precedence = .None }, // RightBrace
        .{ .prefix = null, .infix = dot, .precedence = .Call }, // Dot
        .{ .prefix = null, .infix = null, .precedence = .None }, // Comma
        .{ .prefix = null, .infix = null, .precedence = .None }, // Semicolon
        .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // Greater
        .{ .prefix = list, .infix = binary, .precedence = .Comparison }, // Less
        .{ .prefix = null, .infix = binary, .precedence = .Term }, // Plus
        .{ .prefix = unary, .infix = binary, .precedence = .Term }, // Minus
        .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Star
        .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Slash
        .{ .prefix = null, .infix = binary, .precedence = .Factor }, // Percent
        .{ .prefix = null, .infix = unwrap, .precedence = .Call }, // Question
        .{ .prefix = unary, .infix = forceUnwrap, .precedence = .Call }, // Bang
        .{ .prefix = null, .infix = null, .precedence = .None }, // Colon
        .{ .prefix = null, .infix = null, .precedence = .None }, // Equal
        .{ .prefix = null, .infix = binary, .precedence = .Equality }, // EqualEqual
        .{ .prefix = null, .infix = binary, .precedence = .Equality }, // BangEqual
        .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // GreaterEqual
        .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // LessEqual
        .{ .prefix = null, .infix = binary, .precedence = .NullCoalescing }, // QuestionQuestion
        .{ .prefix = null, .infix = null, .precedence = .None }, // PlusEqual
        .{ .prefix = null, .infix = null, .precedence = .None }, // MinusEqual
        .{ .prefix = null, .infix = null, .precedence = .None }, // StarEqual
        .{ .prefix = null, .infix = null, .precedence = .None }, // SlashEqual
        .{ .prefix = null, .infix = null, .precedence = .None }, // Increment
        .{ .prefix = null, .infix = null, .precedence = .None }, // Decrement
        .{ .prefix = null, .infix = null, .precedence = .None }, // Arrow
        .{ .prefix = literal, .infix = null, .precedence = .None }, // True
        .{ .prefix = literal, .infix = null, .precedence = .None }, // False
        .{ .prefix = literal, .infix = null, .precedence = .None }, // Null
        .{ .prefix = null, .infix = null, .precedence = .None }, // Str
        .{ .prefix = null, .infix = null, .precedence = .None }, // Num
        .{ .prefix = null, .infix = null, .precedence = .None }, // Type
        .{ .prefix = null, .infix = null, .precedence = .None }, // Bool
        .{ .prefix = null, .infix = null, .precedence = .None }, // Function
        .{ .prefix = null, .infix = null, .precedence = .None }, // ShiftRight
        .{ .prefix = null, .infix = null, .precedence = .None }, // ShiftLeft
        .{ .prefix = null, .infix = null, .precedence = .None }, // Xor
        .{ .prefix = null, .infix = or_, .precedence = .Or }, // Or
        .{ .prefix = null, .infix = and_, .precedence = .And }, // And
        .{ .prefix = null, .infix = null, .precedence = .None }, // Return
        .{ .prefix = null, .infix = null, .precedence = .None }, // If
        .{ .prefix = null, .infix = null, .precedence = .None }, // Else
        .{ .prefix = null, .infix = null, .precedence = .None }, // Do
        .{ .prefix = null, .infix = null, .precedence = .None }, // Until
        .{ .prefix = null, .infix = null, .precedence = .None }, // While
        .{ .prefix = null, .infix = null, .precedence = .None }, // For
        .{ .prefix = null, .infix = null, .precedence = .None }, // ForEach
        .{ .prefix = null, .infix = null, .precedence = .None }, // Switch
        .{ .prefix = null, .infix = null, .precedence = .None }, // Break
        .{ .prefix = null, .infix = null, .precedence = .None }, // Continue
        .{ .prefix = null, .infix = null, .precedence = .None }, // Default
        .{ .prefix = null, .infix = null, .precedence = .None }, // In
        .{ .prefix = null, .infix = is, .precedence = .Is }, // Is
        .{ .prefix = number, .infix = null, .precedence = .None }, // Number
        .{ .prefix = string, .infix = null, .precedence = .None }, // String
        .{ .prefix = variable, .infix = null, .precedence = .None }, // Identifier
        .{ .prefix = fun, .infix = null, .precedence = .None }, // Fun
        .{ .prefix = null, .infix = null, .precedence = .None }, // Object
        .{ .prefix = null, .infix = null, .precedence = .None }, // Class
        .{ .prefix = null, .infix = null, .precedence = .None }, // Enum
        .{ .prefix = null, .infix = null, .precedence = .None }, // Throw
        .{ .prefix = null, .infix = null, .precedence = .None }, // Catch
        .{ .prefix = null, .infix = null, .precedence = .None }, // Test
        .{ .prefix = null, .infix = null, .precedence = .None }, // Import
        .{ .prefix = null, .infix = null, .precedence = .None }, // Export
        .{ .prefix = null, .infix = null, .precedence = .None }, // Const
        .{ .prefix = null, .infix = null, .precedence = .None }, // Static
        .{ .prefix = super, .infix = null, .precedence = .None }, // Super
        .{ .prefix = super, .infix = null, .precedence = .None }, // From
        .{ .prefix = super, .infix = null, .precedence = .None }, // As
        .{ .prefix = super, .infix = null, .precedence = .None }, // Extern
        .{ .prefix = null, .infix = null, .precedence = .None }, // Eof
        .{ .prefix = null, .infix = null, .precedence = .None }, // Error
        .{ .prefix = null, .infix = null, .precedence = .None }, // Void
        .{ .prefix = null, .infix = null, .precedence = .None }, // Docblock
    };

    pub const ScriptImport = struct {
        function: *ParseNode,
        globals: std.ArrayList(Global),
    };

    allocator: Allocator,
    scanner: ?Scanner = null,
    parser: ParserState,
    script_name: []const u8 = undefined,
    type_registry: *TypeRegistry,
    strings: *std.StringHashMap(*ObjString),
    // If true the script is being imported
    imported: bool = false,
    // Cached imported functions
    imports: *std.StringHashMap(ScriptImport),
    test_count: u64 = 0,
    current: ?*Frame = null,
    current_object: ?ObjectCompiler = null,
    globals: std.ArrayList(Global),

    // Jump to patch at end of current expression with a optional unwrapping in the middle of it
    opt_jumps: ?std.ArrayList(Precedence) = null,

    pub fn init(allocator: Allocator, strings: *std.StringHashMap(*ObjString), imports: *std.StringHashMap(ScriptImport), type_registry: *TypeRegistry, imported: bool) Self {
        return .{
            .allocator = allocator,
            .parser = ParserState.init(allocator),
            .strings = strings,
            .imports = imports,
            .imported = imported,
            .type_registry = type_registry,
            .globals = std.ArrayList(Global).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.parser.deinit();
    }

    pub fn parse(self: *Self, source: []const u8, file_name: []const u8) !?*ParseNode {
        if (self.scanner != null) {
            self.scanner = null;
        }

        self.scanner = Scanner.init(self.allocator, source);

        const function_type: FunctionType = if (self.imported) .Script else .ScriptEntryPoint;
        var function_node = try self.allocator.create(FunctionNode);
        function_node.* = try FunctionNode.init(
            self,
            function_type,
            file_name,
        );

        self.script_name = function_node.node.type_def.?.resolved_type.?.Function.name.string;

        try self.beginFrame(function_type, function_node, null);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        try self.advance();

        while (!(try self.match(.Eof))) {
            if (self.declarations() catch return null) |decl| {
                try function_node.body.?.statements.append(decl);
            }
        }

        // If top level, search `main` or `test` function(s) and call them
        // Then put any exported globals on the stack
        if (function_type == .ScriptEntryPoint) {
            for (self.globals.items) |global, index| {
                if (mem.eql(u8, global.name.string, "main") and !global.hidden and global.prefix == null) {
                    function_node.main_slot = index;
                    break;
                }
            }
        }

        var test_slots = std.ArrayList(usize).init(self.allocator);
        // Create an entry point wich runs all `test`
        for (self.globals.items) |global, index| {
            if (global.name.string.len > 5 and mem.eql(u8, global.name.string[0..5], "$test") and !global.hidden and global.prefix == null) {
                try test_slots.append(index);
            }
        }

        function_node.test_slots = test_slots.items;

        // If we're being imported, put all globals on the stack
        if (self.imported) {
            function_node.exported_count = self.globals.items.len;
        }

        // Check there's no more root placeholders
        if (Config.debug_placeholders) {
            for (self.globals.items) |global, index| {
                if (global.type_def.def_type == .Placeholder) {
                    std.debug.print(
                        "Placeholder remaining in globals at {}: @{} {s}\n",
                        .{
                            index,
                            @ptrToInt(global.type_def),
                            if (global.type_def.resolved_type.?.Placeholder.name) |name| name.string else "Unknown",
                        },
                    );

                    assert(false);
                }
            }
        }

        return if (self.parser.had_error) null else &self.endFrame().node;
    }

    fn beginFrame(self: *Self, function_type: FunctionType, function_node: *FunctionNode, this: ?*ObjTypeDef) !void {
        var enclosing = self.current;
        self.current = try self.allocator.create(Frame);
        self.current.?.* = Frame{
            .locals = [_]Local{undefined} ** 255,
            .upvalues = [_]UpValue{undefined} ** 255,
            .enclosing = enclosing,
            .function_node = function_node,
            .constants = std.ArrayList(Value).init(self.allocator),
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
                assert(this != null);

                local.type_def = this.?;
            },
            .EntryPoint, .ScriptEntryPoint => {
                // `args` is [str]
                var list_def: ObjList.ListDef = ObjList.ListDef.init(
                    self.allocator,
                    try self.type_registry.getTypeDef(.{ .def_type = .String }),
                );

                var list_union: ObjTypeDef.TypeUnion = .{ .List = list_def };

                local.type_def = try self.type_registry.getTypeDef(ObjTypeDef{ .def_type = .List, .resolved_type = list_union });
            },
            else => {
                // TODO: do we actually need to reserve that space since we statically know if we need it?
                // nothing
                local.type_def = try self.type_registry.getTypeDef(ObjTypeDef{
                    .def_type = .Void,
                });
            },
        }

        const name: []const u8 = switch (function_type) {
            .Method => "this",
            .EntryPoint => "$args",
            .ScriptEntryPoint => "args",
            else => "_",
        };

        local.name = try copyStringRaw(self.strings, self.allocator, name, false);
    }

    fn endFrame(self: *Self) *FunctionNode {
        var current_node = self.current.?.function_node;
        self.current = self.current.?.enclosing;

        return current_node;
    }

    fn beginScope(self: *Self) void {
        self.current.?.scope_depth += 1;
    }

    fn endScope(self: *Self) !std.ArrayList(OpCode) {
        var current = self.current.?;
        var closing = std.ArrayList(OpCode).init(self.allocator);
        current.scope_depth -= 1;

        while (current.local_count > 0 and current.locals[current.local_count - 1].depth > current.scope_depth) {
            if (current.locals[current.local_count - 1].is_captured) {
                try closing.append(.OP_CLOSE_UPVALUE);
            } else {
                try closing.append(.OP_POP);
            }

            current.local_count -= 1;
        }

        return closing;
    }

    pub fn advance(self: *Self) !void {
        self.parser.previous_token = self.parser.current_token;

        while (true) {
            self.parser.current_token = if (self.parser.ahead.items.len > 0)
                self.parser.ahead.swapRemove(0)
            else
                try self.scanner.?.scanToken();
            if (self.parser.current_token.?.token_type != .Error) {
                break;
            }

            try self.reportErrorAtCurrent(self.parser.current_token.?.literal_string orelse "Unknown error.");
        }
    }

    pub fn consume(self: *Self, token_type: TokenType, message: []const u8) !void {
        if (self.parser.current_token.?.token_type == token_type) {
            try self.advance();
            return;
        }

        try self.reportErrorAtCurrent(message);
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.parser.current_token.?.token_type == token_type;
    }

    fn checkAhead(self: *Self, token_type: TokenType, n: u32) !bool {
        while (n + 1 > self.parser.ahead.items.len) {
            while (true) {
                const token = try self.scanner.?.scanToken();
                try self.parser.ahead.append(token);
                if (token.token_type != .Error) {
                    break;
                }

                try self.reportErrorAtCurrent(token.literal_string orelse "Unknown error.");
            }
        }

        return self.parser.ahead.items[n].token_type == token_type;
    }

    fn match(self: *Self, token_type: TokenType) !bool {
        if (!self.check(token_type)) {
            return false;
        }

        try self.advance();

        return true;
    }

    fn report(self: *Self, token: Token, message: []const u8) !void {
        const lines: std.ArrayList([]const u8) = try self.scanner.?.getLines(self.allocator, if (token.line > 0) token.line - 1 else 0, 3);
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.allocator);
        defer report_line.deinit();
        var writer = report_line.writer();

        try writer.print("\n", .{});
        var l: usize = if (token.line > 0) token.line - 1 else 0;
        for (lines.items) |line| {
            if (l != token.line) {
                try writer.print("\u{001b}[2m", .{});
            }

            var prefix_len: usize = report_line.items.len;
            try writer.print(" {: >5} |", .{l + 1});
            prefix_len = report_line.items.len - prefix_len;
            try writer.print(" {s}\n", .{line});

            if (l == token.line) {
                try writer.writeByteNTimes(' ', token.column - 1 + prefix_len);
                try writer.print("^\n", .{});
            }

            l += 1;
        }
        std.debug.print("{s}{}:{}: \u{001b}[31mError: {s}\n", .{ report_line.items, token.line + 1, token.column + 1, message });

        if (Config.debug_stop_on_report) {
            unreachable;
        }
    }

    fn reportErrorAt(self: *Self, token: Token, message: []const u8) !void {
        if (self.parser.panic_mode) {
            return;
        }

        self.parser.panic_mode = true;
        self.parser.had_error = true;

        try self.report(token, message);
    }

    pub fn reportErrorFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        var message = std.ArrayList(u8).init(self.allocator);
        defer message.deinit();

        var writer = message.writer();
        try writer.print(fmt, args);

        try self.reportError(message.items);
    }

    pub fn reportError(self: *Self, message: []const u8) !void {
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
    pub fn resolvePlaceholder(self: *Self, placeholder: *ObjTypeDef, resolved_type: *ObjTypeDef, constant: bool) anyerror!void {
        assert(placeholder.def_type == .Placeholder);

        if (Config.debug_placeholders) {
            std.debug.print("Attempts to resolve @{} ({s}) with @{} a {}{s}\n", .{
                @ptrToInt(placeholder),
                if (placeholder.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                @ptrToInt(resolved_type),
                resolved_type.def_type,
                if (resolved_type.optional) "?" else " ",
            });
        }

        // Both placeholders, we have to connect the child placeholder to a root placeholder so its not orphan
        if (resolved_type.def_type == .Placeholder) {
            if (Config.debug_placeholders) {
                std.debug.print(
                    "Replaced linked placeholder @{} ({s}) with rooted placeholder @{} ({s})\n",
                    .{
                        @ptrToInt(placeholder),
                        if (placeholder.resolved_type.?.Placeholder.name != null) placeholder.resolved_type.?.Placeholder.name.?.string else "unknown",
                        @ptrToInt(resolved_type),
                        if (resolved_type.resolved_type.?.Placeholder.name != null) resolved_type.resolved_type.?.Placeholder.name.?.string else "unknown",
                    },
                );
            }

            if (resolved_type.resolved_type.?.Placeholder.parent) |parent| {
                try parent.resolved_type.?.Placeholder.children.append(placeholder);
            }

            placeholder.* = resolved_type.*;
            return;
        }

        var placeholder_def: PlaceholderDef = placeholder.resolved_type.?.Placeholder;

        // Now walk the chain of placeholders and see if they hold up
        for (placeholder_def.children.items) |child| {
            var child_placeholder: PlaceholderDef = child.resolved_type.?.Placeholder;
            assert(child_placeholder.parent != null);
            assert(child_placeholder.parent_relation != null);

            if (Config.debug_placeholders) {
                std.debug.print(
                    "Attempts to resolve @{} ({s}) child placeholder @{} ({s}) with relation {}\n",
                    .{
                        @ptrToInt(placeholder),
                        if (placeholder.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                        @ptrToInt(child),
                        if (child_placeholder.name) |name| name.string else "unknown",
                        child_placeholder.parent_relation.?,
                    },
                );
            }

            switch (child_placeholder.parent_relation.?) {
                .Optional => {
                    try self.resolvePlaceholder(child, try resolved_type.cloneOptional(self.type_registry), false);
                },
                .Unwrap => {
                    try self.resolvePlaceholder(child, try resolved_type.cloneNonOptional(self.type_registry), false);
                },
                .Instance => {
                    try self.resolvePlaceholder(child, try resolved_type.toInstance(self.allocator, self.type_registry), false);
                },
                .Call => {
                    // Can we call the parent?
                    if (resolved_type.def_type != .Function) {
                        try self.reportErrorAt(placeholder_def.where, "Can't be called");
                        return;
                    }

                    // Is the child types resolvable with parent return type
                    if (resolved_type.def_type == .Function) {
                        try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Function.return_type, false);
                    }
                },
                .Subscript => {
                    if (resolved_type.def_type == .List) {
                        try self.resolvePlaceholder(child, resolved_type.resolved_type.?.List.item_type, false);
                    } else if (resolved_type.def_type == .Map) {
                        try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Map.value_type, false);
                    } else {
                        try self.reportErrorAt(placeholder_def.where, "Can't be subscripted");
                        return;
                    }
                },
                .Key => {
                    if (resolved_type.def_type == .Map) {
                        try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Map.key_type, false);
                    } else {
                        try self.reportErrorAt(placeholder_def.where, "Can't be a key");
                        return;
                    }
                },
                .FieldAccess => {
                    switch (resolved_type.def_type) {
                        .Object => {
                            // We can't create a field access placeholder without a name
                            assert(child_placeholder.name != null);

                            var object_def: ObjObject.ObjectDef = resolved_type.resolved_type.?.Object;

                            // Search for a field matching the placeholder
                            var resolved: bool = false;
                            if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                                try self.resolvePlaceholder(child, field, false);
                                resolved = true;
                            }

                            // Search for a method matching the placeholder
                            if (!resolved) {
                                if (object_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                                    try self.resolvePlaceholder(child, method_def, true);
                                    resolved = true;
                                }
                            }

                            // Search for a static field
                            if (!resolved) {
                                if (object_def.static_fields.get(child_placeholder.name.?.string)) |static_def| {
                                    try self.resolvePlaceholder(child, static_def, false);
                                    resolved = true;
                                }
                            }

                            // TODO: static fields in inherited classes??
                        },
                        .ObjectInstance => {
                            // We can't create a field access placeholder without a name
                            assert(child_placeholder.name != null);

                            var object_def: ObjObject.ObjectDef = resolved_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                            // Search for a field matching the placeholder
                            var resolved_as_field: bool = false;
                            if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                                try self.resolvePlaceholder(child, field, false);
                                resolved_as_field = true;
                            }

                            // Search for a method matching the placeholder
                            if (!resolved_as_field) {
                                if (object_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                                    try self.resolvePlaceholder(child, method_def, true);
                                }
                            }

                            // TODO:search in inherited classes??
                        },
                        .Enum => {
                            // We can't create a field access placeholder without a name
                            assert(child_placeholder.name != null);

                            var enum_def: ObjEnum.EnumDef = resolved_type.resolved_type.?.Enum;

                            // Search for a case matching the placeholder
                            for (enum_def.cases.items) |case| {
                                if (mem.eql(u8, case, child_placeholder.name.?.string)) {
                                    var enum_instance_def: ObjTypeDef.TypeUnion = .{ .EnumInstance = resolved_type };

                                    try self.resolvePlaceholder(child, try self.type_registry.getTypeDef(.{
                                        .def_type = .EnumInstance,
                                        .resolved_type = enum_instance_def,
                                    }), true);
                                    break;
                                }
                            }
                        },
                        else => {
                            try self.reportErrorAt(placeholder_def.where, "Doesn't support field access");
                            return;
                        },
                    }
                },
                .Assignment => {
                    if (constant) {
                        try self.reportErrorAt(placeholder_def.where, "Is constant.");
                        return;
                    }

                    // Assignment relation from a once Placeholder and now Class/Object/Enum is creating an instance
                    var child_type: *ObjTypeDef = try resolved_type.toInstance(self.allocator, self.type_registry);

                    // Is child type matching the parent?
                    try self.resolvePlaceholder(child, child_type, false);
                },
            }
        }

        if (Config.debug_placeholders) {
            std.debug.print(
                "Resolved placeholder @{} {s}{s} with @{}.{}{s}\n",
                .{
                    @ptrToInt(placeholder),
                    if (placeholder.resolved_type.?.Placeholder.name != null) placeholder.resolved_type.?.Placeholder.name.?.string else "unknown",
                    if (placeholder.optional) "?" else " ",
                    @ptrToInt(resolved_type),
                    resolved_type.def_type,
                    if (resolved_type.optional) "?" else " ",
                },
            );
        }

        // Overwrite placeholder with resolved_type
        placeholder.* = resolved_type.*;

        // TODO: should resolved_type be freed?
        // TODO: does this work with vm.type_defs? (i guess not)
    }

    // Skip tokens until we reach something that resembles a new statement
    fn synchronize(self: *Self) !void {
        self.parser.panic_mode = false;

        while (self.parser.current_token.?.token_type != .Eof) : (try self.advance()) {
            if (self.parser.previous_token.?.token_type == .Semicolon) {
                return;
            }

            switch (self.parser.current_token.?.token_type) {
                .Class,
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
                .Switch,
                => return,
                else => {},
            }
        }
    }

    fn declarations(self: *Self) !?*ParseNode {
        var docblock: ?Token = null;
        if (self.current.?.scope_depth == 0 and try self.match(.Docblock)) {
            docblock = self.parser.previous_token.?;
        }

        if (try self.match(.Extern)) {
            var node = try self.funDeclaration();
            node.docblock = docblock;

            return node;
        } else {
            const constant: bool = try self.match(.Const);

            const node = if (!constant and try self.match(.Object))
                try self.objectDeclaration(false)
            else if (!constant and try self.match(.Class))
                try self.objectDeclaration(true)
            else if (!constant and try self.match(.Enum))
                try self.enumDeclaration()
            else if (!constant and try self.match(.Fun))
                try self.funDeclaration()
            else if (try self.match(.Str))
                try self.varDeclaration(
                    try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Num))
                try self.varDeclaration(
                    try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Number }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Bool))
                try self.varDeclaration(
                    try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Type))
                try self.varDeclaration(
                    try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.LeftBracket))
                try self.listDeclaration(constant)
            else if (try self.match(.LeftBrace))
                try self.mapDeclaration(constant)
            else if (!constant and try self.match(.Test))
                try self.testStatement()
            else if (try self.match(.Function))
                try self.varDeclaration(try self.parseFunctionType(), .Semicolon, constant, true)
            else if (try self.match(.Import))
                try self.importStatement()
                // In the declaractive space, starting with an identifier is always a varDeclaration with a user type
            else if (try self.match(.Identifier))
                try self.userVarDeclaration(false, constant)
            else if (!constant and try self.match(.Export))
                try self.exportStatement()
            else
                null;

            if (node == null) {
                try self.reportError("No declaration or statement.");
            } else if (docblock != null) {
                node.?.docblock = docblock;
            }

            return node;
        }

        if (self.parser.panic_mode) {
            try self.synchronize();
        }

        return null;
    }

    fn declarationOrStatement(self: *Self, block_type: BlockType) !?*ParseNode {
        var hanging: bool = false;
        const constant: bool = try self.match(.Const);
        // Things we can match with the first token
        if (!constant and try self.match(.Fun)) {
            return try self.funDeclaration();
        } else if (try self.match(.Str)) {
            return try self.varDeclaration(
                try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Num)) {
            return try self.varDeclaration(
                try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Number }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Bool)) {
            return try self.varDeclaration(
                try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Type)) {
            return try self.varDeclaration(
                try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.LeftBracket)) {
            return try self.listDeclaration(constant);
        } else if (try self.match(.LeftBrace)) {
            return try self.mapDeclaration(constant);
        } else if (try self.match(.Function)) {
            return try self.varDeclaration(try self.parseFunctionType(), .Semicolon, constant, true);
        } else if (try self.match(.Identifier)) {
            // A declaration with a class/object/enum type is one of those:
            // - Type variable
            // - Type? variable
            // - prefix.Type variable
            // - prefix.Type? variable
            // As of now this is the only place where we need to check more than one token ahead
            // zig fmt: off
            if (self.check(.Identifier)
                or (self.check(.Dot) and try self.checkAhead(.Identifier, 0) and try self.checkAhead(.Identifier, 1))
                or (self.check(.Dot) and try self.checkAhead(.Identifier, 0) and try self.checkAhead(.Question, 1) and try self.checkAhead(.Identifier, 2))
                or (self.check(.Question) and try self.checkAhead(.Identifier, 0))) {
            // zig fmt: on
                return try self.userVarDeclaration(false, constant);
            } else {
                hanging = true;
            }
        }

        if (constant) {
            try self.reportError("`const` not allowed here.");
        }

        return try self.statement(hanging, block_type);
    }

    // When a break statement, will return index of jump to patch
    fn statement(self: *Self, hanging: bool, block_type: BlockType) !?*ParseNode {
        if (try self.match(.If)) {
            assert(!hanging);
            return try self.ifStatement(block_type);
        } else if (try self.match(.For)) {
            assert(!hanging);
            return try self.forStatement();
        } else if (try self.match(.ForEach)) {
            assert(!hanging);
            return try self.forEachStatement();
        } else if (try self.match(.While)) {
            assert(!hanging);
            return try self.whileStatement();
        } else if (try self.match(.Do)) {
            assert(!hanging);
            return try self.doUntilStatement();
        } else if (try self.match(.Return)) {
            assert(!hanging);
            return try self.returnStatement();
        } else if (try self.match(.Break)) {
            assert(!hanging);
            return try self.breakStatement(block_type);
        } else if (try self.match(.Continue)) {
            assert(!hanging);
            return try self.continueStatement(block_type);
        } else if (try self.match(.Import)) {
            assert(!hanging);
            return try self.importStatement();
        } else if (try self.match(.Throw)) {
            // For now we don't care about the type. Later if we have `Error` type of data, we'll type check this
            var error_value = try self.expression(false);

            try self.consume(.Semicolon, "Expected `;` after `throw` expression.");

            var node = try self.allocator.create(ThrowNode);
            node.* = .{ .error_value = error_value };

            return &node.node;
        } else {
            return try self.expressionStatement(hanging);
        }

        return null;
    }

    fn objectDeclaration(self: *Self, is_class: bool) !*ParseNode {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Object must be defined at top-level.");
        }

        // Get object name
        try self.consume(.Identifier, "Expected object name.");
        var object_name: Token = self.parser.previous_token.?.clone();

        var object_def = ObjObject.ObjectDef.init(
            self.allocator,
            try copyStringRaw(self.strings, self.allocator, object_name.lexeme, false),
        );

        var resolved_type = ObjTypeDef.TypeUnion{ .Object = object_def };

        // Create type
        var object_type: *ObjTypeDef = try self.type_registry.getTypeDef(
            .{
                .def_type = .Object,
                .resolved_type = resolved_type,
            },
        );

        var object_compiler: ObjectCompiler = .{
            .name = object_name,
            .type_def = object_type,
        };

        self.current_object = object_compiler;

        // Inherited class?
        var parent_slot: ?usize = null;
        if (is_class) {
            object_type.resolved_type.?.Object.inheritable = true;

            if (try self.match(.Less)) {
                try self.consume(.Identifier, "Expected identifier after `<`.");

                if (std.mem.eql(u8, object_name.lexeme, self.parser.previous_token.?.lexeme)) {
                    try self.reportError("A class can't inherit itself.");

                    // Continue as if we did not parse parent
                } else {
                    parent_slot = try self.parseUserType();
                    var parent: *ObjTypeDef = self.globals.items[parent_slot.?].type_def;

                    object_type.resolved_type.?.Object.super = parent;

                    self.beginScope();
                    _ = try self.addLocal(
                        Token{
                            .token_type = .Identifier,
                            .lexeme = "super",
                            .line = 0,
                            .column = 0,
                        },
                        try parent.toInstance(self.allocator, self.type_registry),
                        true,
                    );
                    self.markInitialized();
                }
            } else {
                self.beginScope();
            }
        } else {
            self.beginScope();
        }

        // Body
        try self.consume(.LeftBrace, "Expected `{` before object body.");

        var fields = std.StringHashMap(void).init(self.allocator);
        defer fields.deinit();
        var methods = std.StringHashMap(*ParseNode).init(self.allocator);
        var properties = std.StringHashMap(?*ParseNode).init(self.allocator);
        var docblocks = std.StringHashMap(?Token).init(self.allocator);
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            const docblock: ?Token = if (try self.match(.Docblock)) self.parser.previous_token.? else null;

            const static: bool = try self.match(.Static);

            if (try self.match(.Fun)) {
                var method_node: *ParseNode = try self.method(
                    if (static) object_type else try object_type.toInstance(self.allocator, self.type_registry),
                );

                var method_name: []const u8 = method_node.type_def.?.resolved_type.?.Function.name.string;

                if (fields.get(method_name) != null) {
                    try self.reportError("A member with that name already exists.");
                }

                // Does a placeholder exists for this name ?
                if (static) {
                    if (object_type.resolved_type.?.Object.static_placeholders.get(method_name)) |placeholder| {
                        try self.resolvePlaceholder(placeholder, method_node.type_def.?, true);

                        // Now we know the placeholder was a method
                        if (Config.debug_placeholders) {
                            std.debug.print(
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
                        try self.resolvePlaceholder(placeholder, method_node.type_def.?, true);

                        // Now we know the placeholder was a method
                        if (Config.debug_placeholders) {
                            std.debug.print(
                                "resolved method placeholder for `{s}`\n",
                                .{
                                    method_name,
                                },
                            );
                        }
                        _ = object_type.resolved_type.?.Object.placeholders.remove(method_name);
                    }
                }

                if (static) {
                    try object_type.resolved_type.?.Object.static_fields.put(method_name, method_node.type_def.?);
                } else {
                    try object_type.resolved_type.?.Object.methods.put(
                        method_name,
                        method_node.type_def.?,
                    );
                }

                try fields.put(method_name, {});
                try methods.put(method_name, method_node);
                try docblocks.put(method_name, docblock);
            } else {
                // TODO: constant object properties
                // const constant = try self.match(.Const);
                const property_type = try self.parseTypeDef();

                try self.consume(.Identifier, "Expected property name.");
                const property_name = self.parser.previous_token.?.clone();

                if (fields.get(property_name.lexeme) != null) {
                    try self.reportError("A member with that name already exists.");
                }

                // Does a placeholder exists for this name ?
                if (static) {
                    if (object_type.resolved_type.?.Object.static_placeholders.get(property_name.lexeme)) |placeholder| {
                        try self.resolvePlaceholder(placeholder, property_type, false);

                        // Now we know the placeholder was a field
                        if (Config.debug_placeholders) {
                            std.debug.print(
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
                        try self.resolvePlaceholder(placeholder, property_type, false);

                        // Now we know the placeholder was a field
                        if (Config.debug_placeholders) {
                            std.debug.print(
                                "resolved property placeholder for `{s}`\n",
                                .{
                                    property_name.lexeme,
                                },
                            );
                        }
                        _ = object_type.resolved_type.?.Object.placeholders.remove(property_name.lexeme);
                    }
                }

                var default: ?*ParseNode = null;

                if (try self.match(.Equal)) {
                    default = try self.expression(false);
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

                if (static) {
                    try object_type.resolved_type.?.Object.static_fields.put(
                        property_name.lexeme,
                        property_type,
                    );
                } else {
                    assert(!object_type.optional);
                    try object_type.resolved_type.?.Object.fields.put(property_name.lexeme, property_type);

                    if (default != null) {
                        try object_type.resolved_type.?.Object.fields_defaults.put(property_name.lexeme, {});
                    }
                }

                try fields.put(property_name.lexeme, {});
                try properties.put(property_name.lexeme, default);
                try docblocks.put(property_name.lexeme, docblock);
            }
        }

        try self.consume(.RightBrace, "Expected `}` after object body.");

        var node = try self.allocator.create(ObjectDeclarationNode);
        node.node.ends_scope = try self.endScope();

        const slot = try self.declareVariable(
            object_type,
            object_name,
            true, // Object is always constant
        );

        assert(!object_type.optional);

        self.markInitialized();

        node.* = ObjectDeclarationNode{
            .parent_slot = parent_slot,
            .slot = slot,
            .methods = methods,
            .properties = properties,
            .docblocks = docblocks,
        };
        node.node.type_def = object_type;
        node.node.location = self.parser.previous_token.?;

        assert(object_type.resolved_type.?.Object.placeholders.count() == 0 or object_type.resolved_type.?.Object.static_placeholders.count() == 0);

        self.current_object = null;

        return &node.node;
    }

    fn method(self: *Self, this: *ObjTypeDef) !*ParseNode {
        try self.consume(.Identifier, "Expected method name.");

        return try self.function(
            self.parser.previous_token.?.clone(),
            .Method,
            this,
        );
    }

    fn expressionStatement(self: *Self, hanging: bool) !*ParseNode {
        var node = try self.allocator.create(ExpressionNode);
        node.* = ExpressionNode{
            .expression = try self.expression(hanging),
        };
        node.node.location = node.expression.location;

        try self.consume(.Semicolon, "Expected `;` after expression.");

        return &node.node;
    }

    fn breakStatement(self: *Self, block_type: BlockType) !*ParseNode {
        if (block_type == .Function) {
            try self.reportError("break is not allowed here.");
        }

        try self.consume(.Semicolon, "Expected `;` after `break`.");

        var node = try self.allocator.create(BreakNode);
        node.* = .{};

        return &node.node;
    }

    fn continueStatement(self: *Self, block_type: BlockType) !*ParseNode {
        if (block_type == .Function) {
            try self.reportError("continue is not allowed here.");
        }

        try self.consume(.Semicolon, "Expected `;` after `continue`.");

        var node = try self.allocator.create(ContinueNode);
        node.* = .{};

        return &node.node;
    }

    fn ifStatement(self: *Self, block_type: BlockType) anyerror!*ParseNode {
        try self.consume(.LeftParen, "Expected `(` after `if`.");

        var condition: *ParseNode = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `if` condition.");

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");
        self.beginScope();
        var body = try self.block(block_type);
        body.ends_scope = try self.endScope();

        var else_branch: ?*ParseNode = null;
        if (try self.match(.Else)) {
            if (try self.match(.If)) {
                else_branch = try self.ifStatement(block_type);
            } else {
                try self.consume(.LeftBrace, "Expected `{` after `else`.");

                self.beginScope();
                else_branch = try self.block(block_type);
                else_branch.?.ends_scope = try self.endScope();
            }
        }

        var node = try self.allocator.create(IfNode);
        node.* = IfNode{
            .condition = condition,
            .body = body,
            .else_branch = else_branch,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn forStatement(self: *Self) !*ParseNode {
        try self.consume(.LeftParen, "Expected `(` after `for`.");

        self.beginScope();

        var init_declarations = std.ArrayList(*VarDeclarationNode).init(self.allocator);
        while (!self.check(.Semicolon) and !self.check(.Eof)) {
            try init_declarations.append(VarDeclarationNode.cast(try self.varDeclaration(try self.parseTypeDef(), .Nothing, false, true)).?);

            if (!self.check(.Semicolon)) {
                try self.consume(.Comma, "Expected `,` after for loop variable");
            }
        }

        try self.consume(.Semicolon, "Expected `;` after for loop variables.");

        var condition = try self.expression(false);

        try self.consume(.Semicolon, "Expected `;` after for loop condition.");

        var post_loop = std.ArrayList(*ParseNode).init(self.allocator);
        while (!self.check(.RightParen) and !self.check(.Eof)) {
            try post_loop.append(try self.expression(false));

            if (!self.check(.RightParen)) {
                try self.consume(.Comma, "Expected `,` after for loop expression");
            }
        }

        try self.consume(.RightParen, "Expected `)` after `for` expressions.");

        try self.consume(.LeftBrace, "Expected `{` after `for` definition.");

        self.beginScope();
        var body = try self.block(.For);
        body.ends_scope = try self.endScope();

        var node = try self.allocator.create(ForNode);
        node.* = ForNode{
            .init_declarations = init_declarations,
            .condition = condition,
            .post_loop = post_loop,
            .body = body,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.ends_scope = try self.endScope();

        return &node.node;
    }

    fn forEachStatement(self: *Self) !*ParseNode {
        try self.consume(.LeftParen, "Expected `(` after `foreach`.");

        self.beginScope();

        var key: ?*ParseNode = try self.varDeclaration(try self.parseTypeDef(), .Nothing, false, false);

        var value: ?*ParseNode = if (try self.match(.Comma))
            try self.varDeclaration(try self.parseTypeDef(), .Nothing, false, false)
        else
            null;

        try self.consume(.In, "Expected `in` after `foreach` variables.");

        var iterable = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `foreach`.");

        try self.consume(.LeftBrace, "Expected `{` after `foreach` definition.");

        self.beginScope();
        var body = try self.block(.ForEach);
        body.ends_scope = try self.endScope();

        // Only one variable: it's the value not the key
        if (value == null) {
            value = key;
            key = null;
        }

        var node = try self.allocator.create(ForEachNode);
        node.* = ForEachNode{
            .key = if (key != null) VarDeclarationNode.cast(key.?).? else null,
            .value = VarDeclarationNode.cast(value.?).?,
            .iterable = iterable,
            .block = body,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.ends_scope = try self.endScope();

        return &node.node;
    }

    fn whileStatement(self: *Self) !*ParseNode {
        try self.consume(.LeftParen, "Expected `(` after `while`.");

        var condition = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `while` condition.");

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");

        self.beginScope();

        var body = try self.block(.While);
        body.ends_scope = try self.endScope();

        var node = try self.allocator.create(WhileNode);
        node.* = WhileNode{
            .condition = condition,
            .block = body,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn doUntilStatement(self: *Self) !*ParseNode {
        try self.consume(.LeftBrace, "Expected `{` after `do`.");

        self.beginScope();

        var body = try self.block(.Do);
        body.ends_scope = try self.endScope();

        try self.consume(.Until, "Expected `until` after `do` block.");

        try self.consume(.LeftParen, "Expected `(` after `until`.");

        var condition = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `until` condition.");

        var node = try self.allocator.create(DoUntilNode);
        node.* = DoUntilNode{
            .condition = condition,
            .block = body,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn returnStatement(self: *Self) !*ParseNode {
        if (self.current.?.scope_depth == 0) {
            try self.reportError("Can't use `return` at top-level.");
        }

        var value: ?*ParseNode = null;
        if (!try self.match(.Semicolon)) {
            value = try self.expression(false);

            try self.consume(.Semicolon, "Expected `;` after return value.");
        }

        var node = try self.allocator.create(ReturnNode);
        node.* = ReturnNode{
            .value = value,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn varDeclaration(self: *Self, parsed_type: *ObjTypeDef, terminator: DeclarationTerminator, constant: bool, can_assign: bool) !*ParseNode {
        var var_type = try parsed_type.toInstance(self.allocator, self.type_registry);

        const slot: usize = try self.parseVariable(var_type, constant, "Expected variable name.");

        const name = self.parser.previous_token.?;

        const value = if (can_assign and try self.match(.Equal)) try self.expression(false) else null;

        if (var_type.def_type == .Placeholder and value != null and value.?.type_def != null and value.?.type_def.?.def_type == .Placeholder) {
            try PlaceholderDef.link(var_type, value.?.type_def.?, .Assignment);
        }

        var node = try self.allocator.create(VarDeclarationNode);
        node.* = VarDeclarationNode{
            .name = name,
            .value = value,
            .type_def = var_type,
            .constant = constant,
            .slot = slot,
            .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
        };
        node.node.location = name;

        switch (terminator) {
            .OptComma => _ = try self.match(.Comma),
            .Comma => try self.consume(.Comma, "Expected `,` after variable declaration."),
            .Semicolon => try self.consume(.Semicolon, "Expected `;` after variable declaration."),
            .Nothing => {},
        }

        self.markInitialized();

        return &node.node;
    }

    fn userVarDeclaration(self: *Self, _: bool, constant: bool) !*ParseNode {
        var user_type_name: Token = self.parser.previous_token.?.clone();
        var var_type: ?*ObjTypeDef = null;

        // Search for a global with that name
        if (try self.resolveGlobal(null, user_type_name)) |slot| {
            var_type = self.globals.items[slot].type_def;
        }

        // If none found, create a placeholder
        if (var_type == null) {
            var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                // TODO: token is wrong but what else can we put here?
                .Placeholder = PlaceholderDef.init(self.allocator, user_type_name),
            };

            placeholder_resolved_type.Placeholder.name = try copyStringRaw(
                self.strings,
                self.allocator,
                user_type_name.lexeme,
                false,
            );

            var_type = try self.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            _ = try self.declarePlaceholder(user_type_name, var_type);
        }

        if (try self.match(.Question)) {
            var_type = try var_type.?.cloneOptional(self.type_registry);
        }

        return try self.varDeclaration(var_type.?, .Semicolon, constant, true);
    }

    fn importStatement(self: *Self) anyerror!*ParseNode {
        var imported_symbols = std.StringHashMap(void).init(self.allocator);

        while ((try self.match(.Identifier)) and !self.check(.Eof)) {
            try imported_symbols.put(self.parser.previous_token.?.lexeme, .{});

            if (!self.check(.From) or self.check(.Comma)) { // Allow trailing comma
                try self.consume(.Comma, "Expected `,` after identifier.");
            }
        }

        var prefix: ?Token = null;
        if (imported_symbols.count() > 0) {
            try self.consume(.From, "Expected `from` after import identifier list.");
        }

        try self.consume(.String, "Expected import path.");

        var path = self.parser.previous_token.?;
        var file_name: []const u8 = path.lexeme[1..(path.lexeme.len - 1)];

        if (imported_symbols.count() == 0 and try self.match(.As)) {
            try self.consume(.Identifier, "Expected identifier after `as`.");
            prefix = self.parser.previous_token.?;
        }

        try self.consume(.Semicolon, "Expected `;` after import.");

        var import = try self.importScript(file_name, if (prefix) |pr| pr.lexeme else null, &imported_symbols);

        if (imported_symbols.count() > 0) {
            var it = imported_symbols.iterator();
            while (it.next()) |kv| {
                try self.reportErrorFmt("Unknown import `{s}`.", .{kv.key_ptr.*});
            }
        }

        var node = try self.allocator.create(ImportNode);
        node.* = ImportNode{
            .imported_symbols = if (imported_symbols.count() > 0) imported_symbols else null,
            .prefix = prefix,
            .path = path,
            .import = import,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn exportStatement(self: *Self) !*ParseNode {
        try self.consume(.Identifier, "Expected identifier after `export`.");
        var identifier = self.parser.previous_token.?;

        // Search for a global with that name
        if (try self.resolveGlobal(null, identifier)) |slot| {
            const global: *Global = &self.globals.items[slot];
            var alias: ?Token = null;

            global.exported = true;
            if (global.prefix != null or self.check(.As)) {
                try self.consume(.As, "Expected `as` after prefixed global.");
                try self.consume(.Identifier, "Expected identifier after `as`.");

                global.export_alias = self.parser.previous_token.?.lexeme;
                alias = self.parser.previous_token.?;
            }

            try self.consume(.Semicolon, "Expected `;` after export.");

            var node = try self.allocator.create(ExportNode);
            node.* = ExportNode{
                .identifier = identifier,
                .alias = alias,
            };
            node.node.location = self.parser.previous_token.?;

            return &node.node;
        }

        try self.reportError("Unknown global.");

        var node = try self.allocator.create(ExportNode);
        node.* = ExportNode{
            .identifier = identifier,
            .alias = identifier,
        };
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn funDeclaration(self: *Self) !*ParseNode {
        var function_type: FunctionType = .Function;

        if (self.parser.previous_token.?.token_type == .Extern) {
            try self.consume(.Fun, "Expected `fun` after `extern`.");

            function_type = .Extern;
        }

        // Placeholder until `function()` provides all the necessary bits
        var function_def_placeholder: ObjTypeDef = .{
            .def_type = .Function,
        };

        try self.consume(.Identifier, "Expected function name.");
        var name_token: Token = self.parser.previous_token.?;

        var slot: usize = try self.declareVariable(&function_def_placeholder, name_token, true);

        self.markInitialized();

        const is_main = std.mem.eql(u8, name_token.lexeme, "main") and self.current.?.function_node.node.type_def != null and self.current.?.function_node.node.type_def.?.resolved_type.?.Function.function_type == .ScriptEntryPoint;

        if (is_main) {
            if (function_type == .Extern) {
                try self.reportError("`main` can't be `extern`.");
            }

            function_type = .EntryPoint;
        }

        var function_node: *ParseNode = try self.function(name_token, function_type, null);

        if (function_node.type_def.?.resolved_type.?.Function.lambda) {
            try self.consume(.Semicolon, "Expected `;` after lambda function");
        }

        if (function_type == .Extern) {
            try self.consume(.Semicolon, "Expected `;` after `extern` function declaration.");
        }

        // Now that we have the full function type, get the local and update its type_def
        var function_def = function_node.type_def;
        if (self.current.?.scope_depth > 0) {
            self.current.?.locals[slot].type_def = function_def.?;
        } else {
            if (self.globals.items[slot].type_def.def_type == .Placeholder) {
                // Now that the function definition is complete, resolve the eventual placeholder
                try self.resolvePlaceholder(self.globals.items[slot].type_def, function_def.?, true);
            } else {
                self.globals.items[slot].type_def = function_def.?;
            }
        }

        self.markInitialized();

        var node = try self.allocator.create(FunDeclarationNode);
        node.* = FunDeclarationNode{
            .slot = slot,
            .slot_type = if (self.current.?.scope_depth == 0) .Global else .Local,
            .function = FunctionNode.cast(function_node).?,
        };

        return &node.node;
    }

    fn parseListType(self: *Self) !*ObjTypeDef {
        var list_item_type: *ObjTypeDef = try (try self.parseTypeDef()).toInstance(self.allocator, self.type_registry);

        try self.consume(.RightBracket, "Expected `]` after list type.");

        var list_def = ObjList.ListDef.init(self.allocator, list_item_type);
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .List = list_def,
        };

        return try self.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = resolved_type,
            },
        );
    }

    fn listDeclaration(self: *Self, constant: bool) !*ParseNode {
        return try self.varDeclaration(try self.parseListType(), .Semicolon, constant, true);
    }

    fn parseMapType(self: *Self) !*ObjTypeDef {
        var key_type: *ObjTypeDef = try (try self.parseTypeDef()).toInstance(self.allocator, self.type_registry);

        try self.consume(.Comma, "Expected `,` after key type.");

        var value_type: *ObjTypeDef = try (try self.parseTypeDef()).toInstance(self.allocator, self.type_registry);

        try self.consume(.RightBrace, "Expected `}` after value type.");

        var map_def = ObjMap.MapDef.init(self.allocator, key_type, value_type);
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .Map = map_def,
        };

        return try self.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .Map,
                .resolved_type = resolved_type,
            },
        );
    }

    fn mapDeclaration(self: *Self, constant: bool) !*ParseNode {
        return try self.varDeclaration(try self.parseMapType(), .Semicolon, constant, true);
    }

    fn enumDeclaration(self: *Self) !*ParseNode {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Enum must be defined at top-level.");
        }

        var enum_case_type: *ObjTypeDef = undefined;
        var case_type_picked: bool = false;
        if (try self.match(.LeftParen)) {
            enum_case_type = try self.parseTypeDef();
            try self.consume(.RightParen, "Expected `)` after enum type.");

            case_type_picked = true;
        } else {
            enum_case_type = try self.type_registry.getTypeDef(.{ .def_type = .Number });
        }

        enum_case_type = try enum_case_type.toInstance(self.allocator, self.type_registry);

        try self.consume(.Identifier, "Expected enum name.");
        var enum_name: Token = self.parser.previous_token.?.clone();

        var enum_def: ObjEnum.EnumDef = ObjEnum.EnumDef.init(
            self.allocator,
            try copyStringRaw(self.strings, self.allocator, enum_name.lexeme, false),
            enum_case_type,
        );

        var enum_resolved: ObjTypeDef.TypeUnion = .{ .Enum = enum_def };

        var enum_type: *ObjTypeDef = try self.type_registry.getTypeDef(
            .{
                .def_type = .Enum,
                .resolved_type = enum_resolved,
            },
        );

        const slot: usize = try self.declareVariable(enum_type, enum_name, true);
        self.markInitialized();

        try self.consume(.LeftBrace, "Expected `{` before enum body.");

        var cases = std.ArrayList(*ParseNode).init(self.allocator);
        var case_index: f64 = 0;
        while (!self.check(.RightBrace) and !self.check(.Eof)) : (case_index += 1) {
            if (case_index > 255) {
                try self.reportError("Too many enum cases.");
            }

            try self.consume(.Identifier, "Expected case name.");
            const case_name: []const u8 = self.parser.previous_token.?.lexeme;

            if (case_type_picked) {
                try self.consume(.Equal, "Expected `=` after case name.");

                try cases.append(try self.expression(false));
            } else {
                var constant_node = try self.allocator.create(NumberNode);
                constant_node.* = NumberNode{ .constant = case_index };
                constant_node.node.type_def = try self.type_registry.getTypeDef(.{
                    .def_type = .Number,
                });
                constant_node.node.location = self.parser.previous_token.?;

                try cases.append(&constant_node.node);
            }

            try enum_type.resolved_type.?.Enum.cases.append(case_name);

            // TODO: how to not force a comma at last case?
            try self.consume(.Comma, "Expected `,` after case definition.");
        }

        try self.consume(.RightBrace, "Expected `}` after enum body.");

        if (case_index == 0) {
            try self.reportError("Enum must have at least one case.");
        }

        var node = try self.allocator.create(EnumNode);
        node.* = EnumNode{
            .slot = slot,
            .cases = cases,
        };
        node.node.type_def = enum_type;
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn objectInit(self: *Self, _: bool, object: *ParseNode) anyerror!*ParseNode {
        var node = try self.allocator.create(ObjectInitNode);
        node.* = ObjectInitNode.init(self.allocator, object);
        node.node.location = self.parser.previous_token.?;

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.consume(.Identifier, "Expected property name");

            const property_name: []const u8 = self.parser.previous_token.?.lexeme;

            var property_placeholder: ?*ObjTypeDef = null;

            // Object is placeholder, create placeholder for the property and link it
            if (object.type_def != null and object.type_def.?.def_type == .Placeholder) {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, self.parser.previous_token.?),
                };
                placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, property_name, true);

                property_placeholder = try self.type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    },
                );

                try PlaceholderDef.link(object.type_def.?, property_placeholder.?, .FieldAccess);
            }

            try self.consume(.Equal, "Expected `=` after property name.");

            const expr = try self.expression(false);

            try node.properties.put(property_name, expr);

            if (!self.check(.RightBrace) or self.check(.Comma)) {
                try self.consume(.Comma, "Expected `,` after field initialization.");
            }
        }

        try self.consume(.RightBrace, "Expected `}` after object initialization.");

        node.node.type_def = if (object.type_def) |type_def| try type_def.toInstance(self.allocator, self.type_registry) else null;

        return &node.node;
    }

    pub fn expression(self: *Self, hanging: bool) !*ParseNode {
        return try self.parsePrecedence(.Assignment, hanging);
    }

    // Returns a list of break jumps to patch
    fn block(self: *Self, block_type: BlockType) anyerror!*ParseNode {
        var node = try self.allocator.create(BlockNode);
        node.* = BlockNode.init(self.allocator);

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            if (try self.declarationOrStatement(block_type)) |declOrStmt| {
                try node.statements.append(declOrStmt);
            }
        }

        try self.consume(.RightBrace, "Expected `}}` after block.");

        return &node.node;
    }

    inline fn getRule(token: TokenType) ParseRule {
        return rules[@enumToInt(token)];
    }

    fn parsePrecedence(self: *Self, precedence: Precedence, hanging: bool) !*ParseNode {
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
        var node: *ParseNode = try prefixRule.?(self, canAssign);

        while (@enumToInt(getRule(self.parser.current_token.?.token_type).precedence) >= @enumToInt(precedence)) {
            // Patch optional jumps
            if (self.opt_jumps) |jumps| {
                assert(jumps.items.len > 0);
                var first_jump: Precedence = jumps.items[0];

                if (@enumToInt(getRule(self.parser.current_token.?.token_type).precedence) < @enumToInt(first_jump)) {
                    jumps.deinit();
                    self.opt_jumps = null;

                    node.patch_opt_jumps = true;

                    if (node.type_def != null) {
                        node.type_def = try node.type_def.?.cloneOptional(self.type_registry);
                    }
                }
            }

            _ = try self.advance();

            var infixRule: InfixParseFn = getRule(self.parser.previous_token.?.token_type).infix.?;
            node = try infixRule(self, canAssign, node);
        }

        if (canAssign and (try self.match(.Equal))) {
            try self.reportError("Invalid assignment target.");
        }

        return node;
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) anyerror!*ParseNode {
        var var_def: ?*ObjTypeDef = null;
        var slot: usize = undefined;
        var slot_type: SlotType = undefined;
        if (try self.resolveLocal(self.current.?, name)) |uslot| {
            var_def = self.current.?.locals[uslot].type_def;
            slot = uslot;
            slot_type = .Local;
        } else if (try self.resolveUpvalue(self.current.?, name)) |uslot| {
            var_def = self.current.?.enclosing.?.locals[self.current.?.upvalues[uslot].index].type_def;
            slot = uslot;
            slot_type = .UpValue;
        } else if (try self.resolveGlobal(null, name)) |uslot| {
            var_def = self.globals.items[uslot].type_def;
            slot = uslot;
            slot_type = .Global;
        } else {
            slot = try self.declarePlaceholder(name, null);
            var_def = self.globals.items[slot].type_def;
            slot_type = .Global;
        }

        var value = if (can_assign and try self.match(.Equal)) try self.expression(false) else null;

        var node = try self.allocator.create(NamedVariableNode);
        node.* = NamedVariableNode{
            .identifier = name,
            .value = value,
            .slot = slot,
            .slot_type = slot_type,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = var_def;

        return &node.node;
    }

    fn number(self: *Self, _: bool) anyerror!*ParseNode {
        var node = try self.allocator.create(NumberNode);

        node.* = NumberNode{
            .constant = self.parser.previous_token.?.literal_number.?,
        };
        node.node.type_def = try self.type_registry.getTypeDef(.{
            .def_type = .Number,
        });
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn string(self: *Self, _: bool) anyerror!*ParseNode {
        return &(try StringParser.init(self, self.parser.previous_token.?.literal_string.?).parse()).node;
    }

    fn grouping(self: *Self, _: bool) anyerror!*ParseNode {
        var node: *ParseNode = try self.expression(false);
        try self.consume(.RightParen, "Expected ')' after expression.");

        return node;
    }

    fn literal(self: *Self, _: bool) anyerror!*ParseNode {
        switch (self.parser.previous_token.?.token_type) {
            .False => {
                var node = try self.allocator.create(BooleanNode);

                node.* = BooleanNode{ .constant = false };

                node.node.type_def = try self.type_registry.getTypeDef(.{
                    .def_type = .Bool,
                });

                node.node.location = self.parser.previous_token.?;

                return &node.node;
            },
            .True => {
                var node = try self.allocator.create(BooleanNode);

                node.* = BooleanNode{ .constant = true };

                node.node.type_def = try self.type_registry.getTypeDef(.{
                    .def_type = .Bool,
                });

                node.node.location = self.parser.previous_token.?;

                return &node.node;
            },
            .Null => {
                var node = try self.allocator.create(NullNode);

                node.* = NullNode{};

                node.node.type_def = try self.type_registry.getTypeDef(.{
                    .def_type = .Void,
                });

                node.node.location = self.parser.previous_token.?;

                return &node.node;
            },
            else => unreachable,
        }
    }

    fn unary(self: *Self, _: bool) anyerror!*ParseNode {
        var operator: TokenType = self.parser.previous_token.?.token_type;

        var left: *ParseNode = try self.parsePrecedence(.Unary, false);

        var node = try self.allocator.create(UnaryNode);
        node.* = UnaryNode{
            .left = left,
            .operator = operator,
        };
        node.node.type_def = left.type_def;
        node.node.location = self.parser.previous_token.?;

        return &node.node;
    }

    fn argumentList(self: *Self) !std.StringArrayHashMap(*ParseNode) {
        var arguments = std.StringArrayHashMap(*ParseNode).init(self.allocator);

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

            try arguments.put(if (!hanging and arg_name != null) arg_name.?.lexeme else "$", try self.expression(hanging));

            if (arg_count == 255) {
                try self.reportError("Can't have more than 255 arguments.");

                return arguments;
            }

            arg_count += 1;

            if (!(try self.match(.Comma))) {
                break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after arguments.");

        return arguments;
    }

    fn call(self: *Self, _: bool, callee: *ParseNode) anyerror!*ParseNode {
        var node = try self.allocator.create(CallNode);
        node.* = CallNode{
            .callee = callee,
            .arguments = try self.argumentList(),
            .catches = try self.inlineCatch(),
        };

        node.node.location = self.parser.previous_token.?;

        // Note: callee.node.type_def has been populated with the member being referenced

        // Node type is Function or Native return type or nothing/placeholder
        node.node.type_def = if (callee.type_def != null and callee.type_def.?.def_type == .Function)
            callee.type_def.?.resolved_type.?.Function.return_type
        else
            null;
        node.node.type_def = node.node.type_def orelse if (callee.type_def != null and callee.type_def.?.def_type == .Native)
            callee.type_def.?.resolved_type.?.Native.return_type
        else
            null;

        // If null, create placeholder
        if (node.node.type_def == null) {
            var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.allocator, node.node.location),
            };

            node.node.type_def = try self.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            assert(callee.type_def.?.def_type == .Placeholder);

            try PlaceholderDef.link(callee.type_def.?, node.node.type_def.?, .Call);
        }

        return &node.node;
    }

    fn unwrap(self: *Self, _: bool, unwrapped: *ParseNode) anyerror!*ParseNode {
        var node = try self.allocator.create(UnwrapNode);
        node.* = UnwrapNode{
            .unwrapped = unwrapped,
            .original_type = unwrapped.type_def,
        };
        node.node.location = self.parser.previous_token.?;

        node.node.type_def = if (unwrapped.type_def) |type_def| try type_def.cloneNonOptional(self.type_registry) else null;

        if (self.opt_jumps == null) {
            self.opt_jumps = std.ArrayList(Precedence).init(self.allocator);
        }
        try self.opt_jumps.?.append(getRule(self.parser.current_token.?.token_type).precedence);

        return &node.node;
    }

    fn forceUnwrap(self: *Self, _: bool, unwrapped: *ParseNode) anyerror!*ParseNode {
        var node = try self.allocator.create(ForceUnwrapNode);
        node.* = ForceUnwrapNode{
            .unwrapped = unwrapped,
            .original_type = unwrapped.type_def,
        };
        node.node.location = self.parser.previous_token.?;

        node.node.type_def = if (unwrapped.type_def) |type_def| try type_def.cloneNonOptional(self.type_registry) else null;

        return &node.node;
    }

    fn variable(self: *Self, can_assign: bool) anyerror!*ParseNode {
        return try self.namedVariable(self.parser.previous_token.?, can_assign);
    }

    fn getSuperMethod(self: *Self, object: *ObjTypeDef, name: []const u8) ?*ObjTypeDef {
        const obj_def: ObjObject.ObjectDef = object.resolved_type.?.Object;
        if (obj_def.methods.get(name)) |obj_method| {
            return obj_method;
        } else if (obj_def.super) |obj_super| {
            return self.getSuperMethod(obj_super, name);
        }

        return null;
    }

    fn getSuperField(self: *Self, object: *ObjTypeDef, name: []const u8) ?*ObjTypeDef {
        const obj_def: ObjObject.ObjectDef = object.resolved_type.?.Object;
        if (obj_def.fields.get(name)) |obj_field| {
            return obj_field;
        } else if (obj_def.super) |obj_super| {
            return self.getSuperField(obj_super, name);
        }

        return null;
    }

    fn dot(self: *Self, can_assign: bool, callee: *ParseNode) anyerror!*ParseNode {
        try self.consume(.Identifier, "Expected property name after `.`");
        var member_name_token: Token = self.parser.previous_token.?;
        var member_name: []const u8 = member_name_token.lexeme;

        var node = try self.allocator.create(DotNode);
        node.* = DotNode{
            .callee = callee,
            .identifier = self.parser.previous_token.?,
        };
        node.node.location = self.parser.previous_token.?;
        // Check that name is a property
        const callee_def_type = if (callee.type_def) |type_def| type_def.def_type else .Placeholder;
        switch (callee_def_type) {
            .String => {
                if (try ObjString.memberDef(self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

                        assert(member.def_type == .Native);
                        node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                        // Node type is the return type of the call
                        node.node.type_def = node.call.?.node.type_def;
                    } else {
                        // String has only native functions members
                        node.node.type_def = member;
                    }
                } else {
                    try self.reportError("String property doesn't exist.");
                }
            },
            .Object => {
                var obj_def: ObjObject.ObjectDef = callee.type_def.?.resolved_type.?.Object;

                var property_type: ?*ObjTypeDef = obj_def.static_fields.get(member_name) orelse obj_def.static_placeholders.get(member_name);

                // Not found, create a placeholder, this is a root placeholder not linked to anything
                // TODO: test with something else than a name
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.allocator, member_name_token),
                    };
                    placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, member_name_token.lexeme, true);

                    var placeholder: *ObjTypeDef = try self.type_registry.getTypeDef(.{
                        .optional = false,
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    });

                    if (Config.debug_placeholders) {
                        std.debug.print(
                            "static placeholder @{} for `{s}`\n",
                            .{
                                @ptrToInt(placeholder),
                                member_name_token.lexeme,
                            },
                        );
                    }
                    try callee.type_def.?.resolved_type.?.Object.static_placeholders.put(member_name, placeholder);

                    property_type = placeholder;
                } else if (property_type == null) {
                    try self.reportErrorFmt("Static property `{s}` does not exists in {s}", .{ member_name, obj_def.name.string });
                }

                // Do we assign it ?
                if (can_assign and try self.match(.Equal)) {
                    node.value = try self.expression(false);

                    node.node.type_def = property_type;
                } else if (try self.match(.LeftParen)) { // Do we call it
                    // `call` will look to the parent node for the function definition
                    node.node.type_def = property_type;
                    node.member_type_def = property_type;

                    node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                    // Node type is the return type of the call
                    node.node.type_def = node.call.?.node.type_def;
                } else { // access only
                    node.node.type_def = property_type;
                }
            },
            .ObjectInstance => {
                var object: *ObjTypeDef = callee.type_def.?.resolved_type.?.ObjectInstance;
                var obj_def: ObjObject.ObjectDef = object.resolved_type.?.Object;

                // Is it a method
                var property_type: ?*ObjTypeDef = obj_def.methods.get(member_name) orelse self.getSuperMethod(object, member_name);

                // Is it a property
                property_type = property_type orelse obj_def.fields.get(member_name) orelse obj_def.placeholders.get(member_name) orelse self.getSuperField(object, member_name);

                // Else create placeholder
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.allocator, member_name_token),
                    };
                    placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, member_name, true);

                    var placeholder: *ObjTypeDef = try self.type_registry.getTypeDef(.{
                        .optional = false,
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    });

                    if (Config.debug_placeholders) {
                        std.debug.print(
                            "property placeholder @{} for `{s}.{s}`\n",
                            .{
                                @ptrToInt(placeholder),
                                object.resolved_type.?.Object.name.string,
                                member_name_token.lexeme,
                            },
                        );
                    }
                    try object.resolved_type.?.Object.placeholders.put(member_name, placeholder);

                    property_type = placeholder;
                } else if (property_type == null) {
                    try self.reportErrorFmt("Property `{s}` does not exists in {s}", .{ member_name, obj_def.name.string });
                }

                // If its a field or placeholder, we can assign to it
                // TODO: here get info that field is constant or not
                if (can_assign and try self.match(.Equal)) {
                    node.value = try self.expression(false);

                    node.node.type_def = property_type;
                } else if (try self.match(.LeftParen)) { // If it's a method or placeholder we can call it
                    // `call` will look to the parent node for the function definition
                    node.node.type_def = property_type;
                    node.member_type_def = property_type;

                    node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                    // Node type is the return type of the call
                    node.node.type_def = node.call.?.node.type_def;
                } else {
                    node.node.type_def = property_type;
                }
            },
            .Enum => {
                var enum_def: ObjEnum.EnumDef = callee.type_def.?.resolved_type.?.Enum;

                for (enum_def.cases.items) |case, index| {
                    if (mem.eql(u8, case, member_name)) {
                        var enum_instance_resolved_type: ObjTypeDef.TypeUnion = .{
                            .EnumInstance = callee.type_def.?,
                        };

                        var enum_instance: *ObjTypeDef = try self.type_registry.getTypeDef(.{
                            .optional = false,
                            .def_type = .EnumInstance,
                            .resolved_type = enum_instance_resolved_type,
                        });

                        node.node.type_def = enum_instance;
                        node.enum_index = index;
                        break;
                    }
                }

                if (node.node.type_def == null) {
                    try self.reportError("Enum case doesn't exists.");
                }
            },
            .EnumInstance => {
                // Only available field is `.value` to get associated value
                if (!mem.eql(u8, member_name, "value")) {
                    try self.reportError("Enum provides only field `value`.");
                }

                node.node.type_def = callee.type_def.?.resolved_type.?.EnumInstance.resolved_type.?.Enum.enum_type;
            },
            .List => {
                if (try ObjList.ListDef.member(callee.type_def.?, self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

                        node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                        // Node type is the return type of the call
                        node.node.type_def = node.call.?.node.type_def;
                    } else {
                        node.node.type_def = member;
                    }
                } else {
                    try self.reportError("List property doesn't exist.");
                }
            },
            .Map => {
                if (try ObjMap.MapDef.member(callee.type_def.?, self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

                        node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                        // Node type is the return type of the call
                        node.node.type_def = node.call.?.node.type_def;
                    } else {
                        node.node.type_def = member;
                    }
                } else {
                    try self.reportError("Map property doesn't exist.");
                }
            },
            .Placeholder => {
                // We know nothing of the field
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.allocator, member_name_token),
                };

                placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, member_name, false);

                var placeholder = try self.type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    },
                );

                try PlaceholderDef.link(callee.type_def.?, placeholder, .FieldAccess);

                if (can_assign and try self.match(.Equal)) {
                    node.value = try self.expression(false);
                } else if (try self.match(.LeftParen)) {
                    // `call` will look to the parent node for the function definition
                    node.node.type_def = placeholder;
                    node.member_type_def = placeholder;

                    node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;
                    // TODO: here maybe we invoke instead of call??

                    // Node type is the return type of the call
                    node.node.type_def = node.call.?.node.type_def;
                } else {
                    node.node.type_def = placeholder;
                }
            },
            else => unreachable,
        }

        return &node.node;
    }

    fn and_(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        var right: *ParseNode = try self.parsePrecedence(.And, false);

        var node = try self.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = .And,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = try self.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn or_(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        var right: *ParseNode = try self.parsePrecedence(.And, false);

        var node = try self.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = .Or,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = try self.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn is(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const constant = Value{ .Obj = (try self.parseTypeDef()).toObj() };

        var node = try self.allocator.create(IsNode);
        node.* = IsNode{
            .left = left,
            .constant = constant,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = try self.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn binary(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const operator: TokenType = self.parser.previous_token.?.token_type;
        const rule: ParseRule = getRule(operator);

        const right: *ParseNode = try self.parsePrecedence(
            @intToEnum(Precedence, @enumToInt(rule.precedence) + 1),
            false,
        );

        var node = try self.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = operator,
        };

        node.node.location = self.parser.previous_token.?;

        node.node.type_def = switch (operator) {
            .QuestionQuestion => if (right.type_def orelse left.type_def) |type_def| try type_def.cloneNonOptional(self.type_registry) else null,

            .Greater,
            .Less,
            .GreaterEqual,
            .LessEqual,
            .BangEqual,
            .EqualEqual,
            => try self.type_registry.getTypeDef(ObjTypeDef{
                .def_type = .Bool,
            }),

            .Plus => left.type_def orelse right.type_def,

            .Minus,
            .Star,
            .Slash,
            .Percent,
            => try self.type_registry.getTypeDef(ObjTypeDef{
                .def_type = .Number,
            }),

            else => null,
        };

        return &node.node;
    }

    fn subscript(self: *Self, can_assign: bool, subscripted: *ParseNode) anyerror!*ParseNode {
        const index: *ParseNode = try self.expression(false);

        if (subscripted.type_def.?.def_type == .Placeholder and index.type_def.?.def_type == .Placeholder) {
            try PlaceholderDef.link(subscripted.type_def.?, index.type_def.?, .Key);
        }

        var subscripted_type_def: ?*ObjTypeDef = null;

        if (subscripted.type_def) |type_def| {
            if (!type_def.optional) {
                switch (type_def.def_type) {
                    .Placeholder, .String => subscripted_type_def = type_def,
                    .List => subscripted_type_def = type_def.resolved_type.?.List.item_type,
                    .Map => subscripted_type_def = try type_def.resolved_type.?.Map.value_type.cloneOptional(self.type_registry),
                    else => try self.reportErrorFmt("Type `{s}` is not subscriptable", .{try type_def.toString(self.allocator)}),
                }
            } else {
                try self.reportError("Optional type is not subscriptable");
            }
        }

        try self.consume(.RightBracket, "Expected `]`.");

        var value: ?*ParseNode = null;
        if (can_assign and try self.match(.Equal) and (subscripted.type_def == null or subscripted.type_def.?.def_type != .String)) {
            value = try self.expression(false);

            if (subscripted.type_def.?.def_type == .Placeholder and value.?.type_def.?.def_type == .Placeholder) {
                try PlaceholderDef.link(subscripted.type_def.?, value.?.type_def.?, .Subscript);
            }
        }

        var node = try self.allocator.create(SubscriptNode);
        node.* = SubscriptNode{
            .subscripted = subscripted,
            .index = index,
            .value = value,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = subscripted_type_def;

        return &node.node;
    }

    fn list(self: *Self, _: bool) anyerror!*ParseNode {
        var items = std.ArrayList(*ParseNode).init(self.allocator);
        var item_type: ?*ObjTypeDef = null;

        // A lone list expression is prefixed by `<type>`
        if (self.parser.previous_token.?.token_type == .Less) {
            item_type = try (try self.parseTypeDef()).toInstance(self.allocator, self.type_registry);

            if (try self.match(.Comma)) {
                return try self.mapFinalise(item_type.?);
            }

            try self.consume(.Greater, "Expected `>` after list type.");
            try self.consume(.LeftBracket, "Expected `[` after list type.");
        }

        while (!(try self.match(.RightBracket)) and !(try self.match(.Eof))) {
            var actual_item: *ParseNode = try self.expression(false);

            try items.append(actual_item);

            item_type = item_type orelse actual_item.type_def;

            if (!self.check(.RightBracket)) {
                try self.consume(.Comma, "Expected `,` after list item.");
            }
        }

        assert(item_type != null);

        var list_def = ObjList.ListDef.init(self.allocator, item_type.?);

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .List = list_def };

        var list_type: *ObjTypeDef = try self.type_registry.getTypeDef(
            .{
                .def_type = .List,
                .resolved_type = resolved_type,
            },
        );

        var node = try self.allocator.create(ListNode);
        node.* = ListNode{ .items = items.items };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = list_type;

        return &node.node;
    }

    fn map(self: *Self, _: bool) anyerror!*ParseNode {
        return try self.mapFinalise(null);
    }

    fn mapFinalise(self: *Self, parsed_key_type: ?*ObjTypeDef) anyerror!*ParseNode {
        var value_type: ?*ObjTypeDef = null;
        var key_type: ?*ObjTypeDef = parsed_key_type;

        // A lone map expression is prefixed by `<type, type>`
        // When key_type != null, we come from list() which just parsed `<type,`
        if (key_type != null) {
            value_type = try (try self.parseTypeDef()).toInstance(self.allocator, self.type_registry);

            try self.consume(.Greater, "Expected `>` after map type.");
            try self.consume(.LeftBrace, "Expected `{` before map entries.");
        }

        var keys = std.ArrayList(*ParseNode).init(self.allocator);
        var values = std.ArrayList(*ParseNode).init(self.allocator);

        while (!(try self.match(.RightBrace)) and !(try self.match(.Eof))) {
            var key: *ParseNode = try self.expression(false);
            try self.consume(.Colon, "Expected `:` after key.");
            var value: *ParseNode = try self.expression(false);

            try keys.append(key);
            try values.append(value);

            key_type = key_type orelse key.type_def;
            value_type = value_type orelse value.type_def;

            if (!self.check(.RightBrace)) {
                try self.consume(.Comma, "Expected `,` after map entry.");
            }
        }

        assert(key_type != null);
        assert(value_type != null);

        var map_def = ObjMap.MapDef.init(self.allocator, key_type.?, value_type.?);

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .Map = map_def };

        var map_type: *ObjTypeDef = try self.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .Map,
                .resolved_type = resolved_type,
            },
        );

        var node = try self.allocator.create(MapNode);
        node.* = MapNode{
            .keys = keys.items,
            .values = values.items,
        };
        node.node.location = self.parser.previous_token.?;
        node.node.type_def = map_type;

        return &node.node;
    }

    fn super(self: *Self, _: bool) anyerror!*ParseNode {
        try self.consume(.Dot, "Expected `.` after `super`.");
        try self.consume(.Identifier, "Expected superclass method name.");
        var member_name = self.parser.previous_token.?;

        var node = try self.allocator.create(SuperNode);
        node.* = SuperNode{
            .identifier = member_name,
            .this = NamedVariableNode.cast(try self.namedVariable(
                Token{
                    .token_type = .Identifier,
                    .lexeme = "this",
                    .line = 0,
                    .column = 0,
                },
                false,
            )).?,
        };
        node.node.location = self.parser.previous_token.?;

        if (try self.match(.LeftParen)) {
            const super_method: ?*ObjTypeDef = self.getSuperMethod(self.current_object.?.type_def, member_name.lexeme);

            // call will look at parent node for function definition
            node.node.type_def = super_method;

            node.call = CallNode.cast(try self.call(false, &node.node)).?;
            node.call.?.super = NamedVariableNode.cast(try self.namedVariable(
                Token{
                    .token_type = .Identifier,
                    .lexeme = "super",
                    .line = 0,
                    .column = 0,
                },
                false,
            )).?;

            node.node.type_def = if (super_method) |umethod| umethod.resolved_type.?.Function.return_type else null;

            node.member_type_def = super_method;

            return &node.node;
        } else {
            node.super = NamedVariableNode.cast(try self.namedVariable(
                Token{
                    .token_type = .Identifier,
                    .lexeme = "super",
                    .line = 0,
                    .column = 0,
                },
                false,
            )).?;

            node.node.type_def = self.getSuperMethod(self.current_object.?.type_def, member_name.lexeme) orelse self.getSuperField(self.current_object.?.type_def, member_name.lexeme);
            node.member_type_def = node.node.type_def;

            return &node.node;
        }
    }

    fn fun(self: *Self, _: bool) anyerror!*ParseNode {
        return try self.function(null, .Anonymous, null);
    }

    fn function(self: *Self, name: ?Token, function_type: FunctionType, this: ?*ObjTypeDef) !*ParseNode {
        var function_node = try self.allocator.create(FunctionNode);
        function_node.* = try FunctionNode.init(
            self,
            function_type,
            if (name) |uname| uname.lexeme else "anonymous",
        );
        try self.beginFrame(function_type, function_node, this);
        self.beginScope();

        // The functiont tyepdef is created in several steps, some need already parsed information like return type
        // We create the incomplete type now and enrich it.
        var function_typedef: ObjTypeDef = .{
            .def_type = .Function,
        };

        var function_def = ObjFunction.FunctionDef{
            .name = if (name) |uname|
                try copyStringRaw(
                    self.strings,
                    self.allocator,
                    uname.lexeme,
                    false,
                )
            else
                try copyStringRaw(
                    self.strings,
                    self.allocator,
                    "anonymous",
                    false,
                ),
            .return_type = undefined,
            .parameters = std.StringArrayHashMap(*ObjTypeDef).init(self.allocator),
            .defaults = std.StringArrayHashMap(Value).init(self.allocator),
            .function_type = function_type,
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = function_def };

        function_typedef.resolved_type = function_resolved_type;

        // We replace it with a self.type_registry.getTypeDef pointer at the end
        function_node.node.type_def = &function_typedef;

        // Parse argument list
        if (function_type == .Test) {
            try self.consume(.String, "Expected `str` after `test`.");
            function_node.test_message = try self.string(false);
        } else if (function_type != .Catch or !(try self.match(.Default))) {
            try self.consume(.LeftParen, "Expected `(` after function name.");

            var arity: usize = 0;
            if (!self.check(.RightParen)) {
                while (true) {
                    arity += 1;
                    if (arity > 255) {
                        try self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                    }

                    if (function_type == .Catch and arity > 1) {
                        try self.reportErrorAtCurrent("`catch` clause can't have more than one parameter.");
                    }

                    var param_type: *ObjTypeDef = try self.parseTypeDef();
                    // Convert to instance if revelant
                    param_type = switch (param_type.def_type) {
                        .Object => object: {
                            var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .ObjectInstance = param_type };

                            break :object try self.type_registry.getTypeDef(
                                .{
                                    .optional = try self.match(.Question),
                                    .def_type = .ObjectInstance,
                                    .resolved_type = resolved_type,
                                },
                            );
                        },
                        .Enum => enum_instance: {
                            var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .EnumInstance = param_type };

                            break :enum_instance try self.type_registry.getTypeDef(
                                .{
                                    .optional = try self.match(.Question),
                                    .def_type = .EnumInstance,
                                    .resolved_type = resolved_type,
                                },
                            );
                        },
                        else => param_type,
                    };

                    var slot: usize = try self.parseVariable(
                        param_type,
                        true, // function arguments are constant
                        "Expected parameter name",
                    );
                    var arg_name: []const u8 = undefined;

                    if (self.current.?.scope_depth > 0) {
                        var local: Local = self.current.?.locals[slot];
                        arg_name = local.name.string;
                        try function_node.node.type_def.?.resolved_type.?.Function.parameters.put(local.name.string, local.type_def);
                    } else {
                        var global: Global = self.globals.items[slot];
                        arg_name = global.name.string;
                        try function_node.node.type_def.?.resolved_type.?.Function.parameters.put(global.name.string, global.type_def);
                    }

                    self.markInitialized();

                    // Default arguments
                    if (function_type == .Function or function_type == .Method or function_type == .Anonymous) {
                        if (try self.match(.Equal)) {
                            var expr = try self.expression(false);

                            if (expr.type_def != null and expr.type_def.?.def_type == .Placeholder and param_type.def_type == .Placeholder) {
                                try PlaceholderDef.link(param_type, expr.type_def.?, .Assignment);
                            }

                            if (!expr.isConstant()) {
                                try self.reportError("Default parameters must be constant values.");
                            }

                            try function_node.node.type_def.?.resolved_type.?.Function.defaults.put(arg_name, (try expr.toValue(self.allocator, self.strings)).?);
                        } else if (param_type.optional) {
                            var null_node: *NullNode = try self.allocator.create(NullNode);
                            null_node.* = .{};
                            null_node.node.type_def = try self.type_registry.getTypeDef(.{ .def_type = .Void });

                            try function_node.node.type_def.?.resolved_type.?.Function.defaults.put(arg_name, (try null_node.node.toValue(self.allocator, self.strings)).?);
                        }
                    }

                    if (!try self.match(.Comma)) break;
                }
            }

            try self.consume(.RightParen, "Expected `)` after function parameters.");
        }

        // Parse return type
        var parsed_return_type = false;
        if (function_type != .Test and (function_type != .Catch or self.check(.Greater))) {
            try self.consume(.Greater, "Expected `>` after function argument list.");

            const return_type = try self.parseTypeDef();

            function_node.node.type_def.?.resolved_type.?.Function.return_type = try return_type.toInstance(self.allocator, self.type_registry);

            parsed_return_type = true;
        }

        // Parse body
        if (try self.match(.Arrow)) {
            function_node.node.type_def.?.resolved_type.?.Function.lambda = true;
            function_node.arrow_expr = try self.expression(false);

            if (function_node.body) |placeholder_body| {
                self.allocator.destroy(placeholder_body);
            }
            function_node.body = null;

            if (!parsed_return_type and function_node.arrow_expr.?.type_def != null) {
                function_node.node.type_def.?.resolved_type.?.Function.return_type = function_node.arrow_expr.?.type_def.?;
                parsed_return_type = true;
            }
        } else if (function_type != .Extern) {
            try self.consume(.LeftBrace, "Expected `{` before function body.");
            function_node.body = BlockNode.cast(try self.block(.Function)).?;
        }

        if (!parsed_return_type) {
            function_node.node.type_def.?.resolved_type.?.Function.return_type = try self.type_registry.getTypeDef(.{ .def_type = .Void });
        }

        if (function_type == .Extern) {
            // Search for a dylib/so/dll with the same name as the current script
            if (try self.importLibSymbol(
                self.script_name,
                function_node.node.type_def.?.resolved_type.?.Function.name.string,
            )) |native| {
                function_node.native = native;
            }
        } else {
            // Bind upvalues
            var i: usize = 0;
            while (i < self.current.?.upvalue_count) : (i += 1) {
                try function_node.upvalue_binding.put(
                    self.current.?.upvalues[i].index,
                    if (self.current.?.upvalues[i].is_local) true else false,
                );
            }
        }

        function_node.node.type_def = try self.type_registry.getTypeDef(function_typedef);

        return &self.endFrame().node;
    }

    fn inlineCatch(self: *Self) ![]*ParseNode {
        var catches = std.ArrayList(*ParseNode).init(self.allocator);
        if (try self.match(.Catch)) {
            // Catch closures
            if (try self.match(.LeftBrace)) {
                var catch_count: u8 = 0;
                while (!self.check(.RightBrace) and !self.check(.Eof)) {
                    try catches.append(try self.function(null, .Catch, null));

                    catch_count += 1;

                    if (catch_count > 255) {
                        try self.reportError("Maximum catch closures is 255.");

                        return catches.items;
                    }

                    if (self.check(.Comma) or !self.check(.RightBrace)) {
                        try self.consume(.Comma, "Expected `,` between catch closures.");
                    }
                }

                try self.consume(.RightBrace, "Expected `}` after catch closures.");
            } else {
                try catches.append(try self.expression(false));
            }
        }

        return catches.items;
    }

    // `test` is just like a function but we don't parse arguments and we don't care about its return type
    fn testStatement(self: *Self) !*ParseNode {
        var function_def_placeholder: ObjTypeDef = .{
            .def_type = .Function,
        };

        var test_id = std.ArrayList(u8).init(self.allocator);
        try test_id.writer().print("$test#{}", .{self.test_count});
        // TODO: this string is never freed

        self.test_count += 1;

        const name_token: Token = Token{
            .token_type = .Test,
            .lexeme = test_id.items,
            .line = 0,
            .column = 0,
        };

        const slot = try self.declareVariable(&function_def_placeholder, name_token, true);

        self.markInitialized();

        const function_node = try self.function(name_token, FunctionType.Test, null);

        // Make it as a global definition
        var node = try self.allocator.create(VarDeclarationNode);
        node.* = VarDeclarationNode{
            .name = name_token,
            .value = function_node,
            .type_def = function_node.type_def,
            .constant = true,
            .slot = slot,
            .slot_type = .Global,
        };
        node.node.location = function_node.location;

        return node.toNode();
    }

    fn importScript(self: *Self, file_name: []const u8, prefix: ?[]const u8, imported_symbols: *std.StringHashMap(void)) anyerror!?ScriptImport {
        var import: ?ScriptImport = self.imports.get(file_name);

        if (import == null) {
            const buzz_path: ?[]const u8 = std.os.getenv("BUZZ_PATH") orelse ".";

            var lib_path = std.ArrayList(u8).init(self.allocator);
            defer lib_path.deinit();
            _ = try lib_path.writer().print(
                "{s}/{s}.buzz",
                .{ buzz_path, file_name },
            );

            var dir_path = std.ArrayList(u8).init(self.allocator);
            defer dir_path.deinit();
            _ = try dir_path.writer().print(
                "./{s}.buzz",
                .{file_name},
            );

            // Find and read file
            var file: std.fs.File = (if (std.fs.path.isAbsolute(lib_path.items))
                std.fs.openFileAbsolute(lib_path.items, .{}) catch null
            else
                std.fs.cwd().openFile(lib_path.items, .{}) catch null) orelse (if (std.fs.path.isAbsolute(dir_path.items))
                std.fs.openFileAbsolute(dir_path.items, .{}) catch {
                    try self.reportErrorFmt("Could not find buzz script `{s}`", .{file_name});
                    return null;
                }
            else
                std.fs.cwd().openFile(dir_path.items, .{}) catch {
                    try self.reportErrorFmt("Could not find buzz script `{s}`", .{file_name});
                    return null;
                });

            defer file.close();

            // TODO: put source strings in a ArenaAllocator that frees everything at the end of everything
            const source = try self.allocator.alloc(u8, (try file.stat()).size);
            // defer self.allocator.free(source);

            _ = try file.readAll(source);

            var parser = Parser.init(self.allocator, self.strings, self.imports, self.type_registry, true);
            defer parser.deinit();

            if (try parser.parse(source, file_name)) |import_node| {
                FunctionNode.cast(import_node).?.import_root = true;

                import = ScriptImport{
                    .function = import_node,
                    .globals = std.ArrayList(Global).init(self.allocator),
                };

                for (parser.globals.items) |*global| {
                    if (global.exported) {
                        global.*.exported = false;

                        if (global.export_alias) |export_alias| {
                            global.*.name.string = export_alias;
                            global.*.export_alias = null;
                        }
                    } else {
                        global.*.hidden = true;
                    }

                    global.*.prefix = prefix;

                    try import.?.globals.append(global.*);
                }

                try self.imports.put(file_name, import.?);
            }
        }

        if (import) |imported| {
            const selective_import = imported_symbols.count() > 0;
            for (imported.globals.items) |*global| {
                if (!global.hidden) {
                    if (imported_symbols.get(global.name.string) != null) {
                        _ = imported_symbols.remove(global.name.string);
                    } else if (selective_import) {
                        global.hidden = true;
                    }

                    // Search for name collision
                    if ((try self.resolveGlobal(prefix, Token.identifier(global.name.string))) != null) {
                        try self.reportError("Shadowed global");
                    }
                }

                // TODO: we're forced to import all and hide some because globals are indexed and not looked up by name at runtime
                //       Only way to avoid this is to go back to named globals at runtime. Then again, is it worth it?
                try self.globals.append(global.*);
            }
        } else {
            try self.reportError("Could not compile import.");
        }

        return import;
    }

    // TODO: when to close the lib?
    fn importLibSymbol(self: *Self, file_name: []const u8, symbol: []const u8) !?*ObjNative {
        const buzz_path: []const u8 = std.os.getenv("BUZZ_PATH") orelse ".";

        var lib_path = std.ArrayList(u8).init(self.allocator);
        defer lib_path.deinit();
        try lib_path.writer().print(
            "{s}/{s}.{s}",
            .{
                buzz_path,
                file_name,
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        var dir_path = std.ArrayList(u8).init(self.allocator);
        defer dir_path.deinit();
        try dir_path.writer().print(
            "./{s}.{s}",
            .{
                file_name, switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        var lib: ?std.DynLib = std.DynLib.open(lib_path.items) catch std.DynLib.open(dir_path.items) catch null;

        if (lib) |*dlib| {
            // Convert symbol names to zig slices
            var ssymbol = try (toNullTerminated(self.allocator, symbol) orelse Allocator.Error.OutOfMemory);
            defer self.allocator.free(ssymbol);

            // Lookup symbol NativeFn
            var symbol_method = dlib.lookup(NativeFn, ssymbol);

            if (symbol_method == null) {
                try self.reportErrorFmt("Could not find symbol `{s}` in lib `{s}`", .{ symbol, file_name });
                return null;
            }

            // Create a ObjNative with it
            var native = try self.allocator.create(ObjNative);
            native.* = .{ .native = symbol_method.? };

            return native;
        }

        if (builtin.os.tag == .macos) {
            try self.reportError(std.mem.sliceTo(dlerror(), 0));
        } else {
            try self.reportErrorFmt("Could not open lib `{s}`", .{file_name});
        }

        return null;
    }

    fn parseFunctionType(self: *Self) !*ObjTypeDef {
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
            return_type = try self.type_registry.getTypeDef(.{ .def_type = .Void });
        }

        var function_typedef: ObjTypeDef = .{
            .def_type = .Function,
            .optional = try self.match(.Question),
        };

        var function_def: ObjFunction.FunctionDef = .{
            .name = try copyStringRaw(self.strings, self.allocator, "anonymous", false),
            .return_type = try return_type.toInstance(self.allocator, self.type_registry),
            .parameters = parameters,
            .defaults = std.StringArrayHashMap(Value).init(self.allocator),
            .function_type = .Anonymous,
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = function_def };

        function_typedef.resolved_type = function_resolved_type;

        return try self.type_registry.getTypeDef(function_typedef);
    }

    fn parseUserType(self: *Self) !usize {
        var user_type_name: Token = self.parser.previous_token.?.clone();
        var var_type: ?*ObjTypeDef = null;
        var global_slot: ?usize = null;

        // Search for a global with that name
        if (try self.resolveGlobal(null, user_type_name)) |slot| {
            var_type = self.globals.items[slot].type_def;
            global_slot = slot;
        }

        // If none found, create a placeholder
        if (var_type == null) {
            var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.allocator, user_type_name),
            };

            placeholder_resolved_type.Placeholder.name = try copyStringRaw(
                self.strings,
                self.allocator,
                user_type_name.lexeme,
                false,
            );

            var_type = try self.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            global_slot = try self.declarePlaceholder(user_type_name, var_type.?);
        }

        return global_slot.?;
    }

    fn parseTypeDef(self: *Self) anyerror!*ObjTypeDef {
        if (try self.match(.Str)) {
            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String });
        } else if (try self.match(.Type)) {
            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type });
        } else if (try self.match(.Void)) {
            return try self.type_registry.getTypeDef(.{ .optional = false, .def_type = .Void });
        } else if (try self.match(.Num)) {
            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Number });
        } else if (try self.match(.Bool)) {
            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool });
        } else if (try self.match(.Type)) {
            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type });
        } else if (try self.match(.LeftBracket)) {
            return self.parseListType();
        } else if (try self.match(.LeftBrace)) {
            return self.parseMapType();
        } else if (try self.match(.Function)) {
            return try self.parseFunctionType();
        } else if ((try self.match(.Identifier))) {
            const user_type = self.globals.items[try self.parseUserType()].type_def;

            if (try self.match(.Question)) {
                return try user_type.cloneOptional(self.type_registry);
            }

            return user_type;
        } else {
            try self.reportErrorAtCurrent("Expected type definition.");

            return try self.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Void });
        }
    }

    fn parseVariable(self: *Self, variable_type: *ObjTypeDef, constant: bool, error_message: []const u8) !usize {
        try self.consume(.Identifier, error_message);

        return try self.declareVariable(variable_type, null, constant);
    }

    inline fn markInitialized(self: *Self) void {
        if (self.current.?.scope_depth == 0) {
            self.globals.items[self.globals.items.len - 1].initialized = true;
        } else {
            self.current.?.locals[self.current.?.local_count - 1].depth = @intCast(i32, self.current.?.scope_depth);
        }
    }

    fn declarePlaceholder(self: *Self, name: Token, placeholder: ?*ObjTypeDef) !usize {
        var placeholder_type: *ObjTypeDef = undefined;

        if (placeholder) |uplaceholder| {
            placeholder_type = uplaceholder;
        } else {
            var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.allocator, name),
            };
            placeholder_resolved_type.Placeholder.name = try copyStringRaw(self.strings, self.allocator, name.lexeme, false);

            placeholder_type = try self.type_registry.getTypeDef(.{ .def_type = .Placeholder, .resolved_type = placeholder_resolved_type });
        }

        assert(!placeholder_type.optional);

        const global: usize = try self.addGlobal(
            name,
            placeholder_type,
            false,
        );
        // markInitialized but we don't care what depth we are in
        self.globals.items[global].initialized = true;

        if (Config.debug_placeholders) {
            std.debug.print(
                "global placeholder @{} for `{s}` at {}\n",
                .{
                    @ptrToInt(placeholder_type),
                    name.lexeme,
                    global,
                },
            );
        }

        self.markInitialized();

        return global;
    }

    fn declareVariable(self: *Self, variable_type: *ObjTypeDef, name_token: ?Token, constant: bool) !usize {
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

            return try self.addLocal(name, variable_type, constant);
        } else {
            // Check a global with the same name doesn't exists
            for (self.globals.items) |global, index| {
                if (mem.eql(u8, name.lexeme, global.name.string) and !global.hidden) {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.type_def.resolved_type.?.Placeholder.name.?.string)) {

                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @enumToInt(variable_type.def_type) < @enumToInt(ObjTypeDef.Type.ObjectInstance)) {
                            try self.resolvePlaceholder(global.type_def, variable_type, constant);
                        }

                        return index;
                    } else {
                        try self.reportError("A global with the same name already exists.");
                    }
                }
            }

            return try self.addGlobal(name, variable_type, constant);
        }
    }

    fn addLocal(self: *Self, name: Token, local_type: *ObjTypeDef, constant: bool) !usize {
        if (self.current.?.local_count == 255) {
            try self.reportError("Too many local variables in scope.");
            return 0;
        }

        self.current.?.locals[self.current.?.local_count] = Local{
            .name = try copyStringRaw(self.strings, self.allocator, name.lexeme, false),
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
            .constant = constant,
        };

        self.current.?.local_count += 1;

        return self.current.?.local_count - 1;
    }

    fn dumpGlobals(self: *Self) !void {
        if (Config.debug) {
            for (self.globals.items) |global, index| {
                std.debug.print(
                    "global {}: {s} @{} {s}\n",
                    .{
                        index,
                        global.name.string,
                        @ptrToInt(global.type_def),
                        try global.type_def.toString(self.allocator),
                    },
                );
            }
        }
    }

    fn addGlobal(self: *Self, name: Token, global_type: *ObjTypeDef, constant: bool) !usize {
        // Search for an existing placeholder global with the same name
        for (self.globals.items) |global, index| {
            if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.name.string)) {
                if (global_type.def_type != .Placeholder) {
                    try self.resolvePlaceholder(global.type_def, global_type, constant);
                }

                return index;
            }
        }

        if (self.globals.items.len == 255) {
            try self.reportError("Too many global variables.");
            return 0;
        }

        try self.globals.append(
            Global{
                .name = try copyStringRaw(self.strings, self.allocator, name.lexeme, false),
                .type_def = global_type,
                .constant = constant,
            },
        );

        return self.globals.items.len - 1;
    }

    fn resolveLocal(self: *Self, frame: *Frame, name: Token) !?usize {
        if (frame.local_count == 0) {
            return null;
        }

        var i: usize = frame.local_count - 1;
        while (i >= 0) : (i -= 1) {
            var local: *Local = &frame.locals[i];
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

    // Will consume tokens if find a prefixed identifier
    fn resolveGlobal(self: *Self, prefix: ?[]const u8, name: Token) anyerror!?usize {
        if (self.globals.items.len == 0) {
            return null;
        }

        var i: usize = self.globals.items.len - 1;
        while (i >= 0) : (i -= 1) {
            var global: *Global = &self.globals.items[i];
            if (((prefix == null and global.prefix == null) or (prefix != null and global.prefix != null and mem.eql(u8, prefix.?, global.prefix.?))) and mem.eql(u8, name.lexeme, global.name.string) and !global.hidden) {
                if (!global.initialized) {
                    try self.reportError("Can't read global variable in its own initializer.");
                }

                return i;
                // Is it an import prefix?
            } else if (global.prefix != null and mem.eql(u8, name.lexeme, global.prefix.?)) {
                try self.consume(.Dot, "Expected `.` after import prefix.");
                try self.consume(.Identifier, "Expected identifier after import prefix.");
                return try self.resolveGlobal(global.prefix.?, self.parser.previous_token.?);
            }

            if (i == 0) break;
        }

        return null;
    }

    fn addUpvalue(self: *Self, frame: *Frame, index: usize, is_local: bool) !usize {
        var upvalue_count: u8 = frame.upvalue_count;

        var i: usize = 0;
        while (i < upvalue_count) : (i += 1) {
            var upvalue: *UpValue = &frame.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == 255) {
            try self.reportError("Too many closure variables in function.");
            return 0;
        }

        frame.upvalues[upvalue_count].is_local = is_local;
        frame.upvalues[upvalue_count].index = @intCast(u8, index);
        frame.upvalue_count += 1;

        return frame.upvalue_count - 1;
    }

    fn resolveUpvalue(self: *Self, frame: *Frame, name: Token) anyerror!?usize {
        if (frame.enclosing == null) {
            return null;
        }

        var local: ?usize = try self.resolveLocal(frame.enclosing.?, name);
        if (local) |resolved| {
            frame.enclosing.?.locals[resolved].is_captured = true;
            return try self.addUpvalue(frame, resolved, true);
        }

        var upvalue: ?usize = try self.resolveUpvalue(frame.enclosing.?, name);
        if (upvalue) |resolved| {
            return try self.addUpvalue(frame, resolved, false);
        }

        return null;
    }
};
