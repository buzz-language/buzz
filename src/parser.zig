const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

pub const pcre = @import("./pcre.zig").pcre;

const _obj = @import("./obj.zig");
const _node = @import("./node.zig");
const _token = @import("./token.zig");
const _vm = @import("./vm.zig");
const _value = @import("./value.zig");
const _scanner = @import("./scanner.zig");
const _chunk = @import("./chunk.zig");
const BuildOptions = @import("build_options");
const StringParser = @import("./string_parser.zig").StringParser;
const GarbageCollector = @import("./memory.zig").GarbageCollector;

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
const ObjFiber = _obj.ObjFiber;
const ObjObjectInstance = _obj.ObjObjectInstance;
const ObjObject = _obj.ObjObject;
const ObjectDef = _obj.ObjectDef;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const ObjEnum = _obj.ObjEnum;
const ObjEnumInstance = _obj.ObjEnumInstance;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjPattern = _obj.ObjPattern;
const PlaceholderDef = _obj.PlaceholderDef;
const Token = _token.Token;
const TokenType = _token.TokenType;
const Scanner = _scanner.Scanner;
const NativeFn = _obj.NativeFn;
const FunctionType = _obj.ObjFunction.FunctionType;
const SlotType = _node.SlotType;
const ParseNodeType = _node.ParseNodeType;
const ParseNode = _node.ParseNode;
const NamedVariableNode = _node.NamedVariableNode;
const IntegerNode = _node.IntegerNode;
const FloatNode = _node.FloatNode;
const BooleanNode = _node.BooleanNode;
const StringLiteralNode = _node.StringLiteralNode;
const StringNode = _node.StringNode;
const NullNode = _node.NullNode;
const VoidNode = _node.VoidNode;
const PatternNode = _node.PatternNode;
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
const AsyncCallNode = _node.AsyncCallNode;
const ResumeNode = _node.ResumeNode;
const ResolveNode = _node.ResolveNode;
const YieldNode = _node.YieldNode;
const VarDeclarationNode = _node.VarDeclarationNode;
const ExpressionNode = _node.ExpressionNode;
const GroupingNode = _node.GroupingNode;
const EnumNode = _node.EnumNode;
const ThrowNode = _node.ThrowNode;
const BreakNode = _node.BreakNode;
const ContinueNode = _node.ContinueNode;
const IfNode = _node.IfNode;
const InlineIfNode = _node.InlineIfNode;
const ReturnNode = _node.ReturnNode;
const ForNode = _node.ForNode;
const ForEachNode = _node.ForEachNode;
const WhileNode = _node.WhileNode;
const DoUntilNode = _node.DoUntilNode;
const BlockNode = _node.BlockNode;
const DotNode = _node.DotNode;
const FunDeclarationNode = _node.FunDeclarationNode;
const ObjectInitNode = _node.ObjectInitNode;
const ObjectDeclarationNode = _node.ObjectDeclarationNode;
const ProtocolDeclarationNode = _node.ProtocolDeclarationNode;
const ExportNode = _node.ExportNode;
const ImportNode = _node.ImportNode;
const TryNode = _node.TryNode;
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
    const Self = @This();

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
    generics: ?*std.AutoArrayHashMap(*ObjString, *ObjTypeDef) = null,
    constants: std.ArrayList(Value),

    in_try: bool = false,

    pub fn searchGeneric(self: Self, name: *ObjString) ?*ObjTypeDef {
        if (self.generics) |generics| {
            if (generics.get(name)) |type_def| {
                return type_def;
            }
        }

        return if (self.enclosing) |enclosing| enclosing.searchGeneric(name) else null;
    }
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

    pub const Precedence = enum {
        None,
        Assignment, // =, -=, +=, *=, /=
        Is, // is
        Or, // or
        And, // and
        Equality, // ==, !=
        Comparison, // >=, <=, >, <
        Term, // +, -
        NullCoalescing, // ??
        Bitwise, // \, &, ^
        Shift, // >>, <<
        Factor, // /, *, %
        Unary, // +, ++, -, --, !
        Call, // call(), dot.ref, sub[script], optUnwrap?
        Primary, // literal, (grouped expression), identifier, [<type>, alist], {<a, map>, ...}
    };

    const ParseFn = *const fn (*Parser, bool) anyerror!*ParseNode;
    const InfixParseFn = *const fn (*Parser, bool, *ParseNode) anyerror!*ParseNode;

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
        .{ .prefix = anonymousObjectInit, .infix = dot, .precedence = .Call }, // Dot
        .{ .prefix = null, .infix = null, .precedence = .None }, // Comma
        .{ .prefix = null, .infix = null, .precedence = .None }, // Semicolon
        .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // Greater
        .{ .prefix = null, .infix = binary, .precedence = .Comparison }, // Less
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
        .{ .prefix = null, .infix = null, .precedence = .None }, // BangGreater
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
        .{ .prefix = null, .infix = or_, .precedence = .Or }, // Or
        .{ .prefix = null, .infix = and_, .precedence = .And }, // And
        .{ .prefix = null, .infix = null, .precedence = .None }, // Return
        .{ .prefix = inlineIf, .infix = null, .precedence = .None }, // If
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
        .{ .prefix = integer, .infix = null, .precedence = .None }, // Integer
        .{ .prefix = float, .infix = null, .precedence = .None }, // FloatValue
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
        .{ .prefix = null, .infix = null, .precedence = .None }, // From
        .{ .prefix = null, .infix = null, .precedence = .None }, // As
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
    };

    pub const ScriptImport = struct {
        function: *ParseNode,
        globals: std.ArrayList(Global),
        absolute_path: *ObjString,
    };

    gc: *GarbageCollector,
    scanner: ?Scanner = null,
    parser: ParserState,
    script_name: []const u8 = undefined,
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

    pub fn init(gc: *GarbageCollector, imports: *std.StringHashMap(ScriptImport), imported: bool) Self {
        return .{
            .gc = gc,
            .parser = ParserState.init(gc.allocator),
            .imports = imports,
            .imported = imported,
            .globals = std.ArrayList(Global).init(gc.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.parser.deinit();
    }

    pub fn parse(self: *Self, source: []const u8, file_name: []const u8) !?*ParseNode {
        if (self.scanner != null) {
            self.scanner = null;
        }

        self.scanner = Scanner.init(self.gc.allocator, file_name, source);

        const function_type: FunctionType = if (self.imported) .Script else .ScriptEntryPoint;
        var function_node = try self.gc.allocator.create(FunctionNode);
        function_node.* = try FunctionNode.init(
            self,
            function_type,
            file_name,
            null,
        );

        self.script_name = file_name;

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
            for (self.globals.items, 0..) |global, index| {
                if (mem.eql(u8, global.name.string, "main") and !global.hidden and global.prefix == null) {
                    function_node.main_slot = index;
                    break;
                }
            }
        }

        var test_slots = std.ArrayList(usize).init(self.gc.allocator);
        // Create an entry point wich runs all `test`
        for (self.globals.items, 0..) |global, index| {
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
        if (BuildOptions.debug_placeholders) {
            for (self.globals.items, 0..) |global, index| {
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
        self.current = try self.gc.allocator.create(Frame);
        self.current.?.* = Frame{
            .locals = [_]Local{undefined} ** 255,
            .upvalues = [_]UpValue{undefined} ** 255,
            .enclosing = enclosing,
            .function_node = function_node,
            .constants = std.ArrayList(Value).init(self.gc.allocator),
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
                    self.gc.allocator,
                    try self.gc.type_registry.getTypeDef(.{ .def_type = .String }),
                );

                var list_union: ObjTypeDef.TypeUnion = .{ .List = list_def };

                local.type_def = try self.gc.type_registry.getTypeDef(ObjTypeDef{ .def_type = .List, .resolved_type = list_union });
            },
            else => {
                // TODO: do we actually need to reserve that space since we statically know if we need it?
                // nothing
                local.type_def = try self.gc.type_registry.getTypeDef(ObjTypeDef{
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

        local.name = try self.gc.copyString(name);
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
        var closing = std.ArrayList(OpCode).init(self.gc.allocator);
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

    fn closeScope(self: *Self, upto_depth: usize) !std.ArrayList(OpCode) {
        var current = self.current.?;
        var closing = std.ArrayList(OpCode).init(self.gc.allocator);

        var local_count = current.local_count;
        while (local_count > 0 and current.locals[local_count - 1].depth > upto_depth - 1) {
            if (current.locals[local_count - 1].is_captured) {
                try closing.append(.OP_CLOSE_UPVALUE);
            } else {
                try closing.append(.OP_POP);
            }

            local_count -= 1;
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
        const lines: std.ArrayList([]const u8) = try token.getLines(self.gc.allocator, 3);
        defer lines.deinit();
        var report_line = std.ArrayList(u8).init(self.gc.allocator);
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
            try writer.print(" {s}\n\u{001b}[0m", .{line});

            if (l == token.line) {
                try writer.writeByteNTimes(' ', (if (token.column > 0) token.column - 1 else 0) + prefix_len);
                try writer.print("\u{001b}[31m^\u{001b}[0m\n", .{});
            }

            l += 1;
        }
        std.debug.print("{s}:{}:{}: \u{001b}[31mSyntax error:\u{001b}[0m {s}{s}", .{
            token.script_name,
            token.line + 1,
            token.column + 1,
            message,
            report_line.items,
        });

        if (BuildOptions.stop_on_report) {
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
        var message = std.ArrayList(u8).init(self.gc.allocator);
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
        var expected_str: []const u8 = try expected_type.toString(self.gc.allocator);
        var actual_str: []const u8 = try actual_type.toString(self.gc.allocator);
        var error_message: []u8 = try self.gc.allocator.alloc(u8, expected_str.len + actual_str.len + 200);
        defer {
            self.gc.allocator.free(error_message);
            self.gc.allocator.free(expected_str);
            self.gc.allocator.free(actual_str);
        }

        error_message = try std.fmt.bufPrint(error_message, "{s}: expected type `{s}`, got `{s}`", .{ message, expected_str, actual_str });

        try self.reportErrorAt(at, error_message);
    }

    fn reportTypeCheck(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8) !void {
        try self.reportTypeCheckAt(expected_type, actual_type, message, self.parser.previous_token.?);
    }

    fn resolvePlaceholderWithRelation(
        self: *Self,
        child: *ObjTypeDef,
        resolved_type: *ObjTypeDef,
        constant: bool,
        relation: PlaceholderDef.PlaceholderRelation,
    ) anyerror!void {
        var child_placeholder: PlaceholderDef = child.resolved_type.?.Placeholder;

        if (BuildOptions.debug_placeholders) {
            std.debug.print(
                "Attempts to resolve @{} child placeholder @{} ({s}) with relation {}\n",
                .{
                    @ptrToInt(resolved_type),
                    @ptrToInt(child),
                    if (child_placeholder.name) |name| name.string else "unknown",
                    child_placeholder.parent_relation.?,
                },
            );
        }

        switch (relation) {
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
                    try self.reportErrorAt(child_placeholder.where, "Can't be called");
                    return;
                }

                try self.resolvePlaceholder(
                    child,
                    if (child_placeholder.call_generics) |call_generics|
                        try resolved_type.resolved_type.?.Function.return_type.populateGenerics(
                            resolved_type.resolved_type.?.Function.id,
                            call_generics,
                            &self.gc.type_registry,
                            null,
                        )
                    else
                        resolved_type.resolved_type.?.Function.return_type,
                    false,
                );
            },
            .Yield => {
                // Can we call the parent?
                if (resolved_type.def_type != .Function) {
                    try self.reportErrorAt(child_placeholder.where, "Can't be called");
                    return;
                }

                try self.resolvePlaceholder(
                    child,
                    if (child_placeholder.call_generics) |call_generics|
                        try resolved_type.resolved_type.?.Function.yield_type.populateGenerics(
                            resolved_type.resolved_type.?.Function.id,
                            call_generics,
                            &self.gc.type_registry,
                            null,
                        )
                    else
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
                    try self.resolvePlaceholder(child, try self.gc.type_registry.getTypeDef(.{ .def_type = .String }), false);
                } else {
                    try self.reportErrorAt(child_placeholder.where, "Can't be subscripted");
                    return;
                }
            },
            .Key => {
                if (resolved_type.def_type == .Map) {
                    try self.resolvePlaceholder(child, resolved_type.resolved_type.?.Map.key_type, false);
                } else if (resolved_type.def_type == .List or resolved_type.def_type == .String) {
                    try self.resolvePlaceholder(child, try self.gc.type_registry.getTypeDef(.{ .def_type = .Integer }), false);
                } else {
                    try self.reportErrorAt(child_placeholder.where, "Can't be a key");
                    return;
                }
            },
            .FieldAccess => {
                switch (resolved_type.def_type) {
                    .List => {
                        assert(child_placeholder.name != null);

                        if (try ObjList.ListDef.member(resolved_type, self, child_placeholder.name.?.string)) |member| {
                            try self.resolvePlaceholder(child, member, false);
                        }
                    },
                    .Map => {
                        assert(child_placeholder.name != null);

                        if (try ObjMap.MapDef.member(resolved_type, self, child_placeholder.name.?.string)) |member| {
                            try self.resolvePlaceholder(child, member, false);
                        }
                    },
                    .String => {
                        assert(child_placeholder.name != null);

                        if (try ObjString.memberDef(self, child_placeholder.name.?.string)) |member| {
                            try self.resolvePlaceholder(child, member, false);
                        }
                    },
                    .Pattern => {
                        assert(child_placeholder.name != null);

                        if (try ObjPattern.memberDef(self, child_placeholder.name.?.string)) |member| {
                            try self.resolvePlaceholder(child, member, false);
                        }
                    },
                    .Fiber => {
                        assert(child_placeholder.name != null);

                        if (try ObjFiber.memberDef(self, child_placeholder.name.?.string)) |member| {
                            try self.resolvePlaceholder(child, member, false);
                        }
                    },
                    .Object => {
                        // We can't create a field access placeholder without a name
                        assert(child_placeholder.name != null);

                        var object_def: ObjObject.ObjectDef = resolved_type.resolved_type.?.Object;

                        // Search for a field matching the placeholder
                        if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                            // TODO: remove? should only resolve with a field if field accessing an object instance?
                            try self.resolvePlaceholder(child, field, false);
                        } else if (object_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                            try self.resolvePlaceholder(child, method_def, true);
                        } else if (object_def.static_fields.get(child_placeholder.name.?.string)) |static_def| {
                            try self.resolvePlaceholder(child, static_def, false);
                        } else {
                            try self.reportErrorFmt(
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
                        assert(child_placeholder.name != null);

                        var object_def: ObjObject.ObjectDef = resolved_type.resolved_type.?.ObjectInstance.resolved_type.?.Object;

                        // Search for a field matching the placeholder
                        if (object_def.fields.get(child_placeholder.name.?.string)) |field| {
                            try self.resolvePlaceholder(child, field, false);
                        } else if (object_def.methods.get(child_placeholder.name.?.string)) |method_def| {
                            try self.resolvePlaceholder(child, method_def, true);
                        } else {
                            try self.reportErrorFmt(
                                "`{s}` has no field `{s}`",
                                .{
                                    object_def.name.string,
                                    child_placeholder.name.?.string,
                                },
                            );
                        }
                    },
                    .Enum => {
                        // We can't create a field access placeholder without a name
                        assert(child_placeholder.name != null);

                        var enum_def: ObjEnum.EnumDef = resolved_type.resolved_type.?.Enum;

                        // Search for a case matching the placeholder
                        for (enum_def.cases.items) |case| {
                            if (mem.eql(u8, case, child_placeholder.name.?.string)) {
                                var enum_instance_def: ObjTypeDef.TypeUnion = .{ .EnumInstance = resolved_type };

                                try self.resolvePlaceholder(child, try self.gc.type_registry.getTypeDef(.{
                                    .def_type = .EnumInstance,
                                    .resolved_type = enum_instance_def,
                                }), true);
                                break;
                            }
                        }
                    },
                    .EnumInstance => {
                        assert(child_placeholder.name != null);

                        if (std.mem.eql(u8, "value", child_placeholder.name.?.string)) {
                            try self.resolvePlaceholder(child, resolved_type.resolved_type.?.EnumInstance, false);
                        } else {
                            try self.reportErrorAt(child_placeholder.where, "Enum instance only has field `value`");
                            return;
                        }
                    },
                    else => {
                        try self.reportErrorAt(child_placeholder.where, "Doesn't support field access");
                        return;
                    },
                }
            },
            .Assignment => {
                if (constant) {
                    try self.reportErrorAt(child_placeholder.where, "Is constant.");
                    return;
                }

                // Assignment relation from a once Placeholder and now Object/Enum is creating an instance
                var child_type: *ObjTypeDef = try resolved_type.toInstance(self.gc.allocator, &self.gc.type_registry);

                // Is child type matching the parent?
                try self.resolvePlaceholder(child, child_type, false);
            },
        }
    }

    // When we encounter the missing declaration we replace it with the resolved type.
    // We then follow the chain of placeholders to see if their assumptions were correct.
    // If not we raise a compile error.
    pub fn resolvePlaceholder(self: *Self, placeholder: *ObjTypeDef, resolved_type: *ObjTypeDef, constant: bool) anyerror!void {
        assert(placeholder.def_type == .Placeholder);

        if (BuildOptions.debug_placeholders) {
            std.debug.print("Attempts to resolve @{} ({s}) with @{} a {}({})\n", .{
                @ptrToInt(placeholder),
                if (placeholder.resolved_type.?.Placeholder.name) |name| name.string else "unknown",
                @ptrToInt(resolved_type),
                resolved_type.def_type,
                resolved_type.optional,
            });
        }

        // Both placeholders, we have to connect the child placeholder to a root placeholder so its not orphan
        if (resolved_type.def_type == .Placeholder) {
            if (BuildOptions.debug_placeholders) {
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
            const obj = placeholder.obj;
            placeholder.* = resolved_type.*;
            placeholder.obj = obj;
            return;
        }

        var placeholder_def: PlaceholderDef = placeholder.resolved_type.?.Placeholder;

        if (BuildOptions.debug_placeholders) {
            std.debug.print(
                "Resolved placeholder @{} {s}({}) with @{}.{}({})\n",
                .{
                    @ptrToInt(placeholder),
                    if (placeholder.resolved_type.?.Placeholder.name != null) placeholder.resolved_type.?.Placeholder.name.?.string else "unknown",
                    placeholder.optional,
                    @ptrToInt(resolved_type),
                    resolved_type.def_type,
                    resolved_type.optional,
                },
            );
        }

        // Overwrite placeholder with resolved_type
        // Don't copy obj header or it will break the linked list of objects
        const obj = placeholder.obj;
        placeholder.* = resolved_type.*;
        placeholder.obj = obj;
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

    // Skip tokens until we reach something that resembles a new statement
    fn synchronize(self: *Self) !void {
        self.parser.panic_mode = false;

        while (self.parser.current_token.?.token_type != .Eof) : (try self.advance()) {
            if (self.parser.previous_token.?.token_type == .Semicolon) {
                return;
            }

            switch (self.parser.current_token.?.token_type) {
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
                .Throw,
                .Break,
                .Continue,
                .Export,
                .Import,
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
                try self.objectDeclaration()
            else if (!constant and try self.match(.Protocol))
                try self.protocolDeclaration()
            else if (!constant and try self.match(.Enum))
                try self.enumDeclaration()
            else if (!constant and try self.match(.Fun))
                try self.funDeclaration()
            else if (try self.match(.Pat))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Pattern }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Ud))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .UserData }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Str))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Int))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Integer }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Float))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Float }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Bool))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Type))
                try self.varDeclaration(
                    try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type }),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Fib))
                try self.varDeclaration(
                    try self.parseFiberType(null),
                    .Semicolon,
                    constant,
                    true,
                )
            else if (try self.match(.Obj))
                try self.varDeclaration(
                    try self.parseObjType(null),
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
                try self.varDeclaration(
                    try self.parseFunctionType(null),
                    .Semicolon,
                    constant,
                    true,
                )
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
                try self.reportError("Expected declaration or import/export statement");
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

    fn declarationOrStatement(self: *Self, loop_scope: ?LoopScope) !?*ParseNode {
        var hanging: bool = false;
        const constant: bool = try self.match(.Const);
        // Things we can match with the first token
        if (!constant and try self.match(.Fun)) {
            return try self.funDeclaration();
        } else if (try self.match(.Str)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Pat)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Pattern }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Ud)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .UserData }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Int)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Integer }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Float)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Float }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Bool)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Type)) {
            return try self.varDeclaration(
                try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type }),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Fib)) {
            return try self.varDeclaration(
                try self.parseFiberType(null),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Obj)) {
            return try self.varDeclaration(
                try self.parseObjType(null),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.LeftBracket)) {
            return try self.listDeclaration(constant);
        } else if (try self.match(.LeftBrace)) {
            return try self.mapDeclaration(constant);
        } else if (try self.match(.Function)) {
            return try self.varDeclaration(
                try self.parseFunctionType(null),
                .Semicolon,
                constant,
                true,
            );
        } else if (try self.match(.Identifier)) {
            // A declaration with a object/enum type is one of those:
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

        return try self.statement(hanging, loop_scope);
    }

    // When a break statement, will return index of jump to patch
    fn statement(self: *Self, hanging: bool, loop_scope: ?LoopScope) !?*ParseNode {
        if (try self.match(.If)) {
            assert(!hanging);
            return try self.ifStatement(loop_scope);
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
        } else if (try self.match(.Try)) {
            assert(!hanging);
            return try self.tryStatement();
        } else if (try self.match(.Break)) {
            assert(!hanging);
            return try self.breakStatement(loop_scope);
        } else if (try self.match(.Continue)) {
            assert(!hanging);
            return try self.continueStatement(loop_scope);
        } else if (try self.match(.Import)) {
            assert(!hanging);
            return try self.importStatement();
        } else if (try self.match(.Throw)) {
            const start_location = self.parser.previous_token.?;
            // For now we don't care about the type. Later if we have `Error` type of data, we'll type check this
            var error_value = try self.expression(false);

            try self.consume(.Semicolon, "Expected `;` after `throw` expression.");

            var node = try self.gc.allocator.create(ThrowNode);
            node.* = .{
                .error_value = error_value,
                .unconditional = self.current.?.scope_depth == 1,
            };
            node.node.location = start_location;
            node.node.end_location = self.parser.previous_token.?;

            return &node.node;
        } else {
            return try self.expressionStatement(hanging);
        }

        return null;
    }

    fn objectDeclaration(self: *Self) !*ParseNode {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Object must be defined at top-level.");
        }

        const start_location = self.parser.previous_token.?;

        // Conforms to protocols?
        var protocols = std.AutoHashMap(*ObjTypeDef, void).init(self.gc.allocator);
        var protocol_count: usize = 0;
        if (try self.match(.LeftParen)) {
            while (!self.check(.RightParen) and !self.check(.Eof)) : (protocol_count += 1) {
                if (protocol_count > 255) {
                    try self.reportError("Can't conform to more than 255 protocols");
                }

                try self.consume(.Identifier, "Expected protocol identifier");

                const protocol_slot = try self.parseUserType();
                const protocol = self.globals.items[protocol_slot].type_def;

                if (protocols.get(protocol) != null) {
                    try self.reportErrorFmt("Already conforming to `{s}`.", .{protocol.resolved_type.?.Protocol.name.string});
                }

                try protocols.put(protocol, {});

                if (!(try self.match(.Comma))) {
                    break;
                }
            }

            try self.consume(.RightParen, "Expected `)` after protocols list");
        }

        // Get object name
        try self.consume(.Identifier, "Expected object name.");
        const object_name: Token = self.parser.previous_token.?.clone();

        // Qualified name to avoid cross script collision
        const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
        defer self.gc.allocator.free(qualifier);
        var qualified_object_name = std.ArrayList(u8).init(self.gc.allocator);
        defer qualified_object_name.deinit();
        try qualified_object_name.writer().print("{s}.{s}", .{ qualifier, object_name.lexeme });

        // Create a placeholder for self-reference which will be resolved at the end when declaring the object
        var placeholder_index = try self.declarePlaceholder(object_name, null);
        var object_placeholder = self.globals.items[placeholder_index].type_def;

        var object_def = ObjObject.ObjectDef.init(
            self.gc.allocator,
            try self.gc.copyString(object_name.lexeme),
            try self.gc.copyString(qualified_object_name.items),
            false,
        );

        object_def.conforms_to.deinit();
        object_def.conforms_to = protocols;

        var resolved_type = ObjTypeDef.TypeUnion{ .Object = object_def };

        // Create type
        var object_type: ObjTypeDef = .{
            .def_type = .Object,
            .resolved_type = resolved_type,
        };

        var object_compiler: ObjectCompiler = .{
            .name = object_name,
            .type_def = object_placeholder,
        };

        self.current_object = object_compiler;
        self.beginScope();

        // Body
        try self.consume(.LeftBrace, "Expected `{` before object body.");

        var fields = std.StringArrayHashMap(void).init(self.gc.allocator);
        defer fields.deinit();
        var field_order = std.ArrayList([]const u8).init(self.gc.allocator);
        var methods = std.StringHashMap(*ParseNode).init(self.gc.allocator);
        var properties = std.StringHashMap(?*ParseNode).init(self.gc.allocator);
        var properties_type = std.StringHashMap(*ObjTypeDef).init(self.gc.allocator);
        var docblocks = std.StringHashMap(?Token).init(self.gc.allocator);
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            const docblock: ?Token = if (try self.match(.Docblock)) self.parser.previous_token.? else null;

            const static: bool = try self.match(.Static);

            if (try self.match(.Fun)) {
                var method_node: *ParseNode = try self.method(
                    false,
                    if (static) object_placeholder else try object_placeholder.toInstance(self.gc.allocator, &self.gc.type_registry),
                );

                method_node.docblock = docblock;

                if (FunctionNode.cast(method_node)) |function_node| {
                    function_node.static = static;
                }

                var method_name: []const u8 = method_node.type_def.?.resolved_type.?.Function.name.string;

                if (fields.get(method_name) != null) {
                    try self.reportError("A member with that name already exists.");
                }

                // Does a placeholder exists for this name ?
                if (static) {
                    if (object_type.resolved_type.?.Object.static_placeholders.get(method_name)) |placeholder| {
                        try self.resolvePlaceholder(placeholder, method_node.type_def.?, true);

                        // Now we know the placeholder was a method
                        if (BuildOptions.debug_placeholders) {
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
                        if (BuildOptions.debug_placeholders) {
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

                try field_order.append(method_name);
                try fields.put(method_name, {});
                try methods.put(method_name, method_node);
                try properties_type.put(method_name, method_node.type_def.?);
                try docblocks.put(method_name, docblock);
            } else {
                // TODO: constant object properties
                // const constant = try self.match(.Const);
                const property_type = try (try self.parseTypeDef(null)).toInstance(self.gc.allocator, &self.gc.type_registry);

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
                        if (BuildOptions.debug_placeholders) {
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
                        if (BuildOptions.debug_placeholders) {
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

                    if (!default.?.isConstant(default.?)) {
                        try self.reportErrorAt(default.?.location, "Default value must be constant");
                    }
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

                try field_order.append(property_name.lexeme);
                try fields.put(property_name.lexeme, {});
                try properties.put(property_name.lexeme, default);
                try properties_type.put(property_name.lexeme, property_type);
                try docblocks.put(property_name.lexeme, docblock);
            }
        }

        try self.consume(.RightBrace, "Expected `}` after object body.");

        var node = try self.gc.allocator.create(ObjectDeclarationNode);
        node.node.ends_scope = try self.endScope();

        const slot = try self.declareVariable(
            &object_type, // Should resolve object_placeholder and be discarded
            object_name,
            true, // Object is always constant
        );

        assert(!object_type.optional);

        self.markInitialized();

        node.* = ObjectDeclarationNode{
            .slot = slot,
            .methods = methods,
            .properties = properties,
            .docblocks = docblocks,
            .properties_type = properties_type,
            .fields = field_order.items,
        };
        node.node.type_def = object_placeholder;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        assert(object_placeholder.resolved_type.?.Object.placeholders.count() == 0 or object_placeholder.resolved_type.?.Object.static_placeholders.count() == 0);

        self.current_object = null;

        return &node.node;
    }

    fn method(self: *Self, abstract: bool, this: *ObjTypeDef) !*ParseNode {
        try self.consume(.Identifier, "Expected method name.");

        return try self.function(
            self.parser.previous_token.?.clone(),
            if (abstract) .Abstract else .Method,
            this,
        );
    }

    fn protocolDeclaration(self: *Self) !*ParseNode {
        if (self.current.?.scope_depth > 0) {
            try self.reportError("Protocol must be defined at top-level.");
        }

        const start_location = self.parser.previous_token.?;

        // Get protocol name
        try self.consume(.Identifier, "Expected protocol name.");
        const protocol_name: Token = self.parser.previous_token.?.clone();

        // Qualified name to avoid cross script collision
        const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
        defer self.gc.allocator.free(qualifier);
        var qualified_protocol_name = std.ArrayList(u8).init(self.gc.allocator);
        defer qualified_protocol_name.deinit();
        try qualified_protocol_name.writer().print("{s}.{s}", .{ qualifier, protocol_name.lexeme });

        // Create a placeholder for self-reference which will be resolved at the end when declaring the object
        var placeholder_index = try self.declarePlaceholder(protocol_name, null);
        var protocol_placeholder = self.globals.items[placeholder_index].type_def;

        var protocol_def = ObjObject.ProtocolDef.init(
            self.gc.allocator,
            try self.gc.copyString(protocol_name.lexeme),
            try self.gc.copyString(qualified_protocol_name.items),
        );

        var resolved_type = ObjTypeDef.TypeUnion{ .Protocol = protocol_def };

        // Create type
        var protocol_type: ObjTypeDef = .{
            .def_type = .Protocol,
            .resolved_type = resolved_type,
        };

        self.beginScope();

        // Body
        try self.consume(.LeftBrace, "Expected `{` before protocol body.");

        var fields = std.StringHashMap(void).init(self.gc.allocator);
        defer fields.deinit();
        var methods = std.StringHashMap(*ParseNode).init(self.gc.allocator);
        var docblocks = std.StringHashMap(?Token).init(self.gc.allocator);
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            const docblock: ?Token = if (try self.match(.Docblock)) self.parser.previous_token.? else null;

            try self.consume(.Fun, "Expected method definition");

            var method_node: *ParseNode = try self.method(
                true,
                try protocol_placeholder.toInstance(self.gc.allocator, &self.gc.type_registry),
            );

            try self.consume(.Semicolon, "Expected `;` after method definition");

            var method_name: []const u8 = method_node.type_def.?.resolved_type.?.Function.name.string;

            if (fields.get(method_name) != null) {
                try self.reportError("A method with that name already exists.");
            }

            try protocol_type.resolved_type.?.Protocol.methods.put(
                method_name,
                method_node.type_def.?,
            );

            try fields.put(method_name, {});
            try methods.put(method_name, method_node);
            try docblocks.put(method_name, docblock);
        }

        try self.consume(.RightBrace, "Expected `}` after protocol body.");

        var node = try self.gc.allocator.create(ProtocolDeclarationNode);
        node.node.ends_scope = try self.endScope();

        _ = try self.declareVariable(
            &protocol_type, // Should resolve protocol_placeholder and be discarded
            protocol_name,
            true, // Protocol is always constant
        );

        assert(!protocol_type.optional);

        self.markInitialized();

        node.* = ProtocolDeclarationNode{};
        node.node.type_def = protocol_placeholder;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn expressionStatement(self: *Self, hanging: bool) !*ParseNode {
        const start_location = self.parser.previous_token.?;
        var node = try self.gc.allocator.create(ExpressionNode);
        node.* = ExpressionNode{
            .expression = try self.expression(hanging),
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        try self.consume(.Semicolon, "Expected `;` after expression.");

        return &node.node;
    }

    fn breakStatement(self: *Self, loop_scope: ?LoopScope) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        if (loop_scope == null) {
            try self.reportError("break is not allowed here.");
        }

        try self.consume(.Semicolon, "Expected `;` after `break`.");

        var node = try self.gc.allocator.create(BreakNode);
        node.* = .{};
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.ends_scope = if (loop_scope != null) try self.closeScope(loop_scope.?.loop_body_scope) else null;

        return &node.node;
    }

    fn continueStatement(self: *Self, loop_scope: ?LoopScope) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        if (loop_scope == null) {
            try self.reportError("continue is not allowed here.");
        }

        try self.consume(.Semicolon, "Expected `;` after `continue`.");

        var node = try self.gc.allocator.create(ContinueNode);
        node.* = .{};
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        // Continue only close body scope and not foreach/for loop variables
        node.node.ends_scope = try self.closeScope(loop_scope.?.loop_body_scope);

        return &node.node;
    }

    fn ifStatement(self: *Self, loop_scope: ?LoopScope) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftParen, "Expected `(` after `if`.");

        self.beginScope();
        var condition: *ParseNode = try self.expression(false);

        var unwrapped_identifier: ?Token = null;
        var casted_type: ?*ObjTypeDef = null;
        if (try self.match(.Arrow)) {
            _ = try self.parseVariable(
                try condition.type_def.?.cloneNonOptional(&self.gc.type_registry),
                true,
                "Expected optional unwrap identifier",
            );
            self.markInitialized();

            unwrapped_identifier = self.parser.previous_token.?;
        } else if (try self.match(.As)) {
            casted_type = try self.parseTypeDef(null);

            _ = try self.parseVariable(
                try casted_type.?.toInstance(self.gc.allocator, &self.gc.type_registry),
                true,
                "Expected casted identifier",
            );
            self.markInitialized();
        }

        try self.consume(.RightParen, "Expected `)` after `if` condition.");

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");
        var body = try self.block(loop_scope);
        body.ends_scope = try self.endScope();

        var else_branch: ?*ParseNode = null;
        if (try self.match(.Else)) {
            if (try self.match(.If)) {
                else_branch = try self.ifStatement(loop_scope);
            } else {
                try self.consume(.LeftBrace, "Expected `{` after `else`.");

                self.beginScope();
                else_branch = try self.block(loop_scope);
                else_branch.?.ends_scope = try self.endScope();
            }
        }

        var node = try self.gc.allocator.create(IfNode);
        node.* = IfNode{
            .condition = condition,
            .unwrapped_identifier = unwrapped_identifier,
            .casted_type = casted_type,
            .body = body,
            .else_branch = else_branch,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn forStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftParen, "Expected `(` after `for`.");

        self.beginScope();

        // Should be either VarDeclaration or expression
        var init_declarations = std.ArrayList(*VarDeclarationNode).init(self.gc.allocator);
        while (!self.check(.Semicolon) and !self.check(.Eof)) {
            try init_declarations.append(VarDeclarationNode.cast(try self.varDeclaration(try self.parseTypeDef(null), .Nothing, false, true)).?);

            self.markInitialized();

            if (!self.check(.Semicolon)) {
                try self.consume(.Comma, "Expected `,` after for loop variable");
            }
        }

        try self.consume(.Semicolon, "Expected `;` after for loop variables.");

        var condition = try self.expression(false);

        try self.consume(.Semicolon, "Expected `;` after for loop condition.");

        var post_loop = std.ArrayList(*ParseNode).init(self.gc.allocator);
        while (!self.check(.RightParen) and !self.check(.Eof)) {
            try post_loop.append(try self.expression(false));

            if (!self.check(.RightParen)) {
                try self.consume(.Comma, "Expected `,` after for loop expression");
            }
        }

        try self.consume(.RightParen, "Expected `)` after `for` expressions.");

        try self.consume(.LeftBrace, "Expected `{` after `for` definition.");

        self.beginScope();
        var body = try self.block(LoopScope{
            .loop_type = .For,
            .loop_body_scope = self.current.?.scope_depth,
        });
        body.ends_scope = try self.endScope();

        var node = try self.gc.allocator.create(ForNode);
        node.* = ForNode{
            .init_declarations = init_declarations,
            .condition = condition,
            .post_loop = post_loop,
            .body = body,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.ends_scope = try self.endScope();

        return &node.node;
    }

    fn forEachStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftParen, "Expected `(` after `foreach`.");

        self.beginScope();

        var key: ?*ParseNode = try self.varDeclaration(try self.parseTypeDef(null), .Nothing, false, false);

        var value: ?*ParseNode = if (try self.match(.Comma))
            try self.varDeclaration(try self.parseTypeDef(null), .Nothing, false, false)
        else
            null;

        try self.consume(.In, "Expected `in` after `foreach` variables.");

        var iterable = try self.expression(false);

        // Local not usable by user but needed so that locals are correct
        _ = try self.addLocal(
            Token.identifier("$iterable"),
            iterable.type_def.?,
            true,
        );

        self.markInitialized();

        try self.consume(.RightParen, "Expected `)` after `foreach`.");

        try self.consume(.LeftBrace, "Expected `{` after `foreach` definition.");

        self.beginScope();
        var body = try self.block(LoopScope{
            .loop_type = .ForEach,
            .loop_body_scope = self.current.?.scope_depth,
        });
        body.ends_scope = try self.endScope();

        // Only one variable: it's the value not the key
        if (value == null) {
            value = key;
            key = null;
        }

        var node = try self.gc.allocator.create(ForEachNode);
        node.* = ForEachNode{
            .key = if (key != null) VarDeclarationNode.cast(key.?).? else null,
            .value = VarDeclarationNode.cast(value.?).?,
            .iterable = iterable,
            .block = body,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.ends_scope = try self.endScope();

        return &node.node;
    }

    fn whileStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftParen, "Expected `(` after `while`.");

        var condition = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `while` condition.");

        try self.consume(.LeftBrace, "Expected `{` after `if` condition.");

        self.beginScope();
        var body = try self.block(LoopScope{
            .loop_type = .While,
            .loop_body_scope = self.current.?.scope_depth,
        });
        body.ends_scope = try self.endScope();

        var node = try self.gc.allocator.create(WhileNode);
        node.* = WhileNode{
            .condition = condition,
            .block = body,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn doUntilStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftBrace, "Expected `{` after `do`.");

        self.beginScope();
        var body = try self.block(LoopScope{
            .loop_type = .Do,
            .loop_body_scope = self.current.?.scope_depth,
        });
        body.ends_scope = try self.endScope();

        try self.consume(.Until, "Expected `until` after `do` block.");

        try self.consume(.LeftParen, "Expected `(` after `until`.");

        var condition = try self.expression(false);

        try self.consume(.RightParen, "Expected `)` after `until` condition.");

        var node = try self.gc.allocator.create(DoUntilNode);
        node.* = DoUntilNode{
            .condition = condition,
            .block = body,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn returnStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        if (self.current.?.scope_depth == 0) {
            try self.reportError("Can't use `return` at top-level.");
        }

        var value: ?*ParseNode = null;
        if (!try self.match(.Semicolon)) {
            value = try self.expression(false);

            try self.consume(.Semicolon, "Expected `;` after return value.");
        }

        var node = try self.gc.allocator.create(ReturnNode);
        node.* = ReturnNode{
            .value = value,
            .unconditional = self.current.?.scope_depth == 1,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn tryStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        if (self.current.?.in_try) {
            try self.reportError("Nested `try` statement are not allowed");
        }

        self.current.?.in_try = true;

        try self.consume(.LeftBrace, "Expected `{` after `try`");

        self.beginScope();
        var body = try self.block(null);
        body.ends_scope = try self.endScope();

        var clause_identifiers = std.ArrayList([]const u8).init(self.gc.allocator);
        var clauses = std.AutoArrayHashMap(*ObjTypeDef, *ParseNode).init(self.gc.allocator);
        var unconditional_clause: ?*ParseNode = null;
        while (try self.match(.Catch)) {
            if (try self.match(.LeftParen)) {
                if (unconditional_clause != null) {
                    try self.reportError("Catch clause not allowed after unconditional catch");
                }

                self.beginScope();

                const type_def = try self.parseTypeDef(null);

                _ = try self.parseVariable(
                    try type_def.toInstance(self.gc.allocator, &self.gc.type_registry),
                    true, // function arguments are constant
                    "Expected error identifier",
                );
                try clause_identifiers.append(self.parser.previous_token.?.lexeme);
                self.markInitialized();

                try self.consume(.RightParen, "Expected `)` after error identifier");
                try self.consume(.LeftBrace, "Expected `{`");

                var catch_block = try self.block(null);
                catch_block.ends_scope = try self.endScope();

                try clauses.put(type_def, catch_block);
            } else if (unconditional_clause == null) {
                try self.consume(.LeftBrace, "Expected `{` after `catch`");

                self.beginScope();
                unconditional_clause = try self.block(null);
                unconditional_clause.?.ends_scope = try self.endScope();
            } else {
                try self.reportError("Expected `(` after `catch`");
            }
        }

        self.current.?.in_try = false;

        var node = try self.gc.allocator.create(TryNode);
        node.* = TryNode{
            .body = body,
            .clauses = clauses,
            .clause_identifiers = clause_identifiers.items,
            .unconditional_clause = unconditional_clause,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn varDeclaration(self: *Self, parsed_type: *ObjTypeDef, terminator: DeclarationTerminator, constant: bool, can_assign: bool) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        var var_type = try parsed_type.toInstance(self.gc.allocator, &self.gc.type_registry);

        const slot: usize = try self.parseVariable(var_type, constant, "Expected variable name.");

        const name = self.parser.previous_token.?;

        const value = if (can_assign and try self.match(.Equal)) try self.expression(false) else null;

        if (var_type.def_type == .Placeholder and value != null and value.?.type_def != null and value.?.type_def.?.def_type == .Placeholder) {
            try PlaceholderDef.link(var_type, value.?.type_def.?, .Assignment);
        }

        var node = try self.gc.allocator.create(VarDeclarationNode);
        node.* = VarDeclarationNode{
            .name = name,
            .value = value,
            .type_def = var_type,
            .constant = constant,
            .slot = slot,
            .slot_type = if (self.current.?.scope_depth > 0) .Local else .Global,
            .expression = terminator == .Nothing,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = node.type_def;

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
                .Placeholder = PlaceholderDef.init(self.gc.allocator, user_type_name),
            };

            placeholder_resolved_type.Placeholder.name = try self.gc.copyString(
                user_type_name.lexeme,
            );

            var_type = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            _ = try self.declarePlaceholder(user_type_name, var_type);
        }

        if (try self.match(.Question)) {
            var_type = try var_type.?.cloneOptional(&self.gc.type_registry);
        }

        return try self.varDeclaration(var_type.?, .Semicolon, constant, true);
    }

    fn importStatement(self: *Self) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var imported_symbols = std.StringHashMap(void).init(self.gc.allocator);

        while ((try self.match(.Identifier)) and !self.check(.Eof)) {
            try imported_symbols.put(self.parser.previous_token.?.lexeme, {});

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

        var node = try self.gc.allocator.create(ImportNode);
        node.* = ImportNode{
            .imported_symbols = if (imported_symbols.count() > 0) imported_symbols else null,
            .prefix = prefix,
            .path = path,
            .import = import,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn exportStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

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

            var node = try self.gc.allocator.create(ExportNode);
            node.* = ExportNode{
                .identifier = identifier,
                .alias = alias,
            };
            node.node.location = self.parser.previous_token.?;

            return &node.node;
        }

        try self.reportError("Unknown global.");

        var node = try self.gc.allocator.create(ExportNode);
        node.* = ExportNode{
            .identifier = identifier,
            .alias = identifier,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn funDeclaration(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        var function_type: FunctionType = .Function;

        if (self.parser.previous_token.?.token_type == .Extern) {
            try self.consume(.Fun, "Expected `fun` after `extern`.");

            function_type = .Extern;
        }

        try self.consume(.Identifier, "Expected function name.");
        var name_token: Token = self.parser.previous_token.?;

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

        // Enforce main signature
        if (is_main) {
            const fun_def = function_node.type_def.?.resolved_type.?.Function;

            if (fun_def.parameters.count() != 1) {
                try self.reportError("`main` function signature must only have one `[str]` argument");
            } else {
                const first_param = fun_def.parameters.get(fun_def.parameters.keys()[0]);
                if (first_param == null or
                    !(try self.parseTypeDefFrom("[str]")).eql(first_param.?))
                {
                    try self.reportError("`main` function signature must only have one `[str]` argument");
                }
            }
        }

        var slot: usize = try self.declareVariable(function_node.type_def.?, name_token, true);

        self.markInitialized();

        var node = try self.gc.allocator.create(FunDeclarationNode);
        node.* = FunDeclarationNode{
            .slot = slot,
            .slot_type = if (self.current.?.scope_depth == 0) .Global else .Local,
            .function = FunctionNode.cast(function_node).?,
        };
        node.node.type_def = node.function.node.type_def;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn parseFiberType(self: *Self, generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) !*ObjTypeDef {
        try self.consume(.Less, "Expected `<` after `fib`");
        const return_type = try self.parseTypeDef(generic_types);
        try self.consume(.Comma, "Expected `,` after fiber return type");
        const yield_type = try self.parseTypeDef(generic_types);

        if (!yield_type.optional and yield_type.def_type != .Void) {
            try self.reportError("Expected optional type");
        }

        try self.consume(.Greater, "Expected `>` after fiber yield type");

        const fiber_def = ObjFiber.FiberDef{
            .return_type = return_type,
            .yield_type = yield_type,
        };

        const resolved_type = ObjTypeDef.TypeUnion{
            .Fiber = fiber_def,
        };

        return try self.gc.type_registry.getTypeDef(ObjTypeDef{
            .optional = try self.match(.Question),
            .def_type = .Fiber,
            .resolved_type = resolved_type,
        });
    }

    fn parseListType(self: *Self, generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) !*ObjTypeDef {
        var list_item_type: *ObjTypeDef = try (try self.parseTypeDef(generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);

        try self.consume(.RightBracket, "Expected `]` after list type.");

        var list_def = ObjList.ListDef.init(self.gc.allocator, list_item_type);
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .List = list_def,
        };

        return try self.gc.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = resolved_type,
            },
        );
    }

    fn listDeclaration(self: *Self, constant: bool) !*ParseNode {
        if (self.check(.Less) and self.current.?.scope_depth > 0) {
            // Its a list expression
            return try self.expressionStatement(true);
        }

        return try self.varDeclaration(
            try self.parseListType(null),
            .Semicolon,
            constant,
            true,
        );
    }

    fn parseMapType(self: *Self, generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) !*ObjTypeDef {
        var key_type: *ObjTypeDef = try (try self.parseTypeDef(generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);

        try self.consume(.Comma, "Expected `,` after key type.");

        var value_type: *ObjTypeDef = try (try self.parseTypeDef(generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);

        try self.consume(.RightBrace, "Expected `}` after value type.");

        var map_def = ObjMap.MapDef.init(self.gc.allocator, key_type, value_type);
        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{
            .Map = map_def,
        };

        return try self.gc.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .Map,
                .resolved_type = resolved_type,
            },
        );
    }

    fn mapDeclaration(self: *Self, constant: bool) !*ParseNode {
        if (self.check(.Less) and self.current.?.scope_depth > 0) {
            // Its a map expression
            return try self.expressionStatement(true);
        }

        return try self.varDeclaration(
            try self.parseMapType(null),
            .Semicolon,
            constant,
            true,
        );
    }

    fn enumDeclaration(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        if (self.current.?.scope_depth > 0) {
            try self.reportError("Enum must be defined at top-level.");
        }

        var enum_case_type: *ObjTypeDef = undefined;
        var case_type_picked: bool = false;
        if (try self.match(.LeftParen)) {
            enum_case_type = try self.parseTypeDef(null);
            try self.consume(.RightParen, "Expected `)` after enum type.");

            case_type_picked = true;
        } else {
            enum_case_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Integer });
        }

        enum_case_type = try enum_case_type.toInstance(self.gc.allocator, &self.gc.type_registry);

        try self.consume(.Identifier, "Expected enum name.");
        var enum_name: Token = self.parser.previous_token.?.clone();

        // Qualified name to avoid cross script collision
        const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
        defer self.gc.allocator.free(qualifier);
        var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
        defer qualified_name.deinit();
        try qualified_name.writer().print("{s}.{s}", .{ qualifier, enum_name.lexeme });

        var enum_def: ObjEnum.EnumDef = ObjEnum.EnumDef.init(
            self.gc.allocator,
            try self.gc.copyString(enum_name.lexeme),
            try self.gc.copyString(qualified_name.items),
            enum_case_type,
        );

        var enum_resolved: ObjTypeDef.TypeUnion = .{ .Enum = enum_def };

        var enum_type: *ObjTypeDef = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Enum,
                .resolved_type = enum_resolved,
            },
        );

        const slot: usize = try self.declareVariable(enum_type, enum_name, true);
        self.markInitialized();

        try self.consume(.LeftBrace, "Expected `{` before enum body.");

        var cases = std.ArrayList(*ParseNode).init(self.gc.allocator);
        var picked = std.ArrayList(bool).init(self.gc.allocator);
        var case_index: i32 = 0;
        while (!self.check(.RightBrace) and !self.check(.Eof)) : (case_index += 1) {
            if (case_index > 255) {
                try self.reportError("Too many enum cases.");
            }

            try self.consume(.Identifier, "Expected case name.");
            const case_name: []const u8 = self.parser.previous_token.?.lexeme;

            if (case_type_picked and (enum_case_type.def_type != .String or self.check(.Equal))) {
                try self.consume(.Equal, "Expected `=` after case name.");

                try cases.append(try self.expression(false));
                try picked.append(true);
            } else {
                if (enum_case_type.def_type == .Integer) {
                    var constant_node = try self.gc.allocator.create(IntegerNode);
                    constant_node.* = IntegerNode{
                        .integer_constant = case_index,
                    };
                    constant_node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                        .def_type = .Integer,
                    });
                    constant_node.node.location = self.parser.previous_token.?;

                    try cases.append(&constant_node.node);
                } else {
                    var constant_node = try self.gc.allocator.create(StringLiteralNode);
                    constant_node.* = StringLiteralNode{
                        .constant = try self.gc.copyString(case_name),
                    };
                    constant_node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                        .def_type = .String,
                    });
                    constant_node.node.location = self.parser.previous_token.?;

                    try cases.append(&constant_node.node);
                }

                try picked.append(false);
            }

            try enum_type.resolved_type.?.Enum.cases.append(case_name);

            // TODO: how to not force a comma at last case?
            try self.consume(.Comma, "Expected `,` after case definition.");
        }

        try self.consume(.RightBrace, "Expected `}` after enum body.");

        if (case_index == 0) {
            try self.reportError("Enum must have at least one case.");
        }

        var node = try self.gc.allocator.create(EnumNode);
        node.* = EnumNode{
            .slot = slot,
            .cases = cases,
            .picked = picked,
            .case_type_picked = case_type_picked,
        };
        node.node.type_def = enum_type;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn anonymousObjectInit(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;
        try self.consume(.LeftBrace, "Expected `{` after `.`");

        var node = try self.gc.allocator.create(ObjectInitNode);
        node.* = ObjectInitNode.init(self.gc.allocator, null);
        node.node.location = start_location;

        const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
        defer self.gc.allocator.free(qualifier);
        var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
        defer qualified_name.deinit();
        try qualified_name.writer().print("{s}.anonymous", .{qualifier});

        var object_def = ObjObject.ObjectDef.init(
            self.gc.allocator,
            try self.gc.copyString("anonymous"),
            try self.gc.copyString(qualified_name.items),
            true,
        );

        var resolved_type = ObjTypeDef.TypeUnion{ .Object = object_def };

        // We build the object type has we parse its instanciation
        var object_type: ObjTypeDef = .{
            .def_type = .Object,
            .resolved_type = resolved_type,
        };

        // Anonymous object can only have properties without default values (no methods, no static fields)
        // They can't self reference since their anonymous
        var fields = std.StringHashMap(void).init(self.gc.allocator);
        defer fields.deinit();

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.consume(.Identifier, "Expected property name");

            const property_name: []const u8 = self.parser.previous_token.?.lexeme;

            if (fields.get(property_name) != null) {
                try self.reportError("A property with that name already exists.");
            }

            try self.consume(.Equal, "Expected `=` after property name.");

            const expr = try self.expression(false);

            try fields.put(property_name, {});
            try object_type.resolved_type.?.Object.fields.put(property_name, expr.type_def.?);
            try node.properties.put(property_name, expr);

            if (!self.check(.RightBrace) or self.check(.Comma)) {
                try self.consume(.Comma, "Expected `,` after field initialization.");
            }
        }

        try self.consume(.RightBrace, "Expected `}` after object initialization.");

        node.node.type_def = try (try self.gc.type_registry.getTypeDef(object_type)).toInstance(self.gc.allocator, &self.gc.type_registry);
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn objectInit(self: *Self, _: bool, object: *ParseNode) anyerror!*ParseNode {
        var node = try self.gc.allocator.create(ObjectInitNode);
        node.* = ObjectInitNode.init(self.gc.allocator, object);
        node.node.location = object.location;

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.consume(.Identifier, "Expected property name");

            const property_name: []const u8 = self.parser.previous_token.?.lexeme;

            var property_placeholder: ?*ObjTypeDef = null;

            // Object is placeholder, create placeholder for the property and link it
            if (object.type_def != null and object.type_def.?.def_type == .Placeholder) {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
                };
                placeholder_resolved_type.Placeholder.name = try self.gc.copyString(property_name);

                property_placeholder = try self.gc.type_registry.getTypeDef(
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

        node.node.type_def = if (object.type_def) |type_def| try type_def.toInstance(self.gc.allocator, &self.gc.type_registry) else null;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    pub fn expression(self: *Self, hanging: bool) !*ParseNode {
        return try self.parsePrecedence(.Assignment, hanging);
    }

    // Returns a list of break jumps to patch
    fn block(self: *Self, loop_scope: ?LoopScope) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(BlockNode);
        node.* = BlockNode.init(self.gc.allocator);

        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            if (try self.declarationOrStatement(loop_scope)) |declOrStmt| {
                try node.statements.append(declOrStmt);
            }
        }

        try self.consume(.RightBrace, "Expected `}}` after block.");

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    inline fn getRule(token: TokenType) ParseRule {
        return rules[@enumToInt(token)];
    }

    fn parsePrecedence(self: *Self, precedence: Precedence, hanging: bool) !*ParseNode {
        // In case we are already parsing an expression, the current unwrap chain should not impact deeper expressions
        // Exemple: canBeNull?.aMap[expression] <- here `expression` should not be transformed into an optional
        const previous_opt_jumps = self.opt_jumps;
        self.opt_jumps = null;

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
                        node.type_def = try node.type_def.?.cloneOptional(&self.gc.type_registry);
                    }
                }
            }

            _ = try self.advance();

            var infixRule: InfixParseFn = getRule(self.parser.previous_token.?.token_type).infix.?;
            node = try infixRule(self, canAssign, node);
        }

        if (self.opt_jumps) |jumps| {
            jumps.deinit();
            self.opt_jumps = null;

            node.patch_opt_jumps = true;

            if (node.type_def != null) {
                node.type_def = try node.type_def.?.cloneOptional(&self.gc.type_registry);
            }
        }

        if (canAssign and (try self.match(.Equal))) {
            try self.reportError("Invalid assignment target.");
        }

        self.opt_jumps = previous_opt_jumps;

        return node;
    }

    fn namedVariable(self: *Self, name: Token, can_assign: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var var_def: ?*ObjTypeDef = null;
        var slot: usize = undefined;
        var slot_type: SlotType = undefined;
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
        } else if (try self.resolveGlobal(null, name)) |uslot| {
            var_def = self.globals.items[uslot].type_def;
            slot = uslot;
            slot_type = .Global;
            slot_constant = self.globals.items[uslot].constant;
        } else {
            slot = try self.declarePlaceholder(name, null);
            var_def = self.globals.items[slot].type_def;
            slot_type = .Global;
        }

        var value = if (can_assign and try self.match(.Equal)) try self.expression(false) else null;

        var node = try self.gc.allocator.create(NamedVariableNode);
        node.* = NamedVariableNode{
            .identifier = start_location,
            .value = value,
            .slot = slot,
            .slot_type = slot_type,
            .slot_constant = slot_constant,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = var_def;

        return &node.node;
    }

    fn yield(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(YieldNode);
        node.* = YieldNode{
            .expression = try self.parsePrecedence(.Primary, false),
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = node.expression.type_def;

        return &node.node;
    }

    fn resolveFiber(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(ResolveNode);
        node.* = ResolveNode{
            .fiber = try self.parsePrecedence(.Primary, false),
        };

        const fiber_type = node.fiber.type_def;

        if (fiber_type == null) {
            unreachable;
        } else if (fiber_type.?.def_type == .Placeholder) {
            const return_placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
            };
            const return_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = return_placeholder_resolved_type,
                },
            );

            try PlaceholderDef.link(fiber_type.?, return_placeholder, .Yield);

            node.node.type_def = return_placeholder;
        } else {
            if (fiber_type.?.def_type != .Fiber) {
                try self.reportErrorAt(node.fiber.location, "Can't be resolveed");
            } else {
                const fiber = fiber_type.?.resolved_type.?.Fiber;

                node.node.type_def = fiber.return_type;
            }
        }

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn resumeFiber(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(ResumeNode);
        node.* = ResumeNode{
            .fiber = try self.parsePrecedence(.Primary, false),
        };

        const fiber_type = node.fiber.type_def;

        if (fiber_type == null) {
            unreachable;
        } else if (fiber_type.?.def_type == .Placeholder) {
            const yield_placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
            };
            var yield_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = yield_placeholder_resolved_type,
                },
            );

            yield_placeholder = try yield_placeholder.cloneOptional(&self.gc.type_registry);

            try PlaceholderDef.link(fiber_type.?, yield_placeholder, .Yield);

            node.node.type_def = yield_placeholder;
        } else {
            if (fiber_type.?.def_type != .Fiber) {
                try self.reportErrorAt(node.fiber.location, "Can't be resumed");
            } else {
                const fiber = fiber_type.?.resolved_type.?.Fiber;

                // Resume returns null if nothing was yielded and/or fiber reached its return statement
                assert(fiber.yield_type.optional);
                node.node.type_def = fiber.yield_type;
            }
        }

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn asyncCall(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        const callable = try self.parsePrecedence(.Call, false);

        var node = try self.gc.allocator.create(AsyncCallNode);
        node.* = AsyncCallNode{
            .call = callable,
        };

        // Expression after `&` must either be a call or a dot call
        if (callable.node_type != .Call and (callable.node_type != .Dot or DotNode.cast(callable).?.call == null)) {
            try self.reportErrorAt(callable.location, "Expected function call after `async`");

            return &node.node;
        }

        var call_node = switch (callable.node_type) {
            .Call => CallNode.cast(callable).?,
            .Dot => DotNode.cast(callable).?.call.?,
            else => unreachable,
        };
        call_node.async_call = true;
        const function_type = call_node.callable_type;

        if (function_type == null) {
            unreachable;
        } else if (function_type.?.def_type == .Placeholder) {
            // create placeholders for return and yield types and link them with .Call and .Yield
            const return_placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
            };
            const return_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = return_placeholder_resolved_type,
                },
            );

            try PlaceholderDef.link(function_type.?, return_placeholder, .Call);

            const yield_placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
            };
            var yield_placeholder = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = yield_placeholder_resolved_type,
                },
            );

            try PlaceholderDef.link(function_type.?, yield_placeholder, .Yield);

            const fiber_def = ObjFiber.FiberDef{
                .return_type = return_placeholder,
                .yield_type = yield_placeholder,
            };

            const resolved_type = ObjTypeDef.TypeUnion{
                .Fiber = fiber_def,
            };

            node.node.type_def = try self.gc.type_registry.getTypeDef(ObjTypeDef{
                .optional = try self.match(.Question),
                .def_type = .Fiber,
                .resolved_type = resolved_type,
            });
        } else {
            if (function_type.?.def_type != .Function) {
                try self.reportErrorAt(call_node.callee.location, "Can't be called");
            } else {
                const return_type = function_type.?.resolved_type.?.Function.return_type;
                const yield_type = function_type.?.resolved_type.?.Function.yield_type;

                const fiber_def = ObjFiber.FiberDef{
                    .return_type = return_type,
                    .yield_type = yield_type,
                };

                const resolved_type = ObjTypeDef.TypeUnion{
                    .Fiber = fiber_def,
                };

                node.node.type_def = try (try self.gc.type_registry.getTypeDef(ObjTypeDef{
                    .optional = try self.match(.Question),
                    .def_type = .Fiber,
                    .resolved_type = resolved_type,
                })).populateGenerics(
                    function_type.?.resolved_type.?.Function.id,
                    call_node.resolved_generics,
                    &self.gc.type_registry,
                    null,
                );
            }
        }

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn pattern(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(PatternNode);

        const source_slice = self.parser.previous_token.?.literal_string.?;
        // Replace escaped pattern delimiter with delimiter
        const source_slice_clean = try std.mem.replaceOwned(u8, self.gc.allocator, source_slice, "__", "_");
        const source = try self.gc.allocator.dupeZ(u8, source_slice_clean);

        var err = try self.gc.allocator.allocSentinel(u8, 1000, 0);
        // FIXME: crashes i don't know why
        // defer self.gc.allocator.free(err);
        var err_offset: c_int = undefined;
        const reg: ?*_obj.pcre_struct = pcre.pcre_compile(
            @ptrCast([*c]const u8, source), // pattern
            0, // options
            @ptrCast([*c][*c]const u8, &err), // error message buffer
            &err_offset, // offset at which error occured
            null, // extra ?
        );

        if (reg == null) {
            try self.reportErrorFmt("Could not compile pattern, error at {}: {s}", .{ err_offset, err });
            return CompileError.Unrecoverable;
        }

        var constant = try self.gc.allocateObject(
            ObjPattern,
            .{
                .source = source_slice_clean,
                .pattern = reg.?,
            },
        );

        node.* = PatternNode{
            .constant = constant,
        };
        node.node.type_def = try self.gc.type_registry.getTypeDef(.{
            .def_type = .Pattern,
        });
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn integer(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(IntegerNode);

        node.* = IntegerNode{
            .integer_constant = self.parser.previous_token.?.literal_integer.?,
        };
        node.node.type_def = try self.gc.type_registry.getTypeDef(.{
            .def_type = .Integer,
        });
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn float(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(FloatNode);

        node.* = FloatNode{
            .float_constant = self.parser.previous_token.?.literal_float.?,
        };
        node.node.type_def = try self.gc.type_registry.getTypeDef(.{
            .def_type = .Float,
        });
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn string(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        const string_token = self.parser.previous_token.?;
        var string_parser = StringParser.init(
            self,
            string_token.literal_string.?,
            self.script_name,
            string_token.line,
            string_token.column,
        );
        const string_node = try string_parser.parse();
        string_node.node.location = start_location;
        string_node.node.end_location = self.parser.previous_token.?;

        return &string_node.node;
    }

    fn grouping(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;
        var node = try self.gc.allocator.create(GroupingNode);
        node.* = GroupingNode{
            .expression = try self.expression(false),
        };
        node.node.type_def = node.expression.type_def;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        try self.consume(.RightParen, "Expected ')' after expression.");

        return &node.node;
    }

    fn literal(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        switch (self.parser.previous_token.?.token_type) {
            .False => {
                var node = try self.gc.allocator.create(BooleanNode);

                node.* = BooleanNode{ .constant = false };

                node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                    .def_type = .Bool,
                });

                node.node.location = start_location;
                node.node.end_location = self.parser.previous_token.?;

                return &node.node;
            },
            .True => {
                var node = try self.gc.allocator.create(BooleanNode);

                node.* = BooleanNode{ .constant = true };

                node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                    .def_type = .Bool,
                });

                node.node.location = start_location;
                node.node.end_location = self.parser.previous_token.?;

                return &node.node;
            },
            .Null => {
                var node = try self.gc.allocator.create(NullNode);

                node.* = NullNode{};

                node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                    .def_type = .Void,
                });

                node.node.location = start_location;
                node.node.end_location = self.parser.previous_token.?;

                return &node.node;
            },
            .Void => {
                var node = try self.gc.allocator.create(VoidNode);

                node.* = VoidNode{};

                node.node.type_def = try self.gc.type_registry.getTypeDef(.{
                    .def_type = .Void,
                });

                node.node.location = start_location;
                node.node.end_location = self.parser.previous_token.?;

                return &node.node;
            },
            else => unreachable,
        }
    }

    fn unary(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var operator: TokenType = self.parser.previous_token.?.token_type;

        var left: *ParseNode = try self.parsePrecedence(.Unary, false);

        var node = try self.gc.allocator.create(UnaryNode);
        node.* = UnaryNode{
            .left = left,
            .operator = operator,
        };
        node.node.type_def = left.type_def;
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn inlineIf(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        try self.consume(.LeftParen, "Expected `(` after `if`.");
        const condition = try self.expression(false);
        try self.consume(.RightParen, "Expected `)` after `if` condition.");

        const body = try self.expression(false);

        try self.consume(.Else, "Expected `else` after inline `if` body.");

        const else_branch = try self.expression(false);

        var node = try self.gc.allocator.create(InlineIfNode);
        node.* = InlineIfNode{
            .condition = condition,
            .body = body,
            .else_branch = else_branch,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = body.type_def orelse else_branch.type_def;

        return &node.node;
    }

    // FIXME: doesn't need its own function
    fn argumentList(self: *Self, trailing_comma: *bool) !std.AutoArrayHashMap(*ObjString, *ParseNode) {
        var arguments = std.AutoArrayHashMap(*ObjString, *ParseNode).init(self.gc.allocator);

        var arg_count: u8 = 0;
        while (!(try self.match(.RightParen)) and !(try self.match(.Eof))) {
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

            try arguments.put(
                try self.gc.copyString(if (!hanging and arg_name != null) arg_name.?.lexeme else "$"),
                try self.expression(hanging),
            );

            if (arg_count == 255) {
                try self.reportError("Can't have more than 255 arguments.");

                return arguments;
            }

            arg_count += 1;

            if (!self.check(.RightParen)) {
                trailing_comma.* = true;
                try self.consume(.Comma, "Expected `,` after call argument");
            } else {
                trailing_comma.* = false;
            }
        }

        return arguments;
    }

    fn call(self: *Self, _: bool, callee: *ParseNode) anyerror!*ParseNode {
        const function_type = if (callee.type_def != null and callee.type_def.?.def_type == .Function)
            callee.type_def.?
        else
            null;

        var trailing_comma = false;
        var resolved_generics = std.ArrayList(*ObjTypeDef).init(self.gc.allocator);
        if (function_type != null and try self.match(.Less)) {
            while (!self.check(.Greater) and !self.check(.Eof)) {
                try resolved_generics.append(try self.parseTypeDef(null));

                if (!self.check(.Greater)) {
                    try self.consume(.Comma, "Expected `,` between generic types");
                }
            }

            try self.consume(.Greater, "Expected `>` after generic types list");
            if (!self.check(.RightParen)) {
                trailing_comma = true;
                try self.consume(.Comma, "Expected `,` after generic types list");
            } else {
                trailing_comma = false;
            }
        }

        var node = try self.gc.allocator.create(CallNode);
        node.* = CallNode{
            .callee = callee,
            // In the case of a dot call, callee's type will change to the function return type
            // so we keep a reference to it here
            .callable_type = callee.type_def,
            .resolved_generics = resolved_generics.items,
            .arguments = try self.argumentList(&trailing_comma),
            .catch_default = try self.inlineCatch(),
            .trailing_comma = trailing_comma,
        };

        // Node type is Function or Native return type or nothing/placeholder
        node.node.type_def = if (callee.type_def != null and callee.type_def.?.def_type == .Function)
            callee.type_def.?.resolved_type.?.Function.return_type
        else if (callee.type_def != null and callee.type_def.?.def_type == .Enum)
            try (try callee.type_def.?.toInstance(self.gc.allocator, &self.gc.type_registry)).cloneOptional(&self.gc.type_registry)
        else
            null;

        // If null, create placeholder
        if (node.node.type_def == null) {
            if (callee.type_def == null or callee.type_def.?.def_type != .Placeholder) {
                try self.reportErrorAt(callee.location, "Can't be called");
            } else {
                var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                    .Placeholder = PlaceholderDef.init(self.gc.allocator, node.node.location),
                };
                placeholder_resolved_type.Placeholder.call_generics = resolved_generics.items;

                node.node.type_def = try self.gc.type_registry.getTypeDef(
                    .{
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    },
                );

                try PlaceholderDef.link(callee.type_def.?, node.node.type_def.?, .Call);
            }
        } else if (resolved_generics.items.len > 0) {
            // Check if return type was composed with a generic type
            node.node.type_def = try node.node.type_def.?.populateGenerics(
                function_type.?.resolved_type.?.Function.id,
                resolved_generics.items,
                &self.gc.type_registry,
                null,
            );
        }

        node.node.location = callee.location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn unwrap(self: *Self, _: bool, unwrapped: *ParseNode) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(UnwrapNode);
        node.* = UnwrapNode{
            .unwrapped = unwrapped,
            .original_type = unwrapped.type_def,
        };

        node.node.type_def = if (unwrapped.type_def) |type_def| try type_def.cloneNonOptional(&self.gc.type_registry) else null;

        if (self.opt_jumps == null) {
            self.opt_jumps = std.ArrayList(Precedence).init(self.gc.allocator);
        }
        try self.opt_jumps.?.append(getRule(self.parser.current_token.?.token_type).precedence);

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn forceUnwrap(self: *Self, _: bool, unwrapped: *ParseNode) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var node = try self.gc.allocator.create(ForceUnwrapNode);
        node.* = ForceUnwrapNode{
            .unwrapped = unwrapped,
            .original_type = unwrapped.type_def,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        node.node.type_def = if (unwrapped.type_def) |type_def| try type_def.cloneNonOptional(&self.gc.type_registry) else null;

        return &node.node;
    }

    fn variable(self: *Self, can_assign: bool) anyerror!*ParseNode {
        return try self.namedVariable(self.parser.previous_token.?, can_assign);
    }

    fn dot(self: *Self, can_assign: bool, callee: *ParseNode) anyerror!*ParseNode {
        const start_location = callee.location;

        try self.consume(.Identifier, "Expected property name after `.`");
        var member_name_token: Token = self.parser.previous_token.?;
        var member_name: []const u8 = member_name_token.lexeme;

        var node = try self.gc.allocator.create(DotNode);
        node.* = DotNode{
            .callee = callee,
            .identifier = self.parser.previous_token.?,
        };
        // Check that name is a property
        const callee_def_type = if (callee.type_def) |type_def| type_def.def_type else .Placeholder;
        switch (callee_def_type) {
            .String => {
                if (try ObjString.memberDef(self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

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
            .Pattern => {
                if (try ObjPattern.memberDef(self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

                        node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                        // Node type is the return type of the call
                        node.node.type_def = node.call.?.node.type_def;
                    } else {
                        // Pattern has only native functions members
                        node.node.type_def = member;
                    }
                } else {
                    try self.reportError("Pattern property doesn't exist.");
                }
            },
            .Fiber => {
                if (try ObjFiber.memberDef(self, member_name)) |member| {
                    if (try self.match(.LeftParen)) {
                        // `call` will look to the parent node for the function definition
                        node.node.type_def = member;
                        node.member_type_def = member;

                        node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                        // Node type is the return type of the call
                        node.node.type_def = node.call.?.node.type_def;
                    } else {
                        // Fiber has only native functions members
                        node.node.type_def = member;
                    }
                } else {
                    try self.reportError("Fiber property doesn't exist.");
                }
            },
            .Object => {
                var obj_def: ObjObject.ObjectDef = callee.type_def.?.resolved_type.?.Object;

                var property_type: ?*ObjTypeDef = obj_def.static_fields.get(member_name) orelse obj_def.static_placeholders.get(member_name);

                // Not found, create a placeholder, this is a root placeholder not linked to anything
                // TODO: test with something else than a name
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.gc.allocator, member_name_token),
                    };
                    placeholder_resolved_type.Placeholder.name = try self.gc.copyString(member_name_token.lexeme);

                    var placeholder: *ObjTypeDef = try self.gc.type_registry.getTypeDef(.{
                        .optional = false,
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    });

                    if (BuildOptions.debug_placeholders) {
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
                var property_type: ?*ObjTypeDef = obj_def.methods.get(member_name);

                // Is it a property
                property_type = property_type orelse obj_def.fields.get(member_name) orelse obj_def.placeholders.get(member_name);

                // Else create placeholder
                if (property_type == null and self.current_object != null and std.mem.eql(u8, self.current_object.?.name.lexeme, obj_def.name.string)) {
                    var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                        .Placeholder = PlaceholderDef.init(self.gc.allocator, member_name_token),
                    };
                    placeholder_resolved_type.Placeholder.name = try self.gc.copyString(member_name);

                    var placeholder: *ObjTypeDef = try self.gc.type_registry.getTypeDef(.{
                        .optional = false,
                        .def_type = .Placeholder,
                        .resolved_type = placeholder_resolved_type,
                    });

                    if (BuildOptions.debug_placeholders) {
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
                    try self.reportErrorFmt("Property `{s}` does not exists in object `{s}`", .{ member_name, obj_def.name.string });
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
            .ProtocolInstance => {
                var protocol: *ObjTypeDef = callee.type_def.?.resolved_type.?.ProtocolInstance;
                var protocol_def: ObjObject.ProtocolDef = protocol.resolved_type.?.Protocol;

                var method_type: ?*ObjTypeDef = protocol_def.methods.get(member_name);

                // Else create placeholder
                if (method_type == null) {
                    try self.reportErrorFmt("Method `{s}` does not exists in protocol `{s}`", .{ member_name, protocol_def.name.string });
                }

                // Only call is allowed
                if (try self.match(.LeftParen)) {
                    // `call` will look to the parent node for the function definition
                    node.node.type_def = method_type;
                    node.member_type_def = method_type;

                    node.call = CallNode.cast(try self.call(can_assign, &node.node)).?;

                    // Node type is the return type of the call
                    node.node.type_def = node.call.?.node.type_def;
                } else {
                    node.node.type_def = method_type;
                }
            },
            .Enum => {
                var enum_def: ObjEnum.EnumDef = callee.type_def.?.resolved_type.?.Enum;

                for (enum_def.cases.items, 0..) |case, index| {
                    if (mem.eql(u8, case, member_name)) {
                        var enum_instance_resolved_type: ObjTypeDef.TypeUnion = .{
                            .EnumInstance = callee.type_def.?,
                        };

                        var enum_instance: *ObjTypeDef = try self.gc.type_registry.getTypeDef(.{
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
                    .Placeholder = PlaceholderDef.init(self.gc.allocator, member_name_token),
                };

                placeholder_resolved_type.Placeholder.name = try self.gc.copyString(member_name);

                var placeholder = try self.gc.type_registry.getTypeDef(
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
            else => {
                try self.reportErrorAt(node.node.location, "Not field accessible");
            },
        }

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn and_(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const start_location = left.location;

        var right: *ParseNode = try self.parsePrecedence(.And, false);

        var node = try self.gc.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = .And,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn or_(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const start_location = left.location;

        var right: *ParseNode = try self.parsePrecedence(.And, false);

        var node = try self.gc.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = .Or,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn is(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const start_location = left.location;

        const constant = (try self.parseTypeDef(null)).toValue();

        var node = try self.gc.allocator.create(IsNode);
        node.* = IsNode{
            .left = left,
            .constant = constant,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .Bool,
            },
        );

        return &node.node;
    }

    fn binary(self: *Self, _: bool, left: *ParseNode) anyerror!*ParseNode {
        const start_location = left.location;

        const operator: TokenType = self.parser.previous_token.?.token_type;
        const rule: ParseRule = getRule(operator);

        const right: *ParseNode = try self.parsePrecedence(
            @intToEnum(Precedence, @enumToInt(rule.precedence) + 1),
            false,
        );

        var node = try self.gc.allocator.create(BinaryNode);
        node.* = BinaryNode{
            .left = left,
            .right = right,
            .operator = operator,
        };

        node.node.type_def = switch (operator) {
            .QuestionQuestion => if (right.type_def orelse left.type_def) |type_def| try type_def.cloneNonOptional(&self.gc.type_registry) else null,

            .Greater,
            .Less,
            .GreaterEqual,
            .LessEqual,
            .BangEqual,
            .EqualEqual,
            => try self.gc.type_registry.getTypeDef(.{ .def_type = .Bool }),

            .Plus => left.type_def orelse right.type_def,

            .ShiftLeft,
            .ShiftRight,
            .Ampersand,
            .Bor,
            .Xor,
            => try self.gc.type_registry.getTypeDef(.{ .def_type = .Integer }),

            .Minus,
            .Star,
            .Slash,
            .Percent,
            => try self.gc.type_registry.getTypeDef(
                ObjTypeDef{
                    .def_type = if ((left.type_def != null and left.type_def.?.def_type == .Float) or (right.type_def != null and right.type_def.?.def_type == .Float))
                        .Float
                    else
                        .Integer,
                },
            ),

            else => unreachable,
        };

        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return &node.node;
    }

    fn subscript(self: *Self, can_assign: bool, subscripted: *ParseNode) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        const index: *ParseNode = try self.expression(false);

        if (subscripted.type_def.?.def_type == .Placeholder and index.type_def.?.def_type == .Placeholder) {
            try PlaceholderDef.link(subscripted.type_def.?, index.type_def.?, .Key);
        }

        var subscripted_type_def: ?*ObjTypeDef = null;

        if (subscripted.type_def) |type_def| {
            if (!type_def.optional) {
                switch (type_def.def_type) {
                    .Placeholder => {
                        var placeholder_resolved_type: ObjTypeDef.TypeUnion = .{
                            .Placeholder = PlaceholderDef.init(self.gc.allocator, self.parser.previous_token.?),
                        };

                        var placeholder = try self.gc.type_registry.getTypeDef(
                            .{
                                .def_type = .Placeholder,
                                .resolved_type = placeholder_resolved_type,
                            },
                        );

                        try PlaceholderDef.link(type_def, placeholder, .Subscript);

                        subscripted_type_def = placeholder;
                    },
                    .String => subscripted_type_def = type_def,
                    .List => subscripted_type_def = type_def.resolved_type.?.List.item_type,
                    .Map => subscripted_type_def = try type_def.resolved_type.?.Map.value_type.cloneOptional(&self.gc.type_registry),
                    else => try self.reportErrorFmt("Type `{s}` is not subscriptable", .{(try type_def.toStringAlloc(self.gc.allocator)).items}),
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

        var node = try self.gc.allocator.create(SubscriptNode);
        node.* = SubscriptNode{
            .subscripted = subscripted,
            .index = index,
            .value = value,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = subscripted_type_def;

        return &node.node;
    }

    fn list(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var trailing_comma: bool = false;
        var items = std.ArrayList(*ParseNode).init(self.gc.allocator);
        var item_type: ?*ObjTypeDef = null;

        // A list expression can specify its type `[<int>, ...]`
        if (try self.match(.Less)) {
            item_type = try (try self.parseTypeDef(null)).toInstance(self.gc.allocator, &self.gc.type_registry);

            try self.consume(.Greater, "Expected `>` after list type.");
        }

        if (item_type == null or try self.match(.Comma)) {
            var common_type: ?*ObjTypeDef = null;
            while (!(try self.match(.RightBracket)) and !(try self.match(.Eof))) {
                var actual_item: *ParseNode = try self.expression(false);

                try items.append(actual_item);

                if (item_type == null) {
                    if (common_type == null) {
                        common_type = actual_item.type_def;
                    } else if (actual_item.type_def) |actual_type_def| {
                        if (!common_type.?.eql(actual_type_def) and common_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_type = common_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object) orelse common_type;
                            common_type = try common_type.?.toInstance(self.gc.allocator, &self.gc.type_registry);
                        }
                    }
                }

                if (!self.check(.RightBracket)) {
                    trailing_comma = true;
                    try self.consume(.Comma, "Expected `,` after list item.");
                } else {
                    trailing_comma = false;
                }
            }

            item_type = item_type orelse common_type;
        } else {
            try self.consume(.RightBracket, "Expected `}`");
        }

        // Either item type was specified with `<type>` or the list is not empty and we could infer it
        if (item_type == null) {
            try self.reportError("List item type can't be infered");

            item_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
        }

        var list_def = ObjList.ListDef.init(self.gc.allocator, item_type.?);

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .List = list_def };

        var list_type: *ObjTypeDef = try self.gc.type_registry.getTypeDef(
            .{
                .def_type = .List,
                .resolved_type = resolved_type,
            },
        );

        var node = try self.gc.allocator.create(ListNode);
        node.* = ListNode{
            .items = items.items,
            .trailing_comma = trailing_comma,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = list_type;

        return &node.node;
    }

    fn map(self: *Self, _: bool) anyerror!*ParseNode {
        const start_location = self.parser.previous_token.?;

        var value_type: ?*ObjTypeDef = null;
        var key_type: ?*ObjTypeDef = null;
        var trailing_comma = false;

        // A map expression can specify its type `{<str, str>, ...}`
        if (try self.match(.Less)) {
            key_type = try (try self.parseTypeDef(null)).toInstance(self.gc.allocator, &self.gc.type_registry);

            try self.consume(.Comma, "Expected `,` after key type");

            value_type = try (try self.parseTypeDef(null)).toInstance(self.gc.allocator, &self.gc.type_registry);

            try self.consume(.Greater, "Expected `>` after map type.");
        }

        var keys = std.ArrayList(*ParseNode).init(self.gc.allocator);
        var values = std.ArrayList(*ParseNode).init(self.gc.allocator);

        if (key_type == null or try self.match(.Comma)) {
            var common_key_type: ?*ObjTypeDef = null;
            var common_value_type: ?*ObjTypeDef = null;
            while (!(try self.match(.RightBrace)) and !(try self.match(.Eof))) {
                var key: *ParseNode = try self.expression(false);
                try self.consume(.Colon, "Expected `:` after key.");
                var value: *ParseNode = try self.expression(false);

                try keys.append(key);
                try values.append(value);

                if (key_type == null) {
                    if (common_key_type == null) {
                        common_key_type = key.type_def;
                    } else if (key.type_def) |actual_type_def| {
                        if (!common_key_type.?.eql(actual_type_def) and common_key_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_key_type = common_key_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object) orelse common_key_type;
                            common_key_type = try common_key_type.?.toInstance(self.gc.allocator, &self.gc.type_registry);
                        }
                    }
                }

                if (value_type == null) {
                    if (common_value_type == null) {
                        common_value_type = value.type_def;
                    } else if (value.type_def) |actual_type_def| {
                        if (!common_value_type.?.eql(actual_type_def) and common_value_type.?.def_type == .ObjectInstance and actual_type_def.def_type == .ObjectInstance) {
                            common_value_type = common_value_type.?.resolved_type.?.ObjectInstance.resolved_type.?.Object.both_conforms(actual_type_def.resolved_type.?.ObjectInstance.resolved_type.?.Object) orelse common_value_type;
                            common_value_type = try common_value_type.?.toInstance(self.gc.allocator, &self.gc.type_registry);
                        }
                    }
                }

                if (!self.check(.RightBrace)) {
                    trailing_comma = true;
                    try self.consume(.Comma, "Expected `,` after map entry.");
                } else {
                    trailing_comma = false;
                }
            }

            key_type = key_type orelse common_key_type;
            value_type = value_type orelse common_value_type;
        } else {
            try self.consume(.RightBrace, "Expected `}`");
        }

        if (key_type == null and value_type == null) {
            try self.reportError("Map key and value type can't be infered");

            key_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
            value_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
        }

        var map_def = ObjMap.MapDef.init(self.gc.allocator, key_type.?, value_type.?);

        var resolved_type: ObjTypeDef.TypeUnion = ObjTypeDef.TypeUnion{ .Map = map_def };

        var map_type: *ObjTypeDef = try self.gc.type_registry.getTypeDef(
            .{
                .optional = try self.match(.Question),
                .def_type = .Map,
                .resolved_type = resolved_type,
            },
        );

        var node = try self.gc.allocator.create(MapNode);
        node.* = MapNode{
            .keys = keys.items,
            .values = values.items,
            .trailing_comma = trailing_comma,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;
        node.node.type_def = map_type;

        return &node.node;
    }

    fn fun(self: *Self, _: bool) anyerror!*ParseNode {
        return try self.function(null, .Anonymous, null);
    }

    fn function(self: *Self, name: ?Token, function_type: FunctionType, this: ?*ObjTypeDef) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        var function_node = try self.gc.allocator.create(FunctionNode);
        function_node.* = try FunctionNode.init(
            self,
            function_type,
            self.script_name,
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
            .id = ObjFunction.FunctionDef.nextId(),
            .script_name = try self.gc.copyString(self.script_name),
            .name = if (name) |uname|
                try self.gc.copyString(uname.lexeme)
            else
                try self.gc.copyString("anonymous"),
            .return_type = undefined,
            .yield_type = undefined,
            .parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(self.gc.allocator),
            .defaults = std.AutoArrayHashMap(*ObjString, Value).init(self.gc.allocator),
            .function_type = function_type,
            .generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(self.gc.allocator),
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = function_def };

        function_typedef.resolved_type = function_resolved_type;

        // We replace it with a self.gc.type_registry.getTypeDef pointer at the end
        function_node.node.type_def = &function_typedef;

        // So any reference to a generic in the function's body can be resolved
        self.current.?.generics = &function_node.node.type_def.?.resolved_type.?.Function.generic_types;

        // Parse generic & argument list
        if (function_type == .Test) {
            try self.consume(.String, "Expected a string after `test`.");
            function_node.test_message = try self.string(false);
        } else {
            try self.consume(.LeftParen, "Expected `(` after function name.");

            // Generics
            if (try self.match(.Less)) {
                var i: usize = 0;
                while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
                    try self.consume(.Identifier, "Expected generic type identifier");

                    const generic_identifier = try self.gc.copyString(self.parser.previous_token.?.lexeme);
                    if (function_node.node.type_def.?.resolved_type.?.Function.generic_types.get(generic_identifier) == null) {
                        const generic = ObjTypeDef.GenericDef{
                            .origin = function_node.node.type_def.?.resolved_type.?.Function.id,
                            .index = i,
                        };
                        const resolved_type = ObjTypeDef.TypeUnion{ .Generic = generic };
                        try function_node.node.type_def.?.resolved_type.?.Function.generic_types.put(
                            generic_identifier,
                            try self.gc.type_registry.getTypeDef(
                                ObjTypeDef{
                                    .def_type = .Generic,
                                    .resolved_type = resolved_type,
                                },
                            ),
                        );
                    } else {
                        try self.reportErrorFmt("Generic type `{s}` already defined", .{self.parser.previous_token.?.lexeme});
                    }

                    if (!self.check(.Greater)) {
                        try self.consume(.Comma, "Expected `,` between generic types");
                    }
                }

                if (function_node.node.type_def.?.resolved_type.?.Function.generic_types.count() == 0) {
                    try self.reportError("Expected at least one generic type");
                }

                try self.consume(.Greater, "Expected `>` after generic types list");

                if (!self.check(.RightParen)) {
                    try self.consume(.Comma, "Expected `,` after generic types list");
                }
            }

            // Arguments
            var arity: usize = 0;
            if (!self.check(.RightParen)) {
                while (true) {
                    arity += 1;
                    if (arity > 255) {
                        try self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                    }

                    var param_type: *ObjTypeDef = try (try self.parseTypeDef(function_node.node.type_def.?.resolved_type.?.Function.generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);

                    var slot: usize = try self.parseVariable(
                        param_type,
                        true, // function arguments are constant
                        "Expected parameter name",
                    );
                    var arg_name: *ObjString = undefined;

                    if (self.current.?.scope_depth > 0) {
                        var local: Local = self.current.?.locals[slot];
                        arg_name = local.name;
                        try function_node.node.type_def.?.resolved_type.?.Function.parameters.put(local.name, local.type_def);
                    } else {
                        var global: Global = self.globals.items[slot];
                        arg_name = global.name;
                        try function_node.node.type_def.?.resolved_type.?.Function.parameters.put(global.name, global.type_def);
                    }

                    self.markInitialized();

                    // Default arguments
                    if (function_type == .Function or function_type == .Method or function_type == .Anonymous or function_type == .Extern) {
                        if (try self.match(.Equal)) {
                            var expr = try self.expression(false);

                            if (expr.type_def != null and expr.type_def.?.def_type == .Placeholder and param_type.def_type == .Placeholder) {
                                try PlaceholderDef.link(param_type, expr.type_def.?, .Assignment);
                            }

                            if (!expr.isConstant(expr)) {
                                try self.reportError("Default parameters must be constant values.");
                            }

                            try function_node.node.type_def.?.resolved_type.?.Function.defaults.put(
                                arg_name,
                                try expr.toValue(expr, self.gc),
                            );
                        } else if (param_type.optional) {
                            try function_node.node.type_def.?.resolved_type.?.Function.defaults.put(
                                arg_name,
                                Value.Null,
                            );
                        }
                    }

                    if (!try self.match(.Comma)) break;
                }
            }

            try self.consume(.RightParen, "Expected `)` after function parameters.");
        }

        // Parse return type
        var parsed_return_type = false;
        if (function_type != .Test and try self.match(.Greater)) {
            const return_type = try self.parseTypeDef(function_node.node.type_def.?.resolved_type.?.Function.generic_types);

            function_node.node.type_def.?.resolved_type.?.Function.return_type = try return_type.toInstance(self.gc.allocator, &self.gc.type_registry);

            parsed_return_type = true;
        } else if (function_type != .Anonymous and function_type != .Test) {
            try self.reportError("Expected `>` after function argument list.");
        }

        // Parse yield type
        if (parsed_return_type and (function_type == .Method or function_type == .Function) and (try self.match(.Greater))) {
            const yield_type = try self.parseTypeDef(function_node.node.type_def.?.resolved_type.?.Function.generic_types);

            if (!yield_type.optional and yield_type.def_type != .Void) {
                try self.reportError("Expected optional type");
            }

            function_node.node.type_def.?.resolved_type.?.Function.yield_type = try yield_type.toInstance(self.gc.allocator, &self.gc.type_registry);
        } else {
            function_node.node.type_def.?.resolved_type.?.Function.yield_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
        }

        // Error set
        if (parsed_return_type and (function_type == .Method or function_type == .Function or function_type == .Anonymous or function_type == .EntryPoint or function_type == .Extern) and (try self.match(.BangGreater))) {
            var error_types = std.ArrayList(*ObjTypeDef).init(self.gc.allocator);
            const end_token: TokenType = if (function_type == .Extern) .Semicolon else .LeftBrace;
            while (!self.check(end_token) and !self.check(.Eof)) {
                const error_type = try (try self.parseTypeDef(function_node.node.type_def.?.resolved_type.?.Function.generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);
                try error_types.append(error_type);

                if (error_type.optional) {
                    try self.reportError("Error type can't be optional");
                }

                if (!self.check(end_token)) {
                    try self.consume(.Comma, "Expected `,` after error type");
                }
            }

            if (error_types.items.len > 0) {
                function_node.node.type_def.?.resolved_type.?.Function.error_types = error_types.items;
            } else {
                error_types.deinit();
            }
        }

        // Parse body
        if (try self.match(.Arrow)) {
            function_node.node.type_def.?.resolved_type.?.Function.lambda = true;
            function_node.arrow_expr = try self.expression(false);

            if (function_node.body) |placeholder_body| {
                self.gc.allocator.destroy(placeholder_body);
            }
            function_node.body = null;

            if (!parsed_return_type and function_node.arrow_expr.?.type_def != null) {
                function_node.node.type_def.?.resolved_type.?.Function.return_type = function_node.arrow_expr.?.type_def.?;
                parsed_return_type = true;
            }
        } else if (function_type != .Extern and function_type != .Abstract) {
            if (!parsed_return_type) {
                function_node.node.type_def.?.resolved_type.?.Function.return_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
                parsed_return_type = true;
            }

            try self.consume(.LeftBrace, "Expected `{` before function body.");
            function_node.body = BlockNode.cast(try self.block(null)).?;
        }

        if (!parsed_return_type) {
            function_node.node.type_def.?.resolved_type.?.Function.return_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });
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

        function_node.node.type_def = try self.gc.type_registry.getTypeDef(function_typedef);
        function_node.node.location = start_location;
        function_node.node.end_location = self.parser.previous_token.?;

        return &self.endFrame().node;
    }

    fn inlineCatch(self: *Self) !?*ParseNode {
        if (try self.match(.Catch)) {
            return try self.expression(false);
        }

        return null;
    }

    // `test` is just like a function but we don't parse arguments and we don't care about its return type
    fn testStatement(self: *Self) !*ParseNode {
        const start_location = self.parser.previous_token.?;

        var function_def_placeholder: ObjTypeDef = .{
            .def_type = .Function,
        };

        var test_id = std.ArrayList(u8).init(self.gc.allocator);
        try test_id.writer().print("$test#{}", .{self.test_count});
        // TODO: this string is never freed

        self.test_count += 1;

        const name_token: Token = Token{
            .token_type = .Test,
            .lexeme = test_id.items,
            .line = 0,
            .column = 0,
            .source = "",
            .script_name = "",
        };

        const slot = try self.declareVariable(&function_def_placeholder, name_token, true);

        self.markInitialized();

        const function_node = try self.function(name_token, FunctionType.Test, null);

        // Make it as a global definition
        var node = try self.gc.allocator.create(VarDeclarationNode);
        node.* = VarDeclarationNode{
            .name = name_token,
            .value = function_node,
            .type_def = function_node.type_def.?,
            .constant = true,
            .slot = slot,
            .slot_type = .Global,
            .expression = false,
        };
        node.node.location = start_location;
        node.node.end_location = self.parser.previous_token.?;

        return node.toNode();
    }

    fn importScript(self: *Self, file_name: []const u8, prefix: ?[]const u8, imported_symbols: *std.StringHashMap(void)) anyerror!?ScriptImport {
        var import: ?ScriptImport = self.imports.get(file_name);

        if (import == null) {
            const buzz_path: []const u8 = std.os.getenv("BUZZ_PATH") orelse ".";

            var lib_path = std.ArrayList(u8).init(self.gc.allocator);
            defer lib_path.deinit();
            _ = try lib_path.writer().print(
                "{s}{s}{s}.buzz",
                .{ buzz_path, std.fs.path.sep_str, file_name },
            );

            var dir_path = std.ArrayList(u8).init(self.gc.allocator);
            defer dir_path.deinit();
            _ = try dir_path.writer().print(
                ".{s}{s}.buzz",
                .{ std.fs.path.sep_str, file_name },
            );

            // Find and read file
            // zig fmt: off
            var file: ?std.fs.File = null;
            var absolute_path: ?[]const u8 = null;
            var owned = false;

            if (std.fs.path.isAbsolute(lib_path.items)) {
                file = std.fs.openFileAbsolute(lib_path.items, .{}) catch null;
                if (file != null) {
                    absolute_path = lib_path.items;
                }
            } else {
                file = std.fs.cwd().openFile(lib_path.items, .{}) catch null;
                if (file != null) {
                    absolute_path = try std.fs.cwd().realpathAlloc(self.gc.allocator, lib_path.items);
                    owned = true;
                }
            }

            if (file == null) {
                if (std.fs.path.isAbsolute(dir_path.items)) {
                    file = std.fs.openFileAbsolute(dir_path.items, .{}) catch {
                        try self.reportErrorFmt("Could not find buzz script `{s}`", .{file_name});
                        return null;
                    };

                    absolute_path = dir_path.items;
                } else {
                    file = std.fs.cwd().openFile(dir_path.items, .{}) catch {
                        try self.reportErrorFmt("Could not find buzz script `{s}`", .{file_name});
                        return null;
                    };

                    absolute_path = try std.fs.cwd().realpathAlloc(self.gc.allocator, dir_path.items);
                    owned = true;
                }
            }
            // zig fmt: on
            defer file.?.close();
            defer {
                if (owned) {
                    self.gc.allocator.free(absolute_path.?);
                }
            }

            // TODO: put source strings in a ArenaAllocator that frees everything at the end of everything
            const source = try self.gc.allocator.alloc(u8, (try file.?.stat()).size);
            // defer self.gc.allocator.free(source);

            _ = try file.?.readAll(source);

            var parser = Parser.init(self.gc, self.imports, true);
            defer parser.deinit();

            if (try parser.parse(source, file_name)) |import_node| {
                FunctionNode.cast(import_node).?.import_root = true;

                import = ScriptImport{
                    .function = import_node,
                    .globals = std.ArrayList(Global).init(self.gc.allocator),
                    .absolute_path = try self.gc.copyString(absolute_path.?),
                };

                for (parser.globals.items) |*global| {
                    if (global.exported) {
                        global.*.exported = false;

                        if (global.export_alias) |export_alias| {
                            global.*.name = try self.gc.copyString(export_alias);
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
                        try self.reportErrorFmt("Shadowed global `{s}`", .{global.name.string});
                    }

                    global.*.prefix = prefix;
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
    fn importLibSymbol(self: *Self, full_file_name: []const u8, symbol: []const u8) !?*ObjNative {
        const buzz_path: []const u8 = std.os.getenv("BUZZ_PATH") orelse ".";

        // Remove .buzz extension, this occurs if this is the script being run or if the script was imported like so `import lib/std.buzz`
        // We consider that any other extension is silly from the part of the user
        const file_name =
            if (std.mem.endsWith(u8, full_file_name, ".buzz")) full_file_name[0..(full_file_name.len - 5)] else full_file_name;

        // We have to insert `lib` prefix
        const last_sep = std.mem.lastIndexOf(u8, file_name, std.fs.path.sep_str) orelse 0;

        var lib_path = std.ArrayList(u8).init(self.gc.allocator);
        defer lib_path.deinit();
        try lib_path.writer().print(
            "{s}{s}{s}lib{s}.{s}",
            .{
                buzz_path,
                std.fs.path.sep_str,
                file_name[0 .. last_sep + 1],
                file_name[last_sep + 1 ..],
                switch (builtin.os.tag) {
                    .linux, .freebsd, .openbsd => "so",
                    .windows => "dll",
                    .macos, .tvos, .watchos, .ios => "dylib",
                    else => unreachable,
                },
            },
        );

        var dir_path = std.ArrayList(u8).init(self.gc.allocator);
        defer dir_path.deinit();
        try dir_path.writer().print(
            ".{s}{s}lib{s}.{s}",
            .{
                std.fs.path.sep_str,
                file_name[0 .. last_sep + 1],
                file_name[last_sep + 1 ..],
                switch (builtin.os.tag) {
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
            const ssymbol = try self.gc.allocator.dupeZ(u8, symbol);
            defer self.gc.allocator.free(ssymbol);

            // Lookup symbol NativeFn
            const opaque_symbol_method = dlib.lookup(*anyopaque, ssymbol);

            if (opaque_symbol_method == null) {
                try self.reportErrorFmt("Could not find symbol `{s}` in lib `{s}`", .{ symbol, file_name });
                return null;
            }

            // Create a ObjNative with it
            return try self.gc.allocateObject(
                ObjNative,
                .{
                    .native = opaque_symbol_method.?,
                },
            );
        }

        if (builtin.os.tag == .macos) {
            try self.reportError(std.mem.sliceTo(dlerror(), 0));
        } else {
            try self.reportErrorFmt("Could not open lib `{s}`", .{file_name});
        }

        return null;
    }

    fn parseFunctionType(self: *Self, parent_generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) !*ObjTypeDef {
        assert(self.parser.previous_token.?.token_type == .Function or self.parser.previous_token.?.token_type == .Extern);

        const is_extern = self.parser.previous_token.?.token_type == .Extern;

        if (is_extern) {
            try self.consume(.Function, "Expected `Function` after `extern`.");
        }

        var name: ?*ObjString = null;
        if (try self.match(.Identifier)) {
            name = try self.gc.copyString(self.parser.previous_token.?.lexeme);
        }

        try self.consume(.LeftParen, "Expected `(` after function name.");

        var merged_generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(self.gc.allocator);
        defer merged_generic_types.deinit();
        if (parent_generic_types != null) {
            var it = parent_generic_types.?.iterator();
            while (it.next()) |kv| {
                try merged_generic_types.put(kv.key_ptr.*, kv.value_ptr.*);
            }
        }

        var generic_types = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(self.gc.allocator);
        if (try self.match(.Less)) {
            var i: usize = 0;
            while (!self.check(.Greater) and !self.check(.Eof)) : (i += 1) {
                try self.consume(.Identifier, "Expected generic type identifier");

                const generic_identifier = try self.gc.copyString(self.parser.previous_token.?.lexeme);
                if (generic_types.get(generic_identifier) == null) {
                    const generic = ObjTypeDef.GenericDef{
                        .origin = undefined,
                        .index = i,
                    };
                    const resolved = ObjTypeDef.TypeUnion{ .Generic = generic };
                    const type_def = try self.gc.type_registry.getTypeDef(
                        ObjTypeDef{
                            .def_type = .Generic,
                            .resolved_type = resolved,
                        },
                    );

                    try generic_types.put(
                        generic_identifier,
                        type_def,
                    );
                    try merged_generic_types.put(
                        generic_identifier,
                        type_def,
                    );
                } else {
                    try self.reportErrorFmt("Generic type `{s}` already defined", .{self.parser.previous_token.?.lexeme});
                }

                if (!self.check(.Greater)) {
                    try self.consume(.Comma, "Expected `,` between generic types");
                }
            }

            if (generic_types.count() == 0) {
                try self.reportError("Expected at least one generic type");
            }

            try self.consume(.Greater, "Expected `>` after generic types list");
            if (!self.check(.RightParen)) {
                try self.consume(.Comma, "Expected `,` after generic types list");
            }
        }

        var parameters = std.AutoArrayHashMap(*ObjString, *ObjTypeDef).init(self.gc.allocator);
        var defaults = std.AutoArrayHashMap(*ObjString, Value).init(self.gc.allocator);
        var arity: usize = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                arity += 1;
                if (arity > 255) {
                    try self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                }

                var param_type: *ObjTypeDef = try (try self.parseTypeDef(merged_generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);
                try self.consume(.Identifier, "Expected argument name");
                var param_name: []const u8 = self.parser.previous_token.?.lexeme;
                var arg_name = try self.gc.copyString(param_name);

                try parameters.put(arg_name, param_type);

                if (try self.match(.Equal)) {
                    var expr = try self.expression(false);

                    if (expr.type_def != null and expr.type_def.?.def_type == .Placeholder and param_type.def_type == .Placeholder) {
                        try PlaceholderDef.link(param_type, expr.type_def.?, .Assignment);
                    }

                    if (!expr.isConstant(expr)) {
                        try self.reportError("Default parameters must be constant values.");
                    }

                    try defaults.put(arg_name, try expr.toValue(expr, self.gc));
                } else if (param_type.optional) {
                    try defaults.put(arg_name, Value.Null);
                }

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after function parameters.");

        var return_type: *ObjTypeDef = if (try self.match(.Greater))
            try self.parseTypeDef(null)
        else
            try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });

        var yield_type: *ObjTypeDef = if (try self.match(.Greater))
            try self.parseTypeDef(null)
        else
            try self.gc.type_registry.getTypeDef(.{ .def_type = .Void });

        var error_types: ?std.ArrayList(*ObjTypeDef) = null;
        if (try self.match(.BangGreater)) {
            error_types = std.ArrayList(*ObjTypeDef).init(self.gc.allocator);
            while (!self.check(.Eof)) {
                const error_type = try (try self.parseTypeDef(generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);
                try error_types.?.append(error_type);

                if (error_type.optional) {
                    try self.reportError("Error type can't be optional");
                }

                if (!self.check(.Comma)) {
                    break;
                }
            }
        }

        var function_typedef: ObjTypeDef = .{
            .def_type = .Function,
            .optional = try self.match(.Question),
        };

        var function_def: ObjFunction.FunctionDef = .{
            .id = ObjFunction.FunctionDef.nextId(),
            .script_name = try self.gc.copyString(self.script_name),
            .name = name orelse try self.gc.copyString("anonymous"),
            .return_type = try return_type.toInstance(self.gc.allocator, &self.gc.type_registry),
            .yield_type = try yield_type.toInstance(self.gc.allocator, &self.gc.type_registry),
            .parameters = parameters,
            .defaults = defaults,
            .function_type = if (is_extern) .Extern else .Anonymous,
            .generic_types = generic_types,
            .error_types = if (error_types != null) error_types.?.items else null,
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{ .Function = function_def };

        function_typedef.resolved_type = function_resolved_type;

        return try self.gc.type_registry.getTypeDef(function_typedef);
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
                .Placeholder = PlaceholderDef.init(self.gc.allocator, user_type_name),
            };

            placeholder_resolved_type.Placeholder.name = try self.gc.copyString(user_type_name.lexeme);

            var_type = try self.gc.type_registry.getTypeDef(
                .{
                    .def_type = .Placeholder,
                    .resolved_type = placeholder_resolved_type,
                },
            );

            global_slot = try self.declarePlaceholder(user_type_name, var_type.?);
        }

        return global_slot.?;
    }

    pub fn parseTypeDefFrom(self: *Self, source: []const u8) anyerror!*ObjTypeDef {
        var type_scanner = Scanner.init(self.gc.allocator, self.script_name, source);
        // Replace parser scanner with one that only looks at that substring
        const scanner = self.scanner;
        self.scanner = type_scanner;
        const parser = self.parser;
        self.parser = ParserState.init(self.gc.allocator);

        _ = try self.advance();

        const parsed_type = try self.parseTypeDef(null);

        // Restore normal scanner and parser state
        self.scanner = scanner;
        self.parser.deinit();
        self.parser = parser;

        return parsed_type;
    }

    fn parseTypeDef(self: *Self, generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) anyerror!*ObjTypeDef {
        if (try self.match(.Str)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String });
        } else if (try self.match(.Pat)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Pattern });
        } else if (try self.match(.Ud)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .UserData });
        } else if (try self.match(.Type)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type });
        } else if (try self.match(.Void)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = false, .def_type = .Void });
        } else if (try self.match(.Int)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Integer });
        } else if (try self.match(.Float)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Float });
        } else if (try self.match(.Bool)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool });
        } else if (try self.match(.Type)) {
            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type });
        } else if (try self.match(.LeftBracket)) {
            return self.parseListType(generic_types);
        } else if (try self.match(.LeftBrace)) {
            return self.parseMapType(generic_types);
        } else if (try self.match(.Function) or try self.match(.Extern)) {
            return try self.parseFunctionType(generic_types);
        } else if (try self.match(.Fib)) {
            return try self.parseFiberType(generic_types);
        } else if (try self.match(.Obj)) {
            return try self.parseObjType(generic_types);
        } else if ((try self.match(.Identifier))) {
            var user_type: ?*ObjTypeDef = null;
            // Is it a generic type defined in enclosing functions?
            if (self.current.?.searchGeneric(try self.gc.copyString(self.parser.previous_token.?.lexeme))) |generic_type| {
                user_type = generic_type;
            } else if (generic_types != null) {
                // Is it generic type defined in a function signature being parsed?
                if (generic_types.?.get(try self.gc.copyString(self.parser.previous_token.?.lexeme))) |generic_type| {
                    user_type = generic_type;
                }
            }

            // Is it a user defined type (object, enum, etc.) defined in global scope?
            if (user_type == null) {
                var user_type_index = try self.parseUserType();
                user_type = self.globals.items[user_type_index].type_def;
            }

            if (try self.match(.Question)) {
                return try user_type.?.cloneOptional(&self.gc.type_registry);
            }

            return user_type.?;
        } else {
            try self.reportErrorAtCurrent("Expected type definition.");

            return try self.gc.type_registry.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Void });
        }
    }

    fn parseObjType(self: *Self, generic_types: ?std.AutoArrayHashMap(*ObjString, *ObjTypeDef)) !*ObjTypeDef {
        try self.consume(.LeftBrace, "Expected `{` after `obj`");

        const qualifier = try std.mem.replaceOwned(u8, self.gc.allocator, self.script_name, "/", ".");
        defer self.gc.allocator.free(qualifier);
        var qualified_name = std.ArrayList(u8).init(self.gc.allocator);
        defer qualified_name.deinit();
        try qualified_name.writer().print("{s}.anonymous", .{qualifier});

        var object_def = ObjObject.ObjectDef.init(
            self.gc.allocator,
            try self.gc.copyString("anonymous"),
            try self.gc.copyString(qualified_name.items),
            true,
        );

        var resolved_type = ObjTypeDef.TypeUnion{ .Object = object_def };

        var object_type: ObjTypeDef = .{
            .def_type = .Object,
            .resolved_type = resolved_type,
        };

        // Anonymous object can only have properties without default values (no methods, no static fields)
        // They can't self reference since their anonymous
        var fields = std.StringHashMap(void).init(self.gc.allocator);
        defer fields.deinit();
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            const property_type = try (try self.parseTypeDef(generic_types)).toInstance(self.gc.allocator, &self.gc.type_registry);

            try self.consume(.Identifier, "Expected property name.");
            const property_name = self.parser.previous_token.?.clone();

            if (fields.get(property_name.lexeme) != null) {
                try self.reportError("A property with that name already exists.");
            }

            if (!self.check(.RightBrace) or self.check(.Comma)) {
                try self.consume(.Comma, "Expected `,` after property definition.");
            }
            try object_type.resolved_type.?.Object.fields.put(property_name.lexeme, property_type);
            try fields.put(property_name.lexeme, {});
        }

        try self.consume(.RightBrace, "Expected `}` after object body.");

        return try self.gc.type_registry.getTypeDef(object_type);
    }

    fn parseVariable(self: *Self, variable_type: *ObjTypeDef, constant: bool, error_message: []const u8) !usize {
        try self.consume(.Identifier, error_message);

        return try self.declareVariable(variable_type, null, constant);
    }

    inline fn markInitialized(self: *Self) void {
        if (self.current.?.scope_depth == 0) {
            // assert(!self.globals.items[self.globals.items.len - 1].initialized);
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
                .Placeholder = PlaceholderDef.init(self.gc.allocator, name),
            };
            placeholder_resolved_type.Placeholder.name = try self.gc.copyString(name.lexeme);

            placeholder_type = try self.gc.type_registry.getTypeDef(.{ .def_type = .Placeholder, .resolved_type = placeholder_resolved_type });
        }

        assert(!placeholder_type.optional);

        const global: usize = try self.addGlobal(
            name,
            placeholder_type,
            false,
        );
        // markInitialized but we don't care what depth we are in
        self.globals.items[global].initialized = true;

        if (BuildOptions.debug_placeholders) {
            std.debug.print(
                "global placeholder @{} for `{s}` at {}\n",
                .{
                    @ptrToInt(placeholder_type),
                    name.lexeme,
                    global,
                },
            );
        }

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
            for (self.globals.items, 0..) |global, index| {
                if (mem.eql(u8, name.lexeme, global.name.string) and !global.hidden) {
                    // If we found a placeholder with that name, try to resolve it with `variable_type`
                    if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.type_def.resolved_type.?.Placeholder.name.?.string)) {
                        // A function declares a global with an incomplete typedef so that it can handle recursion
                        // The placeholder resolution occurs after we parsed the functions body in `funDeclaration`
                        if (variable_type.resolved_type != null or @enumToInt(variable_type.def_type) < @enumToInt(ObjTypeDef.Type.ObjectInstance)) {
                            if (BuildOptions.debug_placeholders) {
                                std.debug.print(
                                    "Global placeholder @{} resolve with @{} {s} (opt {})\n",
                                    .{
                                        @ptrToInt(global.type_def),
                                        @ptrToInt(variable_type),
                                        (try variable_type.toStringAlloc(self.gc.allocator)).items,
                                        variable_type.optional,
                                    },
                                );
                            }

                            try self.resolvePlaceholder(global.type_def, variable_type, constant);
                        }

                        return index;
                    } else if (global.prefix == null) {
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
            .name = try self.gc.copyString(name.lexeme),
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
            .constant = constant,
        };

        self.current.?.local_count += 1;

        return self.current.?.local_count - 1;
    }

    fn dumpGlobals(self: *Self) !void {
        if (BuildOptions.debug) {
            for (self.globals.items, 0..) |global, index| {
                std.debug.print(
                    "global {}: {s} @{} {s}\n",
                    .{
                        index,
                        global.name.string,
                        @ptrToInt(global.type_def),
                        try global.type_def.toString(self.gc.allocator),
                    },
                );
            }
        }
    }

    fn addGlobal(self: *Self, name: Token, global_type: *ObjTypeDef, constant: bool) !usize {
        // Search for an existing placeholder global with the same name
        for (self.globals.items, 0..) |global, index| {
            if (global.type_def.def_type == .Placeholder and global.type_def.resolved_type.?.Placeholder.name != null and mem.eql(u8, name.lexeme, global.name.string)) {
                if (global_type.def_type != .Placeholder) {
                    try self.resolvePlaceholder(global.type_def, global_type, constant);
                }

                return index;
            }
        }

        if (self.globals.items.len == std.math.maxInt(u24)) {
            try self.reportError("Too many global variables.");
            return 0;
        }

        try self.globals.append(
            Global{
                .name = try self.gc.copyString(name.lexeme),
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
                    try self.reportErrorFmt("Can't read global `{s}` variable in its own initializer.", .{global.name.string});
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
