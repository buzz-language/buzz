const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const _chunk = @import("./chunk.zig");
const _obj = @import("./obj.zig");
const _token = @import("./token.zig");
const _vm = @import("./vm.zig");
const _value = @import("./value.zig");

const VM = _vm.VM;
const OpCode = _chunk.OpCode;
const ObjFunction = _obj.ObjFunction;
const ObjTypeDef = _obj.ObjTypeDef;
const ObjString = _obj.ObjString;
const ObjList = _obj.ObjList;
const ObjMap = _obj.ObjMap;
const Token = _token.Token;
const TokenType = _token.TokenType;
const Scanner = @import("./scanner.zig").Scanner;
const Value = _value.Value;

const CompileError = error {
    Unrecoverable
};

pub const FunctionType = enum {
    Function,
    Initializer,
    Method,
    Script
};

pub const Local = struct {
    name: *ObjString,
    type_def: *ObjTypeDef,
    depth: i32,
    is_captured: bool
};

pub const Global = struct {
    name: *ObjString,
    type_def: *ObjTypeDef,
    initialized: bool = false,
};

pub const UpValue = struct {
    index: u8,
    is_local: bool
};

pub const ClassCompiler = struct {
    enclosing: ?*ClassCompiler
};

pub const ChunkCompiler = struct {
    const Self = @This();

    enclosing: ?*ChunkCompiler = null,
    function: *ObjFunction,
    function_type: FunctionType,

    locals: [255]Local,
    local_count: u8 = 0,
    upvalues: [255]UpValue,
    scope_depth: u32 = 0,

    pub fn init(compiler: *Compiler, function_type: FunctionType, file_name: ?[]const u8) !void {
        var self: Self = .{
            .locals = [_]Local{undefined} ** 255,
            .upvalues = [_]UpValue{undefined} ** 255,
            .enclosing = compiler.current,
            .function_type = function_type,
            .function = ObjFunction.cast(try _obj.allocateObject(compiler.vm, .Function)).?,
        };

        var file_name_string: ?*ObjString = if (file_name) |name| try _obj.copyString(compiler.vm, name) else null;

        self.function.* = try ObjFunction.init(compiler.vm.allocator, if (function_type != .Script)
            try _obj.copyString(compiler.vm, compiler.parser.previous_token.?.lexeme)
        else
            file_name_string orelse try _obj.copyString(compiler.vm, VM.script_string),

        try compiler.vm.getTypeDef(.{
            .def_type = .Void,
            .optional = false,
        }));

        compiler.current = try compiler.vm.allocator.create(ChunkCompiler);
        compiler.current.?.* = self;

        // First local is reserved for an eventual `this`
        var local: *Local = &compiler.current.?.locals[self.local_count];
        compiler.current.?.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        // TODO: when do we define, `this` typedef ?
        local.type_def = try compiler.vm.getTypeDef(.{
            .def_type = .Void,
            .optional = false,
        });

        local.name = try _obj.copyString(compiler.vm, if (function_type == .Function) VM.this_string else VM.empty_string);
    }
};

pub const ParserState = struct {
    const Self = @This();

    current_token: ?Token = null,
    previous_token: ?Token = null,
    had_error: bool = false,
    panic_mode: bool = false,
};

pub const Compiler = struct {
    const Self = @This();

    const Precedence = enum {
        None,
        Assignment, // =, -=, +=, *=, /=
        Is, // is
        NullOr, // ??
        Or, // or
        And, // and
        Xor, // xor
        Comparison, // ==, !=
        Term, // +, -
        Shift, // >>, <<
        Factor, // /, *, %
        Unary, // +, ++, -, --, !
        Call, // call(), dot.ref, sub[script]
        Primary, // literal, (grouped expression), super.ref, identifier
    };

    const ParseFn = fn (*Compiler, bool) anyerror!*ObjTypeDef;
    const InfixParseFn = fn (*Compiler, bool, *ObjTypeDef) anyerror!*ObjTypeDef;

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?InfixParseFn,
        precedence: Precedence,
    };

    const rules = [_]ParseRule{
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Pipe
        .{ .prefix = null,     .infix = null, .precedence = .None }, // LeftBracket
        .{ .prefix = null,     .infix = null, .precedence = .None }, // RightBracket
        .{ .prefix = grouping, .infix = call, .precedence = .Call }, // LeftParen
        .{ .prefix = null,     .infix = null, .precedence = .None }, // RightParen
        .{ .prefix = null,     .infix = null, .precedence = .None }, // LeftBrace
        .{ .prefix = null,     .infix = null, .precedence = .None }, // RightBrace
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Dot
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Comma
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Semicolon
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Greater
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Less
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Plus
        .{ .prefix = unary,    .infix = null, .precedence = .None }, // Minus
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Star
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Slash
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Percent
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Question
        .{ .prefix = unary,    .infix = null, .precedence = .None }, // Bang
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Colon
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Equal
        .{ .prefix = null,     .infix = null, .precedence = .None }, // EqualEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // BangEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // GreaterEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // LessEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // QuestionQuestion
        .{ .prefix = null,     .infix = null, .precedence = .None }, // PlusEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // MinusEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // StarEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // SlashEqual
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Increment
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Decrement
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Arrow
        .{ .prefix = literal,  .infix = null, .precedence = .None }, // True
        .{ .prefix = literal,  .infix = null, .precedence = .None }, // False
        .{ .prefix = literal,  .infix = null, .precedence = .None }, // Null
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Str
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Num
        .{ .prefix = byte,     .infix = null, .precedence = .None }, // Byte
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Type
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Bool
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Function
        .{ .prefix = null,     .infix = null, .precedence = .None }, // ShiftRight
        .{ .prefix = null,     .infix = null, .precedence = .None }, // ShiftLeft
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Xor
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Or
        .{ .prefix = null,     .infix = null, .precedence = .None }, // And
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Return
        .{ .prefix = null,     .infix = null, .precedence = .None }, // If
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Else
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Do
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Until
        .{ .prefix = null,     .infix = null, .precedence = .None }, // While
        .{ .prefix = null,     .infix = null, .precedence = .None }, // For
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Switch
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Break
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Default
        .{ .prefix = null,     .infix = null, .precedence = .None }, // In
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Is
        .{ .prefix = number,   .infix = null, .precedence = .None }, // Number
        .{ .prefix = string,   .infix = null, .precedence = .None }, // String
        .{ .prefix = variable, .infix = null, .precedence = .None }, // Identifier
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Fun
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Object
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Class
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Enum
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Eof
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Error

        // TODO: remove
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Print
    };

    vm: *VM,

    scanner: ?Scanner = null,
    parser: ParserState = .{},
    current: ?*ChunkCompiler = null,
    current_class: ?*ClassCompiler = null,
    globals: std.ArrayList(Global),

    pub fn init(vm: *VM) Self {
        return .{
            .vm = vm,
            .globals = std.ArrayList(Global).init(vm.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();
    }

    // TODO: walk the chain of compiler and destroy them in deinit

    pub fn compile(self: *Self, source: []const u8, file_name: ?[]const u8) !?*ObjFunction {
        if (self.scanner != null) {
            self.scanner = null;
        }

        self.scanner = Scanner.init(source);
        defer self.scanner = null;

        try ChunkCompiler.init(self, .Script, file_name);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        try self.advance();

        // Enter AST
        while (!(try self.match(.Eof))) {
            self.declaration() catch return null;
        }

        var function: *ObjFunction = try self.endCompiler();

        return if (self.parser.had_error) null else function;
    }

    fn reportErrorAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.parser.panic_mode) {
            return;
        }

        self.parser.panic_mode = true;

        std.debug.warn("\u{001b}[31m[{}:{}] Error", .{ token.line + 1, token.column + 1 });

        if (token.token_type == .Eof) {
            std.debug.warn(" at end", .{});
        } else if (token.token_type != .Error) { // We report error to the token just before a .Error token
            std.debug.warn(" at '{s}'", .{token.lexeme});
        }

        std.debug.warn(": {s}\u{001b}[0m\n", .{message});

        self.parser.had_error = true;
    }

    fn reportError(self: *Self, message: []const u8) void {
        self.reportErrorAt(&self.parser.previous_token.?, message);
    }

    fn reportErrorAtCurrent(self: *Self, message: []const u8) void {
        self.reportErrorAt(&self.parser.current_token.?, message);
    }

    fn reportTypeCheck(self: *Self, expected_type: *ObjTypeDef, actual_type: *ObjTypeDef, message: []const u8) !void {
        var expected_str: []const u8 = try expected_type.toString(self.vm.allocator);
        var actual_str: []const u8 = try actual_type.toString(self.vm.allocator);
        var error_message: []u8 = try self.vm.allocator.alloc(u8, expected_str.len + actual_str.len + 200);
        defer {
            self.vm.allocator.free(error_message);
            self.vm.allocator.free(expected_str);
            self.vm.allocator.free(actual_str);
        }

        error_message = try std.fmt.bufPrint(error_message, "{s}: expected type `{s}`, got `{s}`", .{ message, expected_str, actual_str });

        self.reportError(error_message);
    }

    fn advance(self: *Self) !void {
        self.parser.previous_token = self.parser.current_token;

        while (true) {
            self.parser.current_token = try self.scanner.?.scanToken();
            if (self.parser.current_token.?.token_type != .Error) {
                break;
            }

            self.reportErrorAtCurrent(self.parser.current_token.?.literal_string orelse "Unknown error.");
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) !void {
        if (self.parser.current_token.?.token_type == token_type) {
            try self.advance();
            return;
        }

        self.reportErrorAtCurrent(message);
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.parser.current_token.?.token_type == token_type;
    }

    fn match(self: *Self, token_type: TokenType) !bool {
        if (!self.check(token_type)) {
            return false;
        }

        try self.advance();

        return true;
    }

    fn endCompiler(self: *Self) !*ObjFunction {
        try self.emitReturn();

        var function: *ObjFunction = self.current.?.function;

        self.current = self.current.?.enclosing;

        return function;
    }

    inline fn beginScope(self: *Self) void {
        self.current.?.scope_depth += 1;
    }

    fn endScope(self: *Self) void {
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

    inline fn emitOpCode(self: *Self, code: OpCode) !void {
        try self.emitByte(@enumToInt(code));
    }

    fn emitByte(self: *Self, byte: u8) !void {
        try self.current.?.function.chunk.write(byte, self.parser.previous_token.?.line);
    }

    inline fn emitBytes(self: *Self, byte1: u8, byte2: u8) !void {
        try self.emitByte(byte1);
        try self.emitByte(byte2);
    }

    fn emitReturn(self: *Self) !void {
        if (self.current.?.function_type == .Initializer) {
            try self.emitBytes(@enumToInt(OpCode.OP_RETURN), 0);
        } else {
            try self.emitOpCode(.OP_NULL);
        }

        try self.emitOpCode(.OP_RETURN);
    }

    // AST NODES

    fn declaration(self: *Self) !void {
        // Things we can match with the first token
        if (try self.match(.Class)) {
            // self.classDeclaration();
        } else if (try self.match(.Object)) {
            // self.objectDeclaration();
        } else if (try self.match(.Enum)) {
            // self.enumDeclaration();
        } else if (try self.match(.Fun)) {
            try self.funDeclaration();
        } else if (try self.match(.Str)) {
            try self.varDeclaration(try self.vm.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .String }));
        } else if (try self.match(.Num)) {
            try self.varDeclaration(try self.vm.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Number }));
        } else if (try self.match(.Byte)) {
            try self.varDeclaration(try self.vm.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Byte }));
        } else if (try self.match(.Bool)) {
            try self.varDeclaration(try self.vm.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Bool }));
        } else if (try self.match(.Type)) {
            try self.varDeclaration(try self.vm.getTypeDef(.{ .optional = try self.match(.Question), .def_type = .Type }));
        } else if (try self.match(.LeftBracket)) {
            // self.listDeclaraction();
        } else if (try self.match(.LeftBrace)) {
            // self.mapDeclaraction();
        } else if (try self.match(.Function)) {
            // self.funVarDeclaraction();
        // TODO: matching a identifier here will prevent from parsing a statement that starts with an identifier (function call)
        // } else if ((try self.match(.Identifier))) {
        //     if (self.check(.Identifier)) {
        //         // TODO: instance declaration, needs to retrieve the *ObjTypeDef
        //     }
        } else {
            try self.statement();
        }
    }

    fn statement(self: *Self) !void {
        // TODO: remove
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.Return)) {
            try self.returnStatement();
        } else {
            try self.expressionStatement();
        }
    }

    // TODO: remove
    fn printStatement(self: *Self) !void {
        _ = try self.expression(false);
        try self.consume(.Semicolon, "Expected `;` after value.");
        try self.emitOpCode(.OP_PRINT);
    }

    fn returnStatement(self: *Self) !void {
        // TODO: we allow return from top-level, but in that case we have to not emit the last OP_RETURN

        if (try self.match(.Semicolon)) {
            try self.emitReturn();
        } else {
            if (self.current.?.function_type == .Initializer) {
                self.reportError("Can't return a value from an initializer.");
            }

            var return_type: *ObjTypeDef= try self.expression(false);
            if (!self.current.?.function.return_type.eql(return_type)) {
                try self.reportTypeCheck(self.current.?.function.return_type, return_type, "Return value");
            }

            try self.consume(.Semicolon, "Expected `;` after return value.");
            try self.emitOpCode(.OP_RETURN);
        }
    }

    fn parseTypeDef(self: *Self) anyerror!*ObjTypeDef {
        if (try self.match(.Str)) {
            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .String
            });
        } else if (try self.match(.Num)) {
            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Number
            });
        } else if (try self.match(.Byte)) {
            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Byte
            });
        } else if (try self.match(.Bool)) {
            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Bool
            });
        } else if (try self.match(.Type)) {
            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .Type
            });
        } else if (try self.match(.LeftBracket)) {
            var item_type: *ObjTypeDef = try self.parseTypeDef();

            try self.consume(.RightBracket, "Expected `]` to end list type.");

            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = ObjTypeDef.TypeUnion{
                    .List = ObjTypeDef.ListDef {
                        .item_type = item_type
                    }
                }
            });
        } else if (try self.match(.LeftBrace)) {
            var key_type: *ObjTypeDef = try self.parseTypeDef();

            try self.consume(.Comma, "Expected `,` after map key type.");

            var value_type: *ObjTypeDef = try self.parseTypeDef();

            try self.consume(.RightBrace, "Expected `}}` to end map type.");

            return try self.vm.getTypeDef(.{
                .optional = try self.match(.Question),
                .def_type = .List,
                .resolved_type = ObjTypeDef.TypeUnion{
                    .Map = ObjTypeDef.MapDef {
                        .key_type = key_type,
                        .value_type = value_type,
                    }
                }
            });
        } else if (try self.match(.Function)) {
            unreachable;
        } else if ((try self.match(.Identifier))) {
            // TODO: here search for a local with that name end check it's a class/object/enum
            unreachable;
        } else {
            self.reportErrorAtCurrent("Expected type definition.");

            return try self.vm.getTypeDef(.{
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
            self.reportError("Expect expression");

            // TODO: find a way to continue or catch that error
            return CompileError.Unrecoverable;
        }

        var canAssign: bool = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        var parsed_type: *ObjTypeDef = try prefixRule.?(self, canAssign);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.parser.current_token.?.token_type).precedence)) {
            _ = try self.advance();
            var infixRule: InfixParseFn = getRule(self.parser.previous_token.?.token_type).infix.?;
            parsed_type = try infixRule(self, canAssign, parsed_type);
        }

        if (canAssign and (try self.match(.Equal))) {
            self.reportError("Invalid assignment target.");
        }

        return parsed_type;
    }

    fn expression(self: *Self, hanging: bool) !*ObjTypeDef {
        return try self.parsePrecedence(.Assignment, hanging);
    }

    fn block(self: *Self) anyerror!void {
        while (!self.check(.RightBrace) and !self.check(.Eof)) {
            try self.declaration();
        }

        try self.consume(.RightBrace, "Expected `}}` after block.");
    }

    fn function(self: *Self, function_type: FunctionType) !*ObjTypeDef {
        try ChunkCompiler.init(self, function_type, null);
        var compiler: *ChunkCompiler = self.current.?;
        self.beginScope();

        try self.consume(.LeftParen, "Expected `(` after function name.");

        var parameters = std.StringArrayHashMap(*ObjTypeDef).init(self.vm.allocator);
        var arity: usize = 0;
        if (!self.check(.RightParen)) {
            while (true) {
                arity += 1;
                if (arity > 255) {
                    self.reportErrorAtCurrent("Can't have more than 255 parameters.");
                }

                var param_type: *ObjTypeDef = try self.parseTypeDef();
                var slot: usize = try self.parseVariable(param_type, "Expected parameter name");

                if (self.current.?.scope_depth > 0) {
                    var local: Local = self.current.?.locals[slot];
                    try parameters.put(local.name.string, local.type_def);
                } else {
                    var global: Global = self.globals.items[slot];
                    try parameters.put(global.name.string, global.type_def);
                }

                try self.defineVariable();

                if (!try self.match(.Comma)) break;
            }
        }

        try self.consume(.RightParen, "Expected `)` after function parameters.");
        
        var return_type: *ObjTypeDef = undefined;
        if (try self.match(.Greater)) {
            return_type = try self.parseTypeDef();
        } else {
            return_type = try self.vm.getTypeDef(
                .{
                    .optional = false,
                    .def_type = .Void
                }
            );
        }

        self.current.?.function.return_type = return_type;

        try self.consume(.LeftBrace, "Expected `{{` before function body.");
        try self.block();

        var new_function: *ObjFunction = try self.endCompiler();

        try self.emitBytes(@enumToInt(OpCode.OP_CLOSURE), try self.makeConstant(Value { .Obj = new_function.toObj() }));

        var i: usize = 0;
        while (i < new_function.upvalue_count) : (i += 1) {
            try self.emitByte(if (compiler.upvalues[i].is_local) 1 else 0);
            try self.emitByte(compiler.upvalues[i].index);
        }

        var function_typedef: ObjTypeDef = .{
            .optional = false,
            .def_type = .Function,
        };

        var function_def: ObjTypeDef.FunctionDef = .{
            .return_type = return_type,
            .parameters = parameters,
        };

        var function_resolved_type: ObjTypeDef.TypeUnion = .{
            .Function = function_def
        };

        function_typedef.resolved_type = function_resolved_type;

        return try self.vm.getTypeDef(function_typedef);
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

        var function_def: *ObjTypeDef = try self.function(FunctionType.Function);
        // Now that we have the full function type, get the local and update its type_def
        if (self.current.?.scope_depth > 0) {
            self.current.?.locals[slot].type_def = function_def;
        } else {
            self.globals.items[slot].type_def = function_def;
        }

        try self.defineVariable();
    }

    fn varDeclaration(self: *Self, var_type: *ObjTypeDef) !void {
        _ = try self.parseVariable(var_type, "Expected variable name.");

        if (try self.match(.Equal)) {
            var expr_type: *ObjTypeDef = try self.expression(false);

            if (!var_type.eql(expr_type)) {
                try self.reportTypeCheck(var_type, expr_type, "Wrong variable type");
            }
        } else {
            try self.emitOpCode(.OP_NULL);
        }

        try self.consume(.Semicolon, "Expected `;` after variable declaration.");

        try self.defineVariable();
    }

    fn expressionStatement(self: *Self) !void {
        _ = try self.expression(false);
        try self.consume(.Semicolon, "Expected `;` after expression.");
        try self.emitOpCode(.OP_POP);
    }

    inline fn defineVariable(self: *Self) !void {
        self.markInitialized();

        if (self.current.?.scope_depth > 0) {
            return;
        }

        try self.emitOpCode(.OP_DEFINE_GLOBAL);
    }

    const ParsedArg = struct {
        name: ?Token,
        arg_type: *ObjTypeDef,
    };

    fn argumentList(self: *Self, function_type: *ObjTypeDef) !u8 {
        var arg_count: u8 = 0;
        var function_def: ObjTypeDef.FunctionDef = function_type.resolved_type.?.Function;
        var parameters: std.StringArrayHashMap(*ObjTypeDef) = function_def.parameters;
        var parameter_keys: [][]const u8 = parameters.keys();
        
        if (!self.check(.RightParen)) {
            var parsed_arguments = std.ArrayList(ParsedArg).init(self.vm.allocator);
            defer parsed_arguments.deinit();

            while (arg_count < parameter_keys.len) {
                var hanging = false;
                var arg_name: ?Token = null;
                if (try self.match(.Identifier)) {
                    arg_name = self.parser.previous_token.?;
                }

                if (arg_count != 0 and arg_name == null) {
                    self.reportError("Expected argument name.");
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
                    .name = arg_name,
                    .arg_type = expr_type,
                });

                if (arg_count == 255) {
                    self.reportError("Can't have more than 255 arguments.");
                    
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
                            var wrong_type_str: []u8 = try self.vm.allocator.alloc(u8, 100);
                            var param_type_str: []const u8 = try ptype.toString(self.vm.allocator);
                            var expr_type_str: []const u8 = try argument.arg_type.toString(self.vm.allocator);
                            defer {
                                self.vm.allocator.free(wrong_type_str);
                                self.vm.allocator.free(param_type_str);
                                self.vm.allocator.free(expr_type_str);
                            }

                            self.reportError(
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
                        var wrong_name_str: []u8 = try self.vm.allocator.alloc(u8, 100);
                        defer self.vm.allocator.free(wrong_name_str);

                        self.reportError(
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
                        var wrong_type_str: []u8 = try self.vm.allocator.alloc(u8, 100);
                        var param_type_str: []const u8 = try param_type.toString(self.vm.allocator);
                        var expr_type_str: []const u8 = try argument.arg_type.toString(self.vm.allocator);
                        defer {
                            self.vm.allocator.free(wrong_type_str);
                            self.vm.allocator.free(param_type_str);
                            self.vm.allocator.free(expr_type_str);
                        }
                        var name: []const u8 = parameter_keys[0];

                        self.reportError(
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
                var already_swapped = std.AutoHashMap(u8, u8).init(self.vm.allocator);
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

                                try self.emitOpCode(.OP_SWAP);
                                // from where it is on the stack
                                try self.emitByte(@intCast(u8, index));
                                // to where it should be
                                try self.emitByte(@intCast(u8, pindex));

                                try already_swapped.put(@intCast(u8, index), @intCast(u8, pindex));
                            }
                        }
                    }
                }
            }
        }

        if (parameter_keys.len != arg_count) {
            var arity: []u8 = try self.vm.allocator.alloc(u8, 100);
            defer self.vm.allocator.free(arity);

            self.reportError(try std.fmt.bufPrint(arity, "Expected {} arguments, got {}", .{ parameter_keys.len, arg_count }));

            return 0;
        }

        try self.consume(.RightParen, "Expected `)` after arguments.");
        return arg_count;
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

    fn string(self: *Self, _: bool) anyerror!*ObjTypeDef {
        try self.emitConstant(Value {
            .Obj = (try _obj.copyString(self.vm, self.parser.previous_token.?.literal_string.?)).toObj()
        });

        return try self.vm.getTypeDef(.{
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
                arg = try self.resolveGlobal(name);
                if (arg) |resolved| {
                    var_def = self.globals.items[resolved].type_def;

                    get_op = .OP_GET_GLOBAL;
                    set_op = .OP_SET_GLOBAL;
                } else {
                    var error_str: []u8 = try self.vm.allocator.alloc(u8, name.lexeme.len + 1000);
                    defer self.vm.allocator.free(error_str);
                    error_str = try std.fmt.bufPrint(error_str, "`{s}` is not defined\x00", .{ name.lexeme });

                    self.reportError(error_str);

                    return CompileError.Unrecoverable;
                }
            }
        }

        if (can_assign and try self.match(.Equal)) {
            var expr_type: *ObjTypeDef = try self.expression(false);

            if (!expr_type.eql(var_def)) {
                try self.reportTypeCheck(var_def, expr_type, "Wrong value type");
            }

            try self.emitBytes(@enumToInt(set_op), @intCast(u8, arg.?));
        } else {
            try self.emitBytes(@enumToInt(get_op), @intCast(u8, arg.?));
        }

        return var_def;
    }

    fn variable(self: *Self, can_assign: bool) anyerror!*ObjTypeDef {
        return try self.namedVariable(self.parser.previous_token.?, can_assign);
    }

    fn call(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!*ObjTypeDef {
        var arg_count: u8 = try self.argumentList(expected_type);
        try self.emitBytes(@enumToInt(OpCode.OP_CALL), arg_count);

        return expected_type.resolved_type.?.Function.return_type;
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

                return try self.vm.getTypeDef(.{
                    .def_type = .Bool,
                    .optional = false,
                });
            },
            .True => {
                try self.emitOpCode(.OP_TRUE);

                return try self.vm.getTypeDef(.{
                    .def_type = .Bool,
                    .optional = false,
                });
            },
            .Null => {
                try self.emitOpCode(.OP_NULL);

                return try self.vm.getTypeDef(.{
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

        return try self.vm.getTypeDef(.{
            .def_type = .Number,
            .optional = false,
        });
    }

    fn byte(self: *Self, _: bool) anyerror!*ObjTypeDef {
        var value: u8 = self.parser.previous_token.?.literal_byte.?;

        try self.emitConstant(Value{ .Byte = value });

        return try self.vm.getTypeDef(.{
            .def_type = .Byte,
            .optional = false,
        });
    }

    fn emitConstant(self: *Self, value: Value) !void {
        try self.emitBytes(@enumToInt(OpCode.OP_CONSTANT), try self.makeConstant(value));
    }

    // LOCALS

    fn addLocal(self: *Self, name: Token, local_type: *ObjTypeDef) !usize {
        if (self.current.?.local_count == 255) {
            self.reportError("Too many local variables in scope.");
            return 0;
        }

        self.current.?.locals[self.current.?.local_count] = Local{
            .name = try _obj.copyString(self.vm, name.lexeme),
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
        };

        self.current.?.local_count += 1;

        return self.current.?.local_count - 1;
    }

    fn addGlobal(self: *Self, name: Token, global_type: *ObjTypeDef) !usize {
        if (self.globals.items.len == 255) {
            self.reportError("Too many global variables.");
            return 0;
        }

        try self.globals.append(Global{
            .name = try _obj.copyString(self.vm, name.lexeme),
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
                    self.reportError("Can't read local variable in its own initializer.");
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
                    self.reportError("Can't read global variable in its own initializer.");
                }

                return i;
            }

            if (i == 0) break;
        }

        return null;
    }

    fn addUpvalue(self: *Self, compiler: *ChunkCompiler, index: usize, is_local: bool) usize {
        var upvalue_count: u8 = compiler.function.upvalue_count;

        var i: usize = 0;
        while (i < upvalue_count) : (i += 1) {
            var upvalue: *UpValue = &compiler.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return i;
            }
        }

        if (upvalue_count == 255) {
            self.reportError("Too many closure variables in function.");
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
            return self.addUpvalue(compiler, resolved, true);
        }

        var upvalue: ?usize = try self.resolveUpvalue(compiler.enclosing.?, name);
        if (upvalue) |resolved| {
            return self.addUpvalue(compiler, resolved, false);
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
                    self.reportError("A variable with the same name already exists in this scope.");
                }

                if (i == 0) break;
            }

            return try self.addLocal(name, variable_type);
        } else {
            // Check a global with the same name doesn't exists
            for (self.globals.items) |global| {
                if (mem.eql(u8, name.lexeme, global.name.string)) {
                    self.reportError("A global with the same name already exists.");
                }
            }

            return try self.addGlobal(name, variable_type);
        }
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        var constant: u8 = try self.current.?.function.chunk.addConstant(self.vm, value);
        if (constant > _chunk.Chunk.max_constants) {
            self.reportError("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn identifierConstant(self: *Self, name: *const Token) !u8 {
        return try self.makeConstant(Value{ .Obj = (try _obj.copyString(self.vm, name.lexeme)).toObj() });
    }
};
