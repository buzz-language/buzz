const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

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
const Token = _token.Token;
const TokenType = _token.TokenType;
const Scanner = @import("./scanner.zig").Scanner;
const Value = _value.Value;

pub const FunctionType = enum {
    Function,
    Initializer,
    Method,
    Script
};

pub const Local = struct {
    name: Token,
    type_def: *ObjTypeDef,
    depth: i32,
    is_captured: bool
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

    pub fn init(compiler: *Compiler, function_type: FunctionType, file_name: ?[]const u8) !Self {
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
        // TODO: figure out from where we can get the return_type and parameters
        try compiler.vm.getTypeDef(.{
            .def_type = .Void,
            .optional = false,
        }));

        compiler.current = try compiler.vm.allocator.create(ChunkCompiler);
        compiler.current.?.* = self;

        // First local is reserved for an eventual `this`
        var local: *Local = &self.locals[self.local_count];
        self.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        // TODO: when do we define, `this` typedef ?
        local.type_def = try compiler.vm.getTypeDef(.{
            .def_type = .Void,
            .optional = false,
        });

        local.name = Token{
            .token_type = .String,
            .lexeme = if (function_type == .Function) VM.this_string else VM.empty_string,
            .literal_string = if (function_type == .Function) VM.this_string else VM.empty_string,
            .line = 0,
            .column = 0,
        };

        return self;
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

    const ParseFn = fn (*Compiler, bool, *ObjTypeDef) anyerror!void;

    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?ParseFn,
        precedence: Precedence,
    };

    const rules = [_]ParseRule{
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Pipe
        .{ .prefix = null,     .infix = null, .precedence = .None }, // LeftBracket
        .{ .prefix = null,     .infix = null, .precedence = .None }, // RightBracket
        .{ .prefix = grouping, .infix = null, .precedence = .Call }, // LeftParen
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
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Identifier
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Fun
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Object
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Class
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Enum
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Eof
        .{ .prefix = null,     .infix = null, .precedence = .None }, // Error
    };

    vm: *VM,

    scanner: ?Scanner = null,
    parser: ParserState = .{},
    current: ?*ChunkCompiler = null,
    current_class: ?*ClassCompiler = null,

    pub fn init(vm: *VM) Self {
        return .{
            .vm = vm,
        };
    }

    // TODO: walk the chain of compiler and destroy them in deinit

    pub fn compile(self: *Self, source: []const u8, file_name: ?[]const u8) !?*ObjFunction {
        if (self.scanner != null) {
            self.scanner = null;
        }

        self.scanner = Scanner.init(source);
        defer self.scanner = null;

        _ = try ChunkCompiler.init(self, .Script, file_name);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        try self.advance();

        // Enter AST
        while (!(try self.match(.Eof))) {
            try self.declaration();
        }

        var function: *ObjFunction = try self.endCompiler();

        return if (self.parser.had_error) null else function;
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8) void {
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
        self.errorAt(&self.parser.previous_token.?, message);
    }

    fn reportErrorAtCurrent(self: *Self, message: []const u8) void {
        self.errorAt(&self.parser.current_token.?, message);
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
            // self.funDeclaration();
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
        } else if ((try self.match(.Identifier))) {
            if (self.check(.Identifier)) {
                // TODO: instance declaration, needs to retrieve the *ObjTypeDef
            }
        } else {
            // self.statement();
        }
    }

    inline fn getRule(token: TokenType) ParseRule {
        return rules[@enumToInt(token)];
    }

    fn parsePrecedence(self: *Self, precedence: Precedence, expected_type: *ObjTypeDef) !void {
        _ = try self.advance();

        var prefixRule: ?ParseFn = getRule(self.parser.previous_token.?.token_type).prefix;
        if (prefixRule == null) {
            self.reportError("Expect expression");
            return;
        }

        var canAssign: bool = @enumToInt(precedence) <= @enumToInt(Precedence.Assignment);
        try prefixRule.?(self, canAssign, expected_type);

        while (@enumToInt(precedence) <= @enumToInt(getRule(self.parser.current_token.?.token_type).precedence)) {
            _ = try self.advance();
            var infixRule: ParseFn = getRule(self.parser.previous_token.?.token_type).infix.?;
            try infixRule(self, canAssign, expected_type);
        }

        if (canAssign and (try self.match(.Equal))) {
            self.reportError("Invalid assignment target.");
        }
    }

    fn expression(self: *Self, expected_type: *ObjTypeDef) !void {
        try self.parsePrecedence(.Assignment, expected_type);
    }

    fn varDeclaration(self: *Self, var_type: *ObjTypeDef) !void {
        // var var_type: TokenType = self.parser.previous_token;

        var global: u8 = try self.parseVariable(var_type, "Expected variable name.");

        if (try self.match(.Equal)) {
            try self.expression(var_type);
        } else {
            try self.emitOpCode(.OP_NULL);
        }

        try self.consume(.Semicolon, "Expected `;` after variable declaration.");

        try self.defineVariable(global);
    }

    fn unary(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        var operator_type: TokenType = self.parser.previous_token.?.token_type;
        
        try self.parsePrecedence(.Unary, expected_type);

        switch (operator_type) {
            .Bang => try self.emitOpCode(.OP_NOT),
            .Minus => try self.emitOpCode(.OP_NEGATE),
            else => {},
        }
    }

    fn string(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        if (expected_type.def_type != .String) {
            self.reportError("Expected type str.");
        }

        try self.emitConstant(Value {
            .Obj = (try _obj.copyString(self.vm, self.parser.previous_token.?.literal_string.?)).toObj()
        });
    }

    fn grouping(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        try self.expression(expected_type);
        try self.consume(.RightParen, "Expected ')' after expression.");
    }

    fn literal(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        switch (self.parser.previous_token.?.token_type) {
            .False => {
                if (expected_type.def_type != .Bool) {
                    self.reportError("Expected type bool.");
                }

                try self.emitOpCode(.OP_FALSE);
            },
            .True => {
                if (expected_type.def_type != .Bool) {
                    self.reportError("Expected type bool.");
                }

                try self.emitOpCode(.OP_TRUE);
            },
            .Null => try self.emitOpCode(.OP_NULL),
            else => return,
        }
    }

    fn number(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        if (expected_type.def_type != .Number) {
            self.reportError("Expected type num.");
        }

        var value: f64 = self.parser.previous_token.?.literal_number.?;

        try self.emitConstant(Value{ .Number = value });
    }

    fn byte(self: *Self, _: bool, expected_type: *ObjTypeDef) anyerror!void {
        if (expected_type.def_type != .Byte) {
            self.reportError("Expected type num.");
        }

        var value: u8 = self.parser.previous_token.?.literal_byte.?;

        try self.emitConstant(Value{ .Byte = value });
    }

    fn emitConstant(self: *Self, value: Value) !void {
        try self.emitBytes(@enumToInt(OpCode.OP_CONSTANT), try self.makeConstant(value));
    }

    // LOCALS

    fn addLocal(self: *Self, name: Token, local_type: *ObjTypeDef) !void {
        if (self.current.?.local_count == 255) {
            self.reportError("Too many local variables in scope.");
            return;
        }

        self.current.?.locals[self.current.?.local_count] = Local{
            .name = name,
            .depth = -1,
            .is_captured = false,
            .type_def = local_type,
        };
    }

    fn resolveLocal(self: *Self, compiler: *ChunkCompiler, name: *Token) ?usize {
        var i: usize = compiler.local_count;
        while (i >= 0) {
            var local: *Local = &compiler.locals[i];
            if (self.identifiersEqual(name, &local.name)) {
                if (local.depth == -1) {
                    self.reportError("Can't read local variable in its own initializer.");
                }

                return i;
            }

            i -= 1;
        }

        return null;
    }

    fn identifiersEqual(a: *Token, b: *Token) bool {
        if (a.lexeme.len != b.lexeme.len) {
            return false;
        }

        return mem.eql(u8, a.lexeme, b.lexeme);
    }

    // VARIABLES

    fn parseVariable(self: *Self, variable_type: *ObjTypeDef, error_message: []const u8) !u8 {
        try self.consume(.Identifier, error_message);

        try self.declareVariable(variable_type);

        if (self.current.?.scope_depth > 0) {
            return 0;
        }

        return try self.identifierConstant(&self.parser.previous_token.?);
    }

    fn markInitialized(self: *Self) void {
        if (self.current.?.scope_depth == 0) {
            return;
        }

        self.current.?.locals[self.current.?.local_count - 1].depth = @intCast(i32, self.current.?.scope_depth);
    }

    fn defineVariable(self: *Self, global: u8) !void {
        if (self.current.?.scope_depth > 0) {
            self.markInitialized();
            return;
        }

        try self.emitBytes(@enumToInt(OpCode.OP_DEFINE_GLOBAL), global);
    }

    fn declareVariable(self: *Self, variable_type: *ObjTypeDef) !void {
        if (self.current.?.scope_depth == 0) {
            return;
        }

        var name: *Token = &self.parser.previous_token.?;

        // Check a local with the same name doesn't exists
        var i: usize = self.current.?.locals.len - 1;
        while (i >= 0) {
            var local: *Local = &self.current.?.locals[i];

            if (local.depth != -1 and local.depth < self.current.?.scope_depth) {
                break;
            }

            if (identifiersEqual(name, &local.name)) {
                self.reportError("A variable with the same name already exists in this scope.");
            }

            if (i > 0) i -= 1 else break;
        }

        try self.addLocal(name.*, variable_type);
    }

    fn makeConstant(self: *Self, value: Value) !u8 {
        var constant: u8 = try self.current.?.function.chunk.addConstant(self.vm, value);
        if (constant > _chunk.Chunk.max_constants) {
            self.reportError("Too many constants in one chunk.");
            return 0;
        }

        return constant;
    }

    fn identifierConstant(self: *Self, name: *Token) !u8 {
        return try self.makeConstant(Value{ .Obj = (try _obj.copyString(self.vm, name.lexeme)).toObj() });
    }
};
