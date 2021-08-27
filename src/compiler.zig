const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const _chunk = @import("./chunk.zig");
const _obj = @import("./obj.zig");
const _token = @import("./token.zig");

const OpCode = _chunk.OpCode;
const ObjFunction = _obj.ObjFunction;
const ObjTypeDef = _obj.ObjTypeDef;
const Token = _token.Token;
const TokenType = _token.TokenType;
const Scanner = @import("./scanner.zig").Scanner;

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

const this_string: [4]u8 = "this".*;
const empty_string: [0]u8 = "".*;

pub const ChunkCompiler = struct {
    const Self = @This();

    enclosing: ?*ChunkCompiler = null,
    function: *ObjFunction,
    function_type: FunctionType,

    locals: std.ArrayList(Local),

    upvalues: std.ArrayList(UpValue),
    scope_depth: u32 = 0,

    pub fn init(allocator: *Allocator, compiler: *Compiler, function_type: FunctionType) !Self {
        var self: Self = .{
            .enclosing = compiler.current,
            .function_type = function_type,
            .function = try allocator.create(ObjFunction),
            .locals = std.ArrayList(Local).init(allocator),
            .upvalues = std.ArrayList(UpValue).init(allocator),
        };

        self.function.* = ObjFunction.init(allocator);
        self.function.name = _obj.copyString(vm: *VM, chars: []u8)

        compiler.current = self;

        if (function_type != .Script) {
            compiler.current.?.function.name = compiler.parser.previous_token.copyStringLiteral(allocator);
        }

        // First local is reserved for an eventual `this`
        var local: Local = self.locals.items[0];
        local.depth = 0;
        local.is_captured = false;

        local.name = .{
            .token_type = .String,
            .lexeme = if (function_type == .Function) &this_string orelse &empty_string,
            .literal_string = if (function_type == .Function) &this_string orelse &empty_string,
            .line = 0,
            .column = 0,
        };

        return self;
    }

    pub fn deinit(self: *Self) void {

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

    allocator: *Allocator,

    scanner: ?Scanner = null,
    parser: ParserState = .{},
    current: ?*ChunkCompiler = null,  
    current_class: ?*ClassCompiler = null,

    pub fn init(allocator: *Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
    }

    pub fn compile(self: *Self, source: []u8) !?ObjFunction {
        if (self.scanner) |scanner| {
            scanner.deinit();
            self.scanner = null;
        }

        self.scanner = Scanner.init(self.allocator, source);
        defer {
            scanner.deinit();
            self.scanner = null;
        }

        _ = try ChunkCompiler.init(allocator, self, .Script);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.advance();

        // Enter AST
        while (!self.match(.Eof)) {
            try self.declaration();
        }

        var function: *ObjFunction = self.endCompiler();

        return if (self.parser.had_error) null else function;
    }

    fn errorAt(self: *Self, token: *Token, message: []const u8) void {
        if (self.parser.panic_mode) {
            return;
        }

        self.parser.panic_mode = true;

        std.debug.warn("[{}:{}] Error", .{ token.line, token.column });

        if (token.token_type == .Eof) {
            std.debug.warn(" at end");
        } else if (token.token_type != .Error) { // We report error to the token just before a .Error token
            std.debug.warn(" at '{s}'", .{ token.lexeme });
        }

        std.debug.warn(": {s}\n", .{ message });

        self.parser.had_error = true;
    }

    fn reportError(self: *Self, message: []const u8) void {
        errorAt(&self.parser.previous_token, message);
    }

    fn reportErrorAtCurrent(self: *Self, message: []const u8) void {
        errorAt(&self.parser.current_token, message);
    }

    fn advance(self: *Self) void {
        self.parser.previous_token = self.parser.current_token;

        while (true) {
            parser.current_token = self.scanner.scanToken();
            if (parser.current_token.token_type != .Error) {
                break;
            }

            reportErrorAtCurrent(self.parser.current_token.literal_string orelse "Unknown error.");
        }
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) void {
        if (self.parser.current_token.token_type == token_type) {
            self.advance();
            return;
        }
        
        self.reportErrorAtCurrent(message);
    }

    fn check(self: *Self, token_type: TokenType) bool {
        return self.parser.current_token.token_type == token_type;
    }

    fn match(self: *Self, token_type: TokenType) bool {
        if (!self.check(token_type)) {
            return false;
        }

        self.advance();

        return true;
    }

    fn endCompiler(self: *Self) void {
        self.emitReturn();

        var function: *ObjFunction = self.current.function;

        self.current = self.current.enclosing;

        return function;
    }

    // BYTE EMITTING

    fn emitOpCode(code: OpCode) callconv(.Inline) void {
        self.emitByte(@enumToInt(code));
    }

    fn emitByte(byte: u8) void {
        self.current.function.chunk.write(byte, self.parser.previous.line);
    }

    fn emitBytes(byte1: u8, byte2: u8) callconv(.Inline) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    fn emitReturn(self: *Self) void {
        if (self.current.function_type == .Initializer) {
            self.emitBytes(@enumToInt(OpCode.OP_RETURN), 0);
        } else {
            self.emitOpCode(.OP_NULL);
        }

        self.emitOpCode(.OP_RETURN);
    }

    // AST NODES

    fn declaration(self: *Self) !void {
        // Things we can match with the first token
        if (self.match(.Class)) {
            self.classDeclaration();
        } else if (self.match(.Object)) {
            self.objectDeclaration();
        } else if (self.match(.Enum)) {
            self.enumDeclaration();
        } else if (self.match(.Fun)) {
            self.funDeclaration();
        } else if (self.match(.Str)
            or self.match(.Num)
            or self.match(.Byte)
            or self.match(.Bool)
            or self.match(.Type)) {
            try self.varDeclaration();
        } else if (self.match(.LeftBracket)) {
            self.listDeclaraction();
        } else if (self.match(.LeftBrace)) {
            self.mapDeclaraction();
        } else if (self.match(.Function)) {
            self.funVarDeclaraction();
        } else if (self.match(.Identifier) and self.check(.Identifier)) {
            self.instanceDeclaration();
        } else {
            self.statement();
        }
    }

    fn classDeclaration(self: *Self) void {
        unreachable;
    }

    fn objectDeclaration(self: *Self) void {
        unreachable;
    }

    fn enumDeclaration(self: *Self) void {
        unreachable;
    }

    fn funDeclaration(self: *Self) void {
        unreachable;
    }

    fn varDeclaration(self: *Self) !void {
        var var_type: TokenType = self.parser.previous_token;

        var global: u8 = try self.parseVariable("Expected variable name.");

        if (self.match(.Equal)) {
            self.expression();
        } else {
            self.emitOpCode(.OP_NULL);
        }

        self.consume(.Semicolon, "Expected `;` after variable declaration.");

        self.defineVariable(global);
    }

    fn listDeclaration(self: *Self) void {
        unreachable;
    }

    fn mapDeclaration(self: *Self) void {
        unreachable;
    }

    fn funVarDeclaration(self: *Self) void {
        unreachable;
    }

    fn instanceDeclaration(self: *Self) void {
        unreachable;
    }

    fn statement(self: *Self) void {
        unreachable;
    }

    fn expression(self: *Self) void {

    }

    // LOCALS

    fn addLocal(self: *Self, name: Token) !void {
        if (self.current.locals.items.len == 255) {
            self.reportError("Too many local variables in scope.");
            return;
        }

        try self.current.locals.append(.{
            .name = name,
            .depth = -1,
            .is_captured = false,
        });
    }

    fn identifiersEqual(a: *Token, b: *Token) bool {
        if (a.lexeme.len != b.lexeme.len) {
            return false;
        }

        return mem.eql(u8, a.lexeme, b.lexeme);
    }

    // VARIABLES

    fn parseVariable(self: *Self, error_message: []const u8) !u8 {
        self.consume(.Identifier, error_message);

        try self.declareVariable();

        if (self.current.scope_depth > 0) {
            return 0;
        }
        
        return self.identifierConstant(&self.parser.previous_token);
    }

    fn defineVariable(self: *Self, global: u8) void {
        unreachable;
    }

    fn declareVariable(self: *Self) !void {
        if (self.current.scope_depth == 0) {
            return;
        }

        var name: *Token = &self.parser.previous_token;

        // Check a local with the same name doesn't exists
        var i: usize = self.current.locals.len - 1;
        while (i >= 0) {
            var local: *Local = &self.current.locals[i];

            if (local.depth != -1 and local.depth < self.current.scope_depth) {
                break;
            }

            if (self.identifiersEqual(name, &local.name)) {
                self.reportError("A variable with the same name already exists in this scope.");
            }

            if (i > 0) i -= 1 else break;
        }

        try self.addLocal(name.*);
    }

    fn identifierConstant(self: *Self, name: *Token) u8 {
        unreachable;
    }
};