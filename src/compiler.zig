const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const _obj = @import("./obj.zig");
const ObjFunction = _obj.ObjFunction;
const Token = @import("./token.zig").Token;
const Scanner = @import("./scanner.zig").Scanner;

pub const FunctionType = enum {
  Function,
  Initializer,
  Method,
  Script
};

pub const Local = struct {
   name: Token, 
   depth: u64,
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

pub const ChunckCompiler = struct {
    const Self = @This();

    enclosing: ?*ChunckCompiler = null,
    function: *ObjFunction = null,
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
    hadError: bool = false,
    panicMode: bool = false,
};

pub const Compiler = struct {
    const Self = @This();

    allocator: *Allocator,

    scanner: ?Scanner = null,
    parser: ParserState = .{},
    current: ?*ChunckCompiler = null,  
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

        _ = try ChunckCompiler.init(allocator, self, .Script);

        self.parser.hadError = false;
        self.parser.panicMode = false;
    }

    fn advance(self: *Self) !void {
        self.parser.previous_token = self.parser.current_token;

        
    }
};