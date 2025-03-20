const std = @import("std");
const assert = std.debug.assert;
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");

pub fn Renderer(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        ast: Ast.Slice,
        ais: *AutoIndentingStream(T),

        indent: u16 = 0,

        pub const Error = error{
            OutOfMemory,
            NoSpaceLeft,
            InvalidArgument,
            DiskQuota,
            FileTooBig,
            InputOutput,
            DeviceBusy,
            AccessDenied,
            BrokenPipe,
            SystemResources,
            OperationAborted,
            NotOpenForWriting,
            LockViolation,
            WouldBlock,
            ConnectionResetByPeer,
            ProcessNotFound,
            NoDevice,
            Unexpected,
        };

        const RenderNode = *const fn (
            self: *Self,
            node: Ast.Node.Index,
            space: Space,
        ) Error!void;

        const Space = enum {
            /// Output the token lexeme only.
            None,
            /// Output the token lexeme followed by a single space.
            Space,
            /// Output the token lexeme followed by a newline.
            Newline,
            /// If the next token is a comma, render it as well. If not, insert one.
            /// In either case, a newline will be inserted afterwards.
            Comma,
            /// Additionally consume the next token if it is a comma.
            /// In either case, a space will be inserted afterwards.
            CommaSpace,
            /// Additionally consume the next token if it is a semicolon.
            /// In either case, a newline will be inserted afterwards.
            Semicolon,
            /// Skip rendering whitespace and comments. If this is used, the caller
            /// *must* handle whitespace and comments manually.
            Skip,
        };

        pub fn render(allocator: std.mem.Allocator, out: T, ast: Ast) Error!void {
            var ais = AutoIndentingStream(T).init(allocator, out, 4);
            defer ais.deinit();

            var self = Self{
                .allocator = allocator,
                .ast = ast.slice(),
                .ais = &ais,
            };

            if (ast.root == null) {
                return;
            }

            try self.renderNode(self.ast.root.?, .Newline);
        }

        inline fn renderNode(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            if (Self.renderers[@intFromEnum(self.ast.nodes.items(.tag)[node])]) |renderer| {
                return renderer(self, node, space);
            } else {
                std.debug.print("Dont know how to render {s}\n", .{@tagName(self.ast.nodes.items(.tag)[node])});
            }
        }

        const renderers = [_]?RenderNode{
            renderAnonymousObjectType,
            null, //renderAs, // As,
            null, //renderAsyncCall, // AsyncCall,
            null, //renderBinary, // Binary,
            renderBlock,
            null, //renderBlockExpression, // BlockExpression,
            renderBoolean,
            null, //renderBreak, // Break,
            null, //renderCall, // Call,
            null, //renderContinue, // Continue,
            null, //renderDot, // Dot,
            null, //renderDoUntil, // DoUntil,
            null, //renderEnum, // Enum,
            null, //renderExport, // Export,
            null, //renderExpression, // Expression,
            null, // FiberType,
            renderDouble,
            null, //renderFor, // For,
            null, //renderForceUnwrap, // ForceUnwrap,
            null, //renderForEach, // ForEach,
            renderFunction,
            renderFunctionType,
            renderFunDeclaration,
            null, //renderGenericResolve, // GenericResolve,
            null, // GenericResolveType,
            null, // GenericType,
            null, //renderGrouping, // Grouping,
            null, //renderIf, // If,
            null, //renderImport, // Import,
            renderInteger,
            null, //renderIs, // Is,
            null, //renderList, // List,
            null, // ListType,
            null, //renderMap, // Map,
            null, // MapType,
            null, // Namespace,
            null, //renderNamedVariable, // NamedVariable,
            renderNull,
            null, //renderObjectDeclaration, // ObjectDeclaration,
            renderObjectInit,
            null, //renderOut, // Out,
            null, //renderPattern, // Pattern,
            null, //renderProtocolDeclaration, // ProtocolDeclaration,
            null, //renderRange, // Range,
            null, //renderResolve, // Resolve,
            null, //renderResume, // Resume,
            renderReturn,
            renderSimpleType,
            renderString,
            renderStringLiteral,
            null, //renderSubscript, // Subscript,
            null, //renderThrow, // Throw,
            null, //renderTry, // Try,
            null, //renderTypeExpression, // TypeExpression,
            null, //renderTypeOfExpression, // TypeOfExpression,
            null, //renderUnary, // Unary,
            null, //renderUnwrap, // Unwrap,
            null, // UserType,
            null, //renderVarDeclaration, // VarDeclaration,
            renderVoid,
            null, //renderWhile, // While,
            null, //renderYield, // Yield,
            null, //renderZdef, // Zdef,
        };

        fn renderExpectedTokenSequence(self: *Self, start_token: Ast.TokenIndex, comptime expected: []const Token.Type, space: Space) Error!void {
            for (expected, 0..) |tag, offset| {
                try self.renderExpectedToken(
                    start_token + @as(Ast.TokenIndex, @intCast(offset)),
                    tag,
                    if (expected.len > 1 and offset < expected.len - 1)
                        .None
                    else
                        space,
                );
            }
        }

        fn dumpTokens(self: *Self) void {
            std.debug.print("\n", .{});
            for (self.ast.tokens.items(.lexeme), 0..) |lexeme, i| {
                std.debug.print("{}`{s}` ", .{ i, lexeme });
            }
            std.debug.print("\n", .{});
        }

        fn renderOneOfExpectedToken(self: *Self, token: Ast.TokenIndex, comptime expected: []const Token.Type, space: Space) Error!void {
            if (std.mem.indexOf(Token.Type, expected, &.{self.ast.tokens.items(.tag)[token]}) == null) {
                std.debug.print(
                    "\nGot {s} at {} `{s}`\n",
                    .{
                        @tagName(self.ast.tokens.items(.tag)[token]),
                        token,
                        self.ast.tokens.items(.lexeme)[token],
                    },
                );

                self.dumpTokens();
            }

            assert(std.mem.indexOf(Token.Type, expected, &.{self.ast.tokens.items(.tag)[token]}) != null);
            return self.renderToken(token, space);
        }

        fn renderExpectedToken(self: *Self, token: Ast.TokenIndex, expected: Token.Type, space: Space) Error!void {
            if (self.ast.tokens.items(.tag)[token] != expected) {
                std.debug.print(
                    "\nExpected {s} got {s} at {} `{s}`\n",
                    .{
                        @tagName(expected),
                        @tagName(self.ast.tokens.items(.tag)[token]),
                        token,
                        self.ast.tokens.items(.lexeme)[token],
                    },
                );

                self.dumpTokens();
            }

            assert(self.ast.tokens.items(.tag)[token] == expected);
            return self.renderToken(token, space);
        }

        fn renderToken(self: *Self, token: Ast.TokenIndex, space: Space) Error!void {
            const lexeme = self.ast.tokens.items(.lexeme)[token];
            try self.ais.writer().writeAll(lexeme);
            try self.renderSpace(
                token,
                lexeme.len,
                space,
            );
        }

        fn renderSpace(self: *Self, token: Ast.TokenIndex, lexeme_len: usize, space: Space) Error!void {
            if (space == .Skip) return;

            const next_token = if (self.ast.tokens.items(.utility_token)[token + 1])
                token + 2
            else
                token + 1;
            const next_token_tag = self.ast.tokens.items(.tag)[next_token];
            const offsets = self.ast.tokens.items(.offset);

            if (space == .Comma and next_token_tag != .Comma) {
                try self.ais.writer().writeByte(',');
            }

            if (space == .Semicolon or space == .Comma) self.ais.enableSpaceMode(space);
            defer self.ais.disableSpaceMode();

            if (offsets[next_token] < offsets[token] + lexeme_len) {
                std.debug.print(
                    "\nToken overlap: {s}#{} at {}-{} {} `{s}` and {s}#{} at {}-{} {} `{s}`\n",
                    .{
                        @tagName(self.ast.tokens.items(.tag)[token]),
                        token,
                        offsets[token],
                        offsets[token] + lexeme_len,
                        self.ast.tokens.items(.utility_token)[token],
                        self.ast.tokens.items(.lexeme)[token],
                        @tagName(self.ast.tokens.items(.tag)[next_token]),
                        next_token,
                        offsets[next_token],
                        offsets[next_token] + lexeme_len,
                        self.ast.tokens.items(.utility_token)[next_token],
                        self.ast.tokens.items(.lexeme)[next_token],
                    },
                );

                self.dumpTokens();
            }

            const comment = try self.renderComments(
                self.ast.tokens.items(.source)[token],
                offsets[token] + lexeme_len,
                offsets[next_token],
            );

            switch (space) {
                .None => {},
                .Space => if (!comment) try self.ais.writer().writeByte(' '),
                .Newline => if (!comment) try self.ais.insertNewline(),

                .Comma => if (next_token_tag == .Comma) {
                    try self.renderToken(next_token, .Newline);
                } else if (!comment) {
                    try self.ais.insertNewline();
                },

                .CommaSpace => if (next_token_tag == .Comma) {
                    try self.renderToken(next_token, .Space);
                } else if (!comment) {
                    try self.ais.writer().writeByte(' ');
                },

                .Semicolon => if (next_token_tag == .Semicolon) {
                    try self.renderToken(next_token, .Newline);
                } else if (!comment) {
                    try self.ais.insertNewline();
                },

                .Skip => unreachable,
            }
        }

        fn renderComments(self: *Self, token_source: []const u8, start: usize, end: usize) Error!bool {
            var index: usize = start;
            while (std.mem.indexOf(u8, token_source[index..end], "//")) |offset| {
                const comment_start = index + offset + "//".len;

                // If there is no newline, the comment ends with EOF
                const newline_index = std.mem.indexOfScalar(u8, token_source[comment_start..end], '\n');
                const newline = if (newline_index) |i| comment_start + i else null;

                const untrimmed_comment = token_source[comment_start .. newline orelse token_source.len];
                const trimmed_comment = std.mem.trim(
                    u8,
                    untrimmed_comment,
                    &std.ascii.whitespace,
                );

                // Don't leave any whitespace at the start of the file
                if (index != 0) {
                    if (index == start and std.mem.containsAtLeast(
                        u8,
                        token_source[index..comment_start],
                        2,
                        "\n",
                    )) {
                        // Leave up to one empty line before the first comment
                        try self.ais.insertNewline();
                        try self.ais.insertNewline();
                    } else if (std.mem.indexOfScalar(u8, token_source[index..comment_start], '\n') != null) {
                        // Respect the newline directly before the comment.
                        // Note: This allows an empty line between comments
                        try self.ais.insertNewline();
                    } else if (index == start) {
                        // Otherwise if the first comment is on the same line as
                        // the token before it, prefix it with a single space.
                        try self.ais.writer().writeByte(' ');
                    }
                }

                index = 1 + (newline orelse end - 1);

                const comment_content = std.mem.trim(
                    u8,
                    trimmed_comment["//".len..],
                    &std.ascii.whitespace,
                );

                if (self.ais.disabled_offset != null and std.mem.eql(u8, comment_content, "zig fmt: on")) {
                    // Write the source for which formatting was disabled directly
                    // to the underlying writer, fixing up invalid whitespace.
                    const disabled_source = token_source[self.ais.disabled_offset.?..comment_start];
                    try self.ais.writeFixingWhitespace(disabled_source);
                    // Write with the canonical single space.
                    try self.ais.underlying_writer.writeAll("// zig fmt: on\n");
                    self.ais.disabled_offset = null;
                } else if (self.ais.disabled_offset == null and std.mem.eql(u8, comment_content, "zig fmt: off")) {
                    // Write with the canonical single space.
                    try self.ais.writer().writeAll("// zig fmt: off\n");
                    self.ais.disabled_offset = index;
                } else {
                    // Write the comment minus trailing whitespace.
                    try self.ais.writer().print("// {s}\n", .{trimmed_comment});
                }
            }

            if (index != start and std.mem.containsAtLeast(
                u8,
                token_source[index - 1 .. end],
                2,
                "\n",
            )) {
                // Don't leave any whitespace at the end of the file
                if (end != token_source.len) {
                    try self.ais.insertNewline();
                }
            }

            return index != start;
        }

        fn renderFunDeclaration(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderNode(
                self.ast.nodes.items(.components)[node].FunDeclaration.function,
                space,
            );
        }

        fn renderBlock(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const statements = self.ast.nodes.items(.components)[node].Block;
            const locations = self.ast.nodes.items(.location);
            const end_locations = self.ast.nodes.items(.end_location);

            // {
            try self.renderExpectedToken(
                locations[node],
                .LeftBrace,
                if (statements.len > 0)
                    .Newline
                else
                    .None,
            );

            try self.ais.forcePushIndent(.normal);
            for (statements) |stmt| {
                try self.renderNode(stmt, .Semicolon);
            }
            self.ais.popIndent();

            // }
            try self.renderExpectedToken(
                if (statements.len > 0)
                    end_locations[statements[statements.len - 1]] + 1
                else
                    locations[node] + 1,
                .RightBrace,
                space,
            );
        }

        fn renderFunction(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const locations = self.ast.nodes.items(.location);
            const end_locations = self.ast.nodes.items(.end_location);

            const components = self.ast.nodes.items(.components);
            const fun_def = self.ast.nodes.items(.type_def)[node].?
                .resolved_type.?.Function;
            const comp = components[node].Function;

            // If not at start of script, must be preceded by a new line
            if (self.ast.tokens.items(.offset)[
                if (fun_def.function_type == .Extern)
                    locations[node] - 2
                else if (locations[node] > 0)
                    locations[node] - 1
                else
                    locations[node]
            ] > 0) {
                try self.ais.insertNewline();
            }

            if (fun_def.function_type == .Script or fun_def.function_type == .ScriptEntryPoint) {
                for (components[comp.body.?].Block) |statement| {
                    try self.renderNode(statement, .Semicolon);
                }

                return;
            }

            if (fun_def.function_type == .Extern) {
                // extern
                try self.renderExpectedToken(locations[node] - 2, .Extern, .Space);
            }

            if (comp.docblock) |docblock| {
                try self.renderExpectedToken(docblock, .Docblock, .Newline);
            }

            const token_idx = idx: {
                if (comp.test_message) |test_message| {
                    // test
                    try self.renderExpectedToken(test_message - 1, .Test, .Space);

                    try self.ais.pushIndent(.normal);

                    // "..."
                    try self.renderExpectedToken(test_message, .String, .Space);

                    break :idx test_message + 1;
                } else {
                    assert(comp.function_signature != null);

                    // fun ...
                    try self.renderNode(
                        comp.function_signature.?,
                        if (comp.body != null)
                            .Space
                        else
                            space,
                    );

                    break :idx end_locations[comp.function_signature.?] + 1;
                }
            };

            if (comp.body) |body| {
                if (fun_def.lambda) {
                    assert(fun_def.function_type != .Test);
                    assert(token_idx == locations[body] - 1);

                    try self.ais.forcePushIndent(.normal);

                    // =>
                    try self.renderExpectedToken(token_idx, .DoubleArrow, .Space);
                }

                // expr or block
                try self.renderNode(body, space);

                if (fun_def.lambda) {
                    self.ais.popIndent();
                }
            }
        }

        fn renderFunctionType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const locations = self.ast.nodes.items(.location);
            const end_locations = self.ast.nodes.items(.end_location);

            const components = self.ast.nodes.items(.components);
            const comp = components[node].FunctionType;

            // fun
            try self.renderExpectedToken(locations[node] - 1, .Fun, .Space);

            try self.ais.pushIndent(.normal);
            defer self.ais.popIndent();

            // identifier
            if (comp.name) |name| {
                try self.renderExpectedToken(name, .Identifier, .None);
            }

            // fun or identifier
            var token_idx = comp.name orelse locations[node];

            if (comp.generic_types.len > 0) {
                // ::<
                try self.renderExpectedTokenSequence(
                    token_idx + 1,
                    &.{ .DoubleColon, .Less },
                    .None,
                );

                for (comp.generic_types, 0..) |generic_type, i| {
                    try self.renderNode(
                        generic_type,
                        if (comp.generic_types.len > 1 and i < comp.generic_types.len - 1)
                            .CommaSpace
                        else
                            .None,
                    );
                }

                // >
                try self.renderExpectedToken(
                    comp.generic_types[comp.generic_types.len - 1] + 1,
                    .Greater,
                    .None,
                );

                token_idx = comp.generic_types[comp.generic_types.len - 1] + 1;
            }

            // Skip this/$args/_ utility token
            if (self.ast.tokens.items(.utility_token)[token_idx + 1]) {
                token_idx += 2;
            } else {
                token_idx += 1;
            }

            // (
            try self.renderExpectedToken(token_idx, .LeftParen, .None);

            token_idx += 1;

            for (comp.arguments, 0..) |arg, i| {
                // arg
                try self.renderExpectedToken(arg.name, .Identifier, .None);

                // :
                try self.renderExpectedToken(arg.name + 1, .Colon, .Space);

                // type
                try self.renderNode(
                    arg.type,
                    if (arg.default != null)
                        .Space
                    else if (comp.arguments.len > 1 and i < comp.arguments.len - 1)
                        .CommaSpace
                    else
                        .None,
                );

                if (arg.default) |default| {
                    // =
                    try self.renderExpectedToken(
                        end_locations[arg.type] + 1,
                        .Equal,
                        .Space,
                    );

                    // default
                    try self.renderNode(
                        default,
                        if (comp.arguments.len > 1 and i < comp.arguments.len - 1)
                            .CommaSpace
                        else
                            .None,
                    );
                }
            }

            token_idx = if (comp.arguments.len > 1)
                end_locations[comp.arguments[comp.arguments.len - 1].default orelse comp.arguments[comp.arguments.len - 1].type] + 1
            else
                token_idx;

            // )
            token_idx = if (comp.return_type) |rt|
                locations[rt] - 2
            else if (comp.yield_type) |yt|
                locations[yt] - 2
            else if (comp.error_types.len > 0)
                locations[comp.error_types[comp.error_types.len - 1]] - 3
            else
                end_locations[node];

            try self.renderExpectedToken(
                token_idx,
                .RightParen,
                if (comp.return_type != null or comp.yield_type != null or comp.error_types.len > 0)
                    .Space
                else
                    space,
            );

            if (comp.return_type) |return_type| {
                // >
                try self.renderExpectedToken(token_idx + 1, .Greater, .Space);

                try self.renderNode(
                    return_type,
                    if (comp.yield_type != null or comp.error_types.len > 0)
                        .Space
                    else
                        space,
                );

                token_idx = end_locations[return_type] + 1;
            }

            if (comp.yield_type) |yield_type| {
                // *>
                try self.renderExpectedTokenSequence(
                    token_idx,
                    &.{ .Star, .Greater },
                    .Space,
                );

                try self.renderNode(
                    yield_type,
                    if (comp.error_types.len > 0)
                        .Space
                    else
                        space,
                );

                token_idx = end_locations[yield_type] + 1;
            }

            if (comp.error_types.len > 0) {
                // !>
                try self.renderExpectedToken(token_idx, .BangGreater, .Space);

                for (comp.error_types, 0..) |error_type, i| {
                    try self.renderNode(
                        error_type,
                        if (comp.error_types.len > 1 and i < comp.error_types.len - 1)
                            .CommaSpace
                        else
                            space,
                    );
                }
            }
        }

        fn renderReturn(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const components = self.ast.nodes.items(.components)[node].Return;
            try self.renderExpectedToken(
                self.ast.nodes.items(.location)[node],
                .Return,
                if (components.value != null)
                    .Space
                else
                    space,
            );

            if (components.value) |value| {
                try self.renderNode(value, space);
            }
        }

        fn renderSimpleType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const location = self.ast.nodes.items(.location)[node];
            const optional = self.ast.nodes.items(.type_def)[node].?.optional;

            try self.renderToken(location, if (optional) .None else space);

            if (optional) {
                try self.renderExpectedToken(location + 1, .Question, space);
            }
        }

        fn renderStringLiteral(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.ais.writer().writeAll(
                self.ast.nodes.items(.components)[node].StringLiteral.string,
            );

            const token = self.ast.nodes.items(.location)[node];
            try self.renderSpace(
                token,
                self.ast.tokens.items(.lexeme)[token].len,
                space,
            );
        }

        fn renderString(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const locations = self.ast.nodes.items(.location);
            const string_lexeme = self.ast.tokens.items(.lexeme)[locations[node]];

            // " or `
            try self.ais.writer().writeAll(string_lexeme[0..1]);

            for (self.ast.nodes.items(.components)[node].String) |part| {
                try self.renderNode(part, .None);
            }

            // " or `
            try self.ais.writer().writeByte(string_lexeme[string_lexeme.len - 1]);
            try self.renderSpace(
                locations[node],
                string_lexeme.len,
                space,
            );
        }

        fn renderBoolean(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderOneOfExpectedToken(
                self.ast.nodes.items(.location)[node],
                &.{ .True, .False },
                space,
            );
        }

        fn renderDouble(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderExpectedToken(
                self.ast.nodes.items(.location)[node],
                .DoubleValue,
                space,
            );
        }

        fn renderInteger(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderExpectedToken(
                self.ast.nodes.items(.location)[node],
                .IntegerValue,
                space,
            );
        }

        fn renderVoid(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderExpectedToken(
                self.ast.nodes.items(.location)[node],
                .Void,
                space,
            );
        }

        fn renderNull(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            try self.renderExpectedToken(
                self.ast.nodes.items(.location)[node],
                .Null,
                space,
            );
        }

        fn renderObjectInit(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const locations = self.ast.nodes.items(.location);
            const end_locations = self.ast.nodes.items(.end_location);
            const location = locations[node];
            const end_location = end_locations[node];
            const components = self.ast.nodes.items(.components)[node].ObjectInit;
            const utility_token = self.ast.tokens.items(.utility_token);

            if (components.object) |_| {
                unreachable; // TODO
            } else {
                try self.renderExpectedToken(location, .Dot, .None);
            }

            try self.ais.pushIndent(.normal);

            // {
            try self.renderExpectedToken(
                if (components.object) |object|
                    end_locations[object] + 1
                else
                    location + 1,
                .LeftBrace,
                .Newline,
            );

            for (components.properties) |field| {
                if (!utility_token[field.name]) {
                    // identifier
                    try self.renderToken(field.name, .Space);

                    try self.ais.pushIndent(.normal);

                    // =
                    try self.renderExpectedToken(field.name + 1, .Equal, .Space);
                }

                // value
                try self.renderNode(
                    field.value,
                    if (components.properties.len > 1) .Comma else .CommaSpace,
                );

                if (!utility_token[field.name]) {
                    self.ais.popIndent();
                }
            }

            self.ais.popIndent();

            // }
            try self.renderExpectedToken(end_location, .RightBrace, space);
        }

        fn renderAnonymousObjectType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
            const location = self.ast.nodes.items(.location)[node];
            const end_location = self.ast.nodes.items(.end_location)[node];
            const object_type = self.ast.nodes.items(.type_def)[node].?;
            const fields = self.ast.nodes.items(.components)[node].AnonymousObjectType.fields;
            const utility_token = self.ast.tokens.items(.utility_token);

            // obj
            try self.renderExpectedToken(location, .Obj, .None);

            try self.ais.pushIndent(.normal);

            // {
            try self.renderExpectedToken(location + 1, .LeftBrace, .Space);

            for (fields) |field| {
                if (!utility_token[field.name]) {
                    // identifier
                    try self.renderToken(field.name, .None);
                }

                // :
                try self.renderExpectedToken(
                    field.name + 1,
                    .Colon,
                    if (!utility_token[field.name]) .Space else .None,
                );

                try self.renderNode(
                    field.type,
                    if (fields.len > 2) .Comma else .CommaSpace,
                );
            }

            defer self.ais.popIndent();

            // }
            try self.renderExpectedToken(
                if (object_type.optional) end_location - 1 else end_location,
                .RightBrace,
                if (object_type.optional) .None else space,
            );

            // ?
            if (object_type.optional) {
                try self.renderExpectedToken(
                    end_location,
                    .Question,
                    space,
                );
            }
        }

        /// Shamelessly taken from zig source
        /// Automatically inserts indentation of written data by keeping
        /// track of the current indentation level
        ///
        /// We introduce a new indentation scope with pushIndent/popIndent whenever
        /// we potentially want to introduce an indent after the next newline.
        ///
        /// Indentation should only ever increment by one from one line to the next,
        /// no matter how many new indentation scopes are introduced. This is done by
        /// only realizing the indentation from the most recent scope. As an example:
        ///
        ///         while (foo) if (bar)
        ///             f(x);
        ///
        /// The body of `while` introduces a new indentation scope and the body of
        /// `if` also introduces a new indentation scope. When the newline is seen,
        /// only the indentation scope of the `if` is realized, and the `while` is
        /// not.
        ///
        /// As comments are rendered during space rendering, we need to keep track
        /// of the appropriate indentation level for them with pushSpace/popSpace.
        /// This should be done whenever a scope that ends in a .semicolon or a
        /// .comma is introduced.
        fn AutoIndentingStream(comptime UnderlyingWriter: type) type {
            return struct {
                const SelfAis = @This();
                pub const WriteError = UnderlyingWriter.Error;
                pub const Writer = std.io.Writer(*SelfAis, WriteError, write);

                pub const IndentType = enum {
                    normal,
                    after_equals,
                    binop,
                    field_access,
                };
                const StackElem = struct {
                    indent_type: IndentType,
                    realized: bool,
                };
                const SpaceElem = struct {
                    space: Space,
                    indent_count: usize,
                };

                underlying_writer: UnderlyingWriter,

                /// Offset into the source at which formatting has been disabled with
                /// a `buzz fmt: off` comment.
                ///
                /// If non-null, the AutoIndentingStream will not write any bytes
                /// to the underlying writer. It will however continue to track the
                /// indentation level.
                disabled_offset: ?usize = null,

                indent_count: usize = 0,
                indent_delta: usize,
                indent_stack: std.ArrayList(StackElem),
                space_stack: std.ArrayList(SpaceElem),
                space_mode: ?usize = null,
                disable_indent_committing: usize = 0,
                current_line_empty: bool = true,
                /// the most recently applied indent
                applied_indent: usize = 0,

                pub fn init(allocator: std.mem.Allocator, out: UnderlyingWriter, indent_delta_: usize) SelfAis {
                    return .{
                        .underlying_writer = out,
                        .indent_delta = indent_delta_,
                        .indent_stack = std.ArrayList(StackElem).init(allocator),
                        .space_stack = std.ArrayList(SpaceElem).init(allocator),
                    };
                }

                pub fn deinit(self: *SelfAis) void {
                    self.indent_stack.deinit();
                    self.space_stack.deinit();
                }

                pub fn writer(self: *SelfAis) Writer {
                    return .{ .context = self };
                }

                pub fn write(self: *SelfAis, bytes: []const u8) WriteError!usize {
                    if (bytes.len == 0)
                        return @as(usize, 0);

                    try self.applyIndent();
                    return self.writeNoIndent(bytes);
                }

                // Change the indent delta without changing the final indentation level
                pub fn setIndentDelta(self: *SelfAis, new_indent_delta: usize) void {
                    if (self.indent_delta == new_indent_delta) {
                        return;
                    } else if (self.indent_delta > new_indent_delta) {
                        assert(self.indent_delta % new_indent_delta == 0);
                        self.indent_count = self.indent_count * (self.indent_delta / new_indent_delta);
                    } else {
                        // assert that the current indentation (in spaces) in a multiple of the new delta
                        assert((self.indent_count * self.indent_delta) % new_indent_delta == 0);
                        self.indent_count = self.indent_count / (new_indent_delta / self.indent_delta);
                    }
                    self.indent_delta = new_indent_delta;
                }

                fn writeNoIndent(self: *SelfAis, bytes: []const u8) WriteError!usize {
                    if (bytes.len == 0)
                        return @as(usize, 0);

                    if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
                    if (bytes[bytes.len - 1] == '\n')
                        self.resetLine();
                    return bytes.len;
                }

                pub fn insertNewline(self: *SelfAis) WriteError!void {
                    _ = try self.writeNoIndent("\n");
                }

                fn resetLine(self: *SelfAis) void {
                    self.current_line_empty = true;

                    if (self.disable_indent_committing > 0) return;

                    if (self.indent_stack.items.len > 0) {
                        // By default, we realize the most recent indentation scope.
                        var to_realize = self.indent_stack.items.len - 1;

                        if (self.indent_stack.items.len >= 2 and
                            self.indent_stack.items[to_realize - 1].indent_type == .after_equals and
                            self.indent_stack.items[to_realize - 1].realized and
                            self.indent_stack.items[to_realize].indent_type == .binop)
                        {
                            // If we are in a .binop scope and our direct parent is .after_equals, don't indent.
                            // This ensures correct indentation in the below example:
                            //
                            //        const foo =
                            //            (x >= 'a' and x <= 'z') or         //<-- we are here
                            //            (x >= 'A' and x <= 'Z');
                            //
                            return;
                        }

                        if (self.indent_stack.items[to_realize].indent_type == .field_access) {
                            // Only realize the top-most field_access in a chain.
                            while (to_realize > 0 and self.indent_stack.items[to_realize - 1].indent_type == .field_access)
                                to_realize -= 1;
                        }

                        if (self.indent_stack.items[to_realize].realized) return;
                        self.indent_stack.items[to_realize].realized = true;
                        self.indent_count += 1;
                    }
                }

                /// Disables indentation level changes during the next newlines until re-enabled.
                pub fn disableIndentCommitting(self: *SelfAis) void {
                    self.disable_indent_committing += 1;
                }

                pub fn enableIndentCommitting(self: *SelfAis) void {
                    assert(self.disable_indent_committing > 0);
                    self.disable_indent_committing -= 1;
                }

                pub fn pushSpace(self: *SelfAis, space: Space) Error!void {
                    try self.space_stack.append(.{ .space = space, .indent_count = self.indent_count });
                }

                pub fn popSpace(self: *SelfAis) void {
                    _ = self.space_stack.pop();
                }

                /// Sets current indentation level to be the same as that of the last pushSpace.
                pub fn enableSpaceMode(self: *SelfAis, space: Space) void {
                    if (self.space_stack.items.len == 0) return;
                    const curr = self.space_stack.getLast();
                    if (curr.space != space) return;
                    self.space_mode = curr.indent_count;
                }

                pub fn disableSpaceMode(self: *SelfAis) void {
                    self.space_mode = null;
                }

                pub fn lastSpaceModeIndent(self: *SelfAis) usize {
                    if (self.space_stack.items.len == 0) return 0;
                    return self.space_stack.getLast().indent_count * self.indent_delta;
                }

                /// Insert a newline unless the current line is blank
                pub fn maybeInsertNewline(self: *SelfAis) WriteError!void {
                    if (!self.current_line_empty)
                        try self.insertNewline();
                }

                /// Push default indentation
                /// Doesn't actually write any indentation.
                /// Just primes the stream to be able to write the correct indentation if it needs to.
                pub fn pushIndent(self: *SelfAis, indent_type: IndentType) Error!void {
                    try self.indent_stack.append(.{ .indent_type = indent_type, .realized = false });
                }

                /// Forces an indentation level to be realized.
                pub fn forcePushIndent(self: *SelfAis, indent_type: IndentType) Error!void {
                    try self.indent_stack.append(.{ .indent_type = indent_type, .realized = true });
                    self.indent_count += 1;
                }

                pub fn popIndent(self: *SelfAis) void {
                    if (self.indent_stack.pop().?.realized) {
                        assert(self.indent_count > 0);
                        self.indent_count -= 1;
                    }
                }

                pub fn indentStackEmpty(self: *SelfAis) bool {
                    return self.indent_stack.items.len == 0;
                }

                /// Writes ' ' bytes if the current line is empty
                fn applyIndent(self: *SelfAis) WriteError!void {
                    const current_indent = self.currentIndent();
                    if (self.current_line_empty and current_indent > 0) {
                        if (self.disabled_offset == null) {
                            try self.underlying_writer.writeByteNTimes(' ', current_indent);
                        }
                        self.applied_indent = current_indent;
                    }
                    self.current_line_empty = false;
                }

                /// Checks to see if the most recent indentation exceeds the currently pushed indents
                pub fn isLineOverIndented(self: *SelfAis) bool {
                    if (self.current_line_empty) return false;
                    return self.applied_indent > self.currentIndent();
                }

                fn currentIndent(self: *SelfAis) usize {
                    const indent_count = self.space_mode orelse self.indent_count;
                    return indent_count * self.indent_delta;
                }

                pub fn writeFixingWhitespace(self: *SelfAis, slice: []const u8) Error!void {
                    for (slice) |byte| switch (byte) {
                        '\t' => try self.underlying_writer.writeByteNTimes(' ', self.indent_delta),
                        '\r' => {},
                        else => try self.underlying_writer.writeByte(byte),
                    };
                }
            };
        }
    };
}
