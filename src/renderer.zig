const std = @import("std");
const assert = std.debug.assert;
const builtin = @import("builtin");
const Ast = @import("Ast.zig");
const Token = @import("Token.zig");
const WriteableArrayList = @import("writeable_array_list.zig").WriteableArrayList;

pub const Renderer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    ast: Ast.Slice,
    ais: *AutoIndentingStream,

    indent: u16 = 0,

    pub const Error = error{
        AccessDenied,
        BrokenPipe,
        CantRender,
        ConnectionResetByPeer,
        DeviceBusy,
        DiskQuota,
        FileTooBig,
        InputOutput,
        InvalidArgument,
        LockViolation,
        MessageTooBig,
        NoDevice,
        NoSpaceLeft,
        NotOpenForWriting,
        OperationAborted,
        OutOfMemory,
        PermissionDenied,
        ProcessNotFound,
        SystemResources,
        Unexpected,
        WouldBlock,
        WriteFailed,
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
        /// Additionally consume the next token if it is a semicolon.
        /// In either case, a space will be inserted afterwards.
        SemicolonSpace,
        /// Skip rendering whitespace and comments. If this is used, the caller
        /// *must* handle whitespace and comments manually.
        Skip,
    };

    pub fn render(allocator: std.mem.Allocator, out: *std.Io.Writer, ast: Ast) Error!void {
        var ais: AutoIndentingStream = .init(out, 4);
        defer ais.deinit(allocator);

        var self = Self{
            .allocator = allocator,
            .ast = ast.slice(),
            .ais = &ais,
        };

        if (ast.root == null) {
            return;
        }

        // Render eventual comments before the first statement
        const first_location = self.ast.tokens.get(
            self.ast.nodes.items(.location)[self.ast.root.?],
        );

        if (first_location.offset > 0) {
            _ = try self.renderComments(
                first_location.source,
                0,
                first_location.offset,
            );
        }

        try self.renderNode(self.ast.root.?, .Newline);
    }

    inline fn renderNode(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        return Self.renderers[@intFromEnum(self.ast.nodes.items(.tag)[node])](self, node, space);
    }

    const renderers = [_]RenderNode{
        renderAnonymousObjectType,
        renderAs,
        renderAsyncCall,
        renderBinary,
        renderBlock,
        renderBlockExpression,
        renderBoolean,
        renderBreak,
        renderCall,
        renderContinue,
        renderDot,
        renderDoUntil,
        renderEnum,
        renderExport,
        renderExpression,
        renderFiberType,
        renderDouble,
        renderFor,
        renderForceUnwrap,
        renderForEach,
        renderFunction,
        renderFunctionType,
        renderFunDeclaration,
        renderGenericResolve,
        renderGenericResolveType,
        renderGenericType,
        renderGrouping,
        renderIf,
        renderImport,
        renderInteger,
        renderIs,
        renderList,
        renderListType,
        renderMap,
        renderMapType,
        renderNamespace,
        renderNamedVariable,
        renderNull,
        renderObjectDeclaration,
        renderObjectInit,
        renderOut,
        renderPattern,
        renderProtocolDeclaration,
        renderRange,
        renderResolve,
        renderResume,
        renderReturn,
        renderSimpleType,
        renderString,
        renderStringLiteral,
        renderSubscript,
        renderThrow,
        renderTry,
        renderTypeExpression,
        renderTypeOfExpression,
        renderUnary,
        renderUnwrap,
        renderUserType,
        renderVarDeclaration,
        renderVoid,
        renderWhile,
        renderYield,
        renderZdef,
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

    fn renderQualifiedName(self: *Self, tokens: []const Ast.TokenIndex, space: Space) Error!void {
        for (tokens, 0..) |token, i| {
            const is_last = i == tokens.len - 1;

            try self.renderToken(token, if (is_last) space else .None);

            if (!is_last) {
                try self.ais.writer().writeByte('\\');
            }
        }
    }

    fn dumpTokens(self: *Self, from: Ast.TokenIndex, upto: Ast.TokenIndex) void {
        const lexemes = self.ast.tokens.items(.lexeme);
        const utility = self.ast.tokens.items(.utility_token);
        std.debug.print("\n", .{});
        for (from..upto) |i| {
            std.debug.print(
                "{s}{}`{s}` ",
                .{
                    if (utility[i]) "*" else "",
                    i,
                    lexemes[i],
                },
            );
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

            self.dumpTokens(token - 1, token + 5);
        }

        assert(std.mem.indexOf(Token.Type, expected, &.{self.ast.tokens.items(.tag)[token]}) != null);
        return self.renderToken(token, space);
    }

    fn renderExpectedToken(self: *Self, token: Ast.TokenIndex, expected: Token.Type, space: Space) Error!void {
        if (builtin.mode == .Debug and self.ast.tokens.items(.tag)[token] != expected) {
            std.debug.print(
                "\nExpected {s} got {s} at {} `{s}`\n",
                .{
                    @tagName(expected),
                    @tagName(self.ast.tokens.items(.tag)[token]),
                    token,
                    self.ast.tokens.items(.lexeme)[token],
                },
            );

            self.dumpTokens(token - 5, token + 5);
        }

        assert(self.ast.tokens.items(.tag)[token] == expected);
        return self.renderToken(token, space);
    }

    fn renderToken(self: *Self, token: Ast.TokenIndex, space: Space) Error!void {
        const lexeme = self.ast.tokens.items(.lexeme)[token];

        if (self.ast.tokens.items(.tag)[token] == .Identifier and isAtIdentifier(lexeme)) {
            try self.ais.writer().print(
                "@\"{s}\"",
                .{
                    lexeme,
                },
            );
        } else {
            try self.ais.writer().writeAll(lexeme);
        }

        try self.renderSpace(
            token,
            lexeme.len,
            space,
        );
    }

    fn isAtIdentifier(lexeme: []const u8) bool {
        // Is a keyword or does not start with a letter and is not a lone `_`
        if (Token.keywords.get(lexeme) != null or
            ((lexeme[0] < 'a' or lexeme[0] > 'z') and
                (lexeme[0] < 'A' or lexeme[0] > 'Z') and
                (lexeme.len > 1 or lexeme[0] != '_')))
        {
            return true;
        }

        for (lexeme) |c| {
            switch (c) {
                'a'...'z',
                'A'...'Z',
                '0'...'9',
                '_',
                => {},
                else => return true,
            }
        }

        return false;
    }

    fn renderSpace(self: *Self, token: Ast.TokenIndex, lexeme_len: usize, space: Space) Error!void {
        if (space == .Skip) return;

        const offsets = self.ast.tokens.items(.offset);
        const script_names = self.ast.tokens.items(.script_name);

        // Skip utility tokens
        const utility = self.ast.tokens.items(.utility_token);
        var next_token = token + 1;
        while (utility[next_token] or
            offsets[next_token] < offsets[token] or
            !std.mem.eql(
                u8,
                script_names[token],
                script_names[next_token],
            ))
        {
            next_token += 1;
        }

        const next_token_tag = self.ast.tokens.items(.tag)[next_token];
        if (space == .Comma and next_token_tag != .Comma) {
            try self.ais.writer().writeByte(',');
        }

        if (space == .Semicolon or space == .Comma) self.ais.enableSpaceMode(space);
        defer self.ais.disableSpaceMode();

        if (builtin.mode == .Debug and offsets[next_token] < offsets[token] + lexeme_len) {
            std.debug.print(
                "\nToken overlap: {s}#{} at {}-{} {} `{s}` and {s}#{} at {}-{} {} `{s}`\n{s}\n",
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
                    self.ast.tokens.items(.source)[next_token][offsets[token] .. offsets[next_token] + lexeme_len],
                },
            );

            self.dumpTokens(token - 1, token + 5);
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

            .SemicolonSpace => if (next_token_tag == .Semicolon) {
                try self.renderToken(next_token, .Space);
            } else if (!comment) {
                try self.ais.writer().writeByte(' ');
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

            const comment = token_source[comment_start .. newline orelse token_source.len];

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

            if (self.ais.disabled_offset != null and std.mem.eql(u8, comment, "buzz fmt: on")) {
                // Write the source for which formatting was disabled directly
                // to the underlying writer, fixing up invalid whitespace.
                const disabled_source = token_source[self.ais.disabled_offset.?..comment_start];
                try self.ais.writeFixingWhitespace(disabled_source);
                // Write with the canonical single space.
                try self.ais.underlying_writer.writeAll("// buzz fmt: on\n");
                self.ais.disabled_offset = null;
            } else if (self.ais.disabled_offset == null and std.mem.eql(u8, comment, "buzz fmt: off")) {
                // Write with the canonical single space.
                try self.ais.writer().writeAll("// buzz fmt: off\n");
                self.ais.disabled_offset = index;
            } else {
                // Write the comment minus trailing whitespace.
                try self.ais.writer().print(
                    "//{s}{s}\n",
                    .{
                        if (!std.mem.startsWith(u8, comment, " "))
                            " "
                        else
                            "",
                        comment,
                    },
                );
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

    fn renderExtraNewline(self: *Self, node: Ast.Node.Index) Error!void {
        return self.renderExtraNewlineToken(self.ast.nodes.items(.location)[node]);
    }

    /// Check if there is an empty line immediately before the given token. If so, render it.
    fn renderExtraNewlineToken(self: *Self, token_idx: Ast.TokenIndex) Error!void {
        const offsets = self.ast.tokens.items(.offset);
        const lexemes = self.ast.tokens.items(.lexeme);
        const tags = self.ast.tokens.items(.tag);
        const source = self.ast.tokens.items(.source)[token_idx];
        const script_names = self.ast.tokens.items(.script_name);
        const utility = self.ast.tokens.items(.utility_token);

        const token_start = offsets[token_idx];
        if (token_start == 0) return;

        // Skip eventual utility tokens or tokens from imported scripts
        var prev_token = if (token_idx == 0)
            0
        else
            token_idx - 1;
        while (prev_token > 0 and
            (utility[prev_token] or
                !std.mem.eql(
                    u8,
                    script_names[token_idx],
                    script_names[prev_token],
                )))
        {
            prev_token -= 1;
        }

        const prev_token_end = offsets[prev_token] + lexemes[prev_token].len;

        if (prev_token_end > token_start) {
            std.debug.print(
                "\n{s}#{} {} of {s} - {s}#{} {} of {s}",
                .{
                    @tagName(tags[prev_token]),
                    prev_token,
                    utility[prev_token],
                    script_names[prev_token],
                    @tagName(tags[token_idx]),
                    token_idx,
                    utility[token_idx],
                    script_names[token_idx],
                },
            );
            self.dumpTokens(prev_token -| 2, token_idx + 2);

            unreachable;
        }

        // If there is a immediately preceding comment or doc_comment,
        // skip it because required extra newline has already been rendered.
        if (std.mem.indexOf(u8, source[prev_token_end..token_start], "//") != null)
            return;
        if (tags[prev_token] == .Docblock)
            return;

        // Iterate backwards to the end of the previous token, stopping if a
        // non-whitespace character is encountered or two newlines have been found.
        var i = token_start - 1;
        var newlines: u2 = 0;
        while (std.ascii.isWhitespace(source[i])) : (i -= 1) {
            if (source[i] == '\n') newlines += 1;
            if (newlines == 2) return self.ais.insertNewline();
            if (i == prev_token_end) break;
        }
    }

    fn renderFunDeclaration(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        // docblock
        if (self.ast.nodes.items(.docblock)[node]) |docblock| {
            try self.renderExpectedToken(
                docblock,
                .Docblock,
                .None,
            );
        }

        try self.renderNode(
            self.ast.nodes.items(.components)[node].FunDeclaration.function,
            space,
        );
    }

    fn renderBlock(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const statements = self.ast.nodes.items(.components)[node].Block;
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);

        try self.ais.pushIndent(self.allocator, .normal);

        // {
        try self.renderExpectedToken(
            locations[node],
            .LeftBrace,
            if (statements.len > 0)
                .Newline
            else
                .None,
        );

        for (statements, 0..) |stmt, i| {
            if (i != 0) {
                try self.renderExtraNewline(stmt);
            }

            try self.ais.pushSpace(self.allocator, .Semicolon);
            try self.renderNode(stmt, .Semicolon);
            self.ais.popSpace();
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
        const is_lambda = if (comp.function_signature) |fs|
            components[fs].FunctionType.lambda
        else
            false;

        if (fun_def.function_type == .Script or fun_def.function_type == .ScriptEntryPoint) {
            const tags = self.ast.nodes.items(.tag);

            for (components[comp.body.?].Block, 0..) |statement, i| {
                if (i > 0) {
                    switch (tags[statement]) {
                        .Export,
                        .FunDeclaration,
                        .ObjectDeclaration,
                        .ProtocolDeclaration,
                        .Enum,
                        => try self.ais.insertNewline(),
                        // Keep imports clustered
                        .Import => if (tags[components[comp.body.?].Block[i - 1]] != .Import)
                            try self.ais.insertNewline(),
                        else => try self.renderExtraNewline(statement),
                    }
                }

                try self.ais.pushSpace(self.allocator, .Semicolon);
                try self.renderNode(statement, .Semicolon);
                self.ais.popSpace();
            }

            return;
        }

        if (fun_def.function_type == .Extern) {
            // extern
            try self.renderExpectedToken(locations[node] - 2, .Extern, .Space);
        }

        const token_idx = idx: {
            if (comp.test_message) |test_message| {
                // test
                try self.renderExpectedToken(test_message - 2, .Test, .Space);

                try self.ais.pushIndent(self.allocator, .normal);

                // "..."
                try self.renderExpectedToken(test_message, .String, .Space);

                self.ais.popIndent();

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
            if (is_lambda) {
                assert(fun_def.function_type != .Test);
                assert(token_idx == locations[body] - 1);

                try self.ais.forcePushIndent(self.allocator, .normal);

                // =>
                try self.renderExpectedToken(token_idx, .DoubleArrow, .Space);
            }

            // expr or block
            try self.renderNode(body, space);

            if (is_lambda) {
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
        try self.renderExpectedToken(
            locations[node],
            .Fun,
            if (comp.name == null and comp.generic_types.len > 0)
                .None
            else
                .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        // identifier
        if (comp.name) |name| {
            try self.renderExpectedToken(name, .Identifier, .None);
        }

        // fun or identifier
        var token_idx = comp.name orelse locations[node];

        // FIXME: when the node is an actual type we have token here and when its a signature we have nodes
        if (comp.generic_types.len > 0) {
            // ::<
            try self.renderExpectedTokenSequence(
                if (comp.is_signature)
                    token_idx + 2 // +2 because we skip the utility token
                else
                    token_idx + 1,
                &.{ .DoubleColon, .Less },
                .None,
            );

            if (comp.is_signature) {
                for (comp.generic_types, 0..) |generic_type, i| {
                    try self.renderExpectedToken(
                        generic_type,
                        .Identifier,
                        if (comp.generic_types.len > 1 and i < comp.generic_types.len - 1)
                            .CommaSpace
                        else
                            .None,
                    );
                }

                token_idx = comp.generic_types[comp.generic_types.len - 1] + 1;

                // >
                try self.renderExpectedToken(
                    token_idx,
                    .Greater,
                    .None,
                );
            } else {
                for (comp.generic_types, 0..) |generic_type, i| {
                    try self.renderNode(
                        generic_type,
                        if (comp.generic_types.len > 1 and i < comp.generic_types.len - 1)
                            .CommaSpace
                        else
                            .None,
                    );
                }

                token_idx = end_locations[comp.generic_types[comp.generic_types.len - 1]] + 1;

                // >
                try self.renderExpectedToken(
                    token_idx,
                    .Greater,
                    .None,
                );
            }
        }

        // Skip this/$args/_ utility token
        if (self.ast.tokens.items(.utility_token)[token_idx + 1]) {
            token_idx += 2;
        } else {
            token_idx += 1;
        }

        // (
        try self.renderExpectedToken(token_idx, .LeftParen, .None);

        try self.ais.pushIndent(self.allocator, .normal);

        token_idx += 1;

        // If trailing comma, all args should be on a newline
        const last_arg = if (comp.arguments.len > 0)
            comp.arguments[comp.arguments.len - 1]
        else
            null;
        var has_trailing_comma = if (last_arg) |arg|
            self.ast.tokens.items(.tag)[
                if (arg.default) |d|
                    end_locations[d] + 1
                else
                    end_locations[arg.type] + 1
            ] == .Comma
        else
            false;

        const term: Space = if (has_trailing_comma)
            .Comma
        else
            .CommaSpace;

        if (has_trailing_comma) {
            try self.ais.insertNewline();
        }

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
                    term
                else if (has_trailing_comma)
                    term
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
                        term
                    else if (has_trailing_comma)
                        term
                    else
                        .None,
                );
            }
        }

        self.ais.popIndent();

        // )
        token_idx = if (comp.return_type) |rt|
            locations[rt] - 2
        else if (comp.yield_type) |yt|
            locations[yt] - 2
        else if (comp.error_types.len > 0)
            locations[comp.error_types[0]] - 2
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
            try self.renderExpectedToken(
                locations[comp.error_types[0]] - 1,
                .BangGreater,
                .Space,
            );

            try self.ais.pushIndent(self.allocator, .normal);
            defer self.ais.popIndent();

            has_trailing_comma = self.ast.tokens.items(.tag)[
                end_locations[comp.error_types[comp.error_types.len - 1]] + 1
            ] == .Comma;

            if (has_trailing_comma) {
                try self.ais.insertNewline();
            }

            for (comp.error_types, 0..) |error_type, i| {
                try self.renderNode(
                    error_type,
                    if (comp.error_types.len > 1 and i < comp.error_types.len - 1)
                        if (has_trailing_comma)
                            .Comma
                        else
                            .CommaSpace
                    else if (has_trailing_comma)
                        .CommaSpace
                    else
                        space,
                );
            }

            if (has_trailing_comma) {
                try self.ais.insertNewline();
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

    fn renderExpression(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        try self.renderNode(
            self.ast.nodes.items(.components)[node].Expression,
            space,
        );
    }

    fn renderNamedVariable(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].NamedVariable;

        try self.renderQualifiedName(
            components.name,
            if (components.assign_token != null) .Space else space,
        );

        if (components.assign_token) |assign_token| {
            try self.renderToken(assign_token, .Space);

            try self.renderNode(components.value.?, space);
        }
    }

    fn renderSimpleType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const location = self.ast.nodes.items(.location)[node];
        const optional = self.ast.nodes.items(.type_def)[node].?.optional;

        try self.renderToken(location, if (optional) .None else space);

        if (optional) {
            try self.renderExpectedToken(
                location + 1,
                .Question,
                space,
            );
        }
    }

    /// Print the string as escaped contents of a double quoted or single-quoted string.
    /// Format `{}` treats contents as a double-quoted string.
    /// Format `{'}` treats contents as a single-quoted string.
    // FIXME: should operate on graphemes otherwise we replace emojis with series of escaped bytes?
    fn stringEscape(
        literal: Ast.StringLiteral,
        writer: *std.Io.Writer,
    ) !void {
        for (literal.literal.string) |byte| switch (byte) {
            '{' => try writer.writeAll("\\{"),
            '\n' => if (literal.delimiter == '`') try writer.writeAll("\n") else try writer.writeAll("\\n"),
            '\r' => if (literal.delimiter == '`') try writer.writeAll("\r") else try writer.writeAll("\\r"),
            '\t' => if (literal.delimiter == '`') try writer.writeAll("\t") else try writer.writeAll("\\t"),
            '\\' => try writer.writeAll("\\\\"),
            '"' => {
                if (literal.delimiter == '`') {
                    try writer.writeByte('"');
                } else {
                    try writer.writeAll("\\\"");
                }
            },
            '`' => {
                if (literal.delimiter == '"') {
                    try writer.writeAll("`");
                } else {
                    try writer.writeByte('\'');
                }
            },
            ' ',
            '\'',
            '!',
            '#'...'&',
            '('...'[',
            ']'...('`' - 1),
            ('`' + 1)...('{' - 1),
            ('{' + 1)...'~',
            => try writer.writeByte(byte),
            // Use hex escapes for rest any unprintable characters.
            else => try writer.print("\\{:0>3}", .{byte}),
        };
    }

    fn renderStringLiteral(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const string_literal = self.ast.nodes.items(.components)[node].StringLiteral;
        var formatter = std.fmt.Alt(Ast.StringLiteral, stringEscape){
            .data = string_literal,
        };

        if (string_literal.delimiter == '`') {
            var buffer = WriteableArrayList(u8).init(self.allocator);
            defer buffer.deinit();

            try formatter.format(&buffer.writer);

            try self.ais.writeFixingWhitespace(buffer.list.items);
        } else {
            try formatter.format(self.ais.underlying_writer);
        }

        const token = self.ast.nodes.items(.location)[node];
        try self.renderSpace(
            token,
            self.ast.tokens.items(.lexeme)[token].len,
            space,
        );
    }

    fn renderString(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const tags = self.ast.nodes.items(.tag);
        const string_lexeme = self.ast.tokens.items(.lexeme)[locations[node]];

        // " or `
        try self.ais.writer().writeAll(string_lexeme[0..1]);

        for (self.ast.nodes.items(.components)[node].String) |part| {
            if (tags[part] != .StringLiteral) {
                try self.ais.writer().writeByte('{');

                try self.renderNode(part, .None);

                try self.ais.writer().writeByte('}');
            } else {
                try self.renderNode(part, .None);
            }
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

    fn renderPattern(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Pattern,
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
        const mutable = self.ast.nodes.items(.type_def)[node].?.isMutable();

        var token_idx = location;
        if (mutable) {
            try self.renderExpectedToken(
                token_idx,
                .Mut,
                .Space,
            );

            token_idx += 1;
        }

        if (components.object) |object| {
            try self.renderNode(object, .None);
        } else {
            try self.renderExpectedToken(token_idx, .Dot, .None);

            token_idx += 1;
        }

        try self.ais.pushIndent(self.allocator, .normal);

        const last_field = if (components.properties.len > 0)
            components.properties[components.properties.len - 1]
        else
            null;

        const last_field_token = if (last_field != null and
            self.ast.tokens.items(.utility_token)[end_locations[last_field.?.value] + 1] and
            self.ast.tokens.items(.tag)[end_locations[last_field.?.value] + 1] != .Comma)
            end_locations[last_field.?.value] + 2
        else if (last_field != null)
            end_locations[last_field.?.value] + 1
        else
            null;

        const has_trailing_comma = if (last_field_token) |lft|
            self.ast.tokens.items(.tag)[lft] == .Comma
        else
            false;

        // {
        try self.renderExpectedToken(
            if (components.object) |object|
                end_locations[object] + 1
            else
                token_idx,
            .LeftBrace,
            if (has_trailing_comma)
                .Newline
            else if (components.properties.len == 0)
                .None
            else
                .Space,
        );

        for (components.properties) |field| {
            const show_identifier = !utility_token[field.name] and locations[field.value] != field.name;

            if (show_identifier) {
                // identifier
                try self.renderToken(field.name, .Space);

                try self.ais.pushIndent(self.allocator, .normal);

                // =
                try self.renderExpectedToken(field.name + 1, .Equal, .Space);
            }

            // value
            try self.renderNode(
                field.value,
                if (has_trailing_comma)
                    .Comma
                else
                    .CommaSpace,
            );

            if (show_identifier) {
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

        try self.ais.pushIndent(self.allocator, .normal);

        const last_field = if (fields.len > 0) fields[fields.len - 1] else null;
        const has_trailing_comma = if (last_field) |lf|
            self.ast.tokens.items(.tag)[self.ast.nodes.items(.end_location)[lf.type] + 1] == .Comma
        else
            false;

        // {
        try self.renderExpectedToken(
            location + 1,
            .LeftBrace,
            if (has_trailing_comma)
                .Newline
            else if (fields.len > 0)
                .Space
            else
                .None,
        );

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
                if (has_trailing_comma) .Comma else .CommaSpace,
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

    fn renderIsAs(self: *Self, components: Ast.IsAs, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);

        try self.renderNode(components.left, .Space);

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderOneOfExpectedToken(
            end_locations[components.left] + 1,
            &.{ .Is, .As, .AsQuestion },
            .Space,
        );

        try self.renderNode(components.constant, space);
    }

    fn renderAs(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        return self.renderIsAs(
            self.ast.nodes.items(.components)[node].As,
            space,
        );
    }

    fn renderIs(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        return self.renderIsAs(
            self.ast.nodes.items(.components)[node].Is,
            space,
        );
    }

    fn renderBinary(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].Binary;
        const end_locations = self.ast.nodes.items(.end_location);

        try self.renderNode(components.left, .Space);

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderExpectedToken(
            end_locations[components.left] + 1,
            components.operator,
            .Space,
        );

        try self.renderNode(components.right, space);
    }

    fn renderBreak(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const label = self.ast.nodes.items(.components)[node].Break.label;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Break,
            if (label != null) .Space else space,
        );

        if (label) |lbl| {
            try self.ais.pushIndent(self.allocator, .normal);
            defer self.ais.popIndent();

            try self.renderExpectedToken(lbl, .Identifier, space);
        }
    }

    fn renderContinue(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const label = self.ast.nodes.items(.components)[node].Continue.label;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Continue,
            if (label != null) .Space else space,
        );

        if (label) |lbl| {
            try self.ais.pushIndent(self.allocator, .normal);
            defer self.ais.popIndent();

            try self.renderExpectedToken(lbl, .Identifier, space);
        }
    }

    fn renderOut(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const expr = self.ast.nodes.items(.components)[node].Out;
        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Out,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderNode(expr, space);
    }

    fn renderAsyncCall(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Ampersand,
            .None,
        );

        try self.renderNode(
            self.ast.nodes.items(.components)[node].AsyncCall,
            space,
        );
    }

    fn renderCall(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components);
        const tags = self.ast.nodes.items(.tag);
        const comp = components[node].Call;
        const invoked = tags[comp.callee] == .Dot and
            components[comp.callee].Dot.member_kind == .Call and
            components[comp.callee].Dot.value_or_call_or_enum.Call == node;

        // callee (generated by Dot if call is invokation)
        if (!invoked) {
            try self.renderNode(comp.callee, .None);
        }

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        // (
        var paren_idx = if (invoked)
            if (components[comp.callee].Dot.generic_resolve) |gr|
                end_locations[gr] + 1
            else
                components[comp.callee].Dot.identifier + 1
        else
            end_locations[comp.callee] + 1;
        try self.renderExpectedToken(
            paren_idx,
            .LeftParen,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        const last_arg = if (comp.arguments.len == 0)
            null
        else
            comp.arguments[comp.arguments.len - 1];
        const has_trailing_comma = if (last_arg) |arg|
            self.ast.tokens.items(.tag)[end_locations[arg.value] + 1] == .Comma
        else
            false;

        if (has_trailing_comma) {
            try self.ais.insertNewline();
        }

        for (comp.arguments, 0..) |arg, i| {
            if (arg.name) |name| {
                // Don't show arg name if named expression argument
                if (name != self.ast.nodes.items(.location)[arg.value]) {
                    // arg name
                    try self.renderExpectedToken(name, .Identifier, .None);

                    // :
                    try self.renderExpectedToken(name + 1, .Colon, .Space);
                }
            }

            try self.renderNode(
                arg.value,
                if (comp.arguments.len > 1 and i < comp.arguments.len - 1)
                    if (has_trailing_comma)
                        .Comma
                    else
                        .CommaSpace
                else if (has_trailing_comma)
                    .Comma
                else
                    .None,
            );
        }

        self.ais.popIndent();

        // )
        paren_idx = if (comp.arguments.len == 0)
            paren_idx + 1
        else if (has_trailing_comma)
            end_locations[comp.arguments[comp.arguments.len - 1].value] + 2
        else
            end_locations[comp.arguments[comp.arguments.len - 1].value] + 1;

        try self.renderExpectedToken(
            paren_idx,
            .RightParen,
            if (comp.catch_default != null)
                .Space
            else
                space,
        );

        if (comp.catch_default) |catch_default| {
            // catch
            try self.renderExpectedToken(paren_idx + 1, .Catch, .Space);

            // default_value
            try self.renderNode(catch_default, space);
        }
    }

    fn renderList(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].List;
        const locations = self.ast.nodes.items(.location);
        const location = locations[node];
        const tags = self.ast.tokens.items(.tag);
        const end_locations = self.ast.nodes.items(.end_location);

        const last_item = if (components.items.len > 0)
            components.items[components.items.len - 1]
        else
            null;

        const has_trailing_comma = if (last_item) |item|
            tags[end_locations[item] + 1] == .Comma
        else if (components.explicit_item_type) |it|
            tags[end_locations[it] + 1] == .Comma
        else
            false;

        try self.ais.pushIndent(self.allocator, .normal);

        const mutable = self.ast.nodes.items(.type_def)[node].?.isMutable();
        var token_idx = location;
        if (mutable) {
            try self.renderExpectedToken(
                token_idx,
                .Mut,
                .Space,
            );

            token_idx += 1;
        }

        // [
        try self.renderExpectedToken(
            token_idx,
            .LeftBracket,
            if (has_trailing_comma)
                .Newline
            else if (components.items.len > 0)
                .Space
            else
                .None,
        );

        if (components.explicit_item_type) |item_type| {
            // <
            try self.renderExpectedToken(
                token_idx + 1,
                .Less,
                .None,
            );

            try self.renderNode(item_type, .None);

            // >
            try self.renderExpectedToken(
                locations[item_type] + 1,
                .Greater,
                if (has_trailing_comma)
                    .Comma
                else if (components.items.len > 0)
                    .CommaSpace
                else
                    .None,
            );
        }

        for (components.items, 0..) |item, i| {
            try self.renderNode(
                item,
                if (has_trailing_comma)
                    .Comma
                else if ((components.items.len > 1 and i < components.items.len - 1) or components.explicit_item_type != null)
                    .CommaSpace
                else
                    .Space,
            );
        }

        self.ais.popIndent();

        // ]
        try self.renderExpectedToken(
            self.ast.nodes.items(.end_location)[node],
            .RightBracket,
            space,
        );
    }

    fn renderListType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].ListType;
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const location = locations[node];

        const type_def = self.ast.nodes.items(.type_def)[node].?;
        const is_optional = type_def.optional;
        const is_mutable = type_def.isMutable();

        if (is_mutable) {
            try self.renderExpectedToken(
                location,
                .Mut,
                .Space,
            );
        }

        // [
        try self.renderExpectedToken(
            if (is_mutable)
                location + 1
            else
                location,
            .LeftBracket,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // item_type
        try self.renderNode(components, .None);

        self.ais.popIndent();

        // ]
        try self.renderExpectedToken(
            end_locations[components] + 1,
            .RightBracket,
            if (is_optional)
                .None
            else
                space,
        );

        // ?
        if (is_optional) {
            try self.renderExpectedToken(
                end_locations[components] + 2,
                .Question,
                space,
            );
        }
    }

    fn renderMap(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].Map;
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const location = locations[node];
        const tags = self.ast.tokens.items(.tag);

        const last_entry = if (components.entries.len > 0)
            components.entries[components.entries.len - 1]
        else
            null;

        const has_trailing_comma = if (last_entry) |entry|
            tags[end_locations[entry.value] + 1] == .Comma
        else if (components.explicit_value_type) |vt|
            tags[end_locations[vt] + 1] == .Comma
        else
            false;

        const mutable = self.ast.nodes.items(.type_def)[node].?.isMutable();
        var token_idx = location;
        if (mutable) {
            try self.renderExpectedToken(
                token_idx,
                .Mut,
                .Space,
            );

            token_idx += 1;
        }

        try self.ais.pushIndent(self.allocator, .normal);

        // {
        try self.renderExpectedToken(
            token_idx,
            .LeftBrace,
            if (has_trailing_comma)
                .Newline
            else if (components.entries.len > 0)
                .Space
            else
                .None,
        );

        if (components.explicit_key_type) |key_type| {
            // <
            try self.renderExpectedToken(
                token_idx + 1,
                .Less,
                .None,
            );

            // key_type
            try self.renderNode(key_type, .None);

            // :
            try self.renderExpectedToken(
                end_locations[key_type] + 1,
                .Colon,
                .Space,
            );

            // value_type
            try self.renderNode(components.explicit_value_type.?, .None);

            // >
            try self.renderExpectedToken(
                end_locations[components.explicit_value_type.?] + 1,
                .Greater,
                if (has_trailing_comma)
                    .Comma
                else if (components.entries.len > 0)
                    .CommaSpace
                else
                    .None,
            );
        }

        for (components.entries, 0..) |entry, i| {
            // key
            try self.renderNode(entry.key, .None);

            // :
            try self.renderExpectedToken(
                end_locations[entry.key] + 1,
                .Colon,
                .Space,
            );

            // value
            try self.renderNode(
                entry.value,
                if (has_trailing_comma)
                    .Comma
                else if (components.entries.len > 0 and i < components.entries.len - 1)
                    .CommaSpace
                else
                    .Space,
            );
        }

        self.ais.popIndent();

        // }
        try self.renderExpectedToken(
            end_locations[node],
            .RightBrace,
            space,
        );
    }

    fn renderMapType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].MapType;
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const location = locations[node];

        const type_def = self.ast.nodes.items(.type_def)[node].?;
        const is_optional = type_def.optional;
        const is_mutable = type_def.isMutable();

        if (is_mutable) {
            try self.renderExpectedToken(
                location,
                .Mut,
                .Space,
            );
        }

        // {
        try self.renderExpectedToken(
            if (is_mutable)
                location + 1
            else
                location,
            .LeftBrace,
            .None,
        );

        // key_type,
        try self.renderNode(components.key_type, .None);

        // :
        try self.renderExpectedToken(
            end_locations[components.key_type] + 1,
            .Colon,
            .Space,
        );

        // value_type
        try self.renderNode(components.value_type, .None);

        // }
        try self.renderExpectedToken(
            end_locations[components.value_type] + 1,
            .RightBrace,
            if (is_optional)
                .None
            else
                space,
        );

        // ?
        if (is_optional) {
            try self.renderExpectedToken(
                end_locations[components.value_type] + 2,
                .Question,

                space,
            );
        }
    }

    fn renderNamespace(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        // namespace
        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Namespace,
            .Space,
        );

        // qualified name
        try self.renderQualifiedName(
            self.ast.nodes.items(.components)[node].Namespace,
            .Semicolon,
        );
    }

    fn renderBlockExpression(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const statements = self.ast.nodes.items(.components)[node].BlockExpression;

        // from
        try self.renderExpectedToken(
            locations[node],
            .From,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // {
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftBrace,
            if (statements.len > 0)
                .Newline
            else
                .None,
        );

        for (statements) |stmt| {
            try self.ais.pushSpace(self.allocator, .Semicolon);
            try self.renderNode(stmt, .Semicolon);
            self.ais.popSpace();
        }
        self.ais.popIndent();

        // }
        try self.renderExpectedToken(
            if (statements.len > 0)
                self.ast.nodes.items(.end_location)[statements[statements.len - 1]] + 1
            else
                locations[node] + 1,
            .RightBrace,
            space,
        );
    }

    fn renderDot(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Dot;

        // callee
        try self.renderNode(components.callee, .None);

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        // .
        try self.renderExpectedToken(
            end_locations[components.callee] + 1,
            .Dot,
            .None,
        );

        // identifier
        try self.renderExpectedToken(
            components.identifier,
            .Identifier,
            switch (components.member_kind) {
                .Ref, .EnumCase => space,
                .Value => .Space,
                else => .None,
            },
        );

        switch (components.member_kind) {
            .Value => {
                // =
                try self.renderToken(
                    components.value_or_call_or_enum.Value.assign_token,
                    .Space,
                );

                // value
                try self.renderNode(
                    components.value_or_call_or_enum.Value.value,
                    space,
                );
            },
            .Call => {
                if (components.generic_resolve) |gen_resolve| {
                    try self.renderNode(gen_resolve, .None);
                }

                // call
                try self.renderNode(
                    components.value_or_call_or_enum.Call,
                    space,
                );
            },
            .Ref, .EnumCase => {},
        }
    }

    fn renderDoUntil(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].DoUntil;

        // do
        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Do,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        if (components.label) |label| {
            // :label
            try self.renderExpectedTokenSequence(
                label - 1,
                &.{ .Colon, .Identifier },
                .Space,
            );
        }

        self.ais.popIndent();

        // block
        try self.renderNode(components.body, .Space);

        // until
        try self.renderExpectedToken(
            end_locations[components.body] + 1,
            .Until,
            .Space,
        );

        // (
        try self.renderExpectedToken(
            end_locations[components.body] + 2,
            .LeftParen,
            .None,
        );

        // expr
        try self.renderNode(
            components.condition,
            .None,
        );

        // )
        try self.renderExpectedToken(
            end_locations[components.condition] + 1,
            .RightParen,
            .Newline,
        );
    }

    fn renderEnum(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Enum;

        // enum
        try self.renderExpectedToken(
            locations[node],
            .Enum,
            if (components.case_type != null)
                .None
            else
                .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        if (components.case_type) |case_type| {
            // <
            try self.renderExpectedToken(
                locations[node] + 1,
                .Less,
                .None,
            );

            try self.renderNode(case_type, .None);

            // >
            try self.renderExpectedToken(
                end_locations[case_type] + 1,
                .Greater,
                .Space,
            );
        }

        // identifier
        try self.renderExpectedToken(
            components.name,
            .Identifier,
            .Space,
        );

        // {
        try self.renderExpectedToken(
            components.name + 1,
            .LeftBrace,
            .Newline,
        );

        var token_idx = components.name + 1;

        // cases
        for (components.cases) |case| {
            // docblock
            if (case.docblock) |docblock| {
                try self.renderExpectedToken(
                    docblock,
                    .Docblock,
                    .None,
                );
            }

            // case
            try self.renderExpectedToken(
                case.name,
                .Identifier,
                if (!components.values_omitted and case.value != null)
                    .Space
                else
                    .Comma,
            );

            token_idx = case.name + 1;

            if (!components.values_omitted) {
                if (case.value) |value| {
                    // =
                    try self.renderExpectedToken(
                        locations[value] - 1,
                        .Equal,
                        .Space,
                    );

                    // value
                    try self.renderNode(value, .Comma);

                    token_idx = locations[value] + 1;
                }
            }
        }

        self.ais.popIndent();

        // }
        try self.renderExpectedToken(
            token_idx + 1,
            .RightBrace,
            space,
        );
    }

    fn renderExport(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].Export;

        // export
        try self.renderExpectedToken(
            locations[node],
            .Export,
            .Space,
        );

        if (components.declaration) |decl| {
            try self.renderNode(decl, space);
        } else {
            try self.renderQualifiedName(
                components.name.?,
                if (components.alias != null) .Space else space,
            );

            if (components.alias) |alias| {
                // as
                try self.renderExpectedToken(
                    alias - 1,
                    .As,
                    .Space,
                );

                // alias
                try self.renderExpectedToken(
                    alias,
                    .Identifier,
                    space,
                );
            }
        }
    }

    fn renderFiberType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].FiberType;

        // fib
        try self.renderExpectedToken(
            locations[node],
            .Fib,
            .None,
        );

        // <
        try self.renderExpectedToken(
            locations[node] + 1,
            .Less,
            .None,
        );

        try self.renderNode(components.return_type, .CommaSpace);
        try self.renderNode(components.yield_type, .None);

        // >
        try self.renderExpectedToken(
            self.ast.nodes.items(.end_location)[components.yield_type] + 1,
            .Greater,
            space,
        );
    }

    fn renderFor(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].For;

        // for
        try self.renderExpectedToken(
            locations[node],
            .For,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // (
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftParen,
            .None,
        );

        // init_declarations;
        for (components.init_declarations, 0..) |decl, i| {
            try self.renderNode(
                decl,
                if (components.init_declarations.len > 1 and i < components.init_declarations.len - 1)
                    .CommaSpace
                else
                    .SemicolonSpace,
            );
        }

        // condition;
        try self.renderNode(
            components.condition,
            .SemicolonSpace,
        );

        // post_loop
        for (components.post_loop, 0..) |expr, i| {
            try self.renderNode(
                expr,
                if (components.post_loop.len > 1 and i < components.post_loop.len - 1)
                    .CommaSpace
                else
                    .None,
            );
        }

        // )
        const token_idx = end_locations[
            if (components.post_loop.len > 0)
                components.post_loop[components.post_loop.len - 1]
            else
                components.condition
        ] + 1;
        try self.renderExpectedToken(
            token_idx,
            .RightParen,
            .Space,
        );

        if (components.label) |label| {
            // :label
            try self.renderExpectedTokenSequence(
                label - 1,
                &.{ .Colon, .Identifier },
                .Space,
            );
        }

        self.ais.popIndent();

        // body
        try self.renderNode(
            components.body,
            space,
        );
    }

    fn renderForceUnwrap(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].ForceUnwrap;

        // expr
        try self.renderNode(components.unwrapped, .None);

        // !
        try self.renderExpectedToken(
            end_locations[components.unwrapped] + 1,
            .Bang,
            space,
        );
    }

    fn renderUnwrap(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Unwrap;

        // expr
        try self.renderNode(components.unwrapped, .None);

        // ?
        try self.renderExpectedToken(
            end_locations[components.unwrapped] + 1,
            .Question,
            space,
        );
    }

    fn renderForEach(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].ForEach;

        // foreach
        try self.renderExpectedToken(
            locations[node],
            .ForEach,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // (
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftParen,
            .None,
        );

        // key,
        if (!components.key_omitted) {
            try self.renderNode(
                components.key,
                .CommaSpace,
            );
        }

        // value
        try self.renderNode(components.value, .Space);

        // in
        try self.renderExpectedToken(
            if (components.key_omitted)
                end_locations[components.value] + 2
            else
                end_locations[components.value] + 1,
            .In,
            .Space,
        );

        // iterable
        try self.renderNode(components.iterable, .None);

        // )
        try self.renderExpectedToken(
            end_locations[components.iterable] + 1,
            .RightParen,
            .Space,
        );

        self.ais.popIndent();

        if (components.label) |label| {
            // :label
            try self.renderExpectedTokenSequence(
                label - 1,
                &.{ .Colon, .Identifier },
                .Space,
            );
        }

        // body
        try self.renderNode(components.body, space);
    }

    fn renderGenericResolve(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].GenericResolve;

        try self.renderNode(components.expression, .None);

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        // ::<
        try self.renderExpectedTokenSequence(
            end_locations[components.expression] + 1,
            &.{ .DoubleColon, .Less },
            .None,
        );

        for (components.resolved_types, 0..) |rt, i| {
            try self.renderNode(
                rt,
                if (components.resolved_types.len > 1 and i < components.resolved_types.len - 1)
                    .CommaSpace
                else
                    .None,
            );
        }

        // >
        try self.renderExpectedToken(
            end_locations[components.resolved_types[components.resolved_types.len - 1]] + 1,
            .Greater,
            space,
        );
    }

    fn renderGenericResolveType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const resolved_types = self.ast.nodes.items(.components)[node].GenericResolveType;

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        // ::<
        try self.renderExpectedTokenSequence(
            self.ast.nodes.items(.location)[node],
            &.{ .DoubleColon, .Less },
            .None,
        );

        for (resolved_types, 0..) |rt, i| {
            try self.renderNode(
                rt,
                if (resolved_types.len > 1 and i < resolved_types.len - 1)
                    .CommaSpace
                else
                    .None,
            );
        }

        // >
        try self.renderExpectedToken(
            end_locations[resolved_types[resolved_types.len - 1]] + 1,
            .Greater,
            space,
        );
    }

    fn renderGenericType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const location = self.ast.nodes.items(.location)[node];
        const is_optional = self.ast.nodes.items(.type_def)[node].?.optional;

        try self.renderExpectedToken(
            location,
            .Identifier,
            if (is_optional)
                .None
            else
                space,
        );

        if (is_optional) {
            try self.renderExpectedToken(
                location + 1,
                .Question,
                space,
            );
        }
    }

    fn renderGrouping(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Grouping;

        // (
        try self.renderExpectedToken(
            locations[node],
            .LeftParen,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // expr
        try self.renderNode(components, .None);

        self.ais.popIndent();

        // )
        try self.renderExpectedToken(
            end_locations[components] + 1,
            .RightParen,
            space,
        );
    }

    fn renderIf(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].If;
        const tags = self.ast.nodes.items(.tag);

        // if
        try self.renderExpectedToken(
            locations[node],
            .If,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // (
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftParen,
            .None,
        );

        // condition
        try self.renderNode(
            components.condition,
            if (components.unwrapped_identifier != null or components.casted_type != null)
                .Space
            else
                .None,
        );

        if (components.casted_type) |ct| {
            // as
            try self.renderExpectedToken(
                end_locations[components.condition] + 1,
                .As,
                .Space,
            );

            // identifier
            try self.renderExpectedToken(
                components.unwrapped_identifier.?,
                .Identifier,
                .None,
            );

            // :
            try self.renderExpectedToken(
                components.unwrapped_identifier.? + 1,
                .Colon,
                .Space,
            );

            // casted_type
            try self.renderNode(ct, .None);
        } else if (components.unwrapped_identifier) |ui| {
            // ->
            try self.renderExpectedToken(
                end_locations[components.condition] + 1,
                .Arrow,
                .Space,
            );

            // identifier
            try self.renderExpectedToken(
                ui,
                .Identifier,
                .None,
            );
        }

        if (components.is_statement) {
            self.ais.popIndent();
        }

        // )
        try self.renderExpectedToken(
            if (components.casted_type) |ct|
                end_locations[ct] + 1
            else if (components.unwrapped_identifier) |ui|
                ui + 1
            else
                end_locations[components.condition] + 1,
            .RightParen,
            if (!components.is_statement)
                .Newline
            else
                .Space,
        );

        // body
        try self.renderNode(
            components.body,
            if (components.else_branch != null)
                if (!components.is_statement)
                    .Newline
                else
                    .Space
            else
                space,
        );

        if (!components.is_statement) {
            self.ais.popIndent();
        }

        if (components.else_branch) |eb| {
            // else
            try self.renderExpectedToken(
                end_locations[components.body] + 1,
                .Else,
                if (!components.is_statement and tags[eb] != .If)
                    .Newline
                else
                    .Space,
            );

            // else_branch
            try self.renderNode(eb, space);
        }
    }

    fn renderImport(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].Import;

        // import
        try self.renderExpectedToken(
            locations[node],
            .Import,
            .Space,
        );

        if (components.imported_symbols.len > 0) {
            for (components.imported_symbols, 0..) |symbol, i| {
                try self.renderExpectedToken(
                    symbol,
                    .Identifier,
                    if (components.imported_symbols.len > 1 and i < components.imported_symbols.len - 1)
                        .CommaSpace
                    else
                        .Space,
                );
            }

            // from
            try self.renderExpectedToken(
                components.imported_symbols[components.imported_symbols.len - 1] + 1,
                .From,
                .Space,
            );
        }

        // path
        try self.renderExpectedToken(
            components.path,
            .String,
            if (components.prefix != null) .Space else space,
        );

        if (components.prefix) |prefix| {
            // as
            try self.renderExpectedToken(
                prefix[0] - 1,
                .As,
                .Space,
            );

            // prefix
            try self.renderQualifiedName(prefix, space);
        }
    }

    fn renderObjectDeclaration(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const type_defs = self.ast.nodes.items(.type_def);
        const components = self.ast.nodes.items(.components)[node].ObjectDeclaration;
        const fields = type_defs[node].?.resolved_type.?
            .Object.fields;

        // docblock
        if (self.ast.nodes.items(.docblock)[node]) |docblock| {
            try self.renderExpectedToken(
                docblock,
                .Docblock,
                .None,
            );
        }

        // object
        try self.renderExpectedToken(
            locations[node],
            .Object,
            if (components.protocols.len > 0)
                .None
            else
                .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        if (components.protocols.len > 0) {
            // <
            try self.renderExpectedToken(
                locations[node] + 1,
                .Less,
                .None,
            );

            try self.ais.pushIndent(self.allocator, .normal);

            for (components.protocols, 0..) |p, i| {
                try self.renderNode(
                    p,
                    if (components.protocols.len > 1 and i < components.protocols.len - 1)
                        .CommaSpace
                    else
                        .None,
                );
            }

            self.ais.popIndent();

            // >
            try self.renderExpectedToken(
                end_locations[components.protocols[components.protocols.len - 1]] + 1,
                .Greater,
                .Space,
            );
        }

        // name
        try self.renderExpectedToken(
            components.name,
            .Identifier,
            if (components.generics.len == 0)
                .Space
            else
                .None,
        );

        if (components.generics.len > 0) {
            // ::
            try self.renderExpectedToken(
                components.name + 1,
                .DoubleColon,
                .None,
            );

            // <
            try self.renderExpectedToken(
                components.name + 2,
                .Less,
                .None,
            );

            try self.ais.pushIndent(self.allocator, .normal);

            for (components.generics, 0..) |g, i| {
                try self.renderToken(
                    g,
                    if (components.generics.len > 1 and i < components.generics.len - 1)
                        .CommaSpace
                    else
                        .None,
                );
            }

            self.ais.popIndent();

            // >
            try self.renderExpectedToken(
                components.generics[components.generics.len - 1] + 1,
                .Greater,
                .Space,
            );
        }

        // {
        var token_idx = if (components.generics.len > 1)
            components.generics[components.generics.len - 1] + 2
        else
            components.name + 1;

        try self.renderExpectedToken(
            token_idx,
            .LeftBrace,
            if (components.members.len > 0)
                .Newline
            else
                .None,
        );

        for (components.members, 0..) |member, i| {
            const field = fields.get(self.ast.tokens.items(.lexeme)[member.name]).?;

            if (member.method) {
                if (i > 0) {
                    try self.ais.insertNewline();
                }
            } else {
                try self.renderExtraNewlineToken(
                    // Oh that's horrendous
                    member.docblock orelse
                        if (field.static)
                            if (field.mutable)
                                if (member.method)
                                    member.name - 3
                                else
                                    member.name - 2
                            else if (member.method)
                                member.name - 2
                            else
                                member.name - 1
                        else if (field.mutable)
                            member.name - 2
                        else if (member.method)
                            locations[member.method_or_default_value.?]
                        else if (field.final)
                            member.name - 1
                        else
                            member.name,
                );
            }

            if (member.docblock) |d| {
                try self.renderExpectedToken(
                    d,
                    .Docblock,
                    .None,
                );
            }

            if (field.static) {
                try self.renderExpectedToken(
                    if (field.mutable)
                        if (member.method)
                            member.name - 3
                        else
                            member.name - 2
                    else if (member.method)
                        member.name - 2
                    else
                        member.name - 1,
                    .Static,
                    .Space,
                );
            }

            if (member.method) {
                if (field.mutable) {
                    // mut
                    try self.renderExpectedToken(
                        member.name - 2,
                        .Mut,
                        .Space,
                    );
                }

                // method
                try self.renderNode(
                    member.method_or_default_value.?,
                    .Newline,
                );

                token_idx = end_locations[member.method_or_default_value.?];
            } else {
                // final
                if (field.final) {
                    try self.renderExpectedToken(
                        member.name - 1,
                        .Final,
                        .Space,
                    );
                }

                // property name
                try self.renderExpectedToken(
                    member.name,
                    .Identifier,
                    .None,
                );

                // :
                try self.renderExpectedToken(
                    member.name + 1,
                    .Colon,
                    .Space,
                );

                const has_default_value = if (member.method_or_default_value) |default_value|
                    !type_defs[member.property_type.?].?.optional or self.ast.nodes.items(.tag)[default_value] != .Null
                else
                    false;

                const terminator: Space = if (field.static)
                    .Semicolon
                else
                    .Comma;

                // type
                try self.renderNode(
                    member.property_type.?,
                    if (has_default_value)
                        .Space
                    else
                        terminator,
                );

                if (has_default_value) {
                    if (member.method_or_default_value) |default_value| {
                        // =
                        try self.renderExpectedToken(
                            end_locations[member.property_type.?] + 1,
                            .Equal,
                            .Space,
                        );

                        // default value
                        try self.renderNode(default_value, terminator);
                    }
                }

                token_idx = if (has_default_value)
                    end_locations[member.method_or_default_value.?] + 1
                else
                    end_locations[member.property_type.?];
            }
        }

        self.ais.popIndent();

        // }
        try self.renderExpectedToken(
            end_locations[node],
            .RightBrace,
            .Newline,
        );
    }

    fn renderProtocolDeclaration(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const type_defs = self.ast.nodes.items(.type_def);
        const components = self.ast.nodes.items(.components)[node].ProtocolDeclaration;

        // docblock
        if (self.ast.nodes.items(.docblock)[node]) |docblock| {
            try self.renderExpectedToken(
                docblock,
                .Docblock,
                .None,
            );
        }

        // protocol
        try self.renderExpectedToken(
            locations[node],
            .Protocol,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // identifier
        try self.renderExpectedToken(
            components.name,
            .Identifier,
            .Space,
        );

        // {
        try self.renderExpectedToken(
            components.name + 1,
            .LeftBrace,
            if (components.methods.len > 0)
                .Newline
            else
                .None,
        );

        // methods
        for (components.methods, 0..) |method, i| {
            if (i > 0) {
                try self.ais.insertNewline();
            }

            if (method.docblock) |docblock| {
                try self.renderExpectedToken(
                    docblock,
                    .Docblock,
                    .None,
                );
            }

            // mut
            if (type_defs[node].?.resolved_type.?
                .Protocol.methods
                .get(
                    type_defs[method.method].?.resolved_type.?
                        .Function.name.string,
                ).?.mutable)
            {
                try self.renderExpectedToken(
                    locations[method.method] - 2,
                    .Mut,
                    .Space,
                );
            }

            // method
            try self.renderNode(method.method, .Semicolon);
        }

        self.ais.popIndent();

        // }
        try self.renderExpectedToken(
            if (components.methods.len > 0)
                end_locations[components.methods[components.methods.len - 1].method] + 2
            else
                components.name + 2,
            .RightBrace,
            .Newline,
        );
    }

    fn renderRange(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Range;

        // low
        try self.renderNode(components.low, .None);

        // ..
        try self.renderExpectedToken(
            end_locations[components.low] + 1,
            .Spread,
            .None,
        );

        // high
        try self.renderNode(components.high, space);
    }

    fn renderResolve(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const expr = self.ast.nodes.items(.components)[node].Resolve;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Resolve,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderNode(expr, space);
    }

    fn renderResume(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const expr = self.ast.nodes.items(.components)[node].Resume;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Resume,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderNode(expr, space);
    }

    fn renderYield(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const expr = self.ast.nodes.items(.components)[node].Yield;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Yield,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderNode(expr, space);
    }

    fn renderSubscript(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Subscript;

        // subscripted
        try self.renderNode(components.subscripted, .None);

        try self.ais.pushIndent(self.allocator, .normal);

        // [
        try self.renderExpectedToken(
            end_locations[components.subscripted] + 1,
            .LeftBracket,
            .None,
        );

        if (components.checked) {
            // ?
            try self.renderExpectedToken(
                end_locations[components.subscripted] + 2,
                .Question,
                .None,
            );
        }

        try self.ais.pushIndent(self.allocator, .normal);

        try self.renderNode(components.index, .None);

        self.ais.popIndent();

        // ]
        try self.renderExpectedToken(
            end_locations[components.index] + 1,
            .RightBracket,
            if (components.value != null)
                .Space
            else
                space,
        );

        if (components.value) |value| {
            // =
            try self.renderExpectedToken(
                locations[value] - 1,
                .Equal,
                .Space,
            );

            // value
            try self.renderNode(value, space);
        }

        self.ais.popIndent();
    }

    fn renderThrow(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const expr = self.ast.nodes.items(.components)[node].Throw.expression;

        try self.renderExpectedToken(
            self.ast.nodes.items(.location)[node],
            .Throw,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);
        defer self.ais.popIndent();

        try self.renderNode(expr, space);
    }

    fn renderTry(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].Try;

        // try
        try self.renderExpectedToken(
            locations[node],
            .Try,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // body
        try self.renderNode(components.body, .Space);

        var token_idx = end_locations[components.body];

        for (components.clauses, 0..) |clause, i| {
            // catch
            try self.renderExpectedToken(
                clause.identifier - 2,
                .Catch,
                .Space,
            );

            // (identifier:
            try self.renderExpectedTokenSequence(
                clause.identifier - 1,
                &.{ .LeftParen, .Identifier, .Colon },
                .Space,
            );

            // type
            try self.renderNode(clause.type_def, .None);

            // )
            try self.renderExpectedToken(
                end_locations[clause.type_def] + 1,
                .RightParen,
                .Space,
            );

            // body
            try self.renderNode(
                clause.body,
                if (i == components.clauses.len - 1 and components.unconditional_clause == null)
                    .Newline
                else
                    .Space,
            );

            token_idx = end_locations[clause.body];
        }

        if (components.unconditional_clause) |clause| {
            // catch
            try self.renderExpectedToken(
                token_idx + 1,
                .Catch,
                .Space,
            );

            try self.renderNode(clause, .Newline);
        }

        self.ais.popIndent();
    }

    fn renderTypeExpression(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].TypeExpression;

        // <
        try self.renderExpectedToken(
            locations[node],
            .Less,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // type
        try self.renderNode(components, .None);

        self.ais.popIndent();

        // >
        try self.renderExpectedToken(
            end_locations[components] + 1,
            .Greater,
            space,
        );
    }

    fn renderTypeOfExpression(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].TypeOfExpression;

        // typeof
        try self.renderExpectedToken(
            locations[node],
            .TypeOf,
            .Space,
        );

        // expression
        try self.renderNode(components, space);
    }

    fn renderUnary(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].Unary;

        // operator
        try self.renderExpectedToken(
            locations[node],
            components.operator,
            .None,
        );

        // expression
        try self.renderNode(components.expression, space);
    }

    fn renderUserType(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const components = self.ast.nodes.items(.components)[node].UserType;
        const type_def = self.ast.nodes.items(.type_def)[node].?;
        const is_optional = type_def.optional;

        if (type_def.isMutable()) {
            try self.renderExpectedToken(
                components.name[0] - 1,
                .Mut,
                .Space,
            );
        }

        // user type
        try self.renderQualifiedName(
            components.name,
            if (components.generic_resolve != null or is_optional)
                .None
            else
                space,
        );

        // ?
        if (is_optional) {
            try self.renderExpectedToken(
                components.name[components.name.len - 1] + 1,
                .Question,
                if (components.generic_resolve != null)
                    .None
                else
                    space,
            );
        }

        // generic resolve
        if (components.generic_resolve) |gn| {
            try self.renderNode(gn, space);
        }
    }

    fn renderVarDeclaration(self: *Self, node: Ast.Node.Index, space: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].VarDeclaration;

        // docblock
        if (self.ast.nodes.items(.docblock)[node]) |docblock| {
            try self.renderExpectedToken(
                docblock,
                .Docblock,
                .None,
            );
        }

        // var/final
        if (!components.omits_qualifier) {
            try self.renderExpectedToken(
                locations[node],
                if (components.final) .Final else .Var,
                .Space,
            );
        }

        try self.ais.pushIndent(self.allocator, .normal);

        // identifier
        if (!self.ast.tokens.items(.utility_token)[components.name]) {
            try self.renderExpectedToken(
                components.name,
                .Identifier,
                if (components.type != null)
                    .None
                else if (components.value != null)
                    .Space
                else
                    space,
            );
        }

        if (components.type) |t| {
            // :
            try self.renderExpectedToken(
                locations[t] - 1,
                .Colon,
                .Space,
            );

            // type
            try self.renderNode(
                t,
                if (components.value != null) .Space else space,
            );
        }

        if (components.value) |value| {
            // =
            try self.renderExpectedToken(
                locations[value] - 1,
                .Equal,
                .Space,
            );

            // value
            try self.renderNode(value, space);
        }

        self.ais.popIndent();
    }

    fn renderWhile(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const end_locations = self.ast.nodes.items(.end_location);
        const components = self.ast.nodes.items(.components)[node].While;

        // while
        try self.renderExpectedToken(
            locations[node],
            .While,
            .Space,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // (
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftParen,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // expr
        try self.renderNode(
            components.condition,
            .None,
        );

        self.ais.popIndent();

        // )
        try self.renderExpectedToken(
            end_locations[components.condition] + 1,
            .RightParen,
            .Space,
        );

        if (components.label) |label| {
            // :label
            try self.renderExpectedTokenSequence(
                label - 1,
                &.{ .Colon, .Identifier },
                .Space,
            );
        }

        self.ais.popIndent();

        // block
        try self.renderNode(components.body, .Newline);
    }

    fn renderZdef(self: *Self, node: Ast.Node.Index, _: Space) Error!void {
        const locations = self.ast.nodes.items(.location);
        const components = self.ast.nodes.items(.components)[node].Zdef;

        // zdef
        try self.renderExpectedToken(
            locations[node],
            .Zdef,
            .None,
        );

        try self.ais.pushIndent(self.allocator, .normal);

        // (
        try self.renderExpectedToken(
            locations[node] + 1,
            .LeftParen,
            .Newline,
        );

        // lib name,
        try self.renderExpectedTokenSequence(
            components.lib_name,
            &.{ .String, .Comma },
            .Newline,
        );

        // source
        try self.renderExpectedToken(
            components.source,
            .String,
            .Newline,
        );

        self.ais.popIndent();

        // )
        try self.renderExpectedToken(
            components.source + 1,
            .RightParen,
            .Semicolon,
        );
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
    const AutoIndentingStream = struct {
        const SelfAis = @This();

        pub const WriteError = std.io.Writer.Error;
        pub const Writer = std.io.GenericWriter(
            *SelfAis,
            WriteError,
            write,
        );

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

        /// Offset into the source at which formatting has been disabled with
        /// a `buzz fmt: off` comment.
        ///
        /// If non-null, the AutoIndentingStream will not write any bytes
        /// to the underlying writer. It will however continue to track the
        /// indentation level.
        disabled_offset: ?usize = null,

        indent_count: usize = 0,
        indent_delta: usize,
        indent_stack: std.ArrayList(StackElem) = .empty,
        space_stack: std.ArrayList(SpaceElem) = .empty,
        space_mode: ?usize = null,
        disable_indent_committing: usize = 0,
        current_line_empty: bool = true,
        /// the most recently applied indent
        applied_indent: usize = 0,
        underlying_writer: *std.Io.Writer,

        pub fn init(out: *std.Io.Writer, indent_delta_: usize) SelfAis {
            return .{
                .indent_delta = indent_delta_,
                .underlying_writer = out,
            };
        }

        pub fn deinit(self: *SelfAis, allocator: std.mem.Allocator) void {
            self.indent_stack.deinit(allocator);
            self.space_stack.deinit(allocator);
        }

        pub fn writer(self: *SelfAis) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *SelfAis, bytes: []const u8) std.Io.Writer.Error!usize {
            try self.applyIndent();
            return try self.writeNoIndent(bytes);
        }

        // Change the indent delta without changing the final indentation level
        pub fn setIndentDelta(self: *SelfAis, new_indent_delta: usize) !void {
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

        fn writeNoIndent(self: *SelfAis, bytes: []const u8) std.Io.Writer.Error!usize {
            if (bytes.len == 0)
                return @as(usize, 0);

            if (self.disabled_offset == null) try self.underlying_writer.writeAll(bytes);
            if (bytes[bytes.len - 1] == '\n')
                self.resetLine();

            return bytes.len;
        }

        pub fn insertNewline(self: *SelfAis) std.Io.Writer.Error!void {
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

        pub fn pushSpace(self: *SelfAis, allocator: std.mem.Allocator, space: Space) Error!void {
            try self.space_stack.append(
                allocator,
                .{
                    .space = space,
                    .indent_count = self.indent_count,
                },
            );
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
        pub fn maybeInsertNewline(self: *SelfAis) std.Io.Writer.Error!void {
            if (!self.current_line_empty)
                try self.insertNewline();
        }

        /// Push default indentation
        /// Doesn't actually write any indentation.
        /// Just primes the stream to be able to write the correct indentation if it needs to.
        pub fn pushIndent(self: *SelfAis, allocator: std.mem.Allocator, indent_type: IndentType) Error!void {
            try self.indent_stack.append(
                allocator,
                .{
                    .indent_type = indent_type,
                    .realized = false,
                },
            );
        }

        /// Forces an indentation level to be realized.
        pub fn forcePushIndent(self: *SelfAis, allocator: std.mem.Allocator, indent_type: IndentType) Error!void {
            try self.indent_stack.append(
                allocator,
                .{
                    .indent_type = indent_type,
                    .realized = true,
                },
            );
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
        fn applyIndent(self: *SelfAis) std.Io.Writer.Error!void {
            const current_indent = self.currentIndent();
            if (self.current_line_empty and current_indent > 0) {
                if (self.disabled_offset == null) {
                    _ = try self.underlying_writer.splatByte(' ', current_indent);
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
                '\t' => _ = try self.underlying_writer.splatByte(' ', self.indent_delta),
                '\r' => {},
                else => _ = try self.underlying_writer.writeByte(byte),
            };
        }
    };
};
