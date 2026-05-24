const std = @import("std");
const o = @import("obj.zig");
const Reporter = @import("Reporter.zig");
const Ast = @import("Ast.zig");
const GC = @import("GC.zig");
const Value = @import("value.zig").Value;
const BuildOptions = @import("build_options");
const io = @import("io.zig");

const NodeCheck = *const fn (
    ast: Ast.Slice,
    reporter: *Reporter,
    gc: *GC,
    current_function_node: ?Ast.Node.Index,
    node: Ast.Node.Index,
) error{OutOfMemory}!bool;

const checkers = [@typeInfo(Ast.Node.Tag).@"enum".fields.len]?NodeCheck{
    null, // AnonymousObjectType
    null, // AnonymousEnumCase
    null, // As
    checkAsyncCall,
    checkBinary,
    null, // Block
    null, // BlockExpression
    null, // Boolean
    null, // Break
    checkCall,
    null, // Continue
    checkDot,
    checkDoUntil,
    checkEnum,
    null, // Export
    null, // Expression
    null, // FiberType
    null, // Double
    checkFor,
    checkForceUnwrap,
    checkForEach,
    checkFunction,
    null, // FunctionType
    null, // FunDeclaration
    checkGenericResolve,
    null, // GenericResolveType
    null, // GenericType
    null, // Grouping
    checkIf,
    null, // Import
    null, // Integer
    null, // Is
    checkList,
    null, // ListType
    checkMap,
    null, // MapType
    checkMatch,
    null, // Namespace
    checkNamedVariable,
    null, // Null
    checkObjectDeclaration,
    checkObjectInit,
    null, // Out
    null, // Pattern
    null, // ProtocolDeclaration
    checkRange,
    checkResolve,
    checkResume,
    checkReturn,
    null, // SimpleType
    null, // String
    null, // StringLiteral
    checkSubscript,
    null, // Throw
    null, // Try
    null, // TypeExpression
    null, // TypeOfExpression
    checkUnary,
    checkUnwrap,
    null, // UserType
    checkVarDeclaration,
    null, // Void
    checkWhile,
    checkYield,
    null, // Zdef
};

/// Typecheck the node (but does not typecheck its leaf)
pub fn check(ast: Ast.Slice, reporter: *Reporter, gc: *GC, current_function_node: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    return if (checkers[@intFromEnum(ast.nodes.items(.tag)[node])]) |checker|
        try checker(ast, reporter, gc, current_function_node, node)
    else
        false;
}

fn inferType(ast: Ast.Slice, reporter: *Reporter, gc: *GC, value_node: Ast.Node.Index, target_type: *o.ObjTypeDef) error{OutOfMemory}!bool {
    const tags = ast.nodes.items(.tag);
    const inferred_target_type = if (target_type.optional)
        target_type.cloneNonOptional(&gc.type_registry) catch return error.OutOfMemory
    else
        target_type;

    return switch (tags[value_node]) {
        .AnonymousEnumCase => populateAnonymousEnumCase(ast, reporter, value_node, inferred_target_type),
        .List => try inferListType(ast, reporter, gc, value_node, inferred_target_type),
        .Map => try inferMapType(ast, reporter, gc, value_node, inferred_target_type),
        .Match => try inferMatchType(ast, reporter, gc, value_node, inferred_target_type),
        else => false,
    };
}

fn inferListType(ast: Ast.Slice, reporter: *Reporter, gc: *GC, value: Ast.Node.Index, target_type: *o.ObjTypeDef) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components);
    const type_defs = ast.nodes.items(.type_def);

    if (target_type.def_type != .List) {
        return false;
    }

    const list = components[value].List;
    const item_type = target_type.resolved_type.?.List.item_type;
    var inferred_item = false;

    for (list.items) |item| {
        // A contextual list type propagates to nested inferred item expressions.
        inferred_item = (try inferType(ast, reporter, gc, item, item_type)) or inferred_item;
    }

    // variable: [T] = [<any>] -> variable: [T] = [<T>].
    // When contextual inference resolved an item, the literal's own parser-inferred
    // placeholder type can be replaced with the contextual list type.
    if (list.explicit_item_type == null and (list.items.len == 0 or inferred_item)) {
        type_defs[value] = target_type.cloneMutable(
            &gc.type_registry,
            type_defs[value].?.isMutable(),
        ) catch return error.OutOfMemory;

        return true;
    }

    return false;
}

fn inferMapType(ast: Ast.Slice, reporter: *Reporter, gc: *GC, value: Ast.Node.Index, target_type: *o.ObjTypeDef) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components);
    const type_defs = ast.nodes.items(.type_def);

    if (target_type.def_type != .Map) {
        return false;
    }

    const map = components[value].Map;
    const key_type = target_type.resolved_type.?.Map.key_type;
    const value_type = target_type.resolved_type.?.Map.value_type;
    var inferred_entry = false;

    for (map.entries) |entry| {
        // A contextual map type propagates to nested inferred key/value expressions.
        inferred_entry = (try inferType(ast, reporter, gc, entry.key, key_type)) or inferred_entry;
        inferred_entry = (try inferType(ast, reporter, gc, entry.value, value_type)) or inferred_entry;
    }

    // variable: {K: V} = {<any: any>} -> variable: {K: V} = {<K: V>}.
    if (map.explicit_key_type == null and
        map.explicit_value_type == null and
        (map.entries.len == 0 or inferred_entry))
    {
        type_defs[value] = target_type.cloneMutable(
            &gc.type_registry,
            type_defs[value].?.isMutable(),
        ) catch return error.OutOfMemory;
        return true;
    }

    return false;
}

fn matchTypeFromBranches(ast: Ast.Slice, gc: *GC, match: Ast.Node.Index) error{OutOfMemory}!?*o.ObjTypeDef {
    const components = ast.nodes.items(.components)[match].Match;
    const type_defs = ast.nodes.items(.type_def);

    var type_def: ?*o.ObjTypeDef = null;
    var is_optional = false;

    for (components.branches) |branch| {
        if (type_defs[branch.expression]) |branch_type_def| {
            is_optional = is_optional or branch_type_def.optional or branch_type_def.def_type == .Void;

            if (branch_type_def.def_type == .Void) {
                if (type_def == null) {
                    type_def = branch_type_def;
                }
            } else if (type_def) |previous| {
                if (previous.def_type == .Void) {
                    type_def = branch_type_def;
                } else if (!previous.eql(branch_type_def)) {
                    type_def = gc.type_registry.any_type;
                    break;
                }
            } else {
                type_def = branch_type_def;
            }
        }
    }

    if (components.else_branch) |else_branch| {
        if (type_defs[else_branch]) |else_type_def| {
            is_optional = is_optional or else_type_def.def_type == .Void or else_type_def.optional;

            if (else_type_def.def_type == .Void) {
                if (type_def == null) {
                    type_def = else_type_def;
                }
            } else if (type_def) |previous| {
                if (previous.def_type == .Void) {
                    type_def = else_type_def;
                } else if (!previous.eql(else_type_def)) {
                    type_def = gc.type_registry.any_type;
                }
            } else {
                type_def = else_type_def;
            }
        }
    }

    return if (type_def != null and is_optional and !type_def.?.optional and type_def.?.def_type != .Void)
        type_def.?.cloneOptional(&gc.type_registry) catch return error.OutOfMemory
    else
        type_def;
}

fn inferMatchType(ast: Ast.Slice, reporter: *Reporter, gc: *GC, value: Ast.Node.Index, target_type: *o.ObjTypeDef) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[value].Match;
    const type_defs = ast.nodes.items(.type_def);
    var inferred_branch = false;

    for (components.branches) |branch| {
        inferred_branch = (try inferType(
            ast,
            reporter,
            gc,
            branch.expression,
            target_type,
        )) or inferred_branch;
    }

    if (components.else_branch) |else_branch| {
        inferred_branch = (try inferType(
            ast,
            reporter,
            gc,
            else_branch,
            target_type,
        )) or inferred_branch;
    }

    if (inferred_branch) {
        type_defs[value] = try matchTypeFromBranches(
            ast,
            gc,
            value,
        );
    }

    return inferred_branch;
}

fn populateAnonymousEnumCase(ast: Ast.Slice, reporter: *Reporter, value: Ast.Node.Index, target_type: *o.ObjTypeDef) bool {
    const type_defs = ast.nodes.items(.type_def);

    if (target_type.def_type == .EnumInstance) {
        const locations = ast.nodes.items(.location);
        const end_locations = ast.nodes.items(.end_location);
        const components = ast.nodes.items(.components)[value].AnonymousEnumCase;
        const case_name = ast.tokens.items(.lexeme)[components.case_name];
        const enum_type_def = target_type.resolved_type.?.EnumInstance.of.resolved_type.?.Enum;

        for (enum_type_def.cases) |case| {
            if (std.mem.eql(u8, case, case_name)) {
                type_defs[value] = target_type;

                return true;
            }
        }

        reporter.reportErrorFmt(
            .inferred_type,
            ast.tokens.get(locations[value]),
            ast.tokens.get(end_locations[value]),
            "Could not infer type for enum case `{s}`.",
            .{case_name},
        );

        return true;
    }

    if (type_defs[value].?.def_type == .Placeholder) {
        const locations = ast.nodes.items(.location);
        const end_locations = ast.nodes.items(.end_location);
        reporter.reportErrorAt(
            .inferred_type,
            ast.tokens.get(locations[value]),
            ast.tokens.get(end_locations[value]),
            "Could not infer type for enum case.",
        );

        return true;
    }

    return false;
}

fn checkBinary(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const components = ast.nodes.items(.components);
    const node_components = components[node];
    const locations = ast.nodes.items(.location);
    const node_location = locations[node];
    const end_locations = ast.nodes.items(.end_location);
    const node_end_location = end_locations[node];
    var left_type = type_defs[node_components.Binary.left] orelse gc.type_registry.any_type;
    var right_type = type_defs[node_components.Binary.right] orelse gc.type_registry.any_type;

    var had_error = false;

    if (node_components.Binary.operator == .QuestionQuestion and left_type.def_type != .Placeholder) {
        const fallback_type = left_type.cloneNonOptional(&gc.type_registry) catch return error.OutOfMemory;
        // `a ?? b` gives `b` the non-optional type of `a`.
        if (try inferType(ast, reporter, gc, node_components.Binary.right, fallback_type)) {
            right_type = type_defs[node_components.Binary.right] orelse gc.type_registry.any_type;
        }

        type_defs[node] = right_type;
    } else if (left_type.def_type != .Placeholder) {
        // A concrete left operand can provide context for an inferred right operand.
        if (try inferType(ast, reporter, gc, node_components.Binary.right, left_type)) {
            right_type = type_defs[node_components.Binary.right] orelse gc.type_registry.any_type;
        }
    }

    if (node_components.Binary.operator != .QuestionQuestion and
        right_type.def_type != .Placeholder)
    {
        // A concrete right operand can provide context for an inferred left operand.
        if (try inferType(ast, reporter, gc, node_components.Binary.left, right_type)) {
            left_type = type_defs[node_components.Binary.left] orelse gc.type_registry.any_type;
        }
    }

    switch (node_components.Binary.operator) {
        .QuestionQuestion,
        .Ampersand,
        .Bor,
        .Xor,
        .ShiftLeft,
        .ShiftRight,
        .Plus,
        .Minus,
        .Star,
        .Slash,
        .Percent,
        .And,
        .Or,
        => {
            if (!left_type.eql(right_type)) {
                reporter.reportTypeCheck(
                    .binary_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    left_type,
                    ast.tokens.get(locations[node_components.Binary.right]),
                    ast.tokens.get(end_locations[node_components.Binary.right]),
                    right_type,
                    "Type mismatch",
                );

                had_error = true;
            }
        },

        .Greater,
        .Less,
        .GreaterEqual,
        .LessEqual,
        .BangEqual,
        .EqualEqual,
        => {
            // We allow comparison between double and int so raise error if type != and one operand is not a number
            if (!left_type.eql(right_type) and
                !right_type.eql(left_type) and
                ((left_type.def_type != .Integer and left_type.def_type != .Double) or
                    (right_type.def_type != .Integer and right_type.def_type != .Double)))
            {
                reporter.reportTypeCheck(
                    .comparison_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    left_type,
                    ast.tokens.get(locations[node_components.Binary.right]),
                    ast.tokens.get(end_locations[node_components.Binary.right]),
                    right_type,
                    "Type mismatch",
                );

                had_error = true;
            }
        },

        else => {
            reporter.reportErrorAt(
                .syntax,
                ast.tokens.get(node_location),
                ast.tokens.get(node_end_location),
                "Unexpected binary operator.",
            );

            had_error = true;
        },
    }

    if (node_components.Binary.operator != .QuestionQuestion and
        node_components.Binary.operator != .EqualEqual and
        node_components.Binary.operator != .BangEqual)
    {
        if (left_type.optional) {
            reporter.reportErrorAt(
                .binary_operand_type,
                ast.tokens.get(locations[node_components.Binary.left]),
                ast.tokens.get(end_locations[node_components.Binary.left]),
                "Binary operand can't be optional",
            );

            had_error = true;
        }

        if (right_type.optional) {
            reporter.reportErrorAt(
                .binary_operand_type,
                ast.tokens.get(locations[node_components.Binary.right]),
                ast.tokens.get(end_locations[node_components.Binary.right]),
                "Binary operand can't be optional",
            );

            had_error = true;
        }
    }

    switch (node_components.Binary.operator) {
        .EqualEqual, .BangEqual => {},
        .QuestionQuestion => if (!left_type.optional) {
            reporter.reportErrorAt(
                .optional,
                ast.tokens.get(locations[node_components.Binary.left]),
                ast.tokens.get(end_locations[node_components.Binary.left]),
                "Not an optional",
            );
            had_error = true;
        },
        .Ampersand,
        .Bor,
        .Xor,
        .ShiftLeft,
        .ShiftRight,
        => {
            if (left_type.def_type != .Integer) {
                reporter.reportErrorAt(
                    .bitwise_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    "Expected `int`.",
                );
                had_error = true;
            }
        },
        .Greater,
        .Less,
        .GreaterEqual,
        .LessEqual,
        .Minus,
        .Star,
        .Slash,
        .Percent,
        => {
            if (left_type.def_type != .Integer and left_type.def_type != .Double) {
                reporter.reportErrorAt(
                    .comparison_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    "Expected `int` or `double`.",
                );
                had_error = true;
            }
        },
        .Plus => {
            if (left_type.def_type != .Integer and
                left_type.def_type != .Double and
                left_type.def_type != .String and
                left_type.def_type != .List and
                left_type.def_type != .Map)
            {
                reporter.reportErrorAt(
                    .arithmetic_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    "Expected a `int`, `double`, `str`, list or map.",
                );
                had_error = true;
            }
        },
        .And,
        .Or,
        => {
            if (left_type.def_type != .Boolean) {
                reporter.reportErrorAt(
                    .logical_operand_type,
                    ast.tokens.get(locations[node_components.Binary.left]),
                    ast.tokens.get(end_locations[node_components.Binary.left]),
                    "`and` expects operands to be `bool`",
                );
                had_error = true;
            }
        },
        else => {
            reporter.reportErrorAt(
                .syntax,
                ast.tokens.get(node_location),
                ast.tokens.get(node_end_location),
                "Unexpected binary operator.",
            );

            had_error = true;
        },
    }

    return had_error;
}

fn checkCall(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].Call;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const lexemes = ast.tokens.items(.lexeme);

    var had_error = false;

    const callee_type_def = type_defs[components.callee].?;

    // This is not a call but an Enum(value)
    if (callee_type_def.def_type == .Enum) {
        if (components.is_async) {
            reporter.reportErrorAt(
                .fiber_call_not_allowed,
                ast.tokens.get(locations[components.callee]),
                ast.tokens.get(end_locations[components.callee]),
                "Can't be wrapped in a fiber",
            );
            had_error = true;
        }

        if (components.catch_default != null) {
            reporter.reportErrorAt(
                .no_error,
                ast.tokens.get(locations[components.callee]),
                ast.tokens.get(end_locations[components.callee]),
                "Doesn't raise any error",
            );
            had_error = true;
        }

        if (components.arguments.len != 1) {
            reporter.reportErrorAt(
                .enum_argument,
                ast.tokens.get(locations[components.callee]),
                ast.tokens.get(end_locations[components.callee]),
                "Enum instanciation requires only value argument",
            );
            had_error = true;
        }

        return had_error;
    }

    // Find out if call is invoke or regular call
    var invoked = false;
    var invoked_on: ?o.ObjTypeDef.Type = null;

    if (ast.nodes.items(.tag)[components.callee] == .Dot) {
        const dot = node_components[components.callee].Dot;
        const field_accessed = type_defs[dot.callee].?;

        if (field_accessed.def_type == .Placeholder) {
            reporter.reportPlaceholder(ast, field_accessed.resolved_type.?.Placeholder);
            had_error = true;
        }

        invoked = field_accessed.def_type != .Object;
        invoked_on = field_accessed.def_type;
    }

    const callee_type = switch (ast.nodes.items(.tag)[components.callee]) {
        .Dot => node_components[components.callee].Dot.member_type_def,
        else => type_defs[components.callee],
    };

    if (callee_type == null) {
        reporter.reportErrorAt(
            .undefined,
            ast.tokens.get(locations[components.callee]),
            ast.tokens.get(end_locations[components.callee]),
            "Callee is not defined",
        );

        had_error = true;
    } else if (callee_type.?.def_type != .Function) {
        reporter.reportErrorAt(
            .callable,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            "Can't be called",
        );

        had_error = true;

        // return null;
    } else if (callee_type.?.optional) {
        reporter.reportErrorAt(
            .callable,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            "Function maybe null and can't be called",
        );

        had_error = true;
    }

    // Arguments
    const args = callee_type.?.resolved_type.?.Function.parameters;
    const defaults = callee_type.?.resolved_type.?.Function.defaults;
    const arg_keys = args.keys();
    const arg_count = arg_keys.len;

    var missing_arguments = std.StringArrayHashMapUnmanaged(usize).empty;
    defer missing_arguments.deinit(gc.allocator);
    for (arg_keys, 0..) |arg_name, pindex| {
        try missing_arguments.put(
            gc.allocator,
            arg_name.string,
            pindex,
        );
    }

    if (components.arguments.len > arg_count) {
        reporter.reportErrorAt(
            .call_arguments,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            "Too many arguments.",
        );
        had_error = true;
    }

    for (components.arguments, 0..) |argument, index| {
        if (index >= arg_count) {
            break;
        }

        var argument_type_def = type_defs[argument.value].?;
        const arg_key = if (argument.name) |arg_name|
            gc.copyString(lexemes[arg_name]) catch return error.OutOfMemory
        else
            null;
        const actual_arg_key = if (index == 0 and arg_key == null)
            arg_keys[0]
        else
            arg_key.?;
        const def_arg_type = args.get(actual_arg_key);

        if (def_arg_type) |arg_type| {
            // Function signatures provide contextual types for inferred arguments.
            _ = try inferType(ast, reporter, gc, argument.value, arg_type);
            argument_type_def = type_defs[argument.value].?;

            if (!arg_type.eql(argument_type_def)) {
                reporter.reportTypeCheck(
                    .call_argument_type,
                    ast.tokens.get(locations[components.callee]),
                    ast.tokens.get(end_locations[components.callee]),
                    arg_type,
                    ast.tokens.get(locations[argument.value]),
                    ast.tokens.get(end_locations[argument.value]),
                    argument_type_def,
                    "Bad argument type",
                );
                had_error = true;
            }

            _ = missing_arguments.orderedRemove(actual_arg_key.string);
        } else {
            reporter.reportErrorFmt(
                .call_arguments,
                ast.tokens.get(locations[argument.value]),
                ast.tokens.get(end_locations[argument.value]),
                "Argument `{s}` does not exists.",
                .{if (arg_key) |key| key.string else "unknown"},
            );
            had_error = true;
        }
    }

    // Default arguments
    if (missing_arguments.count() > 0) {
        var tmp_missing_arguments = try missing_arguments.clone(gc.allocator);
        defer tmp_missing_arguments.deinit(gc.allocator);
        const missing_keys = tmp_missing_arguments.keys();
        for (missing_keys) |missing_key| {
            if (defaults.get(gc.copyString(missing_key) catch return error.OutOfMemory) != null) {
                _ = missing_arguments.orderedRemove(missing_key);
            }
        }
    }

    // Not enough arguments?
    if (missing_arguments.count() > 0) {
        var missing = std.Io.Writer.Allocating.init(gc.allocator);
        defer missing.deinit();

        for (missing_arguments.keys(), 0..) |key, i| {
            missing.writer.print(
                "{s}{s}",
                .{
                    key,
                    if (i < missing_arguments.keys().len - 1)
                        ", "
                    else
                        "",
                },
            ) catch return error.OutOfMemory;
        }

        reporter.reportErrorFmt(
            .call_arguments,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            "Missing argument{s}: {s}",
            .{
                if (missing_arguments.count() > 1)
                    "s"
                else
                    "",
                missing.written(),
            },
        );

        had_error = true;
    }

    // Catch clause
    const error_types = callee_type.?.resolved_type.?.Function.error_types;
    if (components.catch_default) |catch_default| {
        const node_type_def = type_defs[node].?;
        // Inline catch defaults must produce the same value type as the call.
        _ = try inferType(ast, reporter, gc, catch_default, node_type_def);
        const catch_default_type_def = type_defs[catch_default].?;
        if (error_types == null or error_types.?.len == 0) {
            reporter.reportErrorAt(
                .no_error,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                "Function doesn't raise any error",
            );
            had_error = true;
        } else if (error_types != null) {
            // Expression
            if (!node_type_def.eql(catch_default_type_def) and
                !(node_type_def.cloneOptional(&gc.type_registry) catch return error.OutOfMemory)
                    .eql(catch_default_type_def))
            {
                reporter.reportTypeCheck(
                    .inline_catch_type,
                    ast.tokens.get(locations[components.callee]),
                    ast.tokens.get(end_locations[components.callee]),
                    node_type_def,
                    ast.tokens.get(locations[catch_default]),
                    ast.tokens.get(end_locations[catch_default]),
                    catch_default_type_def,
                    "Bad inline catch value type",
                );
                had_error = true;
            }
        }
    }

    return had_error;
}

fn checkDot(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].Dot;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const tags = ast.tokens.items(.tag);

    var had_error = false;

    const callee_type = type_defs[components.callee] orelse gc.type_registry.any_type;

    switch (callee_type.def_type) {
        .ObjectInstance,
        .Object,
        .ProtocolInstance,
        .Enum,
        .EnumInstance,
        .List,
        .Map,
        .String,
        .Pattern,
        .Fiber,
        .ForeignContainer,
        .Range,
        => {},
        else => {
            reporter.reportErrorAt(
                .field_access,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                "Doesn't have field access",
            );
            had_error = true;
        },
    }

    if (callee_type.optional) {
        reporter.reportErrorAt(
            .field_access,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            "Optional doesn't have field access",
        );
        had_error = true;
    }

    switch (callee_type.def_type) {
        .Fiber, .Pattern, .String => {},
        .ForeignContainer, .ObjectInstance, .Object => {
            const field_name = ast.tokens.items(.lexeme)[components.identifier];
            const field = switch (callee_type.def_type) {
                .ObjectInstance => callee_type.resolved_type.?.ObjectInstance.of
                    .resolved_type.?.Object
                    .fields
                    .get(field_name),
                .Object => callee_type.resolved_type.?.Object.fields
                    .get(field_name),
                else => null,
            };

            switch (components.member_kind) {
                .Value => {
                    const value = components.value_or_call_or_enum.Value.value;
                    const assign_token = components.value_or_call_or_enum.Value.assign_token;
                    var value_type_def = type_defs[value].?;

                    // Type check value
                    switch (callee_type.def_type) {
                        .ForeignContainer => {
                            if (callee_type.resolved_type.?.ForeignContainer.buzz_type.get(field_name)) |field_type| {
                                if (!field_type.eql(value_type_def)) {
                                    reporter.reportTypeCheck(
                                        .assignment_value_type,
                                        ast.tokens.get(callee_type.resolved_type.?.ForeignContainer.location),
                                        ast.tokens.get(callee_type.resolved_type.?.ForeignContainer.location),
                                        field_type,
                                        ast.tokens.get(locations[value]),
                                        ast.tokens.get(end_locations[value]),
                                        value_type_def,
                                        "Bad property type",
                                    );
                                    had_error = true;
                                }
                            } else {
                                reporter.reportErrorFmt(
                                    .property_does_not_exists,
                                    ast.tokens.get(components.identifier),
                                    ast.tokens.get(components.identifier),
                                    "List property `{s}` does not exists",
                                    .{
                                        field_name,
                                    },
                                );
                                had_error = true;
                            }
                        },
                        .ObjectInstance, .Object => {
                            if (field == null) {
                                reporter.reportErrorFmt(
                                    .property_does_not_exists,
                                    ast.tokens.get(components.identifier),
                                    ast.tokens.get(components.identifier),
                                    "Property `{s}` does not exists",
                                    .{
                                        field_name,
                                    },
                                );
                                had_error = true;
                            } else if (field.?.method or
                                (callee_type.def_type == .ObjectInstance and field.?.static) or
                                (callee_type.def_type == .Object and !field.?.static))
                            {
                                reporter.reportErrorFmt(
                                    .assignable,
                                    ast.tokens.get(locations[components.callee]),
                                    ast.tokens.get(end_locations[components.callee]),
                                    "`{s}` is not assignable",
                                    .{
                                        field_name,
                                    },
                                );
                                had_error = true;
                            } else if (field.?.final) {
                                reporter.reportErrorFmt(
                                    .constant_property,
                                    ast.tokens.get(locations[components.callee]),
                                    ast.tokens.get(end_locations[components.callee]),
                                    "`{s}` is final",
                                    .{
                                        field_name,
                                    },
                                );
                                had_error = true;
                            } else if (callee_type.def_type == .ObjectInstance and !callee_type.resolved_type.?.ObjectInstance.mutable) {
                                reporter.reportWithOrigin(
                                    .not_mutable,
                                    ast.tokens.get(locations[components.callee]),
                                    ast.tokens.get(end_locations[components.callee]),
                                    ast.tokens.get(
                                        callee_type.resolved_type.?.ObjectInstance.of
                                            .resolved_type.?.Object.location,
                                    ),
                                    ast.tokens.get(
                                        callee_type.resolved_type.?.ObjectInstance.of
                                            .resolved_type.?.Object.location,
                                    ),
                                    "Instance of `{s}` is not mutable",
                                    .{
                                        callee_type.resolved_type.?.ObjectInstance.of
                                            .resolved_type.?.Object.qualified_name.string,
                                    },
                                    "declared here",
                                );
                                had_error = true;
                            }

                            // Field assignments provide the declared field type as context.
                            _ = try inferType(ast, reporter, gc, value, field.?.type_def);
                            value_type_def = type_defs[value].?;

                            if (!field.?.type_def.eql(value_type_def)) {
                                reporter.reportTypeCheck(
                                    .assignment_value_type,
                                    ast.tokens.get(field.?.location),
                                    ast.tokens.get(field.?.location),
                                    field.?.type_def,
                                    ast.tokens.get(locations[value]),
                                    ast.tokens.get(end_locations[value]),
                                    value_type_def,
                                    "Bad property type",
                                );
                                had_error = true;
                            }
                        },
                        else => {
                            reporter.reportErrorAt(
                                .syntax,
                                ast.tokens.get(locations[components.callee]),
                                ast.tokens.get(end_locations[components.callee]),
                                "Callee is not field accessible",
                            );
                            had_error = true;
                        },
                    }

                    // Type check that operator is allowed
                    switch (tags[assign_token]) {
                        .PlusEqual => switch (type_defs[value].?.def_type) {
                            .Integer,
                            .Double,
                            .List,
                            .Map,
                            .String,
                            => {},
                            else => {
                                reporter.report(
                                    .arithmetic_operand_type,
                                    ast.tokens.get(assign_token),
                                    ast.tokens.get(assign_token),
                                    "Addition is only allowed for types `int`, `double`, list, map and `str`",
                                );
                                had_error = true;
                            },
                        },
                        .MinusEqual,
                        .StarEqual,
                        .SlashEqual,
                        .PercentEqual,
                        => switch (type_defs[value].?.def_type) {
                            .Integer, .Double => {},
                            else => {
                                reporter.report(
                                    .arithmetic_operand_type,
                                    ast.tokens.get(assign_token),
                                    ast.tokens.get(assign_token),
                                    "Operator is only allowed for types `int`, `double`",
                                );
                                had_error = true;
                            },
                        },
                        .ShiftRightEqual,
                        .ShiftLeftEqual,
                        .XorEqual,
                        .BorEqual,
                        .BnotEqual,
                        .AmpersandEqual,
                        => if (type_defs[value].?.def_type != .Integer) {
                            reporter.report(
                                .arithmetic_operand_type,
                                ast.tokens.get(assign_token),
                                ast.tokens.get(assign_token),
                                "Operator is only allowed for `int`",
                            );
                            had_error = true;
                        },
                        else => {},
                    }
                },
                .Call => {
                    if (callee_type.def_type == .ForeignContainer) {
                        reporter.reportErrorAt(
                            .callable,
                            ast.tokens.get(locations[components.callee]),
                            ast.tokens.get(end_locations[components.callee]),
                            "Not callable",
                        );
                        had_error = true;
                    }

                    if (field == null) {
                        reporter.reportErrorFmt(
                            .property_does_not_exists,
                            ast.tokens.get(components.identifier),
                            ast.tokens.get(components.identifier),
                            "Property `{s}` does not exists",
                            .{
                                field_name,
                            },
                        );
                        had_error = true;
                    } else if (field.?.mutable and !callee_type.resolved_type.?.ObjectInstance.mutable) {
                        reporter.report(
                            .not_mutable,
                            ast.tokens.get(components.identifier),
                            ast.tokens.get(components.identifier),
                            "Method requires mutable instance",
                        );
                        had_error = true;
                    }
                },
                else => {},
            }
        },
        .ProtocolInstance => if (components.member_kind == .Call) {
            const field_name = ast.tokens.items(.lexeme)[components.identifier];
            const field = callee_type.resolved_type.?.ProtocolInstance.of
                .resolved_type.?.Protocol
                .methods
                .get(field_name);

            if (field == null) {
                reporter.reportErrorFmt(
                    .property_does_not_exists,
                    ast.tokens.get(components.identifier),
                    ast.tokens.get(components.identifier),
                    "Method `{s}` does not exists",
                    .{
                        field_name,
                    },
                );
                had_error = true;
            } else if (field.?.mutable and !callee_type.resolved_type.?.ProtocolInstance.mutable) {
                reporter.report(
                    .not_mutable,
                    ast.tokens.get(components.identifier),
                    ast.tokens.get(components.identifier),
                    "Method requires mutable instance",
                );

                had_error = true;
            }
        },
        .Enum => {},
        .EnumInstance => {},
        .List, .Map, .Range => if (components.member_kind == .Call) {
            const identifier = ast.tokens.items(.lexeme)[components.identifier];

            switch (callee_type.def_type) {
                .List => if (callee_type.resolved_type.?.List.methods.get(identifier)) |member| {
                    if (member.mutable and !callee_type.resolved_type.?.List.mutable) {
                        reporter.reportErrorFmt(
                            .not_mutable,
                            ast.tokens.get(components.identifier),
                            ast.tokens.get(components.identifier),
                            "Method `{s}` requires mutable list",
                            .{
                                identifier,
                            },
                        );
                        had_error = true;
                    }
                } else {
                    reporter.reportErrorFmt(
                        .property_does_not_exists,
                        ast.tokens.get(components.identifier),
                        ast.tokens.get(components.identifier),
                        "List property `{s}` does not exists",
                        .{
                            identifier,
                        },
                    );
                    had_error = true;
                },
                .Map => if (callee_type.resolved_type.?.Map.methods.get(identifier)) |member| {
                    if (member.mutable and !callee_type.resolved_type.?.Map.mutable) {
                        reporter.reportErrorFmt(
                            .not_mutable,
                            ast.tokens.get(components.identifier),
                            ast.tokens.get(components.identifier),
                            "Method `{s}` requires mutable list",
                            .{
                                identifier,
                            },
                        );
                        had_error = true;
                    }
                } else {
                    reporter.reportErrorFmt(
                        .property_does_not_exists,
                        ast.tokens.get(components.identifier),
                        ast.tokens.get(components.identifier),
                        "Map property `{s}` does not exists",
                        .{
                            identifier,
                        },
                    );
                    had_error = true;
                },
                else => {},
            }
        },
        else => std.debug.assert(had_error == true),
    }

    return had_error;
}

fn checkDoUntil(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].DoUntil;

    const condition_type_def = type_defs[components.condition] orelse gc.type_registry.any_type;

    if (condition_type_def.def_type != .Boolean) {
        reporter.reportErrorAt(
            .do_condition_type,
            ast.tokens.get(locations[components.condition]),
            ast.tokens.get(end_locations[components.condition]),
            "`do` condition must be bool",
        );

        return true;
    }

    return false;
}

fn checkEnum(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].Enum;

    var had_error = false;

    const enum_type = type_defs[node].?.resolved_type.?.Enum.enum_type;

    switch (enum_type.def_type) {
        .String,
        .Integer,
        .Double,
        .Pattern,
        .UserData,
        .Void,
        .Range,
        => {},
        else => {
            reporter.reportErrorAt(
                .syntax,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                "Type not allowed as enum value",
            );
            had_error = true;
        },
    }

    for (components.cases) |case| {
        const case_type_def = if (case.value) |value|
            type_defs[value].?
        else
            null;

        if (case_type_def) |case_type| {
            if (!((enum_type.toInstance(&gc.type_registry, false) catch return error.OutOfMemory)).eql(case_type)) {
                reporter.reportTypeCheck(
                    .enum_case_type,
                    ast.tokens.get(locations[node]),
                    ast.tokens.get(end_locations[node]),
                    (enum_type.toInstance(
                        &gc.type_registry,
                        false,
                    ) catch return error.OutOfMemory),
                    ast.tokens.get(locations[case.value.?]),
                    ast.tokens.get(end_locations[case.value.?]),
                    case_type,
                    "Bad enum case type",
                );

                had_error = true;
            }
        }
    }

    return had_error;
}

fn checkFor(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].For;

    const condition_type_def = type_defs[components.condition] orelse gc.type_registry.any_type;
    if (condition_type_def.def_type != .Boolean) {
        reporter.reportErrorAt(
            .for_condition_type,
            ast.tokens.get(locations[components.condition]),
            ast.tokens.get(end_locations[components.condition]),
            "`for` condition must be bool",
        );

        return false;
    }

    return true;
}

fn checkForceUnwrap(ast: Ast.Slice, reporter: *Reporter, _: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = ast.nodes.items(.components)[node].ForceUnwrap;

    if (!components.original_type.optional) {
        reporter.reportErrorAt(
            .optional,
            ast.tokens.get(locations[components.unwrapped]),
            ast.tokens.get(end_locations[components.unwrapped]),
            "Not an optional",
        );

        return false;
    }

    return true;
}

fn checkForEach(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const node_components = ast.nodes.items(.components);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const components = node_components[node].ForEach;

    var had_error = false;

    const iterable_type_def = type_defs[components.iterable] orelse gc.type_registry.any_type;
    var key_type_def = type_defs[components.key] orelse gc.type_registry.any_type;
    const value_type_def = type_defs[components.value] orelse gc.type_registry.any_type;

    if (!components.key_omitted) {
        switch (iterable_type_def.def_type) {
            .String, .List => {
                if (key_type_def.def_type != .Integer) {
                    reporter.reportErrorAt(
                        .foreach_key_type,
                        ast.tokens.get(locations[components.key]),
                        ast.tokens.get(end_locations[components.key]),
                        "Expected `int`.",
                    );
                    had_error = true;
                }
            },
            .Map => {
                if (!iterable_type_def.resolved_type.?.Map.key_type.strictEql(key_type_def)) {
                    reporter.reportTypeCheck(
                        .foreach_key_type,
                        ast.tokens.get(locations[components.iterable]),
                        ast.tokens.get(end_locations[components.iterable]),
                        iterable_type_def.resolved_type.?.Map.key_type,
                        ast.tokens.get(locations[components.key]),
                        ast.tokens.get(end_locations[components.key]),
                        key_type_def,
                        "Bad key type",
                    );
                    had_error = true;
                }
            },
            .Enum => {
                reporter.reportErrorAt(
                    .foreach_key_type,
                    ast.tokens.get(locations[components.key]),
                    ast.tokens.get(end_locations[components.key]),
                    "No key available when iterating over enum.",
                );
                had_error = true;
            },
            .Range => {
                reporter.reportErrorAt(
                    .foreach_key_type,
                    ast.tokens.get(locations[components.key]),
                    ast.tokens.get(end_locations[components.key]),
                    "No key available when iterating over range.",
                );
                had_error = true;
            },
            else => {
                reporter.reportErrorAt(
                    .foreach_iterable,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    "Not iterable.",
                );
                had_error = true;
            },
        }
    } else {
        // Key was omitted, put the correct type in the key var declation to avoid raising errors
        switch (iterable_type_def.def_type) {
            .Map => key_type_def = iterable_type_def.resolved_type.?.Map.key_type,
            .String, .List => key_type_def = gc.type_registry.int_type,
            else => {},
        }
    }

    switch (iterable_type_def.def_type) {
        .Map => {
            if (!iterable_type_def.resolved_type.?.Map.value_type.strictEql(value_type_def)) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    iterable_type_def.resolved_type.?.Map.value_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        .List => {
            if (!iterable_type_def.resolved_type.?.List.item_type.strictEql(value_type_def)) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    iterable_type_def.resolved_type.?.List.item_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        .Range => {
            if (value_type_def.def_type != .Integer or value_type_def.optional) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    gc.type_registry.int_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        .String => {
            if (value_type_def.def_type != .String) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    gc.type_registry.str_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        .Enum => {
            const iterable_type = iterable_type_def.toInstance(
                &gc.type_registry,
                false,
            ) catch return error.OutOfMemory;
            if (!iterable_type.strictEql(value_type_def)) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    iterable_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        .Fiber => {
            const iterable_type = iterable_type_def.resolved_type.?.Fiber.yield_type.toInstance(
                &gc.type_registry,
                false,
            ) catch return error.OutOfMemory;
            if (!iterable_type.strictEql(value_type_def)) {
                reporter.reportTypeCheck(
                    .foreach_value_type,
                    ast.tokens.get(locations[components.iterable]),
                    ast.tokens.get(end_locations[components.iterable]),
                    iterable_type,
                    ast.tokens.get(locations[components.value]),
                    ast.tokens.get(end_locations[components.value]),
                    value_type_def,
                    "Bad value type",
                );
                had_error = true;
            }
        },
        else => {
            reporter.reportErrorAt(
                .foreach_iterable,
                ast.tokens.get(locations[components.iterable]),
                ast.tokens.get(end_locations[components.iterable]),
                "Not iterable.",
            );
            had_error = true;
        },
    }

    return had_error;
}

fn checkFunction(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const node_components = ast.nodes.items(.components);
    const type_defs = ast.nodes.items(.type_def);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = node_components[node].Function;
    const node_type_def = type_defs[node].?;
    const function_def = node_type_def.resolved_type.?.Function;
    const function_signature = if (components.function_signature) |fs|
        node_components[fs].FunctionType
    else
        null;

    var had_error = false;

    // Default values type checking
    var it = function_def.defaults.iterator();
    while (it.next()) |kv| {
        if (function_def.parameters.get(kv.key_ptr.*)) |param| {
            const default_type_def = kv.value_ptr.*.typeOf(gc) catch return error.OutOfMemory;
            if (!param.eql(default_type_def)) {
                // Retrieve default node
                var argument: ?Ast.FunctionType.Argument = null;
                if (function_signature) |signature| {
                    for (signature.arguments) |arg| {
                        const name = ast.tokens.items(.lexeme)[arg.name];
                        if (std.mem.eql(u8, name, kv.key_ptr.*.string)) {
                            argument = arg;
                        }
                    }
                }

                if (default_type_def.isMutable()) {
                    reporter.reportErrorAt(
                        .constant_default,
                        ast.tokens.get(
                            locations[if (argument) |arg| arg.type else node],
                        ),
                        ast.tokens.get(
                            end_locations[if (argument) |arg| arg.type else node],
                        ),
                        "Default value must be constant",
                    );
                    had_error = true;
                }

                reporter.reportTypeCheck(
                    .default_value_type,
                    ast.tokens.get(
                        locations[if (argument) |arg| arg.type else node],
                    ),
                    ast.tokens.get(
                        end_locations[if (argument) |arg| arg.type else node],
                    ),
                    param,
                    ast.tokens.get(
                        locations[if (argument) |arg| arg.default orelse node else node],
                    ),
                    ast.tokens.get(
                        end_locations[if (argument) |arg| arg.default orelse node else node],
                    ),
                    default_type_def,
                    "Bad default value type",
                );
                had_error = true;
            }
        }
    }

    return had_error;
}

fn checkGenericResolve(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_def = ast.nodes.items(.type_def)[node].?;
    const node_location = ast.nodes.items(.location)[node];
    const node_end_location = ast.nodes.items(.end_location)[node];

    var had_error = false;

    switch (type_def.def_type) {
        .Function => {
            const function_type = type_def.resolved_type.?.Function;

            if (function_type.generic_types.count() > 0 and
                (function_type.resolved_generics == null or function_type.resolved_generics.?.len < function_type.generic_types.count()))
            {
                reporter.reportErrorFmt(
                    .generic_type,
                    ast.tokens.get(node_location),
                    ast.tokens.get(node_end_location),
                    "Missing generic types. Expected {} got {}.",
                    .{
                        function_type.generic_types.count(),
                        if (function_type.resolved_generics == null)
                            0
                        else
                            function_type.resolved_generics.?.len,
                    },
                );
                had_error = true;
            } else if (function_type.resolved_generics != null and function_type.resolved_generics.?.len > function_type.generic_types.count()) {
                reporter.reportErrorFmt(
                    .generic_type,
                    ast.tokens.get(node_location),
                    ast.tokens.get(node_end_location),
                    "Too many generic types. Expected {} got {}.",
                    .{
                        function_type.generic_types.count(),
                        if (function_type.resolved_generics == null)
                            0
                        else
                            function_type.resolved_generics.?.len,
                    },
                );
                had_error = true;
            }
        },
        .Object => {
            const object_type = type_def.resolved_type.?.Object;

            if (object_type.generic_types.count() > 0 and
                (object_type.resolved_generics == null or object_type.resolved_generics.?.len < object_type.generic_types.count()))
            {
                reporter.reportErrorFmt(
                    .generic_type,
                    ast.tokens.get(node_location),
                    ast.tokens.get(node_end_location),
                    "Missing generic types. Expected {} got {}.",
                    .{
                        object_type.generic_types.count(),
                        if (object_type.resolved_generics == null)
                            0
                        else
                            object_type.resolved_generics.?.len,
                    },
                );
                had_error = true;
            } else if (object_type.resolved_generics != null and object_type.resolved_generics.?.len > object_type.generic_types.count()) {
                reporter.reportErrorFmt(
                    .generic_type,
                    ast.tokens.get(node_location),
                    ast.tokens.get(node_end_location),
                    "Too many generic types. Expected {} got {}.",
                    .{
                        object_type.generic_types.count(),
                        if (object_type.resolved_generics == null)
                            0
                        else
                            object_type.resolved_generics.?.len,
                    },
                );
                had_error = true;
            }
        },
        else => {
            const type_def_str = type_def.toStringAlloc(gc.allocator, false) catch unreachable;
            defer gc.allocator.free(type_def_str);

            reporter.reportErrorFmt(
                .generic_type,
                ast.tokens.get(node_location),
                ast.tokens.get(node_end_location),
                "Type `{s}` does not support generic types",
                .{
                    type_def_str,
                },
            );
            had_error = true;
        },
    }

    return had_error;
}

fn checkIf(ast: Ast.Slice, reporter: *Reporter, _: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const node_components = ast.nodes.items(.components);
    const components = node_components[node].If;
    const location = locations[node];

    var had_error = false;

    if (!components.is_statement) {
        // Both should have same type
        if (!type_defs[node].?.eql(type_defs[components.body].?)) {
            reporter.reportTypeCheck(
                .inline_if_body_type,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                ast.tokens.get(locations[components.body]),
                ast.tokens.get(end_locations[components.body]),
                type_defs[components.body].?,
                "Inline if body type not matching",
            );
            had_error = true;
        }

        if (!type_defs[node].?.eql(type_defs[components.else_branch.?].?)) {
            reporter.reportTypeCheck(
                .inline_if_else_type,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                ast.tokens.get(locations[components.else_branch.?]),
                ast.tokens.get(end_locations[components.else_branch.?]),
                type_defs[components.else_branch.?].?,
                "Inline if else type not matching",
            );
            had_error = true;
        }
    }

    if (components.casted_type == null and components.unwrapped_identifier == null) {
        if (type_defs[components.condition].?.def_type != .Boolean) {
            reporter.reportErrorAt(
                .if_condition_type,
                ast.tokens.get(locations[components.condition]),
                ast.tokens.get(end_locations[components.condition]),
                "`if` condition must be bool",
            );
            had_error = true;
        }
    } else if (components.casted_type == null) {
        if (!type_defs[components.condition].?.optional) {
            reporter.reportErrorAt(
                .optional,
                ast.tokens.get(locations[components.condition]),
                ast.tokens.get(end_locations[components.condition]),
                "Expected optional",
            );
            had_error = true;
        }
    }

    return had_error;
}

fn checkMatch(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = ast.nodes.items(.components);
    const node_components = components[node].Match;
    const value_type_def = type_defs[node_components.value].?;

    var had_error = false;
    if (!value_type_def.optional) {
        if (value_type_def.def_type == .EnumInstance) {
            for (node_components.branches) |branch| {
                for (branch.conditions) |condition| {
                    _ = try inferType(ast, reporter, gc, condition, value_type_def);
                }
            }
        }

        switch (value_type_def.def_type) {
            // match on string accepts str, pat and type as condition types
            .String => for (node_components.branches) |branch| {
                for (branch.conditions) |condition| {
                    const condition_type_def = type_defs[condition];
                    if (condition_type_def == null or condition_type_def.?.optional or
                        (condition_type_def.?.def_type != .String and
                            condition_type_def.?.def_type != .Pattern and
                            condition_type_def.?.def_type != .Type))
                    {
                        reporter.reportErrorAt(
                            .match_condition_type,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            "`match` condition must be of type `str`, `pat` or `type`",
                        );
                        had_error = true;
                    }
                }
            },
            // match on pattern accepts str, pat and type as condition types
            .Pattern => for (node_components.branches) |branch| {
                for (branch.conditions) |condition| {
                    const condition_type_def = type_defs[condition];
                    if (condition_type_def == null or condition_type_def.?.optional or
                        (condition_type_def.?.def_type != .String and
                            condition_type_def.?.def_type != .Pattern and
                            condition_type_def.?.def_type != .Type))
                    {
                        reporter.reportErrorAt(
                            .match_condition_type,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            "`match` condition must be of type `str`, `pat` or `type`",
                        );
                        had_error = true;
                    }
                }
            },
            // match on number accepts int, double, rg or type
            .Integer, .Double => for (node_components.branches) |branch| {
                for (branch.conditions) |condition| {
                    const condition_type_def = type_defs[condition];
                    if (condition_type_def == null or condition_type_def.?.optional or
                        (condition_type_def.?.def_type != .Integer and
                            condition_type_def.?.def_type != .Range and
                            condition_type_def.?.def_type != .Double and
                            condition_type_def.?.def_type != .Type))
                    {
                        reporter.reportErrorAt(
                            .match_condition_type,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            "`match` condition must be of type `int`, `double`, `rg` or `type`",
                        );
                        had_error = true;
                    }
                }
            },
            // Anything goes: any matches against anything and type will result in equality against another type and `is` or anything else
            .Type, .Any => {},
            else => for (node_components.branches) |branch| {
                for (branch.conditions) |condition| {
                    const condition_type_def = type_defs[condition];
                    if (condition_type_def == null or
                        (!value_type_def.eql(condition_type_def.?) and condition_type_def.?.def_type != .Type))
                    {
                        reporter.reportTypeCheck(
                            .match_condition_type,
                            ast.tokens.get(locations[node_components.value]),
                            ast.tokens.get(end_locations[node_components.value]),
                            value_type_def,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            condition_type_def.?,
                            "Bad `match` condition type",
                        );
                        had_error = true;
                    }
                }
            },
        }
    }

    if (!had_error) {
        const numeric_match_value = !value_type_def.optional and
            (value_type_def.def_type == .Integer or value_type_def.def_type == .Double);
        var seen_values = std.ArrayList(Value).empty;
        defer seen_values.deinit(gc.allocator);
        var seen_conditions = std.ArrayList(Ast.Node.Index).empty;
        defer seen_conditions.deinit(gc.allocator);

        for (node_components.branches) |branch| {
            for (branch.conditions) |condition| {
                const is_constant = ast.isConstant(gc.allocator, condition) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => false,
                };
                if (!is_constant) {
                    continue;
                }

                const condition_value = ast.toValue(condition, gc) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => continue,
                };

                for (seen_values.items, seen_conditions.items) |seen_value, seen_condition| {
                    const duplicate_condition = condition_value.eql(seen_value);
                    var overlapping_condition = false;

                    // Numeric range matches use rg.contains semantics: normalized half-open intervals.
                    if (!duplicate_condition and numeric_match_value) {
                        const condition_type_def = type_defs[condition] orelse continue;
                        const seen_condition_type_def = type_defs[seen_condition] orelse continue;
                        const condition_is_range = !condition_type_def.optional and
                            condition_type_def.def_type == .Range;
                        const seen_condition_is_range = !seen_condition_type_def.optional and
                            seen_condition_type_def.def_type == .Range;

                        if (condition_is_range and seen_condition_is_range) {
                            const condition_range = o.ObjRange.cast(condition_value.obj()).?;
                            const seen_range = o.ObjRange.cast(seen_value.obj()).?;
                            const condition_low = @min(condition_range.low, condition_range.high);
                            const condition_high = @max(condition_range.low, condition_range.high);
                            const seen_low = @min(seen_range.low, seen_range.high);
                            const seen_high = @max(seen_range.low, seen_range.high);

                            overlapping_condition = @max(condition_low, seen_low) < @min(condition_high, seen_high);
                        } else if (condition_is_range and seen_value.isNumber()) {
                            const condition_range = o.ObjRange.cast(condition_value.obj()).?;
                            const condition_low: f64 = @floatFromInt(@min(condition_range.low, condition_range.high));
                            const condition_high: f64 = @floatFromInt(@max(condition_range.low, condition_range.high));
                            const seen_number = if (seen_value.isInteger())
                                @as(f64, @floatFromInt(seen_value.integer()))
                            else
                                seen_value.double();

                            overlapping_condition = (value_type_def.def_type == .Double or
                                seen_value.isInteger() or
                                (std.math.isFinite(seen_number) and seen_number == @trunc(seen_number))) and
                                seen_number >= condition_low and seen_number < condition_high;
                        } else if (seen_condition_is_range and condition_value.isNumber()) {
                            const seen_range = o.ObjRange.cast(seen_value.obj()).?;
                            const seen_low: f64 = @floatFromInt(@min(seen_range.low, seen_range.high));
                            const seen_high: f64 = @floatFromInt(@max(seen_range.low, seen_range.high));
                            const condition_number = if (condition_value.isInteger())
                                @as(f64, @floatFromInt(condition_value.integer()))
                            else
                                condition_value.double();

                            overlapping_condition = (value_type_def.def_type == .Double or
                                condition_value.isInteger() or
                                (std.math.isFinite(condition_number) and condition_number == @trunc(condition_number))) and
                                condition_number >= seen_low and condition_number < seen_high;
                        }
                    }

                    if (duplicate_condition) {
                        reporter.reportWithOrigin(
                            .match_duplicate_condition,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            ast.tokens.get(locations[seen_condition]),
                            ast.tokens.get(end_locations[seen_condition]),
                            "Duplicate `match` condition",
                            .{},
                            "first used here",
                        );
                        return true;
                    }

                    if (overlapping_condition) {
                        reporter.reportWithOrigin(
                            .match_duplicate_condition,
                            ast.tokens.get(locations[condition]),
                            ast.tokens.get(end_locations[condition]),
                            ast.tokens.get(locations[seen_condition]),
                            ast.tokens.get(end_locations[seen_condition]),
                            "Overlapping `match` condition",
                            .{},
                            "overlaps with this condition",
                        );
                        return true;
                    }
                }

                try seen_values.append(gc.allocator, condition_value);
                try seen_conditions.append(gc.allocator, condition);
            }
        }
    }

    return had_error;
}

fn checkList(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = ast.nodes.items(.components)[node].List;
    const type_defs = ast.nodes.items(.type_def);
    const item_type = type_defs[node].?.resolved_type.?.List.item_type;

    var had_error = false;

    for (components.items) |item| {
        if (item_type.def_type == .Placeholder) {
            reporter.reportPlaceholder(ast, type_defs[item].?.resolved_type.?.Placeholder);
        } else {
            // The list item type provides context for inferred item expressions.
            _ = try inferType(ast, reporter, gc, item, item_type);
        }

        const actual_item_type = type_defs[item].?;
        if (item_type.def_type != .Placeholder and !item_type.eql(actual_item_type)) {
            reporter.reportTypeCheck(
                .list_item_type,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                item_type,
                ast.tokens.get(locations[item]),
                ast.tokens.get(end_locations[item]),
                actual_item_type,
                "Bad list type",
            );
            had_error = true;
        }
    }

    return had_error;
}

fn checkMap(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = ast.nodes.items(.components)[node].Map;
    const type_defs = ast.nodes.items(.type_def);

    const map_type = type_defs[node].?.resolved_type.?.Map;
    const key_type = if (components.explicit_key_type) |kt|
        type_defs[kt].?
    else
        map_type.key_type;

    const value_type = if (components.explicit_value_type) |vt|
        type_defs[vt].?
    else
        map_type.value_type;

    var had_error = false;

    for (components.entries) |entry| {
        if (key_type.def_type != .Placeholder) {
            // Map key declarations provide context for inferred key expressions.
            _ = try inferType(ast, reporter, gc, entry.key, key_type);
        }

        if (!key_type.eql(type_defs[entry.key].?)) {
            reporter.reportTypeCheck(
                .map_key_type,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                key_type,
                ast.tokens.get(locations[entry.key]),
                ast.tokens.get(end_locations[entry.key]),
                type_defs[entry.key].?,
                "Bad key type",
            );
            had_error = true;
        }

        if (value_type.def_type != .Placeholder) {
            // Map value declarations provide context for inferred value expressions.
            _ = try inferType(ast, reporter, gc, entry.value, value_type);
        }

        if (!value_type.eql(type_defs[entry.value].?)) {
            reporter.reportTypeCheck(
                .map_value_type,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                value_type,
                ast.tokens.get(locations[entry.value]),
                ast.tokens.get(end_locations[entry.value]),
                type_defs[entry.value].?,
                "Bad value type",
            );
            had_error = true;
        }
    }

    return had_error;
}

fn checkNamedVariable(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[node].NamedVariable;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const tags = ast.tokens.items(.tag);

    var had_error = false;

    if (components.value) |value| {
        // Assignment targets provide context for inferred assigned values.
        _ = try inferType(ast, reporter, gc, value, type_defs[node].?);
        const value_type_def = type_defs[value].?;

        if (!type_defs[node].?.eql(value_type_def)) {
            reporter.reportTypeCheck(
                .assignment_value_type,
                ast.tokens.get(locations[node]),
                ast.tokens.get(end_locations[node]),
                type_defs[node].?,
                ast.tokens.get(locations[value]),
                ast.tokens.get(end_locations[value]),
                value_type_def,
                "Bad value type",
            );
            had_error = true;
        }

        // Type check that operator is allowed
        switch (tags[components.assign_token.?]) {
            .PlusEqual => switch (type_defs[node].?.def_type) {
                .Integer,
                .Double,
                .List,
                .Map,
                .String,
                => {},
                else => {
                    reporter.report(
                        .arithmetic_operand_type,
                        ast.tokens.get(components.assign_token.?),
                        ast.tokens.get(components.assign_token.?),
                        "Addition is only allowed for types `int`, `double`, list, map and `str`",
                    );
                    had_error = true;
                },
            },
            .MinusEqual,
            .StarEqual,
            .SlashEqual,
            .PercentEqual,
            => switch (type_defs[node].?.def_type) {
                .Integer, .Double => {},
                else => {
                    reporter.report(
                        .arithmetic_operand_type,
                        ast.tokens.get(components.assign_token.?),
                        ast.tokens.get(components.assign_token.?),
                        "Operator is only allowed for types `int`, `double`",
                    );
                    had_error = true;
                },
            },
            .ShiftRightEqual,
            .ShiftLeftEqual,
            .XorEqual,
            .BorEqual,
            .BnotEqual,
            .AmpersandEqual,
            => if (type_defs[node].?.def_type != .Integer) {
                reporter.report(
                    .arithmetic_operand_type,
                    ast.tokens.get(components.assign_token.?),
                    ast.tokens.get(components.assign_token.?),
                    "Operator is only allowed for `int`",
                );
                had_error = true;
            },
            else => {},
        }
    }

    return had_error;
}

fn checkObjectDeclaration(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const lexemes = ast.tokens.items(.lexeme);
    const components = ast.nodes.items(.components)[node].ObjectDeclaration;
    const location = locations[node];

    const object_type = type_defs[node].?;
    const object_def = object_type.resolved_type.?.Object;

    var had_error = false;

    // Check object conforms to declared protocols
    var protocol_it = object_def.conforms_to.iterator();
    while (protocol_it.next()) |kv| {
        const protocol_type_def = kv.key_ptr.*;

        const protocol_def = protocol_type_def.resolved_type.?.Protocol;

        var method_it = protocol_def.methods.iterator();
        while (method_it.next()) |mkv| {
            var found = false;
            for (components.members) |member| {
                if (member.method and std.mem.eql(u8, ast.tokens.items(.lexeme)[member.name], mkv.key_ptr.*)) {
                    found = true;
                    if (!mkv.value_ptr.*.type_def.eql(type_defs[member.method_or_default_value.?].?) or
                        mkv.value_ptr.*.mutable != object_def.fields.get(mkv.key_ptr.*).?.mutable)
                    {
                        reporter.reportTypeCheck(
                            .protocol_conforming,
                            ast.tokens.get(protocol_def.location),
                            ast.tokens.get(protocol_def.location),
                            mkv.value_ptr.*.type_def,
                            ast.tokens.get(locations[member.method_or_default_value.?]),
                            ast.tokens.get(end_locations[member.method_or_default_value.?]),
                            type_defs[member.method_or_default_value.?].?,
                            "Method not conforming to protocol",
                        );
                        had_error = true;
                    }
                    break;
                }
            }

            if (!found) {
                reporter.reportWithOrigin(
                    .protocol_conforming,
                    ast.tokens.get(location),
                    ast.tokens.get(end_locations[node]),
                    ast.tokens.get(
                        protocol_def.methods_locations.get(
                            mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string,
                        ).?,
                    ),
                    ast.tokens.get(
                        protocol_def.methods_locations.get(
                            mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string,
                        ).?,
                    ),
                    "Object declared as conforming to protocol `{s}` but doesn't implement method `{s}`",
                    .{
                        protocol_def.name.string,
                        mkv.value_ptr.*.type_def.resolved_type.?.Function.name.string,
                    },
                    null,
                );
                had_error = true;
            }
        }
    }

    for (components.members) |member| {
        const member_name = lexemes[member.name];

        if (member.method) {
            // Method
            const member_field = object_def.fields.get(member_name).?;
            const member_type_def = member_field.type_def;

            // Enforce "collect" method signature
            if (std.mem.eql(u8, member_name, "collect")) {
                const collect_def = member_type_def.resolved_type.?.Function;

                if (collect_def.parameters.count() > 0 or
                    collect_def.return_type.def_type != .Void or
                    collect_def.yield_type.def_type != .Void or
                    collect_def.error_types != null)
                {
                    const collect_def_str = member_type_def.toStringAlloc(gc.allocator, false) catch @panic("Out of memory");
                    defer gc.allocator.free(collect_def_str);
                    reporter.reportErrorFmt(
                        .collect_signature,
                        ast.tokens.get(locations[member.method_or_default_value.?]),
                        ast.tokens.get(end_locations[member.method_or_default_value.?]),
                        "Expected `collect` method to be `fun collect() > void` got {s}",
                        .{
                            collect_def_str,
                        },
                    );
                    had_error = true;
                }
            } else if (std.mem.eql(u8, member_name, "toString")) { // Enforce "toString" method signature
                const tostring_def = member_type_def.resolved_type.?.Function;

                if (tostring_def.parameters.count() > 0 or
                    tostring_def.return_type.def_type != .String or
                    tostring_def.yield_type.def_type != .Void or
                    tostring_def.error_types != null or
                    tostring_def.generic_types.count() > 0)
                {
                    const tostring_def_str = member_type_def.toStringAlloc(gc.allocator, false) catch @panic("Out of memory");
                    defer gc.allocator.free(tostring_def_str);
                    reporter.reportErrorFmt(
                        .tostring_signature,
                        ast.tokens.get(locations[member.method_or_default_value.?]),
                        ast.tokens.get(end_locations[member.method_or_default_value.?]),
                        "Expected `toString` method to be `fun toString() > str` got {s}",
                        .{
                            tostring_def_str,
                        },
                    );
                    had_error = true;
                }
            }
        } else {
            // Property
            const property_field = object_def.fields.get(member_name).?;
            const property_type = property_field.type_def;

            // Create property default value
            if (member.method_or_default_value) |default| {
                // Property declarations provide context for inferred default values.
                _ = try inferType(ast, reporter, gc, default, property_type);
                const default_type_def = type_defs[default].?;

                if (default_type_def.isMutable()) {
                    reporter.reportErrorAt(
                        .constant_default,
                        ast.tokens.get(locations[default]),
                        ast.tokens.get(end_locations[default]),
                        "Default value must be constant",
                    );
                    had_error = true;
                }

                if (!property_type.eql(default_type_def)) {
                    reporter.reportTypeCheck(
                        .property_default_value,
                        ast.tokens.get(object_def.location),
                        ast.tokens.get(object_def.location),
                        property_type,
                        ast.tokens.get(locations[default]),
                        ast.tokens.get(end_locations[default]),
                        default_type_def,
                        "Wrong property default value type",
                    );
                    had_error = true;
                }
            }
        }
    }

    return had_error;
}

fn checkObjectInit(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const type_defs = ast.nodes.items(.type_def);
    const lexemes = ast.tokens.items(.lexeme);
    const components = ast.nodes.items(.components)[node].ObjectInit;
    const location = locations[node];
    const node_type_def = type_defs[node].?;

    var had_error = false;

    if (node_type_def.def_type != .ObjectInstance and node_type_def.def_type != .ForeignContainer) {
        reporter.reportErrorAt(
            .expected_object,
            ast.tokens.get(location),
            ast.tokens.get(end_locations[node]),
            "Expected object or foreign struct.",
        );

        had_error = true;
    }

    var fields = if (node_type_def.def_type == .ObjectInstance) inst: {
        const fields = node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields;
        var fields_type_defs = std.StringArrayHashMapUnmanaged(*o.ObjTypeDef).empty;
        var it = fields.iterator();
        while (it.next()) |kv| {
            try fields_type_defs.put(
                gc.allocator,
                kv.value_ptr.*.name,
                kv.value_ptr.*.type_def,
            );
        }
        break :inst fields_type_defs;
    } else node_type_def.resolved_type.?.ForeignContainer.buzz_type;

    defer if (node_type_def.def_type == .ObjectInstance) {
        fields.deinit(gc.allocator);
    };

    const object_location = if (node_type_def.def_type == .ObjectInstance)
        node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.location
    else
        node_type_def.resolved_type.?.ForeignContainer.location;

    // Keep track of what's been initialized or not by this statement
    var init_properties = std.StringHashMapUnmanaged(void).empty;
    defer init_properties.deinit(gc.allocator);

    for (components.properties) |property| {
        const property_name = lexemes[property.name];
        if (fields.get(property_name)) |prop| {
            // Object initializer fields provide context for inferred property values.
            _ = try inferType(ast, reporter, gc, property.value, prop);
            const value_type_def = type_defs[property.value].?;

            if (!prop.eql(value_type_def)) {
                if (BuildOptions.debug_placeholders) {
                    io.print(
                        "prop {}({}), value {}({})\n",
                        .{
                            @intFromPtr(prop.resolved_type.?.ObjectInstance.of),
                            prop.optional,
                            @intFromPtr(value_type_def.resolved_type.?.ObjectInstance.of),
                            value_type_def.optional,
                        },
                    );
                }

                const err_location = ast.tokens.get(
                    if (node_type_def.def_type == .ObjectInstance)
                        node_type_def.resolved_type.?.ObjectInstance.of
                            .resolved_type.?.Object.fields.get(property_name).?.location
                    else
                        object_location,
                );

                reporter.reportTypeCheck(
                    .property_type,
                    err_location,
                    err_location,
                    prop,
                    ast.tokens.get(locations[property.value]),
                    ast.tokens.get(end_locations[property.value]),
                    value_type_def,
                    "Wrong property type",
                );
                had_error = true;
            }

            try init_properties.put(gc.allocator, property_name, {});
        } else {
            reporter.reportWithOrigin(
                .property_does_not_exists,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                ast.tokens.get(object_location),
                ast.tokens.get(object_location),
                "Property `{s}` does not exists",
                .{property_name},
                null,
            );
            had_error = true;
        }
    }

    // Did we initialized all properties without a default value?
    // If union we're statisfied with only on field initialized
    if (node_type_def.def_type != .ForeignContainer or node_type_def.resolved_type.?.ForeignContainer.zig_type != .Union or init_properties.count() == 0) {
        const field_defs = if (node_type_def.def_type == .ObjectInstance)
            node_type_def.resolved_type.?.ObjectInstance.of.resolved_type.?.Object.fields
        else
            null;

        var it = fields.iterator();
        while (it.next()) |kv| {
            const field = if (field_defs) |fd| fd.get(kv.key_ptr.*) else null;
            // If ommitted in initialization and doesn't have default value
            if (init_properties.get(kv.key_ptr.*) == null and
                (field == null or (!field.?.has_default and !field.?.method and !field.?.static)))
            {
                reporter.reportErrorFmt(
                    .property_not_initialized,
                    ast.tokens.get(location),
                    ast.tokens.get(end_locations[node]),
                    "Property `{s}` was not initialized and has no default value",
                    .{kv.key_ptr.*},
                );
                had_error = true;
            }
        }
    }

    return had_error;
}

fn checkRange(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const components = ast.nodes.items(.components)[node].Range;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);

    var had_error = false;

    if (type_defs[components.low].?.def_type != .Integer) {
        reporter.reportTypeCheck(
            .range_type,
            null,
            null,
            gc.type_registry.int_type,
            ast.tokens.get(locations[components.low]),
            ast.tokens.get(end_locations[components.low]),
            type_defs[components.low].?,
            "Bad low range limit type",
        );
        had_error = true;
    }

    if (type_defs[components.high].?.def_type != .Integer) {
        reporter.reportTypeCheck(
            .range_type,
            null,
            null,
            gc.type_registry.int_type,
            ast.tokens.get(locations[components.high]),
            ast.tokens.get(end_locations[components.high]),
            type_defs[components.high].?,
            "Bad high range limit type",
        );
        had_error = true;
    }

    return had_error;
}

fn checkAsyncCall(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const callee = ast.nodes.items(.components)[node].AsyncCall;
    const callee_type_def = ast.nodes.items(.type_def)[callee] orelse gc.type_registry.any_type;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);

    if (callee_type_def.optional or ast.nodes.items(.patch_opt_jumps)[callee]) {
        reporter.reportErrorAt(
            .fiber,
            ast.tokens.get(locations[callee]),
            ast.tokens.get(end_locations[callee]),
            "Not callable",
        );

        return false;
    }

    return true;
}

fn checkResolve(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const fiber = ast.nodes.items(.components)[node].Resolve;
    const fiber_type_def = ast.nodes.items(.type_def)[fiber] orelse gc.type_registry.any_type;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);

    if (fiber_type_def.def_type != .Fiber or fiber_type_def.optional or ast.nodes.items(.patch_opt_jumps)[fiber]) {
        reporter.reportErrorAt(
            .fiber,
            ast.tokens.get(locations[fiber]),
            ast.tokens.get(end_locations[fiber]),
            "Not a fiber",
        );

        return false;
    }

    return true;
}

fn checkResume(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const fiber = ast.nodes.items(.components)[node].Resume;
    const fiber_type_def = ast.nodes.items(.type_def)[fiber] orelse gc.type_registry.any_type;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);

    if (fiber_type_def.def_type != .Fiber or fiber_type_def.optional or ast.nodes.items(.patch_opt_jumps)[fiber]) {
        reporter.reportErrorAt(
            .fiber,
            ast.tokens.get(locations[fiber]),
            ast.tokens.get(end_locations[fiber]),
            "Not a fiber",
        );

        return false;
    }

    return true;
}

fn checkReturn(ast: Ast.Slice, reporter: *Reporter, gc: *GC, current_function_node: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[node].Return;
    const type_defs = ast.nodes.items(.type_def);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const current_function_type_def = type_defs[current_function_node.?].?.resolved_type.?.Function;

    var had_error = false;

    if (components.value) |value| {
        // Function return types provide context for inferred return values.
        _ = try inferType(ast, reporter, gc, value, current_function_type_def.return_type);
        const value_type_def = type_defs[value];
        if (value_type_def == null) {
            reporter.reportErrorAt(
                .undefined,
                ast.tokens.get(locations[value]),
                ast.tokens.get(end_locations[value]),
                "Unknown type.",
            );
            had_error = true;
        } else if (current_function_node != null and !current_function_type_def.return_type.eql(value_type_def.?)) {
            reporter.reportTypeCheck(
                .return_type,
                ast.tokens.get(locations[current_function_node.?]),
                ast.tokens.get(end_locations[current_function_node.?]),
                current_function_type_def.return_type,
                ast.tokens.get(locations[value]),
                ast.tokens.get(end_locations[value]),
                value_type_def.?,
                "Return value",
            );
            had_error = true;
        }
    } else if (current_function_node != null and current_function_type_def.return_type.def_type != .Void) {
        reporter.reportTypeCheck(
            .return_type,
            ast.tokens.get(locations[current_function_node.?]),
            ast.tokens.get(end_locations[current_function_node.?]),
            current_function_type_def.return_type,
            ast.tokens.get(locations[node]),
            ast.tokens.get(end_locations[node]),
            gc.type_registry.void_type,
            "Return value",
        );
        had_error = true;
    }

    return had_error;
}

fn checkSubscript(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const location = locations[node];
    const type_defs = ast.nodes.items(.type_def);
    const components = ast.nodes.items(.components)[node].Subscript;

    const subscripted_type_def = type_defs[components.subscripted].?;
    const index_type_def = type_defs[components.index].?;
    var value_type_def = if (components.value) |value| type_defs[value] else null;

    var had_error = false;

    if (components.assign_token != null and subscripted_type_def.def_type != .List and subscripted_type_def.def_type != .Map) {
        reporter.reportErrorFmt(
            .assignable,
            ast.tokens.get(location),
            ast.tokens.get(end_locations[node]),
            "Can't assign to `{s}`.",
            .{
                subscripted_type_def.toStringAlloc(gc.allocator, false) catch return error.OutOfMemory,
            },
        );
        had_error = true;
    }

    if (components.checked and subscripted_type_def.def_type != .List and subscripted_type_def.def_type != .String) {
        reporter.reportErrorFmt(
            .assignable,
            ast.tokens.get(location),
            ast.tokens.get(end_locations[node]),
            "Checked subscript not available for `{s}`.",
            .{
                subscripted_type_def.toStringAlloc(gc.allocator, false) catch return error.OutOfMemory,
            },
        );
        had_error = true;
    }

    switch (subscripted_type_def.def_type) {
        .String => if (index_type_def.def_type != .Integer or index_type_def.optional) {
            reporter.reportErrorAt(
                .subscript_key_type,
                ast.tokens.get(locations[components.index]),
                ast.tokens.get(end_locations[components.index]),
                "Expected `int` index.",
            );
            had_error = true;
        },
        .List => {
            if (index_type_def.def_type != .Integer or index_type_def.optional) {
                reporter.reportErrorAt(
                    .subscript_key_type,
                    ast.tokens.get(locations[components.index]),
                    ast.tokens.get(end_locations[components.index]),
                    "Expected `int` index.",
                );
                had_error = true;
            }

            if (components.value) |value| {
                if (!type_defs[components.subscripted].?.isMutable()) {
                    const callee_type_str = type_defs[components.subscripted].?.toStringAlloc(gc.allocator, false) catch unreachable;
                    defer gc.allocator.free(callee_type_str);
                    reporter.reportErrorFmt(
                        .not_mutable,
                        ast.tokens.get(locations[components.subscripted]),
                        ast.tokens.get(end_locations[components.subscripted]),
                        "`{s}` not mutable",
                        .{
                            callee_type_str,
                        },
                    );
                    had_error = true;
                }

                // List element assignments provide the list item type as context.
                _ = try inferType(ast, reporter, gc, value, subscripted_type_def.resolved_type.?.List.item_type);
                value_type_def = type_defs[value];

                if (!subscripted_type_def.resolved_type.?.List.item_type.eql(value_type_def.?)) {
                    reporter.reportTypeCheck(
                        .subscript_value_type,
                        ast.tokens.get(locations[components.subscripted]),
                        ast.tokens.get(end_locations[components.subscripted]),
                        subscripted_type_def.resolved_type.?.List.item_type,
                        ast.tokens.get(locations[value]),
                        ast.tokens.get(end_locations[value]),
                        value_type_def.?,
                        "Bad value type",
                    );
                    had_error = true;
                }
            }
        },
        .Map => {
            if (!subscripted_type_def.resolved_type.?.Map.key_type.eql(index_type_def)) {
                reporter.reportTypeCheck(
                    .subscript_key_type,
                    ast.tokens.get(locations[components.subscripted]),
                    ast.tokens.get(end_locations[components.subscripted]),
                    subscripted_type_def.resolved_type.?.Map.key_type,
                    ast.tokens.get(locations[components.index]),
                    ast.tokens.get(end_locations[components.index]),
                    index_type_def,
                    "Bad key type",
                );
                had_error = true;
            }

            if (components.value) |value| {
                if (!type_defs[components.subscripted].?.isMutable()) {
                    const callee_type_str = type_defs[components.subscripted].?.toStringAlloc(gc.allocator, false) catch unreachable;
                    defer gc.allocator.free(callee_type_str);
                    reporter.reportErrorFmt(
                        .not_mutable,
                        ast.tokens.get(locations[components.subscripted]),
                        ast.tokens.get(end_locations[components.subscripted]),
                        "`{s}` not mutable",
                        .{
                            callee_type_str,
                        },
                    );
                    had_error = true;
                }

                // Map element assignments provide the map value type as context.
                _ = try inferType(ast, reporter, gc, value, subscripted_type_def.resolved_type.?.Map.value_type);
                value_type_def = type_defs[value];

                if (!subscripted_type_def.resolved_type.?.Map.value_type.eql(value_type_def.?)) {
                    reporter.reportTypeCheck(
                        .subscript_value_type,
                        ast.tokens.get(locations[components.subscripted]),
                        ast.tokens.get(end_locations[components.subscripted]),
                        subscripted_type_def.resolved_type.?.Map.value_type,
                        ast.tokens.get(locations[value]),
                        ast.tokens.get(end_locations[value]),
                        value_type_def.?,
                        "Bad value type",
                    );
                    had_error = true;
                }
            }
        },
        else => {
            reporter.reportErrorAt(
                .subscriptable,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                "Not subscriptable.",
            );
            had_error = true;
        },
    }

    return had_error;
}

fn checkUnary(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[node].Unary;
    const end_locations = ast.nodes.items(.end_location);
    const expression_location = ast.nodes.items(.location)[components.expression];
    const expression_type_def = ast.nodes.items(.type_def)[components.expression].?;

    var had_error = false;

    switch (components.operator) {
        .Bnot => if (expression_type_def.def_type != .Integer) {
            reporter.reportErrorFmt(
                .bitwise_operand_type,
                ast.tokens.get(expression_location),
                ast.tokens.get(end_locations[components.expression]),
                "Expected type `int`, got `{s}`",
                .{
                    expression_type_def.toStringAlloc(gc.allocator, false) catch return error.OutOfMemory,
                },
            );
            had_error = true;
        },
        .Bang => if (expression_type_def.def_type != .Boolean) {
            reporter.reportErrorFmt(
                .bitwise_operand_type,
                ast.tokens.get(expression_location),
                ast.tokens.get(end_locations[components.expression]),
                "Expected type `bool`, got `{s}`",
                .{
                    expression_type_def.toStringAlloc(gc.allocator, false) catch return error.OutOfMemory,
                },
            );
            had_error = true;
        },
        .Minus => if (expression_type_def.def_type != .Integer and expression_type_def.def_type != .Double) {
            reporter.reportErrorFmt(
                .arithmetic_operand_type,
                ast.tokens.get(expression_location),
                ast.tokens.get(end_locations[components.expression]),
                "Expected type `int` or `double`, got `{s}`",
                .{
                    expression_type_def.toStringAlloc(gc.allocator, false) catch return error.OutOfMemory,
                },
            );
            had_error = true;
        },
        else => unreachable,
    }

    return had_error;
}

fn checkUnwrap(ast: Ast.Slice, reporter: *Reporter, _: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const components = ast.nodes.items(.components)[node].Unwrap;

    if (!components.original_type.optional) {
        reporter.reportErrorAt(
            .optional,
            ast.tokens.get(locations[components.unwrapped]),
            ast.tokens.get(end_locations[components.unwrapped]),
            "Not an optional",
        );

        return true;
    }

    return false;
}

fn checkVarDeclaration(ast: Ast.Slice, reporter: *Reporter, gc: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[node].VarDeclaration;
    const type_defs = ast.nodes.items(.type_def);
    const type_def = type_defs[node].?;
    var value_type_def = if (components.value) |value|
        ast.nodes.items(.type_def)[value]
    else
        null;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const location = locations[node];

    if (components.value) |value| {
        // Declared variable types provide context for inferred initializer values.
        _ = try inferType(ast, reporter, gc, value, type_def);
        value_type_def = ast.nodes.items(.type_def)[value];

        if (!(type_def.toInstance(&gc.type_registry, type_def.isMutable()) catch return error.OutOfMemory)
            .eql(value_type_def.?) and
            !((type_def.toInstance(&gc.type_registry, type_def.isMutable()) catch return error.OutOfMemory)
                .cloneNonOptional(&gc.type_registry) catch return error.OutOfMemory)
                .eql(value_type_def.?))
        {
            reporter.reportTypeCheck(
                .assignment_value_type,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                type_def.toInstance(&gc.type_registry, type_def.isMutable()) catch return error.OutOfMemory,
                ast.tokens.get(locations[value]),
                ast.tokens.get(end_locations[value]),
                value_type_def.?,
                "Wrong variable type",
            );

            return true;
        }
    }

    return false;
}

fn checkWhile(ast: Ast.Slice, reporter: *Reporter, _: *GC, _: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const components = ast.nodes.items(.components)[node].While;
    const type_defs = ast.nodes.items(.type_def);
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const condition_type_def = type_defs[components.condition].?;

    if (condition_type_def.def_type != .Boolean) {
        reporter.reportErrorAt(
            .while_condition_type,
            ast.tokens.get(locations[components.condition]),
            ast.tokens.get(end_locations[components.condition]),
            "`while` condition must be bool",
        );

        return true;
    }

    return false;
}

fn checkYield(ast: Ast.Slice, reporter: *Reporter, gc: *GC, current_function_node: ?Ast.Node.Index, node: Ast.Node.Index) error{OutOfMemory}!bool {
    const type_defs = ast.nodes.items(.type_def);
    const components = ast.nodes.items(.components)[node].Yield;
    const locations = ast.nodes.items(.location);
    const end_locations = ast.nodes.items(.end_location);
    const location = locations[node];

    var had_error = false;

    const current_function_typedef = type_defs[current_function_node.?].?.resolved_type.?.Function;
    const current_function_type = current_function_typedef.function_type;
    switch (current_function_type) {
        .Script,
        .ScriptEntryPoint,
        .Repl,
        .EntryPoint,
        .Test,
        .Extern,
        => {
            reporter.reportErrorAt(
                .yield_not_allowed,
                ast.tokens.get(location),
                ast.tokens.get(end_locations[node]),
                "Can't yield here",
            );
            had_error = true;
        },
        else => {},
    }

    // Fiber yield types provide context for inferred yielded values.
    _ = try inferType(ast, reporter, gc, components, current_function_typedef.yield_type);
    type_defs[node] = type_defs[components];
    const type_def = type_defs[node];

    if (type_def == null) {
        reporter.reportErrorAt(
            .unknown,
            ast.tokens.get(location),
            ast.tokens.get(end_locations[node]),
            "Unknown type.",
        );
        had_error = true;
    } else if (!current_function_typedef.yield_type.eql(type_def.?)) {
        reporter.reportTypeCheck(
            .yield_type,
            ast.tokens.get(locations[current_function_node.?]),
            ast.tokens.get(end_locations[current_function_node.?]),
            current_function_typedef.yield_type,
            ast.tokens.get(location),
            ast.tokens.get(end_locations[node]),
            type_def.?,
            "Bad yield value",
        );
        had_error = true;
    }

    return had_error;
}
