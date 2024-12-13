const std = @import("std");
const obj = @import("../../obj.zig");
const Ast = @import("../../Ast.zig");
const Token = @import("../../Token.zig");

pub const AgentTransform = struct {
    allocator: std.mem.Allocator,
    ast: *Ast,

    pub fn init(allocator: std.mem.Allocator, ast: *Ast) AgentTransform {
        return .{
            .allocator = allocator,
            .ast = ast,
        };
    }

    pub fn transformAgent(self: *AgentTransform, node: Ast.Node.Index) !void {
        const object_decl = &self.ast.nodes.items(.components)[node].ObjectDeclaration;

        // Check if object has @agent decorator
        var has_agent_decorator = false;
        for (object_decl.decorators) |decorator| {
            const decorator_lexeme = self.ast.tokens.items(.lexeme)[decorator];
            if (std.mem.eql(u8, decorator_lexeme, "@agent")) {
                has_agent_decorator = true;
                break;
            }
        }

        if (!has_agent_decorator) {
            return;
        }

        // Transform strategy syntax sugar into full protocol implementation
        var strategy_found = false;
        for (object_decl.members) |member| {
            const member_name = self.ast.tokens.items(.lexeme)[member.name];
            if (std.mem.eql(u8, member_name, "strategy")) {
                strategy_found = true;
                try self.transformStrategy(member);
            }
        }

        if (!strategy_found) {
            // Report error if @agent object doesn't have a strategy
            self.ast.reporter.reportErrorAt(
                .missing_strategy,
                self.ast.tokens.get(object_decl.name),
                "Agent object must define a strategy",
            );
        }
    }

    fn transformStrategy(self: *AgentTransform, member: Ast.ObjectDeclaration.Member) !void {
        const strategy_value = self.ast.nodes.items(.components)[member.method_or_default_value].ObjectLiteral;

        // Generate protocol implementation methods
        try self.generateShouldEnter(member.name, strategy_value.entry);
        try self.generateShouldExit(member.name, strategy_value.exit);
        try self.generatePositionSize(member.name, strategy_value.size);
    }

    fn generateShouldEnter(self: *AgentTransform, strategy_token: Ast.TokenIndex, entry_expr: Ast.Node.Index) !void {
        // Create should_enter method implementation
        const method_node = try self.ast.appendNode(.{
            .tag = .Function,
            .location = strategy_token,
            .end_location = strategy_token,
            .type_def = null,
            .components = .{
                .Function = .{
                    .name = "should_enter",
                    .params = &[_]Ast.Node.Index{
                        try self.ast.appendNode(.{
                            .tag = .Parameter,
                            .location = strategy_token,
                            .end_location = strategy_token,
                            .type_def = try self.ast.getMarketStateType(),
                            .components = .{ .Parameter = "state" },
                        }),
                    },
                    .body = entry_expr,
                    .return_type = try self.ast.getBoolType(),
                },
            },
        });
        _ = method_node;
    }

    fn generateShouldExit(self: *AgentTransform, strategy_token: Ast.TokenIndex, exit_expr: Ast.Node.Index) !void {
        // Create should_exit method implementation
        const method_node = try self.ast.appendNode(.{
            .tag = .Function,
            .location = strategy_token,
            .end_location = strategy_token,
            .type_def = null,
            .components = .{
                .Function = .{
                    .name = "should_exit",
                    .params = &[_]Ast.Node.Index{
                        try self.ast.appendNode(.{
                            .tag = .Parameter,
                            .location = strategy_token,
                            .end_location = strategy_token,
                            .type_def = try self.ast.getMarketStateType(),
                            .components = .{ .Parameter = "state" },
                        }),
                    },
                    .body = exit_expr,
                    .return_type = try self.ast.getBoolType(),
                },
            },
        });
        _ = method_node;
    }

    fn generatePositionSize(self: *AgentTransform, strategy_token: Ast.TokenIndex, size_expr: Ast.Node.Index) !void {
        // Create position_size method implementation
        const method_node = try self.ast.appendNode(.{
            .tag = .Function,
            .location = strategy_token,
            .end_location = strategy_token,
            .type_def = null,
            .components = .{
                .Function = .{
                    .name = "position_size",
                    .params = &[_]Ast.Node.Index{},
                    .body = size_expr,
                    .return_type = try self.ast.getDoubleType(),
                },
            },
        });
        _ = method_node;
    }
};
