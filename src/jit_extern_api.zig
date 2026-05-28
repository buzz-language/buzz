const std = @import("std");
const builtin = @import("builtin");
const m = @import("mir.zig");
const api = @import("lib/buzz_api.zig");
const JIT = @import("Jit.zig");
const jmp = @import("jmp.zig");
const io = @import("io.zig");

export fn bz_exit(code: c_int) callconv(.c) noreturn {
    std.process.exit(@truncate(@as(c_uint, @bitCast(code))));
}

pub const ExternApi = enum {
    NativeFn,
    RawFn,

    bz_stringConcat,
    bz_stringSubscript,
    bz_valueCastToString,
    bz_newList,
    bz_newRange,
    bz_listAppend,
    bz_listGet,
    bz_listSet,
    bz_valueEqual,
    bz_rangeContains,
    bz_patternMatches,
    bz_listConcat,
    bz_newMap,
    bz_mapSet,
    bz_mapGet,
    bz_mapConcat,
    bz_valueIs,
    bz_setTryCtx,
    bz_popTryCtx,
    bz_rethrow,
    bz_throw,
    bz_closeUpValues,
    bz_getUpValue,
    bz_setUpValue,
    bz_closure,
    bz_context,
    bz_newObjectInstance,
    bz_setObjectInstanceProperty,
    bz_getObjectInstanceProperty,
    bz_getObjectInstanceMethod,
    bz_getProtocolMethod,
    bz_getObjectField,
    bz_setObjectField,
    bz_getStringProperty,
    bz_getPatternProperty,
    bz_getFiberProperty,
    bz_getEnumCase,
    bz_getEnumInstanceValue,
    bz_getListProperty,
    bz_getMapProperty,
    bz_getRangeProperty,
    bz_getEnumCaseFromValue,
    bz_bindMethod,
    bz_stringNext,
    bz_listNext,
    bz_mapNext,
    bz_enumNext,
    bz_rangeNext,
    bz_clone,
    bz_valueToCString,
    bz_getUserDataPtr,
    bz_newUserData,
    bz_valueToForeignContainerPtr,
    bz_stringToValueZ,
    bz_foreignContainerGet,
    bz_foreignContainerSet,
    bz_newForeignContainerInstance,
    bz_valueTypeOf,
    bz_newForeignContainerFromSlice,

    bz_dumpStack,

    // https://opensource.apple.com/source/libplatform/libplatform-161/include/setjmp.h.auto.html
    setjmp,
    // libc exit: https://man7.org/linux/man-pages/man3/exit.3.html
    exit,

    bz_valueDump,
    fmod,
    memcpy,
    throwUnwrapError,

    pub fn declare(self: ExternApi, jit: *JIT) !m.MIR_item_t {
        const prototype = jit.state.?.prototypes.get(self) orelse self.proto(jit.ctx);

        try jit.required_ext_api.put(jit.gc.allocator, self, {});
        try jit.state.?.prototypes.put(
            jit.gc.allocator,
            self,
            prototype,
        );

        return prototype;
    }

    // FIXME: this could all be down at comptime by looking at each function type
    fn proto(self: ExternApi, ctx: m.MIR_context_t) m.MIR_item_t {
        return switch (self) {
            .bz_stringSubscript => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                4,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "obj_string",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index_value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "checked",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_closure => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                4,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U32,
                        .name = "function_node",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_raw",
                        .size = 0,
                    },
                },
            ),
            .bz_valueCastToString, .bz_newList, .bz_newMap => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_newRange => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_I64,
                        .name = "low",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_I64,
                        .name = "high",
                        .size = 0,
                    },
                },
            ),
            .bz_listAppend => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_listGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_I64,
                        .name = "index",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "checked",
                        .size = 0,
                    },
                },
            ),
            .bz_mapGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = 0,
                    },
                },
            ),
            .bz_listSet, .bz_mapSet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_valueEqual,
            .bz_rangeContains,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other",
                        .size = 0,
                    },
                },
            ),
            .bz_patternMatches => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "pattern",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = 0,
                    },
                },
            ),
            .bz_listConcat,
            .bz_mapConcat,
            .bz_stringConcat,
            .bz_newObjectInstance,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other_list",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_getEnumCaseFromValue,
            .bz_getEnumCase,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_getEnumInstanceValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                1,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum_instance",
                        .size = 0,
                    },
                },
            ),
            .bz_getObjectField,
            .bz_valueIs,
            .bz_getObjectInstanceProperty,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = 0,
                    },
                },
            ),
            .bz_getListProperty,
            .bz_getMapProperty,
            .bz_getRangeProperty,
            .bz_getStringProperty,
            .bz_getPatternProperty,
            .bz_getFiberProperty,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                4,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "bind",
                        .size = 0,
                    },
                },
            ),
            .bz_getProtocolMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_getObjectInstanceMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                4,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "bind",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_setObjectInstanceProperty,
            .bz_setObjectField,
            .bz_bindMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "instance",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_getUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = 0,
                    },
                },
            ),
            .bz_setUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_closeUpValues => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "last",
                        .size = 0,
                    },
                },
            ),
            .bz_setTryCtx => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                // *TryContext
                &.{m.MIR_T_P},
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_popTryCtx, .bz_rethrow => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_throw => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "payload",
                        .size = 0,
                    },
                },
            ),
            .bz_context => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_P},
                4,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "function",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "new_native_ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "arg_count",
                        .size = 0,
                    },
                },
            ),
            .setjmp =>
            // if (builtin.os.tag == .windows)
            //     m.MIR_new_proto_arr(
            //         ctx,
            //         self.pname(),
            //         1,
            //         &.{m.MIR_T_U64},
            //         2,
            //         &.{
            //             .{
            //                 .type = m.MIR_T_P,
            //                 .name = "jmp_buf",
            //                 .size = 0,
            //             },
            //             .{
            //                 .type = m.MIR_T_P,
            //                 .name = "ctx",
            //                 .size = 0,
            //             },
            //         },
            //     )
            // else
            m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "jmp_buf",
                        .size = 0,
                    },
                },
            ),
            .bz_clone => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_dumpStack => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "offset",
                        .size = 0,
                    },
                },
            ),
            .exit => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &.{
                    .{
                        .type = m.MIR_T_U8,
                        .name = "status",
                        .size = 0,
                    },
                },
            ),
            .bz_stringNext,
            .bz_listNext,
            .bz_enumNext,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "iterable",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "key",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_mapNext => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "iterable",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "key",
                        .size = 0,
                    },
                },
            ),
            .bz_rangeNext => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "range_value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = 0,
                    },
                },
            ),
            .RawFn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_U64},
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = 0,
                    },
                },
            ),
            .NativeFn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I16},
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = 0,
                    },
                },
            ),
            .bz_valueDump => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_valueToCString => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_P},
                1,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_getUserDataPtr => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_P},
                1,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_newUserData => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_valueToForeignContainerPtr => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_P},
                1,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_stringToValueZ => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "string",
                        .size = 0,
                    },
                },
            ),
            .bz_foreignContainerGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                3,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field_idx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_foreignContainerSet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field_idx",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "new_value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_newForeignContainerInstance => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                },
            ),
            .bz_valueTypeOf => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
            .bz_newForeignContainerFromSlice => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                4,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "type_def",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "ptr",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "len",
                        .size = 0,
                    },
                },
            ),
            .fmod => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &.{m.MIR_T_I64},
                2,
                &.{
                    .{
                        .type = m.MIR_T_D,
                        .name = "lhs",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_D,
                        .name = "rhs",
                        .size = 0,
                    },
                },
            ),
            .memcpy => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "dest",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "dest_len",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "source",
                        .size = 0,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "source_len",
                        .size = 0,
                    },
                },
            ),
            .throwUnwrapError => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &.{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = 0,
                    },
                },
            ),
        };
    }

    pub fn ptr(self: ExternApi) *anyopaque {
        return switch (self) {
            .bz_valueCastToString => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueCastToString))),
            .bz_stringConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_stringConcat))),
            .bz_stringSubscript => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_stringSubscript))),
            .bz_stringNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_stringNext))),
            .bz_newList => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newList))),
            .bz_newRange => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newRange))),
            .bz_rangeNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_rangeNext))),
            .bz_listAppend => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_listAppend))),
            .bz_listGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_listGet))),
            .bz_listSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_listSet))),
            .bz_listConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_listConcat))),
            .bz_listNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_listNext))),
            .bz_newMap => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newMap))),
            .bz_mapGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_mapGet))),
            .bz_mapSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_mapSet))),
            .bz_mapNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_mapNext))),
            .bz_mapConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_mapConcat))),
            .bz_valueEqual => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueEqual))),
            .bz_rangeContains => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_rangeContains))),
            .bz_patternMatches => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_patternMatches))),
            .bz_valueIs => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueIs))),
            .bz_closure => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_closure))),
            .bz_context => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_context))),
            .bz_newObjectInstance => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newObjectInstance))),
            .bz_setObjectInstanceProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_setObjectInstanceProperty))),
            .bz_getObjectInstanceProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getObjectInstanceProperty))),
            .bz_getObjectInstanceMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getObjectInstanceMethod))),
            .bz_getProtocolMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getProtocolMethod))),
            .bz_rethrow => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_rethrow))),
            .bz_throw => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_throw))),
            .bz_bindMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_bindMethod))),
            .bz_getUpValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getUpValue))),
            .bz_setUpValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_setUpValue))),
            .bz_closeUpValues => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_closeUpValues))),
            .bz_clone => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_clone))),
            .bz_dumpStack => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_dumpStack))),
            .bz_getEnumCaseFromValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getEnumCaseFromValue))),
            .bz_getEnumCase => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getEnumCase))),
            .bz_enumNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_enumNext))),
            .bz_getEnumInstanceValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getEnumInstanceValue))),
            .bz_setObjectField => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_setObjectField))),
            .bz_getObjectField => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getObjectField))),
            .bz_getListProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getListProperty))),
            .bz_getMapProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getMapProperty))),
            .bz_getRangeProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getRangeProperty))),
            .bz_getStringProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getStringProperty))),
            .bz_getPatternProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getPatternProperty))),
            .bz_getFiberProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getFiberProperty))),
            .bz_setTryCtx => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_setTryCtx))),
            .bz_popTryCtx => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_popTryCtx))),
            .bz_valueToCString => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueToCString))),
            .bz_getUserDataPtr => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_getUserDataPtr))),
            .bz_newUserData => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newUserData))),
            .bz_valueToForeignContainerPtr => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueToForeignContainerPtr))),
            .bz_stringToValueZ => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_stringToValueZ))),
            .bz_foreignContainerGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_foreignContainerGet))),
            .bz_foreignContainerSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_foreignContainerSet))),
            .bz_newForeignContainerInstance => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newForeignContainerInstance))),
            .bz_valueTypeOf => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueTypeOf))),
            .bz_newForeignContainerFromSlice => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_newForeignContainerFromSlice))),
            .setjmp => @as(
                *anyopaque,
                @ptrFromInt(
                    @intFromPtr(
                        &jmp.setjmp,
                        // &(if (builtin.os.tag == .windows)
                        //     jmp.__intrinsic_setjmpex
                        // else if (builtin.os.tag == .macos or builtin.os.tag == .linux)
                        //     jmp._setjmp
                        // else
                        //     jmp.setjmp),
                    ),
                ),
            ),
            .exit => @as(*anyopaque, @ptrFromInt(@intFromPtr(&bz_exit))),

            .bz_valueDump => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueDump))),
            .fmod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&JIT.fmod))),
            .memcpy => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.bz_memcpy))),
            .throwUnwrapError => @as(*anyopaque, @ptrFromInt(@intFromPtr(&JIT.throwUnwrapError))),
            else => unreachable,
        };
    }

    pub fn name(self: ExternApi) [*:0]const u8 {
        return @tagName(self);
    }

    pub fn pname(self: ExternApi) [*:0]const u8 {
        // https://ziggit.dev/t/why-tagname-unable-to-evaluate-comptime-expression-inside-a-function-with-anytype/2282/3
        switch (self) {
            inline else => |tag| {
                return "p_" ++ @tagName(tag);
            },
        }
    }
};
