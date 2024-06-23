const std = @import("std");
const builtin = @import("builtin");
const m = @import("mir.zig");
const api = @import("lib/buzz_api.zig");
const JIT = @import("Jit.zig");
const jmp = @import("jmp.zig").jmp;
const io = @import("io.zig");

export fn bz_exit(code: c_int) noreturn {
    std.process.exit(@truncate(@as(c_uint, @bitCast(code))));
}

pub const ExternApi = enum {
    nativefn,
    rawfn,

    bz_objStringConcat,
    bz_objStringSubscript,
    bz_toString,
    bz_newList,
    bz_newRange,
    bz_listAppend,
    bz_listGet,
    bz_listSet,
    bz_valueEqual,
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
    bz_instance,
    bz_setInstanceProperty,
    bz_getInstanceProperty,
    bz_getInstanceMethod,
    bz_getProtocolMethod,
    bz_getObjectField,
    bz_setObjectField,
    bz_getStringProperty,
    bz_getPatternProperty,
    bz_getFiberProperty,
    bz_getEnumCase,
    bz_getEnumCaseValue,
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
    bz_valueToUserData,
    bz_userDataToValue,
    bz_valueToForeignContainerPtr,
    bz_stringZ,
    bz_containerGet,
    bz_containerSet,
    bz_containerInstance,
    bz_valueTypeOf,
    bz_containerFromSlice,

    bz_dumpStack,

    // https://opensource.apple.com/source/libplatform/libplatform-161/include/setjmp.h.auto.html
    setjmp,
    // libc exit: https://man7.org/linux/man-pages/man3/exit.3.html
    exit,
    memcpy,

    dumpInt,
    bz_valueDump,
    fmod,

    pub fn declare(self: ExternApi, jit: *JIT) !m.MIR_item_t {
        const prototype = jit.state.?.prototypes.get(self) orelse self.proto(jit.ctx);

        try jit.required_ext_api.put(self, {});
        try jit.state.?.prototypes.put(
            self,
            prototype,
        );

        return prototype;
    }

    fn proto(self: ExternApi, ctx: m.MIR_context_t) m.MIR_item_t {
        return switch (self) {
            .bz_objStringSubscript => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "obj_string",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index_value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "checked",
                        .size = undefined,
                    },
                },
            ),
            .bz_closure => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "function_node",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_raw",
                        .size = undefined,
                    },
                },
            ),
            .bz_toString, .bz_newList, .bz_newMap => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_newRange => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_I32,
                        .name = "low",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_I32,
                        .name = "high",
                        .size = undefined,
                    },
                },
            ),
            .bz_listAppend => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_listGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_I32,
                        .name = "index",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "checked",
                        .size = undefined,
                    },
                },
            ),
            .bz_mapGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = undefined,
                    },
                },
            ),
            .bz_listSet, .bz_mapSet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueEqual => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other",
                        .size = undefined,
                    },
                },
            ),
            .bz_listConcat,
            .bz_mapConcat,
            .bz_objStringConcat,
            .bz_instance,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "list",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "other_list",
                        .size = undefined,
                    },
                },
            ),
            .bz_getEnumCaseFromValue, .bz_getEnumCase => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_getEnumCaseValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "enum_instance",
                        .size = undefined,
                    },
                },
            ),
            .bz_getObjectField,
            .bz_valueIs,
            .bz_getInstanceProperty,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                },
            ),
            .bz_getListProperty,
            .bz_getMapProperty,
            .bz_getRangeProperty,
            .bz_getStringProperty,
            .bz_getPatternProperty,
            .bz_getFiberProperty,
            .bz_getInstanceMethod,
            .bz_getProtocolMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "subject",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U8,
                        .name = "bind",
                        .size = undefined,
                    },
                },
            ),
            .bz_setInstanceProperty,
            .bz_setObjectField,
            .bz_bindMethod,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "instance",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_getUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = undefined,
                    },
                },
            ),
            .bz_setUpValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "slot",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_closeUpValues => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "last",
                        .size = undefined,
                    },
                },
            ),
            .bz_setTryCtx => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                // *TryContext
                &[_]m.MIR_type_t{m.MIR_T_P},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_popTryCtx, .bz_rethrow => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_throw => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "payload",
                        .size = undefined,
                    },
                },
            ),
            .bz_context => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_P},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "function",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "new_native_ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "arg_count",
                        .size = undefined,
                    },
                },
            ),
            .setjmp => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "jmp_buf",
                        .size = undefined,
                    },
                },
            ),
            .bz_clone => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_dumpStack => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "offset",
                        .size = undefined,
                    },
                },
            ),
            .exit => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U8,
                        .name = "status",
                        .size = undefined,
                    },
                },
            ),
            .bz_stringNext,
            .bz_listNext,
            .bz_mapNext,
            .bz_enumNext,
            => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "iterable",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "key",
                        .size = undefined,
                    },
                },
            ),
            .bz_rangeNext => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "range_value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "index",
                        .size = undefined,
                    },
                },
            ),
            .rawfn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_U64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                },
            ),
            .nativefn => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I16},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "ctx",
                        .size = undefined,
                    },
                },
            ),
            .dumpInt => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueDump => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueToCString => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_P},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueToUserData => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_P},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_userDataToValue => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueToForeignContainerPtr => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_P},
                1,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_stringZ => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "string",
                        .size = undefined,
                    },
                },
            ),
            .bz_containerGet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                3,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field_idx",
                        .size = undefined,
                    },
                },
            ),
            .bz_containerSet => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "field_idx",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "new_value",
                        .size = undefined,
                    },
                },
            ),
            .bz_containerInstance => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                },
            ),
            .bz_valueTypeOf => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_U64,
                        .name = "value",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                },
            ),
            .bz_containerFromSlice => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "vm",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "type_def",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "ptr",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "len",
                        .size = undefined,
                    },
                },
            ),
            .memcpy => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                0,
                null,
                4,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_P,
                        .name = "dest",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "dest_len",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_P,
                        .name = "source",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_U64,
                        .name = "source_len",
                        .size = undefined,
                    },
                },
            ),
            .fmod => m.MIR_new_proto_arr(
                ctx,
                self.pname(),
                1,
                &[_]m.MIR_type_t{m.MIR_T_I64},
                2,
                &[_]m.MIR_var_t{
                    .{
                        .type = m.MIR_T_D,
                        .name = "lhs",
                        .size = undefined,
                    },
                    .{
                        .type = m.MIR_T_D,
                        .name = "rhs",
                        .size = undefined,
                    },
                },
            ),
        };
    }

    pub fn ptr(self: ExternApi) *anyopaque {
        return switch (self) {
            .bz_toString => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_toString))),
            .bz_objStringConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_objStringConcat))),
            .bz_objStringSubscript => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_objStringSubscript))),
            .bz_stringNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_stringNext))),
            .bz_newList => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_newList))),
            .bz_newRange => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjRange.bz_newRange))),
            .bz_rangeNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjRange.bz_rangeNext))),
            .bz_listAppend => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_listAppend))),
            .bz_listGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_listGet))),
            .bz_listSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_listSet))),
            .bz_listConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_listConcat))),
            .bz_listNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_listNext))),
            .bz_newMap => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_newMap))),
            .bz_mapGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_mapGet))),
            .bz_mapSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_mapSet))),
            .bz_mapNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_mapNext))),
            .bz_mapConcat => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_mapConcat))),
            .bz_valueEqual => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueEqual))),
            .bz_valueIs => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueIs))),
            .bz_closure => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_closure))),
            .bz_context => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_context))),
            .bz_instance => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_instance))),
            .bz_setInstanceProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_setInstanceProperty))),
            .bz_getInstanceProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_getInstanceProperty))),
            .bz_getInstanceMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_getInstanceMethod))),
            .bz_getProtocolMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_getProtocolMethod))),
            .bz_rethrow => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_rethrow))),
            .bz_throw => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_throw))),
            .bz_bindMethod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_bindMethod))),
            .bz_getUpValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_getUpValue))),
            .bz_setUpValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_setUpValue))),
            .bz_closeUpValues => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_closeUpValues))),
            .bz_clone => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_clone))),
            .bz_dumpStack => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_dumpStack))),
            .bz_getEnumCaseFromValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjEnum.bz_getEnumCaseFromValue))),
            .bz_getEnumCase => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjEnum.bz_getEnumCase))),
            .bz_enumNext => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjEnum.bz_enumNext))),
            .bz_getEnumCaseValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjEnumInstance.bz_getEnumCaseValue))),
            .bz_setObjectField => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_setObjectField))),
            .bz_getObjectField => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjObject.bz_getObjectField))),
            .bz_getListProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjList.bz_getListProperty))),
            .bz_getMapProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjMap.bz_getMapProperty))),
            .bz_getRangeProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjRange.bz_getRangeProperty))),
            .bz_getStringProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_getStringProperty))),
            .bz_getPatternProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjPattern.bz_getPatternProperty))),
            .bz_getFiberProperty => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjFiber.bz_getFiberProperty))),
            .bz_setTryCtx => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_setTryCtx))),
            .bz_popTryCtx => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.VM.bz_popTryCtx))),
            .bz_valueToCString => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueToCString))),
            .bz_valueToUserData => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueToUserData))),
            .bz_userDataToValue => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjUserData.bz_userDataToValue))),
            .bz_valueToForeignContainerPtr => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueToForeignContainerPtr))),
            .bz_stringZ => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjString.bz_stringZ))),
            .bz_containerGet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjForeignContainer.bz_containerGet))),
            .bz_containerSet => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjForeignContainer.bz_containerSet))),
            .bz_containerInstance => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjForeignContainer.bz_containerInstance))),
            .bz_valueTypeOf => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueTypeOf))),
            .bz_containerFromSlice => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.ObjForeignContainer.bz_containerFromSlice))),
            .memcpy => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.bz_memcpy))),
            .setjmp => @as(
                *anyopaque,
                @ptrFromInt(
                    @intFromPtr(&(if (builtin.os.tag == .macos or builtin.os.tag == .linux or builtin.os.tag == .windows) jmp._setjmp else jmp.setjmp)),
                ),
            ),
            .exit => @as(*anyopaque, @ptrFromInt(@intFromPtr(&bz_exit))),

            .dumpInt => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.dumpInt))),
            .bz_valueDump => @as(*anyopaque, @ptrFromInt(@intFromPtr(&api.Value.bz_valueDump))),
            .fmod => @as(*anyopaque, @ptrFromInt(@intFromPtr(&JIT.fmod))),
            else => {
                io.print("{s}\n", .{self.name()});
                unreachable;
            },
        };
    }

    // FIXME: no need for this we can return @tagName
    pub fn name(self: ExternApi) [*:0]const u8 {
        return switch (self) {
            .nativefn => "NativeFn",
            .rawfn => "RawFn",

            .bz_objStringConcat => "bz_objStringConcat",
            .bz_objStringSubscript => "bz_objStringSubscript",
            .bz_toString => "bz_toString",
            .bz_newList => "bz_newList",
            .bz_newRange => "bz_newRange",
            .bz_rangeNext => "bz_rangeNext",
            .bz_listAppend => "bz_listAppend",
            .bz_listGet => "bz_listGet",
            .bz_listSet => "bz_listSet",
            .bz_valueEqual => "bz_valueEqual",
            .bz_listConcat => "bz_listConcat",
            .bz_newMap => "bz_newMap",
            .bz_mapSet => "bz_mapSet",
            .bz_mapGet => "bz_mapGet",
            .bz_mapConcat => "bz_mapConcat",
            .bz_valueIs => "bz_valueIs",
            .bz_setTryCtx => "bz_setTryCtx",
            .bz_popTryCtx => "bz_popTryCtx",
            .bz_rethrow => "bz_rethrow",
            .bz_throw => "bz_throw",
            .bz_getUpValue => "bz_getUpValue",
            .bz_setUpValue => "bz_setUpValue",
            .bz_closeUpValues => "bz_closeUpValues",
            .bz_closure => "bz_closure",
            .bz_context => "bz_context",
            .bz_instance => "bz_instance",
            .bz_setInstanceProperty => "bz_setInstanceProperty",
            .bz_getInstanceProperty => "bz_getInstanceProperty",
            .bz_getInstanceMethod => "bz_getInstanceMethod",
            .bz_getProtocolMethod => "bz_getProtocolMethod",
            .bz_setObjectField => "bz_setObjectField",
            .bz_getObjectField => "bz_getObjectField",
            .bz_getStringProperty => "bz_getStringProperty",
            .bz_getPatternProperty => "bz_getPatternProperty",
            .bz_getFiberProperty => "bz_getFiberProperty",
            .bz_getRangeProperty => "bz_getRangeProperty",
            .bz_getEnumCase => "bz_getEnumCase",
            .bz_getEnumCaseValue => "bz_getEnumCaseValue",
            .bz_getListProperty => "bz_getListProperty",
            .bz_getMapProperty => "bz_getMapProperty",
            .bz_getEnumCaseFromValue => "bz_getEnumCaseFromValue",
            .bz_bindMethod => "bz_bindMethod",
            .bz_stringNext => "bz_stringNext",
            .bz_listNext => "bz_listNext",
            .bz_mapNext => "bz_mapNext",
            .bz_enumNext => "bz_enumNext",
            .bz_clone => "bz_clone",
            .bz_valueToCString => "bz_valueToCString",
            .bz_valueToUserData => "bz_valueToUserData",
            .bz_userDataToValue => "bz_userDataToValue",
            .bz_valueToForeignContainerPtr => "bz_valueToForeignContainerPtr",
            .bz_stringZ => "bz_stringZ",
            .bz_containerGet => "bz_containerGet",
            .bz_containerSet => "bz_containerSet",
            .bz_containerInstance => "bz_containerInstance",
            .bz_valueTypeOf => "bz_valueTypeOf",
            .bz_containerFromSlice => "bz_containerFromSlice",
            .memcpy => "bz_memcpy",

            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux or builtin.os.tag == .windows) "_setjmp" else "setjmp",
            .exit => "bz_exit",

            .bz_dumpStack => "bz_dumpStack",

            .dumpInt => "dumpInt",
            .bz_valueDump => "bz_valueDump",
            .fmod => "fmod",
        };
    }

    pub fn pname(self: ExternApi) [*:0]const u8 {
        return switch (self) {
            .nativefn => "p_NativeFn",
            .rawfn => "p_RawFn",

            .bz_objStringConcat => "p_bz_objStringConcat",
            .bz_objStringSubscript => "p_bz_objStringSubscript",
            .bz_toString => "p_bz_toString",
            .bz_newList => "p_bz_newList",
            .bz_newRange => "p_bz_newRange",
            .bz_rangeNext => "p_bz_rangeNext",
            .bz_listAppend => "p_bz_listAppend",
            .bz_listGet => "p_bz_listGet",
            .bz_listSet => "p_bz_listSet",
            .bz_valueEqual => "p_bz_valueEqual",
            .bz_listConcat => "p_bz_listConcat",
            .bz_newMap => "p_bz_newMap",
            .bz_mapSet => "p_bz_mapSet",
            .bz_mapGet => "p_bz_mapGet",
            .bz_mapConcat => "p_bz_mapConcat",
            .bz_valueIs => "p_bz_valueIs",
            .bz_setTryCtx => "p_bz_setTryCtx",
            .bz_popTryCtx => "p_bz_popTryCtx",
            .bz_rethrow => "p_bz_rethrow",
            .bz_throw => "p_bz_throw",
            .bz_getUpValue => "p_bz_getUpValue",
            .bz_setUpValue => "p_bz_setUpValue",
            .bz_closeUpValues => "p_bz_closeUpValues",
            .bz_closure => "p_bz_closure",
            .bz_context => "p_bz_context",
            .bz_instance => "p_bz_instance",
            .bz_setInstanceProperty => "p_bz_setInstanceProperty",
            .bz_getInstanceProperty => "p_bz_getInstanceProperty",
            .bz_getInstanceMethod => "p_bz_getInstanceMethod",
            .bz_getProtocolMethod => "p_bz_getProtocolMethod",
            .bz_setObjectField => "p_bz_setObjectField",
            .bz_getObjectField => "p_bz_getObjectField",
            .bz_getStringProperty => "p_bz_getStringProperty",
            .bz_getPatternProperty => "p_bz_getPatternProperty",
            .bz_getFiberProperty => "p_bz_getFiberProperty",
            .bz_getRangeProperty => "p_bz_getRangeProperty",
            .bz_getEnumCase => "p_bz_getEnumCase",
            .bz_getEnumCaseValue => "p_bz_getEnumCaseValue",
            .bz_getListProperty => "p_bz_getListProperty",
            .bz_getMapProperty => "p_bz_getMapProperty",
            .bz_getEnumCaseFromValue => "p_bz_getEnumCaseFromValue",
            .bz_bindMethod => "p_bz_bindMethod",
            .bz_stringNext => "p_bz_stringNext",
            .bz_listNext => "p_bz_listNext",
            .bz_mapNext => "p_bz_mapNext",
            .bz_enumNext => "p_bz_enumNext",
            .bz_clone => "p_bz_clone",
            .bz_valueToCString => "p_bz_valueToCString",
            .bz_valueToUserData => "p_bz_valueToUserData",
            .bz_userDataToValue => "p_bz_userDataToValue",
            .bz_valueToForeignContainerPtr => "p_bz_valueToForeignContainerPtr",
            .bz_stringZ => "p_bz_stringZ",
            .bz_containerGet => "p_bz_containerGet",
            .bz_containerSet => "p_bz_containerSet",
            .bz_containerInstance => "p_bz_containerInstance",
            .bz_valueTypeOf => "p_bz_valueTypeOf",
            .bz_containerFromSlice => "p_bz_containerFromSlice",
            .memcpy => "p_bz_memcpy",

            .setjmp => if (builtin.os.tag == .macos or builtin.os.tag == .linux or builtin.os.windows) "p__setjmp" else "p_setjmp",
            .exit => "p_exit",

            .bz_dumpStack => "p_bz_dumpStack",

            .dumpInt => "p_dumpInt",
            .bz_valueDump => "p_bz_valueDump",
            .fmod => "p_fmod",
        };
    }
};
