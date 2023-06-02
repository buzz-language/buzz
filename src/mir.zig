const std = @import("std");

pub const MIR_no_error: c_int = 0;
pub const MIR_syntax_error: c_int = 1;
pub const MIR_binary_io_error: c_int = 2;
pub const MIR_alloc_error: c_int = 3;
pub const MIR_finish_error: c_int = 4;
pub const MIR_no_module_error: c_int = 5;
pub const MIR_nested_module_error: c_int = 6;
pub const MIR_no_func_error: c_int = 7;
pub const MIR_func_error: c_int = 8;
pub const MIR_vararg_func_error: c_int = 9;
pub const MIR_nested_func_error: c_int = 10;
pub const MIR_wrong_param_value_error: c_int = 11;
pub const MIR_reserved_name_error: c_int = 12;
pub const MIR_import_export_error: c_int = 13;
pub const MIR_undeclared_func_reg_error: c_int = 14;
pub const MIR_repeated_decl_error: c_int = 15;
pub const MIR_reg_type_error: c_int = 16;
pub const MIR_wrong_type_error: c_int = 17;
pub const MIR_unique_reg_error: c_int = 18;
pub const MIR_undeclared_op_ref_error: c_int = 19;
pub const MIR_ops_num_error: c_int = 20;
pub const MIR_call_op_error: c_int = 21;
pub const MIR_unspec_op_error: c_int = 22;
pub const MIR_ret_error: c_int = 23;
pub const MIR_op_mode_error: c_int = 24;
pub const MIR_out_op_error: c_int = 25;
pub const MIR_invalid_insn_error: c_int = 26;
pub const MIR_ctx_change_error: c_int = 27;
pub const MIR_parallel_error: c_int = 28;
pub const enum_MIR_error_type = c_uint;
pub const MIR_error_type_t = enum_MIR_error_type;
pub const MIR_error_func_t = ?*const fn (MIR_error_type_t, [*]const u8, ...) callconv(.C) noreturn;
pub const MIR_MOV: c_int = 0;
pub const MIR_FMOV: c_int = 1;
pub const MIR_DMOV: c_int = 2;
pub const MIR_LDMOV: c_int = 3;
pub const MIR_EXT8: c_int = 4;
pub const MIR_EXT16: c_int = 5;
pub const MIR_EXT32: c_int = 6;
pub const MIR_UEXT8: c_int = 7;
pub const MIR_UEXT16: c_int = 8;
pub const MIR_UEXT32: c_int = 9;
pub const MIR_I2F: c_int = 10;
pub const MIR_I2D: c_int = 11;
pub const MIR_I2LD: c_int = 12;
pub const MIR_UI2F: c_int = 13;
pub const MIR_UI2D: c_int = 14;
pub const MIR_UI2LD: c_int = 15;
pub const MIR_F2I: c_int = 16;
pub const MIR_D2I: c_int = 17;
pub const MIR_LD2I: c_int = 18;
pub const MIR_F2D: c_int = 19;
pub const MIR_F2LD: c_int = 20;
pub const MIR_D2F: c_int = 21;
pub const MIR_D2LD: c_int = 22;
pub const MIR_LD2F: c_int = 23;
pub const MIR_LD2D: c_int = 24;
pub const MIR_NEG: c_int = 25;
pub const MIR_NEGS: c_int = 26;
pub const MIR_FNEG: c_int = 27;
pub const MIR_DNEG: c_int = 28;
pub const MIR_LDNEG: c_int = 29;
pub const MIR_ADD: c_int = 30;
pub const MIR_ADDS: c_int = 31;
pub const MIR_FADD: c_int = 32;
pub const MIR_DADD: c_int = 33;
pub const MIR_LDADD: c_int = 34;
pub const MIR_SUB: c_int = 35;
pub const MIR_SUBS: c_int = 36;
pub const MIR_FSUB: c_int = 37;
pub const MIR_DSUB: c_int = 38;
pub const MIR_LDSUB: c_int = 39;
pub const MIR_MUL: c_int = 40;
pub const MIR_MULS: c_int = 41;
pub const MIR_FMUL: c_int = 42;
pub const MIR_DMUL: c_int = 43;
pub const MIR_LDMUL: c_int = 44;
pub const MIR_DIV: c_int = 45;
pub const MIR_DIVS: c_int = 46;
pub const MIR_UDIV: c_int = 47;
pub const MIR_UDIVS: c_int = 48;
pub const MIR_FDIV: c_int = 49;
pub const MIR_DDIV: c_int = 50;
pub const MIR_LDDIV: c_int = 51;
pub const MIR_MOD: c_int = 52;
pub const MIR_MODS: c_int = 53;
pub const MIR_UMOD: c_int = 54;
pub const MIR_UMODS: c_int = 55;
pub const MIR_AND: c_int = 56;
pub const MIR_ANDS: c_int = 57;
pub const MIR_OR: c_int = 58;
pub const MIR_ORS: c_int = 59;
pub const MIR_XOR: c_int = 60;
pub const MIR_XORS: c_int = 61;
pub const MIR_LSH: c_int = 62;
pub const MIR_LSHS: c_int = 63;
pub const MIR_RSH: c_int = 64;
pub const MIR_RSHS: c_int = 65;
pub const MIR_URSH: c_int = 66;
pub const MIR_URSHS: c_int = 67;
pub const MIR_EQ: c_int = 68;
pub const MIR_EQS: c_int = 69;
pub const MIR_FEQ: c_int = 70;
pub const MIR_DEQ: c_int = 71;
pub const MIR_LDEQ: c_int = 72;
pub const MIR_NE: c_int = 73;
pub const MIR_NES: c_int = 74;
pub const MIR_FNE: c_int = 75;
pub const MIR_DNE: c_int = 76;
pub const MIR_LDNE: c_int = 77;
pub const MIR_LT: c_int = 78;
pub const MIR_LTS: c_int = 79;
pub const MIR_ULT: c_int = 80;
pub const MIR_ULTS: c_int = 81;
pub const MIR_FLT: c_int = 82;
pub const MIR_DLT: c_int = 83;
pub const MIR_LDLT: c_int = 84;
pub const MIR_LE: c_int = 85;
pub const MIR_LES: c_int = 86;
pub const MIR_ULE: c_int = 87;
pub const MIR_ULES: c_int = 88;
pub const MIR_FLE: c_int = 89;
pub const MIR_DLE: c_int = 90;
pub const MIR_LDLE: c_int = 91;
pub const MIR_GT: c_int = 92;
pub const MIR_GTS: c_int = 93;
pub const MIR_UGT: c_int = 94;
pub const MIR_UGTS: c_int = 95;
pub const MIR_FGT: c_int = 96;
pub const MIR_DGT: c_int = 97;
pub const MIR_LDGT: c_int = 98;
pub const MIR_GE: c_int = 99;
pub const MIR_GES: c_int = 100;
pub const MIR_UGE: c_int = 101;
pub const MIR_UGES: c_int = 102;
pub const MIR_FGE: c_int = 103;
pub const MIR_DGE: c_int = 104;
pub const MIR_LDGE: c_int = 105;
pub const MIR_JMP: c_int = 106;
pub const MIR_BT: c_int = 107;
pub const MIR_BTS: c_int = 108;
pub const MIR_BF: c_int = 109;
pub const MIR_BFS: c_int = 110;
pub const MIR_BEQ: c_int = 111;
pub const MIR_BEQS: c_int = 112;
pub const MIR_FBEQ: c_int = 113;
pub const MIR_DBEQ: c_int = 114;
pub const MIR_LDBEQ: c_int = 115;
pub const MIR_BNE: c_int = 116;
pub const MIR_BNES: c_int = 117;
pub const MIR_FBNE: c_int = 118;
pub const MIR_DBNE: c_int = 119;
pub const MIR_LDBNE: c_int = 120;
pub const MIR_BLT: c_int = 121;
pub const MIR_BLTS: c_int = 122;
pub const MIR_UBLT: c_int = 123;
pub const MIR_UBLTS: c_int = 124;
pub const MIR_FBLT: c_int = 125;
pub const MIR_DBLT: c_int = 126;
pub const MIR_LDBLT: c_int = 127;
pub const MIR_BLE: c_int = 128;
pub const MIR_BLES: c_int = 129;
pub const MIR_UBLE: c_int = 130;
pub const MIR_UBLES: c_int = 131;
pub const MIR_FBLE: c_int = 132;
pub const MIR_DBLE: c_int = 133;
pub const MIR_LDBLE: c_int = 134;
pub const MIR_BGT: c_int = 135;
pub const MIR_BGTS: c_int = 136;
pub const MIR_UBGT: c_int = 137;
pub const MIR_UBGTS: c_int = 138;
pub const MIR_FBGT: c_int = 139;
pub const MIR_DBGT: c_int = 140;
pub const MIR_LDBGT: c_int = 141;
pub const MIR_BGE: c_int = 142;
pub const MIR_BGES: c_int = 143;
pub const MIR_UBGE: c_int = 144;
pub const MIR_UBGES: c_int = 145;
pub const MIR_FBGE: c_int = 146;
pub const MIR_DBGE: c_int = 147;
pub const MIR_LDBGE: c_int = 148;
pub const MIR_CALL: c_int = 149;
pub const MIR_INLINE: c_int = 150;
pub const MIR_SWITCH: c_int = 151;
pub const MIR_RET: c_int = 152;
pub const MIR_ALLOCA: c_int = 153;
pub const MIR_BSTART: c_int = 154;
pub const MIR_BEND: c_int = 155;
pub const MIR_VA_ARG: c_int = 156;
pub const MIR_VA_BLOCK_ARG: c_int = 157;
pub const MIR_VA_START: c_int = 158;
pub const MIR_VA_END: c_int = 159;
pub const MIR_LABEL: c_int = 160;
pub const MIR_UNSPEC: c_int = 161;
pub const MIR_PHI: c_int = 162;
pub const MIR_INVALID_INSN: c_int = 163;
pub const MIR_INSN_BOUND: c_int = 164;
pub const MIR_insn_code_t = c_uint;
pub const MIR_T_I8: c_int = 0;
pub const MIR_T_U8: c_int = 1;
pub const MIR_T_I16: c_int = 2;
pub const MIR_T_U16: c_int = 3;
pub const MIR_T_I32: c_int = 4;
pub const MIR_T_U32: c_int = 5;
pub const MIR_T_I64: c_int = 6;
pub const MIR_T_U64: c_int = 7;
pub const MIR_T_F: c_int = 8;
pub const MIR_T_D: c_int = 9;
pub const MIR_T_LD: c_int = 10;
pub const MIR_T_P: c_int = 11;
pub const MIR_T_BLK: c_int = 12;
pub const MIR_T_RBLK: c_int = 17;
pub const MIR_T_UNDEF: c_int = 18;
pub const MIR_T_BOUND: c_int = 19;
pub const MIR_type_t = c_uint;
pub const MIR_scale_t = u8;
pub const MIR_disp_t = i64;
pub const MIR_reg_t = u32;
pub const MIR_imm_t = extern union {
    i: i64,
    u: u64,
    f: f32,
    d: f64,
    ld: c_longdouble,
};
pub const MIR_mem_t = extern struct {
    type: MIR_type_t = 8,
    scale: MIR_scale_t,
    base: MIR_reg_t,
    index: MIR_reg_t,
    disp: MIR_disp_t,
};
pub const MIR_insn_t = ?*struct_MIR_insn;
pub const struct_DLIST_LINK_MIR_insn_t = extern struct {
    prev: MIR_insn_t,
    next: MIR_insn_t,
};
pub const DLIST_LINK_MIR_insn_t = struct_DLIST_LINK_MIR_insn_t; // /usr/local/include/mir.h:302:19: warning: struct demoted to opaque type - has bitfield
pub const struct_MIR_insn = opaque {};
pub const MIR_label_t = ?*struct_MIR_insn;
pub const MIR_name_t = [*:0]const u8;
pub const MIR_OP_UNDEF: c_int = 0;
pub const MIR_OP_REG: c_int = 1;
pub const MIR_OP_HARD_REG: c_int = 2;
pub const MIR_OP_INT: c_int = 3;
pub const MIR_OP_UINT: c_int = 4;
pub const MIR_OP_FLOAT: c_int = 5;
pub const MIR_OP_DOUBLE: c_int = 6;
pub const MIR_OP_LDOUBLE: c_int = 7;
pub const MIR_OP_REF: c_int = 8;
pub const MIR_OP_STR: c_int = 9;
pub const MIR_OP_MEM: c_int = 10;
pub const MIR_OP_HARD_REG_MEM: c_int = 11;
pub const MIR_OP_LABEL: c_int = 12;
pub const MIR_OP_BOUND: c_int = 13;
pub const MIR_op_mode_t = c_uint;
pub const MIR_item_t = *struct_MIR_item;
pub const struct_DLIST_MIR_item_t = extern struct {
    head: MIR_item_t,
    tail: MIR_item_t,
};
pub const DLIST_MIR_item_t = struct_DLIST_MIR_item_t;
pub const struct_DLIST_LINK_MIR_module_t = extern struct {
    prev: MIR_module_t,
    next: MIR_module_t,
};
pub const DLIST_LINK_MIR_module_t = struct_DLIST_LINK_MIR_module_t;
pub const struct_MIR_module = extern struct {
    data: ?*anyopaque,
    name: [*:0]const u8,
    items: DLIST_MIR_item_t,
    module_link: DLIST_LINK_MIR_module_t,
    last_temp_item_num: u32,
};
pub const MIR_module_t = *struct_MIR_module;
pub const struct_DLIST_LINK_MIR_item_t = extern struct {
    prev: MIR_item_t,
    next: MIR_item_t,
};
pub const DLIST_LINK_MIR_item_t = struct_DLIST_LINK_MIR_item_t;
pub const struct_DLIST_MIR_insn_t = extern struct {
    head: MIR_insn_t,
    tail: MIR_insn_t,
};
pub const DLIST_MIR_insn_t = struct_DLIST_MIR_insn_t;
pub const struct_MIR_var = extern struct {
    type: MIR_type_t,
    name: [*:0]const u8,
    size: usize,
};
pub const MIR_var_t = struct_MIR_var;
pub const struct_VARR_MIR_var_t = extern struct {
    els_num: usize,
    size: usize,
    varr: *MIR_var_t,
};
pub const VARR_MIR_var_t = struct_VARR_MIR_var_t;
pub const struct_MIR_func = extern struct {
    name: [*:0]const u8,
    func_item: MIR_item_t,
    original_vars_num: usize,
    insns: DLIST_MIR_insn_t,
    original_insns: DLIST_MIR_insn_t,
    nres: u32,
    nargs: u32,
    last_temp_num: u32,
    n_inlines: u32,
    res_types: *MIR_type_t,
    vararg_p: u8,
    expr_p: u8,
    vars: *VARR_MIR_var_t,
    machine_code: ?*anyopaque,
    call_addr: ?*anyopaque,
    internal: ?*anyopaque,
};
pub const MIR_func_t = *struct_MIR_func;
pub const struct_MIR_proto = extern struct {
    name: [*:0]const u8,
    nres: u32,
    res_types: *MIR_type_t,
    vararg_p: u8,
    args: *VARR_MIR_var_t,
};
pub const MIR_proto_t = *struct_MIR_proto;
const union_unnamed_4 = extern union {
    d: c_longdouble,
    els: [1]u8,
};
pub const struct_MIR_data = extern struct {
    name: [*:0]const u8,
    el_type: MIR_type_t,
    nel: usize,
    u: union_unnamed_4,
};
pub const MIR_data_t = *struct_MIR_data;
pub const struct_MIR_ref_data = extern struct {
    name: [*:0]const u8,
    ref_item: MIR_item_t,
    disp: i64,
    load_addr: ?*anyopaque,
};
pub const MIR_ref_data_t = *struct_MIR_ref_data;
pub const struct_MIR_expr_data = extern struct {
    name: [*:0]const u8,
    expr_item: MIR_item_t,
    load_addr: ?*anyopaque,
};
pub const MIR_expr_data_t = *struct_MIR_expr_data;
pub const struct_MIR_bss = extern struct {
    name: [*]const u8,
    len: u64,
};
pub const MIR_bss_t = *struct_MIR_bss;
const union_unnamed_3 = extern union {
    func: MIR_func_t,
    proto: MIR_proto_t,
    import_id: MIR_name_t,
    export_id: MIR_name_t,
    forward_id: MIR_name_t,
    data: MIR_data_t,
    ref_data: MIR_ref_data_t,
    expr_data: MIR_expr_data_t,
    bss: MIR_bss_t,
};
pub const struct_MIR_item = extern struct {
    data: ?*anyopaque,
    module: MIR_module_t,
    item_link: DLIST_LINK_MIR_item_t,
    item_type: MIR_item_type_t,
    ref_def: MIR_item_t,
    addr: ?*anyopaque,
    export_p: u8,
    section_head_p: u8,
    u: union_unnamed_3,
};
pub const struct_MIR_str = extern struct {
    len: usize,
    s: [*]const u8,
};
pub const MIR_str_t = struct_MIR_str;
const union_unnamed_5 = extern union {
    reg: MIR_reg_t,
    hard_reg: MIR_reg_t,
    i: i64,
    u: u64,
    f: f32,
    d: f64,
    ld: c_longdouble,
    ref: MIR_item_t,
    str: MIR_str_t,
    mem: MIR_mem_t,
    hard_reg_mem: MIR_mem_t,
    label: MIR_label_t,
};
pub const MIR_op_t = extern struct {
    data: ?*anyopaque,
    mode: MIR_op_mode_t,
    value_mode: MIR_op_mode_t,
    u: union_unnamed_5,
};
pub const MIR_func_item: c_int = 0;
pub const MIR_proto_item: c_int = 1;
pub const MIR_import_item: c_int = 2;
pub const MIR_export_item: c_int = 3;
pub const MIR_forward_item: c_int = 4;
pub const MIR_data_item: c_int = 5;
pub const MIR_ref_data_item: c_int = 6;
pub const MIR_expr_data_item: c_int = 7;
pub const MIR_bss_item: c_int = 8;
pub const MIR_item_type_t = c_uint;
pub const struct_DLIST_MIR_module_t = extern struct {
    head: MIR_module_t,
    tail: MIR_module_t,
};
pub const DLIST_MIR_module_t = struct_DLIST_MIR_module_t;
pub const struct_MIR_context = opaque {};
pub const MIR_context_t = ?*struct_MIR_context;

pub extern fn MIR_get_api_version() f64;
pub const MIR_init = _MIR_init;
extern fn _MIR_init() MIR_context_t;
pub extern fn MIR_finish(ctx: MIR_context_t) void;
pub extern fn MIR_new_module(ctx: MIR_context_t, name: [*:0]const u8) MIR_module_t;
pub extern fn MIR_get_module_list(ctx: MIR_context_t) *DLIST_MIR_module_t;
pub extern fn MIR_new_import(ctx: MIR_context_t, name: [*:0]const u8) MIR_item_t;
pub extern fn MIR_new_export(ctx: MIR_context_t, name: [*:0]const u8) MIR_item_t;
pub extern fn MIR_new_forward(ctx: MIR_context_t, name: [*:0]const u8) MIR_item_t;
pub extern fn MIR_new_bss(ctx: MIR_context_t, name: [*:0]const u8, len: usize) MIR_item_t;
pub extern fn MIR_new_data(ctx: MIR_context_t, name: [*:0]const u8, el_type: MIR_type_t, nel: usize, els: ?*const anyopaque) MIR_item_t;
pub extern fn MIR_new_string_data(ctx: MIR_context_t, name: [*:0]const u8, str: MIR_str_t) MIR_item_t;
pub extern fn MIR_new_ref_data(ctx: MIR_context_t, name: [*:0]const u8, item: MIR_item_t, disp: i64) MIR_item_t;
pub extern fn MIR_new_expr_data(ctx: MIR_context_t, name: [*:0]const u8, expr_item: MIR_item_t) MIR_item_t;
pub extern fn MIR_new_proto_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: ?[*]const MIR_type_t, nargs: usize, vars: ?[*]const MIR_var_t) MIR_item_t;
pub extern fn MIR_new_vararg_proto_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, vars: *MIR_var_t) MIR_item_t;
pub extern fn MIR_new_func_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: [*]const MIR_type_t, nargs: usize, vars: [*]const MIR_var_t) MIR_item_t;
pub extern fn MIR_new_vararg_func_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, vars: *MIR_var_t) MIR_item_t;
pub extern fn MIR_item_name(ctx: MIR_context_t, item: MIR_item_t) [*:0]const u8;
pub extern fn MIR_get_item_func(ctx: MIR_context_t, item: MIR_item_t) MIR_func_t;
pub extern fn MIR_new_func_reg(ctx: MIR_context_t, func: MIR_func_t, @"type": MIR_type_t, name: [*:0]const u8) MIR_reg_t;
pub extern fn MIR_finish_func(ctx: MIR_context_t) void;
pub extern fn MIR_finish_module(ctx: MIR_context_t) void;
pub extern fn MIR_get_error_func(ctx: MIR_context_t) MIR_error_func_t;
pub extern fn MIR_set_error_func(ctx: MIR_context_t, func: MIR_error_func_t) void;
pub extern fn MIR_new_insn_arr(ctx: MIR_context_t, code: MIR_insn_code_t, nops: usize, ops: [*]const MIR_op_t) MIR_insn_t;
pub extern fn MIR_copy_insn(ctx: MIR_context_t, insn: MIR_insn_t) MIR_insn_t;
pub extern fn MIR_insn_name(ctx: MIR_context_t, code: MIR_insn_code_t) [*:0]const u8;
pub extern fn MIR_insn_nops(ctx: MIR_context_t, insn: MIR_insn_t) usize;
pub extern fn MIR_insn_op_mode(ctx: MIR_context_t, insn: MIR_insn_t, nop: usize, out_p: *c_int) MIR_op_mode_t;
pub extern fn MIR_new_label(ctx: MIR_context_t) MIR_insn_t;
pub extern fn MIR_reg(ctx: MIR_context_t, reg_name: [*:0]const u8, func: MIR_func_t) MIR_reg_t;
pub extern fn MIR_reg_type(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) MIR_type_t;
pub extern fn MIR_reg_name(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) [*:0]const u8;
pub extern fn MIR_new_reg_op(ctx: MIR_context_t, reg: MIR_reg_t) MIR_op_t;
pub extern fn MIR_new_int_op(ctx: MIR_context_t, v: i64) MIR_op_t;
pub extern fn MIR_new_uint_op(ctx: MIR_context_t, v: u64) MIR_op_t;
pub extern fn MIR_new_float_op(ctx: MIR_context_t, v: f32) MIR_op_t;
pub extern fn MIR_new_double_op(ctx: MIR_context_t, v: f64) MIR_op_t;
pub extern fn MIR_new_ldouble_op(ctx: MIR_context_t, v: c_longdouble) MIR_op_t;
pub extern fn MIR_new_ref_op(ctx: MIR_context_t, item: MIR_item_t) MIR_op_t;
pub extern fn MIR_new_str_op(ctx: MIR_context_t, str: MIR_str_t) MIR_op_t;

pub extern fn MIR_new_mem_op(
    ctx: MIR_context_t,
    @"type": MIR_type_t,
    disp: MIR_disp_t,
    base: MIR_reg_t,
    index: MIR_reg_t,
    scale: MIR_scale_t,
) MIR_op_t;

pub extern fn MIR_new_label_op(ctx: MIR_context_t, label: MIR_label_t) MIR_op_t;
pub extern fn MIR_op_eq_p(ctx: MIR_context_t, op1: MIR_op_t, op2: MIR_op_t) c_int;
pub const htab_hash_t = c_uint;
pub extern fn MIR_op_hash_step(ctx: MIR_context_t, h: htab_hash_t, op: MIR_op_t) htab_hash_t;
pub extern fn MIR_append_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t) void;
pub extern fn MIR_prepend_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t) void;
pub extern fn MIR_insert_insn_after(ctx: MIR_context_t, func: MIR_item_t, after: MIR_insn_t, insn: MIR_insn_t) void;
pub extern fn MIR_insert_insn_before(ctx: MIR_context_t, func: MIR_item_t, before: MIR_insn_t, insn: MIR_insn_t) void;
pub extern fn MIR_remove_insn(ctx: MIR_context_t, func: MIR_item_t, insn: MIR_insn_t) void;
pub extern fn MIR_change_module_ctx(old_ctx: MIR_context_t, m: MIR_module_t, new_ctx: MIR_context_t) void;
pub extern fn MIR_reverse_branch_code(code: MIR_insn_code_t) MIR_insn_code_t;
pub extern fn MIR_type_str(ctx: MIR_context_t, tp: MIR_type_t) [*:0]const u8;
pub extern fn MIR_output_op(ctx: MIR_context_t, f: ?*std.c.FILE, op: MIR_op_t, func: MIR_func_t) void;
pub extern fn MIR_output_insn(ctx: MIR_context_t, f: ?*std.c.FILE, insn: MIR_insn_t, func: MIR_func_t, newline_p: c_int) void;
pub extern fn MIR_output_item(ctx: MIR_context_t, f: ?*std.c.FILE, item: MIR_item_t) void;
pub extern fn MIR_output_module(ctx: MIR_context_t, f: ?*std.c.FILE, module: MIR_module_t) void;
pub extern fn MIR_output(ctx: MIR_context_t, f: ?*std.c.FILE) void;
pub extern fn MIR_write(ctx: MIR_context_t, f: ?*std.c.FILE) void;
pub extern fn MIR_write_module(ctx: MIR_context_t, f: ?*std.c.FILE, module: MIR_module_t) void;
pub extern fn MIR_read(ctx: MIR_context_t, f: ?*std.c.FILE) void;
pub extern fn MIR_write_with_func(ctx: MIR_context_t, writer_func: ?*const fn (MIR_context_t, u8) callconv(.C) c_int) void;
pub extern fn MIR_write_module_with_func(ctx: MIR_context_t, writer_func: ?*const fn (MIR_context_t, u8) callconv(.C) c_int, module: MIR_module_t) void;
pub extern fn MIR_read_with_func(ctx: MIR_context_t, reader_func: ?*const fn (MIR_context_t) callconv(.C) c_int) void;
pub extern fn MIR_scan_string(ctx: MIR_context_t, str: [*:0]const u8) void;
pub extern fn MIR_get_global_item(ctx: MIR_context_t, name: [*:0]const u8) MIR_item_t;
pub extern fn MIR_load_module(ctx: MIR_context_t, m: MIR_module_t) void;
pub extern fn MIR_load_external(ctx: MIR_context_t, name: [*:0]const u8, addr: ?*anyopaque) void;
pub extern fn MIR_link(ctx: MIR_context_t, set_interface: ?*const fn (MIR_context_t, MIR_item_t) callconv(.C) void, import_resolver: ?*const fn ([*:0]const u8) callconv(.C) ?*anyopaque) void;
pub const MIR_val_t = extern union {
    ic: MIR_insn_code_t,
    a: ?*anyopaque,
    i: i64,
    u: u64,
    f: f32,
    d: f64,
    ld: c_longdouble,
};
pub extern fn MIR_interp_arr(ctx: MIR_context_t, func_item: MIR_item_t, results: *MIR_val_t, nargs: usize, vals: *MIR_val_t) void;
pub extern fn MIR_set_interp_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
extern fn _MIR_uniq_string(ctx: MIR_context_t, str: [*:0]const u8) [*:0]const u8;
extern fn _MIR_reserved_ref_name_p(ctx: MIR_context_t, name: [*:0]const u8) c_int;
extern fn _MIR_reserved_name_p(ctx: MIR_context_t, name: [*:0]const u8) c_int;
extern fn _MIR_new_temp_reg(ctx: MIR_context_t, @"type": MIR_type_t, func: MIR_func_t) MIR_reg_t;
extern fn _MIR_type_size(ctx: MIR_context_t, @"type": MIR_type_t) usize;
extern fn _MIR_insn_code_op_mode(ctx: MIR_context_t, code: MIR_insn_code_t, nop: usize, out_p: *c_int) MIR_op_mode_t;
extern fn _MIR_new_unspec_insn(ctx: MIR_context_t, nops: usize, ...) MIR_insn_t;
extern fn _MIR_register_unspec_insn(ctx: MIR_context_t, code: u64, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, vararg_p: c_int, args: *MIR_var_t) void;
extern fn _MIR_duplicate_func_insns(ctx: MIR_context_t, func_item: MIR_item_t) void;
extern fn _MIR_restore_func_insns(ctx: MIR_context_t, func_item: MIR_item_t) void;
extern fn _MIR_get_temp_item_name(ctx: MIR_context_t, module: MIR_module_t, buff: *u8, buff_len: usize) void;
extern fn _MIR_new_hard_reg_op(ctx: MIR_context_t, hard_reg: MIR_reg_t) MIR_op_t;
extern fn _MIR_new_hard_reg_mem_op(ctx: MIR_context_t, @"type": MIR_type_t, disp: MIR_disp_t, base: MIR_reg_t, index: MIR_reg_t, scale: MIR_scale_t) MIR_op_t;
extern fn _MIR_builtin_proto(ctx: MIR_context_t, module: MIR_module_t, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, ...) MIR_item_t;
extern fn _MIR_builtin_func(ctx: MIR_context_t, module: MIR_module_t, name: [*]const u8, addr: ?*anyopaque) MIR_item_t;
extern fn _MIR_flush_code_cache(start: ?*anyopaque, bound: ?*anyopaque) void;
extern fn _MIR_publish_code(ctx: MIR_context_t, code: [*]const u8, code_len: usize) *u8;
extern fn _MIR_get_new_code_addr(ctx: MIR_context_t, size: usize) *u8;
extern fn _MIR_publish_code_by_addr(ctx: MIR_context_t, addr: ?*anyopaque, code: [*]const u8, code_len: usize) *u8;
pub const struct_MIR_code_reloc = extern struct {
    offset: usize,
    value: ?*const anyopaque,
};
pub const MIR_code_reloc_t = struct_MIR_code_reloc;
extern fn _MIR_set_code(prot_start: usize, prot_len: usize, base: *u8, nloc: usize, relocs: *const MIR_code_reloc_t, reloc_size: usize) void;
extern fn _MIR_change_code(ctx: MIR_context_t, addr: *u8, code: [*]const u8, code_len: usize) void;
extern fn _MIR_update_code_arr(ctx: MIR_context_t, base: *u8, nloc: usize, relocs: *const MIR_code_reloc_t) void;
extern fn _MIR_update_code(ctx: MIR_context_t, base: *u8, nloc: usize, ...) void;
pub extern fn va_arg_builtin(p: ?*anyopaque, t: u64) ?*anyopaque;
pub extern fn va_block_arg_builtin(res: ?*anyopaque, p: ?*anyopaque, s: usize, t: u64) void;
pub extern fn va_start_interp_builtin(ctx: MIR_context_t, p: ?*anyopaque, a: ?*anyopaque) void;
pub extern fn va_end_interp_builtin(ctx: MIR_context_t, p: ?*anyopaque) void;
extern fn _MIR_get_bstart_builtin(ctx: MIR_context_t) ?*anyopaque;
extern fn _MIR_get_bend_builtin(ctx: MIR_context_t) ?*anyopaque;
pub const _MIR_arg_desc_t = extern struct {
    type: MIR_type_t,
    size: usize,
};
extern fn _MIR_get_ff_call(ctx: MIR_context_t, nres: usize, res_types: *MIR_type_t, nargs: usize, arg_descs: *_MIR_arg_desc_t, arg_vars_num: usize) ?*anyopaque;
extern fn _MIR_get_interp_shim(ctx: MIR_context_t, func_item: MIR_item_t, handler: ?*anyopaque) ?*anyopaque;
extern fn _MIR_get_thunk(ctx: MIR_context_t) ?*anyopaque;
extern fn _MIR_redirect_thunk(ctx: MIR_context_t, thunk: ?*anyopaque, to: ?*anyopaque) void;
extern fn _MIR_get_wrapper(ctx: MIR_context_t, called_func: MIR_item_t, hook_address: ?*anyopaque) ?*anyopaque;
extern fn _MIR_dump_code(name: [*:0]const u8, index: c_int, code: *u8, code_len: usize) void;
pub extern fn MIR_gen_init(ctx: MIR_context_t, gens_num: c_int) void;
pub extern fn MIR_gen_set_debug_file(ctx: MIR_context_t, gen_num: c_int, f: ?*std.c.FILE) void;
pub extern fn MIR_gen_set_debug_level(ctx: MIR_context_t, gen_num: c_int, debug_level: c_int) void;
pub extern fn MIR_gen_set_optimize_level(ctx: MIR_context_t, gen_num: c_int, level: c_uint) void;
pub extern fn MIR_gen(ctx: MIR_context_t, gen_num: c_int, func_item: MIR_item_t) ?*anyopaque;
pub extern fn MIR_set_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_set_parallel_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_set_lazy_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_gen_finish(ctx: MIR_context_t) void;
