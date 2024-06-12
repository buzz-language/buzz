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
pub const MIR_hard_reg_error: c_int = 12;
pub const MIR_reserved_name_error: c_int = 13;
pub const MIR_import_export_error: c_int = 14;
pub const MIR_undeclared_func_reg_error: c_int = 15;
pub const MIR_repeated_decl_error: c_int = 16;
pub const MIR_reg_type_error: c_int = 17;
pub const MIR_wrong_type_error: c_int = 18;
pub const MIR_unique_reg_error: c_int = 19;
pub const MIR_undeclared_op_ref_error: c_int = 110;
pub const MIR_ops_num_error: c_int = 21;
pub const MIR_call_op_error: c_int = 22;
pub const MIR_unspec_op_error: c_int = 23;
pub const MIR_wrong_lref_error: c_int = 24;
pub const MIR_ret_error: c_int = 25;
pub const MIR_op_mode_error: c_int = 26;
pub const MIR_out_op_error: c_int = 27;
pub const MIR_invalid_insn_error: c_int = 28;
pub const MIR_ctx_change_error: c_int = 29;
pub const MIR_parallel_error: c_int = 210;
pub const enum_MIR_error_type = c_uint;
pub const MIR_error_type_t = enum_MIR_error_type;
pub const MIR_error_func_t = ?*const fn (MIR_error_type_t, [*]const u8, ...) callconv(.C) noreturn;
// zig fmt: off
pub const MIR_Instruction = enum(c_int) {
    // Abbreviations:
    // I - 64-bit int, S - short (32-bit), U - unsigned, F -float, D - double, LD - long double.
    // 2 operand insns:
    MOV, FMOV, DMOV, LDMOV, // Moves
    // Extensions.  Truncation is not necessary because we can use an extension to use a part.
    EXT8, EXT16, EXT32, UEXT8, UEXT16, UEXT32,
    I2F, I2D, I2LD, // Integer to float or (long) double conversion
    UI2F, UI2D, UI2LD, // Unsigned integer to float or (long) double conversion
    F2I, D2I, LD2I, // Float or (long) double to integer conversion
    F2D, F2LD, D2F, D2LD, LD2F, LD2D, // Float, (long) double conversions
    NEG, NEGS, FNEG, DNEG, LDNEG, // Changing sign
    ADDR, ADDR8, ADDR16, ADDR32, // reg addr in natural mode or given integer mode
    // 3 operand insn:
    ADD, ADDS, FADD, DADD, LDADD, // Addition
    SUB, SUBS, FSUB, DSUB, LDSUB, // Subtraction
    MUL, MULS, FMUL, DMUL, LDMUL, // Multiplication
    DIV, DIVS, UDIV, UDIVS, FDIV, DDIV, LDDIV, // Division
    MOD, MODS, UMOD, UMODS, // Modulo
    AND, ANDS, OR, ORS, XOR, XORS, // Logical
    LSH, LSHS, RSH, RSHS, URSH, URSHS, // Right signed/unsigned shift
    EQ, EQS, FEQ, DEQ, LDEQ, // Equality
    NE, NES, FNE, DNE, LDNE, // Inequality
    LT, LTS, ULT, ULTS, FLT, DLT, LDLT, // Less then
    LE, LES, ULE, ULES, FLE, DLE, LDLE, // Less or equal
    GT, GTS, UGT, UGTS, FGT, DGT, LDGT, // Greater then
    GE, GES, UGE, UGES, FGE, DGE, LDGE, // Greater or equal
    ADDO, ADDOS, SUBO, SUBOS, MULO, MULOS, UMULO, UMULOS, // setting overflow flag
    // Unconditional (1 operand) and conditional (2 operands) branch insns.  The first operand is a label.
    JMP, BT, BTS, BF, BFS,
    // Compare and branch (3 operand) insns.  The first operand is the label.
    BEQ, BEQS, FBEQ, DBEQ, LDBEQ,
    BNE, BNES, FBNE, DBNE, LDBNE,
    BLT, BLTS, UBLT, UBLTS, FBLT, DBLT, LDBLT,
    BLE, BLES, UBLE, UBLES, FBLE, DBLE, LDBLE,
    BGT, BGTS, UBGT, UBGTS, FBGT, DBGT, LDBGT,
    BGE, BGES, UBGE, UBGES, FBGE, DBGE, LDBGE,
    BO, UBO, // branch on overflow: prev insn should be overflow add/sub
    BNO, UBNO, // branch on not overflow: prev insn should be overflow add/sub
    LADDR, // put label address (2nd op) into the 1st op
    JMPI, // indirect jump to the label whose address stored in the 1st op
    // 1st operand is a prototype, 2nd one is ref or op containing func address, 3rd and subsequent ops are
    // optional result (if result in the prototype is not of void type), call arguments.
    CALL, INLINE, JCALL,
    // 1st operand is an index, subsequent ops are labels to which goto according the index (1st label has index zero).
    // The insn behavior is undefined if there is no label for the index.
    SWITCH,
    RET,
    JRET, // return by jumping to address of the operand
    // 1 operand insn:
    ALLOCA, // 2 operands: result address and size
    BSTART, BEND, // block start: result addr; block end: addr from block start
    // Special insns:
    VA_ARG, // result is arg address, operands: va_list addr and memory
    VA_BLOCK_ARG, // result is arg address, operands: va_list addr, integer (size), and integer (block type)
    VA_START,
    VA_END, // operand is va_list
    LABEL, // One immediate operand is unique label number
    UNSPEC, // First operand unspec code and the rest are args
    INSN_EL, PRSET, PRBEQ, PRBNE, // work with properties
    USE, // Used only internally in the generator, all operands are input
    PHI, // Used only internally in the generator, the first operand is output
    INVALID_INSN,
    INSN_BOUND, // Should be the last
};
// zig fmt: on
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
pub const MIR_alias_t = u32;
// C gives me a size of 32 and zig 40 and the sum of the fields gives me 33
// In order to not crash, and since i never really access thos fields, i commented out the scale field which takes 1 byte
pub const MIR_mem_t = extern struct {
    type: u8, // MIR_type_t : 8
    scale: MIR_scale_t,
    alias: MIR_alias_t,
    nonalias: MIR_alias_t,
    nloc: u32,
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
pub const MIR_OP_VAR: c_int = 2;
pub const MIR_OP_INT: c_int = 3;
pub const MIR_OP_UINT: c_int = 4;
pub const MIR_OP_FLOAT: c_int = 5;
pub const MIR_OP_DOUBLE: c_int = 6;
pub const MIR_OP_LDOUBLE: c_int = 7;
pub const MIR_OP_REF: c_int = 8;
pub const MIR_OP_STR: c_int = 9;
pub const MIR_OP_MEM: c_int = 10;
pub const MIR_OP_VAR_MEM: c_int = 11;
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
    global_vars: *VARR_MIR_var_t,
    machine_code: ?*anyopaque,
    call_addr: ?*anyopaque,
    internal: ?*anyopaque,
    lref: *struct_MIR_lref_data,
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
pub const struct_MIR_lref_data = struct {
    name: [*:0]const u8,
    label: MIR_label_t,
    label2: MIR_label_t,
    orig_label: MIR_label_t,
    orig_label2: MIR_label_t,
    disp: i64,
    load_addr: *anyopaque,
    next: *struct_MIR_lref_data,
};
pub const MIR_lref_data_t = *struct_MIR_ref_data;
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
    @"var": MIR_reg_t,
    i: i64,
    u: u64,
    f: f32,
    d: f64,
    ld: c_longdouble,
    ref: MIR_item_t,
    str: MIR_str_t,
    mem: MIR_mem_t,
    var_mem: MIR_mem_t,
    label: MIR_label_t,
};
pub const MIR_op_t = extern struct {
    data: *anyopaque,
    mode: u8, // MIR_op_mode_t : 8,
    value_mode: u8, // MIR_op_mode_t : 8,
    u: union_unnamed_5,
};
pub const MIR_func_item: c_int = 0;
pub const MIR_proto_item: c_int = 1;
pub const MIR_import_item: c_int = 2;
pub const MIR_export_item: c_int = 3;
pub const MIR_forward_item: c_int = 4;
pub const MIR_data_item: c_int = 5;
pub const MIR_ref_data_item: c_int = 6;
pub const MIR_lref_data_item: c_int = 7;
pub const MIR_expr_data_item: c_int = 8;
pub const MIR_bss_item: c_int = 9;
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
pub extern fn MIR_new_lref_data(ctx: MIR_context_t, name: [*:0]const u8, label: MIR_label_t, label2: MIR_label_t, disp: i64) MIR_item_t;
pub extern fn MIR_new_expr_data(ctx: MIR_context_t, name: [*:0]const u8, expr_item: MIR_item_t) MIR_item_t;
pub extern fn MIR_new_proto_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: ?[*]const MIR_type_t, nargs: usize, vars: ?[*]const MIR_var_t) MIR_item_t;
pub extern fn MIR_new_vararg_proto_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, vars: *MIR_var_t) MIR_item_t;
pub extern fn MIR_new_func_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: ?[*]const MIR_type_t, nargs: usize, vars: [*]const MIR_var_t) MIR_item_t;
pub extern fn MIR_new_vararg_func_arr(ctx: MIR_context_t, name: [*:0]const u8, nres: usize, res_types: *MIR_type_t, nargs: usize, vars: *MIR_var_t) MIR_item_t;
pub extern fn MIR_item_name(ctx: MIR_context_t, item: MIR_item_t) [*:0]const u8;
pub extern fn MIR_get_item_func(ctx: MIR_context_t, item: MIR_item_t) MIR_func_t;
pub extern fn MIR_new_func_reg(ctx: MIR_context_t, func: MIR_func_t, @"type": MIR_type_t, name: [*:0]const u8) MIR_reg_t;
pub extern fn MIR_new_global_func_reg(ctx: MIR_context_t, func: MIR_func_t, @"type": MIR_type_t, name: [*:0]const u8, hard_reg_name: [*:0]const u8) MIR_reg_t;
pub extern fn MIR_finish_func(ctx: MIR_context_t) void;
pub extern fn MIR_finish_module(ctx: MIR_context_t) void;
pub extern fn MIR_get_error_func(ctx: MIR_context_t) MIR_error_func_t;
pub extern fn MIR_set_error_func(ctx: MIR_context_t, func: MIR_error_func_t) void;
pub extern fn MIR_get_func_redef_permission_p(ctx: MIR_context_t) c_int;
pub extern fn MIR_set_func_redef_permission(ctx: MIR_context_t, flag_p: c_int) void;
pub extern fn MIR_new_insn_arr(ctx: MIR_context_t, code: MIR_insn_code_t, nops: usize, ops: [*]const MIR_op_t) MIR_insn_t;
pub extern fn MIR_copy_insn(ctx: MIR_context_t, insn: MIR_insn_t) MIR_insn_t;
pub extern fn MIR_insn_name(ctx: MIR_context_t, code: MIR_insn_code_t) [*:0]const u8;
pub extern fn MIR_insn_nops(ctx: MIR_context_t, insn: MIR_insn_t) usize;
pub extern fn MIR_insn_op_mode(ctx: MIR_context_t, insn: MIR_insn_t, nop: usize, out_p: *c_int) MIR_op_mode_t;
pub extern fn MIR_new_label(ctx: MIR_context_t) MIR_insn_t;
pub extern fn MIR_reg(ctx: MIR_context_t, reg_name: [*:0]const u8, func: MIR_func_t) MIR_reg_t;
pub extern fn MIR_reg_type(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) MIR_type_t;
pub extern fn MIR_reg_name(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) [*:0]const u8;
pub extern fn MIR_hard_reg_name(ctx: MIR_context_t, reg: MIR_reg_t, func: MIR_func_t) [*:0]const u8;
pub extern fn MIR_alias_name(ctx: MIR_context_t, alias: MIR_alias_t) [*:0]const u8;
pub extern fn MIR_alias(ctx: MIR_context_t, name: [*:0]const u8) MIR_alias_t;
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
pub extern fn MIR_new_alias_mem_op(
    ctx: MIR_context_t,
    type: MIR_type_t,
    disp: MIR_disp_t,
    base: MIR_reg_t,
    index: MIR_reg_t,
    scale: MIR_scale_t,
    alias: MIR_alias_t,
    @"noalias": MIR_alias_t,
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
pub extern fn MIR_output_str(ctx: MIR_context_t, f: ?*std.c.FILE, str: MIR_str_t) void;
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
extern fn _MIR_new_var_op(ctx: MIR_context_t, hard_reg: MIR_reg_t) MIR_op_t;
extern fn _MIR_new_var_mem_op(ctx: MIR_context_t, @"type": MIR_type_t, disp: MIR_disp_t, base: MIR_reg_t, index: MIR_reg_t, scale: MIR_scale_t) MIR_op_t;
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
pub extern fn MIR_gen_init(ctx: MIR_context_t) void;
pub extern fn MIR_gen_set_debug_file(ctx: MIR_context_t, f: ?*std.c.FILE) void;
pub extern fn MIR_gen_set_debug_level(ctx: MIR_context_t, debug_level: c_int) void;
pub extern fn MIR_gen_set_optimize_level(ctx: MIR_context_t, level: c_uint) void;
pub extern fn MIR_gen(ctx: MIR_context_t, func_item: MIR_item_t) ?*anyopaque;
pub extern fn MIR_set_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_set_parallel_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_set_lazy_gen_interface(ctx: MIR_context_t, func_item: MIR_item_t) void;
pub extern fn MIR_gen_finish(ctx: MIR_context_t) void;
