/// Conservative storage for the platform C `jmp_buf`.
///
/// The exact layout is libc-private and differs by target. Windows x86_64
/// requires 256 bytes with 16-byte alignment, while the old `[48]c_int`
/// binding was only 192 bytes and 4-byte aligned.
pub const jmp_buf = extern struct {
    bytes: [256]u8 align(16),
};

pub extern fn _setjmp(*jmp_buf) c_int;
pub extern fn setjmp(*jmp_buf) c_int;
pub extern fn __intrinsic_setjmpex(*jmp_buf, ?*anyopaque) c_int;
pub extern fn longjmp(*jmp_buf, c_int) noreturn;
pub extern fn _longjmp(*jmp_buf, c_int) noreturn;
