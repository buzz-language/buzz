pub const jmp_buf = [48]c_int;

pub extern fn _setjmp([*c]c_int) c_int;
pub extern fn setjmp([*c]c_int) c_int;
pub extern fn longjmp([*c]c_int, c_int) noreturn;
pub extern fn _longjmp([*c]c_int, c_int) noreturn;
