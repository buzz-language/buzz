const builtin = @import("builtin");
const is_windows = builtin.os.tag == .windows;
const is_linux = builtin.os.tag == .linux;

const WindowsJmp = struct {
    /// Conservative storage for the platform C `jmp_buf`.
    ///
    /// The exact layout is libc-private and differs by target. Windows x86_64
    /// requires 256 bytes with 16-byte alignment, while the old `[48]c_int`
    /// binding was only 192 bytes and 4-byte aligned.
    pub const jmp_buf = extern struct {
        bytes: [256]u8 align(16),
    };

    pub extern fn setjmp(*WindowsJmp.jmp_buf) c_int;
    pub extern fn __intrinsic_setjmpex(*WindowsJmp.jmp_buf, ?*anyopaque) c_int;
    pub extern fn longjmp(*WindowsJmp.jmp_buf, c_int) noreturn;
};

const jmp = @cImport({
    @cInclude("setjmp.h");
});

const LinuxJmp = struct {
    pub extern fn _setjmp(*jmp.jmp_buf) c_int;
    pub extern fn _longjmp(*jmp.jmp_buf, c_int) noreturn;
};

pub const jmp_buf = if (is_windows)
    WindowsJmp.jmp_buf
else
    jmp.jmp_buf;

pub const setjmp = if (is_windows)
    WindowsJmp.setjmp
else if (is_linux)
    LinuxJmp._setjmp
else
    jmp.setjmp;

pub const longjmp = if (is_windows)
    WindowsJmp.longjmp
else if (is_linux)
    LinuxJmp._longjmp
else
    jmp.longjmp;
