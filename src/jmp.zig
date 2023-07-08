// FIXME: remove setjmp/longjmp  https://github.com/ziglang/zig/issues/1656#issuecomment-840190292
// tip: use minicoro instead  https://github.com/edubart/minicoro

const c_jmp = @cImport({
    @cInclude("setjmp.h");
});

pub const jmp = c_jmp;
