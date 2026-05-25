pub const linenoiseState = extern struct {
    in_completion: c_int,
    completion_idx: usize,
    ifd: c_int,
    ofd: c_int,
    buf: [*]u8,
    buflen: usize,
    prompt: [*:0]const u8,
    plen: usize,
    pos: usize,
    oldpos: usize,
    len: usize,
    cols: usize,
    oldrows: usize,
    history_index: c_int,
};

pub extern var linenoiseEditMore: [*:0]u8;

// Non blocking API.
pub extern fn linenoiseEditStart(state: *linenoiseState, stdin_fd: c_int, stdout_fd: c_int, buf: [*]u8, buflen: usize, prompt: [*:0]const u8) callconv(.c) c_int;
pub extern fn linenoiseEditFeed(state: *linenoiseState) callconv(.c) ?[*:0]u8;
pub extern fn linenoiseEditStop(state: *linenoiseState) callconv(.c) void;
pub extern fn linenoiseHide(state: *linenoiseState) callconv(.c) void;
pub extern fn linenoiseShow(state: *linenoiseState) callconv(.c) void;

pub const linenoiseCompletions = extern struct {
    len: usize,
    cvec: *[*:0]u8,
};

// Blocking API.
pub extern fn linenoise(prompt: [*:0]const u8) callconv(.c) ?[*:0]const u8;
pub extern fn linenoiseFree(ptr: ?*anyopaque) callconv(.c) void;

// Completion API.
pub const linenoiseCompletionCallback = fn ([*:0]const u8, *linenoiseCompletions) void;
pub const linenoiseHintsCallback = fn ([*:0]const u8, *c_int, *c_int) [*:0]const u8;
pub const linenoiseFreeHintsCallback = fn (*anyopaque) void;
pub extern fn linenoiseSetCompletionCallback(callback: *linenoiseCompletionCallback) callconv(.c) void;
pub extern fn linenoiseSetHintsCallback(callback: *linenoiseHintsCallback) callconv(.c) void;
pub extern fn linenoiseSetFreeHintsCallback(callback: *linenoiseFreeHintsCallback) callconv(.c) void;
pub extern fn linenoiseAddCompletion(completions: *linenoiseCompletions, completion: [*:0]const u8) callconv(.c) void;

// History API.
pub extern fn linenoiseHistoryAdd(line: [*:0]const u8) callconv(.c) c_int;
pub extern fn linenoiseHistorySetMaxLen(len: c_int) callconv(.c) c_int;
pub extern fn linenoiseHistorySave(filename: [*:0]const u8) callconv(.c) c_int;
pub extern fn linenoiseHistoryLoad(filename: [*:0]const u8) callconv(.c) c_int;

// Other utilities.
pub extern fn linenoiseClearScreen() callconv(.c) void;
pub extern fn linenoiseSetMultiLine(ml: c_int) callconv(.c) void;
pub extern fn linenoisePrintKeyCodes() callconv(.c) void;
pub extern fn linenoiseMaskModeEnable() callconv(.c) void;
pub extern fn linenoiseMaskModeDisable() callconv(.c) void;
