const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");

export fn mkDir(vm: *api.VM) bool {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse "", 0);

    if (std.fs.path.isAbsolute(filename)) {
        std.fs.makeDirAbsolute(filename) catch {
            vm.bz_throwString("Could not create directory");

            return false;
        };
    } else {
        std.fs.cwd().makeDir(filename) catch {
            vm.bz_throwString("Could not create directory");

            return false;
        };
    }

    return false;
}

export fn rm(vm: *api.VM) bool {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse "", 0);

    if (std.fs.path.isAbsolute(filename)) {
        std.fs.deleteTreeAbsolute(filename) catch {
            vm.bz_throwString("Could not delete file or directory");

            return false;
        };
    } else {
        std.fs.cwd().deleteTree(filename) catch {
            vm.bz_throwString("Could not delete file or directory");

            return false;
        };
    }

    return false;
}

export fn move(_: *api.VM) bool {
    unreachable;
}

export fn lsDir(_: *api.VM) bool {
    unreachable;
}
