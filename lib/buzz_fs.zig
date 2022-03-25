const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");

export fn mkDir(vm: *api.VM) c_int {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not get filename");

        return -1;
    }, 0);

    if (std.fs.path.isAbsolute(filename)) {
        std.fs.makeDirAbsolute(filename) catch {
            vm.bz_throwString("Could not create directory");

            return -1;
        };
    } else {
        std.fs.cwd().makeDir(filename) catch {
            vm.bz_throwString("Could not create directory");

            return -1;
        };
    }

    return 0;
}

export fn rm(vm: *api.VM) c_int {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not get filename");

        return -1;
    }, 0);

    if (std.fs.path.isAbsolute(filename)) {
        std.fs.deleteTreeAbsolute(filename) catch {
            vm.bz_throwString("Could not delete file or directory");

            return -1;
        };
    } else {
        std.fs.cwd().deleteTree(filename) catch {
            vm.bz_throwString("Could not delete file or directory");

            return -1;
        };
    }

    return 0;
}

export fn move(_: *api.VM) c_int {
    unreachable;
}

export fn ls(vm: *api.VM) c_int {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not get filename");

        return -1;
    }, 0);

    const dir: std.fs.Dir = if (std.fs.path.isAbsolute(filename))
        std.fs.openDirAbsolute(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return -1;
        }
    else
        std.fs.cwd().openDir(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return -1;
        };

    var list = api.ObjList.bz_newList(vm, api.ObjTypeDef.bz_stringType() orelse {
        vm.bz_throwString("Could not list directory");

        return -1;
    }) orelse {
        vm.bz_throwString("Could not list directory");

        return -1;
    };

    var it = dir.iterate();
    while (it.next() catch {
        vm.bz_throwString("Could not list directory");

        return -1;
    }) |element| {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, element.name) orelse {
            vm.bz_throwString("Could not list directory");

            return -1;
        }) orelse {
            vm.bz_throwString("Could not list directory");

            return -1;
        });

        if (!list.bz_listAppend(vm.bz_pop())) {
            vm.bz_throwString("Could not list directory");

            return -1;
        }
    }

    vm.bz_pushList(list);

    return 1;
}
