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

export fn ls(vm: *api.VM) bool {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse "", 0);

    const dir: std.fs.Dir = if (std.fs.path.isAbsolute(filename))
        std.fs.openDirAbsolute(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return false;
        }
    else
        std.fs.cwd().openDir(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return false;
        };

    var list = api.ObjList.bz_newList(vm, api.ObjTypeDef.bz_stringType() orelse {
        vm.bz_throwString("Could not list directory");

        return false;
    }) orelse {
        vm.bz_throwString("Could not list directory");

        return false;
    };

    var it = dir.iterate();
    while (it.next() catch {
        vm.bz_throwString("Could not list directory");

        return false;
    }) |element| {
        vm.bz_pushString(api.ObjString.bz_string(vm, utils.toCString(api.VM.allocator, element.name) orelse {
            vm.bz_throwString("Could not list directory");

            return false;
        }) orelse {
            vm.bz_throwString("Could not list directory");

            return false;
        });

        if (!list.bz_listAppend(vm.bz_pop())) {
            vm.bz_throwString("Could not list directory");

            return false;
        }
    }

    vm.bz_pushList(list);

    return true;
}
