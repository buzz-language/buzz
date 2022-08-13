const std = @import("std");
const api = @import("./buzz_api.zig");
const utils = @import("../src/utils.zig");

export fn makeDirectory(vm: *api.VM) c_int {
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

export fn delete(vm: *api.VM) c_int {
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

export fn move(vm: *api.VM) c_int {
    const source: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(1)) orelse {
        vm.bz_throwString("Could not get source");

        return -1;
    }, 0);

    const destination: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not get destination");

        return -1;
    }, 0);

    const source_is_absolute = std.fs.path.isAbsolute(source);
    const destination_is_absolute = std.fs.path.isAbsolute(destination);

    if (source_is_absolute and destination_is_absolute) {
        std.fs.renameAbsolute(source, destination) catch {
            vm.bz_throwString("Could move file");

            return -1;
        };
    } else if (!source_is_absolute and !destination_is_absolute) {
        std.fs.cwd().rename(source, destination) catch {
            vm.bz_throwString("Could move file");

            return -1;
        };
    } else {
        const source_absolute = if (source_is_absolute) source else std.fs.cwd().realpathAlloc(api.VM.allocator, source) catch {
            vm.bz_throwString("Could move file");

            return -1;
        };
        const destination_absolute = if (destination_is_absolute) destination else std.fs.cwd().realpathAlloc(api.VM.allocator, destination) catch {
            vm.bz_throwString("Could move file");

            return -1;
        };
        defer {
            if (source_is_absolute) {
                api.VM.allocator.free(source_absolute);
            }

            if (destination_is_absolute) {
                api.VM.allocator.free(destination_absolute);
            }
        }

        std.fs.renameAbsolute(source_absolute, destination_absolute) catch {
            vm.bz_throwString("Could move file");

            return -1;
        };
    }

    return 0;
}

export fn list(vm: *api.VM) c_int {
    const filename: []const u8 = std.mem.sliceTo(api.Value.bz_valueToString(vm.bz_peek(0)) orelse {
        vm.bz_throwString("Could not get filename");

        return -1;
    }, 0);

    const dir = if (std.fs.path.isAbsolute(filename))
        std.fs.openIterableDirAbsolute(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return -1;
        }
    else
        std.fs.cwd().openIterableDir(filename, .{}) catch {
            vm.bz_throwString("Could not list directory");

            return -1;
        };

    var file_list = api.ObjList.bz_newList(vm, api.ObjTypeDef.bz_stringType() orelse {
        vm.bz_throwString("Could not list directory");

        return -1;
    }) orelse {
        vm.bz_throwString("Could not list directory");

        return -1;
    };

    vm.bz_pushList(file_list);

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

        if (!file_list.bz_listAppend(vm.bz_pop())) {
            vm.bz_throwString("Could not list directory");

            return -1;
        }
    }

    return 1;
}
