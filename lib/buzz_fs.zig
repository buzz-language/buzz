const std = @import("std");
const api = @import("./buzz_api.zig");

export fn makeDirectory(vm: *api.VM) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_throwString("File name is empty", "File name is empty".len);

        return -1;
    }

    const filename_slice = filename.?[0..len];
    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.makeDirAbsolute(filename_slice) catch {
            vm.bz_throwString("Could not create directory", "Could not create directory".len);

            return -1;
        };
    } else {
        std.fs.cwd().makeDir(filename_slice) catch {
            vm.bz_throwString("Could not create directory", "Could not create directory".len);

            return -1;
        };
    }

    return 0;
}

export fn delete(vm: *api.VM) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_throwString("File name is empty", "File name is empty".len);

        return -1;
    }

    const filename_slice = filename.?[0..len];

    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.deleteTreeAbsolute(filename_slice) catch {
            vm.bz_throwString("Could not delete file or directory", "Could not delete file or directory".len);

            return -1;
        };
    } else {
        std.fs.cwd().deleteTree(filename_slice) catch {
            vm.bz_throwString("Could not delete file or directory", "Could not delete file or directory".len);

            return -1;
        };
    }

    return 0;
}

export fn move(vm: *api.VM) c_int {
    var len: usize = 0;
    const source = vm.bz_peek(1).bz_valueToString(&len);
    if (len == 0) {
        vm.bz_throwString("Source is empty", "Source is empty".len);

        return -1;
    }
    const source_slice = source.?[0..len];

    const destination = vm.bz_peek(0).bz_valueToString(&len);
    if (len == 0) {
        vm.bz_throwString("Source is empty", "Source is empty".len);

        return -1;
    }
    const destination_slice = destination.?[0..len];

    const source_is_absolute = std.fs.path.isAbsolute(source_slice);
    const destination_is_absolute = std.fs.path.isAbsolute(destination_slice);

    if (source_is_absolute and destination_is_absolute) {
        std.fs.renameAbsolute(source_slice, destination_slice) catch {
            vm.bz_throwString("Could move file", "Could move file".len);

            return -1;
        };
    } else if (!source_is_absolute and !destination_is_absolute) {
        std.fs.cwd().rename(source_slice, destination_slice) catch {
            vm.bz_throwString("Could move file", "Could move file".len);

            return -1;
        };
    } else {
        const source_absolute = if (source_is_absolute) source_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, source_slice) catch {
            vm.bz_throwString("Could move file", "Could move file".len);

            return -1;
        };
        const destination_absolute = if (destination_is_absolute) destination_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, destination_slice) catch {
            vm.bz_throwString("Could move file", "Could move file".len);

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
            vm.bz_throwString("Could move file", "Could move file".len);

            return -1;
        };
    }

    return 0;
}

export fn list(vm: *api.VM) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_throwString("File name is empty", "File name is empty".len);

        return -1;
    }

    const filename_slice = filename.?[0..len];

    const dir = if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openIterableDirAbsolute(filename_slice, .{}) catch {
            vm.bz_throwString("Could not list directory", "Could not list directory".len);

            return -1;
        }
    else
        std.fs.cwd().openIterableDir(filename_slice, .{}) catch {
            vm.bz_throwString("Could not list directory", "Could not list directory".len);

            return -1;
        };

    var file_list = api.ObjList.bz_newList(vm, api.ObjTypeDef.bz_stringType() orelse {
        vm.bz_throwString("Could not list directory", "Could not list directory".len);

        return -1;
    }) orelse {
        vm.bz_throwString("Could not list directory", "Could not list directory".len);

        return -1;
    };

    vm.bz_pushList(file_list);

    var it = dir.iterate();
    while (it.next() catch {
        vm.bz_throwString("Could not list directory", "Could not list directory".len);

        return -1;
    }) |element| {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (element.name.len > 0) @ptrCast([*]const u8, element.name) else null, element.name.len) orelse {
            vm.bz_throwString("Could not list directory", "Could not list directory".len);

            return -1;
        });

        if (!file_list.bz_listAppend(vm.bz_getGC(), vm.bz_pop())) {
            vm.bz_throwString("Could not list directory", "Could not list directory".len);

            return -1;
        }
    }

    return 1;
}
