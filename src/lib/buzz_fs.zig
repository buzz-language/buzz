const std = @import("std");
const api = @import("./buzz_api.zig");

fn handleMakeDirectoryError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.DiskQuota => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DiskQuota", "DiskQuota".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.LinkQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "LinkQuotaExceeded", "LinkQuotaExceeded".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "lib.errors.OutOfMemoryError", "lib.errors.FileNotFoundError".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.PathAlreadyExists => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.ReadOnlyFileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ReadOnlyFileSystem", "ReadOnlyFileSystem".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),

        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
    }
}

export fn makeDirectory(vm: *api.VM, _: null, _: 0, _: null, _: 0) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];
    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.makeDirAbsolute(filename_slice) catch |err| {
            handleMakeDirectoryError(vm, err);

            return -1;
        };
    } else {
        std.fs.cwd().makeDir(filename_slice) catch |err| {
            handleMakeDirectoryError(vm, err);

            return -1;
        };
    }

    return 0;
}

fn handleDeleteDirectoryError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.ReadOnlyFileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ReadOnlyFileSystem", "ReadOnlyFileSystem".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        // Zig doesn't let me use those even though it lists them as being raised
        // error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
        // error.CannotDeleteRootDirectory => vm.bz_pushError("lib.errors.CannotDeleteRootDirectoryError", "lib.errors.CannotDeleteRootDirectoryError".len),
        else => unreachable,
    }
}

export fn delete(vm: *api.VM, _: null, _: 0, _: null, _: 0) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];

    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.deleteTreeAbsolute(filename_slice) catch |err| {
            handleDeleteDirectoryError(vm, err);

            return -1;
        };
    } else {
        std.fs.cwd().deleteTree(filename_slice) catch |err| {
            handleDeleteDirectoryError(vm, err);

            return -1;
        };
    }

    return 0;
}

fn handleMoveError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.FileBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.DiskQuota => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DiskQuota", "DiskQuota".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.LinkQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "LinkQuotaExceeded", "LinkQuotaExceeded".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
        error.PathAlreadyExists => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.ReadOnlyFileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ReadOnlyFileSystem", "ReadOnlyFileSystem".len),
        error.RenameAcrossMountPoints => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "RenameAcrossMountPoints", "RenameAcrossMountPoints".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.SharingViolation => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SharingViolation", "SharingViolation".len),
        error.PipeBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PipeBusy", "PipeBusy".len),
        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
    }
}

fn handleRealpathError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.FileBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
        error.PathAlreadyExists => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.SharingViolation => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SharingViolation", "SharingViolation".len),
        error.PipeBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PipeBusy", "PipeBusy".len),
        error.NotSupported => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotSupported", "NotSupported".len),
        error.InputOutput => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InputOutput", "InputOutput".len),
        error.FileTooBig => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.FileSystem => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileSystem", "FileSystem".len),
        error.DeviceBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DeviceBusy", "DeviceBusy".len),
        error.InvalidHandle => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidHandle", "InvalidHandle".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.OutOfMemory => vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
    }
}

export fn move(vm: *api.VM, _: null, _: 0, _: null, _: 0) c_int {
    var len: usize = 0;
    const source = vm.bz_peek(1).bz_valueToString(&len);
    const source_slice = source.?[0..len];

    const destination = vm.bz_peek(0).bz_valueToString(&len);
    const destination_slice = destination.?[0..len];

    const source_is_absolute = std.fs.path.isAbsolute(source_slice);
    const destination_is_absolute = std.fs.path.isAbsolute(destination_slice);

    if (source_is_absolute and destination_is_absolute) {
        std.fs.renameAbsolute(source_slice, destination_slice) catch |err| {
            handleMoveError(vm, err);

            return -1;
        };
    } else if (!source_is_absolute and !destination_is_absolute) {
        std.fs.cwd().rename(source_slice, destination_slice) catch |err| {
            handleMoveError(vm, err);

            return -1;
        };
    } else {
        const source_absolute = if (source_is_absolute) source_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, source_slice) catch |err| {
            handleRealpathError(vm, err);

            return -1;
        };
        const destination_absolute = if (destination_is_absolute) destination_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, destination_slice) catch |err| {
            handleRealpathError(vm, err);

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

        std.fs.renameAbsolute(source_absolute, destination_absolute) catch |err| {
            handleMoveError(vm, err);

            return -1;
        };
    }

    return 0;
}

fn handleOpenDirAbsoluteError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.DeviceBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DeviceBusy", "DeviceBusy".len),
        error.FileBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileBusy", "FileBusy".len),
        error.FileLocksNotSupported => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileLocksNotSupported", "FileLocksNotSupported".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
        error.FileTooBig => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "FileTooBig", "FileTooBig".len),
        error.InvalidHandle => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidHandle", "InvalidHandle".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.IsDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "IsDir", "IsDir".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NoSpaceLeft => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoSpaceLeft", "NoSpaceLeft".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.PathAlreadyExists => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PathAlreadyExists", "PathAlreadyExists".len),
        error.PipeBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "PipeBusy", "PipeBusy".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SharingViolation => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SharingViolation", "SharingViolation".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.WouldBlock => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "WouldBlock", "WouldBlock".len),
    }
}

fn handleOpenDirError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.BadPathName => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "BadPathName", "BadPathName".len),
        error.DeviceBusy => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "DeviceBusy", "DeviceBusy".len),
        error.InvalidHandle => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidHandle", "InvalidHandle".len),
        error.InvalidUtf8 => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "InvalidUtf8", "InvalidUtf8".len),
        error.NameTooLong => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NameTooLong", "NameTooLong".len),
        error.NoDevice => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NoDevice", "NoDevice".len),
        error.NotDir => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "NotDir", "NotDir".len),
        error.ProcessFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "ProcessFdQuotaExceeded", "ProcessFdQuotaExceeded".len),
        error.SymLinkLoop => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SymLinkLoop", "SymLinkLoop".len),
        error.SystemFdQuotaExceeded => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemFdQuotaExceeded", "SystemFdQuotaExceeded".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),

        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
        error.FileNotFound => vm.bz_pushError("lib.errors.FileNotFoundError", "lib.errors.FileNotFoundError".len),
    }
}

fn handleDirIterateError(vm: *api.VM, err: anytype) void {
    switch (err) {
        error.AccessDenied => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "AccessDenied", "AccessDenied".len),
        error.SystemResources => vm.bz_pushErrorEnum("lib.errors.FileSystemError", "lib.errors.FileSystemError".len, "SystemResources", "SystemResources".len),
        error.Unexpected => vm.bz_pushError("lib.errors.UnexpectedError", "lib.errors.UnexpectedError".len),
    }
}

export fn list(vm: *api.VM, _: null, _: 0, _: null, _: 0) c_int {
    var len: usize = 0;
    const filename = vm.bz_peek(0).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    const dir = if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openIterableDirAbsolute(filename_slice, .{}) catch |err| {
            handleOpenDirAbsoluteError(vm, err);

            return -1;
        }
    else
        std.fs.cwd().openIterableDir(filename_slice, .{}) catch |err| {
            handleOpenDirError(vm, err);

            return -1;
        };

    var file_list = api.ObjList.bz_newList(vm, api.ObjTypeDef.bz_stringType() orelse {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    }) orelse {
        vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

        return -1;
    };

    vm.bz_pushList(file_list);

    var it = dir.iterate();
    while (it.next() catch |err| {
        handleDirIterateError(vm, err);

        return -1;
    }) |element| {
        vm.bz_pushString(api.ObjString.bz_string(vm, if (element.name.len > 0) @ptrCast([*]const u8, element.name) else null, element.name.len) orelse {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        });

        if (!file_list.bz_listAppend(vm.bz_getGC(), vm.bz_pop())) {
            vm.bz_pushError("lib.errors.OutOfMemoryError", "lib.errors.OutOfMemoryError".len);

            return -1;
        }
    }

    return 1;
}
