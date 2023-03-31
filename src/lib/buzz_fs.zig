const std = @import("std");
const api = @import("./buzz_api.zig");

fn handleMakeDirectoryError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.DiskQuota => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DiskQuota"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.LinkQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "LinkQuotaExceeded"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),

        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
    }
}

export fn makeDirectory(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];
    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.makeDirAbsolute(filename_slice) catch |err| {
            handleMakeDirectoryError(ctx, err);

            return -1;
        };
    } else {
        std.fs.cwd().makeDir(filename_slice) catch |err| {
            handleMakeDirectoryError(ctx, err);

            return -1;
        };
    }

    return 0;
}

fn handleDeleteDirectoryError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        // Zig doesn't let me use those even though it lists them as being raised
        // error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
        // error.CannotDeleteRootDirectory => ctx.vm.pushError("lib.errors.CannotDeleteRootDirectoryError"),
        else => unreachable,
    }
}

export fn delete(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];

    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.deleteTreeAbsolute(filename_slice) catch |err| {
            handleDeleteDirectoryError(ctx, err);

            return -1;
        };
    } else {
        std.fs.cwd().deleteTree(filename_slice) catch |err| {
            handleDeleteDirectoryError(ctx, err);

            return -1;
        };
    }

    return 0;
}

fn handleMoveError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.DiskQuota => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DiskQuota"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.LinkQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "LinkQuotaExceeded"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.ReadOnlyFileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ReadOnlyFileSystem"),
        error.RenameAcrossMountPoints => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "RenameAcrossMountPoints"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
        error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
    }
}

fn handleRealpathError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
        error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
        error.NotSupported => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotSupported"),
        error.InputOutput => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InputOutput"),
        error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemFdQuotaExceeded"),
        error.FileSystem => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileSystem"),
        error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.OutOfMemory => ctx.vm.pushError("lib.errors.OutOfMemoryError"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
    }
}

export fn move(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const source = ctx.vm.bz_peek(1).bz_valueToString(&len);
    const source_slice = source.?[0..len];

    const destination = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const destination_slice = destination.?[0..len];

    const source_is_absolute = std.fs.path.isAbsolute(source_slice);
    const destination_is_absolute = std.fs.path.isAbsolute(destination_slice);

    if (source_is_absolute and destination_is_absolute) {
        std.fs.renameAbsolute(source_slice, destination_slice) catch |err| {
            handleMoveError(ctx, err);

            return -1;
        };
    } else if (!source_is_absolute and !destination_is_absolute) {
        std.fs.cwd().rename(source_slice, destination_slice) catch |err| {
            handleMoveError(ctx, err);

            return -1;
        };
    } else {
        const source_absolute = if (source_is_absolute) source_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, source_slice) catch |err| {
            handleRealpathError(ctx, err);

            return -1;
        };
        const destination_absolute = if (destination_is_absolute) destination_slice else std.fs.cwd().realpathAlloc(api.VM.allocator, destination_slice) catch |err| {
            handleRealpathError(ctx, err);

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
            handleMoveError(ctx, err);

            return -1;
        };
    }

    return 0;
}

fn handleOpenDirAbsoluteError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
        error.FileBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileBusy"),
        error.FileLocksNotSupported => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileLocksNotSupported"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
        error.FileTooBig => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "FileTooBig"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.IsDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "IsDir"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NoSpaceLeft => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoSpaceLeft"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.PathAlreadyExists => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PathAlreadyExists"),
        error.PipeBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "PipeBusy"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
        error.SharingViolation => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SharingViolation"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemFdQuotaExceeded"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.WouldBlock => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "WouldBlock"),
    }
}

fn handleOpenDirError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.BadPathName => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "BadPathName"),
        error.DeviceBusy => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "DeviceBusy"),
        error.InvalidHandle => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidHandle"),
        error.InvalidUtf8 => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "InvalidUtf8"),
        error.NameTooLong => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NameTooLong"),
        error.NoDevice => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NoDevice"),
        error.NotDir => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "NotDir"),
        error.ProcessFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "ProcessFdQuotaExceeded"),
        error.SymLinkLoop => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SymLinkLoop"),
        error.SystemFdQuotaExceeded => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemFdQuotaExceeded"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),

        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
        error.FileNotFound => ctx.vm.pushError("lib.errors.FileNotFoundError"),
    }
}

fn handleDirIterateError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "AccessDenied"),
        error.SystemResources => ctx.vm.pushErrorEnum("lib.errors.FileSystemError", "SystemResources"),
        error.Unexpected => ctx.vm.pushError("lib.errors.UnexpectedError"),
    }
}

export fn list(ctx: *api.NativeCtx) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    const dir = if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openIterableDirAbsolute(filename_slice, .{}) catch |err| {
            handleOpenDirAbsoluteError(ctx, err);

            return -1;
        }
    else
        std.fs.cwd().openIterableDir(filename_slice, .{}) catch |err| {
            handleOpenDirError(ctx, err);
            return -1;
        };

    var file_list = api.ObjList.bz_newList(ctx.vm, api.ObjTypeDef.bz_stringType());

    ctx.vm.bz_push(file_list);

    var it = dir.iterate();
    while (it.next() catch |err| {
        _ = ctx.vm.bz_pop(); // Pop list
        handleDirIterateError(ctx, err);

        return -1;
    }) |element| {
        ctx.vm.bz_pushString(api.ObjString.bz_string(
            ctx.vm,
            if (element.name.len > 0) @ptrCast([*]const u8, element.name) else null,
            element.name.len,
        ) orelse {
            _ = ctx.vm.bz_pop(); // Pop list
            ctx.vm.pushError("lib.errors.OutOfMemoryError");

            return -1;
        });

        api.ObjList.bz_listAppend(ctx.vm, file_list, ctx.vm.bz_pop());
    }

    return 1;
}
