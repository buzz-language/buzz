const std = @import("std");
const api = @import("./buzz_api.zig");

fn handleMakeDirectoryError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.DiskQuota,
        error.InvalidUtf8,
        error.LinkQuotaExceeded,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.ReadOnlyFileSystem,
        error.SymLinkLoop,
        error.SystemResources,
        error.FileNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
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
        error.AccessDenied,
        error.BadPathName,
        error.InvalidUtf8,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
        error.ReadOnlyFileSystem,
        error.SymLinkLoop,
        error.SystemResources,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        // Zig doesn't let me use those even though it lists them as being raised
        // error.FileNotFound => ctx.vm.pushError("errors.FileNotFoundError"),
        // error.CannotDeleteRootDirectory => ctx.vm.pushError("errors.CannotDeleteRootDirectoryError"),
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
        error.AccessDenied,
        error.BadPathName,
        error.DiskQuota,
        error.FileBusy,
        error.InvalidUtf8,
        error.IsDir,
        error.LinkQuotaExceeded,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ReadOnlyFileSystem,
        error.RenameAcrossMountPoints,
        error.SharingViolation,
        error.SymLinkLoop,
        error.SystemResources,
        error.FileNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
    }
}

fn handleRealpathError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileSystem,
        error.FileTooBig,
        error.InputOutput,
        error.InvalidHandle,
        error.InvalidUtf8,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.NotSupported,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.SharingViolation,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.WouldBlock,
        error.FileNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
        error.OutOfMemory => ctx.vm.pushError("errors.OutOfMemoryError"),
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
        error.AccessDenied,
        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileNotFound,
        error.FileTooBig,
        error.InvalidHandle,
        error.InvalidUtf8,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.SharingViolation,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.WouldBlock,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
    }
}

fn handleOpenDirError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.DeviceBusy,
        error.InvalidHandle,
        error.InvalidUtf8,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
    }
}

fn handleDirIterateError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.SystemResources,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError"),
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
            if (element.name.len > 0) @as([*]const u8, @ptrCast(element.name)) else null,
            element.name.len,
        ) orelse {
            _ = ctx.vm.bz_pop(); // Pop list
            ctx.vm.pushError("errors.OutOfMemoryError");

            return -1;
        });

        api.ObjList.bz_listAppend(ctx.vm, file_list, ctx.vm.bz_pop());
    }

    return 1;
}
