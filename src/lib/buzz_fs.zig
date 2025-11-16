const std = @import("std");
const api = @import("buzz_api.zig");

fn handleMakeDirectoryError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.InvalidWtf8,
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
        error.NetworkNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

pub export fn makeDirectory(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

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
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        // Zig doesn't let me use those even though it lists them as being raised
        // error.FileNotFound => api.pushError(ctx.vm,"errors.FileNotFoundError"),
        // error.CannotDeleteRootDirectory => api.pushError(ctx.vm,"errors.CannotDeleteRootDirectoryError"),
        else => unreachable,
    }
}

pub export fn delete(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );

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
        error.InvalidWtf8,
        error.AccessDenied,
        error.AntivirusInterference,
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
        error.NetworkNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

fn handleRealpathError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.InvalidWtf8,
        error.AccessDenied,
        error.UnrecognizedVolume,
        error.AntivirusInterference,
        error.BadPathName,
        error.DeviceBusy,
        error.FileSystem,
        error.FileTooBig,
        error.InputOutput,
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
        error.FileNotFound,
        error.NetworkNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

pub export fn move(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const source = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 1),
        &len,
    );
    const source_slice = source.?[0..len];

    const destination = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );
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
        const source_absolute = if (source_is_absolute)
            source_slice
        else
            std.fs.cwd().realpathAlloc(api.VM.allocator, source_slice) catch |err| {
                handleRealpathError(ctx, err);

                return -1;
            };
        const destination_absolute = if (destination_is_absolute)
            destination_slice
        else
            std.fs.cwd().realpathAlloc(api.VM.allocator, destination_slice) catch |err| {
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
        error.InvalidWtf8,
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.DeviceBusy,
        error.FileBusy,
        error.FileLocksNotSupported,
        error.FileNotFound,
        error.FileTooBig,
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
        error.NetworkNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

fn handleOpenDirError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.InvalidWtf8,
        error.AccessDenied,
        error.BadPathName,
        error.DeviceBusy,
        error.InvalidUtf8,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.NetworkNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        error.ProcessNotFound,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

fn handleDirIterateError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.AccessDenied,
        error.SystemResources,
        error.InvalidUtf8,
        => api.pushErrorEnum(ctx.vm, "errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => api.pushErrorEnum(ctx.vm, "errors.ExecError", @errorName(err)),

        error.Unexpected => api.pushError(ctx.vm, "errors.UnexpectedError", null),
    }
}

pub export fn list(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );
    const filename_slice = filename.?[0..len];

    const dir = if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openDirAbsolute(
            filename_slice,
            .{
                .iterate = true,
            },
        ) catch |err| {
            handleOpenDirAbsoluteError(ctx, err);
            return -1;
        }
    else
        std.fs.cwd().openDir(
            filename_slice,
            .{
                .iterate = true,
            },
        ) catch |err| {
            handleOpenDirError(ctx, err);
            return -1;
        };

    const file_list = api.bz_newList(
        ctx.vm,
        api.bz_listType(
            ctx.vm,
            api.bz_stringType(
                ctx.vm,
            ),
            false,
        ),
    );

    api.bz_push(ctx.vm, file_list);

    var it = dir.iterate();
    while (it.next() catch |err| {
        _ = api.bz_pop(ctx.vm); // Pop list
        handleDirIterateError(ctx, err);

        return -1;
    }) |element| {
        api.bz_push(
            ctx.vm,
            api.bz_stringToValue(
                ctx.vm,
                if (element.name.len > 0) @as([*]const u8, @ptrCast(element.name)) else null,
                element.name.len,
            ),
        );

        api.bz_listAppend(ctx.vm, file_list, api.bz_pop(ctx.vm));
    }

    return 1;
}

pub export fn exists(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
        &len,
    );
    const filename_slice = filename.?[0..len];

    var accessed = true;

    if (std.fs.path.isAbsolute(filename_slice)) {
        std.fs.accessAbsolute(filename_slice, .{ .mode = .read_only }) catch {
            accessed = false;
        };
    } else {
        std.fs.cwd().access(filename_slice, .{ .mode = .read_only }) catch {
            accessed = false;
        };
    }

    api.bz_push(ctx.vm, api.Value.fromBoolean(accessed));

    return 1;
}
