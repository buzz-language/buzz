const std = @import("std");
const api = @import("buzz_api.zig");

fn handleMakeDirectoryError(ctx: *api.NativeCtx, err: std.Io.Dir.CreateDirError) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.DiskQuota,
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
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn makeDirectory(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];
    if (std.Io.Dir.path.isAbsolute(filename_slice)) {
        std.Io.Dir.createDirAbsolute(
            ctx.getIo(),
            filename_slice,
            .default_dir,
        ) catch |err| {
            handleMakeDirectoryError(ctx, err);

            return -1;
        };
    } else {
        std.Io.Dir.cwd().createDir(
            ctx.getIo(),
            filename_slice,
            .default_dir,
        ) catch |err| {
            handleMakeDirectoryError(ctx, err);

            return -1;
        };
    }

    return 0;
}

fn handleDeleteDirectoryError(ctx: *api.NativeCtx, err: std.Io.Dir.DeleteDirError) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.NameTooLong,
        error.NotDir,
        error.ReadOnlyFileSystem,
        error.SymLinkLoop,
        error.SystemResources,
        error.PermissionDenied,
        error.FileNotFound,
        error.FileBusy,
        error.Canceled,
        error.NetworkNotFound,
        error.FileSystem,
        error.DirNotEmpty,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

fn handleDeleteFileError(ctx: *api.NativeCtx, err: std.Io.Dir.DeleteFileError) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.NameTooLong,
        error.ReadOnlyFileSystem,
        error.SymLinkLoop,
        error.SystemResources,
        error.PermissionDenied,
        error.FileNotFound,
        error.FileBusy,
        error.Canceled,
        error.NetworkNotFound,
        error.FileSystem,
        error.NotDir,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),

        // Should have been handled before
        error.IsDir => unreachable,
    }
}

fn absolutePathExists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.accessAbsolute(io, path, .{}) catch |err| {
        return err != error.FileNotFound;
    };
    return true;
}

fn relativePathExists(io: std.Io, path: []const u8) bool {
    std.Io.Dir.cwd().access(io, path, .{}) catch |err| {
        return err != error.FileNotFound;
    };
    return true;
}

pub export fn deleteFile(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];
    if (std.Io.Dir.path.isAbsolute(filename_slice)) {
        std.Io.Dir.deleteFileAbsolute(ctx.getIo(), filename_slice) catch |err| {
            handleDeleteFileError(ctx, err);
            return -1;
        };
    } else {
        std.Io.Dir.cwd().deleteFile(ctx.getIo(), filename_slice) catch |err| {
            handleDeleteFileError(ctx, err);
            return -1;
        };
    }

    return 0;
}

pub export fn deleteDirectory(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);

    const filename_slice = filename.?[0..len];

    if (std.Io.Dir.path.isAbsolute(filename_slice)) {
        std.Io.Dir.deleteDirAbsolute(ctx.getIo(), filename_slice) catch |err| {
            handleDeleteDirectoryError(ctx, err);
            return -1;
        };
    } else {
        std.Io.Dir.cwd().deleteDir(ctx.getIo(), filename_slice) catch |err| {
            handleDeleteDirectoryError(ctx, err);
            return -1;
        };
    }

    return 0;
}

fn handleMoveError(ctx: *api.NativeCtx, err: std.Io.Dir.RenameError) void {
    switch (err) {
        error.AccessDenied,
        error.AntivirusInterference,
        error.BadPathName,
        error.DiskQuota,
        error.FileBusy,
        error.IsDir,
        error.LinkQuotaExceeded,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PipeBusy,
        error.ReadOnlyFileSystem,
        error.SymLinkLoop,
        error.SystemResources,
        error.FileNotFound,
        error.NetworkNotFound,
        error.CrossDevice,
        error.DirNotEmpty,
        error.HardwareFailure,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

fn handleRealPathFileAllocError(ctx: *api.NativeCtx, err: std.Io.Dir.RealPathFileAllocError) void {
    switch (err) {
        error.AccessDenied,
        error.UnrecognizedVolume,
        error.AntivirusInterference,
        error.BadPathName,
        error.DeviceBusy,
        error.FileSystem,
        error.FileTooBig,
        error.FileBusy,
        error.InputOutput,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.NetworkNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled,
        error.OperationUnsupported,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        },
    }
}

fn handleRealPathError(ctx: *api.NativeCtx, err: std.Io.Dir.RealPathError) void {
    switch (err) {
        error.AccessDenied,
        error.UnrecognizedVolume,
        error.AntivirusInterference,
        error.DeviceBusy,
        error.FileSystem,
        error.FileTooBig,
        error.FileBusy,
        error.InputOutput,
        error.IsDir,
        error.NameTooLong,
        error.NoDevice,
        error.NoSpaceLeft,
        error.NotDir,
        error.PathAlreadyExists,
        error.PipeBusy,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.NetworkNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled,
        error.OperationUnsupported,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn move(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const source = ctx.vm.bz_peek(1).bz_valueToString(&len);
    const source_slice = source.?[0..len];

    const destination = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const destination_slice = destination.?[0..len];

    const source_is_absolute = std.fs.path.isAbsolute(source_slice);
    const destination_is_absolute = std.fs.path.isAbsolute(destination_slice);

    if (source_is_absolute and destination_is_absolute) {
        std.Io.Dir.renameAbsolute(
            source_slice,
            destination_slice,
            ctx.getIo(),
        ) catch |err| {
            handleMoveError(ctx, err);

            return -1;
        };
    } else if (!source_is_absolute and !destination_is_absolute) {
        std.Io.Dir.cwd().rename(
            source_slice,
            std.Io.Dir.cwd(),
            destination_slice,
            ctx.getIo(),
        ) catch |err| {
            handleMoveError(ctx, err);

            return -1;
        };
    } else {
        const source_absolute = if (source_is_absolute)
            source_slice
        else
            std.Io.Dir.cwd().realPathFileAlloc(
                ctx.getIo(),
                source_slice,
                api.VM.allocator,
            ) catch |err| {
                handleRealPathFileAllocError(ctx, err);

                return -1;
            };
        const destination_absolute = if (destination_is_absolute)
            destination_slice
        else
            std.Io.Dir.cwd().realPathFileAlloc(
                ctx.getIo(),
                destination_slice,
                api.VM.allocator,
            ) catch |err| {
                handleRealPathFileAllocError(ctx, err);

                return -1;
            };
        defer {
            if (!source_is_absolute) {
                api.VM.allocator.free(source_absolute);
            }

            if (!destination_is_absolute) {
                api.VM.allocator.free(destination_absolute);
            }
        }

        std.Io.Dir.renameAbsolute(
            source_absolute,
            destination_absolute,
            ctx.getIo(),
        ) catch |err| {
            handleMoveError(ctx, err);

            return -1;
        };
    }

    return 0;
}

fn handleOpenDirError(ctx: *api.NativeCtx, err: std.Io.Dir.OpenError) void {
    switch (err) {
        error.AccessDenied,
        error.BadPathName,
        error.NameTooLong,
        error.NoDevice,
        error.NotDir,
        error.ProcessFdQuotaExceeded,
        error.SymLinkLoop,
        error.SystemFdQuotaExceeded,
        error.SystemResources,
        error.FileNotFound,
        error.NetworkNotFound,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

fn handleDirIterateError(ctx: *api.NativeCtx, err: std.Io.Dir.Iterator.Error) void {
    switch (err) {
        error.AccessDenied,
        error.SystemResources,
        => ctx.vm.pushErrorEnum("errors.FileSystemError", @errorName(err)),

        error.PermissionDenied,
        => ctx.vm.pushErrorEnum("errors.ExecError", @errorName(err)),

        error.Canceled,
        => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),

        error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
    }
}

pub export fn list(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];
    const io = ctx.getIo();

    const dir = (if (std.fs.path.isAbsolute(filename_slice))
        std.Io.Dir.openDirAbsolute(
            io,
            filename_slice,
            .{
                .iterate = true,
            },
        )
    else
        std.Io.Dir.cwd().openDir(
            io,
            filename_slice,
            .{
                .iterate = true,
            },
        )) catch |err| {
        handleOpenDirError(ctx, err);
        return -1;
    };

    const file_list = ctx.vm.bz_newList(
        ctx.vm.bz_listType(ctx.vm.bz_stringType(), false),
    );

    ctx.vm.bz_push(file_list);

    var it = dir.iterate();
    while (it.next(io) catch |err| {
        _ = ctx.vm.bz_pop(); // Pop list
        handleDirIterateError(ctx, err);

        return -1;
    }) |element| {
        ctx.vm.bz_push(
            api.VM.bz_stringToValue(
                ctx.vm,
                if (element.name.len > 0) @as([*]const u8, @ptrCast(element.name)) else null,
                element.name.len,
            ),
        );

        file_list.bz_listAppend(ctx.vm.bz_pop(), ctx.vm);
    }

    return 1;
}

pub export fn exists(ctx: *api.NativeCtx) callconv(.c) c_int {
    var len: usize = 0;
    const filename = ctx.vm.bz_peek(0).bz_valueToString(&len);
    const filename_slice = filename.?[0..len];

    var accessed = true;

    if (std.fs.path.isAbsolute(filename_slice)) {
        std.Io.Dir.accessAbsolute(
            ctx.getIo(),
            filename_slice,
            .{ .read = true },
        ) catch {
            accessed = false;
        };
    } else {
        std.Io.Dir.cwd().access(
            ctx.getIo(),
            filename_slice,
            .{ .read = true },
        ) catch {
            accessed = false;
        };
    }

    ctx.vm.bz_push(api.Value.fromBoolean(accessed));

    return 1;
}
