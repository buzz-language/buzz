const std = @import("std");
const api = @import("buzz_api.zig");

fn handleThreadError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.ThreadQuotaExceeded,
        error.SystemResources,
        error.OutOfMemory,
        error.LockedMemoryLimitExceeded,
        error.Unexpected,
        => ctx.vm.pushErrorEnum("thread.ThreadError", @errorName(err)),
    }
}

fn spawn(
    current_vm: *api.VM,
    closure: *api.ObjClosure,
    arguments: ?[*]const api.Value,
    len: usize,
    catch_value: ?*const api.Value,
) void {
    const thread_vm = current_vm.bz_newVM() orelse @panic("Out of memory");
    defer thread_vm.bz_deinitVM();

    thread_vm.bz_startVM();

    thread_vm.bz_call(
        closure,
        arguments,
        len,
        catch_value,
    );
}

export fn ThreadSpawn(ctx: *api.NativeCtx) c_int {
    const closure = ctx.vm.bz_peek(2).bz_valueToClosure();
    const arguments = ctx.vm.bz_peek(1).bz_valueToObjList();
    var catch_value = ctx.vm.bz_peek(0);

    var thread = api.VM.allocator.create(std.Thread) catch @panic("Out of memory");
    errdefer api.VM.allocator.destroy(thread);

    thread.* = std.Thread.spawn(
        .{},
        spawn,
        .{
            ctx.vm,
            closure,
            arguments.bz_listPtr(),
            arguments.bz_listLen(),
            &catch_value,
        },
    ) catch |err| {
        handleThreadError(ctx, err);

        return -1;
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @intFromPtr(thread))) |userdata| {
        ctx.vm.bz_pushUserData(userdata);

        return 1;
    } else {
        @panic("Out of memory");
    }

    return 1;
}

export fn ThreadJoin(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const thread: *std.Thread = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    thread.join();

    return 0;
}

export fn ThreadDetach(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const thread: *std.Thread = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    thread.detach();

    return 0;
}

export fn ThreadCollect(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const thread: *std.Thread = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    api.VM.allocator.destroy(thread);

    return 0;
}

export fn SemaphoreInit(ctx: *api.NativeCtx) c_int {
    var semaphore = api.VM.allocator.create(std.Thread.Semaphore) catch @panic("Out of memory");
    errdefer api.VM.allocator.destroy(semaphore);

    semaphore.* = .{
        .permits = 1,
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @intFromPtr(semaphore))) |userdata| {
        ctx.vm.bz_pushUserData(userdata);

        return 1;
    } else {
        @panic("Out of memory");
    }

    return 1;
}

export fn SemaphoreWait(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const semaphore: *std.Thread.Semaphore = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    semaphore.wait();

    return 0;
}

export fn SemaphorePost(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const semaphore: *std.Thread.Semaphore = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    semaphore.post();

    return 0;
}

export fn SemaphoreCollect(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const semaphore: *std.Thread.Semaphore = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    api.VM.allocator.destroy(semaphore);

    return 0;
}

export fn initMutex(ctx: *api.NativeCtx) c_int {
    var mutex = api.VM.allocator.create(std.Thread.Mutex) catch @panic("Out of memory");
    errdefer api.VM.allocator.destroy(mutex);

    mutex.* = .{};

    // This is ugly but we can use a value type which would result in an obj on the heap otherwise the mutex itself will not be threadsafe
    ctx.vm.bz_push(api.Value.fromFloat(@bitCast(@intFromPtr(mutex))));

    return 1;
}

export fn lockMutex(ctx: *api.NativeCtx) c_int {
    const userdata: u64 = @bitCast(ctx.vm.bz_peek(0).float());
    const mutex: *std.Thread.Mutex = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    mutex.lock();

    return 0;
}

export fn tryLockMutex(ctx: *api.NativeCtx) c_int {
    const userdata: u64 = @bitCast(ctx.vm.bz_peek(0).float());
    const mutex: *std.Thread.Mutex = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    ctx.vm.bz_pushBool(mutex.tryLock());

    return 1;
}

export fn unlockMutex(ctx: *api.NativeCtx) c_int {
    const userdata: u64 = @bitCast(ctx.vm.bz_peek(0).float());
    const mutex: *std.Thread.Mutex = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    mutex.unlock();

    return 1;
}

export fn collectMutex(ctx: *api.NativeCtx) c_int {
    const userdata: u64 = @bitCast(ctx.vm.bz_peek(0).float());
    const mutex: *std.Thread.Mutex = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    api.VM.allocator.destroy(mutex);

    return 0;
}
