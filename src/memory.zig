const VM = @import("./vm.zig").VM;

pub fn allocate(vm: *VM, comptime T: type) !*T {
    vm.bytes_allocated += @sizeOf(T);

    if (vm.bytes_allocated > vm.next_gc) {
        collectGarbage(vm);
    }

    return try vm.allocator.create(T);
}

pub fn allocateMany(vm: *VM, comptime T: type, count: usize) ![]T {
    vm.bytes_allocated += @sizeOf(T);

    if (vm.bytes_allocated > vm.next_gc) {
        collectGarbage(vm);
    }

    return try vm.allocator.alloc(T, count);
}

pub fn free(vm: *VM, comptime T: type, pointer: *T) void {
    vm.bytes_allocated -= @sizeOf(T);

    if (vm.bytes_allocated > vm.next_gc) {
        collectGarbage(vm);
    }

    vm.allocator.destroy(pointer);
}

pub fn collectGarbage(vm: *VM) void {
    unreachable;
}