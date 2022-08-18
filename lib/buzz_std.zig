const std = @import("std");
const api = @import("buzz_api.zig");

export fn print(vm: *api.VM) c_int {
    _ = std.io.getStdOut().write(std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0)) catch {
        // TODO: throw something?
        return -1;
    };
    _ = std.io.getStdOut().write("\n") catch {
        // TODO: throw something?
        return -1;
    };

    return 0;
}

export fn parseNumber(vm: *api.VM) c_int {
    const string = std.mem.sliceTo(vm.bz_peek(0).bz_valueToString().?, 0);
    const is_float = std.mem.indexOf(u8, string, ".") != null;

    if (is_float) {
        const number: f64 = std.fmt.parseFloat(f64, string) catch {
            vm.bz_pushNull();

            return 1;
        };

        vm.bz_pushFloat(number);
    } else {
        const number: i64 = std.fmt.parseInt(i64, string, 10) catch {
            vm.bz_pushNull();

            return 1;
        };

        vm.bz_pushInteger(number);
    }

    return 1;
}

export fn runFile(self: *api.VM) c_int {
    // Read file
    const filename: [*:0]const u8 = api.Value.bz_valueToString(self.bz_peek(0)) orelse {
        self.bz_throwString("Could not get filename");

        return -1;
    };
    const filename_slice: []const u8 = std.mem.sliceTo(filename, 0);

    var file: std.fs.File = (if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openFileAbsolute(filename_slice, .{})
    else
        std.fs.cwd().openFile(filename_slice, .{})) catch {
        self.bz_throwString("Could not open file");

        return -1;
    };
    defer file.close();

    const source = api.VM.allocator.alloc(u8, (file.stat() catch {
        self.bz_throwString("Could not read file");

        return -1;
    }).size) catch {
        self.bz_throwString("Could not read file");

        return -1;
    };

    _ = file.readAll(source) catch {
        self.bz_throwString("Could not read file");

        return -1;
    };

    var source_d: ?[]u8 = api.VM.allocator.dupeZ(u8, source) catch {
        self.bz_throwString("Could not read file");

        return -1;
    };

    if (source_d == null) {
        self.bz_throwString("Could not read file");

        return -1;
    }

    defer api.VM.allocator.free(source_d.?);

    var source_0 = @ptrCast([*:0]u8, source_d.?);

    defer api.VM.allocator.free(source);

    // Init new VM
    var vm = self.bz_newVM();
    defer vm.bz_deinitVM();

    // Compile
    var function = vm.bz_compile(source_0, filename) orelse {
        self.bz_throwString("File does not compile");

        return -1;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        self.bz_throwString("Error while running file");

        return -1;
    }

    return 0;
}
