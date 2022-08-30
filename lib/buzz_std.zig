const std = @import("std");
const api = @import("buzz_api.zig");

export fn print(vm: *api.VM) c_int {
    var len: usize = 0;
    const string = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        return 0;
    }

    _ = std.io.getStdOut().write(string.?[0..len]) catch {
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
    var len: usize = 0;
    const string = vm.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        vm.bz_pushNull();

        return 1;
    }

    const string_slice = string.?[0..len];

    const is_float = std.mem.indexOf(u8, string_slice, ".") != null;

    if (is_float) {
        const number: f64 = std.fmt.parseFloat(f64, string_slice) catch {
            vm.bz_pushNull();

            return 1;
        };

        vm.bz_pushFloat(number);
    } else {
        const number: i64 = std.fmt.parseInt(i64, string_slice, 10) catch {
            vm.bz_pushNull();

            return 1;
        };

        vm.bz_pushInteger(number);
    }

    return 1;
}

export fn runFile(self: *api.VM) c_int {
    // Read file
    var len: usize = 0;
    const filename_string = self.bz_peek(0).bz_valueToString(&len);

    if (len == 0) {
        self.bz_throwString("Filename is empty", "Filename is empty".len);

        return -1;
    }

    const filename: []const u8 = filename_string.?[0..len];
    const filename_slice: []const u8 = std.mem.sliceTo(filename, 0);

    var file: std.fs.File = (if (std.fs.path.isAbsolute(filename_slice))
        std.fs.openFileAbsolute(filename_slice, .{})
    else
        std.fs.cwd().openFile(filename_slice, .{})) catch {
        self.bz_throwString("Could not open file", "Could not open file".len);

        return -1;
    };
    defer file.close();

    const source = api.VM.allocator.alloc(u8, (file.stat() catch {
        self.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    }).size) catch {
        self.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };

    _ = file.readAll(source) catch {
        self.bz_throwString("Could not read file", "Could not read file".len);

        return -1;
    };
    defer api.VM.allocator.free(source);

    // Init new VM
    var vm = self.bz_newVM();
    defer vm.bz_deinitVM();

    // Compile
    var function = vm.bz_compile(
        if (source.len > 0) @ptrCast([*]const u8, source) else null,
        source.len,
        if (filename.len > 0) @ptrCast([*]const u8, filename) else null,
        filename.len,
    ) orelse {
        self.bz_throwString("File does not compile", "File does not compile".len);

        return -1;
    };

    // Run
    if (!vm.bz_interpret(function)) {
        self.bz_throwString("Error while running file", "Error while running file".len);

        return -1;
    }

    return 0;
}
