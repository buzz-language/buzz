const B = struct {
    value: u8,
};

const A = struct {
    const Self = @This();

    b: B,

    fn sideEffect(self: *Self) void {
        self.b.value += 1;
    }

    fn passByValue(self: *Self, shouldBeValue: B) void {
        //  b as expected value here
        self.sideEffect();
        // b is modified here
    }
};

var a = A{
    .b = B{
        .value = 1
    }
};

a.passByValue(a.b);