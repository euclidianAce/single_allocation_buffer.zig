# `single_allocation_buffer.zig`

It's basically just a slice with some convenience methods around resizing and reallocating it.

Comes with a `Managed` and `Unmanaged` variant.

## Use cases

I mostly use this inside of intern tables for things like big integers and strings where intermediate operations (arithmetic, concatenation, etc.) need temporary buffers

## Example

```zig
// Some silly big integer arithmetic
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var sab = single_allocation_buffer.Managed.init(gpa.allocator());
    defer sab.deinit();

    var limbs: [32]big_math.Limb = undefined;
    var a = BigIntMut.init(limbs[0..16], 12345_67890_12345_67890);
    const b = BigIntMut.init(limbs[17..], 67890_12345_67890_12345).toConst();

    a.mul(
        a.toConst(),
        b,
        try sab.alloc(big_math.Limb, big_math.int.calcMulLimbsBufferLen(a.len, b.limbs.len, 2)),
        null,
    );

    a.sqrt(
        a.toConst(),
        try sab.alloc(big_math.Limb, big_math.int.calcSqrtLimbsBufferLen(a.toConst().bitCountAbs())),
    );

    std.log.info("{}", .{a.toConst()});
}

const std = @import("std");
const big_math = std.math.big;
const BigInt = big_math.int.Const;
const BigIntMut = big_math.int.Mutable;
const single_allocation_buffer = @import("single_allocation_buffer.zig");
```

## Licensing

Everything in this repository is released to the public domain under the [Creative Commons Zero v1.0 Universal License](https://creativecommons.org/publicdomain/zero/1.0/)
