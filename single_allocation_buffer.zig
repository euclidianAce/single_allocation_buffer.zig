// single_allocation_buffer.zig - Effectively just a slice with some methods to resize with a given (or stored) allocator
//
// 2023 - Corey Williamson <euclidianAce@protonmail.com>
//
// To the extent possible under law, the author(s) have dedicated all copyright
// and related and neighboring rights to this software to the public domain
// worldwide. This software is distributed without any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication along
// with this software. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

pub const Unmanaged = struct {
    slice: ?[]u8 = null,

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        if (self.slice) |s|
            allocator.free(s);
        self.* = .{};
    }

    // begin std.mem.Allocator functions

    pub fn alignedAlloc(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u29,
        len: usize,
    ) Allocator.Error![]align(alignment orelse @alignOf(T)) T {
        var ptr = try self.ensureAlignedSubslice(allocator, @sizeOf(T) * len, alignment orelse @alignOf(T));
        return @as(
            [*]align(alignment orelse @alignOf(T)) T,
            @ptrCast(@alignCast(ptr)),
        )[0..len];
    }

    pub fn alloc(self: *@This(), allocator: Allocator, comptime T: type, len: usize) Allocator.Error![]T {
        return self.alignedAlloc(allocator, T, null, len);
    }

    pub fn allocSentinel(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        len: usize,
        comptime sentinel: T,
    ) Allocator.Error![:sentinel]T {
        var slice = try self.alloc(allocator, T, len + 1);
        slice[len] = sentinel;
        return slice[0..len :sentinel];
    }

    pub fn create(self: *@This(), allocator: Allocator, comptime T: type) Allocator.Error!*T {
        var s = try self.alloc(allocator, T, 1);
        return &s[0];
    }

    pub fn dupe(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) Allocator.Error![]T {
        var dest = try self.alloc(allocator, T, src.len);
        @memcpy(dest, src);
        return dest;
    }

    pub fn dupeZ(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) ![:0]T {
        const new_buf = try self.alloc(allocator, T, src.len + 1);
        @memcpy(new_buf[0..src.len], src);
        new_buf[src.len] = 0;
        return new_buf[0..src.len :0];
    }

    // end std.mem.Allocator functions

    pub fn allocBatch(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        lengths: LengthStruct(T, .ignore),
    ) Allocator.Error!T {
        return self.allocBatchAdvanced(allocator, T, .ignore, lengths);
    }

    pub fn allocBatchStrict(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        lengths: LengthStruct(T, .@"error"),
    ) Allocator.Error!T {
        return self.allocBatchAdvanced(allocator, T, .@"error", lengths);
    }

    pub fn allocBatchAdvanced(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        comptime non_pointer_field_behavior: NonPointerFieldBehavior,
        lengths: LengthStruct(T, non_pointer_field_behavior),
    ) Allocator.Error!T {
        // LengthStruct will already assert that T is a struct
        const fields = @typeInfo(T).Struct.fields;
        const Field = struct {
            index: usize,
            alignment: usize,
            size: usize,
        };

        const sorted_fields: [fields.len]Field = comptime blk: {
            var sorted_fields: [fields.len]Field = undefined;
            for (&sorted_fields, fields, 0..) |*s_field, field, index|
                s_field.* = .{
                    .index = index,
                    .alignment = if (@sizeOf(field.type) == 0) 1 else @alignOf(field.type),
                    .size = @sizeOf(field.type),
                };
            std.mem.sort(
                Field,
                &sorted_fields,
                {},
                struct {
                    pub fn lt(_: void, lhs: Field, rhs: Field) bool {
                        if (lhs.alignment == rhs.alignment)
                            return lhs.index < rhs.index;
                        return lhs.alignment < rhs.alignment;
                    }
                }.lt,
            );
            break :blk sorted_fields;
        };

        var total_size: usize = 0;
        const alignment = sorted_fields[0].alignment;
        var offsets: [fields.len]usize = undefined;

        inline for (sorted_fields) |field| {
            const name = fields[field.index].name;
            const element_count = if (@hasField(@TypeOf(lengths), name)) @field(lengths, name) else 1;

            // first align up the size to the alignment of the current field
            total_size = std.mem.alignForward(usize, total_size, field.alignment);

            // save the offset into the block of memory
            offsets[field.index] = total_size;

            // then add the length of all the elements
            total_size += field.size * element_count;
        }

        var bytes_ptr = try self.ensureAlignedSubslice(allocator, total_size, alignment);

        var result: T = undefined;
        inline for (offsets, fields) |offset, field| {
            const Child = @typeInfo(field.type).Pointer.child;
            var field_ptr = @as([*]align(field.alignment) Child, @ptrCast(@alignCast(bytes_ptr + offset)));
            @field(result, field.name) = switch (@typeInfo(field.type).Pointer.size) {
                .One => &field_ptr[0],
                .Many, .C => field_ptr,
                .Slice => field_ptr[0..@field(lengths, field.name)],
            };
        }
        return result;
    }

    // Resizes or reallocates the slice so there exists a subslice of the given
    // length that is aligned to the given alignment.
    //
    // May invalidate any data within the slice due to reallocation.
    fn ensureAlignedSubslice(
        self: *@This(),
        allocator: Allocator,
        needed_byte_len: usize,
        needed_alignment: u29,
    ) Allocator.Error![*]u8 {
        std.debug.assert(needed_alignment > 0);
        const len_to_alloc = std.math.add(usize, needed_byte_len, needed_alignment - 1) catch return Allocator.Error.OutOfMemory;

        if (self.slice) |slice| {
            slice_can_be_aligned: {
                const offset = std.mem.alignPointerOffset(slice.ptr, needed_alignment) orelse break :slice_can_be_aligned;
                if (slice.len - offset >= needed_byte_len) {
                    return self.getAlignedSubslice(needed_byte_len, needed_alignment);
                }
            }

            // Length is too short (either because we are aligned and don't have enough
            // space, or there isn't enough space to get an aligned subslice).
            //
            // So our slice needs to be resized or reallocated

            // Since we don't care about the data in the slice we manually call
            // `resize`, `free`, and `alloc` rather than `realloc` for two reasons
            //    1. Avoid an unneeded memcpy, and
            //    2. Possibly allow the underlying allocator to reuse memory,
            //       since we called `free` first. (Which `realloc` is not able to
            //       do since it needs to memcpy)
            if (allocator.resize(slice, len_to_alloc)) {
                self.slice.?.len = len_to_alloc;
            } else {
                allocator.free(slice);
                errdefer self.slice = null;
                self.slice = try allocator.alloc(u8, len_to_alloc);
            }
        } else {
            // slice is null and needs to be allocated in the first place
            self.slice = try allocator.alloc(u8, len_to_alloc);
        }

        return self.getAlignedSubslice(needed_byte_len, needed_alignment);
    }

    // asserts that an aligned subslice exists
    fn getAlignedSubslice(
        self: @This(),
        needed_byte_len: usize,
        needed_alignment: u29,
    ) [*]u8 {
        const slice = self.slice.?;
        const offset = std.mem.alignPointerOffset(slice.ptr, needed_alignment).?;
        std.debug.assert(slice.len - offset >= needed_byte_len);
        return slice.ptr + offset;
    }

    test "allocBatch" {
        var buf = @This(){};
        defer buf.deinit(std.testing.allocator);

        const MyStruct = struct { a: i16, b: u8 };

        const result = try buf.allocBatch(
            std.testing.allocator,
            struct {
                x: []usize,
                b: *u8,
                s: []MyStruct,
            },
            .{
                .x = 10,
                .s = 12,
            },
        );

        try std.testing.expectEqual(
            @as(usize, 10),
            result.x.len,
        );

        try std.testing.expectEqual(
            @as(usize, 12),
            result.s.len,
        );

        // shouldnt clobber each other
        @memset(result.s, .{ .a = 100, .b = 7 });
        @memset(result.x, std.math.maxInt(usize));
        result.b.* = 32;

        for (result.s) |s| {
            try std.testing.expectEqual(
                @as(@TypeOf(s.a), 100),
                s.a,
            );
            try std.testing.expectEqual(
                @as(@TypeOf(s.b), 7),
                s.b,
            );
        }

        for (result.x) |x|
            try std.testing.expectEqual(@as(usize, std.math.maxInt(usize)), x);

        try std.testing.expectEqual(@as(u8, 32), result.b.*);
    }

    test "basic (unaligned byte) allocations" {
        var buf = @This(){};
        defer buf.deinit(std.testing.allocator);

        var slice = try buf.alloc(std.testing.allocator, u8, 123);
        @memset(slice, 0xfe);

        var other_slice = try buf.alloc(std.testing.allocator, u8, 32);
        for (other_slice) |item|
            try std.testing.expectEqual(@as(u8, 0xfe), item);
    }

    test "basic (aligned) allocations" {
        var buf = @This(){};
        defer buf.deinit(std.testing.allocator);

        inline for (0..8) |alignment_exp| {
            const alignment = 1 << alignment_exp;
            var slice = try buf.alignedAlloc(std.testing.allocator, u8, alignment, 4);
            try std.testing.expect(std.mem.isAligned(@intFromPtr(slice.ptr), alignment));
        }
    }
};

pub const Managed = struct {
    unmanaged: Unmanaged = .{},
    allocator: Allocator,

    pub fn init(allocator: Allocator) @This() {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *@This()) void {
        self.unmanaged.deinit(self.allocator);
    }

    pub fn alignedAlloc(
        self: *@This(),
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u29,
        len: usize,
    ) Allocator.Error![]align(alignment orelse @alignOf(T)) T {
        return self.unmanaged.alignedAlloc(self.allocator, T, alignment, len);
    }

    pub fn alloc(self: *@This(), comptime T: type, len: usize) Allocator.Error![]T {
        return self.unmanaged.alloc(self.allocator, T, len);
    }

    pub fn allocSentinel(
        self: *@This(),
        comptime T: type,
        len: usize,
        comptime sentinel: T,
    ) Allocator.Error![:sentinel]T {
        return self.unmanaged.allocSentinel(self.allocator, T, len, sentinel);
    }

    pub fn create(self: *@This(), comptime T: type) Allocator.Error!*T {
        return self.unmanaged.create(self.allocator, T);
    }

    pub fn dupe(self: *@This(), comptime T: type, src: []const T) Allocator.Error![]T {
        return self.unmanaged.dupe(self.allocator, T, src);
    }

    pub fn dupeZ(self: *@This(), comptime T: type, src: []const T) ![:0]T {
        return self.unmanaged.dupeZ(self.allocator, T, src);
    }

    pub fn allocBatch(
        self: *@This(),
        comptime T: type,
        lengths: LengthStruct(T, .ignore),
    ) Allocator.Error!T {
        return self.unmanaged.allocBatch(self.allocator, T, lengths);
    }

    pub fn allocBatchStrict(
        self: *@This(),
        comptime T: type,
        lengths: LengthStruct(T, .@"error"),
    ) Allocator.Error!T {
        return self.unmanaged.allocBatchAdvanced(self.allocator, T, .@"error", lengths);
    }

    pub fn allocBatchAdvanced(
        self: *@This(),
        comptime T: type,
        comptime non_pointer_field_behavior: NonPointerFieldBehavior,
        lengths: LengthStruct(T, non_pointer_field_behavior),
    ) Allocator.Error!T {
        return self.unmanaged.allocBatchAdvanced(self.allocator, T, non_pointer_field_behavior, lengths);
    }

    // ensure we have the same methods
    comptime {
        const unmanaged_decls = @typeInfo(Unmanaged).Struct.decls;

        for (unmanaged_decls) |decl| {
            if (!@hasDecl(@This(), decl.name)) {
                @compileError("single_allocation_buffer.Managed is missing declaration " ++ decl.name);
            }
        }
    }
};

pub const NonPointerFieldBehavior = enum {
    ignore,
    @"error",
};

// Takes a struct type S and returns a struct of usize fields corresponding to
// each (non-single) pointer field of S
pub fn LengthStruct(
    comptime S: type,
    comptime non_pointer_field_behavior: NonPointerFieldBehavior,
) type {
    const info = switch (@typeInfo(S)) {
        .Struct => |info| info,
        else => @compileError("Expected a struct type, got " ++ @typeName(S)),
    };

    // for each field of S
    //  - If it isn't a pointer type, either error out or ignore it according
    //    to non_pointer_field_behavior. (Either way, control flow diverges from
    //    here)
    //  - If that pointer has a length, add a length field to the result struct
    var length_fields: [info.fields.len]std.builtin.Type.StructField = undefined;
    var i: usize = 0;
    for (info.fields) |base_field| {
        const field_info = switch (@typeInfo(base_field.type)) {
            .Pointer => |p_info| p_info,
            else => switch (non_pointer_field_behavior) {
                .ignore => continue,
                .@"error" => @compileError("Field " ++ base_field.name ++ " (of type " ++ @typeName(S) ++ ") is not a pointer type"),
            },
        };

        if (field_info.size != .One) {
            length_fields[i] = .{
                .name = base_field.name,
                .type = usize,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(usize),
            };
            i += 1;
        }
    }

    return @Type(.{ .Struct = .{
        .layout = .Auto,
        .fields = length_fields[0..i],
        .decls = &.{},
        .is_tuple = false,
    } });
}

const std = @import("std");
const Allocator = std.mem.Allocator;

test {
    _ = Unmanaged;
    _ = Managed;
}
