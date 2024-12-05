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
    buffer_lock: std.debug.SafetyLock = .{},

    pub fn deinit(self: *@This(), allocator: Allocator) void {
        self.buffer_lock.assertUnlocked();
        if (self.slice) |s|
            allocator.free(s);
        self.* = .{};
    }

    pub fn lockBuffer(self: *@This()) void {
        self.buffer_lock.lock();
    }
    pub fn unlockBuffer(self: *@This()) void {
        self.buffer_lock.unlock();
    }

    pub fn runtimeAlignedAlloc(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        alignment: ?u32,
        len: usize,
    ) Allocator.Error![]T {
        const result = try self.runtimeAlignedAllocTemporary(allocator, T, alignment, len);
        self.lockBuffer();
        return result;
    }

    // begin std.mem.Allocator functions

    pub fn alignedAlloc(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u32,
        len: usize,
    ) Allocator.Error![]align(alignment orelse @alignOf(T)) T {
        const result = try self.alignedAllocTemporary(allocator, T, alignment, len);
        self.lockBuffer();
        return result;
    }

    pub fn alloc(self: *@This(), allocator: Allocator, comptime T: type, len: usize) Allocator.Error![]T {
        const result = self.allocTemporary(allocator, T, len);
        self.lockBuffer();
        return result;
    }

    pub fn allocSentinel(self: *@This(), allocator: Allocator, comptime T: type, len: usize, comptime sentinel: T) Allocator.Error![:sentinel]T {
        const result = try self.allocSentinelTemporary(allocator, T, len, sentinel);
        self.lockBuffer();
        return result;
    }

    pub fn create(self: *@This(), allocator: Allocator, comptime T: type) Allocator.Error!*T {
        const result = try self.createTemporary(allocator, T);
        self.lockBuffer();
        return result;
    }

    pub fn dupe(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) Allocator.Error![]T {
        const result = try self.dupeTemporary(allocator, T, src);
        self.lockBuffer();
        return result;
    }

    pub fn dupeZ(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) Allocator.Error![:0]T {
        const result = try self.dupeZTemporary(allocator, T, src);
        self.lockBuffer();
        return result;
    }

    // end std.mem.Allocator functions

    // The *Temporary functions allocate without locking the buffer lock
    // Use these when functions expect buffers as arguments like big int arithmetic
    //
    // e.g.
    //
    //   big_int.sqrt(other, try buf.allocTempoary(Limb, calcSqrtLimbsBufferLen(other.bitCountAbs())))
    //

    pub fn runtimeAlignedAllocTemporary(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        alignment: ?u32,
        len: usize,
    ) Allocator.Error![]T {
        const slice = try self.ensureAlignedSubslice(allocator, @sizeOf(T) * len, alignment orelse @alignOf(T));
        return @as([*]T, @ptrCast(@alignCast(slice)))[0..len];
    }

    pub fn alignedAllocTemporary(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u32,
        len: usize,
    ) Allocator.Error![]align(alignment orelse @alignOf(T)) T {
        return @alignCast(try self.runtimeAlignedAllocTemporary(allocator, T, alignment, len));
    }

    pub fn allocTemporary(self: *@This(), allocator: Allocator, comptime T: type, len: usize) Allocator.Error![]T {
        return self.alignedAllocTemporary(allocator, T, null, len);
    }

    pub fn allocSentinelTemporary(
        self: *@This(),
        allocator: Allocator,
        comptime T: type,
        len: usize,
        comptime sentinel: T,
    ) Allocator.Error![:sentinel]T {
        var slice = try self.allocTemporary(allocator, T, len + 1);
        slice[len] = sentinel;
        return slice[0..len :sentinel];
    }

    pub fn createTemporary(self: *@This(), allocator: Allocator, comptime T: type) Allocator.Error!*T {
        var s = try self.allocTemporary(allocator, T, 1);
        return &s[0];
    }

    pub fn dupeTemporary(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) Allocator.Error![]T {
        const dest = try self.allocTemporary(allocator, T, src.len);
        @memcpy(dest, src);
        return dest;
    }

    pub fn dupeZTemporary(self: *@This(), allocator: Allocator, comptime T: type, src: []const T) Allocator.Error![:0]T {
        const new_buf = try self.allocTemporary(allocator, T, src.len + 1);
        @memcpy(new_buf[0..src.len], src);
        new_buf[src.len] = 0;
        return new_buf[0..src.len :0];
    }

    pub fn print(
        self: *@This(),
        allocator: Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![]u8 {
        const count = std.fmt.count(fmt, args);
        const buf = try self.alloc(allocator, u8, count);
        return std.fmt.bufPrint(buf, fmt, args) catch unreachable;
    }

    pub fn printTemporary(
        self: *@This(),
        allocator: Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![]u8 {
        const count = std.fmt.count(fmt, args);
        const buf = try self.allocTemporary(allocator, u8, count);
        return std.fmt.bufPrint(buf, fmt, args) catch unreachable;
    }

    pub fn printZ(
        self: *@This(),
        allocator: Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![:0]u8 {
        const count = std.fmt.count(fmt, args);
        const buf = try self.alloc(allocator, u8, count + 1);
        return std.fmt.bufPrintZ(buf, fmt, args) catch unreachable;
    }

    pub fn printZTemporary(
        self: *@This(),
        allocator: Allocator,
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![:0]u8 {
        const count = std.fmt.count(fmt, args);
        const buf = try self.allocTemporary(allocator, u8, count + 1);
        return std.fmt.bufPrintZ(buf, fmt, args) catch unreachable;
    }

    // Resizes or reallocates the slice so there exists a subslice of the given
    // length that is aligned to the given alignment.
    //
    // May invalidate any data within the slice due to reallocation.
    fn ensureAlignedSubslice(
        self: *@This(),
        allocator: Allocator,
        needed_byte_len: usize,
        needed_alignment: u32,
    ) Allocator.Error![*]u8 {
        std.debug.assert(needed_alignment > 0);
        self.buffer_lock.assertUnlocked();

        const len_to_alloc = std.math.add(usize, needed_byte_len, needed_alignment - 1) catch return Allocator.Error.OutOfMemory;

        if (self.slice) |slice| {
            slice_can_be_aligned: {
                const offset = std.mem.alignPointerOffset(slice.ptr, needed_alignment) orelse break :slice_can_be_aligned;
                if (slice.len - offset >= needed_byte_len)
                    return self.getAlignedSubslice(needed_byte_len, needed_alignment).?;
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

        return self.getAlignedSubslice(needed_byte_len, needed_alignment).?;
    }

    pub fn getAlignedSubslice(
        self: @This(),
        needed_byte_len: usize,
        needed_alignment: u32,
    ) ?[*]u8 {
        const slice = self.slice.?;
        const offset = std.mem.alignPointerOffset(slice.ptr, needed_alignment).?;
        if (slice.len - offset < needed_byte_len)
            return null;
        return slice.ptr + offset;
    }

    comptime { // ensure every *Temporary has a corresponding non-temporary
        const decls = @typeInfo(Unmanaged).Struct.decls;
        for (decls) |decl| {
            if (std.mem.endsWith(u8, decl.name, "Temporary") and !@hasDecl(Unmanaged, decl.name[0 .. decl.name.len - "Temporary".len]))
                @compileError("Unmanaged is missing non-temporary version of " ++ decl.name);
        }
    }

    test "basic (unaligned byte) allocations" {
        var buf = @This(){};
        defer buf.deinit(std.testing.allocator);

        const slice = try buf.alloc(std.testing.allocator, u8, 123);
        @memset(slice, 0xfe);
        buf.unlockBuffer();

        const other_slice = try buf.alloc(std.testing.allocator, u8, 32);
        defer buf.unlockBuffer();
        for (other_slice) |item|
            try std.testing.expectEqual(@as(u8, 0xfe), item);
    }

    test "basic (aligned) allocations" {
        var buf = @This(){};
        defer buf.deinit(std.testing.allocator);

        inline for (0..8) |alignment_exp| {
            const alignment = 1 << alignment_exp;
            const slice = try buf.alignedAlloc(std.testing.allocator, u8, alignment, 4);
            defer buf.unlockBuffer();
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

    pub fn lockBuffer(self: *@This()) void {
        self.unmanaged.buffer_lock.lock();
    }
    pub fn unlockBuffer(self: *@This()) void {
        self.unmanaged.buffer_lock.unlock();
    }

    // Note: this is just to satisfy the standard allocator interface, you
    // should verify that the consumer of it only allocates in a way compatible
    // with this: i.e. owners of allocations should still call unlockBuffer
    // when they are done with allocations
    pub fn asUncheckedAllocator(self: *Managed) Allocator {
        const ops = struct {
            pub fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
                _ = ret_addr;
                const s: *Managed = @alignCast(@ptrCast(ctx));
                const slice = s.unmanaged.runtimeAlignedAlloc(
                    s.allocator,
                    u8,
                    @as(u32, 1) << @intCast(ptr_align),
                    len,
                ) catch return null;
                return slice.ptr;
            }

            pub fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, _: usize) bool {
                const s: *Managed = @alignCast(@ptrCast(ctx));
                const ptr = s.unmanaged.getAlignedSubslice(
                    new_len,
                    @as(u32, 1) << @intCast(buf_align),
                ) orelse return false;
                return ptr == buf.ptr;
            }

            pub fn free(ctx: *anyopaque, _: []u8, _: u8, _: usize) void {
                const s: *Managed = @alignCast(@ptrCast(ctx));
                s.unlockBuffer();
            }
        };

        const vtable: Allocator.VTable = .{
            .alloc = ops.alloc,
            .resize = ops.resize,
            .free = ops.free,
        };

        return .{ .ptr = self, .vtable = &vtable };
    }

    pub fn runtimeAlignedAlloc(self: *@This(), comptime T: type, alignment: ?u32, len: usize) Allocator.Error![]T {
        return self.unmanaged.runtimeAlignedAlloc(T, alignment, len);
    }

    pub fn alignedAlloc(
        self: *@This(),
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u32,
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

    pub fn dupeZ(self: *@This(), comptime T: type, src: []const T) Allocator.Error![:0]T {
        return self.unmanaged.dupeZ(self.allocator, T, src);
    }

    pub fn runtimeAlignedAllocTemporary(self: *@This(), comptime T: type, alignment: ?u32, len: usize) Allocator.Error![]T {
        return self.unmanaged.runtimeAlignedAllocTemporary(T, alignment, len);
    }

    pub fn alignedAllocTemporary(
        self: *@This(),
        comptime T: type,
        /// null means naturally aligned
        comptime alignment: ?u32,
        len: usize,
    ) Allocator.Error![]align(alignment orelse @alignOf(T)) T {
        return self.unmanaged.alignedAllocTemporary(self.allocator, T, alignment, len);
    }

    pub fn allocTemporary(self: *@This(), comptime T: type, len: usize) Allocator.Error![]T {
        return self.unmanaged.allocTemporary(self.allocator, T, len);
    }

    pub fn allocSentinelTemporary(
        self: *@This(),
        comptime T: type,
        len: usize,
        comptime sentinel: T,
    ) Allocator.Error![:sentinel]T {
        return self.unmanaged.allocSentinelTemporary(self.allocator, T, len, sentinel);
    }

    pub fn createTemporary(self: *@This(), comptime T: type) Allocator.Error!*T {
        return self.unmanaged.createTemporary(self.allocator, T);
    }

    pub fn dupeTemporary(self: *@This(), comptime T: type, src: []const T) Allocator.Error![]T {
        return self.unmanaged.dupeTemporary(self.allocator, T, src);
    }

    pub fn dupeZTemporary(self: *@This(), comptime T: type, src: []const T) Allocator.Error![:0]T {
        return self.unmanaged.dupeZTemporary(self.allocator, T, src);
    }

    pub fn getAlignedSubslice(
        self: @This(),
        needed_byte_len: usize,
        needed_alignment: u32,
    ) ?[*]u8 {
        return self.unmanaged.getAlignedSubslice(needed_byte_len, needed_alignment);
    }

    pub fn print(
        self: *@This(),
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![]u8 {
        return self.unmanaged.print(self.allocator, fmt, args);
    }

    pub fn printZ(
        self: *@This(),
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![:0]u8 {
        return self.unmanaged.printZ(self.allocator, fmt, args);
    }

    pub fn printTemporary(
        self: *@This(),
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![]u8 {
        return self.unmanaged.printTemporary(self.allocator, fmt, args);
    }

    pub fn printZTemporary(
        self: *@This(),
        comptime fmt: []const u8,
        args: anytype,
    ) Allocator.Error![:0]u8 {
        return self.unmanaged.printZTemporary(self.allocator, fmt, args);
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

    test "usage as standard allocator" {
        var buf = Managed.init(std.testing.allocator);
        defer buf.deinit();

        const ator = buf.asUncheckedAllocator();
        _ = try ator.alloc(u8, 100);
        buf.unlockBuffer();

        _ = try ator.alloc(u8, 100);
        buf.unlockBuffer();
    }
};

const std = @import("std");
const Allocator = std.mem.Allocator;

test {
    _ = Unmanaged;
    _ = Managed;

    std.testing.refAllDecls(Unmanaged);
    std.testing.refAllDecls(Managed);
}
