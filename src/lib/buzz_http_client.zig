//! Local copy of Zig's HTTP(S) client implementation used by buzz.
//!
//! This exists because the Zig stdlib client currently mishandles HTTPS over an
//! explicit HTTP proxy after a successful CONNECT: it reuses the tunneled
//! socket as plain HTTP instead of starting TLS to the origin inside the
//! tunnel. That causes plaintext request bytes to be sent to port 443 and
//! surfaces as `HttpConnectionClosing` in buzz.
//!
//! Keep this file as close to upstream `std/http/Client.zig` as possible and
//! clearly comment any buzz-specific changes so it can be removed once the
//! upstream behavior is fixed.
const Client = @This();

const builtin = @import("builtin");

const std = @import("std");
const Io = std.Io;
const testing = std.testing;
const http = std.http;
const mem = std.mem;
const Uri = std.Uri;
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Writer = std.Io.Writer;
const Reader = std.Io.Reader;
const HostName = std.Io.net.HostName;

pub const disable_tls = std.options.http_disable_tls;

/// Used for all client allocations. Must be thread-safe.
allocator: Allocator,
/// Used for opening TCP connections.
io: Io,

ca_bundle_lock: if (disable_tls) void else Io.RwLock = if (disable_tls) {} else .init,
ca_bundle: if (disable_tls) void else std.crypto.Certificate.Bundle = if (disable_tls) {} else .empty,
/// Used both for the reader and writer buffers.
tls_buffer_size: if (disable_tls) u0 else usize = if (disable_tls) 0 else std.crypto.tls.Client.min_buffer_len,
/// If non-null, ssl secrets are logged to a stream. Creating such a stream
/// allows other processes with access to that stream to decrypt all
/// traffic over connections created with this `Client`.
ssl_key_log: ?*std.crypto.tls.Client.SslKeyLog = null,

/// The time used to decide whether certificates are expired.
///
/// When this is `null`, the next time this client performs an HTTPS request,
/// it will first check the time and rescan the system for root certificates.
now: ?Io.Timestamp = null,

/// The pool of connections that can be reused (and currently in use).
connection_pool: ConnectionPool = .{},
/// Each `Connection` allocates this amount for the reader buffer.
///
/// If the entire HTTP header cannot fit in this amount of bytes,
/// `error.HttpHeadersOversize` will be returned from `Request.wait`.
read_buffer_size: usize = 8192,
/// Each `Connection` allocates this amount for the writer buffer.
write_buffer_size: usize = 1024,

/// If populated, all http traffic travels through this third party.
/// This field cannot be modified while the client has active connections.
/// Pointer to externally-owned memory.
http_proxy: ?*Proxy = null,
/// If populated, all https traffic travels through this third party.
/// This field cannot be modified while the client has active connections.
/// Pointer to externally-owned memory.
https_proxy: ?*Proxy = null,

/// A Least-Recently-Used cache of open connections to be reused.
pub const ConnectionPool = struct {
    mutex: Io.Mutex = .init,
    /// Open connections that are currently in use.
    used: std.DoublyLinkedList = .{},
    /// Open connections that are not currently in use.
    free: std.DoublyLinkedList = .{},
    free_len: usize = 0,
    free_size: usize = 32,

    /// The criteria for a connection to be considered a match.
    pub const Criteria = struct {
        host: HostName,
        port: u16,
        protocol: Protocol,
    };

    /// Finds and acquires a connection from the connection pool matching the criteria.
    /// If no connection is found, null is returned.
    ///
    /// Threadsafe.
    pub fn findConnection(pool: *ConnectionPool, io: Io, criteria: Criteria) ?*Connection {
        pool.mutex.lockUncancelable(io);
        defer pool.mutex.unlock(io);

        var next = pool.free.last;
        while (next) |node| : (next = node.prev) {
            const connection: *Connection = @alignCast(@fieldParentPtr("pool_node", node));
            if (connection.protocol != criteria.protocol) continue;
            if (connection.port != criteria.port) continue;

            // Domain names are case-insensitive (RFC 5890, Section 2.3.2.4)
            if (!connection.host().eql(criteria.host)) continue;

            pool.acquireUnsafe(connection);
            return connection;
        }

        return null;
    }

    /// Acquires an existing connection from the connection pool. This function is not threadsafe.
    pub fn acquireUnsafe(pool: *ConnectionPool, connection: *Connection) void {
        pool.free.remove(&connection.pool_node);
        pool.free_len -= 1;

        pool.used.append(&connection.pool_node);
    }

    /// Acquires an existing connection from the connection pool. This function is threadsafe.
    pub fn acquire(pool: *ConnectionPool, io: Io, connection: *Connection) void {
        pool.mutex.lockUncancelable(io);
        defer pool.mutex.unlock(io);

        return pool.acquireUnsafe(connection);
    }

    /// Tries to release a connection back to the connection pool.
    /// If the connection is marked as closing, it will be closed instead.
    ///
    /// Threadsafe.
    pub fn release(pool: *ConnectionPool, connection: *Connection, io: Io) void {
        pool.mutex.lockUncancelable(io);
        defer pool.mutex.unlock(io);

        pool.used.remove(&connection.pool_node);

        if (connection.closing or pool.free_size == 0) return connection.destroy(io);

        if (pool.free_len >= pool.free_size) {
            const popped: *Connection = @alignCast(@fieldParentPtr("pool_node", pool.free.popFirst().?));
            pool.free_len -= 1;

            popped.destroy(io);
        }

        if (connection.proxied) {
            // proxied connections go to the end of the queue, always try direct connections first
            pool.free.prepend(&connection.pool_node);
        } else {
            pool.free.append(&connection.pool_node);
        }

        pool.free_len += 1;
    }

    /// Adds a newly created node to the pool of used connections. This function is threadsafe.
    pub fn addUsed(pool: *ConnectionPool, io: Io, connection: *Connection) void {
        pool.mutex.lockUncancelable(io);
        defer pool.mutex.unlock(io);

        pool.used.append(&connection.pool_node);
    }

    /// Resizes the connection pool.
    ///
    /// If the new size is smaller than the current size, then idle connections will be closed until the pool is the new size.
    ///
    /// Threadsafe.
    pub fn resize(pool: *ConnectionPool, io: Io, allocator: Allocator, new_size: usize) void {
        pool.mutex.lockUncancelable(io);
        defer pool.mutex.unlock(io);

        const next = pool.free.first;
        _ = next;
        while (pool.free_len > new_size) {
            const popped = pool.free.popFirst() orelse unreachable;
            pool.free_len -= 1;

            popped.data.close(allocator);
            allocator.destroy(popped);
        }

        pool.free_size = new_size;
    }

    /// Frees the connection pool and closes all connections within.
    ///
    /// All future operations on the connection pool will deadlock.
    ///
    /// Threadsafe.
    pub fn deinit(pool: *ConnectionPool, io: Io) void {
        pool.mutex.lockUncancelable(io);

        var next = pool.free.first;
        while (next) |node| {
            const connection: *Connection = @alignCast(@fieldParentPtr("pool_node", node));
            next = node.next;
            connection.destroy(io);
        }

        next = pool.used.first;
        while (next) |node| {
            const connection: *Connection = @alignCast(@fieldParentPtr("pool_node", node));
            next = node.next;
            connection.destroy(io);
        }

        pool.* = undefined;
    }
};

pub const Protocol = std.http.Client.Protocol;

pub const Connection = struct {
    client: *Client,
    stream_writer: Io.net.Stream.Writer,
    stream_reader: Io.net.Stream.Reader,
    /// Entry in `ConnectionPool.used` or `ConnectionPool.free`.
    pool_node: std.DoublyLinkedList.Node,
    port: u16,
    host_len: u8,
    proxied: bool,
    closing: bool,
    protocol: Protocol,

    const Plain = struct {
        connection: Connection,

        fn create(
            client: *Client,
            remote_host: HostName,
            port: u16,
            stream: Io.net.Stream,
        ) error{OutOfMemory}!*Plain {
            const io = client.io;
            const gpa = client.allocator;
            const alloc_len = allocLen(client, remote_host.bytes.len);
            const base = try gpa.alignedAlloc(u8, .of(Plain), alloc_len);
            errdefer gpa.free(base);
            const host_buffer = base[@sizeOf(Plain)..][0..remote_host.bytes.len];
            const socket_read_buffer = host_buffer.ptr[host_buffer.len..][0..client.read_buffer_size];
            const socket_write_buffer = socket_read_buffer.ptr[socket_read_buffer.len..][0..client.write_buffer_size];
            assert(base.ptr + alloc_len == socket_write_buffer.ptr + socket_write_buffer.len);
            @memcpy(host_buffer, remote_host.bytes);
            const plain: *Plain = @ptrCast(base);
            plain.* = .{
                .connection = .{
                    .client = client,
                    .stream_writer = stream.writer(io, socket_write_buffer),
                    .stream_reader = stream.reader(io, socket_read_buffer),
                    .pool_node = .{},
                    .port = port,
                    .host_len = @intCast(remote_host.bytes.len),
                    .proxied = false,
                    .closing = false,
                    .protocol = .plain,
                },
            };
            return plain;
        }

        fn destroy(plain: *Plain) void {
            const c = &plain.connection;
            const gpa = c.client.allocator;
            const base: [*]align(@alignOf(Plain)) u8 = @ptrCast(plain);
            gpa.free(base[0..allocLen(c.client, c.host_len)]);
        }

        fn allocLen(client: *Client, host_len: usize) usize {
            return @sizeOf(Plain) + host_len + client.read_buffer_size + client.write_buffer_size;
        }

        fn host(plain: *Plain) HostName {
            const base: [*]u8 = @ptrCast(plain);
            return .{ .bytes = base[@sizeOf(Plain)..][0..plain.connection.host_len] };
        }
    };

    const Tls = struct {
        client: std.crypto.tls.Client,
        connection: Connection,

        /// Asserts that `client.now` is non-null.
        fn create(
            client: *Client,
            remote_host: HostName,
            port: u16,
            stream: Io.net.Stream,
        ) !*Tls {
            const io = client.io;
            const gpa = client.allocator;
            const alloc_len = allocLen(client, remote_host.bytes.len);
            const base = try gpa.alignedAlloc(u8, .of(Tls), alloc_len);
            errdefer gpa.free(base);
            const host_buffer = base[@sizeOf(Tls)..][0..remote_host.bytes.len];
            // The TLS client wants enough buffer for the max encrypted frame
            // size, and the HTTP body reader wants enough buffer for the
            // entire HTTP header. This means we need a combined upper bound.
            const tls_read_buffer_len = client.tls_buffer_size + client.read_buffer_size;
            const tls_read_buffer = host_buffer.ptr[host_buffer.len..][0..tls_read_buffer_len];
            const tls_write_buffer = tls_read_buffer.ptr[tls_read_buffer.len..][0..client.tls_buffer_size];
            const socket_write_buffer = tls_write_buffer.ptr[tls_write_buffer.len..][0..client.write_buffer_size];
            const socket_read_buffer = socket_write_buffer.ptr[socket_write_buffer.len..][0..client.tls_buffer_size];
            assert(base.ptr + alloc_len == socket_read_buffer.ptr + socket_read_buffer.len);
            @memcpy(host_buffer, remote_host.bytes);
            const tls: *Tls = @ptrCast(base);
            var random_buffer: [std.crypto.tls.Client.Options.entropy_len]u8 = undefined;
            io.random(&random_buffer);
            tls.* = .{
                .connection = .{
                    .client = client,
                    .stream_writer = stream.writer(io, tls_write_buffer),
                    .stream_reader = stream.reader(io, socket_read_buffer),
                    .pool_node = .{},
                    .port = port,
                    .host_len = @intCast(remote_host.bytes.len),
                    .proxied = false,
                    .closing = false,
                    .protocol = .tls,
                },
                // TODO data race here on ca_bundle if the user sets `now` to null
                .client = std.crypto.tls.Client.init(
                    &tls.connection.stream_reader.interface,
                    &tls.connection.stream_writer.interface,
                    .{
                        .host = .{ .explicit = remote_host.bytes },
                        .ca = .{ .bundle = .{
                            .gpa = client.allocator,
                            .io = client.io,
                            .lock = &client.ca_bundle_lock,
                            .bundle = &client.ca_bundle,
                        } },
                        .ssl_key_log = client.ssl_key_log,
                        .read_buffer = tls_read_buffer,
                        .write_buffer = socket_write_buffer,
                        .entropy = &random_buffer,
                        .realtime_now = client.now.?,
                        // This is appropriate for HTTPS because the HTTP headers contain
                        // the content length which is used to detect truncation attacks.
                        .allow_truncation_attacks = true,
                    },
                ) catch |err| switch (err) {
                    error.WriteFailed => return tls.connection.stream_writer.err.?,
                    error.ReadFailed => return tls.connection.stream_reader.err.?,
                    else => |e| return e,
                },
            };
            return tls;
        }

        fn destroy(tls: *Tls) void {
            const c = &tls.connection;
            const gpa = c.client.allocator;
            const base: [*]align(@alignOf(Tls)) u8 = @ptrCast(tls);
            gpa.free(base[0..allocLen(c.client, c.host_len)]);
        }

        fn allocLen(client: *Client, host_len: usize) usize {
            const tls_read_buffer_len = client.tls_buffer_size + client.read_buffer_size;
            return @sizeOf(Tls) + host_len + tls_read_buffer_len + client.tls_buffer_size +
                client.write_buffer_size + client.tls_buffer_size;
        }

        fn host(tls: *Tls) HostName {
            const base: [*]u8 = @ptrCast(tls);
            return .{ .bytes = base[@sizeOf(Tls)..][0..tls.connection.host_len] };
        }
    };

    pub const ReadError = std.crypto.tls.Client.ReadError || Io.net.Stream.Reader.Error;

    pub fn getReadError(c: *const Connection) ?ReadError {
        return switch (c.protocol) {
            .tls => {
                if (disable_tls) unreachable;
                const tls: *const Tls = @alignCast(@fieldParentPtr("connection", c));
                return tls.client.read_err orelse c.stream_reader.err.?;
            },
            .plain => {
                return c.stream_reader.err.?;
            },
        };
    }

    fn getStream(c: *Connection) Io.net.Stream {
        return c.stream_reader.stream;
    }

    pub fn host(c: *Connection) HostName {
        return switch (c.protocol) {
            .tls => {
                if (disable_tls) unreachable;
                const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
                return tls.host();
            },
            .plain => {
                const plain: *Plain = @alignCast(@fieldParentPtr("connection", c));
                return plain.host();
            },
        };
    }

    /// If this is called without calling `flush` or `end`, data will be
    /// dropped unsent.
    pub fn destroy(c: *Connection, io: Io) void {
        c.stream_reader.stream.close(io);
        switch (c.protocol) {
            .tls => {
                if (disable_tls) unreachable;
                const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
                tls.destroy();
            },
            .plain => {
                const plain: *Plain = @alignCast(@fieldParentPtr("connection", c));
                plain.destroy();
            },
        }
    }

    /// HTTP protocol from client to server.
    /// This either goes directly to `stream_writer`, or to a TLS client.
    pub fn writer(c: *Connection) *Writer {
        return switch (c.protocol) {
            .tls => {
                if (disable_tls) unreachable;
                const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
                return &tls.client.writer;
            },
            .plain => &c.stream_writer.interface,
        };
    }

    /// HTTP protocol from server to client.
    /// This either comes directly from `stream_reader`, or from a TLS client.
    pub fn reader(c: *Connection) *Reader {
        return switch (c.protocol) {
            .tls => {
                if (disable_tls) unreachable;
                const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
                return &tls.client.reader;
            },
            .plain => &c.stream_reader.interface,
        };
    }

    pub fn flush(c: *Connection) Writer.Error!void {
        if (c.protocol == .tls) {
            if (disable_tls) unreachable;
            const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
            try tls.client.writer.flush();
        }
        try c.stream_writer.interface.flush();
    }

    /// If the connection is a TLS connection, sends the close_notify alert.
    ///
    /// Flushes all buffers.
    pub fn end(c: *Connection) Writer.Error!void {
        if (c.protocol == .tls) {
            if (disable_tls) unreachable;
            const tls: *Tls = @alignCast(@fieldParentPtr("connection", c));
            try tls.client.end();
        }
        try c.stream_writer.interface.flush();
    }
};

pub const Response = struct {
    request: *Request,
    head: Head,

    pub const Head = std.http.Client.Response.Head;

    /// If compressed body has been negotiated this will return compressed bytes.
    ///
    /// If the returned `Reader` returns `error.ReadFailed` the error is
    /// available via `bodyErr`.
    ///
    /// Asserts that this function is only called once.
    ///
    /// See also:
    /// * `readerDecompressing`
    pub fn reader(response: *Response, transfer_buffer: []u8) *Reader {
        const req = response.request;
        if (!req.method.responseHasBody()) return .ending;
        const head = &response.head;
        return req.reader.bodyReader(transfer_buffer, head.transfer_encoding, head.content_length);
    }

    /// If compressed body has been negotiated this will return decompressed bytes.
    ///
    /// If the returned `Reader` returns `error.ReadFailed` the error is
    /// available via `bodyErr`.
    ///
    /// Asserts that this function is only called once.
    ///
    /// See also:
    /// * `reader`
    pub fn readerDecompressing(
        response: *Response,
        transfer_buffer: []u8,
        decompress: *http.Decompress,
        decompress_buffer: []u8,
    ) *Reader {
        const head = &response.head;
        return response.request.reader.bodyReaderDecompressing(
            transfer_buffer,
            head.transfer_encoding,
            head.content_length,
            head.content_encoding,
            decompress,
            decompress_buffer,
        );
    }

    /// After receiving `error.ReadFailed` from the `Reader` returned by
    /// `reader` or `readerDecompressing`, this function accesses the
    /// more specific error code.
    pub fn bodyErr(response: *const Response) ?http.Reader.BodyError {
        return response.request.reader.body_err;
    }

    pub fn iterateTrailers(response: *const Response) http.HeaderIterator {
        const r = &response.request.reader;
        assert(r.state == .ready);
        return .{
            .bytes = r.trailers,
            .index = 0,
            .is_trailer = true,
        };
    }
};

pub const Request = struct {
    /// This field is provided so that clients can observe redirected URIs.
    ///
    /// Its backing memory is externally provided by API users when creating a
    /// request, and then again provided externally via `redirect_buffer` to
    /// `receiveHead`.
    uri: Uri,
    client: *Client,
    /// This is null when the connection is released.
    connection: ?*Connection,
    reader: http.Reader,
    keep_alive: bool,

    method: http.Method,
    version: http.Version = .@"HTTP/1.1",
    transfer_encoding: TransferEncoding,
    redirect_behavior: RedirectBehavior,
    accept_encoding: @TypeOf(default_accept_encoding) = default_accept_encoding,

    /// Whether the request should handle a 100-continue response before sending the request body.
    handle_continue: bool,

    /// Standard headers that have default, but overridable, behavior.
    headers: Headers,

    /// Populated in `receiveHead`; used in `deinit` to determine whether to
    /// discard the body to reuse the connection.
    response_content_length: ?u64 = null,
    /// Populated in `receiveHead`; used in `deinit` to determine whether to
    /// discard the body to reuse the connection.
    response_transfer_encoding: http.TransferEncoding = .none,

    /// These headers are kept including when following a redirect to a
    /// different domain.
    /// Externally-owned; must outlive the Request.
    extra_headers: []const http.Header,

    /// These headers are stripped when following a redirect to a different
    /// domain.
    /// Externally-owned; must outlive the Request.
    privileged_headers: []const http.Header,

    pub const default_accept_encoding: [@typeInfo(http.ContentEncoding).@"enum".fields.len]bool = b: {
        var result: [@typeInfo(http.ContentEncoding).@"enum".fields.len]bool = @splat(false);
        result[@intFromEnum(http.ContentEncoding.gzip)] = true;
        result[@intFromEnum(http.ContentEncoding.deflate)] = true;
        result[@intFromEnum(http.ContentEncoding.identity)] = true;
        break :b result;
    };

    pub const TransferEncoding = union(enum) {
        content_length: u64,
        chunked: void,
        none: void,
    };

    pub const Headers = struct {
        host: Value = .default,
        authorization: Value = .default,
        user_agent: Value = .default,
        connection: Value = .default,
        accept_encoding: Value = .default,
        content_type: Value = .default,

        pub const Value = union(enum) {
            default,
            omit,
            override: []const u8,
        };
    };

    /// Any value other than `not_allowed` or `unhandled` means that integer represents
    /// how many remaining redirects are allowed.
    pub const RedirectBehavior = enum(u16) {
        /// The next redirect will cause an error.
        not_allowed = 0,
        /// Redirects are passed to the client to analyze the redirect response
        /// directly.
        unhandled = std.math.maxInt(u16),
        _,

        pub fn init(n: u16) RedirectBehavior {
            assert(n != std.math.maxInt(u16));
            return @enumFromInt(n);
        }

        pub fn subtractOne(rb: *RedirectBehavior) void {
            switch (rb.*) {
                .not_allowed => unreachable,
                .unhandled => unreachable,
                _ => rb.* = @enumFromInt(@intFromEnum(rb.*) - 1),
            }
        }

        pub fn remaining(rb: RedirectBehavior) u16 {
            assert(rb != .unhandled);
            return @intFromEnum(rb);
        }
    };

    /// Returns the request's `Connection` back to the pool of the `Client`.
    pub fn deinit(r: *Request) void {
        const io = r.client.io;
        if (r.connection) |connection| {
            connection.closing = connection.closing or switch (r.reader.state) {
                .ready => false,
                .received_head => c: {
                    if (r.method.requestHasBody()) break :c true;
                    if (!r.method.responseHasBody()) break :c false;
                    const reader = r.reader.bodyReader(&.{}, r.response_transfer_encoding, r.response_content_length);
                    _ = reader.discardRemaining() catch |err| switch (err) {
                        error.ReadFailed => break :c true,
                    };
                    break :c r.reader.state != .ready;
                },
                else => true,
            };
            r.client.connection_pool.release(connection, io);
        }
        r.* = undefined;
    }

    /// Sends and flushes a complete request as only HTTP head, no body.
    pub fn sendBodiless(r: *Request) Writer.Error!void {
        try sendBodilessUnflushed(r);
        try r.connection.?.flush();
    }

    /// Sends but does not flush a complete request as only HTTP head, no body.
    pub fn sendBodilessUnflushed(r: *Request) Writer.Error!void {
        assert(r.transfer_encoding == .none);
        assert(!r.method.requestHasBody());
        try sendHead(r);
    }

    /// Transfers the HTTP head over the connection and flushes.
    ///
    /// See also:
    /// * `sendBodyUnflushed`
    pub fn sendBody(r: *Request, buffer: []u8) Writer.Error!http.BodyWriter {
        const result = try sendBodyUnflushed(r, buffer);
        try r.connection.?.flush();
        return result;
    }

    /// Transfers the HTTP head and body over the connection and flushes.
    pub fn sendBodyComplete(r: *Request, body: []u8) Writer.Error!void {
        r.transfer_encoding = .{ .content_length = body.len };
        var bw = try sendBodyUnflushed(r, body);
        bw.writer.end = body.len;
        try bw.end();
        try r.connection.?.flush();
    }

    /// Transfers the HTTP head over the connection, which is not flushed until
    /// `BodyWriter.flush` or `BodyWriter.end` is called.
    ///
    /// See also:
    /// * `sendBody`
    pub fn sendBodyUnflushed(r: *Request, buffer: []u8) Writer.Error!http.BodyWriter {
        assert(r.method.requestHasBody());
        try sendHead(r);
        const http_protocol_output = r.connection.?.writer();
        return switch (r.transfer_encoding) {
            .chunked => .{
                .http_protocol_output = http_protocol_output,
                .state = .init_chunked,
                .writer = .{
                    .buffer = buffer,
                    .vtable = &.{
                        .drain = http.BodyWriter.chunkedDrain,
                        .sendFile = http.BodyWriter.chunkedSendFile,
                    },
                },
            },
            .content_length => |len| .{
                .http_protocol_output = http_protocol_output,
                .state = .{ .content_length = len },
                .writer = .{
                    .buffer = buffer,
                    .vtable = &.{
                        .drain = http.BodyWriter.contentLengthDrain,
                        .sendFile = http.BodyWriter.contentLengthSendFile,
                    },
                },
            },
            .none => .{
                .http_protocol_output = http_protocol_output,
                .state = .none,
                .writer = .{
                    .buffer = buffer,
                    .vtable = &.{
                        .drain = http.BodyWriter.noneDrain,
                        .sendFile = http.BodyWriter.noneSendFile,
                    },
                },
            },
        };
    }

    /// Sends HTTP headers without flushing.
    fn sendHead(r: *Request) Writer.Error!void {
        const uri = r.uri;
        const connection = r.connection.?;
        const w = connection.writer();

        try w.writeAll(@tagName(r.method));
        try w.writeByte(' ');

        if (r.method == .CONNECT) {
            try uri.writeToStream(w, .{ .authority = true });
        } else {
            try uri.writeToStream(w, .{
                .scheme = connection.proxied,
                .authentication = connection.proxied,
                .authority = connection.proxied,
                .path = true,
                .query = true,
            });
        }
        try w.writeByte(' ');
        try w.writeAll(@tagName(r.version));
        try w.writeAll("\r\n");

        if (try emitOverridableHeader("host: ", r.headers.host, w)) {
            try w.writeAll("host: ");
            try uri.writeToStream(w, .{ .authority = true });
            try w.writeAll("\r\n");
        }

        if (try emitOverridableHeader("authorization: ", r.headers.authorization, w)) {
            if (uri.user != null or uri.password != null) {
                try w.writeAll("authorization: ");
                try basic_authorization.write(uri, w);
                try w.writeAll("\r\n");
            }
        }

        if (try emitOverridableHeader("user-agent: ", r.headers.user_agent, w)) {
            try w.writeAll("user-agent: zig/");
            try w.writeAll(builtin.zig_version_string);
            try w.writeAll(" (std.http)\r\n");
        }

        if (try emitOverridableHeader("connection: ", r.headers.connection, w)) {
            if (r.keep_alive) {
                try w.writeAll("connection: keep-alive\r\n");
            } else {
                try w.writeAll("connection: close\r\n");
            }
        }

        if (try emitOverridableHeader("accept-encoding: ", r.headers.accept_encoding, w)) {
            try w.writeAll("accept-encoding: ");
            for (r.accept_encoding, 0..) |enabled, i| {
                if (!enabled) continue;
                const tag: http.ContentEncoding = @enumFromInt(i);
                if (tag == .identity) continue;
                const tag_name = @tagName(tag);
                try w.ensureUnusedCapacity(tag_name.len + 2);
                try w.writeAll(tag_name);
                try w.writeAll(", ");
            }
            w.undo(2);
            try w.writeAll("\r\n");
        }

        switch (r.transfer_encoding) {
            .chunked => try w.writeAll("transfer-encoding: chunked\r\n"),
            .content_length => |len| try w.print("content-length: {d}\r\n", .{len}),
            .none => {},
        }

        if (try emitOverridableHeader("content-type: ", r.headers.content_type, w)) {
            // The default is to omit content-type if not provided because
            // "application/octet-stream" is redundant.
        }

        for (r.extra_headers) |header| {
            assert(header.name.len != 0);

            try w.writeAll(header.name);
            try w.writeAll(": ");
            try w.writeAll(header.value);
            try w.writeAll("\r\n");
        }

        if (connection.proxied) proxy: {
            const proxy = switch (connection.protocol) {
                .plain => r.client.http_proxy,
                .tls => r.client.https_proxy,
            } orelse break :proxy;

            const authorization = proxy.authorization orelse break :proxy;
            try w.writeAll("proxy-authorization: ");
            try w.writeAll(authorization);
            try w.writeAll("\r\n");
        }

        try w.writeAll("\r\n");
    }

    pub const ReceiveHeadError = http.Reader.HeadError || ConnectError || error{
        /// Server sent headers that did not conform to the HTTP protocol.
        ///
        /// To find out more detailed diagnostics, `http.Reader.head_buffer` can be
        /// passed directly to `Request.Head.parse`.
        HttpHeadersInvalid,
        TooManyHttpRedirects,
        /// This can be avoided by calling `receiveHead` before sending the
        /// request body.
        RedirectRequiresResend,
        HttpRedirectLocationMissing,
        HttpRedirectLocationOversize,
        HttpRedirectLocationInvalid,
        HttpContentEncodingUnsupported,
        HttpChunkInvalid,
        HttpChunkTruncated,
        HttpHeadersOversize,
        UnsupportedUriScheme,

        /// Sending the request failed. Error code can be found on the
        /// `Connection` object.
        WriteFailed,
    };

    /// If handling redirects and the request has no payload, then this
    /// function will automatically follow redirects.
    ///
    /// If a request payload is present, then this function will error with
    /// `error.RedirectRequiresResend`.
    ///
    /// This function takes an auxiliary buffer to store the arbitrarily large
    /// URI which may need to be merged with the previous URI, and that data
    /// needs to survive across different connections, which is where the input
    /// buffer lives.
    ///
    /// `redirect_buffer` must outlive accesses to `Request.uri`. If this
    /// buffer capacity would be exceeded, `error.HttpRedirectLocationOversize`
    /// is returned instead. This buffer may be empty if no redirects are to be
    /// handled. RFC 9110 recommends making this at least 8000 bytes.
    ///
    /// If this fails with `error.ReadFailed` then the `Connection.getReadError`
    /// method of `r.connection` can be used to get more detailed information.
    pub fn receiveHead(r: *Request, redirect_buffer: []u8) ReceiveHeadError!Response {
        var aux_buf = redirect_buffer;
        while (true) {
            // This while loop is for handling redirects, which means the request's
            // connection may be different than the previous iteration. However, it
            // is still guaranteed to be non-null with each iteration of this loop.
            const connection = r.connection.?;

            const head_buffer = r.reader.receiveHead() catch |err| {
                // Failure here means the connection can no longer be reused.
                connection.closing = true;
                return err;
            };
            const response: Response = .{
                .request = r,
                .head = Response.Head.parse(head_buffer) catch return error.HttpHeadersInvalid,
            };
            const head = &response.head;

            if (head.status == .@"continue") {
                if (r.handle_continue) continue;
                r.response_transfer_encoding = head.transfer_encoding;
                r.response_content_length = head.content_length;
                return response; // we're not handling the 100-continue
            }

            if (r.method == .CONNECT and head.status.class() == .success) {
                // This connection is no longer doing HTTP.
                connection.closing = false;
                r.response_transfer_encoding = head.transfer_encoding;
                r.response_content_length = head.content_length;
                return response;
            }

            connection.closing = !head.keep_alive or !r.keep_alive;

            // Any response to a HEAD request and any response with a 1xx
            // (Informational), 204 (No Content), or 304 (Not Modified) status
            // code is always terminated by the first empty line after the
            // header fields, regardless of the header fields present in the
            // message.
            if (r.method == .HEAD or head.status.class() == .informational or
                head.status == .no_content or head.status == .not_modified)
            {
                r.response_transfer_encoding = head.transfer_encoding;
                r.response_content_length = head.content_length;
                return response;
            }

            if (head.status.class() == .redirect and r.redirect_behavior != .unhandled) {
                if (r.redirect_behavior == .not_allowed) {
                    // Connection can still be reused by skipping the body.
                    const reader = r.reader.bodyReader(&.{}, head.transfer_encoding, head.content_length);
                    _ = reader.discardRemaining() catch |err| switch (err) {
                        error.ReadFailed => connection.closing = true,
                    };
                    return error.TooManyHttpRedirects;
                }
                try r.redirect(head, &aux_buf);
                try r.sendBodiless();
                continue;
            }

            if (!r.accept_encoding[@intFromEnum(head.content_encoding)])
                return error.HttpContentEncodingUnsupported;

            r.response_transfer_encoding = head.transfer_encoding;
            r.response_content_length = head.content_length;
            return response;
        }
    }

    /// This function takes an auxiliary buffer to store the arbitrarily large
    /// URI which may need to be merged with the previous URI, and that data
    /// needs to survive across different connections, which is where the input
    /// buffer lives.
    ///
    /// `aux_buf` must outlive accesses to `Request.uri`.
    fn redirect(r: *Request, head: *const Response.Head, aux_buf: *[]u8) !void {
        const io = r.client.io;
        const new_location = head.location orelse return error.HttpRedirectLocationMissing;
        if (new_location.len > aux_buf.*.len) return error.HttpRedirectLocationOversize;
        const location = aux_buf.*[0..new_location.len];
        @memcpy(location, new_location);
        {
            // Skip the body of the redirect response to leave the connection in
            // the correct state. This causes `new_location` to be invalidated.
            const reader = r.reader.bodyReader(&.{}, head.transfer_encoding, head.content_length);
            _ = reader.discardRemaining() catch |err| switch (err) {
                error.ReadFailed => return r.reader.body_err.?,
            };
        }
        const new_uri = r.uri.resolveInPlace(location.len, aux_buf) catch |err| switch (err) {
            error.UnexpectedCharacter => return error.HttpRedirectLocationInvalid,
            error.InvalidFormat => return error.HttpRedirectLocationInvalid,
            error.InvalidPort => return error.HttpRedirectLocationInvalid,
            error.InvalidHostName => return error.HttpRedirectLocationInvalid,
            error.NoSpaceLeft => return error.HttpRedirectLocationOversize,
        };

        const protocol = Protocol.fromUri(new_uri) orelse return error.UnsupportedUriScheme;
        const old_connection = r.connection.?;
        const old_host = old_connection.host();
        var new_host_name_buffer: [HostName.max_len]u8 = undefined;
        const new_host = try new_uri.getHost(&new_host_name_buffer);
        const keep_privileged_headers =
            std.ascii.eqlIgnoreCase(r.uri.scheme, new_uri.scheme) and
            old_host.sameParentDomain(new_host);

        r.client.connection_pool.release(old_connection, io);
        r.connection = null;

        if (!keep_privileged_headers) {
            // When redirecting to a different domain, strip privileged headers.
            r.privileged_headers = &.{};
        }

        if (switch (head.status) {
            .see_other => true,
            .moved_permanently, .found => r.method == .POST,
            else => false,
        }) {
            // A redirect to a GET must change the method and remove the body.
            r.method = .GET;
            r.transfer_encoding = .none;
            r.headers.content_type = .omit;
        }

        if (r.transfer_encoding != .none) {
            // The request body has already been sent. The request is
            // still in a valid state, but the redirect must be handled
            // manually.
            return error.RedirectRequiresResend;
        }

        const new_connection = try r.client.connect(new_host, uriPort(new_uri, protocol), protocol);
        r.uri = new_uri;
        r.connection = new_connection;
        r.reader = .{
            .in = new_connection.reader(),
            .state = .ready,
            // Populated when `http.Reader.bodyReader` is called.
            .interface = undefined,
            .max_head_len = r.client.read_buffer_size,
        };
        r.redirect_behavior.subtractOne();
    }

    /// Returns true if the default behavior is required, otherwise handles
    /// writing (or not writing) the header.
    fn emitOverridableHeader(prefix: []const u8, v: Headers.Value, bw: *Writer) Writer.Error!bool {
        switch (v) {
            .default => return true,
            .omit => return false,
            .override => |x| {
                var vecs: [3][]const u8 = .{ prefix, x, "\r\n" };
                try bw.writeVecAll(&vecs);
                return false;
            },
        }
    }
};

pub const Proxy = std.http.Client.Proxy;

/// Release all associated resources with the client.
///
/// All pending requests must be de-initialized and all active connections released
/// before calling this function.
pub fn deinit(client: *Client) void {
    const io = client.io;
    assert(client.connection_pool.used.first == null); // There are still active requests.

    client.connection_pool.deinit(io);
    if (!disable_tls) client.ca_bundle.deinit(client.allocator);

    client.* = undefined;
}

/// Populates `http_proxy` and `https_proxy` via standard proxy environment variables.
/// Asserts the client has no active connections.
/// Uses `arena` for a few small allocations that must outlive the client, or
/// at least until those fields are set to different values.
pub fn initDefaultProxies(client: *Client, arena: Allocator, environ_map: *const std.process.Environ.Map) !void {
    const io = client.io;

    // Prevent any new connections from being created.
    client.connection_pool.mutex.lockUncancelable(io);
    defer client.connection_pool.mutex.unlock(io);

    assert(client.connection_pool.used.first == null); // There are active requests.

    if (client.http_proxy == null) {
        client.http_proxy = try createProxyFromEnvVar(arena, environ_map, &.{
            "http_proxy", "HTTP_PROXY", "all_proxy", "ALL_PROXY",
        });
    }

    if (client.https_proxy == null) {
        client.https_proxy = try createProxyFromEnvVar(arena, environ_map, &.{
            "https_proxy", "HTTPS_PROXY", "all_proxy", "ALL_PROXY",
        });
    }
}

fn createProxyFromEnvVar(
    arena: Allocator,
    environ_map: *const std.process.Environ.Map,
    env_var_names: []const []const u8,
) !?*Proxy {
    const content = for (env_var_names) |name| {
        const content = environ_map.get(name) orelse continue;
        if (content.len == 0) continue;
        break content;
    } else return null;

    const uri = Uri.parse(content) catch try Uri.parseAfterScheme("http", content);
    const protocol = Protocol.fromUri(uri) orelse return null;
    const raw_host = try uri.getHostAlloc(arena);

    const authorization: ?[]const u8 = if (uri.user != null or uri.password != null) a: {
        const authorization = try arena.alloc(u8, basic_authorization.valueLengthFromUri(uri));
        assert(basic_authorization.value(uri, authorization).len == authorization.len);
        break :a authorization;
    } else null;

    const proxy = try arena.create(Proxy);
    proxy.* = .{
        .protocol = protocol,
        .host = raw_host,
        .authorization = authorization,
        .port = uriPort(uri, protocol),
        .supports_connect = true,
    };
    return proxy;
}

pub const basic_authorization = std.http.Client.basic_authorization;

pub const ConnectTcpError = error{
    TlsInitializationFailed,
} || Allocator.Error || HostName.ConnectError;

/// Reuses a `Connection` if one matching `host` and `port` is already open.
///
/// Threadsafe.
pub fn connectTcp(
    client: *Client,
    host: HostName,
    port: u16,
    protocol: Protocol,
) ConnectTcpError!*Connection {
    return connectTcpOptions(client, .{ .host = host, .port = port, .protocol = protocol });
}

pub const ConnectTcpOptions = struct {
    host: HostName,
    port: u16,
    protocol: Protocol,

    proxied_host: ?HostName = null,
    proxied_port: ?u16 = null,
    timeout: Io.Timeout = .none,
};

pub fn connectTcpOptions(client: *Client, options: ConnectTcpOptions) ConnectTcpError!*Connection {
    const io = client.io;
    const host = options.host;
    const port = options.port;
    const protocol = options.protocol;

    const proxied_host = options.proxied_host orelse host;
    const proxied_port = options.proxied_port orelse port;

    if (client.connection_pool.findConnection(io, .{
        .host = proxied_host,
        .port = proxied_port,
        .protocol = protocol,
    })) |conn| return conn;

    var stream = try host.connect(io, port, .{ .mode = .stream });
    errdefer stream.close(io);

    switch (protocol) {
        .tls => {
            if (disable_tls) return error.TlsInitializationFailed;
            const tc = Connection.Tls.create(client, proxied_host, proxied_port, stream) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                error.Unexpected => |e| return e,
                error.Canceled => |e| return e,
                else => return error.TlsInitializationFailed,
            };
            client.connection_pool.addUsed(io, &tc.connection);
            return &tc.connection;
        },
        .plain => {
            const pc = try Connection.Plain.create(client, proxied_host, proxied_port, stream);
            client.connection_pool.addUsed(io, &pc.connection);
            return &pc.connection;
        },
    }
}

/// Connect to `proxied_host:proxied_port` using the specified proxy with HTTP
/// CONNECT. This will reuse a connection if one is already open.
///
/// This function is threadsafe.
pub fn connectProxied(
    client: *Client,
    proxy: *Proxy,
    proxied_host: HostName,
    proxied_port: u16,
    target_protocol: Protocol,
) !*Connection {
    const io = client.io;
    if (!proxy.supports_connect) return error.TunnelNotSupported;

    if (target_protocol == .tls and proxy.protocol == .plain) {
        // buzz-specific: once CONNECT succeeds through a plain HTTP proxy, the
        // stream has to be upgraded to TLS for the origin host before any HTTP
        // request bytes are sent through the tunnel.
        if (client.connection_pool.findConnection(io, .{
            .host = proxied_host,
            .port = proxied_port,
            .protocol = .tls,
        })) |node| return node;

        var maybe_valid = false;
        const tunneled_stream = (tunnel: {
            var stream = try proxy.host.connect(io, proxy.port, .{ .mode = .stream });
            errdefer stream.close(io);

            const plain = try Connection.Plain.create(client, proxied_host, proxied_port, stream);
            errdefer plain.destroy();

            var req = client.request(.CONNECT, .{
                .scheme = "http",
                .host = .{ .raw = proxied_host.bytes },
                .port = proxied_port,
            }, .{
                .redirect_behavior = .unhandled,
                .connection = &plain.connection,
            }) catch |err| {
                break :tunnel err;
            };
            defer req.deinit();

            req.sendBodiless() catch |err| break :tunnel err;
            const response = req.receiveHead(&.{}) catch |err| break :tunnel err;

            if (response.head.status.class() == .server_error) {
                maybe_valid = true;
                break :tunnel error.ServerError;
            }

            if (response.head.status != .ok) break :tunnel error.ConnectionRefused;

            // buzz-specific: detaching the temporary CONNECT request keeps
            // `req.deinit()` from trying to return the tunnel setup state to
            // the pool before it is wrapped in TLS.
            req.connection = null;

            // buzz-specific: free the temporary plain connection bookkeeping
            // without closing the live socket because the TLS client will take
            // ownership of the tunneled stream next.
            const tunneled_stream = plain.connection.getStream();
            plain.destroy();
            break :tunnel tunneled_stream;
        }) catch {
            proxy.supports_connect = maybe_valid;
            return error.TunnelNotSupported;
        };

        const tls = Connection.Tls.create(client, proxied_host, proxied_port, tunneled_stream) catch |err| switch (err) {
            error.OutOfMemory,
            error.Canceled,
            error.Unexpected,
            => |e| return e,
            else => return error.TlsInitializationFailed,
        };

        // buzz-specific: pooled CONNECT tunnels must stay keyed by the
        // origin endpoint and protocol, not by the proxy transport.
        client.connection_pool.addUsed(io, &tls.connection);
        return &tls.connection;
    }

    if (client.connection_pool.findConnection(io, .{
        .host = proxied_host,
        .port = proxied_port,
        .protocol = target_protocol,
    })) |node| return node;

    var maybe_valid = false;
    (tunnel: {
        const connection = try client.connectTcpOptions(.{
            .host = proxy.host,
            .port = proxy.port,
            .protocol = proxy.protocol,
            .proxied_host = proxied_host,
            .proxied_port = proxied_port,
        });
        errdefer {
            connection.closing = true;
            client.connection_pool.release(connection, io);
        }

        var req = client.request(.CONNECT, .{
            .scheme = "http",
            .host = .{ .raw = proxied_host.bytes },
            .port = proxied_port,
        }, .{
            .redirect_behavior = .unhandled,
            .connection = connection,
        }) catch |err| {
            break :tunnel err;
        };
        defer req.deinit();

        req.sendBodiless() catch |err| break :tunnel err;
        const response = req.receiveHead(&.{}) catch |err| break :tunnel err;

        if (response.head.status.class() == .server_error) {
            maybe_valid = true;
            break :tunnel error.ServerError;
        }

        if (response.head.status != .ok) break :tunnel error.ConnectionRefused;

        // this connection is now a tunnel, so we can't use it for anything
        // else, it will only be released when the client is de-initialized.
        req.connection = null;

        connection.closing = false;

        return connection;
    }) catch {
        // something went wrong with the tunnel
        proxy.supports_connect = maybe_valid;
        return error.TunnelNotSupported;
    };
}

pub const ConnectError = ConnectTcpError || RequestError;

/// Connect to `host:port` using the specified protocol. This will reuse a
/// connection if one is already open.
///
/// If a proxy is configured for the client, then the proxy will be used to
/// connect to the host.
///
/// This function is threadsafe.
pub fn connect(
    client: *Client,
    host: HostName,
    port: u16,
    protocol: Protocol,
) ConnectError!*Connection {
    const proxy = switch (protocol) {
        .plain => client.http_proxy,
        .tls => client.https_proxy,
    } orelse return client.connectTcp(host, port, protocol);

    // Prevent proxying through itself.
    if (proxy.host.eql(host) and proxy.port == port and proxy.protocol == protocol) {
        return client.connectTcp(host, port, protocol);
    }

    if (proxy.supports_connect) tunnel: {
        return connectProxied(client, proxy, host, port, protocol) catch |err| switch (err) {
            error.TunnelNotSupported => break :tunnel,
            else => |e| return e,
        };
    }

    // fall back to using the proxy as a normal http proxy
    const connection = try client.connectTcp(proxy.host, proxy.port, proxy.protocol);
    connection.proxied = true;
    return connection;
}

pub const RequestError = ConnectTcpError || error{
    UnsupportedUriScheme,
    UriMissingHost,
    CertificateBundleLoadFailure,
};

pub const RequestOptions = struct {
    version: http.Version = .@"HTTP/1.1",

    /// Automatically ignore 100 Continue responses. This assumes you don't
    /// care, and will have sent the body before you wait for the response.
    ///
    /// If this is not the case AND you know the server will send a 100
    /// Continue, set this to false and wait for a response before sending the
    /// body. If you wait AND the server does not send a 100 Continue before
    /// you finish the request, then the request *will* deadlock.
    handle_continue: bool = true,

    /// If false, close the connection after the one request. If true,
    /// participate in the client connection pool.
    keep_alive: bool = true,

    /// This field specifies whether to automatically follow redirects, and if
    /// so, how many redirects to follow before returning an error.
    ///
    /// This will only follow redirects for repeatable requests (ie. with no
    /// payload or the server has acknowledged the payload).
    redirect_behavior: Request.RedirectBehavior = @enumFromInt(3),

    /// Must be an already acquired connection.
    connection: ?*Connection = null,

    /// Standard headers that have default, but overridable, behavior.
    headers: Request.Headers = .{},
    /// These headers are kept including when following a redirect to a
    /// different domain.
    /// Externally-owned; must outlive the Request.
    extra_headers: []const http.Header = &.{},
    /// These headers are stripped when following a redirect to a different
    /// domain.
    /// Externally-owned; must outlive the Request.
    privileged_headers: []const http.Header = &.{},
};

fn uriPort(uri: Uri, protocol: Protocol) u16 {
    return uri.port orelse switch (protocol) {
        .plain => 80,
        .tls => 443,
    };
}

/// Open a connection to the host specified by `uri` and prepare to send a HTTP request.
///
/// The caller is responsible for calling `deinit()` on the `Request`.
/// This function is threadsafe.
///
/// Asserts that "\r\n" does not occur in any header name or value.
pub fn request(
    client: *Client,
    method: http.Method,
    uri: Uri,
    options: RequestOptions,
) RequestError!Request {
    const io = client.io;

    if (std.debug.runtime_safety) {
        for (options.extra_headers) |header| {
            assert(header.name.len != 0);
            assert(std.mem.findScalar(u8, header.name, ':') == null);
            assert(std.mem.findPosLinear(u8, header.name, 0, "\r\n") == null);
            assert(std.mem.findPosLinear(u8, header.value, 0, "\r\n") == null);
        }
        for (options.privileged_headers) |header| {
            assert(header.name.len != 0);
            assert(std.mem.findPosLinear(u8, header.name, 0, "\r\n") == null);
            assert(std.mem.findPosLinear(u8, header.value, 0, "\r\n") == null);
        }
    }

    const protocol = Protocol.fromUri(uri) orelse return error.UnsupportedUriScheme;

    if (protocol == .tls) tls: {
        if (disable_tls) unreachable;
        {
            try client.ca_bundle_lock.lockShared(io);
            defer client.ca_bundle_lock.unlockShared(io);
            if (client.now != null) break :tls;
        }
        var bundle: std.crypto.Certificate.Bundle = .empty;
        defer bundle.deinit(client.allocator);
        const now = Io.Clock.real.now(io);
        bundle.rescan(client.allocator, io, now) catch |err| switch (err) {
            error.Canceled => |e| return e,
            else => return error.CertificateBundleLoadFailure,
        };
        try client.ca_bundle_lock.lock(io);
        defer client.ca_bundle_lock.unlock(io);
        client.now = now;
        std.mem.swap(std.crypto.Certificate.Bundle, &client.ca_bundle, &bundle);
    }

    const connection = options.connection orelse c: {
        var host_name_buffer: [HostName.max_len]u8 = undefined;
        const host_name = try uri.getHost(&host_name_buffer);
        break :c try client.connect(host_name, uriPort(uri, protocol), protocol);
    };

    return .{
        .uri = uri,
        .client = client,
        .connection = connection,
        .reader = .{
            .in = connection.reader(),
            .state = .ready,
            // Populated when `http.Reader.bodyReader` is called.
            .interface = undefined,
            .max_head_len = client.read_buffer_size,
        },
        .keep_alive = options.keep_alive,
        .method = method,
        .version = options.version,
        .transfer_encoding = .none,
        .redirect_behavior = options.redirect_behavior,
        .handle_continue = options.handle_continue,
        .headers = options.headers,
        .extra_headers = options.extra_headers,
        .privileged_headers = options.privileged_headers,
    };
}

test {
    _ = Response;
}
