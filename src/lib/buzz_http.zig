const std = @import("std");
const api = @import("buzz_api.zig");

const HttpClient = struct {
    client: std.http.Client,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: *HttpClient) void {
        self.client.deinit();
        self.arena.deinit();
    }
};

const HttpRequest = struct {
    request: std.http.Client.Request,
    extra_headers: std.ArrayList(std.http.Header),

    fn deinit(self: *HttpRequest) void {
        self.request.deinit();
        self.extra_headers.deinit(api.VM.allocator);
    }
};

fn innerHttpClientNew(ctx: *api.NativeCtx) !c_int {
    const client = try api.VM.allocator.create(HttpClient);

    client.* = .{
        .arena = std.heap.ArenaAllocator.init(api.VM.allocator),
        .client = std.http.Client{
            .io = ctx.getIo(),
            .allocator = api.VM.allocator,
        },
    };
    errdefer {
        client.deinit();
        api.VM.allocator.destroy(client);
    }

    try client.client.initDefaultProxies(client.arena.allocator(), ctx.getEnv());

    ctx.vm.bz_push(
        api.VM.bz_newUserData(
            ctx.vm,
            @intFromPtr(client),
        ),
    );

    return 1;
}

pub export fn HttpClientNew(ctx: *api.NativeCtx) callconv(.c) c_int {
    return innerHttpClientNew(ctx) catch |err| {
        switch (err) {
            error.InvalidHostName,
            error.UriMissingHost,
            error.UnexpectedCharacter,
            error.InvalidFormat,
            error.InvalidPort,
            => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        }

        return -1;
    };
}

pub export fn HttpClientDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_getUserDataPtr();
    const client = @as(*HttpClient, @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata)))));

    client.deinit();
    api.VM.allocator.destroy(client);

    return 0;
}

const methods = std.StaticStringMap(std.http.Method).initComptime(
    &.{
        &.{ "CONNECT", .CONNECT },
        &.{ "DELETE", .DELETE },
        &.{ "GET", .GET },
        &.{ "HEAD", .HEAD },
        &.{ "OPTIONS", .OPTIONS },
        &.{ "PATCH", .PATCH },
        &.{ "POST", .POST },
        &.{ "PUT", .PUT },
        &.{ "TRACE", .TRACE },
    },
);

fn innerHttpClientSend(ctx: *api.NativeCtx) !c_int {
    const userdata = ctx.vm.bz_peek(4).bz_getUserDataPtr();
    const client: *HttpClient = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    var len: usize = 0;
    const method_str = ctx.vm.bz_peek(3)
        .bz_getEnumInstanceValue()
        .bz_valueToString(&len) orelse return error.OutOfMemory;
    const method = methods.get(method_str[0..len]).?;

    var uri_len: usize = 0;
    const uri = ctx.vm.bz_peek(2).bz_valueToString(&uri_len) orelse return error.OutOfMemory;

    var request_initialized = false;

    const header_values = ctx.vm.bz_peek(1);
    var headers = std.ArrayList(std.http.Header).empty;
    errdefer if (!request_initialized) headers.deinit(api.VM.allocator);
    var next_header_key = api.Value.Null;
    var next_header_value = header_values.bz_mapNext(&next_header_key);
    while (next_header_key.val != api.Value.Null.val) : (next_header_value = header_values.bz_mapNext(&next_header_key)) {
        var key_len: usize = 0;
        const key = next_header_key.bz_valueToString(&key_len) orelse return error.OutOfMemory;
        var value_len: usize = 0;
        const value = next_header_value.bz_valueToString(&value_len) orelse return error.OutOfMemory;

        try headers.append(
            api.VM.allocator,
            .{
                .name = key[0..key_len],
                .value = value[0..value_len],
            },
        );
    }

    const request = try api.VM.allocator.create(HttpRequest);
    errdefer {
        if (request_initialized) request.deinit();
        api.VM.allocator.destroy(request);
    }
    const parsed_uri = try std.Uri.parse(uri[0..uri_len]);
    request.* = .{
        .extra_headers = headers,
        .request = try client.client.request(
            method,
            parsed_uri,
            .{
                .extra_headers = headers.items,
            },
        ),
    };
    request_initialized = true;

    const body_value = ctx.vm.bz_peek(0);

    if (method.requestHasBody()) {
        const body_str =
            if (!body_value.isNull())
                body_value.bz_valueToString(&len) orelse return error.OutOfMemory
            else
                null;
        const body = if (body_str) |str| str[0..len] else null;

        request.request.transfer_encoding = .{
            .content_length = if (body) |b| b.len else 0,
        };

        var body_stream = try request.request.sendBodyUnflushed(&.{});

        if (body) |b| {
            try body_stream.writer.writeAll(b);
        }

        try body_stream.end();

        try request.request.connection.?.flush();
    } else {
        try request.request.sendBodiless();
    }

    ctx.vm.bz_push(
        api.VM.bz_newUserData(
            ctx.vm,
            @intFromPtr(request),
        ),
    );

    return 1;
}

pub export fn HttpClientSend(ctx: *api.NativeCtx) callconv(.c) c_int {
    return innerHttpClientSend(ctx) catch |err| {
        switch (err) {
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
            error.Timeout,
            error.Canceled,
            error.SystemResources,
            error.ConnectionResetByPeer,
            error.WouldBlock,
            error.AccessDenied,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.NetworkDown,
            error.AddressInUse,
            error.AddressUnavailable,
            error.AddressFamilyUnsupported,
            error.ProtocolUnsupportedBySystem,
            error.ProtocolUnsupportedByAddressFamily,
            error.SocketModeUnsupported,
            error.OptionUnsupported,
            error.ConnectionPending,
            error.ConnectionRefused,
            error.HostUnreachable,
            error.NetworkUnreachable,
            error.UnknownHostName,
            error.ResolvConfParseFailed,
            error.InvalidDnsARecord,
            error.InvalidDnsAAAARecord,
            error.InvalidDnsCnameRecord,
            error.NameServerFailure,
            error.NoAddressReturned,
            error.DetectingNetworkConfigurationFailed,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.WriteFailed,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
            error.InvalidHostName,
            error.TlsInitializationFailed,
            error.UnsupportedUriScheme,
            error.UriMissingHost,
            error.CertificateBundleLoadFailure,
            error.UnexpectedCharacter,
            error.InvalidFormat,
            error.InvalidPort,
            => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
        }

        return -1;
    };
}

fn innerHttpRequestWait(ctx: *api.NativeCtx) !c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_getUserDataPtr();
    const request = @as(
        *HttpRequest,
        @ptrCast(
            @alignCast(
                @as(*anyopaque, @ptrFromInt(userdata)),
            ),
        ),
    );

    var redirect_buffer: [8000]u8 = undefined;
    var http_response = try request.request.receiveHead(&redirect_buffer);

    var transfer_buffer: [4096]u8 = undefined;
    var decompress: std.http.Decompress = undefined;
    const decompress_buffer: []u8 = switch (http_response.head.content_encoding) {
        .identity => &.{},
        .zstd => try api.VM.allocator.alloc(u8, std.compress.zstd.default_window_len),
        .deflate, .gzip => try api.VM.allocator.alloc(u8, std.compress.flate.max_window_len),
        .compress => return error.UnsupportedCompressionMethod,
    };
    defer api.VM.allocator.free(decompress_buffer);

    // Create http.Response instance
    const response = ctx.vm.bz_newQualifiedObjectInstance(
        "http.Response",
        "http.Response".len,
        false,
    );

    // Set status
    response.bz_setObjectInstanceProperty(
        3,
        api.Value.fromInteger(@intFromEnum(http_response.head.status)),
        ctx.vm,
    );

    // Set headers
    const string_type = ctx.vm.bz_stringType();
    const headers = ctx.vm.bz_newMap(
        ctx.vm.bz_mapType(
            string_type,
            string_type,
            true,
        ),
    );

    response.bz_setObjectInstanceProperty(
        2,
        headers,
        ctx.vm,
    );

    var header_it = http_response.head.iterateHeaders();
    while (header_it.next()) |header| {
        headers.bz_mapSet(
            api.VM.bz_stringToValue(
                ctx.vm,
                header.name.ptr,
                header.name.len,
            ),
            api.VM.bz_stringToValue(
                ctx.vm,
                header.value.ptr,
                header.value.len,
            ),
            ctx.vm,
        );
    }

    const body_raw = try http_response.readerDecompressing(
        &transfer_buffer,
        &decompress,
        decompress_buffer,
    )
        .allocRemaining(api.VM.allocator, .unlimited);
    defer api.VM.allocator.free(body_raw); // It'll be copied in the GC string pool

    // Set body
    response.bz_setObjectInstanceProperty(
        0,
        if (body_raw.len == 0)
            .Null
        else
            api.VM.bz_stringToValue(
                ctx.vm,
                body_raw.ptr,
                body_raw.len,
            ),
        ctx.vm,
    );

    ctx.vm.bz_push(response);

    return 1;
}

pub export fn HttpRequestWait(ctx: *api.NativeCtx) callconv(.c) c_int {
    return innerHttpRequestWait(ctx) catch |err| {
        switch (err) {
            error.OutOfMemory => {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
            error.Timeout,
            error.Canceled,
            error.SystemResources,
            error.ConnectionResetByPeer,
            error.WouldBlock,
            error.AccessDenied,
            error.ProcessFdQuotaExceeded,
            error.SystemFdQuotaExceeded,
            error.NetworkDown,
            error.AddressInUse,
            error.AddressUnavailable,
            error.AddressFamilyUnsupported,
            error.ProtocolUnsupportedBySystem,
            error.ProtocolUnsupportedByAddressFamily,
            error.SocketModeUnsupported,
            error.OptionUnsupported,
            error.ConnectionPending,
            error.ConnectionRefused,
            error.HostUnreachable,
            error.NetworkUnreachable,
            error.UnknownHostName,
            error.ResolvConfParseFailed,
            error.InvalidDnsARecord,
            error.InvalidDnsAAAARecord,
            error.InvalidDnsCnameRecord,
            error.NameServerFailure,
            error.NoAddressReturned,
            error.DetectingNetworkConfigurationFailed,
            => ctx.vm.pushErrorEnum("errors.SocketError", @errorName(err)),
            error.WriteFailed,
            error.ReadFailed,
            error.StreamTooLong,
            => ctx.vm.pushErrorEnum("errors.ReadWriteError", @errorName(err)),
            error.Unexpected => ctx.vm.pushError("errors.UnexpectedError", null),
            error.HttpChunkInvalid,
            error.HttpChunkTruncated,
            error.HttpHeadersOversize,
            error.HttpRequestTruncated,
            error.HttpConnectionClosing,
            error.TlsInitializationFailed,
            error.UnsupportedUriScheme,
            error.UriMissingHost,
            error.CertificateBundleLoadFailure,
            error.HttpHeadersInvalid,
            error.TooManyHttpRedirects,
            error.RedirectRequiresResend,
            error.HttpRedirectLocationMissing,
            error.HttpRedirectLocationOversize,
            error.HttpRedirectLocationInvalid,
            error.HttpContentEncodingUnsupported,
            error.UnsupportedCompressionMethod,
            => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
        }

        return -1;
    };
}

pub export fn HttpRequestDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_getUserDataPtr();
    const request = @as(
        *HttpRequest,
        @ptrCast(
            @alignCast(
                @as(*anyopaque, @ptrFromInt(userdata)),
            ),
        ),
    );

    request.deinit();
    api.VM.allocator.destroy(request);

    return 0;
}

pub const library = api.BuzzApi(
    "http",
    &.{
        &.{ "HttpClientNew", HttpClientNew },
        &.{ "HttpClientDeinit", HttpClientDeinit },
        &.{ "HttpClientSend", HttpClientSend },
        &.{ "HttpRequestWait", HttpRequestWait },
        &.{ "HttpRequestDeinit", HttpRequestDeinit },
    },
){};
