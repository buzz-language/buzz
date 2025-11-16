const std = @import("std");
const api = @import("buzz_api.zig");
const http = std.http;

pub export fn HttpClientNew(ctx: *api.NativeCtx) callconv(.c) c_int {
    const client = api.VM.allocator.create(http.Client) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    client.* = http.Client{
        .allocator = api.VM.allocator,
    };

    client.initDefaultProxies(api.VM.allocator) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(ctx.vm, @intFromPtr(client)),
    );

    return 1;
}

pub export fn HttpClientDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata = api.bz_getUserDataPtr(
        ctx.vm,
        api.bz_peek(ctx.vm, 0),
    );
    const client = @as(*http.Client, @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata)))));

    client.deinit();
    api.VM.allocator.destroy(client);

    return 0;
}

pub export fn HttpClientSend(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata = api.bz_getUserDataPtr(
        ctx.vm,
        api.bz_peek(ctx.vm, 3),
    );
    const client: *http.Client = @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata))));

    var len: usize = 0;
    const method_str = api.bz_valueToString(
        ctx.vm,
        api.bz_getEnumInstanceValue(
            ctx.vm,
            api.bz_peek(ctx.vm, 2),
        ),
        &len,
    );
    const method: http.Method = @enumFromInt(http.Method.parse(method_str.?[0..len]));

    var uri_len: usize = 0;
    const uri = api.bz_valueToString(
        ctx.vm,
        api.bz_peek(ctx.vm, 1),
        &uri_len,
    );
    if (uri == null) {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    }

    const header_values = api.bz_peek(ctx.vm, 0);
    var headers = std.ArrayList(http.Header).empty;
    var next_header_key = api.Value.Null;
    var next_header_value = header_values.bz_mapNext(&next_header_key);
    while (next_header_key.val != api.Value.Null.val) : (next_header_value = api.bz_mapNext(ctx.vm, header_values, &next_header_key)) {
        var key_len: usize = 0;
        const key = api.bz_valueToString(ctx.vm, next_header_key, &key_len);
        var value_len: usize = 0;
        const value = api.bz_valueToString(ctx.vm, next_header_value, &value_len);

        if (key == null or value == null) {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        }

        headers.append(
            api.VM.allocator,
            .{
                .name = key.?[0..key_len],
                .value = value.?[0..value_len],
            },
        ) catch {
            api.bz_panic(ctx.vm, "Could not send request", "Could not send request".len);
            unreachable;
        };
    }

    const request = api.VM.allocator.create(http.Client.Request) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };
    const server_header_buffer = api.VM.allocator.alloc(u8, 16 * 1024) catch {
        api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
        unreachable;
    };

    request.* = client.request(
        method,
        std.Uri.parse(uri.?[0..uri_len]) catch {
            api.pushErrorEnum(ctx.vm, "http.HttpError", "MalformedUri");

            return -1;
        },
        .{
            .extra_headers = headers.items,
            .server_header_buffer = server_header_buffer,
        },
    ) catch |err| {
        handleError(ctx, err);

        return -1;
    };

    request.send() catch |err| {
        handleStartError(ctx, err);

        return -1;
    };

    api.bz_push(
        ctx.vm,
        api.bz_newUserData(ctx.vm, @intFromPtr(request)),
    );

    return 1;
}

pub export fn HttpRequestWait(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata_value = api.bz_peek(ctx.vm, 0);
    const userdata = api.bz_getUserDataPtr(ctx.vm, userdata_value);
    const request = @as(
        *http.Client.Request,
        @ptrCast(
            @alignCast(
                @as(*anyopaque, @ptrFromInt(userdata)),
            ),
        ),
    );

    request.wait() catch |err| {
        handleWaitError(ctx, err);

        return -1;
    };

    api.bz_push(ctx.vm, userdata_value);

    return 1;
}

pub export fn HttpRequestDeinit(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata_value = api.bz_peek(ctx.vm, 0);
    const userdata = api.bz_getUserDataPtr(ctx.vm, userdata_value);
    const request = @as(
        *http.Client.Request,
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

pub export fn HttpRequestRead(ctx: *api.NativeCtx) callconv(.c) c_int {
    const userdata_value = api.bz_peek(ctx.vm, 0);
    const userdata = api.bz_getUserDataPtr(ctx.vm, userdata_value);
    const request = @as(
        *http.Client.Request,
        @ptrCast(
            @alignCast(
                @as(*anyopaque, @ptrFromInt(userdata)),
            ),
        ),
    );

    var body_raw = std.ArrayList(u8).empty;
    defer body_raw.deinit(api.VM.allocator);

    request.reader().readAllArrayList(&body_raw, std.math.maxInt(usize)) catch |err| {
        handleResponseError(ctx, err);

        return -1;
    };

    // Create http.Response instance
    const response = api.bz_newQualifiedObjectInstance(
        ctx.vm,
        "http.Response",
        "http.Response".len,
        false,
    );

    // Set body
    api.bz_setObjectInstanceProperty(
        ctx.vm,
        response,
        2,
        if (body_raw.items.len == 0)
            .Null
        else
            api.bz_stringToValue(
                ctx.vm,
                body_raw.items.ptr,
                body_raw.items.len,
            ),
        ctx.vm,
    );

    // Set status
    response.bz_setObjectInstanceProperty(
        0,
        .fromInteger(@intFromEnum(request.response.status)),
        ctx.vm,
    );

    // Set headers
    const string_type = api.bz_stringType(ctx.vm);
    const headers = api.bz_newMap(
        ctx.vm,
        api.bz_mapType(
            ctx.vm,
            string_type,
            string_type,
            true,
        ),
    );

    api.bz_setObjectInstanceProperty(
        ctx.vm,
        response,
        1,
        headers,
    );

    var header_it = request.response.iterateHeaders();
    while (header_it.next()) |header| {
        api.bz_mapSet(
            ctx.vm,
            headers,
            api.bz_stringToValue(
                ctx.vm,
                header.name.ptr,
                header.name.len,
            ),
            api.bz_stringToValue(
                ctx.vm,
                header.value.ptr,
                header.value.len,
            ),
        );
    }

    api.bz_push(ctx.vm, response);

    return 1;
}

fn handleWaitError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },

        error.CertificateBundleLoadFailure,
        error.CompressionInitializationFailed,
        error.CompressionUnsupported,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.EndOfStream,
        error.HostLacksNetworkAddresses,
        error.HttpChunkInvalid,
        error.HttpConnectionHeaderUnsupported,
        error.HttpHeaderContinuationsUnsupported,
        error.HttpHeadersInvalid,
        error.HttpHeadersOversize,
        error.HttpRedirectLocationInvalid,
        error.HttpRedirectLocationMissing,
        error.HttpTransferEncodingUnsupported,
        error.InvalidCharacter,
        error.InvalidContentLength,
        error.NameServerFailure,
        error.NetworkUnreachable,
        error.Overflow,
        error.RedirectRequiresResend,
        error.TemporaryNameServerFailure,
        error.TlsAlert,
        error.TlsFailure,
        error.TlsInitializationFailed,
        error.TooManyHttpRedirects,
        error.UnexpectedConnectFailure,
        error.UnexpectedReadFailure,
        error.UnexpectedWriteFailure,
        error.UnknownHostName,
        error.UnsupportedTransferEncoding,
        error.UnsupportedUriScheme,
        error.UriMissingHost,
        => api.pushErrorEnum(ctx.vm, "http.HttpError", @errorName(err)),
    }
}

fn handleStartError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.ConnectionResetByPeer,
        error.UnexpectedWriteFailure,
        error.InvalidContentLength,
        error.UnsupportedTransferEncoding,
        => api.pushErrorEnum(ctx.vm, "http.HttpError", @errorName(err)),
    }
}

fn handleError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },

        error.CertificateBundleLoadFailure,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.HostLacksNetworkAddresses,
        error.InvalidCharacter,
        error.InvalidContentLength,
        error.NameServerFailure,
        error.NetworkUnreachable,
        error.Overflow,
        error.TemporaryNameServerFailure,
        error.TlsInitializationFailed,
        error.UnexpectedConnectFailure,
        error.UnexpectedWriteFailure,
        error.UnknownHostName,
        error.UnsupportedTransferEncoding,
        error.UnsupportedUriScheme,
        error.UriMissingHost,
        => api.pushErrorEnum(ctx.vm, "http.HttpError", @errorName(err)),
    }
}

fn handleResponseError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            api.bz_panic(ctx.vm, "Out of memory", "Out of memory".len);
            unreachable;
        },

        error.TlsFailure,
        error.TlsAlert,
        error.ConnectionTimedOut,
        error.ConnectionResetByPeer,
        error.UnexpectedReadFailure,
        error.EndOfStream,
        error.HttpChunkInvalid,
        error.DecompressionFailure,
        error.InvalidTrailers,
        error.StreamTooLong,
        error.HttpHeadersOversize,
        => api.pushErrorEnum(ctx.vm, "http.HttpError", @errorName(err)),
    }
}
