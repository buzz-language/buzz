const std = @import("std");
const api = @import("buzz_api.zig");
const http = std.http;

pub export fn HttpClientNew(ctx: *api.NativeCtx) c_int {
    const client = api.VM.allocator.create(http.Client) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    client.* = http.Client{
        .allocator = api.VM.allocator,
    };

    client.initDefaultProxies(api.VM.allocator) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @intFromPtr(client))) |userdata| {
        ctx.vm.bz_pushUserData(userdata);

        return 1;
    } else {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    }
}

pub export fn HttpClientDeinit(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const client = @as(*http.Client, @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata)))));

    client.deinit();
    api.VM.allocator.destroy(client);

    return 0;
}

pub export fn HttpClientSend(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(3).bz_valueToUserData();
    const client = @as(*http.Client, @ptrCast(@alignCast(@as(*anyopaque, @ptrFromInt(userdata)))));

    var len: usize = 0;
    const method_str = api.ObjEnumInstance.bz_getEnumCaseValue(ctx.vm.bz_peek(2)).bz_valueToString(&len);
    const method: http.Method = @enumFromInt(http.Method.parse(method_str.?[0..len]));

    var uri_len: usize = 0;
    const uri = ctx.vm.bz_peek(1).bz_valueToObjString().bz_objStringToString(&uri_len);
    if (uri == null) {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    }

    const header_values = ctx.vm.bz_peek(0);
    var headers = std.ArrayList(http.Header).init(api.VM.allocator);
    var next_header_key = api.Value.Null;
    var next_header_value = api.ObjMap.bz_mapNext(ctx.vm, header_values, &next_header_key);
    while (next_header_key.val != api.Value.Null.val) : (next_header_value = api.ObjMap.bz_mapNext(ctx.vm, header_values, &next_header_key)) {
        var key_len: usize = 0;
        const key = next_header_key.bz_valueToString(&key_len);
        var value_len: usize = 0;
        const value = next_header_value.bz_valueToString(&value_len);

        if (key == null or value == null) {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
            unreachable;
        }

        headers.append(
            .{
                .name = key.?[0..key_len],
                .value = value.?[0..value_len],
            },
        ) catch {
            ctx.vm.bz_panic("Could not send request", "Could not send request".len);
            unreachable;
        };
    }

    const request = api.VM.allocator.create(http.Client.Request) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };
    const server_header_buffer = api.VM.allocator.alloc(u8, 16 * 1024) catch {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    };

    request.* = client.open(
        method,
        std.Uri.parse(uri.?[0..uri_len]) catch {
            ctx.vm.pushErrorEnum("http.HttpError", "MalformedUri");

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

    if (api.ObjUserData.bz_newUserData(ctx.vm, @intFromPtr(request))) |request_ud| {
        ctx.vm.bz_pushUserData(request_ud);

        return 1;
    } else {
        ctx.vm.bz_panic("Out of memory", "Out of memory".len);
        unreachable;
    }
}

pub export fn HttpRequestWait(ctx: *api.NativeCtx) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_valueToUserData();
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

    ctx.vm.bz_push(userdata_value);

    return 1;
}

pub export fn HttpRequestDeinit(ctx: *api.NativeCtx) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_valueToUserData();
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

pub export fn HttpRequestRead(ctx: *api.NativeCtx) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_valueToUserData();
    const request = @as(
        *http.Client.Request,
        @ptrCast(
            @alignCast(
                @as(*anyopaque, @ptrFromInt(userdata)),
            ),
        ),
    );

    var body_raw = std.ArrayList(u8).init(api.VM.allocator);
    defer body_raw.deinit();

    request.reader().readAllArrayList(&body_raw, std.math.maxInt(usize)) catch |err| {
        handleResponseError(ctx, err);

        return -1;
    };

    // Create http.Response instance
    const response = api.ObjObject.bz_instanceQualified(
        ctx.vm,
        "http.Response",
        "http.Response".len,
    );

    // Set body
    api.ObjObject.bz_setInstanceField(
        ctx.vm,
        response,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                ctx.vm,
                "body".ptr,
                "body".len,
            ) orelse {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        ),
        if (body_raw.items.len == 0)
            api.Value.Null
        else
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    body_raw.items.ptr,
                    body_raw.items.len,
                ) orelse {
                    ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                    unreachable;
                },
            ),
    );

    // Set status
    api.ObjObject.bz_setInstanceField(
        ctx.vm,
        response,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                ctx.vm,
                "status".ptr,
                "status".len,
            ) orelse {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        ),
        api.Value.fromInteger(@intFromEnum(request.response.status)),
    );

    // Set headers
    const string_type = api.ObjTypeDef.bz_stringType(ctx.vm);
    const headers = api.ObjMap.bz_newMap(
        ctx.vm,
        api.ObjTypeDef.bz_mapType(
            ctx.vm,
            string_type,
            string_type,
        ),
    );

    api.ObjObject.bz_setInstanceField(
        ctx.vm,
        response,
        api.ObjString.bz_objStringToValue(
            api.ObjString.bz_string(
                ctx.vm,
                "headers".ptr,
                "headers".len,
            ) orelse {
                ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                unreachable;
            },
        ),
        headers,
    );

    var header_it = request.response.iterateHeaders();
    while (header_it.next()) |header| {
        api.ObjMap.bz_mapSet(
            ctx.vm,
            headers,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    header.name.ptr,
                    header.name.len,
                ) orelse {
                    ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                    unreachable;
                },
            ),
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    header.value.ptr,
                    header.value.len,
                ) orelse {
                    ctx.vm.bz_panic("Out of memory", "Out of memory".len);
                    unreachable;
                },
            ),
        );
    }

    api.VM.bz_push(ctx.vm, response);

    return 1;
}

fn handleWaitError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
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
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}

fn handleStartError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.ConnectionResetByPeer,
        error.UnexpectedWriteFailure,
        error.InvalidContentLength,
        error.UnsupportedTransferEncoding,
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}

fn handleError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
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
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}

fn handleResponseError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => {
            ctx.vm.bz_panic("Out of memory", "Out of memory".len);
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
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}
