const std = @import("std");
const api = @import("buzz_api.zig");
const http = std.http;

fn getProxy() ?std.http.Client.HttpProxy {
    const proxy = std.os.getenv("HTTPS_PROXY") orelse std.os.getenv("HTTP_PROXY") orelse std.os.getenv("https_proxy") orelse std.os.getenv("http_proxy");

    if (proxy == null) {
        return null;
    }

    const proxy_uri = if (proxy) |p| std.Uri.parse(p) catch null else null;

    return if (proxy_uri) |p|
        .{
            .protocol = if (std.mem.eql(u8, p.scheme, "https")) .tls else .plain,
            .host = proxy_uri.?.host.?,
            .port = p.port,
        }
    else
        null;
}

export fn HttpClientNew(ctx: *api.NativeCtx) c_int {
    const client = api.VM.allocator.create(http.Client) catch @panic("Out of memory");

    client.* = http.Client{
        .allocator = api.VM.allocator,
        .proxy = getProxy(),
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @ptrCast(client))) |userdata| {
        ctx.vm.bz_pushUserData(userdata);

        return 1;
    } else {
        @panic("Out of memory");
    }
}

export fn HttpClientDeinit(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(0).bz_valueToUserData();
    const client = @as(*http.Client, @ptrCast(@alignCast(userdata)));

    client.deinit();

    return 0;
}

export fn HttpClientSend(ctx: *api.NativeCtx) c_int {
    const userdata = ctx.vm.bz_peek(3).bz_valueToUserData();
    const client = @as(*http.Client, @ptrCast(@alignCast(userdata)));

    const method: http.Method = @enumFromInt(api.ObjEnumInstance.bz_getEnumCaseValue(ctx.vm.bz_peek(2)).integer());

    var uri_len: usize = 0;
    const uri = ctx.vm.bz_peek(1).bz_valueToObjString().bz_objStringToString(&uri_len);
    if (uri == null) {
        @panic("Out of memory");
    }

    const header_values = ctx.vm.bz_peek(0);
    var headers = http.Headers.init(api.VM.allocator);
    var next_header_key = api.Value.Null;
    var next_header_value = api.ObjMap.bz_mapNext(ctx.vm, header_values, &next_header_key);
    while (next_header_key.val != api.Value.Null.val) : (next_header_value = api.ObjMap.bz_mapNext(ctx.vm, header_values, &next_header_key)) {
        var key_len: usize = 0;
        const key = next_header_key.bz_valueToString(&key_len);
        var value_len: usize = 0;
        const value = next_header_value.bz_valueToString(&value_len);

        if (key == null or value == null) {
            @panic("Out of memory");
        }

        headers.append(key.?[0..key_len], value.?[0..value_len]) catch @panic("Could not send request");
    }

    const request = api.VM.allocator.create(http.Client.Request) catch @panic("Out of memory");

    request.* = client.request(
        method,
        std.Uri.parse(uri.?[0..uri_len]) catch {
            ctx.vm.pushErrorEnum("http.HttpError", "MalformedUri");

            return -1;
        },
        headers,
        .{},
    ) catch |err| {
        handleError(ctx, err);

        return -1;
    };

    request.start() catch |err| {
        handleStartError(ctx, err);

        return -1;
    };

    if (api.ObjUserData.bz_newUserData(ctx.vm, @ptrCast(request))) |request_ud| {
        ctx.vm.bz_pushUserData(request_ud);

        return 1;
    } else {
        @panic("Out of memory");
    }
}

export fn HttpRequestWait(ctx: *api.NativeCtx) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_valueToUserData();
    const request = @as(*http.Client.Request, @ptrCast(@alignCast(userdata)));

    request.wait() catch |err| {
        handleWaitError(ctx, err);

        return -1;
    };

    ctx.vm.bz_push(userdata_value);

    return 1;
}

export fn HttpRequestRead(ctx: *api.NativeCtx) c_int {
    const userdata_value = ctx.vm.bz_peek(0);
    const userdata = userdata_value.bz_valueToUserData();
    const request = @as(*http.Client.Request, @ptrCast(@alignCast(userdata)));

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
            ) orelse @panic("Out of memory"),
        ),
        if (body_raw.items.len == 0)
            api.Value.Null
        else
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    body_raw.items.ptr,
                    body_raw.items.len,
                ) orelse @panic("Out of memory"),
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
            ) orelse @panic("Out of memory"),
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
            ) orelse @panic("Out of memory"),
        ),
        headers,
    );

    for (request.response.headers.list.items) |header| {
        api.ObjMap.bz_mapSet(
            ctx.vm,
            headers,
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    header.name.ptr,
                    header.name.len,
                ) orelse @panic("Out of memory"),
            ),
            api.ObjString.bz_objStringToValue(
                api.ObjString.bz_string(
                    ctx.vm,
                    header.value.ptr,
                    header.value.len,
                ) orelse @panic("Out of memory"),
            ),
        );
    }

    api.VM.bz_push(ctx.vm, response);

    return 1;
}

fn handleWaitError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => @panic("Out of memory"),

        error.CannotRedirect,
        error.CertificateBundleLoadFailure,
        error.CompressionInitializationFailed,
        error.CompressionNotSupported,
        error.ConnectionRefused,
        error.ConnectionResetByPeer,
        error.ConnectionTimedOut,
        error.EndOfStream,
        error.HostLacksNetworkAddresses,
        error.HttpChunkInvalid,
        error.HttpConnectionHeaderUnsupported,
        error.HttpHeaderContinuationsUnsupported,
        error.HttpHeadersExceededSizeLimit,
        error.HttpHeadersInvalid,
        error.HttpRedirectMissingLocation,
        error.HttpTransferEncodingUnsupported,
        error.InvalidCharacter,
        error.InvalidContentLength,
        error.InvalidFormat,
        error.InvalidPort,
        error.NameServerFailure,
        error.NetworkUnreachable,
        error.Overflow,
        error.TemporaryNameServerFailure,
        error.TlsAlert,
        error.TlsFailure,
        error.TlsInitializationFailed,
        error.TooManyHttpRedirects,
        error.UnexpectedCharacter,
        error.UnexpectedConnectFailure,
        error.UnexpectedReadFailure,
        error.UnexpectedWriteFailure,
        error.UnknownHostName,
        error.UnsupportedTransferEncoding,
        error.UnsupportedUrlScheme,
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
        error.OutOfMemory => @panic("Out of memory"),

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
        error.UnsupportedUrlScheme,
        error.UriMissingHost,
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}

fn handleResponseError(ctx: *api.NativeCtx, err: anytype) void {
    switch (err) {
        error.OutOfMemory => @panic("Out of memory"),

        error.TlsFailure,
        error.TlsAlert,
        error.ConnectionTimedOut,
        error.ConnectionResetByPeer,
        error.UnexpectedReadFailure,
        error.EndOfStream,
        error.HttpChunkInvalid,
        error.HttpHeadersExceededSizeLimit,
        error.DecompressionFailure,
        error.InvalidTrailers,
        error.StreamTooLong,
        => ctx.vm.pushErrorEnum("http.HttpError", @errorName(err)),
    }
}
