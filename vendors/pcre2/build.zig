const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const upstream = b.dependency("pcre2", .{});

    const lib = b.addStaticLibrary(.{
        .name = "pcre2",
        .target = target,
        .optimize = optimize,
    });
    lib.linkLibC();
    lib.addIncludePath(upstream.path("src"));
    lib.installHeadersDirectoryOptions(.{
        .source_dir = upstream.path("src"),
        .install_dir = .header,
        .install_subdir = "",
        .include_extensions = &.{".h"},
    });

    const config_h = b.addConfigHeader(
        .{
            .style = .{
                .autoconf = upstream.path("src/config.h.in"),
            },
        },
        .{
            .EBCDIC = null,
            .EBCDIC_NL25 = null,
            .HAVE_ATTRIBUTE_UNINITIALIZED = null,
            .HAVE_BCOPY = null,
            .HAVE_BZLIB_H = true,
            .HAVE_DIRENT_H = true,
            .HAVE_DLFCN_H = true,
            .HAVE_EDITLINE_READLINE_H = null,
            .HAVE_EDIT_READLINE_READLINE_H = null,
            .HAVE_INTTYPES_H = true,
            .HAVE_LIMITS_H = true,
            .HAVE_MEMFD_CREATE = true,
            .HAVE_MEMMOVE = true,
            .HAVE_MINIX_CONFIG_H = null,
            .HAVE_MKOSTEMP = null,
            .HAVE_PTHREAD = true,
            .HAVE_PTHREAD_PRIO_INHERIT = true,
            .HAVE_READLINE_H = null,
            .HAVE_READLINE_HISTORY_H = null,
            .HAVE_READLINE_READLINE_H = null,
            .HAVE_REALPATH = true,
            .HAVE_SECURE_GETENV = null,
            .HAVE_STDINT_H = true,
            .HAVE_STDIO_H = true,
            .HAVE_STDLIB_H = true,
            .HAVE_STRERROR = true,
            .HAVE_STRINGS_H = true,
            .HAVE_STRING_H = true,
            .HAVE_SYS_STAT_H = true,
            .HAVE_SYS_TYPES_H = true,
            .HAVE_SYS_WAIT_H = true,
            .HAVE_UNISTD_H = true,
            .HAVE_VISIBILITY = true,
            .HAVE_WCHAR_H = true,
            .HAVE_WINDOWS_H = null,
            .HAVE_ZLIB_H = null,
            .HEAP_LIMIT = 20000000,
            .LT_OBJDIR = null,
            .MATCH_LIMIT = 10000000,
            .MATCH_LIMIT_DEPTH = .MATCH_LIMIT,
            .NEVER_BACKSLASH_C = null,
            .NEWLINE_DEFAULT = 2,
            .PACKAGE = null,
            .PACKAGE_BUGREPORT = null,
            .PACKAGE_NAME = null,
            .PACKAGE_STRING = null,
            .PACKAGE_TARNAME = null,
            .PACKAGE_URL = null,
            .PACKAGE_VERSION = null,
            .PARENS_NEST_LIMIT = 250,
            .PCRE2GREP_BUFSIZE = null,
            .PCRE2GREP_MAX_BUFSIZE = null,
            .PCRE2POSIX_EXP_DECL = null,
            .PCRE2POSIX_EXP_DEFN = null,
            .PCRE2_DEBUG = null,
            .PCRE2_EXP_DECL = null,
            .PCRE2_EXP_DEFN = null,
            .PCRE2_STATIC = null,
            .PTHREAD_CREATE_JOINABLE = null,
            .SLJIT_PROT_EXECUTABLE_ALLOCATOR = null,
            .STDC_HEADERS = null,
            .SUPPORT_JIT = null,
            .SUPPORT_LIBBZ2 = null,
            .SUPPORT_LIBEDIT = null,
            .SUPPORT_LIBREADLINE = null,
            .SUPPORT_LIBZ = null,
            .SUPPORT_PCRE2GREP_CALLOUT = null,
            .SUPPORT_PCRE2GREP_CALLOUT_FORK = null,
            .SUPPORT_PCRE2GREP_JIT = null,
            .SUPPORT_PCRE2_16 = null,
            .SUPPORT_PCRE2_32 = null,
            .SUPPORT_PCRE2_8 = null,
            .SUPPORT_UNICODE = null,
            .SUPPORT_VALGRIND = null,
            ._ALL_SOURCE = null,
            ._DARWIN_C_SOURCE = null,
            .__EXTENSIONS__ = null,
            ._GNU_SOURCE = null,
            ._HPUX_ALT_XOPEN_SOCKET_API = null,
            ._MINIX = null,
            ._NETBSD_SOURCE = null,
            ._OPENBSD_SOURCE = null,
            ._POSIX_SOURCE = null,
            ._POSIX_1_SOURCE = null,
            ._POSIX_PTHREAD_SEMANTICS = null,
            .__STDC_WANT_IEC_60559_ATTRIBS_EXT__ = null,
            .__STDC_WANT_IEC_60559_BFP_EXT__ = null,
            .__STDC_WANT_IEC_60559_DFP_EXT__ = null,
            .__STDC_WANT_IEC_60559_FUNCS_EXT__ = null,
            .__STDC_WANT_IEC_60559_TYPES_EXT__ = null,
            .__STDC_WANT_LIB_EXT2__ = null,
            .__STDC_WANT_MATH_SPEC_FUNCS__ = null,
            ._TANDEM_SOURCE = null,
            ._XOPEN_SOURCE = null,
            .VERSION = null,
            ._FILE_OFFSET_BITS = null,
            ._LARGE_FILES = null,
            .@"const" = null,
            .int64_t = null,
            .size_t = null,
            .BSR_ANYCRLF = null,
            .DISABLE_PERCENT_ZT = null,
            .LINK_SIZE = 2,
            .MAX_NAME_COUNT = 10009,
            .MAX_NAME_SIZE = 32,
        },
    );
    const pcre2_h = b.addConfigHeader(.{
        .include_path = "pcre2.h",
        .style = .{
            // WORKAROUND: automake is broken for the pcre2.h.in file
            .cmake = upstream.path("src/pcre2.h.generic"),
        },
    }, .{});

    // TODO: workaround because there is no way to copy file with lazy paths
    const chartables = b.addConfigHeader(.{
        .include_path = "pcre2_chartables.c",
        .style = .{
            .cmake = upstream.path("src/pcre2_chartables.c.dist"),
        },
    }, .{});

    lib.addConfigHeader(config_h);
    lib.addConfigHeader(pcre2_h);

    const files = [_]std.Build.LazyPath{
        upstream.path("src/pcre2_auto_possess.c"),
        upstream.path("src/pcre2_compile.c"),
        upstream.path("src/pcre2_config.c"),
        upstream.path("src/pcre2_context.c"),
        upstream.path("src/pcre2_convert.c"),
        upstream.path("src/pcre2_dfa_match.c"),
        upstream.path("src/pcre2_error.c"),
        upstream.path("src/pcre2_extuni.c"),
        upstream.path("src/pcre2_find_bracket.c"),
        upstream.path("src/pcre2_maketables.c"),
        upstream.path("src/pcre2_match.c"),
        upstream.path("src/pcre2_match_data.c"),
        upstream.path("src/pcre2_newline.c"),
        upstream.path("src/pcre2_ord2utf.c"),
        upstream.path("src/pcre2_pattern_info.c"),
        upstream.path("src/pcre2_script_run.c"),
        upstream.path("src/pcre2_serialize.c"),
        upstream.path("src/pcre2_string_utils.c"),
        upstream.path("src/pcre2_study.c"),
        upstream.path("src/pcre2_substitute.c"),
        upstream.path("src/pcre2_substring.c"),
        upstream.path("src/pcre2_tables.c"),
        upstream.path("src/pcre2_ucd.c"),
        upstream.path("src/pcre2_valid_utf.c"),
        upstream.path("src/pcre2_xclass.c"),
        chartables.getOutput(),
    };
    for (files) |file| {
        lib.addCSourceFile(.{
            .file = file,
            .flags = &.{
                "-std=c99",
                "-DHAVE_CONFIG_H",
                "-DPCRE2_CODE_UNIT_WIDTH=8",
                "-DPCRE2_STATIC",
            },
        });
    }

    b.installArtifact(lib);
}
