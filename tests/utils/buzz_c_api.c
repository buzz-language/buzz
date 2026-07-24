#include "buzz_api.h"

#include <string.h>

static int add(NativeCtx *ctx) {
    const Integer lhs = bz_valueInteger(bz_peek(ctx->vm, 1));
    const Integer rhs = bz_valueInteger(bz_peek(ctx->vm, 0));

    bz_push(ctx->vm, bz_valueFromInteger(lhs + rhs));

    return 1;
}

static int isNumber(NativeCtx *ctx) {
    bz_push(ctx->vm, bz_valueFromBoolean(bz_valueIsNumber(bz_peek(ctx->vm, 0))));

    return 1;
}

static int argCount(NativeCtx *ctx) {
    bz_push(ctx->vm, bz_valueFromInteger((Integer)ctx->arg_count));

    return 1;
}

static int instantiateDefaultedObject(NativeCtx *ctx) {
    Value object_value = bz_peek(ctx->vm, 1);
    Value typedef_value = bz_peek(ctx->vm, 0);
    Integer total = 0;

    for (int i = 0; i < 20000; i++) {
        Value instance = bz_newObjectInstance(
            ctx->vm,
            object_value,
            typedef_value
        );
        Value tokens = bz_getObjectInstanceProperty(instance, 0);

        total += (Integer)bz_listLen(tokens);
    }

    bz_push(ctx->vm, bz_valueFromInteger(total));

    return 1;
}

NativeFn buzz_c_api(const char *symbol) {
    if (strcmp(symbol, "add") == 0) {
        return add;
    }

    if (strcmp(symbol, "isNumber") == 0) {
        return isNumber;
    }

    if (strcmp(symbol, "argCount") == 0) {
        return argCount;
    }

    if (strcmp(symbol, "instantiateDefaultedObject") == 0) {
        return instantiateDefaultedObject;
    }

    return 0;
}
