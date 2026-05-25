#ifndef BUZZ_API_H
#define BUZZ_API_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <setjmp.h>
#include <string.h>

typedef double Double;
typedef int64_t Integer;

typedef struct VM VM;
typedef struct Init Init;
typedef struct ZigType ZigType;
typedef struct Value Value;
typedef struct NativeCtx NativeCtx;
typedef struct TryCtx TryCtx;

typedef int (*NativeFn)(NativeCtx *ctx);

struct Value {
    uint64_t val;
};

#define BZ_TAG_BOOLEAN UINT64_C(0)
#define BZ_TAG_INTEGER UINT64_C(1)
#define BZ_TAG_NULL UINT64_C(2)
#define BZ_TAG_VOID UINT64_C(3)
#define BZ_TAG_OBJ UINT64_C(4)
#define BZ_TAG_ERROR UINT64_C(5)

#define BZ_SIGN_MASK UINT64_C(0x8000000000000000)
#define BZ_TAGGED_VALUE_MASK UINT64_C(0x7ffc000000000000)
#define BZ_TAGGED_UPPER_VALUE_MASK UINT64_C(0xffff000000000000)
#define BZ_POINTER_MASK (BZ_TAGGED_VALUE_MASK | BZ_SIGN_MASK)
#define BZ_BOOLEAN_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_BOOLEAN << 32))
#define BZ_FALSE_MASK BZ_BOOLEAN_MASK
#define BZ_TRUE_BIT_MASK UINT64_C(1)
#define BZ_TRUE_MASK (BZ_BOOLEAN_MASK | BZ_TRUE_BIT_MASK)
#define BZ_INTEGER_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_INTEGER << 49))
#define BZ_NULL_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_NULL << 32))
#define BZ_VOID_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_VOID << 32))
#define BZ_ERROR_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_ERROR << 32))
#define BZ_TAG_MASK UINT64_C(7)
#define BZ_TAGGED_PRIMITIVE_MASK (BZ_TAGGED_VALUE_MASK | (BZ_TAG_MASK << 32) | BZ_INTEGER_MASK)
#define BZ_INTEGER_PAYLOAD_MASK UINT64_C(0x0000ffffffffffff)
#define BZ_INTEGER_SIGN_BIT UINT64_C(0x0000800000000000)

static inline Value bz_valueFromRaw(uint64_t val) {
    Value value = {val};
    return value;
}

static inline Value bz_valueNull(void) {
    return bz_valueFromRaw(BZ_NULL_MASK);
}

static inline Value bz_valueVoid(void) {
    return bz_valueFromRaw(BZ_VOID_MASK);
}

static inline Value bz_valueTrue(void) {
    return bz_valueFromRaw(BZ_TRUE_MASK);
}

static inline Value bz_valueFalse(void) {
    return bz_valueFromRaw(BZ_FALSE_MASK);
}

static inline Value bz_valueError(void) {
    return bz_valueFromRaw(BZ_ERROR_MASK);
}

static inline Value bz_valueFromBoolean(bool val) {
    return val ? bz_valueTrue() : bz_valueFalse();
}

static inline Value bz_valueFromInteger(Integer val) {
    return bz_valueFromRaw(BZ_INTEGER_MASK | (((uint64_t)val) & BZ_INTEGER_PAYLOAD_MASK));
}

static inline Value bz_valueFromDouble(Double val) {
    Value value;
    memcpy(&value.val, &val, sizeof(value.val));
    return value;
}

static inline Value bz_valueFromObj(void *val) {
    return bz_valueFromRaw(BZ_POINTER_MASK | (uint64_t)(uintptr_t)val);
}

static inline uint8_t bz_valueGetTag(Value self) {
    return (uint8_t)(((uint32_t)(self.val >> 32)) & BZ_TAG_MASK);
}

static inline bool bz_valueIsBool(Value self) {
    return (self.val & (BZ_TAGGED_PRIMITIVE_MASK | BZ_SIGN_MASK)) == BZ_BOOLEAN_MASK;
}

static inline bool bz_valueIsInteger(Value self) {
    return (self.val & (BZ_TAGGED_UPPER_VALUE_MASK | BZ_SIGN_MASK)) == BZ_INTEGER_MASK;
}

static inline bool bz_valueIsFloat(Value self) {
    return (self.val & BZ_TAGGED_VALUE_MASK) != BZ_TAGGED_VALUE_MASK;
}

static inline bool bz_valueIsNumber(Value self) {
    return bz_valueIsFloat(self) || bz_valueIsInteger(self);
}

static inline bool bz_valueIsObj(Value self) {
    return (self.val & BZ_POINTER_MASK) == BZ_POINTER_MASK;
}

static inline bool bz_valueIsNull(Value self) {
    return self.val == BZ_NULL_MASK;
}

static inline bool bz_valueIsVoid(Value self) {
    return self.val == BZ_VOID_MASK;
}

static inline bool bz_valueIsError(Value self) {
    return self.val == BZ_ERROR_MASK;
}

static inline bool bz_valueBoolean(Value self) {
    return self.val == BZ_TRUE_MASK;
}

static inline Integer bz_valueInteger(Value self) {
    const uint64_t payload = self.val & BZ_INTEGER_PAYLOAD_MASK;
    if ((payload & BZ_INTEGER_SIGN_BIT) != 0) {
        return -((Integer)((~payload + UINT64_C(1)) & BZ_INTEGER_PAYLOAD_MASK));
    }
    return (Integer)payload;
}

static inline Double bz_valueDouble(Value self) {
    Double value;
    memcpy(&value, &self.val, sizeof(value));
    return value;
}

static inline void *bz_valueObj(Value self) {
    return (void *)(uintptr_t)(self.val & ~BZ_POINTER_MASK);
}

struct NativeCtx {
    VM *vm;
    Init *process;
    Value *globals;
    void **upvalues;
    Value *base;
    Value **stack_top;
    Value callee;
    size_t arg_count;
};

struct TryCtx {
    TryCtx *previous;
    jmp_buf env;
};

#ifdef __cplusplus
extern "C" {
#endif

const uint8_t *bz_valueToString(Value value, size_t *len);
const char *bz_valueToCString(Value value);
uint8_t *bz_valueToForeignContainerPtr(Value value);
bool bz_valueIsForeignContainer(Value value);
void bz_valueDump(Value value, VM *vm);
Value bz_valueEqual(Value self, Value other);
Value bz_rangeContains(Value range, Value value);
Value bz_patternMatches(VM *vm, Value pattern, Value subject);
Value bz_valueIs(Value self, Value type_def);
Value bz_valueTypeOf(Value self, VM *vm);
uint64_t bz_getUserDataPtr(Value userdata);
size_t bz_containerTypeSize(Value container);
size_t bz_containerTypeAlign(Value type_def);
Value bz_valueCastToString(Value value, VM *vm);
Value bz_stringConcat(Value string, Value other, VM *vm);
Value bz_stringSubscript(Value obj_string, Value index_value, bool checked, VM *vm);
Value bz_stringNext(Value string_value, Value *index, VM *vm);
Value bz_rangeNext(Value range_value, Value index_slot);
Value bz_getRangeProperty(Value range_value, size_t property_idx, bool bind, VM *vm);
void bz_listAppend(Value list, Value value, VM *vm);
Value bz_listGet(Value list, int64_t index, bool checked);
void bz_listSet(Value list, size_t index, Value value, VM *vm);
size_t bz_listLen(Value list);
Value bz_listConcat(Value list, Value other_list, VM *vm);
Value bz_listNext(Value list_value, Value *index, VM *vm);
void bz_mapSet(Value map, Value key, Value value, VM *vm);
Value bz_mapGet(Value map, Value key);
Value bz_mapConcat(Value map, Value other_map, VM *vm);
Value bz_mapNext(Value map_value, Value *index);
void bz_setObjectInstanceProperty(Value instance_value, size_t property_idx, Value value, VM *vm);
Value bz_getObjectInstanceProperty(Value instance_value, size_t property_idx);
Value bz_getObjectInstanceMethod(Value instance_value, size_t method_idx, bool bind, VM *vm);
Value bz_getProtocolMethod(Value instance_value, Value method_name, VM *vm);
Value bz_getObjectField(Value object_value, size_t field_idx);
void bz_setObjectField(Value object_value, size_t field_idx, Value value, VM *vm);
Value bz_getEnumInstanceValue(Value enum_instance_value);
Value bz_getEnumCase(Value enum_value, Value case_name_value, VM *vm);
Value bz_getEnumCaseFromValue(Value enum_value, Value case_value, VM *vm);
Value bz_enumNext(Value enum_value, Value case_value, VM *vm);
Value bz_foreignContainerGet(Value value, size_t field_idx, VM *vm);
void bz_foreignContainerSet(Value value, size_t field_idx, Value new_value, VM *vm);
uint8_t *bz_foreignContainerSlice(Value container_value, size_t *len);

size_t bz_zigTypeSize(ZigType *self);
uint16_t bz_zigTypeAlignment(ZigType *self);
Value bz_zigTypeToCString(ZigType *self, VM *vm);

VM *bz_newVM(VM *host);
void bz_deinitVM(VM *self);
void bz_panic(VM *vm, const uint8_t *msg, size_t len);
bool bz_run(VM *self, const uint8_t *source, size_t source_len, const uint8_t *file_name, size_t file_name_len);
bool bz_call(VM *self, Value closure, const Value *const *arguments, size_t len, Value *catch_value);
void bz_push(VM *self, Value value);
Value bz_pop(VM *self);
Value bz_peek(VM *self, uint32_t distance);
Value bz_at(VM *vm, uint32_t at);
void bz_pushError(VM *self, const uint8_t *qualified_name, size_t len, const uint8_t *message, size_t mlen);
void bz_pushErrorEnum(VM *self, const uint8_t *qualified_name, size_t name_len, const uint8_t *case_name, size_t case_len);
Value bz_stringToValue(VM *vm, const uint8_t *string, size_t len);
Value bz_stringToValueZ(VM *vm, const char *string);
Value bz_newUserData(VM *vm, uint64_t userdata);
Value bz_serialize(VM *vm, Value value, Value *error_value);
void bz_throw(VM *vm, Value value);
void bz_rethrow(VM *vm);
Value bz_getQualified(VM *self, const uint8_t *qualified_name, size_t len);
size_t bz_allocated(VM *self);
void bz_collect(VM *self);
TryCtx *bz_setTryCtx(VM *self);
void bz_popTryCtx(VM *self);
void bz_closeUpValues(VM *vm, Value *last);
Value bz_getUpValue(NativeCtx *ctx, size_t slot);
void bz_setUpValue(NativeCtx *ctx, size_t slot, Value value);
Value bz_closure(NativeCtx *ctx, uint32_t function_node, void *native, void *native_raw);
Value bz_bindMethod(VM *vm, Value receiver, Value method_value, Value native_value);
void *bz_context(NativeCtx *ctx, Value closure_value, NativeCtx *new_ctx, size_t arg_count);
Value bz_clone(VM *vm, Value value);
Value bz_currentFiber(VM *vm);
void bz_dumpStack(VM *vm);
ZigType *bz_zigType(VM *vm, const uint8_t *ztype, size_t len, Value *expected_type);
Value bz_stringType(VM *vm);
Value bz_intType(VM *vm);
Value bz_mapType(VM *vm, Value key_type, Value value_type, bool mutable_type);
Value bz_listType(VM *vm, Value item_type, bool mutable_type);
Value bz_getStringProperty(VM *vm, Value string, size_t method_idx);
Value bz_getListProperty(VM *vm, Value list, size_t property_idx, bool bind);
Value bz_getMapProperty(VM *vm, Value map, size_t property_idx, bool bind);
Value bz_getPatternProperty(VM *vm, Value pattern, size_t property_idx);
Value bz_getFiberProperty(VM *vm, Value fiber, size_t property_idx);
Value bz_newRange(VM *vm, int64_t low, int64_t high);
Value bz_newList(VM *vm, Value list_type);
Value bz_newMap(VM *vm, Value map_type);
Value bz_newQualifiedObjectInstance(VM *self, const uint8_t *qualified_name, size_t len, bool mutable_type);
Value bz_newObjectInstance(VM *vm, Value object_value, Value typedef_value);
Value bz_newForeignContainerInstance(VM *vm, Value typedef_value);
Value bz_newForeignContainerFromSlice(VM *vm, Value type_def, uint8_t *ptr, size_t len);
Value bz_readZigValueFromBuffer(VM *vm, ZigType *ztype, size_t at, uint8_t *buf, size_t len);
void bz_writeZigValueToBuffer(VM *vm, Value value, const ZigType *ztype, size_t at, uint8_t *buf, size_t capacity);

void bz_memcpy(uint8_t *dest, size_t dest_len, uint8_t *source, size_t source_len);

#ifdef __cplusplus
}
#endif

#endif
