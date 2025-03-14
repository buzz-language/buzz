#ifndef BUZZ
#define BUZZ

#include <csetjmp>
#include <cstddef>
#include <cstdint>

typedef unsigned long int bz_Value;
typedef struct VM_s *VM;
typedef struct ZigType_s *ZigType;

struct NativeCtx {
  VM vm;
  bz_Value *globals;
  void *upvalues;
  bz_Value *base;
  bz_Value **stack_top;
};

struct TryCtx {
  TryCtx *previous;
  jmp_buf env;
};

extern const char* bz_valueToString(bz_Value value, size_t * len);
extern const char* bz_valueToCString(bz_Value value);
extern char* bz_valueToForeignContainerPtr(bz_Value value);
extern bool bz_valueIsForeignContainer(bz_Value value);
extern void bz_valueDump(bz_Value value, VM vm);
extern bz_Value bz_valueEqual(bz_Value value, bz_Value other);
extern bz_Value bz_valueIs(bz_Value self, bz_Value type_def);
extern bz_Value bz_valueTypeOf(bz_Value self, VM vm);
extern uint64_t bz_getUserDataPtr(bz_Value userdata);
extern size_t bz_containerTypeSize(bz_Value container);
extern size_t bz_containerTypeAlign(bz_Value type_def);
extern bz_Value bz_valueCastToString(bz_Value value, VM vm);
extern bz_Value bz_stringConcat(bz_Value string, bz_Value other, VM vm);
extern bz_Value bz_stringSubscript(bz_Value obj_string, bz_Value value, bool checked, VM vm);
extern bz_Value bz_stringNext(bz_Value value, bz_Value *index, VM vm);
extern bz_Value bz_rangeNext(bz_Value value, bz_Value index_slot);
extern bz_Value bz_getRangeProperty(bz_Value value, size_t property_idx, bool bind, VM vm);
extern void bz_listAppend(bz_Value list, bz_Value value, VM vm);
extern bz_Value bz_listGet(bz_Value list, int64_t index, bool checked);
extern void bz_listSet(bz_Value list, size_t index, bz_Value value, VM vm);
extern size_t bz_listLen(bz_Value list);
extern bz_Value bz_listConcat(bz_Value list, bz_Value other_list, VM vm);
extern bz_Value bz_listNext(bz_Value value, bz_Value *index, VM vm);
extern void bz_mapSet(bz_Value map, bz_Value key, bz_Value value, VM vm);
extern bz_Value bz_mapGet(bz_Value map, bz_Value key);
extern bz_Value bz_mapConcat(bz_Value map, bz_Value other_map, VM vm);
extern bz_Value bz_mapNext(bz_Value value, bz_Value *index);
extern void bz_setObjectInstanceProperty(bz_Value value, size_t property_idx, bz_Value property_value, VM vm);
extern bz_Value bz_getObjectInstanceProperty(bz_Value value, size_t property_idx);
extern bz_Value bz_getObjectInstanceMethod(bz_Value value, size_t method_idx, bool bind, VM vm);
extern bz_Value bz_getProtocolMethod(bz_Value value, bz_Value method_name, VM vm);
extern bz_Value bz_getObjectField(bz_Value value, size_t field_idx);
extern void bz_setObjectField(bz_Value value, size_t field_idx, bz_Value field_value, VM vm);
extern bz_Value bz_getEnumInstanceValue(bz_Value value);
extern bz_Value bz_getEnumCase(bz_Value value, bz_Value case_name_value, VM vm);
extern bz_Value bz_getEnumCaseFromValue(bz_Value value, bz_Value case_value, VM vm);
extern bz_Value bz_enumNext(bz_Value value, bz_Value case_value, VM vm);
extern bz_Value bz_foreignContainerGet(bz_Value value, size_t field_idx, VM vm);
extern void bz_foreignContainerSet(bz_Value value, size_t field_idx, bz_Value field_value, VM vm);
extern char* bz_foreignContainerSlice(bz_Value value, size_t *len);

extern size_t bz_zigTypeSize(ZigType zig_type);
extern uint16_t bz_zigTypeAlignment(ZigType zig_type);
extern const char* bz_zigTypeToCString(ZigType zig_type, VM vm);

#endif
