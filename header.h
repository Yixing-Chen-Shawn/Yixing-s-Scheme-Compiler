#pragma once
// Header file for Scheme Runtime
// compile this to LLVM, concat it to the generated LLVM output,
// and then compile that.

#include "stdio.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define ASSERT_TYPE(obj,expected,msg,...) if (((obj).type) != (expected)) {fatal_errf(msg, __VA_ARGS__);}

#define fatal_err(format) { \
        printf("Fatal library run-time error: "); \
        printf(format); \
        printf("\n"); \
        exit(1); \
    }

#define fatal_errf(format, ...) { \
        printf("Fatal library run-time error: "); \
        printf(format, __VA_ARGS__); \
        printf("\n"); \
        exit(1); \
    }

#define GEN_EXPECT0ARGLIST(newname, fn_name) \
    ScmObj* newname(ScmObj* lst) { \
        if (lst->type != Null) {fatal_errf("Expected an empty list but got something else for function '%s'", #newname);} \
        return fn_name(); \
    }

#define GEN_EXPECT1ARGLIST(newname, fn_name) \
    ScmObj* newname(ScmObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        ScmObj* cons_obj = unwrap_cons(lst, #newname); \
        ScmObj car = cons_obj[0]; \
        ScmObj cdr = cons_obj[1]; \
        if (cdr.type != Null) {fatal_errf("function '%s' only takes 1 argument.", #newname );} \
        return fn_name(&car); \
    }

#define GEN_EXPECT2ARGLIST(newname, fn_name) \
    ScmObj* newname(ScmObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        ScmObj* cons_obj = unwrap_cons(lst, #newname); \
        ScmObj car = cons_obj[0]; \
        ScmObj cdr = cons_obj[1]; \
        if (cdr.type != Cons) {fatal_errf("Function '%s' expected 2 arguments but got 1.", #newname );} \
        ScmObj* cdr_obj = unwrap_cons(&cdr, #newname); \
        ScmObj cadr = cdr_obj[0]; \
        ScmObj cddr = cdr_obj[1]; \
        if (cddr.type != Null) {fatal_errf("Function '%s' only takes 2 arguments.", #newname );} \
        return fn_name(&car, &cadr); \
    }

#define GEN_EXPECT3ARGLIST(newname, fn_name) \
    ScmObj* newname(ScmObj* lst) { \
        if (lst->type != Cons) {fatal_errf("Expected cons but got something else for function '%s'", #newname);} \
        ScmObj* cons_obj = unwrap_cons(lst, #newname); \
        ScmObj car = cons_obj[0]; \
        ScmObj cdr = cons_obj[1]; \
        if (cdr.type != Cons) {fatal_errf("Function '%s' expected 3 arguments but got 1.", #newname );} \
        ScmObj* cdr_obj = unwrap_cons(&cdr, #newname); \
        ScmObj cadr = cdr_obj[0]; \
        ScmObj cddr = cdr_obj[1]; \
        if (cddr.type != Cons) {fatal_errf("Function '%s' expected 3 arguments but got 2.", #newname );} \
        ScmObj* cddr_object = unwrap_cons(&cddr, #newname); \
        ScmObj caddr = cddr_object[0]; \
        ScmObj cdddr = cddr_object[1]; \
        if (cdddr.type != Null) {fatal_errf("Function '%s' only takes 3 arguments.", #newname );} \
        return fn_name(&car, &cadr, &caddr); \
    }

extern "C" {
typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;

typedef enum {  Void=0, Null, Bool,
                Closure, Cons, Int,
                Str, Sym, Vector, Other} ScmType;


#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpadded"
// ignore padded because its just sayng that its gonna pad, and thats OK.
typedef struct ScmObj {
    u64* valueptr;
    ScmType type;
} ScmObj;
#pragma clang diagnostic pop


// used in closure_get_fn_part
typedef void (*proc_ptr)(ScmObj*,ScmObj*);

void start_program();
ScmObj* alloc(const u64);
ScmObj* make_predicate(bool);

ScmObj* closure_alloc(const s64 amt_freevars, u64 cloval);
void closure_place_freevar(ScmObj* clo, ScmObj* freevar, s64 pos);
proc_ptr closure_get_fn_part(ScmObj*);
ScmObj* closure_env_get(ScmObj* clo, s64 pos);

ScmObj* const_init_int(s64);
ScmObj* const_init_void();
ScmObj* const_init_null();
ScmObj* const_init_true();
ScmObj* const_init_false();
ScmObj* const_init_string(char*);
ScmObj* const_init_symbol(char*);


ScmObj* unwrap_cons(ScmObj*, const char*);
ScmObj* unwrap_vector(ScmObj*, const char*);
ScmObj* unwrap_clo(ScmObj*, const char*);
s64 unwrap_int(ScmObj*, const char*);
char* unwrap_str(ScmObj*, const char*);
char* unwrap_sym(ScmObj*, const char*);
u64 unwrap_bool(ScmObj*, const char*);


// utility

int eq_helper(ScmObj*, ScmObj*);
int cons_eq_helper(ScmObj* a, ScmObj* b);
int vec_eq_helper(ScmObj* a, ScmObj* b);

void bounds_check(ScmObj*, s64);
u64 _get_vector_length(ScmObj*);
const char* get_type_name(ScmType);
void _get_both(ScmObj*, ScmObj*, ScmObj*);
u64 is_truthy_value(ScmObj*);
ScmObj* prim_print_aux(ScmObj*);

// primitives in no particular order...

ScmObj* print_cons(ScmObj*);
ScmObj* print_vector(ScmObj*);
ScmObj* prim_display(ScmObj*);
ScmObj* applyprim_display(ScmObj*);
ScmObj* prim_print(ScmObj*);
ScmObj* applyprim_print(ScmObj*);
ScmObj* prim_println(ScmObj*);
ScmObj* applyprim_println(ScmObj*);
ScmObj* prim_equal_63(ScmObj*,ScmObj*);
ScmObj* applyprim_equal_63(ScmObj*);
ScmObj* prim_number_63(ScmObj*);
ScmObj* applyprim_number_63(ScmObj*);
ScmObj* prim_integer_63(ScmObj*);
ScmObj* applyprim_integer_63(ScmObj*);
ScmObj* prim_boolean_63(ScmObj*);
ScmObj* applyprim_boolean_63(ScmObj*);
ScmObj* prim_cdr(ScmObj*);
ScmObj* applyprim_cdr(ScmObj*);
ScmObj* prim__42(ScmObj*, ScmObj*);
ScmObj* applyprim__42(ScmObj*);
ScmObj* prim__43(ScmObj*, ScmObj*);
ScmObj* applyprim__43(ScmObj*);
ScmObj* prim__45(ScmObj*, ScmObj*);
ScmObj* applyprim__45(ScmObj*);
ScmObj* prim_car(ScmObj*);
ScmObj* applyprim_car(ScmObj*);
ScmObj* prim_cdr(ScmObj*);
ScmObj* applyprim_cdr(ScmObj*);
ScmObj* prim_cons_63(ScmObj*);
ScmObj* applyprim_cons_63(ScmObj*);
ScmObj* prim_cons(ScmObj*, ScmObj*);
ScmObj* applyprim_cons(ScmObj*);
ScmObj* prim_procedure_63(ScmObj*);
ScmObj* applyprim_procedure_63(ScmObj*);
ScmObj* prim_null_63(ScmObj*);
ScmObj* applyprim_null_63(ScmObj*);
ScmObj* prim_eqv_63(ScmObj*, ScmObj*);
ScmObj* applyprim_eqv_63(ScmObj*);
ScmObj* prim_void_63(ScmObj*);
ScmObj* applyprim_void_63(ScmObj*);
ScmObj* prim_eq_63(ScmObj*, ScmObj*);
ScmObj* applyprim_eq_63(ScmObj*);
ScmObj* prim_void();
ScmObj* applyprim_void(ScmObj*);
ScmObj* prim_halt(ScmObj*);
ScmObj* applyprim_halt(ScmObj*);
ScmObj* prim_vector_63(ScmObj*);
ScmObj* applyprim_vector_63(ScmObj*);
ScmObj* prim_vector_45length(ScmObj*);
ScmObj* applyprim_vector_45length(ScmObj*);
ScmObj* prim_vector_45set_33(ScmObj*, ScmObj*, ScmObj*);
ScmObj* applyprim_vector_45set_33(ScmObj*);
ScmObj* prim_vector_45ref(ScmObj*, ScmObj*);
ScmObj* applyprim_vector_45ref(ScmObj*);
ScmObj* applyprim_vector(ScmObj*);
ScmObj* prim_make_45vector(ScmObj*, ScmObj*);
ScmObj* applyprim_make_45vector(ScmObj*);


ScmObj* applyprim__43(ScmObj*);
ScmObj* prim__45(ScmObj*, ScmObj*);
ScmObj* applyprim__45(ScmObj*);
ScmObj* applyprim__42(ScmObj*);
ScmObj* prim__47(ScmObj*, ScmObj*);
ScmObj* applyprim__61(ScmObj*);
ScmObj* prim__61(ScmObj*, ScmObj*);
ScmObj* prim__60(ScmObj* , ScmObj*);
ScmObj* applyprim__60_61(ScmObj*);
ScmObj* prim__60_61(ScmObj*, ScmObj*);
ScmObj* prim_not(ScmObj*);
ScmObj* applyprim_not(ScmObj*);

} // end extern "C"
