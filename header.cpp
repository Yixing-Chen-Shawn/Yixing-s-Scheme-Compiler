#include "header.h"

extern "C" {

/// alloc amt ScmObj's.
/// if you do alloc(sizeof(X)) then youre doing something wrong!!!
/// Use alloc when you need to allocate a ScmObj with a pointer in it
/// This includes Cons, Vector, Hash, Closure, etc.
/// STUDENT TODO: THIS LEAKS MEMORY! ADD GARBAGE COLLECTION SUPPORT
///               IF YOU WANT TO NOT LEAK MEMORY!
///               Stack allocate all objects in LLVM
///               (instead of relying on implict allocation from registers)
///               And utilize libgc, GC_ALLOC.
ScmObj* alloc(const u64 amt) {
    return new ScmObj[amt];
}

////////////// Objects

///// Closures

/// Closure Memory Layout
/// It is 2 ScmObj laid out besides eachother.
/// The first ScmObj (index 0) is an Other type and it has a ptrvalue of the function pointer
/// The second ScmObj (index 1) is the vector that holds the environment (free variables).
ScmObj* closure_alloc(const s64 amt_freevars, u64 fptr) {
    ScmObj* clo_obj = alloc(2);
    ScmObj lam_part, env_part;

    // use Other type for the constituents of the Closure
    // as they are internal to the Closure object itself.
    lam_part.type = Other;
    lam_part.valueptr = reinterpret_cast<u64*>(fptr);
    env_part.type = Other;
    env_part.valueptr = reinterpret_cast<u64*>(prim_make_45vector(const_init_int(amt_freevars),
                                                                  const_init_int(0)));
    clo_obj[0] = lam_part;
    clo_obj[1] = env_part;

    ScmObj* ret = alloc(1);
    ret->type = Closure;
    ret->valueptr = reinterpret_cast<u64*>(clo_obj);
    return ret;
}

/// When creating a closure, we need to store the function's free variables into the environment.
/// This takes the closure, the value of the free variable, and the position
/// of the variable in the environment, and places it into the closure.
void closure_place_freevar(ScmObj* clo, ScmObj* freevar, s64 pos) {
    // unwrap handles asserting
    ScmObj* clo_obj = unwrap_clo(clo, "closure_place_freevar");

    ScmObj* vec = reinterpret_cast<ScmObj*>(clo_obj[1].valueptr);
    ScmObj* pos_obj = const_init_int(pos);

    prim_vector_45set_33(vec, pos_obj, freevar);
}


/// Returns a u64, which can be `inttoptr`d/`reinterpret`d
/// into a (void (ScmObj*, ScmObj*)) function pointer.
proc_ptr closure_get_fn_part(ScmObj* clo) {
    ScmObj* clo_obj = unwrap_clo(clo, "closure_get_fn_part");
    return reinterpret_cast<proc_ptr>(clo_obj[0].valueptr);
}

ScmObj* closure_env_get(ScmObj* clo, s64 pos) {
    // unwrap handles asserting
    ScmObj* clo_obj = unwrap_clo(clo, "closure_env_get");
    ScmObj* vec = reinterpret_cast<ScmObj*>(clo_obj[1].valueptr);
    return prim_vector_45ref(vec, const_init_int(static_cast<s64>(pos)));
}


ScmObj* unwrap_cons(ScmObj* cons_obj, const char* fn) {
    ASSERT_TYPE(*cons_obj, Cons, "unwrap_cons takes a Cons object! Got %d in fn %s",
                cons_obj->type, fn);
    return reinterpret_cast<ScmObj*>(cons_obj->valueptr);
}

ScmObj* unwrap_vector(ScmObj* vec_obj, const char* fn) {
    ASSERT_TYPE(*vec_obj, Vector, "unwrap_vector takes a Vector object! Got %d in fn %s",
                vec_obj->type, fn);
    return reinterpret_cast<ScmObj*>(vec_obj->valueptr);
}

ScmObj* unwrap_clo(ScmObj* clo_obj, const char* fn) {
    ASSERT_TYPE(*clo_obj, Closure, "unwrap_clo takes a Closure object! Got %d in fn %s",
                clo_obj->type, fn);
    return reinterpret_cast<ScmObj*>(clo_obj->valueptr);
}

s64 unwrap_int(ScmObj* int_obj, const char* fn) {
    ASSERT_TYPE(*int_obj, Int, "unwrap_int takes an Int object! Got %d in fn %s",
                int_obj->type, fn);
    return reinterpret_cast<s64>(int_obj->valueptr);
}

u64 unwrap_bool(ScmObj* bool_obj, const char* fn) {
    ASSERT_TYPE(*bool_obj, Bool, "unwrap_bool takes a Bool object! Got %d in fn %s",
                bool_obj->type, fn);
    return reinterpret_cast<u64>(bool_obj->valueptr);
}

char* unwrap_str(ScmObj* str_obj, const char* fn) {
    ASSERT_TYPE(*str_obj, Str, "unwrap_str takes a Str object! Got %d in fn %s",
                str_obj->type, fn);
    return reinterpret_cast<char*>(str_obj->valueptr);
}

char* unwrap_sym(ScmObj* sym_obj, const char* fn) {
    ASSERT_TYPE(*sym_obj, Sym, "unwrap_sym takes a Sym object! Got %d in fn %s",
                sym_obj->type, fn);
    return reinterpret_cast<char*>(sym_obj->valueptr);
}

u64 is_truthy_value(ScmObj* obj) {
    return (obj->type == Bool && (unwrap_bool(obj, "is_truthy_value") == false))
            ? false : true;
}

ScmObj* const_init_void() {
    ScmObj* ret = alloc(1);
    ret->valueptr = nullptr;
    ret->type = Void;
    return ret;
}

ScmObj* const_init_null() {
    ScmObj* ret = alloc(1);
    ret->valueptr = nullptr;
    ret->type = Null;
    return ret;
}

ScmObj* const_init_true() {
    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(true);
    ret->type = Bool;
    return ret;
}

ScmObj* const_init_false() {
    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(false);
    ret->type = Bool;
    return ret;
}

ScmObj* const_init_int(s64 i) {
    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(i);
    ret->type = Int;
    return ret;
}

ScmObj* const_init_string(char* s) {
    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(s);
    ret->type = Str;
    return ret;
}

ScmObj* const_init_symbol(char* s) {
    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(s);
    ret->type = Sym;
    return ret;
}

////// Printing

GEN_EXPECT1ARGLIST(applyprim_display, prim_display)
ScmObj* prim_display(ScmObj* obj) {
    // forward to `print`
    return prim_print(obj);
}

GEN_EXPECT1ARGLIST(applyprim_print, prim_print)
ScmObj* prim_print(ScmObj* obj) {
    ScmType typ = obj->type;

    switch (typ) {
    case Null:
        printf("'()");
        break;
    case Sym:
        printf("'%s", unwrap_sym(obj, "prim_print Sym case."));
        break;
    case Void:
        // print nothing on void.
        break;
    case Cons:
        printf("'(");
        print_cons(obj);
        printf(")");
        break;
    case Bool:
    case Closure:
    case Int:
    case Str:
    case Vector:
    case Other:
        prim_print_aux(obj);
        break;
    }
    return const_init_void();
}

GEN_EXPECT1ARGLIST(applyprim_println, prim_println)
ScmObj* prim_println(ScmObj* obj) {
    prim_print(obj);
    printf("\n");
    return const_init_void();
}

ScmObj* prim_print_aux(ScmObj* obj) {
    ScmType typ = obj->type;

    switch (typ) {
    case Void:
        printf("#<void>");
        break;
    case Null:
        printf("()");
        break;
    case Bool: {
        u64 bv = unwrap_bool(obj, "prim_print_aux bool case");
        if (bv == 0) {
            printf("#f");
        } else if (bv == 1) {
            printf("#t");
        } else {
            printf("Unknown Boolean value: %lu", bv);
        }
    }
        break;
    case Closure:
        printf("#<procedure>");
        break;
    case Cons:
        printf("(");
        print_cons(obj);
        printf(")");
        break;
    case Int:
        printf("%ld", unwrap_int(obj, "prim_print_aux Int case."));
        break;
    case Str:
        printf("%s", unwrap_str(obj, "prim_print_aux Str case."));
        break;
    case Sym:
        printf("%s", unwrap_sym(obj, "prim_print_aux Sym case."));
        break;
    case Vector:
        print_vector(obj);
        break;
    case Other:
        printf("(print v); unrecognized value %lu", reinterpret_cast<u64>(obj->valueptr));
        break;
    }
    return const_init_void();
}

ScmObj* print_cons(ScmObj* obj) {
    ScmObj* cons = unwrap_cons(obj, "print_cons");
    ScmObj* car = &cons[0];
    ScmObj* cdr = &cons[1];

    prim_print_aux(car);

    switch (cdr->type) {
    case Null:
        break;
    case Void:
        printf("#<void>");
        break;
    case Cons:
        printf(" ");
        print_cons(cdr);
        break;
    case Sym:
    case Bool:
    case Closure:
    case Int:
    case Str:
    case Vector:
    case Other:
        printf(" . ");
        prim_print_aux(cdr);
        break;
    }
    return const_init_void();
}

ScmObj* print_vector(ScmObj* obj) {
    ScmObj* vector = unwrap_vector(obj, "print_vector");
    u64 len = _get_vector_length(obj);
    printf("#("); // looks like a sad face :P
    for (u64 i = 1; i <= len; i++) {
        if (i != 1) {
            printf(" ");
        }
        ScmObj* inner = reinterpret_cast<ScmObj*>(vector[i].valueptr);
        prim_print_aux(inner);
    }
    printf(")");

    return const_init_void();
}


const char* get_type_name(ScmType type) {
    switch (type) {
    case Void:
        return "Void";
    case Null:
        return "Null";
    case Bool:
        return "Bool";
    case Closure:
        return "Closure";
    case Cons:
        return "Cons";
    case Int:
        return "Int";
    case Str:
        return "String";
    case Sym:
        return "Symbol";
    case Vector:
        return "Vector";
    case Other:
        return "Other";
    }
}


// Primitives

GEN_EXPECT1ARGLIST(applyprim_halt, prim_halt)
ScmObj* prim_halt(ScmObj* val) { // halt
    prim_print(val);
    if (val->type != Void) {
        printf("\n");
    }
    exit(0);
}


u64 _get_vector_length(ScmObj* obj) {
    ScmObj* vec_obj = unwrap_vector(obj, "get_vector_length");
    return reinterpret_cast<u64>(vec_obj[0].valueptr);
}

/**
  * Gets the car and cdr of the given lst (the first argument)
  * And puts them into the 2nd and 3rd arguments.
  */
void _get_both(ScmObj* lst, ScmObj* car, ScmObj* cdr) {
    ScmObj* cons_obj = unwrap_cons(lst, "_get_both");
    // this is safe because the lifetimes are the same
    // the lifetime of lst >= the lifetimes of car and cdr.
    *car = cons_obj[0];
    *cdr = cons_obj[1];
}

/// Vector layout is as so
/// vector->type = Vector
/// vector->valueptr = raw_vector
/// raw_vector is a list of ScmObj
/// position 0 is Int,len (being the amount of ScmObj to the right of this)
/// position i (1-indexed) is Other,ScmObj*

ScmObj* applyprim_vector(ScmObj* curptr) { // apply vector
    ScmObj cur = *curptr;
    // TODO: Support for larger vectors?
    ScmObj* buf[256] = {nullptr};
    u64 i = 0;

    while (cur.type == Cons && i < 256) {
        ScmObj car, cdr;
        _get_both(&cur, &car, &cdr);
        buf[i] = alloc(1);
        buf[i]->type = car.type;
        buf[i++]->valueptr = car.valueptr;
        cur = cdr;
    }
    if (i == 256 && cur.type != Null) {
        fatal_err("Vectors larger than 256 elements are unimplemented. Sorry!");
    }

    // i is amount of elements, add 1 for length at pos 0.
    ScmObj* mem = alloc(i+1);

    mem[0].type = Int;
    mem[0].valueptr = reinterpret_cast<u64*>(i);
    for (u64 j = 0; j < i; j++) {
        mem[j+1].type = Other;
        mem[j+1].valueptr = reinterpret_cast<u64*>(buf[j]);
    }

    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(mem);
    ret->type = Vector;
    return ret;
}

GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)
ScmObj* prim_make_45vector(ScmObj* length_obj, ScmObj* fill) { // make-vector

    u64 len = static_cast<u64>(unwrap_int(length_obj, "make-vector"));
     // i is amount of elements, + 1 is 1 object for size.
    ScmObj* vec = alloc(1 + len);

    vec[0].type = Int;
    vec[0].valueptr = reinterpret_cast<u64*>(len);
    for (u64 i = 1; i <= len; i++) {
        // copy the fill var
        ScmObj* cur = alloc(1);
        cur->type = fill->type;
        cur->valueptr = fill->valueptr;

        vec[i].type = Other;
        vec[i].valueptr = reinterpret_cast<u64*>(cur);
    }

    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(vec);
    ret->type = Vector;
    return ret;
}

GEN_EXPECT1ARGLIST(applyprim_vector_63, prim_vector_63)
ScmObj* prim_vector_63(ScmObj* maybevec) {
    return (maybevec->type == Vector)
        ? const_init_true()
        : const_init_false();
}


GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)
ScmObj* prim_vector_45length(ScmObj* vec) { // vector-length
    ScmObj* raw_vector = unwrap_vector(vec, "prim_vector_45length");
    return &raw_vector[0];
}

void bounds_check(ScmObj* raw_vector, s64 pos) {
    s64 length = unwrap_int(&raw_vector[0], "bounds check");
    if (pos + 1 > length) {
        fatal_errf("Bounds check fail, wanted pos %ld, only %ld elements", pos, length);
    }
}

GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)
ScmObj* prim_vector_45ref(ScmObj* vector, ScmObj* pos) { // vector-ref

    ScmObj* vec_raw = unwrap_vector(vector, "vector-ref");
    s64 index_pos = unwrap_int(pos, "vector-ref index_pos");
    bounds_check(vec_raw, index_pos);

    // + 1 becuase length is at index 0, so need to push everything up 1.
    return reinterpret_cast<ScmObj*>(vec_raw[index_pos + 1].valueptr);
}


GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)
ScmObj* prim_vector_45set_33(ScmObj* vector, ScmObj* pos, ScmObj* val) { // vector-set!

    ScmObj* vec_raw = unwrap_vector(vector, "vector-set!");
    s64 index_pos = unwrap_int(pos, "apply vector-set! index_pos");
    bounds_check(vec_raw, index_pos);

    ScmObj* copy = alloc(1);
    copy->type = val->type;
    copy->valueptr = val->valueptr;

    // + 1 becuase length is index 0, so need to push everything up 1.
    vec_raw[index_pos + 1].valueptr = reinterpret_cast<u64*>(copy);
    return const_init_void();
}


/// Returns a ScmObj of type Void

GEN_EXPECT0ARGLIST(applyprim_void, prim_void)
ScmObj* prim_void() { // void
    return const_init_void();
}

///// eq? eqv? equal?


// it is assumed that if this is called,
// a and b MUST both be Cons objects.
int cons_eq_helper(ScmObj* a, ScmObj* b) {
    ScmObj acar, acdr, bcar, bcdr;

    _get_both(a, &acar, &acdr);
    _get_both(b, &bcar, &bcdr);

    return (eq_helper(&acar, &bcar) == 1) && (eq_helper(&acdr, &bcdr));
}


int vec_eq_helper(ScmObj* a, ScmObj* b) {
    ScmObj* avec = unwrap_vector(a, "vec_eq_helper avec");
    ScmObj* bvec = unwrap_vector(b, "vec_eq_helper bvec");
    // choose either vector it doesnt matter which we use for length.
    s64 alen = unwrap_int(&avec[0], "vec_eq_helper alen");
    s64 blen = unwrap_int(&bvec[0], "vec_eq_helper blen");

    if (alen != blen) {
        return false;
    }

    // if the vectors are of different length, it will be caught on first iteration.
    // else, they it will work perfectly.
    for (s64 i=0; i <= alen; i++) {
        if (eq_helper(&avec[i], &bvec[i]) == 0) {
            return 0;
        }
    }
    // if every object in the vector is equal, it must be equal.
    return 1;
}

// Returns 0 for unequal, and 1 for equal.
int eq_helper(ScmObj* a, ScmObj* b) {
    if (a->type != b->type) {
        return 0;
    }

    switch (a->type) {
    case Void:
        return 1;
    case Null:
        return 1;
    case Bool:
        if (unwrap_bool(a, "prim_eq_63 Bool a") == unwrap_bool(b, "prim_eq_63 Bool b")) {
            return 1;
        } else {
            return 0;
        }
    case Closure:
        return 0;
    case Cons:
        return cons_eq_helper(a, b);
    case Int:
        if (unwrap_int(a, "prim_eq_63 Int a") == unwrap_int(b, "prim_eq_63 Int b")) {
            return 1;
        } else {
            return 0;
        }
    case Str:
    {
        char* astr = unwrap_str(a, "prim_eq_63 Str a");
        char* bstr = unwrap_str(b, "prim_eq_63 Str b");
        if (strcmp(astr, bstr) == 0) {
            return 1;
        } else {
            return 0;
        }
    }
    case Sym:
    {
        char* asym = unwrap_sym(a, "prim_eq_63 Sym a");
        char* bsym = unwrap_sym(b, "prim_eq_63 Sym b");
        if (strcmp(asym, bsym) == 0) {
            return 1;
        } else {
            return 0;
        }
    }
    case Vector:
        return vec_eq_helper(a, b);
    case Other:
        {fatal_err("I do not know what youre trying to eq?!!.")}
    }
}

// Does not conform to Rackets eq? probably. PRs welcome to do that :)))
// But this is a scheme... so i just need to read the standard...
// Takes in 2 ScmObj* and tells if they are equal.
/// Returns a ScmObj* of type Bool
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)
ScmObj* prim_eq_63(ScmObj* a, ScmObj* b) { // eq?
    return make_predicate(eq_helper(a, b) == 1);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)
ScmObj* prim_eqv_63(ScmObj* a, ScmObj* b) { // eqv?
    return prim_eq_63(a, b); // TODO eqv? != eq?
}

/// Returns a ScmObj of type Bool
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
ScmObj* prim_equal_63(ScmObj* a, ScmObj* b) { // equal?
    return prim_eq_63(a, b); // TODO equal? != eq?
}

////// Other Predicates

ScmObj* make_predicate(bool b) {
    return b ? const_init_true() : const_init_false();
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)
ScmObj* prim_number_63(ScmObj* n) { // number?
    return prim_integer_63(n);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)
ScmObj* prim_integer_63(ScmObj* n) { // integer?
    return make_predicate(n->type == Int);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_boolean_63, prim_boolean_63)
ScmObj* prim_boolean_63(ScmObj* b) { // boolean?
    return make_predicate(b->type == Bool);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)
ScmObj* prim_void_63(ScmObj* obj) { // void?
    return make_predicate(obj->type == Void);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)
ScmObj* prim_procedure_63(ScmObj* obj) { // procedure?
    return make_predicate(obj->type == Closure);
}

// null? cons? cons car cdr

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)
ScmObj* prim_null_63(ScmObj* obj) { // null?
    return make_predicate(obj->type == Null);
}

/// Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)
ScmObj* prim_cons_63(ScmObj* obj) { // cons?
    return make_predicate(obj->type == Cons);
}

/// Returns a ScmObj of type Cons
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)
ScmObj* prim_cons(ScmObj* car, ScmObj* cdr) { // cons
    ScmObj* ptr = alloc(2);
    // unsure of safety of this.
    ptr[0] = *car;
    ptr[1] = *cdr;

    ScmObj* ret = alloc(1);
    ret->valueptr = reinterpret_cast<u64*>(ptr);
    ret->type = Cons;
    return ret;
}

/// Takes a Cons and returns its car
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)
ScmObj* prim_car(ScmObj* cons_obj) { // car
    ScmObj* cons = unwrap_cons(cons_obj, "car");
    return &cons[0];
}


/// Takes a Cons and returns its cdr
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)
ScmObj* prim_cdr(ScmObj* cons_obj) { // cdr
    ScmObj* cons = unwrap_cons(cons_obj, "cdr");
    return &cons[1];
}

/// Returns a ScmObj of type Int
ScmObj* prim__43(ScmObj* a, ScmObj* b) { // +
    s64 a_val = unwrap_int(a, "+ a");
    s64 b_val = unwrap_int(b, "+ b");
    return const_init_int(a_val + b_val);
}

// Takes a Cons object
// (called cur becuase we loop thru it and use the same binding)
ScmObj* applyprim__43(ScmObj* cur) { // apply +
    ScmType typ = cur->type;
    if (typ != Cons && typ != Null) {
        fatal_errf("Expected Cons in apply +, but got %s", get_type_name(typ));
    }

    s64 final = 0;

    while (cur->type == Cons) {
        ScmObj car, cdr;
        _get_both(cur, &car, &cdr);
        final += unwrap_int(&car, "apply + final");
        cur = &cdr;
    }
    return const_init_int(final);
}

/// Returns a ScmObj of type Int
ScmObj* prim__45(ScmObj* a, ScmObj* b) { // -
    // a - b
    s64 a_val = unwrap_int(a, "- a");
    s64 b_val = unwrap_int(b, "- b");

    return const_init_int(a_val - b_val);
}

ScmObj* applyprim__45(ScmObj* list) { // apply -
    ScmObj* cons_obj = unwrap_cons(list, "apply -");
    ScmObj car = cons_obj[0];
    ScmObj cdr = cons_obj[1];

    s64 carval = unwrap_int(&car, "apply - carval");

    if (cdr.type == Null) {
        return const_init_int(0 - carval);
    }

    s64 final = carval;
    ScmObj cur = cdr;
    while (cur.type != Null) {
        ScmObj cur_car, cur_cdr;

        _get_both(&cur, &cur_car, &cur_cdr);
        final -= unwrap_int(&cur_car, "applyprim__45 final");
        cur = cur_cdr;
    }
    return const_init_int(final);
}


/// Returns a ScmObj of type Int
ScmObj* prim__42(ScmObj* a, ScmObj* b) { // *
    // a - b
    return const_init_int(unwrap_int(a, "* a") * unwrap_int(b, "* b"));
}

ScmObj* applyprim__42(ScmObj* list) { // apply *
    if (list->type == Null) {
        return const_init_int(1);
    } else if (list->type == Cons) {
        ScmObj* cons_obj = unwrap_cons(list, "apply * cons_obj");
        ScmObj car = cons_obj[0];
        ScmObj cdr = cons_obj[1];
        s64 carval = unwrap_int(&car, "apply * carval");
        s64 cdrval = unwrap_int(applyprim__42(&cdr), "apply * cdrval");
        return const_init_int(carval * cdrval);
    } else {
        fatal_err("apply * taking a non-list argument!");
    }
}

/// Returns a ScmObj of type Int
ScmObj* prim__47(ScmObj* a, ScmObj* b) { // /
    s64 a_val = unwrap_int(a, "/ a");
    s64 b_val = unwrap_int(b, "/ b");
    return const_init_int(a_val / b_val);
}

// ScmObj applyprim__47(ScmObj list) { // apply /
//     ASSERT_TYPE(list, Cons, "apply / must take a list as argument.");
//     {fatal_err("unimplemented...");}
//     // original header does not implement this...
// }

/// Takes 2 ScmObj's of type Int
/// Returns a ScmObj of type Bool
GEN_EXPECT2ARGLIST(applyprim__61, prim__61)
ScmObj* prim__61(ScmObj* a, ScmObj* b) { // =
    if (unwrap_int(a, "= a") == unwrap_int(b, "= b")) {
        return const_init_true();
    } else {
        return const_init_false();
    }
}

/// Takes 2 ScmObj's of type Int
/// Returns a ScmObj of type Bool
ScmObj* prim__60(ScmObj* a , ScmObj* b) { // <
    return make_predicate(unwrap_int(a, "< a") < unwrap_int(b, "< b"));
}

/// Takes 2 ScmObj's of type Int
/// Returns a ScmObj of type Bool
GEN_EXPECT2ARGLIST(applyprim__60_61, prim__60_61)
ScmObj* prim__60_61(ScmObj* a, ScmObj* b) { // <=
    return make_predicate(unwrap_int(a, "<= a") <= unwrap_int(b, "<= b"));
}

/// Takes a ScmObj of type Bool Returns a ScmObj of type Bool
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)
ScmObj* prim_not(ScmObj* b) { // not
    return make_predicate(unwrap_bool(b, "not") == false);
}


} // end extern "C"


