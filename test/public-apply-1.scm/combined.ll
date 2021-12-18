; ModuleID = '../../header.cpp'
source_filename = "../../header.cpp"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx12.0.0"

%struct.ScmObj = type { i64*, i32 }

@.str = private unnamed_addr constant [22 x i8] c"closure_place_freevar\00", align 1
@.str.1 = private unnamed_addr constant [20 x i8] c"closure_get_fn_part\00", align 1
@.str.2 = private unnamed_addr constant [16 x i8] c"closure_env_get\00", align 1
@.str.3 = private unnamed_addr constant [31 x i8] c"Fatal library run-time error: \00", align 1
@.str.4 = private unnamed_addr constant [49 x i8] c"unwrap_cons takes a Cons object! Got %d in fn %s\00", align 1
@.str.5 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.6 = private unnamed_addr constant [53 x i8] c"unwrap_vector takes a Vector object! Got %d in fn %s\00", align 1
@.str.7 = private unnamed_addr constant [51 x i8] c"unwrap_clo takes a Closure object! Got %d in fn %s\00", align 1
@.str.8 = private unnamed_addr constant [48 x i8] c"unwrap_int takes an Int object! Got %d in fn %s\00", align 1
@.str.9 = private unnamed_addr constant [49 x i8] c"unwrap_bool takes a Bool object! Got %d in fn %s\00", align 1
@.str.10 = private unnamed_addr constant [47 x i8] c"unwrap_str takes a Str object! Got %d in fn %s\00", align 1
@.str.11 = private unnamed_addr constant [47 x i8] c"unwrap_sym takes a Sym object! Got %d in fn %s\00", align 1
@.str.12 = private unnamed_addr constant [16 x i8] c"is_truthy_value\00", align 1
@.str.13 = private unnamed_addr constant [55 x i8] c"Expected cons but got something else for function '%s'\00", align 1
@.str.14 = private unnamed_addr constant [18 x i8] c"applyprim_display\00", align 1
@.str.15 = private unnamed_addr constant [37 x i8] c"function '%s' only takes 1 argument.\00", align 1
@.str.16 = private unnamed_addr constant [16 x i8] c"applyprim_print\00", align 1
@.str.17 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.18 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.19 = private unnamed_addr constant [21 x i8] c"prim_print Sym case.\00", align 1
@.str.20 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.21 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.22 = private unnamed_addr constant [18 x i8] c"applyprim_println\00", align 1
@.str.23 = private unnamed_addr constant [8 x i8] c"#<void>\00", align 1
@.str.24 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.25 = private unnamed_addr constant [25 x i8] c"prim_print_aux bool case\00", align 1
@.str.26 = private unnamed_addr constant [3 x i8] c"#f\00", align 1
@.str.27 = private unnamed_addr constant [3 x i8] c"#t\00", align 1
@.str.28 = private unnamed_addr constant [27 x i8] c"Unknown Boolean value: %lu\00", align 1
@.str.29 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.30 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.31 = private unnamed_addr constant [4 x i8] c"%ld\00", align 1
@.str.32 = private unnamed_addr constant [25 x i8] c"prim_print_aux Int case.\00", align 1
@.str.33 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.34 = private unnamed_addr constant [25 x i8] c"prim_print_aux Str case.\00", align 1
@.str.35 = private unnamed_addr constant [25 x i8] c"prim_print_aux Sym case.\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.37 = private unnamed_addr constant [11 x i8] c"print_cons\00", align 1
@.str.38 = private unnamed_addr constant [2 x i8] c" \00", align 1
@.str.39 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.40 = private unnamed_addr constant [13 x i8] c"print_vector\00", align 1
@.str.41 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.42 = private unnamed_addr constant [5 x i8] c"Void\00", align 1
@.str.43 = private unnamed_addr constant [5 x i8] c"Null\00", align 1
@.str.44 = private unnamed_addr constant [5 x i8] c"Bool\00", align 1
@.str.45 = private unnamed_addr constant [8 x i8] c"Closure\00", align 1
@.str.46 = private unnamed_addr constant [5 x i8] c"Cons\00", align 1
@.str.47 = private unnamed_addr constant [4 x i8] c"Int\00", align 1
@.str.48 = private unnamed_addr constant [7 x i8] c"String\00", align 1
@.str.49 = private unnamed_addr constant [7 x i8] c"Symbol\00", align 1
@.str.50 = private unnamed_addr constant [7 x i8] c"Vector\00", align 1
@.str.51 = private unnamed_addr constant [6 x i8] c"Other\00", align 1
@.str.52 = private unnamed_addr constant [15 x i8] c"applyprim_halt\00", align 1
@.str.53 = private unnamed_addr constant [18 x i8] c"get_vector_length\00", align 1
@.str.54 = private unnamed_addr constant [10 x i8] c"_get_both\00", align 1
@.str.55 = private unnamed_addr constant [59 x i8] c"Vectors larger than 256 elements are unimplemented. Sorry!\00", align 1
@.str.56 = private unnamed_addr constant [24 x i8] c"applyprim_make_45vector\00", align 1
@.str.57 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 2 arguments but got 1.\00", align 1
@.str.58 = private unnamed_addr constant [38 x i8] c"Function '%s' only takes 2 arguments.\00", align 1
@.str.59 = private unnamed_addr constant [12 x i8] c"make-vector\00", align 1
@.str.60 = private unnamed_addr constant [20 x i8] c"applyprim_vector_63\00", align 1
@.str.61 = private unnamed_addr constant [26 x i8] c"applyprim_vector_45length\00", align 1
@.str.62 = private unnamed_addr constant [21 x i8] c"prim_vector_45length\00", align 1
@.str.63 = private unnamed_addr constant [13 x i8] c"bounds check\00", align 1
@.str.64 = private unnamed_addr constant [53 x i8] c"Bounds check fail, wanted pos %ld, only %ld elements\00", align 1
@.str.65 = private unnamed_addr constant [23 x i8] c"applyprim_vector_45ref\00", align 1
@.str.66 = private unnamed_addr constant [11 x i8] c"vector-ref\00", align 1
@.str.67 = private unnamed_addr constant [21 x i8] c"vector-ref index_pos\00", align 1
@.str.68 = private unnamed_addr constant [26 x i8] c"applyprim_vector_45set_33\00", align 1
@.str.69 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 3 arguments but got 1.\00", align 1
@.str.70 = private unnamed_addr constant [46 x i8] c"Function '%s' expected 3 arguments but got 2.\00", align 1
@.str.71 = private unnamed_addr constant [38 x i8] c"Function '%s' only takes 3 arguments.\00", align 1
@.str.72 = private unnamed_addr constant [12 x i8] c"vector-set!\00", align 1
@.str.73 = private unnamed_addr constant [28 x i8] c"apply vector-set! index_pos\00", align 1
@.str.74 = private unnamed_addr constant [64 x i8] c"Expected an empty list but got something else for function '%s'\00", align 1
@.str.75 = private unnamed_addr constant [15 x i8] c"applyprim_void\00", align 1
@.str.76 = private unnamed_addr constant [19 x i8] c"vec_eq_helper avec\00", align 1
@.str.77 = private unnamed_addr constant [19 x i8] c"vec_eq_helper bvec\00", align 1
@.str.78 = private unnamed_addr constant [19 x i8] c"vec_eq_helper alen\00", align 1
@.str.79 = private unnamed_addr constant [19 x i8] c"vec_eq_helper blen\00", align 1
@.str.80 = private unnamed_addr constant [18 x i8] c"prim_eq_63 Bool a\00", align 1
@.str.81 = private unnamed_addr constant [18 x i8] c"prim_eq_63 Bool b\00", align 1
@.str.82 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Int a\00", align 1
@.str.83 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Int b\00", align 1
@.str.84 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Str a\00", align 1
@.str.85 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Str b\00", align 1
@.str.86 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Sym a\00", align 1
@.str.87 = private unnamed_addr constant [17 x i8] c"prim_eq_63 Sym b\00", align 1
@.str.88 = private unnamed_addr constant [42 x i8] c"I do not know what youre trying to eq?!!.\00", align 1
@.str.89 = private unnamed_addr constant [16 x i8] c"applyprim_eq_63\00", align 1
@.str.90 = private unnamed_addr constant [17 x i8] c"applyprim_eqv_63\00", align 1
@.str.91 = private unnamed_addr constant [19 x i8] c"applyprim_equal_63\00", align 1
@.str.92 = private unnamed_addr constant [20 x i8] c"applyprim_number_63\00", align 1
@.str.93 = private unnamed_addr constant [21 x i8] c"applyprim_integer_63\00", align 1
@.str.94 = private unnamed_addr constant [21 x i8] c"applyprim_boolean_63\00", align 1
@.str.95 = private unnamed_addr constant [18 x i8] c"applyprim_void_63\00", align 1
@.str.96 = private unnamed_addr constant [23 x i8] c"applyprim_procedure_63\00", align 1
@.str.97 = private unnamed_addr constant [18 x i8] c"applyprim_null_63\00", align 1
@.str.98 = private unnamed_addr constant [18 x i8] c"applyprim_cons_63\00", align 1
@.str.99 = private unnamed_addr constant [15 x i8] c"applyprim_cons\00", align 1
@.str.100 = private unnamed_addr constant [14 x i8] c"applyprim_car\00", align 1
@.str.101 = private unnamed_addr constant [4 x i8] c"car\00", align 1
@.str.102 = private unnamed_addr constant [14 x i8] c"applyprim_cdr\00", align 1
@.str.103 = private unnamed_addr constant [4 x i8] c"cdr\00", align 1
@.str.104 = private unnamed_addr constant [4 x i8] c"+ a\00", align 1
@.str.105 = private unnamed_addr constant [4 x i8] c"+ b\00", align 1
@.str.106 = private unnamed_addr constant [37 x i8] c"Expected Cons in apply +, but got %s\00", align 1
@.str.107 = private unnamed_addr constant [14 x i8] c"apply + final\00", align 1
@.str.108 = private unnamed_addr constant [4 x i8] c"- a\00", align 1
@.str.109 = private unnamed_addr constant [4 x i8] c"- b\00", align 1
@.str.110 = private unnamed_addr constant [8 x i8] c"apply -\00", align 1
@.str.111 = private unnamed_addr constant [15 x i8] c"apply - carval\00", align 1
@.str.112 = private unnamed_addr constant [20 x i8] c"applyprim__45 final\00", align 1
@.str.113 = private unnamed_addr constant [4 x i8] c"* a\00", align 1
@.str.114 = private unnamed_addr constant [4 x i8] c"* b\00", align 1
@.str.115 = private unnamed_addr constant [17 x i8] c"apply * cons_obj\00", align 1
@.str.116 = private unnamed_addr constant [15 x i8] c"apply * carval\00", align 1
@.str.117 = private unnamed_addr constant [15 x i8] c"apply * cdrval\00", align 1
@.str.118 = private unnamed_addr constant [36 x i8] c"apply * taking a non-list argument!\00", align 1
@.str.119 = private unnamed_addr constant [4 x i8] c"/ a\00", align 1
@.str.120 = private unnamed_addr constant [4 x i8] c"/ b\00", align 1
@.str.121 = private unnamed_addr constant [14 x i8] c"applyprim__61\00", align 1
@.str.122 = private unnamed_addr constant [4 x i8] c"= a\00", align 1
@.str.123 = private unnamed_addr constant [4 x i8] c"= b\00", align 1
@.str.124 = private unnamed_addr constant [4 x i8] c"< a\00", align 1
@.str.125 = private unnamed_addr constant [4 x i8] c"< b\00", align 1
@.str.126 = private unnamed_addr constant [17 x i8] c"applyprim__60_61\00", align 1
@.str.127 = private unnamed_addr constant [5 x i8] c"<= a\00", align 1
@.str.128 = private unnamed_addr constant [5 x i8] c"<= b\00", align 1
@.str.129 = private unnamed_addr constant [14 x i8] c"applyprim_not\00", align 1
@.str.130 = private unnamed_addr constant [4 x i8] c"not\00", align 1

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @alloc(i64 %0) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call { i64, i1 } @llvm.umul.with.overflow.i64(i64 %3, i64 16)
  %5 = extractvalue { i64, i1 } %4, 1
  %6 = extractvalue { i64, i1 } %4, 0
  %7 = select i1 %5, i64 -1, i64 %6
  %8 = call noalias nonnull i8* @_Znam(i64 %7) #8
  %9 = bitcast i8* %8 to %struct.ScmObj*
  ret %struct.ScmObj* %9
}

; Function Attrs: nofree nosync nounwind readnone speculatable willreturn
declare { i64, i1 } @llvm.umul.with.overflow.i64(i64, i64) #1

; Function Attrs: nobuiltin allocsize(0)
declare nonnull i8* @_Znam(i64) #2

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @closure_alloc(i64 %0, i64 %1) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %9 = call %struct.ScmObj* @alloc(i64 2)
  store %struct.ScmObj* %9, %struct.ScmObj** %5, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  store i32 9, i32* %10, align 8
  %11 = load i64, i64* %4, align 8
  %12 = inttoptr i64 %11 to i64*
  %13 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 0
  store i64* %12, i64** %13, align 8
  %14 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 1
  store i32 9, i32* %14, align 8
  %15 = load i64, i64* %3, align 8
  %16 = call %struct.ScmObj* @const_init_int(i64 %15)
  %17 = call %struct.ScmObj* @const_init_int(i64 0)
  %18 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %16, %struct.ScmObj* %17)
  %19 = bitcast %struct.ScmObj* %18 to i64*
  %20 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  store i64* %19, i64** %20, align 8
  %21 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 0
  %23 = bitcast %struct.ScmObj* %22 to i8*
  %24 = bitcast %struct.ScmObj* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %26 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %25, i64 1
  %27 = bitcast %struct.ScmObj* %26 to i8*
  %28 = bitcast %struct.ScmObj* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %27, i8* align 8 %28, i64 16, i1 false)
  %29 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %29, %struct.ScmObj** %8, align 8
  %30 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %31 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %30, i32 0, i32 1
  store i32 3, i32* %31, align 8
  %32 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %33 = bitcast %struct.ScmObj* %32 to i64*
  %34 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %35 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %34, i32 0, i32 0
  store i64* %33, i64** %35, align 8
  %36 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  ret %struct.ScmObj* %36
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %struct.ScmObj*, align 8
  %9 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %11 = call i64 @unwrap_int(%struct.ScmObj* %10, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.59, i64 0, i64 0))
  store i64 %11, i64* %5, align 8
  %12 = load i64, i64* %5, align 8
  %13 = add i64 1, %12
  %14 = call %struct.ScmObj* @alloc(i64 %13)
  store %struct.ScmObj* %14, %struct.ScmObj** %6, align 8
  %15 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %16 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %15, i64 0
  %17 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %16, i32 0, i32 1
  store i32 5, i32* %17, align 8
  %18 = load i64, i64* %5, align 8
  %19 = inttoptr i64 %18 to i64*
  %20 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i32 0, i32 0
  store i64* %19, i64** %22, align 8
  store i64 1, i64* %7, align 8
  br label %23

23:                                               ; preds = %49, %2
  %24 = load i64, i64* %7, align 8
  %25 = load i64, i64* %5, align 8
  %26 = icmp ule i64 %24, %25
  br i1 %26, label %27, label %52

27:                                               ; preds = %23
  %28 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %28, %struct.ScmObj** %8, align 8
  %29 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %30 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %29, i32 0, i32 1
  %31 = load i32, i32* %30, align 8
  %32 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %33 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %32, i32 0, i32 1
  store i32 %31, i32* %33, align 8
  %34 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %35 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %34, i32 0, i32 0
  %36 = load i64*, i64** %35, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i32 0, i32 0
  store i64* %36, i64** %38, align 8
  %39 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %40 = load i64, i64* %7, align 8
  %41 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %39, i64 %40
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i32 0, i32 1
  store i32 9, i32* %42, align 8
  %43 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %44 = bitcast %struct.ScmObj* %43 to i64*
  %45 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %46 = load i64, i64* %7, align 8
  %47 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %45, i64 %46
  %48 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %47, i32 0, i32 0
  store i64* %44, i64** %48, align 8
  br label %49

49:                                               ; preds = %27
  %50 = load i64, i64* %7, align 8
  %51 = add i64 %50, 1
  store i64 %51, i64* %7, align 8
  br label %23, !llvm.loop !8

52:                                               ; preds = %23
  %53 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %53, %struct.ScmObj** %9, align 8
  %54 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %55 = bitcast %struct.ScmObj* %54 to i64*
  %56 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %57 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %56, i32 0, i32 0
  store i64* %55, i64** %57, align 8
  %58 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %59 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %58, i32 0, i32 1
  store i32 8, i32* %59, align 8
  %60 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  ret %struct.ScmObj* %60
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_int(i64 %0) #0 {
  %2 = alloca i64, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store i64 %0, i64* %2, align 8
  %4 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %4, %struct.ScmObj** %3, align 8
  %5 = load i64, i64* %2, align 8
  %6 = inttoptr i64 %5 to i64*
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  store i64* %6, i64** %8, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  store i32 5, i32* %10, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  ret %struct.ScmObj* %11
}

; Function Attrs: argmemonly nofree nosync nounwind willreturn
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* noalias nocapture writeonly, i8* noalias nocapture readonly, i64, i1 immarg) #3

; Function Attrs: noinline optnone ssp uwtable mustprogress
define void @closure_place_freevar(%struct.ScmObj* %0, %struct.ScmObj* %1, i64 %2) #0 {
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %struct.ScmObj*, align 8
  %8 = alloca %struct.ScmObj*, align 8
  %9 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  store i64 %2, i64* %6, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %11 = call %struct.ScmObj* @unwrap_clo(%struct.ScmObj* %10, i8* getelementptr inbounds ([22 x i8], [22 x i8]* @.str, i64 0, i64 0))
  store %struct.ScmObj* %11, %struct.ScmObj** %7, align 8
  %12 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %13 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %12, i64 1
  %14 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %13, i32 0, i32 0
  %15 = load i64*, i64** %14, align 8
  %16 = bitcast i64* %15 to %struct.ScmObj*
  store %struct.ScmObj* %16, %struct.ScmObj** %8, align 8
  %17 = load i64, i64* %6, align 8
  %18 = call %struct.ScmObj* @const_init_int(i64 %17)
  store %struct.ScmObj* %18, %struct.ScmObj** %9, align 8
  %19 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %21 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %22 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %19, %struct.ScmObj* %20, %struct.ScmObj* %21)
  ret void
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @unwrap_clo(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 3
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.7, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = bitcast i64* %20 to %struct.ScmObj*
  ret %struct.ScmObj* %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %0, %struct.ScmObj* %1, %struct.ScmObj* %2) #0 {
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  store %struct.ScmObj* %2, %struct.ScmObj** %6, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %11 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %10, i8* getelementptr inbounds ([12 x i8], [12 x i8]* @.str.72, i64 0, i64 0))
  store %struct.ScmObj* %11, %struct.ScmObj** %7, align 8
  %12 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %13 = call i64 @unwrap_int(%struct.ScmObj* %12, i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.73, i64 0, i64 0))
  store i64 %13, i64* %8, align 8
  %14 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %15 = load i64, i64* %8, align 8
  call void @bounds_check(%struct.ScmObj* %14, i64 %15)
  %16 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %16, %struct.ScmObj** %9, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i32 0, i32 1
  %19 = load i32, i32* %18, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i32 0, i32 1
  store i32 %19, i32* %21, align 8
  %22 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %23 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %22, i32 0, i32 0
  %24 = load i64*, i64** %23, align 8
  %25 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %26 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %25, i32 0, i32 0
  store i64* %24, i64** %26, align 8
  %27 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %28 = bitcast %struct.ScmObj* %27 to i64*
  %29 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %30 = load i64, i64* %8, align 8
  %31 = add nsw i64 %30, 1
  %32 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %29, i64 %31
  %33 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %32, i32 0, i32 0
  store i64* %28, i64** %33, align 8
  %34 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %34
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define void (%struct.ScmObj*, %struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = call %struct.ScmObj* @unwrap_clo(%struct.ScmObj* %4, i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.1, i64 0, i64 0))
  store %struct.ScmObj* %5, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 0
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  %9 = load i64*, i64** %8, align 8
  %10 = bitcast i64* %9 to void (%struct.ScmObj*, %struct.ScmObj*)*
  ret void (%struct.ScmObj*, %struct.ScmObj*)* %10
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @closure_env_get(%struct.ScmObj* %0, i64 %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = call %struct.ScmObj* @unwrap_clo(%struct.ScmObj* %7, i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.2, i64 0, i64 0))
  store %struct.ScmObj* %8, %struct.ScmObj** %5, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i64 1
  %11 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %10, i32 0, i32 0
  %12 = load i64*, i64** %11, align 8
  %13 = bitcast i64* %12 to %struct.ScmObj*
  store %struct.ScmObj* %13, %struct.ScmObj** %6, align 8
  %14 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %15 = load i64, i64* %4, align 8
  %16 = call %struct.ScmObj* @const_init_int(i64 %15)
  %17 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %14, %struct.ScmObj* %16)
  ret %struct.ScmObj* %17
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %7, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.66, i64 0, i64 0))
  store %struct.ScmObj* %8, %struct.ScmObj** %5, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.ScmObj* %9, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.67, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %12 = load i64, i64* %6, align 8
  call void @bounds_check(%struct.ScmObj* %11, i64 %12)
  %13 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %14 = load i64, i64* %6, align 8
  %15 = add nsw i64 %14, 1
  %16 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %13, i64 %15
  %17 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %16, i32 0, i32 0
  %18 = load i64*, i64** %17, align 8
  %19 = bitcast i64* %18 to %struct.ScmObj*
  ret %struct.ScmObj* %19
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 4
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.4, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = bitcast i64* %20 to %struct.ScmObj*
  ret %struct.ScmObj* %21
}

declare i32 @printf(i8*, ...) #4

; Function Attrs: noreturn
declare void @exit(i32) #5

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 8
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([53 x i8], [53 x i8]* @.str.6, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = bitcast i64* %20 to %struct.ScmObj*
  ret %struct.ScmObj* %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i64 @unwrap_int(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 5
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.8, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = ptrtoint i64* %20 to i64
  ret i64 %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i64 @unwrap_bool(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 2
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.9, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = ptrtoint i64* %20 to i64
  ret i64 %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i8* @unwrap_str(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 6
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.10, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = bitcast i64* %20 to i8*
  ret i8* %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i8* @unwrap_sym(%struct.ScmObj* %0, i8* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i8* %1, i8** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 7
  br i1 %8, label %9, label %17

9:                                                ; preds = %2
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %12 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %13 = load i32, i32* %12, align 8
  %14 = load i8*, i8** %4, align 8
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.11, i64 0, i64 0), i32 %13, i8* %14)
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %2
  %18 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i32 0, i32 0
  %20 = load i64*, i64** %19, align 8
  %21 = bitcast i64* %20 to i8*
  ret i8* %21
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i64 @is_truthy_value(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 2
  br i1 %6, label %7, label %11

7:                                                ; preds = %1
  %8 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %9 = call i64 @unwrap_bool(%struct.ScmObj* %8, i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.12, i64 0, i64 0))
  %10 = icmp eq i64 %9, 0
  br label %11

11:                                               ; preds = %7, %1
  %12 = phi i1 [ false, %1 ], [ %10, %7 ]
  %13 = zext i1 %12 to i64
  %14 = select i1 %12, i1 false, i1 true
  %15 = zext i1 %14 to i64
  ret i64 %15
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_void() #0 {
  %1 = alloca %struct.ScmObj*, align 8
  %2 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %2, %struct.ScmObj** %1, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 0
  store i64* null, i64** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  store i32 0, i32* %6, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_null() #0 {
  %1 = alloca %struct.ScmObj*, align 8
  %2 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %2, %struct.ScmObj** %1, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 0
  store i64* null, i64** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  store i32 1, i32* %6, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_true() #0 {
  %1 = alloca %struct.ScmObj*, align 8
  %2 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %2, %struct.ScmObj** %1, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 0
  store i64* inttoptr (i64 1 to i64*), i64** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  store i32 2, i32* %6, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_false() #0 {
  %1 = alloca %struct.ScmObj*, align 8
  %2 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %2, %struct.ScmObj** %1, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 0
  store i64* null, i64** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  store i32 2, i32* %6, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %1, align 8
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_string(i8* %0) #0 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store i8* %0, i8** %2, align 8
  %4 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %4, %struct.ScmObj** %3, align 8
  %5 = load i8*, i8** %2, align 8
  %6 = bitcast i8* %5 to i64*
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  store i64* %6, i64** %8, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  store i32 6, i32* %10, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  ret %struct.ScmObj* %11
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @const_init_symbol(i8* %0) #0 {
  %2 = alloca i8*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store i8* %0, i8** %2, align 8
  %4 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %4, %struct.ScmObj** %3, align 8
  %5 = load i8*, i8** %2, align 8
  %6 = bitcast i8* %5 to i64*
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  store i64* %6, i64** %8, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  store i32 7, i32* %10, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  ret %struct.ScmObj* %11
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_display(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.14, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.14, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.14, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_display(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_display(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = call %struct.ScmObj* @prim_print(%struct.ScmObj* %3)
  ret %struct.ScmObj* %4
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_print(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca i32, align 4
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %4, i32 0, i32 1
  %6 = load i32, i32* %5, align 8
  store i32 %6, i32* %3, align 4
  %7 = load i32, i32* %3, align 4
  switch i32 %7, label %23 [
    i32 1, label %8
    i32 7, label %10
    i32 0, label %14
    i32 4, label %15
    i32 2, label %20
    i32 3, label %20
    i32 5, label %20
    i32 6, label %20
    i32 8, label %20
    i32 9, label %20
  ]

8:                                                ; preds = %1
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.17, i64 0, i64 0))
  br label %23

10:                                               ; preds = %1
  %11 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %12 = call i8* @unwrap_sym(%struct.ScmObj* %11, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.19, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.18, i64 0, i64 0), i8* %12)
  br label %23

14:                                               ; preds = %1
  br label %23

15:                                               ; preds = %1
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.20, i64 0, i64 0))
  %17 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %18 = call %struct.ScmObj* @print_cons(%struct.ScmObj* %17)
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.21, i64 0, i64 0))
  br label %23

20:                                               ; preds = %1, %1, %1, %1, %1, %1
  %21 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %22 = call %struct.ScmObj* @prim_print_aux(%struct.ScmObj* %21)
  br label %23

23:                                               ; preds = %1, %20, %15, %14, %10, %8
  %24 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %24
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_print(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.16, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.16, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.16, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_print(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @print_cons(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %6, i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.37, i64 0, i64 0))
  store %struct.ScmObj* %7, %struct.ScmObj** %3, align 8
  %8 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %9 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i64 0
  store %struct.ScmObj* %9, %struct.ScmObj** %4, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %11 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %10, i64 1
  store %struct.ScmObj* %11, %struct.ScmObj** %5, align 8
  %12 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %13 = call %struct.ScmObj* @prim_print_aux(%struct.ScmObj* %12)
  %14 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %15 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %14, i32 0, i32 1
  %16 = load i32, i32* %15, align 8
  switch i32 %16, label %28 [
    i32 1, label %17
    i32 0, label %18
    i32 4, label %20
    i32 7, label %24
    i32 2, label %24
    i32 3, label %24
    i32 5, label %24
    i32 6, label %24
    i32 8, label %24
    i32 9, label %24
  ]

17:                                               ; preds = %1
  br label %28

18:                                               ; preds = %1
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.23, i64 0, i64 0))
  br label %28

20:                                               ; preds = %1
  %21 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.38, i64 0, i64 0))
  %22 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %23 = call %struct.ScmObj* @print_cons(%struct.ScmObj* %22)
  br label %28

24:                                               ; preds = %1, %1, %1, %1, %1, %1, %1
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.39, i64 0, i64 0))
  %26 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %27 = call %struct.ScmObj* @prim_print_aux(%struct.ScmObj* %26)
  br label %28

28:                                               ; preds = %1, %24, %20, %18, %17
  %29 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %29
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_print_aux(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  store i32 %7, i32* %3, align 4
  %8 = load i32, i32* %3, align 4
  switch i32 %8, label %58 [
    i32 0, label %9
    i32 1, label %11
    i32 2, label %13
    i32 3, label %30
    i32 4, label %32
    i32 5, label %37
    i32 6, label %41
    i32 7, label %45
    i32 8, label %49
    i32 9, label %52
  ]

9:                                                ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.23, i64 0, i64 0))
  br label %58

11:                                               ; preds = %1
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.24, i64 0, i64 0))
  br label %58

13:                                               ; preds = %1
  %14 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %15 = call i64 @unwrap_bool(%struct.ScmObj* %14, i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.25, i64 0, i64 0))
  store i64 %15, i64* %4, align 8
  %16 = load i64, i64* %4, align 8
  %17 = icmp eq i64 %16, 0
  br i1 %17, label %18, label %20

18:                                               ; preds = %13
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.26, i64 0, i64 0))
  br label %29

20:                                               ; preds = %13
  %21 = load i64, i64* %4, align 8
  %22 = icmp eq i64 %21, 1
  br i1 %22, label %23, label %25

23:                                               ; preds = %20
  %24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.27, i64 0, i64 0))
  br label %28

25:                                               ; preds = %20
  %26 = load i64, i64* %4, align 8
  %27 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.28, i64 0, i64 0), i64 %26)
  br label %28

28:                                               ; preds = %25, %23
  br label %29

29:                                               ; preds = %28, %18
  br label %58

30:                                               ; preds = %1
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.29, i64 0, i64 0))
  br label %58

32:                                               ; preds = %1
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.30, i64 0, i64 0))
  %34 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %35 = call %struct.ScmObj* @print_cons(%struct.ScmObj* %34)
  %36 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.21, i64 0, i64 0))
  br label %58

37:                                               ; preds = %1
  %38 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %39 = call i64 @unwrap_int(%struct.ScmObj* %38, i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.32, i64 0, i64 0))
  %40 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.31, i64 0, i64 0), i64 %39)
  br label %58

41:                                               ; preds = %1
  %42 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %43 = call i8* @unwrap_str(%struct.ScmObj* %42, i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.34, i64 0, i64 0))
  %44 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.33, i64 0, i64 0), i8* %43)
  br label %58

45:                                               ; preds = %1
  %46 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %47 = call i8* @unwrap_sym(%struct.ScmObj* %46, i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str.35, i64 0, i64 0))
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.33, i64 0, i64 0), i8* %47)
  br label %58

49:                                               ; preds = %1
  %50 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %51 = call %struct.ScmObj* @print_vector(%struct.ScmObj* %50)
  br label %58

52:                                               ; preds = %1
  %53 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %54 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %53, i32 0, i32 0
  %55 = load i64*, i64** %54, align 8
  %56 = ptrtoint i64* %55 to i64
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i64 0, i64 0), i64 %56)
  br label %58

58:                                               ; preds = %1, %52, %49, %45, %41, %37, %32, %30, %29, %11, %9
  %59 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %59
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_println(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.22, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.22, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.22, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_println(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_println(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = call %struct.ScmObj* @prim_print(%struct.ScmObj* %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  %6 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %6
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @print_vector(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %8 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %7, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.40, i64 0, i64 0))
  store %struct.ScmObj* %8, %struct.ScmObj** %3, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = call i64 @_get_vector_length(%struct.ScmObj* %9)
  store i64 %10, i64* %4, align 8
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.41, i64 0, i64 0))
  store i64 1, i64* %5, align 8
  br label %12

12:                                               ; preds = %30, %1
  %13 = load i64, i64* %5, align 8
  %14 = load i64, i64* %4, align 8
  %15 = icmp ule i64 %13, %14
  br i1 %15, label %16, label %33

16:                                               ; preds = %12
  %17 = load i64, i64* %5, align 8
  %18 = icmp ne i64 %17, 1
  br i1 %18, label %19, label %21

19:                                               ; preds = %16
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.38, i64 0, i64 0))
  br label %21

21:                                               ; preds = %19, %16
  %22 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %23 = load i64, i64* %5, align 8
  %24 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %22, i64 %23
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i32 0, i32 0
  %26 = load i64*, i64** %25, align 8
  %27 = bitcast i64* %26 to %struct.ScmObj*
  store %struct.ScmObj* %27, %struct.ScmObj** %6, align 8
  %28 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %29 = call %struct.ScmObj* @prim_print_aux(%struct.ScmObj* %28)
  br label %30

30:                                               ; preds = %21
  %31 = load i64, i64* %5, align 8
  %32 = add i64 %31, 1
  store i64 %32, i64* %5, align 8
  br label %12, !llvm.loop !10

33:                                               ; preds = %12
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.21, i64 0, i64 0))
  %35 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %35
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i64 @_get_vector_length(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %4, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.53, i64 0, i64 0))
  store %struct.ScmObj* %5, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 0
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 0
  %9 = load i64*, i64** %8, align 8
  %10 = ptrtoint i64* %9 to i64
  ret i64 %10
}

; Function Attrs: noinline nounwind optnone ssp uwtable mustprogress
define i8* @get_type_name(i32 %0) #6 {
  %2 = alloca i8*, align 8
  %3 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  %4 = load i32, i32* %3, align 4
  switch i32 %4, label %15 [
    i32 0, label %5
    i32 1, label %6
    i32 2, label %7
    i32 3, label %8
    i32 4, label %9
    i32 5, label %10
    i32 6, label %11
    i32 7, label %12
    i32 8, label %13
    i32 9, label %14
  ]

5:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.42, i64 0, i64 0), i8** %2, align 8
  br label %15

6:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.43, i64 0, i64 0), i8** %2, align 8
  br label %15

7:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.44, i64 0, i64 0), i8** %2, align 8
  br label %15

8:                                                ; preds = %1
  store i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.45, i64 0, i64 0), i8** %2, align 8
  br label %15

9:                                                ; preds = %1
  store i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.46, i64 0, i64 0), i8** %2, align 8
  br label %15

10:                                               ; preds = %1
  store i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.47, i64 0, i64 0), i8** %2, align 8
  br label %15

11:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.48, i64 0, i64 0), i8** %2, align 8
  br label %15

12:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.49, i64 0, i64 0), i8** %2, align 8
  br label %15

13:                                               ; preds = %1
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.50, i64 0, i64 0), i8** %2, align 8
  br label %15

14:                                               ; preds = %1
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.51, i64 0, i64 0), i8** %2, align 8
  br label %15

15:                                               ; preds = %5, %6, %7, %8, %9, %10, %11, %12, %13, %14, %1
  %16 = load i8*, i8** %2, align 8
  ret i8* %16
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_halt(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.52, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.52, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.52, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_halt(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = call %struct.ScmObj* @prim_print(%struct.ScmObj* %3)
  %5 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %6 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %7 = load i32, i32* %6, align 8
  %8 = icmp ne i32 %7, 0
  br i1 %8, label %9, label %11

9:                                                ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  br label %11

11:                                               ; preds = %9, %1
  call void @exit(i32 0) #9
  unreachable
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define void @_get_both(%struct.ScmObj* %0, %struct.ScmObj* %1, %struct.ScmObj* %2) #0 {
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  store %struct.ScmObj* %2, %struct.ScmObj** %6, align 8
  %8 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %9 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %8, i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.54, i64 0, i64 0))
  store %struct.ScmObj* %9, %struct.ScmObj** %7, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %11 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %10, i64 0
  %12 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %13 = bitcast %struct.ScmObj* %12 to i8*
  %14 = bitcast %struct.ScmObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %13, i8* align 8 %14, i64 16, i1 false)
  %15 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %16 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %15, i64 1
  %17 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %18 = bitcast %struct.ScmObj* %17 to i8*
  %19 = bitcast %struct.ScmObj* %16 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %18, i8* align 8 %19, i64 16, i1 false)
  ret void
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj, align 8
  %4 = alloca [256 x %struct.ScmObj*], align 8
  %5 = alloca i64, align 8
  %6 = alloca %struct.ScmObj, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj*, align 8
  %9 = alloca i64, align 8
  %10 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %12 = bitcast %struct.ScmObj* %3 to i8*
  %13 = bitcast %struct.ScmObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %12, i8* align 8 %13, i64 16, i1 false)
  %14 = bitcast [256 x %struct.ScmObj*]* %4 to i8*
  call void @llvm.memset.p0i8.i64(i8* align 8 %14, i8 0, i64 2048, i1 false)
  store i64 0, i64* %5, align 8
  br label %15

15:                                               ; preds = %24, %1
  %16 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %17 = load i32, i32* %16, align 8
  %18 = icmp eq i32 %17, 4
  br i1 %18, label %19, label %22

19:                                               ; preds = %15
  %20 = load i64, i64* %5, align 8
  %21 = icmp ult i64 %20, 256
  br label %22

22:                                               ; preds = %19, %15
  %23 = phi i1 [ false, %15 ], [ %21, %19 ]
  br i1 %23, label %24, label %43

24:                                               ; preds = %22
  call void @_get_both(%struct.ScmObj* %3, %struct.ScmObj* %6, %struct.ScmObj* %7)
  %25 = call %struct.ScmObj* @alloc(i64 1)
  %26 = load i64, i64* %5, align 8
  %27 = getelementptr inbounds [256 x %struct.ScmObj*], [256 x %struct.ScmObj*]* %4, i64 0, i64 %26
  store %struct.ScmObj* %25, %struct.ScmObj** %27, align 8
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = load i64, i64* %5, align 8
  %31 = getelementptr inbounds [256 x %struct.ScmObj*], [256 x %struct.ScmObj*]* %4, i64 0, i64 %30
  %32 = load %struct.ScmObj*, %struct.ScmObj** %31, align 8
  %33 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %32, i32 0, i32 1
  store i32 %29, i32* %33, align 8
  %34 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 0
  %35 = load i64*, i64** %34, align 8
  %36 = load i64, i64* %5, align 8
  %37 = add i64 %36, 1
  store i64 %37, i64* %5, align 8
  %38 = getelementptr inbounds [256 x %struct.ScmObj*], [256 x %struct.ScmObj*]* %4, i64 0, i64 %36
  %39 = load %struct.ScmObj*, %struct.ScmObj** %38, align 8
  %40 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %39, i32 0, i32 0
  store i64* %35, i64** %40, align 8
  %41 = bitcast %struct.ScmObj* %3 to i8*
  %42 = bitcast %struct.ScmObj* %7 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %41, i8* align 8 %42, i64 16, i1 false)
  br label %15, !llvm.loop !11

43:                                               ; preds = %22
  %44 = load i64, i64* %5, align 8
  %45 = icmp eq i64 %44, 256
  br i1 %45, label %46, label %54

46:                                               ; preds = %43
  %47 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %48 = load i32, i32* %47, align 8
  %49 = icmp ne i32 %48, 1
  br i1 %49, label %50, label %54

50:                                               ; preds = %46
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %52 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([59 x i8], [59 x i8]* @.str.55, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

54:                                               ; preds = %46, %43
  %55 = load i64, i64* %5, align 8
  %56 = add i64 %55, 1
  %57 = call %struct.ScmObj* @alloc(i64 %56)
  store %struct.ScmObj* %57, %struct.ScmObj** %8, align 8
  %58 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %59 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %58, i64 0
  %60 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %59, i32 0, i32 1
  store i32 5, i32* %60, align 8
  %61 = load i64, i64* %5, align 8
  %62 = inttoptr i64 %61 to i64*
  %63 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %64 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %63, i64 0
  %65 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %64, i32 0, i32 0
  store i64* %62, i64** %65, align 8
  store i64 0, i64* %9, align 8
  br label %66

66:                                               ; preds = %85, %54
  %67 = load i64, i64* %9, align 8
  %68 = load i64, i64* %5, align 8
  %69 = icmp ult i64 %67, %68
  br i1 %69, label %70, label %88

70:                                               ; preds = %66
  %71 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %72 = load i64, i64* %9, align 8
  %73 = add i64 %72, 1
  %74 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %71, i64 %73
  %75 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %74, i32 0, i32 1
  store i32 9, i32* %75, align 8
  %76 = load i64, i64* %9, align 8
  %77 = getelementptr inbounds [256 x %struct.ScmObj*], [256 x %struct.ScmObj*]* %4, i64 0, i64 %76
  %78 = load %struct.ScmObj*, %struct.ScmObj** %77, align 8
  %79 = bitcast %struct.ScmObj* %78 to i64*
  %80 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %81 = load i64, i64* %9, align 8
  %82 = add i64 %81, 1
  %83 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %80, i64 %82
  %84 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %83, i32 0, i32 0
  store i64* %79, i64** %84, align 8
  br label %85

85:                                               ; preds = %70
  %86 = load i64, i64* %9, align 8
  %87 = add i64 %86, 1
  store i64 %87, i64* %9, align 8
  br label %66, !llvm.loop !12

88:                                               ; preds = %66
  %89 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %89, %struct.ScmObj** %10, align 8
  %90 = load %struct.ScmObj*, %struct.ScmObj** %8, align 8
  %91 = bitcast %struct.ScmObj* %90 to i64*
  %92 = load %struct.ScmObj*, %struct.ScmObj** %10, align 8
  %93 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %92, i32 0, i32 0
  store i64* %91, i64** %93, align 8
  %94 = load %struct.ScmObj*, %struct.ScmObj** %10, align 8
  %95 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %94, i32 0, i32 1
  store i32 8, i32* %95, align 8
  %96 = load %struct.ScmObj*, %struct.ScmObj** %10, align 8
  ret %struct.ScmObj* %96
}

; Function Attrs: argmemonly nofree nosync nounwind willreturn writeonly
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1 immarg) #7

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_make_45vector(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.56, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.56, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.56, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.56, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @.str.56, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_vector_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.60, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.60, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.60, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 8
  br i1 %6, label %7, label %9

7:                                                ; preds = %1
  %8 = call %struct.ScmObj* @const_init_true()
  br label %11

9:                                                ; preds = %1
  %10 = call %struct.ScmObj* @const_init_false()
  br label %11

11:                                               ; preds = %9, %7
  %12 = phi %struct.ScmObj* [ %8, %7 ], [ %10, %9 ]
  ret %struct.ScmObj* %12
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_vector_45length(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.61, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.61, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.61, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %4, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.62, i64 0, i64 0))
  store %struct.ScmObj* %5, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 0
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define void @bounds_check(%struct.ScmObj* %0, i64 %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 0
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.63, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load i64, i64* %4, align 8
  %10 = add nsw i64 %9, 1
  %11 = load i64, i64* %5, align 8
  %12 = icmp sgt i64 %10, %11
  br i1 %12, label %13, label %19

13:                                               ; preds = %2
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = load i64, i64* %4, align 8
  %16 = load i64, i64* %5, align 8
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([53 x i8], [53 x i8]* @.str.64, i64 0, i64 0), i64 %15, i64 %16)
  %18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

19:                                               ; preds = %2
  ret void
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_vector_45ref(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.65, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.65, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.65, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.65, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.65, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_vector_45set_33(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  %9 = alloca %struct.ScmObj*, align 8
  %10 = alloca %struct.ScmObj, align 8
  %11 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %12 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %13 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %12, i32 0, i32 1
  %14 = load i32, i32* %13, align 8
  %15 = icmp ne i32 %14, 4
  br i1 %15, label %16, label %20

16:                                               ; preds = %1
  %17 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %18 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

20:                                               ; preds = %1
  %21 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %22 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %21, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  store %struct.ScmObj* %22, %struct.ScmObj** %3, align 8
  %23 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %24 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %23, i64 0
  %25 = bitcast %struct.ScmObj* %4 to i8*
  %26 = bitcast %struct.ScmObj* %24 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %25, i8* align 8 %26, i64 16, i1 false)
  %27 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %27, i64 1
  %29 = bitcast %struct.ScmObj* %5 to i8*
  %30 = bitcast %struct.ScmObj* %28 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %29, i8* align 8 %30, i64 16, i1 false)
  %31 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %32 = load i32, i32* %31, align 8
  %33 = icmp ne i32 %32, 4
  br i1 %33, label %34, label %38

34:                                               ; preds = %20
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %36 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.69, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  %37 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

38:                                               ; preds = %20
  %39 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  store %struct.ScmObj* %39, %struct.ScmObj** %6, align 8
  %40 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %41 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %40, i64 0
  %42 = bitcast %struct.ScmObj* %7 to i8*
  %43 = bitcast %struct.ScmObj* %41 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %42, i8* align 8 %43, i64 16, i1 false)
  %44 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %44, i64 1
  %46 = bitcast %struct.ScmObj* %8 to i8*
  %47 = bitcast %struct.ScmObj* %45 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %46, i8* align 8 %47, i64 16, i1 false)
  %48 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %49 = load i32, i32* %48, align 8
  %50 = icmp ne i32 %49, 4
  br i1 %50, label %51, label %55

51:                                               ; preds = %38
  %52 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.70, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  %54 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

55:                                               ; preds = %38
  %56 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %8, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  store %struct.ScmObj* %56, %struct.ScmObj** %9, align 8
  %57 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %58 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %57, i64 0
  %59 = bitcast %struct.ScmObj* %10 to i8*
  %60 = bitcast %struct.ScmObj* %58 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %59, i8* align 8 %60, i64 16, i1 false)
  %61 = load %struct.ScmObj*, %struct.ScmObj** %9, align 8
  %62 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %61, i64 1
  %63 = bitcast %struct.ScmObj* %11 to i8*
  %64 = bitcast %struct.ScmObj* %62 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %63, i8* align 8 %64, i64 16, i1 false)
  %65 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %11, i32 0, i32 1
  %66 = load i32, i32* %65, align 8
  %67 = icmp ne i32 %66, 1
  br i1 %67, label %68, label %72

68:                                               ; preds = %55
  %69 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %70 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.71, i64 0, i64 0), i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.68, i64 0, i64 0))
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

72:                                               ; preds = %55
  %73 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %4, %struct.ScmObj* %7, %struct.ScmObj* %10)
  ret %struct.ScmObj* %73
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_void(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp ne i32 %5, 1
  br i1 %6, label %7, label %11

7:                                                ; preds = %1
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %9 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([64 x i8], [64 x i8]* @.str.74, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.75, i64 0, i64 0))
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

11:                                               ; preds = %1
  %12 = call %struct.ScmObj* @prim_void()
  ret %struct.ScmObj* %12
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_void() #0 {
  %1 = call %struct.ScmObj* @const_init_void()
  ret %struct.ScmObj* %1
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i32 @cons_eq_helper(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  call void @_get_both(%struct.ScmObj* %9, %struct.ScmObj* %5, %struct.ScmObj* %6)
  %10 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  call void @_get_both(%struct.ScmObj* %10, %struct.ScmObj* %7, %struct.ScmObj* %8)
  %11 = call i32 @eq_helper(%struct.ScmObj* %5, %struct.ScmObj* %7)
  %12 = icmp eq i32 %11, 1
  br i1 %12, label %13, label %16

13:                                               ; preds = %2
  %14 = call i32 @eq_helper(%struct.ScmObj* %6, %struct.ScmObj* %8)
  %15 = icmp ne i32 %14, 0
  br label %16

16:                                               ; preds = %13, %2
  %17 = phi i1 [ false, %2 ], [ %15, %13 ]
  %18 = zext i1 %17 to i32
  ret i32 %18
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i32 @eq_helper(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca i8*, align 8
  %7 = alloca i8*, align 8
  %8 = alloca i8*, align 8
  %9 = alloca i8*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  %10 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %11 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %10, i32 0, i32 1
  %12 = load i32, i32* %11, align 8
  %13 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %14 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %13, i32 0, i32 1
  %15 = load i32, i32* %14, align 8
  %16 = icmp ne i32 %12, %15
  br i1 %16, label %17, label %18

17:                                               ; preds = %2
  store i32 0, i32* %3, align 4
  br label %75

18:                                               ; preds = %2
  %19 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %20 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %19, i32 0, i32 1
  %21 = load i32, i32* %20, align 8
  switch i32 %21, label %75 [
    i32 0, label %22
    i32 1, label %23
    i32 2, label %24
    i32 3, label %32
    i32 4, label %33
    i32 5, label %37
    i32 6, label %45
    i32 7, label %56
    i32 8, label %67
    i32 9, label %71
  ]

22:                                               ; preds = %18
  store i32 1, i32* %3, align 4
  br label %75

23:                                               ; preds = %18
  store i32 1, i32* %3, align 4
  br label %75

24:                                               ; preds = %18
  %25 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %26 = call i64 @unwrap_bool(%struct.ScmObj* %25, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.80, i64 0, i64 0))
  %27 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %28 = call i64 @unwrap_bool(%struct.ScmObj* %27, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.81, i64 0, i64 0))
  %29 = icmp eq i64 %26, %28
  br i1 %29, label %30, label %31

30:                                               ; preds = %24
  store i32 1, i32* %3, align 4
  br label %75

31:                                               ; preds = %24
  store i32 0, i32* %3, align 4
  br label %75

32:                                               ; preds = %18
  store i32 0, i32* %3, align 4
  br label %75

33:                                               ; preds = %18
  %34 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %35 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %36 = call i32 @cons_eq_helper(%struct.ScmObj* %34, %struct.ScmObj* %35)
  store i32 %36, i32* %3, align 4
  br label %75

37:                                               ; preds = %18
  %38 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %39 = call i64 @unwrap_int(%struct.ScmObj* %38, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.82, i64 0, i64 0))
  %40 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %41 = call i64 @unwrap_int(%struct.ScmObj* %40, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.83, i64 0, i64 0))
  %42 = icmp eq i64 %39, %41
  br i1 %42, label %43, label %44

43:                                               ; preds = %37
  store i32 1, i32* %3, align 4
  br label %75

44:                                               ; preds = %37
  store i32 0, i32* %3, align 4
  br label %75

45:                                               ; preds = %18
  %46 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %47 = call i8* @unwrap_str(%struct.ScmObj* %46, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.84, i64 0, i64 0))
  store i8* %47, i8** %6, align 8
  %48 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %49 = call i8* @unwrap_str(%struct.ScmObj* %48, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.85, i64 0, i64 0))
  store i8* %49, i8** %7, align 8
  %50 = load i8*, i8** %6, align 8
  %51 = load i8*, i8** %7, align 8
  %52 = call i32 @strcmp(i8* %50, i8* %51)
  %53 = icmp eq i32 %52, 0
  br i1 %53, label %54, label %55

54:                                               ; preds = %45
  store i32 1, i32* %3, align 4
  br label %75

55:                                               ; preds = %45
  store i32 0, i32* %3, align 4
  br label %75

56:                                               ; preds = %18
  %57 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %58 = call i8* @unwrap_sym(%struct.ScmObj* %57, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.86, i64 0, i64 0))
  store i8* %58, i8** %8, align 8
  %59 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %60 = call i8* @unwrap_sym(%struct.ScmObj* %59, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.87, i64 0, i64 0))
  store i8* %60, i8** %9, align 8
  %61 = load i8*, i8** %8, align 8
  %62 = load i8*, i8** %9, align 8
  %63 = call i32 @strcmp(i8* %61, i8* %62)
  %64 = icmp eq i32 %63, 0
  br i1 %64, label %65, label %66

65:                                               ; preds = %56
  store i32 1, i32* %3, align 4
  br label %75

66:                                               ; preds = %56
  store i32 0, i32* %3, align 4
  br label %75

67:                                               ; preds = %18
  %68 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %69 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %70 = call i32 @vec_eq_helper(%struct.ScmObj* %68, %struct.ScmObj* %69)
  store i32 %70, i32* %3, align 4
  br label %75

71:                                               ; preds = %18
  %72 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %73 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([42 x i8], [42 x i8]* @.str.88, i64 0, i64 0))
  %74 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

75:                                               ; preds = %17, %22, %23, %30, %31, %32, %33, %43, %44, %54, %55, %65, %66, %67, %18
  %76 = load i32, i32* %3, align 4
  ret i32 %76
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define i32 @vec_eq_helper(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj*, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  %11 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %12 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %11, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.76, i64 0, i64 0))
  store %struct.ScmObj* %12, %struct.ScmObj** %6, align 8
  %13 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %14 = call %struct.ScmObj* @unwrap_vector(%struct.ScmObj* %13, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.77, i64 0, i64 0))
  store %struct.ScmObj* %14, %struct.ScmObj** %7, align 8
  %15 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %16 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %15, i64 0
  %17 = call i64 @unwrap_int(%struct.ScmObj* %16, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.78, i64 0, i64 0))
  store i64 %17, i64* %8, align 8
  %18 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i64 0
  %20 = call i64 @unwrap_int(%struct.ScmObj* %19, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.79, i64 0, i64 0))
  store i64 %20, i64* %9, align 8
  %21 = load i64, i64* %8, align 8
  %22 = load i64, i64* %9, align 8
  %23 = icmp ne i64 %21, %22
  br i1 %23, label %24, label %25

24:                                               ; preds = %2
  store i32 0, i32* %3, align 4
  br label %45

25:                                               ; preds = %2
  store i64 0, i64* %10, align 8
  br label %26

26:                                               ; preds = %41, %25
  %27 = load i64, i64* %10, align 8
  %28 = load i64, i64* %8, align 8
  %29 = icmp sle i64 %27, %28
  br i1 %29, label %30, label %44

30:                                               ; preds = %26
  %31 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %32 = load i64, i64* %10, align 8
  %33 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %31, i64 %32
  %34 = load %struct.ScmObj*, %struct.ScmObj** %7, align 8
  %35 = load i64, i64* %10, align 8
  %36 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %34, i64 %35
  %37 = call i32 @eq_helper(%struct.ScmObj* %33, %struct.ScmObj* %36)
  %38 = icmp eq i32 %37, 0
  br i1 %38, label %39, label %40

39:                                               ; preds = %30
  store i32 0, i32* %3, align 4
  br label %45

40:                                               ; preds = %30
  br label %41

41:                                               ; preds = %40
  %42 = load i64, i64* %10, align 8
  %43 = add nsw i64 %42, 1
  store i64 %43, i64* %10, align 8
  br label %26, !llvm.loop !13

44:                                               ; preds = %26
  store i32 1, i32* %3, align 4
  br label %45

45:                                               ; preds = %44, %39, %24
  %46 = load i32, i32* %3, align 4
  ret i32 %46
}

declare i32 @strcmp(i8*, i8*) #4

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_eq_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.89, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.89, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.89, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.89, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([16 x i8], [16 x i8]* @.str.89, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_eq_63(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_eq_63(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %7 = call i32 @eq_helper(%struct.ScmObj* %5, %struct.ScmObj* %6)
  %8 = icmp eq i32 %7, 1
  %9 = call %struct.ScmObj* @make_predicate(i1 zeroext %8)
  ret %struct.ScmObj* %9
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @make_predicate(i1 zeroext %0) #0 {
  %2 = alloca i8, align 1
  %3 = zext i1 %0 to i8
  store i8 %3, i8* %2, align 1
  %4 = load i8, i8* %2, align 1
  %5 = trunc i8 %4 to i1
  br i1 %5, label %6, label %8

6:                                                ; preds = %1
  %7 = call %struct.ScmObj* @const_init_true()
  br label %10

8:                                                ; preds = %1
  %9 = call %struct.ScmObj* @const_init_false()
  br label %10

10:                                               ; preds = %8, %6
  %11 = phi %struct.ScmObj* [ %7, %6 ], [ %9, %8 ]
  ret %struct.ScmObj* %11
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_eqv_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.90, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.90, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.90, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.90, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.90, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %7 = call %struct.ScmObj* @prim_eq_63(%struct.ScmObj* %5, %struct.ScmObj* %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_equal_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.91, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.91, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.91, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.91, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([19 x i8], [19 x i8]* @.str.91, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %7 = call %struct.ScmObj* @prim_eq_63(%struct.ScmObj* %5, %struct.ScmObj* %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_number_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.92, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.92, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.92, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_number_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = call %struct.ScmObj* @prim_integer_63(%struct.ScmObj* %3)
  ret %struct.ScmObj* %4
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_integer_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 5
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_integer_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.93, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.93, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.93, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_integer_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_boolean_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.94, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.94, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.94, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_boolean_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_boolean_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 2
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_void_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.95, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.95, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.95, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_void_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_void_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 0
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_procedure_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.96, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.96, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([23 x i8], [23 x i8]* @.str.96, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_procedure_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_procedure_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 3
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_null_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.97, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.97, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.97, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_null_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 1
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_cons_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.98, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.98, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.98, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %3, i32 0, i32 1
  %5 = load i32, i32* %4, align 8
  %6 = icmp eq i32 %5, 4
  %7 = call %struct.ScmObj* @make_predicate(i1 zeroext %6)
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_cons(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.99, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.99, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.99, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.99, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.99, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_cons(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  %6 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %7 = call %struct.ScmObj* @alloc(i64 2)
  store %struct.ScmObj* %7, %struct.ScmObj** %5, align 8
  %8 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i64 0
  %11 = bitcast %struct.ScmObj* %10 to i8*
  %12 = bitcast %struct.ScmObj* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %11, i8* align 8 %12, i64 16, i1 false)
  %13 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %14 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %15 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %14, i64 1
  %16 = bitcast %struct.ScmObj* %15 to i8*
  %17 = bitcast %struct.ScmObj* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %16, i8* align 8 %17, i64 16, i1 false)
  %18 = call %struct.ScmObj* @alloc(i64 1)
  store %struct.ScmObj* %18, %struct.ScmObj** %6, align 8
  %19 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %20 = bitcast %struct.ScmObj* %19 to i64*
  %21 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i32 0, i32 0
  store i64* %20, i64** %22, align 8
  %23 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %24 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %23, i32 0, i32 1
  store i32 4, i32* %24, align 8
  %25 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  ret %struct.ScmObj* %25
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_car(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.100, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.100, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.100, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_car(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %4, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.101, i64 0, i64 0))
  store %struct.ScmObj* %5, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 0
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_cdr(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.102, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.102, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.102, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_cdr(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %4 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %5 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %4, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.103, i64 0, i64 0))
  store %struct.ScmObj* %5, %struct.ScmObj** %3, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i64 1
  ret %struct.ScmObj* %7
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__43(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.104, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.ScmObj* %9, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.105, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = add nsw i64 %11, %12
  %14 = call %struct.ScmObj* @const_init_int(i64 %13)
  ret %struct.ScmObj* %14
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim__43(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca i32, align 4
  %4 = alloca i64, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %8 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %7, i32 0, i32 1
  %9 = load i32, i32* %8, align 8
  store i32 %9, i32* %3, align 4
  %10 = load i32, i32* %3, align 4
  %11 = icmp ne i32 %10, 4
  br i1 %11, label %12, label %21

12:                                               ; preds = %1
  %13 = load i32, i32* %3, align 4
  %14 = icmp ne i32 %13, 1
  br i1 %14, label %15, label %21

15:                                               ; preds = %12
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %17 = load i32, i32* %3, align 4
  %18 = call i8* @get_type_name(i32 %17)
  %19 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.106, i64 0, i64 0), i8* %18)
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

21:                                               ; preds = %12, %1
  store i64 0, i64* %4, align 8
  br label %22

22:                                               ; preds = %27, %21
  %23 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %24 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %23, i32 0, i32 1
  %25 = load i32, i32* %24, align 8
  %26 = icmp eq i32 %25, 4
  br i1 %26, label %27, label %32

27:                                               ; preds = %22
  %28 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  call void @_get_both(%struct.ScmObj* %28, %struct.ScmObj* %5, %struct.ScmObj* %6)
  %29 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.107, i64 0, i64 0))
  %30 = load i64, i64* %4, align 8
  %31 = add nsw i64 %30, %29
  store i64 %31, i64* %4, align 8
  store %struct.ScmObj* %6, %struct.ScmObj** %2, align 8
  br label %22, !llvm.loop !14

32:                                               ; preds = %22
  %33 = load i64, i64* %4, align 8
  %34 = call %struct.ScmObj* @const_init_int(i64 %33)
  ret %struct.ScmObj* %34
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__45(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.108, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.ScmObj* %9, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.109, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = sub nsw i64 %11, %12
  %14 = call %struct.ScmObj* @const_init_int(i64 %13)
  ret %struct.ScmObj* %14
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim__45(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca %struct.ScmObj, align 8
  %10 = alloca %struct.ScmObj, align 8
  %11 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  %12 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %13 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %12, i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.110, i64 0, i64 0))
  store %struct.ScmObj* %13, %struct.ScmObj** %4, align 8
  %14 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %15 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %14, i64 0
  %16 = bitcast %struct.ScmObj* %5 to i8*
  %17 = bitcast %struct.ScmObj* %15 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %16, i8* align 8 %17, i64 16, i1 false)
  %18 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %19 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %18, i64 1
  %20 = bitcast %struct.ScmObj* %6 to i8*
  %21 = bitcast %struct.ScmObj* %19 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %20, i8* align 8 %21, i64 16, i1 false)
  %22 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.111, i64 0, i64 0))
  store i64 %22, i64* %7, align 8
  %23 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %24 = load i32, i32* %23, align 8
  %25 = icmp eq i32 %24, 1
  br i1 %25, label %26, label %30

26:                                               ; preds = %1
  %27 = load i64, i64* %7, align 8
  %28 = sub nsw i64 0, %27
  %29 = call %struct.ScmObj* @const_init_int(i64 %28)
  store %struct.ScmObj* %29, %struct.ScmObj** %2, align 8
  br label %47

30:                                               ; preds = %1
  %31 = load i64, i64* %7, align 8
  store i64 %31, i64* %8, align 8
  %32 = bitcast %struct.ScmObj* %9 to i8*
  %33 = bitcast %struct.ScmObj* %6 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %32, i8* align 8 %33, i64 16, i1 false)
  br label %34

34:                                               ; preds = %38, %30
  %35 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %36 = load i32, i32* %35, align 8
  %37 = icmp ne i32 %36, 1
  br i1 %37, label %38, label %44

38:                                               ; preds = %34
  call void @_get_both(%struct.ScmObj* %9, %struct.ScmObj* %10, %struct.ScmObj* %11)
  %39 = call i64 @unwrap_int(%struct.ScmObj* %10, i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str.112, i64 0, i64 0))
  %40 = load i64, i64* %8, align 8
  %41 = sub nsw i64 %40, %39
  store i64 %41, i64* %8, align 8
  %42 = bitcast %struct.ScmObj* %9 to i8*
  %43 = bitcast %struct.ScmObj* %11 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %42, i8* align 8 %43, i64 16, i1 false)
  br label %34, !llvm.loop !15

44:                                               ; preds = %34
  %45 = load i64, i64* %8, align 8
  %46 = call %struct.ScmObj* @const_init_int(i64 %45)
  store %struct.ScmObj* %46, %struct.ScmObj** %2, align 8
  br label %47

47:                                               ; preds = %44, %26
  %48 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  ret %struct.ScmObj* %48
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__42(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.113, i64 0, i64 0))
  %7 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.114, i64 0, i64 0))
  %9 = mul nsw i64 %6, %8
  %10 = call %struct.ScmObj* @const_init_int(i64 %9)
  ret %struct.ScmObj* %10
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim__42(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp eq i32 %11, 1
  br i1 %12, label %13, label %15

13:                                               ; preds = %1
  %14 = call %struct.ScmObj* @const_init_int(i64 1)
  store %struct.ScmObj* %14, %struct.ScmObj** %2, align 8
  br label %42

15:                                               ; preds = %1
  %16 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %17 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %16, i32 0, i32 1
  %18 = load i32, i32* %17, align 8
  %19 = icmp eq i32 %18, 4
  br i1 %19, label %20, label %38

20:                                               ; preds = %15
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %21, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.115, i64 0, i64 0))
  store %struct.ScmObj* %22, %struct.ScmObj** %4, align 8
  %23 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %24 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %23, i64 0
  %25 = bitcast %struct.ScmObj* %5 to i8*
  %26 = bitcast %struct.ScmObj* %24 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %25, i8* align 8 %26, i64 16, i1 false)
  %27 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %27, i64 1
  %29 = bitcast %struct.ScmObj* %6 to i8*
  %30 = bitcast %struct.ScmObj* %28 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %29, i8* align 8 %30, i64 16, i1 false)
  %31 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.116, i64 0, i64 0))
  store i64 %31, i64* %7, align 8
  %32 = call %struct.ScmObj* @applyprim__42(%struct.ScmObj* %6)
  %33 = call i64 @unwrap_int(%struct.ScmObj* %32, i8* getelementptr inbounds ([15 x i8], [15 x i8]* @.str.117, i64 0, i64 0))
  store i64 %33, i64* %8, align 8
  %34 = load i64, i64* %7, align 8
  %35 = load i64, i64* %8, align 8
  %36 = mul nsw i64 %34, %35
  %37 = call %struct.ScmObj* @const_init_int(i64 %36)
  store %struct.ScmObj* %37, %struct.ScmObj** %2, align 8
  br label %42

38:                                               ; preds = %15
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %40 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.118, i64 0, i64 0))
  %41 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

42:                                               ; preds = %20, %13
  %43 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  ret %struct.ScmObj* %43
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__47(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %7 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.119, i64 0, i64 0))
  store i64 %8, i64* %5, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %10 = call i64 @unwrap_int(%struct.ScmObj* %9, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.120, i64 0, i64 0))
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %5, align 8
  %12 = load i64, i64* %6, align 8
  %13 = sdiv i64 %11, %12
  %14 = call %struct.ScmObj* @const_init_int(i64 %13)
  ret %struct.ScmObj* %14
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim__61(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.121, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.121, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.121, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.121, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.121, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__61(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  %5 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %4, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %5, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %7 = call i64 @unwrap_int(%struct.ScmObj* %6, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.122, i64 0, i64 0))
  %8 = load %struct.ScmObj*, %struct.ScmObj** %5, align 8
  %9 = call i64 @unwrap_int(%struct.ScmObj* %8, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.123, i64 0, i64 0))
  %10 = icmp eq i64 %7, %9
  br i1 %10, label %11, label %13

11:                                               ; preds = %2
  %12 = call %struct.ScmObj* @const_init_true()
  store %struct.ScmObj* %12, %struct.ScmObj** %3, align 8
  br label %15

13:                                               ; preds = %2
  %14 = call %struct.ScmObj* @const_init_false()
  store %struct.ScmObj* %14, %struct.ScmObj** %3, align 8
  br label %15

15:                                               ; preds = %13, %11
  %16 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  ret %struct.ScmObj* %16
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__60(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.124, i64 0, i64 0))
  %7 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.125, i64 0, i64 0))
  %9 = icmp slt i64 %6, %8
  %10 = call %struct.ScmObj* @make_predicate(i1 zeroext %9)
  ret %struct.ScmObj* %10
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim__60_61(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  %6 = alloca %struct.ScmObj*, align 8
  %7 = alloca %struct.ScmObj, align 8
  %8 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %9 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %10 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %9, i32 0, i32 1
  %11 = load i32, i32* %10, align 8
  %12 = icmp ne i32 %11, 4
  br i1 %12, label %13, label %17

13:                                               ; preds = %1
  %14 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.126, i64 0, i64 0))
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

17:                                               ; preds = %1
  %18 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %19 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %18, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.126, i64 0, i64 0))
  store %struct.ScmObj* %19, %struct.ScmObj** %3, align 8
  %20 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %21 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %20, i64 0
  %22 = bitcast %struct.ScmObj* %4 to i8*
  %23 = bitcast %struct.ScmObj* %21 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %22, i8* align 8 %23, i64 16, i1 false)
  %24 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %24, i64 1
  %26 = bitcast %struct.ScmObj* %5 to i8*
  %27 = bitcast %struct.ScmObj* %25 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %26, i8* align 8 %27, i64 16, i1 false)
  %28 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %29 = load i32, i32* %28, align 8
  %30 = icmp ne i32 %29, 4
  br i1 %30, label %31, label %35

31:                                               ; preds = %17
  %32 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %33 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.57, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.126, i64 0, i64 0))
  %34 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

35:                                               ; preds = %17
  %36 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %5, i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.126, i64 0, i64 0))
  store %struct.ScmObj* %36, %struct.ScmObj** %6, align 8
  %37 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %38 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %37, i64 0
  %39 = bitcast %struct.ScmObj* %7 to i8*
  %40 = bitcast %struct.ScmObj* %38 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %39, i8* align 8 %40, i64 16, i1 false)
  %41 = load %struct.ScmObj*, %struct.ScmObj** %6, align 8
  %42 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %41, i64 1
  %43 = bitcast %struct.ScmObj* %8 to i8*
  %44 = bitcast %struct.ScmObj* %42 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %43, i8* align 8 %44, i64 16, i1 false)
  %45 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %8, i32 0, i32 1
  %46 = load i32, i32* %45, align 8
  %47 = icmp ne i32 %46, 1
  br i1 %47, label %48, label %52

48:                                               ; preds = %35
  %49 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([38 x i8], [38 x i8]* @.str.58, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.126, i64 0, i64 0))
  %51 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

52:                                               ; preds = %35
  %53 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %4, %struct.ScmObj* %7)
  ret %struct.ScmObj* %53
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim__60_61(%struct.ScmObj* %0, %struct.ScmObj* %1) #0 {
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %3, align 8
  store %struct.ScmObj* %1, %struct.ScmObj** %4, align 8
  %5 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %6 = call i64 @unwrap_int(%struct.ScmObj* %5, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.127, i64 0, i64 0))
  %7 = load %struct.ScmObj*, %struct.ScmObj** %4, align 8
  %8 = call i64 @unwrap_int(%struct.ScmObj* %7, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.128, i64 0, i64 0))
  %9 = icmp sle i64 %6, %8
  %10 = call %struct.ScmObj* @make_predicate(i1 zeroext %9)
  ret %struct.ScmObj* %10
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @applyprim_not(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  %3 = alloca %struct.ScmObj*, align 8
  %4 = alloca %struct.ScmObj, align 8
  %5 = alloca %struct.ScmObj, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %6 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %7 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %6, i32 0, i32 1
  %8 = load i32, i32* %7, align 8
  %9 = icmp ne i32 %8, 4
  br i1 %9, label %10, label %14

10:                                               ; preds = %1
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %12 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([55 x i8], [55 x i8]* @.str.13, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.129, i64 0, i64 0))
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

14:                                               ; preds = %1
  %15 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %16 = call %struct.ScmObj* @unwrap_cons(%struct.ScmObj* %15, i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.129, i64 0, i64 0))
  store %struct.ScmObj* %16, %struct.ScmObj** %3, align 8
  %17 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %18 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %17, i64 0
  %19 = bitcast %struct.ScmObj* %4 to i8*
  %20 = bitcast %struct.ScmObj* %18 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %19, i8* align 8 %20, i64 16, i1 false)
  %21 = load %struct.ScmObj*, %struct.ScmObj** %3, align 8
  %22 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %21, i64 1
  %23 = bitcast %struct.ScmObj* %5 to i8*
  %24 = bitcast %struct.ScmObj* %22 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* align 8 %23, i8* align 8 %24, i64 16, i1 false)
  %25 = getelementptr inbounds %struct.ScmObj, %struct.ScmObj* %5, i32 0, i32 1
  %26 = load i32, i32* %25, align 8
  %27 = icmp ne i32 %26, 1
  br i1 %27, label %28, label %32

28:                                               ; preds = %14
  %29 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.3, i64 0, i64 0))
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.15, i64 0, i64 0), i8* getelementptr inbounds ([14 x i8], [14 x i8]* @.str.129, i64 0, i64 0))
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.5, i64 0, i64 0))
  call void @exit(i32 1) #9
  unreachable

32:                                               ; preds = %14
  %33 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %4)
  ret %struct.ScmObj* %33
}

; Function Attrs: noinline optnone ssp uwtable mustprogress
define %struct.ScmObj* @prim_not(%struct.ScmObj* %0) #0 {
  %2 = alloca %struct.ScmObj*, align 8
  store %struct.ScmObj* %0, %struct.ScmObj** %2, align 8
  %3 = load %struct.ScmObj*, %struct.ScmObj** %2, align 8
  %4 = call i64 @unwrap_bool(%struct.ScmObj* %3, i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.130, i64 0, i64 0))
  %5 = icmp eq i64 %4, 0
  %6 = call %struct.ScmObj* @make_predicate(i1 zeroext %5)
  ret %struct.ScmObj* %6
}

attributes #0 = { noinline optnone ssp uwtable mustprogress "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nofree nosync nounwind readnone speculatable willreturn }
attributes #2 = { nobuiltin allocsize(0) "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nofree nosync nounwind willreturn }
attributes #4 = { "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { noreturn "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { noinline nounwind optnone ssp uwtable mustprogress "disable-tail-calls"="false" "frame-pointer"="non-leaf" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="true" "probe-stack"="__chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="apple-m1" "target-features"="+aes,+crc,+crypto,+dotprod,+fp-armv8,+fp16fml,+fullfp16,+lse,+neon,+ras,+rcpc,+rdm,+sha2,+sha3,+sm4,+v8.5a,+zcm,+zcz" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { argmemonly nofree nosync nounwind willreturn writeonly }
attributes #8 = { builtin allocsize(0) }
attributes #9 = { noreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5, !6}
!llvm.ident = !{!7}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 12, i32 1]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 1, !"branch-target-enforcement", i32 0}
!3 = !{i32 1, !"sign-return-address", i32 0}
!4 = !{i32 1, !"sign-return-address-all", i32 0}
!5 = !{i32 1, !"sign-return-address-with-bkey", i32 0}
!6 = !{i32 7, !"PIC Level", i32 2}
!7 = !{!"Apple clang version 13.0.0 (clang-1300.0.29.3)"}
!8 = distinct !{!8, !9}
!9 = !{!"llvm.loop.mustprogress"}
!10 = distinct !{!10, !9}
!11 = distinct !{!11, !9}
!12 = distinct !{!12, !9}
!13 = distinct !{!13, !9}
!14 = distinct !{!14, !9}
!15 = distinct !{!15, !9}


;;;header ended;;;

@global$sym$ae4392647575 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv47035 = call %struct.ScmObj* @const_init_null()
%mainargs47036 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv47035, %struct.ScmObj* %mainargs47036)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv47033,%struct.ScmObj* %mainargs47034) {
%stackaddr$makeclosure47037 = alloca %struct.ScmObj*, align 8
%fptrToInt47038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40538 to i64
%ae40538 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47038)
store volatile %struct.ScmObj* %ae40538, %struct.ScmObj** %stackaddr$makeclosure47037, align 8
%ae40539 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47039 = alloca %struct.ScmObj*, align 8
%fptrToInt47040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40540 to i64
%ae40540 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47040)
store volatile %struct.ScmObj* %ae40540, %struct.ScmObj** %stackaddr$makeclosure47039, align 8
%args47032$ae40538$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47041 = alloca %struct.ScmObj*, align 8
%args47032$ae40538$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40540, %struct.ScmObj* %args47032$ae40538$0)
store volatile %struct.ScmObj* %args47032$ae40538$1, %struct.ScmObj** %stackaddr$prim47041, align 8
%stackaddr$prim47042 = alloca %struct.ScmObj*, align 8
%args47032$ae40538$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40539, %struct.ScmObj* %args47032$ae40538$1)
store volatile %struct.ScmObj* %args47032$ae40538$2, %struct.ScmObj** %stackaddr$prim47042, align 8
%clofunc47043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40538)
musttail call tailcc void %clofunc47043(%struct.ScmObj* %ae40538, %struct.ScmObj* %args47032$ae40538$2)
ret void
}

define tailcc void @proc_clo$ae40538(%struct.ScmObj* %env$ae40538,%struct.ScmObj* %current_45args46446) {
%stackaddr$prim47044 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim47044, align 8
%stackaddr$prim47045 = alloca %struct.ScmObj*, align 8
%current_45args46447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %current_45args46447, %struct.ScmObj** %stackaddr$prim47045, align 8
%stackaddr$prim47046 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46447)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim47046, align 8
%stackaddr$makeclosure47047 = alloca %struct.ScmObj*, align 8
%fptrToInt47048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40553 to i64
%ae40553 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47048)
store volatile %struct.ScmObj* %ae40553, %struct.ScmObj** %stackaddr$makeclosure47047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40553, %struct.ScmObj* %anf_45bind40229, i64 0)
%ae40554 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47049 = alloca %struct.ScmObj*, align 8
%fptrToInt47050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40555 to i64
%ae40555 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47050)
store volatile %struct.ScmObj* %ae40555, %struct.ScmObj** %stackaddr$makeclosure47049, align 8
%args47027$ae40553$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47051 = alloca %struct.ScmObj*, align 8
%args47027$ae40553$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40555, %struct.ScmObj* %args47027$ae40553$0)
store volatile %struct.ScmObj* %args47027$ae40553$1, %struct.ScmObj** %stackaddr$prim47051, align 8
%stackaddr$prim47052 = alloca %struct.ScmObj*, align 8
%args47027$ae40553$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40554, %struct.ScmObj* %args47027$ae40553$1)
store volatile %struct.ScmObj* %args47027$ae40553$2, %struct.ScmObj** %stackaddr$prim47052, align 8
%clofunc47053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40553)
musttail call tailcc void %clofunc47053(%struct.ScmObj* %ae40553, %struct.ScmObj* %args47027$ae40553$2)
ret void
}

define tailcc void @proc_clo$ae40553(%struct.ScmObj* %env$ae40553,%struct.ScmObj* %current_45args46449) {
%stackaddr$env-ref47054 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40553, i64 0)
store %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$env-ref47054
%stackaddr$prim47055 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim47055, align 8
%stackaddr$prim47056 = alloca %struct.ScmObj*, align 8
%current_45args46450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %current_45args46450, %struct.ScmObj** %stackaddr$prim47056, align 8
%stackaddr$prim47057 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46450)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim47057, align 8
%stackaddr$makeclosure47058 = alloca %struct.ScmObj*, align 8
%fptrToInt47059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40668 to i64
%ae40668 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47059)
store volatile %struct.ScmObj* %ae40668, %struct.ScmObj** %stackaddr$makeclosure47058, align 8
%args47006$anf_45bind40229$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47060 = alloca %struct.ScmObj*, align 8
%args47006$anf_45bind40229$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args47006$anf_45bind40229$0)
store volatile %struct.ScmObj* %args47006$anf_45bind40229$1, %struct.ScmObj** %stackaddr$prim47060, align 8
%stackaddr$prim47061 = alloca %struct.ScmObj*, align 8
%args47006$anf_45bind40229$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40668, %struct.ScmObj* %args47006$anf_45bind40229$1)
store volatile %struct.ScmObj* %args47006$anf_45bind40229$2, %struct.ScmObj** %stackaddr$prim47061, align 8
%clofunc47062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40229)
musttail call tailcc void %clofunc47062(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %args47006$anf_45bind40229$2)
ret void
}

define tailcc void @proc_clo$ae40668(%struct.ScmObj* %env$ae40668,%struct.ScmObj* %current_45args46452) {
%stackaddr$prim47063 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim47063, align 8
%stackaddr$prim47064 = alloca %struct.ScmObj*, align 8
%current_45args46453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %current_45args46453, %struct.ScmObj** %stackaddr$prim47064, align 8
%stackaddr$prim47065 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46453)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47065, align 8
%stackaddr$makeclosure47066 = alloca %struct.ScmObj*, align 8
%fptrToInt47067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40670 to i64
%ae40670 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47067)
store volatile %struct.ScmObj* %ae40670, %struct.ScmObj** %stackaddr$makeclosure47066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40670, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40671 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47068 = alloca %struct.ScmObj*, align 8
%fptrToInt47069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40672 to i64
%ae40672 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47069)
store volatile %struct.ScmObj* %ae40672, %struct.ScmObj** %stackaddr$makeclosure47068, align 8
%args47005$ae40670$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47070 = alloca %struct.ScmObj*, align 8
%args47005$ae40670$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40672, %struct.ScmObj* %args47005$ae40670$0)
store volatile %struct.ScmObj* %args47005$ae40670$1, %struct.ScmObj** %stackaddr$prim47070, align 8
%stackaddr$prim47071 = alloca %struct.ScmObj*, align 8
%args47005$ae40670$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40671, %struct.ScmObj* %args47005$ae40670$1)
store volatile %struct.ScmObj* %args47005$ae40670$2, %struct.ScmObj** %stackaddr$prim47071, align 8
%clofunc47072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40670)
musttail call tailcc void %clofunc47072(%struct.ScmObj* %ae40670, %struct.ScmObj* %args47005$ae40670$2)
ret void
}

define tailcc void @proc_clo$ae40670(%struct.ScmObj* %env$ae40670,%struct.ScmObj* %current_45args46455) {
%stackaddr$env-ref47073 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40670, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47073
%stackaddr$prim47074 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim47074, align 8
%stackaddr$prim47075 = alloca %struct.ScmObj*, align 8
%current_45args46456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %current_45args46456, %struct.ScmObj** %stackaddr$prim47075, align 8
%stackaddr$prim47076 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46456)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim47076, align 8
%stackaddr$makeclosure47077 = alloca %struct.ScmObj*, align 8
%fptrToInt47078 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40748 to i64
%ae40748 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47078)
store volatile %struct.ScmObj* %ae40748, %struct.ScmObj** %stackaddr$makeclosure47077, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40748, %struct.ScmObj* %Ycmb40102, i64 0)
%args46989$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47079 = alloca %struct.ScmObj*, align 8
%args46989$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args46989$Ycmb40102$0)
store volatile %struct.ScmObj* %args46989$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47079, align 8
%stackaddr$prim47080 = alloca %struct.ScmObj*, align 8
%args46989$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40748, %struct.ScmObj* %args46989$Ycmb40102$1)
store volatile %struct.ScmObj* %args46989$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47080, align 8
%clofunc47081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47081(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46989$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40748(%struct.ScmObj* %env$ae40748,%struct.ScmObj* %current_45args46458) {
%stackaddr$env-ref47082 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40748, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47082
%stackaddr$prim47083 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47083, align 8
%stackaddr$prim47084 = alloca %struct.ScmObj*, align 8
%current_45args46459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %current_45args46459, %struct.ScmObj** %stackaddr$prim47084, align 8
%stackaddr$prim47085 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46459)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47085, align 8
%stackaddr$makeclosure47086 = alloca %struct.ScmObj*, align 8
%fptrToInt47087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40750 to i64
%ae40750 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47087)
store volatile %struct.ScmObj* %ae40750, %struct.ScmObj** %stackaddr$makeclosure47086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40750, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40750, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47088 = alloca %struct.ScmObj*, align 8
%fptrToInt47089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40752 to i64
%ae40752 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47089)
store volatile %struct.ScmObj* %ae40752, %struct.ScmObj** %stackaddr$makeclosure47088, align 8
%args46988$ae40750$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47090 = alloca %struct.ScmObj*, align 8
%args46988$ae40750$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40752, %struct.ScmObj* %args46988$ae40750$0)
store volatile %struct.ScmObj* %args46988$ae40750$1, %struct.ScmObj** %stackaddr$prim47090, align 8
%stackaddr$prim47091 = alloca %struct.ScmObj*, align 8
%args46988$ae40750$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40751, %struct.ScmObj* %args46988$ae40750$1)
store volatile %struct.ScmObj* %args46988$ae40750$2, %struct.ScmObj** %stackaddr$prim47091, align 8
%clofunc47092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40750)
musttail call tailcc void %clofunc47092(%struct.ScmObj* %ae40750, %struct.ScmObj* %args46988$ae40750$2)
ret void
}

define tailcc void @proc_clo$ae40750(%struct.ScmObj* %env$ae40750,%struct.ScmObj* %current_45args46461) {
%stackaddr$env-ref47093 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40750, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47093
%stackaddr$env-ref47094 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40750, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47094
%stackaddr$prim47095 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47095, align 8
%stackaddr$prim47096 = alloca %struct.ScmObj*, align 8
%current_45args46462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %current_45args46462, %struct.ScmObj** %stackaddr$prim47096, align 8
%stackaddr$prim47097 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46462)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim47097, align 8
%stackaddr$makeclosure47098 = alloca %struct.ScmObj*, align 8
%fptrToInt47099 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40845 to i64
%ae40845 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47099)
store volatile %struct.ScmObj* %ae40845, %struct.ScmObj** %stackaddr$makeclosure47098, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40845, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40845, %struct.ScmObj* %_37foldr140123, i64 1)
%args46969$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47100 = alloca %struct.ScmObj*, align 8
%args46969$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %args46969$Ycmb40102$0)
store volatile %struct.ScmObj* %args46969$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47100, align 8
%stackaddr$prim47101 = alloca %struct.ScmObj*, align 8
%args46969$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40845, %struct.ScmObj* %args46969$Ycmb40102$1)
store volatile %struct.ScmObj* %args46969$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47101, align 8
%clofunc47102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47102(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46969$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40845(%struct.ScmObj* %env$ae40845,%struct.ScmObj* %current_45args46464) {
%stackaddr$env-ref47103 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40845, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47103
%stackaddr$env-ref47104 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40845, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47104
%stackaddr$prim47105 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47105, align 8
%stackaddr$prim47106 = alloca %struct.ScmObj*, align 8
%current_45args46465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %current_45args46465, %struct.ScmObj** %stackaddr$prim47106, align 8
%stackaddr$prim47107 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46465)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47107, align 8
%stackaddr$makeclosure47108 = alloca %struct.ScmObj*, align 8
%fptrToInt47109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40847 to i64
%ae40847 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47109)
store volatile %struct.ScmObj* %ae40847, %struct.ScmObj** %stackaddr$makeclosure47108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40848 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47110 = alloca %struct.ScmObj*, align 8
%fptrToInt47111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40849 to i64
%ae40849 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47111)
store volatile %struct.ScmObj* %ae40849, %struct.ScmObj** %stackaddr$makeclosure47110, align 8
%args46968$ae40847$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47112 = alloca %struct.ScmObj*, align 8
%args46968$ae40847$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40849, %struct.ScmObj* %args46968$ae40847$0)
store volatile %struct.ScmObj* %args46968$ae40847$1, %struct.ScmObj** %stackaddr$prim47112, align 8
%stackaddr$prim47113 = alloca %struct.ScmObj*, align 8
%args46968$ae40847$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40848, %struct.ScmObj* %args46968$ae40847$1)
store volatile %struct.ScmObj* %args46968$ae40847$2, %struct.ScmObj** %stackaddr$prim47113, align 8
%clofunc47114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40847)
musttail call tailcc void %clofunc47114(%struct.ScmObj* %ae40847, %struct.ScmObj* %args46968$ae40847$2)
ret void
}

define tailcc void @proc_clo$ae40847(%struct.ScmObj* %env$ae40847,%struct.ScmObj* %current_45args46467) {
%stackaddr$env-ref47115 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47115
%stackaddr$env-ref47116 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47116
%stackaddr$env-ref47117 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47117
%stackaddr$prim47118 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47118, align 8
%stackaddr$prim47119 = alloca %struct.ScmObj*, align 8
%current_45args46468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %current_45args46468, %struct.ScmObj** %stackaddr$prim47119, align 8
%stackaddr$prim47120 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46468)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim47120, align 8
%stackaddr$makeclosure47121 = alloca %struct.ScmObj*, align 8
%fptrToInt47122 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40995 to i64
%ae40995 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47122)
store volatile %struct.ScmObj* %ae40995, %struct.ScmObj** %stackaddr$makeclosure47121, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40995, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40995, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40995, %struct.ScmObj* %_37foldr140123, i64 2)
%args46952$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47123 = alloca %struct.ScmObj*, align 8
%args46952$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args46952$Ycmb40102$0)
store volatile %struct.ScmObj* %args46952$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47123, align 8
%stackaddr$prim47124 = alloca %struct.ScmObj*, align 8
%args46952$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40995, %struct.ScmObj* %args46952$Ycmb40102$1)
store volatile %struct.ScmObj* %args46952$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47124, align 8
%clofunc47125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47125(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46952$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40995(%struct.ScmObj* %env$ae40995,%struct.ScmObj* %current_45args46470) {
%stackaddr$env-ref47126 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40995, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47126
%stackaddr$env-ref47127 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40995, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47127
%stackaddr$env-ref47128 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40995, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47128
%stackaddr$prim47129 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47129, align 8
%stackaddr$prim47130 = alloca %struct.ScmObj*, align 8
%current_45args46471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %current_45args46471, %struct.ScmObj** %stackaddr$prim47130, align 8
%stackaddr$prim47131 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46471)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47131, align 8
%stackaddr$makeclosure47132 = alloca %struct.ScmObj*, align 8
%fptrToInt47133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40997 to i64
%ae40997 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47133)
store volatile %struct.ScmObj* %ae40997, %struct.ScmObj** %stackaddr$makeclosure47132, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40997, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40997, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40997, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40997, %struct.ScmObj* %_37foldr140123, i64 3)
%ae40998 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47134 = alloca %struct.ScmObj*, align 8
%fptrToInt47135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40999 to i64
%ae40999 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47135)
store volatile %struct.ScmObj* %ae40999, %struct.ScmObj** %stackaddr$makeclosure47134, align 8
%args46951$ae40997$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47136 = alloca %struct.ScmObj*, align 8
%args46951$ae40997$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40999, %struct.ScmObj* %args46951$ae40997$0)
store volatile %struct.ScmObj* %args46951$ae40997$1, %struct.ScmObj** %stackaddr$prim47136, align 8
%stackaddr$prim47137 = alloca %struct.ScmObj*, align 8
%args46951$ae40997$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40998, %struct.ScmObj* %args46951$ae40997$1)
store volatile %struct.ScmObj* %args46951$ae40997$2, %struct.ScmObj** %stackaddr$prim47137, align 8
%clofunc47138 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40997)
musttail call tailcc void %clofunc47138(%struct.ScmObj* %ae40997, %struct.ScmObj* %args46951$ae40997$2)
ret void
}

define tailcc void @proc_clo$ae40997(%struct.ScmObj* %env$ae40997,%struct.ScmObj* %current_45args46473) {
%stackaddr$env-ref47139 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40997, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47139
%stackaddr$env-ref47140 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40997, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47140
%stackaddr$env-ref47141 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40997, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47141
%stackaddr$env-ref47142 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40997, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47142
%stackaddr$prim47143 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47143, align 8
%stackaddr$prim47144 = alloca %struct.ScmObj*, align 8
%current_45args46474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %current_45args46474, %struct.ScmObj** %stackaddr$prim47144, align 8
%stackaddr$prim47145 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46474)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim47145, align 8
%stackaddr$makeclosure47146 = alloca %struct.ScmObj*, align 8
%fptrToInt47147 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41078 to i64
%ae41078 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47147)
store volatile %struct.ScmObj* %ae41078, %struct.ScmObj** %stackaddr$makeclosure47146, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41078, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41078, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41078, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41078, %struct.ScmObj* %_37foldr140123, i64 3)
%args46937$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47148 = alloca %struct.ScmObj*, align 8
%args46937$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args46937$Ycmb40102$0)
store volatile %struct.ScmObj* %args46937$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47148, align 8
%stackaddr$prim47149 = alloca %struct.ScmObj*, align 8
%args46937$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41078, %struct.ScmObj* %args46937$Ycmb40102$1)
store volatile %struct.ScmObj* %args46937$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47149, align 8
%clofunc47150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47150(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46937$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41078(%struct.ScmObj* %env$ae41078,%struct.ScmObj* %current_45args46476) {
%stackaddr$env-ref47151 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41078, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47151
%stackaddr$env-ref47152 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41078, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47152
%stackaddr$env-ref47153 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41078, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47153
%stackaddr$env-ref47154 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41078, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47154
%stackaddr$prim47155 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47155, align 8
%stackaddr$prim47156 = alloca %struct.ScmObj*, align 8
%current_45args46477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %current_45args46477, %struct.ScmObj** %stackaddr$prim47156, align 8
%stackaddr$prim47157 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46477)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47157, align 8
%stackaddr$makeclosure47158 = alloca %struct.ScmObj*, align 8
%fptrToInt47159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41080 to i64
%ae41080 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47159)
store volatile %struct.ScmObj* %ae41080, %struct.ScmObj** %stackaddr$makeclosure47158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41081 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47160 = alloca %struct.ScmObj*, align 8
%fptrToInt47161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41082 to i64
%ae41082 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47161)
store volatile %struct.ScmObj* %ae41082, %struct.ScmObj** %stackaddr$makeclosure47160, align 8
%args46936$ae41080$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47162 = alloca %struct.ScmObj*, align 8
%args46936$ae41080$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41082, %struct.ScmObj* %args46936$ae41080$0)
store volatile %struct.ScmObj* %args46936$ae41080$1, %struct.ScmObj** %stackaddr$prim47162, align 8
%stackaddr$prim47163 = alloca %struct.ScmObj*, align 8
%args46936$ae41080$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41081, %struct.ScmObj* %args46936$ae41080$1)
store volatile %struct.ScmObj* %args46936$ae41080$2, %struct.ScmObj** %stackaddr$prim47163, align 8
%clofunc47164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41080)
musttail call tailcc void %clofunc47164(%struct.ScmObj* %ae41080, %struct.ScmObj* %args46936$ae41080$2)
ret void
}

define tailcc void @proc_clo$ae41080(%struct.ScmObj* %env$ae41080,%struct.ScmObj* %current_45args46479) {
%stackaddr$env-ref47165 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47165
%stackaddr$env-ref47166 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47166
%stackaddr$env-ref47167 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47167
%stackaddr$env-ref47168 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47168
%stackaddr$env-ref47169 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47169
%stackaddr$prim47170 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47170, align 8
%stackaddr$prim47171 = alloca %struct.ScmObj*, align 8
%current_45args46480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %current_45args46480, %struct.ScmObj** %stackaddr$prim47171, align 8
%stackaddr$prim47172 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46480)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim47172, align 8
%stackaddr$makeclosure47173 = alloca %struct.ScmObj*, align 8
%fptrToInt47174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41157 to i64
%ae41157 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47174)
store volatile %struct.ScmObj* %ae41157, %struct.ScmObj** %stackaddr$makeclosure47173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41157, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41157, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41157, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41157, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41157, %struct.ScmObj* %_37foldr140123, i64 4)
%args46920$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47175 = alloca %struct.ScmObj*, align 8
%args46920$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args46920$Ycmb40102$0)
store volatile %struct.ScmObj* %args46920$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47175, align 8
%stackaddr$prim47176 = alloca %struct.ScmObj*, align 8
%args46920$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41157, %struct.ScmObj* %args46920$Ycmb40102$1)
store volatile %struct.ScmObj* %args46920$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47176, align 8
%clofunc47177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47177(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46920$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41157(%struct.ScmObj* %env$ae41157,%struct.ScmObj* %current_45args46482) {
%stackaddr$env-ref47178 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41157, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47178
%stackaddr$env-ref47179 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41157, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47179
%stackaddr$env-ref47180 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41157, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47180
%stackaddr$env-ref47181 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41157, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47181
%stackaddr$env-ref47182 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41157, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47182
%stackaddr$prim47183 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47183, align 8
%stackaddr$prim47184 = alloca %struct.ScmObj*, align 8
%current_45args46483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %current_45args46483, %struct.ScmObj** %stackaddr$prim47184, align 8
%stackaddr$prim47185 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46483)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47185, align 8
%stackaddr$makeclosure47186 = alloca %struct.ScmObj*, align 8
%fptrToInt47187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41159 to i64
%ae41159 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47187)
store volatile %struct.ScmObj* %ae41159, %struct.ScmObj** %stackaddr$makeclosure47186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41159, %struct.ScmObj* %_37length40112, i64 5)
%ae41160 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47188 = alloca %struct.ScmObj*, align 8
%fptrToInt47189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41161 to i64
%ae41161 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47189)
store volatile %struct.ScmObj* %ae41161, %struct.ScmObj** %stackaddr$makeclosure47188, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41161, %struct.ScmObj* %_37foldl140107, i64 0)
%args46919$ae41159$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47190 = alloca %struct.ScmObj*, align 8
%args46919$ae41159$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41161, %struct.ScmObj* %args46919$ae41159$0)
store volatile %struct.ScmObj* %args46919$ae41159$1, %struct.ScmObj** %stackaddr$prim47190, align 8
%stackaddr$prim47191 = alloca %struct.ScmObj*, align 8
%args46919$ae41159$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41160, %struct.ScmObj* %args46919$ae41159$1)
store volatile %struct.ScmObj* %args46919$ae41159$2, %struct.ScmObj** %stackaddr$prim47191, align 8
%clofunc47192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41159)
musttail call tailcc void %clofunc47192(%struct.ScmObj* %ae41159, %struct.ScmObj* %args46919$ae41159$2)
ret void
}

define tailcc void @proc_clo$ae41159(%struct.ScmObj* %env$ae41159,%struct.ScmObj* %current_45args46485) {
%stackaddr$env-ref47193 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47193
%stackaddr$env-ref47194 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47194
%stackaddr$env-ref47195 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47195
%stackaddr$env-ref47196 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47196
%stackaddr$env-ref47197 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47197
%stackaddr$env-ref47198 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41159, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47198
%stackaddr$prim47199 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47199, align 8
%stackaddr$prim47200 = alloca %struct.ScmObj*, align 8
%current_45args46486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %current_45args46486, %struct.ScmObj** %stackaddr$prim47200, align 8
%stackaddr$prim47201 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46486)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47201, align 8
%stackaddr$makeclosure47202 = alloca %struct.ScmObj*, align 8
%fptrToInt47203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41213 to i64
%ae41213 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47203)
store volatile %struct.ScmObj* %ae41213, %struct.ScmObj** %stackaddr$makeclosure47202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37last40145, i64 4)
%ae41214 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47204 = alloca %struct.ScmObj*, align 8
%fptrToInt47205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41215 to i64
%ae41215 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47205)
store volatile %struct.ScmObj* %ae41215, %struct.ScmObj** %stackaddr$makeclosure47204, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41215, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41215, %struct.ScmObj* %_37length40112, i64 1)
%args46905$ae41213$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47206 = alloca %struct.ScmObj*, align 8
%args46905$ae41213$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41215, %struct.ScmObj* %args46905$ae41213$0)
store volatile %struct.ScmObj* %args46905$ae41213$1, %struct.ScmObj** %stackaddr$prim47206, align 8
%stackaddr$prim47207 = alloca %struct.ScmObj*, align 8
%args46905$ae41213$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41214, %struct.ScmObj* %args46905$ae41213$1)
store volatile %struct.ScmObj* %args46905$ae41213$2, %struct.ScmObj** %stackaddr$prim47207, align 8
%clofunc47208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41213)
musttail call tailcc void %clofunc47208(%struct.ScmObj* %ae41213, %struct.ScmObj* %args46905$ae41213$2)
ret void
}

define tailcc void @proc_clo$ae41213(%struct.ScmObj* %env$ae41213,%struct.ScmObj* %current_45args46488) {
%stackaddr$env-ref47209 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47209
%stackaddr$env-ref47210 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47210
%stackaddr$env-ref47211 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47211
%stackaddr$env-ref47212 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47212
%stackaddr$env-ref47213 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47213
%stackaddr$prim47214 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47214, align 8
%stackaddr$prim47215 = alloca %struct.ScmObj*, align 8
%current_45args46489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %current_45args46489, %struct.ScmObj** %stackaddr$prim47215, align 8
%stackaddr$prim47216 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46489)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47216, align 8
%stackaddr$makeclosure47217 = alloca %struct.ScmObj*, align 8
%fptrToInt47218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41243 to i64
%ae41243 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47218)
store volatile %struct.ScmObj* %ae41243, %struct.ScmObj** %stackaddr$makeclosure47217, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41244 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47219 = alloca %struct.ScmObj*, align 8
%fptrToInt47220 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41245 to i64
%ae41245 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47220)
store volatile %struct.ScmObj* %ae41245, %struct.ScmObj** %stackaddr$makeclosure47219, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %_37foldr140123, i64 1)
%args46895$ae41243$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47221 = alloca %struct.ScmObj*, align 8
%args46895$ae41243$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41245, %struct.ScmObj* %args46895$ae41243$0)
store volatile %struct.ScmObj* %args46895$ae41243$1, %struct.ScmObj** %stackaddr$prim47221, align 8
%stackaddr$prim47222 = alloca %struct.ScmObj*, align 8
%args46895$ae41243$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41244, %struct.ScmObj* %args46895$ae41243$1)
store volatile %struct.ScmObj* %args46895$ae41243$2, %struct.ScmObj** %stackaddr$prim47222, align 8
%clofunc47223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41243)
musttail call tailcc void %clofunc47223(%struct.ScmObj* %ae41243, %struct.ScmObj* %args46895$ae41243$2)
ret void
}

define tailcc void @proc_clo$ae41243(%struct.ScmObj* %env$ae41243,%struct.ScmObj* %current_45args46491) {
%stackaddr$env-ref47224 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47224
%stackaddr$env-ref47225 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47225
%stackaddr$env-ref47226 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47226
%stackaddr$env-ref47227 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47227
%stackaddr$env-ref47228 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47228
%stackaddr$prim47229 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47229, align 8
%stackaddr$prim47230 = alloca %struct.ScmObj*, align 8
%current_45args46492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %current_45args46492, %struct.ScmObj** %stackaddr$prim47230, align 8
%stackaddr$prim47231 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46492)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim47231, align 8
%stackaddr$makeclosure47232 = alloca %struct.ScmObj*, align 8
%fptrToInt47233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41627 to i64
%ae41627 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47233)
store volatile %struct.ScmObj* %ae41627, %struct.ScmObj** %stackaddr$makeclosure47232, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41627, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args46835$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47234 = alloca %struct.ScmObj*, align 8
%args46835$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args46835$Ycmb40102$0)
store volatile %struct.ScmObj* %args46835$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47234, align 8
%stackaddr$prim47235 = alloca %struct.ScmObj*, align 8
%args46835$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41627, %struct.ScmObj* %args46835$Ycmb40102$1)
store volatile %struct.ScmObj* %args46835$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47235, align 8
%clofunc47236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47236(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46835$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41627(%struct.ScmObj* %env$ae41627,%struct.ScmObj* %current_45args46494) {
%stackaddr$env-ref47237 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47237
%stackaddr$env-ref47238 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47238
%stackaddr$env-ref47239 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47239
%stackaddr$env-ref47240 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47240
%stackaddr$env-ref47241 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41627, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47241
%stackaddr$prim47242 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47242, align 8
%stackaddr$prim47243 = alloca %struct.ScmObj*, align 8
%current_45args46495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %current_45args46495, %struct.ScmObj** %stackaddr$prim47243, align 8
%stackaddr$prim47244 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46495)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47244, align 8
%stackaddr$makeclosure47245 = alloca %struct.ScmObj*, align 8
%fptrToInt47246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41629 to i64
%ae41629 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47246)
store volatile %struct.ScmObj* %ae41629, %struct.ScmObj** %stackaddr$makeclosure47245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41629, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41630 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47247 = alloca %struct.ScmObj*, align 8
%fptrToInt47248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41631 to i64
%ae41631 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47248)
store volatile %struct.ScmObj* %ae41631, %struct.ScmObj** %stackaddr$makeclosure47247, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41631, %struct.ScmObj* %_37foldr140123, i64 0)
%args46834$ae41629$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47249 = alloca %struct.ScmObj*, align 8
%args46834$ae41629$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41631, %struct.ScmObj* %args46834$ae41629$0)
store volatile %struct.ScmObj* %args46834$ae41629$1, %struct.ScmObj** %stackaddr$prim47249, align 8
%stackaddr$prim47250 = alloca %struct.ScmObj*, align 8
%args46834$ae41629$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41630, %struct.ScmObj* %args46834$ae41629$1)
store volatile %struct.ScmObj* %args46834$ae41629$2, %struct.ScmObj** %stackaddr$prim47250, align 8
%clofunc47251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41629)
musttail call tailcc void %clofunc47251(%struct.ScmObj* %ae41629, %struct.ScmObj* %args46834$ae41629$2)
ret void
}

define tailcc void @proc_clo$ae41629(%struct.ScmObj* %env$ae41629,%struct.ScmObj* %current_45args46497) {
%stackaddr$env-ref47252 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47252
%stackaddr$env-ref47253 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47253
%stackaddr$env-ref47254 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47254
%stackaddr$env-ref47255 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47255
%stackaddr$env-ref47256 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47256
%stackaddr$env-ref47257 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41629, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47257
%stackaddr$prim47258 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47258, align 8
%stackaddr$prim47259 = alloca %struct.ScmObj*, align 8
%current_45args46498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %current_45args46498, %struct.ScmObj** %stackaddr$prim47259, align 8
%stackaddr$prim47260 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46498)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47260, align 8
%stackaddr$makeclosure47261 = alloca %struct.ScmObj*, align 8
%fptrToInt47262 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41706 to i64
%ae41706 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47262)
store volatile %struct.ScmObj* %ae41706, %struct.ScmObj** %stackaddr$makeclosure47261, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41706, %struct.ScmObj* %_37map140154, i64 4)
%ae41707 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47263 = alloca %struct.ScmObj*, align 8
%fptrToInt47264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41708 to i64
%ae41708 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47264)
store volatile %struct.ScmObj* %ae41708, %struct.ScmObj** %stackaddr$makeclosure47263, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41708, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41708, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41708, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args46815$ae41706$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47265 = alloca %struct.ScmObj*, align 8
%args46815$ae41706$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41708, %struct.ScmObj* %args46815$ae41706$0)
store volatile %struct.ScmObj* %args46815$ae41706$1, %struct.ScmObj** %stackaddr$prim47265, align 8
%stackaddr$prim47266 = alloca %struct.ScmObj*, align 8
%args46815$ae41706$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41707, %struct.ScmObj* %args46815$ae41706$1)
store volatile %struct.ScmObj* %args46815$ae41706$2, %struct.ScmObj** %stackaddr$prim47266, align 8
%clofunc47267 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41706)
musttail call tailcc void %clofunc47267(%struct.ScmObj* %ae41706, %struct.ScmObj* %args46815$ae41706$2)
ret void
}

define tailcc void @proc_clo$ae41706(%struct.ScmObj* %env$ae41706,%struct.ScmObj* %current_45args46500) {
%stackaddr$env-ref47268 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47268
%stackaddr$env-ref47269 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47269
%stackaddr$env-ref47270 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47270
%stackaddr$env-ref47271 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47271
%stackaddr$env-ref47272 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41706, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47272
%stackaddr$prim47273 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47273, align 8
%stackaddr$prim47274 = alloca %struct.ScmObj*, align 8
%current_45args46501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %current_45args46501, %struct.ScmObj** %stackaddr$prim47274, align 8
%stackaddr$prim47275 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46501)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47275, align 8
%stackaddr$makeclosure47276 = alloca %struct.ScmObj*, align 8
%fptrToInt47277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41852 to i64
%ae41852 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47277)
store volatile %struct.ScmObj* %ae41852, %struct.ScmObj** %stackaddr$makeclosure47276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41852, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41852, %struct.ScmObj* %_37foldl140107, i64 1)
%ae41853 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47278 = alloca %struct.ScmObj*, align 8
%fptrToInt47279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41854 to i64
%ae41854 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47279)
store volatile %struct.ScmObj* %ae41854, %struct.ScmObj** %stackaddr$makeclosure47278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41854, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41854, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41854, %struct.ScmObj* %_37map140154, i64 2)
%args46798$ae41852$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47280 = alloca %struct.ScmObj*, align 8
%args46798$ae41852$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41854, %struct.ScmObj* %args46798$ae41852$0)
store volatile %struct.ScmObj* %args46798$ae41852$1, %struct.ScmObj** %stackaddr$prim47280, align 8
%stackaddr$prim47281 = alloca %struct.ScmObj*, align 8
%args46798$ae41852$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41853, %struct.ScmObj* %args46798$ae41852$1)
store volatile %struct.ScmObj* %args46798$ae41852$2, %struct.ScmObj** %stackaddr$prim47281, align 8
%clofunc47282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41852)
musttail call tailcc void %clofunc47282(%struct.ScmObj* %ae41852, %struct.ScmObj* %args46798$ae41852$2)
ret void
}

define tailcc void @proc_clo$ae41852(%struct.ScmObj* %env$ae41852,%struct.ScmObj* %current_45args46503) {
%stackaddr$env-ref47283 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41852, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47283
%stackaddr$env-ref47284 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41852, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47284
%stackaddr$prim47285 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47285, align 8
%stackaddr$prim47286 = alloca %struct.ScmObj*, align 8
%current_45args46504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %current_45args46504, %struct.ScmObj** %stackaddr$prim47286, align 8
%stackaddr$prim47287 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46504)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim47287, align 8
%stackaddr$makeclosure47288 = alloca %struct.ScmObj*, align 8
%fptrToInt47289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42244 to i64
%ae42244 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47289)
store volatile %struct.ScmObj* %ae42244, %struct.ScmObj** %stackaddr$makeclosure47288, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42244, %struct.ScmObj* %_37foldl140107, i64 0)
%args46738$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47290 = alloca %struct.ScmObj*, align 8
%args46738$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40296, %struct.ScmObj* %args46738$Ycmb40102$0)
store volatile %struct.ScmObj* %args46738$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47290, align 8
%stackaddr$prim47291 = alloca %struct.ScmObj*, align 8
%args46738$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42244, %struct.ScmObj* %args46738$Ycmb40102$1)
store volatile %struct.ScmObj* %args46738$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47291, align 8
%clofunc47292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47292(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46738$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42244(%struct.ScmObj* %env$ae42244,%struct.ScmObj* %current_45args46506) {
%stackaddr$env-ref47293 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42244, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47293
%stackaddr$prim47294 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47294, align 8
%stackaddr$prim47295 = alloca %struct.ScmObj*, align 8
%current_45args46507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %current_45args46507, %struct.ScmObj** %stackaddr$prim47295, align 8
%stackaddr$prim47296 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47296, align 8
%stackaddr$makeclosure47297 = alloca %struct.ScmObj*, align 8
%fptrToInt47298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42246 to i64
%ae42246 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47298)
store volatile %struct.ScmObj* %ae42246, %struct.ScmObj** %stackaddr$makeclosure47297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42246, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47299 = alloca %struct.ScmObj*, align 8
%fptrToInt47300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42248 to i64
%ae42248 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47300)
store volatile %struct.ScmObj* %ae42248, %struct.ScmObj** %stackaddr$makeclosure47299, align 8
%args46737$ae42246$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47301 = alloca %struct.ScmObj*, align 8
%args46737$ae42246$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42248, %struct.ScmObj* %args46737$ae42246$0)
store volatile %struct.ScmObj* %args46737$ae42246$1, %struct.ScmObj** %stackaddr$prim47301, align 8
%stackaddr$prim47302 = alloca %struct.ScmObj*, align 8
%args46737$ae42246$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42247, %struct.ScmObj* %args46737$ae42246$1)
store volatile %struct.ScmObj* %args46737$ae42246$2, %struct.ScmObj** %stackaddr$prim47302, align 8
%clofunc47303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42246)
musttail call tailcc void %clofunc47303(%struct.ScmObj* %ae42246, %struct.ScmObj* %args46737$ae42246$2)
ret void
}

define tailcc void @proc_clo$ae42246(%struct.ScmObj* %env$ae42246,%struct.ScmObj* %current_45args46509) {
%stackaddr$env-ref47304 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42246, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47304
%stackaddr$prim47305 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46509)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47305, align 8
%stackaddr$prim47306 = alloca %struct.ScmObj*, align 8
%current_45args46510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46509)
store volatile %struct.ScmObj* %current_45args46510, %struct.ScmObj** %stackaddr$prim47306, align 8
%stackaddr$prim47307 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46510)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47307, align 8
%stackaddr$makeclosure47308 = alloca %struct.ScmObj*, align 8
%fptrToInt47309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42270 to i64
%ae42270 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47309)
store volatile %struct.ScmObj* %ae42270, %struct.ScmObj** %stackaddr$makeclosure47308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42270, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47310 = alloca %struct.ScmObj*, align 8
%fptrToInt47311 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42272 to i64
%ae42272 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47311)
store volatile %struct.ScmObj* %ae42272, %struct.ScmObj** %stackaddr$makeclosure47310, align 8
%args46731$ae42270$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47312 = alloca %struct.ScmObj*, align 8
%args46731$ae42270$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42272, %struct.ScmObj* %args46731$ae42270$0)
store volatile %struct.ScmObj* %args46731$ae42270$1, %struct.ScmObj** %stackaddr$prim47312, align 8
%stackaddr$prim47313 = alloca %struct.ScmObj*, align 8
%args46731$ae42270$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42271, %struct.ScmObj* %args46731$ae42270$1)
store volatile %struct.ScmObj* %args46731$ae42270$2, %struct.ScmObj** %stackaddr$prim47313, align 8
%clofunc47314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42270)
musttail call tailcc void %clofunc47314(%struct.ScmObj* %ae42270, %struct.ScmObj* %args46731$ae42270$2)
ret void
}

define tailcc void @proc_clo$ae42270(%struct.ScmObj* %env$ae42270,%struct.ScmObj* %current_45args46512) {
%stackaddr$env-ref47315 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42270, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47315
%stackaddr$prim47316 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47316, align 8
%stackaddr$prim47317 = alloca %struct.ScmObj*, align 8
%current_45args46513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %current_45args46513, %struct.ScmObj** %stackaddr$prim47317, align 8
%stackaddr$prim47318 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46513)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47318, align 8
%ae42294 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42295 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47319 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42294, %struct.ScmObj* %ae42295)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47319, align 8
%stackaddr$makeclosure47320 = alloca %struct.ScmObj*, align 8
%fptrToInt47321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42296 to i64
%ae42296 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47321)
store volatile %struct.ScmObj* %ae42296, %struct.ScmObj** %stackaddr$makeclosure47320, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42296, %struct.ScmObj* %_37append40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42296, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42297 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47322 = alloca %struct.ScmObj*, align 8
%fptrToInt47323 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42298 to i64
%ae42298 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47323)
store volatile %struct.ScmObj* %ae42298, %struct.ScmObj** %stackaddr$makeclosure47322, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42298, %struct.ScmObj* %_37append40195, i64 0)
%args46725$ae42296$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47324 = alloca %struct.ScmObj*, align 8
%args46725$ae42296$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42298, %struct.ScmObj* %args46725$ae42296$0)
store volatile %struct.ScmObj* %args46725$ae42296$1, %struct.ScmObj** %stackaddr$prim47324, align 8
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%args46725$ae42296$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42297, %struct.ScmObj* %args46725$ae42296$1)
store volatile %struct.ScmObj* %args46725$ae42296$2, %struct.ScmObj** %stackaddr$prim47325, align 8
%clofunc47326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42296)
musttail call tailcc void %clofunc47326(%struct.ScmObj* %ae42296, %struct.ScmObj* %args46725$ae42296$2)
ret void
}

define tailcc void @proc_clo$ae42296(%struct.ScmObj* %env$ae42296,%struct.ScmObj* %current_45args46515) {
%stackaddr$env-ref47327 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42296, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47327
%stackaddr$env-ref47328 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42296, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47328
%stackaddr$prim47329 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46515)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim47329, align 8
%stackaddr$prim47330 = alloca %struct.ScmObj*, align 8
%current_45args46516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46515)
store volatile %struct.ScmObj* %current_45args46516, %struct.ScmObj** %stackaddr$prim47330, align 8
%stackaddr$prim47331 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46516)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47331, align 8
%ae42364 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47332 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42364, %struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47332, align 8
%ae42367 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47333 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42367)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47333, align 8
%stackaddr$makeclosure47334 = alloca %struct.ScmObj*, align 8
%fptrToInt47335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42368 to i64
%ae42368 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47335)
store volatile %struct.ScmObj* %ae42368, %struct.ScmObj** %stackaddr$makeclosure47334, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42368, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47336 = alloca %struct.ScmObj*, align 8
%fptrToInt47337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42370 to i64
%ae42370 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47337)
store volatile %struct.ScmObj* %ae42370, %struct.ScmObj** %stackaddr$makeclosure47336, align 8
%args46714$ae42368$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47338 = alloca %struct.ScmObj*, align 8
%args46714$ae42368$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42370, %struct.ScmObj* %args46714$ae42368$0)
store volatile %struct.ScmObj* %args46714$ae42368$1, %struct.ScmObj** %stackaddr$prim47338, align 8
%stackaddr$prim47339 = alloca %struct.ScmObj*, align 8
%args46714$ae42368$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42369, %struct.ScmObj* %args46714$ae42368$1)
store volatile %struct.ScmObj* %args46714$ae42368$2, %struct.ScmObj** %stackaddr$prim47339, align 8
%clofunc47340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42368)
musttail call tailcc void %clofunc47340(%struct.ScmObj* %ae42368, %struct.ScmObj* %args46714$ae42368$2)
ret void
}

define tailcc void @proc_clo$ae42368(%struct.ScmObj* %env$ae42368,%struct.ScmObj* %current_45args46518) {
%stackaddr$env-ref47341 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42368, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47341
%stackaddr$prim47342 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim47342, align 8
%stackaddr$prim47343 = alloca %struct.ScmObj*, align 8
%current_45args46519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46518)
store volatile %struct.ScmObj* %current_45args46519, %struct.ScmObj** %stackaddr$prim47343, align 8
%stackaddr$prim47344 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46519)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47344, align 8
%stackaddr$makeclosure47345 = alloca %struct.ScmObj*, align 8
%fptrToInt47346 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42784 to i64
%ae42784 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47346)
store volatile %struct.ScmObj* %ae42784, %struct.ScmObj** %stackaddr$makeclosure47345, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42784, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47347 = alloca %struct.ScmObj*, align 8
%fptrToInt47348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42786 to i64
%ae42786 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47348)
store volatile %struct.ScmObj* %ae42786, %struct.ScmObj** %stackaddr$makeclosure47347, align 8
%args46689$ae42784$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47349 = alloca %struct.ScmObj*, align 8
%args46689$ae42784$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42786, %struct.ScmObj* %args46689$ae42784$0)
store volatile %struct.ScmObj* %args46689$ae42784$1, %struct.ScmObj** %stackaddr$prim47349, align 8
%stackaddr$prim47350 = alloca %struct.ScmObj*, align 8
%args46689$ae42784$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42785, %struct.ScmObj* %args46689$ae42784$1)
store volatile %struct.ScmObj* %args46689$ae42784$2, %struct.ScmObj** %stackaddr$prim47350, align 8
%clofunc47351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42784)
musttail call tailcc void %clofunc47351(%struct.ScmObj* %ae42784, %struct.ScmObj* %args46689$ae42784$2)
ret void
}

define tailcc void @proc_clo$ae42784(%struct.ScmObj* %env$ae42784,%struct.ScmObj* %current_45args46521) {
%stackaddr$env-ref47352 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42784, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47352
%stackaddr$prim47353 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46521)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47353, align 8
%stackaddr$prim47354 = alloca %struct.ScmObj*, align 8
%current_45args46522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46521)
store volatile %struct.ScmObj* %current_45args46522, %struct.ScmObj** %stackaddr$prim47354, align 8
%stackaddr$prim47355 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46522)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47355, align 8
%stackaddr$makeclosure47356 = alloca %struct.ScmObj*, align 8
%fptrToInt47357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43320 to i64
%ae43320 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47357)
store volatile %struct.ScmObj* %ae43320, %struct.ScmObj** %stackaddr$makeclosure47356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43320, %struct.ScmObj* %_37foldl140107, i64 0)
%ae43321 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47358 = alloca %struct.ScmObj*, align 8
%fptrToInt47359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43322 to i64
%ae43322 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47359)
store volatile %struct.ScmObj* %ae43322, %struct.ScmObj** %stackaddr$makeclosure47358, align 8
%args46665$ae43320$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47360 = alloca %struct.ScmObj*, align 8
%args46665$ae43320$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43322, %struct.ScmObj* %args46665$ae43320$0)
store volatile %struct.ScmObj* %args46665$ae43320$1, %struct.ScmObj** %stackaddr$prim47360, align 8
%stackaddr$prim47361 = alloca %struct.ScmObj*, align 8
%args46665$ae43320$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43321, %struct.ScmObj* %args46665$ae43320$1)
store volatile %struct.ScmObj* %args46665$ae43320$2, %struct.ScmObj** %stackaddr$prim47361, align 8
%clofunc47362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43320)
musttail call tailcc void %clofunc47362(%struct.ScmObj* %ae43320, %struct.ScmObj* %args46665$ae43320$2)
ret void
}

define tailcc void @proc_clo$ae43320(%struct.ScmObj* %env$ae43320,%struct.ScmObj* %current_45args46524) {
%stackaddr$env-ref47363 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43320, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47363
%stackaddr$prim47364 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46524)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim47364, align 8
%stackaddr$prim47365 = alloca %struct.ScmObj*, align 8
%current_45args46525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46524)
store volatile %struct.ScmObj* %current_45args46525, %struct.ScmObj** %stackaddr$prim47365, align 8
%stackaddr$prim47366 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46525)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47366, align 8
%stackaddr$makeclosure47367 = alloca %struct.ScmObj*, align 8
%fptrToInt47368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43724 to i64
%ae43724 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47368)
store volatile %struct.ScmObj* %ae43724, %struct.ScmObj** %stackaddr$makeclosure47367, align 8
%ae43725 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47369 = alloca %struct.ScmObj*, align 8
%fptrToInt47370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43726 to i64
%ae43726 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47370)
store volatile %struct.ScmObj* %ae43726, %struct.ScmObj** %stackaddr$makeclosure47369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43726, %struct.ScmObj* %_37foldl140107, i64 0)
%args46639$ae43724$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47371 = alloca %struct.ScmObj*, align 8
%args46639$ae43724$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43726, %struct.ScmObj* %args46639$ae43724$0)
store volatile %struct.ScmObj* %args46639$ae43724$1, %struct.ScmObj** %stackaddr$prim47371, align 8
%stackaddr$prim47372 = alloca %struct.ScmObj*, align 8
%args46639$ae43724$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43725, %struct.ScmObj* %args46639$ae43724$1)
store volatile %struct.ScmObj* %args46639$ae43724$2, %struct.ScmObj** %stackaddr$prim47372, align 8
%clofunc47373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43724)
musttail call tailcc void %clofunc47373(%struct.ScmObj* %ae43724, %struct.ScmObj* %args46639$ae43724$2)
ret void
}

define tailcc void @proc_clo$ae43724(%struct.ScmObj* %env$ae43724,%struct.ScmObj* %current_45args46527) {
%stackaddr$prim47374 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46527)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim47374, align 8
%stackaddr$prim47375 = alloca %struct.ScmObj*, align 8
%current_45args46528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46527)
store volatile %struct.ScmObj* %current_45args46528, %struct.ScmObj** %stackaddr$prim47375, align 8
%stackaddr$prim47376 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46528)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim47376, align 8
%stackaddr$makeclosure47377 = alloca %struct.ScmObj*, align 8
%fptrToInt47378 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43822 to i64
%ae43822 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47378)
store volatile %struct.ScmObj* %ae43822, %struct.ScmObj** %stackaddr$makeclosure47377, align 8
%ae43823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47379 = alloca %struct.ScmObj*, align 8
%fptrToInt47380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43824 to i64
%ae43824 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47380)
store volatile %struct.ScmObj* %ae43824, %struct.ScmObj** %stackaddr$makeclosure47379, align 8
%args46626$ae43822$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47381 = alloca %struct.ScmObj*, align 8
%args46626$ae43822$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43824, %struct.ScmObj* %args46626$ae43822$0)
store volatile %struct.ScmObj* %args46626$ae43822$1, %struct.ScmObj** %stackaddr$prim47381, align 8
%stackaddr$prim47382 = alloca %struct.ScmObj*, align 8
%args46626$ae43822$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43823, %struct.ScmObj* %args46626$ae43822$1)
store volatile %struct.ScmObj* %args46626$ae43822$2, %struct.ScmObj** %stackaddr$prim47382, align 8
%clofunc47383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43822)
musttail call tailcc void %clofunc47383(%struct.ScmObj* %ae43822, %struct.ScmObj* %args46626$ae43822$2)
ret void
}

define tailcc void @proc_clo$ae43822(%struct.ScmObj* %env$ae43822,%struct.ScmObj* %current_45args46530) {
%stackaddr$prim47384 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47384, align 8
%stackaddr$prim47385 = alloca %struct.ScmObj*, align 8
%current_45args46531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %current_45args46531, %struct.ScmObj** %stackaddr$prim47385, align 8
%stackaddr$prim47386 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46531)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim47386, align 8
%stackaddr$makeclosure47387 = alloca %struct.ScmObj*, align 8
%fptrToInt47388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43842 to i64
%ae43842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47388)
store volatile %struct.ScmObj* %ae43842, %struct.ScmObj** %stackaddr$makeclosure47387, align 8
%ae43843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47389 = alloca %struct.ScmObj*, align 8
%fptrToInt47390 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43844 to i64
%ae43844 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47390)
store volatile %struct.ScmObj* %ae43844, %struct.ScmObj** %stackaddr$makeclosure47389, align 8
%args46621$ae43842$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47391 = alloca %struct.ScmObj*, align 8
%args46621$ae43842$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43844, %struct.ScmObj* %args46621$ae43842$0)
store volatile %struct.ScmObj* %args46621$ae43842$1, %struct.ScmObj** %stackaddr$prim47391, align 8
%stackaddr$prim47392 = alloca %struct.ScmObj*, align 8
%args46621$ae43842$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43843, %struct.ScmObj* %args46621$ae43842$1)
store volatile %struct.ScmObj* %args46621$ae43842$2, %struct.ScmObj** %stackaddr$prim47392, align 8
%clofunc47393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43842)
musttail call tailcc void %clofunc47393(%struct.ScmObj* %ae43842, %struct.ScmObj* %args46621$ae43842$2)
ret void
}

define tailcc void @proc_clo$ae43842(%struct.ScmObj* %env$ae43842,%struct.ScmObj* %current_45args46533) {
%stackaddr$prim47394 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47394, align 8
%stackaddr$prim47395 = alloca %struct.ScmObj*, align 8
%current_45args46534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46533)
store volatile %struct.ScmObj* %current_45args46534, %struct.ScmObj** %stackaddr$prim47395, align 8
%stackaddr$prim47396 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim47396, align 8
%stackaddr$makeclosure47397 = alloca %struct.ScmObj*, align 8
%fptrToInt47398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43864 to i64
%ae43864 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47398)
store volatile %struct.ScmObj* %ae43864, %struct.ScmObj** %stackaddr$makeclosure47397, align 8
%ae43865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47399 = alloca %struct.ScmObj*, align 8
%fptrToInt47400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43866 to i64
%ae43866 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47400)
store volatile %struct.ScmObj* %ae43866, %struct.ScmObj** %stackaddr$makeclosure47399, align 8
%args46616$ae43864$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47401 = alloca %struct.ScmObj*, align 8
%args46616$ae43864$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43866, %struct.ScmObj* %args46616$ae43864$0)
store volatile %struct.ScmObj* %args46616$ae43864$1, %struct.ScmObj** %stackaddr$prim47401, align 8
%stackaddr$prim47402 = alloca %struct.ScmObj*, align 8
%args46616$ae43864$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43865, %struct.ScmObj* %args46616$ae43864$1)
store volatile %struct.ScmObj* %args46616$ae43864$2, %struct.ScmObj** %stackaddr$prim47402, align 8
%clofunc47403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43864)
musttail call tailcc void %clofunc47403(%struct.ScmObj* %ae43864, %struct.ScmObj* %args46616$ae43864$2)
ret void
}

define tailcc void @proc_clo$ae43864(%struct.ScmObj* %env$ae43864,%struct.ScmObj* %current_45args46536) {
%stackaddr$prim47404 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46536)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47404, align 8
%stackaddr$prim47405 = alloca %struct.ScmObj*, align 8
%current_45args46537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46536)
store volatile %struct.ScmObj* %current_45args46537, %struct.ScmObj** %stackaddr$prim47405, align 8
%stackaddr$prim47406 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46537)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim47406, align 8
%stackaddr$makeclosure47407 = alloca %struct.ScmObj*, align 8
%fptrToInt47408 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43888 to i64
%ae43888 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47408)
store volatile %struct.ScmObj* %ae43888, %struct.ScmObj** %stackaddr$makeclosure47407, align 8
%ae43889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47409 = alloca %struct.ScmObj*, align 8
%fptrToInt47410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43890 to i64
%ae43890 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47410)
store volatile %struct.ScmObj* %ae43890, %struct.ScmObj** %stackaddr$makeclosure47409, align 8
%args46611$ae43888$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47411 = alloca %struct.ScmObj*, align 8
%args46611$ae43888$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43890, %struct.ScmObj* %args46611$ae43888$0)
store volatile %struct.ScmObj* %args46611$ae43888$1, %struct.ScmObj** %stackaddr$prim47411, align 8
%stackaddr$prim47412 = alloca %struct.ScmObj*, align 8
%args46611$ae43888$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43889, %struct.ScmObj* %args46611$ae43888$1)
store volatile %struct.ScmObj* %args46611$ae43888$2, %struct.ScmObj** %stackaddr$prim47412, align 8
%clofunc47413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43888)
musttail call tailcc void %clofunc47413(%struct.ScmObj* %ae43888, %struct.ScmObj* %args46611$ae43888$2)
ret void
}

define tailcc void @proc_clo$ae43888(%struct.ScmObj* %env$ae43888,%struct.ScmObj* %current_45args46539) {
%stackaddr$prim47414 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim47414, align 8
%stackaddr$prim47415 = alloca %struct.ScmObj*, align 8
%current_45args46540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %current_45args46540, %struct.ScmObj** %stackaddr$prim47415, align 8
%stackaddr$prim47416 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46540)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim47416, align 8
%stackaddr$makeclosure47417 = alloca %struct.ScmObj*, align 8
%fptrToInt47418 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43914 to i64
%ae43914 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47418)
store volatile %struct.ScmObj* %ae43914, %struct.ScmObj** %stackaddr$makeclosure47417, align 8
%ae43915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47419 = alloca %struct.ScmObj*, align 8
%fptrToInt47420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43916 to i64
%ae43916 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47420)
store volatile %struct.ScmObj* %ae43916, %struct.ScmObj** %stackaddr$makeclosure47419, align 8
%args46606$ae43914$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47421 = alloca %struct.ScmObj*, align 8
%args46606$ae43914$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43916, %struct.ScmObj* %args46606$ae43914$0)
store volatile %struct.ScmObj* %args46606$ae43914$1, %struct.ScmObj** %stackaddr$prim47421, align 8
%stackaddr$prim47422 = alloca %struct.ScmObj*, align 8
%args46606$ae43914$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43915, %struct.ScmObj* %args46606$ae43914$1)
store volatile %struct.ScmObj* %args46606$ae43914$2, %struct.ScmObj** %stackaddr$prim47422, align 8
%clofunc47423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43914)
musttail call tailcc void %clofunc47423(%struct.ScmObj* %ae43914, %struct.ScmObj* %args46606$ae43914$2)
ret void
}

define tailcc void @proc_clo$ae43914(%struct.ScmObj* %env$ae43914,%struct.ScmObj* %current_45args46542) {
%stackaddr$prim47424 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47424, align 8
%stackaddr$prim47425 = alloca %struct.ScmObj*, align 8
%current_45args46543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46542)
store volatile %struct.ScmObj* %current_45args46543, %struct.ScmObj** %stackaddr$prim47425, align 8
%stackaddr$prim47426 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46543)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim47426, align 8
%stackaddr$makeclosure47427 = alloca %struct.ScmObj*, align 8
%fptrToInt47428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44001 to i64
%ae44001 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47428)
store volatile %struct.ScmObj* %ae44001, %struct.ScmObj** %stackaddr$makeclosure47427, align 8
%ae44002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47429 = alloca %struct.ScmObj*, align 8
%fptrToInt47430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44003 to i64
%ae44003 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47430)
store volatile %struct.ScmObj* %ae44003, %struct.ScmObj** %stackaddr$makeclosure47429, align 8
%args46599$ae44001$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47431 = alloca %struct.ScmObj*, align 8
%args46599$ae44001$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44003, %struct.ScmObj* %args46599$ae44001$0)
store volatile %struct.ScmObj* %args46599$ae44001$1, %struct.ScmObj** %stackaddr$prim47431, align 8
%stackaddr$prim47432 = alloca %struct.ScmObj*, align 8
%args46599$ae44001$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44002, %struct.ScmObj* %args46599$ae44001$1)
store volatile %struct.ScmObj* %args46599$ae44001$2, %struct.ScmObj** %stackaddr$prim47432, align 8
%clofunc47433 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44001)
musttail call tailcc void %clofunc47433(%struct.ScmObj* %ae44001, %struct.ScmObj* %args46599$ae44001$2)
ret void
}

define tailcc void @proc_clo$ae44001(%struct.ScmObj* %env$ae44001,%struct.ScmObj* %current_45args46545) {
%stackaddr$prim47434 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46545)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47434, align 8
%stackaddr$prim47435 = alloca %struct.ScmObj*, align 8
%current_45args46546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46545)
store volatile %struct.ScmObj* %current_45args46546, %struct.ScmObj** %stackaddr$prim47435, align 8
%stackaddr$prim47436 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46546)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim47436, align 8
%stackaddr$makeclosure47437 = alloca %struct.ScmObj*, align 8
%fptrToInt47438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44023 to i64
%ae44023 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47438)
store volatile %struct.ScmObj* %ae44023, %struct.ScmObj** %stackaddr$makeclosure47437, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44023, %struct.ScmObj* %anf_45bind40344, i64 0)
%ae44024 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47439 = alloca %struct.ScmObj*, align 8
%fptrToInt47440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44025 to i64
%ae44025 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47440)
store volatile %struct.ScmObj* %ae44025, %struct.ScmObj** %stackaddr$makeclosure47439, align 8
%args46593$ae44023$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47441 = alloca %struct.ScmObj*, align 8
%args46593$ae44023$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44025, %struct.ScmObj* %args46593$ae44023$0)
store volatile %struct.ScmObj* %args46593$ae44023$1, %struct.ScmObj** %stackaddr$prim47441, align 8
%stackaddr$prim47442 = alloca %struct.ScmObj*, align 8
%args46593$ae44023$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44024, %struct.ScmObj* %args46593$ae44023$1)
store volatile %struct.ScmObj* %args46593$ae44023$2, %struct.ScmObj** %stackaddr$prim47442, align 8
%clofunc47443 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44023)
musttail call tailcc void %clofunc47443(%struct.ScmObj* %ae44023, %struct.ScmObj* %args46593$ae44023$2)
ret void
}

define tailcc void @proc_clo$ae44023(%struct.ScmObj* %env$ae44023,%struct.ScmObj* %current_45args46548) {
%stackaddr$env-ref47444 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44023, i64 0)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47444
%stackaddr$prim47445 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim47445, align 8
%stackaddr$prim47446 = alloca %struct.ScmObj*, align 8
%current_45args46549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46548)
store volatile %struct.ScmObj* %current_45args46549, %struct.ScmObj** %stackaddr$prim47446, align 8
%stackaddr$prim47447 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46549)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim47447, align 8
%stackaddr$makeclosure47448 = alloca %struct.ScmObj*, align 8
%fptrToInt47449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44046 to i64
%ae44046 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47449)
store volatile %struct.ScmObj* %ae44046, %struct.ScmObj** %stackaddr$makeclosure47448, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44046, %struct.ScmObj* %anf_45bind40344, i64 0)
%ae44047 = call %struct.ScmObj* @const_init_int(i64 4)
%ae44048 = call %struct.ScmObj* @const_init_int(i64 10)
%args46591$anf_45bind40345$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47450 = alloca %struct.ScmObj*, align 8
%args46591$anf_45bind40345$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44048, %struct.ScmObj* %args46591$anf_45bind40345$0)
store volatile %struct.ScmObj* %args46591$anf_45bind40345$1, %struct.ScmObj** %stackaddr$prim47450, align 8
%stackaddr$prim47451 = alloca %struct.ScmObj*, align 8
%args46591$anf_45bind40345$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44047, %struct.ScmObj* %args46591$anf_45bind40345$1)
store volatile %struct.ScmObj* %args46591$anf_45bind40345$2, %struct.ScmObj** %stackaddr$prim47451, align 8
%stackaddr$prim47452 = alloca %struct.ScmObj*, align 8
%args46591$anf_45bind40345$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44046, %struct.ScmObj* %args46591$anf_45bind40345$2)
store volatile %struct.ScmObj* %args46591$anf_45bind40345$3, %struct.ScmObj** %stackaddr$prim47452, align 8
%clofunc47453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40345)
musttail call tailcc void %clofunc47453(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args46591$anf_45bind40345$3)
ret void
}

define tailcc void @proc_clo$ae44046(%struct.ScmObj* %env$ae44046,%struct.ScmObj* %current_45args46551) {
%stackaddr$env-ref47454 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44046, i64 0)
store %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$env-ref47454
%stackaddr$prim47455 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim47455, align 8
%stackaddr$prim47456 = alloca %struct.ScmObj*, align 8
%current_45args46552 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %current_45args46552, %struct.ScmObj** %stackaddr$prim47456, align 8
%stackaddr$prim47457 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46552)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim47457, align 8
%stackaddr$makeclosure47458 = alloca %struct.ScmObj*, align 8
%fptrToInt47459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44057 to i64
%ae44057 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47459)
store volatile %struct.ScmObj* %ae44057, %struct.ScmObj** %stackaddr$makeclosure47458, align 8
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%cpsargs40403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44057, %struct.ScmObj* %anf_45bind40346)
store volatile %struct.ScmObj* %cpsargs40403, %struct.ScmObj** %stackaddr$prim47460, align 8
%clofunc47461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40344)
musttail call tailcc void %clofunc47461(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %cpsargs40403)
ret void
}

define tailcc void @proc_clo$ae44057(%struct.ScmObj* %env$ae44057,%struct.ScmObj* %current_45args46554) {
%stackaddr$prim47462 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46554)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim47462, align 8
%stackaddr$prim47463 = alloca %struct.ScmObj*, align 8
%current_45args46555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46554)
store volatile %struct.ScmObj* %current_45args46555, %struct.ScmObj** %stackaddr$prim47463, align 8
%stackaddr$prim47464 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46555)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim47464, align 8
%stackaddr$makeclosure47465 = alloca %struct.ScmObj*, align 8
%fptrToInt47466 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44061 to i64
%ae44061 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47466)
store volatile %struct.ScmObj* %ae44061, %struct.ScmObj** %stackaddr$makeclosure47465, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44061, %struct.ScmObj* %anf_45bind40347, i64 0)
%ae44062 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47467 = alloca %struct.ScmObj*, align 8
%fptrToInt47468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44063 to i64
%ae44063 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47468)
store volatile %struct.ScmObj* %ae44063, %struct.ScmObj** %stackaddr$makeclosure47467, align 8
%args46590$ae44061$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47469 = alloca %struct.ScmObj*, align 8
%args46590$ae44061$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44063, %struct.ScmObj* %args46590$ae44061$0)
store volatile %struct.ScmObj* %args46590$ae44061$1, %struct.ScmObj** %stackaddr$prim47469, align 8
%stackaddr$prim47470 = alloca %struct.ScmObj*, align 8
%args46590$ae44061$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44062, %struct.ScmObj* %args46590$ae44061$1)
store volatile %struct.ScmObj* %args46590$ae44061$2, %struct.ScmObj** %stackaddr$prim47470, align 8
%clofunc47471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44061)
musttail call tailcc void %clofunc47471(%struct.ScmObj* %ae44061, %struct.ScmObj* %args46590$ae44061$2)
ret void
}

define tailcc void @proc_clo$ae44061(%struct.ScmObj* %env$ae44061,%struct.ScmObj* %current_45args46557) {
%stackaddr$env-ref47472 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44061, i64 0)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref47472
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46557)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim47473, align 8
%stackaddr$prim47474 = alloca %struct.ScmObj*, align 8
%current_45args46558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46557)
store volatile %struct.ScmObj* %current_45args46558, %struct.ScmObj** %stackaddr$prim47474, align 8
%stackaddr$prim47475 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46558)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim47475, align 8
%stackaddr$makeclosure47476 = alloca %struct.ScmObj*, align 8
%fptrToInt47477 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44106 to i64
%ae44106 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47477)
store volatile %struct.ScmObj* %ae44106, %struct.ScmObj** %stackaddr$makeclosure47476, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44106, %struct.ScmObj* %anf_45bind40349, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44106, %struct.ScmObj* %anf_45bind40347, i64 1)
%ae44107 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47478 = alloca %struct.ScmObj*, align 8
%fptrToInt47479 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44108 to i64
%ae44108 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47479)
store volatile %struct.ScmObj* %ae44108, %struct.ScmObj** %stackaddr$makeclosure47478, align 8
%args46580$ae44106$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47480 = alloca %struct.ScmObj*, align 8
%args46580$ae44106$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44108, %struct.ScmObj* %args46580$ae44106$0)
store volatile %struct.ScmObj* %args46580$ae44106$1, %struct.ScmObj** %stackaddr$prim47480, align 8
%stackaddr$prim47481 = alloca %struct.ScmObj*, align 8
%args46580$ae44106$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44107, %struct.ScmObj* %args46580$ae44106$1)
store volatile %struct.ScmObj* %args46580$ae44106$2, %struct.ScmObj** %stackaddr$prim47481, align 8
%clofunc47482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44106)
musttail call tailcc void %clofunc47482(%struct.ScmObj* %ae44106, %struct.ScmObj* %args46580$ae44106$2)
ret void
}

define tailcc void @proc_clo$ae44106(%struct.ScmObj* %env$ae44106,%struct.ScmObj* %current_45args46560) {
%stackaddr$env-ref47483 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44106, i64 0)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref47483
%stackaddr$env-ref47484 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44106, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref47484
%stackaddr$prim47485 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46560)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim47485, align 8
%stackaddr$prim47486 = alloca %struct.ScmObj*, align 8
%current_45args46561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46560)
store volatile %struct.ScmObj* %current_45args46561, %struct.ScmObj** %stackaddr$prim47486, align 8
%stackaddr$prim47487 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46561)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim47487, align 8
%stackaddr$makeclosure47488 = alloca %struct.ScmObj*, align 8
%fptrToInt47489 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44129 to i64
%ae44129 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47489)
store volatile %struct.ScmObj* %ae44129, %struct.ScmObj** %stackaddr$makeclosure47488, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44129, %struct.ScmObj* %anf_45bind40349, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44129, %struct.ScmObj* %anf_45bind40347, i64 1)
%ae44130 = call %struct.ScmObj* @const_init_int(i64 4)
%args46578$anf_45bind40350$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47490 = alloca %struct.ScmObj*, align 8
%args46578$anf_45bind40350$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44130, %struct.ScmObj* %args46578$anf_45bind40350$0)
store volatile %struct.ScmObj* %args46578$anf_45bind40350$1, %struct.ScmObj** %stackaddr$prim47490, align 8
%stackaddr$prim47491 = alloca %struct.ScmObj*, align 8
%args46578$anf_45bind40350$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44129, %struct.ScmObj* %args46578$anf_45bind40350$1)
store volatile %struct.ScmObj* %args46578$anf_45bind40350$2, %struct.ScmObj** %stackaddr$prim47491, align 8
%clofunc47492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40350)
musttail call tailcc void %clofunc47492(%struct.ScmObj* %anf_45bind40350, %struct.ScmObj* %args46578$anf_45bind40350$2)
ret void
}

define tailcc void @proc_clo$ae44129(%struct.ScmObj* %env$ae44129,%struct.ScmObj* %current_45args46563) {
%stackaddr$env-ref47493 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44129, i64 0)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref47493
%stackaddr$env-ref47494 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44129, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref47494
%stackaddr$prim47495 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim47495, align 8
%stackaddr$prim47496 = alloca %struct.ScmObj*, align 8
%current_45args46564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46563)
store volatile %struct.ScmObj* %current_45args46564, %struct.ScmObj** %stackaddr$prim47496, align 8
%stackaddr$prim47497 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46564)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim47497, align 8
%stackaddr$makeclosure47498 = alloca %struct.ScmObj*, align 8
%fptrToInt47499 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44134 to i64
%ae44134 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47499)
store volatile %struct.ScmObj* %ae44134, %struct.ScmObj** %stackaddr$makeclosure47498, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44134, %struct.ScmObj* %anf_45bind40347, i64 0)
%stackaddr$prim47500 = alloca %struct.ScmObj*, align 8
%cpsargs40397 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44134, %struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %cpsargs40397, %struct.ScmObj** %stackaddr$prim47500, align 8
%clofunc47501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40349)
musttail call tailcc void %clofunc47501(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %cpsargs40397)
ret void
}

define tailcc void @proc_clo$ae44134(%struct.ScmObj* %env$ae44134,%struct.ScmObj* %current_45args46566) {
%stackaddr$env-ref47502 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44134, i64 0)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref47502
%stackaddr$prim47503 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim47503, align 8
%stackaddr$prim47504 = alloca %struct.ScmObj*, align 8
%current_45args46567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46566)
store volatile %struct.ScmObj* %current_45args46567, %struct.ScmObj** %stackaddr$prim47504, align 8
%stackaddr$prim47505 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46567)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim47505, align 8
%stackaddr$makeclosure47506 = alloca %struct.ScmObj*, align 8
%fptrToInt47507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44139 to i64
%ae44139 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47507)
store volatile %struct.ScmObj* %ae44139, %struct.ScmObj** %stackaddr$makeclosure47506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %anf_45bind40347, i64 0)
%ae44140 = call %struct.ScmObj* @const_init_int(i64 5)
%ae44141 = call %struct.ScmObj* @const_init_int(i64 6)
%args46577$anf_45bind40352$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47508 = alloca %struct.ScmObj*, align 8
%args46577$anf_45bind40352$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44141, %struct.ScmObj* %args46577$anf_45bind40352$0)
store volatile %struct.ScmObj* %args46577$anf_45bind40352$1, %struct.ScmObj** %stackaddr$prim47508, align 8
%stackaddr$prim47509 = alloca %struct.ScmObj*, align 8
%args46577$anf_45bind40352$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44140, %struct.ScmObj* %args46577$anf_45bind40352$1)
store volatile %struct.ScmObj* %args46577$anf_45bind40352$2, %struct.ScmObj** %stackaddr$prim47509, align 8
%stackaddr$prim47510 = alloca %struct.ScmObj*, align 8
%args46577$anf_45bind40352$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44139, %struct.ScmObj* %args46577$anf_45bind40352$2)
store volatile %struct.ScmObj* %args46577$anf_45bind40352$3, %struct.ScmObj** %stackaddr$prim47510, align 8
%clofunc47511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40352)
musttail call tailcc void %clofunc47511(%struct.ScmObj* %anf_45bind40352, %struct.ScmObj* %args46577$anf_45bind40352$3)
ret void
}

define tailcc void @proc_clo$ae44139(%struct.ScmObj* %env$ae44139,%struct.ScmObj* %current_45args46569) {
%stackaddr$env-ref47512 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 0)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref47512
%stackaddr$prim47513 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46569)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim47513, align 8
%stackaddr$prim47514 = alloca %struct.ScmObj*, align 8
%current_45args46570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46569)
store volatile %struct.ScmObj* %current_45args46570, %struct.ScmObj** %stackaddr$prim47514, align 8
%stackaddr$prim47515 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46570)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim47515, align 8
%stackaddr$prim47516 = alloca %struct.ScmObj*, align 8
%cpsprim40396 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %anf_45bind40353)
store volatile %struct.ScmObj* %cpsprim40396, %struct.ScmObj** %stackaddr$prim47516, align 8
%stackaddr$makeclosure47517 = alloca %struct.ScmObj*, align 8
%fptrToInt47518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44152 to i64
%ae44152 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47518)
store volatile %struct.ScmObj* %ae44152, %struct.ScmObj** %stackaddr$makeclosure47517, align 8
%ae44153 = call %struct.ScmObj* @const_init_int(i64 0)
%args46576$ae44152$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47519 = alloca %struct.ScmObj*, align 8
%args46576$ae44152$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40396, %struct.ScmObj* %args46576$ae44152$0)
store volatile %struct.ScmObj* %args46576$ae44152$1, %struct.ScmObj** %stackaddr$prim47519, align 8
%stackaddr$prim47520 = alloca %struct.ScmObj*, align 8
%args46576$ae44152$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44153, %struct.ScmObj* %args46576$ae44152$1)
store volatile %struct.ScmObj* %args46576$ae44152$2, %struct.ScmObj** %stackaddr$prim47520, align 8
%clofunc47521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44152)
musttail call tailcc void %clofunc47521(%struct.ScmObj* %ae44152, %struct.ScmObj* %args46576$ae44152$2)
ret void
}

define tailcc void @proc_clo$ae44152(%struct.ScmObj* %env$ae44152,%struct.ScmObj* %current_45args46572) {
%stackaddr$prim47522 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47522, align 8
%stackaddr$prim47523 = alloca %struct.ScmObj*, align 8
%current_45args46573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %current_45args46573, %struct.ScmObj** %stackaddr$prim47523, align 8
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47524, align 8
%stackaddr$prim47525 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47525, align 8
%args46575$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47526 = alloca %struct.ScmObj*, align 8
%args46575$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46575$k$0)
store volatile %struct.ScmObj* %args46575$k$1, %struct.ScmObj** %stackaddr$prim47526, align 8
%clofunc47527 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47527(%struct.ScmObj* %k, %struct.ScmObj* %args46575$k$1)
ret void
}

define tailcc void @proc_clo$ae44108(%struct.ScmObj* %env$ae44108,%struct.ScmObj* %lst4022840398) {
%stackaddr$prim47528 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022840398)
store volatile %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$prim47528, align 8
%stackaddr$prim47529 = alloca %struct.ScmObj*, align 8
%lst40228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022840398)
store volatile %struct.ScmObj* %lst40228, %struct.ScmObj** %stackaddr$prim47529, align 8
%ae44112 = call %struct.ScmObj* @const_init_int(i64 0)
%args46579$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47530 = alloca %struct.ScmObj*, align 8
%args46579$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40228, %struct.ScmObj* %args46579$k40399$0)
store volatile %struct.ScmObj* %args46579$k40399$1, %struct.ScmObj** %stackaddr$prim47530, align 8
%stackaddr$prim47531 = alloca %struct.ScmObj*, align 8
%args46579$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44112, %struct.ScmObj* %args46579$k40399$1)
store volatile %struct.ScmObj* %args46579$k40399$2, %struct.ScmObj** %stackaddr$prim47531, align 8
%clofunc47532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47532(%struct.ScmObj* %k40399, %struct.ScmObj* %args46579$k40399$2)
ret void
}

define tailcc void @proc_clo$ae44063(%struct.ScmObj* %env$ae44063,%struct.ScmObj* %current_45args46581) {
%stackaddr$prim47533 = alloca %struct.ScmObj*, align 8
%k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %k40400, %struct.ScmObj** %stackaddr$prim47533, align 8
%stackaddr$prim47534 = alloca %struct.ScmObj*, align 8
%current_45args46582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %current_45args46582, %struct.ScmObj** %stackaddr$prim47534, align 8
%stackaddr$prim47535 = alloca %struct.ScmObj*, align 8
%u40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46582)
store volatile %struct.ScmObj* %u40225, %struct.ScmObj** %stackaddr$prim47535, align 8
%ae44065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47536 = alloca %struct.ScmObj*, align 8
%fptrToInt47537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44066 to i64
%ae44066 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47537)
store volatile %struct.ScmObj* %ae44066, %struct.ScmObj** %stackaddr$makeclosure47536, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44066, %struct.ScmObj* %u40225, i64 0)
%args46589$k40400$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47538 = alloca %struct.ScmObj*, align 8
%args46589$k40400$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44066, %struct.ScmObj* %args46589$k40400$0)
store volatile %struct.ScmObj* %args46589$k40400$1, %struct.ScmObj** %stackaddr$prim47538, align 8
%stackaddr$prim47539 = alloca %struct.ScmObj*, align 8
%args46589$k40400$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44065, %struct.ScmObj* %args46589$k40400$1)
store volatile %struct.ScmObj* %args46589$k40400$2, %struct.ScmObj** %stackaddr$prim47539, align 8
%clofunc47540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40400)
musttail call tailcc void %clofunc47540(%struct.ScmObj* %k40400, %struct.ScmObj* %args46589$k40400$2)
ret void
}

define tailcc void @proc_clo$ae44066(%struct.ScmObj* %env$ae44066,%struct.ScmObj* %current_45args46584) {
%stackaddr$env-ref47541 = alloca %struct.ScmObj*, align 8
%u40225 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44066, i64 0)
store %struct.ScmObj* %u40225, %struct.ScmObj** %stackaddr$env-ref47541
%stackaddr$prim47542 = alloca %struct.ScmObj*, align 8
%k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %k40401, %struct.ScmObj** %stackaddr$prim47542, align 8
%stackaddr$prim47543 = alloca %struct.ScmObj*, align 8
%current_45args46585 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %current_45args46585, %struct.ScmObj** %stackaddr$prim47543, align 8
%stackaddr$prim47544 = alloca %struct.ScmObj*, align 8
%v40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46585)
store volatile %struct.ScmObj* %v40227, %struct.ScmObj** %stackaddr$prim47544, align 8
%stackaddr$prim47545 = alloca %struct.ScmObj*, align 8
%current_45args46586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46585)
store volatile %struct.ScmObj* %current_45args46586, %struct.ScmObj** %stackaddr$prim47545, align 8
%stackaddr$prim47546 = alloca %struct.ScmObj*, align 8
%w40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46586)
store volatile %struct.ScmObj* %w40226, %struct.ScmObj** %stackaddr$prim47546, align 8
%stackaddr$prim47547 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %v40227, %struct.ScmObj* %w40226)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim47547, align 8
%stackaddr$prim47548 = alloca %struct.ScmObj*, align 8
%cpsprim40402 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %u40225, %struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %cpsprim40402, %struct.ScmObj** %stackaddr$prim47548, align 8
%ae44072 = call %struct.ScmObj* @const_init_int(i64 0)
%args46588$k40401$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47549 = alloca %struct.ScmObj*, align 8
%args46588$k40401$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40402, %struct.ScmObj* %args46588$k40401$0)
store volatile %struct.ScmObj* %args46588$k40401$1, %struct.ScmObj** %stackaddr$prim47549, align 8
%stackaddr$prim47550 = alloca %struct.ScmObj*, align 8
%args46588$k40401$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44072, %struct.ScmObj* %args46588$k40401$1)
store volatile %struct.ScmObj* %args46588$k40401$2, %struct.ScmObj** %stackaddr$prim47550, align 8
%clofunc47551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40401)
musttail call tailcc void %clofunc47551(%struct.ScmObj* %k40401, %struct.ScmObj* %args46588$k40401$2)
ret void
}

define tailcc void @proc_clo$ae44025(%struct.ScmObj* %env$ae44025,%struct.ScmObj* %lst4022440404) {
%stackaddr$prim47552 = alloca %struct.ScmObj*, align 8
%k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022440404)
store volatile %struct.ScmObj* %k40405, %struct.ScmObj** %stackaddr$prim47552, align 8
%stackaddr$prim47553 = alloca %struct.ScmObj*, align 8
%lst40224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022440404)
store volatile %struct.ScmObj* %lst40224, %struct.ScmObj** %stackaddr$prim47553, align 8
%ae44029 = call %struct.ScmObj* @const_init_int(i64 0)
%args46592$k40405$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47554 = alloca %struct.ScmObj*, align 8
%args46592$k40405$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40224, %struct.ScmObj* %args46592$k40405$0)
store volatile %struct.ScmObj* %args46592$k40405$1, %struct.ScmObj** %stackaddr$prim47554, align 8
%stackaddr$prim47555 = alloca %struct.ScmObj*, align 8
%args46592$k40405$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44029, %struct.ScmObj* %args46592$k40405$1)
store volatile %struct.ScmObj* %args46592$k40405$2, %struct.ScmObj** %stackaddr$prim47555, align 8
%clofunc47556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40405)
musttail call tailcc void %clofunc47556(%struct.ScmObj* %k40405, %struct.ScmObj* %args46592$k40405$2)
ret void
}

define tailcc void @proc_clo$ae44003(%struct.ScmObj* %env$ae44003,%struct.ScmObj* %current_45args46594) {
%stackaddr$prim47557 = alloca %struct.ScmObj*, align 8
%k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46594)
store volatile %struct.ScmObj* %k40406, %struct.ScmObj** %stackaddr$prim47557, align 8
%stackaddr$prim47558 = alloca %struct.ScmObj*, align 8
%current_45args46595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46594)
store volatile %struct.ScmObj* %current_45args46595, %struct.ScmObj** %stackaddr$prim47558, align 8
%stackaddr$prim47559 = alloca %struct.ScmObj*, align 8
%x40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46595)
store volatile %struct.ScmObj* %x40223, %struct.ScmObj** %stackaddr$prim47559, align 8
%stackaddr$prim47560 = alloca %struct.ScmObj*, align 8
%current_45args46596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46595)
store volatile %struct.ScmObj* %current_45args46596, %struct.ScmObj** %stackaddr$prim47560, align 8
%stackaddr$prim47561 = alloca %struct.ScmObj*, align 8
%y40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46596)
store volatile %struct.ScmObj* %y40222, %struct.ScmObj** %stackaddr$prim47561, align 8
%stackaddr$prim47562 = alloca %struct.ScmObj*, align 8
%cpsprim40407 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %y40222, %struct.ScmObj* %x40223)
store volatile %struct.ScmObj* %cpsprim40407, %struct.ScmObj** %stackaddr$prim47562, align 8
%ae44007 = call %struct.ScmObj* @const_init_int(i64 0)
%args46598$k40406$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47563 = alloca %struct.ScmObj*, align 8
%args46598$k40406$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40407, %struct.ScmObj* %args46598$k40406$0)
store volatile %struct.ScmObj* %args46598$k40406$1, %struct.ScmObj** %stackaddr$prim47563, align 8
%stackaddr$prim47564 = alloca %struct.ScmObj*, align 8
%args46598$k40406$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44007, %struct.ScmObj* %args46598$k40406$1)
store volatile %struct.ScmObj* %args46598$k40406$2, %struct.ScmObj** %stackaddr$prim47564, align 8
%clofunc47565 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40406)
musttail call tailcc void %clofunc47565(%struct.ScmObj* %k40406, %struct.ScmObj* %args46598$k40406$2)
ret void
}

define tailcc void @proc_clo$ae43916(%struct.ScmObj* %env$ae43916,%struct.ScmObj* %current_45args46600) {
%stackaddr$prim47566 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46600)
store volatile %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$prim47566, align 8
%stackaddr$prim47567 = alloca %struct.ScmObj*, align 8
%current_45args46601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46600)
store volatile %struct.ScmObj* %current_45args46601, %struct.ScmObj** %stackaddr$prim47567, align 8
%stackaddr$prim47568 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46601)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim47568, align 8
%stackaddr$prim47569 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47569, align 8
%truthy$cmp47570 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40340)
%cmp$cmp47570 = icmp eq i64 %truthy$cmp47570, 1
br i1 %cmp$cmp47570, label %truebranch$cmp47570, label %falsebranch$cmp47570
truebranch$cmp47570:
%stackaddr$prim47571 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47571, align 8
%ae43921 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47572 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40341, %struct.ScmObj* %ae43921)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47572, align 8
%truthy$cmp47573 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40342)
%cmp$cmp47573 = icmp eq i64 %truthy$cmp47573, 1
br i1 %cmp$cmp47573, label %truebranch$cmp47573, label %falsebranch$cmp47573
truebranch$cmp47573:
%ae43924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47574 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43924)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim47574, align 8
%ae43926 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4392647575, i32 0, i32 0))
%stackaddr$prim47576 = alloca %struct.ScmObj*, align 8
%cpsprim40409 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40343, %struct.ScmObj* %ae43926)
store volatile %struct.ScmObj* %cpsprim40409, %struct.ScmObj** %stackaddr$prim47576, align 8
%ae43928 = call %struct.ScmObj* @const_init_int(i64 0)
%args46603$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47577 = alloca %struct.ScmObj*, align 8
%args46603$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40409, %struct.ScmObj* %args46603$k40408$0)
store volatile %struct.ScmObj* %args46603$k40408$1, %struct.ScmObj** %stackaddr$prim47577, align 8
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%args46603$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43928, %struct.ScmObj* %args46603$k40408$1)
store volatile %struct.ScmObj* %args46603$k40408$2, %struct.ScmObj** %stackaddr$prim47578, align 8
%clofunc47579 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47579(%struct.ScmObj* %k40408, %struct.ScmObj* %args46603$k40408$2)
ret void
falsebranch$cmp47573:
%ae43946 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43947 = call %struct.ScmObj* @const_init_false()
%args46604$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47580 = alloca %struct.ScmObj*, align 8
%args46604$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43947, %struct.ScmObj* %args46604$k40408$0)
store volatile %struct.ScmObj* %args46604$k40408$1, %struct.ScmObj** %stackaddr$prim47580, align 8
%stackaddr$prim47581 = alloca %struct.ScmObj*, align 8
%args46604$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43946, %struct.ScmObj* %args46604$k40408$1)
store volatile %struct.ScmObj* %args46604$k40408$2, %struct.ScmObj** %stackaddr$prim47581, align 8
%clofunc47582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47582(%struct.ScmObj* %k40408, %struct.ScmObj* %args46604$k40408$2)
ret void
falsebranch$cmp47570:
%ae43968 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43969 = call %struct.ScmObj* @const_init_false()
%args46605$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47583 = alloca %struct.ScmObj*, align 8
%args46605$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43969, %struct.ScmObj* %args46605$k40408$0)
store volatile %struct.ScmObj* %args46605$k40408$1, %struct.ScmObj** %stackaddr$prim47583, align 8
%stackaddr$prim47584 = alloca %struct.ScmObj*, align 8
%args46605$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43968, %struct.ScmObj* %args46605$k40408$1)
store volatile %struct.ScmObj* %args46605$k40408$2, %struct.ScmObj** %stackaddr$prim47584, align 8
%clofunc47585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47585(%struct.ScmObj* %k40408, %struct.ScmObj* %args46605$k40408$2)
ret void
}

define tailcc void @proc_clo$ae43890(%struct.ScmObj* %env$ae43890,%struct.ScmObj* %current_45args46607) {
%stackaddr$prim47586 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim47586, align 8
%stackaddr$prim47587 = alloca %struct.ScmObj*, align 8
%current_45args46608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %current_45args46608, %struct.ScmObj** %stackaddr$prim47587, align 8
%stackaddr$prim47588 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46608)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim47588, align 8
%stackaddr$prim47589 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47589, align 8
%stackaddr$prim47590 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47590, align 8
%stackaddr$prim47591 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47591, align 8
%stackaddr$prim47592 = alloca %struct.ScmObj*, align 8
%cpsprim40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %cpsprim40411, %struct.ScmObj** %stackaddr$prim47592, align 8
%ae43896 = call %struct.ScmObj* @const_init_int(i64 0)
%args46610$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47593 = alloca %struct.ScmObj*, align 8
%args46610$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40411, %struct.ScmObj* %args46610$k40410$0)
store volatile %struct.ScmObj* %args46610$k40410$1, %struct.ScmObj** %stackaddr$prim47593, align 8
%stackaddr$prim47594 = alloca %struct.ScmObj*, align 8
%args46610$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43896, %struct.ScmObj* %args46610$k40410$1)
store volatile %struct.ScmObj* %args46610$k40410$2, %struct.ScmObj** %stackaddr$prim47594, align 8
%clofunc47595 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc47595(%struct.ScmObj* %k40410, %struct.ScmObj* %args46610$k40410$2)
ret void
}

define tailcc void @proc_clo$ae43866(%struct.ScmObj* %env$ae43866,%struct.ScmObj* %current_45args46612) {
%stackaddr$prim47596 = alloca %struct.ScmObj*, align 8
%k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %k40412, %struct.ScmObj** %stackaddr$prim47596, align 8
%stackaddr$prim47597 = alloca %struct.ScmObj*, align 8
%current_45args46613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %current_45args46613, %struct.ScmObj** %stackaddr$prim47597, align 8
%stackaddr$prim47598 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46613)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim47598, align 8
%stackaddr$prim47599 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47599, align 8
%stackaddr$prim47600 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40335)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47600, align 8
%stackaddr$prim47601 = alloca %struct.ScmObj*, align 8
%cpsprim40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %cpsprim40413, %struct.ScmObj** %stackaddr$prim47601, align 8
%ae43871 = call %struct.ScmObj* @const_init_int(i64 0)
%args46615$k40412$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47602 = alloca %struct.ScmObj*, align 8
%args46615$k40412$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40413, %struct.ScmObj* %args46615$k40412$0)
store volatile %struct.ScmObj* %args46615$k40412$1, %struct.ScmObj** %stackaddr$prim47602, align 8
%stackaddr$prim47603 = alloca %struct.ScmObj*, align 8
%args46615$k40412$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43871, %struct.ScmObj* %args46615$k40412$1)
store volatile %struct.ScmObj* %args46615$k40412$2, %struct.ScmObj** %stackaddr$prim47603, align 8
%clofunc47604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40412)
musttail call tailcc void %clofunc47604(%struct.ScmObj* %k40412, %struct.ScmObj* %args46615$k40412$2)
ret void
}

define tailcc void @proc_clo$ae43844(%struct.ScmObj* %env$ae43844,%struct.ScmObj* %current_45args46617) {
%stackaddr$prim47605 = alloca %struct.ScmObj*, align 8
%k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %k40414, %struct.ScmObj** %stackaddr$prim47605, align 8
%stackaddr$prim47606 = alloca %struct.ScmObj*, align 8
%current_45args46618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %current_45args46618, %struct.ScmObj** %stackaddr$prim47606, align 8
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46618)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim47607, align 8
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47608, align 8
%stackaddr$prim47609 = alloca %struct.ScmObj*, align 8
%cpsprim40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %cpsprim40415, %struct.ScmObj** %stackaddr$prim47609, align 8
%ae43848 = call %struct.ScmObj* @const_init_int(i64 0)
%args46620$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47610 = alloca %struct.ScmObj*, align 8
%args46620$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40415, %struct.ScmObj* %args46620$k40414$0)
store volatile %struct.ScmObj* %args46620$k40414$1, %struct.ScmObj** %stackaddr$prim47610, align 8
%stackaddr$prim47611 = alloca %struct.ScmObj*, align 8
%args46620$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43848, %struct.ScmObj* %args46620$k40414$1)
store volatile %struct.ScmObj* %args46620$k40414$2, %struct.ScmObj** %stackaddr$prim47611, align 8
%clofunc47612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc47612(%struct.ScmObj* %k40414, %struct.ScmObj* %args46620$k40414$2)
ret void
}

define tailcc void @proc_clo$ae43824(%struct.ScmObj* %env$ae43824,%struct.ScmObj* %current_45args46622) {
%stackaddr$prim47613 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46622)
store volatile %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$prim47613, align 8
%stackaddr$prim47614 = alloca %struct.ScmObj*, align 8
%current_45args46623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46622)
store volatile %struct.ScmObj* %current_45args46623, %struct.ScmObj** %stackaddr$prim47614, align 8
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim47615, align 8
%stackaddr$prim47616 = alloca %struct.ScmObj*, align 8
%cpsprim40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40417, %struct.ScmObj** %stackaddr$prim47616, align 8
%ae43827 = call %struct.ScmObj* @const_init_int(i64 0)
%args46625$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47617 = alloca %struct.ScmObj*, align 8
%args46625$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40417, %struct.ScmObj* %args46625$k40416$0)
store volatile %struct.ScmObj* %args46625$k40416$1, %struct.ScmObj** %stackaddr$prim47617, align 8
%stackaddr$prim47618 = alloca %struct.ScmObj*, align 8
%args46625$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43827, %struct.ScmObj* %args46625$k40416$1)
store volatile %struct.ScmObj* %args46625$k40416$2, %struct.ScmObj** %stackaddr$prim47618, align 8
%clofunc47619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc47619(%struct.ScmObj* %k40416, %struct.ScmObj* %args46625$k40416$2)
ret void
}

define tailcc void @proc_clo$ae43726(%struct.ScmObj* %env$ae43726,%struct.ScmObj* %args4016840418) {
%stackaddr$env-ref47620 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43726, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47620
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840418)
store volatile %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840418)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$prim47623 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47623, align 8
%truthy$cmp47624 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp47624 = icmp eq i64 %truthy$cmp47624, 1
br i1 %cmp$cmp47624, label %truebranch$cmp47624, label %falsebranch$cmp47624
truebranch$cmp47624:
%ae43732 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43733 = call %struct.ScmObj* @const_init_int(i64 1)
%args46627$k40419$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47625 = alloca %struct.ScmObj*, align 8
%args46627$k40419$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43733, %struct.ScmObj* %args46627$k40419$0)
store volatile %struct.ScmObj* %args46627$k40419$1, %struct.ScmObj** %stackaddr$prim47625, align 8
%stackaddr$prim47626 = alloca %struct.ScmObj*, align 8
%args46627$k40419$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43732, %struct.ScmObj* %args46627$k40419$1)
store volatile %struct.ScmObj* %args46627$k40419$2, %struct.ScmObj** %stackaddr$prim47626, align 8
%clofunc47627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40419)
musttail call tailcc void %clofunc47627(%struct.ScmObj* %k40419, %struct.ScmObj* %args46627$k40419$2)
ret void
falsebranch$cmp47624:
%stackaddr$prim47628 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47628, align 8
%stackaddr$prim47629 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47629, align 8
%truthy$cmp47630 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40330)
%cmp$cmp47630 = icmp eq i64 %truthy$cmp47630, 1
br i1 %cmp$cmp47630, label %truebranch$cmp47630, label %falsebranch$cmp47630
truebranch$cmp47630:
%stackaddr$prim47631 = alloca %struct.ScmObj*, align 8
%cpsprim40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40420, %struct.ScmObj** %stackaddr$prim47631, align 8
%ae43745 = call %struct.ScmObj* @const_init_int(i64 0)
%args46628$k40419$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47632 = alloca %struct.ScmObj*, align 8
%args46628$k40419$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40420, %struct.ScmObj* %args46628$k40419$0)
store volatile %struct.ScmObj* %args46628$k40419$1, %struct.ScmObj** %stackaddr$prim47632, align 8
%stackaddr$prim47633 = alloca %struct.ScmObj*, align 8
%args46628$k40419$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43745, %struct.ScmObj* %args46628$k40419$1)
store volatile %struct.ScmObj* %args46628$k40419$2, %struct.ScmObj** %stackaddr$prim47633, align 8
%clofunc47634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40419)
musttail call tailcc void %clofunc47634(%struct.ScmObj* %k40419, %struct.ScmObj* %args46628$k40419$2)
ret void
falsebranch$cmp47630:
%stackaddr$makeclosure47635 = alloca %struct.ScmObj*, align 8
%fptrToInt47636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43750 to i64
%ae43750 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47636)
store volatile %struct.ScmObj* %ae43750, %struct.ScmObj** %stackaddr$makeclosure47635, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %args40168, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %k40419, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %_37foldl140107, i64 2)
%ae43751 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47637 = alloca %struct.ScmObj*, align 8
%fptrToInt47638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43752 to i64
%ae43752 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47638)
store volatile %struct.ScmObj* %ae43752, %struct.ScmObj** %stackaddr$makeclosure47637, align 8
%args46638$ae43750$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47639 = alloca %struct.ScmObj*, align 8
%args46638$ae43750$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43752, %struct.ScmObj* %args46638$ae43750$0)
store volatile %struct.ScmObj* %args46638$ae43750$1, %struct.ScmObj** %stackaddr$prim47639, align 8
%stackaddr$prim47640 = alloca %struct.ScmObj*, align 8
%args46638$ae43750$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43751, %struct.ScmObj* %args46638$ae43750$1)
store volatile %struct.ScmObj* %args46638$ae43750$2, %struct.ScmObj** %stackaddr$prim47640, align 8
%clofunc47641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43750)
musttail call tailcc void %clofunc47641(%struct.ScmObj* %ae43750, %struct.ScmObj* %args46638$ae43750$2)
ret void
}

define tailcc void @proc_clo$ae43750(%struct.ScmObj* %env$ae43750,%struct.ScmObj* %current_45args46629) {
%stackaddr$env-ref47642 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 0)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref47642
%stackaddr$env-ref47643 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 1)
store %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$env-ref47643
%stackaddr$env-ref47644 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47644
%stackaddr$prim47645 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46629)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim47645, align 8
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%current_45args46630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46629)
store volatile %struct.ScmObj* %current_45args46630, %struct.ScmObj** %stackaddr$prim47646, align 8
%stackaddr$prim47647 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46630)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47647, align 8
%stackaddr$prim47648 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47648, align 8
%stackaddr$prim47649 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47649, align 8
%args46632$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47650 = alloca %struct.ScmObj*, align 8
%args46632$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40333, %struct.ScmObj* %args46632$_37foldl140107$0)
store volatile %struct.ScmObj* %args46632$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47650, align 8
%stackaddr$prim47651 = alloca %struct.ScmObj*, align 8
%args46632$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40332, %struct.ScmObj* %args46632$_37foldl140107$1)
store volatile %struct.ScmObj* %args46632$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47651, align 8
%stackaddr$prim47652 = alloca %struct.ScmObj*, align 8
%args46632$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %args46632$_37foldl140107$2)
store volatile %struct.ScmObj* %args46632$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47652, align 8
%stackaddr$prim47653 = alloca %struct.ScmObj*, align 8
%args46632$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40419, %struct.ScmObj* %args46632$_37foldl140107$3)
store volatile %struct.ScmObj* %args46632$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47653, align 8
%clofunc47654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47654(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46632$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43752(%struct.ScmObj* %env$ae43752,%struct.ScmObj* %current_45args46633) {
%stackaddr$prim47655 = alloca %struct.ScmObj*, align 8
%k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %k40422, %struct.ScmObj** %stackaddr$prim47655, align 8
%stackaddr$prim47656 = alloca %struct.ScmObj*, align 8
%current_45args46634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %current_45args46634, %struct.ScmObj** %stackaddr$prim47656, align 8
%stackaddr$prim47657 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim47657, align 8
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%current_45args46635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46634)
store volatile %struct.ScmObj* %current_45args46635, %struct.ScmObj** %stackaddr$prim47658, align 8
%stackaddr$prim47659 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim47659, align 8
%stackaddr$prim47660 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim47660, align 8
%ae43756 = call %struct.ScmObj* @const_init_int(i64 0)
%args46637$k40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47661 = alloca %struct.ScmObj*, align 8
%args46637$k40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args46637$k40422$0)
store volatile %struct.ScmObj* %args46637$k40422$1, %struct.ScmObj** %stackaddr$prim47661, align 8
%stackaddr$prim47662 = alloca %struct.ScmObj*, align 8
%args46637$k40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43756, %struct.ScmObj* %args46637$k40422$1)
store volatile %struct.ScmObj* %args46637$k40422$2, %struct.ScmObj** %stackaddr$prim47662, align 8
%clofunc47663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40422)
musttail call tailcc void %clofunc47663(%struct.ScmObj* %k40422, %struct.ScmObj* %args46637$k40422$2)
ret void
}

define tailcc void @proc_clo$ae43322(%struct.ScmObj* %env$ae43322,%struct.ScmObj* %current_45args46640) {
%stackaddr$prim47664 = alloca %struct.ScmObj*, align 8
%k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46640)
store volatile %struct.ScmObj* %k40424, %struct.ScmObj** %stackaddr$prim47664, align 8
%stackaddr$prim47665 = alloca %struct.ScmObj*, align 8
%current_45args46641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46640)
store volatile %struct.ScmObj* %current_45args46641, %struct.ScmObj** %stackaddr$prim47665, align 8
%stackaddr$prim47666 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim47666, align 8
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%current_45args46642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %current_45args46642, %struct.ScmObj** %stackaddr$prim47667, align 8
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46642)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim47668, align 8
%ae43323 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43323, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim47669, align 8
%stackaddr$makeclosure47670 = alloca %struct.ScmObj*, align 8
%fptrToInt47671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43325 to i64
%ae43325 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47671)
store volatile %struct.ScmObj* %ae43325, %struct.ScmObj** %stackaddr$makeclosure47670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %k40424, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43325, %struct.ScmObj* %v40173, i64 2)
%ae43326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47672 = alloca %struct.ScmObj*, align 8
%fptrToInt47673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43327 to i64
%ae43327 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47673)
store volatile %struct.ScmObj* %ae43327, %struct.ScmObj** %stackaddr$makeclosure47672, align 8
%args46664$ae43325$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47674 = alloca %struct.ScmObj*, align 8
%args46664$ae43325$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43327, %struct.ScmObj* %args46664$ae43325$0)
store volatile %struct.ScmObj* %args46664$ae43325$1, %struct.ScmObj** %stackaddr$prim47674, align 8
%stackaddr$prim47675 = alloca %struct.ScmObj*, align 8
%args46664$ae43325$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43326, %struct.ScmObj* %args46664$ae43325$1)
store volatile %struct.ScmObj* %args46664$ae43325$2, %struct.ScmObj** %stackaddr$prim47675, align 8
%clofunc47676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43325)
musttail call tailcc void %clofunc47676(%struct.ScmObj* %ae43325, %struct.ScmObj* %args46664$ae43325$2)
ret void
}

define tailcc void @proc_clo$ae43325(%struct.ScmObj* %env$ae43325,%struct.ScmObj* %current_45args46644) {
%stackaddr$env-ref47677 = alloca %struct.ScmObj*, align 8
%k40424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 0)
store %struct.ScmObj* %k40424, %struct.ScmObj** %stackaddr$env-ref47677
%stackaddr$env-ref47678 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47678
%stackaddr$env-ref47679 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43325, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47679
%stackaddr$prim47680 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46644)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim47680, align 8
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%current_45args46645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46644)
store volatile %struct.ScmObj* %current_45args46645, %struct.ScmObj** %stackaddr$prim47681, align 8
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46645)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47682, align 8
%stackaddr$makeclosure47683 = alloca %struct.ScmObj*, align 8
%fptrToInt47684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43341 to i64
%ae43341 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47684)
store volatile %struct.ScmObj* %ae43341, %struct.ScmObj** %stackaddr$makeclosure47683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43341, %struct.ScmObj* %k40424, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43341, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43341, %struct.ScmObj* %v40173, i64 2)
%stackaddr$makeclosure47685 = alloca %struct.ScmObj*, align 8
%fptrToInt47686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43342 to i64
%ae43342 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47686)
store volatile %struct.ScmObj* %ae43342, %struct.ScmObj** %stackaddr$makeclosure47685, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43342, %struct.ScmObj* %k40424, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43342, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43342, %struct.ScmObj* %v40173, i64 2)
%args46659$anf_45bind40320$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47687 = alloca %struct.ScmObj*, align 8
%args46659$anf_45bind40320$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43342, %struct.ScmObj* %args46659$anf_45bind40320$0)
store volatile %struct.ScmObj* %args46659$anf_45bind40320$1, %struct.ScmObj** %stackaddr$prim47687, align 8
%stackaddr$prim47688 = alloca %struct.ScmObj*, align 8
%args46659$anf_45bind40320$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43341, %struct.ScmObj* %args46659$anf_45bind40320$1)
store volatile %struct.ScmObj* %args46659$anf_45bind40320$2, %struct.ScmObj** %stackaddr$prim47688, align 8
%clofunc47689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40320)
musttail call tailcc void %clofunc47689(%struct.ScmObj* %anf_45bind40320, %struct.ScmObj* %args46659$anf_45bind40320$2)
ret void
}

define tailcc void @proc_clo$ae43341(%struct.ScmObj* %env$ae43341,%struct.ScmObj* %current_45args46647) {
%stackaddr$env-ref47690 = alloca %struct.ScmObj*, align 8
%k40424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43341, i64 0)
store %struct.ScmObj* %k40424, %struct.ScmObj** %stackaddr$env-ref47690
%stackaddr$env-ref47691 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43341, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47691
%stackaddr$env-ref47692 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43341, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47692
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46647)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim47693, align 8
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%current_45args46648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46647)
store volatile %struct.ScmObj* %current_45args46648, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46648)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47695, align 8
%ae43450 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47696 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43450)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47696, align 8
%stackaddr$prim47697 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47697, align 8
%truthy$cmp47698 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40322)
%cmp$cmp47698 = icmp eq i64 %truthy$cmp47698, 1
br i1 %cmp$cmp47698, label %truebranch$cmp47698, label %falsebranch$cmp47698
truebranch$cmp47698:
%ae43454 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43455 = call %struct.ScmObj* @const_init_false()
%args46650$k40424$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47699 = alloca %struct.ScmObj*, align 8
%args46650$k40424$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43455, %struct.ScmObj* %args46650$k40424$0)
store volatile %struct.ScmObj* %args46650$k40424$1, %struct.ScmObj** %stackaddr$prim47699, align 8
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%args46650$k40424$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43454, %struct.ScmObj* %args46650$k40424$1)
store volatile %struct.ScmObj* %args46650$k40424$2, %struct.ScmObj** %stackaddr$prim47700, align 8
%clofunc47701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40424)
musttail call tailcc void %clofunc47701(%struct.ScmObj* %k40424, %struct.ScmObj* %args46650$k40424$2)
ret void
falsebranch$cmp47698:
%ae43463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47702 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43463)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47702, align 8
%stackaddr$prim47703 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47703, align 8
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47704, align 8
%truthy$cmp47705 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40325)
%cmp$cmp47705 = icmp eq i64 %truthy$cmp47705, 1
br i1 %cmp$cmp47705, label %truebranch$cmp47705, label %falsebranch$cmp47705
truebranch$cmp47705:
%ae43469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47706 = alloca %struct.ScmObj*, align 8
%cpsprim40427 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43469)
store volatile %struct.ScmObj* %cpsprim40427, %struct.ScmObj** %stackaddr$prim47706, align 8
%ae43471 = call %struct.ScmObj* @const_init_int(i64 0)
%args46651$k40424$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%args46651$k40424$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40427, %struct.ScmObj* %args46651$k40424$0)
store volatile %struct.ScmObj* %args46651$k40424$1, %struct.ScmObj** %stackaddr$prim47707, align 8
%stackaddr$prim47708 = alloca %struct.ScmObj*, align 8
%args46651$k40424$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43471, %struct.ScmObj* %args46651$k40424$1)
store volatile %struct.ScmObj* %args46651$k40424$2, %struct.ScmObj** %stackaddr$prim47708, align 8
%clofunc47709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40424)
musttail call tailcc void %clofunc47709(%struct.ScmObj* %k40424, %struct.ScmObj* %args46651$k40424$2)
ret void
falsebranch$cmp47705:
%ae43482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47710 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43482)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47710, align 8
%stackaddr$prim47711 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47711, align 8
%ae43485 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43485, %struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47712, align 8
%args46652$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47713 = alloca %struct.ScmObj*, align 8
%args46652$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46652$cc40175$0)
store volatile %struct.ScmObj* %args46652$cc40175$1, %struct.ScmObj** %stackaddr$prim47713, align 8
%stackaddr$prim47714 = alloca %struct.ScmObj*, align 8
%args46652$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40424, %struct.ScmObj* %args46652$cc40175$1)
store volatile %struct.ScmObj* %args46652$cc40175$2, %struct.ScmObj** %stackaddr$prim47714, align 8
%clofunc47715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47715(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46652$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43342(%struct.ScmObj* %env$ae43342,%struct.ScmObj* %current_45args46653) {
%stackaddr$env-ref47716 = alloca %struct.ScmObj*, align 8
%k40424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43342, i64 0)
store %struct.ScmObj* %k40424, %struct.ScmObj** %stackaddr$env-ref47716
%stackaddr$env-ref47717 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43342, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47717
%stackaddr$env-ref47718 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43342, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47718
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46653)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim47719, align 8
%stackaddr$prim47720 = alloca %struct.ScmObj*, align 8
%current_45args46654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46653)
store volatile %struct.ScmObj* %current_45args46654, %struct.ScmObj** %stackaddr$prim47720, align 8
%stackaddr$prim47721 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46654)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47721, align 8
%ae43344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47722 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43344)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47722, align 8
%stackaddr$prim47723 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47723, align 8
%truthy$cmp47724 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40322)
%cmp$cmp47724 = icmp eq i64 %truthy$cmp47724, 1
br i1 %cmp$cmp47724, label %truebranch$cmp47724, label %falsebranch$cmp47724
truebranch$cmp47724:
%ae43348 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43349 = call %struct.ScmObj* @const_init_false()
%args46656$k40424$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47725 = alloca %struct.ScmObj*, align 8
%args46656$k40424$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43349, %struct.ScmObj* %args46656$k40424$0)
store volatile %struct.ScmObj* %args46656$k40424$1, %struct.ScmObj** %stackaddr$prim47725, align 8
%stackaddr$prim47726 = alloca %struct.ScmObj*, align 8
%args46656$k40424$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43348, %struct.ScmObj* %args46656$k40424$1)
store volatile %struct.ScmObj* %args46656$k40424$2, %struct.ScmObj** %stackaddr$prim47726, align 8
%clofunc47727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40424)
musttail call tailcc void %clofunc47727(%struct.ScmObj* %k40424, %struct.ScmObj* %args46656$k40424$2)
ret void
falsebranch$cmp47724:
%ae43357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47728 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43357)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47728, align 8
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47729, align 8
%stackaddr$prim47730 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47730, align 8
%truthy$cmp47731 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40325)
%cmp$cmp47731 = icmp eq i64 %truthy$cmp47731, 1
br i1 %cmp$cmp47731, label %truebranch$cmp47731, label %falsebranch$cmp47731
truebranch$cmp47731:
%ae43363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47732 = alloca %struct.ScmObj*, align 8
%cpsprim40427 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43363)
store volatile %struct.ScmObj* %cpsprim40427, %struct.ScmObj** %stackaddr$prim47732, align 8
%ae43365 = call %struct.ScmObj* @const_init_int(i64 0)
%args46657$k40424$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47733 = alloca %struct.ScmObj*, align 8
%args46657$k40424$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40427, %struct.ScmObj* %args46657$k40424$0)
store volatile %struct.ScmObj* %args46657$k40424$1, %struct.ScmObj** %stackaddr$prim47733, align 8
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%args46657$k40424$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43365, %struct.ScmObj* %args46657$k40424$1)
store volatile %struct.ScmObj* %args46657$k40424$2, %struct.ScmObj** %stackaddr$prim47734, align 8
%clofunc47735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40424)
musttail call tailcc void %clofunc47735(%struct.ScmObj* %k40424, %struct.ScmObj* %args46657$k40424$2)
ret void
falsebranch$cmp47731:
%ae43376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43376)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47736, align 8
%stackaddr$prim47737 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47737, align 8
%ae43379 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47738 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43379, %struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47738, align 8
%args46658$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47739 = alloca %struct.ScmObj*, align 8
%args46658$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46658$cc40175$0)
store volatile %struct.ScmObj* %args46658$cc40175$1, %struct.ScmObj** %stackaddr$prim47739, align 8
%stackaddr$prim47740 = alloca %struct.ScmObj*, align 8
%args46658$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40424, %struct.ScmObj* %args46658$cc40175$1)
store volatile %struct.ScmObj* %args46658$cc40175$2, %struct.ScmObj** %stackaddr$prim47740, align 8
%clofunc47741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47741(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46658$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43327(%struct.ScmObj* %env$ae43327,%struct.ScmObj* %current_45args46660) {
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46660)
store volatile %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$prim47742, align 8
%stackaddr$prim47743 = alloca %struct.ScmObj*, align 8
%current_45args46661 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46660)
store volatile %struct.ScmObj* %current_45args46661, %struct.ScmObj** %stackaddr$prim47743, align 8
%stackaddr$prim47744 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46661)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim47744, align 8
%args46663$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47745 = alloca %struct.ScmObj*, align 8
%args46663$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args46663$u40176$0)
store volatile %struct.ScmObj* %args46663$u40176$1, %struct.ScmObj** %stackaddr$prim47745, align 8
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%args46663$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40428, %struct.ScmObj* %args46663$u40176$1)
store volatile %struct.ScmObj* %args46663$u40176$2, %struct.ScmObj** %stackaddr$prim47746, align 8
%clofunc47747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc47747(%struct.ScmObj* %u40176, %struct.ScmObj* %args46663$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42786(%struct.ScmObj* %env$ae42786,%struct.ScmObj* %current_45args46666) {
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46666)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim47748, align 8
%stackaddr$prim47749 = alloca %struct.ScmObj*, align 8
%current_45args46667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46666)
store volatile %struct.ScmObj* %current_45args46667, %struct.ScmObj** %stackaddr$prim47749, align 8
%stackaddr$prim47750 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim47750, align 8
%stackaddr$prim47751 = alloca %struct.ScmObj*, align 8
%current_45args46668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %current_45args46668, %struct.ScmObj** %stackaddr$prim47751, align 8
%stackaddr$prim47752 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46668)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim47752, align 8
%ae42787 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47753 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42787, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim47753, align 8
%ae42789 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47754 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42789, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47754, align 8
%stackaddr$makeclosure47755 = alloca %struct.ScmObj*, align 8
%fptrToInt47756 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42791 to i64
%ae42791 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47756)
store volatile %struct.ScmObj* %ae42791, %struct.ScmObj** %stackaddr$makeclosure47755, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42791, %struct.ScmObj* %k40429, i64 2)
%ae42792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47757 = alloca %struct.ScmObj*, align 8
%fptrToInt47758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42793 to i64
%ae42793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47758)
store volatile %struct.ScmObj* %ae42793, %struct.ScmObj** %stackaddr$makeclosure47757, align 8
%args46688$ae42791$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47759 = alloca %struct.ScmObj*, align 8
%args46688$ae42791$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42793, %struct.ScmObj* %args46688$ae42791$0)
store volatile %struct.ScmObj* %args46688$ae42791$1, %struct.ScmObj** %stackaddr$prim47759, align 8
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%args46688$ae42791$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42792, %struct.ScmObj* %args46688$ae42791$1)
store volatile %struct.ScmObj* %args46688$ae42791$2, %struct.ScmObj** %stackaddr$prim47760, align 8
%clofunc47761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42791)
musttail call tailcc void %clofunc47761(%struct.ScmObj* %ae42791, %struct.ScmObj* %args46688$ae42791$2)
ret void
}

define tailcc void @proc_clo$ae42791(%struct.ScmObj* %env$ae42791,%struct.ScmObj* %current_45args46670) {
%stackaddr$env-ref47762 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47762
%stackaddr$env-ref47763 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47763
%stackaddr$env-ref47764 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42791, i64 2)
store %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$env-ref47764
%stackaddr$prim47765 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim47765, align 8
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%current_45args46671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46670)
store volatile %struct.ScmObj* %current_45args46671, %struct.ScmObj** %stackaddr$prim47766, align 8
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47767, align 8
%stackaddr$makeclosure47768 = alloca %struct.ScmObj*, align 8
%fptrToInt47769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42807 to i64
%ae42807 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47769)
store volatile %struct.ScmObj* %ae42807, %struct.ScmObj** %stackaddr$makeclosure47768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42807, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42807, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42807, %struct.ScmObj* %k40429, i64 2)
%stackaddr$makeclosure47770 = alloca %struct.ScmObj*, align 8
%fptrToInt47771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42808 to i64
%ae42808 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47771)
store volatile %struct.ScmObj* %ae42808, %struct.ScmObj** %stackaddr$makeclosure47770, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42808, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42808, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42808, %struct.ScmObj* %k40429, i64 2)
%args46683$anf_45bind40313$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%args46683$anf_45bind40313$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42808, %struct.ScmObj* %args46683$anf_45bind40313$0)
store volatile %struct.ScmObj* %args46683$anf_45bind40313$1, %struct.ScmObj** %stackaddr$prim47772, align 8
%stackaddr$prim47773 = alloca %struct.ScmObj*, align 8
%args46683$anf_45bind40313$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42807, %struct.ScmObj* %args46683$anf_45bind40313$1)
store volatile %struct.ScmObj* %args46683$anf_45bind40313$2, %struct.ScmObj** %stackaddr$prim47773, align 8
%clofunc47774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40313)
musttail call tailcc void %clofunc47774(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args46683$anf_45bind40313$2)
ret void
}

define tailcc void @proc_clo$ae42807(%struct.ScmObj* %env$ae42807,%struct.ScmObj* %current_45args46673) {
%stackaddr$env-ref47775 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42807, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47775
%stackaddr$env-ref47776 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42807, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47776
%stackaddr$env-ref47777 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42807, i64 2)
store %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$env-ref47777
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim47778, align 8
%stackaddr$prim47779 = alloca %struct.ScmObj*, align 8
%current_45args46674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %current_45args46674, %struct.ScmObj** %stackaddr$prim47779, align 8
%stackaddr$prim47780 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46674)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47780, align 8
%ae42950 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47781 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42950)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47781, align 8
%ae42951 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47782 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42951, %struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47782, align 8
%truthy$cmp47783 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40315)
%cmp$cmp47783 = icmp eq i64 %truthy$cmp47783, 1
br i1 %cmp$cmp47783, label %truebranch$cmp47783, label %falsebranch$cmp47783
truebranch$cmp47783:
%ae42955 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%cpsprim40432 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42955)
store volatile %struct.ScmObj* %cpsprim40432, %struct.ScmObj** %stackaddr$prim47784, align 8
%ae42957 = call %struct.ScmObj* @const_init_int(i64 0)
%args46676$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%args46676$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40432, %struct.ScmObj* %args46676$k40429$0)
store volatile %struct.ScmObj* %args46676$k40429$1, %struct.ScmObj** %stackaddr$prim47785, align 8
%stackaddr$prim47786 = alloca %struct.ScmObj*, align 8
%args46676$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42957, %struct.ScmObj* %args46676$k40429$1)
store volatile %struct.ScmObj* %args46676$k40429$2, %struct.ScmObj** %stackaddr$prim47786, align 8
%clofunc47787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc47787(%struct.ScmObj* %k40429, %struct.ScmObj* %args46676$k40429$2)
ret void
falsebranch$cmp47783:
%ae42968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47788 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42968)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47788, align 8
%stackaddr$prim47789 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47789, align 8
%ae42971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47790 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42971, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47790, align 8
%ae42974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42974)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47791, align 8
%ae42976 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %ae42976)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47792, align 8
%ae42978 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47793 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42978, %struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47793, align 8
%args46677$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%args46677$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46677$cc40183$0)
store volatile %struct.ScmObj* %args46677$cc40183$1, %struct.ScmObj** %stackaddr$prim47794, align 8
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%args46677$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40429, %struct.ScmObj* %args46677$cc40183$1)
store volatile %struct.ScmObj* %args46677$cc40183$2, %struct.ScmObj** %stackaddr$prim47795, align 8
%clofunc47796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47796(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46677$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42808(%struct.ScmObj* %env$ae42808,%struct.ScmObj* %current_45args46678) {
%stackaddr$env-ref47797 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42808, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47797
%stackaddr$env-ref47798 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42808, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47798
%stackaddr$env-ref47799 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42808, i64 2)
store %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$env-ref47799
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%current_45args46679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %current_45args46679, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$prim47802 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46679)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47802, align 8
%ae42810 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47803 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42810)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47803, align 8
%ae42811 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47804 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42811, %struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47804, align 8
%truthy$cmp47805 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40315)
%cmp$cmp47805 = icmp eq i64 %truthy$cmp47805, 1
br i1 %cmp$cmp47805, label %truebranch$cmp47805, label %falsebranch$cmp47805
truebranch$cmp47805:
%ae42815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%cpsprim40432 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42815)
store volatile %struct.ScmObj* %cpsprim40432, %struct.ScmObj** %stackaddr$prim47806, align 8
%ae42817 = call %struct.ScmObj* @const_init_int(i64 0)
%args46681$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47807 = alloca %struct.ScmObj*, align 8
%args46681$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40432, %struct.ScmObj* %args46681$k40429$0)
store volatile %struct.ScmObj* %args46681$k40429$1, %struct.ScmObj** %stackaddr$prim47807, align 8
%stackaddr$prim47808 = alloca %struct.ScmObj*, align 8
%args46681$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42817, %struct.ScmObj* %args46681$k40429$1)
store volatile %struct.ScmObj* %args46681$k40429$2, %struct.ScmObj** %stackaddr$prim47808, align 8
%clofunc47809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc47809(%struct.ScmObj* %k40429, %struct.ScmObj* %args46681$k40429$2)
ret void
falsebranch$cmp47805:
%ae42828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47810 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42828)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47810, align 8
%stackaddr$prim47811 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47811, align 8
%ae42831 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42831, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47812, align 8
%ae42834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42834)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47813, align 8
%ae42836 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %ae42836)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47814, align 8
%ae42838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42838, %struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47815, align 8
%args46682$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47816 = alloca %struct.ScmObj*, align 8
%args46682$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46682$cc40183$0)
store volatile %struct.ScmObj* %args46682$cc40183$1, %struct.ScmObj** %stackaddr$prim47816, align 8
%stackaddr$prim47817 = alloca %struct.ScmObj*, align 8
%args46682$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40429, %struct.ScmObj* %args46682$cc40183$1)
store volatile %struct.ScmObj* %args46682$cc40183$2, %struct.ScmObj** %stackaddr$prim47817, align 8
%clofunc47818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47818(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46682$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42793(%struct.ScmObj* %env$ae42793,%struct.ScmObj* %current_45args46684) {
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$prim47819, align 8
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%current_45args46685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %current_45args46685, %struct.ScmObj** %stackaddr$prim47820, align 8
%stackaddr$prim47821 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46685)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim47821, align 8
%args46687$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47822 = alloca %struct.ScmObj*, align 8
%args46687$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args46687$u40184$0)
store volatile %struct.ScmObj* %args46687$u40184$1, %struct.ScmObj** %stackaddr$prim47822, align 8
%stackaddr$prim47823 = alloca %struct.ScmObj*, align 8
%args46687$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40433, %struct.ScmObj* %args46687$u40184$1)
store volatile %struct.ScmObj* %args46687$u40184$2, %struct.ScmObj** %stackaddr$prim47823, align 8
%clofunc47824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc47824(%struct.ScmObj* %u40184, %struct.ScmObj* %args46687$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42370(%struct.ScmObj* %env$ae42370,%struct.ScmObj* %current_45args46690) {
%stackaddr$prim47825 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim47825, align 8
%stackaddr$prim47826 = alloca %struct.ScmObj*, align 8
%current_45args46691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46690)
store volatile %struct.ScmObj* %current_45args46691, %struct.ScmObj** %stackaddr$prim47826, align 8
%stackaddr$prim47827 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim47827, align 8
%ae42371 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47828 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42371, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim47828, align 8
%stackaddr$makeclosure47829 = alloca %struct.ScmObj*, align 8
%fptrToInt47830 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42373 to i64
%ae42373 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47830)
store volatile %struct.ScmObj* %ae42373, %struct.ScmObj** %stackaddr$makeclosure47829, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42373, %struct.ScmObj* %k40434, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42373, %struct.ScmObj* %a40189, i64 1)
%ae42374 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47831 = alloca %struct.ScmObj*, align 8
%fptrToInt47832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42375 to i64
%ae42375 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47832)
store volatile %struct.ScmObj* %ae42375, %struct.ScmObj** %stackaddr$makeclosure47831, align 8
%args46713$ae42373$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47833 = alloca %struct.ScmObj*, align 8
%args46713$ae42373$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42375, %struct.ScmObj* %args46713$ae42373$0)
store volatile %struct.ScmObj* %args46713$ae42373$1, %struct.ScmObj** %stackaddr$prim47833, align 8
%stackaddr$prim47834 = alloca %struct.ScmObj*, align 8
%args46713$ae42373$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42374, %struct.ScmObj* %args46713$ae42373$1)
store volatile %struct.ScmObj* %args46713$ae42373$2, %struct.ScmObj** %stackaddr$prim47834, align 8
%clofunc47835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42373)
musttail call tailcc void %clofunc47835(%struct.ScmObj* %ae42373, %struct.ScmObj* %args46713$ae42373$2)
ret void
}

define tailcc void @proc_clo$ae42373(%struct.ScmObj* %env$ae42373,%struct.ScmObj* %current_45args46693) {
%stackaddr$env-ref47836 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42373, i64 0)
store %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$env-ref47836
%stackaddr$env-ref47837 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42373, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47837
%stackaddr$prim47838 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46693)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim47838, align 8
%stackaddr$prim47839 = alloca %struct.ScmObj*, align 8
%current_45args46694 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46693)
store volatile %struct.ScmObj* %current_45args46694, %struct.ScmObj** %stackaddr$prim47839, align 8
%stackaddr$prim47840 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47840, align 8
%stackaddr$makeclosure47841 = alloca %struct.ScmObj*, align 8
%fptrToInt47842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42392 to i64
%ae42392 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47842)
store volatile %struct.ScmObj* %ae42392, %struct.ScmObj** %stackaddr$makeclosure47841, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42392, %struct.ScmObj* %k40434, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42392, %struct.ScmObj* %a40189, i64 1)
%stackaddr$makeclosure47843 = alloca %struct.ScmObj*, align 8
%fptrToInt47844 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42393 to i64
%ae42393 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47844)
store volatile %struct.ScmObj* %ae42393, %struct.ScmObj** %stackaddr$makeclosure47843, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42393, %struct.ScmObj* %k40434, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42393, %struct.ScmObj* %a40189, i64 1)
%args46708$anf_45bind40305$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%args46708$anf_45bind40305$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42393, %struct.ScmObj* %args46708$anf_45bind40305$0)
store volatile %struct.ScmObj* %args46708$anf_45bind40305$1, %struct.ScmObj** %stackaddr$prim47845, align 8
%stackaddr$prim47846 = alloca %struct.ScmObj*, align 8
%args46708$anf_45bind40305$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42392, %struct.ScmObj* %args46708$anf_45bind40305$1)
store volatile %struct.ScmObj* %args46708$anf_45bind40305$2, %struct.ScmObj** %stackaddr$prim47846, align 8
%clofunc47847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40305)
musttail call tailcc void %clofunc47847(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args46708$anf_45bind40305$2)
ret void
}

define tailcc void @proc_clo$ae42392(%struct.ScmObj* %env$ae42392,%struct.ScmObj* %current_45args46696) {
%stackaddr$env-ref47848 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42392, i64 0)
store %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$env-ref47848
%stackaddr$env-ref47849 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42392, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47849
%stackaddr$prim47850 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46696)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim47850, align 8
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%current_45args46697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46696)
store volatile %struct.ScmObj* %current_45args46697, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47852, align 8
%ae42508 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42508)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47853, align 8
%stackaddr$prim47854 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47854, align 8
%truthy$cmp47855 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47855 = icmp eq i64 %truthy$cmp47855, 1
br i1 %cmp$cmp47855, label %truebranch$cmp47855, label %falsebranch$cmp47855
truebranch$cmp47855:
%ae42512 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42513 = call %struct.ScmObj* @const_init_true()
%args46699$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47856 = alloca %struct.ScmObj*, align 8
%args46699$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42513, %struct.ScmObj* %args46699$k40434$0)
store volatile %struct.ScmObj* %args46699$k40434$1, %struct.ScmObj** %stackaddr$prim47856, align 8
%stackaddr$prim47857 = alloca %struct.ScmObj*, align 8
%args46699$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42512, %struct.ScmObj* %args46699$k40434$1)
store volatile %struct.ScmObj* %args46699$k40434$2, %struct.ScmObj** %stackaddr$prim47857, align 8
%clofunc47858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47858(%struct.ScmObj* %k40434, %struct.ScmObj* %args46699$k40434$2)
ret void
falsebranch$cmp47855:
%ae42521 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42521)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47859, align 8
%stackaddr$prim47860 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47860, align 8
%truthy$cmp47861 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40309)
%cmp$cmp47861 = icmp eq i64 %truthy$cmp47861, 1
br i1 %cmp$cmp47861, label %truebranch$cmp47861, label %falsebranch$cmp47861
truebranch$cmp47861:
%ae42525 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47862 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42525)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47862, align 8
%stackaddr$prim47863 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47863, align 8
%ae42528 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47864 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42528)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47864, align 8
%stackaddr$prim47865 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47865, align 8
%ae42531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47866 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42531, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47866, align 8
%args46700$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47867 = alloca %struct.ScmObj*, align 8
%args46700$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46700$cc40190$0)
store volatile %struct.ScmObj* %args46700$cc40190$1, %struct.ScmObj** %stackaddr$prim47867, align 8
%stackaddr$prim47868 = alloca %struct.ScmObj*, align 8
%args46700$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40434, %struct.ScmObj* %args46700$cc40190$1)
store volatile %struct.ScmObj* %args46700$cc40190$2, %struct.ScmObj** %stackaddr$prim47868, align 8
%clofunc47869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47869(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46700$cc40190$2)
ret void
falsebranch$cmp47861:
%ae42564 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42565 = call %struct.ScmObj* @const_init_false()
%args46701$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47870 = alloca %struct.ScmObj*, align 8
%args46701$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42565, %struct.ScmObj* %args46701$k40434$0)
store volatile %struct.ScmObj* %args46701$k40434$1, %struct.ScmObj** %stackaddr$prim47870, align 8
%stackaddr$prim47871 = alloca %struct.ScmObj*, align 8
%args46701$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42564, %struct.ScmObj* %args46701$k40434$1)
store volatile %struct.ScmObj* %args46701$k40434$2, %struct.ScmObj** %stackaddr$prim47871, align 8
%clofunc47872 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47872(%struct.ScmObj* %k40434, %struct.ScmObj* %args46701$k40434$2)
ret void
}

define tailcc void @proc_clo$ae42393(%struct.ScmObj* %env$ae42393,%struct.ScmObj* %current_45args46702) {
%stackaddr$env-ref47873 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42393, i64 0)
store %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$env-ref47873
%stackaddr$env-ref47874 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42393, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47874
%stackaddr$prim47875 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim47875, align 8
%stackaddr$prim47876 = alloca %struct.ScmObj*, align 8
%current_45args46703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46702)
store volatile %struct.ScmObj* %current_45args46703, %struct.ScmObj** %stackaddr$prim47876, align 8
%stackaddr$prim47877 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47877, align 8
%ae42395 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47878 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42395)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47878, align 8
%stackaddr$prim47879 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47879, align 8
%truthy$cmp47880 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40307)
%cmp$cmp47880 = icmp eq i64 %truthy$cmp47880, 1
br i1 %cmp$cmp47880, label %truebranch$cmp47880, label %falsebranch$cmp47880
truebranch$cmp47880:
%ae42399 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42400 = call %struct.ScmObj* @const_init_true()
%args46705$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47881 = alloca %struct.ScmObj*, align 8
%args46705$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42400, %struct.ScmObj* %args46705$k40434$0)
store volatile %struct.ScmObj* %args46705$k40434$1, %struct.ScmObj** %stackaddr$prim47881, align 8
%stackaddr$prim47882 = alloca %struct.ScmObj*, align 8
%args46705$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42399, %struct.ScmObj* %args46705$k40434$1)
store volatile %struct.ScmObj* %args46705$k40434$2, %struct.ScmObj** %stackaddr$prim47882, align 8
%clofunc47883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47883(%struct.ScmObj* %k40434, %struct.ScmObj* %args46705$k40434$2)
ret void
falsebranch$cmp47880:
%ae42408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47884 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42408)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47884, align 8
%stackaddr$prim47885 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47885, align 8
%truthy$cmp47886 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40309)
%cmp$cmp47886 = icmp eq i64 %truthy$cmp47886, 1
br i1 %cmp$cmp47886, label %truebranch$cmp47886, label %falsebranch$cmp47886
truebranch$cmp47886:
%ae42412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42412)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47887, align 8
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47888, align 8
%ae42415 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42415)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47889, align 8
%stackaddr$prim47890 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47890, align 8
%ae42418 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42418, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47891, align 8
%args46706$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47892 = alloca %struct.ScmObj*, align 8
%args46706$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46706$cc40190$0)
store volatile %struct.ScmObj* %args46706$cc40190$1, %struct.ScmObj** %stackaddr$prim47892, align 8
%stackaddr$prim47893 = alloca %struct.ScmObj*, align 8
%args46706$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40434, %struct.ScmObj* %args46706$cc40190$1)
store volatile %struct.ScmObj* %args46706$cc40190$2, %struct.ScmObj** %stackaddr$prim47893, align 8
%clofunc47894 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47894(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46706$cc40190$2)
ret void
falsebranch$cmp47886:
%ae42451 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42452 = call %struct.ScmObj* @const_init_false()
%args46707$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47895 = alloca %struct.ScmObj*, align 8
%args46707$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42452, %struct.ScmObj* %args46707$k40434$0)
store volatile %struct.ScmObj* %args46707$k40434$1, %struct.ScmObj** %stackaddr$prim47895, align 8
%stackaddr$prim47896 = alloca %struct.ScmObj*, align 8
%args46707$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42451, %struct.ScmObj* %args46707$k40434$1)
store volatile %struct.ScmObj* %args46707$k40434$2, %struct.ScmObj** %stackaddr$prim47896, align 8
%clofunc47897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc47897(%struct.ScmObj* %k40434, %struct.ScmObj* %args46707$k40434$2)
ret void
}

define tailcc void @proc_clo$ae42375(%struct.ScmObj* %env$ae42375,%struct.ScmObj* %current_45args46709) {
%stackaddr$prim47898 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$prim47898, align 8
%stackaddr$prim47899 = alloca %struct.ScmObj*, align 8
%current_45args46710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46709)
store volatile %struct.ScmObj* %current_45args46710, %struct.ScmObj** %stackaddr$prim47899, align 8
%stackaddr$prim47900 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46710)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim47900, align 8
%ae42377 = call %struct.ScmObj* @const_init_int(i64 0)
%args46712$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47901 = alloca %struct.ScmObj*, align 8
%args46712$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args46712$k40437$0)
store volatile %struct.ScmObj* %args46712$k40437$1, %struct.ScmObj** %stackaddr$prim47901, align 8
%stackaddr$prim47902 = alloca %struct.ScmObj*, align 8
%args46712$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42377, %struct.ScmObj* %args46712$k40437$1)
store volatile %struct.ScmObj* %args46712$k40437$2, %struct.ScmObj** %stackaddr$prim47902, align 8
%clofunc47903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc47903(%struct.ScmObj* %k40437, %struct.ScmObj* %args46712$k40437$2)
ret void
}

define tailcc void @proc_clo$ae42298(%struct.ScmObj* %env$ae42298,%struct.ScmObj* %current_45args46715) {
%stackaddr$env-ref47904 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42298, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47904
%stackaddr$prim47905 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46715)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim47905, align 8
%stackaddr$prim47906 = alloca %struct.ScmObj*, align 8
%current_45args46716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46715)
store volatile %struct.ScmObj* %current_45args46716, %struct.ScmObj** %stackaddr$prim47906, align 8
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46716)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim47907, align 8
%stackaddr$prim47908 = alloca %struct.ScmObj*, align 8
%current_45args46717 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46716)
store volatile %struct.ScmObj* %current_45args46717, %struct.ScmObj** %stackaddr$prim47908, align 8
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46717)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$prim47910 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47910, align 8
%truthy$cmp47911 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40299)
%cmp$cmp47911 = icmp eq i64 %truthy$cmp47911, 1
br i1 %cmp$cmp47911, label %truebranch$cmp47911, label %falsebranch$cmp47911
truebranch$cmp47911:
%ae42302 = call %struct.ScmObj* @const_init_int(i64 0)
%args46719$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47912 = alloca %struct.ScmObj*, align 8
%args46719$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46719$k40438$0)
store volatile %struct.ScmObj* %args46719$k40438$1, %struct.ScmObj** %stackaddr$prim47912, align 8
%stackaddr$prim47913 = alloca %struct.ScmObj*, align 8
%args46719$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42302, %struct.ScmObj* %args46719$k40438$1)
store volatile %struct.ScmObj* %args46719$k40438$2, %struct.ScmObj** %stackaddr$prim47913, align 8
%clofunc47914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc47914(%struct.ScmObj* %k40438, %struct.ScmObj* %args46719$k40438$2)
ret void
falsebranch$cmp47911:
%stackaddr$prim47915 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47915, align 8
%ae42309 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47916 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42309)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47916, align 8
%stackaddr$prim47917 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim47917, align 8
%stackaddr$makeclosure47918 = alloca %struct.ScmObj*, align 8
%fptrToInt47919 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42312 to i64
%ae42312 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47919)
store volatile %struct.ScmObj* %ae42312, %struct.ScmObj** %stackaddr$makeclosure47918, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42312, %struct.ScmObj* %k40438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42312, %struct.ScmObj* %anf_45bind40300, i64 1)
%args46724$anf_45bind40301$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47920 = alloca %struct.ScmObj*, align 8
%args46724$anf_45bind40301$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46724$anf_45bind40301$0)
store volatile %struct.ScmObj* %args46724$anf_45bind40301$1, %struct.ScmObj** %stackaddr$prim47920, align 8
%stackaddr$prim47921 = alloca %struct.ScmObj*, align 8
%args46724$anf_45bind40301$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %args46724$anf_45bind40301$1)
store volatile %struct.ScmObj* %args46724$anf_45bind40301$2, %struct.ScmObj** %stackaddr$prim47921, align 8
%stackaddr$prim47922 = alloca %struct.ScmObj*, align 8
%args46724$anf_45bind40301$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42312, %struct.ScmObj* %args46724$anf_45bind40301$2)
store volatile %struct.ScmObj* %args46724$anf_45bind40301$3, %struct.ScmObj** %stackaddr$prim47922, align 8
%clofunc47923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40301)
musttail call tailcc void %clofunc47923(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %args46724$anf_45bind40301$3)
ret void
}

define tailcc void @proc_clo$ae42312(%struct.ScmObj* %env$ae42312,%struct.ScmObj* %current_45args46720) {
%stackaddr$env-ref47924 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42312, i64 0)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref47924
%stackaddr$env-ref47925 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42312, i64 1)
store %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$env-ref47925
%stackaddr$prim47926 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46720)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim47926, align 8
%stackaddr$prim47927 = alloca %struct.ScmObj*, align 8
%current_45args46721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46720)
store volatile %struct.ScmObj* %current_45args46721, %struct.ScmObj** %stackaddr$prim47927, align 8
%stackaddr$prim47928 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46721)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47928, align 8
%stackaddr$prim47929 = alloca %struct.ScmObj*, align 8
%cpsprim40440 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %cpsprim40440, %struct.ScmObj** %stackaddr$prim47929, align 8
%ae42318 = call %struct.ScmObj* @const_init_int(i64 0)
%args46723$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%args46723$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40440, %struct.ScmObj* %args46723$k40438$0)
store volatile %struct.ScmObj* %args46723$k40438$1, %struct.ScmObj** %stackaddr$prim47930, align 8
%stackaddr$prim47931 = alloca %struct.ScmObj*, align 8
%args46723$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42318, %struct.ScmObj* %args46723$k40438$1)
store volatile %struct.ScmObj* %args46723$k40438$2, %struct.ScmObj** %stackaddr$prim47931, align 8
%clofunc47932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc47932(%struct.ScmObj* %k40438, %struct.ScmObj* %args46723$k40438$2)
ret void
}

define tailcc void @proc_clo$ae42272(%struct.ScmObj* %env$ae42272,%struct.ScmObj* %current_45args46726) {
%stackaddr$prim47933 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46726)
store volatile %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$prim47933, align 8
%stackaddr$prim47934 = alloca %struct.ScmObj*, align 8
%current_45args46727 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46726)
store volatile %struct.ScmObj* %current_45args46727, %struct.ScmObj** %stackaddr$prim47934, align 8
%stackaddr$prim47935 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46727)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim47935, align 8
%stackaddr$prim47936 = alloca %struct.ScmObj*, align 8
%current_45args46728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46727)
store volatile %struct.ScmObj* %current_45args46728, %struct.ScmObj** %stackaddr$prim47936, align 8
%stackaddr$prim47937 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46728)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim47937, align 8
%stackaddr$prim47938 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim47938, align 8
%stackaddr$prim47939 = alloca %struct.ScmObj*, align 8
%cpsprim40442 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %cpsprim40442, %struct.ScmObj** %stackaddr$prim47939, align 8
%ae42277 = call %struct.ScmObj* @const_init_int(i64 0)
%args46730$k40441$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47940 = alloca %struct.ScmObj*, align 8
%args46730$k40441$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40442, %struct.ScmObj* %args46730$k40441$0)
store volatile %struct.ScmObj* %args46730$k40441$1, %struct.ScmObj** %stackaddr$prim47940, align 8
%stackaddr$prim47941 = alloca %struct.ScmObj*, align 8
%args46730$k40441$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42277, %struct.ScmObj* %args46730$k40441$1)
store volatile %struct.ScmObj* %args46730$k40441$2, %struct.ScmObj** %stackaddr$prim47941, align 8
%clofunc47942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40441)
musttail call tailcc void %clofunc47942(%struct.ScmObj* %k40441, %struct.ScmObj* %args46730$k40441$2)
ret void
}

define tailcc void @proc_clo$ae42248(%struct.ScmObj* %env$ae42248,%struct.ScmObj* %current_45args46732) {
%stackaddr$prim47943 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46732)
store volatile %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$prim47943, align 8
%stackaddr$prim47944 = alloca %struct.ScmObj*, align 8
%current_45args46733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46732)
store volatile %struct.ScmObj* %current_45args46733, %struct.ScmObj** %stackaddr$prim47944, align 8
%stackaddr$prim47945 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46733)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim47945, align 8
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%current_45args46734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46733)
store volatile %struct.ScmObj* %current_45args46734, %struct.ScmObj** %stackaddr$prim47946, align 8
%stackaddr$prim47947 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46734)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim47947, align 8
%stackaddr$prim47948 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47948, align 8
%stackaddr$prim47949 = alloca %struct.ScmObj*, align 8
%cpsprim40444 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsprim40444, %struct.ScmObj** %stackaddr$prim47949, align 8
%ae42253 = call %struct.ScmObj* @const_init_int(i64 0)
%args46736$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47950 = alloca %struct.ScmObj*, align 8
%args46736$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40444, %struct.ScmObj* %args46736$k40443$0)
store volatile %struct.ScmObj* %args46736$k40443$1, %struct.ScmObj** %stackaddr$prim47950, align 8
%stackaddr$prim47951 = alloca %struct.ScmObj*, align 8
%args46736$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42253, %struct.ScmObj* %args46736$k40443$1)
store volatile %struct.ScmObj* %args46736$k40443$2, %struct.ScmObj** %stackaddr$prim47951, align 8
%clofunc47952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc47952(%struct.ScmObj* %k40443, %struct.ScmObj* %args46736$k40443$2)
ret void
}

define tailcc void @proc_clo$ae41854(%struct.ScmObj* %env$ae41854,%struct.ScmObj* %current_45args46739) {
%stackaddr$env-ref47953 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41854, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47953
%stackaddr$env-ref47954 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41854, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47954
%stackaddr$env-ref47955 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41854, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47955
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46739)
store volatile %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%current_45args46740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46739)
store volatile %struct.ScmObj* %current_45args46740, %struct.ScmObj** %stackaddr$prim47957, align 8
%stackaddr$prim47958 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46740)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim47958, align 8
%ae41856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47959 = alloca %struct.ScmObj*, align 8
%fptrToInt47960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41857 to i64
%ae41857 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47960)
store volatile %struct.ScmObj* %ae41857, %struct.ScmObj** %stackaddr$makeclosure47959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41857, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41857, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41857, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41857, %struct.ScmObj* %_37map140154, i64 3)
%args46797$k40445$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47961 = alloca %struct.ScmObj*, align 8
%args46797$k40445$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41857, %struct.ScmObj* %args46797$k40445$0)
store volatile %struct.ScmObj* %args46797$k40445$1, %struct.ScmObj** %stackaddr$prim47961, align 8
%stackaddr$prim47962 = alloca %struct.ScmObj*, align 8
%args46797$k40445$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41856, %struct.ScmObj* %args46797$k40445$1)
store volatile %struct.ScmObj* %args46797$k40445$2, %struct.ScmObj** %stackaddr$prim47962, align 8
%clofunc47963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40445)
musttail call tailcc void %clofunc47963(%struct.ScmObj* %k40445, %struct.ScmObj* %args46797$k40445$2)
ret void
}

define tailcc void @proc_clo$ae41857(%struct.ScmObj* %env$ae41857,%struct.ScmObj* %args4020740446) {
%stackaddr$env-ref47964 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41857, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47964
%stackaddr$env-ref47965 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41857, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47965
%stackaddr$env-ref47966 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41857, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47966
%stackaddr$env-ref47967 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41857, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47967
%stackaddr$prim47968 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740446)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim47968, align 8
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740446)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim47969, align 8
%stackaddr$prim47970 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim47970, align 8
%stackaddr$prim47971 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim47971, align 8
%stackaddr$prim47972 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim47972, align 8
%stackaddr$prim47973 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim47973, align 8
%stackaddr$prim47974 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim47974, align 8
%stackaddr$makeclosure47975 = alloca %struct.ScmObj*, align 8
%fptrToInt47976 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41865 to i64
%ae41865 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47976)
store volatile %struct.ScmObj* %ae41865, %struct.ScmObj** %stackaddr$makeclosure47975, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %k40447, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41865, %struct.ScmObj* %_37map140154, i64 7)
%ae41866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47977 = alloca %struct.ScmObj*, align 8
%fptrToInt47978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41867 to i64
%ae41867 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47978)
store volatile %struct.ScmObj* %ae41867, %struct.ScmObj** %stackaddr$makeclosure47977, align 8
%args46796$ae41865$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%args46796$ae41865$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41867, %struct.ScmObj* %args46796$ae41865$0)
store volatile %struct.ScmObj* %args46796$ae41865$1, %struct.ScmObj** %stackaddr$prim47979, align 8
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%args46796$ae41865$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41866, %struct.ScmObj* %args46796$ae41865$1)
store volatile %struct.ScmObj* %args46796$ae41865$2, %struct.ScmObj** %stackaddr$prim47980, align 8
%clofunc47981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41865)
musttail call tailcc void %clofunc47981(%struct.ScmObj* %ae41865, %struct.ScmObj* %args46796$ae41865$2)
ret void
}

define tailcc void @proc_clo$ae41865(%struct.ScmObj* %env$ae41865,%struct.ScmObj* %current_45args46742) {
%stackaddr$env-ref47982 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47982
%stackaddr$env-ref47983 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47983
%stackaddr$env-ref47984 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47984
%stackaddr$env-ref47985 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47985
%stackaddr$env-ref47986 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 4)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref47986
%stackaddr$env-ref47987 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47987
%stackaddr$env-ref47988 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47988
%stackaddr$env-ref47989 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41865, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47989
%stackaddr$prim47990 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46742)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim47990, align 8
%stackaddr$prim47991 = alloca %struct.ScmObj*, align 8
%current_45args46743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46742)
store volatile %struct.ScmObj* %current_45args46743, %struct.ScmObj** %stackaddr$prim47991, align 8
%stackaddr$prim47992 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46743)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim47992, align 8
%stackaddr$makeclosure47993 = alloca %struct.ScmObj*, align 8
%fptrToInt47994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41897 to i64
%ae41897 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47994)
store volatile %struct.ScmObj* %ae41897, %struct.ScmObj** %stackaddr$makeclosure47993, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %k40447, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41897, %struct.ScmObj* %_37map140154, i64 6)
%ae41899 = call %struct.ScmObj* @const_init_false()
%args46789$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47995 = alloca %struct.ScmObj*, align 8
%args46789$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46789$_37foldr140123$0)
store volatile %struct.ScmObj* %args46789$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim47995, align 8
%stackaddr$prim47996 = alloca %struct.ScmObj*, align 8
%args46789$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41899, %struct.ScmObj* %args46789$_37foldr140123$1)
store volatile %struct.ScmObj* %args46789$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim47996, align 8
%stackaddr$prim47997 = alloca %struct.ScmObj*, align 8
%args46789$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args46789$_37foldr140123$2)
store volatile %struct.ScmObj* %args46789$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim47997, align 8
%stackaddr$prim47998 = alloca %struct.ScmObj*, align 8
%args46789$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41897, %struct.ScmObj* %args46789$_37foldr140123$3)
store volatile %struct.ScmObj* %args46789$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim47998, align 8
%clofunc47999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc47999(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46789$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41897(%struct.ScmObj* %env$ae41897,%struct.ScmObj* %current_45args46745) {
%stackaddr$env-ref48000 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48000
%stackaddr$env-ref48001 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48001
%stackaddr$env-ref48002 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48002
%stackaddr$env-ref48003 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48003
%stackaddr$env-ref48004 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 4)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48004
%stackaddr$env-ref48005 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48005
%stackaddr$env-ref48006 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41897, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48006
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46745)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim48007, align 8
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%current_45args46746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46745)
store volatile %struct.ScmObj* %current_45args46746, %struct.ScmObj** %stackaddr$prim48008, align 8
%stackaddr$prim48009 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46746)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48009, align 8
%truthy$cmp48010 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40288)
%cmp$cmp48010 = icmp eq i64 %truthy$cmp48010, 1
br i1 %cmp$cmp48010, label %truebranch$cmp48010, label %falsebranch$cmp48010
truebranch$cmp48010:
%ae41908 = call %struct.ScmObj* @const_init_int(i64 0)
%args46748$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%args46748$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args46748$k40447$0)
store volatile %struct.ScmObj* %args46748$k40447$1, %struct.ScmObj** %stackaddr$prim48011, align 8
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%args46748$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41908, %struct.ScmObj* %args46748$k40447$1)
store volatile %struct.ScmObj* %args46748$k40447$2, %struct.ScmObj** %stackaddr$prim48012, align 8
%clofunc48013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc48013(%struct.ScmObj* %k40447, %struct.ScmObj* %args46748$k40447$2)
ret void
falsebranch$cmp48010:
%stackaddr$makeclosure48014 = alloca %struct.ScmObj*, align 8
%fptrToInt48015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41913 to i64
%ae41913 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48015)
store volatile %struct.ScmObj* %ae41913, %struct.ScmObj** %stackaddr$makeclosure48014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %k40447, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41913, %struct.ScmObj* %_37map140154, i64 6)
%ae41914 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48016 = alloca %struct.ScmObj*, align 8
%fptrToInt48017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41915 to i64
%ae41915 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48017)
store volatile %struct.ScmObj* %ae41915, %struct.ScmObj** %stackaddr$makeclosure48016, align 8
%args46788$ae41913$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48018 = alloca %struct.ScmObj*, align 8
%args46788$ae41913$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41915, %struct.ScmObj* %args46788$ae41913$0)
store volatile %struct.ScmObj* %args46788$ae41913$1, %struct.ScmObj** %stackaddr$prim48018, align 8
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%args46788$ae41913$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41914, %struct.ScmObj* %args46788$ae41913$1)
store volatile %struct.ScmObj* %args46788$ae41913$2, %struct.ScmObj** %stackaddr$prim48019, align 8
%clofunc48020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41913)
musttail call tailcc void %clofunc48020(%struct.ScmObj* %ae41913, %struct.ScmObj* %args46788$ae41913$2)
ret void
}

define tailcc void @proc_clo$ae41913(%struct.ScmObj* %env$ae41913,%struct.ScmObj* %current_45args46749) {
%stackaddr$env-ref48021 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48021
%stackaddr$env-ref48022 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48022
%stackaddr$env-ref48023 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48023
%stackaddr$env-ref48024 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48024
%stackaddr$env-ref48025 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 4)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48025
%stackaddr$env-ref48026 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48026
%stackaddr$env-ref48027 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41913, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48027
%stackaddr$prim48028 = alloca %struct.ScmObj*, align 8
%_95k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %_95k40450, %struct.ScmObj** %stackaddr$prim48028, align 8
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%current_45args46750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46749)
store volatile %struct.ScmObj* %current_45args46750, %struct.ScmObj** %stackaddr$prim48029, align 8
%stackaddr$prim48030 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46750)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48030, align 8
%stackaddr$makeclosure48031 = alloca %struct.ScmObj*, align 8
%fptrToInt48032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41934 to i64
%ae41934 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48032)
store volatile %struct.ScmObj* %ae41934, %struct.ScmObj** %stackaddr$makeclosure48031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %k40447, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41934, %struct.ScmObj* %_37map140154, i64 6)
%args46783$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%args46783$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46783$_37map140154$0)
store volatile %struct.ScmObj* %args46783$_37map140154$1, %struct.ScmObj** %stackaddr$prim48033, align 8
%stackaddr$prim48034 = alloca %struct.ScmObj*, align 8
%args46783$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args46783$_37map140154$1)
store volatile %struct.ScmObj* %args46783$_37map140154$2, %struct.ScmObj** %stackaddr$prim48034, align 8
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%args46783$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41934, %struct.ScmObj* %args46783$_37map140154$2)
store volatile %struct.ScmObj* %args46783$_37map140154$3, %struct.ScmObj** %stackaddr$prim48035, align 8
%clofunc48036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48036(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46783$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41934(%struct.ScmObj* %env$ae41934,%struct.ScmObj* %current_45args46752) {
%stackaddr$env-ref48037 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48037
%stackaddr$env-ref48038 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48038
%stackaddr$env-ref48039 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48039
%stackaddr$env-ref48040 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48040
%stackaddr$env-ref48041 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 4)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48041
%stackaddr$env-ref48042 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48042
%stackaddr$env-ref48043 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41934, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48043
%stackaddr$prim48044 = alloca %struct.ScmObj*, align 8
%_95k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46752)
store volatile %struct.ScmObj* %_95k40451, %struct.ScmObj** %stackaddr$prim48044, align 8
%stackaddr$prim48045 = alloca %struct.ScmObj*, align 8
%current_45args46753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46752)
store volatile %struct.ScmObj* %current_45args46753, %struct.ScmObj** %stackaddr$prim48045, align 8
%stackaddr$prim48046 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46753)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim48046, align 8
%stackaddr$makeclosure48047 = alloca %struct.ScmObj*, align 8
%fptrToInt48048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41937 to i64
%ae41937 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48048)
store volatile %struct.ScmObj* %ae41937, %struct.ScmObj** %stackaddr$makeclosure48047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %k40447, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %_37foldl40206, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41937, %struct.ScmObj* %_37map140154, i64 7)
%ae41938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48049 = alloca %struct.ScmObj*, align 8
%fptrToInt48050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41939 to i64
%ae41939 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48050)
store volatile %struct.ScmObj* %ae41939, %struct.ScmObj** %stackaddr$makeclosure48049, align 8
%args46782$ae41937$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48051 = alloca %struct.ScmObj*, align 8
%args46782$ae41937$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41939, %struct.ScmObj* %args46782$ae41937$0)
store volatile %struct.ScmObj* %args46782$ae41937$1, %struct.ScmObj** %stackaddr$prim48051, align 8
%stackaddr$prim48052 = alloca %struct.ScmObj*, align 8
%args46782$ae41937$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41938, %struct.ScmObj* %args46782$ae41937$1)
store volatile %struct.ScmObj* %args46782$ae41937$2, %struct.ScmObj** %stackaddr$prim48052, align 8
%clofunc48053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41937)
musttail call tailcc void %clofunc48053(%struct.ScmObj* %ae41937, %struct.ScmObj* %args46782$ae41937$2)
ret void
}

define tailcc void @proc_clo$ae41937(%struct.ScmObj* %env$ae41937,%struct.ScmObj* %current_45args46755) {
%stackaddr$env-ref48054 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48054
%stackaddr$env-ref48055 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48055
%stackaddr$env-ref48056 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48056
%stackaddr$env-ref48057 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48057
%stackaddr$env-ref48058 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48058
%stackaddr$env-ref48059 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 5)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48059
%stackaddr$env-ref48060 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48060
%stackaddr$env-ref48061 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41937, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48061
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%_95k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %_95k40452, %struct.ScmObj** %stackaddr$prim48062, align 8
%stackaddr$prim48063 = alloca %struct.ScmObj*, align 8
%current_45args46756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %current_45args46756, %struct.ScmObj** %stackaddr$prim48063, align 8
%stackaddr$prim48064 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46756)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48064, align 8
%stackaddr$makeclosure48065 = alloca %struct.ScmObj*, align 8
%fptrToInt48066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41958 to i64
%ae41958 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48066)
store volatile %struct.ScmObj* %ae41958, %struct.ScmObj** %stackaddr$makeclosure48065, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %acc40209, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %k40447, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41958, %struct.ScmObj* %_37foldl40206, i64 5)
%args46777$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48067 = alloca %struct.ScmObj*, align 8
%args46777$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46777$_37map140154$0)
store volatile %struct.ScmObj* %args46777$_37map140154$1, %struct.ScmObj** %stackaddr$prim48067, align 8
%stackaddr$prim48068 = alloca %struct.ScmObj*, align 8
%args46777$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46777$_37map140154$1)
store volatile %struct.ScmObj* %args46777$_37map140154$2, %struct.ScmObj** %stackaddr$prim48068, align 8
%stackaddr$prim48069 = alloca %struct.ScmObj*, align 8
%args46777$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41958, %struct.ScmObj* %args46777$_37map140154$2)
store volatile %struct.ScmObj* %args46777$_37map140154$3, %struct.ScmObj** %stackaddr$prim48069, align 8
%clofunc48070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48070(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46777$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41958(%struct.ScmObj* %env$ae41958,%struct.ScmObj* %current_45args46758) {
%stackaddr$env-ref48071 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48071
%stackaddr$env-ref48072 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48072
%stackaddr$env-ref48073 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 2)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48073
%stackaddr$env-ref48074 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48074
%stackaddr$env-ref48075 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 4)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48075
%stackaddr$env-ref48076 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41958, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48076
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%_95k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %_95k40453, %struct.ScmObj** %stackaddr$prim48077, align 8
%stackaddr$prim48078 = alloca %struct.ScmObj*, align 8
%current_45args46759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46758)
store volatile %struct.ScmObj* %current_45args46759, %struct.ScmObj** %stackaddr$prim48078, align 8
%stackaddr$prim48079 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46759)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim48079, align 8
%stackaddr$makeclosure48080 = alloca %struct.ScmObj*, align 8
%fptrToInt48081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41961 to i64
%ae41961 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48081)
store volatile %struct.ScmObj* %ae41961, %struct.ScmObj** %stackaddr$makeclosure48080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %vs40213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %f40210, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %acc40209, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %k40447, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41961, %struct.ScmObj* %_37foldl40206, i64 6)
%ae41962 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48082 = alloca %struct.ScmObj*, align 8
%fptrToInt48083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41963 to i64
%ae41963 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48083)
store volatile %struct.ScmObj* %ae41963, %struct.ScmObj** %stackaddr$makeclosure48082, align 8
%args46776$ae41961$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48084 = alloca %struct.ScmObj*, align 8
%args46776$ae41961$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41963, %struct.ScmObj* %args46776$ae41961$0)
store volatile %struct.ScmObj* %args46776$ae41961$1, %struct.ScmObj** %stackaddr$prim48084, align 8
%stackaddr$prim48085 = alloca %struct.ScmObj*, align 8
%args46776$ae41961$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41962, %struct.ScmObj* %args46776$ae41961$1)
store volatile %struct.ScmObj* %args46776$ae41961$2, %struct.ScmObj** %stackaddr$prim48085, align 8
%clofunc48086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41961)
musttail call tailcc void %clofunc48086(%struct.ScmObj* %ae41961, %struct.ScmObj* %args46776$ae41961$2)
ret void
}

define tailcc void @proc_clo$ae41961(%struct.ScmObj* %env$ae41961,%struct.ScmObj* %current_45args46761) {
%stackaddr$env-ref48087 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48087
%stackaddr$env-ref48088 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 1)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref48088
%stackaddr$env-ref48089 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 2)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48089
%stackaddr$env-ref48090 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 3)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48090
%stackaddr$env-ref48091 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48091
%stackaddr$env-ref48092 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 5)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48092
%stackaddr$env-ref48093 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41961, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48093
%stackaddr$prim48094 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim48094, align 8
%stackaddr$prim48095 = alloca %struct.ScmObj*, align 8
%current_45args46762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46761)
store volatile %struct.ScmObj* %current_45args46762, %struct.ScmObj** %stackaddr$prim48095, align 8
%stackaddr$prim48096 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46762)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim48096, align 8
%ae41984 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48097 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae41984)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim48097, align 8
%stackaddr$makeclosure48098 = alloca %struct.ScmObj*, align 8
%fptrToInt48099 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41986 to i64
%ae41986 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48099)
store volatile %struct.ScmObj* %ae41986, %struct.ScmObj** %stackaddr$makeclosure48098, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %k40447, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37foldl40206, i64 3)
%args46770$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48100 = alloca %struct.ScmObj*, align 8
%args46770$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args46770$_37foldr40128$0)
store volatile %struct.ScmObj* %args46770$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim48100, align 8
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%args46770$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args46770$_37foldr40128$1)
store volatile %struct.ScmObj* %args46770$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim48101, align 8
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%args46770$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args46770$_37foldr40128$2)
store volatile %struct.ScmObj* %args46770$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim48102, align 8
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%args46770$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41986, %struct.ScmObj* %args46770$_37foldr40128$3)
store volatile %struct.ScmObj* %args46770$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim48103, align 8
%clofunc48104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48104(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args46770$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae41986(%struct.ScmObj* %env$ae41986,%struct.ScmObj* %current_45args46764) {
%stackaddr$env-ref48105 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48105
%stackaddr$env-ref48106 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48106
%stackaddr$env-ref48107 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 2)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48107
%stackaddr$env-ref48108 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48108
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%_95k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %_95k40455, %struct.ScmObj** %stackaddr$prim48109, align 8
%stackaddr$prim48110 = alloca %struct.ScmObj*, align 8
%current_45args46765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %current_45args46765, %struct.ScmObj** %stackaddr$prim48110, align 8
%stackaddr$prim48111 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46765)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim48111, align 8
%stackaddr$makeclosure48112 = alloca %struct.ScmObj*, align 8
%fptrToInt48113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41990 to i64
%ae41990 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48113)
store volatile %struct.ScmObj* %ae41990, %struct.ScmObj** %stackaddr$makeclosure48112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41990, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41990, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41990, %struct.ScmObj* %k40447, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41990, %struct.ScmObj* %_37foldl40206, i64 3)
%stackaddr$prim48114 = alloca %struct.ScmObj*, align 8
%cpsargs40458 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41990, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %cpsargs40458, %struct.ScmObj** %stackaddr$prim48114, align 8
%clofunc48115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc48115(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40458)
ret void
}

define tailcc void @proc_clo$ae41990(%struct.ScmObj* %env$ae41990,%struct.ScmObj* %current_45args46767) {
%stackaddr$env-ref48116 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41990, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48116
%stackaddr$env-ref48117 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41990, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48117
%stackaddr$env-ref48118 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41990, i64 2)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48118
%stackaddr$env-ref48119 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41990, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48119
%stackaddr$prim48120 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46767)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim48120, align 8
%stackaddr$prim48121 = alloca %struct.ScmObj*, align 8
%current_45args46768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46767)
store volatile %struct.ScmObj* %current_45args46768, %struct.ScmObj** %stackaddr$prim48121, align 8
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46768)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim48123, align 8
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim48124, align 8
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%cpsargs40457 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40447, %struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %cpsargs40457, %struct.ScmObj** %stackaddr$prim48125, align 8
%clofunc48126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48126(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40457)
ret void
}

define tailcc void @proc_clo$ae41963(%struct.ScmObj* %env$ae41963,%struct.ScmObj* %current_45args46771) {
%stackaddr$prim48127 = alloca %struct.ScmObj*, align 8
%k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46771)
store volatile %struct.ScmObj* %k40459, %struct.ScmObj** %stackaddr$prim48127, align 8
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%current_45args46772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46771)
store volatile %struct.ScmObj* %current_45args46772, %struct.ScmObj** %stackaddr$prim48128, align 8
%stackaddr$prim48129 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46772)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48129, align 8
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%current_45args46773 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46772)
store volatile %struct.ScmObj* %current_45args46773, %struct.ScmObj** %stackaddr$prim48130, align 8
%stackaddr$prim48131 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46773)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48131, align 8
%stackaddr$prim48132 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim48132, align 8
%ae41967 = call %struct.ScmObj* @const_init_int(i64 0)
%args46775$k40459$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48133 = alloca %struct.ScmObj*, align 8
%args46775$k40459$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args46775$k40459$0)
store volatile %struct.ScmObj* %args46775$k40459$1, %struct.ScmObj** %stackaddr$prim48133, align 8
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%args46775$k40459$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41967, %struct.ScmObj* %args46775$k40459$1)
store volatile %struct.ScmObj* %args46775$k40459$2, %struct.ScmObj** %stackaddr$prim48134, align 8
%clofunc48135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40459)
musttail call tailcc void %clofunc48135(%struct.ScmObj* %k40459, %struct.ScmObj* %args46775$k40459$2)
ret void
}

define tailcc void @proc_clo$ae41939(%struct.ScmObj* %env$ae41939,%struct.ScmObj* %current_45args46778) {
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46778)
store volatile %struct.ScmObj* %k40461, %struct.ScmObj** %stackaddr$prim48136, align 8
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%current_45args46779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46778)
store volatile %struct.ScmObj* %current_45args46779, %struct.ScmObj** %stackaddr$prim48137, align 8
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46779)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48138, align 8
%stackaddr$prim48139 = alloca %struct.ScmObj*, align 8
%cpsprim40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40462, %struct.ScmObj** %stackaddr$prim48139, align 8
%ae41942 = call %struct.ScmObj* @const_init_int(i64 0)
%args46781$k40461$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48140 = alloca %struct.ScmObj*, align 8
%args46781$k40461$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40462, %struct.ScmObj* %args46781$k40461$0)
store volatile %struct.ScmObj* %args46781$k40461$1, %struct.ScmObj** %stackaddr$prim48140, align 8
%stackaddr$prim48141 = alloca %struct.ScmObj*, align 8
%args46781$k40461$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41942, %struct.ScmObj* %args46781$k40461$1)
store volatile %struct.ScmObj* %args46781$k40461$2, %struct.ScmObj** %stackaddr$prim48141, align 8
%clofunc48142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40461)
musttail call tailcc void %clofunc48142(%struct.ScmObj* %k40461, %struct.ScmObj* %args46781$k40461$2)
ret void
}

define tailcc void @proc_clo$ae41915(%struct.ScmObj* %env$ae41915,%struct.ScmObj* %current_45args46784) {
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46784)
store volatile %struct.ScmObj* %k40463, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%current_45args46785 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46784)
store volatile %struct.ScmObj* %current_45args46785, %struct.ScmObj** %stackaddr$prim48144, align 8
%stackaddr$prim48145 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46785)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48145, align 8
%stackaddr$prim48146 = alloca %struct.ScmObj*, align 8
%cpsprim40464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40464, %struct.ScmObj** %stackaddr$prim48146, align 8
%ae41918 = call %struct.ScmObj* @const_init_int(i64 0)
%args46787$k40463$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%args46787$k40463$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40464, %struct.ScmObj* %args46787$k40463$0)
store volatile %struct.ScmObj* %args46787$k40463$1, %struct.ScmObj** %stackaddr$prim48147, align 8
%stackaddr$prim48148 = alloca %struct.ScmObj*, align 8
%args46787$k40463$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41918, %struct.ScmObj* %args46787$k40463$1)
store volatile %struct.ScmObj* %args46787$k40463$2, %struct.ScmObj** %stackaddr$prim48148, align 8
%clofunc48149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40463)
musttail call tailcc void %clofunc48149(%struct.ScmObj* %k40463, %struct.ScmObj* %args46787$k40463$2)
ret void
}

define tailcc void @proc_clo$ae41867(%struct.ScmObj* %env$ae41867,%struct.ScmObj* %current_45args46790) {
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46790)
store volatile %struct.ScmObj* %k40465, %struct.ScmObj** %stackaddr$prim48150, align 8
%stackaddr$prim48151 = alloca %struct.ScmObj*, align 8
%current_45args46791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46790)
store volatile %struct.ScmObj* %current_45args46791, %struct.ScmObj** %stackaddr$prim48151, align 8
%stackaddr$prim48152 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46791)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48152, align 8
%stackaddr$prim48153 = alloca %struct.ScmObj*, align 8
%current_45args46792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46791)
store volatile %struct.ScmObj* %current_45args46792, %struct.ScmObj** %stackaddr$prim48153, align 8
%stackaddr$prim48154 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46792)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48154, align 8
%truthy$cmp48155 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48155 = icmp eq i64 %truthy$cmp48155, 1
br i1 %cmp$cmp48155, label %truebranch$cmp48155, label %falsebranch$cmp48155
truebranch$cmp48155:
%ae41870 = call %struct.ScmObj* @const_init_int(i64 0)
%args46794$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%args46794$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args46794$k40465$0)
store volatile %struct.ScmObj* %args46794$k40465$1, %struct.ScmObj** %stackaddr$prim48156, align 8
%stackaddr$prim48157 = alloca %struct.ScmObj*, align 8
%args46794$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41870, %struct.ScmObj* %args46794$k40465$1)
store volatile %struct.ScmObj* %args46794$k40465$2, %struct.ScmObj** %stackaddr$prim48157, align 8
%clofunc48158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc48158(%struct.ScmObj* %k40465, %struct.ScmObj* %args46794$k40465$2)
ret void
falsebranch$cmp48155:
%stackaddr$prim48159 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim48159, align 8
%ae41877 = call %struct.ScmObj* @const_init_int(i64 0)
%args46795$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48160 = alloca %struct.ScmObj*, align 8
%args46795$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args46795$k40465$0)
store volatile %struct.ScmObj* %args46795$k40465$1, %struct.ScmObj** %stackaddr$prim48160, align 8
%stackaddr$prim48161 = alloca %struct.ScmObj*, align 8
%args46795$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41877, %struct.ScmObj* %args46795$k40465$1)
store volatile %struct.ScmObj* %args46795$k40465$2, %struct.ScmObj** %stackaddr$prim48161, align 8
%clofunc48162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc48162(%struct.ScmObj* %k40465, %struct.ScmObj* %args46795$k40465$2)
ret void
}

define tailcc void @proc_clo$ae41708(%struct.ScmObj* %env$ae41708,%struct.ScmObj* %args4015040467) {
%stackaddr$env-ref48163 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41708, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48163
%stackaddr$env-ref48164 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41708, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48164
%stackaddr$env-ref48165 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41708, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48165
%stackaddr$prim48166 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040467)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim48166, align 8
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040467)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$makeclosure48170 = alloca %struct.ScmObj*, align 8
%fptrToInt48171 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41713 to i64
%ae41713 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48171)
store volatile %struct.ScmObj* %ae41713, %struct.ScmObj** %stackaddr$makeclosure48170, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41713, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41713, %struct.ScmObj* %k40468, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41713, %struct.ScmObj* %_37foldr40128, i64 2)
%ae41714 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48172 = alloca %struct.ScmObj*, align 8
%fptrToInt48173 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41715 to i64
%ae41715 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48173)
store volatile %struct.ScmObj* %ae41715, %struct.ScmObj** %stackaddr$makeclosure48172, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %f40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37last40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41715, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args46814$ae41713$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48174 = alloca %struct.ScmObj*, align 8
%args46814$ae41713$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41715, %struct.ScmObj* %args46814$ae41713$0)
store volatile %struct.ScmObj* %args46814$ae41713$1, %struct.ScmObj** %stackaddr$prim48174, align 8
%stackaddr$prim48175 = alloca %struct.ScmObj*, align 8
%args46814$ae41713$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41714, %struct.ScmObj* %args46814$ae41713$1)
store volatile %struct.ScmObj* %args46814$ae41713$2, %struct.ScmObj** %stackaddr$prim48175, align 8
%clofunc48176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41713)
musttail call tailcc void %clofunc48176(%struct.ScmObj* %ae41713, %struct.ScmObj* %args46814$ae41713$2)
ret void
}

define tailcc void @proc_clo$ae41713(%struct.ScmObj* %env$ae41713,%struct.ScmObj* %current_45args46799) {
%stackaddr$env-ref48177 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41713, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref48177
%stackaddr$env-ref48178 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41713, i64 1)
store %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$env-ref48178
%stackaddr$env-ref48179 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41713, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48179
%stackaddr$prim48180 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46799)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim48180, align 8
%stackaddr$prim48181 = alloca %struct.ScmObj*, align 8
%current_45args46800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46799)
store volatile %struct.ScmObj* %current_45args46800, %struct.ScmObj** %stackaddr$prim48181, align 8
%stackaddr$prim48182 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim48182, align 8
%ae41776 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41776, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim48183, align 8
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %anf_45bind40283)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim48184, align 8
%stackaddr$prim48185 = alloca %struct.ScmObj*, align 8
%cpsargs40470 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40468, %struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %cpsargs40470, %struct.ScmObj** %stackaddr$prim48185, align 8
%clofunc48186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48186(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40470)
ret void
}

define tailcc void @proc_clo$ae41715(%struct.ScmObj* %env$ae41715,%struct.ScmObj* %fargs4015340471) {
%stackaddr$env-ref48187 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 0)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48187
%stackaddr$env-ref48188 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 1)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48188
%stackaddr$env-ref48189 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41715, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48189
%stackaddr$prim48190 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340471)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim48190, align 8
%stackaddr$prim48191 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340471)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim48191, align 8
%stackaddr$makeclosure48192 = alloca %struct.ScmObj*, align 8
%fptrToInt48193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41719 to i64
%ae41719 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48193)
store volatile %struct.ScmObj* %ae41719, %struct.ScmObj** %stackaddr$makeclosure48192, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %k40472, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %f40152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %fargs40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37last40145, i64 3)
%ae41721 = call %struct.ScmObj* @const_init_int(i64 1)
%args46813$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48194 = alloca %struct.ScmObj*, align 8
%args46813$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41721, %struct.ScmObj* %args46813$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args46813$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim48194, align 8
%stackaddr$prim48195 = alloca %struct.ScmObj*, align 8
%args46813$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46813$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args46813$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim48195, align 8
%stackaddr$prim48196 = alloca %struct.ScmObj*, align 8
%args46813$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41719, %struct.ScmObj* %args46813$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args46813$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim48196, align 8
%clofunc48197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc48197(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args46813$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41719(%struct.ScmObj* %env$ae41719,%struct.ScmObj* %current_45args46802) {
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 0)
store %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$env-ref48199 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 1)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48199
%stackaddr$env-ref48200 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48200
%stackaddr$env-ref48201 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48201
%stackaddr$prim48202 = alloca %struct.ScmObj*, align 8
%_95k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46802)
store volatile %struct.ScmObj* %_95k40473, %struct.ScmObj** %stackaddr$prim48202, align 8
%stackaddr$prim48203 = alloca %struct.ScmObj*, align 8
%current_45args46803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46802)
store volatile %struct.ScmObj* %current_45args46803, %struct.ScmObj** %stackaddr$prim48203, align 8
%stackaddr$prim48204 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim48204, align 8
%stackaddr$makeclosure48205 = alloca %struct.ScmObj*, align 8
%fptrToInt48206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41726 to i64
%ae41726 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48206)
store volatile %struct.ScmObj* %ae41726, %struct.ScmObj** %stackaddr$makeclosure48205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %fargs40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %k40472, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41726, %struct.ScmObj* %_37last40145, i64 2)
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%cpsargs40477 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41726, %struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %cpsargs40477, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40477)
ret void
}

define tailcc void @proc_clo$ae41726(%struct.ScmObj* %env$ae41726,%struct.ScmObj* %current_45args46805) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 0)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$env-ref48210 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 1)
store %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$env-ref48210
%stackaddr$env-ref48211 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41726, i64 2)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48211
%stackaddr$prim48212 = alloca %struct.ScmObj*, align 8
%_95k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46805)
store volatile %struct.ScmObj* %_95k40474, %struct.ScmObj** %stackaddr$prim48212, align 8
%stackaddr$prim48213 = alloca %struct.ScmObj*, align 8
%current_45args46806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46805)
store volatile %struct.ScmObj* %current_45args46806, %struct.ScmObj** %stackaddr$prim48213, align 8
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46806)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim48214, align 8
%stackaddr$makeclosure48215 = alloca %struct.ScmObj*, align 8
%fptrToInt48216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41731 to i64
%ae41731 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48216)
store volatile %struct.ScmObj* %ae41731, %struct.ScmObj** %stackaddr$makeclosure48215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41731, %struct.ScmObj* %anf_45bind40280, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41731, %struct.ScmObj* %k40472, i64 1)
%args46812$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48217 = alloca %struct.ScmObj*, align 8
%args46812$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46812$_37last40145$0)
store volatile %struct.ScmObj* %args46812$_37last40145$1, %struct.ScmObj** %stackaddr$prim48217, align 8
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%args46812$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41731, %struct.ScmObj* %args46812$_37last40145$1)
store volatile %struct.ScmObj* %args46812$_37last40145$2, %struct.ScmObj** %stackaddr$prim48218, align 8
%clofunc48219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc48219(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args46812$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41731(%struct.ScmObj* %env$ae41731,%struct.ScmObj* %current_45args46808) {
%stackaddr$env-ref48220 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41731, i64 0)
store %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$env-ref48220
%stackaddr$env-ref48221 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41731, i64 1)
store %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$env-ref48221
%stackaddr$prim48222 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46808)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim48222, align 8
%stackaddr$prim48223 = alloca %struct.ScmObj*, align 8
%current_45args46809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46808)
store volatile %struct.ScmObj* %current_45args46809, %struct.ScmObj** %stackaddr$prim48223, align 8
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim48224, align 8
%stackaddr$prim48225 = alloca %struct.ScmObj*, align 8
%cpsprim40476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %anf_45bind40281)
store volatile %struct.ScmObj* %cpsprim40476, %struct.ScmObj** %stackaddr$prim48225, align 8
%ae41736 = call %struct.ScmObj* @const_init_int(i64 0)
%args46811$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48226 = alloca %struct.ScmObj*, align 8
%args46811$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40476, %struct.ScmObj* %args46811$k40472$0)
store volatile %struct.ScmObj* %args46811$k40472$1, %struct.ScmObj** %stackaddr$prim48226, align 8
%stackaddr$prim48227 = alloca %struct.ScmObj*, align 8
%args46811$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41736, %struct.ScmObj* %args46811$k40472$1)
store volatile %struct.ScmObj* %args46811$k40472$2, %struct.ScmObj** %stackaddr$prim48227, align 8
%clofunc48228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc48228(%struct.ScmObj* %k40472, %struct.ScmObj* %args46811$k40472$2)
ret void
}

define tailcc void @proc_clo$ae41631(%struct.ScmObj* %env$ae41631,%struct.ScmObj* %current_45args46816) {
%stackaddr$env-ref48229 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41631, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48229
%stackaddr$prim48230 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim48230, align 8
%stackaddr$prim48231 = alloca %struct.ScmObj*, align 8
%current_45args46817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46816)
store volatile %struct.ScmObj* %current_45args46817, %struct.ScmObj** %stackaddr$prim48231, align 8
%stackaddr$prim48232 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46817)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim48232, align 8
%stackaddr$prim48233 = alloca %struct.ScmObj*, align 8
%current_45args46818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46817)
store volatile %struct.ScmObj* %current_45args46818, %struct.ScmObj** %stackaddr$prim48233, align 8
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46818)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$makeclosure48235 = alloca %struct.ScmObj*, align 8
%fptrToInt48236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41632 to i64
%ae41632 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48236)
store volatile %struct.ScmObj* %ae41632, %struct.ScmObj** %stackaddr$makeclosure48235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41632, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41632, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41632, %struct.ScmObj* %k40478, i64 2)
%ae41633 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48237 = alloca %struct.ScmObj*, align 8
%fptrToInt48238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41634 to i64
%ae41634 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48238)
store volatile %struct.ScmObj* %ae41634, %struct.ScmObj** %stackaddr$makeclosure48237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41634, %struct.ScmObj* %f40156, i64 0)
%args46833$ae41632$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48239 = alloca %struct.ScmObj*, align 8
%args46833$ae41632$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41634, %struct.ScmObj* %args46833$ae41632$0)
store volatile %struct.ScmObj* %args46833$ae41632$1, %struct.ScmObj** %stackaddr$prim48239, align 8
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%args46833$ae41632$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41633, %struct.ScmObj* %args46833$ae41632$1)
store volatile %struct.ScmObj* %args46833$ae41632$2, %struct.ScmObj** %stackaddr$prim48240, align 8
%clofunc48241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41632)
musttail call tailcc void %clofunc48241(%struct.ScmObj* %ae41632, %struct.ScmObj* %args46833$ae41632$2)
ret void
}

define tailcc void @proc_clo$ae41632(%struct.ScmObj* %env$ae41632,%struct.ScmObj* %current_45args46820) {
%stackaddr$env-ref48242 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41632, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref48242
%stackaddr$env-ref48243 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41632, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48243
%stackaddr$env-ref48244 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41632, i64 2)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref48244
%stackaddr$prim48245 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46820)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim48245, align 8
%stackaddr$prim48246 = alloca %struct.ScmObj*, align 8
%current_45args46821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46820)
store volatile %struct.ScmObj* %current_45args46821, %struct.ScmObj** %stackaddr$prim48246, align 8
%stackaddr$prim48247 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46821)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48247, align 8
%ae41666 = call %struct.ScmObj* @const_init_null()
%args46823$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48248 = alloca %struct.ScmObj*, align 8
%args46823$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args46823$_37foldr140123$0)
store volatile %struct.ScmObj* %args46823$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48248, align 8
%stackaddr$prim48249 = alloca %struct.ScmObj*, align 8
%args46823$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41666, %struct.ScmObj* %args46823$_37foldr140123$1)
store volatile %struct.ScmObj* %args46823$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48249, align 8
%stackaddr$prim48250 = alloca %struct.ScmObj*, align 8
%args46823$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40278, %struct.ScmObj* %args46823$_37foldr140123$2)
store volatile %struct.ScmObj* %args46823$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48250, align 8
%stackaddr$prim48251 = alloca %struct.ScmObj*, align 8
%args46823$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40478, %struct.ScmObj* %args46823$_37foldr140123$3)
store volatile %struct.ScmObj* %args46823$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48251, align 8
%clofunc48252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48252(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46823$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41634(%struct.ScmObj* %env$ae41634,%struct.ScmObj* %current_45args46824) {
%stackaddr$env-ref48253 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41634, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref48253
%stackaddr$prim48254 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46824)
store volatile %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$prim48254, align 8
%stackaddr$prim48255 = alloca %struct.ScmObj*, align 8
%current_45args46825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46824)
store volatile %struct.ScmObj* %current_45args46825, %struct.ScmObj** %stackaddr$prim48255, align 8
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46825)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%current_45args46826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46825)
store volatile %struct.ScmObj* %current_45args46826, %struct.ScmObj** %stackaddr$prim48257, align 8
%stackaddr$prim48258 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46826)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim48258, align 8
%stackaddr$makeclosure48259 = alloca %struct.ScmObj*, align 8
%fptrToInt48260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41636 to i64
%ae41636 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48260)
store volatile %struct.ScmObj* %ae41636, %struct.ScmObj** %stackaddr$makeclosure48259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41636, %struct.ScmObj* %r40157, i64 1)
%args46832$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48261 = alloca %struct.ScmObj*, align 8
%args46832$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args46832$f40156$0)
store volatile %struct.ScmObj* %args46832$f40156$1, %struct.ScmObj** %stackaddr$prim48261, align 8
%stackaddr$prim48262 = alloca %struct.ScmObj*, align 8
%args46832$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41636, %struct.ScmObj* %args46832$f40156$1)
store volatile %struct.ScmObj* %args46832$f40156$2, %struct.ScmObj** %stackaddr$prim48262, align 8
%clofunc48263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc48263(%struct.ScmObj* %f40156, %struct.ScmObj* %args46832$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41636(%struct.ScmObj* %env$ae41636,%struct.ScmObj* %current_45args46828) {
%stackaddr$env-ref48264 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48264
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41636, i64 1)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$prim48266 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46828)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim48266, align 8
%stackaddr$prim48267 = alloca %struct.ScmObj*, align 8
%current_45args46829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46828)
store volatile %struct.ScmObj* %current_45args46829, %struct.ScmObj** %stackaddr$prim48267, align 8
%stackaddr$prim48268 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46829)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim48268, align 8
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%cpsprim40482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40482, %struct.ScmObj** %stackaddr$prim48269, align 8
%ae41641 = call %struct.ScmObj* @const_init_int(i64 0)
%args46831$k40480$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48270 = alloca %struct.ScmObj*, align 8
%args46831$k40480$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40482, %struct.ScmObj* %args46831$k40480$0)
store volatile %struct.ScmObj* %args46831$k40480$1, %struct.ScmObj** %stackaddr$prim48270, align 8
%stackaddr$prim48271 = alloca %struct.ScmObj*, align 8
%args46831$k40480$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41641, %struct.ScmObj* %args46831$k40480$1)
store volatile %struct.ScmObj* %args46831$k40480$2, %struct.ScmObj** %stackaddr$prim48271, align 8
%clofunc48272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40480)
musttail call tailcc void %clofunc48272(%struct.ScmObj* %k40480, %struct.ScmObj* %args46831$k40480$2)
ret void
}

define tailcc void @proc_clo$ae41245(%struct.ScmObj* %env$ae41245,%struct.ScmObj* %current_45args46836) {
%stackaddr$env-ref48273 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48273
%stackaddr$env-ref48274 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48274
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$prim48276 = alloca %struct.ScmObj*, align 8
%current_45args46837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %current_45args46837, %struct.ScmObj** %stackaddr$prim48276, align 8
%stackaddr$prim48277 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46837)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48277, align 8
%ae41247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48278 = alloca %struct.ScmObj*, align 8
%fptrToInt48279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41248 to i64
%ae41248 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48279)
store volatile %struct.ScmObj* %ae41248, %struct.ScmObj** %stackaddr$makeclosure48278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41248, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41248, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41248, %struct.ScmObj* %_37foldr140123, i64 2)
%args46894$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%args46894$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41248, %struct.ScmObj* %args46894$k40483$0)
store volatile %struct.ScmObj* %args46894$k40483$1, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%args46894$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41247, %struct.ScmObj* %args46894$k40483$1)
store volatile %struct.ScmObj* %args46894$k40483$2, %struct.ScmObj** %stackaddr$prim48281, align 8
%clofunc48282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48282(%struct.ScmObj* %k40483, %struct.ScmObj* %args46894$k40483$2)
ret void
}

define tailcc void @proc_clo$ae41248(%struct.ScmObj* %env$ae41248,%struct.ScmObj* %args4013040484) {
%stackaddr$env-ref48283 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41248, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48283
%stackaddr$env-ref48284 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41248, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48284
%stackaddr$env-ref48285 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41248, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48285
%stackaddr$prim48286 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040484)
store volatile %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$prim48286, align 8
%stackaddr$prim48287 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040484)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim48287, align 8
%stackaddr$prim48288 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim48288, align 8
%stackaddr$prim48289 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48289, align 8
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40264)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim48291, align 8
%stackaddr$prim48292 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim48292, align 8
%stackaddr$makeclosure48293 = alloca %struct.ScmObj*, align 8
%fptrToInt48294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41256 to i64
%ae41256 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48294)
store volatile %struct.ScmObj* %ae41256, %struct.ScmObj** %stackaddr$makeclosure48293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41256, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41257 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48295 = alloca %struct.ScmObj*, align 8
%fptrToInt48296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41258 to i64
%ae41258 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48296)
store volatile %struct.ScmObj* %ae41258, %struct.ScmObj** %stackaddr$makeclosure48295, align 8
%args46893$ae41256$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48297 = alloca %struct.ScmObj*, align 8
%args46893$ae41256$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41258, %struct.ScmObj* %args46893$ae41256$0)
store volatile %struct.ScmObj* %args46893$ae41256$1, %struct.ScmObj** %stackaddr$prim48297, align 8
%stackaddr$prim48298 = alloca %struct.ScmObj*, align 8
%args46893$ae41256$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41257, %struct.ScmObj* %args46893$ae41256$1)
store volatile %struct.ScmObj* %args46893$ae41256$2, %struct.ScmObj** %stackaddr$prim48298, align 8
%clofunc48299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41256)
musttail call tailcc void %clofunc48299(%struct.ScmObj* %ae41256, %struct.ScmObj* %args46893$ae41256$2)
ret void
}

define tailcc void @proc_clo$ae41256(%struct.ScmObj* %env$ae41256,%struct.ScmObj* %current_45args46839) {
%stackaddr$env-ref48300 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48300
%stackaddr$env-ref48301 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48301
%stackaddr$env-ref48302 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48302
%stackaddr$env-ref48303 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48303
%stackaddr$env-ref48304 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48304
%stackaddr$env-ref48305 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48305
%stackaddr$env-ref48306 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41256, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48306
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%_95k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %_95k40486, %struct.ScmObj** %stackaddr$prim48307, align 8
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%current_45args46840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %current_45args46840, %struct.ScmObj** %stackaddr$prim48308, align 8
%stackaddr$prim48309 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46840)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim48309, align 8
%stackaddr$makeclosure48310 = alloca %struct.ScmObj*, align 8
%fptrToInt48311 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41288 to i64
%ae41288 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48311)
store volatile %struct.ScmObj* %ae41288, %struct.ScmObj** %stackaddr$makeclosure48310, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41288, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41290 = call %struct.ScmObj* @const_init_false()
%args46886$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%args46886$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46886$_37foldr140123$0)
store volatile %struct.ScmObj* %args46886$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48312, align 8
%stackaddr$prim48313 = alloca %struct.ScmObj*, align 8
%args46886$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41290, %struct.ScmObj* %args46886$_37foldr140123$1)
store volatile %struct.ScmObj* %args46886$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48313, align 8
%stackaddr$prim48314 = alloca %struct.ScmObj*, align 8
%args46886$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args46886$_37foldr140123$2)
store volatile %struct.ScmObj* %args46886$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48314, align 8
%stackaddr$prim48315 = alloca %struct.ScmObj*, align 8
%args46886$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41288, %struct.ScmObj* %args46886$_37foldr140123$3)
store volatile %struct.ScmObj* %args46886$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48315, align 8
%clofunc48316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48316(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46886$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41288(%struct.ScmObj* %env$ae41288,%struct.ScmObj* %current_45args46842) {
%stackaddr$env-ref48317 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48317
%stackaddr$env-ref48318 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48318
%stackaddr$env-ref48319 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48319
%stackaddr$env-ref48320 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48320
%stackaddr$env-ref48321 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48321
%stackaddr$env-ref48322 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48322
%stackaddr$env-ref48323 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41288, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48323
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46842)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%current_45args46843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46842)
store volatile %struct.ScmObj* %current_45args46843, %struct.ScmObj** %stackaddr$prim48325, align 8
%stackaddr$prim48326 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46843)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim48326, align 8
%truthy$cmp48327 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40267)
%cmp$cmp48327 = icmp eq i64 %truthy$cmp48327, 1
br i1 %cmp$cmp48327, label %truebranch$cmp48327, label %falsebranch$cmp48327
truebranch$cmp48327:
%ae41299 = call %struct.ScmObj* @const_init_int(i64 0)
%args46845$k40485$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%args46845$k40485$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args46845$k40485$0)
store volatile %struct.ScmObj* %args46845$k40485$1, %struct.ScmObj** %stackaddr$prim48328, align 8
%stackaddr$prim48329 = alloca %struct.ScmObj*, align 8
%args46845$k40485$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41299, %struct.ScmObj* %args46845$k40485$1)
store volatile %struct.ScmObj* %args46845$k40485$2, %struct.ScmObj** %stackaddr$prim48329, align 8
%clofunc48330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40485)
musttail call tailcc void %clofunc48330(%struct.ScmObj* %k40485, %struct.ScmObj* %args46845$k40485$2)
ret void
falsebranch$cmp48327:
%stackaddr$makeclosure48331 = alloca %struct.ScmObj*, align 8
%fptrToInt48332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41304 to i64
%ae41304 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48332)
store volatile %struct.ScmObj* %ae41304, %struct.ScmObj** %stackaddr$makeclosure48331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41304, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48333 = alloca %struct.ScmObj*, align 8
%fptrToInt48334 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41306 to i64
%ae41306 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48334)
store volatile %struct.ScmObj* %ae41306, %struct.ScmObj** %stackaddr$makeclosure48333, align 8
%args46885$ae41304$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48335 = alloca %struct.ScmObj*, align 8
%args46885$ae41304$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41306, %struct.ScmObj* %args46885$ae41304$0)
store volatile %struct.ScmObj* %args46885$ae41304$1, %struct.ScmObj** %stackaddr$prim48335, align 8
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%args46885$ae41304$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41305, %struct.ScmObj* %args46885$ae41304$1)
store volatile %struct.ScmObj* %args46885$ae41304$2, %struct.ScmObj** %stackaddr$prim48336, align 8
%clofunc48337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41304)
musttail call tailcc void %clofunc48337(%struct.ScmObj* %ae41304, %struct.ScmObj* %args46885$ae41304$2)
ret void
}

define tailcc void @proc_clo$ae41304(%struct.ScmObj* %env$ae41304,%struct.ScmObj* %current_45args46846) {
%stackaddr$env-ref48338 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48338
%stackaddr$env-ref48339 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48339
%stackaddr$env-ref48340 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48340
%stackaddr$env-ref48341 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48341
%stackaddr$env-ref48342 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48342
%stackaddr$env-ref48343 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48343
%stackaddr$env-ref48344 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41304, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48344
%stackaddr$prim48345 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46846)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim48345, align 8
%stackaddr$prim48346 = alloca %struct.ScmObj*, align 8
%current_45args46847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46846)
store volatile %struct.ScmObj* %current_45args46847, %struct.ScmObj** %stackaddr$prim48346, align 8
%stackaddr$prim48347 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46847)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48347, align 8
%stackaddr$makeclosure48348 = alloca %struct.ScmObj*, align 8
%fptrToInt48349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41325 to i64
%ae41325 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48349)
store volatile %struct.ScmObj* %ae41325, %struct.ScmObj** %stackaddr$makeclosure48348, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41325, %struct.ScmObj* %_37foldr140123, i64 6)
%args46880$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48350 = alloca %struct.ScmObj*, align 8
%args46880$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46880$_37map140119$0)
store volatile %struct.ScmObj* %args46880$_37map140119$1, %struct.ScmObj** %stackaddr$prim48350, align 8
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%args46880$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args46880$_37map140119$1)
store volatile %struct.ScmObj* %args46880$_37map140119$2, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%args46880$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41325, %struct.ScmObj* %args46880$_37map140119$2)
store volatile %struct.ScmObj* %args46880$_37map140119$3, %struct.ScmObj** %stackaddr$prim48352, align 8
%clofunc48353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48353(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46880$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41325(%struct.ScmObj* %env$ae41325,%struct.ScmObj* %current_45args46849) {
%stackaddr$env-ref48354 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48354
%stackaddr$env-ref48355 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48355
%stackaddr$env-ref48356 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48356
%stackaddr$env-ref48357 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48357
%stackaddr$env-ref48358 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48358
%stackaddr$env-ref48359 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48359
%stackaddr$env-ref48360 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41325, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48360
%stackaddr$prim48361 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46849)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim48361, align 8
%stackaddr$prim48362 = alloca %struct.ScmObj*, align 8
%current_45args46850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46849)
store volatile %struct.ScmObj* %current_45args46850, %struct.ScmObj** %stackaddr$prim48362, align 8
%stackaddr$prim48363 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46850)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim48363, align 8
%stackaddr$makeclosure48364 = alloca %struct.ScmObj*, align 8
%fptrToInt48365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41328 to i64
%ae41328 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48365)
store volatile %struct.ScmObj* %ae41328, %struct.ScmObj** %stackaddr$makeclosure48364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41328, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41329 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48366 = alloca %struct.ScmObj*, align 8
%fptrToInt48367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41330 to i64
%ae41330 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48367)
store volatile %struct.ScmObj* %ae41330, %struct.ScmObj** %stackaddr$makeclosure48366, align 8
%args46879$ae41328$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%args46879$ae41328$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41330, %struct.ScmObj* %args46879$ae41328$0)
store volatile %struct.ScmObj* %args46879$ae41328$1, %struct.ScmObj** %stackaddr$prim48368, align 8
%stackaddr$prim48369 = alloca %struct.ScmObj*, align 8
%args46879$ae41328$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41329, %struct.ScmObj* %args46879$ae41328$1)
store volatile %struct.ScmObj* %args46879$ae41328$2, %struct.ScmObj** %stackaddr$prim48369, align 8
%clofunc48370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41328)
musttail call tailcc void %clofunc48370(%struct.ScmObj* %ae41328, %struct.ScmObj* %args46879$ae41328$2)
ret void
}

define tailcc void @proc_clo$ae41328(%struct.ScmObj* %env$ae41328,%struct.ScmObj* %current_45args46852) {
%stackaddr$env-ref48371 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48371
%stackaddr$env-ref48372 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48372
%stackaddr$env-ref48373 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48373
%stackaddr$env-ref48374 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48374
%stackaddr$env-ref48375 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48375
%stackaddr$env-ref48376 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48376
%stackaddr$env-ref48377 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48377
%stackaddr$env-ref48378 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41328, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48378
%stackaddr$prim48379 = alloca %struct.ScmObj*, align 8
%_95k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46852)
store volatile %struct.ScmObj* %_95k40490, %struct.ScmObj** %stackaddr$prim48379, align 8
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%current_45args46853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46852)
store volatile %struct.ScmObj* %current_45args46853, %struct.ScmObj** %stackaddr$prim48380, align 8
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim48381, align 8
%stackaddr$makeclosure48382 = alloca %struct.ScmObj*, align 8
%fptrToInt48383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41349 to i64
%ae41349 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48383)
store volatile %struct.ScmObj* %ae41349, %struct.ScmObj** %stackaddr$makeclosure48382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41349, %struct.ScmObj* %lsts_4340138, i64 5)
%args46874$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%args46874$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46874$_37map140119$0)
store volatile %struct.ScmObj* %args46874$_37map140119$1, %struct.ScmObj** %stackaddr$prim48384, align 8
%stackaddr$prim48385 = alloca %struct.ScmObj*, align 8
%args46874$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args46874$_37map140119$1)
store volatile %struct.ScmObj* %args46874$_37map140119$2, %struct.ScmObj** %stackaddr$prim48385, align 8
%stackaddr$prim48386 = alloca %struct.ScmObj*, align 8
%args46874$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41349, %struct.ScmObj* %args46874$_37map140119$2)
store volatile %struct.ScmObj* %args46874$_37map140119$3, %struct.ScmObj** %stackaddr$prim48386, align 8
%clofunc48387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48387(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46874$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41349(%struct.ScmObj* %env$ae41349,%struct.ScmObj* %current_45args46855) {
%stackaddr$env-ref48388 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48388
%stackaddr$env-ref48389 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48389
%stackaddr$env-ref48390 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48390
%stackaddr$env-ref48391 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48391
%stackaddr$env-ref48392 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48392
%stackaddr$env-ref48393 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41349, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48393
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%_95k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46855)
store volatile %struct.ScmObj* %_95k40491, %struct.ScmObj** %stackaddr$prim48394, align 8
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%current_45args46856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46855)
store volatile %struct.ScmObj* %current_45args46856, %struct.ScmObj** %stackaddr$prim48395, align 8
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46856)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim48396, align 8
%stackaddr$makeclosure48397 = alloca %struct.ScmObj*, align 8
%fptrToInt48398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41352 to i64
%ae41352 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48398)
store volatile %struct.ScmObj* %ae41352, %struct.ScmObj** %stackaddr$makeclosure48397, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %vs40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %_37foldr140123, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41352, %struct.ScmObj* %lsts_4340138, i64 6)
%ae41353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48399 = alloca %struct.ScmObj*, align 8
%fptrToInt48400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41354 to i64
%ae41354 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48400)
store volatile %struct.ScmObj* %ae41354, %struct.ScmObj** %stackaddr$makeclosure48399, align 8
%args46873$ae41352$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%args46873$ae41352$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41354, %struct.ScmObj* %args46873$ae41352$0)
store volatile %struct.ScmObj* %args46873$ae41352$1, %struct.ScmObj** %stackaddr$prim48401, align 8
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%args46873$ae41352$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41353, %struct.ScmObj* %args46873$ae41352$1)
store volatile %struct.ScmObj* %args46873$ae41352$2, %struct.ScmObj** %stackaddr$prim48402, align 8
%clofunc48403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41352)
musttail call tailcc void %clofunc48403(%struct.ScmObj* %ae41352, %struct.ScmObj* %args46873$ae41352$2)
ret void
}

define tailcc void @proc_clo$ae41352(%struct.ScmObj* %env$ae41352,%struct.ScmObj* %current_45args46858) {
%stackaddr$env-ref48404 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48404
%stackaddr$env-ref48405 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48405
%stackaddr$env-ref48406 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 2)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48406
%stackaddr$env-ref48407 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48407
%stackaddr$env-ref48408 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48408
%stackaddr$env-ref48409 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 5)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48409
%stackaddr$env-ref48410 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41352, i64 6)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48410
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46858)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim48411, align 8
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%current_45args46859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46858)
store volatile %struct.ScmObj* %current_45args46859, %struct.ScmObj** %stackaddr$prim48412, align 8
%stackaddr$prim48413 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46859)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim48413, align 8
%stackaddr$prim48414 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48414, align 8
%stackaddr$prim48415 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40271)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48415, align 8
%stackaddr$makeclosure48416 = alloca %struct.ScmObj*, align 8
%fptrToInt48417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41378 to i64
%ae41378 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48417)
store volatile %struct.ScmObj* %ae41378, %struct.ScmObj** %stackaddr$makeclosure48416, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41378, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41378, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41378, %struct.ScmObj* %vs40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41378, %struct.ScmObj* %anf_45bind40270, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41378, %struct.ScmObj* %_37foldr140123, i64 4)
%stackaddr$prim48418 = alloca %struct.ScmObj*, align 8
%cpsargs40496 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41378, %struct.ScmObj* %anf_45bind40272)
store volatile %struct.ScmObj* %cpsargs40496, %struct.ScmObj** %stackaddr$prim48418, align 8
%clofunc48419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc48419(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40496)
ret void
}

define tailcc void @proc_clo$ae41378(%struct.ScmObj* %env$ae41378,%struct.ScmObj* %current_45args46861) {
%stackaddr$env-ref48420 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41378, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48420
%stackaddr$env-ref48421 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41378, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48421
%stackaddr$env-ref48422 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41378, i64 2)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48422
%stackaddr$env-ref48423 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41378, i64 3)
store %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$env-ref48423
%stackaddr$env-ref48424 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41378, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48424
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46861)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%current_45args46862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46861)
store volatile %struct.ScmObj* %current_45args46862, %struct.ScmObj** %stackaddr$prim48426, align 8
%stackaddr$prim48427 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46862)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48427, align 8
%ae41383 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48428 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %ae41383)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim48428, align 8
%stackaddr$makeclosure48429 = alloca %struct.ScmObj*, align 8
%fptrToInt48430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41385 to i64
%ae41385 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48430)
store volatile %struct.ScmObj* %ae41385, %struct.ScmObj** %stackaddr$makeclosure48429, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %k40485, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %f40133, i64 1)
%args46867$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%args46867$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args46867$_37foldr140123$0)
store volatile %struct.ScmObj* %args46867$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48431, align 8
%stackaddr$prim48432 = alloca %struct.ScmObj*, align 8
%args46867$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args46867$_37foldr140123$1)
store volatile %struct.ScmObj* %args46867$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48432, align 8
%stackaddr$prim48433 = alloca %struct.ScmObj*, align 8
%args46867$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args46867$_37foldr140123$2)
store volatile %struct.ScmObj* %args46867$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48433, align 8
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%args46867$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41385, %struct.ScmObj* %args46867$_37foldr140123$3)
store volatile %struct.ScmObj* %args46867$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48434, align 8
%clofunc48435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48435(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46867$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41385(%struct.ScmObj* %env$ae41385,%struct.ScmObj* %current_45args46864) {
%stackaddr$env-ref48436 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 0)
store %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$env-ref48436
%stackaddr$env-ref48437 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48437
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46864)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim48438, align 8
%stackaddr$prim48439 = alloca %struct.ScmObj*, align 8
%current_45args46865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46864)
store volatile %struct.ScmObj* %current_45args46865, %struct.ScmObj** %stackaddr$prim48439, align 8
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%cpsargs40495 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40485, %struct.ScmObj* %anf_45bind40275)
store volatile %struct.ScmObj* %cpsargs40495, %struct.ScmObj** %stackaddr$prim48441, align 8
%clofunc48442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc48442(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40495)
ret void
}

define tailcc void @proc_clo$ae41354(%struct.ScmObj* %env$ae41354,%struct.ScmObj* %current_45args46868) {
%stackaddr$prim48443 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46868)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim48443, align 8
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%current_45args46869 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46868)
store volatile %struct.ScmObj* %current_45args46869, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46869)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim48445, align 8
%stackaddr$prim48446 = alloca %struct.ScmObj*, align 8
%current_45args46870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46869)
store volatile %struct.ScmObj* %current_45args46870, %struct.ScmObj** %stackaddr$prim48446, align 8
%stackaddr$prim48447 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46870)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim48447, align 8
%stackaddr$prim48448 = alloca %struct.ScmObj*, align 8
%cpsprim40498 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40498, %struct.ScmObj** %stackaddr$prim48448, align 8
%ae41358 = call %struct.ScmObj* @const_init_int(i64 0)
%args46872$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%args46872$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40498, %struct.ScmObj* %args46872$k40497$0)
store volatile %struct.ScmObj* %args46872$k40497$1, %struct.ScmObj** %stackaddr$prim48449, align 8
%stackaddr$prim48450 = alloca %struct.ScmObj*, align 8
%args46872$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args46872$k40497$1)
store volatile %struct.ScmObj* %args46872$k40497$2, %struct.ScmObj** %stackaddr$prim48450, align 8
%clofunc48451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc48451(%struct.ScmObj* %k40497, %struct.ScmObj* %args46872$k40497$2)
ret void
}

define tailcc void @proc_clo$ae41330(%struct.ScmObj* %env$ae41330,%struct.ScmObj* %current_45args46875) {
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46875)
store volatile %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$prim48452, align 8
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%current_45args46876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46875)
store volatile %struct.ScmObj* %current_45args46876, %struct.ScmObj** %stackaddr$prim48453, align 8
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46876)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim48454, align 8
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%cpsprim40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40500, %struct.ScmObj** %stackaddr$prim48455, align 8
%ae41333 = call %struct.ScmObj* @const_init_int(i64 0)
%args46878$k40499$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48456 = alloca %struct.ScmObj*, align 8
%args46878$k40499$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40500, %struct.ScmObj* %args46878$k40499$0)
store volatile %struct.ScmObj* %args46878$k40499$1, %struct.ScmObj** %stackaddr$prim48456, align 8
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%args46878$k40499$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41333, %struct.ScmObj* %args46878$k40499$1)
store volatile %struct.ScmObj* %args46878$k40499$2, %struct.ScmObj** %stackaddr$prim48457, align 8
%clofunc48458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40499)
musttail call tailcc void %clofunc48458(%struct.ScmObj* %k40499, %struct.ScmObj* %args46878$k40499$2)
ret void
}

define tailcc void @proc_clo$ae41306(%struct.ScmObj* %env$ae41306,%struct.ScmObj* %current_45args46881) {
%stackaddr$prim48459 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46881)
store volatile %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$prim48459, align 8
%stackaddr$prim48460 = alloca %struct.ScmObj*, align 8
%current_45args46882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46881)
store volatile %struct.ScmObj* %current_45args46882, %struct.ScmObj** %stackaddr$prim48460, align 8
%stackaddr$prim48461 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46882)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim48461, align 8
%stackaddr$prim48462 = alloca %struct.ScmObj*, align 8
%cpsprim40502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40502, %struct.ScmObj** %stackaddr$prim48462, align 8
%ae41309 = call %struct.ScmObj* @const_init_int(i64 0)
%args46884$k40501$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48463 = alloca %struct.ScmObj*, align 8
%args46884$k40501$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40502, %struct.ScmObj* %args46884$k40501$0)
store volatile %struct.ScmObj* %args46884$k40501$1, %struct.ScmObj** %stackaddr$prim48463, align 8
%stackaddr$prim48464 = alloca %struct.ScmObj*, align 8
%args46884$k40501$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41309, %struct.ScmObj* %args46884$k40501$1)
store volatile %struct.ScmObj* %args46884$k40501$2, %struct.ScmObj** %stackaddr$prim48464, align 8
%clofunc48465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40501)
musttail call tailcc void %clofunc48465(%struct.ScmObj* %k40501, %struct.ScmObj* %args46884$k40501$2)
ret void
}

define tailcc void @proc_clo$ae41258(%struct.ScmObj* %env$ae41258,%struct.ScmObj* %current_45args46887) {
%stackaddr$prim48466 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46887)
store volatile %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$prim48466, align 8
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%current_45args46888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46887)
store volatile %struct.ScmObj* %current_45args46888, %struct.ScmObj** %stackaddr$prim48467, align 8
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46888)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim48468, align 8
%stackaddr$prim48469 = alloca %struct.ScmObj*, align 8
%current_45args46889 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46888)
store volatile %struct.ScmObj* %current_45args46889, %struct.ScmObj** %stackaddr$prim48469, align 8
%stackaddr$prim48470 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46889)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim48470, align 8
%truthy$cmp48471 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp48471 = icmp eq i64 %truthy$cmp48471, 1
br i1 %cmp$cmp48471, label %truebranch$cmp48471, label %falsebranch$cmp48471
truebranch$cmp48471:
%ae41261 = call %struct.ScmObj* @const_init_int(i64 0)
%args46891$k40503$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48472 = alloca %struct.ScmObj*, align 8
%args46891$k40503$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args46891$k40503$0)
store volatile %struct.ScmObj* %args46891$k40503$1, %struct.ScmObj** %stackaddr$prim48472, align 8
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%args46891$k40503$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41261, %struct.ScmObj* %args46891$k40503$1)
store volatile %struct.ScmObj* %args46891$k40503$2, %struct.ScmObj** %stackaddr$prim48473, align 8
%clofunc48474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40503)
musttail call tailcc void %clofunc48474(%struct.ScmObj* %k40503, %struct.ScmObj* %args46891$k40503$2)
ret void
falsebranch$cmp48471:
%stackaddr$prim48475 = alloca %struct.ScmObj*, align 8
%cpsprim40504 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40504, %struct.ScmObj** %stackaddr$prim48475, align 8
%ae41268 = call %struct.ScmObj* @const_init_int(i64 0)
%args46892$k40503$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48476 = alloca %struct.ScmObj*, align 8
%args46892$k40503$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40504, %struct.ScmObj* %args46892$k40503$0)
store volatile %struct.ScmObj* %args46892$k40503$1, %struct.ScmObj** %stackaddr$prim48476, align 8
%stackaddr$prim48477 = alloca %struct.ScmObj*, align 8
%args46892$k40503$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41268, %struct.ScmObj* %args46892$k40503$1)
store volatile %struct.ScmObj* %args46892$k40503$2, %struct.ScmObj** %stackaddr$prim48477, align 8
%clofunc48478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40503)
musttail call tailcc void %clofunc48478(%struct.ScmObj* %k40503, %struct.ScmObj* %args46892$k40503$2)
ret void
}

define tailcc void @proc_clo$ae41215(%struct.ScmObj* %env$ae41215,%struct.ScmObj* %current_45args46896) {
%stackaddr$env-ref48479 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41215, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48479
%stackaddr$env-ref48480 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41215, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref48480
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%current_45args46897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %current_45args46897, %struct.ScmObj** %stackaddr$prim48482, align 8
%stackaddr$prim48483 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim48483, align 8
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%current_45args46898 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %current_45args46898, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46898)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim48485, align 8
%stackaddr$makeclosure48486 = alloca %struct.ScmObj*, align 8
%fptrToInt48487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41217 to i64
%ae41217 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48487)
store volatile %struct.ScmObj* %ae41217, %struct.ScmObj** %stackaddr$makeclosure48486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %_37take40115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %lst40144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %n40143, i64 3)
%args46904$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48488 = alloca %struct.ScmObj*, align 8
%args46904$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46904$_37length40112$0)
store volatile %struct.ScmObj* %args46904$_37length40112$1, %struct.ScmObj** %stackaddr$prim48488, align 8
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%args46904$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41217, %struct.ScmObj* %args46904$_37length40112$1)
store volatile %struct.ScmObj* %args46904$_37length40112$2, %struct.ScmObj** %stackaddr$prim48489, align 8
%clofunc48490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc48490(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args46904$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41217(%struct.ScmObj* %env$ae41217,%struct.ScmObj* %current_45args46900) {
%stackaddr$env-ref48491 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref48491
%stackaddr$env-ref48492 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 1)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48492
%stackaddr$env-ref48493 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 2)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref48493
%stackaddr$env-ref48494 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 3)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref48494
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%_95k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46900)
store volatile %struct.ScmObj* %_95k40506, %struct.ScmObj** %stackaddr$prim48495, align 8
%stackaddr$prim48496 = alloca %struct.ScmObj*, align 8
%current_45args46901 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46900)
store volatile %struct.ScmObj* %current_45args46901, %struct.ScmObj** %stackaddr$prim48496, align 8
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46901)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim48497, align 8
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim48498, align 8
%args46903$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48499 = alloca %struct.ScmObj*, align 8
%args46903$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args46903$_37take40115$0)
store volatile %struct.ScmObj* %args46903$_37take40115$1, %struct.ScmObj** %stackaddr$prim48499, align 8
%stackaddr$prim48500 = alloca %struct.ScmObj*, align 8
%args46903$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46903$_37take40115$1)
store volatile %struct.ScmObj* %args46903$_37take40115$2, %struct.ScmObj** %stackaddr$prim48500, align 8
%stackaddr$prim48501 = alloca %struct.ScmObj*, align 8
%args46903$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40505, %struct.ScmObj* %args46903$_37take40115$2)
store volatile %struct.ScmObj* %args46903$_37take40115$3, %struct.ScmObj** %stackaddr$prim48501, align 8
%clofunc48502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc48502(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args46903$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41161(%struct.ScmObj* %env$ae41161,%struct.ScmObj* %current_45args46906) {
%stackaddr$env-ref48503 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41161, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48503
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46906)
store volatile %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$prim48504, align 8
%stackaddr$prim48505 = alloca %struct.ScmObj*, align 8
%current_45args46907 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46906)
store volatile %struct.ScmObj* %current_45args46907, %struct.ScmObj** %stackaddr$prim48505, align 8
%stackaddr$prim48506 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46907)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim48506, align 8
%stackaddr$makeclosure48507 = alloca %struct.ScmObj*, align 8
%fptrToInt48508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41162 to i64
%ae41162 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48508)
store volatile %struct.ScmObj* %ae41162, %struct.ScmObj** %stackaddr$makeclosure48507, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41162, %struct.ScmObj* %k40507, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41162, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41162, %struct.ScmObj* %lst40146, i64 2)
%ae41163 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48509 = alloca %struct.ScmObj*, align 8
%fptrToInt48510 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41164 to i64
%ae41164 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48510)
store volatile %struct.ScmObj* %ae41164, %struct.ScmObj** %stackaddr$makeclosure48509, align 8
%args46918$ae41162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48511 = alloca %struct.ScmObj*, align 8
%args46918$ae41162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41164, %struct.ScmObj* %args46918$ae41162$0)
store volatile %struct.ScmObj* %args46918$ae41162$1, %struct.ScmObj** %stackaddr$prim48511, align 8
%stackaddr$prim48512 = alloca %struct.ScmObj*, align 8
%args46918$ae41162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41163, %struct.ScmObj* %args46918$ae41162$1)
store volatile %struct.ScmObj* %args46918$ae41162$2, %struct.ScmObj** %stackaddr$prim48512, align 8
%clofunc48513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41162)
musttail call tailcc void %clofunc48513(%struct.ScmObj* %ae41162, %struct.ScmObj* %args46918$ae41162$2)
ret void
}

define tailcc void @proc_clo$ae41162(%struct.ScmObj* %env$ae41162,%struct.ScmObj* %current_45args46909) {
%stackaddr$env-ref48514 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41162, i64 0)
store %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$env-ref48514
%stackaddr$env-ref48515 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41162, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48515
%stackaddr$env-ref48516 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41162, i64 2)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref48516
%stackaddr$prim48517 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46909)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim48517, align 8
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%current_45args46910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46909)
store volatile %struct.ScmObj* %current_45args46910, %struct.ScmObj** %stackaddr$prim48518, align 8
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46910)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48519, align 8
%ae41183 = call %struct.ScmObj* @const_init_null()
%args46912$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48520 = alloca %struct.ScmObj*, align 8
%args46912$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args46912$_37foldl140107$0)
store volatile %struct.ScmObj* %args46912$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48520, align 8
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%args46912$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41183, %struct.ScmObj* %args46912$_37foldl140107$1)
store volatile %struct.ScmObj* %args46912$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48521, align 8
%stackaddr$prim48522 = alloca %struct.ScmObj*, align 8
%args46912$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args46912$_37foldl140107$2)
store volatile %struct.ScmObj* %args46912$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48522, align 8
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%args46912$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40507, %struct.ScmObj* %args46912$_37foldl140107$3)
store volatile %struct.ScmObj* %args46912$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48523, align 8
%clofunc48524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48524(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46912$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41164(%struct.ScmObj* %env$ae41164,%struct.ScmObj* %current_45args46913) {
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %k40509, %struct.ScmObj** %stackaddr$prim48525, align 8
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%current_45args46914 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %current_45args46914, %struct.ScmObj** %stackaddr$prim48526, align 8
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim48527, align 8
%stackaddr$prim48528 = alloca %struct.ScmObj*, align 8
%current_45args46915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %current_45args46915, %struct.ScmObj** %stackaddr$prim48528, align 8
%stackaddr$prim48529 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46915)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim48529, align 8
%ae41166 = call %struct.ScmObj* @const_init_int(i64 0)
%args46917$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48530 = alloca %struct.ScmObj*, align 8
%args46917$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args46917$k40509$0)
store volatile %struct.ScmObj* %args46917$k40509$1, %struct.ScmObj** %stackaddr$prim48530, align 8
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%args46917$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41166, %struct.ScmObj* %args46917$k40509$1)
store volatile %struct.ScmObj* %args46917$k40509$2, %struct.ScmObj** %stackaddr$prim48531, align 8
%clofunc48532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc48532(%struct.ScmObj* %k40509, %struct.ScmObj* %args46917$k40509$2)
ret void
}

define tailcc void @proc_clo$ae41082(%struct.ScmObj* %env$ae41082,%struct.ScmObj* %current_45args46921) {
%stackaddr$prim48533 = alloca %struct.ScmObj*, align 8
%k40510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %k40510, %struct.ScmObj** %stackaddr$prim48533, align 8
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%current_45args46922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %current_45args46922, %struct.ScmObj** %stackaddr$prim48534, align 8
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46922)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim48535, align 8
%ae41084 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48536 = alloca %struct.ScmObj*, align 8
%fptrToInt48537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41085 to i64
%ae41085 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48537)
store volatile %struct.ScmObj* %ae41085, %struct.ScmObj** %stackaddr$makeclosure48536, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41085, %struct.ScmObj* %_37foldl140108, i64 0)
%args46935$k40510$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48538 = alloca %struct.ScmObj*, align 8
%args46935$k40510$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41085, %struct.ScmObj* %args46935$k40510$0)
store volatile %struct.ScmObj* %args46935$k40510$1, %struct.ScmObj** %stackaddr$prim48538, align 8
%stackaddr$prim48539 = alloca %struct.ScmObj*, align 8
%args46935$k40510$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41084, %struct.ScmObj* %args46935$k40510$1)
store volatile %struct.ScmObj* %args46935$k40510$2, %struct.ScmObj** %stackaddr$prim48539, align 8
%clofunc48540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40510)
musttail call tailcc void %clofunc48540(%struct.ScmObj* %k40510, %struct.ScmObj* %args46935$k40510$2)
ret void
}

define tailcc void @proc_clo$ae41085(%struct.ScmObj* %env$ae41085,%struct.ScmObj* %current_45args46924) {
%stackaddr$env-ref48541 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41085, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48541
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46924)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim48542, align 8
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%current_45args46925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46924)
store volatile %struct.ScmObj* %current_45args46925, %struct.ScmObj** %stackaddr$prim48543, align 8
%stackaddr$prim48544 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46925)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim48544, align 8
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%current_45args46926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46925)
store volatile %struct.ScmObj* %current_45args46926, %struct.ScmObj** %stackaddr$prim48545, align 8
%stackaddr$prim48546 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46926)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim48546, align 8
%stackaddr$prim48547 = alloca %struct.ScmObj*, align 8
%current_45args46927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46926)
store volatile %struct.ScmObj* %current_45args46927, %struct.ScmObj** %stackaddr$prim48547, align 8
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46927)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim48548, align 8
%stackaddr$prim48549 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim48549, align 8
%truthy$cmp48550 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40256)
%cmp$cmp48550 = icmp eq i64 %truthy$cmp48550, 1
br i1 %cmp$cmp48550, label %truebranch$cmp48550, label %falsebranch$cmp48550
truebranch$cmp48550:
%ae41089 = call %struct.ScmObj* @const_init_int(i64 0)
%args46929$k40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48551 = alloca %struct.ScmObj*, align 8
%args46929$k40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46929$k40511$0)
store volatile %struct.ScmObj* %args46929$k40511$1, %struct.ScmObj** %stackaddr$prim48551, align 8
%stackaddr$prim48552 = alloca %struct.ScmObj*, align 8
%args46929$k40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41089, %struct.ScmObj* %args46929$k40511$1)
store volatile %struct.ScmObj* %args46929$k40511$2, %struct.ScmObj** %stackaddr$prim48552, align 8
%clofunc48553 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40511)
musttail call tailcc void %clofunc48553(%struct.ScmObj* %k40511, %struct.ScmObj* %args46929$k40511$2)
ret void
falsebranch$cmp48550:
%stackaddr$prim48554 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48554, align 8
%stackaddr$makeclosure48555 = alloca %struct.ScmObj*, align 8
%fptrToInt48556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41096 to i64
%ae41096 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48556)
store volatile %struct.ScmObj* %ae41096, %struct.ScmObj** %stackaddr$makeclosure48555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41096, %struct.ScmObj* %k40511, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41096, %struct.ScmObj* %f40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41096, %struct.ScmObj* %lst40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41096, %struct.ScmObj* %_37foldl140108, i64 3)
%args46934$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48557 = alloca %struct.ScmObj*, align 8
%args46934$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46934$f40111$0)
store volatile %struct.ScmObj* %args46934$f40111$1, %struct.ScmObj** %stackaddr$prim48557, align 8
%stackaddr$prim48558 = alloca %struct.ScmObj*, align 8
%args46934$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args46934$f40111$1)
store volatile %struct.ScmObj* %args46934$f40111$2, %struct.ScmObj** %stackaddr$prim48558, align 8
%stackaddr$prim48559 = alloca %struct.ScmObj*, align 8
%args46934$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41096, %struct.ScmObj* %args46934$f40111$2)
store volatile %struct.ScmObj* %args46934$f40111$3, %struct.ScmObj** %stackaddr$prim48559, align 8
%clofunc48560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc48560(%struct.ScmObj* %f40111, %struct.ScmObj* %args46934$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41096(%struct.ScmObj* %env$ae41096,%struct.ScmObj* %current_45args46930) {
%stackaddr$env-ref48561 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41096, i64 0)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref48561
%stackaddr$env-ref48562 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41096, i64 1)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref48562
%stackaddr$env-ref48563 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41096, i64 2)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref48563
%stackaddr$env-ref48564 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41096, i64 3)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48564
%stackaddr$prim48565 = alloca %struct.ScmObj*, align 8
%_95k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46930)
store volatile %struct.ScmObj* %_95k40512, %struct.ScmObj** %stackaddr$prim48565, align 8
%stackaddr$prim48566 = alloca %struct.ScmObj*, align 8
%current_45args46931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46930)
store volatile %struct.ScmObj* %current_45args46931, %struct.ScmObj** %stackaddr$prim48566, align 8
%stackaddr$prim48567 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46931)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim48567, align 8
%stackaddr$prim48568 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim48568, align 8
%args46933$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%args46933$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args46933$_37foldl140108$0)
store volatile %struct.ScmObj* %args46933$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48569, align 8
%stackaddr$prim48570 = alloca %struct.ScmObj*, align 8
%args46933$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args46933$_37foldl140108$1)
store volatile %struct.ScmObj* %args46933$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48570, align 8
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%args46933$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args46933$_37foldl140108$2)
store volatile %struct.ScmObj* %args46933$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%args46933$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40511, %struct.ScmObj* %args46933$_37foldl140108$3)
store volatile %struct.ScmObj* %args46933$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48572, align 8
%clofunc48573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48573(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46933$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae40999(%struct.ScmObj* %env$ae40999,%struct.ScmObj* %current_45args46938) {
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46938)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%current_45args46939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46938)
store volatile %struct.ScmObj* %current_45args46939, %struct.ScmObj** %stackaddr$prim48575, align 8
%stackaddr$prim48576 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46939)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim48576, align 8
%ae41001 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48577 = alloca %struct.ScmObj*, align 8
%fptrToInt48578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41002 to i64
%ae41002 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48578)
store volatile %struct.ScmObj* %ae41002, %struct.ScmObj** %stackaddr$makeclosure48577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41002, %struct.ScmObj* %_37length40113, i64 0)
%args46950$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48579 = alloca %struct.ScmObj*, align 8
%args46950$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41002, %struct.ScmObj* %args46950$k40513$0)
store volatile %struct.ScmObj* %args46950$k40513$1, %struct.ScmObj** %stackaddr$prim48579, align 8
%stackaddr$prim48580 = alloca %struct.ScmObj*, align 8
%args46950$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41001, %struct.ScmObj* %args46950$k40513$1)
store volatile %struct.ScmObj* %args46950$k40513$2, %struct.ScmObj** %stackaddr$prim48580, align 8
%clofunc48581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc48581(%struct.ScmObj* %k40513, %struct.ScmObj* %args46950$k40513$2)
ret void
}

define tailcc void @proc_clo$ae41002(%struct.ScmObj* %env$ae41002,%struct.ScmObj* %current_45args46941) {
%stackaddr$env-ref48582 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41002, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48582
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46941)
store volatile %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$prim48583, align 8
%stackaddr$prim48584 = alloca %struct.ScmObj*, align 8
%current_45args46942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46941)
store volatile %struct.ScmObj* %current_45args46942, %struct.ScmObj** %stackaddr$prim48584, align 8
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46942)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim48585, align 8
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim48586, align 8
%truthy$cmp48587 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40252)
%cmp$cmp48587 = icmp eq i64 %truthy$cmp48587, 1
br i1 %cmp$cmp48587, label %truebranch$cmp48587, label %falsebranch$cmp48587
truebranch$cmp48587:
%ae41006 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41007 = call %struct.ScmObj* @const_init_int(i64 0)
%args46944$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%args46944$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41007, %struct.ScmObj* %args46944$k40514$0)
store volatile %struct.ScmObj* %args46944$k40514$1, %struct.ScmObj** %stackaddr$prim48588, align 8
%stackaddr$prim48589 = alloca %struct.ScmObj*, align 8
%args46944$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41006, %struct.ScmObj* %args46944$k40514$1)
store volatile %struct.ScmObj* %args46944$k40514$2, %struct.ScmObj** %stackaddr$prim48589, align 8
%clofunc48590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc48590(%struct.ScmObj* %k40514, %struct.ScmObj* %args46944$k40514$2)
ret void
falsebranch$cmp48587:
%stackaddr$prim48591 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim48591, align 8
%stackaddr$makeclosure48592 = alloca %struct.ScmObj*, align 8
%fptrToInt48593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41016 to i64
%ae41016 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48593)
store volatile %struct.ScmObj* %ae41016, %struct.ScmObj** %stackaddr$makeclosure48592, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41016, %struct.ScmObj* %k40514, i64 0)
%args46949$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48594 = alloca %struct.ScmObj*, align 8
%args46949$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args46949$_37length40113$0)
store volatile %struct.ScmObj* %args46949$_37length40113$1, %struct.ScmObj** %stackaddr$prim48594, align 8
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%args46949$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41016, %struct.ScmObj* %args46949$_37length40113$1)
store volatile %struct.ScmObj* %args46949$_37length40113$2, %struct.ScmObj** %stackaddr$prim48595, align 8
%clofunc48596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48596(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args46949$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41016(%struct.ScmObj* %env$ae41016,%struct.ScmObj* %current_45args46945) {
%stackaddr$env-ref48597 = alloca %struct.ScmObj*, align 8
%k40514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41016, i64 0)
store %struct.ScmObj* %k40514, %struct.ScmObj** %stackaddr$env-ref48597
%stackaddr$prim48598 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46945)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim48598, align 8
%stackaddr$prim48599 = alloca %struct.ScmObj*, align 8
%current_45args46946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46945)
store volatile %struct.ScmObj* %current_45args46946, %struct.ScmObj** %stackaddr$prim48599, align 8
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46946)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim48600, align 8
%ae41018 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%cpsprim40516 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41018, %struct.ScmObj* %anf_45bind40254)
store volatile %struct.ScmObj* %cpsprim40516, %struct.ScmObj** %stackaddr$prim48601, align 8
%ae41021 = call %struct.ScmObj* @const_init_int(i64 0)
%args46948$k40514$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48602 = alloca %struct.ScmObj*, align 8
%args46948$k40514$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40516, %struct.ScmObj* %args46948$k40514$0)
store volatile %struct.ScmObj* %args46948$k40514$1, %struct.ScmObj** %stackaddr$prim48602, align 8
%stackaddr$prim48603 = alloca %struct.ScmObj*, align 8
%args46948$k40514$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41021, %struct.ScmObj* %args46948$k40514$1)
store volatile %struct.ScmObj* %args46948$k40514$2, %struct.ScmObj** %stackaddr$prim48603, align 8
%clofunc48604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40514)
musttail call tailcc void %clofunc48604(%struct.ScmObj* %k40514, %struct.ScmObj* %args46948$k40514$2)
ret void
}

define tailcc void @proc_clo$ae40849(%struct.ScmObj* %env$ae40849,%struct.ScmObj* %current_45args46953) {
%stackaddr$prim48605 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46953)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim48605, align 8
%stackaddr$prim48606 = alloca %struct.ScmObj*, align 8
%current_45args46954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46953)
store volatile %struct.ScmObj* %current_45args46954, %struct.ScmObj** %stackaddr$prim48606, align 8
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46954)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim48607, align 8
%ae40851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48608 = alloca %struct.ScmObj*, align 8
%fptrToInt48609 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40852 to i64
%ae40852 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48609)
store volatile %struct.ScmObj* %ae40852, %struct.ScmObj** %stackaddr$makeclosure48608, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40852, %struct.ScmObj* %_37take40116, i64 0)
%args46967$k40517$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48610 = alloca %struct.ScmObj*, align 8
%args46967$k40517$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40852, %struct.ScmObj* %args46967$k40517$0)
store volatile %struct.ScmObj* %args46967$k40517$1, %struct.ScmObj** %stackaddr$prim48610, align 8
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%args46967$k40517$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40851, %struct.ScmObj* %args46967$k40517$1)
store volatile %struct.ScmObj* %args46967$k40517$2, %struct.ScmObj** %stackaddr$prim48611, align 8
%clofunc48612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40517)
musttail call tailcc void %clofunc48612(%struct.ScmObj* %k40517, %struct.ScmObj* %args46967$k40517$2)
ret void
}

define tailcc void @proc_clo$ae40852(%struct.ScmObj* %env$ae40852,%struct.ScmObj* %current_45args46956) {
%stackaddr$env-ref48613 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40852, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48613
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46956)
store volatile %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$prim48614, align 8
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%current_45args46957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46956)
store volatile %struct.ScmObj* %current_45args46957, %struct.ScmObj** %stackaddr$prim48615, align 8
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46957)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim48616, align 8
%stackaddr$prim48617 = alloca %struct.ScmObj*, align 8
%current_45args46958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46957)
store volatile %struct.ScmObj* %current_45args46958, %struct.ScmObj** %stackaddr$prim48617, align 8
%stackaddr$prim48618 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46958)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim48618, align 8
%ae40854 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48619 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40854)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim48619, align 8
%truthy$cmp48620 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40245)
%cmp$cmp48620 = icmp eq i64 %truthy$cmp48620, 1
br i1 %cmp$cmp48620, label %truebranch$cmp48620, label %falsebranch$cmp48620
truebranch$cmp48620:
%ae40857 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40858 = call %struct.ScmObj* @const_init_null()
%args46960$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48621 = alloca %struct.ScmObj*, align 8
%args46960$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40858, %struct.ScmObj* %args46960$k40518$0)
store volatile %struct.ScmObj* %args46960$k40518$1, %struct.ScmObj** %stackaddr$prim48621, align 8
%stackaddr$prim48622 = alloca %struct.ScmObj*, align 8
%args46960$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40857, %struct.ScmObj* %args46960$k40518$1)
store volatile %struct.ScmObj* %args46960$k40518$2, %struct.ScmObj** %stackaddr$prim48622, align 8
%clofunc48623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc48623(%struct.ScmObj* %k40518, %struct.ScmObj* %args46960$k40518$2)
ret void
falsebranch$cmp48620:
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48624, align 8
%truthy$cmp48625 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40246)
%cmp$cmp48625 = icmp eq i64 %truthy$cmp48625, 1
br i1 %cmp$cmp48625, label %truebranch$cmp48625, label %falsebranch$cmp48625
truebranch$cmp48625:
%ae40868 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40869 = call %struct.ScmObj* @const_init_null()
%args46961$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48626 = alloca %struct.ScmObj*, align 8
%args46961$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40869, %struct.ScmObj* %args46961$k40518$0)
store volatile %struct.ScmObj* %args46961$k40518$1, %struct.ScmObj** %stackaddr$prim48626, align 8
%stackaddr$prim48627 = alloca %struct.ScmObj*, align 8
%args46961$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40868, %struct.ScmObj* %args46961$k40518$1)
store volatile %struct.ScmObj* %args46961$k40518$2, %struct.ScmObj** %stackaddr$prim48627, align 8
%clofunc48628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc48628(%struct.ScmObj* %k40518, %struct.ScmObj* %args46961$k40518$2)
ret void
falsebranch$cmp48625:
%stackaddr$prim48629 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48629, align 8
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48630, align 8
%ae40879 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40879)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim48631, align 8
%stackaddr$makeclosure48632 = alloca %struct.ScmObj*, align 8
%fptrToInt48633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40881 to i64
%ae40881 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48633)
store volatile %struct.ScmObj* %ae40881, %struct.ScmObj** %stackaddr$makeclosure48632, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40881, %struct.ScmObj* %anf_45bind40247, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40881, %struct.ScmObj* %k40518, i64 1)
%args46966$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48634 = alloca %struct.ScmObj*, align 8
%args46966$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args46966$_37take40116$0)
store volatile %struct.ScmObj* %args46966$_37take40116$1, %struct.ScmObj** %stackaddr$prim48634, align 8
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%args46966$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %args46966$_37take40116$1)
store volatile %struct.ScmObj* %args46966$_37take40116$2, %struct.ScmObj** %stackaddr$prim48635, align 8
%stackaddr$prim48636 = alloca %struct.ScmObj*, align 8
%args46966$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40881, %struct.ScmObj* %args46966$_37take40116$2)
store volatile %struct.ScmObj* %args46966$_37take40116$3, %struct.ScmObj** %stackaddr$prim48636, align 8
%clofunc48637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48637(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args46966$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40881(%struct.ScmObj* %env$ae40881,%struct.ScmObj* %current_45args46962) {
%stackaddr$env-ref48638 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40881, i64 0)
store %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$env-ref48638
%stackaddr$env-ref48639 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40881, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref48639
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46962)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim48640, align 8
%stackaddr$prim48641 = alloca %struct.ScmObj*, align 8
%current_45args46963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46962)
store volatile %struct.ScmObj* %current_45args46963, %struct.ScmObj** %stackaddr$prim48641, align 8
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46963)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48642, align 8
%stackaddr$prim48643 = alloca %struct.ScmObj*, align 8
%cpsprim40520 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %anf_45bind40250)
store volatile %struct.ScmObj* %cpsprim40520, %struct.ScmObj** %stackaddr$prim48643, align 8
%ae40887 = call %struct.ScmObj* @const_init_int(i64 0)
%args46965$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%args46965$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40520, %struct.ScmObj* %args46965$k40518$0)
store volatile %struct.ScmObj* %args46965$k40518$1, %struct.ScmObj** %stackaddr$prim48644, align 8
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%args46965$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40887, %struct.ScmObj* %args46965$k40518$1)
store volatile %struct.ScmObj* %args46965$k40518$2, %struct.ScmObj** %stackaddr$prim48645, align 8
%clofunc48646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc48646(%struct.ScmObj* %k40518, %struct.ScmObj* %args46965$k40518$2)
ret void
}

define tailcc void @proc_clo$ae40752(%struct.ScmObj* %env$ae40752,%struct.ScmObj* %current_45args46970) {
%stackaddr$prim48647 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46970)
store volatile %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$prim48647, align 8
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%current_45args46971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46970)
store volatile %struct.ScmObj* %current_45args46971, %struct.ScmObj** %stackaddr$prim48648, align 8
%stackaddr$prim48649 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46971)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim48649, align 8
%ae40754 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48650 = alloca %struct.ScmObj*, align 8
%fptrToInt48651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40755 to i64
%ae40755 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48651)
store volatile %struct.ScmObj* %ae40755, %struct.ScmObj** %stackaddr$makeclosure48650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40755, %struct.ScmObj* %_37map40120, i64 0)
%args46987$k40521$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48652 = alloca %struct.ScmObj*, align 8
%args46987$k40521$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40755, %struct.ScmObj* %args46987$k40521$0)
store volatile %struct.ScmObj* %args46987$k40521$1, %struct.ScmObj** %stackaddr$prim48652, align 8
%stackaddr$prim48653 = alloca %struct.ScmObj*, align 8
%args46987$k40521$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40754, %struct.ScmObj* %args46987$k40521$1)
store volatile %struct.ScmObj* %args46987$k40521$2, %struct.ScmObj** %stackaddr$prim48653, align 8
%clofunc48654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40521)
musttail call tailcc void %clofunc48654(%struct.ScmObj* %k40521, %struct.ScmObj* %args46987$k40521$2)
ret void
}

define tailcc void @proc_clo$ae40755(%struct.ScmObj* %env$ae40755,%struct.ScmObj* %current_45args46973) {
%stackaddr$env-ref48655 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40755, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48655
%stackaddr$prim48656 = alloca %struct.ScmObj*, align 8
%k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46973)
store volatile %struct.ScmObj* %k40522, %struct.ScmObj** %stackaddr$prim48656, align 8
%stackaddr$prim48657 = alloca %struct.ScmObj*, align 8
%current_45args46974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46973)
store volatile %struct.ScmObj* %current_45args46974, %struct.ScmObj** %stackaddr$prim48657, align 8
%stackaddr$prim48658 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46974)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim48658, align 8
%stackaddr$prim48659 = alloca %struct.ScmObj*, align 8
%current_45args46975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46974)
store volatile %struct.ScmObj* %current_45args46975, %struct.ScmObj** %stackaddr$prim48659, align 8
%stackaddr$prim48660 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46975)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim48660, align 8
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim48661, align 8
%truthy$cmp48662 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40239)
%cmp$cmp48662 = icmp eq i64 %truthy$cmp48662, 1
br i1 %cmp$cmp48662, label %truebranch$cmp48662, label %falsebranch$cmp48662
truebranch$cmp48662:
%ae40759 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40760 = call %struct.ScmObj* @const_init_null()
%args46977$k40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%args46977$k40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40760, %struct.ScmObj* %args46977$k40522$0)
store volatile %struct.ScmObj* %args46977$k40522$1, %struct.ScmObj** %stackaddr$prim48663, align 8
%stackaddr$prim48664 = alloca %struct.ScmObj*, align 8
%args46977$k40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40759, %struct.ScmObj* %args46977$k40522$1)
store volatile %struct.ScmObj* %args46977$k40522$2, %struct.ScmObj** %stackaddr$prim48664, align 8
%clofunc48665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40522)
musttail call tailcc void %clofunc48665(%struct.ScmObj* %k40522, %struct.ScmObj* %args46977$k40522$2)
ret void
falsebranch$cmp48662:
%stackaddr$prim48666 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48666, align 8
%stackaddr$makeclosure48667 = alloca %struct.ScmObj*, align 8
%fptrToInt48668 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40769 to i64
%ae40769 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48668)
store volatile %struct.ScmObj* %ae40769, %struct.ScmObj** %stackaddr$makeclosure48667, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40769, %struct.ScmObj* %k40522, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40769, %struct.ScmObj* %f40122, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40769, %struct.ScmObj* %lst40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40769, %struct.ScmObj* %_37map40120, i64 3)
%args46986$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48669 = alloca %struct.ScmObj*, align 8
%args46986$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %args46986$f40122$0)
store volatile %struct.ScmObj* %args46986$f40122$1, %struct.ScmObj** %stackaddr$prim48669, align 8
%stackaddr$prim48670 = alloca %struct.ScmObj*, align 8
%args46986$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40769, %struct.ScmObj* %args46986$f40122$1)
store volatile %struct.ScmObj* %args46986$f40122$2, %struct.ScmObj** %stackaddr$prim48670, align 8
%clofunc48671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc48671(%struct.ScmObj* %f40122, %struct.ScmObj* %args46986$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40769(%struct.ScmObj* %env$ae40769,%struct.ScmObj* %current_45args46978) {
%stackaddr$env-ref48672 = alloca %struct.ScmObj*, align 8
%k40522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40769, i64 0)
store %struct.ScmObj* %k40522, %struct.ScmObj** %stackaddr$env-ref48672
%stackaddr$env-ref48673 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40769, i64 1)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref48673
%stackaddr$env-ref48674 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40769, i64 2)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref48674
%stackaddr$env-ref48675 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40769, i64 3)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48675
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%_95k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46978)
store volatile %struct.ScmObj* %_95k40523, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%current_45args46979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46978)
store volatile %struct.ScmObj* %current_45args46979, %struct.ScmObj** %stackaddr$prim48677, align 8
%stackaddr$prim48678 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46979)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48678, align 8
%stackaddr$prim48679 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim48679, align 8
%stackaddr$makeclosure48680 = alloca %struct.ScmObj*, align 8
%fptrToInt48681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40773 to i64
%ae40773 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48681)
store volatile %struct.ScmObj* %ae40773, %struct.ScmObj** %stackaddr$makeclosure48680, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40773, %struct.ScmObj* %anf_45bind40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40773, %struct.ScmObj* %k40522, i64 1)
%args46985$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48682 = alloca %struct.ScmObj*, align 8
%args46985$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args46985$_37map40120$0)
store volatile %struct.ScmObj* %args46985$_37map40120$1, %struct.ScmObj** %stackaddr$prim48682, align 8
%stackaddr$prim48683 = alloca %struct.ScmObj*, align 8
%args46985$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args46985$_37map40120$1)
store volatile %struct.ScmObj* %args46985$_37map40120$2, %struct.ScmObj** %stackaddr$prim48683, align 8
%stackaddr$prim48684 = alloca %struct.ScmObj*, align 8
%args46985$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40773, %struct.ScmObj* %args46985$_37map40120$2)
store volatile %struct.ScmObj* %args46985$_37map40120$3, %struct.ScmObj** %stackaddr$prim48684, align 8
%clofunc48685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc48685(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args46985$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40773(%struct.ScmObj* %env$ae40773,%struct.ScmObj* %current_45args46981) {
%stackaddr$env-ref48686 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40773, i64 0)
store %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$env-ref48686
%stackaddr$env-ref48687 = alloca %struct.ScmObj*, align 8
%k40522 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40773, i64 1)
store %struct.ScmObj* %k40522, %struct.ScmObj** %stackaddr$env-ref48687
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%_95k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46981)
store volatile %struct.ScmObj* %_95k40524, %struct.ScmObj** %stackaddr$prim48688, align 8
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%current_45args46982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46981)
store volatile %struct.ScmObj* %current_45args46982, %struct.ScmObj** %stackaddr$prim48689, align 8
%stackaddr$prim48690 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46982)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim48690, align 8
%stackaddr$prim48691 = alloca %struct.ScmObj*, align 8
%cpsprim40525 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %anf_45bind40243)
store volatile %struct.ScmObj* %cpsprim40525, %struct.ScmObj** %stackaddr$prim48691, align 8
%ae40779 = call %struct.ScmObj* @const_init_int(i64 0)
%args46984$k40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48692 = alloca %struct.ScmObj*, align 8
%args46984$k40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40525, %struct.ScmObj* %args46984$k40522$0)
store volatile %struct.ScmObj* %args46984$k40522$1, %struct.ScmObj** %stackaddr$prim48692, align 8
%stackaddr$prim48693 = alloca %struct.ScmObj*, align 8
%args46984$k40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40779, %struct.ScmObj* %args46984$k40522$1)
store volatile %struct.ScmObj* %args46984$k40522$2, %struct.ScmObj** %stackaddr$prim48693, align 8
%clofunc48694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40522)
musttail call tailcc void %clofunc48694(%struct.ScmObj* %k40522, %struct.ScmObj* %args46984$k40522$2)
ret void
}

define tailcc void @proc_clo$ae40672(%struct.ScmObj* %env$ae40672,%struct.ScmObj* %current_45args46990) {
%stackaddr$prim48695 = alloca %struct.ScmObj*, align 8
%k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46990)
store volatile %struct.ScmObj* %k40526, %struct.ScmObj** %stackaddr$prim48695, align 8
%stackaddr$prim48696 = alloca %struct.ScmObj*, align 8
%current_45args46991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46990)
store volatile %struct.ScmObj* %current_45args46991, %struct.ScmObj** %stackaddr$prim48696, align 8
%stackaddr$prim48697 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46991)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim48697, align 8
%ae40674 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48698 = alloca %struct.ScmObj*, align 8
%fptrToInt48699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40675 to i64
%ae40675 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48699)
store volatile %struct.ScmObj* %ae40675, %struct.ScmObj** %stackaddr$makeclosure48698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40675, %struct.ScmObj* %_37foldr140124, i64 0)
%args47004$k40526$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48700 = alloca %struct.ScmObj*, align 8
%args47004$k40526$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40675, %struct.ScmObj* %args47004$k40526$0)
store volatile %struct.ScmObj* %args47004$k40526$1, %struct.ScmObj** %stackaddr$prim48700, align 8
%stackaddr$prim48701 = alloca %struct.ScmObj*, align 8
%args47004$k40526$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40674, %struct.ScmObj* %args47004$k40526$1)
store volatile %struct.ScmObj* %args47004$k40526$2, %struct.ScmObj** %stackaddr$prim48701, align 8
%clofunc48702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40526)
musttail call tailcc void %clofunc48702(%struct.ScmObj* %k40526, %struct.ScmObj* %args47004$k40526$2)
ret void
}

define tailcc void @proc_clo$ae40675(%struct.ScmObj* %env$ae40675,%struct.ScmObj* %current_45args46993) {
%stackaddr$env-ref48703 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40675, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48703
%stackaddr$prim48704 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46993)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim48704, align 8
%stackaddr$prim48705 = alloca %struct.ScmObj*, align 8
%current_45args46994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46993)
store volatile %struct.ScmObj* %current_45args46994, %struct.ScmObj** %stackaddr$prim48705, align 8
%stackaddr$prim48706 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46994)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim48706, align 8
%stackaddr$prim48707 = alloca %struct.ScmObj*, align 8
%current_45args46995 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46994)
store volatile %struct.ScmObj* %current_45args46995, %struct.ScmObj** %stackaddr$prim48707, align 8
%stackaddr$prim48708 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46995)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim48708, align 8
%stackaddr$prim48709 = alloca %struct.ScmObj*, align 8
%current_45args46996 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46995)
store volatile %struct.ScmObj* %current_45args46996, %struct.ScmObj** %stackaddr$prim48709, align 8
%stackaddr$prim48710 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46996)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim48710, align 8
%stackaddr$prim48711 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim48711, align 8
%truthy$cmp48712 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40234)
%cmp$cmp48712 = icmp eq i64 %truthy$cmp48712, 1
br i1 %cmp$cmp48712, label %truebranch$cmp48712, label %falsebranch$cmp48712
truebranch$cmp48712:
%ae40679 = call %struct.ScmObj* @const_init_int(i64 0)
%args46998$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48713 = alloca %struct.ScmObj*, align 8
%args46998$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args46998$k40527$0)
store volatile %struct.ScmObj* %args46998$k40527$1, %struct.ScmObj** %stackaddr$prim48713, align 8
%stackaddr$prim48714 = alloca %struct.ScmObj*, align 8
%args46998$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40679, %struct.ScmObj* %args46998$k40527$1)
store volatile %struct.ScmObj* %args46998$k40527$2, %struct.ScmObj** %stackaddr$prim48714, align 8
%clofunc48715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc48715(%struct.ScmObj* %k40527, %struct.ScmObj* %args46998$k40527$2)
ret void
falsebranch$cmp48712:
%stackaddr$prim48716 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48716, align 8
%stackaddr$prim48717 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim48717, align 8
%stackaddr$makeclosure48718 = alloca %struct.ScmObj*, align 8
%fptrToInt48719 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40687 to i64
%ae40687 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48719)
store volatile %struct.ScmObj* %ae40687, %struct.ScmObj** %stackaddr$makeclosure48718, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40687, %struct.ScmObj* %k40527, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40687, %struct.ScmObj* %f40127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40687, %struct.ScmObj* %anf_45bind40235, i64 2)
%args47003$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48720 = alloca %struct.ScmObj*, align 8
%args47003$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args47003$_37foldr140124$0)
store volatile %struct.ScmObj* %args47003$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48720, align 8
%stackaddr$prim48721 = alloca %struct.ScmObj*, align 8
%args47003$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47003$_37foldr140124$1)
store volatile %struct.ScmObj* %args47003$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48721, align 8
%stackaddr$prim48722 = alloca %struct.ScmObj*, align 8
%args47003$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args47003$_37foldr140124$2)
store volatile %struct.ScmObj* %args47003$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48722, align 8
%stackaddr$prim48723 = alloca %struct.ScmObj*, align 8
%args47003$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40687, %struct.ScmObj* %args47003$_37foldr140124$3)
store volatile %struct.ScmObj* %args47003$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48723, align 8
%clofunc48724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48724(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47003$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40687(%struct.ScmObj* %env$ae40687,%struct.ScmObj* %current_45args46999) {
%stackaddr$env-ref48725 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40687, i64 0)
store %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$env-ref48725
%stackaddr$env-ref48726 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40687, i64 1)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref48726
%stackaddr$env-ref48727 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40687, i64 2)
store %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$env-ref48727
%stackaddr$prim48728 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46999)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim48728, align 8
%stackaddr$prim48729 = alloca %struct.ScmObj*, align 8
%current_45args47000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46999)
store volatile %struct.ScmObj* %current_45args47000, %struct.ScmObj** %stackaddr$prim48729, align 8
%stackaddr$prim48730 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47000)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim48730, align 8
%args47002$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%args47002$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40237, %struct.ScmObj* %args47002$f40127$0)
store volatile %struct.ScmObj* %args47002$f40127$1, %struct.ScmObj** %stackaddr$prim48731, align 8
%stackaddr$prim48732 = alloca %struct.ScmObj*, align 8
%args47002$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args47002$f40127$1)
store volatile %struct.ScmObj* %args47002$f40127$2, %struct.ScmObj** %stackaddr$prim48732, align 8
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%args47002$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40527, %struct.ScmObj* %args47002$f40127$2)
store volatile %struct.ScmObj* %args47002$f40127$3, %struct.ScmObj** %stackaddr$prim48733, align 8
%clofunc48734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc48734(%struct.ScmObj* %f40127, %struct.ScmObj* %args47002$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40555(%struct.ScmObj* %env$ae40555,%struct.ScmObj* %current_45args47007) {
%stackaddr$prim48735 = alloca %struct.ScmObj*, align 8
%k40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47007)
store volatile %struct.ScmObj* %k40529, %struct.ScmObj** %stackaddr$prim48735, align 8
%stackaddr$prim48736 = alloca %struct.ScmObj*, align 8
%current_45args47008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47007)
store volatile %struct.ScmObj* %current_45args47008, %struct.ScmObj** %stackaddr$prim48736, align 8
%stackaddr$prim48737 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47008)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim48737, align 8
%ae40557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48738 = alloca %struct.ScmObj*, align 8
%fptrToInt48739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40558 to i64
%ae40558 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48739)
store volatile %struct.ScmObj* %ae40558, %struct.ScmObj** %stackaddr$makeclosure48738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40558, %struct.ScmObj* %y40104, i64 0)
%args47026$k40529$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48740 = alloca %struct.ScmObj*, align 8
%args47026$k40529$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40558, %struct.ScmObj* %args47026$k40529$0)
store volatile %struct.ScmObj* %args47026$k40529$1, %struct.ScmObj** %stackaddr$prim48740, align 8
%stackaddr$prim48741 = alloca %struct.ScmObj*, align 8
%args47026$k40529$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40557, %struct.ScmObj* %args47026$k40529$1)
store volatile %struct.ScmObj* %args47026$k40529$2, %struct.ScmObj** %stackaddr$prim48741, align 8
%clofunc48742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40529)
musttail call tailcc void %clofunc48742(%struct.ScmObj* %k40529, %struct.ScmObj* %args47026$k40529$2)
ret void
}

define tailcc void @proc_clo$ae40558(%struct.ScmObj* %env$ae40558,%struct.ScmObj* %current_45args47010) {
%stackaddr$env-ref48743 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40558, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48743
%stackaddr$prim48744 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47010)
store volatile %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$prim48744, align 8
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%current_45args47011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47010)
store volatile %struct.ScmObj* %current_45args47011, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$prim48746 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47011)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim48746, align 8
%stackaddr$makeclosure48747 = alloca %struct.ScmObj*, align 8
%fptrToInt48748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40559 to i64
%ae40559 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48748)
store volatile %struct.ScmObj* %ae40559, %struct.ScmObj** %stackaddr$makeclosure48747, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40559, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40559, %struct.ScmObj* %k40530, i64 1)
%ae40560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48749 = alloca %struct.ScmObj*, align 8
%fptrToInt48750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40561 to i64
%ae40561 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48750)
store volatile %struct.ScmObj* %ae40561, %struct.ScmObj** %stackaddr$makeclosure48749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40561, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40561, %struct.ScmObj* %y40104, i64 1)
%args47025$ae40559$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48751 = alloca %struct.ScmObj*, align 8
%args47025$ae40559$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40561, %struct.ScmObj* %args47025$ae40559$0)
store volatile %struct.ScmObj* %args47025$ae40559$1, %struct.ScmObj** %stackaddr$prim48751, align 8
%stackaddr$prim48752 = alloca %struct.ScmObj*, align 8
%args47025$ae40559$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40560, %struct.ScmObj* %args47025$ae40559$1)
store volatile %struct.ScmObj* %args47025$ae40559$2, %struct.ScmObj** %stackaddr$prim48752, align 8
%clofunc48753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40559)
musttail call tailcc void %clofunc48753(%struct.ScmObj* %ae40559, %struct.ScmObj* %args47025$ae40559$2)
ret void
}

define tailcc void @proc_clo$ae40559(%struct.ScmObj* %env$ae40559,%struct.ScmObj* %current_45args47013) {
%stackaddr$env-ref48754 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40559, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48754
%stackaddr$env-ref48755 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40559, i64 1)
store %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$env-ref48755
%stackaddr$prim48756 = alloca %struct.ScmObj*, align 8
%_95k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47013)
store volatile %struct.ScmObj* %_95k40531, %struct.ScmObj** %stackaddr$prim48756, align 8
%stackaddr$prim48757 = alloca %struct.ScmObj*, align 8
%current_45args47014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47013)
store volatile %struct.ScmObj* %current_45args47014, %struct.ScmObj** %stackaddr$prim48757, align 8
%stackaddr$prim48758 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47014)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim48758, align 8
%args47016$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48759 = alloca %struct.ScmObj*, align 8
%args47016$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40232, %struct.ScmObj* %args47016$f40105$0)
store volatile %struct.ScmObj* %args47016$f40105$1, %struct.ScmObj** %stackaddr$prim48759, align 8
%stackaddr$prim48760 = alloca %struct.ScmObj*, align 8
%args47016$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40530, %struct.ScmObj* %args47016$f40105$1)
store volatile %struct.ScmObj* %args47016$f40105$2, %struct.ScmObj** %stackaddr$prim48760, align 8
%clofunc48761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc48761(%struct.ScmObj* %f40105, %struct.ScmObj* %args47016$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40561(%struct.ScmObj* %env$ae40561,%struct.ScmObj* %args4010640532) {
%stackaddr$env-ref48762 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40561, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48762
%stackaddr$env-ref48763 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40561, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48763
%stackaddr$prim48764 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640532)
store volatile %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$prim48764, align 8
%stackaddr$prim48765 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640532)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim48765, align 8
%stackaddr$makeclosure48766 = alloca %struct.ScmObj*, align 8
%fptrToInt48767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40565 to i64
%ae40565 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48767)
store volatile %struct.ScmObj* %ae40565, %struct.ScmObj** %stackaddr$makeclosure48766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40565, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40565, %struct.ScmObj* %k40533, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40565, %struct.ScmObj* %args40106, i64 2)
%args47024$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48768 = alloca %struct.ScmObj*, align 8
%args47024$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args47024$y40104$0)
store volatile %struct.ScmObj* %args47024$y40104$1, %struct.ScmObj** %stackaddr$prim48768, align 8
%stackaddr$prim48769 = alloca %struct.ScmObj*, align 8
%args47024$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40565, %struct.ScmObj* %args47024$y40104$1)
store volatile %struct.ScmObj* %args47024$y40104$2, %struct.ScmObj** %stackaddr$prim48769, align 8
%clofunc48770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc48770(%struct.ScmObj* %y40104, %struct.ScmObj* %args47024$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40565(%struct.ScmObj* %env$ae40565,%struct.ScmObj* %current_45args47017) {
%stackaddr$env-ref48771 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40565, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48771
%stackaddr$env-ref48772 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40565, i64 1)
store %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$env-ref48772
%stackaddr$env-ref48773 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40565, i64 2)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48773
%stackaddr$prim48774 = alloca %struct.ScmObj*, align 8
%_95k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47017)
store volatile %struct.ScmObj* %_95k40534, %struct.ScmObj** %stackaddr$prim48774, align 8
%stackaddr$prim48775 = alloca %struct.ScmObj*, align 8
%current_45args47018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47017)
store volatile %struct.ScmObj* %current_45args47018, %struct.ScmObj** %stackaddr$prim48775, align 8
%stackaddr$prim48776 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47018)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim48776, align 8
%stackaddr$makeclosure48777 = alloca %struct.ScmObj*, align 8
%fptrToInt48778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40568 to i64
%ae40568 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48778)
store volatile %struct.ScmObj* %ae40568, %struct.ScmObj** %stackaddr$makeclosure48777, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40568, %struct.ScmObj* %k40533, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40568, %struct.ScmObj* %args40106, i64 1)
%args47023$anf_45bind40230$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%args47023$anf_45bind40230$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args47023$anf_45bind40230$0)
store volatile %struct.ScmObj* %args47023$anf_45bind40230$1, %struct.ScmObj** %stackaddr$prim48779, align 8
%stackaddr$prim48780 = alloca %struct.ScmObj*, align 8
%args47023$anf_45bind40230$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40568, %struct.ScmObj* %args47023$anf_45bind40230$1)
store volatile %struct.ScmObj* %args47023$anf_45bind40230$2, %struct.ScmObj** %stackaddr$prim48780, align 8
%clofunc48781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40230)
musttail call tailcc void %clofunc48781(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args47023$anf_45bind40230$2)
ret void
}

define tailcc void @proc_clo$ae40568(%struct.ScmObj* %env$ae40568,%struct.ScmObj* %current_45args47020) {
%stackaddr$env-ref48782 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40568, i64 0)
store %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$env-ref48782
%stackaddr$env-ref48783 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40568, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48783
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%_95k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47020)
store volatile %struct.ScmObj* %_95k40535, %struct.ScmObj** %stackaddr$prim48784, align 8
%stackaddr$prim48785 = alloca %struct.ScmObj*, align 8
%current_45args47021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47020)
store volatile %struct.ScmObj* %current_45args47021, %struct.ScmObj** %stackaddr$prim48785, align 8
%stackaddr$prim48786 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47021)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim48786, align 8
%stackaddr$prim48787 = alloca %struct.ScmObj*, align 8
%cpsargs40536 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40533, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40536, %struct.ScmObj** %stackaddr$prim48787, align 8
%clofunc48788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40231)
musttail call tailcc void %clofunc48788(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %cpsargs40536)
ret void
}

define tailcc void @proc_clo$ae40540(%struct.ScmObj* %env$ae40540,%struct.ScmObj* %current_45args47028) {
%stackaddr$prim48789 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47028)
store volatile %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$prim48789, align 8
%stackaddr$prim48790 = alloca %struct.ScmObj*, align 8
%current_45args47029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47028)
store volatile %struct.ScmObj* %current_45args47029, %struct.ScmObj** %stackaddr$prim48790, align 8
%stackaddr$prim48791 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47029)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim48791, align 8
%args47031$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48792 = alloca %struct.ScmObj*, align 8
%args47031$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47031$yu40103$0)
store volatile %struct.ScmObj* %args47031$yu40103$1, %struct.ScmObj** %stackaddr$prim48792, align 8
%stackaddr$prim48793 = alloca %struct.ScmObj*, align 8
%args47031$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40537, %struct.ScmObj* %args47031$yu40103$1)
store volatile %struct.ScmObj* %args47031$yu40103$2, %struct.ScmObj** %stackaddr$prim48793, align 8
%clofunc48794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc48794(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47031$yu40103$2)
ret void
}