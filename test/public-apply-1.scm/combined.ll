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



define ccc i32 @main() {
%mainenv53785 = call %struct.ScmObj* @const_init_null()
%mainargs53786 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53785, %struct.ScmObj* %mainargs53786)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53783,%struct.ScmObj* %mainargs53784) {
%stackaddr$makeclosure53787 = alloca %struct.ScmObj*, align 8
%fptrToInt53788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47495 to i64
%ae47495 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53788)
store volatile %struct.ScmObj* %ae47495, %struct.ScmObj** %stackaddr$makeclosure53787, align 8
%ae47496 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53789 = alloca %struct.ScmObj*, align 8
%fptrToInt53790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47497 to i64
%ae47497 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53790)
store volatile %struct.ScmObj* %ae47497, %struct.ScmObj** %stackaddr$makeclosure53789, align 8
%argslist53782$ae474950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53791 = alloca %struct.ScmObj*, align 8
%argslist53782$ae474951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47497, %struct.ScmObj* %argslist53782$ae474950)
store volatile %struct.ScmObj* %argslist53782$ae474951, %struct.ScmObj** %stackaddr$prim53791, align 8
%stackaddr$prim53792 = alloca %struct.ScmObj*, align 8
%argslist53782$ae474952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47496, %struct.ScmObj* %argslist53782$ae474951)
store volatile %struct.ScmObj* %argslist53782$ae474952, %struct.ScmObj** %stackaddr$prim53792, align 8
%clofunc53793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47495)
musttail call tailcc void %clofunc53793(%struct.ScmObj* %ae47495, %struct.ScmObj* %argslist53782$ae474952)
ret void
}

define tailcc void @proc_clo$ae47495(%struct.ScmObj* %env$ae47495,%struct.ScmObj* %current_45args53206) {
%stackaddr$prim53794 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim53794, align 8
%stackaddr$prim53795 = alloca %struct.ScmObj*, align 8
%current_45args53207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %current_45args53207, %struct.ScmObj** %stackaddr$prim53795, align 8
%stackaddr$prim53796 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53207)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim53796, align 8
%stackaddr$makeclosure53797 = alloca %struct.ScmObj*, align 8
%fptrToInt53798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47510 to i64
%ae47510 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53798)
store volatile %struct.ScmObj* %ae47510, %struct.ScmObj** %stackaddr$makeclosure53797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47510, %struct.ScmObj* %anf_45bind47193, i64 0)
%ae47511 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53799 = alloca %struct.ScmObj*, align 8
%fptrToInt53800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47512 to i64
%ae47512 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53800)
store volatile %struct.ScmObj* %ae47512, %struct.ScmObj** %stackaddr$makeclosure53799, align 8
%argslist53777$ae475100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53801 = alloca %struct.ScmObj*, align 8
%argslist53777$ae475101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47512, %struct.ScmObj* %argslist53777$ae475100)
store volatile %struct.ScmObj* %argslist53777$ae475101, %struct.ScmObj** %stackaddr$prim53801, align 8
%stackaddr$prim53802 = alloca %struct.ScmObj*, align 8
%argslist53777$ae475102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47511, %struct.ScmObj* %argslist53777$ae475101)
store volatile %struct.ScmObj* %argslist53777$ae475102, %struct.ScmObj** %stackaddr$prim53802, align 8
%clofunc53803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47510)
musttail call tailcc void %clofunc53803(%struct.ScmObj* %ae47510, %struct.ScmObj* %argslist53777$ae475102)
ret void
}

define tailcc void @proc_clo$ae47510(%struct.ScmObj* %env$ae47510,%struct.ScmObj* %current_45args53209) {
%stackaddr$env-ref53804 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47510, i64 0)
store %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$env-ref53804
%stackaddr$prim53805 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim53805, align 8
%stackaddr$prim53806 = alloca %struct.ScmObj*, align 8
%current_45args53210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %current_45args53210, %struct.ScmObj** %stackaddr$prim53806, align 8
%stackaddr$prim53807 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53210)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim53807, align 8
%stackaddr$makeclosure53808 = alloca %struct.ScmObj*, align 8
%fptrToInt53809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47625 to i64
%ae47625 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53809)
store volatile %struct.ScmObj* %ae47625, %struct.ScmObj** %stackaddr$makeclosure53808, align 8
%argslist53756$anf_45bind471930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53810 = alloca %struct.ScmObj*, align 8
%argslist53756$anf_45bind471931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist53756$anf_45bind471930)
store volatile %struct.ScmObj* %argslist53756$anf_45bind471931, %struct.ScmObj** %stackaddr$prim53810, align 8
%stackaddr$prim53811 = alloca %struct.ScmObj*, align 8
%argslist53756$anf_45bind471932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47625, %struct.ScmObj* %argslist53756$anf_45bind471931)
store volatile %struct.ScmObj* %argslist53756$anf_45bind471932, %struct.ScmObj** %stackaddr$prim53811, align 8
%clofunc53812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47193)
musttail call tailcc void %clofunc53812(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %argslist53756$anf_45bind471932)
ret void
}

define tailcc void @proc_clo$ae47625(%struct.ScmObj* %env$ae47625,%struct.ScmObj* %current_45args53212) {
%stackaddr$prim53813 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim53813, align 8
%stackaddr$prim53814 = alloca %struct.ScmObj*, align 8
%current_45args53213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %current_45args53213, %struct.ScmObj** %stackaddr$prim53814, align 8
%stackaddr$prim53815 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53213)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim53815, align 8
%stackaddr$makeclosure53816 = alloca %struct.ScmObj*, align 8
%fptrToInt53817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47627 to i64
%ae47627 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53817)
store volatile %struct.ScmObj* %ae47627, %struct.ScmObj** %stackaddr$makeclosure53816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47627, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47628 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53818 = alloca %struct.ScmObj*, align 8
%fptrToInt53819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47629 to i64
%ae47629 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53819)
store volatile %struct.ScmObj* %ae47629, %struct.ScmObj** %stackaddr$makeclosure53818, align 8
%argslist53755$ae476270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53820 = alloca %struct.ScmObj*, align 8
%argslist53755$ae476271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47629, %struct.ScmObj* %argslist53755$ae476270)
store volatile %struct.ScmObj* %argslist53755$ae476271, %struct.ScmObj** %stackaddr$prim53820, align 8
%stackaddr$prim53821 = alloca %struct.ScmObj*, align 8
%argslist53755$ae476272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47628, %struct.ScmObj* %argslist53755$ae476271)
store volatile %struct.ScmObj* %argslist53755$ae476272, %struct.ScmObj** %stackaddr$prim53821, align 8
%clofunc53822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47627)
musttail call tailcc void %clofunc53822(%struct.ScmObj* %ae47627, %struct.ScmObj* %argslist53755$ae476272)
ret void
}

define tailcc void @proc_clo$ae47627(%struct.ScmObj* %env$ae47627,%struct.ScmObj* %current_45args53215) {
%stackaddr$env-ref53823 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47627, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53823
%stackaddr$prim53824 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim53824, align 8
%stackaddr$prim53825 = alloca %struct.ScmObj*, align 8
%current_45args53216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %current_45args53216, %struct.ScmObj** %stackaddr$prim53825, align 8
%stackaddr$prim53826 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53216)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim53826, align 8
%stackaddr$makeclosure53827 = alloca %struct.ScmObj*, align 8
%fptrToInt53828 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47705 to i64
%ae47705 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53828)
store volatile %struct.ScmObj* %ae47705, %struct.ScmObj** %stackaddr$makeclosure53827, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47705, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist53739$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53829 = alloca %struct.ScmObj*, align 8
%argslist53739$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist53739$Ycmb470680)
store volatile %struct.ScmObj* %argslist53739$Ycmb470681, %struct.ScmObj** %stackaddr$prim53829, align 8
%stackaddr$prim53830 = alloca %struct.ScmObj*, align 8
%argslist53739$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47705, %struct.ScmObj* %argslist53739$Ycmb470681)
store volatile %struct.ScmObj* %argslist53739$Ycmb470682, %struct.ScmObj** %stackaddr$prim53830, align 8
%clofunc53831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53831(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53739$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47705(%struct.ScmObj* %env$ae47705,%struct.ScmObj* %current_45args53218) {
%stackaddr$env-ref53832 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47705, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53832
%stackaddr$prim53833 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53833, align 8
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%current_45args53219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %current_45args53219, %struct.ScmObj** %stackaddr$prim53834, align 8
%stackaddr$prim53835 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53219)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim53835, align 8
%stackaddr$makeclosure53836 = alloca %struct.ScmObj*, align 8
%fptrToInt53837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47707 to i64
%ae47707 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53837)
store volatile %struct.ScmObj* %ae47707, %struct.ScmObj** %stackaddr$makeclosure53836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47707, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47707, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53838 = alloca %struct.ScmObj*, align 8
%fptrToInt53839 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47709 to i64
%ae47709 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53839)
store volatile %struct.ScmObj* %ae47709, %struct.ScmObj** %stackaddr$makeclosure53838, align 8
%argslist53738$ae477070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53840 = alloca %struct.ScmObj*, align 8
%argslist53738$ae477071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47709, %struct.ScmObj* %argslist53738$ae477070)
store volatile %struct.ScmObj* %argslist53738$ae477071, %struct.ScmObj** %stackaddr$prim53840, align 8
%stackaddr$prim53841 = alloca %struct.ScmObj*, align 8
%argslist53738$ae477072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47708, %struct.ScmObj* %argslist53738$ae477071)
store volatile %struct.ScmObj* %argslist53738$ae477072, %struct.ScmObj** %stackaddr$prim53841, align 8
%clofunc53842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47707)
musttail call tailcc void %clofunc53842(%struct.ScmObj* %ae47707, %struct.ScmObj* %argslist53738$ae477072)
ret void
}

define tailcc void @proc_clo$ae47707(%struct.ScmObj* %env$ae47707,%struct.ScmObj* %current_45args53221) {
%stackaddr$env-ref53843 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47707, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53843
%stackaddr$env-ref53844 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47707, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53844
%stackaddr$prim53845 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53845, align 8
%stackaddr$prim53846 = alloca %struct.ScmObj*, align 8
%current_45args53222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %current_45args53222, %struct.ScmObj** %stackaddr$prim53846, align 8
%stackaddr$prim53847 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53222)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim53847, align 8
%stackaddr$makeclosure53848 = alloca %struct.ScmObj*, align 8
%fptrToInt53849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47802 to i64
%ae47802 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53849)
store volatile %struct.ScmObj* %ae47802, %struct.ScmObj** %stackaddr$makeclosure53848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47802, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47802, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist53719$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53850 = alloca %struct.ScmObj*, align 8
%argslist53719$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %argslist53719$Ycmb470680)
store volatile %struct.ScmObj* %argslist53719$Ycmb470681, %struct.ScmObj** %stackaddr$prim53850, align 8
%stackaddr$prim53851 = alloca %struct.ScmObj*, align 8
%argslist53719$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47802, %struct.ScmObj* %argslist53719$Ycmb470681)
store volatile %struct.ScmObj* %argslist53719$Ycmb470682, %struct.ScmObj** %stackaddr$prim53851, align 8
%clofunc53852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53852(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53719$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47802(%struct.ScmObj* %env$ae47802,%struct.ScmObj* %current_45args53224) {
%stackaddr$env-ref53853 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47802, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53853
%stackaddr$env-ref53854 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47802, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53854
%stackaddr$prim53855 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53855, align 8
%stackaddr$prim53856 = alloca %struct.ScmObj*, align 8
%current_45args53225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %current_45args53225, %struct.ScmObj** %stackaddr$prim53856, align 8
%stackaddr$prim53857 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53225)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim53857, align 8
%stackaddr$makeclosure53858 = alloca %struct.ScmObj*, align 8
%fptrToInt53859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47804 to i64
%ae47804 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53859)
store volatile %struct.ScmObj* %ae47804, %struct.ScmObj** %stackaddr$makeclosure53858, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53860 = alloca %struct.ScmObj*, align 8
%fptrToInt53861 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47806 to i64
%ae47806 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53861)
store volatile %struct.ScmObj* %ae47806, %struct.ScmObj** %stackaddr$makeclosure53860, align 8
%argslist53718$ae478040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53862 = alloca %struct.ScmObj*, align 8
%argslist53718$ae478041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47806, %struct.ScmObj* %argslist53718$ae478040)
store volatile %struct.ScmObj* %argslist53718$ae478041, %struct.ScmObj** %stackaddr$prim53862, align 8
%stackaddr$prim53863 = alloca %struct.ScmObj*, align 8
%argslist53718$ae478042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47805, %struct.ScmObj* %argslist53718$ae478041)
store volatile %struct.ScmObj* %argslist53718$ae478042, %struct.ScmObj** %stackaddr$prim53863, align 8
%clofunc53864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47804)
musttail call tailcc void %clofunc53864(%struct.ScmObj* %ae47804, %struct.ScmObj* %argslist53718$ae478042)
ret void
}

define tailcc void @proc_clo$ae47804(%struct.ScmObj* %env$ae47804,%struct.ScmObj* %current_45args53227) {
%stackaddr$env-ref53865 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53865
%stackaddr$env-ref53866 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53866
%stackaddr$env-ref53867 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53867
%stackaddr$prim53868 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53868, align 8
%stackaddr$prim53869 = alloca %struct.ScmObj*, align 8
%current_45args53228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %current_45args53228, %struct.ScmObj** %stackaddr$prim53869, align 8
%stackaddr$prim53870 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53228)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim53870, align 8
%stackaddr$makeclosure53871 = alloca %struct.ScmObj*, align 8
%fptrToInt53872 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47952 to i64
%ae47952 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53872)
store volatile %struct.ScmObj* %ae47952, %struct.ScmObj** %stackaddr$makeclosure53871, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47952, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47952, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47952, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist53702$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53873 = alloca %struct.ScmObj*, align 8
%argslist53702$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist53702$Ycmb470680)
store volatile %struct.ScmObj* %argslist53702$Ycmb470681, %struct.ScmObj** %stackaddr$prim53873, align 8
%stackaddr$prim53874 = alloca %struct.ScmObj*, align 8
%argslist53702$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47952, %struct.ScmObj* %argslist53702$Ycmb470681)
store volatile %struct.ScmObj* %argslist53702$Ycmb470682, %struct.ScmObj** %stackaddr$prim53874, align 8
%clofunc53875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53875(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53702$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47952(%struct.ScmObj* %env$ae47952,%struct.ScmObj* %current_45args53230) {
%stackaddr$env-ref53876 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47952, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53876
%stackaddr$env-ref53877 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47952, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53877
%stackaddr$env-ref53878 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47952, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53878
%stackaddr$prim53879 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53879, align 8
%stackaddr$prim53880 = alloca %struct.ScmObj*, align 8
%current_45args53231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %current_45args53231, %struct.ScmObj** %stackaddr$prim53880, align 8
%stackaddr$prim53881 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53231)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim53881, align 8
%stackaddr$makeclosure53882 = alloca %struct.ScmObj*, align 8
%fptrToInt53883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47954 to i64
%ae47954 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53883)
store volatile %struct.ScmObj* %ae47954, %struct.ScmObj** %stackaddr$makeclosure53882, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47954, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47954, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47954, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47954, %struct.ScmObj* %_37take47081, i64 3)
%ae47955 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53884 = alloca %struct.ScmObj*, align 8
%fptrToInt53885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47956 to i64
%ae47956 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53885)
store volatile %struct.ScmObj* %ae47956, %struct.ScmObj** %stackaddr$makeclosure53884, align 8
%argslist53701$ae479540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53886 = alloca %struct.ScmObj*, align 8
%argslist53701$ae479541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47956, %struct.ScmObj* %argslist53701$ae479540)
store volatile %struct.ScmObj* %argslist53701$ae479541, %struct.ScmObj** %stackaddr$prim53886, align 8
%stackaddr$prim53887 = alloca %struct.ScmObj*, align 8
%argslist53701$ae479542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47955, %struct.ScmObj* %argslist53701$ae479541)
store volatile %struct.ScmObj* %argslist53701$ae479542, %struct.ScmObj** %stackaddr$prim53887, align 8
%clofunc53888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47954)
musttail call tailcc void %clofunc53888(%struct.ScmObj* %ae47954, %struct.ScmObj* %argslist53701$ae479542)
ret void
}

define tailcc void @proc_clo$ae47954(%struct.ScmObj* %env$ae47954,%struct.ScmObj* %current_45args53233) {
%stackaddr$env-ref53889 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47954, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53889
%stackaddr$env-ref53890 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47954, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53890
%stackaddr$env-ref53891 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47954, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53891
%stackaddr$env-ref53892 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47954, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53892
%stackaddr$prim53893 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim53893, align 8
%stackaddr$prim53894 = alloca %struct.ScmObj*, align 8
%current_45args53234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %current_45args53234, %struct.ScmObj** %stackaddr$prim53894, align 8
%stackaddr$prim53895 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53234)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim53895, align 8
%stackaddr$makeclosure53896 = alloca %struct.ScmObj*, align 8
%fptrToInt53897 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48035 to i64
%ae48035 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53897)
store volatile %struct.ScmObj* %ae48035, %struct.ScmObj** %stackaddr$makeclosure53896, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48035, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48035, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48035, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48035, %struct.ScmObj* %_37take47081, i64 3)
%argslist53687$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53898 = alloca %struct.ScmObj*, align 8
%argslist53687$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist53687$Ycmb470680)
store volatile %struct.ScmObj* %argslist53687$Ycmb470681, %struct.ScmObj** %stackaddr$prim53898, align 8
%stackaddr$prim53899 = alloca %struct.ScmObj*, align 8
%argslist53687$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48035, %struct.ScmObj* %argslist53687$Ycmb470681)
store volatile %struct.ScmObj* %argslist53687$Ycmb470682, %struct.ScmObj** %stackaddr$prim53899, align 8
%clofunc53900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53900(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53687$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48035(%struct.ScmObj* %env$ae48035,%struct.ScmObj* %current_45args53236) {
%stackaddr$env-ref53901 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48035, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53901
%stackaddr$env-ref53902 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48035, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53902
%stackaddr$env-ref53903 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48035, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53903
%stackaddr$env-ref53904 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48035, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53904
%stackaddr$prim53905 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim53905, align 8
%stackaddr$prim53906 = alloca %struct.ScmObj*, align 8
%current_45args53237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %current_45args53237, %struct.ScmObj** %stackaddr$prim53906, align 8
%stackaddr$prim53907 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim53907, align 8
%stackaddr$makeclosure53908 = alloca %struct.ScmObj*, align 8
%fptrToInt53909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48037 to i64
%ae48037 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53909)
store volatile %struct.ScmObj* %ae48037, %struct.ScmObj** %stackaddr$makeclosure53908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37take47081, i64 4)
%ae48038 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53910 = alloca %struct.ScmObj*, align 8
%fptrToInt53911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48039 to i64
%ae48039 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53911)
store volatile %struct.ScmObj* %ae48039, %struct.ScmObj** %stackaddr$makeclosure53910, align 8
%argslist53686$ae480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53912 = alloca %struct.ScmObj*, align 8
%argslist53686$ae480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48039, %struct.ScmObj* %argslist53686$ae480370)
store volatile %struct.ScmObj* %argslist53686$ae480371, %struct.ScmObj** %stackaddr$prim53912, align 8
%stackaddr$prim53913 = alloca %struct.ScmObj*, align 8
%argslist53686$ae480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48038, %struct.ScmObj* %argslist53686$ae480371)
store volatile %struct.ScmObj* %argslist53686$ae480372, %struct.ScmObj** %stackaddr$prim53913, align 8
%clofunc53914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48037)
musttail call tailcc void %clofunc53914(%struct.ScmObj* %ae48037, %struct.ScmObj* %argslist53686$ae480372)
ret void
}

define tailcc void @proc_clo$ae48037(%struct.ScmObj* %env$ae48037,%struct.ScmObj* %current_45args53239) {
%stackaddr$env-ref53915 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53915
%stackaddr$env-ref53916 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53916
%stackaddr$env-ref53917 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53917
%stackaddr$env-ref53918 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53918
%stackaddr$env-ref53919 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53919
%stackaddr$prim53920 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim53920, align 8
%stackaddr$prim53921 = alloca %struct.ScmObj*, align 8
%current_45args53240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %current_45args53240, %struct.ScmObj** %stackaddr$prim53921, align 8
%stackaddr$prim53922 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53240)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim53922, align 8
%stackaddr$makeclosure53923 = alloca %struct.ScmObj*, align 8
%fptrToInt53924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48114 to i64
%ae48114 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53924)
store volatile %struct.ScmObj* %ae48114, %struct.ScmObj** %stackaddr$makeclosure53923, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48114, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48114, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48114, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48114, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48114, %struct.ScmObj* %_37take47081, i64 4)
%argslist53670$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53925 = alloca %struct.ScmObj*, align 8
%argslist53670$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist53670$Ycmb470680)
store volatile %struct.ScmObj* %argslist53670$Ycmb470681, %struct.ScmObj** %stackaddr$prim53925, align 8
%stackaddr$prim53926 = alloca %struct.ScmObj*, align 8
%argslist53670$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48114, %struct.ScmObj* %argslist53670$Ycmb470681)
store volatile %struct.ScmObj* %argslist53670$Ycmb470682, %struct.ScmObj** %stackaddr$prim53926, align 8
%clofunc53927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53927(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53670$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48114(%struct.ScmObj* %env$ae48114,%struct.ScmObj* %current_45args53242) {
%stackaddr$env-ref53928 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48114, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53928
%stackaddr$env-ref53929 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48114, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53929
%stackaddr$env-ref53930 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48114, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53930
%stackaddr$env-ref53931 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48114, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53931
%stackaddr$env-ref53932 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48114, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53932
%stackaddr$prim53933 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim53933, align 8
%stackaddr$prim53934 = alloca %struct.ScmObj*, align 8
%current_45args53243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %current_45args53243, %struct.ScmObj** %stackaddr$prim53934, align 8
%stackaddr$prim53935 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53243)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim53935, align 8
%stackaddr$makeclosure53936 = alloca %struct.ScmObj*, align 8
%fptrToInt53937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48116 to i64
%ae48116 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53937)
store volatile %struct.ScmObj* %ae48116, %struct.ScmObj** %stackaddr$makeclosure53936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48116, %struct.ScmObj* %_37take47081, i64 5)
%ae48117 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53938 = alloca %struct.ScmObj*, align 8
%fptrToInt53939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48118 to i64
%ae48118 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53939)
store volatile %struct.ScmObj* %ae48118, %struct.ScmObj** %stackaddr$makeclosure53938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48118, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53669$ae481160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53940 = alloca %struct.ScmObj*, align 8
%argslist53669$ae481161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48118, %struct.ScmObj* %argslist53669$ae481160)
store volatile %struct.ScmObj* %argslist53669$ae481161, %struct.ScmObj** %stackaddr$prim53940, align 8
%stackaddr$prim53941 = alloca %struct.ScmObj*, align 8
%argslist53669$ae481162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48117, %struct.ScmObj* %argslist53669$ae481161)
store volatile %struct.ScmObj* %argslist53669$ae481162, %struct.ScmObj** %stackaddr$prim53941, align 8
%clofunc53942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48116)
musttail call tailcc void %clofunc53942(%struct.ScmObj* %ae48116, %struct.ScmObj* %argslist53669$ae481162)
ret void
}

define tailcc void @proc_clo$ae48116(%struct.ScmObj* %env$ae48116,%struct.ScmObj* %current_45args53245) {
%stackaddr$env-ref53943 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53943
%stackaddr$env-ref53944 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53944
%stackaddr$env-ref53945 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53945
%stackaddr$env-ref53946 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53946
%stackaddr$env-ref53947 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53947
%stackaddr$env-ref53948 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48116, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53948
%stackaddr$prim53949 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim53949, align 8
%stackaddr$prim53950 = alloca %struct.ScmObj*, align 8
%current_45args53246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %current_45args53246, %struct.ScmObj** %stackaddr$prim53950, align 8
%stackaddr$prim53951 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53246)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim53951, align 8
%stackaddr$makeclosure53952 = alloca %struct.ScmObj*, align 8
%fptrToInt53953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48170 to i64
%ae48170 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53953)
store volatile %struct.ScmObj* %ae48170, %struct.ScmObj** %stackaddr$makeclosure53952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37last47111, i64 4)
%ae48171 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53954 = alloca %struct.ScmObj*, align 8
%fptrToInt53955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48172 to i64
%ae48172 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53955)
store volatile %struct.ScmObj* %ae48172, %struct.ScmObj** %stackaddr$makeclosure53954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48172, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48172, %struct.ScmObj* %_37take47081, i64 1)
%argslist53655$ae481700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53956 = alloca %struct.ScmObj*, align 8
%argslist53655$ae481701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48172, %struct.ScmObj* %argslist53655$ae481700)
store volatile %struct.ScmObj* %argslist53655$ae481701, %struct.ScmObj** %stackaddr$prim53956, align 8
%stackaddr$prim53957 = alloca %struct.ScmObj*, align 8
%argslist53655$ae481702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48171, %struct.ScmObj* %argslist53655$ae481701)
store volatile %struct.ScmObj* %argslist53655$ae481702, %struct.ScmObj** %stackaddr$prim53957, align 8
%clofunc53958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48170)
musttail call tailcc void %clofunc53958(%struct.ScmObj* %ae48170, %struct.ScmObj* %argslist53655$ae481702)
ret void
}

define tailcc void @proc_clo$ae48170(%struct.ScmObj* %env$ae48170,%struct.ScmObj* %current_45args53248) {
%stackaddr$env-ref53959 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53959
%stackaddr$env-ref53960 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53960
%stackaddr$env-ref53961 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53961
%stackaddr$env-ref53962 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53962
%stackaddr$env-ref53963 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53963
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim53964, align 8
%stackaddr$prim53965 = alloca %struct.ScmObj*, align 8
%current_45args53249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %current_45args53249, %struct.ScmObj** %stackaddr$prim53965, align 8
%stackaddr$prim53966 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53249)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim53966, align 8
%stackaddr$makeclosure53967 = alloca %struct.ScmObj*, align 8
%fptrToInt53968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48200 to i64
%ae48200 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53968)
store volatile %struct.ScmObj* %ae48200, %struct.ScmObj** %stackaddr$makeclosure53967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48200, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48200, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48200, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48200, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48200, %struct.ScmObj* %_37last47111, i64 4)
%ae48201 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53969 = alloca %struct.ScmObj*, align 8
%fptrToInt53970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48202 to i64
%ae48202 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53970)
store volatile %struct.ScmObj* %ae48202, %struct.ScmObj** %stackaddr$makeclosure53969, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48202, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48202, %struct.ScmObj* %_37map147085, i64 1)
%argslist53645$ae482000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53971 = alloca %struct.ScmObj*, align 8
%argslist53645$ae482001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48202, %struct.ScmObj* %argslist53645$ae482000)
store volatile %struct.ScmObj* %argslist53645$ae482001, %struct.ScmObj** %stackaddr$prim53971, align 8
%stackaddr$prim53972 = alloca %struct.ScmObj*, align 8
%argslist53645$ae482002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48201, %struct.ScmObj* %argslist53645$ae482001)
store volatile %struct.ScmObj* %argslist53645$ae482002, %struct.ScmObj** %stackaddr$prim53972, align 8
%clofunc53973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48200)
musttail call tailcc void %clofunc53973(%struct.ScmObj* %ae48200, %struct.ScmObj* %argslist53645$ae482002)
ret void
}

define tailcc void @proc_clo$ae48200(%struct.ScmObj* %env$ae48200,%struct.ScmObj* %current_45args53251) {
%stackaddr$env-ref53974 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48200, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53974
%stackaddr$env-ref53975 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48200, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53975
%stackaddr$env-ref53976 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48200, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53976
%stackaddr$env-ref53977 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48200, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53977
%stackaddr$env-ref53978 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48200, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53978
%stackaddr$prim53979 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim53979, align 8
%stackaddr$prim53980 = alloca %struct.ScmObj*, align 8
%current_45args53252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %current_45args53252, %struct.ScmObj** %stackaddr$prim53980, align 8
%stackaddr$prim53981 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53252)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim53981, align 8
%stackaddr$makeclosure53982 = alloca %struct.ScmObj*, align 8
%fptrToInt53983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48584 to i64
%ae48584 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53983)
store volatile %struct.ScmObj* %ae48584, %struct.ScmObj** %stackaddr$makeclosure53982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %_37last47111, i64 4)
%argslist53585$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%argslist53585$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist53585$Ycmb470680)
store volatile %struct.ScmObj* %argslist53585$Ycmb470681, %struct.ScmObj** %stackaddr$prim53984, align 8
%stackaddr$prim53985 = alloca %struct.ScmObj*, align 8
%argslist53585$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48584, %struct.ScmObj* %argslist53585$Ycmb470681)
store volatile %struct.ScmObj* %argslist53585$Ycmb470682, %struct.ScmObj** %stackaddr$prim53985, align 8
%clofunc53986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53986(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53585$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48584(%struct.ScmObj* %env$ae48584,%struct.ScmObj* %current_45args53254) {
%stackaddr$env-ref53987 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53987
%stackaddr$env-ref53988 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53988
%stackaddr$env-ref53989 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53989
%stackaddr$env-ref53990 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53990
%stackaddr$env-ref53991 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53991
%stackaddr$prim53992 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim53992, align 8
%stackaddr$prim53993 = alloca %struct.ScmObj*, align 8
%current_45args53255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %current_45args53255, %struct.ScmObj** %stackaddr$prim53993, align 8
%stackaddr$prim53994 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53255)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim53994, align 8
%stackaddr$makeclosure53995 = alloca %struct.ScmObj*, align 8
%fptrToInt53996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48586 to i64
%ae48586 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53996)
store volatile %struct.ScmObj* %ae48586, %struct.ScmObj** %stackaddr$makeclosure53995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48586, %struct.ScmObj* %_37last47111, i64 5)
%ae48587 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53997 = alloca %struct.ScmObj*, align 8
%fptrToInt53998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48588 to i64
%ae48588 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53998)
store volatile %struct.ScmObj* %ae48588, %struct.ScmObj** %stackaddr$makeclosure53997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48588, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist53584$ae485860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53999 = alloca %struct.ScmObj*, align 8
%argslist53584$ae485861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48588, %struct.ScmObj* %argslist53584$ae485860)
store volatile %struct.ScmObj* %argslist53584$ae485861, %struct.ScmObj** %stackaddr$prim53999, align 8
%stackaddr$prim54000 = alloca %struct.ScmObj*, align 8
%argslist53584$ae485862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48587, %struct.ScmObj* %argslist53584$ae485861)
store volatile %struct.ScmObj* %argslist53584$ae485862, %struct.ScmObj** %stackaddr$prim54000, align 8
%clofunc54001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48586)
musttail call tailcc void %clofunc54001(%struct.ScmObj* %ae48586, %struct.ScmObj* %argslist53584$ae485862)
ret void
}

define tailcc void @proc_clo$ae48586(%struct.ScmObj* %env$ae48586,%struct.ScmObj* %current_45args53257) {
%stackaddr$env-ref54002 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54002
%stackaddr$env-ref54003 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54003
%stackaddr$env-ref54004 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54004
%stackaddr$env-ref54005 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54005
%stackaddr$env-ref54006 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54006
%stackaddr$env-ref54007 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48586, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54007
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54008, align 8
%stackaddr$prim54009 = alloca %struct.ScmObj*, align 8
%current_45args53258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %current_45args53258, %struct.ScmObj** %stackaddr$prim54009, align 8
%stackaddr$prim54010 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim54010, align 8
%stackaddr$makeclosure54011 = alloca %struct.ScmObj*, align 8
%fptrToInt54012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48663 to i64
%ae48663 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54012)
store volatile %struct.ScmObj* %ae48663, %struct.ScmObj** %stackaddr$makeclosure54011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48663, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48664 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54013 = alloca %struct.ScmObj*, align 8
%fptrToInt54014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48665 to i64
%ae48665 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54014)
store volatile %struct.ScmObj* %ae48665, %struct.ScmObj** %stackaddr$makeclosure54013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48665, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48665, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48665, %struct.ScmObj* %_37last47111, i64 2)
%argslist53565$ae486630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54015 = alloca %struct.ScmObj*, align 8
%argslist53565$ae486631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48665, %struct.ScmObj* %argslist53565$ae486630)
store volatile %struct.ScmObj* %argslist53565$ae486631, %struct.ScmObj** %stackaddr$prim54015, align 8
%stackaddr$prim54016 = alloca %struct.ScmObj*, align 8
%argslist53565$ae486632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48664, %struct.ScmObj* %argslist53565$ae486631)
store volatile %struct.ScmObj* %argslist53565$ae486632, %struct.ScmObj** %stackaddr$prim54016, align 8
%clofunc54017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48663)
musttail call tailcc void %clofunc54017(%struct.ScmObj* %ae48663, %struct.ScmObj* %argslist53565$ae486632)
ret void
}

define tailcc void @proc_clo$ae48663(%struct.ScmObj* %env$ae48663,%struct.ScmObj* %current_45args53260) {
%stackaddr$env-ref54018 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54018
%stackaddr$env-ref54019 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54019
%stackaddr$env-ref54020 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54020
%stackaddr$env-ref54021 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54021
%stackaddr$env-ref54022 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48663, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54022
%stackaddr$prim54023 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54023, align 8
%stackaddr$prim54024 = alloca %struct.ScmObj*, align 8
%current_45args53261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %current_45args53261, %struct.ScmObj** %stackaddr$prim54024, align 8
%stackaddr$prim54025 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53261)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim54025, align 8
%stackaddr$makeclosure54026 = alloca %struct.ScmObj*, align 8
%fptrToInt54027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48809 to i64
%ae48809 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54027)
store volatile %struct.ScmObj* %ae48809, %struct.ScmObj** %stackaddr$makeclosure54026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48809, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48809, %struct.ScmObj* %Ycmb47068, i64 1)
%ae48810 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54028 = alloca %struct.ScmObj*, align 8
%fptrToInt54029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48811 to i64
%ae48811 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54029)
store volatile %struct.ScmObj* %ae48811, %struct.ScmObj** %stackaddr$makeclosure54028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48811, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48811, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48811, %struct.ScmObj* %_37map147120, i64 2)
%argslist53548$ae488090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54030 = alloca %struct.ScmObj*, align 8
%argslist53548$ae488091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %argslist53548$ae488090)
store volatile %struct.ScmObj* %argslist53548$ae488091, %struct.ScmObj** %stackaddr$prim54030, align 8
%stackaddr$prim54031 = alloca %struct.ScmObj*, align 8
%argslist53548$ae488092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48810, %struct.ScmObj* %argslist53548$ae488091)
store volatile %struct.ScmObj* %argslist53548$ae488092, %struct.ScmObj** %stackaddr$prim54031, align 8
%clofunc54032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48809)
musttail call tailcc void %clofunc54032(%struct.ScmObj* %ae48809, %struct.ScmObj* %argslist53548$ae488092)
ret void
}

define tailcc void @proc_clo$ae48809(%struct.ScmObj* %env$ae48809,%struct.ScmObj* %current_45args53263) {
%stackaddr$env-ref54033 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48809, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54033
%stackaddr$env-ref54034 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48809, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54034
%stackaddr$prim54035 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim54035, align 8
%stackaddr$prim54036 = alloca %struct.ScmObj*, align 8
%current_45args53264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %current_45args53264, %struct.ScmObj** %stackaddr$prim54036, align 8
%stackaddr$prim54037 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53264)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim54037, align 8
%stackaddr$makeclosure54038 = alloca %struct.ScmObj*, align 8
%fptrToInt54039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49201 to i64
%ae49201 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54039)
store volatile %struct.ScmObj* %ae49201, %struct.ScmObj** %stackaddr$makeclosure54038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49201, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53488$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54040 = alloca %struct.ScmObj*, align 8
%argslist53488$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %argslist53488$Ycmb470680)
store volatile %struct.ScmObj* %argslist53488$Ycmb470681, %struct.ScmObj** %stackaddr$prim54040, align 8
%stackaddr$prim54041 = alloca %struct.ScmObj*, align 8
%argslist53488$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49201, %struct.ScmObj* %argslist53488$Ycmb470681)
store volatile %struct.ScmObj* %argslist53488$Ycmb470682, %struct.ScmObj** %stackaddr$prim54041, align 8
%clofunc54042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54042(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53488$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49201(%struct.ScmObj* %env$ae49201,%struct.ScmObj* %current_45args53266) {
%stackaddr$env-ref54043 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49201, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54043
%stackaddr$prim54044 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53266)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim54044, align 8
%stackaddr$prim54045 = alloca %struct.ScmObj*, align 8
%current_45args53267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53266)
store volatile %struct.ScmObj* %current_45args53267, %struct.ScmObj** %stackaddr$prim54045, align 8
%stackaddr$prim54046 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53267)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim54046, align 8
%stackaddr$makeclosure54047 = alloca %struct.ScmObj*, align 8
%fptrToInt54048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49203 to i64
%ae49203 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54048)
store volatile %struct.ScmObj* %ae49203, %struct.ScmObj** %stackaddr$makeclosure54047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49204 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54049 = alloca %struct.ScmObj*, align 8
%fptrToInt54050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49205 to i64
%ae49205 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54050)
store volatile %struct.ScmObj* %ae49205, %struct.ScmObj** %stackaddr$makeclosure54049, align 8
%argslist53487$ae492030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54051 = alloca %struct.ScmObj*, align 8
%argslist53487$ae492031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49205, %struct.ScmObj* %argslist53487$ae492030)
store volatile %struct.ScmObj* %argslist53487$ae492031, %struct.ScmObj** %stackaddr$prim54051, align 8
%stackaddr$prim54052 = alloca %struct.ScmObj*, align 8
%argslist53487$ae492032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist53487$ae492031)
store volatile %struct.ScmObj* %argslist53487$ae492032, %struct.ScmObj** %stackaddr$prim54052, align 8
%clofunc54053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49203)
musttail call tailcc void %clofunc54053(%struct.ScmObj* %ae49203, %struct.ScmObj* %argslist53487$ae492032)
ret void
}

define tailcc void @proc_clo$ae49203(%struct.ScmObj* %env$ae49203,%struct.ScmObj* %current_45args53269) {
%stackaddr$env-ref54054 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54054
%stackaddr$prim54055 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53269)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim54055, align 8
%stackaddr$prim54056 = alloca %struct.ScmObj*, align 8
%current_45args53270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53269)
store volatile %struct.ScmObj* %current_45args53270, %struct.ScmObj** %stackaddr$prim54056, align 8
%stackaddr$prim54057 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53270)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim54057, align 8
%stackaddr$makeclosure54058 = alloca %struct.ScmObj*, align 8
%fptrToInt54059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49227 to i64
%ae49227 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54059)
store volatile %struct.ScmObj* %ae49227, %struct.ScmObj** %stackaddr$makeclosure54058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54060 = alloca %struct.ScmObj*, align 8
%fptrToInt54061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49229 to i64
%ae49229 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54061)
store volatile %struct.ScmObj* %ae49229, %struct.ScmObj** %stackaddr$makeclosure54060, align 8
%argslist53481$ae492270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54062 = alloca %struct.ScmObj*, align 8
%argslist53481$ae492271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49229, %struct.ScmObj* %argslist53481$ae492270)
store volatile %struct.ScmObj* %argslist53481$ae492271, %struct.ScmObj** %stackaddr$prim54062, align 8
%stackaddr$prim54063 = alloca %struct.ScmObj*, align 8
%argslist53481$ae492272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist53481$ae492271)
store volatile %struct.ScmObj* %argslist53481$ae492272, %struct.ScmObj** %stackaddr$prim54063, align 8
%clofunc54064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49227)
musttail call tailcc void %clofunc54064(%struct.ScmObj* %ae49227, %struct.ScmObj* %argslist53481$ae492272)
ret void
}

define tailcc void @proc_clo$ae49227(%struct.ScmObj* %env$ae49227,%struct.ScmObj* %current_45args53272) {
%stackaddr$env-ref54065 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54065
%stackaddr$prim54066 = alloca %struct.ScmObj*, align 8
%_95k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %_95k47336, %struct.ScmObj** %stackaddr$prim54066, align 8
%stackaddr$prim54067 = alloca %struct.ScmObj*, align 8
%current_45args53273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %current_45args53273, %struct.ScmObj** %stackaddr$prim54067, align 8
%stackaddr$prim54068 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53273)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54068, align 8
%ae49251 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49252 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54069 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49251, %struct.ScmObj* %ae49252)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54069, align 8
%stackaddr$makeclosure54070 = alloca %struct.ScmObj*, align 8
%fptrToInt54071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49253 to i64
%ae49253 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54071)
store volatile %struct.ScmObj* %ae49253, %struct.ScmObj** %stackaddr$makeclosure54070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49253, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49253, %struct.ScmObj* %_37append47161, i64 1)
%ae49254 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54072 = alloca %struct.ScmObj*, align 8
%fptrToInt54073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49255 to i64
%ae49255 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54073)
store volatile %struct.ScmObj* %ae49255, %struct.ScmObj** %stackaddr$makeclosure54072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37append47161, i64 0)
%argslist53475$ae492530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54074 = alloca %struct.ScmObj*, align 8
%argslist53475$ae492531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49255, %struct.ScmObj* %argslist53475$ae492530)
store volatile %struct.ScmObj* %argslist53475$ae492531, %struct.ScmObj** %stackaddr$prim54074, align 8
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%argslist53475$ae492532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49254, %struct.ScmObj* %argslist53475$ae492531)
store volatile %struct.ScmObj* %argslist53475$ae492532, %struct.ScmObj** %stackaddr$prim54075, align 8
%clofunc54076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49253)
musttail call tailcc void %clofunc54076(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist53475$ae492532)
ret void
}

define tailcc void @proc_clo$ae49253(%struct.ScmObj* %env$ae49253,%struct.ScmObj* %current_45args53275) {
%stackaddr$env-ref54077 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49253, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54077
%stackaddr$env-ref54078 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49253, i64 1)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54078
%stackaddr$prim54079 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53275)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim54079, align 8
%stackaddr$prim54080 = alloca %struct.ScmObj*, align 8
%current_45args53276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53275)
store volatile %struct.ScmObj* %current_45args53276, %struct.ScmObj** %stackaddr$prim54080, align 8
%stackaddr$prim54081 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53276)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54081, align 8
%ae49321 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54082 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49321, %struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54082, align 8
%ae49324 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49324)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54083, align 8
%stackaddr$makeclosure54084 = alloca %struct.ScmObj*, align 8
%fptrToInt54085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49325 to i64
%ae49325 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54085)
store volatile %struct.ScmObj* %ae49325, %struct.ScmObj** %stackaddr$makeclosure54084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54086 = alloca %struct.ScmObj*, align 8
%fptrToInt54087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49327 to i64
%ae49327 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54087)
store volatile %struct.ScmObj* %ae49327, %struct.ScmObj** %stackaddr$makeclosure54086, align 8
%argslist53464$ae493250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54088 = alloca %struct.ScmObj*, align 8
%argslist53464$ae493251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49327, %struct.ScmObj* %argslist53464$ae493250)
store volatile %struct.ScmObj* %argslist53464$ae493251, %struct.ScmObj** %stackaddr$prim54088, align 8
%stackaddr$prim54089 = alloca %struct.ScmObj*, align 8
%argslist53464$ae493252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49326, %struct.ScmObj* %argslist53464$ae493251)
store volatile %struct.ScmObj* %argslist53464$ae493252, %struct.ScmObj** %stackaddr$prim54089, align 8
%clofunc54090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49325)
musttail call tailcc void %clofunc54090(%struct.ScmObj* %ae49325, %struct.ScmObj* %argslist53464$ae493252)
ret void
}

define tailcc void @proc_clo$ae49325(%struct.ScmObj* %env$ae49325,%struct.ScmObj* %current_45args53278) {
%stackaddr$env-ref54091 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54091
%stackaddr$prim54092 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53278)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim54092, align 8
%stackaddr$prim54093 = alloca %struct.ScmObj*, align 8
%current_45args53279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53278)
store volatile %struct.ScmObj* %current_45args53279, %struct.ScmObj** %stackaddr$prim54093, align 8
%stackaddr$prim54094 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53279)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54094, align 8
%stackaddr$makeclosure54095 = alloca %struct.ScmObj*, align 8
%fptrToInt54096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49741 to i64
%ae49741 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54096)
store volatile %struct.ScmObj* %ae49741, %struct.ScmObj** %stackaddr$makeclosure54095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54097 = alloca %struct.ScmObj*, align 8
%fptrToInt54098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49743 to i64
%ae49743 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54098)
store volatile %struct.ScmObj* %ae49743, %struct.ScmObj** %stackaddr$makeclosure54097, align 8
%argslist53439$ae497410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54099 = alloca %struct.ScmObj*, align 8
%argslist53439$ae497411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49743, %struct.ScmObj* %argslist53439$ae497410)
store volatile %struct.ScmObj* %argslist53439$ae497411, %struct.ScmObj** %stackaddr$prim54099, align 8
%stackaddr$prim54100 = alloca %struct.ScmObj*, align 8
%argslist53439$ae497412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49742, %struct.ScmObj* %argslist53439$ae497411)
store volatile %struct.ScmObj* %argslist53439$ae497412, %struct.ScmObj** %stackaddr$prim54100, align 8
%clofunc54101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49741)
musttail call tailcc void %clofunc54101(%struct.ScmObj* %ae49741, %struct.ScmObj* %argslist53439$ae497412)
ret void
}

define tailcc void @proc_clo$ae49741(%struct.ScmObj* %env$ae49741,%struct.ScmObj* %current_45args53281) {
%stackaddr$env-ref54102 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54102
%stackaddr$prim54103 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53281)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim54103, align 8
%stackaddr$prim54104 = alloca %struct.ScmObj*, align 8
%current_45args53282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53281)
store volatile %struct.ScmObj* %current_45args53282, %struct.ScmObj** %stackaddr$prim54104, align 8
%stackaddr$prim54105 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53282)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54105, align 8
%stackaddr$makeclosure54106 = alloca %struct.ScmObj*, align 8
%fptrToInt54107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50277 to i64
%ae50277 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54107)
store volatile %struct.ScmObj* %ae50277, %struct.ScmObj** %stackaddr$makeclosure54106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %_37foldl147073, i64 0)
%ae50278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54108 = alloca %struct.ScmObj*, align 8
%fptrToInt54109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50279 to i64
%ae50279 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54109)
store volatile %struct.ScmObj* %ae50279, %struct.ScmObj** %stackaddr$makeclosure54108, align 8
%argslist53415$ae502770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54110 = alloca %struct.ScmObj*, align 8
%argslist53415$ae502771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50279, %struct.ScmObj* %argslist53415$ae502770)
store volatile %struct.ScmObj* %argslist53415$ae502771, %struct.ScmObj** %stackaddr$prim54110, align 8
%stackaddr$prim54111 = alloca %struct.ScmObj*, align 8
%argslist53415$ae502772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50278, %struct.ScmObj* %argslist53415$ae502771)
store volatile %struct.ScmObj* %argslist53415$ae502772, %struct.ScmObj** %stackaddr$prim54111, align 8
%clofunc54112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50277)
musttail call tailcc void %clofunc54112(%struct.ScmObj* %ae50277, %struct.ScmObj* %argslist53415$ae502772)
ret void
}

define tailcc void @proc_clo$ae50277(%struct.ScmObj* %env$ae50277,%struct.ScmObj* %current_45args53284) {
%stackaddr$env-ref54113 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54113
%stackaddr$prim54114 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53284)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim54114, align 8
%stackaddr$prim54115 = alloca %struct.ScmObj*, align 8
%current_45args53285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53284)
store volatile %struct.ScmObj* %current_45args53285, %struct.ScmObj** %stackaddr$prim54115, align 8
%stackaddr$prim54116 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53285)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54116, align 8
%stackaddr$makeclosure54117 = alloca %struct.ScmObj*, align 8
%fptrToInt54118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50681 to i64
%ae50681 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54118)
store volatile %struct.ScmObj* %ae50681, %struct.ScmObj** %stackaddr$makeclosure54117, align 8
%ae50682 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54119 = alloca %struct.ScmObj*, align 8
%fptrToInt54120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50683 to i64
%ae50683 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54120)
store volatile %struct.ScmObj* %ae50683, %struct.ScmObj** %stackaddr$makeclosure54119, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50683, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53389$ae506810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54121 = alloca %struct.ScmObj*, align 8
%argslist53389$ae506811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50683, %struct.ScmObj* %argslist53389$ae506810)
store volatile %struct.ScmObj* %argslist53389$ae506811, %struct.ScmObj** %stackaddr$prim54121, align 8
%stackaddr$prim54122 = alloca %struct.ScmObj*, align 8
%argslist53389$ae506812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50682, %struct.ScmObj* %argslist53389$ae506811)
store volatile %struct.ScmObj* %argslist53389$ae506812, %struct.ScmObj** %stackaddr$prim54122, align 8
%clofunc54123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50681)
musttail call tailcc void %clofunc54123(%struct.ScmObj* %ae50681, %struct.ScmObj* %argslist53389$ae506812)
ret void
}

define tailcc void @proc_clo$ae50681(%struct.ScmObj* %env$ae50681,%struct.ScmObj* %current_45args53287) {
%stackaddr$prim54124 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53287)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim54124, align 8
%stackaddr$prim54125 = alloca %struct.ScmObj*, align 8
%current_45args53288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53287)
store volatile %struct.ScmObj* %current_45args53288, %struct.ScmObj** %stackaddr$prim54125, align 8
%stackaddr$prim54126 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53288)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54126, align 8
%stackaddr$makeclosure54127 = alloca %struct.ScmObj*, align 8
%fptrToInt54128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50779 to i64
%ae50779 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54128)
store volatile %struct.ScmObj* %ae50779, %struct.ScmObj** %stackaddr$makeclosure54127, align 8
%ae50780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54129 = alloca %struct.ScmObj*, align 8
%fptrToInt54130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50781 to i64
%ae50781 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54130)
store volatile %struct.ScmObj* %ae50781, %struct.ScmObj** %stackaddr$makeclosure54129, align 8
%argslist53376$ae507790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54131 = alloca %struct.ScmObj*, align 8
%argslist53376$ae507791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50781, %struct.ScmObj* %argslist53376$ae507790)
store volatile %struct.ScmObj* %argslist53376$ae507791, %struct.ScmObj** %stackaddr$prim54131, align 8
%stackaddr$prim54132 = alloca %struct.ScmObj*, align 8
%argslist53376$ae507792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50780, %struct.ScmObj* %argslist53376$ae507791)
store volatile %struct.ScmObj* %argslist53376$ae507792, %struct.ScmObj** %stackaddr$prim54132, align 8
%clofunc54133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50779)
musttail call tailcc void %clofunc54133(%struct.ScmObj* %ae50779, %struct.ScmObj* %argslist53376$ae507792)
ret void
}

define tailcc void @proc_clo$ae50779(%struct.ScmObj* %env$ae50779,%struct.ScmObj* %current_45args53290) {
%stackaddr$prim54134 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54134, align 8
%stackaddr$prim54135 = alloca %struct.ScmObj*, align 8
%current_45args53291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53290)
store volatile %struct.ScmObj* %current_45args53291, %struct.ScmObj** %stackaddr$prim54135, align 8
%stackaddr$prim54136 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54136, align 8
%stackaddr$makeclosure54137 = alloca %struct.ScmObj*, align 8
%fptrToInt54138 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50799 to i64
%ae50799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54138)
store volatile %struct.ScmObj* %ae50799, %struct.ScmObj** %stackaddr$makeclosure54137, align 8
%ae50800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54139 = alloca %struct.ScmObj*, align 8
%fptrToInt54140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50801 to i64
%ae50801 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54140)
store volatile %struct.ScmObj* %ae50801, %struct.ScmObj** %stackaddr$makeclosure54139, align 8
%argslist53371$ae507990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54141 = alloca %struct.ScmObj*, align 8
%argslist53371$ae507991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50801, %struct.ScmObj* %argslist53371$ae507990)
store volatile %struct.ScmObj* %argslist53371$ae507991, %struct.ScmObj** %stackaddr$prim54141, align 8
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%argslist53371$ae507992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50800, %struct.ScmObj* %argslist53371$ae507991)
store volatile %struct.ScmObj* %argslist53371$ae507992, %struct.ScmObj** %stackaddr$prim54142, align 8
%clofunc54143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50799)
musttail call tailcc void %clofunc54143(%struct.ScmObj* %ae50799, %struct.ScmObj* %argslist53371$ae507992)
ret void
}

define tailcc void @proc_clo$ae50799(%struct.ScmObj* %env$ae50799,%struct.ScmObj* %current_45args53293) {
%stackaddr$prim54144 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53293)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54144, align 8
%stackaddr$prim54145 = alloca %struct.ScmObj*, align 8
%current_45args53294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53293)
store volatile %struct.ScmObj* %current_45args53294, %struct.ScmObj** %stackaddr$prim54145, align 8
%stackaddr$prim54146 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53294)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54146, align 8
%stackaddr$makeclosure54147 = alloca %struct.ScmObj*, align 8
%fptrToInt54148 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50821 to i64
%ae50821 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54148)
store volatile %struct.ScmObj* %ae50821, %struct.ScmObj** %stackaddr$makeclosure54147, align 8
%ae50822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54149 = alloca %struct.ScmObj*, align 8
%fptrToInt54150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50823 to i64
%ae50823 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54150)
store volatile %struct.ScmObj* %ae50823, %struct.ScmObj** %stackaddr$makeclosure54149, align 8
%argslist53366$ae508210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54151 = alloca %struct.ScmObj*, align 8
%argslist53366$ae508211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50823, %struct.ScmObj* %argslist53366$ae508210)
store volatile %struct.ScmObj* %argslist53366$ae508211, %struct.ScmObj** %stackaddr$prim54151, align 8
%stackaddr$prim54152 = alloca %struct.ScmObj*, align 8
%argslist53366$ae508212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50822, %struct.ScmObj* %argslist53366$ae508211)
store volatile %struct.ScmObj* %argslist53366$ae508212, %struct.ScmObj** %stackaddr$prim54152, align 8
%clofunc54153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50821)
musttail call tailcc void %clofunc54153(%struct.ScmObj* %ae50821, %struct.ScmObj* %argslist53366$ae508212)
ret void
}

define tailcc void @proc_clo$ae50821(%struct.ScmObj* %env$ae50821,%struct.ScmObj* %current_45args53296) {
%stackaddr$prim54154 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53296)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim54154, align 8
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%current_45args53297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53296)
store volatile %struct.ScmObj* %current_45args53297, %struct.ScmObj** %stackaddr$prim54155, align 8
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53297)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54156, align 8
%stackaddr$makeclosure54157 = alloca %struct.ScmObj*, align 8
%fptrToInt54158 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50845 to i64
%ae50845 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54158)
store volatile %struct.ScmObj* %ae50845, %struct.ScmObj** %stackaddr$makeclosure54157, align 8
%ae50846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54159 = alloca %struct.ScmObj*, align 8
%fptrToInt54160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50847 to i64
%ae50847 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54160)
store volatile %struct.ScmObj* %ae50847, %struct.ScmObj** %stackaddr$makeclosure54159, align 8
%argslist53361$ae508450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54161 = alloca %struct.ScmObj*, align 8
%argslist53361$ae508451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50847, %struct.ScmObj* %argslist53361$ae508450)
store volatile %struct.ScmObj* %argslist53361$ae508451, %struct.ScmObj** %stackaddr$prim54161, align 8
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%argslist53361$ae508452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50846, %struct.ScmObj* %argslist53361$ae508451)
store volatile %struct.ScmObj* %argslist53361$ae508452, %struct.ScmObj** %stackaddr$prim54162, align 8
%clofunc54163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50845)
musttail call tailcc void %clofunc54163(%struct.ScmObj* %ae50845, %struct.ScmObj* %argslist53361$ae508452)
ret void
}

define tailcc void @proc_clo$ae50845(%struct.ScmObj* %env$ae50845,%struct.ScmObj* %current_45args53299) {
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53299)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54164, align 8
%stackaddr$prim54165 = alloca %struct.ScmObj*, align 8
%current_45args53300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53299)
store volatile %struct.ScmObj* %current_45args53300, %struct.ScmObj** %stackaddr$prim54165, align 8
%stackaddr$prim54166 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53300)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54166, align 8
%stackaddr$makeclosure54167 = alloca %struct.ScmObj*, align 8
%fptrToInt54168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50871 to i64
%ae50871 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54168)
store volatile %struct.ScmObj* %ae50871, %struct.ScmObj** %stackaddr$makeclosure54167, align 8
%ae50872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54169 = alloca %struct.ScmObj*, align 8
%fptrToInt54170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50873 to i64
%ae50873 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54170)
store volatile %struct.ScmObj* %ae50873, %struct.ScmObj** %stackaddr$makeclosure54169, align 8
%argslist53356$ae508710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54171 = alloca %struct.ScmObj*, align 8
%argslist53356$ae508711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50873, %struct.ScmObj* %argslist53356$ae508710)
store volatile %struct.ScmObj* %argslist53356$ae508711, %struct.ScmObj** %stackaddr$prim54171, align 8
%stackaddr$prim54172 = alloca %struct.ScmObj*, align 8
%argslist53356$ae508712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50872, %struct.ScmObj* %argslist53356$ae508711)
store volatile %struct.ScmObj* %argslist53356$ae508712, %struct.ScmObj** %stackaddr$prim54172, align 8
%clofunc54173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50871)
musttail call tailcc void %clofunc54173(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist53356$ae508712)
ret void
}

define tailcc void @proc_clo$ae50871(%struct.ScmObj* %env$ae50871,%struct.ScmObj* %current_45args53302) {
%stackaddr$prim54174 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53302)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54174, align 8
%stackaddr$prim54175 = alloca %struct.ScmObj*, align 8
%current_45args53303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53302)
store volatile %struct.ScmObj* %current_45args53303, %struct.ScmObj** %stackaddr$prim54175, align 8
%stackaddr$prim54176 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim54176, align 8
%stackaddr$makeclosure54177 = alloca %struct.ScmObj*, align 8
%fptrToInt54178 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50893 to i64
%ae50893 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54178)
store volatile %struct.ScmObj* %ae50893, %struct.ScmObj** %stackaddr$makeclosure54177, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50893, %struct.ScmObj* %anf_45bind47304, i64 0)
%ae50894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54179 = alloca %struct.ScmObj*, align 8
%fptrToInt54180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50895 to i64
%ae50895 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54180)
store volatile %struct.ScmObj* %ae50895, %struct.ScmObj** %stackaddr$makeclosure54179, align 8
%argslist53350$ae508930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54181 = alloca %struct.ScmObj*, align 8
%argslist53350$ae508931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50895, %struct.ScmObj* %argslist53350$ae508930)
store volatile %struct.ScmObj* %argslist53350$ae508931, %struct.ScmObj** %stackaddr$prim54181, align 8
%stackaddr$prim54182 = alloca %struct.ScmObj*, align 8
%argslist53350$ae508932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50894, %struct.ScmObj* %argslist53350$ae508931)
store volatile %struct.ScmObj* %argslist53350$ae508932, %struct.ScmObj** %stackaddr$prim54182, align 8
%clofunc54183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50893)
musttail call tailcc void %clofunc54183(%struct.ScmObj* %ae50893, %struct.ScmObj* %argslist53350$ae508932)
ret void
}

define tailcc void @proc_clo$ae50893(%struct.ScmObj* %env$ae50893,%struct.ScmObj* %current_45args53305) {
%stackaddr$env-ref54184 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50893, i64 0)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54184
%stackaddr$prim54185 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53305)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim54185, align 8
%stackaddr$prim54186 = alloca %struct.ScmObj*, align 8
%current_45args53306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53305)
store volatile %struct.ScmObj* %current_45args53306, %struct.ScmObj** %stackaddr$prim54186, align 8
%stackaddr$prim54187 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53306)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim54187, align 8
%stackaddr$makeclosure54188 = alloca %struct.ScmObj*, align 8
%fptrToInt54189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50916 to i64
%ae50916 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54189)
store volatile %struct.ScmObj* %ae50916, %struct.ScmObj** %stackaddr$makeclosure54188, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50916, %struct.ScmObj* %anf_45bind47304, i64 0)
%ae50917 = call %struct.ScmObj* @const_init_int(i64 4)
%ae50918 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist53348$anf_45bind473050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54190 = alloca %struct.ScmObj*, align 8
%argslist53348$anf_45bind473051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50918, %struct.ScmObj* %argslist53348$anf_45bind473050)
store volatile %struct.ScmObj* %argslist53348$anf_45bind473051, %struct.ScmObj** %stackaddr$prim54190, align 8
%stackaddr$prim54191 = alloca %struct.ScmObj*, align 8
%argslist53348$anf_45bind473052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50917, %struct.ScmObj* %argslist53348$anf_45bind473051)
store volatile %struct.ScmObj* %argslist53348$anf_45bind473052, %struct.ScmObj** %stackaddr$prim54191, align 8
%stackaddr$prim54192 = alloca %struct.ScmObj*, align 8
%argslist53348$anf_45bind473053 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50916, %struct.ScmObj* %argslist53348$anf_45bind473052)
store volatile %struct.ScmObj* %argslist53348$anf_45bind473053, %struct.ScmObj** %stackaddr$prim54192, align 8
%clofunc54193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47305)
musttail call tailcc void %clofunc54193(%struct.ScmObj* %anf_45bind47305, %struct.ScmObj* %argslist53348$anf_45bind473053)
ret void
}

define tailcc void @proc_clo$ae50916(%struct.ScmObj* %env$ae50916,%struct.ScmObj* %current_45args53308) {
%stackaddr$env-ref54194 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50916, i64 0)
store %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$env-ref54194
%stackaddr$prim54195 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53308)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54195, align 8
%stackaddr$prim54196 = alloca %struct.ScmObj*, align 8
%current_45args53309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53308)
store volatile %struct.ScmObj* %current_45args53309, %struct.ScmObj** %stackaddr$prim54196, align 8
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53309)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim54197, align 8
%stackaddr$makeclosure54198 = alloca %struct.ScmObj*, align 8
%fptrToInt54199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50927 to i64
%ae50927 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54199)
store volatile %struct.ScmObj* %ae50927, %struct.ScmObj** %stackaddr$makeclosure54198, align 8
%stackaddr$prim54200 = alloca %struct.ScmObj*, align 8
%cpsargs47362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50927, %struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %cpsargs47362, %struct.ScmObj** %stackaddr$prim54200, align 8
%clofunc54201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47304)
musttail call tailcc void %clofunc54201(%struct.ScmObj* %anf_45bind47304, %struct.ScmObj* %cpsargs47362)
ret void
}

define tailcc void @proc_clo$ae50927(%struct.ScmObj* %env$ae50927,%struct.ScmObj* %current_45args53311) {
%stackaddr$prim54202 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53311)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim54202, align 8
%stackaddr$prim54203 = alloca %struct.ScmObj*, align 8
%current_45args53312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53311)
store volatile %struct.ScmObj* %current_45args53312, %struct.ScmObj** %stackaddr$prim54203, align 8
%stackaddr$prim54204 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53312)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim54204, align 8
%stackaddr$makeclosure54205 = alloca %struct.ScmObj*, align 8
%fptrToInt54206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50931 to i64
%ae50931 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54206)
store volatile %struct.ScmObj* %ae50931, %struct.ScmObj** %stackaddr$makeclosure54205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50931, %struct.ScmObj* %anf_45bind47307, i64 0)
%ae50932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54207 = alloca %struct.ScmObj*, align 8
%fptrToInt54208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50933 to i64
%ae50933 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54208)
store volatile %struct.ScmObj* %ae50933, %struct.ScmObj** %stackaddr$makeclosure54207, align 8
%argslist53347$ae509310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54209 = alloca %struct.ScmObj*, align 8
%argslist53347$ae509311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50933, %struct.ScmObj* %argslist53347$ae509310)
store volatile %struct.ScmObj* %argslist53347$ae509311, %struct.ScmObj** %stackaddr$prim54209, align 8
%stackaddr$prim54210 = alloca %struct.ScmObj*, align 8
%argslist53347$ae509312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50932, %struct.ScmObj* %argslist53347$ae509311)
store volatile %struct.ScmObj* %argslist53347$ae509312, %struct.ScmObj** %stackaddr$prim54210, align 8
%clofunc54211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50931)
musttail call tailcc void %clofunc54211(%struct.ScmObj* %ae50931, %struct.ScmObj* %argslist53347$ae509312)
ret void
}

define tailcc void @proc_clo$ae50931(%struct.ScmObj* %env$ae50931,%struct.ScmObj* %current_45args53314) {
%stackaddr$env-ref54212 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50931, i64 0)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54212
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53314)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim54213, align 8
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%current_45args53315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53314)
store volatile %struct.ScmObj* %current_45args53315, %struct.ScmObj** %stackaddr$prim54214, align 8
%stackaddr$prim54215 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53315)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim54215, align 8
%stackaddr$makeclosure54216 = alloca %struct.ScmObj*, align 8
%fptrToInt54217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50976 to i64
%ae50976 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54217)
store volatile %struct.ScmObj* %ae50976, %struct.ScmObj** %stackaddr$makeclosure54216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50976, %struct.ScmObj* %anf_45bind47309, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50976, %struct.ScmObj* %anf_45bind47307, i64 1)
%ae50977 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54218 = alloca %struct.ScmObj*, align 8
%fptrToInt54219 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50978 to i64
%ae50978 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54219)
store volatile %struct.ScmObj* %ae50978, %struct.ScmObj** %stackaddr$makeclosure54218, align 8
%argslist53337$ae509760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%argslist53337$ae509761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50978, %struct.ScmObj* %argslist53337$ae509760)
store volatile %struct.ScmObj* %argslist53337$ae509761, %struct.ScmObj** %stackaddr$prim54220, align 8
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%argslist53337$ae509762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50977, %struct.ScmObj* %argslist53337$ae509761)
store volatile %struct.ScmObj* %argslist53337$ae509762, %struct.ScmObj** %stackaddr$prim54221, align 8
%clofunc54222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50976)
musttail call tailcc void %clofunc54222(%struct.ScmObj* %ae50976, %struct.ScmObj* %argslist53337$ae509762)
ret void
}

define tailcc void @proc_clo$ae50976(%struct.ScmObj* %env$ae50976,%struct.ScmObj* %current_45args53317) {
%stackaddr$env-ref54223 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50976, i64 0)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54223
%stackaddr$env-ref54224 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50976, i64 1)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54224
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim54225, align 8
%stackaddr$prim54226 = alloca %struct.ScmObj*, align 8
%current_45args53318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53317)
store volatile %struct.ScmObj* %current_45args53318, %struct.ScmObj** %stackaddr$prim54226, align 8
%stackaddr$prim54227 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim54227, align 8
%stackaddr$makeclosure54228 = alloca %struct.ScmObj*, align 8
%fptrToInt54229 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50999 to i64
%ae50999 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54229)
store volatile %struct.ScmObj* %ae50999, %struct.ScmObj** %stackaddr$makeclosure54228, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50999, %struct.ScmObj* %anf_45bind47309, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50999, %struct.ScmObj* %anf_45bind47307, i64 1)
%ae51000 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist53335$anf_45bind473100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54230 = alloca %struct.ScmObj*, align 8
%argslist53335$anf_45bind473101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51000, %struct.ScmObj* %argslist53335$anf_45bind473100)
store volatile %struct.ScmObj* %argslist53335$anf_45bind473101, %struct.ScmObj** %stackaddr$prim54230, align 8
%stackaddr$prim54231 = alloca %struct.ScmObj*, align 8
%argslist53335$anf_45bind473102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50999, %struct.ScmObj* %argslist53335$anf_45bind473101)
store volatile %struct.ScmObj* %argslist53335$anf_45bind473102, %struct.ScmObj** %stackaddr$prim54231, align 8
%clofunc54232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47310)
musttail call tailcc void %clofunc54232(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist53335$anf_45bind473102)
ret void
}

define tailcc void @proc_clo$ae50999(%struct.ScmObj* %env$ae50999,%struct.ScmObj* %current_45args53320) {
%stackaddr$env-ref54233 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50999, i64 0)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54233
%stackaddr$env-ref54234 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50999, i64 1)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54234
%stackaddr$prim54235 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim54235, align 8
%stackaddr$prim54236 = alloca %struct.ScmObj*, align 8
%current_45args53321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53320)
store volatile %struct.ScmObj* %current_45args53321, %struct.ScmObj** %stackaddr$prim54236, align 8
%stackaddr$prim54237 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53321)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim54237, align 8
%stackaddr$makeclosure54238 = alloca %struct.ScmObj*, align 8
%fptrToInt54239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51004 to i64
%ae51004 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54239)
store volatile %struct.ScmObj* %ae51004, %struct.ScmObj** %stackaddr$makeclosure54238, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51004, %struct.ScmObj* %anf_45bind47307, i64 0)
%stackaddr$prim54240 = alloca %struct.ScmObj*, align 8
%cpsargs47356 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51004, %struct.ScmObj* %anf_45bind47311)
store volatile %struct.ScmObj* %cpsargs47356, %struct.ScmObj** %stackaddr$prim54240, align 8
%clofunc54241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47309)
musttail call tailcc void %clofunc54241(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %cpsargs47356)
ret void
}

define tailcc void @proc_clo$ae51004(%struct.ScmObj* %env$ae51004,%struct.ScmObj* %current_45args53323) {
%stackaddr$env-ref54242 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51004, i64 0)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54242
%stackaddr$prim54243 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54243, align 8
%stackaddr$prim54244 = alloca %struct.ScmObj*, align 8
%current_45args53324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %current_45args53324, %struct.ScmObj** %stackaddr$prim54244, align 8
%stackaddr$prim54245 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53324)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim54245, align 8
%stackaddr$makeclosure54246 = alloca %struct.ScmObj*, align 8
%fptrToInt54247 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51009 to i64
%ae51009 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54247)
store volatile %struct.ScmObj* %ae51009, %struct.ScmObj** %stackaddr$makeclosure54246, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %anf_45bind47307, i64 0)
%ae51010 = call %struct.ScmObj* @const_init_int(i64 5)
%ae51011 = call %struct.ScmObj* @const_init_int(i64 6)
%argslist53334$anf_45bind473120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%argslist53334$anf_45bind473121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51011, %struct.ScmObj* %argslist53334$anf_45bind473120)
store volatile %struct.ScmObj* %argslist53334$anf_45bind473121, %struct.ScmObj** %stackaddr$prim54248, align 8
%stackaddr$prim54249 = alloca %struct.ScmObj*, align 8
%argslist53334$anf_45bind473122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51010, %struct.ScmObj* %argslist53334$anf_45bind473121)
store volatile %struct.ScmObj* %argslist53334$anf_45bind473122, %struct.ScmObj** %stackaddr$prim54249, align 8
%stackaddr$prim54250 = alloca %struct.ScmObj*, align 8
%argslist53334$anf_45bind473123 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51009, %struct.ScmObj* %argslist53334$anf_45bind473122)
store volatile %struct.ScmObj* %argslist53334$anf_45bind473123, %struct.ScmObj** %stackaddr$prim54250, align 8
%clofunc54251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47312)
musttail call tailcc void %clofunc54251(%struct.ScmObj* %anf_45bind47312, %struct.ScmObj* %argslist53334$anf_45bind473123)
ret void
}

define tailcc void @proc_clo$ae51009(%struct.ScmObj* %env$ae51009,%struct.ScmObj* %current_45args53326) {
%stackaddr$env-ref54252 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 0)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54252
%stackaddr$prim54253 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53326)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim54253, align 8
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%current_45args53327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53326)
store volatile %struct.ScmObj* %current_45args53327, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53327)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim54255, align 8
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%cpsprim47355 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %cpsprim47355, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$makeclosure54257 = alloca %struct.ScmObj*, align 8
%fptrToInt54258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51022 to i64
%ae51022 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54258)
store volatile %struct.ScmObj* %ae51022, %struct.ScmObj** %stackaddr$makeclosure54257, align 8
%ae51023 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53333$ae510220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54259 = alloca %struct.ScmObj*, align 8
%argslist53333$ae510221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47355, %struct.ScmObj* %argslist53333$ae510220)
store volatile %struct.ScmObj* %argslist53333$ae510221, %struct.ScmObj** %stackaddr$prim54259, align 8
%stackaddr$prim54260 = alloca %struct.ScmObj*, align 8
%argslist53333$ae510222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51023, %struct.ScmObj* %argslist53333$ae510221)
store volatile %struct.ScmObj* %argslist53333$ae510222, %struct.ScmObj** %stackaddr$prim54260, align 8
%clofunc54261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51022)
musttail call tailcc void %clofunc54261(%struct.ScmObj* %ae51022, %struct.ScmObj* %argslist53333$ae510222)
ret void
}

define tailcc void @proc_clo$ae51022(%struct.ScmObj* %env$ae51022,%struct.ScmObj* %current_45args53329) {
%stackaddr$prim54262 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53329)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54262, align 8
%stackaddr$prim54263 = alloca %struct.ScmObj*, align 8
%current_45args53330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53329)
store volatile %struct.ScmObj* %current_45args53330, %struct.ScmObj** %stackaddr$prim54263, align 8
%stackaddr$prim54264 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53330)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54264, align 8
%stackaddr$prim54265 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54265, align 8
%argslist53332$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54266 = alloca %struct.ScmObj*, align 8
%argslist53332$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53332$k0)
store volatile %struct.ScmObj* %argslist53332$k1, %struct.ScmObj** %stackaddr$prim54266, align 8
%clofunc54267 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54267(%struct.ScmObj* %k, %struct.ScmObj* %argslist53332$k1)
ret void
}

define tailcc void @proc_clo$ae50978(%struct.ScmObj* %env$ae50978,%struct.ScmObj* %lst4719247357) {
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719247357)
store volatile %struct.ScmObj* %k47358, %struct.ScmObj** %stackaddr$prim54268, align 8
%stackaddr$prim54269 = alloca %struct.ScmObj*, align 8
%lst47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719247357)
store volatile %struct.ScmObj* %lst47192, %struct.ScmObj** %stackaddr$prim54269, align 8
%ae50982 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53336$k473580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54270 = alloca %struct.ScmObj*, align 8
%argslist53336$k473581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47192, %struct.ScmObj* %argslist53336$k473580)
store volatile %struct.ScmObj* %argslist53336$k473581, %struct.ScmObj** %stackaddr$prim54270, align 8
%stackaddr$prim54271 = alloca %struct.ScmObj*, align 8
%argslist53336$k473582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50982, %struct.ScmObj* %argslist53336$k473581)
store volatile %struct.ScmObj* %argslist53336$k473582, %struct.ScmObj** %stackaddr$prim54271, align 8
%clofunc54272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47358)
musttail call tailcc void %clofunc54272(%struct.ScmObj* %k47358, %struct.ScmObj* %argslist53336$k473582)
ret void
}

define tailcc void @proc_clo$ae50933(%struct.ScmObj* %env$ae50933,%struct.ScmObj* %current_45args53338) {
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53338)
store volatile %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%current_45args53339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53338)
store volatile %struct.ScmObj* %current_45args53339, %struct.ScmObj** %stackaddr$prim54274, align 8
%stackaddr$prim54275 = alloca %struct.ScmObj*, align 8
%u47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53339)
store volatile %struct.ScmObj* %u47189, %struct.ScmObj** %stackaddr$prim54275, align 8
%ae50935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54276 = alloca %struct.ScmObj*, align 8
%fptrToInt54277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50936 to i64
%ae50936 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54277)
store volatile %struct.ScmObj* %ae50936, %struct.ScmObj** %stackaddr$makeclosure54276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50936, %struct.ScmObj* %u47189, i64 0)
%argslist53346$k473590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54278 = alloca %struct.ScmObj*, align 8
%argslist53346$k473591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50936, %struct.ScmObj* %argslist53346$k473590)
store volatile %struct.ScmObj* %argslist53346$k473591, %struct.ScmObj** %stackaddr$prim54278, align 8
%stackaddr$prim54279 = alloca %struct.ScmObj*, align 8
%argslist53346$k473592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50935, %struct.ScmObj* %argslist53346$k473591)
store volatile %struct.ScmObj* %argslist53346$k473592, %struct.ScmObj** %stackaddr$prim54279, align 8
%clofunc54280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47359)
musttail call tailcc void %clofunc54280(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist53346$k473592)
ret void
}

define tailcc void @proc_clo$ae50936(%struct.ScmObj* %env$ae50936,%struct.ScmObj* %current_45args53341) {
%stackaddr$env-ref54281 = alloca %struct.ScmObj*, align 8
%u47189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50936, i64 0)
store %struct.ScmObj* %u47189, %struct.ScmObj** %stackaddr$env-ref54281
%stackaddr$prim54282 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53341)
store volatile %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$prim54282, align 8
%stackaddr$prim54283 = alloca %struct.ScmObj*, align 8
%current_45args53342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53341)
store volatile %struct.ScmObj* %current_45args53342, %struct.ScmObj** %stackaddr$prim54283, align 8
%stackaddr$prim54284 = alloca %struct.ScmObj*, align 8
%v47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53342)
store volatile %struct.ScmObj* %v47191, %struct.ScmObj** %stackaddr$prim54284, align 8
%stackaddr$prim54285 = alloca %struct.ScmObj*, align 8
%current_45args53343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53342)
store volatile %struct.ScmObj* %current_45args53343, %struct.ScmObj** %stackaddr$prim54285, align 8
%stackaddr$prim54286 = alloca %struct.ScmObj*, align 8
%w47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53343)
store volatile %struct.ScmObj* %w47190, %struct.ScmObj** %stackaddr$prim54286, align 8
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %v47191, %struct.ScmObj* %w47190)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim54287, align 8
%stackaddr$prim54288 = alloca %struct.ScmObj*, align 8
%cpsprim47361 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %u47189, %struct.ScmObj* %anf_45bind47308)
store volatile %struct.ScmObj* %cpsprim47361, %struct.ScmObj** %stackaddr$prim54288, align 8
%ae50942 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53345$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54289 = alloca %struct.ScmObj*, align 8
%argslist53345$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47361, %struct.ScmObj* %argslist53345$k473600)
store volatile %struct.ScmObj* %argslist53345$k473601, %struct.ScmObj** %stackaddr$prim54289, align 8
%stackaddr$prim54290 = alloca %struct.ScmObj*, align 8
%argslist53345$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50942, %struct.ScmObj* %argslist53345$k473601)
store volatile %struct.ScmObj* %argslist53345$k473602, %struct.ScmObj** %stackaddr$prim54290, align 8
%clofunc54291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc54291(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53345$k473602)
ret void
}

define tailcc void @proc_clo$ae50895(%struct.ScmObj* %env$ae50895,%struct.ScmObj* %lst4718847363) {
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4718847363)
store volatile %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$prim54292, align 8
%stackaddr$prim54293 = alloca %struct.ScmObj*, align 8
%lst47188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4718847363)
store volatile %struct.ScmObj* %lst47188, %struct.ScmObj** %stackaddr$prim54293, align 8
%ae50899 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53349$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54294 = alloca %struct.ScmObj*, align 8
%argslist53349$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47188, %struct.ScmObj* %argslist53349$k473640)
store volatile %struct.ScmObj* %argslist53349$k473641, %struct.ScmObj** %stackaddr$prim54294, align 8
%stackaddr$prim54295 = alloca %struct.ScmObj*, align 8
%argslist53349$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50899, %struct.ScmObj* %argslist53349$k473641)
store volatile %struct.ScmObj* %argslist53349$k473642, %struct.ScmObj** %stackaddr$prim54295, align 8
%clofunc54296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54296(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53349$k473642)
ret void
}

define tailcc void @proc_clo$ae50873(%struct.ScmObj* %env$ae50873,%struct.ScmObj* %current_45args53351) {
%stackaddr$prim54297 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53351)
store volatile %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$prim54297, align 8
%stackaddr$prim54298 = alloca %struct.ScmObj*, align 8
%current_45args53352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53351)
store volatile %struct.ScmObj* %current_45args53352, %struct.ScmObj** %stackaddr$prim54298, align 8
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%x47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53352)
store volatile %struct.ScmObj* %x47187, %struct.ScmObj** %stackaddr$prim54299, align 8
%stackaddr$prim54300 = alloca %struct.ScmObj*, align 8
%current_45args53353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53352)
store volatile %struct.ScmObj* %current_45args53353, %struct.ScmObj** %stackaddr$prim54300, align 8
%stackaddr$prim54301 = alloca %struct.ScmObj*, align 8
%y47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53353)
store volatile %struct.ScmObj* %y47186, %struct.ScmObj** %stackaddr$prim54301, align 8
%stackaddr$prim54302 = alloca %struct.ScmObj*, align 8
%cpsprim47366 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %y47186, %struct.ScmObj* %x47187)
store volatile %struct.ScmObj* %cpsprim47366, %struct.ScmObj** %stackaddr$prim54302, align 8
%ae50877 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53355$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54303 = alloca %struct.ScmObj*, align 8
%argslist53355$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47366, %struct.ScmObj* %argslist53355$k473650)
store volatile %struct.ScmObj* %argslist53355$k473651, %struct.ScmObj** %stackaddr$prim54303, align 8
%stackaddr$prim54304 = alloca %struct.ScmObj*, align 8
%argslist53355$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50877, %struct.ScmObj* %argslist53355$k473651)
store volatile %struct.ScmObj* %argslist53355$k473652, %struct.ScmObj** %stackaddr$prim54304, align 8
%clofunc54305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54305(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53355$k473652)
ret void
}

define tailcc void @proc_clo$ae50847(%struct.ScmObj* %env$ae50847,%struct.ScmObj* %current_45args53357) {
%stackaddr$prim54306 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53357)
store volatile %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$prim54306, align 8
%stackaddr$prim54307 = alloca %struct.ScmObj*, align 8
%current_45args53358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53357)
store volatile %struct.ScmObj* %current_45args53358, %struct.ScmObj** %stackaddr$prim54307, align 8
%stackaddr$prim54308 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53358)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim54308, align 8
%stackaddr$prim54309 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim54309, align 8
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim54310, align 8
%stackaddr$prim54311 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim54311, align 8
%stackaddr$prim54312 = alloca %struct.ScmObj*, align 8
%cpsprim47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %cpsprim47368, %struct.ScmObj** %stackaddr$prim54312, align 8
%ae50853 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53360$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54313 = alloca %struct.ScmObj*, align 8
%argslist53360$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47368, %struct.ScmObj* %argslist53360$k473670)
store volatile %struct.ScmObj* %argslist53360$k473671, %struct.ScmObj** %stackaddr$prim54313, align 8
%stackaddr$prim54314 = alloca %struct.ScmObj*, align 8
%argslist53360$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50853, %struct.ScmObj* %argslist53360$k473671)
store volatile %struct.ScmObj* %argslist53360$k473672, %struct.ScmObj** %stackaddr$prim54314, align 8
%clofunc54315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc54315(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist53360$k473672)
ret void
}

define tailcc void @proc_clo$ae50823(%struct.ScmObj* %env$ae50823,%struct.ScmObj* %current_45args53362) {
%stackaddr$prim54316 = alloca %struct.ScmObj*, align 8
%k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %k47369, %struct.ScmObj** %stackaddr$prim54316, align 8
%stackaddr$prim54317 = alloca %struct.ScmObj*, align 8
%current_45args53363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %current_45args53363, %struct.ScmObj** %stackaddr$prim54317, align 8
%stackaddr$prim54318 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53363)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim54318, align 8
%stackaddr$prim54319 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim54319, align 8
%stackaddr$prim54320 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim54320, align 8
%stackaddr$prim54321 = alloca %struct.ScmObj*, align 8
%cpsprim47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %cpsprim47370, %struct.ScmObj** %stackaddr$prim54321, align 8
%ae50828 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53365$k473690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%argslist53365$k473691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47370, %struct.ScmObj* %argslist53365$k473690)
store volatile %struct.ScmObj* %argslist53365$k473691, %struct.ScmObj** %stackaddr$prim54322, align 8
%stackaddr$prim54323 = alloca %struct.ScmObj*, align 8
%argslist53365$k473692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50828, %struct.ScmObj* %argslist53365$k473691)
store volatile %struct.ScmObj* %argslist53365$k473692, %struct.ScmObj** %stackaddr$prim54323, align 8
%clofunc54324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47369)
musttail call tailcc void %clofunc54324(%struct.ScmObj* %k47369, %struct.ScmObj* %argslist53365$k473692)
ret void
}

define tailcc void @proc_clo$ae50801(%struct.ScmObj* %env$ae50801,%struct.ScmObj* %current_45args53367) {
%stackaddr$prim54325 = alloca %struct.ScmObj*, align 8
%k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %k47371, %struct.ScmObj** %stackaddr$prim54325, align 8
%stackaddr$prim54326 = alloca %struct.ScmObj*, align 8
%current_45args53368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %current_45args53368, %struct.ScmObj** %stackaddr$prim54326, align 8
%stackaddr$prim54327 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53368)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim54327, align 8
%stackaddr$prim54328 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54328, align 8
%stackaddr$prim54329 = alloca %struct.ScmObj*, align 8
%cpsprim47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47372, %struct.ScmObj** %stackaddr$prim54329, align 8
%ae50805 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53370$k473710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54330 = alloca %struct.ScmObj*, align 8
%argslist53370$k473711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47372, %struct.ScmObj* %argslist53370$k473710)
store volatile %struct.ScmObj* %argslist53370$k473711, %struct.ScmObj** %stackaddr$prim54330, align 8
%stackaddr$prim54331 = alloca %struct.ScmObj*, align 8
%argslist53370$k473712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50805, %struct.ScmObj* %argslist53370$k473711)
store volatile %struct.ScmObj* %argslist53370$k473712, %struct.ScmObj** %stackaddr$prim54331, align 8
%clofunc54332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47371)
musttail call tailcc void %clofunc54332(%struct.ScmObj* %k47371, %struct.ScmObj* %argslist53370$k473712)
ret void
}

define tailcc void @proc_clo$ae50781(%struct.ScmObj* %env$ae50781,%struct.ScmObj* %current_45args53372) {
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53372)
store volatile %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$prim54333, align 8
%stackaddr$prim54334 = alloca %struct.ScmObj*, align 8
%current_45args53373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53372)
store volatile %struct.ScmObj* %current_45args53373, %struct.ScmObj** %stackaddr$prim54334, align 8
%stackaddr$prim54335 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim54335, align 8
%stackaddr$prim54336 = alloca %struct.ScmObj*, align 8
%cpsprim47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47374, %struct.ScmObj** %stackaddr$prim54336, align 8
%ae50784 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53375$k473730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54337 = alloca %struct.ScmObj*, align 8
%argslist53375$k473731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47374, %struct.ScmObj* %argslist53375$k473730)
store volatile %struct.ScmObj* %argslist53375$k473731, %struct.ScmObj** %stackaddr$prim54337, align 8
%stackaddr$prim54338 = alloca %struct.ScmObj*, align 8
%argslist53375$k473732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50784, %struct.ScmObj* %argslist53375$k473731)
store volatile %struct.ScmObj* %argslist53375$k473732, %struct.ScmObj** %stackaddr$prim54338, align 8
%clofunc54339 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47373)
musttail call tailcc void %clofunc54339(%struct.ScmObj* %k47373, %struct.ScmObj* %argslist53375$k473732)
ret void
}

define tailcc void @proc_clo$ae50683(%struct.ScmObj* %env$ae50683,%struct.ScmObj* %args4713447375) {
%stackaddr$env-ref54340 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50683, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54340
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447375)
store volatile %struct.ScmObj* %k47376, %struct.ScmObj** %stackaddr$prim54341, align 8
%stackaddr$prim54342 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447375)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim54342, align 8
%stackaddr$prim54343 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54343, align 8
%truthy$cmp54344 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp54344 = icmp eq i64 %truthy$cmp54344, 1
br i1 %cmp$cmp54344, label %truebranch$cmp54344, label %falsebranch$cmp54344
truebranch$cmp54344:
%ae50689 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50690 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53377$k473760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54345 = alloca %struct.ScmObj*, align 8
%argslist53377$k473761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50690, %struct.ScmObj* %argslist53377$k473760)
store volatile %struct.ScmObj* %argslist53377$k473761, %struct.ScmObj** %stackaddr$prim54345, align 8
%stackaddr$prim54346 = alloca %struct.ScmObj*, align 8
%argslist53377$k473762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50689, %struct.ScmObj* %argslist53377$k473761)
store volatile %struct.ScmObj* %argslist53377$k473762, %struct.ScmObj** %stackaddr$prim54346, align 8
%clofunc54347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47376)
musttail call tailcc void %clofunc54347(%struct.ScmObj* %k47376, %struct.ScmObj* %argslist53377$k473762)
ret void
falsebranch$cmp54344:
%stackaddr$prim54348 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim54348, align 8
%stackaddr$prim54349 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim54349, align 8
%truthy$cmp54350 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47294)
%cmp$cmp54350 = icmp eq i64 %truthy$cmp54350, 1
br i1 %cmp$cmp54350, label %truebranch$cmp54350, label %falsebranch$cmp54350
truebranch$cmp54350:
%stackaddr$prim54351 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim54351, align 8
%ae50702 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53378$k473760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54352 = alloca %struct.ScmObj*, align 8
%argslist53378$k473761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist53378$k473760)
store volatile %struct.ScmObj* %argslist53378$k473761, %struct.ScmObj** %stackaddr$prim54352, align 8
%stackaddr$prim54353 = alloca %struct.ScmObj*, align 8
%argslist53378$k473762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50702, %struct.ScmObj* %argslist53378$k473761)
store volatile %struct.ScmObj* %argslist53378$k473762, %struct.ScmObj** %stackaddr$prim54353, align 8
%clofunc54354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47376)
musttail call tailcc void %clofunc54354(%struct.ScmObj* %k47376, %struct.ScmObj* %argslist53378$k473762)
ret void
falsebranch$cmp54350:
%stackaddr$makeclosure54355 = alloca %struct.ScmObj*, align 8
%fptrToInt54356 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50707 to i64
%ae50707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54356)
store volatile %struct.ScmObj* %ae50707, %struct.ScmObj** %stackaddr$makeclosure54355, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %k47376, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %args47134, i64 2)
%ae50708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54357 = alloca %struct.ScmObj*, align 8
%fptrToInt54358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50709 to i64
%ae50709 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54358)
store volatile %struct.ScmObj* %ae50709, %struct.ScmObj** %stackaddr$makeclosure54357, align 8
%argslist53388$ae507070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54359 = alloca %struct.ScmObj*, align 8
%argslist53388$ae507071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50709, %struct.ScmObj* %argslist53388$ae507070)
store volatile %struct.ScmObj* %argslist53388$ae507071, %struct.ScmObj** %stackaddr$prim54359, align 8
%stackaddr$prim54360 = alloca %struct.ScmObj*, align 8
%argslist53388$ae507072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50708, %struct.ScmObj* %argslist53388$ae507071)
store volatile %struct.ScmObj* %argslist53388$ae507072, %struct.ScmObj** %stackaddr$prim54360, align 8
%clofunc54361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50707)
musttail call tailcc void %clofunc54361(%struct.ScmObj* %ae50707, %struct.ScmObj* %argslist53388$ae507072)
ret void
}

define tailcc void @proc_clo$ae50707(%struct.ScmObj* %env$ae50707,%struct.ScmObj* %current_45args53379) {
%stackaddr$env-ref54362 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54362
%stackaddr$env-ref54363 = alloca %struct.ScmObj*, align 8
%k47376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 1)
store %struct.ScmObj* %k47376, %struct.ScmObj** %stackaddr$env-ref54363
%stackaddr$env-ref54364 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 2)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref54364
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim54365, align 8
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%current_45args53380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %current_45args53380, %struct.ScmObj** %stackaddr$prim54366, align 8
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53380)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim54367, align 8
%stackaddr$prim54368 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim54368, align 8
%stackaddr$prim54369 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54369, align 8
%argslist53382$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54370 = alloca %struct.ScmObj*, align 8
%argslist53382$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %argslist53382$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53382$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54370, align 8
%stackaddr$prim54371 = alloca %struct.ScmObj*, align 8
%argslist53382$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %argslist53382$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53382$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54371, align 8
%stackaddr$prim54372 = alloca %struct.ScmObj*, align 8
%argslist53382$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %argslist53382$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53382$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54372, align 8
%stackaddr$prim54373 = alloca %struct.ScmObj*, align 8
%argslist53382$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47376, %struct.ScmObj* %argslist53382$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53382$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54373, align 8
%clofunc54374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54374(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53382$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50709(%struct.ScmObj* %env$ae50709,%struct.ScmObj* %current_45args53383) {
%stackaddr$prim54375 = alloca %struct.ScmObj*, align 8
%k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %k47379, %struct.ScmObj** %stackaddr$prim54375, align 8
%stackaddr$prim54376 = alloca %struct.ScmObj*, align 8
%current_45args53384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %current_45args53384, %struct.ScmObj** %stackaddr$prim54376, align 8
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53384)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%current_45args53385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53384)
store volatile %struct.ScmObj* %current_45args53385, %struct.ScmObj** %stackaddr$prim54378, align 8
%stackaddr$prim54379 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim54379, align 8
%stackaddr$prim54380 = alloca %struct.ScmObj*, align 8
%cpsprim47380 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47380, %struct.ScmObj** %stackaddr$prim54380, align 8
%ae50713 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53387$k473790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54381 = alloca %struct.ScmObj*, align 8
%argslist53387$k473791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47380, %struct.ScmObj* %argslist53387$k473790)
store volatile %struct.ScmObj* %argslist53387$k473791, %struct.ScmObj** %stackaddr$prim54381, align 8
%stackaddr$prim54382 = alloca %struct.ScmObj*, align 8
%argslist53387$k473792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50713, %struct.ScmObj* %argslist53387$k473791)
store volatile %struct.ScmObj* %argslist53387$k473792, %struct.ScmObj** %stackaddr$prim54382, align 8
%clofunc54383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47379)
musttail call tailcc void %clofunc54383(%struct.ScmObj* %k47379, %struct.ScmObj* %argslist53387$k473792)
ret void
}

define tailcc void @proc_clo$ae50279(%struct.ScmObj* %env$ae50279,%struct.ScmObj* %current_45args53390) {
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53390)
store volatile %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%current_45args53391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53390)
store volatile %struct.ScmObj* %current_45args53391, %struct.ScmObj** %stackaddr$prim54385, align 8
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim54386, align 8
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%current_45args53392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %current_45args53392, %struct.ScmObj** %stackaddr$prim54387, align 8
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim54388, align 8
%ae50280 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50280, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim54389, align 8
%stackaddr$makeclosure54390 = alloca %struct.ScmObj*, align 8
%fptrToInt54391 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50282 to i64
%ae50282 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54391)
store volatile %struct.ScmObj* %ae50282, %struct.ScmObj** %stackaddr$makeclosure54390, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %v47139, i64 2)
%ae50283 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54392 = alloca %struct.ScmObj*, align 8
%fptrToInt54393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50284 to i64
%ae50284 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54393)
store volatile %struct.ScmObj* %ae50284, %struct.ScmObj** %stackaddr$makeclosure54392, align 8
%argslist53414$ae502820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%argslist53414$ae502821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50284, %struct.ScmObj* %argslist53414$ae502820)
store volatile %struct.ScmObj* %argslist53414$ae502821, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%argslist53414$ae502822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50283, %struct.ScmObj* %argslist53414$ae502821)
store volatile %struct.ScmObj* %argslist53414$ae502822, %struct.ScmObj** %stackaddr$prim54395, align 8
%clofunc54396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50282)
musttail call tailcc void %clofunc54396(%struct.ScmObj* %ae50282, %struct.ScmObj* %argslist53414$ae502822)
ret void
}

define tailcc void @proc_clo$ae50282(%struct.ScmObj* %env$ae50282,%struct.ScmObj* %current_45args53394) {
%stackaddr$env-ref54397 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref54397
%stackaddr$env-ref54398 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54398
%stackaddr$env-ref54399 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54399
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53394)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%current_45args53395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53394)
store volatile %struct.ScmObj* %current_45args53395, %struct.ScmObj** %stackaddr$prim54401, align 8
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53395)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54402, align 8
%stackaddr$makeclosure54403 = alloca %struct.ScmObj*, align 8
%fptrToInt54404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50298 to i64
%ae50298 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54404)
store volatile %struct.ScmObj* %ae50298, %struct.ScmObj** %stackaddr$makeclosure54403, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50298, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50298, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50298, %struct.ScmObj* %v47139, i64 2)
%stackaddr$makeclosure54405 = alloca %struct.ScmObj*, align 8
%fptrToInt54406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50299 to i64
%ae50299 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54406)
store volatile %struct.ScmObj* %ae50299, %struct.ScmObj** %stackaddr$makeclosure54405, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50299, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50299, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50299, %struct.ScmObj* %v47139, i64 2)
%argslist53409$anf_45bind472840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%argslist53409$anf_45bind472841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50299, %struct.ScmObj* %argslist53409$anf_45bind472840)
store volatile %struct.ScmObj* %argslist53409$anf_45bind472841, %struct.ScmObj** %stackaddr$prim54407, align 8
%stackaddr$prim54408 = alloca %struct.ScmObj*, align 8
%argslist53409$anf_45bind472842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50298, %struct.ScmObj* %argslist53409$anf_45bind472841)
store volatile %struct.ScmObj* %argslist53409$anf_45bind472842, %struct.ScmObj** %stackaddr$prim54408, align 8
%clofunc54409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47284)
musttail call tailcc void %clofunc54409(%struct.ScmObj* %anf_45bind47284, %struct.ScmObj* %argslist53409$anf_45bind472842)
ret void
}

define tailcc void @proc_clo$ae50298(%struct.ScmObj* %env$ae50298,%struct.ScmObj* %current_45args53397) {
%stackaddr$env-ref54410 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50298, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref54410
%stackaddr$env-ref54411 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50298, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54411
%stackaddr$env-ref54412 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50298, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54412
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53397)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim54413, align 8
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%current_45args53398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53397)
store volatile %struct.ScmObj* %current_45args53398, %struct.ScmObj** %stackaddr$prim54414, align 8
%stackaddr$prim54415 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53398)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54415, align 8
%ae50407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50407)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54416, align 8
%stackaddr$prim54417 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54417, align 8
%truthy$cmp54418 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47286)
%cmp$cmp54418 = icmp eq i64 %truthy$cmp54418, 1
br i1 %cmp$cmp54418, label %truebranch$cmp54418, label %falsebranch$cmp54418
truebranch$cmp54418:
%ae50411 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50412 = call %struct.ScmObj* @const_init_false()
%argslist53400$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%argslist53400$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50412, %struct.ScmObj* %argslist53400$k473810)
store volatile %struct.ScmObj* %argslist53400$k473811, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist53400$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50411, %struct.ScmObj* %argslist53400$k473811)
store volatile %struct.ScmObj* %argslist53400$k473812, %struct.ScmObj** %stackaddr$prim54420, align 8
%clofunc54421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc54421(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53400$k473812)
ret void
falsebranch$cmp54418:
%ae50420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50420)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54422, align 8
%stackaddr$prim54423 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54423, align 8
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47288, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54424, align 8
%truthy$cmp54425 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47289)
%cmp$cmp54425 = icmp eq i64 %truthy$cmp54425, 1
br i1 %cmp$cmp54425, label %truebranch$cmp54425, label %falsebranch$cmp54425
truebranch$cmp54425:
%ae50426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%cpsprim47384 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50426)
store volatile %struct.ScmObj* %cpsprim47384, %struct.ScmObj** %stackaddr$prim54426, align 8
%ae50428 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53401$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%argslist53401$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47384, %struct.ScmObj* %argslist53401$k473810)
store volatile %struct.ScmObj* %argslist53401$k473811, %struct.ScmObj** %stackaddr$prim54427, align 8
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%argslist53401$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50428, %struct.ScmObj* %argslist53401$k473811)
store volatile %struct.ScmObj* %argslist53401$k473812, %struct.ScmObj** %stackaddr$prim54428, align 8
%clofunc54429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc54429(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53401$k473812)
ret void
falsebranch$cmp54425:
%ae50439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54430 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50439)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54430, align 8
%stackaddr$prim54431 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54431, align 8
%ae50442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50442, %struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54432, align 8
%argslist53402$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%argslist53402$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53402$cc471410)
store volatile %struct.ScmObj* %argslist53402$cc471411, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%argslist53402$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53402$cc471411)
store volatile %struct.ScmObj* %argslist53402$cc471412, %struct.ScmObj** %stackaddr$prim54434, align 8
%clofunc54435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54435(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53402$cc471412)
ret void
}

define tailcc void @proc_clo$ae50299(%struct.ScmObj* %env$ae50299,%struct.ScmObj* %current_45args53403) {
%stackaddr$env-ref54436 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50299, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref54436
%stackaddr$env-ref54437 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50299, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54437
%stackaddr$env-ref54438 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50299, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54438
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%current_45args53404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53403)
store volatile %struct.ScmObj* %current_45args53404, %struct.ScmObj** %stackaddr$prim54440, align 8
%stackaddr$prim54441 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53404)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54441, align 8
%ae50301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54442 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50301)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54442, align 8
%stackaddr$prim54443 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54443, align 8
%truthy$cmp54444 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47286)
%cmp$cmp54444 = icmp eq i64 %truthy$cmp54444, 1
br i1 %cmp$cmp54444, label %truebranch$cmp54444, label %falsebranch$cmp54444
truebranch$cmp54444:
%ae50305 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50306 = call %struct.ScmObj* @const_init_false()
%argslist53406$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%argslist53406$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50306, %struct.ScmObj* %argslist53406$k473810)
store volatile %struct.ScmObj* %argslist53406$k473811, %struct.ScmObj** %stackaddr$prim54445, align 8
%stackaddr$prim54446 = alloca %struct.ScmObj*, align 8
%argslist53406$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50305, %struct.ScmObj* %argslist53406$k473811)
store volatile %struct.ScmObj* %argslist53406$k473812, %struct.ScmObj** %stackaddr$prim54446, align 8
%clofunc54447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc54447(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53406$k473812)
ret void
falsebranch$cmp54444:
%ae50314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54448 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50314)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54448, align 8
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54449, align 8
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47288, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54450, align 8
%truthy$cmp54451 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47289)
%cmp$cmp54451 = icmp eq i64 %truthy$cmp54451, 1
br i1 %cmp$cmp54451, label %truebranch$cmp54451, label %falsebranch$cmp54451
truebranch$cmp54451:
%ae50320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54452 = alloca %struct.ScmObj*, align 8
%cpsprim47384 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50320)
store volatile %struct.ScmObj* %cpsprim47384, %struct.ScmObj** %stackaddr$prim54452, align 8
%ae50322 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53407$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54453 = alloca %struct.ScmObj*, align 8
%argslist53407$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47384, %struct.ScmObj* %argslist53407$k473810)
store volatile %struct.ScmObj* %argslist53407$k473811, %struct.ScmObj** %stackaddr$prim54453, align 8
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%argslist53407$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50322, %struct.ScmObj* %argslist53407$k473811)
store volatile %struct.ScmObj* %argslist53407$k473812, %struct.ScmObj** %stackaddr$prim54454, align 8
%clofunc54455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc54455(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53407$k473812)
ret void
falsebranch$cmp54451:
%ae50333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50333)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54456, align 8
%stackaddr$prim54457 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54457, align 8
%ae50336 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50336, %struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54458, align 8
%argslist53408$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%argslist53408$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53408$cc471410)
store volatile %struct.ScmObj* %argslist53408$cc471411, %struct.ScmObj** %stackaddr$prim54459, align 8
%stackaddr$prim54460 = alloca %struct.ScmObj*, align 8
%argslist53408$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53408$cc471411)
store volatile %struct.ScmObj* %argslist53408$cc471412, %struct.ScmObj** %stackaddr$prim54460, align 8
%clofunc54461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54461(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53408$cc471412)
ret void
}

define tailcc void @proc_clo$ae50284(%struct.ScmObj* %env$ae50284,%struct.ScmObj* %current_45args53410) {
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53410)
store volatile %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$prim54462, align 8
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%current_45args53411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53410)
store volatile %struct.ScmObj* %current_45args53411, %struct.ScmObj** %stackaddr$prim54463, align 8
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53411)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim54464, align 8
%argslist53413$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54465 = alloca %struct.ScmObj*, align 8
%argslist53413$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53413$u471420)
store volatile %struct.ScmObj* %argslist53413$u471421, %struct.ScmObj** %stackaddr$prim54465, align 8
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%argslist53413$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist53413$u471421)
store volatile %struct.ScmObj* %argslist53413$u471422, %struct.ScmObj** %stackaddr$prim54466, align 8
%clofunc54467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc54467(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53413$u471422)
ret void
}

define tailcc void @proc_clo$ae49743(%struct.ScmObj* %env$ae49743,%struct.ScmObj* %current_45args53416) {
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53416)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%current_45args53417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53416)
store volatile %struct.ScmObj* %current_45args53417, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim54470, align 8
%stackaddr$prim54471 = alloca %struct.ScmObj*, align 8
%current_45args53418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %current_45args53418, %struct.ScmObj** %stackaddr$prim54471, align 8
%stackaddr$prim54472 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53418)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim54472, align 8
%ae49744 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54473 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49744, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim54473, align 8
%ae49746 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49746, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54474, align 8
%stackaddr$makeclosure54475 = alloca %struct.ScmObj*, align 8
%fptrToInt54476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49748 to i64
%ae49748 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54476)
store volatile %struct.ScmObj* %ae49748, %struct.ScmObj** %stackaddr$makeclosure54475, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %k47386, i64 2)
%ae49749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54477 = alloca %struct.ScmObj*, align 8
%fptrToInt54478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49750 to i64
%ae49750 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54478)
store volatile %struct.ScmObj* %ae49750, %struct.ScmObj** %stackaddr$makeclosure54477, align 8
%argslist53438$ae497480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%argslist53438$ae497481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49750, %struct.ScmObj* %argslist53438$ae497480)
store volatile %struct.ScmObj* %argslist53438$ae497481, %struct.ScmObj** %stackaddr$prim54479, align 8
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%argslist53438$ae497482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49749, %struct.ScmObj* %argslist53438$ae497481)
store volatile %struct.ScmObj* %argslist53438$ae497482, %struct.ScmObj** %stackaddr$prim54480, align 8
%clofunc54481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49748)
musttail call tailcc void %clofunc54481(%struct.ScmObj* %ae49748, %struct.ScmObj* %argslist53438$ae497482)
ret void
}

define tailcc void @proc_clo$ae49748(%struct.ScmObj* %env$ae49748,%struct.ScmObj* %current_45args53420) {
%stackaddr$env-ref54482 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54482
%stackaddr$env-ref54483 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54483
%stackaddr$env-ref54484 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 2)
store %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$env-ref54484
%stackaddr$prim54485 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53420)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim54485, align 8
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%current_45args53421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53420)
store volatile %struct.ScmObj* %current_45args53421, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54487, align 8
%stackaddr$makeclosure54488 = alloca %struct.ScmObj*, align 8
%fptrToInt54489 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49764 to i64
%ae49764 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54489)
store volatile %struct.ScmObj* %ae49764, %struct.ScmObj** %stackaddr$makeclosure54488, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49764, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49764, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49764, %struct.ScmObj* %k47386, i64 2)
%stackaddr$makeclosure54490 = alloca %struct.ScmObj*, align 8
%fptrToInt54491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49765 to i64
%ae49765 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54491)
store volatile %struct.ScmObj* %ae49765, %struct.ScmObj** %stackaddr$makeclosure54490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49765, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49765, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49765, %struct.ScmObj* %k47386, i64 2)
%argslist53433$anf_45bind472770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%argslist53433$anf_45bind472771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49765, %struct.ScmObj* %argslist53433$anf_45bind472770)
store volatile %struct.ScmObj* %argslist53433$anf_45bind472771, %struct.ScmObj** %stackaddr$prim54492, align 8
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%argslist53433$anf_45bind472772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49764, %struct.ScmObj* %argslist53433$anf_45bind472771)
store volatile %struct.ScmObj* %argslist53433$anf_45bind472772, %struct.ScmObj** %stackaddr$prim54493, align 8
%clofunc54494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47277)
musttail call tailcc void %clofunc54494(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %argslist53433$anf_45bind472772)
ret void
}

define tailcc void @proc_clo$ae49764(%struct.ScmObj* %env$ae49764,%struct.ScmObj* %current_45args53423) {
%stackaddr$env-ref54495 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49764, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54495
%stackaddr$env-ref54496 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49764, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54496
%stackaddr$env-ref54497 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49764, i64 2)
store %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$env-ref54497
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%current_45args53424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %current_45args53424, %struct.ScmObj** %stackaddr$prim54499, align 8
%stackaddr$prim54500 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53424)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54500, align 8
%ae49907 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54501 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49907)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54501, align 8
%ae49908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54502 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49908, %struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54502, align 8
%truthy$cmp54503 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47279)
%cmp$cmp54503 = icmp eq i64 %truthy$cmp54503, 1
br i1 %cmp$cmp54503, label %truebranch$cmp54503, label %falsebranch$cmp54503
truebranch$cmp54503:
%ae49912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49912)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim54504, align 8
%ae49914 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53426$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%argslist53426$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist53426$k473860)
store volatile %struct.ScmObj* %argslist53426$k473861, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%argslist53426$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49914, %struct.ScmObj* %argslist53426$k473861)
store volatile %struct.ScmObj* %argslist53426$k473862, %struct.ScmObj** %stackaddr$prim54506, align 8
%clofunc54507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54507(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53426$k473862)
ret void
falsebranch$cmp54503:
%ae49925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49925)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54508, align 8
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54509, align 8
%ae49928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49928, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54510, align 8
%ae49931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49931)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54511, align 8
%ae49933 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %ae49933)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54512, align 8
%ae49935 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54513 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49935, %struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54513, align 8
%argslist53427$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%argslist53427$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53427$cc471490)
store volatile %struct.ScmObj* %argslist53427$cc471491, %struct.ScmObj** %stackaddr$prim54514, align 8
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%argslist53427$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53427$cc471491)
store volatile %struct.ScmObj* %argslist53427$cc471492, %struct.ScmObj** %stackaddr$prim54515, align 8
%clofunc54516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54516(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53427$cc471492)
ret void
}

define tailcc void @proc_clo$ae49765(%struct.ScmObj* %env$ae49765,%struct.ScmObj* %current_45args53428) {
%stackaddr$env-ref54517 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49765, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54517
%stackaddr$env-ref54518 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49765, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54518
%stackaddr$env-ref54519 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49765, i64 2)
store %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$env-ref54519
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%current_45args53429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %current_45args53429, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53429)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54522, align 8
%ae49767 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54523 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49767)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54523, align 8
%ae49768 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49768, %struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54524, align 8
%truthy$cmp54525 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47279)
%cmp$cmp54525 = icmp eq i64 %truthy$cmp54525, 1
br i1 %cmp$cmp54525, label %truebranch$cmp54525, label %falsebranch$cmp54525
truebranch$cmp54525:
%ae49772 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54526 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49772)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim54526, align 8
%ae49774 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53431$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54527 = alloca %struct.ScmObj*, align 8
%argslist53431$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist53431$k473860)
store volatile %struct.ScmObj* %argslist53431$k473861, %struct.ScmObj** %stackaddr$prim54527, align 8
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%argslist53431$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49774, %struct.ScmObj* %argslist53431$k473861)
store volatile %struct.ScmObj* %argslist53431$k473862, %struct.ScmObj** %stackaddr$prim54528, align 8
%clofunc54529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54529(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53431$k473862)
ret void
falsebranch$cmp54525:
%ae49785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49785)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54530, align 8
%stackaddr$prim54531 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54531, align 8
%ae49788 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49788, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54532, align 8
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49791)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54533, align 8
%ae49793 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %ae49793)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54534, align 8
%ae49795 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49795, %struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54535, align 8
%argslist53432$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%argslist53432$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53432$cc471490)
store volatile %struct.ScmObj* %argslist53432$cc471491, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%argslist53432$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53432$cc471491)
store volatile %struct.ScmObj* %argslist53432$cc471492, %struct.ScmObj** %stackaddr$prim54537, align 8
%clofunc54538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54538(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53432$cc471492)
ret void
}

define tailcc void @proc_clo$ae49750(%struct.ScmObj* %env$ae49750,%struct.ScmObj* %current_45args53434) {
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$prim54539, align 8
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%current_45args53435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %current_45args53435, %struct.ScmObj** %stackaddr$prim54540, align 8
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53435)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim54541, align 8
%argslist53437$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%argslist53437$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53437$u471500)
store volatile %struct.ScmObj* %argslist53437$u471501, %struct.ScmObj** %stackaddr$prim54542, align 8
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%argslist53437$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53437$u471501)
store volatile %struct.ScmObj* %argslist53437$u471502, %struct.ScmObj** %stackaddr$prim54543, align 8
%clofunc54544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc54544(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53437$u471502)
ret void
}

define tailcc void @proc_clo$ae49327(%struct.ScmObj* %env$ae49327,%struct.ScmObj* %current_45args53440) {
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$prim54545, align 8
%stackaddr$prim54546 = alloca %struct.ScmObj*, align 8
%current_45args53441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53440)
store volatile %struct.ScmObj* %current_45args53441, %struct.ScmObj** %stackaddr$prim54546, align 8
%stackaddr$prim54547 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim54547, align 8
%ae49328 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49328, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54548, align 8
%stackaddr$makeclosure54549 = alloca %struct.ScmObj*, align 8
%fptrToInt54550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49330 to i64
%ae49330 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54550)
store volatile %struct.ScmObj* %ae49330, %struct.ScmObj** %stackaddr$makeclosure54549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49330, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49330, %struct.ScmObj* %k47391, i64 1)
%ae49331 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54551 = alloca %struct.ScmObj*, align 8
%fptrToInt54552 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49332 to i64
%ae49332 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54552)
store volatile %struct.ScmObj* %ae49332, %struct.ScmObj** %stackaddr$makeclosure54551, align 8
%argslist53463$ae493300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54553 = alloca %struct.ScmObj*, align 8
%argslist53463$ae493301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49332, %struct.ScmObj* %argslist53463$ae493300)
store volatile %struct.ScmObj* %argslist53463$ae493301, %struct.ScmObj** %stackaddr$prim54553, align 8
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%argslist53463$ae493302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49331, %struct.ScmObj* %argslist53463$ae493301)
store volatile %struct.ScmObj* %argslist53463$ae493302, %struct.ScmObj** %stackaddr$prim54554, align 8
%clofunc54555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49330)
musttail call tailcc void %clofunc54555(%struct.ScmObj* %ae49330, %struct.ScmObj* %argslist53463$ae493302)
ret void
}

define tailcc void @proc_clo$ae49330(%struct.ScmObj* %env$ae49330,%struct.ScmObj* %current_45args53443) {
%stackaddr$env-ref54556 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49330, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54556
%stackaddr$env-ref54557 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49330, i64 1)
store %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$env-ref54557
%stackaddr$prim54558 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53443)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim54558, align 8
%stackaddr$prim54559 = alloca %struct.ScmObj*, align 8
%current_45args53444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53443)
store volatile %struct.ScmObj* %current_45args53444, %struct.ScmObj** %stackaddr$prim54559, align 8
%stackaddr$prim54560 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53444)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54560, align 8
%stackaddr$makeclosure54561 = alloca %struct.ScmObj*, align 8
%fptrToInt54562 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49349 to i64
%ae49349 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54562)
store volatile %struct.ScmObj* %ae49349, %struct.ScmObj** %stackaddr$makeclosure54561, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49349, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49349, %struct.ScmObj* %k47391, i64 1)
%stackaddr$makeclosure54563 = alloca %struct.ScmObj*, align 8
%fptrToInt54564 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49350 to i64
%ae49350 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54564)
store volatile %struct.ScmObj* %ae49350, %struct.ScmObj** %stackaddr$makeclosure54563, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49350, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49350, %struct.ScmObj* %k47391, i64 1)
%argslist53458$anf_45bind472690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%argslist53458$anf_45bind472691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49350, %struct.ScmObj* %argslist53458$anf_45bind472690)
store volatile %struct.ScmObj* %argslist53458$anf_45bind472691, %struct.ScmObj** %stackaddr$prim54565, align 8
%stackaddr$prim54566 = alloca %struct.ScmObj*, align 8
%argslist53458$anf_45bind472692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49349, %struct.ScmObj* %argslist53458$anf_45bind472691)
store volatile %struct.ScmObj* %argslist53458$anf_45bind472692, %struct.ScmObj** %stackaddr$prim54566, align 8
%clofunc54567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47269)
musttail call tailcc void %clofunc54567(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist53458$anf_45bind472692)
ret void
}

define tailcc void @proc_clo$ae49349(%struct.ScmObj* %env$ae49349,%struct.ScmObj* %current_45args53446) {
%stackaddr$env-ref54568 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49349, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54568
%stackaddr$env-ref54569 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49349, i64 1)
store %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$env-ref54569
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%_95k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53446)
store volatile %struct.ScmObj* %_95k47393, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%current_45args53447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53446)
store volatile %struct.ScmObj* %current_45args53447, %struct.ScmObj** %stackaddr$prim54571, align 8
%stackaddr$prim54572 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53447)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54572, align 8
%ae49465 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54573 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49465)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54573, align 8
%stackaddr$prim54574 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54574, align 8
%truthy$cmp54575 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54575 = icmp eq i64 %truthy$cmp54575, 1
br i1 %cmp$cmp54575, label %truebranch$cmp54575, label %falsebranch$cmp54575
truebranch$cmp54575:
%ae49469 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49470 = call %struct.ScmObj* @const_init_true()
%argslist53449$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54576 = alloca %struct.ScmObj*, align 8
%argslist53449$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49470, %struct.ScmObj* %argslist53449$k473910)
store volatile %struct.ScmObj* %argslist53449$k473911, %struct.ScmObj** %stackaddr$prim54576, align 8
%stackaddr$prim54577 = alloca %struct.ScmObj*, align 8
%argslist53449$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49469, %struct.ScmObj* %argslist53449$k473911)
store volatile %struct.ScmObj* %argslist53449$k473912, %struct.ScmObj** %stackaddr$prim54577, align 8
%clofunc54578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54578(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53449$k473912)
ret void
falsebranch$cmp54575:
%ae49478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49478)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54579, align 8
%stackaddr$prim54580 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54580, align 8
%truthy$cmp54581 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47273)
%cmp$cmp54581 = icmp eq i64 %truthy$cmp54581, 1
br i1 %cmp$cmp54581, label %truebranch$cmp54581, label %falsebranch$cmp54581
truebranch$cmp54581:
%ae49482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54582 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49482)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54582, align 8
%stackaddr$prim54583 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54583, align 8
%ae49485 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54584 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49485)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54584, align 8
%stackaddr$prim54585 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54585, align 8
%ae49488 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54586 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49488, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54586, align 8
%argslist53450$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54587 = alloca %struct.ScmObj*, align 8
%argslist53450$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53450$cc471560)
store volatile %struct.ScmObj* %argslist53450$cc471561, %struct.ScmObj** %stackaddr$prim54587, align 8
%stackaddr$prim54588 = alloca %struct.ScmObj*, align 8
%argslist53450$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53450$cc471561)
store volatile %struct.ScmObj* %argslist53450$cc471562, %struct.ScmObj** %stackaddr$prim54588, align 8
%clofunc54589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54589(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53450$cc471562)
ret void
falsebranch$cmp54581:
%ae49521 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49522 = call %struct.ScmObj* @const_init_false()
%argslist53451$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%argslist53451$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49522, %struct.ScmObj* %argslist53451$k473910)
store volatile %struct.ScmObj* %argslist53451$k473911, %struct.ScmObj** %stackaddr$prim54590, align 8
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%argslist53451$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49521, %struct.ScmObj* %argslist53451$k473911)
store volatile %struct.ScmObj* %argslist53451$k473912, %struct.ScmObj** %stackaddr$prim54591, align 8
%clofunc54592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54592(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53451$k473912)
ret void
}

define tailcc void @proc_clo$ae49350(%struct.ScmObj* %env$ae49350,%struct.ScmObj* %current_45args53452) {
%stackaddr$env-ref54593 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49350, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54593
%stackaddr$env-ref54594 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49350, i64 1)
store %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$env-ref54594
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%_95k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %_95k47393, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%current_45args53453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53452)
store volatile %struct.ScmObj* %current_45args53453, %struct.ScmObj** %stackaddr$prim54596, align 8
%stackaddr$prim54597 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53453)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54597, align 8
%ae49352 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54598 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49352)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54598, align 8
%stackaddr$prim54599 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54599, align 8
%truthy$cmp54600 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47271)
%cmp$cmp54600 = icmp eq i64 %truthy$cmp54600, 1
br i1 %cmp$cmp54600, label %truebranch$cmp54600, label %falsebranch$cmp54600
truebranch$cmp54600:
%ae49356 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49357 = call %struct.ScmObj* @const_init_true()
%argslist53455$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%argslist53455$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49357, %struct.ScmObj* %argslist53455$k473910)
store volatile %struct.ScmObj* %argslist53455$k473911, %struct.ScmObj** %stackaddr$prim54601, align 8
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%argslist53455$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49356, %struct.ScmObj* %argslist53455$k473911)
store volatile %struct.ScmObj* %argslist53455$k473912, %struct.ScmObj** %stackaddr$prim54602, align 8
%clofunc54603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54603(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53455$k473912)
ret void
falsebranch$cmp54600:
%ae49365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54604 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49365)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54604, align 8
%stackaddr$prim54605 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54605, align 8
%truthy$cmp54606 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47273)
%cmp$cmp54606 = icmp eq i64 %truthy$cmp54606, 1
br i1 %cmp$cmp54606, label %truebranch$cmp54606, label %falsebranch$cmp54606
truebranch$cmp54606:
%ae49369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49369)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54608, align 8
%ae49372 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49372)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54610, align 8
%ae49375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49375, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54611, align 8
%argslist53456$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%argslist53456$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53456$cc471560)
store volatile %struct.ScmObj* %argslist53456$cc471561, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%argslist53456$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53456$cc471561)
store volatile %struct.ScmObj* %argslist53456$cc471562, %struct.ScmObj** %stackaddr$prim54613, align 8
%clofunc54614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54614(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53456$cc471562)
ret void
falsebranch$cmp54606:
%ae49408 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49409 = call %struct.ScmObj* @const_init_false()
%argslist53457$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54615 = alloca %struct.ScmObj*, align 8
%argslist53457$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49409, %struct.ScmObj* %argslist53457$k473910)
store volatile %struct.ScmObj* %argslist53457$k473911, %struct.ScmObj** %stackaddr$prim54615, align 8
%stackaddr$prim54616 = alloca %struct.ScmObj*, align 8
%argslist53457$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49408, %struct.ScmObj* %argslist53457$k473911)
store volatile %struct.ScmObj* %argslist53457$k473912, %struct.ScmObj** %stackaddr$prim54616, align 8
%clofunc54617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc54617(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist53457$k473912)
ret void
}

define tailcc void @proc_clo$ae49332(%struct.ScmObj* %env$ae49332,%struct.ScmObj* %current_45args53459) {
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53459)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim54618, align 8
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%current_45args53460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53459)
store volatile %struct.ScmObj* %current_45args53460, %struct.ScmObj** %stackaddr$prim54619, align 8
%stackaddr$prim54620 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53460)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim54620, align 8
%ae49334 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53462$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54621 = alloca %struct.ScmObj*, align 8
%argslist53462$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist53462$k473940)
store volatile %struct.ScmObj* %argslist53462$k473941, %struct.ScmObj** %stackaddr$prim54621, align 8
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%argslist53462$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49334, %struct.ScmObj* %argslist53462$k473941)
store volatile %struct.ScmObj* %argslist53462$k473942, %struct.ScmObj** %stackaddr$prim54622, align 8
%clofunc54623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc54623(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist53462$k473942)
ret void
}

define tailcc void @proc_clo$ae49255(%struct.ScmObj* %env$ae49255,%struct.ScmObj* %current_45args53465) {
%stackaddr$env-ref54624 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54624
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53465)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim54625, align 8
%stackaddr$prim54626 = alloca %struct.ScmObj*, align 8
%current_45args53466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53465)
store volatile %struct.ScmObj* %current_45args53466, %struct.ScmObj** %stackaddr$prim54626, align 8
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim54627, align 8
%stackaddr$prim54628 = alloca %struct.ScmObj*, align 8
%current_45args53467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53466)
store volatile %struct.ScmObj* %current_45args53467, %struct.ScmObj** %stackaddr$prim54628, align 8
%stackaddr$prim54629 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim54629, align 8
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54630, align 8
%truthy$cmp54631 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47263)
%cmp$cmp54631 = icmp eq i64 %truthy$cmp54631, 1
br i1 %cmp$cmp54631, label %truebranch$cmp54631, label %falsebranch$cmp54631
truebranch$cmp54631:
%ae49259 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53469$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%argslist53469$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53469$k473950)
store volatile %struct.ScmObj* %argslist53469$k473951, %struct.ScmObj** %stackaddr$prim54632, align 8
%stackaddr$prim54633 = alloca %struct.ScmObj*, align 8
%argslist53469$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49259, %struct.ScmObj* %argslist53469$k473951)
store volatile %struct.ScmObj* %argslist53469$k473952, %struct.ScmObj** %stackaddr$prim54633, align 8
%clofunc54634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54634(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53469$k473952)
ret void
falsebranch$cmp54631:
%stackaddr$prim54635 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54635, align 8
%ae49266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49266)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54636, align 8
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54637, align 8
%stackaddr$makeclosure54638 = alloca %struct.ScmObj*, align 8
%fptrToInt54639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49269 to i64
%ae49269 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54639)
store volatile %struct.ScmObj* %ae49269, %struct.ScmObj** %stackaddr$makeclosure54638, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49269, %struct.ScmObj* %k47395, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49269, %struct.ScmObj* %anf_45bind47264, i64 1)
%argslist53474$anf_45bind472650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54640 = alloca %struct.ScmObj*, align 8
%argslist53474$anf_45bind472651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53474$anf_45bind472650)
store volatile %struct.ScmObj* %argslist53474$anf_45bind472651, %struct.ScmObj** %stackaddr$prim54640, align 8
%stackaddr$prim54641 = alloca %struct.ScmObj*, align 8
%argslist53474$anf_45bind472652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47266, %struct.ScmObj* %argslist53474$anf_45bind472651)
store volatile %struct.ScmObj* %argslist53474$anf_45bind472652, %struct.ScmObj** %stackaddr$prim54641, align 8
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%argslist53474$anf_45bind472653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49269, %struct.ScmObj* %argslist53474$anf_45bind472652)
store volatile %struct.ScmObj* %argslist53474$anf_45bind472653, %struct.ScmObj** %stackaddr$prim54642, align 8
%clofunc54643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47265)
musttail call tailcc void %clofunc54643(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %argslist53474$anf_45bind472653)
ret void
}

define tailcc void @proc_clo$ae49269(%struct.ScmObj* %env$ae49269,%struct.ScmObj* %current_45args53470) {
%stackaddr$env-ref54644 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49269, i64 0)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54644
%stackaddr$env-ref54645 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49269, i64 1)
store %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$env-ref54645
%stackaddr$prim54646 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim54646, align 8
%stackaddr$prim54647 = alloca %struct.ScmObj*, align 8
%current_45args53471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53470)
store volatile %struct.ScmObj* %current_45args53471, %struct.ScmObj** %stackaddr$prim54647, align 8
%stackaddr$prim54648 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53471)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54648, align 8
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%cpsprim47397 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %cpsprim47397, %struct.ScmObj** %stackaddr$prim54649, align 8
%ae49275 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53473$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%argslist53473$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47397, %struct.ScmObj* %argslist53473$k473950)
store volatile %struct.ScmObj* %argslist53473$k473951, %struct.ScmObj** %stackaddr$prim54650, align 8
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%argslist53473$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49275, %struct.ScmObj* %argslist53473$k473951)
store volatile %struct.ScmObj* %argslist53473$k473952, %struct.ScmObj** %stackaddr$prim54651, align 8
%clofunc54652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc54652(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist53473$k473952)
ret void
}

define tailcc void @proc_clo$ae49229(%struct.ScmObj* %env$ae49229,%struct.ScmObj* %current_45args53476) {
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53476)
store volatile %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$prim54654 = alloca %struct.ScmObj*, align 8
%current_45args53477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53476)
store volatile %struct.ScmObj* %current_45args53477, %struct.ScmObj** %stackaddr$prim54654, align 8
%stackaddr$prim54655 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53477)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim54655, align 8
%stackaddr$prim54656 = alloca %struct.ScmObj*, align 8
%current_45args53478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53477)
store volatile %struct.ScmObj* %current_45args53478, %struct.ScmObj** %stackaddr$prim54656, align 8
%stackaddr$prim54657 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53478)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim54657, align 8
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54658, align 8
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%cpsprim47399 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsprim47399, %struct.ScmObj** %stackaddr$prim54659, align 8
%ae49234 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53480$k473980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54660 = alloca %struct.ScmObj*, align 8
%argslist53480$k473981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47399, %struct.ScmObj* %argslist53480$k473980)
store volatile %struct.ScmObj* %argslist53480$k473981, %struct.ScmObj** %stackaddr$prim54660, align 8
%stackaddr$prim54661 = alloca %struct.ScmObj*, align 8
%argslist53480$k473982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49234, %struct.ScmObj* %argslist53480$k473981)
store volatile %struct.ScmObj* %argslist53480$k473982, %struct.ScmObj** %stackaddr$prim54661, align 8
%clofunc54662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47398)
musttail call tailcc void %clofunc54662(%struct.ScmObj* %k47398, %struct.ScmObj* %argslist53480$k473982)
ret void
}

define tailcc void @proc_clo$ae49205(%struct.ScmObj* %env$ae49205,%struct.ScmObj* %current_45args53482) {
%stackaddr$prim54663 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53482)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim54663, align 8
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%current_45args53483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53482)
store volatile %struct.ScmObj* %current_45args53483, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53483)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%current_45args53484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53483)
store volatile %struct.ScmObj* %current_45args53484, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53484)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54668, align 8
%stackaddr$prim54669 = alloca %struct.ScmObj*, align 8
%cpsprim47401 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsprim47401, %struct.ScmObj** %stackaddr$prim54669, align 8
%ae49210 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53486$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%argslist53486$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47401, %struct.ScmObj* %argslist53486$k474000)
store volatile %struct.ScmObj* %argslist53486$k474001, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%argslist53486$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49210, %struct.ScmObj* %argslist53486$k474001)
store volatile %struct.ScmObj* %argslist53486$k474002, %struct.ScmObj** %stackaddr$prim54671, align 8
%clofunc54672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc54672(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53486$k474002)
ret void
}

define tailcc void @proc_clo$ae48811(%struct.ScmObj* %env$ae48811,%struct.ScmObj* %current_45args53489) {
%stackaddr$env-ref54673 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48811, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54673
%stackaddr$env-ref54674 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48811, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54674
%stackaddr$env-ref54675 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48811, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54675
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53489)
store volatile %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%current_45args53490 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53489)
store volatile %struct.ScmObj* %current_45args53490, %struct.ScmObj** %stackaddr$prim54677, align 8
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53490)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim54678, align 8
%ae48813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54679 = alloca %struct.ScmObj*, align 8
%fptrToInt54680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48814 to i64
%ae48814 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54680)
store volatile %struct.ScmObj* %ae48814, %struct.ScmObj** %stackaddr$makeclosure54679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48814, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48814, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48814, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48814, %struct.ScmObj* %_37map147120, i64 3)
%argslist53547$k474020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%argslist53547$k474021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48814, %struct.ScmObj* %argslist53547$k474020)
store volatile %struct.ScmObj* %argslist53547$k474021, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%argslist53547$k474022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48813, %struct.ScmObj* %argslist53547$k474021)
store volatile %struct.ScmObj* %argslist53547$k474022, %struct.ScmObj** %stackaddr$prim54682, align 8
%clofunc54683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47402)
musttail call tailcc void %clofunc54683(%struct.ScmObj* %k47402, %struct.ScmObj* %argslist53547$k474022)
ret void
}

define tailcc void @proc_clo$ae48814(%struct.ScmObj* %env$ae48814,%struct.ScmObj* %args4717347403) {
%stackaddr$env-ref54684 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48814, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54684
%stackaddr$env-ref54685 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48814, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54685
%stackaddr$env-ref54686 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48814, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54686
%stackaddr$env-ref54687 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48814, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54687
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347403)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim54688, align 8
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347403)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim54689, align 8
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$makeclosure54695 = alloca %struct.ScmObj*, align 8
%fptrToInt54696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48822 to i64
%ae48822 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54696)
store volatile %struct.ScmObj* %ae48822, %struct.ScmObj** %stackaddr$makeclosure54695, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %k47404, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48822, %struct.ScmObj* %acc47175, i64 7)
%ae48823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54697 = alloca %struct.ScmObj*, align 8
%fptrToInt54698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48824 to i64
%ae48824 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54698)
store volatile %struct.ScmObj* %ae48824, %struct.ScmObj** %stackaddr$makeclosure54697, align 8
%argslist53546$ae488220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%argslist53546$ae488221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48824, %struct.ScmObj* %argslist53546$ae488220)
store volatile %struct.ScmObj* %argslist53546$ae488221, %struct.ScmObj** %stackaddr$prim54699, align 8
%stackaddr$prim54700 = alloca %struct.ScmObj*, align 8
%argslist53546$ae488222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48823, %struct.ScmObj* %argslist53546$ae488221)
store volatile %struct.ScmObj* %argslist53546$ae488222, %struct.ScmObj** %stackaddr$prim54700, align 8
%clofunc54701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48822)
musttail call tailcc void %clofunc54701(%struct.ScmObj* %ae48822, %struct.ScmObj* %argslist53546$ae488222)
ret void
}

define tailcc void @proc_clo$ae48822(%struct.ScmObj* %env$ae48822,%struct.ScmObj* %current_45args53492) {
%stackaddr$env-ref54702 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54702
%stackaddr$env-ref54703 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54703
%stackaddr$env-ref54704 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54704
%stackaddr$env-ref54705 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54705
%stackaddr$env-ref54706 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54706
%stackaddr$env-ref54707 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 5)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54707
%stackaddr$env-ref54708 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54708
%stackaddr$env-ref54709 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48822, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54709
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53492)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%current_45args53493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53492)
store volatile %struct.ScmObj* %current_45args53493, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53493)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$makeclosure54713 = alloca %struct.ScmObj*, align 8
%fptrToInt54714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48854 to i64
%ae48854 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54714)
store volatile %struct.ScmObj* %ae48854, %struct.ScmObj** %stackaddr$makeclosure54713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %k47404, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48854, %struct.ScmObj* %acc47175, i64 6)
%ae48856 = call %struct.ScmObj* @const_init_false()
%argslist53539$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54715 = alloca %struct.ScmObj*, align 8
%argslist53539$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53539$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53539$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54715, align 8
%stackaddr$prim54716 = alloca %struct.ScmObj*, align 8
%argslist53539$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48856, %struct.ScmObj* %argslist53539$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53539$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54716, align 8
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%argslist53539$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist53539$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53539$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%argslist53539$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48854, %struct.ScmObj* %argslist53539$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53539$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54718, align 8
%clofunc54719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54719(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53539$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48854(%struct.ScmObj* %env$ae48854,%struct.ScmObj* %current_45args53495) {
%stackaddr$env-ref54720 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54720
%stackaddr$env-ref54721 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54721
%stackaddr$env-ref54722 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54722
%stackaddr$env-ref54723 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54723
%stackaddr$env-ref54724 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 4)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54724
%stackaddr$env-ref54725 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54725
%stackaddr$env-ref54726 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48854, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54726
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53495)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%current_45args53496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53495)
store volatile %struct.ScmObj* %current_45args53496, %struct.ScmObj** %stackaddr$prim54728, align 8
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53496)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54729, align 8
%truthy$cmp54730 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47252)
%cmp$cmp54730 = icmp eq i64 %truthy$cmp54730, 1
br i1 %cmp$cmp54730, label %truebranch$cmp54730, label %falsebranch$cmp54730
truebranch$cmp54730:
%ae48865 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53498$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%argslist53498$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist53498$k474040)
store volatile %struct.ScmObj* %argslist53498$k474041, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%argslist53498$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48865, %struct.ScmObj* %argslist53498$k474041)
store volatile %struct.ScmObj* %argslist53498$k474042, %struct.ScmObj** %stackaddr$prim54732, align 8
%clofunc54733 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc54733(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist53498$k474042)
ret void
falsebranch$cmp54730:
%stackaddr$makeclosure54734 = alloca %struct.ScmObj*, align 8
%fptrToInt54735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48870 to i64
%ae48870 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54735)
store volatile %struct.ScmObj* %ae48870, %struct.ScmObj** %stackaddr$makeclosure54734, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %k47404, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48870, %struct.ScmObj* %acc47175, i64 6)
%ae48871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54736 = alloca %struct.ScmObj*, align 8
%fptrToInt54737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48872 to i64
%ae48872 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54737)
store volatile %struct.ScmObj* %ae48872, %struct.ScmObj** %stackaddr$makeclosure54736, align 8
%argslist53538$ae488700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%argslist53538$ae488701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48872, %struct.ScmObj* %argslist53538$ae488700)
store volatile %struct.ScmObj* %argslist53538$ae488701, %struct.ScmObj** %stackaddr$prim54738, align 8
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%argslist53538$ae488702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48871, %struct.ScmObj* %argslist53538$ae488701)
store volatile %struct.ScmObj* %argslist53538$ae488702, %struct.ScmObj** %stackaddr$prim54739, align 8
%clofunc54740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48870)
musttail call tailcc void %clofunc54740(%struct.ScmObj* %ae48870, %struct.ScmObj* %argslist53538$ae488702)
ret void
}

define tailcc void @proc_clo$ae48870(%struct.ScmObj* %env$ae48870,%struct.ScmObj* %current_45args53499) {
%stackaddr$env-ref54741 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54741
%stackaddr$env-ref54742 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54742
%stackaddr$env-ref54743 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54743
%stackaddr$env-ref54744 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54744
%stackaddr$env-ref54745 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 4)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54745
%stackaddr$env-ref54746 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54746
%stackaddr$env-ref54747 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48870, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54747
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%current_45args53500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53499)
store volatile %struct.ScmObj* %current_45args53500, %struct.ScmObj** %stackaddr$prim54749, align 8
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53500)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54750, align 8
%stackaddr$makeclosure54751 = alloca %struct.ScmObj*, align 8
%fptrToInt54752 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48891 to i64
%ae48891 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54752)
store volatile %struct.ScmObj* %ae48891, %struct.ScmObj** %stackaddr$makeclosure54751, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %k47404, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %acc47175, i64 6)
%argslist53533$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%argslist53533$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53533$_37map1471200)
store volatile %struct.ScmObj* %argslist53533$_37map1471201, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%argslist53533$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist53533$_37map1471201)
store volatile %struct.ScmObj* %argslist53533$_37map1471202, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%argslist53533$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48891, %struct.ScmObj* %argslist53533$_37map1471202)
store volatile %struct.ScmObj* %argslist53533$_37map1471203, %struct.ScmObj** %stackaddr$prim54755, align 8
%clofunc54756 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54756(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53533$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48891(%struct.ScmObj* %env$ae48891,%struct.ScmObj* %current_45args53502) {
%stackaddr$env-ref54757 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54757
%stackaddr$env-ref54758 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54758
%stackaddr$env-ref54759 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54759
%stackaddr$env-ref54760 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54760
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 4)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$env-ref54762 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54762
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%_95k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %_95k47408, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%current_45args53503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53502)
store volatile %struct.ScmObj* %current_45args53503, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53503)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim54766, align 8
%stackaddr$makeclosure54767 = alloca %struct.ScmObj*, align 8
%fptrToInt54768 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48894 to i64
%ae48894 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54768)
store volatile %struct.ScmObj* %ae48894, %struct.ScmObj** %stackaddr$makeclosure54767, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %lsts_4347181, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %k47404, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %acc47175, i64 7)
%ae48895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54769 = alloca %struct.ScmObj*, align 8
%fptrToInt54770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48896 to i64
%ae48896 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54770)
store volatile %struct.ScmObj* %ae48896, %struct.ScmObj** %stackaddr$makeclosure54769, align 8
%argslist53532$ae488940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%argslist53532$ae488941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48896, %struct.ScmObj* %argslist53532$ae488940)
store volatile %struct.ScmObj* %argslist53532$ae488941, %struct.ScmObj** %stackaddr$prim54771, align 8
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%argslist53532$ae488942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48895, %struct.ScmObj* %argslist53532$ae488941)
store volatile %struct.ScmObj* %argslist53532$ae488942, %struct.ScmObj** %stackaddr$prim54772, align 8
%clofunc54773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48894)
musttail call tailcc void %clofunc54773(%struct.ScmObj* %ae48894, %struct.ScmObj* %argslist53532$ae488942)
ret void
}

define tailcc void @proc_clo$ae48894(%struct.ScmObj* %env$ae48894,%struct.ScmObj* %current_45args53505) {
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$env-ref54777 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54777
%stackaddr$env-ref54778 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 4)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54778
%stackaddr$env-ref54779 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 5)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54779
%stackaddr$env-ref54780 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54780
%stackaddr$env-ref54781 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54781
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%_95k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %_95k47409, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%current_45args53506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %current_45args53506, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53506)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$makeclosure54785 = alloca %struct.ScmObj*, align 8
%fptrToInt54786 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48915 to i64
%ae48915 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54786)
store volatile %struct.ScmObj* %ae48915, %struct.ScmObj** %stackaddr$makeclosure54785, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %k47404, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %acc47175, i64 5)
%argslist53527$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54787 = alloca %struct.ScmObj*, align 8
%argslist53527$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53527$_37map1471200)
store volatile %struct.ScmObj* %argslist53527$_37map1471201, %struct.ScmObj** %stackaddr$prim54787, align 8
%stackaddr$prim54788 = alloca %struct.ScmObj*, align 8
%argslist53527$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53527$_37map1471201)
store volatile %struct.ScmObj* %argslist53527$_37map1471202, %struct.ScmObj** %stackaddr$prim54788, align 8
%stackaddr$prim54789 = alloca %struct.ScmObj*, align 8
%argslist53527$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48915, %struct.ScmObj* %argslist53527$_37map1471202)
store volatile %struct.ScmObj* %argslist53527$_37map1471203, %struct.ScmObj** %stackaddr$prim54789, align 8
%clofunc54790 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54790(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53527$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48915(%struct.ScmObj* %env$ae48915,%struct.ScmObj* %current_45args53508) {
%stackaddr$env-ref54791 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54791
%stackaddr$env-ref54792 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54792
%stackaddr$env-ref54793 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54793
%stackaddr$env-ref54794 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 3)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54794
%stackaddr$env-ref54795 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54795
%stackaddr$env-ref54796 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54796
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%_95k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %_95k47410, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%current_45args53509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53508)
store volatile %struct.ScmObj* %current_45args53509, %struct.ScmObj** %stackaddr$prim54798, align 8
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53509)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim54799, align 8
%stackaddr$makeclosure54800 = alloca %struct.ScmObj*, align 8
%fptrToInt54801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48918 to i64
%ae48918 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54801)
store volatile %struct.ScmObj* %ae48918, %struct.ScmObj** %stackaddr$makeclosure54800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %k47404, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %acc47175, i64 6)
%ae48919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54802 = alloca %struct.ScmObj*, align 8
%fptrToInt54803 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48920 to i64
%ae48920 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54803)
store volatile %struct.ScmObj* %ae48920, %struct.ScmObj** %stackaddr$makeclosure54802, align 8
%argslist53526$ae489180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%argslist53526$ae489181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48920, %struct.ScmObj* %argslist53526$ae489180)
store volatile %struct.ScmObj* %argslist53526$ae489181, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%argslist53526$ae489182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48919, %struct.ScmObj* %argslist53526$ae489181)
store volatile %struct.ScmObj* %argslist53526$ae489182, %struct.ScmObj** %stackaddr$prim54805, align 8
%clofunc54806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48918)
musttail call tailcc void %clofunc54806(%struct.ScmObj* %ae48918, %struct.ScmObj* %argslist53526$ae489182)
ret void
}

define tailcc void @proc_clo$ae48918(%struct.ScmObj* %env$ae48918,%struct.ScmObj* %current_45args53511) {
%stackaddr$env-ref54807 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54807
%stackaddr$env-ref54808 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54808
%stackaddr$env-ref54809 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54809
%stackaddr$env-ref54810 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 3)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54810
%stackaddr$env-ref54811 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref54811
%stackaddr$env-ref54812 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54812
%stackaddr$env-ref54813 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54813
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%current_45args53512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53511)
store volatile %struct.ScmObj* %current_45args53512, %struct.ScmObj** %stackaddr$prim54815, align 8
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53512)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim54816, align 8
%ae48941 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54817 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48941)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim54817, align 8
%stackaddr$makeclosure54818 = alloca %struct.ScmObj*, align 8
%fptrToInt54819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48943 to i64
%ae48943 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54819)
store volatile %struct.ScmObj* %ae48943, %struct.ScmObj** %stackaddr$makeclosure54818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %k47404, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48943, %struct.ScmObj* %f47176, i64 3)
%argslist53520$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%argslist53520$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist53520$_37foldr470940)
store volatile %struct.ScmObj* %argslist53520$_37foldr470941, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%argslist53520$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist53520$_37foldr470941)
store volatile %struct.ScmObj* %argslist53520$_37foldr470942, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%argslist53520$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist53520$_37foldr470942)
store volatile %struct.ScmObj* %argslist53520$_37foldr470943, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%argslist53520$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist53520$_37foldr470943)
store volatile %struct.ScmObj* %argslist53520$_37foldr470944, %struct.ScmObj** %stackaddr$prim54823, align 8
%clofunc54824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54824(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist53520$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48943(%struct.ScmObj* %env$ae48943,%struct.ScmObj* %current_45args53514) {
%stackaddr$env-ref54825 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54825
%stackaddr$env-ref54826 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54826
%stackaddr$env-ref54827 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 2)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54827
%stackaddr$env-ref54828 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48943, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54828
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%current_45args53515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %current_45args53515, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53515)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$makeclosure54832 = alloca %struct.ScmObj*, align 8
%fptrToInt54833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48947 to i64
%ae48947 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54833)
store volatile %struct.ScmObj* %ae48947, %struct.ScmObj** %stackaddr$makeclosure54832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48947, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48947, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48947, %struct.ScmObj* %k47404, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48947, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%cpsargs47415 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48947, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %cpsargs47415, %struct.ScmObj** %stackaddr$prim54834, align 8
%clofunc54835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc54835(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47415)
ret void
}

define tailcc void @proc_clo$ae48947(%struct.ScmObj* %env$ae48947,%struct.ScmObj* %current_45args53517) {
%stackaddr$env-ref54836 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48947, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54836
%stackaddr$env-ref54837 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48947, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54837
%stackaddr$env-ref54838 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48947, i64 2)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref54838
%stackaddr$env-ref54839 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48947, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54839
%stackaddr$prim54840 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53517)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim54840, align 8
%stackaddr$prim54841 = alloca %struct.ScmObj*, align 8
%current_45args53518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53517)
store volatile %struct.ScmObj* %current_45args53518, %struct.ScmObj** %stackaddr$prim54841, align 8
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53518)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim54842, align 8
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47258)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%cpsargs47414 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47404, %struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %cpsargs47414, %struct.ScmObj** %stackaddr$prim54845, align 8
%clofunc54846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc54846(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47414)
ret void
}

define tailcc void @proc_clo$ae48920(%struct.ScmObj* %env$ae48920,%struct.ScmObj* %current_45args53521) {
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53521)
store volatile %struct.ScmObj* %k47416, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%current_45args53522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53521)
store volatile %struct.ScmObj* %current_45args53522, %struct.ScmObj** %stackaddr$prim54848, align 8
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53522)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%current_45args53523 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53522)
store volatile %struct.ScmObj* %current_45args53523, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53523)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim54851, align 8
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim54852, align 8
%ae48924 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53525$k474160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%argslist53525$k474161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist53525$k474160)
store volatile %struct.ScmObj* %argslist53525$k474161, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%argslist53525$k474162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist53525$k474161)
store volatile %struct.ScmObj* %argslist53525$k474162, %struct.ScmObj** %stackaddr$prim54854, align 8
%clofunc54855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47416)
musttail call tailcc void %clofunc54855(%struct.ScmObj* %k47416, %struct.ScmObj* %argslist53525$k474162)
ret void
}

define tailcc void @proc_clo$ae48896(%struct.ScmObj* %env$ae48896,%struct.ScmObj* %current_45args53528) {
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53528)
store volatile %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$prim54856, align 8
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%current_45args53529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53528)
store volatile %struct.ScmObj* %current_45args53529, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53529)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim54858, align 8
%stackaddr$prim54859 = alloca %struct.ScmObj*, align 8
%cpsprim47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47419, %struct.ScmObj** %stackaddr$prim54859, align 8
%ae48899 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53531$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%argslist53531$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47419, %struct.ScmObj* %argslist53531$k474180)
store volatile %struct.ScmObj* %argslist53531$k474181, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%argslist53531$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist53531$k474181)
store volatile %struct.ScmObj* %argslist53531$k474182, %struct.ScmObj** %stackaddr$prim54861, align 8
%clofunc54862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc54862(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist53531$k474182)
ret void
}

define tailcc void @proc_clo$ae48872(%struct.ScmObj* %env$ae48872,%struct.ScmObj* %current_45args53534) {
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53534)
store volatile %struct.ScmObj* %k47420, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%current_45args53535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53534)
store volatile %struct.ScmObj* %current_45args53535, %struct.ScmObj** %stackaddr$prim54864, align 8
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53535)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%cpsprim47421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47421, %struct.ScmObj** %stackaddr$prim54866, align 8
%ae48875 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53537$k474200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%argslist53537$k474201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47421, %struct.ScmObj* %argslist53537$k474200)
store volatile %struct.ScmObj* %argslist53537$k474201, %struct.ScmObj** %stackaddr$prim54867, align 8
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%argslist53537$k474202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48875, %struct.ScmObj* %argslist53537$k474201)
store volatile %struct.ScmObj* %argslist53537$k474202, %struct.ScmObj** %stackaddr$prim54868, align 8
%clofunc54869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47420)
musttail call tailcc void %clofunc54869(%struct.ScmObj* %k47420, %struct.ScmObj* %argslist53537$k474202)
ret void
}

define tailcc void @proc_clo$ae48824(%struct.ScmObj* %env$ae48824,%struct.ScmObj* %current_45args53540) {
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53540)
store volatile %struct.ScmObj* %k47422, %struct.ScmObj** %stackaddr$prim54870, align 8
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%current_45args53541 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53540)
store volatile %struct.ScmObj* %current_45args53541, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53541)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%current_45args53542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53541)
store volatile %struct.ScmObj* %current_45args53542, %struct.ScmObj** %stackaddr$prim54873, align 8
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53542)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim54874, align 8
%truthy$cmp54875 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp54875 = icmp eq i64 %truthy$cmp54875, 1
br i1 %cmp$cmp54875, label %truebranch$cmp54875, label %falsebranch$cmp54875
truebranch$cmp54875:
%ae48827 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53544$k474220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%argslist53544$k474221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist53544$k474220)
store volatile %struct.ScmObj* %argslist53544$k474221, %struct.ScmObj** %stackaddr$prim54876, align 8
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%argslist53544$k474222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48827, %struct.ScmObj* %argslist53544$k474221)
store volatile %struct.ScmObj* %argslist53544$k474222, %struct.ScmObj** %stackaddr$prim54877, align 8
%clofunc54878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47422)
musttail call tailcc void %clofunc54878(%struct.ScmObj* %k47422, %struct.ScmObj* %argslist53544$k474222)
ret void
falsebranch$cmp54875:
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim54879, align 8
%ae48834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53545$k474220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%argslist53545$k474221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist53545$k474220)
store volatile %struct.ScmObj* %argslist53545$k474221, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%argslist53545$k474222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48834, %struct.ScmObj* %argslist53545$k474221)
store volatile %struct.ScmObj* %argslist53545$k474222, %struct.ScmObj** %stackaddr$prim54881, align 8
%clofunc54882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47422)
musttail call tailcc void %clofunc54882(%struct.ScmObj* %k47422, %struct.ScmObj* %argslist53545$k474222)
ret void
}

define tailcc void @proc_clo$ae48665(%struct.ScmObj* %env$ae48665,%struct.ScmObj* %args4711647424) {
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48665, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48665, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48665, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647424)
store volatile %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647424)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$makeclosure54890 = alloca %struct.ScmObj*, align 8
%fptrToInt54891 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48670 to i64
%ae48670 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54891)
store volatile %struct.ScmObj* %ae48670, %struct.ScmObj** %stackaddr$makeclosure54890, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48670, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48670, %struct.ScmObj* %k47425, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48670, %struct.ScmObj* %lsts47117, i64 2)
%ae48671 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54892 = alloca %struct.ScmObj*, align 8
%fptrToInt54893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48672 to i64
%ae48672 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54893)
store volatile %struct.ScmObj* %ae48672, %struct.ScmObj** %stackaddr$makeclosure54892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48672, %struct.ScmObj* %_37last47111, i64 2)
%argslist53564$ae486700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%argslist53564$ae486701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48672, %struct.ScmObj* %argslist53564$ae486700)
store volatile %struct.ScmObj* %argslist53564$ae486701, %struct.ScmObj** %stackaddr$prim54894, align 8
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%argslist53564$ae486702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48671, %struct.ScmObj* %argslist53564$ae486701)
store volatile %struct.ScmObj* %argslist53564$ae486702, %struct.ScmObj** %stackaddr$prim54895, align 8
%clofunc54896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48670)
musttail call tailcc void %clofunc54896(%struct.ScmObj* %ae48670, %struct.ScmObj* %argslist53564$ae486702)
ret void
}

define tailcc void @proc_clo$ae48670(%struct.ScmObj* %env$ae48670,%struct.ScmObj* %current_45args53549) {
%stackaddr$env-ref54897 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48670, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54897
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48670, i64 1)
store %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48670, i64 2)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$prim54900 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53549)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim54900, align 8
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%current_45args53550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53549)
store volatile %struct.ScmObj* %current_45args53550, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim54902, align 8
%ae48733 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54903 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48733, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim54903, align 8
%stackaddr$prim54904 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %anf_45bind47247)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim54904, align 8
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%cpsargs47427 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47425, %struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %cpsargs47427, %struct.ScmObj** %stackaddr$prim54905, align 8
%clofunc54906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54906(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47427)
ret void
}

define tailcc void @proc_clo$ae48672(%struct.ScmObj* %env$ae48672,%struct.ScmObj* %fargs4711947428) {
%stackaddr$env-ref54907 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54907
%stackaddr$env-ref54908 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54908
%stackaddr$env-ref54909 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48672, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54909
%stackaddr$prim54910 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947428)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim54910, align 8
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947428)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$makeclosure54912 = alloca %struct.ScmObj*, align 8
%fptrToInt54913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48676 to i64
%ae48676 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54913)
store volatile %struct.ScmObj* %ae48676, %struct.ScmObj** %stackaddr$makeclosure54912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %k47429, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %f47118, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48676, %struct.ScmObj* %_37last47111, i64 3)
%ae48678 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53563$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%argslist53563$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48678, %struct.ScmObj* %argslist53563$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist53563$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$prim54915 = alloca %struct.ScmObj*, align 8
%argslist53563$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53563$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist53563$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim54915, align 8
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%argslist53563$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48676, %struct.ScmObj* %argslist53563$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist53563$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim54916, align 8
%clofunc54917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc54917(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist53563$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48676(%struct.ScmObj* %env$ae48676,%struct.ScmObj* %current_45args53552) {
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 0)
store %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 2)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$env-ref54921 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48676, i64 3)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54921
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53552)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%current_45args53553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53552)
store volatile %struct.ScmObj* %current_45args53553, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$makeclosure54925 = alloca %struct.ScmObj*, align 8
%fptrToInt54926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48683 to i64
%ae48683 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54926)
store volatile %struct.ScmObj* %ae48683, %struct.ScmObj** %stackaddr$makeclosure54925, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %k47429, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %_37last47111, i64 2)
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%cpsargs47434 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48683, %struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %cpsargs47434, %struct.ScmObj** %stackaddr$prim54927, align 8
%clofunc54928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc54928(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47434)
ret void
}

define tailcc void @proc_clo$ae48683(%struct.ScmObj* %env$ae48683,%struct.ScmObj* %current_45args53555) {
%stackaddr$env-ref54929 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 0)
store %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$env-ref54929
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53555)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%current_45args53556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53555)
store volatile %struct.ScmObj* %current_45args53556, %struct.ScmObj** %stackaddr$prim54933, align 8
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53556)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$makeclosure54935 = alloca %struct.ScmObj*, align 8
%fptrToInt54936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48688 to i64
%ae48688 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54936)
store volatile %struct.ScmObj* %ae48688, %struct.ScmObj** %stackaddr$makeclosure54935, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48688, %struct.ScmObj* %k47429, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48688, %struct.ScmObj* %anf_45bind47244, i64 1)
%argslist53562$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%argslist53562$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53562$_37last471110)
store volatile %struct.ScmObj* %argslist53562$_37last471111, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%argslist53562$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48688, %struct.ScmObj* %argslist53562$_37last471111)
store volatile %struct.ScmObj* %argslist53562$_37last471112, %struct.ScmObj** %stackaddr$prim54938, align 8
%clofunc54939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc54939(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist53562$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48688(%struct.ScmObj* %env$ae48688,%struct.ScmObj* %current_45args53558) {
%stackaddr$env-ref54940 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48688, i64 0)
store %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$env-ref54940
%stackaddr$env-ref54941 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48688, i64 1)
store %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$env-ref54941
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53558)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%current_45args53559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53558)
store volatile %struct.ScmObj* %current_45args53559, %struct.ScmObj** %stackaddr$prim54943, align 8
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%cpsprim47433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %anf_45bind47245)
store volatile %struct.ScmObj* %cpsprim47433, %struct.ScmObj** %stackaddr$prim54945, align 8
%ae48693 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53561$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%argslist53561$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47433, %struct.ScmObj* %argslist53561$k474290)
store volatile %struct.ScmObj* %argslist53561$k474291, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%argslist53561$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48693, %struct.ScmObj* %argslist53561$k474291)
store volatile %struct.ScmObj* %argslist53561$k474292, %struct.ScmObj** %stackaddr$prim54947, align 8
%clofunc54948 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc54948(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist53561$k474292)
ret void
}

define tailcc void @proc_clo$ae48588(%struct.ScmObj* %env$ae48588,%struct.ScmObj* %current_45args53566) {
%stackaddr$env-ref54949 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48588, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54949
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%current_45args53567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53566)
store volatile %struct.ScmObj* %current_45args53567, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53567)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%current_45args53568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53567)
store volatile %struct.ScmObj* %current_45args53568, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53568)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$makeclosure54955 = alloca %struct.ScmObj*, align 8
%fptrToInt54956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48589 to i64
%ae48589 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54956)
store volatile %struct.ScmObj* %ae48589, %struct.ScmObj** %stackaddr$makeclosure54955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48589, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48589, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48589, %struct.ScmObj* %k47435, i64 2)
%ae48590 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54957 = alloca %struct.ScmObj*, align 8
%fptrToInt54958 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48591 to i64
%ae48591 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54958)
store volatile %struct.ScmObj* %ae48591, %struct.ScmObj** %stackaddr$makeclosure54957, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48591, %struct.ScmObj* %f47122, i64 0)
%argslist53583$ae485890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%argslist53583$ae485891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48591, %struct.ScmObj* %argslist53583$ae485890)
store volatile %struct.ScmObj* %argslist53583$ae485891, %struct.ScmObj** %stackaddr$prim54959, align 8
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist53583$ae485892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48590, %struct.ScmObj* %argslist53583$ae485891)
store volatile %struct.ScmObj* %argslist53583$ae485892, %struct.ScmObj** %stackaddr$prim54960, align 8
%clofunc54961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48589)
musttail call tailcc void %clofunc54961(%struct.ScmObj* %ae48589, %struct.ScmObj* %argslist53583$ae485892)
ret void
}

define tailcc void @proc_clo$ae48589(%struct.ScmObj* %env$ae48589,%struct.ScmObj* %current_45args53570) {
%stackaddr$env-ref54962 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48589, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref54962
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48589, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$env-ref54964 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48589, i64 2)
store %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$env-ref54964
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53570)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%current_45args53571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53570)
store volatile %struct.ScmObj* %current_45args53571, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53571)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim54967, align 8
%ae48623 = call %struct.ScmObj* @const_init_null()
%argslist53573$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%argslist53573$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist53573$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53573$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54968, align 8
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%argslist53573$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist53573$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53573$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%argslist53573$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47242, %struct.ScmObj* %argslist53573$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53573$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist53573$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist53573$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53573$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54971, align 8
%clofunc54972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54972(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53573$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48591(%struct.ScmObj* %env$ae48591,%struct.ScmObj* %current_45args53574) {
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48591, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$prim54974 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53574)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim54974, align 8
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%current_45args53575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53574)
store volatile %struct.ScmObj* %current_45args53575, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53575)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%current_45args53576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53575)
store volatile %struct.ScmObj* %current_45args53576, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53576)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$makeclosure54979 = alloca %struct.ScmObj*, align 8
%fptrToInt54980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48593 to i64
%ae48593 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54980)
store volatile %struct.ScmObj* %ae48593, %struct.ScmObj** %stackaddr$makeclosure54979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %r47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48593, %struct.ScmObj* %k47437, i64 1)
%argslist53582$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%argslist53582$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist53582$f471220)
store volatile %struct.ScmObj* %argslist53582$f471221, %struct.ScmObj** %stackaddr$prim54981, align 8
%stackaddr$prim54982 = alloca %struct.ScmObj*, align 8
%argslist53582$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist53582$f471221)
store volatile %struct.ScmObj* %argslist53582$f471222, %struct.ScmObj** %stackaddr$prim54982, align 8
%clofunc54983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc54983(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist53582$f471222)
ret void
}

define tailcc void @proc_clo$ae48593(%struct.ScmObj* %env$ae48593,%struct.ScmObj* %current_45args53578) {
%stackaddr$env-ref54984 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 0)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref54984
%stackaddr$env-ref54985 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48593, i64 1)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref54985
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53578)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%current_45args53579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53578)
store volatile %struct.ScmObj* %current_45args53579, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53579)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%cpsprim47439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47439, %struct.ScmObj** %stackaddr$prim54989, align 8
%ae48598 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53581$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%argslist53581$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47439, %struct.ScmObj* %argslist53581$k474370)
store volatile %struct.ScmObj* %argslist53581$k474371, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%argslist53581$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48598, %struct.ScmObj* %argslist53581$k474371)
store volatile %struct.ScmObj* %argslist53581$k474372, %struct.ScmObj** %stackaddr$prim54991, align 8
%clofunc54992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc54992(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist53581$k474372)
ret void
}

define tailcc void @proc_clo$ae48202(%struct.ScmObj* %env$ae48202,%struct.ScmObj* %current_45args53586) {
%stackaddr$env-ref54993 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48202, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54993
%stackaddr$env-ref54994 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48202, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54994
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%current_45args53587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %current_45args53587, %struct.ScmObj** %stackaddr$prim54996, align 8
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53587)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim54997, align 8
%ae48204 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54998 = alloca %struct.ScmObj*, align 8
%fptrToInt54999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48205 to i64
%ae48205 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54999)
store volatile %struct.ScmObj* %ae48205, %struct.ScmObj** %stackaddr$makeclosure54998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48205, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48205, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48205, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53644$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%argslist53644$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48205, %struct.ScmObj* %argslist53644$k474400)
store volatile %struct.ScmObj* %argslist53644$k474401, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%argslist53644$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48204, %struct.ScmObj* %argslist53644$k474401)
store volatile %struct.ScmObj* %argslist53644$k474402, %struct.ScmObj** %stackaddr$prim55001, align 8
%clofunc55002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55002(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist53644$k474402)
ret void
}

define tailcc void @proc_clo$ae48205(%struct.ScmObj* %env$ae48205,%struct.ScmObj* %args4709647441) {
%stackaddr$env-ref55003 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48205, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55003
%stackaddr$env-ref55004 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48205, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55004
%stackaddr$env-ref55005 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48205, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55005
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647441)
store volatile %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647441)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim55007, align 8
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47228)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55011, align 8
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47229)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$makeclosure55013 = alloca %struct.ScmObj*, align 8
%fptrToInt55014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48213 to i64
%ae48213 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55014)
store volatile %struct.ScmObj* %ae48213, %struct.ScmObj** %stackaddr$makeclosure55013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48213, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48214 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55015 = alloca %struct.ScmObj*, align 8
%fptrToInt55016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48215 to i64
%ae48215 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55016)
store volatile %struct.ScmObj* %ae48215, %struct.ScmObj** %stackaddr$makeclosure55015, align 8
%argslist53643$ae482130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%argslist53643$ae482131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48215, %struct.ScmObj* %argslist53643$ae482130)
store volatile %struct.ScmObj* %argslist53643$ae482131, %struct.ScmObj** %stackaddr$prim55017, align 8
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%argslist53643$ae482132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48214, %struct.ScmObj* %argslist53643$ae482131)
store volatile %struct.ScmObj* %argslist53643$ae482132, %struct.ScmObj** %stackaddr$prim55018, align 8
%clofunc55019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48213)
musttail call tailcc void %clofunc55019(%struct.ScmObj* %ae48213, %struct.ScmObj* %argslist53643$ae482132)
ret void
}

define tailcc void @proc_clo$ae48213(%struct.ScmObj* %env$ae48213,%struct.ScmObj* %current_45args53589) {
%stackaddr$env-ref55020 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55020
%stackaddr$env-ref55021 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55021
%stackaddr$env-ref55022 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55022
%stackaddr$env-ref55023 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55023
%stackaddr$env-ref55024 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55024
%stackaddr$env-ref55025 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55025
%stackaddr$env-ref55026 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48213, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55026
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%current_45args53590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %current_45args53590, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53590)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim55029, align 8
%stackaddr$makeclosure55030 = alloca %struct.ScmObj*, align 8
%fptrToInt55031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48245 to i64
%ae48245 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55031)
store volatile %struct.ScmObj* %ae48245, %struct.ScmObj** %stackaddr$makeclosure55030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48245, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48247 = call %struct.ScmObj* @const_init_false()
%argslist53636$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%argslist53636$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53636$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53636$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%argslist53636$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48247, %struct.ScmObj* %argslist53636$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53636$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%argslist53636$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist53636$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53636$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%argslist53636$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48245, %struct.ScmObj* %argslist53636$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53636$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55035, align 8
%clofunc55036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55036(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53636$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48245(%struct.ScmObj* %env$ae48245,%struct.ScmObj* %current_45args53592) {
%stackaddr$env-ref55037 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55037
%stackaddr$env-ref55038 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55038
%stackaddr$env-ref55039 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55039
%stackaddr$env-ref55040 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55040
%stackaddr$env-ref55041 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55041
%stackaddr$env-ref55042 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55042
%stackaddr$env-ref55043 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48245, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55043
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53592)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim55044, align 8
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%current_45args53593 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53592)
store volatile %struct.ScmObj* %current_45args53593, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53593)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55046, align 8
%truthy$cmp55047 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47231)
%cmp$cmp55047 = icmp eq i64 %truthy$cmp55047, 1
br i1 %cmp$cmp55047, label %truebranch$cmp55047, label %falsebranch$cmp55047
truebranch$cmp55047:
%ae48256 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53595$k474420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%argslist53595$k474421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist53595$k474420)
store volatile %struct.ScmObj* %argslist53595$k474421, %struct.ScmObj** %stackaddr$prim55048, align 8
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%argslist53595$k474422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48256, %struct.ScmObj* %argslist53595$k474421)
store volatile %struct.ScmObj* %argslist53595$k474422, %struct.ScmObj** %stackaddr$prim55049, align 8
%clofunc55050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47442)
musttail call tailcc void %clofunc55050(%struct.ScmObj* %k47442, %struct.ScmObj* %argslist53595$k474422)
ret void
falsebranch$cmp55047:
%stackaddr$makeclosure55051 = alloca %struct.ScmObj*, align 8
%fptrToInt55052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48261 to i64
%ae48261 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55052)
store volatile %struct.ScmObj* %ae48261, %struct.ScmObj** %stackaddr$makeclosure55051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48261, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55053 = alloca %struct.ScmObj*, align 8
%fptrToInt55054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48263 to i64
%ae48263 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55054)
store volatile %struct.ScmObj* %ae48263, %struct.ScmObj** %stackaddr$makeclosure55053, align 8
%argslist53635$ae482610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%argslist53635$ae482611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48263, %struct.ScmObj* %argslist53635$ae482610)
store volatile %struct.ScmObj* %argslist53635$ae482611, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%argslist53635$ae482612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48262, %struct.ScmObj* %argslist53635$ae482611)
store volatile %struct.ScmObj* %argslist53635$ae482612, %struct.ScmObj** %stackaddr$prim55056, align 8
%clofunc55057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48261)
musttail call tailcc void %clofunc55057(%struct.ScmObj* %ae48261, %struct.ScmObj* %argslist53635$ae482612)
ret void
}

define tailcc void @proc_clo$ae48261(%struct.ScmObj* %env$ae48261,%struct.ScmObj* %current_45args53596) {
%stackaddr$env-ref55058 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55058
%stackaddr$env-ref55059 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55059
%stackaddr$env-ref55060 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55060
%stackaddr$env-ref55061 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55061
%stackaddr$env-ref55062 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55062
%stackaddr$env-ref55063 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55063
%stackaddr$env-ref55064 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48261, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55064
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53596)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim55065, align 8
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%current_45args53597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53596)
store volatile %struct.ScmObj* %current_45args53597, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53597)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$makeclosure55068 = alloca %struct.ScmObj*, align 8
%fptrToInt55069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48282 to i64
%ae48282 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55069)
store volatile %struct.ScmObj* %ae48282, %struct.ScmObj** %stackaddr$makeclosure55068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist53630$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%argslist53630$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53630$_37map1470850)
store volatile %struct.ScmObj* %argslist53630$_37map1470851, %struct.ScmObj** %stackaddr$prim55070, align 8
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%argslist53630$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist53630$_37map1470851)
store volatile %struct.ScmObj* %argslist53630$_37map1470852, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%argslist53630$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48282, %struct.ScmObj* %argslist53630$_37map1470852)
store volatile %struct.ScmObj* %argslist53630$_37map1470853, %struct.ScmObj** %stackaddr$prim55072, align 8
%clofunc55073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55073(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53630$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48282(%struct.ScmObj* %env$ae48282,%struct.ScmObj* %current_45args53599) {
%stackaddr$env-ref55074 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55074
%stackaddr$env-ref55075 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55075
%stackaddr$env-ref55076 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55076
%stackaddr$env-ref55077 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55077
%stackaddr$env-ref55078 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55078
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$env-ref55080 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55080
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53599)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%current_45args53600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53599)
store volatile %struct.ScmObj* %current_45args53600, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53600)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$makeclosure55084 = alloca %struct.ScmObj*, align 8
%fptrToInt55085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48285 to i64
%ae48285 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55085)
store volatile %struct.ScmObj* %ae48285, %struct.ScmObj** %stackaddr$makeclosure55084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %lsts47097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48286 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55086 = alloca %struct.ScmObj*, align 8
%fptrToInt55087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48287 to i64
%ae48287 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55087)
store volatile %struct.ScmObj* %ae48287, %struct.ScmObj** %stackaddr$makeclosure55086, align 8
%argslist53629$ae482850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist53629$ae482851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48287, %struct.ScmObj* %argslist53629$ae482850)
store volatile %struct.ScmObj* %argslist53629$ae482851, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%argslist53629$ae482852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48286, %struct.ScmObj* %argslist53629$ae482851)
store volatile %struct.ScmObj* %argslist53629$ae482852, %struct.ScmObj** %stackaddr$prim55089, align 8
%clofunc55090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48285)
musttail call tailcc void %clofunc55090(%struct.ScmObj* %ae48285, %struct.ScmObj* %argslist53629$ae482852)
ret void
}

define tailcc void @proc_clo$ae48285(%struct.ScmObj* %env$ae48285,%struct.ScmObj* %current_45args53602) {
%stackaddr$env-ref55091 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55091
%stackaddr$env-ref55092 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55092
%stackaddr$env-ref55093 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55093
%stackaddr$env-ref55094 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55094
%stackaddr$env-ref55095 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55095
%stackaddr$env-ref55096 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55096
%stackaddr$env-ref55097 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 6)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55097
%stackaddr$env-ref55098 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55098
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53602)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%current_45args53603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53602)
store volatile %struct.ScmObj* %current_45args53603, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$makeclosure55102 = alloca %struct.ScmObj*, align 8
%fptrToInt55103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48306 to i64
%ae48306 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55103)
store volatile %struct.ScmObj* %ae48306, %struct.ScmObj** %stackaddr$makeclosure55102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53624$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%argslist53624$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53624$_37map1470850)
store volatile %struct.ScmObj* %argslist53624$_37map1470851, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%argslist53624$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist53624$_37map1470851)
store volatile %struct.ScmObj* %argslist53624$_37map1470852, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%argslist53624$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48306, %struct.ScmObj* %argslist53624$_37map1470852)
store volatile %struct.ScmObj* %argslist53624$_37map1470853, %struct.ScmObj** %stackaddr$prim55106, align 8
%clofunc55107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55107(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53624$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48306(%struct.ScmObj* %env$ae48306,%struct.ScmObj* %current_45args53605) {
%stackaddr$env-ref55108 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55108
%stackaddr$env-ref55109 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55109
%stackaddr$env-ref55110 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55110
%stackaddr$env-ref55111 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55111
%stackaddr$env-ref55112 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55112
%stackaddr$env-ref55113 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55113
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%_95k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53605)
store volatile %struct.ScmObj* %_95k47448, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%current_45args53606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53605)
store volatile %struct.ScmObj* %current_45args53606, %struct.ScmObj** %stackaddr$prim55115, align 8
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53606)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim55116, align 8
%stackaddr$makeclosure55117 = alloca %struct.ScmObj*, align 8
%fptrToInt55118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48309 to i64
%ae48309 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55118)
store volatile %struct.ScmObj* %ae48309, %struct.ScmObj** %stackaddr$makeclosure55117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55119 = alloca %struct.ScmObj*, align 8
%fptrToInt55120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48311 to i64
%ae48311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55120)
store volatile %struct.ScmObj* %ae48311, %struct.ScmObj** %stackaddr$makeclosure55119, align 8
%argslist53623$ae483090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%argslist53623$ae483091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48311, %struct.ScmObj* %argslist53623$ae483090)
store volatile %struct.ScmObj* %argslist53623$ae483091, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%argslist53623$ae483092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48310, %struct.ScmObj* %argslist53623$ae483091)
store volatile %struct.ScmObj* %argslist53623$ae483092, %struct.ScmObj** %stackaddr$prim55122, align 8
%clofunc55123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48309)
musttail call tailcc void %clofunc55123(%struct.ScmObj* %ae48309, %struct.ScmObj* %argslist53623$ae483092)
ret void
}

define tailcc void @proc_clo$ae48309(%struct.ScmObj* %env$ae48309,%struct.ScmObj* %current_45args53608) {
%stackaddr$env-ref55124 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55124
%stackaddr$env-ref55125 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55125
%stackaddr$env-ref55126 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55126
%stackaddr$env-ref55127 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55127
%stackaddr$env-ref55128 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55128
%stackaddr$env-ref55129 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55129
%stackaddr$env-ref55130 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55130
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53608)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%current_45args53609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53608)
store volatile %struct.ScmObj* %current_45args53609, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53609)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim55134, align 8
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47235)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55135, align 8
%stackaddr$makeclosure55136 = alloca %struct.ScmObj*, align 8
%fptrToInt55137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48335 to i64
%ae48335 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55137)
store volatile %struct.ScmObj* %ae48335, %struct.ScmObj** %stackaddr$makeclosure55136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48335, %struct.ScmObj* %anf_45bind47234, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48335, %struct.ScmObj* %k47442, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48335, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48335, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48335, %struct.ScmObj* %f47099, i64 4)
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%cpsargs47453 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48335, %struct.ScmObj* %anf_45bind47236)
store volatile %struct.ScmObj* %cpsargs47453, %struct.ScmObj** %stackaddr$prim55138, align 8
%clofunc55139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55139(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47453)
ret void
}

define tailcc void @proc_clo$ae48335(%struct.ScmObj* %env$ae48335,%struct.ScmObj* %current_45args53611) {
%stackaddr$env-ref55140 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48335, i64 0)
store %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$env-ref55140
%stackaddr$env-ref55141 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48335, i64 1)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55141
%stackaddr$env-ref55142 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48335, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55142
%stackaddr$env-ref55143 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48335, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55143
%stackaddr$env-ref55144 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48335, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55144
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53611)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%current_45args53612 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53611)
store volatile %struct.ScmObj* %current_45args53612, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53612)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55147, align 8
%ae48340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %ae48340)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$makeclosure55149 = alloca %struct.ScmObj*, align 8
%fptrToInt55150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48342 to i64
%ae48342 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55150)
store volatile %struct.ScmObj* %ae48342, %struct.ScmObj** %stackaddr$makeclosure55149, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %k47442, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %f47099, i64 1)
%argslist53617$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist53617$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist53617$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53617$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55151, align 8
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%argslist53617$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist53617$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53617$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55152, align 8
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%argslist53617$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist53617$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53617$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%argslist53617$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48342, %struct.ScmObj* %argslist53617$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53617$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55154, align 8
%clofunc55155 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55155(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53617$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48342(%struct.ScmObj* %env$ae48342,%struct.ScmObj* %current_45args53614) {
%stackaddr$env-ref55156 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 0)
store %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$env-ref55156
%stackaddr$env-ref55157 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55157
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%_95k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53614)
store volatile %struct.ScmObj* %_95k47451, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%current_45args53615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53614)
store volatile %struct.ScmObj* %current_45args53615, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim55160, align 8
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%cpsargs47452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47442, %struct.ScmObj* %anf_45bind47239)
store volatile %struct.ScmObj* %cpsargs47452, %struct.ScmObj** %stackaddr$prim55161, align 8
%clofunc55162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55162(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47452)
ret void
}

define tailcc void @proc_clo$ae48311(%struct.ScmObj* %env$ae48311,%struct.ScmObj* %current_45args53618) {
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53618)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%current_45args53619 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53618)
store volatile %struct.ScmObj* %current_45args53619, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53619)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55165, align 8
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%current_45args53620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53619)
store volatile %struct.ScmObj* %current_45args53620, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53620)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55167, align 8
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%cpsprim47455 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47455, %struct.ScmObj** %stackaddr$prim55168, align 8
%ae48315 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53622$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%argslist53622$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47455, %struct.ScmObj* %argslist53622$k474540)
store volatile %struct.ScmObj* %argslist53622$k474541, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%argslist53622$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48315, %struct.ScmObj* %argslist53622$k474541)
store volatile %struct.ScmObj* %argslist53622$k474542, %struct.ScmObj** %stackaddr$prim55170, align 8
%clofunc55171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc55171(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist53622$k474542)
ret void
}

define tailcc void @proc_clo$ae48287(%struct.ScmObj* %env$ae48287,%struct.ScmObj* %current_45args53625) {
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53625)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim55172, align 8
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%current_45args53626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53625)
store volatile %struct.ScmObj* %current_45args53626, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53626)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55174, align 8
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%cpsprim47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47457, %struct.ScmObj** %stackaddr$prim55175, align 8
%ae48290 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53628$k474560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55176 = alloca %struct.ScmObj*, align 8
%argslist53628$k474561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47457, %struct.ScmObj* %argslist53628$k474560)
store volatile %struct.ScmObj* %argslist53628$k474561, %struct.ScmObj** %stackaddr$prim55176, align 8
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%argslist53628$k474562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48290, %struct.ScmObj* %argslist53628$k474561)
store volatile %struct.ScmObj* %argslist53628$k474562, %struct.ScmObj** %stackaddr$prim55177, align 8
%clofunc55178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47456)
musttail call tailcc void %clofunc55178(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist53628$k474562)
ret void
}

define tailcc void @proc_clo$ae48263(%struct.ScmObj* %env$ae48263,%struct.ScmObj* %current_45args53631) {
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53631)
store volatile %struct.ScmObj* %k47458, %struct.ScmObj** %stackaddr$prim55179, align 8
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%current_45args53632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53631)
store volatile %struct.ScmObj* %current_45args53632, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53632)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim55181, align 8
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%cpsprim47459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47459, %struct.ScmObj** %stackaddr$prim55182, align 8
%ae48266 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53634$k474580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%argslist53634$k474581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47459, %struct.ScmObj* %argslist53634$k474580)
store volatile %struct.ScmObj* %argslist53634$k474581, %struct.ScmObj** %stackaddr$prim55183, align 8
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%argslist53634$k474582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48266, %struct.ScmObj* %argslist53634$k474581)
store volatile %struct.ScmObj* %argslist53634$k474582, %struct.ScmObj** %stackaddr$prim55184, align 8
%clofunc55185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47458)
musttail call tailcc void %clofunc55185(%struct.ScmObj* %k47458, %struct.ScmObj* %argslist53634$k474582)
ret void
}

define tailcc void @proc_clo$ae48215(%struct.ScmObj* %env$ae48215,%struct.ScmObj* %current_45args53637) {
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53637)
store volatile %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%current_45args53638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53637)
store volatile %struct.ScmObj* %current_45args53638, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53638)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%current_45args53639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53638)
store volatile %struct.ScmObj* %current_45args53639, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53639)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim55190, align 8
%truthy$cmp55191 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp55191 = icmp eq i64 %truthy$cmp55191, 1
br i1 %cmp$cmp55191, label %truebranch$cmp55191, label %falsebranch$cmp55191
truebranch$cmp55191:
%ae48218 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53641$k474600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%argslist53641$k474601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist53641$k474600)
store volatile %struct.ScmObj* %argslist53641$k474601, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist53641$k474602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48218, %struct.ScmObj* %argslist53641$k474601)
store volatile %struct.ScmObj* %argslist53641$k474602, %struct.ScmObj** %stackaddr$prim55193, align 8
%clofunc55194 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47460)
musttail call tailcc void %clofunc55194(%struct.ScmObj* %k47460, %struct.ScmObj* %argslist53641$k474602)
ret void
falsebranch$cmp55191:
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%cpsprim47461 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47461, %struct.ScmObj** %stackaddr$prim55195, align 8
%ae48225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53642$k474600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%argslist53642$k474601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47461, %struct.ScmObj* %argslist53642$k474600)
store volatile %struct.ScmObj* %argslist53642$k474601, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%argslist53642$k474602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48225, %struct.ScmObj* %argslist53642$k474601)
store volatile %struct.ScmObj* %argslist53642$k474602, %struct.ScmObj** %stackaddr$prim55197, align 8
%clofunc55198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47460)
musttail call tailcc void %clofunc55198(%struct.ScmObj* %k47460, %struct.ScmObj* %argslist53642$k474602)
ret void
}

define tailcc void @proc_clo$ae48172(%struct.ScmObj* %env$ae48172,%struct.ScmObj* %current_45args53646) {
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48172, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$env-ref55200 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48172, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55200
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%current_45args53647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %current_45args53647, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%current_45args53648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %current_45args53648, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53648)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$makeclosure55206 = alloca %struct.ScmObj*, align 8
%fptrToInt55207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48174 to i64
%ae48174 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55207)
store volatile %struct.ScmObj* %ae48174, %struct.ScmObj** %stackaddr$makeclosure55206, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %k47462, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %lst47110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %n47109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %_37take47081, i64 3)
%argslist53654$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist53654$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53654$_37length470780)
store volatile %struct.ScmObj* %argslist53654$_37length470781, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist53654$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48174, %struct.ScmObj* %argslist53654$_37length470781)
store volatile %struct.ScmObj* %argslist53654$_37length470782, %struct.ScmObj** %stackaddr$prim55209, align 8
%clofunc55210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc55210(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist53654$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48174(%struct.ScmObj* %env$ae48174,%struct.ScmObj* %current_45args53650) {
%stackaddr$env-ref55211 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref55211
%stackaddr$env-ref55212 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 1)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref55212
%stackaddr$env-ref55213 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 2)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref55213
%stackaddr$env-ref55214 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55214
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%_95k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53650)
store volatile %struct.ScmObj* %_95k47463, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%current_45args53651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53650)
store volatile %struct.ScmObj* %current_45args53651, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53651)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim55218, align 8
%argslist53653$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%argslist53653$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist53653$_37take470810)
store volatile %struct.ScmObj* %argslist53653$_37take470811, %struct.ScmObj** %stackaddr$prim55219, align 8
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%argslist53653$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53653$_37take470811)
store volatile %struct.ScmObj* %argslist53653$_37take470812, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%argslist53653$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist53653$_37take470812)
store volatile %struct.ScmObj* %argslist53653$_37take470813, %struct.ScmObj** %stackaddr$prim55221, align 8
%clofunc55222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc55222(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist53653$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48118(%struct.ScmObj* %env$ae48118,%struct.ScmObj* %current_45args53656) {
%stackaddr$env-ref55223 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48118, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55223
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53656)
store volatile %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$prim55224, align 8
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%current_45args53657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53656)
store volatile %struct.ScmObj* %current_45args53657, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53657)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim55226, align 8
%stackaddr$makeclosure55227 = alloca %struct.ScmObj*, align 8
%fptrToInt55228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48119 to i64
%ae48119 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55228)
store volatile %struct.ScmObj* %ae48119, %struct.ScmObj** %stackaddr$makeclosure55227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48119, %struct.ScmObj* %k47464, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48119, %struct.ScmObj* %lst47112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48119, %struct.ScmObj* %_37foldl147073, i64 2)
%ae48120 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55229 = alloca %struct.ScmObj*, align 8
%fptrToInt55230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48121 to i64
%ae48121 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55230)
store volatile %struct.ScmObj* %ae48121, %struct.ScmObj** %stackaddr$makeclosure55229, align 8
%argslist53668$ae481190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%argslist53668$ae481191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48121, %struct.ScmObj* %argslist53668$ae481190)
store volatile %struct.ScmObj* %argslist53668$ae481191, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%argslist53668$ae481192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48120, %struct.ScmObj* %argslist53668$ae481191)
store volatile %struct.ScmObj* %argslist53668$ae481192, %struct.ScmObj** %stackaddr$prim55232, align 8
%clofunc55233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48119)
musttail call tailcc void %clofunc55233(%struct.ScmObj* %ae48119, %struct.ScmObj* %argslist53668$ae481192)
ret void
}

define tailcc void @proc_clo$ae48119(%struct.ScmObj* %env$ae48119,%struct.ScmObj* %current_45args53659) {
%stackaddr$env-ref55234 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48119, i64 0)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref55234
%stackaddr$env-ref55235 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48119, i64 1)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref55235
%stackaddr$env-ref55236 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48119, i64 2)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55236
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53659)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%current_45args53660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53659)
store volatile %struct.ScmObj* %current_45args53660, %struct.ScmObj** %stackaddr$prim55238, align 8
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53660)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim55239, align 8
%ae48140 = call %struct.ScmObj* @const_init_null()
%argslist53662$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%argslist53662$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist53662$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53662$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%argslist53662$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48140, %struct.ScmObj* %argslist53662$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53662$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%argslist53662$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist53662$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53662$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist53662$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist53662$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53662$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55243, align 8
%clofunc55244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55244(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53662$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48121(%struct.ScmObj* %env$ae48121,%struct.ScmObj* %current_45args53663) {
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$prim55245, align 8
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%current_45args53664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %current_45args53664, %struct.ScmObj** %stackaddr$prim55246, align 8
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim55247, align 8
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%current_45args53665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %current_45args53665, %struct.ScmObj** %stackaddr$prim55248, align 8
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53665)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim55249, align 8
%ae48123 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53667$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%argslist53667$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist53667$k474660)
store volatile %struct.ScmObj* %argslist53667$k474661, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%argslist53667$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48123, %struct.ScmObj* %argslist53667$k474661)
store volatile %struct.ScmObj* %argslist53667$k474662, %struct.ScmObj** %stackaddr$prim55251, align 8
%clofunc55252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc55252(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist53667$k474662)
ret void
}

define tailcc void @proc_clo$ae48039(%struct.ScmObj* %env$ae48039,%struct.ScmObj* %current_45args53671) {
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %k47467, %struct.ScmObj** %stackaddr$prim55253, align 8
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%current_45args53672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %current_45args53672, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53672)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim55255, align 8
%ae48041 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55256 = alloca %struct.ScmObj*, align 8
%fptrToInt55257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48042 to i64
%ae48042 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55257)
store volatile %struct.ScmObj* %ae48042, %struct.ScmObj** %stackaddr$makeclosure55256, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48042, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53685$k474670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%argslist53685$k474671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48042, %struct.ScmObj* %argslist53685$k474670)
store volatile %struct.ScmObj* %argslist53685$k474671, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%argslist53685$k474672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48041, %struct.ScmObj* %argslist53685$k474671)
store volatile %struct.ScmObj* %argslist53685$k474672, %struct.ScmObj** %stackaddr$prim55259, align 8
%clofunc55260 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47467)
musttail call tailcc void %clofunc55260(%struct.ScmObj* %k47467, %struct.ScmObj* %argslist53685$k474672)
ret void
}

define tailcc void @proc_clo$ae48042(%struct.ScmObj* %env$ae48042,%struct.ScmObj* %current_45args53674) {
%stackaddr$env-ref55261 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48042, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55261
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53674)
store volatile %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$prim55262, align 8
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%current_45args53675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53674)
store volatile %struct.ScmObj* %current_45args53675, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53675)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%current_45args53676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53675)
store volatile %struct.ScmObj* %current_45args53676, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53676)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%current_45args53677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53676)
store volatile %struct.ScmObj* %current_45args53677, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53677)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim55269, align 8
%truthy$cmp55270 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47220)
%cmp$cmp55270 = icmp eq i64 %truthy$cmp55270, 1
br i1 %cmp$cmp55270, label %truebranch$cmp55270, label %falsebranch$cmp55270
truebranch$cmp55270:
%ae48046 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53679$k474680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%argslist53679$k474681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53679$k474680)
store volatile %struct.ScmObj* %argslist53679$k474681, %struct.ScmObj** %stackaddr$prim55271, align 8
%stackaddr$prim55272 = alloca %struct.ScmObj*, align 8
%argslist53679$k474682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48046, %struct.ScmObj* %argslist53679$k474681)
store volatile %struct.ScmObj* %argslist53679$k474682, %struct.ScmObj** %stackaddr$prim55272, align 8
%clofunc55273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47468)
musttail call tailcc void %clofunc55273(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist53679$k474682)
ret void
falsebranch$cmp55270:
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$makeclosure55275 = alloca %struct.ScmObj*, align 8
%fptrToInt55276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48053 to i64
%ae48053 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55276)
store volatile %struct.ScmObj* %ae48053, %struct.ScmObj** %stackaddr$makeclosure55275, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48053, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48053, %struct.ScmObj* %lst47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48053, %struct.ScmObj* %_37foldl147074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48053, %struct.ScmObj* %k47468, i64 3)
%argslist53684$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%argslist53684$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53684$f470770)
store volatile %struct.ScmObj* %argslist53684$f470771, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%argslist53684$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist53684$f470771)
store volatile %struct.ScmObj* %argslist53684$f470772, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%argslist53684$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48053, %struct.ScmObj* %argslist53684$f470772)
store volatile %struct.ScmObj* %argslist53684$f470773, %struct.ScmObj** %stackaddr$prim55279, align 8
%clofunc55280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc55280(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53684$f470773)
ret void
}

define tailcc void @proc_clo$ae48053(%struct.ScmObj* %env$ae48053,%struct.ScmObj* %current_45args53680) {
%stackaddr$env-ref55281 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48053, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref55281
%stackaddr$env-ref55282 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48053, i64 1)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref55282
%stackaddr$env-ref55283 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48053, i64 2)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55283
%stackaddr$env-ref55284 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48053, i64 3)
store %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$env-ref55284
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%_95k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53680)
store volatile %struct.ScmObj* %_95k47469, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%current_45args53681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53680)
store volatile %struct.ScmObj* %current_45args53681, %struct.ScmObj** %stackaddr$prim55286, align 8
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53681)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim55288, align 8
%argslist53683$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist53683$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53683$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist53683$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53683$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53683$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53683$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%argslist53683$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist53683$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53683$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55292, align 8
%clofunc55293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55293(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53683$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47956(%struct.ScmObj* %env$ae47956,%struct.ScmObj* %current_45args53688) {
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53688)
store volatile %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%current_45args53689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53688)
store volatile %struct.ScmObj* %current_45args53689, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53689)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim55296, align 8
%ae47958 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55297 = alloca %struct.ScmObj*, align 8
%fptrToInt55298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47959 to i64
%ae47959 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55298)
store volatile %struct.ScmObj* %ae47959, %struct.ScmObj** %stackaddr$makeclosure55297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47959, %struct.ScmObj* %_37length47079, i64 0)
%argslist53700$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%argslist53700$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47959, %struct.ScmObj* %argslist53700$k474700)
store volatile %struct.ScmObj* %argslist53700$k474701, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%argslist53700$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47958, %struct.ScmObj* %argslist53700$k474701)
store volatile %struct.ScmObj* %argslist53700$k474702, %struct.ScmObj** %stackaddr$prim55300, align 8
%clofunc55301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc55301(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist53700$k474702)
ret void
}

define tailcc void @proc_clo$ae47959(%struct.ScmObj* %env$ae47959,%struct.ScmObj* %current_45args53691) {
%stackaddr$env-ref55302 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47959, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref55302
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53691)
store volatile %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%current_45args53692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53691)
store volatile %struct.ScmObj* %current_45args53692, %struct.ScmObj** %stackaddr$prim55304, align 8
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53692)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim55306, align 8
%truthy$cmp55307 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47216)
%cmp$cmp55307 = icmp eq i64 %truthy$cmp55307, 1
br i1 %cmp$cmp55307, label %truebranch$cmp55307, label %falsebranch$cmp55307
truebranch$cmp55307:
%ae47963 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47964 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53694$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%argslist53694$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47964, %struct.ScmObj* %argslist53694$k474710)
store volatile %struct.ScmObj* %argslist53694$k474711, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%argslist53694$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47963, %struct.ScmObj* %argslist53694$k474711)
store volatile %struct.ScmObj* %argslist53694$k474712, %struct.ScmObj** %stackaddr$prim55309, align 8
%clofunc55310 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc55310(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist53694$k474712)
ret void
falsebranch$cmp55307:
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim55311, align 8
%stackaddr$makeclosure55312 = alloca %struct.ScmObj*, align 8
%fptrToInt55313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47973 to i64
%ae47973 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55313)
store volatile %struct.ScmObj* %ae47973, %struct.ScmObj** %stackaddr$makeclosure55312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47973, %struct.ScmObj* %k47471, i64 0)
%argslist53699$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%argslist53699$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist53699$_37length470790)
store volatile %struct.ScmObj* %argslist53699$_37length470791, %struct.ScmObj** %stackaddr$prim55314, align 8
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%argslist53699$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47973, %struct.ScmObj* %argslist53699$_37length470791)
store volatile %struct.ScmObj* %argslist53699$_37length470792, %struct.ScmObj** %stackaddr$prim55315, align 8
%clofunc55316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc55316(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53699$_37length470792)
ret void
}

define tailcc void @proc_clo$ae47973(%struct.ScmObj* %env$ae47973,%struct.ScmObj* %current_45args53695) {
%stackaddr$env-ref55317 = alloca %struct.ScmObj*, align 8
%k47471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47973, i64 0)
store %struct.ScmObj* %k47471, %struct.ScmObj** %stackaddr$env-ref55317
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53695)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%current_45args53696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53695)
store volatile %struct.ScmObj* %current_45args53696, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53696)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim55320, align 8
%ae47975 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%cpsprim47473 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47975, %struct.ScmObj* %anf_45bind47218)
store volatile %struct.ScmObj* %cpsprim47473, %struct.ScmObj** %stackaddr$prim55321, align 8
%ae47978 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53698$k474710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%argslist53698$k474711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47473, %struct.ScmObj* %argslist53698$k474710)
store volatile %struct.ScmObj* %argslist53698$k474711, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%argslist53698$k474712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47978, %struct.ScmObj* %argslist53698$k474711)
store volatile %struct.ScmObj* %argslist53698$k474712, %struct.ScmObj** %stackaddr$prim55323, align 8
%clofunc55324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47471)
musttail call tailcc void %clofunc55324(%struct.ScmObj* %k47471, %struct.ScmObj* %argslist53698$k474712)
ret void
}

define tailcc void @proc_clo$ae47806(%struct.ScmObj* %env$ae47806,%struct.ScmObj* %current_45args53703) {
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53703)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%current_45args53704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53703)
store volatile %struct.ScmObj* %current_45args53704, %struct.ScmObj** %stackaddr$prim55326, align 8
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53704)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim55327, align 8
%ae47808 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55328 = alloca %struct.ScmObj*, align 8
%fptrToInt55329 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47809 to i64
%ae47809 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55329)
store volatile %struct.ScmObj* %ae47809, %struct.ScmObj** %stackaddr$makeclosure55328, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47809, %struct.ScmObj* %_37take47082, i64 0)
%argslist53717$k474740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist53717$k474741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47809, %struct.ScmObj* %argslist53717$k474740)
store volatile %struct.ScmObj* %argslist53717$k474741, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%argslist53717$k474742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47808, %struct.ScmObj* %argslist53717$k474741)
store volatile %struct.ScmObj* %argslist53717$k474742, %struct.ScmObj** %stackaddr$prim55331, align 8
%clofunc55332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47474)
musttail call tailcc void %clofunc55332(%struct.ScmObj* %k47474, %struct.ScmObj* %argslist53717$k474742)
ret void
}

define tailcc void @proc_clo$ae47809(%struct.ScmObj* %env$ae47809,%struct.ScmObj* %current_45args53706) {
%stackaddr$env-ref55333 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47809, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55333
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53706)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%current_45args53707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53706)
store volatile %struct.ScmObj* %current_45args53707, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53707)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim55336, align 8
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%current_45args53708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53707)
store volatile %struct.ScmObj* %current_45args53708, %struct.ScmObj** %stackaddr$prim55337, align 8
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53708)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim55338, align 8
%ae47811 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47811)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim55339, align 8
%truthy$cmp55340 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47209)
%cmp$cmp55340 = icmp eq i64 %truthy$cmp55340, 1
br i1 %cmp$cmp55340, label %truebranch$cmp55340, label %falsebranch$cmp55340
truebranch$cmp55340:
%ae47814 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47815 = call %struct.ScmObj* @const_init_null()
%argslist53710$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%argslist53710$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47815, %struct.ScmObj* %argslist53710$k474750)
store volatile %struct.ScmObj* %argslist53710$k474751, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%argslist53710$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47814, %struct.ScmObj* %argslist53710$k474751)
store volatile %struct.ScmObj* %argslist53710$k474752, %struct.ScmObj** %stackaddr$prim55342, align 8
%clofunc55343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55343(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53710$k474752)
ret void
falsebranch$cmp55340:
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55344, align 8
%truthy$cmp55345 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47210)
%cmp$cmp55345 = icmp eq i64 %truthy$cmp55345, 1
br i1 %cmp$cmp55345, label %truebranch$cmp55345, label %falsebranch$cmp55345
truebranch$cmp55345:
%ae47825 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47826 = call %struct.ScmObj* @const_init_null()
%argslist53711$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%argslist53711$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47826, %struct.ScmObj* %argslist53711$k474750)
store volatile %struct.ScmObj* %argslist53711$k474751, %struct.ScmObj** %stackaddr$prim55346, align 8
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%argslist53711$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47825, %struct.ScmObj* %argslist53711$k474751)
store volatile %struct.ScmObj* %argslist53711$k474752, %struct.ScmObj** %stackaddr$prim55347, align 8
%clofunc55348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55348(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53711$k474752)
ret void
falsebranch$cmp55345:
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55349, align 8
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim55350, align 8
%ae47836 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47836)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$makeclosure55352 = alloca %struct.ScmObj*, align 8
%fptrToInt55353 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47838 to i64
%ae47838 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55353)
store volatile %struct.ScmObj* %ae47838, %struct.ScmObj** %stackaddr$makeclosure55352, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47838, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47838, %struct.ScmObj* %anf_45bind47211, i64 1)
%argslist53716$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%argslist53716$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist53716$_37take470820)
store volatile %struct.ScmObj* %argslist53716$_37take470821, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%argslist53716$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %argslist53716$_37take470821)
store volatile %struct.ScmObj* %argslist53716$_37take470822, %struct.ScmObj** %stackaddr$prim55355, align 8
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%argslist53716$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47838, %struct.ScmObj* %argslist53716$_37take470822)
store volatile %struct.ScmObj* %argslist53716$_37take470823, %struct.ScmObj** %stackaddr$prim55356, align 8
%clofunc55357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc55357(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53716$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47838(%struct.ScmObj* %env$ae47838,%struct.ScmObj* %current_45args53712) {
%stackaddr$env-ref55358 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47838, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55358
%stackaddr$env-ref55359 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47838, i64 1)
store %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$env-ref55359
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53712)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim55360, align 8
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%current_45args53713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53712)
store volatile %struct.ScmObj* %current_45args53713, %struct.ScmObj** %stackaddr$prim55361, align 8
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53713)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55362, align 8
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%cpsprim47477 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %anf_45bind47214)
store volatile %struct.ScmObj* %cpsprim47477, %struct.ScmObj** %stackaddr$prim55363, align 8
%ae47844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53715$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist53715$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47477, %struct.ScmObj* %argslist53715$k474750)
store volatile %struct.ScmObj* %argslist53715$k474751, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%argslist53715$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47844, %struct.ScmObj* %argslist53715$k474751)
store volatile %struct.ScmObj* %argslist53715$k474752, %struct.ScmObj** %stackaddr$prim55365, align 8
%clofunc55366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55366(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist53715$k474752)
ret void
}

define tailcc void @proc_clo$ae47709(%struct.ScmObj* %env$ae47709,%struct.ScmObj* %current_45args53720) {
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53720)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim55367, align 8
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%current_45args53721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53720)
store volatile %struct.ScmObj* %current_45args53721, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53721)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim55369, align 8
%ae47711 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55370 = alloca %struct.ScmObj*, align 8
%fptrToInt55371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47712 to i64
%ae47712 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55371)
store volatile %struct.ScmObj* %ae47712, %struct.ScmObj** %stackaddr$makeclosure55370, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47712, %struct.ScmObj* %_37map47086, i64 0)
%argslist53737$k474780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%argslist53737$k474781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47712, %struct.ScmObj* %argslist53737$k474780)
store volatile %struct.ScmObj* %argslist53737$k474781, %struct.ScmObj** %stackaddr$prim55372, align 8
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%argslist53737$k474782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47711, %struct.ScmObj* %argslist53737$k474781)
store volatile %struct.ScmObj* %argslist53737$k474782, %struct.ScmObj** %stackaddr$prim55373, align 8
%clofunc55374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47478)
musttail call tailcc void %clofunc55374(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist53737$k474782)
ret void
}

define tailcc void @proc_clo$ae47712(%struct.ScmObj* %env$ae47712,%struct.ScmObj* %current_45args53723) {
%stackaddr$env-ref55375 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47712, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55375
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53723)
store volatile %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%current_45args53724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53723)
store volatile %struct.ScmObj* %current_45args53724, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53724)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%current_45args53725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53724)
store volatile %struct.ScmObj* %current_45args53725, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53725)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim55381, align 8
%truthy$cmp55382 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47203)
%cmp$cmp55382 = icmp eq i64 %truthy$cmp55382, 1
br i1 %cmp$cmp55382, label %truebranch$cmp55382, label %falsebranch$cmp55382
truebranch$cmp55382:
%ae47716 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47717 = call %struct.ScmObj* @const_init_null()
%argslist53727$k474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%argslist53727$k474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47717, %struct.ScmObj* %argslist53727$k474790)
store volatile %struct.ScmObj* %argslist53727$k474791, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%argslist53727$k474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47716, %struct.ScmObj* %argslist53727$k474791)
store volatile %struct.ScmObj* %argslist53727$k474792, %struct.ScmObj** %stackaddr$prim55384, align 8
%clofunc55385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47479)
musttail call tailcc void %clofunc55385(%struct.ScmObj* %k47479, %struct.ScmObj* %argslist53727$k474792)
ret void
falsebranch$cmp55382:
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim55386, align 8
%stackaddr$makeclosure55387 = alloca %struct.ScmObj*, align 8
%fptrToInt55388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47726 to i64
%ae47726 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55388)
store volatile %struct.ScmObj* %ae47726, %struct.ScmObj** %stackaddr$makeclosure55387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47726, %struct.ScmObj* %f47088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47726, %struct.ScmObj* %lst47087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47726, %struct.ScmObj* %_37map47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47726, %struct.ScmObj* %k47479, i64 3)
%argslist53736$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%argslist53736$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %argslist53736$f470880)
store volatile %struct.ScmObj* %argslist53736$f470881, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist53736$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47726, %struct.ScmObj* %argslist53736$f470881)
store volatile %struct.ScmObj* %argslist53736$f470882, %struct.ScmObj** %stackaddr$prim55390, align 8
%clofunc55391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc55391(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53736$f470882)
ret void
}

define tailcc void @proc_clo$ae47726(%struct.ScmObj* %env$ae47726,%struct.ScmObj* %current_45args53728) {
%stackaddr$env-ref55392 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47726, i64 0)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref55392
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47726, i64 1)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$env-ref55394 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47726, i64 2)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55394
%stackaddr$env-ref55395 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47726, i64 3)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref55395
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53728)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%current_45args53729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53728)
store volatile %struct.ScmObj* %current_45args53729, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53729)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$makeclosure55400 = alloca %struct.ScmObj*, align 8
%fptrToInt55401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47730 to i64
%ae47730 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55401)
store volatile %struct.ScmObj* %ae47730, %struct.ScmObj** %stackaddr$makeclosure55400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47730, %struct.ScmObj* %anf_45bind47205, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47730, %struct.ScmObj* %k47479, i64 1)
%argslist53735$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist53735$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist53735$_37map470860)
store volatile %struct.ScmObj* %argslist53735$_37map470861, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%argslist53735$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53735$_37map470861)
store volatile %struct.ScmObj* %argslist53735$_37map470862, %struct.ScmObj** %stackaddr$prim55403, align 8
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%argslist53735$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47730, %struct.ScmObj* %argslist53735$_37map470862)
store volatile %struct.ScmObj* %argslist53735$_37map470863, %struct.ScmObj** %stackaddr$prim55404, align 8
%clofunc55405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc55405(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist53735$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47730(%struct.ScmObj* %env$ae47730,%struct.ScmObj* %current_45args53731) {
%stackaddr$env-ref55406 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47730, i64 0)
store %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$env-ref55406
%stackaddr$env-ref55407 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47730, i64 1)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref55407
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53731)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%current_45args53732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53731)
store volatile %struct.ScmObj* %current_45args53732, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53732)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%cpsprim47482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %anf_45bind47207)
store volatile %struct.ScmObj* %cpsprim47482, %struct.ScmObj** %stackaddr$prim55411, align 8
%ae47736 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53734$k474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%argslist53734$k474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47482, %struct.ScmObj* %argslist53734$k474790)
store volatile %struct.ScmObj* %argslist53734$k474791, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%argslist53734$k474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47736, %struct.ScmObj* %argslist53734$k474791)
store volatile %struct.ScmObj* %argslist53734$k474792, %struct.ScmObj** %stackaddr$prim55413, align 8
%clofunc55414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47479)
musttail call tailcc void %clofunc55414(%struct.ScmObj* %k47479, %struct.ScmObj* %argslist53734$k474792)
ret void
}

define tailcc void @proc_clo$ae47629(%struct.ScmObj* %env$ae47629,%struct.ScmObj* %current_45args53740) {
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53740)
store volatile %struct.ScmObj* %k47483, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%current_45args53741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53740)
store volatile %struct.ScmObj* %current_45args53741, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53741)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim55417, align 8
%ae47631 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55418 = alloca %struct.ScmObj*, align 8
%fptrToInt55419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47632 to i64
%ae47632 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55419)
store volatile %struct.ScmObj* %ae47632, %struct.ScmObj** %stackaddr$makeclosure55418, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47632, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53754$k474830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%argslist53754$k474831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47632, %struct.ScmObj* %argslist53754$k474830)
store volatile %struct.ScmObj* %argslist53754$k474831, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%argslist53754$k474832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47631, %struct.ScmObj* %argslist53754$k474831)
store volatile %struct.ScmObj* %argslist53754$k474832, %struct.ScmObj** %stackaddr$prim55421, align 8
%clofunc55422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47483)
musttail call tailcc void %clofunc55422(%struct.ScmObj* %k47483, %struct.ScmObj* %argslist53754$k474832)
ret void
}

define tailcc void @proc_clo$ae47632(%struct.ScmObj* %env$ae47632,%struct.ScmObj* %current_45args53743) {
%stackaddr$env-ref55423 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47632, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55423
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53743)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim55424, align 8
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%current_45args53744 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53743)
store volatile %struct.ScmObj* %current_45args53744, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53744)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%current_45args53745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53744)
store volatile %struct.ScmObj* %current_45args53745, %struct.ScmObj** %stackaddr$prim55427, align 8
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53745)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%current_45args53746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53745)
store volatile %struct.ScmObj* %current_45args53746, %struct.ScmObj** %stackaddr$prim55429, align 8
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53746)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim55431, align 8
%truthy$cmp55432 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47198)
%cmp$cmp55432 = icmp eq i64 %truthy$cmp55432, 1
br i1 %cmp$cmp55432, label %truebranch$cmp55432, label %falsebranch$cmp55432
truebranch$cmp55432:
%ae47636 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53748$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist53748$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53748$k474840)
store volatile %struct.ScmObj* %argslist53748$k474841, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%argslist53748$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47636, %struct.ScmObj* %argslist53748$k474841)
store volatile %struct.ScmObj* %argslist53748$k474842, %struct.ScmObj** %stackaddr$prim55434, align 8
%clofunc55435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc55435(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist53748$k474842)
ret void
falsebranch$cmp55432:
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$makeclosure55438 = alloca %struct.ScmObj*, align 8
%fptrToInt55439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47644 to i64
%ae47644 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55439)
store volatile %struct.ScmObj* %ae47644, %struct.ScmObj** %stackaddr$makeclosure55438, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47644, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47644, %struct.ScmObj* %anf_45bind47199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47644, %struct.ScmObj* %k47484, i64 2)
%argslist53753$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%argslist53753$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53753$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53753$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%argslist53753$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53753$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53753$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55441, align 8
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%argslist53753$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53753$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53753$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%argslist53753$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47644, %struct.ScmObj* %argslist53753$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53753$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55443, align 8
%clofunc55444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55444(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53753$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47644(%struct.ScmObj* %env$ae47644,%struct.ScmObj* %current_45args53749) {
%stackaddr$env-ref55445 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47644, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref55445
%stackaddr$env-ref55446 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47644, i64 1)
store %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$env-ref55446
%stackaddr$env-ref55447 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47644, i64 2)
store %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$env-ref55447
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%current_45args53750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %current_45args53750, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53750)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim55450, align 8
%argslist53752$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%argslist53752$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47201, %struct.ScmObj* %argslist53752$f470930)
store volatile %struct.ScmObj* %argslist53752$f470931, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%argslist53752$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist53752$f470931)
store volatile %struct.ScmObj* %argslist53752$f470932, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist53752$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist53752$f470932)
store volatile %struct.ScmObj* %argslist53752$f470933, %struct.ScmObj** %stackaddr$prim55453, align 8
%clofunc55454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc55454(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53752$f470933)
ret void
}

define tailcc void @proc_clo$ae47512(%struct.ScmObj* %env$ae47512,%struct.ScmObj* %current_45args53757) {
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%k47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53757)
store volatile %struct.ScmObj* %k47486, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%current_45args53758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53757)
store volatile %struct.ScmObj* %current_45args53758, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53758)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim55457, align 8
%ae47514 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55458 = alloca %struct.ScmObj*, align 8
%fptrToInt55459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47515 to i64
%ae47515 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55459)
store volatile %struct.ScmObj* %ae47515, %struct.ScmObj** %stackaddr$makeclosure55458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47515, %struct.ScmObj* %y47070, i64 0)
%argslist53776$k474860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%argslist53776$k474861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47515, %struct.ScmObj* %argslist53776$k474860)
store volatile %struct.ScmObj* %argslist53776$k474861, %struct.ScmObj** %stackaddr$prim55460, align 8
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%argslist53776$k474862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47514, %struct.ScmObj* %argslist53776$k474861)
store volatile %struct.ScmObj* %argslist53776$k474862, %struct.ScmObj** %stackaddr$prim55461, align 8
%clofunc55462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47486)
musttail call tailcc void %clofunc55462(%struct.ScmObj* %k47486, %struct.ScmObj* %argslist53776$k474862)
ret void
}

define tailcc void @proc_clo$ae47515(%struct.ScmObj* %env$ae47515,%struct.ScmObj* %current_45args53760) {
%stackaddr$env-ref55463 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47515, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55463
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53760)
store volatile %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%current_45args53761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53760)
store volatile %struct.ScmObj* %current_45args53761, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53761)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$makeclosure55467 = alloca %struct.ScmObj*, align 8
%fptrToInt55468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47516 to i64
%ae47516 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55468)
store volatile %struct.ScmObj* %ae47516, %struct.ScmObj** %stackaddr$makeclosure55467, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47516, %struct.ScmObj* %k47487, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47516, %struct.ScmObj* %f47071, i64 1)
%ae47517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55469 = alloca %struct.ScmObj*, align 8
%fptrToInt55470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47518 to i64
%ae47518 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55470)
store volatile %struct.ScmObj* %ae47518, %struct.ScmObj** %stackaddr$makeclosure55469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47518, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47518, %struct.ScmObj* %y47070, i64 1)
%argslist53775$ae475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%argslist53775$ae475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47518, %struct.ScmObj* %argslist53775$ae475160)
store volatile %struct.ScmObj* %argslist53775$ae475161, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist53775$ae475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47517, %struct.ScmObj* %argslist53775$ae475161)
store volatile %struct.ScmObj* %argslist53775$ae475162, %struct.ScmObj** %stackaddr$prim55472, align 8
%clofunc55473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47516)
musttail call tailcc void %clofunc55473(%struct.ScmObj* %ae47516, %struct.ScmObj* %argslist53775$ae475162)
ret void
}

define tailcc void @proc_clo$ae47516(%struct.ScmObj* %env$ae47516,%struct.ScmObj* %current_45args53763) {
%stackaddr$env-ref55474 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47516, i64 0)
store %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$env-ref55474
%stackaddr$env-ref55475 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47516, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55475
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%_95k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53763)
store volatile %struct.ScmObj* %_95k47488, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%current_45args53764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53763)
store volatile %struct.ScmObj* %current_45args53764, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53764)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim55478, align 8
%argslist53766$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%argslist53766$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47196, %struct.ScmObj* %argslist53766$f470710)
store volatile %struct.ScmObj* %argslist53766$f470711, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%argslist53766$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47487, %struct.ScmObj* %argslist53766$f470711)
store volatile %struct.ScmObj* %argslist53766$f470712, %struct.ScmObj** %stackaddr$prim55480, align 8
%clofunc55481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc55481(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53766$f470712)
ret void
}

define tailcc void @proc_clo$ae47518(%struct.ScmObj* %env$ae47518,%struct.ScmObj* %args4707247489) {
%stackaddr$env-ref55482 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47518, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55482
%stackaddr$env-ref55483 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47518, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55483
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%k47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247489)
store volatile %struct.ScmObj* %k47490, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247489)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim55485, align 8
%stackaddr$makeclosure55486 = alloca %struct.ScmObj*, align 8
%fptrToInt55487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47522 to i64
%ae47522 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55487)
store volatile %struct.ScmObj* %ae47522, %struct.ScmObj** %stackaddr$makeclosure55486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47522, %struct.ScmObj* %k47490, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47522, %struct.ScmObj* %args47072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47522, %struct.ScmObj* %f47071, i64 2)
%argslist53774$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%argslist53774$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53774$y470700)
store volatile %struct.ScmObj* %argslist53774$y470701, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%argslist53774$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47522, %struct.ScmObj* %argslist53774$y470701)
store volatile %struct.ScmObj* %argslist53774$y470702, %struct.ScmObj** %stackaddr$prim55489, align 8
%clofunc55490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc55490(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53774$y470702)
ret void
}

define tailcc void @proc_clo$ae47522(%struct.ScmObj* %env$ae47522,%struct.ScmObj* %current_45args53767) {
%stackaddr$env-ref55491 = alloca %struct.ScmObj*, align 8
%k47490 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47522, i64 0)
store %struct.ScmObj* %k47490, %struct.ScmObj** %stackaddr$env-ref55491
%stackaddr$env-ref55492 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47522, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55492
%stackaddr$env-ref55493 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47522, i64 2)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55493
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%_95k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53767)
store volatile %struct.ScmObj* %_95k47491, %struct.ScmObj** %stackaddr$prim55494, align 8
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%current_45args53768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53767)
store volatile %struct.ScmObj* %current_45args53768, %struct.ScmObj** %stackaddr$prim55495, align 8
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53768)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim55496, align 8
%stackaddr$makeclosure55497 = alloca %struct.ScmObj*, align 8
%fptrToInt55498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47525 to i64
%ae47525 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55498)
store volatile %struct.ScmObj* %ae47525, %struct.ScmObj** %stackaddr$makeclosure55497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47525, %struct.ScmObj* %k47490, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47525, %struct.ScmObj* %args47072, i64 1)
%argslist53773$anf_45bind471940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%argslist53773$anf_45bind471941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53773$anf_45bind471940)
store volatile %struct.ScmObj* %argslist53773$anf_45bind471941, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%argslist53773$anf_45bind471942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47525, %struct.ScmObj* %argslist53773$anf_45bind471941)
store volatile %struct.ScmObj* %argslist53773$anf_45bind471942, %struct.ScmObj** %stackaddr$prim55500, align 8
%clofunc55501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47194)
musttail call tailcc void %clofunc55501(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist53773$anf_45bind471942)
ret void
}

define tailcc void @proc_clo$ae47525(%struct.ScmObj* %env$ae47525,%struct.ScmObj* %current_45args53770) {
%stackaddr$env-ref55502 = alloca %struct.ScmObj*, align 8
%k47490 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47525, i64 0)
store %struct.ScmObj* %k47490, %struct.ScmObj** %stackaddr$env-ref55502
%stackaddr$env-ref55503 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47525, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55503
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%_95k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53770)
store volatile %struct.ScmObj* %_95k47492, %struct.ScmObj** %stackaddr$prim55504, align 8
%stackaddr$prim55505 = alloca %struct.ScmObj*, align 8
%current_45args53771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53770)
store volatile %struct.ScmObj* %current_45args53771, %struct.ScmObj** %stackaddr$prim55505, align 8
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53771)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%cpsargs47493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47490, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47493, %struct.ScmObj** %stackaddr$prim55507, align 8
%clofunc55508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47195)
musttail call tailcc void %clofunc55508(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %cpsargs47493)
ret void
}

define tailcc void @proc_clo$ae47497(%struct.ScmObj* %env$ae47497,%struct.ScmObj* %current_45args53778) {
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53778)
store volatile %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%current_45args53779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53778)
store volatile %struct.ScmObj* %current_45args53779, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53779)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim55511, align 8
%argslist53781$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist53781$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53781$yu470690)
store volatile %struct.ScmObj* %argslist53781$yu470691, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%argslist53781$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist53781$yu470691)
store volatile %struct.ScmObj* %argslist53781$yu470692, %struct.ScmObj** %stackaddr$prim55513, align 8
%clofunc55514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc55514(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53781$yu470692)
ret void
}