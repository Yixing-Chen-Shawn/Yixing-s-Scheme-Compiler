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
%mainenv54742 = call %struct.ScmObj* @const_init_null()
%mainargs54743 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54742, %struct.ScmObj* %mainargs54743)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54740,%struct.ScmObj* %mainargs54741) {
%stackaddr$makeclosure54744 = alloca %struct.ScmObj*, align 8
%fptrToInt54745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48452 to i64
%ae48452 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54745)
store volatile %struct.ScmObj* %ae48452, %struct.ScmObj** %stackaddr$makeclosure54744, align 8
%ae48453 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48454 to i64
%ae48454 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae48454, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
%argslist54739$ae484520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%argslist54739$ae484521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48454, %struct.ScmObj* %argslist54739$ae484520)
store volatile %struct.ScmObj* %argslist54739$ae484521, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%argslist54739$ae484522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48453, %struct.ScmObj* %argslist54739$ae484521)
store volatile %struct.ScmObj* %argslist54739$ae484522, %struct.ScmObj** %stackaddr$prim54749, align 8
%clofunc54750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48452)
musttail call tailcc void %clofunc54750(%struct.ScmObj* %ae48452, %struct.ScmObj* %argslist54739$ae484522)
ret void
}

define tailcc void @proc_clo$ae48452(%struct.ScmObj* %env$ae48452,%struct.ScmObj* %current_45args54163) {
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%current_45args54164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %current_45args54164, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$makeclosure54754 = alloca %struct.ScmObj*, align 8
%fptrToInt54755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48467 to i64
%ae48467 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54755)
store volatile %struct.ScmObj* %ae48467, %struct.ScmObj** %stackaddr$makeclosure54754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48467, %struct.ScmObj* %anf_45bind48150, i64 0)
%ae48468 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48469 to i64
%ae48469 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae48469, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
%argslist54734$ae484670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%argslist54734$ae484671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48469, %struct.ScmObj* %argslist54734$ae484670)
store volatile %struct.ScmObj* %argslist54734$ae484671, %struct.ScmObj** %stackaddr$prim54758, align 8
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%argslist54734$ae484672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48468, %struct.ScmObj* %argslist54734$ae484671)
store volatile %struct.ScmObj* %argslist54734$ae484672, %struct.ScmObj** %stackaddr$prim54759, align 8
%clofunc54760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48467)
musttail call tailcc void %clofunc54760(%struct.ScmObj* %ae48467, %struct.ScmObj* %argslist54734$ae484672)
ret void
}

define tailcc void @proc_clo$ae48467(%struct.ScmObj* %env$ae48467,%struct.ScmObj* %current_45args54166) {
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48467, i64 0)
store %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$prim54762 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim54762, align 8
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%current_45args54167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %current_45args54167, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$makeclosure54765 = alloca %struct.ScmObj*, align 8
%fptrToInt54766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48582 to i64
%ae48582 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54766)
store volatile %struct.ScmObj* %ae48582, %struct.ScmObj** %stackaddr$makeclosure54765, align 8
%argslist54713$anf_45bind481500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%argslist54713$anf_45bind481501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist54713$anf_45bind481500)
store volatile %struct.ScmObj* %argslist54713$anf_45bind481501, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%argslist54713$anf_45bind481502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48582, %struct.ScmObj* %argslist54713$anf_45bind481501)
store volatile %struct.ScmObj* %argslist54713$anf_45bind481502, %struct.ScmObj** %stackaddr$prim54768, align 8
%clofunc54769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48150)
musttail call tailcc void %clofunc54769(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %argslist54713$anf_45bind481502)
ret void
}

define tailcc void @proc_clo$ae48582(%struct.ScmObj* %env$ae48582,%struct.ScmObj* %current_45args54169) {
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim54770, align 8
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim54771, align 8
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim54772, align 8
%stackaddr$makeclosure54773 = alloca %struct.ScmObj*, align 8
%fptrToInt54774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48584 to i64
%ae48584 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54774)
store volatile %struct.ScmObj* %ae48584, %struct.ScmObj** %stackaddr$makeclosure54773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48584, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48585 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54775 = alloca %struct.ScmObj*, align 8
%fptrToInt54776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48586 to i64
%ae48586 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54776)
store volatile %struct.ScmObj* %ae48586, %struct.ScmObj** %stackaddr$makeclosure54775, align 8
%argslist54712$ae485840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%argslist54712$ae485841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48586, %struct.ScmObj* %argslist54712$ae485840)
store volatile %struct.ScmObj* %argslist54712$ae485841, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%argslist54712$ae485842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48585, %struct.ScmObj* %argslist54712$ae485841)
store volatile %struct.ScmObj* %argslist54712$ae485842, %struct.ScmObj** %stackaddr$prim54778, align 8
%clofunc54779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48584)
musttail call tailcc void %clofunc54779(%struct.ScmObj* %ae48584, %struct.ScmObj* %argslist54712$ae485842)
ret void
}

define tailcc void @proc_clo$ae48584(%struct.ScmObj* %env$ae48584,%struct.ScmObj* %current_45args54172) {
%stackaddr$env-ref54780 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48584, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54780
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim54781, align 8
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%current_45args54173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %current_45args54173, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$makeclosure54784 = alloca %struct.ScmObj*, align 8
%fptrToInt54785 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48662 to i64
%ae48662 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54785)
store volatile %struct.ScmObj* %ae48662, %struct.ScmObj** %stackaddr$makeclosure54784, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist54696$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54786 = alloca %struct.ScmObj*, align 8
%argslist54696$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist54696$Ycmb480250)
store volatile %struct.ScmObj* %argslist54696$Ycmb480251, %struct.ScmObj** %stackaddr$prim54786, align 8
%stackaddr$prim54787 = alloca %struct.ScmObj*, align 8
%argslist54696$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist54696$Ycmb480251)
store volatile %struct.ScmObj* %argslist54696$Ycmb480252, %struct.ScmObj** %stackaddr$prim54787, align 8
%clofunc54788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54788(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54696$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48662(%struct.ScmObj* %env$ae48662,%struct.ScmObj* %current_45args54175) {
%stackaddr$env-ref54789 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54789
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%current_45args54176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %current_45args54176, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$makeclosure54793 = alloca %struct.ScmObj*, align 8
%fptrToInt54794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48664 to i64
%ae48664 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54794)
store volatile %struct.ScmObj* %ae48664, %struct.ScmObj** %stackaddr$makeclosure54793, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37foldr148046, i64 1)
%ae48665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54795 = alloca %struct.ScmObj*, align 8
%fptrToInt54796 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48666 to i64
%ae48666 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54796)
store volatile %struct.ScmObj* %ae48666, %struct.ScmObj** %stackaddr$makeclosure54795, align 8
%argslist54695$ae486640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%argslist54695$ae486641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48666, %struct.ScmObj* %argslist54695$ae486640)
store volatile %struct.ScmObj* %argslist54695$ae486641, %struct.ScmObj** %stackaddr$prim54797, align 8
%stackaddr$prim54798 = alloca %struct.ScmObj*, align 8
%argslist54695$ae486642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48665, %struct.ScmObj* %argslist54695$ae486641)
store volatile %struct.ScmObj* %argslist54695$ae486642, %struct.ScmObj** %stackaddr$prim54798, align 8
%clofunc54799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48664)
musttail call tailcc void %clofunc54799(%struct.ScmObj* %ae48664, %struct.ScmObj* %argslist54695$ae486642)
ret void
}

define tailcc void @proc_clo$ae48664(%struct.ScmObj* %env$ae48664,%struct.ScmObj* %current_45args54178) {
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$env-ref54801 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54801
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%current_45args54179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %current_45args54179, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$makeclosure54805 = alloca %struct.ScmObj*, align 8
%fptrToInt54806 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48759 to i64
%ae48759 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54806)
store volatile %struct.ScmObj* %ae48759, %struct.ScmObj** %stackaddr$makeclosure54805, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48759, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48759, %struct.ScmObj* %_37foldr148046, i64 1)
%argslist54676$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%argslist54676$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %argslist54676$Ycmb480250)
store volatile %struct.ScmObj* %argslist54676$Ycmb480251, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%argslist54676$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48759, %struct.ScmObj* %argslist54676$Ycmb480251)
store volatile %struct.ScmObj* %argslist54676$Ycmb480252, %struct.ScmObj** %stackaddr$prim54808, align 8
%clofunc54809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54809(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54676$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48759(%struct.ScmObj* %env$ae48759,%struct.ScmObj* %current_45args54181) {
%stackaddr$env-ref54810 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48759, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54810
%stackaddr$env-ref54811 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48759, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54811
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%current_45args54182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %current_45args54182, %struct.ScmObj** %stackaddr$prim54813, align 8
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$makeclosure54815 = alloca %struct.ScmObj*, align 8
%fptrToInt54816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48761 to i64
%ae48761 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54816)
store volatile %struct.ScmObj* %ae48761, %struct.ScmObj** %stackaddr$makeclosure54815, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %_37map148042, i64 2)
%ae48762 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54817 = alloca %struct.ScmObj*, align 8
%fptrToInt54818 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48763 to i64
%ae48763 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54818)
store volatile %struct.ScmObj* %ae48763, %struct.ScmObj** %stackaddr$makeclosure54817, align 8
%argslist54675$ae487610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%argslist54675$ae487611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48763, %struct.ScmObj* %argslist54675$ae487610)
store volatile %struct.ScmObj* %argslist54675$ae487611, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%argslist54675$ae487612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48762, %struct.ScmObj* %argslist54675$ae487611)
store volatile %struct.ScmObj* %argslist54675$ae487612, %struct.ScmObj** %stackaddr$prim54820, align 8
%clofunc54821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48761)
musttail call tailcc void %clofunc54821(%struct.ScmObj* %ae48761, %struct.ScmObj* %argslist54675$ae487612)
ret void
}

define tailcc void @proc_clo$ae48761(%struct.ScmObj* %env$ae48761,%struct.ScmObj* %current_45args54184) {
%stackaddr$env-ref54822 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54822
%stackaddr$env-ref54823 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54823
%stackaddr$env-ref54824 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54824
%stackaddr$prim54825 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54825, align 8
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%current_45args54185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %current_45args54185, %struct.ScmObj** %stackaddr$prim54826, align 8
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim54827, align 8
%stackaddr$makeclosure54828 = alloca %struct.ScmObj*, align 8
%fptrToInt54829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48909 to i64
%ae48909 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54829)
store volatile %struct.ScmObj* %ae48909, %struct.ScmObj** %stackaddr$makeclosure54828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48909, %struct.ScmObj* %_37map148042, i64 2)
%argslist54659$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%argslist54659$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist54659$Ycmb480250)
store volatile %struct.ScmObj* %argslist54659$Ycmb480251, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%argslist54659$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48909, %struct.ScmObj* %argslist54659$Ycmb480251)
store volatile %struct.ScmObj* %argslist54659$Ycmb480252, %struct.ScmObj** %stackaddr$prim54831, align 8
%clofunc54832 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54832(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54659$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48909(%struct.ScmObj* %env$ae48909,%struct.ScmObj* %current_45args54187) {
%stackaddr$env-ref54833 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54833
%stackaddr$env-ref54834 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54834
%stackaddr$env-ref54835 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48909, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54835
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%current_45args54188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %current_45args54188, %struct.ScmObj** %stackaddr$prim54837, align 8
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$makeclosure54839 = alloca %struct.ScmObj*, align 8
%fptrToInt54840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48911 to i64
%ae48911 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54840)
store volatile %struct.ScmObj* %ae48911, %struct.ScmObj** %stackaddr$makeclosure54839, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48911, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48911, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48911, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48911, %struct.ScmObj* %_37map148042, i64 3)
%ae48912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54841 = alloca %struct.ScmObj*, align 8
%fptrToInt54842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48913 to i64
%ae48913 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54842)
store volatile %struct.ScmObj* %ae48913, %struct.ScmObj** %stackaddr$makeclosure54841, align 8
%argslist54658$ae489110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%argslist54658$ae489111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48913, %struct.ScmObj* %argslist54658$ae489110)
store volatile %struct.ScmObj* %argslist54658$ae489111, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%argslist54658$ae489112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist54658$ae489111)
store volatile %struct.ScmObj* %argslist54658$ae489112, %struct.ScmObj** %stackaddr$prim54844, align 8
%clofunc54845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48911)
musttail call tailcc void %clofunc54845(%struct.ScmObj* %ae48911, %struct.ScmObj* %argslist54658$ae489112)
ret void
}

define tailcc void @proc_clo$ae48911(%struct.ScmObj* %env$ae48911,%struct.ScmObj* %current_45args54190) {
%stackaddr$env-ref54846 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48911, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54846
%stackaddr$env-ref54847 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48911, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54847
%stackaddr$env-ref54848 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48911, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54848
%stackaddr$env-ref54849 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48911, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54849
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54850, align 8
%stackaddr$prim54851 = alloca %struct.ScmObj*, align 8
%current_45args54191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %current_45args54191, %struct.ScmObj** %stackaddr$prim54851, align 8
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim54852, align 8
%stackaddr$makeclosure54853 = alloca %struct.ScmObj*, align 8
%fptrToInt54854 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48992 to i64
%ae48992 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54854)
store volatile %struct.ScmObj* %ae48992, %struct.ScmObj** %stackaddr$makeclosure54853, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48992, %struct.ScmObj* %_37map148042, i64 3)
%argslist54644$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%argslist54644$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist54644$Ycmb480250)
store volatile %struct.ScmObj* %argslist54644$Ycmb480251, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%argslist54644$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48992, %struct.ScmObj* %argslist54644$Ycmb480251)
store volatile %struct.ScmObj* %argslist54644$Ycmb480252, %struct.ScmObj** %stackaddr$prim54856, align 8
%clofunc54857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54857(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54644$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48992(%struct.ScmObj* %env$ae48992,%struct.ScmObj* %current_45args54193) {
%stackaddr$env-ref54858 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54858
%stackaddr$env-ref54859 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54859
%stackaddr$env-ref54860 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54860
%stackaddr$env-ref54861 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48992, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54861
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%current_45args54194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %current_45args54194, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim54864, align 8
%stackaddr$makeclosure54865 = alloca %struct.ScmObj*, align 8
%fptrToInt54866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48994 to i64
%ae48994 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54866)
store volatile %struct.ScmObj* %ae48994, %struct.ScmObj** %stackaddr$makeclosure54865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48994, %struct.ScmObj* %_37map148042, i64 4)
%ae48995 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54867 = alloca %struct.ScmObj*, align 8
%fptrToInt54868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48996 to i64
%ae48996 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54868)
store volatile %struct.ScmObj* %ae48996, %struct.ScmObj** %stackaddr$makeclosure54867, align 8
%argslist54643$ae489940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%argslist54643$ae489941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48996, %struct.ScmObj* %argslist54643$ae489940)
store volatile %struct.ScmObj* %argslist54643$ae489941, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%argslist54643$ae489942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48995, %struct.ScmObj* %argslist54643$ae489941)
store volatile %struct.ScmObj* %argslist54643$ae489942, %struct.ScmObj** %stackaddr$prim54870, align 8
%clofunc54871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48994)
musttail call tailcc void %clofunc54871(%struct.ScmObj* %ae48994, %struct.ScmObj* %argslist54643$ae489942)
ret void
}

define tailcc void @proc_clo$ae48994(%struct.ScmObj* %env$ae48994,%struct.ScmObj* %current_45args54196) {
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$env-ref54873 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54873
%stackaddr$env-ref54874 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54874
%stackaddr$env-ref54875 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54875
%stackaddr$env-ref54876 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48994, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54876
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%current_45args54197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %current_45args54197, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54197)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$makeclosure54880 = alloca %struct.ScmObj*, align 8
%fptrToInt54881 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49071 to i64
%ae49071 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54881)
store volatile %struct.ScmObj* %ae49071, %struct.ScmObj** %stackaddr$makeclosure54880, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49071, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49071, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49071, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49071, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49071, %struct.ScmObj* %_37map148042, i64 4)
%argslist54627$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%argslist54627$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist54627$Ycmb480250)
store volatile %struct.ScmObj* %argslist54627$Ycmb480251, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%argslist54627$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49071, %struct.ScmObj* %argslist54627$Ycmb480251)
store volatile %struct.ScmObj* %argslist54627$Ycmb480252, %struct.ScmObj** %stackaddr$prim54883, align 8
%clofunc54884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54884(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54627$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49071(%struct.ScmObj* %env$ae49071,%struct.ScmObj* %current_45args54199) {
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49071, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$env-ref54886 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49071, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54886
%stackaddr$env-ref54887 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49071, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54887
%stackaddr$env-ref54888 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49071, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54888
%stackaddr$env-ref54889 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49071, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54889
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%current_45args54200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %current_45args54200, %struct.ScmObj** %stackaddr$prim54891, align 8
%stackaddr$prim54892 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim54892, align 8
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49073 to i64
%ae49073 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae49073, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %_37take48038, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %_37length48035, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49073, %struct.ScmObj* %_37map148042, i64 5)
%ae49074 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54895 = alloca %struct.ScmObj*, align 8
%fptrToInt54896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49075 to i64
%ae49075 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54896)
store volatile %struct.ScmObj* %ae49075, %struct.ScmObj** %stackaddr$makeclosure54895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49075, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54626$ae490730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54897 = alloca %struct.ScmObj*, align 8
%argslist54626$ae490731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49075, %struct.ScmObj* %argslist54626$ae490730)
store volatile %struct.ScmObj* %argslist54626$ae490731, %struct.ScmObj** %stackaddr$prim54897, align 8
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%argslist54626$ae490732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49074, %struct.ScmObj* %argslist54626$ae490731)
store volatile %struct.ScmObj* %argslist54626$ae490732, %struct.ScmObj** %stackaddr$prim54898, align 8
%clofunc54899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49073)
musttail call tailcc void %clofunc54899(%struct.ScmObj* %ae49073, %struct.ScmObj* %argslist54626$ae490732)
ret void
}

define tailcc void @proc_clo$ae49073(%struct.ScmObj* %env$ae49073,%struct.ScmObj* %current_45args54202) {
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 3)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 4)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$env-ref54905 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49073, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54905
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%current_45args54203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %current_45args54203, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49127 to i64
%ae49127 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae49127, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37map148042, i64 4)
%ae49128 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54911 = alloca %struct.ScmObj*, align 8
%fptrToInt54912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49129 to i64
%ae49129 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54912)
store volatile %struct.ScmObj* %ae49129, %struct.ScmObj** %stackaddr$makeclosure54911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49129, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49129, %struct.ScmObj* %_37length48035, i64 1)
%argslist54612$ae491270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%argslist54612$ae491271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49129, %struct.ScmObj* %argslist54612$ae491270)
store volatile %struct.ScmObj* %argslist54612$ae491271, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%argslist54612$ae491272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49128, %struct.ScmObj* %argslist54612$ae491271)
store volatile %struct.ScmObj* %argslist54612$ae491272, %struct.ScmObj** %stackaddr$prim54914, align 8
%clofunc54915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49127)
musttail call tailcc void %clofunc54915(%struct.ScmObj* %ae49127, %struct.ScmObj* %argslist54612$ae491272)
ret void
}

define tailcc void @proc_clo$ae49127(%struct.ScmObj* %env$ae49127,%struct.ScmObj* %current_45args54205) {
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%current_45args54206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %current_45args54206, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54206)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$makeclosure54924 = alloca %struct.ScmObj*, align 8
%fptrToInt54925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49157 to i64
%ae49157 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54925)
store volatile %struct.ScmObj* %ae49157, %struct.ScmObj** %stackaddr$makeclosure54924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49157, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49157, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49157, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49157, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49157, %struct.ScmObj* %_37drop_45right48065, i64 4)
%ae49158 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54926 = alloca %struct.ScmObj*, align 8
%fptrToInt54927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49159 to i64
%ae49159 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54927)
store volatile %struct.ScmObj* %ae49159, %struct.ScmObj** %stackaddr$makeclosure54926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49159, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49159, %struct.ScmObj* %_37map148042, i64 1)
%argslist54602$ae491570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist54602$ae491571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49159, %struct.ScmObj* %argslist54602$ae491570)
store volatile %struct.ScmObj* %argslist54602$ae491571, %struct.ScmObj** %stackaddr$prim54928, align 8
%stackaddr$prim54929 = alloca %struct.ScmObj*, align 8
%argslist54602$ae491572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49158, %struct.ScmObj* %argslist54602$ae491571)
store volatile %struct.ScmObj* %argslist54602$ae491572, %struct.ScmObj** %stackaddr$prim54929, align 8
%clofunc54930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49157)
musttail call tailcc void %clofunc54930(%struct.ScmObj* %ae49157, %struct.ScmObj* %argslist54602$ae491572)
ret void
}

define tailcc void @proc_clo$ae49157(%struct.ScmObj* %env$ae49157,%struct.ScmObj* %current_45args54208) {
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49157, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$env-ref54932 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49157, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54932
%stackaddr$env-ref54933 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49157, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54933
%stackaddr$env-ref54934 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49157, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54934
%stackaddr$env-ref54935 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49157, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54935
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim54936, align 8
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%current_45args54209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %current_45args54209, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54209)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$makeclosure54939 = alloca %struct.ScmObj*, align 8
%fptrToInt54940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49541 to i64
%ae49541 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54940)
store volatile %struct.ScmObj* %ae49541, %struct.ScmObj** %stackaddr$makeclosure54939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49541, %struct.ScmObj* %_37drop_45right48065, i64 4)
%argslist54542$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%argslist54542$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist54542$Ycmb480250)
store volatile %struct.ScmObj* %argslist54542$Ycmb480251, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%argslist54542$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49541, %struct.ScmObj* %argslist54542$Ycmb480251)
store volatile %struct.ScmObj* %argslist54542$Ycmb480252, %struct.ScmObj** %stackaddr$prim54942, align 8
%clofunc54943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54943(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54542$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49541(%struct.ScmObj* %env$ae49541,%struct.ScmObj* %current_45args54211) {
%stackaddr$env-ref54944 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54944
%stackaddr$env-ref54945 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54945
%stackaddr$env-ref54946 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54946
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49541, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%current_45args54212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %current_45args54212, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54212)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$makeclosure54952 = alloca %struct.ScmObj*, align 8
%fptrToInt54953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49543 to i64
%ae49543 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54953)
store volatile %struct.ScmObj* %ae49543, %struct.ScmObj** %stackaddr$makeclosure54952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49543, %struct.ScmObj* %_37drop_45right48065, i64 5)
%ae49544 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54954 = alloca %struct.ScmObj*, align 8
%fptrToInt54955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49545 to i64
%ae49545 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54955)
store volatile %struct.ScmObj* %ae49545, %struct.ScmObj** %stackaddr$makeclosure54954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49545, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist54541$ae495430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%argslist54541$ae495431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49545, %struct.ScmObj* %argslist54541$ae495430)
store volatile %struct.ScmObj* %argslist54541$ae495431, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%argslist54541$ae495432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49544, %struct.ScmObj* %argslist54541$ae495431)
store volatile %struct.ScmObj* %argslist54541$ae495432, %struct.ScmObj** %stackaddr$prim54957, align 8
%clofunc54958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49543)
musttail call tailcc void %clofunc54958(%struct.ScmObj* %ae49543, %struct.ScmObj* %argslist54541$ae495432)
ret void
}

define tailcc void @proc_clo$ae49543(%struct.ScmObj* %env$ae49543,%struct.ScmObj* %current_45args54214) {
%stackaddr$env-ref54959 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54959
%stackaddr$env-ref54960 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54960
%stackaddr$env-ref54961 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54961
%stackaddr$env-ref54962 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54962
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$env-ref54964 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49543, i64 5)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54964
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%current_45args54215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %current_45args54215, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim54967, align 8
%stackaddr$makeclosure54968 = alloca %struct.ScmObj*, align 8
%fptrToInt54969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49620 to i64
%ae49620 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54969)
store volatile %struct.ScmObj* %ae49620, %struct.ScmObj** %stackaddr$makeclosure54968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49620, %struct.ScmObj* %_37map148077, i64 4)
%ae49621 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54970 = alloca %struct.ScmObj*, align 8
%fptrToInt54971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49622 to i64
%ae49622 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54971)
store volatile %struct.ScmObj* %ae49622, %struct.ScmObj** %stackaddr$makeclosure54970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49622, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49622, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49622, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist54522$ae496200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%argslist54522$ae496201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49622, %struct.ScmObj* %argslist54522$ae496200)
store volatile %struct.ScmObj* %argslist54522$ae496201, %struct.ScmObj** %stackaddr$prim54972, align 8
%stackaddr$prim54973 = alloca %struct.ScmObj*, align 8
%argslist54522$ae496202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49621, %struct.ScmObj* %argslist54522$ae496201)
store volatile %struct.ScmObj* %argslist54522$ae496202, %struct.ScmObj** %stackaddr$prim54973, align 8
%clofunc54974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49620)
musttail call tailcc void %clofunc54974(%struct.ScmObj* %ae49620, %struct.ScmObj* %argslist54522$ae496202)
ret void
}

define tailcc void @proc_clo$ae49620(%struct.ScmObj* %env$ae49620,%struct.ScmObj* %current_45args54217) {
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$env-ref54976 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54976
%stackaddr$env-ref54977 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54977
%stackaddr$env-ref54978 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54978
%stackaddr$env-ref54979 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49620, i64 4)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref54979
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim54981, align 8
%stackaddr$prim54982 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim54982, align 8
%stackaddr$makeclosure54983 = alloca %struct.ScmObj*, align 8
%fptrToInt54984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49766 to i64
%ae49766 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54984)
store volatile %struct.ScmObj* %ae49766, %struct.ScmObj** %stackaddr$makeclosure54983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49766, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49766, %struct.ScmObj* %_37foldl148030, i64 1)
%ae49767 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54985 = alloca %struct.ScmObj*, align 8
%fptrToInt54986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49768 to i64
%ae49768 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54986)
store volatile %struct.ScmObj* %ae49768, %struct.ScmObj** %stackaddr$makeclosure54985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49768, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49768, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49768, %struct.ScmObj* %_37map148077, i64 2)
%argslist54505$ae497660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%argslist54505$ae497661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49768, %struct.ScmObj* %argslist54505$ae497660)
store volatile %struct.ScmObj* %argslist54505$ae497661, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%argslist54505$ae497662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49767, %struct.ScmObj* %argslist54505$ae497661)
store volatile %struct.ScmObj* %argslist54505$ae497662, %struct.ScmObj** %stackaddr$prim54988, align 8
%clofunc54989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49766)
musttail call tailcc void %clofunc54989(%struct.ScmObj* %ae49766, %struct.ScmObj* %argslist54505$ae497662)
ret void
}

define tailcc void @proc_clo$ae49766(%struct.ScmObj* %env$ae49766,%struct.ScmObj* %current_45args54220) {
%stackaddr$env-ref54990 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49766, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54990
%stackaddr$env-ref54991 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49766, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54991
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim54992, align 8
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%current_45args54221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %current_45args54221, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim54994, align 8
%stackaddr$makeclosure54995 = alloca %struct.ScmObj*, align 8
%fptrToInt54996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50158 to i64
%ae50158 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54996)
store volatile %struct.ScmObj* %ae50158, %struct.ScmObj** %stackaddr$makeclosure54995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50158, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54445$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%argslist54445$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %argslist54445$Ycmb480250)
store volatile %struct.ScmObj* %argslist54445$Ycmb480251, %struct.ScmObj** %stackaddr$prim54997, align 8
%stackaddr$prim54998 = alloca %struct.ScmObj*, align 8
%argslist54445$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50158, %struct.ScmObj* %argslist54445$Ycmb480251)
store volatile %struct.ScmObj* %argslist54445$Ycmb480252, %struct.ScmObj** %stackaddr$prim54998, align 8
%clofunc54999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54999(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54445$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50158(%struct.ScmObj* %env$ae50158,%struct.ScmObj* %current_45args54223) {
%stackaddr$env-ref55000 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50158, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55000
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54223)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim55001, align 8
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%current_45args54224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54223)
store volatile %struct.ScmObj* %current_45args54224, %struct.ScmObj** %stackaddr$prim55002, align 8
%stackaddr$prim55003 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim55003, align 8
%stackaddr$makeclosure55004 = alloca %struct.ScmObj*, align 8
%fptrToInt55005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50160 to i64
%ae50160 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55005)
store volatile %struct.ScmObj* %ae50160, %struct.ScmObj** %stackaddr$makeclosure55004, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50160, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50161 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55006 = alloca %struct.ScmObj*, align 8
%fptrToInt55007 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50162 to i64
%ae50162 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55007)
store volatile %struct.ScmObj* %ae50162, %struct.ScmObj** %stackaddr$makeclosure55006, align 8
%argslist54444$ae501600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%argslist54444$ae501601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50162, %struct.ScmObj* %argslist54444$ae501600)
store volatile %struct.ScmObj* %argslist54444$ae501601, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%argslist54444$ae501602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50161, %struct.ScmObj* %argslist54444$ae501601)
store volatile %struct.ScmObj* %argslist54444$ae501602, %struct.ScmObj** %stackaddr$prim55009, align 8
%clofunc55010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50160)
musttail call tailcc void %clofunc55010(%struct.ScmObj* %ae50160, %struct.ScmObj* %argslist54444$ae501602)
ret void
}

define tailcc void @proc_clo$ae50160(%struct.ScmObj* %env$ae50160,%struct.ScmObj* %current_45args54226) {
%stackaddr$env-ref55011 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50160, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55011
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%current_45args54227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %current_45args54227, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54227)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$makeclosure55015 = alloca %struct.ScmObj*, align 8
%fptrToInt55016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50184 to i64
%ae50184 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55016)
store volatile %struct.ScmObj* %ae50184, %struct.ScmObj** %stackaddr$makeclosure55015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50184, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50185 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55017 = alloca %struct.ScmObj*, align 8
%fptrToInt55018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50186 to i64
%ae50186 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55018)
store volatile %struct.ScmObj* %ae50186, %struct.ScmObj** %stackaddr$makeclosure55017, align 8
%argslist54438$ae501840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%argslist54438$ae501841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50186, %struct.ScmObj* %argslist54438$ae501840)
store volatile %struct.ScmObj* %argslist54438$ae501841, %struct.ScmObj** %stackaddr$prim55019, align 8
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%argslist54438$ae501842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50185, %struct.ScmObj* %argslist54438$ae501841)
store volatile %struct.ScmObj* %argslist54438$ae501842, %struct.ScmObj** %stackaddr$prim55020, align 8
%clofunc55021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50184)
musttail call tailcc void %clofunc55021(%struct.ScmObj* %ae50184, %struct.ScmObj* %argslist54438$ae501842)
ret void
}

define tailcc void @proc_clo$ae50184(%struct.ScmObj* %env$ae50184,%struct.ScmObj* %current_45args54229) {
%stackaddr$env-ref55022 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50184, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55022
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%_95k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %_95k48293, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%current_45args54230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %current_45args54230, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim55025, align 8
%ae50208 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50209 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50208, %struct.ScmObj* %ae50209)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim55026, align 8
%stackaddr$makeclosure55027 = alloca %struct.ScmObj*, align 8
%fptrToInt55028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50210 to i64
%ae50210 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55028)
store volatile %struct.ScmObj* %ae50210, %struct.ScmObj** %stackaddr$makeclosure55027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50210, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50210, %struct.ScmObj* %_37foldl148030, i64 1)
%ae50211 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55029 = alloca %struct.ScmObj*, align 8
%fptrToInt55030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50212 to i64
%ae50212 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55030)
store volatile %struct.ScmObj* %ae50212, %struct.ScmObj** %stackaddr$makeclosure55029, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50212, %struct.ScmObj* %_37append48118, i64 0)
%argslist54432$ae502100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%argslist54432$ae502101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50212, %struct.ScmObj* %argslist54432$ae502100)
store volatile %struct.ScmObj* %argslist54432$ae502101, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%argslist54432$ae502102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50211, %struct.ScmObj* %argslist54432$ae502101)
store volatile %struct.ScmObj* %argslist54432$ae502102, %struct.ScmObj** %stackaddr$prim55032, align 8
%clofunc55033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50210)
musttail call tailcc void %clofunc55033(%struct.ScmObj* %ae50210, %struct.ScmObj* %argslist54432$ae502102)
ret void
}

define tailcc void @proc_clo$ae50210(%struct.ScmObj* %env$ae50210,%struct.ScmObj* %current_45args54232) {
%stackaddr$env-ref55034 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50210, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55034
%stackaddr$env-ref55035 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50210, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55035
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim55036, align 8
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%current_45args54233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %current_45args54233, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54233)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55038, align 8
%ae50278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55039 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50278, %struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim55039, align 8
%ae50281 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50281)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$makeclosure55041 = alloca %struct.ScmObj*, align 8
%fptrToInt55042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50282 to i64
%ae50282 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55042)
store volatile %struct.ScmObj* %ae50282, %struct.ScmObj** %stackaddr$makeclosure55041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50282, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50283 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55043 = alloca %struct.ScmObj*, align 8
%fptrToInt55044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50284 to i64
%ae50284 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55044)
store volatile %struct.ScmObj* %ae50284, %struct.ScmObj** %stackaddr$makeclosure55043, align 8
%argslist54421$ae502820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%argslist54421$ae502821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50284, %struct.ScmObj* %argslist54421$ae502820)
store volatile %struct.ScmObj* %argslist54421$ae502821, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist54421$ae502822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50283, %struct.ScmObj* %argslist54421$ae502821)
store volatile %struct.ScmObj* %argslist54421$ae502822, %struct.ScmObj** %stackaddr$prim55046, align 8
%clofunc55047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50282)
musttail call tailcc void %clofunc55047(%struct.ScmObj* %ae50282, %struct.ScmObj* %argslist54421$ae502822)
ret void
}

define tailcc void @proc_clo$ae50282(%struct.ScmObj* %env$ae50282,%struct.ScmObj* %current_45args54235) {
%stackaddr$env-ref55048 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50282, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55048
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim55049, align 8
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%current_45args54236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %current_45args54236, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim55051, align 8
%stackaddr$makeclosure55052 = alloca %struct.ScmObj*, align 8
%fptrToInt55053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50698 to i64
%ae50698 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55053)
store volatile %struct.ScmObj* %ae50698, %struct.ScmObj** %stackaddr$makeclosure55052, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50698, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55054 = alloca %struct.ScmObj*, align 8
%fptrToInt55055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50700 to i64
%ae50700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55055)
store volatile %struct.ScmObj* %ae50700, %struct.ScmObj** %stackaddr$makeclosure55054, align 8
%argslist54396$ae506980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%argslist54396$ae506981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50700, %struct.ScmObj* %argslist54396$ae506980)
store volatile %struct.ScmObj* %argslist54396$ae506981, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%argslist54396$ae506982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50699, %struct.ScmObj* %argslist54396$ae506981)
store volatile %struct.ScmObj* %argslist54396$ae506982, %struct.ScmObj** %stackaddr$prim55057, align 8
%clofunc55058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50698)
musttail call tailcc void %clofunc55058(%struct.ScmObj* %ae50698, %struct.ScmObj* %argslist54396$ae506982)
ret void
}

define tailcc void @proc_clo$ae50698(%struct.ScmObj* %env$ae50698,%struct.ScmObj* %current_45args54238) {
%stackaddr$env-ref55059 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50698, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55059
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%current_45args54239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %current_45args54239, %struct.ScmObj** %stackaddr$prim55061, align 8
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54239)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55062, align 8
%stackaddr$makeclosure55063 = alloca %struct.ScmObj*, align 8
%fptrToInt55064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51234 to i64
%ae51234 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55064)
store volatile %struct.ScmObj* %ae51234, %struct.ScmObj** %stackaddr$makeclosure55063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51234, %struct.ScmObj* %_37foldl148030, i64 0)
%ae51235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55065 = alloca %struct.ScmObj*, align 8
%fptrToInt55066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51236 to i64
%ae51236 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55066)
store volatile %struct.ScmObj* %ae51236, %struct.ScmObj** %stackaddr$makeclosure55065, align 8
%argslist54372$ae512340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%argslist54372$ae512341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51236, %struct.ScmObj* %argslist54372$ae512340)
store volatile %struct.ScmObj* %argslist54372$ae512341, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%argslist54372$ae512342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51235, %struct.ScmObj* %argslist54372$ae512341)
store volatile %struct.ScmObj* %argslist54372$ae512342, %struct.ScmObj** %stackaddr$prim55068, align 8
%clofunc55069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51234)
musttail call tailcc void %clofunc55069(%struct.ScmObj* %ae51234, %struct.ScmObj* %argslist54372$ae512342)
ret void
}

define tailcc void @proc_clo$ae51234(%struct.ScmObj* %env$ae51234,%struct.ScmObj* %current_45args54241) {
%stackaddr$env-ref55070 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51234, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55070
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%current_45args54242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %current_45args54242, %struct.ScmObj** %stackaddr$prim55072, align 8
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54242)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55073, align 8
%stackaddr$makeclosure55074 = alloca %struct.ScmObj*, align 8
%fptrToInt55075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51638 to i64
%ae51638 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55075)
store volatile %struct.ScmObj* %ae51638, %struct.ScmObj** %stackaddr$makeclosure55074, align 8
%ae51639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55076 = alloca %struct.ScmObj*, align 8
%fptrToInt55077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51640 to i64
%ae51640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55077)
store volatile %struct.ScmObj* %ae51640, %struct.ScmObj** %stackaddr$makeclosure55076, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51640, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54346$ae516380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%argslist54346$ae516381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51640, %struct.ScmObj* %argslist54346$ae516380)
store volatile %struct.ScmObj* %argslist54346$ae516381, %struct.ScmObj** %stackaddr$prim55078, align 8
%stackaddr$prim55079 = alloca %struct.ScmObj*, align 8
%argslist54346$ae516382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51639, %struct.ScmObj* %argslist54346$ae516381)
store volatile %struct.ScmObj* %argslist54346$ae516382, %struct.ScmObj** %stackaddr$prim55079, align 8
%clofunc55080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51638)
musttail call tailcc void %clofunc55080(%struct.ScmObj* %ae51638, %struct.ScmObj* %argslist54346$ae516382)
ret void
}

define tailcc void @proc_clo$ae51638(%struct.ScmObj* %env$ae51638,%struct.ScmObj* %current_45args54244) {
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54244)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%current_45args54245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54244)
store volatile %struct.ScmObj* %current_45args54245, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54245)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$makeclosure55084 = alloca %struct.ScmObj*, align 8
%fptrToInt55085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51736 to i64
%ae51736 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55085)
store volatile %struct.ScmObj* %ae51736, %struct.ScmObj** %stackaddr$makeclosure55084, align 8
%ae51737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55086 = alloca %struct.ScmObj*, align 8
%fptrToInt55087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51738 to i64
%ae51738 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55087)
store volatile %struct.ScmObj* %ae51738, %struct.ScmObj** %stackaddr$makeclosure55086, align 8
%argslist54333$ae517360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist54333$ae517361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51738, %struct.ScmObj* %argslist54333$ae517360)
store volatile %struct.ScmObj* %argslist54333$ae517361, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%argslist54333$ae517362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51737, %struct.ScmObj* %argslist54333$ae517361)
store volatile %struct.ScmObj* %argslist54333$ae517362, %struct.ScmObj** %stackaddr$prim55089, align 8
%clofunc55090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51736)
musttail call tailcc void %clofunc55090(%struct.ScmObj* %ae51736, %struct.ScmObj* %argslist54333$ae517362)
ret void
}

define tailcc void @proc_clo$ae51736(%struct.ScmObj* %env$ae51736,%struct.ScmObj* %current_45args54247) {
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%current_45args54248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %current_45args54248, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55093, align 8
%stackaddr$makeclosure55094 = alloca %struct.ScmObj*, align 8
%fptrToInt55095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51756 to i64
%ae51756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55095)
store volatile %struct.ScmObj* %ae51756, %struct.ScmObj** %stackaddr$makeclosure55094, align 8
%ae51757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55096 = alloca %struct.ScmObj*, align 8
%fptrToInt55097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51758 to i64
%ae51758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55097)
store volatile %struct.ScmObj* %ae51758, %struct.ScmObj** %stackaddr$makeclosure55096, align 8
%argslist54328$ae517560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%argslist54328$ae517561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51758, %struct.ScmObj* %argslist54328$ae517560)
store volatile %struct.ScmObj* %argslist54328$ae517561, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%argslist54328$ae517562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51757, %struct.ScmObj* %argslist54328$ae517561)
store volatile %struct.ScmObj* %argslist54328$ae517562, %struct.ScmObj** %stackaddr$prim55099, align 8
%clofunc55100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51756)
musttail call tailcc void %clofunc55100(%struct.ScmObj* %ae51756, %struct.ScmObj* %argslist54328$ae517562)
ret void
}

define tailcc void @proc_clo$ae51756(%struct.ScmObj* %env$ae51756,%struct.ScmObj* %current_45args54250) {
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54250)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%current_45args54251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54250)
store volatile %struct.ScmObj* %current_45args54251, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54251)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55103, align 8
%stackaddr$makeclosure55104 = alloca %struct.ScmObj*, align 8
%fptrToInt55105 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51778 to i64
%ae51778 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55105)
store volatile %struct.ScmObj* %ae51778, %struct.ScmObj** %stackaddr$makeclosure55104, align 8
%ae51779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55106 = alloca %struct.ScmObj*, align 8
%fptrToInt55107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51780 to i64
%ae51780 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55107)
store volatile %struct.ScmObj* %ae51780, %struct.ScmObj** %stackaddr$makeclosure55106, align 8
%argslist54323$ae517780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%argslist54323$ae517781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51780, %struct.ScmObj* %argslist54323$ae517780)
store volatile %struct.ScmObj* %argslist54323$ae517781, %struct.ScmObj** %stackaddr$prim55108, align 8
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%argslist54323$ae517782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51779, %struct.ScmObj* %argslist54323$ae517781)
store volatile %struct.ScmObj* %argslist54323$ae517782, %struct.ScmObj** %stackaddr$prim55109, align 8
%clofunc55110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51778)
musttail call tailcc void %clofunc55110(%struct.ScmObj* %ae51778, %struct.ScmObj* %argslist54323$ae517782)
ret void
}

define tailcc void @proc_clo$ae51778(%struct.ScmObj* %env$ae51778,%struct.ScmObj* %current_45args54253) {
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54253)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim55111, align 8
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%current_45args54254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54253)
store volatile %struct.ScmObj* %current_45args54254, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$makeclosure55114 = alloca %struct.ScmObj*, align 8
%fptrToInt55115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51802 to i64
%ae51802 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55115)
store volatile %struct.ScmObj* %ae51802, %struct.ScmObj** %stackaddr$makeclosure55114, align 8
%ae51803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55116 = alloca %struct.ScmObj*, align 8
%fptrToInt55117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51804 to i64
%ae51804 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55117)
store volatile %struct.ScmObj* %ae51804, %struct.ScmObj** %stackaddr$makeclosure55116, align 8
%argslist54318$ae518020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%argslist54318$ae518021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51804, %struct.ScmObj* %argslist54318$ae518020)
store volatile %struct.ScmObj* %argslist54318$ae518021, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%argslist54318$ae518022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51803, %struct.ScmObj* %argslist54318$ae518021)
store volatile %struct.ScmObj* %argslist54318$ae518022, %struct.ScmObj** %stackaddr$prim55119, align 8
%clofunc55120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51802)
musttail call tailcc void %clofunc55120(%struct.ScmObj* %ae51802, %struct.ScmObj* %argslist54318$ae518022)
ret void
}

define tailcc void @proc_clo$ae51802(%struct.ScmObj* %env$ae51802,%struct.ScmObj* %current_45args54256) {
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%current_45args54257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %current_45args54257, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54257)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55123, align 8
%stackaddr$makeclosure55124 = alloca %struct.ScmObj*, align 8
%fptrToInt55125 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51828 to i64
%ae51828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55125)
store volatile %struct.ScmObj* %ae51828, %struct.ScmObj** %stackaddr$makeclosure55124, align 8
%ae51829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55126 = alloca %struct.ScmObj*, align 8
%fptrToInt55127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51830 to i64
%ae51830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55127)
store volatile %struct.ScmObj* %ae51830, %struct.ScmObj** %stackaddr$makeclosure55126, align 8
%argslist54313$ae518280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%argslist54313$ae518281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51830, %struct.ScmObj* %argslist54313$ae518280)
store volatile %struct.ScmObj* %argslist54313$ae518281, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%argslist54313$ae518282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51829, %struct.ScmObj* %argslist54313$ae518281)
store volatile %struct.ScmObj* %argslist54313$ae518282, %struct.ScmObj** %stackaddr$prim55129, align 8
%clofunc55130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51828)
musttail call tailcc void %clofunc55130(%struct.ScmObj* %ae51828, %struct.ScmObj* %argslist54313$ae518282)
ret void
}

define tailcc void @proc_clo$ae51828(%struct.ScmObj* %env$ae51828,%struct.ScmObj* %current_45args54259) {
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%current_45args54260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %current_45args54260, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$makeclosure55134 = alloca %struct.ScmObj*, align 8
%fptrToInt55135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51850 to i64
%ae51850 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55135)
store volatile %struct.ScmObj* %ae51850, %struct.ScmObj** %stackaddr$makeclosure55134, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51850, %struct.ScmObj* %anf_45bind48261, i64 0)
%ae51851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55136 = alloca %struct.ScmObj*, align 8
%fptrToInt55137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51852 to i64
%ae51852 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55137)
store volatile %struct.ScmObj* %ae51852, %struct.ScmObj** %stackaddr$makeclosure55136, align 8
%argslist54307$ae518500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%argslist54307$ae518501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51852, %struct.ScmObj* %argslist54307$ae518500)
store volatile %struct.ScmObj* %argslist54307$ae518501, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%argslist54307$ae518502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51851, %struct.ScmObj* %argslist54307$ae518501)
store volatile %struct.ScmObj* %argslist54307$ae518502, %struct.ScmObj** %stackaddr$prim55139, align 8
%clofunc55140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51850)
musttail call tailcc void %clofunc55140(%struct.ScmObj* %ae51850, %struct.ScmObj* %argslist54307$ae518502)
ret void
}

define tailcc void @proc_clo$ae51850(%struct.ScmObj* %env$ae51850,%struct.ScmObj* %current_45args54262) {
%stackaddr$env-ref55141 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51850, i64 0)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55141
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%current_45args54263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %current_45args54263, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$makeclosure55145 = alloca %struct.ScmObj*, align 8
%fptrToInt55146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51873 to i64
%ae51873 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55146)
store volatile %struct.ScmObj* %ae51873, %struct.ScmObj** %stackaddr$makeclosure55145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51873, %struct.ScmObj* %anf_45bind48261, i64 0)
%ae51874 = call %struct.ScmObj* @const_init_int(i64 4)
%ae51875 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist54305$anf_45bind482620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%argslist54305$anf_45bind482621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51875, %struct.ScmObj* %argslist54305$anf_45bind482620)
store volatile %struct.ScmObj* %argslist54305$anf_45bind482621, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%argslist54305$anf_45bind482622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51874, %struct.ScmObj* %argslist54305$anf_45bind482621)
store volatile %struct.ScmObj* %argslist54305$anf_45bind482622, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%argslist54305$anf_45bind482623 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51873, %struct.ScmObj* %argslist54305$anf_45bind482622)
store volatile %struct.ScmObj* %argslist54305$anf_45bind482623, %struct.ScmObj** %stackaddr$prim55149, align 8
%clofunc55150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48262)
musttail call tailcc void %clofunc55150(%struct.ScmObj* %anf_45bind48262, %struct.ScmObj* %argslist54305$anf_45bind482623)
ret void
}

define tailcc void @proc_clo$ae51873(%struct.ScmObj* %env$ae51873,%struct.ScmObj* %current_45args54265) {
%stackaddr$env-ref55151 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51873, i64 0)
store %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$env-ref55151
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54265)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55152, align 8
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%current_45args54266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54265)
store volatile %struct.ScmObj* %current_45args54266, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$makeclosure55155 = alloca %struct.ScmObj*, align 8
%fptrToInt55156 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51884 to i64
%ae51884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55156)
store volatile %struct.ScmObj* %ae51884, %struct.ScmObj** %stackaddr$makeclosure55155, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%cpsargs48319 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51884, %struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %cpsargs48319, %struct.ScmObj** %stackaddr$prim55157, align 8
%clofunc55158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48261)
musttail call tailcc void %clofunc55158(%struct.ScmObj* %anf_45bind48261, %struct.ScmObj* %cpsargs48319)
ret void
}

define tailcc void @proc_clo$ae51884(%struct.ScmObj* %env$ae51884,%struct.ScmObj* %current_45args54268) {
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%current_45args54269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %current_45args54269, %struct.ScmObj** %stackaddr$prim55160, align 8
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54269)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim55161, align 8
%stackaddr$makeclosure55162 = alloca %struct.ScmObj*, align 8
%fptrToInt55163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51888 to i64
%ae51888 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55163)
store volatile %struct.ScmObj* %ae51888, %struct.ScmObj** %stackaddr$makeclosure55162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51888, %struct.ScmObj* %anf_45bind48264, i64 0)
%ae51889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55164 = alloca %struct.ScmObj*, align 8
%fptrToInt55165 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51890 to i64
%ae51890 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55165)
store volatile %struct.ScmObj* %ae51890, %struct.ScmObj** %stackaddr$makeclosure55164, align 8
%argslist54304$ae518880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%argslist54304$ae518881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51890, %struct.ScmObj* %argslist54304$ae518880)
store volatile %struct.ScmObj* %argslist54304$ae518881, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%argslist54304$ae518882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51889, %struct.ScmObj* %argslist54304$ae518881)
store volatile %struct.ScmObj* %argslist54304$ae518882, %struct.ScmObj** %stackaddr$prim55167, align 8
%clofunc55168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51888)
musttail call tailcc void %clofunc55168(%struct.ScmObj* %ae51888, %struct.ScmObj* %argslist54304$ae518882)
ret void
}

define tailcc void @proc_clo$ae51888(%struct.ScmObj* %env$ae51888,%struct.ScmObj* %current_45args54271) {
%stackaddr$env-ref55169 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51888, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55169
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54271)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%current_45args54272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54271)
store volatile %struct.ScmObj* %current_45args54272, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54272)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim55172, align 8
%stackaddr$makeclosure55173 = alloca %struct.ScmObj*, align 8
%fptrToInt55174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51933 to i64
%ae51933 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55174)
store volatile %struct.ScmObj* %ae51933, %struct.ScmObj** %stackaddr$makeclosure55173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51933, %struct.ScmObj* %anf_45bind48264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51933, %struct.ScmObj* %anf_45bind48266, i64 1)
%ae51934 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55175 = alloca %struct.ScmObj*, align 8
%fptrToInt55176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51935 to i64
%ae51935 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55176)
store volatile %struct.ScmObj* %ae51935, %struct.ScmObj** %stackaddr$makeclosure55175, align 8
%argslist54294$ae519330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%argslist54294$ae519331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51935, %struct.ScmObj* %argslist54294$ae519330)
store volatile %struct.ScmObj* %argslist54294$ae519331, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%argslist54294$ae519332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51934, %struct.ScmObj* %argslist54294$ae519331)
store volatile %struct.ScmObj* %argslist54294$ae519332, %struct.ScmObj** %stackaddr$prim55178, align 8
%clofunc55179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51933)
musttail call tailcc void %clofunc55179(%struct.ScmObj* %ae51933, %struct.ScmObj* %argslist54294$ae519332)
ret void
}

define tailcc void @proc_clo$ae51933(%struct.ScmObj* %env$ae51933,%struct.ScmObj* %current_45args54274) {
%stackaddr$env-ref55180 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51933, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55180
%stackaddr$env-ref55181 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51933, i64 1)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55181
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim55182, align 8
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%current_45args54275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %current_45args54275, %struct.ScmObj** %stackaddr$prim55183, align 8
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$makeclosure55185 = alloca %struct.ScmObj*, align 8
%fptrToInt55186 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51956 to i64
%ae51956 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55186)
store volatile %struct.ScmObj* %ae51956, %struct.ScmObj** %stackaddr$makeclosure55185, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51956, %struct.ScmObj* %anf_45bind48264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51956, %struct.ScmObj* %anf_45bind48266, i64 1)
%ae51957 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist54292$anf_45bind482670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%argslist54292$anf_45bind482671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51957, %struct.ScmObj* %argslist54292$anf_45bind482670)
store volatile %struct.ScmObj* %argslist54292$anf_45bind482671, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%argslist54292$anf_45bind482672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51956, %struct.ScmObj* %argslist54292$anf_45bind482671)
store volatile %struct.ScmObj* %argslist54292$anf_45bind482672, %struct.ScmObj** %stackaddr$prim55188, align 8
%clofunc55189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48267)
musttail call tailcc void %clofunc55189(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist54292$anf_45bind482672)
ret void
}

define tailcc void @proc_clo$ae51956(%struct.ScmObj* %env$ae51956,%struct.ScmObj* %current_45args54277) {
%stackaddr$env-ref55190 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51956, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55190
%stackaddr$env-ref55191 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51956, i64 1)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55191
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%current_45args54278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %current_45args54278, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54278)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim55194, align 8
%stackaddr$makeclosure55195 = alloca %struct.ScmObj*, align 8
%fptrToInt55196 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51961 to i64
%ae51961 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55196)
store volatile %struct.ScmObj* %ae51961, %struct.ScmObj** %stackaddr$makeclosure55195, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51961, %struct.ScmObj* %anf_45bind48264, i64 0)
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%cpsargs48313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51961, %struct.ScmObj* %anf_45bind48268)
store volatile %struct.ScmObj* %cpsargs48313, %struct.ScmObj** %stackaddr$prim55197, align 8
%clofunc55198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48266)
musttail call tailcc void %clofunc55198(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %cpsargs48313)
ret void
}

define tailcc void @proc_clo$ae51961(%struct.ScmObj* %env$ae51961,%struct.ScmObj* %current_45args54280) {
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51961, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%current_45args54281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %current_45args54281, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$makeclosure55203 = alloca %struct.ScmObj*, align 8
%fptrToInt55204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51966 to i64
%ae51966 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55204)
store volatile %struct.ScmObj* %ae51966, %struct.ScmObj** %stackaddr$makeclosure55203, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %anf_45bind48264, i64 0)
%ae51967 = call %struct.ScmObj* @const_init_int(i64 5)
%ae51968 = call %struct.ScmObj* @const_init_int(i64 6)
%argslist54291$anf_45bind482690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%argslist54291$anf_45bind482691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51968, %struct.ScmObj* %argslist54291$anf_45bind482690)
store volatile %struct.ScmObj* %argslist54291$anf_45bind482691, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%argslist54291$anf_45bind482692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51967, %struct.ScmObj* %argslist54291$anf_45bind482691)
store volatile %struct.ScmObj* %argslist54291$anf_45bind482692, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%argslist54291$anf_45bind482693 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51966, %struct.ScmObj* %argslist54291$anf_45bind482692)
store volatile %struct.ScmObj* %argslist54291$anf_45bind482693, %struct.ScmObj** %stackaddr$prim55207, align 8
%clofunc55208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48269)
musttail call tailcc void %clofunc55208(%struct.ScmObj* %anf_45bind48269, %struct.ScmObj* %argslist54291$anf_45bind482693)
ret void
}

define tailcc void @proc_clo$ae51966(%struct.ScmObj* %env$ae51966,%struct.ScmObj* %current_45args54283) {
%stackaddr$env-ref55209 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55209
%stackaddr$prim55210 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54283)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim55210, align 8
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%current_45args54284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54283)
store volatile %struct.ScmObj* %current_45args54284, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54284)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%cpsprim48312 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %cpsprim48312, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$makeclosure55214 = alloca %struct.ScmObj*, align 8
%fptrToInt55215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51979 to i64
%ae51979 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55215)
store volatile %struct.ScmObj* %ae51979, %struct.ScmObj** %stackaddr$makeclosure55214, align 8
%ae51980 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54290$ae519790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%argslist54290$ae519791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48312, %struct.ScmObj* %argslist54290$ae519790)
store volatile %struct.ScmObj* %argslist54290$ae519791, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%argslist54290$ae519792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51980, %struct.ScmObj* %argslist54290$ae519791)
store volatile %struct.ScmObj* %argslist54290$ae519792, %struct.ScmObj** %stackaddr$prim55217, align 8
%clofunc55218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51979)
musttail call tailcc void %clofunc55218(%struct.ScmObj* %ae51979, %struct.ScmObj* %argslist54290$ae519792)
ret void
}

define tailcc void @proc_clo$ae51979(%struct.ScmObj* %env$ae51979,%struct.ScmObj* %current_45args54286) {
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55219, align 8
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%current_45args54287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %current_45args54287, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55222, align 8
%argslist54289$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%argslist54289$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54289$k0)
store volatile %struct.ScmObj* %argslist54289$k1, %struct.ScmObj** %stackaddr$prim55223, align 8
%clofunc55224 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55224(%struct.ScmObj* %k, %struct.ScmObj* %argslist54289$k1)
ret void
}

define tailcc void @proc_clo$ae51935(%struct.ScmObj* %env$ae51935,%struct.ScmObj* %lst4814948314) {
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814948314)
store volatile %struct.ScmObj* %k48315, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%lst48149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814948314)
store volatile %struct.ScmObj* %lst48149, %struct.ScmObj** %stackaddr$prim55226, align 8
%ae51939 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54293$k483150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%argslist54293$k483151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48149, %struct.ScmObj* %argslist54293$k483150)
store volatile %struct.ScmObj* %argslist54293$k483151, %struct.ScmObj** %stackaddr$prim55227, align 8
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%argslist54293$k483152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51939, %struct.ScmObj* %argslist54293$k483151)
store volatile %struct.ScmObj* %argslist54293$k483152, %struct.ScmObj** %stackaddr$prim55228, align 8
%clofunc55229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48315)
musttail call tailcc void %clofunc55229(%struct.ScmObj* %k48315, %struct.ScmObj* %argslist54293$k483152)
ret void
}

define tailcc void @proc_clo$ae51890(%struct.ScmObj* %env$ae51890,%struct.ScmObj* %current_45args54295) {
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%current_45args54296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %current_45args54296, %struct.ScmObj** %stackaddr$prim55231, align 8
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%u48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54296)
store volatile %struct.ScmObj* %u48146, %struct.ScmObj** %stackaddr$prim55232, align 8
%ae51892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55233 = alloca %struct.ScmObj*, align 8
%fptrToInt55234 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51893 to i64
%ae51893 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55234)
store volatile %struct.ScmObj* %ae51893, %struct.ScmObj** %stackaddr$makeclosure55233, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51893, %struct.ScmObj* %u48146, i64 0)
%argslist54303$k483160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%argslist54303$k483161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51893, %struct.ScmObj* %argslist54303$k483160)
store volatile %struct.ScmObj* %argslist54303$k483161, %struct.ScmObj** %stackaddr$prim55235, align 8
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%argslist54303$k483162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51892, %struct.ScmObj* %argslist54303$k483161)
store volatile %struct.ScmObj* %argslist54303$k483162, %struct.ScmObj** %stackaddr$prim55236, align 8
%clofunc55237 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48316)
musttail call tailcc void %clofunc55237(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist54303$k483162)
ret void
}

define tailcc void @proc_clo$ae51893(%struct.ScmObj* %env$ae51893,%struct.ScmObj* %current_45args54298) {
%stackaddr$env-ref55238 = alloca %struct.ScmObj*, align 8
%u48146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51893, i64 0)
store %struct.ScmObj* %u48146, %struct.ScmObj** %stackaddr$env-ref55238
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54298)
store volatile %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%current_45args54299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54298)
store volatile %struct.ScmObj* %current_45args54299, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%v48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %v48148, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%current_45args54300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %current_45args54300, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%w48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %w48147, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %v48148, %struct.ScmObj* %w48147)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%cpsprim48318 = call %struct.ScmObj* @prim__42(%struct.ScmObj* %u48146, %struct.ScmObj* %anf_45bind48265)
store volatile %struct.ScmObj* %cpsprim48318, %struct.ScmObj** %stackaddr$prim55245, align 8
%ae51899 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54302$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%argslist54302$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48318, %struct.ScmObj* %argslist54302$k483170)
store volatile %struct.ScmObj* %argslist54302$k483171, %struct.ScmObj** %stackaddr$prim55246, align 8
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%argslist54302$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51899, %struct.ScmObj* %argslist54302$k483171)
store volatile %struct.ScmObj* %argslist54302$k483172, %struct.ScmObj** %stackaddr$prim55247, align 8
%clofunc55248 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc55248(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54302$k483172)
ret void
}

define tailcc void @proc_clo$ae51852(%struct.ScmObj* %env$ae51852,%struct.ScmObj* %lst4814548320) {
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814548320)
store volatile %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%lst48145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814548320)
store volatile %struct.ScmObj* %lst48145, %struct.ScmObj** %stackaddr$prim55250, align 8
%ae51856 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54306$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%argslist54306$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48145, %struct.ScmObj* %argslist54306$k483210)
store volatile %struct.ScmObj* %argslist54306$k483211, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%argslist54306$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51856, %struct.ScmObj* %argslist54306$k483211)
store volatile %struct.ScmObj* %argslist54306$k483212, %struct.ScmObj** %stackaddr$prim55252, align 8
%clofunc55253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55253(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54306$k483212)
ret void
}

define tailcc void @proc_clo$ae51830(%struct.ScmObj* %env$ae51830,%struct.ScmObj* %current_45args54308) {
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%current_45args54309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54308)
store volatile %struct.ScmObj* %current_45args54309, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%x48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %x48144, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%current_45args54310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %current_45args54310, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%y48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54310)
store volatile %struct.ScmObj* %y48143, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%cpsprim48323 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %y48143, %struct.ScmObj* %x48144)
store volatile %struct.ScmObj* %cpsprim48323, %struct.ScmObj** %stackaddr$prim55259, align 8
%ae51834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54312$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%argslist54312$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48323, %struct.ScmObj* %argslist54312$k483220)
store volatile %struct.ScmObj* %argslist54312$k483221, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%argslist54312$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51834, %struct.ScmObj* %argslist54312$k483221)
store volatile %struct.ScmObj* %argslist54312$k483222, %struct.ScmObj** %stackaddr$prim55261, align 8
%clofunc55262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55262(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54312$k483222)
ret void
}

define tailcc void @proc_clo$ae51804(%struct.ScmObj* %env$ae51804,%struct.ScmObj* %current_45args54314) {
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%current_45args54315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %current_45args54315, %struct.ScmObj** %stackaddr$prim55264, align 8
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%cpsprim48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %cpsprim48325, %struct.ScmObj** %stackaddr$prim55269, align 8
%ae51810 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54317$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%argslist54317$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48325, %struct.ScmObj* %argslist54317$k483240)
store volatile %struct.ScmObj* %argslist54317$k483241, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%argslist54317$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51810, %struct.ScmObj* %argslist54317$k483241)
store volatile %struct.ScmObj* %argslist54317$k483242, %struct.ScmObj** %stackaddr$prim55271, align 8
%clofunc55272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc55272(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54317$k483242)
ret void
}

define tailcc void @proc_clo$ae51780(%struct.ScmObj* %env$ae51780,%struct.ScmObj* %current_45args54319) {
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %k48326, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%current_45args54320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %current_45args54320, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim55275, align 8
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%cpsprim48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %cpsprim48327, %struct.ScmObj** %stackaddr$prim55278, align 8
%ae51785 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54322$k483260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%argslist54322$k483261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48327, %struct.ScmObj* %argslist54322$k483260)
store volatile %struct.ScmObj* %argslist54322$k483261, %struct.ScmObj** %stackaddr$prim55279, align 8
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%argslist54322$k483262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51785, %struct.ScmObj* %argslist54322$k483261)
store volatile %struct.ScmObj* %argslist54322$k483262, %struct.ScmObj** %stackaddr$prim55280, align 8
%clofunc55281 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48326)
musttail call tailcc void %clofunc55281(%struct.ScmObj* %k48326, %struct.ScmObj* %argslist54322$k483262)
ret void
}

define tailcc void @proc_clo$ae51758(%struct.ScmObj* %env$ae51758,%struct.ScmObj* %current_45args54324) {
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %k48328, %struct.ScmObj** %stackaddr$prim55282, align 8
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%current_45args54325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %current_45args54325, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%cpsprim48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48329, %struct.ScmObj** %stackaddr$prim55286, align 8
%ae51762 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54327$k483280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%argslist54327$k483281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48329, %struct.ScmObj* %argslist54327$k483280)
store volatile %struct.ScmObj* %argslist54327$k483281, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%argslist54327$k483282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51762, %struct.ScmObj* %argslist54327$k483281)
store volatile %struct.ScmObj* %argslist54327$k483282, %struct.ScmObj** %stackaddr$prim55288, align 8
%clofunc55289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48328)
musttail call tailcc void %clofunc55289(%struct.ScmObj* %k48328, %struct.ScmObj* %argslist54327$k483282)
ret void
}

define tailcc void @proc_clo$ae51738(%struct.ScmObj* %env$ae51738,%struct.ScmObj* %current_45args54329) {
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54329)
store volatile %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%current_45args54330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54329)
store volatile %struct.ScmObj* %current_45args54330, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%cpsprim48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48331, %struct.ScmObj** %stackaddr$prim55293, align 8
%ae51741 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54332$k483300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%argslist54332$k483301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48331, %struct.ScmObj* %argslist54332$k483300)
store volatile %struct.ScmObj* %argslist54332$k483301, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%argslist54332$k483302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51741, %struct.ScmObj* %argslist54332$k483301)
store volatile %struct.ScmObj* %argslist54332$k483302, %struct.ScmObj** %stackaddr$prim55295, align 8
%clofunc55296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48330)
musttail call tailcc void %clofunc55296(%struct.ScmObj* %k48330, %struct.ScmObj* %argslist54332$k483302)
ret void
}

define tailcc void @proc_clo$ae51640(%struct.ScmObj* %env$ae51640,%struct.ScmObj* %args4809148332) {
%stackaddr$env-ref55297 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51640, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55297
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148332)
store volatile %struct.ScmObj* %k48333, %struct.ScmObj** %stackaddr$prim55298, align 8
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148332)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55300, align 8
%truthy$cmp55301 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp55301 = icmp eq i64 %truthy$cmp55301, 1
br i1 %cmp$cmp55301, label %truebranch$cmp55301, label %falsebranch$cmp55301
truebranch$cmp55301:
%ae51646 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51647 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54334$k483330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55302 = alloca %struct.ScmObj*, align 8
%argslist54334$k483331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51647, %struct.ScmObj* %argslist54334$k483330)
store volatile %struct.ScmObj* %argslist54334$k483331, %struct.ScmObj** %stackaddr$prim55302, align 8
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%argslist54334$k483332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51646, %struct.ScmObj* %argslist54334$k483331)
store volatile %struct.ScmObj* %argslist54334$k483332, %struct.ScmObj** %stackaddr$prim55303, align 8
%clofunc55304 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48333)
musttail call tailcc void %clofunc55304(%struct.ScmObj* %k48333, %struct.ScmObj* %argslist54334$k483332)
ret void
falsebranch$cmp55301:
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55306, align 8
%truthy$cmp55307 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48251)
%cmp$cmp55307 = icmp eq i64 %truthy$cmp55307, 1
br i1 %cmp$cmp55307, label %truebranch$cmp55307, label %falsebranch$cmp55307
truebranch$cmp55307:
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim55308, align 8
%ae51659 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54335$k483330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%argslist54335$k483331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist54335$k483330)
store volatile %struct.ScmObj* %argslist54335$k483331, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%argslist54335$k483332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51659, %struct.ScmObj* %argslist54335$k483331)
store volatile %struct.ScmObj* %argslist54335$k483332, %struct.ScmObj** %stackaddr$prim55310, align 8
%clofunc55311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48333)
musttail call tailcc void %clofunc55311(%struct.ScmObj* %k48333, %struct.ScmObj* %argslist54335$k483332)
ret void
falsebranch$cmp55307:
%stackaddr$makeclosure55312 = alloca %struct.ScmObj*, align 8
%fptrToInt55313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51664 to i64
%ae51664 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55313)
store volatile %struct.ScmObj* %ae51664, %struct.ScmObj** %stackaddr$makeclosure55312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %k48333, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %args48091, i64 2)
%ae51665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55314 = alloca %struct.ScmObj*, align 8
%fptrToInt55315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51666 to i64
%ae51666 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55315)
store volatile %struct.ScmObj* %ae51666, %struct.ScmObj** %stackaddr$makeclosure55314, align 8
%argslist54345$ae516640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%argslist54345$ae516641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51666, %struct.ScmObj* %argslist54345$ae516640)
store volatile %struct.ScmObj* %argslist54345$ae516641, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%argslist54345$ae516642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51665, %struct.ScmObj* %argslist54345$ae516641)
store volatile %struct.ScmObj* %argslist54345$ae516642, %struct.ScmObj** %stackaddr$prim55317, align 8
%clofunc55318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51664)
musttail call tailcc void %clofunc55318(%struct.ScmObj* %ae51664, %struct.ScmObj* %argslist54345$ae516642)
ret void
}

define tailcc void @proc_clo$ae51664(%struct.ScmObj* %env$ae51664,%struct.ScmObj* %current_45args54336) {
%stackaddr$env-ref55319 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55319
%stackaddr$env-ref55320 = alloca %struct.ScmObj*, align 8
%k48333 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 1)
store %struct.ScmObj* %k48333, %struct.ScmObj** %stackaddr$env-ref55320
%stackaddr$env-ref55321 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref55321
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%current_45args54337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %current_45args54337, %struct.ScmObj** %stackaddr$prim55323, align 8
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55324, align 8
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55325, align 8
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55326, align 8
%argslist54339$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%argslist54339$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %argslist54339$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54339$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55327, align 8
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%argslist54339$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %argslist54339$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54339$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%argslist54339$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %argslist54339$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54339$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist54339$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48333, %struct.ScmObj* %argslist54339$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54339$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55330, align 8
%clofunc55331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55331(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54339$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51666(%struct.ScmObj* %env$ae51666,%struct.ScmObj* %current_45args54340) {
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %k48336, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%current_45args54341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %current_45args54341, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%current_45args54342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %current_45args54342, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim55336, align 8
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%cpsprim48337 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48337, %struct.ScmObj** %stackaddr$prim55337, align 8
%ae51670 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54344$k483360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist54344$k483361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48337, %struct.ScmObj* %argslist54344$k483360)
store volatile %struct.ScmObj* %argslist54344$k483361, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist54344$k483362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51670, %struct.ScmObj* %argslist54344$k483361)
store volatile %struct.ScmObj* %argslist54344$k483362, %struct.ScmObj** %stackaddr$prim55339, align 8
%clofunc55340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48336)
musttail call tailcc void %clofunc55340(%struct.ScmObj* %k48336, %struct.ScmObj* %argslist54344$k483362)
ret void
}

define tailcc void @proc_clo$ae51236(%struct.ScmObj* %env$ae51236,%struct.ScmObj* %current_45args54347) {
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54347)
store volatile %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%current_45args54348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54347)
store volatile %struct.ScmObj* %current_45args54348, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%current_45args54349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %current_45args54349, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim55345, align 8
%ae51237 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51237, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim55346, align 8
%stackaddr$makeclosure55347 = alloca %struct.ScmObj*, align 8
%fptrToInt55348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51239 to i64
%ae51239 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55348)
store volatile %struct.ScmObj* %ae51239, %struct.ScmObj** %stackaddr$makeclosure55347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51239, %struct.ScmObj* %v48096, i64 2)
%ae51240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55349 = alloca %struct.ScmObj*, align 8
%fptrToInt55350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51241 to i64
%ae51241 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55350)
store volatile %struct.ScmObj* %ae51241, %struct.ScmObj** %stackaddr$makeclosure55349, align 8
%argslist54371$ae512390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%argslist54371$ae512391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51241, %struct.ScmObj* %argslist54371$ae512390)
store volatile %struct.ScmObj* %argslist54371$ae512391, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%argslist54371$ae512392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51240, %struct.ScmObj* %argslist54371$ae512391)
store volatile %struct.ScmObj* %argslist54371$ae512392, %struct.ScmObj** %stackaddr$prim55352, align 8
%clofunc55353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51239)
musttail call tailcc void %clofunc55353(%struct.ScmObj* %ae51239, %struct.ScmObj* %argslist54371$ae512392)
ret void
}

define tailcc void @proc_clo$ae51239(%struct.ScmObj* %env$ae51239,%struct.ScmObj* %current_45args54351) {
%stackaddr$env-ref55354 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref55354
%stackaddr$env-ref55355 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55355
%stackaddr$env-ref55356 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51239, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55356
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54351)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%current_45args54352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54351)
store volatile %struct.ScmObj* %current_45args54352, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$makeclosure55360 = alloca %struct.ScmObj*, align 8
%fptrToInt55361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51255 to i64
%ae51255 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55361)
store volatile %struct.ScmObj* %ae51255, %struct.ScmObj** %stackaddr$makeclosure55360, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51255, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51255, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51255, %struct.ScmObj* %v48096, i64 2)
%stackaddr$makeclosure55362 = alloca %struct.ScmObj*, align 8
%fptrToInt55363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51256 to i64
%ae51256 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55363)
store volatile %struct.ScmObj* %ae51256, %struct.ScmObj** %stackaddr$makeclosure55362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51256, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51256, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51256, %struct.ScmObj* %v48096, i64 2)
%argslist54366$anf_45bind482410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist54366$anf_45bind482411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51256, %struct.ScmObj* %argslist54366$anf_45bind482410)
store volatile %struct.ScmObj* %argslist54366$anf_45bind482411, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%argslist54366$anf_45bind482412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51255, %struct.ScmObj* %argslist54366$anf_45bind482411)
store volatile %struct.ScmObj* %argslist54366$anf_45bind482412, %struct.ScmObj** %stackaddr$prim55365, align 8
%clofunc55366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48241)
musttail call tailcc void %clofunc55366(%struct.ScmObj* %anf_45bind48241, %struct.ScmObj* %argslist54366$anf_45bind482412)
ret void
}

define tailcc void @proc_clo$ae51255(%struct.ScmObj* %env$ae51255,%struct.ScmObj* %current_45args54354) {
%stackaddr$env-ref55367 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51255, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref55367
%stackaddr$env-ref55368 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51255, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55368
%stackaddr$env-ref55369 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51255, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55369
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54354)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%current_45args54355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54354)
store volatile %struct.ScmObj* %current_45args54355, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55372, align 8
%ae51364 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51364)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55374, align 8
%truthy$cmp55375 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48243)
%cmp$cmp55375 = icmp eq i64 %truthy$cmp55375, 1
br i1 %cmp$cmp55375, label %truebranch$cmp55375, label %falsebranch$cmp55375
truebranch$cmp55375:
%ae51368 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51369 = call %struct.ScmObj* @const_init_false()
%argslist54357$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%argslist54357$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51369, %struct.ScmObj* %argslist54357$k483380)
store volatile %struct.ScmObj* %argslist54357$k483381, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist54357$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51368, %struct.ScmObj* %argslist54357$k483381)
store volatile %struct.ScmObj* %argslist54357$k483382, %struct.ScmObj** %stackaddr$prim55377, align 8
%clofunc55378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc55378(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54357$k483382)
ret void
falsebranch$cmp55375:
%ae51377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51377)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48245, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55381, align 8
%truthy$cmp55382 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48246)
%cmp$cmp55382 = icmp eq i64 %truthy$cmp55382, 1
br i1 %cmp$cmp55382, label %truebranch$cmp55382, label %falsebranch$cmp55382
truebranch$cmp55382:
%ae51383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%cpsprim48341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51383)
store volatile %struct.ScmObj* %cpsprim48341, %struct.ScmObj** %stackaddr$prim55383, align 8
%ae51385 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54358$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%argslist54358$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48341, %struct.ScmObj* %argslist54358$k483380)
store volatile %struct.ScmObj* %argslist54358$k483381, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%argslist54358$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51385, %struct.ScmObj* %argslist54358$k483381)
store volatile %struct.ScmObj* %argslist54358$k483382, %struct.ScmObj** %stackaddr$prim55385, align 8
%clofunc55386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc55386(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54358$k483382)
ret void
falsebranch$cmp55382:
%ae51396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51396)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55388, align 8
%ae51399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51399, %struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55389, align 8
%argslist54359$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist54359$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54359$cc480980)
store volatile %struct.ScmObj* %argslist54359$cc480981, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%argslist54359$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54359$cc480981)
store volatile %struct.ScmObj* %argslist54359$cc480982, %struct.ScmObj** %stackaddr$prim55391, align 8
%clofunc55392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55392(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54359$cc480982)
ret void
}

define tailcc void @proc_clo$ae51256(%struct.ScmObj* %env$ae51256,%struct.ScmObj* %current_45args54360) {
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51256, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$env-ref55394 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51256, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55394
%stackaddr$env-ref55395 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51256, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55395
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%current_45args54361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %current_45args54361, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54361)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55398, align 8
%ae51258 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51258)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55400, align 8
%truthy$cmp55401 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48243)
%cmp$cmp55401 = icmp eq i64 %truthy$cmp55401, 1
br i1 %cmp$cmp55401, label %truebranch$cmp55401, label %falsebranch$cmp55401
truebranch$cmp55401:
%ae51262 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51263 = call %struct.ScmObj* @const_init_false()
%argslist54363$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist54363$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51263, %struct.ScmObj* %argslist54363$k483380)
store volatile %struct.ScmObj* %argslist54363$k483381, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%argslist54363$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51262, %struct.ScmObj* %argslist54363$k483381)
store volatile %struct.ScmObj* %argslist54363$k483382, %struct.ScmObj** %stackaddr$prim55403, align 8
%clofunc55404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc55404(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54363$k483382)
ret void
falsebranch$cmp55401:
%ae51271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51271)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48245, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55407, align 8
%truthy$cmp55408 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48246)
%cmp$cmp55408 = icmp eq i64 %truthy$cmp55408, 1
br i1 %cmp$cmp55408, label %truebranch$cmp55408, label %falsebranch$cmp55408
truebranch$cmp55408:
%ae51277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%cpsprim48341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51277)
store volatile %struct.ScmObj* %cpsprim48341, %struct.ScmObj** %stackaddr$prim55409, align 8
%ae51279 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54364$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%argslist54364$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48341, %struct.ScmObj* %argslist54364$k483380)
store volatile %struct.ScmObj* %argslist54364$k483381, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%argslist54364$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51279, %struct.ScmObj* %argslist54364$k483381)
store volatile %struct.ScmObj* %argslist54364$k483382, %struct.ScmObj** %stackaddr$prim55411, align 8
%clofunc55412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc55412(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54364$k483382)
ret void
falsebranch$cmp55408:
%ae51290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51290)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55413, align 8
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55414, align 8
%ae51293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51293, %struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55415, align 8
%argslist54365$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%argslist54365$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54365$cc480980)
store volatile %struct.ScmObj* %argslist54365$cc480981, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%argslist54365$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54365$cc480981)
store volatile %struct.ScmObj* %argslist54365$cc480982, %struct.ScmObj** %stackaddr$prim55417, align 8
%clofunc55418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55418(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54365$cc480982)
ret void
}

define tailcc void @proc_clo$ae51241(%struct.ScmObj* %env$ae51241,%struct.ScmObj* %current_45args54367) {
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%current_45args54368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %current_45args54368, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54368)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim55421, align 8
%argslist54370$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%argslist54370$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54370$u480990)
store volatile %struct.ScmObj* %argslist54370$u480991, %struct.ScmObj** %stackaddr$prim55422, align 8
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%argslist54370$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist54370$u480991)
store volatile %struct.ScmObj* %argslist54370$u480992, %struct.ScmObj** %stackaddr$prim55423, align 8
%clofunc55424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc55424(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54370$u480992)
ret void
}

define tailcc void @proc_clo$ae50700(%struct.ScmObj* %env$ae50700,%struct.ScmObj* %current_45args54373) {
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%current_45args54374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %current_45args54374, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim55427, align 8
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%current_45args54375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %current_45args54375, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim55429, align 8
%ae50701 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50701, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim55430, align 8
%ae50703 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50703, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$makeclosure55432 = alloca %struct.ScmObj*, align 8
%fptrToInt55433 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50705 to i64
%ae50705 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55433)
store volatile %struct.ScmObj* %ae50705, %struct.ScmObj** %stackaddr$makeclosure55432, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50705, %struct.ScmObj* %k48343, i64 2)
%ae50706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55434 = alloca %struct.ScmObj*, align 8
%fptrToInt55435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50707 to i64
%ae50707 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55435)
store volatile %struct.ScmObj* %ae50707, %struct.ScmObj** %stackaddr$makeclosure55434, align 8
%argslist54395$ae507050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%argslist54395$ae507051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50707, %struct.ScmObj* %argslist54395$ae507050)
store volatile %struct.ScmObj* %argslist54395$ae507051, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%argslist54395$ae507052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50706, %struct.ScmObj* %argslist54395$ae507051)
store volatile %struct.ScmObj* %argslist54395$ae507052, %struct.ScmObj** %stackaddr$prim55437, align 8
%clofunc55438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50705)
musttail call tailcc void %clofunc55438(%struct.ScmObj* %ae50705, %struct.ScmObj* %argslist54395$ae507052)
ret void
}

define tailcc void @proc_clo$ae50705(%struct.ScmObj* %env$ae50705,%struct.ScmObj* %current_45args54377) {
%stackaddr$env-ref55439 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55439
%stackaddr$env-ref55440 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55440
%stackaddr$env-ref55441 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50705, i64 2)
store %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$env-ref55441
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54377)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%current_45args54378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54377)
store volatile %struct.ScmObj* %current_45args54378, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55444, align 8
%stackaddr$makeclosure55445 = alloca %struct.ScmObj*, align 8
%fptrToInt55446 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50721 to i64
%ae50721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55446)
store volatile %struct.ScmObj* %ae50721, %struct.ScmObj** %stackaddr$makeclosure55445, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50721, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50721, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50721, %struct.ScmObj* %k48343, i64 2)
%stackaddr$makeclosure55447 = alloca %struct.ScmObj*, align 8
%fptrToInt55448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50722 to i64
%ae50722 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55448)
store volatile %struct.ScmObj* %ae50722, %struct.ScmObj** %stackaddr$makeclosure55447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50722, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50722, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50722, %struct.ScmObj* %k48343, i64 2)
%argslist54390$anf_45bind482340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%argslist54390$anf_45bind482341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist54390$anf_45bind482340)
store volatile %struct.ScmObj* %argslist54390$anf_45bind482341, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%argslist54390$anf_45bind482342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50721, %struct.ScmObj* %argslist54390$anf_45bind482341)
store volatile %struct.ScmObj* %argslist54390$anf_45bind482342, %struct.ScmObj** %stackaddr$prim55450, align 8
%clofunc55451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48234)
musttail call tailcc void %clofunc55451(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %argslist54390$anf_45bind482342)
ret void
}

define tailcc void @proc_clo$ae50721(%struct.ScmObj* %env$ae50721,%struct.ScmObj* %current_45args54380) {
%stackaddr$env-ref55452 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50721, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55452
%stackaddr$env-ref55453 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50721, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55453
%stackaddr$env-ref55454 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50721, i64 2)
store %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$env-ref55454
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%current_45args54381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %current_45args54381, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54381)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55457, align 8
%ae50864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50864)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55458, align 8
%ae50865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50865, %struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55459, align 8
%truthy$cmp55460 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48236)
%cmp$cmp55460 = icmp eq i64 %truthy$cmp55460, 1
br i1 %cmp$cmp55460, label %truebranch$cmp55460, label %falsebranch$cmp55460
truebranch$cmp55460:
%ae50869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50869)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim55461, align 8
%ae50871 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54383$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%argslist54383$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist54383$k483430)
store volatile %struct.ScmObj* %argslist54383$k483431, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%argslist54383$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist54383$k483431)
store volatile %struct.ScmObj* %argslist54383$k483432, %struct.ScmObj** %stackaddr$prim55463, align 8
%clofunc55464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55464(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54383$k483432)
ret void
falsebranch$cmp55460:
%ae50882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50882)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55466, align 8
%ae50885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50885, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55467, align 8
%ae50888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50888)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55468, align 8
%ae50890 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %ae50890)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55469, align 8
%ae50892 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50892, %struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55470, align 8
%argslist54384$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%argslist54384$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54384$cc481060)
store volatile %struct.ScmObj* %argslist54384$cc481061, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist54384$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54384$cc481061)
store volatile %struct.ScmObj* %argslist54384$cc481062, %struct.ScmObj** %stackaddr$prim55472, align 8
%clofunc55473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55473(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54384$cc481062)
ret void
}

define tailcc void @proc_clo$ae50722(%struct.ScmObj* %env$ae50722,%struct.ScmObj* %current_45args54385) {
%stackaddr$env-ref55474 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50722, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55474
%stackaddr$env-ref55475 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50722, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55475
%stackaddr$env-ref55476 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50722, i64 2)
store %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$env-ref55476
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%current_45args54386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %current_45args54386, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55479, align 8
%ae50724 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50724)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55480, align 8
%ae50725 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50725, %struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55481, align 8
%truthy$cmp55482 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48236)
%cmp$cmp55482 = icmp eq i64 %truthy$cmp55482, 1
br i1 %cmp$cmp55482, label %truebranch$cmp55482, label %falsebranch$cmp55482
truebranch$cmp55482:
%ae50729 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50729)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim55483, align 8
%ae50731 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54388$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%argslist54388$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist54388$k483430)
store volatile %struct.ScmObj* %argslist54388$k483431, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%argslist54388$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist54388$k483431)
store volatile %struct.ScmObj* %argslist54388$k483432, %struct.ScmObj** %stackaddr$prim55485, align 8
%clofunc55486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55486(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54388$k483432)
ret void
falsebranch$cmp55482:
%ae50742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50742)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55488, align 8
%ae50745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50745, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55489, align 8
%ae50748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50748)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55490, align 8
%ae50750 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %ae50750)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55491, align 8
%ae50752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50752, %struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55492, align 8
%argslist54389$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%argslist54389$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54389$cc481060)
store volatile %struct.ScmObj* %argslist54389$cc481061, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%argslist54389$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54389$cc481061)
store volatile %struct.ScmObj* %argslist54389$cc481062, %struct.ScmObj** %stackaddr$prim55494, align 8
%clofunc55495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55495(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54389$cc481062)
ret void
}

define tailcc void @proc_clo$ae50707(%struct.ScmObj* %env$ae50707,%struct.ScmObj* %current_45args54391) {
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$prim55496, align 8
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%current_45args54392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %current_45args54392, %struct.ScmObj** %stackaddr$prim55497, align 8
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54392)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim55498, align 8
%argslist54394$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%argslist54394$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54394$u481070)
store volatile %struct.ScmObj* %argslist54394$u481071, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%argslist54394$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54394$u481071)
store volatile %struct.ScmObj* %argslist54394$u481072, %struct.ScmObj** %stackaddr$prim55500, align 8
%clofunc55501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc55501(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54394$u481072)
ret void
}

define tailcc void @proc_clo$ae50284(%struct.ScmObj* %env$ae50284,%struct.ScmObj* %current_45args54397) {
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%current_45args54398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %current_45args54398, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim55504, align 8
%ae50285 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55505 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50285, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55505, align 8
%stackaddr$makeclosure55506 = alloca %struct.ScmObj*, align 8
%fptrToInt55507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50287 to i64
%ae50287 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55507)
store volatile %struct.ScmObj* %ae50287, %struct.ScmObj** %stackaddr$makeclosure55506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50287, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50287, %struct.ScmObj* %k48348, i64 1)
%ae50288 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55508 = alloca %struct.ScmObj*, align 8
%fptrToInt55509 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50289 to i64
%ae50289 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55509)
store volatile %struct.ScmObj* %ae50289, %struct.ScmObj** %stackaddr$makeclosure55508, align 8
%argslist54420$ae502870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%argslist54420$ae502871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50289, %struct.ScmObj* %argslist54420$ae502870)
store volatile %struct.ScmObj* %argslist54420$ae502871, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%argslist54420$ae502872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50288, %struct.ScmObj* %argslist54420$ae502871)
store volatile %struct.ScmObj* %argslist54420$ae502872, %struct.ScmObj** %stackaddr$prim55511, align 8
%clofunc55512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50287)
musttail call tailcc void %clofunc55512(%struct.ScmObj* %ae50287, %struct.ScmObj* %argslist54420$ae502872)
ret void
}

define tailcc void @proc_clo$ae50287(%struct.ScmObj* %env$ae50287,%struct.ScmObj* %current_45args54400) {
%stackaddr$env-ref55513 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50287, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55513
%stackaddr$env-ref55514 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50287, i64 1)
store %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$env-ref55514
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%_95k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %_95k48349, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$prim55516 = alloca %struct.ScmObj*, align 8
%current_45args54401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %current_45args54401, %struct.ScmObj** %stackaddr$prim55516, align 8
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54401)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55517, align 8
%stackaddr$makeclosure55518 = alloca %struct.ScmObj*, align 8
%fptrToInt55519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50306 to i64
%ae50306 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55519)
store volatile %struct.ScmObj* %ae50306, %struct.ScmObj** %stackaddr$makeclosure55518, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %k48348, i64 1)
%stackaddr$makeclosure55520 = alloca %struct.ScmObj*, align 8
%fptrToInt55521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50307 to i64
%ae50307 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55521)
store volatile %struct.ScmObj* %ae50307, %struct.ScmObj** %stackaddr$makeclosure55520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50307, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50307, %struct.ScmObj* %k48348, i64 1)
%argslist54415$anf_45bind482260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55522 = alloca %struct.ScmObj*, align 8
%argslist54415$anf_45bind482261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50307, %struct.ScmObj* %argslist54415$anf_45bind482260)
store volatile %struct.ScmObj* %argslist54415$anf_45bind482261, %struct.ScmObj** %stackaddr$prim55522, align 8
%stackaddr$prim55523 = alloca %struct.ScmObj*, align 8
%argslist54415$anf_45bind482262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50306, %struct.ScmObj* %argslist54415$anf_45bind482261)
store volatile %struct.ScmObj* %argslist54415$anf_45bind482262, %struct.ScmObj** %stackaddr$prim55523, align 8
%clofunc55524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48226)
musttail call tailcc void %clofunc55524(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist54415$anf_45bind482262)
ret void
}

define tailcc void @proc_clo$ae50306(%struct.ScmObj* %env$ae50306,%struct.ScmObj* %current_45args54403) {
%stackaddr$env-ref55525 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55525
%stackaddr$env-ref55526 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 1)
store %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$env-ref55526
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%_95k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %_95k48350, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%current_45args54404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54403)
store volatile %struct.ScmObj* %current_45args54404, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54404)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55529, align 8
%ae50422 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50422)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55531, align 8
%truthy$cmp55532 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55532 = icmp eq i64 %truthy$cmp55532, 1
br i1 %cmp$cmp55532, label %truebranch$cmp55532, label %falsebranch$cmp55532
truebranch$cmp55532:
%ae50426 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50427 = call %struct.ScmObj* @const_init_true()
%argslist54406$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%argslist54406$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50427, %struct.ScmObj* %argslist54406$k483480)
store volatile %struct.ScmObj* %argslist54406$k483481, %struct.ScmObj** %stackaddr$prim55533, align 8
%stackaddr$prim55534 = alloca %struct.ScmObj*, align 8
%argslist54406$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50426, %struct.ScmObj* %argslist54406$k483481)
store volatile %struct.ScmObj* %argslist54406$k483482, %struct.ScmObj** %stackaddr$prim55534, align 8
%clofunc55535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55535(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54406$k483482)
ret void
falsebranch$cmp55532:
%ae50435 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50435)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55537, align 8
%truthy$cmp55538 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48230)
%cmp$cmp55538 = icmp eq i64 %truthy$cmp55538, 1
br i1 %cmp$cmp55538, label %truebranch$cmp55538, label %falsebranch$cmp55538
truebranch$cmp55538:
%ae50439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50439)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55539, align 8
%stackaddr$prim55540 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55540, align 8
%ae50442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55541 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50442)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55541, align 8
%stackaddr$prim55542 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55542, align 8
%ae50445 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50445, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55543, align 8
%argslist54407$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%argslist54407$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54407$cc481130)
store volatile %struct.ScmObj* %argslist54407$cc481131, %struct.ScmObj** %stackaddr$prim55544, align 8
%stackaddr$prim55545 = alloca %struct.ScmObj*, align 8
%argslist54407$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54407$cc481131)
store volatile %struct.ScmObj* %argslist54407$cc481132, %struct.ScmObj** %stackaddr$prim55545, align 8
%clofunc55546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55546(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54407$cc481132)
ret void
falsebranch$cmp55538:
%ae50478 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50479 = call %struct.ScmObj* @const_init_false()
%argslist54408$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%argslist54408$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50479, %struct.ScmObj* %argslist54408$k483480)
store volatile %struct.ScmObj* %argslist54408$k483481, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%argslist54408$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50478, %struct.ScmObj* %argslist54408$k483481)
store volatile %struct.ScmObj* %argslist54408$k483482, %struct.ScmObj** %stackaddr$prim55548, align 8
%clofunc55549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55549(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54408$k483482)
ret void
}

define tailcc void @proc_clo$ae50307(%struct.ScmObj* %env$ae50307,%struct.ScmObj* %current_45args54409) {
%stackaddr$env-ref55550 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50307, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55550
%stackaddr$env-ref55551 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50307, i64 1)
store %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$env-ref55551
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%_95k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %_95k48350, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%current_45args54410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54409)
store volatile %struct.ScmObj* %current_45args54410, %struct.ScmObj** %stackaddr$prim55553, align 8
%stackaddr$prim55554 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54410)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55554, align 8
%ae50309 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50309)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55555, align 8
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55556, align 8
%truthy$cmp55557 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55557 = icmp eq i64 %truthy$cmp55557, 1
br i1 %cmp$cmp55557, label %truebranch$cmp55557, label %falsebranch$cmp55557
truebranch$cmp55557:
%ae50313 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50314 = call %struct.ScmObj* @const_init_true()
%argslist54412$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%argslist54412$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50314, %struct.ScmObj* %argslist54412$k483480)
store volatile %struct.ScmObj* %argslist54412$k483481, %struct.ScmObj** %stackaddr$prim55558, align 8
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%argslist54412$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50313, %struct.ScmObj* %argslist54412$k483481)
store volatile %struct.ScmObj* %argslist54412$k483482, %struct.ScmObj** %stackaddr$prim55559, align 8
%clofunc55560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55560(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54412$k483482)
ret void
falsebranch$cmp55557:
%ae50322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55561 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50322)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55561, align 8
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55562, align 8
%truthy$cmp55563 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48230)
%cmp$cmp55563 = icmp eq i64 %truthy$cmp55563, 1
br i1 %cmp$cmp55563, label %truebranch$cmp55563, label %falsebranch$cmp55563
truebranch$cmp55563:
%ae50326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50326)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55565, align 8
%ae50329 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50329)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55567, align 8
%ae50332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50332, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55568, align 8
%argslist54413$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%argslist54413$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54413$cc481130)
store volatile %struct.ScmObj* %argslist54413$cc481131, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%argslist54413$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54413$cc481131)
store volatile %struct.ScmObj* %argslist54413$cc481132, %struct.ScmObj** %stackaddr$prim55570, align 8
%clofunc55571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55571(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54413$cc481132)
ret void
falsebranch$cmp55563:
%ae50365 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50366 = call %struct.ScmObj* @const_init_false()
%argslist54414$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%argslist54414$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50366, %struct.ScmObj* %argslist54414$k483480)
store volatile %struct.ScmObj* %argslist54414$k483481, %struct.ScmObj** %stackaddr$prim55572, align 8
%stackaddr$prim55573 = alloca %struct.ScmObj*, align 8
%argslist54414$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50365, %struct.ScmObj* %argslist54414$k483481)
store volatile %struct.ScmObj* %argslist54414$k483482, %struct.ScmObj** %stackaddr$prim55573, align 8
%clofunc55574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55574(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54414$k483482)
ret void
}

define tailcc void @proc_clo$ae50289(%struct.ScmObj* %env$ae50289,%struct.ScmObj* %current_45args54416) {
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%current_45args54417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54416)
store volatile %struct.ScmObj* %current_45args54417, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$prim55577 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim55577, align 8
%ae50291 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54419$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%argslist54419$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist54419$k483510)
store volatile %struct.ScmObj* %argslist54419$k483511, %struct.ScmObj** %stackaddr$prim55578, align 8
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%argslist54419$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist54419$k483511)
store volatile %struct.ScmObj* %argslist54419$k483512, %struct.ScmObj** %stackaddr$prim55579, align 8
%clofunc55580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc55580(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist54419$k483512)
ret void
}

define tailcc void @proc_clo$ae50212(%struct.ScmObj* %env$ae50212,%struct.ScmObj* %current_45args54422) {
%stackaddr$env-ref55581 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50212, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55581
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54422)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim55582, align 8
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%current_45args54423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54422)
store volatile %struct.ScmObj* %current_45args54423, %struct.ScmObj** %stackaddr$prim55583, align 8
%stackaddr$prim55584 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim55584, align 8
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%current_45args54424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54423)
store volatile %struct.ScmObj* %current_45args54424, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55587, align 8
%truthy$cmp55588 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48220)
%cmp$cmp55588 = icmp eq i64 %truthy$cmp55588, 1
br i1 %cmp$cmp55588, label %truebranch$cmp55588, label %falsebranch$cmp55588
truebranch$cmp55588:
%ae50216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54426$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%argslist54426$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54426$k483520)
store volatile %struct.ScmObj* %argslist54426$k483521, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%argslist54426$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50216, %struct.ScmObj* %argslist54426$k483521)
store volatile %struct.ScmObj* %argslist54426$k483522, %struct.ScmObj** %stackaddr$prim55590, align 8
%clofunc55591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55591(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54426$k483522)
ret void
falsebranch$cmp55588:
%stackaddr$prim55592 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55592, align 8
%ae50223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50223)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55593, align 8
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim55594, align 8
%stackaddr$makeclosure55595 = alloca %struct.ScmObj*, align 8
%fptrToInt55596 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50226 to i64
%ae50226 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55596)
store volatile %struct.ScmObj* %ae50226, %struct.ScmObj** %stackaddr$makeclosure55595, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50226, %struct.ScmObj* %k48352, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50226, %struct.ScmObj* %anf_45bind48221, i64 1)
%argslist54431$anf_45bind482220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55597 = alloca %struct.ScmObj*, align 8
%argslist54431$anf_45bind482221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54431$anf_45bind482220)
store volatile %struct.ScmObj* %argslist54431$anf_45bind482221, %struct.ScmObj** %stackaddr$prim55597, align 8
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%argslist54431$anf_45bind482222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48223, %struct.ScmObj* %argslist54431$anf_45bind482221)
store volatile %struct.ScmObj* %argslist54431$anf_45bind482222, %struct.ScmObj** %stackaddr$prim55598, align 8
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%argslist54431$anf_45bind482223 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50226, %struct.ScmObj* %argslist54431$anf_45bind482222)
store volatile %struct.ScmObj* %argslist54431$anf_45bind482223, %struct.ScmObj** %stackaddr$prim55599, align 8
%clofunc55600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48222)
musttail call tailcc void %clofunc55600(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %argslist54431$anf_45bind482223)
ret void
}

define tailcc void @proc_clo$ae50226(%struct.ScmObj* %env$ae50226,%struct.ScmObj* %current_45args54427) {
%stackaddr$env-ref55601 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50226, i64 0)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55601
%stackaddr$env-ref55602 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50226, i64 1)
store %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$env-ref55602
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%current_45args54428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54427)
store volatile %struct.ScmObj* %current_45args54428, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54428)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%cpsprim48354 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %cpsprim48354, %struct.ScmObj** %stackaddr$prim55606, align 8
%ae50232 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54430$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55607 = alloca %struct.ScmObj*, align 8
%argslist54430$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48354, %struct.ScmObj* %argslist54430$k483520)
store volatile %struct.ScmObj* %argslist54430$k483521, %struct.ScmObj** %stackaddr$prim55607, align 8
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%argslist54430$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50232, %struct.ScmObj* %argslist54430$k483521)
store volatile %struct.ScmObj* %argslist54430$k483522, %struct.ScmObj** %stackaddr$prim55608, align 8
%clofunc55609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55609(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54430$k483522)
ret void
}

define tailcc void @proc_clo$ae50186(%struct.ScmObj* %env$ae50186,%struct.ScmObj* %current_45args54433) {
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54433)
store volatile %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$prim55610, align 8
%stackaddr$prim55611 = alloca %struct.ScmObj*, align 8
%current_45args54434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54433)
store volatile %struct.ScmObj* %current_45args54434, %struct.ScmObj** %stackaddr$prim55611, align 8
%stackaddr$prim55612 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54434)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim55612, align 8
%stackaddr$prim55613 = alloca %struct.ScmObj*, align 8
%current_45args54435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54434)
store volatile %struct.ScmObj* %current_45args54435, %struct.ScmObj** %stackaddr$prim55613, align 8
%stackaddr$prim55614 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54435)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim55614, align 8
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%cpsprim48356 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsprim48356, %struct.ScmObj** %stackaddr$prim55616, align 8
%ae50191 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54437$k483550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55617 = alloca %struct.ScmObj*, align 8
%argslist54437$k483551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48356, %struct.ScmObj* %argslist54437$k483550)
store volatile %struct.ScmObj* %argslist54437$k483551, %struct.ScmObj** %stackaddr$prim55617, align 8
%stackaddr$prim55618 = alloca %struct.ScmObj*, align 8
%argslist54437$k483552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50191, %struct.ScmObj* %argslist54437$k483551)
store volatile %struct.ScmObj* %argslist54437$k483552, %struct.ScmObj** %stackaddr$prim55618, align 8
%clofunc55619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48355)
musttail call tailcc void %clofunc55619(%struct.ScmObj* %k48355, %struct.ScmObj* %argslist54437$k483552)
ret void
}

define tailcc void @proc_clo$ae50162(%struct.ScmObj* %env$ae50162,%struct.ScmObj* %current_45args54439) {
%stackaddr$prim55620 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54439)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim55620, align 8
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%current_45args54440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54439)
store volatile %struct.ScmObj* %current_45args54440, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54440)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%current_45args54441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54440)
store volatile %struct.ScmObj* %current_45args54441, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54441)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55625, align 8
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%cpsprim48358 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsprim48358, %struct.ScmObj** %stackaddr$prim55626, align 8
%ae50167 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54443$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist54443$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48358, %struct.ScmObj* %argslist54443$k483570)
store volatile %struct.ScmObj* %argslist54443$k483571, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%argslist54443$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50167, %struct.ScmObj* %argslist54443$k483571)
store volatile %struct.ScmObj* %argslist54443$k483572, %struct.ScmObj** %stackaddr$prim55628, align 8
%clofunc55629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc55629(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54443$k483572)
ret void
}

define tailcc void @proc_clo$ae49768(%struct.ScmObj* %env$ae49768,%struct.ScmObj* %current_45args54446) {
%stackaddr$env-ref55630 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49768, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55630
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49768, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49768, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54446)
store volatile %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%current_45args54447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54446)
store volatile %struct.ScmObj* %current_45args54447, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54447)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim55635, align 8
%ae49770 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55636 = alloca %struct.ScmObj*, align 8
%fptrToInt55637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49771 to i64
%ae49771 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55637)
store volatile %struct.ScmObj* %ae49771, %struct.ScmObj** %stackaddr$makeclosure55636, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49771, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49771, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49771, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49771, %struct.ScmObj* %_37map148077, i64 3)
%argslist54504$k483590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%argslist54504$k483591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49771, %struct.ScmObj* %argslist54504$k483590)
store volatile %struct.ScmObj* %argslist54504$k483591, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%argslist54504$k483592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49770, %struct.ScmObj* %argslist54504$k483591)
store volatile %struct.ScmObj* %argslist54504$k483592, %struct.ScmObj** %stackaddr$prim55639, align 8
%clofunc55640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48359)
musttail call tailcc void %clofunc55640(%struct.ScmObj* %k48359, %struct.ScmObj* %argslist54504$k483592)
ret void
}

define tailcc void @proc_clo$ae49771(%struct.ScmObj* %env$ae49771,%struct.ScmObj* %args4813048360) {
%stackaddr$env-ref55641 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49771, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55641
%stackaddr$env-ref55642 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49771, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55642
%stackaddr$env-ref55643 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49771, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55643
%stackaddr$env-ref55644 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49771, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55644
%stackaddr$prim55645 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048360)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim55645, align 8
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048360)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim55646, align 8
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim55648, align 8
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$makeclosure55652 = alloca %struct.ScmObj*, align 8
%fptrToInt55653 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49779 to i64
%ae49779 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55653)
store volatile %struct.ScmObj* %ae49779, %struct.ScmObj** %stackaddr$makeclosure55652, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %k48361, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %_37foldr148046, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49779, %struct.ScmObj* %_37map148077, i64 7)
%ae49780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55654 = alloca %struct.ScmObj*, align 8
%fptrToInt55655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49781 to i64
%ae49781 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55655)
store volatile %struct.ScmObj* %ae49781, %struct.ScmObj** %stackaddr$makeclosure55654, align 8
%argslist54503$ae497790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55656 = alloca %struct.ScmObj*, align 8
%argslist54503$ae497791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49781, %struct.ScmObj* %argslist54503$ae497790)
store volatile %struct.ScmObj* %argslist54503$ae497791, %struct.ScmObj** %stackaddr$prim55656, align 8
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%argslist54503$ae497792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49780, %struct.ScmObj* %argslist54503$ae497791)
store volatile %struct.ScmObj* %argslist54503$ae497792, %struct.ScmObj** %stackaddr$prim55657, align 8
%clofunc55658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49779)
musttail call tailcc void %clofunc55658(%struct.ScmObj* %ae49779, %struct.ScmObj* %argslist54503$ae497792)
ret void
}

define tailcc void @proc_clo$ae49779(%struct.ScmObj* %env$ae49779,%struct.ScmObj* %current_45args54449) {
%stackaddr$env-ref55659 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55659
%stackaddr$env-ref55660 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55660
%stackaddr$env-ref55661 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55661
%stackaddr$env-ref55662 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55662
%stackaddr$env-ref55663 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55663
%stackaddr$env-ref55664 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55664
%stackaddr$env-ref55665 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 6)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55665
%stackaddr$env-ref55666 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49779, i64 7)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55666
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54449)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%current_45args54450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54449)
store volatile %struct.ScmObj* %current_45args54450, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54450)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$makeclosure55670 = alloca %struct.ScmObj*, align 8
%fptrToInt55671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49811 to i64
%ae49811 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55671)
store volatile %struct.ScmObj* %ae49811, %struct.ScmObj** %stackaddr$makeclosure55670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %k48361, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49811, %struct.ScmObj* %_37map148077, i64 6)
%ae49813 = call %struct.ScmObj* @const_init_false()
%argslist54496$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55672 = alloca %struct.ScmObj*, align 8
%argslist54496$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54496$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54496$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55672, align 8
%stackaddr$prim55673 = alloca %struct.ScmObj*, align 8
%argslist54496$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist54496$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54496$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55673, align 8
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist54496$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist54496$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54496$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%argslist54496$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49811, %struct.ScmObj* %argslist54496$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54496$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55675, align 8
%clofunc55676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55676(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54496$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49811(%struct.ScmObj* %env$ae49811,%struct.ScmObj* %current_45args54452) {
%stackaddr$env-ref55677 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55677
%stackaddr$env-ref55678 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55678
%stackaddr$env-ref55679 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55679
%stackaddr$env-ref55680 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55680
%stackaddr$env-ref55681 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55681
%stackaddr$env-ref55682 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55682
%stackaddr$env-ref55683 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49811, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55683
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54452)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%current_45args54453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54452)
store volatile %struct.ScmObj* %current_45args54453, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54453)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55686, align 8
%truthy$cmp55687 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48209)
%cmp$cmp55687 = icmp eq i64 %truthy$cmp55687, 1
br i1 %cmp$cmp55687, label %truebranch$cmp55687, label %falsebranch$cmp55687
truebranch$cmp55687:
%ae49822 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54455$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%argslist54455$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist54455$k483610)
store volatile %struct.ScmObj* %argslist54455$k483611, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%argslist54455$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49822, %struct.ScmObj* %argslist54455$k483611)
store volatile %struct.ScmObj* %argslist54455$k483612, %struct.ScmObj** %stackaddr$prim55689, align 8
%clofunc55690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc55690(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist54455$k483612)
ret void
falsebranch$cmp55687:
%stackaddr$makeclosure55691 = alloca %struct.ScmObj*, align 8
%fptrToInt55692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49827 to i64
%ae49827 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55692)
store volatile %struct.ScmObj* %ae49827, %struct.ScmObj** %stackaddr$makeclosure55691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %k48361, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49827, %struct.ScmObj* %_37map148077, i64 6)
%ae49828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55693 = alloca %struct.ScmObj*, align 8
%fptrToInt55694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49829 to i64
%ae49829 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55694)
store volatile %struct.ScmObj* %ae49829, %struct.ScmObj** %stackaddr$makeclosure55693, align 8
%argslist54495$ae498270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%argslist54495$ae498271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49829, %struct.ScmObj* %argslist54495$ae498270)
store volatile %struct.ScmObj* %argslist54495$ae498271, %struct.ScmObj** %stackaddr$prim55695, align 8
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist54495$ae498272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49828, %struct.ScmObj* %argslist54495$ae498271)
store volatile %struct.ScmObj* %argslist54495$ae498272, %struct.ScmObj** %stackaddr$prim55696, align 8
%clofunc55697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49827)
musttail call tailcc void %clofunc55697(%struct.ScmObj* %ae49827, %struct.ScmObj* %argslist54495$ae498272)
ret void
}

define tailcc void @proc_clo$ae49827(%struct.ScmObj* %env$ae49827,%struct.ScmObj* %current_45args54456) {
%stackaddr$env-ref55698 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55698
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$env-ref55701 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55701
%stackaddr$env-ref55702 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55702
%stackaddr$env-ref55703 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55703
%stackaddr$env-ref55704 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49827, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55704
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%current_45args54457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54456)
store volatile %struct.ScmObj* %current_45args54457, %struct.ScmObj** %stackaddr$prim55706, align 8
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54457)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55707, align 8
%stackaddr$makeclosure55708 = alloca %struct.ScmObj*, align 8
%fptrToInt55709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49848 to i64
%ae49848 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55709)
store volatile %struct.ScmObj* %ae49848, %struct.ScmObj** %stackaddr$makeclosure55708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %k48361, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37map148077, i64 6)
%argslist54490$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%argslist54490$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54490$_37map1480770)
store volatile %struct.ScmObj* %argslist54490$_37map1480771, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%argslist54490$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist54490$_37map1480771)
store volatile %struct.ScmObj* %argslist54490$_37map1480772, %struct.ScmObj** %stackaddr$prim55711, align 8
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%argslist54490$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49848, %struct.ScmObj* %argslist54490$_37map1480772)
store volatile %struct.ScmObj* %argslist54490$_37map1480773, %struct.ScmObj** %stackaddr$prim55712, align 8
%clofunc55713 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55713(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54490$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49848(%struct.ScmObj* %env$ae49848,%struct.ScmObj* %current_45args54459) {
%stackaddr$env-ref55714 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55714
%stackaddr$env-ref55715 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55715
%stackaddr$env-ref55716 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55716
%stackaddr$env-ref55717 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55717
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%_95k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %_95k48365, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%current_45args54460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54459)
store volatile %struct.ScmObj* %current_45args54460, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54460)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim55723, align 8
%stackaddr$makeclosure55724 = alloca %struct.ScmObj*, align 8
%fptrToInt55725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49851 to i64
%ae49851 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55725)
store volatile %struct.ScmObj* %ae49851, %struct.ScmObj** %stackaddr$makeclosure55724, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %k48361, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %lsts_4348138, i64 7)
%ae49852 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55726 = alloca %struct.ScmObj*, align 8
%fptrToInt55727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49853 to i64
%ae49853 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55727)
store volatile %struct.ScmObj* %ae49853, %struct.ScmObj** %stackaddr$makeclosure55726, align 8
%argslist54489$ae498510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%argslist54489$ae498511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49853, %struct.ScmObj* %argslist54489$ae498510)
store volatile %struct.ScmObj* %argslist54489$ae498511, %struct.ScmObj** %stackaddr$prim55728, align 8
%stackaddr$prim55729 = alloca %struct.ScmObj*, align 8
%argslist54489$ae498512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49852, %struct.ScmObj* %argslist54489$ae498511)
store volatile %struct.ScmObj* %argslist54489$ae498512, %struct.ScmObj** %stackaddr$prim55729, align 8
%clofunc55730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49851)
musttail call tailcc void %clofunc55730(%struct.ScmObj* %ae49851, %struct.ScmObj* %argslist54489$ae498512)
ret void
}

define tailcc void @proc_clo$ae49851(%struct.ScmObj* %env$ae49851,%struct.ScmObj* %current_45args54462) {
%stackaddr$env-ref55731 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55731
%stackaddr$env-ref55732 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55732
%stackaddr$env-ref55733 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55733
%stackaddr$env-ref55734 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55734
%stackaddr$env-ref55735 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55735
%stackaddr$env-ref55736 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55736
%stackaddr$env-ref55737 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55737
%stackaddr$env-ref55738 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 7)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55738
%stackaddr$prim55739 = alloca %struct.ScmObj*, align 8
%_95k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %_95k48366, %struct.ScmObj** %stackaddr$prim55739, align 8
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%current_45args54463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %current_45args54463, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54463)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55741, align 8
%stackaddr$makeclosure55742 = alloca %struct.ScmObj*, align 8
%fptrToInt55743 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49872 to i64
%ae49872 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55743)
store volatile %struct.ScmObj* %ae49872, %struct.ScmObj** %stackaddr$makeclosure55742, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %f48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %acc48132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %lsts_4348138, i64 5)
%argslist54484$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%argslist54484$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54484$_37map1480770)
store volatile %struct.ScmObj* %argslist54484$_37map1480771, %struct.ScmObj** %stackaddr$prim55744, align 8
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%argslist54484$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54484$_37map1480771)
store volatile %struct.ScmObj* %argslist54484$_37map1480772, %struct.ScmObj** %stackaddr$prim55745, align 8
%stackaddr$prim55746 = alloca %struct.ScmObj*, align 8
%argslist54484$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49872, %struct.ScmObj* %argslist54484$_37map1480772)
store volatile %struct.ScmObj* %argslist54484$_37map1480773, %struct.ScmObj** %stackaddr$prim55746, align 8
%clofunc55747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55747(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54484$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49872(%struct.ScmObj* %env$ae49872,%struct.ScmObj* %current_45args54465) {
%stackaddr$env-ref55748 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55748
%stackaddr$env-ref55749 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 1)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55749
%stackaddr$env-ref55750 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 2)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55750
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$env-ref55753 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 5)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55753
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%_95k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %_95k48367, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%current_45args54466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54465)
store volatile %struct.ScmObj* %current_45args54466, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54466)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim55756, align 8
%stackaddr$makeclosure55757 = alloca %struct.ScmObj*, align 8
%fptrToInt55758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49875 to i64
%ae49875 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55758)
store volatile %struct.ScmObj* %ae49875, %struct.ScmObj** %stackaddr$makeclosure55757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %vs48136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %_37foldl48129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %lsts_4348138, i64 6)
%ae49876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55759 = alloca %struct.ScmObj*, align 8
%fptrToInt55760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49877 to i64
%ae49877 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55760)
store volatile %struct.ScmObj* %ae49877, %struct.ScmObj** %stackaddr$makeclosure55759, align 8
%argslist54483$ae498750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%argslist54483$ae498751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49877, %struct.ScmObj* %argslist54483$ae498750)
store volatile %struct.ScmObj* %argslist54483$ae498751, %struct.ScmObj** %stackaddr$prim55761, align 8
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%argslist54483$ae498752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49876, %struct.ScmObj* %argslist54483$ae498751)
store volatile %struct.ScmObj* %argslist54483$ae498752, %struct.ScmObj** %stackaddr$prim55762, align 8
%clofunc55763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49875)
musttail call tailcc void %clofunc55763(%struct.ScmObj* %ae49875, %struct.ScmObj* %argslist54483$ae498752)
ret void
}

define tailcc void @proc_clo$ae49875(%struct.ScmObj* %env$ae49875,%struct.ScmObj* %current_45args54468) {
%stackaddr$env-ref55764 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55764
%stackaddr$env-ref55765 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 1)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref55765
%stackaddr$env-ref55766 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55766
%stackaddr$env-ref55767 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55767
%stackaddr$env-ref55768 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55768
%stackaddr$env-ref55769 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55769
%stackaddr$env-ref55770 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 6)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55770
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim55771, align 8
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%current_45args54469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54468)
store volatile %struct.ScmObj* %current_45args54469, %struct.ScmObj** %stackaddr$prim55772, align 8
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54469)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55773, align 8
%ae49898 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55774 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49898)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55774, align 8
%stackaddr$makeclosure55775 = alloca %struct.ScmObj*, align 8
%fptrToInt55776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49900 to i64
%ae49900 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55776)
store volatile %struct.ScmObj* %ae49900, %struct.ScmObj** %stackaddr$makeclosure55775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %f48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49900, %struct.ScmObj* %lsts_4348138, i64 3)
%argslist54477$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%argslist54477$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist54477$_37foldr480510)
store volatile %struct.ScmObj* %argslist54477$_37foldr480511, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%argslist54477$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist54477$_37foldr480511)
store volatile %struct.ScmObj* %argslist54477$_37foldr480512, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%argslist54477$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist54477$_37foldr480512)
store volatile %struct.ScmObj* %argslist54477$_37foldr480513, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%argslist54477$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49900, %struct.ScmObj* %argslist54477$_37foldr480513)
store volatile %struct.ScmObj* %argslist54477$_37foldr480514, %struct.ScmObj** %stackaddr$prim55780, align 8
%clofunc55781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55781(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist54477$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49900(%struct.ScmObj* %env$ae49900,%struct.ScmObj* %current_45args54471) {
%stackaddr$env-ref55782 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55782
%stackaddr$env-ref55783 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 1)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55783
%stackaddr$env-ref55784 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55784
%stackaddr$env-ref55785 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49900, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55785
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%current_45args54472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %current_45args54472, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55788, align 8
%stackaddr$makeclosure55789 = alloca %struct.ScmObj*, align 8
%fptrToInt55790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49904 to i64
%ae49904 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55790)
store volatile %struct.ScmObj* %ae49904, %struct.ScmObj** %stackaddr$makeclosure55789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49904, %struct.ScmObj* %k48361, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49904, %struct.ScmObj* %f48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49904, %struct.ScmObj* %_37foldl48129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49904, %struct.ScmObj* %lsts_4348138, i64 3)
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%cpsargs48372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49904, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %cpsargs48372, %struct.ScmObj** %stackaddr$prim55791, align 8
%clofunc55792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc55792(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48372)
ret void
}

define tailcc void @proc_clo$ae49904(%struct.ScmObj* %env$ae49904,%struct.ScmObj* %current_45args54474) {
%stackaddr$env-ref55793 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49904, i64 0)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref55793
%stackaddr$env-ref55794 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49904, i64 1)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55794
%stackaddr$env-ref55795 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49904, i64 2)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55795
%stackaddr$env-ref55796 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49904, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55796
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%current_45args54475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %current_45args54475, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54475)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48215)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim55801, align 8
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%cpsargs48371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48361, %struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %cpsargs48371, %struct.ScmObj** %stackaddr$prim55802, align 8
%clofunc55803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc55803(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48371)
ret void
}

define tailcc void @proc_clo$ae49877(%struct.ScmObj* %env$ae49877,%struct.ScmObj* %current_45args54478) {
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54478)
store volatile %struct.ScmObj* %k48373, %struct.ScmObj** %stackaddr$prim55804, align 8
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%current_45args54479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54478)
store volatile %struct.ScmObj* %current_45args54479, %struct.ScmObj** %stackaddr$prim55805, align 8
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54479)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim55806, align 8
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%current_45args54480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54479)
store volatile %struct.ScmObj* %current_45args54480, %struct.ScmObj** %stackaddr$prim55807, align 8
%stackaddr$prim55808 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54480)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim55808, align 8
%stackaddr$prim55809 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim55809, align 8
%ae49881 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54482$k483730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%argslist54482$k483731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist54482$k483730)
store volatile %struct.ScmObj* %argslist54482$k483731, %struct.ScmObj** %stackaddr$prim55810, align 8
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%argslist54482$k483732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49881, %struct.ScmObj* %argslist54482$k483731)
store volatile %struct.ScmObj* %argslist54482$k483732, %struct.ScmObj** %stackaddr$prim55811, align 8
%clofunc55812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48373)
musttail call tailcc void %clofunc55812(%struct.ScmObj* %k48373, %struct.ScmObj* %argslist54482$k483732)
ret void
}

define tailcc void @proc_clo$ae49853(%struct.ScmObj* %env$ae49853,%struct.ScmObj* %current_45args54485) {
%stackaddr$prim55813 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$prim55813, align 8
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%current_45args54486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %current_45args54486, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54486)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%cpsprim48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48376, %struct.ScmObj** %stackaddr$prim55816, align 8
%ae49856 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54488$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%argslist54488$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48376, %struct.ScmObj* %argslist54488$k483750)
store volatile %struct.ScmObj* %argslist54488$k483751, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%argslist54488$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49856, %struct.ScmObj* %argslist54488$k483751)
store volatile %struct.ScmObj* %argslist54488$k483752, %struct.ScmObj** %stackaddr$prim55818, align 8
%clofunc55819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc55819(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist54488$k483752)
ret void
}

define tailcc void @proc_clo$ae49829(%struct.ScmObj* %env$ae49829,%struct.ScmObj* %current_45args54491) {
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54491)
store volatile %struct.ScmObj* %k48377, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%current_45args54492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54491)
store volatile %struct.ScmObj* %current_45args54492, %struct.ScmObj** %stackaddr$prim55821, align 8
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim55822, align 8
%stackaddr$prim55823 = alloca %struct.ScmObj*, align 8
%cpsprim48378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48378, %struct.ScmObj** %stackaddr$prim55823, align 8
%ae49832 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54494$k483770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55824 = alloca %struct.ScmObj*, align 8
%argslist54494$k483771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48378, %struct.ScmObj* %argslist54494$k483770)
store volatile %struct.ScmObj* %argslist54494$k483771, %struct.ScmObj** %stackaddr$prim55824, align 8
%stackaddr$prim55825 = alloca %struct.ScmObj*, align 8
%argslist54494$k483772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49832, %struct.ScmObj* %argslist54494$k483771)
store volatile %struct.ScmObj* %argslist54494$k483772, %struct.ScmObj** %stackaddr$prim55825, align 8
%clofunc55826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48377)
musttail call tailcc void %clofunc55826(%struct.ScmObj* %k48377, %struct.ScmObj* %argslist54494$k483772)
ret void
}

define tailcc void @proc_clo$ae49781(%struct.ScmObj* %env$ae49781,%struct.ScmObj* %current_45args54497) {
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54497)
store volatile %struct.ScmObj* %k48379, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%current_45args54498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54497)
store volatile %struct.ScmObj* %current_45args54498, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54498)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$prim55830 = alloca %struct.ScmObj*, align 8
%current_45args54499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54498)
store volatile %struct.ScmObj* %current_45args54499, %struct.ScmObj** %stackaddr$prim55830, align 8
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54499)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim55831, align 8
%truthy$cmp55832 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp55832 = icmp eq i64 %truthy$cmp55832, 1
br i1 %cmp$cmp55832, label %truebranch$cmp55832, label %falsebranch$cmp55832
truebranch$cmp55832:
%ae49784 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54501$k483790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%argslist54501$k483791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist54501$k483790)
store volatile %struct.ScmObj* %argslist54501$k483791, %struct.ScmObj** %stackaddr$prim55833, align 8
%stackaddr$prim55834 = alloca %struct.ScmObj*, align 8
%argslist54501$k483792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49784, %struct.ScmObj* %argslist54501$k483791)
store volatile %struct.ScmObj* %argslist54501$k483792, %struct.ScmObj** %stackaddr$prim55834, align 8
%clofunc55835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48379)
musttail call tailcc void %clofunc55835(%struct.ScmObj* %k48379, %struct.ScmObj* %argslist54501$k483792)
ret void
falsebranch$cmp55832:
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim55836, align 8
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54502$k483790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%argslist54502$k483791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist54502$k483790)
store volatile %struct.ScmObj* %argslist54502$k483791, %struct.ScmObj** %stackaddr$prim55837, align 8
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%argslist54502$k483792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49791, %struct.ScmObj* %argslist54502$k483791)
store volatile %struct.ScmObj* %argslist54502$k483792, %struct.ScmObj** %stackaddr$prim55838, align 8
%clofunc55839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48379)
musttail call tailcc void %clofunc55839(%struct.ScmObj* %k48379, %struct.ScmObj* %argslist54502$k483792)
ret void
}

define tailcc void @proc_clo$ae49622(%struct.ScmObj* %env$ae49622,%struct.ScmObj* %args4807348381) {
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49622, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49622, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49622, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348381)
store volatile %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348381)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim55844, align 8
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim55846, align 8
%stackaddr$makeclosure55847 = alloca %struct.ScmObj*, align 8
%fptrToInt55848 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49627 to i64
%ae49627 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55848)
store volatile %struct.ScmObj* %ae49627, %struct.ScmObj** %stackaddr$makeclosure55847, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49627, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49627, %struct.ScmObj* %k48382, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49627, %struct.ScmObj* %lsts48074, i64 2)
%ae49628 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55849 = alloca %struct.ScmObj*, align 8
%fptrToInt55850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49629 to i64
%ae49629 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55850)
store volatile %struct.ScmObj* %ae49629, %struct.ScmObj** %stackaddr$makeclosure55849, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49629, %struct.ScmObj* %f48075, i64 2)
%argslist54521$ae496270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%argslist54521$ae496271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49629, %struct.ScmObj* %argslist54521$ae496270)
store volatile %struct.ScmObj* %argslist54521$ae496271, %struct.ScmObj** %stackaddr$prim55851, align 8
%stackaddr$prim55852 = alloca %struct.ScmObj*, align 8
%argslist54521$ae496272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49628, %struct.ScmObj* %argslist54521$ae496271)
store volatile %struct.ScmObj* %argslist54521$ae496272, %struct.ScmObj** %stackaddr$prim55852, align 8
%clofunc55853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49627)
musttail call tailcc void %clofunc55853(%struct.ScmObj* %ae49627, %struct.ScmObj* %argslist54521$ae496272)
ret void
}

define tailcc void @proc_clo$ae49627(%struct.ScmObj* %env$ae49627,%struct.ScmObj* %current_45args54506) {
%stackaddr$env-ref55854 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49627, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55854
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49627, i64 1)
store %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$env-ref55856 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49627, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref55856
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54506)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim55857, align 8
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%current_45args54507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54506)
store volatile %struct.ScmObj* %current_45args54507, %struct.ScmObj** %stackaddr$prim55858, align 8
%stackaddr$prim55859 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim55859, align 8
%ae49690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55860 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49690, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim55860, align 8
%stackaddr$prim55861 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %anf_45bind48204)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim55861, align 8
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%cpsargs48384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48382, %struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %cpsargs48384, %struct.ScmObj** %stackaddr$prim55862, align 8
%clofunc55863 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55863(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48384)
ret void
}

define tailcc void @proc_clo$ae49629(%struct.ScmObj* %env$ae49629,%struct.ScmObj* %fargs4807648385) {
%stackaddr$env-ref55864 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55864
%stackaddr$env-ref55865 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55865
%stackaddr$env-ref55866 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49629, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55866
%stackaddr$prim55867 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648385)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim55867, align 8
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648385)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$makeclosure55869 = alloca %struct.ScmObj*, align 8
%fptrToInt55870 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49633 to i64
%ae49633 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55870)
store volatile %struct.ScmObj* %ae49633, %struct.ScmObj** %stackaddr$makeclosure55869, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %k48386, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %fargs48076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49633, %struct.ScmObj* %f48075, i64 3)
%ae49635 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54520$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%argslist54520$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49635, %struct.ScmObj* %argslist54520$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist54520$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim55871, align 8
%stackaddr$prim55872 = alloca %struct.ScmObj*, align 8
%argslist54520$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54520$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist54520$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim55872, align 8
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%argslist54520$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49633, %struct.ScmObj* %argslist54520$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist54520$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim55873, align 8
%clofunc55874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc55874(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist54520$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49633(%struct.ScmObj* %env$ae49633,%struct.ScmObj* %current_45args54509) {
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 1)
store %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$env-ref55877 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55877
%stackaddr$env-ref55878 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49633, i64 3)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55878
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%current_45args54510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54509)
store volatile %struct.ScmObj* %current_45args54510, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$makeclosure55882 = alloca %struct.ScmObj*, align 8
%fptrToInt55883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49640 to i64
%ae49640 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55883)
store volatile %struct.ScmObj* %ae49640, %struct.ScmObj** %stackaddr$makeclosure55882, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %k48386, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49640, %struct.ScmObj* %fargs48076, i64 2)
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%cpsargs48391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49640, %struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %cpsargs48391, %struct.ScmObj** %stackaddr$prim55884, align 8
%clofunc55885 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc55885(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48391)
ret void
}

define tailcc void @proc_clo$ae49640(%struct.ScmObj* %env$ae49640,%struct.ScmObj* %current_45args54512) {
%stackaddr$env-ref55886 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55886
%stackaddr$env-ref55887 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 1)
store %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$env-ref55887
%stackaddr$env-ref55888 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49640, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55888
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54512)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%current_45args54513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54512)
store volatile %struct.ScmObj* %current_45args54513, %struct.ScmObj** %stackaddr$prim55890, align 8
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54513)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim55891, align 8
%stackaddr$makeclosure55892 = alloca %struct.ScmObj*, align 8
%fptrToInt55893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49645 to i64
%ae49645 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55893)
store volatile %struct.ScmObj* %ae49645, %struct.ScmObj** %stackaddr$makeclosure55892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49645, %struct.ScmObj* %anf_45bind48201, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49645, %struct.ScmObj* %k48386, i64 1)
%argslist54519$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%argslist54519$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54519$_37last480680)
store volatile %struct.ScmObj* %argslist54519$_37last480681, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%argslist54519$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49645, %struct.ScmObj* %argslist54519$_37last480681)
store volatile %struct.ScmObj* %argslist54519$_37last480682, %struct.ScmObj** %stackaddr$prim55895, align 8
%clofunc55896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc55896(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist54519$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49645(%struct.ScmObj* %env$ae49645,%struct.ScmObj* %current_45args54515) {
%stackaddr$env-ref55897 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49645, i64 0)
store %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$env-ref55897
%stackaddr$env-ref55898 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49645, i64 1)
store %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$env-ref55898
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54515)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%current_45args54516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54515)
store volatile %struct.ScmObj* %current_45args54516, %struct.ScmObj** %stackaddr$prim55900, align 8
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%cpsprim48390 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %anf_45bind48202)
store volatile %struct.ScmObj* %cpsprim48390, %struct.ScmObj** %stackaddr$prim55902, align 8
%ae49650 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54518$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%argslist54518$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48390, %struct.ScmObj* %argslist54518$k483860)
store volatile %struct.ScmObj* %argslist54518$k483861, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%argslist54518$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49650, %struct.ScmObj* %argslist54518$k483861)
store volatile %struct.ScmObj* %argslist54518$k483862, %struct.ScmObj** %stackaddr$prim55904, align 8
%clofunc55905 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc55905(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist54518$k483862)
ret void
}

define tailcc void @proc_clo$ae49545(%struct.ScmObj* %env$ae49545,%struct.ScmObj* %current_45args54523) {
%stackaddr$env-ref55906 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49545, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55906
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%current_45args54524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54523)
store volatile %struct.ScmObj* %current_45args54524, %struct.ScmObj** %stackaddr$prim55908, align 8
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54524)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%current_45args54525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54524)
store volatile %struct.ScmObj* %current_45args54525, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54525)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$makeclosure55912 = alloca %struct.ScmObj*, align 8
%fptrToInt55913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49546 to i64
%ae49546 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55913)
store volatile %struct.ScmObj* %ae49546, %struct.ScmObj** %stackaddr$makeclosure55912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49546, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49546, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49546, %struct.ScmObj* %k48392, i64 2)
%ae49547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55914 = alloca %struct.ScmObj*, align 8
%fptrToInt55915 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49548 to i64
%ae49548 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55915)
store volatile %struct.ScmObj* %ae49548, %struct.ScmObj** %stackaddr$makeclosure55914, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49548, %struct.ScmObj* %f48079, i64 0)
%argslist54540$ae495460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%argslist54540$ae495461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49548, %struct.ScmObj* %argslist54540$ae495460)
store volatile %struct.ScmObj* %argslist54540$ae495461, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%argslist54540$ae495462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49547, %struct.ScmObj* %argslist54540$ae495461)
store volatile %struct.ScmObj* %argslist54540$ae495462, %struct.ScmObj** %stackaddr$prim55917, align 8
%clofunc55918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49546)
musttail call tailcc void %clofunc55918(%struct.ScmObj* %ae49546, %struct.ScmObj* %argslist54540$ae495462)
ret void
}

define tailcc void @proc_clo$ae49546(%struct.ScmObj* %env$ae49546,%struct.ScmObj* %current_45args54527) {
%stackaddr$env-ref55919 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49546, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref55919
%stackaddr$env-ref55920 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49546, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55920
%stackaddr$env-ref55921 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49546, i64 2)
store %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$env-ref55921
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54527)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim55922, align 8
%stackaddr$prim55923 = alloca %struct.ScmObj*, align 8
%current_45args54528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54527)
store volatile %struct.ScmObj* %current_45args54528, %struct.ScmObj** %stackaddr$prim55923, align 8
%stackaddr$prim55924 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim55924, align 8
%ae49580 = call %struct.ScmObj* @const_init_null()
%argslist54530$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%argslist54530$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist54530$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54530$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%argslist54530$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49580, %struct.ScmObj* %argslist54530$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54530$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%argslist54530$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %argslist54530$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54530$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%argslist54530$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist54530$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54530$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55928, align 8
%clofunc55929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55929(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54530$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49548(%struct.ScmObj* %env$ae49548,%struct.ScmObj* %current_45args54531) {
%stackaddr$env-ref55930 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49548, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref55930
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54531)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%current_45args54532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54531)
store volatile %struct.ScmObj* %current_45args54532, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54532)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim55933, align 8
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%current_45args54533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54532)
store volatile %struct.ScmObj* %current_45args54533, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54533)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$makeclosure55936 = alloca %struct.ScmObj*, align 8
%fptrToInt55937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49550 to i64
%ae49550 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55937)
store volatile %struct.ScmObj* %ae49550, %struct.ScmObj** %stackaddr$makeclosure55936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %r48080, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49550, %struct.ScmObj* %k48394, i64 1)
%argslist54539$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%argslist54539$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist54539$f480790)
store volatile %struct.ScmObj* %argslist54539$f480791, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$prim55939 = alloca %struct.ScmObj*, align 8
%argslist54539$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49550, %struct.ScmObj* %argslist54539$f480791)
store volatile %struct.ScmObj* %argslist54539$f480792, %struct.ScmObj** %stackaddr$prim55939, align 8
%clofunc55940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc55940(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist54539$f480792)
ret void
}

define tailcc void @proc_clo$ae49550(%struct.ScmObj* %env$ae49550,%struct.ScmObj* %current_45args54535) {
%stackaddr$env-ref55941 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 0)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref55941
%stackaddr$env-ref55942 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49550, i64 1)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref55942
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54535)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%current_45args54536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54535)
store volatile %struct.ScmObj* %current_45args54536, %struct.ScmObj** %stackaddr$prim55944, align 8
%stackaddr$prim55945 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54536)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55945, align 8
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%cpsprim48396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48396, %struct.ScmObj** %stackaddr$prim55946, align 8
%ae49555 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54538$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%argslist54538$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48396, %struct.ScmObj* %argslist54538$k483940)
store volatile %struct.ScmObj* %argslist54538$k483941, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%argslist54538$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49555, %struct.ScmObj* %argslist54538$k483941)
store volatile %struct.ScmObj* %argslist54538$k483942, %struct.ScmObj** %stackaddr$prim55948, align 8
%clofunc55949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc55949(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist54538$k483942)
ret void
}

define tailcc void @proc_clo$ae49159(%struct.ScmObj* %env$ae49159,%struct.ScmObj* %current_45args54543) {
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49159, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49159, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%current_45args54544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %current_45args54544, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54544)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim55954, align 8
%ae49161 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55955 = alloca %struct.ScmObj*, align 8
%fptrToInt55956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49162 to i64
%ae49162 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55956)
store volatile %struct.ScmObj* %ae49162, %struct.ScmObj** %stackaddr$makeclosure55955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49162, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49162, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49162, %struct.ScmObj* %_37map148042, i64 2)
%argslist54601$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%argslist54601$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49162, %struct.ScmObj* %argslist54601$k483970)
store volatile %struct.ScmObj* %argslist54601$k483971, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%argslist54601$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49161, %struct.ScmObj* %argslist54601$k483971)
store volatile %struct.ScmObj* %argslist54601$k483972, %struct.ScmObj** %stackaddr$prim55958, align 8
%clofunc55959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc55959(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist54601$k483972)
ret void
}

define tailcc void @proc_clo$ae49162(%struct.ScmObj* %env$ae49162,%struct.ScmObj* %args4805348398) {
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49162, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49162, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49162, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$prim55963 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348398)
store volatile %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$prim55963, align 8
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348398)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48185)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48186)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$makeclosure55970 = alloca %struct.ScmObj*, align 8
%fptrToInt55971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49170 to i64
%ae49170 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55971)
store volatile %struct.ScmObj* %ae49170, %struct.ScmObj** %stackaddr$makeclosure55970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %k48399, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37map148042, i64 6)
%ae49171 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55972 = alloca %struct.ScmObj*, align 8
%fptrToInt55973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49172 to i64
%ae49172 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55973)
store volatile %struct.ScmObj* %ae49172, %struct.ScmObj** %stackaddr$makeclosure55972, align 8
%argslist54600$ae491700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%argslist54600$ae491701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49172, %struct.ScmObj* %argslist54600$ae491700)
store volatile %struct.ScmObj* %argslist54600$ae491701, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%argslist54600$ae491702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49171, %struct.ScmObj* %argslist54600$ae491701)
store volatile %struct.ScmObj* %argslist54600$ae491702, %struct.ScmObj** %stackaddr$prim55975, align 8
%clofunc55976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49170)
musttail call tailcc void %clofunc55976(%struct.ScmObj* %ae49170, %struct.ScmObj* %argslist54600$ae491702)
ret void
}

define tailcc void @proc_clo$ae49170(%struct.ScmObj* %env$ae49170,%struct.ScmObj* %current_45args54546) {
%stackaddr$env-ref55977 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55977
%stackaddr$env-ref55978 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55978
%stackaddr$env-ref55979 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55979
%stackaddr$env-ref55980 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55980
%stackaddr$env-ref55981 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 4)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref55981
%stackaddr$env-ref55982 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55982
%stackaddr$env-ref55983 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55983
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%current_45args54547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %current_45args54547, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54547)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$makeclosure55987 = alloca %struct.ScmObj*, align 8
%fptrToInt55988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49202 to i64
%ae49202 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55988)
store volatile %struct.ScmObj* %ae49202, %struct.ScmObj** %stackaddr$makeclosure55987, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %k48399, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49202, %struct.ScmObj* %_37map148042, i64 6)
%ae49204 = call %struct.ScmObj* @const_init_false()
%argslist54593$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%argslist54593$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54593$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54593$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%argslist54593$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist54593$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54593$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%argslist54593$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist54593$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54593$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55991, align 8
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%argslist54593$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49202, %struct.ScmObj* %argslist54593$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54593$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55992, align 8
%clofunc55993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55993(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54593$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49202(%struct.ScmObj* %env$ae49202,%struct.ScmObj* %current_45args54549) {
%stackaddr$env-ref55994 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55994
%stackaddr$env-ref55995 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55995
%stackaddr$env-ref55996 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55996
%stackaddr$env-ref55997 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55997
%stackaddr$env-ref55998 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 4)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref55998
%stackaddr$env-ref55999 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55999
%stackaddr$env-ref56000 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49202, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56000
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54549)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim56001, align 8
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%current_45args54550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54549)
store volatile %struct.ScmObj* %current_45args54550, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54550)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim56003, align 8
%truthy$cmp56004 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48188)
%cmp$cmp56004 = icmp eq i64 %truthy$cmp56004, 1
br i1 %cmp$cmp56004, label %truebranch$cmp56004, label %falsebranch$cmp56004
truebranch$cmp56004:
%ae49213 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54552$k483990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54552$k483991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist54552$k483990)
store volatile %struct.ScmObj* %argslist54552$k483991, %struct.ScmObj** %stackaddr$prim56005, align 8
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%argslist54552$k483992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist54552$k483991)
store volatile %struct.ScmObj* %argslist54552$k483992, %struct.ScmObj** %stackaddr$prim56006, align 8
%clofunc56007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48399)
musttail call tailcc void %clofunc56007(%struct.ScmObj* %k48399, %struct.ScmObj* %argslist54552$k483992)
ret void
falsebranch$cmp56004:
%stackaddr$makeclosure56008 = alloca %struct.ScmObj*, align 8
%fptrToInt56009 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49218 to i64
%ae49218 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56009)
store volatile %struct.ScmObj* %ae49218, %struct.ScmObj** %stackaddr$makeclosure56008, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %k48399, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49218, %struct.ScmObj* %_37map148042, i64 6)
%ae49219 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56010 = alloca %struct.ScmObj*, align 8
%fptrToInt56011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49220 to i64
%ae49220 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56011)
store volatile %struct.ScmObj* %ae49220, %struct.ScmObj** %stackaddr$makeclosure56010, align 8
%argslist54592$ae492180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%argslist54592$ae492181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49220, %struct.ScmObj* %argslist54592$ae492180)
store volatile %struct.ScmObj* %argslist54592$ae492181, %struct.ScmObj** %stackaddr$prim56012, align 8
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%argslist54592$ae492182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49219, %struct.ScmObj* %argslist54592$ae492181)
store volatile %struct.ScmObj* %argslist54592$ae492182, %struct.ScmObj** %stackaddr$prim56013, align 8
%clofunc56014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49218)
musttail call tailcc void %clofunc56014(%struct.ScmObj* %ae49218, %struct.ScmObj* %argslist54592$ae492182)
ret void
}

define tailcc void @proc_clo$ae49218(%struct.ScmObj* %env$ae49218,%struct.ScmObj* %current_45args54553) {
%stackaddr$env-ref56015 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56015
%stackaddr$env-ref56016 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56016
%stackaddr$env-ref56017 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56017
%stackaddr$env-ref56018 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56018
%stackaddr$env-ref56019 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 4)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56019
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49218, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54553)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim56022, align 8
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%current_45args54554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54553)
store volatile %struct.ScmObj* %current_45args54554, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54554)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$makeclosure56025 = alloca %struct.ScmObj*, align 8
%fptrToInt56026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56026)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure56025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %k48399, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37map148042, i64 6)
%argslist54587$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56027 = alloca %struct.ScmObj*, align 8
%argslist54587$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54587$_37map1480420)
store volatile %struct.ScmObj* %argslist54587$_37map1480421, %struct.ScmObj** %stackaddr$prim56027, align 8
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%argslist54587$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist54587$_37map1480421)
store volatile %struct.ScmObj* %argslist54587$_37map1480422, %struct.ScmObj** %stackaddr$prim56028, align 8
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%argslist54587$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist54587$_37map1480422)
store volatile %struct.ScmObj* %argslist54587$_37map1480423, %struct.ScmObj** %stackaddr$prim56029, align 8
%clofunc56030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56030(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54587$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args54556) {
%stackaddr$env-ref56031 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56031
%stackaddr$env-ref56032 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56032
%stackaddr$env-ref56033 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56033
%stackaddr$env-ref56034 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56034
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 4)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$env-ref56036 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56036
%stackaddr$env-ref56037 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56037
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54556)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%current_45args54557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54556)
store volatile %struct.ScmObj* %current_45args54557, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54557)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$makeclosure56041 = alloca %struct.ScmObj*, align 8
%fptrToInt56042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49242 to i64
%ae49242 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56042)
store volatile %struct.ScmObj* %ae49242, %struct.ScmObj** %stackaddr$makeclosure56041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %k48399, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %lsts_4348061, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37map148042, i64 7)
%ae49243 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56043 = alloca %struct.ScmObj*, align 8
%fptrToInt56044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49244 to i64
%ae49244 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56044)
store volatile %struct.ScmObj* %ae49244, %struct.ScmObj** %stackaddr$makeclosure56043, align 8
%argslist54586$ae492420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist54586$ae492421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49244, %struct.ScmObj* %argslist54586$ae492420)
store volatile %struct.ScmObj* %argslist54586$ae492421, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%argslist54586$ae492422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49243, %struct.ScmObj* %argslist54586$ae492421)
store volatile %struct.ScmObj* %argslist54586$ae492422, %struct.ScmObj** %stackaddr$prim56046, align 8
%clofunc56047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49242)
musttail call tailcc void %clofunc56047(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist54586$ae492422)
ret void
}

define tailcc void @proc_clo$ae49242(%struct.ScmObj* %env$ae49242,%struct.ScmObj* %current_45args54559) {
%stackaddr$env-ref56048 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56048
%stackaddr$env-ref56049 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56049
%stackaddr$env-ref56050 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56050
%stackaddr$env-ref56051 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56051
%stackaddr$env-ref56052 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 4)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56052
%stackaddr$env-ref56053 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56053
%stackaddr$env-ref56054 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 6)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56054
%stackaddr$env-ref56055 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 7)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56055
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54559)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%current_45args54560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54559)
store volatile %struct.ScmObj* %current_45args54560, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$makeclosure56059 = alloca %struct.ScmObj*, align 8
%fptrToInt56060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49263 to i64
%ae49263 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56060)
store volatile %struct.ScmObj* %ae49263, %struct.ScmObj** %stackaddr$makeclosure56059, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %k48399, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %lsts_4348061, i64 5)
%argslist54581$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%argslist54581$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54581$_37map1480420)
store volatile %struct.ScmObj* %argslist54581$_37map1480421, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54581$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist54581$_37map1480421)
store volatile %struct.ScmObj* %argslist54581$_37map1480422, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%argslist54581$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49263, %struct.ScmObj* %argslist54581$_37map1480422)
store volatile %struct.ScmObj* %argslist54581$_37map1480423, %struct.ScmObj** %stackaddr$prim56063, align 8
%clofunc56064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56064(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54581$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49263(%struct.ScmObj* %env$ae49263,%struct.ScmObj* %current_45args54562) {
%stackaddr$env-ref56065 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56065
%stackaddr$env-ref56066 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56066
%stackaddr$env-ref56067 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56067
%stackaddr$env-ref56068 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 3)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56068
%stackaddr$env-ref56069 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56069
%stackaddr$env-ref56070 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56070
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%_95k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54562)
store volatile %struct.ScmObj* %_95k48405, %struct.ScmObj** %stackaddr$prim56071, align 8
%stackaddr$prim56072 = alloca %struct.ScmObj*, align 8
%current_45args54563 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54562)
store volatile %struct.ScmObj* %current_45args54563, %struct.ScmObj** %stackaddr$prim56072, align 8
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54563)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$makeclosure56074 = alloca %struct.ScmObj*, align 8
%fptrToInt56075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49266 to i64
%ae49266 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56075)
store volatile %struct.ScmObj* %ae49266, %struct.ScmObj** %stackaddr$makeclosure56074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %k48399, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %vs48059, i64 6)
%ae49267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56076 = alloca %struct.ScmObj*, align 8
%fptrToInt56077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49268 to i64
%ae49268 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56077)
store volatile %struct.ScmObj* %ae49268, %struct.ScmObj** %stackaddr$makeclosure56076, align 8
%argslist54580$ae492660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%argslist54580$ae492661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist54580$ae492660)
store volatile %struct.ScmObj* %argslist54580$ae492661, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist54580$ae492662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49267, %struct.ScmObj* %argslist54580$ae492661)
store volatile %struct.ScmObj* %argslist54580$ae492662, %struct.ScmObj** %stackaddr$prim56079, align 8
%clofunc56080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49266)
musttail call tailcc void %clofunc56080(%struct.ScmObj* %ae49266, %struct.ScmObj* %argslist54580$ae492662)
ret void
}

define tailcc void @proc_clo$ae49266(%struct.ScmObj* %env$ae49266,%struct.ScmObj* %current_45args54565) {
%stackaddr$env-ref56081 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56081
%stackaddr$env-ref56082 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56082
%stackaddr$env-ref56083 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56083
%stackaddr$env-ref56084 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 3)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56084
%stackaddr$env-ref56085 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56085
%stackaddr$env-ref56086 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56086
%stackaddr$env-ref56087 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 6)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56087
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54565)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%current_45args54566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54565)
store volatile %struct.ScmObj* %current_45args54566, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54566)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim56090, align 8
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim56091, align 8
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48192)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56092, align 8
%stackaddr$makeclosure56093 = alloca %struct.ScmObj*, align 8
%fptrToInt56094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49292 to i64
%ae49292 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56094)
store volatile %struct.ScmObj* %ae49292, %struct.ScmObj** %stackaddr$makeclosure56093, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %k48399, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %anf_45bind48191, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %f48056, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %vs48059, i64 4)
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%cpsargs48410 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49292, %struct.ScmObj* %anf_45bind48193)
store volatile %struct.ScmObj* %cpsargs48410, %struct.ScmObj** %stackaddr$prim56095, align 8
%clofunc56096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56096(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48410)
ret void
}

define tailcc void @proc_clo$ae49292(%struct.ScmObj* %env$ae49292,%struct.ScmObj* %current_45args54568) {
%stackaddr$env-ref56097 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 0)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56097
%stackaddr$env-ref56098 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 1)
store %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$env-ref56098
%stackaddr$env-ref56099 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 2)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56099
%stackaddr$env-ref56100 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56100
%stackaddr$env-ref56101 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 4)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56101
%stackaddr$prim56102 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54568)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim56102, align 8
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%current_45args54569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54568)
store volatile %struct.ScmObj* %current_45args54569, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54569)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim56104, align 8
%ae49297 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %ae49297)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$makeclosure56106 = alloca %struct.ScmObj*, align 8
%fptrToInt56107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49299 to i64
%ae49299 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56107)
store volatile %struct.ScmObj* %ae49299, %struct.ScmObj** %stackaddr$makeclosure56106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %k48399, i64 1)
%argslist54574$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%argslist54574$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist54574$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54574$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56108, align 8
%stackaddr$prim56109 = alloca %struct.ScmObj*, align 8
%argslist54574$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist54574$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54574$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56109, align 8
%stackaddr$prim56110 = alloca %struct.ScmObj*, align 8
%argslist54574$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist54574$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54574$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56110, align 8
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%argslist54574$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49299, %struct.ScmObj* %argslist54574$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54574$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56111, align 8
%clofunc56112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56112(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54574$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49299(%struct.ScmObj* %env$ae49299,%struct.ScmObj* %current_45args54571) {
%stackaddr$env-ref56113 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56113
%stackaddr$env-ref56114 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 1)
store %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$env-ref56114
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%_95k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54571)
store volatile %struct.ScmObj* %_95k48408, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%current_45args54572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54571)
store volatile %struct.ScmObj* %current_45args54572, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim56117, align 8
%stackaddr$prim56118 = alloca %struct.ScmObj*, align 8
%cpsargs48409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48399, %struct.ScmObj* %anf_45bind48196)
store volatile %struct.ScmObj* %cpsargs48409, %struct.ScmObj** %stackaddr$prim56118, align 8
%clofunc56119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc56119(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48409)
ret void
}

define tailcc void @proc_clo$ae49268(%struct.ScmObj* %env$ae49268,%struct.ScmObj* %current_45args54575) {
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54575)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim56120, align 8
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%current_45args54576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54575)
store volatile %struct.ScmObj* %current_45args54576, %struct.ScmObj** %stackaddr$prim56121, align 8
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54576)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56122, align 8
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%current_45args54577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54576)
store volatile %struct.ScmObj* %current_45args54577, %struct.ScmObj** %stackaddr$prim56123, align 8
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54577)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56124, align 8
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%cpsprim48412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48412, %struct.ScmObj** %stackaddr$prim56125, align 8
%ae49272 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54579$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%argslist54579$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48412, %struct.ScmObj* %argslist54579$k484110)
store volatile %struct.ScmObj* %argslist54579$k484111, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%argslist54579$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist54579$k484111)
store volatile %struct.ScmObj* %argslist54579$k484112, %struct.ScmObj** %stackaddr$prim56127, align 8
%clofunc56128 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc56128(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist54579$k484112)
ret void
}

define tailcc void @proc_clo$ae49244(%struct.ScmObj* %env$ae49244,%struct.ScmObj* %current_45args54582) {
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54582)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim56129, align 8
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%current_45args54583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54582)
store volatile %struct.ScmObj* %current_45args54583, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54583)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56131, align 8
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%cpsprim48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48414, %struct.ScmObj** %stackaddr$prim56132, align 8
%ae49247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54585$k484130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%argslist54585$k484131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48414, %struct.ScmObj* %argslist54585$k484130)
store volatile %struct.ScmObj* %argslist54585$k484131, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%argslist54585$k484132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist54585$k484131)
store volatile %struct.ScmObj* %argslist54585$k484132, %struct.ScmObj** %stackaddr$prim56134, align 8
%clofunc56135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48413)
musttail call tailcc void %clofunc56135(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist54585$k484132)
ret void
}

define tailcc void @proc_clo$ae49220(%struct.ScmObj* %env$ae49220,%struct.ScmObj* %current_45args54588) {
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54588)
store volatile %struct.ScmObj* %k48415, %struct.ScmObj** %stackaddr$prim56136, align 8
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%current_45args54589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54588)
store volatile %struct.ScmObj* %current_45args54589, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54589)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56138, align 8
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%cpsprim48416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48416, %struct.ScmObj** %stackaddr$prim56139, align 8
%ae49223 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54591$k484150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%argslist54591$k484151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48416, %struct.ScmObj* %argslist54591$k484150)
store volatile %struct.ScmObj* %argslist54591$k484151, %struct.ScmObj** %stackaddr$prim56140, align 8
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%argslist54591$k484152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49223, %struct.ScmObj* %argslist54591$k484151)
store volatile %struct.ScmObj* %argslist54591$k484152, %struct.ScmObj** %stackaddr$prim56141, align 8
%clofunc56142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48415)
musttail call tailcc void %clofunc56142(%struct.ScmObj* %k48415, %struct.ScmObj* %argslist54591$k484152)
ret void
}

define tailcc void @proc_clo$ae49172(%struct.ScmObj* %env$ae49172,%struct.ScmObj* %current_45args54594) {
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54594)
store volatile %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%current_45args54595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54594)
store volatile %struct.ScmObj* %current_45args54595, %struct.ScmObj** %stackaddr$prim56144, align 8
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54595)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56145, align 8
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%current_45args54596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54595)
store volatile %struct.ScmObj* %current_45args54596, %struct.ScmObj** %stackaddr$prim56146, align 8
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54596)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56147, align 8
%truthy$cmp56148 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56148 = icmp eq i64 %truthy$cmp56148, 1
br i1 %cmp$cmp56148, label %truebranch$cmp56148, label %falsebranch$cmp56148
truebranch$cmp56148:
%ae49175 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54598$k484170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%argslist54598$k484171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist54598$k484170)
store volatile %struct.ScmObj* %argslist54598$k484171, %struct.ScmObj** %stackaddr$prim56149, align 8
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%argslist54598$k484172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49175, %struct.ScmObj* %argslist54598$k484171)
store volatile %struct.ScmObj* %argslist54598$k484172, %struct.ScmObj** %stackaddr$prim56150, align 8
%clofunc56151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48417)
musttail call tailcc void %clofunc56151(%struct.ScmObj* %k48417, %struct.ScmObj* %argslist54598$k484172)
ret void
falsebranch$cmp56148:
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%cpsprim48418 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48418, %struct.ScmObj** %stackaddr$prim56152, align 8
%ae49182 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54599$k484170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%argslist54599$k484171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48418, %struct.ScmObj* %argslist54599$k484170)
store volatile %struct.ScmObj* %argslist54599$k484171, %struct.ScmObj** %stackaddr$prim56153, align 8
%stackaddr$prim56154 = alloca %struct.ScmObj*, align 8
%argslist54599$k484172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49182, %struct.ScmObj* %argslist54599$k484171)
store volatile %struct.ScmObj* %argslist54599$k484172, %struct.ScmObj** %stackaddr$prim56154, align 8
%clofunc56155 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48417)
musttail call tailcc void %clofunc56155(%struct.ScmObj* %k48417, %struct.ScmObj* %argslist54599$k484172)
ret void
}

define tailcc void @proc_clo$ae49129(%struct.ScmObj* %env$ae49129,%struct.ScmObj* %current_45args54603) {
%stackaddr$env-ref56156 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49129, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56156
%stackaddr$env-ref56157 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49129, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56157
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%current_45args54604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %current_45args54604, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56160, align 8
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%current_45args54605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %current_45args54605, %struct.ScmObj** %stackaddr$prim56161, align 8
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54605)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$makeclosure56163 = alloca %struct.ScmObj*, align 8
%fptrToInt56164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49131 to i64
%ae49131 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56164)
store volatile %struct.ScmObj* %ae49131, %struct.ScmObj** %stackaddr$makeclosure56163, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %lst48067, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %k48419, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %_37take48038, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %n48066, i64 3)
%argslist54611$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%argslist54611$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54611$_37length480350)
store volatile %struct.ScmObj* %argslist54611$_37length480351, %struct.ScmObj** %stackaddr$prim56165, align 8
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%argslist54611$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49131, %struct.ScmObj* %argslist54611$_37length480351)
store volatile %struct.ScmObj* %argslist54611$_37length480352, %struct.ScmObj** %stackaddr$prim56166, align 8
%clofunc56167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56167(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist54611$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49131(%struct.ScmObj* %env$ae49131,%struct.ScmObj* %current_45args54607) {
%stackaddr$env-ref56168 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 0)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56168
%stackaddr$env-ref56169 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 1)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref56169
%stackaddr$env-ref56170 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 2)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56170
%stackaddr$env-ref56171 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 3)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56171
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%_95k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54607)
store volatile %struct.ScmObj* %_95k48420, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%current_45args54608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54607)
store volatile %struct.ScmObj* %current_45args54608, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54608)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim56174, align 8
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim56175, align 8
%argslist54610$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%argslist54610$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist54610$_37take480380)
store volatile %struct.ScmObj* %argslist54610$_37take480381, %struct.ScmObj** %stackaddr$prim56176, align 8
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%argslist54610$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54610$_37take480381)
store volatile %struct.ScmObj* %argslist54610$_37take480382, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%argslist54610$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist54610$_37take480382)
store volatile %struct.ScmObj* %argslist54610$_37take480383, %struct.ScmObj** %stackaddr$prim56178, align 8
%clofunc56179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc56179(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist54610$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49075(%struct.ScmObj* %env$ae49075,%struct.ScmObj* %current_45args54613) {
%stackaddr$env-ref56180 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49075, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56180
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54613)
store volatile %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$prim56181, align 8
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%current_45args54614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54613)
store volatile %struct.ScmObj* %current_45args54614, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54614)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$makeclosure56184 = alloca %struct.ScmObj*, align 8
%fptrToInt56185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49076 to i64
%ae49076 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56185)
store volatile %struct.ScmObj* %ae49076, %struct.ScmObj** %stackaddr$makeclosure56184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49076, %struct.ScmObj* %lst48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49076, %struct.ScmObj* %k48421, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49076, %struct.ScmObj* %_37foldl148030, i64 2)
%ae49077 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56186 = alloca %struct.ScmObj*, align 8
%fptrToInt56187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49078 to i64
%ae49078 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56187)
store volatile %struct.ScmObj* %ae49078, %struct.ScmObj** %stackaddr$makeclosure56186, align 8
%argslist54625$ae490760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%argslist54625$ae490761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49078, %struct.ScmObj* %argslist54625$ae490760)
store volatile %struct.ScmObj* %argslist54625$ae490761, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%argslist54625$ae490762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49077, %struct.ScmObj* %argslist54625$ae490761)
store volatile %struct.ScmObj* %argslist54625$ae490762, %struct.ScmObj** %stackaddr$prim56189, align 8
%clofunc56190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49076)
musttail call tailcc void %clofunc56190(%struct.ScmObj* %ae49076, %struct.ScmObj* %argslist54625$ae490762)
ret void
}

define tailcc void @proc_clo$ae49076(%struct.ScmObj* %env$ae49076,%struct.ScmObj* %current_45args54616) {
%stackaddr$env-ref56191 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49076, i64 0)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref56191
%stackaddr$env-ref56192 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49076, i64 1)
store %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$env-ref56192
%stackaddr$env-ref56193 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49076, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56193
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54616)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%current_45args54617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54616)
store volatile %struct.ScmObj* %current_45args54617, %struct.ScmObj** %stackaddr$prim56195, align 8
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54617)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim56196, align 8
%ae49097 = call %struct.ScmObj* @const_init_null()
%argslist54619$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%argslist54619$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist54619$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54619$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%argslist54619$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49097, %struct.ScmObj* %argslist54619$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54619$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56198, align 8
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%argslist54619$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist54619$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54619$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist54619$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist54619$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54619$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56200, align 8
%clofunc56201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56201(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54619$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49078(%struct.ScmObj* %env$ae49078,%struct.ScmObj* %current_45args54620) {
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$prim56202, align 8
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%current_45args54621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %current_45args54621, %struct.ScmObj** %stackaddr$prim56203, align 8
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim56204, align 8
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%current_45args54622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %current_45args54622, %struct.ScmObj** %stackaddr$prim56205, align 8
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54622)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim56206, align 8
%ae49080 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54624$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%argslist54624$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist54624$k484230)
store volatile %struct.ScmObj* %argslist54624$k484231, %struct.ScmObj** %stackaddr$prim56207, align 8
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%argslist54624$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49080, %struct.ScmObj* %argslist54624$k484231)
store volatile %struct.ScmObj* %argslist54624$k484232, %struct.ScmObj** %stackaddr$prim56208, align 8
%clofunc56209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc56209(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist54624$k484232)
ret void
}

define tailcc void @proc_clo$ae48996(%struct.ScmObj* %env$ae48996,%struct.ScmObj* %current_45args54628) {
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%current_45args54629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %current_45args54629, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54629)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim56212, align 8
%ae48998 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56213 = alloca %struct.ScmObj*, align 8
%fptrToInt56214 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48999 to i64
%ae48999 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56214)
store volatile %struct.ScmObj* %ae48999, %struct.ScmObj** %stackaddr$makeclosure56213, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48999, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54642$k484240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%argslist54642$k484241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48999, %struct.ScmObj* %argslist54642$k484240)
store volatile %struct.ScmObj* %argslist54642$k484241, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%argslist54642$k484242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48998, %struct.ScmObj* %argslist54642$k484241)
store volatile %struct.ScmObj* %argslist54642$k484242, %struct.ScmObj** %stackaddr$prim56216, align 8
%clofunc56217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48424)
musttail call tailcc void %clofunc56217(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist54642$k484242)
ret void
}

define tailcc void @proc_clo$ae48999(%struct.ScmObj* %env$ae48999,%struct.ScmObj* %current_45args54631) {
%stackaddr$env-ref56218 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48999, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56218
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54631)
store volatile %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%current_45args54632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54631)
store volatile %struct.ScmObj* %current_45args54632, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54632)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim56221, align 8
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%current_45args54633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54632)
store volatile %struct.ScmObj* %current_45args54633, %struct.ScmObj** %stackaddr$prim56222, align 8
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54633)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%current_45args54634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54633)
store volatile %struct.ScmObj* %current_45args54634, %struct.ScmObj** %stackaddr$prim56224, align 8
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54634)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim56226, align 8
%truthy$cmp56227 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48177)
%cmp$cmp56227 = icmp eq i64 %truthy$cmp56227, 1
br i1 %cmp$cmp56227, label %truebranch$cmp56227, label %falsebranch$cmp56227
truebranch$cmp56227:
%ae49003 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54636$k484250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%argslist54636$k484251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54636$k484250)
store volatile %struct.ScmObj* %argslist54636$k484251, %struct.ScmObj** %stackaddr$prim56228, align 8
%stackaddr$prim56229 = alloca %struct.ScmObj*, align 8
%argslist54636$k484252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49003, %struct.ScmObj* %argslist54636$k484251)
store volatile %struct.ScmObj* %argslist54636$k484252, %struct.ScmObj** %stackaddr$prim56229, align 8
%clofunc56230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48425)
musttail call tailcc void %clofunc56230(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist54636$k484252)
ret void
falsebranch$cmp56227:
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$makeclosure56232 = alloca %struct.ScmObj*, align 8
%fptrToInt56233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49010 to i64
%ae49010 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56233)
store volatile %struct.ScmObj* %ae49010, %struct.ScmObj** %stackaddr$makeclosure56232, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49010, %struct.ScmObj* %k48425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49010, %struct.ScmObj* %f48034, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49010, %struct.ScmObj* %lst48032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49010, %struct.ScmObj* %_37foldl148031, i64 3)
%argslist54641$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%argslist54641$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54641$f480340)
store volatile %struct.ScmObj* %argslist54641$f480341, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%argslist54641$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist54641$f480341)
store volatile %struct.ScmObj* %argslist54641$f480342, %struct.ScmObj** %stackaddr$prim56235, align 8
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%argslist54641$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49010, %struct.ScmObj* %argslist54641$f480342)
store volatile %struct.ScmObj* %argslist54641$f480343, %struct.ScmObj** %stackaddr$prim56236, align 8
%clofunc56237 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc56237(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54641$f480343)
ret void
}

define tailcc void @proc_clo$ae49010(%struct.ScmObj* %env$ae49010,%struct.ScmObj* %current_45args54637) {
%stackaddr$env-ref56238 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49010, i64 0)
store %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$env-ref56238
%stackaddr$env-ref56239 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49010, i64 1)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref56239
%stackaddr$env-ref56240 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49010, i64 2)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref56240
%stackaddr$env-ref56241 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49010, i64 3)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56241
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%_95k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54637)
store volatile %struct.ScmObj* %_95k48426, %struct.ScmObj** %stackaddr$prim56242, align 8
%stackaddr$prim56243 = alloca %struct.ScmObj*, align 8
%current_45args54638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54637)
store volatile %struct.ScmObj* %current_45args54638, %struct.ScmObj** %stackaddr$prim56243, align 8
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54638)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim56244, align 8
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim56245, align 8
%argslist54640$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist54640$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54640$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist54640$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54640$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54640$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54640$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%argslist54640$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist54640$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54640$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56249, align 8
%clofunc56250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56250(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54640$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48913(%struct.ScmObj* %env$ae48913,%struct.ScmObj* %current_45args54645) {
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54645)
store volatile %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%current_45args54646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54645)
store volatile %struct.ScmObj* %current_45args54646, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54646)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim56253, align 8
%ae48915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56254 = alloca %struct.ScmObj*, align 8
%fptrToInt56255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48916 to i64
%ae48916 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56255)
store volatile %struct.ScmObj* %ae48916, %struct.ScmObj** %stackaddr$makeclosure56254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %_37length48036, i64 0)
%argslist54657$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%argslist54657$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48916, %struct.ScmObj* %argslist54657$k484270)
store volatile %struct.ScmObj* %argslist54657$k484271, %struct.ScmObj** %stackaddr$prim56256, align 8
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%argslist54657$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48915, %struct.ScmObj* %argslist54657$k484271)
store volatile %struct.ScmObj* %argslist54657$k484272, %struct.ScmObj** %stackaddr$prim56257, align 8
%clofunc56258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc56258(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist54657$k484272)
ret void
}

define tailcc void @proc_clo$ae48916(%struct.ScmObj* %env$ae48916,%struct.ScmObj* %current_45args54648) {
%stackaddr$env-ref56259 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56259
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54648)
store volatile %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$prim56260, align 8
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%current_45args54649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54648)
store volatile %struct.ScmObj* %current_45args54649, %struct.ScmObj** %stackaddr$prim56261, align 8
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54649)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56263, align 8
%truthy$cmp56264 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48173)
%cmp$cmp56264 = icmp eq i64 %truthy$cmp56264, 1
br i1 %cmp$cmp56264, label %truebranch$cmp56264, label %falsebranch$cmp56264
truebranch$cmp56264:
%ae48920 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48921 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54651$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%argslist54651$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48921, %struct.ScmObj* %argslist54651$k484280)
store volatile %struct.ScmObj* %argslist54651$k484281, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$prim56266 = alloca %struct.ScmObj*, align 8
%argslist54651$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48920, %struct.ScmObj* %argslist54651$k484281)
store volatile %struct.ScmObj* %argslist54651$k484282, %struct.ScmObj** %stackaddr$prim56266, align 8
%clofunc56267 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc56267(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist54651$k484282)
ret void
falsebranch$cmp56264:
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim56268, align 8
%stackaddr$makeclosure56269 = alloca %struct.ScmObj*, align 8
%fptrToInt56270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48930 to i64
%ae48930 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56270)
store volatile %struct.ScmObj* %ae48930, %struct.ScmObj** %stackaddr$makeclosure56269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48930, %struct.ScmObj* %k48428, i64 0)
%argslist54656$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%argslist54656$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist54656$_37length480360)
store volatile %struct.ScmObj* %argslist54656$_37length480361, %struct.ScmObj** %stackaddr$prim56271, align 8
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%argslist54656$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48930, %struct.ScmObj* %argslist54656$_37length480361)
store volatile %struct.ScmObj* %argslist54656$_37length480362, %struct.ScmObj** %stackaddr$prim56272, align 8
%clofunc56273 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56273(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54656$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48930(%struct.ScmObj* %env$ae48930,%struct.ScmObj* %current_45args54652) {
%stackaddr$env-ref56274 = alloca %struct.ScmObj*, align 8
%k48428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48930, i64 0)
store %struct.ScmObj* %k48428, %struct.ScmObj** %stackaddr$env-ref56274
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54652)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%current_45args54653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54652)
store volatile %struct.ScmObj* %current_45args54653, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54653)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim56277, align 8
%ae48932 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%cpsprim48430 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48932, %struct.ScmObj* %anf_45bind48175)
store volatile %struct.ScmObj* %cpsprim48430, %struct.ScmObj** %stackaddr$prim56278, align 8
%ae48935 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54655$k484280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%argslist54655$k484281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48430, %struct.ScmObj* %argslist54655$k484280)
store volatile %struct.ScmObj* %argslist54655$k484281, %struct.ScmObj** %stackaddr$prim56279, align 8
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%argslist54655$k484282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48935, %struct.ScmObj* %argslist54655$k484281)
store volatile %struct.ScmObj* %argslist54655$k484282, %struct.ScmObj** %stackaddr$prim56280, align 8
%clofunc56281 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48428)
musttail call tailcc void %clofunc56281(%struct.ScmObj* %k48428, %struct.ScmObj* %argslist54655$k484282)
ret void
}

define tailcc void @proc_clo$ae48763(%struct.ScmObj* %env$ae48763,%struct.ScmObj* %current_45args54660) {
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54660)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%current_45args54661 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54660)
store volatile %struct.ScmObj* %current_45args54661, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54661)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim56284, align 8
%ae48765 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56285 = alloca %struct.ScmObj*, align 8
%fptrToInt56286 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48766 to i64
%ae48766 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56286)
store volatile %struct.ScmObj* %ae48766, %struct.ScmObj** %stackaddr$makeclosure56285, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48766, %struct.ScmObj* %_37take48039, i64 0)
%argslist54674$k484310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist54674$k484311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48766, %struct.ScmObj* %argslist54674$k484310)
store volatile %struct.ScmObj* %argslist54674$k484311, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%argslist54674$k484312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48765, %struct.ScmObj* %argslist54674$k484311)
store volatile %struct.ScmObj* %argslist54674$k484312, %struct.ScmObj** %stackaddr$prim56288, align 8
%clofunc56289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48431)
musttail call tailcc void %clofunc56289(%struct.ScmObj* %k48431, %struct.ScmObj* %argslist54674$k484312)
ret void
}

define tailcc void @proc_clo$ae48766(%struct.ScmObj* %env$ae48766,%struct.ScmObj* %current_45args54663) {
%stackaddr$env-ref56290 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48766, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56290
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54663)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%current_45args54664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54663)
store volatile %struct.ScmObj* %current_45args54664, %struct.ScmObj** %stackaddr$prim56292, align 8
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54664)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim56293, align 8
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%current_45args54665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54664)
store volatile %struct.ScmObj* %current_45args54665, %struct.ScmObj** %stackaddr$prim56294, align 8
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54665)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim56295, align 8
%ae48768 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48768)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim56296, align 8
%truthy$cmp56297 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48166)
%cmp$cmp56297 = icmp eq i64 %truthy$cmp56297, 1
br i1 %cmp$cmp56297, label %truebranch$cmp56297, label %falsebranch$cmp56297
truebranch$cmp56297:
%ae48771 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48772 = call %struct.ScmObj* @const_init_null()
%argslist54667$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%argslist54667$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48772, %struct.ScmObj* %argslist54667$k484320)
store volatile %struct.ScmObj* %argslist54667$k484321, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%argslist54667$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48771, %struct.ScmObj* %argslist54667$k484321)
store volatile %struct.ScmObj* %argslist54667$k484322, %struct.ScmObj** %stackaddr$prim56299, align 8
%clofunc56300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56300(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54667$k484322)
ret void
falsebranch$cmp56297:
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56301, align 8
%truthy$cmp56302 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48167)
%cmp$cmp56302 = icmp eq i64 %truthy$cmp56302, 1
br i1 %cmp$cmp56302, label %truebranch$cmp56302, label %falsebranch$cmp56302
truebranch$cmp56302:
%ae48782 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48783 = call %struct.ScmObj* @const_init_null()
%argslist54668$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56303 = alloca %struct.ScmObj*, align 8
%argslist54668$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist54668$k484320)
store volatile %struct.ScmObj* %argslist54668$k484321, %struct.ScmObj** %stackaddr$prim56303, align 8
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%argslist54668$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48782, %struct.ScmObj* %argslist54668$k484321)
store volatile %struct.ScmObj* %argslist54668$k484322, %struct.ScmObj** %stackaddr$prim56304, align 8
%clofunc56305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56305(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54668$k484322)
ret void
falsebranch$cmp56302:
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56306, align 8
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim56307, align 8
%ae48793 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48793)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$makeclosure56309 = alloca %struct.ScmObj*, align 8
%fptrToInt56310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48795 to i64
%ae48795 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56310)
store volatile %struct.ScmObj* %ae48795, %struct.ScmObj** %stackaddr$makeclosure56309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %anf_45bind48168, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48795, %struct.ScmObj* %k48432, i64 1)
%argslist54673$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%argslist54673$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist54673$_37take480390)
store volatile %struct.ScmObj* %argslist54673$_37take480391, %struct.ScmObj** %stackaddr$prim56311, align 8
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%argslist54673$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %argslist54673$_37take480391)
store volatile %struct.ScmObj* %argslist54673$_37take480392, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%argslist54673$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist54673$_37take480392)
store volatile %struct.ScmObj* %argslist54673$_37take480393, %struct.ScmObj** %stackaddr$prim56313, align 8
%clofunc56314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc56314(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54673$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48795(%struct.ScmObj* %env$ae48795,%struct.ScmObj* %current_45args54669) {
%stackaddr$env-ref56315 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 0)
store %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$env-ref56315
%stackaddr$env-ref56316 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48795, i64 1)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56316
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54669)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%current_45args54670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54669)
store volatile %struct.ScmObj* %current_45args54670, %struct.ScmObj** %stackaddr$prim56318, align 8
%stackaddr$prim56319 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54670)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56319, align 8
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%cpsprim48434 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %anf_45bind48171)
store volatile %struct.ScmObj* %cpsprim48434, %struct.ScmObj** %stackaddr$prim56320, align 8
%ae48801 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54672$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%argslist54672$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48434, %struct.ScmObj* %argslist54672$k484320)
store volatile %struct.ScmObj* %argslist54672$k484321, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%argslist54672$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48801, %struct.ScmObj* %argslist54672$k484321)
store volatile %struct.ScmObj* %argslist54672$k484322, %struct.ScmObj** %stackaddr$prim56322, align 8
%clofunc56323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56323(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist54672$k484322)
ret void
}

define tailcc void @proc_clo$ae48666(%struct.ScmObj* %env$ae48666,%struct.ScmObj* %current_45args54677) {
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54677)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim56324, align 8
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%current_45args54678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54677)
store volatile %struct.ScmObj* %current_45args54678, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54678)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim56326, align 8
%ae48668 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56327 = alloca %struct.ScmObj*, align 8
%fptrToInt56328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48669 to i64
%ae48669 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56328)
store volatile %struct.ScmObj* %ae48669, %struct.ScmObj** %stackaddr$makeclosure56327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48669, %struct.ScmObj* %_37map48043, i64 0)
%argslist54694$k484350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56329 = alloca %struct.ScmObj*, align 8
%argslist54694$k484351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48669, %struct.ScmObj* %argslist54694$k484350)
store volatile %struct.ScmObj* %argslist54694$k484351, %struct.ScmObj** %stackaddr$prim56329, align 8
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%argslist54694$k484352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48668, %struct.ScmObj* %argslist54694$k484351)
store volatile %struct.ScmObj* %argslist54694$k484352, %struct.ScmObj** %stackaddr$prim56330, align 8
%clofunc56331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48435)
musttail call tailcc void %clofunc56331(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist54694$k484352)
ret void
}

define tailcc void @proc_clo$ae48669(%struct.ScmObj* %env$ae48669,%struct.ScmObj* %current_45args54680) {
%stackaddr$env-ref56332 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48669, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56332
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54680)
store volatile %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%current_45args54681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54680)
store volatile %struct.ScmObj* %current_45args54681, %struct.ScmObj** %stackaddr$prim56334, align 8
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54681)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%current_45args54682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54681)
store volatile %struct.ScmObj* %current_45args54682, %struct.ScmObj** %stackaddr$prim56336, align 8
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54682)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim56337, align 8
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim56338, align 8
%truthy$cmp56339 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48160)
%cmp$cmp56339 = icmp eq i64 %truthy$cmp56339, 1
br i1 %cmp$cmp56339, label %truebranch$cmp56339, label %falsebranch$cmp56339
truebranch$cmp56339:
%ae48673 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48674 = call %struct.ScmObj* @const_init_null()
%argslist54684$k484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%argslist54684$k484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48674, %struct.ScmObj* %argslist54684$k484360)
store volatile %struct.ScmObj* %argslist54684$k484361, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%argslist54684$k484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48673, %struct.ScmObj* %argslist54684$k484361)
store volatile %struct.ScmObj* %argslist54684$k484362, %struct.ScmObj** %stackaddr$prim56341, align 8
%clofunc56342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48436)
musttail call tailcc void %clofunc56342(%struct.ScmObj* %k48436, %struct.ScmObj* %argslist54684$k484362)
ret void
falsebranch$cmp56339:
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim56343, align 8
%stackaddr$makeclosure56344 = alloca %struct.ScmObj*, align 8
%fptrToInt56345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48683 to i64
%ae48683 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56345)
store volatile %struct.ScmObj* %ae48683, %struct.ScmObj** %stackaddr$makeclosure56344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %k48436, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %f48045, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %lst48044, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48683, %struct.ScmObj* %_37map48043, i64 3)
%argslist54693$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%argslist54693$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %argslist54693$f480450)
store volatile %struct.ScmObj* %argslist54693$f480451, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%argslist54693$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48683, %struct.ScmObj* %argslist54693$f480451)
store volatile %struct.ScmObj* %argslist54693$f480452, %struct.ScmObj** %stackaddr$prim56347, align 8
%clofunc56348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc56348(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54693$f480452)
ret void
}

define tailcc void @proc_clo$ae48683(%struct.ScmObj* %env$ae48683,%struct.ScmObj* %current_45args54685) {
%stackaddr$env-ref56349 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 0)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref56349
%stackaddr$env-ref56350 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 1)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref56350
%stackaddr$env-ref56351 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 2)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref56351
%stackaddr$env-ref56352 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48683, i64 3)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56352
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54685)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim56353, align 8
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%current_45args54686 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54685)
store volatile %struct.ScmObj* %current_45args54686, %struct.ScmObj** %stackaddr$prim56354, align 8
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54686)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim56355, align 8
%stackaddr$prim56356 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim56356, align 8
%stackaddr$makeclosure56357 = alloca %struct.ScmObj*, align 8
%fptrToInt56358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48687 to i64
%ae48687 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56358)
store volatile %struct.ScmObj* %ae48687, %struct.ScmObj** %stackaddr$makeclosure56357, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %k48436, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %anf_45bind48162, i64 1)
%argslist54692$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56359 = alloca %struct.ScmObj*, align 8
%argslist54692$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist54692$_37map480430)
store volatile %struct.ScmObj* %argslist54692$_37map480431, %struct.ScmObj** %stackaddr$prim56359, align 8
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%argslist54692$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54692$_37map480431)
store volatile %struct.ScmObj* %argslist54692$_37map480432, %struct.ScmObj** %stackaddr$prim56360, align 8
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%argslist54692$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48687, %struct.ScmObj* %argslist54692$_37map480432)
store volatile %struct.ScmObj* %argslist54692$_37map480433, %struct.ScmObj** %stackaddr$prim56361, align 8
%clofunc56362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc56362(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist54692$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48687(%struct.ScmObj* %env$ae48687,%struct.ScmObj* %current_45args54688) {
%stackaddr$env-ref56363 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 0)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref56363
%stackaddr$env-ref56364 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 1)
store %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$env-ref56364
%stackaddr$prim56365 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54688)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim56365, align 8
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%current_45args54689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54688)
store volatile %struct.ScmObj* %current_45args54689, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$prim56367 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54689)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim56367, align 8
%stackaddr$prim56368 = alloca %struct.ScmObj*, align 8
%cpsprim48439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %anf_45bind48164)
store volatile %struct.ScmObj* %cpsprim48439, %struct.ScmObj** %stackaddr$prim56368, align 8
%ae48693 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54691$k484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%argslist54691$k484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48439, %struct.ScmObj* %argslist54691$k484360)
store volatile %struct.ScmObj* %argslist54691$k484361, %struct.ScmObj** %stackaddr$prim56369, align 8
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%argslist54691$k484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48693, %struct.ScmObj* %argslist54691$k484361)
store volatile %struct.ScmObj* %argslist54691$k484362, %struct.ScmObj** %stackaddr$prim56370, align 8
%clofunc56371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48436)
musttail call tailcc void %clofunc56371(%struct.ScmObj* %k48436, %struct.ScmObj* %argslist54691$k484362)
ret void
}

define tailcc void @proc_clo$ae48586(%struct.ScmObj* %env$ae48586,%struct.ScmObj* %current_45args54697) {
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54697)
store volatile %struct.ScmObj* %k48440, %struct.ScmObj** %stackaddr$prim56372, align 8
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%current_45args54698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54697)
store volatile %struct.ScmObj* %current_45args54698, %struct.ScmObj** %stackaddr$prim56373, align 8
%stackaddr$prim56374 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54698)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim56374, align 8
%ae48588 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56375 = alloca %struct.ScmObj*, align 8
%fptrToInt56376 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48589 to i64
%ae48589 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56376)
store volatile %struct.ScmObj* %ae48589, %struct.ScmObj** %stackaddr$makeclosure56375, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48589, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54711$k484400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%argslist54711$k484401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48589, %struct.ScmObj* %argslist54711$k484400)
store volatile %struct.ScmObj* %argslist54711$k484401, %struct.ScmObj** %stackaddr$prim56377, align 8
%stackaddr$prim56378 = alloca %struct.ScmObj*, align 8
%argslist54711$k484402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48588, %struct.ScmObj* %argslist54711$k484401)
store volatile %struct.ScmObj* %argslist54711$k484402, %struct.ScmObj** %stackaddr$prim56378, align 8
%clofunc56379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48440)
musttail call tailcc void %clofunc56379(%struct.ScmObj* %k48440, %struct.ScmObj* %argslist54711$k484402)
ret void
}

define tailcc void @proc_clo$ae48589(%struct.ScmObj* %env$ae48589,%struct.ScmObj* %current_45args54700) {
%stackaddr$env-ref56380 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48589, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56380
%stackaddr$prim56381 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54700)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim56381, align 8
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%current_45args54701 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54700)
store volatile %struct.ScmObj* %current_45args54701, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54701)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%current_45args54702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54701)
store volatile %struct.ScmObj* %current_45args54702, %struct.ScmObj** %stackaddr$prim56384, align 8
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54702)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim56385, align 8
%stackaddr$prim56386 = alloca %struct.ScmObj*, align 8
%current_45args54703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54702)
store volatile %struct.ScmObj* %current_45args54703, %struct.ScmObj** %stackaddr$prim56386, align 8
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54703)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim56387, align 8
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim56388, align 8
%truthy$cmp56389 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48155)
%cmp$cmp56389 = icmp eq i64 %truthy$cmp56389, 1
br i1 %cmp$cmp56389, label %truebranch$cmp56389, label %falsebranch$cmp56389
truebranch$cmp56389:
%ae48593 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54705$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist54705$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54705$k484410)
store volatile %struct.ScmObj* %argslist54705$k484411, %struct.ScmObj** %stackaddr$prim56390, align 8
%stackaddr$prim56391 = alloca %struct.ScmObj*, align 8
%argslist54705$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist54705$k484411)
store volatile %struct.ScmObj* %argslist54705$k484412, %struct.ScmObj** %stackaddr$prim56391, align 8
%clofunc56392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc56392(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist54705$k484412)
ret void
falsebranch$cmp56389:
%stackaddr$prim56393 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim56393, align 8
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim56394, align 8
%stackaddr$makeclosure56395 = alloca %struct.ScmObj*, align 8
%fptrToInt56396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48601 to i64
%ae48601 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56396)
store volatile %struct.ScmObj* %ae48601, %struct.ScmObj** %stackaddr$makeclosure56395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48601, %struct.ScmObj* %k48441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48601, %struct.ScmObj* %f48050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48601, %struct.ScmObj* %anf_45bind48156, i64 2)
%argslist54710$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%argslist54710$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54710$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54710$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56397, align 8
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%argslist54710$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54710$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54710$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56398, align 8
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%argslist54710$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54710$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54710$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%argslist54710$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48601, %struct.ScmObj* %argslist54710$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54710$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56400, align 8
%clofunc56401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56401(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54710$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48601(%struct.ScmObj* %env$ae48601,%struct.ScmObj* %current_45args54706) {
%stackaddr$env-ref56402 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48601, i64 0)
store %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$env-ref56402
%stackaddr$env-ref56403 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48601, i64 1)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref56403
%stackaddr$env-ref56404 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48601, i64 2)
store %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$env-ref56404
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim56405, align 8
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%current_45args54707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %current_45args54707, %struct.ScmObj** %stackaddr$prim56406, align 8
%stackaddr$prim56407 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54707)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim56407, align 8
%argslist54709$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%argslist54709$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48158, %struct.ScmObj* %argslist54709$f480500)
store volatile %struct.ScmObj* %argslist54709$f480501, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%argslist54709$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist54709$f480501)
store volatile %struct.ScmObj* %argslist54709$f480502, %struct.ScmObj** %stackaddr$prim56409, align 8
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%argslist54709$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist54709$f480502)
store volatile %struct.ScmObj* %argslist54709$f480503, %struct.ScmObj** %stackaddr$prim56410, align 8
%clofunc56411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc56411(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54709$f480503)
ret void
}

define tailcc void @proc_clo$ae48469(%struct.ScmObj* %env$ae48469,%struct.ScmObj* %current_45args54714) {
%stackaddr$prim56412 = alloca %struct.ScmObj*, align 8
%k48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54714)
store volatile %struct.ScmObj* %k48443, %struct.ScmObj** %stackaddr$prim56412, align 8
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%current_45args54715 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54714)
store volatile %struct.ScmObj* %current_45args54715, %struct.ScmObj** %stackaddr$prim56413, align 8
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54715)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim56414, align 8
%ae48471 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56415 = alloca %struct.ScmObj*, align 8
%fptrToInt56416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48472 to i64
%ae48472 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56416)
store volatile %struct.ScmObj* %ae48472, %struct.ScmObj** %stackaddr$makeclosure56415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48472, %struct.ScmObj* %y48027, i64 0)
%argslist54733$k484430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56417 = alloca %struct.ScmObj*, align 8
%argslist54733$k484431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48472, %struct.ScmObj* %argslist54733$k484430)
store volatile %struct.ScmObj* %argslist54733$k484431, %struct.ScmObj** %stackaddr$prim56417, align 8
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%argslist54733$k484432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48471, %struct.ScmObj* %argslist54733$k484431)
store volatile %struct.ScmObj* %argslist54733$k484432, %struct.ScmObj** %stackaddr$prim56418, align 8
%clofunc56419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48443)
musttail call tailcc void %clofunc56419(%struct.ScmObj* %k48443, %struct.ScmObj* %argslist54733$k484432)
ret void
}

define tailcc void @proc_clo$ae48472(%struct.ScmObj* %env$ae48472,%struct.ScmObj* %current_45args54717) {
%stackaddr$env-ref56420 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48472, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56420
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54717)
store volatile %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$prim56421, align 8
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%current_45args54718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54717)
store volatile %struct.ScmObj* %current_45args54718, %struct.ScmObj** %stackaddr$prim56422, align 8
%stackaddr$prim56423 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54718)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim56423, align 8
%stackaddr$makeclosure56424 = alloca %struct.ScmObj*, align 8
%fptrToInt56425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48473 to i64
%ae48473 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56425)
store volatile %struct.ScmObj* %ae48473, %struct.ScmObj** %stackaddr$makeclosure56424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48473, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48473, %struct.ScmObj* %k48444, i64 1)
%ae48474 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56426 = alloca %struct.ScmObj*, align 8
%fptrToInt56427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48475 to i64
%ae48475 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56427)
store volatile %struct.ScmObj* %ae48475, %struct.ScmObj** %stackaddr$makeclosure56426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48475, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48475, %struct.ScmObj* %y48027, i64 1)
%argslist54732$ae484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56428 = alloca %struct.ScmObj*, align 8
%argslist54732$ae484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48475, %struct.ScmObj* %argslist54732$ae484730)
store volatile %struct.ScmObj* %argslist54732$ae484731, %struct.ScmObj** %stackaddr$prim56428, align 8
%stackaddr$prim56429 = alloca %struct.ScmObj*, align 8
%argslist54732$ae484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48474, %struct.ScmObj* %argslist54732$ae484731)
store volatile %struct.ScmObj* %argslist54732$ae484732, %struct.ScmObj** %stackaddr$prim56429, align 8
%clofunc56430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48473)
musttail call tailcc void %clofunc56430(%struct.ScmObj* %ae48473, %struct.ScmObj* %argslist54732$ae484732)
ret void
}

define tailcc void @proc_clo$ae48473(%struct.ScmObj* %env$ae48473,%struct.ScmObj* %current_45args54720) {
%stackaddr$env-ref56431 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48473, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56431
%stackaddr$env-ref56432 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48473, i64 1)
store %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$env-ref56432
%stackaddr$prim56433 = alloca %struct.ScmObj*, align 8
%_95k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54720)
store volatile %struct.ScmObj* %_95k48445, %struct.ScmObj** %stackaddr$prim56433, align 8
%stackaddr$prim56434 = alloca %struct.ScmObj*, align 8
%current_45args54721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54720)
store volatile %struct.ScmObj* %current_45args54721, %struct.ScmObj** %stackaddr$prim56434, align 8
%stackaddr$prim56435 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54721)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim56435, align 8
%argslist54723$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%argslist54723$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48153, %struct.ScmObj* %argslist54723$f480280)
store volatile %struct.ScmObj* %argslist54723$f480281, %struct.ScmObj** %stackaddr$prim56436, align 8
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%argslist54723$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48444, %struct.ScmObj* %argslist54723$f480281)
store volatile %struct.ScmObj* %argslist54723$f480282, %struct.ScmObj** %stackaddr$prim56437, align 8
%clofunc56438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc56438(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54723$f480282)
ret void
}

define tailcc void @proc_clo$ae48475(%struct.ScmObj* %env$ae48475,%struct.ScmObj* %args4802948446) {
%stackaddr$env-ref56439 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48475, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56439
%stackaddr$env-ref56440 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48475, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56440
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%k48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948446)
store volatile %struct.ScmObj* %k48447, %struct.ScmObj** %stackaddr$prim56441, align 8
%stackaddr$prim56442 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948446)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim56442, align 8
%stackaddr$makeclosure56443 = alloca %struct.ScmObj*, align 8
%fptrToInt56444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48479 to i64
%ae48479 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56444)
store volatile %struct.ScmObj* %ae48479, %struct.ScmObj** %stackaddr$makeclosure56443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48479, %struct.ScmObj* %k48447, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48479, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48479, %struct.ScmObj* %f48028, i64 2)
%argslist54731$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56445 = alloca %struct.ScmObj*, align 8
%argslist54731$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54731$y480270)
store volatile %struct.ScmObj* %argslist54731$y480271, %struct.ScmObj** %stackaddr$prim56445, align 8
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%argslist54731$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48479, %struct.ScmObj* %argslist54731$y480271)
store volatile %struct.ScmObj* %argslist54731$y480272, %struct.ScmObj** %stackaddr$prim56446, align 8
%clofunc56447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc56447(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54731$y480272)
ret void
}

define tailcc void @proc_clo$ae48479(%struct.ScmObj* %env$ae48479,%struct.ScmObj* %current_45args54724) {
%stackaddr$env-ref56448 = alloca %struct.ScmObj*, align 8
%k48447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48479, i64 0)
store %struct.ScmObj* %k48447, %struct.ScmObj** %stackaddr$env-ref56448
%stackaddr$env-ref56449 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48479, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56449
%stackaddr$env-ref56450 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48479, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56450
%stackaddr$prim56451 = alloca %struct.ScmObj*, align 8
%_95k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54724)
store volatile %struct.ScmObj* %_95k48448, %struct.ScmObj** %stackaddr$prim56451, align 8
%stackaddr$prim56452 = alloca %struct.ScmObj*, align 8
%current_45args54725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54724)
store volatile %struct.ScmObj* %current_45args54725, %struct.ScmObj** %stackaddr$prim56452, align 8
%stackaddr$prim56453 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54725)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim56453, align 8
%stackaddr$makeclosure56454 = alloca %struct.ScmObj*, align 8
%fptrToInt56455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48482 to i64
%ae48482 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56455)
store volatile %struct.ScmObj* %ae48482, %struct.ScmObj** %stackaddr$makeclosure56454, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48482, %struct.ScmObj* %k48447, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48482, %struct.ScmObj* %args48029, i64 1)
%argslist54730$anf_45bind481510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%argslist54730$anf_45bind481511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54730$anf_45bind481510)
store volatile %struct.ScmObj* %argslist54730$anf_45bind481511, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%argslist54730$anf_45bind481512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48482, %struct.ScmObj* %argslist54730$anf_45bind481511)
store volatile %struct.ScmObj* %argslist54730$anf_45bind481512, %struct.ScmObj** %stackaddr$prim56457, align 8
%clofunc56458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48151)
musttail call tailcc void %clofunc56458(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist54730$anf_45bind481512)
ret void
}

define tailcc void @proc_clo$ae48482(%struct.ScmObj* %env$ae48482,%struct.ScmObj* %current_45args54727) {
%stackaddr$env-ref56459 = alloca %struct.ScmObj*, align 8
%k48447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48482, i64 0)
store %struct.ScmObj* %k48447, %struct.ScmObj** %stackaddr$env-ref56459
%stackaddr$env-ref56460 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48482, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56460
%stackaddr$prim56461 = alloca %struct.ScmObj*, align 8
%_95k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54727)
store volatile %struct.ScmObj* %_95k48449, %struct.ScmObj** %stackaddr$prim56461, align 8
%stackaddr$prim56462 = alloca %struct.ScmObj*, align 8
%current_45args54728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54727)
store volatile %struct.ScmObj* %current_45args54728, %struct.ScmObj** %stackaddr$prim56462, align 8
%stackaddr$prim56463 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54728)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim56463, align 8
%stackaddr$prim56464 = alloca %struct.ScmObj*, align 8
%cpsargs48450 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48447, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48450, %struct.ScmObj** %stackaddr$prim56464, align 8
%clofunc56465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48152)
musttail call tailcc void %clofunc56465(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %cpsargs48450)
ret void
}

define tailcc void @proc_clo$ae48454(%struct.ScmObj* %env$ae48454,%struct.ScmObj* %current_45args54735) {
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54735)
store volatile %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$prim56466, align 8
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%current_45args54736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54735)
store volatile %struct.ScmObj* %current_45args54736, %struct.ScmObj** %stackaddr$prim56467, align 8
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54736)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim56468, align 8
%argslist54738$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%argslist54738$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54738$yu480260)
store volatile %struct.ScmObj* %argslist54738$yu480261, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%argslist54738$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist54738$yu480261)
store volatile %struct.ScmObj* %argslist54738$yu480262, %struct.ScmObj** %stackaddr$prim56470, align 8
%clofunc56471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc56471(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54738$yu480262)
ret void
}