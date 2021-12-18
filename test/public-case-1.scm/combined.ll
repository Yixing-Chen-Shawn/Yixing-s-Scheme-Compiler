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

@global$sym$ae4668149959 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv49511 = call %struct.ScmObj* @const_init_null()
%mainargs49512 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv49511, %struct.ScmObj* %mainargs49512)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv49509,%struct.ScmObj* %mainargs49510) {
%stackaddr$makeclosure49513 = alloca %struct.ScmObj*, align 8
%fptrToInt49514 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43293 to i64
%ae43293 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49514)
store volatile %struct.ScmObj* %ae43293, %struct.ScmObj** %stackaddr$makeclosure49513, align 8
%ae43294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49515 = alloca %struct.ScmObj*, align 8
%fptrToInt49516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43295 to i64
%ae43295 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49516)
store volatile %struct.ScmObj* %ae43295, %struct.ScmObj** %stackaddr$makeclosure49515, align 8
%argslist49508$ae432930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49517 = alloca %struct.ScmObj*, align 8
%argslist49508$ae432931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43295, %struct.ScmObj* %argslist49508$ae432930)
store volatile %struct.ScmObj* %argslist49508$ae432931, %struct.ScmObj** %stackaddr$prim49517, align 8
%stackaddr$prim49518 = alloca %struct.ScmObj*, align 8
%argslist49508$ae432932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43294, %struct.ScmObj* %argslist49508$ae432931)
store volatile %struct.ScmObj* %argslist49508$ae432932, %struct.ScmObj** %stackaddr$prim49518, align 8
%clofunc49519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43293)
musttail call tailcc void %clofunc49519(%struct.ScmObj* %ae43293, %struct.ScmObj* %argslist49508$ae432932)
ret void
}

define tailcc void @proc_clo$ae43293(%struct.ScmObj* %env$ae43293,%struct.ScmObj* %current_45args48957) {
%stackaddr$prim49520 = alloca %struct.ScmObj*, align 8
%_95k43130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48957)
store volatile %struct.ScmObj* %_95k43130, %struct.ScmObj** %stackaddr$prim49520, align 8
%stackaddr$prim49521 = alloca %struct.ScmObj*, align 8
%current_45args48958 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48957)
store volatile %struct.ScmObj* %current_45args48958, %struct.ScmObj** %stackaddr$prim49521, align 8
%stackaddr$prim49522 = alloca %struct.ScmObj*, align 8
%anf_45bind43015 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48958)
store volatile %struct.ScmObj* %anf_45bind43015, %struct.ScmObj** %stackaddr$prim49522, align 8
%stackaddr$makeclosure49523 = alloca %struct.ScmObj*, align 8
%fptrToInt49524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43308 to i64
%ae43308 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49524)
store volatile %struct.ScmObj* %ae43308, %struct.ScmObj** %stackaddr$makeclosure49523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43308, %struct.ScmObj* %anf_45bind43015, i64 0)
%ae43309 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49525 = alloca %struct.ScmObj*, align 8
%fptrToInt49526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43310 to i64
%ae43310 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49526)
store volatile %struct.ScmObj* %ae43310, %struct.ScmObj** %stackaddr$makeclosure49525, align 8
%argslist49503$ae433080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%argslist49503$ae433081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43310, %struct.ScmObj* %argslist49503$ae433080)
store volatile %struct.ScmObj* %argslist49503$ae433081, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%argslist49503$ae433082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43309, %struct.ScmObj* %argslist49503$ae433081)
store volatile %struct.ScmObj* %argslist49503$ae433082, %struct.ScmObj** %stackaddr$prim49528, align 8
%clofunc49529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43308)
musttail call tailcc void %clofunc49529(%struct.ScmObj* %ae43308, %struct.ScmObj* %argslist49503$ae433082)
ret void
}

define tailcc void @proc_clo$ae43308(%struct.ScmObj* %env$ae43308,%struct.ScmObj* %current_45args48960) {
%stackaddr$env-ref49530 = alloca %struct.ScmObj*, align 8
%anf_45bind43015 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43308, i64 0)
store %struct.ScmObj* %anf_45bind43015, %struct.ScmObj** %stackaddr$env-ref49530
%stackaddr$prim49531 = alloca %struct.ScmObj*, align 8
%_95k43131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48960)
store volatile %struct.ScmObj* %_95k43131, %struct.ScmObj** %stackaddr$prim49531, align 8
%stackaddr$prim49532 = alloca %struct.ScmObj*, align 8
%current_45args48961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48960)
store volatile %struct.ScmObj* %current_45args48961, %struct.ScmObj** %stackaddr$prim49532, align 8
%stackaddr$prim49533 = alloca %struct.ScmObj*, align 8
%anf_45bind43019 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48961)
store volatile %struct.ScmObj* %anf_45bind43019, %struct.ScmObj** %stackaddr$prim49533, align 8
%stackaddr$makeclosure49534 = alloca %struct.ScmObj*, align 8
%fptrToInt49535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43423 to i64
%ae43423 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49535)
store volatile %struct.ScmObj* %ae43423, %struct.ScmObj** %stackaddr$makeclosure49534, align 8
%argslist49482$anf_45bind430150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49536 = alloca %struct.ScmObj*, align 8
%argslist49482$anf_45bind430151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43019, %struct.ScmObj* %argslist49482$anf_45bind430150)
store volatile %struct.ScmObj* %argslist49482$anf_45bind430151, %struct.ScmObj** %stackaddr$prim49536, align 8
%stackaddr$prim49537 = alloca %struct.ScmObj*, align 8
%argslist49482$anf_45bind430152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43423, %struct.ScmObj* %argslist49482$anf_45bind430151)
store volatile %struct.ScmObj* %argslist49482$anf_45bind430152, %struct.ScmObj** %stackaddr$prim49537, align 8
%clofunc49538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43015)
musttail call tailcc void %clofunc49538(%struct.ScmObj* %anf_45bind43015, %struct.ScmObj* %argslist49482$anf_45bind430152)
ret void
}

define tailcc void @proc_clo$ae43423(%struct.ScmObj* %env$ae43423,%struct.ScmObj* %current_45args48963) {
%stackaddr$prim49539 = alloca %struct.ScmObj*, align 8
%_95k43132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48963)
store volatile %struct.ScmObj* %_95k43132, %struct.ScmObj** %stackaddr$prim49539, align 8
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%current_45args48964 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48963)
store volatile %struct.ScmObj* %current_45args48964, %struct.ScmObj** %stackaddr$prim49540, align 8
%stackaddr$prim49541 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48964)
store volatile %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$prim49541, align 8
%stackaddr$makeclosure49542 = alloca %struct.ScmObj*, align 8
%fptrToInt49543 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43425 to i64
%ae43425 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49543)
store volatile %struct.ScmObj* %ae43425, %struct.ScmObj** %stackaddr$makeclosure49542, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43425, %struct.ScmObj* %Ycmb42895, i64 0)
%ae43426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49544 = alloca %struct.ScmObj*, align 8
%fptrToInt49545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43427 to i64
%ae43427 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49545)
store volatile %struct.ScmObj* %ae43427, %struct.ScmObj** %stackaddr$makeclosure49544, align 8
%argslist49481$ae434250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49546 = alloca %struct.ScmObj*, align 8
%argslist49481$ae434251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43427, %struct.ScmObj* %argslist49481$ae434250)
store volatile %struct.ScmObj* %argslist49481$ae434251, %struct.ScmObj** %stackaddr$prim49546, align 8
%stackaddr$prim49547 = alloca %struct.ScmObj*, align 8
%argslist49481$ae434252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43426, %struct.ScmObj* %argslist49481$ae434251)
store volatile %struct.ScmObj* %argslist49481$ae434252, %struct.ScmObj** %stackaddr$prim49547, align 8
%clofunc49548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43425)
musttail call tailcc void %clofunc49548(%struct.ScmObj* %ae43425, %struct.ScmObj* %argslist49481$ae434252)
ret void
}

define tailcc void @proc_clo$ae43425(%struct.ScmObj* %env$ae43425,%struct.ScmObj* %current_45args48966) {
%stackaddr$env-ref49549 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43425, i64 0)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49549
%stackaddr$prim49550 = alloca %struct.ScmObj*, align 8
%_95k43133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48966)
store volatile %struct.ScmObj* %_95k43133, %struct.ScmObj** %stackaddr$prim49550, align 8
%stackaddr$prim49551 = alloca %struct.ScmObj*, align 8
%current_45args48967 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48966)
store volatile %struct.ScmObj* %current_45args48967, %struct.ScmObj** %stackaddr$prim49551, align 8
%stackaddr$prim49552 = alloca %struct.ScmObj*, align 8
%anf_45bind43024 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48967)
store volatile %struct.ScmObj* %anf_45bind43024, %struct.ScmObj** %stackaddr$prim49552, align 8
%stackaddr$makeclosure49553 = alloca %struct.ScmObj*, align 8
%fptrToInt49554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43503 to i64
%ae43503 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49554)
store volatile %struct.ScmObj* %ae43503, %struct.ScmObj** %stackaddr$makeclosure49553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43503, %struct.ScmObj* %Ycmb42895, i64 0)
%argslist49465$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49555 = alloca %struct.ScmObj*, align 8
%argslist49465$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43024, %struct.ScmObj* %argslist49465$Ycmb428950)
store volatile %struct.ScmObj* %argslist49465$Ycmb428951, %struct.ScmObj** %stackaddr$prim49555, align 8
%stackaddr$prim49556 = alloca %struct.ScmObj*, align 8
%argslist49465$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43503, %struct.ScmObj* %argslist49465$Ycmb428951)
store volatile %struct.ScmObj* %argslist49465$Ycmb428952, %struct.ScmObj** %stackaddr$prim49556, align 8
%clofunc49557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49557(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49465$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae43503(%struct.ScmObj* %env$ae43503,%struct.ScmObj* %current_45args48969) {
%stackaddr$env-ref49558 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43503, i64 0)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49558
%stackaddr$prim49559 = alloca %struct.ScmObj*, align 8
%_95k43134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48969)
store volatile %struct.ScmObj* %_95k43134, %struct.ScmObj** %stackaddr$prim49559, align 8
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%current_45args48970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48969)
store volatile %struct.ScmObj* %current_45args48970, %struct.ScmObj** %stackaddr$prim49560, align 8
%stackaddr$prim49561 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48970)
store volatile %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$prim49561, align 8
%stackaddr$makeclosure49562 = alloca %struct.ScmObj*, align 8
%fptrToInt49563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43505 to i64
%ae43505 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49563)
store volatile %struct.ScmObj* %ae43505, %struct.ScmObj** %stackaddr$makeclosure49562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43505, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43505, %struct.ScmObj* %Ycmb42895, i64 1)
%ae43506 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49564 = alloca %struct.ScmObj*, align 8
%fptrToInt49565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43507 to i64
%ae43507 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49565)
store volatile %struct.ScmObj* %ae43507, %struct.ScmObj** %stackaddr$makeclosure49564, align 8
%argslist49464$ae435050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49566 = alloca %struct.ScmObj*, align 8
%argslist49464$ae435051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43507, %struct.ScmObj* %argslist49464$ae435050)
store volatile %struct.ScmObj* %argslist49464$ae435051, %struct.ScmObj** %stackaddr$prim49566, align 8
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%argslist49464$ae435052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43506, %struct.ScmObj* %argslist49464$ae435051)
store volatile %struct.ScmObj* %argslist49464$ae435052, %struct.ScmObj** %stackaddr$prim49567, align 8
%clofunc49568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43505)
musttail call tailcc void %clofunc49568(%struct.ScmObj* %ae43505, %struct.ScmObj* %argslist49464$ae435052)
ret void
}

define tailcc void @proc_clo$ae43505(%struct.ScmObj* %env$ae43505,%struct.ScmObj* %current_45args48972) {
%stackaddr$env-ref49569 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43505, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49569
%stackaddr$env-ref49570 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43505, i64 1)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49570
%stackaddr$prim49571 = alloca %struct.ScmObj*, align 8
%_95k43135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48972)
store volatile %struct.ScmObj* %_95k43135, %struct.ScmObj** %stackaddr$prim49571, align 8
%stackaddr$prim49572 = alloca %struct.ScmObj*, align 8
%current_45args48973 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48972)
store volatile %struct.ScmObj* %current_45args48973, %struct.ScmObj** %stackaddr$prim49572, align 8
%stackaddr$prim49573 = alloca %struct.ScmObj*, align 8
%anf_45bind43030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48973)
store volatile %struct.ScmObj* %anf_45bind43030, %struct.ScmObj** %stackaddr$prim49573, align 8
%stackaddr$makeclosure49574 = alloca %struct.ScmObj*, align 8
%fptrToInt49575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43600 to i64
%ae43600 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49575)
store volatile %struct.ScmObj* %ae43600, %struct.ScmObj** %stackaddr$makeclosure49574, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43600, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43600, %struct.ScmObj* %Ycmb42895, i64 1)
%argslist49445$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49576 = alloca %struct.ScmObj*, align 8
%argslist49445$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43030, %struct.ScmObj* %argslist49445$Ycmb428950)
store volatile %struct.ScmObj* %argslist49445$Ycmb428951, %struct.ScmObj** %stackaddr$prim49576, align 8
%stackaddr$prim49577 = alloca %struct.ScmObj*, align 8
%argslist49445$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43600, %struct.ScmObj* %argslist49445$Ycmb428951)
store volatile %struct.ScmObj* %argslist49445$Ycmb428952, %struct.ScmObj** %stackaddr$prim49577, align 8
%clofunc49578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49578(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49445$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae43600(%struct.ScmObj* %env$ae43600,%struct.ScmObj* %current_45args48975) {
%stackaddr$env-ref49579 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43600, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49579
%stackaddr$env-ref49580 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43600, i64 1)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49580
%stackaddr$prim49581 = alloca %struct.ScmObj*, align 8
%_95k43136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48975)
store volatile %struct.ScmObj* %_95k43136, %struct.ScmObj** %stackaddr$prim49581, align 8
%stackaddr$prim49582 = alloca %struct.ScmObj*, align 8
%current_45args48976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48975)
store volatile %struct.ScmObj* %current_45args48976, %struct.ScmObj** %stackaddr$prim49582, align 8
%stackaddr$prim49583 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48976)
store volatile %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$prim49583, align 8
%stackaddr$makeclosure49584 = alloca %struct.ScmObj*, align 8
%fptrToInt49585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43602 to i64
%ae43602 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49585)
store volatile %struct.ScmObj* %ae43602, %struct.ScmObj** %stackaddr$makeclosure49584, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43602, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43602, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43602, %struct.ScmObj* %Ycmb42895, i64 2)
%ae43603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49586 = alloca %struct.ScmObj*, align 8
%fptrToInt49587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43604 to i64
%ae43604 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49587)
store volatile %struct.ScmObj* %ae43604, %struct.ScmObj** %stackaddr$makeclosure49586, align 8
%argslist49444$ae436020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49588 = alloca %struct.ScmObj*, align 8
%argslist49444$ae436021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43604, %struct.ScmObj* %argslist49444$ae436020)
store volatile %struct.ScmObj* %argslist49444$ae436021, %struct.ScmObj** %stackaddr$prim49588, align 8
%stackaddr$prim49589 = alloca %struct.ScmObj*, align 8
%argslist49444$ae436022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43603, %struct.ScmObj* %argslist49444$ae436021)
store volatile %struct.ScmObj* %argslist49444$ae436022, %struct.ScmObj** %stackaddr$prim49589, align 8
%clofunc49590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43602)
musttail call tailcc void %clofunc49590(%struct.ScmObj* %ae43602, %struct.ScmObj* %argslist49444$ae436022)
ret void
}

define tailcc void @proc_clo$ae43602(%struct.ScmObj* %env$ae43602,%struct.ScmObj* %current_45args48978) {
%stackaddr$env-ref49591 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43602, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49591
%stackaddr$env-ref49592 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43602, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49592
%stackaddr$env-ref49593 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43602, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49593
%stackaddr$prim49594 = alloca %struct.ScmObj*, align 8
%_95k43137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48978)
store volatile %struct.ScmObj* %_95k43137, %struct.ScmObj** %stackaddr$prim49594, align 8
%stackaddr$prim49595 = alloca %struct.ScmObj*, align 8
%current_45args48979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48978)
store volatile %struct.ScmObj* %current_45args48979, %struct.ScmObj** %stackaddr$prim49595, align 8
%stackaddr$prim49596 = alloca %struct.ScmObj*, align 8
%anf_45bind43037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48979)
store volatile %struct.ScmObj* %anf_45bind43037, %struct.ScmObj** %stackaddr$prim49596, align 8
%stackaddr$makeclosure49597 = alloca %struct.ScmObj*, align 8
%fptrToInt49598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43750 to i64
%ae43750 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49598)
store volatile %struct.ScmObj* %ae43750, %struct.ScmObj** %stackaddr$makeclosure49597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43750, %struct.ScmObj* %Ycmb42895, i64 2)
%argslist49428$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%argslist49428$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43037, %struct.ScmObj* %argslist49428$Ycmb428950)
store volatile %struct.ScmObj* %argslist49428$Ycmb428951, %struct.ScmObj** %stackaddr$prim49599, align 8
%stackaddr$prim49600 = alloca %struct.ScmObj*, align 8
%argslist49428$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43750, %struct.ScmObj* %argslist49428$Ycmb428951)
store volatile %struct.ScmObj* %argslist49428$Ycmb428952, %struct.ScmObj** %stackaddr$prim49600, align 8
%clofunc49601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49601(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49428$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae43750(%struct.ScmObj* %env$ae43750,%struct.ScmObj* %current_45args48981) {
%stackaddr$env-ref49602 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49602
%stackaddr$env-ref49603 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49603
%stackaddr$env-ref49604 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43750, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49604
%stackaddr$prim49605 = alloca %struct.ScmObj*, align 8
%_95k43138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48981)
store volatile %struct.ScmObj* %_95k43138, %struct.ScmObj** %stackaddr$prim49605, align 8
%stackaddr$prim49606 = alloca %struct.ScmObj*, align 8
%current_45args48982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48981)
store volatile %struct.ScmObj* %current_45args48982, %struct.ScmObj** %stackaddr$prim49606, align 8
%stackaddr$prim49607 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48982)
store volatile %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$prim49607, align 8
%stackaddr$makeclosure49608 = alloca %struct.ScmObj*, align 8
%fptrToInt49609 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43752 to i64
%ae43752 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49609)
store volatile %struct.ScmObj* %ae43752, %struct.ScmObj** %stackaddr$makeclosure49608, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43752, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43752, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43752, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43752, %struct.ScmObj* %_37take42908, i64 3)
%ae43753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49610 = alloca %struct.ScmObj*, align 8
%fptrToInt49611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43754 to i64
%ae43754 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49611)
store volatile %struct.ScmObj* %ae43754, %struct.ScmObj** %stackaddr$makeclosure49610, align 8
%argslist49427$ae437520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49612 = alloca %struct.ScmObj*, align 8
%argslist49427$ae437521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43754, %struct.ScmObj* %argslist49427$ae437520)
store volatile %struct.ScmObj* %argslist49427$ae437521, %struct.ScmObj** %stackaddr$prim49612, align 8
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%argslist49427$ae437522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43753, %struct.ScmObj* %argslist49427$ae437521)
store volatile %struct.ScmObj* %argslist49427$ae437522, %struct.ScmObj** %stackaddr$prim49613, align 8
%clofunc49614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43752)
musttail call tailcc void %clofunc49614(%struct.ScmObj* %ae43752, %struct.ScmObj* %argslist49427$ae437522)
ret void
}

define tailcc void @proc_clo$ae43752(%struct.ScmObj* %env$ae43752,%struct.ScmObj* %current_45args48984) {
%stackaddr$env-ref49615 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43752, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49615
%stackaddr$env-ref49616 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43752, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49616
%stackaddr$env-ref49617 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43752, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49617
%stackaddr$env-ref49618 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43752, i64 3)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref49618
%stackaddr$prim49619 = alloca %struct.ScmObj*, align 8
%_95k43139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48984)
store volatile %struct.ScmObj* %_95k43139, %struct.ScmObj** %stackaddr$prim49619, align 8
%stackaddr$prim49620 = alloca %struct.ScmObj*, align 8
%current_45args48985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48984)
store volatile %struct.ScmObj* %current_45args48985, %struct.ScmObj** %stackaddr$prim49620, align 8
%stackaddr$prim49621 = alloca %struct.ScmObj*, align 8
%anf_45bind43041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48985)
store volatile %struct.ScmObj* %anf_45bind43041, %struct.ScmObj** %stackaddr$prim49621, align 8
%stackaddr$makeclosure49622 = alloca %struct.ScmObj*, align 8
%fptrToInt49623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43833 to i64
%ae43833 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49623)
store volatile %struct.ScmObj* %ae43833, %struct.ScmObj** %stackaddr$makeclosure49622, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43833, %struct.ScmObj* %_37take42908, i64 3)
%argslist49413$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49624 = alloca %struct.ScmObj*, align 8
%argslist49413$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43041, %struct.ScmObj* %argslist49413$Ycmb428950)
store volatile %struct.ScmObj* %argslist49413$Ycmb428951, %struct.ScmObj** %stackaddr$prim49624, align 8
%stackaddr$prim49625 = alloca %struct.ScmObj*, align 8
%argslist49413$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43833, %struct.ScmObj* %argslist49413$Ycmb428951)
store volatile %struct.ScmObj* %argslist49413$Ycmb428952, %struct.ScmObj** %stackaddr$prim49625, align 8
%clofunc49626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49626(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49413$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae43833(%struct.ScmObj* %env$ae43833,%struct.ScmObj* %current_45args48987) {
%stackaddr$env-ref49627 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49627
%stackaddr$env-ref49628 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49628
%stackaddr$env-ref49629 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49629
%stackaddr$env-ref49630 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43833, i64 3)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref49630
%stackaddr$prim49631 = alloca %struct.ScmObj*, align 8
%_95k43140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48987)
store volatile %struct.ScmObj* %_95k43140, %struct.ScmObj** %stackaddr$prim49631, align 8
%stackaddr$prim49632 = alloca %struct.ScmObj*, align 8
%current_45args48988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48987)
store volatile %struct.ScmObj* %current_45args48988, %struct.ScmObj** %stackaddr$prim49632, align 8
%stackaddr$prim49633 = alloca %struct.ScmObj*, align 8
%_37length42905 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48988)
store volatile %struct.ScmObj* %_37length42905, %struct.ScmObj** %stackaddr$prim49633, align 8
%stackaddr$makeclosure49634 = alloca %struct.ScmObj*, align 8
%fptrToInt49635 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43835 to i64
%ae43835 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49635)
store volatile %struct.ScmObj* %ae43835, %struct.ScmObj** %stackaddr$makeclosure49634, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43835, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43835, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43835, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43835, %struct.ScmObj* %_37take42908, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae43835, %struct.ScmObj* %_37length42905, i64 4)
%ae43836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49636 = alloca %struct.ScmObj*, align 8
%fptrToInt49637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43837 to i64
%ae43837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49637)
store volatile %struct.ScmObj* %ae43837, %struct.ScmObj** %stackaddr$makeclosure49636, align 8
%argslist49412$ae438350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49638 = alloca %struct.ScmObj*, align 8
%argslist49412$ae438351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43837, %struct.ScmObj* %argslist49412$ae438350)
store volatile %struct.ScmObj* %argslist49412$ae438351, %struct.ScmObj** %stackaddr$prim49638, align 8
%stackaddr$prim49639 = alloca %struct.ScmObj*, align 8
%argslist49412$ae438352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43836, %struct.ScmObj* %argslist49412$ae438351)
store volatile %struct.ScmObj* %argslist49412$ae438352, %struct.ScmObj** %stackaddr$prim49639, align 8
%clofunc49640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43835)
musttail call tailcc void %clofunc49640(%struct.ScmObj* %ae43835, %struct.ScmObj* %argslist49412$ae438352)
ret void
}

define tailcc void @proc_clo$ae43835(%struct.ScmObj* %env$ae43835,%struct.ScmObj* %current_45args48990) {
%stackaddr$env-ref49641 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43835, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49641
%stackaddr$env-ref49642 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43835, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49642
%stackaddr$env-ref49643 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43835, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49643
%stackaddr$env-ref49644 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43835, i64 3)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref49644
%stackaddr$env-ref49645 = alloca %struct.ScmObj*, align 8
%_37length42905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43835, i64 4)
store %struct.ScmObj* %_37length42905, %struct.ScmObj** %stackaddr$env-ref49645
%stackaddr$prim49646 = alloca %struct.ScmObj*, align 8
%_95k43141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48990)
store volatile %struct.ScmObj* %_95k43141, %struct.ScmObj** %stackaddr$prim49646, align 8
%stackaddr$prim49647 = alloca %struct.ScmObj*, align 8
%current_45args48991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48990)
store volatile %struct.ScmObj* %current_45args48991, %struct.ScmObj** %stackaddr$prim49647, align 8
%stackaddr$prim49648 = alloca %struct.ScmObj*, align 8
%anf_45bind43046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48991)
store volatile %struct.ScmObj* %anf_45bind43046, %struct.ScmObj** %stackaddr$prim49648, align 8
%stackaddr$makeclosure49649 = alloca %struct.ScmObj*, align 8
%fptrToInt49650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43912 to i64
%ae43912 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49650)
store volatile %struct.ScmObj* %ae43912, %struct.ScmObj** %stackaddr$makeclosure49649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43912, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43912, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43912, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43912, %struct.ScmObj* %_37take42908, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae43912, %struct.ScmObj* %_37length42905, i64 4)
%argslist49396$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49651 = alloca %struct.ScmObj*, align 8
%argslist49396$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43046, %struct.ScmObj* %argslist49396$Ycmb428950)
store volatile %struct.ScmObj* %argslist49396$Ycmb428951, %struct.ScmObj** %stackaddr$prim49651, align 8
%stackaddr$prim49652 = alloca %struct.ScmObj*, align 8
%argslist49396$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43912, %struct.ScmObj* %argslist49396$Ycmb428951)
store volatile %struct.ScmObj* %argslist49396$Ycmb428952, %struct.ScmObj** %stackaddr$prim49652, align 8
%clofunc49653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49653(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49396$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae43912(%struct.ScmObj* %env$ae43912,%struct.ScmObj* %current_45args48993) {
%stackaddr$env-ref49654 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43912, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49654
%stackaddr$env-ref49655 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43912, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49655
%stackaddr$env-ref49656 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43912, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49656
%stackaddr$env-ref49657 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43912, i64 3)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref49657
%stackaddr$env-ref49658 = alloca %struct.ScmObj*, align 8
%_37length42905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43912, i64 4)
store %struct.ScmObj* %_37length42905, %struct.ScmObj** %stackaddr$env-ref49658
%stackaddr$prim49659 = alloca %struct.ScmObj*, align 8
%_95k43142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48993)
store volatile %struct.ScmObj* %_95k43142, %struct.ScmObj** %stackaddr$prim49659, align 8
%stackaddr$prim49660 = alloca %struct.ScmObj*, align 8
%current_45args48994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48993)
store volatile %struct.ScmObj* %current_45args48994, %struct.ScmObj** %stackaddr$prim49660, align 8
%stackaddr$prim49661 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48994)
store volatile %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$prim49661, align 8
%stackaddr$makeclosure49662 = alloca %struct.ScmObj*, align 8
%fptrToInt49663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43914 to i64
%ae43914 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49663)
store volatile %struct.ScmObj* %ae43914, %struct.ScmObj** %stackaddr$makeclosure49662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %_37map142912, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %Ycmb42895, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %_37take42908, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae43914, %struct.ScmObj* %_37length42905, i64 5)
%ae43915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49664 = alloca %struct.ScmObj*, align 8
%fptrToInt49665 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43916 to i64
%ae43916 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49665)
store volatile %struct.ScmObj* %ae43916, %struct.ScmObj** %stackaddr$makeclosure49664, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43916, %struct.ScmObj* %_37foldl142900, i64 0)
%argslist49395$ae439140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49666 = alloca %struct.ScmObj*, align 8
%argslist49395$ae439141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43916, %struct.ScmObj* %argslist49395$ae439140)
store volatile %struct.ScmObj* %argslist49395$ae439141, %struct.ScmObj** %stackaddr$prim49666, align 8
%stackaddr$prim49667 = alloca %struct.ScmObj*, align 8
%argslist49395$ae439142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43915, %struct.ScmObj* %argslist49395$ae439141)
store volatile %struct.ScmObj* %argslist49395$ae439142, %struct.ScmObj** %stackaddr$prim49667, align 8
%clofunc49668 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43914)
musttail call tailcc void %clofunc49668(%struct.ScmObj* %ae43914, %struct.ScmObj* %argslist49395$ae439142)
ret void
}

define tailcc void @proc_clo$ae43914(%struct.ScmObj* %env$ae43914,%struct.ScmObj* %current_45args48996) {
%stackaddr$env-ref49669 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49669
%stackaddr$env-ref49670 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49670
%stackaddr$env-ref49671 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 2)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49671
%stackaddr$env-ref49672 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 3)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49672
%stackaddr$env-ref49673 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 4)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref49673
%stackaddr$env-ref49674 = alloca %struct.ScmObj*, align 8
%_37length42905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43914, i64 5)
store %struct.ScmObj* %_37length42905, %struct.ScmObj** %stackaddr$env-ref49674
%stackaddr$prim49675 = alloca %struct.ScmObj*, align 8
%_95k43143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48996)
store volatile %struct.ScmObj* %_95k43143, %struct.ScmObj** %stackaddr$prim49675, align 8
%stackaddr$prim49676 = alloca %struct.ScmObj*, align 8
%current_45args48997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48996)
store volatile %struct.ScmObj* %current_45args48997, %struct.ScmObj** %stackaddr$prim49676, align 8
%stackaddr$prim49677 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48997)
store volatile %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$prim49677, align 8
%stackaddr$makeclosure49678 = alloca %struct.ScmObj*, align 8
%fptrToInt49679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43968 to i64
%ae43968 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49679)
store volatile %struct.ScmObj* %ae43968, %struct.ScmObj** %stackaddr$makeclosure49678, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43968, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43968, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43968, %struct.ScmObj* %_37map142912, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43968, %struct.ScmObj* %Ycmb42895, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae43968, %struct.ScmObj* %_37last42938, i64 4)
%ae43969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49680 = alloca %struct.ScmObj*, align 8
%fptrToInt49681 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43970 to i64
%ae43970 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49681)
store volatile %struct.ScmObj* %ae43970, %struct.ScmObj** %stackaddr$makeclosure49680, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43970, %struct.ScmObj* %_37take42908, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43970, %struct.ScmObj* %_37length42905, i64 1)
%argslist49381$ae439680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49682 = alloca %struct.ScmObj*, align 8
%argslist49381$ae439681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43970, %struct.ScmObj* %argslist49381$ae439680)
store volatile %struct.ScmObj* %argslist49381$ae439681, %struct.ScmObj** %stackaddr$prim49682, align 8
%stackaddr$prim49683 = alloca %struct.ScmObj*, align 8
%argslist49381$ae439682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43969, %struct.ScmObj* %argslist49381$ae439681)
store volatile %struct.ScmObj* %argslist49381$ae439682, %struct.ScmObj** %stackaddr$prim49683, align 8
%clofunc49684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43968)
musttail call tailcc void %clofunc49684(%struct.ScmObj* %ae43968, %struct.ScmObj* %argslist49381$ae439682)
ret void
}

define tailcc void @proc_clo$ae43968(%struct.ScmObj* %env$ae43968,%struct.ScmObj* %current_45args48999) {
%stackaddr$env-ref49685 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43968, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49685
%stackaddr$env-ref49686 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43968, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49686
%stackaddr$env-ref49687 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43968, i64 2)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref49687
%stackaddr$env-ref49688 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43968, i64 3)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49688
%stackaddr$env-ref49689 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43968, i64 4)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref49689
%stackaddr$prim49690 = alloca %struct.ScmObj*, align 8
%_95k43144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48999)
store volatile %struct.ScmObj* %_95k43144, %struct.ScmObj** %stackaddr$prim49690, align 8
%stackaddr$prim49691 = alloca %struct.ScmObj*, align 8
%current_45args49000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48999)
store volatile %struct.ScmObj* %current_45args49000, %struct.ScmObj** %stackaddr$prim49691, align 8
%stackaddr$prim49692 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49000)
store volatile %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$prim49692, align 8
%stackaddr$makeclosure49693 = alloca %struct.ScmObj*, align 8
%fptrToInt49694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43998 to i64
%ae43998 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49694)
store volatile %struct.ScmObj* %ae43998, %struct.ScmObj** %stackaddr$makeclosure49693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43998, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43998, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43998, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43998, %struct.ScmObj* %_37last42938, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae43998, %struct.ScmObj* %_37drop_45right42935, i64 4)
%ae43999 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49695 = alloca %struct.ScmObj*, align 8
%fptrToInt49696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44000 to i64
%ae44000 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49696)
store volatile %struct.ScmObj* %ae44000, %struct.ScmObj** %stackaddr$makeclosure49695, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44000, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44000, %struct.ScmObj* %_37map142912, i64 1)
%argslist49371$ae439980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49697 = alloca %struct.ScmObj*, align 8
%argslist49371$ae439981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44000, %struct.ScmObj* %argslist49371$ae439980)
store volatile %struct.ScmObj* %argslist49371$ae439981, %struct.ScmObj** %stackaddr$prim49697, align 8
%stackaddr$prim49698 = alloca %struct.ScmObj*, align 8
%argslist49371$ae439982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43999, %struct.ScmObj* %argslist49371$ae439981)
store volatile %struct.ScmObj* %argslist49371$ae439982, %struct.ScmObj** %stackaddr$prim49698, align 8
%clofunc49699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43998)
musttail call tailcc void %clofunc49699(%struct.ScmObj* %ae43998, %struct.ScmObj* %argslist49371$ae439982)
ret void
}

define tailcc void @proc_clo$ae43998(%struct.ScmObj* %env$ae43998,%struct.ScmObj* %current_45args49002) {
%stackaddr$env-ref49700 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43998, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49700
%stackaddr$env-ref49701 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43998, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49701
%stackaddr$env-ref49702 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43998, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49702
%stackaddr$env-ref49703 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43998, i64 3)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref49703
%stackaddr$env-ref49704 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43998, i64 4)
store %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$env-ref49704
%stackaddr$prim49705 = alloca %struct.ScmObj*, align 8
%_95k43145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49002)
store volatile %struct.ScmObj* %_95k43145, %struct.ScmObj** %stackaddr$prim49705, align 8
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%current_45args49003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49002)
store volatile %struct.ScmObj* %current_45args49003, %struct.ScmObj** %stackaddr$prim49706, align 8
%stackaddr$prim49707 = alloca %struct.ScmObj*, align 8
%anf_45bind43062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49003)
store volatile %struct.ScmObj* %anf_45bind43062, %struct.ScmObj** %stackaddr$prim49707, align 8
%stackaddr$makeclosure49708 = alloca %struct.ScmObj*, align 8
%fptrToInt49709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44382 to i64
%ae44382 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49709)
store volatile %struct.ScmObj* %ae44382, %struct.ScmObj** %stackaddr$makeclosure49708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44382, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44382, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44382, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44382, %struct.ScmObj* %_37last42938, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44382, %struct.ScmObj* %_37drop_45right42935, i64 4)
%argslist49311$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49710 = alloca %struct.ScmObj*, align 8
%argslist49311$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43062, %struct.ScmObj* %argslist49311$Ycmb428950)
store volatile %struct.ScmObj* %argslist49311$Ycmb428951, %struct.ScmObj** %stackaddr$prim49710, align 8
%stackaddr$prim49711 = alloca %struct.ScmObj*, align 8
%argslist49311$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44382, %struct.ScmObj* %argslist49311$Ycmb428951)
store volatile %struct.ScmObj* %argslist49311$Ycmb428952, %struct.ScmObj** %stackaddr$prim49711, align 8
%clofunc49712 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49712(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49311$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae44382(%struct.ScmObj* %env$ae44382,%struct.ScmObj* %current_45args49005) {
%stackaddr$env-ref49713 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44382, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49713
%stackaddr$env-ref49714 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44382, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49714
%stackaddr$env-ref49715 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44382, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49715
%stackaddr$env-ref49716 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44382, i64 3)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref49716
%stackaddr$env-ref49717 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44382, i64 4)
store %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$env-ref49717
%stackaddr$prim49718 = alloca %struct.ScmObj*, align 8
%_95k43146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49005)
store volatile %struct.ScmObj* %_95k43146, %struct.ScmObj** %stackaddr$prim49718, align 8
%stackaddr$prim49719 = alloca %struct.ScmObj*, align 8
%current_45args49006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49005)
store volatile %struct.ScmObj* %current_45args49006, %struct.ScmObj** %stackaddr$prim49719, align 8
%stackaddr$prim49720 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49006)
store volatile %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$prim49720, align 8
%stackaddr$makeclosure49721 = alloca %struct.ScmObj*, align 8
%fptrToInt49722 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44384 to i64
%ae44384 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49722)
store volatile %struct.ScmObj* %ae44384, %struct.ScmObj** %stackaddr$makeclosure49721, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %Ycmb42895, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %_37last42938, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %_37foldr42921, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44384, %struct.ScmObj* %_37drop_45right42935, i64 5)
%ae44385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49723 = alloca %struct.ScmObj*, align 8
%fptrToInt49724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44386 to i64
%ae44386 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49724)
store volatile %struct.ScmObj* %ae44386, %struct.ScmObj** %stackaddr$makeclosure49723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44386, %struct.ScmObj* %_37foldr142916, i64 0)
%argslist49310$ae443840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49725 = alloca %struct.ScmObj*, align 8
%argslist49310$ae443841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44386, %struct.ScmObj* %argslist49310$ae443840)
store volatile %struct.ScmObj* %argslist49310$ae443841, %struct.ScmObj** %stackaddr$prim49725, align 8
%stackaddr$prim49726 = alloca %struct.ScmObj*, align 8
%argslist49310$ae443842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44385, %struct.ScmObj* %argslist49310$ae443841)
store volatile %struct.ScmObj* %argslist49310$ae443842, %struct.ScmObj** %stackaddr$prim49726, align 8
%clofunc49727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44384)
musttail call tailcc void %clofunc49727(%struct.ScmObj* %ae44384, %struct.ScmObj* %argslist49310$ae443842)
ret void
}

define tailcc void @proc_clo$ae44384(%struct.ScmObj* %env$ae44384,%struct.ScmObj* %current_45args49008) {
%stackaddr$env-ref49728 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49728
%stackaddr$env-ref49729 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49729
%stackaddr$env-ref49730 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 2)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49730
%stackaddr$env-ref49731 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 3)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref49731
%stackaddr$env-ref49732 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 4)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref49732
%stackaddr$env-ref49733 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44384, i64 5)
store %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$env-ref49733
%stackaddr$prim49734 = alloca %struct.ScmObj*, align 8
%_95k43147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49008)
store volatile %struct.ScmObj* %_95k43147, %struct.ScmObj** %stackaddr$prim49734, align 8
%stackaddr$prim49735 = alloca %struct.ScmObj*, align 8
%current_45args49009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49008)
store volatile %struct.ScmObj* %current_45args49009, %struct.ScmObj** %stackaddr$prim49735, align 8
%stackaddr$prim49736 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49009)
store volatile %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$prim49736, align 8
%stackaddr$makeclosure49737 = alloca %struct.ScmObj*, align 8
%fptrToInt49738 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44461 to i64
%ae44461 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49738)
store volatile %struct.ScmObj* %ae44461, %struct.ScmObj** %stackaddr$makeclosure49737, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44461, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44461, %struct.ScmObj* %_37foldl142900, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44461, %struct.ScmObj* %_37map142947, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44461, %struct.ScmObj* %Ycmb42895, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44461, %struct.ScmObj* %_37foldr42921, i64 4)
%ae44462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49739 = alloca %struct.ScmObj*, align 8
%fptrToInt49740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44463 to i64
%ae44463 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49740)
store volatile %struct.ScmObj* %ae44463, %struct.ScmObj** %stackaddr$makeclosure49739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44463, %struct.ScmObj* %_37last42938, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44463, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44463, %struct.ScmObj* %_37drop_45right42935, i64 2)
%argslist49291$ae444610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49741 = alloca %struct.ScmObj*, align 8
%argslist49291$ae444611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44463, %struct.ScmObj* %argslist49291$ae444610)
store volatile %struct.ScmObj* %argslist49291$ae444611, %struct.ScmObj** %stackaddr$prim49741, align 8
%stackaddr$prim49742 = alloca %struct.ScmObj*, align 8
%argslist49291$ae444612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44462, %struct.ScmObj* %argslist49291$ae444611)
store volatile %struct.ScmObj* %argslist49291$ae444612, %struct.ScmObj** %stackaddr$prim49742, align 8
%clofunc49743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44461)
musttail call tailcc void %clofunc49743(%struct.ScmObj* %ae44461, %struct.ScmObj* %argslist49291$ae444612)
ret void
}

define tailcc void @proc_clo$ae44461(%struct.ScmObj* %env$ae44461,%struct.ScmObj* %current_45args49011) {
%stackaddr$env-ref49744 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44461, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref49744
%stackaddr$env-ref49745 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44461, i64 1)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49745
%stackaddr$env-ref49746 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44461, i64 2)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref49746
%stackaddr$env-ref49747 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44461, i64 3)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49747
%stackaddr$env-ref49748 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44461, i64 4)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref49748
%stackaddr$prim49749 = alloca %struct.ScmObj*, align 8
%_95k43148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49011)
store volatile %struct.ScmObj* %_95k43148, %struct.ScmObj** %stackaddr$prim49749, align 8
%stackaddr$prim49750 = alloca %struct.ScmObj*, align 8
%current_45args49012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49011)
store volatile %struct.ScmObj* %current_45args49012, %struct.ScmObj** %stackaddr$prim49750, align 8
%stackaddr$prim49751 = alloca %struct.ScmObj*, align 8
%_37map42942 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49012)
store volatile %struct.ScmObj* %_37map42942, %struct.ScmObj** %stackaddr$prim49751, align 8
%stackaddr$makeclosure49752 = alloca %struct.ScmObj*, align 8
%fptrToInt49753 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44607 to i64
%ae44607 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49753)
store volatile %struct.ScmObj* %ae44607, %struct.ScmObj** %stackaddr$makeclosure49752, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44607, %struct.ScmObj* %_37foldl142900, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44607, %struct.ScmObj* %Ycmb42895, i64 1)
%ae44608 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49754 = alloca %struct.ScmObj*, align 8
%fptrToInt49755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44609 to i64
%ae44609 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49755)
store volatile %struct.ScmObj* %ae44609, %struct.ScmObj** %stackaddr$makeclosure49754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44609, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44609, %struct.ScmObj* %_37map142947, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44609, %struct.ScmObj* %_37foldr42921, i64 2)
%argslist49274$ae446070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49756 = alloca %struct.ScmObj*, align 8
%argslist49274$ae446071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44609, %struct.ScmObj* %argslist49274$ae446070)
store volatile %struct.ScmObj* %argslist49274$ae446071, %struct.ScmObj** %stackaddr$prim49756, align 8
%stackaddr$prim49757 = alloca %struct.ScmObj*, align 8
%argslist49274$ae446072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44608, %struct.ScmObj* %argslist49274$ae446071)
store volatile %struct.ScmObj* %argslist49274$ae446072, %struct.ScmObj** %stackaddr$prim49757, align 8
%clofunc49758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44607)
musttail call tailcc void %clofunc49758(%struct.ScmObj* %ae44607, %struct.ScmObj* %argslist49274$ae446072)
ret void
}

define tailcc void @proc_clo$ae44607(%struct.ScmObj* %env$ae44607,%struct.ScmObj* %current_45args49014) {
%stackaddr$env-ref49759 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44607, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49759
%stackaddr$env-ref49760 = alloca %struct.ScmObj*, align 8
%Ycmb42895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44607, i64 1)
store %struct.ScmObj* %Ycmb42895, %struct.ScmObj** %stackaddr$env-ref49760
%stackaddr$prim49761 = alloca %struct.ScmObj*, align 8
%_95k43149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49014)
store volatile %struct.ScmObj* %_95k43149, %struct.ScmObj** %stackaddr$prim49761, align 8
%stackaddr$prim49762 = alloca %struct.ScmObj*, align 8
%current_45args49015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49014)
store volatile %struct.ScmObj* %current_45args49015, %struct.ScmObj** %stackaddr$prim49762, align 8
%stackaddr$prim49763 = alloca %struct.ScmObj*, align 8
%anf_45bind43082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49015)
store volatile %struct.ScmObj* %anf_45bind43082, %struct.ScmObj** %stackaddr$prim49763, align 8
%stackaddr$makeclosure49764 = alloca %struct.ScmObj*, align 8
%fptrToInt49765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44999 to i64
%ae44999 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49765)
store volatile %struct.ScmObj* %ae44999, %struct.ScmObj** %stackaddr$makeclosure49764, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44999, %struct.ScmObj* %_37foldl142900, i64 0)
%argslist49214$Ycmb428950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49766 = alloca %struct.ScmObj*, align 8
%argslist49214$Ycmb428951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43082, %struct.ScmObj* %argslist49214$Ycmb428950)
store volatile %struct.ScmObj* %argslist49214$Ycmb428951, %struct.ScmObj** %stackaddr$prim49766, align 8
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%argslist49214$Ycmb428952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44999, %struct.ScmObj* %argslist49214$Ycmb428951)
store volatile %struct.ScmObj* %argslist49214$Ycmb428952, %struct.ScmObj** %stackaddr$prim49767, align 8
%clofunc49768 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb42895)
musttail call tailcc void %clofunc49768(%struct.ScmObj* %Ycmb42895, %struct.ScmObj* %argslist49214$Ycmb428952)
ret void
}

define tailcc void @proc_clo$ae44999(%struct.ScmObj* %env$ae44999,%struct.ScmObj* %current_45args49017) {
%stackaddr$env-ref49769 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44999, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49769
%stackaddr$prim49770 = alloca %struct.ScmObj*, align 8
%_95k43150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49017)
store volatile %struct.ScmObj* %_95k43150, %struct.ScmObj** %stackaddr$prim49770, align 8
%stackaddr$prim49771 = alloca %struct.ScmObj*, align 8
%current_45args49018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49017)
store volatile %struct.ScmObj* %current_45args49018, %struct.ScmObj** %stackaddr$prim49771, align 8
%stackaddr$prim49772 = alloca %struct.ScmObj*, align 8
%_37foldl42998 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49018)
store volatile %struct.ScmObj* %_37foldl42998, %struct.ScmObj** %stackaddr$prim49772, align 8
%stackaddr$makeclosure49773 = alloca %struct.ScmObj*, align 8
%fptrToInt49774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45001 to i64
%ae45001 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49774)
store volatile %struct.ScmObj* %ae45001, %struct.ScmObj** %stackaddr$makeclosure49773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45001, %struct.ScmObj* %_37foldl142900, i64 0)
%ae45002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49775 = alloca %struct.ScmObj*, align 8
%fptrToInt49776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45003 to i64
%ae45003 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49776)
store volatile %struct.ScmObj* %ae45003, %struct.ScmObj** %stackaddr$makeclosure49775, align 8
%argslist49213$ae450010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49777 = alloca %struct.ScmObj*, align 8
%argslist49213$ae450011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45003, %struct.ScmObj* %argslist49213$ae450010)
store volatile %struct.ScmObj* %argslist49213$ae450011, %struct.ScmObj** %stackaddr$prim49777, align 8
%stackaddr$prim49778 = alloca %struct.ScmObj*, align 8
%argslist49213$ae450012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45002, %struct.ScmObj* %argslist49213$ae450011)
store volatile %struct.ScmObj* %argslist49213$ae450012, %struct.ScmObj** %stackaddr$prim49778, align 8
%clofunc49779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45001)
musttail call tailcc void %clofunc49779(%struct.ScmObj* %ae45001, %struct.ScmObj* %argslist49213$ae450012)
ret void
}

define tailcc void @proc_clo$ae45001(%struct.ScmObj* %env$ae45001,%struct.ScmObj* %current_45args49020) {
%stackaddr$env-ref49780 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45001, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49780
%stackaddr$prim49781 = alloca %struct.ScmObj*, align 8
%_95k43151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49020)
store volatile %struct.ScmObj* %_95k43151, %struct.ScmObj** %stackaddr$prim49781, align 8
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%current_45args49021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49020)
store volatile %struct.ScmObj* %current_45args49021, %struct.ScmObj** %stackaddr$prim49782, align 8
%stackaddr$prim49783 = alloca %struct.ScmObj*, align 8
%_37_6242995 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49021)
store volatile %struct.ScmObj* %_37_6242995, %struct.ScmObj** %stackaddr$prim49783, align 8
%stackaddr$makeclosure49784 = alloca %struct.ScmObj*, align 8
%fptrToInt49785 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45025 to i64
%ae45025 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49785)
store volatile %struct.ScmObj* %ae45025, %struct.ScmObj** %stackaddr$makeclosure49784, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45025, %struct.ScmObj* %_37foldl142900, i64 0)
%ae45026 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49786 = alloca %struct.ScmObj*, align 8
%fptrToInt49787 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45027 to i64
%ae45027 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49787)
store volatile %struct.ScmObj* %ae45027, %struct.ScmObj** %stackaddr$makeclosure49786, align 8
%argslist49207$ae450250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49788 = alloca %struct.ScmObj*, align 8
%argslist49207$ae450251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45027, %struct.ScmObj* %argslist49207$ae450250)
store volatile %struct.ScmObj* %argslist49207$ae450251, %struct.ScmObj** %stackaddr$prim49788, align 8
%stackaddr$prim49789 = alloca %struct.ScmObj*, align 8
%argslist49207$ae450252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45026, %struct.ScmObj* %argslist49207$ae450251)
store volatile %struct.ScmObj* %argslist49207$ae450252, %struct.ScmObj** %stackaddr$prim49789, align 8
%clofunc49790 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45025)
musttail call tailcc void %clofunc49790(%struct.ScmObj* %ae45025, %struct.ScmObj* %argslist49207$ae450252)
ret void
}

define tailcc void @proc_clo$ae45025(%struct.ScmObj* %env$ae45025,%struct.ScmObj* %current_45args49023) {
%stackaddr$env-ref49791 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45025, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49791
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%_95k43152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49023)
store volatile %struct.ScmObj* %_95k43152, %struct.ScmObj** %stackaddr$prim49792, align 8
%stackaddr$prim49793 = alloca %struct.ScmObj*, align 8
%current_45args49024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49023)
store volatile %struct.ScmObj* %current_45args49024, %struct.ScmObj** %stackaddr$prim49793, align 8
%stackaddr$prim49794 = alloca %struct.ScmObj*, align 8
%_37_62_6142992 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49024)
store volatile %struct.ScmObj* %_37_62_6142992, %struct.ScmObj** %stackaddr$prim49794, align 8
%ae45049 = call %struct.ScmObj* @const_init_int(i64 1)
%ae45050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49795 = alloca %struct.ScmObj*, align 8
%_37append42988 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae45049, %struct.ScmObj* %ae45050)
store volatile %struct.ScmObj* %_37append42988, %struct.ScmObj** %stackaddr$prim49795, align 8
%stackaddr$makeclosure49796 = alloca %struct.ScmObj*, align 8
%fptrToInt49797 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45051 to i64
%ae45051 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49797)
store volatile %struct.ScmObj* %ae45051, %struct.ScmObj** %stackaddr$makeclosure49796, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45051, %struct.ScmObj* %_37foldl142900, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45051, %struct.ScmObj* %_37append42988, i64 1)
%ae45052 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49798 = alloca %struct.ScmObj*, align 8
%fptrToInt49799 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45053 to i64
%ae45053 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49799)
store volatile %struct.ScmObj* %ae45053, %struct.ScmObj** %stackaddr$makeclosure49798, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45053, %struct.ScmObj* %_37append42988, i64 0)
%argslist49201$ae450510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49800 = alloca %struct.ScmObj*, align 8
%argslist49201$ae450511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45053, %struct.ScmObj* %argslist49201$ae450510)
store volatile %struct.ScmObj* %argslist49201$ae450511, %struct.ScmObj** %stackaddr$prim49800, align 8
%stackaddr$prim49801 = alloca %struct.ScmObj*, align 8
%argslist49201$ae450512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45052, %struct.ScmObj* %argslist49201$ae450511)
store volatile %struct.ScmObj* %argslist49201$ae450512, %struct.ScmObj** %stackaddr$prim49801, align 8
%clofunc49802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45051)
musttail call tailcc void %clofunc49802(%struct.ScmObj* %ae45051, %struct.ScmObj* %argslist49201$ae450512)
ret void
}

define tailcc void @proc_clo$ae45051(%struct.ScmObj* %env$ae45051,%struct.ScmObj* %current_45args49026) {
%stackaddr$env-ref49803 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45051, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49803
%stackaddr$env-ref49804 = alloca %struct.ScmObj*, align 8
%_37append42988 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45051, i64 1)
store %struct.ScmObj* %_37append42988, %struct.ScmObj** %stackaddr$env-ref49804
%stackaddr$prim49805 = alloca %struct.ScmObj*, align 8
%_95k43153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49026)
store volatile %struct.ScmObj* %_95k43153, %struct.ScmObj** %stackaddr$prim49805, align 8
%stackaddr$prim49806 = alloca %struct.ScmObj*, align 8
%current_45args49027 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49026)
store volatile %struct.ScmObj* %current_45args49027, %struct.ScmObj** %stackaddr$prim49806, align 8
%stackaddr$prim49807 = alloca %struct.ScmObj*, align 8
%anf_45bind43090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49027)
store volatile %struct.ScmObj* %anf_45bind43090, %struct.ScmObj** %stackaddr$prim49807, align 8
%ae45119 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49808 = alloca %struct.ScmObj*, align 8
%_95042989 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append42988, %struct.ScmObj* %ae45119, %struct.ScmObj* %anf_45bind43090)
store volatile %struct.ScmObj* %_95042989, %struct.ScmObj** %stackaddr$prim49808, align 8
%ae45122 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49809 = alloca %struct.ScmObj*, align 8
%_37append42987 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append42988, %struct.ScmObj* %ae45122)
store volatile %struct.ScmObj* %_37append42987, %struct.ScmObj** %stackaddr$prim49809, align 8
%stackaddr$makeclosure49810 = alloca %struct.ScmObj*, align 8
%fptrToInt49811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45123 to i64
%ae45123 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49811)
store volatile %struct.ScmObj* %ae45123, %struct.ScmObj** %stackaddr$makeclosure49810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45123, %struct.ScmObj* %_37foldl142900, i64 0)
%ae45124 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49812 = alloca %struct.ScmObj*, align 8
%fptrToInt49813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45125 to i64
%ae45125 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49813)
store volatile %struct.ScmObj* %ae45125, %struct.ScmObj** %stackaddr$makeclosure49812, align 8
%argslist49190$ae451230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49814 = alloca %struct.ScmObj*, align 8
%argslist49190$ae451231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45125, %struct.ScmObj* %argslist49190$ae451230)
store volatile %struct.ScmObj* %argslist49190$ae451231, %struct.ScmObj** %stackaddr$prim49814, align 8
%stackaddr$prim49815 = alloca %struct.ScmObj*, align 8
%argslist49190$ae451232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45124, %struct.ScmObj* %argslist49190$ae451231)
store volatile %struct.ScmObj* %argslist49190$ae451232, %struct.ScmObj** %stackaddr$prim49815, align 8
%clofunc49816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45123)
musttail call tailcc void %clofunc49816(%struct.ScmObj* %ae45123, %struct.ScmObj* %argslist49190$ae451232)
ret void
}

define tailcc void @proc_clo$ae45123(%struct.ScmObj* %env$ae45123,%struct.ScmObj* %current_45args49029) {
%stackaddr$env-ref49817 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45123, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49817
%stackaddr$prim49818 = alloca %struct.ScmObj*, align 8
%_95k43154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49029)
store volatile %struct.ScmObj* %_95k43154, %struct.ScmObj** %stackaddr$prim49818, align 8
%stackaddr$prim49819 = alloca %struct.ScmObj*, align 8
%current_45args49030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49029)
store volatile %struct.ScmObj* %current_45args49030, %struct.ScmObj** %stackaddr$prim49819, align 8
%stackaddr$prim49820 = alloca %struct.ScmObj*, align 8
%_37list_6342980 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49030)
store volatile %struct.ScmObj* %_37list_6342980, %struct.ScmObj** %stackaddr$prim49820, align 8
%stackaddr$makeclosure49821 = alloca %struct.ScmObj*, align 8
%fptrToInt49822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45539 to i64
%ae45539 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49822)
store volatile %struct.ScmObj* %ae45539, %struct.ScmObj** %stackaddr$makeclosure49821, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45539, %struct.ScmObj* %_37foldl142900, i64 0)
%ae45540 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49823 = alloca %struct.ScmObj*, align 8
%fptrToInt49824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45541 to i64
%ae45541 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49824)
store volatile %struct.ScmObj* %ae45541, %struct.ScmObj** %stackaddr$makeclosure49823, align 8
%argslist49165$ae455390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49825 = alloca %struct.ScmObj*, align 8
%argslist49165$ae455391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45541, %struct.ScmObj* %argslist49165$ae455390)
store volatile %struct.ScmObj* %argslist49165$ae455391, %struct.ScmObj** %stackaddr$prim49825, align 8
%stackaddr$prim49826 = alloca %struct.ScmObj*, align 8
%argslist49165$ae455392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45540, %struct.ScmObj* %argslist49165$ae455391)
store volatile %struct.ScmObj* %argslist49165$ae455392, %struct.ScmObj** %stackaddr$prim49826, align 8
%clofunc49827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45539)
musttail call tailcc void %clofunc49827(%struct.ScmObj* %ae45539, %struct.ScmObj* %argslist49165$ae455392)
ret void
}

define tailcc void @proc_clo$ae45539(%struct.ScmObj* %env$ae45539,%struct.ScmObj* %current_45args49032) {
%stackaddr$env-ref49828 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45539, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49828
%stackaddr$prim49829 = alloca %struct.ScmObj*, align 8
%_95k43155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49032)
store volatile %struct.ScmObj* %_95k43155, %struct.ScmObj** %stackaddr$prim49829, align 8
%stackaddr$prim49830 = alloca %struct.ScmObj*, align 8
%current_45args49033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49032)
store volatile %struct.ScmObj* %current_45args49033, %struct.ScmObj** %stackaddr$prim49830, align 8
%stackaddr$prim49831 = alloca %struct.ScmObj*, align 8
%_37drop42971 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49033)
store volatile %struct.ScmObj* %_37drop42971, %struct.ScmObj** %stackaddr$prim49831, align 8
%stackaddr$makeclosure49832 = alloca %struct.ScmObj*, align 8
%fptrToInt49833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46075 to i64
%ae46075 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49833)
store volatile %struct.ScmObj* %ae46075, %struct.ScmObj** %stackaddr$makeclosure49832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46075, %struct.ScmObj* %_37foldl142900, i64 0)
%ae46076 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49834 = alloca %struct.ScmObj*, align 8
%fptrToInt49835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46077 to i64
%ae46077 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49835)
store volatile %struct.ScmObj* %ae46077, %struct.ScmObj** %stackaddr$makeclosure49834, align 8
%argslist49141$ae460750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49836 = alloca %struct.ScmObj*, align 8
%argslist49141$ae460751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46077, %struct.ScmObj* %argslist49141$ae460750)
store volatile %struct.ScmObj* %argslist49141$ae460751, %struct.ScmObj** %stackaddr$prim49836, align 8
%stackaddr$prim49837 = alloca %struct.ScmObj*, align 8
%argslist49141$ae460752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46076, %struct.ScmObj* %argslist49141$ae460751)
store volatile %struct.ScmObj* %argslist49141$ae460752, %struct.ScmObj** %stackaddr$prim49837, align 8
%clofunc49838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46075)
musttail call tailcc void %clofunc49838(%struct.ScmObj* %ae46075, %struct.ScmObj* %argslist49141$ae460752)
ret void
}

define tailcc void @proc_clo$ae46075(%struct.ScmObj* %env$ae46075,%struct.ScmObj* %current_45args49035) {
%stackaddr$env-ref49839 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46075, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref49839
%stackaddr$prim49840 = alloca %struct.ScmObj*, align 8
%_95k43156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49035)
store volatile %struct.ScmObj* %_95k43156, %struct.ScmObj** %stackaddr$prim49840, align 8
%stackaddr$prim49841 = alloca %struct.ScmObj*, align 8
%current_45args49036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49035)
store volatile %struct.ScmObj* %current_45args49036, %struct.ScmObj** %stackaddr$prim49841, align 8
%stackaddr$prim49842 = alloca %struct.ScmObj*, align 8
%_37memv42964 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49036)
store volatile %struct.ScmObj* %_37memv42964, %struct.ScmObj** %stackaddr$prim49842, align 8
%stackaddr$makeclosure49843 = alloca %struct.ScmObj*, align 8
%fptrToInt49844 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46479 to i64
%ae46479 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49844)
store volatile %struct.ScmObj* %ae46479, %struct.ScmObj** %stackaddr$makeclosure49843, align 8
%ae46480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49845 = alloca %struct.ScmObj*, align 8
%fptrToInt49846 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46481 to i64
%ae46481 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49846)
store volatile %struct.ScmObj* %ae46481, %struct.ScmObj** %stackaddr$makeclosure49845, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46481, %struct.ScmObj* %_37foldl142900, i64 0)
%argslist49115$ae464790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49847 = alloca %struct.ScmObj*, align 8
%argslist49115$ae464791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46481, %struct.ScmObj* %argslist49115$ae464790)
store volatile %struct.ScmObj* %argslist49115$ae464791, %struct.ScmObj** %stackaddr$prim49847, align 8
%stackaddr$prim49848 = alloca %struct.ScmObj*, align 8
%argslist49115$ae464792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46480, %struct.ScmObj* %argslist49115$ae464791)
store volatile %struct.ScmObj* %argslist49115$ae464792, %struct.ScmObj** %stackaddr$prim49848, align 8
%clofunc49849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46479)
musttail call tailcc void %clofunc49849(%struct.ScmObj* %ae46479, %struct.ScmObj* %argslist49115$ae464792)
ret void
}

define tailcc void @proc_clo$ae46479(%struct.ScmObj* %env$ae46479,%struct.ScmObj* %current_45args49038) {
%stackaddr$prim49850 = alloca %struct.ScmObj*, align 8
%_95k43157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49038)
store volatile %struct.ScmObj* %_95k43157, %struct.ScmObj** %stackaddr$prim49850, align 8
%stackaddr$prim49851 = alloca %struct.ScmObj*, align 8
%current_45args49039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49038)
store volatile %struct.ScmObj* %current_45args49039, %struct.ScmObj** %stackaddr$prim49851, align 8
%stackaddr$prim49852 = alloca %struct.ScmObj*, align 8
%_37_4742960 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49039)
store volatile %struct.ScmObj* %_37_4742960, %struct.ScmObj** %stackaddr$prim49852, align 8
%stackaddr$makeclosure49853 = alloca %struct.ScmObj*, align 8
%fptrToInt49854 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46577 to i64
%ae46577 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49854)
store volatile %struct.ScmObj* %ae46577, %struct.ScmObj** %stackaddr$makeclosure49853, align 8
%ae46578 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49855 = alloca %struct.ScmObj*, align 8
%fptrToInt49856 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46579 to i64
%ae46579 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49856)
store volatile %struct.ScmObj* %ae46579, %struct.ScmObj** %stackaddr$makeclosure49855, align 8
%argslist49102$ae465770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49857 = alloca %struct.ScmObj*, align 8
%argslist49102$ae465771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46579, %struct.ScmObj* %argslist49102$ae465770)
store volatile %struct.ScmObj* %argslist49102$ae465771, %struct.ScmObj** %stackaddr$prim49857, align 8
%stackaddr$prim49858 = alloca %struct.ScmObj*, align 8
%argslist49102$ae465772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46578, %struct.ScmObj* %argslist49102$ae465771)
store volatile %struct.ScmObj* %argslist49102$ae465772, %struct.ScmObj** %stackaddr$prim49858, align 8
%clofunc49859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46577)
musttail call tailcc void %clofunc49859(%struct.ScmObj* %ae46577, %struct.ScmObj* %argslist49102$ae465772)
ret void
}

define tailcc void @proc_clo$ae46577(%struct.ScmObj* %env$ae46577,%struct.ScmObj* %current_45args49041) {
%stackaddr$prim49860 = alloca %struct.ScmObj*, align 8
%_95k43158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49041)
store volatile %struct.ScmObj* %_95k43158, %struct.ScmObj** %stackaddr$prim49860, align 8
%stackaddr$prim49861 = alloca %struct.ScmObj*, align 8
%current_45args49042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49041)
store volatile %struct.ScmObj* %current_45args49042, %struct.ScmObj** %stackaddr$prim49861, align 8
%stackaddr$prim49862 = alloca %struct.ScmObj*, align 8
%_37first42958 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49042)
store volatile %struct.ScmObj* %_37first42958, %struct.ScmObj** %stackaddr$prim49862, align 8
%stackaddr$makeclosure49863 = alloca %struct.ScmObj*, align 8
%fptrToInt49864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46597 to i64
%ae46597 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49864)
store volatile %struct.ScmObj* %ae46597, %struct.ScmObj** %stackaddr$makeclosure49863, align 8
%ae46598 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49865 = alloca %struct.ScmObj*, align 8
%fptrToInt49866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46599 to i64
%ae46599 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49866)
store volatile %struct.ScmObj* %ae46599, %struct.ScmObj** %stackaddr$makeclosure49865, align 8
%argslist49097$ae465970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49867 = alloca %struct.ScmObj*, align 8
%argslist49097$ae465971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46599, %struct.ScmObj* %argslist49097$ae465970)
store volatile %struct.ScmObj* %argslist49097$ae465971, %struct.ScmObj** %stackaddr$prim49867, align 8
%stackaddr$prim49868 = alloca %struct.ScmObj*, align 8
%argslist49097$ae465972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46598, %struct.ScmObj* %argslist49097$ae465971)
store volatile %struct.ScmObj* %argslist49097$ae465972, %struct.ScmObj** %stackaddr$prim49868, align 8
%clofunc49869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46597)
musttail call tailcc void %clofunc49869(%struct.ScmObj* %ae46597, %struct.ScmObj* %argslist49097$ae465972)
ret void
}

define tailcc void @proc_clo$ae46597(%struct.ScmObj* %env$ae46597,%struct.ScmObj* %current_45args49044) {
%stackaddr$prim49870 = alloca %struct.ScmObj*, align 8
%_95k43159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49044)
store volatile %struct.ScmObj* %_95k43159, %struct.ScmObj** %stackaddr$prim49870, align 8
%stackaddr$prim49871 = alloca %struct.ScmObj*, align 8
%current_45args49045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49044)
store volatile %struct.ScmObj* %current_45args49045, %struct.ScmObj** %stackaddr$prim49871, align 8
%stackaddr$prim49872 = alloca %struct.ScmObj*, align 8
%_37second42956 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49045)
store volatile %struct.ScmObj* %_37second42956, %struct.ScmObj** %stackaddr$prim49872, align 8
%stackaddr$makeclosure49873 = alloca %struct.ScmObj*, align 8
%fptrToInt49874 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46619 to i64
%ae46619 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49874)
store volatile %struct.ScmObj* %ae46619, %struct.ScmObj** %stackaddr$makeclosure49873, align 8
%ae46620 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49875 = alloca %struct.ScmObj*, align 8
%fptrToInt49876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46621 to i64
%ae46621 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49876)
store volatile %struct.ScmObj* %ae46621, %struct.ScmObj** %stackaddr$makeclosure49875, align 8
%argslist49092$ae466190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49877 = alloca %struct.ScmObj*, align 8
%argslist49092$ae466191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46621, %struct.ScmObj* %argslist49092$ae466190)
store volatile %struct.ScmObj* %argslist49092$ae466191, %struct.ScmObj** %stackaddr$prim49877, align 8
%stackaddr$prim49878 = alloca %struct.ScmObj*, align 8
%argslist49092$ae466192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46620, %struct.ScmObj* %argslist49092$ae466191)
store volatile %struct.ScmObj* %argslist49092$ae466192, %struct.ScmObj** %stackaddr$prim49878, align 8
%clofunc49879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46619)
musttail call tailcc void %clofunc49879(%struct.ScmObj* %ae46619, %struct.ScmObj* %argslist49092$ae466192)
ret void
}

define tailcc void @proc_clo$ae46619(%struct.ScmObj* %env$ae46619,%struct.ScmObj* %current_45args49047) {
%stackaddr$prim49880 = alloca %struct.ScmObj*, align 8
%_95k43160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49047)
store volatile %struct.ScmObj* %_95k43160, %struct.ScmObj** %stackaddr$prim49880, align 8
%stackaddr$prim49881 = alloca %struct.ScmObj*, align 8
%current_45args49048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49047)
store volatile %struct.ScmObj* %current_45args49048, %struct.ScmObj** %stackaddr$prim49881, align 8
%stackaddr$prim49882 = alloca %struct.ScmObj*, align 8
%_37third42954 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49048)
store volatile %struct.ScmObj* %_37third42954, %struct.ScmObj** %stackaddr$prim49882, align 8
%stackaddr$makeclosure49883 = alloca %struct.ScmObj*, align 8
%fptrToInt49884 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46643 to i64
%ae46643 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49884)
store volatile %struct.ScmObj* %ae46643, %struct.ScmObj** %stackaddr$makeclosure49883, align 8
%ae46644 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49885 = alloca %struct.ScmObj*, align 8
%fptrToInt49886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46645 to i64
%ae46645 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49886)
store volatile %struct.ScmObj* %ae46645, %struct.ScmObj** %stackaddr$makeclosure49885, align 8
%argslist49087$ae466430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49887 = alloca %struct.ScmObj*, align 8
%argslist49087$ae466431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46645, %struct.ScmObj* %argslist49087$ae466430)
store volatile %struct.ScmObj* %argslist49087$ae466431, %struct.ScmObj** %stackaddr$prim49887, align 8
%stackaddr$prim49888 = alloca %struct.ScmObj*, align 8
%argslist49087$ae466432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46644, %struct.ScmObj* %argslist49087$ae466431)
store volatile %struct.ScmObj* %argslist49087$ae466432, %struct.ScmObj** %stackaddr$prim49888, align 8
%clofunc49889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46643)
musttail call tailcc void %clofunc49889(%struct.ScmObj* %ae46643, %struct.ScmObj* %argslist49087$ae466432)
ret void
}

define tailcc void @proc_clo$ae46643(%struct.ScmObj* %env$ae46643,%struct.ScmObj* %current_45args49050) {
%stackaddr$prim49890 = alloca %struct.ScmObj*, align 8
%_95k43161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49050)
store volatile %struct.ScmObj* %_95k43161, %struct.ScmObj** %stackaddr$prim49890, align 8
%stackaddr$prim49891 = alloca %struct.ScmObj*, align 8
%current_45args49051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49050)
store volatile %struct.ScmObj* %current_45args49051, %struct.ScmObj** %stackaddr$prim49891, align 8
%stackaddr$prim49892 = alloca %struct.ScmObj*, align 8
%_37fourth42952 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49051)
store volatile %struct.ScmObj* %_37fourth42952, %struct.ScmObj** %stackaddr$prim49892, align 8
%stackaddr$makeclosure49893 = alloca %struct.ScmObj*, align 8
%fptrToInt49894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46669 to i64
%ae46669 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49894)
store volatile %struct.ScmObj* %ae46669, %struct.ScmObj** %stackaddr$makeclosure49893, align 8
%ae46670 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49895 = alloca %struct.ScmObj*, align 8
%fptrToInt49896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46671 to i64
%ae46671 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49896)
store volatile %struct.ScmObj* %ae46671, %struct.ScmObj** %stackaddr$makeclosure49895, align 8
%argslist49082$ae466690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49897 = alloca %struct.ScmObj*, align 8
%argslist49082$ae466691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46671, %struct.ScmObj* %argslist49082$ae466690)
store volatile %struct.ScmObj* %argslist49082$ae466691, %struct.ScmObj** %stackaddr$prim49897, align 8
%stackaddr$prim49898 = alloca %struct.ScmObj*, align 8
%argslist49082$ae466692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46670, %struct.ScmObj* %argslist49082$ae466691)
store volatile %struct.ScmObj* %argslist49082$ae466692, %struct.ScmObj** %stackaddr$prim49898, align 8
%clofunc49899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46669)
musttail call tailcc void %clofunc49899(%struct.ScmObj* %ae46669, %struct.ScmObj* %argslist49082$ae466692)
ret void
}

define tailcc void @proc_clo$ae46669(%struct.ScmObj* %env$ae46669,%struct.ScmObj* %current_45args49053) {
%stackaddr$prim49900 = alloca %struct.ScmObj*, align 8
%_95k43162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49053)
store volatile %struct.ScmObj* %_95k43162, %struct.ScmObj** %stackaddr$prim49900, align 8
%stackaddr$prim49901 = alloca %struct.ScmObj*, align 8
%current_45args49054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49053)
store volatile %struct.ScmObj* %current_45args49054, %struct.ScmObj** %stackaddr$prim49901, align 8
%stackaddr$prim49902 = alloca %struct.ScmObj*, align 8
%promise_6343013 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49054)
store volatile %struct.ScmObj* %promise_6343013, %struct.ScmObj** %stackaddr$prim49902, align 8
%ae46756 = call %struct.ScmObj* @const_init_false()
%truthy$cmp49903 = call i64 @is_truthy_value(%struct.ScmObj* %ae46756)
%cmp$cmp49903 = icmp eq i64 %truthy$cmp49903, 1
br i1 %cmp$cmp49903, label %truebranch$cmp49903, label %falsebranch$cmp49903
truebranch$cmp49903:
%stackaddr$makeclosure49904 = alloca %struct.ScmObj*, align 8
%fptrToInt49905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46757 to i64
%ae46757 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49905)
store volatile %struct.ScmObj* %ae46757, %struct.ScmObj** %stackaddr$makeclosure49904, align 8
%ae46758 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46759 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist49060$ae467570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49906 = alloca %struct.ScmObj*, align 8
%argslist49060$ae467571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46759, %struct.ScmObj* %argslist49060$ae467570)
store volatile %struct.ScmObj* %argslist49060$ae467571, %struct.ScmObj** %stackaddr$prim49906, align 8
%stackaddr$prim49907 = alloca %struct.ScmObj*, align 8
%argslist49060$ae467572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46758, %struct.ScmObj* %argslist49060$ae467571)
store volatile %struct.ScmObj* %argslist49060$ae467572, %struct.ScmObj** %stackaddr$prim49907, align 8
%clofunc49908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46757)
musttail call tailcc void %clofunc49908(%struct.ScmObj* %ae46757, %struct.ScmObj* %argslist49060$ae467572)
ret void
falsebranch$cmp49903:
%ae46772 = call %struct.ScmObj* @const_init_false()
%truthy$cmp49909 = call i64 @is_truthy_value(%struct.ScmObj* %ae46772)
%cmp$cmp49909 = icmp eq i64 %truthy$cmp49909, 1
br i1 %cmp$cmp49909, label %truebranch$cmp49909, label %falsebranch$cmp49909
truebranch$cmp49909:
%stackaddr$makeclosure49910 = alloca %struct.ScmObj*, align 8
%fptrToInt49911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46773 to i64
%ae46773 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49911)
store volatile %struct.ScmObj* %ae46773, %struct.ScmObj** %stackaddr$makeclosure49910, align 8
%ae46774 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46775 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist49065$ae467730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49912 = alloca %struct.ScmObj*, align 8
%argslist49065$ae467731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46775, %struct.ScmObj* %argslist49065$ae467730)
store volatile %struct.ScmObj* %argslist49065$ae467731, %struct.ScmObj** %stackaddr$prim49912, align 8
%stackaddr$prim49913 = alloca %struct.ScmObj*, align 8
%argslist49065$ae467732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46774, %struct.ScmObj* %argslist49065$ae467731)
store volatile %struct.ScmObj* %argslist49065$ae467732, %struct.ScmObj** %stackaddr$prim49913, align 8
%clofunc49914 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46773)
musttail call tailcc void %clofunc49914(%struct.ScmObj* %ae46773, %struct.ScmObj* %argslist49065$ae467732)
ret void
falsebranch$cmp49909:
%ae46788 = call %struct.ScmObj* @const_init_true()
%truthy$cmp49915 = call i64 @is_truthy_value(%struct.ScmObj* %ae46788)
%cmp$cmp49915 = icmp eq i64 %truthy$cmp49915, 1
br i1 %cmp$cmp49915, label %truebranch$cmp49915, label %falsebranch$cmp49915
truebranch$cmp49915:
%stackaddr$makeclosure49916 = alloca %struct.ScmObj*, align 8
%fptrToInt49917 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46789 to i64
%ae46789 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49917)
store volatile %struct.ScmObj* %ae46789, %struct.ScmObj** %stackaddr$makeclosure49916, align 8
%ae46790 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46791 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist49070$ae467890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49918 = alloca %struct.ScmObj*, align 8
%argslist49070$ae467891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46791, %struct.ScmObj* %argslist49070$ae467890)
store volatile %struct.ScmObj* %argslist49070$ae467891, %struct.ScmObj** %stackaddr$prim49918, align 8
%stackaddr$prim49919 = alloca %struct.ScmObj*, align 8
%argslist49070$ae467892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46790, %struct.ScmObj* %argslist49070$ae467891)
store volatile %struct.ScmObj* %argslist49070$ae467892, %struct.ScmObj** %stackaddr$prim49919, align 8
%clofunc49920 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46789)
musttail call tailcc void %clofunc49920(%struct.ScmObj* %ae46789, %struct.ScmObj* %argslist49070$ae467892)
ret void
falsebranch$cmp49915:
%stackaddr$makeclosure49921 = alloca %struct.ScmObj*, align 8
%fptrToInt49922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46804 to i64
%ae46804 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49922)
store volatile %struct.ScmObj* %ae46804, %struct.ScmObj** %stackaddr$makeclosure49921, align 8
%ae46805 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46806 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist49075$ae468040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49923 = alloca %struct.ScmObj*, align 8
%argslist49075$ae468041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46806, %struct.ScmObj* %argslist49075$ae468040)
store volatile %struct.ScmObj* %argslist49075$ae468041, %struct.ScmObj** %stackaddr$prim49923, align 8
%stackaddr$prim49924 = alloca %struct.ScmObj*, align 8
%argslist49075$ae468042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46805, %struct.ScmObj* %argslist49075$ae468041)
store volatile %struct.ScmObj* %argslist49075$ae468042, %struct.ScmObj** %stackaddr$prim49924, align 8
%clofunc49925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46804)
musttail call tailcc void %clofunc49925(%struct.ScmObj* %ae46804, %struct.ScmObj* %argslist49075$ae468042)
ret void
}

define tailcc void @proc_clo$ae46757(%struct.ScmObj* %env$ae46757,%struct.ScmObj* %current_45args49056) {
%stackaddr$prim49926 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49056)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49926, align 8
%stackaddr$prim49927 = alloca %struct.ScmObj*, align 8
%current_45args49057 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49056)
store volatile %struct.ScmObj* %current_45args49057, %struct.ScmObj** %stackaddr$prim49927, align 8
%stackaddr$prim49928 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49057)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49928, align 8
%stackaddr$prim49929 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49929, align 8
%argslist49059$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49930 = alloca %struct.ScmObj*, align 8
%argslist49059$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist49059$k0)
store volatile %struct.ScmObj* %argslist49059$k1, %struct.ScmObj** %stackaddr$prim49930, align 8
%clofunc49931 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49931(%struct.ScmObj* %k, %struct.ScmObj* %argslist49059$k1)
ret void
}

define tailcc void @proc_clo$ae46773(%struct.ScmObj* %env$ae46773,%struct.ScmObj* %current_45args49061) {
%stackaddr$prim49932 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49061)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49932, align 8
%stackaddr$prim49933 = alloca %struct.ScmObj*, align 8
%current_45args49062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49061)
store volatile %struct.ScmObj* %current_45args49062, %struct.ScmObj** %stackaddr$prim49933, align 8
%stackaddr$prim49934 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49062)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49934, align 8
%stackaddr$prim49935 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49935, align 8
%argslist49064$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49936 = alloca %struct.ScmObj*, align 8
%argslist49064$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist49064$k0)
store volatile %struct.ScmObj* %argslist49064$k1, %struct.ScmObj** %stackaddr$prim49936, align 8
%clofunc49937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49937(%struct.ScmObj* %k, %struct.ScmObj* %argslist49064$k1)
ret void
}

define tailcc void @proc_clo$ae46789(%struct.ScmObj* %env$ae46789,%struct.ScmObj* %current_45args49066) {
%stackaddr$prim49938 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49066)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49938, align 8
%stackaddr$prim49939 = alloca %struct.ScmObj*, align 8
%current_45args49067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49066)
store volatile %struct.ScmObj* %current_45args49067, %struct.ScmObj** %stackaddr$prim49939, align 8
%stackaddr$prim49940 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49067)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49940, align 8
%stackaddr$prim49941 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49941, align 8
%argslist49069$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49942 = alloca %struct.ScmObj*, align 8
%argslist49069$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist49069$k0)
store volatile %struct.ScmObj* %argslist49069$k1, %struct.ScmObj** %stackaddr$prim49942, align 8
%clofunc49943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49943(%struct.ScmObj* %k, %struct.ScmObj* %argslist49069$k1)
ret void
}

define tailcc void @proc_clo$ae46804(%struct.ScmObj* %env$ae46804,%struct.ScmObj* %current_45args49071) {
%stackaddr$prim49944 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49071)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49944, align 8
%stackaddr$prim49945 = alloca %struct.ScmObj*, align 8
%current_45args49072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49071)
store volatile %struct.ScmObj* %current_45args49072, %struct.ScmObj** %stackaddr$prim49945, align 8
%stackaddr$prim49946 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49072)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49946, align 8
%stackaddr$prim49947 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49947, align 8
%argslist49074$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49948 = alloca %struct.ScmObj*, align 8
%argslist49074$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist49074$k0)
store volatile %struct.ScmObj* %argslist49074$k1, %struct.ScmObj** %stackaddr$prim49948, align 8
%clofunc49949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49949(%struct.ScmObj* %k, %struct.ScmObj* %argslist49074$k1)
ret void
}

define tailcc void @proc_clo$ae46671(%struct.ScmObj* %env$ae46671,%struct.ScmObj* %current_45args49076) {
%stackaddr$prim49950 = alloca %struct.ScmObj*, align 8
%k43163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49076)
store volatile %struct.ScmObj* %k43163, %struct.ScmObj** %stackaddr$prim49950, align 8
%stackaddr$prim49951 = alloca %struct.ScmObj*, align 8
%current_45args49077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49076)
store volatile %struct.ScmObj* %current_45args49077, %struct.ScmObj** %stackaddr$prim49951, align 8
%stackaddr$prim49952 = alloca %struct.ScmObj*, align 8
%thunk43014 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49077)
store volatile %struct.ScmObj* %thunk43014, %struct.ScmObj** %stackaddr$prim49952, align 8
%stackaddr$prim49953 = alloca %struct.ScmObj*, align 8
%anf_45bind43126 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk43014)
store volatile %struct.ScmObj* %anf_45bind43126, %struct.ScmObj** %stackaddr$prim49953, align 8
%truthy$cmp49954 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43126)
%cmp$cmp49954 = icmp eq i64 %truthy$cmp49954, 1
br i1 %cmp$cmp49954, label %truebranch$cmp49954, label %falsebranch$cmp49954
truebranch$cmp49954:
%stackaddr$prim49955 = alloca %struct.ScmObj*, align 8
%anf_45bind43127 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk43014)
store volatile %struct.ScmObj* %anf_45bind43127, %struct.ScmObj** %stackaddr$prim49955, align 8
%ae46676 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49956 = alloca %struct.ScmObj*, align 8
%anf_45bind43128 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind43127, %struct.ScmObj* %ae46676)
store volatile %struct.ScmObj* %anf_45bind43128, %struct.ScmObj** %stackaddr$prim49956, align 8
%truthy$cmp49957 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43128)
%cmp$cmp49957 = icmp eq i64 %truthy$cmp49957, 1
br i1 %cmp$cmp49957, label %truebranch$cmp49957, label %falsebranch$cmp49957
truebranch$cmp49957:
%ae46679 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49958 = alloca %struct.ScmObj*, align 8
%anf_45bind43129 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk43014, %struct.ScmObj* %ae46679)
store volatile %struct.ScmObj* %anf_45bind43129, %struct.ScmObj** %stackaddr$prim49958, align 8
%ae46681 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4668149959, i32 0, i32 0))
%stackaddr$prim49960 = alloca %struct.ScmObj*, align 8
%cpsprim43164 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind43129, %struct.ScmObj* %ae46681)
store volatile %struct.ScmObj* %cpsprim43164, %struct.ScmObj** %stackaddr$prim49960, align 8
%ae46683 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49079$k431630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49961 = alloca %struct.ScmObj*, align 8
%argslist49079$k431631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43164, %struct.ScmObj* %argslist49079$k431630)
store volatile %struct.ScmObj* %argslist49079$k431631, %struct.ScmObj** %stackaddr$prim49961, align 8
%stackaddr$prim49962 = alloca %struct.ScmObj*, align 8
%argslist49079$k431632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46683, %struct.ScmObj* %argslist49079$k431631)
store volatile %struct.ScmObj* %argslist49079$k431632, %struct.ScmObj** %stackaddr$prim49962, align 8
%clofunc49963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43163)
musttail call tailcc void %clofunc49963(%struct.ScmObj* %k43163, %struct.ScmObj* %argslist49079$k431632)
ret void
falsebranch$cmp49957:
%ae46701 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46702 = call %struct.ScmObj* @const_init_false()
%argslist49080$k431630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49964 = alloca %struct.ScmObj*, align 8
%argslist49080$k431631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46702, %struct.ScmObj* %argslist49080$k431630)
store volatile %struct.ScmObj* %argslist49080$k431631, %struct.ScmObj** %stackaddr$prim49964, align 8
%stackaddr$prim49965 = alloca %struct.ScmObj*, align 8
%argslist49080$k431632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46701, %struct.ScmObj* %argslist49080$k431631)
store volatile %struct.ScmObj* %argslist49080$k431632, %struct.ScmObj** %stackaddr$prim49965, align 8
%clofunc49966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43163)
musttail call tailcc void %clofunc49966(%struct.ScmObj* %k43163, %struct.ScmObj* %argslist49080$k431632)
ret void
falsebranch$cmp49954:
%ae46723 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46724 = call %struct.ScmObj* @const_init_false()
%argslist49081$k431630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49967 = alloca %struct.ScmObj*, align 8
%argslist49081$k431631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46724, %struct.ScmObj* %argslist49081$k431630)
store volatile %struct.ScmObj* %argslist49081$k431631, %struct.ScmObj** %stackaddr$prim49967, align 8
%stackaddr$prim49968 = alloca %struct.ScmObj*, align 8
%argslist49081$k431632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46723, %struct.ScmObj* %argslist49081$k431631)
store volatile %struct.ScmObj* %argslist49081$k431632, %struct.ScmObj** %stackaddr$prim49968, align 8
%clofunc49969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43163)
musttail call tailcc void %clofunc49969(%struct.ScmObj* %k43163, %struct.ScmObj* %argslist49081$k431632)
ret void
}

define tailcc void @proc_clo$ae46645(%struct.ScmObj* %env$ae46645,%struct.ScmObj* %current_45args49083) {
%stackaddr$prim49970 = alloca %struct.ScmObj*, align 8
%k43165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49083)
store volatile %struct.ScmObj* %k43165, %struct.ScmObj** %stackaddr$prim49970, align 8
%stackaddr$prim49971 = alloca %struct.ScmObj*, align 8
%current_45args49084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49083)
store volatile %struct.ScmObj* %current_45args49084, %struct.ScmObj** %stackaddr$prim49971, align 8
%stackaddr$prim49972 = alloca %struct.ScmObj*, align 8
%x42953 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49084)
store volatile %struct.ScmObj* %x42953, %struct.ScmObj** %stackaddr$prim49972, align 8
%stackaddr$prim49973 = alloca %struct.ScmObj*, align 8
%anf_45bind43123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x42953)
store volatile %struct.ScmObj* %anf_45bind43123, %struct.ScmObj** %stackaddr$prim49973, align 8
%stackaddr$prim49974 = alloca %struct.ScmObj*, align 8
%anf_45bind43124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43123)
store volatile %struct.ScmObj* %anf_45bind43124, %struct.ScmObj** %stackaddr$prim49974, align 8
%stackaddr$prim49975 = alloca %struct.ScmObj*, align 8
%anf_45bind43125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43124)
store volatile %struct.ScmObj* %anf_45bind43125, %struct.ScmObj** %stackaddr$prim49975, align 8
%stackaddr$prim49976 = alloca %struct.ScmObj*, align 8
%cpsprim43166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43125)
store volatile %struct.ScmObj* %cpsprim43166, %struct.ScmObj** %stackaddr$prim49976, align 8
%ae46651 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49086$k431650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49977 = alloca %struct.ScmObj*, align 8
%argslist49086$k431651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43166, %struct.ScmObj* %argslist49086$k431650)
store volatile %struct.ScmObj* %argslist49086$k431651, %struct.ScmObj** %stackaddr$prim49977, align 8
%stackaddr$prim49978 = alloca %struct.ScmObj*, align 8
%argslist49086$k431652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46651, %struct.ScmObj* %argslist49086$k431651)
store volatile %struct.ScmObj* %argslist49086$k431652, %struct.ScmObj** %stackaddr$prim49978, align 8
%clofunc49979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43165)
musttail call tailcc void %clofunc49979(%struct.ScmObj* %k43165, %struct.ScmObj* %argslist49086$k431652)
ret void
}

define tailcc void @proc_clo$ae46621(%struct.ScmObj* %env$ae46621,%struct.ScmObj* %current_45args49088) {
%stackaddr$prim49980 = alloca %struct.ScmObj*, align 8
%k43167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49088)
store volatile %struct.ScmObj* %k43167, %struct.ScmObj** %stackaddr$prim49980, align 8
%stackaddr$prim49981 = alloca %struct.ScmObj*, align 8
%current_45args49089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49088)
store volatile %struct.ScmObj* %current_45args49089, %struct.ScmObj** %stackaddr$prim49981, align 8
%stackaddr$prim49982 = alloca %struct.ScmObj*, align 8
%x42955 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49089)
store volatile %struct.ScmObj* %x42955, %struct.ScmObj** %stackaddr$prim49982, align 8
%stackaddr$prim49983 = alloca %struct.ScmObj*, align 8
%anf_45bind43121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x42955)
store volatile %struct.ScmObj* %anf_45bind43121, %struct.ScmObj** %stackaddr$prim49983, align 8
%stackaddr$prim49984 = alloca %struct.ScmObj*, align 8
%anf_45bind43122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43121)
store volatile %struct.ScmObj* %anf_45bind43122, %struct.ScmObj** %stackaddr$prim49984, align 8
%stackaddr$prim49985 = alloca %struct.ScmObj*, align 8
%cpsprim43168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43122)
store volatile %struct.ScmObj* %cpsprim43168, %struct.ScmObj** %stackaddr$prim49985, align 8
%ae46626 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49091$k431670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49986 = alloca %struct.ScmObj*, align 8
%argslist49091$k431671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43168, %struct.ScmObj* %argslist49091$k431670)
store volatile %struct.ScmObj* %argslist49091$k431671, %struct.ScmObj** %stackaddr$prim49986, align 8
%stackaddr$prim49987 = alloca %struct.ScmObj*, align 8
%argslist49091$k431672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46626, %struct.ScmObj* %argslist49091$k431671)
store volatile %struct.ScmObj* %argslist49091$k431672, %struct.ScmObj** %stackaddr$prim49987, align 8
%clofunc49988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43167)
musttail call tailcc void %clofunc49988(%struct.ScmObj* %k43167, %struct.ScmObj* %argslist49091$k431672)
ret void
}

define tailcc void @proc_clo$ae46599(%struct.ScmObj* %env$ae46599,%struct.ScmObj* %current_45args49093) {
%stackaddr$prim49989 = alloca %struct.ScmObj*, align 8
%k43169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49093)
store volatile %struct.ScmObj* %k43169, %struct.ScmObj** %stackaddr$prim49989, align 8
%stackaddr$prim49990 = alloca %struct.ScmObj*, align 8
%current_45args49094 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49093)
store volatile %struct.ScmObj* %current_45args49094, %struct.ScmObj** %stackaddr$prim49990, align 8
%stackaddr$prim49991 = alloca %struct.ScmObj*, align 8
%x42957 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49094)
store volatile %struct.ScmObj* %x42957, %struct.ScmObj** %stackaddr$prim49991, align 8
%stackaddr$prim49992 = alloca %struct.ScmObj*, align 8
%anf_45bind43120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x42957)
store volatile %struct.ScmObj* %anf_45bind43120, %struct.ScmObj** %stackaddr$prim49992, align 8
%stackaddr$prim49993 = alloca %struct.ScmObj*, align 8
%cpsprim43170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43120)
store volatile %struct.ScmObj* %cpsprim43170, %struct.ScmObj** %stackaddr$prim49993, align 8
%ae46603 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49096$k431690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49994 = alloca %struct.ScmObj*, align 8
%argslist49096$k431691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43170, %struct.ScmObj* %argslist49096$k431690)
store volatile %struct.ScmObj* %argslist49096$k431691, %struct.ScmObj** %stackaddr$prim49994, align 8
%stackaddr$prim49995 = alloca %struct.ScmObj*, align 8
%argslist49096$k431692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46603, %struct.ScmObj* %argslist49096$k431691)
store volatile %struct.ScmObj* %argslist49096$k431692, %struct.ScmObj** %stackaddr$prim49995, align 8
%clofunc49996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43169)
musttail call tailcc void %clofunc49996(%struct.ScmObj* %k43169, %struct.ScmObj* %argslist49096$k431692)
ret void
}

define tailcc void @proc_clo$ae46579(%struct.ScmObj* %env$ae46579,%struct.ScmObj* %current_45args49098) {
%stackaddr$prim49997 = alloca %struct.ScmObj*, align 8
%k43171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49098)
store volatile %struct.ScmObj* %k43171, %struct.ScmObj** %stackaddr$prim49997, align 8
%stackaddr$prim49998 = alloca %struct.ScmObj*, align 8
%current_45args49099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49098)
store volatile %struct.ScmObj* %current_45args49099, %struct.ScmObj** %stackaddr$prim49998, align 8
%stackaddr$prim49999 = alloca %struct.ScmObj*, align 8
%x42959 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49099)
store volatile %struct.ScmObj* %x42959, %struct.ScmObj** %stackaddr$prim49999, align 8
%stackaddr$prim50000 = alloca %struct.ScmObj*, align 8
%cpsprim43172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x42959)
store volatile %struct.ScmObj* %cpsprim43172, %struct.ScmObj** %stackaddr$prim50000, align 8
%ae46582 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49101$k431710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50001 = alloca %struct.ScmObj*, align 8
%argslist49101$k431711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43172, %struct.ScmObj* %argslist49101$k431710)
store volatile %struct.ScmObj* %argslist49101$k431711, %struct.ScmObj** %stackaddr$prim50001, align 8
%stackaddr$prim50002 = alloca %struct.ScmObj*, align 8
%argslist49101$k431712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46582, %struct.ScmObj* %argslist49101$k431711)
store volatile %struct.ScmObj* %argslist49101$k431712, %struct.ScmObj** %stackaddr$prim50002, align 8
%clofunc50003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43171)
musttail call tailcc void %clofunc50003(%struct.ScmObj* %k43171, %struct.ScmObj* %argslist49101$k431712)
ret void
}

define tailcc void @proc_clo$ae46481(%struct.ScmObj* %env$ae46481,%struct.ScmObj* %args4296143173) {
%stackaddr$env-ref50004 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46481, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref50004
%stackaddr$prim50005 = alloca %struct.ScmObj*, align 8
%k43174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4296143173)
store volatile %struct.ScmObj* %k43174, %struct.ScmObj** %stackaddr$prim50005, align 8
%stackaddr$prim50006 = alloca %struct.ScmObj*, align 8
%args42961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4296143173)
store volatile %struct.ScmObj* %args42961, %struct.ScmObj** %stackaddr$prim50006, align 8
%stackaddr$prim50007 = alloca %struct.ScmObj*, align 8
%anf_45bind43114 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args42961)
store volatile %struct.ScmObj* %anf_45bind43114, %struct.ScmObj** %stackaddr$prim50007, align 8
%truthy$cmp50008 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43114)
%cmp$cmp50008 = icmp eq i64 %truthy$cmp50008, 1
br i1 %cmp$cmp50008, label %truebranch$cmp50008, label %falsebranch$cmp50008
truebranch$cmp50008:
%ae46487 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46488 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist49103$k431740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50009 = alloca %struct.ScmObj*, align 8
%argslist49103$k431741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46488, %struct.ScmObj* %argslist49103$k431740)
store volatile %struct.ScmObj* %argslist49103$k431741, %struct.ScmObj** %stackaddr$prim50009, align 8
%stackaddr$prim50010 = alloca %struct.ScmObj*, align 8
%argslist49103$k431742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46487, %struct.ScmObj* %argslist49103$k431741)
store volatile %struct.ScmObj* %argslist49103$k431742, %struct.ScmObj** %stackaddr$prim50010, align 8
%clofunc50011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43174)
musttail call tailcc void %clofunc50011(%struct.ScmObj* %k43174, %struct.ScmObj* %argslist49103$k431742)
ret void
falsebranch$cmp50008:
%stackaddr$prim50012 = alloca %struct.ScmObj*, align 8
%anf_45bind43115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args42961)
store volatile %struct.ScmObj* %anf_45bind43115, %struct.ScmObj** %stackaddr$prim50012, align 8
%stackaddr$prim50013 = alloca %struct.ScmObj*, align 8
%anf_45bind43116 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind43115)
store volatile %struct.ScmObj* %anf_45bind43116, %struct.ScmObj** %stackaddr$prim50013, align 8
%truthy$cmp50014 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43116)
%cmp$cmp50014 = icmp eq i64 %truthy$cmp50014, 1
br i1 %cmp$cmp50014, label %truebranch$cmp50014, label %falsebranch$cmp50014
truebranch$cmp50014:
%stackaddr$prim50015 = alloca %struct.ScmObj*, align 8
%cpsprim43175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args42961)
store volatile %struct.ScmObj* %cpsprim43175, %struct.ScmObj** %stackaddr$prim50015, align 8
%ae46500 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49104$k431740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50016 = alloca %struct.ScmObj*, align 8
%argslist49104$k431741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43175, %struct.ScmObj* %argslist49104$k431740)
store volatile %struct.ScmObj* %argslist49104$k431741, %struct.ScmObj** %stackaddr$prim50016, align 8
%stackaddr$prim50017 = alloca %struct.ScmObj*, align 8
%argslist49104$k431742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46500, %struct.ScmObj* %argslist49104$k431741)
store volatile %struct.ScmObj* %argslist49104$k431742, %struct.ScmObj** %stackaddr$prim50017, align 8
%clofunc50018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43174)
musttail call tailcc void %clofunc50018(%struct.ScmObj* %k43174, %struct.ScmObj* %argslist49104$k431742)
ret void
falsebranch$cmp50014:
%stackaddr$makeclosure50019 = alloca %struct.ScmObj*, align 8
%fptrToInt50020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46505 to i64
%ae46505 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50020)
store volatile %struct.ScmObj* %ae46505, %struct.ScmObj** %stackaddr$makeclosure50019, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46505, %struct.ScmObj* %_37foldl142900, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae46505, %struct.ScmObj* %args42961, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae46505, %struct.ScmObj* %k43174, i64 2)
%ae46506 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50021 = alloca %struct.ScmObj*, align 8
%fptrToInt50022 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46507 to i64
%ae46507 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50022)
store volatile %struct.ScmObj* %ae46507, %struct.ScmObj** %stackaddr$makeclosure50021, align 8
%argslist49114$ae465050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50023 = alloca %struct.ScmObj*, align 8
%argslist49114$ae465051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46507, %struct.ScmObj* %argslist49114$ae465050)
store volatile %struct.ScmObj* %argslist49114$ae465051, %struct.ScmObj** %stackaddr$prim50023, align 8
%stackaddr$prim50024 = alloca %struct.ScmObj*, align 8
%argslist49114$ae465052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46506, %struct.ScmObj* %argslist49114$ae465051)
store volatile %struct.ScmObj* %argslist49114$ae465052, %struct.ScmObj** %stackaddr$prim50024, align 8
%clofunc50025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46505)
musttail call tailcc void %clofunc50025(%struct.ScmObj* %ae46505, %struct.ScmObj* %argslist49114$ae465052)
ret void
}

define tailcc void @proc_clo$ae46505(%struct.ScmObj* %env$ae46505,%struct.ScmObj* %current_45args49105) {
%stackaddr$env-ref50026 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46505, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref50026
%stackaddr$env-ref50027 = alloca %struct.ScmObj*, align 8
%args42961 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46505, i64 1)
store %struct.ScmObj* %args42961, %struct.ScmObj** %stackaddr$env-ref50027
%stackaddr$env-ref50028 = alloca %struct.ScmObj*, align 8
%k43174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46505, i64 2)
store %struct.ScmObj* %k43174, %struct.ScmObj** %stackaddr$env-ref50028
%stackaddr$prim50029 = alloca %struct.ScmObj*, align 8
%_95k43176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49105)
store volatile %struct.ScmObj* %_95k43176, %struct.ScmObj** %stackaddr$prim50029, align 8
%stackaddr$prim50030 = alloca %struct.ScmObj*, align 8
%current_45args49106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49105)
store volatile %struct.ScmObj* %current_45args49106, %struct.ScmObj** %stackaddr$prim50030, align 8
%stackaddr$prim50031 = alloca %struct.ScmObj*, align 8
%anf_45bind43117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49106)
store volatile %struct.ScmObj* %anf_45bind43117, %struct.ScmObj** %stackaddr$prim50031, align 8
%stackaddr$prim50032 = alloca %struct.ScmObj*, align 8
%anf_45bind43118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args42961)
store volatile %struct.ScmObj* %anf_45bind43118, %struct.ScmObj** %stackaddr$prim50032, align 8
%stackaddr$prim50033 = alloca %struct.ScmObj*, align 8
%anf_45bind43119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args42961)
store volatile %struct.ScmObj* %anf_45bind43119, %struct.ScmObj** %stackaddr$prim50033, align 8
%argslist49108$_37foldl1429000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50034 = alloca %struct.ScmObj*, align 8
%argslist49108$_37foldl1429001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43119, %struct.ScmObj* %argslist49108$_37foldl1429000)
store volatile %struct.ScmObj* %argslist49108$_37foldl1429001, %struct.ScmObj** %stackaddr$prim50034, align 8
%stackaddr$prim50035 = alloca %struct.ScmObj*, align 8
%argslist49108$_37foldl1429002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43118, %struct.ScmObj* %argslist49108$_37foldl1429001)
store volatile %struct.ScmObj* %argslist49108$_37foldl1429002, %struct.ScmObj** %stackaddr$prim50035, align 8
%stackaddr$prim50036 = alloca %struct.ScmObj*, align 8
%argslist49108$_37foldl1429003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43117, %struct.ScmObj* %argslist49108$_37foldl1429002)
store volatile %struct.ScmObj* %argslist49108$_37foldl1429003, %struct.ScmObj** %stackaddr$prim50036, align 8
%stackaddr$prim50037 = alloca %struct.ScmObj*, align 8
%argslist49108$_37foldl1429004 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43174, %struct.ScmObj* %argslist49108$_37foldl1429003)
store volatile %struct.ScmObj* %argslist49108$_37foldl1429004, %struct.ScmObj** %stackaddr$prim50037, align 8
%clofunc50038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl142900)
musttail call tailcc void %clofunc50038(%struct.ScmObj* %_37foldl142900, %struct.ScmObj* %argslist49108$_37foldl1429004)
ret void
}

define tailcc void @proc_clo$ae46507(%struct.ScmObj* %env$ae46507,%struct.ScmObj* %current_45args49109) {
%stackaddr$prim50039 = alloca %struct.ScmObj*, align 8
%k43177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49109)
store volatile %struct.ScmObj* %k43177, %struct.ScmObj** %stackaddr$prim50039, align 8
%stackaddr$prim50040 = alloca %struct.ScmObj*, align 8
%current_45args49110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49109)
store volatile %struct.ScmObj* %current_45args49110, %struct.ScmObj** %stackaddr$prim50040, align 8
%stackaddr$prim50041 = alloca %struct.ScmObj*, align 8
%n42963 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49110)
store volatile %struct.ScmObj* %n42963, %struct.ScmObj** %stackaddr$prim50041, align 8
%stackaddr$prim50042 = alloca %struct.ScmObj*, align 8
%current_45args49111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49110)
store volatile %struct.ScmObj* %current_45args49111, %struct.ScmObj** %stackaddr$prim50042, align 8
%stackaddr$prim50043 = alloca %struct.ScmObj*, align 8
%v42962 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49111)
store volatile %struct.ScmObj* %v42962, %struct.ScmObj** %stackaddr$prim50043, align 8
%stackaddr$prim50044 = alloca %struct.ScmObj*, align 8
%cpsprim43178 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v42962, %struct.ScmObj* %n42963)
store volatile %struct.ScmObj* %cpsprim43178, %struct.ScmObj** %stackaddr$prim50044, align 8
%ae46511 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49113$k431770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50045 = alloca %struct.ScmObj*, align 8
%argslist49113$k431771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43178, %struct.ScmObj* %argslist49113$k431770)
store volatile %struct.ScmObj* %argslist49113$k431771, %struct.ScmObj** %stackaddr$prim50045, align 8
%stackaddr$prim50046 = alloca %struct.ScmObj*, align 8
%argslist49113$k431772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46511, %struct.ScmObj* %argslist49113$k431771)
store volatile %struct.ScmObj* %argslist49113$k431772, %struct.ScmObj** %stackaddr$prim50046, align 8
%clofunc50047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43177)
musttail call tailcc void %clofunc50047(%struct.ScmObj* %k43177, %struct.ScmObj* %argslist49113$k431772)
ret void
}

define tailcc void @proc_clo$ae46077(%struct.ScmObj* %env$ae46077,%struct.ScmObj* %current_45args49116) {
%stackaddr$prim50048 = alloca %struct.ScmObj*, align 8
%k43179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49116)
store volatile %struct.ScmObj* %k43179, %struct.ScmObj** %stackaddr$prim50048, align 8
%stackaddr$prim50049 = alloca %struct.ScmObj*, align 8
%current_45args49117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49116)
store volatile %struct.ScmObj* %current_45args49117, %struct.ScmObj** %stackaddr$prim50049, align 8
%stackaddr$prim50050 = alloca %struct.ScmObj*, align 8
%v42966 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49117)
store volatile %struct.ScmObj* %v42966, %struct.ScmObj** %stackaddr$prim50050, align 8
%stackaddr$prim50051 = alloca %struct.ScmObj*, align 8
%current_45args49118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49117)
store volatile %struct.ScmObj* %current_45args49118, %struct.ScmObj** %stackaddr$prim50051, align 8
%stackaddr$prim50052 = alloca %struct.ScmObj*, align 8
%lst42965 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49118)
store volatile %struct.ScmObj* %lst42965, %struct.ScmObj** %stackaddr$prim50052, align 8
%ae46078 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50053 = alloca %struct.ScmObj*, align 8
%lst42967 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae46078, %struct.ScmObj* %lst42965)
store volatile %struct.ScmObj* %lst42967, %struct.ScmObj** %stackaddr$prim50053, align 8
%stackaddr$makeclosure50054 = alloca %struct.ScmObj*, align 8
%fptrToInt50055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46080 to i64
%ae46080 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50055)
store volatile %struct.ScmObj* %ae46080, %struct.ScmObj** %stackaddr$makeclosure50054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46080, %struct.ScmObj* %k43179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae46080, %struct.ScmObj* %lst42967, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae46080, %struct.ScmObj* %v42966, i64 2)
%ae46081 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50056 = alloca %struct.ScmObj*, align 8
%fptrToInt50057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46082 to i64
%ae46082 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50057)
store volatile %struct.ScmObj* %ae46082, %struct.ScmObj** %stackaddr$makeclosure50056, align 8
%argslist49140$ae460800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50058 = alloca %struct.ScmObj*, align 8
%argslist49140$ae460801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46082, %struct.ScmObj* %argslist49140$ae460800)
store volatile %struct.ScmObj* %argslist49140$ae460801, %struct.ScmObj** %stackaddr$prim50058, align 8
%stackaddr$prim50059 = alloca %struct.ScmObj*, align 8
%argslist49140$ae460802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46081, %struct.ScmObj* %argslist49140$ae460801)
store volatile %struct.ScmObj* %argslist49140$ae460802, %struct.ScmObj** %stackaddr$prim50059, align 8
%clofunc50060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae46080)
musttail call tailcc void %clofunc50060(%struct.ScmObj* %ae46080, %struct.ScmObj* %argslist49140$ae460802)
ret void
}

define tailcc void @proc_clo$ae46080(%struct.ScmObj* %env$ae46080,%struct.ScmObj* %current_45args49120) {
%stackaddr$env-ref50061 = alloca %struct.ScmObj*, align 8
%k43179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46080, i64 0)
store %struct.ScmObj* %k43179, %struct.ScmObj** %stackaddr$env-ref50061
%stackaddr$env-ref50062 = alloca %struct.ScmObj*, align 8
%lst42967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46080, i64 1)
store %struct.ScmObj* %lst42967, %struct.ScmObj** %stackaddr$env-ref50062
%stackaddr$env-ref50063 = alloca %struct.ScmObj*, align 8
%v42966 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46080, i64 2)
store %struct.ScmObj* %v42966, %struct.ScmObj** %stackaddr$env-ref50063
%stackaddr$prim50064 = alloca %struct.ScmObj*, align 8
%_95k43180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49120)
store volatile %struct.ScmObj* %_95k43180, %struct.ScmObj** %stackaddr$prim50064, align 8
%stackaddr$prim50065 = alloca %struct.ScmObj*, align 8
%current_45args49121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49120)
store volatile %struct.ScmObj* %current_45args49121, %struct.ScmObj** %stackaddr$prim50065, align 8
%stackaddr$prim50066 = alloca %struct.ScmObj*, align 8
%anf_45bind43106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49121)
store volatile %struct.ScmObj* %anf_45bind43106, %struct.ScmObj** %stackaddr$prim50066, align 8
%stackaddr$makeclosure50067 = alloca %struct.ScmObj*, align 8
%fptrToInt50068 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46096 to i64
%ae46096 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50068)
store volatile %struct.ScmObj* %ae46096, %struct.ScmObj** %stackaddr$makeclosure50067, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46096, %struct.ScmObj* %k43179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae46096, %struct.ScmObj* %lst42967, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae46096, %struct.ScmObj* %v42966, i64 2)
%stackaddr$makeclosure50069 = alloca %struct.ScmObj*, align 8
%fptrToInt50070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46097 to i64
%ae46097 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50070)
store volatile %struct.ScmObj* %ae46097, %struct.ScmObj** %stackaddr$makeclosure50069, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae46097, %struct.ScmObj* %k43179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae46097, %struct.ScmObj* %lst42967, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae46097, %struct.ScmObj* %v42966, i64 2)
%argslist49135$anf_45bind431060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50071 = alloca %struct.ScmObj*, align 8
%argslist49135$anf_45bind431061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46097, %struct.ScmObj* %argslist49135$anf_45bind431060)
store volatile %struct.ScmObj* %argslist49135$anf_45bind431061, %struct.ScmObj** %stackaddr$prim50071, align 8
%stackaddr$prim50072 = alloca %struct.ScmObj*, align 8
%argslist49135$anf_45bind431062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46096, %struct.ScmObj* %argslist49135$anf_45bind431061)
store volatile %struct.ScmObj* %argslist49135$anf_45bind431062, %struct.ScmObj** %stackaddr$prim50072, align 8
%clofunc50073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43106)
musttail call tailcc void %clofunc50073(%struct.ScmObj* %anf_45bind43106, %struct.ScmObj* %argslist49135$anf_45bind431062)
ret void
}

define tailcc void @proc_clo$ae46096(%struct.ScmObj* %env$ae46096,%struct.ScmObj* %current_45args49123) {
%stackaddr$env-ref50074 = alloca %struct.ScmObj*, align 8
%k43179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46096, i64 0)
store %struct.ScmObj* %k43179, %struct.ScmObj** %stackaddr$env-ref50074
%stackaddr$env-ref50075 = alloca %struct.ScmObj*, align 8
%lst42967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46096, i64 1)
store %struct.ScmObj* %lst42967, %struct.ScmObj** %stackaddr$env-ref50075
%stackaddr$env-ref50076 = alloca %struct.ScmObj*, align 8
%v42966 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46096, i64 2)
store %struct.ScmObj* %v42966, %struct.ScmObj** %stackaddr$env-ref50076
%stackaddr$prim50077 = alloca %struct.ScmObj*, align 8
%_95k43181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49123)
store volatile %struct.ScmObj* %_95k43181, %struct.ScmObj** %stackaddr$prim50077, align 8
%stackaddr$prim50078 = alloca %struct.ScmObj*, align 8
%current_45args49124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49123)
store volatile %struct.ScmObj* %current_45args49124, %struct.ScmObj** %stackaddr$prim50078, align 8
%stackaddr$prim50079 = alloca %struct.ScmObj*, align 8
%cc42968 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49124)
store volatile %struct.ScmObj* %cc42968, %struct.ScmObj** %stackaddr$prim50079, align 8
%ae46205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50080 = alloca %struct.ScmObj*, align 8
%anf_45bind43107 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46205)
store volatile %struct.ScmObj* %anf_45bind43107, %struct.ScmObj** %stackaddr$prim50080, align 8
%stackaddr$prim50081 = alloca %struct.ScmObj*, align 8
%anf_45bind43108 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind43107)
store volatile %struct.ScmObj* %anf_45bind43108, %struct.ScmObj** %stackaddr$prim50081, align 8
%truthy$cmp50082 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43108)
%cmp$cmp50082 = icmp eq i64 %truthy$cmp50082, 1
br i1 %cmp$cmp50082, label %truebranch$cmp50082, label %falsebranch$cmp50082
truebranch$cmp50082:
%ae46209 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46210 = call %struct.ScmObj* @const_init_false()
%argslist49126$k431790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50083 = alloca %struct.ScmObj*, align 8
%argslist49126$k431791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46210, %struct.ScmObj* %argslist49126$k431790)
store volatile %struct.ScmObj* %argslist49126$k431791, %struct.ScmObj** %stackaddr$prim50083, align 8
%stackaddr$prim50084 = alloca %struct.ScmObj*, align 8
%argslist49126$k431792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46209, %struct.ScmObj* %argslist49126$k431791)
store volatile %struct.ScmObj* %argslist49126$k431792, %struct.ScmObj** %stackaddr$prim50084, align 8
%clofunc50085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43179)
musttail call tailcc void %clofunc50085(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49126$k431792)
ret void
falsebranch$cmp50082:
%ae46218 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50086 = alloca %struct.ScmObj*, align 8
%anf_45bind43109 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46218)
store volatile %struct.ScmObj* %anf_45bind43109, %struct.ScmObj** %stackaddr$prim50086, align 8
%stackaddr$prim50087 = alloca %struct.ScmObj*, align 8
%anf_45bind43110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43109)
store volatile %struct.ScmObj* %anf_45bind43110, %struct.ScmObj** %stackaddr$prim50087, align 8
%stackaddr$prim50088 = alloca %struct.ScmObj*, align 8
%anf_45bind43111 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind43110, %struct.ScmObj* %v42966)
store volatile %struct.ScmObj* %anf_45bind43111, %struct.ScmObj** %stackaddr$prim50088, align 8
%truthy$cmp50089 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43111)
%cmp$cmp50089 = icmp eq i64 %truthy$cmp50089, 1
br i1 %cmp$cmp50089, label %truebranch$cmp50089, label %falsebranch$cmp50089
truebranch$cmp50089:
%ae46224 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50090 = alloca %struct.ScmObj*, align 8
%cpsprim43182 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46224)
store volatile %struct.ScmObj* %cpsprim43182, %struct.ScmObj** %stackaddr$prim50090, align 8
%ae46226 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49127$k431790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50091 = alloca %struct.ScmObj*, align 8
%argslist49127$k431791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43182, %struct.ScmObj* %argslist49127$k431790)
store volatile %struct.ScmObj* %argslist49127$k431791, %struct.ScmObj** %stackaddr$prim50091, align 8
%stackaddr$prim50092 = alloca %struct.ScmObj*, align 8
%argslist49127$k431792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46226, %struct.ScmObj* %argslist49127$k431791)
store volatile %struct.ScmObj* %argslist49127$k431792, %struct.ScmObj** %stackaddr$prim50092, align 8
%clofunc50093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43179)
musttail call tailcc void %clofunc50093(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49127$k431792)
ret void
falsebranch$cmp50089:
%ae46237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50094 = alloca %struct.ScmObj*, align 8
%anf_45bind43112 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46237)
store volatile %struct.ScmObj* %anf_45bind43112, %struct.ScmObj** %stackaddr$prim50094, align 8
%stackaddr$prim50095 = alloca %struct.ScmObj*, align 8
%anf_45bind43113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43112)
store volatile %struct.ScmObj* %anf_45bind43113, %struct.ScmObj** %stackaddr$prim50095, align 8
%ae46240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50096 = alloca %struct.ScmObj*, align 8
%_95042970 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46240, %struct.ScmObj* %anf_45bind43113)
store volatile %struct.ScmObj* %_95042970, %struct.ScmObj** %stackaddr$prim50096, align 8
%argslist49128$cc429680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50097 = alloca %struct.ScmObj*, align 8
%argslist49128$cc429681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42968, %struct.ScmObj* %argslist49128$cc429680)
store volatile %struct.ScmObj* %argslist49128$cc429681, %struct.ScmObj** %stackaddr$prim50097, align 8
%stackaddr$prim50098 = alloca %struct.ScmObj*, align 8
%argslist49128$cc429682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49128$cc429681)
store volatile %struct.ScmObj* %argslist49128$cc429682, %struct.ScmObj** %stackaddr$prim50098, align 8
%clofunc50099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42968)
musttail call tailcc void %clofunc50099(%struct.ScmObj* %cc42968, %struct.ScmObj* %argslist49128$cc429682)
ret void
}

define tailcc void @proc_clo$ae46097(%struct.ScmObj* %env$ae46097,%struct.ScmObj* %current_45args49129) {
%stackaddr$env-ref50100 = alloca %struct.ScmObj*, align 8
%k43179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46097, i64 0)
store %struct.ScmObj* %k43179, %struct.ScmObj** %stackaddr$env-ref50100
%stackaddr$env-ref50101 = alloca %struct.ScmObj*, align 8
%lst42967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46097, i64 1)
store %struct.ScmObj* %lst42967, %struct.ScmObj** %stackaddr$env-ref50101
%stackaddr$env-ref50102 = alloca %struct.ScmObj*, align 8
%v42966 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae46097, i64 2)
store %struct.ScmObj* %v42966, %struct.ScmObj** %stackaddr$env-ref50102
%stackaddr$prim50103 = alloca %struct.ScmObj*, align 8
%_95k43181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49129)
store volatile %struct.ScmObj* %_95k43181, %struct.ScmObj** %stackaddr$prim50103, align 8
%stackaddr$prim50104 = alloca %struct.ScmObj*, align 8
%current_45args49130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49129)
store volatile %struct.ScmObj* %current_45args49130, %struct.ScmObj** %stackaddr$prim50104, align 8
%stackaddr$prim50105 = alloca %struct.ScmObj*, align 8
%cc42968 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49130)
store volatile %struct.ScmObj* %cc42968, %struct.ScmObj** %stackaddr$prim50105, align 8
%ae46099 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50106 = alloca %struct.ScmObj*, align 8
%anf_45bind43107 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46099)
store volatile %struct.ScmObj* %anf_45bind43107, %struct.ScmObj** %stackaddr$prim50106, align 8
%stackaddr$prim50107 = alloca %struct.ScmObj*, align 8
%anf_45bind43108 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind43107)
store volatile %struct.ScmObj* %anf_45bind43108, %struct.ScmObj** %stackaddr$prim50107, align 8
%truthy$cmp50108 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43108)
%cmp$cmp50108 = icmp eq i64 %truthy$cmp50108, 1
br i1 %cmp$cmp50108, label %truebranch$cmp50108, label %falsebranch$cmp50108
truebranch$cmp50108:
%ae46103 = call %struct.ScmObj* @const_init_int(i64 0)
%ae46104 = call %struct.ScmObj* @const_init_false()
%argslist49132$k431790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50109 = alloca %struct.ScmObj*, align 8
%argslist49132$k431791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46104, %struct.ScmObj* %argslist49132$k431790)
store volatile %struct.ScmObj* %argslist49132$k431791, %struct.ScmObj** %stackaddr$prim50109, align 8
%stackaddr$prim50110 = alloca %struct.ScmObj*, align 8
%argslist49132$k431792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46103, %struct.ScmObj* %argslist49132$k431791)
store volatile %struct.ScmObj* %argslist49132$k431792, %struct.ScmObj** %stackaddr$prim50110, align 8
%clofunc50111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43179)
musttail call tailcc void %clofunc50111(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49132$k431792)
ret void
falsebranch$cmp50108:
%ae46112 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50112 = alloca %struct.ScmObj*, align 8
%anf_45bind43109 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46112)
store volatile %struct.ScmObj* %anf_45bind43109, %struct.ScmObj** %stackaddr$prim50112, align 8
%stackaddr$prim50113 = alloca %struct.ScmObj*, align 8
%anf_45bind43110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43109)
store volatile %struct.ScmObj* %anf_45bind43110, %struct.ScmObj** %stackaddr$prim50113, align 8
%stackaddr$prim50114 = alloca %struct.ScmObj*, align 8
%anf_45bind43111 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind43110, %struct.ScmObj* %v42966)
store volatile %struct.ScmObj* %anf_45bind43111, %struct.ScmObj** %stackaddr$prim50114, align 8
%truthy$cmp50115 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43111)
%cmp$cmp50115 = icmp eq i64 %truthy$cmp50115, 1
br i1 %cmp$cmp50115, label %truebranch$cmp50115, label %falsebranch$cmp50115
truebranch$cmp50115:
%ae46118 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50116 = alloca %struct.ScmObj*, align 8
%cpsprim43182 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46118)
store volatile %struct.ScmObj* %cpsprim43182, %struct.ScmObj** %stackaddr$prim50116, align 8
%ae46120 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49133$k431790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50117 = alloca %struct.ScmObj*, align 8
%argslist49133$k431791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43182, %struct.ScmObj* %argslist49133$k431790)
store volatile %struct.ScmObj* %argslist49133$k431791, %struct.ScmObj** %stackaddr$prim50117, align 8
%stackaddr$prim50118 = alloca %struct.ScmObj*, align 8
%argslist49133$k431792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46120, %struct.ScmObj* %argslist49133$k431791)
store volatile %struct.ScmObj* %argslist49133$k431792, %struct.ScmObj** %stackaddr$prim50118, align 8
%clofunc50119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43179)
musttail call tailcc void %clofunc50119(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49133$k431792)
ret void
falsebranch$cmp50115:
%ae46131 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50120 = alloca %struct.ScmObj*, align 8
%anf_45bind43112 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46131)
store volatile %struct.ScmObj* %anf_45bind43112, %struct.ScmObj** %stackaddr$prim50120, align 8
%stackaddr$prim50121 = alloca %struct.ScmObj*, align 8
%anf_45bind43113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43112)
store volatile %struct.ScmObj* %anf_45bind43113, %struct.ScmObj** %stackaddr$prim50121, align 8
%ae46134 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50122 = alloca %struct.ScmObj*, align 8
%_95042970 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst42967, %struct.ScmObj* %ae46134, %struct.ScmObj* %anf_45bind43113)
store volatile %struct.ScmObj* %_95042970, %struct.ScmObj** %stackaddr$prim50122, align 8
%argslist49134$cc429680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50123 = alloca %struct.ScmObj*, align 8
%argslist49134$cc429681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42968, %struct.ScmObj* %argslist49134$cc429680)
store volatile %struct.ScmObj* %argslist49134$cc429681, %struct.ScmObj** %stackaddr$prim50123, align 8
%stackaddr$prim50124 = alloca %struct.ScmObj*, align 8
%argslist49134$cc429682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43179, %struct.ScmObj* %argslist49134$cc429681)
store volatile %struct.ScmObj* %argslist49134$cc429682, %struct.ScmObj** %stackaddr$prim50124, align 8
%clofunc50125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42968)
musttail call tailcc void %clofunc50125(%struct.ScmObj* %cc42968, %struct.ScmObj* %argslist49134$cc429682)
ret void
}

define tailcc void @proc_clo$ae46082(%struct.ScmObj* %env$ae46082,%struct.ScmObj* %current_45args49136) {
%stackaddr$prim50126 = alloca %struct.ScmObj*, align 8
%k43183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49136)
store volatile %struct.ScmObj* %k43183, %struct.ScmObj** %stackaddr$prim50126, align 8
%stackaddr$prim50127 = alloca %struct.ScmObj*, align 8
%current_45args49137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49136)
store volatile %struct.ScmObj* %current_45args49137, %struct.ScmObj** %stackaddr$prim50127, align 8
%stackaddr$prim50128 = alloca %struct.ScmObj*, align 8
%u42969 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49137)
store volatile %struct.ScmObj* %u42969, %struct.ScmObj** %stackaddr$prim50128, align 8
%argslist49139$u429690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50129 = alloca %struct.ScmObj*, align 8
%argslist49139$u429691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u42969, %struct.ScmObj* %argslist49139$u429690)
store volatile %struct.ScmObj* %argslist49139$u429691, %struct.ScmObj** %stackaddr$prim50129, align 8
%stackaddr$prim50130 = alloca %struct.ScmObj*, align 8
%argslist49139$u429692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43183, %struct.ScmObj* %argslist49139$u429691)
store volatile %struct.ScmObj* %argslist49139$u429692, %struct.ScmObj** %stackaddr$prim50130, align 8
%clofunc50131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u42969)
musttail call tailcc void %clofunc50131(%struct.ScmObj* %u42969, %struct.ScmObj* %argslist49139$u429692)
ret void
}

define tailcc void @proc_clo$ae45541(%struct.ScmObj* %env$ae45541,%struct.ScmObj* %current_45args49142) {
%stackaddr$prim50132 = alloca %struct.ScmObj*, align 8
%k43184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49142)
store volatile %struct.ScmObj* %k43184, %struct.ScmObj** %stackaddr$prim50132, align 8
%stackaddr$prim50133 = alloca %struct.ScmObj*, align 8
%current_45args49143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49142)
store volatile %struct.ScmObj* %current_45args49143, %struct.ScmObj** %stackaddr$prim50133, align 8
%stackaddr$prim50134 = alloca %struct.ScmObj*, align 8
%lst42973 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49143)
store volatile %struct.ScmObj* %lst42973, %struct.ScmObj** %stackaddr$prim50134, align 8
%stackaddr$prim50135 = alloca %struct.ScmObj*, align 8
%current_45args49144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49143)
store volatile %struct.ScmObj* %current_45args49144, %struct.ScmObj** %stackaddr$prim50135, align 8
%stackaddr$prim50136 = alloca %struct.ScmObj*, align 8
%n42972 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49144)
store volatile %struct.ScmObj* %n42972, %struct.ScmObj** %stackaddr$prim50136, align 8
%ae45542 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50137 = alloca %struct.ScmObj*, align 8
%n42975 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae45542, %struct.ScmObj* %n42972)
store volatile %struct.ScmObj* %n42975, %struct.ScmObj** %stackaddr$prim50137, align 8
%ae45544 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50138 = alloca %struct.ScmObj*, align 8
%lst42974 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae45544, %struct.ScmObj* %lst42973)
store volatile %struct.ScmObj* %lst42974, %struct.ScmObj** %stackaddr$prim50138, align 8
%stackaddr$makeclosure50139 = alloca %struct.ScmObj*, align 8
%fptrToInt50140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45546 to i64
%ae45546 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50140)
store volatile %struct.ScmObj* %ae45546, %struct.ScmObj** %stackaddr$makeclosure50139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45546, %struct.ScmObj* %k43184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45546, %struct.ScmObj* %n42975, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45546, %struct.ScmObj* %lst42974, i64 2)
%ae45547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50141 = alloca %struct.ScmObj*, align 8
%fptrToInt50142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45548 to i64
%ae45548 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50142)
store volatile %struct.ScmObj* %ae45548, %struct.ScmObj** %stackaddr$makeclosure50141, align 8
%argslist49164$ae455460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50143 = alloca %struct.ScmObj*, align 8
%argslist49164$ae455461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45548, %struct.ScmObj* %argslist49164$ae455460)
store volatile %struct.ScmObj* %argslist49164$ae455461, %struct.ScmObj** %stackaddr$prim50143, align 8
%stackaddr$prim50144 = alloca %struct.ScmObj*, align 8
%argslist49164$ae455462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45547, %struct.ScmObj* %argslist49164$ae455461)
store volatile %struct.ScmObj* %argslist49164$ae455462, %struct.ScmObj** %stackaddr$prim50144, align 8
%clofunc50145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45546)
musttail call tailcc void %clofunc50145(%struct.ScmObj* %ae45546, %struct.ScmObj* %argslist49164$ae455462)
ret void
}

define tailcc void @proc_clo$ae45546(%struct.ScmObj* %env$ae45546,%struct.ScmObj* %current_45args49146) {
%stackaddr$env-ref50146 = alloca %struct.ScmObj*, align 8
%k43184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45546, i64 0)
store %struct.ScmObj* %k43184, %struct.ScmObj** %stackaddr$env-ref50146
%stackaddr$env-ref50147 = alloca %struct.ScmObj*, align 8
%n42975 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45546, i64 1)
store %struct.ScmObj* %n42975, %struct.ScmObj** %stackaddr$env-ref50147
%stackaddr$env-ref50148 = alloca %struct.ScmObj*, align 8
%lst42974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45546, i64 2)
store %struct.ScmObj* %lst42974, %struct.ScmObj** %stackaddr$env-ref50148
%stackaddr$prim50149 = alloca %struct.ScmObj*, align 8
%_95k43185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49146)
store volatile %struct.ScmObj* %_95k43185, %struct.ScmObj** %stackaddr$prim50149, align 8
%stackaddr$prim50150 = alloca %struct.ScmObj*, align 8
%current_45args49147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49146)
store volatile %struct.ScmObj* %current_45args49147, %struct.ScmObj** %stackaddr$prim50150, align 8
%stackaddr$prim50151 = alloca %struct.ScmObj*, align 8
%anf_45bind43099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49147)
store volatile %struct.ScmObj* %anf_45bind43099, %struct.ScmObj** %stackaddr$prim50151, align 8
%stackaddr$makeclosure50152 = alloca %struct.ScmObj*, align 8
%fptrToInt50153 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45562 to i64
%ae45562 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50153)
store volatile %struct.ScmObj* %ae45562, %struct.ScmObj** %stackaddr$makeclosure50152, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45562, %struct.ScmObj* %k43184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45562, %struct.ScmObj* %n42975, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45562, %struct.ScmObj* %lst42974, i64 2)
%stackaddr$makeclosure50154 = alloca %struct.ScmObj*, align 8
%fptrToInt50155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45563 to i64
%ae45563 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50155)
store volatile %struct.ScmObj* %ae45563, %struct.ScmObj** %stackaddr$makeclosure50154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45563, %struct.ScmObj* %k43184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45563, %struct.ScmObj* %n42975, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae45563, %struct.ScmObj* %lst42974, i64 2)
%argslist49159$anf_45bind430990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50156 = alloca %struct.ScmObj*, align 8
%argslist49159$anf_45bind430991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45563, %struct.ScmObj* %argslist49159$anf_45bind430990)
store volatile %struct.ScmObj* %argslist49159$anf_45bind430991, %struct.ScmObj** %stackaddr$prim50156, align 8
%stackaddr$prim50157 = alloca %struct.ScmObj*, align 8
%argslist49159$anf_45bind430992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45562, %struct.ScmObj* %argslist49159$anf_45bind430991)
store volatile %struct.ScmObj* %argslist49159$anf_45bind430992, %struct.ScmObj** %stackaddr$prim50157, align 8
%clofunc50158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43099)
musttail call tailcc void %clofunc50158(%struct.ScmObj* %anf_45bind43099, %struct.ScmObj* %argslist49159$anf_45bind430992)
ret void
}

define tailcc void @proc_clo$ae45562(%struct.ScmObj* %env$ae45562,%struct.ScmObj* %current_45args49149) {
%stackaddr$env-ref50159 = alloca %struct.ScmObj*, align 8
%k43184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45562, i64 0)
store %struct.ScmObj* %k43184, %struct.ScmObj** %stackaddr$env-ref50159
%stackaddr$env-ref50160 = alloca %struct.ScmObj*, align 8
%n42975 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45562, i64 1)
store %struct.ScmObj* %n42975, %struct.ScmObj** %stackaddr$env-ref50160
%stackaddr$env-ref50161 = alloca %struct.ScmObj*, align 8
%lst42974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45562, i64 2)
store %struct.ScmObj* %lst42974, %struct.ScmObj** %stackaddr$env-ref50161
%stackaddr$prim50162 = alloca %struct.ScmObj*, align 8
%_95k43186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49149)
store volatile %struct.ScmObj* %_95k43186, %struct.ScmObj** %stackaddr$prim50162, align 8
%stackaddr$prim50163 = alloca %struct.ScmObj*, align 8
%current_45args49150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49149)
store volatile %struct.ScmObj* %current_45args49150, %struct.ScmObj** %stackaddr$prim50163, align 8
%stackaddr$prim50164 = alloca %struct.ScmObj*, align 8
%cc42976 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49150)
store volatile %struct.ScmObj* %cc42976, %struct.ScmObj** %stackaddr$prim50164, align 8
%ae45705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50165 = alloca %struct.ScmObj*, align 8
%anf_45bind43100 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45705)
store volatile %struct.ScmObj* %anf_45bind43100, %struct.ScmObj** %stackaddr$prim50165, align 8
%ae45706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50166 = alloca %struct.ScmObj*, align 8
%anf_45bind43101 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae45706, %struct.ScmObj* %anf_45bind43100)
store volatile %struct.ScmObj* %anf_45bind43101, %struct.ScmObj** %stackaddr$prim50166, align 8
%truthy$cmp50167 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43101)
%cmp$cmp50167 = icmp eq i64 %truthy$cmp50167, 1
br i1 %cmp$cmp50167, label %truebranch$cmp50167, label %falsebranch$cmp50167
truebranch$cmp50167:
%ae45710 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50168 = alloca %struct.ScmObj*, align 8
%cpsprim43187 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45710)
store volatile %struct.ScmObj* %cpsprim43187, %struct.ScmObj** %stackaddr$prim50168, align 8
%ae45712 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49152$k431840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50169 = alloca %struct.ScmObj*, align 8
%argslist49152$k431841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43187, %struct.ScmObj* %argslist49152$k431840)
store volatile %struct.ScmObj* %argslist49152$k431841, %struct.ScmObj** %stackaddr$prim50169, align 8
%stackaddr$prim50170 = alloca %struct.ScmObj*, align 8
%argslist49152$k431842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45712, %struct.ScmObj* %argslist49152$k431841)
store volatile %struct.ScmObj* %argslist49152$k431842, %struct.ScmObj** %stackaddr$prim50170, align 8
%clofunc50171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43184)
musttail call tailcc void %clofunc50171(%struct.ScmObj* %k43184, %struct.ScmObj* %argslist49152$k431842)
ret void
falsebranch$cmp50167:
%ae45723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50172 = alloca %struct.ScmObj*, align 8
%anf_45bind43102 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45723)
store volatile %struct.ScmObj* %anf_45bind43102, %struct.ScmObj** %stackaddr$prim50172, align 8
%stackaddr$prim50173 = alloca %struct.ScmObj*, align 8
%anf_45bind43103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43102)
store volatile %struct.ScmObj* %anf_45bind43103, %struct.ScmObj** %stackaddr$prim50173, align 8
%ae45726 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50174 = alloca %struct.ScmObj*, align 8
%_95042979 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45726, %struct.ScmObj* %anf_45bind43103)
store volatile %struct.ScmObj* %_95042979, %struct.ScmObj** %stackaddr$prim50174, align 8
%ae45729 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50175 = alloca %struct.ScmObj*, align 8
%anf_45bind43104 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45729)
store volatile %struct.ScmObj* %anf_45bind43104, %struct.ScmObj** %stackaddr$prim50175, align 8
%ae45731 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50176 = alloca %struct.ScmObj*, align 8
%anf_45bind43105 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind43104, %struct.ScmObj* %ae45731)
store volatile %struct.ScmObj* %anf_45bind43105, %struct.ScmObj** %stackaddr$prim50176, align 8
%ae45733 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50177 = alloca %struct.ScmObj*, align 8
%_95142978 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45733, %struct.ScmObj* %anf_45bind43105)
store volatile %struct.ScmObj* %_95142978, %struct.ScmObj** %stackaddr$prim50177, align 8
%argslist49153$cc429760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50178 = alloca %struct.ScmObj*, align 8
%argslist49153$cc429761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42976, %struct.ScmObj* %argslist49153$cc429760)
store volatile %struct.ScmObj* %argslist49153$cc429761, %struct.ScmObj** %stackaddr$prim50178, align 8
%stackaddr$prim50179 = alloca %struct.ScmObj*, align 8
%argslist49153$cc429762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43184, %struct.ScmObj* %argslist49153$cc429761)
store volatile %struct.ScmObj* %argslist49153$cc429762, %struct.ScmObj** %stackaddr$prim50179, align 8
%clofunc50180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42976)
musttail call tailcc void %clofunc50180(%struct.ScmObj* %cc42976, %struct.ScmObj* %argslist49153$cc429762)
ret void
}

define tailcc void @proc_clo$ae45563(%struct.ScmObj* %env$ae45563,%struct.ScmObj* %current_45args49154) {
%stackaddr$env-ref50181 = alloca %struct.ScmObj*, align 8
%k43184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45563, i64 0)
store %struct.ScmObj* %k43184, %struct.ScmObj** %stackaddr$env-ref50181
%stackaddr$env-ref50182 = alloca %struct.ScmObj*, align 8
%n42975 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45563, i64 1)
store %struct.ScmObj* %n42975, %struct.ScmObj** %stackaddr$env-ref50182
%stackaddr$env-ref50183 = alloca %struct.ScmObj*, align 8
%lst42974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45563, i64 2)
store %struct.ScmObj* %lst42974, %struct.ScmObj** %stackaddr$env-ref50183
%stackaddr$prim50184 = alloca %struct.ScmObj*, align 8
%_95k43186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49154)
store volatile %struct.ScmObj* %_95k43186, %struct.ScmObj** %stackaddr$prim50184, align 8
%stackaddr$prim50185 = alloca %struct.ScmObj*, align 8
%current_45args49155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49154)
store volatile %struct.ScmObj* %current_45args49155, %struct.ScmObj** %stackaddr$prim50185, align 8
%stackaddr$prim50186 = alloca %struct.ScmObj*, align 8
%cc42976 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49155)
store volatile %struct.ScmObj* %cc42976, %struct.ScmObj** %stackaddr$prim50186, align 8
%ae45565 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50187 = alloca %struct.ScmObj*, align 8
%anf_45bind43100 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45565)
store volatile %struct.ScmObj* %anf_45bind43100, %struct.ScmObj** %stackaddr$prim50187, align 8
%ae45566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50188 = alloca %struct.ScmObj*, align 8
%anf_45bind43101 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae45566, %struct.ScmObj* %anf_45bind43100)
store volatile %struct.ScmObj* %anf_45bind43101, %struct.ScmObj** %stackaddr$prim50188, align 8
%truthy$cmp50189 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43101)
%cmp$cmp50189 = icmp eq i64 %truthy$cmp50189, 1
br i1 %cmp$cmp50189, label %truebranch$cmp50189, label %falsebranch$cmp50189
truebranch$cmp50189:
%ae45570 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50190 = alloca %struct.ScmObj*, align 8
%cpsprim43187 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45570)
store volatile %struct.ScmObj* %cpsprim43187, %struct.ScmObj** %stackaddr$prim50190, align 8
%ae45572 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49157$k431840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50191 = alloca %struct.ScmObj*, align 8
%argslist49157$k431841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43187, %struct.ScmObj* %argslist49157$k431840)
store volatile %struct.ScmObj* %argslist49157$k431841, %struct.ScmObj** %stackaddr$prim50191, align 8
%stackaddr$prim50192 = alloca %struct.ScmObj*, align 8
%argslist49157$k431842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45572, %struct.ScmObj* %argslist49157$k431841)
store volatile %struct.ScmObj* %argslist49157$k431842, %struct.ScmObj** %stackaddr$prim50192, align 8
%clofunc50193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43184)
musttail call tailcc void %clofunc50193(%struct.ScmObj* %k43184, %struct.ScmObj* %argslist49157$k431842)
ret void
falsebranch$cmp50189:
%ae45583 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50194 = alloca %struct.ScmObj*, align 8
%anf_45bind43102 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45583)
store volatile %struct.ScmObj* %anf_45bind43102, %struct.ScmObj** %stackaddr$prim50194, align 8
%stackaddr$prim50195 = alloca %struct.ScmObj*, align 8
%anf_45bind43103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43102)
store volatile %struct.ScmObj* %anf_45bind43103, %struct.ScmObj** %stackaddr$prim50195, align 8
%ae45586 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50196 = alloca %struct.ScmObj*, align 8
%_95042979 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst42974, %struct.ScmObj* %ae45586, %struct.ScmObj* %anf_45bind43103)
store volatile %struct.ScmObj* %_95042979, %struct.ScmObj** %stackaddr$prim50196, align 8
%ae45589 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50197 = alloca %struct.ScmObj*, align 8
%anf_45bind43104 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45589)
store volatile %struct.ScmObj* %anf_45bind43104, %struct.ScmObj** %stackaddr$prim50197, align 8
%ae45591 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50198 = alloca %struct.ScmObj*, align 8
%anf_45bind43105 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind43104, %struct.ScmObj* %ae45591)
store volatile %struct.ScmObj* %anf_45bind43105, %struct.ScmObj** %stackaddr$prim50198, align 8
%ae45593 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50199 = alloca %struct.ScmObj*, align 8
%_95142978 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n42975, %struct.ScmObj* %ae45593, %struct.ScmObj* %anf_45bind43105)
store volatile %struct.ScmObj* %_95142978, %struct.ScmObj** %stackaddr$prim50199, align 8
%argslist49158$cc429760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50200 = alloca %struct.ScmObj*, align 8
%argslist49158$cc429761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42976, %struct.ScmObj* %argslist49158$cc429760)
store volatile %struct.ScmObj* %argslist49158$cc429761, %struct.ScmObj** %stackaddr$prim50200, align 8
%stackaddr$prim50201 = alloca %struct.ScmObj*, align 8
%argslist49158$cc429762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43184, %struct.ScmObj* %argslist49158$cc429761)
store volatile %struct.ScmObj* %argslist49158$cc429762, %struct.ScmObj** %stackaddr$prim50201, align 8
%clofunc50202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42976)
musttail call tailcc void %clofunc50202(%struct.ScmObj* %cc42976, %struct.ScmObj* %argslist49158$cc429762)
ret void
}

define tailcc void @proc_clo$ae45548(%struct.ScmObj* %env$ae45548,%struct.ScmObj* %current_45args49160) {
%stackaddr$prim50203 = alloca %struct.ScmObj*, align 8
%k43188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49160)
store volatile %struct.ScmObj* %k43188, %struct.ScmObj** %stackaddr$prim50203, align 8
%stackaddr$prim50204 = alloca %struct.ScmObj*, align 8
%current_45args49161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49160)
store volatile %struct.ScmObj* %current_45args49161, %struct.ScmObj** %stackaddr$prim50204, align 8
%stackaddr$prim50205 = alloca %struct.ScmObj*, align 8
%u42977 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49161)
store volatile %struct.ScmObj* %u42977, %struct.ScmObj** %stackaddr$prim50205, align 8
%argslist49163$u429770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50206 = alloca %struct.ScmObj*, align 8
%argslist49163$u429771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u42977, %struct.ScmObj* %argslist49163$u429770)
store volatile %struct.ScmObj* %argslist49163$u429771, %struct.ScmObj** %stackaddr$prim50206, align 8
%stackaddr$prim50207 = alloca %struct.ScmObj*, align 8
%argslist49163$u429772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43188, %struct.ScmObj* %argslist49163$u429771)
store volatile %struct.ScmObj* %argslist49163$u429772, %struct.ScmObj** %stackaddr$prim50207, align 8
%clofunc50208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u42977)
musttail call tailcc void %clofunc50208(%struct.ScmObj* %u42977, %struct.ScmObj* %argslist49163$u429772)
ret void
}

define tailcc void @proc_clo$ae45125(%struct.ScmObj* %env$ae45125,%struct.ScmObj* %current_45args49166) {
%stackaddr$prim50209 = alloca %struct.ScmObj*, align 8
%k43189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49166)
store volatile %struct.ScmObj* %k43189, %struct.ScmObj** %stackaddr$prim50209, align 8
%stackaddr$prim50210 = alloca %struct.ScmObj*, align 8
%current_45args49167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49166)
store volatile %struct.ScmObj* %current_45args49167, %struct.ScmObj** %stackaddr$prim50210, align 8
%stackaddr$prim50211 = alloca %struct.ScmObj*, align 8
%a42981 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49167)
store volatile %struct.ScmObj* %a42981, %struct.ScmObj** %stackaddr$prim50211, align 8
%ae45126 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50212 = alloca %struct.ScmObj*, align 8
%a42982 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae45126, %struct.ScmObj* %a42981)
store volatile %struct.ScmObj* %a42982, %struct.ScmObj** %stackaddr$prim50212, align 8
%stackaddr$makeclosure50213 = alloca %struct.ScmObj*, align 8
%fptrToInt50214 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45128 to i64
%ae45128 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50214)
store volatile %struct.ScmObj* %ae45128, %struct.ScmObj** %stackaddr$makeclosure50213, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45128, %struct.ScmObj* %k43189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45128, %struct.ScmObj* %a42982, i64 1)
%ae45129 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50215 = alloca %struct.ScmObj*, align 8
%fptrToInt50216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45130 to i64
%ae45130 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50216)
store volatile %struct.ScmObj* %ae45130, %struct.ScmObj** %stackaddr$makeclosure50215, align 8
%argslist49189$ae451280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50217 = alloca %struct.ScmObj*, align 8
%argslist49189$ae451281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45130, %struct.ScmObj* %argslist49189$ae451280)
store volatile %struct.ScmObj* %argslist49189$ae451281, %struct.ScmObj** %stackaddr$prim50217, align 8
%stackaddr$prim50218 = alloca %struct.ScmObj*, align 8
%argslist49189$ae451282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45129, %struct.ScmObj* %argslist49189$ae451281)
store volatile %struct.ScmObj* %argslist49189$ae451282, %struct.ScmObj** %stackaddr$prim50218, align 8
%clofunc50219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45128)
musttail call tailcc void %clofunc50219(%struct.ScmObj* %ae45128, %struct.ScmObj* %argslist49189$ae451282)
ret void
}

define tailcc void @proc_clo$ae45128(%struct.ScmObj* %env$ae45128,%struct.ScmObj* %current_45args49169) {
%stackaddr$env-ref50220 = alloca %struct.ScmObj*, align 8
%k43189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45128, i64 0)
store %struct.ScmObj* %k43189, %struct.ScmObj** %stackaddr$env-ref50220
%stackaddr$env-ref50221 = alloca %struct.ScmObj*, align 8
%a42982 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45128, i64 1)
store %struct.ScmObj* %a42982, %struct.ScmObj** %stackaddr$env-ref50221
%stackaddr$prim50222 = alloca %struct.ScmObj*, align 8
%_95k43190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49169)
store volatile %struct.ScmObj* %_95k43190, %struct.ScmObj** %stackaddr$prim50222, align 8
%stackaddr$prim50223 = alloca %struct.ScmObj*, align 8
%current_45args49170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49169)
store volatile %struct.ScmObj* %current_45args49170, %struct.ScmObj** %stackaddr$prim50223, align 8
%stackaddr$prim50224 = alloca %struct.ScmObj*, align 8
%anf_45bind43091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49170)
store volatile %struct.ScmObj* %anf_45bind43091, %struct.ScmObj** %stackaddr$prim50224, align 8
%stackaddr$makeclosure50225 = alloca %struct.ScmObj*, align 8
%fptrToInt50226 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45147 to i64
%ae45147 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50226)
store volatile %struct.ScmObj* %ae45147, %struct.ScmObj** %stackaddr$makeclosure50225, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45147, %struct.ScmObj* %k43189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45147, %struct.ScmObj* %a42982, i64 1)
%stackaddr$makeclosure50227 = alloca %struct.ScmObj*, align 8
%fptrToInt50228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45148 to i64
%ae45148 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50228)
store volatile %struct.ScmObj* %ae45148, %struct.ScmObj** %stackaddr$makeclosure50227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45148, %struct.ScmObj* %k43189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45148, %struct.ScmObj* %a42982, i64 1)
%argslist49184$anf_45bind430910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50229 = alloca %struct.ScmObj*, align 8
%argslist49184$anf_45bind430911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45148, %struct.ScmObj* %argslist49184$anf_45bind430910)
store volatile %struct.ScmObj* %argslist49184$anf_45bind430911, %struct.ScmObj** %stackaddr$prim50229, align 8
%stackaddr$prim50230 = alloca %struct.ScmObj*, align 8
%argslist49184$anf_45bind430912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45147, %struct.ScmObj* %argslist49184$anf_45bind430911)
store volatile %struct.ScmObj* %argslist49184$anf_45bind430912, %struct.ScmObj** %stackaddr$prim50230, align 8
%clofunc50231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43091)
musttail call tailcc void %clofunc50231(%struct.ScmObj* %anf_45bind43091, %struct.ScmObj* %argslist49184$anf_45bind430912)
ret void
}

define tailcc void @proc_clo$ae45147(%struct.ScmObj* %env$ae45147,%struct.ScmObj* %current_45args49172) {
%stackaddr$env-ref50232 = alloca %struct.ScmObj*, align 8
%k43189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45147, i64 0)
store %struct.ScmObj* %k43189, %struct.ScmObj** %stackaddr$env-ref50232
%stackaddr$env-ref50233 = alloca %struct.ScmObj*, align 8
%a42982 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45147, i64 1)
store %struct.ScmObj* %a42982, %struct.ScmObj** %stackaddr$env-ref50233
%stackaddr$prim50234 = alloca %struct.ScmObj*, align 8
%_95k43191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49172)
store volatile %struct.ScmObj* %_95k43191, %struct.ScmObj** %stackaddr$prim50234, align 8
%stackaddr$prim50235 = alloca %struct.ScmObj*, align 8
%current_45args49173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49172)
store volatile %struct.ScmObj* %current_45args49173, %struct.ScmObj** %stackaddr$prim50235, align 8
%stackaddr$prim50236 = alloca %struct.ScmObj*, align 8
%cc42983 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49173)
store volatile %struct.ScmObj* %cc42983, %struct.ScmObj** %stackaddr$prim50236, align 8
%ae45263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50237 = alloca %struct.ScmObj*, align 8
%anf_45bind43092 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45263)
store volatile %struct.ScmObj* %anf_45bind43092, %struct.ScmObj** %stackaddr$prim50237, align 8
%stackaddr$prim50238 = alloca %struct.ScmObj*, align 8
%anf_45bind43093 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind43092)
store volatile %struct.ScmObj* %anf_45bind43093, %struct.ScmObj** %stackaddr$prim50238, align 8
%truthy$cmp50239 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43093)
%cmp$cmp50239 = icmp eq i64 %truthy$cmp50239, 1
br i1 %cmp$cmp50239, label %truebranch$cmp50239, label %falsebranch$cmp50239
truebranch$cmp50239:
%ae45267 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45268 = call %struct.ScmObj* @const_init_true()
%argslist49175$k431890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50240 = alloca %struct.ScmObj*, align 8
%argslist49175$k431891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45268, %struct.ScmObj* %argslist49175$k431890)
store volatile %struct.ScmObj* %argslist49175$k431891, %struct.ScmObj** %stackaddr$prim50240, align 8
%stackaddr$prim50241 = alloca %struct.ScmObj*, align 8
%argslist49175$k431892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45267, %struct.ScmObj* %argslist49175$k431891)
store volatile %struct.ScmObj* %argslist49175$k431892, %struct.ScmObj** %stackaddr$prim50241, align 8
%clofunc50242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43189)
musttail call tailcc void %clofunc50242(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49175$k431892)
ret void
falsebranch$cmp50239:
%ae45276 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50243 = alloca %struct.ScmObj*, align 8
%anf_45bind43094 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45276)
store volatile %struct.ScmObj* %anf_45bind43094, %struct.ScmObj** %stackaddr$prim50243, align 8
%stackaddr$prim50244 = alloca %struct.ScmObj*, align 8
%anf_45bind43095 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind43094)
store volatile %struct.ScmObj* %anf_45bind43095, %struct.ScmObj** %stackaddr$prim50244, align 8
%truthy$cmp50245 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43095)
%cmp$cmp50245 = icmp eq i64 %truthy$cmp50245, 1
br i1 %cmp$cmp50245, label %truebranch$cmp50245, label %falsebranch$cmp50245
truebranch$cmp50245:
%ae45280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50246 = alloca %struct.ScmObj*, align 8
%anf_45bind43096 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45280)
store volatile %struct.ScmObj* %anf_45bind43096, %struct.ScmObj** %stackaddr$prim50246, align 8
%stackaddr$prim50247 = alloca %struct.ScmObj*, align 8
%b42985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43096)
store volatile %struct.ScmObj* %b42985, %struct.ScmObj** %stackaddr$prim50247, align 8
%ae45283 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50248 = alloca %struct.ScmObj*, align 8
%anf_45bind43097 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45283)
store volatile %struct.ScmObj* %anf_45bind43097, %struct.ScmObj** %stackaddr$prim50248, align 8
%stackaddr$prim50249 = alloca %struct.ScmObj*, align 8
%anf_45bind43098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43097)
store volatile %struct.ScmObj* %anf_45bind43098, %struct.ScmObj** %stackaddr$prim50249, align 8
%ae45286 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50250 = alloca %struct.ScmObj*, align 8
%_95042986 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45286, %struct.ScmObj* %anf_45bind43098)
store volatile %struct.ScmObj* %_95042986, %struct.ScmObj** %stackaddr$prim50250, align 8
%argslist49176$cc429830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50251 = alloca %struct.ScmObj*, align 8
%argslist49176$cc429831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42983, %struct.ScmObj* %argslist49176$cc429830)
store volatile %struct.ScmObj* %argslist49176$cc429831, %struct.ScmObj** %stackaddr$prim50251, align 8
%stackaddr$prim50252 = alloca %struct.ScmObj*, align 8
%argslist49176$cc429832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49176$cc429831)
store volatile %struct.ScmObj* %argslist49176$cc429832, %struct.ScmObj** %stackaddr$prim50252, align 8
%clofunc50253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42983)
musttail call tailcc void %clofunc50253(%struct.ScmObj* %cc42983, %struct.ScmObj* %argslist49176$cc429832)
ret void
falsebranch$cmp50245:
%ae45319 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45320 = call %struct.ScmObj* @const_init_false()
%argslist49177$k431890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50254 = alloca %struct.ScmObj*, align 8
%argslist49177$k431891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45320, %struct.ScmObj* %argslist49177$k431890)
store volatile %struct.ScmObj* %argslist49177$k431891, %struct.ScmObj** %stackaddr$prim50254, align 8
%stackaddr$prim50255 = alloca %struct.ScmObj*, align 8
%argslist49177$k431892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45319, %struct.ScmObj* %argslist49177$k431891)
store volatile %struct.ScmObj* %argslist49177$k431892, %struct.ScmObj** %stackaddr$prim50255, align 8
%clofunc50256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43189)
musttail call tailcc void %clofunc50256(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49177$k431892)
ret void
}

define tailcc void @proc_clo$ae45148(%struct.ScmObj* %env$ae45148,%struct.ScmObj* %current_45args49178) {
%stackaddr$env-ref50257 = alloca %struct.ScmObj*, align 8
%k43189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45148, i64 0)
store %struct.ScmObj* %k43189, %struct.ScmObj** %stackaddr$env-ref50257
%stackaddr$env-ref50258 = alloca %struct.ScmObj*, align 8
%a42982 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45148, i64 1)
store %struct.ScmObj* %a42982, %struct.ScmObj** %stackaddr$env-ref50258
%stackaddr$prim50259 = alloca %struct.ScmObj*, align 8
%_95k43191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49178)
store volatile %struct.ScmObj* %_95k43191, %struct.ScmObj** %stackaddr$prim50259, align 8
%stackaddr$prim50260 = alloca %struct.ScmObj*, align 8
%current_45args49179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49178)
store volatile %struct.ScmObj* %current_45args49179, %struct.ScmObj** %stackaddr$prim50260, align 8
%stackaddr$prim50261 = alloca %struct.ScmObj*, align 8
%cc42983 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49179)
store volatile %struct.ScmObj* %cc42983, %struct.ScmObj** %stackaddr$prim50261, align 8
%ae45150 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50262 = alloca %struct.ScmObj*, align 8
%anf_45bind43092 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45150)
store volatile %struct.ScmObj* %anf_45bind43092, %struct.ScmObj** %stackaddr$prim50262, align 8
%stackaddr$prim50263 = alloca %struct.ScmObj*, align 8
%anf_45bind43093 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind43092)
store volatile %struct.ScmObj* %anf_45bind43093, %struct.ScmObj** %stackaddr$prim50263, align 8
%truthy$cmp50264 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43093)
%cmp$cmp50264 = icmp eq i64 %truthy$cmp50264, 1
br i1 %cmp$cmp50264, label %truebranch$cmp50264, label %falsebranch$cmp50264
truebranch$cmp50264:
%ae45154 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45155 = call %struct.ScmObj* @const_init_true()
%argslist49181$k431890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50265 = alloca %struct.ScmObj*, align 8
%argslist49181$k431891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45155, %struct.ScmObj* %argslist49181$k431890)
store volatile %struct.ScmObj* %argslist49181$k431891, %struct.ScmObj** %stackaddr$prim50265, align 8
%stackaddr$prim50266 = alloca %struct.ScmObj*, align 8
%argslist49181$k431892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45154, %struct.ScmObj* %argslist49181$k431891)
store volatile %struct.ScmObj* %argslist49181$k431892, %struct.ScmObj** %stackaddr$prim50266, align 8
%clofunc50267 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43189)
musttail call tailcc void %clofunc50267(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49181$k431892)
ret void
falsebranch$cmp50264:
%ae45163 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50268 = alloca %struct.ScmObj*, align 8
%anf_45bind43094 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45163)
store volatile %struct.ScmObj* %anf_45bind43094, %struct.ScmObj** %stackaddr$prim50268, align 8
%stackaddr$prim50269 = alloca %struct.ScmObj*, align 8
%anf_45bind43095 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind43094)
store volatile %struct.ScmObj* %anf_45bind43095, %struct.ScmObj** %stackaddr$prim50269, align 8
%truthy$cmp50270 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43095)
%cmp$cmp50270 = icmp eq i64 %truthy$cmp50270, 1
br i1 %cmp$cmp50270, label %truebranch$cmp50270, label %falsebranch$cmp50270
truebranch$cmp50270:
%ae45167 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50271 = alloca %struct.ScmObj*, align 8
%anf_45bind43096 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45167)
store volatile %struct.ScmObj* %anf_45bind43096, %struct.ScmObj** %stackaddr$prim50271, align 8
%stackaddr$prim50272 = alloca %struct.ScmObj*, align 8
%b42985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43096)
store volatile %struct.ScmObj* %b42985, %struct.ScmObj** %stackaddr$prim50272, align 8
%ae45170 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50273 = alloca %struct.ScmObj*, align 8
%anf_45bind43097 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45170)
store volatile %struct.ScmObj* %anf_45bind43097, %struct.ScmObj** %stackaddr$prim50273, align 8
%stackaddr$prim50274 = alloca %struct.ScmObj*, align 8
%anf_45bind43098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43097)
store volatile %struct.ScmObj* %anf_45bind43098, %struct.ScmObj** %stackaddr$prim50274, align 8
%ae45173 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50275 = alloca %struct.ScmObj*, align 8
%_95042986 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a42982, %struct.ScmObj* %ae45173, %struct.ScmObj* %anf_45bind43098)
store volatile %struct.ScmObj* %_95042986, %struct.ScmObj** %stackaddr$prim50275, align 8
%argslist49182$cc429830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50276 = alloca %struct.ScmObj*, align 8
%argslist49182$cc429831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc42983, %struct.ScmObj* %argslist49182$cc429830)
store volatile %struct.ScmObj* %argslist49182$cc429831, %struct.ScmObj** %stackaddr$prim50276, align 8
%stackaddr$prim50277 = alloca %struct.ScmObj*, align 8
%argslist49182$cc429832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49182$cc429831)
store volatile %struct.ScmObj* %argslist49182$cc429832, %struct.ScmObj** %stackaddr$prim50277, align 8
%clofunc50278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc42983)
musttail call tailcc void %clofunc50278(%struct.ScmObj* %cc42983, %struct.ScmObj* %argslist49182$cc429832)
ret void
falsebranch$cmp50270:
%ae45206 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45207 = call %struct.ScmObj* @const_init_false()
%argslist49183$k431890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50279 = alloca %struct.ScmObj*, align 8
%argslist49183$k431891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45207, %struct.ScmObj* %argslist49183$k431890)
store volatile %struct.ScmObj* %argslist49183$k431891, %struct.ScmObj** %stackaddr$prim50279, align 8
%stackaddr$prim50280 = alloca %struct.ScmObj*, align 8
%argslist49183$k431892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45206, %struct.ScmObj* %argslist49183$k431891)
store volatile %struct.ScmObj* %argslist49183$k431892, %struct.ScmObj** %stackaddr$prim50280, align 8
%clofunc50281 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43189)
musttail call tailcc void %clofunc50281(%struct.ScmObj* %k43189, %struct.ScmObj* %argslist49183$k431892)
ret void
}

define tailcc void @proc_clo$ae45130(%struct.ScmObj* %env$ae45130,%struct.ScmObj* %current_45args49185) {
%stackaddr$prim50282 = alloca %struct.ScmObj*, align 8
%k43192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49185)
store volatile %struct.ScmObj* %k43192, %struct.ScmObj** %stackaddr$prim50282, align 8
%stackaddr$prim50283 = alloca %struct.ScmObj*, align 8
%current_45args49186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49185)
store volatile %struct.ScmObj* %current_45args49186, %struct.ScmObj** %stackaddr$prim50283, align 8
%stackaddr$prim50284 = alloca %struct.ScmObj*, align 8
%k42984 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49186)
store volatile %struct.ScmObj* %k42984, %struct.ScmObj** %stackaddr$prim50284, align 8
%ae45132 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49188$k431920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50285 = alloca %struct.ScmObj*, align 8
%argslist49188$k431921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k42984, %struct.ScmObj* %argslist49188$k431920)
store volatile %struct.ScmObj* %argslist49188$k431921, %struct.ScmObj** %stackaddr$prim50285, align 8
%stackaddr$prim50286 = alloca %struct.ScmObj*, align 8
%argslist49188$k431922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45132, %struct.ScmObj* %argslist49188$k431921)
store volatile %struct.ScmObj* %argslist49188$k431922, %struct.ScmObj** %stackaddr$prim50286, align 8
%clofunc50287 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43192)
musttail call tailcc void %clofunc50287(%struct.ScmObj* %k43192, %struct.ScmObj* %argslist49188$k431922)
ret void
}

define tailcc void @proc_clo$ae45053(%struct.ScmObj* %env$ae45053,%struct.ScmObj* %current_45args49191) {
%stackaddr$env-ref50288 = alloca %struct.ScmObj*, align 8
%_37append42988 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45053, i64 0)
store %struct.ScmObj* %_37append42988, %struct.ScmObj** %stackaddr$env-ref50288
%stackaddr$prim50289 = alloca %struct.ScmObj*, align 8
%k43193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49191)
store volatile %struct.ScmObj* %k43193, %struct.ScmObj** %stackaddr$prim50289, align 8
%stackaddr$prim50290 = alloca %struct.ScmObj*, align 8
%current_45args49192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49191)
store volatile %struct.ScmObj* %current_45args49192, %struct.ScmObj** %stackaddr$prim50290, align 8
%stackaddr$prim50291 = alloca %struct.ScmObj*, align 8
%ls042991 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49192)
store volatile %struct.ScmObj* %ls042991, %struct.ScmObj** %stackaddr$prim50291, align 8
%stackaddr$prim50292 = alloca %struct.ScmObj*, align 8
%current_45args49193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49192)
store volatile %struct.ScmObj* %current_45args49193, %struct.ScmObj** %stackaddr$prim50292, align 8
%stackaddr$prim50293 = alloca %struct.ScmObj*, align 8
%ls142990 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49193)
store volatile %struct.ScmObj* %ls142990, %struct.ScmObj** %stackaddr$prim50293, align 8
%stackaddr$prim50294 = alloca %struct.ScmObj*, align 8
%anf_45bind43085 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls042991)
store volatile %struct.ScmObj* %anf_45bind43085, %struct.ScmObj** %stackaddr$prim50294, align 8
%truthy$cmp50295 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43085)
%cmp$cmp50295 = icmp eq i64 %truthy$cmp50295, 1
br i1 %cmp$cmp50295, label %truebranch$cmp50295, label %falsebranch$cmp50295
truebranch$cmp50295:
%ae45057 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49195$k431930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50296 = alloca %struct.ScmObj*, align 8
%argslist49195$k431931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls142990, %struct.ScmObj* %argslist49195$k431930)
store volatile %struct.ScmObj* %argslist49195$k431931, %struct.ScmObj** %stackaddr$prim50296, align 8
%stackaddr$prim50297 = alloca %struct.ScmObj*, align 8
%argslist49195$k431932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45057, %struct.ScmObj* %argslist49195$k431931)
store volatile %struct.ScmObj* %argslist49195$k431932, %struct.ScmObj** %stackaddr$prim50297, align 8
%clofunc50298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43193)
musttail call tailcc void %clofunc50298(%struct.ScmObj* %k43193, %struct.ScmObj* %argslist49195$k431932)
ret void
falsebranch$cmp50295:
%stackaddr$prim50299 = alloca %struct.ScmObj*, align 8
%anf_45bind43086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls042991)
store volatile %struct.ScmObj* %anf_45bind43086, %struct.ScmObj** %stackaddr$prim50299, align 8
%ae45064 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50300 = alloca %struct.ScmObj*, align 8
%anf_45bind43087 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append42988, %struct.ScmObj* %ae45064)
store volatile %struct.ScmObj* %anf_45bind43087, %struct.ScmObj** %stackaddr$prim50300, align 8
%stackaddr$prim50301 = alloca %struct.ScmObj*, align 8
%anf_45bind43088 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls042991)
store volatile %struct.ScmObj* %anf_45bind43088, %struct.ScmObj** %stackaddr$prim50301, align 8
%stackaddr$makeclosure50302 = alloca %struct.ScmObj*, align 8
%fptrToInt50303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45067 to i64
%ae45067 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50303)
store volatile %struct.ScmObj* %ae45067, %struct.ScmObj** %stackaddr$makeclosure50302, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45067, %struct.ScmObj* %anf_45bind43086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae45067, %struct.ScmObj* %k43193, i64 1)
%argslist49200$anf_45bind430870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50304 = alloca %struct.ScmObj*, align 8
%argslist49200$anf_45bind430871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls142990, %struct.ScmObj* %argslist49200$anf_45bind430870)
store volatile %struct.ScmObj* %argslist49200$anf_45bind430871, %struct.ScmObj** %stackaddr$prim50304, align 8
%stackaddr$prim50305 = alloca %struct.ScmObj*, align 8
%argslist49200$anf_45bind430872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43088, %struct.ScmObj* %argslist49200$anf_45bind430871)
store volatile %struct.ScmObj* %argslist49200$anf_45bind430872, %struct.ScmObj** %stackaddr$prim50305, align 8
%stackaddr$prim50306 = alloca %struct.ScmObj*, align 8
%argslist49200$anf_45bind430873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45067, %struct.ScmObj* %argslist49200$anf_45bind430872)
store volatile %struct.ScmObj* %argslist49200$anf_45bind430873, %struct.ScmObj** %stackaddr$prim50306, align 8
%clofunc50307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43087)
musttail call tailcc void %clofunc50307(%struct.ScmObj* %anf_45bind43087, %struct.ScmObj* %argslist49200$anf_45bind430873)
ret void
}

define tailcc void @proc_clo$ae45067(%struct.ScmObj* %env$ae45067,%struct.ScmObj* %current_45args49196) {
%stackaddr$env-ref50308 = alloca %struct.ScmObj*, align 8
%anf_45bind43086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45067, i64 0)
store %struct.ScmObj* %anf_45bind43086, %struct.ScmObj** %stackaddr$env-ref50308
%stackaddr$env-ref50309 = alloca %struct.ScmObj*, align 8
%k43193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45067, i64 1)
store %struct.ScmObj* %k43193, %struct.ScmObj** %stackaddr$env-ref50309
%stackaddr$prim50310 = alloca %struct.ScmObj*, align 8
%_95k43194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49196)
store volatile %struct.ScmObj* %_95k43194, %struct.ScmObj** %stackaddr$prim50310, align 8
%stackaddr$prim50311 = alloca %struct.ScmObj*, align 8
%current_45args49197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49196)
store volatile %struct.ScmObj* %current_45args49197, %struct.ScmObj** %stackaddr$prim50311, align 8
%stackaddr$prim50312 = alloca %struct.ScmObj*, align 8
%anf_45bind43089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49197)
store volatile %struct.ScmObj* %anf_45bind43089, %struct.ScmObj** %stackaddr$prim50312, align 8
%stackaddr$prim50313 = alloca %struct.ScmObj*, align 8
%cpsprim43195 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43086, %struct.ScmObj* %anf_45bind43089)
store volatile %struct.ScmObj* %cpsprim43195, %struct.ScmObj** %stackaddr$prim50313, align 8
%ae45073 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49199$k431930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%argslist49199$k431931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43195, %struct.ScmObj* %argslist49199$k431930)
store volatile %struct.ScmObj* %argslist49199$k431931, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%argslist49199$k431932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45073, %struct.ScmObj* %argslist49199$k431931)
store volatile %struct.ScmObj* %argslist49199$k431932, %struct.ScmObj** %stackaddr$prim50315, align 8
%clofunc50316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43193)
musttail call tailcc void %clofunc50316(%struct.ScmObj* %k43193, %struct.ScmObj* %argslist49199$k431932)
ret void
}

define tailcc void @proc_clo$ae45027(%struct.ScmObj* %env$ae45027,%struct.ScmObj* %current_45args49202) {
%stackaddr$prim50317 = alloca %struct.ScmObj*, align 8
%k43196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49202)
store volatile %struct.ScmObj* %k43196, %struct.ScmObj** %stackaddr$prim50317, align 8
%stackaddr$prim50318 = alloca %struct.ScmObj*, align 8
%current_45args49203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49202)
store volatile %struct.ScmObj* %current_45args49203, %struct.ScmObj** %stackaddr$prim50318, align 8
%stackaddr$prim50319 = alloca %struct.ScmObj*, align 8
%a42994 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49203)
store volatile %struct.ScmObj* %a42994, %struct.ScmObj** %stackaddr$prim50319, align 8
%stackaddr$prim50320 = alloca %struct.ScmObj*, align 8
%current_45args49204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49203)
store volatile %struct.ScmObj* %current_45args49204, %struct.ScmObj** %stackaddr$prim50320, align 8
%stackaddr$prim50321 = alloca %struct.ScmObj*, align 8
%b42993 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49204)
store volatile %struct.ScmObj* %b42993, %struct.ScmObj** %stackaddr$prim50321, align 8
%stackaddr$prim50322 = alloca %struct.ScmObj*, align 8
%anf_45bind43084 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a42994, %struct.ScmObj* %b42993)
store volatile %struct.ScmObj* %anf_45bind43084, %struct.ScmObj** %stackaddr$prim50322, align 8
%stackaddr$prim50323 = alloca %struct.ScmObj*, align 8
%cpsprim43197 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind43084)
store volatile %struct.ScmObj* %cpsprim43197, %struct.ScmObj** %stackaddr$prim50323, align 8
%ae45032 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49206$k431960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50324 = alloca %struct.ScmObj*, align 8
%argslist49206$k431961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43197, %struct.ScmObj* %argslist49206$k431960)
store volatile %struct.ScmObj* %argslist49206$k431961, %struct.ScmObj** %stackaddr$prim50324, align 8
%stackaddr$prim50325 = alloca %struct.ScmObj*, align 8
%argslist49206$k431962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45032, %struct.ScmObj* %argslist49206$k431961)
store volatile %struct.ScmObj* %argslist49206$k431962, %struct.ScmObj** %stackaddr$prim50325, align 8
%clofunc50326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43196)
musttail call tailcc void %clofunc50326(%struct.ScmObj* %k43196, %struct.ScmObj* %argslist49206$k431962)
ret void
}

define tailcc void @proc_clo$ae45003(%struct.ScmObj* %env$ae45003,%struct.ScmObj* %current_45args49208) {
%stackaddr$prim50327 = alloca %struct.ScmObj*, align 8
%k43198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49208)
store volatile %struct.ScmObj* %k43198, %struct.ScmObj** %stackaddr$prim50327, align 8
%stackaddr$prim50328 = alloca %struct.ScmObj*, align 8
%current_45args49209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49208)
store volatile %struct.ScmObj* %current_45args49209, %struct.ScmObj** %stackaddr$prim50328, align 8
%stackaddr$prim50329 = alloca %struct.ScmObj*, align 8
%a42997 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49209)
store volatile %struct.ScmObj* %a42997, %struct.ScmObj** %stackaddr$prim50329, align 8
%stackaddr$prim50330 = alloca %struct.ScmObj*, align 8
%current_45args49210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49209)
store volatile %struct.ScmObj* %current_45args49210, %struct.ScmObj** %stackaddr$prim50330, align 8
%stackaddr$prim50331 = alloca %struct.ScmObj*, align 8
%b42996 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49210)
store volatile %struct.ScmObj* %b42996, %struct.ScmObj** %stackaddr$prim50331, align 8
%stackaddr$prim50332 = alloca %struct.ScmObj*, align 8
%anf_45bind43083 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a42997, %struct.ScmObj* %b42996)
store volatile %struct.ScmObj* %anf_45bind43083, %struct.ScmObj** %stackaddr$prim50332, align 8
%stackaddr$prim50333 = alloca %struct.ScmObj*, align 8
%cpsprim43199 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind43083)
store volatile %struct.ScmObj* %cpsprim43199, %struct.ScmObj** %stackaddr$prim50333, align 8
%ae45008 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49212$k431980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50334 = alloca %struct.ScmObj*, align 8
%argslist49212$k431981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43199, %struct.ScmObj* %argslist49212$k431980)
store volatile %struct.ScmObj* %argslist49212$k431981, %struct.ScmObj** %stackaddr$prim50334, align 8
%stackaddr$prim50335 = alloca %struct.ScmObj*, align 8
%argslist49212$k431982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45008, %struct.ScmObj* %argslist49212$k431981)
store volatile %struct.ScmObj* %argslist49212$k431982, %struct.ScmObj** %stackaddr$prim50335, align 8
%clofunc50336 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43198)
musttail call tailcc void %clofunc50336(%struct.ScmObj* %k43198, %struct.ScmObj* %argslist49212$k431982)
ret void
}

define tailcc void @proc_clo$ae44609(%struct.ScmObj* %env$ae44609,%struct.ScmObj* %current_45args49215) {
%stackaddr$env-ref50337 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44609, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50337
%stackaddr$env-ref50338 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44609, i64 1)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50338
%stackaddr$env-ref50339 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44609, i64 2)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50339
%stackaddr$prim50340 = alloca %struct.ScmObj*, align 8
%k43200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49215)
store volatile %struct.ScmObj* %k43200, %struct.ScmObj** %stackaddr$prim50340, align 8
%stackaddr$prim50341 = alloca %struct.ScmObj*, align 8
%current_45args49216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49215)
store volatile %struct.ScmObj* %current_45args49216, %struct.ScmObj** %stackaddr$prim50341, align 8
%stackaddr$prim50342 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49216)
store volatile %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$prim50342, align 8
%ae44611 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50343 = alloca %struct.ScmObj*, align 8
%fptrToInt50344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44612 to i64
%ae44612 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50344)
store volatile %struct.ScmObj* %ae44612, %struct.ScmObj** %stackaddr$makeclosure50343, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44612, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44612, %struct.ScmObj* %_37map142947, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44612, %struct.ScmObj* %_37foldr42921, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44612, %struct.ScmObj* %_37foldl42999, i64 3)
%argslist49273$k432000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50345 = alloca %struct.ScmObj*, align 8
%argslist49273$k432001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44612, %struct.ScmObj* %argslist49273$k432000)
store volatile %struct.ScmObj* %argslist49273$k432001, %struct.ScmObj** %stackaddr$prim50345, align 8
%stackaddr$prim50346 = alloca %struct.ScmObj*, align 8
%argslist49273$k432002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44611, %struct.ScmObj* %argslist49273$k432001)
store volatile %struct.ScmObj* %argslist49273$k432002, %struct.ScmObj** %stackaddr$prim50346, align 8
%clofunc50347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43200)
musttail call tailcc void %clofunc50347(%struct.ScmObj* %k43200, %struct.ScmObj* %argslist49273$k432002)
ret void
}

define tailcc void @proc_clo$ae44612(%struct.ScmObj* %env$ae44612,%struct.ScmObj* %args4300043201) {
%stackaddr$env-ref50348 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44612, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50348
%stackaddr$env-ref50349 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44612, i64 1)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50349
%stackaddr$env-ref50350 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44612, i64 2)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50350
%stackaddr$env-ref50351 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44612, i64 3)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50351
%stackaddr$prim50352 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4300043201)
store volatile %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$prim50352, align 8
%stackaddr$prim50353 = alloca %struct.ScmObj*, align 8
%args43000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4300043201)
store volatile %struct.ScmObj* %args43000, %struct.ScmObj** %stackaddr$prim50353, align 8
%stackaddr$prim50354 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args43000)
store volatile %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$prim50354, align 8
%stackaddr$prim50355 = alloca %struct.ScmObj*, align 8
%anf_45bind43071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args43000)
store volatile %struct.ScmObj* %anf_45bind43071, %struct.ScmObj** %stackaddr$prim50355, align 8
%stackaddr$prim50356 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43071)
store volatile %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$prim50356, align 8
%stackaddr$prim50357 = alloca %struct.ScmObj*, align 8
%anf_45bind43072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args43000)
store volatile %struct.ScmObj* %anf_45bind43072, %struct.ScmObj** %stackaddr$prim50357, align 8
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43072)
store volatile %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$makeclosure50359 = alloca %struct.ScmObj*, align 8
%fptrToInt50360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44620 to i64
%ae44620 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50360)
store volatile %struct.ScmObj* %ae44620, %struct.ScmObj** %stackaddr$makeclosure50359, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %lsts43001, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %_37foldr142916, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %_37map142947, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %k43202, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %f43003, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %acc43002, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44620, %struct.ScmObj* %_37foldl42999, i64 7)
%ae44621 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50361 = alloca %struct.ScmObj*, align 8
%fptrToInt50362 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44622 to i64
%ae44622 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50362)
store volatile %struct.ScmObj* %ae44622, %struct.ScmObj** %stackaddr$makeclosure50361, align 8
%argslist49272$ae446200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50363 = alloca %struct.ScmObj*, align 8
%argslist49272$ae446201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44622, %struct.ScmObj* %argslist49272$ae446200)
store volatile %struct.ScmObj* %argslist49272$ae446201, %struct.ScmObj** %stackaddr$prim50363, align 8
%stackaddr$prim50364 = alloca %struct.ScmObj*, align 8
%argslist49272$ae446202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44621, %struct.ScmObj* %argslist49272$ae446201)
store volatile %struct.ScmObj* %argslist49272$ae446202, %struct.ScmObj** %stackaddr$prim50364, align 8
%clofunc50365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44620)
musttail call tailcc void %clofunc50365(%struct.ScmObj* %ae44620, %struct.ScmObj* %argslist49272$ae446202)
ret void
}

define tailcc void @proc_clo$ae44620(%struct.ScmObj* %env$ae44620,%struct.ScmObj* %current_45args49218) {
%stackaddr$env-ref50366 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 0)
store %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$env-ref50366
%stackaddr$env-ref50367 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50367
%stackaddr$env-ref50368 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 2)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50368
%stackaddr$env-ref50369 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 3)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50369
%stackaddr$env-ref50370 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 4)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50370
%stackaddr$env-ref50371 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 5)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50371
%stackaddr$env-ref50372 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 6)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50372
%stackaddr$env-ref50373 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44620, i64 7)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50373
%stackaddr$prim50374 = alloca %struct.ScmObj*, align 8
%_95k43203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49218)
store volatile %struct.ScmObj* %_95k43203, %struct.ScmObj** %stackaddr$prim50374, align 8
%stackaddr$prim50375 = alloca %struct.ScmObj*, align 8
%current_45args49219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49218)
store volatile %struct.ScmObj* %current_45args49219, %struct.ScmObj** %stackaddr$prim50375, align 8
%stackaddr$prim50376 = alloca %struct.ScmObj*, align 8
%anf_45bind43073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49219)
store volatile %struct.ScmObj* %anf_45bind43073, %struct.ScmObj** %stackaddr$prim50376, align 8
%stackaddr$makeclosure50377 = alloca %struct.ScmObj*, align 8
%fptrToInt50378 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44652 to i64
%ae44652 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50378)
store volatile %struct.ScmObj* %ae44652, %struct.ScmObj** %stackaddr$makeclosure50377, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %lsts43001, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %_37map142947, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %k43202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %f43003, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %acc43002, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44652, %struct.ScmObj* %_37foldl42999, i64 6)
%ae44654 = call %struct.ScmObj* @const_init_false()
%argslist49265$_37foldr1429160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50379 = alloca %struct.ScmObj*, align 8
%argslist49265$_37foldr1429161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts43001, %struct.ScmObj* %argslist49265$_37foldr1429160)
store volatile %struct.ScmObj* %argslist49265$_37foldr1429161, %struct.ScmObj** %stackaddr$prim50379, align 8
%stackaddr$prim50380 = alloca %struct.ScmObj*, align 8
%argslist49265$_37foldr1429162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44654, %struct.ScmObj* %argslist49265$_37foldr1429161)
store volatile %struct.ScmObj* %argslist49265$_37foldr1429162, %struct.ScmObj** %stackaddr$prim50380, align 8
%stackaddr$prim50381 = alloca %struct.ScmObj*, align 8
%argslist49265$_37foldr1429163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43073, %struct.ScmObj* %argslist49265$_37foldr1429162)
store volatile %struct.ScmObj* %argslist49265$_37foldr1429163, %struct.ScmObj** %stackaddr$prim50381, align 8
%stackaddr$prim50382 = alloca %struct.ScmObj*, align 8
%argslist49265$_37foldr1429164 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44652, %struct.ScmObj* %argslist49265$_37foldr1429163)
store volatile %struct.ScmObj* %argslist49265$_37foldr1429164, %struct.ScmObj** %stackaddr$prim50382, align 8
%clofunc50383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr142916)
musttail call tailcc void %clofunc50383(%struct.ScmObj* %_37foldr142916, %struct.ScmObj* %argslist49265$_37foldr1429164)
ret void
}

define tailcc void @proc_clo$ae44652(%struct.ScmObj* %env$ae44652,%struct.ScmObj* %current_45args49221) {
%stackaddr$env-ref50384 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 0)
store %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$env-ref50384
%stackaddr$env-ref50385 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50385
%stackaddr$env-ref50386 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 2)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50386
%stackaddr$env-ref50387 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 3)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50387
%stackaddr$env-ref50388 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 4)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50388
%stackaddr$env-ref50389 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 5)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50389
%stackaddr$env-ref50390 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44652, i64 6)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50390
%stackaddr$prim50391 = alloca %struct.ScmObj*, align 8
%_95k43204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49221)
store volatile %struct.ScmObj* %_95k43204, %struct.ScmObj** %stackaddr$prim50391, align 8
%stackaddr$prim50392 = alloca %struct.ScmObj*, align 8
%current_45args49222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49221)
store volatile %struct.ScmObj* %current_45args49222, %struct.ScmObj** %stackaddr$prim50392, align 8
%stackaddr$prim50393 = alloca %struct.ScmObj*, align 8
%anf_45bind43074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49222)
store volatile %struct.ScmObj* %anf_45bind43074, %struct.ScmObj** %stackaddr$prim50393, align 8
%truthy$cmp50394 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43074)
%cmp$cmp50394 = icmp eq i64 %truthy$cmp50394, 1
br i1 %cmp$cmp50394, label %truebranch$cmp50394, label %falsebranch$cmp50394
truebranch$cmp50394:
%ae44663 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49224$k432020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50395 = alloca %struct.ScmObj*, align 8
%argslist49224$k432021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc43002, %struct.ScmObj* %argslist49224$k432020)
store volatile %struct.ScmObj* %argslist49224$k432021, %struct.ScmObj** %stackaddr$prim50395, align 8
%stackaddr$prim50396 = alloca %struct.ScmObj*, align 8
%argslist49224$k432022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44663, %struct.ScmObj* %argslist49224$k432021)
store volatile %struct.ScmObj* %argslist49224$k432022, %struct.ScmObj** %stackaddr$prim50396, align 8
%clofunc50397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43202)
musttail call tailcc void %clofunc50397(%struct.ScmObj* %k43202, %struct.ScmObj* %argslist49224$k432022)
ret void
falsebranch$cmp50394:
%stackaddr$makeclosure50398 = alloca %struct.ScmObj*, align 8
%fptrToInt50399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44668 to i64
%ae44668 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50399)
store volatile %struct.ScmObj* %ae44668, %struct.ScmObj** %stackaddr$makeclosure50398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %lsts43001, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %_37map142947, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %k43202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %f43003, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %acc43002, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44668, %struct.ScmObj* %_37foldl42999, i64 6)
%ae44669 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50400 = alloca %struct.ScmObj*, align 8
%fptrToInt50401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44670 to i64
%ae44670 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50401)
store volatile %struct.ScmObj* %ae44670, %struct.ScmObj** %stackaddr$makeclosure50400, align 8
%argslist49264$ae446680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50402 = alloca %struct.ScmObj*, align 8
%argslist49264$ae446681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44670, %struct.ScmObj* %argslist49264$ae446680)
store volatile %struct.ScmObj* %argslist49264$ae446681, %struct.ScmObj** %stackaddr$prim50402, align 8
%stackaddr$prim50403 = alloca %struct.ScmObj*, align 8
%argslist49264$ae446682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44669, %struct.ScmObj* %argslist49264$ae446681)
store volatile %struct.ScmObj* %argslist49264$ae446682, %struct.ScmObj** %stackaddr$prim50403, align 8
%clofunc50404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44668)
musttail call tailcc void %clofunc50404(%struct.ScmObj* %ae44668, %struct.ScmObj* %argslist49264$ae446682)
ret void
}

define tailcc void @proc_clo$ae44668(%struct.ScmObj* %env$ae44668,%struct.ScmObj* %current_45args49225) {
%stackaddr$env-ref50405 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 0)
store %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$env-ref50405
%stackaddr$env-ref50406 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50406
%stackaddr$env-ref50407 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 2)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50407
%stackaddr$env-ref50408 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 3)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50408
%stackaddr$env-ref50409 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 4)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50409
%stackaddr$env-ref50410 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 5)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50410
%stackaddr$env-ref50411 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44668, i64 6)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50411
%stackaddr$prim50412 = alloca %struct.ScmObj*, align 8
%_95k43205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49225)
store volatile %struct.ScmObj* %_95k43205, %struct.ScmObj** %stackaddr$prim50412, align 8
%stackaddr$prim50413 = alloca %struct.ScmObj*, align 8
%current_45args49226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49225)
store volatile %struct.ScmObj* %current_45args49226, %struct.ScmObj** %stackaddr$prim50413, align 8
%stackaddr$prim50414 = alloca %struct.ScmObj*, align 8
%anf_45bind43075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49226)
store volatile %struct.ScmObj* %anf_45bind43075, %struct.ScmObj** %stackaddr$prim50414, align 8
%stackaddr$makeclosure50415 = alloca %struct.ScmObj*, align 8
%fptrToInt50416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44689 to i64
%ae44689 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50416)
store volatile %struct.ScmObj* %ae44689, %struct.ScmObj** %stackaddr$makeclosure50415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %lsts43001, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %_37map142947, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %k43202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %f43003, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %acc43002, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44689, %struct.ScmObj* %_37foldl42999, i64 6)
%argslist49259$_37map1429470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50417 = alloca %struct.ScmObj*, align 8
%argslist49259$_37map1429471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts43001, %struct.ScmObj* %argslist49259$_37map1429470)
store volatile %struct.ScmObj* %argslist49259$_37map1429471, %struct.ScmObj** %stackaddr$prim50417, align 8
%stackaddr$prim50418 = alloca %struct.ScmObj*, align 8
%argslist49259$_37map1429472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43075, %struct.ScmObj* %argslist49259$_37map1429471)
store volatile %struct.ScmObj* %argslist49259$_37map1429472, %struct.ScmObj** %stackaddr$prim50418, align 8
%stackaddr$prim50419 = alloca %struct.ScmObj*, align 8
%argslist49259$_37map1429473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44689, %struct.ScmObj* %argslist49259$_37map1429472)
store volatile %struct.ScmObj* %argslist49259$_37map1429473, %struct.ScmObj** %stackaddr$prim50419, align 8
%clofunc50420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map142947)
musttail call tailcc void %clofunc50420(%struct.ScmObj* %_37map142947, %struct.ScmObj* %argslist49259$_37map1429473)
ret void
}

define tailcc void @proc_clo$ae44689(%struct.ScmObj* %env$ae44689,%struct.ScmObj* %current_45args49228) {
%stackaddr$env-ref50421 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 0)
store %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$env-ref50421
%stackaddr$env-ref50422 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50422
%stackaddr$env-ref50423 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 2)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50423
%stackaddr$env-ref50424 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 3)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50424
%stackaddr$env-ref50425 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 4)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50425
%stackaddr$env-ref50426 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 5)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50426
%stackaddr$env-ref50427 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44689, i64 6)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50427
%stackaddr$prim50428 = alloca %struct.ScmObj*, align 8
%_95k43206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49228)
store volatile %struct.ScmObj* %_95k43206, %struct.ScmObj** %stackaddr$prim50428, align 8
%stackaddr$prim50429 = alloca %struct.ScmObj*, align 8
%current_45args49229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49228)
store volatile %struct.ScmObj* %current_45args49229, %struct.ScmObj** %stackaddr$prim50429, align 8
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49229)
store volatile %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$prim50430, align 8
%stackaddr$makeclosure50431 = alloca %struct.ScmObj*, align 8
%fptrToInt50432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44692 to i64
%ae44692 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50432)
store volatile %struct.ScmObj* %ae44692, %struct.ScmObj** %stackaddr$makeclosure50431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %lsts43001, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %_37map142947, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %k43202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %lsts_4343008, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %f43003, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %acc43002, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44692, %struct.ScmObj* %_37foldl42999, i64 7)
%ae44693 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50433 = alloca %struct.ScmObj*, align 8
%fptrToInt50434 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44694 to i64
%ae44694 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50434)
store volatile %struct.ScmObj* %ae44694, %struct.ScmObj** %stackaddr$makeclosure50433, align 8
%argslist49258$ae446920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50435 = alloca %struct.ScmObj*, align 8
%argslist49258$ae446921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44694, %struct.ScmObj* %argslist49258$ae446920)
store volatile %struct.ScmObj* %argslist49258$ae446921, %struct.ScmObj** %stackaddr$prim50435, align 8
%stackaddr$prim50436 = alloca %struct.ScmObj*, align 8
%argslist49258$ae446922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44693, %struct.ScmObj* %argslist49258$ae446921)
store volatile %struct.ScmObj* %argslist49258$ae446922, %struct.ScmObj** %stackaddr$prim50436, align 8
%clofunc50437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44692)
musttail call tailcc void %clofunc50437(%struct.ScmObj* %ae44692, %struct.ScmObj* %argslist49258$ae446922)
ret void
}

define tailcc void @proc_clo$ae44692(%struct.ScmObj* %env$ae44692,%struct.ScmObj* %current_45args49231) {
%stackaddr$env-ref50438 = alloca %struct.ScmObj*, align 8
%lsts43001 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 0)
store %struct.ScmObj* %lsts43001, %struct.ScmObj** %stackaddr$env-ref50438
%stackaddr$env-ref50439 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50439
%stackaddr$env-ref50440 = alloca %struct.ScmObj*, align 8
%_37map142947 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 2)
store %struct.ScmObj* %_37map142947, %struct.ScmObj** %stackaddr$env-ref50440
%stackaddr$env-ref50441 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 3)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50441
%stackaddr$env-ref50442 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 4)
store %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$env-ref50442
%stackaddr$env-ref50443 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 5)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50443
%stackaddr$env-ref50444 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 6)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50444
%stackaddr$env-ref50445 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44692, i64 7)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50445
%stackaddr$prim50446 = alloca %struct.ScmObj*, align 8
%_95k43207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49231)
store volatile %struct.ScmObj* %_95k43207, %struct.ScmObj** %stackaddr$prim50446, align 8
%stackaddr$prim50447 = alloca %struct.ScmObj*, align 8
%current_45args49232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49231)
store volatile %struct.ScmObj* %current_45args49232, %struct.ScmObj** %stackaddr$prim50447, align 8
%stackaddr$prim50448 = alloca %struct.ScmObj*, align 8
%anf_45bind43076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49232)
store volatile %struct.ScmObj* %anf_45bind43076, %struct.ScmObj** %stackaddr$prim50448, align 8
%stackaddr$makeclosure50449 = alloca %struct.ScmObj*, align 8
%fptrToInt50450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44713 to i64
%ae44713 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50450)
store volatile %struct.ScmObj* %ae44713, %struct.ScmObj** %stackaddr$makeclosure50449, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %k43202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %lsts_4343008, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %f43003, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %acc43002, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %_37foldr42921, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44713, %struct.ScmObj* %_37foldl42999, i64 5)
%argslist49253$_37map1429470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50451 = alloca %struct.ScmObj*, align 8
%argslist49253$_37map1429471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts43001, %struct.ScmObj* %argslist49253$_37map1429470)
store volatile %struct.ScmObj* %argslist49253$_37map1429471, %struct.ScmObj** %stackaddr$prim50451, align 8
%stackaddr$prim50452 = alloca %struct.ScmObj*, align 8
%argslist49253$_37map1429472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43076, %struct.ScmObj* %argslist49253$_37map1429471)
store volatile %struct.ScmObj* %argslist49253$_37map1429472, %struct.ScmObj** %stackaddr$prim50452, align 8
%stackaddr$prim50453 = alloca %struct.ScmObj*, align 8
%argslist49253$_37map1429473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44713, %struct.ScmObj* %argslist49253$_37map1429472)
store volatile %struct.ScmObj* %argslist49253$_37map1429473, %struct.ScmObj** %stackaddr$prim50453, align 8
%clofunc50454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map142947)
musttail call tailcc void %clofunc50454(%struct.ScmObj* %_37map142947, %struct.ScmObj* %argslist49253$_37map1429473)
ret void
}

define tailcc void @proc_clo$ae44713(%struct.ScmObj* %env$ae44713,%struct.ScmObj* %current_45args49234) {
%stackaddr$env-ref50455 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 0)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50455
%stackaddr$env-ref50456 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 1)
store %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$env-ref50456
%stackaddr$env-ref50457 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 2)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50457
%stackaddr$env-ref50458 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 3)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50458
%stackaddr$env-ref50459 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 4)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50459
%stackaddr$env-ref50460 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44713, i64 5)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50460
%stackaddr$prim50461 = alloca %struct.ScmObj*, align 8
%_95k43208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49234)
store volatile %struct.ScmObj* %_95k43208, %struct.ScmObj** %stackaddr$prim50461, align 8
%stackaddr$prim50462 = alloca %struct.ScmObj*, align 8
%current_45args49235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49234)
store volatile %struct.ScmObj* %current_45args49235, %struct.ScmObj** %stackaddr$prim50462, align 8
%stackaddr$prim50463 = alloca %struct.ScmObj*, align 8
%vs43006 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49235)
store volatile %struct.ScmObj* %vs43006, %struct.ScmObj** %stackaddr$prim50463, align 8
%stackaddr$makeclosure50464 = alloca %struct.ScmObj*, align 8
%fptrToInt50465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44716 to i64
%ae44716 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50465)
store volatile %struct.ScmObj* %ae44716, %struct.ScmObj** %stackaddr$makeclosure50464, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %k43202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %lsts_4343008, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %vs43006, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %f43003, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %acc43002, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %_37foldr42921, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44716, %struct.ScmObj* %_37foldl42999, i64 6)
%ae44717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50466 = alloca %struct.ScmObj*, align 8
%fptrToInt50467 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44718 to i64
%ae44718 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50467)
store volatile %struct.ScmObj* %ae44718, %struct.ScmObj** %stackaddr$makeclosure50466, align 8
%argslist49252$ae447160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50468 = alloca %struct.ScmObj*, align 8
%argslist49252$ae447161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44718, %struct.ScmObj* %argslist49252$ae447160)
store volatile %struct.ScmObj* %argslist49252$ae447161, %struct.ScmObj** %stackaddr$prim50468, align 8
%stackaddr$prim50469 = alloca %struct.ScmObj*, align 8
%argslist49252$ae447162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44717, %struct.ScmObj* %argslist49252$ae447161)
store volatile %struct.ScmObj* %argslist49252$ae447162, %struct.ScmObj** %stackaddr$prim50469, align 8
%clofunc50470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44716)
musttail call tailcc void %clofunc50470(%struct.ScmObj* %ae44716, %struct.ScmObj* %argslist49252$ae447162)
ret void
}

define tailcc void @proc_clo$ae44716(%struct.ScmObj* %env$ae44716,%struct.ScmObj* %current_45args49237) {
%stackaddr$env-ref50471 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 0)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50471
%stackaddr$env-ref50472 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 1)
store %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$env-ref50472
%stackaddr$env-ref50473 = alloca %struct.ScmObj*, align 8
%vs43006 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 2)
store %struct.ScmObj* %vs43006, %struct.ScmObj** %stackaddr$env-ref50473
%stackaddr$env-ref50474 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 3)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50474
%stackaddr$env-ref50475 = alloca %struct.ScmObj*, align 8
%acc43002 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 4)
store %struct.ScmObj* %acc43002, %struct.ScmObj** %stackaddr$env-ref50475
%stackaddr$env-ref50476 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 5)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50476
%stackaddr$env-ref50477 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44716, i64 6)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50477
%stackaddr$prim50478 = alloca %struct.ScmObj*, align 8
%_95k43209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49237)
store volatile %struct.ScmObj* %_95k43209, %struct.ScmObj** %stackaddr$prim50478, align 8
%stackaddr$prim50479 = alloca %struct.ScmObj*, align 8
%current_45args49238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49237)
store volatile %struct.ScmObj* %current_45args49238, %struct.ScmObj** %stackaddr$prim50479, align 8
%stackaddr$prim50480 = alloca %struct.ScmObj*, align 8
%anf_45bind43077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49238)
store volatile %struct.ScmObj* %anf_45bind43077, %struct.ScmObj** %stackaddr$prim50480, align 8
%ae44739 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50481 = alloca %struct.ScmObj*, align 8
%anf_45bind43078 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc43002, %struct.ScmObj* %ae44739)
store volatile %struct.ScmObj* %anf_45bind43078, %struct.ScmObj** %stackaddr$prim50481, align 8
%stackaddr$makeclosure50482 = alloca %struct.ScmObj*, align 8
%fptrToInt50483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44741 to i64
%ae44741 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50483)
store volatile %struct.ScmObj* %ae44741, %struct.ScmObj** %stackaddr$makeclosure50482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %k43202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %lsts_4343008, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %f43003, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44741, %struct.ScmObj* %_37foldl42999, i64 3)
%argslist49246$_37foldr429210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50484 = alloca %struct.ScmObj*, align 8
%argslist49246$_37foldr429211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs43006, %struct.ScmObj* %argslist49246$_37foldr429210)
store volatile %struct.ScmObj* %argslist49246$_37foldr429211, %struct.ScmObj** %stackaddr$prim50484, align 8
%stackaddr$prim50485 = alloca %struct.ScmObj*, align 8
%argslist49246$_37foldr429212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43078, %struct.ScmObj* %argslist49246$_37foldr429211)
store volatile %struct.ScmObj* %argslist49246$_37foldr429212, %struct.ScmObj** %stackaddr$prim50485, align 8
%stackaddr$prim50486 = alloca %struct.ScmObj*, align 8
%argslist49246$_37foldr429213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43077, %struct.ScmObj* %argslist49246$_37foldr429212)
store volatile %struct.ScmObj* %argslist49246$_37foldr429213, %struct.ScmObj** %stackaddr$prim50486, align 8
%stackaddr$prim50487 = alloca %struct.ScmObj*, align 8
%argslist49246$_37foldr429214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44741, %struct.ScmObj* %argslist49246$_37foldr429213)
store volatile %struct.ScmObj* %argslist49246$_37foldr429214, %struct.ScmObj** %stackaddr$prim50487, align 8
%clofunc50488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr42921)
musttail call tailcc void %clofunc50488(%struct.ScmObj* %_37foldr42921, %struct.ScmObj* %argslist49246$_37foldr429214)
ret void
}

define tailcc void @proc_clo$ae44741(%struct.ScmObj* %env$ae44741,%struct.ScmObj* %current_45args49240) {
%stackaddr$env-ref50489 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 0)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50489
%stackaddr$env-ref50490 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 1)
store %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$env-ref50490
%stackaddr$env-ref50491 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 2)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50491
%stackaddr$env-ref50492 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44741, i64 3)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50492
%stackaddr$prim50493 = alloca %struct.ScmObj*, align 8
%_95k43210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49240)
store volatile %struct.ScmObj* %_95k43210, %struct.ScmObj** %stackaddr$prim50493, align 8
%stackaddr$prim50494 = alloca %struct.ScmObj*, align 8
%current_45args49241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49240)
store volatile %struct.ScmObj* %current_45args49241, %struct.ScmObj** %stackaddr$prim50494, align 8
%stackaddr$prim50495 = alloca %struct.ScmObj*, align 8
%anf_45bind43079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49241)
store volatile %struct.ScmObj* %anf_45bind43079, %struct.ScmObj** %stackaddr$prim50495, align 8
%stackaddr$makeclosure50496 = alloca %struct.ScmObj*, align 8
%fptrToInt50497 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44745 to i64
%ae44745 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50497)
store volatile %struct.ScmObj* %ae44745, %struct.ScmObj** %stackaddr$makeclosure50496, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44745, %struct.ScmObj* %k43202, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44745, %struct.ScmObj* %lsts_4343008, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44745, %struct.ScmObj* %f43003, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44745, %struct.ScmObj* %_37foldl42999, i64 3)
%stackaddr$prim50498 = alloca %struct.ScmObj*, align 8
%cpsargs43213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44745, %struct.ScmObj* %anf_45bind43079)
store volatile %struct.ScmObj* %cpsargs43213, %struct.ScmObj** %stackaddr$prim50498, align 8
%clofunc50499 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f43003)
musttail call tailcc void %clofunc50499(%struct.ScmObj* %f43003, %struct.ScmObj* %cpsargs43213)
ret void
}

define tailcc void @proc_clo$ae44745(%struct.ScmObj* %env$ae44745,%struct.ScmObj* %current_45args49243) {
%stackaddr$env-ref50500 = alloca %struct.ScmObj*, align 8
%k43202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44745, i64 0)
store %struct.ScmObj* %k43202, %struct.ScmObj** %stackaddr$env-ref50500
%stackaddr$env-ref50501 = alloca %struct.ScmObj*, align 8
%lsts_4343008 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44745, i64 1)
store %struct.ScmObj* %lsts_4343008, %struct.ScmObj** %stackaddr$env-ref50501
%stackaddr$env-ref50502 = alloca %struct.ScmObj*, align 8
%f43003 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44745, i64 2)
store %struct.ScmObj* %f43003, %struct.ScmObj** %stackaddr$env-ref50502
%stackaddr$env-ref50503 = alloca %struct.ScmObj*, align 8
%_37foldl42999 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44745, i64 3)
store %struct.ScmObj* %_37foldl42999, %struct.ScmObj** %stackaddr$env-ref50503
%stackaddr$prim50504 = alloca %struct.ScmObj*, align 8
%_95k43211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49243)
store volatile %struct.ScmObj* %_95k43211, %struct.ScmObj** %stackaddr$prim50504, align 8
%stackaddr$prim50505 = alloca %struct.ScmObj*, align 8
%current_45args49244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49243)
store volatile %struct.ScmObj* %current_45args49244, %struct.ScmObj** %stackaddr$prim50505, align 8
%stackaddr$prim50506 = alloca %struct.ScmObj*, align 8
%acc_4343010 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49244)
store volatile %struct.ScmObj* %acc_4343010, %struct.ScmObj** %stackaddr$prim50506, align 8
%stackaddr$prim50507 = alloca %struct.ScmObj*, align 8
%anf_45bind43080 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4343010, %struct.ScmObj* %lsts_4343008)
store volatile %struct.ScmObj* %anf_45bind43080, %struct.ScmObj** %stackaddr$prim50507, align 8
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%anf_45bind43081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f43003, %struct.ScmObj* %anf_45bind43080)
store volatile %struct.ScmObj* %anf_45bind43081, %struct.ScmObj** %stackaddr$prim50508, align 8
%stackaddr$prim50509 = alloca %struct.ScmObj*, align 8
%cpsargs43212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43202, %struct.ScmObj* %anf_45bind43081)
store volatile %struct.ScmObj* %cpsargs43212, %struct.ScmObj** %stackaddr$prim50509, align 8
%clofunc50510 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl42999)
musttail call tailcc void %clofunc50510(%struct.ScmObj* %_37foldl42999, %struct.ScmObj* %cpsargs43212)
ret void
}

define tailcc void @proc_clo$ae44718(%struct.ScmObj* %env$ae44718,%struct.ScmObj* %current_45args49247) {
%stackaddr$prim50511 = alloca %struct.ScmObj*, align 8
%k43214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49247)
store volatile %struct.ScmObj* %k43214, %struct.ScmObj** %stackaddr$prim50511, align 8
%stackaddr$prim50512 = alloca %struct.ScmObj*, align 8
%current_45args49248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49247)
store volatile %struct.ScmObj* %current_45args49248, %struct.ScmObj** %stackaddr$prim50512, align 8
%stackaddr$prim50513 = alloca %struct.ScmObj*, align 8
%a43012 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49248)
store volatile %struct.ScmObj* %a43012, %struct.ScmObj** %stackaddr$prim50513, align 8
%stackaddr$prim50514 = alloca %struct.ScmObj*, align 8
%current_45args49249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49248)
store volatile %struct.ScmObj* %current_45args49249, %struct.ScmObj** %stackaddr$prim50514, align 8
%stackaddr$prim50515 = alloca %struct.ScmObj*, align 8
%b43011 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49249)
store volatile %struct.ScmObj* %b43011, %struct.ScmObj** %stackaddr$prim50515, align 8
%stackaddr$prim50516 = alloca %struct.ScmObj*, align 8
%cpsprim43215 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a43012, %struct.ScmObj* %b43011)
store volatile %struct.ScmObj* %cpsprim43215, %struct.ScmObj** %stackaddr$prim50516, align 8
%ae44722 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49251$k432140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50517 = alloca %struct.ScmObj*, align 8
%argslist49251$k432141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43215, %struct.ScmObj* %argslist49251$k432140)
store volatile %struct.ScmObj* %argslist49251$k432141, %struct.ScmObj** %stackaddr$prim50517, align 8
%stackaddr$prim50518 = alloca %struct.ScmObj*, align 8
%argslist49251$k432142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44722, %struct.ScmObj* %argslist49251$k432141)
store volatile %struct.ScmObj* %argslist49251$k432142, %struct.ScmObj** %stackaddr$prim50518, align 8
%clofunc50519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43214)
musttail call tailcc void %clofunc50519(%struct.ScmObj* %k43214, %struct.ScmObj* %argslist49251$k432142)
ret void
}

define tailcc void @proc_clo$ae44694(%struct.ScmObj* %env$ae44694,%struct.ScmObj* %current_45args49254) {
%stackaddr$prim50520 = alloca %struct.ScmObj*, align 8
%k43216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49254)
store volatile %struct.ScmObj* %k43216, %struct.ScmObj** %stackaddr$prim50520, align 8
%stackaddr$prim50521 = alloca %struct.ScmObj*, align 8
%current_45args49255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49254)
store volatile %struct.ScmObj* %current_45args49255, %struct.ScmObj** %stackaddr$prim50521, align 8
%stackaddr$prim50522 = alloca %struct.ScmObj*, align 8
%x43007 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49255)
store volatile %struct.ScmObj* %x43007, %struct.ScmObj** %stackaddr$prim50522, align 8
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%cpsprim43217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x43007)
store volatile %struct.ScmObj* %cpsprim43217, %struct.ScmObj** %stackaddr$prim50523, align 8
%ae44697 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49257$k432160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50524 = alloca %struct.ScmObj*, align 8
%argslist49257$k432161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43217, %struct.ScmObj* %argslist49257$k432160)
store volatile %struct.ScmObj* %argslist49257$k432161, %struct.ScmObj** %stackaddr$prim50524, align 8
%stackaddr$prim50525 = alloca %struct.ScmObj*, align 8
%argslist49257$k432162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44697, %struct.ScmObj* %argslist49257$k432161)
store volatile %struct.ScmObj* %argslist49257$k432162, %struct.ScmObj** %stackaddr$prim50525, align 8
%clofunc50526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43216)
musttail call tailcc void %clofunc50526(%struct.ScmObj* %k43216, %struct.ScmObj* %argslist49257$k432162)
ret void
}

define tailcc void @proc_clo$ae44670(%struct.ScmObj* %env$ae44670,%struct.ScmObj* %current_45args49260) {
%stackaddr$prim50527 = alloca %struct.ScmObj*, align 8
%k43218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49260)
store volatile %struct.ScmObj* %k43218, %struct.ScmObj** %stackaddr$prim50527, align 8
%stackaddr$prim50528 = alloca %struct.ScmObj*, align 8
%current_45args49261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49260)
store volatile %struct.ScmObj* %current_45args49261, %struct.ScmObj** %stackaddr$prim50528, align 8
%stackaddr$prim50529 = alloca %struct.ScmObj*, align 8
%x43009 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49261)
store volatile %struct.ScmObj* %x43009, %struct.ScmObj** %stackaddr$prim50529, align 8
%stackaddr$prim50530 = alloca %struct.ScmObj*, align 8
%cpsprim43219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x43009)
store volatile %struct.ScmObj* %cpsprim43219, %struct.ScmObj** %stackaddr$prim50530, align 8
%ae44673 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49263$k432180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50531 = alloca %struct.ScmObj*, align 8
%argslist49263$k432181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43219, %struct.ScmObj* %argslist49263$k432180)
store volatile %struct.ScmObj* %argslist49263$k432181, %struct.ScmObj** %stackaddr$prim50531, align 8
%stackaddr$prim50532 = alloca %struct.ScmObj*, align 8
%argslist49263$k432182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44673, %struct.ScmObj* %argslist49263$k432181)
store volatile %struct.ScmObj* %argslist49263$k432182, %struct.ScmObj** %stackaddr$prim50532, align 8
%clofunc50533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43218)
musttail call tailcc void %clofunc50533(%struct.ScmObj* %k43218, %struct.ScmObj* %argslist49263$k432182)
ret void
}

define tailcc void @proc_clo$ae44622(%struct.ScmObj* %env$ae44622,%struct.ScmObj* %current_45args49266) {
%stackaddr$prim50534 = alloca %struct.ScmObj*, align 8
%k43220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49266)
store volatile %struct.ScmObj* %k43220, %struct.ScmObj** %stackaddr$prim50534, align 8
%stackaddr$prim50535 = alloca %struct.ScmObj*, align 8
%current_45args49267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49266)
store volatile %struct.ScmObj* %current_45args49267, %struct.ScmObj** %stackaddr$prim50535, align 8
%stackaddr$prim50536 = alloca %struct.ScmObj*, align 8
%lst43005 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49267)
store volatile %struct.ScmObj* %lst43005, %struct.ScmObj** %stackaddr$prim50536, align 8
%stackaddr$prim50537 = alloca %struct.ScmObj*, align 8
%current_45args49268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49267)
store volatile %struct.ScmObj* %current_45args49268, %struct.ScmObj** %stackaddr$prim50537, align 8
%stackaddr$prim50538 = alloca %struct.ScmObj*, align 8
%b43004 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49268)
store volatile %struct.ScmObj* %b43004, %struct.ScmObj** %stackaddr$prim50538, align 8
%truthy$cmp50539 = call i64 @is_truthy_value(%struct.ScmObj* %b43004)
%cmp$cmp50539 = icmp eq i64 %truthy$cmp50539, 1
br i1 %cmp$cmp50539, label %truebranch$cmp50539, label %falsebranch$cmp50539
truebranch$cmp50539:
%ae44625 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49270$k432200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50540 = alloca %struct.ScmObj*, align 8
%argslist49270$k432201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b43004, %struct.ScmObj* %argslist49270$k432200)
store volatile %struct.ScmObj* %argslist49270$k432201, %struct.ScmObj** %stackaddr$prim50540, align 8
%stackaddr$prim50541 = alloca %struct.ScmObj*, align 8
%argslist49270$k432202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44625, %struct.ScmObj* %argslist49270$k432201)
store volatile %struct.ScmObj* %argslist49270$k432202, %struct.ScmObj** %stackaddr$prim50541, align 8
%clofunc50542 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43220)
musttail call tailcc void %clofunc50542(%struct.ScmObj* %k43220, %struct.ScmObj* %argslist49270$k432202)
ret void
falsebranch$cmp50539:
%stackaddr$prim50543 = alloca %struct.ScmObj*, align 8
%cpsprim43221 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst43005)
store volatile %struct.ScmObj* %cpsprim43221, %struct.ScmObj** %stackaddr$prim50543, align 8
%ae44632 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49271$k432200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50544 = alloca %struct.ScmObj*, align 8
%argslist49271$k432201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43221, %struct.ScmObj* %argslist49271$k432200)
store volatile %struct.ScmObj* %argslist49271$k432201, %struct.ScmObj** %stackaddr$prim50544, align 8
%stackaddr$prim50545 = alloca %struct.ScmObj*, align 8
%argslist49271$k432202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44632, %struct.ScmObj* %argslist49271$k432201)
store volatile %struct.ScmObj* %argslist49271$k432202, %struct.ScmObj** %stackaddr$prim50545, align 8
%clofunc50546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43220)
musttail call tailcc void %clofunc50546(%struct.ScmObj* %k43220, %struct.ScmObj* %argslist49271$k432202)
ret void
}

define tailcc void @proc_clo$ae44463(%struct.ScmObj* %env$ae44463,%struct.ScmObj* %args4294343222) {
%stackaddr$env-ref50547 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44463, i64 0)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref50547
%stackaddr$env-ref50548 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44463, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50548
%stackaddr$env-ref50549 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44463, i64 2)
store %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$env-ref50549
%stackaddr$prim50550 = alloca %struct.ScmObj*, align 8
%k43223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4294343222)
store volatile %struct.ScmObj* %k43223, %struct.ScmObj** %stackaddr$prim50550, align 8
%stackaddr$prim50551 = alloca %struct.ScmObj*, align 8
%args42943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4294343222)
store volatile %struct.ScmObj* %args42943, %struct.ScmObj** %stackaddr$prim50551, align 8
%stackaddr$prim50552 = alloca %struct.ScmObj*, align 8
%f42945 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args42943)
store volatile %struct.ScmObj* %f42945, %struct.ScmObj** %stackaddr$prim50552, align 8
%stackaddr$prim50553 = alloca %struct.ScmObj*, align 8
%lsts42944 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args42943)
store volatile %struct.ScmObj* %lsts42944, %struct.ScmObj** %stackaddr$prim50553, align 8
%stackaddr$makeclosure50554 = alloca %struct.ScmObj*, align 8
%fptrToInt50555 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44468 to i64
%ae44468 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50555)
store volatile %struct.ScmObj* %ae44468, %struct.ScmObj** %stackaddr$makeclosure50554, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44468, %struct.ScmObj* %lsts42944, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44468, %struct.ScmObj* %_37foldr42921, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44468, %struct.ScmObj* %k43223, i64 2)
%ae44469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50556 = alloca %struct.ScmObj*, align 8
%fptrToInt50557 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44470 to i64
%ae44470 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50557)
store volatile %struct.ScmObj* %ae44470, %struct.ScmObj** %stackaddr$makeclosure50556, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44470, %struct.ScmObj* %f42945, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44470, %struct.ScmObj* %_37last42938, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44470, %struct.ScmObj* %_37drop_45right42935, i64 2)
%argslist49290$ae444680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50558 = alloca %struct.ScmObj*, align 8
%argslist49290$ae444681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44470, %struct.ScmObj* %argslist49290$ae444680)
store volatile %struct.ScmObj* %argslist49290$ae444681, %struct.ScmObj** %stackaddr$prim50558, align 8
%stackaddr$prim50559 = alloca %struct.ScmObj*, align 8
%argslist49290$ae444682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44469, %struct.ScmObj* %argslist49290$ae444681)
store volatile %struct.ScmObj* %argslist49290$ae444682, %struct.ScmObj** %stackaddr$prim50559, align 8
%clofunc50560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44468)
musttail call tailcc void %clofunc50560(%struct.ScmObj* %ae44468, %struct.ScmObj* %argslist49290$ae444682)
ret void
}

define tailcc void @proc_clo$ae44468(%struct.ScmObj* %env$ae44468,%struct.ScmObj* %current_45args49275) {
%stackaddr$env-ref50561 = alloca %struct.ScmObj*, align 8
%lsts42944 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44468, i64 0)
store %struct.ScmObj* %lsts42944, %struct.ScmObj** %stackaddr$env-ref50561
%stackaddr$env-ref50562 = alloca %struct.ScmObj*, align 8
%_37foldr42921 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44468, i64 1)
store %struct.ScmObj* %_37foldr42921, %struct.ScmObj** %stackaddr$env-ref50562
%stackaddr$env-ref50563 = alloca %struct.ScmObj*, align 8
%k43223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44468, i64 2)
store %struct.ScmObj* %k43223, %struct.ScmObj** %stackaddr$env-ref50563
%stackaddr$prim50564 = alloca %struct.ScmObj*, align 8
%_95k43224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49275)
store volatile %struct.ScmObj* %_95k43224, %struct.ScmObj** %stackaddr$prim50564, align 8
%stackaddr$prim50565 = alloca %struct.ScmObj*, align 8
%current_45args49276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49275)
store volatile %struct.ScmObj* %current_45args49276, %struct.ScmObj** %stackaddr$prim50565, align 8
%stackaddr$prim50566 = alloca %struct.ScmObj*, align 8
%anf_45bind43068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49276)
store volatile %struct.ScmObj* %anf_45bind43068, %struct.ScmObj** %stackaddr$prim50566, align 8
%ae44531 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50567 = alloca %struct.ScmObj*, align 8
%anf_45bind43069 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44531, %struct.ScmObj* %lsts42944)
store volatile %struct.ScmObj* %anf_45bind43069, %struct.ScmObj** %stackaddr$prim50567, align 8
%stackaddr$prim50568 = alloca %struct.ScmObj*, align 8
%anf_45bind43070 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43068, %struct.ScmObj* %anf_45bind43069)
store volatile %struct.ScmObj* %anf_45bind43070, %struct.ScmObj** %stackaddr$prim50568, align 8
%stackaddr$prim50569 = alloca %struct.ScmObj*, align 8
%cpsargs43225 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43223, %struct.ScmObj* %anf_45bind43070)
store volatile %struct.ScmObj* %cpsargs43225, %struct.ScmObj** %stackaddr$prim50569, align 8
%clofunc50570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr42921)
musttail call tailcc void %clofunc50570(%struct.ScmObj* %_37foldr42921, %struct.ScmObj* %cpsargs43225)
ret void
}

define tailcc void @proc_clo$ae44470(%struct.ScmObj* %env$ae44470,%struct.ScmObj* %fargs4294643226) {
%stackaddr$env-ref50571 = alloca %struct.ScmObj*, align 8
%f42945 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44470, i64 0)
store %struct.ScmObj* %f42945, %struct.ScmObj** %stackaddr$env-ref50571
%stackaddr$env-ref50572 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44470, i64 1)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref50572
%stackaddr$env-ref50573 = alloca %struct.ScmObj*, align 8
%_37drop_45right42935 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44470, i64 2)
store %struct.ScmObj* %_37drop_45right42935, %struct.ScmObj** %stackaddr$env-ref50573
%stackaddr$prim50574 = alloca %struct.ScmObj*, align 8
%k43227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4294643226)
store volatile %struct.ScmObj* %k43227, %struct.ScmObj** %stackaddr$prim50574, align 8
%stackaddr$prim50575 = alloca %struct.ScmObj*, align 8
%fargs42946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4294643226)
store volatile %struct.ScmObj* %fargs42946, %struct.ScmObj** %stackaddr$prim50575, align 8
%stackaddr$makeclosure50576 = alloca %struct.ScmObj*, align 8
%fptrToInt50577 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44474 to i64
%ae44474 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50577)
store volatile %struct.ScmObj* %ae44474, %struct.ScmObj** %stackaddr$makeclosure50576, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44474, %struct.ScmObj* %fargs42946, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44474, %struct.ScmObj* %f42945, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44474, %struct.ScmObj* %k43227, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44474, %struct.ScmObj* %_37last42938, i64 3)
%ae44476 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist49289$_37drop_45right429350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50578 = alloca %struct.ScmObj*, align 8
%argslist49289$_37drop_45right429351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44476, %struct.ScmObj* %argslist49289$_37drop_45right429350)
store volatile %struct.ScmObj* %argslist49289$_37drop_45right429351, %struct.ScmObj** %stackaddr$prim50578, align 8
%stackaddr$prim50579 = alloca %struct.ScmObj*, align 8
%argslist49289$_37drop_45right429352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs42946, %struct.ScmObj* %argslist49289$_37drop_45right429351)
store volatile %struct.ScmObj* %argslist49289$_37drop_45right429352, %struct.ScmObj** %stackaddr$prim50579, align 8
%stackaddr$prim50580 = alloca %struct.ScmObj*, align 8
%argslist49289$_37drop_45right429353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44474, %struct.ScmObj* %argslist49289$_37drop_45right429352)
store volatile %struct.ScmObj* %argslist49289$_37drop_45right429353, %struct.ScmObj** %stackaddr$prim50580, align 8
%clofunc50581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right42935)
musttail call tailcc void %clofunc50581(%struct.ScmObj* %_37drop_45right42935, %struct.ScmObj* %argslist49289$_37drop_45right429353)
ret void
}

define tailcc void @proc_clo$ae44474(%struct.ScmObj* %env$ae44474,%struct.ScmObj* %current_45args49278) {
%stackaddr$env-ref50582 = alloca %struct.ScmObj*, align 8
%fargs42946 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44474, i64 0)
store %struct.ScmObj* %fargs42946, %struct.ScmObj** %stackaddr$env-ref50582
%stackaddr$env-ref50583 = alloca %struct.ScmObj*, align 8
%f42945 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44474, i64 1)
store %struct.ScmObj* %f42945, %struct.ScmObj** %stackaddr$env-ref50583
%stackaddr$env-ref50584 = alloca %struct.ScmObj*, align 8
%k43227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44474, i64 2)
store %struct.ScmObj* %k43227, %struct.ScmObj** %stackaddr$env-ref50584
%stackaddr$env-ref50585 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44474, i64 3)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref50585
%stackaddr$prim50586 = alloca %struct.ScmObj*, align 8
%_95k43228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49278)
store volatile %struct.ScmObj* %_95k43228, %struct.ScmObj** %stackaddr$prim50586, align 8
%stackaddr$prim50587 = alloca %struct.ScmObj*, align 8
%current_45args49279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49278)
store volatile %struct.ScmObj* %current_45args49279, %struct.ScmObj** %stackaddr$prim50587, align 8
%stackaddr$prim50588 = alloca %struct.ScmObj*, align 8
%anf_45bind43065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49279)
store volatile %struct.ScmObj* %anf_45bind43065, %struct.ScmObj** %stackaddr$prim50588, align 8
%stackaddr$makeclosure50589 = alloca %struct.ScmObj*, align 8
%fptrToInt50590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44481 to i64
%ae44481 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50590)
store volatile %struct.ScmObj* %ae44481, %struct.ScmObj** %stackaddr$makeclosure50589, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44481, %struct.ScmObj* %fargs42946, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44481, %struct.ScmObj* %k43227, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44481, %struct.ScmObj* %_37last42938, i64 2)
%stackaddr$prim50591 = alloca %struct.ScmObj*, align 8
%cpsargs43232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44481, %struct.ScmObj* %anf_45bind43065)
store volatile %struct.ScmObj* %cpsargs43232, %struct.ScmObj** %stackaddr$prim50591, align 8
%clofunc50592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42945)
musttail call tailcc void %clofunc50592(%struct.ScmObj* %f42945, %struct.ScmObj* %cpsargs43232)
ret void
}

define tailcc void @proc_clo$ae44481(%struct.ScmObj* %env$ae44481,%struct.ScmObj* %current_45args49281) {
%stackaddr$env-ref50593 = alloca %struct.ScmObj*, align 8
%fargs42946 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44481, i64 0)
store %struct.ScmObj* %fargs42946, %struct.ScmObj** %stackaddr$env-ref50593
%stackaddr$env-ref50594 = alloca %struct.ScmObj*, align 8
%k43227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44481, i64 1)
store %struct.ScmObj* %k43227, %struct.ScmObj** %stackaddr$env-ref50594
%stackaddr$env-ref50595 = alloca %struct.ScmObj*, align 8
%_37last42938 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44481, i64 2)
store %struct.ScmObj* %_37last42938, %struct.ScmObj** %stackaddr$env-ref50595
%stackaddr$prim50596 = alloca %struct.ScmObj*, align 8
%_95k43229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49281)
store volatile %struct.ScmObj* %_95k43229, %struct.ScmObj** %stackaddr$prim50596, align 8
%stackaddr$prim50597 = alloca %struct.ScmObj*, align 8
%current_45args49282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49281)
store volatile %struct.ScmObj* %current_45args49282, %struct.ScmObj** %stackaddr$prim50597, align 8
%stackaddr$prim50598 = alloca %struct.ScmObj*, align 8
%anf_45bind43066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49282)
store volatile %struct.ScmObj* %anf_45bind43066, %struct.ScmObj** %stackaddr$prim50598, align 8
%stackaddr$makeclosure50599 = alloca %struct.ScmObj*, align 8
%fptrToInt50600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44486 to i64
%ae44486 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50600)
store volatile %struct.ScmObj* %ae44486, %struct.ScmObj** %stackaddr$makeclosure50599, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44486, %struct.ScmObj* %k43227, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44486, %struct.ScmObj* %anf_45bind43066, i64 1)
%argslist49288$_37last429380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50601 = alloca %struct.ScmObj*, align 8
%argslist49288$_37last429381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs42946, %struct.ScmObj* %argslist49288$_37last429380)
store volatile %struct.ScmObj* %argslist49288$_37last429381, %struct.ScmObj** %stackaddr$prim50601, align 8
%stackaddr$prim50602 = alloca %struct.ScmObj*, align 8
%argslist49288$_37last429382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44486, %struct.ScmObj* %argslist49288$_37last429381)
store volatile %struct.ScmObj* %argslist49288$_37last429382, %struct.ScmObj** %stackaddr$prim50602, align 8
%clofunc50603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last42938)
musttail call tailcc void %clofunc50603(%struct.ScmObj* %_37last42938, %struct.ScmObj* %argslist49288$_37last429382)
ret void
}

define tailcc void @proc_clo$ae44486(%struct.ScmObj* %env$ae44486,%struct.ScmObj* %current_45args49284) {
%stackaddr$env-ref50604 = alloca %struct.ScmObj*, align 8
%k43227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44486, i64 0)
store %struct.ScmObj* %k43227, %struct.ScmObj** %stackaddr$env-ref50604
%stackaddr$env-ref50605 = alloca %struct.ScmObj*, align 8
%anf_45bind43066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44486, i64 1)
store %struct.ScmObj* %anf_45bind43066, %struct.ScmObj** %stackaddr$env-ref50605
%stackaddr$prim50606 = alloca %struct.ScmObj*, align 8
%_95k43230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49284)
store volatile %struct.ScmObj* %_95k43230, %struct.ScmObj** %stackaddr$prim50606, align 8
%stackaddr$prim50607 = alloca %struct.ScmObj*, align 8
%current_45args49285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49284)
store volatile %struct.ScmObj* %current_45args49285, %struct.ScmObj** %stackaddr$prim50607, align 8
%stackaddr$prim50608 = alloca %struct.ScmObj*, align 8
%anf_45bind43067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49285)
store volatile %struct.ScmObj* %anf_45bind43067, %struct.ScmObj** %stackaddr$prim50608, align 8
%stackaddr$prim50609 = alloca %struct.ScmObj*, align 8
%cpsprim43231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43066, %struct.ScmObj* %anf_45bind43067)
store volatile %struct.ScmObj* %cpsprim43231, %struct.ScmObj** %stackaddr$prim50609, align 8
%ae44491 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49287$k432270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50610 = alloca %struct.ScmObj*, align 8
%argslist49287$k432271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43231, %struct.ScmObj* %argslist49287$k432270)
store volatile %struct.ScmObj* %argslist49287$k432271, %struct.ScmObj** %stackaddr$prim50610, align 8
%stackaddr$prim50611 = alloca %struct.ScmObj*, align 8
%argslist49287$k432272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44491, %struct.ScmObj* %argslist49287$k432271)
store volatile %struct.ScmObj* %argslist49287$k432272, %struct.ScmObj** %stackaddr$prim50611, align 8
%clofunc50612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43227)
musttail call tailcc void %clofunc50612(%struct.ScmObj* %k43227, %struct.ScmObj* %argslist49287$k432272)
ret void
}

define tailcc void @proc_clo$ae44386(%struct.ScmObj* %env$ae44386,%struct.ScmObj* %current_45args49292) {
%stackaddr$env-ref50613 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44386, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50613
%stackaddr$prim50614 = alloca %struct.ScmObj*, align 8
%k43233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49292)
store volatile %struct.ScmObj* %k43233, %struct.ScmObj** %stackaddr$prim50614, align 8
%stackaddr$prim50615 = alloca %struct.ScmObj*, align 8
%current_45args49293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49292)
store volatile %struct.ScmObj* %current_45args49293, %struct.ScmObj** %stackaddr$prim50615, align 8
%stackaddr$prim50616 = alloca %struct.ScmObj*, align 8
%f42949 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49293)
store volatile %struct.ScmObj* %f42949, %struct.ScmObj** %stackaddr$prim50616, align 8
%stackaddr$prim50617 = alloca %struct.ScmObj*, align 8
%current_45args49294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49293)
store volatile %struct.ScmObj* %current_45args49294, %struct.ScmObj** %stackaddr$prim50617, align 8
%stackaddr$prim50618 = alloca %struct.ScmObj*, align 8
%lst42948 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49294)
store volatile %struct.ScmObj* %lst42948, %struct.ScmObj** %stackaddr$prim50618, align 8
%stackaddr$makeclosure50619 = alloca %struct.ScmObj*, align 8
%fptrToInt50620 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44387 to i64
%ae44387 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50620)
store volatile %struct.ScmObj* %ae44387, %struct.ScmObj** %stackaddr$makeclosure50619, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44387, %struct.ScmObj* %lst42948, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44387, %struct.ScmObj* %_37foldr142916, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44387, %struct.ScmObj* %k43233, i64 2)
%ae44388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50621 = alloca %struct.ScmObj*, align 8
%fptrToInt50622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44389 to i64
%ae44389 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50622)
store volatile %struct.ScmObj* %ae44389, %struct.ScmObj** %stackaddr$makeclosure50621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44389, %struct.ScmObj* %f42949, i64 0)
%argslist49309$ae443870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50623 = alloca %struct.ScmObj*, align 8
%argslist49309$ae443871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44389, %struct.ScmObj* %argslist49309$ae443870)
store volatile %struct.ScmObj* %argslist49309$ae443871, %struct.ScmObj** %stackaddr$prim50623, align 8
%stackaddr$prim50624 = alloca %struct.ScmObj*, align 8
%argslist49309$ae443872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44388, %struct.ScmObj* %argslist49309$ae443871)
store volatile %struct.ScmObj* %argslist49309$ae443872, %struct.ScmObj** %stackaddr$prim50624, align 8
%clofunc50625 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44387)
musttail call tailcc void %clofunc50625(%struct.ScmObj* %ae44387, %struct.ScmObj* %argslist49309$ae443872)
ret void
}

define tailcc void @proc_clo$ae44387(%struct.ScmObj* %env$ae44387,%struct.ScmObj* %current_45args49296) {
%stackaddr$env-ref50626 = alloca %struct.ScmObj*, align 8
%lst42948 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44387, i64 0)
store %struct.ScmObj* %lst42948, %struct.ScmObj** %stackaddr$env-ref50626
%stackaddr$env-ref50627 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44387, i64 1)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50627
%stackaddr$env-ref50628 = alloca %struct.ScmObj*, align 8
%k43233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44387, i64 2)
store %struct.ScmObj* %k43233, %struct.ScmObj** %stackaddr$env-ref50628
%stackaddr$prim50629 = alloca %struct.ScmObj*, align 8
%_95k43234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49296)
store volatile %struct.ScmObj* %_95k43234, %struct.ScmObj** %stackaddr$prim50629, align 8
%stackaddr$prim50630 = alloca %struct.ScmObj*, align 8
%current_45args49297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49296)
store volatile %struct.ScmObj* %current_45args49297, %struct.ScmObj** %stackaddr$prim50630, align 8
%stackaddr$prim50631 = alloca %struct.ScmObj*, align 8
%anf_45bind43064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49297)
store volatile %struct.ScmObj* %anf_45bind43064, %struct.ScmObj** %stackaddr$prim50631, align 8
%ae44421 = call %struct.ScmObj* @const_init_null()
%argslist49299$_37foldr1429160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50632 = alloca %struct.ScmObj*, align 8
%argslist49299$_37foldr1429161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst42948, %struct.ScmObj* %argslist49299$_37foldr1429160)
store volatile %struct.ScmObj* %argslist49299$_37foldr1429161, %struct.ScmObj** %stackaddr$prim50632, align 8
%stackaddr$prim50633 = alloca %struct.ScmObj*, align 8
%argslist49299$_37foldr1429162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44421, %struct.ScmObj* %argslist49299$_37foldr1429161)
store volatile %struct.ScmObj* %argslist49299$_37foldr1429162, %struct.ScmObj** %stackaddr$prim50633, align 8
%stackaddr$prim50634 = alloca %struct.ScmObj*, align 8
%argslist49299$_37foldr1429163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43064, %struct.ScmObj* %argslist49299$_37foldr1429162)
store volatile %struct.ScmObj* %argslist49299$_37foldr1429163, %struct.ScmObj** %stackaddr$prim50634, align 8
%stackaddr$prim50635 = alloca %struct.ScmObj*, align 8
%argslist49299$_37foldr1429164 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43233, %struct.ScmObj* %argslist49299$_37foldr1429163)
store volatile %struct.ScmObj* %argslist49299$_37foldr1429164, %struct.ScmObj** %stackaddr$prim50635, align 8
%clofunc50636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr142916)
musttail call tailcc void %clofunc50636(%struct.ScmObj* %_37foldr142916, %struct.ScmObj* %argslist49299$_37foldr1429164)
ret void
}

define tailcc void @proc_clo$ae44389(%struct.ScmObj* %env$ae44389,%struct.ScmObj* %current_45args49300) {
%stackaddr$env-ref50637 = alloca %struct.ScmObj*, align 8
%f42949 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44389, i64 0)
store %struct.ScmObj* %f42949, %struct.ScmObj** %stackaddr$env-ref50637
%stackaddr$prim50638 = alloca %struct.ScmObj*, align 8
%k43235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49300)
store volatile %struct.ScmObj* %k43235, %struct.ScmObj** %stackaddr$prim50638, align 8
%stackaddr$prim50639 = alloca %struct.ScmObj*, align 8
%current_45args49301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49300)
store volatile %struct.ScmObj* %current_45args49301, %struct.ScmObj** %stackaddr$prim50639, align 8
%stackaddr$prim50640 = alloca %struct.ScmObj*, align 8
%v42951 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49301)
store volatile %struct.ScmObj* %v42951, %struct.ScmObj** %stackaddr$prim50640, align 8
%stackaddr$prim50641 = alloca %struct.ScmObj*, align 8
%current_45args49302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49301)
store volatile %struct.ScmObj* %current_45args49302, %struct.ScmObj** %stackaddr$prim50641, align 8
%stackaddr$prim50642 = alloca %struct.ScmObj*, align 8
%r42950 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49302)
store volatile %struct.ScmObj* %r42950, %struct.ScmObj** %stackaddr$prim50642, align 8
%stackaddr$makeclosure50643 = alloca %struct.ScmObj*, align 8
%fptrToInt50644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44391 to i64
%ae44391 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50644)
store volatile %struct.ScmObj* %ae44391, %struct.ScmObj** %stackaddr$makeclosure50643, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44391, %struct.ScmObj* %k43235, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44391, %struct.ScmObj* %r42950, i64 1)
%argslist49308$f429490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50645 = alloca %struct.ScmObj*, align 8
%argslist49308$f429491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v42951, %struct.ScmObj* %argslist49308$f429490)
store volatile %struct.ScmObj* %argslist49308$f429491, %struct.ScmObj** %stackaddr$prim50645, align 8
%stackaddr$prim50646 = alloca %struct.ScmObj*, align 8
%argslist49308$f429492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44391, %struct.ScmObj* %argslist49308$f429491)
store volatile %struct.ScmObj* %argslist49308$f429492, %struct.ScmObj** %stackaddr$prim50646, align 8
%clofunc50647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42949)
musttail call tailcc void %clofunc50647(%struct.ScmObj* %f42949, %struct.ScmObj* %argslist49308$f429492)
ret void
}

define tailcc void @proc_clo$ae44391(%struct.ScmObj* %env$ae44391,%struct.ScmObj* %current_45args49304) {
%stackaddr$env-ref50648 = alloca %struct.ScmObj*, align 8
%k43235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44391, i64 0)
store %struct.ScmObj* %k43235, %struct.ScmObj** %stackaddr$env-ref50648
%stackaddr$env-ref50649 = alloca %struct.ScmObj*, align 8
%r42950 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44391, i64 1)
store %struct.ScmObj* %r42950, %struct.ScmObj** %stackaddr$env-ref50649
%stackaddr$prim50650 = alloca %struct.ScmObj*, align 8
%_95k43236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49304)
store volatile %struct.ScmObj* %_95k43236, %struct.ScmObj** %stackaddr$prim50650, align 8
%stackaddr$prim50651 = alloca %struct.ScmObj*, align 8
%current_45args49305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49304)
store volatile %struct.ScmObj* %current_45args49305, %struct.ScmObj** %stackaddr$prim50651, align 8
%stackaddr$prim50652 = alloca %struct.ScmObj*, align 8
%anf_45bind43063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49305)
store volatile %struct.ScmObj* %anf_45bind43063, %struct.ScmObj** %stackaddr$prim50652, align 8
%stackaddr$prim50653 = alloca %struct.ScmObj*, align 8
%cpsprim43237 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43063, %struct.ScmObj* %r42950)
store volatile %struct.ScmObj* %cpsprim43237, %struct.ScmObj** %stackaddr$prim50653, align 8
%ae44396 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49307$k432350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50654 = alloca %struct.ScmObj*, align 8
%argslist49307$k432351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43237, %struct.ScmObj* %argslist49307$k432350)
store volatile %struct.ScmObj* %argslist49307$k432351, %struct.ScmObj** %stackaddr$prim50654, align 8
%stackaddr$prim50655 = alloca %struct.ScmObj*, align 8
%argslist49307$k432352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44396, %struct.ScmObj* %argslist49307$k432351)
store volatile %struct.ScmObj* %argslist49307$k432352, %struct.ScmObj** %stackaddr$prim50655, align 8
%clofunc50656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43235)
musttail call tailcc void %clofunc50656(%struct.ScmObj* %k43235, %struct.ScmObj* %argslist49307$k432352)
ret void
}

define tailcc void @proc_clo$ae44000(%struct.ScmObj* %env$ae44000,%struct.ScmObj* %current_45args49312) {
%stackaddr$env-ref50657 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44000, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50657
%stackaddr$env-ref50658 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44000, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50658
%stackaddr$prim50659 = alloca %struct.ScmObj*, align 8
%k43238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49312)
store volatile %struct.ScmObj* %k43238, %struct.ScmObj** %stackaddr$prim50659, align 8
%stackaddr$prim50660 = alloca %struct.ScmObj*, align 8
%current_45args49313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49312)
store volatile %struct.ScmObj* %current_45args49313, %struct.ScmObj** %stackaddr$prim50660, align 8
%stackaddr$prim50661 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49313)
store volatile %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$prim50661, align 8
%ae44002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50662 = alloca %struct.ScmObj*, align 8
%fptrToInt50663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44003 to i64
%ae44003 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50663)
store volatile %struct.ScmObj* %ae44003, %struct.ScmObj** %stackaddr$makeclosure50662, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44003, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44003, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44003, %struct.ScmObj* %_37foldr42922, i64 2)
%argslist49370$k432380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50664 = alloca %struct.ScmObj*, align 8
%argslist49370$k432381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44003, %struct.ScmObj* %argslist49370$k432380)
store volatile %struct.ScmObj* %argslist49370$k432381, %struct.ScmObj** %stackaddr$prim50664, align 8
%stackaddr$prim50665 = alloca %struct.ScmObj*, align 8
%argslist49370$k432382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44002, %struct.ScmObj* %argslist49370$k432381)
store volatile %struct.ScmObj* %argslist49370$k432382, %struct.ScmObj** %stackaddr$prim50665, align 8
%clofunc50666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43238)
musttail call tailcc void %clofunc50666(%struct.ScmObj* %k43238, %struct.ScmObj* %argslist49370$k432382)
ret void
}

define tailcc void @proc_clo$ae44003(%struct.ScmObj* %env$ae44003,%struct.ScmObj* %args4292343239) {
%stackaddr$env-ref50667 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44003, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50667
%stackaddr$env-ref50668 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44003, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50668
%stackaddr$env-ref50669 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44003, i64 2)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50669
%stackaddr$prim50670 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4292343239)
store volatile %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$prim50670, align 8
%stackaddr$prim50671 = alloca %struct.ScmObj*, align 8
%args42923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4292343239)
store volatile %struct.ScmObj* %args42923, %struct.ScmObj** %stackaddr$prim50671, align 8
%stackaddr$prim50672 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args42923)
store volatile %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$prim50672, align 8
%stackaddr$prim50673 = alloca %struct.ScmObj*, align 8
%anf_45bind43050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args42923)
store volatile %struct.ScmObj* %anf_45bind43050, %struct.ScmObj** %stackaddr$prim50673, align 8
%stackaddr$prim50674 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind43050)
store volatile %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$prim50674, align 8
%stackaddr$prim50675 = alloca %struct.ScmObj*, align 8
%anf_45bind43051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args42923)
store volatile %struct.ScmObj* %anf_45bind43051, %struct.ScmObj** %stackaddr$prim50675, align 8
%stackaddr$prim50676 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind43051)
store volatile %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$prim50676, align 8
%stackaddr$makeclosure50677 = alloca %struct.ScmObj*, align 8
%fptrToInt50678 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44011 to i64
%ae44011 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50678)
store volatile %struct.ScmObj* %ae44011, %struct.ScmObj** %stackaddr$makeclosure50677, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %f42926, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %acc42925, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %lsts42924, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %_37foldr42922, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44011, %struct.ScmObj* %k43240, i64 6)
%ae44012 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50679 = alloca %struct.ScmObj*, align 8
%fptrToInt50680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44013 to i64
%ae44013 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50680)
store volatile %struct.ScmObj* %ae44013, %struct.ScmObj** %stackaddr$makeclosure50679, align 8
%argslist49369$ae440110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50681 = alloca %struct.ScmObj*, align 8
%argslist49369$ae440111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44013, %struct.ScmObj* %argslist49369$ae440110)
store volatile %struct.ScmObj* %argslist49369$ae440111, %struct.ScmObj** %stackaddr$prim50681, align 8
%stackaddr$prim50682 = alloca %struct.ScmObj*, align 8
%argslist49369$ae440112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44012, %struct.ScmObj* %argslist49369$ae440111)
store volatile %struct.ScmObj* %argslist49369$ae440112, %struct.ScmObj** %stackaddr$prim50682, align 8
%clofunc50683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44011)
musttail call tailcc void %clofunc50683(%struct.ScmObj* %ae44011, %struct.ScmObj* %argslist49369$ae440112)
ret void
}

define tailcc void @proc_clo$ae44011(%struct.ScmObj* %env$ae44011,%struct.ScmObj* %current_45args49315) {
%stackaddr$env-ref50684 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50684
%stackaddr$env-ref50685 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50685
%stackaddr$env-ref50686 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 2)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50686
%stackaddr$env-ref50687 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 3)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50687
%stackaddr$env-ref50688 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 4)
store %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$env-ref50688
%stackaddr$env-ref50689 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 5)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50689
%stackaddr$env-ref50690 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44011, i64 6)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50690
%stackaddr$prim50691 = alloca %struct.ScmObj*, align 8
%_95k43241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49315)
store volatile %struct.ScmObj* %_95k43241, %struct.ScmObj** %stackaddr$prim50691, align 8
%stackaddr$prim50692 = alloca %struct.ScmObj*, align 8
%current_45args49316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49315)
store volatile %struct.ScmObj* %current_45args49316, %struct.ScmObj** %stackaddr$prim50692, align 8
%stackaddr$prim50693 = alloca %struct.ScmObj*, align 8
%anf_45bind43052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49316)
store volatile %struct.ScmObj* %anf_45bind43052, %struct.ScmObj** %stackaddr$prim50693, align 8
%stackaddr$makeclosure50694 = alloca %struct.ScmObj*, align 8
%fptrToInt50695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44043 to i64
%ae44043 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50695)
store volatile %struct.ScmObj* %ae44043, %struct.ScmObj** %stackaddr$makeclosure50694, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %f42926, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %acc42925, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %lsts42924, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %_37foldr42922, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44043, %struct.ScmObj* %k43240, i64 6)
%ae44045 = call %struct.ScmObj* @const_init_false()
%argslist49362$_37foldr1429160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50696 = alloca %struct.ScmObj*, align 8
%argslist49362$_37foldr1429161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts42924, %struct.ScmObj* %argslist49362$_37foldr1429160)
store volatile %struct.ScmObj* %argslist49362$_37foldr1429161, %struct.ScmObj** %stackaddr$prim50696, align 8
%stackaddr$prim50697 = alloca %struct.ScmObj*, align 8
%argslist49362$_37foldr1429162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44045, %struct.ScmObj* %argslist49362$_37foldr1429161)
store volatile %struct.ScmObj* %argslist49362$_37foldr1429162, %struct.ScmObj** %stackaddr$prim50697, align 8
%stackaddr$prim50698 = alloca %struct.ScmObj*, align 8
%argslist49362$_37foldr1429163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43052, %struct.ScmObj* %argslist49362$_37foldr1429162)
store volatile %struct.ScmObj* %argslist49362$_37foldr1429163, %struct.ScmObj** %stackaddr$prim50698, align 8
%stackaddr$prim50699 = alloca %struct.ScmObj*, align 8
%argslist49362$_37foldr1429164 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44043, %struct.ScmObj* %argslist49362$_37foldr1429163)
store volatile %struct.ScmObj* %argslist49362$_37foldr1429164, %struct.ScmObj** %stackaddr$prim50699, align 8
%clofunc50700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr142916)
musttail call tailcc void %clofunc50700(%struct.ScmObj* %_37foldr142916, %struct.ScmObj* %argslist49362$_37foldr1429164)
ret void
}

define tailcc void @proc_clo$ae44043(%struct.ScmObj* %env$ae44043,%struct.ScmObj* %current_45args49318) {
%stackaddr$env-ref50701 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50701
%stackaddr$env-ref50702 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50702
%stackaddr$env-ref50703 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 2)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50703
%stackaddr$env-ref50704 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 3)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50704
%stackaddr$env-ref50705 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 4)
store %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$env-ref50705
%stackaddr$env-ref50706 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 5)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50706
%stackaddr$env-ref50707 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44043, i64 6)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50707
%stackaddr$prim50708 = alloca %struct.ScmObj*, align 8
%_95k43242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49318)
store volatile %struct.ScmObj* %_95k43242, %struct.ScmObj** %stackaddr$prim50708, align 8
%stackaddr$prim50709 = alloca %struct.ScmObj*, align 8
%current_45args49319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49318)
store volatile %struct.ScmObj* %current_45args49319, %struct.ScmObj** %stackaddr$prim50709, align 8
%stackaddr$prim50710 = alloca %struct.ScmObj*, align 8
%anf_45bind43053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49319)
store volatile %struct.ScmObj* %anf_45bind43053, %struct.ScmObj** %stackaddr$prim50710, align 8
%truthy$cmp50711 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43053)
%cmp$cmp50711 = icmp eq i64 %truthy$cmp50711, 1
br i1 %cmp$cmp50711, label %truebranch$cmp50711, label %falsebranch$cmp50711
truebranch$cmp50711:
%ae44054 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49321$k432400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50712 = alloca %struct.ScmObj*, align 8
%argslist49321$k432401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42925, %struct.ScmObj* %argslist49321$k432400)
store volatile %struct.ScmObj* %argslist49321$k432401, %struct.ScmObj** %stackaddr$prim50712, align 8
%stackaddr$prim50713 = alloca %struct.ScmObj*, align 8
%argslist49321$k432402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44054, %struct.ScmObj* %argslist49321$k432401)
store volatile %struct.ScmObj* %argslist49321$k432402, %struct.ScmObj** %stackaddr$prim50713, align 8
%clofunc50714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43240)
musttail call tailcc void %clofunc50714(%struct.ScmObj* %k43240, %struct.ScmObj* %argslist49321$k432402)
ret void
falsebranch$cmp50711:
%stackaddr$makeclosure50715 = alloca %struct.ScmObj*, align 8
%fptrToInt50716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44059 to i64
%ae44059 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50716)
store volatile %struct.ScmObj* %ae44059, %struct.ScmObj** %stackaddr$makeclosure50715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %f42926, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %acc42925, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %lsts42924, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %_37foldr42922, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44059, %struct.ScmObj* %k43240, i64 6)
%ae44060 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50717 = alloca %struct.ScmObj*, align 8
%fptrToInt50718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44061 to i64
%ae44061 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50718)
store volatile %struct.ScmObj* %ae44061, %struct.ScmObj** %stackaddr$makeclosure50717, align 8
%argslist49361$ae440590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50719 = alloca %struct.ScmObj*, align 8
%argslist49361$ae440591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44061, %struct.ScmObj* %argslist49361$ae440590)
store volatile %struct.ScmObj* %argslist49361$ae440591, %struct.ScmObj** %stackaddr$prim50719, align 8
%stackaddr$prim50720 = alloca %struct.ScmObj*, align 8
%argslist49361$ae440592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44060, %struct.ScmObj* %argslist49361$ae440591)
store volatile %struct.ScmObj* %argslist49361$ae440592, %struct.ScmObj** %stackaddr$prim50720, align 8
%clofunc50721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44059)
musttail call tailcc void %clofunc50721(%struct.ScmObj* %ae44059, %struct.ScmObj* %argslist49361$ae440592)
ret void
}

define tailcc void @proc_clo$ae44059(%struct.ScmObj* %env$ae44059,%struct.ScmObj* %current_45args49322) {
%stackaddr$env-ref50722 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50722
%stackaddr$env-ref50723 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50723
%stackaddr$env-ref50724 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 2)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50724
%stackaddr$env-ref50725 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 3)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50725
%stackaddr$env-ref50726 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 4)
store %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$env-ref50726
%stackaddr$env-ref50727 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 5)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50727
%stackaddr$env-ref50728 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44059, i64 6)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50728
%stackaddr$prim50729 = alloca %struct.ScmObj*, align 8
%_95k43243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49322)
store volatile %struct.ScmObj* %_95k43243, %struct.ScmObj** %stackaddr$prim50729, align 8
%stackaddr$prim50730 = alloca %struct.ScmObj*, align 8
%current_45args49323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49322)
store volatile %struct.ScmObj* %current_45args49323, %struct.ScmObj** %stackaddr$prim50730, align 8
%stackaddr$prim50731 = alloca %struct.ScmObj*, align 8
%anf_45bind43054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49323)
store volatile %struct.ScmObj* %anf_45bind43054, %struct.ScmObj** %stackaddr$prim50731, align 8
%stackaddr$makeclosure50732 = alloca %struct.ScmObj*, align 8
%fptrToInt50733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44080 to i64
%ae44080 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50733)
store volatile %struct.ScmObj* %ae44080, %struct.ScmObj** %stackaddr$makeclosure50732, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %_37map142912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %f42926, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %acc42925, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %lsts42924, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %_37foldr42922, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44080, %struct.ScmObj* %k43240, i64 6)
%argslist49356$_37map1429120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50734 = alloca %struct.ScmObj*, align 8
%argslist49356$_37map1429121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts42924, %struct.ScmObj* %argslist49356$_37map1429120)
store volatile %struct.ScmObj* %argslist49356$_37map1429121, %struct.ScmObj** %stackaddr$prim50734, align 8
%stackaddr$prim50735 = alloca %struct.ScmObj*, align 8
%argslist49356$_37map1429122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43054, %struct.ScmObj* %argslist49356$_37map1429121)
store volatile %struct.ScmObj* %argslist49356$_37map1429122, %struct.ScmObj** %stackaddr$prim50735, align 8
%stackaddr$prim50736 = alloca %struct.ScmObj*, align 8
%argslist49356$_37map1429123 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44080, %struct.ScmObj* %argslist49356$_37map1429122)
store volatile %struct.ScmObj* %argslist49356$_37map1429123, %struct.ScmObj** %stackaddr$prim50736, align 8
%clofunc50737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map142912)
musttail call tailcc void %clofunc50737(%struct.ScmObj* %_37map142912, %struct.ScmObj* %argslist49356$_37map1429123)
ret void
}

define tailcc void @proc_clo$ae44080(%struct.ScmObj* %env$ae44080,%struct.ScmObj* %current_45args49325) {
%stackaddr$env-ref50738 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50738
%stackaddr$env-ref50739 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 1)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50739
%stackaddr$env-ref50740 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 2)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50740
%stackaddr$env-ref50741 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 3)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50741
%stackaddr$env-ref50742 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 4)
store %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$env-ref50742
%stackaddr$env-ref50743 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 5)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50743
%stackaddr$env-ref50744 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44080, i64 6)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50744
%stackaddr$prim50745 = alloca %struct.ScmObj*, align 8
%_95k43244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49325)
store volatile %struct.ScmObj* %_95k43244, %struct.ScmObj** %stackaddr$prim50745, align 8
%stackaddr$prim50746 = alloca %struct.ScmObj*, align 8
%current_45args49326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49325)
store volatile %struct.ScmObj* %current_45args49326, %struct.ScmObj** %stackaddr$prim50746, align 8
%stackaddr$prim50747 = alloca %struct.ScmObj*, align 8
%lsts_4342931 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49326)
store volatile %struct.ScmObj* %lsts_4342931, %struct.ScmObj** %stackaddr$prim50747, align 8
%stackaddr$makeclosure50748 = alloca %struct.ScmObj*, align 8
%fptrToInt50749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44083 to i64
%ae44083 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50749)
store volatile %struct.ScmObj* %ae44083, %struct.ScmObj** %stackaddr$makeclosure50748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %lsts_4342931, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %_37map142912, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %f42926, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %acc42925, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %lsts42924, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %_37foldr42922, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %k43240, i64 7)
%ae44084 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50750 = alloca %struct.ScmObj*, align 8
%fptrToInt50751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44085 to i64
%ae44085 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50751)
store volatile %struct.ScmObj* %ae44085, %struct.ScmObj** %stackaddr$makeclosure50750, align 8
%argslist49355$ae440830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50752 = alloca %struct.ScmObj*, align 8
%argslist49355$ae440831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44085, %struct.ScmObj* %argslist49355$ae440830)
store volatile %struct.ScmObj* %argslist49355$ae440831, %struct.ScmObj** %stackaddr$prim50752, align 8
%stackaddr$prim50753 = alloca %struct.ScmObj*, align 8
%argslist49355$ae440832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44084, %struct.ScmObj* %argslist49355$ae440831)
store volatile %struct.ScmObj* %argslist49355$ae440832, %struct.ScmObj** %stackaddr$prim50753, align 8
%clofunc50754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44083)
musttail call tailcc void %clofunc50754(%struct.ScmObj* %ae44083, %struct.ScmObj* %argslist49355$ae440832)
ret void
}

define tailcc void @proc_clo$ae44083(%struct.ScmObj* %env$ae44083,%struct.ScmObj* %current_45args49328) {
%stackaddr$env-ref50755 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50755
%stackaddr$env-ref50756 = alloca %struct.ScmObj*, align 8
%lsts_4342931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 1)
store %struct.ScmObj* %lsts_4342931, %struct.ScmObj** %stackaddr$env-ref50756
%stackaddr$env-ref50757 = alloca %struct.ScmObj*, align 8
%_37map142912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 2)
store %struct.ScmObj* %_37map142912, %struct.ScmObj** %stackaddr$env-ref50757
%stackaddr$env-ref50758 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 3)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50758
%stackaddr$env-ref50759 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 4)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50759
%stackaddr$env-ref50760 = alloca %struct.ScmObj*, align 8
%lsts42924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 5)
store %struct.ScmObj* %lsts42924, %struct.ScmObj** %stackaddr$env-ref50760
%stackaddr$env-ref50761 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 6)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50761
%stackaddr$env-ref50762 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 7)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50762
%stackaddr$prim50763 = alloca %struct.ScmObj*, align 8
%_95k43245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49328)
store volatile %struct.ScmObj* %_95k43245, %struct.ScmObj** %stackaddr$prim50763, align 8
%stackaddr$prim50764 = alloca %struct.ScmObj*, align 8
%current_45args49329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49328)
store volatile %struct.ScmObj* %current_45args49329, %struct.ScmObj** %stackaddr$prim50764, align 8
%stackaddr$prim50765 = alloca %struct.ScmObj*, align 8
%anf_45bind43055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49329)
store volatile %struct.ScmObj* %anf_45bind43055, %struct.ScmObj** %stackaddr$prim50765, align 8
%stackaddr$makeclosure50766 = alloca %struct.ScmObj*, align 8
%fptrToInt50767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44104 to i64
%ae44104 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50767)
store volatile %struct.ScmObj* %ae44104, %struct.ScmObj** %stackaddr$makeclosure50766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %lsts_4342931, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %f42926, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %acc42925, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %_37foldr42922, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44104, %struct.ScmObj* %k43240, i64 5)
%argslist49350$_37map1429120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50768 = alloca %struct.ScmObj*, align 8
%argslist49350$_37map1429121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts42924, %struct.ScmObj* %argslist49350$_37map1429120)
store volatile %struct.ScmObj* %argslist49350$_37map1429121, %struct.ScmObj** %stackaddr$prim50768, align 8
%stackaddr$prim50769 = alloca %struct.ScmObj*, align 8
%argslist49350$_37map1429122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43055, %struct.ScmObj* %argslist49350$_37map1429121)
store volatile %struct.ScmObj* %argslist49350$_37map1429122, %struct.ScmObj** %stackaddr$prim50769, align 8
%stackaddr$prim50770 = alloca %struct.ScmObj*, align 8
%argslist49350$_37map1429123 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44104, %struct.ScmObj* %argslist49350$_37map1429122)
store volatile %struct.ScmObj* %argslist49350$_37map1429123, %struct.ScmObj** %stackaddr$prim50770, align 8
%clofunc50771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map142912)
musttail call tailcc void %clofunc50771(%struct.ScmObj* %_37map142912, %struct.ScmObj* %argslist49350$_37map1429123)
ret void
}

define tailcc void @proc_clo$ae44104(%struct.ScmObj* %env$ae44104,%struct.ScmObj* %current_45args49331) {
%stackaddr$env-ref50772 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50772
%stackaddr$env-ref50773 = alloca %struct.ScmObj*, align 8
%lsts_4342931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 1)
store %struct.ScmObj* %lsts_4342931, %struct.ScmObj** %stackaddr$env-ref50773
%stackaddr$env-ref50774 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 2)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50774
%stackaddr$env-ref50775 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 3)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50775
%stackaddr$env-ref50776 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 4)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50776
%stackaddr$env-ref50777 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44104, i64 5)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50777
%stackaddr$prim50778 = alloca %struct.ScmObj*, align 8
%_95k43246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49331)
store volatile %struct.ScmObj* %_95k43246, %struct.ScmObj** %stackaddr$prim50778, align 8
%stackaddr$prim50779 = alloca %struct.ScmObj*, align 8
%current_45args49332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49331)
store volatile %struct.ScmObj* %current_45args49332, %struct.ScmObj** %stackaddr$prim50779, align 8
%stackaddr$prim50780 = alloca %struct.ScmObj*, align 8
%vs42929 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49332)
store volatile %struct.ScmObj* %vs42929, %struct.ScmObj** %stackaddr$prim50780, align 8
%stackaddr$makeclosure50781 = alloca %struct.ScmObj*, align 8
%fptrToInt50782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44107 to i64
%ae44107 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50782)
store volatile %struct.ScmObj* %ae44107, %struct.ScmObj** %stackaddr$makeclosure50781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %lsts_4342931, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %vs42929, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %f42926, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %acc42925, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %_37foldr42922, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44107, %struct.ScmObj* %k43240, i64 6)
%ae44108 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50783 = alloca %struct.ScmObj*, align 8
%fptrToInt50784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44109 to i64
%ae44109 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50784)
store volatile %struct.ScmObj* %ae44109, %struct.ScmObj** %stackaddr$makeclosure50783, align 8
%argslist49349$ae441070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50785 = alloca %struct.ScmObj*, align 8
%argslist49349$ae441071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44109, %struct.ScmObj* %argslist49349$ae441070)
store volatile %struct.ScmObj* %argslist49349$ae441071, %struct.ScmObj** %stackaddr$prim50785, align 8
%stackaddr$prim50786 = alloca %struct.ScmObj*, align 8
%argslist49349$ae441072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44108, %struct.ScmObj* %argslist49349$ae441071)
store volatile %struct.ScmObj* %argslist49349$ae441072, %struct.ScmObj** %stackaddr$prim50786, align 8
%clofunc50787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44107)
musttail call tailcc void %clofunc50787(%struct.ScmObj* %ae44107, %struct.ScmObj* %argslist49349$ae441072)
ret void
}

define tailcc void @proc_clo$ae44107(%struct.ScmObj* %env$ae44107,%struct.ScmObj* %current_45args49334) {
%stackaddr$env-ref50788 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50788
%stackaddr$env-ref50789 = alloca %struct.ScmObj*, align 8
%lsts_4342931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 1)
store %struct.ScmObj* %lsts_4342931, %struct.ScmObj** %stackaddr$env-ref50789
%stackaddr$env-ref50790 = alloca %struct.ScmObj*, align 8
%vs42929 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 2)
store %struct.ScmObj* %vs42929, %struct.ScmObj** %stackaddr$env-ref50790
%stackaddr$env-ref50791 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 3)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50791
%stackaddr$env-ref50792 = alloca %struct.ScmObj*, align 8
%acc42925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 4)
store %struct.ScmObj* %acc42925, %struct.ScmObj** %stackaddr$env-ref50792
%stackaddr$env-ref50793 = alloca %struct.ScmObj*, align 8
%_37foldr42922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 5)
store %struct.ScmObj* %_37foldr42922, %struct.ScmObj** %stackaddr$env-ref50793
%stackaddr$env-ref50794 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44107, i64 6)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50794
%stackaddr$prim50795 = alloca %struct.ScmObj*, align 8
%_95k43247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49334)
store volatile %struct.ScmObj* %_95k43247, %struct.ScmObj** %stackaddr$prim50795, align 8
%stackaddr$prim50796 = alloca %struct.ScmObj*, align 8
%current_45args49335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49334)
store volatile %struct.ScmObj* %current_45args49335, %struct.ScmObj** %stackaddr$prim50796, align 8
%stackaddr$prim50797 = alloca %struct.ScmObj*, align 8
%anf_45bind43056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49335)
store volatile %struct.ScmObj* %anf_45bind43056, %struct.ScmObj** %stackaddr$prim50797, align 8
%stackaddr$prim50798 = alloca %struct.ScmObj*, align 8
%anf_45bind43057 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42925, %struct.ScmObj* %lsts_4342931)
store volatile %struct.ScmObj* %anf_45bind43057, %struct.ScmObj** %stackaddr$prim50798, align 8
%stackaddr$prim50799 = alloca %struct.ScmObj*, align 8
%anf_45bind43058 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f42926, %struct.ScmObj* %anf_45bind43057)
store volatile %struct.ScmObj* %anf_45bind43058, %struct.ScmObj** %stackaddr$prim50799, align 8
%stackaddr$makeclosure50800 = alloca %struct.ScmObj*, align 8
%fptrToInt50801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44133 to i64
%ae44133 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50801)
store volatile %struct.ScmObj* %ae44133, %struct.ScmObj** %stackaddr$makeclosure50800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %_37foldr142916, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %vs42929, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %anf_45bind43056, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %f42926, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44133, %struct.ScmObj* %k43240, i64 4)
%stackaddr$prim50802 = alloca %struct.ScmObj*, align 8
%cpsargs43251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44133, %struct.ScmObj* %anf_45bind43058)
store volatile %struct.ScmObj* %cpsargs43251, %struct.ScmObj** %stackaddr$prim50802, align 8
%clofunc50803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr42922)
musttail call tailcc void %clofunc50803(%struct.ScmObj* %_37foldr42922, %struct.ScmObj* %cpsargs43251)
ret void
}

define tailcc void @proc_clo$ae44133(%struct.ScmObj* %env$ae44133,%struct.ScmObj* %current_45args49337) {
%stackaddr$env-ref50804 = alloca %struct.ScmObj*, align 8
%_37foldr142916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 0)
store %struct.ScmObj* %_37foldr142916, %struct.ScmObj** %stackaddr$env-ref50804
%stackaddr$env-ref50805 = alloca %struct.ScmObj*, align 8
%vs42929 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 1)
store %struct.ScmObj* %vs42929, %struct.ScmObj** %stackaddr$env-ref50805
%stackaddr$env-ref50806 = alloca %struct.ScmObj*, align 8
%anf_45bind43056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 2)
store %struct.ScmObj* %anf_45bind43056, %struct.ScmObj** %stackaddr$env-ref50806
%stackaddr$env-ref50807 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 3)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50807
%stackaddr$env-ref50808 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44133, i64 4)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50808
%stackaddr$prim50809 = alloca %struct.ScmObj*, align 8
%_95k43248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49337)
store volatile %struct.ScmObj* %_95k43248, %struct.ScmObj** %stackaddr$prim50809, align 8
%stackaddr$prim50810 = alloca %struct.ScmObj*, align 8
%current_45args49338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49337)
store volatile %struct.ScmObj* %current_45args49338, %struct.ScmObj** %stackaddr$prim50810, align 8
%stackaddr$prim50811 = alloca %struct.ScmObj*, align 8
%anf_45bind43059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49338)
store volatile %struct.ScmObj* %anf_45bind43059, %struct.ScmObj** %stackaddr$prim50811, align 8
%ae44138 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50812 = alloca %struct.ScmObj*, align 8
%anf_45bind43060 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43059, %struct.ScmObj* %ae44138)
store volatile %struct.ScmObj* %anf_45bind43060, %struct.ScmObj** %stackaddr$prim50812, align 8
%stackaddr$makeclosure50813 = alloca %struct.ScmObj*, align 8
%fptrToInt50814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44140 to i64
%ae44140 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50814)
store volatile %struct.ScmObj* %ae44140, %struct.ScmObj** %stackaddr$makeclosure50813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44140, %struct.ScmObj* %f42926, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44140, %struct.ScmObj* %k43240, i64 1)
%argslist49343$_37foldr1429160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50815 = alloca %struct.ScmObj*, align 8
%argslist49343$_37foldr1429161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs42929, %struct.ScmObj* %argslist49343$_37foldr1429160)
store volatile %struct.ScmObj* %argslist49343$_37foldr1429161, %struct.ScmObj** %stackaddr$prim50815, align 8
%stackaddr$prim50816 = alloca %struct.ScmObj*, align 8
%argslist49343$_37foldr1429162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43060, %struct.ScmObj* %argslist49343$_37foldr1429161)
store volatile %struct.ScmObj* %argslist49343$_37foldr1429162, %struct.ScmObj** %stackaddr$prim50816, align 8
%stackaddr$prim50817 = alloca %struct.ScmObj*, align 8
%argslist49343$_37foldr1429163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43056, %struct.ScmObj* %argslist49343$_37foldr1429162)
store volatile %struct.ScmObj* %argslist49343$_37foldr1429163, %struct.ScmObj** %stackaddr$prim50817, align 8
%stackaddr$prim50818 = alloca %struct.ScmObj*, align 8
%argslist49343$_37foldr1429164 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44140, %struct.ScmObj* %argslist49343$_37foldr1429163)
store volatile %struct.ScmObj* %argslist49343$_37foldr1429164, %struct.ScmObj** %stackaddr$prim50818, align 8
%clofunc50819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr142916)
musttail call tailcc void %clofunc50819(%struct.ScmObj* %_37foldr142916, %struct.ScmObj* %argslist49343$_37foldr1429164)
ret void
}

define tailcc void @proc_clo$ae44140(%struct.ScmObj* %env$ae44140,%struct.ScmObj* %current_45args49340) {
%stackaddr$env-ref50820 = alloca %struct.ScmObj*, align 8
%f42926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44140, i64 0)
store %struct.ScmObj* %f42926, %struct.ScmObj** %stackaddr$env-ref50820
%stackaddr$env-ref50821 = alloca %struct.ScmObj*, align 8
%k43240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44140, i64 1)
store %struct.ScmObj* %k43240, %struct.ScmObj** %stackaddr$env-ref50821
%stackaddr$prim50822 = alloca %struct.ScmObj*, align 8
%_95k43249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49340)
store volatile %struct.ScmObj* %_95k43249, %struct.ScmObj** %stackaddr$prim50822, align 8
%stackaddr$prim50823 = alloca %struct.ScmObj*, align 8
%current_45args49341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49340)
store volatile %struct.ScmObj* %current_45args49341, %struct.ScmObj** %stackaddr$prim50823, align 8
%stackaddr$prim50824 = alloca %struct.ScmObj*, align 8
%anf_45bind43061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49341)
store volatile %struct.ScmObj* %anf_45bind43061, %struct.ScmObj** %stackaddr$prim50824, align 8
%stackaddr$prim50825 = alloca %struct.ScmObj*, align 8
%cpsargs43250 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43240, %struct.ScmObj* %anf_45bind43061)
store volatile %struct.ScmObj* %cpsargs43250, %struct.ScmObj** %stackaddr$prim50825, align 8
%clofunc50826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42926)
musttail call tailcc void %clofunc50826(%struct.ScmObj* %f42926, %struct.ScmObj* %cpsargs43250)
ret void
}

define tailcc void @proc_clo$ae44109(%struct.ScmObj* %env$ae44109,%struct.ScmObj* %current_45args49344) {
%stackaddr$prim50827 = alloca %struct.ScmObj*, align 8
%k43252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49344)
store volatile %struct.ScmObj* %k43252, %struct.ScmObj** %stackaddr$prim50827, align 8
%stackaddr$prim50828 = alloca %struct.ScmObj*, align 8
%current_45args49345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49344)
store volatile %struct.ScmObj* %current_45args49345, %struct.ScmObj** %stackaddr$prim50828, align 8
%stackaddr$prim50829 = alloca %struct.ScmObj*, align 8
%a42934 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49345)
store volatile %struct.ScmObj* %a42934, %struct.ScmObj** %stackaddr$prim50829, align 8
%stackaddr$prim50830 = alloca %struct.ScmObj*, align 8
%current_45args49346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49345)
store volatile %struct.ScmObj* %current_45args49346, %struct.ScmObj** %stackaddr$prim50830, align 8
%stackaddr$prim50831 = alloca %struct.ScmObj*, align 8
%b42933 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49346)
store volatile %struct.ScmObj* %b42933, %struct.ScmObj** %stackaddr$prim50831, align 8
%stackaddr$prim50832 = alloca %struct.ScmObj*, align 8
%cpsprim43253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a42934, %struct.ScmObj* %b42933)
store volatile %struct.ScmObj* %cpsprim43253, %struct.ScmObj** %stackaddr$prim50832, align 8
%ae44113 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49348$k432520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50833 = alloca %struct.ScmObj*, align 8
%argslist49348$k432521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43253, %struct.ScmObj* %argslist49348$k432520)
store volatile %struct.ScmObj* %argslist49348$k432521, %struct.ScmObj** %stackaddr$prim50833, align 8
%stackaddr$prim50834 = alloca %struct.ScmObj*, align 8
%argslist49348$k432522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44113, %struct.ScmObj* %argslist49348$k432521)
store volatile %struct.ScmObj* %argslist49348$k432522, %struct.ScmObj** %stackaddr$prim50834, align 8
%clofunc50835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43252)
musttail call tailcc void %clofunc50835(%struct.ScmObj* %k43252, %struct.ScmObj* %argslist49348$k432522)
ret void
}

define tailcc void @proc_clo$ae44085(%struct.ScmObj* %env$ae44085,%struct.ScmObj* %current_45args49351) {
%stackaddr$prim50836 = alloca %struct.ScmObj*, align 8
%k43254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49351)
store volatile %struct.ScmObj* %k43254, %struct.ScmObj** %stackaddr$prim50836, align 8
%stackaddr$prim50837 = alloca %struct.ScmObj*, align 8
%current_45args49352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49351)
store volatile %struct.ScmObj* %current_45args49352, %struct.ScmObj** %stackaddr$prim50837, align 8
%stackaddr$prim50838 = alloca %struct.ScmObj*, align 8
%x42930 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49352)
store volatile %struct.ScmObj* %x42930, %struct.ScmObj** %stackaddr$prim50838, align 8
%stackaddr$prim50839 = alloca %struct.ScmObj*, align 8
%cpsprim43255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x42930)
store volatile %struct.ScmObj* %cpsprim43255, %struct.ScmObj** %stackaddr$prim50839, align 8
%ae44088 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49354$k432540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50840 = alloca %struct.ScmObj*, align 8
%argslist49354$k432541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43255, %struct.ScmObj* %argslist49354$k432540)
store volatile %struct.ScmObj* %argslist49354$k432541, %struct.ScmObj** %stackaddr$prim50840, align 8
%stackaddr$prim50841 = alloca %struct.ScmObj*, align 8
%argslist49354$k432542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44088, %struct.ScmObj* %argslist49354$k432541)
store volatile %struct.ScmObj* %argslist49354$k432542, %struct.ScmObj** %stackaddr$prim50841, align 8
%clofunc50842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43254)
musttail call tailcc void %clofunc50842(%struct.ScmObj* %k43254, %struct.ScmObj* %argslist49354$k432542)
ret void
}

define tailcc void @proc_clo$ae44061(%struct.ScmObj* %env$ae44061,%struct.ScmObj* %current_45args49357) {
%stackaddr$prim50843 = alloca %struct.ScmObj*, align 8
%k43256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49357)
store volatile %struct.ScmObj* %k43256, %struct.ScmObj** %stackaddr$prim50843, align 8
%stackaddr$prim50844 = alloca %struct.ScmObj*, align 8
%current_45args49358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49357)
store volatile %struct.ScmObj* %current_45args49358, %struct.ScmObj** %stackaddr$prim50844, align 8
%stackaddr$prim50845 = alloca %struct.ScmObj*, align 8
%x42932 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49358)
store volatile %struct.ScmObj* %x42932, %struct.ScmObj** %stackaddr$prim50845, align 8
%stackaddr$prim50846 = alloca %struct.ScmObj*, align 8
%cpsprim43257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x42932)
store volatile %struct.ScmObj* %cpsprim43257, %struct.ScmObj** %stackaddr$prim50846, align 8
%ae44064 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49360$k432560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50847 = alloca %struct.ScmObj*, align 8
%argslist49360$k432561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43257, %struct.ScmObj* %argslist49360$k432560)
store volatile %struct.ScmObj* %argslist49360$k432561, %struct.ScmObj** %stackaddr$prim50847, align 8
%stackaddr$prim50848 = alloca %struct.ScmObj*, align 8
%argslist49360$k432562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44064, %struct.ScmObj* %argslist49360$k432561)
store volatile %struct.ScmObj* %argslist49360$k432562, %struct.ScmObj** %stackaddr$prim50848, align 8
%clofunc50849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43256)
musttail call tailcc void %clofunc50849(%struct.ScmObj* %k43256, %struct.ScmObj* %argslist49360$k432562)
ret void
}

define tailcc void @proc_clo$ae44013(%struct.ScmObj* %env$ae44013,%struct.ScmObj* %current_45args49363) {
%stackaddr$prim50850 = alloca %struct.ScmObj*, align 8
%k43258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49363)
store volatile %struct.ScmObj* %k43258, %struct.ScmObj** %stackaddr$prim50850, align 8
%stackaddr$prim50851 = alloca %struct.ScmObj*, align 8
%current_45args49364 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49363)
store volatile %struct.ScmObj* %current_45args49364, %struct.ScmObj** %stackaddr$prim50851, align 8
%stackaddr$prim50852 = alloca %struct.ScmObj*, align 8
%lst42928 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49364)
store volatile %struct.ScmObj* %lst42928, %struct.ScmObj** %stackaddr$prim50852, align 8
%stackaddr$prim50853 = alloca %struct.ScmObj*, align 8
%current_45args49365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49364)
store volatile %struct.ScmObj* %current_45args49365, %struct.ScmObj** %stackaddr$prim50853, align 8
%stackaddr$prim50854 = alloca %struct.ScmObj*, align 8
%b42927 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49365)
store volatile %struct.ScmObj* %b42927, %struct.ScmObj** %stackaddr$prim50854, align 8
%truthy$cmp50855 = call i64 @is_truthy_value(%struct.ScmObj* %b42927)
%cmp$cmp50855 = icmp eq i64 %truthy$cmp50855, 1
br i1 %cmp$cmp50855, label %truebranch$cmp50855, label %falsebranch$cmp50855
truebranch$cmp50855:
%ae44016 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49367$k432580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50856 = alloca %struct.ScmObj*, align 8
%argslist49367$k432581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b42927, %struct.ScmObj* %argslist49367$k432580)
store volatile %struct.ScmObj* %argslist49367$k432581, %struct.ScmObj** %stackaddr$prim50856, align 8
%stackaddr$prim50857 = alloca %struct.ScmObj*, align 8
%argslist49367$k432582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44016, %struct.ScmObj* %argslist49367$k432581)
store volatile %struct.ScmObj* %argslist49367$k432582, %struct.ScmObj** %stackaddr$prim50857, align 8
%clofunc50858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43258)
musttail call tailcc void %clofunc50858(%struct.ScmObj* %k43258, %struct.ScmObj* %argslist49367$k432582)
ret void
falsebranch$cmp50855:
%stackaddr$prim50859 = alloca %struct.ScmObj*, align 8
%cpsprim43259 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42928)
store volatile %struct.ScmObj* %cpsprim43259, %struct.ScmObj** %stackaddr$prim50859, align 8
%ae44023 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49368$k432580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50860 = alloca %struct.ScmObj*, align 8
%argslist49368$k432581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43259, %struct.ScmObj* %argslist49368$k432580)
store volatile %struct.ScmObj* %argslist49368$k432581, %struct.ScmObj** %stackaddr$prim50860, align 8
%stackaddr$prim50861 = alloca %struct.ScmObj*, align 8
%argslist49368$k432582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44023, %struct.ScmObj* %argslist49368$k432581)
store volatile %struct.ScmObj* %argslist49368$k432582, %struct.ScmObj** %stackaddr$prim50861, align 8
%clofunc50862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43258)
musttail call tailcc void %clofunc50862(%struct.ScmObj* %k43258, %struct.ScmObj* %argslist49368$k432582)
ret void
}

define tailcc void @proc_clo$ae43970(%struct.ScmObj* %env$ae43970,%struct.ScmObj* %current_45args49372) {
%stackaddr$env-ref50863 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43970, i64 0)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref50863
%stackaddr$env-ref50864 = alloca %struct.ScmObj*, align 8
%_37length42905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43970, i64 1)
store %struct.ScmObj* %_37length42905, %struct.ScmObj** %stackaddr$env-ref50864
%stackaddr$prim50865 = alloca %struct.ScmObj*, align 8
%k43260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49372)
store volatile %struct.ScmObj* %k43260, %struct.ScmObj** %stackaddr$prim50865, align 8
%stackaddr$prim50866 = alloca %struct.ScmObj*, align 8
%current_45args49373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49372)
store volatile %struct.ScmObj* %current_45args49373, %struct.ScmObj** %stackaddr$prim50866, align 8
%stackaddr$prim50867 = alloca %struct.ScmObj*, align 8
%lst42937 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49373)
store volatile %struct.ScmObj* %lst42937, %struct.ScmObj** %stackaddr$prim50867, align 8
%stackaddr$prim50868 = alloca %struct.ScmObj*, align 8
%current_45args49374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49373)
store volatile %struct.ScmObj* %current_45args49374, %struct.ScmObj** %stackaddr$prim50868, align 8
%stackaddr$prim50869 = alloca %struct.ScmObj*, align 8
%n42936 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49374)
store volatile %struct.ScmObj* %n42936, %struct.ScmObj** %stackaddr$prim50869, align 8
%stackaddr$makeclosure50870 = alloca %struct.ScmObj*, align 8
%fptrToInt50871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43972 to i64
%ae43972 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50871)
store volatile %struct.ScmObj* %ae43972, %struct.ScmObj** %stackaddr$makeclosure50870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43972, %struct.ScmObj* %k43260, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43972, %struct.ScmObj* %_37take42908, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43972, %struct.ScmObj* %lst42937, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43972, %struct.ScmObj* %n42936, i64 3)
%argslist49380$_37length429050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50872 = alloca %struct.ScmObj*, align 8
%argslist49380$_37length429051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst42937, %struct.ScmObj* %argslist49380$_37length429050)
store volatile %struct.ScmObj* %argslist49380$_37length429051, %struct.ScmObj** %stackaddr$prim50872, align 8
%stackaddr$prim50873 = alloca %struct.ScmObj*, align 8
%argslist49380$_37length429052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43972, %struct.ScmObj* %argslist49380$_37length429051)
store volatile %struct.ScmObj* %argslist49380$_37length429052, %struct.ScmObj** %stackaddr$prim50873, align 8
%clofunc50874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length42905)
musttail call tailcc void %clofunc50874(%struct.ScmObj* %_37length42905, %struct.ScmObj* %argslist49380$_37length429052)
ret void
}

define tailcc void @proc_clo$ae43972(%struct.ScmObj* %env$ae43972,%struct.ScmObj* %current_45args49376) {
%stackaddr$env-ref50875 = alloca %struct.ScmObj*, align 8
%k43260 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43972, i64 0)
store %struct.ScmObj* %k43260, %struct.ScmObj** %stackaddr$env-ref50875
%stackaddr$env-ref50876 = alloca %struct.ScmObj*, align 8
%_37take42908 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43972, i64 1)
store %struct.ScmObj* %_37take42908, %struct.ScmObj** %stackaddr$env-ref50876
%stackaddr$env-ref50877 = alloca %struct.ScmObj*, align 8
%lst42937 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43972, i64 2)
store %struct.ScmObj* %lst42937, %struct.ScmObj** %stackaddr$env-ref50877
%stackaddr$env-ref50878 = alloca %struct.ScmObj*, align 8
%n42936 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43972, i64 3)
store %struct.ScmObj* %n42936, %struct.ScmObj** %stackaddr$env-ref50878
%stackaddr$prim50879 = alloca %struct.ScmObj*, align 8
%_95k43261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49376)
store volatile %struct.ScmObj* %_95k43261, %struct.ScmObj** %stackaddr$prim50879, align 8
%stackaddr$prim50880 = alloca %struct.ScmObj*, align 8
%current_45args49377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49376)
store volatile %struct.ScmObj* %current_45args49377, %struct.ScmObj** %stackaddr$prim50880, align 8
%stackaddr$prim50881 = alloca %struct.ScmObj*, align 8
%anf_45bind43048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49377)
store volatile %struct.ScmObj* %anf_45bind43048, %struct.ScmObj** %stackaddr$prim50881, align 8
%stackaddr$prim50882 = alloca %struct.ScmObj*, align 8
%anf_45bind43049 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind43048, %struct.ScmObj* %n42936)
store volatile %struct.ScmObj* %anf_45bind43049, %struct.ScmObj** %stackaddr$prim50882, align 8
%argslist49379$_37take429080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50883 = alloca %struct.ScmObj*, align 8
%argslist49379$_37take429081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43049, %struct.ScmObj* %argslist49379$_37take429080)
store volatile %struct.ScmObj* %argslist49379$_37take429081, %struct.ScmObj** %stackaddr$prim50883, align 8
%stackaddr$prim50884 = alloca %struct.ScmObj*, align 8
%argslist49379$_37take429082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst42937, %struct.ScmObj* %argslist49379$_37take429081)
store volatile %struct.ScmObj* %argslist49379$_37take429082, %struct.ScmObj** %stackaddr$prim50884, align 8
%stackaddr$prim50885 = alloca %struct.ScmObj*, align 8
%argslist49379$_37take429083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43260, %struct.ScmObj* %argslist49379$_37take429082)
store volatile %struct.ScmObj* %argslist49379$_37take429083, %struct.ScmObj** %stackaddr$prim50885, align 8
%clofunc50886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take42908)
musttail call tailcc void %clofunc50886(%struct.ScmObj* %_37take42908, %struct.ScmObj* %argslist49379$_37take429083)
ret void
}

define tailcc void @proc_clo$ae43916(%struct.ScmObj* %env$ae43916,%struct.ScmObj* %current_45args49382) {
%stackaddr$env-ref50887 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43916, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref50887
%stackaddr$prim50888 = alloca %struct.ScmObj*, align 8
%k43262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49382)
store volatile %struct.ScmObj* %k43262, %struct.ScmObj** %stackaddr$prim50888, align 8
%stackaddr$prim50889 = alloca %struct.ScmObj*, align 8
%current_45args49383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49382)
store volatile %struct.ScmObj* %current_45args49383, %struct.ScmObj** %stackaddr$prim50889, align 8
%stackaddr$prim50890 = alloca %struct.ScmObj*, align 8
%lst42939 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49383)
store volatile %struct.ScmObj* %lst42939, %struct.ScmObj** %stackaddr$prim50890, align 8
%stackaddr$makeclosure50891 = alloca %struct.ScmObj*, align 8
%fptrToInt50892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43917 to i64
%ae43917 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50892)
store volatile %struct.ScmObj* %ae43917, %struct.ScmObj** %stackaddr$makeclosure50891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43917, %struct.ScmObj* %_37foldl142900, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43917, %struct.ScmObj* %k43262, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43917, %struct.ScmObj* %lst42939, i64 2)
%ae43918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50893 = alloca %struct.ScmObj*, align 8
%fptrToInt50894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43919 to i64
%ae43919 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50894)
store volatile %struct.ScmObj* %ae43919, %struct.ScmObj** %stackaddr$makeclosure50893, align 8
%argslist49394$ae439170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50895 = alloca %struct.ScmObj*, align 8
%argslist49394$ae439171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43919, %struct.ScmObj* %argslist49394$ae439170)
store volatile %struct.ScmObj* %argslist49394$ae439171, %struct.ScmObj** %stackaddr$prim50895, align 8
%stackaddr$prim50896 = alloca %struct.ScmObj*, align 8
%argslist49394$ae439172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43918, %struct.ScmObj* %argslist49394$ae439171)
store volatile %struct.ScmObj* %argslist49394$ae439172, %struct.ScmObj** %stackaddr$prim50896, align 8
%clofunc50897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43917)
musttail call tailcc void %clofunc50897(%struct.ScmObj* %ae43917, %struct.ScmObj* %argslist49394$ae439172)
ret void
}

define tailcc void @proc_clo$ae43917(%struct.ScmObj* %env$ae43917,%struct.ScmObj* %current_45args49385) {
%stackaddr$env-ref50898 = alloca %struct.ScmObj*, align 8
%_37foldl142900 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43917, i64 0)
store %struct.ScmObj* %_37foldl142900, %struct.ScmObj** %stackaddr$env-ref50898
%stackaddr$env-ref50899 = alloca %struct.ScmObj*, align 8
%k43262 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43917, i64 1)
store %struct.ScmObj* %k43262, %struct.ScmObj** %stackaddr$env-ref50899
%stackaddr$env-ref50900 = alloca %struct.ScmObj*, align 8
%lst42939 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43917, i64 2)
store %struct.ScmObj* %lst42939, %struct.ScmObj** %stackaddr$env-ref50900
%stackaddr$prim50901 = alloca %struct.ScmObj*, align 8
%_95k43263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49385)
store volatile %struct.ScmObj* %_95k43263, %struct.ScmObj** %stackaddr$prim50901, align 8
%stackaddr$prim50902 = alloca %struct.ScmObj*, align 8
%current_45args49386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49385)
store volatile %struct.ScmObj* %current_45args49386, %struct.ScmObj** %stackaddr$prim50902, align 8
%stackaddr$prim50903 = alloca %struct.ScmObj*, align 8
%anf_45bind43047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49386)
store volatile %struct.ScmObj* %anf_45bind43047, %struct.ScmObj** %stackaddr$prim50903, align 8
%ae43938 = call %struct.ScmObj* @const_init_null()
%argslist49388$_37foldl1429000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50904 = alloca %struct.ScmObj*, align 8
%argslist49388$_37foldl1429001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst42939, %struct.ScmObj* %argslist49388$_37foldl1429000)
store volatile %struct.ScmObj* %argslist49388$_37foldl1429001, %struct.ScmObj** %stackaddr$prim50904, align 8
%stackaddr$prim50905 = alloca %struct.ScmObj*, align 8
%argslist49388$_37foldl1429002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43938, %struct.ScmObj* %argslist49388$_37foldl1429001)
store volatile %struct.ScmObj* %argslist49388$_37foldl1429002, %struct.ScmObj** %stackaddr$prim50905, align 8
%stackaddr$prim50906 = alloca %struct.ScmObj*, align 8
%argslist49388$_37foldl1429003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43047, %struct.ScmObj* %argslist49388$_37foldl1429002)
store volatile %struct.ScmObj* %argslist49388$_37foldl1429003, %struct.ScmObj** %stackaddr$prim50906, align 8
%stackaddr$prim50907 = alloca %struct.ScmObj*, align 8
%argslist49388$_37foldl1429004 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43262, %struct.ScmObj* %argslist49388$_37foldl1429003)
store volatile %struct.ScmObj* %argslist49388$_37foldl1429004, %struct.ScmObj** %stackaddr$prim50907, align 8
%clofunc50908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl142900)
musttail call tailcc void %clofunc50908(%struct.ScmObj* %_37foldl142900, %struct.ScmObj* %argslist49388$_37foldl1429004)
ret void
}

define tailcc void @proc_clo$ae43919(%struct.ScmObj* %env$ae43919,%struct.ScmObj* %current_45args49389) {
%stackaddr$prim50909 = alloca %struct.ScmObj*, align 8
%k43264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49389)
store volatile %struct.ScmObj* %k43264, %struct.ScmObj** %stackaddr$prim50909, align 8
%stackaddr$prim50910 = alloca %struct.ScmObj*, align 8
%current_45args49390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49389)
store volatile %struct.ScmObj* %current_45args49390, %struct.ScmObj** %stackaddr$prim50910, align 8
%stackaddr$prim50911 = alloca %struct.ScmObj*, align 8
%x42941 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49390)
store volatile %struct.ScmObj* %x42941, %struct.ScmObj** %stackaddr$prim50911, align 8
%stackaddr$prim50912 = alloca %struct.ScmObj*, align 8
%current_45args49391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49390)
store volatile %struct.ScmObj* %current_45args49391, %struct.ScmObj** %stackaddr$prim50912, align 8
%stackaddr$prim50913 = alloca %struct.ScmObj*, align 8
%y42940 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49391)
store volatile %struct.ScmObj* %y42940, %struct.ScmObj** %stackaddr$prim50913, align 8
%ae43921 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49393$k432640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50914 = alloca %struct.ScmObj*, align 8
%argslist49393$k432641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x42941, %struct.ScmObj* %argslist49393$k432640)
store volatile %struct.ScmObj* %argslist49393$k432641, %struct.ScmObj** %stackaddr$prim50914, align 8
%stackaddr$prim50915 = alloca %struct.ScmObj*, align 8
%argslist49393$k432642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43921, %struct.ScmObj* %argslist49393$k432641)
store volatile %struct.ScmObj* %argslist49393$k432642, %struct.ScmObj** %stackaddr$prim50915, align 8
%clofunc50916 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43264)
musttail call tailcc void %clofunc50916(%struct.ScmObj* %k43264, %struct.ScmObj* %argslist49393$k432642)
ret void
}

define tailcc void @proc_clo$ae43837(%struct.ScmObj* %env$ae43837,%struct.ScmObj* %current_45args49397) {
%stackaddr$prim50917 = alloca %struct.ScmObj*, align 8
%k43265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49397)
store volatile %struct.ScmObj* %k43265, %struct.ScmObj** %stackaddr$prim50917, align 8
%stackaddr$prim50918 = alloca %struct.ScmObj*, align 8
%current_45args49398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49397)
store volatile %struct.ScmObj* %current_45args49398, %struct.ScmObj** %stackaddr$prim50918, align 8
%stackaddr$prim50919 = alloca %struct.ScmObj*, align 8
%_37foldl142901 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49398)
store volatile %struct.ScmObj* %_37foldl142901, %struct.ScmObj** %stackaddr$prim50919, align 8
%ae43839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50920 = alloca %struct.ScmObj*, align 8
%fptrToInt50921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43840 to i64
%ae43840 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50921)
store volatile %struct.ScmObj* %ae43840, %struct.ScmObj** %stackaddr$makeclosure50920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43840, %struct.ScmObj* %_37foldl142901, i64 0)
%argslist49411$k432650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50922 = alloca %struct.ScmObj*, align 8
%argslist49411$k432651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43840, %struct.ScmObj* %argslist49411$k432650)
store volatile %struct.ScmObj* %argslist49411$k432651, %struct.ScmObj** %stackaddr$prim50922, align 8
%stackaddr$prim50923 = alloca %struct.ScmObj*, align 8
%argslist49411$k432652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43839, %struct.ScmObj* %argslist49411$k432651)
store volatile %struct.ScmObj* %argslist49411$k432652, %struct.ScmObj** %stackaddr$prim50923, align 8
%clofunc50924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43265)
musttail call tailcc void %clofunc50924(%struct.ScmObj* %k43265, %struct.ScmObj* %argslist49411$k432652)
ret void
}

define tailcc void @proc_clo$ae43840(%struct.ScmObj* %env$ae43840,%struct.ScmObj* %current_45args49400) {
%stackaddr$env-ref50925 = alloca %struct.ScmObj*, align 8
%_37foldl142901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43840, i64 0)
store %struct.ScmObj* %_37foldl142901, %struct.ScmObj** %stackaddr$env-ref50925
%stackaddr$prim50926 = alloca %struct.ScmObj*, align 8
%k43266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49400)
store volatile %struct.ScmObj* %k43266, %struct.ScmObj** %stackaddr$prim50926, align 8
%stackaddr$prim50927 = alloca %struct.ScmObj*, align 8
%current_45args49401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49400)
store volatile %struct.ScmObj* %current_45args49401, %struct.ScmObj** %stackaddr$prim50927, align 8
%stackaddr$prim50928 = alloca %struct.ScmObj*, align 8
%f42904 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49401)
store volatile %struct.ScmObj* %f42904, %struct.ScmObj** %stackaddr$prim50928, align 8
%stackaddr$prim50929 = alloca %struct.ScmObj*, align 8
%current_45args49402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49401)
store volatile %struct.ScmObj* %current_45args49402, %struct.ScmObj** %stackaddr$prim50929, align 8
%stackaddr$prim50930 = alloca %struct.ScmObj*, align 8
%acc42903 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49402)
store volatile %struct.ScmObj* %acc42903, %struct.ScmObj** %stackaddr$prim50930, align 8
%stackaddr$prim50931 = alloca %struct.ScmObj*, align 8
%current_45args49403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49402)
store volatile %struct.ScmObj* %current_45args49403, %struct.ScmObj** %stackaddr$prim50931, align 8
%stackaddr$prim50932 = alloca %struct.ScmObj*, align 8
%lst42902 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49403)
store volatile %struct.ScmObj* %lst42902, %struct.ScmObj** %stackaddr$prim50932, align 8
%stackaddr$prim50933 = alloca %struct.ScmObj*, align 8
%anf_45bind43042 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42902)
store volatile %struct.ScmObj* %anf_45bind43042, %struct.ScmObj** %stackaddr$prim50933, align 8
%truthy$cmp50934 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43042)
%cmp$cmp50934 = icmp eq i64 %truthy$cmp50934, 1
br i1 %cmp$cmp50934, label %truebranch$cmp50934, label %falsebranch$cmp50934
truebranch$cmp50934:
%ae43844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49405$k432660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50935 = alloca %struct.ScmObj*, align 8
%argslist49405$k432661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42903, %struct.ScmObj* %argslist49405$k432660)
store volatile %struct.ScmObj* %argslist49405$k432661, %struct.ScmObj** %stackaddr$prim50935, align 8
%stackaddr$prim50936 = alloca %struct.ScmObj*, align 8
%argslist49405$k432662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43844, %struct.ScmObj* %argslist49405$k432661)
store volatile %struct.ScmObj* %argslist49405$k432662, %struct.ScmObj** %stackaddr$prim50936, align 8
%clofunc50937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43266)
musttail call tailcc void %clofunc50937(%struct.ScmObj* %k43266, %struct.ScmObj* %argslist49405$k432662)
ret void
falsebranch$cmp50934:
%stackaddr$prim50938 = alloca %struct.ScmObj*, align 8
%anf_45bind43043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst42902)
store volatile %struct.ScmObj* %anf_45bind43043, %struct.ScmObj** %stackaddr$prim50938, align 8
%stackaddr$makeclosure50939 = alloca %struct.ScmObj*, align 8
%fptrToInt50940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43851 to i64
%ae43851 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50940)
store volatile %struct.ScmObj* %ae43851, %struct.ScmObj** %stackaddr$makeclosure50939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %_37foldl142901, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %k43266, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %f42904, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43851, %struct.ScmObj* %lst42902, i64 3)
%argslist49410$f429040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50941 = alloca %struct.ScmObj*, align 8
%argslist49410$f429041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42903, %struct.ScmObj* %argslist49410$f429040)
store volatile %struct.ScmObj* %argslist49410$f429041, %struct.ScmObj** %stackaddr$prim50941, align 8
%stackaddr$prim50942 = alloca %struct.ScmObj*, align 8
%argslist49410$f429042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43043, %struct.ScmObj* %argslist49410$f429041)
store volatile %struct.ScmObj* %argslist49410$f429042, %struct.ScmObj** %stackaddr$prim50942, align 8
%stackaddr$prim50943 = alloca %struct.ScmObj*, align 8
%argslist49410$f429043 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43851, %struct.ScmObj* %argslist49410$f429042)
store volatile %struct.ScmObj* %argslist49410$f429043, %struct.ScmObj** %stackaddr$prim50943, align 8
%clofunc50944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42904)
musttail call tailcc void %clofunc50944(%struct.ScmObj* %f42904, %struct.ScmObj* %argslist49410$f429043)
ret void
}

define tailcc void @proc_clo$ae43851(%struct.ScmObj* %env$ae43851,%struct.ScmObj* %current_45args49406) {
%stackaddr$env-ref50945 = alloca %struct.ScmObj*, align 8
%_37foldl142901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 0)
store %struct.ScmObj* %_37foldl142901, %struct.ScmObj** %stackaddr$env-ref50945
%stackaddr$env-ref50946 = alloca %struct.ScmObj*, align 8
%k43266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 1)
store %struct.ScmObj* %k43266, %struct.ScmObj** %stackaddr$env-ref50946
%stackaddr$env-ref50947 = alloca %struct.ScmObj*, align 8
%f42904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 2)
store %struct.ScmObj* %f42904, %struct.ScmObj** %stackaddr$env-ref50947
%stackaddr$env-ref50948 = alloca %struct.ScmObj*, align 8
%lst42902 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43851, i64 3)
store %struct.ScmObj* %lst42902, %struct.ScmObj** %stackaddr$env-ref50948
%stackaddr$prim50949 = alloca %struct.ScmObj*, align 8
%_95k43267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49406)
store volatile %struct.ScmObj* %_95k43267, %struct.ScmObj** %stackaddr$prim50949, align 8
%stackaddr$prim50950 = alloca %struct.ScmObj*, align 8
%current_45args49407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49406)
store volatile %struct.ScmObj* %current_45args49407, %struct.ScmObj** %stackaddr$prim50950, align 8
%stackaddr$prim50951 = alloca %struct.ScmObj*, align 8
%anf_45bind43044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49407)
store volatile %struct.ScmObj* %anf_45bind43044, %struct.ScmObj** %stackaddr$prim50951, align 8
%stackaddr$prim50952 = alloca %struct.ScmObj*, align 8
%anf_45bind43045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst42902)
store volatile %struct.ScmObj* %anf_45bind43045, %struct.ScmObj** %stackaddr$prim50952, align 8
%argslist49409$_37foldl1429010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50953 = alloca %struct.ScmObj*, align 8
%argslist49409$_37foldl1429011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43045, %struct.ScmObj* %argslist49409$_37foldl1429010)
store volatile %struct.ScmObj* %argslist49409$_37foldl1429011, %struct.ScmObj** %stackaddr$prim50953, align 8
%stackaddr$prim50954 = alloca %struct.ScmObj*, align 8
%argslist49409$_37foldl1429012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43044, %struct.ScmObj* %argslist49409$_37foldl1429011)
store volatile %struct.ScmObj* %argslist49409$_37foldl1429012, %struct.ScmObj** %stackaddr$prim50954, align 8
%stackaddr$prim50955 = alloca %struct.ScmObj*, align 8
%argslist49409$_37foldl1429013 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f42904, %struct.ScmObj* %argslist49409$_37foldl1429012)
store volatile %struct.ScmObj* %argslist49409$_37foldl1429013, %struct.ScmObj** %stackaddr$prim50955, align 8
%stackaddr$prim50956 = alloca %struct.ScmObj*, align 8
%argslist49409$_37foldl1429014 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43266, %struct.ScmObj* %argslist49409$_37foldl1429013)
store volatile %struct.ScmObj* %argslist49409$_37foldl1429014, %struct.ScmObj** %stackaddr$prim50956, align 8
%clofunc50957 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl142901)
musttail call tailcc void %clofunc50957(%struct.ScmObj* %_37foldl142901, %struct.ScmObj* %argslist49409$_37foldl1429014)
ret void
}

define tailcc void @proc_clo$ae43754(%struct.ScmObj* %env$ae43754,%struct.ScmObj* %current_45args49414) {
%stackaddr$prim50958 = alloca %struct.ScmObj*, align 8
%k43268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49414)
store volatile %struct.ScmObj* %k43268, %struct.ScmObj** %stackaddr$prim50958, align 8
%stackaddr$prim50959 = alloca %struct.ScmObj*, align 8
%current_45args49415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49414)
store volatile %struct.ScmObj* %current_45args49415, %struct.ScmObj** %stackaddr$prim50959, align 8
%stackaddr$prim50960 = alloca %struct.ScmObj*, align 8
%_37length42906 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49415)
store volatile %struct.ScmObj* %_37length42906, %struct.ScmObj** %stackaddr$prim50960, align 8
%ae43756 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50961 = alloca %struct.ScmObj*, align 8
%fptrToInt50962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43757 to i64
%ae43757 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50962)
store volatile %struct.ScmObj* %ae43757, %struct.ScmObj** %stackaddr$makeclosure50961, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43757, %struct.ScmObj* %_37length42906, i64 0)
%argslist49426$k432680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50963 = alloca %struct.ScmObj*, align 8
%argslist49426$k432681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43757, %struct.ScmObj* %argslist49426$k432680)
store volatile %struct.ScmObj* %argslist49426$k432681, %struct.ScmObj** %stackaddr$prim50963, align 8
%stackaddr$prim50964 = alloca %struct.ScmObj*, align 8
%argslist49426$k432682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43756, %struct.ScmObj* %argslist49426$k432681)
store volatile %struct.ScmObj* %argslist49426$k432682, %struct.ScmObj** %stackaddr$prim50964, align 8
%clofunc50965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43268)
musttail call tailcc void %clofunc50965(%struct.ScmObj* %k43268, %struct.ScmObj* %argslist49426$k432682)
ret void
}

define tailcc void @proc_clo$ae43757(%struct.ScmObj* %env$ae43757,%struct.ScmObj* %current_45args49417) {
%stackaddr$env-ref50966 = alloca %struct.ScmObj*, align 8
%_37length42906 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43757, i64 0)
store %struct.ScmObj* %_37length42906, %struct.ScmObj** %stackaddr$env-ref50966
%stackaddr$prim50967 = alloca %struct.ScmObj*, align 8
%k43269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49417)
store volatile %struct.ScmObj* %k43269, %struct.ScmObj** %stackaddr$prim50967, align 8
%stackaddr$prim50968 = alloca %struct.ScmObj*, align 8
%current_45args49418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49417)
store volatile %struct.ScmObj* %current_45args49418, %struct.ScmObj** %stackaddr$prim50968, align 8
%stackaddr$prim50969 = alloca %struct.ScmObj*, align 8
%lst42907 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49418)
store volatile %struct.ScmObj* %lst42907, %struct.ScmObj** %stackaddr$prim50969, align 8
%stackaddr$prim50970 = alloca %struct.ScmObj*, align 8
%anf_45bind43038 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42907)
store volatile %struct.ScmObj* %anf_45bind43038, %struct.ScmObj** %stackaddr$prim50970, align 8
%truthy$cmp50971 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43038)
%cmp$cmp50971 = icmp eq i64 %truthy$cmp50971, 1
br i1 %cmp$cmp50971, label %truebranch$cmp50971, label %falsebranch$cmp50971
truebranch$cmp50971:
%ae43761 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43762 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49420$k432690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50972 = alloca %struct.ScmObj*, align 8
%argslist49420$k432691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43762, %struct.ScmObj* %argslist49420$k432690)
store volatile %struct.ScmObj* %argslist49420$k432691, %struct.ScmObj** %stackaddr$prim50972, align 8
%stackaddr$prim50973 = alloca %struct.ScmObj*, align 8
%argslist49420$k432692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43761, %struct.ScmObj* %argslist49420$k432691)
store volatile %struct.ScmObj* %argslist49420$k432692, %struct.ScmObj** %stackaddr$prim50973, align 8
%clofunc50974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43269)
musttail call tailcc void %clofunc50974(%struct.ScmObj* %k43269, %struct.ScmObj* %argslist49420$k432692)
ret void
falsebranch$cmp50971:
%stackaddr$prim50975 = alloca %struct.ScmObj*, align 8
%anf_45bind43039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst42907)
store volatile %struct.ScmObj* %anf_45bind43039, %struct.ScmObj** %stackaddr$prim50975, align 8
%stackaddr$makeclosure50976 = alloca %struct.ScmObj*, align 8
%fptrToInt50977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43771 to i64
%ae43771 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50977)
store volatile %struct.ScmObj* %ae43771, %struct.ScmObj** %stackaddr$makeclosure50976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43771, %struct.ScmObj* %k43269, i64 0)
%argslist49425$_37length429060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50978 = alloca %struct.ScmObj*, align 8
%argslist49425$_37length429061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43039, %struct.ScmObj* %argslist49425$_37length429060)
store volatile %struct.ScmObj* %argslist49425$_37length429061, %struct.ScmObj** %stackaddr$prim50978, align 8
%stackaddr$prim50979 = alloca %struct.ScmObj*, align 8
%argslist49425$_37length429062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43771, %struct.ScmObj* %argslist49425$_37length429061)
store volatile %struct.ScmObj* %argslist49425$_37length429062, %struct.ScmObj** %stackaddr$prim50979, align 8
%clofunc50980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length42906)
musttail call tailcc void %clofunc50980(%struct.ScmObj* %_37length42906, %struct.ScmObj* %argslist49425$_37length429062)
ret void
}

define tailcc void @proc_clo$ae43771(%struct.ScmObj* %env$ae43771,%struct.ScmObj* %current_45args49421) {
%stackaddr$env-ref50981 = alloca %struct.ScmObj*, align 8
%k43269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43771, i64 0)
store %struct.ScmObj* %k43269, %struct.ScmObj** %stackaddr$env-ref50981
%stackaddr$prim50982 = alloca %struct.ScmObj*, align 8
%_95k43270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49421)
store volatile %struct.ScmObj* %_95k43270, %struct.ScmObj** %stackaddr$prim50982, align 8
%stackaddr$prim50983 = alloca %struct.ScmObj*, align 8
%current_45args49422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49421)
store volatile %struct.ScmObj* %current_45args49422, %struct.ScmObj** %stackaddr$prim50983, align 8
%stackaddr$prim50984 = alloca %struct.ScmObj*, align 8
%anf_45bind43040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49422)
store volatile %struct.ScmObj* %anf_45bind43040, %struct.ScmObj** %stackaddr$prim50984, align 8
%ae43773 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50985 = alloca %struct.ScmObj*, align 8
%cpsprim43271 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae43773, %struct.ScmObj* %anf_45bind43040)
store volatile %struct.ScmObj* %cpsprim43271, %struct.ScmObj** %stackaddr$prim50985, align 8
%ae43776 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49424$k432690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50986 = alloca %struct.ScmObj*, align 8
%argslist49424$k432691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43271, %struct.ScmObj* %argslist49424$k432690)
store volatile %struct.ScmObj* %argslist49424$k432691, %struct.ScmObj** %stackaddr$prim50986, align 8
%stackaddr$prim50987 = alloca %struct.ScmObj*, align 8
%argslist49424$k432692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43776, %struct.ScmObj* %argslist49424$k432691)
store volatile %struct.ScmObj* %argslist49424$k432692, %struct.ScmObj** %stackaddr$prim50987, align 8
%clofunc50988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43269)
musttail call tailcc void %clofunc50988(%struct.ScmObj* %k43269, %struct.ScmObj* %argslist49424$k432692)
ret void
}

define tailcc void @proc_clo$ae43604(%struct.ScmObj* %env$ae43604,%struct.ScmObj* %current_45args49429) {
%stackaddr$prim50989 = alloca %struct.ScmObj*, align 8
%k43272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49429)
store volatile %struct.ScmObj* %k43272, %struct.ScmObj** %stackaddr$prim50989, align 8
%stackaddr$prim50990 = alloca %struct.ScmObj*, align 8
%current_45args49430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49429)
store volatile %struct.ScmObj* %current_45args49430, %struct.ScmObj** %stackaddr$prim50990, align 8
%stackaddr$prim50991 = alloca %struct.ScmObj*, align 8
%_37take42909 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49430)
store volatile %struct.ScmObj* %_37take42909, %struct.ScmObj** %stackaddr$prim50991, align 8
%ae43606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50992 = alloca %struct.ScmObj*, align 8
%fptrToInt50993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43607 to i64
%ae43607 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50993)
store volatile %struct.ScmObj* %ae43607, %struct.ScmObj** %stackaddr$makeclosure50992, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43607, %struct.ScmObj* %_37take42909, i64 0)
%argslist49443$k432720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50994 = alloca %struct.ScmObj*, align 8
%argslist49443$k432721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43607, %struct.ScmObj* %argslist49443$k432720)
store volatile %struct.ScmObj* %argslist49443$k432721, %struct.ScmObj** %stackaddr$prim50994, align 8
%stackaddr$prim50995 = alloca %struct.ScmObj*, align 8
%argslist49443$k432722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43606, %struct.ScmObj* %argslist49443$k432721)
store volatile %struct.ScmObj* %argslist49443$k432722, %struct.ScmObj** %stackaddr$prim50995, align 8
%clofunc50996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43272)
musttail call tailcc void %clofunc50996(%struct.ScmObj* %k43272, %struct.ScmObj* %argslist49443$k432722)
ret void
}

define tailcc void @proc_clo$ae43607(%struct.ScmObj* %env$ae43607,%struct.ScmObj* %current_45args49432) {
%stackaddr$env-ref50997 = alloca %struct.ScmObj*, align 8
%_37take42909 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43607, i64 0)
store %struct.ScmObj* %_37take42909, %struct.ScmObj** %stackaddr$env-ref50997
%stackaddr$prim50998 = alloca %struct.ScmObj*, align 8
%k43273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49432)
store volatile %struct.ScmObj* %k43273, %struct.ScmObj** %stackaddr$prim50998, align 8
%stackaddr$prim50999 = alloca %struct.ScmObj*, align 8
%current_45args49433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49432)
store volatile %struct.ScmObj* %current_45args49433, %struct.ScmObj** %stackaddr$prim50999, align 8
%stackaddr$prim51000 = alloca %struct.ScmObj*, align 8
%lst42911 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49433)
store volatile %struct.ScmObj* %lst42911, %struct.ScmObj** %stackaddr$prim51000, align 8
%stackaddr$prim51001 = alloca %struct.ScmObj*, align 8
%current_45args49434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49433)
store volatile %struct.ScmObj* %current_45args49434, %struct.ScmObj** %stackaddr$prim51001, align 8
%stackaddr$prim51002 = alloca %struct.ScmObj*, align 8
%n42910 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49434)
store volatile %struct.ScmObj* %n42910, %struct.ScmObj** %stackaddr$prim51002, align 8
%ae43609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51003 = alloca %struct.ScmObj*, align 8
%anf_45bind43031 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n42910, %struct.ScmObj* %ae43609)
store volatile %struct.ScmObj* %anf_45bind43031, %struct.ScmObj** %stackaddr$prim51003, align 8
%truthy$cmp51004 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43031)
%cmp$cmp51004 = icmp eq i64 %truthy$cmp51004, 1
br i1 %cmp$cmp51004, label %truebranch$cmp51004, label %falsebranch$cmp51004
truebranch$cmp51004:
%ae43612 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43613 = call %struct.ScmObj* @const_init_null()
%argslist49436$k432730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51005 = alloca %struct.ScmObj*, align 8
%argslist49436$k432731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43613, %struct.ScmObj* %argslist49436$k432730)
store volatile %struct.ScmObj* %argslist49436$k432731, %struct.ScmObj** %stackaddr$prim51005, align 8
%stackaddr$prim51006 = alloca %struct.ScmObj*, align 8
%argslist49436$k432732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43612, %struct.ScmObj* %argslist49436$k432731)
store volatile %struct.ScmObj* %argslist49436$k432732, %struct.ScmObj** %stackaddr$prim51006, align 8
%clofunc51007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43273)
musttail call tailcc void %clofunc51007(%struct.ScmObj* %k43273, %struct.ScmObj* %argslist49436$k432732)
ret void
falsebranch$cmp51004:
%stackaddr$prim51008 = alloca %struct.ScmObj*, align 8
%anf_45bind43032 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42911)
store volatile %struct.ScmObj* %anf_45bind43032, %struct.ScmObj** %stackaddr$prim51008, align 8
%truthy$cmp51009 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43032)
%cmp$cmp51009 = icmp eq i64 %truthy$cmp51009, 1
br i1 %cmp$cmp51009, label %truebranch$cmp51009, label %falsebranch$cmp51009
truebranch$cmp51009:
%ae43623 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43624 = call %struct.ScmObj* @const_init_null()
%argslist49437$k432730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51010 = alloca %struct.ScmObj*, align 8
%argslist49437$k432731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43624, %struct.ScmObj* %argslist49437$k432730)
store volatile %struct.ScmObj* %argslist49437$k432731, %struct.ScmObj** %stackaddr$prim51010, align 8
%stackaddr$prim51011 = alloca %struct.ScmObj*, align 8
%argslist49437$k432732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43623, %struct.ScmObj* %argslist49437$k432731)
store volatile %struct.ScmObj* %argslist49437$k432732, %struct.ScmObj** %stackaddr$prim51011, align 8
%clofunc51012 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43273)
musttail call tailcc void %clofunc51012(%struct.ScmObj* %k43273, %struct.ScmObj* %argslist49437$k432732)
ret void
falsebranch$cmp51009:
%stackaddr$prim51013 = alloca %struct.ScmObj*, align 8
%anf_45bind43033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst42911)
store volatile %struct.ScmObj* %anf_45bind43033, %struct.ScmObj** %stackaddr$prim51013, align 8
%stackaddr$prim51014 = alloca %struct.ScmObj*, align 8
%anf_45bind43034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst42911)
store volatile %struct.ScmObj* %anf_45bind43034, %struct.ScmObj** %stackaddr$prim51014, align 8
%ae43634 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51015 = alloca %struct.ScmObj*, align 8
%anf_45bind43035 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n42910, %struct.ScmObj* %ae43634)
store volatile %struct.ScmObj* %anf_45bind43035, %struct.ScmObj** %stackaddr$prim51015, align 8
%stackaddr$makeclosure51016 = alloca %struct.ScmObj*, align 8
%fptrToInt51017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43636 to i64
%ae43636 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51017)
store volatile %struct.ScmObj* %ae43636, %struct.ScmObj** %stackaddr$makeclosure51016, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43636, %struct.ScmObj* %anf_45bind43033, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43636, %struct.ScmObj* %k43273, i64 1)
%argslist49442$_37take429090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51018 = alloca %struct.ScmObj*, align 8
%argslist49442$_37take429091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43035, %struct.ScmObj* %argslist49442$_37take429090)
store volatile %struct.ScmObj* %argslist49442$_37take429091, %struct.ScmObj** %stackaddr$prim51018, align 8
%stackaddr$prim51019 = alloca %struct.ScmObj*, align 8
%argslist49442$_37take429092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43034, %struct.ScmObj* %argslist49442$_37take429091)
store volatile %struct.ScmObj* %argslist49442$_37take429092, %struct.ScmObj** %stackaddr$prim51019, align 8
%stackaddr$prim51020 = alloca %struct.ScmObj*, align 8
%argslist49442$_37take429093 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43636, %struct.ScmObj* %argslist49442$_37take429092)
store volatile %struct.ScmObj* %argslist49442$_37take429093, %struct.ScmObj** %stackaddr$prim51020, align 8
%clofunc51021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take42909)
musttail call tailcc void %clofunc51021(%struct.ScmObj* %_37take42909, %struct.ScmObj* %argslist49442$_37take429093)
ret void
}

define tailcc void @proc_clo$ae43636(%struct.ScmObj* %env$ae43636,%struct.ScmObj* %current_45args49438) {
%stackaddr$env-ref51022 = alloca %struct.ScmObj*, align 8
%anf_45bind43033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43636, i64 0)
store %struct.ScmObj* %anf_45bind43033, %struct.ScmObj** %stackaddr$env-ref51022
%stackaddr$env-ref51023 = alloca %struct.ScmObj*, align 8
%k43273 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43636, i64 1)
store %struct.ScmObj* %k43273, %struct.ScmObj** %stackaddr$env-ref51023
%stackaddr$prim51024 = alloca %struct.ScmObj*, align 8
%_95k43274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49438)
store volatile %struct.ScmObj* %_95k43274, %struct.ScmObj** %stackaddr$prim51024, align 8
%stackaddr$prim51025 = alloca %struct.ScmObj*, align 8
%current_45args49439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49438)
store volatile %struct.ScmObj* %current_45args49439, %struct.ScmObj** %stackaddr$prim51025, align 8
%stackaddr$prim51026 = alloca %struct.ScmObj*, align 8
%anf_45bind43036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49439)
store volatile %struct.ScmObj* %anf_45bind43036, %struct.ScmObj** %stackaddr$prim51026, align 8
%stackaddr$prim51027 = alloca %struct.ScmObj*, align 8
%cpsprim43275 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43033, %struct.ScmObj* %anf_45bind43036)
store volatile %struct.ScmObj* %cpsprim43275, %struct.ScmObj** %stackaddr$prim51027, align 8
%ae43642 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49441$k432730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51028 = alloca %struct.ScmObj*, align 8
%argslist49441$k432731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43275, %struct.ScmObj* %argslist49441$k432730)
store volatile %struct.ScmObj* %argslist49441$k432731, %struct.ScmObj** %stackaddr$prim51028, align 8
%stackaddr$prim51029 = alloca %struct.ScmObj*, align 8
%argslist49441$k432732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43642, %struct.ScmObj* %argslist49441$k432731)
store volatile %struct.ScmObj* %argslist49441$k432732, %struct.ScmObj** %stackaddr$prim51029, align 8
%clofunc51030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43273)
musttail call tailcc void %clofunc51030(%struct.ScmObj* %k43273, %struct.ScmObj* %argslist49441$k432732)
ret void
}

define tailcc void @proc_clo$ae43507(%struct.ScmObj* %env$ae43507,%struct.ScmObj* %current_45args49446) {
%stackaddr$prim51031 = alloca %struct.ScmObj*, align 8
%k43276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49446)
store volatile %struct.ScmObj* %k43276, %struct.ScmObj** %stackaddr$prim51031, align 8
%stackaddr$prim51032 = alloca %struct.ScmObj*, align 8
%current_45args49447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49446)
store volatile %struct.ScmObj* %current_45args49447, %struct.ScmObj** %stackaddr$prim51032, align 8
%stackaddr$prim51033 = alloca %struct.ScmObj*, align 8
%_37map42913 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49447)
store volatile %struct.ScmObj* %_37map42913, %struct.ScmObj** %stackaddr$prim51033, align 8
%ae43509 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51034 = alloca %struct.ScmObj*, align 8
%fptrToInt51035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43510 to i64
%ae43510 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51035)
store volatile %struct.ScmObj* %ae43510, %struct.ScmObj** %stackaddr$makeclosure51034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43510, %struct.ScmObj* %_37map42913, i64 0)
%argslist49463$k432760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51036 = alloca %struct.ScmObj*, align 8
%argslist49463$k432761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43510, %struct.ScmObj* %argslist49463$k432760)
store volatile %struct.ScmObj* %argslist49463$k432761, %struct.ScmObj** %stackaddr$prim51036, align 8
%stackaddr$prim51037 = alloca %struct.ScmObj*, align 8
%argslist49463$k432762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43509, %struct.ScmObj* %argslist49463$k432761)
store volatile %struct.ScmObj* %argslist49463$k432762, %struct.ScmObj** %stackaddr$prim51037, align 8
%clofunc51038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43276)
musttail call tailcc void %clofunc51038(%struct.ScmObj* %k43276, %struct.ScmObj* %argslist49463$k432762)
ret void
}

define tailcc void @proc_clo$ae43510(%struct.ScmObj* %env$ae43510,%struct.ScmObj* %current_45args49449) {
%stackaddr$env-ref51039 = alloca %struct.ScmObj*, align 8
%_37map42913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43510, i64 0)
store %struct.ScmObj* %_37map42913, %struct.ScmObj** %stackaddr$env-ref51039
%stackaddr$prim51040 = alloca %struct.ScmObj*, align 8
%k43277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49449)
store volatile %struct.ScmObj* %k43277, %struct.ScmObj** %stackaddr$prim51040, align 8
%stackaddr$prim51041 = alloca %struct.ScmObj*, align 8
%current_45args49450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49449)
store volatile %struct.ScmObj* %current_45args49450, %struct.ScmObj** %stackaddr$prim51041, align 8
%stackaddr$prim51042 = alloca %struct.ScmObj*, align 8
%f42915 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49450)
store volatile %struct.ScmObj* %f42915, %struct.ScmObj** %stackaddr$prim51042, align 8
%stackaddr$prim51043 = alloca %struct.ScmObj*, align 8
%current_45args49451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49450)
store volatile %struct.ScmObj* %current_45args49451, %struct.ScmObj** %stackaddr$prim51043, align 8
%stackaddr$prim51044 = alloca %struct.ScmObj*, align 8
%lst42914 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49451)
store volatile %struct.ScmObj* %lst42914, %struct.ScmObj** %stackaddr$prim51044, align 8
%stackaddr$prim51045 = alloca %struct.ScmObj*, align 8
%anf_45bind43025 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42914)
store volatile %struct.ScmObj* %anf_45bind43025, %struct.ScmObj** %stackaddr$prim51045, align 8
%truthy$cmp51046 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43025)
%cmp$cmp51046 = icmp eq i64 %truthy$cmp51046, 1
br i1 %cmp$cmp51046, label %truebranch$cmp51046, label %falsebranch$cmp51046
truebranch$cmp51046:
%ae43514 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43515 = call %struct.ScmObj* @const_init_null()
%argslist49453$k432770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51047 = alloca %struct.ScmObj*, align 8
%argslist49453$k432771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43515, %struct.ScmObj* %argslist49453$k432770)
store volatile %struct.ScmObj* %argslist49453$k432771, %struct.ScmObj** %stackaddr$prim51047, align 8
%stackaddr$prim51048 = alloca %struct.ScmObj*, align 8
%argslist49453$k432772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43514, %struct.ScmObj* %argslist49453$k432771)
store volatile %struct.ScmObj* %argslist49453$k432772, %struct.ScmObj** %stackaddr$prim51048, align 8
%clofunc51049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43277)
musttail call tailcc void %clofunc51049(%struct.ScmObj* %k43277, %struct.ScmObj* %argslist49453$k432772)
ret void
falsebranch$cmp51046:
%stackaddr$prim51050 = alloca %struct.ScmObj*, align 8
%anf_45bind43026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst42914)
store volatile %struct.ScmObj* %anf_45bind43026, %struct.ScmObj** %stackaddr$prim51050, align 8
%stackaddr$makeclosure51051 = alloca %struct.ScmObj*, align 8
%fptrToInt51052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43524 to i64
%ae43524 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51052)
store volatile %struct.ScmObj* %ae43524, %struct.ScmObj** %stackaddr$makeclosure51051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43524, %struct.ScmObj* %f42915, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43524, %struct.ScmObj* %lst42914, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43524, %struct.ScmObj* %_37map42913, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae43524, %struct.ScmObj* %k43277, i64 3)
%argslist49462$f429150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51053 = alloca %struct.ScmObj*, align 8
%argslist49462$f429151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43026, %struct.ScmObj* %argslist49462$f429150)
store volatile %struct.ScmObj* %argslist49462$f429151, %struct.ScmObj** %stackaddr$prim51053, align 8
%stackaddr$prim51054 = alloca %struct.ScmObj*, align 8
%argslist49462$f429152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43524, %struct.ScmObj* %argslist49462$f429151)
store volatile %struct.ScmObj* %argslist49462$f429152, %struct.ScmObj** %stackaddr$prim51054, align 8
%clofunc51055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42915)
musttail call tailcc void %clofunc51055(%struct.ScmObj* %f42915, %struct.ScmObj* %argslist49462$f429152)
ret void
}

define tailcc void @proc_clo$ae43524(%struct.ScmObj* %env$ae43524,%struct.ScmObj* %current_45args49454) {
%stackaddr$env-ref51056 = alloca %struct.ScmObj*, align 8
%f42915 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43524, i64 0)
store %struct.ScmObj* %f42915, %struct.ScmObj** %stackaddr$env-ref51056
%stackaddr$env-ref51057 = alloca %struct.ScmObj*, align 8
%lst42914 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43524, i64 1)
store %struct.ScmObj* %lst42914, %struct.ScmObj** %stackaddr$env-ref51057
%stackaddr$env-ref51058 = alloca %struct.ScmObj*, align 8
%_37map42913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43524, i64 2)
store %struct.ScmObj* %_37map42913, %struct.ScmObj** %stackaddr$env-ref51058
%stackaddr$env-ref51059 = alloca %struct.ScmObj*, align 8
%k43277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43524, i64 3)
store %struct.ScmObj* %k43277, %struct.ScmObj** %stackaddr$env-ref51059
%stackaddr$prim51060 = alloca %struct.ScmObj*, align 8
%_95k43278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49454)
store volatile %struct.ScmObj* %_95k43278, %struct.ScmObj** %stackaddr$prim51060, align 8
%stackaddr$prim51061 = alloca %struct.ScmObj*, align 8
%current_45args49455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49454)
store volatile %struct.ScmObj* %current_45args49455, %struct.ScmObj** %stackaddr$prim51061, align 8
%stackaddr$prim51062 = alloca %struct.ScmObj*, align 8
%anf_45bind43027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49455)
store volatile %struct.ScmObj* %anf_45bind43027, %struct.ScmObj** %stackaddr$prim51062, align 8
%stackaddr$prim51063 = alloca %struct.ScmObj*, align 8
%anf_45bind43028 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst42914)
store volatile %struct.ScmObj* %anf_45bind43028, %struct.ScmObj** %stackaddr$prim51063, align 8
%stackaddr$makeclosure51064 = alloca %struct.ScmObj*, align 8
%fptrToInt51065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43528 to i64
%ae43528 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51065)
store volatile %struct.ScmObj* %ae43528, %struct.ScmObj** %stackaddr$makeclosure51064, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43528, %struct.ScmObj* %anf_45bind43027, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43528, %struct.ScmObj* %k43277, i64 1)
%argslist49461$_37map429130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51066 = alloca %struct.ScmObj*, align 8
%argslist49461$_37map429131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43028, %struct.ScmObj* %argslist49461$_37map429130)
store volatile %struct.ScmObj* %argslist49461$_37map429131, %struct.ScmObj** %stackaddr$prim51066, align 8
%stackaddr$prim51067 = alloca %struct.ScmObj*, align 8
%argslist49461$_37map429132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f42915, %struct.ScmObj* %argslist49461$_37map429131)
store volatile %struct.ScmObj* %argslist49461$_37map429132, %struct.ScmObj** %stackaddr$prim51067, align 8
%stackaddr$prim51068 = alloca %struct.ScmObj*, align 8
%argslist49461$_37map429133 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43528, %struct.ScmObj* %argslist49461$_37map429132)
store volatile %struct.ScmObj* %argslist49461$_37map429133, %struct.ScmObj** %stackaddr$prim51068, align 8
%clofunc51069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map42913)
musttail call tailcc void %clofunc51069(%struct.ScmObj* %_37map42913, %struct.ScmObj* %argslist49461$_37map429133)
ret void
}

define tailcc void @proc_clo$ae43528(%struct.ScmObj* %env$ae43528,%struct.ScmObj* %current_45args49457) {
%stackaddr$env-ref51070 = alloca %struct.ScmObj*, align 8
%anf_45bind43027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43528, i64 0)
store %struct.ScmObj* %anf_45bind43027, %struct.ScmObj** %stackaddr$env-ref51070
%stackaddr$env-ref51071 = alloca %struct.ScmObj*, align 8
%k43277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43528, i64 1)
store %struct.ScmObj* %k43277, %struct.ScmObj** %stackaddr$env-ref51071
%stackaddr$prim51072 = alloca %struct.ScmObj*, align 8
%_95k43279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49457)
store volatile %struct.ScmObj* %_95k43279, %struct.ScmObj** %stackaddr$prim51072, align 8
%stackaddr$prim51073 = alloca %struct.ScmObj*, align 8
%current_45args49458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49457)
store volatile %struct.ScmObj* %current_45args49458, %struct.ScmObj** %stackaddr$prim51073, align 8
%stackaddr$prim51074 = alloca %struct.ScmObj*, align 8
%anf_45bind43029 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49458)
store volatile %struct.ScmObj* %anf_45bind43029, %struct.ScmObj** %stackaddr$prim51074, align 8
%stackaddr$prim51075 = alloca %struct.ScmObj*, align 8
%cpsprim43280 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43027, %struct.ScmObj* %anf_45bind43029)
store volatile %struct.ScmObj* %cpsprim43280, %struct.ScmObj** %stackaddr$prim51075, align 8
%ae43534 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49460$k432770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51076 = alloca %struct.ScmObj*, align 8
%argslist49460$k432771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim43280, %struct.ScmObj* %argslist49460$k432770)
store volatile %struct.ScmObj* %argslist49460$k432771, %struct.ScmObj** %stackaddr$prim51076, align 8
%stackaddr$prim51077 = alloca %struct.ScmObj*, align 8
%argslist49460$k432772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43534, %struct.ScmObj* %argslist49460$k432771)
store volatile %struct.ScmObj* %argslist49460$k432772, %struct.ScmObj** %stackaddr$prim51077, align 8
%clofunc51078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43277)
musttail call tailcc void %clofunc51078(%struct.ScmObj* %k43277, %struct.ScmObj* %argslist49460$k432772)
ret void
}

define tailcc void @proc_clo$ae43427(%struct.ScmObj* %env$ae43427,%struct.ScmObj* %current_45args49466) {
%stackaddr$prim51079 = alloca %struct.ScmObj*, align 8
%k43281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49466)
store volatile %struct.ScmObj* %k43281, %struct.ScmObj** %stackaddr$prim51079, align 8
%stackaddr$prim51080 = alloca %struct.ScmObj*, align 8
%current_45args49467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49466)
store volatile %struct.ScmObj* %current_45args49467, %struct.ScmObj** %stackaddr$prim51080, align 8
%stackaddr$prim51081 = alloca %struct.ScmObj*, align 8
%_37foldr142917 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49467)
store volatile %struct.ScmObj* %_37foldr142917, %struct.ScmObj** %stackaddr$prim51081, align 8
%ae43429 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51082 = alloca %struct.ScmObj*, align 8
%fptrToInt51083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43430 to i64
%ae43430 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51083)
store volatile %struct.ScmObj* %ae43430, %struct.ScmObj** %stackaddr$makeclosure51082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43430, %struct.ScmObj* %_37foldr142917, i64 0)
%argslist49480$k432810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51084 = alloca %struct.ScmObj*, align 8
%argslist49480$k432811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43430, %struct.ScmObj* %argslist49480$k432810)
store volatile %struct.ScmObj* %argslist49480$k432811, %struct.ScmObj** %stackaddr$prim51084, align 8
%stackaddr$prim51085 = alloca %struct.ScmObj*, align 8
%argslist49480$k432812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43429, %struct.ScmObj* %argslist49480$k432811)
store volatile %struct.ScmObj* %argslist49480$k432812, %struct.ScmObj** %stackaddr$prim51085, align 8
%clofunc51086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43281)
musttail call tailcc void %clofunc51086(%struct.ScmObj* %k43281, %struct.ScmObj* %argslist49480$k432812)
ret void
}

define tailcc void @proc_clo$ae43430(%struct.ScmObj* %env$ae43430,%struct.ScmObj* %current_45args49469) {
%stackaddr$env-ref51087 = alloca %struct.ScmObj*, align 8
%_37foldr142917 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43430, i64 0)
store %struct.ScmObj* %_37foldr142917, %struct.ScmObj** %stackaddr$env-ref51087
%stackaddr$prim51088 = alloca %struct.ScmObj*, align 8
%k43282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49469)
store volatile %struct.ScmObj* %k43282, %struct.ScmObj** %stackaddr$prim51088, align 8
%stackaddr$prim51089 = alloca %struct.ScmObj*, align 8
%current_45args49470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49469)
store volatile %struct.ScmObj* %current_45args49470, %struct.ScmObj** %stackaddr$prim51089, align 8
%stackaddr$prim51090 = alloca %struct.ScmObj*, align 8
%f42920 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49470)
store volatile %struct.ScmObj* %f42920, %struct.ScmObj** %stackaddr$prim51090, align 8
%stackaddr$prim51091 = alloca %struct.ScmObj*, align 8
%current_45args49471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49470)
store volatile %struct.ScmObj* %current_45args49471, %struct.ScmObj** %stackaddr$prim51091, align 8
%stackaddr$prim51092 = alloca %struct.ScmObj*, align 8
%acc42919 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49471)
store volatile %struct.ScmObj* %acc42919, %struct.ScmObj** %stackaddr$prim51092, align 8
%stackaddr$prim51093 = alloca %struct.ScmObj*, align 8
%current_45args49472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49471)
store volatile %struct.ScmObj* %current_45args49472, %struct.ScmObj** %stackaddr$prim51093, align 8
%stackaddr$prim51094 = alloca %struct.ScmObj*, align 8
%lst42918 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49472)
store volatile %struct.ScmObj* %lst42918, %struct.ScmObj** %stackaddr$prim51094, align 8
%stackaddr$prim51095 = alloca %struct.ScmObj*, align 8
%anf_45bind43020 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst42918)
store volatile %struct.ScmObj* %anf_45bind43020, %struct.ScmObj** %stackaddr$prim51095, align 8
%truthy$cmp51096 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind43020)
%cmp$cmp51096 = icmp eq i64 %truthy$cmp51096, 1
br i1 %cmp$cmp51096, label %truebranch$cmp51096, label %falsebranch$cmp51096
truebranch$cmp51096:
%ae43434 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist49474$k432820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51097 = alloca %struct.ScmObj*, align 8
%argslist49474$k432821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42919, %struct.ScmObj* %argslist49474$k432820)
store volatile %struct.ScmObj* %argslist49474$k432821, %struct.ScmObj** %stackaddr$prim51097, align 8
%stackaddr$prim51098 = alloca %struct.ScmObj*, align 8
%argslist49474$k432822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43434, %struct.ScmObj* %argslist49474$k432821)
store volatile %struct.ScmObj* %argslist49474$k432822, %struct.ScmObj** %stackaddr$prim51098, align 8
%clofunc51099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43282)
musttail call tailcc void %clofunc51099(%struct.ScmObj* %k43282, %struct.ScmObj* %argslist49474$k432822)
ret void
falsebranch$cmp51096:
%stackaddr$prim51100 = alloca %struct.ScmObj*, align 8
%anf_45bind43021 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst42918)
store volatile %struct.ScmObj* %anf_45bind43021, %struct.ScmObj** %stackaddr$prim51100, align 8
%stackaddr$prim51101 = alloca %struct.ScmObj*, align 8
%anf_45bind43022 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst42918)
store volatile %struct.ScmObj* %anf_45bind43022, %struct.ScmObj** %stackaddr$prim51101, align 8
%stackaddr$makeclosure51102 = alloca %struct.ScmObj*, align 8
%fptrToInt51103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43442 to i64
%ae43442 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51103)
store volatile %struct.ScmObj* %ae43442, %struct.ScmObj** %stackaddr$makeclosure51102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43442, %struct.ScmObj* %k43282, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43442, %struct.ScmObj* %anf_45bind43021, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43442, %struct.ScmObj* %f42920, i64 2)
%argslist49479$_37foldr1429170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51104 = alloca %struct.ScmObj*, align 8
%argslist49479$_37foldr1429171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43022, %struct.ScmObj* %argslist49479$_37foldr1429170)
store volatile %struct.ScmObj* %argslist49479$_37foldr1429171, %struct.ScmObj** %stackaddr$prim51104, align 8
%stackaddr$prim51105 = alloca %struct.ScmObj*, align 8
%argslist49479$_37foldr1429172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc42919, %struct.ScmObj* %argslist49479$_37foldr1429171)
store volatile %struct.ScmObj* %argslist49479$_37foldr1429172, %struct.ScmObj** %stackaddr$prim51105, align 8
%stackaddr$prim51106 = alloca %struct.ScmObj*, align 8
%argslist49479$_37foldr1429173 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f42920, %struct.ScmObj* %argslist49479$_37foldr1429172)
store volatile %struct.ScmObj* %argslist49479$_37foldr1429173, %struct.ScmObj** %stackaddr$prim51106, align 8
%stackaddr$prim51107 = alloca %struct.ScmObj*, align 8
%argslist49479$_37foldr1429174 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43442, %struct.ScmObj* %argslist49479$_37foldr1429173)
store volatile %struct.ScmObj* %argslist49479$_37foldr1429174, %struct.ScmObj** %stackaddr$prim51107, align 8
%clofunc51108 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr142917)
musttail call tailcc void %clofunc51108(%struct.ScmObj* %_37foldr142917, %struct.ScmObj* %argslist49479$_37foldr1429174)
ret void
}

define tailcc void @proc_clo$ae43442(%struct.ScmObj* %env$ae43442,%struct.ScmObj* %current_45args49475) {
%stackaddr$env-ref51109 = alloca %struct.ScmObj*, align 8
%k43282 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43442, i64 0)
store %struct.ScmObj* %k43282, %struct.ScmObj** %stackaddr$env-ref51109
%stackaddr$env-ref51110 = alloca %struct.ScmObj*, align 8
%anf_45bind43021 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43442, i64 1)
store %struct.ScmObj* %anf_45bind43021, %struct.ScmObj** %stackaddr$env-ref51110
%stackaddr$env-ref51111 = alloca %struct.ScmObj*, align 8
%f42920 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43442, i64 2)
store %struct.ScmObj* %f42920, %struct.ScmObj** %stackaddr$env-ref51111
%stackaddr$prim51112 = alloca %struct.ScmObj*, align 8
%_95k43283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49475)
store volatile %struct.ScmObj* %_95k43283, %struct.ScmObj** %stackaddr$prim51112, align 8
%stackaddr$prim51113 = alloca %struct.ScmObj*, align 8
%current_45args49476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49475)
store volatile %struct.ScmObj* %current_45args49476, %struct.ScmObj** %stackaddr$prim51113, align 8
%stackaddr$prim51114 = alloca %struct.ScmObj*, align 8
%anf_45bind43023 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49476)
store volatile %struct.ScmObj* %anf_45bind43023, %struct.ScmObj** %stackaddr$prim51114, align 8
%argslist49478$f429200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51115 = alloca %struct.ScmObj*, align 8
%argslist49478$f429201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43023, %struct.ScmObj* %argslist49478$f429200)
store volatile %struct.ScmObj* %argslist49478$f429201, %struct.ScmObj** %stackaddr$prim51115, align 8
%stackaddr$prim51116 = alloca %struct.ScmObj*, align 8
%argslist49478$f429202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43021, %struct.ScmObj* %argslist49478$f429201)
store volatile %struct.ScmObj* %argslist49478$f429202, %struct.ScmObj** %stackaddr$prim51116, align 8
%stackaddr$prim51117 = alloca %struct.ScmObj*, align 8
%argslist49478$f429203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43282, %struct.ScmObj* %argslist49478$f429202)
store volatile %struct.ScmObj* %argslist49478$f429203, %struct.ScmObj** %stackaddr$prim51117, align 8
%clofunc51118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42920)
musttail call tailcc void %clofunc51118(%struct.ScmObj* %f42920, %struct.ScmObj* %argslist49478$f429203)
ret void
}

define tailcc void @proc_clo$ae43310(%struct.ScmObj* %env$ae43310,%struct.ScmObj* %current_45args49483) {
%stackaddr$prim51119 = alloca %struct.ScmObj*, align 8
%k43284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49483)
store volatile %struct.ScmObj* %k43284, %struct.ScmObj** %stackaddr$prim51119, align 8
%stackaddr$prim51120 = alloca %struct.ScmObj*, align 8
%current_45args49484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49483)
store volatile %struct.ScmObj* %current_45args49484, %struct.ScmObj** %stackaddr$prim51120, align 8
%stackaddr$prim51121 = alloca %struct.ScmObj*, align 8
%y42897 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49484)
store volatile %struct.ScmObj* %y42897, %struct.ScmObj** %stackaddr$prim51121, align 8
%ae43312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51122 = alloca %struct.ScmObj*, align 8
%fptrToInt51123 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43313 to i64
%ae43313 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51123)
store volatile %struct.ScmObj* %ae43313, %struct.ScmObj** %stackaddr$makeclosure51122, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43313, %struct.ScmObj* %y42897, i64 0)
%argslist49502$k432840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51124 = alloca %struct.ScmObj*, align 8
%argslist49502$k432841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43313, %struct.ScmObj* %argslist49502$k432840)
store volatile %struct.ScmObj* %argslist49502$k432841, %struct.ScmObj** %stackaddr$prim51124, align 8
%stackaddr$prim51125 = alloca %struct.ScmObj*, align 8
%argslist49502$k432842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43312, %struct.ScmObj* %argslist49502$k432841)
store volatile %struct.ScmObj* %argslist49502$k432842, %struct.ScmObj** %stackaddr$prim51125, align 8
%clofunc51126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k43284)
musttail call tailcc void %clofunc51126(%struct.ScmObj* %k43284, %struct.ScmObj* %argslist49502$k432842)
ret void
}

define tailcc void @proc_clo$ae43313(%struct.ScmObj* %env$ae43313,%struct.ScmObj* %current_45args49486) {
%stackaddr$env-ref51127 = alloca %struct.ScmObj*, align 8
%y42897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43313, i64 0)
store %struct.ScmObj* %y42897, %struct.ScmObj** %stackaddr$env-ref51127
%stackaddr$prim51128 = alloca %struct.ScmObj*, align 8
%k43285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49486)
store volatile %struct.ScmObj* %k43285, %struct.ScmObj** %stackaddr$prim51128, align 8
%stackaddr$prim51129 = alloca %struct.ScmObj*, align 8
%current_45args49487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49486)
store volatile %struct.ScmObj* %current_45args49487, %struct.ScmObj** %stackaddr$prim51129, align 8
%stackaddr$prim51130 = alloca %struct.ScmObj*, align 8
%f42898 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49487)
store volatile %struct.ScmObj* %f42898, %struct.ScmObj** %stackaddr$prim51130, align 8
%stackaddr$makeclosure51131 = alloca %struct.ScmObj*, align 8
%fptrToInt51132 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43314 to i64
%ae43314 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51132)
store volatile %struct.ScmObj* %ae43314, %struct.ScmObj** %stackaddr$makeclosure51131, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43314, %struct.ScmObj* %k43285, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43314, %struct.ScmObj* %f42898, i64 1)
%ae43315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51133 = alloca %struct.ScmObj*, align 8
%fptrToInt51134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43316 to i64
%ae43316 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51134)
store volatile %struct.ScmObj* %ae43316, %struct.ScmObj** %stackaddr$makeclosure51133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43316, %struct.ScmObj* %f42898, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43316, %struct.ScmObj* %y42897, i64 1)
%argslist49501$ae433140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51135 = alloca %struct.ScmObj*, align 8
%argslist49501$ae433141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43316, %struct.ScmObj* %argslist49501$ae433140)
store volatile %struct.ScmObj* %argslist49501$ae433141, %struct.ScmObj** %stackaddr$prim51135, align 8
%stackaddr$prim51136 = alloca %struct.ScmObj*, align 8
%argslist49501$ae433142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43315, %struct.ScmObj* %argslist49501$ae433141)
store volatile %struct.ScmObj* %argslist49501$ae433142, %struct.ScmObj** %stackaddr$prim51136, align 8
%clofunc51137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43314)
musttail call tailcc void %clofunc51137(%struct.ScmObj* %ae43314, %struct.ScmObj* %argslist49501$ae433142)
ret void
}

define tailcc void @proc_clo$ae43314(%struct.ScmObj* %env$ae43314,%struct.ScmObj* %current_45args49489) {
%stackaddr$env-ref51138 = alloca %struct.ScmObj*, align 8
%k43285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43314, i64 0)
store %struct.ScmObj* %k43285, %struct.ScmObj** %stackaddr$env-ref51138
%stackaddr$env-ref51139 = alloca %struct.ScmObj*, align 8
%f42898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43314, i64 1)
store %struct.ScmObj* %f42898, %struct.ScmObj** %stackaddr$env-ref51139
%stackaddr$prim51140 = alloca %struct.ScmObj*, align 8
%_95k43286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49489)
store volatile %struct.ScmObj* %_95k43286, %struct.ScmObj** %stackaddr$prim51140, align 8
%stackaddr$prim51141 = alloca %struct.ScmObj*, align 8
%current_45args49490 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49489)
store volatile %struct.ScmObj* %current_45args49490, %struct.ScmObj** %stackaddr$prim51141, align 8
%stackaddr$prim51142 = alloca %struct.ScmObj*, align 8
%anf_45bind43018 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49490)
store volatile %struct.ScmObj* %anf_45bind43018, %struct.ScmObj** %stackaddr$prim51142, align 8
%argslist49492$f428980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51143 = alloca %struct.ScmObj*, align 8
%argslist49492$f428981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind43018, %struct.ScmObj* %argslist49492$f428980)
store volatile %struct.ScmObj* %argslist49492$f428981, %struct.ScmObj** %stackaddr$prim51143, align 8
%stackaddr$prim51144 = alloca %struct.ScmObj*, align 8
%argslist49492$f428982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43285, %struct.ScmObj* %argslist49492$f428981)
store volatile %struct.ScmObj* %argslist49492$f428982, %struct.ScmObj** %stackaddr$prim51144, align 8
%clofunc51145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f42898)
musttail call tailcc void %clofunc51145(%struct.ScmObj* %f42898, %struct.ScmObj* %argslist49492$f428982)
ret void
}

define tailcc void @proc_clo$ae43316(%struct.ScmObj* %env$ae43316,%struct.ScmObj* %args4289943287) {
%stackaddr$env-ref51146 = alloca %struct.ScmObj*, align 8
%f42898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43316, i64 0)
store %struct.ScmObj* %f42898, %struct.ScmObj** %stackaddr$env-ref51146
%stackaddr$env-ref51147 = alloca %struct.ScmObj*, align 8
%y42897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43316, i64 1)
store %struct.ScmObj* %y42897, %struct.ScmObj** %stackaddr$env-ref51147
%stackaddr$prim51148 = alloca %struct.ScmObj*, align 8
%k43288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4289943287)
store volatile %struct.ScmObj* %k43288, %struct.ScmObj** %stackaddr$prim51148, align 8
%stackaddr$prim51149 = alloca %struct.ScmObj*, align 8
%args42899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4289943287)
store volatile %struct.ScmObj* %args42899, %struct.ScmObj** %stackaddr$prim51149, align 8
%stackaddr$makeclosure51150 = alloca %struct.ScmObj*, align 8
%fptrToInt51151 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43320 to i64
%ae43320 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51151)
store volatile %struct.ScmObj* %ae43320, %struct.ScmObj** %stackaddr$makeclosure51150, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43320, %struct.ScmObj* %args42899, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43320, %struct.ScmObj* %f42898, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43320, %struct.ScmObj* %k43288, i64 2)
%argslist49500$y428970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51152 = alloca %struct.ScmObj*, align 8
%argslist49500$y428971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y42897, %struct.ScmObj* %argslist49500$y428970)
store volatile %struct.ScmObj* %argslist49500$y428971, %struct.ScmObj** %stackaddr$prim51152, align 8
%stackaddr$prim51153 = alloca %struct.ScmObj*, align 8
%argslist49500$y428972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43320, %struct.ScmObj* %argslist49500$y428971)
store volatile %struct.ScmObj* %argslist49500$y428972, %struct.ScmObj** %stackaddr$prim51153, align 8
%clofunc51154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y42897)
musttail call tailcc void %clofunc51154(%struct.ScmObj* %y42897, %struct.ScmObj* %argslist49500$y428972)
ret void
}

define tailcc void @proc_clo$ae43320(%struct.ScmObj* %env$ae43320,%struct.ScmObj* %current_45args49493) {
%stackaddr$env-ref51155 = alloca %struct.ScmObj*, align 8
%args42899 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43320, i64 0)
store %struct.ScmObj* %args42899, %struct.ScmObj** %stackaddr$env-ref51155
%stackaddr$env-ref51156 = alloca %struct.ScmObj*, align 8
%f42898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43320, i64 1)
store %struct.ScmObj* %f42898, %struct.ScmObj** %stackaddr$env-ref51156
%stackaddr$env-ref51157 = alloca %struct.ScmObj*, align 8
%k43288 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43320, i64 2)
store %struct.ScmObj* %k43288, %struct.ScmObj** %stackaddr$env-ref51157
%stackaddr$prim51158 = alloca %struct.ScmObj*, align 8
%_95k43289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49493)
store volatile %struct.ScmObj* %_95k43289, %struct.ScmObj** %stackaddr$prim51158, align 8
%stackaddr$prim51159 = alloca %struct.ScmObj*, align 8
%current_45args49494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49493)
store volatile %struct.ScmObj* %current_45args49494, %struct.ScmObj** %stackaddr$prim51159, align 8
%stackaddr$prim51160 = alloca %struct.ScmObj*, align 8
%anf_45bind43016 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49494)
store volatile %struct.ScmObj* %anf_45bind43016, %struct.ScmObj** %stackaddr$prim51160, align 8
%stackaddr$makeclosure51161 = alloca %struct.ScmObj*, align 8
%fptrToInt51162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43323 to i64
%ae43323 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51162)
store volatile %struct.ScmObj* %ae43323, %struct.ScmObj** %stackaddr$makeclosure51161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43323, %struct.ScmObj* %args42899, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43323, %struct.ScmObj* %k43288, i64 1)
%argslist49499$anf_45bind430160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51163 = alloca %struct.ScmObj*, align 8
%argslist49499$anf_45bind430161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f42898, %struct.ScmObj* %argslist49499$anf_45bind430160)
store volatile %struct.ScmObj* %argslist49499$anf_45bind430161, %struct.ScmObj** %stackaddr$prim51163, align 8
%stackaddr$prim51164 = alloca %struct.ScmObj*, align 8
%argslist49499$anf_45bind430162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43323, %struct.ScmObj* %argslist49499$anf_45bind430161)
store volatile %struct.ScmObj* %argslist49499$anf_45bind430162, %struct.ScmObj** %stackaddr$prim51164, align 8
%clofunc51165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43016)
musttail call tailcc void %clofunc51165(%struct.ScmObj* %anf_45bind43016, %struct.ScmObj* %argslist49499$anf_45bind430162)
ret void
}

define tailcc void @proc_clo$ae43323(%struct.ScmObj* %env$ae43323,%struct.ScmObj* %current_45args49496) {
%stackaddr$env-ref51166 = alloca %struct.ScmObj*, align 8
%args42899 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43323, i64 0)
store %struct.ScmObj* %args42899, %struct.ScmObj** %stackaddr$env-ref51166
%stackaddr$env-ref51167 = alloca %struct.ScmObj*, align 8
%k43288 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43323, i64 1)
store %struct.ScmObj* %k43288, %struct.ScmObj** %stackaddr$env-ref51167
%stackaddr$prim51168 = alloca %struct.ScmObj*, align 8
%_95k43290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49496)
store volatile %struct.ScmObj* %_95k43290, %struct.ScmObj** %stackaddr$prim51168, align 8
%stackaddr$prim51169 = alloca %struct.ScmObj*, align 8
%current_45args49497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49496)
store volatile %struct.ScmObj* %current_45args49497, %struct.ScmObj** %stackaddr$prim51169, align 8
%stackaddr$prim51170 = alloca %struct.ScmObj*, align 8
%anf_45bind43017 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49497)
store volatile %struct.ScmObj* %anf_45bind43017, %struct.ScmObj** %stackaddr$prim51170, align 8
%stackaddr$prim51171 = alloca %struct.ScmObj*, align 8
%cpsargs43291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43288, %struct.ScmObj* %args42899)
store volatile %struct.ScmObj* %cpsargs43291, %struct.ScmObj** %stackaddr$prim51171, align 8
%clofunc51172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind43017)
musttail call tailcc void %clofunc51172(%struct.ScmObj* %anf_45bind43017, %struct.ScmObj* %cpsargs43291)
ret void
}

define tailcc void @proc_clo$ae43295(%struct.ScmObj* %env$ae43295,%struct.ScmObj* %current_45args49504) {
%stackaddr$prim51173 = alloca %struct.ScmObj*, align 8
%k43292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49504)
store volatile %struct.ScmObj* %k43292, %struct.ScmObj** %stackaddr$prim51173, align 8
%stackaddr$prim51174 = alloca %struct.ScmObj*, align 8
%current_45args49505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49504)
store volatile %struct.ScmObj* %current_45args49505, %struct.ScmObj** %stackaddr$prim51174, align 8
%stackaddr$prim51175 = alloca %struct.ScmObj*, align 8
%yu42896 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49505)
store volatile %struct.ScmObj* %yu42896, %struct.ScmObj** %stackaddr$prim51175, align 8
%argslist49507$yu428960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51176 = alloca %struct.ScmObj*, align 8
%argslist49507$yu428961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu42896, %struct.ScmObj* %argslist49507$yu428960)
store volatile %struct.ScmObj* %argslist49507$yu428961, %struct.ScmObj** %stackaddr$prim51176, align 8
%stackaddr$prim51177 = alloca %struct.ScmObj*, align 8
%argslist49507$yu428962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k43292, %struct.ScmObj* %argslist49507$yu428961)
store volatile %struct.ScmObj* %argslist49507$yu428962, %struct.ScmObj** %stackaddr$prim51177, align 8
%clofunc51178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu42896)
musttail call tailcc void %clofunc51178(%struct.ScmObj* %yu42896, %struct.ScmObj* %argslist49507$yu428962)
ret void
}