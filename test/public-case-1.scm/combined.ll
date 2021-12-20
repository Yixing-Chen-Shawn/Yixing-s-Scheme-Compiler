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
%mainenv56261 = call %struct.ScmObj* @const_init_null()
%mainargs56262 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv56261, %struct.ScmObj* %mainargs56262)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv56259,%struct.ScmObj* %mainargs56260) {
%stackaddr$makeclosure56263 = alloca %struct.ScmObj*, align 8
%fptrToInt56264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50250 to i64
%ae50250 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56264)
store volatile %struct.ScmObj* %ae50250, %struct.ScmObj** %stackaddr$makeclosure56263, align 8
%ae50251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56265 = alloca %struct.ScmObj*, align 8
%fptrToInt56266 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50252 to i64
%ae50252 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56266)
store volatile %struct.ScmObj* %ae50252, %struct.ScmObj** %stackaddr$makeclosure56265, align 8
%argslist56258$ae502500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%argslist56258$ae502501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist56258$ae502500)
store volatile %struct.ScmObj* %argslist56258$ae502501, %struct.ScmObj** %stackaddr$prim56267, align 8
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%argslist56258$ae502502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50251, %struct.ScmObj* %argslist56258$ae502501)
store volatile %struct.ScmObj* %argslist56258$ae502502, %struct.ScmObj** %stackaddr$prim56268, align 8
%clofunc56269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50250)
musttail call tailcc void %clofunc56269(%struct.ScmObj* %ae50250, %struct.ScmObj* %argslist56258$ae502502)
ret void
}

define tailcc void @proc_clo$ae50250(%struct.ScmObj* %env$ae50250,%struct.ScmObj* %current_45args55717) {
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%_95k50090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55717)
store volatile %struct.ScmObj* %_95k50090, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%current_45args55718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55717)
store volatile %struct.ScmObj* %current_45args55718, %struct.ScmObj** %stackaddr$prim56271, align 8
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%anf_45bind49979 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55718)
store volatile %struct.ScmObj* %anf_45bind49979, %struct.ScmObj** %stackaddr$prim56272, align 8
%stackaddr$makeclosure56273 = alloca %struct.ScmObj*, align 8
%fptrToInt56274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50265 to i64
%ae50265 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56274)
store volatile %struct.ScmObj* %ae50265, %struct.ScmObj** %stackaddr$makeclosure56273, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50265, %struct.ScmObj* %anf_45bind49979, i64 0)
%ae50266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56275 = alloca %struct.ScmObj*, align 8
%fptrToInt56276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50267 to i64
%ae50267 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56276)
store volatile %struct.ScmObj* %ae50267, %struct.ScmObj** %stackaddr$makeclosure56275, align 8
%argslist56253$ae502650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%argslist56253$ae502651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50267, %struct.ScmObj* %argslist56253$ae502650)
store volatile %struct.ScmObj* %argslist56253$ae502651, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%argslist56253$ae502652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50266, %struct.ScmObj* %argslist56253$ae502651)
store volatile %struct.ScmObj* %argslist56253$ae502652, %struct.ScmObj** %stackaddr$prim56278, align 8
%clofunc56279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50265)
musttail call tailcc void %clofunc56279(%struct.ScmObj* %ae50265, %struct.ScmObj* %argslist56253$ae502652)
ret void
}

define tailcc void @proc_clo$ae50265(%struct.ScmObj* %env$ae50265,%struct.ScmObj* %current_45args55720) {
%stackaddr$env-ref56280 = alloca %struct.ScmObj*, align 8
%anf_45bind49979 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50265, i64 0)
store %struct.ScmObj* %anf_45bind49979, %struct.ScmObj** %stackaddr$env-ref56280
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%_95k50091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55720)
store volatile %struct.ScmObj* %_95k50091, %struct.ScmObj** %stackaddr$prim56281, align 8
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%current_45args55721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55720)
store volatile %struct.ScmObj* %current_45args55721, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%anf_45bind49983 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55721)
store volatile %struct.ScmObj* %anf_45bind49983, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$makeclosure56284 = alloca %struct.ScmObj*, align 8
%fptrToInt56285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50380 to i64
%ae50380 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56285)
store volatile %struct.ScmObj* %ae50380, %struct.ScmObj** %stackaddr$makeclosure56284, align 8
%argslist56232$anf_45bind499790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%argslist56232$anf_45bind499791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49983, %struct.ScmObj* %argslist56232$anf_45bind499790)
store volatile %struct.ScmObj* %argslist56232$anf_45bind499791, %struct.ScmObj** %stackaddr$prim56286, align 8
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist56232$anf_45bind499792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50380, %struct.ScmObj* %argslist56232$anf_45bind499791)
store volatile %struct.ScmObj* %argslist56232$anf_45bind499792, %struct.ScmObj** %stackaddr$prim56287, align 8
%clofunc56288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind49979)
musttail call tailcc void %clofunc56288(%struct.ScmObj* %anf_45bind49979, %struct.ScmObj* %argslist56232$anf_45bind499792)
ret void
}

define tailcc void @proc_clo$ae50380(%struct.ScmObj* %env$ae50380,%struct.ScmObj* %current_45args55723) {
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%_95k50092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55723)
store volatile %struct.ScmObj* %_95k50092, %struct.ScmObj** %stackaddr$prim56289, align 8
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%current_45args55724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55723)
store volatile %struct.ScmObj* %current_45args55724, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55724)
store volatile %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$makeclosure56292 = alloca %struct.ScmObj*, align 8
%fptrToInt56293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50382 to i64
%ae50382 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56293)
store volatile %struct.ScmObj* %ae50382, %struct.ScmObj** %stackaddr$makeclosure56292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50382, %struct.ScmObj* %Ycmb49861, i64 0)
%ae50383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56294 = alloca %struct.ScmObj*, align 8
%fptrToInt56295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50384 to i64
%ae50384 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56295)
store volatile %struct.ScmObj* %ae50384, %struct.ScmObj** %stackaddr$makeclosure56294, align 8
%argslist56231$ae503820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%argslist56231$ae503821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50384, %struct.ScmObj* %argslist56231$ae503820)
store volatile %struct.ScmObj* %argslist56231$ae503821, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%argslist56231$ae503822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50383, %struct.ScmObj* %argslist56231$ae503821)
store volatile %struct.ScmObj* %argslist56231$ae503822, %struct.ScmObj** %stackaddr$prim56297, align 8
%clofunc56298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50382)
musttail call tailcc void %clofunc56298(%struct.ScmObj* %ae50382, %struct.ScmObj* %argslist56231$ae503822)
ret void
}

define tailcc void @proc_clo$ae50382(%struct.ScmObj* %env$ae50382,%struct.ScmObj* %current_45args55726) {
%stackaddr$env-ref56299 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50382, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56299
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%_95k50093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55726)
store volatile %struct.ScmObj* %_95k50093, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%current_45args55727 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55726)
store volatile %struct.ScmObj* %current_45args55727, %struct.ScmObj** %stackaddr$prim56301, align 8
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%anf_45bind49988 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55727)
store volatile %struct.ScmObj* %anf_45bind49988, %struct.ScmObj** %stackaddr$prim56302, align 8
%stackaddr$makeclosure56303 = alloca %struct.ScmObj*, align 8
%fptrToInt56304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50460 to i64
%ae50460 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56304)
store volatile %struct.ScmObj* %ae50460, %struct.ScmObj** %stackaddr$makeclosure56303, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50460, %struct.ScmObj* %Ycmb49861, i64 0)
%argslist56215$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%argslist56215$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49988, %struct.ScmObj* %argslist56215$Ycmb498610)
store volatile %struct.ScmObj* %argslist56215$Ycmb498611, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%argslist56215$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50460, %struct.ScmObj* %argslist56215$Ycmb498611)
store volatile %struct.ScmObj* %argslist56215$Ycmb498612, %struct.ScmObj** %stackaddr$prim56306, align 8
%clofunc56307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56307(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56215$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae50460(%struct.ScmObj* %env$ae50460,%struct.ScmObj* %current_45args55729) {
%stackaddr$env-ref56308 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50460, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56308
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%_95k50094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55729)
store volatile %struct.ScmObj* %_95k50094, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%current_45args55730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55729)
store volatile %struct.ScmObj* %current_45args55730, %struct.ScmObj** %stackaddr$prim56310, align 8
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55730)
store volatile %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$prim56311, align 8
%stackaddr$makeclosure56312 = alloca %struct.ScmObj*, align 8
%fptrToInt56313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50462 to i64
%ae50462 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56313)
store volatile %struct.ScmObj* %ae50462, %struct.ScmObj** %stackaddr$makeclosure56312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50462, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50462, %struct.ScmObj* %_37foldr149882, i64 1)
%ae50463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56314 = alloca %struct.ScmObj*, align 8
%fptrToInt56315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50464 to i64
%ae50464 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56315)
store volatile %struct.ScmObj* %ae50464, %struct.ScmObj** %stackaddr$makeclosure56314, align 8
%argslist56214$ae504620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%argslist56214$ae504621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50464, %struct.ScmObj* %argslist56214$ae504620)
store volatile %struct.ScmObj* %argslist56214$ae504621, %struct.ScmObj** %stackaddr$prim56316, align 8
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%argslist56214$ae504622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50463, %struct.ScmObj* %argslist56214$ae504621)
store volatile %struct.ScmObj* %argslist56214$ae504622, %struct.ScmObj** %stackaddr$prim56317, align 8
%clofunc56318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50462)
musttail call tailcc void %clofunc56318(%struct.ScmObj* %ae50462, %struct.ScmObj* %argslist56214$ae504622)
ret void
}

define tailcc void @proc_clo$ae50462(%struct.ScmObj* %env$ae50462,%struct.ScmObj* %current_45args55732) {
%stackaddr$env-ref56319 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50462, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56319
%stackaddr$env-ref56320 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50462, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56320
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%_95k50095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55732)
store volatile %struct.ScmObj* %_95k50095, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%current_45args55733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55732)
store volatile %struct.ScmObj* %current_45args55733, %struct.ScmObj** %stackaddr$prim56322, align 8
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%anf_45bind49994 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55733)
store volatile %struct.ScmObj* %anf_45bind49994, %struct.ScmObj** %stackaddr$prim56323, align 8
%stackaddr$makeclosure56324 = alloca %struct.ScmObj*, align 8
%fptrToInt56325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50557 to i64
%ae50557 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56325)
store volatile %struct.ScmObj* %ae50557, %struct.ScmObj** %stackaddr$makeclosure56324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50557, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50557, %struct.ScmObj* %_37foldr149882, i64 1)
%argslist56195$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%argslist56195$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49994, %struct.ScmObj* %argslist56195$Ycmb498610)
store volatile %struct.ScmObj* %argslist56195$Ycmb498611, %struct.ScmObj** %stackaddr$prim56326, align 8
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%argslist56195$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50557, %struct.ScmObj* %argslist56195$Ycmb498611)
store volatile %struct.ScmObj* %argslist56195$Ycmb498612, %struct.ScmObj** %stackaddr$prim56327, align 8
%clofunc56328 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56328(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56195$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae50557(%struct.ScmObj* %env$ae50557,%struct.ScmObj* %current_45args55735) {
%stackaddr$env-ref56329 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50557, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56329
%stackaddr$env-ref56330 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50557, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56330
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%_95k50096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55735)
store volatile %struct.ScmObj* %_95k50096, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%current_45args55736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55735)
store volatile %struct.ScmObj* %current_45args55736, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55736)
store volatile %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$makeclosure56334 = alloca %struct.ScmObj*, align 8
%fptrToInt56335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50559 to i64
%ae50559 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56335)
store volatile %struct.ScmObj* %ae50559, %struct.ScmObj** %stackaddr$makeclosure56334, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50559, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50559, %struct.ScmObj* %_37foldr149882, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50559, %struct.ScmObj* %_37map149878, i64 2)
%ae50560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56336 = alloca %struct.ScmObj*, align 8
%fptrToInt56337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50561 to i64
%ae50561 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56337)
store volatile %struct.ScmObj* %ae50561, %struct.ScmObj** %stackaddr$makeclosure56336, align 8
%argslist56194$ae505590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%argslist56194$ae505591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50561, %struct.ScmObj* %argslist56194$ae505590)
store volatile %struct.ScmObj* %argslist56194$ae505591, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%argslist56194$ae505592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50560, %struct.ScmObj* %argslist56194$ae505591)
store volatile %struct.ScmObj* %argslist56194$ae505592, %struct.ScmObj** %stackaddr$prim56339, align 8
%clofunc56340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50559)
musttail call tailcc void %clofunc56340(%struct.ScmObj* %ae50559, %struct.ScmObj* %argslist56194$ae505592)
ret void
}

define tailcc void @proc_clo$ae50559(%struct.ScmObj* %env$ae50559,%struct.ScmObj* %current_45args55738) {
%stackaddr$env-ref56341 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50559, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56341
%stackaddr$env-ref56342 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50559, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56342
%stackaddr$env-ref56343 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50559, i64 2)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56343
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%_95k50097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55738)
store volatile %struct.ScmObj* %_95k50097, %struct.ScmObj** %stackaddr$prim56344, align 8
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%current_45args55739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55738)
store volatile %struct.ScmObj* %current_45args55739, %struct.ScmObj** %stackaddr$prim56345, align 8
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%anf_45bind50001 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55739)
store volatile %struct.ScmObj* %anf_45bind50001, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$makeclosure56347 = alloca %struct.ScmObj*, align 8
%fptrToInt56348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50707 to i64
%ae50707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56348)
store volatile %struct.ScmObj* %ae50707, %struct.ScmObj** %stackaddr$makeclosure56347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37foldr149882, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37map149878, i64 2)
%argslist56178$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%argslist56178$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50001, %struct.ScmObj* %argslist56178$Ycmb498610)
store volatile %struct.ScmObj* %argslist56178$Ycmb498611, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%argslist56178$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50707, %struct.ScmObj* %argslist56178$Ycmb498611)
store volatile %struct.ScmObj* %argslist56178$Ycmb498612, %struct.ScmObj** %stackaddr$prim56350, align 8
%clofunc56351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56351(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56178$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae50707(%struct.ScmObj* %env$ae50707,%struct.ScmObj* %current_45args55741) {
%stackaddr$env-ref56352 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56352
%stackaddr$env-ref56353 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56353
%stackaddr$env-ref56354 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 2)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56354
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%_95k50098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55741)
store volatile %struct.ScmObj* %_95k50098, %struct.ScmObj** %stackaddr$prim56355, align 8
%stackaddr$prim56356 = alloca %struct.ScmObj*, align 8
%current_45args55742 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55741)
store volatile %struct.ScmObj* %current_45args55742, %struct.ScmObj** %stackaddr$prim56356, align 8
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55742)
store volatile %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$makeclosure56358 = alloca %struct.ScmObj*, align 8
%fptrToInt56359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50709 to i64
%ae50709 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56359)
store volatile %struct.ScmObj* %ae50709, %struct.ScmObj** %stackaddr$makeclosure56358, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50709, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50709, %struct.ScmObj* %_37take49874, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50709, %struct.ScmObj* %_37foldr149882, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50709, %struct.ScmObj* %_37map149878, i64 3)
%ae50710 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56360 = alloca %struct.ScmObj*, align 8
%fptrToInt56361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50711 to i64
%ae50711 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56361)
store volatile %struct.ScmObj* %ae50711, %struct.ScmObj** %stackaddr$makeclosure56360, align 8
%argslist56177$ae507090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%argslist56177$ae507091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50711, %struct.ScmObj* %argslist56177$ae507090)
store volatile %struct.ScmObj* %argslist56177$ae507091, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%argslist56177$ae507092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50710, %struct.ScmObj* %argslist56177$ae507091)
store volatile %struct.ScmObj* %argslist56177$ae507092, %struct.ScmObj** %stackaddr$prim56363, align 8
%clofunc56364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50709)
musttail call tailcc void %clofunc56364(%struct.ScmObj* %ae50709, %struct.ScmObj* %argslist56177$ae507092)
ret void
}

define tailcc void @proc_clo$ae50709(%struct.ScmObj* %env$ae50709,%struct.ScmObj* %current_45args55744) {
%stackaddr$env-ref56365 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50709, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56365
%stackaddr$env-ref56366 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50709, i64 1)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref56366
%stackaddr$env-ref56367 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50709, i64 2)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56367
%stackaddr$env-ref56368 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50709, i64 3)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56368
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%_95k50099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55744)
store volatile %struct.ScmObj* %_95k50099, %struct.ScmObj** %stackaddr$prim56369, align 8
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%current_45args55745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55744)
store volatile %struct.ScmObj* %current_45args55745, %struct.ScmObj** %stackaddr$prim56370, align 8
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%anf_45bind50005 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55745)
store volatile %struct.ScmObj* %anf_45bind50005, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$makeclosure56372 = alloca %struct.ScmObj*, align 8
%fptrToInt56373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50790 to i64
%ae50790 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56373)
store volatile %struct.ScmObj* %ae50790, %struct.ScmObj** %stackaddr$makeclosure56372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %_37take49874, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %_37foldr149882, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50790, %struct.ScmObj* %_37map149878, i64 3)
%argslist56163$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56374 = alloca %struct.ScmObj*, align 8
%argslist56163$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50005, %struct.ScmObj* %argslist56163$Ycmb498610)
store volatile %struct.ScmObj* %argslist56163$Ycmb498611, %struct.ScmObj** %stackaddr$prim56374, align 8
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%argslist56163$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50790, %struct.ScmObj* %argslist56163$Ycmb498611)
store volatile %struct.ScmObj* %argslist56163$Ycmb498612, %struct.ScmObj** %stackaddr$prim56375, align 8
%clofunc56376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56376(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56163$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae50790(%struct.ScmObj* %env$ae50790,%struct.ScmObj* %current_45args55747) {
%stackaddr$env-ref56377 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56377
%stackaddr$env-ref56378 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 1)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref56378
%stackaddr$env-ref56379 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 2)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56379
%stackaddr$env-ref56380 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50790, i64 3)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56380
%stackaddr$prim56381 = alloca %struct.ScmObj*, align 8
%_95k50100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55747)
store volatile %struct.ScmObj* %_95k50100, %struct.ScmObj** %stackaddr$prim56381, align 8
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%current_45args55748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55747)
store volatile %struct.ScmObj* %current_45args55748, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%_37length49871 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55748)
store volatile %struct.ScmObj* %_37length49871, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$makeclosure56384 = alloca %struct.ScmObj*, align 8
%fptrToInt56385 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50792 to i64
%ae50792 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56385)
store volatile %struct.ScmObj* %ae50792, %struct.ScmObj** %stackaddr$makeclosure56384, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50792, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50792, %struct.ScmObj* %_37take49874, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50792, %struct.ScmObj* %_37length49871, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50792, %struct.ScmObj* %_37foldr149882, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50792, %struct.ScmObj* %_37map149878, i64 4)
%ae50793 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56386 = alloca %struct.ScmObj*, align 8
%fptrToInt56387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50794 to i64
%ae50794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56387)
store volatile %struct.ScmObj* %ae50794, %struct.ScmObj** %stackaddr$makeclosure56386, align 8
%argslist56162$ae507920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%argslist56162$ae507921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50794, %struct.ScmObj* %argslist56162$ae507920)
store volatile %struct.ScmObj* %argslist56162$ae507921, %struct.ScmObj** %stackaddr$prim56388, align 8
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%argslist56162$ae507922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50793, %struct.ScmObj* %argslist56162$ae507921)
store volatile %struct.ScmObj* %argslist56162$ae507922, %struct.ScmObj** %stackaddr$prim56389, align 8
%clofunc56390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50792)
musttail call tailcc void %clofunc56390(%struct.ScmObj* %ae50792, %struct.ScmObj* %argslist56162$ae507922)
ret void
}

define tailcc void @proc_clo$ae50792(%struct.ScmObj* %env$ae50792,%struct.ScmObj* %current_45args55750) {
%stackaddr$env-ref56391 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50792, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56391
%stackaddr$env-ref56392 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50792, i64 1)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref56392
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%_37length49871 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50792, i64 2)
store %struct.ScmObj* %_37length49871, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$env-ref56394 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50792, i64 3)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56394
%stackaddr$env-ref56395 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50792, i64 4)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56395
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%_95k50101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55750)
store volatile %struct.ScmObj* %_95k50101, %struct.ScmObj** %stackaddr$prim56396, align 8
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%current_45args55751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55750)
store volatile %struct.ScmObj* %current_45args55751, %struct.ScmObj** %stackaddr$prim56397, align 8
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%anf_45bind50010 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55751)
store volatile %struct.ScmObj* %anf_45bind50010, %struct.ScmObj** %stackaddr$prim56398, align 8
%stackaddr$makeclosure56399 = alloca %struct.ScmObj*, align 8
%fptrToInt56400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50869 to i64
%ae50869 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56400)
store volatile %struct.ScmObj* %ae50869, %struct.ScmObj** %stackaddr$makeclosure56399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %_37take49874, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %_37length49871, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %_37foldr149882, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50869, %struct.ScmObj* %_37map149878, i64 4)
%argslist56146$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%argslist56146$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50010, %struct.ScmObj* %argslist56146$Ycmb498610)
store volatile %struct.ScmObj* %argslist56146$Ycmb498611, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%argslist56146$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50869, %struct.ScmObj* %argslist56146$Ycmb498611)
store volatile %struct.ScmObj* %argslist56146$Ycmb498612, %struct.ScmObj** %stackaddr$prim56402, align 8
%clofunc56403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56403(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56146$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae50869(%struct.ScmObj* %env$ae50869,%struct.ScmObj* %current_45args55753) {
%stackaddr$env-ref56404 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56404
%stackaddr$env-ref56405 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 1)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref56405
%stackaddr$env-ref56406 = alloca %struct.ScmObj*, align 8
%_37length49871 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 2)
store %struct.ScmObj* %_37length49871, %struct.ScmObj** %stackaddr$env-ref56406
%stackaddr$env-ref56407 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 3)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56407
%stackaddr$env-ref56408 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50869, i64 4)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56408
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%_95k50102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55753)
store volatile %struct.ScmObj* %_95k50102, %struct.ScmObj** %stackaddr$prim56409, align 8
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%current_45args55754 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55753)
store volatile %struct.ScmObj* %current_45args55754, %struct.ScmObj** %stackaddr$prim56410, align 8
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55754)
store volatile %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$prim56411, align 8
%stackaddr$makeclosure56412 = alloca %struct.ScmObj*, align 8
%fptrToInt56413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50871 to i64
%ae50871 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56413)
store volatile %struct.ScmObj* %ae50871, %struct.ScmObj** %stackaddr$makeclosure56412, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %_37take49874, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %_37length49871, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae50871, %struct.ScmObj* %_37map149878, i64 5)
%ae50872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56414 = alloca %struct.ScmObj*, align 8
%fptrToInt56415 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50873 to i64
%ae50873 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56415)
store volatile %struct.ScmObj* %ae50873, %struct.ScmObj** %stackaddr$makeclosure56414, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50873, %struct.ScmObj* %_37foldl149866, i64 0)
%argslist56145$ae508710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56416 = alloca %struct.ScmObj*, align 8
%argslist56145$ae508711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50873, %struct.ScmObj* %argslist56145$ae508710)
store volatile %struct.ScmObj* %argslist56145$ae508711, %struct.ScmObj** %stackaddr$prim56416, align 8
%stackaddr$prim56417 = alloca %struct.ScmObj*, align 8
%argslist56145$ae508712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50872, %struct.ScmObj* %argslist56145$ae508711)
store volatile %struct.ScmObj* %argslist56145$ae508712, %struct.ScmObj** %stackaddr$prim56417, align 8
%clofunc56418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50871)
musttail call tailcc void %clofunc56418(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist56145$ae508712)
ret void
}

define tailcc void @proc_clo$ae50871(%struct.ScmObj* %env$ae50871,%struct.ScmObj* %current_45args55756) {
%stackaddr$env-ref56419 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56419
%stackaddr$env-ref56420 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56420
%stackaddr$env-ref56421 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56421
%stackaddr$env-ref56422 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 3)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref56422
%stackaddr$env-ref56423 = alloca %struct.ScmObj*, align 8
%_37length49871 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 4)
store %struct.ScmObj* %_37length49871, %struct.ScmObj** %stackaddr$env-ref56423
%stackaddr$env-ref56424 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50871, i64 5)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56424
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%_95k50103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55756)
store volatile %struct.ScmObj* %_95k50103, %struct.ScmObj** %stackaddr$prim56425, align 8
%stackaddr$prim56426 = alloca %struct.ScmObj*, align 8
%current_45args55757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55756)
store volatile %struct.ScmObj* %current_45args55757, %struct.ScmObj** %stackaddr$prim56426, align 8
%stackaddr$prim56427 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55757)
store volatile %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$prim56427, align 8
%stackaddr$makeclosure56428 = alloca %struct.ScmObj*, align 8
%fptrToInt56429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50925 to i64
%ae50925 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56429)
store volatile %struct.ScmObj* %ae50925, %struct.ScmObj** %stackaddr$makeclosure56428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50925, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50925, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50925, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50925, %struct.ScmObj* %_37last49904, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50925, %struct.ScmObj* %_37map149878, i64 4)
%ae50926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56430 = alloca %struct.ScmObj*, align 8
%fptrToInt56431 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50927 to i64
%ae50927 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56431)
store volatile %struct.ScmObj* %ae50927, %struct.ScmObj** %stackaddr$makeclosure56430, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50927, %struct.ScmObj* %_37take49874, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50927, %struct.ScmObj* %_37length49871, i64 1)
%argslist56131$ae509250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%argslist56131$ae509251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50927, %struct.ScmObj* %argslist56131$ae509250)
store volatile %struct.ScmObj* %argslist56131$ae509251, %struct.ScmObj** %stackaddr$prim56432, align 8
%stackaddr$prim56433 = alloca %struct.ScmObj*, align 8
%argslist56131$ae509252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50926, %struct.ScmObj* %argslist56131$ae509251)
store volatile %struct.ScmObj* %argslist56131$ae509252, %struct.ScmObj** %stackaddr$prim56433, align 8
%clofunc56434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50925)
musttail call tailcc void %clofunc56434(%struct.ScmObj* %ae50925, %struct.ScmObj* %argslist56131$ae509252)
ret void
}

define tailcc void @proc_clo$ae50925(%struct.ScmObj* %env$ae50925,%struct.ScmObj* %current_45args55759) {
%stackaddr$env-ref56435 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50925, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56435
%stackaddr$env-ref56436 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50925, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56436
%stackaddr$env-ref56437 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50925, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56437
%stackaddr$env-ref56438 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50925, i64 3)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref56438
%stackaddr$env-ref56439 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50925, i64 4)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref56439
%stackaddr$prim56440 = alloca %struct.ScmObj*, align 8
%_95k50104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55759)
store volatile %struct.ScmObj* %_95k50104, %struct.ScmObj** %stackaddr$prim56440, align 8
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%current_45args55760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55759)
store volatile %struct.ScmObj* %current_45args55760, %struct.ScmObj** %stackaddr$prim56441, align 8
%stackaddr$prim56442 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55760)
store volatile %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$prim56442, align 8
%stackaddr$makeclosure56443 = alloca %struct.ScmObj*, align 8
%fptrToInt56444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50955 to i64
%ae50955 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56444)
store volatile %struct.ScmObj* %ae50955, %struct.ScmObj** %stackaddr$makeclosure56443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %_37last49904, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %_37drop_45right49901, i64 4)
%ae50956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56445 = alloca %struct.ScmObj*, align 8
%fptrToInt56446 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50957 to i64
%ae50957 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56446)
store volatile %struct.ScmObj* %ae50957, %struct.ScmObj** %stackaddr$makeclosure56445, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50957, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50957, %struct.ScmObj* %_37map149878, i64 1)
%argslist56121$ae509550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%argslist56121$ae509551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50957, %struct.ScmObj* %argslist56121$ae509550)
store volatile %struct.ScmObj* %argslist56121$ae509551, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%argslist56121$ae509552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50956, %struct.ScmObj* %argslist56121$ae509551)
store volatile %struct.ScmObj* %argslist56121$ae509552, %struct.ScmObj** %stackaddr$prim56448, align 8
%clofunc56449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50955)
musttail call tailcc void %clofunc56449(%struct.ScmObj* %ae50955, %struct.ScmObj* %argslist56121$ae509552)
ret void
}

define tailcc void @proc_clo$ae50955(%struct.ScmObj* %env$ae50955,%struct.ScmObj* %current_45args55762) {
%stackaddr$env-ref56450 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56450
%stackaddr$env-ref56451 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56451
%stackaddr$env-ref56452 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56452
%stackaddr$env-ref56453 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 3)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref56453
%stackaddr$env-ref56454 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 4)
store %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$env-ref56454
%stackaddr$prim56455 = alloca %struct.ScmObj*, align 8
%_95k50105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55762)
store volatile %struct.ScmObj* %_95k50105, %struct.ScmObj** %stackaddr$prim56455, align 8
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%current_45args55763 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55762)
store volatile %struct.ScmObj* %current_45args55763, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%anf_45bind50026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55763)
store volatile %struct.ScmObj* %anf_45bind50026, %struct.ScmObj** %stackaddr$prim56457, align 8
%stackaddr$makeclosure56458 = alloca %struct.ScmObj*, align 8
%fptrToInt56459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51339 to i64
%ae51339 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56459)
store volatile %struct.ScmObj* %ae51339, %struct.ScmObj** %stackaddr$makeclosure56458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %_37last49904, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %_37drop_45right49901, i64 4)
%argslist56061$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%argslist56061$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50026, %struct.ScmObj* %argslist56061$Ycmb498610)
store volatile %struct.ScmObj* %argslist56061$Ycmb498611, %struct.ScmObj** %stackaddr$prim56460, align 8
%stackaddr$prim56461 = alloca %struct.ScmObj*, align 8
%argslist56061$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51339, %struct.ScmObj* %argslist56061$Ycmb498611)
store volatile %struct.ScmObj* %argslist56061$Ycmb498612, %struct.ScmObj** %stackaddr$prim56461, align 8
%clofunc56462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56462(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist56061$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae51339(%struct.ScmObj* %env$ae51339,%struct.ScmObj* %current_45args55765) {
%stackaddr$env-ref56463 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56463
%stackaddr$env-ref56464 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56464
%stackaddr$env-ref56465 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56465
%stackaddr$env-ref56466 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 3)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref56466
%stackaddr$env-ref56467 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 4)
store %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$env-ref56467
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%_95k50106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55765)
store volatile %struct.ScmObj* %_95k50106, %struct.ScmObj** %stackaddr$prim56468, align 8
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%current_45args55766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55765)
store volatile %struct.ScmObj* %current_45args55766, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55766)
store volatile %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$prim56470, align 8
%stackaddr$makeclosure56471 = alloca %struct.ScmObj*, align 8
%fptrToInt56472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51341 to i64
%ae51341 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56472)
store volatile %struct.ScmObj* %ae51341, %struct.ScmObj** %stackaddr$makeclosure56471, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %_37last49904, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %_37foldr49887, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51341, %struct.ScmObj* %_37drop_45right49901, i64 5)
%ae51342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56473 = alloca %struct.ScmObj*, align 8
%fptrToInt56474 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51343 to i64
%ae51343 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56474)
store volatile %struct.ScmObj* %ae51343, %struct.ScmObj** %stackaddr$makeclosure56473, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51343, %struct.ScmObj* %_37foldr149882, i64 0)
%argslist56060$ae513410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56475 = alloca %struct.ScmObj*, align 8
%argslist56060$ae513411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51343, %struct.ScmObj* %argslist56060$ae513410)
store volatile %struct.ScmObj* %argslist56060$ae513411, %struct.ScmObj** %stackaddr$prim56475, align 8
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%argslist56060$ae513412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51342, %struct.ScmObj* %argslist56060$ae513411)
store volatile %struct.ScmObj* %argslist56060$ae513412, %struct.ScmObj** %stackaddr$prim56476, align 8
%clofunc56477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51341)
musttail call tailcc void %clofunc56477(%struct.ScmObj* %ae51341, %struct.ScmObj* %argslist56060$ae513412)
ret void
}

define tailcc void @proc_clo$ae51341(%struct.ScmObj* %env$ae51341,%struct.ScmObj* %current_45args55768) {
%stackaddr$env-ref56478 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56478
%stackaddr$env-ref56479 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56479
%stackaddr$env-ref56480 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56480
%stackaddr$env-ref56481 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 3)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref56481
%stackaddr$env-ref56482 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 4)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref56482
%stackaddr$env-ref56483 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51341, i64 5)
store %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$env-ref56483
%stackaddr$prim56484 = alloca %struct.ScmObj*, align 8
%_95k50107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55768)
store volatile %struct.ScmObj* %_95k50107, %struct.ScmObj** %stackaddr$prim56484, align 8
%stackaddr$prim56485 = alloca %struct.ScmObj*, align 8
%current_45args55769 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55768)
store volatile %struct.ScmObj* %current_45args55769, %struct.ScmObj** %stackaddr$prim56485, align 8
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55769)
store volatile %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$makeclosure56487 = alloca %struct.ScmObj*, align 8
%fptrToInt56488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51418 to i64
%ae51418 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56488)
store volatile %struct.ScmObj* %ae51418, %struct.ScmObj** %stackaddr$makeclosure56487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %_37foldr149882, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %Ycmb49861, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %_37foldr49887, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51418, %struct.ScmObj* %_37map149913, i64 4)
%ae51419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56489 = alloca %struct.ScmObj*, align 8
%fptrToInt56490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51420 to i64
%ae51420 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56490)
store volatile %struct.ScmObj* %ae51420, %struct.ScmObj** %stackaddr$makeclosure56489, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51420, %struct.ScmObj* %_37last49904, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51420, %struct.ScmObj* %_37foldr49887, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51420, %struct.ScmObj* %_37drop_45right49901, i64 2)
%argslist56041$ae514180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56491 = alloca %struct.ScmObj*, align 8
%argslist56041$ae514181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51420, %struct.ScmObj* %argslist56041$ae514180)
store volatile %struct.ScmObj* %argslist56041$ae514181, %struct.ScmObj** %stackaddr$prim56491, align 8
%stackaddr$prim56492 = alloca %struct.ScmObj*, align 8
%argslist56041$ae514182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51419, %struct.ScmObj* %argslist56041$ae514181)
store volatile %struct.ScmObj* %argslist56041$ae514182, %struct.ScmObj** %stackaddr$prim56492, align 8
%clofunc56493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51418)
musttail call tailcc void %clofunc56493(%struct.ScmObj* %ae51418, %struct.ScmObj* %argslist56041$ae514182)
ret void
}

define tailcc void @proc_clo$ae51418(%struct.ScmObj* %env$ae51418,%struct.ScmObj* %current_45args55771) {
%stackaddr$env-ref56494 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref56494
%stackaddr$env-ref56495 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56495
%stackaddr$env-ref56496 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 2)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56496
%stackaddr$env-ref56497 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 3)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref56497
%stackaddr$env-ref56498 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51418, i64 4)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref56498
%stackaddr$prim56499 = alloca %struct.ScmObj*, align 8
%_95k50108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55771)
store volatile %struct.ScmObj* %_95k50108, %struct.ScmObj** %stackaddr$prim56499, align 8
%stackaddr$prim56500 = alloca %struct.ScmObj*, align 8
%current_45args55772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55771)
store volatile %struct.ScmObj* %current_45args55772, %struct.ScmObj** %stackaddr$prim56500, align 8
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%_37map49908 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55772)
store volatile %struct.ScmObj* %_37map49908, %struct.ScmObj** %stackaddr$prim56501, align 8
%stackaddr$makeclosure56502 = alloca %struct.ScmObj*, align 8
%fptrToInt56503 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51564 to i64
%ae51564 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56503)
store volatile %struct.ScmObj* %ae51564, %struct.ScmObj** %stackaddr$makeclosure56502, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51564, %struct.ScmObj* %Ycmb49861, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51564, %struct.ScmObj* %_37foldl149866, i64 1)
%ae51565 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56504 = alloca %struct.ScmObj*, align 8
%fptrToInt56505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51566 to i64
%ae51566 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56505)
store volatile %struct.ScmObj* %ae51566, %struct.ScmObj** %stackaddr$makeclosure56504, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51566, %struct.ScmObj* %_37foldr49887, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51566, %struct.ScmObj* %_37foldr149882, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51566, %struct.ScmObj* %_37map149913, i64 2)
%argslist56024$ae515640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56506 = alloca %struct.ScmObj*, align 8
%argslist56024$ae515641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51566, %struct.ScmObj* %argslist56024$ae515640)
store volatile %struct.ScmObj* %argslist56024$ae515641, %struct.ScmObj** %stackaddr$prim56506, align 8
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%argslist56024$ae515642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51565, %struct.ScmObj* %argslist56024$ae515641)
store volatile %struct.ScmObj* %argslist56024$ae515642, %struct.ScmObj** %stackaddr$prim56507, align 8
%clofunc56508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51564)
musttail call tailcc void %clofunc56508(%struct.ScmObj* %ae51564, %struct.ScmObj* %argslist56024$ae515642)
ret void
}

define tailcc void @proc_clo$ae51564(%struct.ScmObj* %env$ae51564,%struct.ScmObj* %current_45args55774) {
%stackaddr$env-ref56509 = alloca %struct.ScmObj*, align 8
%Ycmb49861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51564, i64 0)
store %struct.ScmObj* %Ycmb49861, %struct.ScmObj** %stackaddr$env-ref56509
%stackaddr$env-ref56510 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51564, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56510
%stackaddr$prim56511 = alloca %struct.ScmObj*, align 8
%_95k50109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55774)
store volatile %struct.ScmObj* %_95k50109, %struct.ScmObj** %stackaddr$prim56511, align 8
%stackaddr$prim56512 = alloca %struct.ScmObj*, align 8
%current_45args55775 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55774)
store volatile %struct.ScmObj* %current_45args55775, %struct.ScmObj** %stackaddr$prim56512, align 8
%stackaddr$prim56513 = alloca %struct.ScmObj*, align 8
%anf_45bind50046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55775)
store volatile %struct.ScmObj* %anf_45bind50046, %struct.ScmObj** %stackaddr$prim56513, align 8
%stackaddr$makeclosure56514 = alloca %struct.ScmObj*, align 8
%fptrToInt56515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51956 to i64
%ae51956 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56515)
store volatile %struct.ScmObj* %ae51956, %struct.ScmObj** %stackaddr$makeclosure56514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51956, %struct.ScmObj* %_37foldl149866, i64 0)
%argslist55964$Ycmb498610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%argslist55964$Ycmb498611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50046, %struct.ScmObj* %argslist55964$Ycmb498610)
store volatile %struct.ScmObj* %argslist55964$Ycmb498611, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%argslist55964$Ycmb498612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51956, %struct.ScmObj* %argslist55964$Ycmb498611)
store volatile %struct.ScmObj* %argslist55964$Ycmb498612, %struct.ScmObj** %stackaddr$prim56517, align 8
%clofunc56518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb49861)
musttail call tailcc void %clofunc56518(%struct.ScmObj* %Ycmb49861, %struct.ScmObj* %argslist55964$Ycmb498612)
ret void
}

define tailcc void @proc_clo$ae51956(%struct.ScmObj* %env$ae51956,%struct.ScmObj* %current_45args55777) {
%stackaddr$env-ref56519 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51956, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56519
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%_95k50110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55777)
store volatile %struct.ScmObj* %_95k50110, %struct.ScmObj** %stackaddr$prim56520, align 8
%stackaddr$prim56521 = alloca %struct.ScmObj*, align 8
%current_45args55778 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55777)
store volatile %struct.ScmObj* %current_45args55778, %struct.ScmObj** %stackaddr$prim56521, align 8
%stackaddr$prim56522 = alloca %struct.ScmObj*, align 8
%_37foldl49964 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55778)
store volatile %struct.ScmObj* %_37foldl49964, %struct.ScmObj** %stackaddr$prim56522, align 8
%stackaddr$makeclosure56523 = alloca %struct.ScmObj*, align 8
%fptrToInt56524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51958 to i64
%ae51958 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56524)
store volatile %struct.ScmObj* %ae51958, %struct.ScmObj** %stackaddr$makeclosure56523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51958, %struct.ScmObj* %_37foldl149866, i64 0)
%ae51959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56525 = alloca %struct.ScmObj*, align 8
%fptrToInt56526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51960 to i64
%ae51960 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56526)
store volatile %struct.ScmObj* %ae51960, %struct.ScmObj** %stackaddr$makeclosure56525, align 8
%argslist55963$ae519580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56527 = alloca %struct.ScmObj*, align 8
%argslist55963$ae519581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51960, %struct.ScmObj* %argslist55963$ae519580)
store volatile %struct.ScmObj* %argslist55963$ae519581, %struct.ScmObj** %stackaddr$prim56527, align 8
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%argslist55963$ae519582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51959, %struct.ScmObj* %argslist55963$ae519581)
store volatile %struct.ScmObj* %argslist55963$ae519582, %struct.ScmObj** %stackaddr$prim56528, align 8
%clofunc56529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51958)
musttail call tailcc void %clofunc56529(%struct.ScmObj* %ae51958, %struct.ScmObj* %argslist55963$ae519582)
ret void
}

define tailcc void @proc_clo$ae51958(%struct.ScmObj* %env$ae51958,%struct.ScmObj* %current_45args55780) {
%stackaddr$env-ref56530 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51958, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56530
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%_95k50111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55780)
store volatile %struct.ScmObj* %_95k50111, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%current_45args55781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55780)
store volatile %struct.ScmObj* %current_45args55781, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%_37_6249961 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55781)
store volatile %struct.ScmObj* %_37_6249961, %struct.ScmObj** %stackaddr$prim56533, align 8
%stackaddr$makeclosure56534 = alloca %struct.ScmObj*, align 8
%fptrToInt56535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51982 to i64
%ae51982 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56535)
store volatile %struct.ScmObj* %ae51982, %struct.ScmObj** %stackaddr$makeclosure56534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51982, %struct.ScmObj* %_37foldl149866, i64 0)
%ae51983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56536 = alloca %struct.ScmObj*, align 8
%fptrToInt56537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51984 to i64
%ae51984 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56537)
store volatile %struct.ScmObj* %ae51984, %struct.ScmObj** %stackaddr$makeclosure56536, align 8
%argslist55957$ae519820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%argslist55957$ae519821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51984, %struct.ScmObj* %argslist55957$ae519820)
store volatile %struct.ScmObj* %argslist55957$ae519821, %struct.ScmObj** %stackaddr$prim56538, align 8
%stackaddr$prim56539 = alloca %struct.ScmObj*, align 8
%argslist55957$ae519822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51983, %struct.ScmObj* %argslist55957$ae519821)
store volatile %struct.ScmObj* %argslist55957$ae519822, %struct.ScmObj** %stackaddr$prim56539, align 8
%clofunc56540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51982)
musttail call tailcc void %clofunc56540(%struct.ScmObj* %ae51982, %struct.ScmObj* %argslist55957$ae519822)
ret void
}

define tailcc void @proc_clo$ae51982(%struct.ScmObj* %env$ae51982,%struct.ScmObj* %current_45args55783) {
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51982, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%_95k50112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55783)
store volatile %struct.ScmObj* %_95k50112, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%current_45args55784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55783)
store volatile %struct.ScmObj* %current_45args55784, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%_37_62_6149958 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55784)
store volatile %struct.ScmObj* %_37_62_6149958, %struct.ScmObj** %stackaddr$prim56544, align 8
%ae52006 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52007 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%_37append49954 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae52006, %struct.ScmObj* %ae52007)
store volatile %struct.ScmObj* %_37append49954, %struct.ScmObj** %stackaddr$prim56545, align 8
%stackaddr$makeclosure56546 = alloca %struct.ScmObj*, align 8
%fptrToInt56547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52008 to i64
%ae52008 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56547)
store volatile %struct.ScmObj* %ae52008, %struct.ScmObj** %stackaddr$makeclosure56546, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52008, %struct.ScmObj* %_37append49954, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52008, %struct.ScmObj* %_37foldl149866, i64 1)
%ae52009 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56548 = alloca %struct.ScmObj*, align 8
%fptrToInt56549 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52010 to i64
%ae52010 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56549)
store volatile %struct.ScmObj* %ae52010, %struct.ScmObj** %stackaddr$makeclosure56548, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52010, %struct.ScmObj* %_37append49954, i64 0)
%argslist55951$ae520080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56550 = alloca %struct.ScmObj*, align 8
%argslist55951$ae520081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52010, %struct.ScmObj* %argslist55951$ae520080)
store volatile %struct.ScmObj* %argslist55951$ae520081, %struct.ScmObj** %stackaddr$prim56550, align 8
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%argslist55951$ae520082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52009, %struct.ScmObj* %argslist55951$ae520081)
store volatile %struct.ScmObj* %argslist55951$ae520082, %struct.ScmObj** %stackaddr$prim56551, align 8
%clofunc56552 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52008)
musttail call tailcc void %clofunc56552(%struct.ScmObj* %ae52008, %struct.ScmObj* %argslist55951$ae520082)
ret void
}

define tailcc void @proc_clo$ae52008(%struct.ScmObj* %env$ae52008,%struct.ScmObj* %current_45args55786) {
%stackaddr$env-ref56553 = alloca %struct.ScmObj*, align 8
%_37append49954 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52008, i64 0)
store %struct.ScmObj* %_37append49954, %struct.ScmObj** %stackaddr$env-ref56553
%stackaddr$env-ref56554 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52008, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56554
%stackaddr$prim56555 = alloca %struct.ScmObj*, align 8
%_95k50113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55786)
store volatile %struct.ScmObj* %_95k50113, %struct.ScmObj** %stackaddr$prim56555, align 8
%stackaddr$prim56556 = alloca %struct.ScmObj*, align 8
%current_45args55787 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55786)
store volatile %struct.ScmObj* %current_45args55787, %struct.ScmObj** %stackaddr$prim56556, align 8
%stackaddr$prim56557 = alloca %struct.ScmObj*, align 8
%anf_45bind50054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55787)
store volatile %struct.ScmObj* %anf_45bind50054, %struct.ScmObj** %stackaddr$prim56557, align 8
%ae52076 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56558 = alloca %struct.ScmObj*, align 8
%_95049955 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append49954, %struct.ScmObj* %ae52076, %struct.ScmObj* %anf_45bind50054)
store volatile %struct.ScmObj* %_95049955, %struct.ScmObj** %stackaddr$prim56558, align 8
%ae52079 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56559 = alloca %struct.ScmObj*, align 8
%_37append49953 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append49954, %struct.ScmObj* %ae52079)
store volatile %struct.ScmObj* %_37append49953, %struct.ScmObj** %stackaddr$prim56559, align 8
%stackaddr$makeclosure56560 = alloca %struct.ScmObj*, align 8
%fptrToInt56561 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52080 to i64
%ae52080 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56561)
store volatile %struct.ScmObj* %ae52080, %struct.ScmObj** %stackaddr$makeclosure56560, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52080, %struct.ScmObj* %_37foldl149866, i64 0)
%ae52081 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56562 = alloca %struct.ScmObj*, align 8
%fptrToInt56563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52082 to i64
%ae52082 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56563)
store volatile %struct.ScmObj* %ae52082, %struct.ScmObj** %stackaddr$makeclosure56562, align 8
%argslist55940$ae520800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56564 = alloca %struct.ScmObj*, align 8
%argslist55940$ae520801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52082, %struct.ScmObj* %argslist55940$ae520800)
store volatile %struct.ScmObj* %argslist55940$ae520801, %struct.ScmObj** %stackaddr$prim56564, align 8
%stackaddr$prim56565 = alloca %struct.ScmObj*, align 8
%argslist55940$ae520802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52081, %struct.ScmObj* %argslist55940$ae520801)
store volatile %struct.ScmObj* %argslist55940$ae520802, %struct.ScmObj** %stackaddr$prim56565, align 8
%clofunc56566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52080)
musttail call tailcc void %clofunc56566(%struct.ScmObj* %ae52080, %struct.ScmObj* %argslist55940$ae520802)
ret void
}

define tailcc void @proc_clo$ae52080(%struct.ScmObj* %env$ae52080,%struct.ScmObj* %current_45args55789) {
%stackaddr$env-ref56567 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52080, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56567
%stackaddr$prim56568 = alloca %struct.ScmObj*, align 8
%_95k50114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55789)
store volatile %struct.ScmObj* %_95k50114, %struct.ScmObj** %stackaddr$prim56568, align 8
%stackaddr$prim56569 = alloca %struct.ScmObj*, align 8
%current_45args55790 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55789)
store volatile %struct.ScmObj* %current_45args55790, %struct.ScmObj** %stackaddr$prim56569, align 8
%stackaddr$prim56570 = alloca %struct.ScmObj*, align 8
%_37list_6349946 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55790)
store volatile %struct.ScmObj* %_37list_6349946, %struct.ScmObj** %stackaddr$prim56570, align 8
%stackaddr$makeclosure56571 = alloca %struct.ScmObj*, align 8
%fptrToInt56572 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52496 to i64
%ae52496 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56572)
store volatile %struct.ScmObj* %ae52496, %struct.ScmObj** %stackaddr$makeclosure56571, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52496, %struct.ScmObj* %_37foldl149866, i64 0)
%ae52497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56573 = alloca %struct.ScmObj*, align 8
%fptrToInt56574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52498 to i64
%ae52498 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56574)
store volatile %struct.ScmObj* %ae52498, %struct.ScmObj** %stackaddr$makeclosure56573, align 8
%argslist55915$ae524960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56575 = alloca %struct.ScmObj*, align 8
%argslist55915$ae524961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52498, %struct.ScmObj* %argslist55915$ae524960)
store volatile %struct.ScmObj* %argslist55915$ae524961, %struct.ScmObj** %stackaddr$prim56575, align 8
%stackaddr$prim56576 = alloca %struct.ScmObj*, align 8
%argslist55915$ae524962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52497, %struct.ScmObj* %argslist55915$ae524961)
store volatile %struct.ScmObj* %argslist55915$ae524962, %struct.ScmObj** %stackaddr$prim56576, align 8
%clofunc56577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52496)
musttail call tailcc void %clofunc56577(%struct.ScmObj* %ae52496, %struct.ScmObj* %argslist55915$ae524962)
ret void
}

define tailcc void @proc_clo$ae52496(%struct.ScmObj* %env$ae52496,%struct.ScmObj* %current_45args55792) {
%stackaddr$env-ref56578 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52496, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56578
%stackaddr$prim56579 = alloca %struct.ScmObj*, align 8
%_95k50115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55792)
store volatile %struct.ScmObj* %_95k50115, %struct.ScmObj** %stackaddr$prim56579, align 8
%stackaddr$prim56580 = alloca %struct.ScmObj*, align 8
%current_45args55793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55792)
store volatile %struct.ScmObj* %current_45args55793, %struct.ScmObj** %stackaddr$prim56580, align 8
%stackaddr$prim56581 = alloca %struct.ScmObj*, align 8
%_37drop49937 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55793)
store volatile %struct.ScmObj* %_37drop49937, %struct.ScmObj** %stackaddr$prim56581, align 8
%stackaddr$makeclosure56582 = alloca %struct.ScmObj*, align 8
%fptrToInt56583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53032 to i64
%ae53032 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56583)
store volatile %struct.ScmObj* %ae53032, %struct.ScmObj** %stackaddr$makeclosure56582, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53032, %struct.ScmObj* %_37foldl149866, i64 0)
%ae53033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56584 = alloca %struct.ScmObj*, align 8
%fptrToInt56585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53034 to i64
%ae53034 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56585)
store volatile %struct.ScmObj* %ae53034, %struct.ScmObj** %stackaddr$makeclosure56584, align 8
%argslist55891$ae530320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56586 = alloca %struct.ScmObj*, align 8
%argslist55891$ae530321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53034, %struct.ScmObj* %argslist55891$ae530320)
store volatile %struct.ScmObj* %argslist55891$ae530321, %struct.ScmObj** %stackaddr$prim56586, align 8
%stackaddr$prim56587 = alloca %struct.ScmObj*, align 8
%argslist55891$ae530322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53033, %struct.ScmObj* %argslist55891$ae530321)
store volatile %struct.ScmObj* %argslist55891$ae530322, %struct.ScmObj** %stackaddr$prim56587, align 8
%clofunc56588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53032)
musttail call tailcc void %clofunc56588(%struct.ScmObj* %ae53032, %struct.ScmObj* %argslist55891$ae530322)
ret void
}

define tailcc void @proc_clo$ae53032(%struct.ScmObj* %env$ae53032,%struct.ScmObj* %current_45args55795) {
%stackaddr$env-ref56589 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53032, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56589
%stackaddr$prim56590 = alloca %struct.ScmObj*, align 8
%_95k50116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55795)
store volatile %struct.ScmObj* %_95k50116, %struct.ScmObj** %stackaddr$prim56590, align 8
%stackaddr$prim56591 = alloca %struct.ScmObj*, align 8
%current_45args55796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55795)
store volatile %struct.ScmObj* %current_45args55796, %struct.ScmObj** %stackaddr$prim56591, align 8
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%_37memv49930 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55796)
store volatile %struct.ScmObj* %_37memv49930, %struct.ScmObj** %stackaddr$prim56592, align 8
%stackaddr$makeclosure56593 = alloca %struct.ScmObj*, align 8
%fptrToInt56594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53436 to i64
%ae53436 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56594)
store volatile %struct.ScmObj* %ae53436, %struct.ScmObj** %stackaddr$makeclosure56593, align 8
%ae53437 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56595 = alloca %struct.ScmObj*, align 8
%fptrToInt56596 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53438 to i64
%ae53438 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56596)
store volatile %struct.ScmObj* %ae53438, %struct.ScmObj** %stackaddr$makeclosure56595, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53438, %struct.ScmObj* %_37foldl149866, i64 0)
%argslist55865$ae534360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56597 = alloca %struct.ScmObj*, align 8
%argslist55865$ae534361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53438, %struct.ScmObj* %argslist55865$ae534360)
store volatile %struct.ScmObj* %argslist55865$ae534361, %struct.ScmObj** %stackaddr$prim56597, align 8
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%argslist55865$ae534362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53437, %struct.ScmObj* %argslist55865$ae534361)
store volatile %struct.ScmObj* %argslist55865$ae534362, %struct.ScmObj** %stackaddr$prim56598, align 8
%clofunc56599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53436)
musttail call tailcc void %clofunc56599(%struct.ScmObj* %ae53436, %struct.ScmObj* %argslist55865$ae534362)
ret void
}

define tailcc void @proc_clo$ae53436(%struct.ScmObj* %env$ae53436,%struct.ScmObj* %current_45args55798) {
%stackaddr$prim56600 = alloca %struct.ScmObj*, align 8
%_95k50117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55798)
store volatile %struct.ScmObj* %_95k50117, %struct.ScmObj** %stackaddr$prim56600, align 8
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%current_45args55799 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55798)
store volatile %struct.ScmObj* %current_45args55799, %struct.ScmObj** %stackaddr$prim56601, align 8
%stackaddr$prim56602 = alloca %struct.ScmObj*, align 8
%_37_4749926 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55799)
store volatile %struct.ScmObj* %_37_4749926, %struct.ScmObj** %stackaddr$prim56602, align 8
%stackaddr$makeclosure56603 = alloca %struct.ScmObj*, align 8
%fptrToInt56604 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53534 to i64
%ae53534 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56604)
store volatile %struct.ScmObj* %ae53534, %struct.ScmObj** %stackaddr$makeclosure56603, align 8
%ae53535 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56605 = alloca %struct.ScmObj*, align 8
%fptrToInt56606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53536 to i64
%ae53536 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56606)
store volatile %struct.ScmObj* %ae53536, %struct.ScmObj** %stackaddr$makeclosure56605, align 8
%argslist55852$ae535340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56607 = alloca %struct.ScmObj*, align 8
%argslist55852$ae535341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53536, %struct.ScmObj* %argslist55852$ae535340)
store volatile %struct.ScmObj* %argslist55852$ae535341, %struct.ScmObj** %stackaddr$prim56607, align 8
%stackaddr$prim56608 = alloca %struct.ScmObj*, align 8
%argslist55852$ae535342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53535, %struct.ScmObj* %argslist55852$ae535341)
store volatile %struct.ScmObj* %argslist55852$ae535342, %struct.ScmObj** %stackaddr$prim56608, align 8
%clofunc56609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53534)
musttail call tailcc void %clofunc56609(%struct.ScmObj* %ae53534, %struct.ScmObj* %argslist55852$ae535342)
ret void
}

define tailcc void @proc_clo$ae53534(%struct.ScmObj* %env$ae53534,%struct.ScmObj* %current_45args55801) {
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%_95k50118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55801)
store volatile %struct.ScmObj* %_95k50118, %struct.ScmObj** %stackaddr$prim56610, align 8
%stackaddr$prim56611 = alloca %struct.ScmObj*, align 8
%current_45args55802 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55801)
store volatile %struct.ScmObj* %current_45args55802, %struct.ScmObj** %stackaddr$prim56611, align 8
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%_37first49924 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55802)
store volatile %struct.ScmObj* %_37first49924, %struct.ScmObj** %stackaddr$prim56612, align 8
%stackaddr$makeclosure56613 = alloca %struct.ScmObj*, align 8
%fptrToInt56614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53554 to i64
%ae53554 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56614)
store volatile %struct.ScmObj* %ae53554, %struct.ScmObj** %stackaddr$makeclosure56613, align 8
%ae53555 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56615 = alloca %struct.ScmObj*, align 8
%fptrToInt56616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53556 to i64
%ae53556 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56616)
store volatile %struct.ScmObj* %ae53556, %struct.ScmObj** %stackaddr$makeclosure56615, align 8
%argslist55847$ae535540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56617 = alloca %struct.ScmObj*, align 8
%argslist55847$ae535541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53556, %struct.ScmObj* %argslist55847$ae535540)
store volatile %struct.ScmObj* %argslist55847$ae535541, %struct.ScmObj** %stackaddr$prim56617, align 8
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%argslist55847$ae535542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53555, %struct.ScmObj* %argslist55847$ae535541)
store volatile %struct.ScmObj* %argslist55847$ae535542, %struct.ScmObj** %stackaddr$prim56618, align 8
%clofunc56619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53554)
musttail call tailcc void %clofunc56619(%struct.ScmObj* %ae53554, %struct.ScmObj* %argslist55847$ae535542)
ret void
}

define tailcc void @proc_clo$ae53554(%struct.ScmObj* %env$ae53554,%struct.ScmObj* %current_45args55804) {
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%_95k50119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55804)
store volatile %struct.ScmObj* %_95k50119, %struct.ScmObj** %stackaddr$prim56620, align 8
%stackaddr$prim56621 = alloca %struct.ScmObj*, align 8
%current_45args55805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55804)
store volatile %struct.ScmObj* %current_45args55805, %struct.ScmObj** %stackaddr$prim56621, align 8
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%_37second49922 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55805)
store volatile %struct.ScmObj* %_37second49922, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$makeclosure56623 = alloca %struct.ScmObj*, align 8
%fptrToInt56624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53576 to i64
%ae53576 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56624)
store volatile %struct.ScmObj* %ae53576, %struct.ScmObj** %stackaddr$makeclosure56623, align 8
%ae53577 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56625 = alloca %struct.ScmObj*, align 8
%fptrToInt56626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53578 to i64
%ae53578 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56626)
store volatile %struct.ScmObj* %ae53578, %struct.ScmObj** %stackaddr$makeclosure56625, align 8
%argslist55842$ae535760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56627 = alloca %struct.ScmObj*, align 8
%argslist55842$ae535761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53578, %struct.ScmObj* %argslist55842$ae535760)
store volatile %struct.ScmObj* %argslist55842$ae535761, %struct.ScmObj** %stackaddr$prim56627, align 8
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%argslist55842$ae535762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53577, %struct.ScmObj* %argslist55842$ae535761)
store volatile %struct.ScmObj* %argslist55842$ae535762, %struct.ScmObj** %stackaddr$prim56628, align 8
%clofunc56629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53576)
musttail call tailcc void %clofunc56629(%struct.ScmObj* %ae53576, %struct.ScmObj* %argslist55842$ae535762)
ret void
}

define tailcc void @proc_clo$ae53576(%struct.ScmObj* %env$ae53576,%struct.ScmObj* %current_45args55807) {
%stackaddr$prim56630 = alloca %struct.ScmObj*, align 8
%_95k50120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55807)
store volatile %struct.ScmObj* %_95k50120, %struct.ScmObj** %stackaddr$prim56630, align 8
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%current_45args55808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55807)
store volatile %struct.ScmObj* %current_45args55808, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%_37third49920 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55808)
store volatile %struct.ScmObj* %_37third49920, %struct.ScmObj** %stackaddr$prim56632, align 8
%stackaddr$makeclosure56633 = alloca %struct.ScmObj*, align 8
%fptrToInt56634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53600 to i64
%ae53600 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56634)
store volatile %struct.ScmObj* %ae53600, %struct.ScmObj** %stackaddr$makeclosure56633, align 8
%ae53601 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56635 = alloca %struct.ScmObj*, align 8
%fptrToInt56636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53602 to i64
%ae53602 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56636)
store volatile %struct.ScmObj* %ae53602, %struct.ScmObj** %stackaddr$makeclosure56635, align 8
%argslist55837$ae536000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%argslist55837$ae536001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53602, %struct.ScmObj* %argslist55837$ae536000)
store volatile %struct.ScmObj* %argslist55837$ae536001, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$prim56638 = alloca %struct.ScmObj*, align 8
%argslist55837$ae536002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53601, %struct.ScmObj* %argslist55837$ae536001)
store volatile %struct.ScmObj* %argslist55837$ae536002, %struct.ScmObj** %stackaddr$prim56638, align 8
%clofunc56639 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53600)
musttail call tailcc void %clofunc56639(%struct.ScmObj* %ae53600, %struct.ScmObj* %argslist55837$ae536002)
ret void
}

define tailcc void @proc_clo$ae53600(%struct.ScmObj* %env$ae53600,%struct.ScmObj* %current_45args55810) {
%stackaddr$prim56640 = alloca %struct.ScmObj*, align 8
%_95k50121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55810)
store volatile %struct.ScmObj* %_95k50121, %struct.ScmObj** %stackaddr$prim56640, align 8
%stackaddr$prim56641 = alloca %struct.ScmObj*, align 8
%current_45args55811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55810)
store volatile %struct.ScmObj* %current_45args55811, %struct.ScmObj** %stackaddr$prim56641, align 8
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%_37fourth49918 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55811)
store volatile %struct.ScmObj* %_37fourth49918, %struct.ScmObj** %stackaddr$prim56642, align 8
%ae53626 = call %struct.ScmObj* @const_init_false()
%truthy$cmp56643 = call i64 @is_truthy_value(%struct.ScmObj* %ae53626)
%cmp$cmp56643 = icmp eq i64 %truthy$cmp56643, 1
br i1 %cmp$cmp56643, label %truebranch$cmp56643, label %falsebranch$cmp56643
truebranch$cmp56643:
%stackaddr$makeclosure56644 = alloca %struct.ScmObj*, align 8
%fptrToInt56645 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53627 to i64
%ae53627 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56645)
store volatile %struct.ScmObj* %ae53627, %struct.ScmObj** %stackaddr$makeclosure56644, align 8
%ae53628 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53629 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55817$ae536270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%argslist55817$ae536271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53629, %struct.ScmObj* %argslist55817$ae536270)
store volatile %struct.ScmObj* %argslist55817$ae536271, %struct.ScmObj** %stackaddr$prim56646, align 8
%stackaddr$prim56647 = alloca %struct.ScmObj*, align 8
%argslist55817$ae536272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53628, %struct.ScmObj* %argslist55817$ae536271)
store volatile %struct.ScmObj* %argslist55817$ae536272, %struct.ScmObj** %stackaddr$prim56647, align 8
%clofunc56648 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53627)
musttail call tailcc void %clofunc56648(%struct.ScmObj* %ae53627, %struct.ScmObj* %argslist55817$ae536272)
ret void
falsebranch$cmp56643:
%ae53642 = call %struct.ScmObj* @const_init_false()
%truthy$cmp56649 = call i64 @is_truthy_value(%struct.ScmObj* %ae53642)
%cmp$cmp56649 = icmp eq i64 %truthy$cmp56649, 1
br i1 %cmp$cmp56649, label %truebranch$cmp56649, label %falsebranch$cmp56649
truebranch$cmp56649:
%stackaddr$makeclosure56650 = alloca %struct.ScmObj*, align 8
%fptrToInt56651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53643 to i64
%ae53643 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56651)
store volatile %struct.ScmObj* %ae53643, %struct.ScmObj** %stackaddr$makeclosure56650, align 8
%ae53644 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53645 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist55822$ae536430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%argslist55822$ae536431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53645, %struct.ScmObj* %argslist55822$ae536430)
store volatile %struct.ScmObj* %argslist55822$ae536431, %struct.ScmObj** %stackaddr$prim56652, align 8
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%argslist55822$ae536432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53644, %struct.ScmObj* %argslist55822$ae536431)
store volatile %struct.ScmObj* %argslist55822$ae536432, %struct.ScmObj** %stackaddr$prim56653, align 8
%clofunc56654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53643)
musttail call tailcc void %clofunc56654(%struct.ScmObj* %ae53643, %struct.ScmObj* %argslist55822$ae536432)
ret void
falsebranch$cmp56649:
%ae53658 = call %struct.ScmObj* @const_init_true()
%truthy$cmp56655 = call i64 @is_truthy_value(%struct.ScmObj* %ae53658)
%cmp$cmp56655 = icmp eq i64 %truthy$cmp56655, 1
br i1 %cmp$cmp56655, label %truebranch$cmp56655, label %falsebranch$cmp56655
truebranch$cmp56655:
%stackaddr$makeclosure56656 = alloca %struct.ScmObj*, align 8
%fptrToInt56657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53659 to i64
%ae53659 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56657)
store volatile %struct.ScmObj* %ae53659, %struct.ScmObj** %stackaddr$makeclosure56656, align 8
%ae53660 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53661 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist55827$ae536590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%argslist55827$ae536591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53661, %struct.ScmObj* %argslist55827$ae536590)
store volatile %struct.ScmObj* %argslist55827$ae536591, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%argslist55827$ae536592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53660, %struct.ScmObj* %argslist55827$ae536591)
store volatile %struct.ScmObj* %argslist55827$ae536592, %struct.ScmObj** %stackaddr$prim56659, align 8
%clofunc56660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53659)
musttail call tailcc void %clofunc56660(%struct.ScmObj* %ae53659, %struct.ScmObj* %argslist55827$ae536592)
ret void
falsebranch$cmp56655:
%stackaddr$makeclosure56661 = alloca %struct.ScmObj*, align 8
%fptrToInt56662 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53674 to i64
%ae53674 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56662)
store volatile %struct.ScmObj* %ae53674, %struct.ScmObj** %stackaddr$makeclosure56661, align 8
%ae53675 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53676 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist55832$ae536740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56663 = alloca %struct.ScmObj*, align 8
%argslist55832$ae536741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53676, %struct.ScmObj* %argslist55832$ae536740)
store volatile %struct.ScmObj* %argslist55832$ae536741, %struct.ScmObj** %stackaddr$prim56663, align 8
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%argslist55832$ae536742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53675, %struct.ScmObj* %argslist55832$ae536741)
store volatile %struct.ScmObj* %argslist55832$ae536742, %struct.ScmObj** %stackaddr$prim56664, align 8
%clofunc56665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53674)
musttail call tailcc void %clofunc56665(%struct.ScmObj* %ae53674, %struct.ScmObj* %argslist55832$ae536742)
ret void
}

define tailcc void @proc_clo$ae53627(%struct.ScmObj* %env$ae53627,%struct.ScmObj* %current_45args55813) {
%stackaddr$prim56666 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55813)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56666, align 8
%stackaddr$prim56667 = alloca %struct.ScmObj*, align 8
%current_45args55814 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55813)
store volatile %struct.ScmObj* %current_45args55814, %struct.ScmObj** %stackaddr$prim56667, align 8
%stackaddr$prim56668 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55814)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56668, align 8
%stackaddr$prim56669 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56669, align 8
%argslist55816$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%argslist55816$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55816$k0)
store volatile %struct.ScmObj* %argslist55816$k1, %struct.ScmObj** %stackaddr$prim56670, align 8
%clofunc56671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56671(%struct.ScmObj* %k, %struct.ScmObj* %argslist55816$k1)
ret void
}

define tailcc void @proc_clo$ae53643(%struct.ScmObj* %env$ae53643,%struct.ScmObj* %current_45args55818) {
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55818)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$prim56673 = alloca %struct.ScmObj*, align 8
%current_45args55819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55818)
store volatile %struct.ScmObj* %current_45args55819, %struct.ScmObj** %stackaddr$prim56673, align 8
%stackaddr$prim56674 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55819)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56674, align 8
%stackaddr$prim56675 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56675, align 8
%argslist55821$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56676 = alloca %struct.ScmObj*, align 8
%argslist55821$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55821$k0)
store volatile %struct.ScmObj* %argslist55821$k1, %struct.ScmObj** %stackaddr$prim56676, align 8
%clofunc56677 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56677(%struct.ScmObj* %k, %struct.ScmObj* %argslist55821$k1)
ret void
}

define tailcc void @proc_clo$ae53659(%struct.ScmObj* %env$ae53659,%struct.ScmObj* %current_45args55823) {
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55823)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56678, align 8
%stackaddr$prim56679 = alloca %struct.ScmObj*, align 8
%current_45args55824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55823)
store volatile %struct.ScmObj* %current_45args55824, %struct.ScmObj** %stackaddr$prim56679, align 8
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55824)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56680, align 8
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56681, align 8
%argslist55826$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%argslist55826$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55826$k0)
store volatile %struct.ScmObj* %argslist55826$k1, %struct.ScmObj** %stackaddr$prim56682, align 8
%clofunc56683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56683(%struct.ScmObj* %k, %struct.ScmObj* %argslist55826$k1)
ret void
}

define tailcc void @proc_clo$ae53674(%struct.ScmObj* %env$ae53674,%struct.ScmObj* %current_45args55828) {
%stackaddr$prim56684 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55828)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56684, align 8
%stackaddr$prim56685 = alloca %struct.ScmObj*, align 8
%current_45args55829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55828)
store volatile %struct.ScmObj* %current_45args55829, %struct.ScmObj** %stackaddr$prim56685, align 8
%stackaddr$prim56686 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55829)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56686, align 8
%stackaddr$prim56687 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56687, align 8
%argslist55831$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56688 = alloca %struct.ScmObj*, align 8
%argslist55831$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55831$k0)
store volatile %struct.ScmObj* %argslist55831$k1, %struct.ScmObj** %stackaddr$prim56688, align 8
%clofunc56689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56689(%struct.ScmObj* %k, %struct.ScmObj* %argslist55831$k1)
ret void
}

define tailcc void @proc_clo$ae53602(%struct.ScmObj* %env$ae53602,%struct.ScmObj* %current_45args55833) {
%stackaddr$prim56690 = alloca %struct.ScmObj*, align 8
%k50122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55833)
store volatile %struct.ScmObj* %k50122, %struct.ScmObj** %stackaddr$prim56690, align 8
%stackaddr$prim56691 = alloca %struct.ScmObj*, align 8
%current_45args55834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55833)
store volatile %struct.ScmObj* %current_45args55834, %struct.ScmObj** %stackaddr$prim56691, align 8
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%x49919 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55834)
store volatile %struct.ScmObj* %x49919, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%anf_45bind50087 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x49919)
store volatile %struct.ScmObj* %anf_45bind50087, %struct.ScmObj** %stackaddr$prim56693, align 8
%stackaddr$prim56694 = alloca %struct.ScmObj*, align 8
%anf_45bind50088 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50087)
store volatile %struct.ScmObj* %anf_45bind50088, %struct.ScmObj** %stackaddr$prim56694, align 8
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%anf_45bind50089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50088)
store volatile %struct.ScmObj* %anf_45bind50089, %struct.ScmObj** %stackaddr$prim56695, align 8
%stackaddr$prim56696 = alloca %struct.ScmObj*, align 8
%cpsprim50123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50089)
store volatile %struct.ScmObj* %cpsprim50123, %struct.ScmObj** %stackaddr$prim56696, align 8
%ae53608 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55836$k501220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56697 = alloca %struct.ScmObj*, align 8
%argslist55836$k501221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50123, %struct.ScmObj* %argslist55836$k501220)
store volatile %struct.ScmObj* %argslist55836$k501221, %struct.ScmObj** %stackaddr$prim56697, align 8
%stackaddr$prim56698 = alloca %struct.ScmObj*, align 8
%argslist55836$k501222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53608, %struct.ScmObj* %argslist55836$k501221)
store volatile %struct.ScmObj* %argslist55836$k501222, %struct.ScmObj** %stackaddr$prim56698, align 8
%clofunc56699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50122)
musttail call tailcc void %clofunc56699(%struct.ScmObj* %k50122, %struct.ScmObj* %argslist55836$k501222)
ret void
}

define tailcc void @proc_clo$ae53578(%struct.ScmObj* %env$ae53578,%struct.ScmObj* %current_45args55838) {
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%k50124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55838)
store volatile %struct.ScmObj* %k50124, %struct.ScmObj** %stackaddr$prim56700, align 8
%stackaddr$prim56701 = alloca %struct.ScmObj*, align 8
%current_45args55839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55838)
store volatile %struct.ScmObj* %current_45args55839, %struct.ScmObj** %stackaddr$prim56701, align 8
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%x49921 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55839)
store volatile %struct.ScmObj* %x49921, %struct.ScmObj** %stackaddr$prim56702, align 8
%stackaddr$prim56703 = alloca %struct.ScmObj*, align 8
%anf_45bind50085 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x49921)
store volatile %struct.ScmObj* %anf_45bind50085, %struct.ScmObj** %stackaddr$prim56703, align 8
%stackaddr$prim56704 = alloca %struct.ScmObj*, align 8
%anf_45bind50086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50085)
store volatile %struct.ScmObj* %anf_45bind50086, %struct.ScmObj** %stackaddr$prim56704, align 8
%stackaddr$prim56705 = alloca %struct.ScmObj*, align 8
%cpsprim50125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50086)
store volatile %struct.ScmObj* %cpsprim50125, %struct.ScmObj** %stackaddr$prim56705, align 8
%ae53583 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55841$k501240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56706 = alloca %struct.ScmObj*, align 8
%argslist55841$k501241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50125, %struct.ScmObj* %argslist55841$k501240)
store volatile %struct.ScmObj* %argslist55841$k501241, %struct.ScmObj** %stackaddr$prim56706, align 8
%stackaddr$prim56707 = alloca %struct.ScmObj*, align 8
%argslist55841$k501242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53583, %struct.ScmObj* %argslist55841$k501241)
store volatile %struct.ScmObj* %argslist55841$k501242, %struct.ScmObj** %stackaddr$prim56707, align 8
%clofunc56708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50124)
musttail call tailcc void %clofunc56708(%struct.ScmObj* %k50124, %struct.ScmObj* %argslist55841$k501242)
ret void
}

define tailcc void @proc_clo$ae53556(%struct.ScmObj* %env$ae53556,%struct.ScmObj* %current_45args55843) {
%stackaddr$prim56709 = alloca %struct.ScmObj*, align 8
%k50126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55843)
store volatile %struct.ScmObj* %k50126, %struct.ScmObj** %stackaddr$prim56709, align 8
%stackaddr$prim56710 = alloca %struct.ScmObj*, align 8
%current_45args55844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55843)
store volatile %struct.ScmObj* %current_45args55844, %struct.ScmObj** %stackaddr$prim56710, align 8
%stackaddr$prim56711 = alloca %struct.ScmObj*, align 8
%x49923 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55844)
store volatile %struct.ScmObj* %x49923, %struct.ScmObj** %stackaddr$prim56711, align 8
%stackaddr$prim56712 = alloca %struct.ScmObj*, align 8
%anf_45bind50084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x49923)
store volatile %struct.ScmObj* %anf_45bind50084, %struct.ScmObj** %stackaddr$prim56712, align 8
%stackaddr$prim56713 = alloca %struct.ScmObj*, align 8
%cpsprim50127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50084)
store volatile %struct.ScmObj* %cpsprim50127, %struct.ScmObj** %stackaddr$prim56713, align 8
%ae53560 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55846$k501260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56714 = alloca %struct.ScmObj*, align 8
%argslist55846$k501261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50127, %struct.ScmObj* %argslist55846$k501260)
store volatile %struct.ScmObj* %argslist55846$k501261, %struct.ScmObj** %stackaddr$prim56714, align 8
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%argslist55846$k501262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53560, %struct.ScmObj* %argslist55846$k501261)
store volatile %struct.ScmObj* %argslist55846$k501262, %struct.ScmObj** %stackaddr$prim56715, align 8
%clofunc56716 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50126)
musttail call tailcc void %clofunc56716(%struct.ScmObj* %k50126, %struct.ScmObj* %argslist55846$k501262)
ret void
}

define tailcc void @proc_clo$ae53536(%struct.ScmObj* %env$ae53536,%struct.ScmObj* %current_45args55848) {
%stackaddr$prim56717 = alloca %struct.ScmObj*, align 8
%k50128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55848)
store volatile %struct.ScmObj* %k50128, %struct.ScmObj** %stackaddr$prim56717, align 8
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%current_45args55849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55848)
store volatile %struct.ScmObj* %current_45args55849, %struct.ScmObj** %stackaddr$prim56718, align 8
%stackaddr$prim56719 = alloca %struct.ScmObj*, align 8
%x49925 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55849)
store volatile %struct.ScmObj* %x49925, %struct.ScmObj** %stackaddr$prim56719, align 8
%stackaddr$prim56720 = alloca %struct.ScmObj*, align 8
%cpsprim50129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x49925)
store volatile %struct.ScmObj* %cpsprim50129, %struct.ScmObj** %stackaddr$prim56720, align 8
%ae53539 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55851$k501280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56721 = alloca %struct.ScmObj*, align 8
%argslist55851$k501281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50129, %struct.ScmObj* %argslist55851$k501280)
store volatile %struct.ScmObj* %argslist55851$k501281, %struct.ScmObj** %stackaddr$prim56721, align 8
%stackaddr$prim56722 = alloca %struct.ScmObj*, align 8
%argslist55851$k501282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53539, %struct.ScmObj* %argslist55851$k501281)
store volatile %struct.ScmObj* %argslist55851$k501282, %struct.ScmObj** %stackaddr$prim56722, align 8
%clofunc56723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50128)
musttail call tailcc void %clofunc56723(%struct.ScmObj* %k50128, %struct.ScmObj* %argslist55851$k501282)
ret void
}

define tailcc void @proc_clo$ae53438(%struct.ScmObj* %env$ae53438,%struct.ScmObj* %args4992750130) {
%stackaddr$env-ref56724 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53438, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56724
%stackaddr$prim56725 = alloca %struct.ScmObj*, align 8
%k50131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4992750130)
store volatile %struct.ScmObj* %k50131, %struct.ScmObj** %stackaddr$prim56725, align 8
%stackaddr$prim56726 = alloca %struct.ScmObj*, align 8
%args49927 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4992750130)
store volatile %struct.ScmObj* %args49927, %struct.ScmObj** %stackaddr$prim56726, align 8
%stackaddr$prim56727 = alloca %struct.ScmObj*, align 8
%anf_45bind50078 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args49927)
store volatile %struct.ScmObj* %anf_45bind50078, %struct.ScmObj** %stackaddr$prim56727, align 8
%truthy$cmp56728 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50078)
%cmp$cmp56728 = icmp eq i64 %truthy$cmp56728, 1
br i1 %cmp$cmp56728, label %truebranch$cmp56728, label %falsebranch$cmp56728
truebranch$cmp56728:
%ae53444 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53445 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55853$k501310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56729 = alloca %struct.ScmObj*, align 8
%argslist55853$k501311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53445, %struct.ScmObj* %argslist55853$k501310)
store volatile %struct.ScmObj* %argslist55853$k501311, %struct.ScmObj** %stackaddr$prim56729, align 8
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%argslist55853$k501312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53444, %struct.ScmObj* %argslist55853$k501311)
store volatile %struct.ScmObj* %argslist55853$k501312, %struct.ScmObj** %stackaddr$prim56730, align 8
%clofunc56731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50131)
musttail call tailcc void %clofunc56731(%struct.ScmObj* %k50131, %struct.ScmObj* %argslist55853$k501312)
ret void
falsebranch$cmp56728:
%stackaddr$prim56732 = alloca %struct.ScmObj*, align 8
%anf_45bind50079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49927)
store volatile %struct.ScmObj* %anf_45bind50079, %struct.ScmObj** %stackaddr$prim56732, align 8
%stackaddr$prim56733 = alloca %struct.ScmObj*, align 8
%anf_45bind50080 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind50079)
store volatile %struct.ScmObj* %anf_45bind50080, %struct.ScmObj** %stackaddr$prim56733, align 8
%truthy$cmp56734 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50080)
%cmp$cmp56734 = icmp eq i64 %truthy$cmp56734, 1
br i1 %cmp$cmp56734, label %truebranch$cmp56734, label %falsebranch$cmp56734
truebranch$cmp56734:
%stackaddr$prim56735 = alloca %struct.ScmObj*, align 8
%cpsprim50132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args49927)
store volatile %struct.ScmObj* %cpsprim50132, %struct.ScmObj** %stackaddr$prim56735, align 8
%ae53457 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55854$k501310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56736 = alloca %struct.ScmObj*, align 8
%argslist55854$k501311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50132, %struct.ScmObj* %argslist55854$k501310)
store volatile %struct.ScmObj* %argslist55854$k501311, %struct.ScmObj** %stackaddr$prim56736, align 8
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%argslist55854$k501312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53457, %struct.ScmObj* %argslist55854$k501311)
store volatile %struct.ScmObj* %argslist55854$k501312, %struct.ScmObj** %stackaddr$prim56737, align 8
%clofunc56738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50131)
musttail call tailcc void %clofunc56738(%struct.ScmObj* %k50131, %struct.ScmObj* %argslist55854$k501312)
ret void
falsebranch$cmp56734:
%stackaddr$makeclosure56739 = alloca %struct.ScmObj*, align 8
%fptrToInt56740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53462 to i64
%ae53462 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56740)
store volatile %struct.ScmObj* %ae53462, %struct.ScmObj** %stackaddr$makeclosure56739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53462, %struct.ScmObj* %k50131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53462, %struct.ScmObj* %_37foldl149866, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53462, %struct.ScmObj* %args49927, i64 2)
%ae53463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56741 = alloca %struct.ScmObj*, align 8
%fptrToInt56742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53464 to i64
%ae53464 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56742)
store volatile %struct.ScmObj* %ae53464, %struct.ScmObj** %stackaddr$makeclosure56741, align 8
%argslist55864$ae534620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%argslist55864$ae534621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53464, %struct.ScmObj* %argslist55864$ae534620)
store volatile %struct.ScmObj* %argslist55864$ae534621, %struct.ScmObj** %stackaddr$prim56743, align 8
%stackaddr$prim56744 = alloca %struct.ScmObj*, align 8
%argslist55864$ae534622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53463, %struct.ScmObj* %argslist55864$ae534621)
store volatile %struct.ScmObj* %argslist55864$ae534622, %struct.ScmObj** %stackaddr$prim56744, align 8
%clofunc56745 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53462)
musttail call tailcc void %clofunc56745(%struct.ScmObj* %ae53462, %struct.ScmObj* %argslist55864$ae534622)
ret void
}

define tailcc void @proc_clo$ae53462(%struct.ScmObj* %env$ae53462,%struct.ScmObj* %current_45args55855) {
%stackaddr$env-ref56746 = alloca %struct.ScmObj*, align 8
%k50131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53462, i64 0)
store %struct.ScmObj* %k50131, %struct.ScmObj** %stackaddr$env-ref56746
%stackaddr$env-ref56747 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53462, i64 1)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref56747
%stackaddr$env-ref56748 = alloca %struct.ScmObj*, align 8
%args49927 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53462, i64 2)
store %struct.ScmObj* %args49927, %struct.ScmObj** %stackaddr$env-ref56748
%stackaddr$prim56749 = alloca %struct.ScmObj*, align 8
%_95k50133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55855)
store volatile %struct.ScmObj* %_95k50133, %struct.ScmObj** %stackaddr$prim56749, align 8
%stackaddr$prim56750 = alloca %struct.ScmObj*, align 8
%current_45args55856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55855)
store volatile %struct.ScmObj* %current_45args55856, %struct.ScmObj** %stackaddr$prim56750, align 8
%stackaddr$prim56751 = alloca %struct.ScmObj*, align 8
%anf_45bind50081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55856)
store volatile %struct.ScmObj* %anf_45bind50081, %struct.ScmObj** %stackaddr$prim56751, align 8
%stackaddr$prim56752 = alloca %struct.ScmObj*, align 8
%anf_45bind50082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args49927)
store volatile %struct.ScmObj* %anf_45bind50082, %struct.ScmObj** %stackaddr$prim56752, align 8
%stackaddr$prim56753 = alloca %struct.ScmObj*, align 8
%anf_45bind50083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49927)
store volatile %struct.ScmObj* %anf_45bind50083, %struct.ScmObj** %stackaddr$prim56753, align 8
%argslist55858$_37foldl1498660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%argslist55858$_37foldl1498661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50083, %struct.ScmObj* %argslist55858$_37foldl1498660)
store volatile %struct.ScmObj* %argslist55858$_37foldl1498661, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%argslist55858$_37foldl1498662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50082, %struct.ScmObj* %argslist55858$_37foldl1498661)
store volatile %struct.ScmObj* %argslist55858$_37foldl1498662, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%argslist55858$_37foldl1498663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50081, %struct.ScmObj* %argslist55858$_37foldl1498662)
store volatile %struct.ScmObj* %argslist55858$_37foldl1498663, %struct.ScmObj** %stackaddr$prim56756, align 8
%stackaddr$prim56757 = alloca %struct.ScmObj*, align 8
%argslist55858$_37foldl1498664 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50131, %struct.ScmObj* %argslist55858$_37foldl1498663)
store volatile %struct.ScmObj* %argslist55858$_37foldl1498664, %struct.ScmObj** %stackaddr$prim56757, align 8
%clofunc56758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl149866)
musttail call tailcc void %clofunc56758(%struct.ScmObj* %_37foldl149866, %struct.ScmObj* %argslist55858$_37foldl1498664)
ret void
}

define tailcc void @proc_clo$ae53464(%struct.ScmObj* %env$ae53464,%struct.ScmObj* %current_45args55859) {
%stackaddr$prim56759 = alloca %struct.ScmObj*, align 8
%k50134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55859)
store volatile %struct.ScmObj* %k50134, %struct.ScmObj** %stackaddr$prim56759, align 8
%stackaddr$prim56760 = alloca %struct.ScmObj*, align 8
%current_45args55860 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55859)
store volatile %struct.ScmObj* %current_45args55860, %struct.ScmObj** %stackaddr$prim56760, align 8
%stackaddr$prim56761 = alloca %struct.ScmObj*, align 8
%n49929 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55860)
store volatile %struct.ScmObj* %n49929, %struct.ScmObj** %stackaddr$prim56761, align 8
%stackaddr$prim56762 = alloca %struct.ScmObj*, align 8
%current_45args55861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55860)
store volatile %struct.ScmObj* %current_45args55861, %struct.ScmObj** %stackaddr$prim56762, align 8
%stackaddr$prim56763 = alloca %struct.ScmObj*, align 8
%v49928 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55861)
store volatile %struct.ScmObj* %v49928, %struct.ScmObj** %stackaddr$prim56763, align 8
%stackaddr$prim56764 = alloca %struct.ScmObj*, align 8
%cpsprim50135 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v49928, %struct.ScmObj* %n49929)
store volatile %struct.ScmObj* %cpsprim50135, %struct.ScmObj** %stackaddr$prim56764, align 8
%ae53468 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55863$k501340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56765 = alloca %struct.ScmObj*, align 8
%argslist55863$k501341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50135, %struct.ScmObj* %argslist55863$k501340)
store volatile %struct.ScmObj* %argslist55863$k501341, %struct.ScmObj** %stackaddr$prim56765, align 8
%stackaddr$prim56766 = alloca %struct.ScmObj*, align 8
%argslist55863$k501342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53468, %struct.ScmObj* %argslist55863$k501341)
store volatile %struct.ScmObj* %argslist55863$k501342, %struct.ScmObj** %stackaddr$prim56766, align 8
%clofunc56767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50134)
musttail call tailcc void %clofunc56767(%struct.ScmObj* %k50134, %struct.ScmObj* %argslist55863$k501342)
ret void
}

define tailcc void @proc_clo$ae53034(%struct.ScmObj* %env$ae53034,%struct.ScmObj* %current_45args55866) {
%stackaddr$prim56768 = alloca %struct.ScmObj*, align 8
%k50136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55866)
store volatile %struct.ScmObj* %k50136, %struct.ScmObj** %stackaddr$prim56768, align 8
%stackaddr$prim56769 = alloca %struct.ScmObj*, align 8
%current_45args55867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55866)
store volatile %struct.ScmObj* %current_45args55867, %struct.ScmObj** %stackaddr$prim56769, align 8
%stackaddr$prim56770 = alloca %struct.ScmObj*, align 8
%v49932 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55867)
store volatile %struct.ScmObj* %v49932, %struct.ScmObj** %stackaddr$prim56770, align 8
%stackaddr$prim56771 = alloca %struct.ScmObj*, align 8
%current_45args55868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55867)
store volatile %struct.ScmObj* %current_45args55868, %struct.ScmObj** %stackaddr$prim56771, align 8
%stackaddr$prim56772 = alloca %struct.ScmObj*, align 8
%lst49931 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55868)
store volatile %struct.ScmObj* %lst49931, %struct.ScmObj** %stackaddr$prim56772, align 8
%ae53035 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%lst49933 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae53035, %struct.ScmObj* %lst49931)
store volatile %struct.ScmObj* %lst49933, %struct.ScmObj** %stackaddr$prim56773, align 8
%stackaddr$makeclosure56774 = alloca %struct.ScmObj*, align 8
%fptrToInt56775 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53037 to i64
%ae53037 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56775)
store volatile %struct.ScmObj* %ae53037, %struct.ScmObj** %stackaddr$makeclosure56774, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53037, %struct.ScmObj* %lst49933, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53037, %struct.ScmObj* %v49932, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53037, %struct.ScmObj* %k50136, i64 2)
%ae53038 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56776 = alloca %struct.ScmObj*, align 8
%fptrToInt56777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53039 to i64
%ae53039 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56777)
store volatile %struct.ScmObj* %ae53039, %struct.ScmObj** %stackaddr$makeclosure56776, align 8
%argslist55890$ae530370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%argslist55890$ae530371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53039, %struct.ScmObj* %argslist55890$ae530370)
store volatile %struct.ScmObj* %argslist55890$ae530371, %struct.ScmObj** %stackaddr$prim56778, align 8
%stackaddr$prim56779 = alloca %struct.ScmObj*, align 8
%argslist55890$ae530372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53038, %struct.ScmObj* %argslist55890$ae530371)
store volatile %struct.ScmObj* %argslist55890$ae530372, %struct.ScmObj** %stackaddr$prim56779, align 8
%clofunc56780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53037)
musttail call tailcc void %clofunc56780(%struct.ScmObj* %ae53037, %struct.ScmObj* %argslist55890$ae530372)
ret void
}

define tailcc void @proc_clo$ae53037(%struct.ScmObj* %env$ae53037,%struct.ScmObj* %current_45args55870) {
%stackaddr$env-ref56781 = alloca %struct.ScmObj*, align 8
%lst49933 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53037, i64 0)
store %struct.ScmObj* %lst49933, %struct.ScmObj** %stackaddr$env-ref56781
%stackaddr$env-ref56782 = alloca %struct.ScmObj*, align 8
%v49932 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53037, i64 1)
store %struct.ScmObj* %v49932, %struct.ScmObj** %stackaddr$env-ref56782
%stackaddr$env-ref56783 = alloca %struct.ScmObj*, align 8
%k50136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53037, i64 2)
store %struct.ScmObj* %k50136, %struct.ScmObj** %stackaddr$env-ref56783
%stackaddr$prim56784 = alloca %struct.ScmObj*, align 8
%_95k50137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55870)
store volatile %struct.ScmObj* %_95k50137, %struct.ScmObj** %stackaddr$prim56784, align 8
%stackaddr$prim56785 = alloca %struct.ScmObj*, align 8
%current_45args55871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55870)
store volatile %struct.ScmObj* %current_45args55871, %struct.ScmObj** %stackaddr$prim56785, align 8
%stackaddr$prim56786 = alloca %struct.ScmObj*, align 8
%anf_45bind50070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55871)
store volatile %struct.ScmObj* %anf_45bind50070, %struct.ScmObj** %stackaddr$prim56786, align 8
%stackaddr$makeclosure56787 = alloca %struct.ScmObj*, align 8
%fptrToInt56788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53053 to i64
%ae53053 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56788)
store volatile %struct.ScmObj* %ae53053, %struct.ScmObj** %stackaddr$makeclosure56787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53053, %struct.ScmObj* %lst49933, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53053, %struct.ScmObj* %v49932, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53053, %struct.ScmObj* %k50136, i64 2)
%stackaddr$makeclosure56789 = alloca %struct.ScmObj*, align 8
%fptrToInt56790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53054 to i64
%ae53054 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56790)
store volatile %struct.ScmObj* %ae53054, %struct.ScmObj** %stackaddr$makeclosure56789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53054, %struct.ScmObj* %lst49933, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53054, %struct.ScmObj* %v49932, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53054, %struct.ScmObj* %k50136, i64 2)
%argslist55885$anf_45bind500700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56791 = alloca %struct.ScmObj*, align 8
%argslist55885$anf_45bind500701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53054, %struct.ScmObj* %argslist55885$anf_45bind500700)
store volatile %struct.ScmObj* %argslist55885$anf_45bind500701, %struct.ScmObj** %stackaddr$prim56791, align 8
%stackaddr$prim56792 = alloca %struct.ScmObj*, align 8
%argslist55885$anf_45bind500702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53053, %struct.ScmObj* %argslist55885$anf_45bind500701)
store volatile %struct.ScmObj* %argslist55885$anf_45bind500702, %struct.ScmObj** %stackaddr$prim56792, align 8
%clofunc56793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50070)
musttail call tailcc void %clofunc56793(%struct.ScmObj* %anf_45bind50070, %struct.ScmObj* %argslist55885$anf_45bind500702)
ret void
}

define tailcc void @proc_clo$ae53053(%struct.ScmObj* %env$ae53053,%struct.ScmObj* %current_45args55873) {
%stackaddr$env-ref56794 = alloca %struct.ScmObj*, align 8
%lst49933 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53053, i64 0)
store %struct.ScmObj* %lst49933, %struct.ScmObj** %stackaddr$env-ref56794
%stackaddr$env-ref56795 = alloca %struct.ScmObj*, align 8
%v49932 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53053, i64 1)
store %struct.ScmObj* %v49932, %struct.ScmObj** %stackaddr$env-ref56795
%stackaddr$env-ref56796 = alloca %struct.ScmObj*, align 8
%k50136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53053, i64 2)
store %struct.ScmObj* %k50136, %struct.ScmObj** %stackaddr$env-ref56796
%stackaddr$prim56797 = alloca %struct.ScmObj*, align 8
%_95k50138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55873)
store volatile %struct.ScmObj* %_95k50138, %struct.ScmObj** %stackaddr$prim56797, align 8
%stackaddr$prim56798 = alloca %struct.ScmObj*, align 8
%current_45args55874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55873)
store volatile %struct.ScmObj* %current_45args55874, %struct.ScmObj** %stackaddr$prim56798, align 8
%stackaddr$prim56799 = alloca %struct.ScmObj*, align 8
%cc49934 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55874)
store volatile %struct.ScmObj* %cc49934, %struct.ScmObj** %stackaddr$prim56799, align 8
%ae53162 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56800 = alloca %struct.ScmObj*, align 8
%anf_45bind50071 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53162)
store volatile %struct.ScmObj* %anf_45bind50071, %struct.ScmObj** %stackaddr$prim56800, align 8
%stackaddr$prim56801 = alloca %struct.ScmObj*, align 8
%anf_45bind50072 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind50071)
store volatile %struct.ScmObj* %anf_45bind50072, %struct.ScmObj** %stackaddr$prim56801, align 8
%truthy$cmp56802 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50072)
%cmp$cmp56802 = icmp eq i64 %truthy$cmp56802, 1
br i1 %cmp$cmp56802, label %truebranch$cmp56802, label %falsebranch$cmp56802
truebranch$cmp56802:
%ae53166 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53167 = call %struct.ScmObj* @const_init_false()
%argslist55876$k501360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56803 = alloca %struct.ScmObj*, align 8
%argslist55876$k501361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53167, %struct.ScmObj* %argslist55876$k501360)
store volatile %struct.ScmObj* %argslist55876$k501361, %struct.ScmObj** %stackaddr$prim56803, align 8
%stackaddr$prim56804 = alloca %struct.ScmObj*, align 8
%argslist55876$k501362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53166, %struct.ScmObj* %argslist55876$k501361)
store volatile %struct.ScmObj* %argslist55876$k501362, %struct.ScmObj** %stackaddr$prim56804, align 8
%clofunc56805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50136)
musttail call tailcc void %clofunc56805(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55876$k501362)
ret void
falsebranch$cmp56802:
%ae53175 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%anf_45bind50073 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53175)
store volatile %struct.ScmObj* %anf_45bind50073, %struct.ScmObj** %stackaddr$prim56806, align 8
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%anf_45bind50074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50073)
store volatile %struct.ScmObj* %anf_45bind50074, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$prim56808 = alloca %struct.ScmObj*, align 8
%anf_45bind50075 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind50074, %struct.ScmObj* %v49932)
store volatile %struct.ScmObj* %anf_45bind50075, %struct.ScmObj** %stackaddr$prim56808, align 8
%truthy$cmp56809 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50075)
%cmp$cmp56809 = icmp eq i64 %truthy$cmp56809, 1
br i1 %cmp$cmp56809, label %truebranch$cmp56809, label %falsebranch$cmp56809
truebranch$cmp56809:
%ae53181 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56810 = alloca %struct.ScmObj*, align 8
%cpsprim50139 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53181)
store volatile %struct.ScmObj* %cpsprim50139, %struct.ScmObj** %stackaddr$prim56810, align 8
%ae53183 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55877$k501360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56811 = alloca %struct.ScmObj*, align 8
%argslist55877$k501361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50139, %struct.ScmObj* %argslist55877$k501360)
store volatile %struct.ScmObj* %argslist55877$k501361, %struct.ScmObj** %stackaddr$prim56811, align 8
%stackaddr$prim56812 = alloca %struct.ScmObj*, align 8
%argslist55877$k501362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53183, %struct.ScmObj* %argslist55877$k501361)
store volatile %struct.ScmObj* %argslist55877$k501362, %struct.ScmObj** %stackaddr$prim56812, align 8
%clofunc56813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50136)
musttail call tailcc void %clofunc56813(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55877$k501362)
ret void
falsebranch$cmp56809:
%ae53194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56814 = alloca %struct.ScmObj*, align 8
%anf_45bind50076 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53194)
store volatile %struct.ScmObj* %anf_45bind50076, %struct.ScmObj** %stackaddr$prim56814, align 8
%stackaddr$prim56815 = alloca %struct.ScmObj*, align 8
%anf_45bind50077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50076)
store volatile %struct.ScmObj* %anf_45bind50077, %struct.ScmObj** %stackaddr$prim56815, align 8
%ae53197 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56816 = alloca %struct.ScmObj*, align 8
%_95049936 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53197, %struct.ScmObj* %anf_45bind50077)
store volatile %struct.ScmObj* %_95049936, %struct.ScmObj** %stackaddr$prim56816, align 8
%argslist55878$cc499340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56817 = alloca %struct.ScmObj*, align 8
%argslist55878$cc499341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49934, %struct.ScmObj* %argslist55878$cc499340)
store volatile %struct.ScmObj* %argslist55878$cc499341, %struct.ScmObj** %stackaddr$prim56817, align 8
%stackaddr$prim56818 = alloca %struct.ScmObj*, align 8
%argslist55878$cc499342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55878$cc499341)
store volatile %struct.ScmObj* %argslist55878$cc499342, %struct.ScmObj** %stackaddr$prim56818, align 8
%clofunc56819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49934)
musttail call tailcc void %clofunc56819(%struct.ScmObj* %cc49934, %struct.ScmObj* %argslist55878$cc499342)
ret void
}

define tailcc void @proc_clo$ae53054(%struct.ScmObj* %env$ae53054,%struct.ScmObj* %current_45args55879) {
%stackaddr$env-ref56820 = alloca %struct.ScmObj*, align 8
%lst49933 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53054, i64 0)
store %struct.ScmObj* %lst49933, %struct.ScmObj** %stackaddr$env-ref56820
%stackaddr$env-ref56821 = alloca %struct.ScmObj*, align 8
%v49932 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53054, i64 1)
store %struct.ScmObj* %v49932, %struct.ScmObj** %stackaddr$env-ref56821
%stackaddr$env-ref56822 = alloca %struct.ScmObj*, align 8
%k50136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53054, i64 2)
store %struct.ScmObj* %k50136, %struct.ScmObj** %stackaddr$env-ref56822
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%_95k50138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55879)
store volatile %struct.ScmObj* %_95k50138, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$prim56824 = alloca %struct.ScmObj*, align 8
%current_45args55880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55879)
store volatile %struct.ScmObj* %current_45args55880, %struct.ScmObj** %stackaddr$prim56824, align 8
%stackaddr$prim56825 = alloca %struct.ScmObj*, align 8
%cc49934 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55880)
store volatile %struct.ScmObj* %cc49934, %struct.ScmObj** %stackaddr$prim56825, align 8
%ae53056 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%anf_45bind50071 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53056)
store volatile %struct.ScmObj* %anf_45bind50071, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%anf_45bind50072 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind50071)
store volatile %struct.ScmObj* %anf_45bind50072, %struct.ScmObj** %stackaddr$prim56827, align 8
%truthy$cmp56828 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50072)
%cmp$cmp56828 = icmp eq i64 %truthy$cmp56828, 1
br i1 %cmp$cmp56828, label %truebranch$cmp56828, label %falsebranch$cmp56828
truebranch$cmp56828:
%ae53060 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53061 = call %struct.ScmObj* @const_init_false()
%argslist55882$k501360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%argslist55882$k501361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53061, %struct.ScmObj* %argslist55882$k501360)
store volatile %struct.ScmObj* %argslist55882$k501361, %struct.ScmObj** %stackaddr$prim56829, align 8
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%argslist55882$k501362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53060, %struct.ScmObj* %argslist55882$k501361)
store volatile %struct.ScmObj* %argslist55882$k501362, %struct.ScmObj** %stackaddr$prim56830, align 8
%clofunc56831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50136)
musttail call tailcc void %clofunc56831(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55882$k501362)
ret void
falsebranch$cmp56828:
%ae53069 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56832 = alloca %struct.ScmObj*, align 8
%anf_45bind50073 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53069)
store volatile %struct.ScmObj* %anf_45bind50073, %struct.ScmObj** %stackaddr$prim56832, align 8
%stackaddr$prim56833 = alloca %struct.ScmObj*, align 8
%anf_45bind50074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50073)
store volatile %struct.ScmObj* %anf_45bind50074, %struct.ScmObj** %stackaddr$prim56833, align 8
%stackaddr$prim56834 = alloca %struct.ScmObj*, align 8
%anf_45bind50075 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind50074, %struct.ScmObj* %v49932)
store volatile %struct.ScmObj* %anf_45bind50075, %struct.ScmObj** %stackaddr$prim56834, align 8
%truthy$cmp56835 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50075)
%cmp$cmp56835 = icmp eq i64 %truthy$cmp56835, 1
br i1 %cmp$cmp56835, label %truebranch$cmp56835, label %falsebranch$cmp56835
truebranch$cmp56835:
%ae53075 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%cpsprim50139 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53075)
store volatile %struct.ScmObj* %cpsprim50139, %struct.ScmObj** %stackaddr$prim56836, align 8
%ae53077 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55883$k501360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%argslist55883$k501361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50139, %struct.ScmObj* %argslist55883$k501360)
store volatile %struct.ScmObj* %argslist55883$k501361, %struct.ScmObj** %stackaddr$prim56837, align 8
%stackaddr$prim56838 = alloca %struct.ScmObj*, align 8
%argslist55883$k501362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53077, %struct.ScmObj* %argslist55883$k501361)
store volatile %struct.ScmObj* %argslist55883$k501362, %struct.ScmObj** %stackaddr$prim56838, align 8
%clofunc56839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50136)
musttail call tailcc void %clofunc56839(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55883$k501362)
ret void
falsebranch$cmp56835:
%ae53088 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%anf_45bind50076 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53088)
store volatile %struct.ScmObj* %anf_45bind50076, %struct.ScmObj** %stackaddr$prim56840, align 8
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%anf_45bind50077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50076)
store volatile %struct.ScmObj* %anf_45bind50077, %struct.ScmObj** %stackaddr$prim56841, align 8
%ae53091 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56842 = alloca %struct.ScmObj*, align 8
%_95049936 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst49933, %struct.ScmObj* %ae53091, %struct.ScmObj* %anf_45bind50077)
store volatile %struct.ScmObj* %_95049936, %struct.ScmObj** %stackaddr$prim56842, align 8
%argslist55884$cc499340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56843 = alloca %struct.ScmObj*, align 8
%argslist55884$cc499341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49934, %struct.ScmObj* %argslist55884$cc499340)
store volatile %struct.ScmObj* %argslist55884$cc499341, %struct.ScmObj** %stackaddr$prim56843, align 8
%stackaddr$prim56844 = alloca %struct.ScmObj*, align 8
%argslist55884$cc499342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50136, %struct.ScmObj* %argslist55884$cc499341)
store volatile %struct.ScmObj* %argslist55884$cc499342, %struct.ScmObj** %stackaddr$prim56844, align 8
%clofunc56845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49934)
musttail call tailcc void %clofunc56845(%struct.ScmObj* %cc49934, %struct.ScmObj* %argslist55884$cc499342)
ret void
}

define tailcc void @proc_clo$ae53039(%struct.ScmObj* %env$ae53039,%struct.ScmObj* %current_45args55886) {
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%k50140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55886)
store volatile %struct.ScmObj* %k50140, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%current_45args55887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55886)
store volatile %struct.ScmObj* %current_45args55887, %struct.ScmObj** %stackaddr$prim56847, align 8
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%u49935 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55887)
store volatile %struct.ScmObj* %u49935, %struct.ScmObj** %stackaddr$prim56848, align 8
%argslist55889$u499350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%argslist55889$u499351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u49935, %struct.ScmObj* %argslist55889$u499350)
store volatile %struct.ScmObj* %argslist55889$u499351, %struct.ScmObj** %stackaddr$prim56849, align 8
%stackaddr$prim56850 = alloca %struct.ScmObj*, align 8
%argslist55889$u499352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50140, %struct.ScmObj* %argslist55889$u499351)
store volatile %struct.ScmObj* %argslist55889$u499352, %struct.ScmObj** %stackaddr$prim56850, align 8
%clofunc56851 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u49935)
musttail call tailcc void %clofunc56851(%struct.ScmObj* %u49935, %struct.ScmObj* %argslist55889$u499352)
ret void
}

define tailcc void @proc_clo$ae52498(%struct.ScmObj* %env$ae52498,%struct.ScmObj* %current_45args55892) {
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%k50141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55892)
store volatile %struct.ScmObj* %k50141, %struct.ScmObj** %stackaddr$prim56852, align 8
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%current_45args55893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55892)
store volatile %struct.ScmObj* %current_45args55893, %struct.ScmObj** %stackaddr$prim56853, align 8
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%lst49939 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55893)
store volatile %struct.ScmObj* %lst49939, %struct.ScmObj** %stackaddr$prim56854, align 8
%stackaddr$prim56855 = alloca %struct.ScmObj*, align 8
%current_45args55894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55893)
store volatile %struct.ScmObj* %current_45args55894, %struct.ScmObj** %stackaddr$prim56855, align 8
%stackaddr$prim56856 = alloca %struct.ScmObj*, align 8
%n49938 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55894)
store volatile %struct.ScmObj* %n49938, %struct.ScmObj** %stackaddr$prim56856, align 8
%ae52499 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56857 = alloca %struct.ScmObj*, align 8
%n49941 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae52499, %struct.ScmObj* %n49938)
store volatile %struct.ScmObj* %n49941, %struct.ScmObj** %stackaddr$prim56857, align 8
%ae52501 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%lst49940 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae52501, %struct.ScmObj* %lst49939)
store volatile %struct.ScmObj* %lst49940, %struct.ScmObj** %stackaddr$prim56858, align 8
%stackaddr$makeclosure56859 = alloca %struct.ScmObj*, align 8
%fptrToInt56860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52503 to i64
%ae52503 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56860)
store volatile %struct.ScmObj* %ae52503, %struct.ScmObj** %stackaddr$makeclosure56859, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52503, %struct.ScmObj* %n49941, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52503, %struct.ScmObj* %lst49940, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52503, %struct.ScmObj* %k50141, i64 2)
%ae52504 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56861 = alloca %struct.ScmObj*, align 8
%fptrToInt56862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52505 to i64
%ae52505 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56862)
store volatile %struct.ScmObj* %ae52505, %struct.ScmObj** %stackaddr$makeclosure56861, align 8
%argslist55914$ae525030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56863 = alloca %struct.ScmObj*, align 8
%argslist55914$ae525031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52505, %struct.ScmObj* %argslist55914$ae525030)
store volatile %struct.ScmObj* %argslist55914$ae525031, %struct.ScmObj** %stackaddr$prim56863, align 8
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%argslist55914$ae525032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52504, %struct.ScmObj* %argslist55914$ae525031)
store volatile %struct.ScmObj* %argslist55914$ae525032, %struct.ScmObj** %stackaddr$prim56864, align 8
%clofunc56865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52503)
musttail call tailcc void %clofunc56865(%struct.ScmObj* %ae52503, %struct.ScmObj* %argslist55914$ae525032)
ret void
}

define tailcc void @proc_clo$ae52503(%struct.ScmObj* %env$ae52503,%struct.ScmObj* %current_45args55896) {
%stackaddr$env-ref56866 = alloca %struct.ScmObj*, align 8
%n49941 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52503, i64 0)
store %struct.ScmObj* %n49941, %struct.ScmObj** %stackaddr$env-ref56866
%stackaddr$env-ref56867 = alloca %struct.ScmObj*, align 8
%lst49940 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52503, i64 1)
store %struct.ScmObj* %lst49940, %struct.ScmObj** %stackaddr$env-ref56867
%stackaddr$env-ref56868 = alloca %struct.ScmObj*, align 8
%k50141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52503, i64 2)
store %struct.ScmObj* %k50141, %struct.ScmObj** %stackaddr$env-ref56868
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%_95k50142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55896)
store volatile %struct.ScmObj* %_95k50142, %struct.ScmObj** %stackaddr$prim56869, align 8
%stackaddr$prim56870 = alloca %struct.ScmObj*, align 8
%current_45args55897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55896)
store volatile %struct.ScmObj* %current_45args55897, %struct.ScmObj** %stackaddr$prim56870, align 8
%stackaddr$prim56871 = alloca %struct.ScmObj*, align 8
%anf_45bind50063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55897)
store volatile %struct.ScmObj* %anf_45bind50063, %struct.ScmObj** %stackaddr$prim56871, align 8
%stackaddr$makeclosure56872 = alloca %struct.ScmObj*, align 8
%fptrToInt56873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52519 to i64
%ae52519 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56873)
store volatile %struct.ScmObj* %ae52519, %struct.ScmObj** %stackaddr$makeclosure56872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52519, %struct.ScmObj* %n49941, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52519, %struct.ScmObj* %lst49940, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52519, %struct.ScmObj* %k50141, i64 2)
%stackaddr$makeclosure56874 = alloca %struct.ScmObj*, align 8
%fptrToInt56875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52520 to i64
%ae52520 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56875)
store volatile %struct.ScmObj* %ae52520, %struct.ScmObj** %stackaddr$makeclosure56874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52520, %struct.ScmObj* %n49941, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52520, %struct.ScmObj* %lst49940, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52520, %struct.ScmObj* %k50141, i64 2)
%argslist55909$anf_45bind500630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56876 = alloca %struct.ScmObj*, align 8
%argslist55909$anf_45bind500631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52520, %struct.ScmObj* %argslist55909$anf_45bind500630)
store volatile %struct.ScmObj* %argslist55909$anf_45bind500631, %struct.ScmObj** %stackaddr$prim56876, align 8
%stackaddr$prim56877 = alloca %struct.ScmObj*, align 8
%argslist55909$anf_45bind500632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52519, %struct.ScmObj* %argslist55909$anf_45bind500631)
store volatile %struct.ScmObj* %argslist55909$anf_45bind500632, %struct.ScmObj** %stackaddr$prim56877, align 8
%clofunc56878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50063)
musttail call tailcc void %clofunc56878(%struct.ScmObj* %anf_45bind50063, %struct.ScmObj* %argslist55909$anf_45bind500632)
ret void
}

define tailcc void @proc_clo$ae52519(%struct.ScmObj* %env$ae52519,%struct.ScmObj* %current_45args55899) {
%stackaddr$env-ref56879 = alloca %struct.ScmObj*, align 8
%n49941 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52519, i64 0)
store %struct.ScmObj* %n49941, %struct.ScmObj** %stackaddr$env-ref56879
%stackaddr$env-ref56880 = alloca %struct.ScmObj*, align 8
%lst49940 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52519, i64 1)
store %struct.ScmObj* %lst49940, %struct.ScmObj** %stackaddr$env-ref56880
%stackaddr$env-ref56881 = alloca %struct.ScmObj*, align 8
%k50141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52519, i64 2)
store %struct.ScmObj* %k50141, %struct.ScmObj** %stackaddr$env-ref56881
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%_95k50143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55899)
store volatile %struct.ScmObj* %_95k50143, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%current_45args55900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55899)
store volatile %struct.ScmObj* %current_45args55900, %struct.ScmObj** %stackaddr$prim56883, align 8
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%cc49942 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55900)
store volatile %struct.ScmObj* %cc49942, %struct.ScmObj** %stackaddr$prim56884, align 8
%ae52662 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56885 = alloca %struct.ScmObj*, align 8
%anf_45bind50064 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52662)
store volatile %struct.ScmObj* %anf_45bind50064, %struct.ScmObj** %stackaddr$prim56885, align 8
%ae52663 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56886 = alloca %struct.ScmObj*, align 8
%anf_45bind50065 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae52663, %struct.ScmObj* %anf_45bind50064)
store volatile %struct.ScmObj* %anf_45bind50065, %struct.ScmObj** %stackaddr$prim56886, align 8
%truthy$cmp56887 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50065)
%cmp$cmp56887 = icmp eq i64 %truthy$cmp56887, 1
br i1 %cmp$cmp56887, label %truebranch$cmp56887, label %falsebranch$cmp56887
truebranch$cmp56887:
%ae52667 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56888 = alloca %struct.ScmObj*, align 8
%cpsprim50144 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52667)
store volatile %struct.ScmObj* %cpsprim50144, %struct.ScmObj** %stackaddr$prim56888, align 8
%ae52669 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55902$k501410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56889 = alloca %struct.ScmObj*, align 8
%argslist55902$k501411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50144, %struct.ScmObj* %argslist55902$k501410)
store volatile %struct.ScmObj* %argslist55902$k501411, %struct.ScmObj** %stackaddr$prim56889, align 8
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%argslist55902$k501412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52669, %struct.ScmObj* %argslist55902$k501411)
store volatile %struct.ScmObj* %argslist55902$k501412, %struct.ScmObj** %stackaddr$prim56890, align 8
%clofunc56891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50141)
musttail call tailcc void %clofunc56891(%struct.ScmObj* %k50141, %struct.ScmObj* %argslist55902$k501412)
ret void
falsebranch$cmp56887:
%ae52680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%anf_45bind50066 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52680)
store volatile %struct.ScmObj* %anf_45bind50066, %struct.ScmObj** %stackaddr$prim56892, align 8
%stackaddr$prim56893 = alloca %struct.ScmObj*, align 8
%anf_45bind50067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50066)
store volatile %struct.ScmObj* %anf_45bind50067, %struct.ScmObj** %stackaddr$prim56893, align 8
%ae52683 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%_95049945 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52683, %struct.ScmObj* %anf_45bind50067)
store volatile %struct.ScmObj* %_95049945, %struct.ScmObj** %stackaddr$prim56894, align 8
%ae52686 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56895 = alloca %struct.ScmObj*, align 8
%anf_45bind50068 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52686)
store volatile %struct.ScmObj* %anf_45bind50068, %struct.ScmObj** %stackaddr$prim56895, align 8
%ae52688 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%anf_45bind50069 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind50068, %struct.ScmObj* %ae52688)
store volatile %struct.ScmObj* %anf_45bind50069, %struct.ScmObj** %stackaddr$prim56896, align 8
%ae52690 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56897 = alloca %struct.ScmObj*, align 8
%_95149944 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52690, %struct.ScmObj* %anf_45bind50069)
store volatile %struct.ScmObj* %_95149944, %struct.ScmObj** %stackaddr$prim56897, align 8
%argslist55903$cc499420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56898 = alloca %struct.ScmObj*, align 8
%argslist55903$cc499421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49942, %struct.ScmObj* %argslist55903$cc499420)
store volatile %struct.ScmObj* %argslist55903$cc499421, %struct.ScmObj** %stackaddr$prim56898, align 8
%stackaddr$prim56899 = alloca %struct.ScmObj*, align 8
%argslist55903$cc499422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50141, %struct.ScmObj* %argslist55903$cc499421)
store volatile %struct.ScmObj* %argslist55903$cc499422, %struct.ScmObj** %stackaddr$prim56899, align 8
%clofunc56900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49942)
musttail call tailcc void %clofunc56900(%struct.ScmObj* %cc49942, %struct.ScmObj* %argslist55903$cc499422)
ret void
}

define tailcc void @proc_clo$ae52520(%struct.ScmObj* %env$ae52520,%struct.ScmObj* %current_45args55904) {
%stackaddr$env-ref56901 = alloca %struct.ScmObj*, align 8
%n49941 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52520, i64 0)
store %struct.ScmObj* %n49941, %struct.ScmObj** %stackaddr$env-ref56901
%stackaddr$env-ref56902 = alloca %struct.ScmObj*, align 8
%lst49940 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52520, i64 1)
store %struct.ScmObj* %lst49940, %struct.ScmObj** %stackaddr$env-ref56902
%stackaddr$env-ref56903 = alloca %struct.ScmObj*, align 8
%k50141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52520, i64 2)
store %struct.ScmObj* %k50141, %struct.ScmObj** %stackaddr$env-ref56903
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%_95k50143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55904)
store volatile %struct.ScmObj* %_95k50143, %struct.ScmObj** %stackaddr$prim56904, align 8
%stackaddr$prim56905 = alloca %struct.ScmObj*, align 8
%current_45args55905 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55904)
store volatile %struct.ScmObj* %current_45args55905, %struct.ScmObj** %stackaddr$prim56905, align 8
%stackaddr$prim56906 = alloca %struct.ScmObj*, align 8
%cc49942 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55905)
store volatile %struct.ScmObj* %cc49942, %struct.ScmObj** %stackaddr$prim56906, align 8
%ae52522 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56907 = alloca %struct.ScmObj*, align 8
%anf_45bind50064 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52522)
store volatile %struct.ScmObj* %anf_45bind50064, %struct.ScmObj** %stackaddr$prim56907, align 8
%ae52523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56908 = alloca %struct.ScmObj*, align 8
%anf_45bind50065 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae52523, %struct.ScmObj* %anf_45bind50064)
store volatile %struct.ScmObj* %anf_45bind50065, %struct.ScmObj** %stackaddr$prim56908, align 8
%truthy$cmp56909 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50065)
%cmp$cmp56909 = icmp eq i64 %truthy$cmp56909, 1
br i1 %cmp$cmp56909, label %truebranch$cmp56909, label %falsebranch$cmp56909
truebranch$cmp56909:
%ae52527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%cpsprim50144 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52527)
store volatile %struct.ScmObj* %cpsprim50144, %struct.ScmObj** %stackaddr$prim56910, align 8
%ae52529 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55907$k501410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%argslist55907$k501411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50144, %struct.ScmObj* %argslist55907$k501410)
store volatile %struct.ScmObj* %argslist55907$k501411, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%argslist55907$k501412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52529, %struct.ScmObj* %argslist55907$k501411)
store volatile %struct.ScmObj* %argslist55907$k501412, %struct.ScmObj** %stackaddr$prim56912, align 8
%clofunc56913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50141)
musttail call tailcc void %clofunc56913(%struct.ScmObj* %k50141, %struct.ScmObj* %argslist55907$k501412)
ret void
falsebranch$cmp56909:
%ae52540 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%anf_45bind50066 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52540)
store volatile %struct.ScmObj* %anf_45bind50066, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%anf_45bind50067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50066)
store volatile %struct.ScmObj* %anf_45bind50067, %struct.ScmObj** %stackaddr$prim56915, align 8
%ae52543 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%_95049945 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst49940, %struct.ScmObj* %ae52543, %struct.ScmObj* %anf_45bind50067)
store volatile %struct.ScmObj* %_95049945, %struct.ScmObj** %stackaddr$prim56916, align 8
%ae52546 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56917 = alloca %struct.ScmObj*, align 8
%anf_45bind50068 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52546)
store volatile %struct.ScmObj* %anf_45bind50068, %struct.ScmObj** %stackaddr$prim56917, align 8
%ae52548 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%anf_45bind50069 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind50068, %struct.ScmObj* %ae52548)
store volatile %struct.ScmObj* %anf_45bind50069, %struct.ScmObj** %stackaddr$prim56918, align 8
%ae52550 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56919 = alloca %struct.ScmObj*, align 8
%_95149944 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n49941, %struct.ScmObj* %ae52550, %struct.ScmObj* %anf_45bind50069)
store volatile %struct.ScmObj* %_95149944, %struct.ScmObj** %stackaddr$prim56919, align 8
%argslist55908$cc499420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56920 = alloca %struct.ScmObj*, align 8
%argslist55908$cc499421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49942, %struct.ScmObj* %argslist55908$cc499420)
store volatile %struct.ScmObj* %argslist55908$cc499421, %struct.ScmObj** %stackaddr$prim56920, align 8
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%argslist55908$cc499422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50141, %struct.ScmObj* %argslist55908$cc499421)
store volatile %struct.ScmObj* %argslist55908$cc499422, %struct.ScmObj** %stackaddr$prim56921, align 8
%clofunc56922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49942)
musttail call tailcc void %clofunc56922(%struct.ScmObj* %cc49942, %struct.ScmObj* %argslist55908$cc499422)
ret void
}

define tailcc void @proc_clo$ae52505(%struct.ScmObj* %env$ae52505,%struct.ScmObj* %current_45args55910) {
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%k50145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55910)
store volatile %struct.ScmObj* %k50145, %struct.ScmObj** %stackaddr$prim56923, align 8
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%current_45args55911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55910)
store volatile %struct.ScmObj* %current_45args55911, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%u49943 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55911)
store volatile %struct.ScmObj* %u49943, %struct.ScmObj** %stackaddr$prim56925, align 8
%argslist55913$u499430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%argslist55913$u499431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u49943, %struct.ScmObj* %argslist55913$u499430)
store volatile %struct.ScmObj* %argslist55913$u499431, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%argslist55913$u499432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50145, %struct.ScmObj* %argslist55913$u499431)
store volatile %struct.ScmObj* %argslist55913$u499432, %struct.ScmObj** %stackaddr$prim56927, align 8
%clofunc56928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u49943)
musttail call tailcc void %clofunc56928(%struct.ScmObj* %u49943, %struct.ScmObj* %argslist55913$u499432)
ret void
}

define tailcc void @proc_clo$ae52082(%struct.ScmObj* %env$ae52082,%struct.ScmObj* %current_45args55916) {
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%k50146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55916)
store volatile %struct.ScmObj* %k50146, %struct.ScmObj** %stackaddr$prim56929, align 8
%stackaddr$prim56930 = alloca %struct.ScmObj*, align 8
%current_45args55917 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55916)
store volatile %struct.ScmObj* %current_45args55917, %struct.ScmObj** %stackaddr$prim56930, align 8
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%a49947 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55917)
store volatile %struct.ScmObj* %a49947, %struct.ScmObj** %stackaddr$prim56931, align 8
%ae52083 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%a49948 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae52083, %struct.ScmObj* %a49947)
store volatile %struct.ScmObj* %a49948, %struct.ScmObj** %stackaddr$prim56932, align 8
%stackaddr$makeclosure56933 = alloca %struct.ScmObj*, align 8
%fptrToInt56934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52085 to i64
%ae52085 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56934)
store volatile %struct.ScmObj* %ae52085, %struct.ScmObj** %stackaddr$makeclosure56933, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52085, %struct.ScmObj* %k50146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52085, %struct.ScmObj* %a49948, i64 1)
%ae52086 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56935 = alloca %struct.ScmObj*, align 8
%fptrToInt56936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52087 to i64
%ae52087 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56936)
store volatile %struct.ScmObj* %ae52087, %struct.ScmObj** %stackaddr$makeclosure56935, align 8
%argslist55939$ae520850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%argslist55939$ae520851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52087, %struct.ScmObj* %argslist55939$ae520850)
store volatile %struct.ScmObj* %argslist55939$ae520851, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%argslist55939$ae520852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52086, %struct.ScmObj* %argslist55939$ae520851)
store volatile %struct.ScmObj* %argslist55939$ae520852, %struct.ScmObj** %stackaddr$prim56938, align 8
%clofunc56939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52085)
musttail call tailcc void %clofunc56939(%struct.ScmObj* %ae52085, %struct.ScmObj* %argslist55939$ae520852)
ret void
}

define tailcc void @proc_clo$ae52085(%struct.ScmObj* %env$ae52085,%struct.ScmObj* %current_45args55919) {
%stackaddr$env-ref56940 = alloca %struct.ScmObj*, align 8
%k50146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52085, i64 0)
store %struct.ScmObj* %k50146, %struct.ScmObj** %stackaddr$env-ref56940
%stackaddr$env-ref56941 = alloca %struct.ScmObj*, align 8
%a49948 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52085, i64 1)
store %struct.ScmObj* %a49948, %struct.ScmObj** %stackaddr$env-ref56941
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%_95k50147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55919)
store volatile %struct.ScmObj* %_95k50147, %struct.ScmObj** %stackaddr$prim56942, align 8
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%current_45args55920 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55919)
store volatile %struct.ScmObj* %current_45args55920, %struct.ScmObj** %stackaddr$prim56943, align 8
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%anf_45bind50055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55920)
store volatile %struct.ScmObj* %anf_45bind50055, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$makeclosure56945 = alloca %struct.ScmObj*, align 8
%fptrToInt56946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52104 to i64
%ae52104 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56946)
store volatile %struct.ScmObj* %ae52104, %struct.ScmObj** %stackaddr$makeclosure56945, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52104, %struct.ScmObj* %k50146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52104, %struct.ScmObj* %a49948, i64 1)
%stackaddr$makeclosure56947 = alloca %struct.ScmObj*, align 8
%fptrToInt56948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52105 to i64
%ae52105 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56948)
store volatile %struct.ScmObj* %ae52105, %struct.ScmObj** %stackaddr$makeclosure56947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52105, %struct.ScmObj* %k50146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52105, %struct.ScmObj* %a49948, i64 1)
%argslist55934$anf_45bind500550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%argslist55934$anf_45bind500551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52105, %struct.ScmObj* %argslist55934$anf_45bind500550)
store volatile %struct.ScmObj* %argslist55934$anf_45bind500551, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%argslist55934$anf_45bind500552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52104, %struct.ScmObj* %argslist55934$anf_45bind500551)
store volatile %struct.ScmObj* %argslist55934$anf_45bind500552, %struct.ScmObj** %stackaddr$prim56950, align 8
%clofunc56951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50055)
musttail call tailcc void %clofunc56951(%struct.ScmObj* %anf_45bind50055, %struct.ScmObj* %argslist55934$anf_45bind500552)
ret void
}

define tailcc void @proc_clo$ae52104(%struct.ScmObj* %env$ae52104,%struct.ScmObj* %current_45args55922) {
%stackaddr$env-ref56952 = alloca %struct.ScmObj*, align 8
%k50146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52104, i64 0)
store %struct.ScmObj* %k50146, %struct.ScmObj** %stackaddr$env-ref56952
%stackaddr$env-ref56953 = alloca %struct.ScmObj*, align 8
%a49948 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52104, i64 1)
store %struct.ScmObj* %a49948, %struct.ScmObj** %stackaddr$env-ref56953
%stackaddr$prim56954 = alloca %struct.ScmObj*, align 8
%_95k50148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55922)
store volatile %struct.ScmObj* %_95k50148, %struct.ScmObj** %stackaddr$prim56954, align 8
%stackaddr$prim56955 = alloca %struct.ScmObj*, align 8
%current_45args55923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55922)
store volatile %struct.ScmObj* %current_45args55923, %struct.ScmObj** %stackaddr$prim56955, align 8
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%cc49949 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55923)
store volatile %struct.ScmObj* %cc49949, %struct.ScmObj** %stackaddr$prim56956, align 8
%ae52220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%anf_45bind50056 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52220)
store volatile %struct.ScmObj* %anf_45bind50056, %struct.ScmObj** %stackaddr$prim56957, align 8
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%anf_45bind50057 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind50056)
store volatile %struct.ScmObj* %anf_45bind50057, %struct.ScmObj** %stackaddr$prim56958, align 8
%truthy$cmp56959 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50057)
%cmp$cmp56959 = icmp eq i64 %truthy$cmp56959, 1
br i1 %cmp$cmp56959, label %truebranch$cmp56959, label %falsebranch$cmp56959
truebranch$cmp56959:
%ae52224 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52225 = call %struct.ScmObj* @const_init_true()
%argslist55925$k501460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%argslist55925$k501461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52225, %struct.ScmObj* %argslist55925$k501460)
store volatile %struct.ScmObj* %argslist55925$k501461, %struct.ScmObj** %stackaddr$prim56960, align 8
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%argslist55925$k501462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52224, %struct.ScmObj* %argslist55925$k501461)
store volatile %struct.ScmObj* %argslist55925$k501462, %struct.ScmObj** %stackaddr$prim56961, align 8
%clofunc56962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50146)
musttail call tailcc void %clofunc56962(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55925$k501462)
ret void
falsebranch$cmp56959:
%ae52233 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56963 = alloca %struct.ScmObj*, align 8
%anf_45bind50058 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52233)
store volatile %struct.ScmObj* %anf_45bind50058, %struct.ScmObj** %stackaddr$prim56963, align 8
%stackaddr$prim56964 = alloca %struct.ScmObj*, align 8
%anf_45bind50059 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind50058)
store volatile %struct.ScmObj* %anf_45bind50059, %struct.ScmObj** %stackaddr$prim56964, align 8
%truthy$cmp56965 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50059)
%cmp$cmp56965 = icmp eq i64 %truthy$cmp56965, 1
br i1 %cmp$cmp56965, label %truebranch$cmp56965, label %falsebranch$cmp56965
truebranch$cmp56965:
%ae52237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%anf_45bind50060 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52237)
store volatile %struct.ScmObj* %anf_45bind50060, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%b49951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50060)
store volatile %struct.ScmObj* %b49951, %struct.ScmObj** %stackaddr$prim56967, align 8
%ae52240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%anf_45bind50061 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52240)
store volatile %struct.ScmObj* %anf_45bind50061, %struct.ScmObj** %stackaddr$prim56968, align 8
%stackaddr$prim56969 = alloca %struct.ScmObj*, align 8
%anf_45bind50062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50061)
store volatile %struct.ScmObj* %anf_45bind50062, %struct.ScmObj** %stackaddr$prim56969, align 8
%ae52243 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56970 = alloca %struct.ScmObj*, align 8
%_95049952 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52243, %struct.ScmObj* %anf_45bind50062)
store volatile %struct.ScmObj* %_95049952, %struct.ScmObj** %stackaddr$prim56970, align 8
%argslist55926$cc499490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56971 = alloca %struct.ScmObj*, align 8
%argslist55926$cc499491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49949, %struct.ScmObj* %argslist55926$cc499490)
store volatile %struct.ScmObj* %argslist55926$cc499491, %struct.ScmObj** %stackaddr$prim56971, align 8
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%argslist55926$cc499492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55926$cc499491)
store volatile %struct.ScmObj* %argslist55926$cc499492, %struct.ScmObj** %stackaddr$prim56972, align 8
%clofunc56973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49949)
musttail call tailcc void %clofunc56973(%struct.ScmObj* %cc49949, %struct.ScmObj* %argslist55926$cc499492)
ret void
falsebranch$cmp56965:
%ae52276 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52277 = call %struct.ScmObj* @const_init_false()
%argslist55927$k501460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%argslist55927$k501461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52277, %struct.ScmObj* %argslist55927$k501460)
store volatile %struct.ScmObj* %argslist55927$k501461, %struct.ScmObj** %stackaddr$prim56974, align 8
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%argslist55927$k501462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52276, %struct.ScmObj* %argslist55927$k501461)
store volatile %struct.ScmObj* %argslist55927$k501462, %struct.ScmObj** %stackaddr$prim56975, align 8
%clofunc56976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50146)
musttail call tailcc void %clofunc56976(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55927$k501462)
ret void
}

define tailcc void @proc_clo$ae52105(%struct.ScmObj* %env$ae52105,%struct.ScmObj* %current_45args55928) {
%stackaddr$env-ref56977 = alloca %struct.ScmObj*, align 8
%k50146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52105, i64 0)
store %struct.ScmObj* %k50146, %struct.ScmObj** %stackaddr$env-ref56977
%stackaddr$env-ref56978 = alloca %struct.ScmObj*, align 8
%a49948 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52105, i64 1)
store %struct.ScmObj* %a49948, %struct.ScmObj** %stackaddr$env-ref56978
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%_95k50148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55928)
store volatile %struct.ScmObj* %_95k50148, %struct.ScmObj** %stackaddr$prim56979, align 8
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%current_45args55929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55928)
store volatile %struct.ScmObj* %current_45args55929, %struct.ScmObj** %stackaddr$prim56980, align 8
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%cc49949 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55929)
store volatile %struct.ScmObj* %cc49949, %struct.ScmObj** %stackaddr$prim56981, align 8
%ae52107 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%anf_45bind50056 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52107)
store volatile %struct.ScmObj* %anf_45bind50056, %struct.ScmObj** %stackaddr$prim56982, align 8
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%anf_45bind50057 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind50056)
store volatile %struct.ScmObj* %anf_45bind50057, %struct.ScmObj** %stackaddr$prim56983, align 8
%truthy$cmp56984 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50057)
%cmp$cmp56984 = icmp eq i64 %truthy$cmp56984, 1
br i1 %cmp$cmp56984, label %truebranch$cmp56984, label %falsebranch$cmp56984
truebranch$cmp56984:
%ae52111 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52112 = call %struct.ScmObj* @const_init_true()
%argslist55931$k501460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56985 = alloca %struct.ScmObj*, align 8
%argslist55931$k501461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52112, %struct.ScmObj* %argslist55931$k501460)
store volatile %struct.ScmObj* %argslist55931$k501461, %struct.ScmObj** %stackaddr$prim56985, align 8
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%argslist55931$k501462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52111, %struct.ScmObj* %argslist55931$k501461)
store volatile %struct.ScmObj* %argslist55931$k501462, %struct.ScmObj** %stackaddr$prim56986, align 8
%clofunc56987 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50146)
musttail call tailcc void %clofunc56987(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55931$k501462)
ret void
falsebranch$cmp56984:
%ae52120 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%anf_45bind50058 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52120)
store volatile %struct.ScmObj* %anf_45bind50058, %struct.ScmObj** %stackaddr$prim56988, align 8
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%anf_45bind50059 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind50058)
store volatile %struct.ScmObj* %anf_45bind50059, %struct.ScmObj** %stackaddr$prim56989, align 8
%truthy$cmp56990 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50059)
%cmp$cmp56990 = icmp eq i64 %truthy$cmp56990, 1
br i1 %cmp$cmp56990, label %truebranch$cmp56990, label %falsebranch$cmp56990
truebranch$cmp56990:
%ae52124 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%anf_45bind50060 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52124)
store volatile %struct.ScmObj* %anf_45bind50060, %struct.ScmObj** %stackaddr$prim56991, align 8
%stackaddr$prim56992 = alloca %struct.ScmObj*, align 8
%b49951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50060)
store volatile %struct.ScmObj* %b49951, %struct.ScmObj** %stackaddr$prim56992, align 8
%ae52127 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%anf_45bind50061 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52127)
store volatile %struct.ScmObj* %anf_45bind50061, %struct.ScmObj** %stackaddr$prim56993, align 8
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%anf_45bind50062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50061)
store volatile %struct.ScmObj* %anf_45bind50062, %struct.ScmObj** %stackaddr$prim56994, align 8
%ae52130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%_95049952 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a49948, %struct.ScmObj* %ae52130, %struct.ScmObj* %anf_45bind50062)
store volatile %struct.ScmObj* %_95049952, %struct.ScmObj** %stackaddr$prim56995, align 8
%argslist55932$cc499490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%argslist55932$cc499491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc49949, %struct.ScmObj* %argslist55932$cc499490)
store volatile %struct.ScmObj* %argslist55932$cc499491, %struct.ScmObj** %stackaddr$prim56996, align 8
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%argslist55932$cc499492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55932$cc499491)
store volatile %struct.ScmObj* %argslist55932$cc499492, %struct.ScmObj** %stackaddr$prim56997, align 8
%clofunc56998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc49949)
musttail call tailcc void %clofunc56998(%struct.ScmObj* %cc49949, %struct.ScmObj* %argslist55932$cc499492)
ret void
falsebranch$cmp56990:
%ae52163 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52164 = call %struct.ScmObj* @const_init_false()
%argslist55933$k501460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%argslist55933$k501461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52164, %struct.ScmObj* %argslist55933$k501460)
store volatile %struct.ScmObj* %argslist55933$k501461, %struct.ScmObj** %stackaddr$prim56999, align 8
%stackaddr$prim57000 = alloca %struct.ScmObj*, align 8
%argslist55933$k501462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52163, %struct.ScmObj* %argslist55933$k501461)
store volatile %struct.ScmObj* %argslist55933$k501462, %struct.ScmObj** %stackaddr$prim57000, align 8
%clofunc57001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50146)
musttail call tailcc void %clofunc57001(%struct.ScmObj* %k50146, %struct.ScmObj* %argslist55933$k501462)
ret void
}

define tailcc void @proc_clo$ae52087(%struct.ScmObj* %env$ae52087,%struct.ScmObj* %current_45args55935) {
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%k50149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55935)
store volatile %struct.ScmObj* %k50149, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%current_45args55936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55935)
store volatile %struct.ScmObj* %current_45args55936, %struct.ScmObj** %stackaddr$prim57003, align 8
%stackaddr$prim57004 = alloca %struct.ScmObj*, align 8
%k49950 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55936)
store volatile %struct.ScmObj* %k49950, %struct.ScmObj** %stackaddr$prim57004, align 8
%ae52089 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55938$k501490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57005 = alloca %struct.ScmObj*, align 8
%argslist55938$k501491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k49950, %struct.ScmObj* %argslist55938$k501490)
store volatile %struct.ScmObj* %argslist55938$k501491, %struct.ScmObj** %stackaddr$prim57005, align 8
%stackaddr$prim57006 = alloca %struct.ScmObj*, align 8
%argslist55938$k501492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52089, %struct.ScmObj* %argslist55938$k501491)
store volatile %struct.ScmObj* %argslist55938$k501492, %struct.ScmObj** %stackaddr$prim57006, align 8
%clofunc57007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50149)
musttail call tailcc void %clofunc57007(%struct.ScmObj* %k50149, %struct.ScmObj* %argslist55938$k501492)
ret void
}

define tailcc void @proc_clo$ae52010(%struct.ScmObj* %env$ae52010,%struct.ScmObj* %current_45args55941) {
%stackaddr$env-ref57008 = alloca %struct.ScmObj*, align 8
%_37append49954 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52010, i64 0)
store %struct.ScmObj* %_37append49954, %struct.ScmObj** %stackaddr$env-ref57008
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%k50150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55941)
store volatile %struct.ScmObj* %k50150, %struct.ScmObj** %stackaddr$prim57009, align 8
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%current_45args55942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55941)
store volatile %struct.ScmObj* %current_45args55942, %struct.ScmObj** %stackaddr$prim57010, align 8
%stackaddr$prim57011 = alloca %struct.ScmObj*, align 8
%ls049957 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55942)
store volatile %struct.ScmObj* %ls049957, %struct.ScmObj** %stackaddr$prim57011, align 8
%stackaddr$prim57012 = alloca %struct.ScmObj*, align 8
%current_45args55943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55942)
store volatile %struct.ScmObj* %current_45args55943, %struct.ScmObj** %stackaddr$prim57012, align 8
%stackaddr$prim57013 = alloca %struct.ScmObj*, align 8
%ls149956 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55943)
store volatile %struct.ScmObj* %ls149956, %struct.ScmObj** %stackaddr$prim57013, align 8
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%anf_45bind50049 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls049957)
store volatile %struct.ScmObj* %anf_45bind50049, %struct.ScmObj** %stackaddr$prim57014, align 8
%truthy$cmp57015 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50049)
%cmp$cmp57015 = icmp eq i64 %truthy$cmp57015, 1
br i1 %cmp$cmp57015, label %truebranch$cmp57015, label %falsebranch$cmp57015
truebranch$cmp57015:
%ae52014 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55945$k501500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%argslist55945$k501501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls149956, %struct.ScmObj* %argslist55945$k501500)
store volatile %struct.ScmObj* %argslist55945$k501501, %struct.ScmObj** %stackaddr$prim57016, align 8
%stackaddr$prim57017 = alloca %struct.ScmObj*, align 8
%argslist55945$k501502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52014, %struct.ScmObj* %argslist55945$k501501)
store volatile %struct.ScmObj* %argslist55945$k501502, %struct.ScmObj** %stackaddr$prim57017, align 8
%clofunc57018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50150)
musttail call tailcc void %clofunc57018(%struct.ScmObj* %k50150, %struct.ScmObj* %argslist55945$k501502)
ret void
falsebranch$cmp57015:
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%anf_45bind50050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls049957)
store volatile %struct.ScmObj* %anf_45bind50050, %struct.ScmObj** %stackaddr$prim57019, align 8
%ae52021 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57020 = alloca %struct.ScmObj*, align 8
%anf_45bind50051 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append49954, %struct.ScmObj* %ae52021)
store volatile %struct.ScmObj* %anf_45bind50051, %struct.ScmObj** %stackaddr$prim57020, align 8
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%anf_45bind50052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls049957)
store volatile %struct.ScmObj* %anf_45bind50052, %struct.ScmObj** %stackaddr$prim57021, align 8
%stackaddr$makeclosure57022 = alloca %struct.ScmObj*, align 8
%fptrToInt57023 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52024 to i64
%ae52024 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57023)
store volatile %struct.ScmObj* %ae52024, %struct.ScmObj** %stackaddr$makeclosure57022, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52024, %struct.ScmObj* %anf_45bind50050, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52024, %struct.ScmObj* %k50150, i64 1)
%argslist55950$anf_45bind500510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%argslist55950$anf_45bind500511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls149956, %struct.ScmObj* %argslist55950$anf_45bind500510)
store volatile %struct.ScmObj* %argslist55950$anf_45bind500511, %struct.ScmObj** %stackaddr$prim57024, align 8
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%argslist55950$anf_45bind500512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50052, %struct.ScmObj* %argslist55950$anf_45bind500511)
store volatile %struct.ScmObj* %argslist55950$anf_45bind500512, %struct.ScmObj** %stackaddr$prim57025, align 8
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%argslist55950$anf_45bind500513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52024, %struct.ScmObj* %argslist55950$anf_45bind500512)
store volatile %struct.ScmObj* %argslist55950$anf_45bind500513, %struct.ScmObj** %stackaddr$prim57026, align 8
%clofunc57027 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50051)
musttail call tailcc void %clofunc57027(%struct.ScmObj* %anf_45bind50051, %struct.ScmObj* %argslist55950$anf_45bind500513)
ret void
}

define tailcc void @proc_clo$ae52024(%struct.ScmObj* %env$ae52024,%struct.ScmObj* %current_45args55946) {
%stackaddr$env-ref57028 = alloca %struct.ScmObj*, align 8
%anf_45bind50050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52024, i64 0)
store %struct.ScmObj* %anf_45bind50050, %struct.ScmObj** %stackaddr$env-ref57028
%stackaddr$env-ref57029 = alloca %struct.ScmObj*, align 8
%k50150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52024, i64 1)
store %struct.ScmObj* %k50150, %struct.ScmObj** %stackaddr$env-ref57029
%stackaddr$prim57030 = alloca %struct.ScmObj*, align 8
%_95k50151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55946)
store volatile %struct.ScmObj* %_95k50151, %struct.ScmObj** %stackaddr$prim57030, align 8
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%current_45args55947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55946)
store volatile %struct.ScmObj* %current_45args55947, %struct.ScmObj** %stackaddr$prim57031, align 8
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%anf_45bind50053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55947)
store volatile %struct.ScmObj* %anf_45bind50053, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%cpsprim50152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50050, %struct.ScmObj* %anf_45bind50053)
store volatile %struct.ScmObj* %cpsprim50152, %struct.ScmObj** %stackaddr$prim57033, align 8
%ae52030 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55949$k501500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%argslist55949$k501501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50152, %struct.ScmObj* %argslist55949$k501500)
store volatile %struct.ScmObj* %argslist55949$k501501, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%argslist55949$k501502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52030, %struct.ScmObj* %argslist55949$k501501)
store volatile %struct.ScmObj* %argslist55949$k501502, %struct.ScmObj** %stackaddr$prim57035, align 8
%clofunc57036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50150)
musttail call tailcc void %clofunc57036(%struct.ScmObj* %k50150, %struct.ScmObj* %argslist55949$k501502)
ret void
}

define tailcc void @proc_clo$ae51984(%struct.ScmObj* %env$ae51984,%struct.ScmObj* %current_45args55952) {
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%k50153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55952)
store volatile %struct.ScmObj* %k50153, %struct.ScmObj** %stackaddr$prim57037, align 8
%stackaddr$prim57038 = alloca %struct.ScmObj*, align 8
%current_45args55953 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55952)
store volatile %struct.ScmObj* %current_45args55953, %struct.ScmObj** %stackaddr$prim57038, align 8
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%a49960 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55953)
store volatile %struct.ScmObj* %a49960, %struct.ScmObj** %stackaddr$prim57039, align 8
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%current_45args55954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55953)
store volatile %struct.ScmObj* %current_45args55954, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%b49959 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55954)
store volatile %struct.ScmObj* %b49959, %struct.ScmObj** %stackaddr$prim57041, align 8
%stackaddr$prim57042 = alloca %struct.ScmObj*, align 8
%anf_45bind50048 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a49960, %struct.ScmObj* %b49959)
store volatile %struct.ScmObj* %anf_45bind50048, %struct.ScmObj** %stackaddr$prim57042, align 8
%stackaddr$prim57043 = alloca %struct.ScmObj*, align 8
%cpsprim50154 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind50048)
store volatile %struct.ScmObj* %cpsprim50154, %struct.ScmObj** %stackaddr$prim57043, align 8
%ae51989 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55956$k501530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57044 = alloca %struct.ScmObj*, align 8
%argslist55956$k501531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50154, %struct.ScmObj* %argslist55956$k501530)
store volatile %struct.ScmObj* %argslist55956$k501531, %struct.ScmObj** %stackaddr$prim57044, align 8
%stackaddr$prim57045 = alloca %struct.ScmObj*, align 8
%argslist55956$k501532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51989, %struct.ScmObj* %argslist55956$k501531)
store volatile %struct.ScmObj* %argslist55956$k501532, %struct.ScmObj** %stackaddr$prim57045, align 8
%clofunc57046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50153)
musttail call tailcc void %clofunc57046(%struct.ScmObj* %k50153, %struct.ScmObj* %argslist55956$k501532)
ret void
}

define tailcc void @proc_clo$ae51960(%struct.ScmObj* %env$ae51960,%struct.ScmObj* %current_45args55958) {
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%k50155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55958)
store volatile %struct.ScmObj* %k50155, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%current_45args55959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55958)
store volatile %struct.ScmObj* %current_45args55959, %struct.ScmObj** %stackaddr$prim57048, align 8
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%a49963 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55959)
store volatile %struct.ScmObj* %a49963, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%current_45args55960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55959)
store volatile %struct.ScmObj* %current_45args55960, %struct.ScmObj** %stackaddr$prim57050, align 8
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%b49962 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55960)
store volatile %struct.ScmObj* %b49962, %struct.ScmObj** %stackaddr$prim57051, align 8
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%anf_45bind50047 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a49963, %struct.ScmObj* %b49962)
store volatile %struct.ScmObj* %anf_45bind50047, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%cpsprim50156 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind50047)
store volatile %struct.ScmObj* %cpsprim50156, %struct.ScmObj** %stackaddr$prim57053, align 8
%ae51965 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55962$k501550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%argslist55962$k501551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50156, %struct.ScmObj* %argslist55962$k501550)
store volatile %struct.ScmObj* %argslist55962$k501551, %struct.ScmObj** %stackaddr$prim57054, align 8
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%argslist55962$k501552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51965, %struct.ScmObj* %argslist55962$k501551)
store volatile %struct.ScmObj* %argslist55962$k501552, %struct.ScmObj** %stackaddr$prim57055, align 8
%clofunc57056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50155)
musttail call tailcc void %clofunc57056(%struct.ScmObj* %k50155, %struct.ScmObj* %argslist55962$k501552)
ret void
}

define tailcc void @proc_clo$ae51566(%struct.ScmObj* %env$ae51566,%struct.ScmObj* %current_45args55965) {
%stackaddr$env-ref57057 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51566, i64 0)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57057
%stackaddr$env-ref57058 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51566, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57058
%stackaddr$env-ref57059 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51566, i64 2)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57059
%stackaddr$prim57060 = alloca %struct.ScmObj*, align 8
%k50157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55965)
store volatile %struct.ScmObj* %k50157, %struct.ScmObj** %stackaddr$prim57060, align 8
%stackaddr$prim57061 = alloca %struct.ScmObj*, align 8
%current_45args55966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55965)
store volatile %struct.ScmObj* %current_45args55966, %struct.ScmObj** %stackaddr$prim57061, align 8
%stackaddr$prim57062 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55966)
store volatile %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$prim57062, align 8
%ae51568 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57063 = alloca %struct.ScmObj*, align 8
%fptrToInt57064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51569 to i64
%ae51569 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57064)
store volatile %struct.ScmObj* %ae51569, %struct.ScmObj** %stackaddr$makeclosure57063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51569, %struct.ScmObj* %_37foldr49887, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51569, %struct.ScmObj* %_37foldl49965, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51569, %struct.ScmObj* %_37foldr149882, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51569, %struct.ScmObj* %_37map149913, i64 3)
%argslist56023$k501570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%argslist56023$k501571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51569, %struct.ScmObj* %argslist56023$k501570)
store volatile %struct.ScmObj* %argslist56023$k501571, %struct.ScmObj** %stackaddr$prim57065, align 8
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%argslist56023$k501572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51568, %struct.ScmObj* %argslist56023$k501571)
store volatile %struct.ScmObj* %argslist56023$k501572, %struct.ScmObj** %stackaddr$prim57066, align 8
%clofunc57067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50157)
musttail call tailcc void %clofunc57067(%struct.ScmObj* %k50157, %struct.ScmObj* %argslist56023$k501572)
ret void
}

define tailcc void @proc_clo$ae51569(%struct.ScmObj* %env$ae51569,%struct.ScmObj* %args4996650158) {
%stackaddr$env-ref57068 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51569, i64 0)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57068
%stackaddr$env-ref57069 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51569, i64 1)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57069
%stackaddr$env-ref57070 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51569, i64 2)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57070
%stackaddr$env-ref57071 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51569, i64 3)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57071
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4996650158)
store volatile %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%args49966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4996650158)
store volatile %struct.ScmObj* %args49966, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args49966)
store volatile %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$prim57074, align 8
%stackaddr$prim57075 = alloca %struct.ScmObj*, align 8
%anf_45bind50035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49966)
store volatile %struct.ScmObj* %anf_45bind50035, %struct.ScmObj** %stackaddr$prim57075, align 8
%stackaddr$prim57076 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50035)
store volatile %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$prim57076, align 8
%stackaddr$prim57077 = alloca %struct.ScmObj*, align 8
%anf_45bind50036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49966)
store volatile %struct.ScmObj* %anf_45bind50036, %struct.ScmObj** %stackaddr$prim57077, align 8
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50036)
store volatile %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$makeclosure57079 = alloca %struct.ScmObj*, align 8
%fptrToInt57080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51577 to i64
%ae51577 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57080)
store volatile %struct.ScmObj* %ae51577, %struct.ScmObj** %stackaddr$makeclosure57079, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %lsts49967, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %_37foldr49887, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %_37foldr149882, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51577, %struct.ScmObj* %_37map149913, i64 7)
%ae51578 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57081 = alloca %struct.ScmObj*, align 8
%fptrToInt57082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51579 to i64
%ae51579 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57082)
store volatile %struct.ScmObj* %ae51579, %struct.ScmObj** %stackaddr$makeclosure57081, align 8
%argslist56022$ae515770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%argslist56022$ae515771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51579, %struct.ScmObj* %argslist56022$ae515770)
store volatile %struct.ScmObj* %argslist56022$ae515771, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%argslist56022$ae515772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51578, %struct.ScmObj* %argslist56022$ae515771)
store volatile %struct.ScmObj* %argslist56022$ae515772, %struct.ScmObj** %stackaddr$prim57084, align 8
%clofunc57085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51577)
musttail call tailcc void %clofunc57085(%struct.ScmObj* %ae51577, %struct.ScmObj* %argslist56022$ae515772)
ret void
}

define tailcc void @proc_clo$ae51577(%struct.ScmObj* %env$ae51577,%struct.ScmObj* %current_45args55968) {
%stackaddr$env-ref57086 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 0)
store %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$env-ref57086
%stackaddr$env-ref57087 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57087
%stackaddr$env-ref57088 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 2)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57088
%stackaddr$env-ref57089 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57089
%stackaddr$env-ref57090 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57090
%stackaddr$env-ref57091 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57091
%stackaddr$env-ref57092 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 6)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57092
%stackaddr$env-ref57093 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51577, i64 7)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57093
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%_95k50160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55968)
store volatile %struct.ScmObj* %_95k50160, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$prim57095 = alloca %struct.ScmObj*, align 8
%current_45args55969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55968)
store volatile %struct.ScmObj* %current_45args55969, %struct.ScmObj** %stackaddr$prim57095, align 8
%stackaddr$prim57096 = alloca %struct.ScmObj*, align 8
%anf_45bind50037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55969)
store volatile %struct.ScmObj* %anf_45bind50037, %struct.ScmObj** %stackaddr$prim57096, align 8
%stackaddr$makeclosure57097 = alloca %struct.ScmObj*, align 8
%fptrToInt57098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51609 to i64
%ae51609 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57098)
store volatile %struct.ScmObj* %ae51609, %struct.ScmObj** %stackaddr$makeclosure57097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %lsts49967, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %_37foldr49887, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %_37map149913, i64 6)
%ae51611 = call %struct.ScmObj* @const_init_false()
%argslist56015$_37foldr1498820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%argslist56015$_37foldr1498821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49967, %struct.ScmObj* %argslist56015$_37foldr1498820)
store volatile %struct.ScmObj* %argslist56015$_37foldr1498821, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%argslist56015$_37foldr1498822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51611, %struct.ScmObj* %argslist56015$_37foldr1498821)
store volatile %struct.ScmObj* %argslist56015$_37foldr1498822, %struct.ScmObj** %stackaddr$prim57100, align 8
%stackaddr$prim57101 = alloca %struct.ScmObj*, align 8
%argslist56015$_37foldr1498823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50037, %struct.ScmObj* %argslist56015$_37foldr1498822)
store volatile %struct.ScmObj* %argslist56015$_37foldr1498823, %struct.ScmObj** %stackaddr$prim57101, align 8
%stackaddr$prim57102 = alloca %struct.ScmObj*, align 8
%argslist56015$_37foldr1498824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51609, %struct.ScmObj* %argslist56015$_37foldr1498823)
store volatile %struct.ScmObj* %argslist56015$_37foldr1498824, %struct.ScmObj** %stackaddr$prim57102, align 8
%clofunc57103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr149882)
musttail call tailcc void %clofunc57103(%struct.ScmObj* %_37foldr149882, %struct.ScmObj* %argslist56015$_37foldr1498824)
ret void
}

define tailcc void @proc_clo$ae51609(%struct.ScmObj* %env$ae51609,%struct.ScmObj* %current_45args55971) {
%stackaddr$env-ref57104 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 0)
store %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$env-ref57104
%stackaddr$env-ref57105 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57105
%stackaddr$env-ref57106 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 2)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57106
%stackaddr$env-ref57107 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57107
%stackaddr$env-ref57108 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57108
%stackaddr$env-ref57109 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57109
%stackaddr$env-ref57110 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 6)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57110
%stackaddr$prim57111 = alloca %struct.ScmObj*, align 8
%_95k50161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55971)
store volatile %struct.ScmObj* %_95k50161, %struct.ScmObj** %stackaddr$prim57111, align 8
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%current_45args55972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55971)
store volatile %struct.ScmObj* %current_45args55972, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%anf_45bind50038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55972)
store volatile %struct.ScmObj* %anf_45bind50038, %struct.ScmObj** %stackaddr$prim57113, align 8
%truthy$cmp57114 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50038)
%cmp$cmp57114 = icmp eq i64 %truthy$cmp57114, 1
br i1 %cmp$cmp57114, label %truebranch$cmp57114, label %falsebranch$cmp57114
truebranch$cmp57114:
%ae51620 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55974$k501590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%argslist55974$k501591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49968, %struct.ScmObj* %argslist55974$k501590)
store volatile %struct.ScmObj* %argslist55974$k501591, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%argslist55974$k501592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51620, %struct.ScmObj* %argslist55974$k501591)
store volatile %struct.ScmObj* %argslist55974$k501592, %struct.ScmObj** %stackaddr$prim57116, align 8
%clofunc57117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50159)
musttail call tailcc void %clofunc57117(%struct.ScmObj* %k50159, %struct.ScmObj* %argslist55974$k501592)
ret void
falsebranch$cmp57114:
%stackaddr$makeclosure57118 = alloca %struct.ScmObj*, align 8
%fptrToInt57119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51625 to i64
%ae51625 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57119)
store volatile %struct.ScmObj* %ae51625, %struct.ScmObj** %stackaddr$makeclosure57118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %lsts49967, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %_37foldr49887, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51625, %struct.ScmObj* %_37map149913, i64 6)
%ae51626 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57120 = alloca %struct.ScmObj*, align 8
%fptrToInt57121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51627 to i64
%ae51627 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57121)
store volatile %struct.ScmObj* %ae51627, %struct.ScmObj** %stackaddr$makeclosure57120, align 8
%argslist56014$ae516250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%argslist56014$ae516251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51627, %struct.ScmObj* %argslist56014$ae516250)
store volatile %struct.ScmObj* %argslist56014$ae516251, %struct.ScmObj** %stackaddr$prim57122, align 8
%stackaddr$prim57123 = alloca %struct.ScmObj*, align 8
%argslist56014$ae516252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51626, %struct.ScmObj* %argslist56014$ae516251)
store volatile %struct.ScmObj* %argslist56014$ae516252, %struct.ScmObj** %stackaddr$prim57123, align 8
%clofunc57124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51625)
musttail call tailcc void %clofunc57124(%struct.ScmObj* %ae51625, %struct.ScmObj* %argslist56014$ae516252)
ret void
}

define tailcc void @proc_clo$ae51625(%struct.ScmObj* %env$ae51625,%struct.ScmObj* %current_45args55975) {
%stackaddr$env-ref57125 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 0)
store %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$env-ref57125
%stackaddr$env-ref57126 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57126
%stackaddr$env-ref57127 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 2)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57127
%stackaddr$env-ref57128 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57128
%stackaddr$env-ref57129 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57129
%stackaddr$env-ref57130 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57130
%stackaddr$env-ref57131 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51625, i64 6)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57131
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%_95k50162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55975)
store volatile %struct.ScmObj* %_95k50162, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%current_45args55976 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55975)
store volatile %struct.ScmObj* %current_45args55976, %struct.ScmObj** %stackaddr$prim57133, align 8
%stackaddr$prim57134 = alloca %struct.ScmObj*, align 8
%anf_45bind50039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55976)
store volatile %struct.ScmObj* %anf_45bind50039, %struct.ScmObj** %stackaddr$prim57134, align 8
%stackaddr$makeclosure57135 = alloca %struct.ScmObj*, align 8
%fptrToInt57136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51646 to i64
%ae51646 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57136)
store volatile %struct.ScmObj* %ae51646, %struct.ScmObj** %stackaddr$makeclosure57135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %lsts49967, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %_37foldr49887, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51646, %struct.ScmObj* %_37map149913, i64 6)
%argslist56009$_37map1499130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%argslist56009$_37map1499131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49967, %struct.ScmObj* %argslist56009$_37map1499130)
store volatile %struct.ScmObj* %argslist56009$_37map1499131, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%argslist56009$_37map1499132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50039, %struct.ScmObj* %argslist56009$_37map1499131)
store volatile %struct.ScmObj* %argslist56009$_37map1499132, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%argslist56009$_37map1499133 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51646, %struct.ScmObj* %argslist56009$_37map1499132)
store volatile %struct.ScmObj* %argslist56009$_37map1499133, %struct.ScmObj** %stackaddr$prim57139, align 8
%clofunc57140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map149913)
musttail call tailcc void %clofunc57140(%struct.ScmObj* %_37map149913, %struct.ScmObj* %argslist56009$_37map1499133)
ret void
}

define tailcc void @proc_clo$ae51646(%struct.ScmObj* %env$ae51646,%struct.ScmObj* %current_45args55978) {
%stackaddr$env-ref57141 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 0)
store %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$env-ref57141
%stackaddr$env-ref57142 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57142
%stackaddr$env-ref57143 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 2)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57143
%stackaddr$env-ref57144 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57144
%stackaddr$env-ref57145 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57145
%stackaddr$env-ref57146 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57146
%stackaddr$env-ref57147 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51646, i64 6)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57147
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%_95k50163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55978)
store volatile %struct.ScmObj* %_95k50163, %struct.ScmObj** %stackaddr$prim57148, align 8
%stackaddr$prim57149 = alloca %struct.ScmObj*, align 8
%current_45args55979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55978)
store volatile %struct.ScmObj* %current_45args55979, %struct.ScmObj** %stackaddr$prim57149, align 8
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55979)
store volatile %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$makeclosure57151 = alloca %struct.ScmObj*, align 8
%fptrToInt57152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51649 to i64
%ae51649 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57152)
store volatile %struct.ScmObj* %ae51649, %struct.ScmObj** %stackaddr$makeclosure57151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %lsts49967, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %_37foldr49887, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %_37map149913, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51649, %struct.ScmObj* %lsts_4349974, i64 7)
%ae51650 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57153 = alloca %struct.ScmObj*, align 8
%fptrToInt57154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51651 to i64
%ae51651 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57154)
store volatile %struct.ScmObj* %ae51651, %struct.ScmObj** %stackaddr$makeclosure57153, align 8
%argslist56008$ae516490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%argslist56008$ae516491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51651, %struct.ScmObj* %argslist56008$ae516490)
store volatile %struct.ScmObj* %argslist56008$ae516491, %struct.ScmObj** %stackaddr$prim57155, align 8
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%argslist56008$ae516492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51650, %struct.ScmObj* %argslist56008$ae516491)
store volatile %struct.ScmObj* %argslist56008$ae516492, %struct.ScmObj** %stackaddr$prim57156, align 8
%clofunc57157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51649)
musttail call tailcc void %clofunc57157(%struct.ScmObj* %ae51649, %struct.ScmObj* %argslist56008$ae516492)
ret void
}

define tailcc void @proc_clo$ae51649(%struct.ScmObj* %env$ae51649,%struct.ScmObj* %current_45args55981) {
%stackaddr$env-ref57158 = alloca %struct.ScmObj*, align 8
%lsts49967 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 0)
store %struct.ScmObj* %lsts49967, %struct.ScmObj** %stackaddr$env-ref57158
%stackaddr$env-ref57159 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57159
%stackaddr$env-ref57160 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 2)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57160
%stackaddr$env-ref57161 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57161
%stackaddr$env-ref57162 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57162
%stackaddr$env-ref57163 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57163
%stackaddr$env-ref57164 = alloca %struct.ScmObj*, align 8
%_37map149913 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 6)
store %struct.ScmObj* %_37map149913, %struct.ScmObj** %stackaddr$env-ref57164
%stackaddr$env-ref57165 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51649, i64 7)
store %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$env-ref57165
%stackaddr$prim57166 = alloca %struct.ScmObj*, align 8
%_95k50164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55981)
store volatile %struct.ScmObj* %_95k50164, %struct.ScmObj** %stackaddr$prim57166, align 8
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%current_45args55982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55981)
store volatile %struct.ScmObj* %current_45args55982, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%anf_45bind50040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55982)
store volatile %struct.ScmObj* %anf_45bind50040, %struct.ScmObj** %stackaddr$prim57168, align 8
%stackaddr$makeclosure57169 = alloca %struct.ScmObj*, align 8
%fptrToInt57170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51670 to i64
%ae51670 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57170)
store volatile %struct.ScmObj* %ae51670, %struct.ScmObj** %stackaddr$makeclosure57169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %k50159, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %_37foldr49887, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %f49969, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %acc49968, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %_37foldl49965, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51670, %struct.ScmObj* %lsts_4349974, i64 5)
%argslist56003$_37map1499130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%argslist56003$_37map1499131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49967, %struct.ScmObj* %argslist56003$_37map1499130)
store volatile %struct.ScmObj* %argslist56003$_37map1499131, %struct.ScmObj** %stackaddr$prim57171, align 8
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%argslist56003$_37map1499132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50040, %struct.ScmObj* %argslist56003$_37map1499131)
store volatile %struct.ScmObj* %argslist56003$_37map1499132, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%argslist56003$_37map1499133 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51670, %struct.ScmObj* %argslist56003$_37map1499132)
store volatile %struct.ScmObj* %argslist56003$_37map1499133, %struct.ScmObj** %stackaddr$prim57173, align 8
%clofunc57174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map149913)
musttail call tailcc void %clofunc57174(%struct.ScmObj* %_37map149913, %struct.ScmObj* %argslist56003$_37map1499133)
ret void
}

define tailcc void @proc_clo$ae51670(%struct.ScmObj* %env$ae51670,%struct.ScmObj* %current_45args55984) {
%stackaddr$env-ref57175 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 0)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57175
%stackaddr$env-ref57176 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 1)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57176
%stackaddr$env-ref57177 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 2)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57177
%stackaddr$env-ref57178 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 3)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57178
%stackaddr$env-ref57179 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 4)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57179
%stackaddr$env-ref57180 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51670, i64 5)
store %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$env-ref57180
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%_95k50165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55984)
store volatile %struct.ScmObj* %_95k50165, %struct.ScmObj** %stackaddr$prim57181, align 8
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%current_45args55985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55984)
store volatile %struct.ScmObj* %current_45args55985, %struct.ScmObj** %stackaddr$prim57182, align 8
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%vs49972 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55985)
store volatile %struct.ScmObj* %vs49972, %struct.ScmObj** %stackaddr$prim57183, align 8
%stackaddr$makeclosure57184 = alloca %struct.ScmObj*, align 8
%fptrToInt57185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51673 to i64
%ae51673 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57185)
store volatile %struct.ScmObj* %ae51673, %struct.ScmObj** %stackaddr$makeclosure57184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %k50159, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %_37foldr49887, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %vs49972, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %f49969, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %acc49968, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %_37foldl49965, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %lsts_4349974, i64 6)
%ae51674 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57186 = alloca %struct.ScmObj*, align 8
%fptrToInt57187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51675 to i64
%ae51675 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57187)
store volatile %struct.ScmObj* %ae51675, %struct.ScmObj** %stackaddr$makeclosure57186, align 8
%argslist56002$ae516730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%argslist56002$ae516731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51675, %struct.ScmObj* %argslist56002$ae516730)
store volatile %struct.ScmObj* %argslist56002$ae516731, %struct.ScmObj** %stackaddr$prim57188, align 8
%stackaddr$prim57189 = alloca %struct.ScmObj*, align 8
%argslist56002$ae516732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51674, %struct.ScmObj* %argslist56002$ae516731)
store volatile %struct.ScmObj* %argslist56002$ae516732, %struct.ScmObj** %stackaddr$prim57189, align 8
%clofunc57190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51673)
musttail call tailcc void %clofunc57190(%struct.ScmObj* %ae51673, %struct.ScmObj* %argslist56002$ae516732)
ret void
}

define tailcc void @proc_clo$ae51673(%struct.ScmObj* %env$ae51673,%struct.ScmObj* %current_45args55987) {
%stackaddr$env-ref57191 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 0)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57191
%stackaddr$env-ref57192 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 1)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57192
%stackaddr$env-ref57193 = alloca %struct.ScmObj*, align 8
%vs49972 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 2)
store %struct.ScmObj* %vs49972, %struct.ScmObj** %stackaddr$env-ref57193
%stackaddr$env-ref57194 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 3)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57194
%stackaddr$env-ref57195 = alloca %struct.ScmObj*, align 8
%acc49968 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 4)
store %struct.ScmObj* %acc49968, %struct.ScmObj** %stackaddr$env-ref57195
%stackaddr$env-ref57196 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 5)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57196
%stackaddr$env-ref57197 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 6)
store %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$env-ref57197
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%_95k50166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55987)
store volatile %struct.ScmObj* %_95k50166, %struct.ScmObj** %stackaddr$prim57198, align 8
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%current_45args55988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55987)
store volatile %struct.ScmObj* %current_45args55988, %struct.ScmObj** %stackaddr$prim57199, align 8
%stackaddr$prim57200 = alloca %struct.ScmObj*, align 8
%anf_45bind50041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55988)
store volatile %struct.ScmObj* %anf_45bind50041, %struct.ScmObj** %stackaddr$prim57200, align 8
%ae51696 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57201 = alloca %struct.ScmObj*, align 8
%anf_45bind50042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49968, %struct.ScmObj* %ae51696)
store volatile %struct.ScmObj* %anf_45bind50042, %struct.ScmObj** %stackaddr$prim57201, align 8
%stackaddr$makeclosure57202 = alloca %struct.ScmObj*, align 8
%fptrToInt57203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51698 to i64
%ae51698 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57203)
store volatile %struct.ScmObj* %ae51698, %struct.ScmObj** %stackaddr$makeclosure57202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %f49969, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %_37foldl49965, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51698, %struct.ScmObj* %lsts_4349974, i64 3)
%argslist55996$_37foldr498870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%argslist55996$_37foldr498871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs49972, %struct.ScmObj* %argslist55996$_37foldr498870)
store volatile %struct.ScmObj* %argslist55996$_37foldr498871, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%argslist55996$_37foldr498872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50042, %struct.ScmObj* %argslist55996$_37foldr498871)
store volatile %struct.ScmObj* %argslist55996$_37foldr498872, %struct.ScmObj** %stackaddr$prim57205, align 8
%stackaddr$prim57206 = alloca %struct.ScmObj*, align 8
%argslist55996$_37foldr498873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50041, %struct.ScmObj* %argslist55996$_37foldr498872)
store volatile %struct.ScmObj* %argslist55996$_37foldr498873, %struct.ScmObj** %stackaddr$prim57206, align 8
%stackaddr$prim57207 = alloca %struct.ScmObj*, align 8
%argslist55996$_37foldr498874 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51698, %struct.ScmObj* %argslist55996$_37foldr498873)
store volatile %struct.ScmObj* %argslist55996$_37foldr498874, %struct.ScmObj** %stackaddr$prim57207, align 8
%clofunc57208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr49887)
musttail call tailcc void %clofunc57208(%struct.ScmObj* %_37foldr49887, %struct.ScmObj* %argslist55996$_37foldr498874)
ret void
}

define tailcc void @proc_clo$ae51698(%struct.ScmObj* %env$ae51698,%struct.ScmObj* %current_45args55990) {
%stackaddr$env-ref57209 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 0)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57209
%stackaddr$env-ref57210 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57210
%stackaddr$env-ref57211 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 2)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57211
%stackaddr$env-ref57212 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51698, i64 3)
store %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$env-ref57212
%stackaddr$prim57213 = alloca %struct.ScmObj*, align 8
%_95k50167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55990)
store volatile %struct.ScmObj* %_95k50167, %struct.ScmObj** %stackaddr$prim57213, align 8
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%current_45args55991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55990)
store volatile %struct.ScmObj* %current_45args55991, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%anf_45bind50043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55991)
store volatile %struct.ScmObj* %anf_45bind50043, %struct.ScmObj** %stackaddr$prim57215, align 8
%stackaddr$makeclosure57216 = alloca %struct.ScmObj*, align 8
%fptrToInt57217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51702 to i64
%ae51702 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57217)
store volatile %struct.ScmObj* %ae51702, %struct.ScmObj** %stackaddr$makeclosure57216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51702, %struct.ScmObj* %f49969, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51702, %struct.ScmObj* %k50159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51702, %struct.ScmObj* %_37foldl49965, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51702, %struct.ScmObj* %lsts_4349974, i64 3)
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%cpsargs50170 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51702, %struct.ScmObj* %anf_45bind50043)
store volatile %struct.ScmObj* %cpsargs50170, %struct.ScmObj** %stackaddr$prim57218, align 8
%clofunc57219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49969)
musttail call tailcc void %clofunc57219(%struct.ScmObj* %f49969, %struct.ScmObj* %cpsargs50170)
ret void
}

define tailcc void @proc_clo$ae51702(%struct.ScmObj* %env$ae51702,%struct.ScmObj* %current_45args55993) {
%stackaddr$env-ref57220 = alloca %struct.ScmObj*, align 8
%f49969 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51702, i64 0)
store %struct.ScmObj* %f49969, %struct.ScmObj** %stackaddr$env-ref57220
%stackaddr$env-ref57221 = alloca %struct.ScmObj*, align 8
%k50159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51702, i64 1)
store %struct.ScmObj* %k50159, %struct.ScmObj** %stackaddr$env-ref57221
%stackaddr$env-ref57222 = alloca %struct.ScmObj*, align 8
%_37foldl49965 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51702, i64 2)
store %struct.ScmObj* %_37foldl49965, %struct.ScmObj** %stackaddr$env-ref57222
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%lsts_4349974 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51702, i64 3)
store %struct.ScmObj* %lsts_4349974, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%_95k50168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55993)
store volatile %struct.ScmObj* %_95k50168, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%current_45args55994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55993)
store volatile %struct.ScmObj* %current_45args55994, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%acc_4349976 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55994)
store volatile %struct.ScmObj* %acc_4349976, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%anf_45bind50044 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4349976, %struct.ScmObj* %lsts_4349974)
store volatile %struct.ScmObj* %anf_45bind50044, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%anf_45bind50045 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49969, %struct.ScmObj* %anf_45bind50044)
store volatile %struct.ScmObj* %anf_45bind50045, %struct.ScmObj** %stackaddr$prim57228, align 8
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%cpsargs50169 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50159, %struct.ScmObj* %anf_45bind50045)
store volatile %struct.ScmObj* %cpsargs50169, %struct.ScmObj** %stackaddr$prim57229, align 8
%clofunc57230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl49965)
musttail call tailcc void %clofunc57230(%struct.ScmObj* %_37foldl49965, %struct.ScmObj* %cpsargs50169)
ret void
}

define tailcc void @proc_clo$ae51675(%struct.ScmObj* %env$ae51675,%struct.ScmObj* %current_45args55997) {
%stackaddr$prim57231 = alloca %struct.ScmObj*, align 8
%k50171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55997)
store volatile %struct.ScmObj* %k50171, %struct.ScmObj** %stackaddr$prim57231, align 8
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%current_45args55998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55997)
store volatile %struct.ScmObj* %current_45args55998, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%a49978 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55998)
store volatile %struct.ScmObj* %a49978, %struct.ScmObj** %stackaddr$prim57233, align 8
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%current_45args55999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55998)
store volatile %struct.ScmObj* %current_45args55999, %struct.ScmObj** %stackaddr$prim57234, align 8
%stackaddr$prim57235 = alloca %struct.ScmObj*, align 8
%b49977 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55999)
store volatile %struct.ScmObj* %b49977, %struct.ScmObj** %stackaddr$prim57235, align 8
%stackaddr$prim57236 = alloca %struct.ScmObj*, align 8
%cpsprim50172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a49978, %struct.ScmObj* %b49977)
store volatile %struct.ScmObj* %cpsprim50172, %struct.ScmObj** %stackaddr$prim57236, align 8
%ae51679 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56001$k501710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%argslist56001$k501711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50172, %struct.ScmObj* %argslist56001$k501710)
store volatile %struct.ScmObj* %argslist56001$k501711, %struct.ScmObj** %stackaddr$prim57237, align 8
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%argslist56001$k501712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist56001$k501711)
store volatile %struct.ScmObj* %argslist56001$k501712, %struct.ScmObj** %stackaddr$prim57238, align 8
%clofunc57239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50171)
musttail call tailcc void %clofunc57239(%struct.ScmObj* %k50171, %struct.ScmObj* %argslist56001$k501712)
ret void
}

define tailcc void @proc_clo$ae51651(%struct.ScmObj* %env$ae51651,%struct.ScmObj* %current_45args56004) {
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%k50173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56004)
store volatile %struct.ScmObj* %k50173, %struct.ScmObj** %stackaddr$prim57240, align 8
%stackaddr$prim57241 = alloca %struct.ScmObj*, align 8
%current_45args56005 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56004)
store volatile %struct.ScmObj* %current_45args56005, %struct.ScmObj** %stackaddr$prim57241, align 8
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%x49973 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56005)
store volatile %struct.ScmObj* %x49973, %struct.ScmObj** %stackaddr$prim57242, align 8
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%cpsprim50174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x49973)
store volatile %struct.ScmObj* %cpsprim50174, %struct.ScmObj** %stackaddr$prim57243, align 8
%ae51654 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56007$k501730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%argslist56007$k501731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50174, %struct.ScmObj* %argslist56007$k501730)
store volatile %struct.ScmObj* %argslist56007$k501731, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$prim57245 = alloca %struct.ScmObj*, align 8
%argslist56007$k501732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51654, %struct.ScmObj* %argslist56007$k501731)
store volatile %struct.ScmObj* %argslist56007$k501732, %struct.ScmObj** %stackaddr$prim57245, align 8
%clofunc57246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50173)
musttail call tailcc void %clofunc57246(%struct.ScmObj* %k50173, %struct.ScmObj* %argslist56007$k501732)
ret void
}

define tailcc void @proc_clo$ae51627(%struct.ScmObj* %env$ae51627,%struct.ScmObj* %current_45args56010) {
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%k50175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56010)
store volatile %struct.ScmObj* %k50175, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%current_45args56011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56010)
store volatile %struct.ScmObj* %current_45args56011, %struct.ScmObj** %stackaddr$prim57248, align 8
%stackaddr$prim57249 = alloca %struct.ScmObj*, align 8
%x49975 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56011)
store volatile %struct.ScmObj* %x49975, %struct.ScmObj** %stackaddr$prim57249, align 8
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%cpsprim50176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x49975)
store volatile %struct.ScmObj* %cpsprim50176, %struct.ScmObj** %stackaddr$prim57250, align 8
%ae51630 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56013$k501750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57251 = alloca %struct.ScmObj*, align 8
%argslist56013$k501751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50176, %struct.ScmObj* %argslist56013$k501750)
store volatile %struct.ScmObj* %argslist56013$k501751, %struct.ScmObj** %stackaddr$prim57251, align 8
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%argslist56013$k501752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51630, %struct.ScmObj* %argslist56013$k501751)
store volatile %struct.ScmObj* %argslist56013$k501752, %struct.ScmObj** %stackaddr$prim57252, align 8
%clofunc57253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50175)
musttail call tailcc void %clofunc57253(%struct.ScmObj* %k50175, %struct.ScmObj* %argslist56013$k501752)
ret void
}

define tailcc void @proc_clo$ae51579(%struct.ScmObj* %env$ae51579,%struct.ScmObj* %current_45args56016) {
%stackaddr$prim57254 = alloca %struct.ScmObj*, align 8
%k50177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56016)
store volatile %struct.ScmObj* %k50177, %struct.ScmObj** %stackaddr$prim57254, align 8
%stackaddr$prim57255 = alloca %struct.ScmObj*, align 8
%current_45args56017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56016)
store volatile %struct.ScmObj* %current_45args56017, %struct.ScmObj** %stackaddr$prim57255, align 8
%stackaddr$prim57256 = alloca %struct.ScmObj*, align 8
%lst49971 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56017)
store volatile %struct.ScmObj* %lst49971, %struct.ScmObj** %stackaddr$prim57256, align 8
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%current_45args56018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56017)
store volatile %struct.ScmObj* %current_45args56018, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%b49970 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56018)
store volatile %struct.ScmObj* %b49970, %struct.ScmObj** %stackaddr$prim57258, align 8
%truthy$cmp57259 = call i64 @is_truthy_value(%struct.ScmObj* %b49970)
%cmp$cmp57259 = icmp eq i64 %truthy$cmp57259, 1
br i1 %cmp$cmp57259, label %truebranch$cmp57259, label %falsebranch$cmp57259
truebranch$cmp57259:
%ae51582 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56020$k501770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57260 = alloca %struct.ScmObj*, align 8
%argslist56020$k501771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b49970, %struct.ScmObj* %argslist56020$k501770)
store volatile %struct.ScmObj* %argslist56020$k501771, %struct.ScmObj** %stackaddr$prim57260, align 8
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%argslist56020$k501772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51582, %struct.ScmObj* %argslist56020$k501771)
store volatile %struct.ScmObj* %argslist56020$k501772, %struct.ScmObj** %stackaddr$prim57261, align 8
%clofunc57262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50177)
musttail call tailcc void %clofunc57262(%struct.ScmObj* %k50177, %struct.ScmObj* %argslist56020$k501772)
ret void
falsebranch$cmp57259:
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%cpsprim50178 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49971)
store volatile %struct.ScmObj* %cpsprim50178, %struct.ScmObj** %stackaddr$prim57263, align 8
%ae51589 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56021$k501770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57264 = alloca %struct.ScmObj*, align 8
%argslist56021$k501771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50178, %struct.ScmObj* %argslist56021$k501770)
store volatile %struct.ScmObj* %argslist56021$k501771, %struct.ScmObj** %stackaddr$prim57264, align 8
%stackaddr$prim57265 = alloca %struct.ScmObj*, align 8
%argslist56021$k501772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51589, %struct.ScmObj* %argslist56021$k501771)
store volatile %struct.ScmObj* %argslist56021$k501772, %struct.ScmObj** %stackaddr$prim57265, align 8
%clofunc57266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50177)
musttail call tailcc void %clofunc57266(%struct.ScmObj* %k50177, %struct.ScmObj* %argslist56021$k501772)
ret void
}

define tailcc void @proc_clo$ae51420(%struct.ScmObj* %env$ae51420,%struct.ScmObj* %args4990950179) {
%stackaddr$env-ref57267 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51420, i64 0)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref57267
%stackaddr$env-ref57268 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51420, i64 1)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57268
%stackaddr$env-ref57269 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51420, i64 2)
store %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$env-ref57269
%stackaddr$prim57270 = alloca %struct.ScmObj*, align 8
%k50180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4990950179)
store volatile %struct.ScmObj* %k50180, %struct.ScmObj** %stackaddr$prim57270, align 8
%stackaddr$prim57271 = alloca %struct.ScmObj*, align 8
%args49909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4990950179)
store volatile %struct.ScmObj* %args49909, %struct.ScmObj** %stackaddr$prim57271, align 8
%stackaddr$prim57272 = alloca %struct.ScmObj*, align 8
%f49911 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args49909)
store volatile %struct.ScmObj* %f49911, %struct.ScmObj** %stackaddr$prim57272, align 8
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%lsts49910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49909)
store volatile %struct.ScmObj* %lsts49910, %struct.ScmObj** %stackaddr$prim57273, align 8
%stackaddr$makeclosure57274 = alloca %struct.ScmObj*, align 8
%fptrToInt57275 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51425 to i64
%ae51425 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57275)
store volatile %struct.ScmObj* %ae51425, %struct.ScmObj** %stackaddr$makeclosure57274, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51425, %struct.ScmObj* %k50180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51425, %struct.ScmObj* %_37foldr49887, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51425, %struct.ScmObj* %lsts49910, i64 2)
%ae51426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57276 = alloca %struct.ScmObj*, align 8
%fptrToInt57277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51427 to i64
%ae51427 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57277)
store volatile %struct.ScmObj* %ae51427, %struct.ScmObj** %stackaddr$makeclosure57276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51427, %struct.ScmObj* %_37last49904, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51427, %struct.ScmObj* %_37drop_45right49901, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51427, %struct.ScmObj* %f49911, i64 2)
%argslist56040$ae514250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%argslist56040$ae514251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51427, %struct.ScmObj* %argslist56040$ae514250)
store volatile %struct.ScmObj* %argslist56040$ae514251, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%argslist56040$ae514252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51426, %struct.ScmObj* %argslist56040$ae514251)
store volatile %struct.ScmObj* %argslist56040$ae514252, %struct.ScmObj** %stackaddr$prim57279, align 8
%clofunc57280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51425)
musttail call tailcc void %clofunc57280(%struct.ScmObj* %ae51425, %struct.ScmObj* %argslist56040$ae514252)
ret void
}

define tailcc void @proc_clo$ae51425(%struct.ScmObj* %env$ae51425,%struct.ScmObj* %current_45args56025) {
%stackaddr$env-ref57281 = alloca %struct.ScmObj*, align 8
%k50180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51425, i64 0)
store %struct.ScmObj* %k50180, %struct.ScmObj** %stackaddr$env-ref57281
%stackaddr$env-ref57282 = alloca %struct.ScmObj*, align 8
%_37foldr49887 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51425, i64 1)
store %struct.ScmObj* %_37foldr49887, %struct.ScmObj** %stackaddr$env-ref57282
%stackaddr$env-ref57283 = alloca %struct.ScmObj*, align 8
%lsts49910 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51425, i64 2)
store %struct.ScmObj* %lsts49910, %struct.ScmObj** %stackaddr$env-ref57283
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%_95k50181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56025)
store volatile %struct.ScmObj* %_95k50181, %struct.ScmObj** %stackaddr$prim57284, align 8
%stackaddr$prim57285 = alloca %struct.ScmObj*, align 8
%current_45args56026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56025)
store volatile %struct.ScmObj* %current_45args56026, %struct.ScmObj** %stackaddr$prim57285, align 8
%stackaddr$prim57286 = alloca %struct.ScmObj*, align 8
%anf_45bind50032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56026)
store volatile %struct.ScmObj* %anf_45bind50032, %struct.ScmObj** %stackaddr$prim57286, align 8
%ae51488 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57287 = alloca %struct.ScmObj*, align 8
%anf_45bind50033 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51488, %struct.ScmObj* %lsts49910)
store volatile %struct.ScmObj* %anf_45bind50033, %struct.ScmObj** %stackaddr$prim57287, align 8
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%anf_45bind50034 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50032, %struct.ScmObj* %anf_45bind50033)
store volatile %struct.ScmObj* %anf_45bind50034, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%cpsargs50182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50180, %struct.ScmObj* %anf_45bind50034)
store volatile %struct.ScmObj* %cpsargs50182, %struct.ScmObj** %stackaddr$prim57289, align 8
%clofunc57290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr49887)
musttail call tailcc void %clofunc57290(%struct.ScmObj* %_37foldr49887, %struct.ScmObj* %cpsargs50182)
ret void
}

define tailcc void @proc_clo$ae51427(%struct.ScmObj* %env$ae51427,%struct.ScmObj* %fargs4991250183) {
%stackaddr$env-ref57291 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51427, i64 0)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref57291
%stackaddr$env-ref57292 = alloca %struct.ScmObj*, align 8
%_37drop_45right49901 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51427, i64 1)
store %struct.ScmObj* %_37drop_45right49901, %struct.ScmObj** %stackaddr$env-ref57292
%stackaddr$env-ref57293 = alloca %struct.ScmObj*, align 8
%f49911 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51427, i64 2)
store %struct.ScmObj* %f49911, %struct.ScmObj** %stackaddr$env-ref57293
%stackaddr$prim57294 = alloca %struct.ScmObj*, align 8
%k50184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4991250183)
store volatile %struct.ScmObj* %k50184, %struct.ScmObj** %stackaddr$prim57294, align 8
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%fargs49912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4991250183)
store volatile %struct.ScmObj* %fargs49912, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$makeclosure57296 = alloca %struct.ScmObj*, align 8
%fptrToInt57297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51431 to i64
%ae51431 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57297)
store volatile %struct.ScmObj* %ae51431, %struct.ScmObj** %stackaddr$makeclosure57296, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51431, %struct.ScmObj* %k50184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51431, %struct.ScmObj* %fargs49912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51431, %struct.ScmObj* %_37last49904, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51431, %struct.ScmObj* %f49911, i64 3)
%ae51433 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56039$_37drop_45right499010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%argslist56039$_37drop_45right499011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51433, %struct.ScmObj* %argslist56039$_37drop_45right499010)
store volatile %struct.ScmObj* %argslist56039$_37drop_45right499011, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%argslist56039$_37drop_45right499012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs49912, %struct.ScmObj* %argslist56039$_37drop_45right499011)
store volatile %struct.ScmObj* %argslist56039$_37drop_45right499012, %struct.ScmObj** %stackaddr$prim57299, align 8
%stackaddr$prim57300 = alloca %struct.ScmObj*, align 8
%argslist56039$_37drop_45right499013 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51431, %struct.ScmObj* %argslist56039$_37drop_45right499012)
store volatile %struct.ScmObj* %argslist56039$_37drop_45right499013, %struct.ScmObj** %stackaddr$prim57300, align 8
%clofunc57301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right49901)
musttail call tailcc void %clofunc57301(%struct.ScmObj* %_37drop_45right49901, %struct.ScmObj* %argslist56039$_37drop_45right499013)
ret void
}

define tailcc void @proc_clo$ae51431(%struct.ScmObj* %env$ae51431,%struct.ScmObj* %current_45args56028) {
%stackaddr$env-ref57302 = alloca %struct.ScmObj*, align 8
%k50184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51431, i64 0)
store %struct.ScmObj* %k50184, %struct.ScmObj** %stackaddr$env-ref57302
%stackaddr$env-ref57303 = alloca %struct.ScmObj*, align 8
%fargs49912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51431, i64 1)
store %struct.ScmObj* %fargs49912, %struct.ScmObj** %stackaddr$env-ref57303
%stackaddr$env-ref57304 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51431, i64 2)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref57304
%stackaddr$env-ref57305 = alloca %struct.ScmObj*, align 8
%f49911 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51431, i64 3)
store %struct.ScmObj* %f49911, %struct.ScmObj** %stackaddr$env-ref57305
%stackaddr$prim57306 = alloca %struct.ScmObj*, align 8
%_95k50185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56028)
store volatile %struct.ScmObj* %_95k50185, %struct.ScmObj** %stackaddr$prim57306, align 8
%stackaddr$prim57307 = alloca %struct.ScmObj*, align 8
%current_45args56029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56028)
store volatile %struct.ScmObj* %current_45args56029, %struct.ScmObj** %stackaddr$prim57307, align 8
%stackaddr$prim57308 = alloca %struct.ScmObj*, align 8
%anf_45bind50029 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56029)
store volatile %struct.ScmObj* %anf_45bind50029, %struct.ScmObj** %stackaddr$prim57308, align 8
%stackaddr$makeclosure57309 = alloca %struct.ScmObj*, align 8
%fptrToInt57310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51438 to i64
%ae51438 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57310)
store volatile %struct.ScmObj* %ae51438, %struct.ScmObj** %stackaddr$makeclosure57309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %k50184, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %fargs49912, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %_37last49904, i64 2)
%stackaddr$prim57311 = alloca %struct.ScmObj*, align 8
%cpsargs50189 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51438, %struct.ScmObj* %anf_45bind50029)
store volatile %struct.ScmObj* %cpsargs50189, %struct.ScmObj** %stackaddr$prim57311, align 8
%clofunc57312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49911)
musttail call tailcc void %clofunc57312(%struct.ScmObj* %f49911, %struct.ScmObj* %cpsargs50189)
ret void
}

define tailcc void @proc_clo$ae51438(%struct.ScmObj* %env$ae51438,%struct.ScmObj* %current_45args56031) {
%stackaddr$env-ref57313 = alloca %struct.ScmObj*, align 8
%k50184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 0)
store %struct.ScmObj* %k50184, %struct.ScmObj** %stackaddr$env-ref57313
%stackaddr$env-ref57314 = alloca %struct.ScmObj*, align 8
%fargs49912 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 1)
store %struct.ScmObj* %fargs49912, %struct.ScmObj** %stackaddr$env-ref57314
%stackaddr$env-ref57315 = alloca %struct.ScmObj*, align 8
%_37last49904 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 2)
store %struct.ScmObj* %_37last49904, %struct.ScmObj** %stackaddr$env-ref57315
%stackaddr$prim57316 = alloca %struct.ScmObj*, align 8
%_95k50186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56031)
store volatile %struct.ScmObj* %_95k50186, %struct.ScmObj** %stackaddr$prim57316, align 8
%stackaddr$prim57317 = alloca %struct.ScmObj*, align 8
%current_45args56032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56031)
store volatile %struct.ScmObj* %current_45args56032, %struct.ScmObj** %stackaddr$prim57317, align 8
%stackaddr$prim57318 = alloca %struct.ScmObj*, align 8
%anf_45bind50030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56032)
store volatile %struct.ScmObj* %anf_45bind50030, %struct.ScmObj** %stackaddr$prim57318, align 8
%stackaddr$makeclosure57319 = alloca %struct.ScmObj*, align 8
%fptrToInt57320 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51443 to i64
%ae51443 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57320)
store volatile %struct.ScmObj* %ae51443, %struct.ScmObj** %stackaddr$makeclosure57319, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51443, %struct.ScmObj* %anf_45bind50030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51443, %struct.ScmObj* %k50184, i64 1)
%argslist56038$_37last499040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%argslist56038$_37last499041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs49912, %struct.ScmObj* %argslist56038$_37last499040)
store volatile %struct.ScmObj* %argslist56038$_37last499041, %struct.ScmObj** %stackaddr$prim57321, align 8
%stackaddr$prim57322 = alloca %struct.ScmObj*, align 8
%argslist56038$_37last499042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51443, %struct.ScmObj* %argslist56038$_37last499041)
store volatile %struct.ScmObj* %argslist56038$_37last499042, %struct.ScmObj** %stackaddr$prim57322, align 8
%clofunc57323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last49904)
musttail call tailcc void %clofunc57323(%struct.ScmObj* %_37last49904, %struct.ScmObj* %argslist56038$_37last499042)
ret void
}

define tailcc void @proc_clo$ae51443(%struct.ScmObj* %env$ae51443,%struct.ScmObj* %current_45args56034) {
%stackaddr$env-ref57324 = alloca %struct.ScmObj*, align 8
%anf_45bind50030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51443, i64 0)
store %struct.ScmObj* %anf_45bind50030, %struct.ScmObj** %stackaddr$env-ref57324
%stackaddr$env-ref57325 = alloca %struct.ScmObj*, align 8
%k50184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51443, i64 1)
store %struct.ScmObj* %k50184, %struct.ScmObj** %stackaddr$env-ref57325
%stackaddr$prim57326 = alloca %struct.ScmObj*, align 8
%_95k50187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56034)
store volatile %struct.ScmObj* %_95k50187, %struct.ScmObj** %stackaddr$prim57326, align 8
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%current_45args56035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56034)
store volatile %struct.ScmObj* %current_45args56035, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$prim57328 = alloca %struct.ScmObj*, align 8
%anf_45bind50031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56035)
store volatile %struct.ScmObj* %anf_45bind50031, %struct.ScmObj** %stackaddr$prim57328, align 8
%stackaddr$prim57329 = alloca %struct.ScmObj*, align 8
%cpsprim50188 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50030, %struct.ScmObj* %anf_45bind50031)
store volatile %struct.ScmObj* %cpsprim50188, %struct.ScmObj** %stackaddr$prim57329, align 8
%ae51448 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56037$k501840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57330 = alloca %struct.ScmObj*, align 8
%argslist56037$k501841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50188, %struct.ScmObj* %argslist56037$k501840)
store volatile %struct.ScmObj* %argslist56037$k501841, %struct.ScmObj** %stackaddr$prim57330, align 8
%stackaddr$prim57331 = alloca %struct.ScmObj*, align 8
%argslist56037$k501842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51448, %struct.ScmObj* %argslist56037$k501841)
store volatile %struct.ScmObj* %argslist56037$k501842, %struct.ScmObj** %stackaddr$prim57331, align 8
%clofunc57332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50184)
musttail call tailcc void %clofunc57332(%struct.ScmObj* %k50184, %struct.ScmObj* %argslist56037$k501842)
ret void
}

define tailcc void @proc_clo$ae51343(%struct.ScmObj* %env$ae51343,%struct.ScmObj* %current_45args56042) {
%stackaddr$env-ref57333 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51343, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57333
%stackaddr$prim57334 = alloca %struct.ScmObj*, align 8
%k50190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56042)
store volatile %struct.ScmObj* %k50190, %struct.ScmObj** %stackaddr$prim57334, align 8
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%current_45args56043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56042)
store volatile %struct.ScmObj* %current_45args56043, %struct.ScmObj** %stackaddr$prim57335, align 8
%stackaddr$prim57336 = alloca %struct.ScmObj*, align 8
%f49915 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56043)
store volatile %struct.ScmObj* %f49915, %struct.ScmObj** %stackaddr$prim57336, align 8
%stackaddr$prim57337 = alloca %struct.ScmObj*, align 8
%current_45args56044 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56043)
store volatile %struct.ScmObj* %current_45args56044, %struct.ScmObj** %stackaddr$prim57337, align 8
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%lst49914 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56044)
store volatile %struct.ScmObj* %lst49914, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$makeclosure57339 = alloca %struct.ScmObj*, align 8
%fptrToInt57340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51344 to i64
%ae51344 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57340)
store volatile %struct.ScmObj* %ae51344, %struct.ScmObj** %stackaddr$makeclosure57339, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51344, %struct.ScmObj* %lst49914, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51344, %struct.ScmObj* %_37foldr149882, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51344, %struct.ScmObj* %k50190, i64 2)
%ae51345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57341 = alloca %struct.ScmObj*, align 8
%fptrToInt57342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51346 to i64
%ae51346 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57342)
store volatile %struct.ScmObj* %ae51346, %struct.ScmObj** %stackaddr$makeclosure57341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51346, %struct.ScmObj* %f49915, i64 0)
%argslist56059$ae513440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57343 = alloca %struct.ScmObj*, align 8
%argslist56059$ae513441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51346, %struct.ScmObj* %argslist56059$ae513440)
store volatile %struct.ScmObj* %argslist56059$ae513441, %struct.ScmObj** %stackaddr$prim57343, align 8
%stackaddr$prim57344 = alloca %struct.ScmObj*, align 8
%argslist56059$ae513442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51345, %struct.ScmObj* %argslist56059$ae513441)
store volatile %struct.ScmObj* %argslist56059$ae513442, %struct.ScmObj** %stackaddr$prim57344, align 8
%clofunc57345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51344)
musttail call tailcc void %clofunc57345(%struct.ScmObj* %ae51344, %struct.ScmObj* %argslist56059$ae513442)
ret void
}

define tailcc void @proc_clo$ae51344(%struct.ScmObj* %env$ae51344,%struct.ScmObj* %current_45args56046) {
%stackaddr$env-ref57346 = alloca %struct.ScmObj*, align 8
%lst49914 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51344, i64 0)
store %struct.ScmObj* %lst49914, %struct.ScmObj** %stackaddr$env-ref57346
%stackaddr$env-ref57347 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51344, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57347
%stackaddr$env-ref57348 = alloca %struct.ScmObj*, align 8
%k50190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51344, i64 2)
store %struct.ScmObj* %k50190, %struct.ScmObj** %stackaddr$env-ref57348
%stackaddr$prim57349 = alloca %struct.ScmObj*, align 8
%_95k50191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %_95k50191, %struct.ScmObj** %stackaddr$prim57349, align 8
%stackaddr$prim57350 = alloca %struct.ScmObj*, align 8
%current_45args56047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56046)
store volatile %struct.ScmObj* %current_45args56047, %struct.ScmObj** %stackaddr$prim57350, align 8
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%anf_45bind50028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56047)
store volatile %struct.ScmObj* %anf_45bind50028, %struct.ScmObj** %stackaddr$prim57351, align 8
%ae51378 = call %struct.ScmObj* @const_init_null()
%argslist56049$_37foldr1498820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57352 = alloca %struct.ScmObj*, align 8
%argslist56049$_37foldr1498821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst49914, %struct.ScmObj* %argslist56049$_37foldr1498820)
store volatile %struct.ScmObj* %argslist56049$_37foldr1498821, %struct.ScmObj** %stackaddr$prim57352, align 8
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%argslist56049$_37foldr1498822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51378, %struct.ScmObj* %argslist56049$_37foldr1498821)
store volatile %struct.ScmObj* %argslist56049$_37foldr1498822, %struct.ScmObj** %stackaddr$prim57353, align 8
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%argslist56049$_37foldr1498823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50028, %struct.ScmObj* %argslist56049$_37foldr1498822)
store volatile %struct.ScmObj* %argslist56049$_37foldr1498823, %struct.ScmObj** %stackaddr$prim57354, align 8
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%argslist56049$_37foldr1498824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50190, %struct.ScmObj* %argslist56049$_37foldr1498823)
store volatile %struct.ScmObj* %argslist56049$_37foldr1498824, %struct.ScmObj** %stackaddr$prim57355, align 8
%clofunc57356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr149882)
musttail call tailcc void %clofunc57356(%struct.ScmObj* %_37foldr149882, %struct.ScmObj* %argslist56049$_37foldr1498824)
ret void
}

define tailcc void @proc_clo$ae51346(%struct.ScmObj* %env$ae51346,%struct.ScmObj* %current_45args56050) {
%stackaddr$env-ref57357 = alloca %struct.ScmObj*, align 8
%f49915 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51346, i64 0)
store %struct.ScmObj* %f49915, %struct.ScmObj** %stackaddr$env-ref57357
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%k50192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56050)
store volatile %struct.ScmObj* %k50192, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%current_45args56051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56050)
store volatile %struct.ScmObj* %current_45args56051, %struct.ScmObj** %stackaddr$prim57359, align 8
%stackaddr$prim57360 = alloca %struct.ScmObj*, align 8
%v49917 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56051)
store volatile %struct.ScmObj* %v49917, %struct.ScmObj** %stackaddr$prim57360, align 8
%stackaddr$prim57361 = alloca %struct.ScmObj*, align 8
%current_45args56052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56051)
store volatile %struct.ScmObj* %current_45args56052, %struct.ScmObj** %stackaddr$prim57361, align 8
%stackaddr$prim57362 = alloca %struct.ScmObj*, align 8
%r49916 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56052)
store volatile %struct.ScmObj* %r49916, %struct.ScmObj** %stackaddr$prim57362, align 8
%stackaddr$makeclosure57363 = alloca %struct.ScmObj*, align 8
%fptrToInt57364 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51348 to i64
%ae51348 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57364)
store volatile %struct.ScmObj* %ae51348, %struct.ScmObj** %stackaddr$makeclosure57363, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51348, %struct.ScmObj* %k50192, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51348, %struct.ScmObj* %r49916, i64 1)
%argslist56058$f499150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%argslist56058$f499151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v49917, %struct.ScmObj* %argslist56058$f499150)
store volatile %struct.ScmObj* %argslist56058$f499151, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%argslist56058$f499152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51348, %struct.ScmObj* %argslist56058$f499151)
store volatile %struct.ScmObj* %argslist56058$f499152, %struct.ScmObj** %stackaddr$prim57366, align 8
%clofunc57367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49915)
musttail call tailcc void %clofunc57367(%struct.ScmObj* %f49915, %struct.ScmObj* %argslist56058$f499152)
ret void
}

define tailcc void @proc_clo$ae51348(%struct.ScmObj* %env$ae51348,%struct.ScmObj* %current_45args56054) {
%stackaddr$env-ref57368 = alloca %struct.ScmObj*, align 8
%k50192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51348, i64 0)
store %struct.ScmObj* %k50192, %struct.ScmObj** %stackaddr$env-ref57368
%stackaddr$env-ref57369 = alloca %struct.ScmObj*, align 8
%r49916 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51348, i64 1)
store %struct.ScmObj* %r49916, %struct.ScmObj** %stackaddr$env-ref57369
%stackaddr$prim57370 = alloca %struct.ScmObj*, align 8
%_95k50193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56054)
store volatile %struct.ScmObj* %_95k50193, %struct.ScmObj** %stackaddr$prim57370, align 8
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%current_45args56055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56054)
store volatile %struct.ScmObj* %current_45args56055, %struct.ScmObj** %stackaddr$prim57371, align 8
%stackaddr$prim57372 = alloca %struct.ScmObj*, align 8
%anf_45bind50027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56055)
store volatile %struct.ScmObj* %anf_45bind50027, %struct.ScmObj** %stackaddr$prim57372, align 8
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%cpsprim50194 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50027, %struct.ScmObj* %r49916)
store volatile %struct.ScmObj* %cpsprim50194, %struct.ScmObj** %stackaddr$prim57373, align 8
%ae51353 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56057$k501920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57374 = alloca %struct.ScmObj*, align 8
%argslist56057$k501921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50194, %struct.ScmObj* %argslist56057$k501920)
store volatile %struct.ScmObj* %argslist56057$k501921, %struct.ScmObj** %stackaddr$prim57374, align 8
%stackaddr$prim57375 = alloca %struct.ScmObj*, align 8
%argslist56057$k501922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51353, %struct.ScmObj* %argslist56057$k501921)
store volatile %struct.ScmObj* %argslist56057$k501922, %struct.ScmObj** %stackaddr$prim57375, align 8
%clofunc57376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50192)
musttail call tailcc void %clofunc57376(%struct.ScmObj* %k50192, %struct.ScmObj* %argslist56057$k501922)
ret void
}

define tailcc void @proc_clo$ae50957(%struct.ScmObj* %env$ae50957,%struct.ScmObj* %current_45args56062) {
%stackaddr$env-ref57377 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50957, i64 0)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57377
%stackaddr$env-ref57378 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50957, i64 1)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57378
%stackaddr$prim57379 = alloca %struct.ScmObj*, align 8
%k50195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56062)
store volatile %struct.ScmObj* %k50195, %struct.ScmObj** %stackaddr$prim57379, align 8
%stackaddr$prim57380 = alloca %struct.ScmObj*, align 8
%current_45args56063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56062)
store volatile %struct.ScmObj* %current_45args56063, %struct.ScmObj** %stackaddr$prim57380, align 8
%stackaddr$prim57381 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56063)
store volatile %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$prim57381, align 8
%ae50959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57382 = alloca %struct.ScmObj*, align 8
%fptrToInt57383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50960 to i64
%ae50960 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57383)
store volatile %struct.ScmObj* %ae50960, %struct.ScmObj** %stackaddr$makeclosure57382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %_37foldr49888, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %_37foldr149882, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %_37map149878, i64 2)
%argslist56120$k501950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57384 = alloca %struct.ScmObj*, align 8
%argslist56120$k501951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50960, %struct.ScmObj* %argslist56120$k501950)
store volatile %struct.ScmObj* %argslist56120$k501951, %struct.ScmObj** %stackaddr$prim57384, align 8
%stackaddr$prim57385 = alloca %struct.ScmObj*, align 8
%argslist56120$k501952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50959, %struct.ScmObj* %argslist56120$k501951)
store volatile %struct.ScmObj* %argslist56120$k501952, %struct.ScmObj** %stackaddr$prim57385, align 8
%clofunc57386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50195)
musttail call tailcc void %clofunc57386(%struct.ScmObj* %k50195, %struct.ScmObj* %argslist56120$k501952)
ret void
}

define tailcc void @proc_clo$ae50960(%struct.ScmObj* %env$ae50960,%struct.ScmObj* %args4988950196) {
%stackaddr$env-ref57387 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 0)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57387
%stackaddr$env-ref57388 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 1)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57388
%stackaddr$env-ref57389 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 2)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57389
%stackaddr$prim57390 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4988950196)
store volatile %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$prim57390, align 8
%stackaddr$prim57391 = alloca %struct.ScmObj*, align 8
%args49889 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4988950196)
store volatile %struct.ScmObj* %args49889, %struct.ScmObj** %stackaddr$prim57391, align 8
%stackaddr$prim57392 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args49889)
store volatile %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$prim57392, align 8
%stackaddr$prim57393 = alloca %struct.ScmObj*, align 8
%anf_45bind50014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49889)
store volatile %struct.ScmObj* %anf_45bind50014, %struct.ScmObj** %stackaddr$prim57393, align 8
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50014)
store volatile %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$prim57394, align 8
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%anf_45bind50015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args49889)
store volatile %struct.ScmObj* %anf_45bind50015, %struct.ScmObj** %stackaddr$prim57395, align 8
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50015)
store volatile %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$prim57396, align 8
%stackaddr$makeclosure57397 = alloca %struct.ScmObj*, align 8
%fptrToInt57398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50968 to i64
%ae50968 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57398)
store volatile %struct.ScmObj* %ae50968, %struct.ScmObj** %stackaddr$makeclosure57397, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %lsts49890, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %_37foldr49888, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %_37foldr149882, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae50968, %struct.ScmObj* %_37map149878, i64 6)
%ae50969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57399 = alloca %struct.ScmObj*, align 8
%fptrToInt57400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50970 to i64
%ae50970 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57400)
store volatile %struct.ScmObj* %ae50970, %struct.ScmObj** %stackaddr$makeclosure57399, align 8
%argslist56119$ae509680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57401 = alloca %struct.ScmObj*, align 8
%argslist56119$ae509681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50970, %struct.ScmObj* %argslist56119$ae509680)
store volatile %struct.ScmObj* %argslist56119$ae509681, %struct.ScmObj** %stackaddr$prim57401, align 8
%stackaddr$prim57402 = alloca %struct.ScmObj*, align 8
%argslist56119$ae509682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50969, %struct.ScmObj* %argslist56119$ae509681)
store volatile %struct.ScmObj* %argslist56119$ae509682, %struct.ScmObj** %stackaddr$prim57402, align 8
%clofunc57403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50968)
musttail call tailcc void %clofunc57403(%struct.ScmObj* %ae50968, %struct.ScmObj* %argslist56119$ae509682)
ret void
}

define tailcc void @proc_clo$ae50968(%struct.ScmObj* %env$ae50968,%struct.ScmObj* %current_45args56065) {
%stackaddr$env-ref57404 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57404
%stackaddr$env-ref57405 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57405
%stackaddr$env-ref57406 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57406
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 3)
store %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$env-ref57408 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 4)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57408
%stackaddr$env-ref57409 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 5)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57409
%stackaddr$env-ref57410 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50968, i64 6)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57410
%stackaddr$prim57411 = alloca %struct.ScmObj*, align 8
%_95k50198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56065)
store volatile %struct.ScmObj* %_95k50198, %struct.ScmObj** %stackaddr$prim57411, align 8
%stackaddr$prim57412 = alloca %struct.ScmObj*, align 8
%current_45args56066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56065)
store volatile %struct.ScmObj* %current_45args56066, %struct.ScmObj** %stackaddr$prim57412, align 8
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%anf_45bind50016 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56066)
store volatile %struct.ScmObj* %anf_45bind50016, %struct.ScmObj** %stackaddr$prim57413, align 8
%stackaddr$makeclosure57414 = alloca %struct.ScmObj*, align 8
%fptrToInt57415 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51000 to i64
%ae51000 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57415)
store volatile %struct.ScmObj* %ae51000, %struct.ScmObj** %stackaddr$makeclosure57414, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %lsts49890, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %_37foldr49888, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %_37foldr149882, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51000, %struct.ScmObj* %_37map149878, i64 6)
%ae51002 = call %struct.ScmObj* @const_init_false()
%argslist56112$_37foldr1498820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57416 = alloca %struct.ScmObj*, align 8
%argslist56112$_37foldr1498821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49890, %struct.ScmObj* %argslist56112$_37foldr1498820)
store volatile %struct.ScmObj* %argslist56112$_37foldr1498821, %struct.ScmObj** %stackaddr$prim57416, align 8
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%argslist56112$_37foldr1498822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51002, %struct.ScmObj* %argslist56112$_37foldr1498821)
store volatile %struct.ScmObj* %argslist56112$_37foldr1498822, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$prim57418 = alloca %struct.ScmObj*, align 8
%argslist56112$_37foldr1498823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50016, %struct.ScmObj* %argslist56112$_37foldr1498822)
store volatile %struct.ScmObj* %argslist56112$_37foldr1498823, %struct.ScmObj** %stackaddr$prim57418, align 8
%stackaddr$prim57419 = alloca %struct.ScmObj*, align 8
%argslist56112$_37foldr1498824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51000, %struct.ScmObj* %argslist56112$_37foldr1498823)
store volatile %struct.ScmObj* %argslist56112$_37foldr1498824, %struct.ScmObj** %stackaddr$prim57419, align 8
%clofunc57420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr149882)
musttail call tailcc void %clofunc57420(%struct.ScmObj* %_37foldr149882, %struct.ScmObj* %argslist56112$_37foldr1498824)
ret void
}

define tailcc void @proc_clo$ae51000(%struct.ScmObj* %env$ae51000,%struct.ScmObj* %current_45args56068) {
%stackaddr$env-ref57421 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57421
%stackaddr$env-ref57422 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57422
%stackaddr$env-ref57423 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57423
%stackaddr$env-ref57424 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 3)
store %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$env-ref57424
%stackaddr$env-ref57425 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 4)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57425
%stackaddr$env-ref57426 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 5)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57426
%stackaddr$env-ref57427 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51000, i64 6)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57427
%stackaddr$prim57428 = alloca %struct.ScmObj*, align 8
%_95k50199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56068)
store volatile %struct.ScmObj* %_95k50199, %struct.ScmObj** %stackaddr$prim57428, align 8
%stackaddr$prim57429 = alloca %struct.ScmObj*, align 8
%current_45args56069 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56068)
store volatile %struct.ScmObj* %current_45args56069, %struct.ScmObj** %stackaddr$prim57429, align 8
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%anf_45bind50017 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56069)
store volatile %struct.ScmObj* %anf_45bind50017, %struct.ScmObj** %stackaddr$prim57430, align 8
%truthy$cmp57431 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50017)
%cmp$cmp57431 = icmp eq i64 %truthy$cmp57431, 1
br i1 %cmp$cmp57431, label %truebranch$cmp57431, label %falsebranch$cmp57431
truebranch$cmp57431:
%ae51011 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56071$k501970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%argslist56071$k501971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49891, %struct.ScmObj* %argslist56071$k501970)
store volatile %struct.ScmObj* %argslist56071$k501971, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%argslist56071$k501972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51011, %struct.ScmObj* %argslist56071$k501971)
store volatile %struct.ScmObj* %argslist56071$k501972, %struct.ScmObj** %stackaddr$prim57433, align 8
%clofunc57434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50197)
musttail call tailcc void %clofunc57434(%struct.ScmObj* %k50197, %struct.ScmObj* %argslist56071$k501972)
ret void
falsebranch$cmp57431:
%stackaddr$makeclosure57435 = alloca %struct.ScmObj*, align 8
%fptrToInt57436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51016 to i64
%ae51016 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57436)
store volatile %struct.ScmObj* %ae51016, %struct.ScmObj** %stackaddr$makeclosure57435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %lsts49890, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %_37foldr49888, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %_37foldr149882, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51016, %struct.ScmObj* %_37map149878, i64 6)
%ae51017 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57437 = alloca %struct.ScmObj*, align 8
%fptrToInt57438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51018 to i64
%ae51018 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57438)
store volatile %struct.ScmObj* %ae51018, %struct.ScmObj** %stackaddr$makeclosure57437, align 8
%argslist56111$ae510160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%argslist56111$ae510161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51018, %struct.ScmObj* %argslist56111$ae510160)
store volatile %struct.ScmObj* %argslist56111$ae510161, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%argslist56111$ae510162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51017, %struct.ScmObj* %argslist56111$ae510161)
store volatile %struct.ScmObj* %argslist56111$ae510162, %struct.ScmObj** %stackaddr$prim57440, align 8
%clofunc57441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51016)
musttail call tailcc void %clofunc57441(%struct.ScmObj* %ae51016, %struct.ScmObj* %argslist56111$ae510162)
ret void
}

define tailcc void @proc_clo$ae51016(%struct.ScmObj* %env$ae51016,%struct.ScmObj* %current_45args56072) {
%stackaddr$env-ref57442 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57442
%stackaddr$env-ref57443 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57443
%stackaddr$env-ref57444 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57444
%stackaddr$env-ref57445 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 3)
store %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$env-ref57445
%stackaddr$env-ref57446 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 4)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57446
%stackaddr$env-ref57447 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 5)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57447
%stackaddr$env-ref57448 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51016, i64 6)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57448
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%_95k50200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56072)
store volatile %struct.ScmObj* %_95k50200, %struct.ScmObj** %stackaddr$prim57449, align 8
%stackaddr$prim57450 = alloca %struct.ScmObj*, align 8
%current_45args56073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56072)
store volatile %struct.ScmObj* %current_45args56073, %struct.ScmObj** %stackaddr$prim57450, align 8
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%anf_45bind50018 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56073)
store volatile %struct.ScmObj* %anf_45bind50018, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$makeclosure57452 = alloca %struct.ScmObj*, align 8
%fptrToInt57453 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51037 to i64
%ae51037 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57453)
store volatile %struct.ScmObj* %ae51037, %struct.ScmObj** %stackaddr$makeclosure57452, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %lsts49890, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %_37foldr49888, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %_37foldr149882, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51037, %struct.ScmObj* %_37map149878, i64 6)
%argslist56106$_37map1498780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%argslist56106$_37map1498781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49890, %struct.ScmObj* %argslist56106$_37map1498780)
store volatile %struct.ScmObj* %argslist56106$_37map1498781, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%argslist56106$_37map1498782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50018, %struct.ScmObj* %argslist56106$_37map1498781)
store volatile %struct.ScmObj* %argslist56106$_37map1498782, %struct.ScmObj** %stackaddr$prim57455, align 8
%stackaddr$prim57456 = alloca %struct.ScmObj*, align 8
%argslist56106$_37map1498783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51037, %struct.ScmObj* %argslist56106$_37map1498782)
store volatile %struct.ScmObj* %argslist56106$_37map1498783, %struct.ScmObj** %stackaddr$prim57456, align 8
%clofunc57457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map149878)
musttail call tailcc void %clofunc57457(%struct.ScmObj* %_37map149878, %struct.ScmObj* %argslist56106$_37map1498783)
ret void
}

define tailcc void @proc_clo$ae51037(%struct.ScmObj* %env$ae51037,%struct.ScmObj* %current_45args56075) {
%stackaddr$env-ref57458 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57458
%stackaddr$env-ref57459 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57459
%stackaddr$env-ref57460 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57460
%stackaddr$env-ref57461 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 3)
store %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$env-ref57461
%stackaddr$env-ref57462 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 4)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57462
%stackaddr$env-ref57463 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 5)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57463
%stackaddr$env-ref57464 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51037, i64 6)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57464
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%_95k50201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56075)
store volatile %struct.ScmObj* %_95k50201, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%current_45args56076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56075)
store volatile %struct.ScmObj* %current_45args56076, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$prim57467 = alloca %struct.ScmObj*, align 8
%lsts_4349897 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56076)
store volatile %struct.ScmObj* %lsts_4349897, %struct.ScmObj** %stackaddr$prim57467, align 8
%stackaddr$makeclosure57468 = alloca %struct.ScmObj*, align 8
%fptrToInt57469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51040 to i64
%ae51040 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57469)
store volatile %struct.ScmObj* %ae51040, %struct.ScmObj** %stackaddr$makeclosure57468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %lsts49890, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %_37foldr49888, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %_37foldr149882, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %lsts_4349897, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51040, %struct.ScmObj* %_37map149878, i64 7)
%ae51041 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57470 = alloca %struct.ScmObj*, align 8
%fptrToInt57471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51042 to i64
%ae51042 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57471)
store volatile %struct.ScmObj* %ae51042, %struct.ScmObj** %stackaddr$makeclosure57470, align 8
%argslist56105$ae510400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57472 = alloca %struct.ScmObj*, align 8
%argslist56105$ae510401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51042, %struct.ScmObj* %argslist56105$ae510400)
store volatile %struct.ScmObj* %argslist56105$ae510401, %struct.ScmObj** %stackaddr$prim57472, align 8
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%argslist56105$ae510402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51041, %struct.ScmObj* %argslist56105$ae510401)
store volatile %struct.ScmObj* %argslist56105$ae510402, %struct.ScmObj** %stackaddr$prim57473, align 8
%clofunc57474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51040)
musttail call tailcc void %clofunc57474(%struct.ScmObj* %ae51040, %struct.ScmObj* %argslist56105$ae510402)
ret void
}

define tailcc void @proc_clo$ae51040(%struct.ScmObj* %env$ae51040,%struct.ScmObj* %current_45args56078) {
%stackaddr$env-ref57475 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57475
%stackaddr$env-ref57476 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57476
%stackaddr$env-ref57477 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57477
%stackaddr$env-ref57478 = alloca %struct.ScmObj*, align 8
%lsts49890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 3)
store %struct.ScmObj* %lsts49890, %struct.ScmObj** %stackaddr$env-ref57478
%stackaddr$env-ref57479 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 4)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57479
%stackaddr$env-ref57480 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 5)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57480
%stackaddr$env-ref57481 = alloca %struct.ScmObj*, align 8
%lsts_4349897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 6)
store %struct.ScmObj* %lsts_4349897, %struct.ScmObj** %stackaddr$env-ref57481
%stackaddr$env-ref57482 = alloca %struct.ScmObj*, align 8
%_37map149878 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51040, i64 7)
store %struct.ScmObj* %_37map149878, %struct.ScmObj** %stackaddr$env-ref57482
%stackaddr$prim57483 = alloca %struct.ScmObj*, align 8
%_95k50202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56078)
store volatile %struct.ScmObj* %_95k50202, %struct.ScmObj** %stackaddr$prim57483, align 8
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%current_45args56079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56078)
store volatile %struct.ScmObj* %current_45args56079, %struct.ScmObj** %stackaddr$prim57484, align 8
%stackaddr$prim57485 = alloca %struct.ScmObj*, align 8
%anf_45bind50019 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56079)
store volatile %struct.ScmObj* %anf_45bind50019, %struct.ScmObj** %stackaddr$prim57485, align 8
%stackaddr$makeclosure57486 = alloca %struct.ScmObj*, align 8
%fptrToInt57487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51061 to i64
%ae51061 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57487)
store volatile %struct.ScmObj* %ae51061, %struct.ScmObj** %stackaddr$makeclosure57486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %_37foldr49888, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %_37foldr149882, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %lsts_4349897, i64 5)
%argslist56100$_37map1498780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%argslist56100$_37map1498781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts49890, %struct.ScmObj* %argslist56100$_37map1498780)
store volatile %struct.ScmObj* %argslist56100$_37map1498781, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%argslist56100$_37map1498782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50019, %struct.ScmObj* %argslist56100$_37map1498781)
store volatile %struct.ScmObj* %argslist56100$_37map1498782, %struct.ScmObj** %stackaddr$prim57489, align 8
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%argslist56100$_37map1498783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51061, %struct.ScmObj* %argslist56100$_37map1498782)
store volatile %struct.ScmObj* %argslist56100$_37map1498783, %struct.ScmObj** %stackaddr$prim57490, align 8
%clofunc57491 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map149878)
musttail call tailcc void %clofunc57491(%struct.ScmObj* %_37map149878, %struct.ScmObj* %argslist56100$_37map1498783)
ret void
}

define tailcc void @proc_clo$ae51061(%struct.ScmObj* %env$ae51061,%struct.ScmObj* %current_45args56081) {
%stackaddr$env-ref57492 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57492
%stackaddr$env-ref57493 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57493
%stackaddr$env-ref57494 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57494
%stackaddr$env-ref57495 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 3)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57495
%stackaddr$env-ref57496 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 4)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57496
%stackaddr$env-ref57497 = alloca %struct.ScmObj*, align 8
%lsts_4349897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 5)
store %struct.ScmObj* %lsts_4349897, %struct.ScmObj** %stackaddr$env-ref57497
%stackaddr$prim57498 = alloca %struct.ScmObj*, align 8
%_95k50203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56081)
store volatile %struct.ScmObj* %_95k50203, %struct.ScmObj** %stackaddr$prim57498, align 8
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%current_45args56082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56081)
store volatile %struct.ScmObj* %current_45args56082, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%vs49895 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56082)
store volatile %struct.ScmObj* %vs49895, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$makeclosure57501 = alloca %struct.ScmObj*, align 8
%fptrToInt57502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51064 to i64
%ae51064 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57502)
store volatile %struct.ScmObj* %ae51064, %struct.ScmObj** %stackaddr$makeclosure57501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %f49892, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %acc49891, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %_37foldr49888, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %_37foldr149882, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %lsts_4349897, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51064, %struct.ScmObj* %vs49895, i64 6)
%ae51065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57503 = alloca %struct.ScmObj*, align 8
%fptrToInt57504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51066 to i64
%ae51066 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57504)
store volatile %struct.ScmObj* %ae51066, %struct.ScmObj** %stackaddr$makeclosure57503, align 8
%argslist56099$ae510640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57505 = alloca %struct.ScmObj*, align 8
%argslist56099$ae510641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51066, %struct.ScmObj* %argslist56099$ae510640)
store volatile %struct.ScmObj* %argslist56099$ae510641, %struct.ScmObj** %stackaddr$prim57505, align 8
%stackaddr$prim57506 = alloca %struct.ScmObj*, align 8
%argslist56099$ae510642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51065, %struct.ScmObj* %argslist56099$ae510641)
store volatile %struct.ScmObj* %argslist56099$ae510642, %struct.ScmObj** %stackaddr$prim57506, align 8
%clofunc57507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51064)
musttail call tailcc void %clofunc57507(%struct.ScmObj* %ae51064, %struct.ScmObj* %argslist56099$ae510642)
ret void
}

define tailcc void @proc_clo$ae51064(%struct.ScmObj* %env$ae51064,%struct.ScmObj* %current_45args56084) {
%stackaddr$env-ref57508 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57508
%stackaddr$env-ref57509 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57509
%stackaddr$env-ref57510 = alloca %struct.ScmObj*, align 8
%acc49891 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 2)
store %struct.ScmObj* %acc49891, %struct.ScmObj** %stackaddr$env-ref57510
%stackaddr$env-ref57511 = alloca %struct.ScmObj*, align 8
%_37foldr49888 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 3)
store %struct.ScmObj* %_37foldr49888, %struct.ScmObj** %stackaddr$env-ref57511
%stackaddr$env-ref57512 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 4)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57512
%stackaddr$env-ref57513 = alloca %struct.ScmObj*, align 8
%lsts_4349897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 5)
store %struct.ScmObj* %lsts_4349897, %struct.ScmObj** %stackaddr$env-ref57513
%stackaddr$env-ref57514 = alloca %struct.ScmObj*, align 8
%vs49895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51064, i64 6)
store %struct.ScmObj* %vs49895, %struct.ScmObj** %stackaddr$env-ref57514
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%_95k50204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56084)
store volatile %struct.ScmObj* %_95k50204, %struct.ScmObj** %stackaddr$prim57515, align 8
%stackaddr$prim57516 = alloca %struct.ScmObj*, align 8
%current_45args56085 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56084)
store volatile %struct.ScmObj* %current_45args56085, %struct.ScmObj** %stackaddr$prim57516, align 8
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%anf_45bind50020 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56085)
store volatile %struct.ScmObj* %anf_45bind50020, %struct.ScmObj** %stackaddr$prim57517, align 8
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%anf_45bind50021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49891, %struct.ScmObj* %lsts_4349897)
store volatile %struct.ScmObj* %anf_45bind50021, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%anf_45bind50022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49892, %struct.ScmObj* %anf_45bind50021)
store volatile %struct.ScmObj* %anf_45bind50022, %struct.ScmObj** %stackaddr$prim57519, align 8
%stackaddr$makeclosure57520 = alloca %struct.ScmObj*, align 8
%fptrToInt57521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51090 to i64
%ae51090 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57521)
store volatile %struct.ScmObj* %ae51090, %struct.ScmObj** %stackaddr$makeclosure57520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51090, %struct.ScmObj* %f49892, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51090, %struct.ScmObj* %anf_45bind50020, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51090, %struct.ScmObj* %k50197, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51090, %struct.ScmObj* %_37foldr149882, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51090, %struct.ScmObj* %vs49895, i64 4)
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%cpsargs50208 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51090, %struct.ScmObj* %anf_45bind50022)
store volatile %struct.ScmObj* %cpsargs50208, %struct.ScmObj** %stackaddr$prim57522, align 8
%clofunc57523 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr49888)
musttail call tailcc void %clofunc57523(%struct.ScmObj* %_37foldr49888, %struct.ScmObj* %cpsargs50208)
ret void
}

define tailcc void @proc_clo$ae51090(%struct.ScmObj* %env$ae51090,%struct.ScmObj* %current_45args56087) {
%stackaddr$env-ref57524 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51090, i64 0)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57524
%stackaddr$env-ref57525 = alloca %struct.ScmObj*, align 8
%anf_45bind50020 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51090, i64 1)
store %struct.ScmObj* %anf_45bind50020, %struct.ScmObj** %stackaddr$env-ref57525
%stackaddr$env-ref57526 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51090, i64 2)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57526
%stackaddr$env-ref57527 = alloca %struct.ScmObj*, align 8
%_37foldr149882 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51090, i64 3)
store %struct.ScmObj* %_37foldr149882, %struct.ScmObj** %stackaddr$env-ref57527
%stackaddr$env-ref57528 = alloca %struct.ScmObj*, align 8
%vs49895 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51090, i64 4)
store %struct.ScmObj* %vs49895, %struct.ScmObj** %stackaddr$env-ref57528
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%_95k50205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56087)
store volatile %struct.ScmObj* %_95k50205, %struct.ScmObj** %stackaddr$prim57529, align 8
%stackaddr$prim57530 = alloca %struct.ScmObj*, align 8
%current_45args56088 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56087)
store volatile %struct.ScmObj* %current_45args56088, %struct.ScmObj** %stackaddr$prim57530, align 8
%stackaddr$prim57531 = alloca %struct.ScmObj*, align 8
%anf_45bind50023 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56088)
store volatile %struct.ScmObj* %anf_45bind50023, %struct.ScmObj** %stackaddr$prim57531, align 8
%ae51095 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57532 = alloca %struct.ScmObj*, align 8
%anf_45bind50024 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50023, %struct.ScmObj* %ae51095)
store volatile %struct.ScmObj* %anf_45bind50024, %struct.ScmObj** %stackaddr$prim57532, align 8
%stackaddr$makeclosure57533 = alloca %struct.ScmObj*, align 8
%fptrToInt57534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51097 to i64
%ae51097 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57534)
store volatile %struct.ScmObj* %ae51097, %struct.ScmObj** %stackaddr$makeclosure57533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51097, %struct.ScmObj* %k50197, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51097, %struct.ScmObj* %f49892, i64 1)
%argslist56093$_37foldr1498820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%argslist56093$_37foldr1498821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs49895, %struct.ScmObj* %argslist56093$_37foldr1498820)
store volatile %struct.ScmObj* %argslist56093$_37foldr1498821, %struct.ScmObj** %stackaddr$prim57535, align 8
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%argslist56093$_37foldr1498822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50024, %struct.ScmObj* %argslist56093$_37foldr1498821)
store volatile %struct.ScmObj* %argslist56093$_37foldr1498822, %struct.ScmObj** %stackaddr$prim57536, align 8
%stackaddr$prim57537 = alloca %struct.ScmObj*, align 8
%argslist56093$_37foldr1498823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50020, %struct.ScmObj* %argslist56093$_37foldr1498822)
store volatile %struct.ScmObj* %argslist56093$_37foldr1498823, %struct.ScmObj** %stackaddr$prim57537, align 8
%stackaddr$prim57538 = alloca %struct.ScmObj*, align 8
%argslist56093$_37foldr1498824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51097, %struct.ScmObj* %argslist56093$_37foldr1498823)
store volatile %struct.ScmObj* %argslist56093$_37foldr1498824, %struct.ScmObj** %stackaddr$prim57538, align 8
%clofunc57539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr149882)
musttail call tailcc void %clofunc57539(%struct.ScmObj* %_37foldr149882, %struct.ScmObj* %argslist56093$_37foldr1498824)
ret void
}

define tailcc void @proc_clo$ae51097(%struct.ScmObj* %env$ae51097,%struct.ScmObj* %current_45args56090) {
%stackaddr$env-ref57540 = alloca %struct.ScmObj*, align 8
%k50197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51097, i64 0)
store %struct.ScmObj* %k50197, %struct.ScmObj** %stackaddr$env-ref57540
%stackaddr$env-ref57541 = alloca %struct.ScmObj*, align 8
%f49892 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51097, i64 1)
store %struct.ScmObj* %f49892, %struct.ScmObj** %stackaddr$env-ref57541
%stackaddr$prim57542 = alloca %struct.ScmObj*, align 8
%_95k50206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56090)
store volatile %struct.ScmObj* %_95k50206, %struct.ScmObj** %stackaddr$prim57542, align 8
%stackaddr$prim57543 = alloca %struct.ScmObj*, align 8
%current_45args56091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56090)
store volatile %struct.ScmObj* %current_45args56091, %struct.ScmObj** %stackaddr$prim57543, align 8
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%anf_45bind50025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56091)
store volatile %struct.ScmObj* %anf_45bind50025, %struct.ScmObj** %stackaddr$prim57544, align 8
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%cpsargs50207 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50197, %struct.ScmObj* %anf_45bind50025)
store volatile %struct.ScmObj* %cpsargs50207, %struct.ScmObj** %stackaddr$prim57545, align 8
%clofunc57546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49892)
musttail call tailcc void %clofunc57546(%struct.ScmObj* %f49892, %struct.ScmObj* %cpsargs50207)
ret void
}

define tailcc void @proc_clo$ae51066(%struct.ScmObj* %env$ae51066,%struct.ScmObj* %current_45args56094) {
%stackaddr$prim57547 = alloca %struct.ScmObj*, align 8
%k50209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56094)
store volatile %struct.ScmObj* %k50209, %struct.ScmObj** %stackaddr$prim57547, align 8
%stackaddr$prim57548 = alloca %struct.ScmObj*, align 8
%current_45args56095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56094)
store volatile %struct.ScmObj* %current_45args56095, %struct.ScmObj** %stackaddr$prim57548, align 8
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%a49900 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56095)
store volatile %struct.ScmObj* %a49900, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%current_45args56096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56095)
store volatile %struct.ScmObj* %current_45args56096, %struct.ScmObj** %stackaddr$prim57550, align 8
%stackaddr$prim57551 = alloca %struct.ScmObj*, align 8
%b49899 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56096)
store volatile %struct.ScmObj* %b49899, %struct.ScmObj** %stackaddr$prim57551, align 8
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%cpsprim50210 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a49900, %struct.ScmObj* %b49899)
store volatile %struct.ScmObj* %cpsprim50210, %struct.ScmObj** %stackaddr$prim57552, align 8
%ae51070 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56098$k502090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%argslist56098$k502091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50210, %struct.ScmObj* %argslist56098$k502090)
store volatile %struct.ScmObj* %argslist56098$k502091, %struct.ScmObj** %stackaddr$prim57553, align 8
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%argslist56098$k502092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51070, %struct.ScmObj* %argslist56098$k502091)
store volatile %struct.ScmObj* %argslist56098$k502092, %struct.ScmObj** %stackaddr$prim57554, align 8
%clofunc57555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50209)
musttail call tailcc void %clofunc57555(%struct.ScmObj* %k50209, %struct.ScmObj* %argslist56098$k502092)
ret void
}

define tailcc void @proc_clo$ae51042(%struct.ScmObj* %env$ae51042,%struct.ScmObj* %current_45args56101) {
%stackaddr$prim57556 = alloca %struct.ScmObj*, align 8
%k50211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56101)
store volatile %struct.ScmObj* %k50211, %struct.ScmObj** %stackaddr$prim57556, align 8
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%current_45args56102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56101)
store volatile %struct.ScmObj* %current_45args56102, %struct.ScmObj** %stackaddr$prim57557, align 8
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%x49896 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56102)
store volatile %struct.ScmObj* %x49896, %struct.ScmObj** %stackaddr$prim57558, align 8
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%cpsprim50212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x49896)
store volatile %struct.ScmObj* %cpsprim50212, %struct.ScmObj** %stackaddr$prim57559, align 8
%ae51045 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56104$k502110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57560 = alloca %struct.ScmObj*, align 8
%argslist56104$k502111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50212, %struct.ScmObj* %argslist56104$k502110)
store volatile %struct.ScmObj* %argslist56104$k502111, %struct.ScmObj** %stackaddr$prim57560, align 8
%stackaddr$prim57561 = alloca %struct.ScmObj*, align 8
%argslist56104$k502112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51045, %struct.ScmObj* %argslist56104$k502111)
store volatile %struct.ScmObj* %argslist56104$k502112, %struct.ScmObj** %stackaddr$prim57561, align 8
%clofunc57562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50211)
musttail call tailcc void %clofunc57562(%struct.ScmObj* %k50211, %struct.ScmObj* %argslist56104$k502112)
ret void
}

define tailcc void @proc_clo$ae51018(%struct.ScmObj* %env$ae51018,%struct.ScmObj* %current_45args56107) {
%stackaddr$prim57563 = alloca %struct.ScmObj*, align 8
%k50213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56107)
store volatile %struct.ScmObj* %k50213, %struct.ScmObj** %stackaddr$prim57563, align 8
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%current_45args56108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56107)
store volatile %struct.ScmObj* %current_45args56108, %struct.ScmObj** %stackaddr$prim57564, align 8
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%x49898 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56108)
store volatile %struct.ScmObj* %x49898, %struct.ScmObj** %stackaddr$prim57565, align 8
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%cpsprim50214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x49898)
store volatile %struct.ScmObj* %cpsprim50214, %struct.ScmObj** %stackaddr$prim57566, align 8
%ae51021 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56110$k502130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57567 = alloca %struct.ScmObj*, align 8
%argslist56110$k502131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50214, %struct.ScmObj* %argslist56110$k502130)
store volatile %struct.ScmObj* %argslist56110$k502131, %struct.ScmObj** %stackaddr$prim57567, align 8
%stackaddr$prim57568 = alloca %struct.ScmObj*, align 8
%argslist56110$k502132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51021, %struct.ScmObj* %argslist56110$k502131)
store volatile %struct.ScmObj* %argslist56110$k502132, %struct.ScmObj** %stackaddr$prim57568, align 8
%clofunc57569 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50213)
musttail call tailcc void %clofunc57569(%struct.ScmObj* %k50213, %struct.ScmObj* %argslist56110$k502132)
ret void
}

define tailcc void @proc_clo$ae50970(%struct.ScmObj* %env$ae50970,%struct.ScmObj* %current_45args56113) {
%stackaddr$prim57570 = alloca %struct.ScmObj*, align 8
%k50215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56113)
store volatile %struct.ScmObj* %k50215, %struct.ScmObj** %stackaddr$prim57570, align 8
%stackaddr$prim57571 = alloca %struct.ScmObj*, align 8
%current_45args56114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56113)
store volatile %struct.ScmObj* %current_45args56114, %struct.ScmObj** %stackaddr$prim57571, align 8
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%lst49894 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56114)
store volatile %struct.ScmObj* %lst49894, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%current_45args56115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56114)
store volatile %struct.ScmObj* %current_45args56115, %struct.ScmObj** %stackaddr$prim57573, align 8
%stackaddr$prim57574 = alloca %struct.ScmObj*, align 8
%b49893 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56115)
store volatile %struct.ScmObj* %b49893, %struct.ScmObj** %stackaddr$prim57574, align 8
%truthy$cmp57575 = call i64 @is_truthy_value(%struct.ScmObj* %b49893)
%cmp$cmp57575 = icmp eq i64 %truthy$cmp57575, 1
br i1 %cmp$cmp57575, label %truebranch$cmp57575, label %falsebranch$cmp57575
truebranch$cmp57575:
%ae50973 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56117$k502150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%argslist56117$k502151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b49893, %struct.ScmObj* %argslist56117$k502150)
store volatile %struct.ScmObj* %argslist56117$k502151, %struct.ScmObj** %stackaddr$prim57576, align 8
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%argslist56117$k502152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50973, %struct.ScmObj* %argslist56117$k502151)
store volatile %struct.ScmObj* %argslist56117$k502152, %struct.ScmObj** %stackaddr$prim57577, align 8
%clofunc57578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50215)
musttail call tailcc void %clofunc57578(%struct.ScmObj* %k50215, %struct.ScmObj* %argslist56117$k502152)
ret void
falsebranch$cmp57575:
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%cpsprim50216 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49894)
store volatile %struct.ScmObj* %cpsprim50216, %struct.ScmObj** %stackaddr$prim57579, align 8
%ae50980 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56118$k502150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57580 = alloca %struct.ScmObj*, align 8
%argslist56118$k502151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50216, %struct.ScmObj* %argslist56118$k502150)
store volatile %struct.ScmObj* %argslist56118$k502151, %struct.ScmObj** %stackaddr$prim57580, align 8
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%argslist56118$k502152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50980, %struct.ScmObj* %argslist56118$k502151)
store volatile %struct.ScmObj* %argslist56118$k502152, %struct.ScmObj** %stackaddr$prim57581, align 8
%clofunc57582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50215)
musttail call tailcc void %clofunc57582(%struct.ScmObj* %k50215, %struct.ScmObj* %argslist56118$k502152)
ret void
}

define tailcc void @proc_clo$ae50927(%struct.ScmObj* %env$ae50927,%struct.ScmObj* %current_45args56122) {
%stackaddr$env-ref57583 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50927, i64 0)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref57583
%stackaddr$env-ref57584 = alloca %struct.ScmObj*, align 8
%_37length49871 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50927, i64 1)
store %struct.ScmObj* %_37length49871, %struct.ScmObj** %stackaddr$env-ref57584
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%k50217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56122)
store volatile %struct.ScmObj* %k50217, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%current_45args56123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56122)
store volatile %struct.ScmObj* %current_45args56123, %struct.ScmObj** %stackaddr$prim57586, align 8
%stackaddr$prim57587 = alloca %struct.ScmObj*, align 8
%lst49903 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56123)
store volatile %struct.ScmObj* %lst49903, %struct.ScmObj** %stackaddr$prim57587, align 8
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%current_45args56124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56123)
store volatile %struct.ScmObj* %current_45args56124, %struct.ScmObj** %stackaddr$prim57588, align 8
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%n49902 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56124)
store volatile %struct.ScmObj* %n49902, %struct.ScmObj** %stackaddr$prim57589, align 8
%stackaddr$makeclosure57590 = alloca %struct.ScmObj*, align 8
%fptrToInt57591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50929 to i64
%ae50929 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57591)
store volatile %struct.ScmObj* %ae50929, %struct.ScmObj** %stackaddr$makeclosure57590, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50929, %struct.ScmObj* %_37take49874, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50929, %struct.ScmObj* %lst49903, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50929, %struct.ScmObj* %n49902, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50929, %struct.ScmObj* %k50217, i64 3)
%argslist56130$_37length498710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57592 = alloca %struct.ScmObj*, align 8
%argslist56130$_37length498711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst49903, %struct.ScmObj* %argslist56130$_37length498710)
store volatile %struct.ScmObj* %argslist56130$_37length498711, %struct.ScmObj** %stackaddr$prim57592, align 8
%stackaddr$prim57593 = alloca %struct.ScmObj*, align 8
%argslist56130$_37length498712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50929, %struct.ScmObj* %argslist56130$_37length498711)
store volatile %struct.ScmObj* %argslist56130$_37length498712, %struct.ScmObj** %stackaddr$prim57593, align 8
%clofunc57594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length49871)
musttail call tailcc void %clofunc57594(%struct.ScmObj* %_37length49871, %struct.ScmObj* %argslist56130$_37length498712)
ret void
}

define tailcc void @proc_clo$ae50929(%struct.ScmObj* %env$ae50929,%struct.ScmObj* %current_45args56126) {
%stackaddr$env-ref57595 = alloca %struct.ScmObj*, align 8
%_37take49874 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50929, i64 0)
store %struct.ScmObj* %_37take49874, %struct.ScmObj** %stackaddr$env-ref57595
%stackaddr$env-ref57596 = alloca %struct.ScmObj*, align 8
%lst49903 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50929, i64 1)
store %struct.ScmObj* %lst49903, %struct.ScmObj** %stackaddr$env-ref57596
%stackaddr$env-ref57597 = alloca %struct.ScmObj*, align 8
%n49902 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50929, i64 2)
store %struct.ScmObj* %n49902, %struct.ScmObj** %stackaddr$env-ref57597
%stackaddr$env-ref57598 = alloca %struct.ScmObj*, align 8
%k50217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50929, i64 3)
store %struct.ScmObj* %k50217, %struct.ScmObj** %stackaddr$env-ref57598
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%_95k50218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56126)
store volatile %struct.ScmObj* %_95k50218, %struct.ScmObj** %stackaddr$prim57599, align 8
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%current_45args56127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56126)
store volatile %struct.ScmObj* %current_45args56127, %struct.ScmObj** %stackaddr$prim57600, align 8
%stackaddr$prim57601 = alloca %struct.ScmObj*, align 8
%anf_45bind50012 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56127)
store volatile %struct.ScmObj* %anf_45bind50012, %struct.ScmObj** %stackaddr$prim57601, align 8
%stackaddr$prim57602 = alloca %struct.ScmObj*, align 8
%anf_45bind50013 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind50012, %struct.ScmObj* %n49902)
store volatile %struct.ScmObj* %anf_45bind50013, %struct.ScmObj** %stackaddr$prim57602, align 8
%argslist56129$_37take498740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57603 = alloca %struct.ScmObj*, align 8
%argslist56129$_37take498741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50013, %struct.ScmObj* %argslist56129$_37take498740)
store volatile %struct.ScmObj* %argslist56129$_37take498741, %struct.ScmObj** %stackaddr$prim57603, align 8
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist56129$_37take498742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst49903, %struct.ScmObj* %argslist56129$_37take498741)
store volatile %struct.ScmObj* %argslist56129$_37take498742, %struct.ScmObj** %stackaddr$prim57604, align 8
%stackaddr$prim57605 = alloca %struct.ScmObj*, align 8
%argslist56129$_37take498743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50217, %struct.ScmObj* %argslist56129$_37take498742)
store volatile %struct.ScmObj* %argslist56129$_37take498743, %struct.ScmObj** %stackaddr$prim57605, align 8
%clofunc57606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take49874)
musttail call tailcc void %clofunc57606(%struct.ScmObj* %_37take49874, %struct.ScmObj* %argslist56129$_37take498743)
ret void
}

define tailcc void @proc_clo$ae50873(%struct.ScmObj* %env$ae50873,%struct.ScmObj* %current_45args56132) {
%stackaddr$env-ref57607 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50873, i64 0)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref57607
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%k50219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56132)
store volatile %struct.ScmObj* %k50219, %struct.ScmObj** %stackaddr$prim57608, align 8
%stackaddr$prim57609 = alloca %struct.ScmObj*, align 8
%current_45args56133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56132)
store volatile %struct.ScmObj* %current_45args56133, %struct.ScmObj** %stackaddr$prim57609, align 8
%stackaddr$prim57610 = alloca %struct.ScmObj*, align 8
%lst49905 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56133)
store volatile %struct.ScmObj* %lst49905, %struct.ScmObj** %stackaddr$prim57610, align 8
%stackaddr$makeclosure57611 = alloca %struct.ScmObj*, align 8
%fptrToInt57612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50874 to i64
%ae50874 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57612)
store volatile %struct.ScmObj* %ae50874, %struct.ScmObj** %stackaddr$makeclosure57611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50874, %struct.ScmObj* %lst49905, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50874, %struct.ScmObj* %k50219, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50874, %struct.ScmObj* %_37foldl149866, i64 2)
%ae50875 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57613 = alloca %struct.ScmObj*, align 8
%fptrToInt57614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50876 to i64
%ae50876 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57614)
store volatile %struct.ScmObj* %ae50876, %struct.ScmObj** %stackaddr$makeclosure57613, align 8
%argslist56144$ae508740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%argslist56144$ae508741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50876, %struct.ScmObj* %argslist56144$ae508740)
store volatile %struct.ScmObj* %argslist56144$ae508741, %struct.ScmObj** %stackaddr$prim57615, align 8
%stackaddr$prim57616 = alloca %struct.ScmObj*, align 8
%argslist56144$ae508742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50875, %struct.ScmObj* %argslist56144$ae508741)
store volatile %struct.ScmObj* %argslist56144$ae508742, %struct.ScmObj** %stackaddr$prim57616, align 8
%clofunc57617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50874)
musttail call tailcc void %clofunc57617(%struct.ScmObj* %ae50874, %struct.ScmObj* %argslist56144$ae508742)
ret void
}

define tailcc void @proc_clo$ae50874(%struct.ScmObj* %env$ae50874,%struct.ScmObj* %current_45args56135) {
%stackaddr$env-ref57618 = alloca %struct.ScmObj*, align 8
%lst49905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50874, i64 0)
store %struct.ScmObj* %lst49905, %struct.ScmObj** %stackaddr$env-ref57618
%stackaddr$env-ref57619 = alloca %struct.ScmObj*, align 8
%k50219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50874, i64 1)
store %struct.ScmObj* %k50219, %struct.ScmObj** %stackaddr$env-ref57619
%stackaddr$env-ref57620 = alloca %struct.ScmObj*, align 8
%_37foldl149866 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50874, i64 2)
store %struct.ScmObj* %_37foldl149866, %struct.ScmObj** %stackaddr$env-ref57620
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%_95k50220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56135)
store volatile %struct.ScmObj* %_95k50220, %struct.ScmObj** %stackaddr$prim57621, align 8
%stackaddr$prim57622 = alloca %struct.ScmObj*, align 8
%current_45args56136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56135)
store volatile %struct.ScmObj* %current_45args56136, %struct.ScmObj** %stackaddr$prim57622, align 8
%stackaddr$prim57623 = alloca %struct.ScmObj*, align 8
%anf_45bind50011 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56136)
store volatile %struct.ScmObj* %anf_45bind50011, %struct.ScmObj** %stackaddr$prim57623, align 8
%ae50895 = call %struct.ScmObj* @const_init_null()
%argslist56138$_37foldl1498660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57624 = alloca %struct.ScmObj*, align 8
%argslist56138$_37foldl1498661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst49905, %struct.ScmObj* %argslist56138$_37foldl1498660)
store volatile %struct.ScmObj* %argslist56138$_37foldl1498661, %struct.ScmObj** %stackaddr$prim57624, align 8
%stackaddr$prim57625 = alloca %struct.ScmObj*, align 8
%argslist56138$_37foldl1498662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50895, %struct.ScmObj* %argslist56138$_37foldl1498661)
store volatile %struct.ScmObj* %argslist56138$_37foldl1498662, %struct.ScmObj** %stackaddr$prim57625, align 8
%stackaddr$prim57626 = alloca %struct.ScmObj*, align 8
%argslist56138$_37foldl1498663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50011, %struct.ScmObj* %argslist56138$_37foldl1498662)
store volatile %struct.ScmObj* %argslist56138$_37foldl1498663, %struct.ScmObj** %stackaddr$prim57626, align 8
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%argslist56138$_37foldl1498664 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50219, %struct.ScmObj* %argslist56138$_37foldl1498663)
store volatile %struct.ScmObj* %argslist56138$_37foldl1498664, %struct.ScmObj** %stackaddr$prim57627, align 8
%clofunc57628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl149866)
musttail call tailcc void %clofunc57628(%struct.ScmObj* %_37foldl149866, %struct.ScmObj* %argslist56138$_37foldl1498664)
ret void
}

define tailcc void @proc_clo$ae50876(%struct.ScmObj* %env$ae50876,%struct.ScmObj* %current_45args56139) {
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%k50221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %k50221, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$prim57630 = alloca %struct.ScmObj*, align 8
%current_45args56140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56139)
store volatile %struct.ScmObj* %current_45args56140, %struct.ScmObj** %stackaddr$prim57630, align 8
%stackaddr$prim57631 = alloca %struct.ScmObj*, align 8
%x49907 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56140)
store volatile %struct.ScmObj* %x49907, %struct.ScmObj** %stackaddr$prim57631, align 8
%stackaddr$prim57632 = alloca %struct.ScmObj*, align 8
%current_45args56141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56140)
store volatile %struct.ScmObj* %current_45args56141, %struct.ScmObj** %stackaddr$prim57632, align 8
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%y49906 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56141)
store volatile %struct.ScmObj* %y49906, %struct.ScmObj** %stackaddr$prim57633, align 8
%ae50878 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56143$k502210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%argslist56143$k502211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x49907, %struct.ScmObj* %argslist56143$k502210)
store volatile %struct.ScmObj* %argslist56143$k502211, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%argslist56143$k502212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50878, %struct.ScmObj* %argslist56143$k502211)
store volatile %struct.ScmObj* %argslist56143$k502212, %struct.ScmObj** %stackaddr$prim57635, align 8
%clofunc57636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50221)
musttail call tailcc void %clofunc57636(%struct.ScmObj* %k50221, %struct.ScmObj* %argslist56143$k502212)
ret void
}

define tailcc void @proc_clo$ae50794(%struct.ScmObj* %env$ae50794,%struct.ScmObj* %current_45args56147) {
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%k50222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56147)
store volatile %struct.ScmObj* %k50222, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%current_45args56148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56147)
store volatile %struct.ScmObj* %current_45args56148, %struct.ScmObj** %stackaddr$prim57638, align 8
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%_37foldl149867 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56148)
store volatile %struct.ScmObj* %_37foldl149867, %struct.ScmObj** %stackaddr$prim57639, align 8
%ae50796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57640 = alloca %struct.ScmObj*, align 8
%fptrToInt57641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50797 to i64
%ae50797 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57641)
store volatile %struct.ScmObj* %ae50797, %struct.ScmObj** %stackaddr$makeclosure57640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50797, %struct.ScmObj* %_37foldl149867, i64 0)
%argslist56161$k502220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%argslist56161$k502221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50797, %struct.ScmObj* %argslist56161$k502220)
store volatile %struct.ScmObj* %argslist56161$k502221, %struct.ScmObj** %stackaddr$prim57642, align 8
%stackaddr$prim57643 = alloca %struct.ScmObj*, align 8
%argslist56161$k502222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50796, %struct.ScmObj* %argslist56161$k502221)
store volatile %struct.ScmObj* %argslist56161$k502222, %struct.ScmObj** %stackaddr$prim57643, align 8
%clofunc57644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50222)
musttail call tailcc void %clofunc57644(%struct.ScmObj* %k50222, %struct.ScmObj* %argslist56161$k502222)
ret void
}

define tailcc void @proc_clo$ae50797(%struct.ScmObj* %env$ae50797,%struct.ScmObj* %current_45args56150) {
%stackaddr$env-ref57645 = alloca %struct.ScmObj*, align 8
%_37foldl149867 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50797, i64 0)
store %struct.ScmObj* %_37foldl149867, %struct.ScmObj** %stackaddr$env-ref57645
%stackaddr$prim57646 = alloca %struct.ScmObj*, align 8
%k50223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56150)
store volatile %struct.ScmObj* %k50223, %struct.ScmObj** %stackaddr$prim57646, align 8
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%current_45args56151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56150)
store volatile %struct.ScmObj* %current_45args56151, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%f49870 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56151)
store volatile %struct.ScmObj* %f49870, %struct.ScmObj** %stackaddr$prim57648, align 8
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%current_45args56152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56151)
store volatile %struct.ScmObj* %current_45args56152, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%acc49869 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56152)
store volatile %struct.ScmObj* %acc49869, %struct.ScmObj** %stackaddr$prim57650, align 8
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%current_45args56153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56152)
store volatile %struct.ScmObj* %current_45args56153, %struct.ScmObj** %stackaddr$prim57651, align 8
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%lst49868 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56153)
store volatile %struct.ScmObj* %lst49868, %struct.ScmObj** %stackaddr$prim57652, align 8
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%anf_45bind50006 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49868)
store volatile %struct.ScmObj* %anf_45bind50006, %struct.ScmObj** %stackaddr$prim57653, align 8
%truthy$cmp57654 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50006)
%cmp$cmp57654 = icmp eq i64 %truthy$cmp57654, 1
br i1 %cmp$cmp57654, label %truebranch$cmp57654, label %falsebranch$cmp57654
truebranch$cmp57654:
%ae50801 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56155$k502230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%argslist56155$k502231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49869, %struct.ScmObj* %argslist56155$k502230)
store volatile %struct.ScmObj* %argslist56155$k502231, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%argslist56155$k502232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50801, %struct.ScmObj* %argslist56155$k502231)
store volatile %struct.ScmObj* %argslist56155$k502232, %struct.ScmObj** %stackaddr$prim57656, align 8
%clofunc57657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50223)
musttail call tailcc void %clofunc57657(%struct.ScmObj* %k50223, %struct.ScmObj* %argslist56155$k502232)
ret void
falsebranch$cmp57654:
%stackaddr$prim57658 = alloca %struct.ScmObj*, align 8
%anf_45bind50007 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst49868)
store volatile %struct.ScmObj* %anf_45bind50007, %struct.ScmObj** %stackaddr$prim57658, align 8
%stackaddr$makeclosure57659 = alloca %struct.ScmObj*, align 8
%fptrToInt57660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50808 to i64
%ae50808 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57660)
store volatile %struct.ScmObj* %ae50808, %struct.ScmObj** %stackaddr$makeclosure57659, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %k50223, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %f49870, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %lst49868, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50808, %struct.ScmObj* %_37foldl149867, i64 3)
%argslist56160$f498700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57661 = alloca %struct.ScmObj*, align 8
%argslist56160$f498701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49869, %struct.ScmObj* %argslist56160$f498700)
store volatile %struct.ScmObj* %argslist56160$f498701, %struct.ScmObj** %stackaddr$prim57661, align 8
%stackaddr$prim57662 = alloca %struct.ScmObj*, align 8
%argslist56160$f498702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50007, %struct.ScmObj* %argslist56160$f498701)
store volatile %struct.ScmObj* %argslist56160$f498702, %struct.ScmObj** %stackaddr$prim57662, align 8
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%argslist56160$f498703 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50808, %struct.ScmObj* %argslist56160$f498702)
store volatile %struct.ScmObj* %argslist56160$f498703, %struct.ScmObj** %stackaddr$prim57663, align 8
%clofunc57664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49870)
musttail call tailcc void %clofunc57664(%struct.ScmObj* %f49870, %struct.ScmObj* %argslist56160$f498703)
ret void
}

define tailcc void @proc_clo$ae50808(%struct.ScmObj* %env$ae50808,%struct.ScmObj* %current_45args56156) {
%stackaddr$env-ref57665 = alloca %struct.ScmObj*, align 8
%k50223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 0)
store %struct.ScmObj* %k50223, %struct.ScmObj** %stackaddr$env-ref57665
%stackaddr$env-ref57666 = alloca %struct.ScmObj*, align 8
%f49870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 1)
store %struct.ScmObj* %f49870, %struct.ScmObj** %stackaddr$env-ref57666
%stackaddr$env-ref57667 = alloca %struct.ScmObj*, align 8
%lst49868 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 2)
store %struct.ScmObj* %lst49868, %struct.ScmObj** %stackaddr$env-ref57667
%stackaddr$env-ref57668 = alloca %struct.ScmObj*, align 8
%_37foldl149867 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50808, i64 3)
store %struct.ScmObj* %_37foldl149867, %struct.ScmObj** %stackaddr$env-ref57668
%stackaddr$prim57669 = alloca %struct.ScmObj*, align 8
%_95k50224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56156)
store volatile %struct.ScmObj* %_95k50224, %struct.ScmObj** %stackaddr$prim57669, align 8
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%current_45args56157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56156)
store volatile %struct.ScmObj* %current_45args56157, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%anf_45bind50008 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56157)
store volatile %struct.ScmObj* %anf_45bind50008, %struct.ScmObj** %stackaddr$prim57671, align 8
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%anf_45bind50009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst49868)
store volatile %struct.ScmObj* %anf_45bind50009, %struct.ScmObj** %stackaddr$prim57672, align 8
%argslist56159$_37foldl1498670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57673 = alloca %struct.ScmObj*, align 8
%argslist56159$_37foldl1498671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50009, %struct.ScmObj* %argslist56159$_37foldl1498670)
store volatile %struct.ScmObj* %argslist56159$_37foldl1498671, %struct.ScmObj** %stackaddr$prim57673, align 8
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%argslist56159$_37foldl1498672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50008, %struct.ScmObj* %argslist56159$_37foldl1498671)
store volatile %struct.ScmObj* %argslist56159$_37foldl1498672, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%argslist56159$_37foldl1498673 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49870, %struct.ScmObj* %argslist56159$_37foldl1498672)
store volatile %struct.ScmObj* %argslist56159$_37foldl1498673, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%argslist56159$_37foldl1498674 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50223, %struct.ScmObj* %argslist56159$_37foldl1498673)
store volatile %struct.ScmObj* %argslist56159$_37foldl1498674, %struct.ScmObj** %stackaddr$prim57676, align 8
%clofunc57677 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl149867)
musttail call tailcc void %clofunc57677(%struct.ScmObj* %_37foldl149867, %struct.ScmObj* %argslist56159$_37foldl1498674)
ret void
}

define tailcc void @proc_clo$ae50711(%struct.ScmObj* %env$ae50711,%struct.ScmObj* %current_45args56164) {
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%k50225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56164)
store volatile %struct.ScmObj* %k50225, %struct.ScmObj** %stackaddr$prim57678, align 8
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%current_45args56165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56164)
store volatile %struct.ScmObj* %current_45args56165, %struct.ScmObj** %stackaddr$prim57679, align 8
%stackaddr$prim57680 = alloca %struct.ScmObj*, align 8
%_37length49872 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56165)
store volatile %struct.ScmObj* %_37length49872, %struct.ScmObj** %stackaddr$prim57680, align 8
%ae50713 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57681 = alloca %struct.ScmObj*, align 8
%fptrToInt57682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50714 to i64
%ae50714 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57682)
store volatile %struct.ScmObj* %ae50714, %struct.ScmObj** %stackaddr$makeclosure57681, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %_37length49872, i64 0)
%argslist56176$k502250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57683 = alloca %struct.ScmObj*, align 8
%argslist56176$k502251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50714, %struct.ScmObj* %argslist56176$k502250)
store volatile %struct.ScmObj* %argslist56176$k502251, %struct.ScmObj** %stackaddr$prim57683, align 8
%stackaddr$prim57684 = alloca %struct.ScmObj*, align 8
%argslist56176$k502252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50713, %struct.ScmObj* %argslist56176$k502251)
store volatile %struct.ScmObj* %argslist56176$k502252, %struct.ScmObj** %stackaddr$prim57684, align 8
%clofunc57685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50225)
musttail call tailcc void %clofunc57685(%struct.ScmObj* %k50225, %struct.ScmObj* %argslist56176$k502252)
ret void
}

define tailcc void @proc_clo$ae50714(%struct.ScmObj* %env$ae50714,%struct.ScmObj* %current_45args56167) {
%stackaddr$env-ref57686 = alloca %struct.ScmObj*, align 8
%_37length49872 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 0)
store %struct.ScmObj* %_37length49872, %struct.ScmObj** %stackaddr$env-ref57686
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%k50226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56167)
store volatile %struct.ScmObj* %k50226, %struct.ScmObj** %stackaddr$prim57687, align 8
%stackaddr$prim57688 = alloca %struct.ScmObj*, align 8
%current_45args56168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56167)
store volatile %struct.ScmObj* %current_45args56168, %struct.ScmObj** %stackaddr$prim57688, align 8
%stackaddr$prim57689 = alloca %struct.ScmObj*, align 8
%lst49873 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56168)
store volatile %struct.ScmObj* %lst49873, %struct.ScmObj** %stackaddr$prim57689, align 8
%stackaddr$prim57690 = alloca %struct.ScmObj*, align 8
%anf_45bind50002 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49873)
store volatile %struct.ScmObj* %anf_45bind50002, %struct.ScmObj** %stackaddr$prim57690, align 8
%truthy$cmp57691 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50002)
%cmp$cmp57691 = icmp eq i64 %truthy$cmp57691, 1
br i1 %cmp$cmp57691, label %truebranch$cmp57691, label %falsebranch$cmp57691
truebranch$cmp57691:
%ae50718 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50719 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56170$k502260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%argslist56170$k502261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50719, %struct.ScmObj* %argslist56170$k502260)
store volatile %struct.ScmObj* %argslist56170$k502261, %struct.ScmObj** %stackaddr$prim57692, align 8
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%argslist56170$k502262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50718, %struct.ScmObj* %argslist56170$k502261)
store volatile %struct.ScmObj* %argslist56170$k502262, %struct.ScmObj** %stackaddr$prim57693, align 8
%clofunc57694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50226)
musttail call tailcc void %clofunc57694(%struct.ScmObj* %k50226, %struct.ScmObj* %argslist56170$k502262)
ret void
falsebranch$cmp57691:
%stackaddr$prim57695 = alloca %struct.ScmObj*, align 8
%anf_45bind50003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst49873)
store volatile %struct.ScmObj* %anf_45bind50003, %struct.ScmObj** %stackaddr$prim57695, align 8
%stackaddr$makeclosure57696 = alloca %struct.ScmObj*, align 8
%fptrToInt57697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50728 to i64
%ae50728 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57697)
store volatile %struct.ScmObj* %ae50728, %struct.ScmObj** %stackaddr$makeclosure57696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50728, %struct.ScmObj* %k50226, i64 0)
%argslist56175$_37length498720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57698 = alloca %struct.ScmObj*, align 8
%argslist56175$_37length498721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50003, %struct.ScmObj* %argslist56175$_37length498720)
store volatile %struct.ScmObj* %argslist56175$_37length498721, %struct.ScmObj** %stackaddr$prim57698, align 8
%stackaddr$prim57699 = alloca %struct.ScmObj*, align 8
%argslist56175$_37length498722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50728, %struct.ScmObj* %argslist56175$_37length498721)
store volatile %struct.ScmObj* %argslist56175$_37length498722, %struct.ScmObj** %stackaddr$prim57699, align 8
%clofunc57700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length49872)
musttail call tailcc void %clofunc57700(%struct.ScmObj* %_37length49872, %struct.ScmObj* %argslist56175$_37length498722)
ret void
}

define tailcc void @proc_clo$ae50728(%struct.ScmObj* %env$ae50728,%struct.ScmObj* %current_45args56171) {
%stackaddr$env-ref57701 = alloca %struct.ScmObj*, align 8
%k50226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50728, i64 0)
store %struct.ScmObj* %k50226, %struct.ScmObj** %stackaddr$env-ref57701
%stackaddr$prim57702 = alloca %struct.ScmObj*, align 8
%_95k50227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56171)
store volatile %struct.ScmObj* %_95k50227, %struct.ScmObj** %stackaddr$prim57702, align 8
%stackaddr$prim57703 = alloca %struct.ScmObj*, align 8
%current_45args56172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56171)
store volatile %struct.ScmObj* %current_45args56172, %struct.ScmObj** %stackaddr$prim57703, align 8
%stackaddr$prim57704 = alloca %struct.ScmObj*, align 8
%anf_45bind50004 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56172)
store volatile %struct.ScmObj* %anf_45bind50004, %struct.ScmObj** %stackaddr$prim57704, align 8
%ae50730 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%cpsprim50228 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae50730, %struct.ScmObj* %anf_45bind50004)
store volatile %struct.ScmObj* %cpsprim50228, %struct.ScmObj** %stackaddr$prim57705, align 8
%ae50733 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56174$k502260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%argslist56174$k502261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50228, %struct.ScmObj* %argslist56174$k502260)
store volatile %struct.ScmObj* %argslist56174$k502261, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%argslist56174$k502262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50733, %struct.ScmObj* %argslist56174$k502261)
store volatile %struct.ScmObj* %argslist56174$k502262, %struct.ScmObj** %stackaddr$prim57707, align 8
%clofunc57708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50226)
musttail call tailcc void %clofunc57708(%struct.ScmObj* %k50226, %struct.ScmObj* %argslist56174$k502262)
ret void
}

define tailcc void @proc_clo$ae50561(%struct.ScmObj* %env$ae50561,%struct.ScmObj* %current_45args56179) {
%stackaddr$prim57709 = alloca %struct.ScmObj*, align 8
%k50229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56179)
store volatile %struct.ScmObj* %k50229, %struct.ScmObj** %stackaddr$prim57709, align 8
%stackaddr$prim57710 = alloca %struct.ScmObj*, align 8
%current_45args56180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56179)
store volatile %struct.ScmObj* %current_45args56180, %struct.ScmObj** %stackaddr$prim57710, align 8
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%_37take49875 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56180)
store volatile %struct.ScmObj* %_37take49875, %struct.ScmObj** %stackaddr$prim57711, align 8
%ae50563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57712 = alloca %struct.ScmObj*, align 8
%fptrToInt57713 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50564 to i64
%ae50564 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57713)
store volatile %struct.ScmObj* %ae50564, %struct.ScmObj** %stackaddr$makeclosure57712, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50564, %struct.ScmObj* %_37take49875, i64 0)
%argslist56193$k502290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57714 = alloca %struct.ScmObj*, align 8
%argslist56193$k502291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50564, %struct.ScmObj* %argslist56193$k502290)
store volatile %struct.ScmObj* %argslist56193$k502291, %struct.ScmObj** %stackaddr$prim57714, align 8
%stackaddr$prim57715 = alloca %struct.ScmObj*, align 8
%argslist56193$k502292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50563, %struct.ScmObj* %argslist56193$k502291)
store volatile %struct.ScmObj* %argslist56193$k502292, %struct.ScmObj** %stackaddr$prim57715, align 8
%clofunc57716 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50229)
musttail call tailcc void %clofunc57716(%struct.ScmObj* %k50229, %struct.ScmObj* %argslist56193$k502292)
ret void
}

define tailcc void @proc_clo$ae50564(%struct.ScmObj* %env$ae50564,%struct.ScmObj* %current_45args56182) {
%stackaddr$env-ref57717 = alloca %struct.ScmObj*, align 8
%_37take49875 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50564, i64 0)
store %struct.ScmObj* %_37take49875, %struct.ScmObj** %stackaddr$env-ref57717
%stackaddr$prim57718 = alloca %struct.ScmObj*, align 8
%k50230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56182)
store volatile %struct.ScmObj* %k50230, %struct.ScmObj** %stackaddr$prim57718, align 8
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%current_45args56183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56182)
store volatile %struct.ScmObj* %current_45args56183, %struct.ScmObj** %stackaddr$prim57719, align 8
%stackaddr$prim57720 = alloca %struct.ScmObj*, align 8
%lst49877 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56183)
store volatile %struct.ScmObj* %lst49877, %struct.ScmObj** %stackaddr$prim57720, align 8
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%current_45args56184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56183)
store volatile %struct.ScmObj* %current_45args56184, %struct.ScmObj** %stackaddr$prim57721, align 8
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%n49876 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %n49876, %struct.ScmObj** %stackaddr$prim57722, align 8
%ae50566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%anf_45bind49995 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n49876, %struct.ScmObj* %ae50566)
store volatile %struct.ScmObj* %anf_45bind49995, %struct.ScmObj** %stackaddr$prim57723, align 8
%truthy$cmp57724 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind49995)
%cmp$cmp57724 = icmp eq i64 %truthy$cmp57724, 1
br i1 %cmp$cmp57724, label %truebranch$cmp57724, label %falsebranch$cmp57724
truebranch$cmp57724:
%ae50569 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50570 = call %struct.ScmObj* @const_init_null()
%argslist56186$k502300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57725 = alloca %struct.ScmObj*, align 8
%argslist56186$k502301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50570, %struct.ScmObj* %argslist56186$k502300)
store volatile %struct.ScmObj* %argslist56186$k502301, %struct.ScmObj** %stackaddr$prim57725, align 8
%stackaddr$prim57726 = alloca %struct.ScmObj*, align 8
%argslist56186$k502302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50569, %struct.ScmObj* %argslist56186$k502301)
store volatile %struct.ScmObj* %argslist56186$k502302, %struct.ScmObj** %stackaddr$prim57726, align 8
%clofunc57727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50230)
musttail call tailcc void %clofunc57727(%struct.ScmObj* %k50230, %struct.ScmObj* %argslist56186$k502302)
ret void
falsebranch$cmp57724:
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%anf_45bind49996 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49877)
store volatile %struct.ScmObj* %anf_45bind49996, %struct.ScmObj** %stackaddr$prim57728, align 8
%truthy$cmp57729 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind49996)
%cmp$cmp57729 = icmp eq i64 %truthy$cmp57729, 1
br i1 %cmp$cmp57729, label %truebranch$cmp57729, label %falsebranch$cmp57729
truebranch$cmp57729:
%ae50580 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50581 = call %struct.ScmObj* @const_init_null()
%argslist56187$k502300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%argslist56187$k502301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50581, %struct.ScmObj* %argslist56187$k502300)
store volatile %struct.ScmObj* %argslist56187$k502301, %struct.ScmObj** %stackaddr$prim57730, align 8
%stackaddr$prim57731 = alloca %struct.ScmObj*, align 8
%argslist56187$k502302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50580, %struct.ScmObj* %argslist56187$k502301)
store volatile %struct.ScmObj* %argslist56187$k502302, %struct.ScmObj** %stackaddr$prim57731, align 8
%clofunc57732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50230)
musttail call tailcc void %clofunc57732(%struct.ScmObj* %k50230, %struct.ScmObj* %argslist56187$k502302)
ret void
falsebranch$cmp57729:
%stackaddr$prim57733 = alloca %struct.ScmObj*, align 8
%anf_45bind49997 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst49877)
store volatile %struct.ScmObj* %anf_45bind49997, %struct.ScmObj** %stackaddr$prim57733, align 8
%stackaddr$prim57734 = alloca %struct.ScmObj*, align 8
%anf_45bind49998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst49877)
store volatile %struct.ScmObj* %anf_45bind49998, %struct.ScmObj** %stackaddr$prim57734, align 8
%ae50591 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57735 = alloca %struct.ScmObj*, align 8
%anf_45bind49999 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n49876, %struct.ScmObj* %ae50591)
store volatile %struct.ScmObj* %anf_45bind49999, %struct.ScmObj** %stackaddr$prim57735, align 8
%stackaddr$makeclosure57736 = alloca %struct.ScmObj*, align 8
%fptrToInt57737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50593 to i64
%ae50593 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57737)
store volatile %struct.ScmObj* %ae50593, %struct.ScmObj** %stackaddr$makeclosure57736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50593, %struct.ScmObj* %anf_45bind49997, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50593, %struct.ScmObj* %k50230, i64 1)
%argslist56192$_37take498750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%argslist56192$_37take498751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49999, %struct.ScmObj* %argslist56192$_37take498750)
store volatile %struct.ScmObj* %argslist56192$_37take498751, %struct.ScmObj** %stackaddr$prim57738, align 8
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%argslist56192$_37take498752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49998, %struct.ScmObj* %argslist56192$_37take498751)
store volatile %struct.ScmObj* %argslist56192$_37take498752, %struct.ScmObj** %stackaddr$prim57739, align 8
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%argslist56192$_37take498753 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50593, %struct.ScmObj* %argslist56192$_37take498752)
store volatile %struct.ScmObj* %argslist56192$_37take498753, %struct.ScmObj** %stackaddr$prim57740, align 8
%clofunc57741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take49875)
musttail call tailcc void %clofunc57741(%struct.ScmObj* %_37take49875, %struct.ScmObj* %argslist56192$_37take498753)
ret void
}

define tailcc void @proc_clo$ae50593(%struct.ScmObj* %env$ae50593,%struct.ScmObj* %current_45args56188) {
%stackaddr$env-ref57742 = alloca %struct.ScmObj*, align 8
%anf_45bind49997 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50593, i64 0)
store %struct.ScmObj* %anf_45bind49997, %struct.ScmObj** %stackaddr$env-ref57742
%stackaddr$env-ref57743 = alloca %struct.ScmObj*, align 8
%k50230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50593, i64 1)
store %struct.ScmObj* %k50230, %struct.ScmObj** %stackaddr$env-ref57743
%stackaddr$prim57744 = alloca %struct.ScmObj*, align 8
%_95k50231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56188)
store volatile %struct.ScmObj* %_95k50231, %struct.ScmObj** %stackaddr$prim57744, align 8
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%current_45args56189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56188)
store volatile %struct.ScmObj* %current_45args56189, %struct.ScmObj** %stackaddr$prim57745, align 8
%stackaddr$prim57746 = alloca %struct.ScmObj*, align 8
%anf_45bind50000 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56189)
store volatile %struct.ScmObj* %anf_45bind50000, %struct.ScmObj** %stackaddr$prim57746, align 8
%stackaddr$prim57747 = alloca %struct.ScmObj*, align 8
%cpsprim50232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49997, %struct.ScmObj* %anf_45bind50000)
store volatile %struct.ScmObj* %cpsprim50232, %struct.ScmObj** %stackaddr$prim57747, align 8
%ae50599 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56191$k502300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57748 = alloca %struct.ScmObj*, align 8
%argslist56191$k502301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50232, %struct.ScmObj* %argslist56191$k502300)
store volatile %struct.ScmObj* %argslist56191$k502301, %struct.ScmObj** %stackaddr$prim57748, align 8
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%argslist56191$k502302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50599, %struct.ScmObj* %argslist56191$k502301)
store volatile %struct.ScmObj* %argslist56191$k502302, %struct.ScmObj** %stackaddr$prim57749, align 8
%clofunc57750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50230)
musttail call tailcc void %clofunc57750(%struct.ScmObj* %k50230, %struct.ScmObj* %argslist56191$k502302)
ret void
}

define tailcc void @proc_clo$ae50464(%struct.ScmObj* %env$ae50464,%struct.ScmObj* %current_45args56196) {
%stackaddr$prim57751 = alloca %struct.ScmObj*, align 8
%k50233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %k50233, %struct.ScmObj** %stackaddr$prim57751, align 8
%stackaddr$prim57752 = alloca %struct.ScmObj*, align 8
%current_45args56197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %current_45args56197, %struct.ScmObj** %stackaddr$prim57752, align 8
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%_37map49879 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56197)
store volatile %struct.ScmObj* %_37map49879, %struct.ScmObj** %stackaddr$prim57753, align 8
%ae50466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57754 = alloca %struct.ScmObj*, align 8
%fptrToInt57755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50467 to i64
%ae50467 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57755)
store volatile %struct.ScmObj* %ae50467, %struct.ScmObj** %stackaddr$makeclosure57754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50467, %struct.ScmObj* %_37map49879, i64 0)
%argslist56213$k502330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57756 = alloca %struct.ScmObj*, align 8
%argslist56213$k502331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50467, %struct.ScmObj* %argslist56213$k502330)
store volatile %struct.ScmObj* %argslist56213$k502331, %struct.ScmObj** %stackaddr$prim57756, align 8
%stackaddr$prim57757 = alloca %struct.ScmObj*, align 8
%argslist56213$k502332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50466, %struct.ScmObj* %argslist56213$k502331)
store volatile %struct.ScmObj* %argslist56213$k502332, %struct.ScmObj** %stackaddr$prim57757, align 8
%clofunc57758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50233)
musttail call tailcc void %clofunc57758(%struct.ScmObj* %k50233, %struct.ScmObj* %argslist56213$k502332)
ret void
}

define tailcc void @proc_clo$ae50467(%struct.ScmObj* %env$ae50467,%struct.ScmObj* %current_45args56199) {
%stackaddr$env-ref57759 = alloca %struct.ScmObj*, align 8
%_37map49879 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50467, i64 0)
store %struct.ScmObj* %_37map49879, %struct.ScmObj** %stackaddr$env-ref57759
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%k50234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56199)
store volatile %struct.ScmObj* %k50234, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%current_45args56200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56199)
store volatile %struct.ScmObj* %current_45args56200, %struct.ScmObj** %stackaddr$prim57761, align 8
%stackaddr$prim57762 = alloca %struct.ScmObj*, align 8
%f49881 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56200)
store volatile %struct.ScmObj* %f49881, %struct.ScmObj** %stackaddr$prim57762, align 8
%stackaddr$prim57763 = alloca %struct.ScmObj*, align 8
%current_45args56201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56200)
store volatile %struct.ScmObj* %current_45args56201, %struct.ScmObj** %stackaddr$prim57763, align 8
%stackaddr$prim57764 = alloca %struct.ScmObj*, align 8
%lst49880 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56201)
store volatile %struct.ScmObj* %lst49880, %struct.ScmObj** %stackaddr$prim57764, align 8
%stackaddr$prim57765 = alloca %struct.ScmObj*, align 8
%anf_45bind49989 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49880)
store volatile %struct.ScmObj* %anf_45bind49989, %struct.ScmObj** %stackaddr$prim57765, align 8
%truthy$cmp57766 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind49989)
%cmp$cmp57766 = icmp eq i64 %truthy$cmp57766, 1
br i1 %cmp$cmp57766, label %truebranch$cmp57766, label %falsebranch$cmp57766
truebranch$cmp57766:
%ae50471 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50472 = call %struct.ScmObj* @const_init_null()
%argslist56203$k502340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57767 = alloca %struct.ScmObj*, align 8
%argslist56203$k502341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50472, %struct.ScmObj* %argslist56203$k502340)
store volatile %struct.ScmObj* %argslist56203$k502341, %struct.ScmObj** %stackaddr$prim57767, align 8
%stackaddr$prim57768 = alloca %struct.ScmObj*, align 8
%argslist56203$k502342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50471, %struct.ScmObj* %argslist56203$k502341)
store volatile %struct.ScmObj* %argslist56203$k502342, %struct.ScmObj** %stackaddr$prim57768, align 8
%clofunc57769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50234)
musttail call tailcc void %clofunc57769(%struct.ScmObj* %k50234, %struct.ScmObj* %argslist56203$k502342)
ret void
falsebranch$cmp57766:
%stackaddr$prim57770 = alloca %struct.ScmObj*, align 8
%anf_45bind49990 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst49880)
store volatile %struct.ScmObj* %anf_45bind49990, %struct.ScmObj** %stackaddr$prim57770, align 8
%stackaddr$makeclosure57771 = alloca %struct.ScmObj*, align 8
%fptrToInt57772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50481 to i64
%ae50481 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57772)
store volatile %struct.ScmObj* %ae50481, %struct.ScmObj** %stackaddr$makeclosure57771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50481, %struct.ScmObj* %k50234, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50481, %struct.ScmObj* %f49881, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50481, %struct.ScmObj* %lst49880, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50481, %struct.ScmObj* %_37map49879, i64 3)
%argslist56212$f498810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%argslist56212$f498811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49990, %struct.ScmObj* %argslist56212$f498810)
store volatile %struct.ScmObj* %argslist56212$f498811, %struct.ScmObj** %stackaddr$prim57773, align 8
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%argslist56212$f498812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50481, %struct.ScmObj* %argslist56212$f498811)
store volatile %struct.ScmObj* %argslist56212$f498812, %struct.ScmObj** %stackaddr$prim57774, align 8
%clofunc57775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49881)
musttail call tailcc void %clofunc57775(%struct.ScmObj* %f49881, %struct.ScmObj* %argslist56212$f498812)
ret void
}

define tailcc void @proc_clo$ae50481(%struct.ScmObj* %env$ae50481,%struct.ScmObj* %current_45args56204) {
%stackaddr$env-ref57776 = alloca %struct.ScmObj*, align 8
%k50234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50481, i64 0)
store %struct.ScmObj* %k50234, %struct.ScmObj** %stackaddr$env-ref57776
%stackaddr$env-ref57777 = alloca %struct.ScmObj*, align 8
%f49881 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50481, i64 1)
store %struct.ScmObj* %f49881, %struct.ScmObj** %stackaddr$env-ref57777
%stackaddr$env-ref57778 = alloca %struct.ScmObj*, align 8
%lst49880 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50481, i64 2)
store %struct.ScmObj* %lst49880, %struct.ScmObj** %stackaddr$env-ref57778
%stackaddr$env-ref57779 = alloca %struct.ScmObj*, align 8
%_37map49879 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50481, i64 3)
store %struct.ScmObj* %_37map49879, %struct.ScmObj** %stackaddr$env-ref57779
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%_95k50235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56204)
store volatile %struct.ScmObj* %_95k50235, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%current_45args56205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56204)
store volatile %struct.ScmObj* %current_45args56205, %struct.ScmObj** %stackaddr$prim57781, align 8
%stackaddr$prim57782 = alloca %struct.ScmObj*, align 8
%anf_45bind49991 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56205)
store volatile %struct.ScmObj* %anf_45bind49991, %struct.ScmObj** %stackaddr$prim57782, align 8
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%anf_45bind49992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst49880)
store volatile %struct.ScmObj* %anf_45bind49992, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$makeclosure57784 = alloca %struct.ScmObj*, align 8
%fptrToInt57785 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50485 to i64
%ae50485 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57785)
store volatile %struct.ScmObj* %ae50485, %struct.ScmObj** %stackaddr$makeclosure57784, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50485, %struct.ScmObj* %k50234, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50485, %struct.ScmObj* %anf_45bind49991, i64 1)
%argslist56211$_37map498790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%argslist56211$_37map498791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49992, %struct.ScmObj* %argslist56211$_37map498790)
store volatile %struct.ScmObj* %argslist56211$_37map498791, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%argslist56211$_37map498792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49881, %struct.ScmObj* %argslist56211$_37map498791)
store volatile %struct.ScmObj* %argslist56211$_37map498792, %struct.ScmObj** %stackaddr$prim57787, align 8
%stackaddr$prim57788 = alloca %struct.ScmObj*, align 8
%argslist56211$_37map498793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50485, %struct.ScmObj* %argslist56211$_37map498792)
store volatile %struct.ScmObj* %argslist56211$_37map498793, %struct.ScmObj** %stackaddr$prim57788, align 8
%clofunc57789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map49879)
musttail call tailcc void %clofunc57789(%struct.ScmObj* %_37map49879, %struct.ScmObj* %argslist56211$_37map498793)
ret void
}

define tailcc void @proc_clo$ae50485(%struct.ScmObj* %env$ae50485,%struct.ScmObj* %current_45args56207) {
%stackaddr$env-ref57790 = alloca %struct.ScmObj*, align 8
%k50234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50485, i64 0)
store %struct.ScmObj* %k50234, %struct.ScmObj** %stackaddr$env-ref57790
%stackaddr$env-ref57791 = alloca %struct.ScmObj*, align 8
%anf_45bind49991 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50485, i64 1)
store %struct.ScmObj* %anf_45bind49991, %struct.ScmObj** %stackaddr$env-ref57791
%stackaddr$prim57792 = alloca %struct.ScmObj*, align 8
%_95k50236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56207)
store volatile %struct.ScmObj* %_95k50236, %struct.ScmObj** %stackaddr$prim57792, align 8
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%current_45args56208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56207)
store volatile %struct.ScmObj* %current_45args56208, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%anf_45bind49993 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56208)
store volatile %struct.ScmObj* %anf_45bind49993, %struct.ScmObj** %stackaddr$prim57794, align 8
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%cpsprim50237 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49991, %struct.ScmObj* %anf_45bind49993)
store volatile %struct.ScmObj* %cpsprim50237, %struct.ScmObj** %stackaddr$prim57795, align 8
%ae50491 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56210$k502340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57796 = alloca %struct.ScmObj*, align 8
%argslist56210$k502341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim50237, %struct.ScmObj* %argslist56210$k502340)
store volatile %struct.ScmObj* %argslist56210$k502341, %struct.ScmObj** %stackaddr$prim57796, align 8
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%argslist56210$k502342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50491, %struct.ScmObj* %argslist56210$k502341)
store volatile %struct.ScmObj* %argslist56210$k502342, %struct.ScmObj** %stackaddr$prim57797, align 8
%clofunc57798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50234)
musttail call tailcc void %clofunc57798(%struct.ScmObj* %k50234, %struct.ScmObj* %argslist56210$k502342)
ret void
}

define tailcc void @proc_clo$ae50384(%struct.ScmObj* %env$ae50384,%struct.ScmObj* %current_45args56216) {
%stackaddr$prim57799 = alloca %struct.ScmObj*, align 8
%k50238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56216)
store volatile %struct.ScmObj* %k50238, %struct.ScmObj** %stackaddr$prim57799, align 8
%stackaddr$prim57800 = alloca %struct.ScmObj*, align 8
%current_45args56217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56216)
store volatile %struct.ScmObj* %current_45args56217, %struct.ScmObj** %stackaddr$prim57800, align 8
%stackaddr$prim57801 = alloca %struct.ScmObj*, align 8
%_37foldr149883 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56217)
store volatile %struct.ScmObj* %_37foldr149883, %struct.ScmObj** %stackaddr$prim57801, align 8
%ae50386 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57802 = alloca %struct.ScmObj*, align 8
%fptrToInt57803 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50387 to i64
%ae50387 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57803)
store volatile %struct.ScmObj* %ae50387, %struct.ScmObj** %stackaddr$makeclosure57802, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50387, %struct.ScmObj* %_37foldr149883, i64 0)
%argslist56230$k502380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%argslist56230$k502381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50387, %struct.ScmObj* %argslist56230$k502380)
store volatile %struct.ScmObj* %argslist56230$k502381, %struct.ScmObj** %stackaddr$prim57804, align 8
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%argslist56230$k502382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50386, %struct.ScmObj* %argslist56230$k502381)
store volatile %struct.ScmObj* %argslist56230$k502382, %struct.ScmObj** %stackaddr$prim57805, align 8
%clofunc57806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50238)
musttail call tailcc void %clofunc57806(%struct.ScmObj* %k50238, %struct.ScmObj* %argslist56230$k502382)
ret void
}

define tailcc void @proc_clo$ae50387(%struct.ScmObj* %env$ae50387,%struct.ScmObj* %current_45args56219) {
%stackaddr$env-ref57807 = alloca %struct.ScmObj*, align 8
%_37foldr149883 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50387, i64 0)
store %struct.ScmObj* %_37foldr149883, %struct.ScmObj** %stackaddr$env-ref57807
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%k50239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56219)
store volatile %struct.ScmObj* %k50239, %struct.ScmObj** %stackaddr$prim57808, align 8
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%current_45args56220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56219)
store volatile %struct.ScmObj* %current_45args56220, %struct.ScmObj** %stackaddr$prim57809, align 8
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%f49886 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %f49886, %struct.ScmObj** %stackaddr$prim57810, align 8
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%current_45args56221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %current_45args56221, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%acc49885 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56221)
store volatile %struct.ScmObj* %acc49885, %struct.ScmObj** %stackaddr$prim57812, align 8
%stackaddr$prim57813 = alloca %struct.ScmObj*, align 8
%current_45args56222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56221)
store volatile %struct.ScmObj* %current_45args56222, %struct.ScmObj** %stackaddr$prim57813, align 8
%stackaddr$prim57814 = alloca %struct.ScmObj*, align 8
%lst49884 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56222)
store volatile %struct.ScmObj* %lst49884, %struct.ScmObj** %stackaddr$prim57814, align 8
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%anf_45bind49984 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst49884)
store volatile %struct.ScmObj* %anf_45bind49984, %struct.ScmObj** %stackaddr$prim57815, align 8
%truthy$cmp57816 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind49984)
%cmp$cmp57816 = icmp eq i64 %truthy$cmp57816, 1
br i1 %cmp$cmp57816, label %truebranch$cmp57816, label %falsebranch$cmp57816
truebranch$cmp57816:
%ae50391 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56224$k502390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57817 = alloca %struct.ScmObj*, align 8
%argslist56224$k502391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49885, %struct.ScmObj* %argslist56224$k502390)
store volatile %struct.ScmObj* %argslist56224$k502391, %struct.ScmObj** %stackaddr$prim57817, align 8
%stackaddr$prim57818 = alloca %struct.ScmObj*, align 8
%argslist56224$k502392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50391, %struct.ScmObj* %argslist56224$k502391)
store volatile %struct.ScmObj* %argslist56224$k502392, %struct.ScmObj** %stackaddr$prim57818, align 8
%clofunc57819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50239)
musttail call tailcc void %clofunc57819(%struct.ScmObj* %k50239, %struct.ScmObj* %argslist56224$k502392)
ret void
falsebranch$cmp57816:
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%anf_45bind49985 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst49884)
store volatile %struct.ScmObj* %anf_45bind49985, %struct.ScmObj** %stackaddr$prim57820, align 8
%stackaddr$prim57821 = alloca %struct.ScmObj*, align 8
%anf_45bind49986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst49884)
store volatile %struct.ScmObj* %anf_45bind49986, %struct.ScmObj** %stackaddr$prim57821, align 8
%stackaddr$makeclosure57822 = alloca %struct.ScmObj*, align 8
%fptrToInt57823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50399 to i64
%ae50399 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57823)
store volatile %struct.ScmObj* %ae50399, %struct.ScmObj** %stackaddr$makeclosure57822, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50399, %struct.ScmObj* %anf_45bind49985, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50399, %struct.ScmObj* %k50239, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50399, %struct.ScmObj* %f49886, i64 2)
%argslist56229$_37foldr1498830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57824 = alloca %struct.ScmObj*, align 8
%argslist56229$_37foldr1498831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49986, %struct.ScmObj* %argslist56229$_37foldr1498830)
store volatile %struct.ScmObj* %argslist56229$_37foldr1498831, %struct.ScmObj** %stackaddr$prim57824, align 8
%stackaddr$prim57825 = alloca %struct.ScmObj*, align 8
%argslist56229$_37foldr1498832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc49885, %struct.ScmObj* %argslist56229$_37foldr1498831)
store volatile %struct.ScmObj* %argslist56229$_37foldr1498832, %struct.ScmObj** %stackaddr$prim57825, align 8
%stackaddr$prim57826 = alloca %struct.ScmObj*, align 8
%argslist56229$_37foldr1498833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49886, %struct.ScmObj* %argslist56229$_37foldr1498832)
store volatile %struct.ScmObj* %argslist56229$_37foldr1498833, %struct.ScmObj** %stackaddr$prim57826, align 8
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%argslist56229$_37foldr1498834 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50399, %struct.ScmObj* %argslist56229$_37foldr1498833)
store volatile %struct.ScmObj* %argslist56229$_37foldr1498834, %struct.ScmObj** %stackaddr$prim57827, align 8
%clofunc57828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr149883)
musttail call tailcc void %clofunc57828(%struct.ScmObj* %_37foldr149883, %struct.ScmObj* %argslist56229$_37foldr1498834)
ret void
}

define tailcc void @proc_clo$ae50399(%struct.ScmObj* %env$ae50399,%struct.ScmObj* %current_45args56225) {
%stackaddr$env-ref57829 = alloca %struct.ScmObj*, align 8
%anf_45bind49985 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50399, i64 0)
store %struct.ScmObj* %anf_45bind49985, %struct.ScmObj** %stackaddr$env-ref57829
%stackaddr$env-ref57830 = alloca %struct.ScmObj*, align 8
%k50239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50399, i64 1)
store %struct.ScmObj* %k50239, %struct.ScmObj** %stackaddr$env-ref57830
%stackaddr$env-ref57831 = alloca %struct.ScmObj*, align 8
%f49886 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50399, i64 2)
store %struct.ScmObj* %f49886, %struct.ScmObj** %stackaddr$env-ref57831
%stackaddr$prim57832 = alloca %struct.ScmObj*, align 8
%_95k50240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56225)
store volatile %struct.ScmObj* %_95k50240, %struct.ScmObj** %stackaddr$prim57832, align 8
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%current_45args56226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56225)
store volatile %struct.ScmObj* %current_45args56226, %struct.ScmObj** %stackaddr$prim57833, align 8
%stackaddr$prim57834 = alloca %struct.ScmObj*, align 8
%anf_45bind49987 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56226)
store volatile %struct.ScmObj* %anf_45bind49987, %struct.ScmObj** %stackaddr$prim57834, align 8
%argslist56228$f498860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57835 = alloca %struct.ScmObj*, align 8
%argslist56228$f498861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49987, %struct.ScmObj* %argslist56228$f498860)
store volatile %struct.ScmObj* %argslist56228$f498861, %struct.ScmObj** %stackaddr$prim57835, align 8
%stackaddr$prim57836 = alloca %struct.ScmObj*, align 8
%argslist56228$f498862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49985, %struct.ScmObj* %argslist56228$f498861)
store volatile %struct.ScmObj* %argslist56228$f498862, %struct.ScmObj** %stackaddr$prim57836, align 8
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%argslist56228$f498863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50239, %struct.ScmObj* %argslist56228$f498862)
store volatile %struct.ScmObj* %argslist56228$f498863, %struct.ScmObj** %stackaddr$prim57837, align 8
%clofunc57838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49886)
musttail call tailcc void %clofunc57838(%struct.ScmObj* %f49886, %struct.ScmObj* %argslist56228$f498863)
ret void
}

define tailcc void @proc_clo$ae50267(%struct.ScmObj* %env$ae50267,%struct.ScmObj* %current_45args56233) {
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%k50241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %k50241, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$prim57840 = alloca %struct.ScmObj*, align 8
%current_45args56234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %current_45args56234, %struct.ScmObj** %stackaddr$prim57840, align 8
%stackaddr$prim57841 = alloca %struct.ScmObj*, align 8
%y49863 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56234)
store volatile %struct.ScmObj* %y49863, %struct.ScmObj** %stackaddr$prim57841, align 8
%ae50269 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57842 = alloca %struct.ScmObj*, align 8
%fptrToInt57843 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50270 to i64
%ae50270 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57843)
store volatile %struct.ScmObj* %ae50270, %struct.ScmObj** %stackaddr$makeclosure57842, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50270, %struct.ScmObj* %y49863, i64 0)
%argslist56252$k502410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57844 = alloca %struct.ScmObj*, align 8
%argslist56252$k502411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50270, %struct.ScmObj* %argslist56252$k502410)
store volatile %struct.ScmObj* %argslist56252$k502411, %struct.ScmObj** %stackaddr$prim57844, align 8
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%argslist56252$k502412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50269, %struct.ScmObj* %argslist56252$k502411)
store volatile %struct.ScmObj* %argslist56252$k502412, %struct.ScmObj** %stackaddr$prim57845, align 8
%clofunc57846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k50241)
musttail call tailcc void %clofunc57846(%struct.ScmObj* %k50241, %struct.ScmObj* %argslist56252$k502412)
ret void
}

define tailcc void @proc_clo$ae50270(%struct.ScmObj* %env$ae50270,%struct.ScmObj* %current_45args56236) {
%stackaddr$env-ref57847 = alloca %struct.ScmObj*, align 8
%y49863 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50270, i64 0)
store %struct.ScmObj* %y49863, %struct.ScmObj** %stackaddr$env-ref57847
%stackaddr$prim57848 = alloca %struct.ScmObj*, align 8
%k50242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %k50242, %struct.ScmObj** %stackaddr$prim57848, align 8
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%current_45args56237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %current_45args56237, %struct.ScmObj** %stackaddr$prim57849, align 8
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%f49864 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56237)
store volatile %struct.ScmObj* %f49864, %struct.ScmObj** %stackaddr$prim57850, align 8
%stackaddr$makeclosure57851 = alloca %struct.ScmObj*, align 8
%fptrToInt57852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50271 to i64
%ae50271 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57852)
store volatile %struct.ScmObj* %ae50271, %struct.ScmObj** %stackaddr$makeclosure57851, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %k50242, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50271, %struct.ScmObj* %f49864, i64 1)
%ae50272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57853 = alloca %struct.ScmObj*, align 8
%fptrToInt57854 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50273 to i64
%ae50273 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57854)
store volatile %struct.ScmObj* %ae50273, %struct.ScmObj** %stackaddr$makeclosure57853, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %f49864, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50273, %struct.ScmObj* %y49863, i64 1)
%argslist56251$ae502710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%argslist56251$ae502711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50273, %struct.ScmObj* %argslist56251$ae502710)
store volatile %struct.ScmObj* %argslist56251$ae502711, %struct.ScmObj** %stackaddr$prim57855, align 8
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%argslist56251$ae502712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50272, %struct.ScmObj* %argslist56251$ae502711)
store volatile %struct.ScmObj* %argslist56251$ae502712, %struct.ScmObj** %stackaddr$prim57856, align 8
%clofunc57857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50271)
musttail call tailcc void %clofunc57857(%struct.ScmObj* %ae50271, %struct.ScmObj* %argslist56251$ae502712)
ret void
}

define tailcc void @proc_clo$ae50271(%struct.ScmObj* %env$ae50271,%struct.ScmObj* %current_45args56239) {
%stackaddr$env-ref57858 = alloca %struct.ScmObj*, align 8
%k50242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 0)
store %struct.ScmObj* %k50242, %struct.ScmObj** %stackaddr$env-ref57858
%stackaddr$env-ref57859 = alloca %struct.ScmObj*, align 8
%f49864 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50271, i64 1)
store %struct.ScmObj* %f49864, %struct.ScmObj** %stackaddr$env-ref57859
%stackaddr$prim57860 = alloca %struct.ScmObj*, align 8
%_95k50243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56239)
store volatile %struct.ScmObj* %_95k50243, %struct.ScmObj** %stackaddr$prim57860, align 8
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%current_45args56240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56239)
store volatile %struct.ScmObj* %current_45args56240, %struct.ScmObj** %stackaddr$prim57861, align 8
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%anf_45bind49982 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56240)
store volatile %struct.ScmObj* %anf_45bind49982, %struct.ScmObj** %stackaddr$prim57862, align 8
%argslist56242$f498640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%argslist56242$f498641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind49982, %struct.ScmObj* %argslist56242$f498640)
store volatile %struct.ScmObj* %argslist56242$f498641, %struct.ScmObj** %stackaddr$prim57863, align 8
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%argslist56242$f498642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50242, %struct.ScmObj* %argslist56242$f498641)
store volatile %struct.ScmObj* %argslist56242$f498642, %struct.ScmObj** %stackaddr$prim57864, align 8
%clofunc57865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f49864)
musttail call tailcc void %clofunc57865(%struct.ScmObj* %f49864, %struct.ScmObj* %argslist56242$f498642)
ret void
}

define tailcc void @proc_clo$ae50273(%struct.ScmObj* %env$ae50273,%struct.ScmObj* %args4986550244) {
%stackaddr$env-ref57866 = alloca %struct.ScmObj*, align 8
%f49864 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 0)
store %struct.ScmObj* %f49864, %struct.ScmObj** %stackaddr$env-ref57866
%stackaddr$env-ref57867 = alloca %struct.ScmObj*, align 8
%y49863 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50273, i64 1)
store %struct.ScmObj* %y49863, %struct.ScmObj** %stackaddr$env-ref57867
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%k50245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4986550244)
store volatile %struct.ScmObj* %k50245, %struct.ScmObj** %stackaddr$prim57868, align 8
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%args49865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4986550244)
store volatile %struct.ScmObj* %args49865, %struct.ScmObj** %stackaddr$prim57869, align 8
%stackaddr$makeclosure57870 = alloca %struct.ScmObj*, align 8
%fptrToInt57871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50277 to i64
%ae50277 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57871)
store volatile %struct.ScmObj* %ae50277, %struct.ScmObj** %stackaddr$makeclosure57870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %k50245, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %args49865, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %f49864, i64 2)
%argslist56250$y498630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%argslist56250$y498631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y49863, %struct.ScmObj* %argslist56250$y498630)
store volatile %struct.ScmObj* %argslist56250$y498631, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%argslist56250$y498632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50277, %struct.ScmObj* %argslist56250$y498631)
store volatile %struct.ScmObj* %argslist56250$y498632, %struct.ScmObj** %stackaddr$prim57873, align 8
%clofunc57874 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y49863)
musttail call tailcc void %clofunc57874(%struct.ScmObj* %y49863, %struct.ScmObj* %argslist56250$y498632)
ret void
}

define tailcc void @proc_clo$ae50277(%struct.ScmObj* %env$ae50277,%struct.ScmObj* %current_45args56243) {
%stackaddr$env-ref57875 = alloca %struct.ScmObj*, align 8
%k50245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 0)
store %struct.ScmObj* %k50245, %struct.ScmObj** %stackaddr$env-ref57875
%stackaddr$env-ref57876 = alloca %struct.ScmObj*, align 8
%args49865 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 1)
store %struct.ScmObj* %args49865, %struct.ScmObj** %stackaddr$env-ref57876
%stackaddr$env-ref57877 = alloca %struct.ScmObj*, align 8
%f49864 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 2)
store %struct.ScmObj* %f49864, %struct.ScmObj** %stackaddr$env-ref57877
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%_95k50246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56243)
store volatile %struct.ScmObj* %_95k50246, %struct.ScmObj** %stackaddr$prim57878, align 8
%stackaddr$prim57879 = alloca %struct.ScmObj*, align 8
%current_45args56244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56243)
store volatile %struct.ScmObj* %current_45args56244, %struct.ScmObj** %stackaddr$prim57879, align 8
%stackaddr$prim57880 = alloca %struct.ScmObj*, align 8
%anf_45bind49980 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56244)
store volatile %struct.ScmObj* %anf_45bind49980, %struct.ScmObj** %stackaddr$prim57880, align 8
%stackaddr$makeclosure57881 = alloca %struct.ScmObj*, align 8
%fptrToInt57882 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50280 to i64
%ae50280 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57882)
store volatile %struct.ScmObj* %ae50280, %struct.ScmObj** %stackaddr$makeclosure57881, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50280, %struct.ScmObj* %k50245, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50280, %struct.ScmObj* %args49865, i64 1)
%argslist56249$anf_45bind499800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57883 = alloca %struct.ScmObj*, align 8
%argslist56249$anf_45bind499801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f49864, %struct.ScmObj* %argslist56249$anf_45bind499800)
store volatile %struct.ScmObj* %argslist56249$anf_45bind499801, %struct.ScmObj** %stackaddr$prim57883, align 8
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%argslist56249$anf_45bind499802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50280, %struct.ScmObj* %argslist56249$anf_45bind499801)
store volatile %struct.ScmObj* %argslist56249$anf_45bind499802, %struct.ScmObj** %stackaddr$prim57884, align 8
%clofunc57885 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind49980)
musttail call tailcc void %clofunc57885(%struct.ScmObj* %anf_45bind49980, %struct.ScmObj* %argslist56249$anf_45bind499802)
ret void
}

define tailcc void @proc_clo$ae50280(%struct.ScmObj* %env$ae50280,%struct.ScmObj* %current_45args56246) {
%stackaddr$env-ref57886 = alloca %struct.ScmObj*, align 8
%k50245 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50280, i64 0)
store %struct.ScmObj* %k50245, %struct.ScmObj** %stackaddr$env-ref57886
%stackaddr$env-ref57887 = alloca %struct.ScmObj*, align 8
%args49865 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50280, i64 1)
store %struct.ScmObj* %args49865, %struct.ScmObj** %stackaddr$env-ref57887
%stackaddr$prim57888 = alloca %struct.ScmObj*, align 8
%_95k50247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56246)
store volatile %struct.ScmObj* %_95k50247, %struct.ScmObj** %stackaddr$prim57888, align 8
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%current_45args56247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56246)
store volatile %struct.ScmObj* %current_45args56247, %struct.ScmObj** %stackaddr$prim57889, align 8
%stackaddr$prim57890 = alloca %struct.ScmObj*, align 8
%anf_45bind49981 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56247)
store volatile %struct.ScmObj* %anf_45bind49981, %struct.ScmObj** %stackaddr$prim57890, align 8
%stackaddr$prim57891 = alloca %struct.ScmObj*, align 8
%cpsargs50248 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50245, %struct.ScmObj* %args49865)
store volatile %struct.ScmObj* %cpsargs50248, %struct.ScmObj** %stackaddr$prim57891, align 8
%clofunc57892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind49981)
musttail call tailcc void %clofunc57892(%struct.ScmObj* %anf_45bind49981, %struct.ScmObj* %cpsargs50248)
ret void
}

define tailcc void @proc_clo$ae50252(%struct.ScmObj* %env$ae50252,%struct.ScmObj* %current_45args56254) {
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%k50249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56254)
store volatile %struct.ScmObj* %k50249, %struct.ScmObj** %stackaddr$prim57893, align 8
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%current_45args56255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56254)
store volatile %struct.ScmObj* %current_45args56255, %struct.ScmObj** %stackaddr$prim57894, align 8
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%yu49862 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56255)
store volatile %struct.ScmObj* %yu49862, %struct.ScmObj** %stackaddr$prim57895, align 8
%argslist56257$yu498620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57896 = alloca %struct.ScmObj*, align 8
%argslist56257$yu498621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu49862, %struct.ScmObj* %argslist56257$yu498620)
store volatile %struct.ScmObj* %argslist56257$yu498621, %struct.ScmObj** %stackaddr$prim57896, align 8
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%argslist56257$yu498622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50249, %struct.ScmObj* %argslist56257$yu498621)
store volatile %struct.ScmObj* %argslist56257$yu498622, %struct.ScmObj** %stackaddr$prim57897, align 8
%clofunc57898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu49862)
musttail call tailcc void %clofunc57898(%struct.ScmObj* %yu49862, %struct.ScmObj* %argslist56257$yu498622)
ret void
}