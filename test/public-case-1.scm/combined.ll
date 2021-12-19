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
%mainenv57218 = call %struct.ScmObj* @const_init_null()
%mainargs57219 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv57218, %struct.ScmObj* %mainargs57219)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57216,%struct.ScmObj* %mainargs57217) {
%stackaddr$makeclosure57220 = alloca %struct.ScmObj*, align 8
%fptrToInt57221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51207 to i64
%ae51207 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57221)
store volatile %struct.ScmObj* %ae51207, %struct.ScmObj** %stackaddr$makeclosure57220, align 8
%ae51208 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57222 = alloca %struct.ScmObj*, align 8
%fptrToInt57223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51209 to i64
%ae51209 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57223)
store volatile %struct.ScmObj* %ae51209, %struct.ScmObj** %stackaddr$makeclosure57222, align 8
%argslist57215$ae512070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%argslist57215$ae512071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51209, %struct.ScmObj* %argslist57215$ae512070)
store volatile %struct.ScmObj* %argslist57215$ae512071, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%argslist57215$ae512072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51208, %struct.ScmObj* %argslist57215$ae512071)
store volatile %struct.ScmObj* %argslist57215$ae512072, %struct.ScmObj** %stackaddr$prim57225, align 8
%clofunc57226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51207)
musttail call tailcc void %clofunc57226(%struct.ScmObj* %ae51207, %struct.ScmObj* %argslist57215$ae512072)
ret void
}

define tailcc void @proc_clo$ae51207(%struct.ScmObj* %env$ae51207,%struct.ScmObj* %current_45args56674) {
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%_95k51047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56674)
store volatile %struct.ScmObj* %_95k51047, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%current_45args56675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56674)
store volatile %struct.ScmObj* %current_45args56675, %struct.ScmObj** %stackaddr$prim57228, align 8
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%anf_45bind50936 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56675)
store volatile %struct.ScmObj* %anf_45bind50936, %struct.ScmObj** %stackaddr$prim57229, align 8
%stackaddr$makeclosure57230 = alloca %struct.ScmObj*, align 8
%fptrToInt57231 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51222 to i64
%ae51222 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57231)
store volatile %struct.ScmObj* %ae51222, %struct.ScmObj** %stackaddr$makeclosure57230, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51222, %struct.ScmObj* %anf_45bind50936, i64 0)
%ae51223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57232 = alloca %struct.ScmObj*, align 8
%fptrToInt57233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51224 to i64
%ae51224 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57233)
store volatile %struct.ScmObj* %ae51224, %struct.ScmObj** %stackaddr$makeclosure57232, align 8
%argslist57210$ae512220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%argslist57210$ae512221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51224, %struct.ScmObj* %argslist57210$ae512220)
store volatile %struct.ScmObj* %argslist57210$ae512221, %struct.ScmObj** %stackaddr$prim57234, align 8
%stackaddr$prim57235 = alloca %struct.ScmObj*, align 8
%argslist57210$ae512222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51223, %struct.ScmObj* %argslist57210$ae512221)
store volatile %struct.ScmObj* %argslist57210$ae512222, %struct.ScmObj** %stackaddr$prim57235, align 8
%clofunc57236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51222)
musttail call tailcc void %clofunc57236(%struct.ScmObj* %ae51222, %struct.ScmObj* %argslist57210$ae512222)
ret void
}

define tailcc void @proc_clo$ae51222(%struct.ScmObj* %env$ae51222,%struct.ScmObj* %current_45args56677) {
%stackaddr$env-ref57237 = alloca %struct.ScmObj*, align 8
%anf_45bind50936 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51222, i64 0)
store %struct.ScmObj* %anf_45bind50936, %struct.ScmObj** %stackaddr$env-ref57237
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%_95k51048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56677)
store volatile %struct.ScmObj* %_95k51048, %struct.ScmObj** %stackaddr$prim57238, align 8
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%current_45args56678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56677)
store volatile %struct.ScmObj* %current_45args56678, %struct.ScmObj** %stackaddr$prim57239, align 8
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%anf_45bind50940 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56678)
store volatile %struct.ScmObj* %anf_45bind50940, %struct.ScmObj** %stackaddr$prim57240, align 8
%stackaddr$makeclosure57241 = alloca %struct.ScmObj*, align 8
%fptrToInt57242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51337 to i64
%ae51337 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57242)
store volatile %struct.ScmObj* %ae51337, %struct.ScmObj** %stackaddr$makeclosure57241, align 8
%argslist57189$anf_45bind509360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%argslist57189$anf_45bind509361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50940, %struct.ScmObj* %argslist57189$anf_45bind509360)
store volatile %struct.ScmObj* %argslist57189$anf_45bind509361, %struct.ScmObj** %stackaddr$prim57243, align 8
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%argslist57189$anf_45bind509362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51337, %struct.ScmObj* %argslist57189$anf_45bind509361)
store volatile %struct.ScmObj* %argslist57189$anf_45bind509362, %struct.ScmObj** %stackaddr$prim57244, align 8
%clofunc57245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50936)
musttail call tailcc void %clofunc57245(%struct.ScmObj* %anf_45bind50936, %struct.ScmObj* %argslist57189$anf_45bind509362)
ret void
}

define tailcc void @proc_clo$ae51337(%struct.ScmObj* %env$ae51337,%struct.ScmObj* %current_45args56680) {
%stackaddr$prim57246 = alloca %struct.ScmObj*, align 8
%_95k51049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56680)
store volatile %struct.ScmObj* %_95k51049, %struct.ScmObj** %stackaddr$prim57246, align 8
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%current_45args56681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56680)
store volatile %struct.ScmObj* %current_45args56681, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$prim57248, align 8
%stackaddr$makeclosure57249 = alloca %struct.ScmObj*, align 8
%fptrToInt57250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51339 to i64
%ae51339 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57250)
store volatile %struct.ScmObj* %ae51339, %struct.ScmObj** %stackaddr$makeclosure57249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51339, %struct.ScmObj* %Ycmb50818, i64 0)
%ae51340 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57251 = alloca %struct.ScmObj*, align 8
%fptrToInt57252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51341 to i64
%ae51341 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57252)
store volatile %struct.ScmObj* %ae51341, %struct.ScmObj** %stackaddr$makeclosure57251, align 8
%argslist57188$ae513390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%argslist57188$ae513391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51341, %struct.ScmObj* %argslist57188$ae513390)
store volatile %struct.ScmObj* %argslist57188$ae513391, %struct.ScmObj** %stackaddr$prim57253, align 8
%stackaddr$prim57254 = alloca %struct.ScmObj*, align 8
%argslist57188$ae513392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51340, %struct.ScmObj* %argslist57188$ae513391)
store volatile %struct.ScmObj* %argslist57188$ae513392, %struct.ScmObj** %stackaddr$prim57254, align 8
%clofunc57255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51339)
musttail call tailcc void %clofunc57255(%struct.ScmObj* %ae51339, %struct.ScmObj* %argslist57188$ae513392)
ret void
}

define tailcc void @proc_clo$ae51339(%struct.ScmObj* %env$ae51339,%struct.ScmObj* %current_45args56683) {
%stackaddr$env-ref57256 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51339, i64 0)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57256
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%_95k51050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56683)
store volatile %struct.ScmObj* %_95k51050, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%current_45args56684 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56683)
store volatile %struct.ScmObj* %current_45args56684, %struct.ScmObj** %stackaddr$prim57258, align 8
%stackaddr$prim57259 = alloca %struct.ScmObj*, align 8
%anf_45bind50945 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56684)
store volatile %struct.ScmObj* %anf_45bind50945, %struct.ScmObj** %stackaddr$prim57259, align 8
%stackaddr$makeclosure57260 = alloca %struct.ScmObj*, align 8
%fptrToInt57261 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51417 to i64
%ae51417 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57261)
store volatile %struct.ScmObj* %ae51417, %struct.ScmObj** %stackaddr$makeclosure57260, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51417, %struct.ScmObj* %Ycmb50818, i64 0)
%argslist57172$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%argslist57172$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50945, %struct.ScmObj* %argslist57172$Ycmb508180)
store volatile %struct.ScmObj* %argslist57172$Ycmb508181, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%argslist57172$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51417, %struct.ScmObj* %argslist57172$Ycmb508181)
store volatile %struct.ScmObj* %argslist57172$Ycmb508182, %struct.ScmObj** %stackaddr$prim57263, align 8
%clofunc57264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57264(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57172$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae51417(%struct.ScmObj* %env$ae51417,%struct.ScmObj* %current_45args56686) {
%stackaddr$env-ref57265 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51417, i64 0)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57265
%stackaddr$prim57266 = alloca %struct.ScmObj*, align 8
%_95k51051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56686)
store volatile %struct.ScmObj* %_95k51051, %struct.ScmObj** %stackaddr$prim57266, align 8
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%current_45args56687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56686)
store volatile %struct.ScmObj* %current_45args56687, %struct.ScmObj** %stackaddr$prim57267, align 8
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$prim57268, align 8
%stackaddr$makeclosure57269 = alloca %struct.ScmObj*, align 8
%fptrToInt57270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51419 to i64
%ae51419 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57270)
store volatile %struct.ScmObj* %ae51419, %struct.ScmObj** %stackaddr$makeclosure57269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51419, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51419, %struct.ScmObj* %Ycmb50818, i64 1)
%ae51420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57271 = alloca %struct.ScmObj*, align 8
%fptrToInt57272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51421 to i64
%ae51421 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57272)
store volatile %struct.ScmObj* %ae51421, %struct.ScmObj** %stackaddr$makeclosure57271, align 8
%argslist57171$ae514190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%argslist57171$ae514191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51421, %struct.ScmObj* %argslist57171$ae514190)
store volatile %struct.ScmObj* %argslist57171$ae514191, %struct.ScmObj** %stackaddr$prim57273, align 8
%stackaddr$prim57274 = alloca %struct.ScmObj*, align 8
%argslist57171$ae514192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51420, %struct.ScmObj* %argslist57171$ae514191)
store volatile %struct.ScmObj* %argslist57171$ae514192, %struct.ScmObj** %stackaddr$prim57274, align 8
%clofunc57275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51419)
musttail call tailcc void %clofunc57275(%struct.ScmObj* %ae51419, %struct.ScmObj* %argslist57171$ae514192)
ret void
}

define tailcc void @proc_clo$ae51419(%struct.ScmObj* %env$ae51419,%struct.ScmObj* %current_45args56689) {
%stackaddr$env-ref57276 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51419, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57276
%stackaddr$env-ref57277 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51419, i64 1)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57277
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%_95k51052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56689)
store volatile %struct.ScmObj* %_95k51052, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%current_45args56690 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56689)
store volatile %struct.ScmObj* %current_45args56690, %struct.ScmObj** %stackaddr$prim57279, align 8
%stackaddr$prim57280 = alloca %struct.ScmObj*, align 8
%anf_45bind50951 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56690)
store volatile %struct.ScmObj* %anf_45bind50951, %struct.ScmObj** %stackaddr$prim57280, align 8
%stackaddr$makeclosure57281 = alloca %struct.ScmObj*, align 8
%fptrToInt57282 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51514 to i64
%ae51514 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57282)
store volatile %struct.ScmObj* %ae51514, %struct.ScmObj** %stackaddr$makeclosure57281, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51514, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51514, %struct.ScmObj* %Ycmb50818, i64 1)
%argslist57152$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%argslist57152$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50951, %struct.ScmObj* %argslist57152$Ycmb508180)
store volatile %struct.ScmObj* %argslist57152$Ycmb508181, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%argslist57152$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51514, %struct.ScmObj* %argslist57152$Ycmb508181)
store volatile %struct.ScmObj* %argslist57152$Ycmb508182, %struct.ScmObj** %stackaddr$prim57284, align 8
%clofunc57285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57285(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57152$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae51514(%struct.ScmObj* %env$ae51514,%struct.ScmObj* %current_45args56692) {
%stackaddr$env-ref57286 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51514, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57286
%stackaddr$env-ref57287 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51514, i64 1)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57287
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%_95k51053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %_95k51053, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%current_45args56693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %current_45args56693, %struct.ScmObj** %stackaddr$prim57289, align 8
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56693)
store volatile %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$prim57290, align 8
%stackaddr$makeclosure57291 = alloca %struct.ScmObj*, align 8
%fptrToInt57292 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51516 to i64
%ae51516 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57292)
store volatile %struct.ScmObj* %ae51516, %struct.ScmObj** %stackaddr$makeclosure57291, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51516, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51516, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51516, %struct.ScmObj* %Ycmb50818, i64 2)
%ae51517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57293 = alloca %struct.ScmObj*, align 8
%fptrToInt57294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51518 to i64
%ae51518 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57294)
store volatile %struct.ScmObj* %ae51518, %struct.ScmObj** %stackaddr$makeclosure57293, align 8
%argslist57151$ae515160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%argslist57151$ae515161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51518, %struct.ScmObj* %argslist57151$ae515160)
store volatile %struct.ScmObj* %argslist57151$ae515161, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%argslist57151$ae515162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51517, %struct.ScmObj* %argslist57151$ae515161)
store volatile %struct.ScmObj* %argslist57151$ae515162, %struct.ScmObj** %stackaddr$prim57296, align 8
%clofunc57297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51516)
musttail call tailcc void %clofunc57297(%struct.ScmObj* %ae51516, %struct.ScmObj* %argslist57151$ae515162)
ret void
}

define tailcc void @proc_clo$ae51516(%struct.ScmObj* %env$ae51516,%struct.ScmObj* %current_45args56695) {
%stackaddr$env-ref57298 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51516, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57298
%stackaddr$env-ref57299 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51516, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57299
%stackaddr$env-ref57300 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51516, i64 2)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57300
%stackaddr$prim57301 = alloca %struct.ScmObj*, align 8
%_95k51054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56695)
store volatile %struct.ScmObj* %_95k51054, %struct.ScmObj** %stackaddr$prim57301, align 8
%stackaddr$prim57302 = alloca %struct.ScmObj*, align 8
%current_45args56696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56695)
store volatile %struct.ScmObj* %current_45args56696, %struct.ScmObj** %stackaddr$prim57302, align 8
%stackaddr$prim57303 = alloca %struct.ScmObj*, align 8
%anf_45bind50958 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56696)
store volatile %struct.ScmObj* %anf_45bind50958, %struct.ScmObj** %stackaddr$prim57303, align 8
%stackaddr$makeclosure57304 = alloca %struct.ScmObj*, align 8
%fptrToInt57305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51664 to i64
%ae51664 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57305)
store volatile %struct.ScmObj* %ae51664, %struct.ScmObj** %stackaddr$makeclosure57304, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %Ycmb50818, i64 2)
%argslist57135$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57306 = alloca %struct.ScmObj*, align 8
%argslist57135$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50958, %struct.ScmObj* %argslist57135$Ycmb508180)
store volatile %struct.ScmObj* %argslist57135$Ycmb508181, %struct.ScmObj** %stackaddr$prim57306, align 8
%stackaddr$prim57307 = alloca %struct.ScmObj*, align 8
%argslist57135$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51664, %struct.ScmObj* %argslist57135$Ycmb508181)
store volatile %struct.ScmObj* %argslist57135$Ycmb508182, %struct.ScmObj** %stackaddr$prim57307, align 8
%clofunc57308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57308(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57135$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae51664(%struct.ScmObj* %env$ae51664,%struct.ScmObj* %current_45args56698) {
%stackaddr$env-ref57309 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57309
%stackaddr$env-ref57310 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57310
%stackaddr$env-ref57311 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 2)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57311
%stackaddr$prim57312 = alloca %struct.ScmObj*, align 8
%_95k51055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56698)
store volatile %struct.ScmObj* %_95k51055, %struct.ScmObj** %stackaddr$prim57312, align 8
%stackaddr$prim57313 = alloca %struct.ScmObj*, align 8
%current_45args56699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56698)
store volatile %struct.ScmObj* %current_45args56699, %struct.ScmObj** %stackaddr$prim57313, align 8
%stackaddr$prim57314 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56699)
store volatile %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$prim57314, align 8
%stackaddr$makeclosure57315 = alloca %struct.ScmObj*, align 8
%fptrToInt57316 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51666 to i64
%ae51666 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57316)
store volatile %struct.ScmObj* %ae51666, %struct.ScmObj** %stackaddr$makeclosure57315, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51666, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51666, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51666, %struct.ScmObj* %Ycmb50818, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51666, %struct.ScmObj* %_37take50831, i64 3)
%ae51667 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57317 = alloca %struct.ScmObj*, align 8
%fptrToInt57318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51668 to i64
%ae51668 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57318)
store volatile %struct.ScmObj* %ae51668, %struct.ScmObj** %stackaddr$makeclosure57317, align 8
%argslist57134$ae516660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57319 = alloca %struct.ScmObj*, align 8
%argslist57134$ae516661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51668, %struct.ScmObj* %argslist57134$ae516660)
store volatile %struct.ScmObj* %argslist57134$ae516661, %struct.ScmObj** %stackaddr$prim57319, align 8
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%argslist57134$ae516662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51667, %struct.ScmObj* %argslist57134$ae516661)
store volatile %struct.ScmObj* %argslist57134$ae516662, %struct.ScmObj** %stackaddr$prim57320, align 8
%clofunc57321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51666)
musttail call tailcc void %clofunc57321(%struct.ScmObj* %ae51666, %struct.ScmObj* %argslist57134$ae516662)
ret void
}

define tailcc void @proc_clo$ae51666(%struct.ScmObj* %env$ae51666,%struct.ScmObj* %current_45args56701) {
%stackaddr$env-ref57322 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51666, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57322
%stackaddr$env-ref57323 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51666, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57323
%stackaddr$env-ref57324 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51666, i64 2)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57324
%stackaddr$env-ref57325 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51666, i64 3)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref57325
%stackaddr$prim57326 = alloca %struct.ScmObj*, align 8
%_95k51056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56701)
store volatile %struct.ScmObj* %_95k51056, %struct.ScmObj** %stackaddr$prim57326, align 8
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%current_45args56702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56701)
store volatile %struct.ScmObj* %current_45args56702, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$prim57328 = alloca %struct.ScmObj*, align 8
%anf_45bind50962 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56702)
store volatile %struct.ScmObj* %anf_45bind50962, %struct.ScmObj** %stackaddr$prim57328, align 8
%stackaddr$makeclosure57329 = alloca %struct.ScmObj*, align 8
%fptrToInt57330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51747 to i64
%ae51747 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57330)
store volatile %struct.ScmObj* %ae51747, %struct.ScmObj** %stackaddr$makeclosure57329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %Ycmb50818, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51747, %struct.ScmObj* %_37take50831, i64 3)
%argslist57120$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57331 = alloca %struct.ScmObj*, align 8
%argslist57120$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50962, %struct.ScmObj* %argslist57120$Ycmb508180)
store volatile %struct.ScmObj* %argslist57120$Ycmb508181, %struct.ScmObj** %stackaddr$prim57331, align 8
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%argslist57120$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51747, %struct.ScmObj* %argslist57120$Ycmb508181)
store volatile %struct.ScmObj* %argslist57120$Ycmb508182, %struct.ScmObj** %stackaddr$prim57332, align 8
%clofunc57333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57333(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57120$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae51747(%struct.ScmObj* %env$ae51747,%struct.ScmObj* %current_45args56704) {
%stackaddr$env-ref57334 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57334
%stackaddr$env-ref57335 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57335
%stackaddr$env-ref57336 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 2)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57336
%stackaddr$env-ref57337 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51747, i64 3)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref57337
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%_95k51057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %_95k51057, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%current_45args56705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %current_45args56705, %struct.ScmObj** %stackaddr$prim57339, align 8
%stackaddr$prim57340 = alloca %struct.ScmObj*, align 8
%_37length50828 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56705)
store volatile %struct.ScmObj* %_37length50828, %struct.ScmObj** %stackaddr$prim57340, align 8
%stackaddr$makeclosure57341 = alloca %struct.ScmObj*, align 8
%fptrToInt57342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51749 to i64
%ae51749 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57342)
store volatile %struct.ScmObj* %ae51749, %struct.ScmObj** %stackaddr$makeclosure57341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51749, %struct.ScmObj* %_37length50828, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51749, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51749, %struct.ScmObj* %_37map150835, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51749, %struct.ScmObj* %Ycmb50818, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51749, %struct.ScmObj* %_37take50831, i64 4)
%ae51750 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57343 = alloca %struct.ScmObj*, align 8
%fptrToInt57344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51751 to i64
%ae51751 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57344)
store volatile %struct.ScmObj* %ae51751, %struct.ScmObj** %stackaddr$makeclosure57343, align 8
%argslist57119$ae517490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57345 = alloca %struct.ScmObj*, align 8
%argslist57119$ae517491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51751, %struct.ScmObj* %argslist57119$ae517490)
store volatile %struct.ScmObj* %argslist57119$ae517491, %struct.ScmObj** %stackaddr$prim57345, align 8
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%argslist57119$ae517492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51750, %struct.ScmObj* %argslist57119$ae517491)
store volatile %struct.ScmObj* %argslist57119$ae517492, %struct.ScmObj** %stackaddr$prim57346, align 8
%clofunc57347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51749)
musttail call tailcc void %clofunc57347(%struct.ScmObj* %ae51749, %struct.ScmObj* %argslist57119$ae517492)
ret void
}

define tailcc void @proc_clo$ae51749(%struct.ScmObj* %env$ae51749,%struct.ScmObj* %current_45args56707) {
%stackaddr$env-ref57348 = alloca %struct.ScmObj*, align 8
%_37length50828 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51749, i64 0)
store %struct.ScmObj* %_37length50828, %struct.ScmObj** %stackaddr$env-ref57348
%stackaddr$env-ref57349 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51749, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57349
%stackaddr$env-ref57350 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51749, i64 2)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57350
%stackaddr$env-ref57351 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51749, i64 3)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57351
%stackaddr$env-ref57352 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51749, i64 4)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref57352
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%_95k51058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56707)
store volatile %struct.ScmObj* %_95k51058, %struct.ScmObj** %stackaddr$prim57353, align 8
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%current_45args56708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56707)
store volatile %struct.ScmObj* %current_45args56708, %struct.ScmObj** %stackaddr$prim57354, align 8
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%anf_45bind50967 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56708)
store volatile %struct.ScmObj* %anf_45bind50967, %struct.ScmObj** %stackaddr$prim57355, align 8
%stackaddr$makeclosure57356 = alloca %struct.ScmObj*, align 8
%fptrToInt57357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51826 to i64
%ae51826 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57357)
store volatile %struct.ScmObj* %ae51826, %struct.ScmObj** %stackaddr$makeclosure57356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %_37length50828, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %_37map150835, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %Ycmb50818, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51826, %struct.ScmObj* %_37take50831, i64 4)
%argslist57103$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%argslist57103$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50967, %struct.ScmObj* %argslist57103$Ycmb508180)
store volatile %struct.ScmObj* %argslist57103$Ycmb508181, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%argslist57103$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51826, %struct.ScmObj* %argslist57103$Ycmb508181)
store volatile %struct.ScmObj* %argslist57103$Ycmb508182, %struct.ScmObj** %stackaddr$prim57359, align 8
%clofunc57360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57360(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57103$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae51826(%struct.ScmObj* %env$ae51826,%struct.ScmObj* %current_45args56710) {
%stackaddr$env-ref57361 = alloca %struct.ScmObj*, align 8
%_37length50828 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 0)
store %struct.ScmObj* %_37length50828, %struct.ScmObj** %stackaddr$env-ref57361
%stackaddr$env-ref57362 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57362
%stackaddr$env-ref57363 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 2)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57363
%stackaddr$env-ref57364 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 3)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57364
%stackaddr$env-ref57365 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51826, i64 4)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref57365
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%_95k51059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56710)
store volatile %struct.ScmObj* %_95k51059, %struct.ScmObj** %stackaddr$prim57366, align 8
%stackaddr$prim57367 = alloca %struct.ScmObj*, align 8
%current_45args56711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56710)
store volatile %struct.ScmObj* %current_45args56711, %struct.ScmObj** %stackaddr$prim57367, align 8
%stackaddr$prim57368 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56711)
store volatile %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$prim57368, align 8
%stackaddr$makeclosure57369 = alloca %struct.ScmObj*, align 8
%fptrToInt57370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51828 to i64
%ae51828 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57370)
store volatile %struct.ScmObj* %ae51828, %struct.ScmObj** %stackaddr$makeclosure57369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %_37length50828, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %_37map150835, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %Ycmb50818, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51828, %struct.ScmObj* %_37take50831, i64 5)
%ae51829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57371 = alloca %struct.ScmObj*, align 8
%fptrToInt57372 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51830 to i64
%ae51830 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57372)
store volatile %struct.ScmObj* %ae51830, %struct.ScmObj** %stackaddr$makeclosure57371, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51830, %struct.ScmObj* %_37foldl150823, i64 0)
%argslist57102$ae518280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%argslist57102$ae518281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51830, %struct.ScmObj* %argslist57102$ae518280)
store volatile %struct.ScmObj* %argslist57102$ae518281, %struct.ScmObj** %stackaddr$prim57373, align 8
%stackaddr$prim57374 = alloca %struct.ScmObj*, align 8
%argslist57102$ae518282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51829, %struct.ScmObj* %argslist57102$ae518281)
store volatile %struct.ScmObj* %argslist57102$ae518282, %struct.ScmObj** %stackaddr$prim57374, align 8
%clofunc57375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51828)
musttail call tailcc void %clofunc57375(%struct.ScmObj* %ae51828, %struct.ScmObj* %argslist57102$ae518282)
ret void
}

define tailcc void @proc_clo$ae51828(%struct.ScmObj* %env$ae51828,%struct.ScmObj* %current_45args56713) {
%stackaddr$env-ref57376 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57376
%stackaddr$env-ref57377 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57377
%stackaddr$env-ref57378 = alloca %struct.ScmObj*, align 8
%_37length50828 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 2)
store %struct.ScmObj* %_37length50828, %struct.ScmObj** %stackaddr$env-ref57378
%stackaddr$env-ref57379 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 3)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57379
%stackaddr$env-ref57380 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 4)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57380
%stackaddr$env-ref57381 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51828, i64 5)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref57381
%stackaddr$prim57382 = alloca %struct.ScmObj*, align 8
%_95k51060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56713)
store volatile %struct.ScmObj* %_95k51060, %struct.ScmObj** %stackaddr$prim57382, align 8
%stackaddr$prim57383 = alloca %struct.ScmObj*, align 8
%current_45args56714 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56713)
store volatile %struct.ScmObj* %current_45args56714, %struct.ScmObj** %stackaddr$prim57383, align 8
%stackaddr$prim57384 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56714)
store volatile %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$prim57384, align 8
%stackaddr$makeclosure57385 = alloca %struct.ScmObj*, align 8
%fptrToInt57386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51882 to i64
%ae51882 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57386)
store volatile %struct.ScmObj* %ae51882, %struct.ScmObj** %stackaddr$makeclosure57385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51882, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51882, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51882, %struct.ScmObj* %_37map150835, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51882, %struct.ScmObj* %Ycmb50818, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51882, %struct.ScmObj* %_37last50861, i64 4)
%ae51883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57387 = alloca %struct.ScmObj*, align 8
%fptrToInt57388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51884 to i64
%ae51884 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57388)
store volatile %struct.ScmObj* %ae51884, %struct.ScmObj** %stackaddr$makeclosure57387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51884, %struct.ScmObj* %_37length50828, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51884, %struct.ScmObj* %_37take50831, i64 1)
%argslist57088$ae518820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57389 = alloca %struct.ScmObj*, align 8
%argslist57088$ae518821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51884, %struct.ScmObj* %argslist57088$ae518820)
store volatile %struct.ScmObj* %argslist57088$ae518821, %struct.ScmObj** %stackaddr$prim57389, align 8
%stackaddr$prim57390 = alloca %struct.ScmObj*, align 8
%argslist57088$ae518822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51883, %struct.ScmObj* %argslist57088$ae518821)
store volatile %struct.ScmObj* %argslist57088$ae518822, %struct.ScmObj** %stackaddr$prim57390, align 8
%clofunc57391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51882)
musttail call tailcc void %clofunc57391(%struct.ScmObj* %ae51882, %struct.ScmObj* %argslist57088$ae518822)
ret void
}

define tailcc void @proc_clo$ae51882(%struct.ScmObj* %env$ae51882,%struct.ScmObj* %current_45args56716) {
%stackaddr$env-ref57392 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51882, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57392
%stackaddr$env-ref57393 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51882, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57393
%stackaddr$env-ref57394 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51882, i64 2)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref57394
%stackaddr$env-ref57395 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51882, i64 3)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57395
%stackaddr$env-ref57396 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51882, i64 4)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref57396
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%_95k51061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56716)
store volatile %struct.ScmObj* %_95k51061, %struct.ScmObj** %stackaddr$prim57397, align 8
%stackaddr$prim57398 = alloca %struct.ScmObj*, align 8
%current_45args56717 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56716)
store volatile %struct.ScmObj* %current_45args56717, %struct.ScmObj** %stackaddr$prim57398, align 8
%stackaddr$prim57399 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56717)
store volatile %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$prim57399, align 8
%stackaddr$makeclosure57400 = alloca %struct.ScmObj*, align 8
%fptrToInt57401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51912 to i64
%ae51912 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57401)
store volatile %struct.ScmObj* %ae51912, %struct.ScmObj** %stackaddr$makeclosure57400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %_37drop_45right50858, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %Ycmb50818, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %_37last50861, i64 4)
%ae51913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57402 = alloca %struct.ScmObj*, align 8
%fptrToInt57403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51914 to i64
%ae51914 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57403)
store volatile %struct.ScmObj* %ae51914, %struct.ScmObj** %stackaddr$makeclosure57402, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51914, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51914, %struct.ScmObj* %_37map150835, i64 1)
%argslist57078$ae519120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist57078$ae519121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51914, %struct.ScmObj* %argslist57078$ae519120)
store volatile %struct.ScmObj* %argslist57078$ae519121, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%argslist57078$ae519122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51913, %struct.ScmObj* %argslist57078$ae519121)
store volatile %struct.ScmObj* %argslist57078$ae519122, %struct.ScmObj** %stackaddr$prim57405, align 8
%clofunc57406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51912)
musttail call tailcc void %clofunc57406(%struct.ScmObj* %ae51912, %struct.ScmObj* %argslist57078$ae519122)
ret void
}

define tailcc void @proc_clo$ae51912(%struct.ScmObj* %env$ae51912,%struct.ScmObj* %current_45args56719) {
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$env-ref57408 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57408
%stackaddr$env-ref57409 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 2)
store %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$env-ref57409
%stackaddr$env-ref57410 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 3)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57410
%stackaddr$env-ref57411 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 4)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref57411
%stackaddr$prim57412 = alloca %struct.ScmObj*, align 8
%_95k51062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %_95k51062, %struct.ScmObj** %stackaddr$prim57412, align 8
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%current_45args56720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %current_45args56720, %struct.ScmObj** %stackaddr$prim57413, align 8
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%anf_45bind50983 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56720)
store volatile %struct.ScmObj* %anf_45bind50983, %struct.ScmObj** %stackaddr$prim57414, align 8
%stackaddr$makeclosure57415 = alloca %struct.ScmObj*, align 8
%fptrToInt57416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52296 to i64
%ae52296 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57416)
store volatile %struct.ScmObj* %ae52296, %struct.ScmObj** %stackaddr$makeclosure57415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52296, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52296, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52296, %struct.ScmObj* %_37drop_45right50858, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52296, %struct.ScmObj* %Ycmb50818, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52296, %struct.ScmObj* %_37last50861, i64 4)
%argslist57018$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%argslist57018$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50983, %struct.ScmObj* %argslist57018$Ycmb508180)
store volatile %struct.ScmObj* %argslist57018$Ycmb508181, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$prim57418 = alloca %struct.ScmObj*, align 8
%argslist57018$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52296, %struct.ScmObj* %argslist57018$Ycmb508181)
store volatile %struct.ScmObj* %argslist57018$Ycmb508182, %struct.ScmObj** %stackaddr$prim57418, align 8
%clofunc57419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57419(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist57018$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae52296(%struct.ScmObj* %env$ae52296,%struct.ScmObj* %current_45args56722) {
%stackaddr$env-ref57420 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52296, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57420
%stackaddr$env-ref57421 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52296, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57421
%stackaddr$env-ref57422 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52296, i64 2)
store %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$env-ref57422
%stackaddr$env-ref57423 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52296, i64 3)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57423
%stackaddr$env-ref57424 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52296, i64 4)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref57424
%stackaddr$prim57425 = alloca %struct.ScmObj*, align 8
%_95k51063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56722)
store volatile %struct.ScmObj* %_95k51063, %struct.ScmObj** %stackaddr$prim57425, align 8
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%current_45args56723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56722)
store volatile %struct.ScmObj* %current_45args56723, %struct.ScmObj** %stackaddr$prim57426, align 8
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$prim57427, align 8
%stackaddr$makeclosure57428 = alloca %struct.ScmObj*, align 8
%fptrToInt57429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52298 to i64
%ae52298 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57429)
store volatile %struct.ScmObj* %ae52298, %struct.ScmObj** %stackaddr$makeclosure57428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %_37drop_45right50858, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %Ycmb50818, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52298, %struct.ScmObj* %_37last50861, i64 5)
%ae52299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57430 = alloca %struct.ScmObj*, align 8
%fptrToInt57431 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52300 to i64
%ae52300 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57431)
store volatile %struct.ScmObj* %ae52300, %struct.ScmObj** %stackaddr$makeclosure57430, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52300, %struct.ScmObj* %_37foldr150839, i64 0)
%argslist57017$ae522980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%argslist57017$ae522981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52300, %struct.ScmObj* %argslist57017$ae522980)
store volatile %struct.ScmObj* %argslist57017$ae522981, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%argslist57017$ae522982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52299, %struct.ScmObj* %argslist57017$ae522981)
store volatile %struct.ScmObj* %argslist57017$ae522982, %struct.ScmObj** %stackaddr$prim57433, align 8
%clofunc57434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52298)
musttail call tailcc void %clofunc57434(%struct.ScmObj* %ae52298, %struct.ScmObj* %argslist57017$ae522982)
ret void
}

define tailcc void @proc_clo$ae52298(%struct.ScmObj* %env$ae52298,%struct.ScmObj* %current_45args56725) {
%stackaddr$env-ref57435 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57435
%stackaddr$env-ref57436 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57436
%stackaddr$env-ref57437 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref57437
%stackaddr$env-ref57438 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 3)
store %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$env-ref57438
%stackaddr$env-ref57439 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 4)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57439
%stackaddr$env-ref57440 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52298, i64 5)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref57440
%stackaddr$prim57441 = alloca %struct.ScmObj*, align 8
%_95k51064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56725)
store volatile %struct.ScmObj* %_95k51064, %struct.ScmObj** %stackaddr$prim57441, align 8
%stackaddr$prim57442 = alloca %struct.ScmObj*, align 8
%current_45args56726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56725)
store volatile %struct.ScmObj* %current_45args56726, %struct.ScmObj** %stackaddr$prim57442, align 8
%stackaddr$prim57443 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56726)
store volatile %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$prim57443, align 8
%stackaddr$makeclosure57444 = alloca %struct.ScmObj*, align 8
%fptrToInt57445 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52375 to i64
%ae52375 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57445)
store volatile %struct.ScmObj* %ae52375, %struct.ScmObj** %stackaddr$makeclosure57444, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %_37map150870, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52375, %struct.ScmObj* %Ycmb50818, i64 4)
%ae52376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57446 = alloca %struct.ScmObj*, align 8
%fptrToInt57447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52377 to i64
%ae52377 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57447)
store volatile %struct.ScmObj* %ae52377, %struct.ScmObj** %stackaddr$makeclosure57446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52377, %struct.ScmObj* %_37foldr50844, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52377, %struct.ScmObj* %_37drop_45right50858, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52377, %struct.ScmObj* %_37last50861, i64 2)
%argslist56998$ae523750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57448 = alloca %struct.ScmObj*, align 8
%argslist56998$ae523751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52377, %struct.ScmObj* %argslist56998$ae523750)
store volatile %struct.ScmObj* %argslist56998$ae523751, %struct.ScmObj** %stackaddr$prim57448, align 8
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%argslist56998$ae523752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52376, %struct.ScmObj* %argslist56998$ae523751)
store volatile %struct.ScmObj* %argslist56998$ae523752, %struct.ScmObj** %stackaddr$prim57449, align 8
%clofunc57450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52375)
musttail call tailcc void %clofunc57450(%struct.ScmObj* %ae52375, %struct.ScmObj* %argslist56998$ae523752)
ret void
}

define tailcc void @proc_clo$ae52375(%struct.ScmObj* %env$ae52375,%struct.ScmObj* %current_45args56728) {
%stackaddr$env-ref57451 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57451
%stackaddr$env-ref57452 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref57452
%stackaddr$env-ref57453 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref57453
%stackaddr$env-ref57454 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 3)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref57454
%stackaddr$env-ref57455 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52375, i64 4)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57455
%stackaddr$prim57456 = alloca %struct.ScmObj*, align 8
%_95k51065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %_95k51065, %struct.ScmObj** %stackaddr$prim57456, align 8
%stackaddr$prim57457 = alloca %struct.ScmObj*, align 8
%current_45args56729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %current_45args56729, %struct.ScmObj** %stackaddr$prim57457, align 8
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%_37map50865 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56729)
store volatile %struct.ScmObj* %_37map50865, %struct.ScmObj** %stackaddr$prim57458, align 8
%stackaddr$makeclosure57459 = alloca %struct.ScmObj*, align 8
%fptrToInt57460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52521 to i64
%ae52521 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57460)
store volatile %struct.ScmObj* %ae52521, %struct.ScmObj** %stackaddr$makeclosure57459, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52521, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52521, %struct.ScmObj* %Ycmb50818, i64 1)
%ae52522 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57461 = alloca %struct.ScmObj*, align 8
%fptrToInt57462 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52523 to i64
%ae52523 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57462)
store volatile %struct.ScmObj* %ae52523, %struct.ScmObj** %stackaddr$makeclosure57461, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52523, %struct.ScmObj* %_37foldr50844, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52523, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52523, %struct.ScmObj* %_37map150870, i64 2)
%argslist56981$ae525210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57463 = alloca %struct.ScmObj*, align 8
%argslist56981$ae525211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52523, %struct.ScmObj* %argslist56981$ae525210)
store volatile %struct.ScmObj* %argslist56981$ae525211, %struct.ScmObj** %stackaddr$prim57463, align 8
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%argslist56981$ae525212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52522, %struct.ScmObj* %argslist56981$ae525211)
store volatile %struct.ScmObj* %argslist56981$ae525212, %struct.ScmObj** %stackaddr$prim57464, align 8
%clofunc57465 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52521)
musttail call tailcc void %clofunc57465(%struct.ScmObj* %ae52521, %struct.ScmObj* %argslist56981$ae525212)
ret void
}

define tailcc void @proc_clo$ae52521(%struct.ScmObj* %env$ae52521,%struct.ScmObj* %current_45args56731) {
%stackaddr$env-ref57466 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52521, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57466
%stackaddr$env-ref57467 = alloca %struct.ScmObj*, align 8
%Ycmb50818 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52521, i64 1)
store %struct.ScmObj* %Ycmb50818, %struct.ScmObj** %stackaddr$env-ref57467
%stackaddr$prim57468 = alloca %struct.ScmObj*, align 8
%_95k51066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56731)
store volatile %struct.ScmObj* %_95k51066, %struct.ScmObj** %stackaddr$prim57468, align 8
%stackaddr$prim57469 = alloca %struct.ScmObj*, align 8
%current_45args56732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56731)
store volatile %struct.ScmObj* %current_45args56732, %struct.ScmObj** %stackaddr$prim57469, align 8
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%anf_45bind51003 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56732)
store volatile %struct.ScmObj* %anf_45bind51003, %struct.ScmObj** %stackaddr$prim57470, align 8
%stackaddr$makeclosure57471 = alloca %struct.ScmObj*, align 8
%fptrToInt57472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52913 to i64
%ae52913 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57472)
store volatile %struct.ScmObj* %ae52913, %struct.ScmObj** %stackaddr$makeclosure57471, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52913, %struct.ScmObj* %_37foldl150823, i64 0)
%argslist56921$Ycmb508180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%argslist56921$Ycmb508181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51003, %struct.ScmObj* %argslist56921$Ycmb508180)
store volatile %struct.ScmObj* %argslist56921$Ycmb508181, %struct.ScmObj** %stackaddr$prim57473, align 8
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%argslist56921$Ycmb508182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52913, %struct.ScmObj* %argslist56921$Ycmb508181)
store volatile %struct.ScmObj* %argslist56921$Ycmb508182, %struct.ScmObj** %stackaddr$prim57474, align 8
%clofunc57475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb50818)
musttail call tailcc void %clofunc57475(%struct.ScmObj* %Ycmb50818, %struct.ScmObj* %argslist56921$Ycmb508182)
ret void
}

define tailcc void @proc_clo$ae52913(%struct.ScmObj* %env$ae52913,%struct.ScmObj* %current_45args56734) {
%stackaddr$env-ref57476 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52913, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57476
%stackaddr$prim57477 = alloca %struct.ScmObj*, align 8
%_95k51067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56734)
store volatile %struct.ScmObj* %_95k51067, %struct.ScmObj** %stackaddr$prim57477, align 8
%stackaddr$prim57478 = alloca %struct.ScmObj*, align 8
%current_45args56735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56734)
store volatile %struct.ScmObj* %current_45args56735, %struct.ScmObj** %stackaddr$prim57478, align 8
%stackaddr$prim57479 = alloca %struct.ScmObj*, align 8
%_37foldl50921 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56735)
store volatile %struct.ScmObj* %_37foldl50921, %struct.ScmObj** %stackaddr$prim57479, align 8
%stackaddr$makeclosure57480 = alloca %struct.ScmObj*, align 8
%fptrToInt57481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52915 to i64
%ae52915 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57481)
store volatile %struct.ScmObj* %ae52915, %struct.ScmObj** %stackaddr$makeclosure57480, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52915, %struct.ScmObj* %_37foldl150823, i64 0)
%ae52916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57482 = alloca %struct.ScmObj*, align 8
%fptrToInt57483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52917 to i64
%ae52917 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57483)
store volatile %struct.ScmObj* %ae52917, %struct.ScmObj** %stackaddr$makeclosure57482, align 8
%argslist56920$ae529150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%argslist56920$ae529151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52917, %struct.ScmObj* %argslist56920$ae529150)
store volatile %struct.ScmObj* %argslist56920$ae529151, %struct.ScmObj** %stackaddr$prim57484, align 8
%stackaddr$prim57485 = alloca %struct.ScmObj*, align 8
%argslist56920$ae529152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52916, %struct.ScmObj* %argslist56920$ae529151)
store volatile %struct.ScmObj* %argslist56920$ae529152, %struct.ScmObj** %stackaddr$prim57485, align 8
%clofunc57486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52915)
musttail call tailcc void %clofunc57486(%struct.ScmObj* %ae52915, %struct.ScmObj* %argslist56920$ae529152)
ret void
}

define tailcc void @proc_clo$ae52915(%struct.ScmObj* %env$ae52915,%struct.ScmObj* %current_45args56737) {
%stackaddr$env-ref57487 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52915, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57487
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%_95k51068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56737)
store volatile %struct.ScmObj* %_95k51068, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%current_45args56738 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56737)
store volatile %struct.ScmObj* %current_45args56738, %struct.ScmObj** %stackaddr$prim57489, align 8
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%_37_6250918 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56738)
store volatile %struct.ScmObj* %_37_6250918, %struct.ScmObj** %stackaddr$prim57490, align 8
%stackaddr$makeclosure57491 = alloca %struct.ScmObj*, align 8
%fptrToInt57492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52939 to i64
%ae52939 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57492)
store volatile %struct.ScmObj* %ae52939, %struct.ScmObj** %stackaddr$makeclosure57491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52939, %struct.ScmObj* %_37foldl150823, i64 0)
%ae52940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57493 = alloca %struct.ScmObj*, align 8
%fptrToInt57494 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52941 to i64
%ae52941 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57494)
store volatile %struct.ScmObj* %ae52941, %struct.ScmObj** %stackaddr$makeclosure57493, align 8
%argslist56914$ae529390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57495 = alloca %struct.ScmObj*, align 8
%argslist56914$ae529391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52941, %struct.ScmObj* %argslist56914$ae529390)
store volatile %struct.ScmObj* %argslist56914$ae529391, %struct.ScmObj** %stackaddr$prim57495, align 8
%stackaddr$prim57496 = alloca %struct.ScmObj*, align 8
%argslist56914$ae529392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52940, %struct.ScmObj* %argslist56914$ae529391)
store volatile %struct.ScmObj* %argslist56914$ae529392, %struct.ScmObj** %stackaddr$prim57496, align 8
%clofunc57497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52939)
musttail call tailcc void %clofunc57497(%struct.ScmObj* %ae52939, %struct.ScmObj* %argslist56914$ae529392)
ret void
}

define tailcc void @proc_clo$ae52939(%struct.ScmObj* %env$ae52939,%struct.ScmObj* %current_45args56740) {
%stackaddr$env-ref57498 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52939, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57498
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%_95k51069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %_95k51069, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%current_45args56741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %current_45args56741, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%_37_62_6150915 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56741)
store volatile %struct.ScmObj* %_37_62_6150915, %struct.ScmObj** %stackaddr$prim57501, align 8
%ae52963 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52964 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%_37append50911 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae52963, %struct.ScmObj* %ae52964)
store volatile %struct.ScmObj* %_37append50911, %struct.ScmObj** %stackaddr$prim57502, align 8
%stackaddr$makeclosure57503 = alloca %struct.ScmObj*, align 8
%fptrToInt57504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52965 to i64
%ae52965 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57504)
store volatile %struct.ScmObj* %ae52965, %struct.ScmObj** %stackaddr$makeclosure57503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52965, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52965, %struct.ScmObj* %_37append50911, i64 1)
%ae52966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57505 = alloca %struct.ScmObj*, align 8
%fptrToInt57506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52967 to i64
%ae52967 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57506)
store volatile %struct.ScmObj* %ae52967, %struct.ScmObj** %stackaddr$makeclosure57505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52967, %struct.ScmObj* %_37append50911, i64 0)
%argslist56908$ae529650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57507 = alloca %struct.ScmObj*, align 8
%argslist56908$ae529651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52967, %struct.ScmObj* %argslist56908$ae529650)
store volatile %struct.ScmObj* %argslist56908$ae529651, %struct.ScmObj** %stackaddr$prim57507, align 8
%stackaddr$prim57508 = alloca %struct.ScmObj*, align 8
%argslist56908$ae529652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52966, %struct.ScmObj* %argslist56908$ae529651)
store volatile %struct.ScmObj* %argslist56908$ae529652, %struct.ScmObj** %stackaddr$prim57508, align 8
%clofunc57509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52965)
musttail call tailcc void %clofunc57509(%struct.ScmObj* %ae52965, %struct.ScmObj* %argslist56908$ae529652)
ret void
}

define tailcc void @proc_clo$ae52965(%struct.ScmObj* %env$ae52965,%struct.ScmObj* %current_45args56743) {
%stackaddr$env-ref57510 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52965, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57510
%stackaddr$env-ref57511 = alloca %struct.ScmObj*, align 8
%_37append50911 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52965, i64 1)
store %struct.ScmObj* %_37append50911, %struct.ScmObj** %stackaddr$env-ref57511
%stackaddr$prim57512 = alloca %struct.ScmObj*, align 8
%_95k51070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56743)
store volatile %struct.ScmObj* %_95k51070, %struct.ScmObj** %stackaddr$prim57512, align 8
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%current_45args56744 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56743)
store volatile %struct.ScmObj* %current_45args56744, %struct.ScmObj** %stackaddr$prim57513, align 8
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%anf_45bind51011 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56744)
store volatile %struct.ScmObj* %anf_45bind51011, %struct.ScmObj** %stackaddr$prim57514, align 8
%ae53033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%_95050912 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append50911, %struct.ScmObj* %ae53033, %struct.ScmObj* %anf_45bind51011)
store volatile %struct.ScmObj* %_95050912, %struct.ScmObj** %stackaddr$prim57515, align 8
%ae53036 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57516 = alloca %struct.ScmObj*, align 8
%_37append50910 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append50911, %struct.ScmObj* %ae53036)
store volatile %struct.ScmObj* %_37append50910, %struct.ScmObj** %stackaddr$prim57516, align 8
%stackaddr$makeclosure57517 = alloca %struct.ScmObj*, align 8
%fptrToInt57518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53037 to i64
%ae53037 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57518)
store volatile %struct.ScmObj* %ae53037, %struct.ScmObj** %stackaddr$makeclosure57517, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53037, %struct.ScmObj* %_37foldl150823, i64 0)
%ae53038 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57519 = alloca %struct.ScmObj*, align 8
%fptrToInt57520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53039 to i64
%ae53039 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57520)
store volatile %struct.ScmObj* %ae53039, %struct.ScmObj** %stackaddr$makeclosure57519, align 8
%argslist56897$ae530370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57521 = alloca %struct.ScmObj*, align 8
%argslist56897$ae530371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53039, %struct.ScmObj* %argslist56897$ae530370)
store volatile %struct.ScmObj* %argslist56897$ae530371, %struct.ScmObj** %stackaddr$prim57521, align 8
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%argslist56897$ae530372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53038, %struct.ScmObj* %argslist56897$ae530371)
store volatile %struct.ScmObj* %argslist56897$ae530372, %struct.ScmObj** %stackaddr$prim57522, align 8
%clofunc57523 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53037)
musttail call tailcc void %clofunc57523(%struct.ScmObj* %ae53037, %struct.ScmObj* %argslist56897$ae530372)
ret void
}

define tailcc void @proc_clo$ae53037(%struct.ScmObj* %env$ae53037,%struct.ScmObj* %current_45args56746) {
%stackaddr$env-ref57524 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53037, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57524
%stackaddr$prim57525 = alloca %struct.ScmObj*, align 8
%_95k51071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56746)
store volatile %struct.ScmObj* %_95k51071, %struct.ScmObj** %stackaddr$prim57525, align 8
%stackaddr$prim57526 = alloca %struct.ScmObj*, align 8
%current_45args56747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56746)
store volatile %struct.ScmObj* %current_45args56747, %struct.ScmObj** %stackaddr$prim57526, align 8
%stackaddr$prim57527 = alloca %struct.ScmObj*, align 8
%_37list_6350903 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56747)
store volatile %struct.ScmObj* %_37list_6350903, %struct.ScmObj** %stackaddr$prim57527, align 8
%stackaddr$makeclosure57528 = alloca %struct.ScmObj*, align 8
%fptrToInt57529 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53453 to i64
%ae53453 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57529)
store volatile %struct.ScmObj* %ae53453, %struct.ScmObj** %stackaddr$makeclosure57528, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53453, %struct.ScmObj* %_37foldl150823, i64 0)
%ae53454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57530 = alloca %struct.ScmObj*, align 8
%fptrToInt57531 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53455 to i64
%ae53455 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57531)
store volatile %struct.ScmObj* %ae53455, %struct.ScmObj** %stackaddr$makeclosure57530, align 8
%argslist56872$ae534530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57532 = alloca %struct.ScmObj*, align 8
%argslist56872$ae534531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53455, %struct.ScmObj* %argslist56872$ae534530)
store volatile %struct.ScmObj* %argslist56872$ae534531, %struct.ScmObj** %stackaddr$prim57532, align 8
%stackaddr$prim57533 = alloca %struct.ScmObj*, align 8
%argslist56872$ae534532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53454, %struct.ScmObj* %argslist56872$ae534531)
store volatile %struct.ScmObj* %argslist56872$ae534532, %struct.ScmObj** %stackaddr$prim57533, align 8
%clofunc57534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53453)
musttail call tailcc void %clofunc57534(%struct.ScmObj* %ae53453, %struct.ScmObj* %argslist56872$ae534532)
ret void
}

define tailcc void @proc_clo$ae53453(%struct.ScmObj* %env$ae53453,%struct.ScmObj* %current_45args56749) {
%stackaddr$env-ref57535 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53453, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57535
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%_95k51072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56749)
store volatile %struct.ScmObj* %_95k51072, %struct.ScmObj** %stackaddr$prim57536, align 8
%stackaddr$prim57537 = alloca %struct.ScmObj*, align 8
%current_45args56750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56749)
store volatile %struct.ScmObj* %current_45args56750, %struct.ScmObj** %stackaddr$prim57537, align 8
%stackaddr$prim57538 = alloca %struct.ScmObj*, align 8
%_37drop50894 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56750)
store volatile %struct.ScmObj* %_37drop50894, %struct.ScmObj** %stackaddr$prim57538, align 8
%stackaddr$makeclosure57539 = alloca %struct.ScmObj*, align 8
%fptrToInt57540 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53989 to i64
%ae53989 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57540)
store volatile %struct.ScmObj* %ae53989, %struct.ScmObj** %stackaddr$makeclosure57539, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53989, %struct.ScmObj* %_37foldl150823, i64 0)
%ae53990 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57541 = alloca %struct.ScmObj*, align 8
%fptrToInt57542 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53991 to i64
%ae53991 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57542)
store volatile %struct.ScmObj* %ae53991, %struct.ScmObj** %stackaddr$makeclosure57541, align 8
%argslist56848$ae539890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57543 = alloca %struct.ScmObj*, align 8
%argslist56848$ae539891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53991, %struct.ScmObj* %argslist56848$ae539890)
store volatile %struct.ScmObj* %argslist56848$ae539891, %struct.ScmObj** %stackaddr$prim57543, align 8
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%argslist56848$ae539892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53990, %struct.ScmObj* %argslist56848$ae539891)
store volatile %struct.ScmObj* %argslist56848$ae539892, %struct.ScmObj** %stackaddr$prim57544, align 8
%clofunc57545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53989)
musttail call tailcc void %clofunc57545(%struct.ScmObj* %ae53989, %struct.ScmObj* %argslist56848$ae539892)
ret void
}

define tailcc void @proc_clo$ae53989(%struct.ScmObj* %env$ae53989,%struct.ScmObj* %current_45args56752) {
%stackaddr$env-ref57546 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53989, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57546
%stackaddr$prim57547 = alloca %struct.ScmObj*, align 8
%_95k51073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56752)
store volatile %struct.ScmObj* %_95k51073, %struct.ScmObj** %stackaddr$prim57547, align 8
%stackaddr$prim57548 = alloca %struct.ScmObj*, align 8
%current_45args56753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56752)
store volatile %struct.ScmObj* %current_45args56753, %struct.ScmObj** %stackaddr$prim57548, align 8
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%_37memv50887 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56753)
store volatile %struct.ScmObj* %_37memv50887, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$makeclosure57550 = alloca %struct.ScmObj*, align 8
%fptrToInt57551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54393 to i64
%ae54393 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57551)
store volatile %struct.ScmObj* %ae54393, %struct.ScmObj** %stackaddr$makeclosure57550, align 8
%ae54394 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57552 = alloca %struct.ScmObj*, align 8
%fptrToInt57553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54395 to i64
%ae54395 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57553)
store volatile %struct.ScmObj* %ae54395, %struct.ScmObj** %stackaddr$makeclosure57552, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae54395, %struct.ScmObj* %_37foldl150823, i64 0)
%argslist56822$ae543930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%argslist56822$ae543931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54395, %struct.ScmObj* %argslist56822$ae543930)
store volatile %struct.ScmObj* %argslist56822$ae543931, %struct.ScmObj** %stackaddr$prim57554, align 8
%stackaddr$prim57555 = alloca %struct.ScmObj*, align 8
%argslist56822$ae543932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54394, %struct.ScmObj* %argslist56822$ae543931)
store volatile %struct.ScmObj* %argslist56822$ae543932, %struct.ScmObj** %stackaddr$prim57555, align 8
%clofunc57556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54393)
musttail call tailcc void %clofunc57556(%struct.ScmObj* %ae54393, %struct.ScmObj* %argslist56822$ae543932)
ret void
}

define tailcc void @proc_clo$ae54393(%struct.ScmObj* %env$ae54393,%struct.ScmObj* %current_45args56755) {
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%_95k51074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56755)
store volatile %struct.ScmObj* %_95k51074, %struct.ScmObj** %stackaddr$prim57557, align 8
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%current_45args56756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56755)
store volatile %struct.ScmObj* %current_45args56756, %struct.ScmObj** %stackaddr$prim57558, align 8
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%_37_4750883 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56756)
store volatile %struct.ScmObj* %_37_4750883, %struct.ScmObj** %stackaddr$prim57559, align 8
%stackaddr$makeclosure57560 = alloca %struct.ScmObj*, align 8
%fptrToInt57561 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54491 to i64
%ae54491 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57561)
store volatile %struct.ScmObj* %ae54491, %struct.ScmObj** %stackaddr$makeclosure57560, align 8
%ae54492 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57562 = alloca %struct.ScmObj*, align 8
%fptrToInt57563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54493 to i64
%ae54493 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57563)
store volatile %struct.ScmObj* %ae54493, %struct.ScmObj** %stackaddr$makeclosure57562, align 8
%argslist56809$ae544910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%argslist56809$ae544911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54493, %struct.ScmObj* %argslist56809$ae544910)
store volatile %struct.ScmObj* %argslist56809$ae544911, %struct.ScmObj** %stackaddr$prim57564, align 8
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%argslist56809$ae544912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54492, %struct.ScmObj* %argslist56809$ae544911)
store volatile %struct.ScmObj* %argslist56809$ae544912, %struct.ScmObj** %stackaddr$prim57565, align 8
%clofunc57566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54491)
musttail call tailcc void %clofunc57566(%struct.ScmObj* %ae54491, %struct.ScmObj* %argslist56809$ae544912)
ret void
}

define tailcc void @proc_clo$ae54491(%struct.ScmObj* %env$ae54491,%struct.ScmObj* %current_45args56758) {
%stackaddr$prim57567 = alloca %struct.ScmObj*, align 8
%_95k51075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56758)
store volatile %struct.ScmObj* %_95k51075, %struct.ScmObj** %stackaddr$prim57567, align 8
%stackaddr$prim57568 = alloca %struct.ScmObj*, align 8
%current_45args56759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56758)
store volatile %struct.ScmObj* %current_45args56759, %struct.ScmObj** %stackaddr$prim57568, align 8
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%_37first50881 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56759)
store volatile %struct.ScmObj* %_37first50881, %struct.ScmObj** %stackaddr$prim57569, align 8
%stackaddr$makeclosure57570 = alloca %struct.ScmObj*, align 8
%fptrToInt57571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54511 to i64
%ae54511 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57571)
store volatile %struct.ScmObj* %ae54511, %struct.ScmObj** %stackaddr$makeclosure57570, align 8
%ae54512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57572 = alloca %struct.ScmObj*, align 8
%fptrToInt57573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54513 to i64
%ae54513 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57573)
store volatile %struct.ScmObj* %ae54513, %struct.ScmObj** %stackaddr$makeclosure57572, align 8
%argslist56804$ae545110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57574 = alloca %struct.ScmObj*, align 8
%argslist56804$ae545111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54513, %struct.ScmObj* %argslist56804$ae545110)
store volatile %struct.ScmObj* %argslist56804$ae545111, %struct.ScmObj** %stackaddr$prim57574, align 8
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%argslist56804$ae545112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54512, %struct.ScmObj* %argslist56804$ae545111)
store volatile %struct.ScmObj* %argslist56804$ae545112, %struct.ScmObj** %stackaddr$prim57575, align 8
%clofunc57576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54511)
musttail call tailcc void %clofunc57576(%struct.ScmObj* %ae54511, %struct.ScmObj* %argslist56804$ae545112)
ret void
}

define tailcc void @proc_clo$ae54511(%struct.ScmObj* %env$ae54511,%struct.ScmObj* %current_45args56761) {
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%_95k51076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %_95k51076, %struct.ScmObj** %stackaddr$prim57577, align 8
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%current_45args56762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %current_45args56762, %struct.ScmObj** %stackaddr$prim57578, align 8
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%_37second50879 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56762)
store volatile %struct.ScmObj* %_37second50879, %struct.ScmObj** %stackaddr$prim57579, align 8
%stackaddr$makeclosure57580 = alloca %struct.ScmObj*, align 8
%fptrToInt57581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54533 to i64
%ae54533 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57581)
store volatile %struct.ScmObj* %ae54533, %struct.ScmObj** %stackaddr$makeclosure57580, align 8
%ae54534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57582 = alloca %struct.ScmObj*, align 8
%fptrToInt57583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54535 to i64
%ae54535 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57583)
store volatile %struct.ScmObj* %ae54535, %struct.ScmObj** %stackaddr$makeclosure57582, align 8
%argslist56799$ae545330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%argslist56799$ae545331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54535, %struct.ScmObj* %argslist56799$ae545330)
store volatile %struct.ScmObj* %argslist56799$ae545331, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%argslist56799$ae545332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54534, %struct.ScmObj* %argslist56799$ae545331)
store volatile %struct.ScmObj* %argslist56799$ae545332, %struct.ScmObj** %stackaddr$prim57585, align 8
%clofunc57586 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54533)
musttail call tailcc void %clofunc57586(%struct.ScmObj* %ae54533, %struct.ScmObj* %argslist56799$ae545332)
ret void
}

define tailcc void @proc_clo$ae54533(%struct.ScmObj* %env$ae54533,%struct.ScmObj* %current_45args56764) {
%stackaddr$prim57587 = alloca %struct.ScmObj*, align 8
%_95k51077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56764)
store volatile %struct.ScmObj* %_95k51077, %struct.ScmObj** %stackaddr$prim57587, align 8
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%current_45args56765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56764)
store volatile %struct.ScmObj* %current_45args56765, %struct.ScmObj** %stackaddr$prim57588, align 8
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%_37third50877 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56765)
store volatile %struct.ScmObj* %_37third50877, %struct.ScmObj** %stackaddr$prim57589, align 8
%stackaddr$makeclosure57590 = alloca %struct.ScmObj*, align 8
%fptrToInt57591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54557 to i64
%ae54557 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57591)
store volatile %struct.ScmObj* %ae54557, %struct.ScmObj** %stackaddr$makeclosure57590, align 8
%ae54558 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57592 = alloca %struct.ScmObj*, align 8
%fptrToInt57593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54559 to i64
%ae54559 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57593)
store volatile %struct.ScmObj* %ae54559, %struct.ScmObj** %stackaddr$makeclosure57592, align 8
%argslist56794$ae545570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%argslist56794$ae545571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54559, %struct.ScmObj* %argslist56794$ae545570)
store volatile %struct.ScmObj* %argslist56794$ae545571, %struct.ScmObj** %stackaddr$prim57594, align 8
%stackaddr$prim57595 = alloca %struct.ScmObj*, align 8
%argslist56794$ae545572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54558, %struct.ScmObj* %argslist56794$ae545571)
store volatile %struct.ScmObj* %argslist56794$ae545572, %struct.ScmObj** %stackaddr$prim57595, align 8
%clofunc57596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54557)
musttail call tailcc void %clofunc57596(%struct.ScmObj* %ae54557, %struct.ScmObj* %argslist56794$ae545572)
ret void
}

define tailcc void @proc_clo$ae54557(%struct.ScmObj* %env$ae54557,%struct.ScmObj* %current_45args56767) {
%stackaddr$prim57597 = alloca %struct.ScmObj*, align 8
%_95k51078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56767)
store volatile %struct.ScmObj* %_95k51078, %struct.ScmObj** %stackaddr$prim57597, align 8
%stackaddr$prim57598 = alloca %struct.ScmObj*, align 8
%current_45args56768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56767)
store volatile %struct.ScmObj* %current_45args56768, %struct.ScmObj** %stackaddr$prim57598, align 8
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%_37fourth50875 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56768)
store volatile %struct.ScmObj* %_37fourth50875, %struct.ScmObj** %stackaddr$prim57599, align 8
%ae54583 = call %struct.ScmObj* @const_init_false()
%truthy$cmp57600 = call i64 @is_truthy_value(%struct.ScmObj* %ae54583)
%cmp$cmp57600 = icmp eq i64 %truthy$cmp57600, 1
br i1 %cmp$cmp57600, label %truebranch$cmp57600, label %falsebranch$cmp57600
truebranch$cmp57600:
%stackaddr$makeclosure57601 = alloca %struct.ScmObj*, align 8
%fptrToInt57602 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54584 to i64
%ae54584 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57602)
store volatile %struct.ScmObj* %ae54584, %struct.ScmObj** %stackaddr$makeclosure57601, align 8
%ae54585 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54586 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56774$ae545840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57603 = alloca %struct.ScmObj*, align 8
%argslist56774$ae545841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54586, %struct.ScmObj* %argslist56774$ae545840)
store volatile %struct.ScmObj* %argslist56774$ae545841, %struct.ScmObj** %stackaddr$prim57603, align 8
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist56774$ae545842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54585, %struct.ScmObj* %argslist56774$ae545841)
store volatile %struct.ScmObj* %argslist56774$ae545842, %struct.ScmObj** %stackaddr$prim57604, align 8
%clofunc57605 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54584)
musttail call tailcc void %clofunc57605(%struct.ScmObj* %ae54584, %struct.ScmObj* %argslist56774$ae545842)
ret void
falsebranch$cmp57600:
%ae54599 = call %struct.ScmObj* @const_init_false()
%truthy$cmp57606 = call i64 @is_truthy_value(%struct.ScmObj* %ae54599)
%cmp$cmp57606 = icmp eq i64 %truthy$cmp57606, 1
br i1 %cmp$cmp57606, label %truebranch$cmp57606, label %falsebranch$cmp57606
truebranch$cmp57606:
%stackaddr$makeclosure57607 = alloca %struct.ScmObj*, align 8
%fptrToInt57608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54600 to i64
%ae54600 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57608)
store volatile %struct.ScmObj* %ae54600, %struct.ScmObj** %stackaddr$makeclosure57607, align 8
%ae54601 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54602 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist56779$ae546000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57609 = alloca %struct.ScmObj*, align 8
%argslist56779$ae546001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54602, %struct.ScmObj* %argslist56779$ae546000)
store volatile %struct.ScmObj* %argslist56779$ae546001, %struct.ScmObj** %stackaddr$prim57609, align 8
%stackaddr$prim57610 = alloca %struct.ScmObj*, align 8
%argslist56779$ae546002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54601, %struct.ScmObj* %argslist56779$ae546001)
store volatile %struct.ScmObj* %argslist56779$ae546002, %struct.ScmObj** %stackaddr$prim57610, align 8
%clofunc57611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54600)
musttail call tailcc void %clofunc57611(%struct.ScmObj* %ae54600, %struct.ScmObj* %argslist56779$ae546002)
ret void
falsebranch$cmp57606:
%ae54615 = call %struct.ScmObj* @const_init_true()
%truthy$cmp57612 = call i64 @is_truthy_value(%struct.ScmObj* %ae54615)
%cmp$cmp57612 = icmp eq i64 %truthy$cmp57612, 1
br i1 %cmp$cmp57612, label %truebranch$cmp57612, label %falsebranch$cmp57612
truebranch$cmp57612:
%stackaddr$makeclosure57613 = alloca %struct.ScmObj*, align 8
%fptrToInt57614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54616 to i64
%ae54616 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57614)
store volatile %struct.ScmObj* %ae54616, %struct.ScmObj** %stackaddr$makeclosure57613, align 8
%ae54617 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54618 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist56784$ae546160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%argslist56784$ae546161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54618, %struct.ScmObj* %argslist56784$ae546160)
store volatile %struct.ScmObj* %argslist56784$ae546161, %struct.ScmObj** %stackaddr$prim57615, align 8
%stackaddr$prim57616 = alloca %struct.ScmObj*, align 8
%argslist56784$ae546162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54617, %struct.ScmObj* %argslist56784$ae546161)
store volatile %struct.ScmObj* %argslist56784$ae546162, %struct.ScmObj** %stackaddr$prim57616, align 8
%clofunc57617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54616)
musttail call tailcc void %clofunc57617(%struct.ScmObj* %ae54616, %struct.ScmObj* %argslist56784$ae546162)
ret void
falsebranch$cmp57612:
%stackaddr$makeclosure57618 = alloca %struct.ScmObj*, align 8
%fptrToInt57619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54631 to i64
%ae54631 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57619)
store volatile %struct.ScmObj* %ae54631, %struct.ScmObj** %stackaddr$makeclosure57618, align 8
%ae54632 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54633 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist56789$ae546310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57620 = alloca %struct.ScmObj*, align 8
%argslist56789$ae546311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54633, %struct.ScmObj* %argslist56789$ae546310)
store volatile %struct.ScmObj* %argslist56789$ae546311, %struct.ScmObj** %stackaddr$prim57620, align 8
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%argslist56789$ae546312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54632, %struct.ScmObj* %argslist56789$ae546311)
store volatile %struct.ScmObj* %argslist56789$ae546312, %struct.ScmObj** %stackaddr$prim57621, align 8
%clofunc57622 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54631)
musttail call tailcc void %clofunc57622(%struct.ScmObj* %ae54631, %struct.ScmObj* %argslist56789$ae546312)
ret void
}

define tailcc void @proc_clo$ae54584(%struct.ScmObj* %env$ae54584,%struct.ScmObj* %current_45args56770) {
%stackaddr$prim57623 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56770)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57623, align 8
%stackaddr$prim57624 = alloca %struct.ScmObj*, align 8
%current_45args56771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56770)
store volatile %struct.ScmObj* %current_45args56771, %struct.ScmObj** %stackaddr$prim57624, align 8
%stackaddr$prim57625 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56771)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57625, align 8
%stackaddr$prim57626 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57626, align 8
%argslist56773$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%argslist56773$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56773$k0)
store volatile %struct.ScmObj* %argslist56773$k1, %struct.ScmObj** %stackaddr$prim57627, align 8
%clofunc57628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57628(%struct.ScmObj* %k, %struct.ScmObj* %argslist56773$k1)
ret void
}

define tailcc void @proc_clo$ae54600(%struct.ScmObj* %env$ae54600,%struct.ScmObj* %current_45args56775) {
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$prim57630 = alloca %struct.ScmObj*, align 8
%current_45args56776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %current_45args56776, %struct.ScmObj** %stackaddr$prim57630, align 8
%stackaddr$prim57631 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56776)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57631, align 8
%stackaddr$prim57632 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57632, align 8
%argslist56778$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%argslist56778$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56778$k0)
store volatile %struct.ScmObj* %argslist56778$k1, %struct.ScmObj** %stackaddr$prim57633, align 8
%clofunc57634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57634(%struct.ScmObj* %k, %struct.ScmObj* %argslist56778$k1)
ret void
}

define tailcc void @proc_clo$ae54616(%struct.ScmObj* %env$ae54616,%struct.ScmObj* %current_45args56780) {
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56780)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57635, align 8
%stackaddr$prim57636 = alloca %struct.ScmObj*, align 8
%current_45args56781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56780)
store volatile %struct.ScmObj* %current_45args56781, %struct.ScmObj** %stackaddr$prim57636, align 8
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56781)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57638, align 8
%argslist56783$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%argslist56783$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56783$k0)
store volatile %struct.ScmObj* %argslist56783$k1, %struct.ScmObj** %stackaddr$prim57639, align 8
%clofunc57640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57640(%struct.ScmObj* %k, %struct.ScmObj* %argslist56783$k1)
ret void
}

define tailcc void @proc_clo$ae54631(%struct.ScmObj* %env$ae54631,%struct.ScmObj* %current_45args56785) {
%stackaddr$prim57641 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56785)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57641, align 8
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%current_45args56786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56785)
store volatile %struct.ScmObj* %current_45args56786, %struct.ScmObj** %stackaddr$prim57642, align 8
%stackaddr$prim57643 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56786)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57643, align 8
%stackaddr$prim57644 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57644, align 8
%argslist56788$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57645 = alloca %struct.ScmObj*, align 8
%argslist56788$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56788$k0)
store volatile %struct.ScmObj* %argslist56788$k1, %struct.ScmObj** %stackaddr$prim57645, align 8
%clofunc57646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57646(%struct.ScmObj* %k, %struct.ScmObj* %argslist56788$k1)
ret void
}

define tailcc void @proc_clo$ae54559(%struct.ScmObj* %env$ae54559,%struct.ScmObj* %current_45args56790) {
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%k51079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56790)
store volatile %struct.ScmObj* %k51079, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%current_45args56791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56790)
store volatile %struct.ScmObj* %current_45args56791, %struct.ScmObj** %stackaddr$prim57648, align 8
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%x50876 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56791)
store volatile %struct.ScmObj* %x50876, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%anf_45bind51044 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x50876)
store volatile %struct.ScmObj* %anf_45bind51044, %struct.ScmObj** %stackaddr$prim57650, align 8
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%anf_45bind51045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51044)
store volatile %struct.ScmObj* %anf_45bind51045, %struct.ScmObj** %stackaddr$prim57651, align 8
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%anf_45bind51046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51045)
store volatile %struct.ScmObj* %anf_45bind51046, %struct.ScmObj** %stackaddr$prim57652, align 8
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%cpsprim51080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind51046)
store volatile %struct.ScmObj* %cpsprim51080, %struct.ScmObj** %stackaddr$prim57653, align 8
%ae54565 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56793$k510790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57654 = alloca %struct.ScmObj*, align 8
%argslist56793$k510791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51080, %struct.ScmObj* %argslist56793$k510790)
store volatile %struct.ScmObj* %argslist56793$k510791, %struct.ScmObj** %stackaddr$prim57654, align 8
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%argslist56793$k510792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54565, %struct.ScmObj* %argslist56793$k510791)
store volatile %struct.ScmObj* %argslist56793$k510792, %struct.ScmObj** %stackaddr$prim57655, align 8
%clofunc57656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51079)
musttail call tailcc void %clofunc57656(%struct.ScmObj* %k51079, %struct.ScmObj* %argslist56793$k510792)
ret void
}

define tailcc void @proc_clo$ae54535(%struct.ScmObj* %env$ae54535,%struct.ScmObj* %current_45args56795) {
%stackaddr$prim57657 = alloca %struct.ScmObj*, align 8
%k51081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56795)
store volatile %struct.ScmObj* %k51081, %struct.ScmObj** %stackaddr$prim57657, align 8
%stackaddr$prim57658 = alloca %struct.ScmObj*, align 8
%current_45args56796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56795)
store volatile %struct.ScmObj* %current_45args56796, %struct.ScmObj** %stackaddr$prim57658, align 8
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%x50878 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56796)
store volatile %struct.ScmObj* %x50878, %struct.ScmObj** %stackaddr$prim57659, align 8
%stackaddr$prim57660 = alloca %struct.ScmObj*, align 8
%anf_45bind51042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x50878)
store volatile %struct.ScmObj* %anf_45bind51042, %struct.ScmObj** %stackaddr$prim57660, align 8
%stackaddr$prim57661 = alloca %struct.ScmObj*, align 8
%anf_45bind51043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51042)
store volatile %struct.ScmObj* %anf_45bind51043, %struct.ScmObj** %stackaddr$prim57661, align 8
%stackaddr$prim57662 = alloca %struct.ScmObj*, align 8
%cpsprim51082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind51043)
store volatile %struct.ScmObj* %cpsprim51082, %struct.ScmObj** %stackaddr$prim57662, align 8
%ae54540 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56798$k510810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%argslist56798$k510811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51082, %struct.ScmObj* %argslist56798$k510810)
store volatile %struct.ScmObj* %argslist56798$k510811, %struct.ScmObj** %stackaddr$prim57663, align 8
%stackaddr$prim57664 = alloca %struct.ScmObj*, align 8
%argslist56798$k510812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54540, %struct.ScmObj* %argslist56798$k510811)
store volatile %struct.ScmObj* %argslist56798$k510812, %struct.ScmObj** %stackaddr$prim57664, align 8
%clofunc57665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51081)
musttail call tailcc void %clofunc57665(%struct.ScmObj* %k51081, %struct.ScmObj* %argslist56798$k510812)
ret void
}

define tailcc void @proc_clo$ae54513(%struct.ScmObj* %env$ae54513,%struct.ScmObj* %current_45args56800) {
%stackaddr$prim57666 = alloca %struct.ScmObj*, align 8
%k51083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56800)
store volatile %struct.ScmObj* %k51083, %struct.ScmObj** %stackaddr$prim57666, align 8
%stackaddr$prim57667 = alloca %struct.ScmObj*, align 8
%current_45args56801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56800)
store volatile %struct.ScmObj* %current_45args56801, %struct.ScmObj** %stackaddr$prim57667, align 8
%stackaddr$prim57668 = alloca %struct.ScmObj*, align 8
%x50880 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56801)
store volatile %struct.ScmObj* %x50880, %struct.ScmObj** %stackaddr$prim57668, align 8
%stackaddr$prim57669 = alloca %struct.ScmObj*, align 8
%anf_45bind51041 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x50880)
store volatile %struct.ScmObj* %anf_45bind51041, %struct.ScmObj** %stackaddr$prim57669, align 8
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%cpsprim51084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind51041)
store volatile %struct.ScmObj* %cpsprim51084, %struct.ScmObj** %stackaddr$prim57670, align 8
%ae54517 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56803$k510830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%argslist56803$k510831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51084, %struct.ScmObj* %argslist56803$k510830)
store volatile %struct.ScmObj* %argslist56803$k510831, %struct.ScmObj** %stackaddr$prim57671, align 8
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%argslist56803$k510832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54517, %struct.ScmObj* %argslist56803$k510831)
store volatile %struct.ScmObj* %argslist56803$k510832, %struct.ScmObj** %stackaddr$prim57672, align 8
%clofunc57673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51083)
musttail call tailcc void %clofunc57673(%struct.ScmObj* %k51083, %struct.ScmObj* %argslist56803$k510832)
ret void
}

define tailcc void @proc_clo$ae54493(%struct.ScmObj* %env$ae54493,%struct.ScmObj* %current_45args56805) {
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%k51085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56805)
store volatile %struct.ScmObj* %k51085, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%current_45args56806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56805)
store volatile %struct.ScmObj* %current_45args56806, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%x50882 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56806)
store volatile %struct.ScmObj* %x50882, %struct.ScmObj** %stackaddr$prim57676, align 8
%stackaddr$prim57677 = alloca %struct.ScmObj*, align 8
%cpsprim51086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x50882)
store volatile %struct.ScmObj* %cpsprim51086, %struct.ScmObj** %stackaddr$prim57677, align 8
%ae54496 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56808$k510850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%argslist56808$k510851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51086, %struct.ScmObj* %argslist56808$k510850)
store volatile %struct.ScmObj* %argslist56808$k510851, %struct.ScmObj** %stackaddr$prim57678, align 8
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%argslist56808$k510852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54496, %struct.ScmObj* %argslist56808$k510851)
store volatile %struct.ScmObj* %argslist56808$k510852, %struct.ScmObj** %stackaddr$prim57679, align 8
%clofunc57680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51085)
musttail call tailcc void %clofunc57680(%struct.ScmObj* %k51085, %struct.ScmObj* %argslist56808$k510852)
ret void
}

define tailcc void @proc_clo$ae54395(%struct.ScmObj* %env$ae54395,%struct.ScmObj* %args5088451087) {
%stackaddr$env-ref57681 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54395, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57681
%stackaddr$prim57682 = alloca %struct.ScmObj*, align 8
%k51088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args5088451087)
store volatile %struct.ScmObj* %k51088, %struct.ScmObj** %stackaddr$prim57682, align 8
%stackaddr$prim57683 = alloca %struct.ScmObj*, align 8
%args50884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args5088451087)
store volatile %struct.ScmObj* %args50884, %struct.ScmObj** %stackaddr$prim57683, align 8
%stackaddr$prim57684 = alloca %struct.ScmObj*, align 8
%anf_45bind51035 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args50884)
store volatile %struct.ScmObj* %anf_45bind51035, %struct.ScmObj** %stackaddr$prim57684, align 8
%truthy$cmp57685 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51035)
%cmp$cmp57685 = icmp eq i64 %truthy$cmp57685, 1
br i1 %cmp$cmp57685, label %truebranch$cmp57685, label %falsebranch$cmp57685
truebranch$cmp57685:
%ae54401 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54402 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56810$k510880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57686 = alloca %struct.ScmObj*, align 8
%argslist56810$k510881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54402, %struct.ScmObj* %argslist56810$k510880)
store volatile %struct.ScmObj* %argslist56810$k510881, %struct.ScmObj** %stackaddr$prim57686, align 8
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%argslist56810$k510882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54401, %struct.ScmObj* %argslist56810$k510881)
store volatile %struct.ScmObj* %argslist56810$k510882, %struct.ScmObj** %stackaddr$prim57687, align 8
%clofunc57688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51088)
musttail call tailcc void %clofunc57688(%struct.ScmObj* %k51088, %struct.ScmObj* %argslist56810$k510882)
ret void
falsebranch$cmp57685:
%stackaddr$prim57689 = alloca %struct.ScmObj*, align 8
%anf_45bind51036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50884)
store volatile %struct.ScmObj* %anf_45bind51036, %struct.ScmObj** %stackaddr$prim57689, align 8
%stackaddr$prim57690 = alloca %struct.ScmObj*, align 8
%anf_45bind51037 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind51036)
store volatile %struct.ScmObj* %anf_45bind51037, %struct.ScmObj** %stackaddr$prim57690, align 8
%truthy$cmp57691 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51037)
%cmp$cmp57691 = icmp eq i64 %truthy$cmp57691, 1
br i1 %cmp$cmp57691, label %truebranch$cmp57691, label %falsebranch$cmp57691
truebranch$cmp57691:
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%cpsprim51089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args50884)
store volatile %struct.ScmObj* %cpsprim51089, %struct.ScmObj** %stackaddr$prim57692, align 8
%ae54414 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56811$k510880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%argslist56811$k510881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51089, %struct.ScmObj* %argslist56811$k510880)
store volatile %struct.ScmObj* %argslist56811$k510881, %struct.ScmObj** %stackaddr$prim57693, align 8
%stackaddr$prim57694 = alloca %struct.ScmObj*, align 8
%argslist56811$k510882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54414, %struct.ScmObj* %argslist56811$k510881)
store volatile %struct.ScmObj* %argslist56811$k510882, %struct.ScmObj** %stackaddr$prim57694, align 8
%clofunc57695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51088)
musttail call tailcc void %clofunc57695(%struct.ScmObj* %k51088, %struct.ScmObj* %argslist56811$k510882)
ret void
falsebranch$cmp57691:
%stackaddr$makeclosure57696 = alloca %struct.ScmObj*, align 8
%fptrToInt57697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54419 to i64
%ae54419 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57697)
store volatile %struct.ScmObj* %ae54419, %struct.ScmObj** %stackaddr$makeclosure57696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae54419, %struct.ScmObj* %_37foldl150823, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae54419, %struct.ScmObj* %args50884, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae54419, %struct.ScmObj* %k51088, i64 2)
%ae54420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57698 = alloca %struct.ScmObj*, align 8
%fptrToInt57699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54421 to i64
%ae54421 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57699)
store volatile %struct.ScmObj* %ae54421, %struct.ScmObj** %stackaddr$makeclosure57698, align 8
%argslist56821$ae544190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%argslist56821$ae544191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54421, %struct.ScmObj* %argslist56821$ae544190)
store volatile %struct.ScmObj* %argslist56821$ae544191, %struct.ScmObj** %stackaddr$prim57700, align 8
%stackaddr$prim57701 = alloca %struct.ScmObj*, align 8
%argslist56821$ae544192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54420, %struct.ScmObj* %argslist56821$ae544191)
store volatile %struct.ScmObj* %argslist56821$ae544192, %struct.ScmObj** %stackaddr$prim57701, align 8
%clofunc57702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae54419)
musttail call tailcc void %clofunc57702(%struct.ScmObj* %ae54419, %struct.ScmObj* %argslist56821$ae544192)
ret void
}

define tailcc void @proc_clo$ae54419(%struct.ScmObj* %env$ae54419,%struct.ScmObj* %current_45args56812) {
%stackaddr$env-ref57703 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54419, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref57703
%stackaddr$env-ref57704 = alloca %struct.ScmObj*, align 8
%args50884 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54419, i64 1)
store %struct.ScmObj* %args50884, %struct.ScmObj** %stackaddr$env-ref57704
%stackaddr$env-ref57705 = alloca %struct.ScmObj*, align 8
%k51088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54419, i64 2)
store %struct.ScmObj* %k51088, %struct.ScmObj** %stackaddr$env-ref57705
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%_95k51090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56812)
store volatile %struct.ScmObj* %_95k51090, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%current_45args56813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56812)
store volatile %struct.ScmObj* %current_45args56813, %struct.ScmObj** %stackaddr$prim57707, align 8
%stackaddr$prim57708 = alloca %struct.ScmObj*, align 8
%anf_45bind51038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56813)
store volatile %struct.ScmObj* %anf_45bind51038, %struct.ScmObj** %stackaddr$prim57708, align 8
%stackaddr$prim57709 = alloca %struct.ScmObj*, align 8
%anf_45bind51039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args50884)
store volatile %struct.ScmObj* %anf_45bind51039, %struct.ScmObj** %stackaddr$prim57709, align 8
%stackaddr$prim57710 = alloca %struct.ScmObj*, align 8
%anf_45bind51040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50884)
store volatile %struct.ScmObj* %anf_45bind51040, %struct.ScmObj** %stackaddr$prim57710, align 8
%argslist56815$_37foldl1508230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%argslist56815$_37foldl1508231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51040, %struct.ScmObj* %argslist56815$_37foldl1508230)
store volatile %struct.ScmObj* %argslist56815$_37foldl1508231, %struct.ScmObj** %stackaddr$prim57711, align 8
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%argslist56815$_37foldl1508232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51039, %struct.ScmObj* %argslist56815$_37foldl1508231)
store volatile %struct.ScmObj* %argslist56815$_37foldl1508232, %struct.ScmObj** %stackaddr$prim57712, align 8
%stackaddr$prim57713 = alloca %struct.ScmObj*, align 8
%argslist56815$_37foldl1508233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51038, %struct.ScmObj* %argslist56815$_37foldl1508232)
store volatile %struct.ScmObj* %argslist56815$_37foldl1508233, %struct.ScmObj** %stackaddr$prim57713, align 8
%stackaddr$prim57714 = alloca %struct.ScmObj*, align 8
%argslist56815$_37foldl1508234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51088, %struct.ScmObj* %argslist56815$_37foldl1508233)
store volatile %struct.ScmObj* %argslist56815$_37foldl1508234, %struct.ScmObj** %stackaddr$prim57714, align 8
%clofunc57715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl150823)
musttail call tailcc void %clofunc57715(%struct.ScmObj* %_37foldl150823, %struct.ScmObj* %argslist56815$_37foldl1508234)
ret void
}

define tailcc void @proc_clo$ae54421(%struct.ScmObj* %env$ae54421,%struct.ScmObj* %current_45args56816) {
%stackaddr$prim57716 = alloca %struct.ScmObj*, align 8
%k51091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56816)
store volatile %struct.ScmObj* %k51091, %struct.ScmObj** %stackaddr$prim57716, align 8
%stackaddr$prim57717 = alloca %struct.ScmObj*, align 8
%current_45args56817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56816)
store volatile %struct.ScmObj* %current_45args56817, %struct.ScmObj** %stackaddr$prim57717, align 8
%stackaddr$prim57718 = alloca %struct.ScmObj*, align 8
%n50886 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56817)
store volatile %struct.ScmObj* %n50886, %struct.ScmObj** %stackaddr$prim57718, align 8
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%current_45args56818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56817)
store volatile %struct.ScmObj* %current_45args56818, %struct.ScmObj** %stackaddr$prim57719, align 8
%stackaddr$prim57720 = alloca %struct.ScmObj*, align 8
%v50885 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56818)
store volatile %struct.ScmObj* %v50885, %struct.ScmObj** %stackaddr$prim57720, align 8
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%cpsprim51092 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v50885, %struct.ScmObj* %n50886)
store volatile %struct.ScmObj* %cpsprim51092, %struct.ScmObj** %stackaddr$prim57721, align 8
%ae54425 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56820$k510910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%argslist56820$k510911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51092, %struct.ScmObj* %argslist56820$k510910)
store volatile %struct.ScmObj* %argslist56820$k510911, %struct.ScmObj** %stackaddr$prim57722, align 8
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%argslist56820$k510912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54425, %struct.ScmObj* %argslist56820$k510911)
store volatile %struct.ScmObj* %argslist56820$k510912, %struct.ScmObj** %stackaddr$prim57723, align 8
%clofunc57724 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51091)
musttail call tailcc void %clofunc57724(%struct.ScmObj* %k51091, %struct.ScmObj* %argslist56820$k510912)
ret void
}

define tailcc void @proc_clo$ae53991(%struct.ScmObj* %env$ae53991,%struct.ScmObj* %current_45args56823) {
%stackaddr$prim57725 = alloca %struct.ScmObj*, align 8
%k51093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56823)
store volatile %struct.ScmObj* %k51093, %struct.ScmObj** %stackaddr$prim57725, align 8
%stackaddr$prim57726 = alloca %struct.ScmObj*, align 8
%current_45args56824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56823)
store volatile %struct.ScmObj* %current_45args56824, %struct.ScmObj** %stackaddr$prim57726, align 8
%stackaddr$prim57727 = alloca %struct.ScmObj*, align 8
%v50889 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56824)
store volatile %struct.ScmObj* %v50889, %struct.ScmObj** %stackaddr$prim57727, align 8
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%current_45args56825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56824)
store volatile %struct.ScmObj* %current_45args56825, %struct.ScmObj** %stackaddr$prim57728, align 8
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%lst50888 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56825)
store volatile %struct.ScmObj* %lst50888, %struct.ScmObj** %stackaddr$prim57729, align 8
%ae53992 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%lst50890 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae53992, %struct.ScmObj* %lst50888)
store volatile %struct.ScmObj* %lst50890, %struct.ScmObj** %stackaddr$prim57730, align 8
%stackaddr$makeclosure57731 = alloca %struct.ScmObj*, align 8
%fptrToInt57732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53994 to i64
%ae53994 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57732)
store volatile %struct.ScmObj* %ae53994, %struct.ScmObj** %stackaddr$makeclosure57731, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53994, %struct.ScmObj* %lst50890, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53994, %struct.ScmObj* %v50889, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53994, %struct.ScmObj* %k51093, i64 2)
%ae53995 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57733 = alloca %struct.ScmObj*, align 8
%fptrToInt57734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53996 to i64
%ae53996 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57734)
store volatile %struct.ScmObj* %ae53996, %struct.ScmObj** %stackaddr$makeclosure57733, align 8
%argslist56847$ae539940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57735 = alloca %struct.ScmObj*, align 8
%argslist56847$ae539941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53996, %struct.ScmObj* %argslist56847$ae539940)
store volatile %struct.ScmObj* %argslist56847$ae539941, %struct.ScmObj** %stackaddr$prim57735, align 8
%stackaddr$prim57736 = alloca %struct.ScmObj*, align 8
%argslist56847$ae539942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53995, %struct.ScmObj* %argslist56847$ae539941)
store volatile %struct.ScmObj* %argslist56847$ae539942, %struct.ScmObj** %stackaddr$prim57736, align 8
%clofunc57737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53994)
musttail call tailcc void %clofunc57737(%struct.ScmObj* %ae53994, %struct.ScmObj* %argslist56847$ae539942)
ret void
}

define tailcc void @proc_clo$ae53994(%struct.ScmObj* %env$ae53994,%struct.ScmObj* %current_45args56827) {
%stackaddr$env-ref57738 = alloca %struct.ScmObj*, align 8
%lst50890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53994, i64 0)
store %struct.ScmObj* %lst50890, %struct.ScmObj** %stackaddr$env-ref57738
%stackaddr$env-ref57739 = alloca %struct.ScmObj*, align 8
%v50889 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53994, i64 1)
store %struct.ScmObj* %v50889, %struct.ScmObj** %stackaddr$env-ref57739
%stackaddr$env-ref57740 = alloca %struct.ScmObj*, align 8
%k51093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53994, i64 2)
store %struct.ScmObj* %k51093, %struct.ScmObj** %stackaddr$env-ref57740
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%_95k51094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56827)
store volatile %struct.ScmObj* %_95k51094, %struct.ScmObj** %stackaddr$prim57741, align 8
%stackaddr$prim57742 = alloca %struct.ScmObj*, align 8
%current_45args56828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56827)
store volatile %struct.ScmObj* %current_45args56828, %struct.ScmObj** %stackaddr$prim57742, align 8
%stackaddr$prim57743 = alloca %struct.ScmObj*, align 8
%anf_45bind51027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56828)
store volatile %struct.ScmObj* %anf_45bind51027, %struct.ScmObj** %stackaddr$prim57743, align 8
%stackaddr$makeclosure57744 = alloca %struct.ScmObj*, align 8
%fptrToInt57745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54010 to i64
%ae54010 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57745)
store volatile %struct.ScmObj* %ae54010, %struct.ScmObj** %stackaddr$makeclosure57744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae54010, %struct.ScmObj* %lst50890, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae54010, %struct.ScmObj* %v50889, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae54010, %struct.ScmObj* %k51093, i64 2)
%stackaddr$makeclosure57746 = alloca %struct.ScmObj*, align 8
%fptrToInt57747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae54011 to i64
%ae54011 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57747)
store volatile %struct.ScmObj* %ae54011, %struct.ScmObj** %stackaddr$makeclosure57746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae54011, %struct.ScmObj* %lst50890, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae54011, %struct.ScmObj* %v50889, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae54011, %struct.ScmObj* %k51093, i64 2)
%argslist56842$anf_45bind510270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57748 = alloca %struct.ScmObj*, align 8
%argslist56842$anf_45bind510271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54011, %struct.ScmObj* %argslist56842$anf_45bind510270)
store volatile %struct.ScmObj* %argslist56842$anf_45bind510271, %struct.ScmObj** %stackaddr$prim57748, align 8
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%argslist56842$anf_45bind510272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54010, %struct.ScmObj* %argslist56842$anf_45bind510271)
store volatile %struct.ScmObj* %argslist56842$anf_45bind510272, %struct.ScmObj** %stackaddr$prim57749, align 8
%clofunc57750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind51027)
musttail call tailcc void %clofunc57750(%struct.ScmObj* %anf_45bind51027, %struct.ScmObj* %argslist56842$anf_45bind510272)
ret void
}

define tailcc void @proc_clo$ae54010(%struct.ScmObj* %env$ae54010,%struct.ScmObj* %current_45args56830) {
%stackaddr$env-ref57751 = alloca %struct.ScmObj*, align 8
%lst50890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54010, i64 0)
store %struct.ScmObj* %lst50890, %struct.ScmObj** %stackaddr$env-ref57751
%stackaddr$env-ref57752 = alloca %struct.ScmObj*, align 8
%v50889 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54010, i64 1)
store %struct.ScmObj* %v50889, %struct.ScmObj** %stackaddr$env-ref57752
%stackaddr$env-ref57753 = alloca %struct.ScmObj*, align 8
%k51093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54010, i64 2)
store %struct.ScmObj* %k51093, %struct.ScmObj** %stackaddr$env-ref57753
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%_95k51095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56830)
store volatile %struct.ScmObj* %_95k51095, %struct.ScmObj** %stackaddr$prim57754, align 8
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%current_45args56831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56830)
store volatile %struct.ScmObj* %current_45args56831, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$prim57756 = alloca %struct.ScmObj*, align 8
%cc50891 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56831)
store volatile %struct.ScmObj* %cc50891, %struct.ScmObj** %stackaddr$prim57756, align 8
%ae54119 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57757 = alloca %struct.ScmObj*, align 8
%anf_45bind51028 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54119)
store volatile %struct.ScmObj* %anf_45bind51028, %struct.ScmObj** %stackaddr$prim57757, align 8
%stackaddr$prim57758 = alloca %struct.ScmObj*, align 8
%anf_45bind51029 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind51028)
store volatile %struct.ScmObj* %anf_45bind51029, %struct.ScmObj** %stackaddr$prim57758, align 8
%truthy$cmp57759 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51029)
%cmp$cmp57759 = icmp eq i64 %truthy$cmp57759, 1
br i1 %cmp$cmp57759, label %truebranch$cmp57759, label %falsebranch$cmp57759
truebranch$cmp57759:
%ae54123 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54124 = call %struct.ScmObj* @const_init_false()
%argslist56833$k510930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%argslist56833$k510931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54124, %struct.ScmObj* %argslist56833$k510930)
store volatile %struct.ScmObj* %argslist56833$k510931, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56833$k510932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54123, %struct.ScmObj* %argslist56833$k510931)
store volatile %struct.ScmObj* %argslist56833$k510932, %struct.ScmObj** %stackaddr$prim57761, align 8
%clofunc57762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51093)
musttail call tailcc void %clofunc57762(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56833$k510932)
ret void
falsebranch$cmp57759:
%ae54132 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57763 = alloca %struct.ScmObj*, align 8
%anf_45bind51030 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54132)
store volatile %struct.ScmObj* %anf_45bind51030, %struct.ScmObj** %stackaddr$prim57763, align 8
%stackaddr$prim57764 = alloca %struct.ScmObj*, align 8
%anf_45bind51031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind51030)
store volatile %struct.ScmObj* %anf_45bind51031, %struct.ScmObj** %stackaddr$prim57764, align 8
%stackaddr$prim57765 = alloca %struct.ScmObj*, align 8
%anf_45bind51032 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind51031, %struct.ScmObj* %v50889)
store volatile %struct.ScmObj* %anf_45bind51032, %struct.ScmObj** %stackaddr$prim57765, align 8
%truthy$cmp57766 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51032)
%cmp$cmp57766 = icmp eq i64 %truthy$cmp57766, 1
br i1 %cmp$cmp57766, label %truebranch$cmp57766, label %falsebranch$cmp57766
truebranch$cmp57766:
%ae54138 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57767 = alloca %struct.ScmObj*, align 8
%cpsprim51096 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54138)
store volatile %struct.ScmObj* %cpsprim51096, %struct.ScmObj** %stackaddr$prim57767, align 8
%ae54140 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56834$k510930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57768 = alloca %struct.ScmObj*, align 8
%argslist56834$k510931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51096, %struct.ScmObj* %argslist56834$k510930)
store volatile %struct.ScmObj* %argslist56834$k510931, %struct.ScmObj** %stackaddr$prim57768, align 8
%stackaddr$prim57769 = alloca %struct.ScmObj*, align 8
%argslist56834$k510932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54140, %struct.ScmObj* %argslist56834$k510931)
store volatile %struct.ScmObj* %argslist56834$k510932, %struct.ScmObj** %stackaddr$prim57769, align 8
%clofunc57770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51093)
musttail call tailcc void %clofunc57770(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56834$k510932)
ret void
falsebranch$cmp57766:
%ae54151 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57771 = alloca %struct.ScmObj*, align 8
%anf_45bind51033 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54151)
store volatile %struct.ScmObj* %anf_45bind51033, %struct.ScmObj** %stackaddr$prim57771, align 8
%stackaddr$prim57772 = alloca %struct.ScmObj*, align 8
%anf_45bind51034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51033)
store volatile %struct.ScmObj* %anf_45bind51034, %struct.ScmObj** %stackaddr$prim57772, align 8
%ae54154 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%_95050893 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54154, %struct.ScmObj* %anf_45bind51034)
store volatile %struct.ScmObj* %_95050893, %struct.ScmObj** %stackaddr$prim57773, align 8
%argslist56835$cc508910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%argslist56835$cc508911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50891, %struct.ScmObj* %argslist56835$cc508910)
store volatile %struct.ScmObj* %argslist56835$cc508911, %struct.ScmObj** %stackaddr$prim57774, align 8
%stackaddr$prim57775 = alloca %struct.ScmObj*, align 8
%argslist56835$cc508912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56835$cc508911)
store volatile %struct.ScmObj* %argslist56835$cc508912, %struct.ScmObj** %stackaddr$prim57775, align 8
%clofunc57776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50891)
musttail call tailcc void %clofunc57776(%struct.ScmObj* %cc50891, %struct.ScmObj* %argslist56835$cc508912)
ret void
}

define tailcc void @proc_clo$ae54011(%struct.ScmObj* %env$ae54011,%struct.ScmObj* %current_45args56836) {
%stackaddr$env-ref57777 = alloca %struct.ScmObj*, align 8
%lst50890 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54011, i64 0)
store %struct.ScmObj* %lst50890, %struct.ScmObj** %stackaddr$env-ref57777
%stackaddr$env-ref57778 = alloca %struct.ScmObj*, align 8
%v50889 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54011, i64 1)
store %struct.ScmObj* %v50889, %struct.ScmObj** %stackaddr$env-ref57778
%stackaddr$env-ref57779 = alloca %struct.ScmObj*, align 8
%k51093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae54011, i64 2)
store %struct.ScmObj* %k51093, %struct.ScmObj** %stackaddr$env-ref57779
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%_95k51095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56836)
store volatile %struct.ScmObj* %_95k51095, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%current_45args56837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56836)
store volatile %struct.ScmObj* %current_45args56837, %struct.ScmObj** %stackaddr$prim57781, align 8
%stackaddr$prim57782 = alloca %struct.ScmObj*, align 8
%cc50891 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56837)
store volatile %struct.ScmObj* %cc50891, %struct.ScmObj** %stackaddr$prim57782, align 8
%ae54013 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%anf_45bind51028 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54013)
store volatile %struct.ScmObj* %anf_45bind51028, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$prim57784 = alloca %struct.ScmObj*, align 8
%anf_45bind51029 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind51028)
store volatile %struct.ScmObj* %anf_45bind51029, %struct.ScmObj** %stackaddr$prim57784, align 8
%truthy$cmp57785 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51029)
%cmp$cmp57785 = icmp eq i64 %truthy$cmp57785, 1
br i1 %cmp$cmp57785, label %truebranch$cmp57785, label %falsebranch$cmp57785
truebranch$cmp57785:
%ae54017 = call %struct.ScmObj* @const_init_int(i64 0)
%ae54018 = call %struct.ScmObj* @const_init_false()
%argslist56839$k510930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%argslist56839$k510931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54018, %struct.ScmObj* %argslist56839$k510930)
store volatile %struct.ScmObj* %argslist56839$k510931, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%argslist56839$k510932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54017, %struct.ScmObj* %argslist56839$k510931)
store volatile %struct.ScmObj* %argslist56839$k510932, %struct.ScmObj** %stackaddr$prim57787, align 8
%clofunc57788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51093)
musttail call tailcc void %clofunc57788(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56839$k510932)
ret void
falsebranch$cmp57785:
%ae54026 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%anf_45bind51030 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54026)
store volatile %struct.ScmObj* %anf_45bind51030, %struct.ScmObj** %stackaddr$prim57789, align 8
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%anf_45bind51031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind51030)
store volatile %struct.ScmObj* %anf_45bind51031, %struct.ScmObj** %stackaddr$prim57790, align 8
%stackaddr$prim57791 = alloca %struct.ScmObj*, align 8
%anf_45bind51032 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind51031, %struct.ScmObj* %v50889)
store volatile %struct.ScmObj* %anf_45bind51032, %struct.ScmObj** %stackaddr$prim57791, align 8
%truthy$cmp57792 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51032)
%cmp$cmp57792 = icmp eq i64 %truthy$cmp57792, 1
br i1 %cmp$cmp57792, label %truebranch$cmp57792, label %falsebranch$cmp57792
truebranch$cmp57792:
%ae54032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%cpsprim51096 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54032)
store volatile %struct.ScmObj* %cpsprim51096, %struct.ScmObj** %stackaddr$prim57793, align 8
%ae54034 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56840$k510930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%argslist56840$k510931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51096, %struct.ScmObj* %argslist56840$k510930)
store volatile %struct.ScmObj* %argslist56840$k510931, %struct.ScmObj** %stackaddr$prim57794, align 8
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%argslist56840$k510932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae54034, %struct.ScmObj* %argslist56840$k510931)
store volatile %struct.ScmObj* %argslist56840$k510932, %struct.ScmObj** %stackaddr$prim57795, align 8
%clofunc57796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51093)
musttail call tailcc void %clofunc57796(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56840$k510932)
ret void
falsebranch$cmp57792:
%ae54045 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%anf_45bind51033 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54045)
store volatile %struct.ScmObj* %anf_45bind51033, %struct.ScmObj** %stackaddr$prim57797, align 8
%stackaddr$prim57798 = alloca %struct.ScmObj*, align 8
%anf_45bind51034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51033)
store volatile %struct.ScmObj* %anf_45bind51034, %struct.ScmObj** %stackaddr$prim57798, align 8
%ae54048 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57799 = alloca %struct.ScmObj*, align 8
%_95050893 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst50890, %struct.ScmObj* %ae54048, %struct.ScmObj* %anf_45bind51034)
store volatile %struct.ScmObj* %_95050893, %struct.ScmObj** %stackaddr$prim57799, align 8
%argslist56841$cc508910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57800 = alloca %struct.ScmObj*, align 8
%argslist56841$cc508911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50891, %struct.ScmObj* %argslist56841$cc508910)
store volatile %struct.ScmObj* %argslist56841$cc508911, %struct.ScmObj** %stackaddr$prim57800, align 8
%stackaddr$prim57801 = alloca %struct.ScmObj*, align 8
%argslist56841$cc508912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51093, %struct.ScmObj* %argslist56841$cc508911)
store volatile %struct.ScmObj* %argslist56841$cc508912, %struct.ScmObj** %stackaddr$prim57801, align 8
%clofunc57802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50891)
musttail call tailcc void %clofunc57802(%struct.ScmObj* %cc50891, %struct.ScmObj* %argslist56841$cc508912)
ret void
}

define tailcc void @proc_clo$ae53996(%struct.ScmObj* %env$ae53996,%struct.ScmObj* %current_45args56843) {
%stackaddr$prim57803 = alloca %struct.ScmObj*, align 8
%k51097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56843)
store volatile %struct.ScmObj* %k51097, %struct.ScmObj** %stackaddr$prim57803, align 8
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%current_45args56844 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56843)
store volatile %struct.ScmObj* %current_45args56844, %struct.ScmObj** %stackaddr$prim57804, align 8
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%u50892 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56844)
store volatile %struct.ScmObj* %u50892, %struct.ScmObj** %stackaddr$prim57805, align 8
%argslist56846$u508920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57806 = alloca %struct.ScmObj*, align 8
%argslist56846$u508921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u50892, %struct.ScmObj* %argslist56846$u508920)
store volatile %struct.ScmObj* %argslist56846$u508921, %struct.ScmObj** %stackaddr$prim57806, align 8
%stackaddr$prim57807 = alloca %struct.ScmObj*, align 8
%argslist56846$u508922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51097, %struct.ScmObj* %argslist56846$u508921)
store volatile %struct.ScmObj* %argslist56846$u508922, %struct.ScmObj** %stackaddr$prim57807, align 8
%clofunc57808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u50892)
musttail call tailcc void %clofunc57808(%struct.ScmObj* %u50892, %struct.ScmObj* %argslist56846$u508922)
ret void
}

define tailcc void @proc_clo$ae53455(%struct.ScmObj* %env$ae53455,%struct.ScmObj* %current_45args56849) {
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%k51098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56849)
store volatile %struct.ScmObj* %k51098, %struct.ScmObj** %stackaddr$prim57809, align 8
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%current_45args56850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56849)
store volatile %struct.ScmObj* %current_45args56850, %struct.ScmObj** %stackaddr$prim57810, align 8
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%lst50896 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56850)
store volatile %struct.ScmObj* %lst50896, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%current_45args56851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56850)
store volatile %struct.ScmObj* %current_45args56851, %struct.ScmObj** %stackaddr$prim57812, align 8
%stackaddr$prim57813 = alloca %struct.ScmObj*, align 8
%n50895 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56851)
store volatile %struct.ScmObj* %n50895, %struct.ScmObj** %stackaddr$prim57813, align 8
%ae53456 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57814 = alloca %struct.ScmObj*, align 8
%n50898 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae53456, %struct.ScmObj* %n50895)
store volatile %struct.ScmObj* %n50898, %struct.ScmObj** %stackaddr$prim57814, align 8
%ae53458 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%lst50897 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae53458, %struct.ScmObj* %lst50896)
store volatile %struct.ScmObj* %lst50897, %struct.ScmObj** %stackaddr$prim57815, align 8
%stackaddr$makeclosure57816 = alloca %struct.ScmObj*, align 8
%fptrToInt57817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53460 to i64
%ae53460 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57817)
store volatile %struct.ScmObj* %ae53460, %struct.ScmObj** %stackaddr$makeclosure57816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53460, %struct.ScmObj* %k51098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53460, %struct.ScmObj* %n50898, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53460, %struct.ScmObj* %lst50897, i64 2)
%ae53461 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57818 = alloca %struct.ScmObj*, align 8
%fptrToInt57819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53462 to i64
%ae53462 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57819)
store volatile %struct.ScmObj* %ae53462, %struct.ScmObj** %stackaddr$makeclosure57818, align 8
%argslist56871$ae534600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%argslist56871$ae534601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53462, %struct.ScmObj* %argslist56871$ae534600)
store volatile %struct.ScmObj* %argslist56871$ae534601, %struct.ScmObj** %stackaddr$prim57820, align 8
%stackaddr$prim57821 = alloca %struct.ScmObj*, align 8
%argslist56871$ae534602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53461, %struct.ScmObj* %argslist56871$ae534601)
store volatile %struct.ScmObj* %argslist56871$ae534602, %struct.ScmObj** %stackaddr$prim57821, align 8
%clofunc57822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53460)
musttail call tailcc void %clofunc57822(%struct.ScmObj* %ae53460, %struct.ScmObj* %argslist56871$ae534602)
ret void
}

define tailcc void @proc_clo$ae53460(%struct.ScmObj* %env$ae53460,%struct.ScmObj* %current_45args56853) {
%stackaddr$env-ref57823 = alloca %struct.ScmObj*, align 8
%k51098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53460, i64 0)
store %struct.ScmObj* %k51098, %struct.ScmObj** %stackaddr$env-ref57823
%stackaddr$env-ref57824 = alloca %struct.ScmObj*, align 8
%n50898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53460, i64 1)
store %struct.ScmObj* %n50898, %struct.ScmObj** %stackaddr$env-ref57824
%stackaddr$env-ref57825 = alloca %struct.ScmObj*, align 8
%lst50897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53460, i64 2)
store %struct.ScmObj* %lst50897, %struct.ScmObj** %stackaddr$env-ref57825
%stackaddr$prim57826 = alloca %struct.ScmObj*, align 8
%_95k51099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56853)
store volatile %struct.ScmObj* %_95k51099, %struct.ScmObj** %stackaddr$prim57826, align 8
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%current_45args56854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56853)
store volatile %struct.ScmObj* %current_45args56854, %struct.ScmObj** %stackaddr$prim57827, align 8
%stackaddr$prim57828 = alloca %struct.ScmObj*, align 8
%anf_45bind51020 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56854)
store volatile %struct.ScmObj* %anf_45bind51020, %struct.ScmObj** %stackaddr$prim57828, align 8
%stackaddr$makeclosure57829 = alloca %struct.ScmObj*, align 8
%fptrToInt57830 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53476 to i64
%ae53476 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57830)
store volatile %struct.ScmObj* %ae53476, %struct.ScmObj** %stackaddr$makeclosure57829, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53476, %struct.ScmObj* %k51098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53476, %struct.ScmObj* %n50898, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53476, %struct.ScmObj* %lst50897, i64 2)
%stackaddr$makeclosure57831 = alloca %struct.ScmObj*, align 8
%fptrToInt57832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53477 to i64
%ae53477 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57832)
store volatile %struct.ScmObj* %ae53477, %struct.ScmObj** %stackaddr$makeclosure57831, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53477, %struct.ScmObj* %k51098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53477, %struct.ScmObj* %n50898, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae53477, %struct.ScmObj* %lst50897, i64 2)
%argslist56866$anf_45bind510200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%argslist56866$anf_45bind510201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53477, %struct.ScmObj* %argslist56866$anf_45bind510200)
store volatile %struct.ScmObj* %argslist56866$anf_45bind510201, %struct.ScmObj** %stackaddr$prim57833, align 8
%stackaddr$prim57834 = alloca %struct.ScmObj*, align 8
%argslist56866$anf_45bind510202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53476, %struct.ScmObj* %argslist56866$anf_45bind510201)
store volatile %struct.ScmObj* %argslist56866$anf_45bind510202, %struct.ScmObj** %stackaddr$prim57834, align 8
%clofunc57835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind51020)
musttail call tailcc void %clofunc57835(%struct.ScmObj* %anf_45bind51020, %struct.ScmObj* %argslist56866$anf_45bind510202)
ret void
}

define tailcc void @proc_clo$ae53476(%struct.ScmObj* %env$ae53476,%struct.ScmObj* %current_45args56856) {
%stackaddr$env-ref57836 = alloca %struct.ScmObj*, align 8
%k51098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53476, i64 0)
store %struct.ScmObj* %k51098, %struct.ScmObj** %stackaddr$env-ref57836
%stackaddr$env-ref57837 = alloca %struct.ScmObj*, align 8
%n50898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53476, i64 1)
store %struct.ScmObj* %n50898, %struct.ScmObj** %stackaddr$env-ref57837
%stackaddr$env-ref57838 = alloca %struct.ScmObj*, align 8
%lst50897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53476, i64 2)
store %struct.ScmObj* %lst50897, %struct.ScmObj** %stackaddr$env-ref57838
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%_95k51100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56856)
store volatile %struct.ScmObj* %_95k51100, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$prim57840 = alloca %struct.ScmObj*, align 8
%current_45args56857 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56856)
store volatile %struct.ScmObj* %current_45args56857, %struct.ScmObj** %stackaddr$prim57840, align 8
%stackaddr$prim57841 = alloca %struct.ScmObj*, align 8
%cc50899 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56857)
store volatile %struct.ScmObj* %cc50899, %struct.ScmObj** %stackaddr$prim57841, align 8
%ae53619 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57842 = alloca %struct.ScmObj*, align 8
%anf_45bind51021 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53619)
store volatile %struct.ScmObj* %anf_45bind51021, %struct.ScmObj** %stackaddr$prim57842, align 8
%ae53620 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57843 = alloca %struct.ScmObj*, align 8
%anf_45bind51022 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae53620, %struct.ScmObj* %anf_45bind51021)
store volatile %struct.ScmObj* %anf_45bind51022, %struct.ScmObj** %stackaddr$prim57843, align 8
%truthy$cmp57844 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51022)
%cmp$cmp57844 = icmp eq i64 %truthy$cmp57844, 1
br i1 %cmp$cmp57844, label %truebranch$cmp57844, label %falsebranch$cmp57844
truebranch$cmp57844:
%ae53624 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%cpsprim51101 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53624)
store volatile %struct.ScmObj* %cpsprim51101, %struct.ScmObj** %stackaddr$prim57845, align 8
%ae53626 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56859$k510980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57846 = alloca %struct.ScmObj*, align 8
%argslist56859$k510981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51101, %struct.ScmObj* %argslist56859$k510980)
store volatile %struct.ScmObj* %argslist56859$k510981, %struct.ScmObj** %stackaddr$prim57846, align 8
%stackaddr$prim57847 = alloca %struct.ScmObj*, align 8
%argslist56859$k510982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53626, %struct.ScmObj* %argslist56859$k510981)
store volatile %struct.ScmObj* %argslist56859$k510982, %struct.ScmObj** %stackaddr$prim57847, align 8
%clofunc57848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51098)
musttail call tailcc void %clofunc57848(%struct.ScmObj* %k51098, %struct.ScmObj* %argslist56859$k510982)
ret void
falsebranch$cmp57844:
%ae53637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%anf_45bind51023 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53637)
store volatile %struct.ScmObj* %anf_45bind51023, %struct.ScmObj** %stackaddr$prim57849, align 8
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%anf_45bind51024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51023)
store volatile %struct.ScmObj* %anf_45bind51024, %struct.ScmObj** %stackaddr$prim57850, align 8
%ae53640 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57851 = alloca %struct.ScmObj*, align 8
%_95050902 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53640, %struct.ScmObj* %anf_45bind51024)
store volatile %struct.ScmObj* %_95050902, %struct.ScmObj** %stackaddr$prim57851, align 8
%ae53643 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57852 = alloca %struct.ScmObj*, align 8
%anf_45bind51025 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53643)
store volatile %struct.ScmObj* %anf_45bind51025, %struct.ScmObj** %stackaddr$prim57852, align 8
%ae53645 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%anf_45bind51026 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind51025, %struct.ScmObj* %ae53645)
store volatile %struct.ScmObj* %anf_45bind51026, %struct.ScmObj** %stackaddr$prim57853, align 8
%ae53647 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57854 = alloca %struct.ScmObj*, align 8
%_95150901 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53647, %struct.ScmObj* %anf_45bind51026)
store volatile %struct.ScmObj* %_95150901, %struct.ScmObj** %stackaddr$prim57854, align 8
%argslist56860$cc508990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%argslist56860$cc508991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50899, %struct.ScmObj* %argslist56860$cc508990)
store volatile %struct.ScmObj* %argslist56860$cc508991, %struct.ScmObj** %stackaddr$prim57855, align 8
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%argslist56860$cc508992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51098, %struct.ScmObj* %argslist56860$cc508991)
store volatile %struct.ScmObj* %argslist56860$cc508992, %struct.ScmObj** %stackaddr$prim57856, align 8
%clofunc57857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50899)
musttail call tailcc void %clofunc57857(%struct.ScmObj* %cc50899, %struct.ScmObj* %argslist56860$cc508992)
ret void
}

define tailcc void @proc_clo$ae53477(%struct.ScmObj* %env$ae53477,%struct.ScmObj* %current_45args56861) {
%stackaddr$env-ref57858 = alloca %struct.ScmObj*, align 8
%k51098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53477, i64 0)
store %struct.ScmObj* %k51098, %struct.ScmObj** %stackaddr$env-ref57858
%stackaddr$env-ref57859 = alloca %struct.ScmObj*, align 8
%n50898 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53477, i64 1)
store %struct.ScmObj* %n50898, %struct.ScmObj** %stackaddr$env-ref57859
%stackaddr$env-ref57860 = alloca %struct.ScmObj*, align 8
%lst50897 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53477, i64 2)
store %struct.ScmObj* %lst50897, %struct.ScmObj** %stackaddr$env-ref57860
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%_95k51100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56861)
store volatile %struct.ScmObj* %_95k51100, %struct.ScmObj** %stackaddr$prim57861, align 8
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%current_45args56862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56861)
store volatile %struct.ScmObj* %current_45args56862, %struct.ScmObj** %stackaddr$prim57862, align 8
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%cc50899 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56862)
store volatile %struct.ScmObj* %cc50899, %struct.ScmObj** %stackaddr$prim57863, align 8
%ae53479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%anf_45bind51021 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53479)
store volatile %struct.ScmObj* %anf_45bind51021, %struct.ScmObj** %stackaddr$prim57864, align 8
%ae53480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%anf_45bind51022 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae53480, %struct.ScmObj* %anf_45bind51021)
store volatile %struct.ScmObj* %anf_45bind51022, %struct.ScmObj** %stackaddr$prim57865, align 8
%truthy$cmp57866 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51022)
%cmp$cmp57866 = icmp eq i64 %truthy$cmp57866, 1
br i1 %cmp$cmp57866, label %truebranch$cmp57866, label %falsebranch$cmp57866
truebranch$cmp57866:
%ae53484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57867 = alloca %struct.ScmObj*, align 8
%cpsprim51101 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53484)
store volatile %struct.ScmObj* %cpsprim51101, %struct.ScmObj** %stackaddr$prim57867, align 8
%ae53486 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56864$k510980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%argslist56864$k510981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51101, %struct.ScmObj* %argslist56864$k510980)
store volatile %struct.ScmObj* %argslist56864$k510981, %struct.ScmObj** %stackaddr$prim57868, align 8
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%argslist56864$k510982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53486, %struct.ScmObj* %argslist56864$k510981)
store volatile %struct.ScmObj* %argslist56864$k510982, %struct.ScmObj** %stackaddr$prim57869, align 8
%clofunc57870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51098)
musttail call tailcc void %clofunc57870(%struct.ScmObj* %k51098, %struct.ScmObj* %argslist56864$k510982)
ret void
falsebranch$cmp57866:
%ae53497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57871 = alloca %struct.ScmObj*, align 8
%anf_45bind51023 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53497)
store volatile %struct.ScmObj* %anf_45bind51023, %struct.ScmObj** %stackaddr$prim57871, align 8
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%anf_45bind51024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51023)
store volatile %struct.ScmObj* %anf_45bind51024, %struct.ScmObj** %stackaddr$prim57872, align 8
%ae53500 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%_95050902 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst50897, %struct.ScmObj* %ae53500, %struct.ScmObj* %anf_45bind51024)
store volatile %struct.ScmObj* %_95050902, %struct.ScmObj** %stackaddr$prim57873, align 8
%ae53503 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%anf_45bind51025 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53503)
store volatile %struct.ScmObj* %anf_45bind51025, %struct.ScmObj** %stackaddr$prim57874, align 8
%ae53505 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%anf_45bind51026 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind51025, %struct.ScmObj* %ae53505)
store volatile %struct.ScmObj* %anf_45bind51026, %struct.ScmObj** %stackaddr$prim57875, align 8
%ae53507 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57876 = alloca %struct.ScmObj*, align 8
%_95150901 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n50898, %struct.ScmObj* %ae53507, %struct.ScmObj* %anf_45bind51026)
store volatile %struct.ScmObj* %_95150901, %struct.ScmObj** %stackaddr$prim57876, align 8
%argslist56865$cc508990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%argslist56865$cc508991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50899, %struct.ScmObj* %argslist56865$cc508990)
store volatile %struct.ScmObj* %argslist56865$cc508991, %struct.ScmObj** %stackaddr$prim57877, align 8
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%argslist56865$cc508992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51098, %struct.ScmObj* %argslist56865$cc508991)
store volatile %struct.ScmObj* %argslist56865$cc508992, %struct.ScmObj** %stackaddr$prim57878, align 8
%clofunc57879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50899)
musttail call tailcc void %clofunc57879(%struct.ScmObj* %cc50899, %struct.ScmObj* %argslist56865$cc508992)
ret void
}

define tailcc void @proc_clo$ae53462(%struct.ScmObj* %env$ae53462,%struct.ScmObj* %current_45args56867) {
%stackaddr$prim57880 = alloca %struct.ScmObj*, align 8
%k51102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56867)
store volatile %struct.ScmObj* %k51102, %struct.ScmObj** %stackaddr$prim57880, align 8
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%current_45args56868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56867)
store volatile %struct.ScmObj* %current_45args56868, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$prim57882 = alloca %struct.ScmObj*, align 8
%u50900 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56868)
store volatile %struct.ScmObj* %u50900, %struct.ScmObj** %stackaddr$prim57882, align 8
%argslist56870$u509000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57883 = alloca %struct.ScmObj*, align 8
%argslist56870$u509001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u50900, %struct.ScmObj* %argslist56870$u509000)
store volatile %struct.ScmObj* %argslist56870$u509001, %struct.ScmObj** %stackaddr$prim57883, align 8
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%argslist56870$u509002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51102, %struct.ScmObj* %argslist56870$u509001)
store volatile %struct.ScmObj* %argslist56870$u509002, %struct.ScmObj** %stackaddr$prim57884, align 8
%clofunc57885 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u50900)
musttail call tailcc void %clofunc57885(%struct.ScmObj* %u50900, %struct.ScmObj* %argslist56870$u509002)
ret void
}

define tailcc void @proc_clo$ae53039(%struct.ScmObj* %env$ae53039,%struct.ScmObj* %current_45args56873) {
%stackaddr$prim57886 = alloca %struct.ScmObj*, align 8
%k51103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56873)
store volatile %struct.ScmObj* %k51103, %struct.ScmObj** %stackaddr$prim57886, align 8
%stackaddr$prim57887 = alloca %struct.ScmObj*, align 8
%current_45args56874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56873)
store volatile %struct.ScmObj* %current_45args56874, %struct.ScmObj** %stackaddr$prim57887, align 8
%stackaddr$prim57888 = alloca %struct.ScmObj*, align 8
%a50904 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56874)
store volatile %struct.ScmObj* %a50904, %struct.ScmObj** %stackaddr$prim57888, align 8
%ae53040 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%a50905 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae53040, %struct.ScmObj* %a50904)
store volatile %struct.ScmObj* %a50905, %struct.ScmObj** %stackaddr$prim57889, align 8
%stackaddr$makeclosure57890 = alloca %struct.ScmObj*, align 8
%fptrToInt57891 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53042 to i64
%ae53042 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57891)
store volatile %struct.ScmObj* %ae53042, %struct.ScmObj** %stackaddr$makeclosure57890, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53042, %struct.ScmObj* %a50905, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53042, %struct.ScmObj* %k51103, i64 1)
%ae53043 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57892 = alloca %struct.ScmObj*, align 8
%fptrToInt57893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53044 to i64
%ae53044 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57893)
store volatile %struct.ScmObj* %ae53044, %struct.ScmObj** %stackaddr$makeclosure57892, align 8
%argslist56896$ae530420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%argslist56896$ae530421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53044, %struct.ScmObj* %argslist56896$ae530420)
store volatile %struct.ScmObj* %argslist56896$ae530421, %struct.ScmObj** %stackaddr$prim57894, align 8
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%argslist56896$ae530422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53043, %struct.ScmObj* %argslist56896$ae530421)
store volatile %struct.ScmObj* %argslist56896$ae530422, %struct.ScmObj** %stackaddr$prim57895, align 8
%clofunc57896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53042)
musttail call tailcc void %clofunc57896(%struct.ScmObj* %ae53042, %struct.ScmObj* %argslist56896$ae530422)
ret void
}

define tailcc void @proc_clo$ae53042(%struct.ScmObj* %env$ae53042,%struct.ScmObj* %current_45args56876) {
%stackaddr$env-ref57897 = alloca %struct.ScmObj*, align 8
%a50905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53042, i64 0)
store %struct.ScmObj* %a50905, %struct.ScmObj** %stackaddr$env-ref57897
%stackaddr$env-ref57898 = alloca %struct.ScmObj*, align 8
%k51103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53042, i64 1)
store %struct.ScmObj* %k51103, %struct.ScmObj** %stackaddr$env-ref57898
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%_95k51104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56876)
store volatile %struct.ScmObj* %_95k51104, %struct.ScmObj** %stackaddr$prim57899, align 8
%stackaddr$prim57900 = alloca %struct.ScmObj*, align 8
%current_45args56877 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56876)
store volatile %struct.ScmObj* %current_45args56877, %struct.ScmObj** %stackaddr$prim57900, align 8
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%anf_45bind51012 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56877)
store volatile %struct.ScmObj* %anf_45bind51012, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$makeclosure57902 = alloca %struct.ScmObj*, align 8
%fptrToInt57903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53061 to i64
%ae53061 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57903)
store volatile %struct.ScmObj* %ae53061, %struct.ScmObj** %stackaddr$makeclosure57902, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53061, %struct.ScmObj* %a50905, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53061, %struct.ScmObj* %k51103, i64 1)
%stackaddr$makeclosure57904 = alloca %struct.ScmObj*, align 8
%fptrToInt57905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53062 to i64
%ae53062 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57905)
store volatile %struct.ScmObj* %ae53062, %struct.ScmObj** %stackaddr$makeclosure57904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53062, %struct.ScmObj* %a50905, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae53062, %struct.ScmObj* %k51103, i64 1)
%argslist56891$anf_45bind510120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57906 = alloca %struct.ScmObj*, align 8
%argslist56891$anf_45bind510121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53062, %struct.ScmObj* %argslist56891$anf_45bind510120)
store volatile %struct.ScmObj* %argslist56891$anf_45bind510121, %struct.ScmObj** %stackaddr$prim57906, align 8
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%argslist56891$anf_45bind510122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53061, %struct.ScmObj* %argslist56891$anf_45bind510121)
store volatile %struct.ScmObj* %argslist56891$anf_45bind510122, %struct.ScmObj** %stackaddr$prim57907, align 8
%clofunc57908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind51012)
musttail call tailcc void %clofunc57908(%struct.ScmObj* %anf_45bind51012, %struct.ScmObj* %argslist56891$anf_45bind510122)
ret void
}

define tailcc void @proc_clo$ae53061(%struct.ScmObj* %env$ae53061,%struct.ScmObj* %current_45args56879) {
%stackaddr$env-ref57909 = alloca %struct.ScmObj*, align 8
%a50905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53061, i64 0)
store %struct.ScmObj* %a50905, %struct.ScmObj** %stackaddr$env-ref57909
%stackaddr$env-ref57910 = alloca %struct.ScmObj*, align 8
%k51103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53061, i64 1)
store %struct.ScmObj* %k51103, %struct.ScmObj** %stackaddr$env-ref57910
%stackaddr$prim57911 = alloca %struct.ScmObj*, align 8
%_95k51105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56879)
store volatile %struct.ScmObj* %_95k51105, %struct.ScmObj** %stackaddr$prim57911, align 8
%stackaddr$prim57912 = alloca %struct.ScmObj*, align 8
%current_45args56880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56879)
store volatile %struct.ScmObj* %current_45args56880, %struct.ScmObj** %stackaddr$prim57912, align 8
%stackaddr$prim57913 = alloca %struct.ScmObj*, align 8
%cc50906 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56880)
store volatile %struct.ScmObj* %cc50906, %struct.ScmObj** %stackaddr$prim57913, align 8
%ae53177 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57914 = alloca %struct.ScmObj*, align 8
%anf_45bind51013 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53177)
store volatile %struct.ScmObj* %anf_45bind51013, %struct.ScmObj** %stackaddr$prim57914, align 8
%stackaddr$prim57915 = alloca %struct.ScmObj*, align 8
%anf_45bind51014 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind51013)
store volatile %struct.ScmObj* %anf_45bind51014, %struct.ScmObj** %stackaddr$prim57915, align 8
%truthy$cmp57916 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51014)
%cmp$cmp57916 = icmp eq i64 %truthy$cmp57916, 1
br i1 %cmp$cmp57916, label %truebranch$cmp57916, label %falsebranch$cmp57916
truebranch$cmp57916:
%ae53181 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53182 = call %struct.ScmObj* @const_init_true()
%argslist56882$k511030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57917 = alloca %struct.ScmObj*, align 8
%argslist56882$k511031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53182, %struct.ScmObj* %argslist56882$k511030)
store volatile %struct.ScmObj* %argslist56882$k511031, %struct.ScmObj** %stackaddr$prim57917, align 8
%stackaddr$prim57918 = alloca %struct.ScmObj*, align 8
%argslist56882$k511032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53181, %struct.ScmObj* %argslist56882$k511031)
store volatile %struct.ScmObj* %argslist56882$k511032, %struct.ScmObj** %stackaddr$prim57918, align 8
%clofunc57919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51103)
musttail call tailcc void %clofunc57919(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56882$k511032)
ret void
falsebranch$cmp57916:
%ae53190 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57920 = alloca %struct.ScmObj*, align 8
%anf_45bind51015 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53190)
store volatile %struct.ScmObj* %anf_45bind51015, %struct.ScmObj** %stackaddr$prim57920, align 8
%stackaddr$prim57921 = alloca %struct.ScmObj*, align 8
%anf_45bind51016 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind51015)
store volatile %struct.ScmObj* %anf_45bind51016, %struct.ScmObj** %stackaddr$prim57921, align 8
%truthy$cmp57922 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51016)
%cmp$cmp57922 = icmp eq i64 %truthy$cmp57922, 1
br i1 %cmp$cmp57922, label %truebranch$cmp57922, label %falsebranch$cmp57922
truebranch$cmp57922:
%ae53194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%anf_45bind51017 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53194)
store volatile %struct.ScmObj* %anf_45bind51017, %struct.ScmObj** %stackaddr$prim57923, align 8
%stackaddr$prim57924 = alloca %struct.ScmObj*, align 8
%b50908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51017)
store volatile %struct.ScmObj* %b50908, %struct.ScmObj** %stackaddr$prim57924, align 8
%ae53197 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%anf_45bind51018 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53197)
store volatile %struct.ScmObj* %anf_45bind51018, %struct.ScmObj** %stackaddr$prim57925, align 8
%stackaddr$prim57926 = alloca %struct.ScmObj*, align 8
%anf_45bind51019 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51018)
store volatile %struct.ScmObj* %anf_45bind51019, %struct.ScmObj** %stackaddr$prim57926, align 8
%ae53200 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57927 = alloca %struct.ScmObj*, align 8
%_95050909 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53200, %struct.ScmObj* %anf_45bind51019)
store volatile %struct.ScmObj* %_95050909, %struct.ScmObj** %stackaddr$prim57927, align 8
%argslist56883$cc509060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57928 = alloca %struct.ScmObj*, align 8
%argslist56883$cc509061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50906, %struct.ScmObj* %argslist56883$cc509060)
store volatile %struct.ScmObj* %argslist56883$cc509061, %struct.ScmObj** %stackaddr$prim57928, align 8
%stackaddr$prim57929 = alloca %struct.ScmObj*, align 8
%argslist56883$cc509062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56883$cc509061)
store volatile %struct.ScmObj* %argslist56883$cc509062, %struct.ScmObj** %stackaddr$prim57929, align 8
%clofunc57930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50906)
musttail call tailcc void %clofunc57930(%struct.ScmObj* %cc50906, %struct.ScmObj* %argslist56883$cc509062)
ret void
falsebranch$cmp57922:
%ae53233 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53234 = call %struct.ScmObj* @const_init_false()
%argslist56884$k511030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57931 = alloca %struct.ScmObj*, align 8
%argslist56884$k511031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53234, %struct.ScmObj* %argslist56884$k511030)
store volatile %struct.ScmObj* %argslist56884$k511031, %struct.ScmObj** %stackaddr$prim57931, align 8
%stackaddr$prim57932 = alloca %struct.ScmObj*, align 8
%argslist56884$k511032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53233, %struct.ScmObj* %argslist56884$k511031)
store volatile %struct.ScmObj* %argslist56884$k511032, %struct.ScmObj** %stackaddr$prim57932, align 8
%clofunc57933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51103)
musttail call tailcc void %clofunc57933(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56884$k511032)
ret void
}

define tailcc void @proc_clo$ae53062(%struct.ScmObj* %env$ae53062,%struct.ScmObj* %current_45args56885) {
%stackaddr$env-ref57934 = alloca %struct.ScmObj*, align 8
%a50905 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53062, i64 0)
store %struct.ScmObj* %a50905, %struct.ScmObj** %stackaddr$env-ref57934
%stackaddr$env-ref57935 = alloca %struct.ScmObj*, align 8
%k51103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53062, i64 1)
store %struct.ScmObj* %k51103, %struct.ScmObj** %stackaddr$env-ref57935
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%_95k51105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56885)
store volatile %struct.ScmObj* %_95k51105, %struct.ScmObj** %stackaddr$prim57936, align 8
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%current_45args56886 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56885)
store volatile %struct.ScmObj* %current_45args56886, %struct.ScmObj** %stackaddr$prim57937, align 8
%stackaddr$prim57938 = alloca %struct.ScmObj*, align 8
%cc50906 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56886)
store volatile %struct.ScmObj* %cc50906, %struct.ScmObj** %stackaddr$prim57938, align 8
%ae53064 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57939 = alloca %struct.ScmObj*, align 8
%anf_45bind51013 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53064)
store volatile %struct.ScmObj* %anf_45bind51013, %struct.ScmObj** %stackaddr$prim57939, align 8
%stackaddr$prim57940 = alloca %struct.ScmObj*, align 8
%anf_45bind51014 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind51013)
store volatile %struct.ScmObj* %anf_45bind51014, %struct.ScmObj** %stackaddr$prim57940, align 8
%truthy$cmp57941 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51014)
%cmp$cmp57941 = icmp eq i64 %truthy$cmp57941, 1
br i1 %cmp$cmp57941, label %truebranch$cmp57941, label %falsebranch$cmp57941
truebranch$cmp57941:
%ae53068 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53069 = call %struct.ScmObj* @const_init_true()
%argslist56888$k511030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%argslist56888$k511031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53069, %struct.ScmObj* %argslist56888$k511030)
store volatile %struct.ScmObj* %argslist56888$k511031, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%argslist56888$k511032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53068, %struct.ScmObj* %argslist56888$k511031)
store volatile %struct.ScmObj* %argslist56888$k511032, %struct.ScmObj** %stackaddr$prim57943, align 8
%clofunc57944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51103)
musttail call tailcc void %clofunc57944(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56888$k511032)
ret void
falsebranch$cmp57941:
%ae53077 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%anf_45bind51015 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53077)
store volatile %struct.ScmObj* %anf_45bind51015, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%anf_45bind51016 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind51015)
store volatile %struct.ScmObj* %anf_45bind51016, %struct.ScmObj** %stackaddr$prim57946, align 8
%truthy$cmp57947 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51016)
%cmp$cmp57947 = icmp eq i64 %truthy$cmp57947, 1
br i1 %cmp$cmp57947, label %truebranch$cmp57947, label %falsebranch$cmp57947
truebranch$cmp57947:
%ae53081 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57948 = alloca %struct.ScmObj*, align 8
%anf_45bind51017 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53081)
store volatile %struct.ScmObj* %anf_45bind51017, %struct.ScmObj** %stackaddr$prim57948, align 8
%stackaddr$prim57949 = alloca %struct.ScmObj*, align 8
%b50908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51017)
store volatile %struct.ScmObj* %b50908, %struct.ScmObj** %stackaddr$prim57949, align 8
%ae53084 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57950 = alloca %struct.ScmObj*, align 8
%anf_45bind51018 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53084)
store volatile %struct.ScmObj* %anf_45bind51018, %struct.ScmObj** %stackaddr$prim57950, align 8
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%anf_45bind51019 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind51018)
store volatile %struct.ScmObj* %anf_45bind51019, %struct.ScmObj** %stackaddr$prim57951, align 8
%ae53087 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57952 = alloca %struct.ScmObj*, align 8
%_95050909 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a50905, %struct.ScmObj* %ae53087, %struct.ScmObj* %anf_45bind51019)
store volatile %struct.ScmObj* %_95050909, %struct.ScmObj** %stackaddr$prim57952, align 8
%argslist56889$cc509060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57953 = alloca %struct.ScmObj*, align 8
%argslist56889$cc509061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc50906, %struct.ScmObj* %argslist56889$cc509060)
store volatile %struct.ScmObj* %argslist56889$cc509061, %struct.ScmObj** %stackaddr$prim57953, align 8
%stackaddr$prim57954 = alloca %struct.ScmObj*, align 8
%argslist56889$cc509062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56889$cc509061)
store volatile %struct.ScmObj* %argslist56889$cc509062, %struct.ScmObj** %stackaddr$prim57954, align 8
%clofunc57955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc50906)
musttail call tailcc void %clofunc57955(%struct.ScmObj* %cc50906, %struct.ScmObj* %argslist56889$cc509062)
ret void
falsebranch$cmp57947:
%ae53120 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53121 = call %struct.ScmObj* @const_init_false()
%argslist56890$k511030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%argslist56890$k511031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53121, %struct.ScmObj* %argslist56890$k511030)
store volatile %struct.ScmObj* %argslist56890$k511031, %struct.ScmObj** %stackaddr$prim57956, align 8
%stackaddr$prim57957 = alloca %struct.ScmObj*, align 8
%argslist56890$k511032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53120, %struct.ScmObj* %argslist56890$k511031)
store volatile %struct.ScmObj* %argslist56890$k511032, %struct.ScmObj** %stackaddr$prim57957, align 8
%clofunc57958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51103)
musttail call tailcc void %clofunc57958(%struct.ScmObj* %k51103, %struct.ScmObj* %argslist56890$k511032)
ret void
}

define tailcc void @proc_clo$ae53044(%struct.ScmObj* %env$ae53044,%struct.ScmObj* %current_45args56892) {
%stackaddr$prim57959 = alloca %struct.ScmObj*, align 8
%k51106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56892)
store volatile %struct.ScmObj* %k51106, %struct.ScmObj** %stackaddr$prim57959, align 8
%stackaddr$prim57960 = alloca %struct.ScmObj*, align 8
%current_45args56893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56892)
store volatile %struct.ScmObj* %current_45args56893, %struct.ScmObj** %stackaddr$prim57960, align 8
%stackaddr$prim57961 = alloca %struct.ScmObj*, align 8
%k50907 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56893)
store volatile %struct.ScmObj* %k50907, %struct.ScmObj** %stackaddr$prim57961, align 8
%ae53046 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56895$k511060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57962 = alloca %struct.ScmObj*, align 8
%argslist56895$k511061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k50907, %struct.ScmObj* %argslist56895$k511060)
store volatile %struct.ScmObj* %argslist56895$k511061, %struct.ScmObj** %stackaddr$prim57962, align 8
%stackaddr$prim57963 = alloca %struct.ScmObj*, align 8
%argslist56895$k511062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53046, %struct.ScmObj* %argslist56895$k511061)
store volatile %struct.ScmObj* %argslist56895$k511062, %struct.ScmObj** %stackaddr$prim57963, align 8
%clofunc57964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51106)
musttail call tailcc void %clofunc57964(%struct.ScmObj* %k51106, %struct.ScmObj* %argslist56895$k511062)
ret void
}

define tailcc void @proc_clo$ae52967(%struct.ScmObj* %env$ae52967,%struct.ScmObj* %current_45args56898) {
%stackaddr$env-ref57965 = alloca %struct.ScmObj*, align 8
%_37append50911 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52967, i64 0)
store %struct.ScmObj* %_37append50911, %struct.ScmObj** %stackaddr$env-ref57965
%stackaddr$prim57966 = alloca %struct.ScmObj*, align 8
%k51107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56898)
store volatile %struct.ScmObj* %k51107, %struct.ScmObj** %stackaddr$prim57966, align 8
%stackaddr$prim57967 = alloca %struct.ScmObj*, align 8
%current_45args56899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56898)
store volatile %struct.ScmObj* %current_45args56899, %struct.ScmObj** %stackaddr$prim57967, align 8
%stackaddr$prim57968 = alloca %struct.ScmObj*, align 8
%ls050914 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %ls050914, %struct.ScmObj** %stackaddr$prim57968, align 8
%stackaddr$prim57969 = alloca %struct.ScmObj*, align 8
%current_45args56900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %current_45args56900, %struct.ScmObj** %stackaddr$prim57969, align 8
%stackaddr$prim57970 = alloca %struct.ScmObj*, align 8
%ls150913 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56900)
store volatile %struct.ScmObj* %ls150913, %struct.ScmObj** %stackaddr$prim57970, align 8
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%anf_45bind51006 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls050914)
store volatile %struct.ScmObj* %anf_45bind51006, %struct.ScmObj** %stackaddr$prim57971, align 8
%truthy$cmp57972 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind51006)
%cmp$cmp57972 = icmp eq i64 %truthy$cmp57972, 1
br i1 %cmp$cmp57972, label %truebranch$cmp57972, label %falsebranch$cmp57972
truebranch$cmp57972:
%ae52971 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56902$k511070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57973 = alloca %struct.ScmObj*, align 8
%argslist56902$k511071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls150913, %struct.ScmObj* %argslist56902$k511070)
store volatile %struct.ScmObj* %argslist56902$k511071, %struct.ScmObj** %stackaddr$prim57973, align 8
%stackaddr$prim57974 = alloca %struct.ScmObj*, align 8
%argslist56902$k511072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52971, %struct.ScmObj* %argslist56902$k511071)
store volatile %struct.ScmObj* %argslist56902$k511072, %struct.ScmObj** %stackaddr$prim57974, align 8
%clofunc57975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51107)
musttail call tailcc void %clofunc57975(%struct.ScmObj* %k51107, %struct.ScmObj* %argslist56902$k511072)
ret void
falsebranch$cmp57972:
%stackaddr$prim57976 = alloca %struct.ScmObj*, align 8
%anf_45bind51007 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls050914)
store volatile %struct.ScmObj* %anf_45bind51007, %struct.ScmObj** %stackaddr$prim57976, align 8
%ae52978 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57977 = alloca %struct.ScmObj*, align 8
%anf_45bind51008 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append50911, %struct.ScmObj* %ae52978)
store volatile %struct.ScmObj* %anf_45bind51008, %struct.ScmObj** %stackaddr$prim57977, align 8
%stackaddr$prim57978 = alloca %struct.ScmObj*, align 8
%anf_45bind51009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls050914)
store volatile %struct.ScmObj* %anf_45bind51009, %struct.ScmObj** %stackaddr$prim57978, align 8
%stackaddr$makeclosure57979 = alloca %struct.ScmObj*, align 8
%fptrToInt57980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52981 to i64
%ae52981 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57980)
store volatile %struct.ScmObj* %ae52981, %struct.ScmObj** %stackaddr$makeclosure57979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52981, %struct.ScmObj* %k51107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52981, %struct.ScmObj* %anf_45bind51007, i64 1)
%argslist56907$anf_45bind510080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57981 = alloca %struct.ScmObj*, align 8
%argslist56907$anf_45bind510081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls150913, %struct.ScmObj* %argslist56907$anf_45bind510080)
store volatile %struct.ScmObj* %argslist56907$anf_45bind510081, %struct.ScmObj** %stackaddr$prim57981, align 8
%stackaddr$prim57982 = alloca %struct.ScmObj*, align 8
%argslist56907$anf_45bind510082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51009, %struct.ScmObj* %argslist56907$anf_45bind510081)
store volatile %struct.ScmObj* %argslist56907$anf_45bind510082, %struct.ScmObj** %stackaddr$prim57982, align 8
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%argslist56907$anf_45bind510083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52981, %struct.ScmObj* %argslist56907$anf_45bind510082)
store volatile %struct.ScmObj* %argslist56907$anf_45bind510083, %struct.ScmObj** %stackaddr$prim57983, align 8
%clofunc57984 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind51008)
musttail call tailcc void %clofunc57984(%struct.ScmObj* %anf_45bind51008, %struct.ScmObj* %argslist56907$anf_45bind510083)
ret void
}

define tailcc void @proc_clo$ae52981(%struct.ScmObj* %env$ae52981,%struct.ScmObj* %current_45args56903) {
%stackaddr$env-ref57985 = alloca %struct.ScmObj*, align 8
%k51107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52981, i64 0)
store %struct.ScmObj* %k51107, %struct.ScmObj** %stackaddr$env-ref57985
%stackaddr$env-ref57986 = alloca %struct.ScmObj*, align 8
%anf_45bind51007 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52981, i64 1)
store %struct.ScmObj* %anf_45bind51007, %struct.ScmObj** %stackaddr$env-ref57986
%stackaddr$prim57987 = alloca %struct.ScmObj*, align 8
%_95k51108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56903)
store volatile %struct.ScmObj* %_95k51108, %struct.ScmObj** %stackaddr$prim57987, align 8
%stackaddr$prim57988 = alloca %struct.ScmObj*, align 8
%current_45args56904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56903)
store volatile %struct.ScmObj* %current_45args56904, %struct.ScmObj** %stackaddr$prim57988, align 8
%stackaddr$prim57989 = alloca %struct.ScmObj*, align 8
%anf_45bind51010 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56904)
store volatile %struct.ScmObj* %anf_45bind51010, %struct.ScmObj** %stackaddr$prim57989, align 8
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%cpsprim51109 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind51007, %struct.ScmObj* %anf_45bind51010)
store volatile %struct.ScmObj* %cpsprim51109, %struct.ScmObj** %stackaddr$prim57990, align 8
%ae52987 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56906$k511070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%argslist56906$k511071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51109, %struct.ScmObj* %argslist56906$k511070)
store volatile %struct.ScmObj* %argslist56906$k511071, %struct.ScmObj** %stackaddr$prim57991, align 8
%stackaddr$prim57992 = alloca %struct.ScmObj*, align 8
%argslist56906$k511072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52987, %struct.ScmObj* %argslist56906$k511071)
store volatile %struct.ScmObj* %argslist56906$k511072, %struct.ScmObj** %stackaddr$prim57992, align 8
%clofunc57993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51107)
musttail call tailcc void %clofunc57993(%struct.ScmObj* %k51107, %struct.ScmObj* %argslist56906$k511072)
ret void
}

define tailcc void @proc_clo$ae52941(%struct.ScmObj* %env$ae52941,%struct.ScmObj* %current_45args56909) {
%stackaddr$prim57994 = alloca %struct.ScmObj*, align 8
%k51110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %k51110, %struct.ScmObj** %stackaddr$prim57994, align 8
%stackaddr$prim57995 = alloca %struct.ScmObj*, align 8
%current_45args56910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %current_45args56910, %struct.ScmObj** %stackaddr$prim57995, align 8
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%a50917 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56910)
store volatile %struct.ScmObj* %a50917, %struct.ScmObj** %stackaddr$prim57996, align 8
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%current_45args56911 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56910)
store volatile %struct.ScmObj* %current_45args56911, %struct.ScmObj** %stackaddr$prim57997, align 8
%stackaddr$prim57998 = alloca %struct.ScmObj*, align 8
%b50916 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56911)
store volatile %struct.ScmObj* %b50916, %struct.ScmObj** %stackaddr$prim57998, align 8
%stackaddr$prim57999 = alloca %struct.ScmObj*, align 8
%anf_45bind51005 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a50917, %struct.ScmObj* %b50916)
store volatile %struct.ScmObj* %anf_45bind51005, %struct.ScmObj** %stackaddr$prim57999, align 8
%stackaddr$prim58000 = alloca %struct.ScmObj*, align 8
%cpsprim51111 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind51005)
store volatile %struct.ScmObj* %cpsprim51111, %struct.ScmObj** %stackaddr$prim58000, align 8
%ae52946 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56913$k511100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58001 = alloca %struct.ScmObj*, align 8
%argslist56913$k511101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51111, %struct.ScmObj* %argslist56913$k511100)
store volatile %struct.ScmObj* %argslist56913$k511101, %struct.ScmObj** %stackaddr$prim58001, align 8
%stackaddr$prim58002 = alloca %struct.ScmObj*, align 8
%argslist56913$k511102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52946, %struct.ScmObj* %argslist56913$k511101)
store volatile %struct.ScmObj* %argslist56913$k511102, %struct.ScmObj** %stackaddr$prim58002, align 8
%clofunc58003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51110)
musttail call tailcc void %clofunc58003(%struct.ScmObj* %k51110, %struct.ScmObj* %argslist56913$k511102)
ret void
}

define tailcc void @proc_clo$ae52917(%struct.ScmObj* %env$ae52917,%struct.ScmObj* %current_45args56915) {
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%k51112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56915)
store volatile %struct.ScmObj* %k51112, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%current_45args56916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56915)
store volatile %struct.ScmObj* %current_45args56916, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%a50920 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56916)
store volatile %struct.ScmObj* %a50920, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$prim58007 = alloca %struct.ScmObj*, align 8
%current_45args56917 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56916)
store volatile %struct.ScmObj* %current_45args56917, %struct.ScmObj** %stackaddr$prim58007, align 8
%stackaddr$prim58008 = alloca %struct.ScmObj*, align 8
%b50919 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %b50919, %struct.ScmObj** %stackaddr$prim58008, align 8
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%anf_45bind51004 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a50920, %struct.ScmObj* %b50919)
store volatile %struct.ScmObj* %anf_45bind51004, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%cpsprim51113 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind51004)
store volatile %struct.ScmObj* %cpsprim51113, %struct.ScmObj** %stackaddr$prim58010, align 8
%ae52922 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56919$k511120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%argslist56919$k511121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51113, %struct.ScmObj* %argslist56919$k511120)
store volatile %struct.ScmObj* %argslist56919$k511121, %struct.ScmObj** %stackaddr$prim58011, align 8
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%argslist56919$k511122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52922, %struct.ScmObj* %argslist56919$k511121)
store volatile %struct.ScmObj* %argslist56919$k511122, %struct.ScmObj** %stackaddr$prim58012, align 8
%clofunc58013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51112)
musttail call tailcc void %clofunc58013(%struct.ScmObj* %k51112, %struct.ScmObj* %argslist56919$k511122)
ret void
}

define tailcc void @proc_clo$ae52523(%struct.ScmObj* %env$ae52523,%struct.ScmObj* %current_45args56922) {
%stackaddr$env-ref58014 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52523, i64 0)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58014
%stackaddr$env-ref58015 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52523, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58015
%stackaddr$env-ref58016 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52523, i64 2)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58016
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%k51114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56922)
store volatile %struct.ScmObj* %k51114, %struct.ScmObj** %stackaddr$prim58017, align 8
%stackaddr$prim58018 = alloca %struct.ScmObj*, align 8
%current_45args56923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56922)
store volatile %struct.ScmObj* %current_45args56923, %struct.ScmObj** %stackaddr$prim58018, align 8
%stackaddr$prim58019 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56923)
store volatile %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$prim58019, align 8
%ae52525 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58020 = alloca %struct.ScmObj*, align 8
%fptrToInt58021 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52526 to i64
%ae52526 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58021)
store volatile %struct.ScmObj* %ae52526, %struct.ScmObj** %stackaddr$makeclosure58020, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52526, %struct.ScmObj* %_37foldr50844, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52526, %struct.ScmObj* %_37foldl50922, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52526, %struct.ScmObj* %_37foldr150839, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52526, %struct.ScmObj* %_37map150870, i64 3)
%argslist56980$k511140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%argslist56980$k511141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52526, %struct.ScmObj* %argslist56980$k511140)
store volatile %struct.ScmObj* %argslist56980$k511141, %struct.ScmObj** %stackaddr$prim58022, align 8
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%argslist56980$k511142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52525, %struct.ScmObj* %argslist56980$k511141)
store volatile %struct.ScmObj* %argslist56980$k511142, %struct.ScmObj** %stackaddr$prim58023, align 8
%clofunc58024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51114)
musttail call tailcc void %clofunc58024(%struct.ScmObj* %k51114, %struct.ScmObj* %argslist56980$k511142)
ret void
}

define tailcc void @proc_clo$ae52526(%struct.ScmObj* %env$ae52526,%struct.ScmObj* %args5092351115) {
%stackaddr$env-ref58025 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52526, i64 0)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58025
%stackaddr$env-ref58026 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52526, i64 1)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58026
%stackaddr$env-ref58027 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52526, i64 2)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58027
%stackaddr$env-ref58028 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52526, i64 3)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58028
%stackaddr$prim58029 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args5092351115)
store volatile %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$prim58029, align 8
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%args50923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args5092351115)
store volatile %struct.ScmObj* %args50923, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$prim58031 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args50923)
store volatile %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$prim58031, align 8
%stackaddr$prim58032 = alloca %struct.ScmObj*, align 8
%anf_45bind50992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50923)
store volatile %struct.ScmObj* %anf_45bind50992, %struct.ScmObj** %stackaddr$prim58032, align 8
%stackaddr$prim58033 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50992)
store volatile %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$prim58033, align 8
%stackaddr$prim58034 = alloca %struct.ScmObj*, align 8
%anf_45bind50993 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50923)
store volatile %struct.ScmObj* %anf_45bind50993, %struct.ScmObj** %stackaddr$prim58034, align 8
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50993)
store volatile %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$makeclosure58036 = alloca %struct.ScmObj*, align 8
%fptrToInt58037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52534 to i64
%ae52534 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58037)
store volatile %struct.ScmObj* %ae52534, %struct.ScmObj** %stackaddr$makeclosure58036, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %lsts50924, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %k51116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %_37foldl50922, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %_37foldr150839, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %_37map150870, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %f50926, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52534, %struct.ScmObj* %acc50925, i64 7)
%ae52535 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58038 = alloca %struct.ScmObj*, align 8
%fptrToInt58039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52536 to i64
%ae52536 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58039)
store volatile %struct.ScmObj* %ae52536, %struct.ScmObj** %stackaddr$makeclosure58038, align 8
%argslist56979$ae525340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%argslist56979$ae525341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52536, %struct.ScmObj* %argslist56979$ae525340)
store volatile %struct.ScmObj* %argslist56979$ae525341, %struct.ScmObj** %stackaddr$prim58040, align 8
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%argslist56979$ae525342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52535, %struct.ScmObj* %argslist56979$ae525341)
store volatile %struct.ScmObj* %argslist56979$ae525342, %struct.ScmObj** %stackaddr$prim58041, align 8
%clofunc58042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52534)
musttail call tailcc void %clofunc58042(%struct.ScmObj* %ae52534, %struct.ScmObj* %argslist56979$ae525342)
ret void
}

define tailcc void @proc_clo$ae52534(%struct.ScmObj* %env$ae52534,%struct.ScmObj* %current_45args56925) {
%stackaddr$env-ref58043 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 0)
store %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$env-ref58043
%stackaddr$env-ref58044 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 1)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58044
%stackaddr$env-ref58045 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58045
%stackaddr$env-ref58046 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 3)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58046
%stackaddr$env-ref58047 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 4)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58047
%stackaddr$env-ref58048 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 5)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58048
%stackaddr$env-ref58049 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 6)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58049
%stackaddr$env-ref58050 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52534, i64 7)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58050
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%_95k51117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56925)
store volatile %struct.ScmObj* %_95k51117, %struct.ScmObj** %stackaddr$prim58051, align 8
%stackaddr$prim58052 = alloca %struct.ScmObj*, align 8
%current_45args56926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56925)
store volatile %struct.ScmObj* %current_45args56926, %struct.ScmObj** %stackaddr$prim58052, align 8
%stackaddr$prim58053 = alloca %struct.ScmObj*, align 8
%anf_45bind50994 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56926)
store volatile %struct.ScmObj* %anf_45bind50994, %struct.ScmObj** %stackaddr$prim58053, align 8
%stackaddr$makeclosure58054 = alloca %struct.ScmObj*, align 8
%fptrToInt58055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52566 to i64
%ae52566 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58055)
store volatile %struct.ScmObj* %ae52566, %struct.ScmObj** %stackaddr$makeclosure58054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %lsts50924, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %k51116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %_37foldl50922, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %_37map150870, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %f50926, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52566, %struct.ScmObj* %acc50925, i64 6)
%ae52568 = call %struct.ScmObj* @const_init_false()
%argslist56972$_37foldr1508390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%argslist56972$_37foldr1508391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50924, %struct.ScmObj* %argslist56972$_37foldr1508390)
store volatile %struct.ScmObj* %argslist56972$_37foldr1508391, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%argslist56972$_37foldr1508392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52568, %struct.ScmObj* %argslist56972$_37foldr1508391)
store volatile %struct.ScmObj* %argslist56972$_37foldr1508392, %struct.ScmObj** %stackaddr$prim58057, align 8
%stackaddr$prim58058 = alloca %struct.ScmObj*, align 8
%argslist56972$_37foldr1508393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50994, %struct.ScmObj* %argslist56972$_37foldr1508392)
store volatile %struct.ScmObj* %argslist56972$_37foldr1508393, %struct.ScmObj** %stackaddr$prim58058, align 8
%stackaddr$prim58059 = alloca %struct.ScmObj*, align 8
%argslist56972$_37foldr1508394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52566, %struct.ScmObj* %argslist56972$_37foldr1508393)
store volatile %struct.ScmObj* %argslist56972$_37foldr1508394, %struct.ScmObj** %stackaddr$prim58059, align 8
%clofunc58060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr150839)
musttail call tailcc void %clofunc58060(%struct.ScmObj* %_37foldr150839, %struct.ScmObj* %argslist56972$_37foldr1508394)
ret void
}

define tailcc void @proc_clo$ae52566(%struct.ScmObj* %env$ae52566,%struct.ScmObj* %current_45args56928) {
%stackaddr$env-ref58061 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 0)
store %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$env-ref58061
%stackaddr$env-ref58062 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 1)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58062
%stackaddr$env-ref58063 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58063
%stackaddr$env-ref58064 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 3)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58064
%stackaddr$env-ref58065 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 4)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58065
%stackaddr$env-ref58066 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 5)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58066
%stackaddr$env-ref58067 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52566, i64 6)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58067
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%_95k51118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56928)
store volatile %struct.ScmObj* %_95k51118, %struct.ScmObj** %stackaddr$prim58068, align 8
%stackaddr$prim58069 = alloca %struct.ScmObj*, align 8
%current_45args56929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56928)
store volatile %struct.ScmObj* %current_45args56929, %struct.ScmObj** %stackaddr$prim58069, align 8
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%anf_45bind50995 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %anf_45bind50995, %struct.ScmObj** %stackaddr$prim58070, align 8
%truthy$cmp58071 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50995)
%cmp$cmp58071 = icmp eq i64 %truthy$cmp58071, 1
br i1 %cmp$cmp58071, label %truebranch$cmp58071, label %falsebranch$cmp58071
truebranch$cmp58071:
%ae52577 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56931$k511160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%argslist56931$k511161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50925, %struct.ScmObj* %argslist56931$k511160)
store volatile %struct.ScmObj* %argslist56931$k511161, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%argslist56931$k511162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52577, %struct.ScmObj* %argslist56931$k511161)
store volatile %struct.ScmObj* %argslist56931$k511162, %struct.ScmObj** %stackaddr$prim58073, align 8
%clofunc58074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51116)
musttail call tailcc void %clofunc58074(%struct.ScmObj* %k51116, %struct.ScmObj* %argslist56931$k511162)
ret void
falsebranch$cmp58071:
%stackaddr$makeclosure58075 = alloca %struct.ScmObj*, align 8
%fptrToInt58076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52582 to i64
%ae52582 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58076)
store volatile %struct.ScmObj* %ae52582, %struct.ScmObj** %stackaddr$makeclosure58075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %lsts50924, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %k51116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %_37foldl50922, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %_37map150870, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %f50926, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52582, %struct.ScmObj* %acc50925, i64 6)
%ae52583 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58077 = alloca %struct.ScmObj*, align 8
%fptrToInt58078 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52584 to i64
%ae52584 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58078)
store volatile %struct.ScmObj* %ae52584, %struct.ScmObj** %stackaddr$makeclosure58077, align 8
%argslist56971$ae525820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%argslist56971$ae525821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52584, %struct.ScmObj* %argslist56971$ae525820)
store volatile %struct.ScmObj* %argslist56971$ae525821, %struct.ScmObj** %stackaddr$prim58079, align 8
%stackaddr$prim58080 = alloca %struct.ScmObj*, align 8
%argslist56971$ae525822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52583, %struct.ScmObj* %argslist56971$ae525821)
store volatile %struct.ScmObj* %argslist56971$ae525822, %struct.ScmObj** %stackaddr$prim58080, align 8
%clofunc58081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52582)
musttail call tailcc void %clofunc58081(%struct.ScmObj* %ae52582, %struct.ScmObj* %argslist56971$ae525822)
ret void
}

define tailcc void @proc_clo$ae52582(%struct.ScmObj* %env$ae52582,%struct.ScmObj* %current_45args56932) {
%stackaddr$env-ref58082 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 0)
store %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$env-ref58082
%stackaddr$env-ref58083 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 1)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58083
%stackaddr$env-ref58084 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58084
%stackaddr$env-ref58085 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 3)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58085
%stackaddr$env-ref58086 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 4)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58086
%stackaddr$env-ref58087 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 5)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58087
%stackaddr$env-ref58088 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52582, i64 6)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58088
%stackaddr$prim58089 = alloca %struct.ScmObj*, align 8
%_95k51119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %_95k51119, %struct.ScmObj** %stackaddr$prim58089, align 8
%stackaddr$prim58090 = alloca %struct.ScmObj*, align 8
%current_45args56933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %current_45args56933, %struct.ScmObj** %stackaddr$prim58090, align 8
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%anf_45bind50996 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %anf_45bind50996, %struct.ScmObj** %stackaddr$prim58091, align 8
%stackaddr$makeclosure58092 = alloca %struct.ScmObj*, align 8
%fptrToInt58093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52603 to i64
%ae52603 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58093)
store volatile %struct.ScmObj* %ae52603, %struct.ScmObj** %stackaddr$makeclosure58092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %lsts50924, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %k51116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %_37foldl50922, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %_37map150870, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %f50926, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52603, %struct.ScmObj* %acc50925, i64 6)
%argslist56966$_37map1508700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%argslist56966$_37map1508701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50924, %struct.ScmObj* %argslist56966$_37map1508700)
store volatile %struct.ScmObj* %argslist56966$_37map1508701, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%argslist56966$_37map1508702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50996, %struct.ScmObj* %argslist56966$_37map1508701)
store volatile %struct.ScmObj* %argslist56966$_37map1508702, %struct.ScmObj** %stackaddr$prim58095, align 8
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%argslist56966$_37map1508703 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52603, %struct.ScmObj* %argslist56966$_37map1508702)
store volatile %struct.ScmObj* %argslist56966$_37map1508703, %struct.ScmObj** %stackaddr$prim58096, align 8
%clofunc58097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map150870)
musttail call tailcc void %clofunc58097(%struct.ScmObj* %_37map150870, %struct.ScmObj* %argslist56966$_37map1508703)
ret void
}

define tailcc void @proc_clo$ae52603(%struct.ScmObj* %env$ae52603,%struct.ScmObj* %current_45args56935) {
%stackaddr$env-ref58098 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 0)
store %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$env-ref58098
%stackaddr$env-ref58099 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 1)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58099
%stackaddr$env-ref58100 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58100
%stackaddr$env-ref58101 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 3)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58101
%stackaddr$env-ref58102 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 4)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58102
%stackaddr$env-ref58103 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 5)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58103
%stackaddr$env-ref58104 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52603, i64 6)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58104
%stackaddr$prim58105 = alloca %struct.ScmObj*, align 8
%_95k51120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56935)
store volatile %struct.ScmObj* %_95k51120, %struct.ScmObj** %stackaddr$prim58105, align 8
%stackaddr$prim58106 = alloca %struct.ScmObj*, align 8
%current_45args56936 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56935)
store volatile %struct.ScmObj* %current_45args56936, %struct.ScmObj** %stackaddr$prim58106, align 8
%stackaddr$prim58107 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56936)
store volatile %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$prim58107, align 8
%stackaddr$makeclosure58108 = alloca %struct.ScmObj*, align 8
%fptrToInt58109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52606 to i64
%ae52606 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58109)
store volatile %struct.ScmObj* %ae52606, %struct.ScmObj** %stackaddr$makeclosure58108, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %lsts50924, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %k51116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %_37foldr50844, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %_37foldl50922, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %_37map150870, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %lsts_4350931, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %f50926, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52606, %struct.ScmObj* %acc50925, i64 7)
%ae52607 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58110 = alloca %struct.ScmObj*, align 8
%fptrToInt58111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52608 to i64
%ae52608 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58111)
store volatile %struct.ScmObj* %ae52608, %struct.ScmObj** %stackaddr$makeclosure58110, align 8
%argslist56965$ae526060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58112 = alloca %struct.ScmObj*, align 8
%argslist56965$ae526061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52608, %struct.ScmObj* %argslist56965$ae526060)
store volatile %struct.ScmObj* %argslist56965$ae526061, %struct.ScmObj** %stackaddr$prim58112, align 8
%stackaddr$prim58113 = alloca %struct.ScmObj*, align 8
%argslist56965$ae526062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52607, %struct.ScmObj* %argslist56965$ae526061)
store volatile %struct.ScmObj* %argslist56965$ae526062, %struct.ScmObj** %stackaddr$prim58113, align 8
%clofunc58114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52606)
musttail call tailcc void %clofunc58114(%struct.ScmObj* %ae52606, %struct.ScmObj* %argslist56965$ae526062)
ret void
}

define tailcc void @proc_clo$ae52606(%struct.ScmObj* %env$ae52606,%struct.ScmObj* %current_45args56938) {
%stackaddr$env-ref58115 = alloca %struct.ScmObj*, align 8
%lsts50924 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 0)
store %struct.ScmObj* %lsts50924, %struct.ScmObj** %stackaddr$env-ref58115
%stackaddr$env-ref58116 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 1)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58116
%stackaddr$env-ref58117 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 2)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58117
%stackaddr$env-ref58118 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 3)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58118
%stackaddr$env-ref58119 = alloca %struct.ScmObj*, align 8
%_37map150870 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 4)
store %struct.ScmObj* %_37map150870, %struct.ScmObj** %stackaddr$env-ref58119
%stackaddr$env-ref58120 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 5)
store %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$env-ref58120
%stackaddr$env-ref58121 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 6)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58121
%stackaddr$env-ref58122 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52606, i64 7)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58122
%stackaddr$prim58123 = alloca %struct.ScmObj*, align 8
%_95k51121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %_95k51121, %struct.ScmObj** %stackaddr$prim58123, align 8
%stackaddr$prim58124 = alloca %struct.ScmObj*, align 8
%current_45args56939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %current_45args56939, %struct.ScmObj** %stackaddr$prim58124, align 8
%stackaddr$prim58125 = alloca %struct.ScmObj*, align 8
%anf_45bind50997 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56939)
store volatile %struct.ScmObj* %anf_45bind50997, %struct.ScmObj** %stackaddr$prim58125, align 8
%stackaddr$makeclosure58126 = alloca %struct.ScmObj*, align 8
%fptrToInt58127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52627 to i64
%ae52627 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58127)
store volatile %struct.ScmObj* %ae52627, %struct.ScmObj** %stackaddr$makeclosure58126, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %k51116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %_37foldr50844, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %_37foldl50922, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %lsts_4350931, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %f50926, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52627, %struct.ScmObj* %acc50925, i64 5)
%argslist56960$_37map1508700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58128 = alloca %struct.ScmObj*, align 8
%argslist56960$_37map1508701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50924, %struct.ScmObj* %argslist56960$_37map1508700)
store volatile %struct.ScmObj* %argslist56960$_37map1508701, %struct.ScmObj** %stackaddr$prim58128, align 8
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%argslist56960$_37map1508702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50997, %struct.ScmObj* %argslist56960$_37map1508701)
store volatile %struct.ScmObj* %argslist56960$_37map1508702, %struct.ScmObj** %stackaddr$prim58129, align 8
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%argslist56960$_37map1508703 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52627, %struct.ScmObj* %argslist56960$_37map1508702)
store volatile %struct.ScmObj* %argslist56960$_37map1508703, %struct.ScmObj** %stackaddr$prim58130, align 8
%clofunc58131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map150870)
musttail call tailcc void %clofunc58131(%struct.ScmObj* %_37map150870, %struct.ScmObj* %argslist56960$_37map1508703)
ret void
}

define tailcc void @proc_clo$ae52627(%struct.ScmObj* %env$ae52627,%struct.ScmObj* %current_45args56941) {
%stackaddr$env-ref58132 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 0)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58132
%stackaddr$env-ref58133 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 1)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58133
%stackaddr$env-ref58134 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 2)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58134
%stackaddr$env-ref58135 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 3)
store %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$env-ref58135
%stackaddr$env-ref58136 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 4)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58136
%stackaddr$env-ref58137 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52627, i64 5)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58137
%stackaddr$prim58138 = alloca %struct.ScmObj*, align 8
%_95k51122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %_95k51122, %struct.ScmObj** %stackaddr$prim58138, align 8
%stackaddr$prim58139 = alloca %struct.ScmObj*, align 8
%current_45args56942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %current_45args56942, %struct.ScmObj** %stackaddr$prim58139, align 8
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%vs50929 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56942)
store volatile %struct.ScmObj* %vs50929, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$makeclosure58141 = alloca %struct.ScmObj*, align 8
%fptrToInt58142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52630 to i64
%ae52630 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58142)
store volatile %struct.ScmObj* %ae52630, %struct.ScmObj** %stackaddr$makeclosure58141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %k51116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %_37foldr50844, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %_37foldl50922, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %lsts_4350931, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %vs50929, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %f50926, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52630, %struct.ScmObj* %acc50925, i64 6)
%ae52631 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58143 = alloca %struct.ScmObj*, align 8
%fptrToInt58144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52632 to i64
%ae52632 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58144)
store volatile %struct.ScmObj* %ae52632, %struct.ScmObj** %stackaddr$makeclosure58143, align 8
%argslist56959$ae526300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58145 = alloca %struct.ScmObj*, align 8
%argslist56959$ae526301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52632, %struct.ScmObj* %argslist56959$ae526300)
store volatile %struct.ScmObj* %argslist56959$ae526301, %struct.ScmObj** %stackaddr$prim58145, align 8
%stackaddr$prim58146 = alloca %struct.ScmObj*, align 8
%argslist56959$ae526302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52631, %struct.ScmObj* %argslist56959$ae526301)
store volatile %struct.ScmObj* %argslist56959$ae526302, %struct.ScmObj** %stackaddr$prim58146, align 8
%clofunc58147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52630)
musttail call tailcc void %clofunc58147(%struct.ScmObj* %ae52630, %struct.ScmObj* %argslist56959$ae526302)
ret void
}

define tailcc void @proc_clo$ae52630(%struct.ScmObj* %env$ae52630,%struct.ScmObj* %current_45args56944) {
%stackaddr$env-ref58148 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 0)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58148
%stackaddr$env-ref58149 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 1)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58149
%stackaddr$env-ref58150 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 2)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58150
%stackaddr$env-ref58151 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 3)
store %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$env-ref58151
%stackaddr$env-ref58152 = alloca %struct.ScmObj*, align 8
%vs50929 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 4)
store %struct.ScmObj* %vs50929, %struct.ScmObj** %stackaddr$env-ref58152
%stackaddr$env-ref58153 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 5)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58153
%stackaddr$env-ref58154 = alloca %struct.ScmObj*, align 8
%acc50925 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52630, i64 6)
store %struct.ScmObj* %acc50925, %struct.ScmObj** %stackaddr$env-ref58154
%stackaddr$prim58155 = alloca %struct.ScmObj*, align 8
%_95k51123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %_95k51123, %struct.ScmObj** %stackaddr$prim58155, align 8
%stackaddr$prim58156 = alloca %struct.ScmObj*, align 8
%current_45args56945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %current_45args56945, %struct.ScmObj** %stackaddr$prim58156, align 8
%stackaddr$prim58157 = alloca %struct.ScmObj*, align 8
%anf_45bind50998 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56945)
store volatile %struct.ScmObj* %anf_45bind50998, %struct.ScmObj** %stackaddr$prim58157, align 8
%ae52653 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58158 = alloca %struct.ScmObj*, align 8
%anf_45bind50999 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50925, %struct.ScmObj* %ae52653)
store volatile %struct.ScmObj* %anf_45bind50999, %struct.ScmObj** %stackaddr$prim58158, align 8
%stackaddr$makeclosure58159 = alloca %struct.ScmObj*, align 8
%fptrToInt58160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52655 to i64
%ae52655 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58160)
store volatile %struct.ScmObj* %ae52655, %struct.ScmObj** %stackaddr$makeclosure58159, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52655, %struct.ScmObj* %k51116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52655, %struct.ScmObj* %_37foldl50922, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52655, %struct.ScmObj* %lsts_4350931, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52655, %struct.ScmObj* %f50926, i64 3)
%argslist56953$_37foldr508440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%argslist56953$_37foldr508441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs50929, %struct.ScmObj* %argslist56953$_37foldr508440)
store volatile %struct.ScmObj* %argslist56953$_37foldr508441, %struct.ScmObj** %stackaddr$prim58161, align 8
%stackaddr$prim58162 = alloca %struct.ScmObj*, align 8
%argslist56953$_37foldr508442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50999, %struct.ScmObj* %argslist56953$_37foldr508441)
store volatile %struct.ScmObj* %argslist56953$_37foldr508442, %struct.ScmObj** %stackaddr$prim58162, align 8
%stackaddr$prim58163 = alloca %struct.ScmObj*, align 8
%argslist56953$_37foldr508443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50998, %struct.ScmObj* %argslist56953$_37foldr508442)
store volatile %struct.ScmObj* %argslist56953$_37foldr508443, %struct.ScmObj** %stackaddr$prim58163, align 8
%stackaddr$prim58164 = alloca %struct.ScmObj*, align 8
%argslist56953$_37foldr508444 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52655, %struct.ScmObj* %argslist56953$_37foldr508443)
store volatile %struct.ScmObj* %argslist56953$_37foldr508444, %struct.ScmObj** %stackaddr$prim58164, align 8
%clofunc58165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr50844)
musttail call tailcc void %clofunc58165(%struct.ScmObj* %_37foldr50844, %struct.ScmObj* %argslist56953$_37foldr508444)
ret void
}

define tailcc void @proc_clo$ae52655(%struct.ScmObj* %env$ae52655,%struct.ScmObj* %current_45args56947) {
%stackaddr$env-ref58166 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52655, i64 0)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58166
%stackaddr$env-ref58167 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52655, i64 1)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58167
%stackaddr$env-ref58168 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52655, i64 2)
store %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$env-ref58168
%stackaddr$env-ref58169 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52655, i64 3)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58169
%stackaddr$prim58170 = alloca %struct.ScmObj*, align 8
%_95k51124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56947)
store volatile %struct.ScmObj* %_95k51124, %struct.ScmObj** %stackaddr$prim58170, align 8
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%current_45args56948 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56947)
store volatile %struct.ScmObj* %current_45args56948, %struct.ScmObj** %stackaddr$prim58171, align 8
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%anf_45bind51000 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56948)
store volatile %struct.ScmObj* %anf_45bind51000, %struct.ScmObj** %stackaddr$prim58172, align 8
%stackaddr$makeclosure58173 = alloca %struct.ScmObj*, align 8
%fptrToInt58174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52659 to i64
%ae52659 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58174)
store volatile %struct.ScmObj* %ae52659, %struct.ScmObj** %stackaddr$makeclosure58173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52659, %struct.ScmObj* %k51116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52659, %struct.ScmObj* %_37foldl50922, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52659, %struct.ScmObj* %lsts_4350931, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52659, %struct.ScmObj* %f50926, i64 3)
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%cpsargs51127 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52659, %struct.ScmObj* %anf_45bind51000)
store volatile %struct.ScmObj* %cpsargs51127, %struct.ScmObj** %stackaddr$prim58175, align 8
%clofunc58176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50926)
musttail call tailcc void %clofunc58176(%struct.ScmObj* %f50926, %struct.ScmObj* %cpsargs51127)
ret void
}

define tailcc void @proc_clo$ae52659(%struct.ScmObj* %env$ae52659,%struct.ScmObj* %current_45args56950) {
%stackaddr$env-ref58177 = alloca %struct.ScmObj*, align 8
%k51116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52659, i64 0)
store %struct.ScmObj* %k51116, %struct.ScmObj** %stackaddr$env-ref58177
%stackaddr$env-ref58178 = alloca %struct.ScmObj*, align 8
%_37foldl50922 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52659, i64 1)
store %struct.ScmObj* %_37foldl50922, %struct.ScmObj** %stackaddr$env-ref58178
%stackaddr$env-ref58179 = alloca %struct.ScmObj*, align 8
%lsts_4350931 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52659, i64 2)
store %struct.ScmObj* %lsts_4350931, %struct.ScmObj** %stackaddr$env-ref58179
%stackaddr$env-ref58180 = alloca %struct.ScmObj*, align 8
%f50926 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52659, i64 3)
store %struct.ScmObj* %f50926, %struct.ScmObj** %stackaddr$env-ref58180
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%_95k51125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56950)
store volatile %struct.ScmObj* %_95k51125, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%current_45args56951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56950)
store volatile %struct.ScmObj* %current_45args56951, %struct.ScmObj** %stackaddr$prim58182, align 8
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%acc_4350933 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56951)
store volatile %struct.ScmObj* %acc_4350933, %struct.ScmObj** %stackaddr$prim58183, align 8
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%anf_45bind51001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4350933, %struct.ScmObj* %lsts_4350931)
store volatile %struct.ScmObj* %anf_45bind51001, %struct.ScmObj** %stackaddr$prim58184, align 8
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%anf_45bind51002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50926, %struct.ScmObj* %anf_45bind51001)
store volatile %struct.ScmObj* %anf_45bind51002, %struct.ScmObj** %stackaddr$prim58185, align 8
%stackaddr$prim58186 = alloca %struct.ScmObj*, align 8
%cpsargs51126 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51116, %struct.ScmObj* %anf_45bind51002)
store volatile %struct.ScmObj* %cpsargs51126, %struct.ScmObj** %stackaddr$prim58186, align 8
%clofunc58187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl50922)
musttail call tailcc void %clofunc58187(%struct.ScmObj* %_37foldl50922, %struct.ScmObj* %cpsargs51126)
ret void
}

define tailcc void @proc_clo$ae52632(%struct.ScmObj* %env$ae52632,%struct.ScmObj* %current_45args56954) {
%stackaddr$prim58188 = alloca %struct.ScmObj*, align 8
%k51128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56954)
store volatile %struct.ScmObj* %k51128, %struct.ScmObj** %stackaddr$prim58188, align 8
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%current_45args56955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56954)
store volatile %struct.ScmObj* %current_45args56955, %struct.ScmObj** %stackaddr$prim58189, align 8
%stackaddr$prim58190 = alloca %struct.ScmObj*, align 8
%a50935 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56955)
store volatile %struct.ScmObj* %a50935, %struct.ScmObj** %stackaddr$prim58190, align 8
%stackaddr$prim58191 = alloca %struct.ScmObj*, align 8
%current_45args56956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56955)
store volatile %struct.ScmObj* %current_45args56956, %struct.ScmObj** %stackaddr$prim58191, align 8
%stackaddr$prim58192 = alloca %struct.ScmObj*, align 8
%b50934 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56956)
store volatile %struct.ScmObj* %b50934, %struct.ScmObj** %stackaddr$prim58192, align 8
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%cpsprim51129 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a50935, %struct.ScmObj* %b50934)
store volatile %struct.ScmObj* %cpsprim51129, %struct.ScmObj** %stackaddr$prim58193, align 8
%ae52636 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56958$k511280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%argslist56958$k511281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51129, %struct.ScmObj* %argslist56958$k511280)
store volatile %struct.ScmObj* %argslist56958$k511281, %struct.ScmObj** %stackaddr$prim58194, align 8
%stackaddr$prim58195 = alloca %struct.ScmObj*, align 8
%argslist56958$k511282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52636, %struct.ScmObj* %argslist56958$k511281)
store volatile %struct.ScmObj* %argslist56958$k511282, %struct.ScmObj** %stackaddr$prim58195, align 8
%clofunc58196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51128)
musttail call tailcc void %clofunc58196(%struct.ScmObj* %k51128, %struct.ScmObj* %argslist56958$k511282)
ret void
}

define tailcc void @proc_clo$ae52608(%struct.ScmObj* %env$ae52608,%struct.ScmObj* %current_45args56961) {
%stackaddr$prim58197 = alloca %struct.ScmObj*, align 8
%k51130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56961)
store volatile %struct.ScmObj* %k51130, %struct.ScmObj** %stackaddr$prim58197, align 8
%stackaddr$prim58198 = alloca %struct.ScmObj*, align 8
%current_45args56962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56961)
store volatile %struct.ScmObj* %current_45args56962, %struct.ScmObj** %stackaddr$prim58198, align 8
%stackaddr$prim58199 = alloca %struct.ScmObj*, align 8
%x50930 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56962)
store volatile %struct.ScmObj* %x50930, %struct.ScmObj** %stackaddr$prim58199, align 8
%stackaddr$prim58200 = alloca %struct.ScmObj*, align 8
%cpsprim51131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x50930)
store volatile %struct.ScmObj* %cpsprim51131, %struct.ScmObj** %stackaddr$prim58200, align 8
%ae52611 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56964$k511300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58201 = alloca %struct.ScmObj*, align 8
%argslist56964$k511301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51131, %struct.ScmObj* %argslist56964$k511300)
store volatile %struct.ScmObj* %argslist56964$k511301, %struct.ScmObj** %stackaddr$prim58201, align 8
%stackaddr$prim58202 = alloca %struct.ScmObj*, align 8
%argslist56964$k511302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52611, %struct.ScmObj* %argslist56964$k511301)
store volatile %struct.ScmObj* %argslist56964$k511302, %struct.ScmObj** %stackaddr$prim58202, align 8
%clofunc58203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51130)
musttail call tailcc void %clofunc58203(%struct.ScmObj* %k51130, %struct.ScmObj* %argslist56964$k511302)
ret void
}

define tailcc void @proc_clo$ae52584(%struct.ScmObj* %env$ae52584,%struct.ScmObj* %current_45args56967) {
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%k51132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56967)
store volatile %struct.ScmObj* %k51132, %struct.ScmObj** %stackaddr$prim58204, align 8
%stackaddr$prim58205 = alloca %struct.ScmObj*, align 8
%current_45args56968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56967)
store volatile %struct.ScmObj* %current_45args56968, %struct.ScmObj** %stackaddr$prim58205, align 8
%stackaddr$prim58206 = alloca %struct.ScmObj*, align 8
%x50932 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56968)
store volatile %struct.ScmObj* %x50932, %struct.ScmObj** %stackaddr$prim58206, align 8
%stackaddr$prim58207 = alloca %struct.ScmObj*, align 8
%cpsprim51133 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x50932)
store volatile %struct.ScmObj* %cpsprim51133, %struct.ScmObj** %stackaddr$prim58207, align 8
%ae52587 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56970$k511320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58208 = alloca %struct.ScmObj*, align 8
%argslist56970$k511321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51133, %struct.ScmObj* %argslist56970$k511320)
store volatile %struct.ScmObj* %argslist56970$k511321, %struct.ScmObj** %stackaddr$prim58208, align 8
%stackaddr$prim58209 = alloca %struct.ScmObj*, align 8
%argslist56970$k511322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52587, %struct.ScmObj* %argslist56970$k511321)
store volatile %struct.ScmObj* %argslist56970$k511322, %struct.ScmObj** %stackaddr$prim58209, align 8
%clofunc58210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51132)
musttail call tailcc void %clofunc58210(%struct.ScmObj* %k51132, %struct.ScmObj* %argslist56970$k511322)
ret void
}

define tailcc void @proc_clo$ae52536(%struct.ScmObj* %env$ae52536,%struct.ScmObj* %current_45args56973) {
%stackaddr$prim58211 = alloca %struct.ScmObj*, align 8
%k51134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56973)
store volatile %struct.ScmObj* %k51134, %struct.ScmObj** %stackaddr$prim58211, align 8
%stackaddr$prim58212 = alloca %struct.ScmObj*, align 8
%current_45args56974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56973)
store volatile %struct.ScmObj* %current_45args56974, %struct.ScmObj** %stackaddr$prim58212, align 8
%stackaddr$prim58213 = alloca %struct.ScmObj*, align 8
%lst50928 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56974)
store volatile %struct.ScmObj* %lst50928, %struct.ScmObj** %stackaddr$prim58213, align 8
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%current_45args56975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56974)
store volatile %struct.ScmObj* %current_45args56975, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%b50927 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56975)
store volatile %struct.ScmObj* %b50927, %struct.ScmObj** %stackaddr$prim58215, align 8
%truthy$cmp58216 = call i64 @is_truthy_value(%struct.ScmObj* %b50927)
%cmp$cmp58216 = icmp eq i64 %truthy$cmp58216, 1
br i1 %cmp$cmp58216, label %truebranch$cmp58216, label %falsebranch$cmp58216
truebranch$cmp58216:
%ae52539 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56977$k511340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%argslist56977$k511341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b50927, %struct.ScmObj* %argslist56977$k511340)
store volatile %struct.ScmObj* %argslist56977$k511341, %struct.ScmObj** %stackaddr$prim58217, align 8
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%argslist56977$k511342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52539, %struct.ScmObj* %argslist56977$k511341)
store volatile %struct.ScmObj* %argslist56977$k511342, %struct.ScmObj** %stackaddr$prim58218, align 8
%clofunc58219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51134)
musttail call tailcc void %clofunc58219(%struct.ScmObj* %k51134, %struct.ScmObj* %argslist56977$k511342)
ret void
falsebranch$cmp58216:
%stackaddr$prim58220 = alloca %struct.ScmObj*, align 8
%cpsprim51135 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50928)
store volatile %struct.ScmObj* %cpsprim51135, %struct.ScmObj** %stackaddr$prim58220, align 8
%ae52546 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56978$k511340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58221 = alloca %struct.ScmObj*, align 8
%argslist56978$k511341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51135, %struct.ScmObj* %argslist56978$k511340)
store volatile %struct.ScmObj* %argslist56978$k511341, %struct.ScmObj** %stackaddr$prim58221, align 8
%stackaddr$prim58222 = alloca %struct.ScmObj*, align 8
%argslist56978$k511342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52546, %struct.ScmObj* %argslist56978$k511341)
store volatile %struct.ScmObj* %argslist56978$k511342, %struct.ScmObj** %stackaddr$prim58222, align 8
%clofunc58223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51134)
musttail call tailcc void %clofunc58223(%struct.ScmObj* %k51134, %struct.ScmObj* %argslist56978$k511342)
ret void
}

define tailcc void @proc_clo$ae52377(%struct.ScmObj* %env$ae52377,%struct.ScmObj* %args5086651136) {
%stackaddr$env-ref58224 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52377, i64 0)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58224
%stackaddr$env-ref58225 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52377, i64 1)
store %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$env-ref58225
%stackaddr$env-ref58226 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52377, i64 2)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref58226
%stackaddr$prim58227 = alloca %struct.ScmObj*, align 8
%k51137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args5086651136)
store volatile %struct.ScmObj* %k51137, %struct.ScmObj** %stackaddr$prim58227, align 8
%stackaddr$prim58228 = alloca %struct.ScmObj*, align 8
%args50866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args5086651136)
store volatile %struct.ScmObj* %args50866, %struct.ScmObj** %stackaddr$prim58228, align 8
%stackaddr$prim58229 = alloca %struct.ScmObj*, align 8
%f50868 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args50866)
store volatile %struct.ScmObj* %f50868, %struct.ScmObj** %stackaddr$prim58229, align 8
%stackaddr$prim58230 = alloca %struct.ScmObj*, align 8
%lsts50867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50866)
store volatile %struct.ScmObj* %lsts50867, %struct.ScmObj** %stackaddr$prim58230, align 8
%stackaddr$makeclosure58231 = alloca %struct.ScmObj*, align 8
%fptrToInt58232 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52382 to i64
%ae52382 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58232)
store volatile %struct.ScmObj* %ae52382, %struct.ScmObj** %stackaddr$makeclosure58231, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52382, %struct.ScmObj* %_37foldr50844, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52382, %struct.ScmObj* %lsts50867, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52382, %struct.ScmObj* %k51137, i64 2)
%ae52383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58233 = alloca %struct.ScmObj*, align 8
%fptrToInt58234 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52384 to i64
%ae52384 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58234)
store volatile %struct.ScmObj* %ae52384, %struct.ScmObj** %stackaddr$makeclosure58233, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52384, %struct.ScmObj* %_37drop_45right50858, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52384, %struct.ScmObj* %f50868, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52384, %struct.ScmObj* %_37last50861, i64 2)
%argslist56997$ae523820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58235 = alloca %struct.ScmObj*, align 8
%argslist56997$ae523821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52384, %struct.ScmObj* %argslist56997$ae523820)
store volatile %struct.ScmObj* %argslist56997$ae523821, %struct.ScmObj** %stackaddr$prim58235, align 8
%stackaddr$prim58236 = alloca %struct.ScmObj*, align 8
%argslist56997$ae523822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52383, %struct.ScmObj* %argslist56997$ae523821)
store volatile %struct.ScmObj* %argslist56997$ae523822, %struct.ScmObj** %stackaddr$prim58236, align 8
%clofunc58237 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52382)
musttail call tailcc void %clofunc58237(%struct.ScmObj* %ae52382, %struct.ScmObj* %argslist56997$ae523822)
ret void
}

define tailcc void @proc_clo$ae52382(%struct.ScmObj* %env$ae52382,%struct.ScmObj* %current_45args56982) {
%stackaddr$env-ref58238 = alloca %struct.ScmObj*, align 8
%_37foldr50844 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52382, i64 0)
store %struct.ScmObj* %_37foldr50844, %struct.ScmObj** %stackaddr$env-ref58238
%stackaddr$env-ref58239 = alloca %struct.ScmObj*, align 8
%lsts50867 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52382, i64 1)
store %struct.ScmObj* %lsts50867, %struct.ScmObj** %stackaddr$env-ref58239
%stackaddr$env-ref58240 = alloca %struct.ScmObj*, align 8
%k51137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52382, i64 2)
store %struct.ScmObj* %k51137, %struct.ScmObj** %stackaddr$env-ref58240
%stackaddr$prim58241 = alloca %struct.ScmObj*, align 8
%_95k51138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %_95k51138, %struct.ScmObj** %stackaddr$prim58241, align 8
%stackaddr$prim58242 = alloca %struct.ScmObj*, align 8
%current_45args56983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %current_45args56983, %struct.ScmObj** %stackaddr$prim58242, align 8
%stackaddr$prim58243 = alloca %struct.ScmObj*, align 8
%anf_45bind50989 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56983)
store volatile %struct.ScmObj* %anf_45bind50989, %struct.ScmObj** %stackaddr$prim58243, align 8
%ae52445 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58244 = alloca %struct.ScmObj*, align 8
%anf_45bind50990 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52445, %struct.ScmObj* %lsts50867)
store volatile %struct.ScmObj* %anf_45bind50990, %struct.ScmObj** %stackaddr$prim58244, align 8
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%anf_45bind50991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50989, %struct.ScmObj* %anf_45bind50990)
store volatile %struct.ScmObj* %anf_45bind50991, %struct.ScmObj** %stackaddr$prim58245, align 8
%stackaddr$prim58246 = alloca %struct.ScmObj*, align 8
%cpsargs51139 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51137, %struct.ScmObj* %anf_45bind50991)
store volatile %struct.ScmObj* %cpsargs51139, %struct.ScmObj** %stackaddr$prim58246, align 8
%clofunc58247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr50844)
musttail call tailcc void %clofunc58247(%struct.ScmObj* %_37foldr50844, %struct.ScmObj* %cpsargs51139)
ret void
}

define tailcc void @proc_clo$ae52384(%struct.ScmObj* %env$ae52384,%struct.ScmObj* %fargs5086951140) {
%stackaddr$env-ref58248 = alloca %struct.ScmObj*, align 8
%_37drop_45right50858 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52384, i64 0)
store %struct.ScmObj* %_37drop_45right50858, %struct.ScmObj** %stackaddr$env-ref58248
%stackaddr$env-ref58249 = alloca %struct.ScmObj*, align 8
%f50868 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52384, i64 1)
store %struct.ScmObj* %f50868, %struct.ScmObj** %stackaddr$env-ref58249
%stackaddr$env-ref58250 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52384, i64 2)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref58250
%stackaddr$prim58251 = alloca %struct.ScmObj*, align 8
%k51141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs5086951140)
store volatile %struct.ScmObj* %k51141, %struct.ScmObj** %stackaddr$prim58251, align 8
%stackaddr$prim58252 = alloca %struct.ScmObj*, align 8
%fargs50869 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs5086951140)
store volatile %struct.ScmObj* %fargs50869, %struct.ScmObj** %stackaddr$prim58252, align 8
%stackaddr$makeclosure58253 = alloca %struct.ScmObj*, align 8
%fptrToInt58254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52388 to i64
%ae52388 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58254)
store volatile %struct.ScmObj* %ae52388, %struct.ScmObj** %stackaddr$makeclosure58253, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52388, %struct.ScmObj* %k51141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52388, %struct.ScmObj* %fargs50869, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52388, %struct.ScmObj* %f50868, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52388, %struct.ScmObj* %_37last50861, i64 3)
%ae52390 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56996$_37drop_45right508580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%argslist56996$_37drop_45right508581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52390, %struct.ScmObj* %argslist56996$_37drop_45right508580)
store volatile %struct.ScmObj* %argslist56996$_37drop_45right508581, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%argslist56996$_37drop_45right508582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs50869, %struct.ScmObj* %argslist56996$_37drop_45right508581)
store volatile %struct.ScmObj* %argslist56996$_37drop_45right508582, %struct.ScmObj** %stackaddr$prim58256, align 8
%stackaddr$prim58257 = alloca %struct.ScmObj*, align 8
%argslist56996$_37drop_45right508583 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52388, %struct.ScmObj* %argslist56996$_37drop_45right508582)
store volatile %struct.ScmObj* %argslist56996$_37drop_45right508583, %struct.ScmObj** %stackaddr$prim58257, align 8
%clofunc58258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right50858)
musttail call tailcc void %clofunc58258(%struct.ScmObj* %_37drop_45right50858, %struct.ScmObj* %argslist56996$_37drop_45right508583)
ret void
}

define tailcc void @proc_clo$ae52388(%struct.ScmObj* %env$ae52388,%struct.ScmObj* %current_45args56985) {
%stackaddr$env-ref58259 = alloca %struct.ScmObj*, align 8
%k51141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52388, i64 0)
store %struct.ScmObj* %k51141, %struct.ScmObj** %stackaddr$env-ref58259
%stackaddr$env-ref58260 = alloca %struct.ScmObj*, align 8
%fargs50869 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52388, i64 1)
store %struct.ScmObj* %fargs50869, %struct.ScmObj** %stackaddr$env-ref58260
%stackaddr$env-ref58261 = alloca %struct.ScmObj*, align 8
%f50868 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52388, i64 2)
store %struct.ScmObj* %f50868, %struct.ScmObj** %stackaddr$env-ref58261
%stackaddr$env-ref58262 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52388, i64 3)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref58262
%stackaddr$prim58263 = alloca %struct.ScmObj*, align 8
%_95k51142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56985)
store volatile %struct.ScmObj* %_95k51142, %struct.ScmObj** %stackaddr$prim58263, align 8
%stackaddr$prim58264 = alloca %struct.ScmObj*, align 8
%current_45args56986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56985)
store volatile %struct.ScmObj* %current_45args56986, %struct.ScmObj** %stackaddr$prim58264, align 8
%stackaddr$prim58265 = alloca %struct.ScmObj*, align 8
%anf_45bind50986 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56986)
store volatile %struct.ScmObj* %anf_45bind50986, %struct.ScmObj** %stackaddr$prim58265, align 8
%stackaddr$makeclosure58266 = alloca %struct.ScmObj*, align 8
%fptrToInt58267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52395 to i64
%ae52395 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58267)
store volatile %struct.ScmObj* %ae52395, %struct.ScmObj** %stackaddr$makeclosure58266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52395, %struct.ScmObj* %k51141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52395, %struct.ScmObj* %fargs50869, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52395, %struct.ScmObj* %_37last50861, i64 2)
%stackaddr$prim58268 = alloca %struct.ScmObj*, align 8
%cpsargs51146 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52395, %struct.ScmObj* %anf_45bind50986)
store volatile %struct.ScmObj* %cpsargs51146, %struct.ScmObj** %stackaddr$prim58268, align 8
%clofunc58269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50868)
musttail call tailcc void %clofunc58269(%struct.ScmObj* %f50868, %struct.ScmObj* %cpsargs51146)
ret void
}

define tailcc void @proc_clo$ae52395(%struct.ScmObj* %env$ae52395,%struct.ScmObj* %current_45args56988) {
%stackaddr$env-ref58270 = alloca %struct.ScmObj*, align 8
%k51141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52395, i64 0)
store %struct.ScmObj* %k51141, %struct.ScmObj** %stackaddr$env-ref58270
%stackaddr$env-ref58271 = alloca %struct.ScmObj*, align 8
%fargs50869 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52395, i64 1)
store %struct.ScmObj* %fargs50869, %struct.ScmObj** %stackaddr$env-ref58271
%stackaddr$env-ref58272 = alloca %struct.ScmObj*, align 8
%_37last50861 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52395, i64 2)
store %struct.ScmObj* %_37last50861, %struct.ScmObj** %stackaddr$env-ref58272
%stackaddr$prim58273 = alloca %struct.ScmObj*, align 8
%_95k51143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56988)
store volatile %struct.ScmObj* %_95k51143, %struct.ScmObj** %stackaddr$prim58273, align 8
%stackaddr$prim58274 = alloca %struct.ScmObj*, align 8
%current_45args56989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56988)
store volatile %struct.ScmObj* %current_45args56989, %struct.ScmObj** %stackaddr$prim58274, align 8
%stackaddr$prim58275 = alloca %struct.ScmObj*, align 8
%anf_45bind50987 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56989)
store volatile %struct.ScmObj* %anf_45bind50987, %struct.ScmObj** %stackaddr$prim58275, align 8
%stackaddr$makeclosure58276 = alloca %struct.ScmObj*, align 8
%fptrToInt58277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52400 to i64
%ae52400 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58277)
store volatile %struct.ScmObj* %ae52400, %struct.ScmObj** %stackaddr$makeclosure58276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52400, %struct.ScmObj* %anf_45bind50987, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52400, %struct.ScmObj* %k51141, i64 1)
%argslist56995$_37last508610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58278 = alloca %struct.ScmObj*, align 8
%argslist56995$_37last508611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs50869, %struct.ScmObj* %argslist56995$_37last508610)
store volatile %struct.ScmObj* %argslist56995$_37last508611, %struct.ScmObj** %stackaddr$prim58278, align 8
%stackaddr$prim58279 = alloca %struct.ScmObj*, align 8
%argslist56995$_37last508612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52400, %struct.ScmObj* %argslist56995$_37last508611)
store volatile %struct.ScmObj* %argslist56995$_37last508612, %struct.ScmObj** %stackaddr$prim58279, align 8
%clofunc58280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last50861)
musttail call tailcc void %clofunc58280(%struct.ScmObj* %_37last50861, %struct.ScmObj* %argslist56995$_37last508612)
ret void
}

define tailcc void @proc_clo$ae52400(%struct.ScmObj* %env$ae52400,%struct.ScmObj* %current_45args56991) {
%stackaddr$env-ref58281 = alloca %struct.ScmObj*, align 8
%anf_45bind50987 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52400, i64 0)
store %struct.ScmObj* %anf_45bind50987, %struct.ScmObj** %stackaddr$env-ref58281
%stackaddr$env-ref58282 = alloca %struct.ScmObj*, align 8
%k51141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52400, i64 1)
store %struct.ScmObj* %k51141, %struct.ScmObj** %stackaddr$env-ref58282
%stackaddr$prim58283 = alloca %struct.ScmObj*, align 8
%_95k51144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56991)
store volatile %struct.ScmObj* %_95k51144, %struct.ScmObj** %stackaddr$prim58283, align 8
%stackaddr$prim58284 = alloca %struct.ScmObj*, align 8
%current_45args56992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56991)
store volatile %struct.ScmObj* %current_45args56992, %struct.ScmObj** %stackaddr$prim58284, align 8
%stackaddr$prim58285 = alloca %struct.ScmObj*, align 8
%anf_45bind50988 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56992)
store volatile %struct.ScmObj* %anf_45bind50988, %struct.ScmObj** %stackaddr$prim58285, align 8
%stackaddr$prim58286 = alloca %struct.ScmObj*, align 8
%cpsprim51145 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50987, %struct.ScmObj* %anf_45bind50988)
store volatile %struct.ScmObj* %cpsprim51145, %struct.ScmObj** %stackaddr$prim58286, align 8
%ae52405 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56994$k511410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58287 = alloca %struct.ScmObj*, align 8
%argslist56994$k511411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51145, %struct.ScmObj* %argslist56994$k511410)
store volatile %struct.ScmObj* %argslist56994$k511411, %struct.ScmObj** %stackaddr$prim58287, align 8
%stackaddr$prim58288 = alloca %struct.ScmObj*, align 8
%argslist56994$k511412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52405, %struct.ScmObj* %argslist56994$k511411)
store volatile %struct.ScmObj* %argslist56994$k511412, %struct.ScmObj** %stackaddr$prim58288, align 8
%clofunc58289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51141)
musttail call tailcc void %clofunc58289(%struct.ScmObj* %k51141, %struct.ScmObj* %argslist56994$k511412)
ret void
}

define tailcc void @proc_clo$ae52300(%struct.ScmObj* %env$ae52300,%struct.ScmObj* %current_45args56999) {
%stackaddr$env-ref58290 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52300, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58290
%stackaddr$prim58291 = alloca %struct.ScmObj*, align 8
%k51147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56999)
store volatile %struct.ScmObj* %k51147, %struct.ScmObj** %stackaddr$prim58291, align 8
%stackaddr$prim58292 = alloca %struct.ScmObj*, align 8
%current_45args57000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56999)
store volatile %struct.ScmObj* %current_45args57000, %struct.ScmObj** %stackaddr$prim58292, align 8
%stackaddr$prim58293 = alloca %struct.ScmObj*, align 8
%f50872 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57000)
store volatile %struct.ScmObj* %f50872, %struct.ScmObj** %stackaddr$prim58293, align 8
%stackaddr$prim58294 = alloca %struct.ScmObj*, align 8
%current_45args57001 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57000)
store volatile %struct.ScmObj* %current_45args57001, %struct.ScmObj** %stackaddr$prim58294, align 8
%stackaddr$prim58295 = alloca %struct.ScmObj*, align 8
%lst50871 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57001)
store volatile %struct.ScmObj* %lst50871, %struct.ScmObj** %stackaddr$prim58295, align 8
%stackaddr$makeclosure58296 = alloca %struct.ScmObj*, align 8
%fptrToInt58297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52301 to i64
%ae52301 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58297)
store volatile %struct.ScmObj* %ae52301, %struct.ScmObj** %stackaddr$makeclosure58296, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52301, %struct.ScmObj* %lst50871, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52301, %struct.ScmObj* %_37foldr150839, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52301, %struct.ScmObj* %k51147, i64 2)
%ae52302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58298 = alloca %struct.ScmObj*, align 8
%fptrToInt58299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52303 to i64
%ae52303 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58299)
store volatile %struct.ScmObj* %ae52303, %struct.ScmObj** %stackaddr$makeclosure58298, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52303, %struct.ScmObj* %f50872, i64 0)
%argslist57016$ae523010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58300 = alloca %struct.ScmObj*, align 8
%argslist57016$ae523011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52303, %struct.ScmObj* %argslist57016$ae523010)
store volatile %struct.ScmObj* %argslist57016$ae523011, %struct.ScmObj** %stackaddr$prim58300, align 8
%stackaddr$prim58301 = alloca %struct.ScmObj*, align 8
%argslist57016$ae523012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52302, %struct.ScmObj* %argslist57016$ae523011)
store volatile %struct.ScmObj* %argslist57016$ae523012, %struct.ScmObj** %stackaddr$prim58301, align 8
%clofunc58302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52301)
musttail call tailcc void %clofunc58302(%struct.ScmObj* %ae52301, %struct.ScmObj* %argslist57016$ae523012)
ret void
}

define tailcc void @proc_clo$ae52301(%struct.ScmObj* %env$ae52301,%struct.ScmObj* %current_45args57003) {
%stackaddr$env-ref58303 = alloca %struct.ScmObj*, align 8
%lst50871 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52301, i64 0)
store %struct.ScmObj* %lst50871, %struct.ScmObj** %stackaddr$env-ref58303
%stackaddr$env-ref58304 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52301, i64 1)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58304
%stackaddr$env-ref58305 = alloca %struct.ScmObj*, align 8
%k51147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52301, i64 2)
store %struct.ScmObj* %k51147, %struct.ScmObj** %stackaddr$env-ref58305
%stackaddr$prim58306 = alloca %struct.ScmObj*, align 8
%_95k51148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %_95k51148, %struct.ScmObj** %stackaddr$prim58306, align 8
%stackaddr$prim58307 = alloca %struct.ScmObj*, align 8
%current_45args57004 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %current_45args57004, %struct.ScmObj** %stackaddr$prim58307, align 8
%stackaddr$prim58308 = alloca %struct.ScmObj*, align 8
%anf_45bind50985 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57004)
store volatile %struct.ScmObj* %anf_45bind50985, %struct.ScmObj** %stackaddr$prim58308, align 8
%ae52335 = call %struct.ScmObj* @const_init_null()
%argslist57006$_37foldr1508390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58309 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1508391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst50871, %struct.ScmObj* %argslist57006$_37foldr1508390)
store volatile %struct.ScmObj* %argslist57006$_37foldr1508391, %struct.ScmObj** %stackaddr$prim58309, align 8
%stackaddr$prim58310 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1508392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52335, %struct.ScmObj* %argslist57006$_37foldr1508391)
store volatile %struct.ScmObj* %argslist57006$_37foldr1508392, %struct.ScmObj** %stackaddr$prim58310, align 8
%stackaddr$prim58311 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1508393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50985, %struct.ScmObj* %argslist57006$_37foldr1508392)
store volatile %struct.ScmObj* %argslist57006$_37foldr1508393, %struct.ScmObj** %stackaddr$prim58311, align 8
%stackaddr$prim58312 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1508394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51147, %struct.ScmObj* %argslist57006$_37foldr1508393)
store volatile %struct.ScmObj* %argslist57006$_37foldr1508394, %struct.ScmObj** %stackaddr$prim58312, align 8
%clofunc58313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr150839)
musttail call tailcc void %clofunc58313(%struct.ScmObj* %_37foldr150839, %struct.ScmObj* %argslist57006$_37foldr1508394)
ret void
}

define tailcc void @proc_clo$ae52303(%struct.ScmObj* %env$ae52303,%struct.ScmObj* %current_45args57007) {
%stackaddr$env-ref58314 = alloca %struct.ScmObj*, align 8
%f50872 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52303, i64 0)
store %struct.ScmObj* %f50872, %struct.ScmObj** %stackaddr$env-ref58314
%stackaddr$prim58315 = alloca %struct.ScmObj*, align 8
%k51149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57007)
store volatile %struct.ScmObj* %k51149, %struct.ScmObj** %stackaddr$prim58315, align 8
%stackaddr$prim58316 = alloca %struct.ScmObj*, align 8
%current_45args57008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57007)
store volatile %struct.ScmObj* %current_45args57008, %struct.ScmObj** %stackaddr$prim58316, align 8
%stackaddr$prim58317 = alloca %struct.ScmObj*, align 8
%v50874 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57008)
store volatile %struct.ScmObj* %v50874, %struct.ScmObj** %stackaddr$prim58317, align 8
%stackaddr$prim58318 = alloca %struct.ScmObj*, align 8
%current_45args57009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57008)
store volatile %struct.ScmObj* %current_45args57009, %struct.ScmObj** %stackaddr$prim58318, align 8
%stackaddr$prim58319 = alloca %struct.ScmObj*, align 8
%r50873 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57009)
store volatile %struct.ScmObj* %r50873, %struct.ScmObj** %stackaddr$prim58319, align 8
%stackaddr$makeclosure58320 = alloca %struct.ScmObj*, align 8
%fptrToInt58321 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52305 to i64
%ae52305 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58321)
store volatile %struct.ScmObj* %ae52305, %struct.ScmObj** %stackaddr$makeclosure58320, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52305, %struct.ScmObj* %r50873, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52305, %struct.ScmObj* %k51149, i64 1)
%argslist57015$f508720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58322 = alloca %struct.ScmObj*, align 8
%argslist57015$f508721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v50874, %struct.ScmObj* %argslist57015$f508720)
store volatile %struct.ScmObj* %argslist57015$f508721, %struct.ScmObj** %stackaddr$prim58322, align 8
%stackaddr$prim58323 = alloca %struct.ScmObj*, align 8
%argslist57015$f508722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52305, %struct.ScmObj* %argslist57015$f508721)
store volatile %struct.ScmObj* %argslist57015$f508722, %struct.ScmObj** %stackaddr$prim58323, align 8
%clofunc58324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50872)
musttail call tailcc void %clofunc58324(%struct.ScmObj* %f50872, %struct.ScmObj* %argslist57015$f508722)
ret void
}

define tailcc void @proc_clo$ae52305(%struct.ScmObj* %env$ae52305,%struct.ScmObj* %current_45args57011) {
%stackaddr$env-ref58325 = alloca %struct.ScmObj*, align 8
%r50873 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52305, i64 0)
store %struct.ScmObj* %r50873, %struct.ScmObj** %stackaddr$env-ref58325
%stackaddr$env-ref58326 = alloca %struct.ScmObj*, align 8
%k51149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52305, i64 1)
store %struct.ScmObj* %k51149, %struct.ScmObj** %stackaddr$env-ref58326
%stackaddr$prim58327 = alloca %struct.ScmObj*, align 8
%_95k51150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57011)
store volatile %struct.ScmObj* %_95k51150, %struct.ScmObj** %stackaddr$prim58327, align 8
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%current_45args57012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57011)
store volatile %struct.ScmObj* %current_45args57012, %struct.ScmObj** %stackaddr$prim58328, align 8
%stackaddr$prim58329 = alloca %struct.ScmObj*, align 8
%anf_45bind50984 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57012)
store volatile %struct.ScmObj* %anf_45bind50984, %struct.ScmObj** %stackaddr$prim58329, align 8
%stackaddr$prim58330 = alloca %struct.ScmObj*, align 8
%cpsprim51151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50984, %struct.ScmObj* %r50873)
store volatile %struct.ScmObj* %cpsprim51151, %struct.ScmObj** %stackaddr$prim58330, align 8
%ae52310 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57014$k511490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58331 = alloca %struct.ScmObj*, align 8
%argslist57014$k511491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51151, %struct.ScmObj* %argslist57014$k511490)
store volatile %struct.ScmObj* %argslist57014$k511491, %struct.ScmObj** %stackaddr$prim58331, align 8
%stackaddr$prim58332 = alloca %struct.ScmObj*, align 8
%argslist57014$k511492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52310, %struct.ScmObj* %argslist57014$k511491)
store volatile %struct.ScmObj* %argslist57014$k511492, %struct.ScmObj** %stackaddr$prim58332, align 8
%clofunc58333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51149)
musttail call tailcc void %clofunc58333(%struct.ScmObj* %k51149, %struct.ScmObj* %argslist57014$k511492)
ret void
}

define tailcc void @proc_clo$ae51914(%struct.ScmObj* %env$ae51914,%struct.ScmObj* %current_45args57019) {
%stackaddr$env-ref58334 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51914, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58334
%stackaddr$env-ref58335 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51914, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58335
%stackaddr$prim58336 = alloca %struct.ScmObj*, align 8
%k51152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57019)
store volatile %struct.ScmObj* %k51152, %struct.ScmObj** %stackaddr$prim58336, align 8
%stackaddr$prim58337 = alloca %struct.ScmObj*, align 8
%current_45args57020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57019)
store volatile %struct.ScmObj* %current_45args57020, %struct.ScmObj** %stackaddr$prim58337, align 8
%stackaddr$prim58338 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57020)
store volatile %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$prim58338, align 8
%ae51916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58339 = alloca %struct.ScmObj*, align 8
%fptrToInt58340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51917 to i64
%ae51917 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58340)
store volatile %struct.ScmObj* %ae51917, %struct.ScmObj** %stackaddr$makeclosure58339, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51917, %struct.ScmObj* %_37foldr50845, i64 2)
%argslist57077$k511520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58341 = alloca %struct.ScmObj*, align 8
%argslist57077$k511521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51917, %struct.ScmObj* %argslist57077$k511520)
store volatile %struct.ScmObj* %argslist57077$k511521, %struct.ScmObj** %stackaddr$prim58341, align 8
%stackaddr$prim58342 = alloca %struct.ScmObj*, align 8
%argslist57077$k511522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51916, %struct.ScmObj* %argslist57077$k511521)
store volatile %struct.ScmObj* %argslist57077$k511522, %struct.ScmObj** %stackaddr$prim58342, align 8
%clofunc58343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51152)
musttail call tailcc void %clofunc58343(%struct.ScmObj* %k51152, %struct.ScmObj* %argslist57077$k511522)
ret void
}

define tailcc void @proc_clo$ae51917(%struct.ScmObj* %env$ae51917,%struct.ScmObj* %args5084651153) {
%stackaddr$env-ref58344 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58344
%stackaddr$env-ref58345 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58345
%stackaddr$env-ref58346 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51917, i64 2)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58346
%stackaddr$prim58347 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args5084651153)
store volatile %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$prim58347, align 8
%stackaddr$prim58348 = alloca %struct.ScmObj*, align 8
%args50846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args5084651153)
store volatile %struct.ScmObj* %args50846, %struct.ScmObj** %stackaddr$prim58348, align 8
%stackaddr$prim58349 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args50846)
store volatile %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$prim58349, align 8
%stackaddr$prim58350 = alloca %struct.ScmObj*, align 8
%anf_45bind50971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50846)
store volatile %struct.ScmObj* %anf_45bind50971, %struct.ScmObj** %stackaddr$prim58350, align 8
%stackaddr$prim58351 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind50971)
store volatile %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$prim58351, align 8
%stackaddr$prim58352 = alloca %struct.ScmObj*, align 8
%anf_45bind50972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args50846)
store volatile %struct.ScmObj* %anf_45bind50972, %struct.ScmObj** %stackaddr$prim58352, align 8
%stackaddr$prim58353 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind50972)
store volatile %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$prim58353, align 8
%stackaddr$makeclosure58354 = alloca %struct.ScmObj*, align 8
%fptrToInt58355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51925 to i64
%ae51925 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58355)
store volatile %struct.ScmObj* %ae51925, %struct.ScmObj** %stackaddr$makeclosure58354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %k51154, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %f50849, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %acc50848, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %lsts50847, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51925, %struct.ScmObj* %_37foldr50845, i64 6)
%ae51926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58356 = alloca %struct.ScmObj*, align 8
%fptrToInt58357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51927 to i64
%ae51927 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58357)
store volatile %struct.ScmObj* %ae51927, %struct.ScmObj** %stackaddr$makeclosure58356, align 8
%argslist57076$ae519250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58358 = alloca %struct.ScmObj*, align 8
%argslist57076$ae519251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51927, %struct.ScmObj* %argslist57076$ae519250)
store volatile %struct.ScmObj* %argslist57076$ae519251, %struct.ScmObj** %stackaddr$prim58358, align 8
%stackaddr$prim58359 = alloca %struct.ScmObj*, align 8
%argslist57076$ae519252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51926, %struct.ScmObj* %argslist57076$ae519251)
store volatile %struct.ScmObj* %argslist57076$ae519252, %struct.ScmObj** %stackaddr$prim58359, align 8
%clofunc58360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51925)
musttail call tailcc void %clofunc58360(%struct.ScmObj* %ae51925, %struct.ScmObj* %argslist57076$ae519252)
ret void
}

define tailcc void @proc_clo$ae51925(%struct.ScmObj* %env$ae51925,%struct.ScmObj* %current_45args57022) {
%stackaddr$env-ref58361 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58361
%stackaddr$env-ref58362 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58362
%stackaddr$env-ref58363 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 2)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58363
%stackaddr$env-ref58364 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 3)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58364
%stackaddr$env-ref58365 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 4)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58365
%stackaddr$env-ref58366 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 5)
store %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$env-ref58366
%stackaddr$env-ref58367 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51925, i64 6)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58367
%stackaddr$prim58368 = alloca %struct.ScmObj*, align 8
%_95k51155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57022)
store volatile %struct.ScmObj* %_95k51155, %struct.ScmObj** %stackaddr$prim58368, align 8
%stackaddr$prim58369 = alloca %struct.ScmObj*, align 8
%current_45args57023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57022)
store volatile %struct.ScmObj* %current_45args57023, %struct.ScmObj** %stackaddr$prim58369, align 8
%stackaddr$prim58370 = alloca %struct.ScmObj*, align 8
%anf_45bind50973 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57023)
store volatile %struct.ScmObj* %anf_45bind50973, %struct.ScmObj** %stackaddr$prim58370, align 8
%stackaddr$makeclosure58371 = alloca %struct.ScmObj*, align 8
%fptrToInt58372 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51957 to i64
%ae51957 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58372)
store volatile %struct.ScmObj* %ae51957, %struct.ScmObj** %stackaddr$makeclosure58371, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %k51154, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %f50849, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %acc50848, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %lsts50847, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51957, %struct.ScmObj* %_37foldr50845, i64 6)
%ae51959 = call %struct.ScmObj* @const_init_false()
%argslist57069$_37foldr1508390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58373 = alloca %struct.ScmObj*, align 8
%argslist57069$_37foldr1508391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50847, %struct.ScmObj* %argslist57069$_37foldr1508390)
store volatile %struct.ScmObj* %argslist57069$_37foldr1508391, %struct.ScmObj** %stackaddr$prim58373, align 8
%stackaddr$prim58374 = alloca %struct.ScmObj*, align 8
%argslist57069$_37foldr1508392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51959, %struct.ScmObj* %argslist57069$_37foldr1508391)
store volatile %struct.ScmObj* %argslist57069$_37foldr1508392, %struct.ScmObj** %stackaddr$prim58374, align 8
%stackaddr$prim58375 = alloca %struct.ScmObj*, align 8
%argslist57069$_37foldr1508393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50973, %struct.ScmObj* %argslist57069$_37foldr1508392)
store volatile %struct.ScmObj* %argslist57069$_37foldr1508393, %struct.ScmObj** %stackaddr$prim58375, align 8
%stackaddr$prim58376 = alloca %struct.ScmObj*, align 8
%argslist57069$_37foldr1508394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51957, %struct.ScmObj* %argslist57069$_37foldr1508393)
store volatile %struct.ScmObj* %argslist57069$_37foldr1508394, %struct.ScmObj** %stackaddr$prim58376, align 8
%clofunc58377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr150839)
musttail call tailcc void %clofunc58377(%struct.ScmObj* %_37foldr150839, %struct.ScmObj* %argslist57069$_37foldr1508394)
ret void
}

define tailcc void @proc_clo$ae51957(%struct.ScmObj* %env$ae51957,%struct.ScmObj* %current_45args57025) {
%stackaddr$env-ref58378 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58378
%stackaddr$env-ref58379 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58379
%stackaddr$env-ref58380 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 2)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58380
%stackaddr$env-ref58381 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 3)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58381
%stackaddr$env-ref58382 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 4)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58382
%stackaddr$env-ref58383 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 5)
store %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$env-ref58383
%stackaddr$env-ref58384 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51957, i64 6)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58384
%stackaddr$prim58385 = alloca %struct.ScmObj*, align 8
%_95k51156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57025)
store volatile %struct.ScmObj* %_95k51156, %struct.ScmObj** %stackaddr$prim58385, align 8
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%current_45args57026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57025)
store volatile %struct.ScmObj* %current_45args57026, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%anf_45bind50974 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57026)
store volatile %struct.ScmObj* %anf_45bind50974, %struct.ScmObj** %stackaddr$prim58387, align 8
%truthy$cmp58388 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50974)
%cmp$cmp58388 = icmp eq i64 %truthy$cmp58388, 1
br i1 %cmp$cmp58388, label %truebranch$cmp58388, label %falsebranch$cmp58388
truebranch$cmp58388:
%ae51968 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57028$k511540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58389 = alloca %struct.ScmObj*, align 8
%argslist57028$k511541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50848, %struct.ScmObj* %argslist57028$k511540)
store volatile %struct.ScmObj* %argslist57028$k511541, %struct.ScmObj** %stackaddr$prim58389, align 8
%stackaddr$prim58390 = alloca %struct.ScmObj*, align 8
%argslist57028$k511542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51968, %struct.ScmObj* %argslist57028$k511541)
store volatile %struct.ScmObj* %argslist57028$k511542, %struct.ScmObj** %stackaddr$prim58390, align 8
%clofunc58391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51154)
musttail call tailcc void %clofunc58391(%struct.ScmObj* %k51154, %struct.ScmObj* %argslist57028$k511542)
ret void
falsebranch$cmp58388:
%stackaddr$makeclosure58392 = alloca %struct.ScmObj*, align 8
%fptrToInt58393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51973 to i64
%ae51973 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58393)
store volatile %struct.ScmObj* %ae51973, %struct.ScmObj** %stackaddr$makeclosure58392, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %k51154, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %f50849, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %acc50848, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %lsts50847, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51973, %struct.ScmObj* %_37foldr50845, i64 6)
%ae51974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58394 = alloca %struct.ScmObj*, align 8
%fptrToInt58395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51975 to i64
%ae51975 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58395)
store volatile %struct.ScmObj* %ae51975, %struct.ScmObj** %stackaddr$makeclosure58394, align 8
%argslist57068$ae519730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58396 = alloca %struct.ScmObj*, align 8
%argslist57068$ae519731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51975, %struct.ScmObj* %argslist57068$ae519730)
store volatile %struct.ScmObj* %argslist57068$ae519731, %struct.ScmObj** %stackaddr$prim58396, align 8
%stackaddr$prim58397 = alloca %struct.ScmObj*, align 8
%argslist57068$ae519732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51974, %struct.ScmObj* %argslist57068$ae519731)
store volatile %struct.ScmObj* %argslist57068$ae519732, %struct.ScmObj** %stackaddr$prim58397, align 8
%clofunc58398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51973)
musttail call tailcc void %clofunc58398(%struct.ScmObj* %ae51973, %struct.ScmObj* %argslist57068$ae519732)
ret void
}

define tailcc void @proc_clo$ae51973(%struct.ScmObj* %env$ae51973,%struct.ScmObj* %current_45args57029) {
%stackaddr$env-ref58399 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58399
%stackaddr$env-ref58400 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58400
%stackaddr$env-ref58401 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 2)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58401
%stackaddr$env-ref58402 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 3)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58402
%stackaddr$env-ref58403 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 4)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58403
%stackaddr$env-ref58404 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 5)
store %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$env-ref58404
%stackaddr$env-ref58405 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51973, i64 6)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58405
%stackaddr$prim58406 = alloca %struct.ScmObj*, align 8
%_95k51157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57029)
store volatile %struct.ScmObj* %_95k51157, %struct.ScmObj** %stackaddr$prim58406, align 8
%stackaddr$prim58407 = alloca %struct.ScmObj*, align 8
%current_45args57030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57029)
store volatile %struct.ScmObj* %current_45args57030, %struct.ScmObj** %stackaddr$prim58407, align 8
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%anf_45bind50975 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57030)
store volatile %struct.ScmObj* %anf_45bind50975, %struct.ScmObj** %stackaddr$prim58408, align 8
%stackaddr$makeclosure58409 = alloca %struct.ScmObj*, align 8
%fptrToInt58410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51994 to i64
%ae51994 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58410)
store volatile %struct.ScmObj* %ae51994, %struct.ScmObj** %stackaddr$makeclosure58409, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %_37map150835, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %k51154, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %f50849, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %acc50848, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %lsts50847, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51994, %struct.ScmObj* %_37foldr50845, i64 6)
%argslist57063$_37map1508350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58411 = alloca %struct.ScmObj*, align 8
%argslist57063$_37map1508351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50847, %struct.ScmObj* %argslist57063$_37map1508350)
store volatile %struct.ScmObj* %argslist57063$_37map1508351, %struct.ScmObj** %stackaddr$prim58411, align 8
%stackaddr$prim58412 = alloca %struct.ScmObj*, align 8
%argslist57063$_37map1508352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50975, %struct.ScmObj* %argslist57063$_37map1508351)
store volatile %struct.ScmObj* %argslist57063$_37map1508352, %struct.ScmObj** %stackaddr$prim58412, align 8
%stackaddr$prim58413 = alloca %struct.ScmObj*, align 8
%argslist57063$_37map1508353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51994, %struct.ScmObj* %argslist57063$_37map1508352)
store volatile %struct.ScmObj* %argslist57063$_37map1508353, %struct.ScmObj** %stackaddr$prim58413, align 8
%clofunc58414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map150835)
musttail call tailcc void %clofunc58414(%struct.ScmObj* %_37map150835, %struct.ScmObj* %argslist57063$_37map1508353)
ret void
}

define tailcc void @proc_clo$ae51994(%struct.ScmObj* %env$ae51994,%struct.ScmObj* %current_45args57032) {
%stackaddr$env-ref58415 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58415
%stackaddr$env-ref58416 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 1)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58416
%stackaddr$env-ref58417 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 2)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58417
%stackaddr$env-ref58418 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 3)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58418
%stackaddr$env-ref58419 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 4)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58419
%stackaddr$env-ref58420 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 5)
store %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$env-ref58420
%stackaddr$env-ref58421 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51994, i64 6)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58421
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%_95k51158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57032)
store volatile %struct.ScmObj* %_95k51158, %struct.ScmObj** %stackaddr$prim58422, align 8
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%current_45args57033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57032)
store volatile %struct.ScmObj* %current_45args57033, %struct.ScmObj** %stackaddr$prim58423, align 8
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%lsts_4350854 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57033)
store volatile %struct.ScmObj* %lsts_4350854, %struct.ScmObj** %stackaddr$prim58424, align 8
%stackaddr$makeclosure58425 = alloca %struct.ScmObj*, align 8
%fptrToInt58426 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51997 to i64
%ae51997 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58426)
store volatile %struct.ScmObj* %ae51997, %struct.ScmObj** %stackaddr$makeclosure58425, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %lsts_4350854, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %_37map150835, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %k51154, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %f50849, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %acc50848, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %lsts50847, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51997, %struct.ScmObj* %_37foldr50845, i64 7)
%ae51998 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58427 = alloca %struct.ScmObj*, align 8
%fptrToInt58428 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51999 to i64
%ae51999 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58428)
store volatile %struct.ScmObj* %ae51999, %struct.ScmObj** %stackaddr$makeclosure58427, align 8
%argslist57062$ae519970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58429 = alloca %struct.ScmObj*, align 8
%argslist57062$ae519971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51999, %struct.ScmObj* %argslist57062$ae519970)
store volatile %struct.ScmObj* %argslist57062$ae519971, %struct.ScmObj** %stackaddr$prim58429, align 8
%stackaddr$prim58430 = alloca %struct.ScmObj*, align 8
%argslist57062$ae519972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51998, %struct.ScmObj* %argslist57062$ae519971)
store volatile %struct.ScmObj* %argslist57062$ae519972, %struct.ScmObj** %stackaddr$prim58430, align 8
%clofunc58431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51997)
musttail call tailcc void %clofunc58431(%struct.ScmObj* %ae51997, %struct.ScmObj* %argslist57062$ae519972)
ret void
}

define tailcc void @proc_clo$ae51997(%struct.ScmObj* %env$ae51997,%struct.ScmObj* %current_45args57035) {
%stackaddr$env-ref58432 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58432
%stackaddr$env-ref58433 = alloca %struct.ScmObj*, align 8
%lsts_4350854 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 1)
store %struct.ScmObj* %lsts_4350854, %struct.ScmObj** %stackaddr$env-ref58433
%stackaddr$env-ref58434 = alloca %struct.ScmObj*, align 8
%_37map150835 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 2)
store %struct.ScmObj* %_37map150835, %struct.ScmObj** %stackaddr$env-ref58434
%stackaddr$env-ref58435 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 3)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58435
%stackaddr$env-ref58436 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 4)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58436
%stackaddr$env-ref58437 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 5)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58437
%stackaddr$env-ref58438 = alloca %struct.ScmObj*, align 8
%lsts50847 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 6)
store %struct.ScmObj* %lsts50847, %struct.ScmObj** %stackaddr$env-ref58438
%stackaddr$env-ref58439 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51997, i64 7)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58439
%stackaddr$prim58440 = alloca %struct.ScmObj*, align 8
%_95k51159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57035)
store volatile %struct.ScmObj* %_95k51159, %struct.ScmObj** %stackaddr$prim58440, align 8
%stackaddr$prim58441 = alloca %struct.ScmObj*, align 8
%current_45args57036 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57035)
store volatile %struct.ScmObj* %current_45args57036, %struct.ScmObj** %stackaddr$prim58441, align 8
%stackaddr$prim58442 = alloca %struct.ScmObj*, align 8
%anf_45bind50976 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57036)
store volatile %struct.ScmObj* %anf_45bind50976, %struct.ScmObj** %stackaddr$prim58442, align 8
%stackaddr$makeclosure58443 = alloca %struct.ScmObj*, align 8
%fptrToInt58444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52018 to i64
%ae52018 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58444)
store volatile %struct.ScmObj* %ae52018, %struct.ScmObj** %stackaddr$makeclosure58443, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %lsts_4350854, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %k51154, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %f50849, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %acc50848, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %_37foldr50845, i64 5)
%argslist57057$_37map1508350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58445 = alloca %struct.ScmObj*, align 8
%argslist57057$_37map1508351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts50847, %struct.ScmObj* %argslist57057$_37map1508350)
store volatile %struct.ScmObj* %argslist57057$_37map1508351, %struct.ScmObj** %stackaddr$prim58445, align 8
%stackaddr$prim58446 = alloca %struct.ScmObj*, align 8
%argslist57057$_37map1508352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50976, %struct.ScmObj* %argslist57057$_37map1508351)
store volatile %struct.ScmObj* %argslist57057$_37map1508352, %struct.ScmObj** %stackaddr$prim58446, align 8
%stackaddr$prim58447 = alloca %struct.ScmObj*, align 8
%argslist57057$_37map1508353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52018, %struct.ScmObj* %argslist57057$_37map1508352)
store volatile %struct.ScmObj* %argslist57057$_37map1508353, %struct.ScmObj** %stackaddr$prim58447, align 8
%clofunc58448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map150835)
musttail call tailcc void %clofunc58448(%struct.ScmObj* %_37map150835, %struct.ScmObj* %argslist57057$_37map1508353)
ret void
}

define tailcc void @proc_clo$ae52018(%struct.ScmObj* %env$ae52018,%struct.ScmObj* %current_45args57038) {
%stackaddr$env-ref58449 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58449
%stackaddr$env-ref58450 = alloca %struct.ScmObj*, align 8
%lsts_4350854 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 1)
store %struct.ScmObj* %lsts_4350854, %struct.ScmObj** %stackaddr$env-ref58450
%stackaddr$env-ref58451 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 2)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58451
%stackaddr$env-ref58452 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 3)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58452
%stackaddr$env-ref58453 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 4)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58453
%stackaddr$env-ref58454 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 5)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58454
%stackaddr$prim58455 = alloca %struct.ScmObj*, align 8
%_95k51160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57038)
store volatile %struct.ScmObj* %_95k51160, %struct.ScmObj** %stackaddr$prim58455, align 8
%stackaddr$prim58456 = alloca %struct.ScmObj*, align 8
%current_45args57039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57038)
store volatile %struct.ScmObj* %current_45args57039, %struct.ScmObj** %stackaddr$prim58456, align 8
%stackaddr$prim58457 = alloca %struct.ScmObj*, align 8
%vs50852 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57039)
store volatile %struct.ScmObj* %vs50852, %struct.ScmObj** %stackaddr$prim58457, align 8
%stackaddr$makeclosure58458 = alloca %struct.ScmObj*, align 8
%fptrToInt58459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52021 to i64
%ae52021 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58459)
store volatile %struct.ScmObj* %ae52021, %struct.ScmObj** %stackaddr$makeclosure58458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %_37foldr150839, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %lsts_4350854, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %vs50852, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %k51154, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %f50849, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %acc50848, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52021, %struct.ScmObj* %_37foldr50845, i64 6)
%ae52022 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58460 = alloca %struct.ScmObj*, align 8
%fptrToInt58461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52023 to i64
%ae52023 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58461)
store volatile %struct.ScmObj* %ae52023, %struct.ScmObj** %stackaddr$makeclosure58460, align 8
%argslist57056$ae520210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58462 = alloca %struct.ScmObj*, align 8
%argslist57056$ae520211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52023, %struct.ScmObj* %argslist57056$ae520210)
store volatile %struct.ScmObj* %argslist57056$ae520211, %struct.ScmObj** %stackaddr$prim58462, align 8
%stackaddr$prim58463 = alloca %struct.ScmObj*, align 8
%argslist57056$ae520212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52022, %struct.ScmObj* %argslist57056$ae520211)
store volatile %struct.ScmObj* %argslist57056$ae520212, %struct.ScmObj** %stackaddr$prim58463, align 8
%clofunc58464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52021)
musttail call tailcc void %clofunc58464(%struct.ScmObj* %ae52021, %struct.ScmObj* %argslist57056$ae520212)
ret void
}

define tailcc void @proc_clo$ae52021(%struct.ScmObj* %env$ae52021,%struct.ScmObj* %current_45args57041) {
%stackaddr$env-ref58465 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 0)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58465
%stackaddr$env-ref58466 = alloca %struct.ScmObj*, align 8
%lsts_4350854 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 1)
store %struct.ScmObj* %lsts_4350854, %struct.ScmObj** %stackaddr$env-ref58466
%stackaddr$env-ref58467 = alloca %struct.ScmObj*, align 8
%vs50852 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 2)
store %struct.ScmObj* %vs50852, %struct.ScmObj** %stackaddr$env-ref58467
%stackaddr$env-ref58468 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 3)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58468
%stackaddr$env-ref58469 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 4)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58469
%stackaddr$env-ref58470 = alloca %struct.ScmObj*, align 8
%acc50848 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 5)
store %struct.ScmObj* %acc50848, %struct.ScmObj** %stackaddr$env-ref58470
%stackaddr$env-ref58471 = alloca %struct.ScmObj*, align 8
%_37foldr50845 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52021, i64 6)
store %struct.ScmObj* %_37foldr50845, %struct.ScmObj** %stackaddr$env-ref58471
%stackaddr$prim58472 = alloca %struct.ScmObj*, align 8
%_95k51161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57041)
store volatile %struct.ScmObj* %_95k51161, %struct.ScmObj** %stackaddr$prim58472, align 8
%stackaddr$prim58473 = alloca %struct.ScmObj*, align 8
%current_45args57042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57041)
store volatile %struct.ScmObj* %current_45args57042, %struct.ScmObj** %stackaddr$prim58473, align 8
%stackaddr$prim58474 = alloca %struct.ScmObj*, align 8
%anf_45bind50977 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57042)
store volatile %struct.ScmObj* %anf_45bind50977, %struct.ScmObj** %stackaddr$prim58474, align 8
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%anf_45bind50978 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50848, %struct.ScmObj* %lsts_4350854)
store volatile %struct.ScmObj* %anf_45bind50978, %struct.ScmObj** %stackaddr$prim58475, align 8
%stackaddr$prim58476 = alloca %struct.ScmObj*, align 8
%anf_45bind50979 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50849, %struct.ScmObj* %anf_45bind50978)
store volatile %struct.ScmObj* %anf_45bind50979, %struct.ScmObj** %stackaddr$prim58476, align 8
%stackaddr$makeclosure58477 = alloca %struct.ScmObj*, align 8
%fptrToInt58478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52047 to i64
%ae52047 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58478)
store volatile %struct.ScmObj* %ae52047, %struct.ScmObj** %stackaddr$makeclosure58477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52047, %struct.ScmObj* %anf_45bind50977, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52047, %struct.ScmObj* %f50849, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52047, %struct.ScmObj* %_37foldr150839, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52047, %struct.ScmObj* %vs50852, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52047, %struct.ScmObj* %k51154, i64 4)
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%cpsargs51165 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52047, %struct.ScmObj* %anf_45bind50979)
store volatile %struct.ScmObj* %cpsargs51165, %struct.ScmObj** %stackaddr$prim58479, align 8
%clofunc58480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr50845)
musttail call tailcc void %clofunc58480(%struct.ScmObj* %_37foldr50845, %struct.ScmObj* %cpsargs51165)
ret void
}

define tailcc void @proc_clo$ae52047(%struct.ScmObj* %env$ae52047,%struct.ScmObj* %current_45args57044) {
%stackaddr$env-ref58481 = alloca %struct.ScmObj*, align 8
%anf_45bind50977 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52047, i64 0)
store %struct.ScmObj* %anf_45bind50977, %struct.ScmObj** %stackaddr$env-ref58481
%stackaddr$env-ref58482 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52047, i64 1)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58482
%stackaddr$env-ref58483 = alloca %struct.ScmObj*, align 8
%_37foldr150839 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52047, i64 2)
store %struct.ScmObj* %_37foldr150839, %struct.ScmObj** %stackaddr$env-ref58483
%stackaddr$env-ref58484 = alloca %struct.ScmObj*, align 8
%vs50852 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52047, i64 3)
store %struct.ScmObj* %vs50852, %struct.ScmObj** %stackaddr$env-ref58484
%stackaddr$env-ref58485 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52047, i64 4)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58485
%stackaddr$prim58486 = alloca %struct.ScmObj*, align 8
%_95k51162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57044)
store volatile %struct.ScmObj* %_95k51162, %struct.ScmObj** %stackaddr$prim58486, align 8
%stackaddr$prim58487 = alloca %struct.ScmObj*, align 8
%current_45args57045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57044)
store volatile %struct.ScmObj* %current_45args57045, %struct.ScmObj** %stackaddr$prim58487, align 8
%stackaddr$prim58488 = alloca %struct.ScmObj*, align 8
%anf_45bind50980 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57045)
store volatile %struct.ScmObj* %anf_45bind50980, %struct.ScmObj** %stackaddr$prim58488, align 8
%ae52052 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58489 = alloca %struct.ScmObj*, align 8
%anf_45bind50981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50980, %struct.ScmObj* %ae52052)
store volatile %struct.ScmObj* %anf_45bind50981, %struct.ScmObj** %stackaddr$prim58489, align 8
%stackaddr$makeclosure58490 = alloca %struct.ScmObj*, align 8
%fptrToInt58491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52054 to i64
%ae52054 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58491)
store volatile %struct.ScmObj* %ae52054, %struct.ScmObj** %stackaddr$makeclosure58490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52054, %struct.ScmObj* %k51154, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52054, %struct.ScmObj* %f50849, i64 1)
%argslist57050$_37foldr1508390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58492 = alloca %struct.ScmObj*, align 8
%argslist57050$_37foldr1508391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs50852, %struct.ScmObj* %argslist57050$_37foldr1508390)
store volatile %struct.ScmObj* %argslist57050$_37foldr1508391, %struct.ScmObj** %stackaddr$prim58492, align 8
%stackaddr$prim58493 = alloca %struct.ScmObj*, align 8
%argslist57050$_37foldr1508392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50981, %struct.ScmObj* %argslist57050$_37foldr1508391)
store volatile %struct.ScmObj* %argslist57050$_37foldr1508392, %struct.ScmObj** %stackaddr$prim58493, align 8
%stackaddr$prim58494 = alloca %struct.ScmObj*, align 8
%argslist57050$_37foldr1508393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50977, %struct.ScmObj* %argslist57050$_37foldr1508392)
store volatile %struct.ScmObj* %argslist57050$_37foldr1508393, %struct.ScmObj** %stackaddr$prim58494, align 8
%stackaddr$prim58495 = alloca %struct.ScmObj*, align 8
%argslist57050$_37foldr1508394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52054, %struct.ScmObj* %argslist57050$_37foldr1508393)
store volatile %struct.ScmObj* %argslist57050$_37foldr1508394, %struct.ScmObj** %stackaddr$prim58495, align 8
%clofunc58496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr150839)
musttail call tailcc void %clofunc58496(%struct.ScmObj* %_37foldr150839, %struct.ScmObj* %argslist57050$_37foldr1508394)
ret void
}

define tailcc void @proc_clo$ae52054(%struct.ScmObj* %env$ae52054,%struct.ScmObj* %current_45args57047) {
%stackaddr$env-ref58497 = alloca %struct.ScmObj*, align 8
%k51154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52054, i64 0)
store %struct.ScmObj* %k51154, %struct.ScmObj** %stackaddr$env-ref58497
%stackaddr$env-ref58498 = alloca %struct.ScmObj*, align 8
%f50849 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52054, i64 1)
store %struct.ScmObj* %f50849, %struct.ScmObj** %stackaddr$env-ref58498
%stackaddr$prim58499 = alloca %struct.ScmObj*, align 8
%_95k51163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57047)
store volatile %struct.ScmObj* %_95k51163, %struct.ScmObj** %stackaddr$prim58499, align 8
%stackaddr$prim58500 = alloca %struct.ScmObj*, align 8
%current_45args57048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57047)
store volatile %struct.ScmObj* %current_45args57048, %struct.ScmObj** %stackaddr$prim58500, align 8
%stackaddr$prim58501 = alloca %struct.ScmObj*, align 8
%anf_45bind50982 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57048)
store volatile %struct.ScmObj* %anf_45bind50982, %struct.ScmObj** %stackaddr$prim58501, align 8
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%cpsargs51164 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51154, %struct.ScmObj* %anf_45bind50982)
store volatile %struct.ScmObj* %cpsargs51164, %struct.ScmObj** %stackaddr$prim58502, align 8
%clofunc58503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50849)
musttail call tailcc void %clofunc58503(%struct.ScmObj* %f50849, %struct.ScmObj* %cpsargs51164)
ret void
}

define tailcc void @proc_clo$ae52023(%struct.ScmObj* %env$ae52023,%struct.ScmObj* %current_45args57051) {
%stackaddr$prim58504 = alloca %struct.ScmObj*, align 8
%k51166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57051)
store volatile %struct.ScmObj* %k51166, %struct.ScmObj** %stackaddr$prim58504, align 8
%stackaddr$prim58505 = alloca %struct.ScmObj*, align 8
%current_45args57052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57051)
store volatile %struct.ScmObj* %current_45args57052, %struct.ScmObj** %stackaddr$prim58505, align 8
%stackaddr$prim58506 = alloca %struct.ScmObj*, align 8
%a50857 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57052)
store volatile %struct.ScmObj* %a50857, %struct.ScmObj** %stackaddr$prim58506, align 8
%stackaddr$prim58507 = alloca %struct.ScmObj*, align 8
%current_45args57053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57052)
store volatile %struct.ScmObj* %current_45args57053, %struct.ScmObj** %stackaddr$prim58507, align 8
%stackaddr$prim58508 = alloca %struct.ScmObj*, align 8
%b50856 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57053)
store volatile %struct.ScmObj* %b50856, %struct.ScmObj** %stackaddr$prim58508, align 8
%stackaddr$prim58509 = alloca %struct.ScmObj*, align 8
%cpsprim51167 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a50857, %struct.ScmObj* %b50856)
store volatile %struct.ScmObj* %cpsprim51167, %struct.ScmObj** %stackaddr$prim58509, align 8
%ae52027 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57055$k511660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58510 = alloca %struct.ScmObj*, align 8
%argslist57055$k511661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51167, %struct.ScmObj* %argslist57055$k511660)
store volatile %struct.ScmObj* %argslist57055$k511661, %struct.ScmObj** %stackaddr$prim58510, align 8
%stackaddr$prim58511 = alloca %struct.ScmObj*, align 8
%argslist57055$k511662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52027, %struct.ScmObj* %argslist57055$k511661)
store volatile %struct.ScmObj* %argslist57055$k511662, %struct.ScmObj** %stackaddr$prim58511, align 8
%clofunc58512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51166)
musttail call tailcc void %clofunc58512(%struct.ScmObj* %k51166, %struct.ScmObj* %argslist57055$k511662)
ret void
}

define tailcc void @proc_clo$ae51999(%struct.ScmObj* %env$ae51999,%struct.ScmObj* %current_45args57058) {
%stackaddr$prim58513 = alloca %struct.ScmObj*, align 8
%k51168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57058)
store volatile %struct.ScmObj* %k51168, %struct.ScmObj** %stackaddr$prim58513, align 8
%stackaddr$prim58514 = alloca %struct.ScmObj*, align 8
%current_45args57059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57058)
store volatile %struct.ScmObj* %current_45args57059, %struct.ScmObj** %stackaddr$prim58514, align 8
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%x50853 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57059)
store volatile %struct.ScmObj* %x50853, %struct.ScmObj** %stackaddr$prim58515, align 8
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%cpsprim51169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x50853)
store volatile %struct.ScmObj* %cpsprim51169, %struct.ScmObj** %stackaddr$prim58516, align 8
%ae52002 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57061$k511680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58517 = alloca %struct.ScmObj*, align 8
%argslist57061$k511681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51169, %struct.ScmObj* %argslist57061$k511680)
store volatile %struct.ScmObj* %argslist57061$k511681, %struct.ScmObj** %stackaddr$prim58517, align 8
%stackaddr$prim58518 = alloca %struct.ScmObj*, align 8
%argslist57061$k511682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52002, %struct.ScmObj* %argslist57061$k511681)
store volatile %struct.ScmObj* %argslist57061$k511682, %struct.ScmObj** %stackaddr$prim58518, align 8
%clofunc58519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51168)
musttail call tailcc void %clofunc58519(%struct.ScmObj* %k51168, %struct.ScmObj* %argslist57061$k511682)
ret void
}

define tailcc void @proc_clo$ae51975(%struct.ScmObj* %env$ae51975,%struct.ScmObj* %current_45args57064) {
%stackaddr$prim58520 = alloca %struct.ScmObj*, align 8
%k51170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57064)
store volatile %struct.ScmObj* %k51170, %struct.ScmObj** %stackaddr$prim58520, align 8
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%current_45args57065 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57064)
store volatile %struct.ScmObj* %current_45args57065, %struct.ScmObj** %stackaddr$prim58521, align 8
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%x50855 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57065)
store volatile %struct.ScmObj* %x50855, %struct.ScmObj** %stackaddr$prim58522, align 8
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%cpsprim51171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x50855)
store volatile %struct.ScmObj* %cpsprim51171, %struct.ScmObj** %stackaddr$prim58523, align 8
%ae51978 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57067$k511700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58524 = alloca %struct.ScmObj*, align 8
%argslist57067$k511701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51171, %struct.ScmObj* %argslist57067$k511700)
store volatile %struct.ScmObj* %argslist57067$k511701, %struct.ScmObj** %stackaddr$prim58524, align 8
%stackaddr$prim58525 = alloca %struct.ScmObj*, align 8
%argslist57067$k511702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51978, %struct.ScmObj* %argslist57067$k511701)
store volatile %struct.ScmObj* %argslist57067$k511702, %struct.ScmObj** %stackaddr$prim58525, align 8
%clofunc58526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51170)
musttail call tailcc void %clofunc58526(%struct.ScmObj* %k51170, %struct.ScmObj* %argslist57067$k511702)
ret void
}

define tailcc void @proc_clo$ae51927(%struct.ScmObj* %env$ae51927,%struct.ScmObj* %current_45args57070) {
%stackaddr$prim58527 = alloca %struct.ScmObj*, align 8
%k51172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57070)
store volatile %struct.ScmObj* %k51172, %struct.ScmObj** %stackaddr$prim58527, align 8
%stackaddr$prim58528 = alloca %struct.ScmObj*, align 8
%current_45args57071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57070)
store volatile %struct.ScmObj* %current_45args57071, %struct.ScmObj** %stackaddr$prim58528, align 8
%stackaddr$prim58529 = alloca %struct.ScmObj*, align 8
%lst50851 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57071)
store volatile %struct.ScmObj* %lst50851, %struct.ScmObj** %stackaddr$prim58529, align 8
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%current_45args57072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57071)
store volatile %struct.ScmObj* %current_45args57072, %struct.ScmObj** %stackaddr$prim58530, align 8
%stackaddr$prim58531 = alloca %struct.ScmObj*, align 8
%b50850 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57072)
store volatile %struct.ScmObj* %b50850, %struct.ScmObj** %stackaddr$prim58531, align 8
%truthy$cmp58532 = call i64 @is_truthy_value(%struct.ScmObj* %b50850)
%cmp$cmp58532 = icmp eq i64 %truthy$cmp58532, 1
br i1 %cmp$cmp58532, label %truebranch$cmp58532, label %falsebranch$cmp58532
truebranch$cmp58532:
%ae51930 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57074$k511720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%argslist57074$k511721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b50850, %struct.ScmObj* %argslist57074$k511720)
store volatile %struct.ScmObj* %argslist57074$k511721, %struct.ScmObj** %stackaddr$prim58533, align 8
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%argslist57074$k511722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51930, %struct.ScmObj* %argslist57074$k511721)
store volatile %struct.ScmObj* %argslist57074$k511722, %struct.ScmObj** %stackaddr$prim58534, align 8
%clofunc58535 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51172)
musttail call tailcc void %clofunc58535(%struct.ScmObj* %k51172, %struct.ScmObj* %argslist57074$k511722)
ret void
falsebranch$cmp58532:
%stackaddr$prim58536 = alloca %struct.ScmObj*, align 8
%cpsprim51173 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50851)
store volatile %struct.ScmObj* %cpsprim51173, %struct.ScmObj** %stackaddr$prim58536, align 8
%ae51937 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57075$k511720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%argslist57075$k511721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51173, %struct.ScmObj* %argslist57075$k511720)
store volatile %struct.ScmObj* %argslist57075$k511721, %struct.ScmObj** %stackaddr$prim58537, align 8
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%argslist57075$k511722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51937, %struct.ScmObj* %argslist57075$k511721)
store volatile %struct.ScmObj* %argslist57075$k511722, %struct.ScmObj** %stackaddr$prim58538, align 8
%clofunc58539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51172)
musttail call tailcc void %clofunc58539(%struct.ScmObj* %k51172, %struct.ScmObj* %argslist57075$k511722)
ret void
}

define tailcc void @proc_clo$ae51884(%struct.ScmObj* %env$ae51884,%struct.ScmObj* %current_45args57079) {
%stackaddr$env-ref58540 = alloca %struct.ScmObj*, align 8
%_37length50828 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51884, i64 0)
store %struct.ScmObj* %_37length50828, %struct.ScmObj** %stackaddr$env-ref58540
%stackaddr$env-ref58541 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51884, i64 1)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref58541
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%k51174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57079)
store volatile %struct.ScmObj* %k51174, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%current_45args57080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57079)
store volatile %struct.ScmObj* %current_45args57080, %struct.ScmObj** %stackaddr$prim58543, align 8
%stackaddr$prim58544 = alloca %struct.ScmObj*, align 8
%lst50860 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57080)
store volatile %struct.ScmObj* %lst50860, %struct.ScmObj** %stackaddr$prim58544, align 8
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%current_45args57081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57080)
store volatile %struct.ScmObj* %current_45args57081, %struct.ScmObj** %stackaddr$prim58545, align 8
%stackaddr$prim58546 = alloca %struct.ScmObj*, align 8
%n50859 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57081)
store volatile %struct.ScmObj* %n50859, %struct.ScmObj** %stackaddr$prim58546, align 8
%stackaddr$makeclosure58547 = alloca %struct.ScmObj*, align 8
%fptrToInt58548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51886 to i64
%ae51886 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58548)
store volatile %struct.ScmObj* %ae51886, %struct.ScmObj** %stackaddr$makeclosure58547, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51886, %struct.ScmObj* %lst50860, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51886, %struct.ScmObj* %n50859, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51886, %struct.ScmObj* %k51174, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51886, %struct.ScmObj* %_37take50831, i64 3)
%argslist57087$_37length508280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58549 = alloca %struct.ScmObj*, align 8
%argslist57087$_37length508281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst50860, %struct.ScmObj* %argslist57087$_37length508280)
store volatile %struct.ScmObj* %argslist57087$_37length508281, %struct.ScmObj** %stackaddr$prim58549, align 8
%stackaddr$prim58550 = alloca %struct.ScmObj*, align 8
%argslist57087$_37length508282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51886, %struct.ScmObj* %argslist57087$_37length508281)
store volatile %struct.ScmObj* %argslist57087$_37length508282, %struct.ScmObj** %stackaddr$prim58550, align 8
%clofunc58551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length50828)
musttail call tailcc void %clofunc58551(%struct.ScmObj* %_37length50828, %struct.ScmObj* %argslist57087$_37length508282)
ret void
}

define tailcc void @proc_clo$ae51886(%struct.ScmObj* %env$ae51886,%struct.ScmObj* %current_45args57083) {
%stackaddr$env-ref58552 = alloca %struct.ScmObj*, align 8
%lst50860 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51886, i64 0)
store %struct.ScmObj* %lst50860, %struct.ScmObj** %stackaddr$env-ref58552
%stackaddr$env-ref58553 = alloca %struct.ScmObj*, align 8
%n50859 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51886, i64 1)
store %struct.ScmObj* %n50859, %struct.ScmObj** %stackaddr$env-ref58553
%stackaddr$env-ref58554 = alloca %struct.ScmObj*, align 8
%k51174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51886, i64 2)
store %struct.ScmObj* %k51174, %struct.ScmObj** %stackaddr$env-ref58554
%stackaddr$env-ref58555 = alloca %struct.ScmObj*, align 8
%_37take50831 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51886, i64 3)
store %struct.ScmObj* %_37take50831, %struct.ScmObj** %stackaddr$env-ref58555
%stackaddr$prim58556 = alloca %struct.ScmObj*, align 8
%_95k51175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57083)
store volatile %struct.ScmObj* %_95k51175, %struct.ScmObj** %stackaddr$prim58556, align 8
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%current_45args57084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57083)
store volatile %struct.ScmObj* %current_45args57084, %struct.ScmObj** %stackaddr$prim58557, align 8
%stackaddr$prim58558 = alloca %struct.ScmObj*, align 8
%anf_45bind50969 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57084)
store volatile %struct.ScmObj* %anf_45bind50969, %struct.ScmObj** %stackaddr$prim58558, align 8
%stackaddr$prim58559 = alloca %struct.ScmObj*, align 8
%anf_45bind50970 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind50969, %struct.ScmObj* %n50859)
store volatile %struct.ScmObj* %anf_45bind50970, %struct.ScmObj** %stackaddr$prim58559, align 8
%argslist57086$_37take508310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58560 = alloca %struct.ScmObj*, align 8
%argslist57086$_37take508311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50970, %struct.ScmObj* %argslist57086$_37take508310)
store volatile %struct.ScmObj* %argslist57086$_37take508311, %struct.ScmObj** %stackaddr$prim58560, align 8
%stackaddr$prim58561 = alloca %struct.ScmObj*, align 8
%argslist57086$_37take508312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst50860, %struct.ScmObj* %argslist57086$_37take508311)
store volatile %struct.ScmObj* %argslist57086$_37take508312, %struct.ScmObj** %stackaddr$prim58561, align 8
%stackaddr$prim58562 = alloca %struct.ScmObj*, align 8
%argslist57086$_37take508313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51174, %struct.ScmObj* %argslist57086$_37take508312)
store volatile %struct.ScmObj* %argslist57086$_37take508313, %struct.ScmObj** %stackaddr$prim58562, align 8
%clofunc58563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take50831)
musttail call tailcc void %clofunc58563(%struct.ScmObj* %_37take50831, %struct.ScmObj* %argslist57086$_37take508313)
ret void
}

define tailcc void @proc_clo$ae51830(%struct.ScmObj* %env$ae51830,%struct.ScmObj* %current_45args57089) {
%stackaddr$env-ref58564 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51830, i64 0)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref58564
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%k51176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57089)
store volatile %struct.ScmObj* %k51176, %struct.ScmObj** %stackaddr$prim58565, align 8
%stackaddr$prim58566 = alloca %struct.ScmObj*, align 8
%current_45args57090 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57089)
store volatile %struct.ScmObj* %current_45args57090, %struct.ScmObj** %stackaddr$prim58566, align 8
%stackaddr$prim58567 = alloca %struct.ScmObj*, align 8
%lst50862 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57090)
store volatile %struct.ScmObj* %lst50862, %struct.ScmObj** %stackaddr$prim58567, align 8
%stackaddr$makeclosure58568 = alloca %struct.ScmObj*, align 8
%fptrToInt58569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51831 to i64
%ae51831 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58569)
store volatile %struct.ScmObj* %ae51831, %struct.ScmObj** %stackaddr$makeclosure58568, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51831, %struct.ScmObj* %k51176, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51831, %struct.ScmObj* %_37foldl150823, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51831, %struct.ScmObj* %lst50862, i64 2)
%ae51832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58570 = alloca %struct.ScmObj*, align 8
%fptrToInt58571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51833 to i64
%ae51833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58571)
store volatile %struct.ScmObj* %ae51833, %struct.ScmObj** %stackaddr$makeclosure58570, align 8
%argslist57101$ae518310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58572 = alloca %struct.ScmObj*, align 8
%argslist57101$ae518311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51833, %struct.ScmObj* %argslist57101$ae518310)
store volatile %struct.ScmObj* %argslist57101$ae518311, %struct.ScmObj** %stackaddr$prim58572, align 8
%stackaddr$prim58573 = alloca %struct.ScmObj*, align 8
%argslist57101$ae518312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51832, %struct.ScmObj* %argslist57101$ae518311)
store volatile %struct.ScmObj* %argslist57101$ae518312, %struct.ScmObj** %stackaddr$prim58573, align 8
%clofunc58574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51831)
musttail call tailcc void %clofunc58574(%struct.ScmObj* %ae51831, %struct.ScmObj* %argslist57101$ae518312)
ret void
}

define tailcc void @proc_clo$ae51831(%struct.ScmObj* %env$ae51831,%struct.ScmObj* %current_45args57092) {
%stackaddr$env-ref58575 = alloca %struct.ScmObj*, align 8
%k51176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51831, i64 0)
store %struct.ScmObj* %k51176, %struct.ScmObj** %stackaddr$env-ref58575
%stackaddr$env-ref58576 = alloca %struct.ScmObj*, align 8
%_37foldl150823 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51831, i64 1)
store %struct.ScmObj* %_37foldl150823, %struct.ScmObj** %stackaddr$env-ref58576
%stackaddr$env-ref58577 = alloca %struct.ScmObj*, align 8
%lst50862 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51831, i64 2)
store %struct.ScmObj* %lst50862, %struct.ScmObj** %stackaddr$env-ref58577
%stackaddr$prim58578 = alloca %struct.ScmObj*, align 8
%_95k51177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57092)
store volatile %struct.ScmObj* %_95k51177, %struct.ScmObj** %stackaddr$prim58578, align 8
%stackaddr$prim58579 = alloca %struct.ScmObj*, align 8
%current_45args57093 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57092)
store volatile %struct.ScmObj* %current_45args57093, %struct.ScmObj** %stackaddr$prim58579, align 8
%stackaddr$prim58580 = alloca %struct.ScmObj*, align 8
%anf_45bind50968 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57093)
store volatile %struct.ScmObj* %anf_45bind50968, %struct.ScmObj** %stackaddr$prim58580, align 8
%ae51852 = call %struct.ScmObj* @const_init_null()
%argslist57095$_37foldl1508230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58581 = alloca %struct.ScmObj*, align 8
%argslist57095$_37foldl1508231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst50862, %struct.ScmObj* %argslist57095$_37foldl1508230)
store volatile %struct.ScmObj* %argslist57095$_37foldl1508231, %struct.ScmObj** %stackaddr$prim58581, align 8
%stackaddr$prim58582 = alloca %struct.ScmObj*, align 8
%argslist57095$_37foldl1508232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51852, %struct.ScmObj* %argslist57095$_37foldl1508231)
store volatile %struct.ScmObj* %argslist57095$_37foldl1508232, %struct.ScmObj** %stackaddr$prim58582, align 8
%stackaddr$prim58583 = alloca %struct.ScmObj*, align 8
%argslist57095$_37foldl1508233 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50968, %struct.ScmObj* %argslist57095$_37foldl1508232)
store volatile %struct.ScmObj* %argslist57095$_37foldl1508233, %struct.ScmObj** %stackaddr$prim58583, align 8
%stackaddr$prim58584 = alloca %struct.ScmObj*, align 8
%argslist57095$_37foldl1508234 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51176, %struct.ScmObj* %argslist57095$_37foldl1508233)
store volatile %struct.ScmObj* %argslist57095$_37foldl1508234, %struct.ScmObj** %stackaddr$prim58584, align 8
%clofunc58585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl150823)
musttail call tailcc void %clofunc58585(%struct.ScmObj* %_37foldl150823, %struct.ScmObj* %argslist57095$_37foldl1508234)
ret void
}

define tailcc void @proc_clo$ae51833(%struct.ScmObj* %env$ae51833,%struct.ScmObj* %current_45args57096) {
%stackaddr$prim58586 = alloca %struct.ScmObj*, align 8
%k51178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57096)
store volatile %struct.ScmObj* %k51178, %struct.ScmObj** %stackaddr$prim58586, align 8
%stackaddr$prim58587 = alloca %struct.ScmObj*, align 8
%current_45args57097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57096)
store volatile %struct.ScmObj* %current_45args57097, %struct.ScmObj** %stackaddr$prim58587, align 8
%stackaddr$prim58588 = alloca %struct.ScmObj*, align 8
%x50864 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57097)
store volatile %struct.ScmObj* %x50864, %struct.ScmObj** %stackaddr$prim58588, align 8
%stackaddr$prim58589 = alloca %struct.ScmObj*, align 8
%current_45args57098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57097)
store volatile %struct.ScmObj* %current_45args57098, %struct.ScmObj** %stackaddr$prim58589, align 8
%stackaddr$prim58590 = alloca %struct.ScmObj*, align 8
%y50863 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57098)
store volatile %struct.ScmObj* %y50863, %struct.ScmObj** %stackaddr$prim58590, align 8
%ae51835 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57100$k511780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%argslist57100$k511781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x50864, %struct.ScmObj* %argslist57100$k511780)
store volatile %struct.ScmObj* %argslist57100$k511781, %struct.ScmObj** %stackaddr$prim58591, align 8
%stackaddr$prim58592 = alloca %struct.ScmObj*, align 8
%argslist57100$k511782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51835, %struct.ScmObj* %argslist57100$k511781)
store volatile %struct.ScmObj* %argslist57100$k511782, %struct.ScmObj** %stackaddr$prim58592, align 8
%clofunc58593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51178)
musttail call tailcc void %clofunc58593(%struct.ScmObj* %k51178, %struct.ScmObj* %argslist57100$k511782)
ret void
}

define tailcc void @proc_clo$ae51751(%struct.ScmObj* %env$ae51751,%struct.ScmObj* %current_45args57104) {
%stackaddr$prim58594 = alloca %struct.ScmObj*, align 8
%k51179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57104)
store volatile %struct.ScmObj* %k51179, %struct.ScmObj** %stackaddr$prim58594, align 8
%stackaddr$prim58595 = alloca %struct.ScmObj*, align 8
%current_45args57105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57104)
store volatile %struct.ScmObj* %current_45args57105, %struct.ScmObj** %stackaddr$prim58595, align 8
%stackaddr$prim58596 = alloca %struct.ScmObj*, align 8
%_37foldl150824 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57105)
store volatile %struct.ScmObj* %_37foldl150824, %struct.ScmObj** %stackaddr$prim58596, align 8
%ae51753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58597 = alloca %struct.ScmObj*, align 8
%fptrToInt58598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51754 to i64
%ae51754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58598)
store volatile %struct.ScmObj* %ae51754, %struct.ScmObj** %stackaddr$makeclosure58597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51754, %struct.ScmObj* %_37foldl150824, i64 0)
%argslist57118$k511790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58599 = alloca %struct.ScmObj*, align 8
%argslist57118$k511791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51754, %struct.ScmObj* %argslist57118$k511790)
store volatile %struct.ScmObj* %argslist57118$k511791, %struct.ScmObj** %stackaddr$prim58599, align 8
%stackaddr$prim58600 = alloca %struct.ScmObj*, align 8
%argslist57118$k511792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51753, %struct.ScmObj* %argslist57118$k511791)
store volatile %struct.ScmObj* %argslist57118$k511792, %struct.ScmObj** %stackaddr$prim58600, align 8
%clofunc58601 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51179)
musttail call tailcc void %clofunc58601(%struct.ScmObj* %k51179, %struct.ScmObj* %argslist57118$k511792)
ret void
}

define tailcc void @proc_clo$ae51754(%struct.ScmObj* %env$ae51754,%struct.ScmObj* %current_45args57107) {
%stackaddr$env-ref58602 = alloca %struct.ScmObj*, align 8
%_37foldl150824 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51754, i64 0)
store %struct.ScmObj* %_37foldl150824, %struct.ScmObj** %stackaddr$env-ref58602
%stackaddr$prim58603 = alloca %struct.ScmObj*, align 8
%k51180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57107)
store volatile %struct.ScmObj* %k51180, %struct.ScmObj** %stackaddr$prim58603, align 8
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%current_45args57108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57107)
store volatile %struct.ScmObj* %current_45args57108, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$prim58605 = alloca %struct.ScmObj*, align 8
%f50827 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57108)
store volatile %struct.ScmObj* %f50827, %struct.ScmObj** %stackaddr$prim58605, align 8
%stackaddr$prim58606 = alloca %struct.ScmObj*, align 8
%current_45args57109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57108)
store volatile %struct.ScmObj* %current_45args57109, %struct.ScmObj** %stackaddr$prim58606, align 8
%stackaddr$prim58607 = alloca %struct.ScmObj*, align 8
%acc50826 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57109)
store volatile %struct.ScmObj* %acc50826, %struct.ScmObj** %stackaddr$prim58607, align 8
%stackaddr$prim58608 = alloca %struct.ScmObj*, align 8
%current_45args57110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57109)
store volatile %struct.ScmObj* %current_45args57110, %struct.ScmObj** %stackaddr$prim58608, align 8
%stackaddr$prim58609 = alloca %struct.ScmObj*, align 8
%lst50825 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57110)
store volatile %struct.ScmObj* %lst50825, %struct.ScmObj** %stackaddr$prim58609, align 8
%stackaddr$prim58610 = alloca %struct.ScmObj*, align 8
%anf_45bind50963 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50825)
store volatile %struct.ScmObj* %anf_45bind50963, %struct.ScmObj** %stackaddr$prim58610, align 8
%truthy$cmp58611 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50963)
%cmp$cmp58611 = icmp eq i64 %truthy$cmp58611, 1
br i1 %cmp$cmp58611, label %truebranch$cmp58611, label %falsebranch$cmp58611
truebranch$cmp58611:
%ae51758 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57112$k511800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58612 = alloca %struct.ScmObj*, align 8
%argslist57112$k511801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50826, %struct.ScmObj* %argslist57112$k511800)
store volatile %struct.ScmObj* %argslist57112$k511801, %struct.ScmObj** %stackaddr$prim58612, align 8
%stackaddr$prim58613 = alloca %struct.ScmObj*, align 8
%argslist57112$k511802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51758, %struct.ScmObj* %argslist57112$k511801)
store volatile %struct.ScmObj* %argslist57112$k511802, %struct.ScmObj** %stackaddr$prim58613, align 8
%clofunc58614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51180)
musttail call tailcc void %clofunc58614(%struct.ScmObj* %k51180, %struct.ScmObj* %argslist57112$k511802)
ret void
falsebranch$cmp58611:
%stackaddr$prim58615 = alloca %struct.ScmObj*, align 8
%anf_45bind50964 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst50825)
store volatile %struct.ScmObj* %anf_45bind50964, %struct.ScmObj** %stackaddr$prim58615, align 8
%stackaddr$makeclosure58616 = alloca %struct.ScmObj*, align 8
%fptrToInt58617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51765 to i64
%ae51765 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58617)
store volatile %struct.ScmObj* %ae51765, %struct.ScmObj** %stackaddr$makeclosure58616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %k51180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %f50827, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %lst50825, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51765, %struct.ScmObj* %_37foldl150824, i64 3)
%argslist57117$f508270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58618 = alloca %struct.ScmObj*, align 8
%argslist57117$f508271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50826, %struct.ScmObj* %argslist57117$f508270)
store volatile %struct.ScmObj* %argslist57117$f508271, %struct.ScmObj** %stackaddr$prim58618, align 8
%stackaddr$prim58619 = alloca %struct.ScmObj*, align 8
%argslist57117$f508272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50964, %struct.ScmObj* %argslist57117$f508271)
store volatile %struct.ScmObj* %argslist57117$f508272, %struct.ScmObj** %stackaddr$prim58619, align 8
%stackaddr$prim58620 = alloca %struct.ScmObj*, align 8
%argslist57117$f508273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51765, %struct.ScmObj* %argslist57117$f508272)
store volatile %struct.ScmObj* %argslist57117$f508273, %struct.ScmObj** %stackaddr$prim58620, align 8
%clofunc58621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50827)
musttail call tailcc void %clofunc58621(%struct.ScmObj* %f50827, %struct.ScmObj* %argslist57117$f508273)
ret void
}

define tailcc void @proc_clo$ae51765(%struct.ScmObj* %env$ae51765,%struct.ScmObj* %current_45args57113) {
%stackaddr$env-ref58622 = alloca %struct.ScmObj*, align 8
%k51180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 0)
store %struct.ScmObj* %k51180, %struct.ScmObj** %stackaddr$env-ref58622
%stackaddr$env-ref58623 = alloca %struct.ScmObj*, align 8
%f50827 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 1)
store %struct.ScmObj* %f50827, %struct.ScmObj** %stackaddr$env-ref58623
%stackaddr$env-ref58624 = alloca %struct.ScmObj*, align 8
%lst50825 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 2)
store %struct.ScmObj* %lst50825, %struct.ScmObj** %stackaddr$env-ref58624
%stackaddr$env-ref58625 = alloca %struct.ScmObj*, align 8
%_37foldl150824 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51765, i64 3)
store %struct.ScmObj* %_37foldl150824, %struct.ScmObj** %stackaddr$env-ref58625
%stackaddr$prim58626 = alloca %struct.ScmObj*, align 8
%_95k51181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57113)
store volatile %struct.ScmObj* %_95k51181, %struct.ScmObj** %stackaddr$prim58626, align 8
%stackaddr$prim58627 = alloca %struct.ScmObj*, align 8
%current_45args57114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57113)
store volatile %struct.ScmObj* %current_45args57114, %struct.ScmObj** %stackaddr$prim58627, align 8
%stackaddr$prim58628 = alloca %struct.ScmObj*, align 8
%anf_45bind50965 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57114)
store volatile %struct.ScmObj* %anf_45bind50965, %struct.ScmObj** %stackaddr$prim58628, align 8
%stackaddr$prim58629 = alloca %struct.ScmObj*, align 8
%anf_45bind50966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst50825)
store volatile %struct.ScmObj* %anf_45bind50966, %struct.ScmObj** %stackaddr$prim58629, align 8
%argslist57116$_37foldl1508240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58630 = alloca %struct.ScmObj*, align 8
%argslist57116$_37foldl1508241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50966, %struct.ScmObj* %argslist57116$_37foldl1508240)
store volatile %struct.ScmObj* %argslist57116$_37foldl1508241, %struct.ScmObj** %stackaddr$prim58630, align 8
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%argslist57116$_37foldl1508242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50965, %struct.ScmObj* %argslist57116$_37foldl1508241)
store volatile %struct.ScmObj* %argslist57116$_37foldl1508242, %struct.ScmObj** %stackaddr$prim58631, align 8
%stackaddr$prim58632 = alloca %struct.ScmObj*, align 8
%argslist57116$_37foldl1508243 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50827, %struct.ScmObj* %argslist57116$_37foldl1508242)
store volatile %struct.ScmObj* %argslist57116$_37foldl1508243, %struct.ScmObj** %stackaddr$prim58632, align 8
%stackaddr$prim58633 = alloca %struct.ScmObj*, align 8
%argslist57116$_37foldl1508244 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51180, %struct.ScmObj* %argslist57116$_37foldl1508243)
store volatile %struct.ScmObj* %argslist57116$_37foldl1508244, %struct.ScmObj** %stackaddr$prim58633, align 8
%clofunc58634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl150824)
musttail call tailcc void %clofunc58634(%struct.ScmObj* %_37foldl150824, %struct.ScmObj* %argslist57116$_37foldl1508244)
ret void
}

define tailcc void @proc_clo$ae51668(%struct.ScmObj* %env$ae51668,%struct.ScmObj* %current_45args57121) {
%stackaddr$prim58635 = alloca %struct.ScmObj*, align 8
%k51182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57121)
store volatile %struct.ScmObj* %k51182, %struct.ScmObj** %stackaddr$prim58635, align 8
%stackaddr$prim58636 = alloca %struct.ScmObj*, align 8
%current_45args57122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57121)
store volatile %struct.ScmObj* %current_45args57122, %struct.ScmObj** %stackaddr$prim58636, align 8
%stackaddr$prim58637 = alloca %struct.ScmObj*, align 8
%_37length50829 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57122)
store volatile %struct.ScmObj* %_37length50829, %struct.ScmObj** %stackaddr$prim58637, align 8
%ae51670 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58638 = alloca %struct.ScmObj*, align 8
%fptrToInt58639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51671 to i64
%ae51671 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58639)
store volatile %struct.ScmObj* %ae51671, %struct.ScmObj** %stackaddr$makeclosure58638, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %_37length50829, i64 0)
%argslist57133$k511820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58640 = alloca %struct.ScmObj*, align 8
%argslist57133$k511821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51671, %struct.ScmObj* %argslist57133$k511820)
store volatile %struct.ScmObj* %argslist57133$k511821, %struct.ScmObj** %stackaddr$prim58640, align 8
%stackaddr$prim58641 = alloca %struct.ScmObj*, align 8
%argslist57133$k511822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51670, %struct.ScmObj* %argslist57133$k511821)
store volatile %struct.ScmObj* %argslist57133$k511822, %struct.ScmObj** %stackaddr$prim58641, align 8
%clofunc58642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51182)
musttail call tailcc void %clofunc58642(%struct.ScmObj* %k51182, %struct.ScmObj* %argslist57133$k511822)
ret void
}

define tailcc void @proc_clo$ae51671(%struct.ScmObj* %env$ae51671,%struct.ScmObj* %current_45args57124) {
%stackaddr$env-ref58643 = alloca %struct.ScmObj*, align 8
%_37length50829 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 0)
store %struct.ScmObj* %_37length50829, %struct.ScmObj** %stackaddr$env-ref58643
%stackaddr$prim58644 = alloca %struct.ScmObj*, align 8
%k51183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57124)
store volatile %struct.ScmObj* %k51183, %struct.ScmObj** %stackaddr$prim58644, align 8
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%current_45args57125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57124)
store volatile %struct.ScmObj* %current_45args57125, %struct.ScmObj** %stackaddr$prim58645, align 8
%stackaddr$prim58646 = alloca %struct.ScmObj*, align 8
%lst50830 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57125)
store volatile %struct.ScmObj* %lst50830, %struct.ScmObj** %stackaddr$prim58646, align 8
%stackaddr$prim58647 = alloca %struct.ScmObj*, align 8
%anf_45bind50959 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50830)
store volatile %struct.ScmObj* %anf_45bind50959, %struct.ScmObj** %stackaddr$prim58647, align 8
%truthy$cmp58648 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50959)
%cmp$cmp58648 = icmp eq i64 %truthy$cmp58648, 1
br i1 %cmp$cmp58648, label %truebranch$cmp58648, label %falsebranch$cmp58648
truebranch$cmp58648:
%ae51675 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51676 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57127$k511830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58649 = alloca %struct.ScmObj*, align 8
%argslist57127$k511831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51676, %struct.ScmObj* %argslist57127$k511830)
store volatile %struct.ScmObj* %argslist57127$k511831, %struct.ScmObj** %stackaddr$prim58649, align 8
%stackaddr$prim58650 = alloca %struct.ScmObj*, align 8
%argslist57127$k511832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51675, %struct.ScmObj* %argslist57127$k511831)
store volatile %struct.ScmObj* %argslist57127$k511832, %struct.ScmObj** %stackaddr$prim58650, align 8
%clofunc58651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51183)
musttail call tailcc void %clofunc58651(%struct.ScmObj* %k51183, %struct.ScmObj* %argslist57127$k511832)
ret void
falsebranch$cmp58648:
%stackaddr$prim58652 = alloca %struct.ScmObj*, align 8
%anf_45bind50960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst50830)
store volatile %struct.ScmObj* %anf_45bind50960, %struct.ScmObj** %stackaddr$prim58652, align 8
%stackaddr$makeclosure58653 = alloca %struct.ScmObj*, align 8
%fptrToInt58654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51685 to i64
%ae51685 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58654)
store volatile %struct.ScmObj* %ae51685, %struct.ScmObj** %stackaddr$makeclosure58653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51685, %struct.ScmObj* %k51183, i64 0)
%argslist57132$_37length508290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58655 = alloca %struct.ScmObj*, align 8
%argslist57132$_37length508291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50960, %struct.ScmObj* %argslist57132$_37length508290)
store volatile %struct.ScmObj* %argslist57132$_37length508291, %struct.ScmObj** %stackaddr$prim58655, align 8
%stackaddr$prim58656 = alloca %struct.ScmObj*, align 8
%argslist57132$_37length508292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51685, %struct.ScmObj* %argslist57132$_37length508291)
store volatile %struct.ScmObj* %argslist57132$_37length508292, %struct.ScmObj** %stackaddr$prim58656, align 8
%clofunc58657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length50829)
musttail call tailcc void %clofunc58657(%struct.ScmObj* %_37length50829, %struct.ScmObj* %argslist57132$_37length508292)
ret void
}

define tailcc void @proc_clo$ae51685(%struct.ScmObj* %env$ae51685,%struct.ScmObj* %current_45args57128) {
%stackaddr$env-ref58658 = alloca %struct.ScmObj*, align 8
%k51183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51685, i64 0)
store %struct.ScmObj* %k51183, %struct.ScmObj** %stackaddr$env-ref58658
%stackaddr$prim58659 = alloca %struct.ScmObj*, align 8
%_95k51184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57128)
store volatile %struct.ScmObj* %_95k51184, %struct.ScmObj** %stackaddr$prim58659, align 8
%stackaddr$prim58660 = alloca %struct.ScmObj*, align 8
%current_45args57129 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57128)
store volatile %struct.ScmObj* %current_45args57129, %struct.ScmObj** %stackaddr$prim58660, align 8
%stackaddr$prim58661 = alloca %struct.ScmObj*, align 8
%anf_45bind50961 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57129)
store volatile %struct.ScmObj* %anf_45bind50961, %struct.ScmObj** %stackaddr$prim58661, align 8
%ae51687 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%cpsprim51185 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae51687, %struct.ScmObj* %anf_45bind50961)
store volatile %struct.ScmObj* %cpsprim51185, %struct.ScmObj** %stackaddr$prim58662, align 8
%ae51690 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57131$k511830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58663 = alloca %struct.ScmObj*, align 8
%argslist57131$k511831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51185, %struct.ScmObj* %argslist57131$k511830)
store volatile %struct.ScmObj* %argslist57131$k511831, %struct.ScmObj** %stackaddr$prim58663, align 8
%stackaddr$prim58664 = alloca %struct.ScmObj*, align 8
%argslist57131$k511832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51690, %struct.ScmObj* %argslist57131$k511831)
store volatile %struct.ScmObj* %argslist57131$k511832, %struct.ScmObj** %stackaddr$prim58664, align 8
%clofunc58665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51183)
musttail call tailcc void %clofunc58665(%struct.ScmObj* %k51183, %struct.ScmObj* %argslist57131$k511832)
ret void
}

define tailcc void @proc_clo$ae51518(%struct.ScmObj* %env$ae51518,%struct.ScmObj* %current_45args57136) {
%stackaddr$prim58666 = alloca %struct.ScmObj*, align 8
%k51186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57136)
store volatile %struct.ScmObj* %k51186, %struct.ScmObj** %stackaddr$prim58666, align 8
%stackaddr$prim58667 = alloca %struct.ScmObj*, align 8
%current_45args57137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57136)
store volatile %struct.ScmObj* %current_45args57137, %struct.ScmObj** %stackaddr$prim58667, align 8
%stackaddr$prim58668 = alloca %struct.ScmObj*, align 8
%_37take50832 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57137)
store volatile %struct.ScmObj* %_37take50832, %struct.ScmObj** %stackaddr$prim58668, align 8
%ae51520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58669 = alloca %struct.ScmObj*, align 8
%fptrToInt58670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51521 to i64
%ae51521 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58670)
store volatile %struct.ScmObj* %ae51521, %struct.ScmObj** %stackaddr$makeclosure58669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51521, %struct.ScmObj* %_37take50832, i64 0)
%argslist57150$k511860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58671 = alloca %struct.ScmObj*, align 8
%argslist57150$k511861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51521, %struct.ScmObj* %argslist57150$k511860)
store volatile %struct.ScmObj* %argslist57150$k511861, %struct.ScmObj** %stackaddr$prim58671, align 8
%stackaddr$prim58672 = alloca %struct.ScmObj*, align 8
%argslist57150$k511862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51520, %struct.ScmObj* %argslist57150$k511861)
store volatile %struct.ScmObj* %argslist57150$k511862, %struct.ScmObj** %stackaddr$prim58672, align 8
%clofunc58673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51186)
musttail call tailcc void %clofunc58673(%struct.ScmObj* %k51186, %struct.ScmObj* %argslist57150$k511862)
ret void
}

define tailcc void @proc_clo$ae51521(%struct.ScmObj* %env$ae51521,%struct.ScmObj* %current_45args57139) {
%stackaddr$env-ref58674 = alloca %struct.ScmObj*, align 8
%_37take50832 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51521, i64 0)
store %struct.ScmObj* %_37take50832, %struct.ScmObj** %stackaddr$env-ref58674
%stackaddr$prim58675 = alloca %struct.ScmObj*, align 8
%k51187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57139)
store volatile %struct.ScmObj* %k51187, %struct.ScmObj** %stackaddr$prim58675, align 8
%stackaddr$prim58676 = alloca %struct.ScmObj*, align 8
%current_45args57140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57139)
store volatile %struct.ScmObj* %current_45args57140, %struct.ScmObj** %stackaddr$prim58676, align 8
%stackaddr$prim58677 = alloca %struct.ScmObj*, align 8
%lst50834 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57140)
store volatile %struct.ScmObj* %lst50834, %struct.ScmObj** %stackaddr$prim58677, align 8
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%current_45args57141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57140)
store volatile %struct.ScmObj* %current_45args57141, %struct.ScmObj** %stackaddr$prim58678, align 8
%stackaddr$prim58679 = alloca %struct.ScmObj*, align 8
%n50833 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57141)
store volatile %struct.ScmObj* %n50833, %struct.ScmObj** %stackaddr$prim58679, align 8
%ae51523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58680 = alloca %struct.ScmObj*, align 8
%anf_45bind50952 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n50833, %struct.ScmObj* %ae51523)
store volatile %struct.ScmObj* %anf_45bind50952, %struct.ScmObj** %stackaddr$prim58680, align 8
%truthy$cmp58681 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50952)
%cmp$cmp58681 = icmp eq i64 %truthy$cmp58681, 1
br i1 %cmp$cmp58681, label %truebranch$cmp58681, label %falsebranch$cmp58681
truebranch$cmp58681:
%ae51526 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51527 = call %struct.ScmObj* @const_init_null()
%argslist57143$k511870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58682 = alloca %struct.ScmObj*, align 8
%argslist57143$k511871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51527, %struct.ScmObj* %argslist57143$k511870)
store volatile %struct.ScmObj* %argslist57143$k511871, %struct.ScmObj** %stackaddr$prim58682, align 8
%stackaddr$prim58683 = alloca %struct.ScmObj*, align 8
%argslist57143$k511872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51526, %struct.ScmObj* %argslist57143$k511871)
store volatile %struct.ScmObj* %argslist57143$k511872, %struct.ScmObj** %stackaddr$prim58683, align 8
%clofunc58684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51187)
musttail call tailcc void %clofunc58684(%struct.ScmObj* %k51187, %struct.ScmObj* %argslist57143$k511872)
ret void
falsebranch$cmp58681:
%stackaddr$prim58685 = alloca %struct.ScmObj*, align 8
%anf_45bind50953 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50834)
store volatile %struct.ScmObj* %anf_45bind50953, %struct.ScmObj** %stackaddr$prim58685, align 8
%truthy$cmp58686 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50953)
%cmp$cmp58686 = icmp eq i64 %truthy$cmp58686, 1
br i1 %cmp$cmp58686, label %truebranch$cmp58686, label %falsebranch$cmp58686
truebranch$cmp58686:
%ae51537 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51538 = call %struct.ScmObj* @const_init_null()
%argslist57144$k511870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58687 = alloca %struct.ScmObj*, align 8
%argslist57144$k511871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51538, %struct.ScmObj* %argslist57144$k511870)
store volatile %struct.ScmObj* %argslist57144$k511871, %struct.ScmObj** %stackaddr$prim58687, align 8
%stackaddr$prim58688 = alloca %struct.ScmObj*, align 8
%argslist57144$k511872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51537, %struct.ScmObj* %argslist57144$k511871)
store volatile %struct.ScmObj* %argslist57144$k511872, %struct.ScmObj** %stackaddr$prim58688, align 8
%clofunc58689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51187)
musttail call tailcc void %clofunc58689(%struct.ScmObj* %k51187, %struct.ScmObj* %argslist57144$k511872)
ret void
falsebranch$cmp58686:
%stackaddr$prim58690 = alloca %struct.ScmObj*, align 8
%anf_45bind50954 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst50834)
store volatile %struct.ScmObj* %anf_45bind50954, %struct.ScmObj** %stackaddr$prim58690, align 8
%stackaddr$prim58691 = alloca %struct.ScmObj*, align 8
%anf_45bind50955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst50834)
store volatile %struct.ScmObj* %anf_45bind50955, %struct.ScmObj** %stackaddr$prim58691, align 8
%ae51548 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58692 = alloca %struct.ScmObj*, align 8
%anf_45bind50956 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n50833, %struct.ScmObj* %ae51548)
store volatile %struct.ScmObj* %anf_45bind50956, %struct.ScmObj** %stackaddr$prim58692, align 8
%stackaddr$makeclosure58693 = alloca %struct.ScmObj*, align 8
%fptrToInt58694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51550 to i64
%ae51550 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58694)
store volatile %struct.ScmObj* %ae51550, %struct.ScmObj** %stackaddr$makeclosure58693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51550, %struct.ScmObj* %anf_45bind50954, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51550, %struct.ScmObj* %k51187, i64 1)
%argslist57149$_37take508320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58695 = alloca %struct.ScmObj*, align 8
%argslist57149$_37take508321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50956, %struct.ScmObj* %argslist57149$_37take508320)
store volatile %struct.ScmObj* %argslist57149$_37take508321, %struct.ScmObj** %stackaddr$prim58695, align 8
%stackaddr$prim58696 = alloca %struct.ScmObj*, align 8
%argslist57149$_37take508322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50955, %struct.ScmObj* %argslist57149$_37take508321)
store volatile %struct.ScmObj* %argslist57149$_37take508322, %struct.ScmObj** %stackaddr$prim58696, align 8
%stackaddr$prim58697 = alloca %struct.ScmObj*, align 8
%argslist57149$_37take508323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51550, %struct.ScmObj* %argslist57149$_37take508322)
store volatile %struct.ScmObj* %argslist57149$_37take508323, %struct.ScmObj** %stackaddr$prim58697, align 8
%clofunc58698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take50832)
musttail call tailcc void %clofunc58698(%struct.ScmObj* %_37take50832, %struct.ScmObj* %argslist57149$_37take508323)
ret void
}

define tailcc void @proc_clo$ae51550(%struct.ScmObj* %env$ae51550,%struct.ScmObj* %current_45args57145) {
%stackaddr$env-ref58699 = alloca %struct.ScmObj*, align 8
%anf_45bind50954 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51550, i64 0)
store %struct.ScmObj* %anf_45bind50954, %struct.ScmObj** %stackaddr$env-ref58699
%stackaddr$env-ref58700 = alloca %struct.ScmObj*, align 8
%k51187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51550, i64 1)
store %struct.ScmObj* %k51187, %struct.ScmObj** %stackaddr$env-ref58700
%stackaddr$prim58701 = alloca %struct.ScmObj*, align 8
%_95k51188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57145)
store volatile %struct.ScmObj* %_95k51188, %struct.ScmObj** %stackaddr$prim58701, align 8
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%current_45args57146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57145)
store volatile %struct.ScmObj* %current_45args57146, %struct.ScmObj** %stackaddr$prim58702, align 8
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%anf_45bind50957 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57146)
store volatile %struct.ScmObj* %anf_45bind50957, %struct.ScmObj** %stackaddr$prim58703, align 8
%stackaddr$prim58704 = alloca %struct.ScmObj*, align 8
%cpsprim51189 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50954, %struct.ScmObj* %anf_45bind50957)
store volatile %struct.ScmObj* %cpsprim51189, %struct.ScmObj** %stackaddr$prim58704, align 8
%ae51556 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57148$k511870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58705 = alloca %struct.ScmObj*, align 8
%argslist57148$k511871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51189, %struct.ScmObj* %argslist57148$k511870)
store volatile %struct.ScmObj* %argslist57148$k511871, %struct.ScmObj** %stackaddr$prim58705, align 8
%stackaddr$prim58706 = alloca %struct.ScmObj*, align 8
%argslist57148$k511872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51556, %struct.ScmObj* %argslist57148$k511871)
store volatile %struct.ScmObj* %argslist57148$k511872, %struct.ScmObj** %stackaddr$prim58706, align 8
%clofunc58707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51187)
musttail call tailcc void %clofunc58707(%struct.ScmObj* %k51187, %struct.ScmObj* %argslist57148$k511872)
ret void
}

define tailcc void @proc_clo$ae51421(%struct.ScmObj* %env$ae51421,%struct.ScmObj* %current_45args57153) {
%stackaddr$prim58708 = alloca %struct.ScmObj*, align 8
%k51190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57153)
store volatile %struct.ScmObj* %k51190, %struct.ScmObj** %stackaddr$prim58708, align 8
%stackaddr$prim58709 = alloca %struct.ScmObj*, align 8
%current_45args57154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57153)
store volatile %struct.ScmObj* %current_45args57154, %struct.ScmObj** %stackaddr$prim58709, align 8
%stackaddr$prim58710 = alloca %struct.ScmObj*, align 8
%_37map50836 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57154)
store volatile %struct.ScmObj* %_37map50836, %struct.ScmObj** %stackaddr$prim58710, align 8
%ae51423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58711 = alloca %struct.ScmObj*, align 8
%fptrToInt58712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51424 to i64
%ae51424 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58712)
store volatile %struct.ScmObj* %ae51424, %struct.ScmObj** %stackaddr$makeclosure58711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51424, %struct.ScmObj* %_37map50836, i64 0)
%argslist57170$k511900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58713 = alloca %struct.ScmObj*, align 8
%argslist57170$k511901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51424, %struct.ScmObj* %argslist57170$k511900)
store volatile %struct.ScmObj* %argslist57170$k511901, %struct.ScmObj** %stackaddr$prim58713, align 8
%stackaddr$prim58714 = alloca %struct.ScmObj*, align 8
%argslist57170$k511902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51423, %struct.ScmObj* %argslist57170$k511901)
store volatile %struct.ScmObj* %argslist57170$k511902, %struct.ScmObj** %stackaddr$prim58714, align 8
%clofunc58715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51190)
musttail call tailcc void %clofunc58715(%struct.ScmObj* %k51190, %struct.ScmObj* %argslist57170$k511902)
ret void
}

define tailcc void @proc_clo$ae51424(%struct.ScmObj* %env$ae51424,%struct.ScmObj* %current_45args57156) {
%stackaddr$env-ref58716 = alloca %struct.ScmObj*, align 8
%_37map50836 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51424, i64 0)
store %struct.ScmObj* %_37map50836, %struct.ScmObj** %stackaddr$env-ref58716
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%k51191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57156)
store volatile %struct.ScmObj* %k51191, %struct.ScmObj** %stackaddr$prim58717, align 8
%stackaddr$prim58718 = alloca %struct.ScmObj*, align 8
%current_45args57157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57156)
store volatile %struct.ScmObj* %current_45args57157, %struct.ScmObj** %stackaddr$prim58718, align 8
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%f50838 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57157)
store volatile %struct.ScmObj* %f50838, %struct.ScmObj** %stackaddr$prim58719, align 8
%stackaddr$prim58720 = alloca %struct.ScmObj*, align 8
%current_45args57158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57157)
store volatile %struct.ScmObj* %current_45args57158, %struct.ScmObj** %stackaddr$prim58720, align 8
%stackaddr$prim58721 = alloca %struct.ScmObj*, align 8
%lst50837 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57158)
store volatile %struct.ScmObj* %lst50837, %struct.ScmObj** %stackaddr$prim58721, align 8
%stackaddr$prim58722 = alloca %struct.ScmObj*, align 8
%anf_45bind50946 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50837)
store volatile %struct.ScmObj* %anf_45bind50946, %struct.ScmObj** %stackaddr$prim58722, align 8
%truthy$cmp58723 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50946)
%cmp$cmp58723 = icmp eq i64 %truthy$cmp58723, 1
br i1 %cmp$cmp58723, label %truebranch$cmp58723, label %falsebranch$cmp58723
truebranch$cmp58723:
%ae51428 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51429 = call %struct.ScmObj* @const_init_null()
%argslist57160$k511910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58724 = alloca %struct.ScmObj*, align 8
%argslist57160$k511911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51429, %struct.ScmObj* %argslist57160$k511910)
store volatile %struct.ScmObj* %argslist57160$k511911, %struct.ScmObj** %stackaddr$prim58724, align 8
%stackaddr$prim58725 = alloca %struct.ScmObj*, align 8
%argslist57160$k511912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51428, %struct.ScmObj* %argslist57160$k511911)
store volatile %struct.ScmObj* %argslist57160$k511912, %struct.ScmObj** %stackaddr$prim58725, align 8
%clofunc58726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51191)
musttail call tailcc void %clofunc58726(%struct.ScmObj* %k51191, %struct.ScmObj* %argslist57160$k511912)
ret void
falsebranch$cmp58723:
%stackaddr$prim58727 = alloca %struct.ScmObj*, align 8
%anf_45bind50947 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst50837)
store volatile %struct.ScmObj* %anf_45bind50947, %struct.ScmObj** %stackaddr$prim58727, align 8
%stackaddr$makeclosure58728 = alloca %struct.ScmObj*, align 8
%fptrToInt58729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51438 to i64
%ae51438 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58729)
store volatile %struct.ScmObj* %ae51438, %struct.ScmObj** %stackaddr$makeclosure58728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %k51191, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %f50838, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %lst50837, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51438, %struct.ScmObj* %_37map50836, i64 3)
%argslist57169$f508380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58730 = alloca %struct.ScmObj*, align 8
%argslist57169$f508381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50947, %struct.ScmObj* %argslist57169$f508380)
store volatile %struct.ScmObj* %argslist57169$f508381, %struct.ScmObj** %stackaddr$prim58730, align 8
%stackaddr$prim58731 = alloca %struct.ScmObj*, align 8
%argslist57169$f508382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51438, %struct.ScmObj* %argslist57169$f508381)
store volatile %struct.ScmObj* %argslist57169$f508382, %struct.ScmObj** %stackaddr$prim58731, align 8
%clofunc58732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50838)
musttail call tailcc void %clofunc58732(%struct.ScmObj* %f50838, %struct.ScmObj* %argslist57169$f508382)
ret void
}

define tailcc void @proc_clo$ae51438(%struct.ScmObj* %env$ae51438,%struct.ScmObj* %current_45args57161) {
%stackaddr$env-ref58733 = alloca %struct.ScmObj*, align 8
%k51191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 0)
store %struct.ScmObj* %k51191, %struct.ScmObj** %stackaddr$env-ref58733
%stackaddr$env-ref58734 = alloca %struct.ScmObj*, align 8
%f50838 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 1)
store %struct.ScmObj* %f50838, %struct.ScmObj** %stackaddr$env-ref58734
%stackaddr$env-ref58735 = alloca %struct.ScmObj*, align 8
%lst50837 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 2)
store %struct.ScmObj* %lst50837, %struct.ScmObj** %stackaddr$env-ref58735
%stackaddr$env-ref58736 = alloca %struct.ScmObj*, align 8
%_37map50836 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51438, i64 3)
store %struct.ScmObj* %_37map50836, %struct.ScmObj** %stackaddr$env-ref58736
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%_95k51192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57161)
store volatile %struct.ScmObj* %_95k51192, %struct.ScmObj** %stackaddr$prim58737, align 8
%stackaddr$prim58738 = alloca %struct.ScmObj*, align 8
%current_45args57162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57161)
store volatile %struct.ScmObj* %current_45args57162, %struct.ScmObj** %stackaddr$prim58738, align 8
%stackaddr$prim58739 = alloca %struct.ScmObj*, align 8
%anf_45bind50948 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57162)
store volatile %struct.ScmObj* %anf_45bind50948, %struct.ScmObj** %stackaddr$prim58739, align 8
%stackaddr$prim58740 = alloca %struct.ScmObj*, align 8
%anf_45bind50949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst50837)
store volatile %struct.ScmObj* %anf_45bind50949, %struct.ScmObj** %stackaddr$prim58740, align 8
%stackaddr$makeclosure58741 = alloca %struct.ScmObj*, align 8
%fptrToInt58742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51442 to i64
%ae51442 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58742)
store volatile %struct.ScmObj* %ae51442, %struct.ScmObj** %stackaddr$makeclosure58741, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51442, %struct.ScmObj* %k51191, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51442, %struct.ScmObj* %anf_45bind50948, i64 1)
%argslist57168$_37map508360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%argslist57168$_37map508361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50949, %struct.ScmObj* %argslist57168$_37map508360)
store volatile %struct.ScmObj* %argslist57168$_37map508361, %struct.ScmObj** %stackaddr$prim58743, align 8
%stackaddr$prim58744 = alloca %struct.ScmObj*, align 8
%argslist57168$_37map508362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50838, %struct.ScmObj* %argslist57168$_37map508361)
store volatile %struct.ScmObj* %argslist57168$_37map508362, %struct.ScmObj** %stackaddr$prim58744, align 8
%stackaddr$prim58745 = alloca %struct.ScmObj*, align 8
%argslist57168$_37map508363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51442, %struct.ScmObj* %argslist57168$_37map508362)
store volatile %struct.ScmObj* %argslist57168$_37map508363, %struct.ScmObj** %stackaddr$prim58745, align 8
%clofunc58746 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map50836)
musttail call tailcc void %clofunc58746(%struct.ScmObj* %_37map50836, %struct.ScmObj* %argslist57168$_37map508363)
ret void
}

define tailcc void @proc_clo$ae51442(%struct.ScmObj* %env$ae51442,%struct.ScmObj* %current_45args57164) {
%stackaddr$env-ref58747 = alloca %struct.ScmObj*, align 8
%k51191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51442, i64 0)
store %struct.ScmObj* %k51191, %struct.ScmObj** %stackaddr$env-ref58747
%stackaddr$env-ref58748 = alloca %struct.ScmObj*, align 8
%anf_45bind50948 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51442, i64 1)
store %struct.ScmObj* %anf_45bind50948, %struct.ScmObj** %stackaddr$env-ref58748
%stackaddr$prim58749 = alloca %struct.ScmObj*, align 8
%_95k51193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57164)
store volatile %struct.ScmObj* %_95k51193, %struct.ScmObj** %stackaddr$prim58749, align 8
%stackaddr$prim58750 = alloca %struct.ScmObj*, align 8
%current_45args57165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57164)
store volatile %struct.ScmObj* %current_45args57165, %struct.ScmObj** %stackaddr$prim58750, align 8
%stackaddr$prim58751 = alloca %struct.ScmObj*, align 8
%anf_45bind50950 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57165)
store volatile %struct.ScmObj* %anf_45bind50950, %struct.ScmObj** %stackaddr$prim58751, align 8
%stackaddr$prim58752 = alloca %struct.ScmObj*, align 8
%cpsprim51194 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50948, %struct.ScmObj* %anf_45bind50950)
store volatile %struct.ScmObj* %cpsprim51194, %struct.ScmObj** %stackaddr$prim58752, align 8
%ae51448 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57167$k511910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%argslist57167$k511911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim51194, %struct.ScmObj* %argslist57167$k511910)
store volatile %struct.ScmObj* %argslist57167$k511911, %struct.ScmObj** %stackaddr$prim58753, align 8
%stackaddr$prim58754 = alloca %struct.ScmObj*, align 8
%argslist57167$k511912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51448, %struct.ScmObj* %argslist57167$k511911)
store volatile %struct.ScmObj* %argslist57167$k511912, %struct.ScmObj** %stackaddr$prim58754, align 8
%clofunc58755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51191)
musttail call tailcc void %clofunc58755(%struct.ScmObj* %k51191, %struct.ScmObj* %argslist57167$k511912)
ret void
}

define tailcc void @proc_clo$ae51341(%struct.ScmObj* %env$ae51341,%struct.ScmObj* %current_45args57173) {
%stackaddr$prim58756 = alloca %struct.ScmObj*, align 8
%k51195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57173)
store volatile %struct.ScmObj* %k51195, %struct.ScmObj** %stackaddr$prim58756, align 8
%stackaddr$prim58757 = alloca %struct.ScmObj*, align 8
%current_45args57174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57173)
store volatile %struct.ScmObj* %current_45args57174, %struct.ScmObj** %stackaddr$prim58757, align 8
%stackaddr$prim58758 = alloca %struct.ScmObj*, align 8
%_37foldr150840 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57174)
store volatile %struct.ScmObj* %_37foldr150840, %struct.ScmObj** %stackaddr$prim58758, align 8
%ae51343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58759 = alloca %struct.ScmObj*, align 8
%fptrToInt58760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51344 to i64
%ae51344 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58760)
store volatile %struct.ScmObj* %ae51344, %struct.ScmObj** %stackaddr$makeclosure58759, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51344, %struct.ScmObj* %_37foldr150840, i64 0)
%argslist57187$k511950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58761 = alloca %struct.ScmObj*, align 8
%argslist57187$k511951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51344, %struct.ScmObj* %argslist57187$k511950)
store volatile %struct.ScmObj* %argslist57187$k511951, %struct.ScmObj** %stackaddr$prim58761, align 8
%stackaddr$prim58762 = alloca %struct.ScmObj*, align 8
%argslist57187$k511952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51343, %struct.ScmObj* %argslist57187$k511951)
store volatile %struct.ScmObj* %argslist57187$k511952, %struct.ScmObj** %stackaddr$prim58762, align 8
%clofunc58763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51195)
musttail call tailcc void %clofunc58763(%struct.ScmObj* %k51195, %struct.ScmObj* %argslist57187$k511952)
ret void
}

define tailcc void @proc_clo$ae51344(%struct.ScmObj* %env$ae51344,%struct.ScmObj* %current_45args57176) {
%stackaddr$env-ref58764 = alloca %struct.ScmObj*, align 8
%_37foldr150840 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51344, i64 0)
store %struct.ScmObj* %_37foldr150840, %struct.ScmObj** %stackaddr$env-ref58764
%stackaddr$prim58765 = alloca %struct.ScmObj*, align 8
%k51196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57176)
store volatile %struct.ScmObj* %k51196, %struct.ScmObj** %stackaddr$prim58765, align 8
%stackaddr$prim58766 = alloca %struct.ScmObj*, align 8
%current_45args57177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57176)
store volatile %struct.ScmObj* %current_45args57177, %struct.ScmObj** %stackaddr$prim58766, align 8
%stackaddr$prim58767 = alloca %struct.ScmObj*, align 8
%f50843 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57177)
store volatile %struct.ScmObj* %f50843, %struct.ScmObj** %stackaddr$prim58767, align 8
%stackaddr$prim58768 = alloca %struct.ScmObj*, align 8
%current_45args57178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57177)
store volatile %struct.ScmObj* %current_45args57178, %struct.ScmObj** %stackaddr$prim58768, align 8
%stackaddr$prim58769 = alloca %struct.ScmObj*, align 8
%acc50842 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57178)
store volatile %struct.ScmObj* %acc50842, %struct.ScmObj** %stackaddr$prim58769, align 8
%stackaddr$prim58770 = alloca %struct.ScmObj*, align 8
%current_45args57179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57178)
store volatile %struct.ScmObj* %current_45args57179, %struct.ScmObj** %stackaddr$prim58770, align 8
%stackaddr$prim58771 = alloca %struct.ScmObj*, align 8
%lst50841 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57179)
store volatile %struct.ScmObj* %lst50841, %struct.ScmObj** %stackaddr$prim58771, align 8
%stackaddr$prim58772 = alloca %struct.ScmObj*, align 8
%anf_45bind50941 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst50841)
store volatile %struct.ScmObj* %anf_45bind50941, %struct.ScmObj** %stackaddr$prim58772, align 8
%truthy$cmp58773 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind50941)
%cmp$cmp58773 = icmp eq i64 %truthy$cmp58773, 1
br i1 %cmp$cmp58773, label %truebranch$cmp58773, label %falsebranch$cmp58773
truebranch$cmp58773:
%ae51348 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57181$k511960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58774 = alloca %struct.ScmObj*, align 8
%argslist57181$k511961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50842, %struct.ScmObj* %argslist57181$k511960)
store volatile %struct.ScmObj* %argslist57181$k511961, %struct.ScmObj** %stackaddr$prim58774, align 8
%stackaddr$prim58775 = alloca %struct.ScmObj*, align 8
%argslist57181$k511962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51348, %struct.ScmObj* %argslist57181$k511961)
store volatile %struct.ScmObj* %argslist57181$k511962, %struct.ScmObj** %stackaddr$prim58775, align 8
%clofunc58776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51196)
musttail call tailcc void %clofunc58776(%struct.ScmObj* %k51196, %struct.ScmObj* %argslist57181$k511962)
ret void
falsebranch$cmp58773:
%stackaddr$prim58777 = alloca %struct.ScmObj*, align 8
%anf_45bind50942 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst50841)
store volatile %struct.ScmObj* %anf_45bind50942, %struct.ScmObj** %stackaddr$prim58777, align 8
%stackaddr$prim58778 = alloca %struct.ScmObj*, align 8
%anf_45bind50943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst50841)
store volatile %struct.ScmObj* %anf_45bind50943, %struct.ScmObj** %stackaddr$prim58778, align 8
%stackaddr$makeclosure58779 = alloca %struct.ScmObj*, align 8
%fptrToInt58780 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51356 to i64
%ae51356 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58780)
store volatile %struct.ScmObj* %ae51356, %struct.ScmObj** %stackaddr$makeclosure58779, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51356, %struct.ScmObj* %k51196, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51356, %struct.ScmObj* %f50843, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51356, %struct.ScmObj* %anf_45bind50942, i64 2)
%argslist57186$_37foldr1508400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58781 = alloca %struct.ScmObj*, align 8
%argslist57186$_37foldr1508401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50943, %struct.ScmObj* %argslist57186$_37foldr1508400)
store volatile %struct.ScmObj* %argslist57186$_37foldr1508401, %struct.ScmObj** %stackaddr$prim58781, align 8
%stackaddr$prim58782 = alloca %struct.ScmObj*, align 8
%argslist57186$_37foldr1508402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc50842, %struct.ScmObj* %argslist57186$_37foldr1508401)
store volatile %struct.ScmObj* %argslist57186$_37foldr1508402, %struct.ScmObj** %stackaddr$prim58782, align 8
%stackaddr$prim58783 = alloca %struct.ScmObj*, align 8
%argslist57186$_37foldr1508403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50843, %struct.ScmObj* %argslist57186$_37foldr1508402)
store volatile %struct.ScmObj* %argslist57186$_37foldr1508403, %struct.ScmObj** %stackaddr$prim58783, align 8
%stackaddr$prim58784 = alloca %struct.ScmObj*, align 8
%argslist57186$_37foldr1508404 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51356, %struct.ScmObj* %argslist57186$_37foldr1508403)
store volatile %struct.ScmObj* %argslist57186$_37foldr1508404, %struct.ScmObj** %stackaddr$prim58784, align 8
%clofunc58785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr150840)
musttail call tailcc void %clofunc58785(%struct.ScmObj* %_37foldr150840, %struct.ScmObj* %argslist57186$_37foldr1508404)
ret void
}

define tailcc void @proc_clo$ae51356(%struct.ScmObj* %env$ae51356,%struct.ScmObj* %current_45args57182) {
%stackaddr$env-ref58786 = alloca %struct.ScmObj*, align 8
%k51196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51356, i64 0)
store %struct.ScmObj* %k51196, %struct.ScmObj** %stackaddr$env-ref58786
%stackaddr$env-ref58787 = alloca %struct.ScmObj*, align 8
%f50843 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51356, i64 1)
store %struct.ScmObj* %f50843, %struct.ScmObj** %stackaddr$env-ref58787
%stackaddr$env-ref58788 = alloca %struct.ScmObj*, align 8
%anf_45bind50942 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51356, i64 2)
store %struct.ScmObj* %anf_45bind50942, %struct.ScmObj** %stackaddr$env-ref58788
%stackaddr$prim58789 = alloca %struct.ScmObj*, align 8
%_95k51197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57182)
store volatile %struct.ScmObj* %_95k51197, %struct.ScmObj** %stackaddr$prim58789, align 8
%stackaddr$prim58790 = alloca %struct.ScmObj*, align 8
%current_45args57183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57182)
store volatile %struct.ScmObj* %current_45args57183, %struct.ScmObj** %stackaddr$prim58790, align 8
%stackaddr$prim58791 = alloca %struct.ScmObj*, align 8
%anf_45bind50944 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57183)
store volatile %struct.ScmObj* %anf_45bind50944, %struct.ScmObj** %stackaddr$prim58791, align 8
%argslist57185$f508430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58792 = alloca %struct.ScmObj*, align 8
%argslist57185$f508431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50944, %struct.ScmObj* %argslist57185$f508430)
store volatile %struct.ScmObj* %argslist57185$f508431, %struct.ScmObj** %stackaddr$prim58792, align 8
%stackaddr$prim58793 = alloca %struct.ScmObj*, align 8
%argslist57185$f508432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50942, %struct.ScmObj* %argslist57185$f508431)
store volatile %struct.ScmObj* %argslist57185$f508432, %struct.ScmObj** %stackaddr$prim58793, align 8
%stackaddr$prim58794 = alloca %struct.ScmObj*, align 8
%argslist57185$f508433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51196, %struct.ScmObj* %argslist57185$f508432)
store volatile %struct.ScmObj* %argslist57185$f508433, %struct.ScmObj** %stackaddr$prim58794, align 8
%clofunc58795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50843)
musttail call tailcc void %clofunc58795(%struct.ScmObj* %f50843, %struct.ScmObj* %argslist57185$f508433)
ret void
}

define tailcc void @proc_clo$ae51224(%struct.ScmObj* %env$ae51224,%struct.ScmObj* %current_45args57190) {
%stackaddr$prim58796 = alloca %struct.ScmObj*, align 8
%k51198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57190)
store volatile %struct.ScmObj* %k51198, %struct.ScmObj** %stackaddr$prim58796, align 8
%stackaddr$prim58797 = alloca %struct.ScmObj*, align 8
%current_45args57191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57190)
store volatile %struct.ScmObj* %current_45args57191, %struct.ScmObj** %stackaddr$prim58797, align 8
%stackaddr$prim58798 = alloca %struct.ScmObj*, align 8
%y50820 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57191)
store volatile %struct.ScmObj* %y50820, %struct.ScmObj** %stackaddr$prim58798, align 8
%ae51226 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58799 = alloca %struct.ScmObj*, align 8
%fptrToInt58800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51227 to i64
%ae51227 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58800)
store volatile %struct.ScmObj* %ae51227, %struct.ScmObj** %stackaddr$makeclosure58799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51227, %struct.ScmObj* %y50820, i64 0)
%argslist57209$k511980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58801 = alloca %struct.ScmObj*, align 8
%argslist57209$k511981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51227, %struct.ScmObj* %argslist57209$k511980)
store volatile %struct.ScmObj* %argslist57209$k511981, %struct.ScmObj** %stackaddr$prim58801, align 8
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%argslist57209$k511982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51226, %struct.ScmObj* %argslist57209$k511981)
store volatile %struct.ScmObj* %argslist57209$k511982, %struct.ScmObj** %stackaddr$prim58802, align 8
%clofunc58803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k51198)
musttail call tailcc void %clofunc58803(%struct.ScmObj* %k51198, %struct.ScmObj* %argslist57209$k511982)
ret void
}

define tailcc void @proc_clo$ae51227(%struct.ScmObj* %env$ae51227,%struct.ScmObj* %current_45args57193) {
%stackaddr$env-ref58804 = alloca %struct.ScmObj*, align 8
%y50820 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51227, i64 0)
store %struct.ScmObj* %y50820, %struct.ScmObj** %stackaddr$env-ref58804
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%k51199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57193)
store volatile %struct.ScmObj* %k51199, %struct.ScmObj** %stackaddr$prim58805, align 8
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%current_45args57194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57193)
store volatile %struct.ScmObj* %current_45args57194, %struct.ScmObj** %stackaddr$prim58806, align 8
%stackaddr$prim58807 = alloca %struct.ScmObj*, align 8
%f50821 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57194)
store volatile %struct.ScmObj* %f50821, %struct.ScmObj** %stackaddr$prim58807, align 8
%stackaddr$makeclosure58808 = alloca %struct.ScmObj*, align 8
%fptrToInt58809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51228 to i64
%ae51228 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58809)
store volatile %struct.ScmObj* %ae51228, %struct.ScmObj** %stackaddr$makeclosure58808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51228, %struct.ScmObj* %f50821, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51228, %struct.ScmObj* %k51199, i64 1)
%ae51229 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58810 = alloca %struct.ScmObj*, align 8
%fptrToInt58811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51230 to i64
%ae51230 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58811)
store volatile %struct.ScmObj* %ae51230, %struct.ScmObj** %stackaddr$makeclosure58810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51230, %struct.ScmObj* %f50821, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51230, %struct.ScmObj* %y50820, i64 1)
%argslist57208$ae512280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58812 = alloca %struct.ScmObj*, align 8
%argslist57208$ae512281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51230, %struct.ScmObj* %argslist57208$ae512280)
store volatile %struct.ScmObj* %argslist57208$ae512281, %struct.ScmObj** %stackaddr$prim58812, align 8
%stackaddr$prim58813 = alloca %struct.ScmObj*, align 8
%argslist57208$ae512282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51229, %struct.ScmObj* %argslist57208$ae512281)
store volatile %struct.ScmObj* %argslist57208$ae512282, %struct.ScmObj** %stackaddr$prim58813, align 8
%clofunc58814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51228)
musttail call tailcc void %clofunc58814(%struct.ScmObj* %ae51228, %struct.ScmObj* %argslist57208$ae512282)
ret void
}

define tailcc void @proc_clo$ae51228(%struct.ScmObj* %env$ae51228,%struct.ScmObj* %current_45args57196) {
%stackaddr$env-ref58815 = alloca %struct.ScmObj*, align 8
%f50821 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51228, i64 0)
store %struct.ScmObj* %f50821, %struct.ScmObj** %stackaddr$env-ref58815
%stackaddr$env-ref58816 = alloca %struct.ScmObj*, align 8
%k51199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51228, i64 1)
store %struct.ScmObj* %k51199, %struct.ScmObj** %stackaddr$env-ref58816
%stackaddr$prim58817 = alloca %struct.ScmObj*, align 8
%_95k51200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57196)
store volatile %struct.ScmObj* %_95k51200, %struct.ScmObj** %stackaddr$prim58817, align 8
%stackaddr$prim58818 = alloca %struct.ScmObj*, align 8
%current_45args57197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57196)
store volatile %struct.ScmObj* %current_45args57197, %struct.ScmObj** %stackaddr$prim58818, align 8
%stackaddr$prim58819 = alloca %struct.ScmObj*, align 8
%anf_45bind50939 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57197)
store volatile %struct.ScmObj* %anf_45bind50939, %struct.ScmObj** %stackaddr$prim58819, align 8
%argslist57199$f508210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58820 = alloca %struct.ScmObj*, align 8
%argslist57199$f508211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind50939, %struct.ScmObj* %argslist57199$f508210)
store volatile %struct.ScmObj* %argslist57199$f508211, %struct.ScmObj** %stackaddr$prim58820, align 8
%stackaddr$prim58821 = alloca %struct.ScmObj*, align 8
%argslist57199$f508212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51199, %struct.ScmObj* %argslist57199$f508211)
store volatile %struct.ScmObj* %argslist57199$f508212, %struct.ScmObj** %stackaddr$prim58821, align 8
%clofunc58822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f50821)
musttail call tailcc void %clofunc58822(%struct.ScmObj* %f50821, %struct.ScmObj* %argslist57199$f508212)
ret void
}

define tailcc void @proc_clo$ae51230(%struct.ScmObj* %env$ae51230,%struct.ScmObj* %args5082251201) {
%stackaddr$env-ref58823 = alloca %struct.ScmObj*, align 8
%f50821 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51230, i64 0)
store %struct.ScmObj* %f50821, %struct.ScmObj** %stackaddr$env-ref58823
%stackaddr$env-ref58824 = alloca %struct.ScmObj*, align 8
%y50820 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51230, i64 1)
store %struct.ScmObj* %y50820, %struct.ScmObj** %stackaddr$env-ref58824
%stackaddr$prim58825 = alloca %struct.ScmObj*, align 8
%k51202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args5082251201)
store volatile %struct.ScmObj* %k51202, %struct.ScmObj** %stackaddr$prim58825, align 8
%stackaddr$prim58826 = alloca %struct.ScmObj*, align 8
%args50822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args5082251201)
store volatile %struct.ScmObj* %args50822, %struct.ScmObj** %stackaddr$prim58826, align 8
%stackaddr$makeclosure58827 = alloca %struct.ScmObj*, align 8
%fptrToInt58828 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51234 to i64
%ae51234 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58828)
store volatile %struct.ScmObj* %ae51234, %struct.ScmObj** %stackaddr$makeclosure58827, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51234, %struct.ScmObj* %args50822, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51234, %struct.ScmObj* %f50821, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51234, %struct.ScmObj* %k51202, i64 2)
%argslist57207$y508200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58829 = alloca %struct.ScmObj*, align 8
%argslist57207$y508201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y50820, %struct.ScmObj* %argslist57207$y508200)
store volatile %struct.ScmObj* %argslist57207$y508201, %struct.ScmObj** %stackaddr$prim58829, align 8
%stackaddr$prim58830 = alloca %struct.ScmObj*, align 8
%argslist57207$y508202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51234, %struct.ScmObj* %argslist57207$y508201)
store volatile %struct.ScmObj* %argslist57207$y508202, %struct.ScmObj** %stackaddr$prim58830, align 8
%clofunc58831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y50820)
musttail call tailcc void %clofunc58831(%struct.ScmObj* %y50820, %struct.ScmObj* %argslist57207$y508202)
ret void
}

define tailcc void @proc_clo$ae51234(%struct.ScmObj* %env$ae51234,%struct.ScmObj* %current_45args57200) {
%stackaddr$env-ref58832 = alloca %struct.ScmObj*, align 8
%args50822 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51234, i64 0)
store %struct.ScmObj* %args50822, %struct.ScmObj** %stackaddr$env-ref58832
%stackaddr$env-ref58833 = alloca %struct.ScmObj*, align 8
%f50821 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51234, i64 1)
store %struct.ScmObj* %f50821, %struct.ScmObj** %stackaddr$env-ref58833
%stackaddr$env-ref58834 = alloca %struct.ScmObj*, align 8
%k51202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51234, i64 2)
store %struct.ScmObj* %k51202, %struct.ScmObj** %stackaddr$env-ref58834
%stackaddr$prim58835 = alloca %struct.ScmObj*, align 8
%_95k51203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57200)
store volatile %struct.ScmObj* %_95k51203, %struct.ScmObj** %stackaddr$prim58835, align 8
%stackaddr$prim58836 = alloca %struct.ScmObj*, align 8
%current_45args57201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57200)
store volatile %struct.ScmObj* %current_45args57201, %struct.ScmObj** %stackaddr$prim58836, align 8
%stackaddr$prim58837 = alloca %struct.ScmObj*, align 8
%anf_45bind50937 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57201)
store volatile %struct.ScmObj* %anf_45bind50937, %struct.ScmObj** %stackaddr$prim58837, align 8
%stackaddr$makeclosure58838 = alloca %struct.ScmObj*, align 8
%fptrToInt58839 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51237 to i64
%ae51237 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58839)
store volatile %struct.ScmObj* %ae51237, %struct.ScmObj** %stackaddr$makeclosure58838, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51237, %struct.ScmObj* %args50822, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51237, %struct.ScmObj* %k51202, i64 1)
%argslist57206$anf_45bind509370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58840 = alloca %struct.ScmObj*, align 8
%argslist57206$anf_45bind509371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f50821, %struct.ScmObj* %argslist57206$anf_45bind509370)
store volatile %struct.ScmObj* %argslist57206$anf_45bind509371, %struct.ScmObj** %stackaddr$prim58840, align 8
%stackaddr$prim58841 = alloca %struct.ScmObj*, align 8
%argslist57206$anf_45bind509372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51237, %struct.ScmObj* %argslist57206$anf_45bind509371)
store volatile %struct.ScmObj* %argslist57206$anf_45bind509372, %struct.ScmObj** %stackaddr$prim58841, align 8
%clofunc58842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50937)
musttail call tailcc void %clofunc58842(%struct.ScmObj* %anf_45bind50937, %struct.ScmObj* %argslist57206$anf_45bind509372)
ret void
}

define tailcc void @proc_clo$ae51237(%struct.ScmObj* %env$ae51237,%struct.ScmObj* %current_45args57203) {
%stackaddr$env-ref58843 = alloca %struct.ScmObj*, align 8
%args50822 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51237, i64 0)
store %struct.ScmObj* %args50822, %struct.ScmObj** %stackaddr$env-ref58843
%stackaddr$env-ref58844 = alloca %struct.ScmObj*, align 8
%k51202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51237, i64 1)
store %struct.ScmObj* %k51202, %struct.ScmObj** %stackaddr$env-ref58844
%stackaddr$prim58845 = alloca %struct.ScmObj*, align 8
%_95k51204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57203)
store volatile %struct.ScmObj* %_95k51204, %struct.ScmObj** %stackaddr$prim58845, align 8
%stackaddr$prim58846 = alloca %struct.ScmObj*, align 8
%current_45args57204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57203)
store volatile %struct.ScmObj* %current_45args57204, %struct.ScmObj** %stackaddr$prim58846, align 8
%stackaddr$prim58847 = alloca %struct.ScmObj*, align 8
%anf_45bind50938 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57204)
store volatile %struct.ScmObj* %anf_45bind50938, %struct.ScmObj** %stackaddr$prim58847, align 8
%stackaddr$prim58848 = alloca %struct.ScmObj*, align 8
%cpsargs51205 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51202, %struct.ScmObj* %args50822)
store volatile %struct.ScmObj* %cpsargs51205, %struct.ScmObj** %stackaddr$prim58848, align 8
%clofunc58849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind50938)
musttail call tailcc void %clofunc58849(%struct.ScmObj* %anf_45bind50938, %struct.ScmObj* %cpsargs51205)
ret void
}

define tailcc void @proc_clo$ae51209(%struct.ScmObj* %env$ae51209,%struct.ScmObj* %current_45args57211) {
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%k51206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57211)
store volatile %struct.ScmObj* %k51206, %struct.ScmObj** %stackaddr$prim58850, align 8
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%current_45args57212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57211)
store volatile %struct.ScmObj* %current_45args57212, %struct.ScmObj** %stackaddr$prim58851, align 8
%stackaddr$prim58852 = alloca %struct.ScmObj*, align 8
%yu50819 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57212)
store volatile %struct.ScmObj* %yu50819, %struct.ScmObj** %stackaddr$prim58852, align 8
%argslist57214$yu508190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58853 = alloca %struct.ScmObj*, align 8
%argslist57214$yu508191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu50819, %struct.ScmObj* %argslist57214$yu508190)
store volatile %struct.ScmObj* %argslist57214$yu508191, %struct.ScmObj** %stackaddr$prim58853, align 8
%stackaddr$prim58854 = alloca %struct.ScmObj*, align 8
%argslist57214$yu508192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k51206, %struct.ScmObj* %argslist57214$yu508191)
store volatile %struct.ScmObj* %argslist57214$yu508192, %struct.ScmObj** %stackaddr$prim58854, align 8
%clofunc58855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu50819)
musttail call tailcc void %clofunc58855(%struct.ScmObj* %yu50819, %struct.ScmObj* %argslist57214$yu508192)
ret void
}