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
%mainenv54681 = call %struct.ScmObj* @const_init_null()
%mainargs54682 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54681, %struct.ScmObj* %mainargs54682)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54679,%struct.ScmObj* %mainargs54680) {
%stackaddr$makeclosure54683 = alloca %struct.ScmObj*, align 8
%fptrToInt54684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48422 to i64
%ae48422 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54684)
store volatile %struct.ScmObj* %ae48422, %struct.ScmObj** %stackaddr$makeclosure54683, align 8
%ae48423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54685 = alloca %struct.ScmObj*, align 8
%fptrToInt54686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48424 to i64
%ae48424 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54686)
store volatile %struct.ScmObj* %ae48424, %struct.ScmObj** %stackaddr$makeclosure54685, align 8
%argslist54678$ae484220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54687 = alloca %struct.ScmObj*, align 8
%argslist54678$ae484221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48424, %struct.ScmObj* %argslist54678$ae484220)
store volatile %struct.ScmObj* %argslist54678$ae484221, %struct.ScmObj** %stackaddr$prim54687, align 8
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%argslist54678$ae484222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48423, %struct.ScmObj* %argslist54678$ae484221)
store volatile %struct.ScmObj* %argslist54678$ae484222, %struct.ScmObj** %stackaddr$prim54688, align 8
%clofunc54689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48422)
musttail call tailcc void %clofunc54689(%struct.ScmObj* %ae48422, %struct.ScmObj* %argslist54678$ae484222)
ret void
}

define tailcc void @proc_clo$ae48422(%struct.ScmObj* %env$ae48422,%struct.ScmObj* %current_45args54124) {
%stackaddr$prim54690 = alloca %struct.ScmObj*, align 8
%_95k48260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %_95k48260, %struct.ScmObj** %stackaddr$prim54690, align 8
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%current_45args54125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %current_45args54125, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%anf_45bind48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54125)
store volatile %struct.ScmObj* %anf_45bind48144, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$makeclosure54693 = alloca %struct.ScmObj*, align 8
%fptrToInt54694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48437 to i64
%ae48437 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54694)
store volatile %struct.ScmObj* %ae48437, %struct.ScmObj** %stackaddr$makeclosure54693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48437, %struct.ScmObj* %anf_45bind48144, i64 0)
%ae48438 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54695 = alloca %struct.ScmObj*, align 8
%fptrToInt54696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48439 to i64
%ae48439 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54696)
store volatile %struct.ScmObj* %ae48439, %struct.ScmObj** %stackaddr$makeclosure54695, align 8
%argslist54673$ae484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%argslist54673$ae484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48439, %struct.ScmObj* %argslist54673$ae484370)
store volatile %struct.ScmObj* %argslist54673$ae484371, %struct.ScmObj** %stackaddr$prim54697, align 8
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%argslist54673$ae484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48438, %struct.ScmObj* %argslist54673$ae484371)
store volatile %struct.ScmObj* %argslist54673$ae484372, %struct.ScmObj** %stackaddr$prim54698, align 8
%clofunc54699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48437)
musttail call tailcc void %clofunc54699(%struct.ScmObj* %ae48437, %struct.ScmObj* %argslist54673$ae484372)
ret void
}

define tailcc void @proc_clo$ae48437(%struct.ScmObj* %env$ae48437,%struct.ScmObj* %current_45args54127) {
%stackaddr$env-ref54700 = alloca %struct.ScmObj*, align 8
%anf_45bind48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48437, i64 0)
store %struct.ScmObj* %anf_45bind48144, %struct.ScmObj** %stackaddr$env-ref54700
%stackaddr$prim54701 = alloca %struct.ScmObj*, align 8
%_95k48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %_95k48261, %struct.ScmObj** %stackaddr$prim54701, align 8
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%current_45args54128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %current_45args54128, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54128)
store volatile %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$makeclosure54704 = alloca %struct.ScmObj*, align 8
%fptrToInt54705 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48552 to i64
%ae48552 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54705)
store volatile %struct.ScmObj* %ae48552, %struct.ScmObj** %stackaddr$makeclosure54704, align 8
%argslist54652$anf_45bind481440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54706 = alloca %struct.ScmObj*, align 8
%argslist54652$anf_45bind481441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48148, %struct.ScmObj* %argslist54652$anf_45bind481440)
store volatile %struct.ScmObj* %argslist54652$anf_45bind481441, %struct.ScmObj** %stackaddr$prim54706, align 8
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%argslist54652$anf_45bind481442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48552, %struct.ScmObj* %argslist54652$anf_45bind481441)
store volatile %struct.ScmObj* %argslist54652$anf_45bind481442, %struct.ScmObj** %stackaddr$prim54707, align 8
%clofunc54708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48144)
musttail call tailcc void %clofunc54708(%struct.ScmObj* %anf_45bind48144, %struct.ScmObj* %argslist54652$anf_45bind481442)
ret void
}

define tailcc void @proc_clo$ae48552(%struct.ScmObj* %env$ae48552,%struct.ScmObj* %current_45args54130) {
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%_95k48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %_95k48262, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%current_45args54131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %current_45args54131, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54131)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim54711, align 8
%stackaddr$makeclosure54712 = alloca %struct.ScmObj*, align 8
%fptrToInt54713 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48554 to i64
%ae48554 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54713)
store volatile %struct.ScmObj* %ae48554, %struct.ScmObj** %stackaddr$makeclosure54712, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48555 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54714 = alloca %struct.ScmObj*, align 8
%fptrToInt54715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48556 to i64
%ae48556 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54715)
store volatile %struct.ScmObj* %ae48556, %struct.ScmObj** %stackaddr$makeclosure54714, align 8
%argslist54651$ae485540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54716 = alloca %struct.ScmObj*, align 8
%argslist54651$ae485541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48556, %struct.ScmObj* %argslist54651$ae485540)
store volatile %struct.ScmObj* %argslist54651$ae485541, %struct.ScmObj** %stackaddr$prim54716, align 8
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%argslist54651$ae485542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48555, %struct.ScmObj* %argslist54651$ae485541)
store volatile %struct.ScmObj* %argslist54651$ae485542, %struct.ScmObj** %stackaddr$prim54717, align 8
%clofunc54718 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48554)
musttail call tailcc void %clofunc54718(%struct.ScmObj* %ae48554, %struct.ScmObj* %argslist54651$ae485542)
ret void
}

define tailcc void @proc_clo$ae48554(%struct.ScmObj* %env$ae48554,%struct.ScmObj* %current_45args54133) {
%stackaddr$env-ref54719 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54719
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%_95k48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %_95k48263, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%current_45args54134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %current_45args54134, %struct.ScmObj** %stackaddr$prim54721, align 8
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54134)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim54722, align 8
%stackaddr$makeclosure54723 = alloca %struct.ScmObj*, align 8
%fptrToInt54724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48632 to i64
%ae48632 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54724)
store volatile %struct.ScmObj* %ae48632, %struct.ScmObj** %stackaddr$makeclosure54723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist54635$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%argslist54635$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48153, %struct.ScmObj* %argslist54635$Ycmb480250)
store volatile %struct.ScmObj* %argslist54635$Ycmb480251, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%argslist54635$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48632, %struct.ScmObj* %argslist54635$Ycmb480251)
store volatile %struct.ScmObj* %argslist54635$Ycmb480252, %struct.ScmObj** %stackaddr$prim54726, align 8
%clofunc54727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54727(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54635$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48632(%struct.ScmObj* %env$ae48632,%struct.ScmObj* %current_45args54136) {
%stackaddr$env-ref54728 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54728
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%_95k48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54136)
store volatile %struct.ScmObj* %_95k48264, %struct.ScmObj** %stackaddr$prim54729, align 8
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%current_45args54137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54136)
store volatile %struct.ScmObj* %current_45args54137, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$makeclosure54732 = alloca %struct.ScmObj*, align 8
%fptrToInt54733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48634 to i64
%ae48634 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54733)
store volatile %struct.ScmObj* %ae48634, %struct.ScmObj** %stackaddr$makeclosure54732, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %_37foldr148046, i64 1)
%ae48635 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54734 = alloca %struct.ScmObj*, align 8
%fptrToInt54735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48636 to i64
%ae48636 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54735)
store volatile %struct.ScmObj* %ae48636, %struct.ScmObj** %stackaddr$makeclosure54734, align 8
%argslist54634$ae486340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%argslist54634$ae486341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48636, %struct.ScmObj* %argslist54634$ae486340)
store volatile %struct.ScmObj* %argslist54634$ae486341, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%argslist54634$ae486342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist54634$ae486341)
store volatile %struct.ScmObj* %argslist54634$ae486342, %struct.ScmObj** %stackaddr$prim54737, align 8
%clofunc54738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48634)
musttail call tailcc void %clofunc54738(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist54634$ae486342)
ret void
}

define tailcc void @proc_clo$ae48634(%struct.ScmObj* %env$ae48634,%struct.ScmObj* %current_45args54139) {
%stackaddr$env-ref54739 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54739
%stackaddr$env-ref54740 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54740
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%_95k48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %_95k48265, %struct.ScmObj** %stackaddr$prim54741, align 8
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%current_45args54140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %current_45args54140, %struct.ScmObj** %stackaddr$prim54742, align 8
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$makeclosure54744 = alloca %struct.ScmObj*, align 8
%fptrToInt54745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48729 to i64
%ae48729 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54745)
store volatile %struct.ScmObj* %ae48729, %struct.ScmObj** %stackaddr$makeclosure54744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48729, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48729, %struct.ScmObj* %_37foldr148046, i64 1)
%argslist54615$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%argslist54615$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist54615$Ycmb480250)
store volatile %struct.ScmObj* %argslist54615$Ycmb480251, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%argslist54615$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48729, %struct.ScmObj* %argslist54615$Ycmb480251)
store volatile %struct.ScmObj* %argslist54615$Ycmb480252, %struct.ScmObj** %stackaddr$prim54747, align 8
%clofunc54748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54748(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54615$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48729(%struct.ScmObj* %env$ae48729,%struct.ScmObj* %current_45args54142) {
%stackaddr$env-ref54749 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48729, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54749
%stackaddr$env-ref54750 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48729, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54750
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%_95k48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %_95k48266, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%current_45args54143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %current_45args54143, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54143)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$makeclosure54754 = alloca %struct.ScmObj*, align 8
%fptrToInt54755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48731 to i64
%ae48731 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54755)
store volatile %struct.ScmObj* %ae48731, %struct.ScmObj** %stackaddr$makeclosure54754, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48731, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48731, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48731, %struct.ScmObj* %_37map148042, i64 2)
%ae48732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48733 to i64
%ae48733 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae48733, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
%argslist54614$ae487310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%argslist54614$ae487311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48733, %struct.ScmObj* %argslist54614$ae487310)
store volatile %struct.ScmObj* %argslist54614$ae487311, %struct.ScmObj** %stackaddr$prim54758, align 8
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%argslist54614$ae487312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48732, %struct.ScmObj* %argslist54614$ae487311)
store volatile %struct.ScmObj* %argslist54614$ae487312, %struct.ScmObj** %stackaddr$prim54759, align 8
%clofunc54760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48731)
musttail call tailcc void %clofunc54760(%struct.ScmObj* %ae48731, %struct.ScmObj* %argslist54614$ae487312)
ret void
}

define tailcc void @proc_clo$ae48731(%struct.ScmObj* %env$ae48731,%struct.ScmObj* %current_45args54145) {
%stackaddr$env-ref54761 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48731, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54761
%stackaddr$env-ref54762 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48731, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54762
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48731, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%_95k48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %_95k48267, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%current_45args54146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %current_45args54146, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim54766, align 8
%stackaddr$makeclosure54767 = alloca %struct.ScmObj*, align 8
%fptrToInt54768 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48879 to i64
%ae48879 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54768)
store volatile %struct.ScmObj* %ae48879, %struct.ScmObj** %stackaddr$makeclosure54767, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48879, %struct.ScmObj* %_37map148042, i64 2)
%argslist54598$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54769 = alloca %struct.ScmObj*, align 8
%argslist54598$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %argslist54598$Ycmb480250)
store volatile %struct.ScmObj* %argslist54598$Ycmb480251, %struct.ScmObj** %stackaddr$prim54769, align 8
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%argslist54598$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48879, %struct.ScmObj* %argslist54598$Ycmb480251)
store volatile %struct.ScmObj* %argslist54598$Ycmb480252, %struct.ScmObj** %stackaddr$prim54770, align 8
%clofunc54771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54771(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54598$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48879(%struct.ScmObj* %env$ae48879,%struct.ScmObj* %current_45args54148) {
%stackaddr$env-ref54772 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54772
%stackaddr$env-ref54773 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54773
%stackaddr$env-ref54774 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48879, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54774
%stackaddr$prim54775 = alloca %struct.ScmObj*, align 8
%_95k48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %_95k48268, %struct.ScmObj** %stackaddr$prim54775, align 8
%stackaddr$prim54776 = alloca %struct.ScmObj*, align 8
%current_45args54149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %current_45args54149, %struct.ScmObj** %stackaddr$prim54776, align 8
%stackaddr$prim54777 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim54777, align 8
%stackaddr$makeclosure54778 = alloca %struct.ScmObj*, align 8
%fptrToInt54779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48881 to i64
%ae48881 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54779)
store volatile %struct.ScmObj* %ae48881, %struct.ScmObj** %stackaddr$makeclosure54778, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48881, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48881, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48881, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48881, %struct.ScmObj* %_37map148042, i64 3)
%ae48882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54780 = alloca %struct.ScmObj*, align 8
%fptrToInt54781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48883 to i64
%ae48883 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54781)
store volatile %struct.ScmObj* %ae48883, %struct.ScmObj** %stackaddr$makeclosure54780, align 8
%argslist54597$ae488810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%argslist54597$ae488811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48883, %struct.ScmObj* %argslist54597$ae488810)
store volatile %struct.ScmObj* %argslist54597$ae488811, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%argslist54597$ae488812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48882, %struct.ScmObj* %argslist54597$ae488811)
store volatile %struct.ScmObj* %argslist54597$ae488812, %struct.ScmObj** %stackaddr$prim54783, align 8
%clofunc54784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48881)
musttail call tailcc void %clofunc54784(%struct.ScmObj* %ae48881, %struct.ScmObj* %argslist54597$ae488812)
ret void
}

define tailcc void @proc_clo$ae48881(%struct.ScmObj* %env$ae48881,%struct.ScmObj* %current_45args54151) {
%stackaddr$env-ref54785 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48881, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54785
%stackaddr$env-ref54786 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48881, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54786
%stackaddr$env-ref54787 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48881, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54787
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48881, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$prim54789 = alloca %struct.ScmObj*, align 8
%_95k48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %_95k48269, %struct.ScmObj** %stackaddr$prim54789, align 8
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%current_45args54152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %current_45args54152, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54152)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$makeclosure54792 = alloca %struct.ScmObj*, align 8
%fptrToInt54793 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48962 to i64
%ae48962 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54793)
store volatile %struct.ScmObj* %ae48962, %struct.ScmObj** %stackaddr$makeclosure54792, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48962, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48962, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48962, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48962, %struct.ScmObj* %_37map148042, i64 3)
%argslist54583$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%argslist54583$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist54583$Ycmb480250)
store volatile %struct.ScmObj* %argslist54583$Ycmb480251, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%argslist54583$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48962, %struct.ScmObj* %argslist54583$Ycmb480251)
store volatile %struct.ScmObj* %argslist54583$Ycmb480252, %struct.ScmObj** %stackaddr$prim54795, align 8
%clofunc54796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54796(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54583$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48962(%struct.ScmObj* %env$ae48962,%struct.ScmObj* %current_45args54154) {
%stackaddr$env-ref54797 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48962, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54797
%stackaddr$env-ref54798 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48962, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54798
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48962, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48962, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%_95k48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %_95k48270, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%current_45args54155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %current_45args54155, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54155)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$makeclosure54804 = alloca %struct.ScmObj*, align 8
%fptrToInt54805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48964 to i64
%ae48964 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54805)
store volatile %struct.ScmObj* %ae48964, %struct.ScmObj** %stackaddr$makeclosure54804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48964, %struct.ScmObj* %_37map148042, i64 4)
%ae48965 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54806 = alloca %struct.ScmObj*, align 8
%fptrToInt54807 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48966 to i64
%ae48966 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54807)
store volatile %struct.ScmObj* %ae48966, %struct.ScmObj** %stackaddr$makeclosure54806, align 8
%argslist54582$ae489640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%argslist54582$ae489641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48966, %struct.ScmObj* %argslist54582$ae489640)
store volatile %struct.ScmObj* %argslist54582$ae489641, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%argslist54582$ae489642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48965, %struct.ScmObj* %argslist54582$ae489641)
store volatile %struct.ScmObj* %argslist54582$ae489642, %struct.ScmObj** %stackaddr$prim54809, align 8
%clofunc54810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48964)
musttail call tailcc void %clofunc54810(%struct.ScmObj* %ae48964, %struct.ScmObj* %argslist54582$ae489642)
ret void
}

define tailcc void @proc_clo$ae48964(%struct.ScmObj* %env$ae48964,%struct.ScmObj* %current_45args54157) {
%stackaddr$env-ref54811 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54811
%stackaddr$env-ref54812 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54812
%stackaddr$env-ref54813 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54813
%stackaddr$env-ref54814 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54814
%stackaddr$env-ref54815 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48964, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54815
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim54816, align 8
%stackaddr$prim54817 = alloca %struct.ScmObj*, align 8
%current_45args54158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %current_45args54158, %struct.ScmObj** %stackaddr$prim54817, align 8
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54158)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$makeclosure54819 = alloca %struct.ScmObj*, align 8
%fptrToInt54820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49041 to i64
%ae49041 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54820)
store volatile %struct.ScmObj* %ae49041, %struct.ScmObj** %stackaddr$makeclosure54819, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49041, %struct.ScmObj* %_37map148042, i64 4)
%argslist54566$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%argslist54566$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist54566$Ycmb480250)
store volatile %struct.ScmObj* %argslist54566$Ycmb480251, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%argslist54566$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49041, %struct.ScmObj* %argslist54566$Ycmb480251)
store volatile %struct.ScmObj* %argslist54566$Ycmb480252, %struct.ScmObj** %stackaddr$prim54822, align 8
%clofunc54823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54823(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54566$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49041(%struct.ScmObj* %env$ae49041,%struct.ScmObj* %current_45args54160) {
%stackaddr$env-ref54824 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54824
%stackaddr$env-ref54825 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54825
%stackaddr$env-ref54826 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54826
%stackaddr$env-ref54827 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54827
%stackaddr$env-ref54828 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49041, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54828
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%current_45args54161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %current_45args54161, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54161)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$makeclosure54832 = alloca %struct.ScmObj*, align 8
%fptrToInt54833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49043 to i64
%ae49043 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54833)
store volatile %struct.ScmObj* %ae49043, %struct.ScmObj** %stackaddr$makeclosure54832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37take48038, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37length48035, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37map148042, i64 5)
%ae49044 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54834 = alloca %struct.ScmObj*, align 8
%fptrToInt54835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49045 to i64
%ae49045 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54835)
store volatile %struct.ScmObj* %ae49045, %struct.ScmObj** %stackaddr$makeclosure54834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49045, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54565$ae490430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%argslist54565$ae490431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49045, %struct.ScmObj* %argslist54565$ae490430)
store volatile %struct.ScmObj* %argslist54565$ae490431, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%argslist54565$ae490432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49044, %struct.ScmObj* %argslist54565$ae490431)
store volatile %struct.ScmObj* %argslist54565$ae490432, %struct.ScmObj** %stackaddr$prim54837, align 8
%clofunc54838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49043)
musttail call tailcc void %clofunc54838(%struct.ScmObj* %ae49043, %struct.ScmObj* %argslist54565$ae490432)
ret void
}

define tailcc void @proc_clo$ae49043(%struct.ScmObj* %env$ae49043,%struct.ScmObj* %current_45args54163) {
%stackaddr$env-ref54839 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54839
%stackaddr$env-ref54840 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54840
%stackaddr$env-ref54841 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54841
%stackaddr$env-ref54842 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 3)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref54842
%stackaddr$env-ref54843 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 4)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref54843
%stackaddr$env-ref54844 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54844
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim54845, align 8
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%current_45args54164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %current_45args54164, %struct.ScmObj** %stackaddr$prim54846, align 8
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$makeclosure54848 = alloca %struct.ScmObj*, align 8
%fptrToInt54849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49097 to i64
%ae49097 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54849)
store volatile %struct.ScmObj* %ae49097, %struct.ScmObj** %stackaddr$makeclosure54848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37map148042, i64 4)
%ae49098 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54850 = alloca %struct.ScmObj*, align 8
%fptrToInt54851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49099 to i64
%ae49099 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54851)
store volatile %struct.ScmObj* %ae49099, %struct.ScmObj** %stackaddr$makeclosure54850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49099, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49099, %struct.ScmObj* %_37length48035, i64 1)
%argslist54551$ae490970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%argslist54551$ae490971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49099, %struct.ScmObj* %argslist54551$ae490970)
store volatile %struct.ScmObj* %argslist54551$ae490971, %struct.ScmObj** %stackaddr$prim54852, align 8
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%argslist54551$ae490972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49098, %struct.ScmObj* %argslist54551$ae490971)
store volatile %struct.ScmObj* %argslist54551$ae490972, %struct.ScmObj** %stackaddr$prim54853, align 8
%clofunc54854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49097)
musttail call tailcc void %clofunc54854(%struct.ScmObj* %ae49097, %struct.ScmObj* %argslist54551$ae490972)
ret void
}

define tailcc void @proc_clo$ae49097(%struct.ScmObj* %env$ae49097,%struct.ScmObj* %current_45args54166) {
%stackaddr$env-ref54855 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54855
%stackaddr$env-ref54856 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54856
%stackaddr$env-ref54857 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54857
%stackaddr$env-ref54858 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54858
%stackaddr$env-ref54859 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref54859
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%current_45args54167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %current_45args54167, %struct.ScmObj** %stackaddr$prim54861, align 8
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$makeclosure54863 = alloca %struct.ScmObj*, align 8
%fptrToInt54864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49127 to i64
%ae49127 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54864)
store volatile %struct.ScmObj* %ae49127, %struct.ScmObj** %stackaddr$makeclosure54863, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49127, %struct.ScmObj* %_37drop_45right48065, i64 4)
%ae49128 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54865 = alloca %struct.ScmObj*, align 8
%fptrToInt54866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49129 to i64
%ae49129 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54866)
store volatile %struct.ScmObj* %ae49129, %struct.ScmObj** %stackaddr$makeclosure54865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49129, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49129, %struct.ScmObj* %_37map148042, i64 1)
%argslist54541$ae491270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%argslist54541$ae491271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49129, %struct.ScmObj* %argslist54541$ae491270)
store volatile %struct.ScmObj* %argslist54541$ae491271, %struct.ScmObj** %stackaddr$prim54867, align 8
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%argslist54541$ae491272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49128, %struct.ScmObj* %argslist54541$ae491271)
store volatile %struct.ScmObj* %argslist54541$ae491272, %struct.ScmObj** %stackaddr$prim54868, align 8
%clofunc54869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49127)
musttail call tailcc void %clofunc54869(%struct.ScmObj* %ae49127, %struct.ScmObj* %argslist54541$ae491272)
ret void
}

define tailcc void @proc_clo$ae49127(%struct.ScmObj* %env$ae49127,%struct.ScmObj* %current_45args54169) {
%stackaddr$env-ref54870 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54870
%stackaddr$env-ref54871 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54871
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$env-ref54873 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54873
%stackaddr$env-ref54874 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49127, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54874
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54875, align 8
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim54876, align 8
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$makeclosure54878 = alloca %struct.ScmObj*, align 8
%fptrToInt54879 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49511 to i64
%ae49511 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54879)
store volatile %struct.ScmObj* %ae49511, %struct.ScmObj** %stackaddr$makeclosure54878, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49511, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49511, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49511, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49511, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49511, %struct.ScmObj* %_37drop_45right48065, i64 4)
%argslist54481$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%argslist54481$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist54481$Ycmb480250)
store volatile %struct.ScmObj* %argslist54481$Ycmb480251, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%argslist54481$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49511, %struct.ScmObj* %argslist54481$Ycmb480251)
store volatile %struct.ScmObj* %argslist54481$Ycmb480252, %struct.ScmObj** %stackaddr$prim54881, align 8
%clofunc54882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54882(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54481$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49511(%struct.ScmObj* %env$ae49511,%struct.ScmObj* %current_45args54172) {
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49511, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49511, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49511, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$env-ref54886 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49511, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54886
%stackaddr$env-ref54887 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49511, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54887
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%current_45args54173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54172)
store volatile %struct.ScmObj* %current_45args54173, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$makeclosure54891 = alloca %struct.ScmObj*, align 8
%fptrToInt54892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49513 to i64
%ae49513 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54892)
store volatile %struct.ScmObj* %ae49513, %struct.ScmObj** %stackaddr$makeclosure54891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49513, %struct.ScmObj* %_37drop_45right48065, i64 5)
%ae49514 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49515 to i64
%ae49515 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae49515, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49515, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist54480$ae495130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%argslist54480$ae495131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49515, %struct.ScmObj* %argslist54480$ae495130)
store volatile %struct.ScmObj* %argslist54480$ae495131, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%argslist54480$ae495132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49514, %struct.ScmObj* %argslist54480$ae495131)
store volatile %struct.ScmObj* %argslist54480$ae495132, %struct.ScmObj** %stackaddr$prim54896, align 8
%clofunc54897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49513)
musttail call tailcc void %clofunc54897(%struct.ScmObj* %ae49513, %struct.ScmObj* %argslist54480$ae495132)
ret void
}

define tailcc void @proc_clo$ae49513(%struct.ScmObj* %env$ae49513,%struct.ScmObj* %current_45args54175) {
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49513, i64 5)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$prim54904 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54904, align 8
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%current_45args54176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %current_45args54176, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$makeclosure54907 = alloca %struct.ScmObj*, align 8
%fptrToInt54908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49590 to i64
%ae49590 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54908)
store volatile %struct.ScmObj* %ae49590, %struct.ScmObj** %stackaddr$makeclosure54907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49590, %struct.ScmObj* %_37map148077, i64 4)
%ae49591 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49592 to i64
%ae49592 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae49592, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49592, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist54461$ae495900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%argslist54461$ae495901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49592, %struct.ScmObj* %argslist54461$ae495900)
store volatile %struct.ScmObj* %argslist54461$ae495901, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%argslist54461$ae495902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49591, %struct.ScmObj* %argslist54461$ae495901)
store volatile %struct.ScmObj* %argslist54461$ae495902, %struct.ScmObj** %stackaddr$prim54912, align 8
%clofunc54913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49590)
musttail call tailcc void %clofunc54913(%struct.ScmObj* %ae49590, %struct.ScmObj* %argslist54461$ae495902)
ret void
}

define tailcc void @proc_clo$ae49590(%struct.ScmObj* %env$ae49590,%struct.ScmObj* %current_45args54178) {
%stackaddr$env-ref54914 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref54914
%stackaddr$env-ref54915 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54915
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49590, i64 4)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$prim54919 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54919, align 8
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%current_45args54179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %current_45args54179, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$makeclosure54922 = alloca %struct.ScmObj*, align 8
%fptrToInt54923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49736 to i64
%ae49736 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54923)
store volatile %struct.ScmObj* %ae49736, %struct.ScmObj** %stackaddr$makeclosure54922, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49736, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49736, %struct.ScmObj* %_37foldl148030, i64 1)
%ae49737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54924 = alloca %struct.ScmObj*, align 8
%fptrToInt54925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49738 to i64
%ae49738 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54925)
store volatile %struct.ScmObj* %ae49738, %struct.ScmObj** %stackaddr$makeclosure54924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49738, %struct.ScmObj* %_37map148077, i64 2)
%argslist54444$ae497360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%argslist54444$ae497361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49738, %struct.ScmObj* %argslist54444$ae497360)
store volatile %struct.ScmObj* %argslist54444$ae497361, %struct.ScmObj** %stackaddr$prim54926, align 8
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%argslist54444$ae497362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49737, %struct.ScmObj* %argslist54444$ae497361)
store volatile %struct.ScmObj* %argslist54444$ae497362, %struct.ScmObj** %stackaddr$prim54927, align 8
%clofunc54928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49736)
musttail call tailcc void %clofunc54928(%struct.ScmObj* %ae49736, %struct.ScmObj* %argslist54444$ae497362)
ret void
}

define tailcc void @proc_clo$ae49736(%struct.ScmObj* %env$ae49736,%struct.ScmObj* %current_45args54181) {
%stackaddr$env-ref54929 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49736, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref54929
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49736, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54931, align 8
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%current_45args54182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %current_45args54182, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim54933, align 8
%stackaddr$makeclosure54934 = alloca %struct.ScmObj*, align 8
%fptrToInt54935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50128 to i64
%ae50128 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54935)
store volatile %struct.ScmObj* %ae50128, %struct.ScmObj** %stackaddr$makeclosure54934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50128, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54384$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54936 = alloca %struct.ScmObj*, align 8
%argslist54384$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54384$Ycmb480250)
store volatile %struct.ScmObj* %argslist54384$Ycmb480251, %struct.ScmObj** %stackaddr$prim54936, align 8
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%argslist54384$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50128, %struct.ScmObj* %argslist54384$Ycmb480251)
store volatile %struct.ScmObj* %argslist54384$Ycmb480252, %struct.ScmObj** %stackaddr$prim54937, align 8
%clofunc54938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc54938(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist54384$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50128(%struct.ScmObj* %env$ae50128,%struct.ScmObj* %current_45args54184) {
%stackaddr$env-ref54939 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50128, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54939
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%current_45args54185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %current_45args54185, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim54942, align 8
%stackaddr$makeclosure54943 = alloca %struct.ScmObj*, align 8
%fptrToInt54944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50130 to i64
%ae50130 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54944)
store volatile %struct.ScmObj* %ae50130, %struct.ScmObj** %stackaddr$makeclosure54943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50130, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50131 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54945 = alloca %struct.ScmObj*, align 8
%fptrToInt54946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50132 to i64
%ae50132 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54946)
store volatile %struct.ScmObj* %ae50132, %struct.ScmObj** %stackaddr$makeclosure54945, align 8
%argslist54383$ae501300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%argslist54383$ae501301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50132, %struct.ScmObj* %argslist54383$ae501300)
store volatile %struct.ScmObj* %argslist54383$ae501301, %struct.ScmObj** %stackaddr$prim54947, align 8
%stackaddr$prim54948 = alloca %struct.ScmObj*, align 8
%argslist54383$ae501302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50131, %struct.ScmObj* %argslist54383$ae501301)
store volatile %struct.ScmObj* %argslist54383$ae501302, %struct.ScmObj** %stackaddr$prim54948, align 8
%clofunc54949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50130)
musttail call tailcc void %clofunc54949(%struct.ScmObj* %ae50130, %struct.ScmObj* %argslist54383$ae501302)
ret void
}

define tailcc void @proc_clo$ae50130(%struct.ScmObj* %env$ae50130,%struct.ScmObj* %current_45args54187) {
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50130, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54951, align 8
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%current_45args54188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %current_45args54188, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$makeclosure54954 = alloca %struct.ScmObj*, align 8
%fptrToInt54955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50154 to i64
%ae50154 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54955)
store volatile %struct.ScmObj* %ae50154, %struct.ScmObj** %stackaddr$makeclosure54954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50154, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50155 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54956 = alloca %struct.ScmObj*, align 8
%fptrToInt54957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50156 to i64
%ae50156 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54957)
store volatile %struct.ScmObj* %ae50156, %struct.ScmObj** %stackaddr$makeclosure54956, align 8
%argslist54377$ae501540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%argslist54377$ae501541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50156, %struct.ScmObj* %argslist54377$ae501540)
store volatile %struct.ScmObj* %argslist54377$ae501541, %struct.ScmObj** %stackaddr$prim54958, align 8
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%argslist54377$ae501542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50155, %struct.ScmObj* %argslist54377$ae501541)
store volatile %struct.ScmObj* %argslist54377$ae501542, %struct.ScmObj** %stackaddr$prim54959, align 8
%clofunc54960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50154)
musttail call tailcc void %clofunc54960(%struct.ScmObj* %ae50154, %struct.ScmObj* %argslist54377$ae501542)
ret void
}

define tailcc void @proc_clo$ae50154(%struct.ScmObj* %env$ae50154,%struct.ScmObj* %current_45args54190) {
%stackaddr$env-ref54961 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50154, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54961
%stackaddr$prim54962 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54962, align 8
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%current_45args54191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %current_45args54191, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim54964, align 8
%ae50178 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50179 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50178, %struct.ScmObj* %ae50179)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim54965, align 8
%stackaddr$makeclosure54966 = alloca %struct.ScmObj*, align 8
%fptrToInt54967 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50180 to i64
%ae50180 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54967)
store volatile %struct.ScmObj* %ae50180, %struct.ScmObj** %stackaddr$makeclosure54966, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50180, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50180, %struct.ScmObj* %_37foldl148030, i64 1)
%ae50181 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54968 = alloca %struct.ScmObj*, align 8
%fptrToInt54969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50182 to i64
%ae50182 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54969)
store volatile %struct.ScmObj* %ae50182, %struct.ScmObj** %stackaddr$makeclosure54968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50182, %struct.ScmObj* %_37append48118, i64 0)
%argslist54371$ae501800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%argslist54371$ae501801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50182, %struct.ScmObj* %argslist54371$ae501800)
store volatile %struct.ScmObj* %argslist54371$ae501801, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist54371$ae501802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50181, %struct.ScmObj* %argslist54371$ae501801)
store volatile %struct.ScmObj* %argslist54371$ae501802, %struct.ScmObj** %stackaddr$prim54971, align 8
%clofunc54972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50180)
musttail call tailcc void %clofunc54972(%struct.ScmObj* %ae50180, %struct.ScmObj* %argslist54371$ae501802)
ret void
}

define tailcc void @proc_clo$ae50180(%struct.ScmObj* %env$ae50180,%struct.ScmObj* %current_45args54193) {
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50180, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50180, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%current_45args54194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54193)
store volatile %struct.ScmObj* %current_45args54194, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54194)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim54977, align 8
%ae50248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50248, %struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim54978, align 8
%ae50251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54979 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50251)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim54979, align 8
%stackaddr$makeclosure54980 = alloca %struct.ScmObj*, align 8
%fptrToInt54981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50252 to i64
%ae50252 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54981)
store volatile %struct.ScmObj* %ae50252, %struct.ScmObj** %stackaddr$makeclosure54980, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50252, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54982 = alloca %struct.ScmObj*, align 8
%fptrToInt54983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50254 to i64
%ae50254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54983)
store volatile %struct.ScmObj* %ae50254, %struct.ScmObj** %stackaddr$makeclosure54982, align 8
%argslist54360$ae502520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%argslist54360$ae502521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50254, %struct.ScmObj* %argslist54360$ae502520)
store volatile %struct.ScmObj* %argslist54360$ae502521, %struct.ScmObj** %stackaddr$prim54984, align 8
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%argslist54360$ae502522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50253, %struct.ScmObj* %argslist54360$ae502521)
store volatile %struct.ScmObj* %argslist54360$ae502522, %struct.ScmObj** %stackaddr$prim54985, align 8
%clofunc54986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50252)
musttail call tailcc void %clofunc54986(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist54360$ae502522)
ret void
}

define tailcc void @proc_clo$ae50252(%struct.ScmObj* %env$ae50252,%struct.ScmObj* %current_45args54196) {
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50252, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%current_45args54197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %current_45args54197, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54197)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$makeclosure54991 = alloca %struct.ScmObj*, align 8
%fptrToInt54992 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50668 to i64
%ae50668 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54992)
store volatile %struct.ScmObj* %ae50668, %struct.ScmObj** %stackaddr$makeclosure54991, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50668, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50669 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54993 = alloca %struct.ScmObj*, align 8
%fptrToInt54994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50670 to i64
%ae50670 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54994)
store volatile %struct.ScmObj* %ae50670, %struct.ScmObj** %stackaddr$makeclosure54993, align 8
%argslist54335$ae506680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%argslist54335$ae506681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50670, %struct.ScmObj* %argslist54335$ae506680)
store volatile %struct.ScmObj* %argslist54335$ae506681, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%argslist54335$ae506682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50669, %struct.ScmObj* %argslist54335$ae506681)
store volatile %struct.ScmObj* %argslist54335$ae506682, %struct.ScmObj** %stackaddr$prim54996, align 8
%clofunc54997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50668)
musttail call tailcc void %clofunc54997(%struct.ScmObj* %ae50668, %struct.ScmObj* %argslist54335$ae506682)
ret void
}

define tailcc void @proc_clo$ae50668(%struct.ScmObj* %env$ae50668,%struct.ScmObj* %current_45args54199) {
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50668, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args54200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %current_45args54200, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55001, align 8
%stackaddr$makeclosure55002 = alloca %struct.ScmObj*, align 8
%fptrToInt55003 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51204 to i64
%ae51204 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55003)
store volatile %struct.ScmObj* %ae51204, %struct.ScmObj** %stackaddr$makeclosure55002, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51204, %struct.ScmObj* %_37foldl148030, i64 0)
%ae51205 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55004 = alloca %struct.ScmObj*, align 8
%fptrToInt55005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51206 to i64
%ae51206 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55005)
store volatile %struct.ScmObj* %ae51206, %struct.ScmObj** %stackaddr$makeclosure55004, align 8
%argslist54311$ae512040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%argslist54311$ae512041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51206, %struct.ScmObj* %argslist54311$ae512040)
store volatile %struct.ScmObj* %argslist54311$ae512041, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%argslist54311$ae512042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51205, %struct.ScmObj* %argslist54311$ae512041)
store volatile %struct.ScmObj* %argslist54311$ae512042, %struct.ScmObj** %stackaddr$prim55007, align 8
%clofunc55008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51204)
musttail call tailcc void %clofunc55008(%struct.ScmObj* %ae51204, %struct.ScmObj* %argslist54311$ae512042)
ret void
}

define tailcc void @proc_clo$ae51204(%struct.ScmObj* %env$ae51204,%struct.ScmObj* %current_45args54202) {
%stackaddr$env-ref55009 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51204, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55009
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%current_45args54203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %current_45args54203, %struct.ScmObj** %stackaddr$prim55011, align 8
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$makeclosure55013 = alloca %struct.ScmObj*, align 8
%fptrToInt55014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51608 to i64
%ae51608 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55014)
store volatile %struct.ScmObj* %ae51608, %struct.ScmObj** %stackaddr$makeclosure55013, align 8
%ae51609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55015 = alloca %struct.ScmObj*, align 8
%fptrToInt55016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51610 to i64
%ae51610 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55016)
store volatile %struct.ScmObj* %ae51610, %struct.ScmObj** %stackaddr$makeclosure55015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51610, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54285$ae516080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%argslist54285$ae516081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51610, %struct.ScmObj* %argslist54285$ae516080)
store volatile %struct.ScmObj* %argslist54285$ae516081, %struct.ScmObj** %stackaddr$prim55017, align 8
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%argslist54285$ae516082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51609, %struct.ScmObj* %argslist54285$ae516081)
store volatile %struct.ScmObj* %argslist54285$ae516082, %struct.ScmObj** %stackaddr$prim55018, align 8
%clofunc55019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51608)
musttail call tailcc void %clofunc55019(%struct.ScmObj* %ae51608, %struct.ScmObj* %argslist54285$ae516082)
ret void
}

define tailcc void @proc_clo$ae51608(%struct.ScmObj* %env$ae51608,%struct.ScmObj* %current_45args54205) {
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%current_45args54206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %current_45args54206, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54206)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$makeclosure55023 = alloca %struct.ScmObj*, align 8
%fptrToInt55024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51706 to i64
%ae51706 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55024)
store volatile %struct.ScmObj* %ae51706, %struct.ScmObj** %stackaddr$makeclosure55023, align 8
%ae51707 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55025 = alloca %struct.ScmObj*, align 8
%fptrToInt55026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51708 to i64
%ae51708 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55026)
store volatile %struct.ScmObj* %ae51708, %struct.ScmObj** %stackaddr$makeclosure55025, align 8
%argslist54272$ae517060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%argslist54272$ae517061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51708, %struct.ScmObj* %argslist54272$ae517060)
store volatile %struct.ScmObj* %argslist54272$ae517061, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%argslist54272$ae517062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51707, %struct.ScmObj* %argslist54272$ae517061)
store volatile %struct.ScmObj* %argslist54272$ae517062, %struct.ScmObj** %stackaddr$prim55028, align 8
%clofunc55029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51706)
musttail call tailcc void %clofunc55029(%struct.ScmObj* %ae51706, %struct.ScmObj* %argslist54272$ae517062)
ret void
}

define tailcc void @proc_clo$ae51706(%struct.ScmObj* %env$ae51706,%struct.ScmObj* %current_45args54208) {
%stackaddr$prim55030 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55030, align 8
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%current_45args54209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %current_45args54209, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54209)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$makeclosure55033 = alloca %struct.ScmObj*, align 8
%fptrToInt55034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51726 to i64
%ae51726 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55034)
store volatile %struct.ScmObj* %ae51726, %struct.ScmObj** %stackaddr$makeclosure55033, align 8
%ae51727 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55035 = alloca %struct.ScmObj*, align 8
%fptrToInt55036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51728 to i64
%ae51728 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55036)
store volatile %struct.ScmObj* %ae51728, %struct.ScmObj** %stackaddr$makeclosure55035, align 8
%argslist54267$ae517260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%argslist54267$ae517261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51728, %struct.ScmObj* %argslist54267$ae517260)
store volatile %struct.ScmObj* %argslist54267$ae517261, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%argslist54267$ae517262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51727, %struct.ScmObj* %argslist54267$ae517261)
store volatile %struct.ScmObj* %argslist54267$ae517262, %struct.ScmObj** %stackaddr$prim55038, align 8
%clofunc55039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51726)
musttail call tailcc void %clofunc55039(%struct.ScmObj* %ae51726, %struct.ScmObj* %argslist54267$ae517262)
ret void
}

define tailcc void @proc_clo$ae51726(%struct.ScmObj* %env$ae51726,%struct.ScmObj* %current_45args54211) {
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%current_45args54212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %current_45args54212, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54212)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$makeclosure55043 = alloca %struct.ScmObj*, align 8
%fptrToInt55044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51748 to i64
%ae51748 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55044)
store volatile %struct.ScmObj* %ae51748, %struct.ScmObj** %stackaddr$makeclosure55043, align 8
%ae51749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55045 = alloca %struct.ScmObj*, align 8
%fptrToInt55046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51750 to i64
%ae51750 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55046)
store volatile %struct.ScmObj* %ae51750, %struct.ScmObj** %stackaddr$makeclosure55045, align 8
%argslist54262$ae517480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%argslist54262$ae517481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51750, %struct.ScmObj* %argslist54262$ae517480)
store volatile %struct.ScmObj* %argslist54262$ae517481, %struct.ScmObj** %stackaddr$prim55047, align 8
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%argslist54262$ae517482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51749, %struct.ScmObj* %argslist54262$ae517481)
store volatile %struct.ScmObj* %argslist54262$ae517482, %struct.ScmObj** %stackaddr$prim55048, align 8
%clofunc55049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51748)
musttail call tailcc void %clofunc55049(%struct.ScmObj* %ae51748, %struct.ScmObj* %argslist54262$ae517482)
ret void
}

define tailcc void @proc_clo$ae51748(%struct.ScmObj* %env$ae51748,%struct.ScmObj* %current_45args54214) {
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%current_45args54215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %current_45args54215, %struct.ScmObj** %stackaddr$prim55051, align 8
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$makeclosure55053 = alloca %struct.ScmObj*, align 8
%fptrToInt55054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51772 to i64
%ae51772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55054)
store volatile %struct.ScmObj* %ae51772, %struct.ScmObj** %stackaddr$makeclosure55053, align 8
%ae51773 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55055 = alloca %struct.ScmObj*, align 8
%fptrToInt55056 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51774 to i64
%ae51774 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55056)
store volatile %struct.ScmObj* %ae51774, %struct.ScmObj** %stackaddr$makeclosure55055, align 8
%argslist54257$ae517720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%argslist54257$ae517721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51774, %struct.ScmObj* %argslist54257$ae517720)
store volatile %struct.ScmObj* %argslist54257$ae517721, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%argslist54257$ae517722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51773, %struct.ScmObj* %argslist54257$ae517721)
store volatile %struct.ScmObj* %argslist54257$ae517722, %struct.ScmObj** %stackaddr$prim55058, align 8
%clofunc55059 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51772)
musttail call tailcc void %clofunc55059(%struct.ScmObj* %ae51772, %struct.ScmObj* %argslist54257$ae517722)
ret void
}

define tailcc void @proc_clo$ae51772(%struct.ScmObj* %env$ae51772,%struct.ScmObj* %current_45args54217) {
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim55061, align 8
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55062, align 8
%ae51798 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51799 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae51798, %struct.ScmObj* %ae51799)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55063, align 8
%truthy$cmp55064 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48255)
%cmp$cmp55064 = icmp eq i64 %truthy$cmp55064, 1
br i1 %cmp$cmp55064, label %truebranch$cmp55064, label %falsebranch$cmp55064
truebranch$cmp55064:
%stackaddr$makeclosure55065 = alloca %struct.ScmObj*, align 8
%fptrToInt55066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51801 to i64
%ae51801 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55066)
store volatile %struct.ScmObj* %ae51801, %struct.ScmObj** %stackaddr$makeclosure55065, align 8
%ae51802 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51803 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54224$ae518010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%argslist54224$ae518011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51803, %struct.ScmObj* %argslist54224$ae518010)
store volatile %struct.ScmObj* %argslist54224$ae518011, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%argslist54224$ae518012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51802, %struct.ScmObj* %argslist54224$ae518011)
store volatile %struct.ScmObj* %argslist54224$ae518012, %struct.ScmObj** %stackaddr$prim55068, align 8
%clofunc55069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51801)
musttail call tailcc void %clofunc55069(%struct.ScmObj* %ae51801, %struct.ScmObj* %argslist54224$ae518012)
ret void
falsebranch$cmp55064:
%ae51816 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51817 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55070 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae51816, %struct.ScmObj* %ae51817)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim55070, align 8
%truthy$cmp55071 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48256)
%cmp$cmp55071 = icmp eq i64 %truthy$cmp55071, 1
br i1 %cmp$cmp55071, label %truebranch$cmp55071, label %falsebranch$cmp55071
truebranch$cmp55071:
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim55072, align 8
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_void_63(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim55073, align 8
%truthy$cmp55074 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48258)
%cmp$cmp55074 = icmp eq i64 %truthy$cmp55074, 1
br i1 %cmp$cmp55074, label %truebranch$cmp55074, label %falsebranch$cmp55074
truebranch$cmp55074:
%ae51821 = call %struct.ScmObj* @const_init_true()
%truthy$cmp55075 = call i64 @is_truthy_value(%struct.ScmObj* %ae51821)
%cmp$cmp55075 = icmp eq i64 %truthy$cmp55075, 1
br i1 %cmp$cmp55075, label %truebranch$cmp55075, label %falsebranch$cmp55075
truebranch$cmp55075:
%stackaddr$makeclosure55076 = alloca %struct.ScmObj*, align 8
%fptrToInt55077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51822 to i64
%ae51822 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55077)
store volatile %struct.ScmObj* %ae51822, %struct.ScmObj** %stackaddr$makeclosure55076, align 8
%ae51823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55078 = alloca %struct.ScmObj*, align 8
%fptrToInt55079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51824 to i64
%ae51824 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55079)
store volatile %struct.ScmObj* %ae51824, %struct.ScmObj** %stackaddr$makeclosure55078, align 8
%argslist54237$ae518220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%argslist54237$ae518221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51824, %struct.ScmObj* %argslist54237$ae518220)
store volatile %struct.ScmObj* %argslist54237$ae518221, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%argslist54237$ae518222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51823, %struct.ScmObj* %argslist54237$ae518221)
store volatile %struct.ScmObj* %argslist54237$ae518222, %struct.ScmObj** %stackaddr$prim55081, align 8
%clofunc55082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51822)
musttail call tailcc void %clofunc55082(%struct.ScmObj* %ae51822, %struct.ScmObj* %argslist54237$ae518222)
ret void
falsebranch$cmp55075:
%stackaddr$makeclosure55083 = alloca %struct.ScmObj*, align 8
%fptrToInt55084 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51858 to i64
%ae51858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55084)
store volatile %struct.ScmObj* %ae51858, %struct.ScmObj** %stackaddr$makeclosure55083, align 8
%ae51859 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51860 = call %struct.ScmObj* @const_init_int(i64 5)
%argslist54242$ae518580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%argslist54242$ae518581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51860, %struct.ScmObj* %argslist54242$ae518580)
store volatile %struct.ScmObj* %argslist54242$ae518581, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%argslist54242$ae518582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51859, %struct.ScmObj* %argslist54242$ae518581)
store volatile %struct.ScmObj* %argslist54242$ae518582, %struct.ScmObj** %stackaddr$prim55086, align 8
%clofunc55087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51858)
musttail call tailcc void %clofunc55087(%struct.ScmObj* %ae51858, %struct.ScmObj* %argslist54242$ae518582)
ret void
falsebranch$cmp55074:
%stackaddr$makeclosure55088 = alloca %struct.ScmObj*, align 8
%fptrToInt55089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51892 to i64
%ae51892 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55089)
store volatile %struct.ScmObj* %ae51892, %struct.ScmObj** %stackaddr$makeclosure55088, align 8
%ae51893 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51894 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist54247$ae518920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55090 = alloca %struct.ScmObj*, align 8
%argslist54247$ae518921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51894, %struct.ScmObj* %argslist54247$ae518920)
store volatile %struct.ScmObj* %argslist54247$ae518921, %struct.ScmObj** %stackaddr$prim55090, align 8
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%argslist54247$ae518922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51893, %struct.ScmObj* %argslist54247$ae518921)
store volatile %struct.ScmObj* %argslist54247$ae518922, %struct.ScmObj** %stackaddr$prim55091, align 8
%clofunc55092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51892)
musttail call tailcc void %clofunc55092(%struct.ScmObj* %ae51892, %struct.ScmObj* %argslist54247$ae518922)
ret void
falsebranch$cmp55071:
%stackaddr$makeclosure55093 = alloca %struct.ScmObj*, align 8
%fptrToInt55094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51907 to i64
%ae51907 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55094)
store volatile %struct.ScmObj* %ae51907, %struct.ScmObj** %stackaddr$makeclosure55093, align 8
%ae51908 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51909 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist54252$ae519070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%argslist54252$ae519071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51909, %struct.ScmObj* %argslist54252$ae519070)
store volatile %struct.ScmObj* %argslist54252$ae519071, %struct.ScmObj** %stackaddr$prim55095, align 8
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%argslist54252$ae519072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51908, %struct.ScmObj* %argslist54252$ae519071)
store volatile %struct.ScmObj* %argslist54252$ae519072, %struct.ScmObj** %stackaddr$prim55096, align 8
%clofunc55097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51907)
musttail call tailcc void %clofunc55097(%struct.ScmObj* %ae51907, %struct.ScmObj* %argslist54252$ae519072)
ret void
}

define tailcc void @proc_clo$ae51801(%struct.ScmObj* %env$ae51801,%struct.ScmObj* %current_45args54220) {
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%current_45args54221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54220)
store volatile %struct.ScmObj* %current_45args54221, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55101, align 8
%argslist54223$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%argslist54223$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54223$k0)
store volatile %struct.ScmObj* %argslist54223$k1, %struct.ScmObj** %stackaddr$prim55102, align 8
%clofunc55103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55103(%struct.ScmObj* %k, %struct.ScmObj* %argslist54223$k1)
ret void
}

define tailcc void @proc_clo$ae51822(%struct.ScmObj* %env$ae51822,%struct.ScmObj* %current_45args54225) {
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%current_45args54226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %current_45args54226, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim55106, align 8
%stackaddr$makeclosure55107 = alloca %struct.ScmObj*, align 8
%fptrToInt55108 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51841 to i64
%ae51841 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55108)
store volatile %struct.ScmObj* %ae51841, %struct.ScmObj** %stackaddr$makeclosure55107, align 8
%ae51842 = call %struct.ScmObj* @const_init_int(i64 11)
%argslist54232$anf_45bind482590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%argslist54232$anf_45bind482591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51842, %struct.ScmObj* %argslist54232$anf_45bind482590)
store volatile %struct.ScmObj* %argslist54232$anf_45bind482591, %struct.ScmObj** %stackaddr$prim55109, align 8
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%argslist54232$anf_45bind482592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51841, %struct.ScmObj* %argslist54232$anf_45bind482591)
store volatile %struct.ScmObj* %argslist54232$anf_45bind482592, %struct.ScmObj** %stackaddr$prim55110, align 8
%clofunc55111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48259)
musttail call tailcc void %clofunc55111(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %argslist54232$anf_45bind482592)
ret void
}

define tailcc void @proc_clo$ae51841(%struct.ScmObj* %env$ae51841,%struct.ScmObj* %current_45args54228) {
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54228)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%current_45args54229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54228)
store volatile %struct.ScmObj* %current_45args54229, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55114, align 8
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55115, align 8
%argslist54231$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%argslist54231$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54231$k0)
store volatile %struct.ScmObj* %argslist54231$k1, %struct.ScmObj** %stackaddr$prim55116, align 8
%clofunc55117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55117(%struct.ScmObj* %k, %struct.ScmObj* %argslist54231$k1)
ret void
}

define tailcc void @proc_clo$ae51824(%struct.ScmObj* %env$ae51824,%struct.ScmObj* %current_45args54233) {
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54233)
store volatile %struct.ScmObj* %k48293, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%current_45args54234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54233)
store volatile %struct.ScmObj* %current_45args54234, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%a48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54234)
store volatile %struct.ScmObj* %a48143, %struct.ScmObj** %stackaddr$prim55120, align 8
%ae51826 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54236$k482930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%argslist54236$k482931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48143, %struct.ScmObj* %argslist54236$k482930)
store volatile %struct.ScmObj* %argslist54236$k482931, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%argslist54236$k482932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51826, %struct.ScmObj* %argslist54236$k482931)
store volatile %struct.ScmObj* %argslist54236$k482932, %struct.ScmObj** %stackaddr$prim55122, align 8
%clofunc55123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48293)
musttail call tailcc void %clofunc55123(%struct.ScmObj* %k48293, %struct.ScmObj* %argslist54236$k482932)
ret void
}

define tailcc void @proc_clo$ae51858(%struct.ScmObj* %env$ae51858,%struct.ScmObj* %current_45args54238) {
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%current_45args54239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %current_45args54239, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54239)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55127, align 8
%argslist54241$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%argslist54241$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54241$k0)
store volatile %struct.ScmObj* %argslist54241$k1, %struct.ScmObj** %stackaddr$prim55128, align 8
%clofunc55129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55129(%struct.ScmObj* %k, %struct.ScmObj* %argslist54241$k1)
ret void
}

define tailcc void @proc_clo$ae51892(%struct.ScmObj* %env$ae51892,%struct.ScmObj* %current_45args54243) {
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%current_45args54244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %current_45args54244, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54244)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55133, align 8
%argslist54246$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%argslist54246$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54246$k0)
store volatile %struct.ScmObj* %argslist54246$k1, %struct.ScmObj** %stackaddr$prim55134, align 8
%clofunc55135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55135(%struct.ScmObj* %k, %struct.ScmObj* %argslist54246$k1)
ret void
}

define tailcc void @proc_clo$ae51907(%struct.ScmObj* %env$ae51907,%struct.ScmObj* %current_45args54248) {
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55136, align 8
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%current_45args54249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %current_45args54249, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55139, align 8
%argslist54251$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%argslist54251$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54251$k0)
store volatile %struct.ScmObj* %argslist54251$k1, %struct.ScmObj** %stackaddr$prim55140, align 8
%clofunc55141 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55141(%struct.ScmObj* %k, %struct.ScmObj* %argslist54251$k1)
ret void
}

define tailcc void @proc_clo$ae51774(%struct.ScmObj* %env$ae51774,%struct.ScmObj* %current_45args54253) {
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54253)
store volatile %struct.ScmObj* %k48294, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%current_45args54254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54253)
store volatile %struct.ScmObj* %current_45args54254, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%cpsprim48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %cpsprim48295, %struct.ScmObj** %stackaddr$prim55148, align 8
%ae51780 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54256$k482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%argslist54256$k482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48295, %struct.ScmObj* %argslist54256$k482940)
store volatile %struct.ScmObj* %argslist54256$k482941, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%argslist54256$k482942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51780, %struct.ScmObj* %argslist54256$k482941)
store volatile %struct.ScmObj* %argslist54256$k482942, %struct.ScmObj** %stackaddr$prim55150, align 8
%clofunc55151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48294)
musttail call tailcc void %clofunc55151(%struct.ScmObj* %k48294, %struct.ScmObj* %argslist54256$k482942)
ret void
}

define tailcc void @proc_clo$ae51750(%struct.ScmObj* %env$ae51750,%struct.ScmObj* %current_45args54258) {
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54258)
store volatile %struct.ScmObj* %k48296, %struct.ScmObj** %stackaddr$prim55152, align 8
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%current_45args54259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54258)
store volatile %struct.ScmObj* %current_45args54259, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%cpsprim48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %cpsprim48297, %struct.ScmObj** %stackaddr$prim55157, align 8
%ae51755 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54261$k482960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%argslist54261$k482961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48297, %struct.ScmObj* %argslist54261$k482960)
store volatile %struct.ScmObj* %argslist54261$k482961, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%argslist54261$k482962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51755, %struct.ScmObj* %argslist54261$k482961)
store volatile %struct.ScmObj* %argslist54261$k482962, %struct.ScmObj** %stackaddr$prim55159, align 8
%clofunc55160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48296)
musttail call tailcc void %clofunc55160(%struct.ScmObj* %k48296, %struct.ScmObj* %argslist54261$k482962)
ret void
}

define tailcc void @proc_clo$ae51728(%struct.ScmObj* %env$ae51728,%struct.ScmObj* %current_45args54263) {
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %k48298, %struct.ScmObj** %stackaddr$prim55161, align 8
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%current_45args54264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %current_45args54264, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54264)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%cpsprim48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %cpsprim48299, %struct.ScmObj** %stackaddr$prim55165, align 8
%ae51732 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54266$k482980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%argslist54266$k482981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48299, %struct.ScmObj* %argslist54266$k482980)
store volatile %struct.ScmObj* %argslist54266$k482981, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%argslist54266$k482982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51732, %struct.ScmObj* %argslist54266$k482981)
store volatile %struct.ScmObj* %argslist54266$k482982, %struct.ScmObj** %stackaddr$prim55167, align 8
%clofunc55168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48298)
musttail call tailcc void %clofunc55168(%struct.ScmObj* %k48298, %struct.ScmObj* %argslist54266$k482982)
ret void
}

define tailcc void @proc_clo$ae51708(%struct.ScmObj* %env$ae51708,%struct.ScmObj* %current_45args54268) {
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %k48300, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%current_45args54269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %current_45args54269, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54269)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%cpsprim48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48301, %struct.ScmObj** %stackaddr$prim55172, align 8
%ae51711 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54271$k483000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%argslist54271$k483001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48301, %struct.ScmObj* %argslist54271$k483000)
store volatile %struct.ScmObj* %argslist54271$k483001, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%argslist54271$k483002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51711, %struct.ScmObj* %argslist54271$k483001)
store volatile %struct.ScmObj* %argslist54271$k483002, %struct.ScmObj** %stackaddr$prim55174, align 8
%clofunc55175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48300)
musttail call tailcc void %clofunc55175(%struct.ScmObj* %k48300, %struct.ScmObj* %argslist54271$k483002)
ret void
}

define tailcc void @proc_clo$ae51610(%struct.ScmObj* %env$ae51610,%struct.ScmObj* %args4809148302) {
%stackaddr$env-ref55176 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51610, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55176
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148302)
store volatile %struct.ScmObj* %k48303, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148302)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim55179, align 8
%truthy$cmp55180 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48243)
%cmp$cmp55180 = icmp eq i64 %truthy$cmp55180, 1
br i1 %cmp$cmp55180, label %truebranch$cmp55180, label %falsebranch$cmp55180
truebranch$cmp55180:
%ae51616 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51617 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54273$k483030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%argslist54273$k483031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51617, %struct.ScmObj* %argslist54273$k483030)
store volatile %struct.ScmObj* %argslist54273$k483031, %struct.ScmObj** %stackaddr$prim55181, align 8
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%argslist54273$k483032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51616, %struct.ScmObj* %argslist54273$k483031)
store volatile %struct.ScmObj* %argslist54273$k483032, %struct.ScmObj** %stackaddr$prim55182, align 8
%clofunc55183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48303)
musttail call tailcc void %clofunc55183(%struct.ScmObj* %k48303, %struct.ScmObj* %argslist54273$k483032)
ret void
falsebranch$cmp55180:
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim55185, align 8
%truthy$cmp55186 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48245)
%cmp$cmp55186 = icmp eq i64 %truthy$cmp55186, 1
br i1 %cmp$cmp55186, label %truebranch$cmp55186, label %falsebranch$cmp55186
truebranch$cmp55186:
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%cpsprim48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48304, %struct.ScmObj** %stackaddr$prim55187, align 8
%ae51629 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54274$k483030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%argslist54274$k483031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48304, %struct.ScmObj* %argslist54274$k483030)
store volatile %struct.ScmObj* %argslist54274$k483031, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%argslist54274$k483032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51629, %struct.ScmObj* %argslist54274$k483031)
store volatile %struct.ScmObj* %argslist54274$k483032, %struct.ScmObj** %stackaddr$prim55189, align 8
%clofunc55190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48303)
musttail call tailcc void %clofunc55190(%struct.ScmObj* %k48303, %struct.ScmObj* %argslist54274$k483032)
ret void
falsebranch$cmp55186:
%stackaddr$makeclosure55191 = alloca %struct.ScmObj*, align 8
%fptrToInt55192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51634 to i64
%ae51634 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55192)
store volatile %struct.ScmObj* %ae51634, %struct.ScmObj** %stackaddr$makeclosure55191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51634, %struct.ScmObj* %k48303, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51634, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51634, %struct.ScmObj* %args48091, i64 2)
%ae51635 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55193 = alloca %struct.ScmObj*, align 8
%fptrToInt55194 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51636 to i64
%ae51636 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55194)
store volatile %struct.ScmObj* %ae51636, %struct.ScmObj** %stackaddr$makeclosure55193, align 8
%argslist54284$ae516340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%argslist54284$ae516341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51636, %struct.ScmObj* %argslist54284$ae516340)
store volatile %struct.ScmObj* %argslist54284$ae516341, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%argslist54284$ae516342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51635, %struct.ScmObj* %argslist54284$ae516341)
store volatile %struct.ScmObj* %argslist54284$ae516342, %struct.ScmObj** %stackaddr$prim55196, align 8
%clofunc55197 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51634)
musttail call tailcc void %clofunc55197(%struct.ScmObj* %ae51634, %struct.ScmObj* %argslist54284$ae516342)
ret void
}

define tailcc void @proc_clo$ae51634(%struct.ScmObj* %env$ae51634,%struct.ScmObj* %current_45args54275) {
%stackaddr$env-ref55198 = alloca %struct.ScmObj*, align 8
%k48303 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51634, i64 0)
store %struct.ScmObj* %k48303, %struct.ScmObj** %stackaddr$env-ref55198
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51634, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$env-ref55200 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51634, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref55200
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%current_45args54276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %current_45args54276, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54276)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim55203, align 8
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim55205, align 8
%argslist54278$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%argslist54278$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %argslist54278$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54278$_37foldl1480301, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%argslist54278$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48247, %struct.ScmObj* %argslist54278$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54278$_37foldl1480302, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist54278$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48246, %struct.ScmObj* %argslist54278$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54278$_37foldl1480303, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist54278$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48303, %struct.ScmObj* %argslist54278$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54278$_37foldl1480304, %struct.ScmObj** %stackaddr$prim55209, align 8
%clofunc55210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc55210(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54278$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51636(%struct.ScmObj* %env$ae51636,%struct.ScmObj* %current_45args54279) {
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %k48306, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%current_45args54280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %current_45args54280, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%current_45args54281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %current_45args54281, %struct.ScmObj** %stackaddr$prim55214, align 8
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54281)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%cpsprim48307 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48307, %struct.ScmObj** %stackaddr$prim55216, align 8
%ae51640 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54283$k483060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%argslist54283$k483061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48307, %struct.ScmObj* %argslist54283$k483060)
store volatile %struct.ScmObj* %argslist54283$k483061, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%argslist54283$k483062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51640, %struct.ScmObj* %argslist54283$k483061)
store volatile %struct.ScmObj* %argslist54283$k483062, %struct.ScmObj** %stackaddr$prim55218, align 8
%clofunc55219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48306)
musttail call tailcc void %clofunc55219(%struct.ScmObj* %k48306, %struct.ScmObj* %argslist54283$k483062)
ret void
}

define tailcc void @proc_clo$ae51206(%struct.ScmObj* %env$ae51206,%struct.ScmObj* %current_45args54286) {
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %k48308, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%current_45args54287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %current_45args54287, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%current_45args54288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %current_45args54288, %struct.ScmObj** %stackaddr$prim55223, align 8
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim55224, align 8
%ae51207 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51207, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$makeclosure55226 = alloca %struct.ScmObj*, align 8
%fptrToInt55227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51209 to i64
%ae51209 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55227)
store volatile %struct.ScmObj* %ae51209, %struct.ScmObj** %stackaddr$makeclosure55226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51209, %struct.ScmObj* %k48308, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51209, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51209, %struct.ScmObj* %v48096, i64 2)
%ae51210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55228 = alloca %struct.ScmObj*, align 8
%fptrToInt55229 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51211 to i64
%ae51211 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55229)
store volatile %struct.ScmObj* %ae51211, %struct.ScmObj** %stackaddr$makeclosure55228, align 8
%argslist54310$ae512090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%argslist54310$ae512091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51211, %struct.ScmObj* %argslist54310$ae512090)
store volatile %struct.ScmObj* %argslist54310$ae512091, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$prim55231 = alloca %struct.ScmObj*, align 8
%argslist54310$ae512092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51210, %struct.ScmObj* %argslist54310$ae512091)
store volatile %struct.ScmObj* %argslist54310$ae512092, %struct.ScmObj** %stackaddr$prim55231, align 8
%clofunc55232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51209)
musttail call tailcc void %clofunc55232(%struct.ScmObj* %ae51209, %struct.ScmObj* %argslist54310$ae512092)
ret void
}

define tailcc void @proc_clo$ae51209(%struct.ScmObj* %env$ae51209,%struct.ScmObj* %current_45args54290) {
%stackaddr$env-ref55233 = alloca %struct.ScmObj*, align 8
%k48308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51209, i64 0)
store %struct.ScmObj* %k48308, %struct.ScmObj** %stackaddr$env-ref55233
%stackaddr$env-ref55234 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51209, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55234
%stackaddr$env-ref55235 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51209, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55235
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim55236, align 8
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%current_45args54291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %current_45args54291, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55238, align 8
%stackaddr$makeclosure55239 = alloca %struct.ScmObj*, align 8
%fptrToInt55240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51225 to i64
%ae51225 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55240)
store volatile %struct.ScmObj* %ae51225, %struct.ScmObj** %stackaddr$makeclosure55239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %k48308, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %v48096, i64 2)
%stackaddr$makeclosure55241 = alloca %struct.ScmObj*, align 8
%fptrToInt55242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51226 to i64
%ae51226 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55242)
store volatile %struct.ScmObj* %ae51226, %struct.ScmObj** %stackaddr$makeclosure55241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51226, %struct.ScmObj* %k48308, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51226, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51226, %struct.ScmObj* %v48096, i64 2)
%argslist54305$anf_45bind482350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist54305$anf_45bind482351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51226, %struct.ScmObj* %argslist54305$anf_45bind482350)
store volatile %struct.ScmObj* %argslist54305$anf_45bind482351, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%argslist54305$anf_45bind482352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51225, %struct.ScmObj* %argslist54305$anf_45bind482351)
store volatile %struct.ScmObj* %argslist54305$anf_45bind482352, %struct.ScmObj** %stackaddr$prim55244, align 8
%clofunc55245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48235)
musttail call tailcc void %clofunc55245(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %argslist54305$anf_45bind482352)
ret void
}

define tailcc void @proc_clo$ae51225(%struct.ScmObj* %env$ae51225,%struct.ScmObj* %current_45args54293) {
%stackaddr$env-ref55246 = alloca %struct.ScmObj*, align 8
%k48308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 0)
store %struct.ScmObj* %k48308, %struct.ScmObj** %stackaddr$env-ref55246
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%current_45args54294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %current_45args54294, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55251, align 8
%ae51334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51334)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55252, align 8
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55253, align 8
%truthy$cmp55254 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48237)
%cmp$cmp55254 = icmp eq i64 %truthy$cmp55254, 1
br i1 %cmp$cmp55254, label %truebranch$cmp55254, label %falsebranch$cmp55254
truebranch$cmp55254:
%ae51338 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51339 = call %struct.ScmObj* @const_init_false()
%argslist54296$k483080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%argslist54296$k483081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51339, %struct.ScmObj* %argslist54296$k483080)
store volatile %struct.ScmObj* %argslist54296$k483081, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist54296$k483082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51338, %struct.ScmObj* %argslist54296$k483081)
store volatile %struct.ScmObj* %argslist54296$k483082, %struct.ScmObj** %stackaddr$prim55256, align 8
%clofunc55257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48308)
musttail call tailcc void %clofunc55257(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54296$k483082)
ret void
falsebranch$cmp55254:
%ae51347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51347)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55260, align 8
%truthy$cmp55261 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48240)
%cmp$cmp55261 = icmp eq i64 %truthy$cmp55261, 1
br i1 %cmp$cmp55261, label %truebranch$cmp55261, label %falsebranch$cmp55261
truebranch$cmp55261:
%ae51353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%cpsprim48311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51353)
store volatile %struct.ScmObj* %cpsprim48311, %struct.ScmObj** %stackaddr$prim55262, align 8
%ae51355 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54297$k483080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%argslist54297$k483081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48311, %struct.ScmObj* %argslist54297$k483080)
store volatile %struct.ScmObj* %argslist54297$k483081, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%argslist54297$k483082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51355, %struct.ScmObj* %argslist54297$k483081)
store volatile %struct.ScmObj* %argslist54297$k483082, %struct.ScmObj** %stackaddr$prim55264, align 8
%clofunc55265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48308)
musttail call tailcc void %clofunc55265(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54297$k483082)
ret void
falsebranch$cmp55261:
%ae51366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51366)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55267, align 8
%ae51369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51369, %struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55268, align 8
%argslist54298$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%argslist54298$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54298$cc480980)
store volatile %struct.ScmObj* %argslist54298$cc480981, %struct.ScmObj** %stackaddr$prim55269, align 8
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%argslist54298$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54298$cc480981)
store volatile %struct.ScmObj* %argslist54298$cc480982, %struct.ScmObj** %stackaddr$prim55270, align 8
%clofunc55271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55271(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54298$cc480982)
ret void
}

define tailcc void @proc_clo$ae51226(%struct.ScmObj* %env$ae51226,%struct.ScmObj* %current_45args54299) {
%stackaddr$env-ref55272 = alloca %struct.ScmObj*, align 8
%k48308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51226, i64 0)
store %struct.ScmObj* %k48308, %struct.ScmObj** %stackaddr$env-ref55272
%stackaddr$env-ref55273 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51226, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref55273
%stackaddr$env-ref55274 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51226, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref55274
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55275, align 8
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%current_45args54300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %current_45args54300, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim55277, align 8
%ae51228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51228)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55279, align 8
%truthy$cmp55280 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48237)
%cmp$cmp55280 = icmp eq i64 %truthy$cmp55280, 1
br i1 %cmp$cmp55280, label %truebranch$cmp55280, label %falsebranch$cmp55280
truebranch$cmp55280:
%ae51232 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51233 = call %struct.ScmObj* @const_init_false()
%argslist54302$k483080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist54302$k483081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51233, %struct.ScmObj* %argslist54302$k483080)
store volatile %struct.ScmObj* %argslist54302$k483081, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%argslist54302$k483082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51232, %struct.ScmObj* %argslist54302$k483081)
store volatile %struct.ScmObj* %argslist54302$k483082, %struct.ScmObj** %stackaddr$prim55282, align 8
%clofunc55283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48308)
musttail call tailcc void %clofunc55283(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54302$k483082)
ret void
falsebranch$cmp55280:
%ae51241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51241)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim55286, align 8
%truthy$cmp55287 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48240)
%cmp$cmp55287 = icmp eq i64 %truthy$cmp55287, 1
br i1 %cmp$cmp55287, label %truebranch$cmp55287, label %falsebranch$cmp55287
truebranch$cmp55287:
%ae51247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%cpsprim48311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51247)
store volatile %struct.ScmObj* %cpsprim48311, %struct.ScmObj** %stackaddr$prim55288, align 8
%ae51249 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54303$k483080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%argslist54303$k483081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48311, %struct.ScmObj* %argslist54303$k483080)
store volatile %struct.ScmObj* %argslist54303$k483081, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%argslist54303$k483082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51249, %struct.ScmObj* %argslist54303$k483081)
store volatile %struct.ScmObj* %argslist54303$k483082, %struct.ScmObj** %stackaddr$prim55290, align 8
%clofunc55291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48308)
musttail call tailcc void %clofunc55291(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54303$k483082)
ret void
falsebranch$cmp55287:
%ae51260 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51260)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim55293, align 8
%ae51263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51263, %struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim55294, align 8
%argslist54304$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%argslist54304$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54304$cc480980)
store volatile %struct.ScmObj* %argslist54304$cc480981, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%argslist54304$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48308, %struct.ScmObj* %argslist54304$cc480981)
store volatile %struct.ScmObj* %argslist54304$cc480982, %struct.ScmObj** %stackaddr$prim55296, align 8
%clofunc55297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc55297(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54304$cc480982)
ret void
}

define tailcc void @proc_clo$ae51211(%struct.ScmObj* %env$ae51211,%struct.ScmObj* %current_45args54306) {
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$prim55298, align 8
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%current_45args54307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %current_45args54307, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54307)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim55300, align 8
%argslist54309$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55301 = alloca %struct.ScmObj*, align 8
%argslist54309$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54309$u480990)
store volatile %struct.ScmObj* %argslist54309$u480991, %struct.ScmObj** %stackaddr$prim55301, align 8
%stackaddr$prim55302 = alloca %struct.ScmObj*, align 8
%argslist54309$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist54309$u480991)
store volatile %struct.ScmObj* %argslist54309$u480992, %struct.ScmObj** %stackaddr$prim55302, align 8
%clofunc55303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc55303(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54309$u480992)
ret void
}

define tailcc void @proc_clo$ae50670(%struct.ScmObj* %env$ae50670,%struct.ScmObj* %current_45args54312) {
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54312)
store volatile %struct.ScmObj* %k48313, %struct.ScmObj** %stackaddr$prim55304, align 8
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%current_45args54313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54312)
store volatile %struct.ScmObj* %current_45args54313, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54313)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%current_45args54314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54313)
store volatile %struct.ScmObj* %current_45args54314, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim55308, align 8
%ae50671 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50671, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim55309, align 8
%ae50673 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50673, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$makeclosure55311 = alloca %struct.ScmObj*, align 8
%fptrToInt55312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50675 to i64
%ae50675 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55312)
store volatile %struct.ScmObj* %ae50675, %struct.ScmObj** %stackaddr$makeclosure55311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50675, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50675, %struct.ScmObj* %k48313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50675, %struct.ScmObj* %lst48104, i64 2)
%ae50676 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55313 = alloca %struct.ScmObj*, align 8
%fptrToInt55314 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50677 to i64
%ae50677 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55314)
store volatile %struct.ScmObj* %ae50677, %struct.ScmObj** %stackaddr$makeclosure55313, align 8
%argslist54334$ae506750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%argslist54334$ae506751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50677, %struct.ScmObj* %argslist54334$ae506750)
store volatile %struct.ScmObj* %argslist54334$ae506751, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%argslist54334$ae506752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50676, %struct.ScmObj* %argslist54334$ae506751)
store volatile %struct.ScmObj* %argslist54334$ae506752, %struct.ScmObj** %stackaddr$prim55316, align 8
%clofunc55317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50675)
musttail call tailcc void %clofunc55317(%struct.ScmObj* %ae50675, %struct.ScmObj* %argslist54334$ae506752)
ret void
}

define tailcc void @proc_clo$ae50675(%struct.ScmObj* %env$ae50675,%struct.ScmObj* %current_45args54316) {
%stackaddr$env-ref55318 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50675, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55318
%stackaddr$env-ref55319 = alloca %struct.ScmObj*, align 8
%k48313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50675, i64 1)
store %struct.ScmObj* %k48313, %struct.ScmObj** %stackaddr$env-ref55319
%stackaddr$env-ref55320 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50675, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55320
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%current_45args54317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %current_45args54317, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54317)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55323, align 8
%stackaddr$makeclosure55324 = alloca %struct.ScmObj*, align 8
%fptrToInt55325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50691 to i64
%ae50691 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55325)
store volatile %struct.ScmObj* %ae50691, %struct.ScmObj** %stackaddr$makeclosure55324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %k48313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %lst48104, i64 2)
%stackaddr$makeclosure55326 = alloca %struct.ScmObj*, align 8
%fptrToInt55327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50692 to i64
%ae50692 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55327)
store volatile %struct.ScmObj* %ae50692, %struct.ScmObj** %stackaddr$makeclosure55326, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50692, %struct.ScmObj* %n48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50692, %struct.ScmObj* %k48313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50692, %struct.ScmObj* %lst48104, i64 2)
%argslist54329$anf_45bind482280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%argslist54329$anf_45bind482281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50692, %struct.ScmObj* %argslist54329$anf_45bind482280)
store volatile %struct.ScmObj* %argslist54329$anf_45bind482281, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%argslist54329$anf_45bind482282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist54329$anf_45bind482281)
store volatile %struct.ScmObj* %argslist54329$anf_45bind482282, %struct.ScmObj** %stackaddr$prim55329, align 8
%clofunc55330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48228)
musttail call tailcc void %clofunc55330(%struct.ScmObj* %anf_45bind48228, %struct.ScmObj* %argslist54329$anf_45bind482282)
ret void
}

define tailcc void @proc_clo$ae50691(%struct.ScmObj* %env$ae50691,%struct.ScmObj* %current_45args54319) {
%stackaddr$env-ref55331 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55331
%stackaddr$env-ref55332 = alloca %struct.ScmObj*, align 8
%k48313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 1)
store %struct.ScmObj* %k48313, %struct.ScmObj** %stackaddr$env-ref55332
%stackaddr$env-ref55333 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55333
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%current_45args54320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %current_45args54320, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55336, align 8
%ae50834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50834)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55337, align 8
%ae50835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50835, %struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55338, align 8
%truthy$cmp55339 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48230)
%cmp$cmp55339 = icmp eq i64 %truthy$cmp55339, 1
br i1 %cmp$cmp55339, label %truebranch$cmp55339, label %falsebranch$cmp55339
truebranch$cmp55339:
%ae50839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%cpsprim48316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50839)
store volatile %struct.ScmObj* %cpsprim48316, %struct.ScmObj** %stackaddr$prim55340, align 8
%ae50841 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54322$k483130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%argslist54322$k483131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48316, %struct.ScmObj* %argslist54322$k483130)
store volatile %struct.ScmObj* %argslist54322$k483131, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%argslist54322$k483132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50841, %struct.ScmObj* %argslist54322$k483131)
store volatile %struct.ScmObj* %argslist54322$k483132, %struct.ScmObj** %stackaddr$prim55342, align 8
%clofunc55343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48313)
musttail call tailcc void %clofunc55343(%struct.ScmObj* %k48313, %struct.ScmObj* %argslist54322$k483132)
ret void
falsebranch$cmp55339:
%ae50852 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50852)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55345, align 8
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50855, %struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55346, align 8
%ae50858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50858)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55347, align 8
%ae50860 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48233, %struct.ScmObj* %ae50860)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55348, align 8
%ae50862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50862, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55349, align 8
%argslist54323$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%argslist54323$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54323$cc481060)
store volatile %struct.ScmObj* %argslist54323$cc481061, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%argslist54323$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48313, %struct.ScmObj* %argslist54323$cc481061)
store volatile %struct.ScmObj* %argslist54323$cc481062, %struct.ScmObj** %stackaddr$prim55351, align 8
%clofunc55352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55352(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54323$cc481062)
ret void
}

define tailcc void @proc_clo$ae50692(%struct.ScmObj* %env$ae50692,%struct.ScmObj* %current_45args54324) {
%stackaddr$env-ref55353 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50692, i64 0)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref55353
%stackaddr$env-ref55354 = alloca %struct.ScmObj*, align 8
%k48313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50692, i64 1)
store %struct.ScmObj* %k48313, %struct.ScmObj** %stackaddr$env-ref55354
%stackaddr$env-ref55355 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50692, i64 2)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref55355
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%current_45args54325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %current_45args54325, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim55358, align 8
%ae50694 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50694)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55359, align 8
%ae50695 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50695, %struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55360, align 8
%truthy$cmp55361 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48230)
%cmp$cmp55361 = icmp eq i64 %truthy$cmp55361, 1
br i1 %cmp$cmp55361, label %truebranch$cmp55361, label %falsebranch$cmp55361
truebranch$cmp55361:
%ae50699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%cpsprim48316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50699)
store volatile %struct.ScmObj* %cpsprim48316, %struct.ScmObj** %stackaddr$prim55362, align 8
%ae50701 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54327$k483130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%argslist54327$k483131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48316, %struct.ScmObj* %argslist54327$k483130)
store volatile %struct.ScmObj* %argslist54327$k483131, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist54327$k483132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50701, %struct.ScmObj* %argslist54327$k483131)
store volatile %struct.ScmObj* %argslist54327$k483132, %struct.ScmObj** %stackaddr$prim55364, align 8
%clofunc55365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48313)
musttail call tailcc void %clofunc55365(%struct.ScmObj* %k48313, %struct.ScmObj* %argslist54327$k483132)
ret void
falsebranch$cmp55361:
%ae50712 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50712)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim55367, align 8
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50715, %struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim55368, align 8
%ae50718 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50718)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55369, align 8
%ae50720 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48233, %struct.ScmObj* %ae50720)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55370, align 8
%ae50722 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50722, %struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim55371, align 8
%argslist54328$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%argslist54328$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54328$cc481060)
store volatile %struct.ScmObj* %argslist54328$cc481061, %struct.ScmObj** %stackaddr$prim55372, align 8
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%argslist54328$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48313, %struct.ScmObj* %argslist54328$cc481061)
store volatile %struct.ScmObj* %argslist54328$cc481062, %struct.ScmObj** %stackaddr$prim55373, align 8
%clofunc55374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc55374(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54328$cc481062)
ret void
}

define tailcc void @proc_clo$ae50677(%struct.ScmObj* %env$ae50677,%struct.ScmObj* %current_45args54330) {
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$prim55375, align 8
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%current_45args54331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %current_45args54331, %struct.ScmObj** %stackaddr$prim55376, align 8
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim55377, align 8
%argslist54333$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist54333$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54333$u481070)
store volatile %struct.ScmObj* %argslist54333$u481071, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%argslist54333$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist54333$u481071)
store volatile %struct.ScmObj* %argslist54333$u481072, %struct.ScmObj** %stackaddr$prim55379, align 8
%clofunc55380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc55380(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54333$u481072)
ret void
}

define tailcc void @proc_clo$ae50254(%struct.ScmObj* %env$ae50254,%struct.ScmObj* %current_45args54336) {
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %k48318, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%current_45args54337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %current_45args54337, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim55383, align 8
%ae50255 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50255, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$makeclosure55385 = alloca %struct.ScmObj*, align 8
%fptrToInt55386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50257 to i64
%ae50257 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55386)
store volatile %struct.ScmObj* %ae50257, %struct.ScmObj** %stackaddr$makeclosure55385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50257, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50257, %struct.ScmObj* %k48318, i64 1)
%ae50258 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55387 = alloca %struct.ScmObj*, align 8
%fptrToInt55388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50259 to i64
%ae50259 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55388)
store volatile %struct.ScmObj* %ae50259, %struct.ScmObj** %stackaddr$makeclosure55387, align 8
%argslist54359$ae502570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%argslist54359$ae502571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50259, %struct.ScmObj* %argslist54359$ae502570)
store volatile %struct.ScmObj* %argslist54359$ae502571, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist54359$ae502572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50258, %struct.ScmObj* %argslist54359$ae502571)
store volatile %struct.ScmObj* %argslist54359$ae502572, %struct.ScmObj** %stackaddr$prim55390, align 8
%clofunc55391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50257)
musttail call tailcc void %clofunc55391(%struct.ScmObj* %ae50257, %struct.ScmObj* %argslist54359$ae502572)
ret void
}

define tailcc void @proc_clo$ae50257(%struct.ScmObj* %env$ae50257,%struct.ScmObj* %current_45args54339) {
%stackaddr$env-ref55392 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50257, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55392
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%k48318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50257, i64 1)
store %struct.ScmObj* %k48318, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%current_45args54340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %current_45args54340, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$makeclosure55397 = alloca %struct.ScmObj*, align 8
%fptrToInt55398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50276 to i64
%ae50276 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55398)
store volatile %struct.ScmObj* %ae50276, %struct.ScmObj** %stackaddr$makeclosure55397, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50276, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50276, %struct.ScmObj* %k48318, i64 1)
%stackaddr$makeclosure55399 = alloca %struct.ScmObj*, align 8
%fptrToInt55400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50277 to i64
%ae50277 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55400)
store volatile %struct.ScmObj* %ae50277, %struct.ScmObj** %stackaddr$makeclosure55399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50277, %struct.ScmObj* %k48318, i64 1)
%argslist54354$anf_45bind482200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%argslist54354$anf_45bind482201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50277, %struct.ScmObj* %argslist54354$anf_45bind482200)
store volatile %struct.ScmObj* %argslist54354$anf_45bind482201, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist54354$anf_45bind482202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50276, %struct.ScmObj* %argslist54354$anf_45bind482201)
store volatile %struct.ScmObj* %argslist54354$anf_45bind482202, %struct.ScmObj** %stackaddr$prim55402, align 8
%clofunc55403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48220)
musttail call tailcc void %clofunc55403(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %argslist54354$anf_45bind482202)
ret void
}

define tailcc void @proc_clo$ae50276(%struct.ScmObj* %env$ae50276,%struct.ScmObj* %current_45args54342) {
%stackaddr$env-ref55404 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50276, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55404
%stackaddr$env-ref55405 = alloca %struct.ScmObj*, align 8
%k48318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50276, i64 1)
store %struct.ScmObj* %k48318, %struct.ScmObj** %stackaddr$env-ref55405
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%current_45args54343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %current_45args54343, %struct.ScmObj** %stackaddr$prim55407, align 8
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54343)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55408, align 8
%ae50392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50392)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55410, align 8
%truthy$cmp55411 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48222)
%cmp$cmp55411 = icmp eq i64 %truthy$cmp55411, 1
br i1 %cmp$cmp55411, label %truebranch$cmp55411, label %falsebranch$cmp55411
truebranch$cmp55411:
%ae50396 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50397 = call %struct.ScmObj* @const_init_true()
%argslist54345$k483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%argslist54345$k483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50397, %struct.ScmObj* %argslist54345$k483180)
store volatile %struct.ScmObj* %argslist54345$k483181, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$prim55413 = alloca %struct.ScmObj*, align 8
%argslist54345$k483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50396, %struct.ScmObj* %argslist54345$k483181)
store volatile %struct.ScmObj* %argslist54345$k483182, %struct.ScmObj** %stackaddr$prim55413, align 8
%clofunc55414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48318)
musttail call tailcc void %clofunc55414(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54345$k483182)
ret void
falsebranch$cmp55411:
%ae50405 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50405)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55416, align 8
%truthy$cmp55417 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48224)
%cmp$cmp55417 = icmp eq i64 %truthy$cmp55417, 1
br i1 %cmp$cmp55417, label %truebranch$cmp55417, label %falsebranch$cmp55417
truebranch$cmp55417:
%ae50409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50409)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55418, align 8
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55419, align 8
%ae50412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50412)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55421, align 8
%ae50415 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50415, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55422, align 8
%argslist54346$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%argslist54346$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54346$cc481130)
store volatile %struct.ScmObj* %argslist54346$cc481131, %struct.ScmObj** %stackaddr$prim55423, align 8
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%argslist54346$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54346$cc481131)
store volatile %struct.ScmObj* %argslist54346$cc481132, %struct.ScmObj** %stackaddr$prim55424, align 8
%clofunc55425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55425(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54346$cc481132)
ret void
falsebranch$cmp55417:
%ae50448 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50449 = call %struct.ScmObj* @const_init_false()
%argslist54347$k483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist54347$k483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50449, %struct.ScmObj* %argslist54347$k483180)
store volatile %struct.ScmObj* %argslist54347$k483181, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%argslist54347$k483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50448, %struct.ScmObj* %argslist54347$k483181)
store volatile %struct.ScmObj* %argslist54347$k483182, %struct.ScmObj** %stackaddr$prim55427, align 8
%clofunc55428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48318)
musttail call tailcc void %clofunc55428(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54347$k483182)
ret void
}

define tailcc void @proc_clo$ae50277(%struct.ScmObj* %env$ae50277,%struct.ScmObj* %current_45args54348) {
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%k48318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50277, i64 1)
store %struct.ScmObj* %k48318, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%current_45args54349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %current_45args54349, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim55433, align 8
%ae50279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50279)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55434, align 8
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55435, align 8
%truthy$cmp55436 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48222)
%cmp$cmp55436 = icmp eq i64 %truthy$cmp55436, 1
br i1 %cmp$cmp55436, label %truebranch$cmp55436, label %falsebranch$cmp55436
truebranch$cmp55436:
%ae50283 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50284 = call %struct.ScmObj* @const_init_true()
%argslist54351$k483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%argslist54351$k483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50284, %struct.ScmObj* %argslist54351$k483180)
store volatile %struct.ScmObj* %argslist54351$k483181, %struct.ScmObj** %stackaddr$prim55437, align 8
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%argslist54351$k483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50283, %struct.ScmObj* %argslist54351$k483181)
store volatile %struct.ScmObj* %argslist54351$k483182, %struct.ScmObj** %stackaddr$prim55438, align 8
%clofunc55439 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48318)
musttail call tailcc void %clofunc55439(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54351$k483182)
ret void
falsebranch$cmp55436:
%ae50292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50292)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55441, align 8
%truthy$cmp55442 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48224)
%cmp$cmp55442 = icmp eq i64 %truthy$cmp55442, 1
br i1 %cmp$cmp55442, label %truebranch$cmp55442, label %falsebranch$cmp55442
truebranch$cmp55442:
%ae50296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50296)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim55444, align 8
%ae50299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50299)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55446, align 8
%ae50302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50302, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim55447, align 8
%argslist54352$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%argslist54352$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54352$cc481130)
store volatile %struct.ScmObj* %argslist54352$cc481131, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%argslist54352$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54352$cc481131)
store volatile %struct.ScmObj* %argslist54352$cc481132, %struct.ScmObj** %stackaddr$prim55449, align 8
%clofunc55450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc55450(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54352$cc481132)
ret void
falsebranch$cmp55442:
%ae50335 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50336 = call %struct.ScmObj* @const_init_false()
%argslist54353$k483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%argslist54353$k483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50336, %struct.ScmObj* %argslist54353$k483180)
store volatile %struct.ScmObj* %argslist54353$k483181, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%argslist54353$k483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50335, %struct.ScmObj* %argslist54353$k483181)
store volatile %struct.ScmObj* %argslist54353$k483182, %struct.ScmObj** %stackaddr$prim55452, align 8
%clofunc55453 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48318)
musttail call tailcc void %clofunc55453(%struct.ScmObj* %k48318, %struct.ScmObj* %argslist54353$k483182)
ret void
}

define tailcc void @proc_clo$ae50259(%struct.ScmObj* %env$ae50259,%struct.ScmObj* %current_45args54355) {
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$prim55454, align 8
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%current_45args54356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54355)
store volatile %struct.ScmObj* %current_45args54356, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim55456, align 8
%ae50261 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54358$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%argslist54358$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist54358$k483210)
store volatile %struct.ScmObj* %argslist54358$k483211, %struct.ScmObj** %stackaddr$prim55457, align 8
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%argslist54358$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50261, %struct.ScmObj* %argslist54358$k483211)
store volatile %struct.ScmObj* %argslist54358$k483212, %struct.ScmObj** %stackaddr$prim55458, align 8
%clofunc55459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55459(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54358$k483212)
ret void
}

define tailcc void @proc_clo$ae50182(%struct.ScmObj* %env$ae50182,%struct.ScmObj* %current_45args54361) {
%stackaddr$env-ref55460 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50182, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55460
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54361)
store volatile %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$prim55461, align 8
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%current_45args54362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54361)
store volatile %struct.ScmObj* %current_45args54362, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%current_45args54363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %current_45args54363, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55466, align 8
%truthy$cmp55467 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48214)
%cmp$cmp55467 = icmp eq i64 %truthy$cmp55467, 1
br i1 %cmp$cmp55467, label %truebranch$cmp55467, label %falsebranch$cmp55467
truebranch$cmp55467:
%ae50186 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54365$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%argslist54365$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54365$k483220)
store volatile %struct.ScmObj* %argslist54365$k483221, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%argslist54365$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50186, %struct.ScmObj* %argslist54365$k483221)
store volatile %struct.ScmObj* %argslist54365$k483222, %struct.ScmObj** %stackaddr$prim55469, align 8
%clofunc55470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55470(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54365$k483222)
ret void
falsebranch$cmp55467:
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim55471, align 8
%ae50193 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50193)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim55472, align 8
%stackaddr$prim55473 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim55473, align 8
%stackaddr$makeclosure55474 = alloca %struct.ScmObj*, align 8
%fptrToInt55475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50196 to i64
%ae50196 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55475)
store volatile %struct.ScmObj* %ae50196, %struct.ScmObj** %stackaddr$makeclosure55474, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50196, %struct.ScmObj* %anf_45bind48215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50196, %struct.ScmObj* %k48322, i64 1)
%argslist54370$anf_45bind482160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%argslist54370$anf_45bind482161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54370$anf_45bind482160)
store volatile %struct.ScmObj* %argslist54370$anf_45bind482161, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%argslist54370$anf_45bind482162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %argslist54370$anf_45bind482161)
store volatile %struct.ScmObj* %argslist54370$anf_45bind482162, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%argslist54370$anf_45bind482163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50196, %struct.ScmObj* %argslist54370$anf_45bind482162)
store volatile %struct.ScmObj* %argslist54370$anf_45bind482163, %struct.ScmObj** %stackaddr$prim55478, align 8
%clofunc55479 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48216)
musttail call tailcc void %clofunc55479(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist54370$anf_45bind482163)
ret void
}

define tailcc void @proc_clo$ae50196(%struct.ScmObj* %env$ae50196,%struct.ScmObj* %current_45args54366) {
%stackaddr$env-ref55480 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50196, i64 0)
store %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$env-ref55480
%stackaddr$env-ref55481 = alloca %struct.ScmObj*, align 8
%k48322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50196, i64 1)
store %struct.ScmObj* %k48322, %struct.ScmObj** %stackaddr$env-ref55481
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55482, align 8
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%current_45args54367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %current_45args54367, %struct.ScmObj** %stackaddr$prim55483, align 8
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%cpsprim48324 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsprim48324, %struct.ScmObj** %stackaddr$prim55485, align 8
%ae50202 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54369$k483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55486 = alloca %struct.ScmObj*, align 8
%argslist54369$k483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48324, %struct.ScmObj* %argslist54369$k483220)
store volatile %struct.ScmObj* %argslist54369$k483221, %struct.ScmObj** %stackaddr$prim55486, align 8
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%argslist54369$k483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50202, %struct.ScmObj* %argslist54369$k483221)
store volatile %struct.ScmObj* %argslist54369$k483222, %struct.ScmObj** %stackaddr$prim55487, align 8
%clofunc55488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48322)
musttail call tailcc void %clofunc55488(%struct.ScmObj* %k48322, %struct.ScmObj* %argslist54369$k483222)
ret void
}

define tailcc void @proc_clo$ae50156(%struct.ScmObj* %env$ae50156,%struct.ScmObj* %current_45args54372) {
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %k48325, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%current_45args54373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %current_45args54373, %struct.ScmObj** %stackaddr$prim55490, align 8
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%current_45args54374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %current_45args54374, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55494, align 8
%stackaddr$prim55495 = alloca %struct.ScmObj*, align 8
%cpsprim48326 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %cpsprim48326, %struct.ScmObj** %stackaddr$prim55495, align 8
%ae50161 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54376$k483250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%argslist54376$k483251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48326, %struct.ScmObj* %argslist54376$k483250)
store volatile %struct.ScmObj* %argslist54376$k483251, %struct.ScmObj** %stackaddr$prim55496, align 8
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%argslist54376$k483252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50161, %struct.ScmObj* %argslist54376$k483251)
store volatile %struct.ScmObj* %argslist54376$k483252, %struct.ScmObj** %stackaddr$prim55497, align 8
%clofunc55498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48325)
musttail call tailcc void %clofunc55498(%struct.ScmObj* %k48325, %struct.ScmObj* %argslist54376$k483252)
ret void
}

define tailcc void @proc_clo$ae50132(%struct.ScmObj* %env$ae50132,%struct.ScmObj* %current_45args54378) {
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %k48327, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%current_45args54379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %current_45args54379, %struct.ScmObj** %stackaddr$prim55500, align 8
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%current_45args54380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %current_45args54380, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54380)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55504, align 8
%stackaddr$prim55505 = alloca %struct.ScmObj*, align 8
%cpsprim48328 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48212)
store volatile %struct.ScmObj* %cpsprim48328, %struct.ScmObj** %stackaddr$prim55505, align 8
%ae50137 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54382$k483270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%argslist54382$k483271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48328, %struct.ScmObj* %argslist54382$k483270)
store volatile %struct.ScmObj* %argslist54382$k483271, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%argslist54382$k483272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50137, %struct.ScmObj* %argslist54382$k483271)
store volatile %struct.ScmObj* %argslist54382$k483272, %struct.ScmObj** %stackaddr$prim55507, align 8
%clofunc55508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48327)
musttail call tailcc void %clofunc55508(%struct.ScmObj* %k48327, %struct.ScmObj* %argslist54382$k483272)
ret void
}

define tailcc void @proc_clo$ae49738(%struct.ScmObj* %env$ae49738,%struct.ScmObj* %current_45args54385) {
%stackaddr$env-ref55509 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55509
%stackaddr$env-ref55510 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55510
%stackaddr$env-ref55511 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49738, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55511
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %k48329, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%current_45args54386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54385)
store volatile %struct.ScmObj* %current_45args54386, %struct.ScmObj** %stackaddr$prim55513, align 8
%stackaddr$prim55514 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim55514, align 8
%ae49740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55515 = alloca %struct.ScmObj*, align 8
%fptrToInt55516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49741 to i64
%ae49741 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55516)
store volatile %struct.ScmObj* %ae49741, %struct.ScmObj** %stackaddr$makeclosure55515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37map148077, i64 3)
%argslist54443$k483290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55517 = alloca %struct.ScmObj*, align 8
%argslist54443$k483291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49741, %struct.ScmObj* %argslist54443$k483290)
store volatile %struct.ScmObj* %argslist54443$k483291, %struct.ScmObj** %stackaddr$prim55517, align 8
%stackaddr$prim55518 = alloca %struct.ScmObj*, align 8
%argslist54443$k483292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49740, %struct.ScmObj* %argslist54443$k483291)
store volatile %struct.ScmObj* %argslist54443$k483292, %struct.ScmObj** %stackaddr$prim55518, align 8
%clofunc55519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48329)
musttail call tailcc void %clofunc55519(%struct.ScmObj* %k48329, %struct.ScmObj* %argslist54443$k483292)
ret void
}

define tailcc void @proc_clo$ae49741(%struct.ScmObj* %env$ae49741,%struct.ScmObj* %args4813048330) {
%stackaddr$env-ref55520 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55520
%stackaddr$env-ref55521 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55521
%stackaddr$env-ref55522 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55522
%stackaddr$env-ref55523 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55523
%stackaddr$prim55524 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048330)
store volatile %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$prim55524, align 8
%stackaddr$prim55525 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048330)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim55525, align 8
%stackaddr$prim55526 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim55526, align 8
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48201)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$makeclosure55531 = alloca %struct.ScmObj*, align 8
%fptrToInt55532 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49749 to i64
%ae49749 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55532)
store volatile %struct.ScmObj* %ae49749, %struct.ScmObj** %stackaddr$makeclosure55531, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49749, %struct.ScmObj* %k48331, i64 7)
%ae49750 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55533 = alloca %struct.ScmObj*, align 8
%fptrToInt55534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49751 to i64
%ae49751 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55534)
store volatile %struct.ScmObj* %ae49751, %struct.ScmObj** %stackaddr$makeclosure55533, align 8
%argslist54442$ae497490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%argslist54442$ae497491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49751, %struct.ScmObj* %argslist54442$ae497490)
store volatile %struct.ScmObj* %argslist54442$ae497491, %struct.ScmObj** %stackaddr$prim55535, align 8
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%argslist54442$ae497492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49750, %struct.ScmObj* %argslist54442$ae497491)
store volatile %struct.ScmObj* %argslist54442$ae497492, %struct.ScmObj** %stackaddr$prim55536, align 8
%clofunc55537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49749)
musttail call tailcc void %clofunc55537(%struct.ScmObj* %ae49749, %struct.ScmObj* %argslist54442$ae497492)
ret void
}

define tailcc void @proc_clo$ae49749(%struct.ScmObj* %env$ae49749,%struct.ScmObj* %current_45args54388) {
%stackaddr$env-ref55538 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55538
%stackaddr$env-ref55539 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55539
%stackaddr$env-ref55540 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55540
%stackaddr$env-ref55541 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55541
%stackaddr$env-ref55542 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55542
%stackaddr$env-ref55543 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55543
%stackaddr$env-ref55544 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55544
%stackaddr$env-ref55545 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49749, i64 7)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55545
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54388)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim55546, align 8
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%current_45args54389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54388)
store volatile %struct.ScmObj* %current_45args54389, %struct.ScmObj** %stackaddr$prim55547, align 8
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54389)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim55548, align 8
%stackaddr$makeclosure55549 = alloca %struct.ScmObj*, align 8
%fptrToInt55550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49781 to i64
%ae49781 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55550)
store volatile %struct.ScmObj* %ae49781, %struct.ScmObj** %stackaddr$makeclosure55549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %k48331, i64 6)
%ae49783 = call %struct.ScmObj* @const_init_false()
%argslist54435$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%argslist54435$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54435$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54435$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%argslist54435$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49783, %struct.ScmObj* %argslist54435$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54435$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%argslist54435$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %argslist54435$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54435$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55553, align 8
%stackaddr$prim55554 = alloca %struct.ScmObj*, align 8
%argslist54435$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49781, %struct.ScmObj* %argslist54435$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54435$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55554, align 8
%clofunc55555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55555(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54435$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49781(%struct.ScmObj* %env$ae49781,%struct.ScmObj* %current_45args54391) {
%stackaddr$env-ref55556 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55556
%stackaddr$env-ref55557 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55557
%stackaddr$env-ref55558 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55558
%stackaddr$env-ref55559 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55559
%stackaddr$env-ref55560 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55560
%stackaddr$env-ref55561 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55561
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 6)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$prim55563 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim55563, align 8
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%current_45args54392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54391)
store volatile %struct.ScmObj* %current_45args54392, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54392)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim55565, align 8
%truthy$cmp55566 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48203)
%cmp$cmp55566 = icmp eq i64 %truthy$cmp55566, 1
br i1 %cmp$cmp55566, label %truebranch$cmp55566, label %falsebranch$cmp55566
truebranch$cmp55566:
%ae49792 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54394$k483310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%argslist54394$k483311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist54394$k483310)
store volatile %struct.ScmObj* %argslist54394$k483311, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%argslist54394$k483312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49792, %struct.ScmObj* %argslist54394$k483311)
store volatile %struct.ScmObj* %argslist54394$k483312, %struct.ScmObj** %stackaddr$prim55568, align 8
%clofunc55569 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48331)
musttail call tailcc void %clofunc55569(%struct.ScmObj* %k48331, %struct.ScmObj* %argslist54394$k483312)
ret void
falsebranch$cmp55566:
%stackaddr$makeclosure55570 = alloca %struct.ScmObj*, align 8
%fptrToInt55571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49797 to i64
%ae49797 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55571)
store volatile %struct.ScmObj* %ae49797, %struct.ScmObj** %stackaddr$makeclosure55570, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %k48331, i64 6)
%ae49798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55572 = alloca %struct.ScmObj*, align 8
%fptrToInt55573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49799 to i64
%ae49799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55573)
store volatile %struct.ScmObj* %ae49799, %struct.ScmObj** %stackaddr$makeclosure55572, align 8
%argslist54434$ae497970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%argslist54434$ae497971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49799, %struct.ScmObj* %argslist54434$ae497970)
store volatile %struct.ScmObj* %argslist54434$ae497971, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%argslist54434$ae497972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist54434$ae497971)
store volatile %struct.ScmObj* %argslist54434$ae497972, %struct.ScmObj** %stackaddr$prim55575, align 8
%clofunc55576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49797)
musttail call tailcc void %clofunc55576(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist54434$ae497972)
ret void
}

define tailcc void @proc_clo$ae49797(%struct.ScmObj* %env$ae49797,%struct.ScmObj* %current_45args54395) {
%stackaddr$env-ref55577 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55577
%stackaddr$env-ref55578 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55578
%stackaddr$env-ref55579 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55579
%stackaddr$env-ref55580 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55580
%stackaddr$env-ref55581 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55581
%stackaddr$env-ref55582 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55582
%stackaddr$env-ref55583 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 6)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55583
%stackaddr$prim55584 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54395)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim55584, align 8
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%current_45args54396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54395)
store volatile %struct.ScmObj* %current_45args54396, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$makeclosure55587 = alloca %struct.ScmObj*, align 8
%fptrToInt55588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49818 to i64
%ae49818 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55588)
store volatile %struct.ScmObj* %ae49818, %struct.ScmObj** %stackaddr$makeclosure55587, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49818, %struct.ScmObj* %k48331, i64 6)
%argslist54429$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%argslist54429$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54429$_37map1480770)
store volatile %struct.ScmObj* %argslist54429$_37map1480771, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%argslist54429$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist54429$_37map1480771)
store volatile %struct.ScmObj* %argslist54429$_37map1480772, %struct.ScmObj** %stackaddr$prim55590, align 8
%stackaddr$prim55591 = alloca %struct.ScmObj*, align 8
%argslist54429$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49818, %struct.ScmObj* %argslist54429$_37map1480772)
store volatile %struct.ScmObj* %argslist54429$_37map1480773, %struct.ScmObj** %stackaddr$prim55591, align 8
%clofunc55592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55592(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54429$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49818(%struct.ScmObj* %env$ae49818,%struct.ScmObj* %current_45args54398) {
%stackaddr$env-ref55593 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55593
%stackaddr$env-ref55594 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55594
%stackaddr$env-ref55595 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55595
%stackaddr$env-ref55596 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55596
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$env-ref55599 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49818, i64 6)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55599
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim55600, align 8
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%current_45args54399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54398)
store volatile %struct.ScmObj* %current_45args54399, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim55602, align 8
%stackaddr$makeclosure55603 = alloca %struct.ScmObj*, align 8
%fptrToInt55604 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49821 to i64
%ae49821 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55604)
store volatile %struct.ScmObj* %ae49821, %struct.ScmObj** %stackaddr$makeclosure55603, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %k48331, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %lsts_4348138, i64 7)
%ae49822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55605 = alloca %struct.ScmObj*, align 8
%fptrToInt55606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49823 to i64
%ae49823 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55606)
store volatile %struct.ScmObj* %ae49823, %struct.ScmObj** %stackaddr$makeclosure55605, align 8
%argslist54428$ae498210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55607 = alloca %struct.ScmObj*, align 8
%argslist54428$ae498211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49823, %struct.ScmObj* %argslist54428$ae498210)
store volatile %struct.ScmObj* %argslist54428$ae498211, %struct.ScmObj** %stackaddr$prim55607, align 8
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%argslist54428$ae498212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49822, %struct.ScmObj* %argslist54428$ae498211)
store volatile %struct.ScmObj* %argslist54428$ae498212, %struct.ScmObj** %stackaddr$prim55608, align 8
%clofunc55609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49821)
musttail call tailcc void %clofunc55609(%struct.ScmObj* %ae49821, %struct.ScmObj* %argslist54428$ae498212)
ret void
}

define tailcc void @proc_clo$ae49821(%struct.ScmObj* %env$ae49821,%struct.ScmObj* %current_45args54401) {
%stackaddr$env-ref55610 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref55610
%stackaddr$env-ref55611 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55611
%stackaddr$env-ref55612 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55612
%stackaddr$env-ref55613 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55613
%stackaddr$env-ref55614 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55614
%stackaddr$env-ref55615 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55615
%stackaddr$env-ref55616 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 6)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55616
%stackaddr$env-ref55617 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 7)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55617
%stackaddr$prim55618 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54401)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim55618, align 8
%stackaddr$prim55619 = alloca %struct.ScmObj*, align 8
%current_45args54402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54401)
store volatile %struct.ScmObj* %current_45args54402, %struct.ScmObj** %stackaddr$prim55619, align 8
%stackaddr$prim55620 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54402)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim55620, align 8
%stackaddr$makeclosure55621 = alloca %struct.ScmObj*, align 8
%fptrToInt55622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49842 to i64
%ae49842 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55622)
store volatile %struct.ScmObj* %ae49842, %struct.ScmObj** %stackaddr$makeclosure55621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %acc48132, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %_37foldr48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %_37foldl48129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %k48331, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %lsts_4348138, i64 5)
%argslist54423$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%argslist54423$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist54423$_37map1480770)
store volatile %struct.ScmObj* %argslist54423$_37map1480771, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$prim55624 = alloca %struct.ScmObj*, align 8
%argslist54423$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist54423$_37map1480771)
store volatile %struct.ScmObj* %argslist54423$_37map1480772, %struct.ScmObj** %stackaddr$prim55624, align 8
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%argslist54423$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49842, %struct.ScmObj* %argslist54423$_37map1480772)
store volatile %struct.ScmObj* %argslist54423$_37map1480773, %struct.ScmObj** %stackaddr$prim55625, align 8
%clofunc55626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55626(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54423$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49842(%struct.ScmObj* %env$ae49842,%struct.ScmObj* %current_45args54404) {
%stackaddr$env-ref55627 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55627
%stackaddr$env-ref55628 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 1)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55628
%stackaddr$env-ref55629 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 2)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55629
%stackaddr$env-ref55630 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55630
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 4)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 5)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54404)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%current_45args54405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54404)
store volatile %struct.ScmObj* %current_45args54405, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54405)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$makeclosure55636 = alloca %struct.ScmObj*, align 8
%fptrToInt55637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49845 to i64
%ae49845 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55637)
store volatile %struct.ScmObj* %ae49845, %struct.ScmObj** %stackaddr$makeclosure55636, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %vs48136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %f48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %acc48132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %k48331, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %lsts_4348138, i64 6)
%ae49846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55638 = alloca %struct.ScmObj*, align 8
%fptrToInt55639 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49847 to i64
%ae49847 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55639)
store volatile %struct.ScmObj* %ae49847, %struct.ScmObj** %stackaddr$makeclosure55638, align 8
%argslist54422$ae498450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%argslist54422$ae498451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49847, %struct.ScmObj* %argslist54422$ae498450)
store volatile %struct.ScmObj* %argslist54422$ae498451, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%argslist54422$ae498452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49846, %struct.ScmObj* %argslist54422$ae498451)
store volatile %struct.ScmObj* %argslist54422$ae498452, %struct.ScmObj** %stackaddr$prim55641, align 8
%clofunc55642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49845)
musttail call tailcc void %clofunc55642(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist54422$ae498452)
ret void
}

define tailcc void @proc_clo$ae49845(%struct.ScmObj* %env$ae49845,%struct.ScmObj* %current_45args54407) {
%stackaddr$env-ref55643 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 0)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref55643
%stackaddr$env-ref55644 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 1)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55644
%stackaddr$env-ref55645 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 2)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref55645
%stackaddr$env-ref55646 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55646
%stackaddr$env-ref55647 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55647
%stackaddr$env-ref55648 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 5)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55648
%stackaddr$env-ref55649 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 6)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55649
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%current_45args54408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %current_45args54408, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim55652, align 8
%ae49868 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49868)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$makeclosure55654 = alloca %struct.ScmObj*, align 8
%fptrToInt55655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49870 to i64
%ae49870 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55655)
store volatile %struct.ScmObj* %ae49870, %struct.ScmObj** %stackaddr$makeclosure55654, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49870, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49870, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49870, %struct.ScmObj* %k48331, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49870, %struct.ScmObj* %lsts_4348138, i64 3)
%argslist54416$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55656 = alloca %struct.ScmObj*, align 8
%argslist54416$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist54416$_37foldr480510)
store volatile %struct.ScmObj* %argslist54416$_37foldr480511, %struct.ScmObj** %stackaddr$prim55656, align 8
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%argslist54416$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48207, %struct.ScmObj* %argslist54416$_37foldr480511)
store volatile %struct.ScmObj* %argslist54416$_37foldr480512, %struct.ScmObj** %stackaddr$prim55657, align 8
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%argslist54416$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist54416$_37foldr480512)
store volatile %struct.ScmObj* %argslist54416$_37foldr480513, %struct.ScmObj** %stackaddr$prim55658, align 8
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%argslist54416$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49870, %struct.ScmObj* %argslist54416$_37foldr480513)
store volatile %struct.ScmObj* %argslist54416$_37foldr480514, %struct.ScmObj** %stackaddr$prim55659, align 8
%clofunc55660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55660(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist54416$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49870(%struct.ScmObj* %env$ae49870,%struct.ScmObj* %current_45args54410) {
%stackaddr$env-ref55661 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49870, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55661
%stackaddr$env-ref55662 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49870, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55662
%stackaddr$env-ref55663 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49870, i64 2)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55663
%stackaddr$env-ref55664 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49870, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55664
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54410)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%current_45args54411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54410)
store volatile %struct.ScmObj* %current_45args54411, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54411)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$makeclosure55668 = alloca %struct.ScmObj*, align 8
%fptrToInt55669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49874 to i64
%ae49874 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55669)
store volatile %struct.ScmObj* %ae49874, %struct.ScmObj** %stackaddr$makeclosure55668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49874, %struct.ScmObj* %f48133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49874, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49874, %struct.ScmObj* %k48331, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49874, %struct.ScmObj* %lsts_4348138, i64 3)
%stackaddr$prim55670 = alloca %struct.ScmObj*, align 8
%cpsargs48342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49874, %struct.ScmObj* %anf_45bind48208)
store volatile %struct.ScmObj* %cpsargs48342, %struct.ScmObj** %stackaddr$prim55670, align 8
%clofunc55671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc55671(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48342)
ret void
}

define tailcc void @proc_clo$ae49874(%struct.ScmObj* %env$ae49874,%struct.ScmObj* %current_45args54413) {
%stackaddr$env-ref55672 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49874, i64 0)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref55672
%stackaddr$env-ref55673 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49874, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref55673
%stackaddr$env-ref55674 = alloca %struct.ScmObj*, align 8
%k48331 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49874, i64 2)
store %struct.ScmObj* %k48331, %struct.ScmObj** %stackaddr$env-ref55674
%stackaddr$env-ref55675 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49874, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref55675
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54413)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%current_45args54414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54413)
store volatile %struct.ScmObj* %current_45args54414, %struct.ScmObj** %stackaddr$prim55677, align 8
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54414)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim55678, align 8
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48209)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%cpsargs48341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48331, %struct.ScmObj* %anf_45bind48210)
store volatile %struct.ScmObj* %cpsargs48341, %struct.ScmObj** %stackaddr$prim55681, align 8
%clofunc55682 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc55682(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48341)
ret void
}

define tailcc void @proc_clo$ae49847(%struct.ScmObj* %env$ae49847,%struct.ScmObj* %current_45args54417) {
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim55683, align 8
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%current_45args54418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54417)
store volatile %struct.ScmObj* %current_45args54418, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54418)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%current_45args54419 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54418)
store volatile %struct.ScmObj* %current_45args54419, %struct.ScmObj** %stackaddr$prim55686, align 8
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54419)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%cpsprim48344 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48344, %struct.ScmObj** %stackaddr$prim55688, align 8
%ae49851 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54421$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%argslist54421$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48344, %struct.ScmObj* %argslist54421$k483430)
store volatile %struct.ScmObj* %argslist54421$k483431, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%argslist54421$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49851, %struct.ScmObj* %argslist54421$k483431)
store volatile %struct.ScmObj* %argslist54421$k483432, %struct.ScmObj** %stackaddr$prim55690, align 8
%clofunc55691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc55691(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54421$k483432)
ret void
}

define tailcc void @proc_clo$ae49823(%struct.ScmObj* %env$ae49823,%struct.ScmObj* %current_45args54424) {
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$prim55692, align 8
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%current_45args54425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54424)
store volatile %struct.ScmObj* %current_45args54425, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54425)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim55694, align 8
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim55695, align 8
%ae49826 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54427$k483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist54427$k483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist54427$k483450)
store volatile %struct.ScmObj* %argslist54427$k483451, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist54427$k483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49826, %struct.ScmObj* %argslist54427$k483451)
store volatile %struct.ScmObj* %argslist54427$k483452, %struct.ScmObj** %stackaddr$prim55697, align 8
%clofunc55698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48345)
musttail call tailcc void %clofunc55698(%struct.ScmObj* %k48345, %struct.ScmObj* %argslist54427$k483452)
ret void
}

define tailcc void @proc_clo$ae49799(%struct.ScmObj* %env$ae49799,%struct.ScmObj* %current_45args54430) {
%stackaddr$prim55699 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54430)
store volatile %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$prim55699, align 8
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%current_45args54431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54430)
store volatile %struct.ScmObj* %current_45args54431, %struct.ScmObj** %stackaddr$prim55700, align 8
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54431)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%cpsprim48348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48348, %struct.ScmObj** %stackaddr$prim55702, align 8
%ae49802 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54433$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%argslist54433$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48348, %struct.ScmObj* %argslist54433$k483470)
store volatile %struct.ScmObj* %argslist54433$k483471, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%argslist54433$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49802, %struct.ScmObj* %argslist54433$k483471)
store volatile %struct.ScmObj* %argslist54433$k483472, %struct.ScmObj** %stackaddr$prim55704, align 8
%clofunc55705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc55705(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54433$k483472)
ret void
}

define tailcc void @proc_clo$ae49751(%struct.ScmObj* %env$ae49751,%struct.ScmObj* %current_45args54436) {
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54436)
store volatile %struct.ScmObj* %k48349, %struct.ScmObj** %stackaddr$prim55706, align 8
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%current_45args54437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54436)
store volatile %struct.ScmObj* %current_45args54437, %struct.ScmObj** %stackaddr$prim55707, align 8
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54437)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim55708, align 8
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%current_45args54438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54437)
store volatile %struct.ScmObj* %current_45args54438, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54438)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim55710, align 8
%truthy$cmp55711 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp55711 = icmp eq i64 %truthy$cmp55711, 1
br i1 %cmp$cmp55711, label %truebranch$cmp55711, label %falsebranch$cmp55711
truebranch$cmp55711:
%ae49754 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54440$k483490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%argslist54440$k483491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist54440$k483490)
store volatile %struct.ScmObj* %argslist54440$k483491, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$prim55713 = alloca %struct.ScmObj*, align 8
%argslist54440$k483492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49754, %struct.ScmObj* %argslist54440$k483491)
store volatile %struct.ScmObj* %argslist54440$k483492, %struct.ScmObj** %stackaddr$prim55713, align 8
%clofunc55714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48349)
musttail call tailcc void %clofunc55714(%struct.ScmObj* %k48349, %struct.ScmObj* %argslist54440$k483492)
ret void
falsebranch$cmp55711:
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%cpsprim48350 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48350, %struct.ScmObj** %stackaddr$prim55715, align 8
%ae49761 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54441$k483490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%argslist54441$k483491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48350, %struct.ScmObj* %argslist54441$k483490)
store volatile %struct.ScmObj* %argslist54441$k483491, %struct.ScmObj** %stackaddr$prim55716, align 8
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%argslist54441$k483492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49761, %struct.ScmObj* %argslist54441$k483491)
store volatile %struct.ScmObj* %argslist54441$k483492, %struct.ScmObj** %stackaddr$prim55717, align 8
%clofunc55718 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48349)
musttail call tailcc void %clofunc55718(%struct.ScmObj* %k48349, %struct.ScmObj* %argslist54441$k483492)
ret void
}

define tailcc void @proc_clo$ae49592(%struct.ScmObj* %env$ae49592,%struct.ScmObj* %args4807348351) {
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$env-ref55721 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49592, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55721
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348351)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348351)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim55723, align 8
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$makeclosure55726 = alloca %struct.ScmObj*, align 8
%fptrToInt55727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49597 to i64
%ae49597 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55727)
store volatile %struct.ScmObj* %ae49597, %struct.ScmObj** %stackaddr$makeclosure55726, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49597, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49597, %struct.ScmObj* %k48352, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49597, %struct.ScmObj* %lsts48074, i64 2)
%ae49598 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55728 = alloca %struct.ScmObj*, align 8
%fptrToInt55729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49599 to i64
%ae49599 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55729)
store volatile %struct.ScmObj* %ae49599, %struct.ScmObj** %stackaddr$makeclosure55728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49599, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49599, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49599, %struct.ScmObj* %f48075, i64 2)
%argslist54460$ae495970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%argslist54460$ae495971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49599, %struct.ScmObj* %argslist54460$ae495970)
store volatile %struct.ScmObj* %argslist54460$ae495971, %struct.ScmObj** %stackaddr$prim55730, align 8
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%argslist54460$ae495972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49598, %struct.ScmObj* %argslist54460$ae495971)
store volatile %struct.ScmObj* %argslist54460$ae495972, %struct.ScmObj** %stackaddr$prim55731, align 8
%clofunc55732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49597)
musttail call tailcc void %clofunc55732(%struct.ScmObj* %ae49597, %struct.ScmObj* %argslist54460$ae495972)
ret void
}

define tailcc void @proc_clo$ae49597(%struct.ScmObj* %env$ae49597,%struct.ScmObj* %current_45args54445) {
%stackaddr$env-ref55733 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49597, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55733
%stackaddr$env-ref55734 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49597, i64 1)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref55734
%stackaddr$env-ref55735 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49597, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref55735
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54445)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim55736, align 8
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%current_45args54446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54445)
store volatile %struct.ScmObj* %current_45args54446, %struct.ScmObj** %stackaddr$prim55737, align 8
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54446)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim55738, align 8
%ae49660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55739 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49660, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55739, align 8
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %anf_45bind48198)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%cpsargs48354 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48352, %struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %cpsargs48354, %struct.ScmObj** %stackaddr$prim55741, align 8
%clofunc55742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc55742(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48354)
ret void
}

define tailcc void @proc_clo$ae49599(%struct.ScmObj* %env$ae49599,%struct.ScmObj* %fargs4807648355) {
%stackaddr$env-ref55743 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49599, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55743
%stackaddr$env-ref55744 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49599, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55744
%stackaddr$env-ref55745 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49599, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55745
%stackaddr$prim55746 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648355)
store volatile %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$prim55746, align 8
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648355)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$makeclosure55748 = alloca %struct.ScmObj*, align 8
%fptrToInt55749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49603 to i64
%ae49603 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55749)
store volatile %struct.ScmObj* %ae49603, %struct.ScmObj** %stackaddr$makeclosure55748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49603, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49603, %struct.ScmObj* %_37last48068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49603, %struct.ScmObj* %fargs48076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49603, %struct.ScmObj* %f48075, i64 3)
%ae49605 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54459$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55750 = alloca %struct.ScmObj*, align 8
%argslist54459$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49605, %struct.ScmObj* %argslist54459$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist54459$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim55750, align 8
%stackaddr$prim55751 = alloca %struct.ScmObj*, align 8
%argslist54459$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54459$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist54459$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim55751, align 8
%stackaddr$prim55752 = alloca %struct.ScmObj*, align 8
%argslist54459$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49603, %struct.ScmObj* %argslist54459$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist54459$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim55752, align 8
%clofunc55753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc55753(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist54459$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49603(%struct.ScmObj* %env$ae49603,%struct.ScmObj* %current_45args54448) {
%stackaddr$env-ref55754 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49603, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref55754
%stackaddr$env-ref55755 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49603, i64 1)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55755
%stackaddr$env-ref55756 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49603, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55756
%stackaddr$env-ref55757 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49603, i64 3)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref55757
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54448)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%current_45args54449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54448)
store volatile %struct.ScmObj* %current_45args54449, %struct.ScmObj** %stackaddr$prim55759, align 8
%stackaddr$prim55760 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54449)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim55760, align 8
%stackaddr$makeclosure55761 = alloca %struct.ScmObj*, align 8
%fptrToInt55762 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49610 to i64
%ae49610 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55762)
store volatile %struct.ScmObj* %ae49610, %struct.ScmObj** %stackaddr$makeclosure55761, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49610, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49610, %struct.ScmObj* %_37last48068, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49610, %struct.ScmObj* %fargs48076, i64 2)
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%cpsargs48361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49610, %struct.ScmObj* %anf_45bind48194)
store volatile %struct.ScmObj* %cpsargs48361, %struct.ScmObj** %stackaddr$prim55763, align 8
%clofunc55764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc55764(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48361)
ret void
}

define tailcc void @proc_clo$ae49610(%struct.ScmObj* %env$ae49610,%struct.ScmObj* %current_45args54451) {
%stackaddr$env-ref55765 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49610, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref55765
%stackaddr$env-ref55766 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49610, i64 1)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55766
%stackaddr$env-ref55767 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49610, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref55767
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54451)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim55768, align 8
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%current_45args54452 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54451)
store volatile %struct.ScmObj* %current_45args54452, %struct.ScmObj** %stackaddr$prim55769, align 8
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54452)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim55770, align 8
%stackaddr$makeclosure55771 = alloca %struct.ScmObj*, align 8
%fptrToInt55772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49615 to i64
%ae49615 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55772)
store volatile %struct.ScmObj* %ae49615, %struct.ScmObj** %stackaddr$makeclosure55771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49615, %struct.ScmObj* %k48356, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49615, %struct.ScmObj* %anf_45bind48195, i64 1)
%argslist54458$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%argslist54458$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist54458$_37last480680)
store volatile %struct.ScmObj* %argslist54458$_37last480681, %struct.ScmObj** %stackaddr$prim55773, align 8
%stackaddr$prim55774 = alloca %struct.ScmObj*, align 8
%argslist54458$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49615, %struct.ScmObj* %argslist54458$_37last480681)
store volatile %struct.ScmObj* %argslist54458$_37last480682, %struct.ScmObj** %stackaddr$prim55774, align 8
%clofunc55775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc55775(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist54458$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49615(%struct.ScmObj* %env$ae49615,%struct.ScmObj* %current_45args54454) {
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%k48356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49615, i64 0)
store %struct.ScmObj* %k48356, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$env-ref55777 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49615, i64 1)
store %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$env-ref55777
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54454)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%current_45args54455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54454)
store volatile %struct.ScmObj* %current_45args54455, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54455)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%cpsprim48360 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %anf_45bind48196)
store volatile %struct.ScmObj* %cpsprim48360, %struct.ScmObj** %stackaddr$prim55781, align 8
%ae49620 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54457$k483560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%argslist54457$k483561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48360, %struct.ScmObj* %argslist54457$k483560)
store volatile %struct.ScmObj* %argslist54457$k483561, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%argslist54457$k483562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49620, %struct.ScmObj* %argslist54457$k483561)
store volatile %struct.ScmObj* %argslist54457$k483562, %struct.ScmObj** %stackaddr$prim55783, align 8
%clofunc55784 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48356)
musttail call tailcc void %clofunc55784(%struct.ScmObj* %k48356, %struct.ScmObj* %argslist54457$k483562)
ret void
}

define tailcc void @proc_clo$ae49515(%struct.ScmObj* %env$ae49515,%struct.ScmObj* %current_45args54462) {
%stackaddr$env-ref55785 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49515, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55785
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%current_45args54463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54462)
store volatile %struct.ScmObj* %current_45args54463, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54463)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim55788, align 8
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%current_45args54464 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54463)
store volatile %struct.ScmObj* %current_45args54464, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54464)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim55790, align 8
%stackaddr$makeclosure55791 = alloca %struct.ScmObj*, align 8
%fptrToInt55792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49516 to i64
%ae49516 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55792)
store volatile %struct.ScmObj* %ae49516, %struct.ScmObj** %stackaddr$makeclosure55791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %k48362, i64 2)
%ae49517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55793 = alloca %struct.ScmObj*, align 8
%fptrToInt55794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49518 to i64
%ae49518 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55794)
store volatile %struct.ScmObj* %ae49518, %struct.ScmObj** %stackaddr$makeclosure55793, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49518, %struct.ScmObj* %f48079, i64 0)
%argslist54479$ae495160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%argslist54479$ae495161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49518, %struct.ScmObj* %argslist54479$ae495160)
store volatile %struct.ScmObj* %argslist54479$ae495161, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%argslist54479$ae495162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49517, %struct.ScmObj* %argslist54479$ae495161)
store volatile %struct.ScmObj* %argslist54479$ae495162, %struct.ScmObj** %stackaddr$prim55796, align 8
%clofunc55797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49516)
musttail call tailcc void %clofunc55797(%struct.ScmObj* %ae49516, %struct.ScmObj* %argslist54479$ae495162)
ret void
}

define tailcc void @proc_clo$ae49516(%struct.ScmObj* %env$ae49516,%struct.ScmObj* %current_45args54466) {
%stackaddr$env-ref55798 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref55798
%stackaddr$env-ref55799 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55799
%stackaddr$env-ref55800 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 2)
store %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$env-ref55800
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54466)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim55801, align 8
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%current_45args54467 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54466)
store volatile %struct.ScmObj* %current_45args54467, %struct.ScmObj** %stackaddr$prim55802, align 8
%stackaddr$prim55803 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54467)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim55803, align 8
%ae49550 = call %struct.ScmObj* @const_init_null()
%argslist54469$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%argslist54469$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist54469$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54469$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55804, align 8
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%argslist54469$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49550, %struct.ScmObj* %argslist54469$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54469$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55805, align 8
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%argslist54469$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist54469$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54469$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55806, align 8
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%argslist54469$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist54469$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54469$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55807, align 8
%clofunc55808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55808(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54469$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49518(%struct.ScmObj* %env$ae49518,%struct.ScmObj* %current_45args54470) {
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49518, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54470)
store volatile %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$prim55810, align 8
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%current_45args54471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54470)
store volatile %struct.ScmObj* %current_45args54471, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$prim55813 = alloca %struct.ScmObj*, align 8
%current_45args54472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54471)
store volatile %struct.ScmObj* %current_45args54472, %struct.ScmObj** %stackaddr$prim55813, align 8
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54472)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$makeclosure55815 = alloca %struct.ScmObj*, align 8
%fptrToInt55816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49520 to i64
%ae49520 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55816)
store volatile %struct.ScmObj* %ae49520, %struct.ScmObj** %stackaddr$makeclosure55815, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49520, %struct.ScmObj* %r48080, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49520, %struct.ScmObj* %k48364, i64 1)
%argslist54478$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%argslist54478$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist54478$f480790)
store volatile %struct.ScmObj* %argslist54478$f480791, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%argslist54478$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49520, %struct.ScmObj* %argslist54478$f480791)
store volatile %struct.ScmObj* %argslist54478$f480792, %struct.ScmObj** %stackaddr$prim55818, align 8
%clofunc55819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc55819(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist54478$f480792)
ret void
}

define tailcc void @proc_clo$ae49520(%struct.ScmObj* %env$ae49520,%struct.ScmObj* %current_45args54474) {
%stackaddr$env-ref55820 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49520, i64 0)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref55820
%stackaddr$env-ref55821 = alloca %struct.ScmObj*, align 8
%k48364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49520, i64 1)
store %struct.ScmObj* %k48364, %struct.ScmObj** %stackaddr$env-ref55821
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%_95k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %_95k48365, %struct.ScmObj** %stackaddr$prim55822, align 8
%stackaddr$prim55823 = alloca %struct.ScmObj*, align 8
%current_45args54475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54474)
store volatile %struct.ScmObj* %current_45args54475, %struct.ScmObj** %stackaddr$prim55823, align 8
%stackaddr$prim55824 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54475)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim55824, align 8
%stackaddr$prim55825 = alloca %struct.ScmObj*, align 8
%cpsprim48366 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48366, %struct.ScmObj** %stackaddr$prim55825, align 8
%ae49525 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54477$k483640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%argslist54477$k483641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48366, %struct.ScmObj* %argslist54477$k483640)
store volatile %struct.ScmObj* %argslist54477$k483641, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%argslist54477$k483642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49525, %struct.ScmObj* %argslist54477$k483641)
store volatile %struct.ScmObj* %argslist54477$k483642, %struct.ScmObj** %stackaddr$prim55827, align 8
%clofunc55828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48364)
musttail call tailcc void %clofunc55828(%struct.ScmObj* %k48364, %struct.ScmObj* %argslist54477$k483642)
ret void
}

define tailcc void @proc_clo$ae49129(%struct.ScmObj* %env$ae49129,%struct.ScmObj* %current_45args54482) {
%stackaddr$env-ref55829 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49129, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55829
%stackaddr$env-ref55830 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49129, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55830
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%current_45args54483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54482)
store volatile %struct.ScmObj* %current_45args54483, %struct.ScmObj** %stackaddr$prim55832, align 8
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54483)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim55833, align 8
%ae49131 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55834 = alloca %struct.ScmObj*, align 8
%fptrToInt55835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49132 to i64
%ae49132 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55835)
store volatile %struct.ScmObj* %ae49132, %struct.ScmObj** %stackaddr$makeclosure55834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49132, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49132, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49132, %struct.ScmObj* %_37map148042, i64 2)
%argslist54540$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%argslist54540$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49132, %struct.ScmObj* %argslist54540$k483670)
store volatile %struct.ScmObj* %argslist54540$k483671, %struct.ScmObj** %stackaddr$prim55836, align 8
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%argslist54540$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49131, %struct.ScmObj* %argslist54540$k483671)
store volatile %struct.ScmObj* %argslist54540$k483672, %struct.ScmObj** %stackaddr$prim55837, align 8
%clofunc55838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc55838(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist54540$k483672)
ret void
}

define tailcc void @proc_clo$ae49132(%struct.ScmObj* %env$ae49132,%struct.ScmObj* %args4805348368) {
%stackaddr$env-ref55839 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49132, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55839
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49132, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49132, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348368)
store volatile %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$prim55842, align 8
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348368)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim55844, align 8
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48179)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim55846, align 8
%stackaddr$prim55847 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim55847, align 8
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48180)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$makeclosure55849 = alloca %struct.ScmObj*, align 8
%fptrToInt55850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49140 to i64
%ae49140 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55850)
store volatile %struct.ScmObj* %ae49140, %struct.ScmObj** %stackaddr$makeclosure55849, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %k48369, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49140, %struct.ScmObj* %_37map148042, i64 6)
%ae49141 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55851 = alloca %struct.ScmObj*, align 8
%fptrToInt55852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49142 to i64
%ae49142 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55852)
store volatile %struct.ScmObj* %ae49142, %struct.ScmObj** %stackaddr$makeclosure55851, align 8
%argslist54539$ae491400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%argslist54539$ae491401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49142, %struct.ScmObj* %argslist54539$ae491400)
store volatile %struct.ScmObj* %argslist54539$ae491401, %struct.ScmObj** %stackaddr$prim55853, align 8
%stackaddr$prim55854 = alloca %struct.ScmObj*, align 8
%argslist54539$ae491402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49141, %struct.ScmObj* %argslist54539$ae491401)
store volatile %struct.ScmObj* %argslist54539$ae491402, %struct.ScmObj** %stackaddr$prim55854, align 8
%clofunc55855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49140)
musttail call tailcc void %clofunc55855(%struct.ScmObj* %ae49140, %struct.ScmObj* %argslist54539$ae491402)
ret void
}

define tailcc void @proc_clo$ae49140(%struct.ScmObj* %env$ae49140,%struct.ScmObj* %current_45args54485) {
%stackaddr$env-ref55856 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55856
%stackaddr$env-ref55857 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55857
%stackaddr$env-ref55858 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55858
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 4)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$env-ref55861 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55861
%stackaddr$env-ref55862 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49140, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55862
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%current_45args54486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54485)
store volatile %struct.ScmObj* %current_45args54486, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54486)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$makeclosure55866 = alloca %struct.ScmObj*, align 8
%fptrToInt55867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49172 to i64
%ae49172 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55867)
store volatile %struct.ScmObj* %ae49172, %struct.ScmObj** %stackaddr$makeclosure55866, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %k48369, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37map148042, i64 6)
%ae49174 = call %struct.ScmObj* @const_init_false()
%argslist54532$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%argslist54532$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54532$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54532$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%argslist54532$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49174, %struct.ScmObj* %argslist54532$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54532$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%argslist54532$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist54532$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54532$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55870, align 8
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%argslist54532$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49172, %struct.ScmObj* %argslist54532$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54532$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55871, align 8
%clofunc55872 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55872(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54532$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49172(%struct.ScmObj* %env$ae49172,%struct.ScmObj* %current_45args54488) {
%stackaddr$env-ref55873 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55873
%stackaddr$env-ref55874 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55874
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$env-ref55877 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 4)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55877
%stackaddr$env-ref55878 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55878
%stackaddr$env-ref55879 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55879
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%_95k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %_95k48371, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%current_45args54489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54488)
store volatile %struct.ScmObj* %current_45args54489, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54489)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim55882, align 8
%truthy$cmp55883 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48182)
%cmp$cmp55883 = icmp eq i64 %truthy$cmp55883, 1
br i1 %cmp$cmp55883, label %truebranch$cmp55883, label %falsebranch$cmp55883
truebranch$cmp55883:
%ae49183 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54491$k483690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist54491$k483691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist54491$k483690)
store volatile %struct.ScmObj* %argslist54491$k483691, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist54491$k483692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49183, %struct.ScmObj* %argslist54491$k483691)
store volatile %struct.ScmObj* %argslist54491$k483692, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48369)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %k48369, %struct.ScmObj* %argslist54491$k483692)
ret void
falsebranch$cmp55883:
%stackaddr$makeclosure55887 = alloca %struct.ScmObj*, align 8
%fptrToInt55888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49188 to i64
%ae49188 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55888)
store volatile %struct.ScmObj* %ae49188, %struct.ScmObj** %stackaddr$makeclosure55887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %k48369, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49188, %struct.ScmObj* %_37map148042, i64 6)
%ae49189 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55889 = alloca %struct.ScmObj*, align 8
%fptrToInt55890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49190 to i64
%ae49190 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55890)
store volatile %struct.ScmObj* %ae49190, %struct.ScmObj** %stackaddr$makeclosure55889, align 8
%argslist54531$ae491880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%argslist54531$ae491881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49190, %struct.ScmObj* %argslist54531$ae491880)
store volatile %struct.ScmObj* %argslist54531$ae491881, %struct.ScmObj** %stackaddr$prim55891, align 8
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%argslist54531$ae491882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49189, %struct.ScmObj* %argslist54531$ae491881)
store volatile %struct.ScmObj* %argslist54531$ae491882, %struct.ScmObj** %stackaddr$prim55892, align 8
%clofunc55893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49188)
musttail call tailcc void %clofunc55893(%struct.ScmObj* %ae49188, %struct.ScmObj* %argslist54531$ae491882)
ret void
}

define tailcc void @proc_clo$ae49188(%struct.ScmObj* %env$ae49188,%struct.ScmObj* %current_45args54492) {
%stackaddr$env-ref55894 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55894
%stackaddr$env-ref55895 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55895
%stackaddr$env-ref55896 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55896
%stackaddr$env-ref55897 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55897
%stackaddr$env-ref55898 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 4)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55898
%stackaddr$env-ref55899 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55899
%stackaddr$env-ref55900 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49188, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55900
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%current_45args54493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54492)
store volatile %struct.ScmObj* %current_45args54493, %struct.ScmObj** %stackaddr$prim55902, align 8
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54493)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$makeclosure55904 = alloca %struct.ScmObj*, align 8
%fptrToInt55905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49209 to i64
%ae49209 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55905)
store volatile %struct.ScmObj* %ae49209, %struct.ScmObj** %stackaddr$makeclosure55904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %k48369, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49209, %struct.ScmObj* %_37map148042, i64 6)
%argslist54526$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55906 = alloca %struct.ScmObj*, align 8
%argslist54526$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54526$_37map1480420)
store volatile %struct.ScmObj* %argslist54526$_37map1480421, %struct.ScmObj** %stackaddr$prim55906, align 8
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%argslist54526$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist54526$_37map1480421)
store volatile %struct.ScmObj* %argslist54526$_37map1480422, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%argslist54526$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49209, %struct.ScmObj* %argslist54526$_37map1480422)
store volatile %struct.ScmObj* %argslist54526$_37map1480423, %struct.ScmObj** %stackaddr$prim55908, align 8
%clofunc55909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc55909(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54526$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49209(%struct.ScmObj* %env$ae49209,%struct.ScmObj* %current_45args54495) {
%stackaddr$env-ref55910 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55910
%stackaddr$env-ref55911 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55911
%stackaddr$env-ref55912 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55912
%stackaddr$env-ref55913 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55913
%stackaddr$env-ref55914 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 4)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55914
%stackaddr$env-ref55915 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55915
%stackaddr$env-ref55916 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49209, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55916
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54495)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim55917, align 8
%stackaddr$prim55918 = alloca %struct.ScmObj*, align 8
%current_45args54496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54495)
store volatile %struct.ScmObj* %current_45args54496, %struct.ScmObj** %stackaddr$prim55918, align 8
%stackaddr$prim55919 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54496)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim55919, align 8
%stackaddr$makeclosure55920 = alloca %struct.ScmObj*, align 8
%fptrToInt55921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49212 to i64
%ae49212 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55921)
store volatile %struct.ScmObj* %ae49212, %struct.ScmObj** %stackaddr$makeclosure55920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %k48369, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %lsts_4348061, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37map148042, i64 7)
%ae49213 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55922 = alloca %struct.ScmObj*, align 8
%fptrToInt55923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49214 to i64
%ae49214 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55923)
store volatile %struct.ScmObj* %ae49214, %struct.ScmObj** %stackaddr$makeclosure55922, align 8
%argslist54525$ae492120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55924 = alloca %struct.ScmObj*, align 8
%argslist54525$ae492121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49214, %struct.ScmObj* %argslist54525$ae492120)
store volatile %struct.ScmObj* %argslist54525$ae492121, %struct.ScmObj** %stackaddr$prim55924, align 8
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%argslist54525$ae492122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist54525$ae492121)
store volatile %struct.ScmObj* %argslist54525$ae492122, %struct.ScmObj** %stackaddr$prim55925, align 8
%clofunc55926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49212)
musttail call tailcc void %clofunc55926(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist54525$ae492122)
ret void
}

define tailcc void @proc_clo$ae49212(%struct.ScmObj* %env$ae49212,%struct.ScmObj* %current_45args54498) {
%stackaddr$env-ref55927 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55927
%stackaddr$env-ref55928 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55928
%stackaddr$env-ref55929 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref55929
%stackaddr$env-ref55930 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55930
%stackaddr$env-ref55931 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 4)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55931
%stackaddr$env-ref55932 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55932
%stackaddr$env-ref55933 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 6)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref55933
%stackaddr$env-ref55934 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 7)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55934
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54498)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$prim55936 = alloca %struct.ScmObj*, align 8
%current_45args54499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54498)
store volatile %struct.ScmObj* %current_45args54499, %struct.ScmObj** %stackaddr$prim55936, align 8
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54499)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$makeclosure55938 = alloca %struct.ScmObj*, align 8
%fptrToInt55939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49233 to i64
%ae49233 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55939)
store volatile %struct.ScmObj* %ae49233, %struct.ScmObj** %stackaddr$makeclosure55938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %k48369, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49233, %struct.ScmObj* %lsts_4348061, i64 5)
%argslist54520$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%argslist54520$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist54520$_37map1480420)
store volatile %struct.ScmObj* %argslist54520$_37map1480421, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%argslist54520$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist54520$_37map1480421)
store volatile %struct.ScmObj* %argslist54520$_37map1480422, %struct.ScmObj** %stackaddr$prim55941, align 8
%stackaddr$prim55942 = alloca %struct.ScmObj*, align 8
%argslist54520$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49233, %struct.ScmObj* %argslist54520$_37map1480422)
store volatile %struct.ScmObj* %argslist54520$_37map1480423, %struct.ScmObj** %stackaddr$prim55942, align 8
%clofunc55943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc55943(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist54520$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49233(%struct.ScmObj* %env$ae49233,%struct.ScmObj* %current_45args54501) {
%stackaddr$env-ref55944 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55944
%stackaddr$env-ref55945 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55945
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$env-ref55947 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 3)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55947
%stackaddr$env-ref55948 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55948
%stackaddr$env-ref55949 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49233, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref55949
%stackaddr$prim55950 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54501)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim55950, align 8
%stackaddr$prim55951 = alloca %struct.ScmObj*, align 8
%current_45args54502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54501)
store volatile %struct.ScmObj* %current_45args54502, %struct.ScmObj** %stackaddr$prim55951, align 8
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54502)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$makeclosure55953 = alloca %struct.ScmObj*, align 8
%fptrToInt55954 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49236 to i64
%ae49236 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55954)
store volatile %struct.ScmObj* %ae49236, %struct.ScmObj** %stackaddr$makeclosure55953, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %k48369, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %vs48059, i64 6)
%ae49237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55955 = alloca %struct.ScmObj*, align 8
%fptrToInt55956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49238 to i64
%ae49238 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55956)
store volatile %struct.ScmObj* %ae49238, %struct.ScmObj** %stackaddr$makeclosure55955, align 8
%argslist54519$ae492360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%argslist54519$ae492361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist54519$ae492360)
store volatile %struct.ScmObj* %argslist54519$ae492361, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%argslist54519$ae492362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist54519$ae492361)
store volatile %struct.ScmObj* %argslist54519$ae492362, %struct.ScmObj** %stackaddr$prim55958, align 8
%clofunc55959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49236)
musttail call tailcc void %clofunc55959(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist54519$ae492362)
ret void
}

define tailcc void @proc_clo$ae49236(%struct.ScmObj* %env$ae49236,%struct.ScmObj* %current_45args54504) {
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$env-ref55963 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 3)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55963
%stackaddr$env-ref55964 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55964
%stackaddr$env-ref55965 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref55965
%stackaddr$env-ref55966 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 6)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref55966
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%current_45args54505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54504)
store volatile %struct.ScmObj* %current_45args54505, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54505)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55970, align 8
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48186)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim55971, align 8
%stackaddr$makeclosure55972 = alloca %struct.ScmObj*, align 8
%fptrToInt55973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49262 to i64
%ae49262 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55973)
store volatile %struct.ScmObj* %ae49262, %struct.ScmObj** %stackaddr$makeclosure55972, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %anf_45bind48185, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %f48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %k48369, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49262, %struct.ScmObj* %vs48059, i64 4)
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%cpsargs48380 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49262, %struct.ScmObj* %anf_45bind48187)
store volatile %struct.ScmObj* %cpsargs48380, %struct.ScmObj** %stackaddr$prim55974, align 8
%clofunc55975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc55975(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48380)
ret void
}

define tailcc void @proc_clo$ae49262(%struct.ScmObj* %env$ae49262,%struct.ScmObj* %current_45args54507) {
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 0)
store %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$env-ref55977 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 1)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55977
%stackaddr$env-ref55978 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 2)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55978
%stackaddr$env-ref55979 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55979
%stackaddr$env-ref55980 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49262, i64 4)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref55980
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%current_45args54508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54507)
store volatile %struct.ScmObj* %current_45args54508, %struct.ScmObj** %stackaddr$prim55982, align 8
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54508)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim55983, align 8
%ae49267 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %ae49267)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$makeclosure55985 = alloca %struct.ScmObj*, align 8
%fptrToInt55986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49269 to i64
%ae49269 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55986)
store volatile %struct.ScmObj* %ae49269, %struct.ScmObj** %stackaddr$makeclosure55985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49269, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49269, %struct.ScmObj* %k48369, i64 1)
%argslist54513$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%argslist54513$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist54513$_37foldr1480460)
store volatile %struct.ScmObj* %argslist54513$_37foldr1480461, %struct.ScmObj** %stackaddr$prim55987, align 8
%stackaddr$prim55988 = alloca %struct.ScmObj*, align 8
%argslist54513$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist54513$_37foldr1480461)
store volatile %struct.ScmObj* %argslist54513$_37foldr1480462, %struct.ScmObj** %stackaddr$prim55988, align 8
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%argslist54513$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist54513$_37foldr1480462)
store volatile %struct.ScmObj* %argslist54513$_37foldr1480463, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%argslist54513$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49269, %struct.ScmObj* %argslist54513$_37foldr1480463)
store volatile %struct.ScmObj* %argslist54513$_37foldr1480464, %struct.ScmObj** %stackaddr$prim55990, align 8
%clofunc55991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc55991(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist54513$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49269(%struct.ScmObj* %env$ae49269,%struct.ScmObj* %current_45args54510) {
%stackaddr$env-ref55992 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49269, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref55992
%stackaddr$env-ref55993 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49269, i64 1)
store %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$env-ref55993
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%current_45args54511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54510)
store volatile %struct.ScmObj* %current_45args54511, %struct.ScmObj** %stackaddr$prim55995, align 8
%stackaddr$prim55996 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54511)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim55996, align 8
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%cpsargs48379 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48369, %struct.ScmObj* %anf_45bind48190)
store volatile %struct.ScmObj* %cpsargs48379, %struct.ScmObj** %stackaddr$prim55997, align 8
%clofunc55998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc55998(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48379)
ret void
}

define tailcc void @proc_clo$ae49238(%struct.ScmObj* %env$ae49238,%struct.ScmObj* %current_45args54514) {
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54514)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%current_45args54515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54514)
store volatile %struct.ScmObj* %current_45args54515, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54515)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56001, align 8
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%current_45args54516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54515)
store volatile %struct.ScmObj* %current_45args54516, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54516)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56003, align 8
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%cpsprim48382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48382, %struct.ScmObj** %stackaddr$prim56004, align 8
%ae49242 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54518$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54518$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48382, %struct.ScmObj* %argslist54518$k483810)
store volatile %struct.ScmObj* %argslist54518$k483811, %struct.ScmObj** %stackaddr$prim56005, align 8
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%argslist54518$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist54518$k483811)
store volatile %struct.ScmObj* %argslist54518$k483812, %struct.ScmObj** %stackaddr$prim56006, align 8
%clofunc56007 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc56007(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54518$k483812)
ret void
}

define tailcc void @proc_clo$ae49214(%struct.ScmObj* %env$ae49214,%struct.ScmObj* %current_45args54521) {
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$prim56008, align 8
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%current_45args54522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54521)
store volatile %struct.ScmObj* %current_45args54522, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54522)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%cpsprim48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48384, %struct.ScmObj** %stackaddr$prim56011, align 8
%ae49217 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54524$k483830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%argslist54524$k483831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48384, %struct.ScmObj* %argslist54524$k483830)
store volatile %struct.ScmObj* %argslist54524$k483831, %struct.ScmObj** %stackaddr$prim56012, align 8
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%argslist54524$k483832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49217, %struct.ScmObj* %argslist54524$k483831)
store volatile %struct.ScmObj* %argslist54524$k483832, %struct.ScmObj** %stackaddr$prim56013, align 8
%clofunc56014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48383)
musttail call tailcc void %clofunc56014(%struct.ScmObj* %k48383, %struct.ScmObj* %argslist54524$k483832)
ret void
}

define tailcc void @proc_clo$ae49190(%struct.ScmObj* %env$ae49190,%struct.ScmObj* %current_45args54527) {
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54527)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim56015, align 8
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%current_45args54528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54527)
store volatile %struct.ScmObj* %current_45args54528, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54528)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56017, align 8
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%cpsprim48386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48386, %struct.ScmObj** %stackaddr$prim56018, align 8
%ae49193 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54530$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%argslist54530$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48386, %struct.ScmObj* %argslist54530$k483850)
store volatile %struct.ScmObj* %argslist54530$k483851, %struct.ScmObj** %stackaddr$prim56019, align 8
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%argslist54530$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49193, %struct.ScmObj* %argslist54530$k483851)
store volatile %struct.ScmObj* %argslist54530$k483852, %struct.ScmObj** %stackaddr$prim56020, align 8
%clofunc56021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc56021(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist54530$k483852)
ret void
}

define tailcc void @proc_clo$ae49142(%struct.ScmObj* %env$ae49142,%struct.ScmObj* %current_45args54533) {
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54533)
store volatile %struct.ScmObj* %k48387, %struct.ScmObj** %stackaddr$prim56022, align 8
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%current_45args54534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54533)
store volatile %struct.ScmObj* %current_45args54534, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54534)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%current_45args54535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54534)
store volatile %struct.ScmObj* %current_45args54535, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54535)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56026, align 8
%truthy$cmp56027 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56027 = icmp eq i64 %truthy$cmp56027, 1
br i1 %cmp$cmp56027, label %truebranch$cmp56027, label %falsebranch$cmp56027
truebranch$cmp56027:
%ae49145 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54537$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%argslist54537$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist54537$k483870)
store volatile %struct.ScmObj* %argslist54537$k483871, %struct.ScmObj** %stackaddr$prim56028, align 8
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%argslist54537$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49145, %struct.ScmObj* %argslist54537$k483871)
store volatile %struct.ScmObj* %argslist54537$k483872, %struct.ScmObj** %stackaddr$prim56029, align 8
%clofunc56030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56030(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist54537$k483872)
ret void
falsebranch$cmp56027:
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%cpsprim48388 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48388, %struct.ScmObj** %stackaddr$prim56031, align 8
%ae49152 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54538$k483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%argslist54538$k483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48388, %struct.ScmObj* %argslist54538$k483870)
store volatile %struct.ScmObj* %argslist54538$k483871, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%argslist54538$k483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49152, %struct.ScmObj* %argslist54538$k483871)
store volatile %struct.ScmObj* %argslist54538$k483872, %struct.ScmObj** %stackaddr$prim56033, align 8
%clofunc56034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48387)
musttail call tailcc void %clofunc56034(%struct.ScmObj* %k48387, %struct.ScmObj* %argslist54538$k483872)
ret void
}

define tailcc void @proc_clo$ae49099(%struct.ScmObj* %env$ae49099,%struct.ScmObj* %current_45args54542) {
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49099, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$env-ref56036 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49099, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56036
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54542)
store volatile %struct.ScmObj* %k48389, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%current_45args54543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54542)
store volatile %struct.ScmObj* %current_45args54543, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%current_45args54544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54543)
store volatile %struct.ScmObj* %current_45args54544, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54544)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56041, align 8
%stackaddr$makeclosure56042 = alloca %struct.ScmObj*, align 8
%fptrToInt56043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49101 to i64
%ae49101 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56043)
store volatile %struct.ScmObj* %ae49101, %struct.ScmObj** %stackaddr$makeclosure56042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49101, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49101, %struct.ScmObj* %k48389, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49101, %struct.ScmObj* %lst48067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49101, %struct.ScmObj* %n48066, i64 3)
%argslist54550$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%argslist54550$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54550$_37length480350)
store volatile %struct.ScmObj* %argslist54550$_37length480351, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist54550$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49101, %struct.ScmObj* %argslist54550$_37length480351)
store volatile %struct.ScmObj* %argslist54550$_37length480352, %struct.ScmObj** %stackaddr$prim56045, align 8
%clofunc56046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56046(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist54550$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49101(%struct.ScmObj* %env$ae49101,%struct.ScmObj* %current_45args54546) {
%stackaddr$env-ref56047 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49101, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56047
%stackaddr$env-ref56048 = alloca %struct.ScmObj*, align 8
%k48389 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49101, i64 1)
store %struct.ScmObj* %k48389, %struct.ScmObj** %stackaddr$env-ref56048
%stackaddr$env-ref56049 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49101, i64 2)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56049
%stackaddr$env-ref56050 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49101, i64 3)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56050
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%current_45args54547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54546)
store volatile %struct.ScmObj* %current_45args54547, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54547)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56054, align 8
%argslist54549$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56055 = alloca %struct.ScmObj*, align 8
%argslist54549$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist54549$_37take480380)
store volatile %struct.ScmObj* %argslist54549$_37take480381, %struct.ScmObj** %stackaddr$prim56055, align 8
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%argslist54549$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist54549$_37take480381)
store volatile %struct.ScmObj* %argslist54549$_37take480382, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%argslist54549$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48389, %struct.ScmObj* %argslist54549$_37take480382)
store volatile %struct.ScmObj* %argslist54549$_37take480383, %struct.ScmObj** %stackaddr$prim56057, align 8
%clofunc56058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc56058(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist54549$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49045(%struct.ScmObj* %env$ae49045,%struct.ScmObj* %current_45args54552) {
%stackaddr$env-ref56059 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49045, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56059
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54552)
store volatile %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$prim56060, align 8
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%current_45args54553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54552)
store volatile %struct.ScmObj* %current_45args54553, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54553)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$makeclosure56063 = alloca %struct.ScmObj*, align 8
%fptrToInt56064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49046 to i64
%ae49046 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56064)
store volatile %struct.ScmObj* %ae49046, %struct.ScmObj** %stackaddr$makeclosure56063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %k48391, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %lst48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37foldl148030, i64 2)
%ae49047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56065 = alloca %struct.ScmObj*, align 8
%fptrToInt56066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49048 to i64
%ae49048 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56066)
store volatile %struct.ScmObj* %ae49048, %struct.ScmObj** %stackaddr$makeclosure56065, align 8
%argslist54564$ae490460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%argslist54564$ae490461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49048, %struct.ScmObj* %argslist54564$ae490460)
store volatile %struct.ScmObj* %argslist54564$ae490461, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%argslist54564$ae490462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49047, %struct.ScmObj* %argslist54564$ae490461)
store volatile %struct.ScmObj* %argslist54564$ae490462, %struct.ScmObj** %stackaddr$prim56068, align 8
%clofunc56069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49046)
musttail call tailcc void %clofunc56069(%struct.ScmObj* %ae49046, %struct.ScmObj* %argslist54564$ae490462)
ret void
}

define tailcc void @proc_clo$ae49046(%struct.ScmObj* %env$ae49046,%struct.ScmObj* %current_45args54555) {
%stackaddr$env-ref56070 = alloca %struct.ScmObj*, align 8
%k48391 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 0)
store %struct.ScmObj* %k48391, %struct.ScmObj** %stackaddr$env-ref56070
%stackaddr$env-ref56071 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 1)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref56071
%stackaddr$env-ref56072 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56072
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54555)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%current_45args54556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54555)
store volatile %struct.ScmObj* %current_45args54556, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54556)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56075, align 8
%ae49067 = call %struct.ScmObj* @const_init_null()
%argslist54558$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%argslist54558$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist54558$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54558$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%argslist54558$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49067, %struct.ScmObj* %argslist54558$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54558$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56077, align 8
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%argslist54558$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist54558$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54558$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist54558$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48391, %struct.ScmObj* %argslist54558$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54558$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56079, align 8
%clofunc56080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56080(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54558$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49048(%struct.ScmObj* %env$ae49048,%struct.ScmObj* %current_45args54559) {
%stackaddr$prim56081 = alloca %struct.ScmObj*, align 8
%k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54559)
store volatile %struct.ScmObj* %k48393, %struct.ScmObj** %stackaddr$prim56081, align 8
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%current_45args54560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54559)
store volatile %struct.ScmObj* %current_45args54560, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%current_45args54561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54560)
store volatile %struct.ScmObj* %current_45args54561, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54561)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim56085, align 8
%ae49050 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54563$k483930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%argslist54563$k483931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist54563$k483930)
store volatile %struct.ScmObj* %argslist54563$k483931, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%argslist54563$k483932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49050, %struct.ScmObj* %argslist54563$k483931)
store volatile %struct.ScmObj* %argslist54563$k483932, %struct.ScmObj** %stackaddr$prim56087, align 8
%clofunc56088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48393)
musttail call tailcc void %clofunc56088(%struct.ScmObj* %k48393, %struct.ScmObj* %argslist54563$k483932)
ret void
}

define tailcc void @proc_clo$ae48966(%struct.ScmObj* %env$ae48966,%struct.ScmObj* %current_45args54567) {
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54567)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%current_45args54568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54567)
store volatile %struct.ScmObj* %current_45args54568, %struct.ScmObj** %stackaddr$prim56090, align 8
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54568)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim56091, align 8
%ae48968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56092 = alloca %struct.ScmObj*, align 8
%fptrToInt56093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48969 to i64
%ae48969 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56093)
store volatile %struct.ScmObj* %ae48969, %struct.ScmObj** %stackaddr$makeclosure56092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54581$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%argslist54581$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist54581$k483940)
store volatile %struct.ScmObj* %argslist54581$k483941, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%argslist54581$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48968, %struct.ScmObj* %argslist54581$k483941)
store volatile %struct.ScmObj* %argslist54581$k483942, %struct.ScmObj** %stackaddr$prim56095, align 8
%clofunc56096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc56096(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist54581$k483942)
ret void
}

define tailcc void @proc_clo$ae48969(%struct.ScmObj* %env$ae48969,%struct.ScmObj* %current_45args54570) {
%stackaddr$env-ref56097 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56097
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54570)
store volatile %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%current_45args54571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54570)
store volatile %struct.ScmObj* %current_45args54571, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54571)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%current_45args54572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54571)
store volatile %struct.ScmObj* %current_45args54572, %struct.ScmObj** %stackaddr$prim56101, align 8
%stackaddr$prim56102 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim56102, align 8
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%current_45args54573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54572)
store volatile %struct.ScmObj* %current_45args54573, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54573)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim56104, align 8
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56105, align 8
%truthy$cmp56106 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48171)
%cmp$cmp56106 = icmp eq i64 %truthy$cmp56106, 1
br i1 %cmp$cmp56106, label %truebranch$cmp56106, label %falsebranch$cmp56106
truebranch$cmp56106:
%ae48973 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54575$k483950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%argslist54575$k483951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54575$k483950)
store volatile %struct.ScmObj* %argslist54575$k483951, %struct.ScmObj** %stackaddr$prim56107, align 8
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%argslist54575$k483952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48973, %struct.ScmObj* %argslist54575$k483951)
store volatile %struct.ScmObj* %argslist54575$k483952, %struct.ScmObj** %stackaddr$prim56108, align 8
%clofunc56109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48395)
musttail call tailcc void %clofunc56109(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist54575$k483952)
ret void
falsebranch$cmp56106:
%stackaddr$prim56110 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim56110, align 8
%stackaddr$makeclosure56111 = alloca %struct.ScmObj*, align 8
%fptrToInt56112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48980 to i64
%ae48980 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56112)
store volatile %struct.ScmObj* %ae48980, %struct.ScmObj** %stackaddr$makeclosure56111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %lst48032, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %_37foldl148031, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %k48395, i64 3)
%argslist54580$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%argslist54580$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist54580$f480340)
store volatile %struct.ScmObj* %argslist54580$f480341, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%argslist54580$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist54580$f480341)
store volatile %struct.ScmObj* %argslist54580$f480342, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%argslist54580$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48980, %struct.ScmObj* %argslist54580$f480342)
store volatile %struct.ScmObj* %argslist54580$f480343, %struct.ScmObj** %stackaddr$prim56115, align 8
%clofunc56116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc56116(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54580$f480343)
ret void
}

define tailcc void @proc_clo$ae48980(%struct.ScmObj* %env$ae48980,%struct.ScmObj* %current_45args54576) {
%stackaddr$env-ref56117 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref56117
%stackaddr$env-ref56118 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 1)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref56118
%stackaddr$env-ref56119 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 2)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56119
%stackaddr$env-ref56120 = alloca %struct.ScmObj*, align 8
%k48395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 3)
store %struct.ScmObj* %k48395, %struct.ScmObj** %stackaddr$env-ref56120
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54576)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim56121, align 8
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%current_45args54577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54576)
store volatile %struct.ScmObj* %current_45args54577, %struct.ScmObj** %stackaddr$prim56122, align 8
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54577)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56123, align 8
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim56124, align 8
%argslist54579$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%argslist54579$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist54579$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54579$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%argslist54579$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist54579$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54579$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%argslist54579$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist54579$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54579$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56127, align 8
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%argslist54579$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48395, %struct.ScmObj* %argslist54579$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54579$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56128, align 8
%clofunc56129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56129(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54579$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48883(%struct.ScmObj* %env$ae48883,%struct.ScmObj* %current_45args54584) {
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54584)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%current_45args54585 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54584)
store volatile %struct.ScmObj* %current_45args54585, %struct.ScmObj** %stackaddr$prim56131, align 8
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54585)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim56132, align 8
%ae48885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56133 = alloca %struct.ScmObj*, align 8
%fptrToInt56134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48886 to i64
%ae48886 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56134)
store volatile %struct.ScmObj* %ae48886, %struct.ScmObj** %stackaddr$makeclosure56133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48886, %struct.ScmObj* %_37length48036, i64 0)
%argslist54596$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%argslist54596$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48886, %struct.ScmObj* %argslist54596$k483970)
store volatile %struct.ScmObj* %argslist54596$k483971, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%argslist54596$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist54596$k483971)
store volatile %struct.ScmObj* %argslist54596$k483972, %struct.ScmObj** %stackaddr$prim56136, align 8
%clofunc56137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56137(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist54596$k483972)
ret void
}

define tailcc void @proc_clo$ae48886(%struct.ScmObj* %env$ae48886,%struct.ScmObj* %current_45args54587) {
%stackaddr$env-ref56138 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48886, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56138
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54587)
store volatile %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$prim56139, align 8
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%current_45args54588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54587)
store volatile %struct.ScmObj* %current_45args54588, %struct.ScmObj** %stackaddr$prim56140, align 8
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54588)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim56141, align 8
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56142, align 8
%truthy$cmp56143 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48167)
%cmp$cmp56143 = icmp eq i64 %truthy$cmp56143, 1
br i1 %cmp$cmp56143, label %truebranch$cmp56143, label %falsebranch$cmp56143
truebranch$cmp56143:
%ae48890 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48891 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54590$k483980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%argslist54590$k483981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48891, %struct.ScmObj* %argslist54590$k483980)
store volatile %struct.ScmObj* %argslist54590$k483981, %struct.ScmObj** %stackaddr$prim56144, align 8
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%argslist54590$k483982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist54590$k483981)
store volatile %struct.ScmObj* %argslist54590$k483982, %struct.ScmObj** %stackaddr$prim56145, align 8
%clofunc56146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48398)
musttail call tailcc void %clofunc56146(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist54590$k483982)
ret void
falsebranch$cmp56143:
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56147, align 8
%stackaddr$makeclosure56148 = alloca %struct.ScmObj*, align 8
%fptrToInt56149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48900 to i64
%ae48900 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56149)
store volatile %struct.ScmObj* %ae48900, %struct.ScmObj** %stackaddr$makeclosure56148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %k48398, i64 0)
%argslist54595$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%argslist54595$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist54595$_37length480360)
store volatile %struct.ScmObj* %argslist54595$_37length480361, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist54595$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist54595$_37length480361)
store volatile %struct.ScmObj* %argslist54595$_37length480362, %struct.ScmObj** %stackaddr$prim56151, align 8
%clofunc56152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56152(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54595$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48900(%struct.ScmObj* %env$ae48900,%struct.ScmObj* %current_45args54591) {
%stackaddr$env-ref56153 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 0)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref56153
%stackaddr$prim56154 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54591)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim56154, align 8
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%current_45args54592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54591)
store volatile %struct.ScmObj* %current_45args54592, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54592)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim56156, align 8
%ae48902 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%cpsprim48400 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48902, %struct.ScmObj* %anf_45bind48169)
store volatile %struct.ScmObj* %cpsprim48400, %struct.ScmObj** %stackaddr$prim56157, align 8
%ae48905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54594$k483980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%argslist54594$k483981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48400, %struct.ScmObj* %argslist54594$k483980)
store volatile %struct.ScmObj* %argslist54594$k483981, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%argslist54594$k483982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist54594$k483981)
store volatile %struct.ScmObj* %argslist54594$k483982, %struct.ScmObj** %stackaddr$prim56159, align 8
%clofunc56160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48398)
musttail call tailcc void %clofunc56160(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist54594$k483982)
ret void
}

define tailcc void @proc_clo$ae48733(%struct.ScmObj* %env$ae48733,%struct.ScmObj* %current_45args54599) {
%stackaddr$prim56161 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54599)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim56161, align 8
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%current_45args54600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54599)
store volatile %struct.ScmObj* %current_45args54600, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54600)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim56163, align 8
%ae48735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56164 = alloca %struct.ScmObj*, align 8
%fptrToInt56165 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48736 to i64
%ae48736 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56165)
store volatile %struct.ScmObj* %ae48736, %struct.ScmObj** %stackaddr$makeclosure56164, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48736, %struct.ScmObj* %_37take48039, i64 0)
%argslist54613$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%argslist54613$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48736, %struct.ScmObj* %argslist54613$k484010)
store volatile %struct.ScmObj* %argslist54613$k484011, %struct.ScmObj** %stackaddr$prim56166, align 8
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist54613$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48735, %struct.ScmObj* %argslist54613$k484011)
store volatile %struct.ScmObj* %argslist54613$k484012, %struct.ScmObj** %stackaddr$prim56167, align 8
%clofunc56168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc56168(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54613$k484012)
ret void
}

define tailcc void @proc_clo$ae48736(%struct.ScmObj* %env$ae48736,%struct.ScmObj* %current_45args54602) {
%stackaddr$env-ref56169 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48736, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref56169
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %k48402, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%current_45args54603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54602)
store volatile %struct.ScmObj* %current_45args54603, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%current_45args54604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54603)
store volatile %struct.ScmObj* %current_45args54604, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54604)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim56174, align 8
%ae48738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48738)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim56175, align 8
%truthy$cmp56176 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48160)
%cmp$cmp56176 = icmp eq i64 %truthy$cmp56176, 1
br i1 %cmp$cmp56176, label %truebranch$cmp56176, label %falsebranch$cmp56176
truebranch$cmp56176:
%ae48741 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48742 = call %struct.ScmObj* @const_init_null()
%argslist54606$k484020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%argslist54606$k484021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48742, %struct.ScmObj* %argslist54606$k484020)
store volatile %struct.ScmObj* %argslist54606$k484021, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%argslist54606$k484022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48741, %struct.ScmObj* %argslist54606$k484021)
store volatile %struct.ScmObj* %argslist54606$k484022, %struct.ScmObj** %stackaddr$prim56178, align 8
%clofunc56179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48402)
musttail call tailcc void %clofunc56179(%struct.ScmObj* %k48402, %struct.ScmObj* %argslist54606$k484022)
ret void
falsebranch$cmp56176:
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim56180, align 8
%truthy$cmp56181 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48161)
%cmp$cmp56181 = icmp eq i64 %truthy$cmp56181, 1
br i1 %cmp$cmp56181, label %truebranch$cmp56181, label %falsebranch$cmp56181
truebranch$cmp56181:
%ae48752 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48753 = call %struct.ScmObj* @const_init_null()
%argslist54607$k484020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%argslist54607$k484021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48753, %struct.ScmObj* %argslist54607$k484020)
store volatile %struct.ScmObj* %argslist54607$k484021, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%argslist54607$k484022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48752, %struct.ScmObj* %argslist54607$k484021)
store volatile %struct.ScmObj* %argslist54607$k484022, %struct.ScmObj** %stackaddr$prim56183, align 8
%clofunc56184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48402)
musttail call tailcc void %clofunc56184(%struct.ScmObj* %k48402, %struct.ScmObj* %argslist54607$k484022)
ret void
falsebranch$cmp56181:
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim56186, align 8
%ae48763 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48763)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim56187, align 8
%stackaddr$makeclosure56188 = alloca %struct.ScmObj*, align 8
%fptrToInt56189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48765 to i64
%ae48765 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56189)
store volatile %struct.ScmObj* %ae48765, %struct.ScmObj** %stackaddr$makeclosure56188, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48765, %struct.ScmObj* %k48402, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48765, %struct.ScmObj* %anf_45bind48162, i64 1)
%argslist54612$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%argslist54612$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist54612$_37take480390)
store volatile %struct.ScmObj* %argslist54612$_37take480391, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%argslist54612$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist54612$_37take480391)
store volatile %struct.ScmObj* %argslist54612$_37take480392, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%argslist54612$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48765, %struct.ScmObj* %argslist54612$_37take480392)
store volatile %struct.ScmObj* %argslist54612$_37take480393, %struct.ScmObj** %stackaddr$prim56192, align 8
%clofunc56193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc56193(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54612$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48765(%struct.ScmObj* %env$ae48765,%struct.ScmObj* %current_45args54608) {
%stackaddr$env-ref56194 = alloca %struct.ScmObj*, align 8
%k48402 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48765, i64 0)
store %struct.ScmObj* %k48402, %struct.ScmObj** %stackaddr$env-ref56194
%stackaddr$env-ref56195 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48765, i64 1)
store %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$env-ref56195
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54608)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim56196, align 8
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%current_45args54609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54608)
store volatile %struct.ScmObj* %current_45args54609, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54609)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim56198, align 8
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%cpsprim48404 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %anf_45bind48165)
store volatile %struct.ScmObj* %cpsprim48404, %struct.ScmObj** %stackaddr$prim56199, align 8
%ae48771 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54611$k484020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist54611$k484021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48404, %struct.ScmObj* %argslist54611$k484020)
store volatile %struct.ScmObj* %argslist54611$k484021, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%argslist54611$k484022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48771, %struct.ScmObj* %argslist54611$k484021)
store volatile %struct.ScmObj* %argslist54611$k484022, %struct.ScmObj** %stackaddr$prim56201, align 8
%clofunc56202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48402)
musttail call tailcc void %clofunc56202(%struct.ScmObj* %k48402, %struct.ScmObj* %argslist54611$k484022)
ret void
}

define tailcc void @proc_clo$ae48636(%struct.ScmObj* %env$ae48636,%struct.ScmObj* %current_45args54616) {
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54616)
store volatile %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$prim56203, align 8
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%current_45args54617 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54616)
store volatile %struct.ScmObj* %current_45args54617, %struct.ScmObj** %stackaddr$prim56204, align 8
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54617)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim56205, align 8
%ae48638 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56206 = alloca %struct.ScmObj*, align 8
%fptrToInt56207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48639 to i64
%ae48639 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56207)
store volatile %struct.ScmObj* %ae48639, %struct.ScmObj** %stackaddr$makeclosure56206, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48639, %struct.ScmObj* %_37map48043, i64 0)
%argslist54633$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%argslist54633$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48639, %struct.ScmObj* %argslist54633$k484050)
store volatile %struct.ScmObj* %argslist54633$k484051, %struct.ScmObj** %stackaddr$prim56208, align 8
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%argslist54633$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist54633$k484051)
store volatile %struct.ScmObj* %argslist54633$k484052, %struct.ScmObj** %stackaddr$prim56209, align 8
%clofunc56210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc56210(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist54633$k484052)
ret void
}

define tailcc void @proc_clo$ae48639(%struct.ScmObj* %env$ae48639,%struct.ScmObj* %current_45args54619) {
%stackaddr$env-ref56211 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48639, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56211
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54619)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim56212, align 8
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%current_45args54620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54619)
store volatile %struct.ScmObj* %current_45args54620, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%current_45args54621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54620)
store volatile %struct.ScmObj* %current_45args54621, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$prim56216 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54621)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim56216, align 8
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim56217, align 8
%truthy$cmp56218 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48154)
%cmp$cmp56218 = icmp eq i64 %truthy$cmp56218, 1
br i1 %cmp$cmp56218, label %truebranch$cmp56218, label %falsebranch$cmp56218
truebranch$cmp56218:
%ae48643 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48644 = call %struct.ScmObj* @const_init_null()
%argslist54623$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%argslist54623$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48644, %struct.ScmObj* %argslist54623$k484060)
store volatile %struct.ScmObj* %argslist54623$k484061, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%argslist54623$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48643, %struct.ScmObj* %argslist54623$k484061)
store volatile %struct.ScmObj* %argslist54623$k484062, %struct.ScmObj** %stackaddr$prim56220, align 8
%clofunc56221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56221(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist54623$k484062)
ret void
falsebranch$cmp56218:
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim56222, align 8
%stackaddr$makeclosure56223 = alloca %struct.ScmObj*, align 8
%fptrToInt56224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48653 to i64
%ae48653 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56224)
store volatile %struct.ScmObj* %ae48653, %struct.ScmObj** %stackaddr$makeclosure56223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %k48406, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %f48045, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %lst48044, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %_37map48043, i64 3)
%argslist54632$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%argslist54632$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %argslist54632$f480450)
store volatile %struct.ScmObj* %argslist54632$f480451, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%argslist54632$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48653, %struct.ScmObj* %argslist54632$f480451)
store volatile %struct.ScmObj* %argslist54632$f480452, %struct.ScmObj** %stackaddr$prim56226, align 8
%clofunc56227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc56227(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54632$f480452)
ret void
}

define tailcc void @proc_clo$ae48653(%struct.ScmObj* %env$ae48653,%struct.ScmObj* %current_45args54624) {
%stackaddr$env-ref56228 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 0)
store %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$env-ref56228
%stackaddr$env-ref56229 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 1)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref56229
%stackaddr$env-ref56230 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 2)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref56230
%stackaddr$env-ref56231 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 3)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref56231
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54624)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim56232, align 8
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%current_45args54625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54624)
store volatile %struct.ScmObj* %current_45args54625, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54625)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim56235, align 8
%stackaddr$makeclosure56236 = alloca %struct.ScmObj*, align 8
%fptrToInt56237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48657 to i64
%ae48657 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56237)
store volatile %struct.ScmObj* %ae48657, %struct.ScmObj** %stackaddr$makeclosure56236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48657, %struct.ScmObj* %k48406, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48657, %struct.ScmObj* %anf_45bind48156, i64 1)
%argslist54631$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%argslist54631$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54631$_37map480430)
store volatile %struct.ScmObj* %argslist54631$_37map480431, %struct.ScmObj** %stackaddr$prim56238, align 8
%stackaddr$prim56239 = alloca %struct.ScmObj*, align 8
%argslist54631$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist54631$_37map480431)
store volatile %struct.ScmObj* %argslist54631$_37map480432, %struct.ScmObj** %stackaddr$prim56239, align 8
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%argslist54631$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48657, %struct.ScmObj* %argslist54631$_37map480432)
store volatile %struct.ScmObj* %argslist54631$_37map480433, %struct.ScmObj** %stackaddr$prim56240, align 8
%clofunc56241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc56241(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist54631$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48657(%struct.ScmObj* %env$ae48657,%struct.ScmObj* %current_45args54627) {
%stackaddr$env-ref56242 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48657, i64 0)
store %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$env-ref56242
%stackaddr$env-ref56243 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48657, i64 1)
store %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$env-ref56243
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%_95k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54627)
store volatile %struct.ScmObj* %_95k48408, %struct.ScmObj** %stackaddr$prim56244, align 8
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%current_45args54628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54627)
store volatile %struct.ScmObj* %current_45args54628, %struct.ScmObj** %stackaddr$prim56245, align 8
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54628)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%cpsprim48409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %anf_45bind48158)
store volatile %struct.ScmObj* %cpsprim48409, %struct.ScmObj** %stackaddr$prim56247, align 8
%ae48663 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54630$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%argslist54630$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48409, %struct.ScmObj* %argslist54630$k484060)
store volatile %struct.ScmObj* %argslist54630$k484061, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%argslist54630$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48663, %struct.ScmObj* %argslist54630$k484061)
store volatile %struct.ScmObj* %argslist54630$k484062, %struct.ScmObj** %stackaddr$prim56249, align 8
%clofunc56250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56250(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist54630$k484062)
ret void
}

define tailcc void @proc_clo$ae48556(%struct.ScmObj* %env$ae48556,%struct.ScmObj* %current_45args54636) {
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54636)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%current_45args54637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54636)
store volatile %struct.ScmObj* %current_45args54637, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54637)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim56253, align 8
%ae48558 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56254 = alloca %struct.ScmObj*, align 8
%fptrToInt56255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48559 to i64
%ae48559 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56255)
store volatile %struct.ScmObj* %ae48559, %struct.ScmObj** %stackaddr$makeclosure56254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54650$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%argslist54650$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48559, %struct.ScmObj* %argslist54650$k484100)
store volatile %struct.ScmObj* %argslist54650$k484101, %struct.ScmObj** %stackaddr$prim56256, align 8
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%argslist54650$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48558, %struct.ScmObj* %argslist54650$k484101)
store volatile %struct.ScmObj* %argslist54650$k484102, %struct.ScmObj** %stackaddr$prim56257, align 8
%clofunc56258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc56258(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist54650$k484102)
ret void
}

define tailcc void @proc_clo$ae48559(%struct.ScmObj* %env$ae48559,%struct.ScmObj* %current_45args54639) {
%stackaddr$env-ref56259 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref56259
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54639)
store volatile %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$prim56260, align 8
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%current_45args54640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54639)
store volatile %struct.ScmObj* %current_45args54640, %struct.ScmObj** %stackaddr$prim56261, align 8
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54640)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%current_45args54641 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54640)
store volatile %struct.ScmObj* %current_45args54641, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim56264, align 8
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%current_45args54642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54641)
store volatile %struct.ScmObj* %current_45args54642, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$prim56266 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54642)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim56266, align 8
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$prim56267, align 8
%truthy$cmp56268 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48149)
%cmp$cmp56268 = icmp eq i64 %truthy$cmp56268, 1
br i1 %cmp$cmp56268, label %truebranch$cmp56268, label %falsebranch$cmp56268
truebranch$cmp56268:
%ae48563 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54644$k484110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%argslist54644$k484111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54644$k484110)
store volatile %struct.ScmObj* %argslist54644$k484111, %struct.ScmObj** %stackaddr$prim56269, align 8
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%argslist54644$k484112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48563, %struct.ScmObj* %argslist54644$k484111)
store volatile %struct.ScmObj* %argslist54644$k484112, %struct.ScmObj** %stackaddr$prim56270, align 8
%clofunc56271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48411)
musttail call tailcc void %clofunc56271(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist54644$k484112)
ret void
falsebranch$cmp56268:
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim56272, align 8
%stackaddr$prim56273 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim56273, align 8
%stackaddr$makeclosure56274 = alloca %struct.ScmObj*, align 8
%fptrToInt56275 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48571 to i64
%ae48571 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56275)
store volatile %struct.ScmObj* %ae48571, %struct.ScmObj** %stackaddr$makeclosure56274, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48571, %struct.ScmObj* %anf_45bind48150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48571, %struct.ScmObj* %f48050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48571, %struct.ScmObj* %k48411, i64 2)
%argslist54649$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%argslist54649$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist54649$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54649$_37foldr1480471, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%argslist54649$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist54649$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54649$_37foldr1480472, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%argslist54649$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54649$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54649$_37foldr1480473, %struct.ScmObj** %stackaddr$prim56278, align 8
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%argslist54649$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48571, %struct.ScmObj* %argslist54649$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54649$_37foldr1480474, %struct.ScmObj** %stackaddr$prim56279, align 8
%clofunc56280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc56280(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54649$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48571(%struct.ScmObj* %env$ae48571,%struct.ScmObj* %current_45args54645) {
%stackaddr$env-ref56281 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48571, i64 0)
store %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$env-ref56281
%stackaddr$env-ref56282 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48571, i64 1)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref56282
%stackaddr$env-ref56283 = alloca %struct.ScmObj*, align 8
%k48411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48571, i64 2)
store %struct.ScmObj* %k48411, %struct.ScmObj** %stackaddr$env-ref56283
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%_95k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54645)
store volatile %struct.ScmObj* %_95k48412, %struct.ScmObj** %stackaddr$prim56284, align 8
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%current_45args54646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54645)
store volatile %struct.ScmObj* %current_45args54646, %struct.ScmObj** %stackaddr$prim56285, align 8
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54646)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim56286, align 8
%argslist54648$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist54648$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist54648$f480500)
store volatile %struct.ScmObj* %argslist54648$f480501, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%argslist54648$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %argslist54648$f480501)
store volatile %struct.ScmObj* %argslist54648$f480502, %struct.ScmObj** %stackaddr$prim56288, align 8
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%argslist54648$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48411, %struct.ScmObj* %argslist54648$f480502)
store volatile %struct.ScmObj* %argslist54648$f480503, %struct.ScmObj** %stackaddr$prim56289, align 8
%clofunc56290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc56290(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist54648$f480503)
ret void
}

define tailcc void @proc_clo$ae48439(%struct.ScmObj* %env$ae48439,%struct.ScmObj* %current_45args54653) {
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54653)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%current_45args54654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54653)
store volatile %struct.ScmObj* %current_45args54654, %struct.ScmObj** %stackaddr$prim56292, align 8
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54654)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim56293, align 8
%ae48441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56294 = alloca %struct.ScmObj*, align 8
%fptrToInt56295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48442 to i64
%ae48442 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56295)
store volatile %struct.ScmObj* %ae48442, %struct.ScmObj** %stackaddr$makeclosure56294, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48442, %struct.ScmObj* %y48027, i64 0)
%argslist54672$k484130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%argslist54672$k484131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48442, %struct.ScmObj* %argslist54672$k484130)
store volatile %struct.ScmObj* %argslist54672$k484131, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%argslist54672$k484132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48441, %struct.ScmObj* %argslist54672$k484131)
store volatile %struct.ScmObj* %argslist54672$k484132, %struct.ScmObj** %stackaddr$prim56297, align 8
%clofunc56298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48413)
musttail call tailcc void %clofunc56298(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist54672$k484132)
ret void
}

define tailcc void @proc_clo$ae48442(%struct.ScmObj* %env$ae48442,%struct.ScmObj* %current_45args54656) {
%stackaddr$env-ref56299 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48442, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56299
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54656)
store volatile %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%current_45args54657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54656)
store volatile %struct.ScmObj* %current_45args54657, %struct.ScmObj** %stackaddr$prim56301, align 8
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54657)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim56302, align 8
%stackaddr$makeclosure56303 = alloca %struct.ScmObj*, align 8
%fptrToInt56304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48443 to i64
%ae48443 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56304)
store volatile %struct.ScmObj* %ae48443, %struct.ScmObj** %stackaddr$makeclosure56303, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48443, %struct.ScmObj* %k48414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48443, %struct.ScmObj* %f48028, i64 1)
%ae48444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56305 = alloca %struct.ScmObj*, align 8
%fptrToInt56306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48445 to i64
%ae48445 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56306)
store volatile %struct.ScmObj* %ae48445, %struct.ScmObj** %stackaddr$makeclosure56305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48445, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48445, %struct.ScmObj* %y48027, i64 1)
%argslist54671$ae484430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%argslist54671$ae484431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48445, %struct.ScmObj* %argslist54671$ae484430)
store volatile %struct.ScmObj* %argslist54671$ae484431, %struct.ScmObj** %stackaddr$prim56307, align 8
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%argslist54671$ae484432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48444, %struct.ScmObj* %argslist54671$ae484431)
store volatile %struct.ScmObj* %argslist54671$ae484432, %struct.ScmObj** %stackaddr$prim56308, align 8
%clofunc56309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48443)
musttail call tailcc void %clofunc56309(%struct.ScmObj* %ae48443, %struct.ScmObj* %argslist54671$ae484432)
ret void
}

define tailcc void @proc_clo$ae48443(%struct.ScmObj* %env$ae48443,%struct.ScmObj* %current_45args54659) {
%stackaddr$env-ref56310 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48443, i64 0)
store %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$env-ref56310
%stackaddr$env-ref56311 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48443, i64 1)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56311
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54659)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%current_45args54660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54659)
store volatile %struct.ScmObj* %current_45args54660, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%anf_45bind48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54660)
store volatile %struct.ScmObj* %anf_45bind48147, %struct.ScmObj** %stackaddr$prim56314, align 8
%argslist54662$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%argslist54662$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48147, %struct.ScmObj* %argslist54662$f480280)
store volatile %struct.ScmObj* %argslist54662$f480281, %struct.ScmObj** %stackaddr$prim56315, align 8
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%argslist54662$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist54662$f480281)
store volatile %struct.ScmObj* %argslist54662$f480282, %struct.ScmObj** %stackaddr$prim56316, align 8
%clofunc56317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc56317(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54662$f480282)
ret void
}

define tailcc void @proc_clo$ae48445(%struct.ScmObj* %env$ae48445,%struct.ScmObj* %args4802948416) {
%stackaddr$env-ref56318 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48445, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56318
%stackaddr$env-ref56319 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48445, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref56319
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948416)
store volatile %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948416)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$makeclosure56322 = alloca %struct.ScmObj*, align 8
%fptrToInt56323 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48449 to i64
%ae48449 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56323)
store volatile %struct.ScmObj* %ae48449, %struct.ScmObj** %stackaddr$makeclosure56322, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48449, %struct.ScmObj* %k48417, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48449, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48449, %struct.ScmObj* %f48028, i64 2)
%argslist54670$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%argslist54670$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54670$y480270)
store volatile %struct.ScmObj* %argslist54670$y480271, %struct.ScmObj** %stackaddr$prim56324, align 8
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%argslist54670$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48449, %struct.ScmObj* %argslist54670$y480271)
store volatile %struct.ScmObj* %argslist54670$y480272, %struct.ScmObj** %stackaddr$prim56325, align 8
%clofunc56326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc56326(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist54670$y480272)
ret void
}

define tailcc void @proc_clo$ae48449(%struct.ScmObj* %env$ae48449,%struct.ScmObj* %current_45args54663) {
%stackaddr$env-ref56327 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48449, i64 0)
store %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$env-ref56327
%stackaddr$env-ref56328 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48449, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56328
%stackaddr$env-ref56329 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48449, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref56329
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54663)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%current_45args54664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54663)
store volatile %struct.ScmObj* %current_45args54664, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%anf_45bind48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54664)
store volatile %struct.ScmObj* %anf_45bind48145, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$makeclosure56333 = alloca %struct.ScmObj*, align 8
%fptrToInt56334 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48452 to i64
%ae48452 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56334)
store volatile %struct.ScmObj* %ae48452, %struct.ScmObj** %stackaddr$makeclosure56333, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48452, %struct.ScmObj* %k48417, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48452, %struct.ScmObj* %args48029, i64 1)
%argslist54669$anf_45bind481450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%argslist54669$anf_45bind481451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist54669$anf_45bind481450)
store volatile %struct.ScmObj* %argslist54669$anf_45bind481451, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%argslist54669$anf_45bind481452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48452, %struct.ScmObj* %argslist54669$anf_45bind481451)
store volatile %struct.ScmObj* %argslist54669$anf_45bind481452, %struct.ScmObj** %stackaddr$prim56336, align 8
%clofunc56337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48145)
musttail call tailcc void %clofunc56337(%struct.ScmObj* %anf_45bind48145, %struct.ScmObj* %argslist54669$anf_45bind481452)
ret void
}

define tailcc void @proc_clo$ae48452(%struct.ScmObj* %env$ae48452,%struct.ScmObj* %current_45args54666) {
%stackaddr$env-ref56338 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48452, i64 0)
store %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$env-ref56338
%stackaddr$env-ref56339 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48452, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref56339
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%_95k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54666)
store volatile %struct.ScmObj* %_95k48419, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%current_45args54667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54666)
store volatile %struct.ScmObj* %current_45args54667, %struct.ScmObj** %stackaddr$prim56341, align 8
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%anf_45bind48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54667)
store volatile %struct.ScmObj* %anf_45bind48146, %struct.ScmObj** %stackaddr$prim56342, align 8
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%cpsargs48420 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48417, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48420, %struct.ScmObj** %stackaddr$prim56343, align 8
%clofunc56344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48146)
musttail call tailcc void %clofunc56344(%struct.ScmObj* %anf_45bind48146, %struct.ScmObj* %cpsargs48420)
ret void
}

define tailcc void @proc_clo$ae48424(%struct.ScmObj* %env$ae48424,%struct.ScmObj* %current_45args54674) {
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54674)
store volatile %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$prim56345, align 8
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%current_45args54675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54674)
store volatile %struct.ScmObj* %current_45args54675, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$prim56347 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54675)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim56347, align 8
%argslist54677$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56348 = alloca %struct.ScmObj*, align 8
%argslist54677$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54677$yu480260)
store volatile %struct.ScmObj* %argslist54677$yu480261, %struct.ScmObj** %stackaddr$prim56348, align 8
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%argslist54677$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist54677$yu480261)
store volatile %struct.ScmObj* %argslist54677$yu480262, %struct.ScmObj** %stackaddr$prim56349, align 8
%clofunc56350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc56350(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist54677$yu480262)
ret void
}