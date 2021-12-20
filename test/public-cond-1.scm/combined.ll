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
%mainenv53724 = call %struct.ScmObj* @const_init_null()
%mainargs53725 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv53724, %struct.ScmObj* %mainargs53725)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv53722,%struct.ScmObj* %mainargs53723) {
%stackaddr$makeclosure53726 = alloca %struct.ScmObj*, align 8
%fptrToInt53727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47465 to i64
%ae47465 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53727)
store volatile %struct.ScmObj* %ae47465, %struct.ScmObj** %stackaddr$makeclosure53726, align 8
%ae47466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53728 = alloca %struct.ScmObj*, align 8
%fptrToInt53729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47467 to i64
%ae47467 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53729)
store volatile %struct.ScmObj* %ae47467, %struct.ScmObj** %stackaddr$makeclosure53728, align 8
%argslist53721$ae474650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53730 = alloca %struct.ScmObj*, align 8
%argslist53721$ae474651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47467, %struct.ScmObj* %argslist53721$ae474650)
store volatile %struct.ScmObj* %argslist53721$ae474651, %struct.ScmObj** %stackaddr$prim53730, align 8
%stackaddr$prim53731 = alloca %struct.ScmObj*, align 8
%argslist53721$ae474652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47466, %struct.ScmObj* %argslist53721$ae474651)
store volatile %struct.ScmObj* %argslist53721$ae474652, %struct.ScmObj** %stackaddr$prim53731, align 8
%clofunc53732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47465)
musttail call tailcc void %clofunc53732(%struct.ScmObj* %ae47465, %struct.ScmObj* %argslist53721$ae474652)
ret void
}

define tailcc void @proc_clo$ae47465(%struct.ScmObj* %env$ae47465,%struct.ScmObj* %current_45args53167) {
%stackaddr$prim53733 = alloca %struct.ScmObj*, align 8
%_95k47303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53167)
store volatile %struct.ScmObj* %_95k47303, %struct.ScmObj** %stackaddr$prim53733, align 8
%stackaddr$prim53734 = alloca %struct.ScmObj*, align 8
%current_45args53168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53167)
store volatile %struct.ScmObj* %current_45args53168, %struct.ScmObj** %stackaddr$prim53734, align 8
%stackaddr$prim53735 = alloca %struct.ScmObj*, align 8
%anf_45bind47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53168)
store volatile %struct.ScmObj* %anf_45bind47187, %struct.ScmObj** %stackaddr$prim53735, align 8
%stackaddr$makeclosure53736 = alloca %struct.ScmObj*, align 8
%fptrToInt53737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47480 to i64
%ae47480 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53737)
store volatile %struct.ScmObj* %ae47480, %struct.ScmObj** %stackaddr$makeclosure53736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47480, %struct.ScmObj* %anf_45bind47187, i64 0)
%ae47481 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53738 = alloca %struct.ScmObj*, align 8
%fptrToInt53739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47482 to i64
%ae47482 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53739)
store volatile %struct.ScmObj* %ae47482, %struct.ScmObj** %stackaddr$makeclosure53738, align 8
%argslist53716$ae474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53740 = alloca %struct.ScmObj*, align 8
%argslist53716$ae474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47482, %struct.ScmObj* %argslist53716$ae474800)
store volatile %struct.ScmObj* %argslist53716$ae474801, %struct.ScmObj** %stackaddr$prim53740, align 8
%stackaddr$prim53741 = alloca %struct.ScmObj*, align 8
%argslist53716$ae474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47481, %struct.ScmObj* %argslist53716$ae474801)
store volatile %struct.ScmObj* %argslist53716$ae474802, %struct.ScmObj** %stackaddr$prim53741, align 8
%clofunc53742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47480)
musttail call tailcc void %clofunc53742(%struct.ScmObj* %ae47480, %struct.ScmObj* %argslist53716$ae474802)
ret void
}

define tailcc void @proc_clo$ae47480(%struct.ScmObj* %env$ae47480,%struct.ScmObj* %current_45args53170) {
%stackaddr$env-ref53743 = alloca %struct.ScmObj*, align 8
%anf_45bind47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47480, i64 0)
store %struct.ScmObj* %anf_45bind47187, %struct.ScmObj** %stackaddr$env-ref53743
%stackaddr$prim53744 = alloca %struct.ScmObj*, align 8
%_95k47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %_95k47304, %struct.ScmObj** %stackaddr$prim53744, align 8
%stackaddr$prim53745 = alloca %struct.ScmObj*, align 8
%current_45args53171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53170)
store volatile %struct.ScmObj* %current_45args53171, %struct.ScmObj** %stackaddr$prim53745, align 8
%stackaddr$prim53746 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53171)
store volatile %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$prim53746, align 8
%stackaddr$makeclosure53747 = alloca %struct.ScmObj*, align 8
%fptrToInt53748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47595 to i64
%ae47595 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53748)
store volatile %struct.ScmObj* %ae47595, %struct.ScmObj** %stackaddr$makeclosure53747, align 8
%argslist53695$anf_45bind471870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53749 = alloca %struct.ScmObj*, align 8
%argslist53695$anf_45bind471871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47191, %struct.ScmObj* %argslist53695$anf_45bind471870)
store volatile %struct.ScmObj* %argslist53695$anf_45bind471871, %struct.ScmObj** %stackaddr$prim53749, align 8
%stackaddr$prim53750 = alloca %struct.ScmObj*, align 8
%argslist53695$anf_45bind471872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47595, %struct.ScmObj* %argslist53695$anf_45bind471871)
store volatile %struct.ScmObj* %argslist53695$anf_45bind471872, %struct.ScmObj** %stackaddr$prim53750, align 8
%clofunc53751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47187)
musttail call tailcc void %clofunc53751(%struct.ScmObj* %anf_45bind47187, %struct.ScmObj* %argslist53695$anf_45bind471872)
ret void
}

define tailcc void @proc_clo$ae47595(%struct.ScmObj* %env$ae47595,%struct.ScmObj* %current_45args53173) {
%stackaddr$prim53752 = alloca %struct.ScmObj*, align 8
%_95k47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53173)
store volatile %struct.ScmObj* %_95k47305, %struct.ScmObj** %stackaddr$prim53752, align 8
%stackaddr$prim53753 = alloca %struct.ScmObj*, align 8
%current_45args53174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53173)
store volatile %struct.ScmObj* %current_45args53174, %struct.ScmObj** %stackaddr$prim53753, align 8
%stackaddr$prim53754 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53174)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim53754, align 8
%stackaddr$makeclosure53755 = alloca %struct.ScmObj*, align 8
%fptrToInt53756 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47597 to i64
%ae47597 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53756)
store volatile %struct.ScmObj* %ae47597, %struct.ScmObj** %stackaddr$makeclosure53755, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47597, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47598 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53757 = alloca %struct.ScmObj*, align 8
%fptrToInt53758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47599 to i64
%ae47599 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53758)
store volatile %struct.ScmObj* %ae47599, %struct.ScmObj** %stackaddr$makeclosure53757, align 8
%argslist53694$ae475970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53759 = alloca %struct.ScmObj*, align 8
%argslist53694$ae475971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47599, %struct.ScmObj* %argslist53694$ae475970)
store volatile %struct.ScmObj* %argslist53694$ae475971, %struct.ScmObj** %stackaddr$prim53759, align 8
%stackaddr$prim53760 = alloca %struct.ScmObj*, align 8
%argslist53694$ae475972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47598, %struct.ScmObj* %argslist53694$ae475971)
store volatile %struct.ScmObj* %argslist53694$ae475972, %struct.ScmObj** %stackaddr$prim53760, align 8
%clofunc53761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47597)
musttail call tailcc void %clofunc53761(%struct.ScmObj* %ae47597, %struct.ScmObj* %argslist53694$ae475972)
ret void
}

define tailcc void @proc_clo$ae47597(%struct.ScmObj* %env$ae47597,%struct.ScmObj* %current_45args53176) {
%stackaddr$env-ref53762 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47597, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53762
%stackaddr$prim53763 = alloca %struct.ScmObj*, align 8
%_95k47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %_95k47306, %struct.ScmObj** %stackaddr$prim53763, align 8
%stackaddr$prim53764 = alloca %struct.ScmObj*, align 8
%current_45args53177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53176)
store volatile %struct.ScmObj* %current_45args53177, %struct.ScmObj** %stackaddr$prim53764, align 8
%stackaddr$prim53765 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53177)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim53765, align 8
%stackaddr$makeclosure53766 = alloca %struct.ScmObj*, align 8
%fptrToInt53767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47675 to i64
%ae47675 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53767)
store volatile %struct.ScmObj* %ae47675, %struct.ScmObj** %stackaddr$makeclosure53766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47675, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist53678$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53768 = alloca %struct.ScmObj*, align 8
%argslist53678$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47196, %struct.ScmObj* %argslist53678$Ycmb470680)
store volatile %struct.ScmObj* %argslist53678$Ycmb470681, %struct.ScmObj** %stackaddr$prim53768, align 8
%stackaddr$prim53769 = alloca %struct.ScmObj*, align 8
%argslist53678$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47675, %struct.ScmObj* %argslist53678$Ycmb470681)
store volatile %struct.ScmObj* %argslist53678$Ycmb470682, %struct.ScmObj** %stackaddr$prim53769, align 8
%clofunc53770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53770(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53678$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47675(%struct.ScmObj* %env$ae47675,%struct.ScmObj* %current_45args53179) {
%stackaddr$env-ref53771 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47675, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53771
%stackaddr$prim53772 = alloca %struct.ScmObj*, align 8
%_95k47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53179)
store volatile %struct.ScmObj* %_95k47307, %struct.ScmObj** %stackaddr$prim53772, align 8
%stackaddr$prim53773 = alloca %struct.ScmObj*, align 8
%current_45args53180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53179)
store volatile %struct.ScmObj* %current_45args53180, %struct.ScmObj** %stackaddr$prim53773, align 8
%stackaddr$prim53774 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53180)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim53774, align 8
%stackaddr$makeclosure53775 = alloca %struct.ScmObj*, align 8
%fptrToInt53776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47677 to i64
%ae47677 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53776)
store volatile %struct.ScmObj* %ae47677, %struct.ScmObj** %stackaddr$makeclosure53775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47677, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47677, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47678 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53777 = alloca %struct.ScmObj*, align 8
%fptrToInt53778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47679 to i64
%ae47679 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53778)
store volatile %struct.ScmObj* %ae47679, %struct.ScmObj** %stackaddr$makeclosure53777, align 8
%argslist53677$ae476770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53779 = alloca %struct.ScmObj*, align 8
%argslist53677$ae476771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47679, %struct.ScmObj* %argslist53677$ae476770)
store volatile %struct.ScmObj* %argslist53677$ae476771, %struct.ScmObj** %stackaddr$prim53779, align 8
%stackaddr$prim53780 = alloca %struct.ScmObj*, align 8
%argslist53677$ae476772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47678, %struct.ScmObj* %argslist53677$ae476771)
store volatile %struct.ScmObj* %argslist53677$ae476772, %struct.ScmObj** %stackaddr$prim53780, align 8
%clofunc53781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47677)
musttail call tailcc void %clofunc53781(%struct.ScmObj* %ae47677, %struct.ScmObj* %argslist53677$ae476772)
ret void
}

define tailcc void @proc_clo$ae47677(%struct.ScmObj* %env$ae47677,%struct.ScmObj* %current_45args53182) {
%stackaddr$env-ref53782 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47677, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53782
%stackaddr$env-ref53783 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47677, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53783
%stackaddr$prim53784 = alloca %struct.ScmObj*, align 8
%_95k47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %_95k47308, %struct.ScmObj** %stackaddr$prim53784, align 8
%stackaddr$prim53785 = alloca %struct.ScmObj*, align 8
%current_45args53183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53182)
store volatile %struct.ScmObj* %current_45args53183, %struct.ScmObj** %stackaddr$prim53785, align 8
%stackaddr$prim53786 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53183)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim53786, align 8
%stackaddr$makeclosure53787 = alloca %struct.ScmObj*, align 8
%fptrToInt53788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47772 to i64
%ae47772 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53788)
store volatile %struct.ScmObj* %ae47772, %struct.ScmObj** %stackaddr$makeclosure53787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47772, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47772, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist53658$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53789 = alloca %struct.ScmObj*, align 8
%argslist53658$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist53658$Ycmb470680)
store volatile %struct.ScmObj* %argslist53658$Ycmb470681, %struct.ScmObj** %stackaddr$prim53789, align 8
%stackaddr$prim53790 = alloca %struct.ScmObj*, align 8
%argslist53658$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47772, %struct.ScmObj* %argslist53658$Ycmb470681)
store volatile %struct.ScmObj* %argslist53658$Ycmb470682, %struct.ScmObj** %stackaddr$prim53790, align 8
%clofunc53791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53791(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53658$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47772(%struct.ScmObj* %env$ae47772,%struct.ScmObj* %current_45args53185) {
%stackaddr$env-ref53792 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47772, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53792
%stackaddr$env-ref53793 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47772, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53793
%stackaddr$prim53794 = alloca %struct.ScmObj*, align 8
%_95k47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53185)
store volatile %struct.ScmObj* %_95k47309, %struct.ScmObj** %stackaddr$prim53794, align 8
%stackaddr$prim53795 = alloca %struct.ScmObj*, align 8
%current_45args53186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53185)
store volatile %struct.ScmObj* %current_45args53186, %struct.ScmObj** %stackaddr$prim53795, align 8
%stackaddr$prim53796 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53186)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim53796, align 8
%stackaddr$makeclosure53797 = alloca %struct.ScmObj*, align 8
%fptrToInt53798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47774 to i64
%ae47774 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53798)
store volatile %struct.ScmObj* %ae47774, %struct.ScmObj** %stackaddr$makeclosure53797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47774, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47774, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47774, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47775 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53799 = alloca %struct.ScmObj*, align 8
%fptrToInt53800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47776 to i64
%ae47776 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53800)
store volatile %struct.ScmObj* %ae47776, %struct.ScmObj** %stackaddr$makeclosure53799, align 8
%argslist53657$ae477740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53801 = alloca %struct.ScmObj*, align 8
%argslist53657$ae477741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47776, %struct.ScmObj* %argslist53657$ae477740)
store volatile %struct.ScmObj* %argslist53657$ae477741, %struct.ScmObj** %stackaddr$prim53801, align 8
%stackaddr$prim53802 = alloca %struct.ScmObj*, align 8
%argslist53657$ae477742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47775, %struct.ScmObj* %argslist53657$ae477741)
store volatile %struct.ScmObj* %argslist53657$ae477742, %struct.ScmObj** %stackaddr$prim53802, align 8
%clofunc53803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47774)
musttail call tailcc void %clofunc53803(%struct.ScmObj* %ae47774, %struct.ScmObj* %argslist53657$ae477742)
ret void
}

define tailcc void @proc_clo$ae47774(%struct.ScmObj* %env$ae47774,%struct.ScmObj* %current_45args53188) {
%stackaddr$env-ref53804 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47774, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53804
%stackaddr$env-ref53805 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47774, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53805
%stackaddr$env-ref53806 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47774, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53806
%stackaddr$prim53807 = alloca %struct.ScmObj*, align 8
%_95k47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %_95k47310, %struct.ScmObj** %stackaddr$prim53807, align 8
%stackaddr$prim53808 = alloca %struct.ScmObj*, align 8
%current_45args53189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53188)
store volatile %struct.ScmObj* %current_45args53189, %struct.ScmObj** %stackaddr$prim53808, align 8
%stackaddr$prim53809 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53189)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim53809, align 8
%stackaddr$makeclosure53810 = alloca %struct.ScmObj*, align 8
%fptrToInt53811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47922 to i64
%ae47922 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53811)
store volatile %struct.ScmObj* %ae47922, %struct.ScmObj** %stackaddr$makeclosure53810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47922, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist53641$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53812 = alloca %struct.ScmObj*, align 8
%argslist53641$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %argslist53641$Ycmb470680)
store volatile %struct.ScmObj* %argslist53641$Ycmb470681, %struct.ScmObj** %stackaddr$prim53812, align 8
%stackaddr$prim53813 = alloca %struct.ScmObj*, align 8
%argslist53641$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47922, %struct.ScmObj* %argslist53641$Ycmb470681)
store volatile %struct.ScmObj* %argslist53641$Ycmb470682, %struct.ScmObj** %stackaddr$prim53813, align 8
%clofunc53814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53814(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53641$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47922(%struct.ScmObj* %env$ae47922,%struct.ScmObj* %current_45args53191) {
%stackaddr$env-ref53815 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53815
%stackaddr$env-ref53816 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53816
%stackaddr$env-ref53817 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47922, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53817
%stackaddr$prim53818 = alloca %struct.ScmObj*, align 8
%_95k47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %_95k47311, %struct.ScmObj** %stackaddr$prim53818, align 8
%stackaddr$prim53819 = alloca %struct.ScmObj*, align 8
%current_45args53192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53191)
store volatile %struct.ScmObj* %current_45args53192, %struct.ScmObj** %stackaddr$prim53819, align 8
%stackaddr$prim53820 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53192)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim53820, align 8
%stackaddr$makeclosure53821 = alloca %struct.ScmObj*, align 8
%fptrToInt53822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47924 to i64
%ae47924 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53822)
store volatile %struct.ScmObj* %ae47924, %struct.ScmObj** %stackaddr$makeclosure53821, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47924, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47924, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47924, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47924, %struct.ScmObj* %_37take47081, i64 3)
%ae47925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53823 = alloca %struct.ScmObj*, align 8
%fptrToInt53824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47926 to i64
%ae47926 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53824)
store volatile %struct.ScmObj* %ae47926, %struct.ScmObj** %stackaddr$makeclosure53823, align 8
%argslist53640$ae479240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53825 = alloca %struct.ScmObj*, align 8
%argslist53640$ae479241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47926, %struct.ScmObj* %argslist53640$ae479240)
store volatile %struct.ScmObj* %argslist53640$ae479241, %struct.ScmObj** %stackaddr$prim53825, align 8
%stackaddr$prim53826 = alloca %struct.ScmObj*, align 8
%argslist53640$ae479242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47925, %struct.ScmObj* %argslist53640$ae479241)
store volatile %struct.ScmObj* %argslist53640$ae479242, %struct.ScmObj** %stackaddr$prim53826, align 8
%clofunc53827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47924)
musttail call tailcc void %clofunc53827(%struct.ScmObj* %ae47924, %struct.ScmObj* %argslist53640$ae479242)
ret void
}

define tailcc void @proc_clo$ae47924(%struct.ScmObj* %env$ae47924,%struct.ScmObj* %current_45args53194) {
%stackaddr$env-ref53828 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47924, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53828
%stackaddr$env-ref53829 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47924, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53829
%stackaddr$env-ref53830 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47924, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53830
%stackaddr$env-ref53831 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47924, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53831
%stackaddr$prim53832 = alloca %struct.ScmObj*, align 8
%_95k47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %_95k47312, %struct.ScmObj** %stackaddr$prim53832, align 8
%stackaddr$prim53833 = alloca %struct.ScmObj*, align 8
%current_45args53195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53194)
store volatile %struct.ScmObj* %current_45args53195, %struct.ScmObj** %stackaddr$prim53833, align 8
%stackaddr$prim53834 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53195)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim53834, align 8
%stackaddr$makeclosure53835 = alloca %struct.ScmObj*, align 8
%fptrToInt53836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48005 to i64
%ae48005 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt53836)
store volatile %struct.ScmObj* %ae48005, %struct.ScmObj** %stackaddr$makeclosure53835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48005, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48005, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48005, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48005, %struct.ScmObj* %_37take47081, i64 3)
%argslist53626$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53837 = alloca %struct.ScmObj*, align 8
%argslist53626$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist53626$Ycmb470680)
store volatile %struct.ScmObj* %argslist53626$Ycmb470681, %struct.ScmObj** %stackaddr$prim53837, align 8
%stackaddr$prim53838 = alloca %struct.ScmObj*, align 8
%argslist53626$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48005, %struct.ScmObj* %argslist53626$Ycmb470681)
store volatile %struct.ScmObj* %argslist53626$Ycmb470682, %struct.ScmObj** %stackaddr$prim53838, align 8
%clofunc53839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53839(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53626$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48005(%struct.ScmObj* %env$ae48005,%struct.ScmObj* %current_45args53197) {
%stackaddr$env-ref53840 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48005, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53840
%stackaddr$env-ref53841 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48005, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53841
%stackaddr$env-ref53842 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48005, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53842
%stackaddr$env-ref53843 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48005, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53843
%stackaddr$prim53844 = alloca %struct.ScmObj*, align 8
%_95k47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %_95k47313, %struct.ScmObj** %stackaddr$prim53844, align 8
%stackaddr$prim53845 = alloca %struct.ScmObj*, align 8
%current_45args53198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53197)
store volatile %struct.ScmObj* %current_45args53198, %struct.ScmObj** %stackaddr$prim53845, align 8
%stackaddr$prim53846 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53198)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim53846, align 8
%stackaddr$makeclosure53847 = alloca %struct.ScmObj*, align 8
%fptrToInt53848 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48007 to i64
%ae48007 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53848)
store volatile %struct.ScmObj* %ae48007, %struct.ScmObj** %stackaddr$makeclosure53847, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48007, %struct.ScmObj* %_37take47081, i64 4)
%ae48008 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53849 = alloca %struct.ScmObj*, align 8
%fptrToInt53850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48009 to i64
%ae48009 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53850)
store volatile %struct.ScmObj* %ae48009, %struct.ScmObj** %stackaddr$makeclosure53849, align 8
%argslist53625$ae480070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53851 = alloca %struct.ScmObj*, align 8
%argslist53625$ae480071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48009, %struct.ScmObj* %argslist53625$ae480070)
store volatile %struct.ScmObj* %argslist53625$ae480071, %struct.ScmObj** %stackaddr$prim53851, align 8
%stackaddr$prim53852 = alloca %struct.ScmObj*, align 8
%argslist53625$ae480072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48008, %struct.ScmObj* %argslist53625$ae480071)
store volatile %struct.ScmObj* %argslist53625$ae480072, %struct.ScmObj** %stackaddr$prim53852, align 8
%clofunc53853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48007)
musttail call tailcc void %clofunc53853(%struct.ScmObj* %ae48007, %struct.ScmObj* %argslist53625$ae480072)
ret void
}

define tailcc void @proc_clo$ae48007(%struct.ScmObj* %env$ae48007,%struct.ScmObj* %current_45args53200) {
%stackaddr$env-ref53854 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53854
%stackaddr$env-ref53855 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53855
%stackaddr$env-ref53856 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53856
%stackaddr$env-ref53857 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53857
%stackaddr$env-ref53858 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48007, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53858
%stackaddr$prim53859 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim53859, align 8
%stackaddr$prim53860 = alloca %struct.ScmObj*, align 8
%current_45args53201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53200)
store volatile %struct.ScmObj* %current_45args53201, %struct.ScmObj** %stackaddr$prim53860, align 8
%stackaddr$prim53861 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53201)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim53861, align 8
%stackaddr$makeclosure53862 = alloca %struct.ScmObj*, align 8
%fptrToInt53863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48084 to i64
%ae48084 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53863)
store volatile %struct.ScmObj* %ae48084, %struct.ScmObj** %stackaddr$makeclosure53862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48084, %struct.ScmObj* %_37take47081, i64 4)
%argslist53609$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53864 = alloca %struct.ScmObj*, align 8
%argslist53609$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist53609$Ycmb470680)
store volatile %struct.ScmObj* %argslist53609$Ycmb470681, %struct.ScmObj** %stackaddr$prim53864, align 8
%stackaddr$prim53865 = alloca %struct.ScmObj*, align 8
%argslist53609$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48084, %struct.ScmObj* %argslist53609$Ycmb470681)
store volatile %struct.ScmObj* %argslist53609$Ycmb470682, %struct.ScmObj** %stackaddr$prim53865, align 8
%clofunc53866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53866(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53609$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48084(%struct.ScmObj* %env$ae48084,%struct.ScmObj* %current_45args53203) {
%stackaddr$env-ref53867 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53867
%stackaddr$env-ref53868 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53868
%stackaddr$env-ref53869 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53869
%stackaddr$env-ref53870 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53870
%stackaddr$env-ref53871 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48084, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53871
%stackaddr$prim53872 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim53872, align 8
%stackaddr$prim53873 = alloca %struct.ScmObj*, align 8
%current_45args53204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53203)
store volatile %struct.ScmObj* %current_45args53204, %struct.ScmObj** %stackaddr$prim53873, align 8
%stackaddr$prim53874 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53204)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim53874, align 8
%stackaddr$makeclosure53875 = alloca %struct.ScmObj*, align 8
%fptrToInt53876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48086 to i64
%ae48086 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53876)
store volatile %struct.ScmObj* %ae48086, %struct.ScmObj** %stackaddr$makeclosure53875, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37take47081, i64 5)
%ae48087 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53877 = alloca %struct.ScmObj*, align 8
%fptrToInt53878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48088 to i64
%ae48088 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53878)
store volatile %struct.ScmObj* %ae48088, %struct.ScmObj** %stackaddr$makeclosure53877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48088, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53608$ae480860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53879 = alloca %struct.ScmObj*, align 8
%argslist53608$ae480861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48088, %struct.ScmObj* %argslist53608$ae480860)
store volatile %struct.ScmObj* %argslist53608$ae480861, %struct.ScmObj** %stackaddr$prim53879, align 8
%stackaddr$prim53880 = alloca %struct.ScmObj*, align 8
%argslist53608$ae480862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48087, %struct.ScmObj* %argslist53608$ae480861)
store volatile %struct.ScmObj* %argslist53608$ae480862, %struct.ScmObj** %stackaddr$prim53880, align 8
%clofunc53881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48086)
musttail call tailcc void %clofunc53881(%struct.ScmObj* %ae48086, %struct.ScmObj* %argslist53608$ae480862)
ret void
}

define tailcc void @proc_clo$ae48086(%struct.ScmObj* %env$ae48086,%struct.ScmObj* %current_45args53206) {
%stackaddr$env-ref53882 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53882
%stackaddr$env-ref53883 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53883
%stackaddr$env-ref53884 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref53884
%stackaddr$env-ref53885 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53885
%stackaddr$env-ref53886 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53886
%stackaddr$env-ref53887 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref53887
%stackaddr$prim53888 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim53888, align 8
%stackaddr$prim53889 = alloca %struct.ScmObj*, align 8
%current_45args53207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53206)
store volatile %struct.ScmObj* %current_45args53207, %struct.ScmObj** %stackaddr$prim53889, align 8
%stackaddr$prim53890 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53207)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim53890, align 8
%stackaddr$makeclosure53891 = alloca %struct.ScmObj*, align 8
%fptrToInt53892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48140 to i64
%ae48140 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53892)
store volatile %struct.ScmObj* %ae48140, %struct.ScmObj** %stackaddr$makeclosure53891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37last47111, i64 4)
%ae48141 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53893 = alloca %struct.ScmObj*, align 8
%fptrToInt53894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48142 to i64
%ae48142 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53894)
store volatile %struct.ScmObj* %ae48142, %struct.ScmObj** %stackaddr$makeclosure53893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48142, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48142, %struct.ScmObj* %_37take47081, i64 1)
%argslist53594$ae481400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53895 = alloca %struct.ScmObj*, align 8
%argslist53594$ae481401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48142, %struct.ScmObj* %argslist53594$ae481400)
store volatile %struct.ScmObj* %argslist53594$ae481401, %struct.ScmObj** %stackaddr$prim53895, align 8
%stackaddr$prim53896 = alloca %struct.ScmObj*, align 8
%argslist53594$ae481402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48141, %struct.ScmObj* %argslist53594$ae481401)
store volatile %struct.ScmObj* %argslist53594$ae481402, %struct.ScmObj** %stackaddr$prim53896, align 8
%clofunc53897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48140)
musttail call tailcc void %clofunc53897(%struct.ScmObj* %ae48140, %struct.ScmObj* %argslist53594$ae481402)
ret void
}

define tailcc void @proc_clo$ae48140(%struct.ScmObj* %env$ae48140,%struct.ScmObj* %current_45args53209) {
%stackaddr$env-ref53898 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53898
%stackaddr$env-ref53899 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53899
%stackaddr$env-ref53900 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref53900
%stackaddr$env-ref53901 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53901
%stackaddr$env-ref53902 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53902
%stackaddr$prim53903 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim53903, align 8
%stackaddr$prim53904 = alloca %struct.ScmObj*, align 8
%current_45args53210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53209)
store volatile %struct.ScmObj* %current_45args53210, %struct.ScmObj** %stackaddr$prim53904, align 8
%stackaddr$prim53905 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53210)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim53905, align 8
%stackaddr$makeclosure53906 = alloca %struct.ScmObj*, align 8
%fptrToInt53907 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48170 to i64
%ae48170 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53907)
store volatile %struct.ScmObj* %ae48170, %struct.ScmObj** %stackaddr$makeclosure53906, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48170, %struct.ScmObj* %_37last47111, i64 4)
%ae48171 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53908 = alloca %struct.ScmObj*, align 8
%fptrToInt53909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48172 to i64
%ae48172 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53909)
store volatile %struct.ScmObj* %ae48172, %struct.ScmObj** %stackaddr$makeclosure53908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48172, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48172, %struct.ScmObj* %_37map147085, i64 1)
%argslist53584$ae481700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53910 = alloca %struct.ScmObj*, align 8
%argslist53584$ae481701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48172, %struct.ScmObj* %argslist53584$ae481700)
store volatile %struct.ScmObj* %argslist53584$ae481701, %struct.ScmObj** %stackaddr$prim53910, align 8
%stackaddr$prim53911 = alloca %struct.ScmObj*, align 8
%argslist53584$ae481702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48171, %struct.ScmObj* %argslist53584$ae481701)
store volatile %struct.ScmObj* %argslist53584$ae481702, %struct.ScmObj** %stackaddr$prim53911, align 8
%clofunc53912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48170)
musttail call tailcc void %clofunc53912(%struct.ScmObj* %ae48170, %struct.ScmObj* %argslist53584$ae481702)
ret void
}

define tailcc void @proc_clo$ae48170(%struct.ScmObj* %env$ae48170,%struct.ScmObj* %current_45args53212) {
%stackaddr$env-ref53913 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53913
%stackaddr$env-ref53914 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53914
%stackaddr$env-ref53915 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53915
%stackaddr$env-ref53916 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53916
%stackaddr$env-ref53917 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48170, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53917
%stackaddr$prim53918 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim53918, align 8
%stackaddr$prim53919 = alloca %struct.ScmObj*, align 8
%current_45args53213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53212)
store volatile %struct.ScmObj* %current_45args53213, %struct.ScmObj** %stackaddr$prim53919, align 8
%stackaddr$prim53920 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53213)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim53920, align 8
%stackaddr$makeclosure53921 = alloca %struct.ScmObj*, align 8
%fptrToInt53922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48554 to i64
%ae48554 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53922)
store volatile %struct.ScmObj* %ae48554, %struct.ScmObj** %stackaddr$makeclosure53921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48554, %struct.ScmObj* %_37last47111, i64 4)
%argslist53524$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53923 = alloca %struct.ScmObj*, align 8
%argslist53524$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist53524$Ycmb470680)
store volatile %struct.ScmObj* %argslist53524$Ycmb470681, %struct.ScmObj** %stackaddr$prim53923, align 8
%stackaddr$prim53924 = alloca %struct.ScmObj*, align 8
%argslist53524$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48554, %struct.ScmObj* %argslist53524$Ycmb470681)
store volatile %struct.ScmObj* %argslist53524$Ycmb470682, %struct.ScmObj** %stackaddr$prim53924, align 8
%clofunc53925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53925(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53524$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48554(%struct.ScmObj* %env$ae48554,%struct.ScmObj* %current_45args53215) {
%stackaddr$env-ref53926 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53926
%stackaddr$env-ref53927 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53927
%stackaddr$env-ref53928 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53928
%stackaddr$env-ref53929 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53929
%stackaddr$env-ref53930 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48554, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53930
%stackaddr$prim53931 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim53931, align 8
%stackaddr$prim53932 = alloca %struct.ScmObj*, align 8
%current_45args53216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53215)
store volatile %struct.ScmObj* %current_45args53216, %struct.ScmObj** %stackaddr$prim53932, align 8
%stackaddr$prim53933 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53216)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim53933, align 8
%stackaddr$makeclosure53934 = alloca %struct.ScmObj*, align 8
%fptrToInt53935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48556 to i64
%ae48556 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt53935)
store volatile %struct.ScmObj* %ae48556, %struct.ScmObj** %stackaddr$makeclosure53934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48556, %struct.ScmObj* %_37last47111, i64 5)
%ae48557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53936 = alloca %struct.ScmObj*, align 8
%fptrToInt53937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48558 to i64
%ae48558 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53937)
store volatile %struct.ScmObj* %ae48558, %struct.ScmObj** %stackaddr$makeclosure53936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48558, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist53523$ae485560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53938 = alloca %struct.ScmObj*, align 8
%argslist53523$ae485561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48558, %struct.ScmObj* %argslist53523$ae485560)
store volatile %struct.ScmObj* %argslist53523$ae485561, %struct.ScmObj** %stackaddr$prim53938, align 8
%stackaddr$prim53939 = alloca %struct.ScmObj*, align 8
%argslist53523$ae485562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist53523$ae485561)
store volatile %struct.ScmObj* %argslist53523$ae485562, %struct.ScmObj** %stackaddr$prim53939, align 8
%clofunc53940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48556)
musttail call tailcc void %clofunc53940(%struct.ScmObj* %ae48556, %struct.ScmObj* %argslist53523$ae485562)
ret void
}

define tailcc void @proc_clo$ae48556(%struct.ScmObj* %env$ae48556,%struct.ScmObj* %current_45args53218) {
%stackaddr$env-ref53941 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53941
%stackaddr$env-ref53942 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53942
%stackaddr$env-ref53943 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref53943
%stackaddr$env-ref53944 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref53944
%stackaddr$env-ref53945 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53945
%stackaddr$env-ref53946 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48556, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref53946
%stackaddr$prim53947 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim53947, align 8
%stackaddr$prim53948 = alloca %struct.ScmObj*, align 8
%current_45args53219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53218)
store volatile %struct.ScmObj* %current_45args53219, %struct.ScmObj** %stackaddr$prim53948, align 8
%stackaddr$prim53949 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53219)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim53949, align 8
%stackaddr$makeclosure53950 = alloca %struct.ScmObj*, align 8
%fptrToInt53951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48633 to i64
%ae48633 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt53951)
store volatile %struct.ScmObj* %ae48633, %struct.ScmObj** %stackaddr$makeclosure53950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53952 = alloca %struct.ScmObj*, align 8
%fptrToInt53953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53953)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure53952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %_37last47111, i64 2)
%argslist53504$ae486330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53954 = alloca %struct.ScmObj*, align 8
%argslist53504$ae486331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist53504$ae486330)
store volatile %struct.ScmObj* %argslist53504$ae486331, %struct.ScmObj** %stackaddr$prim53954, align 8
%stackaddr$prim53955 = alloca %struct.ScmObj*, align 8
%argslist53504$ae486332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist53504$ae486331)
store volatile %struct.ScmObj* %argslist53504$ae486332, %struct.ScmObj** %stackaddr$prim53955, align 8
%clofunc53956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48633)
musttail call tailcc void %clofunc53956(%struct.ScmObj* %ae48633, %struct.ScmObj* %argslist53504$ae486332)
ret void
}

define tailcc void @proc_clo$ae48633(%struct.ScmObj* %env$ae48633,%struct.ScmObj* %current_45args53221) {
%stackaddr$env-ref53957 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref53957
%stackaddr$env-ref53958 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53958
%stackaddr$env-ref53959 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref53959
%stackaddr$env-ref53960 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref53960
%stackaddr$env-ref53961 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53961
%stackaddr$prim53962 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim53962, align 8
%stackaddr$prim53963 = alloca %struct.ScmObj*, align 8
%current_45args53222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53221)
store volatile %struct.ScmObj* %current_45args53222, %struct.ScmObj** %stackaddr$prim53963, align 8
%stackaddr$prim53964 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53222)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim53964, align 8
%stackaddr$makeclosure53965 = alloca %struct.ScmObj*, align 8
%fptrToInt53966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48779 to i64
%ae48779 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt53966)
store volatile %struct.ScmObj* %ae48779, %struct.ScmObj** %stackaddr$makeclosure53965, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48779, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48779, %struct.ScmObj* %Ycmb47068, i64 1)
%ae48780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53967 = alloca %struct.ScmObj*, align 8
%fptrToInt53968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48781 to i64
%ae48781 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt53968)
store volatile %struct.ScmObj* %ae48781, %struct.ScmObj** %stackaddr$makeclosure53967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48781, %struct.ScmObj* %_37map147120, i64 2)
%argslist53487$ae487790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53969 = alloca %struct.ScmObj*, align 8
%argslist53487$ae487791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48781, %struct.ScmObj* %argslist53487$ae487790)
store volatile %struct.ScmObj* %argslist53487$ae487791, %struct.ScmObj** %stackaddr$prim53969, align 8
%stackaddr$prim53970 = alloca %struct.ScmObj*, align 8
%argslist53487$ae487792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48780, %struct.ScmObj* %argslist53487$ae487791)
store volatile %struct.ScmObj* %argslist53487$ae487792, %struct.ScmObj** %stackaddr$prim53970, align 8
%clofunc53971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48779)
musttail call tailcc void %clofunc53971(%struct.ScmObj* %ae48779, %struct.ScmObj* %argslist53487$ae487792)
ret void
}

define tailcc void @proc_clo$ae48779(%struct.ScmObj* %env$ae48779,%struct.ScmObj* %current_45args53224) {
%stackaddr$env-ref53972 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48779, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53972
%stackaddr$env-ref53973 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48779, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref53973
%stackaddr$prim53974 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim53974, align 8
%stackaddr$prim53975 = alloca %struct.ScmObj*, align 8
%current_45args53225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53224)
store volatile %struct.ScmObj* %current_45args53225, %struct.ScmObj** %stackaddr$prim53975, align 8
%stackaddr$prim53976 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53225)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim53976, align 8
%stackaddr$makeclosure53977 = alloca %struct.ScmObj*, align 8
%fptrToInt53978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49171 to i64
%ae49171 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53978)
store volatile %struct.ScmObj* %ae49171, %struct.ScmObj** %stackaddr$makeclosure53977, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53427$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53979 = alloca %struct.ScmObj*, align 8
%argslist53427$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist53427$Ycmb470680)
store volatile %struct.ScmObj* %argslist53427$Ycmb470681, %struct.ScmObj** %stackaddr$prim53979, align 8
%stackaddr$prim53980 = alloca %struct.ScmObj*, align 8
%argslist53427$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49171, %struct.ScmObj* %argslist53427$Ycmb470681)
store volatile %struct.ScmObj* %argslist53427$Ycmb470682, %struct.ScmObj** %stackaddr$prim53980, align 8
%clofunc53981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc53981(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist53427$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49171(%struct.ScmObj* %env$ae49171,%struct.ScmObj* %current_45args53227) {
%stackaddr$env-ref53982 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53982
%stackaddr$prim53983 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim53983, align 8
%stackaddr$prim53984 = alloca %struct.ScmObj*, align 8
%current_45args53228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53227)
store volatile %struct.ScmObj* %current_45args53228, %struct.ScmObj** %stackaddr$prim53984, align 8
%stackaddr$prim53985 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53228)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim53985, align 8
%stackaddr$makeclosure53986 = alloca %struct.ScmObj*, align 8
%fptrToInt53987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49173 to i64
%ae49173 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53987)
store volatile %struct.ScmObj* %ae49173, %struct.ScmObj** %stackaddr$makeclosure53986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49173, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53988 = alloca %struct.ScmObj*, align 8
%fptrToInt53989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49175 to i64
%ae49175 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt53989)
store volatile %struct.ScmObj* %ae49175, %struct.ScmObj** %stackaddr$makeclosure53988, align 8
%argslist53426$ae491730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim53990 = alloca %struct.ScmObj*, align 8
%argslist53426$ae491731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49175, %struct.ScmObj* %argslist53426$ae491730)
store volatile %struct.ScmObj* %argslist53426$ae491731, %struct.ScmObj** %stackaddr$prim53990, align 8
%stackaddr$prim53991 = alloca %struct.ScmObj*, align 8
%argslist53426$ae491732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49174, %struct.ScmObj* %argslist53426$ae491731)
store volatile %struct.ScmObj* %argslist53426$ae491732, %struct.ScmObj** %stackaddr$prim53991, align 8
%clofunc53992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49173)
musttail call tailcc void %clofunc53992(%struct.ScmObj* %ae49173, %struct.ScmObj* %argslist53426$ae491732)
ret void
}

define tailcc void @proc_clo$ae49173(%struct.ScmObj* %env$ae49173,%struct.ScmObj* %current_45args53230) {
%stackaddr$env-ref53993 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49173, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref53993
%stackaddr$prim53994 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim53994, align 8
%stackaddr$prim53995 = alloca %struct.ScmObj*, align 8
%current_45args53231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53230)
store volatile %struct.ScmObj* %current_45args53231, %struct.ScmObj** %stackaddr$prim53995, align 8
%stackaddr$prim53996 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53231)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim53996, align 8
%stackaddr$makeclosure53997 = alloca %struct.ScmObj*, align 8
%fptrToInt53998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49197 to i64
%ae49197 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt53998)
store volatile %struct.ScmObj* %ae49197, %struct.ScmObj** %stackaddr$makeclosure53997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49197, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure53999 = alloca %struct.ScmObj*, align 8
%fptrToInt54000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49199 to i64
%ae49199 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54000)
store volatile %struct.ScmObj* %ae49199, %struct.ScmObj** %stackaddr$makeclosure53999, align 8
%argslist53420$ae491970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54001 = alloca %struct.ScmObj*, align 8
%argslist53420$ae491971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49199, %struct.ScmObj* %argslist53420$ae491970)
store volatile %struct.ScmObj* %argslist53420$ae491971, %struct.ScmObj** %stackaddr$prim54001, align 8
%stackaddr$prim54002 = alloca %struct.ScmObj*, align 8
%argslist53420$ae491972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49198, %struct.ScmObj* %argslist53420$ae491971)
store volatile %struct.ScmObj* %argslist53420$ae491972, %struct.ScmObj** %stackaddr$prim54002, align 8
%clofunc54003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49197)
musttail call tailcc void %clofunc54003(%struct.ScmObj* %ae49197, %struct.ScmObj* %argslist53420$ae491972)
ret void
}

define tailcc void @proc_clo$ae49197(%struct.ScmObj* %env$ae49197,%struct.ScmObj* %current_45args53233) {
%stackaddr$env-ref54004 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49197, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54004
%stackaddr$prim54005 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim54005, align 8
%stackaddr$prim54006 = alloca %struct.ScmObj*, align 8
%current_45args53234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53233)
store volatile %struct.ScmObj* %current_45args53234, %struct.ScmObj** %stackaddr$prim54006, align 8
%stackaddr$prim54007 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53234)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54007, align 8
%ae49221 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49222 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54008 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49221, %struct.ScmObj* %ae49222)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54008, align 8
%stackaddr$makeclosure54009 = alloca %struct.ScmObj*, align 8
%fptrToInt54010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49223 to i64
%ae49223 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54010)
store volatile %struct.ScmObj* %ae49223, %struct.ScmObj** %stackaddr$makeclosure54009, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49223, %struct.ScmObj* %_37append47161, i64 1)
%ae49224 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54011 = alloca %struct.ScmObj*, align 8
%fptrToInt54012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49225 to i64
%ae49225 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54012)
store volatile %struct.ScmObj* %ae49225, %struct.ScmObj** %stackaddr$makeclosure54011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49225, %struct.ScmObj* %_37append47161, i64 0)
%argslist53414$ae492230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54013 = alloca %struct.ScmObj*, align 8
%argslist53414$ae492231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49225, %struct.ScmObj* %argslist53414$ae492230)
store volatile %struct.ScmObj* %argslist53414$ae492231, %struct.ScmObj** %stackaddr$prim54013, align 8
%stackaddr$prim54014 = alloca %struct.ScmObj*, align 8
%argslist53414$ae492232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49224, %struct.ScmObj* %argslist53414$ae492231)
store volatile %struct.ScmObj* %argslist53414$ae492232, %struct.ScmObj** %stackaddr$prim54014, align 8
%clofunc54015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49223)
musttail call tailcc void %clofunc54015(%struct.ScmObj* %ae49223, %struct.ScmObj* %argslist53414$ae492232)
ret void
}

define tailcc void @proc_clo$ae49223(%struct.ScmObj* %env$ae49223,%struct.ScmObj* %current_45args53236) {
%stackaddr$env-ref54016 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54016
%stackaddr$env-ref54017 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49223, i64 1)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54017
%stackaddr$prim54018 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim54018, align 8
%stackaddr$prim54019 = alloca %struct.ScmObj*, align 8
%current_45args53237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53236)
store volatile %struct.ScmObj* %current_45args53237, %struct.ScmObj** %stackaddr$prim54019, align 8
%stackaddr$prim54020 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53237)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim54020, align 8
%ae49291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54021 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49291, %struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54021, align 8
%ae49294 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54022 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49294)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54022, align 8
%stackaddr$makeclosure54023 = alloca %struct.ScmObj*, align 8
%fptrToInt54024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49295 to i64
%ae49295 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54024)
store volatile %struct.ScmObj* %ae49295, %struct.ScmObj** %stackaddr$makeclosure54023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49295, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54025 = alloca %struct.ScmObj*, align 8
%fptrToInt54026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49297 to i64
%ae49297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54026)
store volatile %struct.ScmObj* %ae49297, %struct.ScmObj** %stackaddr$makeclosure54025, align 8
%argslist53403$ae492950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54027 = alloca %struct.ScmObj*, align 8
%argslist53403$ae492951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49297, %struct.ScmObj* %argslist53403$ae492950)
store volatile %struct.ScmObj* %argslist53403$ae492951, %struct.ScmObj** %stackaddr$prim54027, align 8
%stackaddr$prim54028 = alloca %struct.ScmObj*, align 8
%argslist53403$ae492952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist53403$ae492951)
store volatile %struct.ScmObj* %argslist53403$ae492952, %struct.ScmObj** %stackaddr$prim54028, align 8
%clofunc54029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49295)
musttail call tailcc void %clofunc54029(%struct.ScmObj* %ae49295, %struct.ScmObj* %argslist53403$ae492952)
ret void
}

define tailcc void @proc_clo$ae49295(%struct.ScmObj* %env$ae49295,%struct.ScmObj* %current_45args53239) {
%stackaddr$env-ref54030 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49295, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54030
%stackaddr$prim54031 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim54031, align 8
%stackaddr$prim54032 = alloca %struct.ScmObj*, align 8
%current_45args53240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53239)
store volatile %struct.ScmObj* %current_45args53240, %struct.ScmObj** %stackaddr$prim54032, align 8
%stackaddr$prim54033 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53240)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54033, align 8
%stackaddr$makeclosure54034 = alloca %struct.ScmObj*, align 8
%fptrToInt54035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49711 to i64
%ae49711 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54035)
store volatile %struct.ScmObj* %ae49711, %struct.ScmObj** %stackaddr$makeclosure54034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49711, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49712 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54036 = alloca %struct.ScmObj*, align 8
%fptrToInt54037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49713 to i64
%ae49713 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54037)
store volatile %struct.ScmObj* %ae49713, %struct.ScmObj** %stackaddr$makeclosure54036, align 8
%argslist53378$ae497110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54038 = alloca %struct.ScmObj*, align 8
%argslist53378$ae497111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49713, %struct.ScmObj* %argslist53378$ae497110)
store volatile %struct.ScmObj* %argslist53378$ae497111, %struct.ScmObj** %stackaddr$prim54038, align 8
%stackaddr$prim54039 = alloca %struct.ScmObj*, align 8
%argslist53378$ae497112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49712, %struct.ScmObj* %argslist53378$ae497111)
store volatile %struct.ScmObj* %argslist53378$ae497112, %struct.ScmObj** %stackaddr$prim54039, align 8
%clofunc54040 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49711)
musttail call tailcc void %clofunc54040(%struct.ScmObj* %ae49711, %struct.ScmObj* %argslist53378$ae497112)
ret void
}

define tailcc void @proc_clo$ae49711(%struct.ScmObj* %env$ae49711,%struct.ScmObj* %current_45args53242) {
%stackaddr$env-ref54041 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49711, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54041
%stackaddr$prim54042 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim54042, align 8
%stackaddr$prim54043 = alloca %struct.ScmObj*, align 8
%current_45args53243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53242)
store volatile %struct.ScmObj* %current_45args53243, %struct.ScmObj** %stackaddr$prim54043, align 8
%stackaddr$prim54044 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53243)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54044, align 8
%stackaddr$makeclosure54045 = alloca %struct.ScmObj*, align 8
%fptrToInt54046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50247 to i64
%ae50247 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54046)
store volatile %struct.ScmObj* %ae50247, %struct.ScmObj** %stackaddr$makeclosure54045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50247, %struct.ScmObj* %_37foldl147073, i64 0)
%ae50248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54047 = alloca %struct.ScmObj*, align 8
%fptrToInt54048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50249 to i64
%ae50249 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54048)
store volatile %struct.ScmObj* %ae50249, %struct.ScmObj** %stackaddr$makeclosure54047, align 8
%argslist53354$ae502470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54049 = alloca %struct.ScmObj*, align 8
%argslist53354$ae502471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50249, %struct.ScmObj* %argslist53354$ae502470)
store volatile %struct.ScmObj* %argslist53354$ae502471, %struct.ScmObj** %stackaddr$prim54049, align 8
%stackaddr$prim54050 = alloca %struct.ScmObj*, align 8
%argslist53354$ae502472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50248, %struct.ScmObj* %argslist53354$ae502471)
store volatile %struct.ScmObj* %argslist53354$ae502472, %struct.ScmObj** %stackaddr$prim54050, align 8
%clofunc54051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50247)
musttail call tailcc void %clofunc54051(%struct.ScmObj* %ae50247, %struct.ScmObj* %argslist53354$ae502472)
ret void
}

define tailcc void @proc_clo$ae50247(%struct.ScmObj* %env$ae50247,%struct.ScmObj* %current_45args53245) {
%stackaddr$env-ref54052 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50247, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54052
%stackaddr$prim54053 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim54053, align 8
%stackaddr$prim54054 = alloca %struct.ScmObj*, align 8
%current_45args53246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53245)
store volatile %struct.ScmObj* %current_45args53246, %struct.ScmObj** %stackaddr$prim54054, align 8
%stackaddr$prim54055 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53246)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54055, align 8
%stackaddr$makeclosure54056 = alloca %struct.ScmObj*, align 8
%fptrToInt54057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50651 to i64
%ae50651 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54057)
store volatile %struct.ScmObj* %ae50651, %struct.ScmObj** %stackaddr$makeclosure54056, align 8
%ae50652 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54058 = alloca %struct.ScmObj*, align 8
%fptrToInt54059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50653 to i64
%ae50653 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54059)
store volatile %struct.ScmObj* %ae50653, %struct.ScmObj** %stackaddr$makeclosure54058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50653, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53328$ae506510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54060 = alloca %struct.ScmObj*, align 8
%argslist53328$ae506511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50653, %struct.ScmObj* %argslist53328$ae506510)
store volatile %struct.ScmObj* %argslist53328$ae506511, %struct.ScmObj** %stackaddr$prim54060, align 8
%stackaddr$prim54061 = alloca %struct.ScmObj*, align 8
%argslist53328$ae506512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50652, %struct.ScmObj* %argslist53328$ae506511)
store volatile %struct.ScmObj* %argslist53328$ae506512, %struct.ScmObj** %stackaddr$prim54061, align 8
%clofunc54062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50651)
musttail call tailcc void %clofunc54062(%struct.ScmObj* %ae50651, %struct.ScmObj* %argslist53328$ae506512)
ret void
}

define tailcc void @proc_clo$ae50651(%struct.ScmObj* %env$ae50651,%struct.ScmObj* %current_45args53248) {
%stackaddr$prim54063 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim54063, align 8
%stackaddr$prim54064 = alloca %struct.ScmObj*, align 8
%current_45args53249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53248)
store volatile %struct.ScmObj* %current_45args53249, %struct.ScmObj** %stackaddr$prim54064, align 8
%stackaddr$prim54065 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53249)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54065, align 8
%stackaddr$makeclosure54066 = alloca %struct.ScmObj*, align 8
%fptrToInt54067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50749 to i64
%ae50749 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54067)
store volatile %struct.ScmObj* %ae50749, %struct.ScmObj** %stackaddr$makeclosure54066, align 8
%ae50750 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54068 = alloca %struct.ScmObj*, align 8
%fptrToInt54069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50751 to i64
%ae50751 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54069)
store volatile %struct.ScmObj* %ae50751, %struct.ScmObj** %stackaddr$makeclosure54068, align 8
%argslist53315$ae507490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54070 = alloca %struct.ScmObj*, align 8
%argslist53315$ae507491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50751, %struct.ScmObj* %argslist53315$ae507490)
store volatile %struct.ScmObj* %argslist53315$ae507491, %struct.ScmObj** %stackaddr$prim54070, align 8
%stackaddr$prim54071 = alloca %struct.ScmObj*, align 8
%argslist53315$ae507492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50750, %struct.ScmObj* %argslist53315$ae507491)
store volatile %struct.ScmObj* %argslist53315$ae507492, %struct.ScmObj** %stackaddr$prim54071, align 8
%clofunc54072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50749)
musttail call tailcc void %clofunc54072(%struct.ScmObj* %ae50749, %struct.ScmObj* %argslist53315$ae507492)
ret void
}

define tailcc void @proc_clo$ae50749(%struct.ScmObj* %env$ae50749,%struct.ScmObj* %current_45args53251) {
%stackaddr$prim54073 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54073, align 8
%stackaddr$prim54074 = alloca %struct.ScmObj*, align 8
%current_45args53252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53251)
store volatile %struct.ScmObj* %current_45args53252, %struct.ScmObj** %stackaddr$prim54074, align 8
%stackaddr$prim54075 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53252)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54075, align 8
%stackaddr$makeclosure54076 = alloca %struct.ScmObj*, align 8
%fptrToInt54077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50769 to i64
%ae50769 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54077)
store volatile %struct.ScmObj* %ae50769, %struct.ScmObj** %stackaddr$makeclosure54076, align 8
%ae50770 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54078 = alloca %struct.ScmObj*, align 8
%fptrToInt54079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50771 to i64
%ae50771 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54079)
store volatile %struct.ScmObj* %ae50771, %struct.ScmObj** %stackaddr$makeclosure54078, align 8
%argslist53310$ae507690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54080 = alloca %struct.ScmObj*, align 8
%argslist53310$ae507691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50771, %struct.ScmObj* %argslist53310$ae507690)
store volatile %struct.ScmObj* %argslist53310$ae507691, %struct.ScmObj** %stackaddr$prim54080, align 8
%stackaddr$prim54081 = alloca %struct.ScmObj*, align 8
%argslist53310$ae507692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50770, %struct.ScmObj* %argslist53310$ae507691)
store volatile %struct.ScmObj* %argslist53310$ae507692, %struct.ScmObj** %stackaddr$prim54081, align 8
%clofunc54082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50769)
musttail call tailcc void %clofunc54082(%struct.ScmObj* %ae50769, %struct.ScmObj* %argslist53310$ae507692)
ret void
}

define tailcc void @proc_clo$ae50769(%struct.ScmObj* %env$ae50769,%struct.ScmObj* %current_45args53254) {
%stackaddr$prim54083 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54083, align 8
%stackaddr$prim54084 = alloca %struct.ScmObj*, align 8
%current_45args53255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53254)
store volatile %struct.ScmObj* %current_45args53255, %struct.ScmObj** %stackaddr$prim54084, align 8
%stackaddr$prim54085 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53255)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54085, align 8
%stackaddr$makeclosure54086 = alloca %struct.ScmObj*, align 8
%fptrToInt54087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50791 to i64
%ae50791 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54087)
store volatile %struct.ScmObj* %ae50791, %struct.ScmObj** %stackaddr$makeclosure54086, align 8
%ae50792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54088 = alloca %struct.ScmObj*, align 8
%fptrToInt54089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50793 to i64
%ae50793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54089)
store volatile %struct.ScmObj* %ae50793, %struct.ScmObj** %stackaddr$makeclosure54088, align 8
%argslist53305$ae507910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54090 = alloca %struct.ScmObj*, align 8
%argslist53305$ae507911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50793, %struct.ScmObj* %argslist53305$ae507910)
store volatile %struct.ScmObj* %argslist53305$ae507911, %struct.ScmObj** %stackaddr$prim54090, align 8
%stackaddr$prim54091 = alloca %struct.ScmObj*, align 8
%argslist53305$ae507912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50792, %struct.ScmObj* %argslist53305$ae507911)
store volatile %struct.ScmObj* %argslist53305$ae507912, %struct.ScmObj** %stackaddr$prim54091, align 8
%clofunc54092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50791)
musttail call tailcc void %clofunc54092(%struct.ScmObj* %ae50791, %struct.ScmObj* %argslist53305$ae507912)
ret void
}

define tailcc void @proc_clo$ae50791(%struct.ScmObj* %env$ae50791,%struct.ScmObj* %current_45args53257) {
%stackaddr$prim54093 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim54093, align 8
%stackaddr$prim54094 = alloca %struct.ScmObj*, align 8
%current_45args53258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53257)
store volatile %struct.ScmObj* %current_45args53258, %struct.ScmObj** %stackaddr$prim54094, align 8
%stackaddr$prim54095 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53258)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54095, align 8
%stackaddr$makeclosure54096 = alloca %struct.ScmObj*, align 8
%fptrToInt54097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50815 to i64
%ae50815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54097)
store volatile %struct.ScmObj* %ae50815, %struct.ScmObj** %stackaddr$makeclosure54096, align 8
%ae50816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54098 = alloca %struct.ScmObj*, align 8
%fptrToInt54099 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50817 to i64
%ae50817 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54099)
store volatile %struct.ScmObj* %ae50817, %struct.ScmObj** %stackaddr$makeclosure54098, align 8
%argslist53300$ae508150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54100 = alloca %struct.ScmObj*, align 8
%argslist53300$ae508151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50817, %struct.ScmObj* %argslist53300$ae508150)
store volatile %struct.ScmObj* %argslist53300$ae508151, %struct.ScmObj** %stackaddr$prim54100, align 8
%stackaddr$prim54101 = alloca %struct.ScmObj*, align 8
%argslist53300$ae508152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50816, %struct.ScmObj* %argslist53300$ae508151)
store volatile %struct.ScmObj* %argslist53300$ae508152, %struct.ScmObj** %stackaddr$prim54101, align 8
%clofunc54102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50815)
musttail call tailcc void %clofunc54102(%struct.ScmObj* %ae50815, %struct.ScmObj* %argslist53300$ae508152)
ret void
}

define tailcc void @proc_clo$ae50815(%struct.ScmObj* %env$ae50815,%struct.ScmObj* %current_45args53260) {
%stackaddr$prim54103 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim54103, align 8
%stackaddr$prim54104 = alloca %struct.ScmObj*, align 8
%current_45args53261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53260)
store volatile %struct.ScmObj* %current_45args53261, %struct.ScmObj** %stackaddr$prim54104, align 8
%stackaddr$prim54105 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53261)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54105, align 8
%ae50841 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50842 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54106 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae50841, %struct.ScmObj* %ae50842)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54106, align 8
%truthy$cmp54107 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47298)
%cmp$cmp54107 = icmp eq i64 %truthy$cmp54107, 1
br i1 %cmp$cmp54107, label %truebranch$cmp54107, label %falsebranch$cmp54107
truebranch$cmp54107:
%stackaddr$makeclosure54108 = alloca %struct.ScmObj*, align 8
%fptrToInt54109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50844 to i64
%ae50844 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54109)
store volatile %struct.ScmObj* %ae50844, %struct.ScmObj** %stackaddr$makeclosure54108, align 8
%ae50845 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50846 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53267$ae508440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54110 = alloca %struct.ScmObj*, align 8
%argslist53267$ae508441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50846, %struct.ScmObj* %argslist53267$ae508440)
store volatile %struct.ScmObj* %argslist53267$ae508441, %struct.ScmObj** %stackaddr$prim54110, align 8
%stackaddr$prim54111 = alloca %struct.ScmObj*, align 8
%argslist53267$ae508442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50845, %struct.ScmObj* %argslist53267$ae508441)
store volatile %struct.ScmObj* %argslist53267$ae508442, %struct.ScmObj** %stackaddr$prim54111, align 8
%clofunc54112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50844)
musttail call tailcc void %clofunc54112(%struct.ScmObj* %ae50844, %struct.ScmObj* %argslist53267$ae508442)
ret void
falsebranch$cmp54107:
%ae50859 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50860 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54113 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae50859, %struct.ScmObj* %ae50860)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim54113, align 8
%truthy$cmp54114 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47299)
%cmp$cmp54114 = icmp eq i64 %truthy$cmp54114, 1
br i1 %cmp$cmp54114, label %truebranch$cmp54114, label %falsebranch$cmp54114
truebranch$cmp54114:
%stackaddr$prim54115 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim54115, align 8
%stackaddr$prim54116 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_void_63(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim54116, align 8
%truthy$cmp54117 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47301)
%cmp$cmp54117 = icmp eq i64 %truthy$cmp54117, 1
br i1 %cmp$cmp54117, label %truebranch$cmp54117, label %falsebranch$cmp54117
truebranch$cmp54117:
%ae50864 = call %struct.ScmObj* @const_init_true()
%truthy$cmp54118 = call i64 @is_truthy_value(%struct.ScmObj* %ae50864)
%cmp$cmp54118 = icmp eq i64 %truthy$cmp54118, 1
br i1 %cmp$cmp54118, label %truebranch$cmp54118, label %falsebranch$cmp54118
truebranch$cmp54118:
%stackaddr$makeclosure54119 = alloca %struct.ScmObj*, align 8
%fptrToInt54120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50865 to i64
%ae50865 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54120)
store volatile %struct.ScmObj* %ae50865, %struct.ScmObj** %stackaddr$makeclosure54119, align 8
%ae50866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54121 = alloca %struct.ScmObj*, align 8
%fptrToInt54122 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50867 to i64
%ae50867 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54122)
store volatile %struct.ScmObj* %ae50867, %struct.ScmObj** %stackaddr$makeclosure54121, align 8
%argslist53280$ae508650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54123 = alloca %struct.ScmObj*, align 8
%argslist53280$ae508651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50867, %struct.ScmObj* %argslist53280$ae508650)
store volatile %struct.ScmObj* %argslist53280$ae508651, %struct.ScmObj** %stackaddr$prim54123, align 8
%stackaddr$prim54124 = alloca %struct.ScmObj*, align 8
%argslist53280$ae508652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50866, %struct.ScmObj* %argslist53280$ae508651)
store volatile %struct.ScmObj* %argslist53280$ae508652, %struct.ScmObj** %stackaddr$prim54124, align 8
%clofunc54125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50865)
musttail call tailcc void %clofunc54125(%struct.ScmObj* %ae50865, %struct.ScmObj* %argslist53280$ae508652)
ret void
falsebranch$cmp54118:
%stackaddr$makeclosure54126 = alloca %struct.ScmObj*, align 8
%fptrToInt54127 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50901 to i64
%ae50901 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54127)
store volatile %struct.ScmObj* %ae50901, %struct.ScmObj** %stackaddr$makeclosure54126, align 8
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50903 = call %struct.ScmObj* @const_init_int(i64 5)
%argslist53285$ae509010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54128 = alloca %struct.ScmObj*, align 8
%argslist53285$ae509011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50903, %struct.ScmObj* %argslist53285$ae509010)
store volatile %struct.ScmObj* %argslist53285$ae509011, %struct.ScmObj** %stackaddr$prim54128, align 8
%stackaddr$prim54129 = alloca %struct.ScmObj*, align 8
%argslist53285$ae509012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50902, %struct.ScmObj* %argslist53285$ae509011)
store volatile %struct.ScmObj* %argslist53285$ae509012, %struct.ScmObj** %stackaddr$prim54129, align 8
%clofunc54130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50901)
musttail call tailcc void %clofunc54130(%struct.ScmObj* %ae50901, %struct.ScmObj* %argslist53285$ae509012)
ret void
falsebranch$cmp54117:
%stackaddr$makeclosure54131 = alloca %struct.ScmObj*, align 8
%fptrToInt54132 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50935 to i64
%ae50935 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54132)
store volatile %struct.ScmObj* %ae50935, %struct.ScmObj** %stackaddr$makeclosure54131, align 8
%ae50936 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50937 = call %struct.ScmObj* @const_init_int(i64 3)
%argslist53290$ae509350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54133 = alloca %struct.ScmObj*, align 8
%argslist53290$ae509351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50937, %struct.ScmObj* %argslist53290$ae509350)
store volatile %struct.ScmObj* %argslist53290$ae509351, %struct.ScmObj** %stackaddr$prim54133, align 8
%stackaddr$prim54134 = alloca %struct.ScmObj*, align 8
%argslist53290$ae509352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50936, %struct.ScmObj* %argslist53290$ae509351)
store volatile %struct.ScmObj* %argslist53290$ae509352, %struct.ScmObj** %stackaddr$prim54134, align 8
%clofunc54135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50935)
musttail call tailcc void %clofunc54135(%struct.ScmObj* %ae50935, %struct.ScmObj* %argslist53290$ae509352)
ret void
falsebranch$cmp54114:
%stackaddr$makeclosure54136 = alloca %struct.ScmObj*, align 8
%fptrToInt54137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50950 to i64
%ae50950 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54137)
store volatile %struct.ScmObj* %ae50950, %struct.ScmObj** %stackaddr$makeclosure54136, align 8
%ae50951 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50952 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist53295$ae509500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54138 = alloca %struct.ScmObj*, align 8
%argslist53295$ae509501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50952, %struct.ScmObj* %argslist53295$ae509500)
store volatile %struct.ScmObj* %argslist53295$ae509501, %struct.ScmObj** %stackaddr$prim54138, align 8
%stackaddr$prim54139 = alloca %struct.ScmObj*, align 8
%argslist53295$ae509502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50951, %struct.ScmObj* %argslist53295$ae509501)
store volatile %struct.ScmObj* %argslist53295$ae509502, %struct.ScmObj** %stackaddr$prim54139, align 8
%clofunc54140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50950)
musttail call tailcc void %clofunc54140(%struct.ScmObj* %ae50950, %struct.ScmObj* %argslist53295$ae509502)
ret void
}

define tailcc void @proc_clo$ae50844(%struct.ScmObj* %env$ae50844,%struct.ScmObj* %current_45args53263) {
%stackaddr$prim54141 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54141, align 8
%stackaddr$prim54142 = alloca %struct.ScmObj*, align 8
%current_45args53264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53263)
store volatile %struct.ScmObj* %current_45args53264, %struct.ScmObj** %stackaddr$prim54142, align 8
%stackaddr$prim54143 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53264)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54143, align 8
%stackaddr$prim54144 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54144, align 8
%argslist53266$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54145 = alloca %struct.ScmObj*, align 8
%argslist53266$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53266$k0)
store volatile %struct.ScmObj* %argslist53266$k1, %struct.ScmObj** %stackaddr$prim54145, align 8
%clofunc54146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54146(%struct.ScmObj* %k, %struct.ScmObj* %argslist53266$k1)
ret void
}

define tailcc void @proc_clo$ae50865(%struct.ScmObj* %env$ae50865,%struct.ScmObj* %current_45args53268) {
%stackaddr$prim54147 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53268)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim54147, align 8
%stackaddr$prim54148 = alloca %struct.ScmObj*, align 8
%current_45args53269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53268)
store volatile %struct.ScmObj* %current_45args53269, %struct.ScmObj** %stackaddr$prim54148, align 8
%stackaddr$prim54149 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53269)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim54149, align 8
%stackaddr$makeclosure54150 = alloca %struct.ScmObj*, align 8
%fptrToInt54151 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50884 to i64
%ae50884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54151)
store volatile %struct.ScmObj* %ae50884, %struct.ScmObj** %stackaddr$makeclosure54150, align 8
%ae50885 = call %struct.ScmObj* @const_init_int(i64 11)
%argslist53275$anf_45bind473020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54152 = alloca %struct.ScmObj*, align 8
%argslist53275$anf_45bind473021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50885, %struct.ScmObj* %argslist53275$anf_45bind473020)
store volatile %struct.ScmObj* %argslist53275$anf_45bind473021, %struct.ScmObj** %stackaddr$prim54152, align 8
%stackaddr$prim54153 = alloca %struct.ScmObj*, align 8
%argslist53275$anf_45bind473022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50884, %struct.ScmObj* %argslist53275$anf_45bind473021)
store volatile %struct.ScmObj* %argslist53275$anf_45bind473022, %struct.ScmObj** %stackaddr$prim54153, align 8
%clofunc54154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47302)
musttail call tailcc void %clofunc54154(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %argslist53275$anf_45bind473022)
ret void
}

define tailcc void @proc_clo$ae50884(%struct.ScmObj* %env$ae50884,%struct.ScmObj* %current_45args53271) {
%stackaddr$prim54155 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53271)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54155, align 8
%stackaddr$prim54156 = alloca %struct.ScmObj*, align 8
%current_45args53272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53271)
store volatile %struct.ScmObj* %current_45args53272, %struct.ScmObj** %stackaddr$prim54156, align 8
%stackaddr$prim54157 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53272)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54157, align 8
%stackaddr$prim54158 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54158, align 8
%argslist53274$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54159 = alloca %struct.ScmObj*, align 8
%argslist53274$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53274$k0)
store volatile %struct.ScmObj* %argslist53274$k1, %struct.ScmObj** %stackaddr$prim54159, align 8
%clofunc54160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54160(%struct.ScmObj* %k, %struct.ScmObj* %argslist53274$k1)
ret void
}

define tailcc void @proc_clo$ae50867(%struct.ScmObj* %env$ae50867,%struct.ScmObj* %current_45args53276) {
%stackaddr$prim54161 = alloca %struct.ScmObj*, align 8
%k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53276)
store volatile %struct.ScmObj* %k47336, %struct.ScmObj** %stackaddr$prim54161, align 8
%stackaddr$prim54162 = alloca %struct.ScmObj*, align 8
%current_45args53277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53276)
store volatile %struct.ScmObj* %current_45args53277, %struct.ScmObj** %stackaddr$prim54162, align 8
%stackaddr$prim54163 = alloca %struct.ScmObj*, align 8
%a47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53277)
store volatile %struct.ScmObj* %a47186, %struct.ScmObj** %stackaddr$prim54163, align 8
%ae50869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53279$k473360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54164 = alloca %struct.ScmObj*, align 8
%argslist53279$k473361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47186, %struct.ScmObj* %argslist53279$k473360)
store volatile %struct.ScmObj* %argslist53279$k473361, %struct.ScmObj** %stackaddr$prim54164, align 8
%stackaddr$prim54165 = alloca %struct.ScmObj*, align 8
%argslist53279$k473362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50869, %struct.ScmObj* %argslist53279$k473361)
store volatile %struct.ScmObj* %argslist53279$k473362, %struct.ScmObj** %stackaddr$prim54165, align 8
%clofunc54166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47336)
musttail call tailcc void %clofunc54166(%struct.ScmObj* %k47336, %struct.ScmObj* %argslist53279$k473362)
ret void
}

define tailcc void @proc_clo$ae50901(%struct.ScmObj* %env$ae50901,%struct.ScmObj* %current_45args53281) {
%stackaddr$prim54167 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53281)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54167, align 8
%stackaddr$prim54168 = alloca %struct.ScmObj*, align 8
%current_45args53282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53281)
store volatile %struct.ScmObj* %current_45args53282, %struct.ScmObj** %stackaddr$prim54168, align 8
%stackaddr$prim54169 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53282)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54169, align 8
%stackaddr$prim54170 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54170, align 8
%argslist53284$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54171 = alloca %struct.ScmObj*, align 8
%argslist53284$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53284$k0)
store volatile %struct.ScmObj* %argslist53284$k1, %struct.ScmObj** %stackaddr$prim54171, align 8
%clofunc54172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54172(%struct.ScmObj* %k, %struct.ScmObj* %argslist53284$k1)
ret void
}

define tailcc void @proc_clo$ae50935(%struct.ScmObj* %env$ae50935,%struct.ScmObj* %current_45args53286) {
%stackaddr$prim54173 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53286)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54173, align 8
%stackaddr$prim54174 = alloca %struct.ScmObj*, align 8
%current_45args53287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53286)
store volatile %struct.ScmObj* %current_45args53287, %struct.ScmObj** %stackaddr$prim54174, align 8
%stackaddr$prim54175 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53287)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54175, align 8
%stackaddr$prim54176 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54176, align 8
%argslist53289$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54177 = alloca %struct.ScmObj*, align 8
%argslist53289$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53289$k0)
store volatile %struct.ScmObj* %argslist53289$k1, %struct.ScmObj** %stackaddr$prim54177, align 8
%clofunc54178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54178(%struct.ScmObj* %k, %struct.ScmObj* %argslist53289$k1)
ret void
}

define tailcc void @proc_clo$ae50950(%struct.ScmObj* %env$ae50950,%struct.ScmObj* %current_45args53291) {
%stackaddr$prim54179 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54179, align 8
%stackaddr$prim54180 = alloca %struct.ScmObj*, align 8
%current_45args53292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53291)
store volatile %struct.ScmObj* %current_45args53292, %struct.ScmObj** %stackaddr$prim54180, align 8
%stackaddr$prim54181 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53292)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54181, align 8
%stackaddr$prim54182 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54182, align 8
%argslist53294$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54183 = alloca %struct.ScmObj*, align 8
%argslist53294$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53294$k0)
store volatile %struct.ScmObj* %argslist53294$k1, %struct.ScmObj** %stackaddr$prim54183, align 8
%clofunc54184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54184(%struct.ScmObj* %k, %struct.ScmObj* %argslist53294$k1)
ret void
}

define tailcc void @proc_clo$ae50817(%struct.ScmObj* %env$ae50817,%struct.ScmObj* %current_45args53296) {
%stackaddr$prim54185 = alloca %struct.ScmObj*, align 8
%k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53296)
store volatile %struct.ScmObj* %k47337, %struct.ScmObj** %stackaddr$prim54185, align 8
%stackaddr$prim54186 = alloca %struct.ScmObj*, align 8
%current_45args53297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53296)
store volatile %struct.ScmObj* %current_45args53297, %struct.ScmObj** %stackaddr$prim54186, align 8
%stackaddr$prim54187 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53297)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim54187, align 8
%stackaddr$prim54188 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim54188, align 8
%stackaddr$prim54189 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim54189, align 8
%stackaddr$prim54190 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54190, align 8
%stackaddr$prim54191 = alloca %struct.ScmObj*, align 8
%cpsprim47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %cpsprim47338, %struct.ScmObj** %stackaddr$prim54191, align 8
%ae50823 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53299$k473370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54192 = alloca %struct.ScmObj*, align 8
%argslist53299$k473371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47338, %struct.ScmObj* %argslist53299$k473370)
store volatile %struct.ScmObj* %argslist53299$k473371, %struct.ScmObj** %stackaddr$prim54192, align 8
%stackaddr$prim54193 = alloca %struct.ScmObj*, align 8
%argslist53299$k473372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50823, %struct.ScmObj* %argslist53299$k473371)
store volatile %struct.ScmObj* %argslist53299$k473372, %struct.ScmObj** %stackaddr$prim54193, align 8
%clofunc54194 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47337)
musttail call tailcc void %clofunc54194(%struct.ScmObj* %k47337, %struct.ScmObj* %argslist53299$k473372)
ret void
}

define tailcc void @proc_clo$ae50793(%struct.ScmObj* %env$ae50793,%struct.ScmObj* %current_45args53301) {
%stackaddr$prim54195 = alloca %struct.ScmObj*, align 8
%k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53301)
store volatile %struct.ScmObj* %k47339, %struct.ScmObj** %stackaddr$prim54195, align 8
%stackaddr$prim54196 = alloca %struct.ScmObj*, align 8
%current_45args53302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53301)
store volatile %struct.ScmObj* %current_45args53302, %struct.ScmObj** %stackaddr$prim54196, align 8
%stackaddr$prim54197 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53302)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim54197, align 8
%stackaddr$prim54198 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim54198, align 8
%stackaddr$prim54199 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim54199, align 8
%stackaddr$prim54200 = alloca %struct.ScmObj*, align 8
%cpsprim47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %cpsprim47340, %struct.ScmObj** %stackaddr$prim54200, align 8
%ae50798 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53304$k473390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54201 = alloca %struct.ScmObj*, align 8
%argslist53304$k473391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47340, %struct.ScmObj* %argslist53304$k473390)
store volatile %struct.ScmObj* %argslist53304$k473391, %struct.ScmObj** %stackaddr$prim54201, align 8
%stackaddr$prim54202 = alloca %struct.ScmObj*, align 8
%argslist53304$k473392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50798, %struct.ScmObj* %argslist53304$k473391)
store volatile %struct.ScmObj* %argslist53304$k473392, %struct.ScmObj** %stackaddr$prim54202, align 8
%clofunc54203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47339)
musttail call tailcc void %clofunc54203(%struct.ScmObj* %k47339, %struct.ScmObj* %argslist53304$k473392)
ret void
}

define tailcc void @proc_clo$ae50771(%struct.ScmObj* %env$ae50771,%struct.ScmObj* %current_45args53306) {
%stackaddr$prim54204 = alloca %struct.ScmObj*, align 8
%k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53306)
store volatile %struct.ScmObj* %k47341, %struct.ScmObj** %stackaddr$prim54204, align 8
%stackaddr$prim54205 = alloca %struct.ScmObj*, align 8
%current_45args53307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53306)
store volatile %struct.ScmObj* %current_45args53307, %struct.ScmObj** %stackaddr$prim54205, align 8
%stackaddr$prim54206 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53307)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim54206, align 8
%stackaddr$prim54207 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim54207, align 8
%stackaddr$prim54208 = alloca %struct.ScmObj*, align 8
%cpsprim47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %cpsprim47342, %struct.ScmObj** %stackaddr$prim54208, align 8
%ae50775 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53309$k473410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54209 = alloca %struct.ScmObj*, align 8
%argslist53309$k473411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47342, %struct.ScmObj* %argslist53309$k473410)
store volatile %struct.ScmObj* %argslist53309$k473411, %struct.ScmObj** %stackaddr$prim54209, align 8
%stackaddr$prim54210 = alloca %struct.ScmObj*, align 8
%argslist53309$k473412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50775, %struct.ScmObj* %argslist53309$k473411)
store volatile %struct.ScmObj* %argslist53309$k473412, %struct.ScmObj** %stackaddr$prim54210, align 8
%clofunc54211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47341)
musttail call tailcc void %clofunc54211(%struct.ScmObj* %k47341, %struct.ScmObj* %argslist53309$k473412)
ret void
}

define tailcc void @proc_clo$ae50751(%struct.ScmObj* %env$ae50751,%struct.ScmObj* %current_45args53311) {
%stackaddr$prim54212 = alloca %struct.ScmObj*, align 8
%k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53311)
store volatile %struct.ScmObj* %k47343, %struct.ScmObj** %stackaddr$prim54212, align 8
%stackaddr$prim54213 = alloca %struct.ScmObj*, align 8
%current_45args53312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53311)
store volatile %struct.ScmObj* %current_45args53312, %struct.ScmObj** %stackaddr$prim54213, align 8
%stackaddr$prim54214 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53312)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim54214, align 8
%stackaddr$prim54215 = alloca %struct.ScmObj*, align 8
%cpsprim47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47344, %struct.ScmObj** %stackaddr$prim54215, align 8
%ae50754 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53314$k473430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54216 = alloca %struct.ScmObj*, align 8
%argslist53314$k473431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47344, %struct.ScmObj* %argslist53314$k473430)
store volatile %struct.ScmObj* %argslist53314$k473431, %struct.ScmObj** %stackaddr$prim54216, align 8
%stackaddr$prim54217 = alloca %struct.ScmObj*, align 8
%argslist53314$k473432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50754, %struct.ScmObj* %argslist53314$k473431)
store volatile %struct.ScmObj* %argslist53314$k473432, %struct.ScmObj** %stackaddr$prim54217, align 8
%clofunc54218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47343)
musttail call tailcc void %clofunc54218(%struct.ScmObj* %k47343, %struct.ScmObj* %argslist53314$k473432)
ret void
}

define tailcc void @proc_clo$ae50653(%struct.ScmObj* %env$ae50653,%struct.ScmObj* %args4713447345) {
%stackaddr$env-ref54219 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50653, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54219
%stackaddr$prim54220 = alloca %struct.ScmObj*, align 8
%k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447345)
store volatile %struct.ScmObj* %k47346, %struct.ScmObj** %stackaddr$prim54220, align 8
%stackaddr$prim54221 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447345)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim54221, align 8
%stackaddr$prim54222 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim54222, align 8
%truthy$cmp54223 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47286)
%cmp$cmp54223 = icmp eq i64 %truthy$cmp54223, 1
br i1 %cmp$cmp54223, label %truebranch$cmp54223, label %falsebranch$cmp54223
truebranch$cmp54223:
%ae50659 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50660 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53316$k473460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54224 = alloca %struct.ScmObj*, align 8
%argslist53316$k473461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50660, %struct.ScmObj* %argslist53316$k473460)
store volatile %struct.ScmObj* %argslist53316$k473461, %struct.ScmObj** %stackaddr$prim54224, align 8
%stackaddr$prim54225 = alloca %struct.ScmObj*, align 8
%argslist53316$k473462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50659, %struct.ScmObj* %argslist53316$k473461)
store volatile %struct.ScmObj* %argslist53316$k473462, %struct.ScmObj** %stackaddr$prim54225, align 8
%clofunc54226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47346)
musttail call tailcc void %clofunc54226(%struct.ScmObj* %k47346, %struct.ScmObj* %argslist53316$k473462)
ret void
falsebranch$cmp54223:
%stackaddr$prim54227 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim54227, align 8
%stackaddr$prim54228 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim54228, align 8
%truthy$cmp54229 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47288)
%cmp$cmp54229 = icmp eq i64 %truthy$cmp54229, 1
br i1 %cmp$cmp54229, label %truebranch$cmp54229, label %falsebranch$cmp54229
truebranch$cmp54229:
%stackaddr$prim54230 = alloca %struct.ScmObj*, align 8
%cpsprim47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47347, %struct.ScmObj** %stackaddr$prim54230, align 8
%ae50672 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53317$k473460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54231 = alloca %struct.ScmObj*, align 8
%argslist53317$k473461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47347, %struct.ScmObj* %argslist53317$k473460)
store volatile %struct.ScmObj* %argslist53317$k473461, %struct.ScmObj** %stackaddr$prim54231, align 8
%stackaddr$prim54232 = alloca %struct.ScmObj*, align 8
%argslist53317$k473462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50672, %struct.ScmObj* %argslist53317$k473461)
store volatile %struct.ScmObj* %argslist53317$k473462, %struct.ScmObj** %stackaddr$prim54232, align 8
%clofunc54233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47346)
musttail call tailcc void %clofunc54233(%struct.ScmObj* %k47346, %struct.ScmObj* %argslist53317$k473462)
ret void
falsebranch$cmp54229:
%stackaddr$makeclosure54234 = alloca %struct.ScmObj*, align 8
%fptrToInt54235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50677 to i64
%ae50677 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54235)
store volatile %struct.ScmObj* %ae50677, %struct.ScmObj** %stackaddr$makeclosure54234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50677, %struct.ScmObj* %k47346, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50677, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50677, %struct.ScmObj* %args47134, i64 2)
%ae50678 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54236 = alloca %struct.ScmObj*, align 8
%fptrToInt54237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50679 to i64
%ae50679 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54237)
store volatile %struct.ScmObj* %ae50679, %struct.ScmObj** %stackaddr$makeclosure54236, align 8
%argslist53327$ae506770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54238 = alloca %struct.ScmObj*, align 8
%argslist53327$ae506771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50679, %struct.ScmObj* %argslist53327$ae506770)
store volatile %struct.ScmObj* %argslist53327$ae506771, %struct.ScmObj** %stackaddr$prim54238, align 8
%stackaddr$prim54239 = alloca %struct.ScmObj*, align 8
%argslist53327$ae506772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50678, %struct.ScmObj* %argslist53327$ae506771)
store volatile %struct.ScmObj* %argslist53327$ae506772, %struct.ScmObj** %stackaddr$prim54239, align 8
%clofunc54240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50677)
musttail call tailcc void %clofunc54240(%struct.ScmObj* %ae50677, %struct.ScmObj* %argslist53327$ae506772)
ret void
}

define tailcc void @proc_clo$ae50677(%struct.ScmObj* %env$ae50677,%struct.ScmObj* %current_45args53318) {
%stackaddr$env-ref54241 = alloca %struct.ScmObj*, align 8
%k47346 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50677, i64 0)
store %struct.ScmObj* %k47346, %struct.ScmObj** %stackaddr$env-ref54241
%stackaddr$env-ref54242 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50677, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54242
%stackaddr$env-ref54243 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50677, i64 2)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref54243
%stackaddr$prim54244 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54244, align 8
%stackaddr$prim54245 = alloca %struct.ScmObj*, align 8
%current_45args53319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53318)
store volatile %struct.ScmObj* %current_45args53319, %struct.ScmObj** %stackaddr$prim54245, align 8
%stackaddr$prim54246 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53319)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim54246, align 8
%stackaddr$prim54247 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim54247, align 8
%stackaddr$prim54248 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim54248, align 8
%argslist53321$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54249 = alloca %struct.ScmObj*, align 8
%argslist53321$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %argslist53321$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53321$_37foldl1470731, %struct.ScmObj** %stackaddr$prim54249, align 8
%stackaddr$prim54250 = alloca %struct.ScmObj*, align 8
%argslist53321$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47290, %struct.ScmObj* %argslist53321$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53321$_37foldl1470732, %struct.ScmObj** %stackaddr$prim54250, align 8
%stackaddr$prim54251 = alloca %struct.ScmObj*, align 8
%argslist53321$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47289, %struct.ScmObj* %argslist53321$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53321$_37foldl1470733, %struct.ScmObj** %stackaddr$prim54251, align 8
%stackaddr$prim54252 = alloca %struct.ScmObj*, align 8
%argslist53321$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47346, %struct.ScmObj* %argslist53321$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53321$_37foldl1470734, %struct.ScmObj** %stackaddr$prim54252, align 8
%clofunc54253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc54253(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53321$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50679(%struct.ScmObj* %env$ae50679,%struct.ScmObj* %current_45args53322) {
%stackaddr$prim54254 = alloca %struct.ScmObj*, align 8
%k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53322)
store volatile %struct.ScmObj* %k47349, %struct.ScmObj** %stackaddr$prim54254, align 8
%stackaddr$prim54255 = alloca %struct.ScmObj*, align 8
%current_45args53323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53322)
store volatile %struct.ScmObj* %current_45args53323, %struct.ScmObj** %stackaddr$prim54255, align 8
%stackaddr$prim54256 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim54256, align 8
%stackaddr$prim54257 = alloca %struct.ScmObj*, align 8
%current_45args53324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53323)
store volatile %struct.ScmObj* %current_45args53324, %struct.ScmObj** %stackaddr$prim54257, align 8
%stackaddr$prim54258 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53324)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim54258, align 8
%stackaddr$prim54259 = alloca %struct.ScmObj*, align 8
%cpsprim47350 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47350, %struct.ScmObj** %stackaddr$prim54259, align 8
%ae50683 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53326$k473490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54260 = alloca %struct.ScmObj*, align 8
%argslist53326$k473491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47350, %struct.ScmObj* %argslist53326$k473490)
store volatile %struct.ScmObj* %argslist53326$k473491, %struct.ScmObj** %stackaddr$prim54260, align 8
%stackaddr$prim54261 = alloca %struct.ScmObj*, align 8
%argslist53326$k473492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50683, %struct.ScmObj* %argslist53326$k473491)
store volatile %struct.ScmObj* %argslist53326$k473492, %struct.ScmObj** %stackaddr$prim54261, align 8
%clofunc54262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47349)
musttail call tailcc void %clofunc54262(%struct.ScmObj* %k47349, %struct.ScmObj* %argslist53326$k473492)
ret void
}

define tailcc void @proc_clo$ae50249(%struct.ScmObj* %env$ae50249,%struct.ScmObj* %current_45args53329) {
%stackaddr$prim54263 = alloca %struct.ScmObj*, align 8
%k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53329)
store volatile %struct.ScmObj* %k47351, %struct.ScmObj** %stackaddr$prim54263, align 8
%stackaddr$prim54264 = alloca %struct.ScmObj*, align 8
%current_45args53330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53329)
store volatile %struct.ScmObj* %current_45args53330, %struct.ScmObj** %stackaddr$prim54264, align 8
%stackaddr$prim54265 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53330)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim54265, align 8
%stackaddr$prim54266 = alloca %struct.ScmObj*, align 8
%current_45args53331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53330)
store volatile %struct.ScmObj* %current_45args53331, %struct.ScmObj** %stackaddr$prim54266, align 8
%stackaddr$prim54267 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53331)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim54267, align 8
%ae50250 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54268 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50250, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim54268, align 8
%stackaddr$makeclosure54269 = alloca %struct.ScmObj*, align 8
%fptrToInt54270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50252 to i64
%ae50252 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54270)
store volatile %struct.ScmObj* %ae50252, %struct.ScmObj** %stackaddr$makeclosure54269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50252, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50252, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50252, %struct.ScmObj* %k47351, i64 2)
%ae50253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54271 = alloca %struct.ScmObj*, align 8
%fptrToInt54272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50254 to i64
%ae50254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54272)
store volatile %struct.ScmObj* %ae50254, %struct.ScmObj** %stackaddr$makeclosure54271, align 8
%argslist53353$ae502520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54273 = alloca %struct.ScmObj*, align 8
%argslist53353$ae502521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50254, %struct.ScmObj* %argslist53353$ae502520)
store volatile %struct.ScmObj* %argslist53353$ae502521, %struct.ScmObj** %stackaddr$prim54273, align 8
%stackaddr$prim54274 = alloca %struct.ScmObj*, align 8
%argslist53353$ae502522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50253, %struct.ScmObj* %argslist53353$ae502521)
store volatile %struct.ScmObj* %argslist53353$ae502522, %struct.ScmObj** %stackaddr$prim54274, align 8
%clofunc54275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50252)
musttail call tailcc void %clofunc54275(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist53353$ae502522)
ret void
}

define tailcc void @proc_clo$ae50252(%struct.ScmObj* %env$ae50252,%struct.ScmObj* %current_45args53333) {
%stackaddr$env-ref54276 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50252, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54276
%stackaddr$env-ref54277 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50252, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54277
%stackaddr$env-ref54278 = alloca %struct.ScmObj*, align 8
%k47351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50252, i64 2)
store %struct.ScmObj* %k47351, %struct.ScmObj** %stackaddr$env-ref54278
%stackaddr$prim54279 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53333)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim54279, align 8
%stackaddr$prim54280 = alloca %struct.ScmObj*, align 8
%current_45args53334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53333)
store volatile %struct.ScmObj* %current_45args53334, %struct.ScmObj** %stackaddr$prim54280, align 8
%stackaddr$prim54281 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53334)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim54281, align 8
%stackaddr$makeclosure54282 = alloca %struct.ScmObj*, align 8
%fptrToInt54283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50268 to i64
%ae50268 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54283)
store volatile %struct.ScmObj* %ae50268, %struct.ScmObj** %stackaddr$makeclosure54282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %k47351, i64 2)
%stackaddr$makeclosure54284 = alloca %struct.ScmObj*, align 8
%fptrToInt54285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50269 to i64
%ae50269 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54285)
store volatile %struct.ScmObj* %ae50269, %struct.ScmObj** %stackaddr$makeclosure54284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50269, %struct.ScmObj* %lst47140, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50269, %struct.ScmObj* %v47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50269, %struct.ScmObj* %k47351, i64 2)
%argslist53348$anf_45bind472780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54286 = alloca %struct.ScmObj*, align 8
%argslist53348$anf_45bind472781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50269, %struct.ScmObj* %argslist53348$anf_45bind472780)
store volatile %struct.ScmObj* %argslist53348$anf_45bind472781, %struct.ScmObj** %stackaddr$prim54286, align 8
%stackaddr$prim54287 = alloca %struct.ScmObj*, align 8
%argslist53348$anf_45bind472782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50268, %struct.ScmObj* %argslist53348$anf_45bind472781)
store volatile %struct.ScmObj* %argslist53348$anf_45bind472782, %struct.ScmObj** %stackaddr$prim54287, align 8
%clofunc54288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47278)
musttail call tailcc void %clofunc54288(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %argslist53348$anf_45bind472782)
ret void
}

define tailcc void @proc_clo$ae50268(%struct.ScmObj* %env$ae50268,%struct.ScmObj* %current_45args53336) {
%stackaddr$env-ref54289 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54289
%stackaddr$env-ref54290 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54290
%stackaddr$env-ref54291 = alloca %struct.ScmObj*, align 8
%k47351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 2)
store %struct.ScmObj* %k47351, %struct.ScmObj** %stackaddr$env-ref54291
%stackaddr$prim54292 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53336)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54292, align 8
%stackaddr$prim54293 = alloca %struct.ScmObj*, align 8
%current_45args53337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53336)
store volatile %struct.ScmObj* %current_45args53337, %struct.ScmObj** %stackaddr$prim54293, align 8
%stackaddr$prim54294 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53337)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54294, align 8
%ae50377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54295 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50377)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54295, align 8
%stackaddr$prim54296 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54296, align 8
%truthy$cmp54297 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47280)
%cmp$cmp54297 = icmp eq i64 %truthy$cmp54297, 1
br i1 %cmp$cmp54297, label %truebranch$cmp54297, label %falsebranch$cmp54297
truebranch$cmp54297:
%ae50381 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50382 = call %struct.ScmObj* @const_init_false()
%argslist53339$k473510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54298 = alloca %struct.ScmObj*, align 8
%argslist53339$k473511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50382, %struct.ScmObj* %argslist53339$k473510)
store volatile %struct.ScmObj* %argslist53339$k473511, %struct.ScmObj** %stackaddr$prim54298, align 8
%stackaddr$prim54299 = alloca %struct.ScmObj*, align 8
%argslist53339$k473512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50381, %struct.ScmObj* %argslist53339$k473511)
store volatile %struct.ScmObj* %argslist53339$k473512, %struct.ScmObj** %stackaddr$prim54299, align 8
%clofunc54300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47351)
musttail call tailcc void %clofunc54300(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53339$k473512)
ret void
falsebranch$cmp54297:
%ae50390 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54301 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50390)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54301, align 8
%stackaddr$prim54302 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54302, align 8
%stackaddr$prim54303 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54303, align 8
%truthy$cmp54304 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47283)
%cmp$cmp54304 = icmp eq i64 %truthy$cmp54304, 1
br i1 %cmp$cmp54304, label %truebranch$cmp54304, label %falsebranch$cmp54304
truebranch$cmp54304:
%ae50396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54305 = alloca %struct.ScmObj*, align 8
%cpsprim47354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50396)
store volatile %struct.ScmObj* %cpsprim47354, %struct.ScmObj** %stackaddr$prim54305, align 8
%ae50398 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53340$k473510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54306 = alloca %struct.ScmObj*, align 8
%argslist53340$k473511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47354, %struct.ScmObj* %argslist53340$k473510)
store volatile %struct.ScmObj* %argslist53340$k473511, %struct.ScmObj** %stackaddr$prim54306, align 8
%stackaddr$prim54307 = alloca %struct.ScmObj*, align 8
%argslist53340$k473512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50398, %struct.ScmObj* %argslist53340$k473511)
store volatile %struct.ScmObj* %argslist53340$k473512, %struct.ScmObj** %stackaddr$prim54307, align 8
%clofunc54308 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47351)
musttail call tailcc void %clofunc54308(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53340$k473512)
ret void
falsebranch$cmp54304:
%ae50409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54309 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50409)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54309, align 8
%stackaddr$prim54310 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54310, align 8
%ae50412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54311 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50412, %struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54311, align 8
%argslist53341$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54312 = alloca %struct.ScmObj*, align 8
%argslist53341$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53341$cc471410)
store volatile %struct.ScmObj* %argslist53341$cc471411, %struct.ScmObj** %stackaddr$prim54312, align 8
%stackaddr$prim54313 = alloca %struct.ScmObj*, align 8
%argslist53341$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53341$cc471411)
store volatile %struct.ScmObj* %argslist53341$cc471412, %struct.ScmObj** %stackaddr$prim54313, align 8
%clofunc54314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54314(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53341$cc471412)
ret void
}

define tailcc void @proc_clo$ae50269(%struct.ScmObj* %env$ae50269,%struct.ScmObj* %current_45args53342) {
%stackaddr$env-ref54315 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50269, i64 0)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref54315
%stackaddr$env-ref54316 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50269, i64 1)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref54316
%stackaddr$env-ref54317 = alloca %struct.ScmObj*, align 8
%k47351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50269, i64 2)
store %struct.ScmObj* %k47351, %struct.ScmObj** %stackaddr$env-ref54317
%stackaddr$prim54318 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53342)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54318, align 8
%stackaddr$prim54319 = alloca %struct.ScmObj*, align 8
%current_45args53343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53342)
store volatile %struct.ScmObj* %current_45args53343, %struct.ScmObj** %stackaddr$prim54319, align 8
%stackaddr$prim54320 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53343)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim54320, align 8
%ae50271 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54321 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50271)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim54321, align 8
%stackaddr$prim54322 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim54322, align 8
%truthy$cmp54323 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47280)
%cmp$cmp54323 = icmp eq i64 %truthy$cmp54323, 1
br i1 %cmp$cmp54323, label %truebranch$cmp54323, label %falsebranch$cmp54323
truebranch$cmp54323:
%ae50275 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50276 = call %struct.ScmObj* @const_init_false()
%argslist53345$k473510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54324 = alloca %struct.ScmObj*, align 8
%argslist53345$k473511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50276, %struct.ScmObj* %argslist53345$k473510)
store volatile %struct.ScmObj* %argslist53345$k473511, %struct.ScmObj** %stackaddr$prim54324, align 8
%stackaddr$prim54325 = alloca %struct.ScmObj*, align 8
%argslist53345$k473512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist53345$k473511)
store volatile %struct.ScmObj* %argslist53345$k473512, %struct.ScmObj** %stackaddr$prim54325, align 8
%clofunc54326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47351)
musttail call tailcc void %clofunc54326(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53345$k473512)
ret void
falsebranch$cmp54323:
%ae50284 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54327 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50284)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim54327, align 8
%stackaddr$prim54328 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim54328, align 8
%stackaddr$prim54329 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim54329, align 8
%truthy$cmp54330 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47283)
%cmp$cmp54330 = icmp eq i64 %truthy$cmp54330, 1
br i1 %cmp$cmp54330, label %truebranch$cmp54330, label %falsebranch$cmp54330
truebranch$cmp54330:
%ae50290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54331 = alloca %struct.ScmObj*, align 8
%cpsprim47354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50290)
store volatile %struct.ScmObj* %cpsprim47354, %struct.ScmObj** %stackaddr$prim54331, align 8
%ae50292 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53346$k473510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54332 = alloca %struct.ScmObj*, align 8
%argslist53346$k473511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47354, %struct.ScmObj* %argslist53346$k473510)
store volatile %struct.ScmObj* %argslist53346$k473511, %struct.ScmObj** %stackaddr$prim54332, align 8
%stackaddr$prim54333 = alloca %struct.ScmObj*, align 8
%argslist53346$k473512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50292, %struct.ScmObj* %argslist53346$k473511)
store volatile %struct.ScmObj* %argslist53346$k473512, %struct.ScmObj** %stackaddr$prim54333, align 8
%clofunc54334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47351)
musttail call tailcc void %clofunc54334(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53346$k473512)
ret void
falsebranch$cmp54330:
%ae50303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54335 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50303)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim54335, align 8
%stackaddr$prim54336 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim54336, align 8
%ae50306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54337 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50306, %struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim54337, align 8
%argslist53347$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54338 = alloca %struct.ScmObj*, align 8
%argslist53347$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53347$cc471410)
store volatile %struct.ScmObj* %argslist53347$cc471411, %struct.ScmObj** %stackaddr$prim54338, align 8
%stackaddr$prim54339 = alloca %struct.ScmObj*, align 8
%argslist53347$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47351, %struct.ScmObj* %argslist53347$cc471411)
store volatile %struct.ScmObj* %argslist53347$cc471412, %struct.ScmObj** %stackaddr$prim54339, align 8
%clofunc54340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc54340(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53347$cc471412)
ret void
}

define tailcc void @proc_clo$ae50254(%struct.ScmObj* %env$ae50254,%struct.ScmObj* %current_45args53349) {
%stackaddr$prim54341 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53349)
store volatile %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$prim54341, align 8
%stackaddr$prim54342 = alloca %struct.ScmObj*, align 8
%current_45args53350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53349)
store volatile %struct.ScmObj* %current_45args53350, %struct.ScmObj** %stackaddr$prim54342, align 8
%stackaddr$prim54343 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53350)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim54343, align 8
%argslist53352$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54344 = alloca %struct.ScmObj*, align 8
%argslist53352$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53352$u471420)
store volatile %struct.ScmObj* %argslist53352$u471421, %struct.ScmObj** %stackaddr$prim54344, align 8
%stackaddr$prim54345 = alloca %struct.ScmObj*, align 8
%argslist53352$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist53352$u471421)
store volatile %struct.ScmObj* %argslist53352$u471422, %struct.ScmObj** %stackaddr$prim54345, align 8
%clofunc54346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc54346(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53352$u471422)
ret void
}

define tailcc void @proc_clo$ae49713(%struct.ScmObj* %env$ae49713,%struct.ScmObj* %current_45args53355) {
%stackaddr$prim54347 = alloca %struct.ScmObj*, align 8
%k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53355)
store volatile %struct.ScmObj* %k47356, %struct.ScmObj** %stackaddr$prim54347, align 8
%stackaddr$prim54348 = alloca %struct.ScmObj*, align 8
%current_45args53356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53355)
store volatile %struct.ScmObj* %current_45args53356, %struct.ScmObj** %stackaddr$prim54348, align 8
%stackaddr$prim54349 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53356)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim54349, align 8
%stackaddr$prim54350 = alloca %struct.ScmObj*, align 8
%current_45args53357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53356)
store volatile %struct.ScmObj* %current_45args53357, %struct.ScmObj** %stackaddr$prim54350, align 8
%stackaddr$prim54351 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53357)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim54351, align 8
%ae49714 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54352 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49714, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim54352, align 8
%ae49716 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54353 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49716, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim54353, align 8
%stackaddr$makeclosure54354 = alloca %struct.ScmObj*, align 8
%fptrToInt54355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49718 to i64
%ae49718 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54355)
store volatile %struct.ScmObj* %ae49718, %struct.ScmObj** %stackaddr$makeclosure54354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %k47356, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49718, %struct.ScmObj* %lst47147, i64 2)
%ae49719 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54356 = alloca %struct.ScmObj*, align 8
%fptrToInt54357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49720 to i64
%ae49720 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54357)
store volatile %struct.ScmObj* %ae49720, %struct.ScmObj** %stackaddr$makeclosure54356, align 8
%argslist53377$ae497180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54358 = alloca %struct.ScmObj*, align 8
%argslist53377$ae497181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49720, %struct.ScmObj* %argslist53377$ae497180)
store volatile %struct.ScmObj* %argslist53377$ae497181, %struct.ScmObj** %stackaddr$prim54358, align 8
%stackaddr$prim54359 = alloca %struct.ScmObj*, align 8
%argslist53377$ae497182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49719, %struct.ScmObj* %argslist53377$ae497181)
store volatile %struct.ScmObj* %argslist53377$ae497182, %struct.ScmObj** %stackaddr$prim54359, align 8
%clofunc54360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49718)
musttail call tailcc void %clofunc54360(%struct.ScmObj* %ae49718, %struct.ScmObj* %argslist53377$ae497182)
ret void
}

define tailcc void @proc_clo$ae49718(%struct.ScmObj* %env$ae49718,%struct.ScmObj* %current_45args53359) {
%stackaddr$env-ref54361 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54361
%stackaddr$env-ref54362 = alloca %struct.ScmObj*, align 8
%k47356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 1)
store %struct.ScmObj* %k47356, %struct.ScmObj** %stackaddr$env-ref54362
%stackaddr$env-ref54363 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49718, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54363
%stackaddr$prim54364 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53359)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim54364, align 8
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%current_45args53360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53359)
store volatile %struct.ScmObj* %current_45args53360, %struct.ScmObj** %stackaddr$prim54365, align 8
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53360)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54366, align 8
%stackaddr$makeclosure54367 = alloca %struct.ScmObj*, align 8
%fptrToInt54368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49734 to i64
%ae49734 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54368)
store volatile %struct.ScmObj* %ae49734, %struct.ScmObj** %stackaddr$makeclosure54367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %k47356, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %lst47147, i64 2)
%stackaddr$makeclosure54369 = alloca %struct.ScmObj*, align 8
%fptrToInt54370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49735 to i64
%ae49735 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54370)
store volatile %struct.ScmObj* %ae49735, %struct.ScmObj** %stackaddr$makeclosure54369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49735, %struct.ScmObj* %n47148, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49735, %struct.ScmObj* %k47356, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49735, %struct.ScmObj* %lst47147, i64 2)
%argslist53372$anf_45bind472710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54371 = alloca %struct.ScmObj*, align 8
%argslist53372$anf_45bind472711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49735, %struct.ScmObj* %argslist53372$anf_45bind472710)
store volatile %struct.ScmObj* %argslist53372$anf_45bind472711, %struct.ScmObj** %stackaddr$prim54371, align 8
%stackaddr$prim54372 = alloca %struct.ScmObj*, align 8
%argslist53372$anf_45bind472712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49734, %struct.ScmObj* %argslist53372$anf_45bind472711)
store volatile %struct.ScmObj* %argslist53372$anf_45bind472712, %struct.ScmObj** %stackaddr$prim54372, align 8
%clofunc54373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47271)
musttail call tailcc void %clofunc54373(%struct.ScmObj* %anf_45bind47271, %struct.ScmObj* %argslist53372$anf_45bind472712)
ret void
}

define tailcc void @proc_clo$ae49734(%struct.ScmObj* %env$ae49734,%struct.ScmObj* %current_45args53362) {
%stackaddr$env-ref54374 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54374
%stackaddr$env-ref54375 = alloca %struct.ScmObj*, align 8
%k47356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 1)
store %struct.ScmObj* %k47356, %struct.ScmObj** %stackaddr$env-ref54375
%stackaddr$env-ref54376 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54376
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%current_45args53363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53362)
store volatile %struct.ScmObj* %current_45args53363, %struct.ScmObj** %stackaddr$prim54378, align 8
%stackaddr$prim54379 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53363)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54379, align 8
%ae49877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54380 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49877)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54380, align 8
%ae49878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54381 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49878, %struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54381, align 8
%truthy$cmp54382 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47273)
%cmp$cmp54382 = icmp eq i64 %truthy$cmp54382, 1
br i1 %cmp$cmp54382, label %truebranch$cmp54382, label %falsebranch$cmp54382
truebranch$cmp54382:
%ae49882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54383 = alloca %struct.ScmObj*, align 8
%cpsprim47359 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49882)
store volatile %struct.ScmObj* %cpsprim47359, %struct.ScmObj** %stackaddr$prim54383, align 8
%ae49884 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53365$k473560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%argslist53365$k473561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47359, %struct.ScmObj* %argslist53365$k473560)
store volatile %struct.ScmObj* %argslist53365$k473561, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%argslist53365$k473562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49884, %struct.ScmObj* %argslist53365$k473561)
store volatile %struct.ScmObj* %argslist53365$k473562, %struct.ScmObj** %stackaddr$prim54385, align 8
%clofunc54386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47356)
musttail call tailcc void %clofunc54386(%struct.ScmObj* %k47356, %struct.ScmObj* %argslist53365$k473562)
ret void
falsebranch$cmp54382:
%ae49895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54387 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49895)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54387, align 8
%stackaddr$prim54388 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54388, align 8
%ae49898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54389 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49898, %struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54389, align 8
%ae49901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54390 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49901)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54390, align 8
%ae49903 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47276, %struct.ScmObj* %ae49903)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54391, align 8
%ae49905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54392 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49905, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54392, align 8
%argslist53366$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54393 = alloca %struct.ScmObj*, align 8
%argslist53366$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53366$cc471490)
store volatile %struct.ScmObj* %argslist53366$cc471491, %struct.ScmObj** %stackaddr$prim54393, align 8
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%argslist53366$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47356, %struct.ScmObj* %argslist53366$cc471491)
store volatile %struct.ScmObj* %argslist53366$cc471492, %struct.ScmObj** %stackaddr$prim54394, align 8
%clofunc54395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54395(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53366$cc471492)
ret void
}

define tailcc void @proc_clo$ae49735(%struct.ScmObj* %env$ae49735,%struct.ScmObj* %current_45args53367) {
%stackaddr$env-ref54396 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49735, i64 0)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref54396
%stackaddr$env-ref54397 = alloca %struct.ScmObj*, align 8
%k47356 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49735, i64 1)
store %struct.ScmObj* %k47356, %struct.ScmObj** %stackaddr$env-ref54397
%stackaddr$env-ref54398 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49735, i64 2)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref54398
%stackaddr$prim54399 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim54399, align 8
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%current_45args53368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53367)
store volatile %struct.ScmObj* %current_45args53368, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53368)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim54401, align 8
%ae49737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54402 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49737)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim54402, align 8
%ae49738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54403 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49738, %struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim54403, align 8
%truthy$cmp54404 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47273)
%cmp$cmp54404 = icmp eq i64 %truthy$cmp54404, 1
br i1 %cmp$cmp54404, label %truebranch$cmp54404, label %falsebranch$cmp54404
truebranch$cmp54404:
%ae49742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54405 = alloca %struct.ScmObj*, align 8
%cpsprim47359 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49742)
store volatile %struct.ScmObj* %cpsprim47359, %struct.ScmObj** %stackaddr$prim54405, align 8
%ae49744 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53370$k473560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54406 = alloca %struct.ScmObj*, align 8
%argslist53370$k473561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47359, %struct.ScmObj* %argslist53370$k473560)
store volatile %struct.ScmObj* %argslist53370$k473561, %struct.ScmObj** %stackaddr$prim54406, align 8
%stackaddr$prim54407 = alloca %struct.ScmObj*, align 8
%argslist53370$k473562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49744, %struct.ScmObj* %argslist53370$k473561)
store volatile %struct.ScmObj* %argslist53370$k473562, %struct.ScmObj** %stackaddr$prim54407, align 8
%clofunc54408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47356)
musttail call tailcc void %clofunc54408(%struct.ScmObj* %k47356, %struct.ScmObj* %argslist53370$k473562)
ret void
falsebranch$cmp54404:
%ae49755 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54409 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49755)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim54409, align 8
%stackaddr$prim54410 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim54410, align 8
%ae49758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54411 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49758, %struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim54411, align 8
%ae49761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49761)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim54412, align 8
%ae49763 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47276, %struct.ScmObj* %ae49763)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim54413, align 8
%ae49765 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49765, %struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim54414, align 8
%argslist53371$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54415 = alloca %struct.ScmObj*, align 8
%argslist53371$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53371$cc471490)
store volatile %struct.ScmObj* %argslist53371$cc471491, %struct.ScmObj** %stackaddr$prim54415, align 8
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%argslist53371$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47356, %struct.ScmObj* %argslist53371$cc471491)
store volatile %struct.ScmObj* %argslist53371$cc471492, %struct.ScmObj** %stackaddr$prim54416, align 8
%clofunc54417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc54417(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53371$cc471492)
ret void
}

define tailcc void @proc_clo$ae49720(%struct.ScmObj* %env$ae49720,%struct.ScmObj* %current_45args53373) {
%stackaddr$prim54418 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$prim54418, align 8
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%current_45args53374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53373)
store volatile %struct.ScmObj* %current_45args53374, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53374)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim54420, align 8
%argslist53376$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%argslist53376$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53376$u471500)
store volatile %struct.ScmObj* %argslist53376$u471501, %struct.ScmObj** %stackaddr$prim54421, align 8
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%argslist53376$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist53376$u471501)
store volatile %struct.ScmObj* %argslist53376$u471502, %struct.ScmObj** %stackaddr$prim54422, align 8
%clofunc54423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc54423(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist53376$u471502)
ret void
}

define tailcc void @proc_clo$ae49297(%struct.ScmObj* %env$ae49297,%struct.ScmObj* %current_45args53379) {
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %k47361, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%current_45args53380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53379)
store volatile %struct.ScmObj* %current_45args53380, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53380)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim54426, align 8
%ae49298 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49298, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim54427, align 8
%stackaddr$makeclosure54428 = alloca %struct.ScmObj*, align 8
%fptrToInt54429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49300 to i64
%ae49300 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54429)
store volatile %struct.ScmObj* %ae49300, %struct.ScmObj** %stackaddr$makeclosure54428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %k47361, i64 1)
%ae49301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54430 = alloca %struct.ScmObj*, align 8
%fptrToInt54431 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49302 to i64
%ae49302 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54431)
store volatile %struct.ScmObj* %ae49302, %struct.ScmObj** %stackaddr$makeclosure54430, align 8
%argslist53402$ae493000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%argslist53402$ae493001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49302, %struct.ScmObj* %argslist53402$ae493000)
store volatile %struct.ScmObj* %argslist53402$ae493001, %struct.ScmObj** %stackaddr$prim54432, align 8
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%argslist53402$ae493002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %argslist53402$ae493001)
store volatile %struct.ScmObj* %argslist53402$ae493002, %struct.ScmObj** %stackaddr$prim54433, align 8
%clofunc54434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49300)
musttail call tailcc void %clofunc54434(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist53402$ae493002)
ret void
}

define tailcc void @proc_clo$ae49300(%struct.ScmObj* %env$ae49300,%struct.ScmObj* %current_45args53382) {
%stackaddr$env-ref54435 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54435
%stackaddr$env-ref54436 = alloca %struct.ScmObj*, align 8
%k47361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 1)
store %struct.ScmObj* %k47361, %struct.ScmObj** %stackaddr$env-ref54436
%stackaddr$prim54437 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim54437, align 8
%stackaddr$prim54438 = alloca %struct.ScmObj*, align 8
%current_45args53383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53382)
store volatile %struct.ScmObj* %current_45args53383, %struct.ScmObj** %stackaddr$prim54438, align 8
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53383)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$makeclosure54440 = alloca %struct.ScmObj*, align 8
%fptrToInt54441 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49319 to i64
%ae49319 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54441)
store volatile %struct.ScmObj* %ae49319, %struct.ScmObj** %stackaddr$makeclosure54440, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49319, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49319, %struct.ScmObj* %k47361, i64 1)
%stackaddr$makeclosure54442 = alloca %struct.ScmObj*, align 8
%fptrToInt54443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49320 to i64
%ae49320 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54443)
store volatile %struct.ScmObj* %ae49320, %struct.ScmObj** %stackaddr$makeclosure54442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49320, %struct.ScmObj* %k47361, i64 1)
%argslist53397$anf_45bind472630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%argslist53397$anf_45bind472631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49320, %struct.ScmObj* %argslist53397$anf_45bind472630)
store volatile %struct.ScmObj* %argslist53397$anf_45bind472631, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%argslist53397$anf_45bind472632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49319, %struct.ScmObj* %argslist53397$anf_45bind472631)
store volatile %struct.ScmObj* %argslist53397$anf_45bind472632, %struct.ScmObj** %stackaddr$prim54445, align 8
%clofunc54446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47263)
musttail call tailcc void %clofunc54446(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %argslist53397$anf_45bind472632)
ret void
}

define tailcc void @proc_clo$ae49319(%struct.ScmObj* %env$ae49319,%struct.ScmObj* %current_45args53385) {
%stackaddr$env-ref54447 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49319, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54447
%stackaddr$env-ref54448 = alloca %struct.ScmObj*, align 8
%k47361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49319, i64 1)
store %struct.ScmObj* %k47361, %struct.ScmObj** %stackaddr$env-ref54448
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim54449, align 8
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%current_45args53386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53385)
store volatile %struct.ScmObj* %current_45args53386, %struct.ScmObj** %stackaddr$prim54450, align 8
%stackaddr$prim54451 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53386)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54451, align 8
%ae49435 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54452 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49435)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54452, align 8
%stackaddr$prim54453 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54453, align 8
%truthy$cmp54454 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47265)
%cmp$cmp54454 = icmp eq i64 %truthy$cmp54454, 1
br i1 %cmp$cmp54454, label %truebranch$cmp54454, label %falsebranch$cmp54454
truebranch$cmp54454:
%ae49439 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49440 = call %struct.ScmObj* @const_init_true()
%argslist53388$k473610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%argslist53388$k473611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49440, %struct.ScmObj* %argslist53388$k473610)
store volatile %struct.ScmObj* %argslist53388$k473611, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%argslist53388$k473612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49439, %struct.ScmObj* %argslist53388$k473611)
store volatile %struct.ScmObj* %argslist53388$k473612, %struct.ScmObj** %stackaddr$prim54456, align 8
%clofunc54457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47361)
musttail call tailcc void %clofunc54457(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53388$k473612)
ret void
falsebranch$cmp54454:
%ae49448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49448)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54458, align 8
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54459, align 8
%truthy$cmp54460 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47267)
%cmp$cmp54460 = icmp eq i64 %truthy$cmp54460, 1
br i1 %cmp$cmp54460, label %truebranch$cmp54460, label %falsebranch$cmp54460
truebranch$cmp54460:
%ae49452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54461 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49452)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54461, align 8
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54462, align 8
%ae49455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49455)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54463, align 8
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54464, align 8
%ae49458 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54465 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49458, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54465, align 8
%argslist53389$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%argslist53389$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53389$cc471560)
store volatile %struct.ScmObj* %argslist53389$cc471561, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%argslist53389$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53389$cc471561)
store volatile %struct.ScmObj* %argslist53389$cc471562, %struct.ScmObj** %stackaddr$prim54467, align 8
%clofunc54468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54468(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53389$cc471562)
ret void
falsebranch$cmp54460:
%ae49491 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49492 = call %struct.ScmObj* @const_init_false()
%argslist53390$k473610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%argslist53390$k473611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49492, %struct.ScmObj* %argslist53390$k473610)
store volatile %struct.ScmObj* %argslist53390$k473611, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%argslist53390$k473612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49491, %struct.ScmObj* %argslist53390$k473611)
store volatile %struct.ScmObj* %argslist53390$k473612, %struct.ScmObj** %stackaddr$prim54470, align 8
%clofunc54471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47361)
musttail call tailcc void %clofunc54471(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53390$k473612)
ret void
}

define tailcc void @proc_clo$ae49320(%struct.ScmObj* %env$ae49320,%struct.ScmObj* %current_45args53391) {
%stackaddr$env-ref54472 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref54472
%stackaddr$env-ref54473 = alloca %struct.ScmObj*, align 8
%k47361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49320, i64 1)
store %struct.ScmObj* %k47361, %struct.ScmObj** %stackaddr$env-ref54473
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim54474, align 8
%stackaddr$prim54475 = alloca %struct.ScmObj*, align 8
%current_45args53392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53391)
store volatile %struct.ScmObj* %current_45args53392, %struct.ScmObj** %stackaddr$prim54475, align 8
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53392)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim54476, align 8
%ae49322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49322)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim54477, align 8
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim54478, align 8
%truthy$cmp54479 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47265)
%cmp$cmp54479 = icmp eq i64 %truthy$cmp54479, 1
br i1 %cmp$cmp54479, label %truebranch$cmp54479, label %falsebranch$cmp54479
truebranch$cmp54479:
%ae49326 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49327 = call %struct.ScmObj* @const_init_true()
%argslist53394$k473610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%argslist53394$k473611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49327, %struct.ScmObj* %argslist53394$k473610)
store volatile %struct.ScmObj* %argslist53394$k473611, %struct.ScmObj** %stackaddr$prim54480, align 8
%stackaddr$prim54481 = alloca %struct.ScmObj*, align 8
%argslist53394$k473612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49326, %struct.ScmObj* %argslist53394$k473611)
store volatile %struct.ScmObj* %argslist53394$k473612, %struct.ScmObj** %stackaddr$prim54481, align 8
%clofunc54482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47361)
musttail call tailcc void %clofunc54482(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53394$k473612)
ret void
falsebranch$cmp54479:
%ae49335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54483 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49335)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim54483, align 8
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47266)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim54484, align 8
%truthy$cmp54485 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47267)
%cmp$cmp54485 = icmp eq i64 %truthy$cmp54485, 1
br i1 %cmp$cmp54485, label %truebranch$cmp54485, label %falsebranch$cmp54485
truebranch$cmp54485:
%ae49339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49339)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$prim54487 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim54487, align 8
%ae49342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54488 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49342)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim54488, align 8
%stackaddr$prim54489 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim54489, align 8
%ae49345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54490 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49345, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim54490, align 8
%argslist53395$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%argslist53395$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53395$cc471560)
store volatile %struct.ScmObj* %argslist53395$cc471561, %struct.ScmObj** %stackaddr$prim54491, align 8
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%argslist53395$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53395$cc471561)
store volatile %struct.ScmObj* %argslist53395$cc471562, %struct.ScmObj** %stackaddr$prim54492, align 8
%clofunc54493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc54493(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist53395$cc471562)
ret void
falsebranch$cmp54485:
%ae49378 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49379 = call %struct.ScmObj* @const_init_false()
%argslist53396$k473610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54494 = alloca %struct.ScmObj*, align 8
%argslist53396$k473611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49379, %struct.ScmObj* %argslist53396$k473610)
store volatile %struct.ScmObj* %argslist53396$k473611, %struct.ScmObj** %stackaddr$prim54494, align 8
%stackaddr$prim54495 = alloca %struct.ScmObj*, align 8
%argslist53396$k473612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49378, %struct.ScmObj* %argslist53396$k473611)
store volatile %struct.ScmObj* %argslist53396$k473612, %struct.ScmObj** %stackaddr$prim54495, align 8
%clofunc54496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47361)
musttail call tailcc void %clofunc54496(%struct.ScmObj* %k47361, %struct.ScmObj* %argslist53396$k473612)
ret void
}

define tailcc void @proc_clo$ae49302(%struct.ScmObj* %env$ae49302,%struct.ScmObj* %current_45args53398) {
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53398)
store volatile %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$prim54497, align 8
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%current_45args53399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53398)
store volatile %struct.ScmObj* %current_45args53399, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53399)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim54499, align 8
%ae49304 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53401$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54500 = alloca %struct.ScmObj*, align 8
%argslist53401$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist53401$k473640)
store volatile %struct.ScmObj* %argslist53401$k473641, %struct.ScmObj** %stackaddr$prim54500, align 8
%stackaddr$prim54501 = alloca %struct.ScmObj*, align 8
%argslist53401$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49304, %struct.ScmObj* %argslist53401$k473641)
store volatile %struct.ScmObj* %argslist53401$k473642, %struct.ScmObj** %stackaddr$prim54501, align 8
%clofunc54502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc54502(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist53401$k473642)
ret void
}

define tailcc void @proc_clo$ae49225(%struct.ScmObj* %env$ae49225,%struct.ScmObj* %current_45args53404) {
%stackaddr$env-ref54503 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49225, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54503
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53404)
store volatile %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$prim54504, align 8
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%current_45args53405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53404)
store volatile %struct.ScmObj* %current_45args53405, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim54506, align 8
%stackaddr$prim54507 = alloca %struct.ScmObj*, align 8
%current_45args53406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53405)
store volatile %struct.ScmObj* %current_45args53406, %struct.ScmObj** %stackaddr$prim54507, align 8
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53406)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim54508, align 8
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim54509, align 8
%truthy$cmp54510 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47257)
%cmp$cmp54510 = icmp eq i64 %truthy$cmp54510, 1
br i1 %cmp$cmp54510, label %truebranch$cmp54510, label %falsebranch$cmp54510
truebranch$cmp54510:
%ae49229 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53408$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%argslist53408$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53408$k473650)
store volatile %struct.ScmObj* %argslist53408$k473651, %struct.ScmObj** %stackaddr$prim54511, align 8
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%argslist53408$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49229, %struct.ScmObj* %argslist53408$k473651)
store volatile %struct.ScmObj* %argslist53408$k473652, %struct.ScmObj** %stackaddr$prim54512, align 8
%clofunc54513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54513(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53408$k473652)
ret void
falsebranch$cmp54510:
%stackaddr$prim54514 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim54514, align 8
%ae49236 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49236)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim54515, align 8
%stackaddr$prim54516 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim54516, align 8
%stackaddr$makeclosure54517 = alloca %struct.ScmObj*, align 8
%fptrToInt54518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54518)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure54517, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %k47365, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %anf_45bind47258, i64 1)
%argslist53413$anf_45bind472590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%argslist53413$anf_45bind472591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist53413$anf_45bind472590)
store volatile %struct.ScmObj* %argslist53413$anf_45bind472591, %struct.ScmObj** %stackaddr$prim54519, align 8
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%argslist53413$anf_45bind472592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %argslist53413$anf_45bind472591)
store volatile %struct.ScmObj* %argslist53413$anf_45bind472592, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%argslist53413$anf_45bind472593 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist53413$anf_45bind472592)
store volatile %struct.ScmObj* %argslist53413$anf_45bind472593, %struct.ScmObj** %stackaddr$prim54521, align 8
%clofunc54522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47259)
musttail call tailcc void %clofunc54522(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist53413$anf_45bind472593)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args53409) {
%stackaddr$env-ref54523 = alloca %struct.ScmObj*, align 8
%k47365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 0)
store %struct.ScmObj* %k47365, %struct.ScmObj** %stackaddr$env-ref54523
%stackaddr$env-ref54524 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 1)
store %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$env-ref54524
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54525, align 8
%stackaddr$prim54526 = alloca %struct.ScmObj*, align 8
%current_45args53410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53409)
store volatile %struct.ScmObj* %current_45args53410, %struct.ScmObj** %stackaddr$prim54526, align 8
%stackaddr$prim54527 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53410)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54527, align 8
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%cpsprim47367 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsprim47367, %struct.ScmObj** %stackaddr$prim54528, align 8
%ae49245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53412$k473650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54529 = alloca %struct.ScmObj*, align 8
%argslist53412$k473651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47367, %struct.ScmObj* %argslist53412$k473650)
store volatile %struct.ScmObj* %argslist53412$k473651, %struct.ScmObj** %stackaddr$prim54529, align 8
%stackaddr$prim54530 = alloca %struct.ScmObj*, align 8
%argslist53412$k473652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49245, %struct.ScmObj* %argslist53412$k473651)
store volatile %struct.ScmObj* %argslist53412$k473652, %struct.ScmObj** %stackaddr$prim54530, align 8
%clofunc54531 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47365)
musttail call tailcc void %clofunc54531(%struct.ScmObj* %k47365, %struct.ScmObj* %argslist53412$k473652)
ret void
}

define tailcc void @proc_clo$ae49199(%struct.ScmObj* %env$ae49199,%struct.ScmObj* %current_45args53415) {
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53415)
store volatile %struct.ScmObj* %k47368, %struct.ScmObj** %stackaddr$prim54532, align 8
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%current_45args53416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53415)
store volatile %struct.ScmObj* %current_45args53416, %struct.ScmObj** %stackaddr$prim54533, align 8
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53416)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim54534, align 8
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%current_45args53417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53416)
store volatile %struct.ScmObj* %current_45args53417, %struct.ScmObj** %stackaddr$prim54535, align 8
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53417)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim54537, align 8
%stackaddr$prim54538 = alloca %struct.ScmObj*, align 8
%cpsprim47369 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %cpsprim47369, %struct.ScmObj** %stackaddr$prim54538, align 8
%ae49204 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53419$k473680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%argslist53419$k473681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47369, %struct.ScmObj* %argslist53419$k473680)
store volatile %struct.ScmObj* %argslist53419$k473681, %struct.ScmObj** %stackaddr$prim54539, align 8
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%argslist53419$k473682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist53419$k473681)
store volatile %struct.ScmObj* %argslist53419$k473682, %struct.ScmObj** %stackaddr$prim54540, align 8
%clofunc54541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47368)
musttail call tailcc void %clofunc54541(%struct.ScmObj* %k47368, %struct.ScmObj* %argslist53419$k473682)
ret void
}

define tailcc void @proc_clo$ae49175(%struct.ScmObj* %env$ae49175,%struct.ScmObj* %current_45args53421) {
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %k47370, %struct.ScmObj** %stackaddr$prim54542, align 8
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%current_45args53422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53421)
store volatile %struct.ScmObj* %current_45args53422, %struct.ScmObj** %stackaddr$prim54543, align 8
%stackaddr$prim54544 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53422)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim54544, align 8
%stackaddr$prim54545 = alloca %struct.ScmObj*, align 8
%current_45args53423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53422)
store volatile %struct.ScmObj* %current_45args53423, %struct.ScmObj** %stackaddr$prim54545, align 8
%stackaddr$prim54546 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53423)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim54546, align 8
%stackaddr$prim54547 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim54547, align 8
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%cpsprim47371 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47255)
store volatile %struct.ScmObj* %cpsprim47371, %struct.ScmObj** %stackaddr$prim54548, align 8
%ae49180 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53425$k473700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54549 = alloca %struct.ScmObj*, align 8
%argslist53425$k473701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47371, %struct.ScmObj* %argslist53425$k473700)
store volatile %struct.ScmObj* %argslist53425$k473701, %struct.ScmObj** %stackaddr$prim54549, align 8
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%argslist53425$k473702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49180, %struct.ScmObj* %argslist53425$k473701)
store volatile %struct.ScmObj* %argslist53425$k473702, %struct.ScmObj** %stackaddr$prim54550, align 8
%clofunc54551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47370)
musttail call tailcc void %clofunc54551(%struct.ScmObj* %k47370, %struct.ScmObj* %argslist53425$k473702)
ret void
}

define tailcc void @proc_clo$ae48781(%struct.ScmObj* %env$ae48781,%struct.ScmObj* %current_45args53428) {
%stackaddr$env-ref54552 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54552
%stackaddr$env-ref54553 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54553
%stackaddr$env-ref54554 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48781, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54554
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %k47372, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%current_45args53429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53428)
store volatile %struct.ScmObj* %current_45args53429, %struct.ScmObj** %stackaddr$prim54556, align 8
%stackaddr$prim54557 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53429)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim54557, align 8
%ae48783 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54558 = alloca %struct.ScmObj*, align 8
%fptrToInt54559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48784 to i64
%ae48784 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54559)
store volatile %struct.ScmObj* %ae48784, %struct.ScmObj** %stackaddr$makeclosure54558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48784, %struct.ScmObj* %_37map147120, i64 3)
%argslist53486$k473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54560 = alloca %struct.ScmObj*, align 8
%argslist53486$k473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48784, %struct.ScmObj* %argslist53486$k473720)
store volatile %struct.ScmObj* %argslist53486$k473721, %struct.ScmObj** %stackaddr$prim54560, align 8
%stackaddr$prim54561 = alloca %struct.ScmObj*, align 8
%argslist53486$k473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist53486$k473721)
store volatile %struct.ScmObj* %argslist53486$k473722, %struct.ScmObj** %stackaddr$prim54561, align 8
%clofunc54562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47372)
musttail call tailcc void %clofunc54562(%struct.ScmObj* %k47372, %struct.ScmObj* %argslist53486$k473722)
ret void
}

define tailcc void @proc_clo$ae48784(%struct.ScmObj* %env$ae48784,%struct.ScmObj* %args4717347373) {
%stackaddr$env-ref54563 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54563
%stackaddr$env-ref54564 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54564
%stackaddr$env-ref54565 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54565
%stackaddr$env-ref54566 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48784, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54566
%stackaddr$prim54567 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347373)
store volatile %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$prim54567, align 8
%stackaddr$prim54568 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347373)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim54568, align 8
%stackaddr$prim54569 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim54569, align 8
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim54571, align 8
%stackaddr$prim54572 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim54572, align 8
%stackaddr$prim54573 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47244)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim54573, align 8
%stackaddr$makeclosure54574 = alloca %struct.ScmObj*, align 8
%fptrToInt54575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48792 to i64
%ae48792 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54575)
store volatile %struct.ScmObj* %ae48792, %struct.ScmObj** %stackaddr$makeclosure54574, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %k47374, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %acc47175, i64 7)
%ae48793 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54576 = alloca %struct.ScmObj*, align 8
%fptrToInt54577 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48794 to i64
%ae48794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54577)
store volatile %struct.ScmObj* %ae48794, %struct.ScmObj** %stackaddr$makeclosure54576, align 8
%argslist53485$ae487920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%argslist53485$ae487921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48794, %struct.ScmObj* %argslist53485$ae487920)
store volatile %struct.ScmObj* %argslist53485$ae487921, %struct.ScmObj** %stackaddr$prim54578, align 8
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%argslist53485$ae487922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48793, %struct.ScmObj* %argslist53485$ae487921)
store volatile %struct.ScmObj* %argslist53485$ae487922, %struct.ScmObj** %stackaddr$prim54579, align 8
%clofunc54580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48792)
musttail call tailcc void %clofunc54580(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist53485$ae487922)
ret void
}

define tailcc void @proc_clo$ae48792(%struct.ScmObj* %env$ae48792,%struct.ScmObj* %current_45args53431) {
%stackaddr$env-ref54581 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54581
%stackaddr$env-ref54582 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54582
%stackaddr$env-ref54583 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54583
%stackaddr$env-ref54584 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54584
%stackaddr$env-ref54585 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54585
%stackaddr$env-ref54586 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 5)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54586
%stackaddr$env-ref54587 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54587
%stackaddr$env-ref54588 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54588
%stackaddr$prim54589 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53431)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim54589, align 8
%stackaddr$prim54590 = alloca %struct.ScmObj*, align 8
%current_45args53432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53431)
store volatile %struct.ScmObj* %current_45args53432, %struct.ScmObj** %stackaddr$prim54590, align 8
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53432)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim54591, align 8
%stackaddr$makeclosure54592 = alloca %struct.ScmObj*, align 8
%fptrToInt54593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48824 to i64
%ae48824 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54593)
store volatile %struct.ScmObj* %ae48824, %struct.ScmObj** %stackaddr$makeclosure54592, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %k47374, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48824, %struct.ScmObj* %acc47175, i64 6)
%ae48826 = call %struct.ScmObj* @const_init_false()
%argslist53478$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54594 = alloca %struct.ScmObj*, align 8
%argslist53478$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53478$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53478$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54594, align 8
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%argslist53478$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48826, %struct.ScmObj* %argslist53478$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53478$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%argslist53478$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %argslist53478$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53478$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54596, align 8
%stackaddr$prim54597 = alloca %struct.ScmObj*, align 8
%argslist53478$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48824, %struct.ScmObj* %argslist53478$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53478$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54597, align 8
%clofunc54598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54598(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53478$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48824(%struct.ScmObj* %env$ae48824,%struct.ScmObj* %current_45args53434) {
%stackaddr$env-ref54599 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54599
%stackaddr$env-ref54600 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54600
%stackaddr$env-ref54601 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54601
%stackaddr$env-ref54602 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54602
%stackaddr$env-ref54603 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 4)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54603
%stackaddr$env-ref54604 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54604
%stackaddr$env-ref54605 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48824, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54605
%stackaddr$prim54606 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim54606, align 8
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%current_45args53435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53434)
store volatile %struct.ScmObj* %current_45args53435, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53435)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim54608, align 8
%truthy$cmp54609 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47246)
%cmp$cmp54609 = icmp eq i64 %truthy$cmp54609, 1
br i1 %cmp$cmp54609, label %truebranch$cmp54609, label %falsebranch$cmp54609
truebranch$cmp54609:
%ae48835 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53437$k473740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%argslist53437$k473741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist53437$k473740)
store volatile %struct.ScmObj* %argslist53437$k473741, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$prim54611 = alloca %struct.ScmObj*, align 8
%argslist53437$k473742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48835, %struct.ScmObj* %argslist53437$k473741)
store volatile %struct.ScmObj* %argslist53437$k473742, %struct.ScmObj** %stackaddr$prim54611, align 8
%clofunc54612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47374)
musttail call tailcc void %clofunc54612(%struct.ScmObj* %k47374, %struct.ScmObj* %argslist53437$k473742)
ret void
falsebranch$cmp54609:
%stackaddr$makeclosure54613 = alloca %struct.ScmObj*, align 8
%fptrToInt54614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48840 to i64
%ae48840 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54614)
store volatile %struct.ScmObj* %ae48840, %struct.ScmObj** %stackaddr$makeclosure54613, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %k47374, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48840, %struct.ScmObj* %acc47175, i64 6)
%ae48841 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54615 = alloca %struct.ScmObj*, align 8
%fptrToInt54616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48842 to i64
%ae48842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54616)
store volatile %struct.ScmObj* %ae48842, %struct.ScmObj** %stackaddr$makeclosure54615, align 8
%argslist53477$ae488400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54617 = alloca %struct.ScmObj*, align 8
%argslist53477$ae488401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48842, %struct.ScmObj* %argslist53477$ae488400)
store volatile %struct.ScmObj* %argslist53477$ae488401, %struct.ScmObj** %stackaddr$prim54617, align 8
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%argslist53477$ae488402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48841, %struct.ScmObj* %argslist53477$ae488401)
store volatile %struct.ScmObj* %argslist53477$ae488402, %struct.ScmObj** %stackaddr$prim54618, align 8
%clofunc54619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48840)
musttail call tailcc void %clofunc54619(%struct.ScmObj* %ae48840, %struct.ScmObj* %argslist53477$ae488402)
ret void
}

define tailcc void @proc_clo$ae48840(%struct.ScmObj* %env$ae48840,%struct.ScmObj* %current_45args53438) {
%stackaddr$env-ref54620 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54620
%stackaddr$env-ref54621 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54621
%stackaddr$env-ref54622 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54622
%stackaddr$env-ref54623 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54623
%stackaddr$env-ref54624 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 4)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54624
%stackaddr$env-ref54625 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54625
%stackaddr$env-ref54626 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48840, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54626
%stackaddr$prim54627 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53438)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim54627, align 8
%stackaddr$prim54628 = alloca %struct.ScmObj*, align 8
%current_45args53439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53438)
store volatile %struct.ScmObj* %current_45args53439, %struct.ScmObj** %stackaddr$prim54628, align 8
%stackaddr$prim54629 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53439)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim54629, align 8
%stackaddr$makeclosure54630 = alloca %struct.ScmObj*, align 8
%fptrToInt54631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48861 to i64
%ae48861 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54631)
store volatile %struct.ScmObj* %ae48861, %struct.ScmObj** %stackaddr$makeclosure54630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %k47374, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48861, %struct.ScmObj* %acc47175, i64 6)
%argslist53472$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54632 = alloca %struct.ScmObj*, align 8
%argslist53472$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53472$_37map1471200)
store volatile %struct.ScmObj* %argslist53472$_37map1471201, %struct.ScmObj** %stackaddr$prim54632, align 8
%stackaddr$prim54633 = alloca %struct.ScmObj*, align 8
%argslist53472$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist53472$_37map1471201)
store volatile %struct.ScmObj* %argslist53472$_37map1471202, %struct.ScmObj** %stackaddr$prim54633, align 8
%stackaddr$prim54634 = alloca %struct.ScmObj*, align 8
%argslist53472$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48861, %struct.ScmObj* %argslist53472$_37map1471202)
store volatile %struct.ScmObj* %argslist53472$_37map1471203, %struct.ScmObj** %stackaddr$prim54634, align 8
%clofunc54635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54635(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53472$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48861(%struct.ScmObj* %env$ae48861,%struct.ScmObj* %current_45args53441) {
%stackaddr$env-ref54636 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54636
%stackaddr$env-ref54637 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54637
%stackaddr$env-ref54638 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54638
%stackaddr$env-ref54639 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54639
%stackaddr$env-ref54640 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 4)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54640
%stackaddr$env-ref54641 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54641
%stackaddr$env-ref54642 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48861, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54642
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim54643, align 8
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%current_45args53442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53441)
store volatile %struct.ScmObj* %current_45args53442, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53442)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim54645, align 8
%stackaddr$makeclosure54646 = alloca %struct.ScmObj*, align 8
%fptrToInt54647 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48864 to i64
%ae48864 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54647)
store volatile %struct.ScmObj* %ae48864, %struct.ScmObj** %stackaddr$makeclosure54646, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %k47374, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %lsts_4347181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48864, %struct.ScmObj* %acc47175, i64 7)
%ae48865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54648 = alloca %struct.ScmObj*, align 8
%fptrToInt54649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48866 to i64
%ae48866 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54649)
store volatile %struct.ScmObj* %ae48866, %struct.ScmObj** %stackaddr$makeclosure54648, align 8
%argslist53471$ae488640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%argslist53471$ae488641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48866, %struct.ScmObj* %argslist53471$ae488640)
store volatile %struct.ScmObj* %argslist53471$ae488641, %struct.ScmObj** %stackaddr$prim54650, align 8
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%argslist53471$ae488642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48865, %struct.ScmObj* %argslist53471$ae488641)
store volatile %struct.ScmObj* %argslist53471$ae488642, %struct.ScmObj** %stackaddr$prim54651, align 8
%clofunc54652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48864)
musttail call tailcc void %clofunc54652(%struct.ScmObj* %ae48864, %struct.ScmObj* %argslist53471$ae488642)
ret void
}

define tailcc void @proc_clo$ae48864(%struct.ScmObj* %env$ae48864,%struct.ScmObj* %current_45args53444) {
%stackaddr$env-ref54653 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref54653
%stackaddr$env-ref54654 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54654
%stackaddr$env-ref54655 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54655
%stackaddr$env-ref54656 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54656
%stackaddr$env-ref54657 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 4)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54657
%stackaddr$env-ref54658 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 5)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54658
%stackaddr$env-ref54659 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54659
%stackaddr$env-ref54660 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48864, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54660
%stackaddr$prim54661 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53444)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim54661, align 8
%stackaddr$prim54662 = alloca %struct.ScmObj*, align 8
%current_45args53445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53444)
store volatile %struct.ScmObj* %current_45args53445, %struct.ScmObj** %stackaddr$prim54662, align 8
%stackaddr$prim54663 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53445)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim54663, align 8
%stackaddr$makeclosure54664 = alloca %struct.ScmObj*, align 8
%fptrToInt54665 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48885 to i64
%ae48885 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54665)
store volatile %struct.ScmObj* %ae48885, %struct.ScmObj** %stackaddr$makeclosure54664, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %k47374, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %acc47175, i64 5)
%argslist53466$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%argslist53466$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist53466$_37map1471200)
store volatile %struct.ScmObj* %argslist53466$_37map1471201, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%argslist53466$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist53466$_37map1471201)
store volatile %struct.ScmObj* %argslist53466$_37map1471202, %struct.ScmObj** %stackaddr$prim54667, align 8
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%argslist53466$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist53466$_37map1471202)
store volatile %struct.ScmObj* %argslist53466$_37map1471203, %struct.ScmObj** %stackaddr$prim54668, align 8
%clofunc54669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc54669(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53466$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48885(%struct.ScmObj* %env$ae48885,%struct.ScmObj* %current_45args53447) {
%stackaddr$env-ref54670 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54670
%stackaddr$env-ref54671 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54671
%stackaddr$env-ref54672 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 2)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54672
%stackaddr$env-ref54673 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54673
%stackaddr$env-ref54674 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54674
%stackaddr$env-ref54675 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54675
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53447)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%current_45args53448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53447)
store volatile %struct.ScmObj* %current_45args53448, %struct.ScmObj** %stackaddr$prim54677, align 8
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53448)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$makeclosure54679 = alloca %struct.ScmObj*, align 8
%fptrToInt54680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48888 to i64
%ae48888 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54680)
store volatile %struct.ScmObj* %ae48888, %struct.ScmObj** %stackaddr$makeclosure54679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %k47374, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48888, %struct.ScmObj* %acc47175, i64 6)
%ae48889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54681 = alloca %struct.ScmObj*, align 8
%fptrToInt54682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48890 to i64
%ae48890 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54682)
store volatile %struct.ScmObj* %ae48890, %struct.ScmObj** %stackaddr$makeclosure54681, align 8
%argslist53465$ae488880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%argslist53465$ae488881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist53465$ae488880)
store volatile %struct.ScmObj* %argslist53465$ae488881, %struct.ScmObj** %stackaddr$prim54683, align 8
%stackaddr$prim54684 = alloca %struct.ScmObj*, align 8
%argslist53465$ae488882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist53465$ae488881)
store volatile %struct.ScmObj* %argslist53465$ae488882, %struct.ScmObj** %stackaddr$prim54684, align 8
%clofunc54685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48888)
musttail call tailcc void %clofunc54685(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist53465$ae488882)
ret void
}

define tailcc void @proc_clo$ae48888(%struct.ScmObj* %env$ae48888,%struct.ScmObj* %current_45args53450) {
%stackaddr$env-ref54686 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54686
%stackaddr$env-ref54687 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54687
%stackaddr$env-ref54688 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 2)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54688
%stackaddr$env-ref54689 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54689
%stackaddr$env-ref54690 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref54690
%stackaddr$env-ref54691 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54691
%stackaddr$env-ref54692 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48888, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref54692
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53450)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%current_45args53451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53450)
store volatile %struct.ScmObj* %current_45args53451, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53451)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim54695, align 8
%ae48911 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48911)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim54696, align 8
%stackaddr$makeclosure54697 = alloca %struct.ScmObj*, align 8
%fptrToInt54698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48913 to i64
%ae48913 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54698)
store volatile %struct.ScmObj* %ae48913, %struct.ScmObj** %stackaddr$makeclosure54697, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %k47374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48913, %struct.ScmObj* %f47176, i64 3)
%argslist53459$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%argslist53459$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist53459$_37foldr470940)
store volatile %struct.ScmObj* %argslist53459$_37foldr470941, %struct.ScmObj** %stackaddr$prim54699, align 8
%stackaddr$prim54700 = alloca %struct.ScmObj*, align 8
%argslist53459$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47250, %struct.ScmObj* %argslist53459$_37foldr470941)
store volatile %struct.ScmObj* %argslist53459$_37foldr470942, %struct.ScmObj** %stackaddr$prim54700, align 8
%stackaddr$prim54701 = alloca %struct.ScmObj*, align 8
%argslist53459$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist53459$_37foldr470942)
store volatile %struct.ScmObj* %argslist53459$_37foldr470943, %struct.ScmObj** %stackaddr$prim54701, align 8
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%argslist53459$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48913, %struct.ScmObj* %argslist53459$_37foldr470943)
store volatile %struct.ScmObj* %argslist53459$_37foldr470944, %struct.ScmObj** %stackaddr$prim54702, align 8
%clofunc54703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54703(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist53459$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48913(%struct.ScmObj* %env$ae48913,%struct.ScmObj* %current_45args53453) {
%stackaddr$env-ref54704 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54704
%stackaddr$env-ref54705 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 1)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54705
%stackaddr$env-ref54706 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54706
%stackaddr$env-ref54707 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48913, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54707
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53453)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim54708, align 8
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%current_45args53454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53453)
store volatile %struct.ScmObj* %current_45args53454, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53454)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim54710, align 8
%stackaddr$makeclosure54711 = alloca %struct.ScmObj*, align 8
%fptrToInt54712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48917 to i64
%ae48917 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54712)
store volatile %struct.ScmObj* %ae48917, %struct.ScmObj** %stackaddr$makeclosure54711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48917, %struct.ScmObj* %_37foldl47172, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48917, %struct.ScmObj* %k47374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48917, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48917, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%cpsargs47385 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48917, %struct.ScmObj* %anf_45bind47251)
store volatile %struct.ScmObj* %cpsargs47385, %struct.ScmObj** %stackaddr$prim54713, align 8
%clofunc54714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc54714(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47385)
ret void
}

define tailcc void @proc_clo$ae48917(%struct.ScmObj* %env$ae48917,%struct.ScmObj* %current_45args53456) {
%stackaddr$env-ref54715 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48917, i64 0)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref54715
%stackaddr$env-ref54716 = alloca %struct.ScmObj*, align 8
%k47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48917, i64 1)
store %struct.ScmObj* %k47374, %struct.ScmObj** %stackaddr$env-ref54716
%stackaddr$env-ref54717 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48917, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref54717
%stackaddr$env-ref54718 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48917, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref54718
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53456)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%current_45args53457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53456)
store volatile %struct.ScmObj* %current_45args53457, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53457)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim54721, align 8
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim54722, align 8
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47252)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%cpsargs47384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47374, %struct.ScmObj* %anf_45bind47253)
store volatile %struct.ScmObj* %cpsargs47384, %struct.ScmObj** %stackaddr$prim54724, align 8
%clofunc54725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc54725(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47384)
ret void
}

define tailcc void @proc_clo$ae48890(%struct.ScmObj* %env$ae48890,%struct.ScmObj* %current_45args53460) {
%stackaddr$prim54726 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53460)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim54726, align 8
%stackaddr$prim54727 = alloca %struct.ScmObj*, align 8
%current_45args53461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53460)
store volatile %struct.ScmObj* %current_45args53461, %struct.ScmObj** %stackaddr$prim54727, align 8
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53461)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim54728, align 8
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%current_45args53462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53461)
store volatile %struct.ScmObj* %current_45args53462, %struct.ScmObj** %stackaddr$prim54729, align 8
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53462)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%cpsprim47387 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47387, %struct.ScmObj** %stackaddr$prim54731, align 8
%ae48894 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53464$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%argslist53464$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47387, %struct.ScmObj* %argslist53464$k473860)
store volatile %struct.ScmObj* %argslist53464$k473861, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%argslist53464$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48894, %struct.ScmObj* %argslist53464$k473861)
store volatile %struct.ScmObj* %argslist53464$k473862, %struct.ScmObj** %stackaddr$prim54733, align 8
%clofunc54734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc54734(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53464$k473862)
ret void
}

define tailcc void @proc_clo$ae48866(%struct.ScmObj* %env$ae48866,%struct.ScmObj* %current_45args53467) {
%stackaddr$prim54735 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$prim54735, align 8
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%current_45args53468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53467)
store volatile %struct.ScmObj* %current_45args53468, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53468)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim54737, align 8
%stackaddr$prim54738 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim54738, align 8
%ae48869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53470$k473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%argslist53470$k473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist53470$k473880)
store volatile %struct.ScmObj* %argslist53470$k473881, %struct.ScmObj** %stackaddr$prim54739, align 8
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%argslist53470$k473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48869, %struct.ScmObj* %argslist53470$k473881)
store volatile %struct.ScmObj* %argslist53470$k473882, %struct.ScmObj** %stackaddr$prim54740, align 8
%clofunc54741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47388)
musttail call tailcc void %clofunc54741(%struct.ScmObj* %k47388, %struct.ScmObj* %argslist53470$k473882)
ret void
}

define tailcc void @proc_clo$ae48842(%struct.ScmObj* %env$ae48842,%struct.ScmObj* %current_45args53473) {
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53473)
store volatile %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$prim54742, align 8
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%current_45args53474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53473)
store volatile %struct.ScmObj* %current_45args53474, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53474)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim54744, align 8
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%cpsprim47391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47391, %struct.ScmObj** %stackaddr$prim54745, align 8
%ae48845 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53476$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54746 = alloca %struct.ScmObj*, align 8
%argslist53476$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47391, %struct.ScmObj* %argslist53476$k473900)
store volatile %struct.ScmObj* %argslist53476$k473901, %struct.ScmObj** %stackaddr$prim54746, align 8
%stackaddr$prim54747 = alloca %struct.ScmObj*, align 8
%argslist53476$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48845, %struct.ScmObj* %argslist53476$k473901)
store volatile %struct.ScmObj* %argslist53476$k473902, %struct.ScmObj** %stackaddr$prim54747, align 8
%clofunc54748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc54748(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53476$k473902)
ret void
}

define tailcc void @proc_clo$ae48794(%struct.ScmObj* %env$ae48794,%struct.ScmObj* %current_45args53479) {
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53479)
store volatile %struct.ScmObj* %k47392, %struct.ScmObj** %stackaddr$prim54749, align 8
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%current_45args53480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53479)
store volatile %struct.ScmObj* %current_45args53480, %struct.ScmObj** %stackaddr$prim54750, align 8
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53480)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim54751, align 8
%stackaddr$prim54752 = alloca %struct.ScmObj*, align 8
%current_45args53481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53480)
store volatile %struct.ScmObj* %current_45args53481, %struct.ScmObj** %stackaddr$prim54752, align 8
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53481)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim54753, align 8
%truthy$cmp54754 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp54754 = icmp eq i64 %truthy$cmp54754, 1
br i1 %cmp$cmp54754, label %truebranch$cmp54754, label %falsebranch$cmp54754
truebranch$cmp54754:
%ae48797 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53483$k473920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%argslist53483$k473921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist53483$k473920)
store volatile %struct.ScmObj* %argslist53483$k473921, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$prim54756 = alloca %struct.ScmObj*, align 8
%argslist53483$k473922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48797, %struct.ScmObj* %argslist53483$k473921)
store volatile %struct.ScmObj* %argslist53483$k473922, %struct.ScmObj** %stackaddr$prim54756, align 8
%clofunc54757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47392)
musttail call tailcc void %clofunc54757(%struct.ScmObj* %k47392, %struct.ScmObj* %argslist53483$k473922)
ret void
falsebranch$cmp54754:
%stackaddr$prim54758 = alloca %struct.ScmObj*, align 8
%cpsprim47393 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47393, %struct.ScmObj** %stackaddr$prim54758, align 8
%ae48804 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53484$k473920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54759 = alloca %struct.ScmObj*, align 8
%argslist53484$k473921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47393, %struct.ScmObj* %argslist53484$k473920)
store volatile %struct.ScmObj* %argslist53484$k473921, %struct.ScmObj** %stackaddr$prim54759, align 8
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%argslist53484$k473922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48804, %struct.ScmObj* %argslist53484$k473921)
store volatile %struct.ScmObj* %argslist53484$k473922, %struct.ScmObj** %stackaddr$prim54760, align 8
%clofunc54761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47392)
musttail call tailcc void %clofunc54761(%struct.ScmObj* %k47392, %struct.ScmObj* %argslist53484$k473922)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %args4711647394) {
%stackaddr$env-ref54762 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54762
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$env-ref54764 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54764
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647394)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647394)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim54766, align 8
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$prim54768 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim54768, align 8
%stackaddr$makeclosure54769 = alloca %struct.ScmObj*, align 8
%fptrToInt54770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48640 to i64
%ae48640 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54770)
store volatile %struct.ScmObj* %ae48640, %struct.ScmObj** %stackaddr$makeclosure54769, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48640, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48640, %struct.ScmObj* %k47395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48640, %struct.ScmObj* %lsts47117, i64 2)
%ae48641 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54771 = alloca %struct.ScmObj*, align 8
%fptrToInt54772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48642 to i64
%ae48642 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54772)
store volatile %struct.ScmObj* %ae48642, %struct.ScmObj** %stackaddr$makeclosure54771, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %_37last47111, i64 2)
%argslist53503$ae486400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%argslist53503$ae486401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48642, %struct.ScmObj* %argslist53503$ae486400)
store volatile %struct.ScmObj* %argslist53503$ae486401, %struct.ScmObj** %stackaddr$prim54773, align 8
%stackaddr$prim54774 = alloca %struct.ScmObj*, align 8
%argslist53503$ae486402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48641, %struct.ScmObj* %argslist53503$ae486401)
store volatile %struct.ScmObj* %argslist53503$ae486402, %struct.ScmObj** %stackaddr$prim54774, align 8
%clofunc54775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48640)
musttail call tailcc void %clofunc54775(%struct.ScmObj* %ae48640, %struct.ScmObj* %argslist53503$ae486402)
ret void
}

define tailcc void @proc_clo$ae48640(%struct.ScmObj* %env$ae48640,%struct.ScmObj* %current_45args53488) {
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48640, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$env-ref54777 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48640, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref54777
%stackaddr$env-ref54778 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48640, i64 2)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref54778
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53488)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$prim54780 = alloca %struct.ScmObj*, align 8
%current_45args53489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53488)
store volatile %struct.ScmObj* %current_45args53489, %struct.ScmObj** %stackaddr$prim54780, align 8
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53489)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim54781, align 8
%ae48703 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48703, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim54782, align 8
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %anf_45bind47241)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%cpsargs47397 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47395, %struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %cpsargs47397, %struct.ScmObj** %stackaddr$prim54784, align 8
%clofunc54785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc54785(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47397)
ret void
}

define tailcc void @proc_clo$ae48642(%struct.ScmObj* %env$ae48642,%struct.ScmObj* %fargs4711947398) {
%stackaddr$env-ref54786 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54786
%stackaddr$env-ref54787 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54787
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$prim54789 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947398)
store volatile %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$prim54789, align 8
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947398)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$makeclosure54791 = alloca %struct.ScmObj*, align 8
%fptrToInt54792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48646 to i64
%ae48646 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54792)
store volatile %struct.ScmObj* %ae48646, %struct.ScmObj** %stackaddr$makeclosure54791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48646, %struct.ScmObj* %k47399, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48646, %struct.ScmObj* %_37last47111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48646, %struct.ScmObj* %fargs47119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48646, %struct.ScmObj* %f47118, i64 3)
%ae48648 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53502$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%argslist53502$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48648, %struct.ScmObj* %argslist53502$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist53502$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim54793, align 8
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%argslist53502$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53502$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist53502$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%argslist53502$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48646, %struct.ScmObj* %argslist53502$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist53502$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim54795, align 8
%clofunc54796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc54796(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist53502$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48646(%struct.ScmObj* %env$ae48646,%struct.ScmObj* %current_45args53491) {
%stackaddr$env-ref54797 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48646, i64 0)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref54797
%stackaddr$env-ref54798 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48646, i64 1)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54798
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48646, i64 2)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$env-ref54800 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48646, i64 3)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref54800
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53491)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%current_45args53492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53491)
store volatile %struct.ScmObj* %current_45args53492, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53492)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim54803, align 8
%stackaddr$makeclosure54804 = alloca %struct.ScmObj*, align 8
%fptrToInt54805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48653 to i64
%ae48653 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54805)
store volatile %struct.ScmObj* %ae48653, %struct.ScmObj** %stackaddr$makeclosure54804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %k47399, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %_37last47111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48653, %struct.ScmObj* %fargs47119, i64 2)
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%cpsargs47404 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48653, %struct.ScmObj* %anf_45bind47237)
store volatile %struct.ScmObj* %cpsargs47404, %struct.ScmObj** %stackaddr$prim54806, align 8
%clofunc54807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc54807(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47404)
ret void
}

define tailcc void @proc_clo$ae48653(%struct.ScmObj* %env$ae48653,%struct.ScmObj* %current_45args53494) {
%stackaddr$env-ref54808 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 0)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref54808
%stackaddr$env-ref54809 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 1)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54809
%stackaddr$env-ref54810 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48653, i64 2)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref54810
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53494)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim54811, align 8
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%current_45args53495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53494)
store volatile %struct.ScmObj* %current_45args53495, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53495)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim54813, align 8
%stackaddr$makeclosure54814 = alloca %struct.ScmObj*, align 8
%fptrToInt54815 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48658 to i64
%ae48658 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54815)
store volatile %struct.ScmObj* %ae48658, %struct.ScmObj** %stackaddr$makeclosure54814, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48658, %struct.ScmObj* %anf_45bind47238, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48658, %struct.ScmObj* %k47399, i64 1)
%argslist53501$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%argslist53501$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist53501$_37last471110)
store volatile %struct.ScmObj* %argslist53501$_37last471111, %struct.ScmObj** %stackaddr$prim54816, align 8
%stackaddr$prim54817 = alloca %struct.ScmObj*, align 8
%argslist53501$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48658, %struct.ScmObj* %argslist53501$_37last471111)
store volatile %struct.ScmObj* %argslist53501$_37last471112, %struct.ScmObj** %stackaddr$prim54817, align 8
%clofunc54818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc54818(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist53501$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48658(%struct.ScmObj* %env$ae48658,%struct.ScmObj* %current_45args53497) {
%stackaddr$env-ref54819 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48658, i64 0)
store %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$env-ref54819
%stackaddr$env-ref54820 = alloca %struct.ScmObj*, align 8
%k47399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48658, i64 1)
store %struct.ScmObj* %k47399, %struct.ScmObj** %stackaddr$env-ref54820
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53497)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%current_45args53498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53497)
store volatile %struct.ScmObj* %current_45args53498, %struct.ScmObj** %stackaddr$prim54822, align 8
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53498)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%cpsprim47403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %anf_45bind47239)
store volatile %struct.ScmObj* %cpsprim47403, %struct.ScmObj** %stackaddr$prim54824, align 8
%ae48663 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53500$k473990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54825 = alloca %struct.ScmObj*, align 8
%argslist53500$k473991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47403, %struct.ScmObj* %argslist53500$k473990)
store volatile %struct.ScmObj* %argslist53500$k473991, %struct.ScmObj** %stackaddr$prim54825, align 8
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%argslist53500$k473992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48663, %struct.ScmObj* %argslist53500$k473991)
store volatile %struct.ScmObj* %argslist53500$k473992, %struct.ScmObj** %stackaddr$prim54826, align 8
%clofunc54827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47399)
musttail call tailcc void %clofunc54827(%struct.ScmObj* %k47399, %struct.ScmObj* %argslist53500$k473992)
ret void
}

define tailcc void @proc_clo$ae48558(%struct.ScmObj* %env$ae48558,%struct.ScmObj* %current_45args53505) {
%stackaddr$env-ref54828 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48558, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54828
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%current_45args53506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53505)
store volatile %struct.ScmObj* %current_45args53506, %struct.ScmObj** %stackaddr$prim54830, align 8
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53506)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%current_45args53507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53506)
store volatile %struct.ScmObj* %current_45args53507, %struct.ScmObj** %stackaddr$prim54832, align 8
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53507)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim54833, align 8
%stackaddr$makeclosure54834 = alloca %struct.ScmObj*, align 8
%fptrToInt54835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48559 to i64
%ae48559 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54835)
store volatile %struct.ScmObj* %ae48559, %struct.ScmObj** %stackaddr$makeclosure54834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48559, %struct.ScmObj* %k47405, i64 2)
%ae48560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54836 = alloca %struct.ScmObj*, align 8
%fptrToInt54837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48561 to i64
%ae48561 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54837)
store volatile %struct.ScmObj* %ae48561, %struct.ScmObj** %stackaddr$makeclosure54836, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48561, %struct.ScmObj* %f47122, i64 0)
%argslist53522$ae485590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%argslist53522$ae485591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48561, %struct.ScmObj* %argslist53522$ae485590)
store volatile %struct.ScmObj* %argslist53522$ae485591, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%argslist53522$ae485592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48560, %struct.ScmObj* %argslist53522$ae485591)
store volatile %struct.ScmObj* %argslist53522$ae485592, %struct.ScmObj** %stackaddr$prim54839, align 8
%clofunc54840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48559)
musttail call tailcc void %clofunc54840(%struct.ScmObj* %ae48559, %struct.ScmObj* %argslist53522$ae485592)
ret void
}

define tailcc void @proc_clo$ae48559(%struct.ScmObj* %env$ae48559,%struct.ScmObj* %current_45args53509) {
%stackaddr$env-ref54841 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref54841
%stackaddr$env-ref54842 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54842
%stackaddr$env-ref54843 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48559, i64 2)
store %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$env-ref54843
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53509)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim54844, align 8
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%current_45args53510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53509)
store volatile %struct.ScmObj* %current_45args53510, %struct.ScmObj** %stackaddr$prim54845, align 8
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53510)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim54846, align 8
%ae48593 = call %struct.ScmObj* @const_init_null()
%argslist53512$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%argslist53512$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist53512$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53512$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%argslist53512$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48593, %struct.ScmObj* %argslist53512$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53512$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54848, align 8
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%argslist53512$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist53512$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53512$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%argslist53512$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist53512$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53512$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54850, align 8
%clofunc54851 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54851(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53512$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48561(%struct.ScmObj* %env$ae48561,%struct.ScmObj* %current_45args53513) {
%stackaddr$env-ref54852 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48561, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref54852
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53513)
store volatile %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%current_45args53514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53513)
store volatile %struct.ScmObj* %current_45args53514, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$prim54856 = alloca %struct.ScmObj*, align 8
%current_45args53515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53514)
store volatile %struct.ScmObj* %current_45args53515, %struct.ScmObj** %stackaddr$prim54856, align 8
%stackaddr$prim54857 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53515)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim54857, align 8
%stackaddr$makeclosure54858 = alloca %struct.ScmObj*, align 8
%fptrToInt54859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48563 to i64
%ae48563 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54859)
store volatile %struct.ScmObj* %ae48563, %struct.ScmObj** %stackaddr$makeclosure54858, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48563, %struct.ScmObj* %r47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48563, %struct.ScmObj* %k47407, i64 1)
%argslist53521$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%argslist53521$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist53521$f471220)
store volatile %struct.ScmObj* %argslist53521$f471221, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%argslist53521$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48563, %struct.ScmObj* %argslist53521$f471221)
store volatile %struct.ScmObj* %argslist53521$f471222, %struct.ScmObj** %stackaddr$prim54861, align 8
%clofunc54862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc54862(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist53521$f471222)
ret void
}

define tailcc void @proc_clo$ae48563(%struct.ScmObj* %env$ae48563,%struct.ScmObj* %current_45args53517) {
%stackaddr$env-ref54863 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48563, i64 0)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref54863
%stackaddr$env-ref54864 = alloca %struct.ScmObj*, align 8
%k47407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48563, i64 1)
store %struct.ScmObj* %k47407, %struct.ScmObj** %stackaddr$env-ref54864
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%_95k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53517)
store volatile %struct.ScmObj* %_95k47408, %struct.ScmObj** %stackaddr$prim54865, align 8
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%current_45args53518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53517)
store volatile %struct.ScmObj* %current_45args53518, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53518)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim54867, align 8
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%cpsprim47409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47409, %struct.ScmObj** %stackaddr$prim54868, align 8
%ae48568 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53520$k474070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%argslist53520$k474071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47409, %struct.ScmObj* %argslist53520$k474070)
store volatile %struct.ScmObj* %argslist53520$k474071, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%argslist53520$k474072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48568, %struct.ScmObj* %argslist53520$k474071)
store volatile %struct.ScmObj* %argslist53520$k474072, %struct.ScmObj** %stackaddr$prim54870, align 8
%clofunc54871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47407)
musttail call tailcc void %clofunc54871(%struct.ScmObj* %k47407, %struct.ScmObj* %argslist53520$k474072)
ret void
}

define tailcc void @proc_clo$ae48172(%struct.ScmObj* %env$ae48172,%struct.ScmObj* %current_45args53525) {
%stackaddr$env-ref54872 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48172, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54872
%stackaddr$env-ref54873 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48172, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54873
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$prim54874, align 8
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%current_45args53526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53525)
store volatile %struct.ScmObj* %current_45args53526, %struct.ScmObj** %stackaddr$prim54875, align 8
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53526)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim54876, align 8
%ae48174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54877 = alloca %struct.ScmObj*, align 8
%fptrToInt54878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48175 to i64
%ae48175 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54878)
store volatile %struct.ScmObj* %ae48175, %struct.ScmObj** %stackaddr$makeclosure54877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48175, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48175, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48175, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist53583$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%argslist53583$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48175, %struct.ScmObj* %argslist53583$k474100)
store volatile %struct.ScmObj* %argslist53583$k474101, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%argslist53583$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48174, %struct.ScmObj* %argslist53583$k474101)
store volatile %struct.ScmObj* %argslist53583$k474102, %struct.ScmObj** %stackaddr$prim54880, align 8
%clofunc54881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc54881(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist53583$k474102)
ret void
}

define tailcc void @proc_clo$ae48175(%struct.ScmObj* %env$ae48175,%struct.ScmObj* %args4709647411) {
%stackaddr$env-ref54882 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48175, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54882
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48175, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48175, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647411)
store volatile %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$prim54885, align 8
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647411)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47222)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$prim54891 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47223)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim54891, align 8
%stackaddr$makeclosure54892 = alloca %struct.ScmObj*, align 8
%fptrToInt54893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48183 to i64
%ae48183 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54893)
store volatile %struct.ScmObj* %ae48183, %struct.ScmObj** %stackaddr$makeclosure54892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48183, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54894 = alloca %struct.ScmObj*, align 8
%fptrToInt54895 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48185 to i64
%ae48185 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54895)
store volatile %struct.ScmObj* %ae48185, %struct.ScmObj** %stackaddr$makeclosure54894, align 8
%argslist53582$ae481830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%argslist53582$ae481831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48185, %struct.ScmObj* %argslist53582$ae481830)
store volatile %struct.ScmObj* %argslist53582$ae481831, %struct.ScmObj** %stackaddr$prim54896, align 8
%stackaddr$prim54897 = alloca %struct.ScmObj*, align 8
%argslist53582$ae481832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48184, %struct.ScmObj* %argslist53582$ae481831)
store volatile %struct.ScmObj* %argslist53582$ae481832, %struct.ScmObj** %stackaddr$prim54897, align 8
%clofunc54898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48183)
musttail call tailcc void %clofunc54898(%struct.ScmObj* %ae48183, %struct.ScmObj* %argslist53582$ae481832)
ret void
}

define tailcc void @proc_clo$ae48183(%struct.ScmObj* %env$ae48183,%struct.ScmObj* %current_45args53528) {
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$env-ref54905 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48183, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54905
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53528)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%current_45args53529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53528)
store volatile %struct.ScmObj* %current_45args53529, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53529)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48215 to i64
%ae48215 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae48215, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48215, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48217 = call %struct.ScmObj* @const_init_false()
%argslist53575$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%argslist53575$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53575$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53575$_37foldr1470891, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%argslist53575$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48217, %struct.ScmObj* %argslist53575$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53575$_37foldr1470892, %struct.ScmObj** %stackaddr$prim54912, align 8
%stackaddr$prim54913 = alloca %struct.ScmObj*, align 8
%argslist53575$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist53575$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53575$_37foldr1470893, %struct.ScmObj** %stackaddr$prim54913, align 8
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%argslist53575$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48215, %struct.ScmObj* %argslist53575$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53575$_37foldr1470894, %struct.ScmObj** %stackaddr$prim54914, align 8
%clofunc54915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc54915(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53575$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48215(%struct.ScmObj* %env$ae48215,%struct.ScmObj* %current_45args53531) {
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$env-ref54921 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54921
%stackaddr$env-ref54922 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48215, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54922
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%_95k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %_95k47414, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%current_45args53532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53531)
store volatile %struct.ScmObj* %current_45args53532, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53532)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim54925, align 8
%truthy$cmp54926 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47225)
%cmp$cmp54926 = icmp eq i64 %truthy$cmp54926, 1
br i1 %cmp$cmp54926, label %truebranch$cmp54926, label %falsebranch$cmp54926
truebranch$cmp54926:
%ae48226 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53534$k474120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%argslist53534$k474121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist53534$k474120)
store volatile %struct.ScmObj* %argslist53534$k474121, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist53534$k474122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48226, %struct.ScmObj* %argslist53534$k474121)
store volatile %struct.ScmObj* %argslist53534$k474122, %struct.ScmObj** %stackaddr$prim54928, align 8
%clofunc54929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47412)
musttail call tailcc void %clofunc54929(%struct.ScmObj* %k47412, %struct.ScmObj* %argslist53534$k474122)
ret void
falsebranch$cmp54926:
%stackaddr$makeclosure54930 = alloca %struct.ScmObj*, align 8
%fptrToInt54931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48231 to i64
%ae48231 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54931)
store volatile %struct.ScmObj* %ae48231, %struct.ScmObj** %stackaddr$makeclosure54930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48231, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48232 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54932 = alloca %struct.ScmObj*, align 8
%fptrToInt54933 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48233 to i64
%ae48233 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54933)
store volatile %struct.ScmObj* %ae48233, %struct.ScmObj** %stackaddr$makeclosure54932, align 8
%argslist53574$ae482310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%argslist53574$ae482311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48233, %struct.ScmObj* %argslist53574$ae482310)
store volatile %struct.ScmObj* %argslist53574$ae482311, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%argslist53574$ae482312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48232, %struct.ScmObj* %argslist53574$ae482311)
store volatile %struct.ScmObj* %argslist53574$ae482312, %struct.ScmObj** %stackaddr$prim54935, align 8
%clofunc54936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48231)
musttail call tailcc void %clofunc54936(%struct.ScmObj* %ae48231, %struct.ScmObj* %argslist53574$ae482312)
ret void
}

define tailcc void @proc_clo$ae48231(%struct.ScmObj* %env$ae48231,%struct.ScmObj* %current_45args53535) {
%stackaddr$env-ref54937 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54937
%stackaddr$env-ref54938 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54938
%stackaddr$env-ref54939 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54939
%stackaddr$env-ref54940 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54940
%stackaddr$env-ref54941 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54941
%stackaddr$env-ref54942 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54942
%stackaddr$env-ref54943 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48231, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54943
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53535)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%current_45args53536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53535)
store volatile %struct.ScmObj* %current_45args53536, %struct.ScmObj** %stackaddr$prim54945, align 8
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53536)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$makeclosure54947 = alloca %struct.ScmObj*, align 8
%fptrToInt54948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48252 to i64
%ae48252 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54948)
store volatile %struct.ScmObj* %ae48252, %struct.ScmObj** %stackaddr$makeclosure54947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist53569$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%argslist53569$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53569$_37map1470850)
store volatile %struct.ScmObj* %argslist53569$_37map1470851, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%argslist53569$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist53569$_37map1470851)
store volatile %struct.ScmObj* %argslist53569$_37map1470852, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%argslist53569$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48252, %struct.ScmObj* %argslist53569$_37map1470852)
store volatile %struct.ScmObj* %argslist53569$_37map1470853, %struct.ScmObj** %stackaddr$prim54951, align 8
%clofunc54952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc54952(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53569$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48252(%struct.ScmObj* %env$ae48252,%struct.ScmObj* %current_45args53538) {
%stackaddr$env-ref54953 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54953
%stackaddr$env-ref54954 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54954
%stackaddr$env-ref54955 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54955
%stackaddr$env-ref54956 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54956
%stackaddr$env-ref54957 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54957
%stackaddr$env-ref54958 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54958
%stackaddr$env-ref54959 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54959
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53538)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim54960, align 8
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%current_45args53539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53538)
store volatile %struct.ScmObj* %current_45args53539, %struct.ScmObj** %stackaddr$prim54961, align 8
%stackaddr$prim54962 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53539)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim54962, align 8
%stackaddr$makeclosure54963 = alloca %struct.ScmObj*, align 8
%fptrToInt54964 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48255 to i64
%ae48255 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54964)
store volatile %struct.ScmObj* %ae48255, %struct.ScmObj** %stackaddr$makeclosure54963, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %lsts47097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48255, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54965 = alloca %struct.ScmObj*, align 8
%fptrToInt54966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48257 to i64
%ae48257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54966)
store volatile %struct.ScmObj* %ae48257, %struct.ScmObj** %stackaddr$makeclosure54965, align 8
%argslist53568$ae482550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%argslist53568$ae482551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48257, %struct.ScmObj* %argslist53568$ae482550)
store volatile %struct.ScmObj* %argslist53568$ae482551, %struct.ScmObj** %stackaddr$prim54967, align 8
%stackaddr$prim54968 = alloca %struct.ScmObj*, align 8
%argslist53568$ae482552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48256, %struct.ScmObj* %argslist53568$ae482551)
store volatile %struct.ScmObj* %argslist53568$ae482552, %struct.ScmObj** %stackaddr$prim54968, align 8
%clofunc54969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48255)
musttail call tailcc void %clofunc54969(%struct.ScmObj* %ae48255, %struct.ScmObj* %argslist53568$ae482552)
ret void
}

define tailcc void @proc_clo$ae48255(%struct.ScmObj* %env$ae48255,%struct.ScmObj* %current_45args53541) {
%stackaddr$env-ref54970 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54970
%stackaddr$env-ref54971 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54971
%stackaddr$env-ref54972 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref54972
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$env-ref54974 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54974
%stackaddr$env-ref54975 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54975
%stackaddr$env-ref54976 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 6)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref54976
%stackaddr$env-ref54977 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48255, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54977
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53541)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim54978, align 8
%stackaddr$prim54979 = alloca %struct.ScmObj*, align 8
%current_45args53542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53541)
store volatile %struct.ScmObj* %current_45args53542, %struct.ScmObj** %stackaddr$prim54979, align 8
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53542)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$makeclosure54981 = alloca %struct.ScmObj*, align 8
%fptrToInt54982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48276 to i64
%ae48276 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54982)
store volatile %struct.ScmObj* %ae48276, %struct.ScmObj** %stackaddr$makeclosure54981, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48276, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist53563$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%argslist53563$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist53563$_37map1470850)
store volatile %struct.ScmObj* %argslist53563$_37map1470851, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%argslist53563$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist53563$_37map1470851)
store volatile %struct.ScmObj* %argslist53563$_37map1470852, %struct.ScmObj** %stackaddr$prim54984, align 8
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%argslist53563$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48276, %struct.ScmObj* %argslist53563$_37map1470852)
store volatile %struct.ScmObj* %argslist53563$_37map1470853, %struct.ScmObj** %stackaddr$prim54985, align 8
%clofunc54986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc54986(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist53563$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48276(%struct.ScmObj* %env$ae48276,%struct.ScmObj* %current_45args53544) {
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$env-ref54988 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54988
%stackaddr$env-ref54989 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref54989
%stackaddr$env-ref54990 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref54990
%stackaddr$env-ref54991 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref54991
%stackaddr$env-ref54992 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48276, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref54992
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53544)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%current_45args53545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53544)
store volatile %struct.ScmObj* %current_45args53545, %struct.ScmObj** %stackaddr$prim54994, align 8
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53545)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$makeclosure54996 = alloca %struct.ScmObj*, align 8
%fptrToInt54997 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48279 to i64
%ae48279 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54997)
store volatile %struct.ScmObj* %ae48279, %struct.ScmObj** %stackaddr$makeclosure54996, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48279, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54998 = alloca %struct.ScmObj*, align 8
%fptrToInt54999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48281 to i64
%ae48281 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54999)
store volatile %struct.ScmObj* %ae48281, %struct.ScmObj** %stackaddr$makeclosure54998, align 8
%argslist53562$ae482790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%argslist53562$ae482791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48281, %struct.ScmObj* %argslist53562$ae482790)
store volatile %struct.ScmObj* %argslist53562$ae482791, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%argslist53562$ae482792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48280, %struct.ScmObj* %argslist53562$ae482791)
store volatile %struct.ScmObj* %argslist53562$ae482792, %struct.ScmObj** %stackaddr$prim55001, align 8
%clofunc55002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48279)
musttail call tailcc void %clofunc55002(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist53562$ae482792)
ret void
}

define tailcc void @proc_clo$ae48279(%struct.ScmObj* %env$ae48279,%struct.ScmObj* %current_45args53547) {
%stackaddr$env-ref55003 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref55003
%stackaddr$env-ref55004 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55004
%stackaddr$env-ref55005 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55005
%stackaddr$env-ref55006 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55006
%stackaddr$env-ref55007 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55007
%stackaddr$env-ref55008 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55008
%stackaddr$env-ref55009 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48279, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55009
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim55010, align 8
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%current_45args53548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53547)
store volatile %struct.ScmObj* %current_45args53548, %struct.ScmObj** %stackaddr$prim55011, align 8
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53548)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47229)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$makeclosure55015 = alloca %struct.ScmObj*, align 8
%fptrToInt55016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48305 to i64
%ae48305 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55016)
store volatile %struct.ScmObj* %ae48305, %struct.ScmObj** %stackaddr$makeclosure55015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48305, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48305, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48305, %struct.ScmObj* %vs47102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48305, %struct.ScmObj* %anf_45bind47228, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48305, %struct.ScmObj* %f47099, i64 4)
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%cpsargs47423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48305, %struct.ScmObj* %anf_45bind47230)
store volatile %struct.ScmObj* %cpsargs47423, %struct.ScmObj** %stackaddr$prim55017, align 8
%clofunc55018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55018(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47423)
ret void
}

define tailcc void @proc_clo$ae48305(%struct.ScmObj* %env$ae48305,%struct.ScmObj* %current_45args53550) {
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48305, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$env-ref55020 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48305, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55020
%stackaddr$env-ref55021 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48305, i64 2)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55021
%stackaddr$env-ref55022 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48305, i64 3)
store %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$env-ref55022
%stackaddr$env-ref55023 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48305, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55023
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%current_45args53551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53550)
store volatile %struct.ScmObj* %current_45args53551, %struct.ScmObj** %stackaddr$prim55025, align 8
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53551)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55026, align 8
%ae48310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %ae48310)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55027, align 8
%stackaddr$makeclosure55028 = alloca %struct.ScmObj*, align 8
%fptrToInt55029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48312 to i64
%ae48312 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55029)
store volatile %struct.ScmObj* %ae48312, %struct.ScmObj** %stackaddr$makeclosure55028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48312, %struct.ScmObj* %k47412, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48312, %struct.ScmObj* %f47099, i64 1)
%argslist53556$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55030 = alloca %struct.ScmObj*, align 8
%argslist53556$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist53556$_37foldr1470890)
store volatile %struct.ScmObj* %argslist53556$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55030, align 8
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%argslist53556$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist53556$_37foldr1470891)
store volatile %struct.ScmObj* %argslist53556$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%argslist53556$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist53556$_37foldr1470892)
store volatile %struct.ScmObj* %argslist53556$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%argslist53556$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48312, %struct.ScmObj* %argslist53556$_37foldr1470893)
store volatile %struct.ScmObj* %argslist53556$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55033, align 8
%clofunc55034 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55034(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist53556$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48312(%struct.ScmObj* %env$ae48312,%struct.ScmObj* %current_45args53553) {
%stackaddr$env-ref55035 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48312, i64 0)
store %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$env-ref55035
%stackaddr$env-ref55036 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48312, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55036
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%current_45args53554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53553)
store volatile %struct.ScmObj* %current_45args53554, %struct.ScmObj** %stackaddr$prim55038, align 8
%stackaddr$prim55039 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53554)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55039, align 8
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%cpsargs47422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47412, %struct.ScmObj* %anf_45bind47233)
store volatile %struct.ScmObj* %cpsargs47422, %struct.ScmObj** %stackaddr$prim55040, align 8
%clofunc55041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55041(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47422)
ret void
}

define tailcc void @proc_clo$ae48281(%struct.ScmObj* %env$ae48281,%struct.ScmObj* %current_45args53557) {
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53557)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%current_45args53558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53557)
store volatile %struct.ScmObj* %current_45args53558, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53558)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55044, align 8
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%current_45args53559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53558)
store volatile %struct.ScmObj* %current_45args53559, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53559)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%cpsprim47425 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47425, %struct.ScmObj** %stackaddr$prim55047, align 8
%ae48285 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53561$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%argslist53561$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47425, %struct.ScmObj* %argslist53561$k474240)
store volatile %struct.ScmObj* %argslist53561$k474241, %struct.ScmObj** %stackaddr$prim55048, align 8
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%argslist53561$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48285, %struct.ScmObj* %argslist53561$k474241)
store volatile %struct.ScmObj* %argslist53561$k474242, %struct.ScmObj** %stackaddr$prim55049, align 8
%clofunc55050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc55050(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist53561$k474242)
ret void
}

define tailcc void @proc_clo$ae48257(%struct.ScmObj* %env$ae48257,%struct.ScmObj* %current_45args53564) {
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53564)
store volatile %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$prim55051, align 8
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%current_45args53565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53564)
store volatile %struct.ScmObj* %current_45args53565, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53565)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%cpsprim47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47427, %struct.ScmObj** %stackaddr$prim55054, align 8
%ae48260 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53567$k474260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%argslist53567$k474261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47427, %struct.ScmObj* %argslist53567$k474260)
store volatile %struct.ScmObj* %argslist53567$k474261, %struct.ScmObj** %stackaddr$prim55055, align 8
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%argslist53567$k474262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48260, %struct.ScmObj* %argslist53567$k474261)
store volatile %struct.ScmObj* %argslist53567$k474262, %struct.ScmObj** %stackaddr$prim55056, align 8
%clofunc55057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47426)
musttail call tailcc void %clofunc55057(%struct.ScmObj* %k47426, %struct.ScmObj* %argslist53567$k474262)
ret void
}

define tailcc void @proc_clo$ae48233(%struct.ScmObj* %env$ae48233,%struct.ScmObj* %current_45args53570) {
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53570)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim55058, align 8
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%current_45args53571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53570)
store volatile %struct.ScmObj* %current_45args53571, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53571)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%cpsprim47429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47429, %struct.ScmObj** %stackaddr$prim55061, align 8
%ae48236 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53573$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%argslist53573$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47429, %struct.ScmObj* %argslist53573$k474280)
store volatile %struct.ScmObj* %argslist53573$k474281, %struct.ScmObj** %stackaddr$prim55062, align 8
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%argslist53573$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48236, %struct.ScmObj* %argslist53573$k474281)
store volatile %struct.ScmObj* %argslist53573$k474282, %struct.ScmObj** %stackaddr$prim55063, align 8
%clofunc55064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc55064(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist53573$k474282)
ret void
}

define tailcc void @proc_clo$ae48185(%struct.ScmObj* %env$ae48185,%struct.ScmObj* %current_45args53576) {
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53576)
store volatile %struct.ScmObj* %k47430, %struct.ScmObj** %stackaddr$prim55065, align 8
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%current_45args53577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53576)
store volatile %struct.ScmObj* %current_45args53577, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53577)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%current_45args53578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53577)
store volatile %struct.ScmObj* %current_45args53578, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53578)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim55069, align 8
%truthy$cmp55070 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp55070 = icmp eq i64 %truthy$cmp55070, 1
br i1 %cmp$cmp55070, label %truebranch$cmp55070, label %falsebranch$cmp55070
truebranch$cmp55070:
%ae48188 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53580$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%argslist53580$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist53580$k474300)
store volatile %struct.ScmObj* %argslist53580$k474301, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%argslist53580$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48188, %struct.ScmObj* %argslist53580$k474301)
store volatile %struct.ScmObj* %argslist53580$k474302, %struct.ScmObj** %stackaddr$prim55072, align 8
%clofunc55073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55073(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist53580$k474302)
ret void
falsebranch$cmp55070:
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%cpsprim47431 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47431, %struct.ScmObj** %stackaddr$prim55074, align 8
%ae48195 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53581$k474300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist53581$k474301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47431, %struct.ScmObj* %argslist53581$k474300)
store volatile %struct.ScmObj* %argslist53581$k474301, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$prim55076 = alloca %struct.ScmObj*, align 8
%argslist53581$k474302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48195, %struct.ScmObj* %argslist53581$k474301)
store volatile %struct.ScmObj* %argslist53581$k474302, %struct.ScmObj** %stackaddr$prim55076, align 8
%clofunc55077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47430)
musttail call tailcc void %clofunc55077(%struct.ScmObj* %k47430, %struct.ScmObj* %argslist53581$k474302)
ret void
}

define tailcc void @proc_clo$ae48142(%struct.ScmObj* %env$ae48142,%struct.ScmObj* %current_45args53585) {
%stackaddr$env-ref55078 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48142, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref55078
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48142, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53585)
store volatile %struct.ScmObj* %k47432, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%current_45args53586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53585)
store volatile %struct.ScmObj* %current_45args53586, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%current_45args53587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53586)
store volatile %struct.ScmObj* %current_45args53587, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$prim55084 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53587)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim55084, align 8
%stackaddr$makeclosure55085 = alloca %struct.ScmObj*, align 8
%fptrToInt55086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48144 to i64
%ae48144 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55086)
store volatile %struct.ScmObj* %ae48144, %struct.ScmObj** %stackaddr$makeclosure55085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48144, %struct.ScmObj* %lst47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48144, %struct.ScmObj* %n47109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48144, %struct.ScmObj* %_37take47081, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48144, %struct.ScmObj* %k47432, i64 3)
%argslist53593$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%argslist53593$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53593$_37length470780)
store volatile %struct.ScmObj* %argslist53593$_37length470781, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist53593$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48144, %struct.ScmObj* %argslist53593$_37length470781)
store volatile %struct.ScmObj* %argslist53593$_37length470782, %struct.ScmObj** %stackaddr$prim55088, align 8
%clofunc55089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc55089(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist53593$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48144(%struct.ScmObj* %env$ae48144,%struct.ScmObj* %current_45args53589) {
%stackaddr$env-ref55090 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48144, i64 0)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref55090
%stackaddr$env-ref55091 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48144, i64 1)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref55091
%stackaddr$env-ref55092 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48144, i64 2)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55092
%stackaddr$env-ref55093 = alloca %struct.ScmObj*, align 8
%k47432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48144, i64 3)
store %struct.ScmObj* %k47432, %struct.ScmObj** %stackaddr$env-ref55093
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%current_45args53590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53589)
store volatile %struct.ScmObj* %current_45args53590, %struct.ScmObj** %stackaddr$prim55095, align 8
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53590)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim55096, align 8
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55097, align 8
%argslist53592$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%argslist53592$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist53592$_37take470810)
store volatile %struct.ScmObj* %argslist53592$_37take470811, %struct.ScmObj** %stackaddr$prim55098, align 8
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%argslist53592$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist53592$_37take470811)
store volatile %struct.ScmObj* %argslist53592$_37take470812, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%argslist53592$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47432, %struct.ScmObj* %argslist53592$_37take470812)
store volatile %struct.ScmObj* %argslist53592$_37take470813, %struct.ScmObj** %stackaddr$prim55100, align 8
%clofunc55101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc55101(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist53592$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48088(%struct.ScmObj* %env$ae48088,%struct.ScmObj* %current_45args53595) {
%stackaddr$env-ref55102 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48088, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55102
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53595)
store volatile %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$prim55103, align 8
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%current_45args53596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53595)
store volatile %struct.ScmObj* %current_45args53596, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53596)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$makeclosure55106 = alloca %struct.ScmObj*, align 8
%fptrToInt55107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48089 to i64
%ae48089 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55107)
store volatile %struct.ScmObj* %ae48089, %struct.ScmObj** %stackaddr$makeclosure55106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %k47434, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48089, %struct.ScmObj* %lst47112, i64 2)
%ae48090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55108 = alloca %struct.ScmObj*, align 8
%fptrToInt55109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48091 to i64
%ae48091 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55109)
store volatile %struct.ScmObj* %ae48091, %struct.ScmObj** %stackaddr$makeclosure55108, align 8
%argslist53607$ae480890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%argslist53607$ae480891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48091, %struct.ScmObj* %argslist53607$ae480890)
store volatile %struct.ScmObj* %argslist53607$ae480891, %struct.ScmObj** %stackaddr$prim55110, align 8
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%argslist53607$ae480892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48090, %struct.ScmObj* %argslist53607$ae480891)
store volatile %struct.ScmObj* %argslist53607$ae480892, %struct.ScmObj** %stackaddr$prim55111, align 8
%clofunc55112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48089)
musttail call tailcc void %clofunc55112(%struct.ScmObj* %ae48089, %struct.ScmObj* %argslist53607$ae480892)
ret void
}

define tailcc void @proc_clo$ae48089(%struct.ScmObj* %env$ae48089,%struct.ScmObj* %current_45args53598) {
%stackaddr$env-ref55113 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55113
%stackaddr$env-ref55114 = alloca %struct.ScmObj*, align 8
%k47434 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 1)
store %struct.ScmObj* %k47434, %struct.ScmObj** %stackaddr$env-ref55114
%stackaddr$env-ref55115 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48089, i64 2)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref55115
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53598)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim55116, align 8
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%current_45args53599 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53598)
store volatile %struct.ScmObj* %current_45args53599, %struct.ScmObj** %stackaddr$prim55117, align 8
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53599)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55118, align 8
%ae48110 = call %struct.ScmObj* @const_init_null()
%argslist53601$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%argslist53601$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist53601$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53601$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%argslist53601$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48110, %struct.ScmObj* %argslist53601$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53601$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%argslist53601$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist53601$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53601$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%argslist53601$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47434, %struct.ScmObj* %argslist53601$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53601$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55122, align 8
%clofunc55123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55123(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53601$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48091(%struct.ScmObj* %env$ae48091,%struct.ScmObj* %current_45args53602) {
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53602)
store volatile %struct.ScmObj* %k47436, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%current_45args53603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53602)
store volatile %struct.ScmObj* %current_45args53603, %struct.ScmObj** %stackaddr$prim55125, align 8
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%current_45args53604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53603)
store volatile %struct.ScmObj* %current_45args53604, %struct.ScmObj** %stackaddr$prim55127, align 8
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53604)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim55128, align 8
%ae48093 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53606$k474360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%argslist53606$k474361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist53606$k474360)
store volatile %struct.ScmObj* %argslist53606$k474361, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%argslist53606$k474362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48093, %struct.ScmObj* %argslist53606$k474361)
store volatile %struct.ScmObj* %argslist53606$k474362, %struct.ScmObj** %stackaddr$prim55130, align 8
%clofunc55131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47436)
musttail call tailcc void %clofunc55131(%struct.ScmObj* %k47436, %struct.ScmObj* %argslist53606$k474362)
ret void
}

define tailcc void @proc_clo$ae48009(%struct.ScmObj* %env$ae48009,%struct.ScmObj* %current_45args53610) {
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53610)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%current_45args53611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53610)
store volatile %struct.ScmObj* %current_45args53611, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53611)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim55134, align 8
%ae48011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55135 = alloca %struct.ScmObj*, align 8
%fptrToInt55136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48012 to i64
%ae48012 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55136)
store volatile %struct.ScmObj* %ae48012, %struct.ScmObj** %stackaddr$makeclosure55135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48012, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist53624$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%argslist53624$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48012, %struct.ScmObj* %argslist53624$k474370)
store volatile %struct.ScmObj* %argslist53624$k474371, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%argslist53624$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48011, %struct.ScmObj* %argslist53624$k474371)
store volatile %struct.ScmObj* %argslist53624$k474372, %struct.ScmObj** %stackaddr$prim55138, align 8
%clofunc55139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc55139(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist53624$k474372)
ret void
}

define tailcc void @proc_clo$ae48012(%struct.ScmObj* %env$ae48012,%struct.ScmObj* %current_45args53613) {
%stackaddr$env-ref55140 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48012, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55140
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53613)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%current_45args53614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53613)
store volatile %struct.ScmObj* %current_45args53614, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53614)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%current_45args53615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53614)
store volatile %struct.ScmObj* %current_45args53615, %struct.ScmObj** %stackaddr$prim55144, align 8
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%current_45args53616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53615)
store volatile %struct.ScmObj* %current_45args53616, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53616)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55148, align 8
%truthy$cmp55149 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47214)
%cmp$cmp55149 = icmp eq i64 %truthy$cmp55149, 1
br i1 %cmp$cmp55149, label %truebranch$cmp55149, label %falsebranch$cmp55149
truebranch$cmp55149:
%ae48016 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53618$k474380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%argslist53618$k474381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53618$k474380)
store volatile %struct.ScmObj* %argslist53618$k474381, %struct.ScmObj** %stackaddr$prim55150, align 8
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist53618$k474382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48016, %struct.ScmObj* %argslist53618$k474381)
store volatile %struct.ScmObj* %argslist53618$k474382, %struct.ScmObj** %stackaddr$prim55151, align 8
%clofunc55152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47438)
musttail call tailcc void %clofunc55152(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist53618$k474382)
ret void
falsebranch$cmp55149:
%stackaddr$prim55153 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim55153, align 8
%stackaddr$makeclosure55154 = alloca %struct.ScmObj*, align 8
%fptrToInt55155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48023 to i64
%ae48023 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55155)
store volatile %struct.ScmObj* %ae48023, %struct.ScmObj** %stackaddr$makeclosure55154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48023, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48023, %struct.ScmObj* %lst47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48023, %struct.ScmObj* %_37foldl147074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48023, %struct.ScmObj* %k47438, i64 3)
%argslist53623$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%argslist53623$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist53623$f470770)
store volatile %struct.ScmObj* %argslist53623$f470771, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%argslist53623$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist53623$f470771)
store volatile %struct.ScmObj* %argslist53623$f470772, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%argslist53623$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48023, %struct.ScmObj* %argslist53623$f470772)
store volatile %struct.ScmObj* %argslist53623$f470773, %struct.ScmObj** %stackaddr$prim55158, align 8
%clofunc55159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc55159(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53623$f470773)
ret void
}

define tailcc void @proc_clo$ae48023(%struct.ScmObj* %env$ae48023,%struct.ScmObj* %current_45args53619) {
%stackaddr$env-ref55160 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48023, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref55160
%stackaddr$env-ref55161 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48023, i64 1)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref55161
%stackaddr$env-ref55162 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48023, i64 2)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55162
%stackaddr$env-ref55163 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48023, i64 3)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref55163
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53619)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%current_45args53620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53619)
store volatile %struct.ScmObj* %current_45args53620, %struct.ScmObj** %stackaddr$prim55165, align 8
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53620)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim55167, align 8
%argslist53622$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%argslist53622$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist53622$_37foldl1470740)
store volatile %struct.ScmObj* %argslist53622$_37foldl1470741, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%argslist53622$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist53622$_37foldl1470741)
store volatile %struct.ScmObj* %argslist53622$_37foldl1470742, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%argslist53622$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist53622$_37foldl1470742)
store volatile %struct.ScmObj* %argslist53622$_37foldl1470743, %struct.ScmObj** %stackaddr$prim55170, align 8
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%argslist53622$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist53622$_37foldl1470743)
store volatile %struct.ScmObj* %argslist53622$_37foldl1470744, %struct.ScmObj** %stackaddr$prim55171, align 8
%clofunc55172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc55172(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist53622$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47926(%struct.ScmObj* %env$ae47926,%struct.ScmObj* %current_45args53627) {
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53627)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%current_45args53628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53627)
store volatile %struct.ScmObj* %current_45args53628, %struct.ScmObj** %stackaddr$prim55174, align 8
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53628)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim55175, align 8
%ae47928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55176 = alloca %struct.ScmObj*, align 8
%fptrToInt55177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47929 to i64
%ae47929 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55177)
store volatile %struct.ScmObj* %ae47929, %struct.ScmObj** %stackaddr$makeclosure55176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47929, %struct.ScmObj* %_37length47079, i64 0)
%argslist53639$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%argslist53639$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47929, %struct.ScmObj* %argslist53639$k474400)
store volatile %struct.ScmObj* %argslist53639$k474401, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%argslist53639$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47928, %struct.ScmObj* %argslist53639$k474401)
store volatile %struct.ScmObj* %argslist53639$k474402, %struct.ScmObj** %stackaddr$prim55179, align 8
%clofunc55180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc55180(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist53639$k474402)
ret void
}

define tailcc void @proc_clo$ae47929(%struct.ScmObj* %env$ae47929,%struct.ScmObj* %current_45args53630) {
%stackaddr$env-ref55181 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47929, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref55181
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53630)
store volatile %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$prim55182, align 8
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%current_45args53631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53630)
store volatile %struct.ScmObj* %current_45args53631, %struct.ScmObj** %stackaddr$prim55183, align 8
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53631)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim55185, align 8
%truthy$cmp55186 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47210)
%cmp$cmp55186 = icmp eq i64 %truthy$cmp55186, 1
br i1 %cmp$cmp55186, label %truebranch$cmp55186, label %falsebranch$cmp55186
truebranch$cmp55186:
%ae47933 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47934 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53633$k474410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%argslist53633$k474411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47934, %struct.ScmObj* %argslist53633$k474410)
store volatile %struct.ScmObj* %argslist53633$k474411, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%argslist53633$k474412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47933, %struct.ScmObj* %argslist53633$k474411)
store volatile %struct.ScmObj* %argslist53633$k474412, %struct.ScmObj** %stackaddr$prim55188, align 8
%clofunc55189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47441)
musttail call tailcc void %clofunc55189(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist53633$k474412)
ret void
falsebranch$cmp55186:
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$makeclosure55191 = alloca %struct.ScmObj*, align 8
%fptrToInt55192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47943 to i64
%ae47943 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55192)
store volatile %struct.ScmObj* %ae47943, %struct.ScmObj** %stackaddr$makeclosure55191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47943, %struct.ScmObj* %k47441, i64 0)
%argslist53638$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist53638$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist53638$_37length470790)
store volatile %struct.ScmObj* %argslist53638$_37length470791, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist53638$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47943, %struct.ScmObj* %argslist53638$_37length470791)
store volatile %struct.ScmObj* %argslist53638$_37length470792, %struct.ScmObj** %stackaddr$prim55194, align 8
%clofunc55195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc55195(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist53638$_37length470792)
ret void
}

define tailcc void @proc_clo$ae47943(%struct.ScmObj* %env$ae47943,%struct.ScmObj* %current_45args53634) {
%stackaddr$env-ref55196 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47943, i64 0)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref55196
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53634)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim55197, align 8
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%current_45args53635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53634)
store volatile %struct.ScmObj* %current_45args53635, %struct.ScmObj** %stackaddr$prim55198, align 8
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53635)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim55199, align 8
%ae47945 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%cpsprim47443 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47945, %struct.ScmObj* %anf_45bind47212)
store volatile %struct.ScmObj* %cpsprim47443, %struct.ScmObj** %stackaddr$prim55200, align 8
%ae47948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53637$k474410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%argslist53637$k474411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47443, %struct.ScmObj* %argslist53637$k474410)
store volatile %struct.ScmObj* %argslist53637$k474411, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%argslist53637$k474412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47948, %struct.ScmObj* %argslist53637$k474411)
store volatile %struct.ScmObj* %argslist53637$k474412, %struct.ScmObj** %stackaddr$prim55202, align 8
%clofunc55203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47441)
musttail call tailcc void %clofunc55203(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist53637$k474412)
ret void
}

define tailcc void @proc_clo$ae47776(%struct.ScmObj* %env$ae47776,%struct.ScmObj* %current_45args53642) {
%stackaddr$prim55204 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53642)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim55204, align 8
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%current_45args53643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53642)
store volatile %struct.ScmObj* %current_45args53643, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53643)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim55206, align 8
%ae47778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55207 = alloca %struct.ScmObj*, align 8
%fptrToInt55208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47779 to i64
%ae47779 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55208)
store volatile %struct.ScmObj* %ae47779, %struct.ScmObj** %stackaddr$makeclosure55207, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47779, %struct.ScmObj* %_37take47082, i64 0)
%argslist53656$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%argslist53656$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47779, %struct.ScmObj* %argslist53656$k474440)
store volatile %struct.ScmObj* %argslist53656$k474441, %struct.ScmObj** %stackaddr$prim55209, align 8
%stackaddr$prim55210 = alloca %struct.ScmObj*, align 8
%argslist53656$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47778, %struct.ScmObj* %argslist53656$k474441)
store volatile %struct.ScmObj* %argslist53656$k474442, %struct.ScmObj** %stackaddr$prim55210, align 8
%clofunc55211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc55211(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist53656$k474442)
ret void
}

define tailcc void @proc_clo$ae47779(%struct.ScmObj* %env$ae47779,%struct.ScmObj* %current_45args53645) {
%stackaddr$env-ref55212 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47779, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref55212
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %k47445, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%current_45args53646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53645)
store volatile %struct.ScmObj* %current_45args53646, %struct.ScmObj** %stackaddr$prim55214, align 8
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%current_45args53647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53646)
store volatile %struct.ScmObj* %current_45args53647, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53647)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim55217, align 8
%ae47781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47781)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim55218, align 8
%truthy$cmp55219 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47203)
%cmp$cmp55219 = icmp eq i64 %truthy$cmp55219, 1
br i1 %cmp$cmp55219, label %truebranch$cmp55219, label %falsebranch$cmp55219
truebranch$cmp55219:
%ae47784 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47785 = call %struct.ScmObj* @const_init_null()
%argslist53649$k474450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%argslist53649$k474451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47785, %struct.ScmObj* %argslist53649$k474450)
store volatile %struct.ScmObj* %argslist53649$k474451, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%argslist53649$k474452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47784, %struct.ScmObj* %argslist53649$k474451)
store volatile %struct.ScmObj* %argslist53649$k474452, %struct.ScmObj** %stackaddr$prim55221, align 8
%clofunc55222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47445)
musttail call tailcc void %clofunc55222(%struct.ScmObj* %k47445, %struct.ScmObj* %argslist53649$k474452)
ret void
falsebranch$cmp55219:
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim55223, align 8
%truthy$cmp55224 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47204)
%cmp$cmp55224 = icmp eq i64 %truthy$cmp55224, 1
br i1 %cmp$cmp55224, label %truebranch$cmp55224, label %falsebranch$cmp55224
truebranch$cmp55224:
%ae47795 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47796 = call %struct.ScmObj* @const_init_null()
%argslist53650$k474450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%argslist53650$k474451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47796, %struct.ScmObj* %argslist53650$k474450)
store volatile %struct.ScmObj* %argslist53650$k474451, %struct.ScmObj** %stackaddr$prim55225, align 8
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%argslist53650$k474452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47795, %struct.ScmObj* %argslist53650$k474451)
store volatile %struct.ScmObj* %argslist53650$k474452, %struct.ScmObj** %stackaddr$prim55226, align 8
%clofunc55227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47445)
musttail call tailcc void %clofunc55227(%struct.ScmObj* %k47445, %struct.ScmObj* %argslist53650$k474452)
ret void
falsebranch$cmp55224:
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim55229, align 8
%ae47806 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47806)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim55230, align 8
%stackaddr$makeclosure55231 = alloca %struct.ScmObj*, align 8
%fptrToInt55232 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47808 to i64
%ae47808 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55232)
store volatile %struct.ScmObj* %ae47808, %struct.ScmObj** %stackaddr$makeclosure55231, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47808, %struct.ScmObj* %anf_45bind47205, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47808, %struct.ScmObj* %k47445, i64 1)
%argslist53655$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%argslist53655$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist53655$_37take470820)
store volatile %struct.ScmObj* %argslist53655$_37take470821, %struct.ScmObj** %stackaddr$prim55233, align 8
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%argslist53655$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist53655$_37take470821)
store volatile %struct.ScmObj* %argslist53655$_37take470822, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%argslist53655$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47808, %struct.ScmObj* %argslist53655$_37take470822)
store volatile %struct.ScmObj* %argslist53655$_37take470823, %struct.ScmObj** %stackaddr$prim55235, align 8
%clofunc55236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc55236(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist53655$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47808(%struct.ScmObj* %env$ae47808,%struct.ScmObj* %current_45args53651) {
%stackaddr$env-ref55237 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47808, i64 0)
store %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$env-ref55237
%stackaddr$env-ref55238 = alloca %struct.ScmObj*, align 8
%k47445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47808, i64 1)
store %struct.ScmObj* %k47445, %struct.ScmObj** %stackaddr$env-ref55238
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53651)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%current_45args53652 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53651)
store volatile %struct.ScmObj* %current_45args53652, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53652)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim55241, align 8
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%cpsprim47447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %anf_45bind47208)
store volatile %struct.ScmObj* %cpsprim47447, %struct.ScmObj** %stackaddr$prim55242, align 8
%ae47814 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53654$k474450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist53654$k474451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47447, %struct.ScmObj* %argslist53654$k474450)
store volatile %struct.ScmObj* %argslist53654$k474451, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%argslist53654$k474452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47814, %struct.ScmObj* %argslist53654$k474451)
store volatile %struct.ScmObj* %argslist53654$k474452, %struct.ScmObj** %stackaddr$prim55244, align 8
%clofunc55245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47445)
musttail call tailcc void %clofunc55245(%struct.ScmObj* %k47445, %struct.ScmObj* %argslist53654$k474452)
ret void
}

define tailcc void @proc_clo$ae47679(%struct.ScmObj* %env$ae47679,%struct.ScmObj* %current_45args53659) {
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53659)
store volatile %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$prim55246, align 8
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%current_45args53660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53659)
store volatile %struct.ScmObj* %current_45args53660, %struct.ScmObj** %stackaddr$prim55247, align 8
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53660)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim55248, align 8
%ae47681 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55249 = alloca %struct.ScmObj*, align 8
%fptrToInt55250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47682 to i64
%ae47682 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55250)
store volatile %struct.ScmObj* %ae47682, %struct.ScmObj** %stackaddr$makeclosure55249, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47682, %struct.ScmObj* %_37map47086, i64 0)
%argslist53676$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%argslist53676$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47682, %struct.ScmObj* %argslist53676$k474480)
store volatile %struct.ScmObj* %argslist53676$k474481, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%argslist53676$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47681, %struct.ScmObj* %argslist53676$k474481)
store volatile %struct.ScmObj* %argslist53676$k474482, %struct.ScmObj** %stackaddr$prim55252, align 8
%clofunc55253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc55253(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist53676$k474482)
ret void
}

define tailcc void @proc_clo$ae47682(%struct.ScmObj* %env$ae47682,%struct.ScmObj* %current_45args53662) {
%stackaddr$env-ref55254 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47682, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55254
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53662)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%current_45args53663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53662)
store volatile %struct.ScmObj* %current_45args53663, %struct.ScmObj** %stackaddr$prim55256, align 8
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%current_45args53664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53663)
store volatile %struct.ScmObj* %current_45args53664, %struct.ScmObj** %stackaddr$prim55258, align 8
%stackaddr$prim55259 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53664)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim55259, align 8
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim55260, align 8
%truthy$cmp55261 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47197)
%cmp$cmp55261 = icmp eq i64 %truthy$cmp55261, 1
br i1 %cmp$cmp55261, label %truebranch$cmp55261, label %falsebranch$cmp55261
truebranch$cmp55261:
%ae47686 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47687 = call %struct.ScmObj* @const_init_null()
%argslist53666$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%argslist53666$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47687, %struct.ScmObj* %argslist53666$k474490)
store volatile %struct.ScmObj* %argslist53666$k474491, %struct.ScmObj** %stackaddr$prim55262, align 8
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%argslist53666$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47686, %struct.ScmObj* %argslist53666$k474491)
store volatile %struct.ScmObj* %argslist53666$k474492, %struct.ScmObj** %stackaddr$prim55263, align 8
%clofunc55264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55264(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist53666$k474492)
ret void
falsebranch$cmp55261:
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$makeclosure55266 = alloca %struct.ScmObj*, align 8
%fptrToInt55267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47696 to i64
%ae47696 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55267)
store volatile %struct.ScmObj* %ae47696, %struct.ScmObj** %stackaddr$makeclosure55266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %f47088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %lst47087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %_37map47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47696, %struct.ScmObj* %k47449, i64 3)
%argslist53675$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%argslist53675$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %argslist53675$f470880)
store volatile %struct.ScmObj* %argslist53675$f470881, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%argslist53675$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47696, %struct.ScmObj* %argslist53675$f470881)
store volatile %struct.ScmObj* %argslist53675$f470882, %struct.ScmObj** %stackaddr$prim55269, align 8
%clofunc55270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc55270(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53675$f470882)
ret void
}

define tailcc void @proc_clo$ae47696(%struct.ScmObj* %env$ae47696,%struct.ScmObj* %current_45args53667) {
%stackaddr$env-ref55271 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 0)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref55271
%stackaddr$env-ref55272 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 1)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref55272
%stackaddr$env-ref55273 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 2)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref55273
%stackaddr$env-ref55274 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47696, i64 3)
store %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$env-ref55274
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53667)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim55275, align 8
%stackaddr$prim55276 = alloca %struct.ScmObj*, align 8
%current_45args53668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53667)
store volatile %struct.ScmObj* %current_45args53668, %struct.ScmObj** %stackaddr$prim55276, align 8
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53668)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim55278, align 8
%stackaddr$makeclosure55279 = alloca %struct.ScmObj*, align 8
%fptrToInt55280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47700 to i64
%ae47700 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55280)
store volatile %struct.ScmObj* %ae47700, %struct.ScmObj** %stackaddr$makeclosure55279, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47700, %struct.ScmObj* %anf_45bind47199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47700, %struct.ScmObj* %k47449, i64 1)
%argslist53674$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist53674$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist53674$_37map470860)
store volatile %struct.ScmObj* %argslist53674$_37map470861, %struct.ScmObj** %stackaddr$prim55281, align 8
%stackaddr$prim55282 = alloca %struct.ScmObj*, align 8
%argslist53674$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist53674$_37map470861)
store volatile %struct.ScmObj* %argslist53674$_37map470862, %struct.ScmObj** %stackaddr$prim55282, align 8
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%argslist53674$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47700, %struct.ScmObj* %argslist53674$_37map470862)
store volatile %struct.ScmObj* %argslist53674$_37map470863, %struct.ScmObj** %stackaddr$prim55283, align 8
%clofunc55284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc55284(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist53674$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47700(%struct.ScmObj* %env$ae47700,%struct.ScmObj* %current_45args53670) {
%stackaddr$env-ref55285 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47700, i64 0)
store %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$env-ref55285
%stackaddr$env-ref55286 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47700, i64 1)
store %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$env-ref55286
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%_95k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53670)
store volatile %struct.ScmObj* %_95k47451, %struct.ScmObj** %stackaddr$prim55287, align 8
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%current_45args53671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53670)
store volatile %struct.ScmObj* %current_45args53671, %struct.ScmObj** %stackaddr$prim55288, align 8
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53671)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%cpsprim47452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %anf_45bind47201)
store volatile %struct.ScmObj* %cpsprim47452, %struct.ScmObj** %stackaddr$prim55290, align 8
%ae47706 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53673$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%argslist53673$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47452, %struct.ScmObj* %argslist53673$k474490)
store volatile %struct.ScmObj* %argslist53673$k474491, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%argslist53673$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47706, %struct.ScmObj* %argslist53673$k474491)
store volatile %struct.ScmObj* %argslist53673$k474492, %struct.ScmObj** %stackaddr$prim55292, align 8
%clofunc55293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55293(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist53673$k474492)
ret void
}

define tailcc void @proc_clo$ae47599(%struct.ScmObj* %env$ae47599,%struct.ScmObj* %current_45args53679) {
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53679)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%current_45args53680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53679)
store volatile %struct.ScmObj* %current_45args53680, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$prim55296 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53680)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim55296, align 8
%ae47601 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55297 = alloca %struct.ScmObj*, align 8
%fptrToInt55298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47602 to i64
%ae47602 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55298)
store volatile %struct.ScmObj* %ae47602, %struct.ScmObj** %stackaddr$makeclosure55297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47602, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist53693$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%argslist53693$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47602, %struct.ScmObj* %argslist53693$k474530)
store volatile %struct.ScmObj* %argslist53693$k474531, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%argslist53693$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47601, %struct.ScmObj* %argslist53693$k474531)
store volatile %struct.ScmObj* %argslist53693$k474532, %struct.ScmObj** %stackaddr$prim55300, align 8
%clofunc55301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc55301(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist53693$k474532)
ret void
}

define tailcc void @proc_clo$ae47602(%struct.ScmObj* %env$ae47602,%struct.ScmObj* %current_45args53682) {
%stackaddr$env-ref55302 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47602, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref55302
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53682)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim55303, align 8
%stackaddr$prim55304 = alloca %struct.ScmObj*, align 8
%current_45args53683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53682)
store volatile %struct.ScmObj* %current_45args53683, %struct.ScmObj** %stackaddr$prim55304, align 8
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53683)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%current_45args53684 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53683)
store volatile %struct.ScmObj* %current_45args53684, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%current_45args53685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53684)
store volatile %struct.ScmObj* %current_45args53685, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$prim55309 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53685)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim55309, align 8
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$prim55310, align 8
%truthy$cmp55311 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47192)
%cmp$cmp55311 = icmp eq i64 %truthy$cmp55311, 1
br i1 %cmp$cmp55311, label %truebranch$cmp55311, label %falsebranch$cmp55311
truebranch$cmp55311:
%ae47606 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53687$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55312 = alloca %struct.ScmObj*, align 8
%argslist53687$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53687$k474540)
store volatile %struct.ScmObj* %argslist53687$k474541, %struct.ScmObj** %stackaddr$prim55312, align 8
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%argslist53687$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47606, %struct.ScmObj* %argslist53687$k474541)
store volatile %struct.ScmObj* %argslist53687$k474542, %struct.ScmObj** %stackaddr$prim55313, align 8
%clofunc55314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc55314(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist53687$k474542)
ret void
falsebranch$cmp55311:
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$makeclosure55317 = alloca %struct.ScmObj*, align 8
%fptrToInt55318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47614 to i64
%ae47614 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55318)
store volatile %struct.ScmObj* %ae47614, %struct.ScmObj** %stackaddr$makeclosure55317, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47614, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47614, %struct.ScmObj* %k47454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47614, %struct.ScmObj* %anf_45bind47193, i64 2)
%argslist53692$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%argslist53692$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist53692$_37foldr1470900)
store volatile %struct.ScmObj* %argslist53692$_37foldr1470901, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%argslist53692$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist53692$_37foldr1470901)
store volatile %struct.ScmObj* %argslist53692$_37foldr1470902, %struct.ScmObj** %stackaddr$prim55320, align 8
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%argslist53692$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53692$_37foldr1470902)
store volatile %struct.ScmObj* %argslist53692$_37foldr1470903, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%argslist53692$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47614, %struct.ScmObj* %argslist53692$_37foldr1470903)
store volatile %struct.ScmObj* %argslist53692$_37foldr1470904, %struct.ScmObj** %stackaddr$prim55322, align 8
%clofunc55323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc55323(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist53692$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47614(%struct.ScmObj* %env$ae47614,%struct.ScmObj* %current_45args53688) {
%stackaddr$env-ref55324 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47614, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref55324
%stackaddr$env-ref55325 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47614, i64 1)
store %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$env-ref55325
%stackaddr$env-ref55326 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47614, i64 2)
store %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$env-ref55326
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%_95k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53688)
store volatile %struct.ScmObj* %_95k47455, %struct.ScmObj** %stackaddr$prim55327, align 8
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%current_45args53689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53688)
store volatile %struct.ScmObj* %current_45args53689, %struct.ScmObj** %stackaddr$prim55328, align 8
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53689)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim55329, align 8
%argslist53691$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist53691$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist53691$f470930)
store volatile %struct.ScmObj* %argslist53691$f470931, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%argslist53691$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %argslist53691$f470931)
store volatile %struct.ScmObj* %argslist53691$f470932, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%argslist53691$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist53691$f470932)
store volatile %struct.ScmObj* %argslist53691$f470933, %struct.ScmObj** %stackaddr$prim55332, align 8
%clofunc55333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc55333(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist53691$f470933)
ret void
}

define tailcc void @proc_clo$ae47482(%struct.ScmObj* %env$ae47482,%struct.ScmObj* %current_45args53696) {
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53696)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%current_45args53697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53696)
store volatile %struct.ScmObj* %current_45args53697, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53697)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim55336, align 8
%ae47484 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55337 = alloca %struct.ScmObj*, align 8
%fptrToInt55338 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47485 to i64
%ae47485 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55338)
store volatile %struct.ScmObj* %ae47485, %struct.ScmObj** %stackaddr$makeclosure55337, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47485, %struct.ScmObj* %y47070, i64 0)
%argslist53715$k474560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist53715$k474561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47485, %struct.ScmObj* %argslist53715$k474560)
store volatile %struct.ScmObj* %argslist53715$k474561, %struct.ScmObj** %stackaddr$prim55339, align 8
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%argslist53715$k474562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47484, %struct.ScmObj* %argslist53715$k474561)
store volatile %struct.ScmObj* %argslist53715$k474562, %struct.ScmObj** %stackaddr$prim55340, align 8
%clofunc55341 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47456)
musttail call tailcc void %clofunc55341(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist53715$k474562)
ret void
}

define tailcc void @proc_clo$ae47485(%struct.ScmObj* %env$ae47485,%struct.ScmObj* %current_45args53699) {
%stackaddr$env-ref55342 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47485, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55342
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53699)
store volatile %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%current_45args53700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53699)
store volatile %struct.ScmObj* %current_45args53700, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53700)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim55345, align 8
%stackaddr$makeclosure55346 = alloca %struct.ScmObj*, align 8
%fptrToInt55347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47486 to i64
%ae47486 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55347)
store volatile %struct.ScmObj* %ae47486, %struct.ScmObj** %stackaddr$makeclosure55346, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47486, %struct.ScmObj* %k47457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47486, %struct.ScmObj* %f47071, i64 1)
%ae47487 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55348 = alloca %struct.ScmObj*, align 8
%fptrToInt55349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47488 to i64
%ae47488 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55349)
store volatile %struct.ScmObj* %ae47488, %struct.ScmObj** %stackaddr$makeclosure55348, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47488, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47488, %struct.ScmObj* %y47070, i64 1)
%argslist53714$ae474860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%argslist53714$ae474861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47488, %struct.ScmObj* %argslist53714$ae474860)
store volatile %struct.ScmObj* %argslist53714$ae474861, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%argslist53714$ae474862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47487, %struct.ScmObj* %argslist53714$ae474861)
store volatile %struct.ScmObj* %argslist53714$ae474862, %struct.ScmObj** %stackaddr$prim55351, align 8
%clofunc55352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47486)
musttail call tailcc void %clofunc55352(%struct.ScmObj* %ae47486, %struct.ScmObj* %argslist53714$ae474862)
ret void
}

define tailcc void @proc_clo$ae47486(%struct.ScmObj* %env$ae47486,%struct.ScmObj* %current_45args53702) {
%stackaddr$env-ref55353 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47486, i64 0)
store %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$env-ref55353
%stackaddr$env-ref55354 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47486, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55354
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%_95k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53702)
store volatile %struct.ScmObj* %_95k47458, %struct.ScmObj** %stackaddr$prim55355, align 8
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%current_45args53703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53702)
store volatile %struct.ScmObj* %current_45args53703, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%anf_45bind47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53703)
store volatile %struct.ScmObj* %anf_45bind47190, %struct.ScmObj** %stackaddr$prim55357, align 8
%argslist53705$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%argslist53705$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47190, %struct.ScmObj* %argslist53705$f470710)
store volatile %struct.ScmObj* %argslist53705$f470711, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%argslist53705$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist53705$f470711)
store volatile %struct.ScmObj* %argslist53705$f470712, %struct.ScmObj** %stackaddr$prim55359, align 8
%clofunc55360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc55360(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53705$f470712)
ret void
}

define tailcc void @proc_clo$ae47488(%struct.ScmObj* %env$ae47488,%struct.ScmObj* %args4707247459) {
%stackaddr$env-ref55361 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47488, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55361
%stackaddr$env-ref55362 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47488, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref55362
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247459)
store volatile %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247459)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$makeclosure55365 = alloca %struct.ScmObj*, align 8
%fptrToInt55366 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47492 to i64
%ae47492 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55366)
store volatile %struct.ScmObj* %ae47492, %struct.ScmObj** %stackaddr$makeclosure55365, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47492, %struct.ScmObj* %k47460, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47492, %struct.ScmObj* %args47072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47492, %struct.ScmObj* %f47071, i64 2)
%argslist53713$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%argslist53713$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53713$y470700)
store volatile %struct.ScmObj* %argslist53713$y470701, %struct.ScmObj** %stackaddr$prim55367, align 8
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%argslist53713$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47492, %struct.ScmObj* %argslist53713$y470701)
store volatile %struct.ScmObj* %argslist53713$y470702, %struct.ScmObj** %stackaddr$prim55368, align 8
%clofunc55369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc55369(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist53713$y470702)
ret void
}

define tailcc void @proc_clo$ae47492(%struct.ScmObj* %env$ae47492,%struct.ScmObj* %current_45args53706) {
%stackaddr$env-ref55370 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47492, i64 0)
store %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$env-ref55370
%stackaddr$env-ref55371 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47492, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55371
%stackaddr$env-ref55372 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47492, i64 2)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref55372
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53706)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%current_45args53707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53706)
store volatile %struct.ScmObj* %current_45args53707, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%anf_45bind47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53707)
store volatile %struct.ScmObj* %anf_45bind47188, %struct.ScmObj** %stackaddr$prim55375, align 8
%stackaddr$makeclosure55376 = alloca %struct.ScmObj*, align 8
%fptrToInt55377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47495 to i64
%ae47495 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55377)
store volatile %struct.ScmObj* %ae47495, %struct.ScmObj** %stackaddr$makeclosure55376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47495, %struct.ScmObj* %k47460, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47495, %struct.ScmObj* %args47072, i64 1)
%argslist53712$anf_45bind471880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist53712$anf_45bind471881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist53712$anf_45bind471880)
store volatile %struct.ScmObj* %argslist53712$anf_45bind471881, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%argslist53712$anf_45bind471882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47495, %struct.ScmObj* %argslist53712$anf_45bind471881)
store volatile %struct.ScmObj* %argslist53712$anf_45bind471882, %struct.ScmObj** %stackaddr$prim55379, align 8
%clofunc55380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47188)
musttail call tailcc void %clofunc55380(%struct.ScmObj* %anf_45bind47188, %struct.ScmObj* %argslist53712$anf_45bind471882)
ret void
}

define tailcc void @proc_clo$ae47495(%struct.ScmObj* %env$ae47495,%struct.ScmObj* %current_45args53709) {
%stackaddr$env-ref55381 = alloca %struct.ScmObj*, align 8
%k47460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47495, i64 0)
store %struct.ScmObj* %k47460, %struct.ScmObj** %stackaddr$env-ref55381
%stackaddr$env-ref55382 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47495, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref55382
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%_95k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53709)
store volatile %struct.ScmObj* %_95k47462, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%current_45args53710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53709)
store volatile %struct.ScmObj* %current_45args53710, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%anf_45bind47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53710)
store volatile %struct.ScmObj* %anf_45bind47189, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%cpsargs47463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47460, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47463, %struct.ScmObj** %stackaddr$prim55386, align 8
%clofunc55387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47189)
musttail call tailcc void %clofunc55387(%struct.ScmObj* %anf_45bind47189, %struct.ScmObj* %cpsargs47463)
ret void
}

define tailcc void @proc_clo$ae47467(%struct.ScmObj* %env$ae47467,%struct.ScmObj* %current_45args53717) {
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53717)
store volatile %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$prim55388, align 8
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%current_45args53718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53717)
store volatile %struct.ScmObj* %current_45args53718, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53718)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim55390, align 8
%argslist53720$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%argslist53720$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53720$yu470690)
store volatile %struct.ScmObj* %argslist53720$yu470691, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%argslist53720$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist53720$yu470691)
store volatile %struct.ScmObj* %argslist53720$yu470692, %struct.ScmObj** %stackaddr$prim55392, align 8
%clofunc55393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc55393(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist53720$yu470692)
ret void
}