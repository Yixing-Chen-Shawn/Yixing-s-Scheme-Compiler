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

@global$sym$ae4389647454 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv46974 = call %struct.ScmObj* @const_init_null()
%mainargs46975 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv46974, %struct.ScmObj* %mainargs46975)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv46972,%struct.ScmObj* %mainargs46973) {
%stackaddr$makeclosure46976 = alloca %struct.ScmObj*, align 8
%fptrToInt46977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40508 to i64
%ae40508 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46977)
store volatile %struct.ScmObj* %ae40508, %struct.ScmObj** %stackaddr$makeclosure46976, align 8
%ae40509 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46978 = alloca %struct.ScmObj*, align 8
%fptrToInt46979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40510 to i64
%ae40510 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46979)
store volatile %struct.ScmObj* %ae40510, %struct.ScmObj** %stackaddr$makeclosure46978, align 8
%args46971$ae40508$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46980 = alloca %struct.ScmObj*, align 8
%args46971$ae40508$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40510, %struct.ScmObj* %args46971$ae40508$0)
store volatile %struct.ScmObj* %args46971$ae40508$1, %struct.ScmObj** %stackaddr$prim46980, align 8
%stackaddr$prim46981 = alloca %struct.ScmObj*, align 8
%args46971$ae40508$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40509, %struct.ScmObj* %args46971$ae40508$1)
store volatile %struct.ScmObj* %args46971$ae40508$2, %struct.ScmObj** %stackaddr$prim46981, align 8
%clofunc46982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40508)
musttail call tailcc void %clofunc46982(%struct.ScmObj* %ae40508, %struct.ScmObj* %args46971$ae40508$2)
ret void
}

define tailcc void @proc_clo$ae40508(%struct.ScmObj* %env$ae40508,%struct.ScmObj* %current_45args46407) {
%stackaddr$prim46983 = alloca %struct.ScmObj*, align 8
%_95k40343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %_95k40343, %struct.ScmObj** %stackaddr$prim46983, align 8
%stackaddr$prim46984 = alloca %struct.ScmObj*, align 8
%current_45args46408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46407)
store volatile %struct.ScmObj* %current_45args46408, %struct.ScmObj** %stackaddr$prim46984, align 8
%stackaddr$prim46985 = alloca %struct.ScmObj*, align 8
%anf_45bind40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46408)
store volatile %struct.ScmObj* %anf_45bind40223, %struct.ScmObj** %stackaddr$prim46985, align 8
%stackaddr$makeclosure46986 = alloca %struct.ScmObj*, align 8
%fptrToInt46987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40523 to i64
%ae40523 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt46987)
store volatile %struct.ScmObj* %ae40523, %struct.ScmObj** %stackaddr$makeclosure46986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40523, %struct.ScmObj* %anf_45bind40223, i64 0)
%ae40524 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure46988 = alloca %struct.ScmObj*, align 8
%fptrToInt46989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40525 to i64
%ae40525 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46989)
store volatile %struct.ScmObj* %ae40525, %struct.ScmObj** %stackaddr$makeclosure46988, align 8
%args46966$ae40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46990 = alloca %struct.ScmObj*, align 8
%args46966$ae40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40525, %struct.ScmObj* %args46966$ae40523$0)
store volatile %struct.ScmObj* %args46966$ae40523$1, %struct.ScmObj** %stackaddr$prim46990, align 8
%stackaddr$prim46991 = alloca %struct.ScmObj*, align 8
%args46966$ae40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40524, %struct.ScmObj* %args46966$ae40523$1)
store volatile %struct.ScmObj* %args46966$ae40523$2, %struct.ScmObj** %stackaddr$prim46991, align 8
%clofunc46992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40523)
musttail call tailcc void %clofunc46992(%struct.ScmObj* %ae40523, %struct.ScmObj* %args46966$ae40523$2)
ret void
}

define tailcc void @proc_clo$ae40523(%struct.ScmObj* %env$ae40523,%struct.ScmObj* %current_45args46410) {
%stackaddr$env-ref46993 = alloca %struct.ScmObj*, align 8
%anf_45bind40223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40523, i64 0)
store %struct.ScmObj* %anf_45bind40223, %struct.ScmObj** %stackaddr$env-ref46993
%stackaddr$prim46994 = alloca %struct.ScmObj*, align 8
%_95k40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %_95k40344, %struct.ScmObj** %stackaddr$prim46994, align 8
%stackaddr$prim46995 = alloca %struct.ScmObj*, align 8
%current_45args46411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46410)
store volatile %struct.ScmObj* %current_45args46411, %struct.ScmObj** %stackaddr$prim46995, align 8
%stackaddr$prim46996 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46411)
store volatile %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$prim46996, align 8
%stackaddr$makeclosure46997 = alloca %struct.ScmObj*, align 8
%fptrToInt46998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40638 to i64
%ae40638 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt46998)
store volatile %struct.ScmObj* %ae40638, %struct.ScmObj** %stackaddr$makeclosure46997, align 8
%args46945$anf_45bind40223$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim46999 = alloca %struct.ScmObj*, align 8
%args46945$anf_45bind40223$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40227, %struct.ScmObj* %args46945$anf_45bind40223$0)
store volatile %struct.ScmObj* %args46945$anf_45bind40223$1, %struct.ScmObj** %stackaddr$prim46999, align 8
%stackaddr$prim47000 = alloca %struct.ScmObj*, align 8
%args46945$anf_45bind40223$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40638, %struct.ScmObj* %args46945$anf_45bind40223$1)
store volatile %struct.ScmObj* %args46945$anf_45bind40223$2, %struct.ScmObj** %stackaddr$prim47000, align 8
%clofunc47001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40223)
musttail call tailcc void %clofunc47001(%struct.ScmObj* %anf_45bind40223, %struct.ScmObj* %args46945$anf_45bind40223$2)
ret void
}

define tailcc void @proc_clo$ae40638(%struct.ScmObj* %env$ae40638,%struct.ScmObj* %current_45args46413) {
%stackaddr$prim47002 = alloca %struct.ScmObj*, align 8
%_95k40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %_95k40345, %struct.ScmObj** %stackaddr$prim47002, align 8
%stackaddr$prim47003 = alloca %struct.ScmObj*, align 8
%current_45args46414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46413)
store volatile %struct.ScmObj* %current_45args46414, %struct.ScmObj** %stackaddr$prim47003, align 8
%stackaddr$prim47004 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46414)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47004, align 8
%stackaddr$makeclosure47005 = alloca %struct.ScmObj*, align 8
%fptrToInt47006 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40640 to i64
%ae40640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47006)
store volatile %struct.ScmObj* %ae40640, %struct.ScmObj** %stackaddr$makeclosure47005, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40640, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40641 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47007 = alloca %struct.ScmObj*, align 8
%fptrToInt47008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40642 to i64
%ae40642 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47008)
store volatile %struct.ScmObj* %ae40642, %struct.ScmObj** %stackaddr$makeclosure47007, align 8
%args46944$ae40640$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47009 = alloca %struct.ScmObj*, align 8
%args46944$ae40640$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40642, %struct.ScmObj* %args46944$ae40640$0)
store volatile %struct.ScmObj* %args46944$ae40640$1, %struct.ScmObj** %stackaddr$prim47009, align 8
%stackaddr$prim47010 = alloca %struct.ScmObj*, align 8
%args46944$ae40640$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40641, %struct.ScmObj* %args46944$ae40640$1)
store volatile %struct.ScmObj* %args46944$ae40640$2, %struct.ScmObj** %stackaddr$prim47010, align 8
%clofunc47011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40640)
musttail call tailcc void %clofunc47011(%struct.ScmObj* %ae40640, %struct.ScmObj* %args46944$ae40640$2)
ret void
}

define tailcc void @proc_clo$ae40640(%struct.ScmObj* %env$ae40640,%struct.ScmObj* %current_45args46416) {
%stackaddr$env-ref47012 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40640, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47012
%stackaddr$prim47013 = alloca %struct.ScmObj*, align 8
%_95k40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46416)
store volatile %struct.ScmObj* %_95k40346, %struct.ScmObj** %stackaddr$prim47013, align 8
%stackaddr$prim47014 = alloca %struct.ScmObj*, align 8
%current_45args46417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46416)
store volatile %struct.ScmObj* %current_45args46417, %struct.ScmObj** %stackaddr$prim47014, align 8
%stackaddr$prim47015 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46417)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim47015, align 8
%stackaddr$makeclosure47016 = alloca %struct.ScmObj*, align 8
%fptrToInt47017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40718 to i64
%ae40718 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47017)
store volatile %struct.ScmObj* %ae40718, %struct.ScmObj** %stackaddr$makeclosure47016, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40718, %struct.ScmObj* %Ycmb40102, i64 0)
%args46928$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47018 = alloca %struct.ScmObj*, align 8
%args46928$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40232, %struct.ScmObj* %args46928$Ycmb40102$0)
store volatile %struct.ScmObj* %args46928$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47018, align 8
%stackaddr$prim47019 = alloca %struct.ScmObj*, align 8
%args46928$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40718, %struct.ScmObj* %args46928$Ycmb40102$1)
store volatile %struct.ScmObj* %args46928$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47019, align 8
%clofunc47020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47020(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46928$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40718(%struct.ScmObj* %env$ae40718,%struct.ScmObj* %current_45args46419) {
%stackaddr$env-ref47021 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40718, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47021
%stackaddr$prim47022 = alloca %struct.ScmObj*, align 8
%_95k40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46419)
store volatile %struct.ScmObj* %_95k40347, %struct.ScmObj** %stackaddr$prim47022, align 8
%stackaddr$prim47023 = alloca %struct.ScmObj*, align 8
%current_45args46420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46419)
store volatile %struct.ScmObj* %current_45args46420, %struct.ScmObj** %stackaddr$prim47023, align 8
%stackaddr$prim47024 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46420)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47024, align 8
%stackaddr$makeclosure47025 = alloca %struct.ScmObj*, align 8
%fptrToInt47026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40720 to i64
%ae40720 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47026)
store volatile %struct.ScmObj* %ae40720, %struct.ScmObj** %stackaddr$makeclosure47025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40720, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40720, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47027 = alloca %struct.ScmObj*, align 8
%fptrToInt47028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40722 to i64
%ae40722 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47028)
store volatile %struct.ScmObj* %ae40722, %struct.ScmObj** %stackaddr$makeclosure47027, align 8
%args46927$ae40720$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47029 = alloca %struct.ScmObj*, align 8
%args46927$ae40720$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40722, %struct.ScmObj* %args46927$ae40720$0)
store volatile %struct.ScmObj* %args46927$ae40720$1, %struct.ScmObj** %stackaddr$prim47029, align 8
%stackaddr$prim47030 = alloca %struct.ScmObj*, align 8
%args46927$ae40720$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40721, %struct.ScmObj* %args46927$ae40720$1)
store volatile %struct.ScmObj* %args46927$ae40720$2, %struct.ScmObj** %stackaddr$prim47030, align 8
%clofunc47031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40720)
musttail call tailcc void %clofunc47031(%struct.ScmObj* %ae40720, %struct.ScmObj* %args46927$ae40720$2)
ret void
}

define tailcc void @proc_clo$ae40720(%struct.ScmObj* %env$ae40720,%struct.ScmObj* %current_45args46422) {
%stackaddr$env-ref47032 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40720, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47032
%stackaddr$env-ref47033 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40720, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47033
%stackaddr$prim47034 = alloca %struct.ScmObj*, align 8
%_95k40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46422)
store volatile %struct.ScmObj* %_95k40348, %struct.ScmObj** %stackaddr$prim47034, align 8
%stackaddr$prim47035 = alloca %struct.ScmObj*, align 8
%current_45args46423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46422)
store volatile %struct.ScmObj* %current_45args46423, %struct.ScmObj** %stackaddr$prim47035, align 8
%stackaddr$prim47036 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46423)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim47036, align 8
%stackaddr$makeclosure47037 = alloca %struct.ScmObj*, align 8
%fptrToInt47038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40815 to i64
%ae40815 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47038)
store volatile %struct.ScmObj* %ae40815, %struct.ScmObj** %stackaddr$makeclosure47037, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40815, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40815, %struct.ScmObj* %_37foldr140123, i64 1)
%args46908$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47039 = alloca %struct.ScmObj*, align 8
%args46908$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args46908$Ycmb40102$0)
store volatile %struct.ScmObj* %args46908$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47039, align 8
%stackaddr$prim47040 = alloca %struct.ScmObj*, align 8
%args46908$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40815, %struct.ScmObj* %args46908$Ycmb40102$1)
store volatile %struct.ScmObj* %args46908$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47040, align 8
%clofunc47041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47041(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46908$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40815(%struct.ScmObj* %env$ae40815,%struct.ScmObj* %current_45args46425) {
%stackaddr$env-ref47042 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40815, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47042
%stackaddr$env-ref47043 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40815, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47043
%stackaddr$prim47044 = alloca %struct.ScmObj*, align 8
%_95k40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46425)
store volatile %struct.ScmObj* %_95k40349, %struct.ScmObj** %stackaddr$prim47044, align 8
%stackaddr$prim47045 = alloca %struct.ScmObj*, align 8
%current_45args46426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46425)
store volatile %struct.ScmObj* %current_45args46426, %struct.ScmObj** %stackaddr$prim47045, align 8
%stackaddr$prim47046 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46426)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47046, align 8
%stackaddr$makeclosure47047 = alloca %struct.ScmObj*, align 8
%fptrToInt47048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40817 to i64
%ae40817 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47048)
store volatile %struct.ScmObj* %ae40817, %struct.ScmObj** %stackaddr$makeclosure47047, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40817, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40817, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40817, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40818 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47049 = alloca %struct.ScmObj*, align 8
%fptrToInt47050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40819 to i64
%ae40819 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47050)
store volatile %struct.ScmObj* %ae40819, %struct.ScmObj** %stackaddr$makeclosure47049, align 8
%args46907$ae40817$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47051 = alloca %struct.ScmObj*, align 8
%args46907$ae40817$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40819, %struct.ScmObj* %args46907$ae40817$0)
store volatile %struct.ScmObj* %args46907$ae40817$1, %struct.ScmObj** %stackaddr$prim47051, align 8
%stackaddr$prim47052 = alloca %struct.ScmObj*, align 8
%args46907$ae40817$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40818, %struct.ScmObj* %args46907$ae40817$1)
store volatile %struct.ScmObj* %args46907$ae40817$2, %struct.ScmObj** %stackaddr$prim47052, align 8
%clofunc47053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40817)
musttail call tailcc void %clofunc47053(%struct.ScmObj* %ae40817, %struct.ScmObj* %args46907$ae40817$2)
ret void
}

define tailcc void @proc_clo$ae40817(%struct.ScmObj* %env$ae40817,%struct.ScmObj* %current_45args46428) {
%stackaddr$env-ref47054 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40817, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47054
%stackaddr$env-ref47055 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40817, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47055
%stackaddr$env-ref47056 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40817, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47056
%stackaddr$prim47057 = alloca %struct.ScmObj*, align 8
%_95k40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46428)
store volatile %struct.ScmObj* %_95k40350, %struct.ScmObj** %stackaddr$prim47057, align 8
%stackaddr$prim47058 = alloca %struct.ScmObj*, align 8
%current_45args46429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46428)
store volatile %struct.ScmObj* %current_45args46429, %struct.ScmObj** %stackaddr$prim47058, align 8
%stackaddr$prim47059 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46429)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim47059, align 8
%stackaddr$makeclosure47060 = alloca %struct.ScmObj*, align 8
%fptrToInt47061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40965 to i64
%ae40965 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47061)
store volatile %struct.ScmObj* %ae40965, %struct.ScmObj** %stackaddr$makeclosure47060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40965, %struct.ScmObj* %_37foldr140123, i64 2)
%args46891$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47062 = alloca %struct.ScmObj*, align 8
%args46891$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %args46891$Ycmb40102$0)
store volatile %struct.ScmObj* %args46891$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47062, align 8
%stackaddr$prim47063 = alloca %struct.ScmObj*, align 8
%args46891$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40965, %struct.ScmObj* %args46891$Ycmb40102$1)
store volatile %struct.ScmObj* %args46891$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47063, align 8
%clofunc47064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47064(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46891$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40965(%struct.ScmObj* %env$ae40965,%struct.ScmObj* %current_45args46431) {
%stackaddr$env-ref47065 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47065
%stackaddr$env-ref47066 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47066
%stackaddr$env-ref47067 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40965, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47067
%stackaddr$prim47068 = alloca %struct.ScmObj*, align 8
%_95k40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46431)
store volatile %struct.ScmObj* %_95k40351, %struct.ScmObj** %stackaddr$prim47068, align 8
%stackaddr$prim47069 = alloca %struct.ScmObj*, align 8
%current_45args46432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46431)
store volatile %struct.ScmObj* %current_45args46432, %struct.ScmObj** %stackaddr$prim47069, align 8
%stackaddr$prim47070 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46432)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47070, align 8
%stackaddr$makeclosure47071 = alloca %struct.ScmObj*, align 8
%fptrToInt47072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40967 to i64
%ae40967 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47072)
store volatile %struct.ScmObj* %ae40967, %struct.ScmObj** %stackaddr$makeclosure47071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40967, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40967, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40967, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40967, %struct.ScmObj* %_37foldr140123, i64 3)
%ae40968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47073 = alloca %struct.ScmObj*, align 8
%fptrToInt47074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40969 to i64
%ae40969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47074)
store volatile %struct.ScmObj* %ae40969, %struct.ScmObj** %stackaddr$makeclosure47073, align 8
%args46890$ae40967$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47075 = alloca %struct.ScmObj*, align 8
%args46890$ae40967$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40969, %struct.ScmObj* %args46890$ae40967$0)
store volatile %struct.ScmObj* %args46890$ae40967$1, %struct.ScmObj** %stackaddr$prim47075, align 8
%stackaddr$prim47076 = alloca %struct.ScmObj*, align 8
%args46890$ae40967$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40968, %struct.ScmObj* %args46890$ae40967$1)
store volatile %struct.ScmObj* %args46890$ae40967$2, %struct.ScmObj** %stackaddr$prim47076, align 8
%clofunc47077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40967)
musttail call tailcc void %clofunc47077(%struct.ScmObj* %ae40967, %struct.ScmObj* %args46890$ae40967$2)
ret void
}

define tailcc void @proc_clo$ae40967(%struct.ScmObj* %env$ae40967,%struct.ScmObj* %current_45args46434) {
%stackaddr$env-ref47078 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40967, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47078
%stackaddr$env-ref47079 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40967, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47079
%stackaddr$env-ref47080 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40967, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47080
%stackaddr$env-ref47081 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40967, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47081
%stackaddr$prim47082 = alloca %struct.ScmObj*, align 8
%_95k40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %_95k40352, %struct.ScmObj** %stackaddr$prim47082, align 8
%stackaddr$prim47083 = alloca %struct.ScmObj*, align 8
%current_45args46435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46434)
store volatile %struct.ScmObj* %current_45args46435, %struct.ScmObj** %stackaddr$prim47083, align 8
%stackaddr$prim47084 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46435)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim47084, align 8
%stackaddr$makeclosure47085 = alloca %struct.ScmObj*, align 8
%fptrToInt47086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41048 to i64
%ae41048 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47086)
store volatile %struct.ScmObj* %ae41048, %struct.ScmObj** %stackaddr$makeclosure47085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41048, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41048, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41048, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41048, %struct.ScmObj* %_37foldr140123, i64 3)
%args46876$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47087 = alloca %struct.ScmObj*, align 8
%args46876$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args46876$Ycmb40102$0)
store volatile %struct.ScmObj* %args46876$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47087, align 8
%stackaddr$prim47088 = alloca %struct.ScmObj*, align 8
%args46876$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41048, %struct.ScmObj* %args46876$Ycmb40102$1)
store volatile %struct.ScmObj* %args46876$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47088, align 8
%clofunc47089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47089(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46876$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41048(%struct.ScmObj* %env$ae41048,%struct.ScmObj* %current_45args46437) {
%stackaddr$env-ref47090 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41048, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47090
%stackaddr$env-ref47091 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41048, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47091
%stackaddr$env-ref47092 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41048, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47092
%stackaddr$env-ref47093 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41048, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47093
%stackaddr$prim47094 = alloca %struct.ScmObj*, align 8
%_95k40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46437)
store volatile %struct.ScmObj* %_95k40353, %struct.ScmObj** %stackaddr$prim47094, align 8
%stackaddr$prim47095 = alloca %struct.ScmObj*, align 8
%current_45args46438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46437)
store volatile %struct.ScmObj* %current_45args46438, %struct.ScmObj** %stackaddr$prim47095, align 8
%stackaddr$prim47096 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46438)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47096, align 8
%stackaddr$makeclosure47097 = alloca %struct.ScmObj*, align 8
%fptrToInt47098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41050 to i64
%ae41050 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47098)
store volatile %struct.ScmObj* %ae41050, %struct.ScmObj** %stackaddr$makeclosure47097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41050, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41051 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47099 = alloca %struct.ScmObj*, align 8
%fptrToInt47100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41052 to i64
%ae41052 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47100)
store volatile %struct.ScmObj* %ae41052, %struct.ScmObj** %stackaddr$makeclosure47099, align 8
%args46875$ae41050$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47101 = alloca %struct.ScmObj*, align 8
%args46875$ae41050$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41052, %struct.ScmObj* %args46875$ae41050$0)
store volatile %struct.ScmObj* %args46875$ae41050$1, %struct.ScmObj** %stackaddr$prim47101, align 8
%stackaddr$prim47102 = alloca %struct.ScmObj*, align 8
%args46875$ae41050$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41051, %struct.ScmObj* %args46875$ae41050$1)
store volatile %struct.ScmObj* %args46875$ae41050$2, %struct.ScmObj** %stackaddr$prim47102, align 8
%clofunc47103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41050)
musttail call tailcc void %clofunc47103(%struct.ScmObj* %ae41050, %struct.ScmObj* %args46875$ae41050$2)
ret void
}

define tailcc void @proc_clo$ae41050(%struct.ScmObj* %env$ae41050,%struct.ScmObj* %current_45args46440) {
%stackaddr$env-ref47104 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47104
%stackaddr$env-ref47105 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47105
%stackaddr$env-ref47106 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47106
%stackaddr$env-ref47107 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47107
%stackaddr$env-ref47108 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41050, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47108
%stackaddr$prim47109 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46440)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim47109, align 8
%stackaddr$prim47110 = alloca %struct.ScmObj*, align 8
%current_45args46441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46440)
store volatile %struct.ScmObj* %current_45args46441, %struct.ScmObj** %stackaddr$prim47110, align 8
%stackaddr$prim47111 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46441)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim47111, align 8
%stackaddr$makeclosure47112 = alloca %struct.ScmObj*, align 8
%fptrToInt47113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41127 to i64
%ae41127 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47113)
store volatile %struct.ScmObj* %ae41127, %struct.ScmObj** %stackaddr$makeclosure47112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41127, %struct.ScmObj* %_37foldr140123, i64 4)
%args46859$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47114 = alloca %struct.ScmObj*, align 8
%args46859$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args46859$Ycmb40102$0)
store volatile %struct.ScmObj* %args46859$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47114, align 8
%stackaddr$prim47115 = alloca %struct.ScmObj*, align 8
%args46859$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41127, %struct.ScmObj* %args46859$Ycmb40102$1)
store volatile %struct.ScmObj* %args46859$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47115, align 8
%clofunc47116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47116(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46859$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41127(%struct.ScmObj* %env$ae41127,%struct.ScmObj* %current_45args46443) {
%stackaddr$env-ref47117 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47117
%stackaddr$env-ref47118 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47118
%stackaddr$env-ref47119 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47119
%stackaddr$env-ref47120 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47120
%stackaddr$env-ref47121 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41127, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47121
%stackaddr$prim47122 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46443)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim47122, align 8
%stackaddr$prim47123 = alloca %struct.ScmObj*, align 8
%current_45args46444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46443)
store volatile %struct.ScmObj* %current_45args46444, %struct.ScmObj** %stackaddr$prim47123, align 8
%stackaddr$prim47124 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46444)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47124, align 8
%stackaddr$makeclosure47125 = alloca %struct.ScmObj*, align 8
%fptrToInt47126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41129 to i64
%ae41129 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47126)
store volatile %struct.ScmObj* %ae41129, %struct.ScmObj** %stackaddr$makeclosure47125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37length40112, i64 5)
%ae41130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47127 = alloca %struct.ScmObj*, align 8
%fptrToInt47128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41131 to i64
%ae41131 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47128)
store volatile %struct.ScmObj* %ae41131, %struct.ScmObj** %stackaddr$makeclosure47127, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41131, %struct.ScmObj* %_37foldl140107, i64 0)
%args46858$ae41129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47129 = alloca %struct.ScmObj*, align 8
%args46858$ae41129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41131, %struct.ScmObj* %args46858$ae41129$0)
store volatile %struct.ScmObj* %args46858$ae41129$1, %struct.ScmObj** %stackaddr$prim47129, align 8
%stackaddr$prim47130 = alloca %struct.ScmObj*, align 8
%args46858$ae41129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41130, %struct.ScmObj* %args46858$ae41129$1)
store volatile %struct.ScmObj* %args46858$ae41129$2, %struct.ScmObj** %stackaddr$prim47130, align 8
%clofunc47131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41129)
musttail call tailcc void %clofunc47131(%struct.ScmObj* %ae41129, %struct.ScmObj* %args46858$ae41129$2)
ret void
}

define tailcc void @proc_clo$ae41129(%struct.ScmObj* %env$ae41129,%struct.ScmObj* %current_45args46446) {
%stackaddr$env-ref47132 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47132
%stackaddr$env-ref47133 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47133
%stackaddr$env-ref47134 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47134
%stackaddr$env-ref47135 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47135
%stackaddr$env-ref47136 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47136
%stackaddr$env-ref47137 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47137
%stackaddr$prim47138 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim47138, align 8
%stackaddr$prim47139 = alloca %struct.ScmObj*, align 8
%current_45args46447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46446)
store volatile %struct.ScmObj* %current_45args46447, %struct.ScmObj** %stackaddr$prim47139, align 8
%stackaddr$prim47140 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46447)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47140, align 8
%stackaddr$makeclosure47141 = alloca %struct.ScmObj*, align 8
%fptrToInt47142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41183 to i64
%ae41183 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47142)
store volatile %struct.ScmObj* %ae41183, %struct.ScmObj** %stackaddr$makeclosure47141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41183, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41183, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41183, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41183, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41183, %struct.ScmObj* %_37last40145, i64 4)
%ae41184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47143 = alloca %struct.ScmObj*, align 8
%fptrToInt47144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41185 to i64
%ae41185 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47144)
store volatile %struct.ScmObj* %ae41185, %struct.ScmObj** %stackaddr$makeclosure47143, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37length40112, i64 1)
%args46844$ae41183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47145 = alloca %struct.ScmObj*, align 8
%args46844$ae41183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41185, %struct.ScmObj* %args46844$ae41183$0)
store volatile %struct.ScmObj* %args46844$ae41183$1, %struct.ScmObj** %stackaddr$prim47145, align 8
%stackaddr$prim47146 = alloca %struct.ScmObj*, align 8
%args46844$ae41183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41184, %struct.ScmObj* %args46844$ae41183$1)
store volatile %struct.ScmObj* %args46844$ae41183$2, %struct.ScmObj** %stackaddr$prim47146, align 8
%clofunc47147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41183)
musttail call tailcc void %clofunc47147(%struct.ScmObj* %ae41183, %struct.ScmObj* %args46844$ae41183$2)
ret void
}

define tailcc void @proc_clo$ae41183(%struct.ScmObj* %env$ae41183,%struct.ScmObj* %current_45args46449) {
%stackaddr$env-ref47148 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41183, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47148
%stackaddr$env-ref47149 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41183, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47149
%stackaddr$env-ref47150 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41183, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47150
%stackaddr$env-ref47151 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41183, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47151
%stackaddr$env-ref47152 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41183, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47152
%stackaddr$prim47153 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim47153, align 8
%stackaddr$prim47154 = alloca %struct.ScmObj*, align 8
%current_45args46450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46449)
store volatile %struct.ScmObj* %current_45args46450, %struct.ScmObj** %stackaddr$prim47154, align 8
%stackaddr$prim47155 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46450)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47155, align 8
%stackaddr$makeclosure47156 = alloca %struct.ScmObj*, align 8
%fptrToInt47157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41213 to i64
%ae41213 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47157)
store volatile %struct.ScmObj* %ae41213, %struct.ScmObj** %stackaddr$makeclosure47156, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41213, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41214 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47158 = alloca %struct.ScmObj*, align 8
%fptrToInt47159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41215 to i64
%ae41215 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47159)
store volatile %struct.ScmObj* %ae41215, %struct.ScmObj** %stackaddr$makeclosure47158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41215, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41215, %struct.ScmObj* %_37foldr140123, i64 1)
%args46834$ae41213$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47160 = alloca %struct.ScmObj*, align 8
%args46834$ae41213$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41215, %struct.ScmObj* %args46834$ae41213$0)
store volatile %struct.ScmObj* %args46834$ae41213$1, %struct.ScmObj** %stackaddr$prim47160, align 8
%stackaddr$prim47161 = alloca %struct.ScmObj*, align 8
%args46834$ae41213$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41214, %struct.ScmObj* %args46834$ae41213$1)
store volatile %struct.ScmObj* %args46834$ae41213$2, %struct.ScmObj** %stackaddr$prim47161, align 8
%clofunc47162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41213)
musttail call tailcc void %clofunc47162(%struct.ScmObj* %ae41213, %struct.ScmObj* %args46834$ae41213$2)
ret void
}

define tailcc void @proc_clo$ae41213(%struct.ScmObj* %env$ae41213,%struct.ScmObj* %current_45args46452) {
%stackaddr$env-ref47163 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47163
%stackaddr$env-ref47164 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47164
%stackaddr$env-ref47165 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47165
%stackaddr$env-ref47166 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47166
%stackaddr$env-ref47167 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41213, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47167
%stackaddr$prim47168 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47168, align 8
%stackaddr$prim47169 = alloca %struct.ScmObj*, align 8
%current_45args46453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46452)
store volatile %struct.ScmObj* %current_45args46453, %struct.ScmObj** %stackaddr$prim47169, align 8
%stackaddr$prim47170 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46453)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim47170, align 8
%stackaddr$makeclosure47171 = alloca %struct.ScmObj*, align 8
%fptrToInt47172 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41597 to i64
%ae41597 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47172)
store volatile %struct.ScmObj* %ae41597, %struct.ScmObj** %stackaddr$makeclosure47171, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41597, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41597, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41597, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41597, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41597, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args46774$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47173 = alloca %struct.ScmObj*, align 8
%args46774$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args46774$Ycmb40102$0)
store volatile %struct.ScmObj* %args46774$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47173, align 8
%stackaddr$prim47174 = alloca %struct.ScmObj*, align 8
%args46774$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41597, %struct.ScmObj* %args46774$Ycmb40102$1)
store volatile %struct.ScmObj* %args46774$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47174, align 8
%clofunc47175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47175(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46774$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41597(%struct.ScmObj* %env$ae41597,%struct.ScmObj* %current_45args46455) {
%stackaddr$env-ref47176 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41597, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47176
%stackaddr$env-ref47177 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41597, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47177
%stackaddr$env-ref47178 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41597, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47178
%stackaddr$env-ref47179 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41597, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47179
%stackaddr$env-ref47180 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41597, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47180
%stackaddr$prim47181 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47181, align 8
%stackaddr$prim47182 = alloca %struct.ScmObj*, align 8
%current_45args46456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46455)
store volatile %struct.ScmObj* %current_45args46456, %struct.ScmObj** %stackaddr$prim47182, align 8
%stackaddr$prim47183 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46456)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47183, align 8
%stackaddr$makeclosure47184 = alloca %struct.ScmObj*, align 8
%fptrToInt47185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41599 to i64
%ae41599 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47185)
store volatile %struct.ScmObj* %ae41599, %struct.ScmObj** %stackaddr$makeclosure47184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41599, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41600 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47186 = alloca %struct.ScmObj*, align 8
%fptrToInt47187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41601 to i64
%ae41601 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47187)
store volatile %struct.ScmObj* %ae41601, %struct.ScmObj** %stackaddr$makeclosure47186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41601, %struct.ScmObj* %_37foldr140123, i64 0)
%args46773$ae41599$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47188 = alloca %struct.ScmObj*, align 8
%args46773$ae41599$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41601, %struct.ScmObj* %args46773$ae41599$0)
store volatile %struct.ScmObj* %args46773$ae41599$1, %struct.ScmObj** %stackaddr$prim47188, align 8
%stackaddr$prim47189 = alloca %struct.ScmObj*, align 8
%args46773$ae41599$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41600, %struct.ScmObj* %args46773$ae41599$1)
store volatile %struct.ScmObj* %args46773$ae41599$2, %struct.ScmObj** %stackaddr$prim47189, align 8
%clofunc47190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41599)
musttail call tailcc void %clofunc47190(%struct.ScmObj* %ae41599, %struct.ScmObj* %args46773$ae41599$2)
ret void
}

define tailcc void @proc_clo$ae41599(%struct.ScmObj* %env$ae41599,%struct.ScmObj* %current_45args46458) {
%stackaddr$env-ref47191 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47191
%stackaddr$env-ref47192 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47192
%stackaddr$env-ref47193 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47193
%stackaddr$env-ref47194 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47194
%stackaddr$env-ref47195 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47195
%stackaddr$env-ref47196 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41599, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47196
%stackaddr$prim47197 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47197, align 8
%stackaddr$prim47198 = alloca %struct.ScmObj*, align 8
%current_45args46459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46458)
store volatile %struct.ScmObj* %current_45args46459, %struct.ScmObj** %stackaddr$prim47198, align 8
%stackaddr$prim47199 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46459)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47199, align 8
%stackaddr$makeclosure47200 = alloca %struct.ScmObj*, align 8
%fptrToInt47201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41676 to i64
%ae41676 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47201)
store volatile %struct.ScmObj* %ae41676, %struct.ScmObj** %stackaddr$makeclosure47200, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41676, %struct.ScmObj* %_37map140154, i64 4)
%ae41677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47202 = alloca %struct.ScmObj*, align 8
%fptrToInt47203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41678 to i64
%ae41678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47203)
store volatile %struct.ScmObj* %ae41678, %struct.ScmObj** %stackaddr$makeclosure47202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41678, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args46754$ae41676$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47204 = alloca %struct.ScmObj*, align 8
%args46754$ae41676$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41678, %struct.ScmObj* %args46754$ae41676$0)
store volatile %struct.ScmObj* %args46754$ae41676$1, %struct.ScmObj** %stackaddr$prim47204, align 8
%stackaddr$prim47205 = alloca %struct.ScmObj*, align 8
%args46754$ae41676$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41677, %struct.ScmObj* %args46754$ae41676$1)
store volatile %struct.ScmObj* %args46754$ae41676$2, %struct.ScmObj** %stackaddr$prim47205, align 8
%clofunc47206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41676)
musttail call tailcc void %clofunc47206(%struct.ScmObj* %ae41676, %struct.ScmObj* %args46754$ae41676$2)
ret void
}

define tailcc void @proc_clo$ae41676(%struct.ScmObj* %env$ae41676,%struct.ScmObj* %current_45args46461) {
%stackaddr$env-ref47207 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47207
%stackaddr$env-ref47208 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47208
%stackaddr$env-ref47209 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47209
%stackaddr$env-ref47210 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47210
%stackaddr$env-ref47211 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41676, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47211
%stackaddr$prim47212 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47212, align 8
%stackaddr$prim47213 = alloca %struct.ScmObj*, align 8
%current_45args46462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46461)
store volatile %struct.ScmObj* %current_45args46462, %struct.ScmObj** %stackaddr$prim47213, align 8
%stackaddr$prim47214 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46462)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47214, align 8
%stackaddr$makeclosure47215 = alloca %struct.ScmObj*, align 8
%fptrToInt47216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41822 to i64
%ae41822 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47216)
store volatile %struct.ScmObj* %ae41822, %struct.ScmObj** %stackaddr$makeclosure47215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41822, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41822, %struct.ScmObj* %_37foldl140107, i64 1)
%ae41823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47217 = alloca %struct.ScmObj*, align 8
%fptrToInt47218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41824 to i64
%ae41824 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47218)
store volatile %struct.ScmObj* %ae41824, %struct.ScmObj** %stackaddr$makeclosure47217, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41824, %struct.ScmObj* %_37map140154, i64 2)
%args46737$ae41822$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47219 = alloca %struct.ScmObj*, align 8
%args46737$ae41822$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41824, %struct.ScmObj* %args46737$ae41822$0)
store volatile %struct.ScmObj* %args46737$ae41822$1, %struct.ScmObj** %stackaddr$prim47219, align 8
%stackaddr$prim47220 = alloca %struct.ScmObj*, align 8
%args46737$ae41822$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41823, %struct.ScmObj* %args46737$ae41822$1)
store volatile %struct.ScmObj* %args46737$ae41822$2, %struct.ScmObj** %stackaddr$prim47220, align 8
%clofunc47221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41822)
musttail call tailcc void %clofunc47221(%struct.ScmObj* %ae41822, %struct.ScmObj* %args46737$ae41822$2)
ret void
}

define tailcc void @proc_clo$ae41822(%struct.ScmObj* %env$ae41822,%struct.ScmObj* %current_45args46464) {
%stackaddr$env-ref47222 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41822, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47222
%stackaddr$env-ref47223 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41822, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47223
%stackaddr$prim47224 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47224, align 8
%stackaddr$prim47225 = alloca %struct.ScmObj*, align 8
%current_45args46465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46464)
store volatile %struct.ScmObj* %current_45args46465, %struct.ScmObj** %stackaddr$prim47225, align 8
%stackaddr$prim47226 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46465)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim47226, align 8
%stackaddr$makeclosure47227 = alloca %struct.ScmObj*, align 8
%fptrToInt47228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42214 to i64
%ae42214 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47228)
store volatile %struct.ScmObj* %ae42214, %struct.ScmObj** %stackaddr$makeclosure47227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42214, %struct.ScmObj* %_37foldl140107, i64 0)
%args46677$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47229 = alloca %struct.ScmObj*, align 8
%args46677$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args46677$Ycmb40102$0)
store volatile %struct.ScmObj* %args46677$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47229, align 8
%stackaddr$prim47230 = alloca %struct.ScmObj*, align 8
%args46677$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42214, %struct.ScmObj* %args46677$Ycmb40102$1)
store volatile %struct.ScmObj* %args46677$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47230, align 8
%clofunc47231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47231(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args46677$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42214(%struct.ScmObj* %env$ae42214,%struct.ScmObj* %current_45args46467) {
%stackaddr$env-ref47232 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42214, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47232
%stackaddr$prim47233 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47233, align 8
%stackaddr$prim47234 = alloca %struct.ScmObj*, align 8
%current_45args46468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46467)
store volatile %struct.ScmObj* %current_45args46468, %struct.ScmObj** %stackaddr$prim47234, align 8
%stackaddr$prim47235 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46468)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47235, align 8
%stackaddr$makeclosure47236 = alloca %struct.ScmObj*, align 8
%fptrToInt47237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42216 to i64
%ae42216 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47237)
store volatile %struct.ScmObj* %ae42216, %struct.ScmObj** %stackaddr$makeclosure47236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42216, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42217 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47238 = alloca %struct.ScmObj*, align 8
%fptrToInt47239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42218 to i64
%ae42218 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47239)
store volatile %struct.ScmObj* %ae42218, %struct.ScmObj** %stackaddr$makeclosure47238, align 8
%args46676$ae42216$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47240 = alloca %struct.ScmObj*, align 8
%args46676$ae42216$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42218, %struct.ScmObj* %args46676$ae42216$0)
store volatile %struct.ScmObj* %args46676$ae42216$1, %struct.ScmObj** %stackaddr$prim47240, align 8
%stackaddr$prim47241 = alloca %struct.ScmObj*, align 8
%args46676$ae42216$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42217, %struct.ScmObj* %args46676$ae42216$1)
store volatile %struct.ScmObj* %args46676$ae42216$2, %struct.ScmObj** %stackaddr$prim47241, align 8
%clofunc47242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42216)
musttail call tailcc void %clofunc47242(%struct.ScmObj* %ae42216, %struct.ScmObj* %args46676$ae42216$2)
ret void
}

define tailcc void @proc_clo$ae42216(%struct.ScmObj* %env$ae42216,%struct.ScmObj* %current_45args46470) {
%stackaddr$env-ref47243 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42216, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47243
%stackaddr$prim47244 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47244, align 8
%stackaddr$prim47245 = alloca %struct.ScmObj*, align 8
%current_45args46471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46470)
store volatile %struct.ScmObj* %current_45args46471, %struct.ScmObj** %stackaddr$prim47245, align 8
%stackaddr$prim47246 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46471)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47246, align 8
%stackaddr$makeclosure47247 = alloca %struct.ScmObj*, align 8
%fptrToInt47248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42240 to i64
%ae42240 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47248)
store volatile %struct.ScmObj* %ae42240, %struct.ScmObj** %stackaddr$makeclosure47247, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42240, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47249 = alloca %struct.ScmObj*, align 8
%fptrToInt47250 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42242 to i64
%ae42242 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47250)
store volatile %struct.ScmObj* %ae42242, %struct.ScmObj** %stackaddr$makeclosure47249, align 8
%args46670$ae42240$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47251 = alloca %struct.ScmObj*, align 8
%args46670$ae42240$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42242, %struct.ScmObj* %args46670$ae42240$0)
store volatile %struct.ScmObj* %args46670$ae42240$1, %struct.ScmObj** %stackaddr$prim47251, align 8
%stackaddr$prim47252 = alloca %struct.ScmObj*, align 8
%args46670$ae42240$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42241, %struct.ScmObj* %args46670$ae42240$1)
store volatile %struct.ScmObj* %args46670$ae42240$2, %struct.ScmObj** %stackaddr$prim47252, align 8
%clofunc47253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42240)
musttail call tailcc void %clofunc47253(%struct.ScmObj* %ae42240, %struct.ScmObj* %args46670$ae42240$2)
ret void
}

define tailcc void @proc_clo$ae42240(%struct.ScmObj* %env$ae42240,%struct.ScmObj* %current_45args46473) {
%stackaddr$env-ref47254 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42240, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47254
%stackaddr$prim47255 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47255, align 8
%stackaddr$prim47256 = alloca %struct.ScmObj*, align 8
%current_45args46474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46473)
store volatile %struct.ScmObj* %current_45args46474, %struct.ScmObj** %stackaddr$prim47256, align 8
%stackaddr$prim47257 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46474)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47257, align 8
%ae42264 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42265 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47258 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42264, %struct.ScmObj* %ae42265)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47258, align 8
%stackaddr$makeclosure47259 = alloca %struct.ScmObj*, align 8
%fptrToInt47260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42266 to i64
%ae42266 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47260)
store volatile %struct.ScmObj* %ae42266, %struct.ScmObj** %stackaddr$makeclosure47259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42266, %struct.ScmObj* %_37append40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42266, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47261 = alloca %struct.ScmObj*, align 8
%fptrToInt47262 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42268 to i64
%ae42268 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47262)
store volatile %struct.ScmObj* %ae42268, %struct.ScmObj** %stackaddr$makeclosure47261, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42268, %struct.ScmObj* %_37append40195, i64 0)
%args46664$ae42266$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47263 = alloca %struct.ScmObj*, align 8
%args46664$ae42266$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42268, %struct.ScmObj* %args46664$ae42266$0)
store volatile %struct.ScmObj* %args46664$ae42266$1, %struct.ScmObj** %stackaddr$prim47263, align 8
%stackaddr$prim47264 = alloca %struct.ScmObj*, align 8
%args46664$ae42266$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42267, %struct.ScmObj* %args46664$ae42266$1)
store volatile %struct.ScmObj* %args46664$ae42266$2, %struct.ScmObj** %stackaddr$prim47264, align 8
%clofunc47265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42266)
musttail call tailcc void %clofunc47265(%struct.ScmObj* %ae42266, %struct.ScmObj* %args46664$ae42266$2)
ret void
}

define tailcc void @proc_clo$ae42266(%struct.ScmObj* %env$ae42266,%struct.ScmObj* %current_45args46476) {
%stackaddr$env-ref47266 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42266, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47266
%stackaddr$env-ref47267 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42266, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47267
%stackaddr$prim47268 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47268, align 8
%stackaddr$prim47269 = alloca %struct.ScmObj*, align 8
%current_45args46477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46476)
store volatile %struct.ScmObj* %current_45args46477, %struct.ScmObj** %stackaddr$prim47269, align 8
%stackaddr$prim47270 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46477)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim47270, align 8
%ae42334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47271 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42334, %struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47271, align 8
%ae42337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47272 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42337)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47272, align 8
%stackaddr$makeclosure47273 = alloca %struct.ScmObj*, align 8
%fptrToInt47274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42338 to i64
%ae42338 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47274)
store volatile %struct.ScmObj* %ae42338, %struct.ScmObj** %stackaddr$makeclosure47273, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42338, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42339 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47275 = alloca %struct.ScmObj*, align 8
%fptrToInt47276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42340 to i64
%ae42340 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47276)
store volatile %struct.ScmObj* %ae42340, %struct.ScmObj** %stackaddr$makeclosure47275, align 8
%args46653$ae42338$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47277 = alloca %struct.ScmObj*, align 8
%args46653$ae42338$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42340, %struct.ScmObj* %args46653$ae42338$0)
store volatile %struct.ScmObj* %args46653$ae42338$1, %struct.ScmObj** %stackaddr$prim47277, align 8
%stackaddr$prim47278 = alloca %struct.ScmObj*, align 8
%args46653$ae42338$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42339, %struct.ScmObj* %args46653$ae42338$1)
store volatile %struct.ScmObj* %args46653$ae42338$2, %struct.ScmObj** %stackaddr$prim47278, align 8
%clofunc47279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42338)
musttail call tailcc void %clofunc47279(%struct.ScmObj* %ae42338, %struct.ScmObj* %args46653$ae42338$2)
ret void
}

define tailcc void @proc_clo$ae42338(%struct.ScmObj* %env$ae42338,%struct.ScmObj* %current_45args46479) {
%stackaddr$env-ref47280 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42338, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47280
%stackaddr$prim47281 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47281, align 8
%stackaddr$prim47282 = alloca %struct.ScmObj*, align 8
%current_45args46480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46479)
store volatile %struct.ScmObj* %current_45args46480, %struct.ScmObj** %stackaddr$prim47282, align 8
%stackaddr$prim47283 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46480)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47283, align 8
%stackaddr$makeclosure47284 = alloca %struct.ScmObj*, align 8
%fptrToInt47285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42754 to i64
%ae42754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47285)
store volatile %struct.ScmObj* %ae42754, %struct.ScmObj** %stackaddr$makeclosure47284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42754, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42755 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47286 = alloca %struct.ScmObj*, align 8
%fptrToInt47287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42756 to i64
%ae42756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47287)
store volatile %struct.ScmObj* %ae42756, %struct.ScmObj** %stackaddr$makeclosure47286, align 8
%args46628$ae42754$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47288 = alloca %struct.ScmObj*, align 8
%args46628$ae42754$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42756, %struct.ScmObj* %args46628$ae42754$0)
store volatile %struct.ScmObj* %args46628$ae42754$1, %struct.ScmObj** %stackaddr$prim47288, align 8
%stackaddr$prim47289 = alloca %struct.ScmObj*, align 8
%args46628$ae42754$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42755, %struct.ScmObj* %args46628$ae42754$1)
store volatile %struct.ScmObj* %args46628$ae42754$2, %struct.ScmObj** %stackaddr$prim47289, align 8
%clofunc47290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42754)
musttail call tailcc void %clofunc47290(%struct.ScmObj* %ae42754, %struct.ScmObj* %args46628$ae42754$2)
ret void
}

define tailcc void @proc_clo$ae42754(%struct.ScmObj* %env$ae42754,%struct.ScmObj* %current_45args46482) {
%stackaddr$env-ref47291 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42754, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47291
%stackaddr$prim47292 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47292, align 8
%stackaddr$prim47293 = alloca %struct.ScmObj*, align 8
%current_45args46483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46482)
store volatile %struct.ScmObj* %current_45args46483, %struct.ScmObj** %stackaddr$prim47293, align 8
%stackaddr$prim47294 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46483)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47294, align 8
%stackaddr$makeclosure47295 = alloca %struct.ScmObj*, align 8
%fptrToInt47296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43290 to i64
%ae43290 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47296)
store volatile %struct.ScmObj* %ae43290, %struct.ScmObj** %stackaddr$makeclosure47295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43290, %struct.ScmObj* %_37foldl140107, i64 0)
%ae43291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47297 = alloca %struct.ScmObj*, align 8
%fptrToInt47298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43292 to i64
%ae43292 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47298)
store volatile %struct.ScmObj* %ae43292, %struct.ScmObj** %stackaddr$makeclosure47297, align 8
%args46604$ae43290$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47299 = alloca %struct.ScmObj*, align 8
%args46604$ae43290$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43292, %struct.ScmObj* %args46604$ae43290$0)
store volatile %struct.ScmObj* %args46604$ae43290$1, %struct.ScmObj** %stackaddr$prim47299, align 8
%stackaddr$prim47300 = alloca %struct.ScmObj*, align 8
%args46604$ae43290$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43291, %struct.ScmObj* %args46604$ae43290$1)
store volatile %struct.ScmObj* %args46604$ae43290$2, %struct.ScmObj** %stackaddr$prim47300, align 8
%clofunc47301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43290)
musttail call tailcc void %clofunc47301(%struct.ScmObj* %ae43290, %struct.ScmObj* %args46604$ae43290$2)
ret void
}

define tailcc void @proc_clo$ae43290(%struct.ScmObj* %env$ae43290,%struct.ScmObj* %current_45args46485) {
%stackaddr$env-ref47302 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43290, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47302
%stackaddr$prim47303 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47303, align 8
%stackaddr$prim47304 = alloca %struct.ScmObj*, align 8
%current_45args46486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46485)
store volatile %struct.ScmObj* %current_45args46486, %struct.ScmObj** %stackaddr$prim47304, align 8
%stackaddr$prim47305 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46486)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47305, align 8
%stackaddr$makeclosure47306 = alloca %struct.ScmObj*, align 8
%fptrToInt47307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43694 to i64
%ae43694 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47307)
store volatile %struct.ScmObj* %ae43694, %struct.ScmObj** %stackaddr$makeclosure47306, align 8
%ae43695 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47308 = alloca %struct.ScmObj*, align 8
%fptrToInt47309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43696 to i64
%ae43696 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47309)
store volatile %struct.ScmObj* %ae43696, %struct.ScmObj** %stackaddr$makeclosure47308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43696, %struct.ScmObj* %_37foldl140107, i64 0)
%args46578$ae43694$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47310 = alloca %struct.ScmObj*, align 8
%args46578$ae43694$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43696, %struct.ScmObj* %args46578$ae43694$0)
store volatile %struct.ScmObj* %args46578$ae43694$1, %struct.ScmObj** %stackaddr$prim47310, align 8
%stackaddr$prim47311 = alloca %struct.ScmObj*, align 8
%args46578$ae43694$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43695, %struct.ScmObj* %args46578$ae43694$1)
store volatile %struct.ScmObj* %args46578$ae43694$2, %struct.ScmObj** %stackaddr$prim47311, align 8
%clofunc47312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43694)
musttail call tailcc void %clofunc47312(%struct.ScmObj* %ae43694, %struct.ScmObj* %args46578$ae43694$2)
ret void
}

define tailcc void @proc_clo$ae43694(%struct.ScmObj* %env$ae43694,%struct.ScmObj* %current_45args46488) {
%stackaddr$prim47313 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47313, align 8
%stackaddr$prim47314 = alloca %struct.ScmObj*, align 8
%current_45args46489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46488)
store volatile %struct.ScmObj* %current_45args46489, %struct.ScmObj** %stackaddr$prim47314, align 8
%stackaddr$prim47315 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46489)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim47315, align 8
%stackaddr$makeclosure47316 = alloca %struct.ScmObj*, align 8
%fptrToInt47317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43792 to i64
%ae43792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47317)
store volatile %struct.ScmObj* %ae43792, %struct.ScmObj** %stackaddr$makeclosure47316, align 8
%ae43793 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47318 = alloca %struct.ScmObj*, align 8
%fptrToInt47319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43794 to i64
%ae43794 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47319)
store volatile %struct.ScmObj* %ae43794, %struct.ScmObj** %stackaddr$makeclosure47318, align 8
%args46565$ae43792$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47320 = alloca %struct.ScmObj*, align 8
%args46565$ae43792$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43794, %struct.ScmObj* %args46565$ae43792$0)
store volatile %struct.ScmObj* %args46565$ae43792$1, %struct.ScmObj** %stackaddr$prim47320, align 8
%stackaddr$prim47321 = alloca %struct.ScmObj*, align 8
%args46565$ae43792$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43793, %struct.ScmObj* %args46565$ae43792$1)
store volatile %struct.ScmObj* %args46565$ae43792$2, %struct.ScmObj** %stackaddr$prim47321, align 8
%clofunc47322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43792)
musttail call tailcc void %clofunc47322(%struct.ScmObj* %ae43792, %struct.ScmObj* %args46565$ae43792$2)
ret void
}

define tailcc void @proc_clo$ae43792(%struct.ScmObj* %env$ae43792,%struct.ScmObj* %current_45args46491) {
%stackaddr$prim47323 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47323, align 8
%stackaddr$prim47324 = alloca %struct.ScmObj*, align 8
%current_45args46492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46491)
store volatile %struct.ScmObj* %current_45args46492, %struct.ScmObj** %stackaddr$prim47324, align 8
%stackaddr$prim47325 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46492)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim47325, align 8
%stackaddr$makeclosure47326 = alloca %struct.ScmObj*, align 8
%fptrToInt47327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43812 to i64
%ae43812 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47327)
store volatile %struct.ScmObj* %ae43812, %struct.ScmObj** %stackaddr$makeclosure47326, align 8
%ae43813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47328 = alloca %struct.ScmObj*, align 8
%fptrToInt47329 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43814 to i64
%ae43814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47329)
store volatile %struct.ScmObj* %ae43814, %struct.ScmObj** %stackaddr$makeclosure47328, align 8
%args46560$ae43812$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47330 = alloca %struct.ScmObj*, align 8
%args46560$ae43812$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43814, %struct.ScmObj* %args46560$ae43812$0)
store volatile %struct.ScmObj* %args46560$ae43812$1, %struct.ScmObj** %stackaddr$prim47330, align 8
%stackaddr$prim47331 = alloca %struct.ScmObj*, align 8
%args46560$ae43812$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43813, %struct.ScmObj* %args46560$ae43812$1)
store volatile %struct.ScmObj* %args46560$ae43812$2, %struct.ScmObj** %stackaddr$prim47331, align 8
%clofunc47332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43812)
musttail call tailcc void %clofunc47332(%struct.ScmObj* %ae43812, %struct.ScmObj* %args46560$ae43812$2)
ret void
}

define tailcc void @proc_clo$ae43812(%struct.ScmObj* %env$ae43812,%struct.ScmObj* %current_45args46494) {
%stackaddr$prim47333 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47333, align 8
%stackaddr$prim47334 = alloca %struct.ScmObj*, align 8
%current_45args46495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46494)
store volatile %struct.ScmObj* %current_45args46495, %struct.ScmObj** %stackaddr$prim47334, align 8
%stackaddr$prim47335 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46495)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim47335, align 8
%stackaddr$makeclosure47336 = alloca %struct.ScmObj*, align 8
%fptrToInt47337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43834 to i64
%ae43834 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47337)
store volatile %struct.ScmObj* %ae43834, %struct.ScmObj** %stackaddr$makeclosure47336, align 8
%ae43835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47338 = alloca %struct.ScmObj*, align 8
%fptrToInt47339 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43836 to i64
%ae43836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47339)
store volatile %struct.ScmObj* %ae43836, %struct.ScmObj** %stackaddr$makeclosure47338, align 8
%args46555$ae43834$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47340 = alloca %struct.ScmObj*, align 8
%args46555$ae43834$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43836, %struct.ScmObj* %args46555$ae43834$0)
store volatile %struct.ScmObj* %args46555$ae43834$1, %struct.ScmObj** %stackaddr$prim47340, align 8
%stackaddr$prim47341 = alloca %struct.ScmObj*, align 8
%args46555$ae43834$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43835, %struct.ScmObj* %args46555$ae43834$1)
store volatile %struct.ScmObj* %args46555$ae43834$2, %struct.ScmObj** %stackaddr$prim47341, align 8
%clofunc47342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43834)
musttail call tailcc void %clofunc47342(%struct.ScmObj* %ae43834, %struct.ScmObj* %args46555$ae43834$2)
ret void
}

define tailcc void @proc_clo$ae43834(%struct.ScmObj* %env$ae43834,%struct.ScmObj* %current_45args46497) {
%stackaddr$prim47343 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47343, align 8
%stackaddr$prim47344 = alloca %struct.ScmObj*, align 8
%current_45args46498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46497)
store volatile %struct.ScmObj* %current_45args46498, %struct.ScmObj** %stackaddr$prim47344, align 8
%stackaddr$prim47345 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46498)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim47345, align 8
%stackaddr$makeclosure47346 = alloca %struct.ScmObj*, align 8
%fptrToInt47347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43858 to i64
%ae43858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47347)
store volatile %struct.ScmObj* %ae43858, %struct.ScmObj** %stackaddr$makeclosure47346, align 8
%ae43859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47348 = alloca %struct.ScmObj*, align 8
%fptrToInt47349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43860 to i64
%ae43860 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47349)
store volatile %struct.ScmObj* %ae43860, %struct.ScmObj** %stackaddr$makeclosure47348, align 8
%args46550$ae43858$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47350 = alloca %struct.ScmObj*, align 8
%args46550$ae43858$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43860, %struct.ScmObj* %args46550$ae43858$0)
store volatile %struct.ScmObj* %args46550$ae43858$1, %struct.ScmObj** %stackaddr$prim47350, align 8
%stackaddr$prim47351 = alloca %struct.ScmObj*, align 8
%args46550$ae43858$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43859, %struct.ScmObj* %args46550$ae43858$1)
store volatile %struct.ScmObj* %args46550$ae43858$2, %struct.ScmObj** %stackaddr$prim47351, align 8
%clofunc47352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43858)
musttail call tailcc void %clofunc47352(%struct.ScmObj* %ae43858, %struct.ScmObj* %args46550$ae43858$2)
ret void
}

define tailcc void @proc_clo$ae43858(%struct.ScmObj* %env$ae43858,%struct.ScmObj* %current_45args46500) {
%stackaddr$prim47353 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47353, align 8
%stackaddr$prim47354 = alloca %struct.ScmObj*, align 8
%current_45args46501 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46500)
store volatile %struct.ScmObj* %current_45args46501, %struct.ScmObj** %stackaddr$prim47354, align 8
%stackaddr$prim47355 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46501)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim47355, align 8
%stackaddr$makeclosure47356 = alloca %struct.ScmObj*, align 8
%fptrToInt47357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43884 to i64
%ae43884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47357)
store volatile %struct.ScmObj* %ae43884, %struct.ScmObj** %stackaddr$makeclosure47356, align 8
%ae43885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47358 = alloca %struct.ScmObj*, align 8
%fptrToInt47359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43886 to i64
%ae43886 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47359)
store volatile %struct.ScmObj* %ae43886, %struct.ScmObj** %stackaddr$makeclosure47358, align 8
%args46545$ae43884$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47360 = alloca %struct.ScmObj*, align 8
%args46545$ae43884$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43886, %struct.ScmObj* %args46545$ae43884$0)
store volatile %struct.ScmObj* %args46545$ae43884$1, %struct.ScmObj** %stackaddr$prim47360, align 8
%stackaddr$prim47361 = alloca %struct.ScmObj*, align 8
%args46545$ae43884$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43885, %struct.ScmObj* %args46545$ae43884$1)
store volatile %struct.ScmObj* %args46545$ae43884$2, %struct.ScmObj** %stackaddr$prim47361, align 8
%clofunc47362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43884)
musttail call tailcc void %clofunc47362(%struct.ScmObj* %ae43884, %struct.ScmObj* %args46545$ae43884$2)
ret void
}

define tailcc void @proc_clo$ae43884(%struct.ScmObj* %env$ae43884,%struct.ScmObj* %current_45args46503) {
%stackaddr$prim47363 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47363, align 8
%stackaddr$prim47364 = alloca %struct.ScmObj*, align 8
%current_45args46504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46503)
store volatile %struct.ScmObj* %current_45args46504, %struct.ScmObj** %stackaddr$prim47364, align 8
%stackaddr$prim47365 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46504)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim47365, align 8
%ae43971 = call %struct.ScmObj* @const_init_int(i64 1)
%ae43972 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47366 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae43971, %struct.ScmObj* %ae43972)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim47366, align 8
%truthy$cmp47367 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp47367 = icmp eq i64 %truthy$cmp47367, 1
br i1 %cmp$cmp47367, label %truebranch$cmp47367, label %falsebranch$cmp47367
truebranch$cmp47367:
%stackaddr$makeclosure47368 = alloca %struct.ScmObj*, align 8
%fptrToInt47369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43974 to i64
%ae43974 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47369)
store volatile %struct.ScmObj* %ae43974, %struct.ScmObj** %stackaddr$makeclosure47368, align 8
%ae43975 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43976 = call %struct.ScmObj* @const_init_int(i64 0)
%args46510$ae43974$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47370 = alloca %struct.ScmObj*, align 8
%args46510$ae43974$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43976, %struct.ScmObj* %args46510$ae43974$0)
store volatile %struct.ScmObj* %args46510$ae43974$1, %struct.ScmObj** %stackaddr$prim47370, align 8
%stackaddr$prim47371 = alloca %struct.ScmObj*, align 8
%args46510$ae43974$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43975, %struct.ScmObj* %args46510$ae43974$1)
store volatile %struct.ScmObj* %args46510$ae43974$2, %struct.ScmObj** %stackaddr$prim47371, align 8
%clofunc47372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43974)
musttail call tailcc void %clofunc47372(%struct.ScmObj* %ae43974, %struct.ScmObj* %args46510$ae43974$2)
ret void
falsebranch$cmp47367:
%ae43989 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43990 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47373 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %ae43989, %struct.ScmObj* %ae43990)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim47373, align 8
%truthy$cmp47374 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp47374 = icmp eq i64 %truthy$cmp47374, 1
br i1 %cmp$cmp47374, label %truebranch$cmp47374, label %falsebranch$cmp47374
truebranch$cmp47374:
%stackaddr$prim47375 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim47375, align 8
%stackaddr$prim47376 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_void_63(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim47376, align 8
%truthy$cmp47377 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40341)
%cmp$cmp47377 = icmp eq i64 %truthy$cmp47377, 1
br i1 %cmp$cmp47377, label %truebranch$cmp47377, label %falsebranch$cmp47377
truebranch$cmp47377:
%ae43994 = call %struct.ScmObj* @const_init_true()
%truthy$cmp47378 = call i64 @is_truthy_value(%struct.ScmObj* %ae43994)
%cmp$cmp47378 = icmp eq i64 %truthy$cmp47378, 1
br i1 %cmp$cmp47378, label %truebranch$cmp47378, label %falsebranch$cmp47378
truebranch$cmp47378:
%stackaddr$makeclosure47379 = alloca %struct.ScmObj*, align 8
%fptrToInt47380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43995 to i64
%ae43995 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47380)
store volatile %struct.ScmObj* %ae43995, %struct.ScmObj** %stackaddr$makeclosure47379, align 8
%ae43996 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47381 = alloca %struct.ScmObj*, align 8
%fptrToInt47382 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43997 to i64
%ae43997 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47382)
store volatile %struct.ScmObj* %ae43997, %struct.ScmObj** %stackaddr$makeclosure47381, align 8
%args46523$ae43995$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47383 = alloca %struct.ScmObj*, align 8
%args46523$ae43995$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43997, %struct.ScmObj* %args46523$ae43995$0)
store volatile %struct.ScmObj* %args46523$ae43995$1, %struct.ScmObj** %stackaddr$prim47383, align 8
%stackaddr$prim47384 = alloca %struct.ScmObj*, align 8
%args46523$ae43995$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43996, %struct.ScmObj* %args46523$ae43995$1)
store volatile %struct.ScmObj* %args46523$ae43995$2, %struct.ScmObj** %stackaddr$prim47384, align 8
%clofunc47385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43995)
musttail call tailcc void %clofunc47385(%struct.ScmObj* %ae43995, %struct.ScmObj* %args46523$ae43995$2)
ret void
falsebranch$cmp47378:
%stackaddr$makeclosure47386 = alloca %struct.ScmObj*, align 8
%fptrToInt47387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44031 to i64
%ae44031 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47387)
store volatile %struct.ScmObj* %ae44031, %struct.ScmObj** %stackaddr$makeclosure47386, align 8
%ae44032 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44033 = call %struct.ScmObj* @const_init_int(i64 5)
%args46528$ae44031$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47388 = alloca %struct.ScmObj*, align 8
%args46528$ae44031$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44033, %struct.ScmObj* %args46528$ae44031$0)
store volatile %struct.ScmObj* %args46528$ae44031$1, %struct.ScmObj** %stackaddr$prim47388, align 8
%stackaddr$prim47389 = alloca %struct.ScmObj*, align 8
%args46528$ae44031$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44032, %struct.ScmObj* %args46528$ae44031$1)
store volatile %struct.ScmObj* %args46528$ae44031$2, %struct.ScmObj** %stackaddr$prim47389, align 8
%clofunc47390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44031)
musttail call tailcc void %clofunc47390(%struct.ScmObj* %ae44031, %struct.ScmObj* %args46528$ae44031$2)
ret void
falsebranch$cmp47377:
%stackaddr$makeclosure47391 = alloca %struct.ScmObj*, align 8
%fptrToInt47392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44065 to i64
%ae44065 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47392)
store volatile %struct.ScmObj* %ae44065, %struct.ScmObj** %stackaddr$makeclosure47391, align 8
%ae44066 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44067 = call %struct.ScmObj* @const_init_int(i64 3)
%args46533$ae44065$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47393 = alloca %struct.ScmObj*, align 8
%args46533$ae44065$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44067, %struct.ScmObj* %args46533$ae44065$0)
store volatile %struct.ScmObj* %args46533$ae44065$1, %struct.ScmObj** %stackaddr$prim47393, align 8
%stackaddr$prim47394 = alloca %struct.ScmObj*, align 8
%args46533$ae44065$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44066, %struct.ScmObj* %args46533$ae44065$1)
store volatile %struct.ScmObj* %args46533$ae44065$2, %struct.ScmObj** %stackaddr$prim47394, align 8
%clofunc47395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44065)
musttail call tailcc void %clofunc47395(%struct.ScmObj* %ae44065, %struct.ScmObj* %args46533$ae44065$2)
ret void
falsebranch$cmp47374:
%stackaddr$makeclosure47396 = alloca %struct.ScmObj*, align 8
%fptrToInt47397 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44080 to i64
%ae44080 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47397)
store volatile %struct.ScmObj* %ae44080, %struct.ScmObj** %stackaddr$makeclosure47396, align 8
%ae44081 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44082 = call %struct.ScmObj* @const_init_int(i64 2)
%args46538$ae44080$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47398 = alloca %struct.ScmObj*, align 8
%args46538$ae44080$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44082, %struct.ScmObj* %args46538$ae44080$0)
store volatile %struct.ScmObj* %args46538$ae44080$1, %struct.ScmObj** %stackaddr$prim47398, align 8
%stackaddr$prim47399 = alloca %struct.ScmObj*, align 8
%args46538$ae44080$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44081, %struct.ScmObj* %args46538$ae44080$1)
store volatile %struct.ScmObj* %args46538$ae44080$2, %struct.ScmObj** %stackaddr$prim47399, align 8
%clofunc47400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44080)
musttail call tailcc void %clofunc47400(%struct.ScmObj* %ae44080, %struct.ScmObj* %args46538$ae44080$2)
ret void
}

define tailcc void @proc_clo$ae43974(%struct.ScmObj* %env$ae43974,%struct.ScmObj* %current_45args46506) {
%stackaddr$prim47401 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47401, align 8
%stackaddr$prim47402 = alloca %struct.ScmObj*, align 8
%current_45args46507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46506)
store volatile %struct.ScmObj* %current_45args46507, %struct.ScmObj** %stackaddr$prim47402, align 8
%stackaddr$prim47403 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46507)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47403, align 8
%stackaddr$prim47404 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47404, align 8
%args46509$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47405 = alloca %struct.ScmObj*, align 8
%args46509$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46509$k$0)
store volatile %struct.ScmObj* %args46509$k$1, %struct.ScmObj** %stackaddr$prim47405, align 8
%clofunc47406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47406(%struct.ScmObj* %k, %struct.ScmObj* %args46509$k$1)
ret void
}

define tailcc void @proc_clo$ae43995(%struct.ScmObj* %env$ae43995,%struct.ScmObj* %current_45args46511) {
%stackaddr$prim47407 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46511)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47407, align 8
%stackaddr$prim47408 = alloca %struct.ScmObj*, align 8
%current_45args46512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46511)
store volatile %struct.ScmObj* %current_45args46512, %struct.ScmObj** %stackaddr$prim47408, align 8
%stackaddr$prim47409 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46512)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim47409, align 8
%stackaddr$makeclosure47410 = alloca %struct.ScmObj*, align 8
%fptrToInt47411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44014 to i64
%ae44014 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47411)
store volatile %struct.ScmObj* %ae44014, %struct.ScmObj** %stackaddr$makeclosure47410, align 8
%ae44015 = call %struct.ScmObj* @const_init_int(i64 11)
%args46518$anf_45bind40342$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47412 = alloca %struct.ScmObj*, align 8
%args46518$anf_45bind40342$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44015, %struct.ScmObj* %args46518$anf_45bind40342$0)
store volatile %struct.ScmObj* %args46518$anf_45bind40342$1, %struct.ScmObj** %stackaddr$prim47412, align 8
%stackaddr$prim47413 = alloca %struct.ScmObj*, align 8
%args46518$anf_45bind40342$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44014, %struct.ScmObj* %args46518$anf_45bind40342$1)
store volatile %struct.ScmObj* %args46518$anf_45bind40342$2, %struct.ScmObj** %stackaddr$prim47413, align 8
%clofunc47414 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40342)
musttail call tailcc void %clofunc47414(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %args46518$anf_45bind40342$2)
ret void
}

define tailcc void @proc_clo$ae44014(%struct.ScmObj* %env$ae44014,%struct.ScmObj* %current_45args46514) {
%stackaddr$prim47415 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46514)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47415, align 8
%stackaddr$prim47416 = alloca %struct.ScmObj*, align 8
%current_45args46515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46514)
store volatile %struct.ScmObj* %current_45args46515, %struct.ScmObj** %stackaddr$prim47416, align 8
%stackaddr$prim47417 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46515)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47417, align 8
%stackaddr$prim47418 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47418, align 8
%args46517$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47419 = alloca %struct.ScmObj*, align 8
%args46517$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46517$k$0)
store volatile %struct.ScmObj* %args46517$k$1, %struct.ScmObj** %stackaddr$prim47419, align 8
%clofunc47420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47420(%struct.ScmObj* %k, %struct.ScmObj* %args46517$k$1)
ret void
}

define tailcc void @proc_clo$ae43997(%struct.ScmObj* %env$ae43997,%struct.ScmObj* %current_45args46519) {
%stackaddr$prim47421 = alloca %struct.ScmObj*, align 8
%k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46519)
store volatile %struct.ScmObj* %k40377, %struct.ScmObj** %stackaddr$prim47421, align 8
%stackaddr$prim47422 = alloca %struct.ScmObj*, align 8
%current_45args46520 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46519)
store volatile %struct.ScmObj* %current_45args46520, %struct.ScmObj** %stackaddr$prim47422, align 8
%stackaddr$prim47423 = alloca %struct.ScmObj*, align 8
%a40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46520)
store volatile %struct.ScmObj* %a40222, %struct.ScmObj** %stackaddr$prim47423, align 8
%ae43999 = call %struct.ScmObj* @const_init_int(i64 0)
%args46522$k40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47424 = alloca %struct.ScmObj*, align 8
%args46522$k40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40222, %struct.ScmObj* %args46522$k40377$0)
store volatile %struct.ScmObj* %args46522$k40377$1, %struct.ScmObj** %stackaddr$prim47424, align 8
%stackaddr$prim47425 = alloca %struct.ScmObj*, align 8
%args46522$k40377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43999, %struct.ScmObj* %args46522$k40377$1)
store volatile %struct.ScmObj* %args46522$k40377$2, %struct.ScmObj** %stackaddr$prim47425, align 8
%clofunc47426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40377)
musttail call tailcc void %clofunc47426(%struct.ScmObj* %k40377, %struct.ScmObj* %args46522$k40377$2)
ret void
}

define tailcc void @proc_clo$ae44031(%struct.ScmObj* %env$ae44031,%struct.ScmObj* %current_45args46524) {
%stackaddr$prim47427 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46524)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47427, align 8
%stackaddr$prim47428 = alloca %struct.ScmObj*, align 8
%current_45args46525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46524)
store volatile %struct.ScmObj* %current_45args46525, %struct.ScmObj** %stackaddr$prim47428, align 8
%stackaddr$prim47429 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46525)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47429, align 8
%stackaddr$prim47430 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47430, align 8
%args46527$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47431 = alloca %struct.ScmObj*, align 8
%args46527$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46527$k$0)
store volatile %struct.ScmObj* %args46527$k$1, %struct.ScmObj** %stackaddr$prim47431, align 8
%clofunc47432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47432(%struct.ScmObj* %k, %struct.ScmObj* %args46527$k$1)
ret void
}

define tailcc void @proc_clo$ae44065(%struct.ScmObj* %env$ae44065,%struct.ScmObj* %current_45args46529) {
%stackaddr$prim47433 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47433, align 8
%stackaddr$prim47434 = alloca %struct.ScmObj*, align 8
%current_45args46530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46529)
store volatile %struct.ScmObj* %current_45args46530, %struct.ScmObj** %stackaddr$prim47434, align 8
%stackaddr$prim47435 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46530)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47435, align 8
%stackaddr$prim47436 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47436, align 8
%args46532$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47437 = alloca %struct.ScmObj*, align 8
%args46532$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46532$k$0)
store volatile %struct.ScmObj* %args46532$k$1, %struct.ScmObj** %stackaddr$prim47437, align 8
%clofunc47438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47438(%struct.ScmObj* %k, %struct.ScmObj* %args46532$k$1)
ret void
}

define tailcc void @proc_clo$ae44080(%struct.ScmObj* %env$ae44080,%struct.ScmObj* %current_45args46534) {
%stackaddr$prim47439 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim47439, align 8
%stackaddr$prim47440 = alloca %struct.ScmObj*, align 8
%current_45args46535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46534)
store volatile %struct.ScmObj* %current_45args46535, %struct.ScmObj** %stackaddr$prim47440, align 8
%stackaddr$prim47441 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46535)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim47441, align 8
%stackaddr$prim47442 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim47442, align 8
%args46537$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47443 = alloca %struct.ScmObj*, align 8
%args46537$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args46537$k$0)
store volatile %struct.ScmObj* %args46537$k$1, %struct.ScmObj** %stackaddr$prim47443, align 8
%clofunc47444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc47444(%struct.ScmObj* %k, %struct.ScmObj* %args46537$k$1)
ret void
}

define tailcc void @proc_clo$ae43886(%struct.ScmObj* %env$ae43886,%struct.ScmObj* %current_45args46539) {
%stackaddr$prim47445 = alloca %struct.ScmObj*, align 8
%k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %k40378, %struct.ScmObj** %stackaddr$prim47445, align 8
%stackaddr$prim47446 = alloca %struct.ScmObj*, align 8
%current_45args46540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46539)
store volatile %struct.ScmObj* %current_45args46540, %struct.ScmObj** %stackaddr$prim47446, align 8
%stackaddr$prim47447 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46540)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim47447, align 8
%stackaddr$prim47448 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim47448, align 8
%truthy$cmp47449 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40334)
%cmp$cmp47449 = icmp eq i64 %truthy$cmp47449, 1
br i1 %cmp$cmp47449, label %truebranch$cmp47449, label %falsebranch$cmp47449
truebranch$cmp47449:
%stackaddr$prim47450 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim47450, align 8
%ae43891 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim47451 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %ae43891)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim47451, align 8
%truthy$cmp47452 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40336)
%cmp$cmp47452 = icmp eq i64 %truthy$cmp47452, 1
br i1 %cmp$cmp47452, label %truebranch$cmp47452, label %falsebranch$cmp47452
truebranch$cmp47452:
%ae43894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47453 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43894)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim47453, align 8
%ae43896 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4389647454, i32 0, i32 0))
%stackaddr$prim47455 = alloca %struct.ScmObj*, align 8
%cpsprim40379 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %ae43896)
store volatile %struct.ScmObj* %cpsprim40379, %struct.ScmObj** %stackaddr$prim47455, align 8
%ae43898 = call %struct.ScmObj* @const_init_int(i64 0)
%args46542$k40378$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47456 = alloca %struct.ScmObj*, align 8
%args46542$k40378$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40379, %struct.ScmObj* %args46542$k40378$0)
store volatile %struct.ScmObj* %args46542$k40378$1, %struct.ScmObj** %stackaddr$prim47456, align 8
%stackaddr$prim47457 = alloca %struct.ScmObj*, align 8
%args46542$k40378$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43898, %struct.ScmObj* %args46542$k40378$1)
store volatile %struct.ScmObj* %args46542$k40378$2, %struct.ScmObj** %stackaddr$prim47457, align 8
%clofunc47458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40378)
musttail call tailcc void %clofunc47458(%struct.ScmObj* %k40378, %struct.ScmObj* %args46542$k40378$2)
ret void
falsebranch$cmp47452:
%ae43916 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43917 = call %struct.ScmObj* @const_init_false()
%args46543$k40378$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47459 = alloca %struct.ScmObj*, align 8
%args46543$k40378$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43917, %struct.ScmObj* %args46543$k40378$0)
store volatile %struct.ScmObj* %args46543$k40378$1, %struct.ScmObj** %stackaddr$prim47459, align 8
%stackaddr$prim47460 = alloca %struct.ScmObj*, align 8
%args46543$k40378$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43916, %struct.ScmObj* %args46543$k40378$1)
store volatile %struct.ScmObj* %args46543$k40378$2, %struct.ScmObj** %stackaddr$prim47460, align 8
%clofunc47461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40378)
musttail call tailcc void %clofunc47461(%struct.ScmObj* %k40378, %struct.ScmObj* %args46543$k40378$2)
ret void
falsebranch$cmp47449:
%ae43938 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43939 = call %struct.ScmObj* @const_init_false()
%args46544$k40378$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47462 = alloca %struct.ScmObj*, align 8
%args46544$k40378$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43939, %struct.ScmObj* %args46544$k40378$0)
store volatile %struct.ScmObj* %args46544$k40378$1, %struct.ScmObj** %stackaddr$prim47462, align 8
%stackaddr$prim47463 = alloca %struct.ScmObj*, align 8
%args46544$k40378$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43938, %struct.ScmObj* %args46544$k40378$1)
store volatile %struct.ScmObj* %args46544$k40378$2, %struct.ScmObj** %stackaddr$prim47463, align 8
%clofunc47464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40378)
musttail call tailcc void %clofunc47464(%struct.ScmObj* %k40378, %struct.ScmObj* %args46544$k40378$2)
ret void
}

define tailcc void @proc_clo$ae43860(%struct.ScmObj* %env$ae43860,%struct.ScmObj* %current_45args46546) {
%stackaddr$prim47465 = alloca %struct.ScmObj*, align 8
%k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46546)
store volatile %struct.ScmObj* %k40380, %struct.ScmObj** %stackaddr$prim47465, align 8
%stackaddr$prim47466 = alloca %struct.ScmObj*, align 8
%current_45args46547 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46546)
store volatile %struct.ScmObj* %current_45args46547, %struct.ScmObj** %stackaddr$prim47466, align 8
%stackaddr$prim47467 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46547)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim47467, align 8
%stackaddr$prim47468 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim47468, align 8
%stackaddr$prim47469 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim47469, align 8
%stackaddr$prim47470 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim47470, align 8
%stackaddr$prim47471 = alloca %struct.ScmObj*, align 8
%cpsprim40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %cpsprim40381, %struct.ScmObj** %stackaddr$prim47471, align 8
%ae43866 = call %struct.ScmObj* @const_init_int(i64 0)
%args46549$k40380$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47472 = alloca %struct.ScmObj*, align 8
%args46549$k40380$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40381, %struct.ScmObj* %args46549$k40380$0)
store volatile %struct.ScmObj* %args46549$k40380$1, %struct.ScmObj** %stackaddr$prim47472, align 8
%stackaddr$prim47473 = alloca %struct.ScmObj*, align 8
%args46549$k40380$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43866, %struct.ScmObj* %args46549$k40380$1)
store volatile %struct.ScmObj* %args46549$k40380$2, %struct.ScmObj** %stackaddr$prim47473, align 8
%clofunc47474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40380)
musttail call tailcc void %clofunc47474(%struct.ScmObj* %k40380, %struct.ScmObj* %args46549$k40380$2)
ret void
}

define tailcc void @proc_clo$ae43836(%struct.ScmObj* %env$ae43836,%struct.ScmObj* %current_45args46551) {
%stackaddr$prim47475 = alloca %struct.ScmObj*, align 8
%k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %k40382, %struct.ScmObj** %stackaddr$prim47475, align 8
%stackaddr$prim47476 = alloca %struct.ScmObj*, align 8
%current_45args46552 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46551)
store volatile %struct.ScmObj* %current_45args46552, %struct.ScmObj** %stackaddr$prim47476, align 8
%stackaddr$prim47477 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46552)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim47477, align 8
%stackaddr$prim47478 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim47478, align 8
%stackaddr$prim47479 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim47479, align 8
%stackaddr$prim47480 = alloca %struct.ScmObj*, align 8
%cpsprim40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %cpsprim40383, %struct.ScmObj** %stackaddr$prim47480, align 8
%ae43841 = call %struct.ScmObj* @const_init_int(i64 0)
%args46554$k40382$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47481 = alloca %struct.ScmObj*, align 8
%args46554$k40382$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40383, %struct.ScmObj* %args46554$k40382$0)
store volatile %struct.ScmObj* %args46554$k40382$1, %struct.ScmObj** %stackaddr$prim47481, align 8
%stackaddr$prim47482 = alloca %struct.ScmObj*, align 8
%args46554$k40382$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43841, %struct.ScmObj* %args46554$k40382$1)
store volatile %struct.ScmObj* %args46554$k40382$2, %struct.ScmObj** %stackaddr$prim47482, align 8
%clofunc47483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40382)
musttail call tailcc void %clofunc47483(%struct.ScmObj* %k40382, %struct.ScmObj* %args46554$k40382$2)
ret void
}

define tailcc void @proc_clo$ae43814(%struct.ScmObj* %env$ae43814,%struct.ScmObj* %current_45args46556) {
%stackaddr$prim47484 = alloca %struct.ScmObj*, align 8
%k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46556)
store volatile %struct.ScmObj* %k40384, %struct.ScmObj** %stackaddr$prim47484, align 8
%stackaddr$prim47485 = alloca %struct.ScmObj*, align 8
%current_45args46557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46556)
store volatile %struct.ScmObj* %current_45args46557, %struct.ScmObj** %stackaddr$prim47485, align 8
%stackaddr$prim47486 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46557)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim47486, align 8
%stackaddr$prim47487 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim47487, align 8
%stackaddr$prim47488 = alloca %struct.ScmObj*, align 8
%cpsprim40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %cpsprim40385, %struct.ScmObj** %stackaddr$prim47488, align 8
%ae43818 = call %struct.ScmObj* @const_init_int(i64 0)
%args46559$k40384$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47489 = alloca %struct.ScmObj*, align 8
%args46559$k40384$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40385, %struct.ScmObj* %args46559$k40384$0)
store volatile %struct.ScmObj* %args46559$k40384$1, %struct.ScmObj** %stackaddr$prim47489, align 8
%stackaddr$prim47490 = alloca %struct.ScmObj*, align 8
%args46559$k40384$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43818, %struct.ScmObj* %args46559$k40384$1)
store volatile %struct.ScmObj* %args46559$k40384$2, %struct.ScmObj** %stackaddr$prim47490, align 8
%clofunc47491 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40384)
musttail call tailcc void %clofunc47491(%struct.ScmObj* %k40384, %struct.ScmObj* %args46559$k40384$2)
ret void
}

define tailcc void @proc_clo$ae43794(%struct.ScmObj* %env$ae43794,%struct.ScmObj* %current_45args46561) {
%stackaddr$prim47492 = alloca %struct.ScmObj*, align 8
%k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46561)
store volatile %struct.ScmObj* %k40386, %struct.ScmObj** %stackaddr$prim47492, align 8
%stackaddr$prim47493 = alloca %struct.ScmObj*, align 8
%current_45args46562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46561)
store volatile %struct.ScmObj* %current_45args46562, %struct.ScmObj** %stackaddr$prim47493, align 8
%stackaddr$prim47494 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46562)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim47494, align 8
%stackaddr$prim47495 = alloca %struct.ScmObj*, align 8
%cpsprim40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40387, %struct.ScmObj** %stackaddr$prim47495, align 8
%ae43797 = call %struct.ScmObj* @const_init_int(i64 0)
%args46564$k40386$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47496 = alloca %struct.ScmObj*, align 8
%args46564$k40386$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40387, %struct.ScmObj* %args46564$k40386$0)
store volatile %struct.ScmObj* %args46564$k40386$1, %struct.ScmObj** %stackaddr$prim47496, align 8
%stackaddr$prim47497 = alloca %struct.ScmObj*, align 8
%args46564$k40386$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43797, %struct.ScmObj* %args46564$k40386$1)
store volatile %struct.ScmObj* %args46564$k40386$2, %struct.ScmObj** %stackaddr$prim47497, align 8
%clofunc47498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40386)
musttail call tailcc void %clofunc47498(%struct.ScmObj* %k40386, %struct.ScmObj* %args46564$k40386$2)
ret void
}

define tailcc void @proc_clo$ae43696(%struct.ScmObj* %env$ae43696,%struct.ScmObj* %args4016840388) {
%stackaddr$env-ref47499 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43696, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47499
%stackaddr$prim47500 = alloca %struct.ScmObj*, align 8
%k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840388)
store volatile %struct.ScmObj* %k40389, %struct.ScmObj** %stackaddr$prim47500, align 8
%stackaddr$prim47501 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840388)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim47501, align 8
%stackaddr$prim47502 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim47502, align 8
%truthy$cmp47503 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40322)
%cmp$cmp47503 = icmp eq i64 %truthy$cmp47503, 1
br i1 %cmp$cmp47503, label %truebranch$cmp47503, label %falsebranch$cmp47503
truebranch$cmp47503:
%ae43702 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43703 = call %struct.ScmObj* @const_init_int(i64 1)
%args46566$k40389$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47504 = alloca %struct.ScmObj*, align 8
%args46566$k40389$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43703, %struct.ScmObj* %args46566$k40389$0)
store volatile %struct.ScmObj* %args46566$k40389$1, %struct.ScmObj** %stackaddr$prim47504, align 8
%stackaddr$prim47505 = alloca %struct.ScmObj*, align 8
%args46566$k40389$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43702, %struct.ScmObj* %args46566$k40389$1)
store volatile %struct.ScmObj* %args46566$k40389$2, %struct.ScmObj** %stackaddr$prim47505, align 8
%clofunc47506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40389)
musttail call tailcc void %clofunc47506(%struct.ScmObj* %k40389, %struct.ScmObj* %args46566$k40389$2)
ret void
falsebranch$cmp47503:
%stackaddr$prim47507 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim47507, align 8
%stackaddr$prim47508 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim47508, align 8
%truthy$cmp47509 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40324)
%cmp$cmp47509 = icmp eq i64 %truthy$cmp47509, 1
br i1 %cmp$cmp47509, label %truebranch$cmp47509, label %falsebranch$cmp47509
truebranch$cmp47509:
%stackaddr$prim47510 = alloca %struct.ScmObj*, align 8
%cpsprim40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40390, %struct.ScmObj** %stackaddr$prim47510, align 8
%ae43715 = call %struct.ScmObj* @const_init_int(i64 0)
%args46567$k40389$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47511 = alloca %struct.ScmObj*, align 8
%args46567$k40389$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40390, %struct.ScmObj* %args46567$k40389$0)
store volatile %struct.ScmObj* %args46567$k40389$1, %struct.ScmObj** %stackaddr$prim47511, align 8
%stackaddr$prim47512 = alloca %struct.ScmObj*, align 8
%args46567$k40389$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43715, %struct.ScmObj* %args46567$k40389$1)
store volatile %struct.ScmObj* %args46567$k40389$2, %struct.ScmObj** %stackaddr$prim47512, align 8
%clofunc47513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40389)
musttail call tailcc void %clofunc47513(%struct.ScmObj* %k40389, %struct.ScmObj* %args46567$k40389$2)
ret void
falsebranch$cmp47509:
%stackaddr$makeclosure47514 = alloca %struct.ScmObj*, align 8
%fptrToInt47515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43720 to i64
%ae43720 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47515)
store volatile %struct.ScmObj* %ae43720, %struct.ScmObj** %stackaddr$makeclosure47514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43720, %struct.ScmObj* %k40389, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43720, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43720, %struct.ScmObj* %args40168, i64 2)
%ae43721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47516 = alloca %struct.ScmObj*, align 8
%fptrToInt47517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43722 to i64
%ae43722 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47517)
store volatile %struct.ScmObj* %ae43722, %struct.ScmObj** %stackaddr$makeclosure47516, align 8
%args46577$ae43720$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47518 = alloca %struct.ScmObj*, align 8
%args46577$ae43720$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43722, %struct.ScmObj* %args46577$ae43720$0)
store volatile %struct.ScmObj* %args46577$ae43720$1, %struct.ScmObj** %stackaddr$prim47518, align 8
%stackaddr$prim47519 = alloca %struct.ScmObj*, align 8
%args46577$ae43720$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43721, %struct.ScmObj* %args46577$ae43720$1)
store volatile %struct.ScmObj* %args46577$ae43720$2, %struct.ScmObj** %stackaddr$prim47519, align 8
%clofunc47520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43720)
musttail call tailcc void %clofunc47520(%struct.ScmObj* %ae43720, %struct.ScmObj* %args46577$ae43720$2)
ret void
}

define tailcc void @proc_clo$ae43720(%struct.ScmObj* %env$ae43720,%struct.ScmObj* %current_45args46568) {
%stackaddr$env-ref47521 = alloca %struct.ScmObj*, align 8
%k40389 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43720, i64 0)
store %struct.ScmObj* %k40389, %struct.ScmObj** %stackaddr$env-ref47521
%stackaddr$env-ref47522 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43720, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47522
%stackaddr$env-ref47523 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43720, i64 2)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref47523
%stackaddr$prim47524 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46568)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim47524, align 8
%stackaddr$prim47525 = alloca %struct.ScmObj*, align 8
%current_45args46569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46568)
store volatile %struct.ScmObj* %current_45args46569, %struct.ScmObj** %stackaddr$prim47525, align 8
%stackaddr$prim47526 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46569)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim47526, align 8
%stackaddr$prim47527 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim47527, align 8
%stackaddr$prim47528 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim47528, align 8
%args46571$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47529 = alloca %struct.ScmObj*, align 8
%args46571$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %args46571$_37foldl140107$0)
store volatile %struct.ScmObj* %args46571$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim47529, align 8
%stackaddr$prim47530 = alloca %struct.ScmObj*, align 8
%args46571$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40326, %struct.ScmObj* %args46571$_37foldl140107$1)
store volatile %struct.ScmObj* %args46571$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim47530, align 8
%stackaddr$prim47531 = alloca %struct.ScmObj*, align 8
%args46571$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40325, %struct.ScmObj* %args46571$_37foldl140107$2)
store volatile %struct.ScmObj* %args46571$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim47531, align 8
%stackaddr$prim47532 = alloca %struct.ScmObj*, align 8
%args46571$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40389, %struct.ScmObj* %args46571$_37foldl140107$3)
store volatile %struct.ScmObj* %args46571$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim47532, align 8
%clofunc47533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc47533(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46571$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43722(%struct.ScmObj* %env$ae43722,%struct.ScmObj* %current_45args46572) {
%stackaddr$prim47534 = alloca %struct.ScmObj*, align 8
%k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %k40392, %struct.ScmObj** %stackaddr$prim47534, align 8
%stackaddr$prim47535 = alloca %struct.ScmObj*, align 8
%current_45args46573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46572)
store volatile %struct.ScmObj* %current_45args46573, %struct.ScmObj** %stackaddr$prim47535, align 8
%stackaddr$prim47536 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim47536, align 8
%stackaddr$prim47537 = alloca %struct.ScmObj*, align 8
%current_45args46574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46573)
store volatile %struct.ScmObj* %current_45args46574, %struct.ScmObj** %stackaddr$prim47537, align 8
%stackaddr$prim47538 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46574)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim47538, align 8
%stackaddr$prim47539 = alloca %struct.ScmObj*, align 8
%cpsprim40393 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40393, %struct.ScmObj** %stackaddr$prim47539, align 8
%ae43726 = call %struct.ScmObj* @const_init_int(i64 0)
%args46576$k40392$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47540 = alloca %struct.ScmObj*, align 8
%args46576$k40392$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40393, %struct.ScmObj* %args46576$k40392$0)
store volatile %struct.ScmObj* %args46576$k40392$1, %struct.ScmObj** %stackaddr$prim47540, align 8
%stackaddr$prim47541 = alloca %struct.ScmObj*, align 8
%args46576$k40392$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43726, %struct.ScmObj* %args46576$k40392$1)
store volatile %struct.ScmObj* %args46576$k40392$2, %struct.ScmObj** %stackaddr$prim47541, align 8
%clofunc47542 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40392)
musttail call tailcc void %clofunc47542(%struct.ScmObj* %k40392, %struct.ScmObj* %args46576$k40392$2)
ret void
}

define tailcc void @proc_clo$ae43292(%struct.ScmObj* %env$ae43292,%struct.ScmObj* %current_45args46579) {
%stackaddr$prim47543 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46579)
store volatile %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$prim47543, align 8
%stackaddr$prim47544 = alloca %struct.ScmObj*, align 8
%current_45args46580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46579)
store volatile %struct.ScmObj* %current_45args46580, %struct.ScmObj** %stackaddr$prim47544, align 8
%stackaddr$prim47545 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46580)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim47545, align 8
%stackaddr$prim47546 = alloca %struct.ScmObj*, align 8
%current_45args46581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46580)
store volatile %struct.ScmObj* %current_45args46581, %struct.ScmObj** %stackaddr$prim47546, align 8
%stackaddr$prim47547 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46581)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim47547, align 8
%ae43293 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47548 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43293, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim47548, align 8
%stackaddr$makeclosure47549 = alloca %struct.ScmObj*, align 8
%fptrToInt47550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43295 to i64
%ae43295 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47550)
store volatile %struct.ScmObj* %ae43295, %struct.ScmObj** %stackaddr$makeclosure47549, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43295, %struct.ScmObj* %lst40174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43295, %struct.ScmObj* %v40173, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43295, %struct.ScmObj* %k40394, i64 2)
%ae43296 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47551 = alloca %struct.ScmObj*, align 8
%fptrToInt47552 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43297 to i64
%ae43297 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47552)
store volatile %struct.ScmObj* %ae43297, %struct.ScmObj** %stackaddr$makeclosure47551, align 8
%args46603$ae43295$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47553 = alloca %struct.ScmObj*, align 8
%args46603$ae43295$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43297, %struct.ScmObj* %args46603$ae43295$0)
store volatile %struct.ScmObj* %args46603$ae43295$1, %struct.ScmObj** %stackaddr$prim47553, align 8
%stackaddr$prim47554 = alloca %struct.ScmObj*, align 8
%args46603$ae43295$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43296, %struct.ScmObj* %args46603$ae43295$1)
store volatile %struct.ScmObj* %args46603$ae43295$2, %struct.ScmObj** %stackaddr$prim47554, align 8
%clofunc47555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43295)
musttail call tailcc void %clofunc47555(%struct.ScmObj* %ae43295, %struct.ScmObj* %args46603$ae43295$2)
ret void
}

define tailcc void @proc_clo$ae43295(%struct.ScmObj* %env$ae43295,%struct.ScmObj* %current_45args46583) {
%stackaddr$env-ref47556 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43295, i64 0)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47556
%stackaddr$env-ref47557 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43295, i64 1)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47557
%stackaddr$env-ref47558 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43295, i64 2)
store %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$env-ref47558
%stackaddr$prim47559 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46583)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim47559, align 8
%stackaddr$prim47560 = alloca %struct.ScmObj*, align 8
%current_45args46584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46583)
store volatile %struct.ScmObj* %current_45args46584, %struct.ScmObj** %stackaddr$prim47560, align 8
%stackaddr$prim47561 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46584)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim47561, align 8
%stackaddr$makeclosure47562 = alloca %struct.ScmObj*, align 8
%fptrToInt47563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43311 to i64
%ae43311 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47563)
store volatile %struct.ScmObj* %ae43311, %struct.ScmObj** %stackaddr$makeclosure47562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %lst40174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %v40173, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %k40394, i64 2)
%stackaddr$makeclosure47564 = alloca %struct.ScmObj*, align 8
%fptrToInt47565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43312 to i64
%ae43312 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47565)
store volatile %struct.ScmObj* %ae43312, %struct.ScmObj** %stackaddr$makeclosure47564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43312, %struct.ScmObj* %lst40174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43312, %struct.ScmObj* %v40173, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43312, %struct.ScmObj* %k40394, i64 2)
%args46598$anf_45bind40314$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47566 = alloca %struct.ScmObj*, align 8
%args46598$anf_45bind40314$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43312, %struct.ScmObj* %args46598$anf_45bind40314$0)
store volatile %struct.ScmObj* %args46598$anf_45bind40314$1, %struct.ScmObj** %stackaddr$prim47566, align 8
%stackaddr$prim47567 = alloca %struct.ScmObj*, align 8
%args46598$anf_45bind40314$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43311, %struct.ScmObj* %args46598$anf_45bind40314$1)
store volatile %struct.ScmObj* %args46598$anf_45bind40314$2, %struct.ScmObj** %stackaddr$prim47567, align 8
%clofunc47568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40314)
musttail call tailcc void %clofunc47568(%struct.ScmObj* %anf_45bind40314, %struct.ScmObj* %args46598$anf_45bind40314$2)
ret void
}

define tailcc void @proc_clo$ae43311(%struct.ScmObj* %env$ae43311,%struct.ScmObj* %current_45args46586) {
%stackaddr$env-ref47569 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 0)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47569
%stackaddr$env-ref47570 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 1)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47570
%stackaddr$env-ref47571 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 2)
store %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$env-ref47571
%stackaddr$prim47572 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46586)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim47572, align 8
%stackaddr$prim47573 = alloca %struct.ScmObj*, align 8
%current_45args46587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46586)
store volatile %struct.ScmObj* %current_45args46587, %struct.ScmObj** %stackaddr$prim47573, align 8
%stackaddr$prim47574 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46587)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47574, align 8
%ae43420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47575 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43420)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47575, align 8
%stackaddr$prim47576 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47576, align 8
%truthy$cmp47577 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40316)
%cmp$cmp47577 = icmp eq i64 %truthy$cmp47577, 1
br i1 %cmp$cmp47577, label %truebranch$cmp47577, label %falsebranch$cmp47577
truebranch$cmp47577:
%ae43424 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43425 = call %struct.ScmObj* @const_init_false()
%args46589$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47578 = alloca %struct.ScmObj*, align 8
%args46589$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43425, %struct.ScmObj* %args46589$k40394$0)
store volatile %struct.ScmObj* %args46589$k40394$1, %struct.ScmObj** %stackaddr$prim47578, align 8
%stackaddr$prim47579 = alloca %struct.ScmObj*, align 8
%args46589$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43424, %struct.ScmObj* %args46589$k40394$1)
store volatile %struct.ScmObj* %args46589$k40394$2, %struct.ScmObj** %stackaddr$prim47579, align 8
%clofunc47580 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47580(%struct.ScmObj* %k40394, %struct.ScmObj* %args46589$k40394$2)
ret void
falsebranch$cmp47577:
%ae43433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47581 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43433)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47581, align 8
%stackaddr$prim47582 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47582, align 8
%stackaddr$prim47583 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47583, align 8
%truthy$cmp47584 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40319)
%cmp$cmp47584 = icmp eq i64 %truthy$cmp47584, 1
br i1 %cmp$cmp47584, label %truebranch$cmp47584, label %falsebranch$cmp47584
truebranch$cmp47584:
%ae43439 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47585 = alloca %struct.ScmObj*, align 8
%cpsprim40397 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43439)
store volatile %struct.ScmObj* %cpsprim40397, %struct.ScmObj** %stackaddr$prim47585, align 8
%ae43441 = call %struct.ScmObj* @const_init_int(i64 0)
%args46590$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47586 = alloca %struct.ScmObj*, align 8
%args46590$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40397, %struct.ScmObj* %args46590$k40394$0)
store volatile %struct.ScmObj* %args46590$k40394$1, %struct.ScmObj** %stackaddr$prim47586, align 8
%stackaddr$prim47587 = alloca %struct.ScmObj*, align 8
%args46590$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43441, %struct.ScmObj* %args46590$k40394$1)
store volatile %struct.ScmObj* %args46590$k40394$2, %struct.ScmObj** %stackaddr$prim47587, align 8
%clofunc47588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47588(%struct.ScmObj* %k40394, %struct.ScmObj* %args46590$k40394$2)
ret void
falsebranch$cmp47584:
%ae43452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47589 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43452)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47589, align 8
%stackaddr$prim47590 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47590, align 8
%ae43455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47591 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43455, %struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47591, align 8
%args46591$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47592 = alloca %struct.ScmObj*, align 8
%args46591$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46591$cc40175$0)
store volatile %struct.ScmObj* %args46591$cc40175$1, %struct.ScmObj** %stackaddr$prim47592, align 8
%stackaddr$prim47593 = alloca %struct.ScmObj*, align 8
%args46591$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40394, %struct.ScmObj* %args46591$cc40175$1)
store volatile %struct.ScmObj* %args46591$cc40175$2, %struct.ScmObj** %stackaddr$prim47593, align 8
%clofunc47594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47594(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46591$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43312(%struct.ScmObj* %env$ae43312,%struct.ScmObj* %current_45args46592) {
%stackaddr$env-ref47595 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43312, i64 0)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref47595
%stackaddr$env-ref47596 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43312, i64 1)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref47596
%stackaddr$env-ref47597 = alloca %struct.ScmObj*, align 8
%k40394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43312, i64 2)
store %struct.ScmObj* %k40394, %struct.ScmObj** %stackaddr$env-ref47597
%stackaddr$prim47598 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46592)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim47598, align 8
%stackaddr$prim47599 = alloca %struct.ScmObj*, align 8
%current_45args46593 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46592)
store volatile %struct.ScmObj* %current_45args46593, %struct.ScmObj** %stackaddr$prim47599, align 8
%stackaddr$prim47600 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46593)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim47600, align 8
%ae43314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47601 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim47601, align 8
%stackaddr$prim47602 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim47602, align 8
%truthy$cmp47603 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40316)
%cmp$cmp47603 = icmp eq i64 %truthy$cmp47603, 1
br i1 %cmp$cmp47603, label %truebranch$cmp47603, label %falsebranch$cmp47603
truebranch$cmp47603:
%ae43318 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43319 = call %struct.ScmObj* @const_init_false()
%args46595$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47604 = alloca %struct.ScmObj*, align 8
%args46595$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43319, %struct.ScmObj* %args46595$k40394$0)
store volatile %struct.ScmObj* %args46595$k40394$1, %struct.ScmObj** %stackaddr$prim47604, align 8
%stackaddr$prim47605 = alloca %struct.ScmObj*, align 8
%args46595$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43318, %struct.ScmObj* %args46595$k40394$1)
store volatile %struct.ScmObj* %args46595$k40394$2, %struct.ScmObj** %stackaddr$prim47605, align 8
%clofunc47606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47606(%struct.ScmObj* %k40394, %struct.ScmObj* %args46595$k40394$2)
ret void
falsebranch$cmp47603:
%ae43327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47607 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43327)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim47607, align 8
%stackaddr$prim47608 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim47608, align 8
%stackaddr$prim47609 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim47609, align 8
%truthy$cmp47610 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40319)
%cmp$cmp47610 = icmp eq i64 %truthy$cmp47610, 1
br i1 %cmp$cmp47610, label %truebranch$cmp47610, label %falsebranch$cmp47610
truebranch$cmp47610:
%ae43333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47611 = alloca %struct.ScmObj*, align 8
%cpsprim40397 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43333)
store volatile %struct.ScmObj* %cpsprim40397, %struct.ScmObj** %stackaddr$prim47611, align 8
%ae43335 = call %struct.ScmObj* @const_init_int(i64 0)
%args46596$k40394$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47612 = alloca %struct.ScmObj*, align 8
%args46596$k40394$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40397, %struct.ScmObj* %args46596$k40394$0)
store volatile %struct.ScmObj* %args46596$k40394$1, %struct.ScmObj** %stackaddr$prim47612, align 8
%stackaddr$prim47613 = alloca %struct.ScmObj*, align 8
%args46596$k40394$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43335, %struct.ScmObj* %args46596$k40394$1)
store volatile %struct.ScmObj* %args46596$k40394$2, %struct.ScmObj** %stackaddr$prim47613, align 8
%clofunc47614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40394)
musttail call tailcc void %clofunc47614(%struct.ScmObj* %k40394, %struct.ScmObj* %args46596$k40394$2)
ret void
falsebranch$cmp47610:
%ae43346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43346)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim47615, align 8
%stackaddr$prim47616 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim47616, align 8
%ae43349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47617 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43349, %struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim47617, align 8
%args46597$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47618 = alloca %struct.ScmObj*, align 8
%args46597$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46597$cc40175$0)
store volatile %struct.ScmObj* %args46597$cc40175$1, %struct.ScmObj** %stackaddr$prim47618, align 8
%stackaddr$prim47619 = alloca %struct.ScmObj*, align 8
%args46597$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40394, %struct.ScmObj* %args46597$cc40175$1)
store volatile %struct.ScmObj* %args46597$cc40175$2, %struct.ScmObj** %stackaddr$prim47619, align 8
%clofunc47620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc47620(%struct.ScmObj* %cc40175, %struct.ScmObj* %args46597$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43297(%struct.ScmObj* %env$ae43297,%struct.ScmObj* %current_45args46599) {
%stackaddr$prim47621 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46599)
store volatile %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$prim47621, align 8
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%current_45args46600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46599)
store volatile %struct.ScmObj* %current_45args46600, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$prim47623 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46600)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim47623, align 8
%args46602$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47624 = alloca %struct.ScmObj*, align 8
%args46602$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args46602$u40176$0)
store volatile %struct.ScmObj* %args46602$u40176$1, %struct.ScmObj** %stackaddr$prim47624, align 8
%stackaddr$prim47625 = alloca %struct.ScmObj*, align 8
%args46602$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40398, %struct.ScmObj* %args46602$u40176$1)
store volatile %struct.ScmObj* %args46602$u40176$2, %struct.ScmObj** %stackaddr$prim47625, align 8
%clofunc47626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc47626(%struct.ScmObj* %u40176, %struct.ScmObj* %args46602$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42756(%struct.ScmObj* %env$ae42756,%struct.ScmObj* %current_45args46605) {
%stackaddr$prim47627 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46605)
store volatile %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$prim47627, align 8
%stackaddr$prim47628 = alloca %struct.ScmObj*, align 8
%current_45args46606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46605)
store volatile %struct.ScmObj* %current_45args46606, %struct.ScmObj** %stackaddr$prim47628, align 8
%stackaddr$prim47629 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46606)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim47629, align 8
%stackaddr$prim47630 = alloca %struct.ScmObj*, align 8
%current_45args46607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46606)
store volatile %struct.ScmObj* %current_45args46607, %struct.ScmObj** %stackaddr$prim47630, align 8
%stackaddr$prim47631 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46607)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim47631, align 8
%ae42757 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47632 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42757, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim47632, align 8
%ae42759 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47633 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42759, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim47633, align 8
%stackaddr$makeclosure47634 = alloca %struct.ScmObj*, align 8
%fptrToInt47635 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42761 to i64
%ae42761 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47635)
store volatile %struct.ScmObj* %ae42761, %struct.ScmObj** %stackaddr$makeclosure47634, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42761, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42761, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42761, %struct.ScmObj* %k40399, i64 2)
%ae42762 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47636 = alloca %struct.ScmObj*, align 8
%fptrToInt47637 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42763 to i64
%ae42763 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47637)
store volatile %struct.ScmObj* %ae42763, %struct.ScmObj** %stackaddr$makeclosure47636, align 8
%args46627$ae42761$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47638 = alloca %struct.ScmObj*, align 8
%args46627$ae42761$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42763, %struct.ScmObj* %args46627$ae42761$0)
store volatile %struct.ScmObj* %args46627$ae42761$1, %struct.ScmObj** %stackaddr$prim47638, align 8
%stackaddr$prim47639 = alloca %struct.ScmObj*, align 8
%args46627$ae42761$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42762, %struct.ScmObj* %args46627$ae42761$1)
store volatile %struct.ScmObj* %args46627$ae42761$2, %struct.ScmObj** %stackaddr$prim47639, align 8
%clofunc47640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42761)
musttail call tailcc void %clofunc47640(%struct.ScmObj* %ae42761, %struct.ScmObj* %args46627$ae42761$2)
ret void
}

define tailcc void @proc_clo$ae42761(%struct.ScmObj* %env$ae42761,%struct.ScmObj* %current_45args46609) {
%stackaddr$env-ref47641 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42761, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47641
%stackaddr$env-ref47642 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42761, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47642
%stackaddr$env-ref47643 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42761, i64 2)
store %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$env-ref47643
%stackaddr$prim47644 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46609)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim47644, align 8
%stackaddr$prim47645 = alloca %struct.ScmObj*, align 8
%current_45args46610 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46609)
store volatile %struct.ScmObj* %current_45args46610, %struct.ScmObj** %stackaddr$prim47645, align 8
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46610)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47646, align 8
%stackaddr$makeclosure47647 = alloca %struct.ScmObj*, align 8
%fptrToInt47648 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42777 to i64
%ae42777 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47648)
store volatile %struct.ScmObj* %ae42777, %struct.ScmObj** %stackaddr$makeclosure47647, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %k40399, i64 2)
%stackaddr$makeclosure47649 = alloca %struct.ScmObj*, align 8
%fptrToInt47650 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42778 to i64
%ae42778 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47650)
store volatile %struct.ScmObj* %ae42778, %struct.ScmObj** %stackaddr$makeclosure47649, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42778, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42778, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42778, %struct.ScmObj* %k40399, i64 2)
%args46622$anf_45bind40307$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47651 = alloca %struct.ScmObj*, align 8
%args46622$anf_45bind40307$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42778, %struct.ScmObj* %args46622$anf_45bind40307$0)
store volatile %struct.ScmObj* %args46622$anf_45bind40307$1, %struct.ScmObj** %stackaddr$prim47651, align 8
%stackaddr$prim47652 = alloca %struct.ScmObj*, align 8
%args46622$anf_45bind40307$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42777, %struct.ScmObj* %args46622$anf_45bind40307$1)
store volatile %struct.ScmObj* %args46622$anf_45bind40307$2, %struct.ScmObj** %stackaddr$prim47652, align 8
%clofunc47653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40307)
musttail call tailcc void %clofunc47653(%struct.ScmObj* %anf_45bind40307, %struct.ScmObj* %args46622$anf_45bind40307$2)
ret void
}

define tailcc void @proc_clo$ae42777(%struct.ScmObj* %env$ae42777,%struct.ScmObj* %current_45args46612) {
%stackaddr$env-ref47654 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47654
%stackaddr$env-ref47655 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47655
%stackaddr$env-ref47656 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 2)
store %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$env-ref47656
%stackaddr$prim47657 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim47657, align 8
%stackaddr$prim47658 = alloca %struct.ScmObj*, align 8
%current_45args46613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46612)
store volatile %struct.ScmObj* %current_45args46613, %struct.ScmObj** %stackaddr$prim47658, align 8
%stackaddr$prim47659 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46613)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47659, align 8
%ae42920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47660 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42920)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47660, align 8
%ae42921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47661 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42921, %struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47661, align 8
%truthy$cmp47662 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40309)
%cmp$cmp47662 = icmp eq i64 %truthy$cmp47662, 1
br i1 %cmp$cmp47662, label %truebranch$cmp47662, label %falsebranch$cmp47662
truebranch$cmp47662:
%ae42925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47663 = alloca %struct.ScmObj*, align 8
%cpsprim40402 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42925)
store volatile %struct.ScmObj* %cpsprim40402, %struct.ScmObj** %stackaddr$prim47663, align 8
%ae42927 = call %struct.ScmObj* @const_init_int(i64 0)
%args46615$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47664 = alloca %struct.ScmObj*, align 8
%args46615$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40402, %struct.ScmObj* %args46615$k40399$0)
store volatile %struct.ScmObj* %args46615$k40399$1, %struct.ScmObj** %stackaddr$prim47664, align 8
%stackaddr$prim47665 = alloca %struct.ScmObj*, align 8
%args46615$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42927, %struct.ScmObj* %args46615$k40399$1)
store volatile %struct.ScmObj* %args46615$k40399$2, %struct.ScmObj** %stackaddr$prim47665, align 8
%clofunc47666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47666(%struct.ScmObj* %k40399, %struct.ScmObj* %args46615$k40399$2)
ret void
falsebranch$cmp47662:
%ae42938 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42938)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47667, align 8
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47668, align 8
%ae42941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47669 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42941, %struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47669, align 8
%ae42944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47670 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42944)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47670, align 8
%ae42946 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40312, %struct.ScmObj* %ae42946)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47671, align 8
%ae42948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47672 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42948, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47672, align 8
%args46616$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47673 = alloca %struct.ScmObj*, align 8
%args46616$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46616$cc40183$0)
store volatile %struct.ScmObj* %args46616$cc40183$1, %struct.ScmObj** %stackaddr$prim47673, align 8
%stackaddr$prim47674 = alloca %struct.ScmObj*, align 8
%args46616$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40399, %struct.ScmObj* %args46616$cc40183$1)
store volatile %struct.ScmObj* %args46616$cc40183$2, %struct.ScmObj** %stackaddr$prim47674, align 8
%clofunc47675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47675(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46616$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42778(%struct.ScmObj* %env$ae42778,%struct.ScmObj* %current_45args46617) {
%stackaddr$env-ref47676 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42778, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref47676
%stackaddr$env-ref47677 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42778, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref47677
%stackaddr$env-ref47678 = alloca %struct.ScmObj*, align 8
%k40399 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42778, i64 2)
store %struct.ScmObj* %k40399, %struct.ScmObj** %stackaddr$env-ref47678
%stackaddr$prim47679 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim47679, align 8
%stackaddr$prim47680 = alloca %struct.ScmObj*, align 8
%current_45args46618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46617)
store volatile %struct.ScmObj* %current_45args46618, %struct.ScmObj** %stackaddr$prim47680, align 8
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46618)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim47681, align 8
%ae42780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47682 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42780)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim47682, align 8
%ae42781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47683 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42781, %struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim47683, align 8
%truthy$cmp47684 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40309)
%cmp$cmp47684 = icmp eq i64 %truthy$cmp47684, 1
br i1 %cmp$cmp47684, label %truebranch$cmp47684, label %falsebranch$cmp47684
truebranch$cmp47684:
%ae42785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47685 = alloca %struct.ScmObj*, align 8
%cpsprim40402 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42785)
store volatile %struct.ScmObj* %cpsprim40402, %struct.ScmObj** %stackaddr$prim47685, align 8
%ae42787 = call %struct.ScmObj* @const_init_int(i64 0)
%args46620$k40399$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47686 = alloca %struct.ScmObj*, align 8
%args46620$k40399$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40402, %struct.ScmObj* %args46620$k40399$0)
store volatile %struct.ScmObj* %args46620$k40399$1, %struct.ScmObj** %stackaddr$prim47686, align 8
%stackaddr$prim47687 = alloca %struct.ScmObj*, align 8
%args46620$k40399$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42787, %struct.ScmObj* %args46620$k40399$1)
store volatile %struct.ScmObj* %args46620$k40399$2, %struct.ScmObj** %stackaddr$prim47687, align 8
%clofunc47688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40399)
musttail call tailcc void %clofunc47688(%struct.ScmObj* %k40399, %struct.ScmObj* %args46620$k40399$2)
ret void
falsebranch$cmp47684:
%ae42798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42798)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim47689, align 8
%stackaddr$prim47690 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim47690, align 8
%ae42801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47691 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42801, %struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim47691, align 8
%ae42804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47692 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42804)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim47692, align 8
%ae42806 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40312, %struct.ScmObj* %ae42806)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim47693, align 8
%ae42808 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42808, %struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim47694, align 8
%args46621$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%args46621$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46621$cc40183$0)
store volatile %struct.ScmObj* %args46621$cc40183$1, %struct.ScmObj** %stackaddr$prim47695, align 8
%stackaddr$prim47696 = alloca %struct.ScmObj*, align 8
%args46621$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40399, %struct.ScmObj* %args46621$cc40183$1)
store volatile %struct.ScmObj* %args46621$cc40183$2, %struct.ScmObj** %stackaddr$prim47696, align 8
%clofunc47697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc47697(%struct.ScmObj* %cc40183, %struct.ScmObj* %args46621$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42763(%struct.ScmObj* %env$ae42763,%struct.ScmObj* %current_45args46623) {
%stackaddr$prim47698 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$prim47698, align 8
%stackaddr$prim47699 = alloca %struct.ScmObj*, align 8
%current_45args46624 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46623)
store volatile %struct.ScmObj* %current_45args46624, %struct.ScmObj** %stackaddr$prim47699, align 8
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46624)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim47700, align 8
%args46626$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47701 = alloca %struct.ScmObj*, align 8
%args46626$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args46626$u40184$0)
store volatile %struct.ScmObj* %args46626$u40184$1, %struct.ScmObj** %stackaddr$prim47701, align 8
%stackaddr$prim47702 = alloca %struct.ScmObj*, align 8
%args46626$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40403, %struct.ScmObj* %args46626$u40184$1)
store volatile %struct.ScmObj* %args46626$u40184$2, %struct.ScmObj** %stackaddr$prim47702, align 8
%clofunc47703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc47703(%struct.ScmObj* %u40184, %struct.ScmObj* %args46626$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42340(%struct.ScmObj* %env$ae42340,%struct.ScmObj* %current_45args46629) {
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46629)
store volatile %struct.ScmObj* %k40404, %struct.ScmObj** %stackaddr$prim47704, align 8
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%current_45args46630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46629)
store volatile %struct.ScmObj* %current_45args46630, %struct.ScmObj** %stackaddr$prim47705, align 8
%stackaddr$prim47706 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46630)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim47706, align 8
%ae42341 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42341, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim47707, align 8
%stackaddr$makeclosure47708 = alloca %struct.ScmObj*, align 8
%fptrToInt47709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42343 to i64
%ae42343 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47709)
store volatile %struct.ScmObj* %ae42343, %struct.ScmObj** %stackaddr$makeclosure47708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42343, %struct.ScmObj* %k40404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42343, %struct.ScmObj* %a40189, i64 1)
%ae42344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47710 = alloca %struct.ScmObj*, align 8
%fptrToInt47711 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42345 to i64
%ae42345 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47711)
store volatile %struct.ScmObj* %ae42345, %struct.ScmObj** %stackaddr$makeclosure47710, align 8
%args46652$ae42343$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%args46652$ae42343$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42345, %struct.ScmObj* %args46652$ae42343$0)
store volatile %struct.ScmObj* %args46652$ae42343$1, %struct.ScmObj** %stackaddr$prim47712, align 8
%stackaddr$prim47713 = alloca %struct.ScmObj*, align 8
%args46652$ae42343$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42344, %struct.ScmObj* %args46652$ae42343$1)
store volatile %struct.ScmObj* %args46652$ae42343$2, %struct.ScmObj** %stackaddr$prim47713, align 8
%clofunc47714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42343)
musttail call tailcc void %clofunc47714(%struct.ScmObj* %ae42343, %struct.ScmObj* %args46652$ae42343$2)
ret void
}

define tailcc void @proc_clo$ae42343(%struct.ScmObj* %env$ae42343,%struct.ScmObj* %current_45args46632) {
%stackaddr$env-ref47715 = alloca %struct.ScmObj*, align 8
%k40404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42343, i64 0)
store %struct.ScmObj* %k40404, %struct.ScmObj** %stackaddr$env-ref47715
%stackaddr$env-ref47716 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42343, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47716
%stackaddr$prim47717 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim47717, align 8
%stackaddr$prim47718 = alloca %struct.ScmObj*, align 8
%current_45args46633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46632)
store volatile %struct.ScmObj* %current_45args46633, %struct.ScmObj** %stackaddr$prim47718, align 8
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46633)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47719, align 8
%stackaddr$makeclosure47720 = alloca %struct.ScmObj*, align 8
%fptrToInt47721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42362 to i64
%ae42362 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47721)
store volatile %struct.ScmObj* %ae42362, %struct.ScmObj** %stackaddr$makeclosure47720, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42362, %struct.ScmObj* %k40404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42362, %struct.ScmObj* %a40189, i64 1)
%stackaddr$makeclosure47722 = alloca %struct.ScmObj*, align 8
%fptrToInt47723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42363 to i64
%ae42363 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47723)
store volatile %struct.ScmObj* %ae42363, %struct.ScmObj** %stackaddr$makeclosure47722, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42363, %struct.ScmObj* %k40404, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42363, %struct.ScmObj* %a40189, i64 1)
%args46647$anf_45bind40299$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47724 = alloca %struct.ScmObj*, align 8
%args46647$anf_45bind40299$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42363, %struct.ScmObj* %args46647$anf_45bind40299$0)
store volatile %struct.ScmObj* %args46647$anf_45bind40299$1, %struct.ScmObj** %stackaddr$prim47724, align 8
%stackaddr$prim47725 = alloca %struct.ScmObj*, align 8
%args46647$anf_45bind40299$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42362, %struct.ScmObj* %args46647$anf_45bind40299$1)
store volatile %struct.ScmObj* %args46647$anf_45bind40299$2, %struct.ScmObj** %stackaddr$prim47725, align 8
%clofunc47726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40299)
musttail call tailcc void %clofunc47726(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %args46647$anf_45bind40299$2)
ret void
}

define tailcc void @proc_clo$ae42362(%struct.ScmObj* %env$ae42362,%struct.ScmObj* %current_45args46635) {
%stackaddr$env-ref47727 = alloca %struct.ScmObj*, align 8
%k40404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42362, i64 0)
store %struct.ScmObj* %k40404, %struct.ScmObj** %stackaddr$env-ref47727
%stackaddr$env-ref47728 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42362, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47728
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim47729, align 8
%stackaddr$prim47730 = alloca %struct.ScmObj*, align 8
%current_45args46636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46635)
store volatile %struct.ScmObj* %current_45args46636, %struct.ScmObj** %stackaddr$prim47730, align 8
%stackaddr$prim47731 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46636)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47731, align 8
%ae42478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47732 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42478)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47732, align 8
%stackaddr$prim47733 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40300)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47733, align 8
%truthy$cmp47734 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40301)
%cmp$cmp47734 = icmp eq i64 %truthy$cmp47734, 1
br i1 %cmp$cmp47734, label %truebranch$cmp47734, label %falsebranch$cmp47734
truebranch$cmp47734:
%ae42482 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42483 = call %struct.ScmObj* @const_init_true()
%args46638$k40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47735 = alloca %struct.ScmObj*, align 8
%args46638$k40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42483, %struct.ScmObj* %args46638$k40404$0)
store volatile %struct.ScmObj* %args46638$k40404$1, %struct.ScmObj** %stackaddr$prim47735, align 8
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%args46638$k40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42482, %struct.ScmObj* %args46638$k40404$1)
store volatile %struct.ScmObj* %args46638$k40404$2, %struct.ScmObj** %stackaddr$prim47736, align 8
%clofunc47737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40404)
musttail call tailcc void %clofunc47737(%struct.ScmObj* %k40404, %struct.ScmObj* %args46638$k40404$2)
ret void
falsebranch$cmp47734:
%ae42491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47738 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42491)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim47738, align 8
%stackaddr$prim47739 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47739, align 8
%truthy$cmp47740 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40303)
%cmp$cmp47740 = icmp eq i64 %truthy$cmp47740, 1
br i1 %cmp$cmp47740, label %truebranch$cmp47740, label %falsebranch$cmp47740
truebranch$cmp47740:
%ae42495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47741 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42495)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47741, align 8
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47742, align 8
%ae42498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47743 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42498)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47743, align 8
%stackaddr$prim47744 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47744, align 8
%ae42501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47745 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42501, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47745, align 8
%args46639$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%args46639$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46639$cc40190$0)
store volatile %struct.ScmObj* %args46639$cc40190$1, %struct.ScmObj** %stackaddr$prim47746, align 8
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%args46639$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40404, %struct.ScmObj* %args46639$cc40190$1)
store volatile %struct.ScmObj* %args46639$cc40190$2, %struct.ScmObj** %stackaddr$prim47747, align 8
%clofunc47748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47748(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46639$cc40190$2)
ret void
falsebranch$cmp47740:
%ae42534 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42535 = call %struct.ScmObj* @const_init_false()
%args46640$k40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47749 = alloca %struct.ScmObj*, align 8
%args46640$k40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42535, %struct.ScmObj* %args46640$k40404$0)
store volatile %struct.ScmObj* %args46640$k40404$1, %struct.ScmObj** %stackaddr$prim47749, align 8
%stackaddr$prim47750 = alloca %struct.ScmObj*, align 8
%args46640$k40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42534, %struct.ScmObj* %args46640$k40404$1)
store volatile %struct.ScmObj* %args46640$k40404$2, %struct.ScmObj** %stackaddr$prim47750, align 8
%clofunc47751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40404)
musttail call tailcc void %clofunc47751(%struct.ScmObj* %k40404, %struct.ScmObj* %args46640$k40404$2)
ret void
}

define tailcc void @proc_clo$ae42363(%struct.ScmObj* %env$ae42363,%struct.ScmObj* %current_45args46641) {
%stackaddr$env-ref47752 = alloca %struct.ScmObj*, align 8
%k40404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42363, i64 0)
store %struct.ScmObj* %k40404, %struct.ScmObj** %stackaddr$env-ref47752
%stackaddr$env-ref47753 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42363, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref47753
%stackaddr$prim47754 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim47754, align 8
%stackaddr$prim47755 = alloca %struct.ScmObj*, align 8
%current_45args46642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46641)
store volatile %struct.ScmObj* %current_45args46642, %struct.ScmObj** %stackaddr$prim47755, align 8
%stackaddr$prim47756 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46642)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim47756, align 8
%ae42365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47757 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42365)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim47757, align 8
%stackaddr$prim47758 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40300)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim47758, align 8
%truthy$cmp47759 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40301)
%cmp$cmp47759 = icmp eq i64 %truthy$cmp47759, 1
br i1 %cmp$cmp47759, label %truebranch$cmp47759, label %falsebranch$cmp47759
truebranch$cmp47759:
%ae42369 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42370 = call %struct.ScmObj* @const_init_true()
%args46644$k40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%args46644$k40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42370, %struct.ScmObj* %args46644$k40404$0)
store volatile %struct.ScmObj* %args46644$k40404$1, %struct.ScmObj** %stackaddr$prim47760, align 8
%stackaddr$prim47761 = alloca %struct.ScmObj*, align 8
%args46644$k40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42369, %struct.ScmObj* %args46644$k40404$1)
store volatile %struct.ScmObj* %args46644$k40404$2, %struct.ScmObj** %stackaddr$prim47761, align 8
%clofunc47762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40404)
musttail call tailcc void %clofunc47762(%struct.ScmObj* %k40404, %struct.ScmObj* %args46644$k40404$2)
ret void
falsebranch$cmp47759:
%ae42378 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47763 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42378)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim47763, align 8
%stackaddr$prim47764 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40302)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim47764, align 8
%truthy$cmp47765 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40303)
%cmp$cmp47765 = icmp eq i64 %truthy$cmp47765, 1
br i1 %cmp$cmp47765, label %truebranch$cmp47765, label %falsebranch$cmp47765
truebranch$cmp47765:
%ae42382 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42382)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim47766, align 8
%stackaddr$prim47767 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim47767, align 8
%ae42385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47768 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42385)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim47768, align 8
%stackaddr$prim47769 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim47769, align 8
%ae42388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47770 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42388, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim47770, align 8
%args46645$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%args46645$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46645$cc40190$0)
store volatile %struct.ScmObj* %args46645$cc40190$1, %struct.ScmObj** %stackaddr$prim47771, align 8
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%args46645$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40404, %struct.ScmObj* %args46645$cc40190$1)
store volatile %struct.ScmObj* %args46645$cc40190$2, %struct.ScmObj** %stackaddr$prim47772, align 8
%clofunc47773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc47773(%struct.ScmObj* %cc40190, %struct.ScmObj* %args46645$cc40190$2)
ret void
falsebranch$cmp47765:
%ae42421 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42422 = call %struct.ScmObj* @const_init_false()
%args46646$k40404$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47774 = alloca %struct.ScmObj*, align 8
%args46646$k40404$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42422, %struct.ScmObj* %args46646$k40404$0)
store volatile %struct.ScmObj* %args46646$k40404$1, %struct.ScmObj** %stackaddr$prim47774, align 8
%stackaddr$prim47775 = alloca %struct.ScmObj*, align 8
%args46646$k40404$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42421, %struct.ScmObj* %args46646$k40404$1)
store volatile %struct.ScmObj* %args46646$k40404$2, %struct.ScmObj** %stackaddr$prim47775, align 8
%clofunc47776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40404)
musttail call tailcc void %clofunc47776(%struct.ScmObj* %k40404, %struct.ScmObj* %args46646$k40404$2)
ret void
}

define tailcc void @proc_clo$ae42345(%struct.ScmObj* %env$ae42345,%struct.ScmObj* %current_45args46648) {
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46648)
store volatile %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$prim47777, align 8
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%current_45args46649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46648)
store volatile %struct.ScmObj* %current_45args46649, %struct.ScmObj** %stackaddr$prim47778, align 8
%stackaddr$prim47779 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46649)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim47779, align 8
%ae42347 = call %struct.ScmObj* @const_init_int(i64 0)
%args46651$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47780 = alloca %struct.ScmObj*, align 8
%args46651$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args46651$k40407$0)
store volatile %struct.ScmObj* %args46651$k40407$1, %struct.ScmObj** %stackaddr$prim47780, align 8
%stackaddr$prim47781 = alloca %struct.ScmObj*, align 8
%args46651$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42347, %struct.ScmObj* %args46651$k40407$1)
store volatile %struct.ScmObj* %args46651$k40407$2, %struct.ScmObj** %stackaddr$prim47781, align 8
%clofunc47782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc47782(%struct.ScmObj* %k40407, %struct.ScmObj* %args46651$k40407$2)
ret void
}

define tailcc void @proc_clo$ae42268(%struct.ScmObj* %env$ae42268,%struct.ScmObj* %current_45args46654) {
%stackaddr$env-ref47783 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42268, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47783
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46654)
store volatile %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$prim47784, align 8
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%current_45args46655 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46654)
store volatile %struct.ScmObj* %current_45args46655, %struct.ScmObj** %stackaddr$prim47785, align 8
%stackaddr$prim47786 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim47786, align 8
%stackaddr$prim47787 = alloca %struct.ScmObj*, align 8
%current_45args46656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46655)
store volatile %struct.ScmObj* %current_45args46656, %struct.ScmObj** %stackaddr$prim47787, align 8
%stackaddr$prim47788 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46656)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim47788, align 8
%stackaddr$prim47789 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim47789, align 8
%truthy$cmp47790 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40293)
%cmp$cmp47790 = icmp eq i64 %truthy$cmp47790, 1
br i1 %cmp$cmp47790, label %truebranch$cmp47790, label %falsebranch$cmp47790
truebranch$cmp47790:
%ae42272 = call %struct.ScmObj* @const_init_int(i64 0)
%args46658$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%args46658$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46658$k40408$0)
store volatile %struct.ScmObj* %args46658$k40408$1, %struct.ScmObj** %stackaddr$prim47791, align 8
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%args46658$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42272, %struct.ScmObj* %args46658$k40408$1)
store volatile %struct.ScmObj* %args46658$k40408$2, %struct.ScmObj** %stackaddr$prim47792, align 8
%clofunc47793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47793(%struct.ScmObj* %k40408, %struct.ScmObj* %args46658$k40408$2)
ret void
falsebranch$cmp47790:
%stackaddr$prim47794 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim47794, align 8
%ae42279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47795 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42279)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim47795, align 8
%stackaddr$prim47796 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim47796, align 8
%stackaddr$makeclosure47797 = alloca %struct.ScmObj*, align 8
%fptrToInt47798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42282 to i64
%ae42282 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47798)
store volatile %struct.ScmObj* %ae42282, %struct.ScmObj** %stackaddr$makeclosure47797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42282, %struct.ScmObj* %anf_45bind40294, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42282, %struct.ScmObj* %k40408, i64 1)
%args46663$anf_45bind40295$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47799 = alloca %struct.ScmObj*, align 8
%args46663$anf_45bind40295$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args46663$anf_45bind40295$0)
store volatile %struct.ScmObj* %args46663$anf_45bind40295$1, %struct.ScmObj** %stackaddr$prim47799, align 8
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%args46663$anf_45bind40295$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40296, %struct.ScmObj* %args46663$anf_45bind40295$1)
store volatile %struct.ScmObj* %args46663$anf_45bind40295$2, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%args46663$anf_45bind40295$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42282, %struct.ScmObj* %args46663$anf_45bind40295$2)
store volatile %struct.ScmObj* %args46663$anf_45bind40295$3, %struct.ScmObj** %stackaddr$prim47801, align 8
%clofunc47802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40295)
musttail call tailcc void %clofunc47802(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args46663$anf_45bind40295$3)
ret void
}

define tailcc void @proc_clo$ae42282(%struct.ScmObj* %env$ae42282,%struct.ScmObj* %current_45args46659) {
%stackaddr$env-ref47803 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42282, i64 0)
store %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$env-ref47803
%stackaddr$env-ref47804 = alloca %struct.ScmObj*, align 8
%k40408 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42282, i64 1)
store %struct.ScmObj* %k40408, %struct.ScmObj** %stackaddr$env-ref47804
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim47805, align 8
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%current_45args46660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46659)
store volatile %struct.ScmObj* %current_45args46660, %struct.ScmObj** %stackaddr$prim47806, align 8
%stackaddr$prim47807 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46660)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47807, align 8
%stackaddr$prim47808 = alloca %struct.ScmObj*, align 8
%cpsprim40410 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsprim40410, %struct.ScmObj** %stackaddr$prim47808, align 8
%ae42288 = call %struct.ScmObj* @const_init_int(i64 0)
%args46662$k40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47809 = alloca %struct.ScmObj*, align 8
%args46662$k40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40410, %struct.ScmObj* %args46662$k40408$0)
store volatile %struct.ScmObj* %args46662$k40408$1, %struct.ScmObj** %stackaddr$prim47809, align 8
%stackaddr$prim47810 = alloca %struct.ScmObj*, align 8
%args46662$k40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42288, %struct.ScmObj* %args46662$k40408$1)
store volatile %struct.ScmObj* %args46662$k40408$2, %struct.ScmObj** %stackaddr$prim47810, align 8
%clofunc47811 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40408)
musttail call tailcc void %clofunc47811(%struct.ScmObj* %k40408, %struct.ScmObj* %args46662$k40408$2)
ret void
}

define tailcc void @proc_clo$ae42242(%struct.ScmObj* %env$ae42242,%struct.ScmObj* %current_45args46665) {
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46665)
store volatile %struct.ScmObj* %k40411, %struct.ScmObj** %stackaddr$prim47812, align 8
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%current_45args46666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46665)
store volatile %struct.ScmObj* %current_45args46666, %struct.ScmObj** %stackaddr$prim47813, align 8
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46666)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim47814, align 8
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%current_45args46667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46666)
store volatile %struct.ScmObj* %current_45args46667, %struct.ScmObj** %stackaddr$prim47815, align 8
%stackaddr$prim47816 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46667)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim47816, align 8
%stackaddr$prim47817 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim47817, align 8
%stackaddr$prim47818 = alloca %struct.ScmObj*, align 8
%cpsprim40412 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %cpsprim40412, %struct.ScmObj** %stackaddr$prim47818, align 8
%ae42247 = call %struct.ScmObj* @const_init_int(i64 0)
%args46669$k40411$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%args46669$k40411$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40412, %struct.ScmObj* %args46669$k40411$0)
store volatile %struct.ScmObj* %args46669$k40411$1, %struct.ScmObj** %stackaddr$prim47819, align 8
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%args46669$k40411$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42247, %struct.ScmObj* %args46669$k40411$1)
store volatile %struct.ScmObj* %args46669$k40411$2, %struct.ScmObj** %stackaddr$prim47820, align 8
%clofunc47821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40411)
musttail call tailcc void %clofunc47821(%struct.ScmObj* %k40411, %struct.ScmObj* %args46669$k40411$2)
ret void
}

define tailcc void @proc_clo$ae42218(%struct.ScmObj* %env$ae42218,%struct.ScmObj* %current_45args46671) {
%stackaddr$prim47822 = alloca %struct.ScmObj*, align 8
%k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %k40413, %struct.ScmObj** %stackaddr$prim47822, align 8
%stackaddr$prim47823 = alloca %struct.ScmObj*, align 8
%current_45args46672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46671)
store volatile %struct.ScmObj* %current_45args46672, %struct.ScmObj** %stackaddr$prim47823, align 8
%stackaddr$prim47824 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46672)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim47824, align 8
%stackaddr$prim47825 = alloca %struct.ScmObj*, align 8
%current_45args46673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46672)
store volatile %struct.ScmObj* %current_45args46673, %struct.ScmObj** %stackaddr$prim47825, align 8
%stackaddr$prim47826 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46673)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim47826, align 8
%stackaddr$prim47827 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim47827, align 8
%stackaddr$prim47828 = alloca %struct.ScmObj*, align 8
%cpsprim40414 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40291)
store volatile %struct.ScmObj* %cpsprim40414, %struct.ScmObj** %stackaddr$prim47828, align 8
%ae42223 = call %struct.ScmObj* @const_init_int(i64 0)
%args46675$k40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47829 = alloca %struct.ScmObj*, align 8
%args46675$k40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40414, %struct.ScmObj* %args46675$k40413$0)
store volatile %struct.ScmObj* %args46675$k40413$1, %struct.ScmObj** %stackaddr$prim47829, align 8
%stackaddr$prim47830 = alloca %struct.ScmObj*, align 8
%args46675$k40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42223, %struct.ScmObj* %args46675$k40413$1)
store volatile %struct.ScmObj* %args46675$k40413$2, %struct.ScmObj** %stackaddr$prim47830, align 8
%clofunc47831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40413)
musttail call tailcc void %clofunc47831(%struct.ScmObj* %k40413, %struct.ScmObj* %args46675$k40413$2)
ret void
}

define tailcc void @proc_clo$ae41824(%struct.ScmObj* %env$ae41824,%struct.ScmObj* %current_45args46678) {
%stackaddr$env-ref47832 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47832
%stackaddr$env-ref47833 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47833
%stackaddr$env-ref47834 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41824, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47834
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %k40415, %struct.ScmObj** %stackaddr$prim47835, align 8
%stackaddr$prim47836 = alloca %struct.ScmObj*, align 8
%current_45args46679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46678)
store volatile %struct.ScmObj* %current_45args46679, %struct.ScmObj** %stackaddr$prim47836, align 8
%stackaddr$prim47837 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46679)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim47837, align 8
%ae41826 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47838 = alloca %struct.ScmObj*, align 8
%fptrToInt47839 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41827 to i64
%ae41827 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47839)
store volatile %struct.ScmObj* %ae41827, %struct.ScmObj** %stackaddr$makeclosure47838, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41827, %struct.ScmObj* %_37map140154, i64 3)
%args46736$k40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47840 = alloca %struct.ScmObj*, align 8
%args46736$k40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41827, %struct.ScmObj* %args46736$k40415$0)
store volatile %struct.ScmObj* %args46736$k40415$1, %struct.ScmObj** %stackaddr$prim47840, align 8
%stackaddr$prim47841 = alloca %struct.ScmObj*, align 8
%args46736$k40415$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41826, %struct.ScmObj* %args46736$k40415$1)
store volatile %struct.ScmObj* %args46736$k40415$2, %struct.ScmObj** %stackaddr$prim47841, align 8
%clofunc47842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40415)
musttail call tailcc void %clofunc47842(%struct.ScmObj* %k40415, %struct.ScmObj* %args46736$k40415$2)
ret void
}

define tailcc void @proc_clo$ae41827(%struct.ScmObj* %env$ae41827,%struct.ScmObj* %args4020740416) {
%stackaddr$env-ref47843 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47843
%stackaddr$env-ref47844 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47844
%stackaddr$env-ref47845 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47845
%stackaddr$env-ref47846 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41827, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47846
%stackaddr$prim47847 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740416)
store volatile %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$prim47847, align 8
%stackaddr$prim47848 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740416)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim47848, align 8
%stackaddr$prim47849 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim47849, align 8
%stackaddr$prim47850 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim47850, align 8
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim47852, align 8
%stackaddr$prim47853 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40280)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim47853, align 8
%stackaddr$makeclosure47854 = alloca %struct.ScmObj*, align 8
%fptrToInt47855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41835 to i64
%ae41835 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47855)
store volatile %struct.ScmObj* %ae41835, %struct.ScmObj** %stackaddr$makeclosure47854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %lsts40208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41835, %struct.ScmObj* %_37map140154, i64 7)
%ae41836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47856 = alloca %struct.ScmObj*, align 8
%fptrToInt47857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41837 to i64
%ae41837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47857)
store volatile %struct.ScmObj* %ae41837, %struct.ScmObj** %stackaddr$makeclosure47856, align 8
%args46735$ae41835$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%args46735$ae41835$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41837, %struct.ScmObj* %args46735$ae41835$0)
store volatile %struct.ScmObj* %args46735$ae41835$1, %struct.ScmObj** %stackaddr$prim47858, align 8
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%args46735$ae41835$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41836, %struct.ScmObj* %args46735$ae41835$1)
store volatile %struct.ScmObj* %args46735$ae41835$2, %struct.ScmObj** %stackaddr$prim47859, align 8
%clofunc47860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41835)
musttail call tailcc void %clofunc47860(%struct.ScmObj* %ae41835, %struct.ScmObj* %args46735$ae41835$2)
ret void
}

define tailcc void @proc_clo$ae41835(%struct.ScmObj* %env$ae41835,%struct.ScmObj* %current_45args46681) {
%stackaddr$env-ref47861 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47861
%stackaddr$env-ref47862 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47862
%stackaddr$env-ref47863 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 2)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47863
%stackaddr$env-ref47864 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47864
%stackaddr$env-ref47865 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47865
%stackaddr$env-ref47866 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47866
%stackaddr$env-ref47867 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47867
%stackaddr$env-ref47868 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41835, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47868
%stackaddr$prim47869 = alloca %struct.ScmObj*, align 8
%_95k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46681)
store volatile %struct.ScmObj* %_95k40418, %struct.ScmObj** %stackaddr$prim47869, align 8
%stackaddr$prim47870 = alloca %struct.ScmObj*, align 8
%current_45args46682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46681)
store volatile %struct.ScmObj* %current_45args46682, %struct.ScmObj** %stackaddr$prim47870, align 8
%stackaddr$prim47871 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46682)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim47871, align 8
%stackaddr$makeclosure47872 = alloca %struct.ScmObj*, align 8
%fptrToInt47873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41867 to i64
%ae41867 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47873)
store volatile %struct.ScmObj* %ae41867, %struct.ScmObj** %stackaddr$makeclosure47872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %lsts40208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41867, %struct.ScmObj* %_37map140154, i64 6)
%ae41869 = call %struct.ScmObj* @const_init_false()
%args46728$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47874 = alloca %struct.ScmObj*, align 8
%args46728$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46728$_37foldr140123$0)
store volatile %struct.ScmObj* %args46728$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim47874, align 8
%stackaddr$prim47875 = alloca %struct.ScmObj*, align 8
%args46728$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41869, %struct.ScmObj* %args46728$_37foldr140123$1)
store volatile %struct.ScmObj* %args46728$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim47875, align 8
%stackaddr$prim47876 = alloca %struct.ScmObj*, align 8
%args46728$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %args46728$_37foldr140123$2)
store volatile %struct.ScmObj* %args46728$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim47876, align 8
%stackaddr$prim47877 = alloca %struct.ScmObj*, align 8
%args46728$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41867, %struct.ScmObj* %args46728$_37foldr140123$3)
store volatile %struct.ScmObj* %args46728$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim47877, align 8
%clofunc47878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc47878(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46728$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41867(%struct.ScmObj* %env$ae41867,%struct.ScmObj* %current_45args46684) {
%stackaddr$env-ref47879 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47879
%stackaddr$env-ref47880 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47880
%stackaddr$env-ref47881 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 2)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47881
%stackaddr$env-ref47882 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47882
%stackaddr$env-ref47883 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47883
%stackaddr$env-ref47884 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47884
%stackaddr$env-ref47885 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41867, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47885
%stackaddr$prim47886 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim47886, align 8
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%current_45args46685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46684)
store volatile %struct.ScmObj* %current_45args46685, %struct.ScmObj** %stackaddr$prim47887, align 8
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46685)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim47888, align 8
%truthy$cmp47889 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40282)
%cmp$cmp47889 = icmp eq i64 %truthy$cmp47889, 1
br i1 %cmp$cmp47889, label %truebranch$cmp47889, label %falsebranch$cmp47889
truebranch$cmp47889:
%ae41878 = call %struct.ScmObj* @const_init_int(i64 0)
%args46687$k40417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47890 = alloca %struct.ScmObj*, align 8
%args46687$k40417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args46687$k40417$0)
store volatile %struct.ScmObj* %args46687$k40417$1, %struct.ScmObj** %stackaddr$prim47890, align 8
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%args46687$k40417$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41878, %struct.ScmObj* %args46687$k40417$1)
store volatile %struct.ScmObj* %args46687$k40417$2, %struct.ScmObj** %stackaddr$prim47891, align 8
%clofunc47892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40417)
musttail call tailcc void %clofunc47892(%struct.ScmObj* %k40417, %struct.ScmObj* %args46687$k40417$2)
ret void
falsebranch$cmp47889:
%stackaddr$makeclosure47893 = alloca %struct.ScmObj*, align 8
%fptrToInt47894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41883 to i64
%ae41883 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47894)
store volatile %struct.ScmObj* %ae41883, %struct.ScmObj** %stackaddr$makeclosure47893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %lsts40208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41883, %struct.ScmObj* %_37map140154, i64 6)
%ae41884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47895 = alloca %struct.ScmObj*, align 8
%fptrToInt47896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41885 to i64
%ae41885 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47896)
store volatile %struct.ScmObj* %ae41885, %struct.ScmObj** %stackaddr$makeclosure47895, align 8
%args46727$ae41883$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47897 = alloca %struct.ScmObj*, align 8
%args46727$ae41883$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41885, %struct.ScmObj* %args46727$ae41883$0)
store volatile %struct.ScmObj* %args46727$ae41883$1, %struct.ScmObj** %stackaddr$prim47897, align 8
%stackaddr$prim47898 = alloca %struct.ScmObj*, align 8
%args46727$ae41883$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41884, %struct.ScmObj* %args46727$ae41883$1)
store volatile %struct.ScmObj* %args46727$ae41883$2, %struct.ScmObj** %stackaddr$prim47898, align 8
%clofunc47899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41883)
musttail call tailcc void %clofunc47899(%struct.ScmObj* %ae41883, %struct.ScmObj* %args46727$ae41883$2)
ret void
}

define tailcc void @proc_clo$ae41883(%struct.ScmObj* %env$ae41883,%struct.ScmObj* %current_45args46688) {
%stackaddr$env-ref47900 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47900
%stackaddr$env-ref47901 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47901
%stackaddr$env-ref47902 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 2)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47902
%stackaddr$env-ref47903 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47903
%stackaddr$env-ref47904 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47904
%stackaddr$env-ref47905 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47905
%stackaddr$env-ref47906 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41883, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47906
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46688)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim47907, align 8
%stackaddr$prim47908 = alloca %struct.ScmObj*, align 8
%current_45args46689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46688)
store volatile %struct.ScmObj* %current_45args46689, %struct.ScmObj** %stackaddr$prim47908, align 8
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46689)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$makeclosure47910 = alloca %struct.ScmObj*, align 8
%fptrToInt47911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41904 to i64
%ae41904 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47911)
store volatile %struct.ScmObj* %ae41904, %struct.ScmObj** %stackaddr$makeclosure47910, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %lsts40208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41904, %struct.ScmObj* %_37map140154, i64 6)
%args46722$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47912 = alloca %struct.ScmObj*, align 8
%args46722$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46722$_37map140154$0)
store volatile %struct.ScmObj* %args46722$_37map140154$1, %struct.ScmObj** %stackaddr$prim47912, align 8
%stackaddr$prim47913 = alloca %struct.ScmObj*, align 8
%args46722$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args46722$_37map140154$1)
store volatile %struct.ScmObj* %args46722$_37map140154$2, %struct.ScmObj** %stackaddr$prim47913, align 8
%stackaddr$prim47914 = alloca %struct.ScmObj*, align 8
%args46722$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41904, %struct.ScmObj* %args46722$_37map140154$2)
store volatile %struct.ScmObj* %args46722$_37map140154$3, %struct.ScmObj** %stackaddr$prim47914, align 8
%clofunc47915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc47915(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46722$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41904(%struct.ScmObj* %env$ae41904,%struct.ScmObj* %current_45args46691) {
%stackaddr$env-ref47916 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47916
%stackaddr$env-ref47917 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47917
%stackaddr$env-ref47918 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 2)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47918
%stackaddr$env-ref47919 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47919
%stackaddr$env-ref47920 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47920
%stackaddr$env-ref47921 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47921
%stackaddr$env-ref47922 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41904, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47922
%stackaddr$prim47923 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim47923, align 8
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%current_45args46692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46691)
store volatile %struct.ScmObj* %current_45args46692, %struct.ScmObj** %stackaddr$prim47924, align 8
%stackaddr$prim47925 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46692)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim47925, align 8
%stackaddr$makeclosure47926 = alloca %struct.ScmObj*, align 8
%fptrToInt47927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41907 to i64
%ae41907 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt47927)
store volatile %struct.ScmObj* %ae41907, %struct.ScmObj** %stackaddr$makeclosure47926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %lsts40208, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %lsts_4340215, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %f40210, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37foldl40206, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41907, %struct.ScmObj* %_37map140154, i64 7)
%ae41908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47928 = alloca %struct.ScmObj*, align 8
%fptrToInt47929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41909 to i64
%ae41909 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47929)
store volatile %struct.ScmObj* %ae41909, %struct.ScmObj** %stackaddr$makeclosure47928, align 8
%args46721$ae41907$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%args46721$ae41907$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41909, %struct.ScmObj* %args46721$ae41907$0)
store volatile %struct.ScmObj* %args46721$ae41907$1, %struct.ScmObj** %stackaddr$prim47930, align 8
%stackaddr$prim47931 = alloca %struct.ScmObj*, align 8
%args46721$ae41907$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41908, %struct.ScmObj* %args46721$ae41907$1)
store volatile %struct.ScmObj* %args46721$ae41907$2, %struct.ScmObj** %stackaddr$prim47931, align 8
%clofunc47932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41907)
musttail call tailcc void %clofunc47932(%struct.ScmObj* %ae41907, %struct.ScmObj* %args46721$ae41907$2)
ret void
}

define tailcc void @proc_clo$ae41907(%struct.ScmObj* %env$ae41907,%struct.ScmObj* %current_45args46694) {
%stackaddr$env-ref47933 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47933
%stackaddr$env-ref47934 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47934
%stackaddr$env-ref47935 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 2)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref47935
%stackaddr$env-ref47936 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47936
%stackaddr$env-ref47937 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 4)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref47937
%stackaddr$env-ref47938 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 5)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47938
%stackaddr$env-ref47939 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47939
%stackaddr$env-ref47940 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41907, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47940
%stackaddr$prim47941 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim47941, align 8
%stackaddr$prim47942 = alloca %struct.ScmObj*, align 8
%current_45args46695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46694)
store volatile %struct.ScmObj* %current_45args46695, %struct.ScmObj** %stackaddr$prim47942, align 8
%stackaddr$prim47943 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46695)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim47943, align 8
%stackaddr$makeclosure47944 = alloca %struct.ScmObj*, align 8
%fptrToInt47945 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41928 to i64
%ae41928 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47945)
store volatile %struct.ScmObj* %ae41928, %struct.ScmObj** %stackaddr$makeclosure47944, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41928, %struct.ScmObj* %_37foldl40206, i64 5)
%args46716$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%args46716$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args46716$_37map140154$0)
store volatile %struct.ScmObj* %args46716$_37map140154$1, %struct.ScmObj** %stackaddr$prim47946, align 8
%stackaddr$prim47947 = alloca %struct.ScmObj*, align 8
%args46716$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %args46716$_37map140154$1)
store volatile %struct.ScmObj* %args46716$_37map140154$2, %struct.ScmObj** %stackaddr$prim47947, align 8
%stackaddr$prim47948 = alloca %struct.ScmObj*, align 8
%args46716$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41928, %struct.ScmObj* %args46716$_37map140154$2)
store volatile %struct.ScmObj* %args46716$_37map140154$3, %struct.ScmObj** %stackaddr$prim47948, align 8
%clofunc47949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc47949(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args46716$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41928(%struct.ScmObj* %env$ae41928,%struct.ScmObj* %current_45args46697) {
%stackaddr$env-ref47950 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47950
%stackaddr$env-ref47951 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47951
%stackaddr$env-ref47952 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref47952
%stackaddr$env-ref47953 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47953
%stackaddr$env-ref47954 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47954
%stackaddr$env-ref47955 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41928, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47955
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%current_45args46698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46697)
store volatile %struct.ScmObj* %current_45args46698, %struct.ScmObj** %stackaddr$prim47957, align 8
%stackaddr$prim47958 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46698)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim47958, align 8
%stackaddr$makeclosure47959 = alloca %struct.ScmObj*, align 8
%fptrToInt47960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41931 to i64
%ae41931 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt47960)
store volatile %struct.ScmObj* %ae41931, %struct.ScmObj** %stackaddr$makeclosure47959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %acc40209, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %k40417, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %vs40213, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldr40128, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41931, %struct.ScmObj* %_37foldl40206, i64 6)
%ae41932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47961 = alloca %struct.ScmObj*, align 8
%fptrToInt47962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41933 to i64
%ae41933 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47962)
store volatile %struct.ScmObj* %ae41933, %struct.ScmObj** %stackaddr$makeclosure47961, align 8
%args46715$ae41931$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47963 = alloca %struct.ScmObj*, align 8
%args46715$ae41931$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41933, %struct.ScmObj* %args46715$ae41931$0)
store volatile %struct.ScmObj* %args46715$ae41931$1, %struct.ScmObj** %stackaddr$prim47963, align 8
%stackaddr$prim47964 = alloca %struct.ScmObj*, align 8
%args46715$ae41931$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41932, %struct.ScmObj* %args46715$ae41931$1)
store volatile %struct.ScmObj* %args46715$ae41931$2, %struct.ScmObj** %stackaddr$prim47964, align 8
%clofunc47965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41931)
musttail call tailcc void %clofunc47965(%struct.ScmObj* %ae41931, %struct.ScmObj* %args46715$ae41931$2)
ret void
}

define tailcc void @proc_clo$ae41931(%struct.ScmObj* %env$ae41931,%struct.ScmObj* %current_45args46700) {
%stackaddr$env-ref47966 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 0)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref47966
%stackaddr$env-ref47967 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 1)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47967
%stackaddr$env-ref47968 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref47968
%stackaddr$env-ref47969 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 3)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref47969
%stackaddr$env-ref47970 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47970
%stackaddr$env-ref47971 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 5)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47971
%stackaddr$env-ref47972 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41931, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47972
%stackaddr$prim47973 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim47973, align 8
%stackaddr$prim47974 = alloca %struct.ScmObj*, align 8
%current_45args46701 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46700)
store volatile %struct.ScmObj* %current_45args46701, %struct.ScmObj** %stackaddr$prim47974, align 8
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46701)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim47975, align 8
%ae41954 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47976 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae41954)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim47976, align 8
%stackaddr$makeclosure47977 = alloca %struct.ScmObj*, align 8
%fptrToInt47978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41956 to i64
%ae41956 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47978)
store volatile %struct.ScmObj* %ae41956, %struct.ScmObj** %stackaddr$makeclosure47977, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41956, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41956, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41956, %struct.ScmObj* %k40417, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41956, %struct.ScmObj* %_37foldl40206, i64 3)
%args46709$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%args46709$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args46709$_37foldr40128$0)
store volatile %struct.ScmObj* %args46709$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim47979, align 8
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%args46709$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40286, %struct.ScmObj* %args46709$_37foldr40128$1)
store volatile %struct.ScmObj* %args46709$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim47980, align 8
%stackaddr$prim47981 = alloca %struct.ScmObj*, align 8
%args46709$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args46709$_37foldr40128$2)
store volatile %struct.ScmObj* %args46709$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim47981, align 8
%stackaddr$prim47982 = alloca %struct.ScmObj*, align 8
%args46709$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41956, %struct.ScmObj* %args46709$_37foldr40128$3)
store volatile %struct.ScmObj* %args46709$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim47982, align 8
%clofunc47983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc47983(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args46709$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae41956(%struct.ScmObj* %env$ae41956,%struct.ScmObj* %current_45args46703) {
%stackaddr$env-ref47984 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41956, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref47984
%stackaddr$env-ref47985 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41956, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47985
%stackaddr$env-ref47986 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41956, i64 2)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47986
%stackaddr$env-ref47987 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41956, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47987
%stackaddr$prim47988 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim47988, align 8
%stackaddr$prim47989 = alloca %struct.ScmObj*, align 8
%current_45args46704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46703)
store volatile %struct.ScmObj* %current_45args46704, %struct.ScmObj** %stackaddr$prim47989, align 8
%stackaddr$prim47990 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46704)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim47990, align 8
%stackaddr$makeclosure47991 = alloca %struct.ScmObj*, align 8
%fptrToInt47992 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41960 to i64
%ae41960 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47992)
store volatile %struct.ScmObj* %ae41960, %struct.ScmObj** %stackaddr$makeclosure47991, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41960, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41960, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41960, %struct.ScmObj* %k40417, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41960, %struct.ScmObj* %_37foldl40206, i64 3)
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%cpsargs40428 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41960, %struct.ScmObj* %anf_45bind40287)
store volatile %struct.ScmObj* %cpsargs40428, %struct.ScmObj** %stackaddr$prim47993, align 8
%clofunc47994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc47994(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40428)
ret void
}

define tailcc void @proc_clo$ae41960(%struct.ScmObj* %env$ae41960,%struct.ScmObj* %current_45args46706) {
%stackaddr$env-ref47995 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41960, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref47995
%stackaddr$env-ref47996 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41960, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref47996
%stackaddr$env-ref47997 = alloca %struct.ScmObj*, align 8
%k40417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41960, i64 2)
store %struct.ScmObj* %k40417, %struct.ScmObj** %stackaddr$env-ref47997
%stackaddr$env-ref47998 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41960, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref47998
%stackaddr$prim47999 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim47999, align 8
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%current_45args46707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46706)
store volatile %struct.ScmObj* %current_45args46707, %struct.ScmObj** %stackaddr$prim48000, align 8
%stackaddr$prim48001 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46707)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48001, align 8
%stackaddr$prim48002 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48002, align 8
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40288)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48003, align 8
%stackaddr$prim48004 = alloca %struct.ScmObj*, align 8
%cpsargs40427 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40417, %struct.ScmObj* %anf_45bind40289)
store volatile %struct.ScmObj* %cpsargs40427, %struct.ScmObj** %stackaddr$prim48004, align 8
%clofunc48005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48005(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40427)
ret void
}

define tailcc void @proc_clo$ae41933(%struct.ScmObj* %env$ae41933,%struct.ScmObj* %current_45args46710) {
%stackaddr$prim48006 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46710)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim48006, align 8
%stackaddr$prim48007 = alloca %struct.ScmObj*, align 8
%current_45args46711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46710)
store volatile %struct.ScmObj* %current_45args46711, %struct.ScmObj** %stackaddr$prim48007, align 8
%stackaddr$prim48008 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46711)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48008, align 8
%stackaddr$prim48009 = alloca %struct.ScmObj*, align 8
%current_45args46712 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46711)
store volatile %struct.ScmObj* %current_45args46712, %struct.ScmObj** %stackaddr$prim48009, align 8
%stackaddr$prim48010 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46712)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48010, align 8
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%cpsprim40430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40430, %struct.ScmObj** %stackaddr$prim48011, align 8
%ae41937 = call %struct.ScmObj* @const_init_int(i64 0)
%args46714$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48012 = alloca %struct.ScmObj*, align 8
%args46714$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40430, %struct.ScmObj* %args46714$k40429$0)
store volatile %struct.ScmObj* %args46714$k40429$1, %struct.ScmObj** %stackaddr$prim48012, align 8
%stackaddr$prim48013 = alloca %struct.ScmObj*, align 8
%args46714$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41937, %struct.ScmObj* %args46714$k40429$1)
store volatile %struct.ScmObj* %args46714$k40429$2, %struct.ScmObj** %stackaddr$prim48013, align 8
%clofunc48014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc48014(%struct.ScmObj* %k40429, %struct.ScmObj* %args46714$k40429$2)
ret void
}

define tailcc void @proc_clo$ae41909(%struct.ScmObj* %env$ae41909,%struct.ScmObj* %current_45args46717) {
%stackaddr$prim48015 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46717)
store volatile %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$prim48015, align 8
%stackaddr$prim48016 = alloca %struct.ScmObj*, align 8
%current_45args46718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46717)
store volatile %struct.ScmObj* %current_45args46718, %struct.ScmObj** %stackaddr$prim48016, align 8
%stackaddr$prim48017 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46718)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48017, align 8
%stackaddr$prim48018 = alloca %struct.ScmObj*, align 8
%cpsprim40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40432, %struct.ScmObj** %stackaddr$prim48018, align 8
%ae41912 = call %struct.ScmObj* @const_init_int(i64 0)
%args46720$k40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48019 = alloca %struct.ScmObj*, align 8
%args46720$k40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40432, %struct.ScmObj* %args46720$k40431$0)
store volatile %struct.ScmObj* %args46720$k40431$1, %struct.ScmObj** %stackaddr$prim48019, align 8
%stackaddr$prim48020 = alloca %struct.ScmObj*, align 8
%args46720$k40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41912, %struct.ScmObj* %args46720$k40431$1)
store volatile %struct.ScmObj* %args46720$k40431$2, %struct.ScmObj** %stackaddr$prim48020, align 8
%clofunc48021 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40431)
musttail call tailcc void %clofunc48021(%struct.ScmObj* %k40431, %struct.ScmObj* %args46720$k40431$2)
ret void
}

define tailcc void @proc_clo$ae41885(%struct.ScmObj* %env$ae41885,%struct.ScmObj* %current_45args46723) {
%stackaddr$prim48022 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46723)
store volatile %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$prim48022, align 8
%stackaddr$prim48023 = alloca %struct.ScmObj*, align 8
%current_45args46724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46723)
store volatile %struct.ScmObj* %current_45args46724, %struct.ScmObj** %stackaddr$prim48023, align 8
%stackaddr$prim48024 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46724)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48024, align 8
%stackaddr$prim48025 = alloca %struct.ScmObj*, align 8
%cpsprim40434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40434, %struct.ScmObj** %stackaddr$prim48025, align 8
%ae41888 = call %struct.ScmObj* @const_init_int(i64 0)
%args46726$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48026 = alloca %struct.ScmObj*, align 8
%args46726$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40434, %struct.ScmObj* %args46726$k40433$0)
store volatile %struct.ScmObj* %args46726$k40433$1, %struct.ScmObj** %stackaddr$prim48026, align 8
%stackaddr$prim48027 = alloca %struct.ScmObj*, align 8
%args46726$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41888, %struct.ScmObj* %args46726$k40433$1)
store volatile %struct.ScmObj* %args46726$k40433$2, %struct.ScmObj** %stackaddr$prim48027, align 8
%clofunc48028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc48028(%struct.ScmObj* %k40433, %struct.ScmObj* %args46726$k40433$2)
ret void
}

define tailcc void @proc_clo$ae41837(%struct.ScmObj* %env$ae41837,%struct.ScmObj* %current_45args46729) {
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46729)
store volatile %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$prim48029, align 8
%stackaddr$prim48030 = alloca %struct.ScmObj*, align 8
%current_45args46730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46729)
store volatile %struct.ScmObj* %current_45args46730, %struct.ScmObj** %stackaddr$prim48030, align 8
%stackaddr$prim48031 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46730)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48031, align 8
%stackaddr$prim48032 = alloca %struct.ScmObj*, align 8
%current_45args46731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46730)
store volatile %struct.ScmObj* %current_45args46731, %struct.ScmObj** %stackaddr$prim48032, align 8
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46731)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48033, align 8
%truthy$cmp48034 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48034 = icmp eq i64 %truthy$cmp48034, 1
br i1 %cmp$cmp48034, label %truebranch$cmp48034, label %falsebranch$cmp48034
truebranch$cmp48034:
%ae41840 = call %struct.ScmObj* @const_init_int(i64 0)
%args46733$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%args46733$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args46733$k40435$0)
store volatile %struct.ScmObj* %args46733$k40435$1, %struct.ScmObj** %stackaddr$prim48035, align 8
%stackaddr$prim48036 = alloca %struct.ScmObj*, align 8
%args46733$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41840, %struct.ScmObj* %args46733$k40435$1)
store volatile %struct.ScmObj* %args46733$k40435$2, %struct.ScmObj** %stackaddr$prim48036, align 8
%clofunc48037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc48037(%struct.ScmObj* %k40435, %struct.ScmObj* %args46733$k40435$2)
ret void
falsebranch$cmp48034:
%stackaddr$prim48038 = alloca %struct.ScmObj*, align 8
%cpsprim40436 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40436, %struct.ScmObj** %stackaddr$prim48038, align 8
%ae41847 = call %struct.ScmObj* @const_init_int(i64 0)
%args46734$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48039 = alloca %struct.ScmObj*, align 8
%args46734$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40436, %struct.ScmObj* %args46734$k40435$0)
store volatile %struct.ScmObj* %args46734$k40435$1, %struct.ScmObj** %stackaddr$prim48039, align 8
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%args46734$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41847, %struct.ScmObj* %args46734$k40435$1)
store volatile %struct.ScmObj* %args46734$k40435$2, %struct.ScmObj** %stackaddr$prim48040, align 8
%clofunc48041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc48041(%struct.ScmObj* %k40435, %struct.ScmObj* %args46734$k40435$2)
ret void
}

define tailcc void @proc_clo$ae41678(%struct.ScmObj* %env$ae41678,%struct.ScmObj* %args4015040437) {
%stackaddr$env-ref48042 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48042
%stackaddr$env-ref48043 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48043
%stackaddr$env-ref48044 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41678, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48044
%stackaddr$prim48045 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040437)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim48045, align 8
%stackaddr$prim48046 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040437)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48046, align 8
%stackaddr$prim48047 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48047, align 8
%stackaddr$prim48048 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48048, align 8
%stackaddr$makeclosure48049 = alloca %struct.ScmObj*, align 8
%fptrToInt48050 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41683 to i64
%ae41683 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48050)
store volatile %struct.ScmObj* %ae41683, %struct.ScmObj** %stackaddr$makeclosure48049, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41683, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41683, %struct.ScmObj* %k40438, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41683, %struct.ScmObj* %_37foldr40128, i64 2)
%ae41684 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48051 = alloca %struct.ScmObj*, align 8
%fptrToInt48052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41685 to i64
%ae41685 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48052)
store volatile %struct.ScmObj* %ae41685, %struct.ScmObj** %stackaddr$makeclosure48051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41685, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41685, %struct.ScmObj* %_37drop_45right40142, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41685, %struct.ScmObj* %f40152, i64 2)
%args46753$ae41683$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48053 = alloca %struct.ScmObj*, align 8
%args46753$ae41683$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41685, %struct.ScmObj* %args46753$ae41683$0)
store volatile %struct.ScmObj* %args46753$ae41683$1, %struct.ScmObj** %stackaddr$prim48053, align 8
%stackaddr$prim48054 = alloca %struct.ScmObj*, align 8
%args46753$ae41683$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41684, %struct.ScmObj* %args46753$ae41683$1)
store volatile %struct.ScmObj* %args46753$ae41683$2, %struct.ScmObj** %stackaddr$prim48054, align 8
%clofunc48055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41683)
musttail call tailcc void %clofunc48055(%struct.ScmObj* %ae41683, %struct.ScmObj* %args46753$ae41683$2)
ret void
}

define tailcc void @proc_clo$ae41683(%struct.ScmObj* %env$ae41683,%struct.ScmObj* %current_45args46738) {
%stackaddr$env-ref48056 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41683, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref48056
%stackaddr$env-ref48057 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41683, i64 1)
store %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$env-ref48057
%stackaddr$env-ref48058 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41683, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48058
%stackaddr$prim48059 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46738)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim48059, align 8
%stackaddr$prim48060 = alloca %struct.ScmObj*, align 8
%current_45args46739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46738)
store volatile %struct.ScmObj* %current_45args46739, %struct.ScmObj** %stackaddr$prim48060, align 8
%stackaddr$prim48061 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46739)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim48061, align 8
%ae41746 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41746, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim48062, align 8
%stackaddr$prim48063 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %anf_45bind40277)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48063, align 8
%stackaddr$prim48064 = alloca %struct.ScmObj*, align 8
%cpsargs40440 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40438, %struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %cpsargs40440, %struct.ScmObj** %stackaddr$prim48064, align 8
%clofunc48065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48065(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40440)
ret void
}

define tailcc void @proc_clo$ae41685(%struct.ScmObj* %env$ae41685,%struct.ScmObj* %fargs4015340441) {
%stackaddr$env-ref48066 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41685, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48066
%stackaddr$env-ref48067 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41685, i64 1)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48067
%stackaddr$env-ref48068 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41685, i64 2)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48068
%stackaddr$prim48069 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340441)
store volatile %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$prim48069, align 8
%stackaddr$prim48070 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340441)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim48070, align 8
%stackaddr$makeclosure48071 = alloca %struct.ScmObj*, align 8
%fptrToInt48072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41689 to i64
%ae41689 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48072)
store volatile %struct.ScmObj* %ae41689, %struct.ScmObj** %stackaddr$makeclosure48071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41689, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41689, %struct.ScmObj* %k40442, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41689, %struct.ScmObj* %fargs40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41689, %struct.ScmObj* %f40152, i64 3)
%ae41691 = call %struct.ScmObj* @const_init_int(i64 1)
%args46752$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48073 = alloca %struct.ScmObj*, align 8
%args46752$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41691, %struct.ScmObj* %args46752$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args46752$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim48073, align 8
%stackaddr$prim48074 = alloca %struct.ScmObj*, align 8
%args46752$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46752$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args46752$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim48074, align 8
%stackaddr$prim48075 = alloca %struct.ScmObj*, align 8
%args46752$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41689, %struct.ScmObj* %args46752$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args46752$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim48075, align 8
%clofunc48076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc48076(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args46752$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41689(%struct.ScmObj* %env$ae41689,%struct.ScmObj* %current_45args46741) {
%stackaddr$env-ref48077 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41689, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48077
%stackaddr$env-ref48078 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41689, i64 1)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref48078
%stackaddr$env-ref48079 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41689, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48079
%stackaddr$env-ref48080 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41689, i64 3)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48080
%stackaddr$prim48081 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46741)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim48081, align 8
%stackaddr$prim48082 = alloca %struct.ScmObj*, align 8
%current_45args46742 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46741)
store volatile %struct.ScmObj* %current_45args46742, %struct.ScmObj** %stackaddr$prim48082, align 8
%stackaddr$prim48083 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46742)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48083, align 8
%stackaddr$makeclosure48084 = alloca %struct.ScmObj*, align 8
%fptrToInt48085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41696 to i64
%ae41696 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48085)
store volatile %struct.ScmObj* %ae41696, %struct.ScmObj** %stackaddr$makeclosure48084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41696, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41696, %struct.ScmObj* %k40442, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41696, %struct.ScmObj* %fargs40153, i64 2)
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%cpsargs40447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41696, %struct.ScmObj* %anf_45bind40273)
store volatile %struct.ScmObj* %cpsargs40447, %struct.ScmObj** %stackaddr$prim48086, align 8
%clofunc48087 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc48087(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40447)
ret void
}

define tailcc void @proc_clo$ae41696(%struct.ScmObj* %env$ae41696,%struct.ScmObj* %current_45args46744) {
%stackaddr$env-ref48088 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41696, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48088
%stackaddr$env-ref48089 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41696, i64 1)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref48089
%stackaddr$env-ref48090 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41696, i64 2)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48090
%stackaddr$prim48091 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46744)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim48091, align 8
%stackaddr$prim48092 = alloca %struct.ScmObj*, align 8
%current_45args46745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46744)
store volatile %struct.ScmObj* %current_45args46745, %struct.ScmObj** %stackaddr$prim48092, align 8
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46745)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim48093, align 8
%stackaddr$makeclosure48094 = alloca %struct.ScmObj*, align 8
%fptrToInt48095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41701 to i64
%ae41701 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48095)
store volatile %struct.ScmObj* %ae41701, %struct.ScmObj** %stackaddr$makeclosure48094, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41701, %struct.ScmObj* %anf_45bind40274, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41701, %struct.ScmObj* %k40442, i64 1)
%args46751$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48096 = alloca %struct.ScmObj*, align 8
%args46751$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args46751$_37last40145$0)
store volatile %struct.ScmObj* %args46751$_37last40145$1, %struct.ScmObj** %stackaddr$prim48096, align 8
%stackaddr$prim48097 = alloca %struct.ScmObj*, align 8
%args46751$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41701, %struct.ScmObj* %args46751$_37last40145$1)
store volatile %struct.ScmObj* %args46751$_37last40145$2, %struct.ScmObj** %stackaddr$prim48097, align 8
%clofunc48098 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc48098(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args46751$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41701(%struct.ScmObj* %env$ae41701,%struct.ScmObj* %current_45args46747) {
%stackaddr$env-ref48099 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41701, i64 0)
store %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$env-ref48099
%stackaddr$env-ref48100 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41701, i64 1)
store %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$env-ref48100
%stackaddr$prim48101 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46747)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim48101, align 8
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%current_45args46748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46747)
store volatile %struct.ScmObj* %current_45args46748, %struct.ScmObj** %stackaddr$prim48102, align 8
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46748)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim48103, align 8
%stackaddr$prim48104 = alloca %struct.ScmObj*, align 8
%cpsprim40446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %anf_45bind40275)
store volatile %struct.ScmObj* %cpsprim40446, %struct.ScmObj** %stackaddr$prim48104, align 8
%ae41706 = call %struct.ScmObj* @const_init_int(i64 0)
%args46750$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48105 = alloca %struct.ScmObj*, align 8
%args46750$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40446, %struct.ScmObj* %args46750$k40442$0)
store volatile %struct.ScmObj* %args46750$k40442$1, %struct.ScmObj** %stackaddr$prim48105, align 8
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%args46750$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41706, %struct.ScmObj* %args46750$k40442$1)
store volatile %struct.ScmObj* %args46750$k40442$2, %struct.ScmObj** %stackaddr$prim48106, align 8
%clofunc48107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc48107(%struct.ScmObj* %k40442, %struct.ScmObj* %args46750$k40442$2)
ret void
}

define tailcc void @proc_clo$ae41601(%struct.ScmObj* %env$ae41601,%struct.ScmObj* %current_45args46755) {
%stackaddr$env-ref48108 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41601, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48108
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %k40448, %struct.ScmObj** %stackaddr$prim48109, align 8
%stackaddr$prim48110 = alloca %struct.ScmObj*, align 8
%current_45args46756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46755)
store volatile %struct.ScmObj* %current_45args46756, %struct.ScmObj** %stackaddr$prim48110, align 8
%stackaddr$prim48111 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46756)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim48111, align 8
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%current_45args46757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46756)
store volatile %struct.ScmObj* %current_45args46757, %struct.ScmObj** %stackaddr$prim48112, align 8
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46757)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim48113, align 8
%stackaddr$makeclosure48114 = alloca %struct.ScmObj*, align 8
%fptrToInt48115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41602 to i64
%ae41602 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48115)
store volatile %struct.ScmObj* %ae41602, %struct.ScmObj** %stackaddr$makeclosure48114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41602, %struct.ScmObj* %k40448, i64 2)
%ae41603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48116 = alloca %struct.ScmObj*, align 8
%fptrToInt48117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41604 to i64
%ae41604 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48117)
store volatile %struct.ScmObj* %ae41604, %struct.ScmObj** %stackaddr$makeclosure48116, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41604, %struct.ScmObj* %f40156, i64 0)
%args46772$ae41602$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48118 = alloca %struct.ScmObj*, align 8
%args46772$ae41602$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41604, %struct.ScmObj* %args46772$ae41602$0)
store volatile %struct.ScmObj* %args46772$ae41602$1, %struct.ScmObj** %stackaddr$prim48118, align 8
%stackaddr$prim48119 = alloca %struct.ScmObj*, align 8
%args46772$ae41602$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41603, %struct.ScmObj* %args46772$ae41602$1)
store volatile %struct.ScmObj* %args46772$ae41602$2, %struct.ScmObj** %stackaddr$prim48119, align 8
%clofunc48120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41602)
musttail call tailcc void %clofunc48120(%struct.ScmObj* %ae41602, %struct.ScmObj* %args46772$ae41602$2)
ret void
}

define tailcc void @proc_clo$ae41602(%struct.ScmObj* %env$ae41602,%struct.ScmObj* %current_45args46759) {
%stackaddr$env-ref48121 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref48121
%stackaddr$env-ref48122 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48122
%stackaddr$env-ref48123 = alloca %struct.ScmObj*, align 8
%k40448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41602, i64 2)
store %struct.ScmObj* %k40448, %struct.ScmObj** %stackaddr$env-ref48123
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46759)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim48124, align 8
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%current_45args46760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46759)
store volatile %struct.ScmObj* %current_45args46760, %struct.ScmObj** %stackaddr$prim48125, align 8
%stackaddr$prim48126 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46760)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48126, align 8
%ae41636 = call %struct.ScmObj* @const_init_null()
%args46762$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48127 = alloca %struct.ScmObj*, align 8
%args46762$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args46762$_37foldr140123$0)
store volatile %struct.ScmObj* %args46762$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48127, align 8
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%args46762$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41636, %struct.ScmObj* %args46762$_37foldr140123$1)
store volatile %struct.ScmObj* %args46762$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48128, align 8
%stackaddr$prim48129 = alloca %struct.ScmObj*, align 8
%args46762$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args46762$_37foldr140123$2)
store volatile %struct.ScmObj* %args46762$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48129, align 8
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%args46762$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40448, %struct.ScmObj* %args46762$_37foldr140123$3)
store volatile %struct.ScmObj* %args46762$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48130, align 8
%clofunc48131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48131(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46762$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41604(%struct.ScmObj* %env$ae41604,%struct.ScmObj* %current_45args46763) {
%stackaddr$env-ref48132 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41604, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref48132
%stackaddr$prim48133 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46763)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim48133, align 8
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%current_45args46764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46763)
store volatile %struct.ScmObj* %current_45args46764, %struct.ScmObj** %stackaddr$prim48134, align 8
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$prim48136 = alloca %struct.ScmObj*, align 8
%current_45args46765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46764)
store volatile %struct.ScmObj* %current_45args46765, %struct.ScmObj** %stackaddr$prim48136, align 8
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46765)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim48137, align 8
%stackaddr$makeclosure48138 = alloca %struct.ScmObj*, align 8
%fptrToInt48139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41606 to i64
%ae41606 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48139)
store volatile %struct.ScmObj* %ae41606, %struct.ScmObj** %stackaddr$makeclosure48138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41606, %struct.ScmObj* %k40450, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41606, %struct.ScmObj* %r40157, i64 1)
%args46771$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48140 = alloca %struct.ScmObj*, align 8
%args46771$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args46771$f40156$0)
store volatile %struct.ScmObj* %args46771$f40156$1, %struct.ScmObj** %stackaddr$prim48140, align 8
%stackaddr$prim48141 = alloca %struct.ScmObj*, align 8
%args46771$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41606, %struct.ScmObj* %args46771$f40156$1)
store volatile %struct.ScmObj* %args46771$f40156$2, %struct.ScmObj** %stackaddr$prim48141, align 8
%clofunc48142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc48142(%struct.ScmObj* %f40156, %struct.ScmObj* %args46771$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41606(%struct.ScmObj* %env$ae41606,%struct.ScmObj* %current_45args46767) {
%stackaddr$env-ref48143 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41606, i64 0)
store %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$env-ref48143
%stackaddr$env-ref48144 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41606, i64 1)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref48144
%stackaddr$prim48145 = alloca %struct.ScmObj*, align 8
%_95k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46767)
store volatile %struct.ScmObj* %_95k40451, %struct.ScmObj** %stackaddr$prim48145, align 8
%stackaddr$prim48146 = alloca %struct.ScmObj*, align 8
%current_45args46768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46767)
store volatile %struct.ScmObj* %current_45args46768, %struct.ScmObj** %stackaddr$prim48146, align 8
%stackaddr$prim48147 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46768)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48147, align 8
%stackaddr$prim48148 = alloca %struct.ScmObj*, align 8
%cpsprim40452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40452, %struct.ScmObj** %stackaddr$prim48148, align 8
%ae41611 = call %struct.ScmObj* @const_init_int(i64 0)
%args46770$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%args46770$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40452, %struct.ScmObj* %args46770$k40450$0)
store volatile %struct.ScmObj* %args46770$k40450$1, %struct.ScmObj** %stackaddr$prim48149, align 8
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%args46770$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41611, %struct.ScmObj* %args46770$k40450$1)
store volatile %struct.ScmObj* %args46770$k40450$2, %struct.ScmObj** %stackaddr$prim48150, align 8
%clofunc48151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc48151(%struct.ScmObj* %k40450, %struct.ScmObj* %args46770$k40450$2)
ret void
}

define tailcc void @proc_clo$ae41215(%struct.ScmObj* %env$ae41215,%struct.ScmObj* %current_45args46775) {
%stackaddr$env-ref48152 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41215, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48152
%stackaddr$env-ref48153 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41215, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48153
%stackaddr$prim48154 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$prim48154, align 8
%stackaddr$prim48155 = alloca %struct.ScmObj*, align 8
%current_45args46776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46775)
store volatile %struct.ScmObj* %current_45args46776, %struct.ScmObj** %stackaddr$prim48155, align 8
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46776)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48156, align 8
%ae41217 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48157 = alloca %struct.ScmObj*, align 8
%fptrToInt48158 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41218 to i64
%ae41218 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48158)
store volatile %struct.ScmObj* %ae41218, %struct.ScmObj** %stackaddr$makeclosure48157, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41218, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41218, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41218, %struct.ScmObj* %_37foldr140123, i64 2)
%args46833$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48159 = alloca %struct.ScmObj*, align 8
%args46833$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41218, %struct.ScmObj* %args46833$k40453$0)
store volatile %struct.ScmObj* %args46833$k40453$1, %struct.ScmObj** %stackaddr$prim48159, align 8
%stackaddr$prim48160 = alloca %struct.ScmObj*, align 8
%args46833$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41217, %struct.ScmObj* %args46833$k40453$1)
store volatile %struct.ScmObj* %args46833$k40453$2, %struct.ScmObj** %stackaddr$prim48160, align 8
%clofunc48161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc48161(%struct.ScmObj* %k40453, %struct.ScmObj* %args46833$k40453$2)
ret void
}

define tailcc void @proc_clo$ae41218(%struct.ScmObj* %env$ae41218,%struct.ScmObj* %args4013040454) {
%stackaddr$env-ref48162 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41218, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48162
%stackaddr$env-ref48163 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41218, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48163
%stackaddr$env-ref48164 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41218, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48164
%stackaddr$prim48165 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040454)
store volatile %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$prim48165, align 8
%stackaddr$prim48166 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040454)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim48166, align 8
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40258)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$prim48170 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim48170, align 8
%stackaddr$prim48171 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40259)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim48171, align 8
%stackaddr$makeclosure48172 = alloca %struct.ScmObj*, align 8
%fptrToInt48173 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41226 to i64
%ae41226 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48173)
store volatile %struct.ScmObj* %ae41226, %struct.ScmObj** %stackaddr$makeclosure48172, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37map140119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41226, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48174 = alloca %struct.ScmObj*, align 8
%fptrToInt48175 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41228 to i64
%ae41228 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48175)
store volatile %struct.ScmObj* %ae41228, %struct.ScmObj** %stackaddr$makeclosure48174, align 8
%args46832$ae41226$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48176 = alloca %struct.ScmObj*, align 8
%args46832$ae41226$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41228, %struct.ScmObj* %args46832$ae41226$0)
store volatile %struct.ScmObj* %args46832$ae41226$1, %struct.ScmObj** %stackaddr$prim48176, align 8
%stackaddr$prim48177 = alloca %struct.ScmObj*, align 8
%args46832$ae41226$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41227, %struct.ScmObj* %args46832$ae41226$1)
store volatile %struct.ScmObj* %args46832$ae41226$2, %struct.ScmObj** %stackaddr$prim48177, align 8
%clofunc48178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41226)
musttail call tailcc void %clofunc48178(%struct.ScmObj* %ae41226, %struct.ScmObj* %args46832$ae41226$2)
ret void
}

define tailcc void @proc_clo$ae41226(%struct.ScmObj* %env$ae41226,%struct.ScmObj* %current_45args46778) {
%stackaddr$env-ref48179 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48179
%stackaddr$env-ref48180 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 1)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48180
%stackaddr$env-ref48181 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48181
%stackaddr$env-ref48182 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48182
%stackaddr$env-ref48183 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48183
%stackaddr$env-ref48184 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48184
%stackaddr$env-ref48185 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41226, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48185
%stackaddr$prim48186 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46778)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim48186, align 8
%stackaddr$prim48187 = alloca %struct.ScmObj*, align 8
%current_45args46779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46778)
store volatile %struct.ScmObj* %current_45args46779, %struct.ScmObj** %stackaddr$prim48187, align 8
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46779)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$makeclosure48189 = alloca %struct.ScmObj*, align 8
%fptrToInt48190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41258 to i64
%ae41258 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48190)
store volatile %struct.ScmObj* %ae41258, %struct.ScmObj** %stackaddr$makeclosure48189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %_37map140119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41258, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41260 = call %struct.ScmObj* @const_init_false()
%args46825$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48191 = alloca %struct.ScmObj*, align 8
%args46825$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46825$_37foldr140123$0)
store volatile %struct.ScmObj* %args46825$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48191, align 8
%stackaddr$prim48192 = alloca %struct.ScmObj*, align 8
%args46825$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41260, %struct.ScmObj* %args46825$_37foldr140123$1)
store volatile %struct.ScmObj* %args46825$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48192, align 8
%stackaddr$prim48193 = alloca %struct.ScmObj*, align 8
%args46825$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args46825$_37foldr140123$2)
store volatile %struct.ScmObj* %args46825$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48193, align 8
%stackaddr$prim48194 = alloca %struct.ScmObj*, align 8
%args46825$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41258, %struct.ScmObj* %args46825$_37foldr140123$3)
store volatile %struct.ScmObj* %args46825$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48194, align 8
%clofunc48195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48195(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46825$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41258(%struct.ScmObj* %env$ae41258,%struct.ScmObj* %current_45args46781) {
%stackaddr$env-ref48196 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48196
%stackaddr$env-ref48197 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 1)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48197
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$env-ref48199 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48199
%stackaddr$env-ref48200 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48200
%stackaddr$env-ref48201 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48201
%stackaddr$env-ref48202 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41258, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48202
%stackaddr$prim48203 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim48203, align 8
%stackaddr$prim48204 = alloca %struct.ScmObj*, align 8
%current_45args46782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46781)
store volatile %struct.ScmObj* %current_45args46782, %struct.ScmObj** %stackaddr$prim48204, align 8
%stackaddr$prim48205 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46782)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim48205, align 8
%truthy$cmp48206 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40261)
%cmp$cmp48206 = icmp eq i64 %truthy$cmp48206, 1
br i1 %cmp$cmp48206, label %truebranch$cmp48206, label %falsebranch$cmp48206
truebranch$cmp48206:
%ae41269 = call %struct.ScmObj* @const_init_int(i64 0)
%args46784$k40455$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args46784$k40455$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args46784$k40455$0)
store volatile %struct.ScmObj* %args46784$k40455$1, %struct.ScmObj** %stackaddr$prim48207, align 8
%stackaddr$prim48208 = alloca %struct.ScmObj*, align 8
%args46784$k40455$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41269, %struct.ScmObj* %args46784$k40455$1)
store volatile %struct.ScmObj* %args46784$k40455$2, %struct.ScmObj** %stackaddr$prim48208, align 8
%clofunc48209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40455)
musttail call tailcc void %clofunc48209(%struct.ScmObj* %k40455, %struct.ScmObj* %args46784$k40455$2)
ret void
falsebranch$cmp48206:
%stackaddr$makeclosure48210 = alloca %struct.ScmObj*, align 8
%fptrToInt48211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41274 to i64
%ae41274 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48211)
store volatile %struct.ScmObj* %ae41274, %struct.ScmObj** %stackaddr$makeclosure48210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %_37map140119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41274, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41275 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48212 = alloca %struct.ScmObj*, align 8
%fptrToInt48213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41276 to i64
%ae41276 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48213)
store volatile %struct.ScmObj* %ae41276, %struct.ScmObj** %stackaddr$makeclosure48212, align 8
%args46824$ae41274$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%args46824$ae41274$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41276, %struct.ScmObj* %args46824$ae41274$0)
store volatile %struct.ScmObj* %args46824$ae41274$1, %struct.ScmObj** %stackaddr$prim48214, align 8
%stackaddr$prim48215 = alloca %struct.ScmObj*, align 8
%args46824$ae41274$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41275, %struct.ScmObj* %args46824$ae41274$1)
store volatile %struct.ScmObj* %args46824$ae41274$2, %struct.ScmObj** %stackaddr$prim48215, align 8
%clofunc48216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41274)
musttail call tailcc void %clofunc48216(%struct.ScmObj* %ae41274, %struct.ScmObj* %args46824$ae41274$2)
ret void
}

define tailcc void @proc_clo$ae41274(%struct.ScmObj* %env$ae41274,%struct.ScmObj* %current_45args46785) {
%stackaddr$env-ref48217 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48217
%stackaddr$env-ref48218 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 1)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48218
%stackaddr$env-ref48219 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48219
%stackaddr$env-ref48220 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48220
%stackaddr$env-ref48221 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48221
%stackaddr$env-ref48222 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48222
%stackaddr$env-ref48223 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41274, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48223
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46785)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim48224, align 8
%stackaddr$prim48225 = alloca %struct.ScmObj*, align 8
%current_45args46786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46785)
store volatile %struct.ScmObj* %current_45args46786, %struct.ScmObj** %stackaddr$prim48225, align 8
%stackaddr$prim48226 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46786)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim48226, align 8
%stackaddr$makeclosure48227 = alloca %struct.ScmObj*, align 8
%fptrToInt48228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41295 to i64
%ae41295 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48228)
store volatile %struct.ScmObj* %ae41295, %struct.ScmObj** %stackaddr$makeclosure48227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37map140119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37foldr140123, i64 6)
%args46819$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48229 = alloca %struct.ScmObj*, align 8
%args46819$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46819$_37map140119$0)
store volatile %struct.ScmObj* %args46819$_37map140119$1, %struct.ScmObj** %stackaddr$prim48229, align 8
%stackaddr$prim48230 = alloca %struct.ScmObj*, align 8
%args46819$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args46819$_37map140119$1)
store volatile %struct.ScmObj* %args46819$_37map140119$2, %struct.ScmObj** %stackaddr$prim48230, align 8
%stackaddr$prim48231 = alloca %struct.ScmObj*, align 8
%args46819$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41295, %struct.ScmObj* %args46819$_37map140119$2)
store volatile %struct.ScmObj* %args46819$_37map140119$3, %struct.ScmObj** %stackaddr$prim48231, align 8
%clofunc48232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48232(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46819$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41295(%struct.ScmObj* %env$ae41295,%struct.ScmObj* %current_45args46788) {
%stackaddr$env-ref48233 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48233
%stackaddr$env-ref48234 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 1)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48234
%stackaddr$env-ref48235 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48235
%stackaddr$env-ref48236 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48236
%stackaddr$env-ref48237 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48237
%stackaddr$env-ref48238 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48238
%stackaddr$env-ref48239 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48239
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46788)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim48240, align 8
%stackaddr$prim48241 = alloca %struct.ScmObj*, align 8
%current_45args46789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46788)
store volatile %struct.ScmObj* %current_45args46789, %struct.ScmObj** %stackaddr$prim48241, align 8
%stackaddr$prim48242 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46789)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim48242, align 8
%stackaddr$makeclosure48243 = alloca %struct.ScmObj*, align 8
%fptrToInt48244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41298 to i64
%ae41298 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48244)
store volatile %struct.ScmObj* %ae41298, %struct.ScmObj** %stackaddr$makeclosure48243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37map140119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41298, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48245 = alloca %struct.ScmObj*, align 8
%fptrToInt48246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41300 to i64
%ae41300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48246)
store volatile %struct.ScmObj* %ae41300, %struct.ScmObj** %stackaddr$makeclosure48245, align 8
%args46818$ae41298$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48247 = alloca %struct.ScmObj*, align 8
%args46818$ae41298$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41300, %struct.ScmObj* %args46818$ae41298$0)
store volatile %struct.ScmObj* %args46818$ae41298$1, %struct.ScmObj** %stackaddr$prim48247, align 8
%stackaddr$prim48248 = alloca %struct.ScmObj*, align 8
%args46818$ae41298$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41299, %struct.ScmObj* %args46818$ae41298$1)
store volatile %struct.ScmObj* %args46818$ae41298$2, %struct.ScmObj** %stackaddr$prim48248, align 8
%clofunc48249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41298)
musttail call tailcc void %clofunc48249(%struct.ScmObj* %ae41298, %struct.ScmObj* %args46818$ae41298$2)
ret void
}

define tailcc void @proc_clo$ae41298(%struct.ScmObj* %env$ae41298,%struct.ScmObj* %current_45args46791) {
%stackaddr$env-ref48250 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48250
%stackaddr$env-ref48251 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 1)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48251
%stackaddr$env-ref48252 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48252
%stackaddr$env-ref48253 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48253
%stackaddr$env-ref48254 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref48254
%stackaddr$env-ref48255 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48255
%stackaddr$env-ref48256 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48256
%stackaddr$env-ref48257 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41298, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48257
%stackaddr$prim48258 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46791)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim48258, align 8
%stackaddr$prim48259 = alloca %struct.ScmObj*, align 8
%current_45args46792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46791)
store volatile %struct.ScmObj* %current_45args46792, %struct.ScmObj** %stackaddr$prim48259, align 8
%stackaddr$prim48260 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46792)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim48260, align 8
%stackaddr$makeclosure48261 = alloca %struct.ScmObj*, align 8
%fptrToInt48262 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41319 to i64
%ae41319 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48262)
store volatile %struct.ScmObj* %ae41319, %struct.ScmObj** %stackaddr$makeclosure48261, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41319, %struct.ScmObj* %lsts_4340138, i64 5)
%args46813$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48263 = alloca %struct.ScmObj*, align 8
%args46813$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args46813$_37map140119$0)
store volatile %struct.ScmObj* %args46813$_37map140119$1, %struct.ScmObj** %stackaddr$prim48263, align 8
%stackaddr$prim48264 = alloca %struct.ScmObj*, align 8
%args46813$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args46813$_37map140119$1)
store volatile %struct.ScmObj* %args46813$_37map140119$2, %struct.ScmObj** %stackaddr$prim48264, align 8
%stackaddr$prim48265 = alloca %struct.ScmObj*, align 8
%args46813$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41319, %struct.ScmObj* %args46813$_37map140119$2)
store volatile %struct.ScmObj* %args46813$_37map140119$3, %struct.ScmObj** %stackaddr$prim48265, align 8
%clofunc48266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc48266(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args46813$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41319(%struct.ScmObj* %env$ae41319,%struct.ScmObj* %current_45args46794) {
%stackaddr$env-ref48267 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48267
%stackaddr$env-ref48268 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48268
%stackaddr$env-ref48269 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48269
%stackaddr$env-ref48270 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48270
%stackaddr$env-ref48271 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48271
%stackaddr$env-ref48272 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41319, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48272
%stackaddr$prim48273 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46794)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim48273, align 8
%stackaddr$prim48274 = alloca %struct.ScmObj*, align 8
%current_45args46795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46794)
store volatile %struct.ScmObj* %current_45args46795, %struct.ScmObj** %stackaddr$prim48274, align 8
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46795)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$makeclosure48276 = alloca %struct.ScmObj*, align 8
%fptrToInt48277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41322 to i64
%ae41322 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48277)
store volatile %struct.ScmObj* %ae41322, %struct.ScmObj** %stackaddr$makeclosure48276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %lsts_4340138, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41322, %struct.ScmObj* %vs40136, i64 6)
%ae41323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48278 = alloca %struct.ScmObj*, align 8
%fptrToInt48279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41324 to i64
%ae41324 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48279)
store volatile %struct.ScmObj* %ae41324, %struct.ScmObj** %stackaddr$makeclosure48278, align 8
%args46812$ae41322$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%args46812$ae41322$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41324, %struct.ScmObj* %args46812$ae41322$0)
store volatile %struct.ScmObj* %args46812$ae41322$1, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%args46812$ae41322$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41323, %struct.ScmObj* %args46812$ae41322$1)
store volatile %struct.ScmObj* %args46812$ae41322$2, %struct.ScmObj** %stackaddr$prim48281, align 8
%clofunc48282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41322)
musttail call tailcc void %clofunc48282(%struct.ScmObj* %ae41322, %struct.ScmObj* %args46812$ae41322$2)
ret void
}

define tailcc void @proc_clo$ae41322(%struct.ScmObj* %env$ae41322,%struct.ScmObj* %current_45args46797) {
%stackaddr$env-ref48283 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48283
%stackaddr$env-ref48284 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48284
%stackaddr$env-ref48285 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref48285
%stackaddr$env-ref48286 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48286
%stackaddr$env-ref48287 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48287
%stackaddr$env-ref48288 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref48288
%stackaddr$env-ref48289 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41322, i64 6)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48289
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%current_45args46798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46797)
store volatile %struct.ScmObj* %current_45args46798, %struct.ScmObj** %stackaddr$prim48291, align 8
%stackaddr$prim48292 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46798)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48292, align 8
%stackaddr$prim48293 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim48293, align 8
%stackaddr$prim48294 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim48294, align 8
%stackaddr$makeclosure48295 = alloca %struct.ScmObj*, align 8
%fptrToInt48296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41348 to i64
%ae41348 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48296)
store volatile %struct.ScmObj* %ae41348, %struct.ScmObj** %stackaddr$makeclosure48295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41348, %struct.ScmObj* %anf_45bind40264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41348, %struct.ScmObj* %vs40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41348, %struct.ScmObj* %k40455, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41348, %struct.ScmObj* %f40133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41348, %struct.ScmObj* %_37foldr140123, i64 4)
%stackaddr$prim48297 = alloca %struct.ScmObj*, align 8
%cpsargs40466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41348, %struct.ScmObj* %anf_45bind40266)
store volatile %struct.ScmObj* %cpsargs40466, %struct.ScmObj** %stackaddr$prim48297, align 8
%clofunc48298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc48298(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40466)
ret void
}

define tailcc void @proc_clo$ae41348(%struct.ScmObj* %env$ae41348,%struct.ScmObj* %current_45args46800) {
%stackaddr$env-ref48299 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41348, i64 0)
store %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$env-ref48299
%stackaddr$env-ref48300 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41348, i64 1)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref48300
%stackaddr$env-ref48301 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41348, i64 2)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48301
%stackaddr$env-ref48302 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41348, i64 3)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48302
%stackaddr$env-ref48303 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41348, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48303
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%current_45args46801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46800)
store volatile %struct.ScmObj* %current_45args46801, %struct.ScmObj** %stackaddr$prim48305, align 8
%stackaddr$prim48306 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46801)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim48306, align 8
%ae41353 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %ae41353)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48307, align 8
%stackaddr$makeclosure48308 = alloca %struct.ScmObj*, align 8
%fptrToInt48309 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41355 to i64
%ae41355 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48309)
store volatile %struct.ScmObj* %ae41355, %struct.ScmObj** %stackaddr$makeclosure48308, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %f40133, i64 1)
%args46806$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48310 = alloca %struct.ScmObj*, align 8
%args46806$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args46806$_37foldr140123$0)
store volatile %struct.ScmObj* %args46806$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48310, align 8
%stackaddr$prim48311 = alloca %struct.ScmObj*, align 8
%args46806$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args46806$_37foldr140123$1)
store volatile %struct.ScmObj* %args46806$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48311, align 8
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%args46806$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args46806$_37foldr140123$2)
store volatile %struct.ScmObj* %args46806$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48312, align 8
%stackaddr$prim48313 = alloca %struct.ScmObj*, align 8
%args46806$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41355, %struct.ScmObj* %args46806$_37foldr140123$3)
store volatile %struct.ScmObj* %args46806$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48313, align 8
%clofunc48314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48314(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args46806$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41355(%struct.ScmObj* %env$ae41355,%struct.ScmObj* %current_45args46803) {
%stackaddr$env-ref48315 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref48315
%stackaddr$env-ref48316 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref48316
%stackaddr$prim48317 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim48317, align 8
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%current_45args46804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46803)
store volatile %struct.ScmObj* %current_45args46804, %struct.ScmObj** %stackaddr$prim48318, align 8
%stackaddr$prim48319 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46804)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim48319, align 8
%stackaddr$prim48320 = alloca %struct.ScmObj*, align 8
%cpsargs40465 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40455, %struct.ScmObj* %anf_45bind40269)
store volatile %struct.ScmObj* %cpsargs40465, %struct.ScmObj** %stackaddr$prim48320, align 8
%clofunc48321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc48321(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40465)
ret void
}

define tailcc void @proc_clo$ae41324(%struct.ScmObj* %env$ae41324,%struct.ScmObj* %current_45args46807) {
%stackaddr$prim48322 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46807)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim48322, align 8
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%current_45args46808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46807)
store volatile %struct.ScmObj* %current_45args46808, %struct.ScmObj** %stackaddr$prim48323, align 8
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46808)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%current_45args46809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46808)
store volatile %struct.ScmObj* %current_45args46809, %struct.ScmObj** %stackaddr$prim48325, align 8
%stackaddr$prim48326 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46809)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim48326, align 8
%stackaddr$prim48327 = alloca %struct.ScmObj*, align 8
%cpsprim40468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40468, %struct.ScmObj** %stackaddr$prim48327, align 8
%ae41328 = call %struct.ScmObj* @const_init_int(i64 0)
%args46811$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%args46811$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40468, %struct.ScmObj* %args46811$k40467$0)
store volatile %struct.ScmObj* %args46811$k40467$1, %struct.ScmObj** %stackaddr$prim48328, align 8
%stackaddr$prim48329 = alloca %struct.ScmObj*, align 8
%args46811$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41328, %struct.ScmObj* %args46811$k40467$1)
store volatile %struct.ScmObj* %args46811$k40467$2, %struct.ScmObj** %stackaddr$prim48329, align 8
%clofunc48330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48330(%struct.ScmObj* %k40467, %struct.ScmObj* %args46811$k40467$2)
ret void
}

define tailcc void @proc_clo$ae41300(%struct.ScmObj* %env$ae41300,%struct.ScmObj* %current_45args46814) {
%stackaddr$prim48331 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46814)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim48331, align 8
%stackaddr$prim48332 = alloca %struct.ScmObj*, align 8
%current_45args46815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46814)
store volatile %struct.ScmObj* %current_45args46815, %struct.ScmObj** %stackaddr$prim48332, align 8
%stackaddr$prim48333 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46815)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim48333, align 8
%stackaddr$prim48334 = alloca %struct.ScmObj*, align 8
%cpsprim40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40470, %struct.ScmObj** %stackaddr$prim48334, align 8
%ae41303 = call %struct.ScmObj* @const_init_int(i64 0)
%args46817$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48335 = alloca %struct.ScmObj*, align 8
%args46817$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40470, %struct.ScmObj* %args46817$k40469$0)
store volatile %struct.ScmObj* %args46817$k40469$1, %struct.ScmObj** %stackaddr$prim48335, align 8
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%args46817$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41303, %struct.ScmObj* %args46817$k40469$1)
store volatile %struct.ScmObj* %args46817$k40469$2, %struct.ScmObj** %stackaddr$prim48336, align 8
%clofunc48337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc48337(%struct.ScmObj* %k40469, %struct.ScmObj* %args46817$k40469$2)
ret void
}

define tailcc void @proc_clo$ae41276(%struct.ScmObj* %env$ae41276,%struct.ScmObj* %current_45args46820) {
%stackaddr$prim48338 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46820)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim48338, align 8
%stackaddr$prim48339 = alloca %struct.ScmObj*, align 8
%current_45args46821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46820)
store volatile %struct.ScmObj* %current_45args46821, %struct.ScmObj** %stackaddr$prim48339, align 8
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46821)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim48340, align 8
%stackaddr$prim48341 = alloca %struct.ScmObj*, align 8
%cpsprim40472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40472, %struct.ScmObj** %stackaddr$prim48341, align 8
%ae41279 = call %struct.ScmObj* @const_init_int(i64 0)
%args46823$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48342 = alloca %struct.ScmObj*, align 8
%args46823$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40472, %struct.ScmObj* %args46823$k40471$0)
store volatile %struct.ScmObj* %args46823$k40471$1, %struct.ScmObj** %stackaddr$prim48342, align 8
%stackaddr$prim48343 = alloca %struct.ScmObj*, align 8
%args46823$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41279, %struct.ScmObj* %args46823$k40471$1)
store volatile %struct.ScmObj* %args46823$k40471$2, %struct.ScmObj** %stackaddr$prim48343, align 8
%clofunc48344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc48344(%struct.ScmObj* %k40471, %struct.ScmObj* %args46823$k40471$2)
ret void
}

define tailcc void @proc_clo$ae41228(%struct.ScmObj* %env$ae41228,%struct.ScmObj* %current_45args46826) {
%stackaddr$prim48345 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46826)
store volatile %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$prim48345, align 8
%stackaddr$prim48346 = alloca %struct.ScmObj*, align 8
%current_45args46827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46826)
store volatile %struct.ScmObj* %current_45args46827, %struct.ScmObj** %stackaddr$prim48346, align 8
%stackaddr$prim48347 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46827)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim48347, align 8
%stackaddr$prim48348 = alloca %struct.ScmObj*, align 8
%current_45args46828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46827)
store volatile %struct.ScmObj* %current_45args46828, %struct.ScmObj** %stackaddr$prim48348, align 8
%stackaddr$prim48349 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46828)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim48349, align 8
%truthy$cmp48350 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp48350 = icmp eq i64 %truthy$cmp48350, 1
br i1 %cmp$cmp48350, label %truebranch$cmp48350, label %falsebranch$cmp48350
truebranch$cmp48350:
%ae41231 = call %struct.ScmObj* @const_init_int(i64 0)
%args46830$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48351 = alloca %struct.ScmObj*, align 8
%args46830$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args46830$k40473$0)
store volatile %struct.ScmObj* %args46830$k40473$1, %struct.ScmObj** %stackaddr$prim48351, align 8
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%args46830$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41231, %struct.ScmObj* %args46830$k40473$1)
store volatile %struct.ScmObj* %args46830$k40473$2, %struct.ScmObj** %stackaddr$prim48352, align 8
%clofunc48353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48353(%struct.ScmObj* %k40473, %struct.ScmObj* %args46830$k40473$2)
ret void
falsebranch$cmp48350:
%stackaddr$prim48354 = alloca %struct.ScmObj*, align 8
%cpsprim40474 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40474, %struct.ScmObj** %stackaddr$prim48354, align 8
%ae41238 = call %struct.ScmObj* @const_init_int(i64 0)
%args46831$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48355 = alloca %struct.ScmObj*, align 8
%args46831$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40474, %struct.ScmObj* %args46831$k40473$0)
store volatile %struct.ScmObj* %args46831$k40473$1, %struct.ScmObj** %stackaddr$prim48355, align 8
%stackaddr$prim48356 = alloca %struct.ScmObj*, align 8
%args46831$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41238, %struct.ScmObj* %args46831$k40473$1)
store volatile %struct.ScmObj* %args46831$k40473$2, %struct.ScmObj** %stackaddr$prim48356, align 8
%clofunc48357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc48357(%struct.ScmObj* %k40473, %struct.ScmObj* %args46831$k40473$2)
ret void
}

define tailcc void @proc_clo$ae41185(%struct.ScmObj* %env$ae41185,%struct.ScmObj* %current_45args46835) {
%stackaddr$env-ref48358 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48358
%stackaddr$env-ref48359 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref48359
%stackaddr$prim48360 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46835)
store volatile %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$prim48360, align 8
%stackaddr$prim48361 = alloca %struct.ScmObj*, align 8
%current_45args46836 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46835)
store volatile %struct.ScmObj* %current_45args46836, %struct.ScmObj** %stackaddr$prim48361, align 8
%stackaddr$prim48362 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim48362, align 8
%stackaddr$prim48363 = alloca %struct.ScmObj*, align 8
%current_45args46837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46836)
store volatile %struct.ScmObj* %current_45args46837, %struct.ScmObj** %stackaddr$prim48363, align 8
%stackaddr$prim48364 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46837)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim48364, align 8
%stackaddr$makeclosure48365 = alloca %struct.ScmObj*, align 8
%fptrToInt48366 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41187 to i64
%ae41187 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48366)
store volatile %struct.ScmObj* %ae41187, %struct.ScmObj** %stackaddr$makeclosure48365, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %lst40144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %n40143, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %k40475, i64 3)
%args46843$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%args46843$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46843$_37length40112$0)
store volatile %struct.ScmObj* %args46843$_37length40112$1, %struct.ScmObj** %stackaddr$prim48367, align 8
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%args46843$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41187, %struct.ScmObj* %args46843$_37length40112$1)
store volatile %struct.ScmObj* %args46843$_37length40112$2, %struct.ScmObj** %stackaddr$prim48368, align 8
%clofunc48369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc48369(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args46843$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41187(%struct.ScmObj* %env$ae41187,%struct.ScmObj* %current_45args46839) {
%stackaddr$env-ref48370 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref48370
%stackaddr$env-ref48371 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 1)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref48371
%stackaddr$env-ref48372 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 2)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref48372
%stackaddr$env-ref48373 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 3)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref48373
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%_95k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %_95k40476, %struct.ScmObj** %stackaddr$prim48374, align 8
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%current_45args46840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46839)
store volatile %struct.ScmObj* %current_45args46840, %struct.ScmObj** %stackaddr$prim48375, align 8
%stackaddr$prim48376 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46840)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim48376, align 8
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48377, align 8
%args46842$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48378 = alloca %struct.ScmObj*, align 8
%args46842$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args46842$_37take40115$0)
store volatile %struct.ScmObj* %args46842$_37take40115$1, %struct.ScmObj** %stackaddr$prim48378, align 8
%stackaddr$prim48379 = alloca %struct.ScmObj*, align 8
%args46842$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args46842$_37take40115$1)
store volatile %struct.ScmObj* %args46842$_37take40115$2, %struct.ScmObj** %stackaddr$prim48379, align 8
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%args46842$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40475, %struct.ScmObj* %args46842$_37take40115$2)
store volatile %struct.ScmObj* %args46842$_37take40115$3, %struct.ScmObj** %stackaddr$prim48380, align 8
%clofunc48381 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc48381(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args46842$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41131(%struct.ScmObj* %env$ae41131,%struct.ScmObj* %current_45args46845) {
%stackaddr$env-ref48382 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41131, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48382
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46845)
store volatile %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$prim48383, align 8
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%current_45args46846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46845)
store volatile %struct.ScmObj* %current_45args46846, %struct.ScmObj** %stackaddr$prim48384, align 8
%stackaddr$prim48385 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46846)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim48385, align 8
%stackaddr$makeclosure48386 = alloca %struct.ScmObj*, align 8
%fptrToInt48387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41132 to i64
%ae41132 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48387)
store volatile %struct.ScmObj* %ae41132, %struct.ScmObj** %stackaddr$makeclosure48386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %lst40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %k40477, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41132, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41133 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48388 = alloca %struct.ScmObj*, align 8
%fptrToInt48389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41134 to i64
%ae41134 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48389)
store volatile %struct.ScmObj* %ae41134, %struct.ScmObj** %stackaddr$makeclosure48388, align 8
%args46857$ae41132$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%args46857$ae41132$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41134, %struct.ScmObj* %args46857$ae41132$0)
store volatile %struct.ScmObj* %args46857$ae41132$1, %struct.ScmObj** %stackaddr$prim48390, align 8
%stackaddr$prim48391 = alloca %struct.ScmObj*, align 8
%args46857$ae41132$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41133, %struct.ScmObj* %args46857$ae41132$1)
store volatile %struct.ScmObj* %args46857$ae41132$2, %struct.ScmObj** %stackaddr$prim48391, align 8
%clofunc48392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41132)
musttail call tailcc void %clofunc48392(%struct.ScmObj* %ae41132, %struct.ScmObj* %args46857$ae41132$2)
ret void
}

define tailcc void @proc_clo$ae41132(%struct.ScmObj* %env$ae41132,%struct.ScmObj* %current_45args46848) {
%stackaddr$env-ref48393 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 0)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref48393
%stackaddr$env-ref48394 = alloca %struct.ScmObj*, align 8
%k40477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 1)
store %struct.ScmObj* %k40477, %struct.ScmObj** %stackaddr$env-ref48394
%stackaddr$env-ref48395 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41132, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48395
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46848)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim48396, align 8
%stackaddr$prim48397 = alloca %struct.ScmObj*, align 8
%current_45args46849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46848)
store volatile %struct.ScmObj* %current_45args46849, %struct.ScmObj** %stackaddr$prim48397, align 8
%stackaddr$prim48398 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46849)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim48398, align 8
%ae41153 = call %struct.ScmObj* @const_init_null()
%args46851$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48399 = alloca %struct.ScmObj*, align 8
%args46851$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args46851$_37foldl140107$0)
store volatile %struct.ScmObj* %args46851$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48399, align 8
%stackaddr$prim48400 = alloca %struct.ScmObj*, align 8
%args46851$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41153, %struct.ScmObj* %args46851$_37foldl140107$1)
store volatile %struct.ScmObj* %args46851$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48400, align 8
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%args46851$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args46851$_37foldl140107$2)
store volatile %struct.ScmObj* %args46851$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48401, align 8
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%args46851$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40477, %struct.ScmObj* %args46851$_37foldl140107$3)
store volatile %struct.ScmObj* %args46851$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48402, align 8
%clofunc48403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48403(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args46851$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41134(%struct.ScmObj* %env$ae41134,%struct.ScmObj* %current_45args46852) {
%stackaddr$prim48404 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46852)
store volatile %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$prim48404, align 8
%stackaddr$prim48405 = alloca %struct.ScmObj*, align 8
%current_45args46853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46852)
store volatile %struct.ScmObj* %current_45args46853, %struct.ScmObj** %stackaddr$prim48405, align 8
%stackaddr$prim48406 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim48406, align 8
%stackaddr$prim48407 = alloca %struct.ScmObj*, align 8
%current_45args46854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46853)
store volatile %struct.ScmObj* %current_45args46854, %struct.ScmObj** %stackaddr$prim48407, align 8
%stackaddr$prim48408 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46854)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim48408, align 8
%ae41136 = call %struct.ScmObj* @const_init_int(i64 0)
%args46856$k40479$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%args46856$k40479$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args46856$k40479$0)
store volatile %struct.ScmObj* %args46856$k40479$1, %struct.ScmObj** %stackaddr$prim48409, align 8
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%args46856$k40479$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41136, %struct.ScmObj* %args46856$k40479$1)
store volatile %struct.ScmObj* %args46856$k40479$2, %struct.ScmObj** %stackaddr$prim48410, align 8
%clofunc48411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40479)
musttail call tailcc void %clofunc48411(%struct.ScmObj* %k40479, %struct.ScmObj* %args46856$k40479$2)
ret void
}

define tailcc void @proc_clo$ae41052(%struct.ScmObj* %env$ae41052,%struct.ScmObj* %current_45args46860) {
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46860)
store volatile %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$prim48412, align 8
%stackaddr$prim48413 = alloca %struct.ScmObj*, align 8
%current_45args46861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46860)
store volatile %struct.ScmObj* %current_45args46861, %struct.ScmObj** %stackaddr$prim48413, align 8
%stackaddr$prim48414 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46861)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim48414, align 8
%ae41054 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48415 = alloca %struct.ScmObj*, align 8
%fptrToInt48416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41055 to i64
%ae41055 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48416)
store volatile %struct.ScmObj* %ae41055, %struct.ScmObj** %stackaddr$makeclosure48415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41055, %struct.ScmObj* %_37foldl140108, i64 0)
%args46874$k40480$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48417 = alloca %struct.ScmObj*, align 8
%args46874$k40480$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41055, %struct.ScmObj* %args46874$k40480$0)
store volatile %struct.ScmObj* %args46874$k40480$1, %struct.ScmObj** %stackaddr$prim48417, align 8
%stackaddr$prim48418 = alloca %struct.ScmObj*, align 8
%args46874$k40480$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41054, %struct.ScmObj* %args46874$k40480$1)
store volatile %struct.ScmObj* %args46874$k40480$2, %struct.ScmObj** %stackaddr$prim48418, align 8
%clofunc48419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40480)
musttail call tailcc void %clofunc48419(%struct.ScmObj* %k40480, %struct.ScmObj* %args46874$k40480$2)
ret void
}

define tailcc void @proc_clo$ae41055(%struct.ScmObj* %env$ae41055,%struct.ScmObj* %current_45args46863) {
%stackaddr$env-ref48420 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41055, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48420
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46863)
store volatile %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$prim48421, align 8
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%current_45args46864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46863)
store volatile %struct.ScmObj* %current_45args46864, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46864)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim48423, align 8
%stackaddr$prim48424 = alloca %struct.ScmObj*, align 8
%current_45args46865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46864)
store volatile %struct.ScmObj* %current_45args46865, %struct.ScmObj** %stackaddr$prim48424, align 8
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%current_45args46866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46865)
store volatile %struct.ScmObj* %current_45args46866, %struct.ScmObj** %stackaddr$prim48426, align 8
%stackaddr$prim48427 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46866)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim48427, align 8
%stackaddr$prim48428 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim48428, align 8
%truthy$cmp48429 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40250)
%cmp$cmp48429 = icmp eq i64 %truthy$cmp48429, 1
br i1 %cmp$cmp48429, label %truebranch$cmp48429, label %falsebranch$cmp48429
truebranch$cmp48429:
%ae41059 = call %struct.ScmObj* @const_init_int(i64 0)
%args46868$k40481$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48430 = alloca %struct.ScmObj*, align 8
%args46868$k40481$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46868$k40481$0)
store volatile %struct.ScmObj* %args46868$k40481$1, %struct.ScmObj** %stackaddr$prim48430, align 8
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%args46868$k40481$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41059, %struct.ScmObj* %args46868$k40481$1)
store volatile %struct.ScmObj* %args46868$k40481$2, %struct.ScmObj** %stackaddr$prim48431, align 8
%clofunc48432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40481)
musttail call tailcc void %clofunc48432(%struct.ScmObj* %k40481, %struct.ScmObj* %args46868$k40481$2)
ret void
falsebranch$cmp48429:
%stackaddr$prim48433 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48433, align 8
%stackaddr$makeclosure48434 = alloca %struct.ScmObj*, align 8
%fptrToInt48435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41066 to i64
%ae41066 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48435)
store volatile %struct.ScmObj* %ae41066, %struct.ScmObj** %stackaddr$makeclosure48434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41066, %struct.ScmObj* %k40481, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41066, %struct.ScmObj* %f40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41066, %struct.ScmObj* %lst40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41066, %struct.ScmObj* %_37foldl140108, i64 3)
%args46873$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48436 = alloca %struct.ScmObj*, align 8
%args46873$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args46873$f40111$0)
store volatile %struct.ScmObj* %args46873$f40111$1, %struct.ScmObj** %stackaddr$prim48436, align 8
%stackaddr$prim48437 = alloca %struct.ScmObj*, align 8
%args46873$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args46873$f40111$1)
store volatile %struct.ScmObj* %args46873$f40111$2, %struct.ScmObj** %stackaddr$prim48437, align 8
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%args46873$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41066, %struct.ScmObj* %args46873$f40111$2)
store volatile %struct.ScmObj* %args46873$f40111$3, %struct.ScmObj** %stackaddr$prim48438, align 8
%clofunc48439 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc48439(%struct.ScmObj* %f40111, %struct.ScmObj* %args46873$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41066(%struct.ScmObj* %env$ae41066,%struct.ScmObj* %current_45args46869) {
%stackaddr$env-ref48440 = alloca %struct.ScmObj*, align 8
%k40481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41066, i64 0)
store %struct.ScmObj* %k40481, %struct.ScmObj** %stackaddr$env-ref48440
%stackaddr$env-ref48441 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41066, i64 1)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref48441
%stackaddr$env-ref48442 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41066, i64 2)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref48442
%stackaddr$env-ref48443 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41066, i64 3)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref48443
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46869)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%current_45args46870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46869)
store volatile %struct.ScmObj* %current_45args46870, %struct.ScmObj** %stackaddr$prim48445, align 8
%stackaddr$prim48446 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46870)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim48446, align 8
%stackaddr$prim48447 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim48447, align 8
%args46872$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48448 = alloca %struct.ScmObj*, align 8
%args46872$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args46872$_37foldl140108$0)
store volatile %struct.ScmObj* %args46872$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim48448, align 8
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%args46872$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args46872$_37foldl140108$1)
store volatile %struct.ScmObj* %args46872$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim48449, align 8
%stackaddr$prim48450 = alloca %struct.ScmObj*, align 8
%args46872$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args46872$_37foldl140108$2)
store volatile %struct.ScmObj* %args46872$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim48450, align 8
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%args46872$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40481, %struct.ScmObj* %args46872$_37foldl140108$3)
store volatile %struct.ScmObj* %args46872$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim48451, align 8
%clofunc48452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc48452(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args46872$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae40969(%struct.ScmObj* %env$ae40969,%struct.ScmObj* %current_45args46877) {
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46877)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim48453, align 8
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%current_45args46878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46877)
store volatile %struct.ScmObj* %current_45args46878, %struct.ScmObj** %stackaddr$prim48454, align 8
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46878)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim48455, align 8
%ae40971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48456 = alloca %struct.ScmObj*, align 8
%fptrToInt48457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40972 to i64
%ae40972 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48457)
store volatile %struct.ScmObj* %ae40972, %struct.ScmObj** %stackaddr$makeclosure48456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40972, %struct.ScmObj* %_37length40113, i64 0)
%args46889$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48458 = alloca %struct.ScmObj*, align 8
%args46889$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40972, %struct.ScmObj* %args46889$k40483$0)
store volatile %struct.ScmObj* %args46889$k40483$1, %struct.ScmObj** %stackaddr$prim48458, align 8
%stackaddr$prim48459 = alloca %struct.ScmObj*, align 8
%args46889$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40971, %struct.ScmObj* %args46889$k40483$1)
store volatile %struct.ScmObj* %args46889$k40483$2, %struct.ScmObj** %stackaddr$prim48459, align 8
%clofunc48460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc48460(%struct.ScmObj* %k40483, %struct.ScmObj* %args46889$k40483$2)
ret void
}

define tailcc void @proc_clo$ae40972(%struct.ScmObj* %env$ae40972,%struct.ScmObj* %current_45args46880) {
%stackaddr$env-ref48461 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40972, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref48461
%stackaddr$prim48462 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46880)
store volatile %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$prim48462, align 8
%stackaddr$prim48463 = alloca %struct.ScmObj*, align 8
%current_45args46881 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46880)
store volatile %struct.ScmObj* %current_45args46881, %struct.ScmObj** %stackaddr$prim48463, align 8
%stackaddr$prim48464 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46881)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim48464, align 8
%stackaddr$prim48465 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48465, align 8
%truthy$cmp48466 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40246)
%cmp$cmp48466 = icmp eq i64 %truthy$cmp48466, 1
br i1 %cmp$cmp48466, label %truebranch$cmp48466, label %falsebranch$cmp48466
truebranch$cmp48466:
%ae40976 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40977 = call %struct.ScmObj* @const_init_int(i64 0)
%args46883$k40484$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%args46883$k40484$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40977, %struct.ScmObj* %args46883$k40484$0)
store volatile %struct.ScmObj* %args46883$k40484$1, %struct.ScmObj** %stackaddr$prim48467, align 8
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%args46883$k40484$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40976, %struct.ScmObj* %args46883$k40484$1)
store volatile %struct.ScmObj* %args46883$k40484$2, %struct.ScmObj** %stackaddr$prim48468, align 8
%clofunc48469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40484)
musttail call tailcc void %clofunc48469(%struct.ScmObj* %k40484, %struct.ScmObj* %args46883$k40484$2)
ret void
falsebranch$cmp48466:
%stackaddr$prim48470 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim48470, align 8
%stackaddr$makeclosure48471 = alloca %struct.ScmObj*, align 8
%fptrToInt48472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40986 to i64
%ae40986 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48472)
store volatile %struct.ScmObj* %ae40986, %struct.ScmObj** %stackaddr$makeclosure48471, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40986, %struct.ScmObj* %k40484, i64 0)
%args46888$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%args46888$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args46888$_37length40113$0)
store volatile %struct.ScmObj* %args46888$_37length40113$1, %struct.ScmObj** %stackaddr$prim48473, align 8
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%args46888$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40986, %struct.ScmObj* %args46888$_37length40113$1)
store volatile %struct.ScmObj* %args46888$_37length40113$2, %struct.ScmObj** %stackaddr$prim48474, align 8
%clofunc48475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc48475(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args46888$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae40986(%struct.ScmObj* %env$ae40986,%struct.ScmObj* %current_45args46884) {
%stackaddr$env-ref48476 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40986, i64 0)
store %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$env-ref48476
%stackaddr$prim48477 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46884)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48477, align 8
%stackaddr$prim48478 = alloca %struct.ScmObj*, align 8
%current_45args46885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46884)
store volatile %struct.ScmObj* %current_45args46885, %struct.ScmObj** %stackaddr$prim48478, align 8
%stackaddr$prim48479 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46885)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim48479, align 8
%ae40988 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48480 = alloca %struct.ScmObj*, align 8
%cpsprim40486 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae40988, %struct.ScmObj* %anf_45bind40248)
store volatile %struct.ScmObj* %cpsprim40486, %struct.ScmObj** %stackaddr$prim48480, align 8
%ae40991 = call %struct.ScmObj* @const_init_int(i64 0)
%args46887$k40484$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%args46887$k40484$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40486, %struct.ScmObj* %args46887$k40484$0)
store volatile %struct.ScmObj* %args46887$k40484$1, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%args46887$k40484$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40991, %struct.ScmObj* %args46887$k40484$1)
store volatile %struct.ScmObj* %args46887$k40484$2, %struct.ScmObj** %stackaddr$prim48482, align 8
%clofunc48483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40484)
musttail call tailcc void %clofunc48483(%struct.ScmObj* %k40484, %struct.ScmObj* %args46887$k40484$2)
ret void
}

define tailcc void @proc_clo$ae40819(%struct.ScmObj* %env$ae40819,%struct.ScmObj* %current_45args46892) {
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46892)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%current_45args46893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46892)
store volatile %struct.ScmObj* %current_45args46893, %struct.ScmObj** %stackaddr$prim48485, align 8
%stackaddr$prim48486 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46893)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim48486, align 8
%ae40821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48487 = alloca %struct.ScmObj*, align 8
%fptrToInt48488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40822 to i64
%ae40822 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48488)
store volatile %struct.ScmObj* %ae40822, %struct.ScmObj** %stackaddr$makeclosure48487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40822, %struct.ScmObj* %_37take40116, i64 0)
%args46906$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%args46906$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40822, %struct.ScmObj* %args46906$k40487$0)
store volatile %struct.ScmObj* %args46906$k40487$1, %struct.ScmObj** %stackaddr$prim48489, align 8
%stackaddr$prim48490 = alloca %struct.ScmObj*, align 8
%args46906$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40821, %struct.ScmObj* %args46906$k40487$1)
store volatile %struct.ScmObj* %args46906$k40487$2, %struct.ScmObj** %stackaddr$prim48490, align 8
%clofunc48491 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc48491(%struct.ScmObj* %k40487, %struct.ScmObj* %args46906$k40487$2)
ret void
}

define tailcc void @proc_clo$ae40822(%struct.ScmObj* %env$ae40822,%struct.ScmObj* %current_45args46895) {
%stackaddr$env-ref48492 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40822, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref48492
%stackaddr$prim48493 = alloca %struct.ScmObj*, align 8
%k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %k40488, %struct.ScmObj** %stackaddr$prim48493, align 8
%stackaddr$prim48494 = alloca %struct.ScmObj*, align 8
%current_45args46896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46895)
store volatile %struct.ScmObj* %current_45args46896, %struct.ScmObj** %stackaddr$prim48494, align 8
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim48495, align 8
%stackaddr$prim48496 = alloca %struct.ScmObj*, align 8
%current_45args46897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46896)
store volatile %struct.ScmObj* %current_45args46897, %struct.ScmObj** %stackaddr$prim48496, align 8
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46897)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim48497, align 8
%ae40824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40824)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim48498, align 8
%truthy$cmp48499 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40239)
%cmp$cmp48499 = icmp eq i64 %truthy$cmp48499, 1
br i1 %cmp$cmp48499, label %truebranch$cmp48499, label %falsebranch$cmp48499
truebranch$cmp48499:
%ae40827 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40828 = call %struct.ScmObj* @const_init_null()
%args46899$k40488$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48500 = alloca %struct.ScmObj*, align 8
%args46899$k40488$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40828, %struct.ScmObj* %args46899$k40488$0)
store volatile %struct.ScmObj* %args46899$k40488$1, %struct.ScmObj** %stackaddr$prim48500, align 8
%stackaddr$prim48501 = alloca %struct.ScmObj*, align 8
%args46899$k40488$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40827, %struct.ScmObj* %args46899$k40488$1)
store volatile %struct.ScmObj* %args46899$k40488$2, %struct.ScmObj** %stackaddr$prim48501, align 8
%clofunc48502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40488)
musttail call tailcc void %clofunc48502(%struct.ScmObj* %k40488, %struct.ScmObj* %args46899$k40488$2)
ret void
falsebranch$cmp48499:
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim48503, align 8
%truthy$cmp48504 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40240)
%cmp$cmp48504 = icmp eq i64 %truthy$cmp48504, 1
br i1 %cmp$cmp48504, label %truebranch$cmp48504, label %falsebranch$cmp48504
truebranch$cmp48504:
%ae40838 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40839 = call %struct.ScmObj* @const_init_null()
%args46900$k40488$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48505 = alloca %struct.ScmObj*, align 8
%args46900$k40488$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40839, %struct.ScmObj* %args46900$k40488$0)
store volatile %struct.ScmObj* %args46900$k40488$1, %struct.ScmObj** %stackaddr$prim48505, align 8
%stackaddr$prim48506 = alloca %struct.ScmObj*, align 8
%args46900$k40488$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40838, %struct.ScmObj* %args46900$k40488$1)
store volatile %struct.ScmObj* %args46900$k40488$2, %struct.ScmObj** %stackaddr$prim48506, align 8
%clofunc48507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40488)
musttail call tailcc void %clofunc48507(%struct.ScmObj* %k40488, %struct.ScmObj* %args46900$k40488$2)
ret void
falsebranch$cmp48504:
%stackaddr$prim48508 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim48508, align 8
%stackaddr$prim48509 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim48509, align 8
%ae40849 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48510 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40849)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim48510, align 8
%stackaddr$makeclosure48511 = alloca %struct.ScmObj*, align 8
%fptrToInt48512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40851 to i64
%ae40851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48512)
store volatile %struct.ScmObj* %ae40851, %struct.ScmObj** %stackaddr$makeclosure48511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %anf_45bind40241, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %k40488, i64 1)
%args46905$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48513 = alloca %struct.ScmObj*, align 8
%args46905$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args46905$_37take40116$0)
store volatile %struct.ScmObj* %args46905$_37take40116$1, %struct.ScmObj** %stackaddr$prim48513, align 8
%stackaddr$prim48514 = alloca %struct.ScmObj*, align 8
%args46905$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args46905$_37take40116$1)
store volatile %struct.ScmObj* %args46905$_37take40116$2, %struct.ScmObj** %stackaddr$prim48514, align 8
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%args46905$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40851, %struct.ScmObj* %args46905$_37take40116$2)
store volatile %struct.ScmObj* %args46905$_37take40116$3, %struct.ScmObj** %stackaddr$prim48515, align 8
%clofunc48516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc48516(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args46905$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40851(%struct.ScmObj* %env$ae40851,%struct.ScmObj* %current_45args46901) {
%stackaddr$env-ref48517 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 0)
store %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$env-ref48517
%stackaddr$env-ref48518 = alloca %struct.ScmObj*, align 8
%k40488 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 1)
store %struct.ScmObj* %k40488, %struct.ScmObj** %stackaddr$env-ref48518
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46901)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim48519, align 8
%stackaddr$prim48520 = alloca %struct.ScmObj*, align 8
%current_45args46902 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46901)
store volatile %struct.ScmObj* %current_45args46902, %struct.ScmObj** %stackaddr$prim48520, align 8
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46902)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim48521, align 8
%stackaddr$prim48522 = alloca %struct.ScmObj*, align 8
%cpsprim40490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %anf_45bind40244)
store volatile %struct.ScmObj* %cpsprim40490, %struct.ScmObj** %stackaddr$prim48522, align 8
%ae40857 = call %struct.ScmObj* @const_init_int(i64 0)
%args46904$k40488$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%args46904$k40488$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40490, %struct.ScmObj* %args46904$k40488$0)
store volatile %struct.ScmObj* %args46904$k40488$1, %struct.ScmObj** %stackaddr$prim48523, align 8
%stackaddr$prim48524 = alloca %struct.ScmObj*, align 8
%args46904$k40488$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40857, %struct.ScmObj* %args46904$k40488$1)
store volatile %struct.ScmObj* %args46904$k40488$2, %struct.ScmObj** %stackaddr$prim48524, align 8
%clofunc48525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40488)
musttail call tailcc void %clofunc48525(%struct.ScmObj* %k40488, %struct.ScmObj* %args46904$k40488$2)
ret void
}

define tailcc void @proc_clo$ae40722(%struct.ScmObj* %env$ae40722,%struct.ScmObj* %current_45args46909) {
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46909)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim48526, align 8
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%current_45args46910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46909)
store volatile %struct.ScmObj* %current_45args46910, %struct.ScmObj** %stackaddr$prim48527, align 8
%stackaddr$prim48528 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46910)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim48528, align 8
%ae40724 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48529 = alloca %struct.ScmObj*, align 8
%fptrToInt48530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40725 to i64
%ae40725 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48530)
store volatile %struct.ScmObj* %ae40725, %struct.ScmObj** %stackaddr$makeclosure48529, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40725, %struct.ScmObj* %_37map40120, i64 0)
%args46926$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%args46926$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40725, %struct.ScmObj* %args46926$k40491$0)
store volatile %struct.ScmObj* %args46926$k40491$1, %struct.ScmObj** %stackaddr$prim48531, align 8
%stackaddr$prim48532 = alloca %struct.ScmObj*, align 8
%args46926$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40724, %struct.ScmObj* %args46926$k40491$1)
store volatile %struct.ScmObj* %args46926$k40491$2, %struct.ScmObj** %stackaddr$prim48532, align 8
%clofunc48533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc48533(%struct.ScmObj* %k40491, %struct.ScmObj* %args46926$k40491$2)
ret void
}

define tailcc void @proc_clo$ae40725(%struct.ScmObj* %env$ae40725,%struct.ScmObj* %current_45args46912) {
%stackaddr$env-ref48534 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40725, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48534
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46912)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim48535, align 8
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%current_45args46913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46912)
store volatile %struct.ScmObj* %current_45args46913, %struct.ScmObj** %stackaddr$prim48536, align 8
%stackaddr$prim48537 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim48537, align 8
%stackaddr$prim48538 = alloca %struct.ScmObj*, align 8
%current_45args46914 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46913)
store volatile %struct.ScmObj* %current_45args46914, %struct.ScmObj** %stackaddr$prim48538, align 8
%stackaddr$prim48539 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46914)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim48539, align 8
%stackaddr$prim48540 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim48540, align 8
%truthy$cmp48541 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40233)
%cmp$cmp48541 = icmp eq i64 %truthy$cmp48541, 1
br i1 %cmp$cmp48541, label %truebranch$cmp48541, label %falsebranch$cmp48541
truebranch$cmp48541:
%ae40729 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40730 = call %struct.ScmObj* @const_init_null()
%args46916$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%args46916$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40730, %struct.ScmObj* %args46916$k40492$0)
store volatile %struct.ScmObj* %args46916$k40492$1, %struct.ScmObj** %stackaddr$prim48542, align 8
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%args46916$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40729, %struct.ScmObj* %args46916$k40492$1)
store volatile %struct.ScmObj* %args46916$k40492$2, %struct.ScmObj** %stackaddr$prim48543, align 8
%clofunc48544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc48544(%struct.ScmObj* %k40492, %struct.ScmObj* %args46916$k40492$2)
ret void
falsebranch$cmp48541:
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim48545, align 8
%stackaddr$makeclosure48546 = alloca %struct.ScmObj*, align 8
%fptrToInt48547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40739 to i64
%ae40739 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48547)
store volatile %struct.ScmObj* %ae40739, %struct.ScmObj** %stackaddr$makeclosure48546, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %k40492, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %f40122, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %lst40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40739, %struct.ScmObj* %_37map40120, i64 3)
%args46925$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%args46925$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %args46925$f40122$0)
store volatile %struct.ScmObj* %args46925$f40122$1, %struct.ScmObj** %stackaddr$prim48548, align 8
%stackaddr$prim48549 = alloca %struct.ScmObj*, align 8
%args46925$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40739, %struct.ScmObj* %args46925$f40122$1)
store volatile %struct.ScmObj* %args46925$f40122$2, %struct.ScmObj** %stackaddr$prim48549, align 8
%clofunc48550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc48550(%struct.ScmObj* %f40122, %struct.ScmObj* %args46925$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40739(%struct.ScmObj* %env$ae40739,%struct.ScmObj* %current_45args46917) {
%stackaddr$env-ref48551 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 0)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref48551
%stackaddr$env-ref48552 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 1)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref48552
%stackaddr$env-ref48553 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 2)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref48553
%stackaddr$env-ref48554 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40739, i64 3)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref48554
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46917)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim48555, align 8
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%current_45args46918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46917)
store volatile %struct.ScmObj* %current_45args46918, %struct.ScmObj** %stackaddr$prim48556, align 8
%stackaddr$prim48557 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46918)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim48557, align 8
%stackaddr$prim48558 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim48558, align 8
%stackaddr$makeclosure48559 = alloca %struct.ScmObj*, align 8
%fptrToInt48560 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40743 to i64
%ae40743 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48560)
store volatile %struct.ScmObj* %ae40743, %struct.ScmObj** %stackaddr$makeclosure48559, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40743, %struct.ScmObj* %k40492, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40743, %struct.ScmObj* %anf_45bind40235, i64 1)
%args46924$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%args46924$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args46924$_37map40120$0)
store volatile %struct.ScmObj* %args46924$_37map40120$1, %struct.ScmObj** %stackaddr$prim48561, align 8
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%args46924$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args46924$_37map40120$1)
store volatile %struct.ScmObj* %args46924$_37map40120$2, %struct.ScmObj** %stackaddr$prim48562, align 8
%stackaddr$prim48563 = alloca %struct.ScmObj*, align 8
%args46924$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40743, %struct.ScmObj* %args46924$_37map40120$2)
store volatile %struct.ScmObj* %args46924$_37map40120$3, %struct.ScmObj** %stackaddr$prim48563, align 8
%clofunc48564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc48564(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args46924$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40743(%struct.ScmObj* %env$ae40743,%struct.ScmObj* %current_45args46920) {
%stackaddr$env-ref48565 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40743, i64 0)
store %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$env-ref48565
%stackaddr$env-ref48566 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40743, i64 1)
store %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$env-ref48566
%stackaddr$prim48567 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46920)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim48567, align 8
%stackaddr$prim48568 = alloca %struct.ScmObj*, align 8
%current_45args46921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46920)
store volatile %struct.ScmObj* %current_45args46921, %struct.ScmObj** %stackaddr$prim48568, align 8
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46921)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim48569, align 8
%stackaddr$prim48570 = alloca %struct.ScmObj*, align 8
%cpsprim40495 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %anf_45bind40237)
store volatile %struct.ScmObj* %cpsprim40495, %struct.ScmObj** %stackaddr$prim48570, align 8
%ae40749 = call %struct.ScmObj* @const_init_int(i64 0)
%args46923$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%args46923$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40495, %struct.ScmObj* %args46923$k40492$0)
store volatile %struct.ScmObj* %args46923$k40492$1, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%args46923$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40749, %struct.ScmObj* %args46923$k40492$1)
store volatile %struct.ScmObj* %args46923$k40492$2, %struct.ScmObj** %stackaddr$prim48572, align 8
%clofunc48573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc48573(%struct.ScmObj* %k40492, %struct.ScmObj* %args46923$k40492$2)
ret void
}

define tailcc void @proc_clo$ae40642(%struct.ScmObj* %env$ae40642,%struct.ScmObj* %current_45args46929) {
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46929)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%current_45args46930 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46929)
store volatile %struct.ScmObj* %current_45args46930, %struct.ScmObj** %stackaddr$prim48575, align 8
%stackaddr$prim48576 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46930)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim48576, align 8
%ae40644 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48577 = alloca %struct.ScmObj*, align 8
%fptrToInt48578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40645 to i64
%ae40645 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48578)
store volatile %struct.ScmObj* %ae40645, %struct.ScmObj** %stackaddr$makeclosure48577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40645, %struct.ScmObj* %_37foldr140124, i64 0)
%args46943$k40496$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48579 = alloca %struct.ScmObj*, align 8
%args46943$k40496$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40645, %struct.ScmObj* %args46943$k40496$0)
store volatile %struct.ScmObj* %args46943$k40496$1, %struct.ScmObj** %stackaddr$prim48579, align 8
%stackaddr$prim48580 = alloca %struct.ScmObj*, align 8
%args46943$k40496$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40644, %struct.ScmObj* %args46943$k40496$1)
store volatile %struct.ScmObj* %args46943$k40496$2, %struct.ScmObj** %stackaddr$prim48580, align 8
%clofunc48581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40496)
musttail call tailcc void %clofunc48581(%struct.ScmObj* %k40496, %struct.ScmObj* %args46943$k40496$2)
ret void
}

define tailcc void @proc_clo$ae40645(%struct.ScmObj* %env$ae40645,%struct.ScmObj* %current_45args46932) {
%stackaddr$env-ref48582 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40645, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref48582
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46932)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim48583, align 8
%stackaddr$prim48584 = alloca %struct.ScmObj*, align 8
%current_45args46933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46932)
store volatile %struct.ScmObj* %current_45args46933, %struct.ScmObj** %stackaddr$prim48584, align 8
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46933)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim48585, align 8
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%current_45args46934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46933)
store volatile %struct.ScmObj* %current_45args46934, %struct.ScmObj** %stackaddr$prim48586, align 8
%stackaddr$prim48587 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim48587, align 8
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%current_45args46935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46934)
store volatile %struct.ScmObj* %current_45args46935, %struct.ScmObj** %stackaddr$prim48588, align 8
%stackaddr$prim48589 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46935)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim48589, align 8
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$prim48590, align 8
%truthy$cmp48591 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40228)
%cmp$cmp48591 = icmp eq i64 %truthy$cmp48591, 1
br i1 %cmp$cmp48591, label %truebranch$cmp48591, label %falsebranch$cmp48591
truebranch$cmp48591:
%ae40649 = call %struct.ScmObj* @const_init_int(i64 0)
%args46937$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48592 = alloca %struct.ScmObj*, align 8
%args46937$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args46937$k40497$0)
store volatile %struct.ScmObj* %args46937$k40497$1, %struct.ScmObj** %stackaddr$prim48592, align 8
%stackaddr$prim48593 = alloca %struct.ScmObj*, align 8
%args46937$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40649, %struct.ScmObj* %args46937$k40497$1)
store volatile %struct.ScmObj* %args46937$k40497$2, %struct.ScmObj** %stackaddr$prim48593, align 8
%clofunc48594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc48594(%struct.ScmObj* %k40497, %struct.ScmObj* %args46937$k40497$2)
ret void
falsebranch$cmp48591:
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim48595, align 8
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim48596, align 8
%stackaddr$makeclosure48597 = alloca %struct.ScmObj*, align 8
%fptrToInt48598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40657 to i64
%ae40657 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48598)
store volatile %struct.ScmObj* %ae40657, %struct.ScmObj** %stackaddr$makeclosure48597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40657, %struct.ScmObj* %anf_45bind40229, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40657, %struct.ScmObj* %k40497, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40657, %struct.ScmObj* %f40127, i64 2)
%args46942$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48599 = alloca %struct.ScmObj*, align 8
%args46942$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args46942$_37foldr140124$0)
store volatile %struct.ScmObj* %args46942$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim48599, align 8
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%args46942$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args46942$_37foldr140124$1)
store volatile %struct.ScmObj* %args46942$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim48600, align 8
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%args46942$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args46942$_37foldr140124$2)
store volatile %struct.ScmObj* %args46942$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim48601, align 8
%stackaddr$prim48602 = alloca %struct.ScmObj*, align 8
%args46942$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40657, %struct.ScmObj* %args46942$_37foldr140124$3)
store volatile %struct.ScmObj* %args46942$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim48602, align 8
%clofunc48603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc48603(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args46942$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40657(%struct.ScmObj* %env$ae40657,%struct.ScmObj* %current_45args46938) {
%stackaddr$env-ref48604 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40657, i64 0)
store %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$env-ref48604
%stackaddr$env-ref48605 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40657, i64 1)
store %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$env-ref48605
%stackaddr$env-ref48606 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40657, i64 2)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref48606
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%_95k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46938)
store volatile %struct.ScmObj* %_95k40498, %struct.ScmObj** %stackaddr$prim48607, align 8
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%current_45args46939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46938)
store volatile %struct.ScmObj* %current_45args46939, %struct.ScmObj** %stackaddr$prim48608, align 8
%stackaddr$prim48609 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46939)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim48609, align 8
%args46941$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48610 = alloca %struct.ScmObj*, align 8
%args46941$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args46941$f40127$0)
store volatile %struct.ScmObj* %args46941$f40127$1, %struct.ScmObj** %stackaddr$prim48610, align 8
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%args46941$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %args46941$f40127$1)
store volatile %struct.ScmObj* %args46941$f40127$2, %struct.ScmObj** %stackaddr$prim48611, align 8
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%args46941$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40497, %struct.ScmObj* %args46941$f40127$2)
store volatile %struct.ScmObj* %args46941$f40127$3, %struct.ScmObj** %stackaddr$prim48612, align 8
%clofunc48613 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc48613(%struct.ScmObj* %f40127, %struct.ScmObj* %args46941$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40525(%struct.ScmObj* %env$ae40525,%struct.ScmObj* %current_45args46946) {
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46946)
store volatile %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$prim48614, align 8
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%current_45args46947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46946)
store volatile %struct.ScmObj* %current_45args46947, %struct.ScmObj** %stackaddr$prim48615, align 8
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46947)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim48616, align 8
%ae40527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48617 = alloca %struct.ScmObj*, align 8
%fptrToInt48618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40528 to i64
%ae40528 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48618)
store volatile %struct.ScmObj* %ae40528, %struct.ScmObj** %stackaddr$makeclosure48617, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40528, %struct.ScmObj* %y40104, i64 0)
%args46965$k40499$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48619 = alloca %struct.ScmObj*, align 8
%args46965$k40499$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40528, %struct.ScmObj* %args46965$k40499$0)
store volatile %struct.ScmObj* %args46965$k40499$1, %struct.ScmObj** %stackaddr$prim48619, align 8
%stackaddr$prim48620 = alloca %struct.ScmObj*, align 8
%args46965$k40499$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40527, %struct.ScmObj* %args46965$k40499$1)
store volatile %struct.ScmObj* %args46965$k40499$2, %struct.ScmObj** %stackaddr$prim48620, align 8
%clofunc48621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40499)
musttail call tailcc void %clofunc48621(%struct.ScmObj* %k40499, %struct.ScmObj* %args46965$k40499$2)
ret void
}

define tailcc void @proc_clo$ae40528(%struct.ScmObj* %env$ae40528,%struct.ScmObj* %current_45args46949) {
%stackaddr$env-ref48622 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40528, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48622
%stackaddr$prim48623 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46949)
store volatile %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$prim48623, align 8
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%current_45args46950 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46949)
store volatile %struct.ScmObj* %current_45args46950, %struct.ScmObj** %stackaddr$prim48624, align 8
%stackaddr$prim48625 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46950)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim48625, align 8
%stackaddr$makeclosure48626 = alloca %struct.ScmObj*, align 8
%fptrToInt48627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40529 to i64
%ae40529 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48627)
store volatile %struct.ScmObj* %ae40529, %struct.ScmObj** %stackaddr$makeclosure48626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40529, %struct.ScmObj* %k40500, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40529, %struct.ScmObj* %f40105, i64 1)
%ae40530 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48628 = alloca %struct.ScmObj*, align 8
%fptrToInt48629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40531 to i64
%ae40531 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48629)
store volatile %struct.ScmObj* %ae40531, %struct.ScmObj** %stackaddr$makeclosure48628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40531, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40531, %struct.ScmObj* %y40104, i64 1)
%args46964$ae40529$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%args46964$ae40529$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40531, %struct.ScmObj* %args46964$ae40529$0)
store volatile %struct.ScmObj* %args46964$ae40529$1, %struct.ScmObj** %stackaddr$prim48630, align 8
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%args46964$ae40529$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40530, %struct.ScmObj* %args46964$ae40529$1)
store volatile %struct.ScmObj* %args46964$ae40529$2, %struct.ScmObj** %stackaddr$prim48631, align 8
%clofunc48632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40529)
musttail call tailcc void %clofunc48632(%struct.ScmObj* %ae40529, %struct.ScmObj* %args46964$ae40529$2)
ret void
}

define tailcc void @proc_clo$ae40529(%struct.ScmObj* %env$ae40529,%struct.ScmObj* %current_45args46952) {
%stackaddr$env-ref48633 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40529, i64 0)
store %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$env-ref48633
%stackaddr$env-ref48634 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40529, i64 1)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48634
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46952)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim48635, align 8
%stackaddr$prim48636 = alloca %struct.ScmObj*, align 8
%current_45args46953 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46952)
store volatile %struct.ScmObj* %current_45args46953, %struct.ScmObj** %stackaddr$prim48636, align 8
%stackaddr$prim48637 = alloca %struct.ScmObj*, align 8
%anf_45bind40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46953)
store volatile %struct.ScmObj* %anf_45bind40226, %struct.ScmObj** %stackaddr$prim48637, align 8
%args46955$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48638 = alloca %struct.ScmObj*, align 8
%args46955$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40226, %struct.ScmObj* %args46955$f40105$0)
store volatile %struct.ScmObj* %args46955$f40105$1, %struct.ScmObj** %stackaddr$prim48638, align 8
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%args46955$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40500, %struct.ScmObj* %args46955$f40105$1)
store volatile %struct.ScmObj* %args46955$f40105$2, %struct.ScmObj** %stackaddr$prim48639, align 8
%clofunc48640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc48640(%struct.ScmObj* %f40105, %struct.ScmObj* %args46955$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40531(%struct.ScmObj* %env$ae40531,%struct.ScmObj* %args4010640502) {
%stackaddr$env-ref48641 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40531, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48641
%stackaddr$env-ref48642 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40531, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref48642
%stackaddr$prim48643 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640502)
store volatile %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$prim48643, align 8
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640502)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim48644, align 8
%stackaddr$makeclosure48645 = alloca %struct.ScmObj*, align 8
%fptrToInt48646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40535 to i64
%ae40535 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48646)
store volatile %struct.ScmObj* %ae40535, %struct.ScmObj** %stackaddr$makeclosure48645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40535, %struct.ScmObj* %k40503, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40535, %struct.ScmObj* %args40106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40535, %struct.ScmObj* %f40105, i64 2)
%args46963$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48647 = alloca %struct.ScmObj*, align 8
%args46963$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args46963$y40104$0)
store volatile %struct.ScmObj* %args46963$y40104$1, %struct.ScmObj** %stackaddr$prim48647, align 8
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%args46963$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40535, %struct.ScmObj* %args46963$y40104$1)
store volatile %struct.ScmObj* %args46963$y40104$2, %struct.ScmObj** %stackaddr$prim48648, align 8
%clofunc48649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc48649(%struct.ScmObj* %y40104, %struct.ScmObj* %args46963$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40535(%struct.ScmObj* %env$ae40535,%struct.ScmObj* %current_45args46956) {
%stackaddr$env-ref48650 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40535, i64 0)
store %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$env-ref48650
%stackaddr$env-ref48651 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40535, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48651
%stackaddr$env-ref48652 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40535, i64 2)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref48652
%stackaddr$prim48653 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46956)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim48653, align 8
%stackaddr$prim48654 = alloca %struct.ScmObj*, align 8
%current_45args46957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46956)
store volatile %struct.ScmObj* %current_45args46957, %struct.ScmObj** %stackaddr$prim48654, align 8
%stackaddr$prim48655 = alloca %struct.ScmObj*, align 8
%anf_45bind40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46957)
store volatile %struct.ScmObj* %anf_45bind40224, %struct.ScmObj** %stackaddr$prim48655, align 8
%stackaddr$makeclosure48656 = alloca %struct.ScmObj*, align 8
%fptrToInt48657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40538 to i64
%ae40538 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48657)
store volatile %struct.ScmObj* %ae40538, %struct.ScmObj** %stackaddr$makeclosure48656, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40538, %struct.ScmObj* %k40503, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40538, %struct.ScmObj* %args40106, i64 1)
%args46962$anf_45bind40224$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48658 = alloca %struct.ScmObj*, align 8
%args46962$anf_45bind40224$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args46962$anf_45bind40224$0)
store volatile %struct.ScmObj* %args46962$anf_45bind40224$1, %struct.ScmObj** %stackaddr$prim48658, align 8
%stackaddr$prim48659 = alloca %struct.ScmObj*, align 8
%args46962$anf_45bind40224$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40538, %struct.ScmObj* %args46962$anf_45bind40224$1)
store volatile %struct.ScmObj* %args46962$anf_45bind40224$2, %struct.ScmObj** %stackaddr$prim48659, align 8
%clofunc48660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40224)
musttail call tailcc void %clofunc48660(%struct.ScmObj* %anf_45bind40224, %struct.ScmObj* %args46962$anf_45bind40224$2)
ret void
}

define tailcc void @proc_clo$ae40538(%struct.ScmObj* %env$ae40538,%struct.ScmObj* %current_45args46959) {
%stackaddr$env-ref48661 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40538, i64 0)
store %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$env-ref48661
%stackaddr$env-ref48662 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40538, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref48662
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%_95k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46959)
store volatile %struct.ScmObj* %_95k40505, %struct.ScmObj** %stackaddr$prim48663, align 8
%stackaddr$prim48664 = alloca %struct.ScmObj*, align 8
%current_45args46960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46959)
store volatile %struct.ScmObj* %current_45args46960, %struct.ScmObj** %stackaddr$prim48664, align 8
%stackaddr$prim48665 = alloca %struct.ScmObj*, align 8
%anf_45bind40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46960)
store volatile %struct.ScmObj* %anf_45bind40225, %struct.ScmObj** %stackaddr$prim48665, align 8
%stackaddr$prim48666 = alloca %struct.ScmObj*, align 8
%cpsargs40506 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40503, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40506, %struct.ScmObj** %stackaddr$prim48666, align 8
%clofunc48667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40225)
musttail call tailcc void %clofunc48667(%struct.ScmObj* %anf_45bind40225, %struct.ScmObj* %cpsargs40506)
ret void
}

define tailcc void @proc_clo$ae40510(%struct.ScmObj* %env$ae40510,%struct.ScmObj* %current_45args46967) {
%stackaddr$prim48668 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46967)
store volatile %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$prim48668, align 8
%stackaddr$prim48669 = alloca %struct.ScmObj*, align 8
%current_45args46968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46967)
store volatile %struct.ScmObj* %current_45args46968, %struct.ScmObj** %stackaddr$prim48669, align 8
%stackaddr$prim48670 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46968)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim48670, align 8
%args46970$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48671 = alloca %struct.ScmObj*, align 8
%args46970$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args46970$yu40103$0)
store volatile %struct.ScmObj* %args46970$yu40103$1, %struct.ScmObj** %stackaddr$prim48671, align 8
%stackaddr$prim48672 = alloca %struct.ScmObj*, align 8
%args46970$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40507, %struct.ScmObj* %args46970$yu40103$1)
store volatile %struct.ScmObj* %args46970$yu40103$2, %struct.ScmObj** %stackaddr$prim48672, align 8
%clofunc48673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc48673(%struct.ScmObj* %yu40103, %struct.ScmObj* %args46970$yu40103$2)
ret void
}