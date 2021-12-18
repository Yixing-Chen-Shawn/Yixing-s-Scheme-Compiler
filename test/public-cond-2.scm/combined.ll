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

@global$sym$ae4389548290 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv47664 = call %struct.ScmObj* @const_init_null()
%mainargs47665 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv47664, %struct.ScmObj* %mainargs47665)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv47662,%struct.ScmObj* %mainargs47663) {
%stackaddr$makeclosure47666 = alloca %struct.ScmObj*, align 8
%fptrToInt47667 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40507 to i64
%ae40507 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47667)
store volatile %struct.ScmObj* %ae40507, %struct.ScmObj** %stackaddr$makeclosure47666, align 8
%ae40508 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47668 = alloca %struct.ScmObj*, align 8
%fptrToInt47669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40509 to i64
%ae40509 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47669)
store volatile %struct.ScmObj* %ae40509, %struct.ScmObj** %stackaddr$makeclosure47668, align 8
%args47661$ae40507$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47670 = alloca %struct.ScmObj*, align 8
%args47661$ae40507$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40509, %struct.ScmObj* %args47661$ae40507$0)
store volatile %struct.ScmObj* %args47661$ae40507$1, %struct.ScmObj** %stackaddr$prim47670, align 8
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%args47661$ae40507$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40508, %struct.ScmObj* %args47661$ae40507$1)
store volatile %struct.ScmObj* %args47661$ae40507$2, %struct.ScmObj** %stackaddr$prim47671, align 8
%clofunc47672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40507)
musttail call tailcc void %clofunc47672(%struct.ScmObj* %ae40507, %struct.ScmObj* %args47661$ae40507$2)
ret void
}

define tailcc void @proc_clo$ae40507(%struct.ScmObj* %env$ae40507,%struct.ScmObj* %current_45args47037) {
%stackaddr$prim47673 = alloca %struct.ScmObj*, align 8
%_95k40339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47037)
store volatile %struct.ScmObj* %_95k40339, %struct.ScmObj** %stackaddr$prim47673, align 8
%stackaddr$prim47674 = alloca %struct.ScmObj*, align 8
%current_45args47038 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47037)
store volatile %struct.ScmObj* %current_45args47038, %struct.ScmObj** %stackaddr$prim47674, align 8
%stackaddr$prim47675 = alloca %struct.ScmObj*, align 8
%anf_45bind40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47038)
store volatile %struct.ScmObj* %anf_45bind40222, %struct.ScmObj** %stackaddr$prim47675, align 8
%stackaddr$makeclosure47676 = alloca %struct.ScmObj*, align 8
%fptrToInt47677 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40522 to i64
%ae40522 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47677)
store volatile %struct.ScmObj* %ae40522, %struct.ScmObj** %stackaddr$makeclosure47676, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40522, %struct.ScmObj* %anf_45bind40222, i64 0)
%ae40523 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47678 = alloca %struct.ScmObj*, align 8
%fptrToInt47679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40524 to i64
%ae40524 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47679)
store volatile %struct.ScmObj* %ae40524, %struct.ScmObj** %stackaddr$makeclosure47678, align 8
%args47656$ae40522$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47680 = alloca %struct.ScmObj*, align 8
%args47656$ae40522$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40524, %struct.ScmObj* %args47656$ae40522$0)
store volatile %struct.ScmObj* %args47656$ae40522$1, %struct.ScmObj** %stackaddr$prim47680, align 8
%stackaddr$prim47681 = alloca %struct.ScmObj*, align 8
%args47656$ae40522$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40523, %struct.ScmObj* %args47656$ae40522$1)
store volatile %struct.ScmObj* %args47656$ae40522$2, %struct.ScmObj** %stackaddr$prim47681, align 8
%clofunc47682 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40522)
musttail call tailcc void %clofunc47682(%struct.ScmObj* %ae40522, %struct.ScmObj* %args47656$ae40522$2)
ret void
}

define tailcc void @proc_clo$ae40522(%struct.ScmObj* %env$ae40522,%struct.ScmObj* %current_45args47040) {
%stackaddr$env-ref47683 = alloca %struct.ScmObj*, align 8
%anf_45bind40222 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40522, i64 0)
store %struct.ScmObj* %anf_45bind40222, %struct.ScmObj** %stackaddr$env-ref47683
%stackaddr$prim47684 = alloca %struct.ScmObj*, align 8
%_95k40340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47040)
store volatile %struct.ScmObj* %_95k40340, %struct.ScmObj** %stackaddr$prim47684, align 8
%stackaddr$prim47685 = alloca %struct.ScmObj*, align 8
%current_45args47041 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47040)
store volatile %struct.ScmObj* %current_45args47041, %struct.ScmObj** %stackaddr$prim47685, align 8
%stackaddr$prim47686 = alloca %struct.ScmObj*, align 8
%anf_45bind40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47041)
store volatile %struct.ScmObj* %anf_45bind40226, %struct.ScmObj** %stackaddr$prim47686, align 8
%stackaddr$makeclosure47687 = alloca %struct.ScmObj*, align 8
%fptrToInt47688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40637 to i64
%ae40637 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47688)
store volatile %struct.ScmObj* %ae40637, %struct.ScmObj** %stackaddr$makeclosure47687, align 8
%args47635$anf_45bind40222$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%args47635$anf_45bind40222$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40226, %struct.ScmObj* %args47635$anf_45bind40222$0)
store volatile %struct.ScmObj* %args47635$anf_45bind40222$1, %struct.ScmObj** %stackaddr$prim47689, align 8
%stackaddr$prim47690 = alloca %struct.ScmObj*, align 8
%args47635$anf_45bind40222$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40637, %struct.ScmObj* %args47635$anf_45bind40222$1)
store volatile %struct.ScmObj* %args47635$anf_45bind40222$2, %struct.ScmObj** %stackaddr$prim47690, align 8
%clofunc47691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40222)
musttail call tailcc void %clofunc47691(%struct.ScmObj* %anf_45bind40222, %struct.ScmObj* %args47635$anf_45bind40222$2)
ret void
}

define tailcc void @proc_clo$ae40637(%struct.ScmObj* %env$ae40637,%struct.ScmObj* %current_45args47043) {
%stackaddr$prim47692 = alloca %struct.ScmObj*, align 8
%_95k40341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47043)
store volatile %struct.ScmObj* %_95k40341, %struct.ScmObj** %stackaddr$prim47692, align 8
%stackaddr$prim47693 = alloca %struct.ScmObj*, align 8
%current_45args47044 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47043)
store volatile %struct.ScmObj* %current_45args47044, %struct.ScmObj** %stackaddr$prim47693, align 8
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47044)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$makeclosure47695 = alloca %struct.ScmObj*, align 8
%fptrToInt47696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40639 to i64
%ae40639 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47696)
store volatile %struct.ScmObj* %ae40639, %struct.ScmObj** %stackaddr$makeclosure47695, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40639, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40640 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47697 = alloca %struct.ScmObj*, align 8
%fptrToInt47698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40641 to i64
%ae40641 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47698)
store volatile %struct.ScmObj* %ae40641, %struct.ScmObj** %stackaddr$makeclosure47697, align 8
%args47634$ae40639$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47699 = alloca %struct.ScmObj*, align 8
%args47634$ae40639$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40641, %struct.ScmObj* %args47634$ae40639$0)
store volatile %struct.ScmObj* %args47634$ae40639$1, %struct.ScmObj** %stackaddr$prim47699, align 8
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%args47634$ae40639$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40640, %struct.ScmObj* %args47634$ae40639$1)
store volatile %struct.ScmObj* %args47634$ae40639$2, %struct.ScmObj** %stackaddr$prim47700, align 8
%clofunc47701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40639)
musttail call tailcc void %clofunc47701(%struct.ScmObj* %ae40639, %struct.ScmObj* %args47634$ae40639$2)
ret void
}

define tailcc void @proc_clo$ae40639(%struct.ScmObj* %env$ae40639,%struct.ScmObj* %current_45args47046) {
%stackaddr$env-ref47702 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40639, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47702
%stackaddr$prim47703 = alloca %struct.ScmObj*, align 8
%_95k40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47046)
store volatile %struct.ScmObj* %_95k40342, %struct.ScmObj** %stackaddr$prim47703, align 8
%stackaddr$prim47704 = alloca %struct.ScmObj*, align 8
%current_45args47047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47046)
store volatile %struct.ScmObj* %current_45args47047, %struct.ScmObj** %stackaddr$prim47704, align 8
%stackaddr$prim47705 = alloca %struct.ScmObj*, align 8
%anf_45bind40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47047)
store volatile %struct.ScmObj* %anf_45bind40231, %struct.ScmObj** %stackaddr$prim47705, align 8
%stackaddr$makeclosure47706 = alloca %struct.ScmObj*, align 8
%fptrToInt47707 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40717 to i64
%ae40717 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47707)
store volatile %struct.ScmObj* %ae40717, %struct.ScmObj** %stackaddr$makeclosure47706, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40717, %struct.ScmObj* %Ycmb40102, i64 0)
%args47618$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47708 = alloca %struct.ScmObj*, align 8
%args47618$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40231, %struct.ScmObj* %args47618$Ycmb40102$0)
store volatile %struct.ScmObj* %args47618$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47708, align 8
%stackaddr$prim47709 = alloca %struct.ScmObj*, align 8
%args47618$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40717, %struct.ScmObj* %args47618$Ycmb40102$1)
store volatile %struct.ScmObj* %args47618$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47709, align 8
%clofunc47710 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47710(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47618$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40717(%struct.ScmObj* %env$ae40717,%struct.ScmObj* %current_45args47049) {
%stackaddr$env-ref47711 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40717, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47711
%stackaddr$prim47712 = alloca %struct.ScmObj*, align 8
%_95k40343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47049)
store volatile %struct.ScmObj* %_95k40343, %struct.ScmObj** %stackaddr$prim47712, align 8
%stackaddr$prim47713 = alloca %struct.ScmObj*, align 8
%current_45args47050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47049)
store volatile %struct.ScmObj* %current_45args47050, %struct.ScmObj** %stackaddr$prim47713, align 8
%stackaddr$prim47714 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47050)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47714, align 8
%stackaddr$makeclosure47715 = alloca %struct.ScmObj*, align 8
%fptrToInt47716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40719 to i64
%ae40719 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47716)
store volatile %struct.ScmObj* %ae40719, %struct.ScmObj** %stackaddr$makeclosure47715, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40719, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40719, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47717 = alloca %struct.ScmObj*, align 8
%fptrToInt47718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40721 to i64
%ae40721 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47718)
store volatile %struct.ScmObj* %ae40721, %struct.ScmObj** %stackaddr$makeclosure47717, align 8
%args47617$ae40719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%args47617$ae40719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40721, %struct.ScmObj* %args47617$ae40719$0)
store volatile %struct.ScmObj* %args47617$ae40719$1, %struct.ScmObj** %stackaddr$prim47719, align 8
%stackaddr$prim47720 = alloca %struct.ScmObj*, align 8
%args47617$ae40719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40720, %struct.ScmObj* %args47617$ae40719$1)
store volatile %struct.ScmObj* %args47617$ae40719$2, %struct.ScmObj** %stackaddr$prim47720, align 8
%clofunc47721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40719)
musttail call tailcc void %clofunc47721(%struct.ScmObj* %ae40719, %struct.ScmObj* %args47617$ae40719$2)
ret void
}

define tailcc void @proc_clo$ae40719(%struct.ScmObj* %env$ae40719,%struct.ScmObj* %current_45args47052) {
%stackaddr$env-ref47722 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40719, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47722
%stackaddr$env-ref47723 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40719, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47723
%stackaddr$prim47724 = alloca %struct.ScmObj*, align 8
%_95k40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47052)
store volatile %struct.ScmObj* %_95k40344, %struct.ScmObj** %stackaddr$prim47724, align 8
%stackaddr$prim47725 = alloca %struct.ScmObj*, align 8
%current_45args47053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47052)
store volatile %struct.ScmObj* %current_45args47053, %struct.ScmObj** %stackaddr$prim47725, align 8
%stackaddr$prim47726 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47053)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim47726, align 8
%stackaddr$makeclosure47727 = alloca %struct.ScmObj*, align 8
%fptrToInt47728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40814 to i64
%ae40814 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47728)
store volatile %struct.ScmObj* %ae40814, %struct.ScmObj** %stackaddr$makeclosure47727, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40814, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40814, %struct.ScmObj* %_37foldr140123, i64 1)
%args47598$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47729 = alloca %struct.ScmObj*, align 8
%args47598$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40237, %struct.ScmObj* %args47598$Ycmb40102$0)
store volatile %struct.ScmObj* %args47598$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47729, align 8
%stackaddr$prim47730 = alloca %struct.ScmObj*, align 8
%args47598$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40814, %struct.ScmObj* %args47598$Ycmb40102$1)
store volatile %struct.ScmObj* %args47598$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47730, align 8
%clofunc47731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47731(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47598$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40814(%struct.ScmObj* %env$ae40814,%struct.ScmObj* %current_45args47055) {
%stackaddr$env-ref47732 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40814, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47732
%stackaddr$env-ref47733 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40814, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47733
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%_95k40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47055)
store volatile %struct.ScmObj* %_95k40345, %struct.ScmObj** %stackaddr$prim47734, align 8
%stackaddr$prim47735 = alloca %struct.ScmObj*, align 8
%current_45args47056 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47055)
store volatile %struct.ScmObj* %current_45args47056, %struct.ScmObj** %stackaddr$prim47735, align 8
%stackaddr$prim47736 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47056)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47736, align 8
%stackaddr$makeclosure47737 = alloca %struct.ScmObj*, align 8
%fptrToInt47738 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40816 to i64
%ae40816 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47738)
store volatile %struct.ScmObj* %ae40816, %struct.ScmObj** %stackaddr$makeclosure47737, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40816, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40816, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40816, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47739 = alloca %struct.ScmObj*, align 8
%fptrToInt47740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40818 to i64
%ae40818 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47740)
store volatile %struct.ScmObj* %ae40818, %struct.ScmObj** %stackaddr$makeclosure47739, align 8
%args47597$ae40816$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47741 = alloca %struct.ScmObj*, align 8
%args47597$ae40816$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40818, %struct.ScmObj* %args47597$ae40816$0)
store volatile %struct.ScmObj* %args47597$ae40816$1, %struct.ScmObj** %stackaddr$prim47741, align 8
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%args47597$ae40816$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40817, %struct.ScmObj* %args47597$ae40816$1)
store volatile %struct.ScmObj* %args47597$ae40816$2, %struct.ScmObj** %stackaddr$prim47742, align 8
%clofunc47743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40816)
musttail call tailcc void %clofunc47743(%struct.ScmObj* %ae40816, %struct.ScmObj* %args47597$ae40816$2)
ret void
}

define tailcc void @proc_clo$ae40816(%struct.ScmObj* %env$ae40816,%struct.ScmObj* %current_45args47058) {
%stackaddr$env-ref47744 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40816, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47744
%stackaddr$env-ref47745 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40816, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47745
%stackaddr$env-ref47746 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40816, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47746
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%_95k40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47058)
store volatile %struct.ScmObj* %_95k40346, %struct.ScmObj** %stackaddr$prim47747, align 8
%stackaddr$prim47748 = alloca %struct.ScmObj*, align 8
%current_45args47059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47058)
store volatile %struct.ScmObj* %current_45args47059, %struct.ScmObj** %stackaddr$prim47748, align 8
%stackaddr$prim47749 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47059)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim47749, align 8
%stackaddr$makeclosure47750 = alloca %struct.ScmObj*, align 8
%fptrToInt47751 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40964 to i64
%ae40964 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47751)
store volatile %struct.ScmObj* %ae40964, %struct.ScmObj** %stackaddr$makeclosure47750, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40964, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40964, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40964, %struct.ScmObj* %_37foldr140123, i64 2)
%args47581$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47752 = alloca %struct.ScmObj*, align 8
%args47581$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %args47581$Ycmb40102$0)
store volatile %struct.ScmObj* %args47581$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47752, align 8
%stackaddr$prim47753 = alloca %struct.ScmObj*, align 8
%args47581$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40964, %struct.ScmObj* %args47581$Ycmb40102$1)
store volatile %struct.ScmObj* %args47581$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47753, align 8
%clofunc47754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47754(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47581$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40964(%struct.ScmObj* %env$ae40964,%struct.ScmObj* %current_45args47061) {
%stackaddr$env-ref47755 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40964, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47755
%stackaddr$env-ref47756 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40964, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47756
%stackaddr$env-ref47757 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40964, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47757
%stackaddr$prim47758 = alloca %struct.ScmObj*, align 8
%_95k40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47061)
store volatile %struct.ScmObj* %_95k40347, %struct.ScmObj** %stackaddr$prim47758, align 8
%stackaddr$prim47759 = alloca %struct.ScmObj*, align 8
%current_45args47062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47061)
store volatile %struct.ScmObj* %current_45args47062, %struct.ScmObj** %stackaddr$prim47759, align 8
%stackaddr$prim47760 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47062)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47760, align 8
%stackaddr$makeclosure47761 = alloca %struct.ScmObj*, align 8
%fptrToInt47762 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40966 to i64
%ae40966 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47762)
store volatile %struct.ScmObj* %ae40966, %struct.ScmObj** %stackaddr$makeclosure47761, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40966, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40966, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40966, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40966, %struct.ScmObj* %_37foldr140123, i64 3)
%ae40967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47763 = alloca %struct.ScmObj*, align 8
%fptrToInt47764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40968 to i64
%ae40968 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47764)
store volatile %struct.ScmObj* %ae40968, %struct.ScmObj** %stackaddr$makeclosure47763, align 8
%args47580$ae40966$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47765 = alloca %struct.ScmObj*, align 8
%args47580$ae40966$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40968, %struct.ScmObj* %args47580$ae40966$0)
store volatile %struct.ScmObj* %args47580$ae40966$1, %struct.ScmObj** %stackaddr$prim47765, align 8
%stackaddr$prim47766 = alloca %struct.ScmObj*, align 8
%args47580$ae40966$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40967, %struct.ScmObj* %args47580$ae40966$1)
store volatile %struct.ScmObj* %args47580$ae40966$2, %struct.ScmObj** %stackaddr$prim47766, align 8
%clofunc47767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40966)
musttail call tailcc void %clofunc47767(%struct.ScmObj* %ae40966, %struct.ScmObj* %args47580$ae40966$2)
ret void
}

define tailcc void @proc_clo$ae40966(%struct.ScmObj* %env$ae40966,%struct.ScmObj* %current_45args47064) {
%stackaddr$env-ref47768 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40966, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47768
%stackaddr$env-ref47769 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40966, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47769
%stackaddr$env-ref47770 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40966, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47770
%stackaddr$env-ref47771 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40966, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47771
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%_95k40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47064)
store volatile %struct.ScmObj* %_95k40348, %struct.ScmObj** %stackaddr$prim47772, align 8
%stackaddr$prim47773 = alloca %struct.ScmObj*, align 8
%current_45args47065 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47064)
store volatile %struct.ScmObj* %current_45args47065, %struct.ScmObj** %stackaddr$prim47773, align 8
%stackaddr$prim47774 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47065)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim47774, align 8
%stackaddr$makeclosure47775 = alloca %struct.ScmObj*, align 8
%fptrToInt47776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41047 to i64
%ae41047 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47776)
store volatile %struct.ScmObj* %ae41047, %struct.ScmObj** %stackaddr$makeclosure47775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41047, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41047, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41047, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41047, %struct.ScmObj* %_37foldr140123, i64 3)
%args47566$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%args47566$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %args47566$Ycmb40102$0)
store volatile %struct.ScmObj* %args47566$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47777, align 8
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%args47566$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41047, %struct.ScmObj* %args47566$Ycmb40102$1)
store volatile %struct.ScmObj* %args47566$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47778, align 8
%clofunc47779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47779(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47566$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41047(%struct.ScmObj* %env$ae41047,%struct.ScmObj* %current_45args47067) {
%stackaddr$env-ref47780 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41047, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47780
%stackaddr$env-ref47781 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41047, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47781
%stackaddr$env-ref47782 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41047, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47782
%stackaddr$env-ref47783 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41047, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47783
%stackaddr$prim47784 = alloca %struct.ScmObj*, align 8
%_95k40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47067)
store volatile %struct.ScmObj* %_95k40349, %struct.ScmObj** %stackaddr$prim47784, align 8
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%current_45args47068 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47067)
store volatile %struct.ScmObj* %current_45args47068, %struct.ScmObj** %stackaddr$prim47785, align 8
%stackaddr$prim47786 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47068)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47786, align 8
%stackaddr$makeclosure47787 = alloca %struct.ScmObj*, align 8
%fptrToInt47788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41049 to i64
%ae41049 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47788)
store volatile %struct.ScmObj* %ae41049, %struct.ScmObj** %stackaddr$makeclosure47787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41050 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47789 = alloca %struct.ScmObj*, align 8
%fptrToInt47790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41051 to i64
%ae41051 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47790)
store volatile %struct.ScmObj* %ae41051, %struct.ScmObj** %stackaddr$makeclosure47789, align 8
%args47565$ae41049$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47791 = alloca %struct.ScmObj*, align 8
%args47565$ae41049$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41051, %struct.ScmObj* %args47565$ae41049$0)
store volatile %struct.ScmObj* %args47565$ae41049$1, %struct.ScmObj** %stackaddr$prim47791, align 8
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%args47565$ae41049$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41050, %struct.ScmObj* %args47565$ae41049$1)
store volatile %struct.ScmObj* %args47565$ae41049$2, %struct.ScmObj** %stackaddr$prim47792, align 8
%clofunc47793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41049)
musttail call tailcc void %clofunc47793(%struct.ScmObj* %ae41049, %struct.ScmObj* %args47565$ae41049$2)
ret void
}

define tailcc void @proc_clo$ae41049(%struct.ScmObj* %env$ae41049,%struct.ScmObj* %current_45args47070) {
%stackaddr$env-ref47794 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47794
%stackaddr$env-ref47795 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47795
%stackaddr$env-ref47796 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47796
%stackaddr$env-ref47797 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47797
%stackaddr$env-ref47798 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47798
%stackaddr$prim47799 = alloca %struct.ScmObj*, align 8
%_95k40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47070)
store volatile %struct.ScmObj* %_95k40350, %struct.ScmObj** %stackaddr$prim47799, align 8
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%current_45args47071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47070)
store volatile %struct.ScmObj* %current_45args47071, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47071)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$makeclosure47802 = alloca %struct.ScmObj*, align 8
%fptrToInt47803 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41126 to i64
%ae41126 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47803)
store volatile %struct.ScmObj* %ae41126, %struct.ScmObj** %stackaddr$makeclosure47802, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41126, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41126, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41126, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41126, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41126, %struct.ScmObj* %_37foldr140123, i64 4)
%args47549$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47804 = alloca %struct.ScmObj*, align 8
%args47549$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args47549$Ycmb40102$0)
store volatile %struct.ScmObj* %args47549$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47804, align 8
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%args47549$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41126, %struct.ScmObj* %args47549$Ycmb40102$1)
store volatile %struct.ScmObj* %args47549$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47805, align 8
%clofunc47806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47806(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47549$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41126(%struct.ScmObj* %env$ae41126,%struct.ScmObj* %current_45args47073) {
%stackaddr$env-ref47807 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41126, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47807
%stackaddr$env-ref47808 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41126, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47808
%stackaddr$env-ref47809 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41126, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47809
%stackaddr$env-ref47810 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41126, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47810
%stackaddr$env-ref47811 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41126, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47811
%stackaddr$prim47812 = alloca %struct.ScmObj*, align 8
%_95k40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47073)
store volatile %struct.ScmObj* %_95k40351, %struct.ScmObj** %stackaddr$prim47812, align 8
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%current_45args47074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47073)
store volatile %struct.ScmObj* %current_45args47074, %struct.ScmObj** %stackaddr$prim47813, align 8
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47074)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47814, align 8
%stackaddr$makeclosure47815 = alloca %struct.ScmObj*, align 8
%fptrToInt47816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41128 to i64
%ae41128 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47816)
store volatile %struct.ScmObj* %ae41128, %struct.ScmObj** %stackaddr$makeclosure47815, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41128, %struct.ScmObj* %_37length40112, i64 5)
%ae41129 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47817 = alloca %struct.ScmObj*, align 8
%fptrToInt47818 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41130 to i64
%ae41130 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47818)
store volatile %struct.ScmObj* %ae41130, %struct.ScmObj** %stackaddr$makeclosure47817, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41130, %struct.ScmObj* %_37foldl140107, i64 0)
%args47548$ae41128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47819 = alloca %struct.ScmObj*, align 8
%args47548$ae41128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41130, %struct.ScmObj* %args47548$ae41128$0)
store volatile %struct.ScmObj* %args47548$ae41128$1, %struct.ScmObj** %stackaddr$prim47819, align 8
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%args47548$ae41128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41129, %struct.ScmObj* %args47548$ae41128$1)
store volatile %struct.ScmObj* %args47548$ae41128$2, %struct.ScmObj** %stackaddr$prim47820, align 8
%clofunc47821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41128)
musttail call tailcc void %clofunc47821(%struct.ScmObj* %ae41128, %struct.ScmObj* %args47548$ae41128$2)
ret void
}

define tailcc void @proc_clo$ae41128(%struct.ScmObj* %env$ae41128,%struct.ScmObj* %current_45args47076) {
%stackaddr$env-ref47822 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47822
%stackaddr$env-ref47823 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47823
%stackaddr$env-ref47824 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47824
%stackaddr$env-ref47825 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47825
%stackaddr$env-ref47826 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47826
%stackaddr$env-ref47827 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41128, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47827
%stackaddr$prim47828 = alloca %struct.ScmObj*, align 8
%_95k40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %_95k40352, %struct.ScmObj** %stackaddr$prim47828, align 8
%stackaddr$prim47829 = alloca %struct.ScmObj*, align 8
%current_45args47077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %current_45args47077, %struct.ScmObj** %stackaddr$prim47829, align 8
%stackaddr$prim47830 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47077)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47830, align 8
%stackaddr$makeclosure47831 = alloca %struct.ScmObj*, align 8
%fptrToInt47832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41182 to i64
%ae41182 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47832)
store volatile %struct.ScmObj* %ae41182, %struct.ScmObj** %stackaddr$makeclosure47831, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41182, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41182, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41182, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41182, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41182, %struct.ScmObj* %_37last40145, i64 4)
%ae41183 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47833 = alloca %struct.ScmObj*, align 8
%fptrToInt47834 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41184 to i64
%ae41184 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47834)
store volatile %struct.ScmObj* %ae41184, %struct.ScmObj** %stackaddr$makeclosure47833, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41184, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41184, %struct.ScmObj* %_37length40112, i64 1)
%args47534$ae41182$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47835 = alloca %struct.ScmObj*, align 8
%args47534$ae41182$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41184, %struct.ScmObj* %args47534$ae41182$0)
store volatile %struct.ScmObj* %args47534$ae41182$1, %struct.ScmObj** %stackaddr$prim47835, align 8
%stackaddr$prim47836 = alloca %struct.ScmObj*, align 8
%args47534$ae41182$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41183, %struct.ScmObj* %args47534$ae41182$1)
store volatile %struct.ScmObj* %args47534$ae41182$2, %struct.ScmObj** %stackaddr$prim47836, align 8
%clofunc47837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41182)
musttail call tailcc void %clofunc47837(%struct.ScmObj* %ae41182, %struct.ScmObj* %args47534$ae41182$2)
ret void
}

define tailcc void @proc_clo$ae41182(%struct.ScmObj* %env$ae41182,%struct.ScmObj* %current_45args47079) {
%stackaddr$env-ref47838 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41182, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47838
%stackaddr$env-ref47839 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41182, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47839
%stackaddr$env-ref47840 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41182, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47840
%stackaddr$env-ref47841 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41182, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47841
%stackaddr$env-ref47842 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41182, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47842
%stackaddr$prim47843 = alloca %struct.ScmObj*, align 8
%_95k40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47079)
store volatile %struct.ScmObj* %_95k40353, %struct.ScmObj** %stackaddr$prim47843, align 8
%stackaddr$prim47844 = alloca %struct.ScmObj*, align 8
%current_45args47080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47079)
store volatile %struct.ScmObj* %current_45args47080, %struct.ScmObj** %stackaddr$prim47844, align 8
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47080)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47845, align 8
%stackaddr$makeclosure47846 = alloca %struct.ScmObj*, align 8
%fptrToInt47847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41212 to i64
%ae41212 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47847)
store volatile %struct.ScmObj* %ae41212, %struct.ScmObj** %stackaddr$makeclosure47846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41212, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41212, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41212, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41212, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41212, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41213 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47848 = alloca %struct.ScmObj*, align 8
%fptrToInt47849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41214 to i64
%ae41214 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47849)
store volatile %struct.ScmObj* %ae41214, %struct.ScmObj** %stackaddr$makeclosure47848, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41214, %struct.ScmObj* %_37foldr140123, i64 1)
%args47524$ae41212$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47850 = alloca %struct.ScmObj*, align 8
%args47524$ae41212$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41214, %struct.ScmObj* %args47524$ae41212$0)
store volatile %struct.ScmObj* %args47524$ae41212$1, %struct.ScmObj** %stackaddr$prim47850, align 8
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%args47524$ae41212$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41213, %struct.ScmObj* %args47524$ae41212$1)
store volatile %struct.ScmObj* %args47524$ae41212$2, %struct.ScmObj** %stackaddr$prim47851, align 8
%clofunc47852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41212)
musttail call tailcc void %clofunc47852(%struct.ScmObj* %ae41212, %struct.ScmObj* %args47524$ae41212$2)
ret void
}

define tailcc void @proc_clo$ae41212(%struct.ScmObj* %env$ae41212,%struct.ScmObj* %current_45args47082) {
%stackaddr$env-ref47853 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41212, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47853
%stackaddr$env-ref47854 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41212, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47854
%stackaddr$env-ref47855 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41212, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47855
%stackaddr$env-ref47856 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41212, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47856
%stackaddr$env-ref47857 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41212, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47857
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%_95k40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47082)
store volatile %struct.ScmObj* %_95k40354, %struct.ScmObj** %stackaddr$prim47858, align 8
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%current_45args47083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47082)
store volatile %struct.ScmObj* %current_45args47083, %struct.ScmObj** %stackaddr$prim47859, align 8
%stackaddr$prim47860 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47083)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim47860, align 8
%stackaddr$makeclosure47861 = alloca %struct.ScmObj*, align 8
%fptrToInt47862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41596 to i64
%ae41596 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47862)
store volatile %struct.ScmObj* %ae41596, %struct.ScmObj** %stackaddr$makeclosure47861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41596, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41596, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41596, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41596, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41596, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args47464$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47863 = alloca %struct.ScmObj*, align 8
%args47464$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args47464$Ycmb40102$0)
store volatile %struct.ScmObj* %args47464$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47863, align 8
%stackaddr$prim47864 = alloca %struct.ScmObj*, align 8
%args47464$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41596, %struct.ScmObj* %args47464$Ycmb40102$1)
store volatile %struct.ScmObj* %args47464$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47864, align 8
%clofunc47865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47865(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47464$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41596(%struct.ScmObj* %env$ae41596,%struct.ScmObj* %current_45args47085) {
%stackaddr$env-ref47866 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41596, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47866
%stackaddr$env-ref47867 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41596, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47867
%stackaddr$env-ref47868 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41596, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47868
%stackaddr$env-ref47869 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41596, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47869
%stackaddr$env-ref47870 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41596, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47870
%stackaddr$prim47871 = alloca %struct.ScmObj*, align 8
%_95k40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47085)
store volatile %struct.ScmObj* %_95k40355, %struct.ScmObj** %stackaddr$prim47871, align 8
%stackaddr$prim47872 = alloca %struct.ScmObj*, align 8
%current_45args47086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47085)
store volatile %struct.ScmObj* %current_45args47086, %struct.ScmObj** %stackaddr$prim47872, align 8
%stackaddr$prim47873 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47086)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47873, align 8
%stackaddr$makeclosure47874 = alloca %struct.ScmObj*, align 8
%fptrToInt47875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41598 to i64
%ae41598 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47875)
store volatile %struct.ScmObj* %ae41598, %struct.ScmObj** %stackaddr$makeclosure47874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41598, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41599 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47876 = alloca %struct.ScmObj*, align 8
%fptrToInt47877 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41600 to i64
%ae41600 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47877)
store volatile %struct.ScmObj* %ae41600, %struct.ScmObj** %stackaddr$makeclosure47876, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41600, %struct.ScmObj* %_37foldr140123, i64 0)
%args47463$ae41598$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47878 = alloca %struct.ScmObj*, align 8
%args47463$ae41598$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41600, %struct.ScmObj* %args47463$ae41598$0)
store volatile %struct.ScmObj* %args47463$ae41598$1, %struct.ScmObj** %stackaddr$prim47878, align 8
%stackaddr$prim47879 = alloca %struct.ScmObj*, align 8
%args47463$ae41598$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41599, %struct.ScmObj* %args47463$ae41598$1)
store volatile %struct.ScmObj* %args47463$ae41598$2, %struct.ScmObj** %stackaddr$prim47879, align 8
%clofunc47880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41598)
musttail call tailcc void %clofunc47880(%struct.ScmObj* %ae41598, %struct.ScmObj* %args47463$ae41598$2)
ret void
}

define tailcc void @proc_clo$ae41598(%struct.ScmObj* %env$ae41598,%struct.ScmObj* %current_45args47088) {
%stackaddr$env-ref47881 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47881
%stackaddr$env-ref47882 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47882
%stackaddr$env-ref47883 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47883
%stackaddr$env-ref47884 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47884
%stackaddr$env-ref47885 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47885
%stackaddr$env-ref47886 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41598, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47886
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%_95k40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %_95k40356, %struct.ScmObj** %stackaddr$prim47887, align 8
%stackaddr$prim47888 = alloca %struct.ScmObj*, align 8
%current_45args47089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %current_45args47089, %struct.ScmObj** %stackaddr$prim47888, align 8
%stackaddr$prim47889 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47089)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47889, align 8
%stackaddr$makeclosure47890 = alloca %struct.ScmObj*, align 8
%fptrToInt47891 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41675 to i64
%ae41675 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47891)
store volatile %struct.ScmObj* %ae41675, %struct.ScmObj** %stackaddr$makeclosure47890, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41675, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41675, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41675, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41675, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41675, %struct.ScmObj* %_37map140154, i64 4)
%ae41676 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47892 = alloca %struct.ScmObj*, align 8
%fptrToInt47893 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41677 to i64
%ae41677 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47893)
store volatile %struct.ScmObj* %ae41677, %struct.ScmObj** %stackaddr$makeclosure47892, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41677, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41677, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41677, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args47444$ae41675$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%args47444$ae41675$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41677, %struct.ScmObj* %args47444$ae41675$0)
store volatile %struct.ScmObj* %args47444$ae41675$1, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$prim47895 = alloca %struct.ScmObj*, align 8
%args47444$ae41675$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41676, %struct.ScmObj* %args47444$ae41675$1)
store volatile %struct.ScmObj* %args47444$ae41675$2, %struct.ScmObj** %stackaddr$prim47895, align 8
%clofunc47896 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41675)
musttail call tailcc void %clofunc47896(%struct.ScmObj* %ae41675, %struct.ScmObj* %args47444$ae41675$2)
ret void
}

define tailcc void @proc_clo$ae41675(%struct.ScmObj* %env$ae41675,%struct.ScmObj* %current_45args47091) {
%stackaddr$env-ref47897 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41675, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47897
%stackaddr$env-ref47898 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41675, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47898
%stackaddr$env-ref47899 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41675, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47899
%stackaddr$env-ref47900 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41675, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47900
%stackaddr$env-ref47901 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41675, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47901
%stackaddr$prim47902 = alloca %struct.ScmObj*, align 8
%_95k40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47091)
store volatile %struct.ScmObj* %_95k40357, %struct.ScmObj** %stackaddr$prim47902, align 8
%stackaddr$prim47903 = alloca %struct.ScmObj*, align 8
%current_45args47092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47091)
store volatile %struct.ScmObj* %current_45args47092, %struct.ScmObj** %stackaddr$prim47903, align 8
%stackaddr$prim47904 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47092)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47904, align 8
%stackaddr$makeclosure47905 = alloca %struct.ScmObj*, align 8
%fptrToInt47906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41821 to i64
%ae41821 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47906)
store volatile %struct.ScmObj* %ae41821, %struct.ScmObj** %stackaddr$makeclosure47905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41821, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41821, %struct.ScmObj* %_37foldl140107, i64 1)
%ae41822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47907 = alloca %struct.ScmObj*, align 8
%fptrToInt47908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41823 to i64
%ae41823 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47908)
store volatile %struct.ScmObj* %ae41823, %struct.ScmObj** %stackaddr$makeclosure47907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41823, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41823, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41823, %struct.ScmObj* %_37map140154, i64 2)
%args47427$ae41821$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%args47427$ae41821$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41823, %struct.ScmObj* %args47427$ae41821$0)
store volatile %struct.ScmObj* %args47427$ae41821$1, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$prim47910 = alloca %struct.ScmObj*, align 8
%args47427$ae41821$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41822, %struct.ScmObj* %args47427$ae41821$1)
store volatile %struct.ScmObj* %args47427$ae41821$2, %struct.ScmObj** %stackaddr$prim47910, align 8
%clofunc47911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41821)
musttail call tailcc void %clofunc47911(%struct.ScmObj* %ae41821, %struct.ScmObj* %args47427$ae41821$2)
ret void
}

define tailcc void @proc_clo$ae41821(%struct.ScmObj* %env$ae41821,%struct.ScmObj* %current_45args47094) {
%stackaddr$env-ref47912 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41821, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47912
%stackaddr$env-ref47913 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41821, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47913
%stackaddr$prim47914 = alloca %struct.ScmObj*, align 8
%_95k40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47094)
store volatile %struct.ScmObj* %_95k40358, %struct.ScmObj** %stackaddr$prim47914, align 8
%stackaddr$prim47915 = alloca %struct.ScmObj*, align 8
%current_45args47095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47094)
store volatile %struct.ScmObj* %current_45args47095, %struct.ScmObj** %stackaddr$prim47915, align 8
%stackaddr$prim47916 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47095)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim47916, align 8
%stackaddr$makeclosure47917 = alloca %struct.ScmObj*, align 8
%fptrToInt47918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42213 to i64
%ae42213 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47918)
store volatile %struct.ScmObj* %ae42213, %struct.ScmObj** %stackaddr$makeclosure47917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42213, %struct.ScmObj* %_37foldl140107, i64 0)
%args47367$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47919 = alloca %struct.ScmObj*, align 8
%args47367$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args47367$Ycmb40102$0)
store volatile %struct.ScmObj* %args47367$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47919, align 8
%stackaddr$prim47920 = alloca %struct.ScmObj*, align 8
%args47367$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42213, %struct.ScmObj* %args47367$Ycmb40102$1)
store volatile %struct.ScmObj* %args47367$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47920, align 8
%clofunc47921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47921(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47367$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42213(%struct.ScmObj* %env$ae42213,%struct.ScmObj* %current_45args47097) {
%stackaddr$env-ref47922 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42213, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47922
%stackaddr$prim47923 = alloca %struct.ScmObj*, align 8
%_95k40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47097)
store volatile %struct.ScmObj* %_95k40359, %struct.ScmObj** %stackaddr$prim47923, align 8
%stackaddr$prim47924 = alloca %struct.ScmObj*, align 8
%current_45args47098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47097)
store volatile %struct.ScmObj* %current_45args47098, %struct.ScmObj** %stackaddr$prim47924, align 8
%stackaddr$prim47925 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47098)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47925, align 8
%stackaddr$makeclosure47926 = alloca %struct.ScmObj*, align 8
%fptrToInt47927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42215 to i64
%ae42215 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47927)
store volatile %struct.ScmObj* %ae42215, %struct.ScmObj** %stackaddr$makeclosure47926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42215, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42216 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47928 = alloca %struct.ScmObj*, align 8
%fptrToInt47929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42217 to i64
%ae42217 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47929)
store volatile %struct.ScmObj* %ae42217, %struct.ScmObj** %stackaddr$makeclosure47928, align 8
%args47366$ae42215$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47930 = alloca %struct.ScmObj*, align 8
%args47366$ae42215$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42217, %struct.ScmObj* %args47366$ae42215$0)
store volatile %struct.ScmObj* %args47366$ae42215$1, %struct.ScmObj** %stackaddr$prim47930, align 8
%stackaddr$prim47931 = alloca %struct.ScmObj*, align 8
%args47366$ae42215$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42216, %struct.ScmObj* %args47366$ae42215$1)
store volatile %struct.ScmObj* %args47366$ae42215$2, %struct.ScmObj** %stackaddr$prim47931, align 8
%clofunc47932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42215)
musttail call tailcc void %clofunc47932(%struct.ScmObj* %ae42215, %struct.ScmObj* %args47366$ae42215$2)
ret void
}

define tailcc void @proc_clo$ae42215(%struct.ScmObj* %env$ae42215,%struct.ScmObj* %current_45args47100) {
%stackaddr$env-ref47933 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42215, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47933
%stackaddr$prim47934 = alloca %struct.ScmObj*, align 8
%_95k40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47100)
store volatile %struct.ScmObj* %_95k40360, %struct.ScmObj** %stackaddr$prim47934, align 8
%stackaddr$prim47935 = alloca %struct.ScmObj*, align 8
%current_45args47101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47100)
store volatile %struct.ScmObj* %current_45args47101, %struct.ScmObj** %stackaddr$prim47935, align 8
%stackaddr$prim47936 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47101)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47936, align 8
%stackaddr$makeclosure47937 = alloca %struct.ScmObj*, align 8
%fptrToInt47938 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42239 to i64
%ae42239 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47938)
store volatile %struct.ScmObj* %ae42239, %struct.ScmObj** %stackaddr$makeclosure47937, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42239, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47939 = alloca %struct.ScmObj*, align 8
%fptrToInt47940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42241 to i64
%ae42241 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47940)
store volatile %struct.ScmObj* %ae42241, %struct.ScmObj** %stackaddr$makeclosure47939, align 8
%args47360$ae42239$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47941 = alloca %struct.ScmObj*, align 8
%args47360$ae42239$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42241, %struct.ScmObj* %args47360$ae42239$0)
store volatile %struct.ScmObj* %args47360$ae42239$1, %struct.ScmObj** %stackaddr$prim47941, align 8
%stackaddr$prim47942 = alloca %struct.ScmObj*, align 8
%args47360$ae42239$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42240, %struct.ScmObj* %args47360$ae42239$1)
store volatile %struct.ScmObj* %args47360$ae42239$2, %struct.ScmObj** %stackaddr$prim47942, align 8
%clofunc47943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42239)
musttail call tailcc void %clofunc47943(%struct.ScmObj* %ae42239, %struct.ScmObj* %args47360$ae42239$2)
ret void
}

define tailcc void @proc_clo$ae42239(%struct.ScmObj* %env$ae42239,%struct.ScmObj* %current_45args47103) {
%stackaddr$env-ref47944 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42239, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47944
%stackaddr$prim47945 = alloca %struct.ScmObj*, align 8
%_95k40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47103)
store volatile %struct.ScmObj* %_95k40361, %struct.ScmObj** %stackaddr$prim47945, align 8
%stackaddr$prim47946 = alloca %struct.ScmObj*, align 8
%current_45args47104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47103)
store volatile %struct.ScmObj* %current_45args47104, %struct.ScmObj** %stackaddr$prim47946, align 8
%stackaddr$prim47947 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47104)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47947, align 8
%ae42263 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42264 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47948 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42263, %struct.ScmObj* %ae42264)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47948, align 8
%stackaddr$makeclosure47949 = alloca %struct.ScmObj*, align 8
%fptrToInt47950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42265 to i64
%ae42265 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47950)
store volatile %struct.ScmObj* %ae42265, %struct.ScmObj** %stackaddr$makeclosure47949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42265, %struct.ScmObj* %_37append40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42265, %struct.ScmObj* %_37foldl140107, i64 1)
%ae42266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47951 = alloca %struct.ScmObj*, align 8
%fptrToInt47952 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42267 to i64
%ae42267 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47952)
store volatile %struct.ScmObj* %ae42267, %struct.ScmObj** %stackaddr$makeclosure47951, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42267, %struct.ScmObj* %_37append40195, i64 0)
%args47354$ae42265$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47953 = alloca %struct.ScmObj*, align 8
%args47354$ae42265$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42267, %struct.ScmObj* %args47354$ae42265$0)
store volatile %struct.ScmObj* %args47354$ae42265$1, %struct.ScmObj** %stackaddr$prim47953, align 8
%stackaddr$prim47954 = alloca %struct.ScmObj*, align 8
%args47354$ae42265$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42266, %struct.ScmObj* %args47354$ae42265$1)
store volatile %struct.ScmObj* %args47354$ae42265$2, %struct.ScmObj** %stackaddr$prim47954, align 8
%clofunc47955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42265)
musttail call tailcc void %clofunc47955(%struct.ScmObj* %ae42265, %struct.ScmObj* %args47354$ae42265$2)
ret void
}

define tailcc void @proc_clo$ae42265(%struct.ScmObj* %env$ae42265,%struct.ScmObj* %current_45args47106) {
%stackaddr$env-ref47956 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42265, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47956
%stackaddr$env-ref47957 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42265, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47957
%stackaddr$prim47958 = alloca %struct.ScmObj*, align 8
%_95k40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47106)
store volatile %struct.ScmObj* %_95k40362, %struct.ScmObj** %stackaddr$prim47958, align 8
%stackaddr$prim47959 = alloca %struct.ScmObj*, align 8
%current_45args47107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47106)
store volatile %struct.ScmObj* %current_45args47107, %struct.ScmObj** %stackaddr$prim47959, align 8
%stackaddr$prim47960 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47107)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim47960, align 8
%ae42333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47961 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42333, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47961, align 8
%ae42336 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47962 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42336)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47962, align 8
%stackaddr$makeclosure47963 = alloca %struct.ScmObj*, align 8
%fptrToInt47964 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42337 to i64
%ae42337 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47964)
store volatile %struct.ScmObj* %ae42337, %struct.ScmObj** %stackaddr$makeclosure47963, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42337, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47965 = alloca %struct.ScmObj*, align 8
%fptrToInt47966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42339 to i64
%ae42339 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47966)
store volatile %struct.ScmObj* %ae42339, %struct.ScmObj** %stackaddr$makeclosure47965, align 8
%args47343$ae42337$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47967 = alloca %struct.ScmObj*, align 8
%args47343$ae42337$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42339, %struct.ScmObj* %args47343$ae42337$0)
store volatile %struct.ScmObj* %args47343$ae42337$1, %struct.ScmObj** %stackaddr$prim47967, align 8
%stackaddr$prim47968 = alloca %struct.ScmObj*, align 8
%args47343$ae42337$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42338, %struct.ScmObj* %args47343$ae42337$1)
store volatile %struct.ScmObj* %args47343$ae42337$2, %struct.ScmObj** %stackaddr$prim47968, align 8
%clofunc47969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42337)
musttail call tailcc void %clofunc47969(%struct.ScmObj* %ae42337, %struct.ScmObj* %args47343$ae42337$2)
ret void
}

define tailcc void @proc_clo$ae42337(%struct.ScmObj* %env$ae42337,%struct.ScmObj* %current_45args47109) {
%stackaddr$env-ref47970 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42337, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47970
%stackaddr$prim47971 = alloca %struct.ScmObj*, align 8
%_95k40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47109)
store volatile %struct.ScmObj* %_95k40363, %struct.ScmObj** %stackaddr$prim47971, align 8
%stackaddr$prim47972 = alloca %struct.ScmObj*, align 8
%current_45args47110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47109)
store volatile %struct.ScmObj* %current_45args47110, %struct.ScmObj** %stackaddr$prim47972, align 8
%stackaddr$prim47973 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47110)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47973, align 8
%stackaddr$makeclosure47974 = alloca %struct.ScmObj*, align 8
%fptrToInt47975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42753 to i64
%ae42753 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47975)
store volatile %struct.ScmObj* %ae42753, %struct.ScmObj** %stackaddr$makeclosure47974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42753, %struct.ScmObj* %_37foldl140107, i64 0)
%ae42754 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47976 = alloca %struct.ScmObj*, align 8
%fptrToInt47977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42755 to i64
%ae42755 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47977)
store volatile %struct.ScmObj* %ae42755, %struct.ScmObj** %stackaddr$makeclosure47976, align 8
%args47318$ae42753$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47978 = alloca %struct.ScmObj*, align 8
%args47318$ae42753$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42755, %struct.ScmObj* %args47318$ae42753$0)
store volatile %struct.ScmObj* %args47318$ae42753$1, %struct.ScmObj** %stackaddr$prim47978, align 8
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%args47318$ae42753$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42754, %struct.ScmObj* %args47318$ae42753$1)
store volatile %struct.ScmObj* %args47318$ae42753$2, %struct.ScmObj** %stackaddr$prim47979, align 8
%clofunc47980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42753)
musttail call tailcc void %clofunc47980(%struct.ScmObj* %ae42753, %struct.ScmObj* %args47318$ae42753$2)
ret void
}

define tailcc void @proc_clo$ae42753(%struct.ScmObj* %env$ae42753,%struct.ScmObj* %current_45args47112) {
%stackaddr$env-ref47981 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42753, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47981
%stackaddr$prim47982 = alloca %struct.ScmObj*, align 8
%_95k40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47112)
store volatile %struct.ScmObj* %_95k40364, %struct.ScmObj** %stackaddr$prim47982, align 8
%stackaddr$prim47983 = alloca %struct.ScmObj*, align 8
%current_45args47113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47112)
store volatile %struct.ScmObj* %current_45args47113, %struct.ScmObj** %stackaddr$prim47983, align 8
%stackaddr$prim47984 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47113)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47984, align 8
%stackaddr$makeclosure47985 = alloca %struct.ScmObj*, align 8
%fptrToInt47986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43289 to i64
%ae43289 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47986)
store volatile %struct.ScmObj* %ae43289, %struct.ScmObj** %stackaddr$makeclosure47985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43289, %struct.ScmObj* %_37foldl140107, i64 0)
%ae43290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47987 = alloca %struct.ScmObj*, align 8
%fptrToInt47988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43291 to i64
%ae43291 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47988)
store volatile %struct.ScmObj* %ae43291, %struct.ScmObj** %stackaddr$makeclosure47987, align 8
%args47294$ae43289$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47989 = alloca %struct.ScmObj*, align 8
%args47294$ae43289$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43291, %struct.ScmObj* %args47294$ae43289$0)
store volatile %struct.ScmObj* %args47294$ae43289$1, %struct.ScmObj** %stackaddr$prim47989, align 8
%stackaddr$prim47990 = alloca %struct.ScmObj*, align 8
%args47294$ae43289$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43290, %struct.ScmObj* %args47294$ae43289$1)
store volatile %struct.ScmObj* %args47294$ae43289$2, %struct.ScmObj** %stackaddr$prim47990, align 8
%clofunc47991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43289)
musttail call tailcc void %clofunc47991(%struct.ScmObj* %ae43289, %struct.ScmObj* %args47294$ae43289$2)
ret void
}

define tailcc void @proc_clo$ae43289(%struct.ScmObj* %env$ae43289,%struct.ScmObj* %current_45args47115) {
%stackaddr$env-ref47992 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43289, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47992
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47115)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim47993, align 8
%stackaddr$prim47994 = alloca %struct.ScmObj*, align 8
%current_45args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47115)
store volatile %struct.ScmObj* %current_45args47116, %struct.ScmObj** %stackaddr$prim47994, align 8
%stackaddr$prim47995 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47116)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47995, align 8
%stackaddr$makeclosure47996 = alloca %struct.ScmObj*, align 8
%fptrToInt47997 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43693 to i64
%ae43693 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47997)
store volatile %struct.ScmObj* %ae43693, %struct.ScmObj** %stackaddr$makeclosure47996, align 8
%ae43694 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47998 = alloca %struct.ScmObj*, align 8
%fptrToInt47999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43695 to i64
%ae43695 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47999)
store volatile %struct.ScmObj* %ae43695, %struct.ScmObj** %stackaddr$makeclosure47998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43695, %struct.ScmObj* %_37foldl140107, i64 0)
%args47268$ae43693$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48000 = alloca %struct.ScmObj*, align 8
%args47268$ae43693$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43695, %struct.ScmObj* %args47268$ae43693$0)
store volatile %struct.ScmObj* %args47268$ae43693$1, %struct.ScmObj** %stackaddr$prim48000, align 8
%stackaddr$prim48001 = alloca %struct.ScmObj*, align 8
%args47268$ae43693$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43694, %struct.ScmObj* %args47268$ae43693$1)
store volatile %struct.ScmObj* %args47268$ae43693$2, %struct.ScmObj** %stackaddr$prim48001, align 8
%clofunc48002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43693)
musttail call tailcc void %clofunc48002(%struct.ScmObj* %ae43693, %struct.ScmObj* %args47268$ae43693$2)
ret void
}

define tailcc void @proc_clo$ae43693(%struct.ScmObj* %env$ae43693,%struct.ScmObj* %current_45args47118) {
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47118)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim48003, align 8
%stackaddr$prim48004 = alloca %struct.ScmObj*, align 8
%current_45args47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47118)
store volatile %struct.ScmObj* %current_45args47119, %struct.ScmObj** %stackaddr$prim48004, align 8
%stackaddr$prim48005 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47119)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim48005, align 8
%stackaddr$makeclosure48006 = alloca %struct.ScmObj*, align 8
%fptrToInt48007 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43791 to i64
%ae43791 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48007)
store volatile %struct.ScmObj* %ae43791, %struct.ScmObj** %stackaddr$makeclosure48006, align 8
%ae43792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48008 = alloca %struct.ScmObj*, align 8
%fptrToInt48009 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43793 to i64
%ae43793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48009)
store volatile %struct.ScmObj* %ae43793, %struct.ScmObj** %stackaddr$makeclosure48008, align 8
%args47255$ae43791$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48010 = alloca %struct.ScmObj*, align 8
%args47255$ae43791$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43793, %struct.ScmObj* %args47255$ae43791$0)
store volatile %struct.ScmObj* %args47255$ae43791$1, %struct.ScmObj** %stackaddr$prim48010, align 8
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%args47255$ae43791$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43792, %struct.ScmObj* %args47255$ae43791$1)
store volatile %struct.ScmObj* %args47255$ae43791$2, %struct.ScmObj** %stackaddr$prim48011, align 8
%clofunc48012 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43791)
musttail call tailcc void %clofunc48012(%struct.ScmObj* %ae43791, %struct.ScmObj* %args47255$ae43791$2)
ret void
}

define tailcc void @proc_clo$ae43791(%struct.ScmObj* %env$ae43791,%struct.ScmObj* %current_45args47121) {
%stackaddr$prim48013 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47121)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim48013, align 8
%stackaddr$prim48014 = alloca %struct.ScmObj*, align 8
%current_45args47122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47121)
store volatile %struct.ScmObj* %current_45args47122, %struct.ScmObj** %stackaddr$prim48014, align 8
%stackaddr$prim48015 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47122)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim48015, align 8
%stackaddr$makeclosure48016 = alloca %struct.ScmObj*, align 8
%fptrToInt48017 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43811 to i64
%ae43811 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48017)
store volatile %struct.ScmObj* %ae43811, %struct.ScmObj** %stackaddr$makeclosure48016, align 8
%ae43812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48018 = alloca %struct.ScmObj*, align 8
%fptrToInt48019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43813 to i64
%ae43813 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48019)
store volatile %struct.ScmObj* %ae43813, %struct.ScmObj** %stackaddr$makeclosure48018, align 8
%args47250$ae43811$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48020 = alloca %struct.ScmObj*, align 8
%args47250$ae43811$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43813, %struct.ScmObj* %args47250$ae43811$0)
store volatile %struct.ScmObj* %args47250$ae43811$1, %struct.ScmObj** %stackaddr$prim48020, align 8
%stackaddr$prim48021 = alloca %struct.ScmObj*, align 8
%args47250$ae43811$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43812, %struct.ScmObj* %args47250$ae43811$1)
store volatile %struct.ScmObj* %args47250$ae43811$2, %struct.ScmObj** %stackaddr$prim48021, align 8
%clofunc48022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43811)
musttail call tailcc void %clofunc48022(%struct.ScmObj* %ae43811, %struct.ScmObj* %args47250$ae43811$2)
ret void
}

define tailcc void @proc_clo$ae43811(%struct.ScmObj* %env$ae43811,%struct.ScmObj* %current_45args47124) {
%stackaddr$prim48023 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47124)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim48023, align 8
%stackaddr$prim48024 = alloca %struct.ScmObj*, align 8
%current_45args47125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47124)
store volatile %struct.ScmObj* %current_45args47125, %struct.ScmObj** %stackaddr$prim48024, align 8
%stackaddr$prim48025 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47125)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim48025, align 8
%stackaddr$makeclosure48026 = alloca %struct.ScmObj*, align 8
%fptrToInt48027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43833 to i64
%ae43833 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48027)
store volatile %struct.ScmObj* %ae43833, %struct.ScmObj** %stackaddr$makeclosure48026, align 8
%ae43834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48028 = alloca %struct.ScmObj*, align 8
%fptrToInt48029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43835 to i64
%ae43835 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48029)
store volatile %struct.ScmObj* %ae43835, %struct.ScmObj** %stackaddr$makeclosure48028, align 8
%args47245$ae43833$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48030 = alloca %struct.ScmObj*, align 8
%args47245$ae43833$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43835, %struct.ScmObj* %args47245$ae43833$0)
store volatile %struct.ScmObj* %args47245$ae43833$1, %struct.ScmObj** %stackaddr$prim48030, align 8
%stackaddr$prim48031 = alloca %struct.ScmObj*, align 8
%args47245$ae43833$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43834, %struct.ScmObj* %args47245$ae43833$1)
store volatile %struct.ScmObj* %args47245$ae43833$2, %struct.ScmObj** %stackaddr$prim48031, align 8
%clofunc48032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43833)
musttail call tailcc void %clofunc48032(%struct.ScmObj* %ae43833, %struct.ScmObj* %args47245$ae43833$2)
ret void
}

define tailcc void @proc_clo$ae43833(%struct.ScmObj* %env$ae43833,%struct.ScmObj* %current_45args47127) {
%stackaddr$prim48033 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47127)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim48033, align 8
%stackaddr$prim48034 = alloca %struct.ScmObj*, align 8
%current_45args47128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47127)
store volatile %struct.ScmObj* %current_45args47128, %struct.ScmObj** %stackaddr$prim48034, align 8
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47128)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim48035, align 8
%stackaddr$makeclosure48036 = alloca %struct.ScmObj*, align 8
%fptrToInt48037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43857 to i64
%ae43857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48037)
store volatile %struct.ScmObj* %ae43857, %struct.ScmObj** %stackaddr$makeclosure48036, align 8
%ae43858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48038 = alloca %struct.ScmObj*, align 8
%fptrToInt48039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43859 to i64
%ae43859 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48039)
store volatile %struct.ScmObj* %ae43859, %struct.ScmObj** %stackaddr$makeclosure48038, align 8
%args47240$ae43857$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%args47240$ae43857$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43859, %struct.ScmObj* %args47240$ae43857$0)
store volatile %struct.ScmObj* %args47240$ae43857$1, %struct.ScmObj** %stackaddr$prim48040, align 8
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%args47240$ae43857$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43858, %struct.ScmObj* %args47240$ae43857$1)
store volatile %struct.ScmObj* %args47240$ae43857$2, %struct.ScmObj** %stackaddr$prim48041, align 8
%clofunc48042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43857)
musttail call tailcc void %clofunc48042(%struct.ScmObj* %ae43857, %struct.ScmObj* %args47240$ae43857$2)
ret void
}

define tailcc void @proc_clo$ae43857(%struct.ScmObj* %env$ae43857,%struct.ScmObj* %current_45args47130) {
%stackaddr$prim48043 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47130)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim48043, align 8
%stackaddr$prim48044 = alloca %struct.ScmObj*, align 8
%current_45args47131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47130)
store volatile %struct.ScmObj* %current_45args47131, %struct.ScmObj** %stackaddr$prim48044, align 8
%stackaddr$prim48045 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47131)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim48045, align 8
%stackaddr$makeclosure48046 = alloca %struct.ScmObj*, align 8
%fptrToInt48047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43883 to i64
%ae43883 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48047)
store volatile %struct.ScmObj* %ae43883, %struct.ScmObj** %stackaddr$makeclosure48046, align 8
%ae43884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48048 = alloca %struct.ScmObj*, align 8
%fptrToInt48049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43885 to i64
%ae43885 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48049)
store volatile %struct.ScmObj* %ae43885, %struct.ScmObj** %stackaddr$makeclosure48048, align 8
%args47235$ae43883$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48050 = alloca %struct.ScmObj*, align 8
%args47235$ae43883$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43885, %struct.ScmObj* %args47235$ae43883$0)
store volatile %struct.ScmObj* %args47235$ae43883$1, %struct.ScmObj** %stackaddr$prim48050, align 8
%stackaddr$prim48051 = alloca %struct.ScmObj*, align 8
%args47235$ae43883$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43884, %struct.ScmObj* %args47235$ae43883$1)
store volatile %struct.ScmObj* %args47235$ae43883$2, %struct.ScmObj** %stackaddr$prim48051, align 8
%clofunc48052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43883)
musttail call tailcc void %clofunc48052(%struct.ScmObj* %ae43883, %struct.ScmObj* %args47235$ae43883$2)
ret void
}

define tailcc void @proc_clo$ae43883(%struct.ScmObj* %env$ae43883,%struct.ScmObj* %current_45args47133) {
%stackaddr$prim48053 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim48053, align 8
%stackaddr$prim48054 = alloca %struct.ScmObj*, align 8
%current_45args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %current_45args47134, %struct.ScmObj** %stackaddr$prim48054, align 8
%stackaddr$prim48055 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47134)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim48055, align 8
%ae43970 = call %struct.ScmObj* @const_init_true()
%truthy$cmp48056 = call i64 @is_truthy_value(%struct.ScmObj* %ae43970)
%cmp$cmp48056 = icmp eq i64 %truthy$cmp48056, 1
br i1 %cmp$cmp48056, label %truebranch$cmp48056, label %falsebranch$cmp48056
truebranch$cmp48056:
%stackaddr$makeclosure48057 = alloca %struct.ScmObj*, align 8
%fptrToInt48058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43971 to i64
%ae43971 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48058)
store volatile %struct.ScmObj* %ae43971, %struct.ScmObj** %stackaddr$makeclosure48057, align 8
%ae43972 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43973 = call %struct.ScmObj* @const_init_int(i64 10)
%args47166$ae43971$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48059 = alloca %struct.ScmObj*, align 8
%args47166$ae43971$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43973, %struct.ScmObj* %args47166$ae43971$0)
store volatile %struct.ScmObj* %args47166$ae43971$1, %struct.ScmObj** %stackaddr$prim48059, align 8
%stackaddr$prim48060 = alloca %struct.ScmObj*, align 8
%args47166$ae43971$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43972, %struct.ScmObj* %args47166$ae43971$1)
store volatile %struct.ScmObj* %args47166$ae43971$2, %struct.ScmObj** %stackaddr$prim48060, align 8
%clofunc48061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43971)
musttail call tailcc void %clofunc48061(%struct.ScmObj* %ae43971, %struct.ScmObj* %args47166$ae43971$2)
ret void
falsebranch$cmp48056:
%ae44118 = call %struct.ScmObj* @const_init_int(i64 20)
%truthy$cmp48062 = call i64 @is_truthy_value(%struct.ScmObj* %ae44118)
%cmp$cmp48062 = icmp eq i64 %truthy$cmp48062, 1
br i1 %cmp$cmp48062, label %truebranch$cmp48062, label %falsebranch$cmp48062
truebranch$cmp48062:
%stackaddr$makeclosure48063 = alloca %struct.ScmObj*, align 8
%fptrToInt48064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44119 to i64
%ae44119 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48064)
store volatile %struct.ScmObj* %ae44119, %struct.ScmObj** %stackaddr$makeclosure48063, align 8
%ae44120 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44121 = call %struct.ScmObj* @const_init_int(i64 20)
%args47197$ae44119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48065 = alloca %struct.ScmObj*, align 8
%args47197$ae44119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44121, %struct.ScmObj* %args47197$ae44119$0)
store volatile %struct.ScmObj* %args47197$ae44119$1, %struct.ScmObj** %stackaddr$prim48065, align 8
%stackaddr$prim48066 = alloca %struct.ScmObj*, align 8
%args47197$ae44119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44120, %struct.ScmObj* %args47197$ae44119$1)
store volatile %struct.ScmObj* %args47197$ae44119$2, %struct.ScmObj** %stackaddr$prim48066, align 8
%clofunc48067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44119)
musttail call tailcc void %clofunc48067(%struct.ScmObj* %ae44119, %struct.ScmObj* %args47197$ae44119$2)
ret void
falsebranch$cmp48062:
%stackaddr$prim48068 = alloca %struct.ScmObj*, align 8
%cpsprim40376 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim40376, %struct.ScmObj** %stackaddr$prim48068, align 8
%stackaddr$makeclosure48069 = alloca %struct.ScmObj*, align 8
%fptrToInt48070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44266 to i64
%ae44266 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48070)
store volatile %struct.ScmObj* %ae44266, %struct.ScmObj** %stackaddr$makeclosure48069, align 8
%ae44267 = call %struct.ScmObj* @const_init_int(i64 0)
%args47228$ae44266$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48071 = alloca %struct.ScmObj*, align 8
%args47228$ae44266$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40376, %struct.ScmObj* %args47228$ae44266$0)
store volatile %struct.ScmObj* %args47228$ae44266$1, %struct.ScmObj** %stackaddr$prim48071, align 8
%stackaddr$prim48072 = alloca %struct.ScmObj*, align 8
%args47228$ae44266$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44267, %struct.ScmObj* %args47228$ae44266$1)
store volatile %struct.ScmObj* %args47228$ae44266$2, %struct.ScmObj** %stackaddr$prim48072, align 8
%clofunc48073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44266)
musttail call tailcc void %clofunc48073(%struct.ScmObj* %ae44266, %struct.ScmObj* %args47228$ae44266$2)
ret void
}

define tailcc void @proc_clo$ae43971(%struct.ScmObj* %env$ae43971,%struct.ScmObj* %current_45args47136) {
%stackaddr$prim48074 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47136)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim48074, align 8
%stackaddr$prim48075 = alloca %struct.ScmObj*, align 8
%current_45args47137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47136)
store volatile %struct.ScmObj* %current_45args47137, %struct.ScmObj** %stackaddr$prim48075, align 8
%stackaddr$prim48076 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47137)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48076, align 8
%ae43980 = call %struct.ScmObj* @const_init_false()
%truthy$cmp48077 = call i64 @is_truthy_value(%struct.ScmObj* %ae43980)
%cmp$cmp48077 = icmp eq i64 %truthy$cmp48077, 1
br i1 %cmp$cmp48077, label %truebranch$cmp48077, label %falsebranch$cmp48077
truebranch$cmp48077:
%stackaddr$makeclosure48078 = alloca %struct.ScmObj*, align 8
%fptrToInt48079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43981 to i64
%ae43981 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48079)
store volatile %struct.ScmObj* %ae43981, %struct.ScmObj** %stackaddr$makeclosure48078, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43981, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae43982 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43983 = call %struct.ScmObj* @const_init_int(i64 10)
%args47147$ae43981$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48080 = alloca %struct.ScmObj*, align 8
%args47147$ae43981$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43983, %struct.ScmObj* %args47147$ae43981$0)
store volatile %struct.ScmObj* %args47147$ae43981$1, %struct.ScmObj** %stackaddr$prim48080, align 8
%stackaddr$prim48081 = alloca %struct.ScmObj*, align 8
%args47147$ae43981$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43982, %struct.ScmObj* %args47147$ae43981$1)
store volatile %struct.ScmObj* %args47147$ae43981$2, %struct.ScmObj** %stackaddr$prim48081, align 8
%clofunc48082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43981)
musttail call tailcc void %clofunc48082(%struct.ScmObj* %ae43981, %struct.ScmObj* %args47147$ae43981$2)
ret void
falsebranch$cmp48077:
%ae44007 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp48083 = call i64 @is_truthy_value(%struct.ScmObj* %ae44007)
%cmp$cmp48083 = icmp eq i64 %truthy$cmp48083, 1
br i1 %cmp$cmp48083, label %truebranch$cmp48083, label %falsebranch$cmp48083
truebranch$cmp48083:
%stackaddr$makeclosure48084 = alloca %struct.ScmObj*, align 8
%fptrToInt48085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44008 to i64
%ae44008 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48085)
store volatile %struct.ScmObj* %ae44008, %struct.ScmObj** %stackaddr$makeclosure48084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44008, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44009 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44010 = call %struct.ScmObj* @const_init_int(i64 30)
%args47156$ae44008$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%args47156$ae44008$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44010, %struct.ScmObj* %args47156$ae44008$0)
store volatile %struct.ScmObj* %args47156$ae44008$1, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%args47156$ae44008$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44009, %struct.ScmObj* %args47156$ae44008$1)
store volatile %struct.ScmObj* %args47156$ae44008$2, %struct.ScmObj** %stackaddr$prim48087, align 8
%clofunc48088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44008)
musttail call tailcc void %clofunc48088(%struct.ScmObj* %ae44008, %struct.ScmObj* %args47156$ae44008$2)
ret void
falsebranch$cmp48083:
%stackaddr$prim48089 = alloca %struct.ScmObj*, align 8
%cpsprim40375 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim40375, %struct.ScmObj** %stackaddr$prim48089, align 8
%stackaddr$makeclosure48090 = alloca %struct.ScmObj*, align 8
%fptrToInt48091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44034 to i64
%ae44034 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48091)
store volatile %struct.ScmObj* %ae44034, %struct.ScmObj** %stackaddr$makeclosure48090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44035 = call %struct.ScmObj* @const_init_int(i64 0)
%args47165$ae44034$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48092 = alloca %struct.ScmObj*, align 8
%args47165$ae44034$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40375, %struct.ScmObj* %args47165$ae44034$0)
store volatile %struct.ScmObj* %args47165$ae44034$1, %struct.ScmObj** %stackaddr$prim48092, align 8
%stackaddr$prim48093 = alloca %struct.ScmObj*, align 8
%args47165$ae44034$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44035, %struct.ScmObj* %args47165$ae44034$1)
store volatile %struct.ScmObj* %args47165$ae44034$2, %struct.ScmObj** %stackaddr$prim48093, align 8
%clofunc48094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44034)
musttail call tailcc void %clofunc48094(%struct.ScmObj* %ae44034, %struct.ScmObj* %args47165$ae44034$2)
ret void
}

define tailcc void @proc_clo$ae43981(%struct.ScmObj* %env$ae43981,%struct.ScmObj* %current_45args47139) {
%stackaddr$env-ref48095 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43981, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48095
%stackaddr$prim48096 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47139)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48096, align 8
%stackaddr$prim48097 = alloca %struct.ScmObj*, align 8
%current_45args47140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47139)
store volatile %struct.ScmObj* %current_45args47140, %struct.ScmObj** %stackaddr$prim48097, align 8
%stackaddr$prim48098 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47140)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48098, align 8
%stackaddr$prim48099 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48099, align 8
%stackaddr$makeclosure48100 = alloca %struct.ScmObj*, align 8
%fptrToInt48101 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43992 to i64
%ae43992 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48101)
store volatile %struct.ScmObj* %ae43992, %struct.ScmObj** %stackaddr$makeclosure48100, align 8
%ae43993 = call %struct.ScmObj* @const_init_int(i64 0)
%args47146$ae43992$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48102 = alloca %struct.ScmObj*, align 8
%args47146$ae43992$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47146$ae43992$0)
store volatile %struct.ScmObj* %args47146$ae43992$1, %struct.ScmObj** %stackaddr$prim48102, align 8
%stackaddr$prim48103 = alloca %struct.ScmObj*, align 8
%args47146$ae43992$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43993, %struct.ScmObj* %args47146$ae43992$1)
store volatile %struct.ScmObj* %args47146$ae43992$2, %struct.ScmObj** %stackaddr$prim48103, align 8
%clofunc48104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43992)
musttail call tailcc void %clofunc48104(%struct.ScmObj* %ae43992, %struct.ScmObj* %args47146$ae43992$2)
ret void
}

define tailcc void @proc_clo$ae43992(%struct.ScmObj* %env$ae43992,%struct.ScmObj* %current_45args47142) {
%stackaddr$prim48105 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47142)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48105, align 8
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%current_45args47143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47142)
store volatile %struct.ScmObj* %current_45args47143, %struct.ScmObj** %stackaddr$prim48106, align 8
%stackaddr$prim48107 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47143)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48107, align 8
%stackaddr$prim48108 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48108, align 8
%args47145$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48109 = alloca %struct.ScmObj*, align 8
%args47145$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47145$k$0)
store volatile %struct.ScmObj* %args47145$k$1, %struct.ScmObj** %stackaddr$prim48109, align 8
%clofunc48110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48110(%struct.ScmObj* %k, %struct.ScmObj* %args47145$k$1)
ret void
}

define tailcc void @proc_clo$ae44008(%struct.ScmObj* %env$ae44008,%struct.ScmObj* %current_45args47148) {
%stackaddr$env-ref48111 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44008, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48111
%stackaddr$prim48112 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47148)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48112, align 8
%stackaddr$prim48113 = alloca %struct.ScmObj*, align 8
%current_45args47149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47148)
store volatile %struct.ScmObj* %current_45args47149, %struct.ScmObj** %stackaddr$prim48113, align 8
%stackaddr$prim48114 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47149)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48114, align 8
%stackaddr$prim48115 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48115, align 8
%stackaddr$makeclosure48116 = alloca %struct.ScmObj*, align 8
%fptrToInt48117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44019 to i64
%ae44019 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48117)
store volatile %struct.ScmObj* %ae44019, %struct.ScmObj** %stackaddr$makeclosure48116, align 8
%ae44020 = call %struct.ScmObj* @const_init_int(i64 0)
%args47155$ae44019$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48118 = alloca %struct.ScmObj*, align 8
%args47155$ae44019$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47155$ae44019$0)
store volatile %struct.ScmObj* %args47155$ae44019$1, %struct.ScmObj** %stackaddr$prim48118, align 8
%stackaddr$prim48119 = alloca %struct.ScmObj*, align 8
%args47155$ae44019$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44020, %struct.ScmObj* %args47155$ae44019$1)
store volatile %struct.ScmObj* %args47155$ae44019$2, %struct.ScmObj** %stackaddr$prim48119, align 8
%clofunc48120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44019)
musttail call tailcc void %clofunc48120(%struct.ScmObj* %ae44019, %struct.ScmObj* %args47155$ae44019$2)
ret void
}

define tailcc void @proc_clo$ae44019(%struct.ScmObj* %env$ae44019,%struct.ScmObj* %current_45args47151) {
%stackaddr$prim48121 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47151)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48121, align 8
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%current_45args47152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47151)
store volatile %struct.ScmObj* %current_45args47152, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47152)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48123, align 8
%stackaddr$prim48124 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48124, align 8
%args47154$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48125 = alloca %struct.ScmObj*, align 8
%args47154$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47154$k$0)
store volatile %struct.ScmObj* %args47154$k$1, %struct.ScmObj** %stackaddr$prim48125, align 8
%clofunc48126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48126(%struct.ScmObj* %k, %struct.ScmObj* %args47154$k$1)
ret void
}

define tailcc void @proc_clo$ae44034(%struct.ScmObj* %env$ae44034,%struct.ScmObj* %current_45args47157) {
%stackaddr$env-ref48127 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48127
%stackaddr$prim48128 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47157)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48128, align 8
%stackaddr$prim48129 = alloca %struct.ScmObj*, align 8
%current_45args47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47157)
store volatile %struct.ScmObj* %current_45args47158, %struct.ScmObj** %stackaddr$prim48129, align 8
%stackaddr$prim48130 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47158)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48130, align 8
%stackaddr$prim48131 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48131, align 8
%stackaddr$makeclosure48132 = alloca %struct.ScmObj*, align 8
%fptrToInt48133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44042 to i64
%ae44042 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48133)
store volatile %struct.ScmObj* %ae44042, %struct.ScmObj** %stackaddr$makeclosure48132, align 8
%ae44043 = call %struct.ScmObj* @const_init_int(i64 0)
%args47164$ae44042$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%args47164$ae44042$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47164$ae44042$0)
store volatile %struct.ScmObj* %args47164$ae44042$1, %struct.ScmObj** %stackaddr$prim48134, align 8
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%args47164$ae44042$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44043, %struct.ScmObj* %args47164$ae44042$1)
store volatile %struct.ScmObj* %args47164$ae44042$2, %struct.ScmObj** %stackaddr$prim48135, align 8
%clofunc48136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44042)
musttail call tailcc void %clofunc48136(%struct.ScmObj* %ae44042, %struct.ScmObj* %args47164$ae44042$2)
ret void
}

define tailcc void @proc_clo$ae44042(%struct.ScmObj* %env$ae44042,%struct.ScmObj* %current_45args47160) {
%stackaddr$prim48137 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47160)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48137, align 8
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%current_45args47161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47160)
store volatile %struct.ScmObj* %current_45args47161, %struct.ScmObj** %stackaddr$prim48138, align 8
%stackaddr$prim48139 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47161)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48139, align 8
%stackaddr$prim48140 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48140, align 8
%args47163$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48141 = alloca %struct.ScmObj*, align 8
%args47163$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47163$k$0)
store volatile %struct.ScmObj* %args47163$k$1, %struct.ScmObj** %stackaddr$prim48141, align 8
%clofunc48142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48142(%struct.ScmObj* %k, %struct.ScmObj* %args47163$k$1)
ret void
}

define tailcc void @proc_clo$ae44119(%struct.ScmObj* %env$ae44119,%struct.ScmObj* %current_45args47167) {
%stackaddr$prim48143 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47167)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim48143, align 8
%stackaddr$prim48144 = alloca %struct.ScmObj*, align 8
%current_45args47168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47167)
store volatile %struct.ScmObj* %current_45args47168, %struct.ScmObj** %stackaddr$prim48144, align 8
%stackaddr$prim48145 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47168)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48145, align 8
%ae44128 = call %struct.ScmObj* @const_init_false()
%truthy$cmp48146 = call i64 @is_truthy_value(%struct.ScmObj* %ae44128)
%cmp$cmp48146 = icmp eq i64 %truthy$cmp48146, 1
br i1 %cmp$cmp48146, label %truebranch$cmp48146, label %falsebranch$cmp48146
truebranch$cmp48146:
%stackaddr$makeclosure48147 = alloca %struct.ScmObj*, align 8
%fptrToInt48148 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44129 to i64
%ae44129 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48148)
store volatile %struct.ScmObj* %ae44129, %struct.ScmObj** %stackaddr$makeclosure48147, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44129, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44130 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44131 = call %struct.ScmObj* @const_init_int(i64 10)
%args47178$ae44129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%args47178$ae44129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44131, %struct.ScmObj* %args47178$ae44129$0)
store volatile %struct.ScmObj* %args47178$ae44129$1, %struct.ScmObj** %stackaddr$prim48149, align 8
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%args47178$ae44129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44130, %struct.ScmObj* %args47178$ae44129$1)
store volatile %struct.ScmObj* %args47178$ae44129$2, %struct.ScmObj** %stackaddr$prim48150, align 8
%clofunc48151 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44129)
musttail call tailcc void %clofunc48151(%struct.ScmObj* %ae44129, %struct.ScmObj* %args47178$ae44129$2)
ret void
falsebranch$cmp48146:
%ae44155 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp48152 = call i64 @is_truthy_value(%struct.ScmObj* %ae44155)
%cmp$cmp48152 = icmp eq i64 %truthy$cmp48152, 1
br i1 %cmp$cmp48152, label %truebranch$cmp48152, label %falsebranch$cmp48152
truebranch$cmp48152:
%stackaddr$makeclosure48153 = alloca %struct.ScmObj*, align 8
%fptrToInt48154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44156 to i64
%ae44156 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48154)
store volatile %struct.ScmObj* %ae44156, %struct.ScmObj** %stackaddr$makeclosure48153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44156, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44157 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44158 = call %struct.ScmObj* @const_init_int(i64 30)
%args47187$ae44156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48155 = alloca %struct.ScmObj*, align 8
%args47187$ae44156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44158, %struct.ScmObj* %args47187$ae44156$0)
store volatile %struct.ScmObj* %args47187$ae44156$1, %struct.ScmObj** %stackaddr$prim48155, align 8
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%args47187$ae44156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44157, %struct.ScmObj* %args47187$ae44156$1)
store volatile %struct.ScmObj* %args47187$ae44156$2, %struct.ScmObj** %stackaddr$prim48156, align 8
%clofunc48157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44156)
musttail call tailcc void %clofunc48157(%struct.ScmObj* %ae44156, %struct.ScmObj* %args47187$ae44156$2)
ret void
falsebranch$cmp48152:
%stackaddr$prim48158 = alloca %struct.ScmObj*, align 8
%cpsprim40375 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim40375, %struct.ScmObj** %stackaddr$prim48158, align 8
%stackaddr$makeclosure48159 = alloca %struct.ScmObj*, align 8
%fptrToInt48160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44182 to i64
%ae44182 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48160)
store volatile %struct.ScmObj* %ae44182, %struct.ScmObj** %stackaddr$makeclosure48159, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44182, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44183 = call %struct.ScmObj* @const_init_int(i64 0)
%args47196$ae44182$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48161 = alloca %struct.ScmObj*, align 8
%args47196$ae44182$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40375, %struct.ScmObj* %args47196$ae44182$0)
store volatile %struct.ScmObj* %args47196$ae44182$1, %struct.ScmObj** %stackaddr$prim48161, align 8
%stackaddr$prim48162 = alloca %struct.ScmObj*, align 8
%args47196$ae44182$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44183, %struct.ScmObj* %args47196$ae44182$1)
store volatile %struct.ScmObj* %args47196$ae44182$2, %struct.ScmObj** %stackaddr$prim48162, align 8
%clofunc48163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44182)
musttail call tailcc void %clofunc48163(%struct.ScmObj* %ae44182, %struct.ScmObj* %args47196$ae44182$2)
ret void
}

define tailcc void @proc_clo$ae44129(%struct.ScmObj* %env$ae44129,%struct.ScmObj* %current_45args47170) {
%stackaddr$env-ref48164 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44129, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48164
%stackaddr$prim48165 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47170)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48165, align 8
%stackaddr$prim48166 = alloca %struct.ScmObj*, align 8
%current_45args47171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47170)
store volatile %struct.ScmObj* %current_45args47171, %struct.ScmObj** %stackaddr$prim48166, align 8
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47171)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$makeclosure48169 = alloca %struct.ScmObj*, align 8
%fptrToInt48170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44140 to i64
%ae44140 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48170)
store volatile %struct.ScmObj* %ae44140, %struct.ScmObj** %stackaddr$makeclosure48169, align 8
%ae44141 = call %struct.ScmObj* @const_init_int(i64 0)
%args47177$ae44140$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48171 = alloca %struct.ScmObj*, align 8
%args47177$ae44140$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47177$ae44140$0)
store volatile %struct.ScmObj* %args47177$ae44140$1, %struct.ScmObj** %stackaddr$prim48171, align 8
%stackaddr$prim48172 = alloca %struct.ScmObj*, align 8
%args47177$ae44140$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44141, %struct.ScmObj* %args47177$ae44140$1)
store volatile %struct.ScmObj* %args47177$ae44140$2, %struct.ScmObj** %stackaddr$prim48172, align 8
%clofunc48173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44140)
musttail call tailcc void %clofunc48173(%struct.ScmObj* %ae44140, %struct.ScmObj* %args47177$ae44140$2)
ret void
}

define tailcc void @proc_clo$ae44140(%struct.ScmObj* %env$ae44140,%struct.ScmObj* %current_45args47173) {
%stackaddr$prim48174 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47173)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48174, align 8
%stackaddr$prim48175 = alloca %struct.ScmObj*, align 8
%current_45args47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47173)
store volatile %struct.ScmObj* %current_45args47174, %struct.ScmObj** %stackaddr$prim48175, align 8
%stackaddr$prim48176 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47174)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48176, align 8
%stackaddr$prim48177 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48177, align 8
%args47176$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48178 = alloca %struct.ScmObj*, align 8
%args47176$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47176$k$0)
store volatile %struct.ScmObj* %args47176$k$1, %struct.ScmObj** %stackaddr$prim48178, align 8
%clofunc48179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48179(%struct.ScmObj* %k, %struct.ScmObj* %args47176$k$1)
ret void
}

define tailcc void @proc_clo$ae44156(%struct.ScmObj* %env$ae44156,%struct.ScmObj* %current_45args47179) {
%stackaddr$env-ref48180 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44156, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48180
%stackaddr$prim48181 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47179)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48181, align 8
%stackaddr$prim48182 = alloca %struct.ScmObj*, align 8
%current_45args47180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47179)
store volatile %struct.ScmObj* %current_45args47180, %struct.ScmObj** %stackaddr$prim48182, align 8
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47180)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48183, align 8
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48184, align 8
%stackaddr$makeclosure48185 = alloca %struct.ScmObj*, align 8
%fptrToInt48186 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44167 to i64
%ae44167 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48186)
store volatile %struct.ScmObj* %ae44167, %struct.ScmObj** %stackaddr$makeclosure48185, align 8
%ae44168 = call %struct.ScmObj* @const_init_int(i64 0)
%args47186$ae44167$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48187 = alloca %struct.ScmObj*, align 8
%args47186$ae44167$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47186$ae44167$0)
store volatile %struct.ScmObj* %args47186$ae44167$1, %struct.ScmObj** %stackaddr$prim48187, align 8
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%args47186$ae44167$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44168, %struct.ScmObj* %args47186$ae44167$1)
store volatile %struct.ScmObj* %args47186$ae44167$2, %struct.ScmObj** %stackaddr$prim48188, align 8
%clofunc48189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44167)
musttail call tailcc void %clofunc48189(%struct.ScmObj* %ae44167, %struct.ScmObj* %args47186$ae44167$2)
ret void
}

define tailcc void @proc_clo$ae44167(%struct.ScmObj* %env$ae44167,%struct.ScmObj* %current_45args47182) {
%stackaddr$prim48190 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47182)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48190, align 8
%stackaddr$prim48191 = alloca %struct.ScmObj*, align 8
%current_45args47183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47182)
store volatile %struct.ScmObj* %current_45args47183, %struct.ScmObj** %stackaddr$prim48191, align 8
%stackaddr$prim48192 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47183)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48192, align 8
%stackaddr$prim48193 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48193, align 8
%args47185$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48194 = alloca %struct.ScmObj*, align 8
%args47185$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47185$k$0)
store volatile %struct.ScmObj* %args47185$k$1, %struct.ScmObj** %stackaddr$prim48194, align 8
%clofunc48195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48195(%struct.ScmObj* %k, %struct.ScmObj* %args47185$k$1)
ret void
}

define tailcc void @proc_clo$ae44182(%struct.ScmObj* %env$ae44182,%struct.ScmObj* %current_45args47188) {
%stackaddr$env-ref48196 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44182, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48196
%stackaddr$prim48197 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47188)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48197, align 8
%stackaddr$prim48198 = alloca %struct.ScmObj*, align 8
%current_45args47189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47188)
store volatile %struct.ScmObj* %current_45args47189, %struct.ScmObj** %stackaddr$prim48198, align 8
%stackaddr$prim48199 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47189)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48199, align 8
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$makeclosure48201 = alloca %struct.ScmObj*, align 8
%fptrToInt48202 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44190 to i64
%ae44190 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48202)
store volatile %struct.ScmObj* %ae44190, %struct.ScmObj** %stackaddr$makeclosure48201, align 8
%ae44191 = call %struct.ScmObj* @const_init_int(i64 0)
%args47195$ae44190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48203 = alloca %struct.ScmObj*, align 8
%args47195$ae44190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47195$ae44190$0)
store volatile %struct.ScmObj* %args47195$ae44190$1, %struct.ScmObj** %stackaddr$prim48203, align 8
%stackaddr$prim48204 = alloca %struct.ScmObj*, align 8
%args47195$ae44190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44191, %struct.ScmObj* %args47195$ae44190$1)
store volatile %struct.ScmObj* %args47195$ae44190$2, %struct.ScmObj** %stackaddr$prim48204, align 8
%clofunc48205 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44190)
musttail call tailcc void %clofunc48205(%struct.ScmObj* %ae44190, %struct.ScmObj* %args47195$ae44190$2)
ret void
}

define tailcc void @proc_clo$ae44190(%struct.ScmObj* %env$ae44190,%struct.ScmObj* %current_45args47191) {
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47191)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%current_45args47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47191)
store volatile %struct.ScmObj* %current_45args47192, %struct.ScmObj** %stackaddr$prim48207, align 8
%stackaddr$prim48208 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47192)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48208, align 8
%stackaddr$prim48209 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48209, align 8
%args47194$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48210 = alloca %struct.ScmObj*, align 8
%args47194$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47194$k$0)
store volatile %struct.ScmObj* %args47194$k$1, %struct.ScmObj** %stackaddr$prim48210, align 8
%clofunc48211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48211(%struct.ScmObj* %k, %struct.ScmObj* %args47194$k$1)
ret void
}

define tailcc void @proc_clo$ae44266(%struct.ScmObj* %env$ae44266,%struct.ScmObj* %current_45args47198) {
%stackaddr$prim48212 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47198)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim48212, align 8
%stackaddr$prim48213 = alloca %struct.ScmObj*, align 8
%current_45args47199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47198)
store volatile %struct.ScmObj* %current_45args47199, %struct.ScmObj** %stackaddr$prim48213, align 8
%stackaddr$prim48214 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47199)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48214, align 8
%ae44272 = call %struct.ScmObj* @const_init_false()
%truthy$cmp48215 = call i64 @is_truthy_value(%struct.ScmObj* %ae44272)
%cmp$cmp48215 = icmp eq i64 %truthy$cmp48215, 1
br i1 %cmp$cmp48215, label %truebranch$cmp48215, label %falsebranch$cmp48215
truebranch$cmp48215:
%stackaddr$makeclosure48216 = alloca %struct.ScmObj*, align 8
%fptrToInt48217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44273 to i64
%ae44273 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48217)
store volatile %struct.ScmObj* %ae44273, %struct.ScmObj** %stackaddr$makeclosure48216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44273, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44274 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44275 = call %struct.ScmObj* @const_init_int(i64 10)
%args47209$ae44273$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%args47209$ae44273$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44275, %struct.ScmObj* %args47209$ae44273$0)
store volatile %struct.ScmObj* %args47209$ae44273$1, %struct.ScmObj** %stackaddr$prim48218, align 8
%stackaddr$prim48219 = alloca %struct.ScmObj*, align 8
%args47209$ae44273$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44274, %struct.ScmObj* %args47209$ae44273$1)
store volatile %struct.ScmObj* %args47209$ae44273$2, %struct.ScmObj** %stackaddr$prim48219, align 8
%clofunc48220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44273)
musttail call tailcc void %clofunc48220(%struct.ScmObj* %ae44273, %struct.ScmObj* %args47209$ae44273$2)
ret void
falsebranch$cmp48215:
%ae44299 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp48221 = call i64 @is_truthy_value(%struct.ScmObj* %ae44299)
%cmp$cmp48221 = icmp eq i64 %truthy$cmp48221, 1
br i1 %cmp$cmp48221, label %truebranch$cmp48221, label %falsebranch$cmp48221
truebranch$cmp48221:
%stackaddr$makeclosure48222 = alloca %struct.ScmObj*, align 8
%fptrToInt48223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44300 to i64
%ae44300 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48223)
store volatile %struct.ScmObj* %ae44300, %struct.ScmObj** %stackaddr$makeclosure48222, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44301 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44302 = call %struct.ScmObj* @const_init_int(i64 30)
%args47218$ae44300$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48224 = alloca %struct.ScmObj*, align 8
%args47218$ae44300$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44302, %struct.ScmObj* %args47218$ae44300$0)
store volatile %struct.ScmObj* %args47218$ae44300$1, %struct.ScmObj** %stackaddr$prim48224, align 8
%stackaddr$prim48225 = alloca %struct.ScmObj*, align 8
%args47218$ae44300$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44301, %struct.ScmObj* %args47218$ae44300$1)
store volatile %struct.ScmObj* %args47218$ae44300$2, %struct.ScmObj** %stackaddr$prim48225, align 8
%clofunc48226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44300)
musttail call tailcc void %clofunc48226(%struct.ScmObj* %ae44300, %struct.ScmObj* %args47218$ae44300$2)
ret void
falsebranch$cmp48221:
%stackaddr$prim48227 = alloca %struct.ScmObj*, align 8
%cpsprim40375 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim40375, %struct.ScmObj** %stackaddr$prim48227, align 8
%stackaddr$makeclosure48228 = alloca %struct.ScmObj*, align 8
%fptrToInt48229 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44326 to i64
%ae44326 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48229)
store volatile %struct.ScmObj* %ae44326, %struct.ScmObj** %stackaddr$makeclosure48228, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44326, %struct.ScmObj* %anf_45bind40337, i64 0)
%ae44327 = call %struct.ScmObj* @const_init_int(i64 0)
%args47227$ae44326$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48230 = alloca %struct.ScmObj*, align 8
%args47227$ae44326$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40375, %struct.ScmObj* %args47227$ae44326$0)
store volatile %struct.ScmObj* %args47227$ae44326$1, %struct.ScmObj** %stackaddr$prim48230, align 8
%stackaddr$prim48231 = alloca %struct.ScmObj*, align 8
%args47227$ae44326$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44327, %struct.ScmObj* %args47227$ae44326$1)
store volatile %struct.ScmObj* %args47227$ae44326$2, %struct.ScmObj** %stackaddr$prim48231, align 8
%clofunc48232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44326)
musttail call tailcc void %clofunc48232(%struct.ScmObj* %ae44326, %struct.ScmObj* %args47227$ae44326$2)
ret void
}

define tailcc void @proc_clo$ae44273(%struct.ScmObj* %env$ae44273,%struct.ScmObj* %current_45args47201) {
%stackaddr$env-ref48233 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44273, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48233
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47201)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$prim48235 = alloca %struct.ScmObj*, align 8
%current_45args47202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47201)
store volatile %struct.ScmObj* %current_45args47202, %struct.ScmObj** %stackaddr$prim48235, align 8
%stackaddr$prim48236 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47202)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48236, align 8
%stackaddr$prim48237 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48237, align 8
%stackaddr$makeclosure48238 = alloca %struct.ScmObj*, align 8
%fptrToInt48239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44284 to i64
%ae44284 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48239)
store volatile %struct.ScmObj* %ae44284, %struct.ScmObj** %stackaddr$makeclosure48238, align 8
%ae44285 = call %struct.ScmObj* @const_init_int(i64 0)
%args47208$ae44284$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%args47208$ae44284$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47208$ae44284$0)
store volatile %struct.ScmObj* %args47208$ae44284$1, %struct.ScmObj** %stackaddr$prim48240, align 8
%stackaddr$prim48241 = alloca %struct.ScmObj*, align 8
%args47208$ae44284$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44285, %struct.ScmObj* %args47208$ae44284$1)
store volatile %struct.ScmObj* %args47208$ae44284$2, %struct.ScmObj** %stackaddr$prim48241, align 8
%clofunc48242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44284)
musttail call tailcc void %clofunc48242(%struct.ScmObj* %ae44284, %struct.ScmObj* %args47208$ae44284$2)
ret void
}

define tailcc void @proc_clo$ae44284(%struct.ScmObj* %env$ae44284,%struct.ScmObj* %current_45args47204) {
%stackaddr$prim48243 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47204)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48243, align 8
%stackaddr$prim48244 = alloca %struct.ScmObj*, align 8
%current_45args47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47204)
store volatile %struct.ScmObj* %current_45args47205, %struct.ScmObj** %stackaddr$prim48244, align 8
%stackaddr$prim48245 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47205)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48245, align 8
%stackaddr$prim48246 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48246, align 8
%args47207$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48247 = alloca %struct.ScmObj*, align 8
%args47207$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47207$k$0)
store volatile %struct.ScmObj* %args47207$k$1, %struct.ScmObj** %stackaddr$prim48247, align 8
%clofunc48248 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48248(%struct.ScmObj* %k, %struct.ScmObj* %args47207$k$1)
ret void
}

define tailcc void @proc_clo$ae44300(%struct.ScmObj* %env$ae44300,%struct.ScmObj* %current_45args47210) {
%stackaddr$env-ref48249 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48249
%stackaddr$prim48250 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47210)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48250, align 8
%stackaddr$prim48251 = alloca %struct.ScmObj*, align 8
%current_45args47211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47210)
store volatile %struct.ScmObj* %current_45args47211, %struct.ScmObj** %stackaddr$prim48251, align 8
%stackaddr$prim48252 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47211)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48252, align 8
%stackaddr$prim48253 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48253, align 8
%stackaddr$makeclosure48254 = alloca %struct.ScmObj*, align 8
%fptrToInt48255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44311 to i64
%ae44311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48255)
store volatile %struct.ScmObj* %ae44311, %struct.ScmObj** %stackaddr$makeclosure48254, align 8
%ae44312 = call %struct.ScmObj* @const_init_int(i64 0)
%args47217$ae44311$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%args47217$ae44311$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47217$ae44311$0)
store volatile %struct.ScmObj* %args47217$ae44311$1, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%args47217$ae44311$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44312, %struct.ScmObj* %args47217$ae44311$1)
store volatile %struct.ScmObj* %args47217$ae44311$2, %struct.ScmObj** %stackaddr$prim48257, align 8
%clofunc48258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44311)
musttail call tailcc void %clofunc48258(%struct.ScmObj* %ae44311, %struct.ScmObj* %args47217$ae44311$2)
ret void
}

define tailcc void @proc_clo$ae44311(%struct.ScmObj* %env$ae44311,%struct.ScmObj* %current_45args47213) {
%stackaddr$prim48259 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47213)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48259, align 8
%stackaddr$prim48260 = alloca %struct.ScmObj*, align 8
%current_45args47214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47213)
store volatile %struct.ScmObj* %current_45args47214, %struct.ScmObj** %stackaddr$prim48260, align 8
%stackaddr$prim48261 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47214)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48261, align 8
%stackaddr$prim48262 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48262, align 8
%args47216$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48263 = alloca %struct.ScmObj*, align 8
%args47216$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47216$k$0)
store volatile %struct.ScmObj* %args47216$k$1, %struct.ScmObj** %stackaddr$prim48263, align 8
%clofunc48264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48264(%struct.ScmObj* %k, %struct.ScmObj* %args47216$k$1)
ret void
}

define tailcc void @proc_clo$ae44326(%struct.ScmObj* %env$ae44326,%struct.ScmObj* %current_45args47219) {
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44326, i64 0)
store %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$prim48266 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47219)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim48266, align 8
%stackaddr$prim48267 = alloca %struct.ScmObj*, align 8
%current_45args47220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47219)
store volatile %struct.ScmObj* %current_45args47220, %struct.ScmObj** %stackaddr$prim48267, align 8
%stackaddr$prim48268 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47220)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48268, align 8
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%cpsprim40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %cpsprim40374, %struct.ScmObj** %stackaddr$prim48269, align 8
%stackaddr$makeclosure48270 = alloca %struct.ScmObj*, align 8
%fptrToInt48271 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44334 to i64
%ae44334 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48271)
store volatile %struct.ScmObj* %ae44334, %struct.ScmObj** %stackaddr$makeclosure48270, align 8
%ae44335 = call %struct.ScmObj* @const_init_int(i64 0)
%args47226$ae44334$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48272 = alloca %struct.ScmObj*, align 8
%args47226$ae44334$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40374, %struct.ScmObj* %args47226$ae44334$0)
store volatile %struct.ScmObj* %args47226$ae44334$1, %struct.ScmObj** %stackaddr$prim48272, align 8
%stackaddr$prim48273 = alloca %struct.ScmObj*, align 8
%args47226$ae44334$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44335, %struct.ScmObj* %args47226$ae44334$1)
store volatile %struct.ScmObj* %args47226$ae44334$2, %struct.ScmObj** %stackaddr$prim48273, align 8
%clofunc48274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44334)
musttail call tailcc void %clofunc48274(%struct.ScmObj* %ae44334, %struct.ScmObj* %args47226$ae44334$2)
ret void
}

define tailcc void @proc_clo$ae44334(%struct.ScmObj* %env$ae44334,%struct.ScmObj* %current_45args47222) {
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47222)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$prim48276 = alloca %struct.ScmObj*, align 8
%current_45args47223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47222)
store volatile %struct.ScmObj* %current_45args47223, %struct.ScmObj** %stackaddr$prim48276, align 8
%stackaddr$prim48277 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47223)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48277, align 8
%stackaddr$prim48278 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48278, align 8
%args47225$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48279 = alloca %struct.ScmObj*, align 8
%args47225$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47225$k$0)
store volatile %struct.ScmObj* %args47225$k$1, %struct.ScmObj** %stackaddr$prim48279, align 8
%clofunc48280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48280(%struct.ScmObj* %k, %struct.ScmObj* %args47225$k$1)
ret void
}

define tailcc void @proc_clo$ae43885(%struct.ScmObj* %env$ae43885,%struct.ScmObj* %current_45args47229) {
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47229)
store volatile %struct.ScmObj* %k40377, %struct.ScmObj** %stackaddr$prim48281, align 8
%stackaddr$prim48282 = alloca %struct.ScmObj*, align 8
%current_45args47230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47229)
store volatile %struct.ScmObj* %current_45args47230, %struct.ScmObj** %stackaddr$prim48282, align 8
%stackaddr$prim48283 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47230)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim48283, align 8
%stackaddr$prim48284 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim48284, align 8
%truthy$cmp48285 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40333)
%cmp$cmp48285 = icmp eq i64 %truthy$cmp48285, 1
br i1 %cmp$cmp48285, label %truebranch$cmp48285, label %falsebranch$cmp48285
truebranch$cmp48285:
%stackaddr$prim48286 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim48286, align 8
%ae43890 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim48287 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40334, %struct.ScmObj* %ae43890)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim48287, align 8
%truthy$cmp48288 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40335)
%cmp$cmp48288 = icmp eq i64 %truthy$cmp48288, 1
br i1 %cmp$cmp48288, label %truebranch$cmp48288, label %falsebranch$cmp48288
truebranch$cmp48288:
%ae43893 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48289 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43893)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim48289, align 8
%ae43895 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4389548290, i32 0, i32 0))
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%cpsprim40378 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40336, %struct.ScmObj* %ae43895)
store volatile %struct.ScmObj* %cpsprim40378, %struct.ScmObj** %stackaddr$prim48291, align 8
%ae43897 = call %struct.ScmObj* @const_init_int(i64 0)
%args47232$k40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48292 = alloca %struct.ScmObj*, align 8
%args47232$k40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40378, %struct.ScmObj* %args47232$k40377$0)
store volatile %struct.ScmObj* %args47232$k40377$1, %struct.ScmObj** %stackaddr$prim48292, align 8
%stackaddr$prim48293 = alloca %struct.ScmObj*, align 8
%args47232$k40377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43897, %struct.ScmObj* %args47232$k40377$1)
store volatile %struct.ScmObj* %args47232$k40377$2, %struct.ScmObj** %stackaddr$prim48293, align 8
%clofunc48294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40377)
musttail call tailcc void %clofunc48294(%struct.ScmObj* %k40377, %struct.ScmObj* %args47232$k40377$2)
ret void
falsebranch$cmp48288:
%ae43915 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43916 = call %struct.ScmObj* @const_init_false()
%args47233$k40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48295 = alloca %struct.ScmObj*, align 8
%args47233$k40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43916, %struct.ScmObj* %args47233$k40377$0)
store volatile %struct.ScmObj* %args47233$k40377$1, %struct.ScmObj** %stackaddr$prim48295, align 8
%stackaddr$prim48296 = alloca %struct.ScmObj*, align 8
%args47233$k40377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43915, %struct.ScmObj* %args47233$k40377$1)
store volatile %struct.ScmObj* %args47233$k40377$2, %struct.ScmObj** %stackaddr$prim48296, align 8
%clofunc48297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40377)
musttail call tailcc void %clofunc48297(%struct.ScmObj* %k40377, %struct.ScmObj* %args47233$k40377$2)
ret void
falsebranch$cmp48285:
%ae43937 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43938 = call %struct.ScmObj* @const_init_false()
%args47234$k40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48298 = alloca %struct.ScmObj*, align 8
%args47234$k40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43938, %struct.ScmObj* %args47234$k40377$0)
store volatile %struct.ScmObj* %args47234$k40377$1, %struct.ScmObj** %stackaddr$prim48298, align 8
%stackaddr$prim48299 = alloca %struct.ScmObj*, align 8
%args47234$k40377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43937, %struct.ScmObj* %args47234$k40377$1)
store volatile %struct.ScmObj* %args47234$k40377$2, %struct.ScmObj** %stackaddr$prim48299, align 8
%clofunc48300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40377)
musttail call tailcc void %clofunc48300(%struct.ScmObj* %k40377, %struct.ScmObj* %args47234$k40377$2)
ret void
}

define tailcc void @proc_clo$ae43859(%struct.ScmObj* %env$ae43859,%struct.ScmObj* %current_45args47236) {
%stackaddr$prim48301 = alloca %struct.ScmObj*, align 8
%k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47236)
store volatile %struct.ScmObj* %k40379, %struct.ScmObj** %stackaddr$prim48301, align 8
%stackaddr$prim48302 = alloca %struct.ScmObj*, align 8
%current_45args47237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47236)
store volatile %struct.ScmObj* %current_45args47237, %struct.ScmObj** %stackaddr$prim48302, align 8
%stackaddr$prim48303 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47237)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim48303, align 8
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim48305, align 8
%stackaddr$prim48306 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40331)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim48306, align 8
%stackaddr$prim48307 = alloca %struct.ScmObj*, align 8
%cpsprim40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %cpsprim40380, %struct.ScmObj** %stackaddr$prim48307, align 8
%ae43865 = call %struct.ScmObj* @const_init_int(i64 0)
%args47239$k40379$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%args47239$k40379$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40380, %struct.ScmObj* %args47239$k40379$0)
store volatile %struct.ScmObj* %args47239$k40379$1, %struct.ScmObj** %stackaddr$prim48308, align 8
%stackaddr$prim48309 = alloca %struct.ScmObj*, align 8
%args47239$k40379$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43865, %struct.ScmObj* %args47239$k40379$1)
store volatile %struct.ScmObj* %args47239$k40379$2, %struct.ScmObj** %stackaddr$prim48309, align 8
%clofunc48310 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40379)
musttail call tailcc void %clofunc48310(%struct.ScmObj* %k40379, %struct.ScmObj* %args47239$k40379$2)
ret void
}

define tailcc void @proc_clo$ae43835(%struct.ScmObj* %env$ae43835,%struct.ScmObj* %current_45args47241) {
%stackaddr$prim48311 = alloca %struct.ScmObj*, align 8
%k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47241)
store volatile %struct.ScmObj* %k40381, %struct.ScmObj** %stackaddr$prim48311, align 8
%stackaddr$prim48312 = alloca %struct.ScmObj*, align 8
%current_45args47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47241)
store volatile %struct.ScmObj* %current_45args47242, %struct.ScmObj** %stackaddr$prim48312, align 8
%stackaddr$prim48313 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47242)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim48313, align 8
%stackaddr$prim48314 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim48314, align 8
%stackaddr$prim48315 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim48315, align 8
%stackaddr$prim48316 = alloca %struct.ScmObj*, align 8
%cpsprim40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %cpsprim40382, %struct.ScmObj** %stackaddr$prim48316, align 8
%ae43840 = call %struct.ScmObj* @const_init_int(i64 0)
%args47244$k40381$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48317 = alloca %struct.ScmObj*, align 8
%args47244$k40381$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40382, %struct.ScmObj* %args47244$k40381$0)
store volatile %struct.ScmObj* %args47244$k40381$1, %struct.ScmObj** %stackaddr$prim48317, align 8
%stackaddr$prim48318 = alloca %struct.ScmObj*, align 8
%args47244$k40381$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43840, %struct.ScmObj* %args47244$k40381$1)
store volatile %struct.ScmObj* %args47244$k40381$2, %struct.ScmObj** %stackaddr$prim48318, align 8
%clofunc48319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40381)
musttail call tailcc void %clofunc48319(%struct.ScmObj* %k40381, %struct.ScmObj* %args47244$k40381$2)
ret void
}

define tailcc void @proc_clo$ae43813(%struct.ScmObj* %env$ae43813,%struct.ScmObj* %current_45args47246) {
%stackaddr$prim48320 = alloca %struct.ScmObj*, align 8
%k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47246)
store volatile %struct.ScmObj* %k40383, %struct.ScmObj** %stackaddr$prim48320, align 8
%stackaddr$prim48321 = alloca %struct.ScmObj*, align 8
%current_45args47247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47246)
store volatile %struct.ScmObj* %current_45args47247, %struct.ScmObj** %stackaddr$prim48321, align 8
%stackaddr$prim48322 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47247)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim48322, align 8
%stackaddr$prim48323 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim48323, align 8
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%cpsprim40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %cpsprim40384, %struct.ScmObj** %stackaddr$prim48324, align 8
%ae43817 = call %struct.ScmObj* @const_init_int(i64 0)
%args47249$k40383$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%args47249$k40383$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40384, %struct.ScmObj* %args47249$k40383$0)
store volatile %struct.ScmObj* %args47249$k40383$1, %struct.ScmObj** %stackaddr$prim48325, align 8
%stackaddr$prim48326 = alloca %struct.ScmObj*, align 8
%args47249$k40383$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43817, %struct.ScmObj* %args47249$k40383$1)
store volatile %struct.ScmObj* %args47249$k40383$2, %struct.ScmObj** %stackaddr$prim48326, align 8
%clofunc48327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40383)
musttail call tailcc void %clofunc48327(%struct.ScmObj* %k40383, %struct.ScmObj* %args47249$k40383$2)
ret void
}

define tailcc void @proc_clo$ae43793(%struct.ScmObj* %env$ae43793,%struct.ScmObj* %current_45args47251) {
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47251)
store volatile %struct.ScmObj* %k40385, %struct.ScmObj** %stackaddr$prim48328, align 8
%stackaddr$prim48329 = alloca %struct.ScmObj*, align 8
%current_45args47252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47251)
store volatile %struct.ScmObj* %current_45args47252, %struct.ScmObj** %stackaddr$prim48329, align 8
%stackaddr$prim48330 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47252)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim48330, align 8
%stackaddr$prim48331 = alloca %struct.ScmObj*, align 8
%cpsprim40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40386, %struct.ScmObj** %stackaddr$prim48331, align 8
%ae43796 = call %struct.ScmObj* @const_init_int(i64 0)
%args47254$k40385$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48332 = alloca %struct.ScmObj*, align 8
%args47254$k40385$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40386, %struct.ScmObj* %args47254$k40385$0)
store volatile %struct.ScmObj* %args47254$k40385$1, %struct.ScmObj** %stackaddr$prim48332, align 8
%stackaddr$prim48333 = alloca %struct.ScmObj*, align 8
%args47254$k40385$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43796, %struct.ScmObj* %args47254$k40385$1)
store volatile %struct.ScmObj* %args47254$k40385$2, %struct.ScmObj** %stackaddr$prim48333, align 8
%clofunc48334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40385)
musttail call tailcc void %clofunc48334(%struct.ScmObj* %k40385, %struct.ScmObj* %args47254$k40385$2)
ret void
}

define tailcc void @proc_clo$ae43695(%struct.ScmObj* %env$ae43695,%struct.ScmObj* %args4016840387) {
%stackaddr$env-ref48335 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43695, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48335
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840387)
store volatile %struct.ScmObj* %k40388, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840387)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim48337, align 8
%stackaddr$prim48338 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim48338, align 8
%truthy$cmp48339 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp48339 = icmp eq i64 %truthy$cmp48339, 1
br i1 %cmp$cmp48339, label %truebranch$cmp48339, label %falsebranch$cmp48339
truebranch$cmp48339:
%ae43701 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43702 = call %struct.ScmObj* @const_init_int(i64 1)
%args47256$k40388$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%args47256$k40388$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43702, %struct.ScmObj* %args47256$k40388$0)
store volatile %struct.ScmObj* %args47256$k40388$1, %struct.ScmObj** %stackaddr$prim48340, align 8
%stackaddr$prim48341 = alloca %struct.ScmObj*, align 8
%args47256$k40388$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43701, %struct.ScmObj* %args47256$k40388$1)
store volatile %struct.ScmObj* %args47256$k40388$2, %struct.ScmObj** %stackaddr$prim48341, align 8
%clofunc48342 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40388)
musttail call tailcc void %clofunc48342(%struct.ScmObj* %k40388, %struct.ScmObj* %args47256$k40388$2)
ret void
falsebranch$cmp48339:
%stackaddr$prim48343 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim48343, align 8
%stackaddr$prim48344 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim48344, align 8
%truthy$cmp48345 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40323)
%cmp$cmp48345 = icmp eq i64 %truthy$cmp48345, 1
br i1 %cmp$cmp48345, label %truebranch$cmp48345, label %falsebranch$cmp48345
truebranch$cmp48345:
%stackaddr$prim48346 = alloca %struct.ScmObj*, align 8
%cpsprim40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40389, %struct.ScmObj** %stackaddr$prim48346, align 8
%ae43714 = call %struct.ScmObj* @const_init_int(i64 0)
%args47257$k40388$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48347 = alloca %struct.ScmObj*, align 8
%args47257$k40388$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40389, %struct.ScmObj* %args47257$k40388$0)
store volatile %struct.ScmObj* %args47257$k40388$1, %struct.ScmObj** %stackaddr$prim48347, align 8
%stackaddr$prim48348 = alloca %struct.ScmObj*, align 8
%args47257$k40388$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43714, %struct.ScmObj* %args47257$k40388$1)
store volatile %struct.ScmObj* %args47257$k40388$2, %struct.ScmObj** %stackaddr$prim48348, align 8
%clofunc48349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40388)
musttail call tailcc void %clofunc48349(%struct.ScmObj* %k40388, %struct.ScmObj* %args47257$k40388$2)
ret void
falsebranch$cmp48345:
%stackaddr$makeclosure48350 = alloca %struct.ScmObj*, align 8
%fptrToInt48351 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43719 to i64
%ae43719 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48351)
store volatile %struct.ScmObj* %ae43719, %struct.ScmObj** %stackaddr$makeclosure48350, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43719, %struct.ScmObj* %args40168, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43719, %struct.ScmObj* %k40388, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43719, %struct.ScmObj* %_37foldl140107, i64 2)
%ae43720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48352 = alloca %struct.ScmObj*, align 8
%fptrToInt48353 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43721 to i64
%ae43721 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48353)
store volatile %struct.ScmObj* %ae43721, %struct.ScmObj** %stackaddr$makeclosure48352, align 8
%args47267$ae43719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48354 = alloca %struct.ScmObj*, align 8
%args47267$ae43719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43721, %struct.ScmObj* %args47267$ae43719$0)
store volatile %struct.ScmObj* %args47267$ae43719$1, %struct.ScmObj** %stackaddr$prim48354, align 8
%stackaddr$prim48355 = alloca %struct.ScmObj*, align 8
%args47267$ae43719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43720, %struct.ScmObj* %args47267$ae43719$1)
store volatile %struct.ScmObj* %args47267$ae43719$2, %struct.ScmObj** %stackaddr$prim48355, align 8
%clofunc48356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43719)
musttail call tailcc void %clofunc48356(%struct.ScmObj* %ae43719, %struct.ScmObj* %args47267$ae43719$2)
ret void
}

define tailcc void @proc_clo$ae43719(%struct.ScmObj* %env$ae43719,%struct.ScmObj* %current_45args47258) {
%stackaddr$env-ref48357 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43719, i64 0)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref48357
%stackaddr$env-ref48358 = alloca %struct.ScmObj*, align 8
%k40388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43719, i64 1)
store %struct.ScmObj* %k40388, %struct.ScmObj** %stackaddr$env-ref48358
%stackaddr$env-ref48359 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43719, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48359
%stackaddr$prim48360 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47258)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim48360, align 8
%stackaddr$prim48361 = alloca %struct.ScmObj*, align 8
%current_45args47259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47258)
store volatile %struct.ScmObj* %current_45args47259, %struct.ScmObj** %stackaddr$prim48361, align 8
%stackaddr$prim48362 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47259)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim48362, align 8
%stackaddr$prim48363 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim48363, align 8
%stackaddr$prim48364 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim48364, align 8
%args47261$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48365 = alloca %struct.ScmObj*, align 8
%args47261$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40326, %struct.ScmObj* %args47261$_37foldl140107$0)
store volatile %struct.ScmObj* %args47261$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48365, align 8
%stackaddr$prim48366 = alloca %struct.ScmObj*, align 8
%args47261$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40325, %struct.ScmObj* %args47261$_37foldl140107$1)
store volatile %struct.ScmObj* %args47261$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48366, align 8
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%args47261$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %args47261$_37foldl140107$2)
store volatile %struct.ScmObj* %args47261$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48367, align 8
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%args47261$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40388, %struct.ScmObj* %args47261$_37foldl140107$3)
store volatile %struct.ScmObj* %args47261$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48368, align 8
%clofunc48369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48369(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47261$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43721(%struct.ScmObj* %env$ae43721,%struct.ScmObj* %current_45args47262) {
%stackaddr$prim48370 = alloca %struct.ScmObj*, align 8
%k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47262)
store volatile %struct.ScmObj* %k40391, %struct.ScmObj** %stackaddr$prim48370, align 8
%stackaddr$prim48371 = alloca %struct.ScmObj*, align 8
%current_45args47263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47262)
store volatile %struct.ScmObj* %current_45args47263, %struct.ScmObj** %stackaddr$prim48371, align 8
%stackaddr$prim48372 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47263)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim48372, align 8
%stackaddr$prim48373 = alloca %struct.ScmObj*, align 8
%current_45args47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47263)
store volatile %struct.ScmObj* %current_45args47264, %struct.ScmObj** %stackaddr$prim48373, align 8
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47264)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim48374, align 8
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%cpsprim40392 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40392, %struct.ScmObj** %stackaddr$prim48375, align 8
%ae43725 = call %struct.ScmObj* @const_init_int(i64 0)
%args47266$k40391$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48376 = alloca %struct.ScmObj*, align 8
%args47266$k40391$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40392, %struct.ScmObj* %args47266$k40391$0)
store volatile %struct.ScmObj* %args47266$k40391$1, %struct.ScmObj** %stackaddr$prim48376, align 8
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%args47266$k40391$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43725, %struct.ScmObj* %args47266$k40391$1)
store volatile %struct.ScmObj* %args47266$k40391$2, %struct.ScmObj** %stackaddr$prim48377, align 8
%clofunc48378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40391)
musttail call tailcc void %clofunc48378(%struct.ScmObj* %k40391, %struct.ScmObj* %args47266$k40391$2)
ret void
}

define tailcc void @proc_clo$ae43291(%struct.ScmObj* %env$ae43291,%struct.ScmObj* %current_45args47269) {
%stackaddr$prim48379 = alloca %struct.ScmObj*, align 8
%k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47269)
store volatile %struct.ScmObj* %k40393, %struct.ScmObj** %stackaddr$prim48379, align 8
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%current_45args47270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47269)
store volatile %struct.ScmObj* %current_45args47270, %struct.ScmObj** %stackaddr$prim48380, align 8
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47270)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim48381, align 8
%stackaddr$prim48382 = alloca %struct.ScmObj*, align 8
%current_45args47271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47270)
store volatile %struct.ScmObj* %current_45args47271, %struct.ScmObj** %stackaddr$prim48382, align 8
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47271)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim48383, align 8
%ae43292 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43292, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim48384, align 8
%stackaddr$makeclosure48385 = alloca %struct.ScmObj*, align 8
%fptrToInt48386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43294 to i64
%ae43294 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48386)
store volatile %struct.ScmObj* %ae43294, %struct.ScmObj** %stackaddr$makeclosure48385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43294, %struct.ScmObj* %k40393, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43294, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43294, %struct.ScmObj* %v40173, i64 2)
%ae43295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48387 = alloca %struct.ScmObj*, align 8
%fptrToInt48388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43296 to i64
%ae43296 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48388)
store volatile %struct.ScmObj* %ae43296, %struct.ScmObj** %stackaddr$makeclosure48387, align 8
%args47293$ae43294$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48389 = alloca %struct.ScmObj*, align 8
%args47293$ae43294$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43296, %struct.ScmObj* %args47293$ae43294$0)
store volatile %struct.ScmObj* %args47293$ae43294$1, %struct.ScmObj** %stackaddr$prim48389, align 8
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%args47293$ae43294$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43295, %struct.ScmObj* %args47293$ae43294$1)
store volatile %struct.ScmObj* %args47293$ae43294$2, %struct.ScmObj** %stackaddr$prim48390, align 8
%clofunc48391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43294)
musttail call tailcc void %clofunc48391(%struct.ScmObj* %ae43294, %struct.ScmObj* %args47293$ae43294$2)
ret void
}

define tailcc void @proc_clo$ae43294(%struct.ScmObj* %env$ae43294,%struct.ScmObj* %current_45args47273) {
%stackaddr$env-ref48392 = alloca %struct.ScmObj*, align 8
%k40393 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43294, i64 0)
store %struct.ScmObj* %k40393, %struct.ScmObj** %stackaddr$env-ref48392
%stackaddr$env-ref48393 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43294, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48393
%stackaddr$env-ref48394 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43294, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48394
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47273)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim48395, align 8
%stackaddr$prim48396 = alloca %struct.ScmObj*, align 8
%current_45args47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47273)
store volatile %struct.ScmObj* %current_45args47274, %struct.ScmObj** %stackaddr$prim48396, align 8
%stackaddr$prim48397 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47274)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48397, align 8
%stackaddr$makeclosure48398 = alloca %struct.ScmObj*, align 8
%fptrToInt48399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43310 to i64
%ae43310 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48399)
store volatile %struct.ScmObj* %ae43310, %struct.ScmObj** %stackaddr$makeclosure48398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43310, %struct.ScmObj* %k40393, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43310, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43310, %struct.ScmObj* %v40173, i64 2)
%stackaddr$makeclosure48400 = alloca %struct.ScmObj*, align 8
%fptrToInt48401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43311 to i64
%ae43311 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48401)
store volatile %struct.ScmObj* %ae43311, %struct.ScmObj** %stackaddr$makeclosure48400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %k40393, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43311, %struct.ScmObj* %v40173, i64 2)
%args47288$anf_45bind40313$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48402 = alloca %struct.ScmObj*, align 8
%args47288$anf_45bind40313$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43311, %struct.ScmObj* %args47288$anf_45bind40313$0)
store volatile %struct.ScmObj* %args47288$anf_45bind40313$1, %struct.ScmObj** %stackaddr$prim48402, align 8
%stackaddr$prim48403 = alloca %struct.ScmObj*, align 8
%args47288$anf_45bind40313$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43310, %struct.ScmObj* %args47288$anf_45bind40313$1)
store volatile %struct.ScmObj* %args47288$anf_45bind40313$2, %struct.ScmObj** %stackaddr$prim48403, align 8
%clofunc48404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40313)
musttail call tailcc void %clofunc48404(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args47288$anf_45bind40313$2)
ret void
}

define tailcc void @proc_clo$ae43310(%struct.ScmObj* %env$ae43310,%struct.ScmObj* %current_45args47276) {
%stackaddr$env-ref48405 = alloca %struct.ScmObj*, align 8
%k40393 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43310, i64 0)
store %struct.ScmObj* %k40393, %struct.ScmObj** %stackaddr$env-ref48405
%stackaddr$env-ref48406 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43310, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48406
%stackaddr$env-ref48407 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43310, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48407
%stackaddr$prim48408 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47276)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim48408, align 8
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%current_45args47277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47276)
store volatile %struct.ScmObj* %current_45args47277, %struct.ScmObj** %stackaddr$prim48409, align 8
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47277)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim48410, align 8
%ae43419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43419)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim48411, align 8
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48412, align 8
%truthy$cmp48413 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40315)
%cmp$cmp48413 = icmp eq i64 %truthy$cmp48413, 1
br i1 %cmp$cmp48413, label %truebranch$cmp48413, label %falsebranch$cmp48413
truebranch$cmp48413:
%ae43423 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43424 = call %struct.ScmObj* @const_init_false()
%args47279$k40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48414 = alloca %struct.ScmObj*, align 8
%args47279$k40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43424, %struct.ScmObj* %args47279$k40393$0)
store volatile %struct.ScmObj* %args47279$k40393$1, %struct.ScmObj** %stackaddr$prim48414, align 8
%stackaddr$prim48415 = alloca %struct.ScmObj*, align 8
%args47279$k40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43423, %struct.ScmObj* %args47279$k40393$1)
store volatile %struct.ScmObj* %args47279$k40393$2, %struct.ScmObj** %stackaddr$prim48415, align 8
%clofunc48416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40393)
musttail call tailcc void %clofunc48416(%struct.ScmObj* %k40393, %struct.ScmObj* %args47279$k40393$2)
ret void
falsebranch$cmp48413:
%ae43432 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48417 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43432)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48417, align 8
%stackaddr$prim48418 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48418, align 8
%stackaddr$prim48419 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48419, align 8
%truthy$cmp48420 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp48420 = icmp eq i64 %truthy$cmp48420, 1
br i1 %cmp$cmp48420, label %truebranch$cmp48420, label %falsebranch$cmp48420
truebranch$cmp48420:
%ae43438 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%cpsprim40396 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43438)
store volatile %struct.ScmObj* %cpsprim40396, %struct.ScmObj** %stackaddr$prim48421, align 8
%ae43440 = call %struct.ScmObj* @const_init_int(i64 0)
%args47280$k40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%args47280$k40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40396, %struct.ScmObj* %args47280$k40393$0)
store volatile %struct.ScmObj* %args47280$k40393$1, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%args47280$k40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43440, %struct.ScmObj* %args47280$k40393$1)
store volatile %struct.ScmObj* %args47280$k40393$2, %struct.ScmObj** %stackaddr$prim48423, align 8
%clofunc48424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40393)
musttail call tailcc void %clofunc48424(%struct.ScmObj* %k40393, %struct.ScmObj* %args47280$k40393$2)
ret void
falsebranch$cmp48420:
%ae43451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43451)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48426, align 8
%ae43454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48427 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43454, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim48427, align 8
%args47281$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48428 = alloca %struct.ScmObj*, align 8
%args47281$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47281$cc40175$0)
store volatile %struct.ScmObj* %args47281$cc40175$1, %struct.ScmObj** %stackaddr$prim48428, align 8
%stackaddr$prim48429 = alloca %struct.ScmObj*, align 8
%args47281$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40393, %struct.ScmObj* %args47281$cc40175$1)
store volatile %struct.ScmObj* %args47281$cc40175$2, %struct.ScmObj** %stackaddr$prim48429, align 8
%clofunc48430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc48430(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47281$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43311(%struct.ScmObj* %env$ae43311,%struct.ScmObj* %current_45args47282) {
%stackaddr$env-ref48431 = alloca %struct.ScmObj*, align 8
%k40393 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 0)
store %struct.ScmObj* %k40393, %struct.ScmObj** %stackaddr$env-ref48431
%stackaddr$env-ref48432 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48432
%stackaddr$env-ref48433 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43311, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48433
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47282)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim48434, align 8
%stackaddr$prim48435 = alloca %struct.ScmObj*, align 8
%current_45args47283 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47282)
store volatile %struct.ScmObj* %current_45args47283, %struct.ScmObj** %stackaddr$prim48435, align 8
%stackaddr$prim48436 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47283)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim48436, align 8
%ae43313 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48437 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43313)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim48437, align 8
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48438, align 8
%truthy$cmp48439 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40315)
%cmp$cmp48439 = icmp eq i64 %truthy$cmp48439, 1
br i1 %cmp$cmp48439, label %truebranch$cmp48439, label %falsebranch$cmp48439
truebranch$cmp48439:
%ae43317 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43318 = call %struct.ScmObj* @const_init_false()
%args47285$k40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%args47285$k40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43318, %struct.ScmObj* %args47285$k40393$0)
store volatile %struct.ScmObj* %args47285$k40393$1, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%args47285$k40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43317, %struct.ScmObj* %args47285$k40393$1)
store volatile %struct.ScmObj* %args47285$k40393$2, %struct.ScmObj** %stackaddr$prim48441, align 8
%clofunc48442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40393)
musttail call tailcc void %clofunc48442(%struct.ScmObj* %k40393, %struct.ScmObj* %args47285$k40393$2)
ret void
falsebranch$cmp48439:
%ae43326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48443 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43326)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48443, align 8
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40317, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48445, align 8
%truthy$cmp48446 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp48446 = icmp eq i64 %truthy$cmp48446, 1
br i1 %cmp$cmp48446, label %truebranch$cmp48446, label %falsebranch$cmp48446
truebranch$cmp48446:
%ae43332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48447 = alloca %struct.ScmObj*, align 8
%cpsprim40396 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43332)
store volatile %struct.ScmObj* %cpsprim40396, %struct.ScmObj** %stackaddr$prim48447, align 8
%ae43334 = call %struct.ScmObj* @const_init_int(i64 0)
%args47286$k40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48448 = alloca %struct.ScmObj*, align 8
%args47286$k40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40396, %struct.ScmObj* %args47286$k40393$0)
store volatile %struct.ScmObj* %args47286$k40393$1, %struct.ScmObj** %stackaddr$prim48448, align 8
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%args47286$k40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43334, %struct.ScmObj* %args47286$k40393$1)
store volatile %struct.ScmObj* %args47286$k40393$2, %struct.ScmObj** %stackaddr$prim48449, align 8
%clofunc48450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40393)
musttail call tailcc void %clofunc48450(%struct.ScmObj* %k40393, %struct.ScmObj* %args47286$k40393$2)
ret void
falsebranch$cmp48446:
%ae43345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43345)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48451, align 8
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48452, align 8
%ae43348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48453 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43348, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim48453, align 8
%args47287$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%args47287$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47287$cc40175$0)
store volatile %struct.ScmObj* %args47287$cc40175$1, %struct.ScmObj** %stackaddr$prim48454, align 8
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%args47287$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40393, %struct.ScmObj* %args47287$cc40175$1)
store volatile %struct.ScmObj* %args47287$cc40175$2, %struct.ScmObj** %stackaddr$prim48455, align 8
%clofunc48456 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc48456(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47287$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43296(%struct.ScmObj* %env$ae43296,%struct.ScmObj* %current_45args47289) {
%stackaddr$prim48457 = alloca %struct.ScmObj*, align 8
%k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47289)
store volatile %struct.ScmObj* %k40397, %struct.ScmObj** %stackaddr$prim48457, align 8
%stackaddr$prim48458 = alloca %struct.ScmObj*, align 8
%current_45args47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47289)
store volatile %struct.ScmObj* %current_45args47290, %struct.ScmObj** %stackaddr$prim48458, align 8
%stackaddr$prim48459 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47290)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim48459, align 8
%args47292$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48460 = alloca %struct.ScmObj*, align 8
%args47292$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args47292$u40176$0)
store volatile %struct.ScmObj* %args47292$u40176$1, %struct.ScmObj** %stackaddr$prim48460, align 8
%stackaddr$prim48461 = alloca %struct.ScmObj*, align 8
%args47292$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40397, %struct.ScmObj* %args47292$u40176$1)
store volatile %struct.ScmObj* %args47292$u40176$2, %struct.ScmObj** %stackaddr$prim48461, align 8
%clofunc48462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc48462(%struct.ScmObj* %u40176, %struct.ScmObj* %args47292$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42755(%struct.ScmObj* %env$ae42755,%struct.ScmObj* %current_45args47295) {
%stackaddr$prim48463 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47295)
store volatile %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$prim48463, align 8
%stackaddr$prim48464 = alloca %struct.ScmObj*, align 8
%current_45args47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47295)
store volatile %struct.ScmObj* %current_45args47296, %struct.ScmObj** %stackaddr$prim48464, align 8
%stackaddr$prim48465 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47296)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim48465, align 8
%stackaddr$prim48466 = alloca %struct.ScmObj*, align 8
%current_45args47297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47296)
store volatile %struct.ScmObj* %current_45args47297, %struct.ScmObj** %stackaddr$prim48466, align 8
%stackaddr$prim48467 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47297)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim48467, align 8
%ae42756 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42756, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim48468, align 8
%ae42758 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48469 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42758, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim48469, align 8
%stackaddr$makeclosure48470 = alloca %struct.ScmObj*, align 8
%fptrToInt48471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42760 to i64
%ae42760 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48471)
store volatile %struct.ScmObj* %ae42760, %struct.ScmObj** %stackaddr$makeclosure48470, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42760, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42760, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42760, %struct.ScmObj* %k40398, i64 2)
%ae42761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48472 = alloca %struct.ScmObj*, align 8
%fptrToInt48473 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42762 to i64
%ae42762 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48473)
store volatile %struct.ScmObj* %ae42762, %struct.ScmObj** %stackaddr$makeclosure48472, align 8
%args47317$ae42760$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%args47317$ae42760$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42762, %struct.ScmObj* %args47317$ae42760$0)
store volatile %struct.ScmObj* %args47317$ae42760$1, %struct.ScmObj** %stackaddr$prim48474, align 8
%stackaddr$prim48475 = alloca %struct.ScmObj*, align 8
%args47317$ae42760$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42761, %struct.ScmObj* %args47317$ae42760$1)
store volatile %struct.ScmObj* %args47317$ae42760$2, %struct.ScmObj** %stackaddr$prim48475, align 8
%clofunc48476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42760)
musttail call tailcc void %clofunc48476(%struct.ScmObj* %ae42760, %struct.ScmObj* %args47317$ae42760$2)
ret void
}

define tailcc void @proc_clo$ae42760(%struct.ScmObj* %env$ae42760,%struct.ScmObj* %current_45args47299) {
%stackaddr$env-ref48477 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42760, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48477
%stackaddr$env-ref48478 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42760, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48478
%stackaddr$env-ref48479 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42760, i64 2)
store %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$env-ref48479
%stackaddr$prim48480 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47299)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim48480, align 8
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%current_45args47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47299)
store volatile %struct.ScmObj* %current_45args47300, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47300)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim48482, align 8
%stackaddr$makeclosure48483 = alloca %struct.ScmObj*, align 8
%fptrToInt48484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42776 to i64
%ae42776 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48484)
store volatile %struct.ScmObj* %ae42776, %struct.ScmObj** %stackaddr$makeclosure48483, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42776, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42776, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42776, %struct.ScmObj* %k40398, i64 2)
%stackaddr$makeclosure48485 = alloca %struct.ScmObj*, align 8
%fptrToInt48486 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42777 to i64
%ae42777 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48486)
store volatile %struct.ScmObj* %ae42777, %struct.ScmObj** %stackaddr$makeclosure48485, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42777, %struct.ScmObj* %k40398, i64 2)
%args47312$anf_45bind40306$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48487 = alloca %struct.ScmObj*, align 8
%args47312$anf_45bind40306$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42777, %struct.ScmObj* %args47312$anf_45bind40306$0)
store volatile %struct.ScmObj* %args47312$anf_45bind40306$1, %struct.ScmObj** %stackaddr$prim48487, align 8
%stackaddr$prim48488 = alloca %struct.ScmObj*, align 8
%args47312$anf_45bind40306$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42776, %struct.ScmObj* %args47312$anf_45bind40306$1)
store volatile %struct.ScmObj* %args47312$anf_45bind40306$2, %struct.ScmObj** %stackaddr$prim48488, align 8
%clofunc48489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40306)
musttail call tailcc void %clofunc48489(%struct.ScmObj* %anf_45bind40306, %struct.ScmObj* %args47312$anf_45bind40306$2)
ret void
}

define tailcc void @proc_clo$ae42776(%struct.ScmObj* %env$ae42776,%struct.ScmObj* %current_45args47302) {
%stackaddr$env-ref48490 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42776, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48490
%stackaddr$env-ref48491 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42776, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48491
%stackaddr$env-ref48492 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42776, i64 2)
store %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$env-ref48492
%stackaddr$prim48493 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47302)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim48493, align 8
%stackaddr$prim48494 = alloca %struct.ScmObj*, align 8
%current_45args47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47302)
store volatile %struct.ScmObj* %current_45args47303, %struct.ScmObj** %stackaddr$prim48494, align 8
%stackaddr$prim48495 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47303)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48495, align 8
%ae42919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48496 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42919)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim48496, align 8
%ae42920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42920, %struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim48497, align 8
%truthy$cmp48498 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp48498 = icmp eq i64 %truthy$cmp48498, 1
br i1 %cmp$cmp48498, label %truebranch$cmp48498, label %falsebranch$cmp48498
truebranch$cmp48498:
%ae42924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48499 = alloca %struct.ScmObj*, align 8
%cpsprim40401 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42924)
store volatile %struct.ScmObj* %cpsprim40401, %struct.ScmObj** %stackaddr$prim48499, align 8
%ae42926 = call %struct.ScmObj* @const_init_int(i64 0)
%args47305$k40398$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48500 = alloca %struct.ScmObj*, align 8
%args47305$k40398$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40401, %struct.ScmObj* %args47305$k40398$0)
store volatile %struct.ScmObj* %args47305$k40398$1, %struct.ScmObj** %stackaddr$prim48500, align 8
%stackaddr$prim48501 = alloca %struct.ScmObj*, align 8
%args47305$k40398$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42926, %struct.ScmObj* %args47305$k40398$1)
store volatile %struct.ScmObj* %args47305$k40398$2, %struct.ScmObj** %stackaddr$prim48501, align 8
%clofunc48502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40398)
musttail call tailcc void %clofunc48502(%struct.ScmObj* %k40398, %struct.ScmObj* %args47305$k40398$2)
ret void
falsebranch$cmp48498:
%ae42937 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42937)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48504, align 8
%ae42940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48505 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42940, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48505, align 8
%ae42943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48506 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42943)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48506, align 8
%ae42945 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48507 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %ae42945)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48507, align 8
%ae42947 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48508 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42947, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48508, align 8
%args47306$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48509 = alloca %struct.ScmObj*, align 8
%args47306$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47306$cc40183$0)
store volatile %struct.ScmObj* %args47306$cc40183$1, %struct.ScmObj** %stackaddr$prim48509, align 8
%stackaddr$prim48510 = alloca %struct.ScmObj*, align 8
%args47306$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40398, %struct.ScmObj* %args47306$cc40183$1)
store volatile %struct.ScmObj* %args47306$cc40183$2, %struct.ScmObj** %stackaddr$prim48510, align 8
%clofunc48511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48511(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47306$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42777(%struct.ScmObj* %env$ae42777,%struct.ScmObj* %current_45args47307) {
%stackaddr$env-ref48512 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48512
%stackaddr$env-ref48513 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48513
%stackaddr$env-ref48514 = alloca %struct.ScmObj*, align 8
%k40398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42777, i64 2)
store %struct.ScmObj* %k40398, %struct.ScmObj** %stackaddr$env-ref48514
%stackaddr$prim48515 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47307)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim48515, align 8
%stackaddr$prim48516 = alloca %struct.ScmObj*, align 8
%current_45args47308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47307)
store volatile %struct.ScmObj* %current_45args47308, %struct.ScmObj** %stackaddr$prim48516, align 8
%stackaddr$prim48517 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47308)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48517, align 8
%ae42779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42779)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim48518, align 8
%ae42780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42780, %struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim48519, align 8
%truthy$cmp48520 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40308)
%cmp$cmp48520 = icmp eq i64 %truthy$cmp48520, 1
br i1 %cmp$cmp48520, label %truebranch$cmp48520, label %falsebranch$cmp48520
truebranch$cmp48520:
%ae42784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48521 = alloca %struct.ScmObj*, align 8
%cpsprim40401 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42784)
store volatile %struct.ScmObj* %cpsprim40401, %struct.ScmObj** %stackaddr$prim48521, align 8
%ae42786 = call %struct.ScmObj* @const_init_int(i64 0)
%args47310$k40398$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48522 = alloca %struct.ScmObj*, align 8
%args47310$k40398$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40401, %struct.ScmObj* %args47310$k40398$0)
store volatile %struct.ScmObj* %args47310$k40398$1, %struct.ScmObj** %stackaddr$prim48522, align 8
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%args47310$k40398$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42786, %struct.ScmObj* %args47310$k40398$1)
store volatile %struct.ScmObj* %args47310$k40398$2, %struct.ScmObj** %stackaddr$prim48523, align 8
%clofunc48524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40398)
musttail call tailcc void %clofunc48524(%struct.ScmObj* %k40398, %struct.ScmObj* %args47310$k40398$2)
ret void
falsebranch$cmp48520:
%ae42797 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42797)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48525, align 8
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48526, align 8
%ae42800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42800, %struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48527, align 8
%ae42803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48528 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42803)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48528, align 8
%ae42805 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48529 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %ae42805)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48529, align 8
%ae42807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48530 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42807, %struct.ScmObj* %anf_45bind40312)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48530, align 8
%args47311$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%args47311$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47311$cc40183$0)
store volatile %struct.ScmObj* %args47311$cc40183$1, %struct.ScmObj** %stackaddr$prim48531, align 8
%stackaddr$prim48532 = alloca %struct.ScmObj*, align 8
%args47311$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40398, %struct.ScmObj* %args47311$cc40183$1)
store volatile %struct.ScmObj* %args47311$cc40183$2, %struct.ScmObj** %stackaddr$prim48532, align 8
%clofunc48533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48533(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47311$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42762(%struct.ScmObj* %env$ae42762,%struct.ScmObj* %current_45args47313) {
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47313)
store volatile %struct.ScmObj* %k40402, %struct.ScmObj** %stackaddr$prim48534, align 8
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%current_45args47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47313)
store volatile %struct.ScmObj* %current_45args47314, %struct.ScmObj** %stackaddr$prim48535, align 8
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47314)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim48536, align 8
%args47316$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48537 = alloca %struct.ScmObj*, align 8
%args47316$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args47316$u40184$0)
store volatile %struct.ScmObj* %args47316$u40184$1, %struct.ScmObj** %stackaddr$prim48537, align 8
%stackaddr$prim48538 = alloca %struct.ScmObj*, align 8
%args47316$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40402, %struct.ScmObj* %args47316$u40184$1)
store volatile %struct.ScmObj* %args47316$u40184$2, %struct.ScmObj** %stackaddr$prim48538, align 8
%clofunc48539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc48539(%struct.ScmObj* %u40184, %struct.ScmObj* %args47316$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42339(%struct.ScmObj* %env$ae42339,%struct.ScmObj* %current_45args47319) {
%stackaddr$prim48540 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47319)
store volatile %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$prim48540, align 8
%stackaddr$prim48541 = alloca %struct.ScmObj*, align 8
%current_45args47320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47319)
store volatile %struct.ScmObj* %current_45args47320, %struct.ScmObj** %stackaddr$prim48541, align 8
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47320)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim48542, align 8
%ae42340 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42340, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim48543, align 8
%stackaddr$makeclosure48544 = alloca %struct.ScmObj*, align 8
%fptrToInt48545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42342 to i64
%ae42342 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48545)
store volatile %struct.ScmObj* %ae42342, %struct.ScmObj** %stackaddr$makeclosure48544, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42342, %struct.ScmObj* %k40403, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42342, %struct.ScmObj* %a40189, i64 1)
%ae42343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48546 = alloca %struct.ScmObj*, align 8
%fptrToInt48547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42344 to i64
%ae42344 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48547)
store volatile %struct.ScmObj* %ae42344, %struct.ScmObj** %stackaddr$makeclosure48546, align 8
%args47342$ae42342$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%args47342$ae42342$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42344, %struct.ScmObj* %args47342$ae42342$0)
store volatile %struct.ScmObj* %args47342$ae42342$1, %struct.ScmObj** %stackaddr$prim48548, align 8
%stackaddr$prim48549 = alloca %struct.ScmObj*, align 8
%args47342$ae42342$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42343, %struct.ScmObj* %args47342$ae42342$1)
store volatile %struct.ScmObj* %args47342$ae42342$2, %struct.ScmObj** %stackaddr$prim48549, align 8
%clofunc48550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42342)
musttail call tailcc void %clofunc48550(%struct.ScmObj* %ae42342, %struct.ScmObj* %args47342$ae42342$2)
ret void
}

define tailcc void @proc_clo$ae42342(%struct.ScmObj* %env$ae42342,%struct.ScmObj* %current_45args47322) {
%stackaddr$env-ref48551 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42342, i64 0)
store %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$env-ref48551
%stackaddr$env-ref48552 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42342, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48552
%stackaddr$prim48553 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47322)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim48553, align 8
%stackaddr$prim48554 = alloca %struct.ScmObj*, align 8
%current_45args47323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47322)
store volatile %struct.ScmObj* %current_45args47323, %struct.ScmObj** %stackaddr$prim48554, align 8
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47323)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim48555, align 8
%stackaddr$makeclosure48556 = alloca %struct.ScmObj*, align 8
%fptrToInt48557 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42361 to i64
%ae42361 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48557)
store volatile %struct.ScmObj* %ae42361, %struct.ScmObj** %stackaddr$makeclosure48556, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42361, %struct.ScmObj* %k40403, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42361, %struct.ScmObj* %a40189, i64 1)
%stackaddr$makeclosure48558 = alloca %struct.ScmObj*, align 8
%fptrToInt48559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42362 to i64
%ae42362 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48559)
store volatile %struct.ScmObj* %ae42362, %struct.ScmObj** %stackaddr$makeclosure48558, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42362, %struct.ScmObj* %k40403, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42362, %struct.ScmObj* %a40189, i64 1)
%args47337$anf_45bind40298$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48560 = alloca %struct.ScmObj*, align 8
%args47337$anf_45bind40298$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42362, %struct.ScmObj* %args47337$anf_45bind40298$0)
store volatile %struct.ScmObj* %args47337$anf_45bind40298$1, %struct.ScmObj** %stackaddr$prim48560, align 8
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%args47337$anf_45bind40298$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42361, %struct.ScmObj* %args47337$anf_45bind40298$1)
store volatile %struct.ScmObj* %args47337$anf_45bind40298$2, %struct.ScmObj** %stackaddr$prim48561, align 8
%clofunc48562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40298)
musttail call tailcc void %clofunc48562(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %args47337$anf_45bind40298$2)
ret void
}

define tailcc void @proc_clo$ae42361(%struct.ScmObj* %env$ae42361,%struct.ScmObj* %current_45args47325) {
%stackaddr$env-ref48563 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42361, i64 0)
store %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$env-ref48563
%stackaddr$env-ref48564 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42361, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48564
%stackaddr$prim48565 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47325)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim48565, align 8
%stackaddr$prim48566 = alloca %struct.ScmObj*, align 8
%current_45args47326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47325)
store volatile %struct.ScmObj* %current_45args47326, %struct.ScmObj** %stackaddr$prim48566, align 8
%stackaddr$prim48567 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47326)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48567, align 8
%ae42477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48568 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42477)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim48568, align 8
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40299)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim48569, align 8
%truthy$cmp48570 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40300)
%cmp$cmp48570 = icmp eq i64 %truthy$cmp48570, 1
br i1 %cmp$cmp48570, label %truebranch$cmp48570, label %falsebranch$cmp48570
truebranch$cmp48570:
%ae42481 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42482 = call %struct.ScmObj* @const_init_true()
%args47328$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%args47328$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42482, %struct.ScmObj* %args47328$k40403$0)
store volatile %struct.ScmObj* %args47328$k40403$1, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%args47328$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42481, %struct.ScmObj* %args47328$k40403$1)
store volatile %struct.ScmObj* %args47328$k40403$2, %struct.ScmObj** %stackaddr$prim48572, align 8
%clofunc48573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc48573(%struct.ScmObj* %k40403, %struct.ScmObj* %args47328$k40403$2)
ret void
falsebranch$cmp48570:
%ae42490 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42490)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim48575, align 8
%truthy$cmp48576 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40302)
%cmp$cmp48576 = icmp eq i64 %truthy$cmp48576, 1
br i1 %cmp$cmp48576, label %truebranch$cmp48576, label %falsebranch$cmp48576
truebranch$cmp48576:
%ae42494 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48577 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42494)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim48577, align 8
%stackaddr$prim48578 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48578, align 8
%ae42497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48579 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42497)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim48579, align 8
%stackaddr$prim48580 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim48580, align 8
%ae42500 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48581 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42500, %struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48581, align 8
%args47329$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48582 = alloca %struct.ScmObj*, align 8
%args47329$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47329$cc40190$0)
store volatile %struct.ScmObj* %args47329$cc40190$1, %struct.ScmObj** %stackaddr$prim48582, align 8
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%args47329$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40403, %struct.ScmObj* %args47329$cc40190$1)
store volatile %struct.ScmObj* %args47329$cc40190$2, %struct.ScmObj** %stackaddr$prim48583, align 8
%clofunc48584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48584(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47329$cc40190$2)
ret void
falsebranch$cmp48576:
%ae42533 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42534 = call %struct.ScmObj* @const_init_false()
%args47330$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48585 = alloca %struct.ScmObj*, align 8
%args47330$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42534, %struct.ScmObj* %args47330$k40403$0)
store volatile %struct.ScmObj* %args47330$k40403$1, %struct.ScmObj** %stackaddr$prim48585, align 8
%stackaddr$prim48586 = alloca %struct.ScmObj*, align 8
%args47330$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42533, %struct.ScmObj* %args47330$k40403$1)
store volatile %struct.ScmObj* %args47330$k40403$2, %struct.ScmObj** %stackaddr$prim48586, align 8
%clofunc48587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc48587(%struct.ScmObj* %k40403, %struct.ScmObj* %args47330$k40403$2)
ret void
}

define tailcc void @proc_clo$ae42362(%struct.ScmObj* %env$ae42362,%struct.ScmObj* %current_45args47331) {
%stackaddr$env-ref48588 = alloca %struct.ScmObj*, align 8
%k40403 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42362, i64 0)
store %struct.ScmObj* %k40403, %struct.ScmObj** %stackaddr$env-ref48588
%stackaddr$env-ref48589 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42362, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48589
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47331)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$prim48591 = alloca %struct.ScmObj*, align 8
%current_45args47332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47331)
store volatile %struct.ScmObj* %current_45args47332, %struct.ScmObj** %stackaddr$prim48591, align 8
%stackaddr$prim48592 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47332)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48592, align 8
%ae42364 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48593 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42364)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim48593, align 8
%stackaddr$prim48594 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40299)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim48594, align 8
%truthy$cmp48595 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40300)
%cmp$cmp48595 = icmp eq i64 %truthy$cmp48595, 1
br i1 %cmp$cmp48595, label %truebranch$cmp48595, label %falsebranch$cmp48595
truebranch$cmp48595:
%ae42368 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42369 = call %struct.ScmObj* @const_init_true()
%args47334$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%args47334$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42369, %struct.ScmObj* %args47334$k40403$0)
store volatile %struct.ScmObj* %args47334$k40403$1, %struct.ScmObj** %stackaddr$prim48596, align 8
%stackaddr$prim48597 = alloca %struct.ScmObj*, align 8
%args47334$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42368, %struct.ScmObj* %args47334$k40403$1)
store volatile %struct.ScmObj* %args47334$k40403$2, %struct.ScmObj** %stackaddr$prim48597, align 8
%clofunc48598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc48598(%struct.ScmObj* %k40403, %struct.ScmObj* %args47334$k40403$2)
ret void
falsebranch$cmp48595:
%ae42377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48599 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42377)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim48599, align 8
%stackaddr$prim48600 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim48600, align 8
%truthy$cmp48601 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40302)
%cmp$cmp48601 = icmp eq i64 %truthy$cmp48601, 1
br i1 %cmp$cmp48601, label %truebranch$cmp48601, label %falsebranch$cmp48601
truebranch$cmp48601:
%ae42381 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48602 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42381)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim48602, align 8
%stackaddr$prim48603 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48603, align 8
%ae42384 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48604 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42384)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim48604, align 8
%stackaddr$prim48605 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim48605, align 8
%ae42387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48606 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42387, %struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48606, align 8
%args47335$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%args47335$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47335$cc40190$0)
store volatile %struct.ScmObj* %args47335$cc40190$1, %struct.ScmObj** %stackaddr$prim48607, align 8
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%args47335$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40403, %struct.ScmObj* %args47335$cc40190$1)
store volatile %struct.ScmObj* %args47335$cc40190$2, %struct.ScmObj** %stackaddr$prim48608, align 8
%clofunc48609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48609(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47335$cc40190$2)
ret void
falsebranch$cmp48601:
%ae42420 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42421 = call %struct.ScmObj* @const_init_false()
%args47336$k40403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48610 = alloca %struct.ScmObj*, align 8
%args47336$k40403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42421, %struct.ScmObj* %args47336$k40403$0)
store volatile %struct.ScmObj* %args47336$k40403$1, %struct.ScmObj** %stackaddr$prim48610, align 8
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%args47336$k40403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42420, %struct.ScmObj* %args47336$k40403$1)
store volatile %struct.ScmObj* %args47336$k40403$2, %struct.ScmObj** %stackaddr$prim48611, align 8
%clofunc48612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40403)
musttail call tailcc void %clofunc48612(%struct.ScmObj* %k40403, %struct.ScmObj* %args47336$k40403$2)
ret void
}

define tailcc void @proc_clo$ae42344(%struct.ScmObj* %env$ae42344,%struct.ScmObj* %current_45args47338) {
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %k40406, %struct.ScmObj** %stackaddr$prim48613, align 8
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%current_45args47339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %current_45args47339, %struct.ScmObj** %stackaddr$prim48614, align 8
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47339)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim48615, align 8
%ae42346 = call %struct.ScmObj* @const_init_int(i64 0)
%args47341$k40406$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%args47341$k40406$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args47341$k40406$0)
store volatile %struct.ScmObj* %args47341$k40406$1, %struct.ScmObj** %stackaddr$prim48616, align 8
%stackaddr$prim48617 = alloca %struct.ScmObj*, align 8
%args47341$k40406$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42346, %struct.ScmObj* %args47341$k40406$1)
store volatile %struct.ScmObj* %args47341$k40406$2, %struct.ScmObj** %stackaddr$prim48617, align 8
%clofunc48618 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40406)
musttail call tailcc void %clofunc48618(%struct.ScmObj* %k40406, %struct.ScmObj* %args47341$k40406$2)
ret void
}

define tailcc void @proc_clo$ae42267(%struct.ScmObj* %env$ae42267,%struct.ScmObj* %current_45args47344) {
%stackaddr$env-ref48619 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42267, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref48619
%stackaddr$prim48620 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47344)
store volatile %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$prim48620, align 8
%stackaddr$prim48621 = alloca %struct.ScmObj*, align 8
%current_45args47345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47344)
store volatile %struct.ScmObj* %current_45args47345, %struct.ScmObj** %stackaddr$prim48621, align 8
%stackaddr$prim48622 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47345)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim48622, align 8
%stackaddr$prim48623 = alloca %struct.ScmObj*, align 8
%current_45args47346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47345)
store volatile %struct.ScmObj* %current_45args47346, %struct.ScmObj** %stackaddr$prim48623, align 8
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47346)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim48624, align 8
%stackaddr$prim48625 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim48625, align 8
%truthy$cmp48626 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40292)
%cmp$cmp48626 = icmp eq i64 %truthy$cmp48626, 1
br i1 %cmp$cmp48626, label %truebranch$cmp48626, label %falsebranch$cmp48626
truebranch$cmp48626:
%ae42271 = call %struct.ScmObj* @const_init_int(i64 0)
%args47348$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48627 = alloca %struct.ScmObj*, align 8
%args47348$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args47348$k40407$0)
store volatile %struct.ScmObj* %args47348$k40407$1, %struct.ScmObj** %stackaddr$prim48627, align 8
%stackaddr$prim48628 = alloca %struct.ScmObj*, align 8
%args47348$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42271, %struct.ScmObj* %args47348$k40407$1)
store volatile %struct.ScmObj* %args47348$k40407$2, %struct.ScmObj** %stackaddr$prim48628, align 8
%clofunc48629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc48629(%struct.ScmObj* %k40407, %struct.ScmObj* %args47348$k40407$2)
ret void
falsebranch$cmp48626:
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim48630, align 8
%ae42278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42278)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim48631, align 8
%stackaddr$prim48632 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim48632, align 8
%stackaddr$makeclosure48633 = alloca %struct.ScmObj*, align 8
%fptrToInt48634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42281 to i64
%ae42281 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48634)
store volatile %struct.ScmObj* %ae42281, %struct.ScmObj** %stackaddr$makeclosure48633, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42281, %struct.ScmObj* %k40407, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42281, %struct.ScmObj* %anf_45bind40293, i64 1)
%args47353$anf_45bind40294$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%args47353$anf_45bind40294$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args47353$anf_45bind40294$0)
store volatile %struct.ScmObj* %args47353$anf_45bind40294$1, %struct.ScmObj** %stackaddr$prim48635, align 8
%stackaddr$prim48636 = alloca %struct.ScmObj*, align 8
%args47353$anf_45bind40294$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args47353$anf_45bind40294$1)
store volatile %struct.ScmObj* %args47353$anf_45bind40294$2, %struct.ScmObj** %stackaddr$prim48636, align 8
%stackaddr$prim48637 = alloca %struct.ScmObj*, align 8
%args47353$anf_45bind40294$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args47353$anf_45bind40294$2)
store volatile %struct.ScmObj* %args47353$anf_45bind40294$3, %struct.ScmObj** %stackaddr$prim48637, align 8
%clofunc48638 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40294)
musttail call tailcc void %clofunc48638(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %args47353$anf_45bind40294$3)
ret void
}

define tailcc void @proc_clo$ae42281(%struct.ScmObj* %env$ae42281,%struct.ScmObj* %current_45args47349) {
%stackaddr$env-ref48639 = alloca %struct.ScmObj*, align 8
%k40407 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42281, i64 0)
store %struct.ScmObj* %k40407, %struct.ScmObj** %stackaddr$env-ref48639
%stackaddr$env-ref48640 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42281, i64 1)
store %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$env-ref48640
%stackaddr$prim48641 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47349)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim48641, align 8
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%current_45args47350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47349)
store volatile %struct.ScmObj* %current_45args47350, %struct.ScmObj** %stackaddr$prim48642, align 8
%stackaddr$prim48643 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47350)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim48643, align 8
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%cpsprim40409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsprim40409, %struct.ScmObj** %stackaddr$prim48644, align 8
%ae42287 = call %struct.ScmObj* @const_init_int(i64 0)
%args47352$k40407$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%args47352$k40407$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40409, %struct.ScmObj* %args47352$k40407$0)
store volatile %struct.ScmObj* %args47352$k40407$1, %struct.ScmObj** %stackaddr$prim48645, align 8
%stackaddr$prim48646 = alloca %struct.ScmObj*, align 8
%args47352$k40407$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42287, %struct.ScmObj* %args47352$k40407$1)
store volatile %struct.ScmObj* %args47352$k40407$2, %struct.ScmObj** %stackaddr$prim48646, align 8
%clofunc48647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40407)
musttail call tailcc void %clofunc48647(%struct.ScmObj* %k40407, %struct.ScmObj* %args47352$k40407$2)
ret void
}

define tailcc void @proc_clo$ae42241(%struct.ScmObj* %env$ae42241,%struct.ScmObj* %current_45args47355) {
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47355)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim48648, align 8
%stackaddr$prim48649 = alloca %struct.ScmObj*, align 8
%current_45args47356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47355)
store volatile %struct.ScmObj* %current_45args47356, %struct.ScmObj** %stackaddr$prim48649, align 8
%stackaddr$prim48650 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47356)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim48650, align 8
%stackaddr$prim48651 = alloca %struct.ScmObj*, align 8
%current_45args47357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47356)
store volatile %struct.ScmObj* %current_45args47357, %struct.ScmObj** %stackaddr$prim48651, align 8
%stackaddr$prim48652 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47357)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim48652, align 8
%stackaddr$prim48653 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim48653, align 8
%stackaddr$prim48654 = alloca %struct.ScmObj*, align 8
%cpsprim40411 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40291)
store volatile %struct.ScmObj* %cpsprim40411, %struct.ScmObj** %stackaddr$prim48654, align 8
%ae42246 = call %struct.ScmObj* @const_init_int(i64 0)
%args47359$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48655 = alloca %struct.ScmObj*, align 8
%args47359$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40411, %struct.ScmObj* %args47359$k40410$0)
store volatile %struct.ScmObj* %args47359$k40410$1, %struct.ScmObj** %stackaddr$prim48655, align 8
%stackaddr$prim48656 = alloca %struct.ScmObj*, align 8
%args47359$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42246, %struct.ScmObj* %args47359$k40410$1)
store volatile %struct.ScmObj* %args47359$k40410$2, %struct.ScmObj** %stackaddr$prim48656, align 8
%clofunc48657 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc48657(%struct.ScmObj* %k40410, %struct.ScmObj* %args47359$k40410$2)
ret void
}

define tailcc void @proc_clo$ae42217(%struct.ScmObj* %env$ae42217,%struct.ScmObj* %current_45args47361) {
%stackaddr$prim48658 = alloca %struct.ScmObj*, align 8
%k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47361)
store volatile %struct.ScmObj* %k40412, %struct.ScmObj** %stackaddr$prim48658, align 8
%stackaddr$prim48659 = alloca %struct.ScmObj*, align 8
%current_45args47362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47361)
store volatile %struct.ScmObj* %current_45args47362, %struct.ScmObj** %stackaddr$prim48659, align 8
%stackaddr$prim48660 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim48660, align 8
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%current_45args47363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %current_45args47363, %struct.ScmObj** %stackaddr$prim48661, align 8
%stackaddr$prim48662 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47363)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim48662, align 8
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48663, align 8
%stackaddr$prim48664 = alloca %struct.ScmObj*, align 8
%cpsprim40413 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40290)
store volatile %struct.ScmObj* %cpsprim40413, %struct.ScmObj** %stackaddr$prim48664, align 8
%ae42222 = call %struct.ScmObj* @const_init_int(i64 0)
%args47365$k40412$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48665 = alloca %struct.ScmObj*, align 8
%args47365$k40412$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40413, %struct.ScmObj* %args47365$k40412$0)
store volatile %struct.ScmObj* %args47365$k40412$1, %struct.ScmObj** %stackaddr$prim48665, align 8
%stackaddr$prim48666 = alloca %struct.ScmObj*, align 8
%args47365$k40412$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42222, %struct.ScmObj* %args47365$k40412$1)
store volatile %struct.ScmObj* %args47365$k40412$2, %struct.ScmObj** %stackaddr$prim48666, align 8
%clofunc48667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40412)
musttail call tailcc void %clofunc48667(%struct.ScmObj* %k40412, %struct.ScmObj* %args47365$k40412$2)
ret void
}

define tailcc void @proc_clo$ae41823(%struct.ScmObj* %env$ae41823,%struct.ScmObj* %current_45args47368) {
%stackaddr$env-ref48668 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41823, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48668
%stackaddr$env-ref48669 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41823, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48669
%stackaddr$env-ref48670 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41823, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48670
%stackaddr$prim48671 = alloca %struct.ScmObj*, align 8
%k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47368)
store volatile %struct.ScmObj* %k40414, %struct.ScmObj** %stackaddr$prim48671, align 8
%stackaddr$prim48672 = alloca %struct.ScmObj*, align 8
%current_45args47369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47368)
store volatile %struct.ScmObj* %current_45args47369, %struct.ScmObj** %stackaddr$prim48672, align 8
%stackaddr$prim48673 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47369)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim48673, align 8
%ae41825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48674 = alloca %struct.ScmObj*, align 8
%fptrToInt48675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41826 to i64
%ae41826 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48675)
store volatile %struct.ScmObj* %ae41826, %struct.ScmObj** %stackaddr$makeclosure48674, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41826, %struct.ScmObj* %_37map140154, i64 3)
%args47426$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%args47426$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41826, %struct.ScmObj* %args47426$k40414$0)
store volatile %struct.ScmObj* %args47426$k40414$1, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%args47426$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41825, %struct.ScmObj* %args47426$k40414$1)
store volatile %struct.ScmObj* %args47426$k40414$2, %struct.ScmObj** %stackaddr$prim48677, align 8
%clofunc48678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc48678(%struct.ScmObj* %k40414, %struct.ScmObj* %args47426$k40414$2)
ret void
}

define tailcc void @proc_clo$ae41826(%struct.ScmObj* %env$ae41826,%struct.ScmObj* %args4020740415) {
%stackaddr$env-ref48679 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48679
%stackaddr$env-ref48680 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48680
%stackaddr$env-ref48681 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48681
%stackaddr$env-ref48682 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41826, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48682
%stackaddr$prim48683 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740415)
store volatile %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$prim48683, align 8
%stackaddr$prim48684 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740415)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim48684, align 8
%stackaddr$prim48685 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim48685, align 8
%stackaddr$prim48686 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim48686, align 8
%stackaddr$prim48687 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim48687, align 8
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim48688, align 8
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40279)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim48689, align 8
%stackaddr$makeclosure48690 = alloca %struct.ScmObj*, align 8
%fptrToInt48691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41834 to i64
%ae41834 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48691)
store volatile %struct.ScmObj* %ae41834, %struct.ScmObj** %stackaddr$makeclosure48690, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %k40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41834, %struct.ScmObj* %_37map140154, i64 7)
%ae41835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48692 = alloca %struct.ScmObj*, align 8
%fptrToInt48693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41836 to i64
%ae41836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48693)
store volatile %struct.ScmObj* %ae41836, %struct.ScmObj** %stackaddr$makeclosure48692, align 8
%args47425$ae41834$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48694 = alloca %struct.ScmObj*, align 8
%args47425$ae41834$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41836, %struct.ScmObj* %args47425$ae41834$0)
store volatile %struct.ScmObj* %args47425$ae41834$1, %struct.ScmObj** %stackaddr$prim48694, align 8
%stackaddr$prim48695 = alloca %struct.ScmObj*, align 8
%args47425$ae41834$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41835, %struct.ScmObj* %args47425$ae41834$1)
store volatile %struct.ScmObj* %args47425$ae41834$2, %struct.ScmObj** %stackaddr$prim48695, align 8
%clofunc48696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41834)
musttail call tailcc void %clofunc48696(%struct.ScmObj* %ae41834, %struct.ScmObj* %args47425$ae41834$2)
ret void
}

define tailcc void @proc_clo$ae41834(%struct.ScmObj* %env$ae41834,%struct.ScmObj* %current_45args47371) {
%stackaddr$env-ref48697 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48697
%stackaddr$env-ref48698 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 1)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48698
%stackaddr$env-ref48699 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48699
%stackaddr$env-ref48700 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48700
%stackaddr$env-ref48701 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48701
%stackaddr$env-ref48702 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48702
%stackaddr$env-ref48703 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48703
%stackaddr$env-ref48704 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41834, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48704
%stackaddr$prim48705 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47371)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim48705, align 8
%stackaddr$prim48706 = alloca %struct.ScmObj*, align 8
%current_45args47372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47371)
store volatile %struct.ScmObj* %current_45args47372, %struct.ScmObj** %stackaddr$prim48706, align 8
%stackaddr$prim48707 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47372)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim48707, align 8
%stackaddr$makeclosure48708 = alloca %struct.ScmObj*, align 8
%fptrToInt48709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41866 to i64
%ae41866 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48709)
store volatile %struct.ScmObj* %ae41866, %struct.ScmObj** %stackaddr$makeclosure48708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %k40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41866, %struct.ScmObj* %_37map140154, i64 6)
%ae41868 = call %struct.ScmObj* @const_init_false()
%args47418$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48710 = alloca %struct.ScmObj*, align 8
%args47418$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47418$_37foldr140123$0)
store volatile %struct.ScmObj* %args47418$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48710, align 8
%stackaddr$prim48711 = alloca %struct.ScmObj*, align 8
%args47418$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41868, %struct.ScmObj* %args47418$_37foldr140123$1)
store volatile %struct.ScmObj* %args47418$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48711, align 8
%stackaddr$prim48712 = alloca %struct.ScmObj*, align 8
%args47418$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args47418$_37foldr140123$2)
store volatile %struct.ScmObj* %args47418$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48712, align 8
%stackaddr$prim48713 = alloca %struct.ScmObj*, align 8
%args47418$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41866, %struct.ScmObj* %args47418$_37foldr140123$3)
store volatile %struct.ScmObj* %args47418$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48713, align 8
%clofunc48714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48714(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47418$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41866(%struct.ScmObj* %env$ae41866,%struct.ScmObj* %current_45args47374) {
%stackaddr$env-ref48715 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48715
%stackaddr$env-ref48716 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 1)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48716
%stackaddr$env-ref48717 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48717
%stackaddr$env-ref48718 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48718
%stackaddr$env-ref48719 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48719
%stackaddr$env-ref48720 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48720
%stackaddr$env-ref48721 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41866, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48721
%stackaddr$prim48722 = alloca %struct.ScmObj*, align 8
%_95k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47374)
store volatile %struct.ScmObj* %_95k40418, %struct.ScmObj** %stackaddr$prim48722, align 8
%stackaddr$prim48723 = alloca %struct.ScmObj*, align 8
%current_45args47375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47374)
store volatile %struct.ScmObj* %current_45args47375, %struct.ScmObj** %stackaddr$prim48723, align 8
%stackaddr$prim48724 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47375)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim48724, align 8
%truthy$cmp48725 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40281)
%cmp$cmp48725 = icmp eq i64 %truthy$cmp48725, 1
br i1 %cmp$cmp48725, label %truebranch$cmp48725, label %falsebranch$cmp48725
truebranch$cmp48725:
%ae41877 = call %struct.ScmObj* @const_init_int(i64 0)
%args47377$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48726 = alloca %struct.ScmObj*, align 8
%args47377$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args47377$k40416$0)
store volatile %struct.ScmObj* %args47377$k40416$1, %struct.ScmObj** %stackaddr$prim48726, align 8
%stackaddr$prim48727 = alloca %struct.ScmObj*, align 8
%args47377$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41877, %struct.ScmObj* %args47377$k40416$1)
store volatile %struct.ScmObj* %args47377$k40416$2, %struct.ScmObj** %stackaddr$prim48727, align 8
%clofunc48728 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc48728(%struct.ScmObj* %k40416, %struct.ScmObj* %args47377$k40416$2)
ret void
falsebranch$cmp48725:
%stackaddr$makeclosure48729 = alloca %struct.ScmObj*, align 8
%fptrToInt48730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41882 to i64
%ae41882 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48730)
store volatile %struct.ScmObj* %ae41882, %struct.ScmObj** %stackaddr$makeclosure48729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %k40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37map140154, i64 6)
%ae41883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48731 = alloca %struct.ScmObj*, align 8
%fptrToInt48732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41884 to i64
%ae41884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48732)
store volatile %struct.ScmObj* %ae41884, %struct.ScmObj** %stackaddr$makeclosure48731, align 8
%args47417$ae41882$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%args47417$ae41882$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41884, %struct.ScmObj* %args47417$ae41882$0)
store volatile %struct.ScmObj* %args47417$ae41882$1, %struct.ScmObj** %stackaddr$prim48733, align 8
%stackaddr$prim48734 = alloca %struct.ScmObj*, align 8
%args47417$ae41882$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41883, %struct.ScmObj* %args47417$ae41882$1)
store volatile %struct.ScmObj* %args47417$ae41882$2, %struct.ScmObj** %stackaddr$prim48734, align 8
%clofunc48735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41882)
musttail call tailcc void %clofunc48735(%struct.ScmObj* %ae41882, %struct.ScmObj* %args47417$ae41882$2)
ret void
}

define tailcc void @proc_clo$ae41882(%struct.ScmObj* %env$ae41882,%struct.ScmObj* %current_45args47378) {
%stackaddr$env-ref48736 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48736
%stackaddr$env-ref48737 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 1)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48737
%stackaddr$env-ref48738 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48738
%stackaddr$env-ref48739 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48739
%stackaddr$env-ref48740 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48740
%stackaddr$env-ref48741 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48741
%stackaddr$env-ref48742 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48742
%stackaddr$prim48743 = alloca %struct.ScmObj*, align 8
%_95k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47378)
store volatile %struct.ScmObj* %_95k40419, %struct.ScmObj** %stackaddr$prim48743, align 8
%stackaddr$prim48744 = alloca %struct.ScmObj*, align 8
%current_45args47379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47378)
store volatile %struct.ScmObj* %current_45args47379, %struct.ScmObj** %stackaddr$prim48744, align 8
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47379)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$makeclosure48746 = alloca %struct.ScmObj*, align 8
%fptrToInt48747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41903 to i64
%ae41903 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48747)
store volatile %struct.ScmObj* %ae41903, %struct.ScmObj** %stackaddr$makeclosure48746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %k40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37map140154, i64 6)
%args47412$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48748 = alloca %struct.ScmObj*, align 8
%args47412$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47412$_37map140154$0)
store volatile %struct.ScmObj* %args47412$_37map140154$1, %struct.ScmObj** %stackaddr$prim48748, align 8
%stackaddr$prim48749 = alloca %struct.ScmObj*, align 8
%args47412$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args47412$_37map140154$1)
store volatile %struct.ScmObj* %args47412$_37map140154$2, %struct.ScmObj** %stackaddr$prim48749, align 8
%stackaddr$prim48750 = alloca %struct.ScmObj*, align 8
%args47412$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41903, %struct.ScmObj* %args47412$_37map140154$2)
store volatile %struct.ScmObj* %args47412$_37map140154$3, %struct.ScmObj** %stackaddr$prim48750, align 8
%clofunc48751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48751(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47412$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41903(%struct.ScmObj* %env$ae41903,%struct.ScmObj* %current_45args47381) {
%stackaddr$env-ref48752 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48752
%stackaddr$env-ref48753 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 1)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48753
%stackaddr$env-ref48754 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48754
%stackaddr$env-ref48755 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48755
%stackaddr$env-ref48756 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48756
%stackaddr$env-ref48757 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48757
%stackaddr$env-ref48758 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48758
%stackaddr$prim48759 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47381)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim48759, align 8
%stackaddr$prim48760 = alloca %struct.ScmObj*, align 8
%current_45args47382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47381)
store volatile %struct.ScmObj* %current_45args47382, %struct.ScmObj** %stackaddr$prim48760, align 8
%stackaddr$prim48761 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47382)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim48761, align 8
%stackaddr$makeclosure48762 = alloca %struct.ScmObj*, align 8
%fptrToInt48763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41906 to i64
%ae41906 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48763)
store volatile %struct.ScmObj* %ae41906, %struct.ScmObj** %stackaddr$makeclosure48762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %lsts40208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %k40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %lsts_4340215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %acc40209, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37foldl40206, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41906, %struct.ScmObj* %_37map140154, i64 7)
%ae41907 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48764 = alloca %struct.ScmObj*, align 8
%fptrToInt48765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41908 to i64
%ae41908 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48765)
store volatile %struct.ScmObj* %ae41908, %struct.ScmObj** %stackaddr$makeclosure48764, align 8
%args47411$ae41906$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48766 = alloca %struct.ScmObj*, align 8
%args47411$ae41906$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41908, %struct.ScmObj* %args47411$ae41906$0)
store volatile %struct.ScmObj* %args47411$ae41906$1, %struct.ScmObj** %stackaddr$prim48766, align 8
%stackaddr$prim48767 = alloca %struct.ScmObj*, align 8
%args47411$ae41906$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41907, %struct.ScmObj* %args47411$ae41906$1)
store volatile %struct.ScmObj* %args47411$ae41906$2, %struct.ScmObj** %stackaddr$prim48767, align 8
%clofunc48768 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41906)
musttail call tailcc void %clofunc48768(%struct.ScmObj* %ae41906, %struct.ScmObj* %args47411$ae41906$2)
ret void
}

define tailcc void @proc_clo$ae41906(%struct.ScmObj* %env$ae41906,%struct.ScmObj* %current_45args47384) {
%stackaddr$env-ref48769 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 0)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48769
%stackaddr$env-ref48770 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 1)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48770
%stackaddr$env-ref48771 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48771
%stackaddr$env-ref48772 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 3)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48772
%stackaddr$env-ref48773 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48773
%stackaddr$env-ref48774 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 5)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48774
%stackaddr$env-ref48775 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48775
%stackaddr$env-ref48776 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41906, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48776
%stackaddr$prim48777 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47384)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim48777, align 8
%stackaddr$prim48778 = alloca %struct.ScmObj*, align 8
%current_45args47385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47384)
store volatile %struct.ScmObj* %current_45args47385, %struct.ScmObj** %stackaddr$prim48778, align 8
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47385)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim48779, align 8
%stackaddr$makeclosure48780 = alloca %struct.ScmObj*, align 8
%fptrToInt48781 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41927 to i64
%ae41927 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48781)
store volatile %struct.ScmObj* %ae41927, %struct.ScmObj** %stackaddr$makeclosure48780, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41927, %struct.ScmObj* %_37foldl40206, i64 5)
%args47406$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48782 = alloca %struct.ScmObj*, align 8
%args47406$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47406$_37map140154$0)
store volatile %struct.ScmObj* %args47406$_37map140154$1, %struct.ScmObj** %stackaddr$prim48782, align 8
%stackaddr$prim48783 = alloca %struct.ScmObj*, align 8
%args47406$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args47406$_37map140154$1)
store volatile %struct.ScmObj* %args47406$_37map140154$2, %struct.ScmObj** %stackaddr$prim48783, align 8
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%args47406$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41927, %struct.ScmObj* %args47406$_37map140154$2)
store volatile %struct.ScmObj* %args47406$_37map140154$3, %struct.ScmObj** %stackaddr$prim48784, align 8
%clofunc48785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48785(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47406$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41927(%struct.ScmObj* %env$ae41927,%struct.ScmObj* %current_45args47387) {
%stackaddr$env-ref48786 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48786
%stackaddr$env-ref48787 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48787
%stackaddr$env-ref48788 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48788
%stackaddr$env-ref48789 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48789
%stackaddr$env-ref48790 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48790
%stackaddr$env-ref48791 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41927, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48791
%stackaddr$prim48792 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim48792, align 8
%stackaddr$prim48793 = alloca %struct.ScmObj*, align 8
%current_45args47388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %current_45args47388, %struct.ScmObj** %stackaddr$prim48793, align 8
%stackaddr$prim48794 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47388)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim48794, align 8
%stackaddr$makeclosure48795 = alloca %struct.ScmObj*, align 8
%fptrToInt48796 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41930 to i64
%ae41930 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48796)
store volatile %struct.ScmObj* %ae41930, %struct.ScmObj** %stackaddr$makeclosure48795, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %vs40213, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %acc40209, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37foldl40206, i64 6)
%ae41931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48797 = alloca %struct.ScmObj*, align 8
%fptrToInt48798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41932 to i64
%ae41932 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48798)
store volatile %struct.ScmObj* %ae41932, %struct.ScmObj** %stackaddr$makeclosure48797, align 8
%args47405$ae41930$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48799 = alloca %struct.ScmObj*, align 8
%args47405$ae41930$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41932, %struct.ScmObj* %args47405$ae41930$0)
store volatile %struct.ScmObj* %args47405$ae41930$1, %struct.ScmObj** %stackaddr$prim48799, align 8
%stackaddr$prim48800 = alloca %struct.ScmObj*, align 8
%args47405$ae41930$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41931, %struct.ScmObj* %args47405$ae41930$1)
store volatile %struct.ScmObj* %args47405$ae41930$2, %struct.ScmObj** %stackaddr$prim48800, align 8
%clofunc48801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41930)
musttail call tailcc void %clofunc48801(%struct.ScmObj* %ae41930, %struct.ScmObj* %args47405$ae41930$2)
ret void
}

define tailcc void @proc_clo$ae41930(%struct.ScmObj* %env$ae41930,%struct.ScmObj* %current_45args47390) {
%stackaddr$env-ref48802 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48802
%stackaddr$env-ref48803 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48803
%stackaddr$env-ref48804 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48804
%stackaddr$env-ref48805 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 3)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref48805
%stackaddr$env-ref48806 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48806
%stackaddr$env-ref48807 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 5)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48807
%stackaddr$env-ref48808 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48808
%stackaddr$prim48809 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47390)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim48809, align 8
%stackaddr$prim48810 = alloca %struct.ScmObj*, align 8
%current_45args47391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47390)
store volatile %struct.ScmObj* %current_45args47391, %struct.ScmObj** %stackaddr$prim48810, align 8
%stackaddr$prim48811 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47391)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim48811, align 8
%ae41953 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48812 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae41953)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim48812, align 8
%stackaddr$makeclosure48813 = alloca %struct.ScmObj*, align 8
%fptrToInt48814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41955 to i64
%ae41955 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48814)
store volatile %struct.ScmObj* %ae41955, %struct.ScmObj** %stackaddr$makeclosure48813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41955, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41955, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41955, %struct.ScmObj* %k40416, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41955, %struct.ScmObj* %_37foldl40206, i64 3)
%args47399$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48815 = alloca %struct.ScmObj*, align 8
%args47399$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args47399$_37foldr40128$0)
store volatile %struct.ScmObj* %args47399$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim48815, align 8
%stackaddr$prim48816 = alloca %struct.ScmObj*, align 8
%args47399$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args47399$_37foldr40128$1)
store volatile %struct.ScmObj* %args47399$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim48816, align 8
%stackaddr$prim48817 = alloca %struct.ScmObj*, align 8
%args47399$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %args47399$_37foldr40128$2)
store volatile %struct.ScmObj* %args47399$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim48817, align 8
%stackaddr$prim48818 = alloca %struct.ScmObj*, align 8
%args47399$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41955, %struct.ScmObj* %args47399$_37foldr40128$3)
store volatile %struct.ScmObj* %args47399$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim48818, align 8
%clofunc48819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48819(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args47399$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae41955(%struct.ScmObj* %env$ae41955,%struct.ScmObj* %current_45args47393) {
%stackaddr$env-ref48820 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41955, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48820
%stackaddr$env-ref48821 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41955, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48821
%stackaddr$env-ref48822 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41955, i64 2)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48822
%stackaddr$env-ref48823 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41955, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48823
%stackaddr$prim48824 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47393)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim48824, align 8
%stackaddr$prim48825 = alloca %struct.ScmObj*, align 8
%current_45args47394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47393)
store volatile %struct.ScmObj* %current_45args47394, %struct.ScmObj** %stackaddr$prim48825, align 8
%stackaddr$prim48826 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47394)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim48826, align 8
%stackaddr$makeclosure48827 = alloca %struct.ScmObj*, align 8
%fptrToInt48828 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41959 to i64
%ae41959 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48828)
store volatile %struct.ScmObj* %ae41959, %struct.ScmObj** %stackaddr$makeclosure48827, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %k40416, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41959, %struct.ScmObj* %_37foldl40206, i64 3)
%stackaddr$prim48829 = alloca %struct.ScmObj*, align 8
%cpsargs40427 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41959, %struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %cpsargs40427, %struct.ScmObj** %stackaddr$prim48829, align 8
%clofunc48830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc48830(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40427)
ret void
}

define tailcc void @proc_clo$ae41959(%struct.ScmObj* %env$ae41959,%struct.ScmObj* %current_45args47396) {
%stackaddr$env-ref48831 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48831
%stackaddr$env-ref48832 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48832
%stackaddr$env-ref48833 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 2)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref48833
%stackaddr$env-ref48834 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41959, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48834
%stackaddr$prim48835 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47396)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim48835, align 8
%stackaddr$prim48836 = alloca %struct.ScmObj*, align 8
%current_45args47397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47396)
store volatile %struct.ScmObj* %current_45args47397, %struct.ScmObj** %stackaddr$prim48836, align 8
%stackaddr$prim48837 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47397)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48837, align 8
%stackaddr$prim48838 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim48838, align 8
%stackaddr$prim48839 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40287)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48839, align 8
%stackaddr$prim48840 = alloca %struct.ScmObj*, align 8
%cpsargs40426 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40416, %struct.ScmObj* %anf_45bind40288)
store volatile %struct.ScmObj* %cpsargs40426, %struct.ScmObj** %stackaddr$prim48840, align 8
%clofunc48841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48841(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40426)
ret void
}

define tailcc void @proc_clo$ae41932(%struct.ScmObj* %env$ae41932,%struct.ScmObj* %current_45args47400) {
%stackaddr$prim48842 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47400)
store volatile %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$prim48842, align 8
%stackaddr$prim48843 = alloca %struct.ScmObj*, align 8
%current_45args47401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47400)
store volatile %struct.ScmObj* %current_45args47401, %struct.ScmObj** %stackaddr$prim48843, align 8
%stackaddr$prim48844 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47401)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48844, align 8
%stackaddr$prim48845 = alloca %struct.ScmObj*, align 8
%current_45args47402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47401)
store volatile %struct.ScmObj* %current_45args47402, %struct.ScmObj** %stackaddr$prim48845, align 8
%stackaddr$prim48846 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47402)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48846, align 8
%stackaddr$prim48847 = alloca %struct.ScmObj*, align 8
%cpsprim40429 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40429, %struct.ScmObj** %stackaddr$prim48847, align 8
%ae41936 = call %struct.ScmObj* @const_init_int(i64 0)
%args47404$k40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48848 = alloca %struct.ScmObj*, align 8
%args47404$k40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40429, %struct.ScmObj* %args47404$k40428$0)
store volatile %struct.ScmObj* %args47404$k40428$1, %struct.ScmObj** %stackaddr$prim48848, align 8
%stackaddr$prim48849 = alloca %struct.ScmObj*, align 8
%args47404$k40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41936, %struct.ScmObj* %args47404$k40428$1)
store volatile %struct.ScmObj* %args47404$k40428$2, %struct.ScmObj** %stackaddr$prim48849, align 8
%clofunc48850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40428)
musttail call tailcc void %clofunc48850(%struct.ScmObj* %k40428, %struct.ScmObj* %args47404$k40428$2)
ret void
}

define tailcc void @proc_clo$ae41908(%struct.ScmObj* %env$ae41908,%struct.ScmObj* %current_45args47407) {
%stackaddr$prim48851 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47407)
store volatile %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$prim48851, align 8
%stackaddr$prim48852 = alloca %struct.ScmObj*, align 8
%current_45args47408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47407)
store volatile %struct.ScmObj* %current_45args47408, %struct.ScmObj** %stackaddr$prim48852, align 8
%stackaddr$prim48853 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47408)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48853, align 8
%stackaddr$prim48854 = alloca %struct.ScmObj*, align 8
%cpsprim40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40431, %struct.ScmObj** %stackaddr$prim48854, align 8
%ae41911 = call %struct.ScmObj* @const_init_int(i64 0)
%args47410$k40430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48855 = alloca %struct.ScmObj*, align 8
%args47410$k40430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40431, %struct.ScmObj* %args47410$k40430$0)
store volatile %struct.ScmObj* %args47410$k40430$1, %struct.ScmObj** %stackaddr$prim48855, align 8
%stackaddr$prim48856 = alloca %struct.ScmObj*, align 8
%args47410$k40430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41911, %struct.ScmObj* %args47410$k40430$1)
store volatile %struct.ScmObj* %args47410$k40430$2, %struct.ScmObj** %stackaddr$prim48856, align 8
%clofunc48857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40430)
musttail call tailcc void %clofunc48857(%struct.ScmObj* %k40430, %struct.ScmObj* %args47410$k40430$2)
ret void
}

define tailcc void @proc_clo$ae41884(%struct.ScmObj* %env$ae41884,%struct.ScmObj* %current_45args47413) {
%stackaddr$prim48858 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47413)
store volatile %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$prim48858, align 8
%stackaddr$prim48859 = alloca %struct.ScmObj*, align 8
%current_45args47414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47413)
store volatile %struct.ScmObj* %current_45args47414, %struct.ScmObj** %stackaddr$prim48859, align 8
%stackaddr$prim48860 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47414)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48860, align 8
%stackaddr$prim48861 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim48861, align 8
%ae41887 = call %struct.ScmObj* @const_init_int(i64 0)
%args47416$k40432$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48862 = alloca %struct.ScmObj*, align 8
%args47416$k40432$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args47416$k40432$0)
store volatile %struct.ScmObj* %args47416$k40432$1, %struct.ScmObj** %stackaddr$prim48862, align 8
%stackaddr$prim48863 = alloca %struct.ScmObj*, align 8
%args47416$k40432$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41887, %struct.ScmObj* %args47416$k40432$1)
store volatile %struct.ScmObj* %args47416$k40432$2, %struct.ScmObj** %stackaddr$prim48863, align 8
%clofunc48864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40432)
musttail call tailcc void %clofunc48864(%struct.ScmObj* %k40432, %struct.ScmObj* %args47416$k40432$2)
ret void
}

define tailcc void @proc_clo$ae41836(%struct.ScmObj* %env$ae41836,%struct.ScmObj* %current_45args47419) {
%stackaddr$prim48865 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47419)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim48865, align 8
%stackaddr$prim48866 = alloca %struct.ScmObj*, align 8
%current_45args47420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47419)
store volatile %struct.ScmObj* %current_45args47420, %struct.ScmObj** %stackaddr$prim48866, align 8
%stackaddr$prim48867 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47420)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48867, align 8
%stackaddr$prim48868 = alloca %struct.ScmObj*, align 8
%current_45args47421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47420)
store volatile %struct.ScmObj* %current_45args47421, %struct.ScmObj** %stackaddr$prim48868, align 8
%stackaddr$prim48869 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47421)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48869, align 8
%truthy$cmp48870 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48870 = icmp eq i64 %truthy$cmp48870, 1
br i1 %cmp$cmp48870, label %truebranch$cmp48870, label %falsebranch$cmp48870
truebranch$cmp48870:
%ae41839 = call %struct.ScmObj* @const_init_int(i64 0)
%args47423$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48871 = alloca %struct.ScmObj*, align 8
%args47423$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args47423$k40434$0)
store volatile %struct.ScmObj* %args47423$k40434$1, %struct.ScmObj** %stackaddr$prim48871, align 8
%stackaddr$prim48872 = alloca %struct.ScmObj*, align 8
%args47423$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41839, %struct.ScmObj* %args47423$k40434$1)
store volatile %struct.ScmObj* %args47423$k40434$2, %struct.ScmObj** %stackaddr$prim48872, align 8
%clofunc48873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc48873(%struct.ScmObj* %k40434, %struct.ScmObj* %args47423$k40434$2)
ret void
falsebranch$cmp48870:
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%cpsprim40435 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40435, %struct.ScmObj** %stackaddr$prim48874, align 8
%ae41846 = call %struct.ScmObj* @const_init_int(i64 0)
%args47424$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48875 = alloca %struct.ScmObj*, align 8
%args47424$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40435, %struct.ScmObj* %args47424$k40434$0)
store volatile %struct.ScmObj* %args47424$k40434$1, %struct.ScmObj** %stackaddr$prim48875, align 8
%stackaddr$prim48876 = alloca %struct.ScmObj*, align 8
%args47424$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41846, %struct.ScmObj* %args47424$k40434$1)
store volatile %struct.ScmObj* %args47424$k40434$2, %struct.ScmObj** %stackaddr$prim48876, align 8
%clofunc48877 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc48877(%struct.ScmObj* %k40434, %struct.ScmObj* %args47424$k40434$2)
ret void
}

define tailcc void @proc_clo$ae41677(%struct.ScmObj* %env$ae41677,%struct.ScmObj* %args4015040436) {
%stackaddr$env-ref48878 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41677, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48878
%stackaddr$env-ref48879 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41677, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48879
%stackaddr$env-ref48880 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41677, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48880
%stackaddr$prim48881 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040436)
store volatile %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$prim48881, align 8
%stackaddr$prim48882 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040436)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48882, align 8
%stackaddr$prim48883 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48883, align 8
%stackaddr$prim48884 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48884, align 8
%stackaddr$makeclosure48885 = alloca %struct.ScmObj*, align 8
%fptrToInt48886 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41682 to i64
%ae41682 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48886)
store volatile %struct.ScmObj* %ae41682, %struct.ScmObj** %stackaddr$makeclosure48885, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41682, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41682, %struct.ScmObj* %k40437, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41682, %struct.ScmObj* %_37foldr40128, i64 2)
%ae41683 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48887 = alloca %struct.ScmObj*, align 8
%fptrToInt48888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41684 to i64
%ae41684 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48888)
store volatile %struct.ScmObj* %ae41684, %struct.ScmObj** %stackaddr$makeclosure48887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41684, %struct.ScmObj* %f40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41684, %struct.ScmObj* %_37last40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41684, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args47443$ae41682$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48889 = alloca %struct.ScmObj*, align 8
%args47443$ae41682$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41684, %struct.ScmObj* %args47443$ae41682$0)
store volatile %struct.ScmObj* %args47443$ae41682$1, %struct.ScmObj** %stackaddr$prim48889, align 8
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%args47443$ae41682$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41683, %struct.ScmObj* %args47443$ae41682$1)
store volatile %struct.ScmObj* %args47443$ae41682$2, %struct.ScmObj** %stackaddr$prim48890, align 8
%clofunc48891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41682)
musttail call tailcc void %clofunc48891(%struct.ScmObj* %ae41682, %struct.ScmObj* %args47443$ae41682$2)
ret void
}

define tailcc void @proc_clo$ae41682(%struct.ScmObj* %env$ae41682,%struct.ScmObj* %current_45args47428) {
%stackaddr$env-ref48892 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41682, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref48892
%stackaddr$env-ref48893 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41682, i64 1)
store %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$env-ref48893
%stackaddr$env-ref48894 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41682, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48894
%stackaddr$prim48895 = alloca %struct.ScmObj*, align 8
%_95k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47428)
store volatile %struct.ScmObj* %_95k40438, %struct.ScmObj** %stackaddr$prim48895, align 8
%stackaddr$prim48896 = alloca %struct.ScmObj*, align 8
%current_45args47429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47428)
store volatile %struct.ScmObj* %current_45args47429, %struct.ScmObj** %stackaddr$prim48896, align 8
%stackaddr$prim48897 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47429)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim48897, align 8
%ae41745 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48898 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41745, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim48898, align 8
%stackaddr$prim48899 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %anf_45bind40276)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim48899, align 8
%stackaddr$prim48900 = alloca %struct.ScmObj*, align 8
%cpsargs40439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40437, %struct.ScmObj* %anf_45bind40277)
store volatile %struct.ScmObj* %cpsargs40439, %struct.ScmObj** %stackaddr$prim48900, align 8
%clofunc48901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48901(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40439)
ret void
}

define tailcc void @proc_clo$ae41684(%struct.ScmObj* %env$ae41684,%struct.ScmObj* %fargs4015340440) {
%stackaddr$env-ref48902 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41684, i64 0)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48902
%stackaddr$env-ref48903 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41684, i64 1)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48903
%stackaddr$env-ref48904 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41684, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48904
%stackaddr$prim48905 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340440)
store volatile %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$prim48905, align 8
%stackaddr$prim48906 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340440)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim48906, align 8
%stackaddr$makeclosure48907 = alloca %struct.ScmObj*, align 8
%fptrToInt48908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41688 to i64
%ae41688 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48908)
store volatile %struct.ScmObj* %ae41688, %struct.ScmObj** %stackaddr$makeclosure48907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %k40441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %fargs40153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %f40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41688, %struct.ScmObj* %_37last40145, i64 3)
%ae41690 = call %struct.ScmObj* @const_init_int(i64 1)
%args47442$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48909 = alloca %struct.ScmObj*, align 8
%args47442$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41690, %struct.ScmObj* %args47442$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args47442$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim48909, align 8
%stackaddr$prim48910 = alloca %struct.ScmObj*, align 8
%args47442$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args47442$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args47442$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim48910, align 8
%stackaddr$prim48911 = alloca %struct.ScmObj*, align 8
%args47442$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41688, %struct.ScmObj* %args47442$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args47442$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim48911, align 8
%clofunc48912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc48912(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args47442$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41688(%struct.ScmObj* %env$ae41688,%struct.ScmObj* %current_45args47431) {
%stackaddr$env-ref48913 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 0)
store %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$env-ref48913
%stackaddr$env-ref48914 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 1)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48914
%stackaddr$env-ref48915 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 2)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref48915
%stackaddr$env-ref48916 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41688, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48916
%stackaddr$prim48917 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47431)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim48917, align 8
%stackaddr$prim48918 = alloca %struct.ScmObj*, align 8
%current_45args47432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47431)
store volatile %struct.ScmObj* %current_45args47432, %struct.ScmObj** %stackaddr$prim48918, align 8
%stackaddr$prim48919 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47432)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim48919, align 8
%stackaddr$makeclosure48920 = alloca %struct.ScmObj*, align 8
%fptrToInt48921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41695 to i64
%ae41695 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48921)
store volatile %struct.ScmObj* %ae41695, %struct.ScmObj** %stackaddr$makeclosure48920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41695, %struct.ScmObj* %k40441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41695, %struct.ScmObj* %fargs40153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41695, %struct.ScmObj* %_37last40145, i64 2)
%stackaddr$prim48922 = alloca %struct.ScmObj*, align 8
%cpsargs40446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41695, %struct.ScmObj* %anf_45bind40272)
store volatile %struct.ScmObj* %cpsargs40446, %struct.ScmObj** %stackaddr$prim48922, align 8
%clofunc48923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc48923(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40446)
ret void
}

define tailcc void @proc_clo$ae41695(%struct.ScmObj* %env$ae41695,%struct.ScmObj* %current_45args47434) {
%stackaddr$env-ref48924 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41695, i64 0)
store %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$env-ref48924
%stackaddr$env-ref48925 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41695, i64 1)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref48925
%stackaddr$env-ref48926 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41695, i64 2)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48926
%stackaddr$prim48927 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47434)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim48927, align 8
%stackaddr$prim48928 = alloca %struct.ScmObj*, align 8
%current_45args47435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47434)
store volatile %struct.ScmObj* %current_45args47435, %struct.ScmObj** %stackaddr$prim48928, align 8
%stackaddr$prim48929 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47435)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48929, align 8
%stackaddr$makeclosure48930 = alloca %struct.ScmObj*, align 8
%fptrToInt48931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41700 to i64
%ae41700 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48931)
store volatile %struct.ScmObj* %ae41700, %struct.ScmObj** %stackaddr$makeclosure48930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41700, %struct.ScmObj* %k40441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41700, %struct.ScmObj* %anf_45bind40273, i64 1)
%args47441$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48932 = alloca %struct.ScmObj*, align 8
%args47441$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args47441$_37last40145$0)
store volatile %struct.ScmObj* %args47441$_37last40145$1, %struct.ScmObj** %stackaddr$prim48932, align 8
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%args47441$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41700, %struct.ScmObj* %args47441$_37last40145$1)
store volatile %struct.ScmObj* %args47441$_37last40145$2, %struct.ScmObj** %stackaddr$prim48933, align 8
%clofunc48934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc48934(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args47441$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41700(%struct.ScmObj* %env$ae41700,%struct.ScmObj* %current_45args47437) {
%stackaddr$env-ref48935 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41700, i64 0)
store %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$env-ref48935
%stackaddr$env-ref48936 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41700, i64 1)
store %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$env-ref48936
%stackaddr$prim48937 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47437)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim48937, align 8
%stackaddr$prim48938 = alloca %struct.ScmObj*, align 8
%current_45args47438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47437)
store volatile %struct.ScmObj* %current_45args47438, %struct.ScmObj** %stackaddr$prim48938, align 8
%stackaddr$prim48939 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47438)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim48939, align 8
%stackaddr$prim48940 = alloca %struct.ScmObj*, align 8
%cpsprim40445 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %anf_45bind40274)
store volatile %struct.ScmObj* %cpsprim40445, %struct.ScmObj** %stackaddr$prim48940, align 8
%ae41705 = call %struct.ScmObj* @const_init_int(i64 0)
%args47440$k40441$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48941 = alloca %struct.ScmObj*, align 8
%args47440$k40441$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40445, %struct.ScmObj* %args47440$k40441$0)
store volatile %struct.ScmObj* %args47440$k40441$1, %struct.ScmObj** %stackaddr$prim48941, align 8
%stackaddr$prim48942 = alloca %struct.ScmObj*, align 8
%args47440$k40441$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41705, %struct.ScmObj* %args47440$k40441$1)
store volatile %struct.ScmObj* %args47440$k40441$2, %struct.ScmObj** %stackaddr$prim48942, align 8
%clofunc48943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40441)
musttail call tailcc void %clofunc48943(%struct.ScmObj* %k40441, %struct.ScmObj* %args47440$k40441$2)
ret void
}

define tailcc void @proc_clo$ae41600(%struct.ScmObj* %env$ae41600,%struct.ScmObj* %current_45args47445) {
%stackaddr$env-ref48944 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41600, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48944
%stackaddr$prim48945 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47445)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim48945, align 8
%stackaddr$prim48946 = alloca %struct.ScmObj*, align 8
%current_45args47446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47445)
store volatile %struct.ScmObj* %current_45args47446, %struct.ScmObj** %stackaddr$prim48946, align 8
%stackaddr$prim48947 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47446)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim48947, align 8
%stackaddr$prim48948 = alloca %struct.ScmObj*, align 8
%current_45args47447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47446)
store volatile %struct.ScmObj* %current_45args47447, %struct.ScmObj** %stackaddr$prim48948, align 8
%stackaddr$prim48949 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47447)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim48949, align 8
%stackaddr$makeclosure48950 = alloca %struct.ScmObj*, align 8
%fptrToInt48951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41601 to i64
%ae41601 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48951)
store volatile %struct.ScmObj* %ae41601, %struct.ScmObj** %stackaddr$makeclosure48950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41601, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41601, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41601, %struct.ScmObj* %k40447, i64 2)
%ae41602 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48952 = alloca %struct.ScmObj*, align 8
%fptrToInt48953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41603 to i64
%ae41603 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48953)
store volatile %struct.ScmObj* %ae41603, %struct.ScmObj** %stackaddr$makeclosure48952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41603, %struct.ScmObj* %f40156, i64 0)
%args47462$ae41601$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48954 = alloca %struct.ScmObj*, align 8
%args47462$ae41601$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41603, %struct.ScmObj* %args47462$ae41601$0)
store volatile %struct.ScmObj* %args47462$ae41601$1, %struct.ScmObj** %stackaddr$prim48954, align 8
%stackaddr$prim48955 = alloca %struct.ScmObj*, align 8
%args47462$ae41601$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41602, %struct.ScmObj* %args47462$ae41601$1)
store volatile %struct.ScmObj* %args47462$ae41601$2, %struct.ScmObj** %stackaddr$prim48955, align 8
%clofunc48956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41601)
musttail call tailcc void %clofunc48956(%struct.ScmObj* %ae41601, %struct.ScmObj* %args47462$ae41601$2)
ret void
}

define tailcc void @proc_clo$ae41601(%struct.ScmObj* %env$ae41601,%struct.ScmObj* %current_45args47449) {
%stackaddr$env-ref48957 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41601, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref48957
%stackaddr$env-ref48958 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41601, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48958
%stackaddr$env-ref48959 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41601, i64 2)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref48959
%stackaddr$prim48960 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47449)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim48960, align 8
%stackaddr$prim48961 = alloca %struct.ScmObj*, align 8
%current_45args47450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47449)
store volatile %struct.ScmObj* %current_45args47450, %struct.ScmObj** %stackaddr$prim48961, align 8
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47450)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim48962, align 8
%ae41635 = call %struct.ScmObj* @const_init_null()
%args47452$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%args47452$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args47452$_37foldr140123$0)
store volatile %struct.ScmObj* %args47452$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%args47452$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41635, %struct.ScmObj* %args47452$_37foldr140123$1)
store volatile %struct.ScmObj* %args47452$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48964, align 8
%stackaddr$prim48965 = alloca %struct.ScmObj*, align 8
%args47452$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args47452$_37foldr140123$2)
store volatile %struct.ScmObj* %args47452$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48965, align 8
%stackaddr$prim48966 = alloca %struct.ScmObj*, align 8
%args47452$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40447, %struct.ScmObj* %args47452$_37foldr140123$3)
store volatile %struct.ScmObj* %args47452$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48966, align 8
%clofunc48967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48967(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47452$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41603(%struct.ScmObj* %env$ae41603,%struct.ScmObj* %current_45args47453) {
%stackaddr$env-ref48968 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41603, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref48968
%stackaddr$prim48969 = alloca %struct.ScmObj*, align 8
%k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47453)
store volatile %struct.ScmObj* %k40449, %struct.ScmObj** %stackaddr$prim48969, align 8
%stackaddr$prim48970 = alloca %struct.ScmObj*, align 8
%current_45args47454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47453)
store volatile %struct.ScmObj* %current_45args47454, %struct.ScmObj** %stackaddr$prim48970, align 8
%stackaddr$prim48971 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47454)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim48971, align 8
%stackaddr$prim48972 = alloca %struct.ScmObj*, align 8
%current_45args47455 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47454)
store volatile %struct.ScmObj* %current_45args47455, %struct.ScmObj** %stackaddr$prim48972, align 8
%stackaddr$prim48973 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47455)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim48973, align 8
%stackaddr$makeclosure48974 = alloca %struct.ScmObj*, align 8
%fptrToInt48975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41605 to i64
%ae41605 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48975)
store volatile %struct.ScmObj* %ae41605, %struct.ScmObj** %stackaddr$makeclosure48974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41605, %struct.ScmObj* %k40449, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41605, %struct.ScmObj* %r40157, i64 1)
%args47461$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48976 = alloca %struct.ScmObj*, align 8
%args47461$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args47461$f40156$0)
store volatile %struct.ScmObj* %args47461$f40156$1, %struct.ScmObj** %stackaddr$prim48976, align 8
%stackaddr$prim48977 = alloca %struct.ScmObj*, align 8
%args47461$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41605, %struct.ScmObj* %args47461$f40156$1)
store volatile %struct.ScmObj* %args47461$f40156$2, %struct.ScmObj** %stackaddr$prim48977, align 8
%clofunc48978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc48978(%struct.ScmObj* %f40156, %struct.ScmObj* %args47461$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41605(%struct.ScmObj* %env$ae41605,%struct.ScmObj* %current_45args47457) {
%stackaddr$env-ref48979 = alloca %struct.ScmObj*, align 8
%k40449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41605, i64 0)
store %struct.ScmObj* %k40449, %struct.ScmObj** %stackaddr$env-ref48979
%stackaddr$env-ref48980 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41605, i64 1)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref48980
%stackaddr$prim48981 = alloca %struct.ScmObj*, align 8
%_95k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47457)
store volatile %struct.ScmObj* %_95k40450, %struct.ScmObj** %stackaddr$prim48981, align 8
%stackaddr$prim48982 = alloca %struct.ScmObj*, align 8
%current_45args47458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47457)
store volatile %struct.ScmObj* %current_45args47458, %struct.ScmObj** %stackaddr$prim48982, align 8
%stackaddr$prim48983 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47458)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim48983, align 8
%stackaddr$prim48984 = alloca %struct.ScmObj*, align 8
%cpsprim40451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40451, %struct.ScmObj** %stackaddr$prim48984, align 8
%ae41610 = call %struct.ScmObj* @const_init_int(i64 0)
%args47460$k40449$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48985 = alloca %struct.ScmObj*, align 8
%args47460$k40449$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40451, %struct.ScmObj* %args47460$k40449$0)
store volatile %struct.ScmObj* %args47460$k40449$1, %struct.ScmObj** %stackaddr$prim48985, align 8
%stackaddr$prim48986 = alloca %struct.ScmObj*, align 8
%args47460$k40449$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41610, %struct.ScmObj* %args47460$k40449$1)
store volatile %struct.ScmObj* %args47460$k40449$2, %struct.ScmObj** %stackaddr$prim48986, align 8
%clofunc48987 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40449)
musttail call tailcc void %clofunc48987(%struct.ScmObj* %k40449, %struct.ScmObj* %args47460$k40449$2)
ret void
}

define tailcc void @proc_clo$ae41214(%struct.ScmObj* %env$ae41214,%struct.ScmObj* %current_45args47465) {
%stackaddr$env-ref48988 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48988
%stackaddr$env-ref48989 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41214, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48989
%stackaddr$prim48990 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47465)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48990, align 8
%stackaddr$prim48991 = alloca %struct.ScmObj*, align 8
%current_45args47466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47465)
store volatile %struct.ScmObj* %current_45args47466, %struct.ScmObj** %stackaddr$prim48991, align 8
%stackaddr$prim48992 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47466)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim48992, align 8
%ae41216 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48993 = alloca %struct.ScmObj*, align 8
%fptrToInt48994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41217 to i64
%ae41217 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48994)
store volatile %struct.ScmObj* %ae41217, %struct.ScmObj** %stackaddr$makeclosure48993, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41217, %struct.ScmObj* %_37foldr140123, i64 2)
%args47523$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48995 = alloca %struct.ScmObj*, align 8
%args47523$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41217, %struct.ScmObj* %args47523$k40452$0)
store volatile %struct.ScmObj* %args47523$k40452$1, %struct.ScmObj** %stackaddr$prim48995, align 8
%stackaddr$prim48996 = alloca %struct.ScmObj*, align 8
%args47523$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41216, %struct.ScmObj* %args47523$k40452$1)
store volatile %struct.ScmObj* %args47523$k40452$2, %struct.ScmObj** %stackaddr$prim48996, align 8
%clofunc48997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48997(%struct.ScmObj* %k40452, %struct.ScmObj* %args47523$k40452$2)
ret void
}

define tailcc void @proc_clo$ae41217(%struct.ScmObj* %env$ae41217,%struct.ScmObj* %args4013040453) {
%stackaddr$env-ref48998 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref48998
%stackaddr$env-ref48999 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref48999
%stackaddr$env-ref49000 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41217, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49000
%stackaddr$prim49001 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040453)
store volatile %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$prim49001, align 8
%stackaddr$prim49002 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040453)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim49002, align 8
%stackaddr$prim49003 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim49003, align 8
%stackaddr$prim49004 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim49004, align 8
%stackaddr$prim49005 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40257)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim49005, align 8
%stackaddr$prim49006 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim49006, align 8
%stackaddr$prim49007 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40258)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim49007, align 8
%stackaddr$makeclosure49008 = alloca %struct.ScmObj*, align 8
%fptrToInt49009 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41225 to i64
%ae41225 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49009)
store volatile %struct.ScmObj* %ae41225, %struct.ScmObj** %stackaddr$makeclosure49008, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41225, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41226 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49010 = alloca %struct.ScmObj*, align 8
%fptrToInt49011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41227 to i64
%ae41227 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49011)
store volatile %struct.ScmObj* %ae41227, %struct.ScmObj** %stackaddr$makeclosure49010, align 8
%args47522$ae41225$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49012 = alloca %struct.ScmObj*, align 8
%args47522$ae41225$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41227, %struct.ScmObj* %args47522$ae41225$0)
store volatile %struct.ScmObj* %args47522$ae41225$1, %struct.ScmObj** %stackaddr$prim49012, align 8
%stackaddr$prim49013 = alloca %struct.ScmObj*, align 8
%args47522$ae41225$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41226, %struct.ScmObj* %args47522$ae41225$1)
store volatile %struct.ScmObj* %args47522$ae41225$2, %struct.ScmObj** %stackaddr$prim49013, align 8
%clofunc49014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41225)
musttail call tailcc void %clofunc49014(%struct.ScmObj* %ae41225, %struct.ScmObj* %args47522$ae41225$2)
ret void
}

define tailcc void @proc_clo$ae41225(%struct.ScmObj* %env$ae41225,%struct.ScmObj* %current_45args47468) {
%stackaddr$env-ref49015 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49015
%stackaddr$env-ref49016 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49016
%stackaddr$env-ref49017 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49017
%stackaddr$env-ref49018 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49018
%stackaddr$env-ref49019 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49019
%stackaddr$env-ref49020 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49020
%stackaddr$env-ref49021 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41225, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49021
%stackaddr$prim49022 = alloca %struct.ScmObj*, align 8
%_95k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47468)
store volatile %struct.ScmObj* %_95k40455, %struct.ScmObj** %stackaddr$prim49022, align 8
%stackaddr$prim49023 = alloca %struct.ScmObj*, align 8
%current_45args47469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47468)
store volatile %struct.ScmObj* %current_45args47469, %struct.ScmObj** %stackaddr$prim49023, align 8
%stackaddr$prim49024 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47469)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim49024, align 8
%stackaddr$makeclosure49025 = alloca %struct.ScmObj*, align 8
%fptrToInt49026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41257 to i64
%ae41257 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49026)
store volatile %struct.ScmObj* %ae41257, %struct.ScmObj** %stackaddr$makeclosure49025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41257, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41259 = call %struct.ScmObj* @const_init_false()
%args47515$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49027 = alloca %struct.ScmObj*, align 8
%args47515$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47515$_37foldr140123$0)
store volatile %struct.ScmObj* %args47515$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim49027, align 8
%stackaddr$prim49028 = alloca %struct.ScmObj*, align 8
%args47515$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41259, %struct.ScmObj* %args47515$_37foldr140123$1)
store volatile %struct.ScmObj* %args47515$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim49028, align 8
%stackaddr$prim49029 = alloca %struct.ScmObj*, align 8
%args47515$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args47515$_37foldr140123$2)
store volatile %struct.ScmObj* %args47515$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim49029, align 8
%stackaddr$prim49030 = alloca %struct.ScmObj*, align 8
%args47515$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41257, %struct.ScmObj* %args47515$_37foldr140123$3)
store volatile %struct.ScmObj* %args47515$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim49030, align 8
%clofunc49031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc49031(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47515$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41257(%struct.ScmObj* %env$ae41257,%struct.ScmObj* %current_45args47471) {
%stackaddr$env-ref49032 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49032
%stackaddr$env-ref49033 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49033
%stackaddr$env-ref49034 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49034
%stackaddr$env-ref49035 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49035
%stackaddr$env-ref49036 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49036
%stackaddr$env-ref49037 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49037
%stackaddr$env-ref49038 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41257, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49038
%stackaddr$prim49039 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47471)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim49039, align 8
%stackaddr$prim49040 = alloca %struct.ScmObj*, align 8
%current_45args47472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47471)
store volatile %struct.ScmObj* %current_45args47472, %struct.ScmObj** %stackaddr$prim49040, align 8
%stackaddr$prim49041 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47472)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim49041, align 8
%truthy$cmp49042 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40260)
%cmp$cmp49042 = icmp eq i64 %truthy$cmp49042, 1
br i1 %cmp$cmp49042, label %truebranch$cmp49042, label %falsebranch$cmp49042
truebranch$cmp49042:
%ae41268 = call %struct.ScmObj* @const_init_int(i64 0)
%args47474$k40454$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49043 = alloca %struct.ScmObj*, align 8
%args47474$k40454$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args47474$k40454$0)
store volatile %struct.ScmObj* %args47474$k40454$1, %struct.ScmObj** %stackaddr$prim49043, align 8
%stackaddr$prim49044 = alloca %struct.ScmObj*, align 8
%args47474$k40454$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41268, %struct.ScmObj* %args47474$k40454$1)
store volatile %struct.ScmObj* %args47474$k40454$2, %struct.ScmObj** %stackaddr$prim49044, align 8
%clofunc49045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40454)
musttail call tailcc void %clofunc49045(%struct.ScmObj* %k40454, %struct.ScmObj* %args47474$k40454$2)
ret void
falsebranch$cmp49042:
%stackaddr$makeclosure49046 = alloca %struct.ScmObj*, align 8
%fptrToInt49047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41273 to i64
%ae41273 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49047)
store volatile %struct.ScmObj* %ae41273, %struct.ScmObj** %stackaddr$makeclosure49046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41274 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49048 = alloca %struct.ScmObj*, align 8
%fptrToInt49049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41275 to i64
%ae41275 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49049)
store volatile %struct.ScmObj* %ae41275, %struct.ScmObj** %stackaddr$makeclosure49048, align 8
%args47514$ae41273$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49050 = alloca %struct.ScmObj*, align 8
%args47514$ae41273$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41275, %struct.ScmObj* %args47514$ae41273$0)
store volatile %struct.ScmObj* %args47514$ae41273$1, %struct.ScmObj** %stackaddr$prim49050, align 8
%stackaddr$prim49051 = alloca %struct.ScmObj*, align 8
%args47514$ae41273$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41274, %struct.ScmObj* %args47514$ae41273$1)
store volatile %struct.ScmObj* %args47514$ae41273$2, %struct.ScmObj** %stackaddr$prim49051, align 8
%clofunc49052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41273)
musttail call tailcc void %clofunc49052(%struct.ScmObj* %ae41273, %struct.ScmObj* %args47514$ae41273$2)
ret void
}

define tailcc void @proc_clo$ae41273(%struct.ScmObj* %env$ae41273,%struct.ScmObj* %current_45args47475) {
%stackaddr$env-ref49053 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49053
%stackaddr$env-ref49054 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49054
%stackaddr$env-ref49055 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49055
%stackaddr$env-ref49056 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49056
%stackaddr$env-ref49057 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49057
%stackaddr$env-ref49058 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49058
%stackaddr$env-ref49059 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49059
%stackaddr$prim49060 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47475)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim49060, align 8
%stackaddr$prim49061 = alloca %struct.ScmObj*, align 8
%current_45args47476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47475)
store volatile %struct.ScmObj* %current_45args47476, %struct.ScmObj** %stackaddr$prim49061, align 8
%stackaddr$prim49062 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47476)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim49062, align 8
%stackaddr$makeclosure49063 = alloca %struct.ScmObj*, align 8
%fptrToInt49064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41294 to i64
%ae41294 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49064)
store volatile %struct.ScmObj* %ae41294, %struct.ScmObj** %stackaddr$makeclosure49063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37foldr140123, i64 6)
%args47509$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49065 = alloca %struct.ScmObj*, align 8
%args47509$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47509$_37map140119$0)
store volatile %struct.ScmObj* %args47509$_37map140119$1, %struct.ScmObj** %stackaddr$prim49065, align 8
%stackaddr$prim49066 = alloca %struct.ScmObj*, align 8
%args47509$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args47509$_37map140119$1)
store volatile %struct.ScmObj* %args47509$_37map140119$2, %struct.ScmObj** %stackaddr$prim49066, align 8
%stackaddr$prim49067 = alloca %struct.ScmObj*, align 8
%args47509$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41294, %struct.ScmObj* %args47509$_37map140119$2)
store volatile %struct.ScmObj* %args47509$_37map140119$3, %struct.ScmObj** %stackaddr$prim49067, align 8
%clofunc49068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc49068(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47509$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41294(%struct.ScmObj* %env$ae41294,%struct.ScmObj* %current_45args47478) {
%stackaddr$env-ref49069 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49069
%stackaddr$env-ref49070 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49070
%stackaddr$env-ref49071 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49071
%stackaddr$env-ref49072 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49072
%stackaddr$env-ref49073 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49073
%stackaddr$env-ref49074 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49074
%stackaddr$env-ref49075 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49075
%stackaddr$prim49076 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47478)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim49076, align 8
%stackaddr$prim49077 = alloca %struct.ScmObj*, align 8
%current_45args47479 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47478)
store volatile %struct.ScmObj* %current_45args47479, %struct.ScmObj** %stackaddr$prim49077, align 8
%stackaddr$prim49078 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47479)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim49078, align 8
%stackaddr$makeclosure49079 = alloca %struct.ScmObj*, align 8
%fptrToInt49080 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41297 to i64
%ae41297 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49080)
store volatile %struct.ScmObj* %ae41297, %struct.ScmObj** %stackaddr$makeclosure49079, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41297, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41298 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49081 = alloca %struct.ScmObj*, align 8
%fptrToInt49082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41299 to i64
%ae41299 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49082)
store volatile %struct.ScmObj* %ae41299, %struct.ScmObj** %stackaddr$makeclosure49081, align 8
%args47508$ae41297$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49083 = alloca %struct.ScmObj*, align 8
%args47508$ae41297$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41299, %struct.ScmObj* %args47508$ae41297$0)
store volatile %struct.ScmObj* %args47508$ae41297$1, %struct.ScmObj** %stackaddr$prim49083, align 8
%stackaddr$prim49084 = alloca %struct.ScmObj*, align 8
%args47508$ae41297$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41298, %struct.ScmObj* %args47508$ae41297$1)
store volatile %struct.ScmObj* %args47508$ae41297$2, %struct.ScmObj** %stackaddr$prim49084, align 8
%clofunc49085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41297)
musttail call tailcc void %clofunc49085(%struct.ScmObj* %ae41297, %struct.ScmObj* %args47508$ae41297$2)
ret void
}

define tailcc void @proc_clo$ae41297(%struct.ScmObj* %env$ae41297,%struct.ScmObj* %current_45args47481) {
%stackaddr$env-ref49086 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49086
%stackaddr$env-ref49087 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49087
%stackaddr$env-ref49088 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49088
%stackaddr$env-ref49089 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49089
%stackaddr$env-ref49090 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49090
%stackaddr$env-ref49091 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49091
%stackaddr$env-ref49092 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49092
%stackaddr$env-ref49093 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41297, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49093
%stackaddr$prim49094 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47481)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim49094, align 8
%stackaddr$prim49095 = alloca %struct.ScmObj*, align 8
%current_45args47482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47481)
store volatile %struct.ScmObj* %current_45args47482, %struct.ScmObj** %stackaddr$prim49095, align 8
%stackaddr$prim49096 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47482)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim49096, align 8
%stackaddr$makeclosure49097 = alloca %struct.ScmObj*, align 8
%fptrToInt49098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41318 to i64
%ae41318 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49098)
store volatile %struct.ScmObj* %ae41318, %struct.ScmObj** %stackaddr$makeclosure49097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %k40454, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41318, %struct.ScmObj* %lsts_4340138, i64 5)
%args47503$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49099 = alloca %struct.ScmObj*, align 8
%args47503$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47503$_37map140119$0)
store volatile %struct.ScmObj* %args47503$_37map140119$1, %struct.ScmObj** %stackaddr$prim49099, align 8
%stackaddr$prim49100 = alloca %struct.ScmObj*, align 8
%args47503$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args47503$_37map140119$1)
store volatile %struct.ScmObj* %args47503$_37map140119$2, %struct.ScmObj** %stackaddr$prim49100, align 8
%stackaddr$prim49101 = alloca %struct.ScmObj*, align 8
%args47503$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41318, %struct.ScmObj* %args47503$_37map140119$2)
store volatile %struct.ScmObj* %args47503$_37map140119$3, %struct.ScmObj** %stackaddr$prim49101, align 8
%clofunc49102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc49102(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47503$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41318(%struct.ScmObj* %env$ae41318,%struct.ScmObj* %current_45args47484) {
%stackaddr$env-ref49103 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 0)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49103
%stackaddr$env-ref49104 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49104
%stackaddr$env-ref49105 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49105
%stackaddr$env-ref49106 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49106
%stackaddr$env-ref49107 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49107
%stackaddr$env-ref49108 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41318, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49108
%stackaddr$prim49109 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47484)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim49109, align 8
%stackaddr$prim49110 = alloca %struct.ScmObj*, align 8
%current_45args47485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47484)
store volatile %struct.ScmObj* %current_45args47485, %struct.ScmObj** %stackaddr$prim49110, align 8
%stackaddr$prim49111 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47485)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim49111, align 8
%stackaddr$makeclosure49112 = alloca %struct.ScmObj*, align 8
%fptrToInt49113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41321 to i64
%ae41321 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49113)
store volatile %struct.ScmObj* %ae41321, %struct.ScmObj** %stackaddr$makeclosure49112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %vs40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %k40454, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldr140123, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %lsts_4340138, i64 6)
%ae41322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49114 = alloca %struct.ScmObj*, align 8
%fptrToInt49115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41323 to i64
%ae41323 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49115)
store volatile %struct.ScmObj* %ae41323, %struct.ScmObj** %stackaddr$makeclosure49114, align 8
%args47502$ae41321$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49116 = alloca %struct.ScmObj*, align 8
%args47502$ae41321$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41323, %struct.ScmObj* %args47502$ae41321$0)
store volatile %struct.ScmObj* %args47502$ae41321$1, %struct.ScmObj** %stackaddr$prim49116, align 8
%stackaddr$prim49117 = alloca %struct.ScmObj*, align 8
%args47502$ae41321$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41322, %struct.ScmObj* %args47502$ae41321$1)
store volatile %struct.ScmObj* %args47502$ae41321$2, %struct.ScmObj** %stackaddr$prim49117, align 8
%clofunc49118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41321)
musttail call tailcc void %clofunc49118(%struct.ScmObj* %ae41321, %struct.ScmObj* %args47502$ae41321$2)
ret void
}

define tailcc void @proc_clo$ae41321(%struct.ScmObj* %env$ae41321,%struct.ScmObj* %current_45args47487) {
%stackaddr$env-ref49119 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 0)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref49119
%stackaddr$env-ref49120 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 1)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49120
%stackaddr$env-ref49121 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49121
%stackaddr$env-ref49122 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49122
%stackaddr$env-ref49123 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49123
%stackaddr$env-ref49124 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 5)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49124
%stackaddr$env-ref49125 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 6)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49125
%stackaddr$prim49126 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47487)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim49126, align 8
%stackaddr$prim49127 = alloca %struct.ScmObj*, align 8
%current_45args47488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47487)
store volatile %struct.ScmObj* %current_45args47488, %struct.ScmObj** %stackaddr$prim49127, align 8
%stackaddr$prim49128 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47488)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim49128, align 8
%stackaddr$prim49129 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim49129, align 8
%stackaddr$prim49130 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40264)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim49130, align 8
%stackaddr$makeclosure49131 = alloca %struct.ScmObj*, align 8
%fptrToInt49132 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41347 to i64
%ae41347 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49132)
store volatile %struct.ScmObj* %ae41347, %struct.ScmObj** %stackaddr$makeclosure49131, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41347, %struct.ScmObj* %vs40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41347, %struct.ScmObj* %anf_45bind40263, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41347, %struct.ScmObj* %k40454, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41347, %struct.ScmObj* %f40133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41347, %struct.ScmObj* %_37foldr140123, i64 4)
%stackaddr$prim49133 = alloca %struct.ScmObj*, align 8
%cpsargs40465 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41347, %struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %cpsargs40465, %struct.ScmObj** %stackaddr$prim49133, align 8
%clofunc49134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc49134(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40465)
ret void
}

define tailcc void @proc_clo$ae41347(%struct.ScmObj* %env$ae41347,%struct.ScmObj* %current_45args47490) {
%stackaddr$env-ref49135 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41347, i64 0)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref49135
%stackaddr$env-ref49136 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41347, i64 1)
store %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$env-ref49136
%stackaddr$env-ref49137 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41347, i64 2)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49137
%stackaddr$env-ref49138 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41347, i64 3)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49138
%stackaddr$env-ref49139 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41347, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49139
%stackaddr$prim49140 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47490)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim49140, align 8
%stackaddr$prim49141 = alloca %struct.ScmObj*, align 8
%current_45args47491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47490)
store volatile %struct.ScmObj* %current_45args47491, %struct.ScmObj** %stackaddr$prim49141, align 8
%stackaddr$prim49142 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47491)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim49142, align 8
%ae41352 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49143 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %ae41352)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim49143, align 8
%stackaddr$makeclosure49144 = alloca %struct.ScmObj*, align 8
%fptrToInt49145 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41354 to i64
%ae41354 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49145)
store volatile %struct.ScmObj* %ae41354, %struct.ScmObj** %stackaddr$makeclosure49144, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41354, %struct.ScmObj* %k40454, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41354, %struct.ScmObj* %f40133, i64 1)
%args47496$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49146 = alloca %struct.ScmObj*, align 8
%args47496$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args47496$_37foldr140123$0)
store volatile %struct.ScmObj* %args47496$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim49146, align 8
%stackaddr$prim49147 = alloca %struct.ScmObj*, align 8
%args47496$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40267, %struct.ScmObj* %args47496$_37foldr140123$1)
store volatile %struct.ScmObj* %args47496$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim49147, align 8
%stackaddr$prim49148 = alloca %struct.ScmObj*, align 8
%args47496$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args47496$_37foldr140123$2)
store volatile %struct.ScmObj* %args47496$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim49148, align 8
%stackaddr$prim49149 = alloca %struct.ScmObj*, align 8
%args47496$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41354, %struct.ScmObj* %args47496$_37foldr140123$3)
store volatile %struct.ScmObj* %args47496$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim49149, align 8
%clofunc49150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc49150(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47496$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41354(%struct.ScmObj* %env$ae41354,%struct.ScmObj* %current_45args47493) {
%stackaddr$env-ref49151 = alloca %struct.ScmObj*, align 8
%k40454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41354, i64 0)
store %struct.ScmObj* %k40454, %struct.ScmObj** %stackaddr$env-ref49151
%stackaddr$env-ref49152 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41354, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49152
%stackaddr$prim49153 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47493)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim49153, align 8
%stackaddr$prim49154 = alloca %struct.ScmObj*, align 8
%current_45args47494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47493)
store volatile %struct.ScmObj* %current_45args47494, %struct.ScmObj** %stackaddr$prim49154, align 8
%stackaddr$prim49155 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47494)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim49155, align 8
%stackaddr$prim49156 = alloca %struct.ScmObj*, align 8
%cpsargs40464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40454, %struct.ScmObj* %anf_45bind40268)
store volatile %struct.ScmObj* %cpsargs40464, %struct.ScmObj** %stackaddr$prim49156, align 8
%clofunc49157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc49157(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40464)
ret void
}

define tailcc void @proc_clo$ae41323(%struct.ScmObj* %env$ae41323,%struct.ScmObj* %current_45args47497) {
%stackaddr$prim49158 = alloca %struct.ScmObj*, align 8
%k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47497)
store volatile %struct.ScmObj* %k40466, %struct.ScmObj** %stackaddr$prim49158, align 8
%stackaddr$prim49159 = alloca %struct.ScmObj*, align 8
%current_45args47498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47497)
store volatile %struct.ScmObj* %current_45args47498, %struct.ScmObj** %stackaddr$prim49159, align 8
%stackaddr$prim49160 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47498)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim49160, align 8
%stackaddr$prim49161 = alloca %struct.ScmObj*, align 8
%current_45args47499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47498)
store volatile %struct.ScmObj* %current_45args47499, %struct.ScmObj** %stackaddr$prim49161, align 8
%stackaddr$prim49162 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47499)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim49162, align 8
%stackaddr$prim49163 = alloca %struct.ScmObj*, align 8
%cpsprim40467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40467, %struct.ScmObj** %stackaddr$prim49163, align 8
%ae41327 = call %struct.ScmObj* @const_init_int(i64 0)
%args47501$k40466$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49164 = alloca %struct.ScmObj*, align 8
%args47501$k40466$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40467, %struct.ScmObj* %args47501$k40466$0)
store volatile %struct.ScmObj* %args47501$k40466$1, %struct.ScmObj** %stackaddr$prim49164, align 8
%stackaddr$prim49165 = alloca %struct.ScmObj*, align 8
%args47501$k40466$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41327, %struct.ScmObj* %args47501$k40466$1)
store volatile %struct.ScmObj* %args47501$k40466$2, %struct.ScmObj** %stackaddr$prim49165, align 8
%clofunc49166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40466)
musttail call tailcc void %clofunc49166(%struct.ScmObj* %k40466, %struct.ScmObj* %args47501$k40466$2)
ret void
}

define tailcc void @proc_clo$ae41299(%struct.ScmObj* %env$ae41299,%struct.ScmObj* %current_45args47504) {
%stackaddr$prim49167 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47504)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim49167, align 8
%stackaddr$prim49168 = alloca %struct.ScmObj*, align 8
%current_45args47505 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47504)
store volatile %struct.ScmObj* %current_45args47505, %struct.ScmObj** %stackaddr$prim49168, align 8
%stackaddr$prim49169 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47505)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim49169, align 8
%stackaddr$prim49170 = alloca %struct.ScmObj*, align 8
%cpsprim40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40469, %struct.ScmObj** %stackaddr$prim49170, align 8
%ae41302 = call %struct.ScmObj* @const_init_int(i64 0)
%args47507$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49171 = alloca %struct.ScmObj*, align 8
%args47507$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40469, %struct.ScmObj* %args47507$k40468$0)
store volatile %struct.ScmObj* %args47507$k40468$1, %struct.ScmObj** %stackaddr$prim49171, align 8
%stackaddr$prim49172 = alloca %struct.ScmObj*, align 8
%args47507$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41302, %struct.ScmObj* %args47507$k40468$1)
store volatile %struct.ScmObj* %args47507$k40468$2, %struct.ScmObj** %stackaddr$prim49172, align 8
%clofunc49173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc49173(%struct.ScmObj* %k40468, %struct.ScmObj* %args47507$k40468$2)
ret void
}

define tailcc void @proc_clo$ae41275(%struct.ScmObj* %env$ae41275,%struct.ScmObj* %current_45args47510) {
%stackaddr$prim49174 = alloca %struct.ScmObj*, align 8
%k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47510)
store volatile %struct.ScmObj* %k40470, %struct.ScmObj** %stackaddr$prim49174, align 8
%stackaddr$prim49175 = alloca %struct.ScmObj*, align 8
%current_45args47511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47510)
store volatile %struct.ScmObj* %current_45args47511, %struct.ScmObj** %stackaddr$prim49175, align 8
%stackaddr$prim49176 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47511)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim49176, align 8
%stackaddr$prim49177 = alloca %struct.ScmObj*, align 8
%cpsprim40471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40471, %struct.ScmObj** %stackaddr$prim49177, align 8
%ae41278 = call %struct.ScmObj* @const_init_int(i64 0)
%args47513$k40470$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49178 = alloca %struct.ScmObj*, align 8
%args47513$k40470$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40471, %struct.ScmObj* %args47513$k40470$0)
store volatile %struct.ScmObj* %args47513$k40470$1, %struct.ScmObj** %stackaddr$prim49178, align 8
%stackaddr$prim49179 = alloca %struct.ScmObj*, align 8
%args47513$k40470$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41278, %struct.ScmObj* %args47513$k40470$1)
store volatile %struct.ScmObj* %args47513$k40470$2, %struct.ScmObj** %stackaddr$prim49179, align 8
%clofunc49180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40470)
musttail call tailcc void %clofunc49180(%struct.ScmObj* %k40470, %struct.ScmObj* %args47513$k40470$2)
ret void
}

define tailcc void @proc_clo$ae41227(%struct.ScmObj* %env$ae41227,%struct.ScmObj* %current_45args47516) {
%stackaddr$prim49181 = alloca %struct.ScmObj*, align 8
%k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47516)
store volatile %struct.ScmObj* %k40472, %struct.ScmObj** %stackaddr$prim49181, align 8
%stackaddr$prim49182 = alloca %struct.ScmObj*, align 8
%current_45args47517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47516)
store volatile %struct.ScmObj* %current_45args47517, %struct.ScmObj** %stackaddr$prim49182, align 8
%stackaddr$prim49183 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47517)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim49183, align 8
%stackaddr$prim49184 = alloca %struct.ScmObj*, align 8
%current_45args47518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47517)
store volatile %struct.ScmObj* %current_45args47518, %struct.ScmObj** %stackaddr$prim49184, align 8
%stackaddr$prim49185 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47518)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim49185, align 8
%truthy$cmp49186 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp49186 = icmp eq i64 %truthy$cmp49186, 1
br i1 %cmp$cmp49186, label %truebranch$cmp49186, label %falsebranch$cmp49186
truebranch$cmp49186:
%ae41230 = call %struct.ScmObj* @const_init_int(i64 0)
%args47520$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49187 = alloca %struct.ScmObj*, align 8
%args47520$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args47520$k40472$0)
store volatile %struct.ScmObj* %args47520$k40472$1, %struct.ScmObj** %stackaddr$prim49187, align 8
%stackaddr$prim49188 = alloca %struct.ScmObj*, align 8
%args47520$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41230, %struct.ScmObj* %args47520$k40472$1)
store volatile %struct.ScmObj* %args47520$k40472$2, %struct.ScmObj** %stackaddr$prim49188, align 8
%clofunc49189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc49189(%struct.ScmObj* %k40472, %struct.ScmObj* %args47520$k40472$2)
ret void
falsebranch$cmp49186:
%stackaddr$prim49190 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim49190, align 8
%ae41237 = call %struct.ScmObj* @const_init_int(i64 0)
%args47521$k40472$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49191 = alloca %struct.ScmObj*, align 8
%args47521$k40472$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args47521$k40472$0)
store volatile %struct.ScmObj* %args47521$k40472$1, %struct.ScmObj** %stackaddr$prim49191, align 8
%stackaddr$prim49192 = alloca %struct.ScmObj*, align 8
%args47521$k40472$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41237, %struct.ScmObj* %args47521$k40472$1)
store volatile %struct.ScmObj* %args47521$k40472$2, %struct.ScmObj** %stackaddr$prim49192, align 8
%clofunc49193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40472)
musttail call tailcc void %clofunc49193(%struct.ScmObj* %k40472, %struct.ScmObj* %args47521$k40472$2)
ret void
}

define tailcc void @proc_clo$ae41184(%struct.ScmObj* %env$ae41184,%struct.ScmObj* %current_45args47525) {
%stackaddr$env-ref49194 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41184, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref49194
%stackaddr$env-ref49195 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41184, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref49195
%stackaddr$prim49196 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47525)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim49196, align 8
%stackaddr$prim49197 = alloca %struct.ScmObj*, align 8
%current_45args47526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47525)
store volatile %struct.ScmObj* %current_45args47526, %struct.ScmObj** %stackaddr$prim49197, align 8
%stackaddr$prim49198 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47526)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim49198, align 8
%stackaddr$prim49199 = alloca %struct.ScmObj*, align 8
%current_45args47527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47526)
store volatile %struct.ScmObj* %current_45args47527, %struct.ScmObj** %stackaddr$prim49199, align 8
%stackaddr$prim49200 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47527)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim49200, align 8
%stackaddr$makeclosure49201 = alloca %struct.ScmObj*, align 8
%fptrToInt49202 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41186 to i64
%ae41186 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49202)
store volatile %struct.ScmObj* %ae41186, %struct.ScmObj** %stackaddr$makeclosure49201, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %lst40144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %n40143, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41186, %struct.ScmObj* %k40474, i64 3)
%args47533$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49203 = alloca %struct.ScmObj*, align 8
%args47533$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47533$_37length40112$0)
store volatile %struct.ScmObj* %args47533$_37length40112$1, %struct.ScmObj** %stackaddr$prim49203, align 8
%stackaddr$prim49204 = alloca %struct.ScmObj*, align 8
%args47533$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41186, %struct.ScmObj* %args47533$_37length40112$1)
store volatile %struct.ScmObj* %args47533$_37length40112$2, %struct.ScmObj** %stackaddr$prim49204, align 8
%clofunc49205 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc49205(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args47533$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41186(%struct.ScmObj* %env$ae41186,%struct.ScmObj* %current_45args47529) {
%stackaddr$env-ref49206 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref49206
%stackaddr$env-ref49207 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 1)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref49207
%stackaddr$env-ref49208 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 2)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref49208
%stackaddr$env-ref49209 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41186, i64 3)
store %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$env-ref49209
%stackaddr$prim49210 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47529)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim49210, align 8
%stackaddr$prim49211 = alloca %struct.ScmObj*, align 8
%current_45args47530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47529)
store volatile %struct.ScmObj* %current_45args47530, %struct.ScmObj** %stackaddr$prim49211, align 8
%stackaddr$prim49212 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47530)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim49212, align 8
%stackaddr$prim49213 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim49213, align 8
%args47532$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49214 = alloca %struct.ScmObj*, align 8
%args47532$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args47532$_37take40115$0)
store volatile %struct.ScmObj* %args47532$_37take40115$1, %struct.ScmObj** %stackaddr$prim49214, align 8
%stackaddr$prim49215 = alloca %struct.ScmObj*, align 8
%args47532$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47532$_37take40115$1)
store volatile %struct.ScmObj* %args47532$_37take40115$2, %struct.ScmObj** %stackaddr$prim49215, align 8
%stackaddr$prim49216 = alloca %struct.ScmObj*, align 8
%args47532$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40474, %struct.ScmObj* %args47532$_37take40115$2)
store volatile %struct.ScmObj* %args47532$_37take40115$3, %struct.ScmObj** %stackaddr$prim49216, align 8
%clofunc49217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc49217(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args47532$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41130(%struct.ScmObj* %env$ae41130,%struct.ScmObj* %current_45args47535) {
%stackaddr$env-ref49218 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41130, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref49218
%stackaddr$prim49219 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47535)
store volatile %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$prim49219, align 8
%stackaddr$prim49220 = alloca %struct.ScmObj*, align 8
%current_45args47536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47535)
store volatile %struct.ScmObj* %current_45args47536, %struct.ScmObj** %stackaddr$prim49220, align 8
%stackaddr$prim49221 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47536)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim49221, align 8
%stackaddr$makeclosure49222 = alloca %struct.ScmObj*, align 8
%fptrToInt49223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41131 to i64
%ae41131 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49223)
store volatile %struct.ScmObj* %ae41131, %struct.ScmObj** %stackaddr$makeclosure49222, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41131, %struct.ScmObj* %lst40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41131, %struct.ScmObj* %k40476, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41131, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41132 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49224 = alloca %struct.ScmObj*, align 8
%fptrToInt49225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41133 to i64
%ae41133 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49225)
store volatile %struct.ScmObj* %ae41133, %struct.ScmObj** %stackaddr$makeclosure49224, align 8
%args47547$ae41131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49226 = alloca %struct.ScmObj*, align 8
%args47547$ae41131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41133, %struct.ScmObj* %args47547$ae41131$0)
store volatile %struct.ScmObj* %args47547$ae41131$1, %struct.ScmObj** %stackaddr$prim49226, align 8
%stackaddr$prim49227 = alloca %struct.ScmObj*, align 8
%args47547$ae41131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41132, %struct.ScmObj* %args47547$ae41131$1)
store volatile %struct.ScmObj* %args47547$ae41131$2, %struct.ScmObj** %stackaddr$prim49227, align 8
%clofunc49228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41131)
musttail call tailcc void %clofunc49228(%struct.ScmObj* %ae41131, %struct.ScmObj* %args47547$ae41131$2)
ret void
}

define tailcc void @proc_clo$ae41131(%struct.ScmObj* %env$ae41131,%struct.ScmObj* %current_45args47538) {
%stackaddr$env-ref49229 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41131, i64 0)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref49229
%stackaddr$env-ref49230 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41131, i64 1)
store %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$env-ref49230
%stackaddr$env-ref49231 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41131, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref49231
%stackaddr$prim49232 = alloca %struct.ScmObj*, align 8
%_95k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47538)
store volatile %struct.ScmObj* %_95k40477, %struct.ScmObj** %stackaddr$prim49232, align 8
%stackaddr$prim49233 = alloca %struct.ScmObj*, align 8
%current_45args47539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47538)
store volatile %struct.ScmObj* %current_45args47539, %struct.ScmObj** %stackaddr$prim49233, align 8
%stackaddr$prim49234 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47539)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim49234, align 8
%ae41152 = call %struct.ScmObj* @const_init_null()
%args47541$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49235 = alloca %struct.ScmObj*, align 8
%args47541$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args47541$_37foldl140107$0)
store volatile %struct.ScmObj* %args47541$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim49235, align 8
%stackaddr$prim49236 = alloca %struct.ScmObj*, align 8
%args47541$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41152, %struct.ScmObj* %args47541$_37foldl140107$1)
store volatile %struct.ScmObj* %args47541$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim49236, align 8
%stackaddr$prim49237 = alloca %struct.ScmObj*, align 8
%args47541$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args47541$_37foldl140107$2)
store volatile %struct.ScmObj* %args47541$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim49237, align 8
%stackaddr$prim49238 = alloca %struct.ScmObj*, align 8
%args47541$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40476, %struct.ScmObj* %args47541$_37foldl140107$3)
store volatile %struct.ScmObj* %args47541$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim49238, align 8
%clofunc49239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc49239(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47541$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41133(%struct.ScmObj* %env$ae41133,%struct.ScmObj* %current_45args47542) {
%stackaddr$prim49240 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47542)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim49240, align 8
%stackaddr$prim49241 = alloca %struct.ScmObj*, align 8
%current_45args47543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47542)
store volatile %struct.ScmObj* %current_45args47543, %struct.ScmObj** %stackaddr$prim49241, align 8
%stackaddr$prim49242 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47543)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim49242, align 8
%stackaddr$prim49243 = alloca %struct.ScmObj*, align 8
%current_45args47544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47543)
store volatile %struct.ScmObj* %current_45args47544, %struct.ScmObj** %stackaddr$prim49243, align 8
%stackaddr$prim49244 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47544)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim49244, align 8
%ae41135 = call %struct.ScmObj* @const_init_int(i64 0)
%args47546$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49245 = alloca %struct.ScmObj*, align 8
%args47546$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args47546$k40478$0)
store volatile %struct.ScmObj* %args47546$k40478$1, %struct.ScmObj** %stackaddr$prim49245, align 8
%stackaddr$prim49246 = alloca %struct.ScmObj*, align 8
%args47546$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41135, %struct.ScmObj* %args47546$k40478$1)
store volatile %struct.ScmObj* %args47546$k40478$2, %struct.ScmObj** %stackaddr$prim49246, align 8
%clofunc49247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc49247(%struct.ScmObj* %k40478, %struct.ScmObj* %args47546$k40478$2)
ret void
}

define tailcc void @proc_clo$ae41051(%struct.ScmObj* %env$ae41051,%struct.ScmObj* %current_45args47550) {
%stackaddr$prim49248 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47550)
store volatile %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$prim49248, align 8
%stackaddr$prim49249 = alloca %struct.ScmObj*, align 8
%current_45args47551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47550)
store volatile %struct.ScmObj* %current_45args47551, %struct.ScmObj** %stackaddr$prim49249, align 8
%stackaddr$prim49250 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47551)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim49250, align 8
%ae41053 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49251 = alloca %struct.ScmObj*, align 8
%fptrToInt49252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41054 to i64
%ae41054 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49252)
store volatile %struct.ScmObj* %ae41054, %struct.ScmObj** %stackaddr$makeclosure49251, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41054, %struct.ScmObj* %_37foldl140108, i64 0)
%args47564$k40479$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49253 = alloca %struct.ScmObj*, align 8
%args47564$k40479$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41054, %struct.ScmObj* %args47564$k40479$0)
store volatile %struct.ScmObj* %args47564$k40479$1, %struct.ScmObj** %stackaddr$prim49253, align 8
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%args47564$k40479$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41053, %struct.ScmObj* %args47564$k40479$1)
store volatile %struct.ScmObj* %args47564$k40479$2, %struct.ScmObj** %stackaddr$prim49254, align 8
%clofunc49255 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40479)
musttail call tailcc void %clofunc49255(%struct.ScmObj* %k40479, %struct.ScmObj* %args47564$k40479$2)
ret void
}

define tailcc void @proc_clo$ae41054(%struct.ScmObj* %env$ae41054,%struct.ScmObj* %current_45args47553) {
%stackaddr$env-ref49256 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41054, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49256
%stackaddr$prim49257 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47553)
store volatile %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$prim49257, align 8
%stackaddr$prim49258 = alloca %struct.ScmObj*, align 8
%current_45args47554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47553)
store volatile %struct.ScmObj* %current_45args47554, %struct.ScmObj** %stackaddr$prim49258, align 8
%stackaddr$prim49259 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47554)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim49259, align 8
%stackaddr$prim49260 = alloca %struct.ScmObj*, align 8
%current_45args47555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47554)
store volatile %struct.ScmObj* %current_45args47555, %struct.ScmObj** %stackaddr$prim49260, align 8
%stackaddr$prim49261 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47555)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim49261, align 8
%stackaddr$prim49262 = alloca %struct.ScmObj*, align 8
%current_45args47556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47555)
store volatile %struct.ScmObj* %current_45args47556, %struct.ScmObj** %stackaddr$prim49262, align 8
%stackaddr$prim49263 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47556)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim49263, align 8
%stackaddr$prim49264 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim49264, align 8
%truthy$cmp49265 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40249)
%cmp$cmp49265 = icmp eq i64 %truthy$cmp49265, 1
br i1 %cmp$cmp49265, label %truebranch$cmp49265, label %falsebranch$cmp49265
truebranch$cmp49265:
%ae41058 = call %struct.ScmObj* @const_init_int(i64 0)
%args47558$k40480$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49266 = alloca %struct.ScmObj*, align 8
%args47558$k40480$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47558$k40480$0)
store volatile %struct.ScmObj* %args47558$k40480$1, %struct.ScmObj** %stackaddr$prim49266, align 8
%stackaddr$prim49267 = alloca %struct.ScmObj*, align 8
%args47558$k40480$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41058, %struct.ScmObj* %args47558$k40480$1)
store volatile %struct.ScmObj* %args47558$k40480$2, %struct.ScmObj** %stackaddr$prim49267, align 8
%clofunc49268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40480)
musttail call tailcc void %clofunc49268(%struct.ScmObj* %k40480, %struct.ScmObj* %args47558$k40480$2)
ret void
falsebranch$cmp49265:
%stackaddr$prim49269 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim49269, align 8
%stackaddr$makeclosure49270 = alloca %struct.ScmObj*, align 8
%fptrToInt49271 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41065 to i64
%ae41065 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49271)
store volatile %struct.ScmObj* %ae41065, %struct.ScmObj** %stackaddr$makeclosure49270, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41065, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41065, %struct.ScmObj* %f40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41065, %struct.ScmObj* %lst40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41065, %struct.ScmObj* %_37foldl140108, i64 3)
%args47563$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49272 = alloca %struct.ScmObj*, align 8
%args47563$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47563$f40111$0)
store volatile %struct.ScmObj* %args47563$f40111$1, %struct.ScmObj** %stackaddr$prim49272, align 8
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%args47563$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args47563$f40111$1)
store volatile %struct.ScmObj* %args47563$f40111$2, %struct.ScmObj** %stackaddr$prim49273, align 8
%stackaddr$prim49274 = alloca %struct.ScmObj*, align 8
%args47563$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41065, %struct.ScmObj* %args47563$f40111$2)
store volatile %struct.ScmObj* %args47563$f40111$3, %struct.ScmObj** %stackaddr$prim49274, align 8
%clofunc49275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc49275(%struct.ScmObj* %f40111, %struct.ScmObj* %args47563$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41065(%struct.ScmObj* %env$ae41065,%struct.ScmObj* %current_45args47559) {
%stackaddr$env-ref49276 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41065, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref49276
%stackaddr$env-ref49277 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41065, i64 1)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref49277
%stackaddr$env-ref49278 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41065, i64 2)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref49278
%stackaddr$env-ref49279 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41065, i64 3)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49279
%stackaddr$prim49280 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47559)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim49280, align 8
%stackaddr$prim49281 = alloca %struct.ScmObj*, align 8
%current_45args47560 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47559)
store volatile %struct.ScmObj* %current_45args47560, %struct.ScmObj** %stackaddr$prim49281, align 8
%stackaddr$prim49282 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47560)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim49282, align 8
%stackaddr$prim49283 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim49283, align 8
%args47562$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49284 = alloca %struct.ScmObj*, align 8
%args47562$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args47562$_37foldl140108$0)
store volatile %struct.ScmObj* %args47562$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim49284, align 8
%stackaddr$prim49285 = alloca %struct.ScmObj*, align 8
%args47562$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args47562$_37foldl140108$1)
store volatile %struct.ScmObj* %args47562$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim49285, align 8
%stackaddr$prim49286 = alloca %struct.ScmObj*, align 8
%args47562$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args47562$_37foldl140108$2)
store volatile %struct.ScmObj* %args47562$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim49286, align 8
%stackaddr$prim49287 = alloca %struct.ScmObj*, align 8
%args47562$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40480, %struct.ScmObj* %args47562$_37foldl140108$3)
store volatile %struct.ScmObj* %args47562$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim49287, align 8
%clofunc49288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc49288(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args47562$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae40968(%struct.ScmObj* %env$ae40968,%struct.ScmObj* %current_45args47567) {
%stackaddr$prim49289 = alloca %struct.ScmObj*, align 8
%k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47567)
store volatile %struct.ScmObj* %k40482, %struct.ScmObj** %stackaddr$prim49289, align 8
%stackaddr$prim49290 = alloca %struct.ScmObj*, align 8
%current_45args47568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47567)
store volatile %struct.ScmObj* %current_45args47568, %struct.ScmObj** %stackaddr$prim49290, align 8
%stackaddr$prim49291 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47568)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim49291, align 8
%ae40970 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49292 = alloca %struct.ScmObj*, align 8
%fptrToInt49293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40971 to i64
%ae40971 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49293)
store volatile %struct.ScmObj* %ae40971, %struct.ScmObj** %stackaddr$makeclosure49292, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40971, %struct.ScmObj* %_37length40113, i64 0)
%args47579$k40482$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49294 = alloca %struct.ScmObj*, align 8
%args47579$k40482$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40971, %struct.ScmObj* %args47579$k40482$0)
store volatile %struct.ScmObj* %args47579$k40482$1, %struct.ScmObj** %stackaddr$prim49294, align 8
%stackaddr$prim49295 = alloca %struct.ScmObj*, align 8
%args47579$k40482$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40970, %struct.ScmObj* %args47579$k40482$1)
store volatile %struct.ScmObj* %args47579$k40482$2, %struct.ScmObj** %stackaddr$prim49295, align 8
%clofunc49296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40482)
musttail call tailcc void %clofunc49296(%struct.ScmObj* %k40482, %struct.ScmObj* %args47579$k40482$2)
ret void
}

define tailcc void @proc_clo$ae40971(%struct.ScmObj* %env$ae40971,%struct.ScmObj* %current_45args47570) {
%stackaddr$env-ref49297 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40971, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref49297
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47570)
store volatile %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$prim49298, align 8
%stackaddr$prim49299 = alloca %struct.ScmObj*, align 8
%current_45args47571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47570)
store volatile %struct.ScmObj* %current_45args47571, %struct.ScmObj** %stackaddr$prim49299, align 8
%stackaddr$prim49300 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47571)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim49300, align 8
%stackaddr$prim49301 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim49301, align 8
%truthy$cmp49302 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40245)
%cmp$cmp49302 = icmp eq i64 %truthy$cmp49302, 1
br i1 %cmp$cmp49302, label %truebranch$cmp49302, label %falsebranch$cmp49302
truebranch$cmp49302:
%ae40975 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40976 = call %struct.ScmObj* @const_init_int(i64 0)
%args47573$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49303 = alloca %struct.ScmObj*, align 8
%args47573$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40976, %struct.ScmObj* %args47573$k40483$0)
store volatile %struct.ScmObj* %args47573$k40483$1, %struct.ScmObj** %stackaddr$prim49303, align 8
%stackaddr$prim49304 = alloca %struct.ScmObj*, align 8
%args47573$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40975, %struct.ScmObj* %args47573$k40483$1)
store volatile %struct.ScmObj* %args47573$k40483$2, %struct.ScmObj** %stackaddr$prim49304, align 8
%clofunc49305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc49305(%struct.ScmObj* %k40483, %struct.ScmObj* %args47573$k40483$2)
ret void
falsebranch$cmp49302:
%stackaddr$prim49306 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim49306, align 8
%stackaddr$makeclosure49307 = alloca %struct.ScmObj*, align 8
%fptrToInt49308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40985 to i64
%ae40985 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49308)
store volatile %struct.ScmObj* %ae40985, %struct.ScmObj** %stackaddr$makeclosure49307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40985, %struct.ScmObj* %k40483, i64 0)
%args47578$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49309 = alloca %struct.ScmObj*, align 8
%args47578$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args47578$_37length40113$0)
store volatile %struct.ScmObj* %args47578$_37length40113$1, %struct.ScmObj** %stackaddr$prim49309, align 8
%stackaddr$prim49310 = alloca %struct.ScmObj*, align 8
%args47578$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40985, %struct.ScmObj* %args47578$_37length40113$1)
store volatile %struct.ScmObj* %args47578$_37length40113$2, %struct.ScmObj** %stackaddr$prim49310, align 8
%clofunc49311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc49311(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args47578$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae40985(%struct.ScmObj* %env$ae40985,%struct.ScmObj* %current_45args47574) {
%stackaddr$env-ref49312 = alloca %struct.ScmObj*, align 8
%k40483 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40985, i64 0)
store %struct.ScmObj* %k40483, %struct.ScmObj** %stackaddr$env-ref49312
%stackaddr$prim49313 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47574)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim49313, align 8
%stackaddr$prim49314 = alloca %struct.ScmObj*, align 8
%current_45args47575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47574)
store volatile %struct.ScmObj* %current_45args47575, %struct.ScmObj** %stackaddr$prim49314, align 8
%stackaddr$prim49315 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47575)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim49315, align 8
%ae40987 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49316 = alloca %struct.ScmObj*, align 8
%cpsprim40485 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae40987, %struct.ScmObj* %anf_45bind40247)
store volatile %struct.ScmObj* %cpsprim40485, %struct.ScmObj** %stackaddr$prim49316, align 8
%ae40990 = call %struct.ScmObj* @const_init_int(i64 0)
%args47577$k40483$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49317 = alloca %struct.ScmObj*, align 8
%args47577$k40483$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40485, %struct.ScmObj* %args47577$k40483$0)
store volatile %struct.ScmObj* %args47577$k40483$1, %struct.ScmObj** %stackaddr$prim49317, align 8
%stackaddr$prim49318 = alloca %struct.ScmObj*, align 8
%args47577$k40483$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40990, %struct.ScmObj* %args47577$k40483$1)
store volatile %struct.ScmObj* %args47577$k40483$2, %struct.ScmObj** %stackaddr$prim49318, align 8
%clofunc49319 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40483)
musttail call tailcc void %clofunc49319(%struct.ScmObj* %k40483, %struct.ScmObj* %args47577$k40483$2)
ret void
}

define tailcc void @proc_clo$ae40818(%struct.ScmObj* %env$ae40818,%struct.ScmObj* %current_45args47582) {
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47582)
store volatile %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$prim49320, align 8
%stackaddr$prim49321 = alloca %struct.ScmObj*, align 8
%current_45args47583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47582)
store volatile %struct.ScmObj* %current_45args47583, %struct.ScmObj** %stackaddr$prim49321, align 8
%stackaddr$prim49322 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47583)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim49322, align 8
%ae40820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49323 = alloca %struct.ScmObj*, align 8
%fptrToInt49324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40821 to i64
%ae40821 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49324)
store volatile %struct.ScmObj* %ae40821, %struct.ScmObj** %stackaddr$makeclosure49323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40821, %struct.ScmObj* %_37take40116, i64 0)
%args47596$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49325 = alloca %struct.ScmObj*, align 8
%args47596$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40821, %struct.ScmObj* %args47596$k40486$0)
store volatile %struct.ScmObj* %args47596$k40486$1, %struct.ScmObj** %stackaddr$prim49325, align 8
%stackaddr$prim49326 = alloca %struct.ScmObj*, align 8
%args47596$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40820, %struct.ScmObj* %args47596$k40486$1)
store volatile %struct.ScmObj* %args47596$k40486$2, %struct.ScmObj** %stackaddr$prim49326, align 8
%clofunc49327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc49327(%struct.ScmObj* %k40486, %struct.ScmObj* %args47596$k40486$2)
ret void
}

define tailcc void @proc_clo$ae40821(%struct.ScmObj* %env$ae40821,%struct.ScmObj* %current_45args47585) {
%stackaddr$env-ref49328 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40821, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref49328
%stackaddr$prim49329 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47585)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim49329, align 8
%stackaddr$prim49330 = alloca %struct.ScmObj*, align 8
%current_45args47586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47585)
store volatile %struct.ScmObj* %current_45args47586, %struct.ScmObj** %stackaddr$prim49330, align 8
%stackaddr$prim49331 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47586)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim49331, align 8
%stackaddr$prim49332 = alloca %struct.ScmObj*, align 8
%current_45args47587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47586)
store volatile %struct.ScmObj* %current_45args47587, %struct.ScmObj** %stackaddr$prim49332, align 8
%stackaddr$prim49333 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47587)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim49333, align 8
%ae40823 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49334 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40823)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim49334, align 8
%truthy$cmp49335 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40238)
%cmp$cmp49335 = icmp eq i64 %truthy$cmp49335, 1
br i1 %cmp$cmp49335, label %truebranch$cmp49335, label %falsebranch$cmp49335
truebranch$cmp49335:
%ae40826 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40827 = call %struct.ScmObj* @const_init_null()
%args47589$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49336 = alloca %struct.ScmObj*, align 8
%args47589$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40827, %struct.ScmObj* %args47589$k40487$0)
store volatile %struct.ScmObj* %args47589$k40487$1, %struct.ScmObj** %stackaddr$prim49336, align 8
%stackaddr$prim49337 = alloca %struct.ScmObj*, align 8
%args47589$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40826, %struct.ScmObj* %args47589$k40487$1)
store volatile %struct.ScmObj* %args47589$k40487$2, %struct.ScmObj** %stackaddr$prim49337, align 8
%clofunc49338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc49338(%struct.ScmObj* %k40487, %struct.ScmObj* %args47589$k40487$2)
ret void
falsebranch$cmp49335:
%stackaddr$prim49339 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim49339, align 8
%truthy$cmp49340 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40239)
%cmp$cmp49340 = icmp eq i64 %truthy$cmp49340, 1
br i1 %cmp$cmp49340, label %truebranch$cmp49340, label %falsebranch$cmp49340
truebranch$cmp49340:
%ae40837 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40838 = call %struct.ScmObj* @const_init_null()
%args47590$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49341 = alloca %struct.ScmObj*, align 8
%args47590$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40838, %struct.ScmObj* %args47590$k40487$0)
store volatile %struct.ScmObj* %args47590$k40487$1, %struct.ScmObj** %stackaddr$prim49341, align 8
%stackaddr$prim49342 = alloca %struct.ScmObj*, align 8
%args47590$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40837, %struct.ScmObj* %args47590$k40487$1)
store volatile %struct.ScmObj* %args47590$k40487$2, %struct.ScmObj** %stackaddr$prim49342, align 8
%clofunc49343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc49343(%struct.ScmObj* %k40487, %struct.ScmObj* %args47590$k40487$2)
ret void
falsebranch$cmp49340:
%stackaddr$prim49344 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim49344, align 8
%stackaddr$prim49345 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim49345, align 8
%ae40848 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49346 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40848)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim49346, align 8
%stackaddr$makeclosure49347 = alloca %struct.ScmObj*, align 8
%fptrToInt49348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40850 to i64
%ae40850 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49348)
store volatile %struct.ScmObj* %ae40850, %struct.ScmObj** %stackaddr$makeclosure49347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40850, %struct.ScmObj* %k40487, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40850, %struct.ScmObj* %anf_45bind40240, i64 1)
%args47595$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49349 = alloca %struct.ScmObj*, align 8
%args47595$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args47595$_37take40116$0)
store volatile %struct.ScmObj* %args47595$_37take40116$1, %struct.ScmObj** %stackaddr$prim49349, align 8
%stackaddr$prim49350 = alloca %struct.ScmObj*, align 8
%args47595$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %args47595$_37take40116$1)
store volatile %struct.ScmObj* %args47595$_37take40116$2, %struct.ScmObj** %stackaddr$prim49350, align 8
%stackaddr$prim49351 = alloca %struct.ScmObj*, align 8
%args47595$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40850, %struct.ScmObj* %args47595$_37take40116$2)
store volatile %struct.ScmObj* %args47595$_37take40116$3, %struct.ScmObj** %stackaddr$prim49351, align 8
%clofunc49352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc49352(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args47595$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40850(%struct.ScmObj* %env$ae40850,%struct.ScmObj* %current_45args47591) {
%stackaddr$env-ref49353 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40850, i64 0)
store %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$env-ref49353
%stackaddr$env-ref49354 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40850, i64 1)
store %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$env-ref49354
%stackaddr$prim49355 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47591)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim49355, align 8
%stackaddr$prim49356 = alloca %struct.ScmObj*, align 8
%current_45args47592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47591)
store volatile %struct.ScmObj* %current_45args47592, %struct.ScmObj** %stackaddr$prim49356, align 8
%stackaddr$prim49357 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47592)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim49357, align 8
%stackaddr$prim49358 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %anf_45bind40243)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim49358, align 8
%ae40856 = call %struct.ScmObj* @const_init_int(i64 0)
%args47594$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49359 = alloca %struct.ScmObj*, align 8
%args47594$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args47594$k40487$0)
store volatile %struct.ScmObj* %args47594$k40487$1, %struct.ScmObj** %stackaddr$prim49359, align 8
%stackaddr$prim49360 = alloca %struct.ScmObj*, align 8
%args47594$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40856, %struct.ScmObj* %args47594$k40487$1)
store volatile %struct.ScmObj* %args47594$k40487$2, %struct.ScmObj** %stackaddr$prim49360, align 8
%clofunc49361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc49361(%struct.ScmObj* %k40487, %struct.ScmObj* %args47594$k40487$2)
ret void
}

define tailcc void @proc_clo$ae40721(%struct.ScmObj* %env$ae40721,%struct.ScmObj* %current_45args47599) {
%stackaddr$prim49362 = alloca %struct.ScmObj*, align 8
%k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %k40490, %struct.ScmObj** %stackaddr$prim49362, align 8
%stackaddr$prim49363 = alloca %struct.ScmObj*, align 8
%current_45args47600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %current_45args47600, %struct.ScmObj** %stackaddr$prim49363, align 8
%stackaddr$prim49364 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47600)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim49364, align 8
%ae40723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49365 = alloca %struct.ScmObj*, align 8
%fptrToInt49366 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40724 to i64
%ae40724 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49366)
store volatile %struct.ScmObj* %ae40724, %struct.ScmObj** %stackaddr$makeclosure49365, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40724, %struct.ScmObj* %_37map40120, i64 0)
%args47616$k40490$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49367 = alloca %struct.ScmObj*, align 8
%args47616$k40490$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40724, %struct.ScmObj* %args47616$k40490$0)
store volatile %struct.ScmObj* %args47616$k40490$1, %struct.ScmObj** %stackaddr$prim49367, align 8
%stackaddr$prim49368 = alloca %struct.ScmObj*, align 8
%args47616$k40490$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40723, %struct.ScmObj* %args47616$k40490$1)
store volatile %struct.ScmObj* %args47616$k40490$2, %struct.ScmObj** %stackaddr$prim49368, align 8
%clofunc49369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40490)
musttail call tailcc void %clofunc49369(%struct.ScmObj* %k40490, %struct.ScmObj* %args47616$k40490$2)
ret void
}

define tailcc void @proc_clo$ae40724(%struct.ScmObj* %env$ae40724,%struct.ScmObj* %current_45args47602) {
%stackaddr$env-ref49370 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40724, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref49370
%stackaddr$prim49371 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47602)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim49371, align 8
%stackaddr$prim49372 = alloca %struct.ScmObj*, align 8
%current_45args47603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47602)
store volatile %struct.ScmObj* %current_45args47603, %struct.ScmObj** %stackaddr$prim49372, align 8
%stackaddr$prim49373 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47603)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim49373, align 8
%stackaddr$prim49374 = alloca %struct.ScmObj*, align 8
%current_45args47604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47603)
store volatile %struct.ScmObj* %current_45args47604, %struct.ScmObj** %stackaddr$prim49374, align 8
%stackaddr$prim49375 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47604)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim49375, align 8
%stackaddr$prim49376 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim49376, align 8
%truthy$cmp49377 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40232)
%cmp$cmp49377 = icmp eq i64 %truthy$cmp49377, 1
br i1 %cmp$cmp49377, label %truebranch$cmp49377, label %falsebranch$cmp49377
truebranch$cmp49377:
%ae40728 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40729 = call %struct.ScmObj* @const_init_null()
%args47606$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49378 = alloca %struct.ScmObj*, align 8
%args47606$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40729, %struct.ScmObj* %args47606$k40491$0)
store volatile %struct.ScmObj* %args47606$k40491$1, %struct.ScmObj** %stackaddr$prim49378, align 8
%stackaddr$prim49379 = alloca %struct.ScmObj*, align 8
%args47606$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40728, %struct.ScmObj* %args47606$k40491$1)
store volatile %struct.ScmObj* %args47606$k40491$2, %struct.ScmObj** %stackaddr$prim49379, align 8
%clofunc49380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc49380(%struct.ScmObj* %k40491, %struct.ScmObj* %args47606$k40491$2)
ret void
falsebranch$cmp49377:
%stackaddr$prim49381 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim49381, align 8
%stackaddr$makeclosure49382 = alloca %struct.ScmObj*, align 8
%fptrToInt49383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40738 to i64
%ae40738 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49383)
store volatile %struct.ScmObj* %ae40738, %struct.ScmObj** %stackaddr$makeclosure49382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40738, %struct.ScmObj* %lst40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40738, %struct.ScmObj* %_37map40120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40738, %struct.ScmObj* %k40491, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40738, %struct.ScmObj* %f40122, i64 3)
%args47615$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49384 = alloca %struct.ScmObj*, align 8
%args47615$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args47615$f40122$0)
store volatile %struct.ScmObj* %args47615$f40122$1, %struct.ScmObj** %stackaddr$prim49384, align 8
%stackaddr$prim49385 = alloca %struct.ScmObj*, align 8
%args47615$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40738, %struct.ScmObj* %args47615$f40122$1)
store volatile %struct.ScmObj* %args47615$f40122$2, %struct.ScmObj** %stackaddr$prim49385, align 8
%clofunc49386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc49386(%struct.ScmObj* %f40122, %struct.ScmObj* %args47615$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40738(%struct.ScmObj* %env$ae40738,%struct.ScmObj* %current_45args47607) {
%stackaddr$env-ref49387 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40738, i64 0)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref49387
%stackaddr$env-ref49388 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40738, i64 1)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref49388
%stackaddr$env-ref49389 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40738, i64 2)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref49389
%stackaddr$env-ref49390 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40738, i64 3)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref49390
%stackaddr$prim49391 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47607)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim49391, align 8
%stackaddr$prim49392 = alloca %struct.ScmObj*, align 8
%current_45args47608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47607)
store volatile %struct.ScmObj* %current_45args47608, %struct.ScmObj** %stackaddr$prim49392, align 8
%stackaddr$prim49393 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47608)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim49393, align 8
%stackaddr$prim49394 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim49394, align 8
%stackaddr$makeclosure49395 = alloca %struct.ScmObj*, align 8
%fptrToInt49396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40742 to i64
%ae40742 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49396)
store volatile %struct.ScmObj* %ae40742, %struct.ScmObj** %stackaddr$makeclosure49395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40742, %struct.ScmObj* %anf_45bind40234, i64 1)
%args47614$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49397 = alloca %struct.ScmObj*, align 8
%args47614$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args47614$_37map40120$0)
store volatile %struct.ScmObj* %args47614$_37map40120$1, %struct.ScmObj** %stackaddr$prim49397, align 8
%stackaddr$prim49398 = alloca %struct.ScmObj*, align 8
%args47614$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args47614$_37map40120$1)
store volatile %struct.ScmObj* %args47614$_37map40120$2, %struct.ScmObj** %stackaddr$prim49398, align 8
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%args47614$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40742, %struct.ScmObj* %args47614$_37map40120$2)
store volatile %struct.ScmObj* %args47614$_37map40120$3, %struct.ScmObj** %stackaddr$prim49399, align 8
%clofunc49400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc49400(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args47614$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40742(%struct.ScmObj* %env$ae40742,%struct.ScmObj* %current_45args47610) {
%stackaddr$env-ref49401 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref49401
%stackaddr$env-ref49402 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40742, i64 1)
store %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$env-ref49402
%stackaddr$prim49403 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47610)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim49403, align 8
%stackaddr$prim49404 = alloca %struct.ScmObj*, align 8
%current_45args47611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47610)
store volatile %struct.ScmObj* %current_45args47611, %struct.ScmObj** %stackaddr$prim49404, align 8
%stackaddr$prim49405 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47611)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim49405, align 8
%stackaddr$prim49406 = alloca %struct.ScmObj*, align 8
%cpsprim40494 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %anf_45bind40236)
store volatile %struct.ScmObj* %cpsprim40494, %struct.ScmObj** %stackaddr$prim49406, align 8
%ae40748 = call %struct.ScmObj* @const_init_int(i64 0)
%args47613$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49407 = alloca %struct.ScmObj*, align 8
%args47613$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40494, %struct.ScmObj* %args47613$k40491$0)
store volatile %struct.ScmObj* %args47613$k40491$1, %struct.ScmObj** %stackaddr$prim49407, align 8
%stackaddr$prim49408 = alloca %struct.ScmObj*, align 8
%args47613$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40748, %struct.ScmObj* %args47613$k40491$1)
store volatile %struct.ScmObj* %args47613$k40491$2, %struct.ScmObj** %stackaddr$prim49408, align 8
%clofunc49409 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc49409(%struct.ScmObj* %k40491, %struct.ScmObj* %args47613$k40491$2)
ret void
}

define tailcc void @proc_clo$ae40641(%struct.ScmObj* %env$ae40641,%struct.ScmObj* %current_45args47619) {
%stackaddr$prim49410 = alloca %struct.ScmObj*, align 8
%k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47619)
store volatile %struct.ScmObj* %k40495, %struct.ScmObj** %stackaddr$prim49410, align 8
%stackaddr$prim49411 = alloca %struct.ScmObj*, align 8
%current_45args47620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47619)
store volatile %struct.ScmObj* %current_45args47620, %struct.ScmObj** %stackaddr$prim49411, align 8
%stackaddr$prim49412 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47620)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim49412, align 8
%ae40643 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49413 = alloca %struct.ScmObj*, align 8
%fptrToInt49414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40644 to i64
%ae40644 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49414)
store volatile %struct.ScmObj* %ae40644, %struct.ScmObj** %stackaddr$makeclosure49413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40644, %struct.ScmObj* %_37foldr140124, i64 0)
%args47633$k40495$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49415 = alloca %struct.ScmObj*, align 8
%args47633$k40495$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40644, %struct.ScmObj* %args47633$k40495$0)
store volatile %struct.ScmObj* %args47633$k40495$1, %struct.ScmObj** %stackaddr$prim49415, align 8
%stackaddr$prim49416 = alloca %struct.ScmObj*, align 8
%args47633$k40495$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40643, %struct.ScmObj* %args47633$k40495$1)
store volatile %struct.ScmObj* %args47633$k40495$2, %struct.ScmObj** %stackaddr$prim49416, align 8
%clofunc49417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40495)
musttail call tailcc void %clofunc49417(%struct.ScmObj* %k40495, %struct.ScmObj* %args47633$k40495$2)
ret void
}

define tailcc void @proc_clo$ae40644(%struct.ScmObj* %env$ae40644,%struct.ScmObj* %current_45args47622) {
%stackaddr$env-ref49418 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40644, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49418
%stackaddr$prim49419 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47622)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim49419, align 8
%stackaddr$prim49420 = alloca %struct.ScmObj*, align 8
%current_45args47623 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47622)
store volatile %struct.ScmObj* %current_45args47623, %struct.ScmObj** %stackaddr$prim49420, align 8
%stackaddr$prim49421 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47623)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim49421, align 8
%stackaddr$prim49422 = alloca %struct.ScmObj*, align 8
%current_45args47624 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47623)
store volatile %struct.ScmObj* %current_45args47624, %struct.ScmObj** %stackaddr$prim49422, align 8
%stackaddr$prim49423 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47624)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim49423, align 8
%stackaddr$prim49424 = alloca %struct.ScmObj*, align 8
%current_45args47625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47624)
store volatile %struct.ScmObj* %current_45args47625, %struct.ScmObj** %stackaddr$prim49424, align 8
%stackaddr$prim49425 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47625)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim49425, align 8
%stackaddr$prim49426 = alloca %struct.ScmObj*, align 8
%anf_45bind40227 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40227, %struct.ScmObj** %stackaddr$prim49426, align 8
%truthy$cmp49427 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40227)
%cmp$cmp49427 = icmp eq i64 %truthy$cmp49427, 1
br i1 %cmp$cmp49427, label %truebranch$cmp49427, label %falsebranch$cmp49427
truebranch$cmp49427:
%ae40648 = call %struct.ScmObj* @const_init_int(i64 0)
%args47627$k40496$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49428 = alloca %struct.ScmObj*, align 8
%args47627$k40496$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47627$k40496$0)
store volatile %struct.ScmObj* %args47627$k40496$1, %struct.ScmObj** %stackaddr$prim49428, align 8
%stackaddr$prim49429 = alloca %struct.ScmObj*, align 8
%args47627$k40496$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40648, %struct.ScmObj* %args47627$k40496$1)
store volatile %struct.ScmObj* %args47627$k40496$2, %struct.ScmObj** %stackaddr$prim49429, align 8
%clofunc49430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40496)
musttail call tailcc void %clofunc49430(%struct.ScmObj* %k40496, %struct.ScmObj* %args47627$k40496$2)
ret void
falsebranch$cmp49427:
%stackaddr$prim49431 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$prim49431, align 8
%stackaddr$prim49432 = alloca %struct.ScmObj*, align 8
%anf_45bind40229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40229, %struct.ScmObj** %stackaddr$prim49432, align 8
%stackaddr$makeclosure49433 = alloca %struct.ScmObj*, align 8
%fptrToInt49434 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40656 to i64
%ae40656 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49434)
store volatile %struct.ScmObj* %ae40656, %struct.ScmObj** %stackaddr$makeclosure49433, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40656, %struct.ScmObj* %anf_45bind40228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40656, %struct.ScmObj* %k40496, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40656, %struct.ScmObj* %f40127, i64 2)
%args47632$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49435 = alloca %struct.ScmObj*, align 8
%args47632$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40229, %struct.ScmObj* %args47632$_37foldr140124$0)
store volatile %struct.ScmObj* %args47632$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49435, align 8
%stackaddr$prim49436 = alloca %struct.ScmObj*, align 8
%args47632$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47632$_37foldr140124$1)
store volatile %struct.ScmObj* %args47632$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49436, align 8
%stackaddr$prim49437 = alloca %struct.ScmObj*, align 8
%args47632$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args47632$_37foldr140124$2)
store volatile %struct.ScmObj* %args47632$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49437, align 8
%stackaddr$prim49438 = alloca %struct.ScmObj*, align 8
%args47632$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40656, %struct.ScmObj* %args47632$_37foldr140124$3)
store volatile %struct.ScmObj* %args47632$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49438, align 8
%clofunc49439 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49439(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47632$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40656(%struct.ScmObj* %env$ae40656,%struct.ScmObj* %current_45args47628) {
%stackaddr$env-ref49440 = alloca %struct.ScmObj*, align 8
%anf_45bind40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40656, i64 0)
store %struct.ScmObj* %anf_45bind40228, %struct.ScmObj** %stackaddr$env-ref49440
%stackaddr$env-ref49441 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40656, i64 1)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref49441
%stackaddr$env-ref49442 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40656, i64 2)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref49442
%stackaddr$prim49443 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47628)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim49443, align 8
%stackaddr$prim49444 = alloca %struct.ScmObj*, align 8
%current_45args47629 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47628)
store volatile %struct.ScmObj* %current_45args47629, %struct.ScmObj** %stackaddr$prim49444, align 8
%stackaddr$prim49445 = alloca %struct.ScmObj*, align 8
%anf_45bind40230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47629)
store volatile %struct.ScmObj* %anf_45bind40230, %struct.ScmObj** %stackaddr$prim49445, align 8
%args47631$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49446 = alloca %struct.ScmObj*, align 8
%args47631$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40230, %struct.ScmObj* %args47631$f40127$0)
store volatile %struct.ScmObj* %args47631$f40127$1, %struct.ScmObj** %stackaddr$prim49446, align 8
%stackaddr$prim49447 = alloca %struct.ScmObj*, align 8
%args47631$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40228, %struct.ScmObj* %args47631$f40127$1)
store volatile %struct.ScmObj* %args47631$f40127$2, %struct.ScmObj** %stackaddr$prim49447, align 8
%stackaddr$prim49448 = alloca %struct.ScmObj*, align 8
%args47631$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40496, %struct.ScmObj* %args47631$f40127$2)
store volatile %struct.ScmObj* %args47631$f40127$3, %struct.ScmObj** %stackaddr$prim49448, align 8
%clofunc49449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc49449(%struct.ScmObj* %f40127, %struct.ScmObj* %args47631$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40524(%struct.ScmObj* %env$ae40524,%struct.ScmObj* %current_45args47636) {
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47636)
store volatile %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$prim49450, align 8
%stackaddr$prim49451 = alloca %struct.ScmObj*, align 8
%current_45args47637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47636)
store volatile %struct.ScmObj* %current_45args47637, %struct.ScmObj** %stackaddr$prim49451, align 8
%stackaddr$prim49452 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47637)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim49452, align 8
%ae40526 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49453 = alloca %struct.ScmObj*, align 8
%fptrToInt49454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40527 to i64
%ae40527 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49454)
store volatile %struct.ScmObj* %ae40527, %struct.ScmObj** %stackaddr$makeclosure49453, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40527, %struct.ScmObj* %y40104, i64 0)
%args47655$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49455 = alloca %struct.ScmObj*, align 8
%args47655$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40527, %struct.ScmObj* %args47655$k40498$0)
store volatile %struct.ScmObj* %args47655$k40498$1, %struct.ScmObj** %stackaddr$prim49455, align 8
%stackaddr$prim49456 = alloca %struct.ScmObj*, align 8
%args47655$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40526, %struct.ScmObj* %args47655$k40498$1)
store volatile %struct.ScmObj* %args47655$k40498$2, %struct.ScmObj** %stackaddr$prim49456, align 8
%clofunc49457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc49457(%struct.ScmObj* %k40498, %struct.ScmObj* %args47655$k40498$2)
ret void
}

define tailcc void @proc_clo$ae40527(%struct.ScmObj* %env$ae40527,%struct.ScmObj* %current_45args47639) {
%stackaddr$env-ref49458 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40527, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref49458
%stackaddr$prim49459 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47639)
store volatile %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$prim49459, align 8
%stackaddr$prim49460 = alloca %struct.ScmObj*, align 8
%current_45args47640 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47639)
store volatile %struct.ScmObj* %current_45args47640, %struct.ScmObj** %stackaddr$prim49460, align 8
%stackaddr$prim49461 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47640)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim49461, align 8
%stackaddr$makeclosure49462 = alloca %struct.ScmObj*, align 8
%fptrToInt49463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40528 to i64
%ae40528 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49463)
store volatile %struct.ScmObj* %ae40528, %struct.ScmObj** %stackaddr$makeclosure49462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40528, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40528, %struct.ScmObj* %k40499, i64 1)
%ae40529 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49464 = alloca %struct.ScmObj*, align 8
%fptrToInt49465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40530 to i64
%ae40530 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49465)
store volatile %struct.ScmObj* %ae40530, %struct.ScmObj** %stackaddr$makeclosure49464, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40530, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40530, %struct.ScmObj* %y40104, i64 1)
%args47654$ae40528$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49466 = alloca %struct.ScmObj*, align 8
%args47654$ae40528$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40530, %struct.ScmObj* %args47654$ae40528$0)
store volatile %struct.ScmObj* %args47654$ae40528$1, %struct.ScmObj** %stackaddr$prim49466, align 8
%stackaddr$prim49467 = alloca %struct.ScmObj*, align 8
%args47654$ae40528$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40529, %struct.ScmObj* %args47654$ae40528$1)
store volatile %struct.ScmObj* %args47654$ae40528$2, %struct.ScmObj** %stackaddr$prim49467, align 8
%clofunc49468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40528)
musttail call tailcc void %clofunc49468(%struct.ScmObj* %ae40528, %struct.ScmObj* %args47654$ae40528$2)
ret void
}

define tailcc void @proc_clo$ae40528(%struct.ScmObj* %env$ae40528,%struct.ScmObj* %current_45args47642) {
%stackaddr$env-ref49469 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40528, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49469
%stackaddr$env-ref49470 = alloca %struct.ScmObj*, align 8
%k40499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40528, i64 1)
store %struct.ScmObj* %k40499, %struct.ScmObj** %stackaddr$env-ref49470
%stackaddr$prim49471 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47642)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim49471, align 8
%stackaddr$prim49472 = alloca %struct.ScmObj*, align 8
%current_45args47643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47642)
store volatile %struct.ScmObj* %current_45args47643, %struct.ScmObj** %stackaddr$prim49472, align 8
%stackaddr$prim49473 = alloca %struct.ScmObj*, align 8
%anf_45bind40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47643)
store volatile %struct.ScmObj* %anf_45bind40225, %struct.ScmObj** %stackaddr$prim49473, align 8
%args47645$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49474 = alloca %struct.ScmObj*, align 8
%args47645$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40225, %struct.ScmObj* %args47645$f40105$0)
store volatile %struct.ScmObj* %args47645$f40105$1, %struct.ScmObj** %stackaddr$prim49474, align 8
%stackaddr$prim49475 = alloca %struct.ScmObj*, align 8
%args47645$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40499, %struct.ScmObj* %args47645$f40105$1)
store volatile %struct.ScmObj* %args47645$f40105$2, %struct.ScmObj** %stackaddr$prim49475, align 8
%clofunc49476 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc49476(%struct.ScmObj* %f40105, %struct.ScmObj* %args47645$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40530(%struct.ScmObj* %env$ae40530,%struct.ScmObj* %args4010640501) {
%stackaddr$env-ref49477 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40530, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49477
%stackaddr$env-ref49478 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40530, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref49478
%stackaddr$prim49479 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640501)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim49479, align 8
%stackaddr$prim49480 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640501)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim49480, align 8
%stackaddr$makeclosure49481 = alloca %struct.ScmObj*, align 8
%fptrToInt49482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40534 to i64
%ae40534 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49482)
store volatile %struct.ScmObj* %ae40534, %struct.ScmObj** %stackaddr$makeclosure49481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40534, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40534, %struct.ScmObj* %k40502, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40534, %struct.ScmObj* %args40106, i64 2)
%args47653$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49483 = alloca %struct.ScmObj*, align 8
%args47653$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args47653$y40104$0)
store volatile %struct.ScmObj* %args47653$y40104$1, %struct.ScmObj** %stackaddr$prim49483, align 8
%stackaddr$prim49484 = alloca %struct.ScmObj*, align 8
%args47653$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40534, %struct.ScmObj* %args47653$y40104$1)
store volatile %struct.ScmObj* %args47653$y40104$2, %struct.ScmObj** %stackaddr$prim49484, align 8
%clofunc49485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc49485(%struct.ScmObj* %y40104, %struct.ScmObj* %args47653$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40534(%struct.ScmObj* %env$ae40534,%struct.ScmObj* %current_45args47646) {
%stackaddr$env-ref49486 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40534, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49486
%stackaddr$env-ref49487 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40534, i64 1)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref49487
%stackaddr$env-ref49488 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40534, i64 2)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref49488
%stackaddr$prim49489 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47646)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim49489, align 8
%stackaddr$prim49490 = alloca %struct.ScmObj*, align 8
%current_45args47647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47646)
store volatile %struct.ScmObj* %current_45args47647, %struct.ScmObj** %stackaddr$prim49490, align 8
%stackaddr$prim49491 = alloca %struct.ScmObj*, align 8
%anf_45bind40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47647)
store volatile %struct.ScmObj* %anf_45bind40223, %struct.ScmObj** %stackaddr$prim49491, align 8
%stackaddr$makeclosure49492 = alloca %struct.ScmObj*, align 8
%fptrToInt49493 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40537 to i64
%ae40537 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49493)
store volatile %struct.ScmObj* %ae40537, %struct.ScmObj** %stackaddr$makeclosure49492, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40537, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40537, %struct.ScmObj* %args40106, i64 1)
%args47652$anf_45bind40223$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49494 = alloca %struct.ScmObj*, align 8
%args47652$anf_45bind40223$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args47652$anf_45bind40223$0)
store volatile %struct.ScmObj* %args47652$anf_45bind40223$1, %struct.ScmObj** %stackaddr$prim49494, align 8
%stackaddr$prim49495 = alloca %struct.ScmObj*, align 8
%args47652$anf_45bind40223$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40537, %struct.ScmObj* %args47652$anf_45bind40223$1)
store volatile %struct.ScmObj* %args47652$anf_45bind40223$2, %struct.ScmObj** %stackaddr$prim49495, align 8
%clofunc49496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40223)
musttail call tailcc void %clofunc49496(%struct.ScmObj* %anf_45bind40223, %struct.ScmObj* %args47652$anf_45bind40223$2)
ret void
}

define tailcc void @proc_clo$ae40537(%struct.ScmObj* %env$ae40537,%struct.ScmObj* %current_45args47649) {
%stackaddr$env-ref49497 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40537, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref49497
%stackaddr$env-ref49498 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40537, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref49498
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47649)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$prim49500 = alloca %struct.ScmObj*, align 8
%current_45args47650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47649)
store volatile %struct.ScmObj* %current_45args47650, %struct.ScmObj** %stackaddr$prim49500, align 8
%stackaddr$prim49501 = alloca %struct.ScmObj*, align 8
%anf_45bind40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47650)
store volatile %struct.ScmObj* %anf_45bind40224, %struct.ScmObj** %stackaddr$prim49501, align 8
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%cpsargs40505 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40502, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40505, %struct.ScmObj** %stackaddr$prim49502, align 8
%clofunc49503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40224)
musttail call tailcc void %clofunc49503(%struct.ScmObj* %anf_45bind40224, %struct.ScmObj* %cpsargs40505)
ret void
}

define tailcc void @proc_clo$ae40509(%struct.ScmObj* %env$ae40509,%struct.ScmObj* %current_45args47657) {
%stackaddr$prim49504 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47657)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim49504, align 8
%stackaddr$prim49505 = alloca %struct.ScmObj*, align 8
%current_45args47658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47657)
store volatile %struct.ScmObj* %current_45args47658, %struct.ScmObj** %stackaddr$prim49505, align 8
%stackaddr$prim49506 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47658)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim49506, align 8
%args47660$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49507 = alloca %struct.ScmObj*, align 8
%args47660$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47660$yu40103$0)
store volatile %struct.ScmObj* %args47660$yu40103$1, %struct.ScmObj** %stackaddr$prim49507, align 8
%stackaddr$prim49508 = alloca %struct.ScmObj*, align 8
%args47660$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40506, %struct.ScmObj* %args47660$yu40103$1)
store volatile %struct.ScmObj* %args47660$yu40103$2, %struct.ScmObj** %stackaddr$prim49508, align 8
%clofunc49509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc49509(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47660$yu40103$2)
ret void
}