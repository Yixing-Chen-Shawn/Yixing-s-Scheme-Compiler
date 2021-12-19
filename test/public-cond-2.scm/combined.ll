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
%mainenv55371 = call %struct.ScmObj* @const_init_null()
%mainargs55372 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv55371, %struct.ScmObj* %mainargs55372)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv55369,%struct.ScmObj* %mainargs55370) {
%stackaddr$makeclosure55373 = alloca %struct.ScmObj*, align 8
%fptrToInt55374 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48421 to i64
%ae48421 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55374)
store volatile %struct.ScmObj* %ae48421, %struct.ScmObj** %stackaddr$makeclosure55373, align 8
%ae48422 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55375 = alloca %struct.ScmObj*, align 8
%fptrToInt55376 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48423 to i64
%ae48423 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55376)
store volatile %struct.ScmObj* %ae48423, %struct.ScmObj** %stackaddr$makeclosure55375, align 8
%argslist55368$ae484210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist55368$ae484211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48423, %struct.ScmObj* %argslist55368$ae484210)
store volatile %struct.ScmObj* %argslist55368$ae484211, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist55368$ae484212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48422, %struct.ScmObj* %argslist55368$ae484211)
store volatile %struct.ScmObj* %argslist55368$ae484212, %struct.ScmObj** %stackaddr$prim55378, align 8
%clofunc55379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48421)
musttail call tailcc void %clofunc55379(%struct.ScmObj* %ae48421, %struct.ScmObj* %argslist55368$ae484212)
ret void
}

define tailcc void @proc_clo$ae48421(%struct.ScmObj* %env$ae48421,%struct.ScmObj* %current_45args54754) {
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%_95k48256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54754)
store volatile %struct.ScmObj* %_95k48256, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%current_45args54755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54754)
store volatile %struct.ScmObj* %current_45args54755, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%anf_45bind48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54755)
store volatile %struct.ScmObj* %anf_45bind48143, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$makeclosure55383 = alloca %struct.ScmObj*, align 8
%fptrToInt55384 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48436 to i64
%ae48436 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55384)
store volatile %struct.ScmObj* %ae48436, %struct.ScmObj** %stackaddr$makeclosure55383, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48436, %struct.ScmObj* %anf_45bind48143, i64 0)
%ae48437 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55385 = alloca %struct.ScmObj*, align 8
%fptrToInt55386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48438 to i64
%ae48438 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55386)
store volatile %struct.ScmObj* %ae48438, %struct.ScmObj** %stackaddr$makeclosure55385, align 8
%argslist55363$ae484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%argslist55363$ae484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48438, %struct.ScmObj* %argslist55363$ae484360)
store volatile %struct.ScmObj* %argslist55363$ae484361, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%argslist55363$ae484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48437, %struct.ScmObj* %argslist55363$ae484361)
store volatile %struct.ScmObj* %argslist55363$ae484362, %struct.ScmObj** %stackaddr$prim55388, align 8
%clofunc55389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48436)
musttail call tailcc void %clofunc55389(%struct.ScmObj* %ae48436, %struct.ScmObj* %argslist55363$ae484362)
ret void
}

define tailcc void @proc_clo$ae48436(%struct.ScmObj* %env$ae48436,%struct.ScmObj* %current_45args54757) {
%stackaddr$env-ref55390 = alloca %struct.ScmObj*, align 8
%anf_45bind48143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48436, i64 0)
store %struct.ScmObj* %anf_45bind48143, %struct.ScmObj** %stackaddr$env-ref55390
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%_95k48257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %_95k48257, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%current_45args54758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %current_45args54758, %struct.ScmObj** %stackaddr$prim55392, align 8
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%anf_45bind48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54758)
store volatile %struct.ScmObj* %anf_45bind48147, %struct.ScmObj** %stackaddr$prim55393, align 8
%stackaddr$makeclosure55394 = alloca %struct.ScmObj*, align 8
%fptrToInt55395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48551 to i64
%ae48551 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55395)
store volatile %struct.ScmObj* %ae48551, %struct.ScmObj** %stackaddr$makeclosure55394, align 8
%argslist55342$anf_45bind481430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%argslist55342$anf_45bind481431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48147, %struct.ScmObj* %argslist55342$anf_45bind481430)
store volatile %struct.ScmObj* %argslist55342$anf_45bind481431, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%argslist55342$anf_45bind481432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48551, %struct.ScmObj* %argslist55342$anf_45bind481431)
store volatile %struct.ScmObj* %argslist55342$anf_45bind481432, %struct.ScmObj** %stackaddr$prim55397, align 8
%clofunc55398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48143)
musttail call tailcc void %clofunc55398(%struct.ScmObj* %anf_45bind48143, %struct.ScmObj* %argslist55342$anf_45bind481432)
ret void
}

define tailcc void @proc_clo$ae48551(%struct.ScmObj* %env$ae48551,%struct.ScmObj* %current_45args54760) {
%stackaddr$prim55399 = alloca %struct.ScmObj*, align 8
%_95k48258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54760)
store volatile %struct.ScmObj* %_95k48258, %struct.ScmObj** %stackaddr$prim55399, align 8
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%current_45args54761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54760)
store volatile %struct.ScmObj* %current_45args54761, %struct.ScmObj** %stackaddr$prim55400, align 8
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54761)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$makeclosure55402 = alloca %struct.ScmObj*, align 8
%fptrToInt55403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48553 to i64
%ae48553 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55403)
store volatile %struct.ScmObj* %ae48553, %struct.ScmObj** %stackaddr$makeclosure55402, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48554 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55404 = alloca %struct.ScmObj*, align 8
%fptrToInt55405 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48555 to i64
%ae48555 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55405)
store volatile %struct.ScmObj* %ae48555, %struct.ScmObj** %stackaddr$makeclosure55404, align 8
%argslist55341$ae485530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%argslist55341$ae485531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48555, %struct.ScmObj* %argslist55341$ae485530)
store volatile %struct.ScmObj* %argslist55341$ae485531, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%argslist55341$ae485532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48554, %struct.ScmObj* %argslist55341$ae485531)
store volatile %struct.ScmObj* %argslist55341$ae485532, %struct.ScmObj** %stackaddr$prim55407, align 8
%clofunc55408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48553)
musttail call tailcc void %clofunc55408(%struct.ScmObj* %ae48553, %struct.ScmObj* %argslist55341$ae485532)
ret void
}

define tailcc void @proc_clo$ae48553(%struct.ScmObj* %env$ae48553,%struct.ScmObj* %current_45args54763) {
%stackaddr$env-ref55409 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55409
%stackaddr$prim55410 = alloca %struct.ScmObj*, align 8
%_95k48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54763)
store volatile %struct.ScmObj* %_95k48259, %struct.ScmObj** %stackaddr$prim55410, align 8
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%current_45args54764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54763)
store volatile %struct.ScmObj* %current_45args54764, %struct.ScmObj** %stackaddr$prim55411, align 8
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54764)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$makeclosure55413 = alloca %struct.ScmObj*, align 8
%fptrToInt55414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48631 to i64
%ae48631 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55414)
store volatile %struct.ScmObj* %ae48631, %struct.ScmObj** %stackaddr$makeclosure55413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48631, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist55325$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%argslist55325$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist55325$Ycmb480250)
store volatile %struct.ScmObj* %argslist55325$Ycmb480251, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%argslist55325$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48631, %struct.ScmObj* %argslist55325$Ycmb480251)
store volatile %struct.ScmObj* %argslist55325$Ycmb480252, %struct.ScmObj** %stackaddr$prim55416, align 8
%clofunc55417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55417(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55325$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48631(%struct.ScmObj* %env$ae48631,%struct.ScmObj* %current_45args54766) {
%stackaddr$env-ref55418 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48631, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55418
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%_95k48260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %_95k48260, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%current_45args54767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %current_45args54767, %struct.ScmObj** %stackaddr$prim55420, align 8
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54767)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim55421, align 8
%stackaddr$makeclosure55422 = alloca %struct.ScmObj*, align 8
%fptrToInt55423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48633 to i64
%ae48633 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55423)
store volatile %struct.ScmObj* %ae48633, %struct.ScmObj** %stackaddr$makeclosure55422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48633, %struct.ScmObj* %_37foldr148046, i64 1)
%ae48634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55424 = alloca %struct.ScmObj*, align 8
%fptrToInt55425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55425)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure55424, align 8
%argslist55324$ae486330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist55324$ae486331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist55324$ae486330)
store volatile %struct.ScmObj* %argslist55324$ae486331, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%argslist55324$ae486332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist55324$ae486331)
store volatile %struct.ScmObj* %argslist55324$ae486332, %struct.ScmObj** %stackaddr$prim55427, align 8
%clofunc55428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48633)
musttail call tailcc void %clofunc55428(%struct.ScmObj* %ae48633, %struct.ScmObj* %argslist55324$ae486332)
ret void
}

define tailcc void @proc_clo$ae48633(%struct.ScmObj* %env$ae48633,%struct.ScmObj* %current_45args54769) {
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48633, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%_95k48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54769)
store volatile %struct.ScmObj* %_95k48261, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%current_45args54770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54769)
store volatile %struct.ScmObj* %current_45args54770, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54770)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$makeclosure55434 = alloca %struct.ScmObj*, align 8
%fptrToInt55435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48728 to i64
%ae48728 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55435)
store volatile %struct.ScmObj* %ae48728, %struct.ScmObj** %stackaddr$makeclosure55434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48728, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48728, %struct.ScmObj* %_37foldr148046, i64 1)
%argslist55305$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%argslist55305$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48158, %struct.ScmObj* %argslist55305$Ycmb480250)
store volatile %struct.ScmObj* %argslist55305$Ycmb480251, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%argslist55305$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48728, %struct.ScmObj* %argslist55305$Ycmb480251)
store volatile %struct.ScmObj* %argslist55305$Ycmb480252, %struct.ScmObj** %stackaddr$prim55437, align 8
%clofunc55438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55438(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55305$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48728(%struct.ScmObj* %env$ae48728,%struct.ScmObj* %current_45args54772) {
%stackaddr$env-ref55439 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48728, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55439
%stackaddr$env-ref55440 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48728, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55440
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%_95k48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54772)
store volatile %struct.ScmObj* %_95k48262, %struct.ScmObj** %stackaddr$prim55441, align 8
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%current_45args54773 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54772)
store volatile %struct.ScmObj* %current_45args54773, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54773)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$makeclosure55444 = alloca %struct.ScmObj*, align 8
%fptrToInt55445 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48730 to i64
%ae48730 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55445)
store volatile %struct.ScmObj* %ae48730, %struct.ScmObj** %stackaddr$makeclosure55444, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48730, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48730, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48730, %struct.ScmObj* %_37map148042, i64 2)
%ae48731 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55446 = alloca %struct.ScmObj*, align 8
%fptrToInt55447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48732 to i64
%ae48732 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55447)
store volatile %struct.ScmObj* %ae48732, %struct.ScmObj** %stackaddr$makeclosure55446, align 8
%argslist55304$ae487300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%argslist55304$ae487301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48732, %struct.ScmObj* %argslist55304$ae487300)
store volatile %struct.ScmObj* %argslist55304$ae487301, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%argslist55304$ae487302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48731, %struct.ScmObj* %argslist55304$ae487301)
store volatile %struct.ScmObj* %argslist55304$ae487302, %struct.ScmObj** %stackaddr$prim55449, align 8
%clofunc55450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48730)
musttail call tailcc void %clofunc55450(%struct.ScmObj* %ae48730, %struct.ScmObj* %argslist55304$ae487302)
ret void
}

define tailcc void @proc_clo$ae48730(%struct.ScmObj* %env$ae48730,%struct.ScmObj* %current_45args54775) {
%stackaddr$env-ref55451 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48730, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55451
%stackaddr$env-ref55452 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48730, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55452
%stackaddr$env-ref55453 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48730, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55453
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%_95k48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54775)
store volatile %struct.ScmObj* %_95k48263, %struct.ScmObj** %stackaddr$prim55454, align 8
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%current_45args54776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54775)
store volatile %struct.ScmObj* %current_45args54776, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54776)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim55456, align 8
%stackaddr$makeclosure55457 = alloca %struct.ScmObj*, align 8
%fptrToInt55458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48878 to i64
%ae48878 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55458)
store volatile %struct.ScmObj* %ae48878, %struct.ScmObj** %stackaddr$makeclosure55457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37map148042, i64 2)
%argslist55288$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55459 = alloca %struct.ScmObj*, align 8
%argslist55288$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %argslist55288$Ycmb480250)
store volatile %struct.ScmObj* %argslist55288$Ycmb480251, %struct.ScmObj** %stackaddr$prim55459, align 8
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%argslist55288$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48878, %struct.ScmObj* %argslist55288$Ycmb480251)
store volatile %struct.ScmObj* %argslist55288$Ycmb480252, %struct.ScmObj** %stackaddr$prim55460, align 8
%clofunc55461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55461(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55288$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48878(%struct.ScmObj* %env$ae48878,%struct.ScmObj* %current_45args54778) {
%stackaddr$env-ref55462 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55462
%stackaddr$env-ref55463 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55463
%stackaddr$env-ref55464 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55464
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%_95k48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54778)
store volatile %struct.ScmObj* %_95k48264, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%current_45args54779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54778)
store volatile %struct.ScmObj* %current_45args54779, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54779)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$makeclosure55468 = alloca %struct.ScmObj*, align 8
%fptrToInt55469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48880 to i64
%ae48880 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55469)
store volatile %struct.ScmObj* %ae48880, %struct.ScmObj** %stackaddr$makeclosure55468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48880, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48880, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48880, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48880, %struct.ScmObj* %_37map148042, i64 3)
%ae48881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55470 = alloca %struct.ScmObj*, align 8
%fptrToInt55471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48882 to i64
%ae48882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55471)
store volatile %struct.ScmObj* %ae48882, %struct.ScmObj** %stackaddr$makeclosure55470, align 8
%argslist55287$ae488800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist55287$ae488801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48882, %struct.ScmObj* %argslist55287$ae488800)
store volatile %struct.ScmObj* %argslist55287$ae488801, %struct.ScmObj** %stackaddr$prim55472, align 8
%stackaddr$prim55473 = alloca %struct.ScmObj*, align 8
%argslist55287$ae488802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48881, %struct.ScmObj* %argslist55287$ae488801)
store volatile %struct.ScmObj* %argslist55287$ae488802, %struct.ScmObj** %stackaddr$prim55473, align 8
%clofunc55474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48880)
musttail call tailcc void %clofunc55474(%struct.ScmObj* %ae48880, %struct.ScmObj* %argslist55287$ae488802)
ret void
}

define tailcc void @proc_clo$ae48880(%struct.ScmObj* %env$ae48880,%struct.ScmObj* %current_45args54781) {
%stackaddr$env-ref55475 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48880, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55475
%stackaddr$env-ref55476 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48880, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55476
%stackaddr$env-ref55477 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48880, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55477
%stackaddr$env-ref55478 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48880, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55478
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%_95k48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54781)
store volatile %struct.ScmObj* %_95k48265, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%current_45args54782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54781)
store volatile %struct.ScmObj* %current_45args54782, %struct.ScmObj** %stackaddr$prim55480, align 8
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54782)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$makeclosure55482 = alloca %struct.ScmObj*, align 8
%fptrToInt55483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48961 to i64
%ae48961 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55483)
store volatile %struct.ScmObj* %ae48961, %struct.ScmObj** %stackaddr$makeclosure55482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48961, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48961, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48961, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48961, %struct.ScmObj* %_37map148042, i64 3)
%argslist55273$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%argslist55273$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %argslist55273$Ycmb480250)
store volatile %struct.ScmObj* %argslist55273$Ycmb480251, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%argslist55273$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48961, %struct.ScmObj* %argslist55273$Ycmb480251)
store volatile %struct.ScmObj* %argslist55273$Ycmb480252, %struct.ScmObj** %stackaddr$prim55485, align 8
%clofunc55486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55486(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55273$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48961(%struct.ScmObj* %env$ae48961,%struct.ScmObj* %current_45args54784) {
%stackaddr$env-ref55487 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48961, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55487
%stackaddr$env-ref55488 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48961, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55488
%stackaddr$env-ref55489 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48961, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55489
%stackaddr$env-ref55490 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48961, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55490
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%_95k48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %_95k48266, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%current_45args54785 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %current_45args54785, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54785)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$makeclosure55494 = alloca %struct.ScmObj*, align 8
%fptrToInt55495 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48963 to i64
%ae48963 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55495)
store volatile %struct.ScmObj* %ae48963, %struct.ScmObj** %stackaddr$makeclosure55494, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %_37map148042, i64 4)
%ae48964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55496 = alloca %struct.ScmObj*, align 8
%fptrToInt55497 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48965 to i64
%ae48965 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55497)
store volatile %struct.ScmObj* %ae48965, %struct.ScmObj** %stackaddr$makeclosure55496, align 8
%argslist55272$ae489630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%argslist55272$ae489631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48965, %struct.ScmObj* %argslist55272$ae489630)
store volatile %struct.ScmObj* %argslist55272$ae489631, %struct.ScmObj** %stackaddr$prim55498, align 8
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%argslist55272$ae489632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48964, %struct.ScmObj* %argslist55272$ae489631)
store volatile %struct.ScmObj* %argslist55272$ae489632, %struct.ScmObj** %stackaddr$prim55499, align 8
%clofunc55500 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48963)
musttail call tailcc void %clofunc55500(%struct.ScmObj* %ae48963, %struct.ScmObj* %argslist55272$ae489632)
ret void
}

define tailcc void @proc_clo$ae48963(%struct.ScmObj* %env$ae48963,%struct.ScmObj* %current_45args54787) {
%stackaddr$env-ref55501 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55501
%stackaddr$env-ref55502 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55502
%stackaddr$env-ref55503 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55503
%stackaddr$env-ref55504 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55504
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$prim55506 = alloca %struct.ScmObj*, align 8
%_95k48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54787)
store volatile %struct.ScmObj* %_95k48267, %struct.ScmObj** %stackaddr$prim55506, align 8
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%current_45args54788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54787)
store volatile %struct.ScmObj* %current_45args54788, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54788)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim55508, align 8
%stackaddr$makeclosure55509 = alloca %struct.ScmObj*, align 8
%fptrToInt55510 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49040 to i64
%ae49040 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55510)
store volatile %struct.ScmObj* %ae49040, %struct.ScmObj** %stackaddr$makeclosure55509, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49040, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49040, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49040, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49040, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49040, %struct.ScmObj* %_37map148042, i64 4)
%argslist55256$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%argslist55256$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist55256$Ycmb480250)
store volatile %struct.ScmObj* %argslist55256$Ycmb480251, %struct.ScmObj** %stackaddr$prim55511, align 8
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist55256$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49040, %struct.ScmObj* %argslist55256$Ycmb480251)
store volatile %struct.ScmObj* %argslist55256$Ycmb480252, %struct.ScmObj** %stackaddr$prim55512, align 8
%clofunc55513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55513(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55256$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49040(%struct.ScmObj* %env$ae49040,%struct.ScmObj* %current_45args54790) {
%stackaddr$env-ref55514 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49040, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55514
%stackaddr$env-ref55515 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49040, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55515
%stackaddr$env-ref55516 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49040, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55516
%stackaddr$env-ref55517 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49040, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55517
%stackaddr$env-ref55518 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49040, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55518
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%_95k48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54790)
store volatile %struct.ScmObj* %_95k48268, %struct.ScmObj** %stackaddr$prim55519, align 8
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%current_45args54791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54790)
store volatile %struct.ScmObj* %current_45args54791, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54791)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim55521, align 8
%stackaddr$makeclosure55522 = alloca %struct.ScmObj*, align 8
%fptrToInt55523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49042 to i64
%ae49042 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55523)
store volatile %struct.ScmObj* %ae49042, %struct.ScmObj** %stackaddr$makeclosure55522, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %_37take48038, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %_37length48035, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49042, %struct.ScmObj* %_37map148042, i64 5)
%ae49043 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55524 = alloca %struct.ScmObj*, align 8
%fptrToInt55525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49044 to i64
%ae49044 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55525)
store volatile %struct.ScmObj* %ae49044, %struct.ScmObj** %stackaddr$makeclosure55524, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist55255$ae490420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55526 = alloca %struct.ScmObj*, align 8
%argslist55255$ae490421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49044, %struct.ScmObj* %argslist55255$ae490420)
store volatile %struct.ScmObj* %argslist55255$ae490421, %struct.ScmObj** %stackaddr$prim55526, align 8
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist55255$ae490422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49043, %struct.ScmObj* %argslist55255$ae490421)
store volatile %struct.ScmObj* %argslist55255$ae490422, %struct.ScmObj** %stackaddr$prim55527, align 8
%clofunc55528 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49042)
musttail call tailcc void %clofunc55528(%struct.ScmObj* %ae49042, %struct.ScmObj* %argslist55255$ae490422)
ret void
}

define tailcc void @proc_clo$ae49042(%struct.ScmObj* %env$ae49042,%struct.ScmObj* %current_45args54793) {
%stackaddr$env-ref55529 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55529
%stackaddr$env-ref55530 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55530
%stackaddr$env-ref55531 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55531
%stackaddr$env-ref55532 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 3)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55532
%stackaddr$env-ref55533 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 4)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55533
%stackaddr$env-ref55534 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49042, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55534
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%_95k48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54793)
store volatile %struct.ScmObj* %_95k48269, %struct.ScmObj** %stackaddr$prim55535, align 8
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%current_45args54794 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54793)
store volatile %struct.ScmObj* %current_45args54794, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54794)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$makeclosure55538 = alloca %struct.ScmObj*, align 8
%fptrToInt55539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49096 to i64
%ae49096 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55539)
store volatile %struct.ScmObj* %ae49096, %struct.ScmObj** %stackaddr$makeclosure55538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49096, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49096, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49096, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49096, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49096, %struct.ScmObj* %_37map148042, i64 4)
%ae49097 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55540 = alloca %struct.ScmObj*, align 8
%fptrToInt55541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49098 to i64
%ae49098 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55541)
store volatile %struct.ScmObj* %ae49098, %struct.ScmObj** %stackaddr$makeclosure55540, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49098, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49098, %struct.ScmObj* %_37length48035, i64 1)
%argslist55241$ae490960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55542 = alloca %struct.ScmObj*, align 8
%argslist55241$ae490961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49098, %struct.ScmObj* %argslist55241$ae490960)
store volatile %struct.ScmObj* %argslist55241$ae490961, %struct.ScmObj** %stackaddr$prim55542, align 8
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%argslist55241$ae490962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49097, %struct.ScmObj* %argslist55241$ae490961)
store volatile %struct.ScmObj* %argslist55241$ae490962, %struct.ScmObj** %stackaddr$prim55543, align 8
%clofunc55544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49096)
musttail call tailcc void %clofunc55544(%struct.ScmObj* %ae49096, %struct.ScmObj* %argslist55241$ae490962)
ret void
}

define tailcc void @proc_clo$ae49096(%struct.ScmObj* %env$ae49096,%struct.ScmObj* %current_45args54796) {
%stackaddr$env-ref55545 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49096, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55545
%stackaddr$env-ref55546 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49096, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55546
%stackaddr$env-ref55547 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49096, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55547
%stackaddr$env-ref55548 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49096, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55548
%stackaddr$env-ref55549 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49096, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55549
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%_95k48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %_95k48270, %struct.ScmObj** %stackaddr$prim55550, align 8
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%current_45args54797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %current_45args54797, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54797)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$makeclosure55553 = alloca %struct.ScmObj*, align 8
%fptrToInt55554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49126 to i64
%ae49126 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55554)
store volatile %struct.ScmObj* %ae49126, %struct.ScmObj** %stackaddr$makeclosure55553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49126, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49126, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49126, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49126, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49126, %struct.ScmObj* %_37drop_45right48065, i64 4)
%ae49127 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55555 = alloca %struct.ScmObj*, align 8
%fptrToInt55556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49128 to i64
%ae49128 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55556)
store volatile %struct.ScmObj* %ae49128, %struct.ScmObj** %stackaddr$makeclosure55555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49128, %struct.ScmObj* %_37map148042, i64 1)
%argslist55231$ae491260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%argslist55231$ae491261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49128, %struct.ScmObj* %argslist55231$ae491260)
store volatile %struct.ScmObj* %argslist55231$ae491261, %struct.ScmObj** %stackaddr$prim55557, align 8
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%argslist55231$ae491262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49127, %struct.ScmObj* %argslist55231$ae491261)
store volatile %struct.ScmObj* %argslist55231$ae491262, %struct.ScmObj** %stackaddr$prim55558, align 8
%clofunc55559 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49126)
musttail call tailcc void %clofunc55559(%struct.ScmObj* %ae49126, %struct.ScmObj* %argslist55231$ae491262)
ret void
}

define tailcc void @proc_clo$ae49126(%struct.ScmObj* %env$ae49126,%struct.ScmObj* %current_45args54799) {
%stackaddr$env-ref55560 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49126, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55560
%stackaddr$env-ref55561 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49126, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55561
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49126, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$env-ref55563 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49126, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55563
%stackaddr$env-ref55564 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49126, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55564
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54799)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%current_45args54800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54799)
store volatile %struct.ScmObj* %current_45args54800, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54800)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$makeclosure55568 = alloca %struct.ScmObj*, align 8
%fptrToInt55569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49510 to i64
%ae49510 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55569)
store volatile %struct.ScmObj* %ae49510, %struct.ScmObj** %stackaddr$makeclosure55568, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49510, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49510, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49510, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49510, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49510, %struct.ScmObj* %_37drop_45right48065, i64 4)
%argslist55171$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%argslist55171$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist55171$Ycmb480250)
store volatile %struct.ScmObj* %argslist55171$Ycmb480251, %struct.ScmObj** %stackaddr$prim55570, align 8
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%argslist55171$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49510, %struct.ScmObj* %argslist55171$Ycmb480251)
store volatile %struct.ScmObj* %argslist55171$Ycmb480252, %struct.ScmObj** %stackaddr$prim55571, align 8
%clofunc55572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55572(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55171$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49510(%struct.ScmObj* %env$ae49510,%struct.ScmObj* %current_45args54802) {
%stackaddr$env-ref55573 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49510, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55573
%stackaddr$env-ref55574 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49510, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55574
%stackaddr$env-ref55575 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49510, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55575
%stackaddr$env-ref55576 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49510, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55576
%stackaddr$env-ref55577 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49510, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55577
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54802)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim55578, align 8
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%current_45args54803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54802)
store volatile %struct.ScmObj* %current_45args54803, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54803)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim55580, align 8
%stackaddr$makeclosure55581 = alloca %struct.ScmObj*, align 8
%fptrToInt55582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49512 to i64
%ae49512 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55582)
store volatile %struct.ScmObj* %ae49512, %struct.ScmObj** %stackaddr$makeclosure55581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49512, %struct.ScmObj* %_37drop_45right48065, i64 5)
%ae49513 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55583 = alloca %struct.ScmObj*, align 8
%fptrToInt55584 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49514 to i64
%ae49514 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55584)
store volatile %struct.ScmObj* %ae49514, %struct.ScmObj** %stackaddr$makeclosure55583, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist55170$ae495120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%argslist55170$ae495121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49514, %struct.ScmObj* %argslist55170$ae495120)
store volatile %struct.ScmObj* %argslist55170$ae495121, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%argslist55170$ae495122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49513, %struct.ScmObj* %argslist55170$ae495121)
store volatile %struct.ScmObj* %argslist55170$ae495122, %struct.ScmObj** %stackaddr$prim55586, align 8
%clofunc55587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49512)
musttail call tailcc void %clofunc55587(%struct.ScmObj* %ae49512, %struct.ScmObj* %argslist55170$ae495122)
ret void
}

define tailcc void @proc_clo$ae49512(%struct.ScmObj* %env$ae49512,%struct.ScmObj* %current_45args54805) {
%stackaddr$env-ref55588 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55588
%stackaddr$env-ref55589 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55589
%stackaddr$env-ref55590 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55590
%stackaddr$env-ref55591 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55591
%stackaddr$env-ref55592 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55592
%stackaddr$env-ref55593 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49512, i64 5)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55593
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54805)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim55594, align 8
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%current_45args54806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54805)
store volatile %struct.ScmObj* %current_45args54806, %struct.ScmObj** %stackaddr$prim55595, align 8
%stackaddr$prim55596 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54806)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim55596, align 8
%stackaddr$makeclosure55597 = alloca %struct.ScmObj*, align 8
%fptrToInt55598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49589 to i64
%ae49589 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55598)
store volatile %struct.ScmObj* %ae49589, %struct.ScmObj** %stackaddr$makeclosure55597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49589, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49589, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49589, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49589, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49589, %struct.ScmObj* %_37map148077, i64 4)
%ae49590 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55599 = alloca %struct.ScmObj*, align 8
%fptrToInt55600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49591 to i64
%ae49591 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55600)
store volatile %struct.ScmObj* %ae49591, %struct.ScmObj** %stackaddr$makeclosure55599, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49591, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49591, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49591, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist55151$ae495890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%argslist55151$ae495891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49591, %struct.ScmObj* %argslist55151$ae495890)
store volatile %struct.ScmObj* %argslist55151$ae495891, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%argslist55151$ae495892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49590, %struct.ScmObj* %argslist55151$ae495891)
store volatile %struct.ScmObj* %argslist55151$ae495892, %struct.ScmObj** %stackaddr$prim55602, align 8
%clofunc55603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49589)
musttail call tailcc void %clofunc55603(%struct.ScmObj* %ae49589, %struct.ScmObj* %argslist55151$ae495892)
ret void
}

define tailcc void @proc_clo$ae49589(%struct.ScmObj* %env$ae49589,%struct.ScmObj* %current_45args54808) {
%stackaddr$env-ref55604 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49589, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55604
%stackaddr$env-ref55605 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49589, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55605
%stackaddr$env-ref55606 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49589, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55606
%stackaddr$env-ref55607 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49589, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55607
%stackaddr$env-ref55608 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49589, i64 4)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55608
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%current_45args54809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %current_45args54809, %struct.ScmObj** %stackaddr$prim55610, align 8
%stackaddr$prim55611 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54809)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim55611, align 8
%stackaddr$makeclosure55612 = alloca %struct.ScmObj*, align 8
%fptrToInt55613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49735 to i64
%ae49735 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55613)
store volatile %struct.ScmObj* %ae49735, %struct.ScmObj** %stackaddr$makeclosure55612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49735, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49735, %struct.ScmObj* %_37foldl148030, i64 1)
%ae49736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55614 = alloca %struct.ScmObj*, align 8
%fptrToInt55615 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49737 to i64
%ae49737 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55615)
store volatile %struct.ScmObj* %ae49737, %struct.ScmObj** %stackaddr$makeclosure55614, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49737, %struct.ScmObj* %_37map148077, i64 2)
%argslist55134$ae497350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%argslist55134$ae497351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49737, %struct.ScmObj* %argslist55134$ae497350)
store volatile %struct.ScmObj* %argslist55134$ae497351, %struct.ScmObj** %stackaddr$prim55616, align 8
%stackaddr$prim55617 = alloca %struct.ScmObj*, align 8
%argslist55134$ae497352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49736, %struct.ScmObj* %argslist55134$ae497351)
store volatile %struct.ScmObj* %argslist55134$ae497352, %struct.ScmObj** %stackaddr$prim55617, align 8
%clofunc55618 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49735)
musttail call tailcc void %clofunc55618(%struct.ScmObj* %ae49735, %struct.ScmObj* %argslist55134$ae497352)
ret void
}

define tailcc void @proc_clo$ae49735(%struct.ScmObj* %env$ae49735,%struct.ScmObj* %current_45args54811) {
%stackaddr$env-ref55619 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49735, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55619
%stackaddr$env-ref55620 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49735, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55620
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%current_45args54812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %current_45args54812, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54812)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55623, align 8
%stackaddr$makeclosure55624 = alloca %struct.ScmObj*, align 8
%fptrToInt55625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50127 to i64
%ae50127 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55625)
store volatile %struct.ScmObj* %ae50127, %struct.ScmObj** %stackaddr$makeclosure55624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50127, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist55074$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%argslist55074$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist55074$Ycmb480250)
store volatile %struct.ScmObj* %argslist55074$Ycmb480251, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%argslist55074$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50127, %struct.ScmObj* %argslist55074$Ycmb480251)
store volatile %struct.ScmObj* %argslist55074$Ycmb480252, %struct.ScmObj** %stackaddr$prim55627, align 8
%clofunc55628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55628(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55074$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50127(%struct.ScmObj* %env$ae50127,%struct.ScmObj* %current_45args54814) {
%stackaddr$env-ref55629 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50127, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55629
%stackaddr$prim55630 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54814)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim55630, align 8
%stackaddr$prim55631 = alloca %struct.ScmObj*, align 8
%current_45args54815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54814)
store volatile %struct.ScmObj* %current_45args54815, %struct.ScmObj** %stackaddr$prim55631, align 8
%stackaddr$prim55632 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54815)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim55632, align 8
%stackaddr$makeclosure55633 = alloca %struct.ScmObj*, align 8
%fptrToInt55634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50129 to i64
%ae50129 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55634)
store volatile %struct.ScmObj* %ae50129, %struct.ScmObj** %stackaddr$makeclosure55633, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50129, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55635 = alloca %struct.ScmObj*, align 8
%fptrToInt55636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50131 to i64
%ae50131 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55636)
store volatile %struct.ScmObj* %ae50131, %struct.ScmObj** %stackaddr$makeclosure55635, align 8
%argslist55073$ae501290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55637 = alloca %struct.ScmObj*, align 8
%argslist55073$ae501291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50131, %struct.ScmObj* %argslist55073$ae501290)
store volatile %struct.ScmObj* %argslist55073$ae501291, %struct.ScmObj** %stackaddr$prim55637, align 8
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%argslist55073$ae501292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50130, %struct.ScmObj* %argslist55073$ae501291)
store volatile %struct.ScmObj* %argslist55073$ae501292, %struct.ScmObj** %stackaddr$prim55638, align 8
%clofunc55639 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50129)
musttail call tailcc void %clofunc55639(%struct.ScmObj* %ae50129, %struct.ScmObj* %argslist55073$ae501292)
ret void
}

define tailcc void @proc_clo$ae50129(%struct.ScmObj* %env$ae50129,%struct.ScmObj* %current_45args54817) {
%stackaddr$env-ref55640 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50129, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55640
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54817)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim55641, align 8
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%current_45args54818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54817)
store volatile %struct.ScmObj* %current_45args54818, %struct.ScmObj** %stackaddr$prim55642, align 8
%stackaddr$prim55643 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54818)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim55643, align 8
%stackaddr$makeclosure55644 = alloca %struct.ScmObj*, align 8
%fptrToInt55645 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50153 to i64
%ae50153 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55645)
store volatile %struct.ScmObj* %ae50153, %struct.ScmObj** %stackaddr$makeclosure55644, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50153, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50154 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55646 = alloca %struct.ScmObj*, align 8
%fptrToInt55647 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50155 to i64
%ae50155 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55647)
store volatile %struct.ScmObj* %ae50155, %struct.ScmObj** %stackaddr$makeclosure55646, align 8
%argslist55067$ae501530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%argslist55067$ae501531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50155, %struct.ScmObj* %argslist55067$ae501530)
store volatile %struct.ScmObj* %argslist55067$ae501531, %struct.ScmObj** %stackaddr$prim55648, align 8
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%argslist55067$ae501532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50154, %struct.ScmObj* %argslist55067$ae501531)
store volatile %struct.ScmObj* %argslist55067$ae501532, %struct.ScmObj** %stackaddr$prim55649, align 8
%clofunc55650 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50153)
musttail call tailcc void %clofunc55650(%struct.ScmObj* %ae50153, %struct.ScmObj* %argslist55067$ae501532)
ret void
}

define tailcc void @proc_clo$ae50153(%struct.ScmObj* %env$ae50153,%struct.ScmObj* %current_45args54820) {
%stackaddr$env-ref55651 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50153, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55651
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54820)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%current_45args54821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54820)
store volatile %struct.ScmObj* %current_45args54821, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54821)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim55654, align 8
%ae50177 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50178 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50177, %struct.ScmObj* %ae50178)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim55655, align 8
%stackaddr$makeclosure55656 = alloca %struct.ScmObj*, align 8
%fptrToInt55657 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50179 to i64
%ae50179 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55657)
store volatile %struct.ScmObj* %ae50179, %struct.ScmObj** %stackaddr$makeclosure55656, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50179, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50179, %struct.ScmObj* %_37foldl148030, i64 1)
%ae50180 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55658 = alloca %struct.ScmObj*, align 8
%fptrToInt55659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50181 to i64
%ae50181 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55659)
store volatile %struct.ScmObj* %ae50181, %struct.ScmObj** %stackaddr$makeclosure55658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50181, %struct.ScmObj* %_37append48118, i64 0)
%argslist55061$ae501790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%argslist55061$ae501791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50181, %struct.ScmObj* %argslist55061$ae501790)
store volatile %struct.ScmObj* %argslist55061$ae501791, %struct.ScmObj** %stackaddr$prim55660, align 8
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%argslist55061$ae501792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50180, %struct.ScmObj* %argslist55061$ae501791)
store volatile %struct.ScmObj* %argslist55061$ae501792, %struct.ScmObj** %stackaddr$prim55661, align 8
%clofunc55662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50179)
musttail call tailcc void %clofunc55662(%struct.ScmObj* %ae50179, %struct.ScmObj* %argslist55061$ae501792)
ret void
}

define tailcc void @proc_clo$ae50179(%struct.ScmObj* %env$ae50179,%struct.ScmObj* %current_45args54823) {
%stackaddr$env-ref55663 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50179, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55663
%stackaddr$env-ref55664 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50179, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55664
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%current_45args54824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %current_45args54824, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54824)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55667, align 8
%ae50247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50247, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim55668, align 8
%ae50250 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50250)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$makeclosure55670 = alloca %struct.ScmObj*, align 8
%fptrToInt55671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50251 to i64
%ae50251 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55671)
store volatile %struct.ScmObj* %ae50251, %struct.ScmObj** %stackaddr$makeclosure55670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50251, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50252 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55672 = alloca %struct.ScmObj*, align 8
%fptrToInt55673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50253 to i64
%ae50253 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55673)
store volatile %struct.ScmObj* %ae50253, %struct.ScmObj** %stackaddr$makeclosure55672, align 8
%argslist55050$ae502510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist55050$ae502511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50253, %struct.ScmObj* %argslist55050$ae502510)
store volatile %struct.ScmObj* %argslist55050$ae502511, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%argslist55050$ae502512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist55050$ae502511)
store volatile %struct.ScmObj* %argslist55050$ae502512, %struct.ScmObj** %stackaddr$prim55675, align 8
%clofunc55676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50251)
musttail call tailcc void %clofunc55676(%struct.ScmObj* %ae50251, %struct.ScmObj* %argslist55050$ae502512)
ret void
}

define tailcc void @proc_clo$ae50251(%struct.ScmObj* %env$ae50251,%struct.ScmObj* %current_45args54826) {
%stackaddr$env-ref55677 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50251, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55677
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim55678, align 8
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%current_45args54827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %current_45args54827, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54827)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$makeclosure55681 = alloca %struct.ScmObj*, align 8
%fptrToInt55682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50667 to i64
%ae50667 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55682)
store volatile %struct.ScmObj* %ae50667, %struct.ScmObj** %stackaddr$makeclosure55681, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50667, %struct.ScmObj* %_37foldl148030, i64 0)
%ae50668 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55683 = alloca %struct.ScmObj*, align 8
%fptrToInt55684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50669 to i64
%ae50669 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55684)
store volatile %struct.ScmObj* %ae50669, %struct.ScmObj** %stackaddr$makeclosure55683, align 8
%argslist55025$ae506670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%argslist55025$ae506671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50669, %struct.ScmObj* %argslist55025$ae506670)
store volatile %struct.ScmObj* %argslist55025$ae506671, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%argslist55025$ae506672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50668, %struct.ScmObj* %argslist55025$ae506671)
store volatile %struct.ScmObj* %argslist55025$ae506672, %struct.ScmObj** %stackaddr$prim55686, align 8
%clofunc55687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50667)
musttail call tailcc void %clofunc55687(%struct.ScmObj* %ae50667, %struct.ScmObj* %argslist55025$ae506672)
ret void
}

define tailcc void @proc_clo$ae50667(%struct.ScmObj* %env$ae50667,%struct.ScmObj* %current_45args54829) {
%stackaddr$env-ref55688 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50667, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55688
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54829)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%current_45args54830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54829)
store volatile %struct.ScmObj* %current_45args54830, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54830)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55691, align 8
%stackaddr$makeclosure55692 = alloca %struct.ScmObj*, align 8
%fptrToInt55693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51203 to i64
%ae51203 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55693)
store volatile %struct.ScmObj* %ae51203, %struct.ScmObj** %stackaddr$makeclosure55692, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51203, %struct.ScmObj* %_37foldl148030, i64 0)
%ae51204 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55694 = alloca %struct.ScmObj*, align 8
%fptrToInt55695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51205 to i64
%ae51205 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55695)
store volatile %struct.ScmObj* %ae51205, %struct.ScmObj** %stackaddr$makeclosure55694, align 8
%argslist55001$ae512030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist55001$ae512031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51205, %struct.ScmObj* %argslist55001$ae512030)
store volatile %struct.ScmObj* %argslist55001$ae512031, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist55001$ae512032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51204, %struct.ScmObj* %argslist55001$ae512031)
store volatile %struct.ScmObj* %argslist55001$ae512032, %struct.ScmObj** %stackaddr$prim55697, align 8
%clofunc55698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51203)
musttail call tailcc void %clofunc55698(%struct.ScmObj* %ae51203, %struct.ScmObj* %argslist55001$ae512032)
ret void
}

define tailcc void @proc_clo$ae51203(%struct.ScmObj* %env$ae51203,%struct.ScmObj* %current_45args54832) {
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51203, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim55700, align 8
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%current_45args54833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %current_45args54833, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54833)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$makeclosure55703 = alloca %struct.ScmObj*, align 8
%fptrToInt55704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51607 to i64
%ae51607 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55704)
store volatile %struct.ScmObj* %ae51607, %struct.ScmObj** %stackaddr$makeclosure55703, align 8
%ae51608 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55705 = alloca %struct.ScmObj*, align 8
%fptrToInt55706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51609 to i64
%ae51609 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55706)
store volatile %struct.ScmObj* %ae51609, %struct.ScmObj** %stackaddr$makeclosure55705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51609, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54975$ae516070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%argslist54975$ae516071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51609, %struct.ScmObj* %argslist54975$ae516070)
store volatile %struct.ScmObj* %argslist54975$ae516071, %struct.ScmObj** %stackaddr$prim55707, align 8
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%argslist54975$ae516072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51608, %struct.ScmObj* %argslist54975$ae516071)
store volatile %struct.ScmObj* %argslist54975$ae516072, %struct.ScmObj** %stackaddr$prim55708, align 8
%clofunc55709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51607)
musttail call tailcc void %clofunc55709(%struct.ScmObj* %ae51607, %struct.ScmObj* %argslist54975$ae516072)
ret void
}

define tailcc void @proc_clo$ae51607(%struct.ScmObj* %env$ae51607,%struct.ScmObj* %current_45args54835) {
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54835)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%current_45args54836 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54835)
store volatile %struct.ScmObj* %current_45args54836, %struct.ScmObj** %stackaddr$prim55711, align 8
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54836)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$makeclosure55713 = alloca %struct.ScmObj*, align 8
%fptrToInt55714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51705 to i64
%ae51705 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55714)
store volatile %struct.ScmObj* %ae51705, %struct.ScmObj** %stackaddr$makeclosure55713, align 8
%ae51706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55715 = alloca %struct.ScmObj*, align 8
%fptrToInt55716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51707 to i64
%ae51707 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55716)
store volatile %struct.ScmObj* %ae51707, %struct.ScmObj** %stackaddr$makeclosure55715, align 8
%argslist54962$ae517050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%argslist54962$ae517051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51707, %struct.ScmObj* %argslist54962$ae517050)
store volatile %struct.ScmObj* %argslist54962$ae517051, %struct.ScmObj** %stackaddr$prim55717, align 8
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%argslist54962$ae517052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51706, %struct.ScmObj* %argslist54962$ae517051)
store volatile %struct.ScmObj* %argslist54962$ae517052, %struct.ScmObj** %stackaddr$prim55718, align 8
%clofunc55719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51705)
musttail call tailcc void %clofunc55719(%struct.ScmObj* %ae51705, %struct.ScmObj* %argslist54962$ae517052)
ret void
}

define tailcc void @proc_clo$ae51705(%struct.ScmObj* %env$ae51705,%struct.ScmObj* %current_45args54838) {
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54838)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim55720, align 8
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%current_45args54839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54838)
store volatile %struct.ScmObj* %current_45args54839, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54839)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$makeclosure55723 = alloca %struct.ScmObj*, align 8
%fptrToInt55724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51725 to i64
%ae51725 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55724)
store volatile %struct.ScmObj* %ae51725, %struct.ScmObj** %stackaddr$makeclosure55723, align 8
%ae51726 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55725 = alloca %struct.ScmObj*, align 8
%fptrToInt55726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51727 to i64
%ae51727 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55726)
store volatile %struct.ScmObj* %ae51727, %struct.ScmObj** %stackaddr$makeclosure55725, align 8
%argslist54957$ae517250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%argslist54957$ae517251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51727, %struct.ScmObj* %argslist54957$ae517250)
store volatile %struct.ScmObj* %argslist54957$ae517251, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$prim55728 = alloca %struct.ScmObj*, align 8
%argslist54957$ae517252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51726, %struct.ScmObj* %argslist54957$ae517251)
store volatile %struct.ScmObj* %argslist54957$ae517252, %struct.ScmObj** %stackaddr$prim55728, align 8
%clofunc55729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51725)
musttail call tailcc void %clofunc55729(%struct.ScmObj* %ae51725, %struct.ScmObj* %argslist54957$ae517252)
ret void
}

define tailcc void @proc_clo$ae51725(%struct.ScmObj* %env$ae51725,%struct.ScmObj* %current_45args54841) {
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54841)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim55730, align 8
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%current_45args54842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54841)
store volatile %struct.ScmObj* %current_45args54842, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54842)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$makeclosure55733 = alloca %struct.ScmObj*, align 8
%fptrToInt55734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51747 to i64
%ae51747 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55734)
store volatile %struct.ScmObj* %ae51747, %struct.ScmObj** %stackaddr$makeclosure55733, align 8
%ae51748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55735 = alloca %struct.ScmObj*, align 8
%fptrToInt55736 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51749 to i64
%ae51749 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55736)
store volatile %struct.ScmObj* %ae51749, %struct.ScmObj** %stackaddr$makeclosure55735, align 8
%argslist54952$ae517470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%argslist54952$ae517471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51749, %struct.ScmObj* %argslist54952$ae517470)
store volatile %struct.ScmObj* %argslist54952$ae517471, %struct.ScmObj** %stackaddr$prim55737, align 8
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%argslist54952$ae517472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51748, %struct.ScmObj* %argslist54952$ae517471)
store volatile %struct.ScmObj* %argslist54952$ae517472, %struct.ScmObj** %stackaddr$prim55738, align 8
%clofunc55739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51747)
musttail call tailcc void %clofunc55739(%struct.ScmObj* %ae51747, %struct.ScmObj* %argslist54952$ae517472)
ret void
}

define tailcc void @proc_clo$ae51747(%struct.ScmObj* %env$ae51747,%struct.ScmObj* %current_45args54844) {
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim55740, align 8
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%current_45args54845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %current_45args54845, %struct.ScmObj** %stackaddr$prim55741, align 8
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54845)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$makeclosure55743 = alloca %struct.ScmObj*, align 8
%fptrToInt55744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51771 to i64
%ae51771 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55744)
store volatile %struct.ScmObj* %ae51771, %struct.ScmObj** %stackaddr$makeclosure55743, align 8
%ae51772 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55745 = alloca %struct.ScmObj*, align 8
%fptrToInt55746 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51773 to i64
%ae51773 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55746)
store volatile %struct.ScmObj* %ae51773, %struct.ScmObj** %stackaddr$makeclosure55745, align 8
%argslist54947$ae517710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%argslist54947$ae517711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51773, %struct.ScmObj* %argslist54947$ae517710)
store volatile %struct.ScmObj* %argslist54947$ae517711, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%argslist54947$ae517712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51772, %struct.ScmObj* %argslist54947$ae517711)
store volatile %struct.ScmObj* %argslist54947$ae517712, %struct.ScmObj** %stackaddr$prim55748, align 8
%clofunc55749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51771)
musttail call tailcc void %clofunc55749(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist54947$ae517712)
ret void
}

define tailcc void @proc_clo$ae51771(%struct.ScmObj* %env$ae51771,%struct.ScmObj* %current_45args54847) {
%stackaddr$prim55750 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54847)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim55750, align 8
%stackaddr$prim55751 = alloca %struct.ScmObj*, align 8
%current_45args54848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54847)
store volatile %struct.ScmObj* %current_45args54848, %struct.ScmObj** %stackaddr$prim55751, align 8
%stackaddr$prim55752 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54848)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55752, align 8
%ae51797 = call %struct.ScmObj* @const_init_true()
%truthy$cmp55753 = call i64 @is_truthy_value(%struct.ScmObj* %ae51797)
%cmp$cmp55753 = icmp eq i64 %truthy$cmp55753, 1
br i1 %cmp$cmp55753, label %truebranch$cmp55753, label %falsebranch$cmp55753
truebranch$cmp55753:
%stackaddr$makeclosure55754 = alloca %struct.ScmObj*, align 8
%fptrToInt55755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51798 to i64
%ae51798 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55755)
store volatile %struct.ScmObj* %ae51798, %struct.ScmObj** %stackaddr$makeclosure55754, align 8
%ae51799 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51800 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist54880$ae517980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%argslist54880$ae517981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51800, %struct.ScmObj* %argslist54880$ae517980)
store volatile %struct.ScmObj* %argslist54880$ae517981, %struct.ScmObj** %stackaddr$prim55756, align 8
%stackaddr$prim55757 = alloca %struct.ScmObj*, align 8
%argslist54880$ae517982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51799, %struct.ScmObj* %argslist54880$ae517981)
store volatile %struct.ScmObj* %argslist54880$ae517982, %struct.ScmObj** %stackaddr$prim55757, align 8
%clofunc55758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51798)
musttail call tailcc void %clofunc55758(%struct.ScmObj* %ae51798, %struct.ScmObj* %argslist54880$ae517982)
ret void
falsebranch$cmp55753:
%ae51945 = call %struct.ScmObj* @const_init_int(i64 20)
%truthy$cmp55759 = call i64 @is_truthy_value(%struct.ScmObj* %ae51945)
%cmp$cmp55759 = icmp eq i64 %truthy$cmp55759, 1
br i1 %cmp$cmp55759, label %truebranch$cmp55759, label %falsebranch$cmp55759
truebranch$cmp55759:
%stackaddr$makeclosure55760 = alloca %struct.ScmObj*, align 8
%fptrToInt55761 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51946 to i64
%ae51946 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55761)
store volatile %struct.ScmObj* %ae51946, %struct.ScmObj** %stackaddr$makeclosure55760, align 8
%ae51947 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51948 = call %struct.ScmObj* @const_init_int(i64 20)
%argslist54911$ae519460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55762 = alloca %struct.ScmObj*, align 8
%argslist54911$ae519461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51948, %struct.ScmObj* %argslist54911$ae519460)
store volatile %struct.ScmObj* %argslist54911$ae519461, %struct.ScmObj** %stackaddr$prim55762, align 8
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%argslist54911$ae519462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51947, %struct.ScmObj* %argslist54911$ae519461)
store volatile %struct.ScmObj* %argslist54911$ae519462, %struct.ScmObj** %stackaddr$prim55763, align 8
%clofunc55764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51946)
musttail call tailcc void %clofunc55764(%struct.ScmObj* %ae51946, %struct.ScmObj* %argslist54911$ae519462)
ret void
falsebranch$cmp55759:
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%cpsprim48292 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim48292, %struct.ScmObj** %stackaddr$prim55765, align 8
%stackaddr$makeclosure55766 = alloca %struct.ScmObj*, align 8
%fptrToInt55767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52093 to i64
%ae52093 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55767)
store volatile %struct.ScmObj* %ae52093, %struct.ScmObj** %stackaddr$makeclosure55766, align 8
%ae52094 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54942$ae520930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%argslist54942$ae520931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48292, %struct.ScmObj* %argslist54942$ae520930)
store volatile %struct.ScmObj* %argslist54942$ae520931, %struct.ScmObj** %stackaddr$prim55768, align 8
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%argslist54942$ae520932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52094, %struct.ScmObj* %argslist54942$ae520931)
store volatile %struct.ScmObj* %argslist54942$ae520932, %struct.ScmObj** %stackaddr$prim55769, align 8
%clofunc55770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52093)
musttail call tailcc void %clofunc55770(%struct.ScmObj* %ae52093, %struct.ScmObj* %argslist54942$ae520932)
ret void
}

define tailcc void @proc_clo$ae51798(%struct.ScmObj* %env$ae51798,%struct.ScmObj* %current_45args54850) {
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54850)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55771, align 8
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%current_45args54851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54850)
store volatile %struct.ScmObj* %current_45args54851, %struct.ScmObj** %stackaddr$prim55772, align 8
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54851)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55773, align 8
%ae51807 = call %struct.ScmObj* @const_init_false()
%truthy$cmp55774 = call i64 @is_truthy_value(%struct.ScmObj* %ae51807)
%cmp$cmp55774 = icmp eq i64 %truthy$cmp55774, 1
br i1 %cmp$cmp55774, label %truebranch$cmp55774, label %falsebranch$cmp55774
truebranch$cmp55774:
%stackaddr$makeclosure55775 = alloca %struct.ScmObj*, align 8
%fptrToInt55776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51808 to i64
%ae51808 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55776)
store volatile %struct.ScmObj* %ae51808, %struct.ScmObj** %stackaddr$makeclosure55775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51808, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae51809 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51810 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist54861$ae518080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55777 = alloca %struct.ScmObj*, align 8
%argslist54861$ae518081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51810, %struct.ScmObj* %argslist54861$ae518080)
store volatile %struct.ScmObj* %argslist54861$ae518081, %struct.ScmObj** %stackaddr$prim55777, align 8
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%argslist54861$ae518082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51809, %struct.ScmObj* %argslist54861$ae518081)
store volatile %struct.ScmObj* %argslist54861$ae518082, %struct.ScmObj** %stackaddr$prim55778, align 8
%clofunc55779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51808)
musttail call tailcc void %clofunc55779(%struct.ScmObj* %ae51808, %struct.ScmObj* %argslist54861$ae518082)
ret void
falsebranch$cmp55774:
%ae51834 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp55780 = call i64 @is_truthy_value(%struct.ScmObj* %ae51834)
%cmp$cmp55780 = icmp eq i64 %truthy$cmp55780, 1
br i1 %cmp$cmp55780, label %truebranch$cmp55780, label %falsebranch$cmp55780
truebranch$cmp55780:
%stackaddr$makeclosure55781 = alloca %struct.ScmObj*, align 8
%fptrToInt55782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51835 to i64
%ae51835 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55782)
store volatile %struct.ScmObj* %ae51835, %struct.ScmObj** %stackaddr$makeclosure55781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51835, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae51836 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51837 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist54870$ae518350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55783 = alloca %struct.ScmObj*, align 8
%argslist54870$ae518351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51837, %struct.ScmObj* %argslist54870$ae518350)
store volatile %struct.ScmObj* %argslist54870$ae518351, %struct.ScmObj** %stackaddr$prim55783, align 8
%stackaddr$prim55784 = alloca %struct.ScmObj*, align 8
%argslist54870$ae518352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51836, %struct.ScmObj* %argslist54870$ae518351)
store volatile %struct.ScmObj* %argslist54870$ae518352, %struct.ScmObj** %stackaddr$prim55784, align 8
%clofunc55785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51835)
musttail call tailcc void %clofunc55785(%struct.ScmObj* %ae51835, %struct.ScmObj* %argslist54870$ae518352)
ret void
falsebranch$cmp55780:
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%cpsprim48291 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim48291, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$makeclosure55787 = alloca %struct.ScmObj*, align 8
%fptrToInt55788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51861 to i64
%ae51861 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55788)
store volatile %struct.ScmObj* %ae51861, %struct.ScmObj** %stackaddr$makeclosure55787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae51862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54879$ae518610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%argslist54879$ae518611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48291, %struct.ScmObj* %argslist54879$ae518610)
store volatile %struct.ScmObj* %argslist54879$ae518611, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%argslist54879$ae518612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51862, %struct.ScmObj* %argslist54879$ae518611)
store volatile %struct.ScmObj* %argslist54879$ae518612, %struct.ScmObj** %stackaddr$prim55790, align 8
%clofunc55791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51861)
musttail call tailcc void %clofunc55791(%struct.ScmObj* %ae51861, %struct.ScmObj* %argslist54879$ae518612)
ret void
}

define tailcc void @proc_clo$ae51808(%struct.ScmObj* %env$ae51808,%struct.ScmObj* %current_45args54853) {
%stackaddr$env-ref55792 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51808, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55792
%stackaddr$prim55793 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54853)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55793, align 8
%stackaddr$prim55794 = alloca %struct.ScmObj*, align 8
%current_45args54854 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54853)
store volatile %struct.ScmObj* %current_45args54854, %struct.ScmObj** %stackaddr$prim55794, align 8
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54854)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55796, align 8
%stackaddr$makeclosure55797 = alloca %struct.ScmObj*, align 8
%fptrToInt55798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51819 to i64
%ae51819 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55798)
store volatile %struct.ScmObj* %ae51819, %struct.ScmObj** %stackaddr$makeclosure55797, align 8
%ae51820 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54860$ae518190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%argslist54860$ae518191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54860$ae518190)
store volatile %struct.ScmObj* %argslist54860$ae518191, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%argslist54860$ae518192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51820, %struct.ScmObj* %argslist54860$ae518191)
store volatile %struct.ScmObj* %argslist54860$ae518192, %struct.ScmObj** %stackaddr$prim55800, align 8
%clofunc55801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51819)
musttail call tailcc void %clofunc55801(%struct.ScmObj* %ae51819, %struct.ScmObj* %argslist54860$ae518192)
ret void
}

define tailcc void @proc_clo$ae51819(%struct.ScmObj* %env$ae51819,%struct.ScmObj* %current_45args54856) {
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54856)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55802, align 8
%stackaddr$prim55803 = alloca %struct.ScmObj*, align 8
%current_45args54857 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54856)
store volatile %struct.ScmObj* %current_45args54857, %struct.ScmObj** %stackaddr$prim55803, align 8
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54857)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55804, align 8
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55805, align 8
%argslist54859$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%argslist54859$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54859$k0)
store volatile %struct.ScmObj* %argslist54859$k1, %struct.ScmObj** %stackaddr$prim55806, align 8
%clofunc55807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55807(%struct.ScmObj* %k, %struct.ScmObj* %argslist54859$k1)
ret void
}

define tailcc void @proc_clo$ae51835(%struct.ScmObj* %env$ae51835,%struct.ScmObj* %current_45args54862) {
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51835, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$prim55809 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54862)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55809, align 8
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%current_45args54863 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54862)
store volatile %struct.ScmObj* %current_45args54863, %struct.ScmObj** %stackaddr$prim55810, align 8
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54863)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$makeclosure55813 = alloca %struct.ScmObj*, align 8
%fptrToInt55814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51846 to i64
%ae51846 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55814)
store volatile %struct.ScmObj* %ae51846, %struct.ScmObj** %stackaddr$makeclosure55813, align 8
%ae51847 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54869$ae518460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%argslist54869$ae518461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54869$ae518460)
store volatile %struct.ScmObj* %argslist54869$ae518461, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%argslist54869$ae518462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51847, %struct.ScmObj* %argslist54869$ae518461)
store volatile %struct.ScmObj* %argslist54869$ae518462, %struct.ScmObj** %stackaddr$prim55816, align 8
%clofunc55817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51846)
musttail call tailcc void %clofunc55817(%struct.ScmObj* %ae51846, %struct.ScmObj* %argslist54869$ae518462)
ret void
}

define tailcc void @proc_clo$ae51846(%struct.ScmObj* %env$ae51846,%struct.ScmObj* %current_45args54865) {
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54865)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55818, align 8
%stackaddr$prim55819 = alloca %struct.ScmObj*, align 8
%current_45args54866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54865)
store volatile %struct.ScmObj* %current_45args54866, %struct.ScmObj** %stackaddr$prim55819, align 8
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54866)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55821, align 8
%argslist54868$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%argslist54868$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54868$k0)
store volatile %struct.ScmObj* %argslist54868$k1, %struct.ScmObj** %stackaddr$prim55822, align 8
%clofunc55823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55823(%struct.ScmObj* %k, %struct.ScmObj* %argslist54868$k1)
ret void
}

define tailcc void @proc_clo$ae51861(%struct.ScmObj* %env$ae51861,%struct.ScmObj* %current_45args54871) {
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$prim55825 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54871)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55825, align 8
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%current_45args54872 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54871)
store volatile %struct.ScmObj* %current_45args54872, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54872)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$makeclosure55829 = alloca %struct.ScmObj*, align 8
%fptrToInt55830 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51869 to i64
%ae51869 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55830)
store volatile %struct.ScmObj* %ae51869, %struct.ScmObj** %stackaddr$makeclosure55829, align 8
%ae51870 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54878$ae518690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%argslist54878$ae518691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54878$ae518690)
store volatile %struct.ScmObj* %argslist54878$ae518691, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%argslist54878$ae518692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51870, %struct.ScmObj* %argslist54878$ae518691)
store volatile %struct.ScmObj* %argslist54878$ae518692, %struct.ScmObj** %stackaddr$prim55832, align 8
%clofunc55833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51869)
musttail call tailcc void %clofunc55833(%struct.ScmObj* %ae51869, %struct.ScmObj* %argslist54878$ae518692)
ret void
}

define tailcc void @proc_clo$ae51869(%struct.ScmObj* %env$ae51869,%struct.ScmObj* %current_45args54874) {
%stackaddr$prim55834 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54874)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55834, align 8
%stackaddr$prim55835 = alloca %struct.ScmObj*, align 8
%current_45args54875 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54874)
store volatile %struct.ScmObj* %current_45args54875, %struct.ScmObj** %stackaddr$prim55835, align 8
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54875)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55836, align 8
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55837, align 8
%argslist54877$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%argslist54877$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54877$k0)
store volatile %struct.ScmObj* %argslist54877$k1, %struct.ScmObj** %stackaddr$prim55838, align 8
%clofunc55839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55839(%struct.ScmObj* %k, %struct.ScmObj* %argslist54877$k1)
ret void
}

define tailcc void @proc_clo$ae51946(%struct.ScmObj* %env$ae51946,%struct.ScmObj* %current_45args54881) {
%stackaddr$prim55840 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54881)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55840, align 8
%stackaddr$prim55841 = alloca %struct.ScmObj*, align 8
%current_45args54882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54881)
store volatile %struct.ScmObj* %current_45args54882, %struct.ScmObj** %stackaddr$prim55841, align 8
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54882)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55842, align 8
%ae51955 = call %struct.ScmObj* @const_init_false()
%truthy$cmp55843 = call i64 @is_truthy_value(%struct.ScmObj* %ae51955)
%cmp$cmp55843 = icmp eq i64 %truthy$cmp55843, 1
br i1 %cmp$cmp55843, label %truebranch$cmp55843, label %falsebranch$cmp55843
truebranch$cmp55843:
%stackaddr$makeclosure55844 = alloca %struct.ScmObj*, align 8
%fptrToInt55845 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51956 to i64
%ae51956 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55845)
store volatile %struct.ScmObj* %ae51956, %struct.ScmObj** %stackaddr$makeclosure55844, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51956, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae51957 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51958 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist54892$ae519560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%argslist54892$ae519561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51958, %struct.ScmObj* %argslist54892$ae519560)
store volatile %struct.ScmObj* %argslist54892$ae519561, %struct.ScmObj** %stackaddr$prim55846, align 8
%stackaddr$prim55847 = alloca %struct.ScmObj*, align 8
%argslist54892$ae519562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51957, %struct.ScmObj* %argslist54892$ae519561)
store volatile %struct.ScmObj* %argslist54892$ae519562, %struct.ScmObj** %stackaddr$prim55847, align 8
%clofunc55848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51956)
musttail call tailcc void %clofunc55848(%struct.ScmObj* %ae51956, %struct.ScmObj* %argslist54892$ae519562)
ret void
falsebranch$cmp55843:
%ae51982 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp55849 = call i64 @is_truthy_value(%struct.ScmObj* %ae51982)
%cmp$cmp55849 = icmp eq i64 %truthy$cmp55849, 1
br i1 %cmp$cmp55849, label %truebranch$cmp55849, label %falsebranch$cmp55849
truebranch$cmp55849:
%stackaddr$makeclosure55850 = alloca %struct.ScmObj*, align 8
%fptrToInt55851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51983 to i64
%ae51983 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55851)
store volatile %struct.ScmObj* %ae51983, %struct.ScmObj** %stackaddr$makeclosure55850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51983, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae51984 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51985 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist54901$ae519830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55852 = alloca %struct.ScmObj*, align 8
%argslist54901$ae519831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51985, %struct.ScmObj* %argslist54901$ae519830)
store volatile %struct.ScmObj* %argslist54901$ae519831, %struct.ScmObj** %stackaddr$prim55852, align 8
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%argslist54901$ae519832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51984, %struct.ScmObj* %argslist54901$ae519831)
store volatile %struct.ScmObj* %argslist54901$ae519832, %struct.ScmObj** %stackaddr$prim55853, align 8
%clofunc55854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51983)
musttail call tailcc void %clofunc55854(%struct.ScmObj* %ae51983, %struct.ScmObj* %argslist54901$ae519832)
ret void
falsebranch$cmp55849:
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%cpsprim48291 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim48291, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$makeclosure55856 = alloca %struct.ScmObj*, align 8
%fptrToInt55857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52009 to i64
%ae52009 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55857)
store volatile %struct.ScmObj* %ae52009, %struct.ScmObj** %stackaddr$makeclosure55856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52009, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae52010 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54910$ae520090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55858 = alloca %struct.ScmObj*, align 8
%argslist54910$ae520091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48291, %struct.ScmObj* %argslist54910$ae520090)
store volatile %struct.ScmObj* %argslist54910$ae520091, %struct.ScmObj** %stackaddr$prim55858, align 8
%stackaddr$prim55859 = alloca %struct.ScmObj*, align 8
%argslist54910$ae520092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52010, %struct.ScmObj* %argslist54910$ae520091)
store volatile %struct.ScmObj* %argslist54910$ae520092, %struct.ScmObj** %stackaddr$prim55859, align 8
%clofunc55860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52009)
musttail call tailcc void %clofunc55860(%struct.ScmObj* %ae52009, %struct.ScmObj* %argslist54910$ae520092)
ret void
}

define tailcc void @proc_clo$ae51956(%struct.ScmObj* %env$ae51956,%struct.ScmObj* %current_45args54884) {
%stackaddr$env-ref55861 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51956, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55861
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54884)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55862, align 8
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%current_45args54885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54884)
store volatile %struct.ScmObj* %current_45args54885, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54885)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$makeclosure55866 = alloca %struct.ScmObj*, align 8
%fptrToInt55867 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51967 to i64
%ae51967 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55867)
store volatile %struct.ScmObj* %ae51967, %struct.ScmObj** %stackaddr$makeclosure55866, align 8
%ae51968 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54891$ae519670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%argslist54891$ae519671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54891$ae519670)
store volatile %struct.ScmObj* %argslist54891$ae519671, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%argslist54891$ae519672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51968, %struct.ScmObj* %argslist54891$ae519671)
store volatile %struct.ScmObj* %argslist54891$ae519672, %struct.ScmObj** %stackaddr$prim55869, align 8
%clofunc55870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51967)
musttail call tailcc void %clofunc55870(%struct.ScmObj* %ae51967, %struct.ScmObj* %argslist54891$ae519672)
ret void
}

define tailcc void @proc_clo$ae51967(%struct.ScmObj* %env$ae51967,%struct.ScmObj* %current_45args54887) {
%stackaddr$prim55871 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54887)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55871, align 8
%stackaddr$prim55872 = alloca %struct.ScmObj*, align 8
%current_45args54888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54887)
store volatile %struct.ScmObj* %current_45args54888, %struct.ScmObj** %stackaddr$prim55872, align 8
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54888)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55874, align 8
%argslist54890$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55875 = alloca %struct.ScmObj*, align 8
%argslist54890$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54890$k0)
store volatile %struct.ScmObj* %argslist54890$k1, %struct.ScmObj** %stackaddr$prim55875, align 8
%clofunc55876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55876(%struct.ScmObj* %k, %struct.ScmObj* %argslist54890$k1)
ret void
}

define tailcc void @proc_clo$ae51983(%struct.ScmObj* %env$ae51983,%struct.ScmObj* %current_45args54893) {
%stackaddr$env-ref55877 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51983, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55877
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54893)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%current_45args54894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54893)
store volatile %struct.ScmObj* %current_45args54894, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54894)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$makeclosure55882 = alloca %struct.ScmObj*, align 8
%fptrToInt55883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51994 to i64
%ae51994 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55883)
store volatile %struct.ScmObj* %ae51994, %struct.ScmObj** %stackaddr$makeclosure55882, align 8
%ae51995 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54900$ae519940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist54900$ae519941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54900$ae519940)
store volatile %struct.ScmObj* %argslist54900$ae519941, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist54900$ae519942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51995, %struct.ScmObj* %argslist54900$ae519941)
store volatile %struct.ScmObj* %argslist54900$ae519942, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51994)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %ae51994, %struct.ScmObj* %argslist54900$ae519942)
ret void
}

define tailcc void @proc_clo$ae51994(%struct.ScmObj* %env$ae51994,%struct.ScmObj* %current_45args54896) {
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54896)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%current_45args54897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54896)
store volatile %struct.ScmObj* %current_45args54897, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54897)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55890, align 8
%argslist54899$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%argslist54899$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54899$k0)
store volatile %struct.ScmObj* %argslist54899$k1, %struct.ScmObj** %stackaddr$prim55891, align 8
%clofunc55892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55892(%struct.ScmObj* %k, %struct.ScmObj* %argslist54899$k1)
ret void
}

define tailcc void @proc_clo$ae52009(%struct.ScmObj* %env$ae52009,%struct.ScmObj* %current_45args54902) {
%stackaddr$env-ref55893 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52009, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55893
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54902)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%current_45args54903 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54902)
store volatile %struct.ScmObj* %current_45args54903, %struct.ScmObj** %stackaddr$prim55895, align 8
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54903)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55896, align 8
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55897, align 8
%stackaddr$makeclosure55898 = alloca %struct.ScmObj*, align 8
%fptrToInt55899 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52017 to i64
%ae52017 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55899)
store volatile %struct.ScmObj* %ae52017, %struct.ScmObj** %stackaddr$makeclosure55898, align 8
%ae52018 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54909$ae520170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%argslist54909$ae520171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54909$ae520170)
store volatile %struct.ScmObj* %argslist54909$ae520171, %struct.ScmObj** %stackaddr$prim55900, align 8
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%argslist54909$ae520172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52018, %struct.ScmObj* %argslist54909$ae520171)
store volatile %struct.ScmObj* %argslist54909$ae520172, %struct.ScmObj** %stackaddr$prim55901, align 8
%clofunc55902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52017)
musttail call tailcc void %clofunc55902(%struct.ScmObj* %ae52017, %struct.ScmObj* %argslist54909$ae520172)
ret void
}

define tailcc void @proc_clo$ae52017(%struct.ScmObj* %env$ae52017,%struct.ScmObj* %current_45args54905) {
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54905)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%current_45args54906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54905)
store volatile %struct.ScmObj* %current_45args54906, %struct.ScmObj** %stackaddr$prim55904, align 8
%stackaddr$prim55905 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54906)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55905, align 8
%stackaddr$prim55906 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55906, align 8
%argslist54908$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%argslist54908$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54908$k0)
store volatile %struct.ScmObj* %argslist54908$k1, %struct.ScmObj** %stackaddr$prim55907, align 8
%clofunc55908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55908(%struct.ScmObj* %k, %struct.ScmObj* %argslist54908$k1)
ret void
}

define tailcc void @proc_clo$ae52093(%struct.ScmObj* %env$ae52093,%struct.ScmObj* %current_45args54912) {
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54912)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%current_45args54913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54912)
store volatile %struct.ScmObj* %current_45args54913, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54913)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim55911, align 8
%ae52099 = call %struct.ScmObj* @const_init_false()
%truthy$cmp55912 = call i64 @is_truthy_value(%struct.ScmObj* %ae52099)
%cmp$cmp55912 = icmp eq i64 %truthy$cmp55912, 1
br i1 %cmp$cmp55912, label %truebranch$cmp55912, label %falsebranch$cmp55912
truebranch$cmp55912:
%stackaddr$makeclosure55913 = alloca %struct.ScmObj*, align 8
%fptrToInt55914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52100 to i64
%ae52100 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55914)
store volatile %struct.ScmObj* %ae52100, %struct.ScmObj** %stackaddr$makeclosure55913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52100, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae52101 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52102 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist54923$ae521000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55915 = alloca %struct.ScmObj*, align 8
%argslist54923$ae521001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52102, %struct.ScmObj* %argslist54923$ae521000)
store volatile %struct.ScmObj* %argslist54923$ae521001, %struct.ScmObj** %stackaddr$prim55915, align 8
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%argslist54923$ae521002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52101, %struct.ScmObj* %argslist54923$ae521001)
store volatile %struct.ScmObj* %argslist54923$ae521002, %struct.ScmObj** %stackaddr$prim55916, align 8
%clofunc55917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52100)
musttail call tailcc void %clofunc55917(%struct.ScmObj* %ae52100, %struct.ScmObj* %argslist54923$ae521002)
ret void
falsebranch$cmp55912:
%ae52126 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp55918 = call i64 @is_truthy_value(%struct.ScmObj* %ae52126)
%cmp$cmp55918 = icmp eq i64 %truthy$cmp55918, 1
br i1 %cmp$cmp55918, label %truebranch$cmp55918, label %falsebranch$cmp55918
truebranch$cmp55918:
%stackaddr$makeclosure55919 = alloca %struct.ScmObj*, align 8
%fptrToInt55920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52127 to i64
%ae52127 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55920)
store volatile %struct.ScmObj* %ae52127, %struct.ScmObj** %stackaddr$makeclosure55919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae52128 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52129 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist54932$ae521270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist54932$ae521271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52129, %struct.ScmObj* %argslist54932$ae521270)
store volatile %struct.ScmObj* %argslist54932$ae521271, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%argslist54932$ae521272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52128, %struct.ScmObj* %argslist54932$ae521271)
store volatile %struct.ScmObj* %argslist54932$ae521272, %struct.ScmObj** %stackaddr$prim55922, align 8
%clofunc55923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52127)
musttail call tailcc void %clofunc55923(%struct.ScmObj* %ae52127, %struct.ScmObj* %argslist54932$ae521272)
ret void
falsebranch$cmp55918:
%stackaddr$prim55924 = alloca %struct.ScmObj*, align 8
%cpsprim48291 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim48291, %struct.ScmObj** %stackaddr$prim55924, align 8
%stackaddr$makeclosure55925 = alloca %struct.ScmObj*, align 8
%fptrToInt55926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52153 to i64
%ae52153 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55926)
store volatile %struct.ScmObj* %ae52153, %struct.ScmObj** %stackaddr$makeclosure55925, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52153, %struct.ScmObj* %anf_45bind48254, i64 0)
%ae52154 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54941$ae521530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%argslist54941$ae521531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48291, %struct.ScmObj* %argslist54941$ae521530)
store volatile %struct.ScmObj* %argslist54941$ae521531, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%argslist54941$ae521532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52154, %struct.ScmObj* %argslist54941$ae521531)
store volatile %struct.ScmObj* %argslist54941$ae521532, %struct.ScmObj** %stackaddr$prim55928, align 8
%clofunc55929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52153)
musttail call tailcc void %clofunc55929(%struct.ScmObj* %ae52153, %struct.ScmObj* %argslist54941$ae521532)
ret void
}

define tailcc void @proc_clo$ae52100(%struct.ScmObj* %env$ae52100,%struct.ScmObj* %current_45args54915) {
%stackaddr$env-ref55930 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52100, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55930
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54915)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%current_45args54916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54915)
store volatile %struct.ScmObj* %current_45args54916, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54916)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55933, align 8
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$makeclosure55935 = alloca %struct.ScmObj*, align 8
%fptrToInt55936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52111 to i64
%ae52111 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55936)
store volatile %struct.ScmObj* %ae52111, %struct.ScmObj** %stackaddr$makeclosure55935, align 8
%ae52112 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54922$ae521110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%argslist54922$ae521111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54922$ae521110)
store volatile %struct.ScmObj* %argslist54922$ae521111, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%argslist54922$ae521112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52112, %struct.ScmObj* %argslist54922$ae521111)
store volatile %struct.ScmObj* %argslist54922$ae521112, %struct.ScmObj** %stackaddr$prim55938, align 8
%clofunc55939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52111)
musttail call tailcc void %clofunc55939(%struct.ScmObj* %ae52111, %struct.ScmObj* %argslist54922$ae521112)
ret void
}

define tailcc void @proc_clo$ae52111(%struct.ScmObj* %env$ae52111,%struct.ScmObj* %current_45args54918) {
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54918)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%current_45args54919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54918)
store volatile %struct.ScmObj* %current_45args54919, %struct.ScmObj** %stackaddr$prim55941, align 8
%stackaddr$prim55942 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54919)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55942, align 8
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55943, align 8
%argslist54921$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%argslist54921$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54921$k0)
store volatile %struct.ScmObj* %argslist54921$k1, %struct.ScmObj** %stackaddr$prim55944, align 8
%clofunc55945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55945(%struct.ScmObj* %k, %struct.ScmObj* %argslist54921$k1)
ret void
}

define tailcc void @proc_clo$ae52127(%struct.ScmObj* %env$ae52127,%struct.ScmObj* %current_45args54924) {
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54924)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%current_45args54925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54924)
store volatile %struct.ScmObj* %current_45args54925, %struct.ScmObj** %stackaddr$prim55948, align 8
%stackaddr$prim55949 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54925)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55949, align 8
%stackaddr$prim55950 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55950, align 8
%stackaddr$makeclosure55951 = alloca %struct.ScmObj*, align 8
%fptrToInt55952 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52138 to i64
%ae52138 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55952)
store volatile %struct.ScmObj* %ae52138, %struct.ScmObj** %stackaddr$makeclosure55951, align 8
%ae52139 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54931$ae521380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%argslist54931$ae521381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54931$ae521380)
store volatile %struct.ScmObj* %argslist54931$ae521381, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%argslist54931$ae521382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52139, %struct.ScmObj* %argslist54931$ae521381)
store volatile %struct.ScmObj* %argslist54931$ae521382, %struct.ScmObj** %stackaddr$prim55954, align 8
%clofunc55955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52138)
musttail call tailcc void %clofunc55955(%struct.ScmObj* %ae52138, %struct.ScmObj* %argslist54931$ae521382)
ret void
}

define tailcc void @proc_clo$ae52138(%struct.ScmObj* %env$ae52138,%struct.ScmObj* %current_45args54927) {
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54927)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%current_45args54928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54927)
store volatile %struct.ScmObj* %current_45args54928, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54928)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55958, align 8
%stackaddr$prim55959 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55959, align 8
%argslist54930$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55960 = alloca %struct.ScmObj*, align 8
%argslist54930$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54930$k0)
store volatile %struct.ScmObj* %argslist54930$k1, %struct.ScmObj** %stackaddr$prim55960, align 8
%clofunc55961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55961(%struct.ScmObj* %k, %struct.ScmObj* %argslist54930$k1)
ret void
}

define tailcc void @proc_clo$ae52153(%struct.ScmObj* %env$ae52153,%struct.ScmObj* %current_45args54933) {
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52153, i64 0)
store %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$prim55963 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54933)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55963, align 8
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%current_45args54934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54933)
store volatile %struct.ScmObj* %current_45args54934, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54934)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%cpsprim48290 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48290, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$makeclosure55967 = alloca %struct.ScmObj*, align 8
%fptrToInt55968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52161 to i64
%ae52161 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55968)
store volatile %struct.ScmObj* %ae52161, %struct.ScmObj** %stackaddr$makeclosure55967, align 8
%ae52162 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54940$ae521610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%argslist54940$ae521611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48290, %struct.ScmObj* %argslist54940$ae521610)
store volatile %struct.ScmObj* %argslist54940$ae521611, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%argslist54940$ae521612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52162, %struct.ScmObj* %argslist54940$ae521611)
store volatile %struct.ScmObj* %argslist54940$ae521612, %struct.ScmObj** %stackaddr$prim55970, align 8
%clofunc55971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52161)
musttail call tailcc void %clofunc55971(%struct.ScmObj* %ae52161, %struct.ScmObj* %argslist54940$ae521612)
ret void
}

define tailcc void @proc_clo$ae52161(%struct.ScmObj* %env$ae52161,%struct.ScmObj* %current_45args54936) {
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54936)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55972, align 8
%stackaddr$prim55973 = alloca %struct.ScmObj*, align 8
%current_45args54937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54936)
store volatile %struct.ScmObj* %current_45args54937, %struct.ScmObj** %stackaddr$prim55973, align 8
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54937)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55975, align 8
%argslist54939$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55976 = alloca %struct.ScmObj*, align 8
%argslist54939$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54939$k0)
store volatile %struct.ScmObj* %argslist54939$k1, %struct.ScmObj** %stackaddr$prim55976, align 8
%clofunc55977 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55977(%struct.ScmObj* %k, %struct.ScmObj* %argslist54939$k1)
ret void
}

define tailcc void @proc_clo$ae51773(%struct.ScmObj* %env$ae51773,%struct.ScmObj* %current_45args54943) {
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54943)
store volatile %struct.ScmObj* %k48293, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%current_45args54944 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54943)
store volatile %struct.ScmObj* %current_45args54944, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54944)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim55982, align 8
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim55983, align 8
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%cpsprim48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %cpsprim48294, %struct.ScmObj** %stackaddr$prim55984, align 8
%ae51779 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54946$k482930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%argslist54946$k482931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48294, %struct.ScmObj* %argslist54946$k482930)
store volatile %struct.ScmObj* %argslist54946$k482931, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%argslist54946$k482932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51779, %struct.ScmObj* %argslist54946$k482931)
store volatile %struct.ScmObj* %argslist54946$k482932, %struct.ScmObj** %stackaddr$prim55986, align 8
%clofunc55987 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48293)
musttail call tailcc void %clofunc55987(%struct.ScmObj* %k48293, %struct.ScmObj* %argslist54946$k482932)
ret void
}

define tailcc void @proc_clo$ae51749(%struct.ScmObj* %env$ae51749,%struct.ScmObj* %current_45args54948) {
%stackaddr$prim55988 = alloca %struct.ScmObj*, align 8
%k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54948)
store volatile %struct.ScmObj* %k48295, %struct.ScmObj** %stackaddr$prim55988, align 8
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%current_45args54949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54948)
store volatile %struct.ScmObj* %current_45args54949, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54949)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim55991, align 8
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%cpsprim48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %cpsprim48296, %struct.ScmObj** %stackaddr$prim55993, align 8
%ae51754 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54951$k482950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%argslist54951$k482951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48296, %struct.ScmObj* %argslist54951$k482950)
store volatile %struct.ScmObj* %argslist54951$k482951, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%argslist54951$k482952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51754, %struct.ScmObj* %argslist54951$k482951)
store volatile %struct.ScmObj* %argslist54951$k482952, %struct.ScmObj** %stackaddr$prim55995, align 8
%clofunc55996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48295)
musttail call tailcc void %clofunc55996(%struct.ScmObj* %k48295, %struct.ScmObj* %argslist54951$k482952)
ret void
}

define tailcc void @proc_clo$ae51727(%struct.ScmObj* %env$ae51727,%struct.ScmObj* %current_45args54953) {
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54953)
store volatile %struct.ScmObj* %k48297, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%current_45args54954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54953)
store volatile %struct.ScmObj* %current_45args54954, %struct.ScmObj** %stackaddr$prim55998, align 8
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54954)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%cpsprim48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %cpsprim48298, %struct.ScmObj** %stackaddr$prim56001, align 8
%ae51731 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54956$k482970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%argslist54956$k482971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48298, %struct.ScmObj* %argslist54956$k482970)
store volatile %struct.ScmObj* %argslist54956$k482971, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%argslist54956$k482972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51731, %struct.ScmObj* %argslist54956$k482971)
store volatile %struct.ScmObj* %argslist54956$k482972, %struct.ScmObj** %stackaddr$prim56003, align 8
%clofunc56004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48297)
musttail call tailcc void %clofunc56004(%struct.ScmObj* %k48297, %struct.ScmObj* %argslist54956$k482972)
ret void
}

define tailcc void @proc_clo$ae51707(%struct.ScmObj* %env$ae51707,%struct.ScmObj* %current_45args54958) {
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54958)
store volatile %struct.ScmObj* %k48299, %struct.ScmObj** %stackaddr$prim56005, align 8
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%current_45args54959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54958)
store volatile %struct.ScmObj* %current_45args54959, %struct.ScmObj** %stackaddr$prim56006, align 8
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54959)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim56007, align 8
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%cpsprim48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48300, %struct.ScmObj** %stackaddr$prim56008, align 8
%ae51710 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54961$k482990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%argslist54961$k482991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48300, %struct.ScmObj* %argslist54961$k482990)
store volatile %struct.ScmObj* %argslist54961$k482991, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%argslist54961$k482992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51710, %struct.ScmObj* %argslist54961$k482991)
store volatile %struct.ScmObj* %argslist54961$k482992, %struct.ScmObj** %stackaddr$prim56010, align 8
%clofunc56011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48299)
musttail call tailcc void %clofunc56011(%struct.ScmObj* %k48299, %struct.ScmObj* %argslist54961$k482992)
ret void
}

define tailcc void @proc_clo$ae51609(%struct.ScmObj* %env$ae51609,%struct.ScmObj* %args4809148301) {
%stackaddr$env-ref56012 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51609, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56012
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148301)
store volatile %struct.ScmObj* %k48302, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148301)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim56014, align 8
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56015, align 8
%truthy$cmp56016 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp56016 = icmp eq i64 %truthy$cmp56016, 1
br i1 %cmp$cmp56016, label %truebranch$cmp56016, label %falsebranch$cmp56016
truebranch$cmp56016:
%ae51615 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51616 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54963$k483020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%argslist54963$k483021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51616, %struct.ScmObj* %argslist54963$k483020)
store volatile %struct.ScmObj* %argslist54963$k483021, %struct.ScmObj** %stackaddr$prim56017, align 8
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%argslist54963$k483022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51615, %struct.ScmObj* %argslist54963$k483021)
store volatile %struct.ScmObj* %argslist54963$k483022, %struct.ScmObj** %stackaddr$prim56018, align 8
%clofunc56019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48302)
musttail call tailcc void %clofunc56019(%struct.ScmObj* %k48302, %struct.ScmObj* %argslist54963$k483022)
ret void
falsebranch$cmp56016:
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim56020, align 8
%stackaddr$prim56021 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim56021, align 8
%truthy$cmp56022 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp56022 = icmp eq i64 %truthy$cmp56022, 1
br i1 %cmp$cmp56022, label %truebranch$cmp56022, label %falsebranch$cmp56022
truebranch$cmp56022:
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%cpsprim48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48303, %struct.ScmObj** %stackaddr$prim56023, align 8
%ae51628 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54964$k483020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%argslist54964$k483021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48303, %struct.ScmObj* %argslist54964$k483020)
store volatile %struct.ScmObj* %argslist54964$k483021, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%argslist54964$k483022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51628, %struct.ScmObj* %argslist54964$k483021)
store volatile %struct.ScmObj* %argslist54964$k483022, %struct.ScmObj** %stackaddr$prim56025, align 8
%clofunc56026 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48302)
musttail call tailcc void %clofunc56026(%struct.ScmObj* %k48302, %struct.ScmObj* %argslist54964$k483022)
ret void
falsebranch$cmp56022:
%stackaddr$makeclosure56027 = alloca %struct.ScmObj*, align 8
%fptrToInt56028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51633 to i64
%ae51633 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56028)
store volatile %struct.ScmObj* %ae51633, %struct.ScmObj** %stackaddr$makeclosure56027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51633, %struct.ScmObj* %k48302, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51633, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51633, %struct.ScmObj* %args48091, i64 2)
%ae51634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56029 = alloca %struct.ScmObj*, align 8
%fptrToInt56030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51635 to i64
%ae51635 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56030)
store volatile %struct.ScmObj* %ae51635, %struct.ScmObj** %stackaddr$makeclosure56029, align 8
%argslist54974$ae516330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%argslist54974$ae516331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51635, %struct.ScmObj* %argslist54974$ae516330)
store volatile %struct.ScmObj* %argslist54974$ae516331, %struct.ScmObj** %stackaddr$prim56031, align 8
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%argslist54974$ae516332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51634, %struct.ScmObj* %argslist54974$ae516331)
store volatile %struct.ScmObj* %argslist54974$ae516332, %struct.ScmObj** %stackaddr$prim56032, align 8
%clofunc56033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51633)
musttail call tailcc void %clofunc56033(%struct.ScmObj* %ae51633, %struct.ScmObj* %argslist54974$ae516332)
ret void
}

define tailcc void @proc_clo$ae51633(%struct.ScmObj* %env$ae51633,%struct.ScmObj* %current_45args54965) {
%stackaddr$env-ref56034 = alloca %struct.ScmObj*, align 8
%k48302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51633, i64 0)
store %struct.ScmObj* %k48302, %struct.ScmObj** %stackaddr$env-ref56034
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51633, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$env-ref56036 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51633, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref56036
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54965)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%current_45args54966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54965)
store volatile %struct.ScmObj* %current_45args54966, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54966)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim56041, align 8
%argslist54968$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%argslist54968$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48247, %struct.ScmObj* %argslist54968$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54968$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%argslist54968$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48246, %struct.ScmObj* %argslist54968$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54968$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56043, align 8
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%argslist54968$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48245, %struct.ScmObj* %argslist54968$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54968$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist54968$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48302, %struct.ScmObj* %argslist54968$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54968$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56045, align 8
%clofunc56046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56046(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54968$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51635(%struct.ScmObj* %env$ae51635,%struct.ScmObj* %current_45args54969) {
%stackaddr$prim56047 = alloca %struct.ScmObj*, align 8
%k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54969)
store volatile %struct.ScmObj* %k48305, %struct.ScmObj** %stackaddr$prim56047, align 8
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%current_45args54970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54969)
store volatile %struct.ScmObj* %current_45args54970, %struct.ScmObj** %stackaddr$prim56048, align 8
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54970)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%current_45args54971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54970)
store volatile %struct.ScmObj* %current_45args54971, %struct.ScmObj** %stackaddr$prim56050, align 8
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54971)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%cpsprim48306 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48306, %struct.ScmObj** %stackaddr$prim56052, align 8
%ae51639 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54973$k483050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%argslist54973$k483051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48306, %struct.ScmObj* %argslist54973$k483050)
store volatile %struct.ScmObj* %argslist54973$k483051, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%argslist54973$k483052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51639, %struct.ScmObj* %argslist54973$k483051)
store volatile %struct.ScmObj* %argslist54973$k483052, %struct.ScmObj** %stackaddr$prim56054, align 8
%clofunc56055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48305)
musttail call tailcc void %clofunc56055(%struct.ScmObj* %k48305, %struct.ScmObj* %argslist54973$k483052)
ret void
}

define tailcc void @proc_clo$ae51205(%struct.ScmObj* %env$ae51205,%struct.ScmObj* %current_45args54976) {
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54976)
store volatile %struct.ScmObj* %k48307, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%current_45args54977 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54976)
store volatile %struct.ScmObj* %current_45args54977, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54977)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%current_45args54978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54977)
store volatile %struct.ScmObj* %current_45args54978, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54978)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim56060, align 8
%ae51206 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51206, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$makeclosure56062 = alloca %struct.ScmObj*, align 8
%fptrToInt56063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51208 to i64
%ae51208 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56063)
store volatile %struct.ScmObj* %ae51208, %struct.ScmObj** %stackaddr$makeclosure56062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51208, %struct.ScmObj* %k48307, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51208, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51208, %struct.ScmObj* %v48096, i64 2)
%ae51209 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56064 = alloca %struct.ScmObj*, align 8
%fptrToInt56065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51210 to i64
%ae51210 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56065)
store volatile %struct.ScmObj* %ae51210, %struct.ScmObj** %stackaddr$makeclosure56064, align 8
%argslist55000$ae512080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%argslist55000$ae512081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51210, %struct.ScmObj* %argslist55000$ae512080)
store volatile %struct.ScmObj* %argslist55000$ae512081, %struct.ScmObj** %stackaddr$prim56066, align 8
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%argslist55000$ae512082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51209, %struct.ScmObj* %argslist55000$ae512081)
store volatile %struct.ScmObj* %argslist55000$ae512082, %struct.ScmObj** %stackaddr$prim56067, align 8
%clofunc56068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51208)
musttail call tailcc void %clofunc56068(%struct.ScmObj* %ae51208, %struct.ScmObj* %argslist55000$ae512082)
ret void
}

define tailcc void @proc_clo$ae51208(%struct.ScmObj* %env$ae51208,%struct.ScmObj* %current_45args54980) {
%stackaddr$env-ref56069 = alloca %struct.ScmObj*, align 8
%k48307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51208, i64 0)
store %struct.ScmObj* %k48307, %struct.ScmObj** %stackaddr$env-ref56069
%stackaddr$env-ref56070 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51208, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56070
%stackaddr$env-ref56071 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51208, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56071
%stackaddr$prim56072 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim56072, align 8
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%current_45args54981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %current_45args54981, %struct.ScmObj** %stackaddr$prim56073, align 8
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54981)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$makeclosure56075 = alloca %struct.ScmObj*, align 8
%fptrToInt56076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51224 to i64
%ae51224 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56076)
store volatile %struct.ScmObj* %ae51224, %struct.ScmObj** %stackaddr$makeclosure56075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51224, %struct.ScmObj* %k48307, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51224, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51224, %struct.ScmObj* %v48096, i64 2)
%stackaddr$makeclosure56077 = alloca %struct.ScmObj*, align 8
%fptrToInt56078 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51225 to i64
%ae51225 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56078)
store volatile %struct.ScmObj* %ae51225, %struct.ScmObj** %stackaddr$makeclosure56077, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %k48307, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51225, %struct.ScmObj* %v48096, i64 2)
%argslist54995$anf_45bind482340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist54995$anf_45bind482341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51225, %struct.ScmObj* %argslist54995$anf_45bind482340)
store volatile %struct.ScmObj* %argslist54995$anf_45bind482341, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$prim56080 = alloca %struct.ScmObj*, align 8
%argslist54995$anf_45bind482342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51224, %struct.ScmObj* %argslist54995$anf_45bind482341)
store volatile %struct.ScmObj* %argslist54995$anf_45bind482342, %struct.ScmObj** %stackaddr$prim56080, align 8
%clofunc56081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48234)
musttail call tailcc void %clofunc56081(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %argslist54995$anf_45bind482342)
ret void
}

define tailcc void @proc_clo$ae51224(%struct.ScmObj* %env$ae51224,%struct.ScmObj* %current_45args54983) {
%stackaddr$env-ref56082 = alloca %struct.ScmObj*, align 8
%k48307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51224, i64 0)
store %struct.ScmObj* %k48307, %struct.ScmObj** %stackaddr$env-ref56082
%stackaddr$env-ref56083 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51224, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56083
%stackaddr$env-ref56084 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51224, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56084
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54983)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%current_45args54984 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54983)
store volatile %struct.ScmObj* %current_45args54984, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54984)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim56087, align 8
%ae51333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51333)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim56089, align 8
%truthy$cmp56090 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48236)
%cmp$cmp56090 = icmp eq i64 %truthy$cmp56090, 1
br i1 %cmp$cmp56090, label %truebranch$cmp56090, label %falsebranch$cmp56090
truebranch$cmp56090:
%ae51337 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51338 = call %struct.ScmObj* @const_init_false()
%argslist54986$k483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%argslist54986$k483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51338, %struct.ScmObj* %argslist54986$k483070)
store volatile %struct.ScmObj* %argslist54986$k483071, %struct.ScmObj** %stackaddr$prim56091, align 8
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%argslist54986$k483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51337, %struct.ScmObj* %argslist54986$k483071)
store volatile %struct.ScmObj* %argslist54986$k483072, %struct.ScmObj** %stackaddr$prim56092, align 8
%clofunc56093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48307)
musttail call tailcc void %clofunc56093(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54986$k483072)
ret void
falsebranch$cmp56090:
%ae51346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51346)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim56096, align 8
%truthy$cmp56097 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48239)
%cmp$cmp56097 = icmp eq i64 %truthy$cmp56097, 1
br i1 %cmp$cmp56097, label %truebranch$cmp56097, label %falsebranch$cmp56097
truebranch$cmp56097:
%ae51352 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%cpsprim48310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51352)
store volatile %struct.ScmObj* %cpsprim48310, %struct.ScmObj** %stackaddr$prim56098, align 8
%ae51354 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54987$k483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%argslist54987$k483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48310, %struct.ScmObj* %argslist54987$k483070)
store volatile %struct.ScmObj* %argslist54987$k483071, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist54987$k483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51354, %struct.ScmObj* %argslist54987$k483071)
store volatile %struct.ScmObj* %argslist54987$k483072, %struct.ScmObj** %stackaddr$prim56100, align 8
%clofunc56101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48307)
musttail call tailcc void %clofunc56101(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54987$k483072)
ret void
falsebranch$cmp56097:
%ae51365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56102 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51365)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim56102, align 8
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56103, align 8
%ae51368 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51368, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim56104, align 8
%argslist54988$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%argslist54988$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54988$cc480980)
store volatile %struct.ScmObj* %argslist54988$cc480981, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%argslist54988$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54988$cc480981)
store volatile %struct.ScmObj* %argslist54988$cc480982, %struct.ScmObj** %stackaddr$prim56106, align 8
%clofunc56107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc56107(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54988$cc480982)
ret void
}

define tailcc void @proc_clo$ae51225(%struct.ScmObj* %env$ae51225,%struct.ScmObj* %current_45args54989) {
%stackaddr$env-ref56108 = alloca %struct.ScmObj*, align 8
%k48307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 0)
store %struct.ScmObj* %k48307, %struct.ScmObj** %stackaddr$env-ref56108
%stackaddr$env-ref56109 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56109
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51225, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54989)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim56111, align 8
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%current_45args54990 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54989)
store volatile %struct.ScmObj* %current_45args54990, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54990)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim56113, align 8
%ae51227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51227)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim56115, align 8
%truthy$cmp56116 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48236)
%cmp$cmp56116 = icmp eq i64 %truthy$cmp56116, 1
br i1 %cmp$cmp56116, label %truebranch$cmp56116, label %falsebranch$cmp56116
truebranch$cmp56116:
%ae51231 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51232 = call %struct.ScmObj* @const_init_false()
%argslist54992$k483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%argslist54992$k483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51232, %struct.ScmObj* %argslist54992$k483070)
store volatile %struct.ScmObj* %argslist54992$k483071, %struct.ScmObj** %stackaddr$prim56117, align 8
%stackaddr$prim56118 = alloca %struct.ScmObj*, align 8
%argslist54992$k483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51231, %struct.ScmObj* %argslist54992$k483071)
store volatile %struct.ScmObj* %argslist54992$k483072, %struct.ScmObj** %stackaddr$prim56118, align 8
%clofunc56119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48307)
musttail call tailcc void %clofunc56119(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54992$k483072)
ret void
falsebranch$cmp56116:
%ae51240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51240)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim56120, align 8
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim56121, align 8
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48238, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim56122, align 8
%truthy$cmp56123 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48239)
%cmp$cmp56123 = icmp eq i64 %truthy$cmp56123, 1
br i1 %cmp$cmp56123, label %truebranch$cmp56123, label %falsebranch$cmp56123
truebranch$cmp56123:
%ae51246 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%cpsprim48310 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51246)
store volatile %struct.ScmObj* %cpsprim48310, %struct.ScmObj** %stackaddr$prim56124, align 8
%ae51248 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54993$k483070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%argslist54993$k483071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48310, %struct.ScmObj* %argslist54993$k483070)
store volatile %struct.ScmObj* %argslist54993$k483071, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%argslist54993$k483072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51248, %struct.ScmObj* %argslist54993$k483071)
store volatile %struct.ScmObj* %argslist54993$k483072, %struct.ScmObj** %stackaddr$prim56126, align 8
%clofunc56127 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48307)
musttail call tailcc void %clofunc56127(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54993$k483072)
ret void
falsebranch$cmp56123:
%ae51259 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51259)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim56128, align 8
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56129, align 8
%ae51262 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51262, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim56130, align 8
%argslist54994$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%argslist54994$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54994$cc480980)
store volatile %struct.ScmObj* %argslist54994$cc480981, %struct.ScmObj** %stackaddr$prim56131, align 8
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%argslist54994$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48307, %struct.ScmObj* %argslist54994$cc480981)
store volatile %struct.ScmObj* %argslist54994$cc480982, %struct.ScmObj** %stackaddr$prim56132, align 8
%clofunc56133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc56133(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54994$cc480982)
ret void
}

define tailcc void @proc_clo$ae51210(%struct.ScmObj* %env$ae51210,%struct.ScmObj* %current_45args54996) {
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54996)
store volatile %struct.ScmObj* %k48311, %struct.ScmObj** %stackaddr$prim56134, align 8
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%current_45args54997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54996)
store volatile %struct.ScmObj* %current_45args54997, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54997)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim56136, align 8
%argslist54999$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%argslist54999$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54999$u480990)
store volatile %struct.ScmObj* %argslist54999$u480991, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%argslist54999$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54999$u480991)
store volatile %struct.ScmObj* %argslist54999$u480992, %struct.ScmObj** %stackaddr$prim56138, align 8
%clofunc56139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc56139(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54999$u480992)
ret void
}

define tailcc void @proc_clo$ae50669(%struct.ScmObj* %env$ae50669,%struct.ScmObj* %current_45args55002) {
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55002)
store volatile %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$prim56140, align 8
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%current_45args55003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55002)
store volatile %struct.ScmObj* %current_45args55003, %struct.ScmObj** %stackaddr$prim56141, align 8
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55003)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim56142, align 8
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%current_45args55004 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55003)
store volatile %struct.ScmObj* %current_45args55004, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55004)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim56144, align 8
%ae50670 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50670, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim56145, align 8
%ae50672 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50672, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim56146, align 8
%stackaddr$makeclosure56147 = alloca %struct.ScmObj*, align 8
%fptrToInt56148 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50674 to i64
%ae50674 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56148)
store volatile %struct.ScmObj* %ae50674, %struct.ScmObj** %stackaddr$makeclosure56147, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50674, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50674, %struct.ScmObj* %k48312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50674, %struct.ScmObj* %n48105, i64 2)
%ae50675 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56149 = alloca %struct.ScmObj*, align 8
%fptrToInt56150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50676 to i64
%ae50676 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56150)
store volatile %struct.ScmObj* %ae50676, %struct.ScmObj** %stackaddr$makeclosure56149, align 8
%argslist55024$ae506740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist55024$ae506741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50676, %struct.ScmObj* %argslist55024$ae506740)
store volatile %struct.ScmObj* %argslist55024$ae506741, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%argslist55024$ae506742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50675, %struct.ScmObj* %argslist55024$ae506741)
store volatile %struct.ScmObj* %argslist55024$ae506742, %struct.ScmObj** %stackaddr$prim56152, align 8
%clofunc56153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50674)
musttail call tailcc void %clofunc56153(%struct.ScmObj* %ae50674, %struct.ScmObj* %argslist55024$ae506742)
ret void
}

define tailcc void @proc_clo$ae50674(%struct.ScmObj* %env$ae50674,%struct.ScmObj* %current_45args55006) {
%stackaddr$env-ref56154 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50674, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56154
%stackaddr$env-ref56155 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50674, i64 1)
store %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$env-ref56155
%stackaddr$env-ref56156 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50674, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56156
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55006)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%current_45args55007 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55006)
store volatile %struct.ScmObj* %current_45args55007, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55007)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$makeclosure56160 = alloca %struct.ScmObj*, align 8
%fptrToInt56161 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50690 to i64
%ae50690 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56161)
store volatile %struct.ScmObj* %ae50690, %struct.ScmObj** %stackaddr$makeclosure56160, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50690, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50690, %struct.ScmObj* %k48312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50690, %struct.ScmObj* %n48105, i64 2)
%stackaddr$makeclosure56162 = alloca %struct.ScmObj*, align 8
%fptrToInt56163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50691 to i64
%ae50691 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56163)
store volatile %struct.ScmObj* %ae50691, %struct.ScmObj** %stackaddr$makeclosure56162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %lst48104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %k48312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50691, %struct.ScmObj* %n48105, i64 2)
%argslist55019$anf_45bind482270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%argslist55019$anf_45bind482271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50691, %struct.ScmObj* %argslist55019$anf_45bind482270)
store volatile %struct.ScmObj* %argslist55019$anf_45bind482271, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%argslist55019$anf_45bind482272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50690, %struct.ScmObj* %argslist55019$anf_45bind482271)
store volatile %struct.ScmObj* %argslist55019$anf_45bind482272, %struct.ScmObj** %stackaddr$prim56165, align 8
%clofunc56166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48227)
musttail call tailcc void %clofunc56166(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist55019$anf_45bind482272)
ret void
}

define tailcc void @proc_clo$ae50690(%struct.ScmObj* %env$ae50690,%struct.ScmObj* %current_45args55009) {
%stackaddr$env-ref56167 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50690, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56167
%stackaddr$env-ref56168 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50690, i64 1)
store %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$env-ref56168
%stackaddr$env-ref56169 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50690, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56169
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55009)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%current_45args55010 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55009)
store volatile %struct.ScmObj* %current_45args55010, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55010)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim56172, align 8
%ae50833 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50833)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim56173, align 8
%ae50834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50834, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim56174, align 8
%truthy$cmp56175 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp56175 = icmp eq i64 %truthy$cmp56175, 1
br i1 %cmp$cmp56175, label %truebranch$cmp56175, label %falsebranch$cmp56175
truebranch$cmp56175:
%ae50838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%cpsprim48315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50838)
store volatile %struct.ScmObj* %cpsprim48315, %struct.ScmObj** %stackaddr$prim56176, align 8
%ae50840 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55012$k483120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%argslist55012$k483121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48315, %struct.ScmObj* %argslist55012$k483120)
store volatile %struct.ScmObj* %argslist55012$k483121, %struct.ScmObj** %stackaddr$prim56177, align 8
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%argslist55012$k483122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50840, %struct.ScmObj* %argslist55012$k483121)
store volatile %struct.ScmObj* %argslist55012$k483122, %struct.ScmObj** %stackaddr$prim56178, align 8
%clofunc56179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48312)
musttail call tailcc void %clofunc56179(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist55012$k483122)
ret void
falsebranch$cmp56175:
%ae50851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50851)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim56180, align 8
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim56181, align 8
%ae50854 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50854, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim56182, align 8
%ae50857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50857)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim56183, align 8
%ae50859 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56184 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %ae50859)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim56184, align 8
%ae50861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50861, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim56185, align 8
%argslist55013$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%argslist55013$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist55013$cc481060)
store volatile %struct.ScmObj* %argslist55013$cc481061, %struct.ScmObj** %stackaddr$prim56186, align 8
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%argslist55013$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist55013$cc481061)
store volatile %struct.ScmObj* %argslist55013$cc481062, %struct.ScmObj** %stackaddr$prim56187, align 8
%clofunc56188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc56188(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist55013$cc481062)
ret void
}

define tailcc void @proc_clo$ae50691(%struct.ScmObj* %env$ae50691,%struct.ScmObj* %current_45args55014) {
%stackaddr$env-ref56189 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 0)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56189
%stackaddr$env-ref56190 = alloca %struct.ScmObj*, align 8
%k48312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 1)
store %struct.ScmObj* %k48312, %struct.ScmObj** %stackaddr$env-ref56190
%stackaddr$env-ref56191 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50691, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56191
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55014)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim56192, align 8
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%current_45args55015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55014)
store volatile %struct.ScmObj* %current_45args55015, %struct.ScmObj** %stackaddr$prim56193, align 8
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55015)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim56194, align 8
%ae50693 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50693)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim56195, align 8
%ae50694 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56196 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50694, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim56196, align 8
%truthy$cmp56197 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48229)
%cmp$cmp56197 = icmp eq i64 %truthy$cmp56197, 1
br i1 %cmp$cmp56197, label %truebranch$cmp56197, label %falsebranch$cmp56197
truebranch$cmp56197:
%ae50698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%cpsprim48315 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50698)
store volatile %struct.ScmObj* %cpsprim48315, %struct.ScmObj** %stackaddr$prim56198, align 8
%ae50700 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55017$k483120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%argslist55017$k483121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48315, %struct.ScmObj* %argslist55017$k483120)
store volatile %struct.ScmObj* %argslist55017$k483121, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist55017$k483122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50700, %struct.ScmObj* %argslist55017$k483121)
store volatile %struct.ScmObj* %argslist55017$k483122, %struct.ScmObj** %stackaddr$prim56200, align 8
%clofunc56201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48312)
musttail call tailcc void %clofunc56201(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist55017$k483122)
ret void
falsebranch$cmp56197:
%ae50711 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50711)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim56202, align 8
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim56203, align 8
%ae50714 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50714, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim56204, align 8
%ae50717 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50717)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim56205, align 8
%ae50719 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %ae50719)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim56206, align 8
%ae50721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50721, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim56207, align 8
%argslist55018$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%argslist55018$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist55018$cc481060)
store volatile %struct.ScmObj* %argslist55018$cc481061, %struct.ScmObj** %stackaddr$prim56208, align 8
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%argslist55018$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48312, %struct.ScmObj* %argslist55018$cc481061)
store volatile %struct.ScmObj* %argslist55018$cc481062, %struct.ScmObj** %stackaddr$prim56209, align 8
%clofunc56210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc56210(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist55018$cc481062)
ret void
}

define tailcc void @proc_clo$ae50676(%struct.ScmObj* %env$ae50676,%struct.ScmObj* %current_45args55020) {
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55020)
store volatile %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%current_45args55021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55020)
store volatile %struct.ScmObj* %current_45args55021, %struct.ScmObj** %stackaddr$prim56212, align 8
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55021)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim56213, align 8
%argslist55023$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist55023$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist55023$u481070)
store volatile %struct.ScmObj* %argslist55023$u481071, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%argslist55023$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist55023$u481071)
store volatile %struct.ScmObj* %argslist55023$u481072, %struct.ScmObj** %stackaddr$prim56215, align 8
%clofunc56216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc56216(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist55023$u481072)
ret void
}

define tailcc void @proc_clo$ae50253(%struct.ScmObj* %env$ae50253,%struct.ScmObj* %current_45args55026) {
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55026)
store volatile %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$prim56217, align 8
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%current_45args55027 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55026)
store volatile %struct.ScmObj* %current_45args55027, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55027)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim56219, align 8
%ae50254 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50254, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$makeclosure56221 = alloca %struct.ScmObj*, align 8
%fptrToInt56222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50256 to i64
%ae50256 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56222)
store volatile %struct.ScmObj* %ae50256, %struct.ScmObj** %stackaddr$makeclosure56221, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50256, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50256, %struct.ScmObj* %k48317, i64 1)
%ae50257 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56223 = alloca %struct.ScmObj*, align 8
%fptrToInt56224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50258 to i64
%ae50258 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56224)
store volatile %struct.ScmObj* %ae50258, %struct.ScmObj** %stackaddr$makeclosure56223, align 8
%argslist55049$ae502560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%argslist55049$ae502561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50258, %struct.ScmObj* %argslist55049$ae502560)
store volatile %struct.ScmObj* %argslist55049$ae502561, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%argslist55049$ae502562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50257, %struct.ScmObj* %argslist55049$ae502561)
store volatile %struct.ScmObj* %argslist55049$ae502562, %struct.ScmObj** %stackaddr$prim56226, align 8
%clofunc56227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50256)
musttail call tailcc void %clofunc56227(%struct.ScmObj* %ae50256, %struct.ScmObj* %argslist55049$ae502562)
ret void
}

define tailcc void @proc_clo$ae50256(%struct.ScmObj* %env$ae50256,%struct.ScmObj* %current_45args55029) {
%stackaddr$env-ref56228 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50256, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56228
%stackaddr$env-ref56229 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50256, i64 1)
store %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$env-ref56229
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55029)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim56230, align 8
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%current_45args55030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55029)
store volatile %struct.ScmObj* %current_45args55030, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55030)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim56232, align 8
%stackaddr$makeclosure56233 = alloca %struct.ScmObj*, align 8
%fptrToInt56234 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50275 to i64
%ae50275 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56234)
store volatile %struct.ScmObj* %ae50275, %struct.ScmObj** %stackaddr$makeclosure56233, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50275, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50275, %struct.ScmObj* %k48317, i64 1)
%stackaddr$makeclosure56235 = alloca %struct.ScmObj*, align 8
%fptrToInt56236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50276 to i64
%ae50276 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56236)
store volatile %struct.ScmObj* %ae50276, %struct.ScmObj** %stackaddr$makeclosure56235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50276, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50276, %struct.ScmObj* %k48317, i64 1)
%argslist55044$anf_45bind482190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%argslist55044$anf_45bind482191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50276, %struct.ScmObj* %argslist55044$anf_45bind482190)
store volatile %struct.ScmObj* %argslist55044$anf_45bind482191, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%argslist55044$anf_45bind482192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist55044$anf_45bind482191)
store volatile %struct.ScmObj* %argslist55044$anf_45bind482192, %struct.ScmObj** %stackaddr$prim56238, align 8
%clofunc56239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48219)
musttail call tailcc void %clofunc56239(%struct.ScmObj* %anf_45bind48219, %struct.ScmObj* %argslist55044$anf_45bind482192)
ret void
}

define tailcc void @proc_clo$ae50275(%struct.ScmObj* %env$ae50275,%struct.ScmObj* %current_45args55032) {
%stackaddr$env-ref56240 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50275, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56240
%stackaddr$env-ref56241 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50275, i64 1)
store %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$env-ref56241
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55032)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim56242, align 8
%stackaddr$prim56243 = alloca %struct.ScmObj*, align 8
%current_45args55033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55032)
store volatile %struct.ScmObj* %current_45args55033, %struct.ScmObj** %stackaddr$prim56243, align 8
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55033)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim56244, align 8
%ae50391 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50391)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim56245, align 8
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim56246, align 8
%truthy$cmp56247 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48221)
%cmp$cmp56247 = icmp eq i64 %truthy$cmp56247, 1
br i1 %cmp$cmp56247, label %truebranch$cmp56247, label %falsebranch$cmp56247
truebranch$cmp56247:
%ae50395 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50396 = call %struct.ScmObj* @const_init_true()
%argslist55035$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%argslist55035$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50396, %struct.ScmObj* %argslist55035$k483170)
store volatile %struct.ScmObj* %argslist55035$k483171, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%argslist55035$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50395, %struct.ScmObj* %argslist55035$k483171)
store volatile %struct.ScmObj* %argslist55035$k483172, %struct.ScmObj** %stackaddr$prim56249, align 8
%clofunc56250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc56250(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55035$k483172)
ret void
falsebranch$cmp56247:
%ae50404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50404)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim56252, align 8
%truthy$cmp56253 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48223)
%cmp$cmp56253 = icmp eq i64 %truthy$cmp56253, 1
br i1 %cmp$cmp56253, label %truebranch$cmp56253, label %falsebranch$cmp56253
truebranch$cmp56253:
%ae50408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50408)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim56254, align 8
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim56255, align 8
%ae50411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50411)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim56256, align 8
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim56257, align 8
%ae50414 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56258 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50414, %struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim56258, align 8
%argslist55036$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56259 = alloca %struct.ScmObj*, align 8
%argslist55036$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist55036$cc481130)
store volatile %struct.ScmObj* %argslist55036$cc481131, %struct.ScmObj** %stackaddr$prim56259, align 8
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%argslist55036$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55036$cc481131)
store volatile %struct.ScmObj* %argslist55036$cc481132, %struct.ScmObj** %stackaddr$prim56260, align 8
%clofunc56261 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc56261(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist55036$cc481132)
ret void
falsebranch$cmp56253:
%ae50447 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50448 = call %struct.ScmObj* @const_init_false()
%argslist55037$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%argslist55037$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50448, %struct.ScmObj* %argslist55037$k483170)
store volatile %struct.ScmObj* %argslist55037$k483171, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%argslist55037$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50447, %struct.ScmObj* %argslist55037$k483171)
store volatile %struct.ScmObj* %argslist55037$k483172, %struct.ScmObj** %stackaddr$prim56263, align 8
%clofunc56264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc56264(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55037$k483172)
ret void
}

define tailcc void @proc_clo$ae50276(%struct.ScmObj* %env$ae50276,%struct.ScmObj* %current_45args55038) {
%stackaddr$env-ref56265 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50276, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56265
%stackaddr$env-ref56266 = alloca %struct.ScmObj*, align 8
%k48317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50276, i64 1)
store %struct.ScmObj* %k48317, %struct.ScmObj** %stackaddr$env-ref56266
%stackaddr$prim56267 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55038)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim56267, align 8
%stackaddr$prim56268 = alloca %struct.ScmObj*, align 8
%current_45args55039 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55038)
store volatile %struct.ScmObj* %current_45args55039, %struct.ScmObj** %stackaddr$prim56268, align 8
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55039)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim56269, align 8
%ae50278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50278)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim56271, align 8
%truthy$cmp56272 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48221)
%cmp$cmp56272 = icmp eq i64 %truthy$cmp56272, 1
br i1 %cmp$cmp56272, label %truebranch$cmp56272, label %falsebranch$cmp56272
truebranch$cmp56272:
%ae50282 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50283 = call %struct.ScmObj* @const_init_true()
%argslist55041$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56273 = alloca %struct.ScmObj*, align 8
%argslist55041$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50283, %struct.ScmObj* %argslist55041$k483170)
store volatile %struct.ScmObj* %argslist55041$k483171, %struct.ScmObj** %stackaddr$prim56273, align 8
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%argslist55041$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50282, %struct.ScmObj* %argslist55041$k483171)
store volatile %struct.ScmObj* %argslist55041$k483172, %struct.ScmObj** %stackaddr$prim56274, align 8
%clofunc56275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc56275(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55041$k483172)
ret void
falsebranch$cmp56272:
%ae50291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50291)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim56277, align 8
%truthy$cmp56278 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48223)
%cmp$cmp56278 = icmp eq i64 %truthy$cmp56278, 1
br i1 %cmp$cmp56278, label %truebranch$cmp56278, label %falsebranch$cmp56278
truebranch$cmp56278:
%ae50295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50295)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim56279, align 8
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48224)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim56280, align 8
%ae50298 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50298)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim56281, align 8
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim56282, align 8
%ae50301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50301, %struct.ScmObj* %anf_45bind48226)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim56283, align 8
%argslist55042$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%argslist55042$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist55042$cc481130)
store volatile %struct.ScmObj* %argslist55042$cc481131, %struct.ScmObj** %stackaddr$prim56284, align 8
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%argslist55042$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55042$cc481131)
store volatile %struct.ScmObj* %argslist55042$cc481132, %struct.ScmObj** %stackaddr$prim56285, align 8
%clofunc56286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc56286(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist55042$cc481132)
ret void
falsebranch$cmp56278:
%ae50334 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50335 = call %struct.ScmObj* @const_init_false()
%argslist55043$k483170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%argslist55043$k483171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50335, %struct.ScmObj* %argslist55043$k483170)
store volatile %struct.ScmObj* %argslist55043$k483171, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%argslist55043$k483172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50334, %struct.ScmObj* %argslist55043$k483171)
store volatile %struct.ScmObj* %argslist55043$k483172, %struct.ScmObj** %stackaddr$prim56288, align 8
%clofunc56289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48317)
musttail call tailcc void %clofunc56289(%struct.ScmObj* %k48317, %struct.ScmObj* %argslist55043$k483172)
ret void
}

define tailcc void @proc_clo$ae50258(%struct.ScmObj* %env$ae50258,%struct.ScmObj* %current_45args55045) {
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55045)
store volatile %struct.ScmObj* %k48320, %struct.ScmObj** %stackaddr$prim56290, align 8
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%current_45args55046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55045)
store volatile %struct.ScmObj* %current_45args55046, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55046)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim56292, align 8
%ae50260 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55048$k483200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%argslist55048$k483201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist55048$k483200)
store volatile %struct.ScmObj* %argslist55048$k483201, %struct.ScmObj** %stackaddr$prim56293, align 8
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%argslist55048$k483202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50260, %struct.ScmObj* %argslist55048$k483201)
store volatile %struct.ScmObj* %argslist55048$k483202, %struct.ScmObj** %stackaddr$prim56294, align 8
%clofunc56295 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48320)
musttail call tailcc void %clofunc56295(%struct.ScmObj* %k48320, %struct.ScmObj* %argslist55048$k483202)
ret void
}

define tailcc void @proc_clo$ae50181(%struct.ScmObj* %env$ae50181,%struct.ScmObj* %current_45args55051) {
%stackaddr$env-ref56296 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50181, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref56296
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55051)
store volatile %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$prim56297, align 8
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%current_45args55052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55051)
store volatile %struct.ScmObj* %current_45args55052, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55052)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim56299, align 8
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%current_45args55053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55052)
store volatile %struct.ScmObj* %current_45args55053, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55053)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim56301, align 8
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim56302, align 8
%truthy$cmp56303 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48213)
%cmp$cmp56303 = icmp eq i64 %truthy$cmp56303, 1
br i1 %cmp$cmp56303, label %truebranch$cmp56303, label %falsebranch$cmp56303
truebranch$cmp56303:
%ae50185 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55055$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%argslist55055$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist55055$k483210)
store volatile %struct.ScmObj* %argslist55055$k483211, %struct.ScmObj** %stackaddr$prim56304, align 8
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%argslist55055$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50185, %struct.ScmObj* %argslist55055$k483211)
store volatile %struct.ScmObj* %argslist55055$k483212, %struct.ScmObj** %stackaddr$prim56305, align 8
%clofunc56306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc56306(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist55055$k483212)
ret void
falsebranch$cmp56303:
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim56307, align 8
%ae50192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50192)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$makeclosure56310 = alloca %struct.ScmObj*, align 8
%fptrToInt56311 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50195 to i64
%ae50195 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56311)
store volatile %struct.ScmObj* %ae50195, %struct.ScmObj** %stackaddr$makeclosure56310, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50195, %struct.ScmObj* %anf_45bind48214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50195, %struct.ScmObj* %k48321, i64 1)
%argslist55060$anf_45bind482150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%argslist55060$anf_45bind482151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist55060$anf_45bind482150)
store volatile %struct.ScmObj* %argslist55060$anf_45bind482151, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%argslist55060$anf_45bind482152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist55060$anf_45bind482151)
store volatile %struct.ScmObj* %argslist55060$anf_45bind482152, %struct.ScmObj** %stackaddr$prim56313, align 8
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%argslist55060$anf_45bind482153 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist55060$anf_45bind482152)
store volatile %struct.ScmObj* %argslist55060$anf_45bind482153, %struct.ScmObj** %stackaddr$prim56314, align 8
%clofunc56315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48215)
musttail call tailcc void %clofunc56315(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %argslist55060$anf_45bind482153)
ret void
}

define tailcc void @proc_clo$ae50195(%struct.ScmObj* %env$ae50195,%struct.ScmObj* %current_45args55056) {
%stackaddr$env-ref56316 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50195, i64 0)
store %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$env-ref56316
%stackaddr$env-ref56317 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50195, i64 1)
store %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$env-ref56317
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55056)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim56318, align 8
%stackaddr$prim56319 = alloca %struct.ScmObj*, align 8
%current_45args55057 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55056)
store volatile %struct.ScmObj* %current_45args55057, %struct.ScmObj** %stackaddr$prim56319, align 8
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55057)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%cpsprim48323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsprim48323, %struct.ScmObj** %stackaddr$prim56321, align 8
%ae50201 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55059$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%argslist55059$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48323, %struct.ScmObj* %argslist55059$k483210)
store volatile %struct.ScmObj* %argslist55059$k483211, %struct.ScmObj** %stackaddr$prim56322, align 8
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%argslist55059$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50201, %struct.ScmObj* %argslist55059$k483211)
store volatile %struct.ScmObj* %argslist55059$k483212, %struct.ScmObj** %stackaddr$prim56323, align 8
%clofunc56324 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc56324(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist55059$k483212)
ret void
}

define tailcc void @proc_clo$ae50155(%struct.ScmObj* %env$ae50155,%struct.ScmObj* %current_45args55062) {
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55062)
store volatile %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%current_45args55063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55062)
store volatile %struct.ScmObj* %current_45args55063, %struct.ScmObj** %stackaddr$prim56326, align 8
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55063)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%current_45args55064 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55063)
store volatile %struct.ScmObj* %current_45args55064, %struct.ScmObj** %stackaddr$prim56328, align 8
%stackaddr$prim56329 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55064)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim56329, align 8
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%cpsprim48325 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48212)
store volatile %struct.ScmObj* %cpsprim48325, %struct.ScmObj** %stackaddr$prim56331, align 8
%ae50160 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55066$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%argslist55066$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48325, %struct.ScmObj* %argslist55066$k483240)
store volatile %struct.ScmObj* %argslist55066$k483241, %struct.ScmObj** %stackaddr$prim56332, align 8
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%argslist55066$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50160, %struct.ScmObj* %argslist55066$k483241)
store volatile %struct.ScmObj* %argslist55066$k483242, %struct.ScmObj** %stackaddr$prim56333, align 8
%clofunc56334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc56334(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist55066$k483242)
ret void
}

define tailcc void @proc_clo$ae50131(%struct.ScmObj* %env$ae50131,%struct.ScmObj* %current_45args55068) {
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %k48326, %struct.ScmObj** %stackaddr$prim56335, align 8
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%current_45args55069 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %current_45args55069, %struct.ScmObj** %stackaddr$prim56336, align 8
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55069)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim56337, align 8
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%current_45args55070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55069)
store volatile %struct.ScmObj* %current_45args55070, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55070)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim56339, align 8
%stackaddr$prim56340 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim56340, align 8
%stackaddr$prim56341 = alloca %struct.ScmObj*, align 8
%cpsprim48327 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48211)
store volatile %struct.ScmObj* %cpsprim48327, %struct.ScmObj** %stackaddr$prim56341, align 8
%ae50136 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55072$k483260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%argslist55072$k483261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48327, %struct.ScmObj* %argslist55072$k483260)
store volatile %struct.ScmObj* %argslist55072$k483261, %struct.ScmObj** %stackaddr$prim56342, align 8
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%argslist55072$k483262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50136, %struct.ScmObj* %argslist55072$k483261)
store volatile %struct.ScmObj* %argslist55072$k483262, %struct.ScmObj** %stackaddr$prim56343, align 8
%clofunc56344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48326)
musttail call tailcc void %clofunc56344(%struct.ScmObj* %k48326, %struct.ScmObj* %argslist55072$k483262)
ret void
}

define tailcc void @proc_clo$ae49737(%struct.ScmObj* %env$ae49737,%struct.ScmObj* %current_45args55075) {
%stackaddr$env-ref56345 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56345
%stackaddr$env-ref56346 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56346
%stackaddr$env-ref56347 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49737, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56347
%stackaddr$prim56348 = alloca %struct.ScmObj*, align 8
%k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55075)
store volatile %struct.ScmObj* %k48328, %struct.ScmObj** %stackaddr$prim56348, align 8
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%current_45args55076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55075)
store volatile %struct.ScmObj* %current_45args55076, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55076)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim56350, align 8
%ae49739 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56351 = alloca %struct.ScmObj*, align 8
%fptrToInt56352 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49740 to i64
%ae49740 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56352)
store volatile %struct.ScmObj* %ae49740, %struct.ScmObj** %stackaddr$makeclosure56351, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49740, %struct.ScmObj* %_37map148077, i64 3)
%argslist55133$k483280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%argslist55133$k483281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49740, %struct.ScmObj* %argslist55133$k483280)
store volatile %struct.ScmObj* %argslist55133$k483281, %struct.ScmObj** %stackaddr$prim56353, align 8
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%argslist55133$k483282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49739, %struct.ScmObj* %argslist55133$k483281)
store volatile %struct.ScmObj* %argslist55133$k483282, %struct.ScmObj** %stackaddr$prim56354, align 8
%clofunc56355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48328)
musttail call tailcc void %clofunc56355(%struct.ScmObj* %k48328, %struct.ScmObj* %argslist55133$k483282)
ret void
}

define tailcc void @proc_clo$ae49740(%struct.ScmObj* %env$ae49740,%struct.ScmObj* %args4813048329) {
%stackaddr$env-ref56356 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56356
%stackaddr$env-ref56357 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56357
%stackaddr$env-ref56358 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56358
%stackaddr$env-ref56359 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49740, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56359
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048329)
store volatile %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$prim56360, align 8
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048329)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim56361, align 8
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim56363, align 8
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim56364, align 8
%stackaddr$prim56365 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim56365, align 8
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim56366, align 8
%stackaddr$makeclosure56367 = alloca %struct.ScmObj*, align 8
%fptrToInt56368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49748 to i64
%ae49748 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56368)
store volatile %struct.ScmObj* %ae49748, %struct.ScmObj** %stackaddr$makeclosure56367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49748, %struct.ScmObj* %k48330, i64 7)
%ae49749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56369 = alloca %struct.ScmObj*, align 8
%fptrToInt56370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49750 to i64
%ae49750 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56370)
store volatile %struct.ScmObj* %ae49750, %struct.ScmObj** %stackaddr$makeclosure56369, align 8
%argslist55132$ae497480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%argslist55132$ae497481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49750, %struct.ScmObj* %argslist55132$ae497480)
store volatile %struct.ScmObj* %argslist55132$ae497481, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%argslist55132$ae497482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49749, %struct.ScmObj* %argslist55132$ae497481)
store volatile %struct.ScmObj* %argslist55132$ae497482, %struct.ScmObj** %stackaddr$prim56372, align 8
%clofunc56373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49748)
musttail call tailcc void %clofunc56373(%struct.ScmObj* %ae49748, %struct.ScmObj* %argslist55132$ae497482)
ret void
}

define tailcc void @proc_clo$ae49748(%struct.ScmObj* %env$ae49748,%struct.ScmObj* %current_45args55078) {
%stackaddr$env-ref56374 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56374
%stackaddr$env-ref56375 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56375
%stackaddr$env-ref56376 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56376
%stackaddr$env-ref56377 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56377
%stackaddr$env-ref56378 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56378
%stackaddr$env-ref56379 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56379
%stackaddr$env-ref56380 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56380
%stackaddr$env-ref56381 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49748, i64 7)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56381
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55078)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%current_45args55079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55078)
store volatile %struct.ScmObj* %current_45args55079, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55079)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim56384, align 8
%stackaddr$makeclosure56385 = alloca %struct.ScmObj*, align 8
%fptrToInt56386 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49780 to i64
%ae49780 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56386)
store volatile %struct.ScmObj* %ae49780, %struct.ScmObj** %stackaddr$makeclosure56385, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49780, %struct.ScmObj* %k48330, i64 6)
%ae49782 = call %struct.ScmObj* @const_init_false()
%argslist55125$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%argslist55125$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55125$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55125$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56387, align 8
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%argslist55125$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49782, %struct.ScmObj* %argslist55125$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55125$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56388, align 8
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%argslist55125$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist55125$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55125$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56389, align 8
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist55125$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49780, %struct.ScmObj* %argslist55125$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55125$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56390, align 8
%clofunc56391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56391(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55125$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49780(%struct.ScmObj* %env$ae49780,%struct.ScmObj* %current_45args55081) {
%stackaddr$env-ref56392 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56392
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$env-ref56394 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56394
%stackaddr$env-ref56395 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56395
%stackaddr$env-ref56396 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56396
%stackaddr$env-ref56397 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56397
%stackaddr$env-ref56398 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49780, i64 6)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56398
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55081)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%current_45args55082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55081)
store volatile %struct.ScmObj* %current_45args55082, %struct.ScmObj** %stackaddr$prim56400, align 8
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55082)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim56401, align 8
%truthy$cmp56402 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48202)
%cmp$cmp56402 = icmp eq i64 %truthy$cmp56402, 1
br i1 %cmp$cmp56402, label %truebranch$cmp56402, label %falsebranch$cmp56402
truebranch$cmp56402:
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55084$k483300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%argslist55084$k483301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist55084$k483300)
store volatile %struct.ScmObj* %argslist55084$k483301, %struct.ScmObj** %stackaddr$prim56403, align 8
%stackaddr$prim56404 = alloca %struct.ScmObj*, align 8
%argslist55084$k483302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49791, %struct.ScmObj* %argslist55084$k483301)
store volatile %struct.ScmObj* %argslist55084$k483302, %struct.ScmObj** %stackaddr$prim56404, align 8
%clofunc56405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48330)
musttail call tailcc void %clofunc56405(%struct.ScmObj* %k48330, %struct.ScmObj* %argslist55084$k483302)
ret void
falsebranch$cmp56402:
%stackaddr$makeclosure56406 = alloca %struct.ScmObj*, align 8
%fptrToInt56407 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49796 to i64
%ae49796 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56407)
store volatile %struct.ScmObj* %ae49796, %struct.ScmObj** %stackaddr$makeclosure56406, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49796, %struct.ScmObj* %k48330, i64 6)
%ae49797 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56408 = alloca %struct.ScmObj*, align 8
%fptrToInt56409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49798 to i64
%ae49798 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56409)
store volatile %struct.ScmObj* %ae49798, %struct.ScmObj** %stackaddr$makeclosure56408, align 8
%argslist55124$ae497960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%argslist55124$ae497961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist55124$ae497960)
store volatile %struct.ScmObj* %argslist55124$ae497961, %struct.ScmObj** %stackaddr$prim56410, align 8
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%argslist55124$ae497962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist55124$ae497961)
store volatile %struct.ScmObj* %argslist55124$ae497962, %struct.ScmObj** %stackaddr$prim56411, align 8
%clofunc56412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49796)
musttail call tailcc void %clofunc56412(%struct.ScmObj* %ae49796, %struct.ScmObj* %argslist55124$ae497962)
ret void
}

define tailcc void @proc_clo$ae49796(%struct.ScmObj* %env$ae49796,%struct.ScmObj* %current_45args55085) {
%stackaddr$env-ref56413 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56413
%stackaddr$env-ref56414 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56414
%stackaddr$env-ref56415 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56415
%stackaddr$env-ref56416 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56416
%stackaddr$env-ref56417 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56417
%stackaddr$env-ref56418 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56418
%stackaddr$env-ref56419 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49796, i64 6)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56419
%stackaddr$prim56420 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55085)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim56420, align 8
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%current_45args55086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55085)
store volatile %struct.ScmObj* %current_45args55086, %struct.ScmObj** %stackaddr$prim56421, align 8
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55086)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim56422, align 8
%stackaddr$makeclosure56423 = alloca %struct.ScmObj*, align 8
%fptrToInt56424 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49817 to i64
%ae49817 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56424)
store volatile %struct.ScmObj* %ae49817, %struct.ScmObj** %stackaddr$makeclosure56423, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49817, %struct.ScmObj* %k48330, i64 6)
%argslist55119$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%argslist55119$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55119$_37map1480770)
store volatile %struct.ScmObj* %argslist55119$_37map1480771, %struct.ScmObj** %stackaddr$prim56425, align 8
%stackaddr$prim56426 = alloca %struct.ScmObj*, align 8
%argslist55119$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist55119$_37map1480771)
store volatile %struct.ScmObj* %argslist55119$_37map1480772, %struct.ScmObj** %stackaddr$prim56426, align 8
%stackaddr$prim56427 = alloca %struct.ScmObj*, align 8
%argslist55119$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49817, %struct.ScmObj* %argslist55119$_37map1480772)
store volatile %struct.ScmObj* %argslist55119$_37map1480773, %struct.ScmObj** %stackaddr$prim56427, align 8
%clofunc56428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc56428(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist55119$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49817(%struct.ScmObj* %env$ae49817,%struct.ScmObj* %current_45args55088) {
%stackaddr$env-ref56429 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56429
%stackaddr$env-ref56430 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56430
%stackaddr$env-ref56431 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56431
%stackaddr$env-ref56432 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56432
%stackaddr$env-ref56433 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56433
%stackaddr$env-ref56434 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56434
%stackaddr$env-ref56435 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49817, i64 6)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56435
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55088)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim56436, align 8
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%current_45args55089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55088)
store volatile %struct.ScmObj* %current_45args55089, %struct.ScmObj** %stackaddr$prim56437, align 8
%stackaddr$prim56438 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55089)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim56438, align 8
%stackaddr$makeclosure56439 = alloca %struct.ScmObj*, align 8
%fptrToInt56440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49820 to i64
%ae49820 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56440)
store volatile %struct.ScmObj* %ae49820, %struct.ScmObj** %stackaddr$makeclosure56439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %lsts_4348138, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %k48330, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %f48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %acc48132, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37foldl48129, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49820, %struct.ScmObj* %_37map148077, i64 7)
%ae49821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56441 = alloca %struct.ScmObj*, align 8
%fptrToInt56442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49822 to i64
%ae49822 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56442)
store volatile %struct.ScmObj* %ae49822, %struct.ScmObj** %stackaddr$makeclosure56441, align 8
%argslist55118$ae498200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56443 = alloca %struct.ScmObj*, align 8
%argslist55118$ae498201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49822, %struct.ScmObj* %argslist55118$ae498200)
store volatile %struct.ScmObj* %argslist55118$ae498201, %struct.ScmObj** %stackaddr$prim56443, align 8
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%argslist55118$ae498202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49821, %struct.ScmObj* %argslist55118$ae498201)
store volatile %struct.ScmObj* %argslist55118$ae498202, %struct.ScmObj** %stackaddr$prim56444, align 8
%clofunc56445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49820)
musttail call tailcc void %clofunc56445(%struct.ScmObj* %ae49820, %struct.ScmObj* %argslist55118$ae498202)
ret void
}

define tailcc void @proc_clo$ae49820(%struct.ScmObj* %env$ae49820,%struct.ScmObj* %current_45args55091) {
%stackaddr$env-ref56446 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56446
%stackaddr$env-ref56447 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56447
%stackaddr$env-ref56448 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 2)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56448
%stackaddr$env-ref56449 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 3)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56449
%stackaddr$env-ref56450 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 4)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56450
%stackaddr$env-ref56451 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 5)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56451
%stackaddr$env-ref56452 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 6)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56452
%stackaddr$env-ref56453 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49820, i64 7)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56453
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55091)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim56454, align 8
%stackaddr$prim56455 = alloca %struct.ScmObj*, align 8
%current_45args55092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55091)
store volatile %struct.ScmObj* %current_45args55092, %struct.ScmObj** %stackaddr$prim56455, align 8
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55092)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim56456, align 8
%stackaddr$makeclosure56457 = alloca %struct.ScmObj*, align 8
%fptrToInt56458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49841 to i64
%ae49841 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56458)
store volatile %struct.ScmObj* %ae49841, %struct.ScmObj** %stackaddr$makeclosure56457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %lsts_4348138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %k48330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49841, %struct.ScmObj* %_37foldl48129, i64 5)
%argslist55113$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56459 = alloca %struct.ScmObj*, align 8
%argslist55113$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55113$_37map1480770)
store volatile %struct.ScmObj* %argslist55113$_37map1480771, %struct.ScmObj** %stackaddr$prim56459, align 8
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%argslist55113$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist55113$_37map1480771)
store volatile %struct.ScmObj* %argslist55113$_37map1480772, %struct.ScmObj** %stackaddr$prim56460, align 8
%stackaddr$prim56461 = alloca %struct.ScmObj*, align 8
%argslist55113$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49841, %struct.ScmObj* %argslist55113$_37map1480772)
store volatile %struct.ScmObj* %argslist55113$_37map1480773, %struct.ScmObj** %stackaddr$prim56461, align 8
%clofunc56462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc56462(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist55113$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49841(%struct.ScmObj* %env$ae49841,%struct.ScmObj* %current_45args55094) {
%stackaddr$env-ref56463 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 0)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56463
%stackaddr$env-ref56464 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 1)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56464
%stackaddr$env-ref56465 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56465
%stackaddr$env-ref56466 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56466
%stackaddr$env-ref56467 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56467
%stackaddr$env-ref56468 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49841, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56468
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55094)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%current_45args55095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55094)
store volatile %struct.ScmObj* %current_45args55095, %struct.ScmObj** %stackaddr$prim56470, align 8
%stackaddr$prim56471 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55095)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim56471, align 8
%stackaddr$makeclosure56472 = alloca %struct.ScmObj*, align 8
%fptrToInt56473 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49844 to i64
%ae49844 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56473)
store volatile %struct.ScmObj* %ae49844, %struct.ScmObj** %stackaddr$makeclosure56472, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %lsts_4348138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %k48330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %vs48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37foldr48051, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37foldl48129, i64 6)
%ae49845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56474 = alloca %struct.ScmObj*, align 8
%fptrToInt56475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49846 to i64
%ae49846 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56475)
store volatile %struct.ScmObj* %ae49846, %struct.ScmObj** %stackaddr$makeclosure56474, align 8
%argslist55112$ae498440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%argslist55112$ae498441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49846, %struct.ScmObj* %argslist55112$ae498440)
store volatile %struct.ScmObj* %argslist55112$ae498441, %struct.ScmObj** %stackaddr$prim56476, align 8
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%argslist55112$ae498442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist55112$ae498441)
store volatile %struct.ScmObj* %argslist55112$ae498442, %struct.ScmObj** %stackaddr$prim56477, align 8
%clofunc56478 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49844)
musttail call tailcc void %clofunc56478(%struct.ScmObj* %ae49844, %struct.ScmObj* %argslist55112$ae498442)
ret void
}

define tailcc void @proc_clo$ae49844(%struct.ScmObj* %env$ae49844,%struct.ScmObj* %current_45args55097) {
%stackaddr$env-ref56479 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 0)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56479
%stackaddr$env-ref56480 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 1)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56480
%stackaddr$env-ref56481 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 2)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref56481
%stackaddr$env-ref56482 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56482
%stackaddr$env-ref56483 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56483
%stackaddr$env-ref56484 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 5)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56484
%stackaddr$env-ref56485 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 6)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56485
%stackaddr$prim56486 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55097)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim56486, align 8
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%current_45args55098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55097)
store volatile %struct.ScmObj* %current_45args55098, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55098)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim56488, align 8
%ae49867 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56489 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49867)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim56489, align 8
%stackaddr$makeclosure56490 = alloca %struct.ScmObj*, align 8
%fptrToInt56491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49869 to i64
%ae49869 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56491)
store volatile %struct.ScmObj* %ae49869, %struct.ScmObj** %stackaddr$makeclosure56490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49869, %struct.ScmObj* %lsts_4348138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49869, %struct.ScmObj* %k48330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49869, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49869, %struct.ScmObj* %_37foldl48129, i64 3)
%argslist55106$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56492 = alloca %struct.ScmObj*, align 8
%argslist55106$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist55106$_37foldr480510)
store volatile %struct.ScmObj* %argslist55106$_37foldr480511, %struct.ScmObj** %stackaddr$prim56492, align 8
%stackaddr$prim56493 = alloca %struct.ScmObj*, align 8
%argslist55106$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist55106$_37foldr480511)
store volatile %struct.ScmObj* %argslist55106$_37foldr480512, %struct.ScmObj** %stackaddr$prim56493, align 8
%stackaddr$prim56494 = alloca %struct.ScmObj*, align 8
%argslist55106$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist55106$_37foldr480512)
store volatile %struct.ScmObj* %argslist55106$_37foldr480513, %struct.ScmObj** %stackaddr$prim56494, align 8
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%argslist55106$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49869, %struct.ScmObj* %argslist55106$_37foldr480513)
store volatile %struct.ScmObj* %argslist55106$_37foldr480514, %struct.ScmObj** %stackaddr$prim56495, align 8
%clofunc56496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56496(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist55106$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49869(%struct.ScmObj* %env$ae49869,%struct.ScmObj* %current_45args55100) {
%stackaddr$env-ref56497 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49869, i64 0)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56497
%stackaddr$env-ref56498 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49869, i64 1)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56498
%stackaddr$env-ref56499 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49869, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56499
%stackaddr$env-ref56500 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49869, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56500
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55100)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim56501, align 8
%stackaddr$prim56502 = alloca %struct.ScmObj*, align 8
%current_45args55101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55100)
store volatile %struct.ScmObj* %current_45args55101, %struct.ScmObj** %stackaddr$prim56502, align 8
%stackaddr$prim56503 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55101)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim56503, align 8
%stackaddr$makeclosure56504 = alloca %struct.ScmObj*, align 8
%fptrToInt56505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49873 to i64
%ae49873 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56505)
store volatile %struct.ScmObj* %ae49873, %struct.ScmObj** %stackaddr$makeclosure56504, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %lsts_4348138, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %k48330, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %_37foldl48129, i64 3)
%stackaddr$prim56506 = alloca %struct.ScmObj*, align 8
%cpsargs48341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49873, %struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %cpsargs48341, %struct.ScmObj** %stackaddr$prim56506, align 8
%clofunc56507 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc56507(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48341)
ret void
}

define tailcc void @proc_clo$ae49873(%struct.ScmObj* %env$ae49873,%struct.ScmObj* %current_45args55103) {
%stackaddr$env-ref56508 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 0)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56508
%stackaddr$env-ref56509 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 1)
store %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$env-ref56509
%stackaddr$env-ref56510 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56510
%stackaddr$env-ref56511 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56511
%stackaddr$prim56512 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim56512, align 8
%stackaddr$prim56513 = alloca %struct.ScmObj*, align 8
%current_45args55104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %current_45args55104, %struct.ScmObj** %stackaddr$prim56513, align 8
%stackaddr$prim56514 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55104)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim56514, align 8
%stackaddr$prim56515 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim56515, align 8
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48208)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%cpsargs48340 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48330, %struct.ScmObj* %anf_45bind48209)
store volatile %struct.ScmObj* %cpsargs48340, %struct.ScmObj** %stackaddr$prim56517, align 8
%clofunc56518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc56518(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48340)
ret void
}

define tailcc void @proc_clo$ae49846(%struct.ScmObj* %env$ae49846,%struct.ScmObj* %current_45args55107) {
%stackaddr$prim56519 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$prim56519, align 8
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%current_45args55108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %current_45args55108, %struct.ScmObj** %stackaddr$prim56520, align 8
%stackaddr$prim56521 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55108)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim56521, align 8
%stackaddr$prim56522 = alloca %struct.ScmObj*, align 8
%current_45args55109 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55108)
store volatile %struct.ScmObj* %current_45args55109, %struct.ScmObj** %stackaddr$prim56522, align 8
%stackaddr$prim56523 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55109)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim56523, align 8
%stackaddr$prim56524 = alloca %struct.ScmObj*, align 8
%cpsprim48343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48343, %struct.ScmObj** %stackaddr$prim56524, align 8
%ae49850 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55111$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56525 = alloca %struct.ScmObj*, align 8
%argslist55111$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48343, %struct.ScmObj* %argslist55111$k483420)
store volatile %struct.ScmObj* %argslist55111$k483421, %struct.ScmObj** %stackaddr$prim56525, align 8
%stackaddr$prim56526 = alloca %struct.ScmObj*, align 8
%argslist55111$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49850, %struct.ScmObj* %argslist55111$k483421)
store volatile %struct.ScmObj* %argslist55111$k483422, %struct.ScmObj** %stackaddr$prim56526, align 8
%clofunc56527 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc56527(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist55111$k483422)
ret void
}

define tailcc void @proc_clo$ae49822(%struct.ScmObj* %env$ae49822,%struct.ScmObj* %current_45args55114) {
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55114)
store volatile %struct.ScmObj* %k48344, %struct.ScmObj** %stackaddr$prim56528, align 8
%stackaddr$prim56529 = alloca %struct.ScmObj*, align 8
%current_45args55115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55114)
store volatile %struct.ScmObj* %current_45args55115, %struct.ScmObj** %stackaddr$prim56529, align 8
%stackaddr$prim56530 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55115)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim56530, align 8
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%cpsprim48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48345, %struct.ScmObj** %stackaddr$prim56531, align 8
%ae49825 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55117$k483440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%argslist55117$k483441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48345, %struct.ScmObj* %argslist55117$k483440)
store volatile %struct.ScmObj* %argslist55117$k483441, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%argslist55117$k483442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49825, %struct.ScmObj* %argslist55117$k483441)
store volatile %struct.ScmObj* %argslist55117$k483442, %struct.ScmObj** %stackaddr$prim56533, align 8
%clofunc56534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48344)
musttail call tailcc void %clofunc56534(%struct.ScmObj* %k48344, %struct.ScmObj* %argslist55117$k483442)
ret void
}

define tailcc void @proc_clo$ae49798(%struct.ScmObj* %env$ae49798,%struct.ScmObj* %current_45args55120) {
%stackaddr$prim56535 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55120)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim56535, align 8
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%current_45args55121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55120)
store volatile %struct.ScmObj* %current_45args55121, %struct.ScmObj** %stackaddr$prim56536, align 8
%stackaddr$prim56537 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55121)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim56537, align 8
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%cpsprim48347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48347, %struct.ScmObj** %stackaddr$prim56538, align 8
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55123$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56539 = alloca %struct.ScmObj*, align 8
%argslist55123$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48347, %struct.ScmObj* %argslist55123$k483460)
store volatile %struct.ScmObj* %argslist55123$k483461, %struct.ScmObj** %stackaddr$prim56539, align 8
%stackaddr$prim56540 = alloca %struct.ScmObj*, align 8
%argslist55123$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49801, %struct.ScmObj* %argslist55123$k483461)
store volatile %struct.ScmObj* %argslist55123$k483462, %struct.ScmObj** %stackaddr$prim56540, align 8
%clofunc56541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc56541(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist55123$k483462)
ret void
}

define tailcc void @proc_clo$ae49750(%struct.ScmObj* %env$ae49750,%struct.ScmObj* %current_45args55126) {
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55126)
store volatile %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%current_45args55127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55126)
store volatile %struct.ScmObj* %current_45args55127, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55127)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim56544, align 8
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%current_45args55128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55127)
store volatile %struct.ScmObj* %current_45args55128, %struct.ScmObj** %stackaddr$prim56545, align 8
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55128)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim56546, align 8
%truthy$cmp56547 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp56547 = icmp eq i64 %truthy$cmp56547, 1
br i1 %cmp$cmp56547, label %truebranch$cmp56547, label %falsebranch$cmp56547
truebranch$cmp56547:
%ae49753 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55130$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56548 = alloca %struct.ScmObj*, align 8
%argslist55130$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist55130$k483480)
store volatile %struct.ScmObj* %argslist55130$k483481, %struct.ScmObj** %stackaddr$prim56548, align 8
%stackaddr$prim56549 = alloca %struct.ScmObj*, align 8
%argslist55130$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49753, %struct.ScmObj* %argslist55130$k483481)
store volatile %struct.ScmObj* %argslist55130$k483482, %struct.ScmObj** %stackaddr$prim56549, align 8
%clofunc56550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc56550(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist55130$k483482)
ret void
falsebranch$cmp56547:
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%cpsprim48349 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48349, %struct.ScmObj** %stackaddr$prim56551, align 8
%ae49760 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55131$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56552 = alloca %struct.ScmObj*, align 8
%argslist55131$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48349, %struct.ScmObj* %argslist55131$k483480)
store volatile %struct.ScmObj* %argslist55131$k483481, %struct.ScmObj** %stackaddr$prim56552, align 8
%stackaddr$prim56553 = alloca %struct.ScmObj*, align 8
%argslist55131$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49760, %struct.ScmObj* %argslist55131$k483481)
store volatile %struct.ScmObj* %argslist55131$k483482, %struct.ScmObj** %stackaddr$prim56553, align 8
%clofunc56554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc56554(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist55131$k483482)
ret void
}

define tailcc void @proc_clo$ae49591(%struct.ScmObj* %env$ae49591,%struct.ScmObj* %args4807348350) {
%stackaddr$env-ref56555 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49591, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56555
%stackaddr$env-ref56556 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49591, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56556
%stackaddr$env-ref56557 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49591, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56557
%stackaddr$prim56558 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348350)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim56558, align 8
%stackaddr$prim56559 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348350)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim56559, align 8
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim56561, align 8
%stackaddr$makeclosure56562 = alloca %struct.ScmObj*, align 8
%fptrToInt56563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49596 to i64
%ae49596 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56563)
store volatile %struct.ScmObj* %ae49596, %struct.ScmObj** %stackaddr$makeclosure56562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49596, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49596, %struct.ScmObj* %k48351, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49596, %struct.ScmObj* %lsts48074, i64 2)
%ae49597 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56564 = alloca %struct.ScmObj*, align 8
%fptrToInt56565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49598 to i64
%ae49598 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56565)
store volatile %struct.ScmObj* %ae49598, %struct.ScmObj** %stackaddr$makeclosure56564, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49598, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49598, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49598, %struct.ScmObj* %f48075, i64 2)
%argslist55150$ae495960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%argslist55150$ae495961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49598, %struct.ScmObj* %argslist55150$ae495960)
store volatile %struct.ScmObj* %argslist55150$ae495961, %struct.ScmObj** %stackaddr$prim56566, align 8
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%argslist55150$ae495962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49597, %struct.ScmObj* %argslist55150$ae495961)
store volatile %struct.ScmObj* %argslist55150$ae495962, %struct.ScmObj** %stackaddr$prim56567, align 8
%clofunc56568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49596)
musttail call tailcc void %clofunc56568(%struct.ScmObj* %ae49596, %struct.ScmObj* %argslist55150$ae495962)
ret void
}

define tailcc void @proc_clo$ae49596(%struct.ScmObj* %env$ae49596,%struct.ScmObj* %current_45args55135) {
%stackaddr$env-ref56569 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49596, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56569
%stackaddr$env-ref56570 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49596, i64 1)
store %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$env-ref56570
%stackaddr$env-ref56571 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49596, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref56571
%stackaddr$prim56572 = alloca %struct.ScmObj*, align 8
%_95k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55135)
store volatile %struct.ScmObj* %_95k48352, %struct.ScmObj** %stackaddr$prim56572, align 8
%stackaddr$prim56573 = alloca %struct.ScmObj*, align 8
%current_45args55136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55135)
store volatile %struct.ScmObj* %current_45args55136, %struct.ScmObj** %stackaddr$prim56573, align 8
%stackaddr$prim56574 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55136)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim56574, align 8
%ae49659 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56575 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49659, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim56575, align 8
%stackaddr$prim56576 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %anf_45bind48197)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim56576, align 8
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%cpsargs48353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48351, %struct.ScmObj* %anf_45bind48198)
store volatile %struct.ScmObj* %cpsargs48353, %struct.ScmObj** %stackaddr$prim56577, align 8
%clofunc56578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56578(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48353)
ret void
}

define tailcc void @proc_clo$ae49598(%struct.ScmObj* %env$ae49598,%struct.ScmObj* %fargs4807648354) {
%stackaddr$env-ref56579 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49598, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56579
%stackaddr$env-ref56580 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49598, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56580
%stackaddr$env-ref56581 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49598, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56581
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648354)
store volatile %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$prim56582, align 8
%stackaddr$prim56583 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648354)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim56583, align 8
%stackaddr$makeclosure56584 = alloca %struct.ScmObj*, align 8
%fptrToInt56585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49602 to i64
%ae49602 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56585)
store volatile %struct.ScmObj* %ae49602, %struct.ScmObj** %stackaddr$makeclosure56584, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %k48355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %fargs48076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %f48075, i64 3)
%ae49604 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55149$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56586 = alloca %struct.ScmObj*, align 8
%argslist55149$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49604, %struct.ScmObj* %argslist55149$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist55149$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim56586, align 8
%stackaddr$prim56587 = alloca %struct.ScmObj*, align 8
%argslist55149$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist55149$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist55149$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim56587, align 8
%stackaddr$prim56588 = alloca %struct.ScmObj*, align 8
%argslist55149$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49602, %struct.ScmObj* %argslist55149$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist55149$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim56588, align 8
%clofunc56589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc56589(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist55149$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49602(%struct.ScmObj* %env$ae49602,%struct.ScmObj* %current_45args55138) {
%stackaddr$env-ref56590 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56590
%stackaddr$env-ref56591 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 1)
store %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$env-ref56591
%stackaddr$env-ref56592 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56592
%stackaddr$env-ref56593 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 3)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56593
%stackaddr$prim56594 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55138)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim56594, align 8
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%current_45args55139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55138)
store volatile %struct.ScmObj* %current_45args55139, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55139)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56596, align 8
%stackaddr$makeclosure56597 = alloca %struct.ScmObj*, align 8
%fptrToInt56598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49609 to i64
%ae49609 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56598)
store volatile %struct.ScmObj* %ae49609, %struct.ScmObj** %stackaddr$makeclosure56597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49609, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49609, %struct.ScmObj* %k48355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49609, %struct.ScmObj* %fargs48076, i64 2)
%stackaddr$prim56599 = alloca %struct.ScmObj*, align 8
%cpsargs48360 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49609, %struct.ScmObj* %anf_45bind48193)
store volatile %struct.ScmObj* %cpsargs48360, %struct.ScmObj** %stackaddr$prim56599, align 8
%clofunc56600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc56600(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48360)
ret void
}

define tailcc void @proc_clo$ae49609(%struct.ScmObj* %env$ae49609,%struct.ScmObj* %current_45args55141) {
%stackaddr$env-ref56601 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49609, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56601
%stackaddr$env-ref56602 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49609, i64 1)
store %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$env-ref56602
%stackaddr$env-ref56603 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49609, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56603
%stackaddr$prim56604 = alloca %struct.ScmObj*, align 8
%_95k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55141)
store volatile %struct.ScmObj* %_95k48357, %struct.ScmObj** %stackaddr$prim56604, align 8
%stackaddr$prim56605 = alloca %struct.ScmObj*, align 8
%current_45args55142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55141)
store volatile %struct.ScmObj* %current_45args55142, %struct.ScmObj** %stackaddr$prim56605, align 8
%stackaddr$prim56606 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55142)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim56606, align 8
%stackaddr$makeclosure56607 = alloca %struct.ScmObj*, align 8
%fptrToInt56608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49614 to i64
%ae49614 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56608)
store volatile %struct.ScmObj* %ae49614, %struct.ScmObj** %stackaddr$makeclosure56607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49614, %struct.ScmObj* %k48355, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49614, %struct.ScmObj* %anf_45bind48194, i64 1)
%argslist55148$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%argslist55148$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist55148$_37last480680)
store volatile %struct.ScmObj* %argslist55148$_37last480681, %struct.ScmObj** %stackaddr$prim56609, align 8
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%argslist55148$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49614, %struct.ScmObj* %argslist55148$_37last480681)
store volatile %struct.ScmObj* %argslist55148$_37last480682, %struct.ScmObj** %stackaddr$prim56610, align 8
%clofunc56611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc56611(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist55148$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49614(%struct.ScmObj* %env$ae49614,%struct.ScmObj* %current_45args55144) {
%stackaddr$env-ref56612 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49614, i64 0)
store %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$env-ref56612
%stackaddr$env-ref56613 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49614, i64 1)
store %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$env-ref56613
%stackaddr$prim56614 = alloca %struct.ScmObj*, align 8
%_95k48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55144)
store volatile %struct.ScmObj* %_95k48358, %struct.ScmObj** %stackaddr$prim56614, align 8
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%current_45args55145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55144)
store volatile %struct.ScmObj* %current_45args55145, %struct.ScmObj** %stackaddr$prim56615, align 8
%stackaddr$prim56616 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55145)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim56616, align 8
%stackaddr$prim56617 = alloca %struct.ScmObj*, align 8
%cpsprim48359 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %anf_45bind48195)
store volatile %struct.ScmObj* %cpsprim48359, %struct.ScmObj** %stackaddr$prim56617, align 8
%ae49619 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55147$k483550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%argslist55147$k483551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48359, %struct.ScmObj* %argslist55147$k483550)
store volatile %struct.ScmObj* %argslist55147$k483551, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%argslist55147$k483552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49619, %struct.ScmObj* %argslist55147$k483551)
store volatile %struct.ScmObj* %argslist55147$k483552, %struct.ScmObj** %stackaddr$prim56619, align 8
%clofunc56620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48355)
musttail call tailcc void %clofunc56620(%struct.ScmObj* %k48355, %struct.ScmObj* %argslist55147$k483552)
ret void
}

define tailcc void @proc_clo$ae49514(%struct.ScmObj* %env$ae49514,%struct.ScmObj* %current_45args55152) {
%stackaddr$env-ref56621 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56621
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55152)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%current_45args55153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55152)
store volatile %struct.ScmObj* %current_45args55153, %struct.ScmObj** %stackaddr$prim56623, align 8
%stackaddr$prim56624 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55153)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim56624, align 8
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%current_45args55154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55153)
store volatile %struct.ScmObj* %current_45args55154, %struct.ScmObj** %stackaddr$prim56625, align 8
%stackaddr$prim56626 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55154)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim56626, align 8
%stackaddr$makeclosure56627 = alloca %struct.ScmObj*, align 8
%fptrToInt56628 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49515 to i64
%ae49515 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56628)
store volatile %struct.ScmObj* %ae49515, %struct.ScmObj** %stackaddr$makeclosure56627, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49515, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49515, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49515, %struct.ScmObj* %k48361, i64 2)
%ae49516 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56629 = alloca %struct.ScmObj*, align 8
%fptrToInt56630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49517 to i64
%ae49517 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56630)
store volatile %struct.ScmObj* %ae49517, %struct.ScmObj** %stackaddr$makeclosure56629, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49517, %struct.ScmObj* %f48079, i64 0)
%argslist55169$ae495150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%argslist55169$ae495151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49517, %struct.ScmObj* %argslist55169$ae495150)
store volatile %struct.ScmObj* %argslist55169$ae495151, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%argslist55169$ae495152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49516, %struct.ScmObj* %argslist55169$ae495151)
store volatile %struct.ScmObj* %argslist55169$ae495152, %struct.ScmObj** %stackaddr$prim56632, align 8
%clofunc56633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49515)
musttail call tailcc void %clofunc56633(%struct.ScmObj* %ae49515, %struct.ScmObj* %argslist55169$ae495152)
ret void
}

define tailcc void @proc_clo$ae49515(%struct.ScmObj* %env$ae49515,%struct.ScmObj* %current_45args55156) {
%stackaddr$env-ref56634 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49515, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref56634
%stackaddr$env-ref56635 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49515, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56635
%stackaddr$env-ref56636 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49515, i64 2)
store %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$env-ref56636
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55156)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$prim56638 = alloca %struct.ScmObj*, align 8
%current_45args55157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55156)
store volatile %struct.ScmObj* %current_45args55157, %struct.ScmObj** %stackaddr$prim56638, align 8
%stackaddr$prim56639 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55157)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim56639, align 8
%ae49549 = call %struct.ScmObj* @const_init_null()
%argslist55159$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56640 = alloca %struct.ScmObj*, align 8
%argslist55159$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist55159$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55159$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56640, align 8
%stackaddr$prim56641 = alloca %struct.ScmObj*, align 8
%argslist55159$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49549, %struct.ScmObj* %argslist55159$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55159$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56641, align 8
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%argslist55159$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist55159$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55159$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%argslist55159$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist55159$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55159$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56643, align 8
%clofunc56644 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56644(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55159$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49517(%struct.ScmObj* %env$ae49517,%struct.ScmObj* %current_45args55160) {
%stackaddr$env-ref56645 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49517, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref56645
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55160)
store volatile %struct.ScmObj* %k48363, %struct.ScmObj** %stackaddr$prim56646, align 8
%stackaddr$prim56647 = alloca %struct.ScmObj*, align 8
%current_45args55161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55160)
store volatile %struct.ScmObj* %current_45args55161, %struct.ScmObj** %stackaddr$prim56647, align 8
%stackaddr$prim56648 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55161)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim56648, align 8
%stackaddr$prim56649 = alloca %struct.ScmObj*, align 8
%current_45args55162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55161)
store volatile %struct.ScmObj* %current_45args55162, %struct.ScmObj** %stackaddr$prim56649, align 8
%stackaddr$prim56650 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55162)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim56650, align 8
%stackaddr$makeclosure56651 = alloca %struct.ScmObj*, align 8
%fptrToInt56652 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49519 to i64
%ae49519 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56652)
store volatile %struct.ScmObj* %ae49519, %struct.ScmObj** %stackaddr$makeclosure56651, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49519, %struct.ScmObj* %r48080, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49519, %struct.ScmObj* %k48363, i64 1)
%argslist55168$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56653 = alloca %struct.ScmObj*, align 8
%argslist55168$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist55168$f480790)
store volatile %struct.ScmObj* %argslist55168$f480791, %struct.ScmObj** %stackaddr$prim56653, align 8
%stackaddr$prim56654 = alloca %struct.ScmObj*, align 8
%argslist55168$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49519, %struct.ScmObj* %argslist55168$f480791)
store volatile %struct.ScmObj* %argslist55168$f480792, %struct.ScmObj** %stackaddr$prim56654, align 8
%clofunc56655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc56655(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist55168$f480792)
ret void
}

define tailcc void @proc_clo$ae49519(%struct.ScmObj* %env$ae49519,%struct.ScmObj* %current_45args55164) {
%stackaddr$env-ref56656 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49519, i64 0)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref56656
%stackaddr$env-ref56657 = alloca %struct.ScmObj*, align 8
%k48363 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49519, i64 1)
store %struct.ScmObj* %k48363, %struct.ScmObj** %stackaddr$env-ref56657
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%current_45args55165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55164)
store volatile %struct.ScmObj* %current_45args55165, %struct.ScmObj** %stackaddr$prim56659, align 8
%stackaddr$prim56660 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55165)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim56660, align 8
%stackaddr$prim56661 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim56661, align 8
%ae49524 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55167$k483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56662 = alloca %struct.ScmObj*, align 8
%argslist55167$k483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist55167$k483630)
store volatile %struct.ScmObj* %argslist55167$k483631, %struct.ScmObj** %stackaddr$prim56662, align 8
%stackaddr$prim56663 = alloca %struct.ScmObj*, align 8
%argslist55167$k483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49524, %struct.ScmObj* %argslist55167$k483631)
store volatile %struct.ScmObj* %argslist55167$k483632, %struct.ScmObj** %stackaddr$prim56663, align 8
%clofunc56664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48363)
musttail call tailcc void %clofunc56664(%struct.ScmObj* %k48363, %struct.ScmObj* %argslist55167$k483632)
ret void
}

define tailcc void @proc_clo$ae49128(%struct.ScmObj* %env$ae49128,%struct.ScmObj* %current_45args55172) {
%stackaddr$env-ref56665 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56665
%stackaddr$env-ref56666 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49128, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56666
%stackaddr$prim56667 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55172)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim56667, align 8
%stackaddr$prim56668 = alloca %struct.ScmObj*, align 8
%current_45args55173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55172)
store volatile %struct.ScmObj* %current_45args55173, %struct.ScmObj** %stackaddr$prim56668, align 8
%stackaddr$prim56669 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55173)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim56669, align 8
%ae49130 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56670 = alloca %struct.ScmObj*, align 8
%fptrToInt56671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49131 to i64
%ae49131 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56671)
store volatile %struct.ScmObj* %ae49131, %struct.ScmObj** %stackaddr$makeclosure56670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49131, %struct.ScmObj* %_37map148042, i64 2)
%argslist55230$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%argslist55230$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49131, %struct.ScmObj* %argslist55230$k483660)
store volatile %struct.ScmObj* %argslist55230$k483661, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$prim56673 = alloca %struct.ScmObj*, align 8
%argslist55230$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49130, %struct.ScmObj* %argslist55230$k483661)
store volatile %struct.ScmObj* %argslist55230$k483662, %struct.ScmObj** %stackaddr$prim56673, align 8
%clofunc56674 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc56674(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist55230$k483662)
ret void
}

define tailcc void @proc_clo$ae49131(%struct.ScmObj* %env$ae49131,%struct.ScmObj* %args4805348367) {
%stackaddr$env-ref56675 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56675
%stackaddr$env-ref56676 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56676
%stackaddr$env-ref56677 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49131, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56677
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348367)
store volatile %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$prim56678, align 8
%stackaddr$prim56679 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348367)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim56679, align 8
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim56680, align 8
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim56681, align 8
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48178)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim56682, align 8
%stackaddr$prim56683 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim56683, align 8
%stackaddr$prim56684 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48179)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim56684, align 8
%stackaddr$makeclosure56685 = alloca %struct.ScmObj*, align 8
%fptrToInt56686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49139 to i64
%ae49139 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56686)
store volatile %struct.ScmObj* %ae49139, %struct.ScmObj** %stackaddr$makeclosure56685, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %k48368, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49139, %struct.ScmObj* %_37map148042, i64 6)
%ae49140 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56687 = alloca %struct.ScmObj*, align 8
%fptrToInt56688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49141 to i64
%ae49141 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56688)
store volatile %struct.ScmObj* %ae49141, %struct.ScmObj** %stackaddr$makeclosure56687, align 8
%argslist55229$ae491390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%argslist55229$ae491391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49141, %struct.ScmObj* %argslist55229$ae491390)
store volatile %struct.ScmObj* %argslist55229$ae491391, %struct.ScmObj** %stackaddr$prim56689, align 8
%stackaddr$prim56690 = alloca %struct.ScmObj*, align 8
%argslist55229$ae491392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49140, %struct.ScmObj* %argslist55229$ae491391)
store volatile %struct.ScmObj* %argslist55229$ae491392, %struct.ScmObj** %stackaddr$prim56690, align 8
%clofunc56691 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49139)
musttail call tailcc void %clofunc56691(%struct.ScmObj* %ae49139, %struct.ScmObj* %argslist55229$ae491392)
ret void
}

define tailcc void @proc_clo$ae49139(%struct.ScmObj* %env$ae49139,%struct.ScmObj* %current_45args55175) {
%stackaddr$env-ref56692 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56692
%stackaddr$env-ref56693 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56693
%stackaddr$env-ref56694 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56694
%stackaddr$env-ref56695 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56695
%stackaddr$env-ref56696 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 4)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56696
%stackaddr$env-ref56697 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56697
%stackaddr$env-ref56698 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49139, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56698
%stackaddr$prim56699 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55175)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim56699, align 8
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%current_45args55176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55175)
store volatile %struct.ScmObj* %current_45args55176, %struct.ScmObj** %stackaddr$prim56700, align 8
%stackaddr$prim56701 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55176)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim56701, align 8
%stackaddr$makeclosure56702 = alloca %struct.ScmObj*, align 8
%fptrToInt56703 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49171 to i64
%ae49171 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56703)
store volatile %struct.ScmObj* %ae49171, %struct.ScmObj** %stackaddr$makeclosure56702, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %k48368, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49171, %struct.ScmObj* %_37map148042, i64 6)
%ae49173 = call %struct.ScmObj* @const_init_false()
%argslist55222$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56704 = alloca %struct.ScmObj*, align 8
%argslist55222$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55222$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55222$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56704, align 8
%stackaddr$prim56705 = alloca %struct.ScmObj*, align 8
%argslist55222$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49173, %struct.ScmObj* %argslist55222$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55222$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56705, align 8
%stackaddr$prim56706 = alloca %struct.ScmObj*, align 8
%argslist55222$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist55222$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55222$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56706, align 8
%stackaddr$prim56707 = alloca %struct.ScmObj*, align 8
%argslist55222$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49171, %struct.ScmObj* %argslist55222$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55222$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56707, align 8
%clofunc56708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56708(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55222$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49171(%struct.ScmObj* %env$ae49171,%struct.ScmObj* %current_45args55178) {
%stackaddr$env-ref56709 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56709
%stackaddr$env-ref56710 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56710
%stackaddr$env-ref56711 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56711
%stackaddr$env-ref56712 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56712
%stackaddr$env-ref56713 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 4)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56713
%stackaddr$env-ref56714 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56714
%stackaddr$env-ref56715 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49171, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56715
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%_95k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55178)
store volatile %struct.ScmObj* %_95k48370, %struct.ScmObj** %stackaddr$prim56716, align 8
%stackaddr$prim56717 = alloca %struct.ScmObj*, align 8
%current_45args55179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55178)
store volatile %struct.ScmObj* %current_45args55179, %struct.ScmObj** %stackaddr$prim56717, align 8
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55179)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim56718, align 8
%truthy$cmp56719 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48181)
%cmp$cmp56719 = icmp eq i64 %truthy$cmp56719, 1
br i1 %cmp$cmp56719, label %truebranch$cmp56719, label %falsebranch$cmp56719
truebranch$cmp56719:
%ae49182 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55181$k483680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56720 = alloca %struct.ScmObj*, align 8
%argslist55181$k483681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist55181$k483680)
store volatile %struct.ScmObj* %argslist55181$k483681, %struct.ScmObj** %stackaddr$prim56720, align 8
%stackaddr$prim56721 = alloca %struct.ScmObj*, align 8
%argslist55181$k483682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49182, %struct.ScmObj* %argslist55181$k483681)
store volatile %struct.ScmObj* %argslist55181$k483682, %struct.ScmObj** %stackaddr$prim56721, align 8
%clofunc56722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48368)
musttail call tailcc void %clofunc56722(%struct.ScmObj* %k48368, %struct.ScmObj* %argslist55181$k483682)
ret void
falsebranch$cmp56719:
%stackaddr$makeclosure56723 = alloca %struct.ScmObj*, align 8
%fptrToInt56724 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49187 to i64
%ae49187 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56724)
store volatile %struct.ScmObj* %ae49187, %struct.ScmObj** %stackaddr$makeclosure56723, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %k48368, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49187, %struct.ScmObj* %_37map148042, i64 6)
%ae49188 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56725 = alloca %struct.ScmObj*, align 8
%fptrToInt56726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49189 to i64
%ae49189 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56726)
store volatile %struct.ScmObj* %ae49189, %struct.ScmObj** %stackaddr$makeclosure56725, align 8
%argslist55221$ae491870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56727 = alloca %struct.ScmObj*, align 8
%argslist55221$ae491871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49189, %struct.ScmObj* %argslist55221$ae491870)
store volatile %struct.ScmObj* %argslist55221$ae491871, %struct.ScmObj** %stackaddr$prim56727, align 8
%stackaddr$prim56728 = alloca %struct.ScmObj*, align 8
%argslist55221$ae491872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49188, %struct.ScmObj* %argslist55221$ae491871)
store volatile %struct.ScmObj* %argslist55221$ae491872, %struct.ScmObj** %stackaddr$prim56728, align 8
%clofunc56729 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49187)
musttail call tailcc void %clofunc56729(%struct.ScmObj* %ae49187, %struct.ScmObj* %argslist55221$ae491872)
ret void
}

define tailcc void @proc_clo$ae49187(%struct.ScmObj* %env$ae49187,%struct.ScmObj* %current_45args55182) {
%stackaddr$env-ref56730 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56730
%stackaddr$env-ref56731 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56731
%stackaddr$env-ref56732 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56732
%stackaddr$env-ref56733 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56733
%stackaddr$env-ref56734 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 4)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56734
%stackaddr$env-ref56735 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56735
%stackaddr$env-ref56736 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49187, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56736
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%_95k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %_95k48371, %struct.ScmObj** %stackaddr$prim56737, align 8
%stackaddr$prim56738 = alloca %struct.ScmObj*, align 8
%current_45args55183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55182)
store volatile %struct.ScmObj* %current_45args55183, %struct.ScmObj** %stackaddr$prim56738, align 8
%stackaddr$prim56739 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55183)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim56739, align 8
%stackaddr$makeclosure56740 = alloca %struct.ScmObj*, align 8
%fptrToInt56741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49208 to i64
%ae49208 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56741)
store volatile %struct.ScmObj* %ae49208, %struct.ScmObj** %stackaddr$makeclosure56740, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %k48368, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49208, %struct.ScmObj* %_37map148042, i64 6)
%argslist55216$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56742 = alloca %struct.ScmObj*, align 8
%argslist55216$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55216$_37map1480420)
store volatile %struct.ScmObj* %argslist55216$_37map1480421, %struct.ScmObj** %stackaddr$prim56742, align 8
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%argslist55216$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist55216$_37map1480421)
store volatile %struct.ScmObj* %argslist55216$_37map1480422, %struct.ScmObj** %stackaddr$prim56743, align 8
%stackaddr$prim56744 = alloca %struct.ScmObj*, align 8
%argslist55216$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49208, %struct.ScmObj* %argslist55216$_37map1480422)
store volatile %struct.ScmObj* %argslist55216$_37map1480423, %struct.ScmObj** %stackaddr$prim56744, align 8
%clofunc56745 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56745(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist55216$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49208(%struct.ScmObj* %env$ae49208,%struct.ScmObj* %current_45args55185) {
%stackaddr$env-ref56746 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56746
%stackaddr$env-ref56747 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56747
%stackaddr$env-ref56748 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56748
%stackaddr$env-ref56749 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56749
%stackaddr$env-ref56750 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 4)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56750
%stackaddr$env-ref56751 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56751
%stackaddr$env-ref56752 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49208, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56752
%stackaddr$prim56753 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55185)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim56753, align 8
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%current_45args55186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55185)
store volatile %struct.ScmObj* %current_45args55186, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55186)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$makeclosure56756 = alloca %struct.ScmObj*, align 8
%fptrToInt56757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49211 to i64
%ae49211 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56757)
store volatile %struct.ScmObj* %ae49211, %struct.ScmObj** %stackaddr$makeclosure56756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %k48368, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %lsts_4348061, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49211, %struct.ScmObj* %_37map148042, i64 7)
%ae49212 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56758 = alloca %struct.ScmObj*, align 8
%fptrToInt56759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49213 to i64
%ae49213 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56759)
store volatile %struct.ScmObj* %ae49213, %struct.ScmObj** %stackaddr$makeclosure56758, align 8
%argslist55215$ae492110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56760 = alloca %struct.ScmObj*, align 8
%argslist55215$ae492111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49213, %struct.ScmObj* %argslist55215$ae492110)
store volatile %struct.ScmObj* %argslist55215$ae492111, %struct.ScmObj** %stackaddr$prim56760, align 8
%stackaddr$prim56761 = alloca %struct.ScmObj*, align 8
%argslist55215$ae492112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist55215$ae492111)
store volatile %struct.ScmObj* %argslist55215$ae492112, %struct.ScmObj** %stackaddr$prim56761, align 8
%clofunc56762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49211)
musttail call tailcc void %clofunc56762(%struct.ScmObj* %ae49211, %struct.ScmObj* %argslist55215$ae492112)
ret void
}

define tailcc void @proc_clo$ae49211(%struct.ScmObj* %env$ae49211,%struct.ScmObj* %current_45args55188) {
%stackaddr$env-ref56763 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56763
%stackaddr$env-ref56764 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56764
%stackaddr$env-ref56765 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56765
%stackaddr$env-ref56766 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56766
%stackaddr$env-ref56767 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 4)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56767
%stackaddr$env-ref56768 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56768
%stackaddr$env-ref56769 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 6)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56769
%stackaddr$env-ref56770 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49211, i64 7)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56770
%stackaddr$prim56771 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55188)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim56771, align 8
%stackaddr$prim56772 = alloca %struct.ScmObj*, align 8
%current_45args55189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55188)
store volatile %struct.ScmObj* %current_45args55189, %struct.ScmObj** %stackaddr$prim56772, align 8
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55189)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim56773, align 8
%stackaddr$makeclosure56774 = alloca %struct.ScmObj*, align 8
%fptrToInt56775 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49232 to i64
%ae49232 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56775)
store volatile %struct.ScmObj* %ae49232, %struct.ScmObj** %stackaddr$makeclosure56774, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %k48368, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49232, %struct.ScmObj* %lsts_4348061, i64 5)
%argslist55210$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56776 = alloca %struct.ScmObj*, align 8
%argslist55210$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55210$_37map1480420)
store volatile %struct.ScmObj* %argslist55210$_37map1480421, %struct.ScmObj** %stackaddr$prim56776, align 8
%stackaddr$prim56777 = alloca %struct.ScmObj*, align 8
%argslist55210$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist55210$_37map1480421)
store volatile %struct.ScmObj* %argslist55210$_37map1480422, %struct.ScmObj** %stackaddr$prim56777, align 8
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%argslist55210$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49232, %struct.ScmObj* %argslist55210$_37map1480422)
store volatile %struct.ScmObj* %argslist55210$_37map1480423, %struct.ScmObj** %stackaddr$prim56778, align 8
%clofunc56779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56779(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist55210$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49232(%struct.ScmObj* %env$ae49232,%struct.ScmObj* %current_45args55191) {
%stackaddr$env-ref56780 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56780
%stackaddr$env-ref56781 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56781
%stackaddr$env-ref56782 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56782
%stackaddr$env-ref56783 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 3)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56783
%stackaddr$env-ref56784 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56784
%stackaddr$env-ref56785 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49232, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56785
%stackaddr$prim56786 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55191)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim56786, align 8
%stackaddr$prim56787 = alloca %struct.ScmObj*, align 8
%current_45args55192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55191)
store volatile %struct.ScmObj* %current_45args55192, %struct.ScmObj** %stackaddr$prim56787, align 8
%stackaddr$prim56788 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55192)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim56788, align 8
%stackaddr$makeclosure56789 = alloca %struct.ScmObj*, align 8
%fptrToInt56790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49235 to i64
%ae49235 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56790)
store volatile %struct.ScmObj* %ae49235, %struct.ScmObj** %stackaddr$makeclosure56789, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %k48368, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %vs48059, i64 6)
%ae49236 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56791 = alloca %struct.ScmObj*, align 8
%fptrToInt56792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49237 to i64
%ae49237 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56792)
store volatile %struct.ScmObj* %ae49237, %struct.ScmObj** %stackaddr$makeclosure56791, align 8
%argslist55209$ae492350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56793 = alloca %struct.ScmObj*, align 8
%argslist55209$ae492351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist55209$ae492350)
store volatile %struct.ScmObj* %argslist55209$ae492351, %struct.ScmObj** %stackaddr$prim56793, align 8
%stackaddr$prim56794 = alloca %struct.ScmObj*, align 8
%argslist55209$ae492352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist55209$ae492351)
store volatile %struct.ScmObj* %argslist55209$ae492352, %struct.ScmObj** %stackaddr$prim56794, align 8
%clofunc56795 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49235)
musttail call tailcc void %clofunc56795(%struct.ScmObj* %ae49235, %struct.ScmObj* %argslist55209$ae492352)
ret void
}

define tailcc void @proc_clo$ae49235(%struct.ScmObj* %env$ae49235,%struct.ScmObj* %current_45args55194) {
%stackaddr$env-ref56796 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56796
%stackaddr$env-ref56797 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56797
%stackaddr$env-ref56798 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56798
%stackaddr$env-ref56799 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 3)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56799
%stackaddr$env-ref56800 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56800
%stackaddr$env-ref56801 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56801
%stackaddr$env-ref56802 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 6)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56802
%stackaddr$prim56803 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55194)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim56803, align 8
%stackaddr$prim56804 = alloca %struct.ScmObj*, align 8
%current_45args55195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55194)
store volatile %struct.ScmObj* %current_45args55195, %struct.ScmObj** %stackaddr$prim56804, align 8
%stackaddr$prim56805 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55195)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim56805, align 8
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim56806, align 8
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48185)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$makeclosure56808 = alloca %struct.ScmObj*, align 8
%fptrToInt56809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49261 to i64
%ae49261 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56809)
store volatile %struct.ScmObj* %ae49261, %struct.ScmObj** %stackaddr$makeclosure56808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %anf_45bind48184, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %k48368, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49261, %struct.ScmObj* %vs48059, i64 4)
%stackaddr$prim56810 = alloca %struct.ScmObj*, align 8
%cpsargs48379 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49261, %struct.ScmObj* %anf_45bind48186)
store volatile %struct.ScmObj* %cpsargs48379, %struct.ScmObj** %stackaddr$prim56810, align 8
%clofunc56811 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56811(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48379)
ret void
}

define tailcc void @proc_clo$ae49261(%struct.ScmObj* %env$ae49261,%struct.ScmObj* %current_45args55197) {
%stackaddr$env-ref56812 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56812
%stackaddr$env-ref56813 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 1)
store %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$env-ref56813
%stackaddr$env-ref56814 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 2)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56814
%stackaddr$env-ref56815 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56815
%stackaddr$env-ref56816 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49261, i64 4)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56816
%stackaddr$prim56817 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55197)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim56817, align 8
%stackaddr$prim56818 = alloca %struct.ScmObj*, align 8
%current_45args55198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55197)
store volatile %struct.ScmObj* %current_45args55198, %struct.ScmObj** %stackaddr$prim56818, align 8
%stackaddr$prim56819 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55198)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim56819, align 8
%ae49266 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56820 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %ae49266)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim56820, align 8
%stackaddr$makeclosure56821 = alloca %struct.ScmObj*, align 8
%fptrToInt56822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49268 to i64
%ae49268 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56822)
store volatile %struct.ScmObj* %ae49268, %struct.ScmObj** %stackaddr$makeclosure56821, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49268, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49268, %struct.ScmObj* %k48368, i64 1)
%argslist55203$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%argslist55203$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist55203$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55203$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$prim56824 = alloca %struct.ScmObj*, align 8
%argslist55203$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist55203$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55203$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56824, align 8
%stackaddr$prim56825 = alloca %struct.ScmObj*, align 8
%argslist55203$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist55203$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55203$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56825, align 8
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%argslist55203$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist55203$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55203$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56826, align 8
%clofunc56827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56827(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55203$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49268(%struct.ScmObj* %env$ae49268,%struct.ScmObj* %current_45args55200) {
%stackaddr$env-ref56828 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49268, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56828
%stackaddr$env-ref56829 = alloca %struct.ScmObj*, align 8
%k48368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49268, i64 1)
store %struct.ScmObj* %k48368, %struct.ScmObj** %stackaddr$env-ref56829
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim56830, align 8
%stackaddr$prim56831 = alloca %struct.ScmObj*, align 8
%current_45args55201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %current_45args55201, %struct.ScmObj** %stackaddr$prim56831, align 8
%stackaddr$prim56832 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55201)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56832, align 8
%stackaddr$prim56833 = alloca %struct.ScmObj*, align 8
%cpsargs48378 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48368, %struct.ScmObj* %anf_45bind48189)
store volatile %struct.ScmObj* %cpsargs48378, %struct.ScmObj** %stackaddr$prim56833, align 8
%clofunc56834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc56834(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48378)
ret void
}

define tailcc void @proc_clo$ae49237(%struct.ScmObj* %env$ae49237,%struct.ScmObj* %current_45args55204) {
%stackaddr$prim56835 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55204)
store volatile %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$prim56835, align 8
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%current_45args55205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55204)
store volatile %struct.ScmObj* %current_45args55205, %struct.ScmObj** %stackaddr$prim56836, align 8
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55205)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56837, align 8
%stackaddr$prim56838 = alloca %struct.ScmObj*, align 8
%current_45args55206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55205)
store volatile %struct.ScmObj* %current_45args55206, %struct.ScmObj** %stackaddr$prim56838, align 8
%stackaddr$prim56839 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55206)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56839, align 8
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%cpsprim48381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48381, %struct.ScmObj** %stackaddr$prim56840, align 8
%ae49241 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55208$k483800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%argslist55208$k483801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48381, %struct.ScmObj* %argslist55208$k483800)
store volatile %struct.ScmObj* %argslist55208$k483801, %struct.ScmObj** %stackaddr$prim56841, align 8
%stackaddr$prim56842 = alloca %struct.ScmObj*, align 8
%argslist55208$k483802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49241, %struct.ScmObj* %argslist55208$k483801)
store volatile %struct.ScmObj* %argslist55208$k483802, %struct.ScmObj** %stackaddr$prim56842, align 8
%clofunc56843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48380)
musttail call tailcc void %clofunc56843(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist55208$k483802)
ret void
}

define tailcc void @proc_clo$ae49213(%struct.ScmObj* %env$ae49213,%struct.ScmObj* %current_45args55211) {
%stackaddr$prim56844 = alloca %struct.ScmObj*, align 8
%k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55211)
store volatile %struct.ScmObj* %k48382, %struct.ScmObj** %stackaddr$prim56844, align 8
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%current_45args55212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55211)
store volatile %struct.ScmObj* %current_45args55212, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55212)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%cpsprim48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48383, %struct.ScmObj** %stackaddr$prim56847, align 8
%ae49216 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55214$k483820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%argslist55214$k483821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48383, %struct.ScmObj* %argslist55214$k483820)
store volatile %struct.ScmObj* %argslist55214$k483821, %struct.ScmObj** %stackaddr$prim56848, align 8
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%argslist55214$k483822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49216, %struct.ScmObj* %argslist55214$k483821)
store volatile %struct.ScmObj* %argslist55214$k483822, %struct.ScmObj** %stackaddr$prim56849, align 8
%clofunc56850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48382)
musttail call tailcc void %clofunc56850(%struct.ScmObj* %k48382, %struct.ScmObj* %argslist55214$k483822)
ret void
}

define tailcc void @proc_clo$ae49189(%struct.ScmObj* %env$ae49189,%struct.ScmObj* %current_45args55217) {
%stackaddr$prim56851 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55217)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim56851, align 8
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%current_45args55218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55217)
store volatile %struct.ScmObj* %current_45args55218, %struct.ScmObj** %stackaddr$prim56852, align 8
%stackaddr$prim56853 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55218)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56853, align 8
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%cpsprim48385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48385, %struct.ScmObj** %stackaddr$prim56854, align 8
%ae49192 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55220$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56855 = alloca %struct.ScmObj*, align 8
%argslist55220$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48385, %struct.ScmObj* %argslist55220$k483840)
store volatile %struct.ScmObj* %argslist55220$k483841, %struct.ScmObj** %stackaddr$prim56855, align 8
%stackaddr$prim56856 = alloca %struct.ScmObj*, align 8
%argslist55220$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist55220$k483841)
store volatile %struct.ScmObj* %argslist55220$k483842, %struct.ScmObj** %stackaddr$prim56856, align 8
%clofunc56857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc56857(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist55220$k483842)
ret void
}

define tailcc void @proc_clo$ae49141(%struct.ScmObj* %env$ae49141,%struct.ScmObj* %current_45args55223) {
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim56858, align 8
%stackaddr$prim56859 = alloca %struct.ScmObj*, align 8
%current_45args55224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %current_45args55224, %struct.ScmObj** %stackaddr$prim56859, align 8
%stackaddr$prim56860 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56860, align 8
%stackaddr$prim56861 = alloca %struct.ScmObj*, align 8
%current_45args55225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %current_45args55225, %struct.ScmObj** %stackaddr$prim56861, align 8
%stackaddr$prim56862 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55225)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56862, align 8
%truthy$cmp56863 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56863 = icmp eq i64 %truthy$cmp56863, 1
br i1 %cmp$cmp56863, label %truebranch$cmp56863, label %falsebranch$cmp56863
truebranch$cmp56863:
%ae49144 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55227$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56864 = alloca %struct.ScmObj*, align 8
%argslist55227$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist55227$k483860)
store volatile %struct.ScmObj* %argslist55227$k483861, %struct.ScmObj** %stackaddr$prim56864, align 8
%stackaddr$prim56865 = alloca %struct.ScmObj*, align 8
%argslist55227$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49144, %struct.ScmObj* %argslist55227$k483861)
store volatile %struct.ScmObj* %argslist55227$k483862, %struct.ScmObj** %stackaddr$prim56865, align 8
%clofunc56866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc56866(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist55227$k483862)
ret void
falsebranch$cmp56863:
%stackaddr$prim56867 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim56867, align 8
%ae49151 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55228$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56868 = alloca %struct.ScmObj*, align 8
%argslist55228$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist55228$k483860)
store volatile %struct.ScmObj* %argslist55228$k483861, %struct.ScmObj** %stackaddr$prim56868, align 8
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%argslist55228$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49151, %struct.ScmObj* %argslist55228$k483861)
store volatile %struct.ScmObj* %argslist55228$k483862, %struct.ScmObj** %stackaddr$prim56869, align 8
%clofunc56870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc56870(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist55228$k483862)
ret void
}

define tailcc void @proc_clo$ae49098(%struct.ScmObj* %env$ae49098,%struct.ScmObj* %current_45args55232) {
%stackaddr$env-ref56871 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49098, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56871
%stackaddr$env-ref56872 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49098, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56872
%stackaddr$prim56873 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55232)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim56873, align 8
%stackaddr$prim56874 = alloca %struct.ScmObj*, align 8
%current_45args55233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55232)
store volatile %struct.ScmObj* %current_45args55233, %struct.ScmObj** %stackaddr$prim56874, align 8
%stackaddr$prim56875 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55233)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56875, align 8
%stackaddr$prim56876 = alloca %struct.ScmObj*, align 8
%current_45args55234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55233)
store volatile %struct.ScmObj* %current_45args55234, %struct.ScmObj** %stackaddr$prim56876, align 8
%stackaddr$prim56877 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55234)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56877, align 8
%stackaddr$makeclosure56878 = alloca %struct.ScmObj*, align 8
%fptrToInt56879 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49100 to i64
%ae49100 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56879)
store volatile %struct.ScmObj* %ae49100, %struct.ScmObj** %stackaddr$makeclosure56878, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %k48388, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %lst48067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %n48066, i64 3)
%argslist55240$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56880 = alloca %struct.ScmObj*, align 8
%argslist55240$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist55240$_37length480350)
store volatile %struct.ScmObj* %argslist55240$_37length480351, %struct.ScmObj** %stackaddr$prim56880, align 8
%stackaddr$prim56881 = alloca %struct.ScmObj*, align 8
%argslist55240$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49100, %struct.ScmObj* %argslist55240$_37length480351)
store volatile %struct.ScmObj* %argslist55240$_37length480352, %struct.ScmObj** %stackaddr$prim56881, align 8
%clofunc56882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56882(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist55240$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49100(%struct.ScmObj* %env$ae49100,%struct.ScmObj* %current_45args55236) {
%stackaddr$env-ref56883 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56883
%stackaddr$env-ref56884 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 1)
store %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$env-ref56884
%stackaddr$env-ref56885 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 2)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56885
%stackaddr$env-ref56886 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 3)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56886
%stackaddr$prim56887 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim56887, align 8
%stackaddr$prim56888 = alloca %struct.ScmObj*, align 8
%current_45args55237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %current_45args55237, %struct.ScmObj** %stackaddr$prim56888, align 8
%stackaddr$prim56889 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55237)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim56889, align 8
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim56890, align 8
%argslist55239$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56891 = alloca %struct.ScmObj*, align 8
%argslist55239$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist55239$_37take480380)
store volatile %struct.ScmObj* %argslist55239$_37take480381, %struct.ScmObj** %stackaddr$prim56891, align 8
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%argslist55239$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist55239$_37take480381)
store volatile %struct.ScmObj* %argslist55239$_37take480382, %struct.ScmObj** %stackaddr$prim56892, align 8
%stackaddr$prim56893 = alloca %struct.ScmObj*, align 8
%argslist55239$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist55239$_37take480382)
store volatile %struct.ScmObj* %argslist55239$_37take480383, %struct.ScmObj** %stackaddr$prim56893, align 8
%clofunc56894 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc56894(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist55239$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49044(%struct.ScmObj* %env$ae49044,%struct.ScmObj* %current_45args55242) {
%stackaddr$env-ref56895 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56895
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55242)
store volatile %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$prim56896, align 8
%stackaddr$prim56897 = alloca %struct.ScmObj*, align 8
%current_45args55243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55242)
store volatile %struct.ScmObj* %current_45args55243, %struct.ScmObj** %stackaddr$prim56897, align 8
%stackaddr$prim56898 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim56898, align 8
%stackaddr$makeclosure56899 = alloca %struct.ScmObj*, align 8
%fptrToInt56900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49045 to i64
%ae49045 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56900)
store volatile %struct.ScmObj* %ae49045, %struct.ScmObj** %stackaddr$makeclosure56899, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49045, %struct.ScmObj* %k48390, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49045, %struct.ScmObj* %lst48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49045, %struct.ScmObj* %_37foldl148030, i64 2)
%ae49046 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56901 = alloca %struct.ScmObj*, align 8
%fptrToInt56902 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49047 to i64
%ae49047 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56902)
store volatile %struct.ScmObj* %ae49047, %struct.ScmObj** %stackaddr$makeclosure56901, align 8
%argslist55254$ae490450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56903 = alloca %struct.ScmObj*, align 8
%argslist55254$ae490451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49047, %struct.ScmObj* %argslist55254$ae490450)
store volatile %struct.ScmObj* %argslist55254$ae490451, %struct.ScmObj** %stackaddr$prim56903, align 8
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%argslist55254$ae490452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49046, %struct.ScmObj* %argslist55254$ae490451)
store volatile %struct.ScmObj* %argslist55254$ae490452, %struct.ScmObj** %stackaddr$prim56904, align 8
%clofunc56905 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49045)
musttail call tailcc void %clofunc56905(%struct.ScmObj* %ae49045, %struct.ScmObj* %argslist55254$ae490452)
ret void
}

define tailcc void @proc_clo$ae49045(%struct.ScmObj* %env$ae49045,%struct.ScmObj* %current_45args55245) {
%stackaddr$env-ref56906 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49045, i64 0)
store %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$env-ref56906
%stackaddr$env-ref56907 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49045, i64 1)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref56907
%stackaddr$env-ref56908 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49045, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56908
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55245)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim56909, align 8
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%current_45args55246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55245)
store volatile %struct.ScmObj* %current_45args55246, %struct.ScmObj** %stackaddr$prim56910, align 8
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55246)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim56911, align 8
%ae49066 = call %struct.ScmObj* @const_init_null()
%argslist55248$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%argslist55248$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist55248$_37foldl1480300)
store volatile %struct.ScmObj* %argslist55248$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56912, align 8
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%argslist55248$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49066, %struct.ScmObj* %argslist55248$_37foldl1480301)
store volatile %struct.ScmObj* %argslist55248$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%argslist55248$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist55248$_37foldl1480302)
store volatile %struct.ScmObj* %argslist55248$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%argslist55248$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist55248$_37foldl1480303)
store volatile %struct.ScmObj* %argslist55248$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56915, align 8
%clofunc56916 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56916(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist55248$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49047(%struct.ScmObj* %env$ae49047,%struct.ScmObj* %current_45args55249) {
%stackaddr$prim56917 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim56917, align 8
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%current_45args55250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %current_45args55250, %struct.ScmObj** %stackaddr$prim56918, align 8
%stackaddr$prim56919 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55250)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim56919, align 8
%stackaddr$prim56920 = alloca %struct.ScmObj*, align 8
%current_45args55251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55250)
store volatile %struct.ScmObj* %current_45args55251, %struct.ScmObj** %stackaddr$prim56920, align 8
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim56921, align 8
%ae49049 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55253$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56922 = alloca %struct.ScmObj*, align 8
%argslist55253$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist55253$k483920)
store volatile %struct.ScmObj* %argslist55253$k483921, %struct.ScmObj** %stackaddr$prim56922, align 8
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%argslist55253$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49049, %struct.ScmObj* %argslist55253$k483921)
store volatile %struct.ScmObj* %argslist55253$k483922, %struct.ScmObj** %stackaddr$prim56923, align 8
%clofunc56924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc56924(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55253$k483922)
ret void
}

define tailcc void @proc_clo$ae48965(%struct.ScmObj* %env$ae48965,%struct.ScmObj* %current_45args55257) {
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %k48393, %struct.ScmObj** %stackaddr$prim56925, align 8
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%current_45args55258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %current_45args55258, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55258)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim56927, align 8
%ae48967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56928 = alloca %struct.ScmObj*, align 8
%fptrToInt56929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48968 to i64
%ae48968 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56929)
store volatile %struct.ScmObj* %ae48968, %struct.ScmObj** %stackaddr$makeclosure56928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48968, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist55271$k483930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56930 = alloca %struct.ScmObj*, align 8
%argslist55271$k483931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48968, %struct.ScmObj* %argslist55271$k483930)
store volatile %struct.ScmObj* %argslist55271$k483931, %struct.ScmObj** %stackaddr$prim56930, align 8
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%argslist55271$k483932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48967, %struct.ScmObj* %argslist55271$k483931)
store volatile %struct.ScmObj* %argslist55271$k483932, %struct.ScmObj** %stackaddr$prim56931, align 8
%clofunc56932 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48393)
musttail call tailcc void %clofunc56932(%struct.ScmObj* %k48393, %struct.ScmObj* %argslist55271$k483932)
ret void
}

define tailcc void @proc_clo$ae48968(%struct.ScmObj* %env$ae48968,%struct.ScmObj* %current_45args55260) {
%stackaddr$env-ref56933 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48968, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56933
%stackaddr$prim56934 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim56934, align 8
%stackaddr$prim56935 = alloca %struct.ScmObj*, align 8
%current_45args55261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %current_45args55261, %struct.ScmObj** %stackaddr$prim56935, align 8
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55261)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim56936, align 8
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%current_45args55262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55261)
store volatile %struct.ScmObj* %current_45args55262, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55262)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim56938, align 8
%stackaddr$prim56939 = alloca %struct.ScmObj*, align 8
%current_45args55263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55262)
store volatile %struct.ScmObj* %current_45args55263, %struct.ScmObj** %stackaddr$prim56939, align 8
%stackaddr$prim56940 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55263)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim56940, align 8
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim56941, align 8
%truthy$cmp56942 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48170)
%cmp$cmp56942 = icmp eq i64 %truthy$cmp56942, 1
br i1 %cmp$cmp56942, label %truebranch$cmp56942, label %falsebranch$cmp56942
truebranch$cmp56942:
%ae48972 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55265$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%argslist55265$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist55265$k483940)
store volatile %struct.ScmObj* %argslist55265$k483941, %struct.ScmObj** %stackaddr$prim56943, align 8
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%argslist55265$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48972, %struct.ScmObj* %argslist55265$k483941)
store volatile %struct.ScmObj* %argslist55265$k483942, %struct.ScmObj** %stackaddr$prim56944, align 8
%clofunc56945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc56945(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist55265$k483942)
ret void
falsebranch$cmp56942:
%stackaddr$prim56946 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim56946, align 8
%stackaddr$makeclosure56947 = alloca %struct.ScmObj*, align 8
%fptrToInt56948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48979 to i64
%ae48979 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56948)
store volatile %struct.ScmObj* %ae48979, %struct.ScmObj** %stackaddr$makeclosure56947, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48979, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48979, %struct.ScmObj* %lst48032, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48979, %struct.ScmObj* %_37foldl148031, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48979, %struct.ScmObj* %k48394, i64 3)
%argslist55270$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%argslist55270$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist55270$f480340)
store volatile %struct.ScmObj* %argslist55270$f480341, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%argslist55270$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist55270$f480341)
store volatile %struct.ScmObj* %argslist55270$f480342, %struct.ScmObj** %stackaddr$prim56950, align 8
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%argslist55270$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48979, %struct.ScmObj* %argslist55270$f480342)
store volatile %struct.ScmObj* %argslist55270$f480343, %struct.ScmObj** %stackaddr$prim56951, align 8
%clofunc56952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc56952(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist55270$f480343)
ret void
}

define tailcc void @proc_clo$ae48979(%struct.ScmObj* %env$ae48979,%struct.ScmObj* %current_45args55266) {
%stackaddr$env-ref56953 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48979, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref56953
%stackaddr$env-ref56954 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48979, i64 1)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref56954
%stackaddr$env-ref56955 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48979, i64 2)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref56955
%stackaddr$env-ref56956 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48979, i64 3)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56956
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55266)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim56957, align 8
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%current_45args55267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55266)
store volatile %struct.ScmObj* %current_45args55267, %struct.ScmObj** %stackaddr$prim56958, align 8
%stackaddr$prim56959 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55267)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim56959, align 8
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56960, align 8
%argslist55269$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%argslist55269$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist55269$_37foldl1480310)
store volatile %struct.ScmObj* %argslist55269$_37foldl1480311, %struct.ScmObj** %stackaddr$prim56961, align 8
%stackaddr$prim56962 = alloca %struct.ScmObj*, align 8
%argslist55269$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist55269$_37foldl1480311)
store volatile %struct.ScmObj* %argslist55269$_37foldl1480312, %struct.ScmObj** %stackaddr$prim56962, align 8
%stackaddr$prim56963 = alloca %struct.ScmObj*, align 8
%argslist55269$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist55269$_37foldl1480312)
store volatile %struct.ScmObj* %argslist55269$_37foldl1480313, %struct.ScmObj** %stackaddr$prim56963, align 8
%stackaddr$prim56964 = alloca %struct.ScmObj*, align 8
%argslist55269$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist55269$_37foldl1480313)
store volatile %struct.ScmObj* %argslist55269$_37foldl1480314, %struct.ScmObj** %stackaddr$prim56964, align 8
%clofunc56965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc56965(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist55269$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48882(%struct.ScmObj* %env$ae48882,%struct.ScmObj* %current_45args55274) {
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55274)
store volatile %struct.ScmObj* %k48396, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%current_45args55275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55274)
store volatile %struct.ScmObj* %current_45args55275, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55275)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim56968, align 8
%ae48884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56969 = alloca %struct.ScmObj*, align 8
%fptrToInt56970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48885 to i64
%ae48885 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56970)
store volatile %struct.ScmObj* %ae48885, %struct.ScmObj** %stackaddr$makeclosure56969, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48885, %struct.ScmObj* %_37length48036, i64 0)
%argslist55286$k483960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56971 = alloca %struct.ScmObj*, align 8
%argslist55286$k483961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist55286$k483960)
store volatile %struct.ScmObj* %argslist55286$k483961, %struct.ScmObj** %stackaddr$prim56971, align 8
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%argslist55286$k483962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist55286$k483961)
store volatile %struct.ScmObj* %argslist55286$k483962, %struct.ScmObj** %stackaddr$prim56972, align 8
%clofunc56973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48396)
musttail call tailcc void %clofunc56973(%struct.ScmObj* %k48396, %struct.ScmObj* %argslist55286$k483962)
ret void
}

define tailcc void @proc_clo$ae48885(%struct.ScmObj* %env$ae48885,%struct.ScmObj* %current_45args55277) {
%stackaddr$env-ref56974 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48885, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref56974
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim56975, align 8
%stackaddr$prim56976 = alloca %struct.ScmObj*, align 8
%current_45args55278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %current_45args55278, %struct.ScmObj** %stackaddr$prim56976, align 8
%stackaddr$prim56977 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55278)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim56977, align 8
%stackaddr$prim56978 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim56978, align 8
%truthy$cmp56979 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48166)
%cmp$cmp56979 = icmp eq i64 %truthy$cmp56979, 1
br i1 %cmp$cmp56979, label %truebranch$cmp56979, label %falsebranch$cmp56979
truebranch$cmp56979:
%ae48889 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55280$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%argslist55280$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48890, %struct.ScmObj* %argslist55280$k483970)
store volatile %struct.ScmObj* %argslist55280$k483971, %struct.ScmObj** %stackaddr$prim56980, align 8
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%argslist55280$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist55280$k483971)
store volatile %struct.ScmObj* %argslist55280$k483972, %struct.ScmObj** %stackaddr$prim56981, align 8
%clofunc56982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56982(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55280$k483972)
ret void
falsebranch$cmp56979:
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim56983, align 8
%stackaddr$makeclosure56984 = alloca %struct.ScmObj*, align 8
%fptrToInt56985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48899 to i64
%ae48899 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56985)
store volatile %struct.ScmObj* %ae48899, %struct.ScmObj** %stackaddr$makeclosure56984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48899, %struct.ScmObj* %k48397, i64 0)
%argslist55285$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%argslist55285$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist55285$_37length480360)
store volatile %struct.ScmObj* %argslist55285$_37length480361, %struct.ScmObj** %stackaddr$prim56986, align 8
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%argslist55285$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist55285$_37length480361)
store volatile %struct.ScmObj* %argslist55285$_37length480362, %struct.ScmObj** %stackaddr$prim56987, align 8
%clofunc56988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc56988(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist55285$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48899(%struct.ScmObj* %env$ae48899,%struct.ScmObj* %current_45args55281) {
%stackaddr$env-ref56989 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48899, i64 0)
store %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$env-ref56989
%stackaddr$prim56990 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55281)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim56990, align 8
%stackaddr$prim56991 = alloca %struct.ScmObj*, align 8
%current_45args55282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55281)
store volatile %struct.ScmObj* %current_45args55282, %struct.ScmObj** %stackaddr$prim56991, align 8
%stackaddr$prim56992 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55282)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56992, align 8
%ae48901 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%cpsprim48399 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48901, %struct.ScmObj* %anf_45bind48168)
store volatile %struct.ScmObj* %cpsprim48399, %struct.ScmObj** %stackaddr$prim56993, align 8
%ae48904 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55284$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%argslist55284$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48399, %struct.ScmObj* %argslist55284$k483970)
store volatile %struct.ScmObj* %argslist55284$k483971, %struct.ScmObj** %stackaddr$prim56994, align 8
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%argslist55284$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist55284$k483971)
store volatile %struct.ScmObj* %argslist55284$k483972, %struct.ScmObj** %stackaddr$prim56995, align 8
%clofunc56996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc56996(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist55284$k483972)
ret void
}

define tailcc void @proc_clo$ae48732(%struct.ScmObj* %env$ae48732,%struct.ScmObj* %current_45args55289) {
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55289)
store volatile %struct.ScmObj* %k48400, %struct.ScmObj** %stackaddr$prim56997, align 8
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%current_45args55290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55289)
store volatile %struct.ScmObj* %current_45args55290, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55290)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim56999, align 8
%ae48734 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57000 = alloca %struct.ScmObj*, align 8
%fptrToInt57001 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48735 to i64
%ae48735 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57001)
store volatile %struct.ScmObj* %ae48735, %struct.ScmObj** %stackaddr$makeclosure57000, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48735, %struct.ScmObj* %_37take48039, i64 0)
%argslist55303$k484000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%argslist55303$k484001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48735, %struct.ScmObj* %argslist55303$k484000)
store volatile %struct.ScmObj* %argslist55303$k484001, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%argslist55303$k484002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48734, %struct.ScmObj* %argslist55303$k484001)
store volatile %struct.ScmObj* %argslist55303$k484002, %struct.ScmObj** %stackaddr$prim57003, align 8
%clofunc57004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48400)
musttail call tailcc void %clofunc57004(%struct.ScmObj* %k48400, %struct.ScmObj* %argslist55303$k484002)
ret void
}

define tailcc void @proc_clo$ae48735(%struct.ScmObj* %env$ae48735,%struct.ScmObj* %current_45args55292) {
%stackaddr$env-ref57005 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48735, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref57005
%stackaddr$prim57006 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55292)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim57006, align 8
%stackaddr$prim57007 = alloca %struct.ScmObj*, align 8
%current_45args55293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55292)
store volatile %struct.ScmObj* %current_45args55293, %struct.ScmObj** %stackaddr$prim57007, align 8
%stackaddr$prim57008 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55293)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim57008, align 8
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%current_45args55294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55293)
store volatile %struct.ScmObj* %current_45args55294, %struct.ScmObj** %stackaddr$prim57009, align 8
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55294)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim57010, align 8
%ae48737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57011 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48737)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim57011, align 8
%truthy$cmp57012 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48159)
%cmp$cmp57012 = icmp eq i64 %truthy$cmp57012, 1
br i1 %cmp$cmp57012, label %truebranch$cmp57012, label %falsebranch$cmp57012
truebranch$cmp57012:
%ae48740 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48741 = call %struct.ScmObj* @const_init_null()
%argslist55296$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57013 = alloca %struct.ScmObj*, align 8
%argslist55296$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48741, %struct.ScmObj* %argslist55296$k484010)
store volatile %struct.ScmObj* %argslist55296$k484011, %struct.ScmObj** %stackaddr$prim57013, align 8
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%argslist55296$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48740, %struct.ScmObj* %argslist55296$k484011)
store volatile %struct.ScmObj* %argslist55296$k484012, %struct.ScmObj** %stackaddr$prim57014, align 8
%clofunc57015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc57015(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist55296$k484012)
ret void
falsebranch$cmp57012:
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim57016, align 8
%truthy$cmp57017 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48160)
%cmp$cmp57017 = icmp eq i64 %truthy$cmp57017, 1
br i1 %cmp$cmp57017, label %truebranch$cmp57017, label %falsebranch$cmp57017
truebranch$cmp57017:
%ae48751 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48752 = call %struct.ScmObj* @const_init_null()
%argslist55297$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57018 = alloca %struct.ScmObj*, align 8
%argslist55297$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48752, %struct.ScmObj* %argslist55297$k484010)
store volatile %struct.ScmObj* %argslist55297$k484011, %struct.ScmObj** %stackaddr$prim57018, align 8
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%argslist55297$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48751, %struct.ScmObj* %argslist55297$k484011)
store volatile %struct.ScmObj* %argslist55297$k484012, %struct.ScmObj** %stackaddr$prim57019, align 8
%clofunc57020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc57020(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist55297$k484012)
ret void
falsebranch$cmp57017:
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim57021, align 8
%stackaddr$prim57022 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim57022, align 8
%ae48762 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57023 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48762)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim57023, align 8
%stackaddr$makeclosure57024 = alloca %struct.ScmObj*, align 8
%fptrToInt57025 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48764 to i64
%ae48764 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57025)
store volatile %struct.ScmObj* %ae48764, %struct.ScmObj** %stackaddr$makeclosure57024, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48764, %struct.ScmObj* %k48401, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48764, %struct.ScmObj* %anf_45bind48161, i64 1)
%argslist55302$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%argslist55302$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist55302$_37take480390)
store volatile %struct.ScmObj* %argslist55302$_37take480391, %struct.ScmObj** %stackaddr$prim57026, align 8
%stackaddr$prim57027 = alloca %struct.ScmObj*, align 8
%argslist55302$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %argslist55302$_37take480391)
store volatile %struct.ScmObj* %argslist55302$_37take480392, %struct.ScmObj** %stackaddr$prim57027, align 8
%stackaddr$prim57028 = alloca %struct.ScmObj*, align 8
%argslist55302$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48764, %struct.ScmObj* %argslist55302$_37take480392)
store volatile %struct.ScmObj* %argslist55302$_37take480393, %struct.ScmObj** %stackaddr$prim57028, align 8
%clofunc57029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc57029(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist55302$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48764(%struct.ScmObj* %env$ae48764,%struct.ScmObj* %current_45args55298) {
%stackaddr$env-ref57030 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48764, i64 0)
store %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$env-ref57030
%stackaddr$env-ref57031 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48764, i64 1)
store %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$env-ref57031
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%current_45args55299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %current_45args55299, %struct.ScmObj** %stackaddr$prim57033, align 8
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$prim57035 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %anf_45bind48164)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim57035, align 8
%ae48770 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55301$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57036 = alloca %struct.ScmObj*, align 8
%argslist55301$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist55301$k484010)
store volatile %struct.ScmObj* %argslist55301$k484011, %struct.ScmObj** %stackaddr$prim57036, align 8
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%argslist55301$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48770, %struct.ScmObj* %argslist55301$k484011)
store volatile %struct.ScmObj* %argslist55301$k484012, %struct.ScmObj** %stackaddr$prim57037, align 8
%clofunc57038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc57038(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist55301$k484012)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %current_45args55306) {
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %k48404, %struct.ScmObj** %stackaddr$prim57039, align 8
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%current_45args55307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %current_45args55307, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55307)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim57041, align 8
%ae48637 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57042 = alloca %struct.ScmObj*, align 8
%fptrToInt57043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48638 to i64
%ae48638 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57043)
store volatile %struct.ScmObj* %ae48638, %struct.ScmObj** %stackaddr$makeclosure57042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %_37map48043, i64 0)
%argslist55323$k484040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57044 = alloca %struct.ScmObj*, align 8
%argslist55323$k484041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist55323$k484040)
store volatile %struct.ScmObj* %argslist55323$k484041, %struct.ScmObj** %stackaddr$prim57044, align 8
%stackaddr$prim57045 = alloca %struct.ScmObj*, align 8
%argslist55323$k484042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48637, %struct.ScmObj* %argslist55323$k484041)
store volatile %struct.ScmObj* %argslist55323$k484042, %struct.ScmObj** %stackaddr$prim57045, align 8
%clofunc57046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48404)
musttail call tailcc void %clofunc57046(%struct.ScmObj* %k48404, %struct.ScmObj* %argslist55323$k484042)
ret void
}

define tailcc void @proc_clo$ae48638(%struct.ScmObj* %env$ae48638,%struct.ScmObj* %current_45args55309) {
%stackaddr$env-ref57047 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref57047
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$prim57048, align 8
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%current_45args55310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %current_45args55310, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55310)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim57050, align 8
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%current_45args55311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55310)
store volatile %struct.ScmObj* %current_45args55311, %struct.ScmObj** %stackaddr$prim57051, align 8
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55311)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim57053, align 8
%truthy$cmp57054 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48153)
%cmp$cmp57054 = icmp eq i64 %truthy$cmp57054, 1
br i1 %cmp$cmp57054, label %truebranch$cmp57054, label %falsebranch$cmp57054
truebranch$cmp57054:
%ae48642 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48643 = call %struct.ScmObj* @const_init_null()
%argslist55313$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%argslist55313$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48643, %struct.ScmObj* %argslist55313$k484050)
store volatile %struct.ScmObj* %argslist55313$k484051, %struct.ScmObj** %stackaddr$prim57055, align 8
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%argslist55313$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48642, %struct.ScmObj* %argslist55313$k484051)
store volatile %struct.ScmObj* %argslist55313$k484052, %struct.ScmObj** %stackaddr$prim57056, align 8
%clofunc57057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc57057(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist55313$k484052)
ret void
falsebranch$cmp57054:
%stackaddr$prim57058 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim57058, align 8
%stackaddr$makeclosure57059 = alloca %struct.ScmObj*, align 8
%fptrToInt57060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48652 to i64
%ae48652 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57060)
store volatile %struct.ScmObj* %ae48652, %struct.ScmObj** %stackaddr$makeclosure57059, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %k48405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %f48045, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %lst48044, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %_37map48043, i64 3)
%argslist55322$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57061 = alloca %struct.ScmObj*, align 8
%argslist55322$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist55322$f480450)
store volatile %struct.ScmObj* %argslist55322$f480451, %struct.ScmObj** %stackaddr$prim57061, align 8
%stackaddr$prim57062 = alloca %struct.ScmObj*, align 8
%argslist55322$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48652, %struct.ScmObj* %argslist55322$f480451)
store volatile %struct.ScmObj* %argslist55322$f480452, %struct.ScmObj** %stackaddr$prim57062, align 8
%clofunc57063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc57063(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist55322$f480452)
ret void
}

define tailcc void @proc_clo$ae48652(%struct.ScmObj* %env$ae48652,%struct.ScmObj* %current_45args55314) {
%stackaddr$env-ref57064 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 0)
store %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$env-ref57064
%stackaddr$env-ref57065 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 1)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref57065
%stackaddr$env-ref57066 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 2)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref57066
%stackaddr$env-ref57067 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 3)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref57067
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55314)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim57068, align 8
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%current_45args55315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55314)
store volatile %struct.ScmObj* %current_45args55315, %struct.ScmObj** %stackaddr$prim57069, align 8
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55315)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim57070, align 8
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim57071, align 8
%stackaddr$makeclosure57072 = alloca %struct.ScmObj*, align 8
%fptrToInt57073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48656 to i64
%ae48656 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57073)
store volatile %struct.ScmObj* %ae48656, %struct.ScmObj** %stackaddr$makeclosure57072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %k48405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %anf_45bind48155, i64 1)
%argslist55321$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%argslist55321$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist55321$_37map480430)
store volatile %struct.ScmObj* %argslist55321$_37map480431, %struct.ScmObj** %stackaddr$prim57074, align 8
%stackaddr$prim57075 = alloca %struct.ScmObj*, align 8
%argslist55321$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist55321$_37map480431)
store volatile %struct.ScmObj* %argslist55321$_37map480432, %struct.ScmObj** %stackaddr$prim57075, align 8
%stackaddr$prim57076 = alloca %struct.ScmObj*, align 8
%argslist55321$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist55321$_37map480432)
store volatile %struct.ScmObj* %argslist55321$_37map480433, %struct.ScmObj** %stackaddr$prim57076, align 8
%clofunc57077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc57077(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist55321$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48656(%struct.ScmObj* %env$ae48656,%struct.ScmObj* %current_45args55317) {
%stackaddr$env-ref57078 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 0)
store %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$env-ref57078
%stackaddr$env-ref57079 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 1)
store %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$env-ref57079
%stackaddr$prim57080 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55317)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim57080, align 8
%stackaddr$prim57081 = alloca %struct.ScmObj*, align 8
%current_45args55318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55317)
store volatile %struct.ScmObj* %current_45args55318, %struct.ScmObj** %stackaddr$prim57081, align 8
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55318)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%cpsprim48408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %anf_45bind48157)
store volatile %struct.ScmObj* %cpsprim48408, %struct.ScmObj** %stackaddr$prim57083, align 8
%ae48662 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55320$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%argslist55320$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48408, %struct.ScmObj* %argslist55320$k484050)
store volatile %struct.ScmObj* %argslist55320$k484051, %struct.ScmObj** %stackaddr$prim57084, align 8
%stackaddr$prim57085 = alloca %struct.ScmObj*, align 8
%argslist55320$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist55320$k484051)
store volatile %struct.ScmObj* %argslist55320$k484052, %struct.ScmObj** %stackaddr$prim57085, align 8
%clofunc57086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc57086(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist55320$k484052)
ret void
}

define tailcc void @proc_clo$ae48555(%struct.ScmObj* %env$ae48555,%struct.ScmObj* %current_45args55326) {
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55326)
store volatile %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$prim57087, align 8
%stackaddr$prim57088 = alloca %struct.ScmObj*, align 8
%current_45args55327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55326)
store volatile %struct.ScmObj* %current_45args55327, %struct.ScmObj** %stackaddr$prim57088, align 8
%stackaddr$prim57089 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55327)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim57089, align 8
%ae48557 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57090 = alloca %struct.ScmObj*, align 8
%fptrToInt57091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48558 to i64
%ae48558 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57091)
store volatile %struct.ScmObj* %ae48558, %struct.ScmObj** %stackaddr$makeclosure57090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48558, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist55340$k484090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%argslist55340$k484091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48558, %struct.ScmObj* %argslist55340$k484090)
store volatile %struct.ScmObj* %argslist55340$k484091, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%argslist55340$k484092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist55340$k484091)
store volatile %struct.ScmObj* %argslist55340$k484092, %struct.ScmObj** %stackaddr$prim57093, align 8
%clofunc57094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48409)
musttail call tailcc void %clofunc57094(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist55340$k484092)
ret void
}

define tailcc void @proc_clo$ae48558(%struct.ScmObj* %env$ae48558,%struct.ScmObj* %current_45args55329) {
%stackaddr$env-ref57095 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48558, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57095
%stackaddr$prim57096 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim57096, align 8
%stackaddr$prim57097 = alloca %struct.ScmObj*, align 8
%current_45args55330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %current_45args55330, %struct.ScmObj** %stackaddr$prim57097, align 8
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55330)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim57098, align 8
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%current_45args55331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55330)
store volatile %struct.ScmObj* %current_45args55331, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55331)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim57100, align 8
%stackaddr$prim57101 = alloca %struct.ScmObj*, align 8
%current_45args55332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55331)
store volatile %struct.ScmObj* %current_45args55332, %struct.ScmObj** %stackaddr$prim57101, align 8
%stackaddr$prim57102 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55332)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim57102, align 8
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$prim57103, align 8
%truthy$cmp57104 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48148)
%cmp$cmp57104 = icmp eq i64 %truthy$cmp57104, 1
br i1 %cmp$cmp57104, label %truebranch$cmp57104, label %falsebranch$cmp57104
truebranch$cmp57104:
%ae48562 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55334$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%argslist55334$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist55334$k484100)
store volatile %struct.ScmObj* %argslist55334$k484101, %struct.ScmObj** %stackaddr$prim57105, align 8
%stackaddr$prim57106 = alloca %struct.ScmObj*, align 8
%argslist55334$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48562, %struct.ScmObj* %argslist55334$k484101)
store volatile %struct.ScmObj* %argslist55334$k484102, %struct.ScmObj** %stackaddr$prim57106, align 8
%clofunc57107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc57107(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist55334$k484102)
ret void
falsebranch$cmp57104:
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim57109, align 8
%stackaddr$makeclosure57110 = alloca %struct.ScmObj*, align 8
%fptrToInt57111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48570 to i64
%ae48570 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57111)
store volatile %struct.ScmObj* %ae48570, %struct.ScmObj** %stackaddr$makeclosure57110, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %anf_45bind48149, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %f48050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48570, %struct.ScmObj* %k48410, i64 2)
%argslist55339$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%argslist55339$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %argslist55339$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55339$_37foldr1480471, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%argslist55339$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist55339$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55339$_37foldr1480472, %struct.ScmObj** %stackaddr$prim57113, align 8
%stackaddr$prim57114 = alloca %struct.ScmObj*, align 8
%argslist55339$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist55339$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55339$_37foldr1480473, %struct.ScmObj** %stackaddr$prim57114, align 8
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%argslist55339$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48570, %struct.ScmObj* %argslist55339$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55339$_37foldr1480474, %struct.ScmObj** %stackaddr$prim57115, align 8
%clofunc57116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc57116(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55339$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48570(%struct.ScmObj* %env$ae48570,%struct.ScmObj* %current_45args55335) {
%stackaddr$env-ref57117 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 0)
store %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$env-ref57117
%stackaddr$env-ref57118 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 1)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref57118
%stackaddr$env-ref57119 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48570, i64 2)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref57119
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55335)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim57120, align 8
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%current_45args55336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55335)
store volatile %struct.ScmObj* %current_45args55336, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55336)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim57122, align 8
%argslist55338$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57123 = alloca %struct.ScmObj*, align 8
%argslist55338$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist55338$f480500)
store volatile %struct.ScmObj* %argslist55338$f480501, %struct.ScmObj** %stackaddr$prim57123, align 8
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%argslist55338$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48149, %struct.ScmObj* %argslist55338$f480501)
store volatile %struct.ScmObj* %argslist55338$f480502, %struct.ScmObj** %stackaddr$prim57124, align 8
%stackaddr$prim57125 = alloca %struct.ScmObj*, align 8
%argslist55338$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist55338$f480502)
store volatile %struct.ScmObj* %argslist55338$f480503, %struct.ScmObj** %stackaddr$prim57125, align 8
%clofunc57126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc57126(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist55338$f480503)
ret void
}

define tailcc void @proc_clo$ae48438(%struct.ScmObj* %env$ae48438,%struct.ScmObj* %current_45args55343) {
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55343)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%current_45args55344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55343)
store volatile %struct.ScmObj* %current_45args55344, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55344)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim57129, align 8
%ae48440 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57130 = alloca %struct.ScmObj*, align 8
%fptrToInt57131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48441 to i64
%ae48441 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57131)
store volatile %struct.ScmObj* %ae48441, %struct.ScmObj** %stackaddr$makeclosure57130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48441, %struct.ScmObj* %y48027, i64 0)
%argslist55362$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%argslist55362$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48441, %struct.ScmObj* %argslist55362$k484120)
store volatile %struct.ScmObj* %argslist55362$k484121, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%argslist55362$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48440, %struct.ScmObj* %argslist55362$k484121)
store volatile %struct.ScmObj* %argslist55362$k484122, %struct.ScmObj** %stackaddr$prim57133, align 8
%clofunc57134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc57134(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist55362$k484122)
ret void
}

define tailcc void @proc_clo$ae48441(%struct.ScmObj* %env$ae48441,%struct.ScmObj* %current_45args55346) {
%stackaddr$env-ref57135 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48441, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref57135
%stackaddr$prim57136 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55346)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim57136, align 8
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%current_45args55347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55346)
store volatile %struct.ScmObj* %current_45args55347, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55347)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$makeclosure57139 = alloca %struct.ScmObj*, align 8
%fptrToInt57140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48442 to i64
%ae48442 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57140)
store volatile %struct.ScmObj* %ae48442, %struct.ScmObj** %stackaddr$makeclosure57139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48442, %struct.ScmObj* %k48413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48442, %struct.ScmObj* %f48028, i64 1)
%ae48443 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57141 = alloca %struct.ScmObj*, align 8
%fptrToInt57142 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48444 to i64
%ae48444 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57142)
store volatile %struct.ScmObj* %ae48444, %struct.ScmObj** %stackaddr$makeclosure57141, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48444, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48444, %struct.ScmObj* %y48027, i64 1)
%argslist55361$ae484420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57143 = alloca %struct.ScmObj*, align 8
%argslist55361$ae484421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48444, %struct.ScmObj* %argslist55361$ae484420)
store volatile %struct.ScmObj* %argslist55361$ae484421, %struct.ScmObj** %stackaddr$prim57143, align 8
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%argslist55361$ae484422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48443, %struct.ScmObj* %argslist55361$ae484421)
store volatile %struct.ScmObj* %argslist55361$ae484422, %struct.ScmObj** %stackaddr$prim57144, align 8
%clofunc57145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48442)
musttail call tailcc void %clofunc57145(%struct.ScmObj* %ae48442, %struct.ScmObj* %argslist55361$ae484422)
ret void
}

define tailcc void @proc_clo$ae48442(%struct.ScmObj* %env$ae48442,%struct.ScmObj* %current_45args55349) {
%stackaddr$env-ref57146 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48442, i64 0)
store %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$env-ref57146
%stackaddr$env-ref57147 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48442, i64 1)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57147
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%_95k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55349)
store volatile %struct.ScmObj* %_95k48414, %struct.ScmObj** %stackaddr$prim57148, align 8
%stackaddr$prim57149 = alloca %struct.ScmObj*, align 8
%current_45args55350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55349)
store volatile %struct.ScmObj* %current_45args55350, %struct.ScmObj** %stackaddr$prim57149, align 8
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%anf_45bind48146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55350)
store volatile %struct.ScmObj* %anf_45bind48146, %struct.ScmObj** %stackaddr$prim57150, align 8
%argslist55352$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%argslist55352$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48146, %struct.ScmObj* %argslist55352$f480280)
store volatile %struct.ScmObj* %argslist55352$f480281, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%argslist55352$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist55352$f480281)
store volatile %struct.ScmObj* %argslist55352$f480282, %struct.ScmObj** %stackaddr$prim57152, align 8
%clofunc57153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc57153(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist55352$f480282)
ret void
}

define tailcc void @proc_clo$ae48444(%struct.ScmObj* %env$ae48444,%struct.ScmObj* %args4802948415) {
%stackaddr$env-ref57154 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48444, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57154
%stackaddr$env-ref57155 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48444, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref57155
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948415)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim57156, align 8
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948415)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim57157, align 8
%stackaddr$makeclosure57158 = alloca %struct.ScmObj*, align 8
%fptrToInt57159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48448 to i64
%ae48448 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57159)
store volatile %struct.ScmObj* %ae48448, %struct.ScmObj** %stackaddr$makeclosure57158, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48448, %struct.ScmObj* %k48416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48448, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48448, %struct.ScmObj* %f48028, i64 2)
%argslist55360$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%argslist55360$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist55360$y480270)
store volatile %struct.ScmObj* %argslist55360$y480271, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%argslist55360$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48448, %struct.ScmObj* %argslist55360$y480271)
store volatile %struct.ScmObj* %argslist55360$y480272, %struct.ScmObj** %stackaddr$prim57161, align 8
%clofunc57162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc57162(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist55360$y480272)
ret void
}

define tailcc void @proc_clo$ae48448(%struct.ScmObj* %env$ae48448,%struct.ScmObj* %current_45args55353) {
%stackaddr$env-ref57163 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48448, i64 0)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref57163
%stackaddr$env-ref57164 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48448, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref57164
%stackaddr$env-ref57165 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48448, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57165
%stackaddr$prim57166 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55353)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim57166, align 8
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%current_45args55354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55353)
store volatile %struct.ScmObj* %current_45args55354, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%anf_45bind48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %anf_45bind48144, %struct.ScmObj** %stackaddr$prim57168, align 8
%stackaddr$makeclosure57169 = alloca %struct.ScmObj*, align 8
%fptrToInt57170 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48451 to i64
%ae48451 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57170)
store volatile %struct.ScmObj* %ae48451, %struct.ScmObj** %stackaddr$makeclosure57169, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48451, %struct.ScmObj* %k48416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48451, %struct.ScmObj* %args48029, i64 1)
%argslist55359$anf_45bind481440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%argslist55359$anf_45bind481441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist55359$anf_45bind481440)
store volatile %struct.ScmObj* %argslist55359$anf_45bind481441, %struct.ScmObj** %stackaddr$prim57171, align 8
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%argslist55359$anf_45bind481442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48451, %struct.ScmObj* %argslist55359$anf_45bind481441)
store volatile %struct.ScmObj* %argslist55359$anf_45bind481442, %struct.ScmObj** %stackaddr$prim57172, align 8
%clofunc57173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48144)
musttail call tailcc void %clofunc57173(%struct.ScmObj* %anf_45bind48144, %struct.ScmObj* %argslist55359$anf_45bind481442)
ret void
}

define tailcc void @proc_clo$ae48451(%struct.ScmObj* %env$ae48451,%struct.ScmObj* %current_45args55356) {
%stackaddr$env-ref57174 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48451, i64 0)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref57174
%stackaddr$env-ref57175 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48451, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref57175
%stackaddr$prim57176 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55356)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim57176, align 8
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%current_45args55357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55356)
store volatile %struct.ScmObj* %current_45args55357, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%anf_45bind48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55357)
store volatile %struct.ScmObj* %anf_45bind48145, %struct.ScmObj** %stackaddr$prim57178, align 8
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%cpsargs48419 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48419, %struct.ScmObj** %stackaddr$prim57179, align 8
%clofunc57180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48145)
musttail call tailcc void %clofunc57180(%struct.ScmObj* %anf_45bind48145, %struct.ScmObj* %cpsargs48419)
ret void
}

define tailcc void @proc_clo$ae48423(%struct.ScmObj* %env$ae48423,%struct.ScmObj* %current_45args55364) {
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55364)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim57181, align 8
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%current_45args55365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55364)
store volatile %struct.ScmObj* %current_45args55365, %struct.ScmObj** %stackaddr$prim57182, align 8
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55365)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim57183, align 8
%argslist55367$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%argslist55367$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist55367$yu480260)
store volatile %struct.ScmObj* %argslist55367$yu480261, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$prim57185 = alloca %struct.ScmObj*, align 8
%argslist55367$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48420, %struct.ScmObj* %argslist55367$yu480261)
store volatile %struct.ScmObj* %argslist55367$yu480262, %struct.ScmObj** %stackaddr$prim57185, align 8
%clofunc57186 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc57186(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist55367$yu480262)
ret void
}