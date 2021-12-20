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
%mainenv54414 = call %struct.ScmObj* @const_init_null()
%mainargs54415 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54414, %struct.ScmObj* %mainargs54415)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54412,%struct.ScmObj* %mainargs54413) {
%stackaddr$makeclosure54416 = alloca %struct.ScmObj*, align 8
%fptrToInt54417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47464 to i64
%ae47464 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54417)
store volatile %struct.ScmObj* %ae47464, %struct.ScmObj** %stackaddr$makeclosure54416, align 8
%ae47465 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54418 = alloca %struct.ScmObj*, align 8
%fptrToInt54419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47466 to i64
%ae47466 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54419)
store volatile %struct.ScmObj* %ae47466, %struct.ScmObj** %stackaddr$makeclosure54418, align 8
%argslist54411$ae474640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist54411$ae474641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47466, %struct.ScmObj* %argslist54411$ae474640)
store volatile %struct.ScmObj* %argslist54411$ae474641, %struct.ScmObj** %stackaddr$prim54420, align 8
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%argslist54411$ae474642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47465, %struct.ScmObj* %argslist54411$ae474641)
store volatile %struct.ScmObj* %argslist54411$ae474642, %struct.ScmObj** %stackaddr$prim54421, align 8
%clofunc54422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47464)
musttail call tailcc void %clofunc54422(%struct.ScmObj* %ae47464, %struct.ScmObj* %argslist54411$ae474642)
ret void
}

define tailcc void @proc_clo$ae47464(%struct.ScmObj* %env$ae47464,%struct.ScmObj* %current_45args53797) {
%stackaddr$prim54423 = alloca %struct.ScmObj*, align 8
%_95k47299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53797)
store volatile %struct.ScmObj* %_95k47299, %struct.ScmObj** %stackaddr$prim54423, align 8
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%current_45args53798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53797)
store volatile %struct.ScmObj* %current_45args53798, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%anf_45bind47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53798)
store volatile %struct.ScmObj* %anf_45bind47186, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$makeclosure54426 = alloca %struct.ScmObj*, align 8
%fptrToInt54427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47479 to i64
%ae47479 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54427)
store volatile %struct.ScmObj* %ae47479, %struct.ScmObj** %stackaddr$makeclosure54426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47479, %struct.ScmObj* %anf_45bind47186, i64 0)
%ae47480 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54428 = alloca %struct.ScmObj*, align 8
%fptrToInt54429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47481 to i64
%ae47481 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54429)
store volatile %struct.ScmObj* %ae47481, %struct.ScmObj** %stackaddr$makeclosure54428, align 8
%argslist54406$ae474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54430 = alloca %struct.ScmObj*, align 8
%argslist54406$ae474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47481, %struct.ScmObj* %argslist54406$ae474790)
store volatile %struct.ScmObj* %argslist54406$ae474791, %struct.ScmObj** %stackaddr$prim54430, align 8
%stackaddr$prim54431 = alloca %struct.ScmObj*, align 8
%argslist54406$ae474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47480, %struct.ScmObj* %argslist54406$ae474791)
store volatile %struct.ScmObj* %argslist54406$ae474792, %struct.ScmObj** %stackaddr$prim54431, align 8
%clofunc54432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47479)
musttail call tailcc void %clofunc54432(%struct.ScmObj* %ae47479, %struct.ScmObj* %argslist54406$ae474792)
ret void
}

define tailcc void @proc_clo$ae47479(%struct.ScmObj* %env$ae47479,%struct.ScmObj* %current_45args53800) {
%stackaddr$env-ref54433 = alloca %struct.ScmObj*, align 8
%anf_45bind47186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47479, i64 0)
store %struct.ScmObj* %anf_45bind47186, %struct.ScmObj** %stackaddr$env-ref54433
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%_95k47300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %_95k47300, %struct.ScmObj** %stackaddr$prim54434, align 8
%stackaddr$prim54435 = alloca %struct.ScmObj*, align 8
%current_45args53801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %current_45args53801, %struct.ScmObj** %stackaddr$prim54435, align 8
%stackaddr$prim54436 = alloca %struct.ScmObj*, align 8
%anf_45bind47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53801)
store volatile %struct.ScmObj* %anf_45bind47190, %struct.ScmObj** %stackaddr$prim54436, align 8
%stackaddr$makeclosure54437 = alloca %struct.ScmObj*, align 8
%fptrToInt54438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47594 to i64
%ae47594 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54438)
store volatile %struct.ScmObj* %ae47594, %struct.ScmObj** %stackaddr$makeclosure54437, align 8
%argslist54385$anf_45bind471860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%argslist54385$anf_45bind471861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47190, %struct.ScmObj* %argslist54385$anf_45bind471860)
store volatile %struct.ScmObj* %argslist54385$anf_45bind471861, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%argslist54385$anf_45bind471862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47594, %struct.ScmObj* %argslist54385$anf_45bind471861)
store volatile %struct.ScmObj* %argslist54385$anf_45bind471862, %struct.ScmObj** %stackaddr$prim54440, align 8
%clofunc54441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47186)
musttail call tailcc void %clofunc54441(%struct.ScmObj* %anf_45bind47186, %struct.ScmObj* %argslist54385$anf_45bind471862)
ret void
}

define tailcc void @proc_clo$ae47594(%struct.ScmObj* %env$ae47594,%struct.ScmObj* %current_45args53803) {
%stackaddr$prim54442 = alloca %struct.ScmObj*, align 8
%_95k47301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53803)
store volatile %struct.ScmObj* %_95k47301, %struct.ScmObj** %stackaddr$prim54442, align 8
%stackaddr$prim54443 = alloca %struct.ScmObj*, align 8
%current_45args53804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53803)
store volatile %struct.ScmObj* %current_45args53804, %struct.ScmObj** %stackaddr$prim54443, align 8
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53804)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$makeclosure54445 = alloca %struct.ScmObj*, align 8
%fptrToInt54446 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47596 to i64
%ae47596 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54446)
store volatile %struct.ScmObj* %ae47596, %struct.ScmObj** %stackaddr$makeclosure54445, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47596, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47597 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54447 = alloca %struct.ScmObj*, align 8
%fptrToInt54448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47598 to i64
%ae47598 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54448)
store volatile %struct.ScmObj* %ae47598, %struct.ScmObj** %stackaddr$makeclosure54447, align 8
%argslist54384$ae475960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%argslist54384$ae475961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47598, %struct.ScmObj* %argslist54384$ae475960)
store volatile %struct.ScmObj* %argslist54384$ae475961, %struct.ScmObj** %stackaddr$prim54449, align 8
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%argslist54384$ae475962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47597, %struct.ScmObj* %argslist54384$ae475961)
store volatile %struct.ScmObj* %argslist54384$ae475962, %struct.ScmObj** %stackaddr$prim54450, align 8
%clofunc54451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47596)
musttail call tailcc void %clofunc54451(%struct.ScmObj* %ae47596, %struct.ScmObj* %argslist54384$ae475962)
ret void
}

define tailcc void @proc_clo$ae47596(%struct.ScmObj* %env$ae47596,%struct.ScmObj* %current_45args53806) {
%stackaddr$env-ref54452 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47596, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54452
%stackaddr$prim54453 = alloca %struct.ScmObj*, align 8
%_95k47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53806)
store volatile %struct.ScmObj* %_95k47302, %struct.ScmObj** %stackaddr$prim54453, align 8
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%current_45args53807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53806)
store volatile %struct.ScmObj* %current_45args53807, %struct.ScmObj** %stackaddr$prim54454, align 8
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%anf_45bind47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53807)
store volatile %struct.ScmObj* %anf_45bind47195, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$makeclosure54456 = alloca %struct.ScmObj*, align 8
%fptrToInt54457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47674 to i64
%ae47674 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54457)
store volatile %struct.ScmObj* %ae47674, %struct.ScmObj** %stackaddr$makeclosure54456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47674, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist54368$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%argslist54368$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47195, %struct.ScmObj* %argslist54368$Ycmb470680)
store volatile %struct.ScmObj* %argslist54368$Ycmb470681, %struct.ScmObj** %stackaddr$prim54458, align 8
%stackaddr$prim54459 = alloca %struct.ScmObj*, align 8
%argslist54368$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47674, %struct.ScmObj* %argslist54368$Ycmb470681)
store volatile %struct.ScmObj* %argslist54368$Ycmb470682, %struct.ScmObj** %stackaddr$prim54459, align 8
%clofunc54460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54460(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54368$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47674(%struct.ScmObj* %env$ae47674,%struct.ScmObj* %current_45args53809) {
%stackaddr$env-ref54461 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47674, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54461
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%_95k47303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %_95k47303, %struct.ScmObj** %stackaddr$prim54462, align 8
%stackaddr$prim54463 = alloca %struct.ScmObj*, align 8
%current_45args53810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %current_45args53810, %struct.ScmObj** %stackaddr$prim54463, align 8
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53810)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim54464, align 8
%stackaddr$makeclosure54465 = alloca %struct.ScmObj*, align 8
%fptrToInt54466 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47676 to i64
%ae47676 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54466)
store volatile %struct.ScmObj* %ae47676, %struct.ScmObj** %stackaddr$makeclosure54465, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47676, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47676, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54467 = alloca %struct.ScmObj*, align 8
%fptrToInt54468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47678 to i64
%ae47678 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54468)
store volatile %struct.ScmObj* %ae47678, %struct.ScmObj** %stackaddr$makeclosure54467, align 8
%argslist54367$ae476760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%argslist54367$ae476761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47678, %struct.ScmObj* %argslist54367$ae476760)
store volatile %struct.ScmObj* %argslist54367$ae476761, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%argslist54367$ae476762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47677, %struct.ScmObj* %argslist54367$ae476761)
store volatile %struct.ScmObj* %argslist54367$ae476762, %struct.ScmObj** %stackaddr$prim54470, align 8
%clofunc54471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47676)
musttail call tailcc void %clofunc54471(%struct.ScmObj* %ae47676, %struct.ScmObj* %argslist54367$ae476762)
ret void
}

define tailcc void @proc_clo$ae47676(%struct.ScmObj* %env$ae47676,%struct.ScmObj* %current_45args53812) {
%stackaddr$env-ref54472 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47676, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54472
%stackaddr$env-ref54473 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47676, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54473
%stackaddr$prim54474 = alloca %struct.ScmObj*, align 8
%_95k47304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53812)
store volatile %struct.ScmObj* %_95k47304, %struct.ScmObj** %stackaddr$prim54474, align 8
%stackaddr$prim54475 = alloca %struct.ScmObj*, align 8
%current_45args53813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53812)
store volatile %struct.ScmObj* %current_45args53813, %struct.ScmObj** %stackaddr$prim54475, align 8
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53813)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim54476, align 8
%stackaddr$makeclosure54477 = alloca %struct.ScmObj*, align 8
%fptrToInt54478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47771 to i64
%ae47771 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54478)
store volatile %struct.ScmObj* %ae47771, %struct.ScmObj** %stackaddr$makeclosure54477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47771, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47771, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist54348$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%argslist54348$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47201, %struct.ScmObj* %argslist54348$Ycmb470680)
store volatile %struct.ScmObj* %argslist54348$Ycmb470681, %struct.ScmObj** %stackaddr$prim54479, align 8
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%argslist54348$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47771, %struct.ScmObj* %argslist54348$Ycmb470681)
store volatile %struct.ScmObj* %argslist54348$Ycmb470682, %struct.ScmObj** %stackaddr$prim54480, align 8
%clofunc54481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54481(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54348$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47771(%struct.ScmObj* %env$ae47771,%struct.ScmObj* %current_45args53815) {
%stackaddr$env-ref54482 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47771, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54482
%stackaddr$env-ref54483 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47771, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54483
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%_95k47305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53815)
store volatile %struct.ScmObj* %_95k47305, %struct.ScmObj** %stackaddr$prim54484, align 8
%stackaddr$prim54485 = alloca %struct.ScmObj*, align 8
%current_45args53816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53815)
store volatile %struct.ScmObj* %current_45args53816, %struct.ScmObj** %stackaddr$prim54485, align 8
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53816)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim54486, align 8
%stackaddr$makeclosure54487 = alloca %struct.ScmObj*, align 8
%fptrToInt54488 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47773 to i64
%ae47773 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54488)
store volatile %struct.ScmObj* %ae47773, %struct.ScmObj** %stackaddr$makeclosure54487, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47773, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47773, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47773, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47774 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54489 = alloca %struct.ScmObj*, align 8
%fptrToInt54490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47775 to i64
%ae47775 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54490)
store volatile %struct.ScmObj* %ae47775, %struct.ScmObj** %stackaddr$makeclosure54489, align 8
%argslist54347$ae477730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%argslist54347$ae477731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47775, %struct.ScmObj* %argslist54347$ae477730)
store volatile %struct.ScmObj* %argslist54347$ae477731, %struct.ScmObj** %stackaddr$prim54491, align 8
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%argslist54347$ae477732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47774, %struct.ScmObj* %argslist54347$ae477731)
store volatile %struct.ScmObj* %argslist54347$ae477732, %struct.ScmObj** %stackaddr$prim54492, align 8
%clofunc54493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47773)
musttail call tailcc void %clofunc54493(%struct.ScmObj* %ae47773, %struct.ScmObj* %argslist54347$ae477732)
ret void
}

define tailcc void @proc_clo$ae47773(%struct.ScmObj* %env$ae47773,%struct.ScmObj* %current_45args53818) {
%stackaddr$env-ref54494 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47773, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54494
%stackaddr$env-ref54495 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47773, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54495
%stackaddr$env-ref54496 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47773, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54496
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%_95k47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53818)
store volatile %struct.ScmObj* %_95k47306, %struct.ScmObj** %stackaddr$prim54497, align 8
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%current_45args53819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53818)
store volatile %struct.ScmObj* %current_45args53819, %struct.ScmObj** %stackaddr$prim54498, align 8
%stackaddr$prim54499 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53819)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim54499, align 8
%stackaddr$makeclosure54500 = alloca %struct.ScmObj*, align 8
%fptrToInt54501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47921 to i64
%ae47921 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54501)
store volatile %struct.ScmObj* %ae47921, %struct.ScmObj** %stackaddr$makeclosure54500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47921, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47921, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47921, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist54331$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54502 = alloca %struct.ScmObj*, align 8
%argslist54331$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %argslist54331$Ycmb470680)
store volatile %struct.ScmObj* %argslist54331$Ycmb470681, %struct.ScmObj** %stackaddr$prim54502, align 8
%stackaddr$prim54503 = alloca %struct.ScmObj*, align 8
%argslist54331$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47921, %struct.ScmObj* %argslist54331$Ycmb470681)
store volatile %struct.ScmObj* %argslist54331$Ycmb470682, %struct.ScmObj** %stackaddr$prim54503, align 8
%clofunc54504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54504(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54331$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47921(%struct.ScmObj* %env$ae47921,%struct.ScmObj* %current_45args53821) {
%stackaddr$env-ref54505 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47921, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54505
%stackaddr$env-ref54506 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47921, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54506
%stackaddr$env-ref54507 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47921, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54507
%stackaddr$prim54508 = alloca %struct.ScmObj*, align 8
%_95k47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53821)
store volatile %struct.ScmObj* %_95k47307, %struct.ScmObj** %stackaddr$prim54508, align 8
%stackaddr$prim54509 = alloca %struct.ScmObj*, align 8
%current_45args53822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53821)
store volatile %struct.ScmObj* %current_45args53822, %struct.ScmObj** %stackaddr$prim54509, align 8
%stackaddr$prim54510 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53822)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim54510, align 8
%stackaddr$makeclosure54511 = alloca %struct.ScmObj*, align 8
%fptrToInt54512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47923 to i64
%ae47923 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54512)
store volatile %struct.ScmObj* %ae47923, %struct.ScmObj** %stackaddr$makeclosure54511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47923, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47923, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47923, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47923, %struct.ScmObj* %_37take47081, i64 3)
%ae47924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54513 = alloca %struct.ScmObj*, align 8
%fptrToInt54514 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47925 to i64
%ae47925 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54514)
store volatile %struct.ScmObj* %ae47925, %struct.ScmObj** %stackaddr$makeclosure54513, align 8
%argslist54330$ae479230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54515 = alloca %struct.ScmObj*, align 8
%argslist54330$ae479231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47925, %struct.ScmObj* %argslist54330$ae479230)
store volatile %struct.ScmObj* %argslist54330$ae479231, %struct.ScmObj** %stackaddr$prim54515, align 8
%stackaddr$prim54516 = alloca %struct.ScmObj*, align 8
%argslist54330$ae479232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47924, %struct.ScmObj* %argslist54330$ae479231)
store volatile %struct.ScmObj* %argslist54330$ae479232, %struct.ScmObj** %stackaddr$prim54516, align 8
%clofunc54517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47923)
musttail call tailcc void %clofunc54517(%struct.ScmObj* %ae47923, %struct.ScmObj* %argslist54330$ae479232)
ret void
}

define tailcc void @proc_clo$ae47923(%struct.ScmObj* %env$ae47923,%struct.ScmObj* %current_45args53824) {
%stackaddr$env-ref54518 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47923, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54518
%stackaddr$env-ref54519 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47923, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54519
%stackaddr$env-ref54520 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47923, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54520
%stackaddr$env-ref54521 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47923, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54521
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%_95k47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53824)
store volatile %struct.ScmObj* %_95k47308, %struct.ScmObj** %stackaddr$prim54522, align 8
%stackaddr$prim54523 = alloca %struct.ScmObj*, align 8
%current_45args53825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53824)
store volatile %struct.ScmObj* %current_45args53825, %struct.ScmObj** %stackaddr$prim54523, align 8
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53825)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim54524, align 8
%stackaddr$makeclosure54525 = alloca %struct.ScmObj*, align 8
%fptrToInt54526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48004 to i64
%ae48004 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54526)
store volatile %struct.ScmObj* %ae48004, %struct.ScmObj** %stackaddr$makeclosure54525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48004, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48004, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48004, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48004, %struct.ScmObj* %_37take47081, i64 3)
%argslist54316$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54527 = alloca %struct.ScmObj*, align 8
%argslist54316$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %argslist54316$Ycmb470680)
store volatile %struct.ScmObj* %argslist54316$Ycmb470681, %struct.ScmObj** %stackaddr$prim54527, align 8
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%argslist54316$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48004, %struct.ScmObj* %argslist54316$Ycmb470681)
store volatile %struct.ScmObj* %argslist54316$Ycmb470682, %struct.ScmObj** %stackaddr$prim54528, align 8
%clofunc54529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54529(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54316$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48004(%struct.ScmObj* %env$ae48004,%struct.ScmObj* %current_45args53827) {
%stackaddr$env-ref54530 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48004, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54530
%stackaddr$env-ref54531 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48004, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54531
%stackaddr$env-ref54532 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48004, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54532
%stackaddr$env-ref54533 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48004, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54533
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%_95k47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %_95k47309, %struct.ScmObj** %stackaddr$prim54534, align 8
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%current_45args53828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %current_45args53828, %struct.ScmObj** %stackaddr$prim54535, align 8
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53828)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$makeclosure54537 = alloca %struct.ScmObj*, align 8
%fptrToInt54538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48006 to i64
%ae48006 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54538)
store volatile %struct.ScmObj* %ae48006, %struct.ScmObj** %stackaddr$makeclosure54537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %_37take47081, i64 4)
%ae48007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54539 = alloca %struct.ScmObj*, align 8
%fptrToInt54540 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48008 to i64
%ae48008 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54540)
store volatile %struct.ScmObj* %ae48008, %struct.ScmObj** %stackaddr$makeclosure54539, align 8
%argslist54315$ae480060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54541 = alloca %struct.ScmObj*, align 8
%argslist54315$ae480061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48008, %struct.ScmObj* %argslist54315$ae480060)
store volatile %struct.ScmObj* %argslist54315$ae480061, %struct.ScmObj** %stackaddr$prim54541, align 8
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%argslist54315$ae480062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48007, %struct.ScmObj* %argslist54315$ae480061)
store volatile %struct.ScmObj* %argslist54315$ae480062, %struct.ScmObj** %stackaddr$prim54542, align 8
%clofunc54543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48006)
musttail call tailcc void %clofunc54543(%struct.ScmObj* %ae48006, %struct.ScmObj* %argslist54315$ae480062)
ret void
}

define tailcc void @proc_clo$ae48006(%struct.ScmObj* %env$ae48006,%struct.ScmObj* %current_45args53830) {
%stackaddr$env-ref54544 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54544
%stackaddr$env-ref54545 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54545
%stackaddr$env-ref54546 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54546
%stackaddr$env-ref54547 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54547
%stackaddr$env-ref54548 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54548
%stackaddr$prim54549 = alloca %struct.ScmObj*, align 8
%_95k47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53830)
store volatile %struct.ScmObj* %_95k47310, %struct.ScmObj** %stackaddr$prim54549, align 8
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%current_45args53831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53830)
store volatile %struct.ScmObj* %current_45args53831, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53831)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim54551, align 8
%stackaddr$makeclosure54552 = alloca %struct.ScmObj*, align 8
%fptrToInt54553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48083 to i64
%ae48083 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54553)
store volatile %struct.ScmObj* %ae48083, %struct.ScmObj** %stackaddr$makeclosure54552, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48083, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48083, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48083, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48083, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48083, %struct.ScmObj* %_37take47081, i64 4)
%argslist54299$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54554 = alloca %struct.ScmObj*, align 8
%argslist54299$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist54299$Ycmb470680)
store volatile %struct.ScmObj* %argslist54299$Ycmb470681, %struct.ScmObj** %stackaddr$prim54554, align 8
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%argslist54299$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48083, %struct.ScmObj* %argslist54299$Ycmb470681)
store volatile %struct.ScmObj* %argslist54299$Ycmb470682, %struct.ScmObj** %stackaddr$prim54555, align 8
%clofunc54556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54556(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54299$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48083(%struct.ScmObj* %env$ae48083,%struct.ScmObj* %current_45args53833) {
%stackaddr$env-ref54557 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48083, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54557
%stackaddr$env-ref54558 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48083, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54558
%stackaddr$env-ref54559 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48083, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54559
%stackaddr$env-ref54560 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48083, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54560
%stackaddr$env-ref54561 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48083, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54561
%stackaddr$prim54562 = alloca %struct.ScmObj*, align 8
%_95k47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53833)
store volatile %struct.ScmObj* %_95k47311, %struct.ScmObj** %stackaddr$prim54562, align 8
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%current_45args53834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53833)
store volatile %struct.ScmObj* %current_45args53834, %struct.ScmObj** %stackaddr$prim54563, align 8
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53834)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim54564, align 8
%stackaddr$makeclosure54565 = alloca %struct.ScmObj*, align 8
%fptrToInt54566 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48085 to i64
%ae48085 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54566)
store volatile %struct.ScmObj* %ae48085, %struct.ScmObj** %stackaddr$makeclosure54565, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48085, %struct.ScmObj* %_37take47081, i64 5)
%ae48086 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54567 = alloca %struct.ScmObj*, align 8
%fptrToInt54568 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48087 to i64
%ae48087 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54568)
store volatile %struct.ScmObj* %ae48087, %struct.ScmObj** %stackaddr$makeclosure54567, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48087, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist54298$ae480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54569 = alloca %struct.ScmObj*, align 8
%argslist54298$ae480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48087, %struct.ScmObj* %argslist54298$ae480850)
store volatile %struct.ScmObj* %argslist54298$ae480851, %struct.ScmObj** %stackaddr$prim54569, align 8
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist54298$ae480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48086, %struct.ScmObj* %argslist54298$ae480851)
store volatile %struct.ScmObj* %argslist54298$ae480852, %struct.ScmObj** %stackaddr$prim54570, align 8
%clofunc54571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48085)
musttail call tailcc void %clofunc54571(%struct.ScmObj* %ae48085, %struct.ScmObj* %argslist54298$ae480852)
ret void
}

define tailcc void @proc_clo$ae48085(%struct.ScmObj* %env$ae48085,%struct.ScmObj* %current_45args53836) {
%stackaddr$env-ref54572 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54572
%stackaddr$env-ref54573 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54573
%stackaddr$env-ref54574 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54574
%stackaddr$env-ref54575 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54575
%stackaddr$env-ref54576 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54576
%stackaddr$env-ref54577 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48085, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54577
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%_95k47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53836)
store volatile %struct.ScmObj* %_95k47312, %struct.ScmObj** %stackaddr$prim54578, align 8
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%current_45args53837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53836)
store volatile %struct.ScmObj* %current_45args53837, %struct.ScmObj** %stackaddr$prim54579, align 8
%stackaddr$prim54580 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53837)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim54580, align 8
%stackaddr$makeclosure54581 = alloca %struct.ScmObj*, align 8
%fptrToInt54582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48139 to i64
%ae48139 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54582)
store volatile %struct.ScmObj* %ae48139, %struct.ScmObj** %stackaddr$makeclosure54581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48139, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48139, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48139, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48139, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48139, %struct.ScmObj* %_37last47111, i64 4)
%ae48140 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54583 = alloca %struct.ScmObj*, align 8
%fptrToInt54584 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48141 to i64
%ae48141 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54584)
store volatile %struct.ScmObj* %ae48141, %struct.ScmObj** %stackaddr$makeclosure54583, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48141, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48141, %struct.ScmObj* %_37take47081, i64 1)
%argslist54284$ae481390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54585 = alloca %struct.ScmObj*, align 8
%argslist54284$ae481391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48141, %struct.ScmObj* %argslist54284$ae481390)
store volatile %struct.ScmObj* %argslist54284$ae481391, %struct.ScmObj** %stackaddr$prim54585, align 8
%stackaddr$prim54586 = alloca %struct.ScmObj*, align 8
%argslist54284$ae481392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48140, %struct.ScmObj* %argslist54284$ae481391)
store volatile %struct.ScmObj* %argslist54284$ae481392, %struct.ScmObj** %stackaddr$prim54586, align 8
%clofunc54587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48139)
musttail call tailcc void %clofunc54587(%struct.ScmObj* %ae48139, %struct.ScmObj* %argslist54284$ae481392)
ret void
}

define tailcc void @proc_clo$ae48139(%struct.ScmObj* %env$ae48139,%struct.ScmObj* %current_45args53839) {
%stackaddr$env-ref54588 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48139, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54588
%stackaddr$env-ref54589 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48139, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54589
%stackaddr$env-ref54590 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48139, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54590
%stackaddr$env-ref54591 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48139, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54591
%stackaddr$env-ref54592 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48139, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54592
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%_95k47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %_95k47313, %struct.ScmObj** %stackaddr$prim54593, align 8
%stackaddr$prim54594 = alloca %struct.ScmObj*, align 8
%current_45args53840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %current_45args53840, %struct.ScmObj** %stackaddr$prim54594, align 8
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53840)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$makeclosure54596 = alloca %struct.ScmObj*, align 8
%fptrToInt54597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48169 to i64
%ae48169 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54597)
store volatile %struct.ScmObj* %ae48169, %struct.ScmObj** %stackaddr$makeclosure54596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48169, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48169, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48169, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48169, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48169, %struct.ScmObj* %_37last47111, i64 4)
%ae48170 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54598 = alloca %struct.ScmObj*, align 8
%fptrToInt54599 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48171 to i64
%ae48171 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54599)
store volatile %struct.ScmObj* %ae48171, %struct.ScmObj** %stackaddr$makeclosure54598, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48171, %struct.ScmObj* %_37map147085, i64 1)
%argslist54274$ae481690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54600 = alloca %struct.ScmObj*, align 8
%argslist54274$ae481691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48171, %struct.ScmObj* %argslist54274$ae481690)
store volatile %struct.ScmObj* %argslist54274$ae481691, %struct.ScmObj** %stackaddr$prim54600, align 8
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%argslist54274$ae481692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48170, %struct.ScmObj* %argslist54274$ae481691)
store volatile %struct.ScmObj* %argslist54274$ae481692, %struct.ScmObj** %stackaddr$prim54601, align 8
%clofunc54602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48169)
musttail call tailcc void %clofunc54602(%struct.ScmObj* %ae48169, %struct.ScmObj* %argslist54274$ae481692)
ret void
}

define tailcc void @proc_clo$ae48169(%struct.ScmObj* %env$ae48169,%struct.ScmObj* %current_45args53842) {
%stackaddr$env-ref54603 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48169, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54603
%stackaddr$env-ref54604 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48169, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54604
%stackaddr$env-ref54605 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48169, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54605
%stackaddr$env-ref54606 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48169, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54606
%stackaddr$env-ref54607 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48169, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54607
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%_95k47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53842)
store volatile %struct.ScmObj* %_95k47314, %struct.ScmObj** %stackaddr$prim54608, align 8
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%current_45args53843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53842)
store volatile %struct.ScmObj* %current_45args53843, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$prim54610 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53843)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim54610, align 8
%stackaddr$makeclosure54611 = alloca %struct.ScmObj*, align 8
%fptrToInt54612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48553 to i64
%ae48553 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54612)
store volatile %struct.ScmObj* %ae48553, %struct.ScmObj** %stackaddr$makeclosure54611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48553, %struct.ScmObj* %_37last47111, i64 4)
%argslist54214$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%argslist54214$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist54214$Ycmb470680)
store volatile %struct.ScmObj* %argslist54214$Ycmb470681, %struct.ScmObj** %stackaddr$prim54613, align 8
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%argslist54214$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48553, %struct.ScmObj* %argslist54214$Ycmb470681)
store volatile %struct.ScmObj* %argslist54214$Ycmb470682, %struct.ScmObj** %stackaddr$prim54614, align 8
%clofunc54615 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54615(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54214$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48553(%struct.ScmObj* %env$ae48553,%struct.ScmObj* %current_45args53845) {
%stackaddr$env-ref54616 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54616
%stackaddr$env-ref54617 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54617
%stackaddr$env-ref54618 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54618
%stackaddr$env-ref54619 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54619
%stackaddr$env-ref54620 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48553, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54620
%stackaddr$prim54621 = alloca %struct.ScmObj*, align 8
%_95k47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53845)
store volatile %struct.ScmObj* %_95k47315, %struct.ScmObj** %stackaddr$prim54621, align 8
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%current_45args53846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53845)
store volatile %struct.ScmObj* %current_45args53846, %struct.ScmObj** %stackaddr$prim54622, align 8
%stackaddr$prim54623 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53846)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim54623, align 8
%stackaddr$makeclosure54624 = alloca %struct.ScmObj*, align 8
%fptrToInt54625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48555 to i64
%ae48555 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54625)
store volatile %struct.ScmObj* %ae48555, %struct.ScmObj** %stackaddr$makeclosure54624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48555, %struct.ScmObj* %_37last47111, i64 5)
%ae48556 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54626 = alloca %struct.ScmObj*, align 8
%fptrToInt54627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48557 to i64
%ae48557 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54627)
store volatile %struct.ScmObj* %ae48557, %struct.ScmObj** %stackaddr$makeclosure54626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist54213$ae485550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54628 = alloca %struct.ScmObj*, align 8
%argslist54213$ae485551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist54213$ae485550)
store volatile %struct.ScmObj* %argslist54213$ae485551, %struct.ScmObj** %stackaddr$prim54628, align 8
%stackaddr$prim54629 = alloca %struct.ScmObj*, align 8
%argslist54213$ae485552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48556, %struct.ScmObj* %argslist54213$ae485551)
store volatile %struct.ScmObj* %argslist54213$ae485552, %struct.ScmObj** %stackaddr$prim54629, align 8
%clofunc54630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48555)
musttail call tailcc void %clofunc54630(%struct.ScmObj* %ae48555, %struct.ScmObj* %argslist54213$ae485552)
ret void
}

define tailcc void @proc_clo$ae48555(%struct.ScmObj* %env$ae48555,%struct.ScmObj* %current_45args53848) {
%stackaddr$env-ref54631 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54631
%stackaddr$env-ref54632 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54632
%stackaddr$env-ref54633 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54633
%stackaddr$env-ref54634 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54634
%stackaddr$env-ref54635 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54635
%stackaddr$env-ref54636 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48555, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54636
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%_95k47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53848)
store volatile %struct.ScmObj* %_95k47316, %struct.ScmObj** %stackaddr$prim54637, align 8
%stackaddr$prim54638 = alloca %struct.ScmObj*, align 8
%current_45args53849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53848)
store volatile %struct.ScmObj* %current_45args53849, %struct.ScmObj** %stackaddr$prim54638, align 8
%stackaddr$prim54639 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53849)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim54639, align 8
%stackaddr$makeclosure54640 = alloca %struct.ScmObj*, align 8
%fptrToInt54641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48632 to i64
%ae48632 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54641)
store volatile %struct.ScmObj* %ae48632, %struct.ScmObj** %stackaddr$makeclosure54640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48632, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48633 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54642 = alloca %struct.ScmObj*, align 8
%fptrToInt54643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48634 to i64
%ae48634 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54643)
store volatile %struct.ScmObj* %ae48634, %struct.ScmObj** %stackaddr$makeclosure54642, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %_37last47111, i64 2)
%argslist54194$ae486320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%argslist54194$ae486321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist54194$ae486320)
store volatile %struct.ScmObj* %argslist54194$ae486321, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%argslist54194$ae486322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48633, %struct.ScmObj* %argslist54194$ae486321)
store volatile %struct.ScmObj* %argslist54194$ae486322, %struct.ScmObj** %stackaddr$prim54645, align 8
%clofunc54646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48632)
musttail call tailcc void %clofunc54646(%struct.ScmObj* %ae48632, %struct.ScmObj* %argslist54194$ae486322)
ret void
}

define tailcc void @proc_clo$ae48632(%struct.ScmObj* %env$ae48632,%struct.ScmObj* %current_45args53851) {
%stackaddr$env-ref54647 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54647
%stackaddr$env-ref54648 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54648
%stackaddr$env-ref54649 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54649
%stackaddr$env-ref54650 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54650
%stackaddr$env-ref54651 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48632, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54651
%stackaddr$prim54652 = alloca %struct.ScmObj*, align 8
%_95k47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %_95k47317, %struct.ScmObj** %stackaddr$prim54652, align 8
%stackaddr$prim54653 = alloca %struct.ScmObj*, align 8
%current_45args53852 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %current_45args53852, %struct.ScmObj** %stackaddr$prim54653, align 8
%stackaddr$prim54654 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53852)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim54654, align 8
%stackaddr$makeclosure54655 = alloca %struct.ScmObj*, align 8
%fptrToInt54656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48778 to i64
%ae48778 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54656)
store volatile %struct.ScmObj* %ae48778, %struct.ScmObj** %stackaddr$makeclosure54655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48778, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48778, %struct.ScmObj* %Ycmb47068, i64 1)
%ae48779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54657 = alloca %struct.ScmObj*, align 8
%fptrToInt54658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48780 to i64
%ae48780 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54658)
store volatile %struct.ScmObj* %ae48780, %struct.ScmObj** %stackaddr$makeclosure54657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48780, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48780, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48780, %struct.ScmObj* %_37map147120, i64 2)
%argslist54177$ae487780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%argslist54177$ae487781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48780, %struct.ScmObj* %argslist54177$ae487780)
store volatile %struct.ScmObj* %argslist54177$ae487781, %struct.ScmObj** %stackaddr$prim54659, align 8
%stackaddr$prim54660 = alloca %struct.ScmObj*, align 8
%argslist54177$ae487782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48779, %struct.ScmObj* %argslist54177$ae487781)
store volatile %struct.ScmObj* %argslist54177$ae487782, %struct.ScmObj** %stackaddr$prim54660, align 8
%clofunc54661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48778)
musttail call tailcc void %clofunc54661(%struct.ScmObj* %ae48778, %struct.ScmObj* %argslist54177$ae487782)
ret void
}

define tailcc void @proc_clo$ae48778(%struct.ScmObj* %env$ae48778,%struct.ScmObj* %current_45args53854) {
%stackaddr$env-ref54662 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48778, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54662
%stackaddr$env-ref54663 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48778, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54663
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%_95k47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %_95k47318, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%current_45args53855 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %current_45args53855, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53855)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$makeclosure54667 = alloca %struct.ScmObj*, align 8
%fptrToInt54668 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49170 to i64
%ae49170 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54668)
store volatile %struct.ScmObj* %ae49170, %struct.ScmObj** %stackaddr$makeclosure54667, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49170, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist54117$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54669 = alloca %struct.ScmObj*, align 8
%argslist54117$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist54117$Ycmb470680)
store volatile %struct.ScmObj* %argslist54117$Ycmb470681, %struct.ScmObj** %stackaddr$prim54669, align 8
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%argslist54117$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49170, %struct.ScmObj* %argslist54117$Ycmb470681)
store volatile %struct.ScmObj* %argslist54117$Ycmb470682, %struct.ScmObj** %stackaddr$prim54670, align 8
%clofunc54671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54671(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54117$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49170(%struct.ScmObj* %env$ae49170,%struct.ScmObj* %current_45args53857) {
%stackaddr$env-ref54672 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49170, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54672
%stackaddr$prim54673 = alloca %struct.ScmObj*, align 8
%_95k47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53857)
store volatile %struct.ScmObj* %_95k47319, %struct.ScmObj** %stackaddr$prim54673, align 8
%stackaddr$prim54674 = alloca %struct.ScmObj*, align 8
%current_45args53858 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53857)
store volatile %struct.ScmObj* %current_45args53858, %struct.ScmObj** %stackaddr$prim54674, align 8
%stackaddr$prim54675 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim54675, align 8
%stackaddr$makeclosure54676 = alloca %struct.ScmObj*, align 8
%fptrToInt54677 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49172 to i64
%ae49172 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54677)
store volatile %struct.ScmObj* %ae49172, %struct.ScmObj** %stackaddr$makeclosure54676, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49172, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49173 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54678 = alloca %struct.ScmObj*, align 8
%fptrToInt54679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49174 to i64
%ae49174 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54679)
store volatile %struct.ScmObj* %ae49174, %struct.ScmObj** %stackaddr$makeclosure54678, align 8
%argslist54116$ae491720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54680 = alloca %struct.ScmObj*, align 8
%argslist54116$ae491721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49174, %struct.ScmObj* %argslist54116$ae491720)
store volatile %struct.ScmObj* %argslist54116$ae491721, %struct.ScmObj** %stackaddr$prim54680, align 8
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%argslist54116$ae491722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49173, %struct.ScmObj* %argslist54116$ae491721)
store volatile %struct.ScmObj* %argslist54116$ae491722, %struct.ScmObj** %stackaddr$prim54681, align 8
%clofunc54682 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49172)
musttail call tailcc void %clofunc54682(%struct.ScmObj* %ae49172, %struct.ScmObj* %argslist54116$ae491722)
ret void
}

define tailcc void @proc_clo$ae49172(%struct.ScmObj* %env$ae49172,%struct.ScmObj* %current_45args53860) {
%stackaddr$env-ref54683 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49172, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54683
%stackaddr$prim54684 = alloca %struct.ScmObj*, align 8
%_95k47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53860)
store volatile %struct.ScmObj* %_95k47320, %struct.ScmObj** %stackaddr$prim54684, align 8
%stackaddr$prim54685 = alloca %struct.ScmObj*, align 8
%current_45args53861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53860)
store volatile %struct.ScmObj* %current_45args53861, %struct.ScmObj** %stackaddr$prim54685, align 8
%stackaddr$prim54686 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53861)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim54686, align 8
%stackaddr$makeclosure54687 = alloca %struct.ScmObj*, align 8
%fptrToInt54688 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54688)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure54687, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49197 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54689 = alloca %struct.ScmObj*, align 8
%fptrToInt54690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49198 to i64
%ae49198 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54690)
store volatile %struct.ScmObj* %ae49198, %struct.ScmObj** %stackaddr$makeclosure54689, align 8
%argslist54110$ae491960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%argslist54110$ae491961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49198, %struct.ScmObj* %argslist54110$ae491960)
store volatile %struct.ScmObj* %argslist54110$ae491961, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%argslist54110$ae491962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49197, %struct.ScmObj* %argslist54110$ae491961)
store volatile %struct.ScmObj* %argslist54110$ae491962, %struct.ScmObj** %stackaddr$prim54692, align 8
%clofunc54693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49196)
musttail call tailcc void %clofunc54693(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist54110$ae491962)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %current_45args53863) {
%stackaddr$env-ref54694 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54694
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%_95k47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53863)
store volatile %struct.ScmObj* %_95k47321, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$prim54696 = alloca %struct.ScmObj*, align 8
%current_45args53864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53863)
store volatile %struct.ScmObj* %current_45args53864, %struct.ScmObj** %stackaddr$prim54696, align 8
%stackaddr$prim54697 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53864)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54697, align 8
%ae49220 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49221 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49220, %struct.ScmObj* %ae49221)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54698, align 8
%stackaddr$makeclosure54699 = alloca %struct.ScmObj*, align 8
%fptrToInt54700 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49222 to i64
%ae49222 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54700)
store volatile %struct.ScmObj* %ae49222, %struct.ScmObj** %stackaddr$makeclosure54699, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49222, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49222, %struct.ScmObj* %_37append47161, i64 1)
%ae49223 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54701 = alloca %struct.ScmObj*, align 8
%fptrToInt54702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49224 to i64
%ae49224 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54702)
store volatile %struct.ScmObj* %ae49224, %struct.ScmObj** %stackaddr$makeclosure54701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49224, %struct.ScmObj* %_37append47161, i64 0)
%argslist54104$ae492220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%argslist54104$ae492221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49224, %struct.ScmObj* %argslist54104$ae492220)
store volatile %struct.ScmObj* %argslist54104$ae492221, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%argslist54104$ae492222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49223, %struct.ScmObj* %argslist54104$ae492221)
store volatile %struct.ScmObj* %argslist54104$ae492222, %struct.ScmObj** %stackaddr$prim54704, align 8
%clofunc54705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49222)
musttail call tailcc void %clofunc54705(%struct.ScmObj* %ae49222, %struct.ScmObj* %argslist54104$ae492222)
ret void
}

define tailcc void @proc_clo$ae49222(%struct.ScmObj* %env$ae49222,%struct.ScmObj* %current_45args53866) {
%stackaddr$env-ref54706 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49222, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54706
%stackaddr$env-ref54707 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49222, i64 1)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54707
%stackaddr$prim54708 = alloca %struct.ScmObj*, align 8
%_95k47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %_95k47322, %struct.ScmObj** %stackaddr$prim54708, align 8
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%current_45args53867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %current_45args53867, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53867)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim54710, align 8
%ae49290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54711 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49290, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54711, align 8
%ae49293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49293)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$makeclosure54713 = alloca %struct.ScmObj*, align 8
%fptrToInt54714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49294 to i64
%ae49294 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54714)
store volatile %struct.ScmObj* %ae49294, %struct.ScmObj** %stackaddr$makeclosure54713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49294, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54715 = alloca %struct.ScmObj*, align 8
%fptrToInt54716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49296 to i64
%ae49296 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54716)
store volatile %struct.ScmObj* %ae49296, %struct.ScmObj** %stackaddr$makeclosure54715, align 8
%argslist54093$ae492940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%argslist54093$ae492941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist54093$ae492940)
store volatile %struct.ScmObj* %argslist54093$ae492941, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%argslist54093$ae492942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49295, %struct.ScmObj* %argslist54093$ae492941)
store volatile %struct.ScmObj* %argslist54093$ae492942, %struct.ScmObj** %stackaddr$prim54718, align 8
%clofunc54719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49294)
musttail call tailcc void %clofunc54719(%struct.ScmObj* %ae49294, %struct.ScmObj* %argslist54093$ae492942)
ret void
}

define tailcc void @proc_clo$ae49294(%struct.ScmObj* %env$ae49294,%struct.ScmObj* %current_45args53869) {
%stackaddr$env-ref54720 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49294, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54720
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%_95k47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %_95k47323, %struct.ScmObj** %stackaddr$prim54721, align 8
%stackaddr$prim54722 = alloca %struct.ScmObj*, align 8
%current_45args53870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %current_45args53870, %struct.ScmObj** %stackaddr$prim54722, align 8
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$makeclosure54724 = alloca %struct.ScmObj*, align 8
%fptrToInt54725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49710 to i64
%ae49710 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54725)
store volatile %struct.ScmObj* %ae49710, %struct.ScmObj** %stackaddr$makeclosure54724, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49710, %struct.ScmObj* %_37foldl147073, i64 0)
%ae49711 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54726 = alloca %struct.ScmObj*, align 8
%fptrToInt54727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49712 to i64
%ae49712 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54727)
store volatile %struct.ScmObj* %ae49712, %struct.ScmObj** %stackaddr$makeclosure54726, align 8
%argslist54068$ae497100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54728 = alloca %struct.ScmObj*, align 8
%argslist54068$ae497101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49712, %struct.ScmObj* %argslist54068$ae497100)
store volatile %struct.ScmObj* %argslist54068$ae497101, %struct.ScmObj** %stackaddr$prim54728, align 8
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%argslist54068$ae497102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49711, %struct.ScmObj* %argslist54068$ae497101)
store volatile %struct.ScmObj* %argslist54068$ae497102, %struct.ScmObj** %stackaddr$prim54729, align 8
%clofunc54730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49710)
musttail call tailcc void %clofunc54730(%struct.ScmObj* %ae49710, %struct.ScmObj* %argslist54068$ae497102)
ret void
}

define tailcc void @proc_clo$ae49710(%struct.ScmObj* %env$ae49710,%struct.ScmObj* %current_45args53872) {
%stackaddr$env-ref54731 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49710, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54731
%stackaddr$prim54732 = alloca %struct.ScmObj*, align 8
%_95k47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53872)
store volatile %struct.ScmObj* %_95k47324, %struct.ScmObj** %stackaddr$prim54732, align 8
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%current_45args53873 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53872)
store volatile %struct.ScmObj* %current_45args53873, %struct.ScmObj** %stackaddr$prim54733, align 8
%stackaddr$prim54734 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53873)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54734, align 8
%stackaddr$makeclosure54735 = alloca %struct.ScmObj*, align 8
%fptrToInt54736 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50246 to i64
%ae50246 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54736)
store volatile %struct.ScmObj* %ae50246, %struct.ScmObj** %stackaddr$makeclosure54735, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50246, %struct.ScmObj* %_37foldl147073, i64 0)
%ae50247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54737 = alloca %struct.ScmObj*, align 8
%fptrToInt54738 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50248 to i64
%ae50248 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54738)
store volatile %struct.ScmObj* %ae50248, %struct.ScmObj** %stackaddr$makeclosure54737, align 8
%argslist54044$ae502460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54739 = alloca %struct.ScmObj*, align 8
%argslist54044$ae502461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50248, %struct.ScmObj* %argslist54044$ae502460)
store volatile %struct.ScmObj* %argslist54044$ae502461, %struct.ScmObj** %stackaddr$prim54739, align 8
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%argslist54044$ae502462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50247, %struct.ScmObj* %argslist54044$ae502461)
store volatile %struct.ScmObj* %argslist54044$ae502462, %struct.ScmObj** %stackaddr$prim54740, align 8
%clofunc54741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50246)
musttail call tailcc void %clofunc54741(%struct.ScmObj* %ae50246, %struct.ScmObj* %argslist54044$ae502462)
ret void
}

define tailcc void @proc_clo$ae50246(%struct.ScmObj* %env$ae50246,%struct.ScmObj* %current_45args53875) {
%stackaddr$env-ref54742 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50246, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54742
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%_95k47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %_95k47325, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%current_45args53876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %current_45args53876, %struct.ScmObj** %stackaddr$prim54744, align 8
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53876)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50650 to i64
%ae50650 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae50650, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
%ae50651 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54748 = alloca %struct.ScmObj*, align 8
%fptrToInt54749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50652 to i64
%ae50652 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54749)
store volatile %struct.ScmObj* %ae50652, %struct.ScmObj** %stackaddr$makeclosure54748, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50652, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist54018$ae506500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%argslist54018$ae506501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50652, %struct.ScmObj* %argslist54018$ae506500)
store volatile %struct.ScmObj* %argslist54018$ae506501, %struct.ScmObj** %stackaddr$prim54750, align 8
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%argslist54018$ae506502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50651, %struct.ScmObj* %argslist54018$ae506501)
store volatile %struct.ScmObj* %argslist54018$ae506502, %struct.ScmObj** %stackaddr$prim54751, align 8
%clofunc54752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50650)
musttail call tailcc void %clofunc54752(%struct.ScmObj* %ae50650, %struct.ScmObj* %argslist54018$ae506502)
ret void
}

define tailcc void @proc_clo$ae50650(%struct.ScmObj* %env$ae50650,%struct.ScmObj* %current_45args53878) {
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53878)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%current_45args53879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53878)
store volatile %struct.ScmObj* %current_45args53879, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53879)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50748 to i64
%ae50748 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae50748, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
%ae50749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54758 = alloca %struct.ScmObj*, align 8
%fptrToInt54759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50750 to i64
%ae50750 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54759)
store volatile %struct.ScmObj* %ae50750, %struct.ScmObj** %stackaddr$makeclosure54758, align 8
%argslist54005$ae507480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%argslist54005$ae507481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50750, %struct.ScmObj* %argslist54005$ae507480)
store volatile %struct.ScmObj* %argslist54005$ae507481, %struct.ScmObj** %stackaddr$prim54760, align 8
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%argslist54005$ae507482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50749, %struct.ScmObj* %argslist54005$ae507481)
store volatile %struct.ScmObj* %argslist54005$ae507482, %struct.ScmObj** %stackaddr$prim54761, align 8
%clofunc54762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50748)
musttail call tailcc void %clofunc54762(%struct.ScmObj* %ae50748, %struct.ScmObj* %argslist54005$ae507482)
ret void
}

define tailcc void @proc_clo$ae50748(%struct.ScmObj* %env$ae50748,%struct.ScmObj* %current_45args53881) {
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53881)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%current_45args53882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53881)
store volatile %struct.ScmObj* %current_45args53882, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53882)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$makeclosure54766 = alloca %struct.ScmObj*, align 8
%fptrToInt54767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50768 to i64
%ae50768 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54767)
store volatile %struct.ScmObj* %ae50768, %struct.ScmObj** %stackaddr$makeclosure54766, align 8
%ae50769 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54768 = alloca %struct.ScmObj*, align 8
%fptrToInt54769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50770 to i64
%ae50770 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54769)
store volatile %struct.ScmObj* %ae50770, %struct.ScmObj** %stackaddr$makeclosure54768, align 8
%argslist54000$ae507680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%argslist54000$ae507681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50770, %struct.ScmObj* %argslist54000$ae507680)
store volatile %struct.ScmObj* %argslist54000$ae507681, %struct.ScmObj** %stackaddr$prim54770, align 8
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%argslist54000$ae507682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50769, %struct.ScmObj* %argslist54000$ae507681)
store volatile %struct.ScmObj* %argslist54000$ae507682, %struct.ScmObj** %stackaddr$prim54771, align 8
%clofunc54772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50768)
musttail call tailcc void %clofunc54772(%struct.ScmObj* %ae50768, %struct.ScmObj* %argslist54000$ae507682)
ret void
}

define tailcc void @proc_clo$ae50768(%struct.ScmObj* %env$ae50768,%struct.ScmObj* %current_45args53884) {
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53884)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim54773, align 8
%stackaddr$prim54774 = alloca %struct.ScmObj*, align 8
%current_45args53885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53884)
store volatile %struct.ScmObj* %current_45args53885, %struct.ScmObj** %stackaddr$prim54774, align 8
%stackaddr$prim54775 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53885)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54775, align 8
%stackaddr$makeclosure54776 = alloca %struct.ScmObj*, align 8
%fptrToInt54777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50790 to i64
%ae50790 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54777)
store volatile %struct.ScmObj* %ae50790, %struct.ScmObj** %stackaddr$makeclosure54776, align 8
%ae50791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54778 = alloca %struct.ScmObj*, align 8
%fptrToInt54779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50792 to i64
%ae50792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54779)
store volatile %struct.ScmObj* %ae50792, %struct.ScmObj** %stackaddr$makeclosure54778, align 8
%argslist53995$ae507900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54780 = alloca %struct.ScmObj*, align 8
%argslist53995$ae507901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50792, %struct.ScmObj* %argslist53995$ae507900)
store volatile %struct.ScmObj* %argslist53995$ae507901, %struct.ScmObj** %stackaddr$prim54780, align 8
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%argslist53995$ae507902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50791, %struct.ScmObj* %argslist53995$ae507901)
store volatile %struct.ScmObj* %argslist53995$ae507902, %struct.ScmObj** %stackaddr$prim54781, align 8
%clofunc54782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50790)
musttail call tailcc void %clofunc54782(%struct.ScmObj* %ae50790, %struct.ScmObj* %argslist53995$ae507902)
ret void
}

define tailcc void @proc_clo$ae50790(%struct.ScmObj* %env$ae50790,%struct.ScmObj* %current_45args53887) {
%stackaddr$prim54783 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim54783, align 8
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%current_45args53888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %current_45args53888, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53888)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$makeclosure54786 = alloca %struct.ScmObj*, align 8
%fptrToInt54787 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50814 to i64
%ae50814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54787)
store volatile %struct.ScmObj* %ae50814, %struct.ScmObj** %stackaddr$makeclosure54786, align 8
%ae50815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54788 = alloca %struct.ScmObj*, align 8
%fptrToInt54789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50816 to i64
%ae50816 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54789)
store volatile %struct.ScmObj* %ae50816, %struct.ScmObj** %stackaddr$makeclosure54788, align 8
%argslist53990$ae508140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%argslist53990$ae508141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50816, %struct.ScmObj* %argslist53990$ae508140)
store volatile %struct.ScmObj* %argslist53990$ae508141, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%argslist53990$ae508142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50815, %struct.ScmObj* %argslist53990$ae508141)
store volatile %struct.ScmObj* %argslist53990$ae508142, %struct.ScmObj** %stackaddr$prim54791, align 8
%clofunc54792 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50814)
musttail call tailcc void %clofunc54792(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist53990$ae508142)
ret void
}

define tailcc void @proc_clo$ae50814(%struct.ScmObj* %env$ae50814,%struct.ScmObj* %current_45args53890) {
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53890)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim54793, align 8
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%current_45args53891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53890)
store volatile %struct.ScmObj* %current_45args53891, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$prim54795 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54795, align 8
%ae50840 = call %struct.ScmObj* @const_init_true()
%truthy$cmp54796 = call i64 @is_truthy_value(%struct.ScmObj* %ae50840)
%cmp$cmp54796 = icmp eq i64 %truthy$cmp54796, 1
br i1 %cmp$cmp54796, label %truebranch$cmp54796, label %falsebranch$cmp54796
truebranch$cmp54796:
%stackaddr$makeclosure54797 = alloca %struct.ScmObj*, align 8
%fptrToInt54798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50841 to i64
%ae50841 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54798)
store volatile %struct.ScmObj* %ae50841, %struct.ScmObj** %stackaddr$makeclosure54797, align 8
%ae50842 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50843 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist53923$ae508410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%argslist53923$ae508411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50843, %struct.ScmObj* %argslist53923$ae508410)
store volatile %struct.ScmObj* %argslist53923$ae508411, %struct.ScmObj** %stackaddr$prim54799, align 8
%stackaddr$prim54800 = alloca %struct.ScmObj*, align 8
%argslist53923$ae508412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50842, %struct.ScmObj* %argslist53923$ae508411)
store volatile %struct.ScmObj* %argslist53923$ae508412, %struct.ScmObj** %stackaddr$prim54800, align 8
%clofunc54801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50841)
musttail call tailcc void %clofunc54801(%struct.ScmObj* %ae50841, %struct.ScmObj* %argslist53923$ae508412)
ret void
falsebranch$cmp54796:
%ae50988 = call %struct.ScmObj* @const_init_int(i64 20)
%truthy$cmp54802 = call i64 @is_truthy_value(%struct.ScmObj* %ae50988)
%cmp$cmp54802 = icmp eq i64 %truthy$cmp54802, 1
br i1 %cmp$cmp54802, label %truebranch$cmp54802, label %falsebranch$cmp54802
truebranch$cmp54802:
%stackaddr$makeclosure54803 = alloca %struct.ScmObj*, align 8
%fptrToInt54804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50989 to i64
%ae50989 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54804)
store volatile %struct.ScmObj* %ae50989, %struct.ScmObj** %stackaddr$makeclosure54803, align 8
%ae50990 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50991 = call %struct.ScmObj* @const_init_int(i64 20)
%argslist53954$ae509890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%argslist53954$ae509891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50991, %struct.ScmObj* %argslist53954$ae509890)
store volatile %struct.ScmObj* %argslist53954$ae509891, %struct.ScmObj** %stackaddr$prim54805, align 8
%stackaddr$prim54806 = alloca %struct.ScmObj*, align 8
%argslist53954$ae509892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50990, %struct.ScmObj* %argslist53954$ae509891)
store volatile %struct.ScmObj* %argslist53954$ae509892, %struct.ScmObj** %stackaddr$prim54806, align 8
%clofunc54807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50989)
musttail call tailcc void %clofunc54807(%struct.ScmObj* %ae50989, %struct.ScmObj* %argslist53954$ae509892)
ret void
falsebranch$cmp54802:
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%cpsprim47335 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim47335, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$makeclosure54809 = alloca %struct.ScmObj*, align 8
%fptrToInt54810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51136 to i64
%ae51136 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54810)
store volatile %struct.ScmObj* %ae51136, %struct.ScmObj** %stackaddr$makeclosure54809, align 8
%ae51137 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53985$ae511360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%argslist53985$ae511361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47335, %struct.ScmObj* %argslist53985$ae511360)
store volatile %struct.ScmObj* %argslist53985$ae511361, %struct.ScmObj** %stackaddr$prim54811, align 8
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%argslist53985$ae511362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51137, %struct.ScmObj* %argslist53985$ae511361)
store volatile %struct.ScmObj* %argslist53985$ae511362, %struct.ScmObj** %stackaddr$prim54812, align 8
%clofunc54813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51136)
musttail call tailcc void %clofunc54813(%struct.ScmObj* %ae51136, %struct.ScmObj* %argslist53985$ae511362)
ret void
}

define tailcc void @proc_clo$ae50841(%struct.ScmObj* %env$ae50841,%struct.ScmObj* %current_45args53893) {
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53893)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%current_45args53894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53893)
store volatile %struct.ScmObj* %current_45args53894, %struct.ScmObj** %stackaddr$prim54815, align 8
%stackaddr$prim54816 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53894)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54816, align 8
%ae50850 = call %struct.ScmObj* @const_init_false()
%truthy$cmp54817 = call i64 @is_truthy_value(%struct.ScmObj* %ae50850)
%cmp$cmp54817 = icmp eq i64 %truthy$cmp54817, 1
br i1 %cmp$cmp54817, label %truebranch$cmp54817, label %falsebranch$cmp54817
truebranch$cmp54817:
%stackaddr$makeclosure54818 = alloca %struct.ScmObj*, align 8
%fptrToInt54819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50851 to i64
%ae50851 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54819)
store volatile %struct.ScmObj* %ae50851, %struct.ScmObj** %stackaddr$makeclosure54818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50851, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae50852 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50853 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist53904$ae508510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%argslist53904$ae508511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50853, %struct.ScmObj* %argslist53904$ae508510)
store volatile %struct.ScmObj* %argslist53904$ae508511, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%argslist53904$ae508512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50852, %struct.ScmObj* %argslist53904$ae508511)
store volatile %struct.ScmObj* %argslist53904$ae508512, %struct.ScmObj** %stackaddr$prim54821, align 8
%clofunc54822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50851)
musttail call tailcc void %clofunc54822(%struct.ScmObj* %ae50851, %struct.ScmObj* %argslist53904$ae508512)
ret void
falsebranch$cmp54817:
%ae50877 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp54823 = call i64 @is_truthy_value(%struct.ScmObj* %ae50877)
%cmp$cmp54823 = icmp eq i64 %truthy$cmp54823, 1
br i1 %cmp$cmp54823, label %truebranch$cmp54823, label %falsebranch$cmp54823
truebranch$cmp54823:
%stackaddr$makeclosure54824 = alloca %struct.ScmObj*, align 8
%fptrToInt54825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50878 to i64
%ae50878 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54825)
store volatile %struct.ScmObj* %ae50878, %struct.ScmObj** %stackaddr$makeclosure54824, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50878, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae50879 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50880 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist53913$ae508780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%argslist53913$ae508781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50880, %struct.ScmObj* %argslist53913$ae508780)
store volatile %struct.ScmObj* %argslist53913$ae508781, %struct.ScmObj** %stackaddr$prim54826, align 8
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%argslist53913$ae508782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50879, %struct.ScmObj* %argslist53913$ae508781)
store volatile %struct.ScmObj* %argslist53913$ae508782, %struct.ScmObj** %stackaddr$prim54827, align 8
%clofunc54828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50878)
musttail call tailcc void %clofunc54828(%struct.ScmObj* %ae50878, %struct.ScmObj* %argslist53913$ae508782)
ret void
falsebranch$cmp54823:
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%cpsprim47334 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim47334, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$makeclosure54830 = alloca %struct.ScmObj*, align 8
%fptrToInt54831 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50904 to i64
%ae50904 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54831)
store volatile %struct.ScmObj* %ae50904, %struct.ScmObj** %stackaddr$makeclosure54830, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae50905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53922$ae509040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%argslist53922$ae509041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47334, %struct.ScmObj* %argslist53922$ae509040)
store volatile %struct.ScmObj* %argslist53922$ae509041, %struct.ScmObj** %stackaddr$prim54832, align 8
%stackaddr$prim54833 = alloca %struct.ScmObj*, align 8
%argslist53922$ae509042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50905, %struct.ScmObj* %argslist53922$ae509041)
store volatile %struct.ScmObj* %argslist53922$ae509042, %struct.ScmObj** %stackaddr$prim54833, align 8
%clofunc54834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50904)
musttail call tailcc void %clofunc54834(%struct.ScmObj* %ae50904, %struct.ScmObj* %argslist53922$ae509042)
ret void
}

define tailcc void @proc_clo$ae50851(%struct.ScmObj* %env$ae50851,%struct.ScmObj* %current_45args53896) {
%stackaddr$env-ref54835 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50851, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54835
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53896)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%current_45args53897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53896)
store volatile %struct.ScmObj* %current_45args53897, %struct.ScmObj** %stackaddr$prim54837, align 8
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53897)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54839, align 8
%stackaddr$makeclosure54840 = alloca %struct.ScmObj*, align 8
%fptrToInt54841 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50862 to i64
%ae50862 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54841)
store volatile %struct.ScmObj* %ae50862, %struct.ScmObj** %stackaddr$makeclosure54840, align 8
%ae50863 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53903$ae508620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%argslist53903$ae508621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53903$ae508620)
store volatile %struct.ScmObj* %argslist53903$ae508621, %struct.ScmObj** %stackaddr$prim54842, align 8
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%argslist53903$ae508622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50863, %struct.ScmObj* %argslist53903$ae508621)
store volatile %struct.ScmObj* %argslist53903$ae508622, %struct.ScmObj** %stackaddr$prim54843, align 8
%clofunc54844 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50862)
musttail call tailcc void %clofunc54844(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist53903$ae508622)
ret void
}

define tailcc void @proc_clo$ae50862(%struct.ScmObj* %env$ae50862,%struct.ScmObj* %current_45args53899) {
%stackaddr$prim54845 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53899)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54845, align 8
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%current_45args53900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53899)
store volatile %struct.ScmObj* %current_45args53900, %struct.ScmObj** %stackaddr$prim54846, align 8
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53900)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54847, align 8
%stackaddr$prim54848 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54848, align 8
%argslist53902$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%argslist53902$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53902$k0)
store volatile %struct.ScmObj* %argslist53902$k1, %struct.ScmObj** %stackaddr$prim54849, align 8
%clofunc54850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54850(%struct.ScmObj* %k, %struct.ScmObj* %argslist53902$k1)
ret void
}

define tailcc void @proc_clo$ae50878(%struct.ScmObj* %env$ae50878,%struct.ScmObj* %current_45args53905) {
%stackaddr$env-ref54851 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50878, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54851
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53905)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54852, align 8
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%current_45args53906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53905)
store volatile %struct.ScmObj* %current_45args53906, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53906)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$makeclosure54856 = alloca %struct.ScmObj*, align 8
%fptrToInt54857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50889 to i64
%ae50889 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54857)
store volatile %struct.ScmObj* %ae50889, %struct.ScmObj** %stackaddr$makeclosure54856, align 8
%ae50890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53912$ae508890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54858 = alloca %struct.ScmObj*, align 8
%argslist53912$ae508891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53912$ae508890)
store volatile %struct.ScmObj* %argslist53912$ae508891, %struct.ScmObj** %stackaddr$prim54858, align 8
%stackaddr$prim54859 = alloca %struct.ScmObj*, align 8
%argslist53912$ae508892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50890, %struct.ScmObj* %argslist53912$ae508891)
store volatile %struct.ScmObj* %argslist53912$ae508892, %struct.ScmObj** %stackaddr$prim54859, align 8
%clofunc54860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50889)
musttail call tailcc void %clofunc54860(%struct.ScmObj* %ae50889, %struct.ScmObj* %argslist53912$ae508892)
ret void
}

define tailcc void @proc_clo$ae50889(%struct.ScmObj* %env$ae50889,%struct.ScmObj* %current_45args53908) {
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53908)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54861, align 8
%stackaddr$prim54862 = alloca %struct.ScmObj*, align 8
%current_45args53909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53908)
store volatile %struct.ScmObj* %current_45args53909, %struct.ScmObj** %stackaddr$prim54862, align 8
%stackaddr$prim54863 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53909)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54863, align 8
%stackaddr$prim54864 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54864, align 8
%argslist53911$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54865 = alloca %struct.ScmObj*, align 8
%argslist53911$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53911$k0)
store volatile %struct.ScmObj* %argslist53911$k1, %struct.ScmObj** %stackaddr$prim54865, align 8
%clofunc54866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54866(%struct.ScmObj* %k, %struct.ScmObj* %argslist53911$k1)
ret void
}

define tailcc void @proc_clo$ae50904(%struct.ScmObj* %env$ae50904,%struct.ScmObj* %current_45args53914) {
%stackaddr$env-ref54867 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54867
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53914)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54868, align 8
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%current_45args53915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53914)
store volatile %struct.ScmObj* %current_45args53915, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53915)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54870, align 8
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$makeclosure54872 = alloca %struct.ScmObj*, align 8
%fptrToInt54873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50912 to i64
%ae50912 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54873)
store volatile %struct.ScmObj* %ae50912, %struct.ScmObj** %stackaddr$makeclosure54872, align 8
%ae50913 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53921$ae509120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%argslist53921$ae509121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53921$ae509120)
store volatile %struct.ScmObj* %argslist53921$ae509121, %struct.ScmObj** %stackaddr$prim54874, align 8
%stackaddr$prim54875 = alloca %struct.ScmObj*, align 8
%argslist53921$ae509122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50913, %struct.ScmObj* %argslist53921$ae509121)
store volatile %struct.ScmObj* %argslist53921$ae509122, %struct.ScmObj** %stackaddr$prim54875, align 8
%clofunc54876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50912)
musttail call tailcc void %clofunc54876(%struct.ScmObj* %ae50912, %struct.ScmObj* %argslist53921$ae509122)
ret void
}

define tailcc void @proc_clo$ae50912(%struct.ScmObj* %env$ae50912,%struct.ScmObj* %current_45args53917) {
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53917)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%current_45args53918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53917)
store volatile %struct.ScmObj* %current_45args53918, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53918)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54880, align 8
%argslist53920$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%argslist53920$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53920$k0)
store volatile %struct.ScmObj* %argslist53920$k1, %struct.ScmObj** %stackaddr$prim54881, align 8
%clofunc54882 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54882(%struct.ScmObj* %k, %struct.ScmObj* %argslist53920$k1)
ret void
}

define tailcc void @proc_clo$ae50989(%struct.ScmObj* %env$ae50989,%struct.ScmObj* %current_45args53924) {
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53924)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54883, align 8
%stackaddr$prim54884 = alloca %struct.ScmObj*, align 8
%current_45args53925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53924)
store volatile %struct.ScmObj* %current_45args53925, %struct.ScmObj** %stackaddr$prim54884, align 8
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53925)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54885, align 8
%ae50998 = call %struct.ScmObj* @const_init_false()
%truthy$cmp54886 = call i64 @is_truthy_value(%struct.ScmObj* %ae50998)
%cmp$cmp54886 = icmp eq i64 %truthy$cmp54886, 1
br i1 %cmp$cmp54886, label %truebranch$cmp54886, label %falsebranch$cmp54886
truebranch$cmp54886:
%stackaddr$makeclosure54887 = alloca %struct.ScmObj*, align 8
%fptrToInt54888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50999 to i64
%ae50999 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54888)
store volatile %struct.ScmObj* %ae50999, %struct.ScmObj** %stackaddr$makeclosure54887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50999, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51000 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51001 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist53935$ae509990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%argslist53935$ae509991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51001, %struct.ScmObj* %argslist53935$ae509990)
store volatile %struct.ScmObj* %argslist53935$ae509991, %struct.ScmObj** %stackaddr$prim54889, align 8
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%argslist53935$ae509992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51000, %struct.ScmObj* %argslist53935$ae509991)
store volatile %struct.ScmObj* %argslist53935$ae509992, %struct.ScmObj** %stackaddr$prim54890, align 8
%clofunc54891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50999)
musttail call tailcc void %clofunc54891(%struct.ScmObj* %ae50999, %struct.ScmObj* %argslist53935$ae509992)
ret void
falsebranch$cmp54886:
%ae51025 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp54892 = call i64 @is_truthy_value(%struct.ScmObj* %ae51025)
%cmp$cmp54892 = icmp eq i64 %truthy$cmp54892, 1
br i1 %cmp$cmp54892, label %truebranch$cmp54892, label %falsebranch$cmp54892
truebranch$cmp54892:
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51026 to i64
%ae51026 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae51026, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51026, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51027 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51028 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist53944$ae510260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%argslist53944$ae510261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51028, %struct.ScmObj* %argslist53944$ae510260)
store volatile %struct.ScmObj* %argslist53944$ae510261, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%argslist53944$ae510262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51027, %struct.ScmObj* %argslist53944$ae510261)
store volatile %struct.ScmObj* %argslist53944$ae510262, %struct.ScmObj** %stackaddr$prim54896, align 8
%clofunc54897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51026)
musttail call tailcc void %clofunc54897(%struct.ScmObj* %ae51026, %struct.ScmObj* %argslist53944$ae510262)
ret void
falsebranch$cmp54892:
%stackaddr$prim54898 = alloca %struct.ScmObj*, align 8
%cpsprim47334 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim47334, %struct.ScmObj** %stackaddr$prim54898, align 8
%stackaddr$makeclosure54899 = alloca %struct.ScmObj*, align 8
%fptrToInt54900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51052 to i64
%ae51052 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54900)
store volatile %struct.ScmObj* %ae51052, %struct.ScmObj** %stackaddr$makeclosure54899, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51052, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51053 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53953$ae510520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%argslist53953$ae510521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47334, %struct.ScmObj* %argslist53953$ae510520)
store volatile %struct.ScmObj* %argslist53953$ae510521, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%argslist53953$ae510522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51053, %struct.ScmObj* %argslist53953$ae510521)
store volatile %struct.ScmObj* %argslist53953$ae510522, %struct.ScmObj** %stackaddr$prim54902, align 8
%clofunc54903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51052)
musttail call tailcc void %clofunc54903(%struct.ScmObj* %ae51052, %struct.ScmObj* %argslist53953$ae510522)
ret void
}

define tailcc void @proc_clo$ae50999(%struct.ScmObj* %env$ae50999,%struct.ScmObj* %current_45args53927) {
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50999, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53927)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%current_45args53928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53927)
store volatile %struct.ScmObj* %current_45args53928, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53928)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$makeclosure54909 = alloca %struct.ScmObj*, align 8
%fptrToInt54910 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51010 to i64
%ae51010 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54910)
store volatile %struct.ScmObj* %ae51010, %struct.ScmObj** %stackaddr$makeclosure54909, align 8
%ae51011 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53934$ae510100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54911 = alloca %struct.ScmObj*, align 8
%argslist53934$ae510101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53934$ae510100)
store volatile %struct.ScmObj* %argslist53934$ae510101, %struct.ScmObj** %stackaddr$prim54911, align 8
%stackaddr$prim54912 = alloca %struct.ScmObj*, align 8
%argslist53934$ae510102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51011, %struct.ScmObj* %argslist53934$ae510101)
store volatile %struct.ScmObj* %argslist53934$ae510102, %struct.ScmObj** %stackaddr$prim54912, align 8
%clofunc54913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51010)
musttail call tailcc void %clofunc54913(%struct.ScmObj* %ae51010, %struct.ScmObj* %argslist53934$ae510102)
ret void
}

define tailcc void @proc_clo$ae51010(%struct.ScmObj* %env$ae51010,%struct.ScmObj* %current_45args53930) {
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$prim54915 = alloca %struct.ScmObj*, align 8
%current_45args53931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %current_45args53931, %struct.ScmObj** %stackaddr$prim54915, align 8
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53931)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54916, align 8
%stackaddr$prim54917 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54917, align 8
%argslist53933$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54918 = alloca %struct.ScmObj*, align 8
%argslist53933$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53933$k0)
store volatile %struct.ScmObj* %argslist53933$k1, %struct.ScmObj** %stackaddr$prim54918, align 8
%clofunc54919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54919(%struct.ScmObj* %k, %struct.ScmObj* %argslist53933$k1)
ret void
}

define tailcc void @proc_clo$ae51026(%struct.ScmObj* %env$ae51026,%struct.ScmObj* %current_45args53936) {
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51026, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53936)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%current_45args53937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53936)
store volatile %struct.ScmObj* %current_45args53937, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53937)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$makeclosure54925 = alloca %struct.ScmObj*, align 8
%fptrToInt54926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51037 to i64
%ae51037 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54926)
store volatile %struct.ScmObj* %ae51037, %struct.ScmObj** %stackaddr$makeclosure54925, align 8
%ae51038 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53943$ae510370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%argslist53943$ae510371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53943$ae510370)
store volatile %struct.ScmObj* %argslist53943$ae510371, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist53943$ae510372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51038, %struct.ScmObj* %argslist53943$ae510371)
store volatile %struct.ScmObj* %argslist53943$ae510372, %struct.ScmObj** %stackaddr$prim54928, align 8
%clofunc54929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51037)
musttail call tailcc void %clofunc54929(%struct.ScmObj* %ae51037, %struct.ScmObj* %argslist53943$ae510372)
ret void
}

define tailcc void @proc_clo$ae51037(%struct.ScmObj* %env$ae51037,%struct.ScmObj* %current_45args53939) {
%stackaddr$prim54930 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53939)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54930, align 8
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%current_45args53940 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53939)
store volatile %struct.ScmObj* %current_45args53940, %struct.ScmObj** %stackaddr$prim54931, align 8
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53940)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54932, align 8
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54933, align 8
%argslist53942$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%argslist53942$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53942$k0)
store volatile %struct.ScmObj* %argslist53942$k1, %struct.ScmObj** %stackaddr$prim54934, align 8
%clofunc54935 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54935(%struct.ScmObj* %k, %struct.ScmObj* %argslist53942$k1)
ret void
}

define tailcc void @proc_clo$ae51052(%struct.ScmObj* %env$ae51052,%struct.ScmObj* %current_45args53945) {
%stackaddr$env-ref54936 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51052, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54936
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53945)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%current_45args53946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53945)
store volatile %struct.ScmObj* %current_45args53946, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53946)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$makeclosure54941 = alloca %struct.ScmObj*, align 8
%fptrToInt54942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51060 to i64
%ae51060 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54942)
store volatile %struct.ScmObj* %ae51060, %struct.ScmObj** %stackaddr$makeclosure54941, align 8
%ae51061 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53952$ae510600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%argslist53952$ae510601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53952$ae510600)
store volatile %struct.ScmObj* %argslist53952$ae510601, %struct.ScmObj** %stackaddr$prim54943, align 8
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%argslist53952$ae510602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51061, %struct.ScmObj* %argslist53952$ae510601)
store volatile %struct.ScmObj* %argslist53952$ae510602, %struct.ScmObj** %stackaddr$prim54944, align 8
%clofunc54945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51060)
musttail call tailcc void %clofunc54945(%struct.ScmObj* %ae51060, %struct.ScmObj* %argslist53952$ae510602)
ret void
}

define tailcc void @proc_clo$ae51060(%struct.ScmObj* %env$ae51060,%struct.ScmObj* %current_45args53948) {
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53948)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%current_45args53949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53948)
store volatile %struct.ScmObj* %current_45args53949, %struct.ScmObj** %stackaddr$prim54947, align 8
%stackaddr$prim54948 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53949)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54948, align 8
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54949, align 8
%argslist53951$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%argslist53951$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53951$k0)
store volatile %struct.ScmObj* %argslist53951$k1, %struct.ScmObj** %stackaddr$prim54950, align 8
%clofunc54951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54951(%struct.ScmObj* %k, %struct.ScmObj* %argslist53951$k1)
ret void
}

define tailcc void @proc_clo$ae51136(%struct.ScmObj* %env$ae51136,%struct.ScmObj* %current_45args53955) {
%stackaddr$prim54952 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53955)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54952, align 8
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%current_45args53956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53955)
store volatile %struct.ScmObj* %current_45args53956, %struct.ScmObj** %stackaddr$prim54953, align 8
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53956)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim54954, align 8
%ae51142 = call %struct.ScmObj* @const_init_false()
%truthy$cmp54955 = call i64 @is_truthy_value(%struct.ScmObj* %ae51142)
%cmp$cmp54955 = icmp eq i64 %truthy$cmp54955, 1
br i1 %cmp$cmp54955, label %truebranch$cmp54955, label %falsebranch$cmp54955
truebranch$cmp54955:
%stackaddr$makeclosure54956 = alloca %struct.ScmObj*, align 8
%fptrToInt54957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51143 to i64
%ae51143 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54957)
store volatile %struct.ScmObj* %ae51143, %struct.ScmObj** %stackaddr$makeclosure54956, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51143, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51144 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51145 = call %struct.ScmObj* @const_init_int(i64 10)
%argslist53966$ae511430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%argslist53966$ae511431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51145, %struct.ScmObj* %argslist53966$ae511430)
store volatile %struct.ScmObj* %argslist53966$ae511431, %struct.ScmObj** %stackaddr$prim54958, align 8
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%argslist53966$ae511432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51144, %struct.ScmObj* %argslist53966$ae511431)
store volatile %struct.ScmObj* %argslist53966$ae511432, %struct.ScmObj** %stackaddr$prim54959, align 8
%clofunc54960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51143)
musttail call tailcc void %clofunc54960(%struct.ScmObj* %ae51143, %struct.ScmObj* %argslist53966$ae511432)
ret void
falsebranch$cmp54955:
%ae51169 = call %struct.ScmObj* @const_init_int(i64 30)
%truthy$cmp54961 = call i64 @is_truthy_value(%struct.ScmObj* %ae51169)
%cmp$cmp54961 = icmp eq i64 %truthy$cmp54961, 1
br i1 %cmp$cmp54961, label %truebranch$cmp54961, label %falsebranch$cmp54961
truebranch$cmp54961:
%stackaddr$makeclosure54962 = alloca %struct.ScmObj*, align 8
%fptrToInt54963 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51170 to i64
%ae51170 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54963)
store volatile %struct.ScmObj* %ae51170, %struct.ScmObj** %stackaddr$makeclosure54962, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51171 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51172 = call %struct.ScmObj* @const_init_int(i64 30)
%argslist53975$ae511700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%argslist53975$ae511701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51172, %struct.ScmObj* %argslist53975$ae511700)
store volatile %struct.ScmObj* %argslist53975$ae511701, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%argslist53975$ae511702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51171, %struct.ScmObj* %argslist53975$ae511701)
store volatile %struct.ScmObj* %argslist53975$ae511702, %struct.ScmObj** %stackaddr$prim54965, align 8
%clofunc54966 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51170)
musttail call tailcc void %clofunc54966(%struct.ScmObj* %ae51170, %struct.ScmObj* %argslist53975$ae511702)
ret void
falsebranch$cmp54961:
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%cpsprim47334 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %cpsprim47334, %struct.ScmObj** %stackaddr$prim54967, align 8
%stackaddr$makeclosure54968 = alloca %struct.ScmObj*, align 8
%fptrToInt54969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51196 to i64
%ae51196 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54969)
store volatile %struct.ScmObj* %ae51196, %struct.ScmObj** %stackaddr$makeclosure54968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51196, %struct.ScmObj* %anf_45bind47297, i64 0)
%ae51197 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53984$ae511960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%argslist53984$ae511961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47334, %struct.ScmObj* %argslist53984$ae511960)
store volatile %struct.ScmObj* %argslist53984$ae511961, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%argslist53984$ae511962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51197, %struct.ScmObj* %argslist53984$ae511961)
store volatile %struct.ScmObj* %argslist53984$ae511962, %struct.ScmObj** %stackaddr$prim54971, align 8
%clofunc54972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51196)
musttail call tailcc void %clofunc54972(%struct.ScmObj* %ae51196, %struct.ScmObj* %argslist53984$ae511962)
ret void
}

define tailcc void @proc_clo$ae51143(%struct.ScmObj* %env$ae51143,%struct.ScmObj* %current_45args53958) {
%stackaddr$env-ref54973 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51143, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54973
%stackaddr$prim54974 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53958)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54974, align 8
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%current_45args53959 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53958)
store volatile %struct.ScmObj* %current_45args53959, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$prim54976 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53959)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54976, align 8
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$makeclosure54978 = alloca %struct.ScmObj*, align 8
%fptrToInt54979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51154 to i64
%ae51154 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54979)
store volatile %struct.ScmObj* %ae51154, %struct.ScmObj** %stackaddr$makeclosure54978, align 8
%ae51155 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53965$ae511540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%argslist53965$ae511541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53965$ae511540)
store volatile %struct.ScmObj* %argslist53965$ae511541, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%argslist53965$ae511542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51155, %struct.ScmObj* %argslist53965$ae511541)
store volatile %struct.ScmObj* %argslist53965$ae511542, %struct.ScmObj** %stackaddr$prim54981, align 8
%clofunc54982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51154)
musttail call tailcc void %clofunc54982(%struct.ScmObj* %ae51154, %struct.ScmObj* %argslist53965$ae511542)
ret void
}

define tailcc void @proc_clo$ae51154(%struct.ScmObj* %env$ae51154,%struct.ScmObj* %current_45args53961) {
%stackaddr$prim54983 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54983, align 8
%stackaddr$prim54984 = alloca %struct.ScmObj*, align 8
%current_45args53962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %current_45args53962, %struct.ScmObj** %stackaddr$prim54984, align 8
%stackaddr$prim54985 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53962)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54985, align 8
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54986, align 8
%argslist53964$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%argslist53964$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53964$k0)
store volatile %struct.ScmObj* %argslist53964$k1, %struct.ScmObj** %stackaddr$prim54987, align 8
%clofunc54988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54988(%struct.ScmObj* %k, %struct.ScmObj* %argslist53964$k1)
ret void
}

define tailcc void @proc_clo$ae51170(%struct.ScmObj* %env$ae51170,%struct.ScmObj* %current_45args53967) {
%stackaddr$env-ref54989 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref54989
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53967)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%current_45args53968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53967)
store volatile %struct.ScmObj* %current_45args53968, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$prim54992 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53968)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim54992, align 8
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$makeclosure54994 = alloca %struct.ScmObj*, align 8
%fptrToInt54995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51181 to i64
%ae51181 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54995)
store volatile %struct.ScmObj* %ae51181, %struct.ScmObj** %stackaddr$makeclosure54994, align 8
%ae51182 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53974$ae511810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%argslist53974$ae511811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53974$ae511810)
store volatile %struct.ScmObj* %argslist53974$ae511811, %struct.ScmObj** %stackaddr$prim54996, align 8
%stackaddr$prim54997 = alloca %struct.ScmObj*, align 8
%argslist53974$ae511812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51182, %struct.ScmObj* %argslist53974$ae511811)
store volatile %struct.ScmObj* %argslist53974$ae511812, %struct.ScmObj** %stackaddr$prim54997, align 8
%clofunc54998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51181)
musttail call tailcc void %clofunc54998(%struct.ScmObj* %ae51181, %struct.ScmObj* %argslist53974$ae511812)
ret void
}

define tailcc void @proc_clo$ae51181(%struct.ScmObj* %env$ae51181,%struct.ScmObj* %current_45args53970) {
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53970)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args53971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53970)
store volatile %struct.ScmObj* %current_45args53971, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53971)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55001, align 8
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55002, align 8
%argslist53973$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55003 = alloca %struct.ScmObj*, align 8
%argslist53973$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53973$k0)
store volatile %struct.ScmObj* %argslist53973$k1, %struct.ScmObj** %stackaddr$prim55003, align 8
%clofunc55004 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55004(%struct.ScmObj* %k, %struct.ScmObj* %argslist53973$k1)
ret void
}

define tailcc void @proc_clo$ae51196(%struct.ScmObj* %env$ae51196,%struct.ScmObj* %current_45args53976) {
%stackaddr$env-ref55005 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51196, i64 0)
store %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$env-ref55005
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53976)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%current_45args53977 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53976)
store volatile %struct.ScmObj* %current_45args53977, %struct.ScmObj** %stackaddr$prim55007, align 8
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53977)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%cpsprim47333 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %cpsprim47333, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$makeclosure55010 = alloca %struct.ScmObj*, align 8
%fptrToInt55011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51204 to i64
%ae51204 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55011)
store volatile %struct.ScmObj* %ae51204, %struct.ScmObj** %stackaddr$makeclosure55010, align 8
%ae51205 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53983$ae512040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%argslist53983$ae512041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47333, %struct.ScmObj* %argslist53983$ae512040)
store volatile %struct.ScmObj* %argslist53983$ae512041, %struct.ScmObj** %stackaddr$prim55012, align 8
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%argslist53983$ae512042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51205, %struct.ScmObj* %argslist53983$ae512041)
store volatile %struct.ScmObj* %argslist53983$ae512042, %struct.ScmObj** %stackaddr$prim55013, align 8
%clofunc55014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51204)
musttail call tailcc void %clofunc55014(%struct.ScmObj* %ae51204, %struct.ScmObj* %argslist53983$ae512042)
ret void
}

define tailcc void @proc_clo$ae51204(%struct.ScmObj* %env$ae51204,%struct.ScmObj* %current_45args53979) {
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53979)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%current_45args53980 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53979)
store volatile %struct.ScmObj* %current_45args53980, %struct.ScmObj** %stackaddr$prim55016, align 8
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53980)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55017, align 8
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55018, align 8
%argslist53982$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%argslist53982$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53982$k0)
store volatile %struct.ScmObj* %argslist53982$k1, %struct.ScmObj** %stackaddr$prim55019, align 8
%clofunc55020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55020(%struct.ScmObj* %k, %struct.ScmObj* %argslist53982$k1)
ret void
}

define tailcc void @proc_clo$ae50816(%struct.ScmObj* %env$ae50816,%struct.ScmObj* %current_45args53986) {
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53986)
store volatile %struct.ScmObj* %k47336, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%current_45args53987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53986)
store volatile %struct.ScmObj* %current_45args53987, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53987)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim55023, align 8
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim55024, align 8
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim55025, align 8
%stackaddr$prim55026 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim55026, align 8
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%cpsprim47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %cpsprim47337, %struct.ScmObj** %stackaddr$prim55027, align 8
%ae50822 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53989$k473360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%argslist53989$k473361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47337, %struct.ScmObj* %argslist53989$k473360)
store volatile %struct.ScmObj* %argslist53989$k473361, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%argslist53989$k473362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50822, %struct.ScmObj* %argslist53989$k473361)
store volatile %struct.ScmObj* %argslist53989$k473362, %struct.ScmObj** %stackaddr$prim55029, align 8
%clofunc55030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47336)
musttail call tailcc void %clofunc55030(%struct.ScmObj* %k47336, %struct.ScmObj* %argslist53989$k473362)
ret void
}

define tailcc void @proc_clo$ae50792(%struct.ScmObj* %env$ae50792,%struct.ScmObj* %current_45args53991) {
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %k47338, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%current_45args53992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %current_45args53992, %struct.ScmObj** %stackaddr$prim55032, align 8
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53992)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim55035, align 8
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%cpsprim47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %cpsprim47339, %struct.ScmObj** %stackaddr$prim55036, align 8
%ae50797 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53994$k473380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%argslist53994$k473381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47339, %struct.ScmObj* %argslist53994$k473380)
store volatile %struct.ScmObj* %argslist53994$k473381, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%argslist53994$k473382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50797, %struct.ScmObj* %argslist53994$k473381)
store volatile %struct.ScmObj* %argslist53994$k473382, %struct.ScmObj** %stackaddr$prim55038, align 8
%clofunc55039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47338)
musttail call tailcc void %clofunc55039(%struct.ScmObj* %k47338, %struct.ScmObj* %argslist53994$k473382)
ret void
}

define tailcc void @proc_clo$ae50770(%struct.ScmObj* %env$ae50770,%struct.ScmObj* %current_45args53996) {
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %k47340, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%current_45args53997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %current_45args53997, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53997)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%cpsprim47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %cpsprim47341, %struct.ScmObj** %stackaddr$prim55044, align 8
%ae50774 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53999$k473400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55045 = alloca %struct.ScmObj*, align 8
%argslist53999$k473401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47341, %struct.ScmObj* %argslist53999$k473400)
store volatile %struct.ScmObj* %argslist53999$k473401, %struct.ScmObj** %stackaddr$prim55045, align 8
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist53999$k473402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50774, %struct.ScmObj* %argslist53999$k473401)
store volatile %struct.ScmObj* %argslist53999$k473402, %struct.ScmObj** %stackaddr$prim55046, align 8
%clofunc55047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47340)
musttail call tailcc void %clofunc55047(%struct.ScmObj* %k47340, %struct.ScmObj* %argslist53999$k473402)
ret void
}

define tailcc void @proc_clo$ae50750(%struct.ScmObj* %env$ae50750,%struct.ScmObj* %current_45args54001) {
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54001)
store volatile %struct.ScmObj* %k47342, %struct.ScmObj** %stackaddr$prim55048, align 8
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%current_45args54002 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54001)
store volatile %struct.ScmObj* %current_45args54002, %struct.ScmObj** %stackaddr$prim55049, align 8
%stackaddr$prim55050 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54002)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim55050, align 8
%stackaddr$prim55051 = alloca %struct.ScmObj*, align 8
%cpsprim47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47343, %struct.ScmObj** %stackaddr$prim55051, align 8
%ae50753 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54004$k473420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%argslist54004$k473421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47343, %struct.ScmObj* %argslist54004$k473420)
store volatile %struct.ScmObj* %argslist54004$k473421, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%argslist54004$k473422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50753, %struct.ScmObj* %argslist54004$k473421)
store volatile %struct.ScmObj* %argslist54004$k473422, %struct.ScmObj** %stackaddr$prim55053, align 8
%clofunc55054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47342)
musttail call tailcc void %clofunc55054(%struct.ScmObj* %k47342, %struct.ScmObj* %argslist54004$k473422)
ret void
}

define tailcc void @proc_clo$ae50652(%struct.ScmObj* %env$ae50652,%struct.ScmObj* %args4713447344) {
%stackaddr$env-ref55055 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50652, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55055
%stackaddr$prim55056 = alloca %struct.ScmObj*, align 8
%k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447344)
store volatile %struct.ScmObj* %k47345, %struct.ScmObj** %stackaddr$prim55056, align 8
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447344)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55058, align 8
%truthy$cmp55059 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp55059 = icmp eq i64 %truthy$cmp55059, 1
br i1 %cmp$cmp55059, label %truebranch$cmp55059, label %falsebranch$cmp55059
truebranch$cmp55059:
%ae50658 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50659 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54006$k473450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%argslist54006$k473451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50659, %struct.ScmObj* %argslist54006$k473450)
store volatile %struct.ScmObj* %argslist54006$k473451, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%argslist54006$k473452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50658, %struct.ScmObj* %argslist54006$k473451)
store volatile %struct.ScmObj* %argslist54006$k473452, %struct.ScmObj** %stackaddr$prim55061, align 8
%clofunc55062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47345)
musttail call tailcc void %clofunc55062(%struct.ScmObj* %k47345, %struct.ScmObj* %argslist54006$k473452)
ret void
falsebranch$cmp55059:
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim55063, align 8
%stackaddr$prim55064 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim55064, align 8
%truthy$cmp55065 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp55065 = icmp eq i64 %truthy$cmp55065, 1
br i1 %cmp$cmp55065, label %truebranch$cmp55065, label %falsebranch$cmp55065
truebranch$cmp55065:
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%cpsprim47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47346, %struct.ScmObj** %stackaddr$prim55066, align 8
%ae50671 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54007$k473450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%argslist54007$k473451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47346, %struct.ScmObj* %argslist54007$k473450)
store volatile %struct.ScmObj* %argslist54007$k473451, %struct.ScmObj** %stackaddr$prim55067, align 8
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%argslist54007$k473452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50671, %struct.ScmObj* %argslist54007$k473451)
store volatile %struct.ScmObj* %argslist54007$k473452, %struct.ScmObj** %stackaddr$prim55068, align 8
%clofunc55069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47345)
musttail call tailcc void %clofunc55069(%struct.ScmObj* %k47345, %struct.ScmObj* %argslist54007$k473452)
ret void
falsebranch$cmp55065:
%stackaddr$makeclosure55070 = alloca %struct.ScmObj*, align 8
%fptrToInt55071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50676 to i64
%ae50676 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55071)
store volatile %struct.ScmObj* %ae50676, %struct.ScmObj** %stackaddr$makeclosure55070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50676, %struct.ScmObj* %k47345, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50676, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50676, %struct.ScmObj* %args47134, i64 2)
%ae50677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55072 = alloca %struct.ScmObj*, align 8
%fptrToInt55073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50678 to i64
%ae50678 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55073)
store volatile %struct.ScmObj* %ae50678, %struct.ScmObj** %stackaddr$makeclosure55072, align 8
%argslist54017$ae506760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%argslist54017$ae506761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50678, %struct.ScmObj* %argslist54017$ae506760)
store volatile %struct.ScmObj* %argslist54017$ae506761, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%argslist54017$ae506762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50677, %struct.ScmObj* %argslist54017$ae506761)
store volatile %struct.ScmObj* %argslist54017$ae506762, %struct.ScmObj** %stackaddr$prim55075, align 8
%clofunc55076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50676)
musttail call tailcc void %clofunc55076(%struct.ScmObj* %ae50676, %struct.ScmObj* %argslist54017$ae506762)
ret void
}

define tailcc void @proc_clo$ae50676(%struct.ScmObj* %env$ae50676,%struct.ScmObj* %current_45args54008) {
%stackaddr$env-ref55077 = alloca %struct.ScmObj*, align 8
%k47345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50676, i64 0)
store %struct.ScmObj* %k47345, %struct.ScmObj** %stackaddr$env-ref55077
%stackaddr$env-ref55078 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50676, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55078
%stackaddr$env-ref55079 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50676, i64 2)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref55079
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54008)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%current_45args54009 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54008)
store volatile %struct.ScmObj* %current_45args54009, %struct.ScmObj** %stackaddr$prim55081, align 8
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54009)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$prim55084 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim55084, align 8
%argslist54011$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%argslist54011$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47290, %struct.ScmObj* %argslist54011$_37foldl1470730)
store volatile %struct.ScmObj* %argslist54011$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%argslist54011$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47289, %struct.ScmObj* %argslist54011$_37foldl1470731)
store volatile %struct.ScmObj* %argslist54011$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55086, align 8
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%argslist54011$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47288, %struct.ScmObj* %argslist54011$_37foldl1470732)
store volatile %struct.ScmObj* %argslist54011$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist54011$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47345, %struct.ScmObj* %argslist54011$_37foldl1470733)
store volatile %struct.ScmObj* %argslist54011$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55088, align 8
%clofunc55089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55089(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist54011$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50678(%struct.ScmObj* %env$ae50678,%struct.ScmObj* %current_45args54012) {
%stackaddr$prim55090 = alloca %struct.ScmObj*, align 8
%k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54012)
store volatile %struct.ScmObj* %k47348, %struct.ScmObj** %stackaddr$prim55090, align 8
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%current_45args54013 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54012)
store volatile %struct.ScmObj* %current_45args54013, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54013)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%current_45args54014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54013)
store volatile %struct.ScmObj* %current_45args54014, %struct.ScmObj** %stackaddr$prim55093, align 8
%stackaddr$prim55094 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54014)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim55094, align 8
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%cpsprim47349 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47349, %struct.ScmObj** %stackaddr$prim55095, align 8
%ae50682 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54016$k473480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%argslist54016$k473481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47349, %struct.ScmObj* %argslist54016$k473480)
store volatile %struct.ScmObj* %argslist54016$k473481, %struct.ScmObj** %stackaddr$prim55096, align 8
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%argslist54016$k473482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50682, %struct.ScmObj* %argslist54016$k473481)
store volatile %struct.ScmObj* %argslist54016$k473482, %struct.ScmObj** %stackaddr$prim55097, align 8
%clofunc55098 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47348)
musttail call tailcc void %clofunc55098(%struct.ScmObj* %k47348, %struct.ScmObj* %argslist54016$k473482)
ret void
}

define tailcc void @proc_clo$ae50248(%struct.ScmObj* %env$ae50248,%struct.ScmObj* %current_45args54019) {
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54019)
store volatile %struct.ScmObj* %k47350, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%current_45args54020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54019)
store volatile %struct.ScmObj* %current_45args54020, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54020)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim55101, align 8
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%current_45args54021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54020)
store volatile %struct.ScmObj* %current_45args54021, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54021)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim55103, align 8
%ae50249 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55104 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50249, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim55104, align 8
%stackaddr$makeclosure55105 = alloca %struct.ScmObj*, align 8
%fptrToInt55106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50251 to i64
%ae50251 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55106)
store volatile %struct.ScmObj* %ae50251, %struct.ScmObj** %stackaddr$makeclosure55105, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50251, %struct.ScmObj* %k47350, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50251, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50251, %struct.ScmObj* %v47139, i64 2)
%ae50252 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55107 = alloca %struct.ScmObj*, align 8
%fptrToInt55108 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50253 to i64
%ae50253 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55108)
store volatile %struct.ScmObj* %ae50253, %struct.ScmObj** %stackaddr$makeclosure55107, align 8
%argslist54043$ae502510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%argslist54043$ae502511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50253, %struct.ScmObj* %argslist54043$ae502510)
store volatile %struct.ScmObj* %argslist54043$ae502511, %struct.ScmObj** %stackaddr$prim55109, align 8
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%argslist54043$ae502512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50252, %struct.ScmObj* %argslist54043$ae502511)
store volatile %struct.ScmObj* %argslist54043$ae502512, %struct.ScmObj** %stackaddr$prim55110, align 8
%clofunc55111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50251)
musttail call tailcc void %clofunc55111(%struct.ScmObj* %ae50251, %struct.ScmObj* %argslist54043$ae502512)
ret void
}

define tailcc void @proc_clo$ae50251(%struct.ScmObj* %env$ae50251,%struct.ScmObj* %current_45args54023) {
%stackaddr$env-ref55112 = alloca %struct.ScmObj*, align 8
%k47350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50251, i64 0)
store %struct.ScmObj* %k47350, %struct.ScmObj** %stackaddr$env-ref55112
%stackaddr$env-ref55113 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50251, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55113
%stackaddr$env-ref55114 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50251, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55114
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim55115, align 8
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%current_45args54024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %current_45args54024, %struct.ScmObj** %stackaddr$prim55116, align 8
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54024)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim55117, align 8
%stackaddr$makeclosure55118 = alloca %struct.ScmObj*, align 8
%fptrToInt55119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50267 to i64
%ae50267 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55119)
store volatile %struct.ScmObj* %ae50267, %struct.ScmObj** %stackaddr$makeclosure55118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50267, %struct.ScmObj* %k47350, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50267, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50267, %struct.ScmObj* %v47139, i64 2)
%stackaddr$makeclosure55120 = alloca %struct.ScmObj*, align 8
%fptrToInt55121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50268 to i64
%ae50268 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55121)
store volatile %struct.ScmObj* %ae50268, %struct.ScmObj** %stackaddr$makeclosure55120, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %k47350, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50268, %struct.ScmObj* %v47139, i64 2)
%argslist54038$anf_45bind472770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%argslist54038$anf_45bind472771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50268, %struct.ScmObj* %argslist54038$anf_45bind472770)
store volatile %struct.ScmObj* %argslist54038$anf_45bind472771, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%argslist54038$anf_45bind472772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50267, %struct.ScmObj* %argslist54038$anf_45bind472771)
store volatile %struct.ScmObj* %argslist54038$anf_45bind472772, %struct.ScmObj** %stackaddr$prim55123, align 8
%clofunc55124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47277)
musttail call tailcc void %clofunc55124(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %argslist54038$anf_45bind472772)
ret void
}

define tailcc void @proc_clo$ae50267(%struct.ScmObj* %env$ae50267,%struct.ScmObj* %current_45args54026) {
%stackaddr$env-ref55125 = alloca %struct.ScmObj*, align 8
%k47350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50267, i64 0)
store %struct.ScmObj* %k47350, %struct.ScmObj** %stackaddr$env-ref55125
%stackaddr$env-ref55126 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50267, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55126
%stackaddr$env-ref55127 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50267, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55127
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54026)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%current_45args54027 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54026)
store volatile %struct.ScmObj* %current_45args54027, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54027)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim55130, align 8
%ae50376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50376)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim55132, align 8
%truthy$cmp55133 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47279)
%cmp$cmp55133 = icmp eq i64 %truthy$cmp55133, 1
br i1 %cmp$cmp55133, label %truebranch$cmp55133, label %falsebranch$cmp55133
truebranch$cmp55133:
%ae50380 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50381 = call %struct.ScmObj* @const_init_false()
%argslist54029$k473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%argslist54029$k473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50381, %struct.ScmObj* %argslist54029$k473500)
store volatile %struct.ScmObj* %argslist54029$k473501, %struct.ScmObj** %stackaddr$prim55134, align 8
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%argslist54029$k473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50380, %struct.ScmObj* %argslist54029$k473501)
store volatile %struct.ScmObj* %argslist54029$k473502, %struct.ScmObj** %stackaddr$prim55135, align 8
%clofunc55136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47350)
musttail call tailcc void %clofunc55136(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54029$k473502)
ret void
falsebranch$cmp55133:
%ae50389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50389)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim55139, align 8
%truthy$cmp55140 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47282)
%cmp$cmp55140 = icmp eq i64 %truthy$cmp55140, 1
br i1 %cmp$cmp55140, label %truebranch$cmp55140, label %falsebranch$cmp55140
truebranch$cmp55140:
%ae50395 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%cpsprim47353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50395)
store volatile %struct.ScmObj* %cpsprim47353, %struct.ScmObj** %stackaddr$prim55141, align 8
%ae50397 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54030$k473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%argslist54030$k473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47353, %struct.ScmObj* %argslist54030$k473500)
store volatile %struct.ScmObj* %argslist54030$k473501, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%argslist54030$k473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50397, %struct.ScmObj* %argslist54030$k473501)
store volatile %struct.ScmObj* %argslist54030$k473502, %struct.ScmObj** %stackaddr$prim55143, align 8
%clofunc55144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47350)
musttail call tailcc void %clofunc55144(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54030$k473502)
ret void
falsebranch$cmp55140:
%ae50408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55145 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50408)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim55145, align 8
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55146, align 8
%ae50411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50411, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim55147, align 8
%argslist54031$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%argslist54031$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist54031$cc471410)
store volatile %struct.ScmObj* %argslist54031$cc471411, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%argslist54031$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54031$cc471411)
store volatile %struct.ScmObj* %argslist54031$cc471412, %struct.ScmObj** %stackaddr$prim55149, align 8
%clofunc55150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc55150(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist54031$cc471412)
ret void
}

define tailcc void @proc_clo$ae50268(%struct.ScmObj* %env$ae50268,%struct.ScmObj* %current_45args54032) {
%stackaddr$env-ref55151 = alloca %struct.ScmObj*, align 8
%k47350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 0)
store %struct.ScmObj* %k47350, %struct.ScmObj** %stackaddr$env-ref55151
%stackaddr$env-ref55152 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55152
%stackaddr$env-ref55153 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50268, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55153
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54032)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%current_45args54033 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54032)
store volatile %struct.ScmObj* %current_45args54033, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54033)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim55156, align 8
%ae50270 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50270)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim55158, align 8
%truthy$cmp55159 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47279)
%cmp$cmp55159 = icmp eq i64 %truthy$cmp55159, 1
br i1 %cmp$cmp55159, label %truebranch$cmp55159, label %falsebranch$cmp55159
truebranch$cmp55159:
%ae50274 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50275 = call %struct.ScmObj* @const_init_false()
%argslist54035$k473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%argslist54035$k473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50275, %struct.ScmObj* %argslist54035$k473500)
store volatile %struct.ScmObj* %argslist54035$k473501, %struct.ScmObj** %stackaddr$prim55160, align 8
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%argslist54035$k473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50274, %struct.ScmObj* %argslist54035$k473501)
store volatile %struct.ScmObj* %argslist54035$k473502, %struct.ScmObj** %stackaddr$prim55161, align 8
%clofunc55162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47350)
musttail call tailcc void %clofunc55162(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54035$k473502)
ret void
falsebranch$cmp55159:
%ae50283 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50283)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47281, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim55165, align 8
%truthy$cmp55166 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47282)
%cmp$cmp55166 = icmp eq i64 %truthy$cmp55166, 1
br i1 %cmp$cmp55166, label %truebranch$cmp55166, label %falsebranch$cmp55166
truebranch$cmp55166:
%ae50289 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%cpsprim47353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50289)
store volatile %struct.ScmObj* %cpsprim47353, %struct.ScmObj** %stackaddr$prim55167, align 8
%ae50291 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54036$k473500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%argslist54036$k473501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47353, %struct.ScmObj* %argslist54036$k473500)
store volatile %struct.ScmObj* %argslist54036$k473501, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%argslist54036$k473502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50291, %struct.ScmObj* %argslist54036$k473501)
store volatile %struct.ScmObj* %argslist54036$k473502, %struct.ScmObj** %stackaddr$prim55169, align 8
%clofunc55170 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47350)
musttail call tailcc void %clofunc55170(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54036$k473502)
ret void
falsebranch$cmp55166:
%ae50302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50302)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55172, align 8
%ae50305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50305, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim55173, align 8
%argslist54037$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%argslist54037$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist54037$cc471410)
store volatile %struct.ScmObj* %argslist54037$cc471411, %struct.ScmObj** %stackaddr$prim55174, align 8
%stackaddr$prim55175 = alloca %struct.ScmObj*, align 8
%argslist54037$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47350, %struct.ScmObj* %argslist54037$cc471411)
store volatile %struct.ScmObj* %argslist54037$cc471412, %struct.ScmObj** %stackaddr$prim55175, align 8
%clofunc55176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc55176(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist54037$cc471412)
ret void
}

define tailcc void @proc_clo$ae50253(%struct.ScmObj* %env$ae50253,%struct.ScmObj* %current_45args54039) {
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54039)
store volatile %struct.ScmObj* %k47354, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%current_45args54040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54039)
store volatile %struct.ScmObj* %current_45args54040, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54040)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim55179, align 8
%argslist54042$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%argslist54042$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist54042$u471420)
store volatile %struct.ScmObj* %argslist54042$u471421, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%argslist54042$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47354, %struct.ScmObj* %argslist54042$u471421)
store volatile %struct.ScmObj* %argslist54042$u471422, %struct.ScmObj** %stackaddr$prim55181, align 8
%clofunc55182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc55182(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist54042$u471422)
ret void
}

define tailcc void @proc_clo$ae49712(%struct.ScmObj* %env$ae49712,%struct.ScmObj* %current_45args54045) {
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54045)
store volatile %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$prim55183, align 8
%stackaddr$prim55184 = alloca %struct.ScmObj*, align 8
%current_45args54046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54045)
store volatile %struct.ScmObj* %current_45args54046, %struct.ScmObj** %stackaddr$prim55184, align 8
%stackaddr$prim55185 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54046)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim55185, align 8
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%current_45args54047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54046)
store volatile %struct.ScmObj* %current_45args54047, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54047)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim55187, align 8
%ae49713 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49713, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim55188, align 8
%ae49715 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49715, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$makeclosure55190 = alloca %struct.ScmObj*, align 8
%fptrToInt55191 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49717 to i64
%ae49717 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55191)
store volatile %struct.ScmObj* %ae49717, %struct.ScmObj** %stackaddr$makeclosure55190, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49717, %struct.ScmObj* %lst47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49717, %struct.ScmObj* %k47355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49717, %struct.ScmObj* %n47148, i64 2)
%ae49718 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55192 = alloca %struct.ScmObj*, align 8
%fptrToInt55193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49719 to i64
%ae49719 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55193)
store volatile %struct.ScmObj* %ae49719, %struct.ScmObj** %stackaddr$makeclosure55192, align 8
%argslist54067$ae497170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist54067$ae497171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49719, %struct.ScmObj* %argslist54067$ae497170)
store volatile %struct.ScmObj* %argslist54067$ae497171, %struct.ScmObj** %stackaddr$prim55194, align 8
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%argslist54067$ae497172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49718, %struct.ScmObj* %argslist54067$ae497171)
store volatile %struct.ScmObj* %argslist54067$ae497172, %struct.ScmObj** %stackaddr$prim55195, align 8
%clofunc55196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49717)
musttail call tailcc void %clofunc55196(%struct.ScmObj* %ae49717, %struct.ScmObj* %argslist54067$ae497172)
ret void
}

define tailcc void @proc_clo$ae49717(%struct.ScmObj* %env$ae49717,%struct.ScmObj* %current_45args54049) {
%stackaddr$env-ref55197 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49717, i64 0)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55197
%stackaddr$env-ref55198 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49717, i64 1)
store %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$env-ref55198
%stackaddr$env-ref55199 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49717, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55199
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54049)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%current_45args54050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54049)
store volatile %struct.ScmObj* %current_45args54050, %struct.ScmObj** %stackaddr$prim55201, align 8
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54050)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$makeclosure55203 = alloca %struct.ScmObj*, align 8
%fptrToInt55204 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49733 to i64
%ae49733 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55204)
store volatile %struct.ScmObj* %ae49733, %struct.ScmObj** %stackaddr$makeclosure55203, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49733, %struct.ScmObj* %lst47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49733, %struct.ScmObj* %k47355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49733, %struct.ScmObj* %n47148, i64 2)
%stackaddr$makeclosure55205 = alloca %struct.ScmObj*, align 8
%fptrToInt55206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49734 to i64
%ae49734 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55206)
store volatile %struct.ScmObj* %ae49734, %struct.ScmObj** %stackaddr$makeclosure55205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %lst47147, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %k47355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49734, %struct.ScmObj* %n47148, i64 2)
%argslist54062$anf_45bind472700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%argslist54062$anf_45bind472701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49734, %struct.ScmObj* %argslist54062$anf_45bind472700)
store volatile %struct.ScmObj* %argslist54062$anf_45bind472701, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%argslist54062$anf_45bind472702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49733, %struct.ScmObj* %argslist54062$anf_45bind472701)
store volatile %struct.ScmObj* %argslist54062$anf_45bind472702, %struct.ScmObj** %stackaddr$prim55208, align 8
%clofunc55209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47270)
musttail call tailcc void %clofunc55209(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist54062$anf_45bind472702)
ret void
}

define tailcc void @proc_clo$ae49733(%struct.ScmObj* %env$ae49733,%struct.ScmObj* %current_45args54052) {
%stackaddr$env-ref55210 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49733, i64 0)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55210
%stackaddr$env-ref55211 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49733, i64 1)
store %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$env-ref55211
%stackaddr$env-ref55212 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49733, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55212
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54052)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$prim55214 = alloca %struct.ScmObj*, align 8
%current_45args54053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54052)
store volatile %struct.ScmObj* %current_45args54053, %struct.ScmObj** %stackaddr$prim55214, align 8
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54053)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim55215, align 8
%ae49876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49876)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim55216, align 8
%ae49877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49877, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim55217, align 8
%truthy$cmp55218 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp55218 = icmp eq i64 %truthy$cmp55218, 1
br i1 %cmp$cmp55218, label %truebranch$cmp55218, label %falsebranch$cmp55218
truebranch$cmp55218:
%ae49881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%cpsprim47358 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49881)
store volatile %struct.ScmObj* %cpsprim47358, %struct.ScmObj** %stackaddr$prim55219, align 8
%ae49883 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54055$k473550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55220 = alloca %struct.ScmObj*, align 8
%argslist54055$k473551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47358, %struct.ScmObj* %argslist54055$k473550)
store volatile %struct.ScmObj* %argslist54055$k473551, %struct.ScmObj** %stackaddr$prim55220, align 8
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%argslist54055$k473552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49883, %struct.ScmObj* %argslist54055$k473551)
store volatile %struct.ScmObj* %argslist54055$k473552, %struct.ScmObj** %stackaddr$prim55221, align 8
%clofunc55222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47355)
musttail call tailcc void %clofunc55222(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist54055$k473552)
ret void
falsebranch$cmp55218:
%ae49894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49894)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim55223, align 8
%stackaddr$prim55224 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim55224, align 8
%ae49897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55225 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49897, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim55225, align 8
%ae49900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55226 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49900)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim55226, align 8
%ae49902 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55227 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %ae49902)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim55227, align 8
%ae49904 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49904, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim55228, align 8
%argslist54056$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%argslist54056$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54056$cc471490)
store volatile %struct.ScmObj* %argslist54056$cc471491, %struct.ScmObj** %stackaddr$prim55229, align 8
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%argslist54056$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist54056$cc471491)
store volatile %struct.ScmObj* %argslist54056$cc471492, %struct.ScmObj** %stackaddr$prim55230, align 8
%clofunc55231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc55231(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54056$cc471492)
ret void
}

define tailcc void @proc_clo$ae49734(%struct.ScmObj* %env$ae49734,%struct.ScmObj* %current_45args54057) {
%stackaddr$env-ref55232 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 0)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55232
%stackaddr$env-ref55233 = alloca %struct.ScmObj*, align 8
%k47355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 1)
store %struct.ScmObj* %k47355, %struct.ScmObj** %stackaddr$env-ref55233
%stackaddr$env-ref55234 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49734, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55234
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54057)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim55235, align 8
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%current_45args54058 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54057)
store volatile %struct.ScmObj* %current_45args54058, %struct.ScmObj** %stackaddr$prim55236, align 8
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54058)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim55237, align 8
%ae49736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49736)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim55238, align 8
%ae49737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49737, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim55239, align 8
%truthy$cmp55240 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47272)
%cmp$cmp55240 = icmp eq i64 %truthy$cmp55240, 1
br i1 %cmp$cmp55240, label %truebranch$cmp55240, label %falsebranch$cmp55240
truebranch$cmp55240:
%ae49741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%cpsprim47358 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49741)
store volatile %struct.ScmObj* %cpsprim47358, %struct.ScmObj** %stackaddr$prim55241, align 8
%ae49743 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54060$k473550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55242 = alloca %struct.ScmObj*, align 8
%argslist54060$k473551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47358, %struct.ScmObj* %argslist54060$k473550)
store volatile %struct.ScmObj* %argslist54060$k473551, %struct.ScmObj** %stackaddr$prim55242, align 8
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%argslist54060$k473552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49743, %struct.ScmObj* %argslist54060$k473551)
store volatile %struct.ScmObj* %argslist54060$k473552, %struct.ScmObj** %stackaddr$prim55243, align 8
%clofunc55244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47355)
musttail call tailcc void %clofunc55244(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist54060$k473552)
ret void
falsebranch$cmp55240:
%ae49754 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49754)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim55245, align 8
%stackaddr$prim55246 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim55246, align 8
%ae49757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49757, %struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim55247, align 8
%ae49760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49760)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim55248, align 8
%ae49762 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %ae49762)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim55249, align 8
%ae49764 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49764, %struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim55250, align 8
%argslist54061$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%argslist54061$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54061$cc471490)
store volatile %struct.ScmObj* %argslist54061$cc471491, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%argslist54061$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47355, %struct.ScmObj* %argslist54061$cc471491)
store volatile %struct.ScmObj* %argslist54061$cc471492, %struct.ScmObj** %stackaddr$prim55252, align 8
%clofunc55253 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc55253(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54061$cc471492)
ret void
}

define tailcc void @proc_clo$ae49719(%struct.ScmObj* %env$ae49719,%struct.ScmObj* %current_45args54063) {
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54063)
store volatile %struct.ScmObj* %k47359, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%current_45args54064 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54063)
store volatile %struct.ScmObj* %current_45args54064, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54064)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim55256, align 8
%argslist54066$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55257 = alloca %struct.ScmObj*, align 8
%argslist54066$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist54066$u471500)
store volatile %struct.ScmObj* %argslist54066$u471501, %struct.ScmObj** %stackaddr$prim55257, align 8
%stackaddr$prim55258 = alloca %struct.ScmObj*, align 8
%argslist54066$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47359, %struct.ScmObj* %argslist54066$u471501)
store volatile %struct.ScmObj* %argslist54066$u471502, %struct.ScmObj** %stackaddr$prim55258, align 8
%clofunc55259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc55259(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist54066$u471502)
ret void
}

define tailcc void @proc_clo$ae49296(%struct.ScmObj* %env$ae49296,%struct.ScmObj* %current_45args54069) {
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54069)
store volatile %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%current_45args54070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54069)
store volatile %struct.ScmObj* %current_45args54070, %struct.ScmObj** %stackaddr$prim55261, align 8
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54070)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim55262, align 8
%ae49297 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49297, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$makeclosure55264 = alloca %struct.ScmObj*, align 8
%fptrToInt55265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49299 to i64
%ae49299 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55265)
store volatile %struct.ScmObj* %ae49299, %struct.ScmObj** %stackaddr$makeclosure55264, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %k47360, i64 1)
%ae49300 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55266 = alloca %struct.ScmObj*, align 8
%fptrToInt55267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49301 to i64
%ae49301 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55267)
store volatile %struct.ScmObj* %ae49301, %struct.ScmObj** %stackaddr$makeclosure55266, align 8
%argslist54092$ae492990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55268 = alloca %struct.ScmObj*, align 8
%argslist54092$ae492991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %argslist54092$ae492990)
store volatile %struct.ScmObj* %argslist54092$ae492991, %struct.ScmObj** %stackaddr$prim55268, align 8
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%argslist54092$ae492992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist54092$ae492991)
store volatile %struct.ScmObj* %argslist54092$ae492992, %struct.ScmObj** %stackaddr$prim55269, align 8
%clofunc55270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49299)
musttail call tailcc void %clofunc55270(%struct.ScmObj* %ae49299, %struct.ScmObj* %argslist54092$ae492992)
ret void
}

define tailcc void @proc_clo$ae49299(%struct.ScmObj* %env$ae49299,%struct.ScmObj* %current_45args54072) {
%stackaddr$env-ref55271 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55271
%stackaddr$env-ref55272 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 1)
store %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$env-ref55272
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54072)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim55273, align 8
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%current_45args54073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54072)
store volatile %struct.ScmObj* %current_45args54073, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54073)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim55275, align 8
%stackaddr$makeclosure55276 = alloca %struct.ScmObj*, align 8
%fptrToInt55277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49318 to i64
%ae49318 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55277)
store volatile %struct.ScmObj* %ae49318, %struct.ScmObj** %stackaddr$makeclosure55276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49318, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49318, %struct.ScmObj* %k47360, i64 1)
%stackaddr$makeclosure55278 = alloca %struct.ScmObj*, align 8
%fptrToInt55279 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49319 to i64
%ae49319 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55279)
store volatile %struct.ScmObj* %ae49319, %struct.ScmObj** %stackaddr$makeclosure55278, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49319, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49319, %struct.ScmObj* %k47360, i64 1)
%argslist54087$anf_45bind472620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%argslist54087$anf_45bind472621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49319, %struct.ScmObj* %argslist54087$anf_45bind472620)
store volatile %struct.ScmObj* %argslist54087$anf_45bind472621, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist54087$anf_45bind472622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49318, %struct.ScmObj* %argslist54087$anf_45bind472621)
store volatile %struct.ScmObj* %argslist54087$anf_45bind472622, %struct.ScmObj** %stackaddr$prim55281, align 8
%clofunc55282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47262)
musttail call tailcc void %clofunc55282(%struct.ScmObj* %anf_45bind47262, %struct.ScmObj* %argslist54087$anf_45bind472622)
ret void
}

define tailcc void @proc_clo$ae49318(%struct.ScmObj* %env$ae49318,%struct.ScmObj* %current_45args54075) {
%stackaddr$env-ref55283 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49318, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55283
%stackaddr$env-ref55284 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49318, i64 1)
store %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$env-ref55284
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54075)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%current_45args54076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54075)
store volatile %struct.ScmObj* %current_45args54076, %struct.ScmObj** %stackaddr$prim55286, align 8
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54076)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim55287, align 8
%ae49434 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49434)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim55288, align 8
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim55289, align 8
%truthy$cmp55290 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47264)
%cmp$cmp55290 = icmp eq i64 %truthy$cmp55290, 1
br i1 %cmp$cmp55290, label %truebranch$cmp55290, label %falsebranch$cmp55290
truebranch$cmp55290:
%ae49438 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49439 = call %struct.ScmObj* @const_init_true()
%argslist54078$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%argslist54078$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49439, %struct.ScmObj* %argslist54078$k473600)
store volatile %struct.ScmObj* %argslist54078$k473601, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%argslist54078$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49438, %struct.ScmObj* %argslist54078$k473601)
store volatile %struct.ScmObj* %argslist54078$k473602, %struct.ScmObj** %stackaddr$prim55292, align 8
%clofunc55293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc55293(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54078$k473602)
ret void
falsebranch$cmp55290:
%ae49447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49447)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim55294, align 8
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim55295, align 8
%truthy$cmp55296 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47266)
%cmp$cmp55296 = icmp eq i64 %truthy$cmp55296, 1
br i1 %cmp$cmp55296, label %truebranch$cmp55296, label %falsebranch$cmp55296
truebranch$cmp55296:
%ae49451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55297 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49451)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim55297, align 8
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim55298, align 8
%ae49454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49454)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim55300, align 8
%ae49457 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55301 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49457, %struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim55301, align 8
%argslist54079$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55302 = alloca %struct.ScmObj*, align 8
%argslist54079$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54079$cc471560)
store volatile %struct.ScmObj* %argslist54079$cc471561, %struct.ScmObj** %stackaddr$prim55302, align 8
%stackaddr$prim55303 = alloca %struct.ScmObj*, align 8
%argslist54079$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54079$cc471561)
store volatile %struct.ScmObj* %argslist54079$cc471562, %struct.ScmObj** %stackaddr$prim55303, align 8
%clofunc55304 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc55304(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54079$cc471562)
ret void
falsebranch$cmp55296:
%ae49490 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49491 = call %struct.ScmObj* @const_init_false()
%argslist54080$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%argslist54080$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49491, %struct.ScmObj* %argslist54080$k473600)
store volatile %struct.ScmObj* %argslist54080$k473601, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%argslist54080$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49490, %struct.ScmObj* %argslist54080$k473601)
store volatile %struct.ScmObj* %argslist54080$k473602, %struct.ScmObj** %stackaddr$prim55306, align 8
%clofunc55307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc55307(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54080$k473602)
ret void
}

define tailcc void @proc_clo$ae49319(%struct.ScmObj* %env$ae49319,%struct.ScmObj* %current_45args54081) {
%stackaddr$env-ref55308 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49319, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55308
%stackaddr$env-ref55309 = alloca %struct.ScmObj*, align 8
%k47360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49319, i64 1)
store %struct.ScmObj* %k47360, %struct.ScmObj** %stackaddr$env-ref55309
%stackaddr$prim55310 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54081)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim55310, align 8
%stackaddr$prim55311 = alloca %struct.ScmObj*, align 8
%current_45args54082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54081)
store volatile %struct.ScmObj* %current_45args54082, %struct.ScmObj** %stackaddr$prim55311, align 8
%stackaddr$prim55312 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54082)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim55312, align 8
%ae49321 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49321)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim55313, align 8
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim55314, align 8
%truthy$cmp55315 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47264)
%cmp$cmp55315 = icmp eq i64 %truthy$cmp55315, 1
br i1 %cmp$cmp55315, label %truebranch$cmp55315, label %falsebranch$cmp55315
truebranch$cmp55315:
%ae49325 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49326 = call %struct.ScmObj* @const_init_true()
%argslist54084$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%argslist54084$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49326, %struct.ScmObj* %argslist54084$k473600)
store volatile %struct.ScmObj* %argslist54084$k473601, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%argslist54084$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49325, %struct.ScmObj* %argslist54084$k473601)
store volatile %struct.ScmObj* %argslist54084$k473602, %struct.ScmObj** %stackaddr$prim55317, align 8
%clofunc55318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc55318(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54084$k473602)
ret void
falsebranch$cmp55315:
%ae49334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49334)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim55320, align 8
%truthy$cmp55321 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47266)
%cmp$cmp55321 = icmp eq i64 %truthy$cmp55321, 1
br i1 %cmp$cmp55321, label %truebranch$cmp55321, label %falsebranch$cmp55321
truebranch$cmp55321:
%ae49338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49338)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim55323, align 8
%ae49341 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49341)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim55324, align 8
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47268)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim55325, align 8
%ae49344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49344, %struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim55326, align 8
%argslist54085$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%argslist54085$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54085$cc471560)
store volatile %struct.ScmObj* %argslist54085$cc471561, %struct.ScmObj** %stackaddr$prim55327, align 8
%stackaddr$prim55328 = alloca %struct.ScmObj*, align 8
%argslist54085$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54085$cc471561)
store volatile %struct.ScmObj* %argslist54085$cc471562, %struct.ScmObj** %stackaddr$prim55328, align 8
%clofunc55329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc55329(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54085$cc471562)
ret void
falsebranch$cmp55321:
%ae49377 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49378 = call %struct.ScmObj* @const_init_false()
%argslist54086$k473600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist54086$k473601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49378, %struct.ScmObj* %argslist54086$k473600)
store volatile %struct.ScmObj* %argslist54086$k473601, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%argslist54086$k473602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49377, %struct.ScmObj* %argslist54086$k473601)
store volatile %struct.ScmObj* %argslist54086$k473602, %struct.ScmObj** %stackaddr$prim55331, align 8
%clofunc55332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47360)
musttail call tailcc void %clofunc55332(%struct.ScmObj* %k47360, %struct.ScmObj* %argslist54086$k473602)
ret void
}

define tailcc void @proc_clo$ae49301(%struct.ScmObj* %env$ae49301,%struct.ScmObj* %current_45args54088) {
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %k47363, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%current_45args54089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %current_45args54089, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54089)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim55335, align 8
%ae49303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54091$k473630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%argslist54091$k473631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist54091$k473630)
store volatile %struct.ScmObj* %argslist54091$k473631, %struct.ScmObj** %stackaddr$prim55336, align 8
%stackaddr$prim55337 = alloca %struct.ScmObj*, align 8
%argslist54091$k473632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49303, %struct.ScmObj* %argslist54091$k473631)
store volatile %struct.ScmObj* %argslist54091$k473632, %struct.ScmObj** %stackaddr$prim55337, align 8
%clofunc55338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47363)
musttail call tailcc void %clofunc55338(%struct.ScmObj* %k47363, %struct.ScmObj* %argslist54091$k473632)
ret void
}

define tailcc void @proc_clo$ae49224(%struct.ScmObj* %env$ae49224,%struct.ScmObj* %current_45args54094) {
%stackaddr$env-ref55339 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49224, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref55339
%stackaddr$prim55340 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$prim55340, align 8
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%current_45args54095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %current_45args54095, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54095)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%current_45args54096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54095)
store volatile %struct.ScmObj* %current_45args54096, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54096)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim55345, align 8
%truthy$cmp55346 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47256)
%cmp$cmp55346 = icmp eq i64 %truthy$cmp55346, 1
br i1 %cmp$cmp55346, label %truebranch$cmp55346, label %falsebranch$cmp55346
truebranch$cmp55346:
%ae49228 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54098$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%argslist54098$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist54098$k473640)
store volatile %struct.ScmObj* %argslist54098$k473641, %struct.ScmObj** %stackaddr$prim55347, align 8
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist54098$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist54098$k473641)
store volatile %struct.ScmObj* %argslist54098$k473642, %struct.ScmObj** %stackaddr$prim55348, align 8
%clofunc55349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc55349(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist54098$k473642)
ret void
falsebranch$cmp55346:
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim55350, align 8
%ae49235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49235)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$makeclosure55353 = alloca %struct.ScmObj*, align 8
%fptrToInt55354 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49238 to i64
%ae49238 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55354)
store volatile %struct.ScmObj* %ae49238, %struct.ScmObj** %stackaddr$makeclosure55353, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49238, %struct.ScmObj* %k47364, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49238, %struct.ScmObj* %anf_45bind47257, i64 1)
%argslist54103$anf_45bind472580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%argslist54103$anf_45bind472581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist54103$anf_45bind472580)
store volatile %struct.ScmObj* %argslist54103$anf_45bind472581, %struct.ScmObj** %stackaddr$prim55355, align 8
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%argslist54103$anf_45bind472582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist54103$anf_45bind472581)
store volatile %struct.ScmObj* %argslist54103$anf_45bind472582, %struct.ScmObj** %stackaddr$prim55356, align 8
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%argslist54103$anf_45bind472583 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist54103$anf_45bind472582)
store volatile %struct.ScmObj* %argslist54103$anf_45bind472583, %struct.ScmObj** %stackaddr$prim55357, align 8
%clofunc55358 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47258)
musttail call tailcc void %clofunc55358(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %argslist54103$anf_45bind472583)
ret void
}

define tailcc void @proc_clo$ae49238(%struct.ScmObj* %env$ae49238,%struct.ScmObj* %current_45args54099) {
%stackaddr$env-ref55359 = alloca %struct.ScmObj*, align 8
%k47364 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49238, i64 0)
store %struct.ScmObj* %k47364, %struct.ScmObj** %stackaddr$env-ref55359
%stackaddr$env-ref55360 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49238, i64 1)
store %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$env-ref55360
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim55361, align 8
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%current_45args54100 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %current_45args54100, %struct.ScmObj** %stackaddr$prim55362, align 8
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54100)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%cpsprim47366 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsprim47366, %struct.ScmObj** %stackaddr$prim55364, align 8
%ae49244 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54102$k473640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%argslist54102$k473641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47366, %struct.ScmObj* %argslist54102$k473640)
store volatile %struct.ScmObj* %argslist54102$k473641, %struct.ScmObj** %stackaddr$prim55365, align 8
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%argslist54102$k473642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49244, %struct.ScmObj* %argslist54102$k473641)
store volatile %struct.ScmObj* %argslist54102$k473642, %struct.ScmObj** %stackaddr$prim55366, align 8
%clofunc55367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47364)
musttail call tailcc void %clofunc55367(%struct.ScmObj* %k47364, %struct.ScmObj* %argslist54102$k473642)
ret void
}

define tailcc void @proc_clo$ae49198(%struct.ScmObj* %env$ae49198,%struct.ScmObj* %current_45args54105) {
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %k47367, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%current_45args54106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %current_45args54106, %struct.ScmObj** %stackaddr$prim55369, align 8
%stackaddr$prim55370 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54106)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim55370, align 8
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%current_45args54107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54106)
store volatile %struct.ScmObj* %current_45args54107, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54107)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim55372, align 8
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%cpsprim47368 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47255)
store volatile %struct.ScmObj* %cpsprim47368, %struct.ScmObj** %stackaddr$prim55374, align 8
%ae49203 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54109$k473670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%argslist54109$k473671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47368, %struct.ScmObj* %argslist54109$k473670)
store volatile %struct.ScmObj* %argslist54109$k473671, %struct.ScmObj** %stackaddr$prim55375, align 8
%stackaddr$prim55376 = alloca %struct.ScmObj*, align 8
%argslist54109$k473672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49203, %struct.ScmObj* %argslist54109$k473671)
store volatile %struct.ScmObj* %argslist54109$k473672, %struct.ScmObj** %stackaddr$prim55376, align 8
%clofunc55377 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47367)
musttail call tailcc void %clofunc55377(%struct.ScmObj* %k47367, %struct.ScmObj* %argslist54109$k473672)
ret void
}

define tailcc void @proc_clo$ae49174(%struct.ScmObj* %env$ae49174,%struct.ScmObj* %current_45args54111) {
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %k47369, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%current_45args54112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %current_45args54112, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%current_45args54113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %current_45args54113, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54113)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim55382, align 8
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%cpsprim47370 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47254)
store volatile %struct.ScmObj* %cpsprim47370, %struct.ScmObj** %stackaddr$prim55384, align 8
%ae49179 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54115$k473690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%argslist54115$k473691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47370, %struct.ScmObj* %argslist54115$k473690)
store volatile %struct.ScmObj* %argslist54115$k473691, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%argslist54115$k473692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49179, %struct.ScmObj* %argslist54115$k473691)
store volatile %struct.ScmObj* %argslist54115$k473692, %struct.ScmObj** %stackaddr$prim55386, align 8
%clofunc55387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47369)
musttail call tailcc void %clofunc55387(%struct.ScmObj* %k47369, %struct.ScmObj* %argslist54115$k473692)
ret void
}

define tailcc void @proc_clo$ae48780(%struct.ScmObj* %env$ae48780,%struct.ScmObj* %current_45args54118) {
%stackaddr$env-ref55388 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48780, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55388
%stackaddr$env-ref55389 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48780, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55389
%stackaddr$env-ref55390 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48780, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55390
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54118)
store volatile %struct.ScmObj* %k47371, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%current_45args54119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54118)
store volatile %struct.ScmObj* %current_45args54119, %struct.ScmObj** %stackaddr$prim55392, align 8
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54119)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim55393, align 8
%ae48782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55394 = alloca %struct.ScmObj*, align 8
%fptrToInt55395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48783 to i64
%ae48783 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55395)
store volatile %struct.ScmObj* %ae48783, %struct.ScmObj** %stackaddr$makeclosure55394, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37map147120, i64 3)
%argslist54176$k473710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%argslist54176$k473711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist54176$k473710)
store volatile %struct.ScmObj* %argslist54176$k473711, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%argslist54176$k473712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48782, %struct.ScmObj* %argslist54176$k473711)
store volatile %struct.ScmObj* %argslist54176$k473712, %struct.ScmObj** %stackaddr$prim55397, align 8
%clofunc55398 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47371)
musttail call tailcc void %clofunc55398(%struct.ScmObj* %k47371, %struct.ScmObj* %argslist54176$k473712)
ret void
}

define tailcc void @proc_clo$ae48783(%struct.ScmObj* %env$ae48783,%struct.ScmObj* %args4717347372) {
%stackaddr$env-ref55399 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55399
%stackaddr$env-ref55400 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55400
%stackaddr$env-ref55401 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55401
%stackaddr$env-ref55402 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55402
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347372)
store volatile %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$prim55403, align 8
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347372)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim55404, align 8
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim55406, align 8
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim55407, align 8
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$makeclosure55410 = alloca %struct.ScmObj*, align 8
%fptrToInt55411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48791 to i64
%ae48791 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55411)
store volatile %struct.ScmObj* %ae48791, %struct.ScmObj** %stackaddr$makeclosure55410, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %k47373, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48791, %struct.ScmObj* %acc47175, i64 7)
%ae48792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55412 = alloca %struct.ScmObj*, align 8
%fptrToInt55413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48793 to i64
%ae48793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55413)
store volatile %struct.ScmObj* %ae48793, %struct.ScmObj** %stackaddr$makeclosure55412, align 8
%argslist54175$ae487910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%argslist54175$ae487911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48793, %struct.ScmObj* %argslist54175$ae487910)
store volatile %struct.ScmObj* %argslist54175$ae487911, %struct.ScmObj** %stackaddr$prim55414, align 8
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%argslist54175$ae487912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist54175$ae487911)
store volatile %struct.ScmObj* %argslist54175$ae487912, %struct.ScmObj** %stackaddr$prim55415, align 8
%clofunc55416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48791)
musttail call tailcc void %clofunc55416(%struct.ScmObj* %ae48791, %struct.ScmObj* %argslist54175$ae487912)
ret void
}

define tailcc void @proc_clo$ae48791(%struct.ScmObj* %env$ae48791,%struct.ScmObj* %current_45args54121) {
%stackaddr$env-ref55417 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55417
%stackaddr$env-ref55418 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55418
%stackaddr$env-ref55419 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55419
%stackaddr$env-ref55420 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55420
%stackaddr$env-ref55421 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55421
%stackaddr$env-ref55422 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 5)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55422
%stackaddr$env-ref55423 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55423
%stackaddr$env-ref55424 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48791, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55424
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%current_45args54122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %current_45args54122, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54122)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim55427, align 8
%stackaddr$makeclosure55428 = alloca %struct.ScmObj*, align 8
%fptrToInt55429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48823 to i64
%ae48823 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55429)
store volatile %struct.ScmObj* %ae48823, %struct.ScmObj** %stackaddr$makeclosure55428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %k47373, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48823, %struct.ScmObj* %acc47175, i64 6)
%ae48825 = call %struct.ScmObj* @const_init_false()
%argslist54168$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%argslist54168$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54168$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54168$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%argslist54168$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48825, %struct.ScmObj* %argslist54168$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54168$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55431, align 8
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%argslist54168$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist54168$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54168$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55432, align 8
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist54168$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48823, %struct.ScmObj* %argslist54168$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54168$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55433, align 8
%clofunc55434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55434(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54168$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48823(%struct.ScmObj* %env$ae48823,%struct.ScmObj* %current_45args54124) {
%stackaddr$env-ref55435 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55435
%stackaddr$env-ref55436 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55436
%stackaddr$env-ref55437 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55437
%stackaddr$env-ref55438 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55438
%stackaddr$env-ref55439 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 4)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55439
%stackaddr$env-ref55440 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55440
%stackaddr$env-ref55441 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48823, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55441
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%current_45args54125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %current_45args54125, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54125)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim55444, align 8
%truthy$cmp55445 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47245)
%cmp$cmp55445 = icmp eq i64 %truthy$cmp55445, 1
br i1 %cmp$cmp55445, label %truebranch$cmp55445, label %falsebranch$cmp55445
truebranch$cmp55445:
%ae48834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54127$k473730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%argslist54127$k473731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist54127$k473730)
store volatile %struct.ScmObj* %argslist54127$k473731, %struct.ScmObj** %stackaddr$prim55446, align 8
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%argslist54127$k473732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48834, %struct.ScmObj* %argslist54127$k473731)
store volatile %struct.ScmObj* %argslist54127$k473732, %struct.ScmObj** %stackaddr$prim55447, align 8
%clofunc55448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47373)
musttail call tailcc void %clofunc55448(%struct.ScmObj* %k47373, %struct.ScmObj* %argslist54127$k473732)
ret void
falsebranch$cmp55445:
%stackaddr$makeclosure55449 = alloca %struct.ScmObj*, align 8
%fptrToInt55450 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48839 to i64
%ae48839 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55450)
store volatile %struct.ScmObj* %ae48839, %struct.ScmObj** %stackaddr$makeclosure55449, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %k47373, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48839, %struct.ScmObj* %acc47175, i64 6)
%ae48840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55451 = alloca %struct.ScmObj*, align 8
%fptrToInt55452 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48841 to i64
%ae48841 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55452)
store volatile %struct.ScmObj* %ae48841, %struct.ScmObj** %stackaddr$makeclosure55451, align 8
%argslist54167$ae488390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist54167$ae488391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48841, %struct.ScmObj* %argslist54167$ae488390)
store volatile %struct.ScmObj* %argslist54167$ae488391, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%argslist54167$ae488392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48840, %struct.ScmObj* %argslist54167$ae488391)
store volatile %struct.ScmObj* %argslist54167$ae488392, %struct.ScmObj** %stackaddr$prim55454, align 8
%clofunc55455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48839)
musttail call tailcc void %clofunc55455(%struct.ScmObj* %ae48839, %struct.ScmObj* %argslist54167$ae488392)
ret void
}

define tailcc void @proc_clo$ae48839(%struct.ScmObj* %env$ae48839,%struct.ScmObj* %current_45args54128) {
%stackaddr$env-ref55456 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55456
%stackaddr$env-ref55457 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55457
%stackaddr$env-ref55458 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55458
%stackaddr$env-ref55459 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55459
%stackaddr$env-ref55460 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 4)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55460
%stackaddr$env-ref55461 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55461
%stackaddr$env-ref55462 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48839, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55462
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54128)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$prim55464 = alloca %struct.ScmObj*, align 8
%current_45args54129 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54128)
store volatile %struct.ScmObj* %current_45args54129, %struct.ScmObj** %stackaddr$prim55464, align 8
%stackaddr$prim55465 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54129)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim55465, align 8
%stackaddr$makeclosure55466 = alloca %struct.ScmObj*, align 8
%fptrToInt55467 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48860 to i64
%ae48860 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55467)
store volatile %struct.ScmObj* %ae48860, %struct.ScmObj** %stackaddr$makeclosure55466, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %k47373, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48860, %struct.ScmObj* %acc47175, i64 6)
%argslist54162$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%argslist54162$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54162$_37map1471200)
store volatile %struct.ScmObj* %argslist54162$_37map1471201, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%argslist54162$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist54162$_37map1471201)
store volatile %struct.ScmObj* %argslist54162$_37map1471202, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%argslist54162$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48860, %struct.ScmObj* %argslist54162$_37map1471202)
store volatile %struct.ScmObj* %argslist54162$_37map1471203, %struct.ScmObj** %stackaddr$prim55470, align 8
%clofunc55471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55471(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist54162$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48860(%struct.ScmObj* %env$ae48860,%struct.ScmObj* %current_45args54131) {
%stackaddr$env-ref55472 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55472
%stackaddr$env-ref55473 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55473
%stackaddr$env-ref55474 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55474
%stackaddr$env-ref55475 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55475
%stackaddr$env-ref55476 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 4)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55476
%stackaddr$env-ref55477 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55477
%stackaddr$env-ref55478 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48860, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55478
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54131)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%current_45args54132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54131)
store volatile %struct.ScmObj* %current_45args54132, %struct.ScmObj** %stackaddr$prim55480, align 8
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54132)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$makeclosure55482 = alloca %struct.ScmObj*, align 8
%fptrToInt55483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48863 to i64
%ae48863 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55483)
store volatile %struct.ScmObj* %ae48863, %struct.ScmObj** %stackaddr$makeclosure55482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %lsts_4347181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %k47373, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %_37map147120, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48863, %struct.ScmObj* %acc47175, i64 7)
%ae48864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55484 = alloca %struct.ScmObj*, align 8
%fptrToInt55485 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48865 to i64
%ae48865 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55485)
store volatile %struct.ScmObj* %ae48865, %struct.ScmObj** %stackaddr$makeclosure55484, align 8
%argslist54161$ae488630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55486 = alloca %struct.ScmObj*, align 8
%argslist54161$ae488631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48865, %struct.ScmObj* %argslist54161$ae488630)
store volatile %struct.ScmObj* %argslist54161$ae488631, %struct.ScmObj** %stackaddr$prim55486, align 8
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%argslist54161$ae488632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48864, %struct.ScmObj* %argslist54161$ae488631)
store volatile %struct.ScmObj* %argslist54161$ae488632, %struct.ScmObj** %stackaddr$prim55487, align 8
%clofunc55488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48863)
musttail call tailcc void %clofunc55488(%struct.ScmObj* %ae48863, %struct.ScmObj* %argslist54161$ae488632)
ret void
}

define tailcc void @proc_clo$ae48863(%struct.ScmObj* %env$ae48863,%struct.ScmObj* %current_45args54134) {
%stackaddr$env-ref55489 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55489
%stackaddr$env-ref55490 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55490
%stackaddr$env-ref55491 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 2)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55491
%stackaddr$env-ref55492 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 3)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55492
%stackaddr$env-ref55493 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55493
%stackaddr$env-ref55494 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 5)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55494
%stackaddr$env-ref55495 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55495
%stackaddr$env-ref55496 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48863, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55496
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54134)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim55497, align 8
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%current_45args54135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54134)
store volatile %struct.ScmObj* %current_45args54135, %struct.ScmObj** %stackaddr$prim55498, align 8
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54135)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$makeclosure55500 = alloca %struct.ScmObj*, align 8
%fptrToInt55501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48884 to i64
%ae48884 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55501)
store volatile %struct.ScmObj* %ae48884, %struct.ScmObj** %stackaddr$makeclosure55500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %lsts_4347181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %k47373, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %acc47175, i64 5)
%argslist54156$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%argslist54156$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54156$_37map1471200)
store volatile %struct.ScmObj* %argslist54156$_37map1471201, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%argslist54156$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist54156$_37map1471201)
store volatile %struct.ScmObj* %argslist54156$_37map1471202, %struct.ScmObj** %stackaddr$prim55503, align 8
%stackaddr$prim55504 = alloca %struct.ScmObj*, align 8
%argslist54156$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist54156$_37map1471202)
store volatile %struct.ScmObj* %argslist54156$_37map1471203, %struct.ScmObj** %stackaddr$prim55504, align 8
%clofunc55505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55505(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist54156$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48884(%struct.ScmObj* %env$ae48884,%struct.ScmObj* %current_45args54137) {
%stackaddr$env-ref55506 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 0)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55506
%stackaddr$env-ref55507 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 1)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55507
%stackaddr$env-ref55508 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55508
%stackaddr$env-ref55509 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55509
%stackaddr$env-ref55510 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55510
%stackaddr$env-ref55511 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55511
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%current_45args54138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %current_45args54138, %struct.ScmObj** %stackaddr$prim55513, align 8
%stackaddr$prim55514 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54138)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim55514, align 8
%stackaddr$makeclosure55515 = alloca %struct.ScmObj*, align 8
%fptrToInt55516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48887 to i64
%ae48887 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55516)
store volatile %struct.ScmObj* %ae48887, %struct.ScmObj** %stackaddr$makeclosure55515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %lsts_4347181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %k47373, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %acc47175, i64 6)
%ae48888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55517 = alloca %struct.ScmObj*, align 8
%fptrToInt55518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48889 to i64
%ae48889 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55518)
store volatile %struct.ScmObj* %ae48889, %struct.ScmObj** %stackaddr$makeclosure55517, align 8
%argslist54155$ae488870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%argslist54155$ae488871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist54155$ae488870)
store volatile %struct.ScmObj* %argslist54155$ae488871, %struct.ScmObj** %stackaddr$prim55519, align 8
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%argslist54155$ae488872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist54155$ae488871)
store volatile %struct.ScmObj* %argslist54155$ae488872, %struct.ScmObj** %stackaddr$prim55520, align 8
%clofunc55521 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48887)
musttail call tailcc void %clofunc55521(%struct.ScmObj* %ae48887, %struct.ScmObj* %argslist54155$ae488872)
ret void
}

define tailcc void @proc_clo$ae48887(%struct.ScmObj* %env$ae48887,%struct.ScmObj* %current_45args54140) {
%stackaddr$env-ref55522 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 0)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55522
%stackaddr$env-ref55523 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 1)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55523
%stackaddr$env-ref55524 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55524
%stackaddr$env-ref55525 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55525
%stackaddr$env-ref55526 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref55526
%stackaddr$env-ref55527 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55527
%stackaddr$env-ref55528 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55528
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%current_45args54141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %current_45args54141, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim55531, align 8
%ae48910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55532 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48910)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim55532, align 8
%stackaddr$makeclosure55533 = alloca %struct.ScmObj*, align 8
%fptrToInt55534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48912 to i64
%ae48912 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55534)
store volatile %struct.ScmObj* %ae48912, %struct.ScmObj** %stackaddr$makeclosure55533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48912, %struct.ScmObj* %lsts_4347181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48912, %struct.ScmObj* %k47373, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48912, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48912, %struct.ScmObj* %f47176, i64 3)
%argslist54149$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%argslist54149$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist54149$_37foldr470940)
store volatile %struct.ScmObj* %argslist54149$_37foldr470941, %struct.ScmObj** %stackaddr$prim55535, align 8
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%argslist54149$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist54149$_37foldr470941)
store volatile %struct.ScmObj* %argslist54149$_37foldr470942, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%argslist54149$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist54149$_37foldr470942)
store volatile %struct.ScmObj* %argslist54149$_37foldr470943, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%argslist54149$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist54149$_37foldr470943)
store volatile %struct.ScmObj* %argslist54149$_37foldr470944, %struct.ScmObj** %stackaddr$prim55538, align 8
%clofunc55539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55539(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist54149$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48912(%struct.ScmObj* %env$ae48912,%struct.ScmObj* %current_45args54143) {
%stackaddr$env-ref55540 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48912, i64 0)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55540
%stackaddr$env-ref55541 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48912, i64 1)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55541
%stackaddr$env-ref55542 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48912, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55542
%stackaddr$env-ref55543 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48912, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55543
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54143)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim55544, align 8
%stackaddr$prim55545 = alloca %struct.ScmObj*, align 8
%current_45args54144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54143)
store volatile %struct.ScmObj* %current_45args54144, %struct.ScmObj** %stackaddr$prim55545, align 8
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54144)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim55546, align 8
%stackaddr$makeclosure55547 = alloca %struct.ScmObj*, align 8
%fptrToInt55548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48916 to i64
%ae48916 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55548)
store volatile %struct.ScmObj* %ae48916, %struct.ScmObj** %stackaddr$makeclosure55547, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %lsts_4347181, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %k47373, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48916, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%cpsargs47384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48916, %struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %cpsargs47384, %struct.ScmObj** %stackaddr$prim55549, align 8
%clofunc55550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc55550(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47384)
ret void
}

define tailcc void @proc_clo$ae48916(%struct.ScmObj* %env$ae48916,%struct.ScmObj* %current_45args54146) {
%stackaddr$env-ref55551 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 0)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55551
%stackaddr$env-ref55552 = alloca %struct.ScmObj*, align 8
%k47373 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 1)
store %struct.ScmObj* %k47373, %struct.ScmObj** %stackaddr$env-ref55552
%stackaddr$env-ref55553 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55553
%stackaddr$env-ref55554 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48916, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55554
%stackaddr$prim55555 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim55555, align 8
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%current_45args54147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %current_45args54147, %struct.ScmObj** %stackaddr$prim55556, align 8
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54147)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim55557, align 8
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim55558, align 8
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47251)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%cpsargs47383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47373, %struct.ScmObj* %anf_45bind47252)
store volatile %struct.ScmObj* %cpsargs47383, %struct.ScmObj** %stackaddr$prim55560, align 8
%clofunc55561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc55561(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47383)
ret void
}

define tailcc void @proc_clo$ae48889(%struct.ScmObj* %env$ae48889,%struct.ScmObj* %current_45args54150) {
%stackaddr$prim55562 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$prim55562, align 8
%stackaddr$prim55563 = alloca %struct.ScmObj*, align 8
%current_45args54151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %current_45args54151, %struct.ScmObj** %stackaddr$prim55563, align 8
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%current_45args54152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %current_45args54152, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54152)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%cpsprim47386 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47386, %struct.ScmObj** %stackaddr$prim55567, align 8
%ae48893 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54154$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%argslist54154$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47386, %struct.ScmObj* %argslist54154$k473850)
store volatile %struct.ScmObj* %argslist54154$k473851, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%argslist54154$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48893, %struct.ScmObj* %argslist54154$k473851)
store volatile %struct.ScmObj* %argslist54154$k473852, %struct.ScmObj** %stackaddr$prim55569, align 8
%clofunc55570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc55570(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist54154$k473852)
ret void
}

define tailcc void @proc_clo$ae48865(%struct.ScmObj* %env$ae48865,%struct.ScmObj* %current_45args54157) {
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %k47387, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%current_45args54158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %current_45args54158, %struct.ScmObj** %stackaddr$prim55572, align 8
%stackaddr$prim55573 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54158)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim55573, align 8
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%cpsprim47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47388, %struct.ScmObj** %stackaddr$prim55574, align 8
%ae48868 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54160$k473870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%argslist54160$k473871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47388, %struct.ScmObj* %argslist54160$k473870)
store volatile %struct.ScmObj* %argslist54160$k473871, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%argslist54160$k473872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48868, %struct.ScmObj* %argslist54160$k473871)
store volatile %struct.ScmObj* %argslist54160$k473872, %struct.ScmObj** %stackaddr$prim55576, align 8
%clofunc55577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47387)
musttail call tailcc void %clofunc55577(%struct.ScmObj* %k47387, %struct.ScmObj* %argslist54160$k473872)
ret void
}

define tailcc void @proc_clo$ae48841(%struct.ScmObj* %env$ae48841,%struct.ScmObj* %current_45args54163) {
%stackaddr$prim55578 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim55578, align 8
%stackaddr$prim55579 = alloca %struct.ScmObj*, align 8
%current_45args54164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %current_45args54164, %struct.ScmObj** %stackaddr$prim55579, align 8
%stackaddr$prim55580 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim55580, align 8
%stackaddr$prim55581 = alloca %struct.ScmObj*, align 8
%cpsprim47390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47390, %struct.ScmObj** %stackaddr$prim55581, align 8
%ae48844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54166$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%argslist54166$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47390, %struct.ScmObj* %argslist54166$k473890)
store volatile %struct.ScmObj* %argslist54166$k473891, %struct.ScmObj** %stackaddr$prim55582, align 8
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%argslist54166$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist54166$k473891)
store volatile %struct.ScmObj* %argslist54166$k473892, %struct.ScmObj** %stackaddr$prim55583, align 8
%clofunc55584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc55584(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist54166$k473892)
ret void
}

define tailcc void @proc_clo$ae48793(%struct.ScmObj* %env$ae48793,%struct.ScmObj* %current_45args54169) {
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%current_45args54171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %current_45args54171, %struct.ScmObj** %stackaddr$prim55588, align 8
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54171)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim55589, align 8
%truthy$cmp55590 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp55590 = icmp eq i64 %truthy$cmp55590, 1
br i1 %cmp$cmp55590, label %truebranch$cmp55590, label %falsebranch$cmp55590
truebranch$cmp55590:
%ae48796 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54173$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55591 = alloca %struct.ScmObj*, align 8
%argslist54173$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist54173$k473910)
store volatile %struct.ScmObj* %argslist54173$k473911, %struct.ScmObj** %stackaddr$prim55591, align 8
%stackaddr$prim55592 = alloca %struct.ScmObj*, align 8
%argslist54173$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist54173$k473911)
store volatile %struct.ScmObj* %argslist54173$k473912, %struct.ScmObj** %stackaddr$prim55592, align 8
%clofunc55593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc55593(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist54173$k473912)
ret void
falsebranch$cmp55590:
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%cpsprim47392 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47392, %struct.ScmObj** %stackaddr$prim55594, align 8
%ae48803 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54174$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55595 = alloca %struct.ScmObj*, align 8
%argslist54174$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47392, %struct.ScmObj* %argslist54174$k473910)
store volatile %struct.ScmObj* %argslist54174$k473911, %struct.ScmObj** %stackaddr$prim55595, align 8
%stackaddr$prim55596 = alloca %struct.ScmObj*, align 8
%argslist54174$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48803, %struct.ScmObj* %argslist54174$k473911)
store volatile %struct.ScmObj* %argslist54174$k473912, %struct.ScmObj** %stackaddr$prim55596, align 8
%clofunc55597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc55597(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist54174$k473912)
ret void
}

define tailcc void @proc_clo$ae48634(%struct.ScmObj* %env$ae48634,%struct.ScmObj* %args4711647393) {
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$env-ref55599 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55599
%stackaddr$env-ref55600 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55600
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647393)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647393)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim55602, align 8
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$makeclosure55605 = alloca %struct.ScmObj*, align 8
%fptrToInt55606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48639 to i64
%ae48639 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55606)
store volatile %struct.ScmObj* %ae48639, %struct.ScmObj** %stackaddr$makeclosure55605, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48639, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48639, %struct.ScmObj* %k47394, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48639, %struct.ScmObj* %lsts47117, i64 2)
%ae48640 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55607 = alloca %struct.ScmObj*, align 8
%fptrToInt55608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48641 to i64
%ae48641 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55608)
store volatile %struct.ScmObj* %ae48641, %struct.ScmObj** %stackaddr$makeclosure55607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48641, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48641, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48641, %struct.ScmObj* %_37last47111, i64 2)
%argslist54193$ae486390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%argslist54193$ae486391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48641, %struct.ScmObj* %argslist54193$ae486390)
store volatile %struct.ScmObj* %argslist54193$ae486391, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%argslist54193$ae486392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48640, %struct.ScmObj* %argslist54193$ae486391)
store volatile %struct.ScmObj* %argslist54193$ae486392, %struct.ScmObj** %stackaddr$prim55610, align 8
%clofunc55611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48639)
musttail call tailcc void %clofunc55611(%struct.ScmObj* %ae48639, %struct.ScmObj* %argslist54193$ae486392)
ret void
}

define tailcc void @proc_clo$ae48639(%struct.ScmObj* %env$ae48639,%struct.ScmObj* %current_45args54178) {
%stackaddr$env-ref55612 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48639, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55612
%stackaddr$env-ref55613 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48639, i64 1)
store %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$env-ref55613
%stackaddr$env-ref55614 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48639, i64 2)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref55614
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%_95k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %_95k47395, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%current_45args54179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %current_45args54179, %struct.ScmObj** %stackaddr$prim55616, align 8
%stackaddr$prim55617 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim55617, align 8
%ae48702 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55618 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim55618, align 8
%stackaddr$prim55619 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %anf_45bind47240)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim55619, align 8
%stackaddr$prim55620 = alloca %struct.ScmObj*, align 8
%cpsargs47396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47394, %struct.ScmObj* %anf_45bind47241)
store volatile %struct.ScmObj* %cpsargs47396, %struct.ScmObj** %stackaddr$prim55620, align 8
%clofunc55621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55621(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47396)
ret void
}

define tailcc void @proc_clo$ae48641(%struct.ScmObj* %env$ae48641,%struct.ScmObj* %fargs4711947397) {
%stackaddr$env-ref55622 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48641, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55622
%stackaddr$env-ref55623 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48641, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55623
%stackaddr$env-ref55624 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48641, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55624
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947397)
store volatile %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$prim55625, align 8
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947397)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$makeclosure55627 = alloca %struct.ScmObj*, align 8
%fptrToInt55628 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48645 to i64
%ae48645 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55628)
store volatile %struct.ScmObj* %ae48645, %struct.ScmObj** %stackaddr$makeclosure55627, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %k47398, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %f47118, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48645, %struct.ScmObj* %_37last47111, i64 3)
%ae48647 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54192$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55629 = alloca %struct.ScmObj*, align 8
%argslist54192$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48647, %struct.ScmObj* %argslist54192$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist54192$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim55629, align 8
%stackaddr$prim55630 = alloca %struct.ScmObj*, align 8
%argslist54192$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist54192$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist54192$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim55630, align 8
%stackaddr$prim55631 = alloca %struct.ScmObj*, align 8
%argslist54192$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48645, %struct.ScmObj* %argslist54192$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist54192$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim55631, align 8
%clofunc55632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc55632(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist54192$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48645(%struct.ScmObj* %env$ae48645,%struct.ScmObj* %current_45args54181) {
%stackaddr$env-ref55633 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 0)
store %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$env-ref55633
%stackaddr$env-ref55634 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55634
%stackaddr$env-ref55635 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 2)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55635
%stackaddr$env-ref55636 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48645, i64 3)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55636
%stackaddr$prim55637 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim55637, align 8
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%current_45args54182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54181)
store volatile %struct.ScmObj* %current_45args54182, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55639, align 8
%stackaddr$makeclosure55640 = alloca %struct.ScmObj*, align 8
%fptrToInt55641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48652 to i64
%ae48652 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55641)
store volatile %struct.ScmObj* %ae48652, %struct.ScmObj** %stackaddr$makeclosure55640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %k47398, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48652, %struct.ScmObj* %_37last47111, i64 2)
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%cpsargs47403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48652, %struct.ScmObj* %anf_45bind47236)
store volatile %struct.ScmObj* %cpsargs47403, %struct.ScmObj** %stackaddr$prim55642, align 8
%clofunc55643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc55643(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47403)
ret void
}

define tailcc void @proc_clo$ae48652(%struct.ScmObj* %env$ae48652,%struct.ScmObj* %current_45args54184) {
%stackaddr$env-ref55644 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 0)
store %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$env-ref55644
%stackaddr$env-ref55645 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55645
%stackaddr$env-ref55646 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48652, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55646
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%current_45args54185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54184)
store volatile %struct.ScmObj* %current_45args54185, %struct.ScmObj** %stackaddr$prim55648, align 8
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$makeclosure55650 = alloca %struct.ScmObj*, align 8
%fptrToInt55651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48657 to i64
%ae48657 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55651)
store volatile %struct.ScmObj* %ae48657, %struct.ScmObj** %stackaddr$makeclosure55650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48657, %struct.ScmObj* %k47398, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48657, %struct.ScmObj* %anf_45bind47237, i64 1)
%argslist54191$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%argslist54191$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist54191$_37last471110)
store volatile %struct.ScmObj* %argslist54191$_37last471111, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%argslist54191$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48657, %struct.ScmObj* %argslist54191$_37last471111)
store volatile %struct.ScmObj* %argslist54191$_37last471112, %struct.ScmObj** %stackaddr$prim55653, align 8
%clofunc55654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc55654(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist54191$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48657(%struct.ScmObj* %env$ae48657,%struct.ScmObj* %current_45args54187) {
%stackaddr$env-ref55655 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48657, i64 0)
store %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$env-ref55655
%stackaddr$env-ref55656 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48657, i64 1)
store %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$env-ref55656
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim55657, align 8
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%current_45args54188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54187)
store volatile %struct.ScmObj* %current_45args54188, %struct.ScmObj** %stackaddr$prim55658, align 8
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%cpsprim47402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %anf_45bind47238)
store volatile %struct.ScmObj* %cpsprim47402, %struct.ScmObj** %stackaddr$prim55660, align 8
%ae48662 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54190$k473980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%argslist54190$k473981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47402, %struct.ScmObj* %argslist54190$k473980)
store volatile %struct.ScmObj* %argslist54190$k473981, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%argslist54190$k473982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist54190$k473981)
store volatile %struct.ScmObj* %argslist54190$k473982, %struct.ScmObj** %stackaddr$prim55662, align 8
%clofunc55663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47398)
musttail call tailcc void %clofunc55663(%struct.ScmObj* %k47398, %struct.ScmObj* %argslist54190$k473982)
ret void
}

define tailcc void @proc_clo$ae48557(%struct.ScmObj* %env$ae48557,%struct.ScmObj* %current_45args54195) {
%stackaddr$env-ref55664 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55664
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%current_45args54196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %current_45args54196, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%current_45args54197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %current_45args54197, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54197)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$makeclosure55670 = alloca %struct.ScmObj*, align 8
%fptrToInt55671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48558 to i64
%ae48558 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55671)
store volatile %struct.ScmObj* %ae48558, %struct.ScmObj** %stackaddr$makeclosure55670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48558, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48558, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48558, %struct.ScmObj* %k47404, i64 2)
%ae48559 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55672 = alloca %struct.ScmObj*, align 8
%fptrToInt55673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48560 to i64
%ae48560 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55673)
store volatile %struct.ScmObj* %ae48560, %struct.ScmObj** %stackaddr$makeclosure55672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48560, %struct.ScmObj* %f47122, i64 0)
%argslist54212$ae485580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist54212$ae485581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48560, %struct.ScmObj* %argslist54212$ae485580)
store volatile %struct.ScmObj* %argslist54212$ae485581, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%argslist54212$ae485582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48559, %struct.ScmObj* %argslist54212$ae485581)
store volatile %struct.ScmObj* %argslist54212$ae485582, %struct.ScmObj** %stackaddr$prim55675, align 8
%clofunc55676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48558)
musttail call tailcc void %clofunc55676(%struct.ScmObj* %ae48558, %struct.ScmObj* %argslist54212$ae485582)
ret void
}

define tailcc void @proc_clo$ae48558(%struct.ScmObj* %env$ae48558,%struct.ScmObj* %current_45args54199) {
%stackaddr$env-ref55677 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48558, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref55677
%stackaddr$env-ref55678 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48558, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55678
%stackaddr$env-ref55679 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48558, i64 2)
store %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$env-ref55679
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%current_45args54200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %current_45args54200, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54200)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim55682, align 8
%ae48592 = call %struct.ScmObj* @const_init_null()
%argslist54202$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%argslist54202$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist54202$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54202$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55683, align 8
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%argslist54202$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48592, %struct.ScmObj* %argslist54202$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54202$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%argslist54202$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist54202$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54202$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%argslist54202$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist54202$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54202$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55686, align 8
%clofunc55687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55687(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54202$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48560(%struct.ScmObj* %env$ae48560,%struct.ScmObj* %current_45args54203) {
%stackaddr$env-ref55688 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48560, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref55688
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %k47406, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%current_45args54204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %current_45args54204, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54204)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim55691, align 8
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%current_45args54205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54204)
store volatile %struct.ScmObj* %current_45args54205, %struct.ScmObj** %stackaddr$prim55692, align 8
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$makeclosure55694 = alloca %struct.ScmObj*, align 8
%fptrToInt55695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48562 to i64
%ae48562 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55695)
store volatile %struct.ScmObj* %ae48562, %struct.ScmObj** %stackaddr$makeclosure55694, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %r47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %k47406, i64 1)
%argslist54211$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%argslist54211$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist54211$f471220)
store volatile %struct.ScmObj* %argslist54211$f471221, %struct.ScmObj** %stackaddr$prim55696, align 8
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist54211$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48562, %struct.ScmObj* %argslist54211$f471221)
store volatile %struct.ScmObj* %argslist54211$f471222, %struct.ScmObj** %stackaddr$prim55697, align 8
%clofunc55698 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc55698(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist54211$f471222)
ret void
}

define tailcc void @proc_clo$ae48562(%struct.ScmObj* %env$ae48562,%struct.ScmObj* %current_45args54207) {
%stackaddr$env-ref55699 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 0)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref55699
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%k47406 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 1)
store %struct.ScmObj* %k47406, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%current_45args54208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %current_45args54208, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim55704, align 8
%ae48567 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54210$k474060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%argslist54210$k474061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist54210$k474060)
store volatile %struct.ScmObj* %argslist54210$k474061, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%argslist54210$k474062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48567, %struct.ScmObj* %argslist54210$k474061)
store volatile %struct.ScmObj* %argslist54210$k474062, %struct.ScmObj** %stackaddr$prim55706, align 8
%clofunc55707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47406)
musttail call tailcc void %clofunc55707(%struct.ScmObj* %k47406, %struct.ScmObj* %argslist54210$k474062)
ret void
}

define tailcc void @proc_clo$ae48171(%struct.ScmObj* %env$ae48171,%struct.ScmObj* %current_45args54215) {
%stackaddr$env-ref55708 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55708
%stackaddr$env-ref55709 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48171, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55709
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%current_45args54216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54215)
store volatile %struct.ScmObj* %current_45args54216, %struct.ScmObj** %stackaddr$prim55711, align 8
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54216)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim55712, align 8
%ae48173 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55713 = alloca %struct.ScmObj*, align 8
%fptrToInt55714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48174 to i64
%ae48174 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55714)
store volatile %struct.ScmObj* %ae48174, %struct.ScmObj** %stackaddr$makeclosure55713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48174, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist54273$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%argslist54273$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48174, %struct.ScmObj* %argslist54273$k474090)
store volatile %struct.ScmObj* %argslist54273$k474091, %struct.ScmObj** %stackaddr$prim55715, align 8
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%argslist54273$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48173, %struct.ScmObj* %argslist54273$k474091)
store volatile %struct.ScmObj* %argslist54273$k474092, %struct.ScmObj** %stackaddr$prim55716, align 8
%clofunc55717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc55717(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist54273$k474092)
ret void
}

define tailcc void @proc_clo$ae48174(%struct.ScmObj* %env$ae48174,%struct.ScmObj* %args4709647410) {
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48174, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647410)
store volatile %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647410)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim55723, align 8
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47221)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim55726, align 8
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47222)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$makeclosure55728 = alloca %struct.ScmObj*, align 8
%fptrToInt55729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48182 to i64
%ae48182 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55729)
store volatile %struct.ScmObj* %ae48182, %struct.ScmObj** %stackaddr$makeclosure55728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48182, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48183 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55730 = alloca %struct.ScmObj*, align 8
%fptrToInt55731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48184 to i64
%ae48184 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55731)
store volatile %struct.ScmObj* %ae48184, %struct.ScmObj** %stackaddr$makeclosure55730, align 8
%argslist54272$ae481820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%argslist54272$ae481821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48184, %struct.ScmObj* %argslist54272$ae481820)
store volatile %struct.ScmObj* %argslist54272$ae481821, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%argslist54272$ae481822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48183, %struct.ScmObj* %argslist54272$ae481821)
store volatile %struct.ScmObj* %argslist54272$ae481822, %struct.ScmObj** %stackaddr$prim55733, align 8
%clofunc55734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48182)
musttail call tailcc void %clofunc55734(%struct.ScmObj* %ae48182, %struct.ScmObj* %argslist54272$ae481822)
ret void
}

define tailcc void @proc_clo$ae48182(%struct.ScmObj* %env$ae48182,%struct.ScmObj* %current_45args54218) {
%stackaddr$env-ref55735 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55735
%stackaddr$env-ref55736 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55736
%stackaddr$env-ref55737 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55737
%stackaddr$env-ref55738 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55738
%stackaddr$env-ref55739 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55739
%stackaddr$env-ref55740 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55740
%stackaddr$env-ref55741 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48182, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55741
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%current_45args54219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %current_45args54219, %struct.ScmObj** %stackaddr$prim55743, align 8
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54219)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim55744, align 8
%stackaddr$makeclosure55745 = alloca %struct.ScmObj*, align 8
%fptrToInt55746 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48214 to i64
%ae48214 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55746)
store volatile %struct.ScmObj* %ae48214, %struct.ScmObj** %stackaddr$makeclosure55745, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48214, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48216 = call %struct.ScmObj* @const_init_false()
%argslist54265$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%argslist54265$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54265$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54265$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%argslist54265$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48216, %struct.ScmObj* %argslist54265$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54265$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55748, align 8
%stackaddr$prim55749 = alloca %struct.ScmObj*, align 8
%argslist54265$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist54265$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54265$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55749, align 8
%stackaddr$prim55750 = alloca %struct.ScmObj*, align 8
%argslist54265$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48214, %struct.ScmObj* %argslist54265$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54265$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55750, align 8
%clofunc55751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55751(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54265$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48214(%struct.ScmObj* %env$ae48214,%struct.ScmObj* %current_45args54221) {
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$env-ref55753 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55753
%stackaddr$env-ref55754 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55754
%stackaddr$env-ref55755 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55755
%stackaddr$env-ref55756 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55756
%stackaddr$env-ref55757 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55757
%stackaddr$env-ref55758 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48214, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55758
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim55759, align 8
%stackaddr$prim55760 = alloca %struct.ScmObj*, align 8
%current_45args54222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %current_45args54222, %struct.ScmObj** %stackaddr$prim55760, align 8
%stackaddr$prim55761 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54222)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim55761, align 8
%truthy$cmp55762 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47224)
%cmp$cmp55762 = icmp eq i64 %truthy$cmp55762, 1
br i1 %cmp$cmp55762, label %truebranch$cmp55762, label %falsebranch$cmp55762
truebranch$cmp55762:
%ae48225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54224$k474110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%argslist54224$k474111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist54224$k474110)
store volatile %struct.ScmObj* %argslist54224$k474111, %struct.ScmObj** %stackaddr$prim55763, align 8
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%argslist54224$k474112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48225, %struct.ScmObj* %argslist54224$k474111)
store volatile %struct.ScmObj* %argslist54224$k474112, %struct.ScmObj** %stackaddr$prim55764, align 8
%clofunc55765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47411)
musttail call tailcc void %clofunc55765(%struct.ScmObj* %k47411, %struct.ScmObj* %argslist54224$k474112)
ret void
falsebranch$cmp55762:
%stackaddr$makeclosure55766 = alloca %struct.ScmObj*, align 8
%fptrToInt55767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48230 to i64
%ae48230 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55767)
store volatile %struct.ScmObj* %ae48230, %struct.ScmObj** %stackaddr$makeclosure55766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48230, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48231 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55768 = alloca %struct.ScmObj*, align 8
%fptrToInt55769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48232 to i64
%ae48232 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55769)
store volatile %struct.ScmObj* %ae48232, %struct.ScmObj** %stackaddr$makeclosure55768, align 8
%argslist54264$ae482300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%argslist54264$ae482301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48232, %struct.ScmObj* %argslist54264$ae482300)
store volatile %struct.ScmObj* %argslist54264$ae482301, %struct.ScmObj** %stackaddr$prim55770, align 8
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%argslist54264$ae482302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48231, %struct.ScmObj* %argslist54264$ae482301)
store volatile %struct.ScmObj* %argslist54264$ae482302, %struct.ScmObj** %stackaddr$prim55771, align 8
%clofunc55772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48230)
musttail call tailcc void %clofunc55772(%struct.ScmObj* %ae48230, %struct.ScmObj* %argslist54264$ae482302)
ret void
}

define tailcc void @proc_clo$ae48230(%struct.ScmObj* %env$ae48230,%struct.ScmObj* %current_45args54225) {
%stackaddr$env-ref55773 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55773
%stackaddr$env-ref55774 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55774
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$env-ref55777 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55777
%stackaddr$env-ref55778 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55778
%stackaddr$env-ref55779 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48230, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55779
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%_95k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %_95k47414, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%current_45args54226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %current_45args54226, %struct.ScmObj** %stackaddr$prim55781, align 8
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54226)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$makeclosure55783 = alloca %struct.ScmObj*, align 8
%fptrToInt55784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48251 to i64
%ae48251 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55784)
store volatile %struct.ScmObj* %ae48251, %struct.ScmObj** %stackaddr$makeclosure55783, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48251, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist54259$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%argslist54259$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54259$_37map1470850)
store volatile %struct.ScmObj* %argslist54259$_37map1470851, %struct.ScmObj** %stackaddr$prim55785, align 8
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%argslist54259$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist54259$_37map1470851)
store volatile %struct.ScmObj* %argslist54259$_37map1470852, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%argslist54259$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48251, %struct.ScmObj* %argslist54259$_37map1470852)
store volatile %struct.ScmObj* %argslist54259$_37map1470853, %struct.ScmObj** %stackaddr$prim55787, align 8
%clofunc55788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55788(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist54259$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48251(%struct.ScmObj* %env$ae48251,%struct.ScmObj* %current_45args54228) {
%stackaddr$env-ref55789 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55789
%stackaddr$env-ref55790 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55790
%stackaddr$env-ref55791 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55791
%stackaddr$env-ref55792 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55792
%stackaddr$env-ref55793 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55793
%stackaddr$env-ref55794 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55794
%stackaddr$env-ref55795 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48251, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55795
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54228)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim55796, align 8
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%current_45args54229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54228)
store volatile %struct.ScmObj* %current_45args54229, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54229)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$makeclosure55799 = alloca %struct.ScmObj*, align 8
%fptrToInt55800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48254 to i64
%ae48254 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55800)
store volatile %struct.ScmObj* %ae48254, %struct.ScmObj** %stackaddr$makeclosure55799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %lsts47097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48254, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48255 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55801 = alloca %struct.ScmObj*, align 8
%fptrToInt55802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48256 to i64
%ae48256 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55802)
store volatile %struct.ScmObj* %ae48256, %struct.ScmObj** %stackaddr$makeclosure55801, align 8
%argslist54258$ae482540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55803 = alloca %struct.ScmObj*, align 8
%argslist54258$ae482541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48256, %struct.ScmObj* %argslist54258$ae482540)
store volatile %struct.ScmObj* %argslist54258$ae482541, %struct.ScmObj** %stackaddr$prim55803, align 8
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%argslist54258$ae482542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48255, %struct.ScmObj* %argslist54258$ae482541)
store volatile %struct.ScmObj* %argslist54258$ae482542, %struct.ScmObj** %stackaddr$prim55804, align 8
%clofunc55805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48254)
musttail call tailcc void %clofunc55805(%struct.ScmObj* %ae48254, %struct.ScmObj* %argslist54258$ae482542)
ret void
}

define tailcc void @proc_clo$ae48254(%struct.ScmObj* %env$ae48254,%struct.ScmObj* %current_45args54231) {
%stackaddr$env-ref55806 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55806
%stackaddr$env-ref55807 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55807
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$env-ref55810 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55810
%stackaddr$env-ref55811 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55811
%stackaddr$env-ref55812 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 6)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55812
%stackaddr$env-ref55813 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48254, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55813
%stackaddr$prim55814 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim55814, align 8
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%current_45args54232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %current_45args54232, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54232)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim55816, align 8
%stackaddr$makeclosure55817 = alloca %struct.ScmObj*, align 8
%fptrToInt55818 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48275 to i64
%ae48275 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55818)
store volatile %struct.ScmObj* %ae48275, %struct.ScmObj** %stackaddr$makeclosure55817, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48275, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist54253$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55819 = alloca %struct.ScmObj*, align 8
%argslist54253$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54253$_37map1470850)
store volatile %struct.ScmObj* %argslist54253$_37map1470851, %struct.ScmObj** %stackaddr$prim55819, align 8
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%argslist54253$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist54253$_37map1470851)
store volatile %struct.ScmObj* %argslist54253$_37map1470852, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%argslist54253$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48275, %struct.ScmObj* %argslist54253$_37map1470852)
store volatile %struct.ScmObj* %argslist54253$_37map1470853, %struct.ScmObj** %stackaddr$prim55821, align 8
%clofunc55822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55822(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist54253$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48275(%struct.ScmObj* %env$ae48275,%struct.ScmObj* %current_45args54234) {
%stackaddr$env-ref55823 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55823
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$env-ref55825 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55825
%stackaddr$env-ref55826 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55826
%stackaddr$env-ref55827 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55827
%stackaddr$env-ref55828 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48275, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55828
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54234)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$prim55830 = alloca %struct.ScmObj*, align 8
%current_45args54235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54234)
store volatile %struct.ScmObj* %current_45args54235, %struct.ScmObj** %stackaddr$prim55830, align 8
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$makeclosure55832 = alloca %struct.ScmObj*, align 8
%fptrToInt55833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48278 to i64
%ae48278 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55833)
store volatile %struct.ScmObj* %ae48278, %struct.ScmObj** %stackaddr$makeclosure55832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55834 = alloca %struct.ScmObj*, align 8
%fptrToInt55835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48280 to i64
%ae48280 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55835)
store volatile %struct.ScmObj* %ae48280, %struct.ScmObj** %stackaddr$makeclosure55834, align 8
%argslist54252$ae482780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%argslist54252$ae482781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48280, %struct.ScmObj* %argslist54252$ae482780)
store volatile %struct.ScmObj* %argslist54252$ae482781, %struct.ScmObj** %stackaddr$prim55836, align 8
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%argslist54252$ae482782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist54252$ae482781)
store volatile %struct.ScmObj* %argslist54252$ae482782, %struct.ScmObj** %stackaddr$prim55837, align 8
%clofunc55838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48278)
musttail call tailcc void %clofunc55838(%struct.ScmObj* %ae48278, %struct.ScmObj* %argslist54252$ae482782)
ret void
}

define tailcc void @proc_clo$ae48278(%struct.ScmObj* %env$ae48278,%struct.ScmObj* %current_45args54237) {
%stackaddr$env-ref55839 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55839
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$env-ref55843 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55843
%stackaddr$env-ref55844 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55844
%stackaddr$env-ref55845 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55845
%stackaddr$prim55846 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim55846, align 8
%stackaddr$prim55847 = alloca %struct.ScmObj*, align 8
%current_45args54238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %current_45args54238, %struct.ScmObj** %stackaddr$prim55847, align 8
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim55849, align 8
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47228)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$makeclosure55851 = alloca %struct.ScmObj*, align 8
%fptrToInt55852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48304 to i64
%ae48304 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55852)
store volatile %struct.ScmObj* %ae48304, %struct.ScmObj** %stackaddr$makeclosure55851, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48304, %struct.ScmObj* %anf_45bind47227, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48304, %struct.ScmObj* %f47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48304, %struct.ScmObj* %k47411, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48304, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48304, %struct.ScmObj* %vs47102, i64 4)
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%cpsargs47422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48304, %struct.ScmObj* %anf_45bind47229)
store volatile %struct.ScmObj* %cpsargs47422, %struct.ScmObj** %stackaddr$prim55853, align 8
%clofunc55854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55854(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47422)
ret void
}

define tailcc void @proc_clo$ae48304(%struct.ScmObj* %env$ae48304,%struct.ScmObj* %current_45args54240) {
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48304, i64 0)
store %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$env-ref55856 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48304, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55856
%stackaddr$env-ref55857 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48304, i64 2)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55857
%stackaddr$env-ref55858 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48304, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55858
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48304, i64 4)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$prim55860 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim55860, align 8
%stackaddr$prim55861 = alloca %struct.ScmObj*, align 8
%current_45args54241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54240)
store volatile %struct.ScmObj* %current_45args54241, %struct.ScmObj** %stackaddr$prim55861, align 8
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54241)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim55862, align 8
%ae48309 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %ae48309)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$makeclosure55864 = alloca %struct.ScmObj*, align 8
%fptrToInt55865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48311 to i64
%ae48311 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55865)
store volatile %struct.ScmObj* %ae48311, %struct.ScmObj** %stackaddr$makeclosure55864, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48311, %struct.ScmObj* %k47411, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48311, %struct.ScmObj* %f47099, i64 1)
%argslist54246$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%argslist54246$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist54246$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54246$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55866, align 8
%stackaddr$prim55867 = alloca %struct.ScmObj*, align 8
%argslist54246$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist54246$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54246$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55867, align 8
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%argslist54246$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist54246$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54246$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%argslist54246$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48311, %struct.ScmObj* %argslist54246$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54246$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55869, align 8
%clofunc55870 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55870(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54246$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48311(%struct.ScmObj* %env$ae48311,%struct.ScmObj* %current_45args54243) {
%stackaddr$env-ref55871 = alloca %struct.ScmObj*, align 8
%k47411 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48311, i64 0)
store %struct.ScmObj* %k47411, %struct.ScmObj** %stackaddr$env-ref55871
%stackaddr$env-ref55872 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48311, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55872
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%current_45args54244 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %current_45args54244, %struct.ScmObj** %stackaddr$prim55874, align 8
%stackaddr$prim55875 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54244)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55875, align 8
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%cpsargs47421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47411, %struct.ScmObj* %anf_45bind47232)
store volatile %struct.ScmObj* %cpsargs47421, %struct.ScmObj** %stackaddr$prim55876, align 8
%clofunc55877 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55877(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47421)
ret void
}

define tailcc void @proc_clo$ae48280(%struct.ScmObj* %env$ae48280,%struct.ScmObj* %current_45args54247) {
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%current_45args54248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %current_45args54248, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%current_45args54249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %current_45args54249, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55882, align 8
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%cpsprim47424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47424, %struct.ScmObj** %stackaddr$prim55883, align 8
%ae48284 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54251$k474230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist54251$k474231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47424, %struct.ScmObj* %argslist54251$k474230)
store volatile %struct.ScmObj* %argslist54251$k474231, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist54251$k474232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48284, %struct.ScmObj* %argslist54251$k474231)
store volatile %struct.ScmObj* %argslist54251$k474232, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47423)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist54251$k474232)
ret void
}

define tailcc void @proc_clo$ae48256(%struct.ScmObj* %env$ae48256,%struct.ScmObj* %current_45args54254) {
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %k47425, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%current_45args54255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54254)
store volatile %struct.ScmObj* %current_45args54255, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55889, align 8
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%cpsprim47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47426, %struct.ScmObj** %stackaddr$prim55890, align 8
%ae48259 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54257$k474250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%argslist54257$k474251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47426, %struct.ScmObj* %argslist54257$k474250)
store volatile %struct.ScmObj* %argslist54257$k474251, %struct.ScmObj** %stackaddr$prim55891, align 8
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%argslist54257$k474252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48259, %struct.ScmObj* %argslist54257$k474251)
store volatile %struct.ScmObj* %argslist54257$k474252, %struct.ScmObj** %stackaddr$prim55892, align 8
%clofunc55893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47425)
musttail call tailcc void %clofunc55893(%struct.ScmObj* %k47425, %struct.ScmObj* %argslist54257$k474252)
ret void
}

define tailcc void @proc_clo$ae48232(%struct.ScmObj* %env$ae48232,%struct.ScmObj* %current_45args54260) {
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%current_45args54261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %current_45args54261, %struct.ScmObj** %stackaddr$prim55895, align 8
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54261)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim55896, align 8
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%cpsprim47428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47428, %struct.ScmObj** %stackaddr$prim55897, align 8
%ae48235 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54263$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55898 = alloca %struct.ScmObj*, align 8
%argslist54263$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47428, %struct.ScmObj* %argslist54263$k474270)
store volatile %struct.ScmObj* %argslist54263$k474271, %struct.ScmObj** %stackaddr$prim55898, align 8
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%argslist54263$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48235, %struct.ScmObj* %argslist54263$k474271)
store volatile %struct.ScmObj* %argslist54263$k474272, %struct.ScmObj** %stackaddr$prim55899, align 8
%clofunc55900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc55900(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist54263$k474272)
ret void
}

define tailcc void @proc_clo$ae48184(%struct.ScmObj* %env$ae48184,%struct.ScmObj* %current_45args54266) {
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %k47429, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%current_45args54267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %current_45args54267, %struct.ScmObj** %stackaddr$prim55902, align 8
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%current_45args54268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %current_45args54268, %struct.ScmObj** %stackaddr$prim55904, align 8
%stackaddr$prim55905 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54268)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim55905, align 8
%truthy$cmp55906 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp55906 = icmp eq i64 %truthy$cmp55906, 1
br i1 %cmp$cmp55906, label %truebranch$cmp55906, label %falsebranch$cmp55906
truebranch$cmp55906:
%ae48187 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54270$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55907 = alloca %struct.ScmObj*, align 8
%argslist54270$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist54270$k474290)
store volatile %struct.ScmObj* %argslist54270$k474291, %struct.ScmObj** %stackaddr$prim55907, align 8
%stackaddr$prim55908 = alloca %struct.ScmObj*, align 8
%argslist54270$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48187, %struct.ScmObj* %argslist54270$k474291)
store volatile %struct.ScmObj* %argslist54270$k474292, %struct.ScmObj** %stackaddr$prim55908, align 8
%clofunc55909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc55909(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist54270$k474292)
ret void
falsebranch$cmp55906:
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim55910, align 8
%ae48194 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54271$k474290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%argslist54271$k474291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist54271$k474290)
store volatile %struct.ScmObj* %argslist54271$k474291, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%argslist54271$k474292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48194, %struct.ScmObj* %argslist54271$k474291)
store volatile %struct.ScmObj* %argslist54271$k474292, %struct.ScmObj** %stackaddr$prim55912, align 8
%clofunc55913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47429)
musttail call tailcc void %clofunc55913(%struct.ScmObj* %k47429, %struct.ScmObj* %argslist54271$k474292)
ret void
}

define tailcc void @proc_clo$ae48141(%struct.ScmObj* %env$ae48141,%struct.ScmObj* %current_45args54275) {
%stackaddr$env-ref55914 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48141, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref55914
%stackaddr$env-ref55915 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48141, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55915
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%current_45args54276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %current_45args54276, %struct.ScmObj** %stackaddr$prim55917, align 8
%stackaddr$prim55918 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54276)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim55918, align 8
%stackaddr$prim55919 = alloca %struct.ScmObj*, align 8
%current_45args54277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54276)
store volatile %struct.ScmObj* %current_45args54277, %struct.ScmObj** %stackaddr$prim55919, align 8
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$makeclosure55921 = alloca %struct.ScmObj*, align 8
%fptrToInt55922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48143 to i64
%ae48143 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55922)
store volatile %struct.ScmObj* %ae48143, %struct.ScmObj** %stackaddr$makeclosure55921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %lst47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %n47109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37take47081, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %k47431, i64 3)
%argslist54283$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55923 = alloca %struct.ScmObj*, align 8
%argslist54283$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist54283$_37length470780)
store volatile %struct.ScmObj* %argslist54283$_37length470781, %struct.ScmObj** %stackaddr$prim55923, align 8
%stackaddr$prim55924 = alloca %struct.ScmObj*, align 8
%argslist54283$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48143, %struct.ScmObj* %argslist54283$_37length470781)
store volatile %struct.ScmObj* %argslist54283$_37length470782, %struct.ScmObj** %stackaddr$prim55924, align 8
%clofunc55925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc55925(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist54283$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48143(%struct.ScmObj* %env$ae48143,%struct.ScmObj* %current_45args54279) {
%stackaddr$env-ref55926 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 0)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref55926
%stackaddr$env-ref55927 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 1)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref55927
%stackaddr$env-ref55928 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 2)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref55928
%stackaddr$env-ref55929 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 3)
store %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$env-ref55929
%stackaddr$prim55930 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim55930, align 8
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%current_45args54280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %current_45args54280, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim55933, align 8
%argslist54282$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%argslist54282$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist54282$_37take470810)
store volatile %struct.ScmObj* %argslist54282$_37take470811, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%argslist54282$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist54282$_37take470811)
store volatile %struct.ScmObj* %argslist54282$_37take470812, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$prim55936 = alloca %struct.ScmObj*, align 8
%argslist54282$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist54282$_37take470812)
store volatile %struct.ScmObj* %argslist54282$_37take470813, %struct.ScmObj** %stackaddr$prim55936, align 8
%clofunc55937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc55937(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist54282$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48087(%struct.ScmObj* %env$ae48087,%struct.ScmObj* %current_45args54285) {
%stackaddr$env-ref55938 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48087, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55938
%stackaddr$prim55939 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54285)
store volatile %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$prim55939, align 8
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%current_45args54286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54285)
store volatile %struct.ScmObj* %current_45args54286, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54286)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim55941, align 8
%stackaddr$makeclosure55942 = alloca %struct.ScmObj*, align 8
%fptrToInt55943 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48088 to i64
%ae48088 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55943)
store volatile %struct.ScmObj* %ae48088, %struct.ScmObj** %stackaddr$makeclosure55942, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48088, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48088, %struct.ScmObj* %k47433, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48088, %struct.ScmObj* %lst47112, i64 2)
%ae48089 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55944 = alloca %struct.ScmObj*, align 8
%fptrToInt55945 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48090 to i64
%ae48090 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55945)
store volatile %struct.ScmObj* %ae48090, %struct.ScmObj** %stackaddr$makeclosure55944, align 8
%argslist54297$ae480880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%argslist54297$ae480881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48090, %struct.ScmObj* %argslist54297$ae480880)
store volatile %struct.ScmObj* %argslist54297$ae480881, %struct.ScmObj** %stackaddr$prim55946, align 8
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%argslist54297$ae480882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48089, %struct.ScmObj* %argslist54297$ae480881)
store volatile %struct.ScmObj* %argslist54297$ae480882, %struct.ScmObj** %stackaddr$prim55947, align 8
%clofunc55948 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48088)
musttail call tailcc void %clofunc55948(%struct.ScmObj* %ae48088, %struct.ScmObj* %argslist54297$ae480882)
ret void
}

define tailcc void @proc_clo$ae48088(%struct.ScmObj* %env$ae48088,%struct.ScmObj* %current_45args54288) {
%stackaddr$env-ref55949 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48088, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55949
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48088, i64 1)
store %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48088, i64 2)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%current_45args54289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %current_45args54289, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54289)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim55954, align 8
%ae48109 = call %struct.ScmObj* @const_init_null()
%argslist54291$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%argslist54291$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist54291$_37foldl1470730)
store volatile %struct.ScmObj* %argslist54291$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55955, align 8
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%argslist54291$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48109, %struct.ScmObj* %argslist54291$_37foldl1470731)
store volatile %struct.ScmObj* %argslist54291$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%argslist54291$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist54291$_37foldl1470732)
store volatile %struct.ScmObj* %argslist54291$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%argslist54291$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist54291$_37foldl1470733)
store volatile %struct.ScmObj* %argslist54291$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55958, align 8
%clofunc55959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55959(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist54291$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48090(%struct.ScmObj* %env$ae48090,%struct.ScmObj* %current_45args54292) {
%stackaddr$prim55960 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim55960, align 8
%stackaddr$prim55961 = alloca %struct.ScmObj*, align 8
%current_45args54293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %current_45args54293, %struct.ScmObj** %stackaddr$prim55961, align 8
%stackaddr$prim55962 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim55962, align 8
%stackaddr$prim55963 = alloca %struct.ScmObj*, align 8
%current_45args54294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54293)
store volatile %struct.ScmObj* %current_45args54294, %struct.ScmObj** %stackaddr$prim55963, align 8
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim55964, align 8
%ae48092 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54296$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%argslist54296$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist54296$k474350)
store volatile %struct.ScmObj* %argslist54296$k474351, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%argslist54296$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48092, %struct.ScmObj* %argslist54296$k474351)
store volatile %struct.ScmObj* %argslist54296$k474352, %struct.ScmObj** %stackaddr$prim55966, align 8
%clofunc55967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc55967(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54296$k474352)
ret void
}

define tailcc void @proc_clo$ae48008(%struct.ScmObj* %env$ae48008,%struct.ScmObj* %current_45args54300) {
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %k47436, %struct.ScmObj** %stackaddr$prim55968, align 8
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%current_45args54301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %current_45args54301, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54301)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim55970, align 8
%ae48010 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55971 = alloca %struct.ScmObj*, align 8
%fptrToInt55972 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48011 to i64
%ae48011 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55972)
store volatile %struct.ScmObj* %ae48011, %struct.ScmObj** %stackaddr$makeclosure55971, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48011, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist54314$k474360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55973 = alloca %struct.ScmObj*, align 8
%argslist54314$k474361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48011, %struct.ScmObj* %argslist54314$k474360)
store volatile %struct.ScmObj* %argslist54314$k474361, %struct.ScmObj** %stackaddr$prim55973, align 8
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%argslist54314$k474362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48010, %struct.ScmObj* %argslist54314$k474361)
store volatile %struct.ScmObj* %argslist54314$k474362, %struct.ScmObj** %stackaddr$prim55974, align 8
%clofunc55975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47436)
musttail call tailcc void %clofunc55975(%struct.ScmObj* %k47436, %struct.ScmObj* %argslist54314$k474362)
ret void
}

define tailcc void @proc_clo$ae48011(%struct.ScmObj* %env$ae48011,%struct.ScmObj* %current_45args54303) {
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48011, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%current_45args54304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %current_45args54304, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54304)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%current_45args54305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54304)
store volatile %struct.ScmObj* %current_45args54305, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%current_45args54306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %current_45args54306, %struct.ScmObj** %stackaddr$prim55982, align 8
%stackaddr$prim55983 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim55983, align 8
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim55984, align 8
%truthy$cmp55985 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47213)
%cmp$cmp55985 = icmp eq i64 %truthy$cmp55985, 1
br i1 %cmp$cmp55985, label %truebranch$cmp55985, label %falsebranch$cmp55985
truebranch$cmp55985:
%ae48015 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54308$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%argslist54308$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist54308$k474370)
store volatile %struct.ScmObj* %argslist54308$k474371, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%argslist54308$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48015, %struct.ScmObj* %argslist54308$k474371)
store volatile %struct.ScmObj* %argslist54308$k474372, %struct.ScmObj** %stackaddr$prim55987, align 8
%clofunc55988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc55988(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist54308$k474372)
ret void
falsebranch$cmp55985:
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$makeclosure55990 = alloca %struct.ScmObj*, align 8
%fptrToInt55991 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48022 to i64
%ae48022 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55991)
store volatile %struct.ScmObj* %ae48022, %struct.ScmObj** %stackaddr$makeclosure55990, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48022, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48022, %struct.ScmObj* %lst47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48022, %struct.ScmObj* %_37foldl147074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48022, %struct.ScmObj* %k47437, i64 3)
%argslist54313$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%argslist54313$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist54313$f470770)
store volatile %struct.ScmObj* %argslist54313$f470771, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%argslist54313$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist54313$f470771)
store volatile %struct.ScmObj* %argslist54313$f470772, %struct.ScmObj** %stackaddr$prim55993, align 8
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%argslist54313$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48022, %struct.ScmObj* %argslist54313$f470772)
store volatile %struct.ScmObj* %argslist54313$f470773, %struct.ScmObj** %stackaddr$prim55994, align 8
%clofunc55995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc55995(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist54313$f470773)
ret void
}

define tailcc void @proc_clo$ae48022(%struct.ScmObj* %env$ae48022,%struct.ScmObj* %current_45args54309) {
%stackaddr$env-ref55996 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48022, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref55996
%stackaddr$env-ref55997 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48022, i64 1)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref55997
%stackaddr$env-ref55998 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48022, i64 2)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref55998
%stackaddr$env-ref55999 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48022, i64 3)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55999
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%current_45args54310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54309)
store volatile %struct.ScmObj* %current_45args54310, %struct.ScmObj** %stackaddr$prim56001, align 8
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54310)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim56003, align 8
%argslist54312$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54312$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist54312$_37foldl1470740)
store volatile %struct.ScmObj* %argslist54312$_37foldl1470741, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54312$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist54312$_37foldl1470741)
store volatile %struct.ScmObj* %argslist54312$_37foldl1470742, %struct.ScmObj** %stackaddr$prim56005, align 8
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%argslist54312$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist54312$_37foldl1470742)
store volatile %struct.ScmObj* %argslist54312$_37foldl1470743, %struct.ScmObj** %stackaddr$prim56006, align 8
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%argslist54312$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist54312$_37foldl1470743)
store volatile %struct.ScmObj* %argslist54312$_37foldl1470744, %struct.ScmObj** %stackaddr$prim56007, align 8
%clofunc56008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc56008(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist54312$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47925(%struct.ScmObj* %env$ae47925,%struct.ScmObj* %current_45args54317) {
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54317)
store volatile %struct.ScmObj* %k47439, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%current_45args54318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54317)
store volatile %struct.ScmObj* %current_45args54318, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54318)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim56011, align 8
%ae47927 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56012 = alloca %struct.ScmObj*, align 8
%fptrToInt56013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47928 to i64
%ae47928 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56013)
store volatile %struct.ScmObj* %ae47928, %struct.ScmObj** %stackaddr$makeclosure56012, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47928, %struct.ScmObj* %_37length47079, i64 0)
%argslist54329$k474390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%argslist54329$k474391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47928, %struct.ScmObj* %argslist54329$k474390)
store volatile %struct.ScmObj* %argslist54329$k474391, %struct.ScmObj** %stackaddr$prim56014, align 8
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%argslist54329$k474392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47927, %struct.ScmObj* %argslist54329$k474391)
store volatile %struct.ScmObj* %argslist54329$k474392, %struct.ScmObj** %stackaddr$prim56015, align 8
%clofunc56016 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47439)
musttail call tailcc void %clofunc56016(%struct.ScmObj* %k47439, %struct.ScmObj* %argslist54329$k474392)
ret void
}

define tailcc void @proc_clo$ae47928(%struct.ScmObj* %env$ae47928,%struct.ScmObj* %current_45args54320) {
%stackaddr$env-ref56017 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47928, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref56017
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim56018, align 8
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%current_45args54321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %current_45args54321, %struct.ScmObj** %stackaddr$prim56019, align 8
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim56020, align 8
%stackaddr$prim56021 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim56021, align 8
%truthy$cmp56022 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47209)
%cmp$cmp56022 = icmp eq i64 %truthy$cmp56022, 1
br i1 %cmp$cmp56022, label %truebranch$cmp56022, label %falsebranch$cmp56022
truebranch$cmp56022:
%ae47932 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47933 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54323$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%argslist54323$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47933, %struct.ScmObj* %argslist54323$k474400)
store volatile %struct.ScmObj* %argslist54323$k474401, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%argslist54323$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47932, %struct.ScmObj* %argslist54323$k474401)
store volatile %struct.ScmObj* %argslist54323$k474402, %struct.ScmObj** %stackaddr$prim56024, align 8
%clofunc56025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc56025(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54323$k474402)
ret void
falsebranch$cmp56022:
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$makeclosure56027 = alloca %struct.ScmObj*, align 8
%fptrToInt56028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47942 to i64
%ae47942 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56028)
store volatile %struct.ScmObj* %ae47942, %struct.ScmObj** %stackaddr$makeclosure56027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47942, %struct.ScmObj* %k47440, i64 0)
%argslist54328$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%argslist54328$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist54328$_37length470790)
store volatile %struct.ScmObj* %argslist54328$_37length470791, %struct.ScmObj** %stackaddr$prim56029, align 8
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%argslist54328$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47942, %struct.ScmObj* %argslist54328$_37length470791)
store volatile %struct.ScmObj* %argslist54328$_37length470792, %struct.ScmObj** %stackaddr$prim56030, align 8
%clofunc56031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc56031(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist54328$_37length470792)
ret void
}

define tailcc void @proc_clo$ae47942(%struct.ScmObj* %env$ae47942,%struct.ScmObj* %current_45args54324) {
%stackaddr$env-ref56032 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47942, i64 0)
store %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$env-ref56032
%stackaddr$prim56033 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim56033, align 8
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%current_45args54325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %current_45args54325, %struct.ScmObj** %stackaddr$prim56034, align 8
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54325)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim56035, align 8
%ae47944 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%cpsprim47442 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47944, %struct.ScmObj* %anf_45bind47211)
store volatile %struct.ScmObj* %cpsprim47442, %struct.ScmObj** %stackaddr$prim56036, align 8
%ae47947 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54327$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%argslist54327$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47442, %struct.ScmObj* %argslist54327$k474400)
store volatile %struct.ScmObj* %argslist54327$k474401, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%argslist54327$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47947, %struct.ScmObj* %argslist54327$k474401)
store volatile %struct.ScmObj* %argslist54327$k474402, %struct.ScmObj** %stackaddr$prim56038, align 8
%clofunc56039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc56039(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist54327$k474402)
ret void
}

define tailcc void @proc_clo$ae47775(%struct.ScmObj* %env$ae47775,%struct.ScmObj* %current_45args54332) {
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %k47443, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%current_45args54333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %current_45args54333, %struct.ScmObj** %stackaddr$prim56041, align 8
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54333)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim56042, align 8
%ae47777 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56043 = alloca %struct.ScmObj*, align 8
%fptrToInt56044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47778 to i64
%ae47778 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56044)
store volatile %struct.ScmObj* %ae47778, %struct.ScmObj** %stackaddr$makeclosure56043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47778, %struct.ScmObj* %_37take47082, i64 0)
%argslist54346$k474430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist54346$k474431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47778, %struct.ScmObj* %argslist54346$k474430)
store volatile %struct.ScmObj* %argslist54346$k474431, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%argslist54346$k474432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47777, %struct.ScmObj* %argslist54346$k474431)
store volatile %struct.ScmObj* %argslist54346$k474432, %struct.ScmObj** %stackaddr$prim56046, align 8
%clofunc56047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47443)
musttail call tailcc void %clofunc56047(%struct.ScmObj* %k47443, %struct.ScmObj* %argslist54346$k474432)
ret void
}

define tailcc void @proc_clo$ae47778(%struct.ScmObj* %env$ae47778,%struct.ScmObj* %current_45args54335) {
%stackaddr$env-ref56048 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47778, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref56048
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54335)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%current_45args54336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54335)
store volatile %struct.ScmObj* %current_45args54336, %struct.ScmObj** %stackaddr$prim56050, align 8
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%current_45args54337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54336)
store volatile %struct.ScmObj* %current_45args54337, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54337)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim56053, align 8
%ae47780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47780)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim56054, align 8
%truthy$cmp56055 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47202)
%cmp$cmp56055 = icmp eq i64 %truthy$cmp56055, 1
br i1 %cmp$cmp56055, label %truebranch$cmp56055, label %falsebranch$cmp56055
truebranch$cmp56055:
%ae47783 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47784 = call %struct.ScmObj* @const_init_null()
%argslist54339$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56056 = alloca %struct.ScmObj*, align 8
%argslist54339$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47784, %struct.ScmObj* %argslist54339$k474440)
store volatile %struct.ScmObj* %argslist54339$k474441, %struct.ScmObj** %stackaddr$prim56056, align 8
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%argslist54339$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47783, %struct.ScmObj* %argslist54339$k474441)
store volatile %struct.ScmObj* %argslist54339$k474442, %struct.ScmObj** %stackaddr$prim56057, align 8
%clofunc56058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc56058(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist54339$k474442)
ret void
falsebranch$cmp56055:
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim56059, align 8
%truthy$cmp56060 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47203)
%cmp$cmp56060 = icmp eq i64 %truthy$cmp56060, 1
br i1 %cmp$cmp56060, label %truebranch$cmp56060, label %falsebranch$cmp56060
truebranch$cmp56060:
%ae47794 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47795 = call %struct.ScmObj* @const_init_null()
%argslist54340$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%argslist54340$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47795, %struct.ScmObj* %argslist54340$k474440)
store volatile %struct.ScmObj* %argslist54340$k474441, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54340$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47794, %struct.ScmObj* %argslist54340$k474441)
store volatile %struct.ScmObj* %argslist54340$k474442, %struct.ScmObj** %stackaddr$prim56062, align 8
%clofunc56063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc56063(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist54340$k474442)
ret void
falsebranch$cmp56060:
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim56064, align 8
%stackaddr$prim56065 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim56065, align 8
%ae47805 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47805)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim56066, align 8
%stackaddr$makeclosure56067 = alloca %struct.ScmObj*, align 8
%fptrToInt56068 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47807 to i64
%ae47807 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56068)
store volatile %struct.ScmObj* %ae47807, %struct.ScmObj** %stackaddr$makeclosure56067, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47807, %struct.ScmObj* %anf_45bind47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47807, %struct.ScmObj* %k47444, i64 1)
%argslist54345$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%argslist54345$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist54345$_37take470820)
store volatile %struct.ScmObj* %argslist54345$_37take470821, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%argslist54345$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %argslist54345$_37take470821)
store volatile %struct.ScmObj* %argslist54345$_37take470822, %struct.ScmObj** %stackaddr$prim56070, align 8
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%argslist54345$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47807, %struct.ScmObj* %argslist54345$_37take470822)
store volatile %struct.ScmObj* %argslist54345$_37take470823, %struct.ScmObj** %stackaddr$prim56071, align 8
%clofunc56072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc56072(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist54345$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47807(%struct.ScmObj* %env$ae47807,%struct.ScmObj* %current_45args54341) {
%stackaddr$env-ref56073 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47807, i64 0)
store %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$env-ref56073
%stackaddr$env-ref56074 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47807, i64 1)
store %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$env-ref56074
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%current_45args54342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %current_45args54342, %struct.ScmObj** %stackaddr$prim56076, align 8
%stackaddr$prim56077 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim56077, align 8
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %anf_45bind47207)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim56078, align 8
%ae47813 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54344$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist54344$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist54344$k474440)
store volatile %struct.ScmObj* %argslist54344$k474441, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$prim56080 = alloca %struct.ScmObj*, align 8
%argslist54344$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47813, %struct.ScmObj* %argslist54344$k474441)
store volatile %struct.ScmObj* %argslist54344$k474442, %struct.ScmObj** %stackaddr$prim56080, align 8
%clofunc56081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc56081(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist54344$k474442)
ret void
}

define tailcc void @proc_clo$ae47678(%struct.ScmObj* %env$ae47678,%struct.ScmObj* %current_45args54349) {
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %k47447, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%current_45args54350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %current_45args54350, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim56084, align 8
%ae47680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56085 = alloca %struct.ScmObj*, align 8
%fptrToInt56086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47681 to i64
%ae47681 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56086)
store volatile %struct.ScmObj* %ae47681, %struct.ScmObj** %stackaddr$makeclosure56085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47681, %struct.ScmObj* %_37map47086, i64 0)
%argslist54366$k474470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%argslist54366$k474471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47681, %struct.ScmObj* %argslist54366$k474470)
store volatile %struct.ScmObj* %argslist54366$k474471, %struct.ScmObj** %stackaddr$prim56087, align 8
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%argslist54366$k474472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47680, %struct.ScmObj* %argslist54366$k474471)
store volatile %struct.ScmObj* %argslist54366$k474472, %struct.ScmObj** %stackaddr$prim56088, align 8
%clofunc56089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47447)
musttail call tailcc void %clofunc56089(%struct.ScmObj* %k47447, %struct.ScmObj* %argslist54366$k474472)
ret void
}

define tailcc void @proc_clo$ae47681(%struct.ScmObj* %env$ae47681,%struct.ScmObj* %current_45args54352) {
%stackaddr$env-ref56090 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47681, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref56090
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$prim56091, align 8
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%current_45args54353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54352)
store volatile %struct.ScmObj* %current_45args54353, %struct.ScmObj** %stackaddr$prim56092, align 8
%stackaddr$prim56093 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54353)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim56093, align 8
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%current_45args54354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54353)
store volatile %struct.ScmObj* %current_45args54354, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54354)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim56096, align 8
%truthy$cmp56097 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47196)
%cmp$cmp56097 = icmp eq i64 %truthy$cmp56097, 1
br i1 %cmp$cmp56097, label %truebranch$cmp56097, label %falsebranch$cmp56097
truebranch$cmp56097:
%ae47685 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47686 = call %struct.ScmObj* @const_init_null()
%argslist54356$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%argslist54356$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47686, %struct.ScmObj* %argslist54356$k474480)
store volatile %struct.ScmObj* %argslist54356$k474481, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%argslist54356$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47685, %struct.ScmObj* %argslist54356$k474481)
store volatile %struct.ScmObj* %argslist54356$k474482, %struct.ScmObj** %stackaddr$prim56099, align 8
%clofunc56100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc56100(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist54356$k474482)
ret void
falsebranch$cmp56097:
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim56101, align 8
%stackaddr$makeclosure56102 = alloca %struct.ScmObj*, align 8
%fptrToInt56103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47695 to i64
%ae47695 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56103)
store volatile %struct.ScmObj* %ae47695, %struct.ScmObj** %stackaddr$makeclosure56102, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47695, %struct.ScmObj* %f47088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47695, %struct.ScmObj* %lst47087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47695, %struct.ScmObj* %_37map47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47695, %struct.ScmObj* %k47448, i64 3)
%argslist54365$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%argslist54365$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist54365$f470880)
store volatile %struct.ScmObj* %argslist54365$f470881, %struct.ScmObj** %stackaddr$prim56104, align 8
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%argslist54365$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47695, %struct.ScmObj* %argslist54365$f470881)
store volatile %struct.ScmObj* %argslist54365$f470882, %struct.ScmObj** %stackaddr$prim56105, align 8
%clofunc56106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc56106(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist54365$f470882)
ret void
}

define tailcc void @proc_clo$ae47695(%struct.ScmObj* %env$ae47695,%struct.ScmObj* %current_45args54357) {
%stackaddr$env-ref56107 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47695, i64 0)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref56107
%stackaddr$env-ref56108 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47695, i64 1)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref56108
%stackaddr$env-ref56109 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47695, i64 2)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref56109
%stackaddr$env-ref56110 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47695, i64 3)
store %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$env-ref56110
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim56111, align 8
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%current_45args54358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %current_45args54358, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54358)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$makeclosure56115 = alloca %struct.ScmObj*, align 8
%fptrToInt56116 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47699 to i64
%ae47699 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56116)
store volatile %struct.ScmObj* %ae47699, %struct.ScmObj** %stackaddr$makeclosure56115, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %anf_45bind47198, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47699, %struct.ScmObj* %k47448, i64 1)
%argslist54364$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%argslist54364$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist54364$_37map470860)
store volatile %struct.ScmObj* %argslist54364$_37map470861, %struct.ScmObj** %stackaddr$prim56117, align 8
%stackaddr$prim56118 = alloca %struct.ScmObj*, align 8
%argslist54364$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist54364$_37map470861)
store volatile %struct.ScmObj* %argslist54364$_37map470862, %struct.ScmObj** %stackaddr$prim56118, align 8
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%argslist54364$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47699, %struct.ScmObj* %argslist54364$_37map470862)
store volatile %struct.ScmObj* %argslist54364$_37map470863, %struct.ScmObj** %stackaddr$prim56119, align 8
%clofunc56120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc56120(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist54364$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47699(%struct.ScmObj* %env$ae47699,%struct.ScmObj* %current_45args54360) {
%stackaddr$env-ref56121 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 0)
store %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$env-ref56121
%stackaddr$env-ref56122 = alloca %struct.ScmObj*, align 8
%k47448 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47699, i64 1)
store %struct.ScmObj* %k47448, %struct.ScmObj** %stackaddr$env-ref56122
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim56123, align 8
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%current_45args54361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %current_45args54361, %struct.ScmObj** %stackaddr$prim56124, align 8
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54361)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%cpsprim47451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %anf_45bind47200)
store volatile %struct.ScmObj* %cpsprim47451, %struct.ScmObj** %stackaddr$prim56126, align 8
%ae47705 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54363$k474480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%argslist54363$k474481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47451, %struct.ScmObj* %argslist54363$k474480)
store volatile %struct.ScmObj* %argslist54363$k474481, %struct.ScmObj** %stackaddr$prim56127, align 8
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%argslist54363$k474482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47705, %struct.ScmObj* %argslist54363$k474481)
store volatile %struct.ScmObj* %argslist54363$k474482, %struct.ScmObj** %stackaddr$prim56128, align 8
%clofunc56129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47448)
musttail call tailcc void %clofunc56129(%struct.ScmObj* %k47448, %struct.ScmObj* %argslist54363$k474482)
ret void
}

define tailcc void @proc_clo$ae47598(%struct.ScmObj* %env$ae47598,%struct.ScmObj* %current_45args54369) {
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%k47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %k47452, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%current_45args54370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %current_45args54370, %struct.ScmObj** %stackaddr$prim56131, align 8
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54370)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim56132, align 8
%ae47600 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56133 = alloca %struct.ScmObj*, align 8
%fptrToInt56134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47601 to i64
%ae47601 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56134)
store volatile %struct.ScmObj* %ae47601, %struct.ScmObj** %stackaddr$makeclosure56133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47601, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist54383$k474520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%argslist54383$k474521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47601, %struct.ScmObj* %argslist54383$k474520)
store volatile %struct.ScmObj* %argslist54383$k474521, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%argslist54383$k474522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47600, %struct.ScmObj* %argslist54383$k474521)
store volatile %struct.ScmObj* %argslist54383$k474522, %struct.ScmObj** %stackaddr$prim56136, align 8
%clofunc56137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47452)
musttail call tailcc void %clofunc56137(%struct.ScmObj* %k47452, %struct.ScmObj* %argslist54383$k474522)
ret void
}

define tailcc void @proc_clo$ae47601(%struct.ScmObj* %env$ae47601,%struct.ScmObj* %current_45args54372) {
%stackaddr$env-ref56138 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47601, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56138
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim56139, align 8
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%current_45args54373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54372)
store volatile %struct.ScmObj* %current_45args54373, %struct.ScmObj** %stackaddr$prim56140, align 8
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim56141, align 8
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%current_45args54374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54373)
store volatile %struct.ScmObj* %current_45args54374, %struct.ScmObj** %stackaddr$prim56142, align 8
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%current_45args54375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54374)
store volatile %struct.ScmObj* %current_45args54375, %struct.ScmObj** %stackaddr$prim56144, align 8
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54375)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim56145, align 8
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%anf_45bind47191 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47191, %struct.ScmObj** %stackaddr$prim56146, align 8
%truthy$cmp56147 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47191)
%cmp$cmp56147 = icmp eq i64 %truthy$cmp56147, 1
br i1 %cmp$cmp56147, label %truebranch$cmp56147, label %falsebranch$cmp56147
truebranch$cmp56147:
%ae47605 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54377$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%argslist54377$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist54377$k474530)
store volatile %struct.ScmObj* %argslist54377$k474531, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%argslist54377$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47605, %struct.ScmObj* %argslist54377$k474531)
store volatile %struct.ScmObj* %argslist54377$k474532, %struct.ScmObj** %stackaddr$prim56149, align 8
%clofunc56150 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc56150(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist54377$k474532)
ret void
falsebranch$cmp56147:
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%anf_45bind47193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47193, %struct.ScmObj** %stackaddr$prim56152, align 8
%stackaddr$makeclosure56153 = alloca %struct.ScmObj*, align 8
%fptrToInt56154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47613 to i64
%ae47613 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56154)
store volatile %struct.ScmObj* %ae47613, %struct.ScmObj** %stackaddr$makeclosure56153, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47613, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47613, %struct.ScmObj* %k47453, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47613, %struct.ScmObj* %anf_45bind47192, i64 2)
%argslist54382$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%argslist54382$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47193, %struct.ScmObj* %argslist54382$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54382$_37foldr1470901, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%argslist54382$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist54382$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54382$_37foldr1470902, %struct.ScmObj** %stackaddr$prim56156, align 8
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%argslist54382$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist54382$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54382$_37foldr1470903, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%argslist54382$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47613, %struct.ScmObj* %argslist54382$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54382$_37foldr1470904, %struct.ScmObj** %stackaddr$prim56158, align 8
%clofunc56159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc56159(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54382$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47613(%struct.ScmObj* %env$ae47613,%struct.ScmObj* %current_45args54378) {
%stackaddr$env-ref56160 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47613, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref56160
%stackaddr$env-ref56161 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47613, i64 1)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref56161
%stackaddr$env-ref56162 = alloca %struct.ScmObj*, align 8
%anf_45bind47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47613, i64 2)
store %struct.ScmObj* %anf_45bind47192, %struct.ScmObj** %stackaddr$env-ref56162
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim56163, align 8
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%current_45args54379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %current_45args54379, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%anf_45bind47194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54379)
store volatile %struct.ScmObj* %anf_45bind47194, %struct.ScmObj** %stackaddr$prim56165, align 8
%argslist54381$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%argslist54381$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47194, %struct.ScmObj* %argslist54381$f470930)
store volatile %struct.ScmObj* %argslist54381$f470931, %struct.ScmObj** %stackaddr$prim56166, align 8
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist54381$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47192, %struct.ScmObj* %argslist54381$f470931)
store volatile %struct.ScmObj* %argslist54381$f470932, %struct.ScmObj** %stackaddr$prim56167, align 8
%stackaddr$prim56168 = alloca %struct.ScmObj*, align 8
%argslist54381$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist54381$f470932)
store volatile %struct.ScmObj* %argslist54381$f470933, %struct.ScmObj** %stackaddr$prim56168, align 8
%clofunc56169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc56169(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist54381$f470933)
ret void
}

define tailcc void @proc_clo$ae47481(%struct.ScmObj* %env$ae47481,%struct.ScmObj* %current_45args54386) {
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%current_45args54387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54386)
store volatile %struct.ScmObj* %current_45args54387, %struct.ScmObj** %stackaddr$prim56171, align 8
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54387)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim56172, align 8
%ae47483 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56173 = alloca %struct.ScmObj*, align 8
%fptrToInt56174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47484 to i64
%ae47484 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56174)
store volatile %struct.ScmObj* %ae47484, %struct.ScmObj** %stackaddr$makeclosure56173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47484, %struct.ScmObj* %y47070, i64 0)
%argslist54405$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%argslist54405$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47484, %struct.ScmObj* %argslist54405$k474550)
store volatile %struct.ScmObj* %argslist54405$k474551, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%argslist54405$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47483, %struct.ScmObj* %argslist54405$k474551)
store volatile %struct.ScmObj* %argslist54405$k474552, %struct.ScmObj** %stackaddr$prim56176, align 8
%clofunc56177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc56177(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist54405$k474552)
ret void
}

define tailcc void @proc_clo$ae47484(%struct.ScmObj* %env$ae47484,%struct.ScmObj* %current_45args54389) {
%stackaddr$env-ref56178 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47484, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref56178
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54389)
store volatile %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$prim56179, align 8
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%current_45args54390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54389)
store volatile %struct.ScmObj* %current_45args54390, %struct.ScmObj** %stackaddr$prim56180, align 8
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54390)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim56181, align 8
%stackaddr$makeclosure56182 = alloca %struct.ScmObj*, align 8
%fptrToInt56183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47485 to i64
%ae47485 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56183)
store volatile %struct.ScmObj* %ae47485, %struct.ScmObj** %stackaddr$makeclosure56182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47485, %struct.ScmObj* %k47456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47485, %struct.ScmObj* %f47071, i64 1)
%ae47486 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56184 = alloca %struct.ScmObj*, align 8
%fptrToInt56185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47487 to i64
%ae47487 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56185)
store volatile %struct.ScmObj* %ae47487, %struct.ScmObj** %stackaddr$makeclosure56184, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47487, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47487, %struct.ScmObj* %y47070, i64 1)
%argslist54404$ae474850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%argslist54404$ae474851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47487, %struct.ScmObj* %argslist54404$ae474850)
store volatile %struct.ScmObj* %argslist54404$ae474851, %struct.ScmObj** %stackaddr$prim56186, align 8
%stackaddr$prim56187 = alloca %struct.ScmObj*, align 8
%argslist54404$ae474852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47486, %struct.ScmObj* %argslist54404$ae474851)
store volatile %struct.ScmObj* %argslist54404$ae474852, %struct.ScmObj** %stackaddr$prim56187, align 8
%clofunc56188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47485)
musttail call tailcc void %clofunc56188(%struct.ScmObj* %ae47485, %struct.ScmObj* %argslist54404$ae474852)
ret void
}

define tailcc void @proc_clo$ae47485(%struct.ScmObj* %env$ae47485,%struct.ScmObj* %current_45args54392) {
%stackaddr$env-ref56189 = alloca %struct.ScmObj*, align 8
%k47456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47485, i64 0)
store %struct.ScmObj* %k47456, %struct.ScmObj** %stackaddr$env-ref56189
%stackaddr$env-ref56190 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47485, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56190
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%_95k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54392)
store volatile %struct.ScmObj* %_95k47457, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%current_45args54393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54392)
store volatile %struct.ScmObj* %current_45args54393, %struct.ScmObj** %stackaddr$prim56192, align 8
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%anf_45bind47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54393)
store volatile %struct.ScmObj* %anf_45bind47189, %struct.ScmObj** %stackaddr$prim56193, align 8
%argslist54395$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%argslist54395$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47189, %struct.ScmObj* %argslist54395$f470710)
store volatile %struct.ScmObj* %argslist54395$f470711, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%argslist54395$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47456, %struct.ScmObj* %argslist54395$f470711)
store volatile %struct.ScmObj* %argslist54395$f470712, %struct.ScmObj** %stackaddr$prim56195, align 8
%clofunc56196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc56196(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist54395$f470712)
ret void
}

define tailcc void @proc_clo$ae47487(%struct.ScmObj* %env$ae47487,%struct.ScmObj* %args4707247458) {
%stackaddr$env-ref56197 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47487, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56197
%stackaddr$env-ref56198 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47487, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref56198
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247458)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247458)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$makeclosure56201 = alloca %struct.ScmObj*, align 8
%fptrToInt56202 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47491 to i64
%ae47491 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56202)
store volatile %struct.ScmObj* %ae47491, %struct.ScmObj** %stackaddr$makeclosure56201, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47491, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47491, %struct.ScmObj* %args47072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47491, %struct.ScmObj* %f47071, i64 2)
%argslist54403$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%argslist54403$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist54403$y470700)
store volatile %struct.ScmObj* %argslist54403$y470701, %struct.ScmObj** %stackaddr$prim56203, align 8
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%argslist54403$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47491, %struct.ScmObj* %argslist54403$y470701)
store volatile %struct.ScmObj* %argslist54403$y470702, %struct.ScmObj** %stackaddr$prim56204, align 8
%clofunc56205 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc56205(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist54403$y470702)
ret void
}

define tailcc void @proc_clo$ae47491(%struct.ScmObj* %env$ae47491,%struct.ScmObj* %current_45args54396) {
%stackaddr$env-ref56206 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47491, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref56206
%stackaddr$env-ref56207 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47491, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref56207
%stackaddr$env-ref56208 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47491, i64 2)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56208
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim56209, align 8
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%current_45args54397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54396)
store volatile %struct.ScmObj* %current_45args54397, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%anf_45bind47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54397)
store volatile %struct.ScmObj* %anf_45bind47187, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$makeclosure56212 = alloca %struct.ScmObj*, align 8
%fptrToInt56213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47494 to i64
%ae47494 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56213)
store volatile %struct.ScmObj* %ae47494, %struct.ScmObj** %stackaddr$makeclosure56212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47494, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47494, %struct.ScmObj* %args47072, i64 1)
%argslist54402$anf_45bind471870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%argslist54402$anf_45bind471871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist54402$anf_45bind471870)
store volatile %struct.ScmObj* %argslist54402$anf_45bind471871, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%argslist54402$anf_45bind471872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47494, %struct.ScmObj* %argslist54402$anf_45bind471871)
store volatile %struct.ScmObj* %argslist54402$anf_45bind471872, %struct.ScmObj** %stackaddr$prim56215, align 8
%clofunc56216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47187)
musttail call tailcc void %clofunc56216(%struct.ScmObj* %anf_45bind47187, %struct.ScmObj* %argslist54402$anf_45bind471872)
ret void
}

define tailcc void @proc_clo$ae47494(%struct.ScmObj* %env$ae47494,%struct.ScmObj* %current_45args54399) {
%stackaddr$env-ref56217 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47494, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref56217
%stackaddr$env-ref56218 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47494, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref56218
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%current_45args54400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54399)
store volatile %struct.ScmObj* %current_45args54400, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%anf_45bind47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54400)
store volatile %struct.ScmObj* %anf_45bind47188, %struct.ScmObj** %stackaddr$prim56221, align 8
%stackaddr$prim56222 = alloca %struct.ScmObj*, align 8
%cpsargs47462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47462, %struct.ScmObj** %stackaddr$prim56222, align 8
%clofunc56223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47188)
musttail call tailcc void %clofunc56223(%struct.ScmObj* %anf_45bind47188, %struct.ScmObj* %cpsargs47462)
ret void
}

define tailcc void @proc_clo$ae47466(%struct.ScmObj* %env$ae47466,%struct.ScmObj* %current_45args54407) {
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim56224, align 8
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%current_45args54408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54407)
store volatile %struct.ScmObj* %current_45args54408, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54408)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim56226, align 8
%argslist54410$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%argslist54410$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist54410$yu470690)
store volatile %struct.ScmObj* %argslist54410$yu470691, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%argslist54410$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist54410$yu470691)
store volatile %struct.ScmObj* %argslist54410$yu470692, %struct.ScmObj** %stackaddr$prim56228, align 8
%clofunc56229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc56229(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist54410$yu470692)
ret void
}